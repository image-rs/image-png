extern crate png;
extern crate glob;

use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;

use std::io::BufReader;
use std::io::prelude::*;

const BASE_PATH: [&'static str; 2] = [".", "tests"];

fn process_images<F>(func: F)
where F: Fn(PathBuf) -> Result<u32, png::DecodingError> {
    let base: PathBuf = BASE_PATH.iter().collect();
    let test_suites = &["pngsuite", "pngsuite-extra", "bugfixes"];
    let mut results = HashMap::new();
    let mut expected_failures = 0;
    for suite in test_suites {
        let mut path = base.clone();
        path.push(suite);
        path.push("*.png");
        let pattern = &*format!("{}", path.display());
        for path in glob::glob(pattern).unwrap().filter_map(Result::ok) {
            print!("{:?}: ", path.clone());
            match func(path.clone()) {
                Ok(crc) => {
                    results.insert(format!("{:?}", path), format!("{}", crc));
                    println!("{}", crc)
                },
                Err(_) if path.file_name().unwrap().to_str().unwrap().starts_with("x") => {
                    expected_failures += 1;
                    println!("Expected failure")
                },   
                err => panic!("{:?}", err)
            }
        }
    }
    let mut path = base.clone();
    path.push("results.txt");
    let mut ref_results = HashMap::new();
    let mut failures = 0;
    for line in BufReader::new(File::open(path).unwrap()).lines() {
        let line = line.unwrap();
        let parts: Vec<_> = line.split(": ").collect();
        if parts[1] == "Expected failure" {
            failures += 1;
        } else {
            ref_results.insert(parts[0].to_string(), parts[1].to_string());
        }
    }
    assert_eq!(expected_failures, failures);
    for (path, crc) in results.iter() {
        assert_eq!(
            ref_results.get(path).expect(&format!("reference for {:?} is missing", path)), 
            crc
        )
    }
}

#[test]
fn render_images() {
    process_images(|path| {
        let decoder = png::Decoder::new(File::open(path)?);
        let (info, mut reader) = decoder.read_info()?;
        let mut img_data = vec![0; info.buffer_size()];
        reader.next_frame(&mut img_data)?;
        // First sanity check:
        assert_eq!(
            img_data.len(), 
            info.width as usize
            * info.height as usize
            * info.color_type.samples()
            * info.bit_depth as usize/8
        );
        let mut crc = png::crc::Crc32::new();
        crc.update(&img_data);
        Ok(crc.checksum())
    })
}
