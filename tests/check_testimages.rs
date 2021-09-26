extern crate crc32fast;
extern crate glob;
extern crate png;

use std::collections::BTreeMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::{Component, Path, PathBuf};

use crc32fast::Hasher as Crc32;

const TEST_SUITES: [&'static str; 3] = ["pngsuite", "pngsuite-extra", "bugfixes"];
const APNG_SUITES: [&'static str; 1] = ["animated"];

fn process_images<F>(results_path: &str, test_suites: &[&'static str], func: F)
where
    F: Fn(PathBuf) -> Result<u32, png::DecodingError>,
{
    let base = PathBuf::from("tests");

    let mut results_path = base.join(results_path);
    let mut suite_paths: Vec<PathBuf> = test_suites
        .into_iter()
        .map(|suite| base.join(suite))
        .collect();
    xtest_data::setup!()
        .rewrite([&mut results_path])
        .rewrite(suite_paths.iter_mut())
        .build();

    let mut results = BTreeMap::new();
    let mut expected_failures = vec![];

    for (suite_name, suite) in test_suites.iter().zip(suite_paths) {
        let mut test_name_path = base.clone();
        test_name_path.push(suite_name);

        let pattern = suite.join("*.png").display().to_string();

        for path in glob::glob(&pattern).unwrap().filter_map(Result::ok) {
            let filename = path.file_name().unwrap();
            let testname = test_name_path.join(filename);

            print!("{}: ", path.display());
            match func(path.clone()) {
                Ok(crc) => {
                    results.insert(testname.display().to_string(), crc.to_string());
                    println!("{}", crc)
                }
                Err(_) if filename.to_str().unwrap().starts_with("x") => {
                    expected_failures.push(testname.display().to_string());
                    println!("Expected failure")
                }
                err => panic!("{:?}", err),
            }
        }
    }

    let mut ref_results = BTreeMap::new();
    let mut failures = vec![];
    for line in BufReader::new(File::open(results_path).unwrap()).lines() {
        let line = line.unwrap();
        let parts: Vec<_> = line.split(": ").collect();
        if parts.is_empty() {
            panic!(
                "Result file malformed, missing expected result in line {}",
                line
            );
        }
        if parts[1] == "Expected failure" {
            failures.push(format!(
                "{}",
                normalize_path(Path::new(&parts[0])).display()
            ));
        } else {
            let current_path = format!("{}", normalize_path(Path::new(&parts[0])).display());
            ref_results.insert(current_path, parts[1].to_string());
        }
    }
    assert_eq!(
        expected_failures.len(),
        failures.len(),
        "missing, extra: {:?}",
        {
            let mut same = vec![];
            for (idx, f) in expected_failures.iter().enumerate() {
                if let Some(inner) = failures.iter().position(|el| *el == *f) {
                    failures.remove(inner);
                    same.push(idx);
                }
            }
            for &idx in same.iter().rev() {
                expected_failures.remove(idx);
            }
            (failures, expected_failures)
        }
    );
    for (path, crc) in results.iter() {
        assert_eq!(
            ref_results.get(path).expect(&format!(
                "reference for {} is missing, expected {}",
                path, crc
            )),
            crc,
            "{}",
            path
        )
    }
}

#[test]
fn render_images() {
    process_images("results.txt", &TEST_SUITES, |path| {
        let mut decoder = png::Decoder::new(File::open(path)?);
        decoder.set_transformations(png::Transformations::normalize_to_color8());
        let mut reader = decoder.read_info()?;
        let mut img_data = vec![0; reader.output_buffer_size()];
        let info = reader.next_frame(&mut img_data)?;
        // First sanity check:
        assert_eq!(
            img_data.len(),
            info.width as usize
                * info.height as usize
                * info.color_type.samples()
                * info.bit_depth as usize
                / 8
        );
        let mut crc = Crc32::new();
        crc.update(&img_data);
        Ok(crc.finalize())
    })
}

#[test]
fn render_images_identity() {
    process_images("results_identity.txt", &TEST_SUITES, |path| {
        let decoder = png::Decoder::new(File::open(&path)?);
        let mut reader = decoder.read_info()?;
        let mut img_data = vec![0; reader.output_buffer_size()];
        let info = reader.next_frame(&mut img_data)?;
        let bits = (info.width as usize * info.color_type.samples() * info.bit_depth as usize + 7
            & !7)
            * info.height as usize;
        // First sanity check:
        assert_eq!(
            img_data.len() * 8,
            bits,
            "path: {} info: {:?} bits: {}",
            path.display(),
            info,
            bits
        );
        let mut crc = Crc32::new();
        crc.update(&img_data);
        Ok(crc.finalize())
    });
}

#[test]
fn apng_images() {
    process_images("results_apng.txt", &APNG_SUITES, |path: PathBuf| {
        let frame_count: usize = {
            let stem = path
                .file_stem()
                .expect("Test images should all have filenames")
                .to_str()
                .expect("Test image names should be unicode");
            let count = stem
                .rsplit("_f")
                .next()
                .expect("Test image name should end with `_f0` to denote frame count")
                .parse()
                .expect("Test image frame could should be an integer");
            count
        };

        let mut decoder = png::Decoder::new(File::open(&path)?);
        decoder.set_transformations(png::Transformations::normalize_to_color8());
        let mut reader = decoder.read_info()?;
        let mut img_data = vec![0; reader.output_buffer_size()];
        let real_frames = reader.info().animation_control().unwrap().num_frames;
        // Print out frame info, helps with blessing the result file for new images.
        println!(
            "file {}; images to decode: {}, per actl: {}",
            path.display(),
            frame_count,
            real_frames
        );

        let mut crc = Crc32::new();
        for _ in 0..frame_count {
            reader.next_frame(&mut img_data)?;
            crc.update(&img_data);
        }
        Ok(crc.finalize())
    })
}

// until rust standardizes path normalization, see https://github.com/rust-lang/rfcs/issues/2208
fn normalize_path(path: &Path) -> PathBuf {
    let mut components = path.components().peekable();
    let mut ret = if let Some(c @ Component::Prefix(..)) = components.peek().cloned() {
        components.next();
        PathBuf::from(c.as_os_str())
    } else {
        PathBuf::new()
    };

    for component in components {
        match component {
            Component::Prefix(..) => unreachable!(),
            Component::RootDir => {
                ret.push(component.as_os_str());
            }
            Component::CurDir => {}
            Component::ParentDir => {
                ret.pop();
            }
            Component::Normal(c) => {
                ret.push(c);
            }
        }
    }
    ret
}
