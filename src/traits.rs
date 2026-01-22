use std::io;

#[cfg(feature = "decoder")]
macro_rules! read_bytes_ext {
    ($output_type:ty) => {
        impl<W: io::Read + ?Sized> ReadBytesExt<$output_type> for W {
            #[inline]
            fn read_be(&mut self) -> io::Result<$output_type> {
                let mut bytes = [0u8; std::mem::size_of::<$output_type>()];
                self.read_exact(&mut bytes)?;
                Ok(<$output_type>::from_be_bytes(bytes))
            }
        }
    };
}

#[cfg(feature = "encoder")]
macro_rules! write_bytes_ext {
    ($input_type:ty) => {
        impl<W: io::Write + ?Sized> WriteBytesExt<$input_type> for W {
            #[inline]
            fn write_be(&mut self, n: $input_type) -> io::Result<()> {
                self.write_all(&n.to_be_bytes())
            }
        }
    };
}

/// Read extension to read big endian data
#[cfg(feature = "decoder")]
pub trait ReadBytesExt<T>: io::Read {
    /// Read `T` from a bytes stream. Most significant byte first.
    fn read_be(&mut self) -> io::Result<T>;
}

/// Write extension to write big endian data
#[cfg(feature = "encoder")]
pub trait WriteBytesExt<T>: io::Write {
    /// Writes `T` to a bytes stream. Most significant byte first.
    fn write_be(&mut self, _: T) -> io::Result<()>;
}

#[cfg(feature = "decoder")]
read_bytes_ext!(u8);
#[cfg(feature = "decoder")]
read_bytes_ext!(u16);
#[cfg(feature = "decoder")]
read_bytes_ext!(u32);

#[cfg(feature = "encoder")]
write_bytes_ext!(u32);
