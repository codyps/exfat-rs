/**
 * exFat filesystem
 *
 * General layout:
 *
 *                       |offs| size
 * boot sector (aka sb)  | 0  | 1
 * extended boot sectors | 1  | 8
 * oem parameters        | 9  | 1
 * reserved              | 10 | 1
 * boot checksum         | 11 | 1
 *
 * Immediately followed by a "backup boot region"
 * of the same layout
 *
 *
 * fat alignment         | 24
 * (undef contents)      |
 * first fat             | fat_offs | fat_len
 * second_fat            | fat_offs + fat_len | fat_len
 * (repeated for `number_of_fats`)
 *
 * cluster heap align    | fat_offs + fat_len * num_of_fats
 *                             | cluster_heap_offs - (fat_offs + fat_len * num_of_fats)
 * cluster heap          |
 *
 */

#[macro_use]
extern crate index_fixed;
extern crate io_at;
extern crate fmt_extra;
extern crate core;

use ::io_at::{ReadAt,WriteAt};
use ::std::io::Read;
use ::fmt_extra::AsciiStr;

#[derive(Debug)]
pub enum SbInitError {
    BadMagic(AsciiStr<[u8;8]>),
    MustBeZeroNonZero,
}

#[derive(Debug)]
pub enum SbInitIoError {
    Io(::std::io::Error),
    Init(SbInitError)
}

macro_rules! read_num_bytes {
    ($ty:ty, $size:expr, $src:expr) => ({
        assert!($size == ::core::mem::size_of::<$ty>());
        assert!($size <= $src.len());
        let mut data: $ty = 0;
        unsafe {
            ::core::ptr::copy_nonoverlapping(
                $src.as_ptr(),
                &mut data as *mut $ty as *mut u8,
                $size);
        }
        data.to_le()
    });
}

/**
 * An Exfat superblock. Sometimes refered to as a "boot sector".
 */
pub struct Sb {
    raw: [u8;512 * 12],
}

impl Sb {
    /*
     * FIXME: we really need a unification of ReadAt and Read here: as we're only doing a single
     * call (and don't care where the cursor ends up), it'd be nice to allow either
     */
    /// Populate with a superblock from this `ReadAt`able thing, at a given offset
    pub fn read_at_from<R: ReadAt>(s: R, offs: u64) -> Result<Self, SbInitIoError> {
        let mut sb = unsafe { Sb { raw: ::std::mem::uninitialized() } };
        /*
         * FIXME: ReadAt does not promise that this returns all the data requested. Add a wrapper
         * here or in io-at
         */
        try!(s.read_at(&mut sb.raw, offs).map_err(|e| SbInitIoError::Io(e)));
        sb.validate().map_err(|e| SbInitIoError::Init(e))
    }

    /// Populate with a superblock from this `Read`able thing, at it's current offset
    pub fn read_from<R: Read>(mut s: R) -> Result<Self, SbInitIoError> {
        let mut sb = unsafe { Sb { raw: ::std::mem::uninitialized() } };
        try!(s.read_exact(&mut sb.raw).map_err(|e| SbInitIoError::Io(e)));
        sb.validate().map_err(|e| SbInitIoError::Init(e))
    }

    pub fn from(s: [u8;512 * 12]) -> Result<Self, SbInitError> {
        /* validate Sb */
        Sb { raw: s }.validate()
    }

    pub fn raw(&self) -> &[u8;512 * 12] {
        &self.raw
    }

    /// The string "EXFAT   " (3 trailing spaces)
    ///
    /// Used to check that a volume is using exfat
    ///
    /// offset: 3, size: 8
    pub fn magic(&self) -> &[u8;8] {
        index_fixed!(&self.raw(); 3, .. 11)
    }

    /// Offset in sectors from the start of the media on which this partition is stored.
    ///
    /// If zero (0), should be ignored.
    ///
    /// Only indented for use in BIOS bootup senarios (in which BIOS would load this data into
    /// memory)
    ///
    /// offset: 64, size: 8
    pub fn partition_offs(&self) -> u64 {
        read_num_bytes!(u64, 8, &self.raw()[64..])
    }

    /// Size of the volume in sectors
    ///
    /// offset: 72, size 8
    pub fn volume_len(&self) -> u64 {
        read_num_bytes!(u64, 8, &self.raw()[72..])
    }

    /// offset: 80, size 4
    pub fn fat_offs(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[80..])
    }

    /// offset: 84, size 4
    pub fn fat_len(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[84..])
    }

    /// offset: 88, size 4
    pub fn cluster_heap_offs(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[88..])
    }

    /// offset: 92, size 4
    pub fn cluster_count(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[92..])
    }

    /// offset: 96, size 4
    pub fn first_cluster_of_root_dir(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[96..])
    }

    /// offset: 100, size 4
    pub fn volume_serial_num(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[100..])
    }

    /// First byte is major version, Second byte is minor version
    ///
    /// Windows 10 as of 2016-09-10 formats disks with "1.0"
    ///
    /// offset: 104, size: 2
    pub fn file_system_rev(&self) -> u16 {
        read_num_bytes!(u16, 2, &self.raw()[104..])
    }

    /// offset: 106, size: 2
    pub fn volume_flags(&self) -> u16 {
        read_num_bytes!(u16, 2, &self.raw()[106..])
    }

    /// offset: 108, size: 1
    pub fn bytes_per_sector_shift(&self) -> u8 {
        self.raw()[108]
    }

    /// offset: 109, size: 1
    pub fn sectors_per_cluster_shift(&self) -> u8 {
        self.raw()[109]
    }

    /// offset: 110, size: 1
    pub fn number_of_fats(&self) -> u8 {
        self.raw()[110]
    }

    /// offset: 111, size: 1
    pub fn drive_select(&self) -> u8 {
        self.raw()[111]
    }

    /// offset: 112, size: 1
    pub fn percent_in_use(&self) -> u8 {
        self.raw()[112]
    }

    /// offset 120, size 390
    pub fn boot_code(&self) -> &[u8;390] {
        index_fixed!(&self.raw(); 120, .. (120+390))
    }

    fn validate(self) -> Result<Self, SbInitError> {
        /* 0,1,2: jmp junk */
        /* 3-11: "EXFAT" */
        {
            let magic = self.magic();
            if magic != b"EXFAT   " {
                return Err(SbInitError::BadMagic(AsciiStr(magic.clone())))
            }
        }

        /* 11..(53-11): must be zero */
        {
            let z = &self.raw()[11..(11+53)];
            for b in z {
                if *b != 0 {
                    return Err(SbInitError::MustBeZeroNonZero);
                }
            }
        }

        Ok(self)
    }
}

pub enum ExfatInitError {
    SbInitError(SbInitError)
}

pub struct Exfat<S: ReadAt> {
    store: S
}

impl<S: ReadAt> Exfat<S> {
    pub fn new_ro(t: S) -> Result<Self, ExfatInitError> {
        Ok(Exfat { store: t })
    }

    pub fn new_rw(t: S) -> Result<Self, ExfatInitError>
        where S: WriteAt
    {
        Ok(Exfat { store: t })
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
