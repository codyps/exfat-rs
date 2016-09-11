/**
 * exFat filesystem
 *
 * A sector contains a fixed number (per-exfat volume, power of 2) of bytes.
 *
 * A cluster contains a fixed number (per-exfat volume, power of 2) of sectors.
 *
 *
 * General layout:
 *
 *                       |offs| size (sectors)
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
    FatOffsTooSmall(u32)
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

    /// A jump instruction for x86
    ///
    /// Specified to be [0xEB, 0x76, 0x90].
    ///
    /// TODO: consider testing that this matches expectations
    pub fn jump_boot(&self) -> &[u8;3] {
        index_fixed!(&self.raw();0, .. 3)
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

    /// Length in sectors of the volume
    ///
    /// At least: `2*20/(2**bytes_per_sector_shift)`
    /// At most: `2**64-1`
    ///
    /// offset: 72, size 8
    pub fn volume_len(&self) -> u64 {
        read_num_bytes!(u64, 8, &self.raw()[72..])
    }

    /// Volume-relative sector offset for the first (and perhaps only) FAT.
    ///
    /// At least: 24
    /// At most: cluster_heap_offs - (fat_len * num_fats)
    ///
    /// offset: 80, size 4
    pub fn fat_offs(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[80..])
    }

    /// Length in sectors of the FAT table(s)
    ///
    /// At least: (cluster_count + 2) * 2**2 / 2**bytes_per_sector_shift
    ///     rounded up to the nearest integer
    ///     [ie: a FAT must have room for all the clusters]
    /// At most: (cluster_heap_offset - fat_offset) / num_fats
    ///     [ie: the FATs must be ordered before the cluster heap]
    ///
    /// offset: 84, size 4
    pub fn fat_len(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[84..])
    }

    /// Volume-relative sector offset for the cluster heap
    ///
    /// At least: fat_offs + fat_len * num_fats
    /// [ie: the preceeding rebions]
    ///
    /// At most: min( 2**32-1 , volume_len - cluster_count * 2**sectors_per_cluster_shift )
    ///
    /// offset: 88, size 4
    pub fn cluster_heap_offs(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[88..])
    }

    /// Number of cluster in the cluster heap
    ///
    /// Value: min(
    ///         volume_len - cluster_heap_offs) / 2**sectors_per_cluster_shift,
    ///         2**32 - 11
    ///     )
    ///
    /// offset: 92, size 4
    pub fn cluster_count(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.raw()[92..])
    }

    /// Cluster index of the first cluster of the root directory
    ///
    /// At least: 2
    /// At most: cluster_count + 1
    ///
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
    /// At Least:  1.0
    /// At Most:  99.99
    ///
    /// offset: 104, size: 2
    pub fn file_system_rev(&self) -> u16 {
        read_num_bytes!(u16, 2, &self.raw()[104..])
    }

    /// Flags indicating tthe status of file system structures on this volume
    /// 0 = Active FAT & allocation bitmap (0 = first, 1 = second)
    /// 1 = Volume dirty (0 = claims consistency, 1 = claims inconsistency)
    /// 2 = media failure (0 = no failures reported, or known failures recorded in bad clusters,
    ///                    1 = reported failures)
    /// 3 = clear to zero (0 = nothing in particular,
    ///                    1 = impls shall set it to 0 prior to modifying any fs structures, dirs,
    ///                      or files)
    /// rest: reseved
    ///
    /// offset: 106, size: 2
    pub fn volume_flags(&self) -> u16 {
        read_num_bytes!(u16, 2, &self.raw()[106..])
    }

    /// bytes per sector in log2(N) form
    ///
    /// At least: 9 (512 bytes)
    /// At most: 12 (4096 bytes)
    ///
    /// Nothing is really restricting these things other than convention, recommend accepting
    /// larger variations.
    ///
    /// offset: 108, size: 1
    pub fn bytes_per_sector_shift(&self) -> u8 {
        self.raw()[108]
    }

    /// sectors per cluster in log2(N) form
    ///
    /// At least: 0 (1 sector)
    /// At most: 25-bytes_per_sector_shift (2**25 - bytes_per_sector = 32MB)
    ///
    /// XXX: determine the basis of the upper limit here.
    ///
    /// offset: 109, size: 1
    pub fn sectors_per_cluster_shift(&self) -> u8 {
        self.raw()[109]
    }

    /// Number of FATs and allocation bitmaps in the volume
    ///
    /// At least: 1
    /// At most: 2
    ///
    /// offset: 110, size: 1
    pub fn number_of_fats(&self) -> u8 {
        self.raw()[110]
    }

    /// INT 13h drive number, intended primarily for bios bootup
    ///
    /// Using 0x80 is common.
    ///
    /// offset: 111, size: 1
    pub fn drive_select(&self) -> u8 {
        self.raw()[111]
    }

    /// percentage of clusters in the cluster heap which are allocated
    ///
    /// At least: 0
    /// At most: 100
    ///
    /// Or set to 0xff to mark as unavalible.
    ///
    /// offset: 112, size: 1
    pub fn percent_in_use(&self) -> u8 {
        self.raw()[112]
    }

    /// Bootstrap data (jumped to by jump_code) intended for use by BIOS boot.
    ///
    /// offset 120, size 390
    pub fn boot_code(&self) -> &[u8;390] {
        index_fixed!(&self.raw(); 120, .. (120+390))
    }

    /// The value AA55
    ///
    /// offset 510, size 2
    pub fn boot_signature(&self) -> &[u8;2] {
        index_fixed!(&self.raw(); 510, .. (510+2))
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

        if self.fat_offs() < 24 {
            return Err(SbInitError::FatOffsTooSmall(self.fat_offs()));
        }

        {
            // self.volume_len() > (2**(20-self.bytes_per_sector_shift()))
        }

        {
            // self.fat_offs() > (self.cluster_heap_offs() - self.fat_len() * self.num_fats())
        }

        {
            // self.f
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
