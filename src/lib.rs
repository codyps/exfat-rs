/**
 * exFat filesystem
 *
 * A sector contains a fixed number (per-exfat volume, power of 2) of bytes.
 *
 * A cluster contains a fixed number (per-exfat volume, power of 2) of sectors.
 *
 * The FAT is an array of u32 entires, with each describing a cluster.
 *
 * Using FAT entries as "next pointers", the clusters are formed into chains.
 *
 * The "cluster heap" is basically the entire remainder of the storage volume (after the boot
 * sector and FAT).
 *
 * Directory & File data is stored in the cluster heap.
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
use ::std::ops::Index;
use ::fmt_extra::AsciiStr;

#[derive(Debug)]
pub enum BootSectorInitError {
    BadMagic(AsciiStr<[u8;8]>),
    MustBeZeroNonZero,
    FatOffsTooSmall(u32)
}

#[derive(Debug)]
pub enum BootSectorInitIoError {
    Io(::std::io::Error),
    Init(BootSectorInitError)
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
 * An Exfat superblock. Sometimes refered to as a "boot sector". Contains all the essential items
 * for recognizing and using the filesystem.
 *
 * We store the entire thing is it's very likely that we'll need to "write-back" the entire sector
 * if anything changes (as block devices don't have byte-level access)
 *
 * As an alternative, it might make sense to construct this from any AsRef<[u8]> which can promise
 * it's long enough.
 */
pub struct BootSector {
    raw: [u8;512],
}

impl BootSector {
    /*
     * FIXME: we really need a unification of ReadAt and Read here: as we're only doing a single
     * call (and don't care where the cursor ends up), it'd be nice to allow either
     */
    /// Populate with a superblock from this `ReadAt`able thing, at a given offset
    pub fn read_at_from<R: ReadAt>(s: R, offs: u64) -> Result<Self, BootSectorInitIoError> {
        let mut sb = unsafe { BootSector { raw: ::std::mem::uninitialized() } };
        /*
         * FIXME: ReadAt does not promise that this returns all the data requested. Add a wrapper
         * here or in io-at
         */
        try!(s.read_at(&mut sb.raw, offs).map_err(|e| BootSectorInitIoError::Io(e)));
        sb.validate().map_err(|e| BootSectorInitIoError::Init(e))
    }

    /// Populate with a superblock from this `Read`able thing, at it's current offset
    pub fn read_from<R: Read>(mut s: R) -> Result<Self, BootSectorInitIoError> {
        let mut sb = unsafe { BootSector { raw: ::std::mem::uninitialized() } };
        try!(s.read_exact(&mut sb.raw).map_err(|e| BootSectorInitIoError::Io(e)));
        sb.validate().map_err(|e| BootSectorInitIoError::Init(e))
    }

    /// Create from the exact amount of data needed
    pub fn from(s: [u8;512]) -> Result<Self, BootSectorInitError> {
        /* validate BootSector */
        BootSector { raw: s }.validate()
    }

    pub fn raw(&self) -> &[u8;512] {
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

    fn validate(self) -> Result<Self, BootSectorInitError> {
        /* 0,1,2: jmp junk */
        /* 3-11: "EXFAT" */
        {
            let magic = self.magic();
            if magic != b"EXFAT   " {
                return Err(BootSectorInitError::BadMagic(AsciiStr(magic.clone())))
            }
        }

        /* 11..(53-11): must be zero */
        {
            let z = &self.raw()[11..(11+53)];
            for b in z {
                if *b != 0 {
                    return Err(BootSectorInitError::MustBeZeroNonZero);
                }
            }
        }

        if self.fat_offs() < 24 {
            return Err(BootSectorInitError::FatOffsTooSmall(self.fat_offs()));
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

/**
 * After an exFAT bootsector, there are 8 extended boot sectors.
 *
 * These are intended to carry extra boot code.
 *
 * These can be marked with 0xAA550000 to indicate that they are, in fact, extended boot sectors.
 *
 * The purpose of marking is unclear, as is what the data represents in the case where they are
 * unmarked.
 */
struct ExtendedBootSector<T> {
    s: T,
    bytes_per_sector_shift: u8,
}

impl<T: AsRef<[u8]>> ExtendedBootSector<T> {
    pub fn from(s: T, bytes_per_sector_shift: u8) -> Self {
        /* TODO: split the "kind" out early? Or late?
         * Perhaps an enum is appropriate here?
         */
        ExtendedBootSector { s: s, bytes_per_sector_shift: bytes_per_sector_shift }
    }

    pub fn raw(&self) -> &[u8] {
        self.s.as_ref()
    }

    pub fn signature(&self) -> u32 {
        let offs = 1 << self.bytes_per_sector_shift - 4;
        read_num_bytes!(u32, 4, &self.raw()[offs..])
    }

    pub fn is_extended_boot_sector(&self) -> bool {
        self.signature() == 0xAA_55_00_00
    }
}

struct ExtendedBootSectors;

struct OemParameter {
    raw: [u8;48],
}

impl OemParameter {
    pub fn is_used(&self) -> bool {
        for i in self.uuid() {
            if *i != 0 {
                return true;
            }
        }

        return false;
    }

    pub fn uuid(&self) -> &[u8;16] {
        index_fixed!(&self.raw; 0, .. 16)
    }

    pub fn data(&self) -> &[u8;32] {
        index_fixed!(&self.raw; 16, .. 48)
    }
}

struct OemParameters<T> {
    s: T,
}

impl<S: AsRef<[u8]>> OemParameters<S> {
    pub fn from(s: S) -> Self {
        OemParameters { s: s }
    }

    pub fn raw(&self) -> &[u8] {
        self.s.as_ref()
    }

    pub fn all(&self) -> &[OemParameter;10] {
        unsafe {
            ::std::mem::transmute::<*const u8, &[OemParameter;10]>
                (self.raw().as_ptr())
        }
    }
}

pub enum FsInitError {
    BootSectorInitError(BootSectorInitIoError)
}

pub struct Fs<S: ReadAt> {
    bs: BootSector,
    store: S
}

impl<S: ReadAt> Fs<S> {
    pub fn from_ro(t: S) -> Result<Self, FsInitError> {
        let bs = try!(BootSector::read_at_from(&t, 0).map_err(|e| FsInitError::BootSectorInitError(e)));
        Ok(Fs { bs: bs, store: t })
    }

    pub fn boot_sector(&self) -> &BootSector {
        &self.bs
    }

    /*
    pub fn ext_boot_sectors(&self) -> &ExtendedBootSectors {
        /* do something?? */
    }
    */

    /*
    pub fn oem_parameters(&self) -> OemParameters {

    }
    */
}

/// The FAT (file allocation table) contains a contiguous series of FAT entries.
///
/// Each FAT entry is 4 bytes.
///
/// Some FAT entries are special:
///
/// 0: 0xFF_FF_FF_F8 (f8 indicates "media type")
/// 1: 0xFF_FF_FF_FF ("nothing of interest")
///
/// 2...(cluster_count + 1) : each describe a cluster in the cluster heap
///
/// Values for FAT entires:
///  2...(cluster_count+1): fat entry of the next cluster in the cluster chain
///  0xFF_FF_FF_F7: bad cluster
///  0xFF_FF_FF_FF: last cluster in the cluster chain
pub struct Fat {
    v: Vec<u32>
}

impl Fat {
    pub fn read_at_from<T: ReadAt>(s: T, offs: u64, len: u64) -> ::io_at::Result<Self> {
        let e = len / 4;
        if (len % 4) != 0 {
            panic!("FAT length must be a multiple of 4");
        }

        let f = Fat { v: Vec::with_capacitry(e) };
        unsafe { f.v.set_len(e) }
        try!(s.read_at(f.v.as_mut_slice(), offs));
        f
    }

    pub fn media_type(&self) -> u8 {
        self.v[0] & 0xff;
    }

    pub fn cluster_ct(&self) -> u32 {
        self.v.len()  / 4 - 2
    }
}

impl Index<usize> for Fat {
    type Target = FatEntry

    fn index(&self, i: usize) -> FatEntry {
        FatEntry::from_index(self.v[2 + i])
    }
}

pub struct FatEntry {
    v: u32
}

impl FatEntry {
    pub fn from_val(i: u32) -> Self {
        FatEntry { v: i }
    }

    pub fn is_bad(&self) -> bool {
        self.v == 0xFF_FF_FF_F7
    }

    pub fn val(&self) -> u32 {
        self.v
    }
}

/*
/// An array of Cluster fields, each being of cluster size (ie: 2**sectors_per_cluster_shift
/// sectors)
pub struct ClusterHeap {
}
*/

/// A series of `DirectoryEntry`s stored in a cluster chain
///
/// Each entry is 32 bytes
pub struct Dir {
}

pub struct DirEntry {
    v: [u8;32],
}

impl DirEntry {
    /// 0x00 = end-of-directory, all other fields reserved
    ///        subsequent DirEntries in a Dir are also given this type
    pub fn entry_type(&self) -> u8 {
        self.v[0]
    }

    pub fn custom_defined(&self) -> &[u8;19] {
        index_fixed!(&self; 1, .. 19)
    }

    pub fn first_cluster(&self) -> u32 {
        read_num_bytes!(u32, 4, &self.v[20..])
    }

    pub fn data_len(&self) -> u64 {
        read_num_bytes!(u64, 8, &self.v[24..])
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
