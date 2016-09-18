extern crate exfat;
extern crate clap;
extern crate fmt_extra;
use ::fmt_extra::{AsciiStr,Hs};
use ::clap::{Arg, App, SubCommand};
use ::std::path::Path;

#[derive(Debug)]
enum BootSectorOpenError {
    Open(::std::io::Error),
    BootSector(::exfat::BootSectorInitIoError)
}

fn bs_from_file<P: AsRef<Path>>(path: P) -> Result<exfat::BootSector, BootSectorOpenError> {
    let f = try!(::std::fs::File::open(path)
                 .map_err(|e| BootSectorOpenError::Open(e)));
    exfat::BootSector::read_at_from(&f, 0).map_err(|e| BootSectorOpenError::BootSector(e))
}

fn main() {
    let matches = App::new("dumpexfat")
        .arg(Arg::with_name("file")
             .value_name("DISK-IMAGE")
             .help("read the super block from this file")
             .required(true)
             .takes_value(true))
        .arg(Arg::with_name("blocksize")
             .short("b")
             .value_name("BLOCK-SIZE-IN-BYTES")
             .help("Specify a block size to use")
             .required(true)
             .takes_value(true))
        .get_matches();

    let f = matches.value_of("file").unwrap();
    println!("reading from file {}", f);


    let bs = match bs_from_file(f) {
        Err(e) => {
            println!("Failed to read bs: {:?}", e);
            ::std::process::exit(1);
        },
        Ok(v) => v,
    };

    println!("jump boot: {}", Hs(bs.jump_boot()));
    println!("magic: {}", AsciiStr(bs.magic()));
    println!("partition offset: {}", bs.partition_offs());
    println!("volume length: {}", bs.volume_len());
    println!("fat offset: {}", bs.fat_offs());
    println!("fat len: {}", bs.fat_len());
    println!("cluster heap offset: {}", bs.cluster_heap_offs());
    println!("cluster count: {}", bs.cluster_count());
    println!("first cluster of root directory: {}", bs.first_cluster_of_root_dir());
    println!("volume serial number: {}", bs.volume_serial_num());
    println!("file system revision: {}", bs.file_system_rev());
    println!("volume flags: {}", bs.volume_flags());
    println!("bytes per sector shift: {}", bs.bytes_per_sector_shift());
    println!(" \\ bytes per sector: {}", 1<<bs.bytes_per_sector_shift());
    println!("sectors per cluster shift: {}", bs.sectors_per_cluster_shift());
    println!(" \\ sectors per cluster: {}", 1<<bs.sectors_per_cluster_shift());
    println!("number of fats: {}", bs.number_of_fats());
    println!("drive select: {}", bs.drive_select());
    println!("percent in use: {}", bs.percent_in_use());
    println!("boot signature: {}", Hs(bs.boot_signature()));
}
