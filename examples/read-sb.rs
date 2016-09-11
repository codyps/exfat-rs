extern crate exfat;
extern crate clap;
extern crate fmt_extra;
use ::fmt_extra::{AsciiStr,Hs};
use ::clap::{Arg, App, SubCommand};
use ::std::path::Path;

#[derive(Debug)]
enum SbOpenError {
    Open(::std::io::Error),
    Sb(::exfat::SbInitIoError)
}

fn sb_from_file<P: AsRef<Path>>(path: P) -> Result<exfat::Sb, SbOpenError> {
    let f = try!(::std::fs::File::open(path)
                 .map_err(|e| SbOpenError::Open(e)));
    exfat::Sb::read_at_from(&f, 0).map_err(|e| SbOpenError::Sb(e))
}

fn main() {
    let matches = App::new("exfat-read-sb")
        .arg(Arg::with_name("file")
             .short("f")
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


    let sb = match sb_from_file(f) {
        Err(e) => {
            println!("Failed to read sb: {:?}", e);
            ::std::process::exit(1);
        },
        Ok(v) => v,
    };

    println!("jump boot: {}", Hs(sb.jump_boot()));
    println!("magic: {}", AsciiStr(sb.magic()));
    println!("partition offset: {}", sb.partition_offs());
    println!("volume length: {}", sb.volume_len());
    println!("fat offset: {}", sb.fat_offs());
    println!("fat len: {}", sb.fat_len());
    println!("cluster heap offset: {}", sb.cluster_heap_offs());
    println!("cluster count: {}", sb.cluster_count());
    println!("first cluster of root directory: {}", sb.first_cluster_of_root_dir());
    println!("volume serial number: {}", sb.volume_serial_num());
    println!("file system revision: {}", sb.file_system_rev());
    println!("volume flags: {}", sb.volume_flags());
    println!("bytes per sector shift: {}", sb.bytes_per_sector_shift());
    println!(" \\ bytes per sector: {}", 1<<sb.bytes_per_sector_shift());
    println!("sectors per cluster shift: {}", sb.sectors_per_cluster_shift());
    println!(" \\ sectors per cluster: {}", 1<<sb.sectors_per_cluster_shift());
    println!("number of fats: {}", sb.number_of_fats());
    println!("drive select: {}", sb.drive_select());
    println!("percent in use: {}", sb.percent_in_use());
    println!("boot signature: {}", Hs(sb.boot_signature()));
}
