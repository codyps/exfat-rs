extern crate exfat;
extern crate clap;
use ::clap::{Arg, App, SubCommand};
use ::std::path::Path;

#[derive(Debug)]
enum SbOpenError {
    Io(::std::io::Error),
    Sb(::exfat::SbInitIoError)
}

fn sb_from_file<P: AsRef<Path>>(path: P) -> Result<exfat::Sb, SbOpenError> {
    let f = try!(::std::fs::File::open(path)
                 .map_err(|e| SbOpenError::Io(e)));

    exfat::Sb::read_at_from(&f, 512).map_err(|e| SbOpenError::Sb(e))
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

    let sb = sb_from_file(f).unwrap();

    println!("read superblock");
}
