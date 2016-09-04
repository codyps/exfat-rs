extern crate io_at;

use io_at::{ReadAt,WriteAt};

enum SbInitError {
    NoMagic,
}

struct Sb {
    raw: [u8;512],
}

/**
 * An Exfat super block
 */
impl Sb {
    pub fn from(s: [u8;512]) -> Result<Self, SbInitError> {
        /* validate Sb */
        Ok(Sb { raw: s })
    }
}

enum ExfatInitError {
    SbInitError(SbInitError)
}

struct Exfat<S: ReadAt> {
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
