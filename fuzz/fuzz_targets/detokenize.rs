#![no_main]

use libfuzzer_sys::fuzz_target;
use tokens89_core::{detokenize, tifile::TiFile};

fuzz_target!(|data: &[u8]| {
    let payload = TiFile::parse(data)
        .map(|file| file.payload)
        .unwrap_or_else(|_| data.to_vec());
    let _ = detokenize::detokenize(&payload);
});
