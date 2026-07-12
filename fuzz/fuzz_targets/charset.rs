#![no_main]

use libfuzzer_sys::fuzz_target;
use tokens89_core::charset;

fuzz_target!(|data: &[u8]| {
    let decoded = charset::decode_source(data);
    let _ = charset::encode_source(&decoded);
    if let Ok(text) = std::str::from_utf8(data) {
        let _ = charset::encode_source(text);
    }
});
