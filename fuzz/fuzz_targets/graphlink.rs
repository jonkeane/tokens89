#![no_main]

use libfuzzer_sys::fuzz_target;
use tokens89_core::graphlink;

fuzz_target!(|data: &[u8]| {
    if let Ok(file) = graphlink::parse(data) {
        if let Ok(rebuilt) = graphlink::write(&file.name, &file.source) {
            let reparsed = graphlink::parse(&rebuilt).expect("written GraphLink file must parse");
            assert_eq!(reparsed.name, file.name);
            assert_eq!(reparsed.source, file.source);
        }
    }
});
