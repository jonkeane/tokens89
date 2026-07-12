#![no_main]

use libfuzzer_sys::fuzz_target;
use tokens89_core::TiGroup;

fuzz_target!(|data: &[u8]| {
    if let Ok(group) = TiGroup::parse(data) {
        let rebuilt = group.to_bytes().expect("accepted TI group must serialize");
        let reparsed = TiGroup::parse(&rebuilt).expect("serialized TI group must parse");
        assert_eq!(reparsed.description, group.description);
        assert_eq!(reparsed.entries, group.entries);
    }
});
