#![no_main]

use libfuzzer_sys::fuzz_target;
use tokens89_core::tifile::TiFile;

fuzz_target!(|data: &[u8]| {
    if let Ok(file) = TiFile::parse(data) {
        let rebuilt = file.to_bytes().expect("accepted TI file must serialize");
        let reparsed = TiFile::parse(&rebuilt).expect("serialized TI file must parse");
        assert_eq!(reparsed.folder, file.folder);
        assert_eq!(reparsed.name, file.name);
        assert_eq!(reparsed.description, file.description);
        assert_eq!(reparsed.kind, file.kind);
        assert_eq!(reparsed.payload, file.payload);
    }
});
