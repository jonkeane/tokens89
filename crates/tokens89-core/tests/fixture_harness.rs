use tokens89_core::{
    detokenize, normalize_line_endings,
    tifile::{checksum, TiFile},
    tokenize, VariableType,
};

fn hex(input: &str) -> Vec<u8> {
    input
        .split_whitespace()
        .map(|part| u8::from_str_radix(part.trim_start_matches("0x"), 16).unwrap())
        .collect()
}
fn assert_binary(actual: &[u8], expected_hex: &str) {
    assert_eq!(actual, hex(expected_hex));
}
fn extract_payload(container: &[u8]) -> Vec<u8> {
    TiFile::parse(container).unwrap().payload
}
#[test]
fn hexadecimal_exact_payload_and_checksum_helpers() {
    let payload = hex("00 01 78 00 e0");
    assert_binary(&payload, "00 01 78 00 e0");
    assert_eq!(checksum(&payload), 0x015e);
    let file = TiFile::new("main", "x", VariableType::Text, payload.clone())
        .unwrap()
        .to_bytes()
        .unwrap();
    assert_eq!(extract_payload(&file), payload);
}
#[test]
fn synthetic_fixtures_execute_hermetically() {
    assert_eq!(normalize_line_endings("a\r\nb\nc\rd"), "a\rb\rc\rd");

    let expected = hex("00 01 68 69 00 e0");
    let payload = tokenize::tokenize(b"hi", VariableType::Text, true).unwrap();
    assert_eq!(payload, expected);
    let container = TiFile::new("main", "hello", VariableType::Text, payload)
        .unwrap()
        .to_bytes()
        .unwrap();
    let parsed = TiFile::parse(&container).unwrap();
    assert_eq!(parsed.payload, expected);

    let source = b"()\rPrgm\rEndPrgm";
    let expected = hex("e9 12 e4 00 e8 19 e4 e5 00 00 00 dc");
    let payload = tokenize::tokenize(source, VariableType::Program, true).unwrap();
    assert_eq!(payload, expected);
    assert_eq!(detokenize::detokenize(&payload).unwrap(), source);

    assert!(TiFile::parse(&[0x2a, 0x00]).is_err());

    let mut bytes = TiFile::new("main", "bad", VariableType::Text, vec![0, 1, 0, 0xe0])
        .unwrap()
        .to_bytes()
        .unwrap();
    *bytes.last_mut().unwrap() ^= 1;
    assert!(TiFile::parse(&bytes).is_err());
}
