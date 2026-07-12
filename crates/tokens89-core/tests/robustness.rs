use tokens89_core::{graphlink, tifile::TiFile, VariableType};

#[test]
fn arbitrary_short_graphlink_inputs_never_panic() {
    for first in 0..=u8::MAX {
        let _ = graphlink::parse(&[first]);
        for second in 0..=u8::MAX {
            let _ = graphlink::parse(&[first, second]);
        }
    }
}

#[test]
fn synthetic_tifile_truncation_families_are_rejected() {
    let cases = [
        (VariableType::Program, 0_usize),
        (VariableType::Program, 1_usize),
        (VariableType::Text, 5_usize),
        (VariableType::Expression, 17_usize),
        (VariableType::List, 63_usize),
        (VariableType::Matrix, 127_usize),
        (VariableType::Zip, 255_usize),
    ];
    for (index, (kind, payload_len)) in cases.into_iter().enumerate() {
        let payload = (0..payload_len)
            .map(|offset| (offset as u8).wrapping_mul(37).wrapping_add(index as u8))
            .collect();
        let bytes = TiFile::new("main", "sample", kind, payload)
            .unwrap()
            .to_bytes()
            .unwrap();
        assert!(TiFile::parse(&bytes).is_ok(), "case {index}");
        for end in 0..bytes.len() {
            assert!(
                TiFile::parse(&bytes[..end]).is_err(),
                "accepted synthetic case {index} truncated to {end} bytes"
            );
        }
    }
}
