use proptest::{
    prelude::*,
    sample::select,
    string::{string_regex, RegexGeneratorStrategy},
};
use tokens89_core::{
    charset, detokenize, graphlink, number, tifile::checksum, tokenize, TiFile, VariableType,
};

#[derive(Debug, Clone)]
enum Expr {
    Number(u16),
    Variable(char),
    String(String),
    List(Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
    Unary(&'static str, Box<Expr>),
    Binary(Box<Expr>, &'static str, Box<Expr>),
    BinaryCall(&'static str, Box<Expr>, Box<Expr>),
}

impl Expr {
    fn render(&self) -> String {
        match self {
            Self::Number(n) => n.to_string(),
            Self::Variable(v) => v.to_string(),
            Self::String(value) => format!("\"{value}\""),
            Self::List(a, b) => format!("{{{},{}}}", a.render(), b.render()),
            Self::Paren(inner) => format!("({})", inner.render()),
            Self::Unary(name, inner) => format!("{name}({})", inner.render()),
            Self::Binary(left, op, right) => format!("({}{}{})", left.render(), op, right.render()),
            Self::BinaryCall(name, a, b) => format!("{name}({},{})", a.render(), b.render()),
        }
    }
}

fn calc_name() -> RegexGeneratorStrategy<String> {
    string_regex("[a-z][a-z0-9_]{0,7}").expect("valid calculator name regex")
}

fn source_kind() -> impl Strategy<Value = VariableType> {
    prop_oneof![
        Just(VariableType::Expression),
        Just(VariableType::List),
        Just(VariableType::Matrix),
        Just(VariableType::Data),
        Just(VariableType::Text),
        Just(VariableType::String),
        Just(VariableType::Gdb),
        Just(VariableType::Figure),
        Just(VariableType::Picture),
        Just(VariableType::Program),
        Just(VariableType::Function),
        Just(VariableType::Macro),
        Just(VariableType::Zip),
        Just(VariableType::Assembler),
        any::<u8>()
            .prop_filter("exclude mapped variable IDs", |id| {
                !matches!(
                    *id,
                    0 | 4 | 6 | 10 | 11 | 12 | 13 | 14 | 16 | 18 | 19 | 20 | 28 | 33
                )
            })
            .prop_map(VariableType::Unknown),
    ]
}

fn expr_strategy() -> impl Strategy<Value = Expr> {
    let atom = prop_oneof![
        (0u16..50_000).prop_map(Expr::Number),
        select((b'a'..=b'z').map(char::from).collect::<Vec<_>>()).prop_map(Expr::Variable),
        string_regex("[a-z0-9 ]{0,12}")
            .expect("valid string literal regex")
            .prop_map(Expr::String),
    ];
    atom.prop_recursive(5, 128, 10, |inner| {
        prop_oneof![
            inner.clone().prop_map(|v| Expr::Paren(Box::new(v))),
            (
                select(vec!["sin", "cos", "abs", "sqrt", "exp", "floor"]),
                inner.clone()
            )
                .prop_map(|(name, value)| Expr::Unary(name, Box::new(value))),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| Expr::List(Box::new(a), Box::new(b))),
            (
                inner.clone(),
                select(vec!["+", "-", "*", "/", "^"]),
                inner.clone()
            )
                .prop_map(|(left, op, right)| {
                    Expr::Binary(Box::new(left), op, Box::new(right))
                }),
            (
                select(vec!["min", "max", "gcd", "mod", "solve", "nCr"]),
                inner.clone(),
                inner
            )
                .prop_map(|(name, a, b)| Expr::BinaryCall(
                    name,
                    Box::new(a),
                    Box::new(b)
                )),
        ]
    })
}

#[derive(Debug, Clone)]
enum Statement {
    Disp(Expr),
    Loop(Box<Statement>),
    If(Box<Statement>),
    Try(Box<Statement>, Box<Statement>),
}

impl Statement {
    fn render(&self) -> String {
        match self {
            Self::Disp(expr) => format!("Disp {}", expr.render()),
            Self::Loop(body) => format!("Loop\r{}\rEndLoop", body.render()),
            Self::If(body) => format!("If x Then\r{}\rEndIf", body.render()),
            Self::Try(body, fallback) => {
                format!(
                    "Try\r{}\rElse\r{}\rEndTry",
                    body.render(),
                    fallback.render()
                )
            }
        }
    }
}

fn statement_strategy() -> impl Strategy<Value = Statement> {
    expr_strategy()
        .prop_map(Statement::Disp)
        .prop_recursive(4, 64, 4, |inner| {
            prop_oneof![
                inner
                    .clone()
                    .prop_map(|body| Statement::Loop(Box::new(body))),
                inner.clone().prop_map(|body| Statement::If(Box::new(body))),
                (inner.clone(), inner).prop_map(|(body, fallback)| {
                    Statement::Try(Box::new(body), Box::new(fallback))
                }),
            ]
        })
}

fn graphlink_byte() -> impl Strategy<Value = u8> {
    select(
        (0..=u8::MAX)
            .filter(|byte| {
                *byte != b'\n'
                    && *byte != b'\\'
                    && (byte.is_ascii() || charset::graphlink_escape(*byte).is_some())
            })
            .collect::<Vec<_>>(),
    )
}

fn build_extended_container(
    folder: &str,
    name: &str,
    kind: VariableType,
    payload: &[u8],
) -> Vec<u8> {
    const EXTENDED_HEADER_LEN: usize = 104;
    let total = (EXTENDED_HEADER_LEN + payload.len() + 2) as u32;
    let mut bytes = vec![0u8; EXTENDED_HEADER_LEN];
    bytes[..8].copy_from_slice(b"**TI89**");
    bytes[8] = 1;
    bytes[0x3a] = 2;
    bytes[0x3c..0x40].copy_from_slice(&0x62u32.to_le_bytes());

    bytes[0x40..0x48].fill(0);
    bytes[0x50..0x58].fill(0);
    bytes[0x40..0x40 + folder.len()].copy_from_slice(folder.as_bytes());
    bytes[0x50..0x50 + name.len()].copy_from_slice(name.as_bytes());
    bytes[0x58] = kind.id();
    bytes[0x5c..0x60].copy_from_slice(&total.to_le_bytes());
    bytes[0x60] = 0xa5;
    bytes[0x61] = 0x5a;
    bytes[0x66..0x68].copy_from_slice(&(payload.len() as u16).to_be_bytes());
    bytes.extend(payload);
    bytes.extend(checksum(payload).to_le_bytes());
    bytes
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(512))]

    #[test]
    fn tifile_new_parse_roundtrip_preserves_fields(
        folder in calc_name(),
        name in calc_name(),
        kind in source_kind(),
        payload in prop::collection::vec(any::<u8>(), 0..512),
    ) {
        let file = TiFile::new(&folder, &name, kind, payload.clone()).expect("valid generated tifile");
        let bytes = file.to_bytes().expect("serialize generated tifile");
        let parsed = TiFile::parse(&bytes).expect("parse generated tifile bytes");
        prop_assert_eq!(parsed.folder, folder);
        prop_assert_eq!(parsed.name, name);
        prop_assert_eq!(parsed.kind, kind);
        prop_assert_eq!(parsed.payload, payload);
    }

    #[test]
    fn tifile_extended_layout_parse_and_rebuild_is_exact(
        folder in calc_name(),
        name in calc_name(),
        kind in source_kind(),
        payload in prop::collection::vec(any::<u8>(), 0..512),
    ) {
        let bytes = build_extended_container(&folder, &name, kind, &payload);
        let parsed = TiFile::parse(&bytes).expect("parse generated extended layout");
        prop_assert_eq!(&parsed.folder, &folder);
        prop_assert_eq!(&parsed.name, &name);
        prop_assert_eq!(parsed.kind, kind);
        prop_assert_eq!(&parsed.payload, &payload);
        prop_assert_eq!(parsed.to_bytes().expect("rebuild extended layout"), bytes);
    }

    #[test]
    fn generated_expression_payloads_are_stable_under_detokenize_retokenize(expr in expr_strategy()) {
        let source = expr.render();
        let payload = tokenize::tokenize(source.as_bytes(), VariableType::Expression, true)
            .expect("tokenize generated expression");
        let canonical = detokenize::detokenize(&payload)
            .expect("detokenize generated expression payload");
        let canonical_once = detokenize::detokenize(
            &tokenize::tokenize(&canonical, VariableType::Expression, true)
                .expect("retokenize canonical expression")
        )
        .expect("detokenize canonical expression payload");
        let canonical_twice = detokenize::detokenize(
            &tokenize::tokenize(&canonical_once, VariableType::Expression, true)
                .expect("retokenize canonical expression second pass")
        )
        .expect("detokenize canonical expression second pass payload");
        prop_assert_eq!(canonical_twice, canonical_once);
    }

    #[test]
    fn generated_balanced_programs_roundtrip(statement in statement_strategy()) {
        let source = format!("()\rPrgm\r{}\rEndPrgm", statement.render());
        let payload = tokenize::tokenize(source.as_bytes(), VariableType::Program, true)
            .expect("tokenize generated balanced program");
        let decoded = detokenize::detokenize(&payload)
            .expect("detokenize generated balanced program");
        let rebuilt = tokenize::tokenize(&decoded, VariableType::Program, true)
            .expect("retokenize canonical generated program");
        let canonical_once = detokenize::detokenize(&rebuilt)
            .expect("detokenize canonical generated program");
        let canonical_twice = detokenize::detokenize(
            &tokenize::tokenize(&canonical_once, VariableType::Program, true)
                .expect("retokenize canonical generated program a second time"),
        )
        .expect("detokenize canonical generated program a second time");
        prop_assert_eq!(canonical_twice, canonical_once);
    }

    #[test]
    fn integer_and_fraction_encodings_roundtrip(
        value in any::<u128>(),
        negative in any::<bool>(),
        numerator in any::<u128>(),
        denominator in 1u128..=u128::MAX,
    ) {
        let integer_source = format!("{}{value}", if negative { "-" } else { "" });
        let integer = number::encode_integer(&integer_source).expect("encode generated integer");
        let (decoded_integer, start) = number::decode_integer(&integer, integer.len())
            .expect("decode generated integer");
        prop_assert_eq!(start, 0);
        prop_assert_eq!(
            decoded_integer,
            format!("{}{value}", if negative { "−" } else { "" })
        );

        let fraction = number::encode_fraction(numerator, denominator, negative)
            .expect("encode generated fraction");
        let (decoded_fraction, start) = number::decode_fraction(&fraction, fraction.len())
            .expect("decode generated fraction");
        prop_assert_eq!(start, 0);
        prop_assert_eq!(
            decoded_fraction,
            format!("{}{numerator}/{denominator}", if negative { "−" } else { "" })
        );
    }

    #[test]
    fn finite_float_tokens_reach_a_stable_canonical_encoding(value in any::<f64>()) {
        prop_assume!(value.is_finite());
        let encoded = number::encode_float(&value.to_string()).expect("encode finite float");
        let (canonical, start) = number::decode_float(&encoded, encoded.len())
            .expect("decode generated float");
        prop_assert_eq!(start, 0);
        let rebuilt = number::encode_float(&canonical).expect("re-encode canonical float");
        prop_assert_eq!(rebuilt, encoded);
    }

    #[test]
    fn generated_graphlink_files_roundtrip(
        name in calc_name(),
        source in prop::collection::vec(graphlink_byte(), 0..512),
    ) {
        let bytes = graphlink::write(&name, &source).expect("write generated GraphLink file");
        let parsed = graphlink::parse(&bytes).expect("parse generated GraphLink file");
        prop_assert_eq!(parsed.name, name);
        prop_assert_eq!(parsed.source, source);
    }

    #[test]
    fn arbitrary_bounded_inputs_are_total(input in prop::collection::vec(any::<u8>(), 0..512)) {
        let _ = TiFile::parse(&input);
        let _ = detokenize::detokenize(&input);
        let _ = graphlink::parse(&input);
        let decoded = charset::decode_source(&input);
        let _ = charset::encode_source(&decoded);
    }

    #[test]
    fn tifile_mutations_are_rejected(
        folder in calc_name(),
        name in calc_name(),
        kind in source_kind(),
        payload in prop::collection::vec(any::<u8>(), 0..128),
    ) {
        let valid = TiFile::new(&folder, &name, kind, payload)
            .expect("build valid tifile")
            .to_bytes()
            .expect("serialize valid tifile");

        let mut bad_checksum = valid.clone();
        let last = bad_checksum.len() - 1;
        bad_checksum[last] ^= 1;
        prop_assert!(TiFile::parse(&bad_checksum).is_err());

        let mut bad_signature = valid.clone();
        bad_signature[0] ^= 1;
        prop_assert!(TiFile::parse(&bad_signature).is_err());

        let mut appended = valid.clone();
        appended.push(0);
        prop_assert!(TiFile::parse(&appended).is_err());

        prop_assert!(TiFile::parse(&valid[..valid.len() - 1]).is_err());

        let mut bad_payload_size = valid.clone();
        let declared = u16::from_be_bytes([bad_payload_size[0x56], bad_payload_size[0x57]]);
        bad_payload_size[0x56..0x58].copy_from_slice(&declared.wrapping_add(1).to_be_bytes());
        prop_assert!(TiFile::parse(&bad_payload_size).is_err());

        let mut bad_total_size = valid;
        let declared_total = u32::from_le_bytes([
            bad_total_size[0x4c],
            bad_total_size[0x4d],
            bad_total_size[0x4e],
            bad_total_size[0x4f],
        ]);
        bad_total_size[0x4c..0x50].copy_from_slice(&declared_total.wrapping_add(1).to_le_bytes());
        prop_assert!(TiFile::parse(&bad_total_size).is_err());
    }

    #[test]
    fn extended_tifile_structural_mutations_are_rejected(
        folder in calc_name(),
        name in calc_name(),
        kind in source_kind(),
        payload in prop::collection::vec(any::<u8>(), 0..128),
    ) {
        let valid = build_extended_container(&folder, &name, kind, &payload);

        let mut bad_checksum = valid.clone();
        let last = bad_checksum.len() - 1;
        bad_checksum[last] ^= 1;
        prop_assert!(TiFile::parse(&bad_checksum).is_err());

        let mut bad_payload_size = valid.clone();
        let declared = u16::from_be_bytes([bad_payload_size[0x66], bad_payload_size[0x67]]);
        bad_payload_size[0x66..0x68]
            .copy_from_slice(&declared.wrapping_add(1).to_be_bytes());
        prop_assert!(TiFile::parse(&bad_payload_size).is_err());

        let mut bad_total_size = valid;
        let declared_total = u32::from_le_bytes([
            bad_total_size[0x5c],
            bad_total_size[0x5d],
            bad_total_size[0x5e],
            bad_total_size[0x5f],
        ]);
        bad_total_size[0x5c..0x60]
            .copy_from_slice(&declared_total.wrapping_add(1).to_le_bytes());
        prop_assert!(TiFile::parse(&bad_total_size).is_err());
    }
}
