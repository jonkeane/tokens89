#![allow(clippy::possible_missing_else)]

pub mod charset;
pub mod detokenize;
pub mod error;
pub mod graphlink;
pub mod group;
pub mod number;
pub mod tifile;
pub mod token;
pub mod tokenize;

pub use error::{Error, Result};
pub use group::{GroupEntry, TiGroup};
pub use tifile::{TiFile, VariableType};

pub fn normalize_line_endings(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    let mut string = false;
    while i < bytes.len() {
        if bytes[i] == b'"' {
            if string && bytes.get(i + 1) == Some(&b'"') {
                out.extend(b"\"\"");
                i += 2;
                continue;
            }
            string = !string;
            out.push(b'"');
            i += 1;
            continue;
        }
        if !string && bytes[i] == b'\r' {
            out.push(b'\r');
            i += 1;
            if bytes.get(i) == Some(&b'\n') {
                i += 1;
            }
            continue;
        }
        if !string && bytes[i] == b'\n' {
            out.push(b'\r');
            i += 1;
            continue;
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8(out).expect("normalizing ASCII control bytes preserves UTF-8")
}

pub fn display_line_endings(input: &str) -> String {
    input.replace('\r', "\n")
}

pub fn encode_source(
    source: &str,
    folder: &str,
    name: &str,
    kind: VariableType,
    tokenized: bool,
) -> Result<Vec<u8>> {
    if !matches!(
        kind,
        VariableType::Expression
            | VariableType::List
            | VariableType::Matrix
            | VariableType::Text
            | VariableType::String
            | VariableType::Program
            | VariableType::Function
    ) {
        return Err(Error::UnsupportedVariableType(kind.id()));
    }
    let calculator = charset::encode_source(&normalize_line_endings(source))?;
    let payload = tokenize::tokenize(&calculator, kind, tokenized)?;
    // VB WriteTIVar derives algebraic file types from the terminal payload
    // token rather than trusting the caller's broad expression category.
    let actual_kind = if matches!(
        kind,
        VariableType::Expression | VariableType::List | VariableType::Matrix | VariableType::String
    ) {
        match payload.last().copied() {
            Some(0x2d) => VariableType::String,
            Some(0xdb) => VariableType::Matrix,
            Some(0xd9) if payload.get(payload.len().saturating_sub(2)) == Some(&0xd9) => {
                VariableType::Matrix
            }
            Some(0xd9) => VariableType::List,
            _ => VariableType::Expression,
        }
    } else {
        kind
    };
    TiFile::new(folder, name, actual_kind, payload)?.to_bytes()
}

pub fn decode_file(bytes: &[u8]) -> Result<String> {
    let file = TiFile::parse(bytes)?;
    if !matches!(
        file.kind,
        VariableType::Expression
            | VariableType::List
            | VariableType::Matrix
            | VariableType::Text
            | VariableType::String
            | VariableType::Program
            | VariableType::Function
    ) {
        return Err(Error::UnsupportedVariableType(file.kind.id()));
    }
    let raw = detokenize::detokenize(&file.payload)?;
    Ok(display_line_endings(&charset::decode_source(&raw)))
}

pub fn open_graphlink_ascii(bytes: &[u8]) -> Result<(String, String)> {
    let file = graphlink::parse(bytes)?;
    Ok((
        file.name,
        display_line_endings(&charset::decode_source(&file.source)),
    ))
}
pub fn save_graphlink_ascii(name: &str, source: &str) -> Result<Vec<u8>> {
    let normalized = charset::encode_source(&normalize_line_endings(source))?;
    graphlink::write(name, &normalized)
}

#[cfg(test)]
mod high_level_tests {
    use super::*;
    #[test]
    fn binary_and_graphlink_high_level_round_trips() {
        let source = "()\nPrgm\n  Disp π\nEndPrgm";
        let binary = encode_source(source, "main", "demo", VariableType::Program, true).unwrap();
        assert_eq!(decode_file(&binary).unwrap(), source);
        let ascii = save_graphlink_ascii("demo", source).unwrap();
        let (name, decoded) = open_graphlink_ascii(&ascii).unwrap();
        assert_eq!(name, "demo");
        assert_eq!(decoded, source);
    }
    #[test]
    fn line_normalization_preserves_quoted_content() {
        assert_eq!(
            normalize_line_endings("a\r\nb\n\"c\nd\"\re"),
            "a\rb\r\"c\nd\"\re"
        );
        assert_eq!(normalize_line_endings("\"a\"\"b\"\n1"), "\"a\"\"b\"\r1");
    }
    #[test]
    fn archives_are_not_treated_as_token_streams() {
        let binary = TiFile::new("main", "archive", VariableType::Zip, vec![0xf8])
            .unwrap()
            .to_bytes()
            .unwrap();
        assert!(matches!(
            decode_file(&binary),
            Err(Error::UnsupportedVariableType(0x1c))
        ));
    }

    #[test]
    fn algebraic_container_type_is_derived_from_the_payload() {
        let list = encode_source("{1,2}", "main", "x", VariableType::Expression, true).unwrap();
        assert_eq!(TiFile::parse(&list).unwrap().kind, VariableType::List);

        let matrix =
            encode_source("[[1,2][3,4]]", "main", "x", VariableType::Expression, true).unwrap();
        assert_eq!(TiFile::parse(&matrix).unwrap().kind, VariableType::Matrix);

        let string =
            encode_source("\"hello\"", "main", "x", VariableType::Expression, true).unwrap();
        assert_eq!(TiFile::parse(&string).unwrap().kind, VariableType::String);

        let expression = encode_source("1+2", "main", "x", VariableType::List, true).unwrap();
        assert_eq!(
            TiFile::parse(&expression).unwrap().kind,
            VariableType::Expression
        );
    }

    #[test]
    fn source_encoding_rejects_raw_only_variable_types() {
        assert!(matches!(
            encode_source("1", "main", "x", VariableType::Zip, true),
            Err(Error::UnsupportedVariableType(0x1c))
        ));
    }

    #[test]
    fn source_encoding_reports_the_calculator_name_limit() {
        let error =
            encode_source("1", "main", "fracapprox", VariableType::Expression, true).unwrap_err();
        assert_eq!(
            error.to_string(),
            "invalid calculator name: fracapprox (10 calculator characters; maximum is 8)"
        );
    }
}
