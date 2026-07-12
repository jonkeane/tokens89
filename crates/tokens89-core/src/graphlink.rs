use crate::{charset, Error, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GraphLinkFile {
    pub name: String,
    pub source: Vec<u8>,
}

pub fn parse(input: &[u8]) -> Result<GraphLinkFile> {
    // GraphLink calls this an ASCII format, but it writes calculator bytes
    // directly in fields such as NAME when no textual escape is available.
    // Split the envelope as bytes so those files remain readable.
    let lines = input
        .split(|&byte| byte == b'\n')
        .map(|line| line.strip_suffix(b"\r").unwrap_or(line))
        .collect::<Vec<_>>();
    let mut lines = lines.into_iter();
    lines
        .find(|line| *line == b"\\START92\\")
        .ok_or(Error::Malformed("missing GraphLink START92 marker"))?;
    if !lines.next().is_some_and(|x| x.starts_with(b"\\COMMENT=")) {
        return Err(Error::Malformed("missing GraphLink COMMENT field"));
    }
    let name = lines
        .next()
        .and_then(|x| x.strip_prefix(b"\\NAME="))
        .ok_or(Error::Malformed("missing GraphLink NAME field"))?
        .to_owned();
    if !lines.next().is_some_and(|x| x.starts_with(b"\\FILE=")) {
        return Err(Error::Malformed("missing GraphLink FILE field"));
    }
    let rest = lines.collect::<Vec<_>>();
    let stop = rest
        .iter()
        .position(|x| *x == b"\\STOP92\\")
        .ok_or(Error::Malformed("missing GraphLink STOP92 marker"))?;
    let mut escaped = Vec::new();
    for (index, line) in rest[..stop].iter().enumerate() {
        if index != 0 {
            escaped.push(b'\r');
        }
        escaped.extend_from_slice(line);
    }
    let mut out = Vec::new();
    let b = escaped.as_slice();
    let mut i = 0;
    while i < b.len() {
        if b[i] == b'\\' {
            if let Some(end) = b[i + 1..].iter().position(|&x| x == b'\\') {
                if let Ok(key) = std::str::from_utf8(&b[i + 1..i + 1 + end]) {
                    if let Some(v) = charset::graphlink_unescape(key) {
                        out.push(v);
                        i += end + 2;
                        continue;
                    }
                }
            }
        }
        out.push(b[i]);
        i += 1;
    }
    Ok(GraphLinkFile {
        name: charset::decode_source(&name),
        source: out,
    })
}
pub fn write(name: &str, source: &[u8]) -> Result<Vec<u8>> {
    if name.len() > 8 {
        return Err(Error::name_too_long(name, name.len(), 8));
    }
    if name.is_empty() {
        return Err(Error::InvalidName(name.into()));
    }
    let mut body = String::new();
    for &b in source {
        if let Some(e) = charset::graphlink_escape(b) {
            body.push('\\');
            body.push_str(e);
            body.push('\\');
        } else if b.is_ascii() {
            body.push(b as char);
        } else {
            return Err(Error::MalformedDetail(format!(
                "byte 0x{b:02x} has no GraphLink escape"
            )));
        }
    }
    body = body.replace('\r', "\r\n");
    Ok(format!("\\START92\\\r\n\\COMMENT=saved with tokens89\r\n\\NAME={name}\r\n\\FILE={}\r\n{body}\r\n\\STOP92\\\r\n",name.to_ascii_uppercase()).into_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn envelope_roundtrip() {
        let b = write("demo", &[b'x', 140, b'\r']).unwrap();
        let f = parse(&b).unwrap();
        assert_eq!(f.name, "demo");
        assert_eq!(f.source, [b'x', 140, b'\r']);
    }

    #[test]
    fn parse_rejects_missing_required_fields() {
        assert!(matches!(
            parse(b"\\COMMENT=saved\r\n\\NAME=DEMO\r\n\\FILE=1\r\n\\STOP92\\\r\n"),
            Err(Error::Malformed(msg)) if msg.contains("START92")
        ));
        assert!(matches!(
            parse(b"\\START92\\\r\n\\NAME=DEMO\r\n\\FILE=1\r\n\\STOP92\\\r\n"),
            Err(Error::Malformed(msg)) if msg.contains("COMMENT")
        ));
        assert!(matches!(
            parse(b"\\START92\\\r\n\\COMMENT=saved\r\n\\FILE=1\r\n\\STOP92\\\r\n"),
            Err(Error::Malformed(msg)) if msg.contains("NAME")
        ));
        assert!(matches!(
            parse(b"\\START92\\\r\n\\COMMENT=saved\r\n\\NAME=DEMO\r\n"),
            Err(Error::Malformed(msg)) if msg.contains("FILE")
        ));
        assert!(matches!(
            parse(b"\\START92\\\r\n\\COMMENT=saved\r\n\\NAME=DEMO\r\n\\FILE=1\r\nabc\r\n"),
            Err(Error::Malformed(msg)) if msg.contains("STOP92")
        ));
    }

    #[test]
    fn parser_accepts_raw_calculator_bytes_in_names() {
        let text = b"\\START92\\\r\n\\COMMENT=\r\n\\NAME=\x91a\r\n\\FILE=phia.89f\r\n(a)\r\n\\STOP92\\\r\n";
        let file = parse(text).unwrap();
        assert_eq!(file.name, "φa");
        assert_eq!(file.source, b"(a)");
    }

    #[test]
    fn parse_unknown_escape_is_preserved_verbatim() {
        let text = b"\\START92\\\r\n\\COMMENT=saved\r\n\\NAME=demo\r\n\\FILE=1\r\n\\NOPE\\\r\n\\STOP92\\\r\n";
        let file = parse(text).unwrap();
        assert_eq!(file.source, b"\\NOPE\\");
    }

    #[test]
    fn parser_finds_start_marker_after_optional_preamble() {
        let text = b"transfer preamble\r\nignored\r\n\\START92\\\r\n\\COMMENT=saved\r\n\\NAME=demo\r\n\\FILE=1\r\nx\r\n\\STOP92\\\r\n";
        let file = parse(text).unwrap();
        assert_eq!(file.name, "demo");
        assert_eq!(file.source, b"x");
    }

    #[test]
    fn writer_validates_name_and_payload_encodability() {
        assert!(matches!(write("", b"x"), Err(Error::InvalidName(_))));
        assert!(matches!(
            write("toolonggg", b"x"),
            Err(Error::InvalidName(_))
        ));
        let unmapped = (0x80..=0xff)
            .find(|b| crate::charset::graphlink_escape(*b).is_none())
            .expect("at least one non-ASCII byte should be unescapable");
        assert!(matches!(
            write("demo", &[unmapped]),
            Err(Error::MalformedDetail(msg)) if msg.contains("no GraphLink escape")
        ));
    }
}
