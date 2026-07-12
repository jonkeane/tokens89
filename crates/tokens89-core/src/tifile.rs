use crate::{Error, Result};

pub const HEADER_LEN: usize = 88;
const EXTENDED_HEADER_LEN: usize = 104;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HeaderLayout {
    Legacy,
    Extended,
}
impl HeaderLayout {
    fn header_len(self) -> usize {
        match self {
            Self::Legacy => HEADER_LEN,
            Self::Extended => EXTENDED_HEADER_LEN,
        }
    }
    fn folder_offset(self) -> usize {
        match self {
            Self::Legacy => 0x0a,
            Self::Extended => 0x40,
        }
    }
    fn name_offset(self) -> usize {
        match self {
            Self::Legacy => 0x40,
            Self::Extended => 0x50,
        }
    }
    fn kind_offset(self) -> usize {
        match self {
            Self::Legacy => 0x48,
            Self::Extended => 0x58,
        }
    }
    fn flags_offset(self) -> usize {
        self.kind_offset() + 1
    }
    fn file_size_offset(self) -> usize {
        match self {
            Self::Legacy => 0x4c,
            Self::Extended => 0x5c,
        }
    }
    fn marker_offset(self) -> usize {
        match self {
            Self::Legacy => 0x50,
            Self::Extended => 0x60,
        }
    }
    fn payload_size_offset(self) -> usize {
        match self {
            Self::Legacy => 0x56,
            Self::Extended => 0x66,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableType {
    Expression,
    List,
    Matrix,
    Data,
    Text,
    String,
    Gdb,
    Figure,
    Picture,
    Program,
    Function,
    Macro,
    Zip,
    Assembler,
    Unknown(u8),
}
impl VariableType {
    pub fn id(self) -> u8 {
        match self {
            Self::Expression => 0,
            Self::List => 4,
            Self::Matrix => 6,
            Self::Data => 10,
            Self::Text => 11,
            Self::String => 12,
            Self::Gdb => 13,
            Self::Figure => 14,
            Self::Picture => 16,
            Self::Program => 18,
            Self::Function => 19,
            Self::Macro => 20,
            Self::Zip => 28,
            Self::Assembler => 33,
            Self::Unknown(v) => v,
        }
    }
    pub fn from_id(v: u8) -> Self {
        match v {
            0 => Self::Expression,
            4 => Self::List,
            6 => Self::Matrix,
            10 => Self::Data,
            11 => Self::Text,
            12 => Self::String,
            13 => Self::Gdb,
            14 => Self::Figure,
            16 => Self::Picture,
            18 => Self::Program,
            19 => Self::Function,
            20 => Self::Macro,
            28 => Self::Zip,
            33 => Self::Assembler,
            v => Self::Unknown(v),
        }
    }
    pub fn label(self) -> &'static str {
        match self {
            Self::Expression => "expression",
            Self::List => "list",
            Self::Matrix => "matrix",
            Self::Data => "data",
            Self::Text => "text",
            Self::String => "string",
            Self::Gdb => "gdb",
            Self::Figure => "figure",
            Self::Picture => "picture",
            Self::Program => "program",
            Self::Function => "function",
            Self::Macro => "macro",
            Self::Zip => "zip",
            Self::Assembler => "assembler",
            Self::Unknown(_) => "unknown",
        }
    }
}

#[derive(Debug, Clone)]
pub struct TiFile {
    pub folder: String,
    pub name: String,
    pub description: String,
    pub kind: VariableType,
    pub payload: Vec<u8>,
    header: Vec<u8>,
    layout: HeaderLayout,
    preserve_flags: bool,
    original_description: Option<String>,
}

fn name_field(value: &str) -> Result<[u8; 8]> {
    let b = crate::charset::encode_source(value).map_err(|_| Error::InvalidName(value.into()))?;
    if b.len() > 8 {
        return Err(Error::name_too_long(value, b.len(), 8));
    }
    if b.is_empty() || b.contains(&0) {
        return Err(Error::InvalidName(value.into()));
    }
    let mut out = [0; 8];
    out[..b.len()].copy_from_slice(&b);
    Ok(out)
}
fn read_name(b: &[u8]) -> String {
    let n = b.iter().position(|&x| x == 0).unwrap_or(b.len());
    crate::charset::decode_source(&b[..n])
}
pub fn checksum(payload: &[u8]) -> u16 {
    let n = payload.len() as u16;
    payload
        .iter()
        .fold(((n >> 8) + (n & 255)) as u32, |s, &b| s + b as u32) as u16
}

impl TiFile {
    pub fn new(folder: &str, name: &str, kind: VariableType, payload: Vec<u8>) -> Result<Self> {
        name_field(folder)?;
        name_field(name)?;
        let mut header = vec![0; HEADER_LEN];
        header[..8].copy_from_slice(b"**TI89**");
        header[8] = 1;
        header[0x3a] = 1;
        header[0x3c] = 0x52;
        header[0x50] = 0xa5;
        header[0x51] = 0x5a;
        Ok(Self {
            folder: folder.into(),
            name: name.into(),
            description: "saved with tokens89".into(),
            kind,
            payload,
            header,
            layout: HeaderLayout::Legacy,
            preserve_flags: false,
            original_description: None,
        })
    }
    fn parse_layout(bytes: &[u8], layout: HeaderLayout) -> Option<(usize, usize)> {
        let header_len = layout.header_len();
        if bytes.len() < header_len + 2 {
            return None;
        }
        let payload_size_offset = layout.payload_size_offset();
        let payload_len = u16::from_be_bytes([
            *bytes.get(payload_size_offset)?,
            *bytes.get(payload_size_offset + 1)?,
        ]) as usize;
        let expected = header_len.checked_add(payload_len)?.checked_add(2)?;
        if expected != bytes.len() {
            return None;
        }
        let file_size_offset = layout.file_size_offset();
        let declared = u32::from_le_bytes([
            *bytes.get(file_size_offset)?,
            *bytes.get(file_size_offset + 1)?,
            *bytes.get(file_size_offset + 2)?,
            *bytes.get(file_size_offset + 3)?,
        ]) as usize;
        if declared != bytes.len() {
            return None;
        }
        Some((payload_len, expected))
    }
    pub fn parse(bytes: &[u8]) -> Result<Self> {
        if bytes.len() < HEADER_LEN + 2 {
            return Err(Error::Malformed("file is shorter than the fixed header"));
        }
        if &bytes[..8] != b"**TI89**" {
            return Err(Error::Malformed("invalid TI-89 signature"));
        }
        let layout_and_sizes = Self::parse_layout(bytes, HeaderLayout::Legacy)
            .map(|(payload_len, expected)| (HeaderLayout::Legacy, payload_len, expected))
            .or_else(|| {
                Self::parse_layout(bytes, HeaderLayout::Extended)
                    .map(|(payload_len, expected)| (HeaderLayout::Extended, payload_len, expected))
            });
        let (layout, payload_len, expected) = match layout_and_sizes {
            Some(v) => v,
            None => {
                let payload_len = u16::from_be_bytes([bytes[0x56], bytes[0x57]]) as usize;
                let expected = HEADER_LEN
                    .checked_add(payload_len)
                    .and_then(|v| v.checked_add(2))
                    .ok_or(Error::Malformed("size overflow"))?;
                return Err(Error::MalformedDetail(format!(
                    "declared payload requires {expected} bytes, found {}",
                    bytes.len()
                )));
            }
        };
        let header_len = layout.header_len();
        let payload = bytes[header_len..header_len + payload_len].to_vec();
        let stored = u16::from_le_bytes(bytes[expected - 2..].try_into().unwrap());
        if stored != checksum(&payload) {
            return Err(Error::Malformed("checksum mismatch"));
        }
        let folder_offset = layout.folder_offset();
        let name_offset = layout.name_offset();
        let kind_offset = layout.kind_offset();
        let folder = read_name(&bytes[folder_offset..folder_offset + 8]);
        let name = read_name(&bytes[name_offset..name_offset + 8]);
        name_field(&folder)?;
        name_field(&name)?;
        let description = read_name(&bytes[0x12..0x3a]);
        Ok(Self {
            folder,
            name,
            description: description.clone(),
            kind: VariableType::from_id(bytes[kind_offset]),
            payload,
            header: bytes[..header_len].to_vec(),
            layout,
            preserve_flags: true,
            original_description: Some(description),
        })
    }
    pub fn to_bytes(&self) -> Result<Vec<u8>> {
        if self.payload.len() > u16::MAX as usize {
            return Err(Error::Malformed("payload exceeds 65535 bytes"));
        }
        let layout = self.layout;
        let mut h = if self.header.len() >= layout.header_len() {
            self.header.clone()
        } else {
            let mut v = vec![0; layout.header_len()];
            v[..self.header.len()].copy_from_slice(&self.header);
            v
        };
        let folder_offset = layout.folder_offset();
        let name_offset = layout.name_offset();
        let kind_offset = layout.kind_offset();
        let flags_offset = layout.flags_offset();
        let marker_offset = layout.marker_offset();
        let file_size_offset = layout.file_size_offset();
        let payload_size_offset = layout.payload_size_offset();
        h[folder_offset..folder_offset + 8].copy_from_slice(&name_field(&self.folder)?);
        h[name_offset..name_offset + 8].copy_from_slice(&name_field(&self.name)?);
        h[kind_offset] = self.kind.id();
        if !self.preserve_flags
            && matches!(
                self.kind,
                VariableType::Expression
                    | VariableType::List
                    | VariableType::Matrix
                    | VariableType::Text
                    | VariableType::String
                    | VariableType::Program
                    | VariableType::Function
            )
        {
            h[flags_offset + 1] =
                if self.payload.len() >= 4 && self.payload[self.payload.len() - 2] & 8 == 0 {
                    3
                } else {
                    0
                };
        }
        h[marker_offset] = 0xa5;
        h[marker_offset + 1] = 0x5a;
        if self.original_description.as_ref() != Some(&self.description) {
            h[0x12..0x3a].fill(0);
            let d = crate::charset::encode_source(&self.description)?;
            h[0x12..0x12 + d.len().min(39)].copy_from_slice(&d[..d.len().min(39)]);
        }
        let n = self.payload.len() as u16;
        h[payload_size_offset..payload_size_offset + 2].copy_from_slice(&n.to_be_bytes());
        let total = (layout.header_len() + self.payload.len() + 2) as u32;
        h[file_size_offset..file_size_offset + 4].copy_from_slice(&total.to_le_bytes());
        let mut out = Vec::with_capacity(total as usize);
        out.extend(h);
        out.extend(&self.payload);
        out.extend(checksum(&self.payload).to_le_bytes());
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn exact_layout_roundtrip() {
        let f = TiFile::new(
            "main",
            "hello",
            VariableType::Text,
            vec![0, 1, b'x', 0, 0xe0],
        )
        .unwrap();
        let b = f.to_bytes().unwrap();
        assert_eq!(&b[..8], b"**TI89**");
        assert_eq!(&b[0x56..0x58], &[0, 5]);
        assert_eq!(TiFile::parse(&b).unwrap().payload, f.payload);
    }
    #[test]
    fn checksum_is_checked() {
        let f = TiFile::new("main", "x", VariableType::Text, vec![1]).unwrap();
        let mut b = f.to_bytes().unwrap();
        *b.last_mut().unwrap() ^= 1;
        assert!(TiFile::parse(&b).is_err());
    }
    #[test]
    fn empty_header_names_are_rejected_during_parse() {
        let file = TiFile::new("main", "x", VariableType::Text, vec![0, 1, 0, 0xe0])
            .unwrap()
            .to_bytes()
            .unwrap();
        let mut empty_folder = file.clone();
        empty_folder[0x0a..0x12].fill(0);
        assert!(matches!(
            TiFile::parse(&empty_folder),
            Err(Error::InvalidName(_))
        ));
        let mut empty_name = file;
        empty_name[0x40..0x48].fill(0);
        assert!(matches!(
            TiFile::parse(&empty_name),
            Err(Error::InvalidName(_))
        ));
    }
    #[test]
    fn calculator_characters_in_descriptions_rebuild_exactly() {
        let mut bytes = TiFile::new("main", "x", VariableType::Text, vec![0, 1, 0, 0xe0])
            .unwrap()
            .to_bytes()
            .unwrap();
        bytes[0x12] = 0xf0;
        bytes[0x13] = 0;
        let parsed = TiFile::parse(&bytes).unwrap();
        assert_eq!(parsed.description, "ð");
        assert_eq!(parsed.to_bytes().unwrap(), bytes);
    }
    #[test]
    fn non_terminated_descriptions_are_preserved_until_edited() {
        let mut bytes = TiFile::new("main", "x", VariableType::Text, vec![0, 1, 0, 0xe0])
            .unwrap()
            .to_bytes()
            .unwrap();
        bytes[0x12..0x3a].copy_from_slice(b"1234567890123456789012345678901234567890");
        let parsed = TiFile::parse(&bytes).unwrap();
        assert_eq!(parsed.description.len(), 40);
        assert_eq!(parsed.to_bytes().unwrap(), bytes);

        let mut edited = parsed;
        edited.description = "changed".into();
        let rebuilt = edited.to_bytes().unwrap();
        assert_eq!(&rebuilt[0x12..0x1a], b"changed\0");
    }
    #[test]
    fn tokenized_header_flag_matches_legacy_rule() {
        let tokenized = TiFile::new(
            "main",
            "p",
            VariableType::Program,
            vec![0xe9, 0xe5, 0, 0, 0, 0xdc],
        )
        .unwrap()
        .to_bytes()
        .unwrap();
        assert_eq!(tokenized[0x4a], 3);
        let plain = TiFile::new(
            "main",
            "p",
            VariableType::Program,
            vec![b'x', 0, 0, 8, 0xdc],
        )
        .unwrap()
        .to_bytes()
        .unwrap();
        assert_eq!(plain[0x4a], 0);
    }
    #[test]
    fn calculator_character_names_round_trip() {
        let f = TiFile::new(
            "main",
            "αβ",
            VariableType::Expression,
            vec![0xe9, 1, 1, 0x1f],
        )
        .unwrap();
        let parsed = TiFile::parse(&f.to_bytes().unwrap()).unwrap();
        assert_eq!(parsed.name, "αβ");
    }
    #[test]
    fn parses_extended_header_layout_from_calculator() {
        let payload = vec![0xe9, 0x12, 0xe4, 0x00, 0xdc];
        let total = (EXTENDED_HEADER_LEN + payload.len() + 2) as u32;
        let mut b = vec![0u8; EXTENDED_HEADER_LEN];
        b[..8].copy_from_slice(b"**TI89**");
        b[8] = 1;
        b[0x3a] = 2;
        b[0x3c..0x40].copy_from_slice(&0x62u32.to_le_bytes());
        b[0x40..0x48].copy_from_slice(b"main\0\0\0\0");
        b[0x50..0x58].copy_from_slice(b"x\0\0\0\0\0\0\0");
        b[0x58] = VariableType::Program.id();
        b[0x5c..0x60].copy_from_slice(&total.to_le_bytes());
        b[0x60] = 0xa5;
        b[0x61] = 0x5a;
        b[0x66..0x68].copy_from_slice(&(payload.len() as u16).to_be_bytes());
        b.extend(&payload);
        b.extend(checksum(&payload).to_le_bytes());

        let parsed = TiFile::parse(&b).unwrap();
        assert_eq!(parsed.folder, "main");
        assert_eq!(parsed.name, "x");
        assert_eq!(parsed.kind, VariableType::Program);
        assert_eq!(parsed.payload, payload);
        let reparsed = TiFile::parse(&parsed.to_bytes().unwrap()).unwrap();
        assert_eq!(reparsed.folder, parsed.folder);
        assert_eq!(reparsed.name, parsed.name);
        assert_eq!(reparsed.kind, parsed.kind);
        assert_eq!(reparsed.payload, parsed.payload);
    }
}
