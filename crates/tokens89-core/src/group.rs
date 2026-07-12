use crate::{charset, tifile::checksum, Error, Result, TiFile, VariableType};

const HEADER_LEN: usize = 64;
const DIRECTORY_ENTRY_LEN: usize = 16;
const DATA_MARKER: [u8; 2] = [0xa5, 0x5a];

/// A variable stored in a TI-89 group file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GroupEntry {
    pub folder: String,
    pub name: String,
    pub kind: VariableType,
    pub attributes: [u8; 3],
    pub payload: Vec<u8>,
}

impl GroupEntry {
    pub fn new(folder: &str, name: &str, kind: VariableType, payload: Vec<u8>) -> Result<Self> {
        name_field(folder)?;
        name_field(name)?;
        let mut attributes = [0; 3];
        if matches!(
            kind,
            VariableType::Expression
                | VariableType::List
                | VariableType::Matrix
                | VariableType::Text
                | VariableType::String
                | VariableType::Program
                | VariableType::Function
        ) && payload.len() >= 4
            && payload[payload.len() - 2] & 8 == 0
        {
            attributes[1] = 3;
        }
        Ok(Self {
            folder: folder.into(),
            name: name.into(),
            kind,
            attributes,
            payload,
        })
    }
}

impl TryFrom<TiFile> for GroupEntry {
    type Error = Error;

    fn try_from(file: TiFile) -> Result<Self> {
        Self::new(&file.folder, &file.name, file.kind, file.payload)
    }
}

/// A `.89g` group containing variables from one or more calculator folders.
#[derive(Debug, Clone)]
pub struct TiGroup {
    pub description: String,
    pub entries: Vec<GroupEntry>,
    header: Vec<u8>,
    original_description: Option<String>,
    original_entries: Option<Vec<GroupEntry>>,
    original_bytes: Option<Vec<u8>>,
}

fn name_field(value: &str) -> Result<[u8; 8]> {
    let bytes = charset::encode_source(value).map_err(|_| Error::InvalidName(value.into()))?;
    if bytes.len() > 8 {
        return Err(Error::name_too_long(value, bytes.len(), 8));
    }
    if bytes.is_empty() || bytes.contains(&0) {
        return Err(Error::InvalidName(value.into()));
    }
    let mut field = [0; 8];
    field[..bytes.len()].copy_from_slice(&bytes);
    Ok(field)
}

fn read_name(bytes: &[u8]) -> String {
    let len = bytes
        .iter()
        .position(|&byte| byte == 0)
        .unwrap_or(bytes.len());
    charset::decode_source(&bytes[..len])
}

fn supported_signature(bytes: &[u8]) -> bool {
    matches!(bytes, b"**TI89**" | b"**TI92P*" | b"**V200**")
}

impl TiGroup {
    /// Creates a group from already prepared group entries.
    pub fn new(entries: Vec<GroupEntry>) -> Result<Self> {
        for entry in &entries {
            name_field(&entry.folder)?;
            name_field(&entry.name)?;
        }
        let mut header = vec![0; HEADER_LEN];
        header[..8].copy_from_slice(b"**TI89**");
        header[8] = 1;
        Ok(Self {
            description: "saved with tokens89".into(),
            entries,
            header,
            original_description: None,
            original_entries: None,
            original_bytes: None,
        })
    }

    /// Creates a group from ordinary single-variable TI files.
    pub fn from_files(files: Vec<TiFile>) -> Result<Self> {
        let entries = files
            .into_iter()
            .map(GroupEntry::try_from)
            .collect::<Result<Vec<_>>>()?;
        Self::new(entries)
    }

    /// Parses a TI-89, TI-92 Plus, or Voyage 200 group file.
    pub fn parse(bytes: &[u8]) -> Result<Self> {
        if bytes.len() < HEADER_LEN + DATA_MARKER.len() {
            return Err(Error::Malformed("group file is shorter than its header"));
        }
        if !supported_signature(&bytes[..8]) {
            return Err(Error::Malformed("invalid TI group signature"));
        }
        let directory_count = u16::from_le_bytes([bytes[0x3a], bytes[0x3b]]) as usize;
        if directory_count == 0 {
            return Err(Error::Malformed("group directory is empty"));
        }
        let directory_end = HEADER_LEN
            .checked_add(
                directory_count
                    .checked_mul(DIRECTORY_ENTRY_LEN)
                    .ok_or(Error::Malformed("group directory size overflow"))?,
            )
            .ok_or(Error::Malformed("group directory size overflow"))?;
        let data_start = directory_end
            .checked_add(DATA_MARKER.len())
            .ok_or(Error::Malformed("group data offset overflow"))?;
        if data_start > bytes.len() || bytes.get(directory_end..data_start) != Some(&DATA_MARKER) {
            return Err(Error::Malformed("invalid group data marker"));
        }
        let declared_data_start = u32::from_le_bytes(
            bytes[0x3c..0x40]
                .try_into()
                .expect("fixed header contains the data offset"),
        ) as usize;
        if declared_data_start != data_start {
            return Err(Error::Malformed(
                "group data offset does not match its directory",
            ));
        }

        let mut entries = Vec::with_capacity(directory_count.saturating_sub(1));
        let mut folder = None;
        let mut payload_start = data_start;
        let mut variables_in_folder = 0_u16;
        let mut declared_variables = None;

        for index in 0..directory_count {
            let start = HEADER_LEN + index * DIRECTORY_ENTRY_LEN;
            let directory = &bytes[start..start + DIRECTORY_ENTRY_LEN];
            let name = read_name(&directory[..8]);
            name_field(&name)?;
            let kind = VariableType::from_id(directory[8]);
            let attributes: [u8; 3] = directory[9..12].try_into().unwrap();
            let end = u32::from_le_bytes(directory[12..16].try_into().unwrap()) as usize;
            if end < payload_start || end > bytes.len() {
                return Err(Error::Malformed("group variable offset is out of bounds"));
            }

            if directory[8] == 0x1f {
                if end != payload_start {
                    return Err(Error::Malformed("group folder record has variable data"));
                }
                if let Some(expected) = declared_variables {
                    if variables_in_folder != expected {
                        return Err(Error::Malformed("group folder variable count mismatch"));
                    }
                }
                folder = Some(name);
                variables_in_folder = 0;
                declared_variables = Some(u16::from_le_bytes([attributes[1], attributes[2]]));
                continue;
            }

            let current_folder = folder.as_deref().ok_or(Error::Malformed(
                "group variable appears before a folder record",
            ))?;
            let record = &bytes[payload_start..end];
            if record.len() < 8 || record[..4] != [0; 4] {
                return Err(Error::Malformed("invalid group variable data prefix"));
            }
            let payload_len = u16::from_be_bytes([record[4], record[5]]) as usize;
            let payload_end = 6 + payload_len;
            let checksum_end = payload_end + 2;
            if checksum_end > record.len()
                || !matches!(record.len() - checksum_end, 0 | 2)
                || record[checksum_end..].iter().any(|&byte| byte != 0)
            {
                return Err(Error::Malformed("group variable data size mismatch"));
            }
            let payload = &record[6..payload_end];
            let stored_checksum =
                u16::from_le_bytes([record[payload_end], record[payload_end + 1]]);
            if stored_checksum != checksum(payload) {
                return Err(Error::Malformed("group variable checksum mismatch"));
            }
            entries.push(GroupEntry {
                folder: current_folder.into(),
                name,
                kind,
                attributes,
                payload: payload.to_vec(),
            });
            variables_in_folder = variables_in_folder
                .checked_add(1)
                .ok_or(Error::Malformed("too many variables in group folder"))?;
            payload_start = end;
        }
        if let Some(expected) = declared_variables {
            if variables_in_folder != expected {
                return Err(Error::Malformed("group folder variable count mismatch"));
            }
        }
        if payload_start != bytes.len() {
            return Err(Error::Malformed(
                "group contains unreferenced trailing data",
            ));
        }

        let description = read_name(&bytes[0x12..0x3a]);
        Ok(Self {
            description: description.clone(),
            original_entries: Some(entries.clone()),
            entries,
            header: bytes[..HEADER_LEN].to_vec(),
            original_description: Some(description),
            original_bytes: Some(bytes.to_vec()),
        })
    }

    /// Serializes this group as an `.89g`-compatible container.
    pub fn to_bytes(&self) -> Result<Vec<u8>> {
        if self.original_description.as_ref() == Some(&self.description)
            && self.original_entries.as_ref() == Some(&self.entries)
        {
            if let Some(bytes) = &self.original_bytes {
                return Ok(bytes.clone());
            }
        }
        for entry in &self.entries {
            name_field(&entry.folder)?;
            name_field(&entry.name)?;
            if entry.payload.len() > u16::MAX as usize {
                return Err(Error::Malformed(
                    "group variable payload exceeds 65535 bytes",
                ));
            }
        }

        let mut runs = Vec::new();
        for entry in &self.entries {
            match runs.last_mut() {
                Some((folder, count)) if folder == &entry.folder => *count += 1_u16,
                _ => runs.push((entry.folder.clone(), 1_u16)),
            }
        }
        let directory_count = self
            .entries
            .len()
            .checked_add(runs.len())
            .ok_or(Error::Malformed("group directory size overflow"))?;
        let directory_count = u16::try_from(directory_count)
            .map_err(|_| Error::Malformed("group has too many directory entries"))?;
        if directory_count == 0 {
            return Err(Error::Malformed("group must contain at least one variable"));
        }
        let data_start = HEADER_LEN + directory_count as usize * DIRECTORY_ENTRY_LEN + 2;
        let mut header = self.header.clone();
        header.resize(HEADER_LEN, 0);
        if !supported_signature(&header[..8]) {
            header[..8].copy_from_slice(b"**TI89**");
        }
        header[0x3a..0x3c].copy_from_slice(&directory_count.to_le_bytes());
        header[0x3c..0x40].copy_from_slice(
            &u32::try_from(data_start)
                .map_err(|_| Error::Malformed("group data offset exceeds 32 bits"))?
                .to_le_bytes(),
        );
        if self.original_description.as_ref() != Some(&self.description) {
            header[0x12..0x3a].fill(0);
            let description = charset::encode_source(&self.description)?;
            let len = description.len().min(39);
            header[0x12..0x12 + len].copy_from_slice(&description[..len]);
        }

        let mut directory = Vec::with_capacity(directory_count as usize * DIRECTORY_ENTRY_LEN);
        let mut data = Vec::new();
        let mut run_index = 0;
        let mut current_folder: Option<&str> = None;
        for entry in &self.entries {
            if current_folder != Some(&entry.folder) {
                let (folder, count) = &runs[run_index];
                run_index += 1;
                directory.extend(name_field(folder)?);
                directory.push(0x1f);
                directory.push(0);
                directory.extend(count.to_le_bytes());
                directory.extend(
                    u32::try_from(data_start + data.len())
                        .map_err(|_| Error::Malformed("group exceeds 32-bit file size"))?
                        .to_le_bytes(),
                );
                current_folder = Some(&entry.folder);
            }
            data.extend([0; 4]);
            data.extend((entry.payload.len() as u16).to_be_bytes());
            data.extend(&entry.payload);
            data.extend(checksum(&entry.payload).to_le_bytes());
            directory.extend(name_field(&entry.name)?);
            directory.push(entry.kind.id());
            directory.extend(entry.attributes);
            directory.extend(
                u32::try_from(data_start + data.len())
                    .map_err(|_| Error::Malformed("group exceeds 32-bit file size"))?
                    .to_le_bytes(),
            );
        }

        let mut output = Vec::with_capacity(data_start + data.len());
        output.extend(header);
        output.extend(directory);
        output.extend(DATA_MARKER);
        output.extend(data);
        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn newly_created_group_round_trips() {
        let entries = vec![
            GroupEntry::new("main", "one", VariableType::Expression, vec![1, 2, 3, 4]).unwrap(),
            GroupEntry::new("games", "two", VariableType::Program, vec![5, 6, 7, 8]).unwrap(),
        ];
        let bytes = TiGroup::new(entries.clone()).unwrap().to_bytes().unwrap();
        let parsed = TiGroup::parse(&bytes).unwrap();
        assert_eq!(parsed.entries, entries);
        assert_eq!(parsed.to_bytes().unwrap(), bytes);
    }

    #[test]
    fn group_can_be_built_from_single_variable_files() {
        let file = TiFile::new("main", "demo", VariableType::Text, vec![0, 1, 2, 3]).unwrap();
        let group = TiGroup::from_files(vec![file]).unwrap();
        let parsed = TiGroup::parse(&group.to_bytes().unwrap()).unwrap();
        assert_eq!(parsed.entries.len(), 1);
        assert_eq!(parsed.entries[0].folder, "main");
        assert_eq!(parsed.entries[0].name, "demo");
        assert_eq!(parsed.entries[0].kind, VariableType::Text);
        assert_eq!(parsed.entries[0].payload, [0, 1, 2, 3]);
    }

    #[test]
    fn group_variable_checksums_are_validated() {
        let entry =
            GroupEntry::new("main", "demo", VariableType::Program, vec![1, 2, 3, 4]).unwrap();
        let mut bytes = TiGroup::new(vec![entry]).unwrap().to_bytes().unwrap();
        let last = bytes.len() - 1;
        bytes[last] ^= 1;
        assert!(matches!(
            TiGroup::parse(&bytes),
            Err(Error::Malformed("group variable checksum mismatch"))
        ));
    }
}
