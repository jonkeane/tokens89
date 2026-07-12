# TI-89 single-variable file format

All offsets are zero based.

Two layouts appear in the wild and are now supported by the parser:

1. Legacy layout with an 88-byte (`0x58`) header.
2. Extended layout with a 104-byte (`0x68`) header (seen in calculator-exported
	files such as TI-89 Titanium `.89p`).

The payload length and declared file-size fields move in the extended layout,
but checksum semantics remain the same.

| Offset | Size | Meaning |
| --- | ---: | --- |
| `0x00` | 8 | signature `**TI89**` |
| `0x08` | 2 | fixed bytes (`01 00`) |
| `0x0a` | 8 | zero-padded folder name |
| `0x12` | 40 | zero-terminated description |
| `0x3a` | 6 | fixed/unknown header bytes |
| `0x40` | 8 | zero-padded variable name |
| `0x48` | 1 | variable type ID |
| `0x49` | 3 | flags/unknown bytes |
| `0x4c` | 4 | little-endian total file size |
| `0x50` | 6 | marker/unknown bytes; starts `A5 5A` |
| `0x56` | 2 | big-endian payload size |
| `0x58` | n | payload |
| end | 2 | little-endian checksum |

## Extended layout (104-byte header)

| Offset | Size | Meaning |
| --- | ---: | --- |
| `0x00` | 8 | signature `**TI89**` |
| `0x08` | 2 | fixed bytes (`01 00`) |
| `0x12` | 40 | zero-terminated description |
| `0x40` | 8 | zero-padded folder name |
| `0x50` | 8 | zero-padded variable name |
| `0x58` | 1 | variable type ID |
| `0x59` | 3 | flags/unknown bytes |
| `0x5c` | 4 | little-endian total file size |
| `0x60` | 6 | marker/unknown bytes; starts `A5 5A` |
| `0x66` | 2 | big-endian payload size |
| `0x68` | n | payload |
| end | 2 | little-endian checksum |

The checksum is the low 16 bits of the sum of the two payload-size bytes and
all payload bytes. Parsing validates the signature, declared total length,
payload length and checksum. Unknown fixed bytes are retained when a parsed
file is serialized again.

Known type IDs include expression `00`, list `04`, matrix `06`, data `0a`,
text `0b`, string `0c`, GDB `0d`, figure `0e`, picture `10`, program `12`,
function `13`, macro `14`, zip `1c`, and assembler `21`.

