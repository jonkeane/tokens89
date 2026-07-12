# Legacy behavior specification

This port treats `TiFiles.ctl` v1.00.0009 as the normative implementation.
Calculator strings are byte strings; Unicode conversion happens only at API
and CLI boundaries.

## Routine inventory

| VB routine | Rust module | Test category |
| --- | --- | --- |
| `OpenTI`, `SaveTI` | `lib`, `tokenize`, `detokenize`, `tifile` | high-level round trips |
| `TIVar`, `GetTIFolder`, `GetTIFile`, `WriteTIVar` | `tifile` | exact containers, malformed files |
| `OpenTIGLAscii`, `GetTIGLAsciiName`, `SaveTIGLAscii` | `graphlink`, `charset` | fixed vectors, malformed files |
| `Token`, `Token_2`, `Token_3` | `tokenize`, `number`, `token` | exact tokens, regressions |
| `DeToken` | `detokenize`, `number`, `token` | exact source, malformed tokens |
| `LCase89` and character substitutions | `charset` | exhaustive byte conversion |
| decimal/binary/hex helpers | `number` | numeric edge cases |
| CR/LF helpers | `lib` | source normalization |

## Compatibility rules

- `Token` consumes CR-only source. The high-level API also accepts LF and CRLF.
- Text payloads are `00 01`, source bytes, `00 E0` and are never tokenized.
- Untokenized programs/functions retain source followed by the legacy AMS
  trailer. Expressions and one-line functions are always tokenized.
- Token streams are traversed from the final byte toward the first byte.
- Tokenization is a representation conversion, not TI-BASIC validation.
- Names are zero-padded byte strings of at most eight calculator characters.
- Header integers are serialized explicitly. The payload length at offsets
  `0x56..0x57` is big-endian; the final checksum is little-endian.
- GraphLink ASCII uses CRLF and a `START92`/`STOP92` envelope.

## Historical regressions

Tests are reserved for: DMS literals; parenthesis counting; `[3,pi/4]`;
program/function type IDs and tokenized flag; negative floats in lists;
element-wise operators; `PowerReg`, `list>mat`, `regCoef[1]`, binary/hex
integers, inverse trig, leading-dot floats, booleans, locals and branch
offsets; square root, differentiation and integration; inline comments;
indentation after comments; Greek names; null-terminated descriptions.

## Ambiguities

- VB file I/O accepts some truncated files by returning zero-filled values;
  Rust rejects them as malformed.
- The legacy description embeds the host application's version. Rust writes a
  stable `saved with tokens89` description.
- Invalid GraphLink data was returned as an error-shaped string. Rust returns
  a structured error.
- Unsupported and reserved AMS expression tokens are reported explicitly. In a
  tokenized program, a deliberately malformed statement is preserved losslessly
  as `@tokens(hex)` so protected ecosystem files can be decoded and recoded.
