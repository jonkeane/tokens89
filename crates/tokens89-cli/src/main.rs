use std::{
    env, fs,
    io::{self, IsTerminal, Read, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};
use tokens89_core::{
    charset, detokenize, tifile::TiFile, tokenize, Error, Result, TiGroup, VariableType,
};

const HELP:&str="tokens89 — TI-89/92+/Voyage 200 source and variable converter

Usage:
  tokens89 encode SOURCE [-o FILE] [--name NAME] [--folder FOLDER] [--type TYPE] [--no-tokenize] [--force]
  tokens89 decode FILE [-o FILE|-] [--force]
  tokens89 inspect FILE
  tokens89 tokenize SOURCE [-o FILE|-] [--type TYPE] [--hex] [--force]
  tokens89 detokenize TOKENS [-o FILE|-] [--hex] [--force]
  tokens89 verify FILE
  tokens89 group FILE... [-o GROUP.89g] [--force]

SOURCE, FILE, or TOKENS may be - for standard input. Binary output is refused
when standard output is an interactive terminal. Tokenization converts source
representation; it does not perform full TI-BASIC syntax validation.";

#[derive(Default)]
struct Opts {
    input: Option<String>,
    output: Option<String>,
    name: Option<String>,
    folder: Option<String>,
    kind: Option<String>,
    force: bool,
    hex: bool,
    no_tokenize: bool,
}
fn opts(args: &[String]) -> Result<Opts> {
    let mut o = Opts::default();
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-o" | "--output" | "--name" | "--folder" | "--type" => {
                if i + 1 == args.len() {
                    return Err(Error::Usage(format!("{} requires a value", args[i])));
                }
                let v = args[i + 1].clone();
                match args[i].as_str() {
                    "-o" | "--output" => o.output = Some(v),
                    "--name" => o.name = Some(v),
                    "--folder" => o.folder = Some(v),
                    _ => o.kind = Some(v),
                }
                i += 2
            }
            "--force" => {
                o.force = true;
                i += 1
            }
            "--hex" => {
                o.hex = true;
                i += 1
            }
            "--no-tokenize" => {
                o.no_tokenize = true;
                i += 1
            }
            v if !v.starts_with('-') || v == "-" => {
                if o.input.replace(v.into()).is_some() {
                    return Err(Error::Usage("only one input may be supplied".into()));
                }
                i += 1
            }
            v => return Err(Error::Usage(format!("unknown option {v}"))),
        }
    }
    if o.input.is_none() {
        return Err(Error::Usage("missing input path".into()));
    }
    Ok(o)
}

#[derive(Default)]
struct GroupOpts {
    inputs: Vec<String>,
    output: Option<String>,
    force: bool,
}

fn group_opts(args: &[String]) -> Result<GroupOpts> {
    let mut options = GroupOpts::default();
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "-o" | "--output" => {
                if index + 1 == args.len() {
                    return Err(Error::Usage(format!("{} requires a value", args[index])));
                }
                options.output = Some(args[index + 1].clone());
                index += 2;
            }
            "--force" => {
                options.force = true;
                index += 1;
            }
            value if !value.starts_with('-') || value == "-" => {
                options.inputs.push(value.into());
                index += 1;
            }
            value => return Err(Error::Usage(format!("unknown option {value}"))),
        }
    }
    if options.inputs.is_empty() {
        return Err(Error::Usage("missing variable input path".into()));
    }
    if options
        .inputs
        .iter()
        .filter(|input| input.as_str() == "-")
        .count()
        > 1
    {
        return Err(Error::Usage("standard input may only be used once".into()));
    }
    Ok(options)
}
fn read(path: &str) -> Result<Vec<u8>> {
    if path == "-" {
        let mut b = Vec::new();
        io::stdin().read_to_end(&mut b)?;
        Ok(b)
    } else {
        Ok(fs::read(path)?)
    }
}
fn write(path: Option<&str>, data: &[u8], force: bool, binary: bool) -> Result<()> {
    match path {
        None | Some("-") => {
            if binary && io::stdout().is_terminal() {
                return Err(Error::Usage(
                    "refusing to write binary data to an interactive terminal; use --output FILE"
                        .into(),
                ));
            }
            io::stdout().write_all(data)?
        }
        Some(p) => {
            if Path::new(p).exists() && !force {
                return Err(Error::Usage(format!(
                    "{p} exists; pass --force to overwrite"
                )));
            }
            fs::write(p, data)?
        }
    }
    Ok(())
}
fn kind(s: &str) -> Result<VariableType> {
    Ok(match s.to_ascii_lowercase().as_str() {
        "expression" | "expr" | "89e" => VariableType::Expression,
        "list" | "89l" => VariableType::List,
        "matrix" | "89m" => VariableType::Matrix,
        "data" => VariableType::Data,
        "text" | "89t" => VariableType::Text,
        "string" | "89s" => VariableType::String,
        "gdb" | "89d" => VariableType::Gdb,
        "figure" | "89a" => VariableType::Figure,
        "picture" | "89i" => VariableType::Picture,
        "program" | "prgm" | "89p" => VariableType::Program,
        "function" | "func" | "89f" => VariableType::Function,
        "macro" => VariableType::Macro,
        "zip" => VariableType::Zip,
        "assembler" | "asm" => VariableType::Assembler,
        _ => return Err(Error::Usage(format!("unknown variable type {s}"))),
    })
}
fn inferred(path: &str) -> Option<VariableType> {
    let e = Path::new(path).extension()?.to_str()?.to_ascii_lowercase();
    kind(&e).ok()
}
fn default_output(input: &str, ext: &str) -> Option<String> {
    if input == "-" {
        None
    } else {
        Some(
            PathBuf::from(input)
                .with_extension(ext)
                .to_string_lossy()
                .into_owned(),
        )
    }
}
fn hex_encode(b: &[u8]) -> Vec<u8> {
    let mut s = String::with_capacity(b.len() * 3);
    for (i, x) in b.iter().enumerate() {
        if i > 0 {
            s.push(' ')
        }
        s.push_str(&format!("{x:02x}"));
    }
    s.push('\n');
    s.into_bytes()
}
fn hex_decode(b: &[u8]) -> Result<Vec<u8>> {
    let s = std::str::from_utf8(b).map_err(|_| Error::Malformed("hex input is not UTF-8"))?;
    s.split_whitespace()
        .map(|x| {
            u8::from_str_radix(x.trim_start_matches("0x"), 16)
                .map_err(|_| Error::MalformedDetail(format!("invalid hex byte {x}")))
        })
        .collect()
}
fn stem(path: &str) -> String {
    Path::new(path)
        .file_stem()
        .and_then(|x| x.to_str())
        .unwrap_or("noname")
        .chars()
        .take(8)
        .collect()
}

/// Detects a GraphLink-style `name(arguments)` header and converts it to the
/// calculator's `(arguments)` source header.
fn name_from_program_header(source: &str) -> Option<(String, String)> {
    let line_end = source.find(['\r', '\n']).unwrap_or(source.len());
    let header = &source[..line_end];
    let argument_start = header.find('(')?;
    let name = &header[..argument_start];
    if name.is_empty() || name.trim() != name || !header[argument_start..].ends_with(')') {
        return None;
    }
    Some((
        name.to_owned(),
        format!("{}{}", &header[argument_start..], &source[line_end..]),
    ))
}

fn run_group(args: &[String]) -> Result<()> {
    let options = group_opts(args)?;
    let files = options
        .inputs
        .iter()
        .map(|input| TiFile::parse(&read(input)?))
        .collect::<Result<Vec<_>>>()?;
    let bytes = TiGroup::from_files(files)?.to_bytes()?;
    let output = options
        .output
        .or_else(|| default_output(&options.inputs[0], "89g"));
    write(output.as_deref(), &bytes, options.force, true)
}

fn run() -> Result<()> {
    let mut a = env::args().skip(1).collect::<Vec<_>>();
    if a.is_empty() || matches!(a[0].as_str(), "-h" | "--help") {
        println!("{HELP}");
        return Ok(());
    }
    if matches!(a[0].as_str(), "-V" | "--version") {
        println!("tokens89 {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }
    let command = a.remove(0);
    if command == "group" {
        return run_group(&a);
    }
    let o = opts(&a)?;
    let input = o.input.as_deref().unwrap();
    match command.as_str() {
        "encode" => {
            let src = read(input)?;
            let text =
                std::str::from_utf8(&src).map_err(|_| Error::Malformed("source is not UTF-8"))?;
            let k = if let Some(v) = o.kind.as_deref() {
                kind(v)?
            } else {
                inferred(o.output.as_deref().unwrap_or(input))
                    .ok_or_else(|| Error::Usage("cannot infer output type; pass --type".into()))?
            };
            let (header_name, source) =
                if matches!(k, VariableType::Program | VariableType::Function) {
                    match name_from_program_header(text) {
                        Some((name, source)) => (Some(name), source),
                        None => (None, text.into()),
                    }
                } else {
                    (None, text.into())
                };
            let name = o.name.or(header_name).unwrap_or_else(|| stem(input));
            let folder = o.folder.unwrap_or_else(|| "main".into());
            let bytes = tokens89_core::encode_source(&source, &folder, &name, k, !o.no_tokenize)?;
            let out = o.output.or_else(|| {
                default_output(
                    input,
                    match k {
                        VariableType::Program => "89p",
                        VariableType::Function => "89f",
                        VariableType::Text => "89t",
                        _ => "89e",
                    },
                )
            });
            write(out.as_deref(), &bytes, o.force, true)
        }
        "decode" => {
            let b = read(input)?;
            let text = tokens89_core::decode_file(&b)?;
            let out = o.output.or_else(|| default_output(input, "bas"));
            write(out.as_deref(), text.as_bytes(), o.force, false)
        }
        "inspect" => {
            let bytes = read(input)?;
            if let Ok(group) = TiGroup::parse(&bytes) {
                println!(
                    "signature: group\ndescription: {}\nentries: {}",
                    group.description,
                    group.entries.len()
                );
                for entry in group.entries {
                    println!(
                        "  {}\\{} ({}, {} payload bytes)",
                        entry.folder,
                        entry.name,
                        entry.kind.label(),
                        entry.payload.len()
                    );
                }
                return Ok(());
            }
            let f = TiFile::parse(&bytes)?;
            println!("signature: **TI89**\nfolder: {}\nname: {}\ntype: {} (0x{:02x})\ndescription: {}\npayload-size: {}\nchecksum: 0x{:04x}",f.folder,f.name,f.kind.label(),f.kind.id(),f.description,f.payload.len(),tokens89_core::tifile::checksum(&f.payload));
            Ok(())
        }
        "verify" => {
            let bytes = read(input)?;
            if let Ok(group) = TiGroup::parse(&bytes) {
                println!("valid: group ({} variables)", group.entries.len());
                return Ok(());
            }
            let f = TiFile::parse(&bytes)?;
            println!(
                "valid: {}\\{} ({}, {} payload bytes)",
                f.folder,
                f.name,
                f.kind.label(),
                f.payload.len()
            );
            Ok(())
        }
        "tokenize" => {
            let b = read(input)?;
            let text =
                std::str::from_utf8(&b).map_err(|_| Error::Malformed("source is not UTF-8"))?;
            let calc = charset::encode_source(&tokens89_core::normalize_line_endings(text))?;
            let k = o
                .kind
                .as_deref()
                .map(kind)
                .transpose()?
                .unwrap_or(VariableType::Expression);
            let tok = tokenize::tokenize(&calc, k, !o.no_tokenize)?;
            let data = if o.hex { hex_encode(&tok) } else { tok };
            write(o.output.as_deref(), &data, o.force, !o.hex)
        }
        "detokenize" => {
            let mut b = read(input)?;
            if o.hex {
                b = hex_decode(&b)?
            }
            let raw = detokenize::detokenize(&b)?;
            let text = tokens89_core::display_line_endings(&charset::decode_source(&raw));
            write(o.output.as_deref(), text.as_bytes(), o.force, false)
        }
        _ => Err(Error::Usage(format!("unknown command {command}\n\n{HELP}"))),
    }
}
fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("tokens89: {e}");
            ExitCode::from(2)
        }
    }
}
