use std::{
    fs,
    io::Write,
    process::{Command, Stdio},
    sync::atomic::{AtomicU64, Ordering},
    time::{SystemTime, UNIX_EPOCH},
};
use tokens89_core::TiGroup;
static NEXT: AtomicU64 = AtomicU64::new(0);
fn dir() -> std::path::PathBuf {
    let p = std::env::temp_dir().join(format!(
        "tokens89-cli-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
            + NEXT.fetch_add(1, Ordering::Relaxed) as u128
    ));
    fs::create_dir(&p).unwrap();
    p
}
fn run(args: &[&str], stdin: Option<&[u8]>) -> std::process::Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_tokens89"));
    command
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if stdin.is_some() {
        command.stdin(Stdio::piped());
    }
    let mut child = command.spawn().unwrap();
    if let Some(data) = stdin {
        child.stdin.take().unwrap().write_all(data).unwrap();
    }
    child.wait_with_output().unwrap()
}
#[test]
fn all_six_commands_work_in_scripts() {
    let d = dir();
    let ti = d.join("demo.89p");
    let ti_s = ti.to_str().unwrap();
    let source = b"()\nPrgm\n  Disp 1+2\nEndPrgm\n";
    assert!(run(
        &["encode", "-", "--output", ti_s, "--name", "demo", "--type", "program"],
        Some(source)
    )
    .status
    .success());
    for command in ["inspect", "verify"] {
        let out = run(&[command, ti_s], None);
        assert!(
            out.status.success(),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
    let decoded = run(&["decode", ti_s, "--output", "-"], None);
    assert!(decoded.status.success());
    assert_eq!(decoded.stdout, b"()\nPrgm\n  Disp 1+2\nEndPrgm");
    let tokens = run(&["tokenize", "-", "--hex"], Some(b"1+2"));
    assert!(tokens.status.success());
    let plain = run(&["detokenize", "-", "--hex"], Some(&tokens.stdout));
    assert!(plain.status.success());
    assert_eq!(plain.stdout, b"1+2");
}
#[test]
fn overwrite_requires_force() {
    let d = dir();
    let output = d.join("x.89e");
    fs::write(&output, b"existing").unwrap();
    let out = run(
        &[
            "encode",
            "-",
            "--output",
            output.to_str().unwrap(),
            "--name",
            "x",
            "--type",
            "expression",
        ],
        Some(b"1"),
    );
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("--force"));
}

#[test]
fn help_and_version_are_available() {
    let help = run(&["--help"], None);
    assert!(help.status.success());
    assert!(String::from_utf8_lossy(&help.stdout).contains("Usage:"));

    let version = run(&["--version"], None);
    assert!(version.status.success());
    assert!(String::from_utf8_lossy(&version.stdout).contains("tokens89"));
}

#[test]
fn unknown_command_and_option_fail_with_usage() {
    let unknown_command = run(&["unknown", "-"], Some(b"1"));
    assert!(!unknown_command.status.success());
    assert!(String::from_utf8_lossy(&unknown_command.stderr).contains("unknown command"));

    let unknown_option = run(&["tokenize", "-", "--wat"], Some(b"1"));
    assert!(!unknown_option.status.success());
    assert!(String::from_utf8_lossy(&unknown_option.stderr).contains("unknown option"));
}

#[test]
fn missing_required_option_values_are_rejected() {
    let missing_type_value = run(&["encode", "-", "--type"], Some(b"1"));
    assert!(!missing_type_value.status.success());
    assert!(String::from_utf8_lossy(&missing_type_value.stderr).contains("requires a value"));

    let missing_input = run(&["tokenize"], None);
    assert!(!missing_input.status.success());
    assert!(String::from_utf8_lossy(&missing_input.stderr).contains("missing input path"));
}

#[test]
fn encode_requires_explicit_type_when_it_cannot_be_inferred() {
    let out = run(&["encode", "-", "--output", "-"], Some(b"1+2"));
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("cannot infer output type"));
}

#[test]
fn encode_can_take_a_program_name_from_its_first_line() {
    let d = dir();
    let output = d.join("f4.89p");
    let out = run(
        &[
            "encode",
            "-",
            "--output",
            output.to_str().unwrap(),
            "--type",
            "program",
        ],
        Some(b"f4(v)\nPrgm\n  Return v\nEndPrgm\n"),
    );
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );

    let inspected = run(&["inspect", output.to_str().unwrap()], None);
    assert!(inspected.status.success());
    assert!(String::from_utf8_lossy(&inspected.stdout).contains("name: f4"));

    let decoded = run(&["decode", output.to_str().unwrap(), "--output", "-"], None);
    assert!(decoded.status.success());
    assert_eq!(decoded.stdout, b"(v)\nPrgm\n  Return v\nEndPrgm");
}

#[test]
fn explicit_name_overrides_a_name_from_the_program_header() {
    let d = dir();
    let output = d.join("override.89p");
    let out = run(
        &[
            "encode",
            "-",
            "--output",
            output.to_str().unwrap(),
            "--type",
            "program",
            "--name",
            "other",
        ],
        Some(b"f4(v)\nPrgm\nEndPrgm\n"),
    );
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let inspected = run(&["inspect", output.to_str().unwrap()], None);
    assert!(String::from_utf8_lossy(&inspected.stdout).contains("name: other"));
}

#[test]
fn tokenize_and_detokenize_validate_utf8_and_hex_input() {
    let bad_utf8 = run(&["tokenize", "-"], Some(&[0xff, 0xfe, 0xfd]));
    assert!(!bad_utf8.status.success());
    assert!(String::from_utf8_lossy(&bad_utf8.stderr).contains("not UTF-8"));

    let bad_hex = run(&["detokenize", "-", "--hex"], Some(b"00 zz ff"));
    assert!(!bad_hex.status.success());
    assert!(String::from_utf8_lossy(&bad_hex.stderr).contains("invalid hex byte"));
}

#[test]
fn group_combines_single_variable_files() {
    let d = dir();
    let program = d.join("demo.89p");
    let text = d.join("readme.89t");
    let group = d.join("bundle.89g");
    for (output, kind, source) in [
        (&program, "program", b"()\nPrgm\nEndPrgm".as_slice()),
        (&text, "text", b"Read me".as_slice()),
    ] {
        let result = run(
            &[
                "encode",
                "-",
                "--output",
                output.to_str().unwrap(),
                "--name",
                output.file_stem().unwrap().to_str().unwrap(),
                "--type",
                kind,
            ],
            Some(source),
        );
        assert!(result.status.success());
    }
    let result = run(
        &[
            "group",
            program.to_str().unwrap(),
            text.to_str().unwrap(),
            "--output",
            group.to_str().unwrap(),
        ],
        None,
    );
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    let parsed = TiGroup::parse(&fs::read(&group).unwrap()).unwrap();
    assert_eq!(parsed.entries.len(), 2);
    assert_eq!(parsed.entries[0].name, "demo");
    assert_eq!(parsed.entries[1].name, "readme");
    let inspected = run(&["inspect", group.to_str().unwrap()], None);
    assert!(inspected.status.success());
    assert!(String::from_utf8_lossy(&inspected.stdout).contains("entries: 2"));
    let verified = run(&["verify", group.to_str().unwrap()], None);
    assert!(verified.status.success());
    assert!(String::from_utf8_lossy(&verified.stdout).contains("2 variables"));
}
