// The shown-or-hidden note of the Mantine chapter, read with serde.
// Mantine の章の「見せるか隠すか」の感想を、serde で読む
use serde::Deserialize;

// ANCHOR: seen_note
// Rust (serde: the "tag" field picks the variant)
// Rust（serde：tag の欄で、どの種類かを選ぶ）
#[derive(Deserialize, Debug, PartialEq)]
struct NoteItem {
    entry: Option<i64>,
    body: String,
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(tag = "tag")]
enum SeenNote {
    ShownNote { note: NoteItem },
    HiddenNote { entry: Option<i64>, hint: String },
}

fn line_of(s: &SeenNote) -> &str {
    match s {
        SeenNote::ShownNote { note } => &note.body,
        SeenNote::HiddenNote { hint, .. } => hint,
    }
}
// ANCHOR_END: seen_note

#[test]
fn reads_the_haskell_json() {
    let wire = r#"[
      {"tag": "ShownNote", "note": {"entry": null, "body": "絵が多くて読みやすい"}},
      {"tag": "HiddenNote", "entry": 8, "hint": "ネタバレ（「第4章」を読んだら見えます）"}
    ]"#;
    let seen: Vec<SeenNote> = serde_json::from_str(wire).unwrap();
    let lines: Vec<&str> = seen.iter().map(line_of).collect();
    assert_eq!(lines, ["絵が多くて読みやすい", "ネタバレ（「第4章」を読んだら見えます）"]);
    // a hidden note has no body to read
    // 隠した感想には、読める本文がない
    assert!(serde_json::from_str::<SeenNote>(r#"{"tag": "LostNote"}"#).is_err());
}
