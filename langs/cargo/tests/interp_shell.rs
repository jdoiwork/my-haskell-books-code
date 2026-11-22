// The Rust code in the chapter on shells: xshell's cmd! cuts the words
// of the line it is given, and each {value} is one word; {values...} is
// as many words as the Vec has. No shell runs it.
// シェルのようにの章の Rust のコード：xshell の cmd! は、渡した行の語を
// 切り、{value} はそれぞれ1つの語。{values...} は Vec の数の語。
// シェルは動かさない
use xshell::{Shell, cmd};

#[test]
fn one_word_and_many() -> xshell::Result<()> {
    let sh = Shell::new()?;
    let dir = sh.create_temp_dir()?;
    sh.change_dir(dir.path());
    sh.write_file("notes; echo INJECTED.txt", "one\ntwo\n")?;
    // ANCHOR: xshell
    // Rust (xshell): {name} is one word, {files...} is one for each
    // Rust（xshell）：{name} は1つの語、{files...} は要素ごとに1つ
    let name = "notes; echo INJECTED.txt";
    let counted = cmd!(sh, "wc -l {name}").read()?;
    let files = vec!["a b.txt", "c.txt"];
    let shown = cmd!(sh, "printf <%s> {files...}").read()?;
    // ANCHOR_END: xshell
    let words: Vec<&str> = counted.split_whitespace().collect();
    assert_eq!(words, ["2", "notes;", "echo", "INJECTED.txt"]);
    assert_eq!(shown, "<a b.txt><c.txt>");
    // ANCHOR: xshellEnv
    // Rust (xshell): a variable and an input for one command only
    // Rust（xshell）：環境変数と入力を、そのコマンドだけに
    let sorted = cmd!(sh, "sort")
        .env("LC_ALL", "C")
        .stdin("b\na\n")
        .read()?;
    // ANCHOR_END: xshellEnv
    assert_eq!(sorted, "a\nb");
    Ok(())
}
