// The Rust code in the chapter on switching engines: regex-automata's
// bounded backtracker remembers where it has been, so it is linear too;
// but only for a text short enough for its table.
// エンジンを乗り換える章の Rust のコード：regex-automata の上限つきの
// バックトラックは、通った所を覚えているので、これも線形。ただし、表に
// 収まるほど短い文字列だけ
use regex_automata::nfa::thompson::backtrack::BoundedBacktracker;
use regex_automata::nfa::thompson::pikevm::PikeVM;

// 🪪 A name field with n names, and a "!" by mistake.
// 名前が n 個と、まちがえて入った「!」の名前の欄
fn field_of(n: usize) -> String {
    let names = ["Mira", "Ruby", "Lam", "GHC", "Perl"];
    let mut words = Vec::new();
    for i in 0..n {
        words.push(names[i % names.len()]);
    }
    words.join(" ") + "!"
}

#[test]
fn backtracks_only_on_a_short_text() {
    // ANCHOR: bounded
    // Rust: the bounded backtracker, and the longest text it takes
    // Rust：上限つきのバックトラックと、受け取れるいちばん長い文字列
    let re = BoundedBacktracker::new(r"^(\w+\s?)+$").unwrap();
    let mut cache = re.create_cache();
    println!("{}", re.max_haystack_len());
    for n in [5, 10_000] {
        let field = field_of(n);
        match re.try_is_match(&mut cache, field.as_str()) {
            Ok(found) => println!("{n}: {found}"),
            Err(e) => println!("{n}: {e}"),
        }
    }
    // the Pike VM takes any length
    // Pike VM は、どんな長さでも
    let pike = PikeVM::new(r"^(\w+\s?)+$").unwrap();
    let field = field_of(10_000);
    println!("{}", pike.is_match(&mut pike.create_cache(), field.as_str()));
    // ANCHOR_END: bounded
    let short = field_of(5);
    assert_eq!(re.try_is_match(&mut cache, short.as_str()).unwrap(), false);
    let e = re.try_is_match(&mut cache, field.as_str()).unwrap_err();
    assert!(e.to_string().contains("too long"), "{e}");
    assert!(re.max_haystack_len() < field.len());
    assert!(!pike.is_match(&mut pike.create_cache(), field.as_str()));
}
