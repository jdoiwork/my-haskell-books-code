// The Rust code in the linear-time chapter: regex-automata has a Pike VM
// of its own, and the regex crate says no to a back reference when the
// pattern is made, not when it runs.
// 線形時間の章の Rust のコード：regex-automata には、自分の Pike VM が
// ある。regex クレートは、後方参照を、動かすときではなく、作るときに断る
use regex::Regex;
use regex_automata::nfa::thompson::pikevm::PikeVM;
use std::time::Instant;

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
fn runs_the_name_field_on_a_pike_vm() {
    // ANCHOR: pikevm
    // Rust: the Pike VM of regex-automata, with 10000 names
    // Rust：regex-automata の Pike VM で、名前が 10000 個
    let re = PikeVM::new(r"^(\w+\s?)+$").unwrap();
    let mut cache = re.create_cache();
    let field = field_of(10_000);
    println!("{}", re.is_match(&mut cache, field.as_str()));
    // The same priority as Perl's: a|ab finds "a"
    // Perl と同じ優先の順番：a|ab は「a」を見つける
    let first = PikeVM::new("a|ab").unwrap();
    let found = first.find(&mut first.create_cache(), "ab").unwrap();
    println!("{:?}", found.range());
    // ANCHOR_END: pikevm
    let start = Instant::now();
    assert!(!re.is_match(&mut cache, field.as_str()));
    assert!(start.elapsed().as_secs_f64() < 1.0);
    assert_eq!(found.range(), 0..1);
}

#[test]
fn says_no_when_it_is_made() {
    // ANCHOR: refuse
    // Rust: a back reference and a lookahead are refused by new
    // Rust：後方参照と先読みは、new が断る
    for pattern in [r"(\w+) \1", r"\d{4}-(?=\d)"] {
        match Regex::new(pattern) {
            Ok(_) => println!("ok"),
            Err(e) => println!("{e}"),
        }
    }
    // ANCHOR_END: refuse
    let e = Regex::new(r"(\w+) \1").unwrap_err().to_string();
    assert!(e.contains("backreferences are not supported"), "{e}");
    let e = Regex::new(r"\d{4}-(?=\d)").unwrap_err().to_string();
    assert!(e.contains("look-around"), "{e}");
}
