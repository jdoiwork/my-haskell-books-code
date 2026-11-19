// The Rust code in the chapter on classes and instances.
// 型クラスを作るの章の Rust のコード

enum Shape {
    Circle { radius: i64 },
    Rect { width: i64, height: i64 },
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct Yen(i64);

// ANCHOR: procedural
// Rust (procedural style)
// Rust（手続き型スタイル）
fn describe_shape(s: &Shape) -> String {
    match s {
        Shape::Circle { radius } => format!("a circle of radius {radius}"),
        Shape::Rect { width, height } => format!("a {width}x{height} rect"),
    }
}

fn describe_yen(m: &Yen) -> String {
    format!("{} yen", m.0)
}
// ANCHOR_END: procedural

// ANCHOR: functional
// Rust (functional style: a trait, with a default method)
// Rust（関数型スタイル：トレイトと、既定のメソッド）
trait Describe {
    fn describe(&self) -> String;
    fn describe_all(xs: &[Self]) -> String
    where
        Self: Sized,
    {
        xs.iter().map(|x| x.describe()).collect::<Vec<_>>().join(", ")
    }
}

impl Describe for Shape {
    fn describe(&self) -> String {
        describe_shape(self)
    }
}

impl Describe for Yen {
    fn describe(&self) -> String {
        describe_yen(self)
    }
}

// Option<T> is in Describe when T is.
// T が Describe なら、Option<T> も
impl<T: Describe> Describe for Option<T> {
    fn describe(&self) -> String {
        match self {
            None => "nothing".to_string(),
            Some(x) => format!("just {}", x.describe()),
        }
    }
}

// A trait above another: Ranked needs Describe.
// 上位のトレイト：Ranked には Describe が要る
trait Ranked: Describe {
    fn rank(&self) -> i64;
}

impl Ranked for Yen {
    fn rank(&self) -> i64 {
        self.0
    }
}

fn best<T: Ranked>(xs: &[T]) -> String {
    match xs.iter().max_by_key(|x| x.rank()) {
        None => "nothing".to_string(),
        Some(x) => x.describe(),
    }
}
// ANCHOR_END: functional

// ANCHOR: empty
// Rust (empty has no self: the type of the answer chooses it)
// Rust（empty には self がない。答えの型が選ぶ）
trait Combine {
    fn empty() -> Self;
    fn combine(self, other: Self) -> Self;
}

impl Combine for Yen {
    fn empty() -> Self {
        Yen(0)
    }
    fn combine(self, other: Self) -> Self {
        Yen(self.0 + other.0)
    }
}

fn total<T: Combine>(xs: Vec<T>) -> T {
    xs.into_iter().fold(T::empty(), T::combine)
}
// ANCHOR_END: empty

fn main() {
    let shapes = [Shape::Circle { radius: 1 }, Shape::Rect { width: 2, height: 3 }];
    assert_eq!(Shape::describe_all(&shapes), "a circle of radius 1, a 2x3 rect");
    assert_eq!(Some(Some(Yen(3))).describe(), "just just 3 yen");
    assert_eq!(None::<Yen>.describe(), "nothing");
    assert_eq!(best(&[Yen(300), Yen(450), Yen(80)]), "450 yen");
    assert_eq!(total(vec![Yen(300), Yen(450)]), Yen(750));
    let nothing: Yen = total(vec![]);
    assert_eq!(nothing, Yen(0));
    println!("ok");
}
