// Gleam's Int when it is turned into JavaScript (target = "javascript"
// in gleam.toml), for the chapter "Turning Lam into JavaScript".
// JavaScript にしたときの Gleam の Int（gleam.toml の target）。
// 「JavaScript に変換する」の章

// ANCHOR: gleam
// Gleam (target = "javascript")
pub fn product(xs: List(Int), total: Int) -> Int {
  case xs {
    [] -> total
    [x, ..rest] -> product(rest, total * x)
  }
}

pub fn main() -> Nil {
  // Above 2^53, a JavaScript number has no room for every Int.
  // 2^53 を超えると、JavaScript の number には、すべての Int の場所がない
  let safe = 9_007_199_254_740_991
  let assert True = safe + 2 == safe + 1
  let assert -3 = -7 / 2       // toward 0
  let assert -1 = -7 % 2       // the sign of -7
  let assert 0 = 7 / 0         // no error: 0
  let assert True = product([2, 3, 4], 1) == 24
  Nil
}
// ANCHOR_END: gleam
