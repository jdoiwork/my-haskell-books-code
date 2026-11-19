// Prices of the shop, for the chapter "Making operators".
// お店の値段。「演算子を作る」の章

// ANCHOR: price
// Rust (the traits Add and Mul)
// Rust（Add と Mul のトレイト）
use std::ops::{Add, Mul};

#[derive(Clone, Copy, Debug, PartialEq)]
struct Price(u32);

// price + price
impl Add for Price {
    type Output = Price;
    fn add(self, other: Price) -> Price {
        Price(self.0 + other.0)
    }
}

// 4 * price: Mul is implemented for u32, with Price on the right
// 4 * price：u32 に、右が Price の Mul を実装する
impl Mul<Price> for u32 {
    type Output = Price;
    fn mul(self, p: Price) -> Price {
        Price(self * p.0)
    }
}

// No new symbol like -%: a method instead
// -% のような新しい記号は作れないので、メソッドにする
impl Price {
    fn off(self, percent: u32) -> Price {
        Price(self.0 * (100 - percent) / 100)
    }
}
// ANCHOR_END: price

fn main() {
    let (sticker, mug, tshirt) = (Price(300), Price(1200), Price(2500));
    // ANCHOR: use
    let today = 4 * sticker + mug + 3 * tshirt;
    let sale = (4 * sticker + mug).off(10);
    // ANCHOR_END: use
    assert_eq!(today, Price(9900));
    assert_eq!(sale, Price(2160));
    println!("ok");
}
