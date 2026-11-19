// Prices of the shop, for the chapter "Making operators".
// お店の値段。「演算子を作る」の章

// ANCHOR: price
// Kotlin (operator fun, and an infix function)
// Kotlin（operator fun と、infix の関数）
data class Price(val yen: Int) {
    // price + price
    operator fun plus(other: Price) = Price(yen + other.yen)

    // price off 10: a name, written between its two sides
    // price off 10：名前を、両側のあいだに書く
    infix fun off(percent: Int) = Price(yen * (100 - percent) / 100)
}

// 4 * price: an operator of Int, written outside Int
// 4 * price：Int の演算子を、Int の外に書く
operator fun Int.times(p: Price) = Price(this * p.yen)
// ANCHOR_END: price

fun main() {
    val sticker = Price(300)
    val mug = Price(1200)
    val tshirt = Price(2500)
    // ANCHOR: use
    val today = 4 * sticker + mug + 3 * tshirt
    // off is weaker than + : (4 * sticker + mug) off 10
    // off は + より弱い
    val sale = 4 * sticker + mug off 10
    // ANCHOR_END: use
    check(today == Price(9900))
    check(sale == Price(2160))
    println("ok")
}
