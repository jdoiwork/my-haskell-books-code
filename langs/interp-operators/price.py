# Prices of the shop, for the chapter "Making operators".
# お店の値段。「演算子を作る」の章

# ANCHOR: price
# Python (__add__ and __rmul__)
# Python（__add__ と __rmul__）
class Price:
    def __init__(self, yen: int) -> None:
        self.yen = yen

    # price + price
    def __add__(self, other: "Price") -> "Price":
        return Price(self.yen + other.yen)

    # 4 * price: int does not know Price, so Python asks the right side
    # 4 * price：int は Price を知らないので、右側に聞く
    def __rmul__(self, n: int) -> "Price":
        return Price(n * self.yen)

    # No new symbol like -%: a method instead
    # -% のような新しい記号は作れないので、メソッドにする
    def off(self, percent: int) -> "Price":
        return Price(self.yen * (100 - percent) // 100)


sticker, mug, tshirt = Price(300), Price(1200), Price(2500)

today = 4 * sticker + mug + 3 * tshirt
sale = (4 * sticker + mug).off(10)
# ANCHOR_END: price

assert today.yen == 9900
assert sale.yen == 2160
print("ok")
