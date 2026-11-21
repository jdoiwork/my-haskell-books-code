// The Swift code in the emoji chapter of MiniRegex: a String's count is
// what a person sees; the code points and the bytes are other views.
// MiniRegex の絵文字の章の Swift のコード：String の count は、人が見る
// 文字の数。コードポイントとバイトは、別の見方
// 👨‍👩‍👧: a man, a woman and a girl, with ZWJs between
// 👨‍👩‍👧：男の人、女の人、女の子のあいだに ZWJ
let family = "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}"

// ANCHOR: count
// Swift: count is the clusters; unicodeScalars and utf8 are views
// Swift：count はクラスター。unicodeScalars と utf8 は、別の見方
print(family.count, family.unicodeScalars.count, family.utf8.count)
print(String(("\(family)🇯🇵👍🏽").prefix(2)))
// ANCHOR_END: count

precondition(family.count == 1)
precondition(family.unicodeScalars.count == 5)
precondition(family.utf8.count == 18)
precondition(String(("\(family)🇯🇵👍🏽").prefix(2)) == "\(family)🇯🇵")
