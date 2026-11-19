// The TypeScript code in the groups chapter of MiniRegex: named groups,
// and three places where JavaScript's rules are not PCRE2's.
// MiniRegex のグループの章の TypeScript のコード：名前付きのグループと、
// JavaScript の決まりが PCRE2 とちがう3つの場所
function same(actual: unknown, expected: unknown): void {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`unexpected: ${JSON.stringify(actual)}`);
  }
}

// ANCHOR: groups
// TypeScript (functional style): match.groups holds the named groups
// TypeScript（関数型スタイル）：match.groups に名前付きのグループ
const date = /(?<year>[0-9]{4})-(?<month>[0-9]{2})-(?<day>[0-9]{2})/g;
const text = "2026-11-19 sold; 2026-11-21 seen";
console.log([...text.matchAll(date)].map((m) => m.groups?.day));
// ANCHOR_END: groups

same([...text.matchAll(date)].map((m) => m.groups?.day), ["19", "21"]);

// ANCHOR: repeat
// TypeScript: a group in a repeat forgets what it found, at the start
//   of each round (PCRE2 keeps it)
// TypeScript：くり返しの中のグループは、回の頭で、見つけたものを忘れる
//   （PCRE2 は覚えておく）
console.log(/(?:(a)|b)*/.exec("ab")?.[1]);
console.log(/(a*)*/.exec("aab")?.[1]);
// ANCHOR_END: repeat

same(/(?:(a)|b)*/.exec("ab")?.[1], undefined);
same(/(a*)*/.exec("aab")?.[1], "aa");

// ANCHOR: backref
// TypeScript: \1 of a group that found nothing matches the empty text
// TypeScript：何も見つけなかったグループの \1 は、空の文字列に合う
console.log(/(a)?\1/.exec("b")?.[0]);
// ANCHOR_END: backref

same(/(a)?\1/.exec("b")?.[0], "");

// ANCHOR: findall
// TypeScript: after an empty match, one character on (lastIndex + 1)
// TypeScript：空の一致のあとは、1文字先へ（lastIndex + 1）
console.log([..."a".matchAll(/a??/g)].map((m) => m[0]));
// ANCHOR_END: findall

same([..."a".matchAll(/a??/g)].map((m) => m[0]), ["", ""]);

// ANCHOR: replace
// TypeScript: $<name> in a replacement text; $2 of a group that is not
//   there stays as it is
// TypeScript：置き換えの文字列の $<name>。ないグループの $2 は、
//   そのまま残る
console.log("On 2026-11-19".replace(date, "$<day>/$<month>/$<year>"));
console.log("a".replace(/(a)/, "[$2][$1]"));
// ANCHOR_END: replace

same("On 2026-11-19".replace(date, "$<day>/$<month>/$<year>"),
  "On 19/11/2026");
same("a".replace(/(a)/, "[$2][$1]"), "[$2][a]");
