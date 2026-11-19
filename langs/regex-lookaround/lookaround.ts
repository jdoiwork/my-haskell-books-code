// The TypeScript code in the chapter on looking around of MiniRegex:
// the numbers before " gold", a lookbehind of any length (read from
// the right), no atomic group (a lookahead and a back reference do the
// same), and \b that stays ASCII.
// MiniRegex のまわりを見る章の TypeScript のコード：" gold" の前の数、
// どんな長さでもよい後読み（右から読む）、アトミックグループはない
// （先読みと後方参照で同じことができる）、ASCII のままの \b
function same(actual: unknown, expected: unknown): void {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`unexpected: ${JSON.stringify(actual)}`);
  }
}

const ledger = "price:1200 gold, price: 80 gold, price:   7 gold";

// ANCHOR: behind
// TypeScript: a lookbehind may be any length, even \s*
// TypeScript：後読みは、どんな長さでもよい。\s* でも
console.log(ledger.match(/(?<=price:\s*)\d+/g));
// ANCHOR_END: behind

same(ledger.match(/(?<=price:\s*)\d+/g), ["1200", "80", "7"]);

// ANCHOR: right
// TypeScript: a lookbehind is matched from right to left, so (a|aa)
//   finds the nearer a (PCRE2 and Perl: aa)
// TypeScript：後読みは右から左へ合わせるので、(a|aa) は近いほうの a を
//   見つける（PCRE2 と Perl は aa）
console.log("aab".match(/(?<=(a|aa))b/)?.[1]);
// ANCHOR_END: right

same("aab".match(/(?<=(a|aa))b/)?.[1], "a");

// ANCHOR: atomic
// TypeScript: no (?>...); a lookahead is atomic, and \1 takes what it
//   saw, so (?=(a|ab))\1 works the same
// TypeScript：(?>...) はない。先読みはアトミックで、\1 が、先読みが
//   見たものを食べるので、(?=(a|ab))\1 で同じことができる
console.log(/(?=(a|ab))\1c/.test("abc"), /(?:a|ab)c/.test("abc"));
// ANCHOR_END: atomic

same(/(?=(a|ab))\1c/.test("abc"), false);
same(/(?:a|ab)c/.test("abc"), true);
let refused = false;
try {
  new RegExp("(?>a|ab)c");
} catch {
  refused = true;
}
same(refused, true);

// ANCHOR: boundary
// TypeScript: \b looks at \w, which is ASCII even with u
// TypeScript：\b は \w を見る。\w は、u を付けても ASCII
console.log(/\bcafé\b/u.test("a café."), /\bcafe\b/u.test("a cafe."));
// ANCHOR_END: boundary

same(/\bcafé\b/u.test("a café."), false);
same(/\bcafe\b/u.test("a cafe."), true);
