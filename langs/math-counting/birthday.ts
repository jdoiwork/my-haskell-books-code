// The birthday paradox in TypeScript, for the chapter
// "Counting and probability".
// 「数え上げと確率」の章の誕生日のパラドックスを、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const sameBirthday = (days: number, people: number): number =>
  1 -
  Array.from({ length: people }, (_, k) => (days - k) / days)
    .reduce((acc, x) => acc * x, 1);

const party = sameBirthday(365, 23);       // 0.507...
const hashes = sameBirthday(2 ** 32, 77163); // about 0.5
// ANCHOR_END: functional

check(Math.abs(party - 0.5072972343239857) < 1e-12, "23 people");
check(Math.abs(hashes - 0.5) < 1e-4, "77163 hashes");
console.log("ok");
