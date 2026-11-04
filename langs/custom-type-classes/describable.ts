// The TypeScript code in the custom type classes chapter.
// 型クラスを作る章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
// ANCHOR: interface
// TypeScript (interface)
// TypeScript（インターフェース）
interface Describable {
  describe(): string;
}

const announce = (x: Describable): string => `Look! ${x.describe()}`;
// ANCHOR_END: interface
check(announce({ describe: () => "yes, indeed" }) === "Look! yes, indeed", "announce");
