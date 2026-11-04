// The TypeScript code in the security chapter.
// セキュリティの章の TypeScript のコード

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: branded
// TypeScript (a branded type: a string that passed the check)
// TypeScript（ブランド型：確かめを通った string）
type Username = string & { readonly __brand: "Username" };

function parseUsername(raw: string): Username | Error {
  if (!/^[a-z0-9_]{3,16}$/.test(raw)) {
    return new Error(`bad username: ${raw}`);
  }
  return raw as Username;
}

function greet(name: Username): string {
  return `Welcome, ${name}!`;
}
// ANCHOR_END: branded

// ANCHOR: secret
// TypeScript (a secret: printing or JSON never shows the inside)
// TypeScript（秘密：表示しても JSON にしても、中身は出ない）
class Password {
  readonly #value: string;
  constructor(value: string) {
    this.#value = value;
  }
  reveal(): string {
    return this.#value;
  }
  toString(): string {
    return "<redacted>";
  }
  toJSON(): string {
    return "<redacted>";
  }
}
// ANCHOR_END: secret

const name = parseUsername("lambda_3");
if (name instanceof Error) throw name;
check(greet(name) === "Welcome, lambda_3!", "greet");
check(parseUsername("x' OR '1'='1") instanceof Error, "parse");
// @ts-expect-error: a plain string is not a Username.
greet("x' OR '1'='1");

const pass = new Password("hunter2");
check(`${pass}` === "<redacted>", "toString");
check(JSON.stringify({ user: name, pass }) ===
  '{"user":"lambda_3","pass":"<redacted>"}', "toJSON");
check(pass.reveal() === "hunter2", "reveal");
