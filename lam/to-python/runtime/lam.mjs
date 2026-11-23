// ANCHOR: all
// The runtime of lam to-js: what a Lam program needs in JavaScript,
// but does not write itself. lam puts this file in front of every
// program it turns into JavaScript.
// lam to-js のランタイム：Lam のプログラムが JavaScript で要るが、自分では
// 書かないもの。lam は、JavaScript にするどのプログラムの前にも、この
// ファイルを入れる。
import * as fs from "node:fs";

// ANCHOR: makeRuntime
// 🧰 Make the runtime. Everything is inside this one function, so
//    the names of the program (all of them have a $) never meet the
//    names here.
// ランタイムを作る。全部この関数の中なので、プログラムの名前（どれも $ を
// 持つ）が、ここの名前とぶつからない
export function makeRuntime(world = realWorld()) {
  // ANCHOR_END: makeRuntime

  // ANCHOR: values
  // 🧱 The values of Lam that JavaScript has no shape for. An Int is
  //    a BigInt, a Float a number, a String a string, a Bool a
  //    boolean, a tuple an array, a record an object. The others:
  // JavaScript に形のない Lam の値。Int は BigInt、Float は number、
  // String は string、Bool は boolean、タプルは配列、レコードは
  // オブジェクト。ほかは次のとおり

  // A constructor's value: Just 3 is new Con("Just", 1, [3n]).
  // 構成子の値：Just 3 は new Con("Just", 1, [3n])
  class Con {
    constructor(tag, index, args) {
      this.tag = tag;
      this.index = index;
      this.args = args;
    }
  }

  // A list: cells, each with its head and the rest (null is []).
  // リスト：セルをつなぐ。セルは頭と残り（null は []）
  class Cons {
    constructor(h, t) {
      this.h = h;
      this.t = t;
    }
  }

  // A Map: its pairs, sorted by key. A Set: its keys, sorted.
  // Map：キーの順に並べた組。Set：並べたキー
  class LamMap {
    constructor(pairs) {
      this.pairs = pairs;
    }
  }
  class LamSet {
    constructor(keys) {
      this.keys = keys;
    }
  }

  // An error of Lam: error "...", head [], a case that fits nothing.
  // Lam のエラー：error "..."、head []、どの枝にも合わない case
  class LamError extends Error {}
  const fail = (message) => {
    throw new LamError(message);
  };
  // ANCHOR_END: values

  // ANCHOR: lists
  // 📋 Between Lam's lists and JavaScript's arrays.
  // Lam のリストと、JavaScript の配列のあいだ
  const list = (...xs) => fromArray(xs);
  function fromArray(xs) {
    let out = null;
    for (let i = xs.length - 1; i >= 0; i--) out = new Cons(xs[i], out);
    return out;
  }
  function toArray(xs) {
    const out = [];
    for (let c = xs; c !== null; c = c.t) out.push(c.h);
    return out;
  }
  const cons = (x, xs) => new Cons(x, xs);
  // ANCHOR_END: lists

  // ANCHOR: strings
  // 🔤 Lam counts the letters of a string (code points); JavaScript's
  //    .length counts UTF-16 units: "🧾" is 1 in Lam, 2 in JavaScript.
  //    So the runtime cuts strings by code points.
  // Lam は文字列の文字（コードポイント）を数え、JavaScript の .length は
  // UTF-16 の単位を数える。"🧾" は Lam で 1、JavaScript で 2。だから
  // ランタイムは、文字列をコードポイントで切る
  function codePoints(s) {
    let n = 0;
    for (let i = 0; i < s.length; i++) {
      const u = s.charCodeAt(i);
      // A high surrogate starts a pair: count the pair once.
      // 上位サロゲートはペアの始まり。ペアで1つと数える
      if (u >= 0xd800 && u <= 0xdbff && i + 1 < s.length) i++;
      n++;
    }
    return n;
  }
  const letters = (s) => Array.from(s);

  // ⚖️ Compare two strings by code points, as Lam (Haskell's Text)
  //    does. JavaScript's < compares UTF-16 units: "｡" < "😀" is
  //    false there, but U+FF61 comes before U+1F600.
  // 2つの文字列を、Lam（Haskell の Text）と同じくコードポイントで比べる。
  // JavaScript の < は UTF-16 の単位で比べる
  function compareStrings(a, b) {
    const n = Math.min(a.length, b.length);
    for (let i = 0; i < n; i++) {
      const x = a.charCodeAt(i);
      const y = b.charCodeAt(i);
      if (x !== y) {
        const p = a.codePointAt(i);
        const q = b.codePointAt(i);
        return p < q ? -1 : 1;
      }
    }
    return a.length === b.length ? 0 : a.length < b.length ? -1 : 1;
  }
  // ANCHOR_END: strings

  // ANCHOR: compare
  // ⚖️ Compare two values, as lam does: numbers by value (an Int and
  //    a Float, too), lists and tuples item by item, a constructor
  //    written first is smaller, records by their fields in the order
  //    of names. A function cannot be compared.
  // lam と同じく、2つの値を比べる。数は値で（Int と Float でも）、リストと
  // タプルは1つずつ、先に書いた構成子が小さい、レコードはフィールドを
  // 名前の順に。関数は比べられない
  function compare(a, b, nan = nanLast) {
    if (typeof a === "bigint" && typeof b === "bigint") {
      return a < b ? -1 : a > b ? 1 : 0;
    }
    if (isNumber(a) && isNumber(b)) return nan(Number(a), Number(b));
    if (typeof a === "boolean") return a === b ? 0 : a ? 1 : -1;
    if (typeof a === "string") return compareStrings(a, b);
    if (Array.isArray(a)) return compareArrays(a, b, nan);
    if (a === null || a instanceof Cons) {
      return compareArrays(toArray(a), toArray(b), nan);
    }
    if (a instanceof Con) {
      if (a.index !== b.index) return a.index < b.index ? -1 : 1;
      return compareArrays(a.args, b.args, nan);
    }
    if (a instanceof LamMap) {
      return compareArrays(a.pairs, b.pairs, nan);
    }
    if (a instanceof LamSet) return compareArrays(a.keys, b.keys, nan);
    if (typeof a === "function") {
      return fail("cannot compare a function: " + show(a));
    }
    const names = Object.keys(a).sort(compareStrings);
    return compareArrays(names.map((k) => a[k]),
                         names.map((k) => b[k]), nan);
  }
  function compareArrays(xs, ys, nan) {
    const n = Math.min(xs.length, ys.length);
    for (let i = 0; i < n; i++) {
      const o = compare(xs[i], ys[i], nan);
      if (o !== 0) return o;
    }
    return xs.length === ys.length ? 0 : xs.length < ys.length ? -1 : 1;
  }
  const isNumber = (x) => typeof x === "bigint" || typeof x === "number";
  // Haskell's compare on Doubles: NaN is larger than everything.
  // Haskell の Double の compare：NaN は、どれよりも大きい
  function nanLast(x, y) {
    return x < y ? -1 : x === y ? 0 : 1;
  }
  // The keys of a Map: NaN is equal to NaN, to find it again.
  // Map のキー：NaN は NaN と等しい。また引けるように
  function keyOrder(x, y) {
    if (Number.isNaN(x)) return Number.isNaN(y) ? 0 : 1;
    if (Number.isNaN(y)) return -1;
    return x < y ? -1 : x > y ? 1 : 0;
  }
  // ANCHOR_END: compare

  // ANCHOR: operators
  // ➕ The operators of Lam. Two Ints give a BigInt; if a Float is on
  //    either side, both are numbers (an Int literal can stand for a
  //    Float: 1.5 + 1).
  // Lam の演算子。Int どうしなら BigInt。どちらかが Float なら、両方を
  // number に（Int の書き方の値が Float のこともある：1.5 + 1）
  const both = (a, b) => typeof a === "bigint" && typeof b === "bigint";
  const add = (a, b) => (both(a, b) ? a + b : Number(a) + Number(b));
  const sub = (a, b) => (both(a, b) ? a - b : Number(a) - Number(b));
  const mul = (a, b) => (both(a, b) ? a * b : Number(a) * Number(b));
  const div = (a, b) => Number(a) / Number(b);
  // // rounds down, as Haskell's div and Python's //. JavaScript's
  // BigInt / goes to 0: -7n / 2n is -3n, but -7 // 2 is -4.
  // // は Haskell の div・Python の // と同じく下へ丸める。JavaScript の
  // BigInt の / は 0 の方へ：-7n / 2n は -3n だが、-7 // 2 は -4
  function intDiv(a, b) {
    if (b === 0n) fail("divided by zero");
    const q = a / b;
    return (a % b !== 0n) && ((a < 0n) !== (b < 0n)) ? q - 1n : q;
  }
  const divMaybe = (a, b) =>
    Number(b) === 0 ? nothing : just(Number(a) / Number(b));
  const intDivMaybe = (a, b) => (b === 0n ? nothing : just(intDiv(a, b)));
  // == on two numbers, one a Float: as IEEE 754 says (NaN == NaN is
  // false). Anything else: compare the insides.
  // 片方が Float の数どうしの ==：IEEE 754 のとおり。ほかは中身を比べる
  const floats = (a, b) => isNumber(a) && isNumber(b) && !both(a, b);
  const eq = (a, b) =>
    floats(a, b) ? Number(a) === Number(b) : compare(a, b) === 0;
  const ne = (a, b) =>
    floats(a, b) ? Number(a) !== Number(b) : compare(a, b) !== 0;
  const lt = (a, b) =>
    floats(a, b) ? Number(a) < Number(b) : compare(a, b) < 0;
  const le = (a, b) =>
    floats(a, b) ? Number(a) <= Number(b) : compare(a, b) <= 0;
  const gt = (a, b) =>
    floats(a, b) ? Number(a) > Number(b) : compare(a, b) > 0;
  const ge = (a, b) =>
    floats(a, b) ? Number(a) >= Number(b) : compare(a, b) >= 0;
  // ++ on strings or lists. A list is copied on the left only.
  // 文字列かリストの ++。リストは左だけを写す
  function append(a, b) {
    if (typeof a === "string") return a + b;
    const left = toArray(a);
    let out = b;
    for (let i = left.length - 1; i >= 0; i--) out = new Cons(left[i], out);
    return out;
  }
  // m ?: d : the value in Just or Ok, or else d.
  // m ?: d：Just か Ok の中身。なければ d
  const orElse = (m, d) =>
    m.tag === "Just" || m.tag === "Ok" ? m.args[0] : d();
  // ANCHOR_END: operators

  // ANCHOR: constructors
  // 🧱 Constructors the runtime makes itself: their numbers are the
  //    ones of the prelude (Nothing is 0, Just is 1).
  // ランタイムが自分で作る構成子。番号はプレリュードと同じ
  const con = (tag, index, args) => new Con(tag, index, args);
  const nothing = con("Nothing", 0, []);
  const just = (x) => con("Just", 1, [x]);
  const ok = (x) => con("Ok", 0, [x]);
  const err = (e) => con("Err", 1, [e]);
  const maybe = (x) => (x === undefined ? nothing : just(x));
  const unit = [];
  const time = (ms) => con("Time", 0, [ms]);
  const zone = (minutes) => con("Zone", 0, [minutes]);
  const noMatch = (v) => fail("no branch of case fits " + show(v));
  // ANCHOR_END: constructors

  // ANCHOR: show
  // 🖨️ A value, as lam shows it: [1, 2], Just (-3), { age = 3 }.
  // lam と同じ形で、値を見せる
  function show(v) {
    if (typeof v === "bigint") return String(v);
    if (typeof v === "number") return showFloat(v);
    if (typeof v === "boolean") return v ? "true" : "false";
    if (typeof v === "string") return showString(v);
    if (Array.isArray(v)) return "(" + v.map(show).join(", ") + ")";
    if (v === null || v instanceof Cons) {
      return "[" + toArray(v).map(show).join(", ") + "]";
    }
    if (v instanceof LamMap) {
      const items = v.pairs.map(([k, x]) => showKey(k) + ": " + show(x));
      return "{" + items.join(", ") + "}";
    }
    if (v instanceof LamSet) return "Set.fromList " + show(list(...v.keys));
    if (v instanceof Con) return showCon(v);
    if (typeof v === "function") return "<function>";
    if (v instanceof Action) return "<IO action>";
    if (v instanceof Task) return "<task>";
    const names = Object.keys(v).sort(compareStrings);
    return "{ " + names.map((k) => k + " = " + show(v[k])).join(", ") + " }";
  }
  function showCon(v) {
    if (v.tag === "Time" && v.args.length === 1) return isoOf(v.args[0]);
    if (v.tag === "Zone" && v.args.length === 1) return offsetOf(v.args[0]);
    if (v.args.length === 0) return v.tag;
    return [v.tag, ...v.args.map(argument)].join(" ");
  }
  // An argument with arguments of its own, or a negative number, is put
  // in ( ): Just (Just 3), Just (-3).
  // 自分の引数を持つ引数と、負の数は ( ) で包む
  function argument(a) {
    const timeLike = a instanceof Con && a.args.length === 1 &&
      (a.tag === "Time" || a.tag === "Zone");
    if (timeLike) return show(a);
    if (a instanceof Con && a.args.length > 0) return "(" + show(a) + ")";
    if (a instanceof LamSet) return "(" + show(a) + ")";
    if (isNumber(a) && a < 0) return "(" + show(a) + ")";
    return show(a);
  }
  // A Float, as JavaScript shows it: Lam took its way of showing from
  // JavaScript (and Elm), so String is right as it is.
  // Float は JavaScript の見せ方。Lam は JavaScript（と Elm）から見せ方を
  // 取ったので、String がそのまま合う
  const showFloat = (x) => String(x);
  function showString(s) {
    let out = "\"";
    for (const c of s) {
      if (c === "\"") out += "\\\"";
      else if (c === "\\") out += "\\\\";
      else if (c === "\n") out += "\\n";
      else if (c === "\t") out += "\\t";
      else out += c;
    }
    return out + "\"";
  }
  function showKey(k) {
    if (typeof k === "string") return showString(k);
    if (isNumber(k)) return show(k);
    return "[" + show(k) + "]";
  }
  // ANCHOR_END: show

  // ANCHOR: maps
  // 🗺️ A Map keeps its pairs sorted by key: finding a key is a binary
  //    search, and adding one makes a new array (the old Map stays as
  //    it is: Lam's values never change).
  // Map は組をキーの順に並べておく。キーを探すのは二分探索、足すと新しい
  // 配列を作る（古い Map はそのまま。Lam の値は変わらない）
  function findKey(items, key, keyOf) {
    let lo = 0;
    let hi = items.length;
    while (lo < hi) {
      const mid = (lo + hi) >> 1;
      const o = compare(keyOf(items[mid]), key, keyOrder);
      if (o === 0) return [true, mid];
      if (o < 0) lo = mid + 1;
      else hi = mid;
    }
    return [false, lo];
  }
  const pairKey = (p) => p[0];
  const itself = (k) => k;
  function mapInsert(key, value, m) {
    const [found, i] = findKey(m.pairs, key, pairKey);
    const pairs = m.pairs.slice();
    pairs.splice(i, found ? 1 : 0, [key, value]);
    return new LamMap(pairs);
  }
  function mapLookup(key, m) {
    const [found, i] = findKey(m.pairs, key, pairKey);
    return found ? just(m.pairs[i][1]) : nothing;
  }
  function mapDelete(key, m) {
    const [found, i] = findKey(m.pairs, key, pairKey);
    if (!found) return m;
    const pairs = m.pairs.slice();
    pairs.splice(i, 1);
    return new LamMap(pairs);
  }
  function mapFromList(xs) {
    let m = new LamMap([]);
    for (const [k, v] of toArray(xs)) m = mapInsert(k, v, m);
    return m;
  }
  // The left one wins, as Haskell's Map.union.
  // Haskell の Map.union と同じく、左が勝つ
  function mapUnion(a, b) {
    let m = b;
    for (const [k, v] of a.pairs) m = mapInsert(k, v, m);
    return m;
  }
  function setInsert(key, s) {
    const [found, i] = findKey(s.keys, key, itself);
    if (found) return s;
    const keys = s.keys.slice();
    keys.splice(i, 0, key);
    return new LamSet(keys);
  }
  const setHas = (key, s) => findKey(s.keys, key, itself)[0];
  function setDelete(key, s) {
    const [found, i] = findKey(s.keys, key, itself);
    if (!found) return s;
    const keys = s.keys.slice();
    keys.splice(i, 1);
    return new LamSet(keys);
  }
  const setFrom = (keys) => keys.reduce((s, k) => setInsert(k, s),
                                        new LamSet([]));
  // ANCHOR_END: maps

  // ANCHOR: numbers
  // 🔢 From a Float to Just an Int (Nothing for NaN and Infinity). The
  //    part after the point is found exactly: x - Math.trunc(x) loses
  //    nothing.
  // Float から Int へ（NaN と Infinity は Nothing）。小数点のあとは正確に
  // 求める。x - Math.trunc(x) は何も失わない
  function toInt(round) {
    return (v) => {
      const x = Number(v);
      if (!Number.isFinite(x)) return nothing;
      return just(BigInt(round(x)));
    };
  }
  // 2.5 is 3, -2.5 is -3: away from 0.
  // 0 から遠い方へ
  function roundAway(x) {
    const whole = Math.trunc(x);
    const part = x - whole;
    if (Math.abs(part) >= 0.5) return whole + Math.sign(x);
    return whole;
  }
  // 2.5 is 2, 3.5 is 4: to the even one.
  // 偶数の方へ
  function roundEven(x) {
    const low = Math.floor(x);
    const part = x - low;
    if (part > 0.5) return low + 1;
    if (part < 0.5) return low;
    return low % 2 === 0 ? low : low + 1;
  }
  function roundUp(x) {
    const low = Math.floor(x);
    return x - low >= 0.5 ? low + 1 : low;
  }
  // The text of a number in String.toFloat: Haskell's read (no .5,
  // no 5., no Infinity).
  // String.toFloat が読む数の文字。Haskell の read と同じ
  const floatText = /^-?[0-9]+(\.[0-9]+)?(e[+-]?[0-9]+)?$/;
  function readFloat(s) {
    const t = s.startsWith("+") ? s.slice(1) : s;
    return floatText.test(t) ? just(Number(t)) : nothing;
  }
  // Float.fixed 2 x: two digits after the point, rounded away from 0.
  // JavaScript's toFixed works on the exact value of x, as lam does.
  // 点のあとに2桁、0 から遠い方へ丸める。JavaScript の toFixed は x の
  // 正確な値で丸める。lam と同じ
  function fixed(n, v) {
    const x = Number(v);
    if (Number.isNaN(x)) return "NaN";
    if (!Number.isFinite(x)) return x > 0 ? "Infinity" : "-Infinity";
    const digits = Number(n < 0n ? 0n : n);
    const text = Math.abs(x).toFixed(digits);
    return (x < 0 ? "-" : "") + text;
  }
  // ANCHOR_END: numbers

  // ANCHOR: format
  // 📐 Padding and digits, counted in letters (code points).
  // 文字（コードポイント）で数えて、そろえる
  const spaces = (n) => " ".repeat(Math.max(0, n));
  const missing = (w, s) => Math.max(0, Number(w) - codePoints(s));
  const padLeft = (w, s) => spaces(missing(w, s)) + s;
  const padRight = (w, s) => s + spaces(missing(w, s));
  function center(w, s) {
    const d = missing(w, s);
    const left = Math.floor(d / 2);
    return spaces(left) + s + spaces(d - left);
  }
  function groupDigits(t) {
    const m = /^(-*)([0-9]*)([\s\S]*)$/.exec(t);
    const groups = [];
    for (let i = m[2].length; i > 0; i -= 3) {
      groups.unshift(m[2].slice(Math.max(0, i - 3), i));
    }
    return m[1] + groups.join(",") + m[3];
  }
  function zeroPad(w, t) {
    const m = /^(-*)([\s\S]*)$/.exec(t);
    return m[1] + "0".repeat(missing(w, t)) + m[2];
  }
  // ANCHOR_END: format

  // ANCHOR: json
  // 🧾 JSON, with the numbers kept exact: 12345678901234567890 is a
  //    JInt, 19.99 a JDec 1999 2. JSON.parse would make both a number,
  //    and lose digits, so the runtime reads JSON itself.
  // JSON。数は正確なまま：12345678901234567890 は JInt、19.99 は
  // JDec 1999 2。JSON.parse はどちらも number にして桁を失うので、
  // ランタイムが自分で読む
  const jNull = con("JNull", 0, []);
  const jBool = (b) => con("JBool", 1, [b]);
  const jStr = (s) => con("JStr", 4, [s]);
  const jArr = (xs) => con("JArr", 5, [xs]);
  const jObj = (m) => con("JObj", 6, [m]);
  // A number as its digits and the place of the point: JInt if whole.
  // 数を、数字の並びと小数点の場所から。整数なら JInt
  function jNumber(negative, digits, exponent) {
    let ds = digits.replace(/^0+(?=[0-9])/, "");
    let e = exponent;
    while (ds.length > 1 && ds.endsWith("0")) {
      ds = ds.slice(0, -1);
      e += 1;
    }
    let c = BigInt(ds);
    if (negative) c = -c;
    if (c === 0n) return con("JInt", 2, [0n]);
    if (e >= 0 && e <= 1024) return con("JInt", 2, [c * 10n ** BigInt(e)]);
    return con("JDec", 3, [c, BigInt(-e)]);
  }
  function parseJson(text) {
    let i = 0;
    const space = () => {
      while (i < text.length && " \t\n\r".includes(text[i])) i++;
    };
    const stop = (why) => {
      throw new SyntaxError(why + " at " + i);
    };
    function value() {
      space();
      const c = text[i];
      if (c === "{") return object();
      if (c === "[") return array();
      if (c === "\"") return jStr(string());
      if (text.startsWith("true", i)) return (i += 4), jBool(true);
      if (text.startsWith("false", i)) return (i += 5), jBool(false);
      if (text.startsWith("null", i)) return (i += 4), jNull;
      return number();
    }
    function object() {
      i++;
      let m = new LamMap([]);
      space();
      if (text[i] === "}") return i++, jObj(m);
      for (;;) {
        space();
        if (text[i] !== "\"") stop("a key is needed");
        const k = string();
        space();
        if (text[i++] !== ":") stop("a : is needed");
        m = mapInsert(k, value(), m);
        space();
        const c = text[i++];
        if (c === "}") return jObj(m);
        if (c !== ",") stop("a , or } is needed");
      }
    }
    function array() {
      i++;
      const xs = [];
      space();
      if (text[i] === "]") return i++, jArr(null);
      for (;;) {
        xs.push(value());
        space();
        const c = text[i++];
        if (c === "]") return jArr(fromArray(xs));
        if (c !== ",") stop("a , or ] is needed");
      }
    }
    function string() {
      const m = /^"(?:[^"\\\u0000-\u001f]|\\["\\/bfnrt]|\\u[0-9a-fA-F]{4})*"/
        .exec(text.slice(i));
      if (!m) stop("a string is broken");
      i += m[0].length;
      return JSON.parse(m[0]);
    }
    function number() {
      const m = /^(-?)(0|[1-9][0-9]*)(?:\.([0-9]+))?(?:[eE]([+-]?[0-9]+))?/
        .exec(text.slice(i));
      if (!m) stop("not a JSON value");
      i += m[0].length;
      const fraction = m[3] || "";
      const e = Number(m[4] || "0") - fraction.length;
      return jNumber(m[1] === "-", m[2] + fraction, e);
    }
    const v = value();
    space();
    if (i < text.length) stop("extra text after the JSON value");
    return v;
  }
  // 🖨️ JSON as aeson writes it: 19.99, 1.0e-7, and \u0008 for \b.
  // aeson と同じ書き方の JSON
  function jsonText(v, indent, depth) {
    const pad = (d) => (indent ? "\n" + "  ".repeat(d) : "");
    const [x] = v.args;
    switch (v.tag) {
      case "JNull": return "null";
      case "JBool": return x ? "true" : "false";
      case "JInt": return String(x);
      case "JDec": return decimalText(x, v.args[1]);
      case "JStr": return jsonString(x);
      case "JArr": {
        const items = toArray(x);
        if (items.length === 0) return "[]";
        const parts = items.map((j) => pad(depth + 1) +
                                       jsonText(j, indent, depth + 1));
        return "[" + parts.join(",") + pad(depth) + "]";
      }
      case "JObj": {
        if (x.pairs.length === 0) return "{}";
        const colon = indent ? ": " : ":";
        const parts = x.pairs.map(([k, j]) => pad(depth + 1) +
          jsonString(k) + colon + jsonText(j, indent, depth + 1));
        return "{" + parts.join(",") + pad(depth) + "}";
      }
    }
    return fail("Json.show: not a Json value");
  }
  function jsonString(s) {
    let out = "\"";
    for (const c of s) {
      const u = c.codePointAt(0);
      if (c === "\"") out += "\\\"";
      else if (c === "\\") out += "\\\\";
      else if (c === "\n") out += "\\n";
      else if (c === "\t") out += "\\t";
      else if (c === "\r") out += "\\r";
      else if (u < 0x20) out += "\\u" + u.toString(16).padStart(4, "0");
      else out += c;
    }
    return out + "\"";
  }
  // JDec c d is c times 10 to the -d. As Haskell's Scientific shows it:
  // with a point from 0.1 up to 10 million, else with e.
  // JDec c d は c × 10 の -d 乗。Haskell の Scientific と同じ見せ方：
  // 0.1 から 1000万までは小数点で、ほかは e で
  function decimalText(c, d) {
    const sign = c < 0n ? "-" : "";
    const digits = String(c < 0n ? -c : c);
    const e = digits.length - Number(d);
    if (e < 0 || e > 7) {
      const rest = digits.length > 1 ? digits.slice(1) : "0";
      return sign + digits[0] + "." + rest + "e" + (e - 1);
    }
    if (e === 0) return sign + "0." + digits;
    return sign + digits.slice(0, e) + "." + digits.slice(e);
  }
  // A Float as JSON: the fewest digits (String gives them).
  // Float を JSON に：いちばん少ない桁（String がくれる）
  function jsonFromFloat(v) {
    const x = Number(v);
    if (!Number.isFinite(x)) return jNull;
    const m = /^(-?)([0-9]+)(?:\.([0-9]+))?(?:e([+-][0-9]+))?$/
      .exec(String(x));
    const fraction = m[3] || "";
    const e = Number(m[4] || "0") - fraction.length;
    return jNumber(m[1] === "-", m[2] + fraction, e);
  }
  function jsonToFloat(v) {
    if (v.tag === "JInt") return just(Number(v.args[0]));
    if (v.tag === "JDec") {
      return just(Number(String(v.args[0]) + "e-" + String(v.args[1])));
    }
    return nothing;
  }
  // ANCHOR_END: json

  // ANCHOR: paths
  // 🛣️ Paths, as Haskell's filepath does them (on macOS and Linux).
  // 道。Haskell の filepath と同じ（macOS と Linux）
  function pathJoin(a, b) {
    if (b.startsWith("/") || a === "") return b;
    return a.endsWith("/") ? a + b : a + "/" + b;
  }
  const basename = (p) => p.slice(p.lastIndexOf("/") + 1);
  function dirname(p) {
    let d = p.slice(0, p.lastIndexOf("/") + 1);
    while (d.length > 1 && d.endsWith("/")) d = d.slice(0, -1);
    return d === "" ? "." : d;
  }
  function extension(p) {
    const name = basename(p);
    const dot = name.lastIndexOf(".");
    return dot < 0 ? "" : name.slice(dot);
  }
  function stem(p) {
    const name = basename(p);
    const dot = name.lastIndexOf(".");
    return dot < 0 ? name : name.slice(0, dot);
  }
  function withExtension(e, p) {
    const base = p.slice(0, p.length - extension(p).length);
    if (e === "") return base;
    return e.startsWith(".") ? base + e : base + "." + e;
  }
  // ANCHOR_END: paths

  // ANCHOR: clock
  // 🕰️ A Time is the milliseconds since 1970, shown in UTC as
  //    ISO 8601; a Zone is its minutes ahead of UTC.
  // Time は 1970年からのミリ秒で、UTC の ISO 8601 で見せる。Zone は
  // UTC より進んだ分
  function isoOf(ms) {
    const text = new Date(Number(ms)).toISOString();
    return text.replace(".000Z", "Z");
  }
  function offsetOf(m) {
    const n = Number(m);
    const sign = n < 0 ? "-" : "+";
    const two = (x) => String(x).padStart(2, "0");
    return sign + two(Math.floor(Math.abs(n) / 60)) + ":" +
      two(Math.abs(n) % 60);
  }
  const isoText = new RegExp(
    "^([0-9]{4})-([0-9]{2})-([0-9]{2})" +
    "(?:T([0-9]{2}):([0-9]{2}):([0-9]{2})(?:\\.([0-9]+))?" +
    "(Z|[+-][0-9]{2}:[0-9]{2}))?$");
  function fromIso(text) {
    const m = isoText.exec(text);
    if (!m) return err("not an ISO 8601 time: " + text);
    const [, y, mo, d, h = "0", mi = "0", s = "0", frac = "", z = "Z"] = m;
    let ms = Date.UTC(Number(y), Number(mo) - 1, Number(d),
                      Number(h), Number(mi), Number(s),
                      Number((frac + "000").slice(0, 3)));
    if (z !== "Z") {
      const minutes = Number(z.slice(1, 3)) * 60 + Number(z.slice(4, 6));
      ms -= (z[0] === "-" ? -minutes : minutes) * 60000;
    }
    return ok(time(BigInt(ms)));
  }
  // ANCHOR_END: clock

  // ANCHOR: actions
  // 📮 An IO value is a request, as data: making one does nothing.
  //    Only run does them (the same as lam's runner, Lam.Io).
  // IO の値は、データにした頼みごと。作っても何も起きない。やるのは run
  // だけ（lam の実行役 Lam.Io と同じ）
  class Action {
    constructor(kind, fields) {
      this.kind = kind;
      Object.assign(this, fields);
    }
  }
  const pure = (value) => new Action("pure", { value });
  const ask = (tag, fields = {}) =>
    new Action("ask", { request: { tag, ...fields } });
  const bindIO = (first, f) => new Action("bind", { first, rest: f });
  class Task {
    constructor(promise) {
      this.promise = promise;
    }
  }
  // ANCHOR_END: actions

  // ANCHOR: prims
  // 📋 The builtins: each name of Lam, how many arguments it takes, and
  //    what it does with them. prim(name) gives it curried, the same as
  //    a function of Lam: Map.insert(k)(v)(m).
  // 組み込み：Lam の名前ごとに、引数の数と、引数ですること。prim(name)
  // は、Lam の関数と同じくカリー化して渡す：Map.insert(k)(v)(m)
  const n = (v) => Number(v);
  const prims = {
    // Strings, tuples, lists
    // 文字列・タプル・リスト
    "$key.name": [1, (s) => s],
    "toUpper": [1, (s) => s.toUpperCase()],
    "toLower": [1, (s) => s.toLowerCase()],
    "chars": [1, (s) => fromArray(letters(s))],
    "join": [2, (sep, xs) => toArray(xs).join(sep)],
    "showInt": [1, (i) => String(i)],
    "fst": [1, (p) => p[0]],
    "snd": [1, (p) => p[1]],
    "length": [1, (x) => BigInt(typeof x === "string" ? codePoints(x)
                                                     : toArray(x).length)],
    "head": [1, (xs) => (xs === null ? fail("head: the list is empty")
                                     : xs.h)],
    "tail": [1, (xs) => (xs === null ? fail("tail: the list is empty")
                                     : xs.t)],
    "lines": [1, (s) => fromArray(linesOf(s))],
    "words": [1, (s) => fromArray(s.split(/\s+/).filter((w) => w !== ""))],
    "String.isLetter": [1, (s) => /^\p{L}+$/u.test(s)],
    "String.toInt": [1, (s) =>
      (/^-?[0-9]+$/.test(s) ? just(BigInt(s)) : nothing)],
    "String.toFloat": [1, readFloat],
    "String.trim": [1, (s) => s.trim()],
    "String.trimStart": [1, (s) => s.trimStart()],
    "String.trimEnd": [1, (s) => s.trimEnd()],
    "String.split": [2, (sep, s) => fromArray(splitOn(sep, s))],
    "String.startsWith": [2, (x, s) => s.startsWith(x)],
    "String.endsWith": [2, (x, s) => s.endsWith(x)],
    "String.contains": [2, (x, s) => s.includes(x)],
    "String.replace": [3, (o, w, s) => splitOn(o, s).join(w)],
    "String.repeat": [2, (k, s) => s.repeat(Math.max(0, n(k)))],
    "String.reverse": [1, (s) => letters(s).reverse().join("")],
    "String.take": [2, (k, s) => letters(s).slice(0, Math.max(0, n(k)))
                                            .join("")],
    "String.toUpper": [1, (s) => s.toUpperCase()],
    "String.toLower": [1, (s) => s.toLowerCase()],
    "String.join": [2, (sep, xs) => toArray(xs).join(sep)],
    "String.lines": [1, (s) => fromArray(linesOf(s))],
    "String.words": [1, (s) =>
      fromArray(s.split(/\s+/).filter((w) => w !== ""))],
    "String.drop": [2, (k, s) => letters(s).slice(Math.max(0, n(k)))
                                            .join("")],
    // Map and Set
    // Map と Set
    "Map.empty": [0, () => new LamMap([])],
    "Map.insert": [3, mapInsert],
    "Map.lookup": [2, mapLookup],
    "Map.delete": [2, mapDelete],
    "Map.member": [2, (k, m) => findKey(m.pairs, k, pairKey)[0]],
    "Map.size": [1, (m) => BigInt(m.pairs.length)],
    "Map.toList": [1, (m) => fromArray(m.pairs.map(([k, v]) => [k, v]))],
    "Map.fromList": [1, mapFromList],
    "Map.keys": [1, (m) => fromArray(m.pairs.map(pairKey))],
    "Map.values": [1, (m) => fromArray(m.pairs.map((p) => p[1]))],
    "Map.union": [2, mapUnion],
    "Map.map": [2, (f, m) => new LamMap(m.pairs.map(([k, v]) => [k, f(v)]))],
    "Set.empty": [0, () => new LamSet([])],
    "Set.fromList": [1, (xs) => setFrom(toArray(xs))],
    "Set.toList": [1, (s) => fromArray(s.keys)],
    "Set.insert": [2, setInsert],
    "Set.delete": [2, setDelete],
    "Set.member": [2, setHas],
    "Set.size": [1, (s) => BigInt(s.keys.length)],
    "Set.union": [2, (a, b) => setFrom([...a.keys, ...b.keys])],
    "Set.intersection": [2, (a, b) =>
      new LamSet(a.keys.filter((k) => setHas(k, b)))],
    "Set.difference": [2, (a, b) =>
      new LamSet(a.keys.filter((k) => !setHas(k, b)))],
    // Numbers
    // 数
    "toFloat": [1, (i) => Number(i)],
    "round": [1, toInt(roundAway)],
    "floor": [1, toInt(Math.floor)],
    "ceiling": [1, toInt(Math.ceil)],
    "truncate": [1, toInt(Math.trunc)],
    "Float.roundTiesEven": [1, toInt(roundEven)],
    "Float.roundTiesUp": [1, toInt(roundUp)],
    "negate": [1, (x) => -x],
    "sqrt": [1, (x) => Math.sqrt(n(x))],
    "Float.exp": [1, (x) => Math.exp(n(x))],
    "Float.log": [1, (x) => Math.log(n(x))],
    "Float.pow": [2, (x, y) => n(x) ** n(y)],
    "Float.pi": [0, () => Math.PI],
    "Float.isNaN": [1, (x) => Number.isNaN(n(x))],
    "Float.isInfinite": [1, (x) => Math.abs(n(x)) === Infinity],
    "showFloat": [1, (x) => showFloat(n(x))],
    "Float.fixed": [2, fixed],
    // JSON, and showing values
    // JSON と、値を見せること
    "Json.parse": [1, (s) => {
      try {
        return ok(parseJson(s));
      } catch (e) {
        return err(e.message);
      }
    }],
    "Json.show": [1, (j) => jsonText(j, false, 0)],
    "Json.pretty": [1, (j) => jsonText(j, true, 0)],
    "Json.toFloat": [1, jsonToFloat],
    "Json.fromFloat": [1, jsonFromFloat],
    "show": [1, show],
    "showNumber": [1, show],
    "padLeft": [2, padLeft],
    "padRight": [2, padRight],
    "center": [2, center],
    "groupDigits": [1, groupDigits],
    "zeroPad": [2, zeroPad],
    // ANCHOR: primsIo
    // IO: each one is a request, as data
    // IO：どれも、データにした頼みごと
    "IO.returnIO": [1, pure],
    "IO.bindIO": [2, bindIO],
    "IO.putStr": [1, (text) => ask("PutStr", { text })],
    "IO.putStrLn": [1, (text) => ask("PutStr", { text: text + "\n" })],
    "IO.getLine": [0, () => ask("GetLine")],
    "IO.readFile": [1, (path) => ask("ReadFile", { path })],
    "IO.writeFile": [2, (path, text) => ask("WriteFile", { path, text })],
    "IO.finally": [2, (body, cleanup) =>
      new Action("finally", { body, cleanup })],
    "IO.try": [1, (body) => new Action("catch", { body })],
    "IO.getArgs": [0, () => ask("GetArgs")],
    "IO.lookupEnv": [1, (key) => ask("LookupEnv", { key })],
    "IO.getEnvironment": [0, () => ask("GetEnvironment")],
    "IO.getContents": [0, () => ask("GetContents")],
    "IO.ePutStr": [1, (text) => ask("PutErr", { text })],
    "IO.ePutStrLn": [1, (text) => ask("PutErr", { text: text + "\n" })],
    "IO.exit": [1, (code) => new Action("exit", { code: n(code) })],
    "error": [1, (message) => fail(message)],
    "todo": [1, (message) => fail("not written yet: " + message)],
    // ANCHOR_END: primsIo
    // Paths, files, the clock, dice
    // 道・ファイル・時計・さいころ
    "Path.join": [2, pathJoin],
    "Path.basename": [1, basename],
    "Path.dirname": [1, dirname],
    "Path.extension": [1, extension],
    "Path.stem": [1, stem],
    "Path.withExtension": [2, withExtension],
    "File.list": [1, (path) => ask("ListDir", { path })],
    "File.exists": [1, (path) => ask("Exists", { path })],
    "File.isDir": [1, (path) => ask("IsDir", { path })],
    "File.makeDir": [1, (path) => ask("MakeDir", { path })],
    "File.remove": [1, (path) => ask("Remove", { path })],
    "File.rename": [2, (from, to) => ask("Rename", { from, to })],
    "File.copy": [2, (from, to) => ask("CopyFile", { from, to })],
    "File.append": [2, (path, text) => ask("AppendFile", { path, text })],
    "Time.now": [0, () => ask("Now")],
    "Time.toIso": [1, (t) => isoOf(t.args[0])],
    "Time.fromIso": [1, fromIso],
    "Random.int": [2, (lo, hi) => ask("RandomInt", { lo, hi })],
    // At once
    // 同時に
    "sleep": [1, (ms) => (ms < 0n
      ? fail("sleep: needs an Int, 0 or more")
      : ask("Sleep", { ms: n(ms) }))],
    "IO.all": [1, (xs) => gather(0, false, xs)],
    "IO.allN": [2, (k, xs) => (k < 1n
      ? fail("IO.allN: needs an Int, 1 or more")
      : gather(n(k), false, xs))],
    "IO.both": [2, (a, b) => bindIO(gather(0, false, list(a, b)),
                                    (xs) => pure([xs.h, xs.t.h]))],
    "IO.allOk": [1, (xs) => gather(0, true, xs)],
    "IO.race": [1, (xs) => new Action("race", { actions: toArray(xs) })],
    "spawn": [1, (work) => new Action("spawn", { work })],
    "await": [1, (task) => new Action("await", { task })],
    "Debug.log": [2, (label, v) => {
      fs.writeSync(2, label + ": " + show(v) + "\n");
      return v;
    }],
    // ANCHOR: natives
    // The definitions of List that the runtime writes as loops.
    // List の定義のうち、ランタイムがループで書くもの
    "List.foldr": [3, (f, z, xs) => {
      const items = toArray(xs);
      let acc = z;
      for (let i = items.length - 1; i >= 0; i--) acc = f(items[i])(acc);
      return acc;
    }],
    "List.take": [2, (k, xs) => {
      const out = [];
      for (let c = xs; c !== null && out.length < k; c = c.t) out.push(c.h);
      return fromArray(out);
    }],
    "List.merge": [3, (before, xs, ys) => {
      const out = [];
      let a = xs;
      let b = ys;
      while (a !== null && b !== null) {
        if (before(b.h)(a.h)) {
          out.push(b.h);
          b = b.t;
        } else {
          out.push(a.h);
          a = a.t;
        }
      }
      return append(fromArray(out), a === null ? b : a);
    }],
    // ANCHOR_END: natives
    "Prelude.range": [2, (a, b) => {
      const out = [];
      for (let i = a; i <= b; i++) out.push(i);
      return fromArray(out);
    }],
    "Prelude.replicate": [2, (k, x) =>
      fromArray(new Array(Math.max(0, n(k))).fill(x))],
    "Prelude.zipWith": [3, (f, xs, ys) => {
      const out = [];
      for (let a = xs, b = ys; a !== null && b !== null; a = a.t, b = b.t) {
        out.push(f(a.h)(b.h));
      }
      return fromArray(out);
    }],
    "Prelude.takeWhile": [2, (p, xs) => {
      const out = [];
      for (let c = xs; c !== null && p(c.h); c = c.t) out.push(c.h);
      return fromArray(out);
    }],
  };
  const gather = (most, untilErr, xs) =>
    new Action("gather", { most, untilErr, actions: toArray(xs) });
  function linesOf(s) {
    if (s === "") return [];
    const parts = s.split("\n");
    if (s.endsWith("\n")) parts.pop();
    return parts;
  }
  function splitOn(sep, s) {
    return sep === "" ? letters(s) : s.split(sep);
  }
  // ANCHOR: curry
  // 🍛 A builtin, curried: arity 0 is the value itself.
  // カリー化した組み込み。引数が 0 なら、値そのもの
  function prim(name) {
    const [arity, f] = prims[name];
    if (arity === 0) return f();
    const take = (got) => (x) =>
      got.length + 1 === arity ? f(...got, x) : take([...got, x]);
    return take([]);
  }
  // ANCHOR_END: curry
  // ANCHOR_END: prims

  // ANCHOR: run
  // 🏃 The runner: do an action, and give how it ended:
  //      { value }          it gave a value
  //      { failed: why }    it stopped with an error
  //      { exited: code }   exit code
  //    Bind is done with a stack, not by calling run again: a long do
  //    (or a loop of a million binds) does not use up JavaScript's.
  // 実行役：頼みごとをやり、終わり方を返す。Bind は run を呼び直さず、
  // スタックで回す。長い do（や、百万回の bind のループ）でも、
  // JavaScript のスタックを使い切らない
  async function run(action, job) {
    const rests = [];
    let current = action;
    for (;;) {
      if (cancelled(job)) return { failed: "cancelled" };
      let done;
      // pure and bind need no waiting: only the others go to step.
      // pure と bind は待たなくてよい。ほかだけを step へ
      if (current.kind === "bind") {
        rests.push(current.rest);
        current = current.first;
        continue;
      } else if (current.kind === "pure") {
        done = { value: current.value };
      } else {
        try {
          done = await step(current, rests, job);
        } catch (e) {
          return { failed: messageOf(e) };
        }
      }
      if (done.next) {
        current = done.next;
        continue;
      }
      if (!("value" in done)) return done;
      if (rests.length === 0) return done;
      try {
        current = rests.pop()(done.value);
      } catch (e) {
        return { failed: messageOf(e) };
      }
    }
  }
  // A job is cancelled if it, or a job above it, is.
  // 仕事が取り消されたのは、自分か、上の仕事が取り消されたとき
  function cancelled(job) {
    for (let j = job; j; j = j.parent) if (j.cancelled) return true;
    return false;
  }
  const child = (job) => ({ cancelled: false, parent: job });
  const messageOf = (e) =>
    e instanceof LamError ? e.message : String(e && e.message || e);

  // 👣 One step: a value, the next action, or how it ended.
  // 1歩：値か、次の頼みごとか、終わり方
  async function step(a, rests, job) {
    switch (a.kind) {
      case "pure": return { value: a.value };
      case "ask": return { value: await world(a.request, job, self) };
      case "bind":
        rests.push(a.rest);
        return { next: a.first };
      case "exit": return { exited: a.code };
      // The cleanup comes after the body, whatever the body did.
      // 後始末は、本体が何をしても、本体のあとに
      case "finally": {
        const body = await run(a.body, job);
        const after = await run(a.cleanup, { cancelled: false });
        return "value" in after ? body : after;
      }
      // IO.try: an error becomes an Err; exit goes on out.
      // IO.try：エラーは Err に。exit はそのまま外へ
      case "catch": {
        const body = await run(a.body, job);
        if ("value" in body) return { value: ok(body.value) };
        if ("failed" in body) return { value: err(body.failed) };
        return body;
      }
      case "gather": return gatherAll(a, job);
      case "race": return raceAll(a.actions, job);
      case "spawn": {
        const promise = run(a.work, { cancelled: false });
        return { value: new Task(promise) };
      }
      case "await": return a.task.promise;
    }
    return { failed: "not an IO action: " + show(a) };
  }
  // ANCHOR_END: run

  // ANCHOR: together
  // 🧵 IO.all and its friends: at most `most` at once (0: no limit).
  //    The first stop (or, for IO.allOk, the first Err) cancels the
  //    others, waits for their cleanups, and is the answer.
  // IO.all の仲間：同時に多くて most 個（0 は上限なし）。最初に止まった
  // もの（IO.allOk なら最初の Err）が、ほかを取り消し、その後始末を待って、
  // 答えになる
  async function gatherAll(a, job) {
    const jobs = a.actions.map(() => child(job));
    const answers = new Array(a.actions.length);
    let first = null;
    let next = 0;
    const final = (r) =>
      !("value" in r) || (a.untilErr && r.value.tag === "Err");
    async function worker() {
      while (next < a.actions.length && first === null) {
        const i = next++;
        const r = await run(a.actions[i], jobs[i]);
        answers[i] = r;
        if (final(r) && first === null) {
          first = r;
          jobs.forEach((j) => (j.cancelled = true));
        }
      }
    }
    const width = a.most === 0 ? a.actions.length : a.most;
    const workers = [];
    for (let k = 0; k < Math.max(1, width); k++) workers.push(worker());
    await Promise.all(workers);
    if (cancelled(job)) return { failed: "cancelled" };
    if (first !== null) return a.untilErr && "value" in first
      ? { value: first.value } : first;
    const values = answers.map((r) => r.value);
    if (a.untilErr) return { value: ok(fromArray(values.map((r) =>
      r.args[0]))) };
    return { value: fromArray(values) };
  }
  // IO.race: the first to end wins; the others are cancelled.
  // IO.race：最初に終わったものが勝ち。ほかは取り消す
  async function raceAll(actions, job) {
    if (actions.length === 0) {
      return { failed: "IO.race: got no actions" };
    }
    const jobs = actions.map(() => child(job));
    const runs = actions.map((x, i) =>
      run(x, jobs[i]).then((r) => [i, r]));
    const [winner, answer] = await Promise.race(runs);
    jobs.forEach((j, i) => i !== winner && (j.cancelled = true));
    await Promise.all(runs);
    return cancelled(job) ? { failed: "cancelled" } : answer;
  }
  // ANCHOR_END: together

  // ANCHOR: start
  // 🚪 Start the program: compute its definitions and main, run main,
  //    and end the process as lam would: an error is "error: ..." and
  //    exit code 1; main's Err is the same; exit n is n.
  // プログラムを始める：定義と main を計算し、main を動かし、lam と同じく
  // プロセスを終える。エラーは「error: ...」と終了コード 1。main の Err も
  // 同じ。exit n は n
  async function start(program) {
    let main;
    try {
      main = program();
    } catch (e) {
      return finish({ failed: messageOf(e) });
    }
    finish(await run(main, { cancelled: false }));
  }
  function finish(done) {
    if ("exited" in done) process.exit(done.exited);
    if ("failed" in done) {
      fs.writeSync(2, "error: " + done.failed + "\n");
      process.exit(1);
    }
    const v = done.value;
    if (v instanceof Con && v.tag === "Err") {
      const [reason] = v.args;
      const said = typeof reason === "string" ? reason : show(reason);
      fs.writeSync(2, "error: " + said + "\n");
      process.exit(1);
    }
    process.exit(0);
  }
  // ANCHOR_END: start

  const self = {
    prim, prims, con, list, cons, unit, noMatch, start, run,
    add, sub, mul, div, intDiv, divMaybe, intDivMaybe,
    eq, ne, lt, le, gt, ge, append, orElse,
    show, compare, codePoints, fromArray, toArray,
    LamError, Con, Cons, LamMap, LamSet, Action,
    ok, err, just, nothing, maybe, time, zone, cancelled,
  };
  return self;
}

// ANCHOR: world
// 🌍 The real world of node: here, at last, the requests are done.
//    rt is the runtime, to make the answers with. A test can give
//    makeRuntime a fake world instead (the same idea as lam's Handler).
// node の本物の世界：ここで、やっと頼みごとをやる。rt は答えを作るための
// ランタイム。テストは、代わりに偽物の世界を makeRuntime に渡せる
// （lam の Handler と同じ考え）
export function realWorld() {
  const decoder = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
  const stdin = lineReader(0);
  return async function world(r, job, rt) {
    const done = (path, act) => {
      try {
        return rt.ok(act());
      } catch (e) {
        return rt.err(path + ": " + why(e));
      }
    };
    switch (r.tag) {
      case "PutStr":
        fs.writeSync(1, r.text);
        return rt.unit;
      case "PutErr":
        fs.writeSync(2, r.text);
        return rt.unit;
      case "GetLine": return rt.maybe(stdin.line() ?? undefined);
      case "GetContents": return stdin.rest();
      case "ReadFile":
        return done(r.path, () => textOf(fs.readFileSync(r.path)));
      case "WriteFile":
        return done(r.path, () => (fs.writeFileSync(r.path, r.text),
                                   rt.unit));
      case "AppendFile":
        return done(r.path, () => (fs.appendFileSync(r.path, r.text),
                                   rt.unit));
      case "GetArgs": return rt.fromArray(process.argv.slice(2));
      case "LookupEnv": return rt.maybe(process.env[r.key]);
      case "GetEnvironment":
        return rt.prim("Map.fromList")(
          rt.fromArray(Object.entries(process.env)));
      case "ListDir":
        return done(r.path,
                    () => rt.fromArray(fs.readdirSync(r.path).sort()));
      case "Exists": return fs.existsSync(r.path);
      case "IsDir":
        return fs.existsSync(r.path) && fs.statSync(r.path).isDirectory();
      case "MakeDir":
        return done(r.path, () => (fs.mkdirSync(r.path, { recursive: true }),
                                   rt.unit));
      case "Remove":
        return done(r.path, () => (fs.statSync(r.path).isDirectory()
          ? fs.rmdirSync(r.path) : fs.unlinkSync(r.path), rt.unit));
      case "Rename":
        return done(r.from, () => (fs.renameSync(r.from, r.to), rt.unit));
      case "CopyFile":
        return done(r.from, () => (fs.copyFileSync(r.from, r.to), rt.unit));
      case "Now": return rt.time(BigInt(Date.now()));
      case "RandomInt": {
        const span = r.hi - r.lo + 1n;
        if (span <= 0n) return r.lo;
        return r.lo + BigInt(Math.floor(Math.random() * Number(span)));
      }
      case "Sleep": return sleep(r.ms, () => rt.cancelled(job));
    }
    throw new Error("the real world cannot do " + r.tag);
  };
  // Bytes as text, strictly: a file that is not UTF-8 is an Err.
  // バイト列を文字に。厳しく：UTF-8 でないファイルは Err
  function textOf(bytes) {
    try {
      return decoder.decode(bytes);
    } catch {
      throw { lam: "not UTF-8 from byte " + badByte(bytes) };
    }
  }
  // The short words lam uses for a failure.
  // lam が失敗に使う短い言葉
  function why(e) {
    if (e.lam) return e.lam;
    if (e.code === "ENOENT") return "no such file";
    if (e.code === "EACCES" || e.code === "EPERM") {
      return "permission denied";
    }
    return e.message;
  }
}
// ANCHOR_END: world

// ⏳ Wait, but wake up early when the job is cancelled.
// 待つ。ただし、仕事が取り消されたら早めに起きる
function sleep(ms, cancelled) {
  return new Promise((resolve) => {
    const timer = setTimeout(() => {
      clearInterval(look);
      resolve([]);
    }, ms);
    const look = setInterval(() => {
      if (cancelled()) {
        clearTimeout(timer);
        clearInterval(look);
        resolve([]);
      }
    }, 5);
  });
}

// 📖 The standard input, a line at a time, read as it is needed.
// 標準入力。要るときに、1行ずつ読む
function lineReader(fd) {
  const decoder = new TextDecoder("utf-8", { ignoreBOM: true });
  let text = "";
  let ended = false;
  function more() {
    const buffer = Buffer.alloc(65536);
    let got = 0;
    try {
      got = fs.readSync(fd, buffer, 0, buffer.length, null);
    } catch (e) {
      if (e.code === "EAGAIN") return true;
      if (e.code !== "EOF") got = 0;
    }
    if (got === 0) {
      text += decoder.decode();
      ended = true;
      return false;
    }
    text += decoder.decode(buffer.subarray(0, got), { stream: true });
    return true;
  }
  return {
    line() {
      for (;;) {
        const at = text.indexOf("\n");
        if (at >= 0) {
          const line = text.slice(0, at);
          text = text.slice(at + 1);
          return line;
        }
        if (ended || !more()) {
          if (text === "") return null;
          const last = text;
          text = "";
          return last;
        }
      }
    },
    rest() {
      while (!ended && more());
      const all = text;
      text = "";
      return all;
    },
  };
}

// 🔍 Where bytes stop being UTF-8: the first byte of the first broken
//    letter (as lam says it).
// バイト列が UTF-8 でなくなる所：最初の壊れた文字の、最初のバイト
function badByte(b) {
  let i = 0;
  while (i < b.length) {
    const x = b[i];
    let need = 0;
    let low = 0x80;
    let high = 0xbf;
    if (x < 0x80) need = 0;
    else if (x >= 0xc2 && x <= 0xdf) need = 1;
    else if (x >= 0xe0 && x <= 0xef) {
      need = 2;
      if (x === 0xe0) low = 0xa0;
      if (x === 0xed) high = 0x9f;
    } else if (x >= 0xf0 && x <= 0xf4) {
      need = 3;
      if (x === 0xf0) low = 0x90;
      if (x === 0xf4) high = 0x8f;
    } else return i;
    for (let k = 1; k <= need; k++) {
      const y = b[i + k];
      const lo = k === 1 ? low : 0x80;
      const hi = k === 1 ? high : 0xbf;
      if (y === undefined || y < lo || y > hi) return i;
    }
    i += need + 1;
  }
  return i;
}
// ANCHOR_END: all
