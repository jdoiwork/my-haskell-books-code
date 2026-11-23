# ANCHOR: all
# The runtime of lam to-python: what a Lam program needs in Python, but
# does not write itself. lam puts this file in front of every program it
# turns into Python. It is runtime/lam.mjs of lam to-js, written again.
# lam to-python のランタイム：Lam のプログラムが Python で要るが、自分では
# 書かないもの。lam は、Python にするどのプログラムの前にも、このファイルを
# 入れる。lam to-js の runtime/lam.mjs を、書き直したもの。
import asyncio
import codecs
import datetime
import decimal
import json
import math
import os
import random
import re
import shutil
import sys
import time
import types

# ANCHOR: recursion
# 🪜 Python stops a call 1000 deep (sys.getrecursionlimit()). A Lam
#    function that is not a loop calls itself once for each item, so
#    let it go deeper: 100000 is fine for Python 3.12 and later.
# Python は、1000 段の深さで呼び出しを止める。ループでない Lam の関数は、
# 要素1つごとに自分を1回呼ぶので、もっと深くまで許す
sys.setrecursionlimit(100_000)
# ANCHOR_END: recursion


# ANCHOR: values
# 🧱 The values of Lam that Python has no shape for. An Int is an int, a
#    Float a float, a String a str, a Bool a bool, a tuple a tuple, a
#    record a dict. The others:
# Python に形のない Lam の値。Int は int、Float は float、String は str、
# Bool は bool、タプルは tuple、レコードは dict。ほかは次のとおり

# A constructor's value: Just 3 is Con("Just", 1, (3,)). In a match,
# Con("Just", [x]) looks at the tag and the arguments.
# 構成子の値：Just 3 は Con("Just", 1, (3,))。match では
# Con("Just", [x]) が、名前と引数を見る
class Con:
    __slots__ = ("tag", "index", "args")
    __match_args__ = ("tag", "args")

    def __init__(self, tag, index, args):
        self.tag = tag
        self.index = index
        self.args = args


# A list: cells, each with its head and the rest (None is []).
# リスト：セルをつなぐ。セルは頭と残り（None は []）
class Cons:
    __slots__ = ("h", "t")
    __match_args__ = ("h", "t")

    def __init__(self, h, t):
        self.h = h
        self.t = t


# A Map: its pairs, sorted by key. A Set: its keys, sorted.
# Map：キーの順に並べた組。Set：並べたキー
class LamMap:
    __slots__ = ("pairs",)

    def __init__(self, pairs):
        self.pairs = pairs


class LamSet:
    __slots__ = ("keys",)

    def __init__(self, keys):
        self.keys = keys


# An error of Lam: error "...", head [], a case that fits nothing.
# Lam のエラー：error "..."、head []、どの枝にも合わない case
class LamError(Exception):
    pass


def fail(message):
    raise LamError(message)
# ANCHOR_END: values


# ANCHOR: lists
# 📋 Between Lam's lists and Python's lists.
# Lam のリストと、Python のリストのあいだ
def from_list(xs):
    out = None
    for x in reversed(xs):
        out = Cons(x, out)
    return out


def to_list(xs):
    out = []
    while xs is not None:
        out.append(xs.h)
        xs = xs.t
    return out


def lam_list(*xs):
    return from_list(xs)
# ANCHOR_END: lists


# ANCHOR: compare
# ⚖️ Compare two values, as lam does: numbers by value (an Int and a
#    Float, too), lists and tuples item by item, a constructor written
#    first is smaller, records by their fields in the order of names. A
#    function cannot be compared. A bool is an int in Python, so it is
#    looked at first.
# lam と同じく、2つの値を比べる。数は値で（Int と Float でも）、リストと
# タプルは1つずつ、先に書いた構成子が小さい、レコードはフィールドを名前の
# 順に。関数は比べられない。Python の bool は int でもあるので、先に見る
NUMBERS = (int, float)


def is_number(x):
    return type(x) in NUMBERS


# Haskell's compare on Doubles: NaN is larger than everything.
# Haskell の Double の compare：NaN は、どれよりも大きい
def nan_last(x, y):
    return -1 if x < y else 0 if x == y else 1


# The keys of a Map: NaN is equal to NaN, to find it again.
# Map のキー：NaN は NaN と等しい。また引けるように
def key_order(x, y):
    if x != x:
        return 0 if y != y else 1
    if y != y:
        return -1
    return -1 if x < y else 1 if x > y else 0


def compare(a, b, nan=nan_last):
    ta = type(a)
    if ta is int and type(b) is int:
        return -1 if a < b else 1 if a > b else 0
    if ta is bool:
        return 0 if a == b else 1 if a else -1
    if ta in NUMBERS and type(b) in NUMBERS:
        return nan(float(a), float(b))
    if ta is str:
        # Python's str compares code points, as Lam (Haskell's Text).
        # Python の str は、Lam（Haskell の Text）と同じくコードポイントで
        return -1 if a < b else 1 if a > b else 0
    if ta is tuple:
        return compare_seq(a, b, nan)
    if a is None or ta is Cons:
        return compare_seq(to_list(a), to_list(b), nan)
    if ta is Con:
        if a.index != b.index:
            return -1 if a.index < b.index else 1
        return compare_seq(a.args, b.args, nan)
    if ta is LamMap:
        return compare_seq(a.pairs, b.pairs, nan)
    if ta is LamSet:
        return compare_seq(a.keys, b.keys, nan)
    if ta is dict:
        names = sorted(a)
        return compare_seq([a[k] for k in names], [b[k] for k in names], nan)
    return fail("cannot compare a function: " + show(a))


def compare_seq(xs, ys, nan):
    for x, y in zip(xs, ys):
        o = compare(x, y, nan)
        if o != 0:
            return o
    return 0 if len(xs) == len(ys) else -1 if len(xs) < len(ys) else 1
# ANCHOR_END: compare


# ANCHOR: operators
# ➕ The operators that are not Python's own. + - * are written as they
#    are: an int and a float give a float, as in lam. / and // here
#    stop as lam does: 1 / 0 is Infinity in Lam, but Python raises
#    ZeroDivisionError.
# Python のものでない演算子。+ - * はそのまま書く：int と float なら
# float。lam と同じ。/ は、lam と同じに止まる（Lam の 1 / 0 は
# Infinity だが、Python は ZeroDivisionError にする）
def div(a, b):
    x = float(a)
    y = float(b)
    if y == 0:
        if x != x or x == 0:
            return math.nan
        # The sign of 0 matters: 1 / -0 is -Infinity.
        # 0 の符号も見る：1 / -0 は -Infinity
        return math.copysign(math.inf, x) * math.copysign(1.0, y)
    return x / y


def div_maybe(a, b):
    return nothing if float(b) == 0 else just(float(a) / float(b))


def int_div_maybe(a, b):
    return nothing if b == 0 else just(a // b)


# == on two numbers, one a Float: as IEEE 754 says (NaN == NaN is
# false). Anything else: compare the insides.
# 片方が Float の数どうしの ==：IEEE 754 のとおり。ほかは中身を比べる
def floats(a, b):
    return (is_number(a) and is_number(b)
            and not (type(a) is int and type(b) is int))


def eq(a, b):
    return a == b if floats(a, b) else compare(a, b) == 0


def ne(a, b):
    return a != b if floats(a, b) else compare(a, b) != 0


def lt(a, b):
    return a < b if floats(a, b) else compare(a, b) < 0


def le(a, b):
    return a <= b if floats(a, b) else compare(a, b) <= 0


def gt(a, b):
    return a > b if floats(a, b) else compare(a, b) > 0


def ge(a, b):
    return a >= b if floats(a, b) else compare(a, b) >= 0


# ++ on strings or lists. A list is copied on the left only.
# 文字列かリストの ++。リストは左だけを写す
def append(a, b):
    if type(a) is str:
        return a + b
    out = b
    for x in reversed(to_list(a)):
        out = Cons(x, out)
    return out


# m ?: d : the value in Just or Ok, or else d.
# m ?: d：Just か Ok の中身。なければ d
def or_else(m, d):
    return m.args[0] if m.tag in ("Just", "Ok") else d()
# ANCHOR_END: operators


# ANCHOR: constructors
# 🧱 Constructors the runtime makes itself: their numbers are the ones
#    of the prelude (Nothing is 0, Just is 1).
# ランタイムが自分で作る構成子。番号はプレリュードと同じ
nothing = Con("Nothing", 0, ())


def just(x):
    return Con("Just", 1, (x,))


def ok(x):
    return Con("Ok", 0, (x,))


def err(e):
    return Con("Err", 1, (e,))


def maybe(x):
    return nothing if x is None else just(x)


unit = ()


def lam_time(ms):
    return Con("Time", 0, (ms,))


def no_match(v):
    return fail("no branch of case fits " + show(v))
# ANCHOR_END: constructors


# ANCHOR: show
# 🖨️ A value, as lam shows it: [1, 2], Just (-3), { age = 3 }.
# lam と同じ形で、値を見せる
def show(v):
    t = type(v)
    if t is bool:
        return "true" if v else "false"
    if t is int:
        return str(v)
    if t is float:
        return show_float(v)
    if t is str:
        return show_string(v)
    if t is tuple:
        return "(" + ", ".join(map(show, v)) + ")"
    if v is None or t is Cons:
        return "[" + ", ".join(map(show, to_list(v))) + "]"
    if t is LamMap:
        items = [show_key(k) + ": " + show(x) for k, x in v.pairs]
        return "{" + ", ".join(items) + "}"
    if t is LamSet:
        return "Set.fromList " + show(from_list(v.keys))
    if t is Con:
        return show_con(v)
    if t is Action:
        return "<IO action>"
    if t is Task:
        return "<task>"
    if t is dict:
        names = sorted(v)
        return "{ " + ", ".join(k + " = " + show(v[k]) for k in names) + " }"
    return "<function>"


def show_con(v):
    if v.tag == "Time" and len(v.args) == 1:
        return iso_of(v.args[0])
    if v.tag == "Zone" and len(v.args) == 1:
        return offset_of(v.args[0])
    if not v.args:
        return v.tag
    return " ".join([v.tag] + [argument(a) for a in v.args])


# An argument with arguments of its own, or a negative number, is put in
# ( ): Just (Just 3), Just (-3).
# 自分の引数を持つ引数と、負の数は ( ) で包む
def argument(a):
    t = type(a)
    if t is Con and len(a.args) == 1 and a.tag in ("Time", "Zone"):
        return show(a)
    if (t is Con and a.args) or t is LamSet:
        return "(" + show(a) + ")"
    if t in NUMBERS and a < 0:
        return "(" + show(a) + ")"
    return show(a)


def show_string(s):
    out = ['"']
    for c in s:
        if c == '"':
            out.append('\\"')
        elif c == "\\":
            out.append("\\\\")
        elif c == "\n":
            out.append("\\n")
        elif c == "\t":
            out.append("\\t")
        else:
            out.append(c)
    out.append('"')
    return "".join(out)


def show_key(k):
    if type(k) is str:
        return show_string(k)
    if is_number(k):
        return show(k)
    return "[" + show(k) + "]"
# ANCHOR_END: show


# ANCHOR: showFloat
# 🔢 A Float, as Lam shows it (JavaScript's way): the fewest digits that
#    give the same Float back, and no ".0" for a whole number. Python's
#    repr also gives the fewest digits, but writes 1.0, 1e+16 and 1e-07.
#      1.0 is "1", 1e16 is "10000000000000000", 1e-7 is "1e-7"
# Float を、Lam（JavaScript）と同じ形で見せる。同じ Float に戻る
# いちばん少ない桁で、整数なら ".0" を付けない。Python の repr も
# いちばん少ない桁をくれるが、1.0・1e+16・1e-07 と書く
def show_float(x):
    if x != x:
        return "NaN"
    if math.isinf(x):
        return "Infinity" if x > 0 else "-Infinity"
    if x == 0:
        return "0"
    if x < 0:
        return "-" + show_float(-x)
    # repr: the digits, and where the point goes (n: 0.d1d2... × 10^n).
    # repr から、数字の並びと小数点の場所
    whole, _, rest = repr(x).partition("e")
    left, _, right = whole.partition(".")
    point = len(left) + (int(rest) if rest else 0)
    digits = (left + right).lstrip("0")
    point -= len(left + right) - len(digits)
    digits = digits.rstrip("0")
    k = len(digits)
    if k <= point <= 21:
        return digits + "0" * (point - k)
    if 0 < point <= 21:
        return digits[:point] + "." + digits[point:]
    if -6 < point <= 0:
        return "0." + "0" * -point + digits
    e = point - 1
    exponent = ("e+" if e >= 0 else "e-") + str(abs(e))
    if k == 1:
        return digits + exponent
    return digits[0] + "." + digits[1:] + exponent
# ANCHOR_END: showFloat


# ANCHOR: maps
# 🗺️ A Map keeps its pairs sorted by key: finding a key is a binary
#    search, and adding one makes a new list (the old Map stays as it
#    is: Lam's values never change).
# Map は組をキーの順に並べておく。キーを探すのは二分探索、足すと新しい
# リストを作る（古い Map はそのまま。Lam の値は変わらない）
def find_key(items, key, key_of):
    lo = 0
    hi = len(items)
    while lo < hi:
        mid = (lo + hi) // 2
        o = compare(key_of(items[mid]), key, key_order)
        if o == 0:
            return True, mid
        if o < 0:
            lo = mid + 1
        else:
            hi = mid
    return False, lo


def pair_key(p):
    return p[0]


def itself(k):
    return k


def map_insert(key, value, m):
    found, i = find_key(m.pairs, key, pair_key)
    return LamMap(m.pairs[:i] + [(key, value)] + m.pairs[i + found:])


def map_lookup(key, m):
    found, i = find_key(m.pairs, key, pair_key)
    return just(m.pairs[i][1]) if found else nothing


def map_delete(key, m):
    found, i = find_key(m.pairs, key, pair_key)
    return LamMap(m.pairs[:i] + m.pairs[i + 1:]) if found else m


def map_from_list(xs):
    m = LamMap([])
    for k, v in to_list(xs):
        m = map_insert(k, v, m)
    return m


# The left one wins, as Haskell's Map.union.
# Haskell の Map.union と同じく、左が勝つ
def map_union(a, b):
    m = b
    for k, v in a.pairs:
        m = map_insert(k, v, m)
    return m


def set_insert(key, s):
    found, i = find_key(s.keys, key, itself)
    return s if found else LamSet(s.keys[:i] + [key] + s.keys[i:])


def set_has(key, s):
    return find_key(s.keys, key, itself)[0]


def set_delete(key, s):
    found, i = find_key(s.keys, key, itself)
    return LamSet(s.keys[:i] + s.keys[i + 1:]) if found else s


def set_from(keys):
    s = LamSet([])
    for k in keys:
        s = set_insert(k, s)
    return s
# ANCHOR_END: maps


# ANCHOR: numbers
# 🔢 From a Float to Just an Int (Nothing for NaN and Infinity). Python's
#    math.floor and math.trunc give an int already, of any size.
# Float から Int へ（NaN と Infinity は Nothing）。Python の math.floor と
# math.trunc は、どんな大きさでも、もう int をくれる
def to_int(rounding):
    def go(v):
        x = float(v)
        if math.isnan(x) or math.isinf(x):
            return nothing
        return just(int(rounding(x)))
    return go


# 2.5 is 3, -2.5 is -3: away from 0. Python's round goes to the even one.
# 0 から遠い方へ。Python の round は偶数の方へ
def round_away(x):
    whole = math.trunc(x)
    if abs(x - whole) >= 0.5:
        return whole + (1 if x > 0 else -1)
    return whole


def round_even(x):
    low = math.floor(x)
    part = x - low
    if part > 0.5:
        return low + 1
    if part < 0.5:
        return low
    return low if low % 2 == 0 else low + 1


def round_up(x):
    low = math.floor(x)
    return low + 1 if x - low >= 0.5 else low


# The text of a number in String.toFloat: Haskell's read (no .5, no 5.,
# no Infinity). Python's float would take all of those.
# String.toFloat が読む数の文字。Haskell の read と同じ。Python の float は、
# どれも読んでしまう
FLOAT_TEXT = re.compile(r"-?[0-9]+(\.[0-9]+)?(e[+-]?[0-9]+)?")


def read_float(s):
    t = s[1:] if s.startswith("+") else s
    return just(float(t)) if FLOAT_TEXT.fullmatch(t) else nothing


# Float.fixed 2 x: two digits after the point, rounded away from 0, on
# the exact value of x (as JavaScript's toFixed, and lam). Python's
# format(x, ".2f") would round a tie to the even one.
# 点のあとに2桁、x の正確な値で、0 から遠い方へ丸める（JavaScript の
# toFixed、lam と同じ）。Python の format は、ちょうど真ん中を偶数の方へ
def fixed(n, v):
    x = float(v)
    if math.isnan(x):
        return "NaN"
    if math.isinf(x):
        return "Infinity" if x > 0 else "-Infinity"
    if abs(x) >= 1e21:
        return show_float(x)
    digits = max(0, n)
    with decimal.localcontext() as context:
        context.prec = 500
        exact = decimal.Decimal(abs(x))
        step = decimal.Decimal(1).scaleb(-digits)
        text = format(exact.quantize(step, decimal.ROUND_HALF_UP), "f")
    return ("-" if x < 0 else "") + text


# The functions of math that raise where Lam (IEEE 754) gives a value:
# math.sqrt(-1) and math.log(0) raise ValueError, math.exp(1000)
# OverflowError.
# Lam（IEEE 754）が値を返す所で、例外にする math の関数
def sqrt(x):
    x = float(x)
    return math.sqrt(x) if x >= 0 else math.nan


def log(x):
    x = float(x)
    if x == 0:
        return -math.inf
    return math.log(x) if x > 0 else math.nan


def exp(x):
    try:
        return math.exp(x)
    except OverflowError:
        return math.inf


def power(x, y):
    x = float(x)
    y = float(y)
    try:
        return math.pow(x, y)
    except ValueError:
        if x == 0:
            return math.inf
        return math.nan
    except OverflowError:
        odd = y.is_integer() and int(y) % 2 == 1
        return -math.inf if x < 0 and odd else math.inf
# ANCHOR_END: numbers


# ANCHOR: format
# 📐 Padding and digits, counted in letters (code points): Python's len
#    counts code points already.
# 文字（コードポイント）で数えて、そろえる。Python の len は、もう
# コードポイントを数える
def missing(w, s):
    return max(0, w - len(s))


def pad_left(w, s):
    return " " * missing(w, s) + s


def pad_right(w, s):
    return s + " " * missing(w, s)


def center(w, s):
    d = missing(w, s)
    left = d // 2
    return " " * left + s + " " * (d - left)


def group_digits(t):
    m = re.fullmatch(r"(-*)([0-9]*)(.*)", t, re.DOTALL)
    digits = m.group(2)
    groups = []
    i = len(digits)
    while i > 0:
        groups.insert(0, digits[max(0, i - 3):i])
        i -= 3
    return m.group(1) + ",".join(groups) + m.group(3)


def zero_pad(w, t):
    m = re.fullmatch(r"(-*)(.*)", t, re.DOTALL)
    return m.group(1) + "0" * missing(w, t) + m.group(2)
# ANCHOR_END: format


# ANCHOR: json
# 🧾 JSON, with the numbers kept exact: 12345678901234567890 is a JInt,
#    19.99 a JDec 1999 2. Python's json would make 19.99 a float, so the
#    runtime reads JSON itself (as the one of lam to-js).
# JSON。数は正確なまま：12345678901234567890 は JInt、19.99 は
# JDec 1999 2。Python の json は 19.99 を float にするので、ランタイムが
# 自分で読む（lam to-js のものと同じ）
j_null = Con("JNull", 0, ())


def j_bool(b):
    return Con("JBool", 1, (b,))


def j_str(s):
    return Con("JStr", 4, (s,))


def j_arr(xs):
    return Con("JArr", 5, (xs,))


def j_obj(m):
    return Con("JObj", 6, (m,))


# A number as its digits and the place of the point: JInt if whole.
# 数を、数字の並びと小数点の場所から。整数なら JInt
def j_number(negative, digits, exponent):
    ds = digits.lstrip("0") or "0"
    e = exponent
    while len(ds) > 1 and ds.endswith("0"):
        ds = ds[:-1]
        e += 1
    c = -int(ds) if negative else int(ds)
    if c == 0:
        return Con("JInt", 2, (0,))
    if 0 <= e <= 1024:
        return Con("JInt", 2, (c * 10 ** e,))
    return Con("JDec", 3, (c, -e))


JSON_STRING = re.compile(
    r'"(?:[^"\\\x00-\x1f]|\\["\\/bfnrt]|\\u[0-9a-fA-F]{4})*"')
JSON_NUMBER = re.compile(
    r"(-?)(0|[1-9][0-9]*)(?:\.([0-9]+))?(?:[eE]([+-]?[0-9]+))?")


def parse_json(text):
    at = [0]

    def space():
        while at[0] < len(text) and text[at[0]] in " \t\n\r":
            at[0] += 1

    def stop(why):
        raise ValueError(why + " at " + str(at[0]))

    def value():
        space()
        i = at[0]
        c = text[i:i + 1]
        if c == "{":
            return obj()
        if c == "[":
            return array()
        if c == '"':
            return j_str(string())
        for word, v in (("true", j_bool(True)), ("false", j_bool(False)),
                        ("null", j_null)):
            if text.startswith(word, i):
                at[0] += len(word)
                return v
        return number()

    def obj():
        at[0] += 1
        m = LamMap([])
        space()
        if text[at[0]:at[0] + 1] == "}":
            at[0] += 1
            return j_obj(m)
        while True:
            space()
            if text[at[0]:at[0] + 1] != '"':
                stop("a key is needed")
            k = string()
            space()
            if text[at[0]:at[0] + 1] != ":":
                stop("a : is needed")
            at[0] += 1
            m = map_insert(k, value(), m)
            space()
            c = text[at[0]:at[0] + 1]
            at[0] += 1
            if c == "}":
                return j_obj(m)
            if c != ",":
                stop("a , or } is needed")

    def array():
        at[0] += 1
        xs = []
        space()
        if text[at[0]:at[0] + 1] == "]":
            at[0] += 1
            return j_arr(None)
        while True:
            xs.append(value())
            space()
            c = text[at[0]:at[0] + 1]
            at[0] += 1
            if c == "]":
                return j_arr(from_list(xs))
            if c != ",":
                stop("a , or ] is needed")

    def string():
        m = JSON_STRING.match(text, at[0])
        if not m:
            stop("a string is broken")
        at[0] = m.end()
        return json.loads(m.group(0))

    def number():
        m = JSON_NUMBER.match(text, at[0])
        if not m:
            stop("not a JSON value")
        at[0] = m.end()
        fraction = m.group(3) or ""
        e = int(m.group(4) or "0") - len(fraction)
        return j_number(m.group(1) == "-", m.group(2) + fraction, e)

    v = value()
    space()
    if at[0] < len(text):
        stop("extra text after the JSON value")
    return v


# 🖨️ JSON as aeson writes it: 19.99, 1.0e-7, and \u0008 for \b.
# aeson と同じ書き方の JSON
def json_text(v, indent, depth):
    def pad(d):
        return "\n" + "  " * d if indent else ""
    tag = v.tag
    if tag == "JNull":
        return "null"
    x = v.args[0]
    if tag == "JBool":
        return "true" if x else "false"
    if tag == "JInt":
        return str(x)
    if tag == "JDec":
        return decimal_text(x, v.args[1])
    if tag == "JStr":
        return json_string(x)
    if tag == "JArr":
        items = to_list(x)
        if not items:
            return "[]"
        parts = [pad(depth + 1) + json_text(j, indent, depth + 1)
                 for j in items]
        return "[" + ",".join(parts) + pad(depth) + "]"
    if tag == "JObj":
        if not x.pairs:
            return "{}"
        colon = ": " if indent else ":"
        parts = [pad(depth + 1) + json_string(k) + colon
                 + json_text(j, indent, depth + 1) for k, j in x.pairs]
        return "{" + ",".join(parts) + pad(depth) + "}"
    return fail("Json.show: not a Json value")


def json_string(s):
    out = ['"']
    for c in s:
        u = ord(c)
        if c == '"':
            out.append('\\"')
        elif c == "\\":
            out.append("\\\\")
        elif c == "\n":
            out.append("\\n")
        elif c == "\t":
            out.append("\\t")
        elif c == "\r":
            out.append("\\r")
        elif u < 0x20:
            out.append("\\u%04x" % u)
        else:
            out.append(c)
    out.append('"')
    return "".join(out)


# JDec c d is c times 10 to the -d. As Haskell's Scientific shows it:
# with a point from 0.1 up to 10 million, else with e.
# JDec c d は c × 10 の -d 乗。Haskell の Scientific と同じ見せ方：
# 0.1 から 1000万までは小数点で、ほかは e で
def decimal_text(c, d):
    sign = "-" if c < 0 else ""
    digits = str(abs(c))
    e = len(digits) - d
    if e < 0 or e > 7:
        rest = digits[1:] if len(digits) > 1 else "0"
        return sign + digits[0] + "." + rest + "e" + str(e - 1)
    if e == 0:
        return sign + "0." + digits
    return sign + digits[:e] + "." + digits[e:]


# A Float as JSON: the fewest digits (show_float gives them).
# Float を JSON に：いちばん少ない桁（show_float がくれる）
def json_from_float(v):
    x = float(v)
    if math.isnan(x) or math.isinf(x):
        return j_null
    m = re.fullmatch(r"(-?)([0-9]+)(?:\.([0-9]+))?(?:e([+-][0-9]+))?",
                     show_float(x))
    fraction = m.group(3) or ""
    e = int(m.group(4) or "0") - len(fraction)
    return j_number(m.group(1) == "-", m.group(2) + fraction, e)


def json_to_float(v):
    if v.tag == "JInt":
        return just(float(v.args[0]))
    if v.tag == "JDec":
        return just(float(str(v.args[0]) + "e-" + str(v.args[1])))
    return nothing


def json_parse(s):
    try:
        return ok(parse_json(s))
    except ValueError as e:
        return err(str(e))
# ANCHOR_END: json


# ANCHOR: paths
# 🛣️ Paths, as Haskell's filepath does them (on macOS and Linux).
# 道。Haskell の filepath と同じ（macOS と Linux）
def path_join(a, b):
    if b.startswith("/") or a == "":
        return b
    return a + b if a.endswith("/") else a + "/" + b


def basename(p):
    return p[p.rfind("/") + 1:]


def dirname(p):
    d = p[:p.rfind("/") + 1]
    while len(d) > 1 and d.endswith("/"):
        d = d[:-1]
    return "." if d == "" else d


def extension(p):
    name = basename(p)
    dot = name.rfind(".")
    return "" if dot < 0 else name[dot:]


def stem(p):
    name = basename(p)
    dot = name.rfind(".")
    return name if dot < 0 else name[:dot]


def with_extension(e, p):
    base = p[:len(p) - len(extension(p))]
    if e == "":
        return base
    return base + e if e.startswith(".") else base + "." + e
# ANCHOR_END: paths


# ANCHOR: clock
# 🕰️ A Time is the milliseconds since 1970, shown in UTC as ISO 8601;
#    a Zone is its minutes ahead of UTC.
# Time は 1970年からのミリ秒で、UTC の ISO 8601 で見せる。Zone は UTC より
# 進んだ分
EPOCH = datetime.datetime(1970, 1, 1, tzinfo=datetime.timezone.utc)


def iso_of(ms):
    t = EPOCH + datetime.timedelta(milliseconds=ms)
    text = t.strftime("%Y-%m-%dT%H:%M:%S")
    part = ms % 1000
    return text + ("Z" if part == 0 else ".%03dZ" % part)


def offset_of(m):
    sign = "-" if m < 0 else "+"
    return "%s%02d:%02d" % (sign, abs(m) // 60, abs(m) % 60)


ISO_TEXT = re.compile(
    r"([0-9]{4})-([0-9]{2})-([0-9]{2})"
    r"(?:T([0-9]{2}):([0-9]{2}):([0-9]{2})(?:\.([0-9]+))?"
    r"(Z|[+-][0-9]{2}:[0-9]{2}))?")


def from_iso(text):
    m = ISO_TEXT.fullmatch(text)
    if not m:
        return err("not an ISO 8601 time: " + text)
    y, mo, d, h, mi, s, frac, z = m.groups()
    try:
        t = datetime.datetime(int(y), int(mo), int(d), int(h or 0),
                              int(mi or 0), int(s or 0),
                              tzinfo=datetime.timezone.utc)
    except ValueError:
        return err("not an ISO 8601 time: " + text)
    ms = (t - EPOCH) // datetime.timedelta(milliseconds=1)
    ms += int(((frac or "") + "000")[:3])
    if z and z != "Z":
        minutes = int(z[1:3]) * 60 + int(z[4:6])
        ms -= (-minutes if z[0] == "-" else minutes) * 60000
    return ok(lam_time(ms))
# ANCHOR_END: clock


# ANCHOR: strings
# 🔤 Strings: Python's str is a list of code points, the same as Lam's.
#    len, the slices, and < need no help (lam to-js needed some).
# 文字列：Python の str はコードポイントの並びで、Lam と同じ。len も
# 切り出しも < も、手助けが要らない（lam to-js では要った）
def lines_of(s):
    if s == "":
        return []
    parts = s.split("\n")
    if s.endswith("\n"):
        parts.pop()
    return parts


def split_on(sep, s):
    return list(s) if sep == "" else s.split(sep)


def is_letter(s):
    return s.isalpha()


INT_TEXT = re.compile(r"-?[0-9]+")


def read_int(s):
    return just(int(s)) if INT_TEXT.fullmatch(s) else nothing
# ANCHOR_END: strings


# ANCHOR: actions
# 📮 An IO value is a request, as data: making one does nothing. Only
#    run does them (the same as lam's runner, Lam.Io).
# IO の値は、データにした頼みごと。作っても何も起きない。やるのは run
# だけ（lam の実行役 Lam.Io と同じ）
class Action:
    def __init__(self, kind, **fields):
        self.kind = kind
        self.__dict__.update(fields)


def pure(value):
    return Action("pure", value=value)


def ask(tag, **fields):
    return Action("ask", request=dict(tag=tag, **fields))


def bind_io(first, f):
    return Action("bind", first=first, rest=f)


def gather(most, until_err, xs):
    return Action("gather", most=most, until_err=until_err,
                  actions=to_list(xs))


class Task:
    def __init__(self, future):
        self.future = future
# ANCHOR_END: actions


def sleep_for(ms):
    if ms < 0:
        return fail("sleep: needs an Int, 0 or more")
    return ask("Sleep", ms=ms)


def all_n(k, xs):
    if k < 1:
        return fail("IO.allN: needs an Int, 1 or more")
    return gather(k, False, xs)


def debug_log(label, v):
    write_all(2, label + ": " + show(v) + "\n")
    return v


# ANCHOR: prims
# 📋 The builtins: each name of Lam, how many arguments it takes, and
#    what it does with them. prim(name) gives it curried, the same as a
#    function of Lam: Map.insert(k)(v)(m).
# 組み込み：Lam の名前ごとに、引数の数と、引数ですること。prim(name) は、
# Lam の関数と同じくカリー化して渡す：Map.insert(k)(v)(m)
PRIMS = {
    # Strings, tuples, lists
    # 文字列・タプル・リスト
    "$key.name": (1, lambda s: s),
    "toUpper": (1, str.upper),
    "toLower": (1, str.lower),
    "chars": (1, lambda s: from_list(list(s))),
    "join": (2, lambda sep, xs: sep.join(to_list(xs))),
    "showInt": (1, str),
    "fst": (1, lambda p: p[0]),
    "snd": (1, lambda p: p[1]),
    "length": (1, lambda x: len(x) if type(x) is str else len(to_list(x))),
    "head": (1, lambda xs: fail("head: the list is empty")
             if xs is None else xs.h),
    "tail": (1, lambda xs: fail("tail: the list is empty")
             if xs is None else xs.t),
    "lines": (1, lambda s: from_list(lines_of(s))),
    "words": (1, lambda s: from_list(s.split())),
    "String.isLetter": (1, is_letter),
    "String.toInt": (1, read_int),
    "String.toFloat": (1, read_float),
    "String.trim": (1, str.strip),
    "String.trimStart": (1, str.lstrip),
    "String.trimEnd": (1, str.rstrip),
    "String.split": (2, lambda sep, s: from_list(split_on(sep, s))),
    "String.startsWith": (2, lambda x, s: s.startswith(x)),
    "String.endsWith": (2, lambda x, s: s.endswith(x)),
    "String.contains": (2, lambda x, s: x in s),
    "String.replace": (3, lambda o, w, s: w.join(split_on(o, s))),
    "String.repeat": (2, lambda k, s: s * max(0, k)),
    "String.reverse": (1, lambda s: s[::-1]),
    "String.take": (2, lambda k, s: s[:max(0, k)]),
    "String.drop": (2, lambda k, s: s[max(0, k):]),
    "String.toUpper": (1, str.upper),
    "String.toLower": (1, str.lower),
    "String.join": (2, lambda sep, xs: sep.join(to_list(xs))),
    "String.lines": (1, lambda s: from_list(lines_of(s))),
    "String.words": (1, lambda s: from_list(s.split())),
    # Map and Set
    # Map と Set
    "Map.empty": (0, lambda: LamMap([])),
    "Map.insert": (3, map_insert),
    "Map.lookup": (2, map_lookup),
    "Map.delete": (2, map_delete),
    "Map.member": (2, lambda k, m: find_key(m.pairs, k, pair_key)[0]),
    "Map.size": (1, lambda m: len(m.pairs)),
    "Map.toList": (1, lambda m: from_list(m.pairs)),
    "Map.fromList": (1, map_from_list),
    "Map.keys": (1, lambda m: from_list([k for k, _ in m.pairs])),
    "Map.values": (1, lambda m: from_list([v for _, v in m.pairs])),
    "Map.union": (2, map_union),
    "Map.map": (2, lambda f, m: LamMap([(k, f(v)) for k, v in m.pairs])),
    "Set.empty": (0, lambda: LamSet([])),
    "Set.fromList": (1, lambda xs: set_from(to_list(xs))),
    "Set.toList": (1, lambda s: from_list(s.keys)),
    "Set.insert": (2, set_insert),
    "Set.delete": (2, set_delete),
    "Set.member": (2, set_has),
    "Set.size": (1, lambda s: len(s.keys)),
    "Set.union": (2, lambda a, b: set_from(a.keys + b.keys)),
    "Set.intersection": (2, lambda a, b:
                         LamSet([k for k in a.keys if set_has(k, b)])),
    "Set.difference": (2, lambda a, b:
                       LamSet([k for k in a.keys if not set_has(k, b)])),
    # Numbers
    # 数
    "toFloat": (1, float),
    "round": (1, to_int(round_away)),
    "floor": (1, to_int(math.floor)),
    "ceiling": (1, to_int(math.ceil)),
    "truncate": (1, to_int(math.trunc)),
    "Float.roundTiesEven": (1, to_int(round_even)),
    "Float.roundTiesUp": (1, to_int(round_up)),
    "negate": (1, lambda x: -x),
    "sqrt": (1, sqrt),
    "Float.exp": (1, exp),
    "Float.log": (1, log),
    "Float.pow": (2, power),
    "Float.pi": (0, lambda: math.pi),
    "Float.isNaN": (1, lambda x: math.isnan(x)),
    "Float.isInfinite": (1, lambda x: math.isinf(x)),
    "showFloat": (1, lambda x: show_float(float(x))),
    "Float.fixed": (2, fixed),
    # JSON, and showing values
    # JSON と、値を見せること
    "Json.parse": (1, json_parse),
    "Json.show": (1, lambda j: json_text(j, False, 0)),
    "Json.pretty": (1, lambda j: json_text(j, True, 0)),
    "Json.toFloat": (1, json_to_float),
    "Json.fromFloat": (1, json_from_float),
    "show": (1, show),
    "showNumber": (1, show),
    "padLeft": (2, pad_left),
    "padRight": (2, pad_right),
    "center": (2, center),
    "groupDigits": (1, group_digits),
    "zeroPad": (2, zero_pad),
    # ANCHOR: primsIo
    # IO: each one is a request, as data
    # IO：どれも、データにした頼みごと
    "IO.returnIO": (1, pure),
    "IO.bindIO": (2, bind_io),
    "IO.putStr": (1, lambda text: ask("PutStr", text=text)),
    "IO.putStrLn": (1, lambda text: ask("PutStr", text=text + "\n")),
    "IO.getLine": (0, lambda: ask("GetLine")),
    "IO.readFile": (1, lambda path: ask("ReadFile", path=path)),
    "IO.writeFile": (2, lambda path, text:
                     ask("WriteFile", path=path, text=text)),
    "IO.finally": (2, lambda body, cleanup:
                   Action("finally", body=body, cleanup=cleanup)),
    "IO.try": (1, lambda body: Action("catch", body=body)),
    "IO.getArgs": (0, lambda: ask("GetArgs")),
    "IO.lookupEnv": (1, lambda key: ask("LookupEnv", key=key)),
    "IO.getEnvironment": (0, lambda: ask("GetEnvironment")),
    "IO.getContents": (0, lambda: ask("GetContents")),
    "IO.ePutStr": (1, lambda text: ask("PutErr", text=text)),
    "IO.ePutStrLn": (1, lambda text: ask("PutErr", text=text + "\n")),
    "IO.exit": (1, lambda code: Action("exit", code=code)),
    "error": (1, fail),
    "todo": (1, lambda message: fail("not written yet: " + message)),
    # ANCHOR_END: primsIo
    # Paths, files, the clock, dice
    # 道・ファイル・時計・さいころ
    "Path.join": (2, path_join),
    "Path.basename": (1, basename),
    "Path.dirname": (1, dirname),
    "Path.extension": (1, extension),
    "Path.stem": (1, stem),
    "Path.withExtension": (2, with_extension),
    "File.list": (1, lambda path: ask("ListDir", path=path)),
    "File.exists": (1, lambda path: ask("Exists", path=path)),
    "File.isDir": (1, lambda path: ask("IsDir", path=path)),
    "File.makeDir": (1, lambda path: ask("MakeDir", path=path)),
    "File.remove": (1, lambda path: ask("Remove", path=path)),
    "File.rename": (2, lambda a, b: ask("Rename", source=a, target=b)),
    "File.copy": (2, lambda a, b: ask("CopyFile", source=a, target=b)),
    "File.append": (2, lambda path, text:
                    ask("AppendFile", path=path, text=text)),
    "Time.now": (0, lambda: ask("Now")),
    "Time.toIso": (1, lambda t: iso_of(t.args[0])),
    "Time.fromIso": (1, from_iso),
    "Random.int": (2, lambda lo, hi: ask("RandomInt", lo=lo, hi=hi)),
    # At once
    # 同時に
    "sleep": (1, sleep_for),
    "IO.all": (1, lambda xs: gather(0, False, xs)),
    "IO.allN": (2, all_n),
    "IO.both": (2, lambda a, b: bind_io(gather(0, False, lam_list(a, b)),
                                        lambda xs: pure((xs.h, xs.t.h)))),
    "IO.allOk": (1, lambda xs: gather(0, True, xs)),
    "IO.race": (1, lambda xs: Action("race", actions=to_list(xs))),
    "spawn": (1, lambda work: Action("spawn", work=work)),
    "await": (1, lambda task: Action("await", task=task)),
    "Debug.log": (2, debug_log),
}
# ANCHOR_END: prims


# ANCHOR: natives
# 🏎️ The definitions of the standard modules that the runtime writes as
#    loops: the Lam ones call themselves once for each item.
# 標準のモジュールの定義のうち、ランタイムがループで書くもの。Lam の
# ものは、要素1つごとに自分を1回呼ぶ
def fold_right(f, z, xs):
    acc = z
    for x in reversed(to_list(xs)):
        acc = f(x)(acc)
    return acc


def take(k, xs):
    out = []
    while xs is not None and len(out) < k:
        out.append(xs.h)
        xs = xs.t
    return from_list(out)


def merge(before, xs, ys):
    out = []
    while xs is not None and ys is not None:
        if before(ys.h)(xs.h):
            out.append(ys.h)
            ys = ys.t
        else:
            out.append(xs.h)
            xs = xs.t
    return append(from_list(out), ys if xs is None else xs)


def zip_with(f, xs, ys):
    out = []
    while xs is not None and ys is not None:
        out.append(f(xs.h)(ys.h))
        xs = xs.t
        ys = ys.t
    return from_list(out)


def take_while(p, xs):
    out = []
    while xs is not None and p(xs.h):
        out.append(xs.h)
        xs = xs.t
    return from_list(out)


# Lam's groupBy compares with the first item of the group, not with the
# one next to it (takeWhile (same x) rest).
# Lam の groupBy は、となりの要素ではなく、グループの最初の要素と比べる
def group_by(same, xs):
    groups = []
    for x in to_list(xs):
        if groups and same(groups[-1][0])(x):
            groups[-1].append(x)
        else:
            groups.append([x])
    return from_list([from_list(g) for g in groups])


PRIMS.update({
    "List.foldr": (3, fold_right),
    "List.take": (2, take),
    "List.merge": (3, merge),
    "Prelude.range": (2, lambda a, b: from_list(range(a, b + 1))),
    "Prelude.replicate": (2, lambda k, x: from_list([x] * max(0, k))),
    "Prelude.zipWith": (3, zip_with),
    "Prelude.takeWhile": (2, take_while),
    "Prelude.groupBy": (2, group_by),
})
# ANCHOR_END: natives


# ANCHOR: curry
# 🍛 A function of n arguments, curried: f(a)(b)(c). As a decorator,
#    @curry(3) over a def of three arguments.
# n 個の引数の関数を、カリー化する：f(a)(b)(c)。デコレーターとしては、
# 3つの引数の def の上に @curry(3)
def curry(n, f=None):
    if f is None:
        return lambda g: curry(n, g)
    if n == 1:
        return f
    if n == 2:
        return lambda a: lambda b: f(a, b)
    if n == 3:
        return lambda a: lambda b: lambda c: f(a, b, c)

    def taking(got):
        if len(got) + 1 == n:
            return lambda x: f(*got, x)
        return lambda x: taking(got + (x,))
    return taking(())


# A builtin, curried: arity 0 is the value itself.
# カリー化した組み込み。引数が 0 なら、値そのもの
def prim(name):
    arity, f = PRIMS[name]
    return f() if arity == 0 else curry(arity, f)
# ANCHOR_END: curry


# ANCHOR: run
# 🧵 A job: cancelled, when it (or a job above it) is cancelled.
# 仕事：自分か、上の仕事が取り消されたら、取り消し
class Job:
    def __init__(self, parent=None):
        self.cancelled = False
        self.parent = parent


def cancelled(job):
    while job is not None:
        if job.cancelled:
            return True
        job = job.parent
    return False


def message_of(e):
    if isinstance(e, ZeroDivisionError):
        return "divided by zero"
    return str(e)


def make_runtime(world=None):
    world = world or real_world()
    rt = types.SimpleNamespace()

    # 🏃 The runner: do an action, and give how it ended:
    #      {"value": v}        it gave a value
    #      {"failed": why}     it stopped with an error
    #      {"exited": code}    exit code
    #    Bind is done with a list as a stack, not by calling run again:
    #    a long do (or a loop of a million binds) is not a deep call.
    # 実行役：頼みごとをやり、終わり方を返す。Bind は run を呼び直さず、
    # リストをスタックにして回す。長い do でも、深い呼び出しにならない
    async def run(action, job):
        rests = []
        current = action
        while True:
            if cancelled(job):
                return {"failed": "cancelled"}
            # pure and bind need no waiting: only the others go to step.
            # pure と bind は待たなくてよい。ほかだけを step へ
            if current.kind == "bind":
                rests.append(current.rest)
                current = current.first
                continue
            if current.kind == "pure":
                done = {"value": current.value}
            else:
                try:
                    done = await step(current, job)
                except Exception as e:
                    return {"failed": message_of(e)}
            if "next" in done:
                current = done["next"]
                continue
            if "value" not in done or not rests:
                return done
            try:
                current = rests.pop()(done["value"])
            except Exception as e:
                return {"failed": message_of(e)}

    # 👣 One step: a value, the next action, or how it ended.
    # 1歩：値か、次の頼みごとか、終わり方
    async def step(a, job):
        kind = a.kind
        if kind == "ask":
            value = await world(a.request, job, rt)
            # Let the other jobs go on, as an await of JavaScript does.
            # ほかの仕事に番をゆずる。JavaScript の await と同じ
            await asyncio.sleep(0)
            return {"value": value}
        if kind == "exit":
            return {"exited": a.code}
        # The cleanup comes after the body, whatever the body did.
        # 後始末は、本体が何をしても、本体のあとに
        if kind == "finally":
            body = await run(a.body, job)
            after = await run(a.cleanup, Job())
            return body if "value" in after else after
        # IO.try: an error becomes an Err; exit goes on out.
        # IO.try：エラーは Err に。exit はそのまま外へ
        if kind == "catch":
            body = await run(a.body, job)
            if "value" in body:
                return {"value": ok(body["value"])}
            if "failed" in body:
                return {"value": err(body["failed"])}
            return body
        if kind == "gather":
            return await gather_all(a, job)
        if kind == "race":
            return await race_all(a.actions, job)
        if kind == "spawn":
            return {"value": Task(asyncio.ensure_future(run(a.work, Job())))}
        if kind == "await":
            return await a.task.future
        return {"failed": "not an IO action: " + show(a)}
    # ANCHOR_END: run

    # ANCHOR: together
    # 🧵 IO.all and its friends: at most `most` at once (0: no limit).
    #    The first stop (or, for IO.allOk, the first Err) cancels the
    #    others, waits for their cleanups, and is the answer.
    # IO.all の仲間：同時に多くて most 個（0 は上限なし）。最初に止まった
    # もの（IO.allOk なら最初の Err）が、ほかを取り消し、その後始末を
    # 待って、答えになる
    async def gather_all(a, job):
        jobs = [Job(job) for _ in a.actions]
        answers = [None] * len(a.actions)
        state = {"first": None, "next": 0}

        def final(r):
            return "value" not in r or (a.until_err
                                        and r["value"].tag == "Err")

        async def worker():
            while state["next"] < len(a.actions) and state["first"] is None:
                i = state["next"]
                state["next"] += 1
                r = await run(a.actions[i], jobs[i])
                answers[i] = r
                if final(r) and state["first"] is None:
                    state["first"] = r
                    for j in jobs:
                        j.cancelled = True

        width = len(a.actions) if a.most == 0 else a.most
        await asyncio.gather(*(worker() for _ in range(max(1, width))))
        if cancelled(job):
            return {"failed": "cancelled"}
        first = state["first"]
        if first is not None:
            if a.until_err and "value" in first:
                return {"value": first["value"]}
            return first
        values = [r["value"] for r in answers]
        if a.until_err:
            return {"value": ok(from_list([r.args[0] for r in values]))}
        return {"value": from_list(values)}

    # IO.race: the first to end wins; the others are cancelled.
    # IO.race：最初に終わったものが勝ち。ほかは取り消す
    async def race_all(actions, job):
        if not actions:
            return {"failed": "IO.race: got no actions"}
        jobs = [Job(job) for _ in actions]
        runs = [asyncio.ensure_future(run(x, j))
                for x, j in zip(actions, jobs)]
        done, _ = await asyncio.wait(runs,
                                     return_when=asyncio.FIRST_COMPLETED)
        winner = min(i for i, r in enumerate(runs) if r in done)
        for i, j in enumerate(jobs):
            if i != winner:
                j.cancelled = True
        await asyncio.gather(*runs)
        if cancelled(job):
            return {"failed": "cancelled"}
        return runs[winner].result()
    # ANCHOR_END: together

    # ANCHOR: start
    # 🚪 Start the program: compute its definitions and main, run main,
    #    and end the process as lam would: an error is "error: ..." and
    #    exit code 1; main's Err is the same; exit n is n.
    # プログラムを始める：定義と main を計算し、main を動かし、lam と同じく
    # プロセスを終える。エラーは「error: ...」と終了コード 1。main の Err
    # も同じ。exit n は n
    def start(program):
        try:
            main = program()
        except Exception as e:
            finish({"failed": message_of(e)})
        finish(asyncio.run(run(main, Job())))

    def finish(done):
        if "exited" in done:
            sys.exit(done["exited"])
        if "failed" in done:
            write_all(2, "error: " + done["failed"] + "\n")
            sys.exit(1)
        v = done["value"]
        if type(v) is Con and v.tag == "Err":
            reason = v.args[0]
            said = reason if type(reason) is str else show(reason)
            write_all(2, "error: " + said + "\n")
            sys.exit(1)
        sys.exit(0)
    # ANCHOR_END: start

    rt.__dict__.update(
        prim=prim, curry=curry, list=lam_list, unit=unit, no_match=no_match,
        start=start, run=run, div=div, div_maybe=div_maybe,
        int_div_maybe=int_div_maybe, eq=eq, ne=ne, lt=lt, le=le, gt=gt,
        ge=ge, append=append, or_else=or_else, show=show, compare=compare,
        from_list=from_list, to_list=to_list, ok=ok, err=err, just=just,
        nothing=nothing, maybe=maybe, time=lam_time, cancelled=cancelled)
    return rt


# ANCHOR: world
# 🌍 The real world of Python: here, at last, the requests are done. rt
#    is the runtime, to make the answers with. A test can give
#    make_runtime a fake world instead (the same idea as lam's Handler).
# Python の本物の世界：ここで、やっと頼みごとをやる。rt は答えを作るため
# のランタイム。テストは、代わりに偽物の世界を make_runtime に渡せる
# （lam の Handler と同じ考え）
class Refused(Exception):
    pass


def real_world():
    stdin = LineReader(0)

    def done(path, act):
        try:
            return ok(act())
        except (OSError, Refused) as e:
            return err(path + ": " + why(e))

    async def world(r, job, rt):
        tag = r["tag"]
        if tag == "PutStr":
            write_all(1, r["text"])
            return unit
        if tag == "PutErr":
            write_all(2, r["text"])
            return unit
        if tag == "GetLine":
            return maybe(stdin.line())
        if tag == "GetContents":
            return stdin.rest()
        if tag == "ReadFile":
            return done(r["path"], lambda: text_of(r["path"]))
        if tag == "WriteFile":
            return done(r["path"], lambda: write_file(r["path"], r["text"],
                                                      "w"))
        if tag == "AppendFile":
            return done(r["path"], lambda: write_file(r["path"], r["text"],
                                                      "a"))
        if tag == "GetArgs":
            return from_list(sys.argv[1:])
        if tag == "LookupEnv":
            return maybe(os.environ.get(r["key"]))
        if tag == "GetEnvironment":
            return map_from_list(from_list(list(os.environ.items())))
        if tag == "ListDir":
            return done(r["path"],
                        lambda: from_list(sorted(os.listdir(r["path"]))))
        if tag == "Exists":
            return os.path.exists(r["path"])
        if tag == "IsDir":
            return os.path.isdir(r["path"])
        if tag == "MakeDir":
            return done(r["path"], lambda: os.makedirs(r["path"],
                                                       exist_ok=True) or ())
        if tag == "Remove":
            return done(r["path"], lambda: remove(r["path"]))
        if tag == "Rename":
            return done(r["source"],
                        lambda: os.rename(r["source"], r["target"]) or ())
        if tag == "CopyFile":
            return done(r["source"], lambda: shutil.copyfile(
                r["source"], r["target"]) and ())
        if tag == "Now":
            return lam_time(time.time_ns() // 1_000_000)
        if tag == "RandomInt":
            lo, hi = r["lo"], r["hi"]
            return lo if hi < lo else random.randint(lo, hi)
        if tag == "Sleep":
            await sleep(r["ms"], lambda: cancelled(job))
            return unit
        raise Exception("the real world cannot do " + tag)
    return world


# Bytes as text, strictly: a file that is not UTF-8 is an Err. Python's
# error says where (e.start), as lam does.
# バイト列を文字に。厳しく：UTF-8 でないファイルは Err。Python のエラーは、
# lam と同じく場所を言う（e.start）
def text_of(path):
    with open(path, "rb") as f:
        data = f.read()
    try:
        return data.decode("utf-8")
    except UnicodeDecodeError as e:
        raise Refused("not UTF-8 from byte " + str(e.start))


def write_file(path, text, mode):
    with open(path, mode, encoding="utf-8", newline="") as f:
        f.write(text)
    return ()


def remove(path):
    if os.path.isdir(path):
        os.rmdir(path)
    else:
        os.unlink(path)
    return ()


# The short words lam uses for a failure.
# lam が失敗に使う短い言葉
def why(e):
    if isinstance(e, Refused):
        return str(e)
    if isinstance(e, FileNotFoundError):
        return "no such file"
    if isinstance(e, PermissionError):
        return "permission denied"
    return e.strerror or str(e)
# ANCHOR_END: world


# ANCHOR: write
# 🖨️ Write to the screen at once, with os.write: sys.stdout keeps what
#    is written in a buffer when it is a pipe or a file, and then the
#    standard output and error would not come in the order written.
# os.write で、すぐに画面に書く。sys.stdout は、パイプやファイルのとき、
# 書いたものをバッファーにためるので、標準出力と標準エラーが書いた順に
# ならなくなる
def write_all(fd, text):
    data = text.encode("utf-8", "replace")
    while data:
        n = os.write(fd, data)
        data = data[n:]
# ANCHOR_END: write


# ⏳ Wait, but wake up early when the job is cancelled.
# 待つ。ただし、仕事が取り消されたら早めに起きる
async def sleep(ms, is_cancelled):
    end = time.monotonic() + ms / 1000
    while not is_cancelled():
        left = end - time.monotonic()
        if left <= 0:
            return
        await asyncio.sleep(min(left, 0.005))


# 📖 The standard input, a line at a time, read as it is needed.
# 標準入力。要るときに、1行ずつ読む
class LineReader:
    def __init__(self, fd):
        self.fd = fd
        self.decoder = codecs.getincrementaldecoder("utf-8")("replace")
        self.text = ""
        self.ended = False

    def more(self):
        try:
            got = os.read(self.fd, 65536)
        except BlockingIOError:
            time.sleep(0.001)
            return True
        except OSError:
            got = b""
        if not got:
            self.text += self.decoder.decode(b"", final=True)
            self.ended = True
            return False
        self.text += self.decoder.decode(got)
        return True

    def line(self):
        while True:
            at = self.text.find("\n")
            if at >= 0:
                line = self.text[:at]
                self.text = self.text[at + 1:]
                return line
            if self.ended or not self.more():
                if self.text == "":
                    return None
                last = self.text
                self.text = ""
                return last

    def rest(self):
        while not self.ended and self.more():
            pass
        everything = self.text
        self.text = ""
        return everything
# ANCHOR_END: all
