# The same, with the libraries everybody uses: ebooklib for EPUB and
# pypdf for PDF. books/ has the sample books that `shelf samples` writes
# (code/tsundoku/import).
# 同じことを、みんなが使うライブラリで：EPUB は ebooklib、PDF は pypdf。
# books/ には、shelf samples が書き出す見本の本がある
import warnings
from pathlib import Path

from ebooklib import epub
from pypdf import PdfReader

BOOKS = Path(__file__).parent / "books"
warnings.simplefilter("ignore")   # ebooklib's notes about future options


# ANCHOR: ebooklib
# Python (functional style, with ebooklib)
# Python（関数型スタイル。ebooklib で）
def epub_toc(path):
    book = epub.read_epub(str(path))

    def walk(items, depth):
        # a section with children comes as a pair (section, [children])
        # 子のある見出しは（見出し, [子]）の組で来る
        return [row
                for item in items
                for row in (
                    [(depth, item[0].title)] + walk(item[1], depth + 1)
                    if isinstance(item, tuple)
                    else [(depth, item.title)])]

    return book.get_metadata("DC", "title")[0][0], walk(book.toc, 0)
# ANCHOR_END: ebooklib


# ANCHOR: pypdf
# Python (functional style, with pypdf)
# Python（関数型スタイル。pypdf で）
def pdf_toc(path):
    reader = PdfReader(path)

    def walk(items, depth):
        # a list after a bookmark holds its children
        # しおりの後ろのリストが、その子
        return [row
                for item in items
                for row in (
                    walk(item, depth + 1) if isinstance(item, list)
                    else [(depth, item.title, page_of(item))])]

    def page_of(item):
        return reader.get_destination_page_number(item) + 1

    return len(reader.pages), walk(reader.outline, 0)
# ANCHOR_END: pypdf


title, toc = epub_toc(BOOKS / "dragon.epub")
assert title == "はじめてのドラゴン学"
assert toc == [
    (0, "第1部 ドラゴンを知る"),
    (1, "第1章 見分け方"),
    (2, "1.1 うろこの色"),
    (2, "1.2 火を吐くもの"),
    (1, "第2章 すみかと季節"),
    (0, "第2部 ドラゴンと話す"),
    (1, "第3章 あいさつの作法"),
    (1, "第4章 怒らせたときは"),
], toc

pages, marks = pdf_toc(BOOKS / "alice.pdf")
assert pages == 6
assert marks == [
    (0, "Alice's Adventures in Wonderland", 1),
    (1, "I. Down the Rabbit-Hole", 2),
    (1, "II. The Pool of Tears", 4),
    (1, "III. A Caucus-Race", 6),
], marks
# ANCHOR: broken
# Python: what ebooklib does with broken books
# Python：壊れた本を、ebooklib はどうするか
try:
    epub.read_epub(str(BOOKS / "hello.epub"))          # not a zip
except epub.EpubException as e:
    print("hello.epub:", e)                            # Bad Zip file
try:
    epub.read_epub(str(BOOKS / "nochap.epub"))         # ch2.xhtml is missing
except KeyError as e:
    print("nochap.epub:", e)                           # the whole book
print("notoc.epub:", epub.read_epub(str(BOOKS / "notoc.epub")).toc)   # []
# ANCHOR_END: broken


def raises(kind, path):
    try:
        epub.read_epub(str(path))
    except kind:
        return True
    return False


assert raises(epub.EpubException, BOOKS / "hello.epub")
assert raises(KeyError, BOOKS / "nochap.epub")
assert epub_toc(BOOKS / "notoc.epub")[1] == []
print("libraries.py: ok")
