# Read the table of contents of an EPUB with Python's standard library
# (zipfile and ElementTree): the same steps as the Haskell version.
# Python の標準ライブラリ（zipfile と ElementTree）で、EPUB の目次を読む。
# Haskell 版と同じ手順
#
# books/dragon.epub is the sample book that `shelf samples` writes
# (code/tsundoku/06-import).
# books/dragon.epub は、shelf samples が書き出す見本の本
import posixpath
import zipfile
import xml.etree.ElementTree as ET
from pathlib import Path

BOOK = Path(__file__).parent / "books" / "dragon.epub"


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def walk(ol, depth, toc):
    for li in ol:
        for child in li:
            if child.tag.endswith("a"):
                toc.append((depth, child.text.strip(), child.get("href")))
            elif child.tag.endswith("ol"):
                walk(child, depth + 1, toc)


def read_toc(path):
    toc = []                                     # (depth, heading, file)
    with zipfile.ZipFile(path) as z:
        # 1. container.xml says where the package file is
        #    container.xml が、パッケージのファイルの場所を教える
        container = ET.fromstring(z.read("META-INF/container.xml"))
        opf_path = None
        for el in container.iter():
            if el.tag.endswith("rootfile"):
                opf_path = el.get("full-path")
        # 2. the package file says which item is the nav
        #    パッケージのファイルが、どの item が nav かを教える
        opf = ET.fromstring(z.read(opf_path))
        nav_path = None
        folder = posixpath.dirname(opf_path)
        for el in opf.iter():
            props = el.get("properties") or ""
            if el.tag.endswith("item") and "nav" in props.split():
                nav_path = posixpath.join(folder, el.get("href"))
        # 3. walk the lists in the nav: an <ol> in an <li> is one level
        #    deeper
        #    nav のリストをたどる。<li> の中の <ol> は、1段深い
        nav = ET.fromstring(z.read(nav_path))
        for el in nav.iter():
            if el.tag.endswith("nav"):
                for ol in el:
                    if ol.tag.endswith("ol"):
                        walk(ol, 0, toc)
    return toc
# ANCHOR_END: procedural


toc = read_toc(BOOK)
assert [(d, h) for d, h, _ in toc] == [
    (0, "第1部 ドラゴンを知る"),
    (1, "第1章 見分け方"),
    (2, "1.1 うろこの色"),
    (2, "1.2 火を吐くもの"),
    (1, "第2章 すみかと季節"),
    (0, "第2部 ドラゴンと話す"),
    (1, "第3章 あいさつの作法"),
    (1, "第4章 怒らせたときは"),
], toc
assert toc[0][2] == "part1.xhtml"
print("epub_toc.py: ok")
