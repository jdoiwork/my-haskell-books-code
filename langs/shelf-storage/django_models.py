# The shelf in SQLite with Django (models and the admin), for the chapter
# "The window and the records". Runs alone: the settings are below, and
# the tables go into SQLite in memory.
# 「窓口と記録」の章の、SQLite の本棚を Django（モデルと管理画面）で。
# 1つで動く：設定はこの下にあり、表はメモリの SQLite に作る
import django
from django.apps import AppConfig
from django.conf import settings


# The app of the shelf is this file itself.
# 本棚のアプリは、このファイルそのもの
class ShelfConfig(AppConfig):
    name = "__main__"
    label = "shelf"


settings.configure(
    DEBUG=False,
    SECRET_KEY="only-for-this-check",
    ALLOWED_HOSTS=["testserver"],
    ROOT_URLCONF="__main__",
    DATABASES={"default": {"ENGINE": "django.db.backends.sqlite3",
                           "NAME": ":memory:"}},
    INSTALLED_APPS=[
        "django.contrib.admin",
        "django.contrib.auth",
        "django.contrib.contenttypes",
        "django.contrib.sessions",
        "django.contrib.messages",
        "__main__.ShelfConfig",
    ],
    MIDDLEWARE=[
        "django.contrib.sessions.middleware.SessionMiddleware",
        "django.contrib.auth.middleware.AuthenticationMiddleware",
        "django.contrib.messages.middleware.MessageMiddleware",
    ],
    TEMPLATES=[{
        "BACKEND": "django.template.backends.django.DjangoTemplates",
        "APP_DIRS": True,
        "OPTIONS": {"context_processors": [
            "django.template.context_processors.request",
            "django.contrib.auth.context_processors.auth",
            "django.contrib.messages.context_processors.messages",
        ]},
    }],
    DEFAULT_AUTO_FIELD="django.db.models.BigAutoField",
    USE_TZ=True,
)
django.setup()

from django.db import models  # noqa: E402

# ANCHOR: models
# Python (Django: declare the tables as classes)
# Python（Django：表をクラスで宣言する）

# 📕 A work on the shelf. Django gives the number (id).
# 本棚の作品。番号（id）は Django が付ける
class Work(models.Model):
    title = models.CharField(max_length=200)

    def __str__(self):
        return self.title


# 🌳 One heading of the table of contents; parent is None at the top.
# 目次の見出し1つ。いちばん上は parent が None
class Entry(models.Model):
    work = models.ForeignKey(Work, on_delete=models.CASCADE,
                             related_name="entries")
    parent = models.ForeignKey("self", on_delete=models.CASCADE,
                               null=True, blank=True,
                               related_name="children")
    position = models.PositiveIntegerField()
    heading = models.CharField(max_length=200)
    size = models.IntegerField()

    def __str__(self):
        return self.heading

    class Meta:
        ordering = ["position"]
        constraints = [models.CheckConstraint(
            condition=models.Q(size__gte=0), name="size_not_negative")]


# 📝 A note on a work.
# 作品の感想
class Note(models.Model):
    work = models.ForeignKey(Work, on_delete=models.CASCADE,
                             related_name="notes")
    body = models.TextField()
# ANCHOR_END: models

from django.contrib import admin  # noqa: E402
from django.urls import path  # noqa: E402

# ANCHOR: admin
# Python (Django: the admin pages, with the entries inside the work)
# Python（Django：管理画面。作品の中に見出しを並べる）

# 📋 The entries, as rows of a table in the page of a work.
# 見出しを、作品のページの中の表の行として
class EntryInline(admin.TabularInline):
    model = Entry
    fields = ["parent", "position", "heading", "size"]
    extra = 1


# 🛠️ The page of the works: a list, a search box, and the entries.
# 作品のページ：一覧、検索窓、見出し
@admin.register(Work)
class WorkAdmin(admin.ModelAdmin):
    list_display = ["id", "title"]
    search_fields = ["title"]
    inlines = [EntryInline]


urlpatterns = [path("admin/", admin.site.urls)]
# ANCHOR_END: admin

from django.contrib.auth.models import User  # noqa: E402
from django.core.management import call_command  # noqa: E402
from django.db import IntegrityError, connection, transaction  # noqa: E402
from django.test import Client  # noqa: E402

# Make the tables: Django's own by migrate, and the shelf's from the
# models (this file has no migration files).
# 表を作る：Django の表は migrate で、本棚の表はモデルから
# （このファイルにはマイグレーションのファイルがない）
call_command("migrate", verbosity=0)
with connection.schema_editor() as editor:
    for model in (Work, Entry, Note):
        editor.create_model(model)


# 📚 Add a work and its tree: all of it, or none of it.
# 作品と目次の木を足す：全部か、何もなしか
def add_work(title, toc):
    with transaction.atomic():
        work = Work.objects.create(title=title)
        add_entries(work, None, toc)
        return work


# 🌿 Add the children of one parent, and their children in turn.
# 1つの親の子を足し、その子の子も順に足す
def add_entries(work, parent, children):
    for position, (heading, size, sub) in enumerate(children):
        entry = work.entries.create(parent=parent, position=position,
                                    heading=heading, size=size)
        add_entries(work, entry, sub)


# 🌲 Walk the tree from the roots: (depth, heading), in order.
# 根から木を歩く：(深さ, 見出し) を順番どおりに
def toc(entries, depth=0):
    rows = []
    for e in entries:
        rows.append((depth, e.heading))
        rows += toc(e.children.all(), depth + 1)
    return rows


DRAGONS = [
    ("第1部 ドラゴンを知る", 2, [
        ("第1章 見分け方", 4, [
            ("1.1 うろこの色", 8, []),
            ("1.2 火を吐くもの", 10, []),
        ]),
        ("第2章 すみかと季節", 24, []),
    ]),
    ("第2部 ドラゴンと話す", 2, [
        ("第3章 あいさつの作法", 16, []),
        ("第4章 怒らせたときは", 12, []),
    ]),
]

# SQLite's foreign keys: Django turns them on for us.
# SQLite の外部キー：Django が入れておいてくれる
with connection.cursor() as c:
    c.execute("PRAGMA foreign_keys")
    assert c.fetchone() == (1,)

work = add_work("はじめてのドラゴン学", DRAGONS)
assert work.id == 1
rows = toc(work.entries.filter(parent=None))
assert [d for d, _ in rows] == [0, 1, 2, 2, 1, 0, 1, 1], rows
assert [h for _, h in rows][-1] == "第4章 怒らせたときは"
assert Entry.objects.count() == 8

# delete() removes its entries and notes. Django does the CASCADE itself
# (in Python), not the database.
# delete() で目次と感想も消える。CASCADE は DB でなく Django が
# （Python で）行う
work.notes.create(body="うろこの色の図がきれい")
work.delete()
assert Note.objects.count() == 0 and Entry.objects.count() == 0
with connection.cursor() as c:
    c.execute("SELECT sql FROM sqlite_master WHERE name = 'shelf_note'")
    assert "ON DELETE CASCADE" not in c.fetchone()[0]

# So a DELETE in plain SQL is stopped by the foreign key.
# だから、ふつうの SQL の DELETE は外部キーに止められる
again = add_work("はじめてのドラゴン学", DRAGONS)
# Django makes SQLite's ids AUTOINCREMENT: 1 is not used again.
# Django は SQLite の id を AUTOINCREMENT にする：1 は使い回さない
assert again.id == 2, again.id
try:
    with transaction.atomic(), connection.cursor() as c:
        c.execute("DELETE FROM shelf_work WHERE id = %s", [again.id])
    raise AssertionError("should fail")
except IntegrityError:
    pass
assert Work.objects.count() == 1

# A negative size in the third entry: CHECK fails, nothing is left.
# 3つ目の見出しの大きさが負：CHECK で止まり、何も残らない
bad = [("第1部", 2, [("第1章", 4, []), ("第2章", -1, [])])]
try:
    add_work("こわれた本", bad)
    raise AssertionError("should fail")
except IntegrityError as e:
    assert "CHECK" in str(e), e
assert Work.objects.count() == 1 and Entry.objects.count() == 8

# The admin: log in as a superuser and open the list and a work.
# 管理画面：管理者でログインして、一覧と作品のページを開く
assert admin.site.is_registered(Work)
boss = User.objects.create_superuser("boss", "boss@example.com", "pw")
client = Client()
client.force_login(boss)
page = client.get("/admin/shelf/work/")
assert page.status_code == 200, page.status_code
assert "はじめてのドラゴン学" in page.text
page = client.get(f"/admin/shelf/work/{again.id}/change/")
assert page.status_code == 200, page.status_code
assert "1.2 火を吐くもの" in page.text
