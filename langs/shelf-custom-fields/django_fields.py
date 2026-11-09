# My own attributes with Django (a JSONField, and a form built from the
# definitions), for the chapter "Attributes and states of my own".
# Runs alone: the settings are below, and the tables go into SQLite in
# memory.
# 「自分で決める属性と状態」の章の、自分で決めた属性を Django で
# （JSONField と、定義から組み立てるフォーム）。
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

from django import forms  # noqa: E402
from django.db import connection, models  # noqa: E402


# ANCHOR: model
# Python (Django: a JSONField keeps any attributes)
# Python（Django：JSONField に、どんな属性でも入れる）

# 📕 A work, with my own attributes in one JSON column.
# 作品。自分で決めた属性は、JSON の列1つに
class Work(models.Model):
    title = models.CharField(max_length=200)
    attrs = models.JSONField(default=dict)


# ANCHOR_END: model
with connection.schema_editor() as editor:
    editor.create_model(Work)

# ANCHOR: query
Work.objects.create(title="はじめてのドラゴン学",
                    attrs={"推し度": 4, "形": "紙"})
Work.objects.create(title="図書室の司書に転生した件",
                    attrs={"推し度": 5, "形": "Web"})
Work.objects.create(title="迷宮図書館の写本係",
                    attrs={"推し度": 3, "形": "Web"})

# 🔎 Keys of the JSON go into the lookup, as in a normal column.
# JSON のキーを、ふつうの列のように絞り込みに書ける
found = Work.objects.filter(attrs__推し度__gte=3, attrs__形="Web")
# ANCHOR_END: query
assert [w.title for w in found] == ["図書室の司書に転生した件",
                                    "迷宮図書館の写本係"]

# ANCHOR: loose
# 🙈 But nothing checks the shape: "すごく" goes in, and on SQLite,
#    text is bigger than any number, so it passes 推し度 >= 3.
# でも、形はだれも確かめない。"すごく" も入り、SQLite では文字が
# どんな数より大きいので、推し度 >= 3 を通ってしまう
Work.objects.create(title="すごい本", attrs={"推し度": "すごく"})
loose = Work.objects.filter(attrs__推し度__gte=3)
# ANCHOR_END: loose
assert "すごい本" in [w.title for w in loose]

# ANCHOR: form
# Python (Django: a form class built at run time from the definitions)
# Python（Django：定義から、動かしながらフォームのクラスを組み立てる）
DEFINITIONS = [("推し度", "number"), ("買った日", "date"),
               ("サイン本", "yesno"), ("形", ["紙", "電子", "Web"])]


# 🎛️ A form field for each kind.
# 種類ごとのフォームの欄
def field_for(kind):
    if kind == "number":
        return forms.IntegerField(required=False)
    if kind == "date":
        return forms.DateField(required=False)
    if kind == "yesno":
        return forms.BooleanField(required=False)
    return forms.ChoiceField(choices=[(c, c) for c in kind],
                             required=False)


# 🧱 type() makes a new class: a form with one field per definition.
# type() で新しいクラスを作る：定義ごとに欄が1つあるフォーム
AttrsForm = type("AttrsForm", (forms.Form,),
                 {name: field_for(kind) for name, kind in DEFINITIONS})

form = AttrsForm({"推し度": "すごく", "買った日": "11月1日", "形": "Web"})
# ANCHOR_END: form
assert not form.is_valid()
# Every bad field, not just the first: errors are kept per field.
# 最初の1つだけでなく、まずい欄の全部。エラーは欄ごとに残る
assert sorted(form.errors) == ["推し度", "買った日"], form.errors
assert form.cleaned_data == {"サイン本": False, "形": "Web"}
good = AttrsForm({"推し度": "4", "買った日": "2026-11-01", "形": "紙"})
assert good.is_valid() and good.cleaned_data["推し度"] == 4
# An HTML form, drawn from the definitions.
# 定義から描いた HTML のフォーム
assert 'type="number"' in str(good["推し度"])
assert "<select" in str(good["形"])
print("ok")
