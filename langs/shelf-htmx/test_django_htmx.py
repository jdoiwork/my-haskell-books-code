# The stars of the shelf with Django and htmx, for comparing with lucid
# and htmx. One file: the settings, the view, the template, and tests
# with Django's test client (no server, no browser).
# 本棚の★を Django と htmx で。lucid と htmx とくらべるため。ファイル1つに、
# 設定、ビュー、テンプレート、Django のテストクライアントでのテスト
# （サーバーもブラウザーも使わない）
import django
from django.conf import settings

settings.configure(
    DEBUG=False,
    SECRET_KEY="only-for-this-test",
    ROOT_URLCONF=__name__,
    ALLOWED_HOSTS=["testserver"],
    MIDDLEWARE=["django.middleware.csrf.CsrfViewMiddleware"],
    TEMPLATES=[{"BACKEND": "django.template.backends.django.DjangoTemplates"}],
)
django.setup()

from django.http import HttpResponse  # noqa: E402
from django.template import engines  # noqa: E402
from django.test import Client  # noqa: E402
from django.urls import path  # noqa: E402

RATINGS = {1: 90}

# ANCHOR: template
# Python (Django: the part of the stars, as a template)
# Python（Django：★の部品を、テンプレートで）
STARS = engines["django"].from_string("""
<form id="rating" hx-put="/works/{{ wid }}/rating" hx-swap="outerHTML">
  {% for points, label in marks %}
    <button name="points" value="{{ points }}"
            {% if points == now %}class="now"{% endif %}>{{ label }}</button>
  {% endfor %}
</form>
""")
# ANCHOR_END: template


# ANCHOR: view
# Python (Django: the view answers only the part)
# Python（Django：ビューは部品だけを答える）
def rating(request, wid):
    if request.method == "PUT":
        typed = request.body.decode().removeprefix("points=")
        # digits only, as readPoints: int() also reads "+70", "7_0", "٧٠"
        # readPoints と同じく数字だけ。int() は "+70"・"7_0"・"٧٠" も読む
        if not (typed.isascii() and typed.isdigit() and len(typed) <= 3):
            return HttpResponse("点として読めません", status=422)
        points = int(typed)
        if not 0 <= points <= 100:
            return HttpResponse("点は 0〜100 です", status=422)
        RATINGS[wid] = points
    now = (RATINGS[wid] + 5) // 10 * 10
    marks = [(p, f"★{p / 20:g}") for p in range(0, 101, 10)]
    context = {"wid": wid, "now": now, "marks": marks}
    return HttpResponse(STARS.render(context, request))
# ANCHOR_END: view


urlpatterns = [path("works/<int:wid>/rating", rating)]


def test_answers_only_the_stars():
    client = Client()
    res = client.put("/works/1/rating", "points=70",
                     content_type="application/x-www-form-urlencoded",
                     headers={"HX-Request": "true"})
    assert res.status_code == 200
    html = res.content.decode()
    assert 'class="now">★3.5<' in html
    assert html.strip().startswith('<form id="rating"')
    assert RATINGS[1] == 70


def test_refuses_101_points():
    res = Client().put("/works/1/rating", "points=101",
                       content_type="application/x-www-form-urlencoded")
    assert res.status_code == 422


def test_refuses_what_is_not_digits():
    for typed in ["abc", "", "+70", "7_0", "٧٠", " 70"]:
        res = Client().put("/works/1/rating", f"points={typed}",
                           content_type="application/x-www-form-urlencoded")
        assert res.status_code == 422, typed


# ANCHOR: csrf
# Python (Django checks a CSRF token on every change; htmx sends it in a
# header: <body hx-headers='{"X-CSRFToken": "{{ csrf_token }}"}'>)
# Python（Django は、変更のたびに CSRF のトークンを確かめる。htmx は、
# それをヘッダーで送る：<body hx-headers='{"X-CSRFToken": "…"}'>）
def test_needs_the_csrf_token():
    client = Client(enforce_csrf_checks=True)
    form = {"content_type": "application/x-www-form-urlencoded"}
    without = client.put("/works/1/rating", "points=80", **form)
    assert without.status_code == 403
    token = "t" * 32  # what {{ csrf_token }} would put in the page
    client.cookies["csrftoken"] = token
    res = client.put("/works/1/rating", "points=80", **form,
                     headers={"X-CSRFToken": token})
    assert res.status_code == 200
# ANCHOR_END: csrf


def test_escapes_what_it_shows():
    # Django's templates escape, as lucid does
    # Django のテンプレートも、lucid と同じくエスケープする
    tpl = engines["django"].from_string("<p>{{ body }}</p>")
    assert tpl.render({"body": "<script>"}) == "<p>&lt;script&gt;</p>"
