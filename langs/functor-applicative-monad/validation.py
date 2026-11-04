# Collect every problem in a form, for the chapter
# "Functor, Applicative and Monad, deeper".
# フォームの問題を全部集める。「Functor・Applicative・Monad をもっと深く」の章

JOBS = ["warrior", "mage", "healer"]


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def apply_all(name, age, job):
    errors = []
    if name == "":
        errors.append("name is empty")
    elif len(name) > 12:
        errors.append("name is too long")
    if age < 12:
        errors.append("too young")
    if job not in JOBS:
        errors.append("unknown job")
    if errors:
        return errors
    return {"name": name, "age": age, "job": job}
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def apply_all_f(name, age, job):
    checks = [
        (name == "", "name is empty"),
        (len(name) > 12, "name is too long"),
        (age < 12, "too young"),
        (job not in JOBS, "unknown job"),
    ]
    errors = [message for failed, message in checks if failed]
    return errors or {"name": name, "age": age, "job": job}
# ANCHOR_END: functional


for apply in (apply_all, apply_all_f):
    assert apply("", 9, "thief") == ["name is empty", "too young",
                                      "unknown job"]
    assert apply("Lambda", 20, "mage") == {"name": "Lambda", "age": 20,
                                           "job": "mage"}
print("ok")
