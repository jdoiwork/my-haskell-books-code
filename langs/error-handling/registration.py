# The Python code in the error handling chapter.
# エラー処理の章の Python のコード
# ANCHOR: custom_exception_classes
# Python (custom exception classes)
# Python（自分で作る例外クラス）
class RegistrationError(Exception): ...
class EmptyName(RegistrationError): ...
class TooYoung(RegistrationError):
    def __init__(self, age):
        self.age = age
# ANCHOR_END: custom_exception_classes
assert issubclass(TooYoung, RegistrationError) and TooYoung(13).age == 13
from dataclasses import dataclass

@dataclass
class Adventurer:
    name: str
    age: int
    job: str

def validate_name(name):
    if name == "":
        raise EmptyName()
    return name

def validate_age(age):
    a = int(age)
    if a < 15:
        raise TooYoung(a)
    return a

def validate_job(job):
    return job
# ANCHOR: exceptions
# Python (exceptions)
# Python（例外を使う）
def register_parts(name, age, job):
    n = validate_name(name)   # raises RegistrationError
    a = validate_age(age)
    j = validate_job(job)
    return Adventurer(n, a, j)

try:
    adventurer = register_parts("Rust", "13", "Warrior")
except RegistrationError as e:
    print(e)
# ANCHOR_END: exceptions
assert register_parts("Rust", "16", "Warrior") == Adventurer("Rust", 16, "Warrior")
