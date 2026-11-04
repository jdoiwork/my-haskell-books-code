# The Python code in the security chapter.
# セキュリティの章の Python のコード
from typing import NewType

import pytest
from pydantic import BaseModel, Field, SecretStr, ValidationError

# ANCHOR: newtype
# Python (NewType: a different name for the type checker only)
# Python（NewType：型チェッカーにだけ見える、別の名前）
Username = NewType("Username", str)


def greet(name: Username) -> str:
    return f"Welcome, {name}!"
# ANCHOR_END: newtype

# ANCHOR: pydantic
# Python (pydantic: check at the door, and keep the secret secret)
# Python（pydantic：門で確かめて、秘密は秘密のまま）
class Login(BaseModel):
    user: str = Field(pattern=r"^[a-z0-9_]{3,16}$")
    password: SecretStr
# ANCHOR_END: pydantic


def test_newtype_is_only_for_the_checker():
    # At run time, NewType does nothing: any str gets through.
    # 実行時の NewType は何もしない：どんな str も通る
    assert greet(Username("x' OR '1'='1")) == "Welcome, x' OR '1'='1!"


def test_pydantic_login():
    login = Login(user="lambda", password="hunter2")
    assert "hunter2" not in repr(login)
    assert "hunter2" not in login.model_dump_json()
    assert login.password.get_secret_value() == "hunter2"
    with pytest.raises(ValidationError):
        Login(user="x' OR '1'='1", password="x")
