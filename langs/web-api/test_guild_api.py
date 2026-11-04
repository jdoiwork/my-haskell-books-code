# The FastAPI code in the Web API chapter.
# Web API の章の FastAPI のコード
from fastapi.testclient import TestClient

# ANCHOR: fastapi
# Python (FastAPI: types on the functions become the API and its docs)
# Python（FastAPI：関数に付けた型が、API とドキュメントになる）
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel


class NewAdventurer(BaseModel):
    name: str
    job: str
    level: int


class Adventurer(NewAdventurer):
    adventurerId: int


app = FastAPI()
roster: dict[str, Adventurer] = {}


@app.get("/adventurers")
def list_adventurers() -> list[Adventurer]:
    return sorted(roster.values(), key=lambda a: a.adventurerId)


@app.get("/adventurers/{name}")
def find_adventurer(name: str) -> Adventurer:
    if name not in roster:
        raise HTTPException(404, "no such adventurer")
    return roster[name]


@app.post("/adventurers", status_code=201)
def add_adventurer(new: NewAdventurer) -> Adventurer:
    if new.name in roster:
        raise HTTPException(409, "the name is taken")
    added = Adventurer(adventurerId=len(roster) + 1, **new.model_dump())
    roster[new.name] = added
    return added
# ANCHOR_END: fastapi


client = TestClient(app)


def test_api():
    assert client.get("/adventurers").json() == []
    lambda_ = {"name": "Lambda", "job": "Mage", "level": 3}
    res = client.post("/adventurers", json=lambda_)
    assert res.status_code == 201
    assert res.json() == {**lambda_, "adventurerId": 1}
    assert client.post("/adventurers", json=lambda_).status_code == 409
    assert client.get("/adventurers/Nobody").status_code == 404
    assert client.get("/openapi.json").status_code == 200
