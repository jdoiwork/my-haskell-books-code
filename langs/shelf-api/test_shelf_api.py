# The shelf's API in FastAPI, for comparing with servant.
# 本棚の API を FastAPI で。servant とくらべるため
from fastapi.testclient import TestClient

# ANCHOR: fastapi
# Python (FastAPI: the type hints are the gate)
# Python（FastAPI：型ヒントが、そのまま門番になる）
from typing import Literal

from fastapi import FastAPI, Request
from fastapi.exceptions import RequestValidationError
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field


class Points(BaseModel):
    # 0..100, or 422. strict: "70" and true are not numbers here
    # 0〜100。違えば 422。strict："70" や true を数に直さない
    points: int = Field(strict=True, ge=0, le=100)


class Happened(BaseModel):
    event: Literal["Start", "Finish", "Stop", "Resume", "Reread"]


class Problem(BaseModel):
    mistake: str
    messages: list[str]


app = FastAPI()
ratings: dict[int, int] = {1: 90}


@app.exception_handler(RequestValidationError)
async def as_problem(_: Request, error: RequestValidationError):
    # What came in and could not be read, in one shape (a route that
    # does not exist still answers FastAPI's own 404).
    # 読めなかった入力の断りを、同じ形で（ない道の 404 は、
    # FastAPI の既定のまま）
    problem = Problem(mistake="bad-body",
                      messages=[e["msg"] for e in error.errors()])
    return JSONResponse(problem.model_dump(), status_code=422)


@app.put("/works/{work_id}/rating", status_code=204)
def rate(work_id: int, given: Points) -> None:
    ratings[work_id] = given.points
# ANCHOR_END: fastapi


# ANCHOR: event
# Python (an event, read through Literal)
# Python（できごとを、Literal で読む）
@app.post("/works/{work_id}/reading")
def happen(work_id: int, happened: Happened) -> dict[str, str]:
    return {"reading": "Reading" if happened.event == "Start" else "?"}
# ANCHOR_END: event


client = TestClient(app)


def test_points_are_checked_by_the_type():
    assert client.put("/works/1/rating", json={"points": 70}).status_code == 204
    assert ratings[1] == 70
    res = client.put("/works/1/rating", json={"points": 101})
    assert res.status_code == 422
    assert res.json()["mistake"] == "bad-body"
    assert ratings[1] == 70
    for not_a_number in ["70", True]:
        res = client.put("/works/1/rating", json={"points": not_a_number})
        assert res.status_code == 422
    assert ratings[1] == 70


def test_unknown_words_never_reach_the_function():
    res = client.post("/works/1/reading", json={"event": "Jump"})
    assert res.status_code == 422
    ok = client.post("/works/1/reading", json={"event": "Start"})
    assert ok.json() == {"reading": "Reading"}


def test_the_openapi_document_knows_the_bounds():
    schema = client.get("/openapi.json").json()
    points = schema["components"]["schemas"]["Points"]["properties"]["points"]
    assert (points["minimum"], points["maximum"]) == (0, 100)
