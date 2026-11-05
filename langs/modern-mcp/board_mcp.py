# A tiny quest board as an MCP server ("Growing an MCP server").
# Only the standard library; a chat's lines are fed by hand at the end.
# MCP サーバーになった小さな依頼板（「MCP サーバーを育てる」の章）。
# 標準ライブラリだけを使い、最後にチャットの行を手で流しこむ
import json

board = []


# ANCHOR: procedural
# Python (procedural style: one function looks at the tool's name)
# Python（手続き型スタイル：1つの関数が、道具の名前を見て分ける）
def call_tool(name, args):
    if name == "post_quest":
        quest = {"id": len(board) + 1, "title": args["title"],
                 "reward": args["reward"], "taken_by": None}
        board.append(quest)
        return False, f"#{quest['id']} posted"
    elif name == "take_quest":
        quest = board[args["id"] - 1]
        if quest["taken_by"] is not None:
            return True, f"quest #{args['id']} is not open"
        quest["taken_by"] = args["who"]
        return False, f"#{args['id']} taken by {args['who']}"
    else:
        return True, f"no such tool: {name}"
# ANCHOR_END: procedural


def answer(line, call):
    request = json.loads(line)
    if "id" not in request:
        return None
    params = request.get("params", {})
    is_error, text = call(params["name"], params.get("arguments", {}))
    return {"jsonrpc": "2.0", "id": request["id"],
            "result": {"content": [{"type": "text", "text": text}],
                       "isError": is_error}}


chat = [
    '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":'
    '{"name":"post_quest",'
    '"arguments":{"title":"Slay the slimes","reward":30}}}',
    '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":'
    '{"name":"take_quest","arguments":{"id":1,"who":"Lambda"}}}',
    '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":'
    '{"name":"take_quest","arguments":{"id":1,"who":"Python"}}}',
]
replies = [answer(line, call_tool) for line in chat]
assert replies[1]["result"]["isError"] is False
assert replies[2]["result"]["content"][0]["text"] == "quest #1 is not open"
assert replies[2]["result"]["isError"] is True


# ANCHOR: functional
# Python (functional style: tools are values in a dictionary,
#   registered with a decorator, like the official SDK's @mcp.tool())
# Python（関数型スタイル：道具は辞書の中の値。公式の SDK の
#   @mcp.tool() と同じく、デコレーターで登録する）
tools = {}


def tool(f):
    tools[f.__name__] = f
    return f


@tool
def post_quest(title, reward):
    board.append({"id": len(board) + 1, "title": title,
                  "reward": reward, "taken_by": None})
    return False, f"#{len(board)} posted"


@tool
def list_quests():
    return False, "\n".join(f"#{q['id']} {q['title']}" for q in board)


def call_registered(name, args):
    if name not in tools:
        return True, f"no such tool: {name}"
    return tools[name](**args)
# ANCHOR_END: functional


board.clear()
replies = [answer(line, call_registered) for line in chat[:1]]
assert replies[0]["result"]["content"][0]["text"] == "#1 posted"
assert call_registered("list_quests", {}) == (False, "#1 Slay the slimes")
assert call_registered("take_quest", {})[0] is True
print("ok")
