# A tiny key-value server with asyncio, for the chapter
# "Making it a TCP server". One thread runs every command in turn,
# so the dict needs no lock.
# 「TCP サーバーにする」の章の、asyncio の小さなキーと値のサーバー。
# 1つのスレッドがコマンドを順番に動かすので、dict に錠は要らない
import asyncio

store = {}


# ANCHOR: procedural
# Python (asyncio: one client per task, one thread for all)
# Python（asyncio：クライアントごとにタスク、スレッドは全部で1つ）
async def serve_client(reader, writer):
    while line := await reader.readline():
        words = line.decode().split()
        if words[:1] == ["SET"] and len(words) == 3:
            store[words[1]] = words[2]
            writer.write(b"+OK\r\n")
        elif words[:1] == ["INCR"] and len(words) == 2:
            store[words[1]] = str(int(store.get(words[1], "0")) + 1)
            writer.write(f":{store[words[1]]}\r\n".encode())
        else:
            writer.write(b"-ERR unknown command\r\n")
        await writer.drain()
    writer.close()
# ANCHOR_END: procedural


async def client(port, lines):
    reader, writer = await asyncio.open_connection("127.0.0.1", port)
    replies = []
    for line in lines:
        writer.write(line.encode() + b"\r\n")
        replies.append((await reader.readline()).decode().strip())
    writer.close()
    return replies


async def main():
    server = await asyncio.start_server(serve_client, "127.0.0.1", 0)
    port = server.sockets[0].getsockname()[1]
    assert await client(port, ["SET hero Lambda", "FLY"]) == \
        ["+OK", "-ERR unknown command"]
    await asyncio.gather(*[client(port, ["INCR visits"] * 200)
                           for _ in range(10)])
    assert store["visits"] == "2000"
    server.close()
    await server.wait_closed()
    print("ok")

asyncio.run(main())
