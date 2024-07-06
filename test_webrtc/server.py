import asyncio
import websockets
import json

clients = []

async def handler(websocket, path):
    clients.append(websocket)
    try:
        async for message in websocket:
            data = json.loads(message)
            await broadcast(data, websocket)
    except websockets.ConnectionClosed:
        print("Client disconnected")
    finally:
        clients.remove(websocket)

async def broadcast(message, sender):
    for client in clients:
        if client != sender:
            await client.send(json.dumps(message))

start_server = websockets.serve(handler, 'localhost', 8080)

asyncio.get_event_loop().run_until_complete(start_server)
print("Signaling server started on ws://localhost:8080")
asyncio.get_event_loop().run_forever()
