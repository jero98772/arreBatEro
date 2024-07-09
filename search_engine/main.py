from fastapi import FastAPI, WebSocket
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
from starlette.websockets import WebSocketDisconnect
from fastapi import FastAPI, Request
from fastapi.templating import Jinja2Templates

from tools.tools import *
from tools import pagerank 

app = FastAPI()

# Mounting static files for frontend (optional)
app.mount("/static", StaticFiles(directory="static"), name="static")
templates = Jinja2Templates(directory="templates")

file_path = 'data/data.json'
data = read_data_from_json(file_path)

graph = data.get('graph', {})
pages = data.get('pages', {})
page_content = data.get('page_content', {})

inverted_index = create_index(page_content)
print(inverted_index)
pr = pagerank.PageRank(graph)
pr.calculatePageRank()
ranks = pr.getRanks()

@app.get("/")
async def read_root():
    return HTMLResponse(content=open("templates/index.html", "r").read())

# WebSocket example
class ConnectionManager:
    def __init__(self):
        self.active_connections = []

    async def connect(self, websocket: WebSocket):
        await websocket.accept()
        self.active_connections.append(websocket)

    def disconnect(self, websocket: WebSocket):
        self.active_connections.remove(websocket)

    async def send_message(self, message: str):
        for connection in self.active_connections:
            await connection.send_text(message)

manager = ConnectionManager()

@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    await manager.connect(websocket)
    try:
        while True:
            query = await websocket.receive_text()
            print(f"Received message: {query}")  # Print the received message
            result = search(query, ranks,inverted_index,pages)

            await manager.send_message(f"Message text was: {result}")
    except WebSocketDisconnect:
        manager.disconnect(websocket)
        await manager.send_message(f"Disconnected")

if __name__ == "__main__":
    uvicorn.run(app, host="localhost", port=8000)
