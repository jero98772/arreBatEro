from fastapi import FastAPI, WebSocket
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
from starlette.websockets import WebSocketDisconnect
from tools import *
app = FastAPI()

# Mounting static files for frontend (optional)
app.mount("/static", StaticFiles(directory="static"), name="static")

# Serve the HTML file with HTTP GET
@app.get("/")
async def read_root():
    return HTMLResponse(content=open("static/index.html", "r").read())

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


graph = {
    0: [1, 2],
    1: [2],
    2: [0],
    3: [0]
}
# Global variables
pages = {
    "x": 0,
    "s.net": 1,
    "sd.com": 2,
    "lin.com": 3

}

# Simulated page content
page_content = {
    0: "Welcome to x. This is a search engine.",
    1: "S.net is a social network for developers.",
    2: "SD.com offers software development services.",
    3: "En álgebra lineal, a menudo es importante saber qué vectores mantienen sus direcciones sin cambios mediante una transformación lineal."

}


inverted_index = create_index(page_content)

pr = pagerank.PageRank(graph)
pr.calculatePageRank()
ranks = pr.getRanks()

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
