from fastapi import FastAPI, WebSocket
from fastapi.middleware.cors import CORSMiddleware
import asyncio
from tools.tools import *
app = FastAPI()

chat_history = [AIMessage(content="Hello, I am a bot. How can I help you?")]

# CORS setup (adjust allowed origins as needed)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:5173"],  # React Vite default port
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# WebSocket connection manager
class ConnectionManager:
    def __init__(self):
        self.active_connections = []

    async def connect(self, websocket: WebSocket):
        await websocket.accept()
        self.active_connections.append(websocket)

    async def disconnect(self, websocket: WebSocket):
        self.active_connections.remove(websocket)

    async def broadcast(self, message: str):
        for connection in self.active_connections:
            await connection.send_text(message)

manager = ConnectionManager()

@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    await websocket.accept()

    # Send initial bot message
    await websocket.send_json({"role": "ai", "content": chat_history[0].content})

    try:
        while True:
            user_query = await websocket.receive_text()
            chat_history.append(HumanMessage(content=user_query))

            # Notify frontend of new user message
            await websocket.send_json({"role": "user", "content": user_query})

            # Stream LLM response
            response_text = ""
            async for chunk in get_response(user_query):
                response_text += chunk
                await websocket.send_json({"role": "ai_stream", "content": chunk})

            # Send final response and add to chat history
            chat_history.append(AIMessage(content=response_text))
            await websocket.send_json({"role": "ai_final", "content": response_text})

    except:
        pass  # Handle disconnections