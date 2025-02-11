from fastapi import FastAPI, Request
from fastapi.responses import FileResponse, HTMLResponse
from fastapi.staticfiles import StaticFiles
from fastapi.middleware.cors import CORSMiddleware

app = FastAPI()

# Serve static files (adjust the directory as needed)
app.mount("/static", StaticFiles(directory="static"), name="static")

# Enable CORS (allow all origins in this example)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.get("/", response_class=HTMLResponse)
async def get_index():
    return FileResponse("static/index.html")

@app.post("/transcript")
async def receive_transcript(request: Request):
    data = await request.json()
    print(data)
    text = data.get("text")
    print(f"Received transcript: {text}")
    return {"message": "Transcript received", "text": text}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
