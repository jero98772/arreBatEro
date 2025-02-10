# main.py
from fastapi import FastAPI, WebSocket, UploadFile, File
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse
import whisper
import numpy as np
import io
import base64
from pydub import AudioSegment
import tempfile

app = FastAPI()

# Load Whisper model
model = whisper.load_model("base")

# Mount static files
app.mount("/static", StaticFiles(directory="static"), name="static")

@app.get("/")
async def get():
    with open("static/index.html") as f:
        return HTMLResponse(content=f.read())

@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    await websocket.accept()
    
    try:
        while True:
            # Receive audio data
            audio_data = await websocket.receive_text()
            
            try:
                # Decode base64 audio data
                audio_bytes = base64.b64decode(audio_data.split(",")[1])
                
                # Save to temporary WAV file
                with tempfile.NamedTemporaryFile(suffix='.wav', delete=True) as temp_wav:
                    # Convert audio bytes to WAV using pydub
                    audio = AudioSegment.from_file(io.BytesIO(audio_bytes))
                    audio.export(temp_wav.name, format='wav')
                    
                    # Transcribe using the temporary file
                    result = model.transcribe(temp_wav.name)
                    
                    # Send transcription back
                    if result["text"].strip():  # Only send non-empty transcriptions
                        await websocket.send_text(result["text"])
                    
            except Exception as e:
                print(f"Processing error: {e}")
                continue  # Continue with next chunk even if this one fails
            
    except Exception as e:
        print(f"WebSocket error: {e}")
        await websocket.close()