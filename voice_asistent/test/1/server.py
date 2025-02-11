# main.py
from fastapi import FastAPI, WebSocket
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
    audio_buffer = []
    buffer_duration = 0
    
    try:
        while True:
            # Receive audio data
            data = await websocket.receive_json()
            audio_data = data['audio']
            language = data.get('language', 'en')  # Default to English if not specified
            
            try:
                # Decode base64 audio data
                audio_bytes = base64.b64decode(audio_data.split(",")[1])
                
                # Save to temporary WebM file
                with tempfile.NamedTemporaryFile(suffix='.webm', delete=True) as temp_audio:
                    # Save original audio
                    temp_audio.write(audio_bytes)
                    temp_audio.flush()
                    
                    # Convert to wav using pydub
                    audio = AudioSegment.from_file(temp_audio.name, format="webm")
                    audio = audio.set_frame_rate(16000).set_channels(1)
                    
                    # Add to buffer
                    audio_buffer.append(audio)
                    buffer_duration += len(audio)
                    
                    # Process when buffer reaches 3 seconds
                    if buffer_duration >= 3000:  # 3000ms = 3 seconds
                        # Combine audio segments
                        combined_audio = sum(audio_buffer)
                        
                        with tempfile.NamedTemporaryFile(suffix='.wav', delete=True) as temp_wav:
                            combined_audio.export(temp_wav.name, format='wav', 
                                               parameters=["-ac", "1", "-ar", "16000"])
                            
                            # Transcribe using the temporary file
                            result = model.transcribe(
                                temp_wav.name,
                                fp16=False,
                                language=language,
                                task='transcribe'
                            )
                            
                            if result["text"].strip():
                                await websocket.send_text(result["text"])
                        
                        # Keep last segment for overlap
                        audio_buffer = [audio_buffer[-1]]
                        buffer_duration = len(audio_buffer[0])
                
            except Exception as e:
                print(f"Processing error: {e}")
                audio_buffer = []
                buffer_duration = 0
                continue
            
    except Exception as e:
        print(f"WebSocket error: {e}")
        await websocket.close()