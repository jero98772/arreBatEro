from fastapi import FastAPI, HTTPException
from fastapi.responses import FileResponse
from pydantic import BaseModel
import subprocess
import os

app = FastAPI()

class Command(BaseModel):
    command: str

class FileRequest(BaseModel):
    content: str

# Function to open a file as binary
@app.get("/open-file/")
def open_file(file_path: str):
    try:
        if os.path.exists(file_path):
            return FileResponse(path=file_path, media_type='application/octet-stream', filename=file_path)
        else:
            raise HTTPException(status_code=404, detail="File not found")
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Function to write to a file as binary
@app.post("/write-file/")
def write_file(file_path: str, file_request: FileRequest):
    try:
        with open(file_path, 'wb') as file:
            file.write(file_request.content.encode())  # Writing as binary
        return {"status": "success", "message": f"File '{file_path}' written successfully."}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

# Function to run a shell command and return the output
@app.post("/run-command/")
def run_command(cmd: Command):
    try:
        result = subprocess.run(cmd.command, shell=True, capture_output=True, text=True)
        return {"output": result.stdout, "error": result.stderr}
    except Exception as e:
        return {"error": str(e)}
