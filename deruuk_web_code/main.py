from fastapi import FastAPI, HTTPException
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
import subprocess
import os

app = FastAPI()

class CodeRequest(BaseModel):
    code: str

@app.get("/", response_class=HTMLResponse)
def read_root():
    with open("templates/index.html", "r") as f:
        return HTMLResponse(content=f.read())

@app.post("/execute")
async def execute_code(request: CodeRequest):
    try:
        with open("temp.lisp", "w") as f:
            f.write(request.code)
        
        result = subprocess.run(
            ["clisp", "temp.lisp"],
            capture_output=True,
            text=True,
            check=True
        )
        
        return {"output": result.stdout, "error": result.stderr}
    except subprocess.CalledProcessError as e:
        raise HTTPException(status_code=400, detail=f"Execution Error: {e.stderr}")
