from fastapi import FastAPI, Request, Form
from fastapi.responses import HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates

app = FastAPI()

# Mount static files
app.mount("/static", StaticFiles(directory="static"), name="static")

# Setup templates
templates = Jinja2Templates(directory="templates")

# In-memory data store
items = []

@app.get("/", response_class=HTMLResponse)
async def index(request: Request):
    return templates.TemplateResponse("index.html", {
        "request": request,
        "items": items
    })

@app.post("/add")
async def add_item(request: Request, item: str = Form(...)):
    if item:
        items.append(item)
    return templates.TemplateResponse("index.html", {
        "request": request,
        "items": items
    })

@app.post("/delete/{index}")
async def delete_item(request: Request, index: int):
    if 0 <= index < len(items):
        items.pop(index)
    return templates.TemplateResponse("index.html", {
        "request": request,
        "items": items
    })

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)