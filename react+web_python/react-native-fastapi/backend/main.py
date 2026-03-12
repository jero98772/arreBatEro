from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List
from datetime import datetime

app = FastAPI()

# Configure CORS to allow React Native app to connect
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # In production, specify your frontend URL
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# In-memory storage for demo purposes
items_db = []

class Item(BaseModel):
    id: int = None
    title: str
    description: str
    created_at: str = None

@app.get("/")
def read_root():
    return {"message": "Welcome to FastAPI backend!"}

@app.get("/api/items", response_model=List[Item])
def get_items():
    """Get all items"""
    return items_db

@app.post("/api/items", response_model=Item)
def create_item(item: Item):
    """Create a new item"""
    new_item = item.dict()
    new_item["id"] = len(items_db) + 1
    new_item["created_at"] = datetime.now().isoformat()
    items_db.append(new_item)
    return new_item

@app.delete("/api/items/{item_id}")
def delete_item(item_id: int):
    """Delete an item by ID"""
    global items_db
    items_db = [item for item in items_db if item["id"] != item_id]
    print(items_db)
    return {"message": "Item deleted successfully"}

@app.get("/api/health")
def health_check():
    """Health check endpoint"""
    return {"status": "healthy", "timestamp": datetime.now().isoformat()}