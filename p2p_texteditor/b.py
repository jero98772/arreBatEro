from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
import json
import asyncio
from typing import Dict, List, Set
import uuid
from datetime import datetime
import os

app = FastAPI()

# Data models
class DocumentOperation:
    def __init__(self, op_type: str, position: int, content: str, user_id: str):
        self.id = str(uuid.uuid4())
        self.op_type = op_type  # 'insert', 'delete', 'replace'
        self.position = position
        self.content = content
        self.user_id = user_id
        self.timestamp = datetime.now().isoformat()

class Document:
    def __init__(self, doc_id: str):
        self.id = doc_id
        self.content = ""
        self.operations: List[DocumentOperation] = []
        self.connected_users: Set[str] = set()

class FileUpload(BaseModel):
    filename: str
    content: str
    doc_id: str

# Storage
documents: Dict[str, Document] = {}
active_connections: Dict[str, List[WebSocket]] = {}

# Helper functions
def apply_operation(doc: Document, operation: DocumentOperation) -> bool:
    """Apply operation to document and return success status"""
    try:
        if operation.op_type == "insert":
            doc.content = doc.content[:operation.position] + operation.content + doc.content[operation.position:]
        elif operation.op_type == "delete":
            end_pos = operation.position + len(operation.content)
            doc.content = doc.content[:operation.position] + doc.content[end_pos:]
        elif operation.op_type == "replace":
            # For replace, content format is "old_text|new_text"
            parts = operation.content.split("|", 1)
            if len(parts) == 2:
                old_text, new_text = parts
                doc.content = doc.content.replace(old_text, new_text, 1)
        
        doc.operations.append(operation)
        return True
    except Exception as e:
        print(f"Error applying operation: {e}")
        return False

async def broadcast_to_document(doc_id: str, message: dict, sender_ws: WebSocket = None):
    """Broadcast message to all users in a document except sender"""
    if doc_id in active_connections:
        disconnected = []
        for ws in active_connections[doc_id]:
            if ws != sender_ws:
                try:
                    await ws.send_text(json.dumps(message))
                except:
                    disconnected.append(ws)
        
        # Clean up disconnected connections
        for ws in disconnected:
            active_connections[doc_id].remove(ws)

# API Routes
@app.get("/")
async def get_editor():
    return HTMLResponse(content="""
<!DOCTYPE html>
<html>
<head>
    <title>P2P Text Editor</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .container { max-width: 1200px; margin: 0 auto; }
        .header { display: flex; justify-content: between; align-items: center; margin-bottom: 20px; }
        .controls { display: flex; gap: 10px; margin-bottom: 20px; align-items: center; }
        input, button { padding: 8px 12px; border: 1px solid #ddd; border-radius: 4px; }
        button { background: #007bff; color: white; cursor: pointer; }
        button:hover { background: #0056b3; }
        #editor { width: 100%; height: 400px; border: 1px solid #ddd; padding: 10px; 
                 font-family: monospace; font-size: 14px; resize: vertical; }
        .status { margin-top: 10px; padding: 10px; background: #f8f9fa; border-radius: 4px; }
        .users { display: flex; gap: 10px; flex-wrap: wrap; }
        .user { padding: 4px 8px; background: #e9ecef; border-radius: 12px; font-size: 12px; }
        .file-controls { display: flex; gap: 10px; align-items: center; margin-top: 10px; }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>P2P Text Editor</h1>
        </div>
        
        <div class="controls">
            <input type="text" id="docId" placeholder="Document ID" value="demo-doc">
            <button onclick="connectToDocument()">Connect</button>
            <button onclick="disconnect()">Disconnect</button>
            <span id="connectionStatus">Disconnected</span>
        </div>

        <div class="file-controls">
            <input type="file" id="fileInput" accept=".txt,.md,.js,.py,.html,.css">
            <button onclick="loadFile()">Load File</button>
            <input type="text" id="filename" placeholder="filename.txt">
            <button onclick="saveFile()">Save to Server</button>
            <button onclick="downloadFile()">Download</button>
        </div>
        
        <textarea id="editor" placeholder="Start typing..."></textarea>
        
        <div class="status">
            <div>Connected Users: <span class="users" id="usersList"></span></div>
            <div>Document: <span id="currentDoc">None</span></div>
            <div>Operations: <span id="opCount">0</span></div>
        </div>
    </div>

    <script>
        let ws = null;
        let currentDocId = null;
        let userId = 'user-' + Math.random().toString(36).substr(2, 9);
        let isUpdating = false;
        
        const editor = document.getElementById('editor');
        const docIdInput = document.getElementById('docId');
        const statusSpan = document.getElementById('connectionStatus');
        const usersListSpan = document.getElementById('usersList');
        const currentDocSpan = document.getElementById('currentDoc');
        const opCountSpan = document.getElementById('opCount');
        const filenameInput = document.getElementById('filename');
        
        function updateStatus(status, color = '#333') {
            statusSpan.textContent = status;
            statusSpan.style.color = color;
        }
        
        function updateUsers(users) {
            usersListSpan.innerHTML = users.map(user => 
                `<span class="user">${user}</span>`
            ).join('');
        }
        
        async function connectToDocument() {
            const docId = docIdInput.value.trim();
            if (!docId) {
                alert('Please enter a document ID');
                return;
            }
            
            if (ws) {
                ws.close();
            }
            
            currentDocId = docId;
            updateStatus('Connecting...', 'orange');
            
            const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
            ws = new WebSocket(`${wsProtocol}//${window.location.host}/ws/${docId}/${userId}`);
            
            ws.onopen = function() {
                updateStatus('Connected', 'green');
                currentDocSpan.textContent = docId;
            };
            
            ws.onmessage = function(event) {
                const data = JSON.parse(event.data);
                handleMessage(data);
            };
            
            ws.onclose = function() {
                updateStatus('Disconnected', 'red');
                currentDocSpan.textContent = 'None';
                updateUsers([]);
            };
            
            ws.onerror = function(error) {
                updateStatus('Connection Error', 'red');
                console.error('WebSocket error:', error);
            };
        }
        
        function disconnect() {
            if (ws) {
                ws.close();
                ws = null;
            }
        }
        
        function handleMessage(data) {
            switch(data.type) {
                case 'document_state':
                    isUpdating = true;
                    editor.value = data.content;
                    isUpdating = false;
                    updateUsers(data.users);
                    opCountSpan.textContent = data.operation_count;
                    break;
                    
                case 'operation':
                    if (data.user_id !== userId) {
                        applyOperation(data);
                    }
                    break;
                    
                case 'user_joined':
                    updateUsers(data.users);
                    break;
                    
                case 'user_left':
                    updateUsers(data.users);
                    break;
                    
                case 'file_saved':
                    alert(`File saved successfully: ${data.filename}`);
                    break;
                    
                case 'error':
                    alert('Error: ' + data.message);
                    break;
            }
        }
        
        function applyOperation(op) {
            isUpdating = true;
            const currentValue = editor.value;
            const cursorPos = editor.selectionStart;
            
            if (op.op_type === 'insert') {
                editor.value = currentValue.slice(0, op.position) + 
                              op.content + 
                              currentValue.slice(op.position);
                
                // Adjust cursor position if insertion happened before cursor
                if (op.position <= cursorPos) {
                    editor.setSelectionRange(cursorPos + op.content.length, cursorPos + op.content.length);
                }
            } else if (op.op_type === 'delete') {
                const endPos = op.position + op.content.length;
                editor.value = currentValue.slice(0, op.position) + 
                              currentValue.slice(endPos);
                
                // Adjust cursor position if deletion happened before cursor
                if (op.position < cursorPos) {
                    const newCursorPos = Math.max(op.position, cursorPos - op.content.length);
                    editor.setSelectionRange(newCursorPos, newCursorPos);
                } else if (op.position === cursorPos) {
                    editor.setSelectionRange(cursorPos, cursorPos);
                }
            } else if (op.op_type === 'replace') {
                editor.value = op.content.substring(1); // Remove the "|" prefix
                editor.setSelectionRange(editor.value.length, editor.value.length);
            }
            
            lastContent = editor.value;
            lastCursorPosition = editor.selectionStart;
            isUpdating = false;
            opCountSpan.textContent = parseInt(opCountSpan.textContent) + 1;
        }
        
        let lastContent = '';
        let lastCursorPosition = 0;
        let operationTimeout = null;
        
        editor.addEventListener('input', function() {
            if (isUpdating || !ws || ws.readyState !== WebSocket.OPEN) return;
            
            const currentContent = editor.value;
            const cursorPosition = editor.selectionStart;
            
            // Clear previous timeout to batch rapid operations
            if (operationTimeout) {
                clearTimeout(operationTimeout);
            }
            
            operationTimeout = setTimeout(() => {
                const operations = calculateOperations(lastContent, currentContent, lastCursorPosition, cursorPosition);
                
                for (const operation of operations) {
                    ws.send(JSON.stringify(operation));
                }
                
                lastContent = currentContent;
                lastCursorPosition = cursorPosition;
            }, 50); // Small delay to batch rapid typing
        });
        
        // Also handle keydown for better deletion tracking
        editor.addEventListener('keydown', function(e) {
            if (isUpdating || !ws || ws.readyState !== WebSocket.OPEN) return;
            
            // Store cursor position before the change
            lastCursorPosition = editor.selectionStart;
            
            // Small delay to capture the change after it happens
            setTimeout(() => {
                if (!isUpdating) {
                    const currentContent = editor.value;
                    const cursorPosition = editor.selectionStart;
                    
                    const operations = calculateOperations(lastContent, currentContent, lastCursorPosition, cursorPosition);
                    
                    for (const operation of operations) {
                        ws.send(JSON.stringify(operation));
                    }
                    
                    lastContent = currentContent;
                    lastCursorPosition = cursorPosition;
                }
            }, 10);
        });
        
        function calculateOperations(oldText, newText, oldCursorPos, newCursorPos) {
            const operations = [];
            
            if (newText.length > oldText.length) {
                // Insertion
                const lengthDiff = newText.length - oldText.length;
                const insertPos = newCursorPos - lengthDiff;
                const insertedText = newText.slice(insertPos, newCursorPos);
                
                operations.push({
                    type: 'operation',
                    op_type: 'insert',
                    position: insertPos,
                    content: insertedText,
                    user_id: userId
                });
                
            } else if (newText.length < oldText.length) {
                // Deletion - find what was deleted
                const lengthDiff = oldText.length - newText.length;
                
                // Check if deletion happened at cursor position (backspace)
                if (newCursorPos === oldCursorPos - lengthDiff) {
                    // Backspace deletion
                    const deletePos = newCursorPos;
                    const deletedText = oldText.slice(deletePos, deletePos + lengthDiff);
                    
                    operations.push({
                        type: 'operation',
                        op_type: 'delete',
                        position: deletePos,
                        content: deletedText,
                        user_id: userId
                    });
                } else if (newCursorPos === oldCursorPos) {
                    // Forward delete
                    const deletePos = newCursorPos;
                    const deletedText = oldText.slice(deletePos, deletePos + lengthDiff);
                    
                    operations.push({
                        type: 'operation',
                        op_type: 'delete',
                        position: deletePos,
                        content: deletedText,
                        user_id: userId
                    });
                } else {
                    // Selection deletion - more complex case
                    const deletePos = Math.min(newCursorPos, oldCursorPos);
                    const deletedText = oldText.slice(deletePos, deletePos + lengthDiff);
                    
                    operations.push({
                        type: 'operation',
                        op_type: 'delete',
                        position: deletePos,
                        content: deletedText,
                        user_id: userId
                    });
                }
            }
            
            return operations;
        }
        
        // File operations
        function loadFile() {
            const fileInput = document.getElementById('fileInput');
            const file = fileInput.files[0];
            if (!file) return;
            
            const reader = new FileReader();
            reader.onload = function(e) {
                editor.value = e.target.result;
                lastContent = e.target.result;
                filenameInput.value = file.name;
                
                // Send full content as replace operation
                if (ws && ws.readyState === WebSocket.OPEN) {
                    ws.send(JSON.stringify({
                        type: 'operation',
                        op_type: 'replace',
                        position: 0,
                        content: `|${e.target.result}`,
                        user_id: userId
                    }));
                }
            };
            reader.readAsText(file);
        }
        
        async function saveFile() {
            if (!currentDocId) {
                alert('Please connect to a document first');
                return;
            }
            
            const filename = filenameInput.value.trim() || 'document.txt';
            const content = editor.value;
            
            try {
                const response = await fetch('/save-file', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({
                        filename: filename,
                        content: content,
                        doc_id: currentDocId
                    })
                });
                
                if (response.ok) {
                    const result = await response.json();
                    alert(`File saved: ${result.filepath}`);
                } else {
                    alert('Error saving file');
                }
            } catch (error) {
                alert('Error saving file: ' + error.message);
            }
        }
        
        function downloadFile() {
            const filename = filenameInput.value.trim() || 'document.txt';
            const content = editor.value;
            
            const blob = new Blob([content], { type: 'text/plain' });
            const url = URL.createObjectURL(blob);
            
            const a = document.createElement('a');
            a.href = url;
            a.download = filename;
            a.click();
            
            URL.revokeObjectURL(url);
        }
        
        // Initialize
        lastContent = editor.value;
    </script>
</body>
</html>
    """)

@app.websocket("/ws/{doc_id}/{user_id}")
async def websocket_endpoint(websocket: WebSocket, doc_id: str, user_id: str):
    await websocket.accept()
    
    # Initialize document if it doesn't exist
    if doc_id not in documents:
        documents[doc_id] = Document(doc_id)
    
    # Add connection
    if doc_id not in active_connections:
        active_connections[doc_id] = []
    active_connections[doc_id].append(websocket)
    
    doc = documents[doc_id]
    doc.connected_users.add(user_id)
    
    try:
        # Send current document state
        await websocket.send_text(json.dumps({
            "type": "document_state",
            "content": doc.content,
            "users": list(doc.connected_users),
            "operation_count": len(doc.operations)
        }))
        
        # Notify other users
        await broadcast_to_document(doc_id, {
            "type": "user_joined",
            "user_id": user_id,
            "users": list(doc.connected_users)
        }, websocket)
        
        while True:
            data = await websocket.receive_text()
            message = json.loads(data)
            
            if message["type"] == "operation":
                operation = DocumentOperation(
                    op_type=message["op_type"],
                    position=message["position"],
                    content=message["content"],
                    user_id=message["user_id"]
                )
                
                if apply_operation(doc, operation):
                    # Broadcast to other users
                    await broadcast_to_document(doc_id, {
                        "type": "operation",
                        "op_type": operation.op_type,
                        "position": operation.position,
                        "content": operation.content,
                        "user_id": operation.user_id
                    }, websocket)
                else:
                    await websocket.send_text(json.dumps({
                        "type": "error",
                        "message": "Failed to apply operation"
                    }))
    
    except WebSocketDisconnect:
        # Remove connection
        if doc_id in active_connections:
            active_connections[doc_id].remove(websocket)
        
        doc.connected_users.discard(user_id)
        
        # Notify other users
        await broadcast_to_document(doc_id, {
            "type": "user_left",
            "user_id": user_id,
            "users": list(doc.connected_users)
        })

@app.post("/save-file")
async def save_file(file_data: FileUpload):
    """Save document content to a file on the server"""
    try:
        # Create uploads directory if it doesn't exist
        os.makedirs("uploads", exist_ok=True)
        
        # Generate safe filename
        safe_filename = file_data.filename.replace("..", "").replace("/", "_").replace("\\", "_")
        filepath = os.path.join("uploads", f"{file_data.doc_id}_{safe_filename}")
        
        # Write file
        with open(filepath, "w", encoding="utf-8") as f:
            f.write(file_data.content)
        
        return {"message": "File saved successfully", "filepath": filepath}
    
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error saving file: {str(e)}")

@app.get("/documents/{doc_id}/export")
async def export_document(doc_id: str):
    """Export document as plain text"""
    if doc_id not in documents:
        raise HTTPException(status_code=404, detail="Document not found")
    
    doc = documents[doc_id]
    return {"content": doc.content, "operation_count": len(doc.operations)}

@app.get("/documents/{doc_id}/operations")
async def get_operations(doc_id: str):
    """Get operation history for a document"""
    if doc_id not in documents:
        raise HTTPException(status_code=404, detail="Document not found")
    
    doc = documents[doc_id]
    operations = []
    for op in doc.operations:
        operations.append({
            "id": op.id,
            "type": op.op_type,
            "position": op.position,
            "content": op.content,
            "user_id": op.user_id,
            "timestamp": op.timestamp
        })
    
    return {"operations": operations}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)