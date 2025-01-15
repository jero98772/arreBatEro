import React, { useState, useEffect } from 'react';
import axios from 'axios';
import { Treebeard } from 'react-treebeard';
import { useLocation } from 'react-router-dom';

function FileTree({ files, onFileSelect }) {
    const handleClick = (file) => {
        if (file.children) return;
        onFileSelect(file);
    };

    return <Treebeard data={files} onToggle={(node) => handleClick(node)} />;
}

function TextEditor({ selectedFile, content, setContent, saveFile }) {
    return (
        <div className="editor">
            {selectedFile ? (
                <>
                    <textarea
                        value={content}
                        onChange={(e) => setContent(e.target.value)}
                        style={{ width: '100%', height: '500px' }}
                    />
                    <button onClick={saveFile}>Save File</button>
                </>
            ) : (
                <p>Select a file to edit</p>
            )}
        </div>
    );
}

function Shell() {
    const [command, setCommand] = useState('');
    const [output, setOutput] = useState('');

    const runCommand = async () => {
        try {
            const response = await axios.post('http://localhost:8000/run-command/', {
                command: command
            });
            setOutput(response.data.output || response.data.error);
        } catch (error) {
            setOutput('Error connecting to the server');
        }
    };

    return (
        <div className="shell">
            <input
                type="text"
                value={command}
                onChange={(e) => setCommand(e.target.value)}
                placeholder="Enter shell command"
            />
            <button onClick={runCommand}>Run</button>
            <pre>{output}</pre>
        </div>
    );
}

function App() {
    const location = useLocation();
    const [files, setFiles] = useState({
        name: 'root',
        toggled: true,
        children: [
            { name: 'file1.txt', path: '/home/me/file1' },
            { name: 'file2.js', path: '/home/me/file2.js' },
            { name: 'folder', children: [{ name: 'file3.py', path: '/home/me/folder/file3.py' }] }
        ]
    });

    const [selectedFile, setSelectedFile] = useState(null);
    const [content, setContent] = useState('');

    useEffect(() => {
        const filePath = location.pathname.substring(1);  // Extract file path from URL
        if (filePath) {
            openFile(filePath);
        }
    }, [location]);

    const openFile = async (filePath) => {
        try {
            const response = await axios.get(`http://localhost:8000/open-file/?file_path=${filePath}`);
            const fileContent = await response.data;  // assuming plain text for simplicity
            setSelectedFile(filePath);
            setContent(fileContent);
        } catch (error) {
            console.error("Error opening file", error);
            setContent("Error loading file");
        }
    };

    const saveFile = async () => {
        try {
            await axios.post(`http://localhost:8000/write-file/?file_path=${selectedFile}`, {
                content: content
            });
            alert('File saved successfully!');
        } catch (error) {
            console.error("Error saving file", error);
            alert('Error saving file');
        }
    };

    return (
        <div className="app">
            <div className="file-tree" style={{ width: '20%', float: 'left' }}>
                <FileTree files={files} onFileSelect={(file) => openFile(file.path)} />
            </div>
            <div className="text-editor" style={{ width: '60%', float: 'left' }}>
                <TextEditor selectedFile={selectedFile} content={content} setContent={setContent} saveFile={saveFile} />
            </div>
            <div className="shell" style={{ width: '100%', clear: 'both', marginTop: '20px' }}>
                <Shell />
            </div>
        </div>
    );
}

export default App;
