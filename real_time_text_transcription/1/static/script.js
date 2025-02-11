# static/script.js
let isRecording = false;
let mediaRecorder;
let ws;
const recordButton = document.getElementById('recordButton');
const transcriptionDiv = document.getElementById('transcription');

// AudioContext for processing audio data
const audioContext = new AudioContext();
let audioProcessor = null;

function connectWebSocket() {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    ws = new WebSocket(`${protocol}//${window.location.host}/ws`);
    
    ws.onmessage = function(event) {
        transcriptionDiv.textContent += event.data + ' ';
        transcriptionDiv.scrollTop = transcriptionDiv.scrollHeight;
    };
}

async function startRecording() {
    try {
        const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
        const audioInput = audioContext.createMediaStreamSource(stream);
        
        // Create audio processor
        audioProcessor = audioContext.createScriptProcessor(4096, 1, 1);
        
        audioProcessor.onaudioprocess = function(e) {
            if (ws && ws.readyState === WebSocket.OPEN) {
                const audioData = e.inputBuffer.getChannelData(0);
                const base64Data = arrayBufferToBase64(audioData.buffer);
                ws.send("data:audio/wav;base64," + base64Data);
            }
        };

        // Connect nodes
        audioInput.connect(audioProcessor);
        audioProcessor.connect(audioContext.destination);

    } catch (error) {
        console.error('Error:', error);
        stopRecording();
    }
}

function stopRecording() {
    if (audioProcessor) {
        audioProcessor.disconnect();
        audioProcessor = null;
    }
    if (ws) {
        ws.close();
    }
    recordButton.textContent = 'Start Recording';
    recordButton.classList.remove('recording');
    isRecording = false;
}

function arrayBufferToBase64(buffer) {
    const binary = new Float32Array(buffer);
    const bytes = new Uint8Array(binary.buffer);
    let binary_string = '';
    for (let i = 0; i < bytes.byteLength; i++) {
        binary_string += String.fromCharCode(bytes[i]);
    }
    return btoa(binary_string);
}

recordButton.onclick = async () => {
    if (!isRecording) {
        connectWebSocket();
        await startRecording();
        recordButton.textContent = 'Stop Recording';
        recordButton.classList.add('recording');
        isRecording = true;
    } else {
        stopRecording();
    }
};

window.onbeforeunload = function() {
    if (isRecording) {
        stopRecording();
    }
};