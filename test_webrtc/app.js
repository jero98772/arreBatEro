let localStream;
let remoteStream;
let peerConnection;

const configuration = {
    iceServers: [
        { urls: 'stun:stun.l.google.com:19302' }
    ]
};

const localVideo = document.getElementById('localVideo');
const remoteVideo = document.getElementById('remoteVideo');
const startButton = document.getElementById('startButton');
const callButton = document.getElementById('callButton');
const hangupButton = document.getElementById('hangupButton');

startButton.addEventListener('click', startVideo);
callButton.addEventListener('click', startCall);
hangupButton.addEventListener('click', hangup);

async function startVideo() {
    try {
        localStream = await navigator.mediaDevices.getUserMedia({ video: true, audio: true });
        localVideo.srcObject = localStream;
    } catch (error) {
        console.error('Error accessing media devices:', error);
    }
}

function startCall() {
    peerConnection = new RTCPeerConnection(configuration);
    
    localStream.getTracks().forEach(track => {
        peerConnection.addTrack(track, localStream);
    });

    peerConnection.ontrack = event => {
        remoteVideo.srcObject = event.streams[0];
    };

    peerConnection.onicecandidate = event => {
        if (event.candidate) {
            // Send the candidate to the remote peer
            // In a real application, you would send this over your signaling channel
            console.log('New ICE candidate:', event.candidate);
        }
    };

    createAndSendOffer();
}

async function createAndSendOffer() {
    try {
        const offer = await peerConnection.createOffer();
        await peerConnection.setLocalDescription(offer);
        
        // Send the offer to the remote peer
        // In a real application, you would send this over your signaling channel
        console.log('Offer:', offer);
    } catch (error) {
        console.error('Error creating offer:', error);
    }
}

function hangup() {
    if (peerConnection) {
        peerConnection.close();
        peerConnection = null;
    }
    localStream.getTracks().forEach(track => track.stop());
    localVideo.srcObject = null;
    remoteVideo.srcObject = null;
}