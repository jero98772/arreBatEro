#!/usr/bin/env python3
"""
Audio transcription using Vosk (offline)
Usage: python vosk_transcribe.py <audio_file_path>
Requires: Vosk model in ./model/ directory
"""

import sys
import json
import subprocess
import vosk
import wave
import os

def convert_to_wav(input_file, output_file):
    """Convert audio file to WAV format using ffmpeg"""
    try:
        subprocess.run([
            'ffmpeg', '-i', input_file, '-ac', '1', '-ar', '16000', 
            '-f', 'wav', output_file, '-y'
        ], check=True, capture_output=True)
        return True
    except subprocess.CalledProcessError:
        return False
    except FileNotFoundError:
        print("Error: ffmpeg not found. Please install ffmpeg.", file=sys.stderr)
        return False

def main():
    if len(sys.argv) != 2:
        print("Usage: python vosk_transcribe.py <audio_file_path>")
        sys.exit(1)
    
    audio_file = sys.argv[1]
    model_path = "model"
    
    # Check if model exists
    if not os.path.exists(model_path):
        print(f"Error: Model directory '{model_path}' not found.", file=sys.stderr)
        print("Please download a Vosk model and extract it to the 'model' directory.", file=sys.stderr)
        print("Models available at: https://alphacephei.com/vosk/models", file=sys.stderr)
        sys.exit(1)
    
    # Load the model
    print("Loading Vosk model...", file=sys.stderr)
    model = vosk.Model(model_path)
    rec = vosk.KaldiRecognizer(model, 16000)
    
    # Convert audio to proper format if needed
    temp_wav = None
    if not audio_file.lower().endswith('.wav'):
        temp_wav = f"/tmp/temp_audio_{os.getpid()}.wav"
        print("Converting audio to WAV format...", file=sys.stderr)
        if not convert_to_wav(audio_file, temp_wav):
            print("Error: Could not convert audio file. Please ensure ffmpeg is installed.", file=sys.stderr)
            sys.exit(1)
        audio_file = temp_wav
    
    try:
        # Open WAV file
        wf = wave.open(audio_file, 'rb')
        
        # Check audio format
        if wf.getnchannels() != 1 or wf.getsampwidth() != 2 or wf.getframerate() != 16000:
            print("Warning: Audio should be 16kHz mono WAV for best results", file=sys.stderr)
        
        print("Transcribing audio...", file=sys.stderr)
        
        # Process audio in chunks
        results = []
        while True:
            data = wf.readframes(4000)
            if len(data) == 0:
                break
            if rec.AcceptWaveform(data):
                result = json.loads(rec.Result())
                if result['text']:
                    results.append(result['text'])
        
        # Get final result
        final_result = json.loads(rec.FinalResult())
        if final_result['text']:
            results.append(final_result['text'])
        
        # Print transcription
        full_text = ' '.join(results)
        print(full_text)
        
    except Exception as e:
        print(f"Error processing audio: {e}", file=sys.stderr)
        sys.exit(1)
    finally:
        if temp_wav and os.path.exists(temp_wav):
            os.remove(temp_wav)
        if 'wf' in locals():
            wf.close()

if __name__ == "__main__":
    main()
