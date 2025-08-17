#!/usr/bin/env python3
"""
Audio transcription using OpenAI Whisper
Usage: python whisper_transcribe.py <audio_file_path> [language_code]
Example: python whisper_transcribe.py audio.wav es  # for Spanish
"""

import sys
import whisper
import torch

def main():
    if len(sys.argv) < 2 or len(sys.argv) > 3:
        print("Usage: python whisper_transcribe.py <audio_file_path> [language_code]")
        print("Language codes: en, es, fr, de, it, pt, ru, ja, ko, zh, etc.")
        print("Leave language blank for automatic detection")
        sys.exit(1)
    
    audio_file = sys.argv[1]
    language = sys.argv[2] if len(sys.argv) == 3 else None
    
    # Check if CUDA is available for GPU acceleration
    device = "cuda" if torch.cuda.is_available() else "cpu"
    print(f"Using device: {device}", file=sys.stderr)
    
    # Load the model (base model is a good balance of speed and accuracy)
    print("Loading Whisper model...", file=sys.stderr)
    model = whisper.load_model("base", device=device)
    
    # Transcribe the audio file with language option
    print("Transcribing audio...", file=sys.stderr)
    if language:
        print(f"Language: {language}", file=sys.stderr)
        result = model.transcribe(audio_file, language=language)
    else:
        print("Language: auto-detect", file=sys.stderr)
        result = model.transcribe(audio_file)
    
    # Print detected language if auto-detected
    if not language and 'language' in result:
        print(f"Detected language: {result['language']}", file=sys.stderr)
    
    # Print the transcription
    print(result["text"])

if __name__ == "__main__":
    main()
