"""
# Install Piper
!pip install piper-tts

# Download voice models (need to do this first)
!mkdir -p ~/.local/share/piper/
!curl -L https://github.com/rhasspy/piper/releases/download/v1.2.0/voice-en-us-libritts-high.tar.gz | tar -xzf - -C ~/.local/share/piper/
!curl -L https://github.com/rhasspy/piper/releases/download/v1.2.0/voice-de-thorsten-low.tar.gz | tar -xzf - -C ~/.local/share/piper/
"""
# Example usage
from piper import PiperVoice
import wave

# English voice
english_voice = PiperVoice.load("~/.local/share/piper/en-us-libritts-high.onnx")

with wave.open("english_output.wav", "wb") as wav_file:
    wav_file.setframerate(english_voice.sample_rate)
    wav_file.setsampwidth(2)  # 16-bit audio
    wav_file.setnchannels(1)  # Mono
    audio = english_voice.synthesize("This is an English test with Piper TTS.")
    wav_file.writeframes(audio.tobytes())

# German voice
german_voice = PiperVoice.load("~/.local/share/piper/de-thorsten-low.onnx")
with wave.open("german_output.wav", "wb") as wav_file:
    wav_file.setframerate(german_voice.sample_rate)
    wav_file.setsampwidth(2)
    wav_file.setnchannels(1)
    audio = german_voice.synthesize("Dies ist ein deutscher Test mit Piper TTS.")
    wav_file.writeframes(audio.tobytes())