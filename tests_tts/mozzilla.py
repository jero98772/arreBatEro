# Example usage
from TTS.api import TTS

# Initialize TTS with English model
tts = TTS(model_name="tts_models/en/ljspeech/tacotron2-DDC")

# Generate speech in English
tts.tts_to_file(text="Hello, this is a test in English.", file_path="output_english.wav")

# Switch to a different language (e.g., German)
tts = TTS(model_name="tts_models/de/thorsten/tacotron2-DDC")
tts.tts_to_file(text="Hallo, dies ist ein Test auf Deutsch.", file_path="output_german.wav")