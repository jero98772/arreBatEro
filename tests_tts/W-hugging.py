# Install required libraries

#pip install transformers torch soundfile

# Using MMS-TTS model for multilingual TTS
from transformers import VitsModel, AutoTokenizer
import torch
import soundfile as sf

# Load model and tokenizer for MMS-TTS
model_id = "facebook/mms-tts-eng"  # English model
tokenizer = AutoTokenizer.from_pretrained(model_id)
model = VitsModel.from_pretrained(model_id)

# Generate English speech (female voice)
inputs = tokenizer("Hello world! This is a test of the Hugging Face MMS-TTS model.", return_tensors="pt")
with torch.no_grad():
    output = model(**inputs, speaker_id=1).waveform  # <- Female speaker ID
sf.write("hf_english_female.wav", output.squeeze().numpy(), model.config.sampling_rate)

# For another language (e.g., Spanish)
model_id = "facebook/mms-tts-spa"  # Spanish model
tokenizer = AutoTokenizer.from_pretrained(model_id)
model = VitsModel.from_pretrained(model_id)

inputs = tokenizer("Hola mundo! Esta es una prueba del modelo MMS-TTS de Hugging Face.", return_tensors="pt")
with torch.no_grad():
    output = model(**inputs, speaker_id=3).waveform  # <- Female speaker ID
sf.write("hf_spanish_female.wav", output.squeeze().numpy(), model.config.sampling_rate)
