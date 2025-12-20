import torch
from openvoice.api import ToneColorConverter
from openvoice import se_extractor
from openvoice.api import BaseSpeakerTTS

# Setup
device = 'cuda' if torch.cuda.is_available() else 'cpu'
print(f"Using device: {device}")

# Paths
en_ckpt_base = 'checkpoints/base_speakers/EN'
ckpt_converter = 'checkpoints/converter'

# Load models
print("Loading TTS model...")
tts_model = BaseSpeakerTTS(f'{en_ckpt_base}/config.json', device=device)
tts_model.load_ckpt(f'{en_ckpt_base}/checkpoint.pth')

print("Loading tone converter...")
tone_converter = ToneColorConverter(f'{ckpt_converter}/config.json', device=device)
tone_converter.load_ckpt(f'{ckpt_converter}/checkpoint.pth')

# Load speaker embeddings
print("Loading speaker embeddings...")
source_se = torch.load(f'{en_ckpt_base}/en_default_se.pth').to(device)

# Generate speech
print("Generating speech...")
text = "Hello, this is a test of OpenVoice text to speech."
output_path = "test_output.wav"

tts_model.tts(text, output_path, speaker='default', language='English', speed=1.0)

print(f"✓ Audio saved to {output_path}")
print("Test completed successfully!")
