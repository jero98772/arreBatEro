# Install ESPnet
!pip install espnet espnet_model_zoo
!pip install torch matplotlib soundfile parallel_wavegan

# Usage example
from espnet2.bin.tts_inference import Text2Speech
from espnet_model_zoo.downloader import ModelDownloader
import soundfile as sf

# Download a Japanese model
d = ModelDownloader()
model_info = d.download_and_unpack("kan-bayashi/jsut_tacotron2")
text2speech = Text2Speech(**model_info, device="cpu")

# Generate speech in Japanese
wav = text2speech("こんにちは、これはテストです。")["wav"]
sf.write("japanese_output.wav", wav.numpy(), text2speech.fs, "PCM_16")

# For English
model_info = d.download_and_unpack("kan-bayashi/ljspeech_tacotron2")
text2speech = Text2Speech(**model_info, device="cpu")
wav = text2speech("This is an English test..")["wav"]
sf.write("english_output.wav", wav.numpy(), text2speech.fs, "PCM_16")