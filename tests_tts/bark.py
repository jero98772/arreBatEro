# Example with Bark
from bark import SAMPLE_RATE, generate_audio, preload_models
import soundfile as sf

preload_models()
# “history_prompt” picks one of the predefined voices
audio = generate_audio("Hello world, this is a female voice example.", history_prompt="v2/en_speaker_1")
sf.write("female_bark.wav", audio, SAMPLE_RATE)
