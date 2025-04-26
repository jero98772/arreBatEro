# Install espeak and the python wrapper
# On Ubuntu/Debian: sudo apt-get install espeak
# Then: pip install py-espeak-ng

from espeakng import ESpeakNG
import subprocess
import os

# Initialize the TTS engine
esng = ESpeakNG()

# Function to generate speech in different languages
def speak_multilingual(text, lang='en', output_file=None):
    esng.voice = lang
    if output_file:
        # Use subprocess to call espeak directly for file output
        subprocess.run([
            'espeak-ng',
            '-v', lang,
            '-w', output_file,
            text
        ])
        print(f"Saved to {output_file}")
    else:
        esng.say(text)  # This will play the audio

# Examples in multiple languages
speak_multilingual("This is an English example.", "en-us", "espeak_english.wav")
speak_multilingual("Dies ist ein Beispiel auf Deutsch.", "de", "espeak_german.wav")
speak_multilingual("Ceci est un exemple en français.", "fr", "espeak_french.wav")
speak_multilingual("Este es un ejemplo en español.", "es", "espeak_spanish.wav")