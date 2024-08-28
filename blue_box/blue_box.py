import numpy as np
import sounddevice as sd
import matplotlib.pyplot as plt
from scipy.io.wavfile import write
import os

# Define the DTMF frequency table
dtmf_frequencies = {
    '1': (697, 1209),
    '2': (697, 1336),
    '3': (697, 1477),
    '4': (770, 1209),
    '5': (770, 1336),
    '6': (770, 1477),
    '7': (852, 1209),
    '8': (852, 1336),
    '9': (852, 1477),
    '0': (941, 1336),
    '*': (941, 1209),
    '#': (941, 1477),
}

def generate_dtmf_tone(digit, duration=0.5, sampling_rate=44100):
    if digit not in dtmf_frequencies:
        raise ValueError("Invalid DTMF digit.")
    
    f1, f2 = dtmf_frequencies[digit]
    t = np.linspace(0, duration, int(sampling_rate * duration), endpoint=False)
    signal = np.sin(2 * np.pi * f1 * t) + np.sin(2 * np.pi * f2 * t)
    return signal

def save_audio(signal, filename='dtmf_tone.wav', sampling_rate=44100):
    # Ensure the signal is in the range of int16 for WAV format
    scaled_signal = np.int16(signal / np.max(np.abs(signal)) * 32767)
    write(filename, sampling_rate, scaled_signal)
    print(f"Audio saved as {filename}")

def play_audio(filename):
    # Use a system command to play the audio file
    if os.name == 'nt':  # Windows
        os.system(f'start {filename}')
    else:  # macOS or Linux
        os.system(f'xdg-open {filename}')

# Example usage
digit = input("Press a DTMF digit (0-9, *, #): ")
signal = generate_dtmf_tone(digit)
save_audio(signal)
play_audio('dtmf_tone.wav')
