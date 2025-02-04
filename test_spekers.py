import speech_recognition as sr
import time
import os
from gtts import gTTS
import pygame

# Initialize the pygame mixer
pygame.mixer.init()

def speak(audioString):
    print(audioString)
    tts = gTTS(text=audioString, lang='en')
    tts.save("audio.mp3")
    
    # Play the audio using pygame
    pygame.mixer.music.load("audio.mp3")
    pygame.mixer.music.play()
    
    # Wait until the audio finishes playing
    while pygame.mixer.music.get_busy():
        time.sleep(1)

speak("Hello, how are you?")
