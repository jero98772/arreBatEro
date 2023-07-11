import speech_recognition as sr
from gtts import gTTS
import os
from pygame import mixer
import time
import openai
openai.api_key = "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"
def recordAudio():
    # Record Audio
    r = sr.Recognizer()
    with sr.Microphone() as source:
        print("Say something!")
        audio = r.listen(source)
    
        # Speech recognition using Google Speech Recognition
        data = ""
        try:
            # Uses the default API key
            # To use another API key: r.recognize_google(audio, key="GOOGLE_SPEECH_RECOGNITION_API_KEY")
            data = r.recognize_google(audio)
            print("You said: " + data)
        except sr.UnknownValueError:
            print("Google Speech Recognition could not understand audio")
            speak("I couldn't understand you")
        except sr.RequestError as e:
            print("Could not request results from Google Speech Recognition service; {0}".format(e))
        data = data.lower()
        print(data)
        return data
def speak(audioString):
    print(audioString)
    tts = gTTS(text=audioString, lang='en')
    tts.save("audio.mp3")
    os.system("mpg123 audio.mp3")
    mixer.init()
    mixer.music.load("audio.mp3")
    mixer.music.play()
    #while mixer.music.get_busy():
    #    time.sleep(0.1)
 

def  chatGPT(msg):
    completion = openai.ChatCompletion.create(
      model="gpt-3.5-turbo",
      messages=[
        {"role": "user", "content": msg}
      ]
    )
    return  completion.choices[0].message.content


import speech_recognition as sr

import speech_recognition as sr

def real_time_speech_recognition(timeout=5):
    # Create a recognizer object
    recognizer = sr.Recognizer()

    # Use the default microphone as the audio source
    microphone = sr.Microphone()

    # Adjust the microphone for ambient noise
    with microphone as source:
        recognizer.adjust_for_ambient_noise(source)

    # Continuously listen for speech and perform recognition
    print("Listening...")

    with microphone as source:
        # Start listening for speech
        audio = recognizer.listen(source, timeout=timeout)

        try:
            # Recognize speech using Google Speech Recognition
            text = recognizer.recognize_google(audio)
            return text
        except sr.UnknownValueError:
            return "Speech recognition could not understand audio"
        except sr.RequestError as e:
            return "Could not request results from Google Speech Recognition service; {0}".format(e)
