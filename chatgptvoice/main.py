from tools import *
speak("hablame")
txt=real_time_speech_recognition()

for i in real_time_speech_recognition():
	print(txt,"\n")
	speak(chatGPT(txt))
#speak(chatGPT(input()))

