import socket
from tools.tools import HOST,FROMIDIOM,TOIDIOM, PORT,speak,webTranslate,recordAudio
s = socket.socket()
s.connect((HOST,PORT))
#try:
while True:
	txt=recordAudio()
	#txt = input("S: ")
	txt=webTranslate(txt,FROMIDIOM,TOIDIOM)
	s.send(txt.encode());
	if(txt == "Bye" or txt == "bye"):
		break
	rcvdData=str(s.recv(1024).decode())
	print ("N:",rcvdData)
	speak(rcvdData)
#except:
#	s.close()
s.close()