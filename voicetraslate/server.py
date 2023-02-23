import socket
from tools.tools import HOST,FROMIDIOM,TOIDIOM, PORT,speak,webTranslate,recordAudio
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind(('', PORT))
s.listen(5)
c, addr = s.accept()
print("Socket Up and running with a connection from",addr)
try:
    while True:
        rcvdData = c.recv(1024).decode()
        txt=webTranslate(rcvdData,FROMIDIOM,TOIDIOM)
        print( "S:",txt)
        speak(rcvdData)
        sendData =recordAudio()# input("N: ")
        c.send(sendData.encode())
        if(sendData == "Bye" or sendData == "bye"):
            break
except:
    c.close()  
c.close()