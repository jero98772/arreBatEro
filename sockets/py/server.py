# echo-server.py
#https://realpython.com/python-sockets/
import socket

HOST = "192.168.20.30"  # Standard loopback interface address (localhost)
PORT = 65431  # Port to listen on (non-privileged ports are > 1023)

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    s.bind((HOST, PORT))
    s.listen()
    conn, addr = s.accept()
    with conn:
        print(f"Connected by {addr}")
        while True:
            data = conn.recv(1024)
            if not data:
                break
            conn.sendall(data)
            print(int(data)*100)