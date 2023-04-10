import socket
#https://realpython.com/python-sockets/
HOST = "192.168.20.30"  # Standard loopback interface address (localhost)
PORT = 65431 # Port to listen on (non-privileged ports are > 1023)

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    s.connect((HOST, PORT))
    s.send(bytes(input(), 'utf8'))
    data = s.recv(1024)

print(f"Received {data!r}")