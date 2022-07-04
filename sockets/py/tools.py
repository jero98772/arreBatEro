import socket
global HOST
global PORT
HOST = "127.0.0.1"
PORT = 12345
def clientinit(mesage,HOST, PORT):
	with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
		s.bind((HOST, PORT))
		data = s.recv(1024)
		s.sendall(2)
def client():
	with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
		s.bind((HOST, PORT))
		s.sendall(b"Hello, world")
		data = s.recv(1024)
def server():
	with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
		s.bind((HOST, PORT))
		conn, addr = s.accept()
		with conn:
			print(f"Connected by {addr}")
			while True:
				data = conn.recv(1024)
				if not data:
					break
				conn.sendall(data)
