import socket
import threading

# Server implementation
def start_server(host, port):
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind((host, port))
    server_socket.listen(1)
    print(f"Server listening on {host}:{port}")

    while True:
        client_socket, client_address = server_socket.accept()
        print(f"Connection from {client_address}")
        handle_client(client_socket)
def handle_client(client_socket):
    while True:
        message = client_socket.recv(1024).decode('utf-8')
        if not message:
            break
        print(f"Received: {message}")
        response = f"Echo: {message}"
        client_socket.sendall(response.encode('utf-8'))
    client_socket.close()
# Client implementation
def start_client(host, port):
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_socket.connect((host, port))
    print(f"Connected to {host}:{port}")

    while True:
        message = input("Enter message: ")
        client_socket.sendall(message.encode('utf-8'))
        response = client_socket.recv(1024).decode('utf-8')
        print(f"Response: {response}")

    client_socket.close()
# Example usage
if __name__ == "__main__":

    server_thread = threading.Thread(target=start_server, args=("localhost", 65432))
    server_thread.start()

    client_thread = threading.Thread(target=start_client, args=("localhost", 65432))
    client_thread.start()