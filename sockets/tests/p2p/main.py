#Aman Nagpal
#https://github.com/Ezi0aaudit0re
import socket
import threading 
import sys
import time
from random import randint

BYTE_SIZE = 1024
HOST = '127.0.0.1'
PORT = 5000
PEER_BYTE_DIFFERENTIATOR = b'\x11' 
RAND_TIME_START = 1
RAND_TIME_END = 2
REQUEST_STRING = "req"

__author__ = "Aman Nagpal"

class Client: 
    def __init__(self, addr):
       self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
       self.s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
       self.s.connect((addr, PORT))
       self.previous_data = None
       i_thread = threading.Thread(target=self.send_message)
       i_thread.daemon = True
       i_thread.start()
       while True:
           r_thread = threading.Thread(target=self.recieve_message)
           r_thread.start()
           r_thread.join()
           data = self.recieve_message()
           print(data)
           if not data:
               print("-" * 21 + " Server failed " + "-" * 21)
               break
           elif data[0:1] == b'\x11':
               print("Got peers")
               self.update_peers(data[1:])
           txt=input("send data:")
           s.send(txt.encode());
    def recieve_message(self):
       try:
           print("Recieving -------")
           data = self.s.recv(BYTE_SIZE)
           print(data.decode("utf-8"))
           print("\nRecieved message on the client side is:")
           if self.previous_data != data:
               #fileIO.create_file(data)
               self.previous_data = data 
           return data
       except KeyboardInterrupt:
           self.send_disconnect_signal()
    def update_peers(self, peers):
        p2p.peers = str(peers, "utf-8").split(',')[:-1]
    def send_message(self):
        try:
            self.s.send(REQUEST_STRING.encode('utf-8'))
        except KeyboardInterrupt as e:
            self.send_disconnect_signal()
            return
    def send_disconnect_signal(self):
       print("Disconnected from server")
       self.s.send("q".encode('utf-8'))
       sys.exit()
class Server: 
    def __init__(self):
        try:
            self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            self.connections = []
            self.peers = []
            self.s.bind((HOST, PORT))
            self.s.listen(1)
            print("-" * 12+ "Server Running"+ "-" *21)
            self.run()
        except Exception as e:
            sys.exit()

    def handler(self, connection, a):
        try:
            while True:
                data = connection.recv(BYTE_SIZE)
                for connection in self.connections:
                    if data and data.decode('utf-8')[0].lower() == 'q':
                        self.disconnect(connection, a)
                        return
                    elif data and data.decode('utf-8') == REQUEST_STRING:
                        print("-" * 21 + " UPLOADING " + "-" * 21)
                        connection.send(data)
        except Exception as e:
            sys.exit()

    def disconnect(self, connection, a):
        self.connections.remove(connection)
        self.peers.remove(a)
        connection.close()
        self.send_peers()
        print("{}, disconnected".format(a))
        print("-" * 50)
    def run(self):
        while True:
            connection, a = self.s.accept()
            self.peers.append(a)
            print("Peers are: {}".format(self.peers) )
            self.send_peers()
            c_thread = threading.Thread(target=self.handler, args=(connection, a))
            c_thread.daemon = True
            c_thread.start()
            self.connections.append(connection)
            print("{}, connected".format(a))
            print("-" * 50)
    def send_peers(self):
        peer_list = ""
        for peer in self.peers:
            peer_list = peer_list + str(peer[0]) + ","
        for connection in self.connections:
            data = PEER_BYTE_DIFFERENTIATOR + bytes(peer_list, 'utf-8')
            connection.send(PEER_BYTE_DIFFERENTIATOR + bytes(peer_list, 'utf-8'))
class p2p:
    peers = ['127.0.0.1']
def main():
    while True:
        try:
            print("-" * 21 + "Trying to connect" + "-" * 21)
            time.sleep(randint(RAND_TIME_START,RAND_TIME_END))
            for peer in p2p.peers:
                try:
                    client = Client(peer)
                except KeyboardInterrupt:
                    sys.exit(0)
                except:
                    pass
                try:
                    server = Server()
                except KeyboardInterrupt:
                    sys.exit()
                except:
                    pass
        except KeyboardInterrupt as e:
            sys.exit(0)
if __name__ == "__main__":
    main()
