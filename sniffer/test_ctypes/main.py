import ctypes
import os
import socket
import threading
import time

# Define a corresponding ctypes Structure for PacketInfo
class PacketInfo(ctypes.Structure):
    _fields_ = [
        ("packet_number", ctypes.c_int),
        ("dest_mac", ctypes.c_ubyte * 6),
        ("src_mac", ctypes.c_ubyte * 6),
        ("eth_proto", ctypes.c_ushort),
        ("src_ip", ctypes.c_char * 16),
        ("dst_ip", ctypes.c_char * 16),
        ("total_length", ctypes.c_uint),
        ("ttl", ctypes.c_ubyte),
        ("protocol", ctypes.c_ubyte),
        ("src_port", ctypes.c_ushort),
        ("dst_port", ctypes.c_ushort),
        ("seq", ctypes.c_uint),
        ("ack", ctypes.c_uint),
        ("tcp_flags", ctypes.c_ubyte),
        # Skipping UDP fields for brevity.
        ("payload", ctypes.POINTER(ctypes.c_ubyte)),
        ("payload_size", ctypes.c_int),
        ("capture_time", ctypes.c_long)  # time_t may be long
    ]

# Load the shared library (adjust path as needed)
lib_path = os.path.join(os.getcwd(), "libpacket_capture.so")
packet_lib = ctypes.CDLL(lib_path)

# Declare return types for getter functions.
packet_lib.get_packet_count.restype = ctypes.c_int
packet_lib.get_packet_info.restype = ctypes.POINTER(PacketInfo)

# Optionally, start the capture in a separate thread (or run in blocking mode).
# For demonstration, we could call start_capture() in a separate thread using threading.

def run_capture():
    packet_lib.start_capture()

# Start capture in a daemon thread (it will run until stopped manually)
capture_thread = threading.Thread(target=run_capture, daemon=True)
capture_thread.start()

# Later, access the captured packets:
time.sleep(5)  # Give some time for packets to be captured

count = packet_lib.get_packet_count()
print("Packets captured:", count)

for i in range(count):
    pkt_ptr = packet_lib.get_packet_info(i)
    pkt = pkt_ptr.contents
    # Convert MAC addresses to string
    src_mac = ":".join(f"{b:02X}" for b in pkt.src_mac)
    dst_mac = ":".join(f"{b:02X}" for b in pkt.dest_mac)
    src_ip = pkt.src_ip.decode('utf-8')
    dst_ip = pkt.dst_ip.decode('utf-8')
    print(f"Packet {pkt.packet_number}: {src_ip} -> {dst_ip}, Src MAC: {src_mac}, Dst MAC: {dst_mac}, Protocol: {pkt.protocol}")

# Later, these getter functions can be wrapped in FastAPI endpoints.
