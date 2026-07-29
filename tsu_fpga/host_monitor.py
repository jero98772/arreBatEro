#!/usr/bin/env python3
"""
host_monitor.py — Read TSU UART stream and display spin/energy data.

Protocol (5 bytes per sweep):
  0xAA  |  spin_byte  |  E_hi  |  E_lo  |  0x55

Install:  pip install pyserial
Run:      python3 host_monitor.py /dev/ttyUSB0
"""

import sys
import struct
import serial
import time
from collections import deque

PORT  = sys.argv[1] if len(sys.argv) > 1 else "/dev/ttyUSB0"
BAUD  = 115_200
N     = 8       # number of p-bits

def spin_str(byte: int, n: int = N) -> str:
    """Print spin states as ↑/↓ symbols."""
    return " ".join("↑" if (byte >> i) & 1 else "↓" for i in range(n))

def bar(value: int, lo: int, hi: int, width: int = 30) -> str:
    """ASCII bar chart."""
    frac = max(0.0, min(1.0, (value - lo) / (hi - lo)))
    filled = int(frac * width)
    return "[" + "█" * filled + "░" * (width - filled) + f"] {value:5d}"

def find_frame(ser: serial.Serial) -> bytes | None:
    """Sync to 0xAA header, return 5-byte packet or None on timeout."""
    timeout_at = time.time() + 2.0
    while time.time() < timeout_at:
        b = ser.read(1)
        if b == b'\xaa':
            rest = ser.read(4)
            if len(rest) == 4 and rest[-1] == 0x55:
                return bytes([0xAA]) + rest
    return None

def main():
    print(f"Connecting to TSU on {PORT} @ {BAUD} baud …")
    ser = serial.Serial(PORT, BAUD, timeout=2)
    time.sleep(0.2)
    ser.reset_input_buffer()
    print("Connected. Receiving samples …\n")

    energy_history: deque[int] = deque(maxlen=64)
    sweep = 0

    try:
        while True:
            pkt = find_frame(ser)
            if pkt is None:
                print("  [no data — check connection]")
                continue

            spin_byte = pkt[1]
            energy    = struct.unpack(">h", bytes([pkt[2], pkt[3]]))[0]  # signed 16-bit
            energy_history.append(energy)
            sweep += 1

            # Clear line and print
            spins  = spin_str(spin_byte)
            e_bar  = bar(energy, -200, 200)
            avg_e  = sum(energy_history) / len(energy_history)
            colour = "\033[94m" if energy < -60 else ("\033[91m" if energy > 60 else "\033[92m")
            reset  = "\033[0m"

            print(f"\r{colour}Sweep {sweep:6d}{reset}  "
                  f"Spins: {spins}  "
                  f"E: {e_bar}  "
                  f"<E>={avg_e:+7.1f}",
                  end="", flush=True)

            if sweep % 100 == 0:
                print()   # newline every 100 sweeps

    except KeyboardInterrupt:
        print("\n\nStopped.")
    finally:
        ser.close()

if __name__ == "__main__":
    main()
