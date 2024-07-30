import network_monitor

interface_name = "enp8s0"  # Change this to the appropriate network interface name
packets = network_monitor.sniff(interface_name)

for packet in packets:
    print(packet)

