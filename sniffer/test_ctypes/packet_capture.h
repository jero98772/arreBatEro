#ifndef PACKET_CAPTURE_H
#define PACKET_CAPTURE_H

#include <netinet/ip.h>
#include <net/ethernet.h>
#include <time.h>

#define MAX_PACKETS 1024

// Structure to store packet information
typedef struct PacketInfo {
    int packet_number;
    // Ethernet header info
    unsigned char dest_mac[6];
    unsigned char src_mac[6];
    unsigned short eth_proto;
    // IP header info
    char src_ip[16];
    char dst_ip[16];
    unsigned int total_length;
    unsigned char ttl;
    unsigned char protocol;
    // Transport layer info (for TCP example)
    unsigned short src_port;      // valid if protocol==TCP
    unsigned short dst_port;      // valid if protocol==TCP
    unsigned int seq;             // TCP sequence
    unsigned int ack;             // TCP ack
    unsigned char tcp_flags;      // packed flags (if TCP)
    // You can add UDP fields or more as needed.
    // Payload (dynamically allocated, freed when needed)
    unsigned char *payload;
    int payload_size;
    // Capture timestamp
    time_t capture_time;
} PacketInfo;

#ifdef __cplusplus
extern "C" {
#endif

// Starts the packet capture (blocking call)
__attribute__((visibility("default"))) void start_capture(void);

// Get number of packets captured so far.
__attribute__((visibility("default"))) int get_packet_count(void);

// Get pointer to PacketInfo for packet at index (0-based). Returns NULL if out-of-range.
__attribute__((visibility("default"))) PacketInfo* get_packet_info(int index);

#ifdef __cplusplus
}
#endif

#endif
