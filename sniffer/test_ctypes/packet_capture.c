#include "packet_capture.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>

// Global static array to hold packet info.
static PacketInfo packet_array[MAX_PACKETS];
static int packet_count = 0;

// Helper function to add a new packet to our storage.
static void add_packet_info(PacketInfo *pkt) {
    if(packet_count < MAX_PACKETS) {
        packet_array[packet_count] = *pkt;
        packet_array[packet_count].packet_number = packet_count + 1;
        packet_count++;
    } else {
        // Optionally handle overflow.
        printf("Packet storage full.\n");
    }
}

// Example function to create a PacketInfo from raw buffer.
// (For brevity, this example only extracts Ethernet and IP header data and TCP ports if TCP.)
static PacketInfo create_packet_info(unsigned char *buffer, int size) {
    PacketInfo pkt;
    memset(&pkt, 0, sizeof(PacketInfo));
    
    // Ethernet header
    struct ethhdr *eth = (struct ethhdr *)buffer;
    memcpy(pkt.dest_mac, eth->h_dest, 6);
    memcpy(pkt.src_mac, eth->h_source, 6);
    pkt.eth_proto = eth->h_proto;
    
    // IP header
    struct iphdr *iph = (struct iphdr*)(buffer + sizeof(struct ethhdr));
    inet_ntop(AF_INET, &(iph->saddr), pkt.src_ip, sizeof(pkt.src_ip));
    inet_ntop(AF_INET, &(iph->daddr), pkt.dst_ip, sizeof(pkt.dst_ip));
    pkt.total_length = ntohs(iph->tot_len);
    pkt.ttl = iph->ttl;
    pkt.protocol = iph->protocol;
    
    // If TCP, extract TCP header info.
    if(pkt.protocol == IPPROTO_TCP) {
        unsigned short iphdr_len = iph->ihl * 4;
        struct tcphdr *tcph = (struct tcphdr*)(buffer + sizeof(struct ethhdr) + iphdr_len);
        pkt.src_port = ntohs(tcph->source);
        pkt.dst_port = ntohs(tcph->dest);
        pkt.seq = ntohl(tcph->seq);
        pkt.ack = ntohl(tcph->ack_seq);
        pkt.tcp_flags = (tcph->urg << 5) | (tcph->ack << 4) |
                        (tcph->psh << 3) | (tcph->rst << 2) |
                        (tcph->syn << 1) | tcph->fin;
    }
    
    // Calculate header size and copy payload if any.
    int header_size = sizeof(struct ethhdr) + (iph->ihl * 4);
    if(pkt.protocol == IPPROTO_TCP) {
        struct tcphdr *tcph = (struct tcphdr*)(buffer + sizeof(struct ethhdr) + (iph->ihl * 4));
        header_size += tcph->doff * 4;
    } else if(pkt.protocol == IPPROTO_UDP) {
        header_size += sizeof(struct udphdr);
    }
    
    pkt.payload_size = size - header_size;
    if(pkt.payload_size > 0) {
        pkt.payload = malloc(pkt.payload_size);
        if(pkt.payload)
            memcpy(pkt.payload, buffer + header_size, pkt.payload_size);
    } else {
        pkt.payload = NULL;
    }
    
    pkt.capture_time = time(NULL);
    
    return pkt;
}

__attribute__((visibility("default")))
void start_capture(void) {
    int sock_raw = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if(sock_raw < 0) {
        perror("Socket creation failed");
        return;
    }
    
    unsigned char *buffer = (unsigned char *)malloc(65536);
    if(!buffer) {
        close(sock_raw);
        return;
    }
    
    struct sockaddr saddr;
    int saddr_len = sizeof(saddr);
    
    printf("Starting Advanced Packet Capture...\n");
    printf("Press Ctrl+C to stop.\n\n");
    
    while(1) {
        int buflen = recvfrom(sock_raw, buffer, 65536, 0, &saddr, (socklen_t *)&saddr_len);
        if(buflen < 0) {
            printf("Failed to get packets\n");
            continue;
        }
        
        // Create a PacketInfo from the raw packet.
        PacketInfo pkt = create_packet_info(buffer, buflen);
        add_packet_info(&pkt);
        
        // (Optionally print or log here.)
    }
    
    free(buffer);
    close(sock_raw);
}

__attribute__((visibility("default")))
int get_packet_count(void) {
    return packet_count;
}

__attribute__((visibility("default")))
PacketInfo* get_packet_info(int index) {
    if(index < 0 || index >= packet_count) {
        return NULL;
    }
    return &packet_array[index];
}
