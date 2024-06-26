#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/io.h>

// Example I/O port addresses for a generic NIC
#define NIC_DATA_PORT 0x4000
#define NIC_CMD_PORT 0x4001
#define NIC_STATUS_PORT 0x4002
#define NIC_BUFFER_SIZE 256

// NIC status flags
#define NIC_STATUS_READY 0x01
#define NIC_STATUS_BUSY 0x02
#define NIC_STATUS_ERROR 0x04

// NIC commands
#define NIC_CMD_SEND 0x02
#define NIC_CMD_RECEIVE 0x03
#define NIC_CMD_RESET 0x00

// Define a structure for packets with metadata
typedef struct {
    uint8_t data[NIC_BUFFER_SIZE];
    uint16_t size;
    uint8_t src;
    uint8_t dest;
    uint8_t type;
} packet_t;

pthread_mutex_t nic_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t nic_cond = PTHREAD_COND_INITIALIZER;
volatile bool nic_interrupt_occurred = false;

void nic_interrupt_handler(int signum) {
    if (signum == SIGUSR1) {
        pthread_mutex_lock(&nic_mutex);
        nic_interrupt_occurred = true;
        pthread_cond_signal(&nic_cond);
        pthread_mutex_unlock(&nic_mutex);
    }
}

void nic_initialize() {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = nic_interrupt_handler;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);

    if (sigaction(SIGUSR1, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    if (iopl(3)) {
        perror("iopl");
        exit(1);
    }

    outb(NIC_CMD_RESET, NIC_CMD_PORT);
    usleep(1000);

    outb(NIC_STATUS_READY, NIC_STATUS_PORT);
}

void nic_send_packet(packet_t *packet) {
    pthread_mutex_lock(&nic_mutex);

    if (inb(NIC_STATUS_PORT) & NIC_STATUS_READY) {
        for (uint16_t i = 0; i < packet->size; i++) {
            outb(packet->data[i], NIC_DATA_PORT);
        }
        outb(NIC_CMD_SEND, NIC_CMD_PORT);
        printf("Packet sent\n");
    } else {
        fprintf(stderr, "NIC not ready\n");
    }

    pthread_mutex_unlock(&nic_mutex);
}

void nic_receive_packet(packet_t *packet) {
    pthread_mutex_lock(&nic_mutex);

    if (inb(NIC_STATUS_PORT) & NIC_STATUS_READY) {
        packet->size = inb(NIC_DATA_PORT);
        if (packet->size > NIC_BUFFER_SIZE) {
            fprintf(stderr, "Received packet size exceeds buffer size\n");
        } else {
            for (uint16_t i = 0; i < packet->size; i++) {
                packet->data[i] = inb(NIC_DATA_PORT);
            }
            outb(NIC_CMD_RECEIVE, NIC_CMD_PORT);
            printf("Packet received\n");
        }
    } else {
        fprintf(stderr, "NIC not ready\n");
    }

    pthread_mutex_unlock(&nic_mutex);
}

int main() {
    nic_initialize();

    packet_t packet;
    packet.size = 4;
    packet.data[0] = 0x01;
    packet.data[1] = 0x02;
    packet.data[2] = 0x03;
    packet.data[3] = 0x04;
    packet.src = 1;
    packet.dest = 2;
    packet.type = 0x01;

    nic_send_packet(&packet);
    nic_receive_packet(&packet);

    return 0;
}
