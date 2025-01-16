#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <pthread.h>

#define PORT 8080
#define BUFFER_SIZE 1024
#define STATIC_DIR "./static"

// Function to handle HTTP requests
void handle_request(int client_socket) {
    char buffer[BUFFER_SIZE] = {0};
    read(client_socket, buffer, BUFFER_SIZE);
    printf("Received request:\n%s\n", buffer);

    // Extract the requested file path
    char method[10], path[256];
    sscanf(buffer, "%s %s", method, path);

    if (strcmp(method, "GET") != 0) {
        const char *response = "HTTP/1.1 405 Method Not Allowed\r\n\r\n";
        write(client_socket, response, strlen(response));
        close(client_socket);
        return;
    }

    // Construct file path
    char file_path[512];
    snprintf(file_path, sizeof(file_path), "%s%s", STATIC_DIR, path);

    // If the root is requested, serve index.html
    if (strcmp(path, "/") == 0) {
        snprintf(file_path, sizeof(file_path), "%s/index.html", STATIC_DIR);
    }

    // Open the file
    int file_fd = open(file_path, O_RDONLY);
    if (file_fd < 0) {
        const char *not_found_response = "HTTP/1.1 404 Not Found\r\n\r\nFile not found.";
        write(client_socket, not_found_response, strlen(not_found_response));
    } else {
        // Send HTTP response headers
        const char *headers = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n";
        write(client_socket, headers, strlen(headers));

        // Send file content
        char file_buffer[BUFFER_SIZE];
        int bytes_read;
        while ((bytes_read = read(file_fd, file_buffer, BUFFER_SIZE)) > 0) {
            write(client_socket, file_buffer, bytes_read);
        }
        close(file_fd);
    }
    close(client_socket);
}

int main() {
    int server_fd, client_socket;
    struct sockaddr_in address;
    int opt = 1;
    int addrlen = sizeof(address);

    // Create socket
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("Socket failed");
        exit(EXIT_FAILURE);
    }

    // Attach socket to the port
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt))) {
        perror("Setsockopt failed");
        exit(EXIT_FAILURE);
    }

    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(PORT);

    // Bind socket to the address
    if (bind(server_fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        perror("Bind failed");
        exit(EXIT_FAILURE);
    }

    // Listen for connections
    if (listen(server_fd, 10) < 0) {
        perror("Listen failed");
        exit(EXIT_FAILURE);
    }

    printf("Server is listening on port %d\n", PORT);

    while (1) {
        // Accept new connection
        if ((client_socket = accept(server_fd, (struct sockaddr *)&address, (socklen_t*)&addrlen)) < 0) {
            perror("Accept failed");
            exit(EXIT_FAILURE);
        }

        // Handle the request
        handle_request(client_socket);
    }

    return 0;
}
