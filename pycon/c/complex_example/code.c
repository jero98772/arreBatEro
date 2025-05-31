// hello_kernel.c — compile with: i686-elf-gcc -ffreestanding -c hello_kernel.c -o hello_kernel.o
void kernel_main() {
    const char *str = "Hello from kernel!";
    char *video_memory = (char *) 0xb8000;
    for (int i = 0; str[i] != '\0'; i++) {
        video_memory[i * 2] = str[i];
        video_memory[i * 2 + 1] = 0x07; // Light grey on black
    }
}
