#include <iostream>
#include <thread>
#include <mutex>

//std::mutex mtx;
int counter = 0;

void incrementCounter() {
    for (int i = 0; i < 1000000; ++i) {
        std::lock_guard<std::mutex> lock(mtx);  // RAII-style mutex locking
        ++counter;
    }
}

int main() {
    std::thread t1(incrementCounter);
    std::thread t2(incrementCounter);

    t1.join();
    t2.join();

    std::cout << "Final counter value: " << counter << std::endl;
    return 0;
}
