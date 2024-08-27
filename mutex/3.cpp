#include <iostream>
#include <thread>
#include <queue>
#include <mutex>
#include <condition_variable>

std::queue<int> buffer;
std::mutex mtx;
std::condition_variable cv;
const int MAX_BUFFER_SIZE = 10;

void producer(int id) {
    int count = 0;
    while (count < 20) {
        std::unique_lock<std::mutex> lock(mtx);
        cv.wait(lock, []() { return buffer.size() < MAX_BUFFER_SIZE; });
        buffer.push(count);
        std::cout << "Producer " << id << " produced " << count << std::endl;
        count++;
        cv.notify_all();
    }
}

void consumer(int id) {
    int data;
    while (true) {
        std::unique_lock<std::mutex> lock(mtx);
        cv.wait(lock, []() { return !buffer.empty(); });
        data = buffer.front();
        buffer.pop();
        std::cout << "Consumer " << id << " consumed " << data << std::endl;
        cv.notify_all();
    }
}

int main() {
    std::thread p1(producer, 1);
    std::thread p2(producer, 2);
    std::thread c1(consumer, 1);
    std::thread c2(consumer, 2);

    p1.join();
    p2.join();
    c1.detach(); // To keep consuming indefinitely
    c2.detach(); // To keep consuming indefinitely

    return 0;
}
