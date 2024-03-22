package main

import (
	"fmt"
	"time"
)

func hello() {
	for i := 0; i < 5; i++ {
		fmt.Println("Hello from goroutine!", i)
		time.Sleep(500 * time.Millisecond)
	}
}

func main() {
	go hello()

	for i := 0; i < 3; i++ {
		fmt.Println("Hello from main!", i)
		time.Sleep(1000 * time.Millisecond)
	}
}
