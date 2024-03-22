package main

import (
    "fmt"
    "sync"
)

func worker(num int, wg *sync.WaitGroup) {
    defer wg.Done()
    result := num * num
    fmt.Printf("The square of %d is %d\n", num, result)
}

func main() {
    var wg sync.WaitGroup

    numbers := []int{1, 2, 3, 4, 5}
    for _, num := range numbers {
        wg.Add(1)
        go worker(num, &wg)
    }

    wg.Wait()
}
 
