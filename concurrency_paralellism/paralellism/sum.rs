use std::thread;

fn parallel_sum(arr: &[i32]) -> i32 {
    const NUM_THREADS: usize = 4; // Number of threads for parallel processing
    let chunk_size = arr.len() / NUM_THREADS; // Divide the array into equal chunks
    
    let mut handles = vec![];
    let mut sums = vec![0; NUM_THREADS]; // Store partial sums for each chunk
    
    for (i, chunk) in arr.chunks(chunk_size).enumerate() {
        let handle = thread::spawn(move || {
            let sum = chunk.iter().sum(); // Calculate sum of the chunk
            sums[i] = sum; // Store the partial sum
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap(); // Wait for all threads to finish
    }

    // Calculate total sum by summing up partial sums
    let total_sum: i32 = sums.iter().sum();
    total_sum
}

fn main() {
    let arr = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let sum = parallel_sum(&arr);
    println!("{}", sum);
}

