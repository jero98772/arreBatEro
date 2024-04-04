import multiprocessing

def parallel_sum(arr):
    num_processes = multiprocessing.cpu_count()
    chunk_size = len(arr) // num_processes
    
    def sum_worker(chunk):
        return sum(chunk)

    with multiprocessing.Pool(processes=num_processes) as pool:
        chunks = [arr[i:i+chunk_size] for i in range(0, len(arr), chunk_size)]
        results = pool.map(sum_worker, chunks)
    
    return sum(results)

if __name__ == "__main__":
    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    print(parallel_sum(arr))
