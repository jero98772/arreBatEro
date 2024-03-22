import multiprocessing

def worker(num):
	answer=num*num
	print(num,answer)

if __name__ == "__main__":
	pool = multiprocessing.Pool()

	numbers=[1,2,3,4,5]
	pool.map(worker,numbers)

	pool.close()
	pool.join() 
