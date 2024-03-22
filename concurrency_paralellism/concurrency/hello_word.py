import threading
import time 

def hello():
		for i in range(50):
			print("hello from therad!",i)
			time.sleep(0.00005)
thread= threading.Thread(target=hello)
thread.start()
for i in range(20):
    print("Hello from main!", i)
    time.sleep(0.0001)

thread.join()
