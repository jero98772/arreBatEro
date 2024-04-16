value="M=-1"
for i in range(64*2+32):
	print("@"+str(16384+32*i+10))
	print(value)
	print("@"+str(16384+32*i+11))
	print(value)
	if 64<i<128-32:
		for j in range(10):
			print("@"+str(16384+32*i+6+j))
			print(value)

