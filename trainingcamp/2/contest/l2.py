input()
nums=map(int,input().split())

def istprime(n):
	sqr=n**(1/2)
	print(sqr)
	sqr3=n**(1/3)
	if n%sqr==0: return True
	#elif:
	else: return False

for i in nums:
	if i<3:
		print("NO")		
	elif istprime(i):
		print("YES")
	else :
		print("NO")