num=int(input())
dp=[]
for i in range(num+1):
	dp.append(-1)	
#def fib(num):
ans=0
fib1=1
fib2=1
for i in range(num+1):
	if dp[num]!=-1:
		ans=dp[num]
	else:
		tmp=fib1
		fib1=fib1+fib2
		fib2=tmp
		#print(fib1)
		dp[i]=fib1
		#return ans
#print(fib1)
print(dp[num],fib1)#,dp)