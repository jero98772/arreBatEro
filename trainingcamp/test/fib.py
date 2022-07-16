num=int(input())
dp=[]
for i in range(num+2):
	dp.append(-1)	
def fib(num):
	if dp[num]!=-1:
		return dp[num]
	if num<=2:
		return num
	else:
		ans= fib(num-1)+fib(num-2)
		dp[num]=ans
		return ans
print(fib(num))