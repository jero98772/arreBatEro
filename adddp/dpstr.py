def adddp(n,dp,f=None):
    if dp[n]!=-1:
        return dp[n]
    else:   
        ans=f(n)
        dp[n]=ans
        return ans

def fib(n):
    if n<=2:
        return n
    else:
        return fib(n-2)+fib(n-1)

f1="""
	def fib(n):
		  if n<=2:
		      return n
		  else:
		      return fib(n-2)+fib(n-1)
"""
namef=f.__name__
tmp=""
for i in f1:
    token+=i
    if i==""
    if token=="def " 
        token=""
        namef
posf=f1.find(":")
fname=
elsepos=f1.find("else:")
f1[:pos]+"""\n    if dp[n]!=-1:
        return dp[n]"""+f1[pos:]
#adddp(fib(n),dp)

#"""
