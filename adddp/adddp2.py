def fibdp(n,dp):
    if dp[n]!=-1:
        return dp[n]
    if n<=2:
        return n
    else:
        ans=fib(n-2)+fib(n-1)
        dp[n]=ans
        return ans
        
def adddpaux(n,f=None):
    tmp=f.__name__
    f.__name__=adddp.__name__
    adddp.__name__=tmp
    #return f(n)

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

n=int(input())
dp=[]

for i in range(n+3):
    dp.append(-1)  

adddp(dp)


#adddp(fib(n),dp)

#"""
