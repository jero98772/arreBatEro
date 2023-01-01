import re
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

def main():
    functionstr=input()
    for i in range(len(functionstr)):
        if f1[i]==":" and fn:

if __name__ == '__main__':
    main()

iselse=False
fn=True
        pos=i+1
        f1=f1[:pos]+"\n\tif dp[n]!=er:\n\t\treturn dp[n]"+f1[pos:]
        pos=i-1
        f1=f1[:pos]+",dp,er="+str(er)+""+f1[pos:]
        fn=False
print(f1)


