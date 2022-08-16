def adddp(f,n,er=-1,m=10):
    dp=[er]*(n+m)
    return adddpaux(f,n,dp,er=-1,m=10)

def adddpaux(f,n,dp,er=-1,m=10):
    # = newname
    if dp[n]!= er:
        return dp[n]
    num=f(n)
    dp[n]=num
    return dp[n]


def adddp2(f):
    def dps(n,er=-1,m=10):
        dp=[er]*(n+m)
        #print(n,dp[n])
        if dp[n]!= er:
            return dp[n]
        #else:
        num=f(n)
        #print(num)
        dp[n]=num
        #print(dp)
        return dp[n]
    return dps

def catalan(n):
    if n<=1:
       return 1
    res=0
    for i in range(n):
      res+= catalan(i)*catalan(n-i-1)
    return res

def fib(n):
    if n<=2:
        return n
    else:
        return fib(n-2)+fib(n-1)

def adpaux(f,n):
    dp=[]
    for i in range(n+3):
        dp.append(-1)   
    adp.__name__=f.__name__
    return adp(f,n,dp)

def adp(f,n,dp):
    if dp[n]!=-1:
        return dp[n]
    else:
        ans=f(n)
        dp[n]=ans
        return ans


def catalandp(n):
    if dp[n]!=-1:
        return dp[n]
    if n<=1:
       return 1
    res=0
    for i in range(n):
      res+= catalandp(i)*catalandp(n-i-1)
    dp[n]=res
    return res


def fibdp(num):
    if dp[num]!=-1:
        return dp[num]
    if num<=2:
        return num
    else:
        ans= fibdp(num-1)+fibdp(num-2)
        dp[num]=ans
        return ans



n=int(input())
dp=[]
for i in range(n+3):
    dp.append(-1)   
    
#a=fibdp(n)
#d=catalandp(n)
b=adpaux(fib,n)

#c=adp(catalan,n);print(c)

##decoratos?
print(b)


def dpfaux(n,f):
    dp=[]
    for i in range(n+3):
        dp.append(-1)   
    

def dpf(n,f,dp):
    if dp[n]!=-1:
        return dp[n]
    else:
        ans=f(n)
        dp[n]=ans
        return ans









