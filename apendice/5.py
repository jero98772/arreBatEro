
def portions(A, B, a, b, n):

    if A / B == a / b:
        return n
    else:
        if a > A or b > B:
            return float('inf')
        return min(portions(A, B, a + b + a, b, n + 1), portions(A, B, a, a + b + b, n + 1))


def portions(A, B, a, b, n,dp):
    if dp[n]!=-1:
        return dp[n]
    if A / B == a / b:
        ans=n
        dp[n]=ans
        return ans
    else:
        if a > A or b > B:
            return float('inf')
        return min(portions(A, B, a + b + a, b, n + 1,dp), portions(A, B, a, a + b + b, n + 1,dp))
        #return ans

t = int(input())
for x in range(t):
    dp=[]
    a, b, i, j = map(int, input().split())
    for i in range(10):
        dp.append(-1)
    s = portions(a, b, 1, 1, 2,dp)
    if s == float('inf'):
        s = -1
    print(s)
    print(dp)