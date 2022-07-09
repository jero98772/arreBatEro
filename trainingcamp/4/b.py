k,z=map(int,input().split())
arr=list(map(int,input().split()))
points=0
for i in arr:
	print(z-i,z,i)
	if z-i>=z:
		points+=1
print(points)
   