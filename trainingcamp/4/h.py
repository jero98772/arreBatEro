k,m=map(int,input().split())
arr=map(int,input().split())
cout=0
for i in arr:
	if i>m:cout+=1
print(cout)