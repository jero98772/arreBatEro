#a1,a2,a3,a4
d={}
a=True
a=list(map(int,input().split()))
for i in a:
	if i in d:
		d[i]+=1
	else:
		d[i]=1
for i in d.values():
	if i>=2 :
		a=False
		print("YES")
		break
if a:
	print("NO")