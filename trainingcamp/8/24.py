cola=0
for i in range(int(input())):
	m,vl=list(map(int,input().split()))
	#print(m)
	if m>cola:
		cola=m+vl
	else:
		cola+=vl
print(cola)