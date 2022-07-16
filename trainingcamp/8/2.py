t=0
cola=0
for i in range(int(input())):
	m,vl=list(map(int,input().split()))
	#print(m)
	if i==0:
		m1=m
		vl1=vl
	if m==m1:
		cola+=vl1
	if m>cola:
		cola=vl
	m1=m
	vl1=vl
print(m,cola)

max()
"""
if max moment
max(moment)

momments equals
"""