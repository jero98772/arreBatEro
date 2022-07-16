t=0
cola=0
momensts=[]
vls={}
svls=0
for i in range(int(input())):
	m,vl=list(map(int,input().split()))
	momensts.append(m)
	svls+=vl
	if m in vls:
		vls[m]+=vl
	else:
		vls[m]=vl
mm=max(momensts)
print(mm+vls[mm])
print(svls)
"""


(1)if momento<videolength momento<cola:
	momento+videolength

(2)if momento varios:
	sum(momento varios.video length)

(3)if momento>videolength y cola:
	momento



11
1 4
-> 5 (1)

3 3
-> 8 (3)

6 1
-> 9 (1)

10 2 (3)
-> 12

10 3 (2)
-> 15


9 10 (1)
-> 19



110 9
110 9
110 9
110 9
111 5
111 7

6 1
10 2
10 3

"""