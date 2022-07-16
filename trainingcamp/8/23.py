sumas=0
cola=0
m1=-1
for i in range(int(input())):
	m,vl=list(map(int,input().split()))
	a=False
	"""if #m<vl:
		cola+=m+vl
		print("->1")"""
	if m<cola or m1==m or m==cola:#121 122
		cola+=vl
		#print("->2")
	if m>=cola:
		cola=m+vl
		#print("->3")
	if m1==m or m==cola:
		#print(vl)
		#print(cola,"==",m,m1)
		cola+=vl
		a=True
	m1=m
if a:
	print(cola-vl)
else:
	print(cola)
	
"""
## formula y modulo
(1)if momento<videolength momento<cola:
	momento+videolength

(2)if momento varios:
	sum(momento varios.video length)

(3)if momento>videolength y cola:
	momento
m<vl and m<cola
1 4
5
4 5 , 

11
1 4 m<vld
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

20 21 (1)
-> 41


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