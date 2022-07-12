from random import randint

def f(i,suma1,suma2,a):
	if i==len(a):
		return suma1*suma2
	return max(f(i+1,suma1+a[i],suma2,a),f(i+1,suma1,suma2+a[i],a))
n=10
#global x
while 1:
	x=0
	a=[]
	for i in range(n):
		r=randint(1,n)
		a.append(r)
		print(r)
	a.sort(reverse=True)
	#print(a)
	suma1=0
	suma2=0
	for i in a:
		if suma1>suma2:
			suma2+=i
		else:
			suma1+=i
	#print(suma1,suma2)
	y=suma1*suma2
	suma1=0
	suma2=0
	sumatotal=sum(a)+1
	for i in a:
		if suma1+i<=sumatotal/2:
			suma1+=i
		else:
			suma2+=i
	#print(suma1,suma2,sumatotal)
	y=max(y,suma1*suma2)
	#print(y)

	
	x=f(0,0,0,a)
	if x==y:
		pass
	else:
		print(a)
		break
	#print()