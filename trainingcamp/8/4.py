input()
a=list(map(int,input().split()))
suma=0
a.sort()
for i in range(len(a)):
	print(a[i]-(i+1))
	if a[i]-(i+1)<0:
		print(0)
		break
	suma+=a[i]-(i+1)
print(suma)