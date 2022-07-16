input()
a=list(map(int,input().split()))
suma=0
#a.sort()
ii=1
for i in range(len(a)):
	#print(a[i]-(i+1))
	if a[i]-(ii)<=0:
		#print(0)
		#break
		continue
	else:
		suma+=a[i]-(ii)
		ii+=1
print(suma)