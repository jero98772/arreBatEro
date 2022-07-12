alph="ROYGBIV"
conc=""
num=int(input())
for i in range(num):
	if i%num/2==0:
		conc+=alph[(i%4)%len(alph)]
	else:
		conc+=alph[i%len(alph)]
print(conc)