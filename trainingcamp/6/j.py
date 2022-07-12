n=input()
txt=input().split()
concidencias=1
for i in set(txt):
	if txt.count(i)>=2:
		concidencias+=1
print(concidencias,len(set(txt)))