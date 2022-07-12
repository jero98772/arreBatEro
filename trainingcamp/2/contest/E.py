input()
name=input()
pokes=["vaporeon", "jolteon", "flareon", "espeon", "umbreon", "leafeon", "glaceon", "sylveon"]
pokesd={"vaporeon":0, "jolteon":0, "flareon":0, "espeon":0, "umbreon":0, "leafeon":0, "glaceon":0, "sylveon":0}
pokeocurrence=[]
#if name==""
onlysize=False
for i in pokes:
	ocur=0
	if len(name)==len(i):
		for ii in range(len(name)):
			if i[ii]==name[ii]:
				onlysize=True
				ocur+=1
		pokesd[i]=ocur
	pokeocurrence.append(ocur)





	"""ocur=0
	for ii in name:
		if ii in i:
			print(i,name)
			for iii in range(len(name)):
				if i[iii]==name[iii]:
					pokesd[i]+=1
			ocur+=1
	"""
maxx=0
pos=0
poket=""
for j in range(len(pokeocurrence)):
	if maxx<pokeocurrence[j]:
		#print(pokesd)
		maxx=pokesd[pokes[j]]
		pos=j
		poket=pokes[j]
		#maxx=pokeocurrence[j]


#print(pokes[pos])
#print(pokesd)
if onlysize:
	print(poket)
else:
	ss=list(map(len,pokes))
	#ss.count(len(name))==
	if ss.count(len(name))==1:
		for i in range(len(pokes)):
			if len(pokes[i])==len(name):
				print(pokes[i])