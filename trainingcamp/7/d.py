for i in range(int(input())):
	sis=int(input())-1
	p=list(map(int,input().split()))
	#p.reverse()
	treefree=[]
	suma=0
	count=0
	totalprice=0
	#gropitos de 3
	m=p[sis]
	for ii in range(len(p)):
		#treefree.append(p)
		#print(sis-ii,m)
		if p[sis-ii]<m:
			m=p[sis-ii]
		suma+=p[sis-ii]
		count+=1
		if count%3==0:
			#m=min(treefree)
			totalprice=+suma-m
			m=10**9
			suma=0
			count=0
	else:
		if treefree!=0:
			totalprice+=suma
	print("Case #"+str(i+1)+": "+str(totalprice))
	
#sort
#min