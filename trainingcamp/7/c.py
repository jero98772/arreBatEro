def searchball(target,ballsizes):
	l=0
	r=len(ballsizes)
	for i in ballsizes:
		mid=int(l+(r-l)/2)
		#print(mid)
		#print(l,r,mid,ballsizes[mid])
		if ballsizes[mid]==target:
			#print("mid")
			return ballsizes[mid],mid
		elif target>ballsizes[mid]:
			#print("<")
			l=mid+1
		elif target<ballsizes[mid]:
			r=mid-1
			#print(">")
		else:
			#print("else",ballsizes[l],mid)
			return l,mid
	return ballsizes[l],mid
def solove():
	n,k=map(int,input().split())
	ballsizes=list(map(int,input().split()))
	ballsizes.sort()
	#print(ballsizes)
	n-=1
	target=ballsizes[n]//2
	b=1
	h=0
	poss=[n]
	#print("target",target)
	for i in range(n):
		for _ in range(k):
			if len(ballsizes)!=0:
				ball,pos=searchball(target,ballsizes)
				print("la,",ball,target,"<",ballsizes)
				if ball<=target:
					print("pos")
					b+=1
					poss.append(pos)
					print(poss)
				if b==k:
					print("se")
					h+=1
					#poss.sort(reverse=True)
					print(poss)
					for ii in range(k):
						print("pop",ii,poss[ii],ballsizes)
						ballsizes.pop(poss[ii])
					poss=[]
					b=1
				print(b,ballsizes)
				target=ball//2
			#else:
				#break
			#b+=1
	print("end",poss,ballsizes)
	return h
for i in range(int(input())):
	print("Case #"+str(i+1)+" : "+str(solove()))

"""
ordeno el arreglo para aplciar un binari search escogiendo el elemento menor igual a la mitad eliminelo y repita el proceso k veces 
"""