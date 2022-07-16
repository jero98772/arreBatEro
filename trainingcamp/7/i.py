def distance(s1,s2,ss):
	dis=ss
	alert=False
	cons=""
	ws=[]
	for i in range(len(s1)):
		#print(s1[i])
		if s1[i]==s2[i]:
			dis-=1
			cons+=s1[i]
			alert=True
		#elif alert:
			#ws.append(cons)
		else:
			cons+=""
			#alert=False
	#print("\n",cons,"\n")
	return dis,cons

s=int(input())
a=input()
b=input()
c=input()
dis1,w1=distance(a,b,s)
dis2,w2=distance(c,a,s)
dis3,w3=distance(b,c,s)
ans=""
if dis1<dis2 and dis1<dis3:
	#print("d1")#c
	ans=c[:s-len(w1)]+w1
if dis2<dis1 and dis2<dis3:
	ans=b[:s-len(w2)]+w2


if dis3<dis1 and dis3<dis2:
	#print("d3")#a
	ans=a[:s-len(w3)]+w3
print(ans)
#if w1=="_"*s:

#print(dis1,dis2,dis3,min(dis1,dis2,dis3))
#print(w1,w2,w3)