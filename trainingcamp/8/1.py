def solove():
	input()
	suma=0
	pointsdies=[0]
	magsources=list(map(int,input().split()))
	for i in magsources:
		suma+=i
		if suma<0:
			pointsdies.append(suma)
	print(-1*(min(pointsdies)))
		
		
solove()