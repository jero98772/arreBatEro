def aplayformat(s):
	if s<10:
		return "0"+str(s)
	else:
		return str(s)
def prietopal(t):
	if t[0]==t[4] and t[3]==t[1]:
		return True
	else:
		return False
def checkpalindrom(t):
	t=str(t)
	if t==t[::-1]:
		return True
	else:
		return False
		
def updatetime(time,mins):
		hh,mm=map(int,time.split(":"))
		#print(hh,mm)
		hh=(hh+(mins+mm)//60)%24
		mm=(mm+mins)%60
		return aplayformat(hh)+":"+aplayformat(mm)
		
tests=input()
for _ in range(int(tests)):
	timestr,minutes=input().split()
	minutes=int(minutes)
	pals=0
	time=timestr

	if checkpalindrom(timestr):
		pals+=1
	timestr=updatetime(timestr,minutes)
	
	while timestr!=time:
		if checkpalindrom(timestr):
			#print(timestr,"<-----")
			pals+=1
		#print(timestr)
		timestr=updatetime(timestr,minutes)
	print(pals)



"""
	low=mm+(hh*60)
	print(low)
	time=timestr
	while time==timestr:
	#for i in range(low,1440+low,int(minutes)):
		i+
		hh=(//60)%24
		mm=(mm+i)%60
		time=aplayformat(hh)+":"+str(mm)
		print(time)
		if(checkpalindrom(time)):
			pals+=1
		#if totaltime>1440:
	print(pals)"""