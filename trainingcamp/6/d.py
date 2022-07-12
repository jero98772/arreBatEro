txt=input()
lows=0
upps=0
for i in txt:
	if i.isupper():
		upps+=1
	if i.islower():
		lows+=1

if upps==lows:
	print(txt.lower())
if upps>lows:
	print(txt.upper())
if lows>upps:
	print(txt.lower())