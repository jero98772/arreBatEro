t=input()
txts=""
count=0
for i in t:
	if i in txts:
		pass
	else:
		txts+=i
		count+=1
if count %2==0:
	print("CHAT WITH HER!")
else:
	print("IGNORE HIM!")	