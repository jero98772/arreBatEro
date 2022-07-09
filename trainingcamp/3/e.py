input()
wordIUndestand=input().split()
numOfword=len(wordIUndestand)
tmpswords=[]
nameword=[]
for i in range(int(input())):
	alert=False
	word,num=input().split()
	words=input().split()
	for ii in words:
		if ii in wordIUndestand:
			alert=True
		else:
			alert=False
			break
	if alert:
		wordIUndestand.append(word)
	else:
		tmpswords.append(words)
		nameword.append(word)
#print(tmpswords,nameword)
tmpswords.reverse()
nameword.reverse()
#print(tmpswords,nameword)
for j in range(len(tmpswords)):
	alert2=False
	#print(j,tmpswords[j],nameword[j])
	for jj in tmpswords[j]:
		#print(jj)
		if jj in wordIUndestand:
			alert2=True
		else: 
			alert2=False
			break
	if alert2:
		wordIUndestand.append(nameword[j])
print(len(set(wordIUndestand)))

print(set(wordIUndestand))
print((wordIUndestand))