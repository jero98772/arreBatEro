input()
vowels="aeiouy"
word=input()
alert = True
newword=""
for i in word:
	if i in vowels and alert:
		newword+=i
		alert=False
	elif not i in vowels:
		newword+=i
		alert=True
print(newword)
		