s=input()
l=s[0]
for i in s:
	if i==l:
		ans=len(s)-1
	else:
		print(0)
		ans=0
		break
if ans!=0:
	print(ans)