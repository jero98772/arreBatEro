rows=input()
s=""
for i in range(int(rows)):
	s+=input()+"\n"
if "OO" in s:
	print("YES")
	print(s.replace("OO","++",1)[:-1])
else:
	print("NO")