alph="abcdefghijklmnopqrstuvwxyz"
input()
txt=input().lower()
for i in txt:
    alph=alph.replace(i,"")
if alph=="":
    print("YES")
else:
    print("NO")
