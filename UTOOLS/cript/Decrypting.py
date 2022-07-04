from collections import deque
abc="abcdefghijklmnñopqrstuvwxyz"
def cifrarcesar(text,key, chars=abc):
    cifrar = ""
    text = str(text)
    for char in text:
            num = chars.find(char) + key
            mod = int(num) % len(chars)
            cifrar = cifrar + (chars[mod])
    return  str(cifrar) 
def descifrarcesar(text,key,chars=abc):
    descifrar = ""
    text = str(text)
    for char in text:
            num = chars.find(char ) - key
            mod = int(num) % len(chars)
            descifrar = descifrar + str(chars[mod])
    return str(descifrar)
def cifswap(passsawp,cripttxt):
    txt=deque(cripttxt)
    for i in range(passsawp):
        #print(txt)
        txt.appendleft(txt.pop())
    return txt
def decifswap(passsawp,cripttxt):
    txt=deque(cripttxt)
    for i in range(passsawp):
        txt.append(txt.popleft())
    return txt
def main():
    option=input("option encript[E] or decript[D]\n")
    text=input("input text\n")
    password=int(input("input number 0 to "+str(len(abc))+"\n"))
    swap=password%len(text)
    if option.upper()=="E" or option.lower()=="encript":
        txtcif=cifrarcesar(text,password, chars=abc)
        print(txtcif,swap)
        txt=cifswap(swap,txtcif)
        print(txt)
    if option.upper()=="D" or option.lower()=="decript":
        txtdecif=descifrarcesar(text,password, chars=abc)
        txt=decifswap(swap,txtdecif)
        print(txt)
    """decifer=deque(input())
	ciferword=input()
	difs=[]
	print(ciferword)
	abc="abcdefghijklmnñopqrstuvwxyz"
	tmp=ciferword
	maxnum=0
	for i in range(len(abc)):#cesar
		print("cesar")
		ciftmp=cifrarcesar(tmp,i,abc)

		tmp2=deque(ciftmp)
		for ii in range(len(ciferword)):#swap
			#print(tmp2)			
			tmp2=swap(tmp2)
			print(tmp2)
			difs.append()
			maxnum=max(maxnum,difwords(tmp2,decifer))
	print(maxnum)
	"""
main()
