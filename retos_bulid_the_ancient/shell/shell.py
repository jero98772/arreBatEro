import subprocess
import os
while True:
	current=os.getcwd()
	txt=input(str(current)+">").replace("\n","").split()
	print(txt)
	if txt[0]=="cd":
		
		if txt[1]=="..":
			if current.find("/",len(current)-1)==0 or current.find("/",len(current)-1)==-1 :current="/"
			else: current=current[0:current.find("/",len(current)-1)]
			print(current)
			os.chdir(current)		
		else:
			current+="/"+txt[1]
			os.chdir(current)
	subprocess.run(txt,shell="True")
	