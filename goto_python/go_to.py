import os
import sys


def readfilelist(name):
	"""
	readtxt(name) , return txt content as array ,element by line 
	"""
	content = []
	with open(name, 'r') as file:
		for i in file.readlines():
			content.append(str(i).replace("\n",""))
	return content

def go_to(line,actuall_line=None):
	filename = sys.argv[0]

	file=readfilelist(filename)
	file[actuall_line]="None"
	newfile=file+file[line:actuall_line-1]
	print(newfile)
	newcode=""
	for i in newfile:
		if "import" in i:
			pass
		else:
			newcode=newcode+i+"\n"
		#print(i)
		#try:

	eval(newcode)
	exit()
