import os
def readtxtstr(name):
	"""
	readtxtstr(name) , return txt content as string
	"""
	content = ""
	with open(name, 'r') as file:
		for i in file.readlines():
			content += str(i)
	return content
def write(txt,filename,mode):
	with open(filename,mode) as file:
		file.write(txt)
		file.close()

#content=readtxtstr("bla.txt")
#write("#bla by bla\n"+content,"bla.txt","w")
#def writeFiles(path):
def writeFiles(header,actualdir=os.getcwd()):
	print(actualdir)
	paths=os.listdir(actualdir)
	for i in paths: 
		print(i,actualdir+i)
		if os.path.isdir(actualdir+i):
			print("folder")
			writeFiles(header,actualdir+i+"/")
		if ".py" in i:
			content=readtxtstr(actualdir+i)
			write('"""\n'+header+'\n"""\n'+content,actualdir+i,"w")
		if ".html" in i:
			content=readtxtstr(actualdir+i)
			write('<!--\n'+header+'\n-->\n'+content,actualdir+i,"w")
		

def main():
	header=input()
	writeFiles(header,os.getcwd()+"/test/")
main()
"""
rec()
if file-> write
if folder-> rec()



"""