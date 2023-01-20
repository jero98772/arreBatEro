from tkinter import *
from  tkinter.filedialog  import *
filename=None
def newFile():
	global filename
	filename="Untitled"
	text.delete(0.0,END)
def saveFile():
	global filename
	text=text.get(0.0,END)
	with open(filename,"w") as file:
		file.write(text)
		file.close()
def saveAs():
	file=ask_save_as_file(mode="w",default_extencion=".tx")
	text=text.get(0.0,END)
	try:
		file.write(t.rstrip())
	except:
		showerror(title="Opps",message="Unable to save")
def openFile():

	#file=ask_open_file(mode="r")
	f=input()
	text=f.read()
	text.delete(0.0,END)
	text.insert(0.0,text)
root=Tk()
root.title("my text editor")
root.minsize(width=400,height=400)
root.maxsize(width=400,height=400)
text=Text(root,width=400,height=400)
text.pack()
menubar=Menu(root)
filemenu=Menu(menubar)
filemenu.add_command(label="New",command=newFile)
filemenu.add_command(label="Open",command=openFile)
filemenu.add_command(label="Save",command=saveFile)
filemenu.add_command(label="SaveAs",command=saveAs)
filemenu.add_separator()
filemenu.add_command(label="quit",command=root.quit)
menubar.add_cascade(label="File",menu=filemenu)
root.config(menu=menubar)
root.mainloop()