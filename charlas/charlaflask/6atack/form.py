from flask import Flask,render_template,request,session
import os
import random
app=Flask(__name__)
FILEUPLOAD="static/uploads/"
key=random.randint(0,9000000000000)
app.secret_key = bin(key)#app needed for use session's (sessions are like cookies)

def getExt(filename):
	"""
	getExt(filename) return extencion of file 
	"""
	isPoint = False
	for i in str(filename):
		if i == ".":
			ext = "."
			isPoint = True
		elif isPoint:
			if i == "'":
				break
			ext += i
	return ext
	
@app.route("/",methods=['POST','GET'])
def index():
	try:
		session["username"]
	except:	
		session["username"]=""
	if request.method == "POST": #needen
		file = request.files["file"]
		ext = getExt(file)
		name = str(len(os.listdir(FILEUPLOAD)))+ext
		file.save(FILEUPLOAD+name)
		session["username"] = request.form["name"]
		#print(session["author"])
	return render_template("index.html")

@app.route("/r")
def sayhello():
	return "hello "+session["username"]

if __name__ == "__main__":
  app.run(host="0.0.0.0",port=9600)
