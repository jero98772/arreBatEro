from Flask import Flask
from subprocess import run
app=Flask(__name__)

FILETEST="data/test.txt"
CODE="data/code.py"
def wr(name,conent,mode):
	with open(name,mode) as f:
		f.write(conent)
		f.close()
def read()

@app.route("/", methotds=["POST","GET"]) 
def testcases():
	if request.methot=="POST":
		code=request.from["code"]
		wr(code)
	
@app.route("/", methotds=["POST","GET"]) 
def index():
	if request.methot=="POST":
		code=request.from["code"]
		wr()
	
	return render_template("index.html")
if __name__=="__main__":
	app.run()