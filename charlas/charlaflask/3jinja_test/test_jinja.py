from flask import Flask,render_template
app=Flask(__name__)
@app.route("/")
def index():
	msg="yesus"
	return render_template("index.html",msg=msg)
if __name__ == "__main__":
  app.run(host="0.0.0.0",port=9600)
