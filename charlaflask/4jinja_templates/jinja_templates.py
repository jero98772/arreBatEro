from flask import Flask,render_template
app=Flask(__name__)
@app.route("/")
@app.route("/a.html")
def a():
	return render_template("a.html")
@app.route("/b.html")
def b():
	return render_template("b.html")
if __name__ == "__main__":
  app.run(host="0.0.0.0",port=9600)
