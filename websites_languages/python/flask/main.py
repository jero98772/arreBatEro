from flask import Flask

app = Flask(__name__)

@app.route('/')
def home():
    return "Hello, World!"

@app.route('/status')
def status():
    return "Server is running!"

if __name__ == '__main__':
    app.run(debug=True)
