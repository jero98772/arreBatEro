from flask import Flask

# Create a Flask application
app = Flask(__name__)

# Define a route for the homepage
@app.route('/')
def hello_world():
    return 'Hello, World!'

# Run the application if this script is executed
if __name__ == '__main__':
    app.run(debug=True)
