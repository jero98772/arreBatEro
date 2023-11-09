from flask import Flask, jsonify
import tools.tools
app = Flask(__name__)

# Define a route
@app.route('/')
@app.route('/api/data')
def get_data():
    a=tools.sum_as_string(1,34)
    data = {'message': a}
    return jsonify(data)

if __name__ == '__main__':
    app.run(debug=True)
