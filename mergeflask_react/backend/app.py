from flask import Flask, request, jsonify
app = Flask(__name__)

@app.route('/api/receive_data', methods=['POST'])
def receive_data():
    data = request.json  # Assuming data is sent as JSON
    # Process the data and return a response if needed
    print(data)
    return jsonify({"message": "Data received successfully!"})


if __name__ == '__main__':
	app.run(debug=True)