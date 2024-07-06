# order_service.py
from flask import Flask, jsonify, request

app = Flask(__name__)

# Mock order data
orders = []

@app.route('/orders', methods=['POST'])
def create_order():
    data = request.json
    orders.append(data)
    return jsonify({"message": "Order created successfully"})

@app.route('/orders', methods=['GET'])
def get_orders():
    return jsonify(orders)

if __name__ == '__main__':
    app.run(debug=True,port ="5001")
