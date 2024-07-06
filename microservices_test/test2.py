from flask import Flask, jsonify, request
import requests  # Import requests library for making HTTP requests

app = Flask(__name__)

# Initialize an empty list for orders
orders = []

# URL of the Product Service
PRODUCT_SERVICE_URL = 'http://localhost:5000'

@app.route('/orders', methods=['POST'])
def create_order():
    data = request.json
    product_id = data.get('product_id')
    quantity = data.get('quantity')
    
    # Fetch product details from Product Service
    product_response = requests.get(f'{PRODUCT_SERVICE_URL}/products/{product_id}')
    if product_response.status_code == 200:
        product_data = product_response.json()
        # Assume product_data contains product information
        order = {
            'product_id': product_id,
            'quantity': quantity,
            'product_name': product_data.get('name'),
            'product_price': product_data.get('price'),
        }
        orders.append(order)  # Append the new order to the orders list
        print("Updated orders:", orders)  # Print the updated orders array
        return jsonify({"message": "Order created successfully"})
    else:
        return jsonify({"error": "Product not found"}), 404

@app.route('/orders', methods=['GET'])
def get_orders():
    return jsonify(orders)

if __name__ == '__main__':
    app.run(debug=True, port=5001)
