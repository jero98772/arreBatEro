# product_service.py
from flask import Flask, jsonify

app = Flask(__name__)

# Mock product data
products = [
    {"id": 1, "name": "Product A", "price": 50.0},
    {"id": 2, "name": "Product B", "price": 75.0},
    {"id": 3, "name": "Product C", "price": 100.0},
]

@app.route('/products/<int:product_id>', methods=['GET'])
def get_product(product_id):
    print(f"get product {product_id}")
    for product in products:
        if product['id'] == product_id:
            return jsonify(product)
    return jsonify({"error": "Product not found"}), 404

if __name__ == '__main__':
    app.run(debug=True)
