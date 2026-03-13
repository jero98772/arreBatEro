from flask import Flask, render_template, request, jsonify

app = Flask(__name__)

# In-memory data store (replace with database if needed)
items = []

@app.route('/')
def index():
    return render_template('index.html', items=items)

@app.route('/add', methods=['POST'])
def add_item():
    item = request.form.get('item')
    if item:
        items.append(item)
    return render_template('index.html', items=items)

@app.route('/delete/<int:index>', methods=['POST'])
def delete_item(index):
    if 0 <= index < len(items):
        items.pop(index)
    return render_template('index.html', items=items)

if __name__ == '__main__':
    app.run(debug=True)