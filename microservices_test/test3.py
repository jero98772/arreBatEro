from fastapi import FastAPI, Request, Form, HTTPException, Response
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.templating import Jinja2Templates
import requests

app = FastAPI()

# Templates directory setup
templates = Jinja2Templates(directory="templates")

# URL endpoints of your microservices
PRODUCT_SERVICE_URL = 'http://localhost:5000'
ORDER_SERVICE_URL = 'http://localhost:5001'  # Assuming order service is on port 5001

# Define routes

@app.get("/", response_class=HTMLResponse)
async def read_root(request: Request):
    return templates.TemplateResponse("index.html", {"request": request})

@app.post("/create_order/", response_class=HTMLResponse)
async def create_order(product_id: int = Form(...), quantity: int = Form(...)):
    try:
        # Example of creating an order using Order Service and Product Service
        product_response = requests.get(f'{PRODUCT_SERVICE_URL}/products/{product_id}')
        product_response.raise_for_status()  # Raise an exception for bad responses

        product_data = product_response.json()
        order_data = {
            'product_id': product_id,
            'quantity': quantity,
            'product_name': product_data.get('name'),
            'product_price': product_data.get('price'),
        }
        print(order_data)
        # Assume order creation logic here with ORDER_SERVICE_URL

        return JSONResponse(content={"message": "Order created successfully"})
    
    except requests.exceptions.RequestException as e:
        # Handle any request exception, including JSONDecodeError
        raise HTTPException(status_code=500, detail=str(e))

# Static files (like CSS, JS) can be served with `app.mount()` if needed
@app.get("/orders", response_class=HTMLResponse)
async def get_orders(request: Request):
    try:
        order_response = requests.get(f'{ORDER_SERVICE_URL}/orders')
        order_response.raise_for_status()  # Raise an exception for bad responses

        orders = order_response.json()
        return templates.TemplateResponse("orders.html", {"request": request, "orders": orders})
    
    except requests.exceptions.RequestException as e:
        # Handle any request exception
        raise HTTPException(status_code=500, detail=str(e))

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
