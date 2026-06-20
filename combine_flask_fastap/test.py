from fastapi import FastAPI
from fastapi.middleware.wsgi import WSGIMiddleware
from flask import Flask

flask_app = Flask(__name__)


@flask_app.route("/legacy")
def legacy():
    return "Hello from Flask"


app = FastAPI()


@app.get("/fast")
async def fast_endpoint():
    return {"msg": "Hello from FastAPI"}


# Mount Flask at a sub-path, served by the same process
app.mount("/old", WSGIMiddleware(flask_app))
