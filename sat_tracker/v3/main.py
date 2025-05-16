from flask import Flask, render_template, jsonify, request  # Add request here
from flask_cors import CORS  # Import Flask-CORS
from datetime import datetime, timedelta
from tools.tools import get_satellite_object, get_current_position, predict_satellite_positions, calculate_footprint_radius, is_satellite_in_sunlight
from skyfield.api import load

app = Flask(__name__)
CORS(app)  # Enable CORS for the entire app
ts = load.timescale()

# Example TLE data - in a real application, you would fetch this dynamically
TLE_DATA = {
    "NOAA 15": [
        "NOAA 15",
        "1 25338U 98030A  24135.52397200  .00000156  00000+0  90386-4 0  9992",
        "2 25338  98.7639 106.1640 0009925 318.8307  41.2075 14.26057163339116"
    ],
    "NOAA 18": [
        "NOAA 18",
        "1 28654U 05018A  24135.54223295  .00000126  00000+0  93486-4 0  9998",
        "2 28654  99.0513  94.0011 0014456  70.0554 290.2117 14.12767638972404"
    ],
    "NOAA 19": [
        "NOAA 19",
        "1 33591U 09005A  24135.51878759  .00000146  00000+0  10189-3 0  9994",
        "2 33591  99.1608 112.9023 0013907 133.3462 226.8789 14.12675733793113"
    ]
}

SATELLITES = {
    name: get_satellite_object(tle[1], tle[2], name, ts)
    for name, tle in TLE_DATA.items()
}

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/satellites")
def get_satellites():
    satellite_list = []
    for name, tle in TLE_DATA.items():
        satellite_list.append({
            "name": name,
            "norad_id": tle[1][2:7].lstrip('0'), # Extract NORAD ID
            "line1": tle[1],
            "line2": tle[2]
        })
    return jsonify(satellite_list)

@app.route("/positions")
def get_positions():
    now = ts.now()
    position = get_current_position(
        satellite, 
        now,
        observer_lat=6.223301, 
        observer_lon=-75.5959321,
        observer_elevation_m=1695
    )
    for name, satellite in SATELLITES.items():
        position = get_current_position(satellite, now)
        altitude = position['altitude']
        positions_data.append({
            "norad_id": TLE_DATA[name][1][2:7].lstrip('0'),
            "latitude": position['latitude'],
            "longitude": position['longitude'],
            "altitude": altitude,
            "velocity": position['velocity'],
            "footprint_radius": calculate_footprint_radius(altitude),
            "visible": is_satellite_in_sunlight(satellite, now)
        })
    return jsonify(positions_data)

@app.route("/predict/<norad_id>")
def predict(norad_id):
    hours = float(request.args.get('hours', 24))
    points = int(request.args.get('points', 100))

    satellite_name = None
    tle1 = None
    tle2 = None
    for name, tle in TLE_DATA.items():
        if tle[1][2:7].lstrip('0') == norad_id:
            satellite_name = name
            tle1 = tle[1]
            tle2 = tle[2]
            break

    if tle1 and tle2:
        satellite = get_satellite_object(tle1, tle2, satellite_name, ts)
        now = ts.now()
        end_time = now + timedelta(hours=hours)
        predictions = predict_satellite_positions(satellite, now, end_time, points)
        return jsonify({"points": predictions})
    else:
        return jsonify({"error": "Satellite not found"}), 404

if __name__ == "__main__":
    app.run(debug=True, port=8000)

