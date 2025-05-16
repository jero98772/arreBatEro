import numpy as np
from datetime import datetime, timedelta
from skyfield.api import load, wgs84, EarthSatellite
from skyfield.toposlib import Topos

def get_satellite_object(tle_line1, tle_line2, name="Satellite", ts=None):
    """
    Creates a Skyfield EarthSatellite object from two TLE lines.
    """
    if ts is None:
        ts = load.timescale()
    return EarthSatellite(tle_line1, tle_line2, name, ts)

def get_current_position(Satellite, timestamp,observer_lat=6.223301, observer_lon=-75.5959321, observer_elevation_m=1695):
    """
    Calculates the latitude, longitude, altitude, velocity of a satellite
    and its altitude and azimuth as seen from a specific ground observer
    at a given timestamp.
    """
    ts = load.timescale()
    geocentric = satellite.at(timestamp)
    subpoint = wgs84.subpoint(geocentric)
    velocity = np.linalg.norm(geocentric.velocity.km_per_s)

    observer = wgs84.latlon(observer_lat, observer_lon, observer_elevation_m)
    astrometric = observer.at(timestamp).observe(satellite)
    alt, az, distance = astrometric.altaz()

    return {
        "latitude": subpoint.latitude.degrees,
        "longitude": subpoint.longitude.degrees,
        "altitude": subpoint.elevation.km,
        "velocity": velocity * 1000,
        "distance": distance.km,
        "alt": alt.degrees,
        "az": az.degrees,
    }

def predict_satellite_positions(satellite, start_time, end_time, steps):
    """
    Predicts the positions of a satellite over a given time period.
    """
    ts = load.timescale()
    times = ts.linspace(start_time, end_time, steps)
    positions = []
    for t in times:
        geocentric = satellite.at(t)
        subpoint = wgs84.subpoint(geocentric)
        positions.append({
            "latitude": subpoint.latitude.degrees,
            "longitude": subpoint.longitude.degrees,
            "altitude": subpoint.elevation.km,
            "timestamp": t.utc_iso(),
        })
    return positions


def calculate_footprint_radius(altitude_km):
    """
    Approximates the Earth coverage radius of a satellite based on its altitude.
    Uses the formula: R_earth * arccos(R_earth / (R_earth + altitude))
    """
    R_earth = 6371  # Earth's radius in km
    angle = np.arccos(R_earth / (R_earth + altitude_km))
    footprint_radius = R_earth * angle
    return footprint_radius


def is_satellite_in_sunlight(satellite, timestamp):
    """
    Determines if a satellite is in sunlight at a given timestamp.
    """
    eph = load("de421.bsp")
    sun = eph["sun"]
    earth = eph["earth"]
    astrometric = earth.at(timestamp).observe(sun)
    apparent = astrometric.apparent()
    illuminated = earth.at(timestamp).is_sunlit(satellite, apparent)
    return illuminated
