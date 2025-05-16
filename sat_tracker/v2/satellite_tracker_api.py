"""
NOAA Satellite Tracker - FastAPI Backend
This module provides satellite tracking functionality using the SGP4 propagator
"""

import asyncio
from datetime import datetime, timedelta
import json
from typing import List, Dict, Optional

import aiohttp
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import numpy as np
from skyfield.api import load, wgs84, EarthSatellite
from skyfield.timelib import Time

app = FastAPI(title="NOAA Satellite Tracker API")

# Add CORS middleware to allow requests from frontend
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # In production, replace with actual origin
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# TLE (Two-Line Element) data structure
class TLEData(BaseModel):
    name: str
    line1: str
    line2: str
    norad_id: Optional[int] = None

# Satellite position model
class SatellitePosition(BaseModel):
    name: str
    norad_id: int
    timestamp: float
    latitude: float
    longitude: float
    altitude: float
    velocity: float
    footprint_radius: float
    visible: bool

# Prediction point model
class PredictionPoint(BaseModel):
    timestamp: float
    latitude: float
    longitude: float
    altitude: float
    visible: bool

# Prediction data model
class SatellitePrediction(BaseModel):
    name: str
    norad_id: int
    points: List[PredictionPoint]

# Cache for TLE data
tle_cache = {}
last_tle_update = datetime.now() - timedelta(days=1)  # Initialize to force first update

# NOAA satellite NORAD IDs
NOAA_SATELLITES = {
    "NOAA 15": 25338,
    "NOAA 18": 28654,
    "NOAA 19": 33591
}

async def update_tle_data():
    """Fetch the latest TLE data from Celestrak"""
    global tle_cache, last_tle_update
    
    # Only update TLE data once a day
    if datetime.now() - last_tle_update < timedelta(hours=24) and tle_cache:
        return
    
    try:
        async with aiohttp.ClientSession() as session:
            # Get NOAA weather satellite TLEs
            url = "https://celestrak.org/NORAD/elements/gp.php?GROUP=noaa&FORMAT=tle"
            async with session.get(url) as response:
                if response.status != 200:
                    raise HTTPException(status_code=503, detail="Failed to fetch TLE data")
                
                tle_text = await response.text()
                
                # Parse TLE data
                lines = tle_text.strip().split('\n')
                
                # Process TLE data in groups of 3 lines
                for i in range(0, len(lines), 3):
                    if i + 2 < len(lines):
                        name = lines[i].strip()
                        line1 = lines[i + 1].strip()
                        line2 = lines[i + 2].strip()
                        
                        # Extract NORAD ID from line 1 (positions 3-7)
                        try:
                            norad_id = int(line2[2:7])
                            tle_cache[norad_id] = TLEData(
                                name=name,
                                line1=line1,
                                line2=line2,
                                norad_id=norad_id
                            )
                        except ValueError:
                            print(f"Failed to parse NORAD ID for satellite: {name}")
        
        last_tle_update = datetime.now()
    except Exception as e:
        print(f"Error updating TLE data: {e}")
        if not tle_cache:
            # If cache is empty, this is a critical error
            raise HTTPException(status_code=503, detail=f"Failed to fetch TLE data: {str(e)}")

@app.on_event("startup")
async def startup_event():
    """Initialize TLE data on startup"""
    await update_tle_data()

@app.get("/satellites", response_model=List[TLEData])
async def get_satellites():
    """Get all available NOAA satellites"""
    await update_tle_data()
    
    # Filter for NOAA 15, 18, and 19
    noaa_satellites = [
        tle for norad_id, tle in tle_cache.items()
        if norad_id in NOAA_SATELLITES.values()
    ]
    
    return noaa_satellites

@app.get("/satellites/{norad_id}", response_model=TLEData)
async def get_satellite(norad_id: int):
    """Get TLE data for a specific satellite"""
    await update_tle_data()
    
    if norad_id not in tle_cache:
        raise HTTPException(status_code=404, detail=f"Satellite {norad_id} not found")
    
    return tle_cache[norad_id]

@app.get("/positions", response_model=List[SatellitePosition])
async def get_current_positions():
    """Get current positions of all NOAA satellites"""
    await update_tle_data()
    
    positions = []
    
    # Load time scale and calculate current time
    ts = load.timescale()
    t = ts.now()
    
    # Calculate position for each NOAA satellite
    for name, norad_id in NOAA_SATELLITES.items():
        if norad_id in tle_cache:
            tle = tle_cache[norad_id]
            
            # Create Skyfield satellite object
            satellite = EarthSatellite(tle.line1, tle.line2, tle.name, ts)
            
            # Get geocentric position
            geocentric = satellite.at(t)
            subpoint = wgs84.subpoint(geocentric)
            
            # Calculate satellite speed (m/s)
            velocity = np.linalg.norm(geocentric.velocity.km * 1000)
            
            # Calculate if satellite is in sunlight
            sunlit = satellite.at(t).is_sunlit(load('de421.bsp'))
            
            # Calculate footprint radius (km)
            altitude = subpoint.elevation.km
            footprint_radius = 12.756 * np.sqrt(altitude / (12.756 + altitude))
            
            positions.append(SatellitePosition(
                name=name,
                norad_id=norad_id,
                timestamp=t.utc_datetime().timestamp(),
                latitude=subpoint.latitude.degrees,
                longitude=subpoint.longitude.degrees,
                altitude=altitude,
                velocity=velocity,
                footprint_radius=footprint_radius,
                visible=bool(sunlit)
            ))
    
    return positions

@app.get("/predict/{norad_id}", response_model=SatellitePrediction)
async def predict_passes(norad_id: int, hours: int = 24, points: int = 100):
    """Predict satellite positions for the next specified hours"""
    await update_tle_data()
    
    if norad_id not in tle_cache:
        raise HTTPException(status_code=404, detail=f"Satellite {norad_id} not found")
    
    # Find satellite name from NOAA_SATELLITES
    satellite_name = next((name for name, sat_id in NOAA_SATELLITES.items() if sat_id == norad_id), "Unknown")
    
    # Load time scale
    ts = load.timescale()
    
    # Generate time points for prediction
    start_time = ts.now()
    end_time = ts.now() + hours / 24  # Convert hours to days
    
    times = ts.linspace(start_time, end_time, points)
    
    # Create satellite object
    tle = tle_cache[norad_id]
    satellite = EarthSatellite(tle.line1, tle.line2, tle.name, ts)
    
    # Calculate positions
    prediction_points = []
    
    for t in times:
        # Get position
        geocentric = satellite.at(t)
        subpoint = wgs84.subpoint(geocentric)
        
        # Check if satellite is in sunlight
        sunlit = satellite.at(t).is_sunlit(load('de421.bsp'))
        
        prediction_points.append(PredictionPoint(
            timestamp=t.utc_datetime().timestamp(),
            latitude=subpoint.latitude.degrees,
            longitude=subpoint.longitude.degrees,
            altitude=subpoint.elevation.km,
            visible=bool(sunlit)
        ))
    
    return SatellitePrediction(
        name=satellite_name,
        norad_id=norad_id,
        points=prediction_points
    )

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
