"""
NOAA Satellite Tracker - Project Setup Guide
"""

# NOAA Satellite Tracker Setup Guide

This guide will help you set up and run the NOAA Satellite Tracker application, which consists of a FastAPI backend and an HTML/JavaScript frontend using MapLibre GL.

## Prerequisites

- Python 3.8 or higher
- pip (Python package manager)
- A web browser that supports MapLibre GL

## Installation

### Backend Setup

1. Create a virtual environment (recommended):
   ```bash
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   ```

2. Install required Python packages:
   ```bash
   pip install fastapi uvicorn skyfield numpy aiohttp
   ```

3. Save the backend code to a file named `satellite_tracker_api.py`.

### Frontend Setup

1. Save the frontend HTML/JavaScript code to a file named `index.html`.

2. Get a free MapTiler API key:
   - Visit [MapTiler](https://www.maptiler.com/cloud/) and sign up for a free account
   - Create an API key
   - Replace `get_your_own_key` in the HTML file with your actual API key

## Running the Application

1. Start the FastAPI backend server:
   ```bash
   uvicorn satellite_tracker_api:app --reload --host 0.0.0.0 --port 8000
   ```

2. Open the `index.html` file in your web browser:
   - You can use any local web server or simply open the file directly
   - For a simple local server, you can use Python's built-in server:
     ```bash
     python -m http.server 8080
     ```
     Then open http://localhost:8080 in your browser

## How It Works

### Backend (FastAPI)

The backend provides the following API endpoints:

- `/satellites`: Returns information about all available NOAA satellites (15, 18, 19)
- `/satellites/{norad_id}`: Returns TLE data for a specific satellite
- `/positions`: Returns current positions of all NOAA satellites
- `/predict/{norad_id}`: Generates orbit predictions for a satellite

The backend fetches TLE data from Celestrak and uses the Skyfield library to perform precise orbital calculations.

### Frontend (MapLibre GL)

The frontend provides:

- Interactive map displaying satellite positions and coverage areas
- Control panel showing satellite information and status
- Ability to track and follow individual satellites
- Option to display predicted orbital paths for each satellite

## Extending the Application

Here are some ways you could extend this application:

1. Add pass prediction for your location:
   - Implement a feature to predict when satellites will pass over a specific location
   - Calculate rise/set times and maximum elevation for each pass

2. Add radio reception capabilities:
   - Integrate with software defined radio (SDR) equipment
   - Automatically tune to the correct frequency during passes
   - Decode APT (Automatic Picture Transmission) signals to receive weather images

3. Add notification system:
   - Send alerts before satellite passes
   - Integrate with email, SMS, or desktop notifications

4. Add more satellites:
   - Expand beyond NOAA satellites to track other weather, amateur radio, or other interesting satellites

5. Improve the UI:
   - Add day/night visualization on the map
   - Add satellite ground station locations
   - Improve mobile device support

## Troubleshooting

- **CORS issues**: If you have CORS errors in the browser console, make sure your FastAPI server is properly configured to allow cross-origin requests.
- **TLE data not loading**: Check your internet connection and make sure you can access celestrak.org.
- **Map not showing**: Verify your MapTiler API key is correct and not expired.