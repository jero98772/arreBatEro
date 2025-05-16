"""
A comprehensive example of the satellite prediction algorithm using SGP4 model
"""

import numpy as np
from datetime import datetime, timedelta
from skyfield.api import load, wgs84, EarthSatellite
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap

def sgp4_prediction_example():
    """
    This is a standalone example showing how the SGP4 prediction algorithm works
    to track NOAA weather satellites.
    """
    # Load timescale
    ts = load.timescale()
    
    # Example TLE data for NOAA satellites
    # These are sample TLEs - in a real application you would fetch updated ones
    tle_data = {
        "NOAA 15": [
            "NOAA 15",
            "1 25338U 98030A   24135.52397200  .00000156  00000+0  90386-4 0  9992",
            "2 25338  98.7639 106.1640 0009925 318.8307  41.2075 14.26057163339116"
        ],
        "NOAA 18": [
            "NOAA 18",
            "1 28654U 05018A   24135.54223295  .00000126  00000+0  93486-4 0  9998",
            "2 28654  99.0513  94.0011 0014456  70.0554 290.2117 14.12767638972404"
        ],
        "NOAA 19": [
            "NOAA 19",
            "1 33591U 09005A   24135.51878759  .00000146  00000+0  10189-3 0  9994",
            "2 33591  99.1608 112.9023 0013907 133.3462 226.8789 14.12675733793113"
        ]
    }
    
    # Create satellite objects
    satellites = {}
    for name, tle in tle_data.items():
        satellites[name] = EarthSatellite(tle[1], tle[2], tle[0], ts)
    
    # Define time period for prediction
    start_time = ts.now()
    end_time = start_time + 1  # 1 day from now
    times = ts.linspace(start_time, end_time, 100)  # 100 points over the time period
    
    # Create a map for visualization
    plt.figure(figsize=(12, 8))
    m = Basemap(projection='mill', llcrnrlat=-90, urcrnrlat=90,
                llcrnrlon=-180, urcrnrlon=180, resolution='c')
    m.drawcoastlines()
    m.drawcountries()
    m.drawparallels(np.arange(-90, 91, 30), labels=[1, 0, 0, 0])
    m.drawmeridians(np.arange(-180, 181, 60), labels=[0, 0, 0, 1])
    m.fillcontinents(color='lightgray', lake_color='aqua')
    m.drawmapboundary(fill_color='aqua')
    
    # Calculate and plot orbits
    for name, satellite in satellites.items():
        latitudes = []
        longitudes = []
        
        for t in times:
            # Get position at time t
            geocentric = satellite.at(t)
            subpoint = wgs84.subpoint(geocentric)
            
            lat = subpoint.latitude.degrees
            lon = subpoint.longitude.degrees
            
            latitudes.append(lat)
            longitudes.append(lon)
        
        # Convert to map coordinates and plot
        x, y = m(longitudes, latitudes)
        m.plot(x, y, '-', label=name, linewidth=2)
        
        # Mark the current position
        current_pos = satellite.at(start_time)
        current_subpoint = wgs84.subpoint(current_pos)
        current_lat = current_subpoint.latitude.degrees
        current_lon = current_subpoint.longitude.degrees
        x, y = m(current_lon, current_lat)
        m.plot(x, y, 'o', markersize=8, label=f"{name} Current")
    
    plt.title("NOAA Satellite Orbits Prediction (24 hours)")
    plt.legend(loc="upper left")
    plt.savefig("noaa_satellites_prediction.png", dpi=300)
    plt.close()
    
    # Calculate satellite visibility from a specific location
    # Example: New York City
    location = wgs84.latlon(40.7128, -74.0060)  # NYC coordinates
    
    print("Satellite pass predictions for New York City:")
    print("============================================")
    
    for name, satellite in satellites.items():
        print(f"\n{name}:")
        
        # Find satellite passes in the next 24 hours
        t0 = ts.now()
        t1 = ts.now() + 1  # 1 day
        
        # Create a difference function that returns a negative number when the satellite
        # is above the horizon, and a positive number when it's below
        def satellite_altitude_difference(t):
            """Return satellite altitude above the horizon in degrees"""
            topocentric = (satellite - location).at(t)
            alt, az, distance = topocentric.altaz()
            return -alt.degrees
        
        # Find rise and set times
        max_passes = 5  # Maximum number of passes to check
        current_time = t0
        passes = []
        
        for _ in range(max_passes * 2):  # We need to find both rise and set times
            try:
                # Find next time when satellite crosses the horizon (rising or setting)
                transition_time = ts.tai_jd(current_time.tai + 
                                            find_zero(satellite_altitude_difference, 
                                                     current_time.tai, t1.tai))
                
                # Check if it's rising or setting
                is_rising = satellite_altitude_difference(transition_time - timedelta(minutes=1)) > 0
                
                # Add to our list of events
                passes.append((transition_time, "rise" if is_rising else "set"))
                
                # Move past this event
                current_time = transition_time + timedelta(minutes=1)
                
                if current_time.tai > t1.tai:
                    break
                    
            except ValueError:
                # No more crossings found
                break
        
        # Process and display passes
        rise_time = None
        for event_time, event_type in passes:
            if event_type == "rise":
                rise_time = event_time
            elif event_type == "set" and rise_time is not None:
                # Calculate max elevation during this pass
                times_during_pass = ts.linspace(rise_time, event_time, 20)
                
                max_elevation = -90
                max_elevation_time = None
                
                for t in times_during_pass:
                    topocentric = (satellite - location).at(t)
                    alt, az, distance = topocentric.altaz()
                    
                    if alt.degrees > max_elevation:
                        max_elevation = alt.degrees
                        max_elevation_time = t
                
                # Display pass information
                print(f"Pass: {rise_time.utc_strftime('%Y-%m-%d %H:%M:%S')} to {event_time.utc_strftime('%H:%M:%S')}")
                print(f"  Max elevation: {max_elevation:.1f}° at {max_elevation_time.utc_strftime('%H:%M:%S')}")
                print(f"  Duration: {(event_time.tai - rise_time.tai) * 24 * 60:.1f} minutes")
                
                rise_time = None

def find_zero(func, start, end, precision=1e-8):
    """
    Simple root finding algorithm using bisection method
    """
    a, b = start, end
    if func(a) * func(b) > 0:
        raise ValueError("Function must have opposite signs at the ends of the interval")
    
    while (b - a) > precision:
        c = (a + b) / 2
        if func(c) == 0:
            return c
        if func(c) * func(a) < 0:
            b = c
        else:
            a = c
    
    return (a + b) / 2

if __name__ == "__main__":
    sgp4_prediction_example()
