import tkinter as tk
from tkinter import ttk, messagebox
import numpy as np
import math
import time
import datetime
import urllib.request
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
import threading
import os

class TLE:
    """Two-Line Element parser and data container"""
    
    def __init__(self, name, line1, line2):
        self.name = name.strip()
        self.line1 = line1.strip()
        self.line2 = line2.strip()
        self.parse()
        
    def parse(self):
        """Parse TLE data from the two lines"""
        # Line 1 parsing
        self.catalog_number = int(self.line1[2:7])
        self.classification = self.line1[7]
        self.int_designator = self.line1[9:17].strip()
        self.epoch_year = int(self.line1[18:20])
        self.epoch_year = self.epoch_year + 2000 if self.epoch_year < 57 else self.epoch_year + 1900
        self.epoch_day = float(self.line1[20:32])
        self.first_derivative = float(self.line1[33:43])
        self.second_derivative = float(f"{self.line1[44:45]}.{self.line1[45:50]}e{self.line1[50:52]}")
        self.bstar = float(f"{self.line1[53:54]}.{self.line1[54:59]}e{self.line1[59:61]}")
        self.element_number = int(self.line1[64:68])
        
        # Line 2 parsing
        self.inclination = float(self.line2[8:16])
        self.raan = float(self.line2[17:25])
        self.eccentricity = float(f"0.{self.line2[26:33]}")
        self.arg_perigee = float(self.line2[34:42])
        self.mean_anomaly = float(self.line2[43:51])
        self.mean_motion = float(self.line2[52:63])
        self.revolution_number = int(self.line2[63:68])
        
        # Calculate derived values
        self.period_minutes = 1440.0 / self.mean_motion
        
        # Calculate epoch as datetime
        year = self.epoch_year
        days = int(self.epoch_day)
        fraction = self.epoch_day - days
        hours = int(fraction * 24)
        minutes = int((fraction * 24 - hours) * 60)
        seconds = int(((fraction * 24 - hours) * 60 - minutes) * 60)
        
        date = datetime.datetime(year, 1, 1) + datetime.timedelta(days=days-1, 
                                                                hours=hours,
                                                                minutes=minutes,
                                                                seconds=seconds)
        self.epoch = date
        
    def __str__(self):
        return f"{self.name}\n{self.line1}\n{self.line2}"


class SatelliteTracker:
    """Main satellite tracking engine using simplified SGP4"""
    
    # WGS-84 Earth constants
    EARTH_RADIUS = 6378.137  # Earth radius in km
    EARTH_MU = 398600.4418  # Earth gravitational parameter in km^3/s^2
    
    def __init__(self, tle):
        self.tle = tle
        self.setup_orbital_elements()
        
    def setup_orbital_elements(self):
        """Convert TLE elements to orbital parameters"""
        self.inclination_rad = math.radians(self.tle.inclination)
        self.raan_rad = math.radians(self.tle.raan)
        self.arg_perigee_rad = math.radians(self.tle.arg_perigee)
        self.mean_anomaly_rad = math.radians(self.tle.mean_anomaly)
        self.eccentricity = self.tle.eccentricity
        
        # Calculate semi-major axis
        n = self.tle.mean_motion * 2 * math.pi / 86400  # rad/sec
        self.semi_major_axis = (self.EARTH_MU / (n * n)) ** (1/3)  # km
        
    def compute_position(self, dt):
        """
        Compute satellite position at a given datetime
        This is a simplified orbital model and not a full SGP4 implementation
        """
        # Time since epoch in seconds
        time_since_epoch = (dt - self.tle.epoch).total_seconds()
        
        # Calculate mean anomaly at the given time
        n = self.tle.mean_motion * 2 * math.pi / 86400  # rad/sec
        mean_anomaly = self.mean_anomaly_rad + n * time_since_epoch
        mean_anomaly = mean_anomaly % (2 * math.pi)
        
        # Solve Kepler's equation using Newton-Raphson method
        eccentric_anomaly = mean_anomaly
        for _ in range(10):  # Usually converges in a few iterations
            eccentric_anomaly = mean_anomaly + self.eccentricity * math.sin(eccentric_anomaly)
        
        # Calculate true anomaly
        cos_e = math.cos(eccentric_anomaly)
        sin_e = math.sin(eccentric_anomaly)
        cos_v = (cos_e - self.eccentricity) / (1 - self.eccentricity * cos_e)
        sin_v = (math.sqrt(1 - self.eccentricity**2) * sin_e) / (1 - self.eccentricity * cos_e)
        true_anomaly = math.atan2(sin_v, cos_v)
        
        # Calculate distance from focus
        distance = self.semi_major_axis * (1 - self.eccentricity * cos_e)
        
        # Calculate position in orbital plane
        x_orbital = distance * math.cos(true_anomaly)
        y_orbital = distance * math.sin(true_anomaly)
        
        # Rotate to ECI (Earth-Centered Inertial) coordinates
        # First, rotate by argument of perigee
        x_nodal = x_orbital * math.cos(self.arg_perigee_rad) - y_orbital * math.sin(self.arg_perigee_rad)
        y_nodal = x_orbital * math.sin(self.arg_perigee_rad) + y_orbital * math.cos(self.arg_perigee_rad)
        z_nodal = 0
        
        # Then, rotate by inclination
        x_eci_temp = x_nodal
        y_eci_temp = y_nodal * math.cos(self.inclination_rad) - z_nodal * math.sin(self.inclination_rad)
        z_eci_temp = y_nodal * math.sin(self.inclination_rad) + z_nodal * math.cos(self.inclination_rad)
        
        # Finally, rotate by RAAN
        x_eci = x_eci_temp * math.cos(self.raan_rad) - y_eci_temp * math.sin(self.raan_rad)
        y_eci = x_eci_temp * math.sin(self.raan_rad) + y_eci_temp * math.cos(self.raan_rad)
        z_eci = z_eci_temp
        
        return np.array([x_eci, y_eci, z_eci])
    
    def eci_to_ecef(self, position_eci, dt):
        """Convert ECI coordinates to ECEF (Earth-Centered Earth-Fixed)"""
        # Calculate GMST (Greenwich Mean Sidereal Time)
        j2000 = datetime.datetime(2000, 1, 1, 12, 0, 0)
        days_since_j2000 = (dt - j2000).total_seconds() / 86400.0
        
        # Simplified GMST calculation
        gmst = 280.46061837 + 360.98564736629 * days_since_j2000
        gmst = gmst % 360
        gmst_rad = math.radians(gmst)
        
        # Rotate ECI to ECEF
        x_ecef = position_eci[0] * math.cos(gmst_rad) + position_eci[1] * math.sin(gmst_rad)
        y_ecef = -position_eci[0] * math.sin(gmst_rad) + position_eci[1] * math.cos(gmst_rad)
        z_ecef = position_eci[2]
        
        return np.array([x_ecef, y_ecef, z_ecef])
    
    def ecef_to_geodetic(self, position_ecef):
        """Convert ECEF coordinates to geodetic (latitude, longitude, altitude)"""
        x, y, z = position_ecef
        
        # Constants for WGS-84 ellipsoid
        a = self.EARTH_RADIUS  # semi-major axis
        b = 6356.7523142  # semi-minor axis
        e2 = 1 - (b*b)/(a*a)  # square of eccentricity
        
        # Calculate longitude
        longitude = math.atan2(y, x)
        
        # Initial values
        p = math.sqrt(x*x + y*y)
        latitude = math.atan2(z, p * (1-e2))
        
        # Iterative calculation for latitude and altitude
        for _ in range(10):
            sin_lat = math.sin(latitude)
            N = a / math.sqrt(1 - e2 * sin_lat * sin_lat)
            altitude = p / math.cos(latitude) - N
            latitude = math.atan2(z, p * (1 - e2 * N / (N + altitude)))
        
        # Convert to degrees
        latitude_deg = math.degrees(latitude)
        longitude_deg = math.degrees(longitude)
        
        return latitude_deg, longitude_deg, altitude
    
    def get_look_angles(self, observer_lat, observer_lon, observer_alt, dt):
        """Calculate azimuth and elevation from observer to satellite"""
        # Get satellite position
        pos_eci = self.compute_position(dt)
        pos_ecef = self.eci_to_ecef(pos_eci, dt)
        sat_lat, sat_lon, sat_alt = self.ecef_to_geodetic(pos_ecef)
        
        # Convert observer position to radians
        obs_lat_rad = math.radians(observer_lat)
        obs_lon_rad = math.radians(observer_lon)
        
        # Observer position in ECEF
        obs_ecef = self.geodetic_to_ecef(observer_lat, observer_lon, observer_alt)
        
        # Vector from observer to satellite
        range_vector = pos_ecef - obs_ecef
        
        # Convert to topocentric coordinates (observer-centered)
        sin_lat = math.sin(obs_lat_rad)
        cos_lat = math.cos(obs_lat_rad)
        sin_lon = math.sin(obs_lon_rad)
        cos_lon = math.cos(obs_lon_rad)
        
        # Rotation matrix for local tangent plane
        rx = -sin_lat * cos_lon * range_vector[0] - sin_lat * sin_lon * range_vector[1] + cos_lat * range_vector[2]
        ry = -sin_lon * range_vector[0] + cos_lon * range_vector[1]
        rz = cos_lat * cos_lon * range_vector[0] + cos_lat * sin_lon * range_vector[1] + sin_lat * range_vector[2]
        
        # Calculate look angles
        range_km = math.sqrt(rx*rx + ry*ry + rz*rz)
        elevation = math.degrees(math.asin(rz / range_km))
        azimuth = math.degrees(math.atan2(ry, rx)) % 360
        
        return azimuth, elevation, range_km, sat_lat, sat_lon, sat_alt
    
    def geodetic_to_ecef(self, lat, lon, alt):
        """Convert geodetic coordinates to ECEF"""
        lat_rad = math.radians(lat)
        lon_rad = math.radians(lon)
        
        # Constants for WGS-84 ellipsoid
        a = self.EARTH_RADIUS  # semi-major axis in km
        b = 6356.7523142  # semi-minor axis in km
        e2 = 1 - (b*b)/(a*a)  # square of eccentricity
        
        N = a / math.sqrt(1 - e2 * math.sin(lat_rad) * math.sin(lat_rad))
        
        x = (N + alt) * math.cos(lat_rad) * math.cos(lon_rad)
        y = (N + alt) * math.cos(lat_rad) * math.sin(lon_rad)
        z = (N * (1 - e2) + alt) * math.sin(lat_rad)
        
        return np.array([x, y, z])
    
    def predict_passes(self, observer_lat, observer_lon, observer_alt, 
                      start_time, duration_hours, min_elevation=5):
        """Predict satellite passes for a given location and time window"""
        passes = []
        current_pass = None
        
        # Step through time in 30-second increments
        dt = datetime.timedelta(seconds=30)
        time_points = int(duration_hours * 3600 / dt.total_seconds())
        
        for i in range(time_points):
            current_time = start_time + i * dt
            
            az, el, rng, sat_lat, sat_lon, sat_alt = self.get_look_angles(
                observer_lat, observer_lon, observer_alt, current_time)
            
            # Check if satellite is above horizon with minimum elevation
            if el >= min_elevation:
                if current_pass is None:
                    # Start of a new pass
                    current_pass = {
                        'start_time': current_time,
                        'max_elevation': el,
                        'max_elevation_time': current_time,
                        'points': [(current_time, az, el, rng, sat_lat, sat_lon)]
                    }
                else:
                    # Continue current pass
                    current_pass['points'].append((current_time, az, el, rng, sat_lat, sat_lon))
                    if el > current_pass['max_elevation']:
                        current_pass['max_elevation'] = el
                        current_pass['max_elevation_time'] = current_time
            elif current_pass is not None:
                # End of current pass
                current_pass['end_time'] = current_time - dt
                passes.append(current_pass)
                current_pass = None
        
        # Handle pass in progress at the end of the time window
        if current_pass is not None:
            current_pass['end_time'] = start_time + time_points * dt
            passes.append(current_pass)
        
        return passes


class NOAASatelliteTrackerApp:
    """GUI application for tracking NOAA weather satellites"""
    
    def __init__(self, root):
        self.root = root
        self.root.title("NOAA Satellite Tracker")
        self.root.geometry("1000x700")
        
        # Default NOAA satellite TLEs (these should be updated regularly)
        self.default_tles = {
            "NOAA 15": ("NOAA 15 [B]", 
                      "1 25338U 98030A   24135.56636277  .00000077  00000+0  57685-4 0  9992", 
                      "2 25338  98.6963 139.1226 0009885 212.7207 147.3308 14.26181779346693"),
            "NOAA 18": ("NOAA 18", 
                      "1 28654U 05018A   24135.52598750  .00000077  00000+0  65330-4 0  9995", 
                      "2 28654  99.0345  88.4691 0015031  90.2361 270.0512 14.12912161966499"),
            "NOAA 19": ("NOAA 19", 
                      "1 33591U 09005A   24135.54384146  .00000083  00000+0  62651-4 0  9992", 
                      "2 33591  99.1471  89.7064 0013577 135.7848 224.4271 14.12558292784124")
        }
        
        # Initialize satellite trackers with default TLEs
        self.satellites = {}
        for name, tle_data in self.default_tles.items():
            tle = TLE(*tle_data)
            self.satellites[name] = SatelliteTracker(tle)
        
        self.selected_satellite = tk.StringVar(value="NOAA 19")  # Default selection
        
        # Observer location (default: New York City)
        self.observer_lat = tk.DoubleVar(value=40.7128)
        self.observer_lon = tk.DoubleVar(value=-74.0060)
        self.observer_alt = tk.DoubleVar(value=0.010)  # km above sea level
        
        # Create UI elements
        self.create_ui()
        
        # Start tracking thread
        self.stop_thread = False
        self.tracking_thread = threading.Thread(target=self.tracking_loop)
        self.tracking_thread.daemon = True
        self.tracking_thread.start()
    
    def create_ui(self):
        """Create the user interface"""
        # Create main frames
        control_frame = ttk.Frame(self.root, padding="10")
        control_frame.pack(side=tk.TOP, fill=tk.X)
        
        map_frame = ttk.Frame(self.root)
        map_frame.pack(side=tk.TOP, fill=tk.BOTH, expand=True)
        
        info_frame = ttk.Frame(self.root, padding="10")
        info_frame.pack(side=tk.BOTTOM, fill=tk.X)
        
        # Control elements
        ttk.Label(control_frame, text="Satellite:").grid(row=0, column=0, padx=5, pady=5, sticky=tk.W)
        sat_menu = ttk.Combobox(control_frame, textvariable=self.selected_satellite, 
                               values=list(self.satellites.keys()), state="readonly", width=10)
        sat_menu.grid(row=0, column=1, padx=5, pady=5, sticky=tk.W)
        
        ttk.Label(control_frame, text="Observer Location:").grid(row=0, column=2, padx=5, pady=5, sticky=tk.W)
        ttk.Label(control_frame, text="Lat:").grid(row=0, column=3, padx=5, pady=5, sticky=tk.W)
        ttk.Entry(control_frame, textvariable=self.observer_lat, width=8).grid(row=0, column=4, padx=2, pady=5, sticky=tk.W)
        ttk.Label(control_frame, text="Lon:").grid(row=0, column=5, padx=5, pady=5, sticky=tk.W)
        ttk.Entry(control_frame, textvariable=self.observer_lon, width=8).grid(row=0, column=6, padx=2, pady=5, sticky=tk.W)
        ttk.Label(control_frame, text="Alt (km):").grid(row=0, column=7, padx=5, pady=5, sticky=tk.W)
        ttk.Entry(control_frame, textvariable=self.observer_alt, width=6).grid(row=0, column=8, padx=2, pady=5, sticky=tk.W)
        
        # Button to update TLEs
        update_button = ttk.Button(control_frame, text="Update TLEs", command=self.update_tles)
        update_button.grid(row=0, column=9, padx=10, pady=5, sticky=tk.W)
        
        # Button to show upcoming passes
        passes_button = ttk.Button(control_frame, text="Show Passes", command=self.show_passes)
        passes_button.grid(row=0, column=10, padx=10, pady=5, sticky=tk.W)
        
        # Create map visualization
        self.fig = Figure(figsize=(10, 6), dpi=100)
        self.map_ax = self.fig.add_subplot(111)
        self.map_canvas = FigureCanvasTkAgg(self.fig, master=map_frame)
        self.map_canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)
        
        # Info panel
        self.info_text = tk.Text(info_frame, height=6, width=80)
        self.info_text.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
        # Initialize map
        self.init_map()
    
    def init_map(self):
        """Initialize world map visualization"""
        # Create a basic world map
        self.map_ax.clear()
        self.map_ax.set_xlim(-180, 180)
        self.map_ax.set_ylim(-90, 90)
        self.map_ax.set_xlabel('Longitude')
        self.map_ax.set_ylabel('Latitude')
        self.map_ax.set_title(f'{self.selected_satellite.get()} Position')
        self.map_ax.grid(True)
        
        # Add simplified world map outlines
        try:
            # Very simplified continent outlines - in a real app, use cartopy or similar
            world_map = [
                # North America (simplified)
                [(-125, 50), (-125, 30), (-105, 30), (-90, 25), (-80, 25), (-65, 45), (-125, 50)],
                # South America (simplified)
                [(-80, 10), (-80, -50), (-70, -55), (-50, -55), (-35, -10), (-50, 10), (-80, 10)],
                # Europe/Asia (simplified)
                [(0, 50), (30, 70), (180, 70), (180, 35), (100, 0), (30, 0), (10, 35), (0, 50)],
                # Africa (simplified)
                [(0, 35), (30, 35), (50, 0), (20, -35), (-15, -35), (-15, 15), (0, 35)],
                # Australia (simplified)
                [(115, -10), (155, -10), (155, -45), (115, -45), (115, -10)]
            ]
            
            for continent in world_map:
                xs, ys = zip(*continent)
                self.map_ax.plot(xs, ys, 'k-', linewidth=1)
        except Exception as e:
            print(f"Error drawing world map: {e}")
        
        # Mark observer location
        lat = self.observer_lat.get()
        lon = self.observer_lon.get()
        self.map_ax.plot(lon, lat, 'ro', markersize=5)
        self.map_ax.text(lon+2, lat+2, 'Observer', fontsize=8)
        
        # Update canvas
        self.map_canvas.draw()
      
    def tracking_loop(self):
        """Background thread for continuous satellite tracking"""
        last_update = time.time()
        
        while not self.stop_thread:
            current_time = time.time()
            
            # Update every 1 second
            if current_time - last_update >= 1.0:
                last_update = current_time
                
                try:
                    self.update_satellite_position()
                except Exception as e:
                    print(f"Error in tracking loop: {e}")
                
            time.sleep(0.1)  # Small sleep to prevent high CPU usage
    
    def update_satellite_position(self):
        """Update satellite position on the map"""
        # Get current satellite tracker
        sat_name = self.selected_satellite.get()
        if sat_name not in self.satellites:
            return
            
        tracker = self.satellites[sat_name]
        
        # Current time
        now = datetime.datetime.utcnow()
        
        # Calculate current position
        pos_eci = tracker.compute_position(now)
        pos_ecef = tracker.eci_to_ecef(pos_eci, now)
        sat_lat, sat_lon, sat_alt = tracker.ecef_to_geodetic(pos_ecef)
        
        # Calculate look angles from observer
        obs_lat = self.observer_lat.get()
        obs_lon = self.observer_lon.get()
        obs_alt = self.observer_alt.get()
        
        az, el, rng, _, _, _ = tracker.get_look_angles(obs_lat, obs_lon, obs_alt, now)
        
        # Update map (in main thread)
        self.root.after(0, lambda: self.update_map(sat_lat, sat_lon, sat_alt, az, el, rng, now))
    
    def update_map(self, sat_lat, sat_lon, sat_alt, az, el, rng, timestamp):
        """Update the map with new satellite position (called in main thread)"""
        try:
            # Update map
            self.map_ax.clear()
            self.map_ax.set_xlim(-180, 180)
            self.map_ax.set_ylim(-90, 90)
            self.map_ax.set_xlabel('Longitude')
            self.map_ax.set_ylabel('Latitude')
            self.map_ax.set_title(f'{self.selected_satellite.get()} - {timestamp.strftime("%Y-%m-%d %H:%M:%S")} UTC')
            self.map_ax.grid(True)
            
            # Draw simplified continent outlines
            world_map = [
                # Same as in init_map
                [(-125, 50), (-125, 30), (-105, 30), (-90, 25), (-80, 25), (-65, 45), (-125, 50)],
                [(-80, 10), (-80, -50), (-70, -55), (-50, -55), (-35, -10), (-50, 10), (-80, 10)],
                [(0, 50), (30, 70), (180, 70), (180, 35), (100, 0), (30, 0), (10, 35), (0, 50)],
                [(0, 35), (30, 35), (50, 0), (20, -35), (-15, -35), (-15, 15), (0, 35)],
                [(115, -10), (155, -10), (155, -45), (115, -45), (115, -10)]
            ]
            
            for continent in world_map:
                xs, ys = zip(*continent)
                self.map_ax.plot(xs, ys, 'k-', linewidth=1)
            
            # Draw satellite position
            self.map_ax.plot(sat_lon, sat_lat, 'bo', markersize=6)
            
            # Draw ground track (simplified)
            sat_tracker = self.satellites[self.selected_satellite.get()]
            track_lats = []
            track_lons = []
            
            # Calculate 100 minutes of orbit (roughly one orbit)
            for minute in range(-50, 50, 2):
                try:
                    future_time = timestamp + datetime.timedelta(minutes=minute)
                    pos = sat_tracker.compute_position(future_time)
                    ecef = sat_tracker.eci_to_ecef(pos, future_time)
                    lat, lon, _ = sat_tracker.ecef_to_geodetic(ecef)
                    track_lats.append(lat)
                    track_lons.append(lon)
                except:
                    continue
            
            self.map_ax.plot(track_lons, track_lats, 'b--', linewidth=1, alpha=0.5)
            
            # Draw observer location
            obs_lat = self.observer_lat.get()
            obs_lon = self.observer_lon.get()
            self.map_ax.plot(obs_lon, obs_lat, 'ro', markersize=5)
            
            # Draw visibility circle if satellite is above horizon
            if el > 0:
                # Draw a circle from observer to satellite representing visibility
                self.map_ax.plot([obs_lon, sat_lon], [obs_lat, sat_lat], 'g-', linewidth=1.5)
            
            # Update canvas
            self.map_canvas.draw()
            
            # Update info text
            self.info_text.delete(1.0, tk.END)
            self.info_text.insert(tk.END, (
                f"Satellite: {self.selected_satellite.get()}\n"
                f"Time (UTC): {timestamp.strftime('%Y-%m-%d %H:%M:%S')}\n"
                f"Position: Lat {sat_lat:.2f}°, Lon {sat_lon:.2f}°, Alt {sat_alt:.2f} km\n"
                f"From Observer: Az {az:.1f}°, El {el:.1f}°, Range {rng:.1f} km\n"
                f"Status: {'Visible' if el > 0 else 'Below horizon'}"
            ))
        except Exception as e:
            print(f"Error updating map: {e}")
    
    def update_tles(self):
        """Download and update TLEs from online source"""
        try:
            messagebox.showinfo("Updating TLEs", 
                              "Downloading latest TLE data for NOAA satellites...")
            
            # URLs for TLE data
            celestrak_url = "https://celestrak.org/NORAD/elements/noaa.txt"
            
            # Download TLE data
            response = urllib.request.urlopen(celestrak_url)
            tle_data = response.read().decode('utf-8')
            
            # Parse TLE data
            lines = tle_data.strip().split('\n')
            new_tles = {}
            
            for i in range(0, len(lines), 3):
                if i+2 < len(lines):
                    sat_name = lines[i].strip()
                    line1 = lines[i+1].strip()
                    line2 = lines[i+2].strip()
                    
                    # Only update NOAA weather satellites we're interested in
                    if "NOAA 15" in sat_name or "NOAA 18" in sat_name or "NOAA 19" in sat_name:
                        new_tles[sat_name] = (sat_name, line1, line2)
            
            # Update satellite trackers with new TLEs
            for name, tle_data in new_tles.items():
                try:
                    tle = TLE(*tle_data)
                    
                    # Match with our existing satellite keys
                    if "NOAA 15" in name:
                        self.satellites["NOAA 15"] = SatelliteTracker(tle)
                    elif "NOAA 18" in name:
                        self.satellites["NOAA 18"] = SatelliteTracker(tle)
                    elif "NOAA 19" in name:
                        self.satellites["NOAA 19"] = SatelliteTracker(tle)
                except Exception as e:
                    print(f"Error parsing TLE for {name}: {e}")
            
            messagebox.showinfo("TLE Update", "TLE data updated successfully!")
            
        except Exception as e:
            messagebox.showerror("Error", f"Failed to update TLEs: {e}")
    
    def show_passes(self):
        """Show upcoming satellite passes"""
        # Create a new window for passes
        passes_window = tk.Toplevel(self.root)
        passes_window.title("Upcoming Passes")
        passes_window.geometry("800x600")
        
        # Add satellites selection
        frame = ttk.Frame(passes_window, padding="10")
        frame.pack(fill=tk.X)
        
        ttk.Label(frame, text="Select Satellite:").pack(side=tk.LEFT, padx=5)
        sat_var = tk.StringVar(value="NOAA 19")
        sat_combo = ttk.Combobox(frame, textvariable=sat_var, 
                               values=list(self.satellites.keys()), state="readonly", width=10)
        sat_combo.pack(side=tk.LEFT, padx=5)
        
        ttk.Label(frame, text="Duration (hrs):").pack(side=tk.LEFT, padx=5)
        duration_var = tk.IntVar(value=24)
        duration_spin = ttk.Spinbox(frame, from_=1, to=72, textvariable=duration_var, width=5)
        duration_spin.pack(side=tk.LEFT, padx=5)
        
        ttk.Label(frame, text="Min. Elevation (°):").pack(side=tk.LEFT, padx=5)
        elevation_var = tk.IntVar(value=10)
        elevation_spin = ttk.Spinbox(frame, from_=0, to=90, textvariable=elevation_var, width=5)
        elevation_spin.pack(side=tk.LEFT, padx=5)
        
        # Text widget for pass information
        pass_text = tk.Text(passes_window, height=30, width=100)
        pass_text.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        scrollbar = ttk.Scrollbar(pass_text, command=pass_text.yview)
        pass_text.configure(yscrollcommand=scrollbar.set)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        # Button to calculate passes
        def calculate_passes():
            # Clear previous passes
            pass_text.delete(1.0, tk.END)
            
            # Get selected satellite
            sat_name = sat_var.get()
            if sat_name not in self.satellites:
                pass_text.insert(tk.END, "Invalid satellite selection.")
                return
                
            tracker = self.satellites[sat_name]
            
            # Observer information
            obs_lat = self.observer_lat.get()
            obs_lon = self.observer_lon.get()
            obs_alt = self.observer_alt.get()
            
            # Time parameters
            start_time = datetime.datetime.utcnow()
            duration = duration_var.get()
            min_elevation = elevation_var.get()
            
            # Calculate passes
            passes = tracker.predict_passes(obs_lat, obs_lon, obs_alt, 
                                          start_time, duration, min_elevation)
            
            # Display pass information
            if not passes:
                pass_text.insert(tk.END, f"No passes found for {sat_name} in the next {duration} hours.\n")
                return
                
            pass_text.insert(tk.END, f"Upcoming passes for {sat_name} (next {duration} hours)\n")
            pass_text.insert(tk.END, f"Observer: Lat {obs_lat:.2f}°, Lon {obs_lon:.2f}°, Alt {obs_alt:.2f} km\n\n")
            
            for i, p in enumerate(passes):
                # Format start and end times
                start_str = p['start_time'].strftime('%Y-%m-%d %H:%M:%S')
                end_str = p['end_time'].strftime('%H:%M:%S')
                max_el_str = p['max_elevation_time'].strftime('%H:%M:%S')
                
                duration_sec = (p['end_time'] - p['start_time']).total_seconds()
                duration_min = duration_sec / 60
                
                pass_text.insert(tk.END, f"Pass #{i+1}\n")
                pass_text.insert(tk.END, f"  Start: {start_str} UTC\n")
                pass_text.insert(tk.END, f"  End: {end_str} UTC\n")
                pass_text.insert(tk.END, f"  Duration: {duration_min:.1f} minutes\n")
                pass_text.insert(tk.END, f"  Max Elevation: {p['max_elevation']:.1f}° at {max_el_str} UTC\n\n")
                
                # Add details about the pass trajectory
                pass_text.insert(tk.END, "  Trajectory Details:\n")
                pass_text.insert(tk.END, "  Time       Az(°)   El(°)   Range(km)  Lat(°)    Lon(°)\n")
                pass_text.insert(tk.END, "  --------------------------------------------------------\n")
                
                # Print details at 1-minute intervals
                # Get up to 10 points spaced evenly through the pass
                points = p['points']
                if len(points) > 10:
                    step = len(points) // 10
                    selected_points = points[::step]
                else:
                    selected_points = points
                    
                for pt in selected_points:
                    t, az, el, rng, lat, lon = pt
                    t_str = t.strftime('%H:%M:%S')
                    pass_text.insert(tk.END, f"  {t_str}   {az:6.1f}   {el:5.1f}   {rng:8.1f}   {lat:7.2f}   {lon:7.2f}\n")
                
                pass_text.insert(tk.END, "\n" + "-"*60 + "\n\n")
        
        calc_button = ttk.Button(frame, text="Calculate Passes", command=calculate_passes)
        calc_button.pack(side=tk.LEFT, padx=20)
        
        # Initialize with a calculation
        calculate_passes()


    def __del__(self):
        """Clean up resources when the app is closed"""
        self.stop_thread = True
        if hasattr(self, 'tracking_thread') and self.tracking_thread.is_alive():
            self.tracking_thread.join(timeout=1.0)


class NOAASatelliteVisualizer:
    """Optional 3D visualization of satellite orbits"""
    
    def __init__(self, satellites):
        """Initialize 3D orbit visualization
        
        Args:
            satellites: Dictionary of satellite trackers
        """
        self.satellites = satellites
        self.create_visualization()
    
    def create_visualization(self):
        """Create a 3D visualization window"""
        visualization_window = tk.Toplevel()
        visualization_window.title("3D Orbit Visualization")
        visualization_window.geometry("800x600")
        
        # Create 3D figure
        self.fig = Figure(figsize=(8, 6), dpi=100)
        self.ax = self.fig.add_subplot(111, projection='3d')
        
        # Create canvas for the figure
        self.canvas = FigureCanvasTkAgg(self.fig, master=visualization_window)
        self.canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)
        
        # Add Earth
        self._add_earth()
        
        # Add satellite orbits
        self._add_orbits()
        
        # Add control buttons
        control_frame = ttk.Frame(visualization_window, padding="10")
        control_frame.pack(fill=tk.X)
        
        ttk.Button(control_frame, text="Rotate View", 
                  command=lambda: self._rotate_view(30)).pack(side=tk.LEFT, padx=5)
        ttk.Button(control_frame, text="Reset View", 
                  command=self._reset_view).pack(side=tk.LEFT, padx=5)
        
        # Update canvas
        self.canvas.draw()
    
    def _add_earth(self):
        """Add simplified Earth representation"""
        # Constants
        earth_radius = 6378.137  # km
        
        # Create sphere
        u = np.linspace(0, 2 * np.pi, 100)
        v = np.linspace(0, np.pi, 50)
        x = earth_radius * np.outer(np.cos(u), np.sin(v))
        y = earth_radius * np.outer(np.sin(u), np.sin(v))
        z = earth_radius * np.outer(np.ones(np.size(u)), np.cos(v))
        
        # Plot Earth as wireframe
        self.ax.plot_surface(x, y, z, color='b', alpha=0.1)
        
        # Add axis lines
        max_val = earth_radius * 1.5
        self.ax.plot([-max_val, max_val], [0, 0], [0, 0], 'k-', alpha=0.2)  # X-axis
        self.ax.plot([0, 0], [-max_val, max_val], [0, 0], 'k-', alpha=0.2)  # Y-axis
        self.ax.plot([0, 0], [0, 0], [-max_val, max_val], 'k-', alpha=0.2)  # Z-axis
        
        # Set labels
        self.ax.set_xlabel('X (km)')
        self.ax.set_ylabel('Y (km)')
        self.ax.set_zlabel('Z (km)')
        
        # Set limits
        self.ax.set_xlim(-max_val, max_val)
        self.ax.set_ylim(-max_val, max_val)
        self.ax.set_zlim(-max_val, max_val)
    
    def _add_orbits(self):
        """Add satellite orbits"""
        # Set colors for different satellites
        colors = {
            "NOAA 15": 'r',
            "NOAA 18": 'g',
            "NOAA 19": 'b'
        }
        
        # Current time
        now = datetime.datetime.utcnow()
        
        # Plot orbit for each satellite
        for name, tracker in self.satellites.items():
            color = colors.get(name, 'c')
            
            # Calculate points for one orbit
            x_orbit = []
            y_orbit = []
            z_orbit = []
            
            # Get orbital period in minutes
            period_mins = 24 * 60 / tracker.tle.mean_motion
            
            # Calculate 100 points around the orbit
            for i in range(100):
                # Time offset in minutes
                t_offset = i * period_mins / 100
                time_point = now + datetime.timedelta(minutes=t_offset)
                
                # Get position
                pos_eci = tracker.compute_position(time_point)
                x_orbit.append(pos_eci[0])
                y_orbit.append(pos_eci[1])
                z_orbit.append(pos_eci[2])
            
            # Plot orbit
            self.ax.plot(x_orbit, y_orbit, z_orbit, color=color, label=name)
            
            # Plot current position
            pos_eci = tracker.compute_position(now)
            self.ax.scatter(pos_eci[0], pos_eci[1], pos_eci[2], color=color, s=50)
        
        # Add legend
        self.ax.legend()
    
    def _rotate_view(self, angle_deg):
        """Rotate the view by the specified angle"""
        self.ax.view_init(elev=self.ax.elev, azim=self.ax.azim + angle_deg)
        self.canvas.draw()
    
    def _reset_view(self):
        """Reset the view to the default angle"""
        self.ax.view_init(elev=30, azim=120)
        self.canvas.draw()


def main():
    """Main function to run the application"""
    root = tk.Tk()
    app = NOAASatelliteTrackerApp(root)
    root.mainloop()


if __name__ == "__main__":
    main()