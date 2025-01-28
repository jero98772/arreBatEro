import networkx as nx
import folium
from geopy.distance import geodesic

# Example coordinates for warehouses and stores (latitude, longitude)
nodes = {
    'W1': (40.748817, -73.985428),  # Warehouse 1 (e.g., New York)
    'W2': (40.712776, -74.005974),  # Warehouse 2 (e.g., NYC Downtown)
    'W3': (40.758896, -73.985130),  # Warehouse 3
    'S1': (40.730610, -73.935242),  # Store 1 (Brooklyn)
    'S2': (40.675457, -73.978285)   # Store 2 (Queens)
}

# Create the graph
G = nx.DiGraph()

# Add edges (edges between warehouses and stores, with capacities and costs)
G.add_edge('W1', 'S1', capacity=10, weight=5)  # Warehouse W1 to Store S1 with capacity 10, cost 5
G.add_edge('W1', 'S2', capacity=5, weight=4)   # Warehouse W1 to Store S2 with capacity 5, cost 4
G.add_edge('W2', 'S1', capacity=6, weight=3)   # Warehouse W2 to Store S1 with capacity 6, cost 3
G.add_edge('W3', 'S2', capacity=8, weight=2)   # Warehouse W3 to Store S2 with capacity 8, cost 2

# Calculate max-flow min-cost
flow_value, flow_dict = nx.maximum_flow(G, 'W1', 'S2')
min_cost = nx.cost_of_flow(G, flow_dict)

print(f"Max Flow Value: {flow_value}")
print(f"Flow Distribution: {flow_dict}")
print(f"Min-Cost of Flow: {min_cost}")


import folium

# Create a map centered around the average of all coordinates
map_center = [40.730610, -73.935242]  # Center of New York City (example)
m = folium.Map(location=map_center, zoom_start=13)

# Add nodes (Warehouses and Stores)
for node, coord in nodes.items():
    folium.Marker(location=coord, popup=node).add_to(m)

# Draw lines between warehouses and stores based on the flow distribution
flow_lines = [
    ('W1', 'S1', 6),
    ('W1', 'S2', 4),
    ('W2', 'S1', 0),
    ('W3', 'S2', 8)
]

for edge in flow_lines:
    source = nodes[edge[0]]
    target = nodes[edge[1]]
    flow = edge[2]
    if flow > 0:
        folium.PolyLine([source, target], color="blue", weight=2, opacity=0.5).add_to(m)
        folium.Marker(location=target, popup=f"Flow: {flow} units").add_to(m)

# Save the map as an HTML file
m.save("logistics_network_map.html")


import plotly.graph_objects as go

# Set up the 3D scatter plot for the logistics network
nodes_3d = list(nodes.values())  # Coordinates of nodes
x, y = zip(*nodes_3d)  # Unpack x, y coordinates

# 3D scatter plot of the nodes
fig = go.Figure(data=[go.Scatter3d(
    x=x, y=y, z=[0]*len(x),  # Set Z to 0 as we only have latitude and longitude
    mode='markers+text',
    text=list(nodes.keys()),
    marker=dict(size=10, color='blue')
)])

# Add edges (connections between warehouses and stores)
for edge in flow_lines:
    source = nodes[edge[0]]
    target = nodes[edge[1]]
    flow = edge[2]
    if flow > 0:
        fig.add_trace(go.Scatter3d(
            x=[source[0], target[0]], y=[source[1], target[1]], z=[0, 0],
            mode='lines+text', line=dict(color='blue', width=4),
            text=[f"Flow: {flow} units" for _ in range(2)]
        ))

fig.update_layout(title="3D Logistics Network Flow", scene=dict(xaxis_title='Longitude', yaxis_title='Latitude'))
fig.show()
