import networkx as nx
import folium
from geopy.distance import geodesic
import plotly.graph_objects as go
import random

# Example function to generate random data
def generate_random_data(num_warehouses=3, num_stores=3, max_flow_lines=5):
    nodes = {}
    for i in range(1, num_warehouses + 1):
        nodes[f'W{i}'] = (random.uniform(-90, 90), random.uniform(-180, 180))
    for i in range(1, num_stores + 1):
        nodes[f'S{i}'] = (random.uniform(-90, 90), random.uniform(-180, 180))
    
    flow_lines = []
    for _ in range(random.randint(1, max_flow_lines)):
        warehouse = f'W{random.randint(1, num_warehouses)}'
        store = f'S{random.randint(1, num_stores)}'
        flow = random.randint(1, 10)
        flow_lines.append((warehouse, store, flow))
    
    G = nx.DiGraph()
    for warehouse, store, flow in flow_lines:
        capacity = random.randint(1, 10)
        weight = random.randint(1, 5)
        G.add_edge(warehouse, store, capacity=capacity, weight=weight)
    
    return nodes, flow_lines, G

# Generate random data
num_warehouses = 6
num_stores = 1
max_flow_lines = 4
nodes, flow_lines, G = generate_random_data(num_warehouses, num_stores, max_flow_lines)

# Generate a fully connected graph
full_graph = nx.DiGraph()
for source in nodes:
    for target in nodes:
        if source != target:
            full_graph.add_edge(source, target, weight=geodesic(nodes[source], nodes[target]).km)

# Ensure source and sink are valid
source = next((node for node in G.nodes if node.startswith('W')), None)
sink = next((node for node in G.nodes if node.startswith('S')), None)
if not source or not sink:
    raise ValueError("Source or sink not found in the graph!")

# Ensure connectivity
if not nx.has_path(G, source, sink):
    raise ValueError("No path exists between source and sink!")

# Calculate maximum flow
flow_value, flow_dict = nx.maximum_flow(G, source, sink, capacity='capacity')

# Calculate min-cost flow
min_cost_flow_dict = nx.max_flow_min_cost(G, source, sink, capacity='capacity', weight='weight')
min_cost = nx.cost_of_flow(G, min_cost_flow_dict)

print(f"Maximum Flow: {flow_value}, Minimum Cost: {min_cost}")

# Create a Folium map
map_center = [40.730610, -73.935242]
m = folium.Map(location=map_center, zoom_start=13)

# Plot all edges (full graph) in black
for u, v in full_graph.edges:
    folium.PolyLine([nodes[u], nodes[v]], color="black", weight=1, opacity=0.5).add_to(m)

# Plot edges from the flow in blue
for u, v in G.edges:
    if flow_dict[u][v] > 0:
        folium.PolyLine([nodes[u], nodes[v]], color="blue", weight=2, opacity=0.8).add_to(m)

# Plot nodes
for node, coord in nodes.items():
    folium.Marker(location=coord, popup=node, icon=folium.Icon(color='red')).add_to(m)

# Save the map
m.save("logistics_network_map.html")

# Create a 3D plot
nodes_3d = list(nodes.values())
x, y = zip(*nodes_3d)
fig = go.Figure()

# Add nodes in red
fig.add_trace(go.Scatter3d(
    x=x, y=y, z=[0] * len(x),
    mode='markers+text',
    text=list(nodes.keys()),
    marker=dict(size=10, color='red')
))

# Add edges from the full graph in black
for u, v in full_graph.edges:
    fig.add_trace(go.Scatter3d(
        x=[nodes[u][0], nodes[v][0]], 
        y=[nodes[u][1], nodes[v][1]], 
        z=[0, 0],
        mode='lines',
        line=dict(color='black', width=1)
    ))

# Add edges from the flow in blue
for u, v in G.edges:
    if flow_dict[u][v] > 0:
        fig.add_trace(go.Scatter3d(
            x=[nodes[u][0], nodes[v][0]], 
            y=[nodes[u][1], nodes[v][1]], 
            z=[0, 0],
            mode='lines',
            line=dict(color='blue', width=7)
        ))

fig.update_layout(title="3D Logistics Network Flow", scene=dict(
    xaxis_title='Longitude', yaxis_title='Latitude', zaxis_title='Height'
))

fig.write_html("logistics_network_3d_flow.html")