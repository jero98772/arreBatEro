import folium
#import json
import pandas as pd

#from pyodide.http import open_url

#url = ("https://raw.githubusercontent.com/python-visualization/folium/master/examples/data")
#state_geo = f"{url}/us-states.json"
#state_unemployment = f"{url}/US_Unemployment_Oct2012.csv"
state_data = pd.read_json("https://raw.githubusercontent.com/jero98772/AlOtroLado/main/core/data/medellin_data.json")
#geo_json = json.loads(open_url(state_geo).read())

m = folium.Map(location=[-75, 6], zoom_start=3)
"""
folium.Choropleth(
    geo_data=state_data,
    name="choropleth",
    data=state_data,
    columns=["State", "Unemployment"],
    key_on="feature.id",
    fill_color="YlGn",
    fill_opacity=0.7,
    line_opacity=0.2,
    legend_name="Unemployment Rate (%)",
).add_to(m)
"""
print(state_data["edges"][0])
folium.PolyLine(state_data["edges"][0],
                color='green',
                weight=15,
                opacity=0.8).add_to(m)

folium.LayerControl().add_to(m)

m.save('my_map.html')