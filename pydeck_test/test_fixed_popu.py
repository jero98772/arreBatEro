import pydeck as pdk
import pandas as pd

# Data from OpenStreetMap, accessed via osmpy
DATA_URL = "https://raw.githubusercontent.com/ajduberstein/geo_datasets/master/biergartens.json"
ICON_URL = "https://upload.wikimedia.org/wikipedia/commons/c/c4/Projet_bi%C3%A8re_logo_v2.png"

icon_data = {
    "url": ICON_URL,
    "width": 242,
    "height": 242,
    "anchorY": 242,
}

data = pd.read_json(DATA_URL)
data["icon_data"] = None
data["fixed"] = False  # Add a new column to track fixed status

for i in data.index:
    data["icon_data"][i] = icon_data

view_state = pdk.data_utils.compute_view(data[["lon", "lat"]], 0.1)

icon_layer = pdk.Layer(
    type="IconLayer",
    data=data,
    get_icon="icon_data",
    get_size=4,
    size_scale=15,
    get_position=["lon", "lat"],
    pickable=True,
    auto_highlight=True
)

tooltip = {
    "html": "<b>{name}</b><br>"
            "<a href='#' onclick='toggleFixed({index}); return false;'>Toggle Fixed</a><br>"
            "<a href='#' onclick='window.open(\"https://en.wikipedia.org/wiki/San_Francisco\", \"_blank\"); return false;'>Open Wikipedia</a>",
    "style": {
        "backgroundColor": "steelblue",
        "color": "white"
    }
}

r = pdk.Deck(
    layers=[icon_layer], 
    initial_view_state=view_state, 
    tooltip=tooltip,
    height=600,
    width="100%"
)

# JavaScript function to toggle fixed status
js_function = """
function toggleFixed(index) {
    const deck = document.getElementById('deck.gl-wrapper').deck;
    const data = deck.props.layers[0].props.data;
    data[index].fixed = !data[index].fixed;
    
    // Update the layer
    deck.setProps({
        layers: [
            new deck.IconLayer({
                ...deck.props.layers[0].props,
                data: data,
                getPosition: d => d.fixed ? [d.lon, d.lat] : null
            })
        ]
    });
}
"""

html_content = r.to_html(css_background_color="#333")
html_with_js = html_content.replace('</script>', f'{js_function}</script>')

r = pdk.Deck(layers=[icon_layer], initial_view_state=view_state, tooltip=tooltip)
r.to_html("icon_layer.html")

with open("icon_layer.html", "w") as f:
    f.write(html_with_js)
