import prettymaps
from matplotlib import pyplot as plt
from matplotlib.font_manager import FontProperties

plot = prettymaps.plot(
    (6.182085, -75.563192)
)

# Change background color
plot.fig.patch.set_facecolor('#F2F4CB')
# Add title
plot.ax.set_title(
    'San lucas',
    size = 100
)

plt.show()