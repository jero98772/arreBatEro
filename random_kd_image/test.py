import matplotlib.pyplot as plt
import matplotlib.patches as patches
import numpy as np
import random

class Node:
    def __init__(self, point, left=None, right=None):
        self.point = point
        self.left = left
        self.right = right

def build_kdtree(points, depth=0):
    if not points:
        return None

    k = len(points[0])  # Assume all points have the same dimension
    axis = depth % k

    points.sort(key=lambda x: x[axis])
    median = len(points) // 2

    return Node(
        point=points[median],
        left=build_kdtree(points[:median], depth + 1),
        right=build_kdtree(points[median + 1:], depth + 1)
    )

# Function to generate distinct colors
def get_colors(num_colors):
    colors = plt.cm.get_cmap('hsv', num_colors)
    return [colors(i) for i in range(num_colors)]

def draw_kdtree(node, depth=0, min_x=0, max_x=100, min_y=0, max_y=100, ax=None, colors=None, color_index=0):
    if node is None:
        return color_index

    k = 2  # For 2D points
    axis = depth % k

    if axis == 0:  # Vertical division
        # Draw the vertical line
        ax.plot([node.point[0], node.point[0]], [min_y, max_y], 'k-')
        color = colors[color_index % len(colors)]
        ax.add_patch(patches.Rectangle((min_x, min_y), node.point[0] - min_x, max_y - min_y, color=color, alpha=0.2))
        color_index = draw_kdtree(node.left, depth + 1, min_x, node.point[0], min_y, max_y, ax, colors, color_index + 1)
        ax.add_patch(patches.Rectangle((node.point[0], min_y), max_x - node.point[0], max_y - min_y, color=color, alpha=0.2))
        color_index = draw_kdtree(node.right, depth + 1, node.point[0], max_x, min_y, max_y, ax, colors, color_index + 1)
    else:  # Horizontal division
        # Draw the horizontal line
        ax.plot([min_x, max_x], [node.point[1], node.point[1]], 'k-')
        color = colors[color_index % len(colors)]
        ax.add_patch(patches.Rectangle((min_x, min_y), max_x - min_x, node.point[1] - min_y, color=color, alpha=0.2))
        color_index = draw_kdtree(node.left, depth + 1, min_x, max_x, min_y, node.point[1], ax, colors, color_index + 1)
        ax.add_patch(patches.Rectangle((min_x, node.point[1]), max_x - min_x, max_y - node.point[1], color=color, alpha=0.2))
        color_index = draw_kdtree(node.right, depth + 1, min_x, max_x, node.point[1], max_y, ax, colors, color_index + 1)

    # Draw the node point
    #ax.plot(node.point[0], node.point[1], 'ro')

    return color_index

def generate_random_points(num_points, x_range=(0, 100), y_range=(0, 100)):
    points = []
    for _ in range(num_points):
        x = random.uniform(x_range[0], x_range[1])
        y = random.uniform(y_range[0], y_range[1])
        points.append((x, y))
    return points

points=generate_random_points(random.randint(0,5000))
kd_tree = build_kdtree(points)

num_regions = len(points)
colors = get_colors(random.randint(0,len(points)//255))

fig, ax = plt.subplots()
ax.set_xlim(0, 100)
ax.set_ylim(0, 100)

draw_kdtree(kd_tree, ax=ax, colors=colors)
plt.savefig('kd_tree_colored.png')  # Save the image
plt.show()
