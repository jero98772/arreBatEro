from PIL import Image
import numpy as np
import trimesh

# Load image and remove transparency by pasting on white background
img = Image.open("input.png")
if img.mode in ('RGBA', 'LA'):
    background = Image.new("RGB", img.size, (255, 255, 255))
    background.paste(img, mask=img.split()[-1])  # paste using alpha channel
    img = background.convert("L")  # convert to grayscale
else:
    img = img.convert("L")

# Convert to normalized grayscale array
arr = np.array(img) / 255.0

rows, cols = arr.shape
width_mm = cols  # Preserve dimensions: 1 pixel = 1 mm
height_mm = rows

# Generate X, Y based on image dimensions in mm
x = np.linspace(0, width_mm, cols)
y = np.linspace(0, height_mm, rows)
xx, yy = np.meshgrid(x, y)
zz = arr * 5  # Extrusion height scale in mm

# Create vertices
vertices = np.column_stack((xx.flatten(), yy.flatten(), zz.flatten()))

# Create faces
faces = []
for i in range(rows - 1):
    for j in range(cols - 1):
        idx = lambda r, c: r * cols + c
        a = idx(i, j)
        b = idx(i, j+1)
        c = idx(i+1, j)
        d = idx(i+1, j+1)
        faces.append([a, c, b])
        faces.append([b, c, d])

# Build and export mesh
mesh = trimesh.Trimesh(vertices=vertices, faces=faces)
mesh.export("output.stl")
print("✅ STL saved to output.stl")
