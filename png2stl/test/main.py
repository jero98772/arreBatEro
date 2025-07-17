from flask import Flask, request, send_file, render_template
from PIL import Image
import numpy as np
import trimesh
import os
import io

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = 'uploads'
app.config['DOWNLOAD_FOLDER'] = 'downloads'

# Ensure upload and download folders exist
os.makedirs(app.config['UPLOAD_FOLDER'], exist_ok=True)
os.makedirs(app.config['DOWNLOAD_FOLDER'], exist_ok=True)

@app.route('/')
def index():
    """Renders the main HTML page for image upload and 3D viewing."""
    return render_template('index.html')

from flask import Flask, request, send_file, render_template
from PIL import Image
import numpy as np
import trimesh
import os
import io

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = 'uploads'
app.config['DOWNLOAD_FOLDER'] = 'downloads'

# Ensure upload and download folders exist
os.makedirs(app.config['UPLOAD_FOLDER'], exist_ok=True)
os.makedirs(app.config['DOWNLOAD_FOLDER'], exist_ok=True)

@app.route('/')
def index():
    """Renders the main HTML page for image upload and 3D viewing."""
    return render_template('index.html')

def convert_image_to_stl(image_stream: io.BytesIO) -> trimesh.Trimesh:
    """
    Converts a 2D image stream into a 3D STL mesh.
    White/transparent areas (very bright grayscale values) are ignored.
    """
    img = Image.open(image_stream)

    # Convert to grayscale with white background for transparency
    if img.mode in ('RGBA', 'LA'):
        background = Image.new("RGB", img.size, (255, 255, 255))
        background.paste(img, mask=img.split()[-1])
        img = background.convert("L")
    else:
        img = img.convert("L")

    # Normalize grayscale image (0 = black, 1 = white)
    arr = np.array(img) / 255.0
    rows, cols = arr.shape

    width_mm = cols
    height_mm = rows

    x = np.linspace(0, width_mm, cols)
    y = np.linspace(0, height_mm, rows)
    xx, yy = np.meshgrid(x, y)

    # Invert grayscale and scale for height
    extrusion_scale = 50.0
    zz = (1 - arr) * extrusion_scale

    # Mask out background areas (almost white = flat)
    threshold = 0#0.1  # Exclude values where arr > (1 - threshold)
    mask = arr < (1 - threshold)

    # Flatten arrays and apply mask
    vertices = np.column_stack((xx.flatten(), yy.flatten(), zz.flatten()))
    #mask_flat = mask.flatten()
    #valid_indices = np.nonzero(mask_flat)[0]
    #vertices = vertices[valid_indices]
    valid_indices = vertices
    # Rebuild 2D mask index mapping for valid vertex indices
    index_map = -np.ones(rows * cols, dtype=int)
    index_map[valid_indices] = np.arange(len(valid_indices))

    # Recreate faces using only valid vertices
    faces = []
    for i in range(rows - 1):
        for j in range(cols - 1):
            idx = lambda r, c: r * cols + c
            a, b, c_, d = idx(i, j), idx(i, j+1), idx(i+1, j), idx(i+1, j+1)

            if all(index_map[v] >= 0 for v in (a, b, c_)):
                faces.append([index_map[a], index_map[c_], index_map[b]])
            if all(index_map[v] >= 0 for v in (b, c_, d)):
                faces.append([index_map[b], index_map[c_], index_map[d]])

    mesh = trimesh.Trimesh(vertices=vertices, faces=faces)
    return mesh

@app.route('/upload', methods=['POST'])
def upload_and_convert_stl():
    """
    Handles image upload, calls the conversion function, and provides download link.
    """
    # 1. Validate file upload
    if 'image' not in request.files:
        return 'No image part in the request', 400
    file = request.files['image']
    if file.filename == '':
        return 'No selected image', 400

    if file:
        try:
            # Read image data into an in-memory stream
            img_stream = io.BytesIO(file.read())

            # 2. Call the image-to-STL conversion function
            mesh = convert_image_to_stl(img_stream)

            # 3. Export the generated mesh to a file
            output_stl_path = os.path.join(app.config['DOWNLOAD_FOLDER'], 'output.stl')
            mesh.export(output_stl_path)

            # 4. Return success response with download link
            return f'<a href="/download/output.stl">Download STL</a><br><button onclick="location.reload()">Upload another image</button>', 200

        except Exception as e:
            # Handle any errors during processing
            return f'Error processing image: {str(e)}', 500

@app.route('/download/<filename>')
def download_file(filename):
    """Allows users to download the generated STL file."""
    return send_file(os.path.join(app.config['DOWNLOAD_FOLDER'], filename), as_attachment=True)

if __name__ == '__main__':
    app.run(debug=True)

@app.route('/upload', methods=['POST'])
def upload_and_convert_stl():
    """
    Handles image upload, calls the conversion function, and provides download link.
    """
    # 1. Validate file upload
    if 'image' not in request.files:
        return 'No image part in the request', 400
    file = request.files['image']
    if file.filename == '':
        return 'No selected image', 400

    if file:
        try:
            # Read image data into an in-memory stream
            img_stream = io.BytesIO(file.read())

            # 2. Call the image-to-STL conversion function
            mesh = convert_image_to_stl(img_stream)

            # 3. Export the generated mesh to a file
            output_stl_path = os.path.join(app.config['DOWNLOAD_FOLDER'], 'output.stl')
            mesh.export(output_stl_path)

            # 4. Return success response with download link
            return f'<a href="/download/output.stl">Download STL</a><br><button onclick="location.reload()">Upload another image</button>', 200

        except Exception as e:
            # Handle any errors during processing
            return f'Error processing image: {str(e)}', 500

@app.route('/download/<filename>')
def download_file(filename):
    """Allows users to download the generated STL file."""
    return send_file(os.path.join(app.config['DOWNLOAD_FOLDER'], filename), as_attachment=True)

if __name__ == '__main__':
    app.run(debug=True)
