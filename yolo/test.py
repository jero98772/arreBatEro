import torch
from PIL import Image

def detect_objects_yolov5(image_path):
    # Load YOLOv5 model
    model = torch.hub.load('ultralytics/yolov5', 'yolov5s', pretrained=True)

    # Load image
    img = Image.open(image_path)

    # Perform inference
    results = model(img)

    # Display results
    results.show()

if __name__ == '__main__':
    image_path = '2.jpg'  # Change this to the path of your image
    detect_objects_yolov5(image_path)

