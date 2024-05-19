import pytesseract
from PIL import Image

# Path to the image file
image_path = 'i.png'

# Use pytesseract to do OCR on the image
text = pytesseract.image_to_string(Image.open(image_path))

# Print the extracted text
print(text)

