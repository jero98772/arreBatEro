import numpy as np
import math
import cv2

# Read the image from a file

def rotate_image(image, angle):
    angle_rad = math.radians(angle)
    cos_angle = math.cos(angle_rad)
    sin_angle = math.sin(angle_rad)
    
    # Get original image dimensions
    h, w = image.shape[:2]
    
    # Compute the new dimensions
    new_h = int(abs(h * cos_angle) + abs(w * sin_angle))
    new_w = int(abs(w * cos_angle) + abs(h * sin_angle))
    
    # Create the output image
    rotated_image = np.zeros((new_h, new_w, 3), dtype=image.dtype)
    
    # Find the center of the old and new images
    center_old = np.array([w / 2, h / 2])
    center_new = np.array([new_w / 2, new_h / 2])
    
    # Iterate over every pixel in the new image
    for y in range(new_h):
        for x in range(new_w):
            # Translate the pixel to the origin
            pixel = np.array([x, y]) - center_new
            
            # Rotate the pixel
            old_x = int(pixel[0] * cos_angle + pixel[1] * sin_angle + center_old[0])
            old_y = int(-pixel[0] * sin_angle + pixel[1] * cos_angle + center_old[1])
            
            # If the pixel is within the bounds of the old image, copy the pixel value
            if 0 <= old_x < w and 0 <= old_y < h:
                rotated_image[y, x] = image[old_y, old_x]
    
    return rotated_image

# Example usage with a dummy image (replace with actual image matrix)
image = np.random.randint(0, 255, (100, 100, 3), dtype=np.uint8)
image = cv2.imread('1.png')
rotated_image = rotate_image(image, 145)


cv2.imshow('Loaded Image', rotated_image)
cv2.waitKey(0)  # Wait for a key press to close the window
cv2.destroyAllWindows()

# rotated_image now contains the rotated image
