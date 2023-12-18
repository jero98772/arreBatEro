from segment_anything import sam_model_registry
from urllib.request import urlopen
from segment_anything import SamAutomaticMaskGenerator
import cv2
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import numpy as np
###variables
MODEL_TYPE = "vit_h"
CHECKPOINT_PATH = "sam_vit_h_4b8939.pth"
DEVICE = "cuda"
img='https://images.unsplash.com/photo-1615948812700-8828458d368a?ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&w=2072&q=80'

def draw_masks_fromDict(image, masks_generated) :
  masked_image = image.copy()
  for i in range(len(masks_generated)) :
    masked_image = np.where(np.repeat(masks_generated[i]['segmentation'].astype(int)[:, :, np.newaxis], 3, axis=2),
                            np.random.choice(range(256), size=3),
                            masked_image)

    masked_image = masked_image.astype(np.uint8)

  return cv2.addWeighted(image, 0.3, masked_image, 0.7, 0)


def show(img,name='image'):
	cv2.imshow('image',img)
	cv2.waitKey(0) 
	cv2.destroyAllWindows() 

def loadImage(img):

	resp = urlopen(img)
	imageorg = np.asarray(bytearray(resp.read()), dtype='uint8')
	image1 = cv2.imdecode(imageorg, cv2.IMREAD_COLOR)
	image2 = cv2.resize(image1, (int(image1.shape[1]/2.5), int(image1.shape[0]/2.5)))
	return image2

def maskimage(image):
	masks_generated = mask_generator.generate(image)
	segmented_image = draw_masks_fromDict(image, masks_generated)
	return segmented_image


def main():
	sam = sam_model_registry[MODEL_TYPE](checkpoint=CHECKPOINT_PATH).to(device=DEVICE)
	mask_generator = SamAutomaticMaskGenerator(sam)
	img=loadImage(img)
	show(img)
	mask=maskimage(img)
	show(mask)
