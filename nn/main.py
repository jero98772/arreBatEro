import numpy as np
import mnist
class convnxm:
  def __init__(self,numFilter,n,m):
     self.n=n
     self.m=m
     self.numFilter=numFilter
     self.filter=np.random.rand(n,m,numFilter)/n*m
  def  iterate_regions(self,image):
     h,w,_=image.shape
     for i in range(h-2):
        for j in range(w-2):
            yield image[i:(i+3),j:(j+3)] , i ,j
  def forward(self, inputimg):
    h,w,_=inputimg.shape
    out=np.zeros((h-2,w-2,self.numFilter))
    for imgRegion, i,j in self.iterate_regions(inputimg):
      out[i,j]=np.sum(imgRegion*self.numFilter,axis=(1,2))
    return out
class MaxPool():
  def iterate_regions(self,img):
    h,w,_=image.shape
    new_h=h//2
    new_w=w//2
    for i in range(new_h):
      for ii in range(new_w):
        yield image[(i*2):(i*2+2),(ii*2):(ii*2+2)], i, ii 
  def forward(self,inp):
    h,w,numFilter=inp.shape
    output=np.zeros((h//2,w//2,numFilter))  
    for im_region,i,ii in self.iterate_regions(inp):
      output[i,ii]=np.amax(im_region,axis=(0,1))
    return output
def main():
   train_img=mnist.train_images()
   label_img=mnist.train_labels()
   conv=convnxm(8,3,3)
   pool=MaxPool()
   out=conv.forward(train_img)
   print(out)
   out=pool.forward(out)
   print(out)
main()