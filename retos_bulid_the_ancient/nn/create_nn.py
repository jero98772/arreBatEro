#my implementation of https://victorzhou.com/blog/intro-to-cnns-part-1/
import numpy as np
import mnist
class convnxm:
  def __init__(self,numFilter,n,m):
     self.n=n
     self.m=m
     self.numFilter=numFilter
     self.filter=np.random.rand(numFilter,n,m)/n*m
  def  iterate_regions(self,image):
     h,w=image.shape
     for i in range(h-2):
        for j in range(w-2):
            yield image[i:(i+3),j:(j+3)] , i ,j
  def forward(self, inputimg):
    h,w=inputimg.shape
    out=np.zeros((h-2,w-2,self.numFilter))
    for imgRegion, i,j in self.iterate_regions(inputimg):
      out[i, j] = np.sum(imgRegion * self.filter, axis=(1, 2))
      #out[i,j]=np.sum(imgRegion*self.numFilter,axis=(1,2))
    return out
class MaxPool():
  def iterate_regions(self,img):
    h,w,_=img.shape
    new_h=h//2
    new_w=w//2
    for i in range(new_h):
      for ii in range(new_w):
        yield img[(i*2):(i*2+2),(ii*2):(ii*2+2)], i, ii 
  def forward(self,inp):
    h,w,numFilter=inp.shape
    output=np.zeros((h//2,w//2,numFilter))  
    for im_region,i,ii in self.iterate_regions(inp):
      output[i,ii]=np.amax(im_region,axis=(0,1))
    return output
class softmax():
	def __init__(self,input_len,nodes):
		self.weigths=np.random.randn(input_len,nodes)/input_len
		self.biases=np.zeros(nodes)
	def forward(self,input):
		input=input.flatten()
		input_len,nodes=self.weigths.shape
		totals=np.dot(input,self.weigths)+self.biases
		exp=np.exp(totals)
		return exp/np.sum(exp,axis=0)

#def softmax(xs):
#	return np.exp(xs)/np.sum(np.exp(xs))
def forward(image,label):
  conv=convnxm(8,3,3)
  pool=MaxPool()
  Softmax=softmax(13*13*8,10)
  out=conv.forward((image/255)-0.5)
  out=pool.forward(out)
  out=Softmax.forward(out)
  loss=-np.log(out[label])
  acc=1 if np.argmax(out)==label else 0
  return out,loss,acc
def main():
   loss=0
   num_correct=0
   train_img=mnist.train_images()
   label_img=mnist.train_labels()

   test_images=mnist.test_images()
   test_labels=mnist.test_labels()
   for i,(im,label) in enumerate(zip(test_images,test_labels)):
     _,l,acc=forward(im,label)
     loss+=l
     num_correct+=acc
     if i % 100 == 99:
      print('[Step %d] Past 100 steps: Average Loss %.3f | Accuracy: %d%%' %(i + 1, loss / 100, num_correct))
      loss = 0
      num_correct = 0
main()
