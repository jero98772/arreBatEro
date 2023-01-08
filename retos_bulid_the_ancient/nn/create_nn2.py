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
    self.last_input=inputimg
    h,w=inputimg.shape
    out=np.zeros((h-2,w-2,self.numFilter))
    for imgRegion, i,j in self.iterate_regions(inputimg):
      out[i, j] = np.sum(imgRegion * self.filter, axis=(1, 2))
      #out[i,j]=np.sum(imgRegion*self.numFilter,axis=(1,2))
    return out
  def backprop(self,d_l_d_out,learn_rate):
    d_l_d_filter=np.zeros(self.filter.shape)
    for im_region, i ,j in self.iterate_regions(self.last_input):
      for f in range(self.numFilter):
        d_l_d_filter[f]+=d_l_d_out[i,j,f]*im_region
    self.filter-=learn_rate*d_l_d_filter
    return None
class MaxPool():
  def iterate_regions(self,img):
    h,w,_=img.shape
    new_h=h//2
    new_w=w//2
    for i in range(new_h):
      for ii in range(new_w):
        yield img[(i*2):(i*2+2),(ii*2):(ii*2+2)], i, ii 
  def forward(self,inp):
    self.last_input=inp
    h,w,numFilter=inp.shape
    output=np.zeros((h//2,w//2,numFilter))  
    for im_region,i,ii in self.iterate_regions(inp):
      output[i,ii]=np.amax(im_region,axis=(0,1))
    return output
  def backprop(self,d_l_d_out):
    d_L_d_input=np.zeros(self.last_input.shape)
    for im_region,i,j in self.iterate_regions(self.last_input):
      h,w,f=im_region.shape
      amax=np.amax(im_region,axis=(0,1))
      for ii in range(h):
        for jj in range(w):
          for ff in range(f):
            if im_region[ii,jj,ff]==amax[ff]:
              d_L_d_input[i*2+ii,j*2+jj,ff]=d_l_d_out[i,j,ff]
    return d_L_d_input
class softmax():
	def __init__(self,input_len,nodes):
		self.weigths=np.random.randn(input_len,nodes)/input_len
		self.biases=np.zeros(nodes)
	def forward(self,input):
		self.last_input_shape=input.shape
		input=input.flatten()
		self.last_input=input
		input_len,nodes=self.weigths.shape
		totals=np.dot(input,self.weigths)+self.biases
		self.last_totals=totals
		exp=np.exp(totals)
		return exp/np.sum(exp,axis=0)
	def backprop(self,d_l_d_out,learn_rate):
		for i,gradient in enumerate(d_l_d_out):
			if gradient==0:
				continue
			t_exp=np.exp(self.last_totals)
			S=np.sum(t_exp)
			d_out_d_t=-t_exp[i]*t_exp/(S**2) 
			d_out_d_t[i]=t_exp[i]*(S-t_exp[i])/(S**2)
			d_t_d_w=self.last_input
			d_t_d_b=1
			d_t_d_input=self.weigths
			d_L_d_t=gradient*d_out_d_t
			d_L_d_w=d_t_d_w[np.newaxis].T@d_L_d_t[np.newaxis]
			d_L_d_b=d_L_d_t * d_t_d_b
			d_L_d_input=d_t_d_input@d_L_d_t
			self.weigths-=learn_rate*d_L_d_w
			self.biases-=learn_rate*d_L_d_b
			return d_L_d_input.reshape(self.last_input_shape)
#def softmax(xs):
#	return np.exp(xs)/np.sum(np.exp(xs))
def forward(image,label):
  out=conv.forward((image/255)-0.5)
  out=pool.forward(out)
  out=Softmax.forward(out)
  
  loss=-np.log(out[label])
  acc=1 if np.argmax(out)==label else 0
  return out,loss,acc
def train(im,label,lr=0.005,outlayer=10):
  out,loss,acc=forward(im,label)
  gradient=np.zeros(outlayer)
  gradient[label]=-1/out[label]
  gradient=Softmax.backprop(gradient,lr)
  gradient=pool.backprop(gradient)
  gradient=conv.backprop(gradient,lr)
  return loss,acc
conv=convnxm(8,3,3)
pool=MaxPool()
Softmax=softmax(13*13*8,10)
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
     l,acc=train(im,label)
     loss+=l
     num_correct+=acc
main()
