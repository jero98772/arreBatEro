import numpy as np
def vec2mat(vec):
	size=int(len(vec)**(1/2))
	mat=np.zeros((size,size))
	iii=0
	for i in range(size):
		for ii in range(size):
			mat[i][ii]=vec[iii]	
			#print("i",i,"ii",ii,"iii",iii)
			#print(mat[i][ii])
			#print("\nmat",mat[i][ii],vec[iii])
			iii+=1
	return mat
def mat2vec(mat):
	iii=0
	size=len(mat)
	vec=np.zeros((size**2))
	for i in range(size):
		for ii in range(size):
			#print("i",i,"ii",ii,"iii",iii)
			vec[iii]=mat[i][ii]	
			iii+=1
	return vec
def main():
	vec=input().split()
	np.asarray(vec)
	mat=vec2mat(vec)
	print(mat)	
	print(mat2vec(mat))
	
main()