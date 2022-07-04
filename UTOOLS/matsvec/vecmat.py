import numpy as np
def vec2mat(vec):
	tmp=None
	for i in range(1,len(vec)):
		print(str(len(vec)/i)[-2:]==".0",i,str(len(vec)/i)[:-2],str(len(vec)/i)[-2:])
		if str(len(vec)/i)[-2:]==".0":
			tmp=i
	print(tmp)
	size=len(vec)/tmp
	print(tmp,size)
	mat=np.zeros((size,size))
	iii=0
	for i in range(len(vec)):
		for ii in range(len(vec[0])):
			mat[i][ii]=vec[iii]	
			#print("i",i,"ii",ii,"iii",iii)
			#print(mat[i][ii])
			#print("\nmat",mat[i][ii],vec[iii])
			iii+=1
	return mat
def mat2vec(mat):
	iii=0
	size=len(mat)*len(mat[0])
	vec=np.zeros((size))
	for i in range(len(mat)):
		for ii in range(len(mat[0])):
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