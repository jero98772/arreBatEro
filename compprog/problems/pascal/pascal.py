import numpy as np
rows=int(input())
pascal=[]
tmp=None
for i in range(rows):
	tmp=[]
	for ii in range(i+1):
		if i>=2:
			if ii==0 or ii>=i:
				tmp.append(1)
			else:
				tmp.append(pascal[i-1][ii]+pascal[i-1][ii-1])		
		else:
			tmp.append(1)
	pascal.append(tmp)

print(pascal)