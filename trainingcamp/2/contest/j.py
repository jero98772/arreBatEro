from itertools import combinations
def writetxt(name,content):
	with open(name, 'w') as file:
		file.write(content)
		file.close()
name="input.txt"
with open(name, 'r') as file:
	n,m=map(int,str(file.readline()).split())

b1=tuple(range(int(m)))
a1=tuple(combinations(b1,int(n)))
writetxt("output.txt",str(len(a1)))
