#https://www.datacamp.com/tutorial/markov-chains-python-tutorial
import random
data={"pa":[0.2,0.2,0.1,0.1,0.4],"pu":[0.1,0.2,0.2,0.1,0.4],"pi":[0.5,0.01,0.29,0.1,0.5],"po":[0.1,0.3,0.01,0.4,0.1],"pe":[0.1,0.3,0.01,0.5,0]}


def markovchain(data,times=1,start=None):
	values=[]
	value="pa"
	for i in range(100):
		value=random.choices(list(data.keys()),weights=data[value],k=1)	
		value=value[0]	
		values.append(value)
	return values
def getprob(l):
	clearlist=list(set(l))
	prob={}
	for i in clearlist:
		prob[str(i)]=l.count(i)/len(l)
		
		