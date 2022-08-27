import random
data={"pa":[0.2,0.2,0.1,0.1,0.4],"pu":[0.1,0.2,0.2,0.1,0.4],"pi":[0.5,0.01,0.29,0.1,0.5],"po":[0.1,0.3,0.01,0.4,0.1],"pe":[0.1,0.3,0.01,0.5,0]}
values=[]
value="pa"
for i in range(8):
	value=random.choices(list(data.keys()),weights=data[value],k=1)	
	value=value[0]	
	values.append(value)
print(values)