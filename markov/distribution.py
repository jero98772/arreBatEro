import random
def distribution(data,times):
	values=[]
	for i in range(times):
		value=random.choices(list(data.keys()),weights=data[value],k=1)	
		value=value[0]	
		values.append(value)
	print(values)