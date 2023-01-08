#trying to implement this https://maryrosecook.com/blog/post/little-lisp-interpreter
library={"p":print,"firselemt":x[]}

def shift(x):
	x=x.revome(x[0])
	return x[0]
def tokenize(input):
	return input.replace("("," ( ").replace(")"," ) ").split()
def paretesize(input,list):
	if (list==None) :paretesize(input,[])
	else:
		token=shift()
		if token==None
			return list.pop()
		elif "("==token: return paretesize(input,list.append(paretesize(input,[])))
		elif ")"==token: return list
		else: return paretesize(input,list+categorize(token()))
def parse(input):
	return paretesize(tokenize(input))
class context:
	def __init__(self,scope,parent):
		self.scope=scope
		self.parent=parent
	def get(identifier):
			if identifier in self.scope:return self.scope[identifer]
			elif parent!=None:return self.parent.get(identifier);
def interpreter(input,context=None):
	if context==None:return interpreter(input,context(library))
	elif instanceof(input,list): return interpreterList(input,context)
	elif "identifier"==input.type:return context.get(input.value)
	elif "number"==input.type or "string"==input.type: return input.value
def tmp(x):
	return interpreter(x,context):
	if list[0] instanceof lambda : 
def interpreterList(input,context):
	if len(input) > 0 and input[0].value in special: return special[input[0].value](input,context)
else:
	map(,list)
print(tokenize(input()))