class node:
	def __init__(self,value,next=None):
		self.value=value
		self.next=next
class graph:
	def __init__(self):
		self.graph={}
	#def add()

def main():
	n1=node(1)
	n2=node(2)
	n1.next=n2
	n3=node(3)
	#n2.next=n
	tmp=n1
	while tmp.next:
		print(tmp.value)
		tmp=tmp.next
main()