#!/usr/bin/env python
# -*- coding: utf-8 -*-"
#popuml - by jero98772
class uml():
	"""uml"""
	def __init__(self, txt,sep="="):
		self.uml=txt.split(sep)
		self.className=uml[0]
		self.arguments=uml[1]
		self.methots=uml[2]
		self.code=""
	#def file():
	#def string():
		
class uml2Python(uml):
	"""uml2Python"""
	def __init__(self, txt,sep="="):
		super(uml2Python, self).__init__()
		super.code=f"""class 
"""

class ocr():
	def __init__(self, arg):
		pass
