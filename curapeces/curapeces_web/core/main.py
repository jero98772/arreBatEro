#!/usr/bin/env python
# -*- coding: utf-8 -*-"
#curapeces - by jero98772

from flask import Flask, render_template, request, flash, redirect ,session
app = Flask(__name__)
class webpage():
		return render_template("index.html")

	@app.route("/")
	def curapeces():
		imgdir = "core/static/img/download/"
		sickness = ""
		#try:
		from core.wwwof.curapeces import predict
		fPredict = predict()
		checkPredict = predict()
		if request.method == 'POST':
			file = request.files["file"]
			ext = getExt(file)
			fileName = imgdir+str(img2NumName(imgdir))+"fish"+ext
			file.save(fileName)
			#remeber clear background
			clearimg(fileName)
			fPredict.pez = file
			checkPredict.pez = file
			diases= fPredict.predict()
			checkDiases = checkPredict.predict()
			outSickness = fPredict.ChosePredeictionEn(diases)
			checkSickness = fPredict.ChosePredeictionEn(checkDiases)
			if outSickness == checkSickness :
				sickness = outSickness
			else:
				sickness = "please try again"
		#except:
		#	sickness = "Error 500, you need a model for predict diase of that fish. is a error form server please report to curapeces@gmail.com"
		return render_template('wwwof/curapeces/curapeces.html',prediccion=sickness)
