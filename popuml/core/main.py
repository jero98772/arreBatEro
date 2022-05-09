#!/usr/bin/env python
# -*- coding: utf-8 -*-"
#popuml - by jero98772

from flask import Flask, render_template, request, flash, redirect ,session
app = Flask(__name__)
class webpage():
  @app.route()
  def index():
    return render_template("index.html")
      