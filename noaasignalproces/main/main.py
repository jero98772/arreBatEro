#https://medium.com/swlh/decoding-noaa-satellite-images-using-50-lines-of-code-3c5d1d0a08da
import scipy.io.wavfile as wav
import scipy.signal as signal
import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
from flask import Flask, render_template,redirect,request
import datetime

SAVESFOLDER="static/wav/"
IMGFOLDER="static/img/"

app=Flask(__name__)

@app.route("/",methods=['GET','POST'])
def main():
    if request.method == 'POST':
        name="wavnoaa"+str(datetime.datetime.now()).replace(" ","").replace("-","").replace(":","").replace(".","")+".wav"
        file = request.files["file"]
        file.save(SAVESFOLDER+name)
        try:
            resample=request.form["resample"]
            print(resample,type(resample))
        except :
            resample=None
        if resample:
            return redirect("outr/"+name)  
        else:  
            return redirect("out/"+name)
        #time.sleep(3)
        #print("redirect")
    return render_template("index.html")
@app.route("/outr/<string:name>")
def outr(name):
    fs, data = wav.read(SAVESFOLDER+name)  
    data_crop = data[20*fs:21*fs]
    analytical_signal = signal.hilbert(data)
    data_am = np.abs(analytical_signal)
    frame_width = int(0.5*fs)
    w, h = frame_width, data_am.shape[0]//frame_width
    image = Image.new('RGB', (w, h))
    px, py = 0, 0
    #fill image
    for p in range(data_am.shape[0]):
        lum = int(data_am[p]//32 - 32)
        if lum < 0: lum = 0
        if lum > 255: lum = 255
        image.putpixel((px, py), (0, lum, 0))
        px += 1
        if px >= w:
            #if (py % 50) == 0:
            #    print(f"Line saved {py} of {h}")
            px = 0
            py += 1
            if py >= h:
                break
    image = image.resize((w, 4*h))
    plt.imshow(image)
    #plt.show()
    filename=name.replace(".wav",".png")
    plt.savefig(IMGFOLDER+filename)
    return render_template("out.html",name="img/"+filename)

@app.route("/out/<string:name>")
def out(name):
    fs, data = wav.read(SAVESFOLDER+name)  
    data_crop = data[20*fs:21*fs]
    resample = 4
    data = data[::resample]
    fs = fs//resample
    analytical_signal = signal.hilbert(data)
    data_am = np.abs(analytical_signal)
    frame_width = int(0.5*fs)
    w, h = frame_width, data_am.shape[0]//frame_width
    image = Image.new('RGB', (w, h))
    px, py = 0, 0
    #fill image
    for p in range(data_am.shape[0]):
        lum = int(data_am[p]//32 - 32)
        if lum < 0: lum = 0
        if lum > 255: lum = 255
        image.putpixel((px, py), (0, lum, 0))
        px += 1
        if px >= w:
            #if (py % 50) == 0:
            #    print(f"Line saved {py} of {h}")
            px = 0
            py += 1
            if py >= h:
                break
    image = image.resize((w, 4*h))
    plt.imshow(image)
    #plt.show()
    filename=name.replace(".wav",".png")
    plt.savefig(IMGFOLDER+filename)
    return render_template("out.html",name="img/"+filename)
try:
    if __name__ == '__main__':
        app.run(debug=True,host="0.0.0.0",port=9600)
except :
    if __name__ == '__main__':
        app.run(debug=True,host="0.0.0.0",port=9600)
"""
if __name__ == '__main__':
    app.run(debug=True,host="0.0.0.0",port=9600)

"""