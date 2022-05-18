#https://medium.com/swlh/decoding-noaa-satellite-images-using-50-lines-of-code-3c5d1d0a08da
import scipy.io.wavfile as wav
import scipy.signal as signal
import numpy as np
from flask import Flask, render_template,redirect,request
import datetime
import time

SAVESFOLDER="saves/"

app=Flask(__name__)


def hilbert(data):
    analytical_signal = signal.hilbert(data)
    amplitude_envelope = np.abs(analytical_signal)
    return amplitude_envelope
#data_am = hilbert(data)
def cleardata(name):
    print(name)
    fs, data = wav.read(SAVESFOLDER+name)
    resample = 4
    data = data[::resample]
    fs = fs//resample
    print("error?2")
    data_am = hilbert(data)
    return data_am ,fs

@app.route("/",methods=['GET','POST'])
def main():
    if request.method == 'POST':
        name="wavnoaa"+str(datetime.datetime.now()).replace(" ","").replace("-","").replace(":","").replace(".","")+".wav"
        file = request.files["file"]
        file.save(SAVESFOLDER+name)
        time.sleep(3)
        print("redirect")
        return redirect("out/"+name)
    return render_template("index.html")

@app.route("/out/<string:name>")
def out(name):
    #data_crop = data[20*fs:21*fs]
    print("error?")
    data_am,fs=cleardata(name)
    #print("error?")
    print(data_am)
    return render_template("out.html",hilbertData=list(data_am),fs=fs)
if __name__ == '__main__':
    app.run(debug=True,host="0.0.0.0",port=9600)