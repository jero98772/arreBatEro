#!/usr/bin/env python 
# -*- coding: utf-8 -*-"
"""
my_good_site - 2020 - por jero98772
my_good_site - 2020 - by jero98772
"""
import numpy as np
import tensorflow as tf
import os

from tensorflow.keras.preprocessing.image import ImageDataGenerator,load_img, img_to_array
from tensorflow.python.keras import optimizers
from tensorflow.python.keras import backend as k
from tensorflow.python.keras.models import Sequential,load_model
from tensorflow.python.keras.layers import Dropout, Flatten, Dense, Activation,Convolution2D, MaxPooling2D
class curapeces():
    #K.clear_session()
    directorio="./static/www_of/img/datos_limpios"
    pruebas = 1
    alturadelaimagen = 32
    longituddelaimagen= 32
    numerodeimagenesamandar=4 
    pasos=400
    filtroprimeravez= 32
    filtrosegundavez= 64
    filtroterceravez= 32
    filtrocurtavez =128
    filtroquintavez =256
    filtrouno=(3,3)
    filtrodos=(2,2)
    filtrotres=(3,3)
    filtrocutro=(4,4)
    filtroquinto=(5,5)
    pulido=(2,2)
    numerodenfermedades=14
    lr = 0.0004
    def image(self):
        entrenamiento_datagen = ImageDataGenerator(
            rescale=1. / 255,
            shear_range=0.2,
            zoom_range=0.2,
            horizontal_flip=True)
        test_datagen = ImageDataGenerator(rescale=1. / 255)
        self.entrenamiento_generador = entrenamiento_datagen.flow_from_directory(
            self.directorio,
            target_size=(self.alturadelaimagen, self.longituddelaimagen),
            batch_size=self.numerodeimagenesamandar,
            class_mode='categorical')

        self.validacion_generador = test_datagen.flow_from_directory(
            self.directorio,
            target_size=(self.alturadelaimagen, self.longituddelaimagen),
            batch_size=self.numerodeimagenesamandar,
            class_mode='categorical')
    def nn(self):
        nn = Sequential()
        nn.add(Convolution2D(self.filtroprimeravez, self.filtrouno, padding ="same", input_shape=(self.longituddelaimagen, self.alturadelaimagen , 3), activation='relu'))
        nn.add(MaxPooling2D(pool_size=self.pulido))
        nn.add(Convolution2D(self.filtrosegundavez, self.filtrodos, padding ="same"))
        nn.add(MaxPooling2D(pool_size=self.pulido))
        nn.add(Flatten())
        nn.add(Dense(256, activation='relu'))
        nn.add(Dropout(0.1))
        nn.add(Dense(self.numerodenfermedades, activation='softmax'))
        nn.compile(loss='categorical_crossentropy',optimizer=optimizers.Adam(lr=self.lr),metrics=['accuracy'])
        nn.fit_generator(self.entrenamiento_generador,steps_per_epoch=self.pasos,epochs=self.pruebas,validation_data=self.validacion_generador,validation_steps=self.validacon)
        #nn.save('./modelo_lab_experimental/modelo_pezenfermo.h5')
        #nn.save_weights('./modelo_lab_experimental/pesospezenfermo.h5')
        return nn
    def save_nn(self,path):
        self.nn=curapeces.nn()
        numFolder = len(os.listdir(path))
        target_dir = path+"/model"+str(numFolder)
        os.mkdir(target_dir)
        self.nn.save(str(self.target_dir)+ '/model.h5')
        self.nn.save_weights(str(self.target_dir) +'/weights.h5')
class predict():
    pez = ""
    numfolders = 0
    #imagenpez = cv2.imread(pez, cv2.IMREAD_COLOR)
    modelsFolder = "data/modelsAI/"
    numfolders = len(os.listdir(modelsFolder))
    modelfolder =modelsFolder+str(numfolders)
    model = modelfolder+"/model.h5"
    weights = modelfolder+"/weights.h5"
    longitud, altura = 500, 500
    """
    def display_image(self):
        cv2.imshow ('ventana1',self.imagenpez)
        cv2.waitKey(0)
        cv2.destroyAllWindows()
    """
    def predict(self):
      self.nn =  load_model(self.model)
      self.nn.load_weights(self.weights)
      x = load_img(self.pez, target_size=(self.longitud, self.altura))
      x = img_to_array(x)
      x = np.expand_dims(x, axis=0)
      array = self.nn.predict(x)
      result = array[0]
      self.answer = np.argmax(result)
    def pChosePredeictionEs(self,answer):
      if not answer:
        self.answer = answer
      if self.answer == 0:
        print("prediccion:  atcado o tumor y deformidad")
      elif self.answer ==1 :
        print("prediccion: branquias ")
      elif self.answer ==2 :
        print("prediccion: girodactilo ")
      elif self.answer == 3:
        print("prediccion: gusano lernea ")
      elif self.answer ==4 :
        print("prediccion: hidropecia ")
      elif self.answer == 5:
        print("prediccion: hongos")
      elif self.answer ==6 :
        print("prediccion: huecos en la cabesa")
      elif self.answer == 7 :
        print("prediccion: ich ")
      elif self.answer ==8 :
        print("prediccion: no es un pez")
      elif self.answer == 9:
        print("prediccion: ojo picho ")
      elif self.answer == 10:
        print("parasito en la lengua")
      elif self.answer == 11:
        print("prediccion: podredumbre de aletas ")
      elif self.answer == 12:
        print("prediccion: quemadura de bagre ")
      elif self.answer == 13:
        print("prediccion: es un pez sano")
      return self.answer
    def ChosePredeictionEs(self,answer):
      if not answer:
        self.answer = answer
      if self.answer == 0:
        sickness="prediccion:  atcado o tumor y deformidad"
      elif self.answer ==1 :
        sickness="prediccion: branquias "
      elif self.answer ==2 :
        sickness="prediccion: girodactilo "
      elif self.answer == 3:
        sickness="prediccion: gusano lernea "
      elif self.answer ==4 :
        sickness="prediccion: hidropecia "
      elif self.answer == 5:
        sickness="prediccion: hongos"
      elif self.answer ==6 :
        sickness="prediccion: huecos en la cabesa"
      elif self.answer == 7 :
        sickness="prediccion: ich "
      elif self.answer ==8 :
        sickness="prediccion: no es un pez"
      elif self.answer == 9:
        sickness="prediccion: ojo picho "
      elif self.answer == 10:
        sickness="parasito en la lengua"
      elif self.answer == 11:
        sickness="prediccion: podredumbre de aletas "
      elif self.answer == 12:
        sickness="prediccion: quemadura de bagre "
      elif self.answer == 13:
        sickness="prediccion: es un pez sano"
      return sickness
    def ChosePredeictionEn(self,answer):
      if not answer:
        self.answer = answer
      if self.answer == 0:
        sickness="prediccion: attacked ,tumor or deformity"
      elif self.answer ==1 :
        sickness="prediccion: sick gills "
      elif self.answer ==2 :
        sickness="prediccion: girodactilo "
      elif self.answer == 3:
        sickness="prediccion: lernea worm"
      elif self.answer ==4 :
        sickness="prediccion: hidropecia "
      elif self.answer == 5:
        sickness="prediccion: fungus"
      elif self.answer ==6 :
        sickness="prediccion: holes in the head"
      elif self.answer == 7 :
        sickness="prediccion: ich "
      elif self.answer ==8 :
        sickness="prediccion: not is a fish"
      elif self.answer == 9:
        sickness="prediccion: Popeye eye"
      elif self.answer == 10:
        sickness="prediccion: parasite in the mouth"
      elif self.answer == 11:
        sickness="prediccion: fin rot "
      elif self.answer == 12:
        sickness="prediccion: skin burn "
      elif self.answer == 13:
        sickness="prediccion: your fish is ok "
      return sickness
#curapeces=curapeces()
#curapeces.image()
#curapeces.save_nn("where going to save the model file?")
#predict=predict()
#predict.display_image()#
#print(predict.predict())
#print(predict.model)
