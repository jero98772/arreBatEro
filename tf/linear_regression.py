import tensorflow as tf
import numpy as np
xs=np.array([1,2,3,4,5,6],dtype=float)
ys=np.array([150,300,450,600,750,900],dtype=float)

model=tf.keras.Sequential([tf.keras.layers.Dense(units=1,input_shape=[1])])

model.compile(optimizer='sgd',loss='mean_squared_error')

model.fit(xs,ys,epochs=10)