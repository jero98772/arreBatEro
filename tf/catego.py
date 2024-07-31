from tensorflow.keras.utils import to_categorical
y = [0, 1, 2, 3, 4,5,1,2]
y_one_hot = to_categorical(y)

print(y_one_hot)
