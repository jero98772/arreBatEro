import tensorflow as tf

# Create a dense layer with 10 units and ReLU activation
dense_layer = tf.keras.layers.Dense(units=10, activation='relu')

# Create some input data
input_data = tf.random.normal(shape=(1, 5))

# Perform a forward pass through the dense layer
output_data = dense_layer(input_data)

print(output_data)
