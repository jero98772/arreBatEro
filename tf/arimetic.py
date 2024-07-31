import tensorflow as tf
a=tf.constant(10)
b=tf.constant(5)

add=tf.add(a,b)
sub=tf.subtract(a,b)
mult=tf.multiply(a,b)
div=tf.divide(a,b)
print([add,sub,mult,div])