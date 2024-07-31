import tensorflow as tf
matrix1 = tf.constant([[1, 2], [3, 4]], dtype=tf.float32)
matrix2 = tf.constant([[5, 6], [7, 8]], dtype=tf.float32)

matrix_sum = tf.add(matrix1, matrix2)
matrix_diff = tf.subtract(matrix1, matrix2)
matrix_product = tf.matmul(matrix1, matrix2)
elementwise_product = tf.multiply(matrix1, matrix2)
elementwise_product = tf.multiply(matrix1, matrix2)
print(matrix_sum)
print(matrix_diff)
print(matrix_product)
print(elementwise_product)
print(elementwise_product)