# TensorFlow basics using a virtualenv on MBP
# H. Achicanoy, 2016

# Load and import tensorflow library
import tensorflow as tf

# Define some values
x1 = tf.constant([5])
x2 = tf.constant([6])

# Make an operation between values
# result = x1 * x2
result = tf.mul(x1, x2)

print(result)

# Open and close sessions
#sess = tf.Session() # Start the session
#print(sess.run(result)) # Print results
#sess.close() # Close the session

# Another way to do that
with tf.Session() as sess:
    output = sess.run(result)
    print(output)

print(output)