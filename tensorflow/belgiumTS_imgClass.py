def load_data(data_directory):
    directories = [d for d in os.listdir(data_directory) 
                   if os.path.isdir(os.path.join(data_directory, d))]
    labels = []
    images = []
    for d in directories:
        label_directory = os.path.join(data_directory, d)
        file_names = [os.path.join(label_directory, f) 
                      for f in os.listdir(label_directory) 
                      if f.endswith(".ppm")]
        for f in file_names:
            images.append(skimage.data.imread(f))
            labels.append(int(d))
    return images, labels

import os
from skimage import data
import matplotlib

ROOT_PATH = "/Users/haachicanoy/Documents/Datasets"
train_data_directory = os.path.join(ROOT_PATH, "BelgiumTS_dataset/Training")
test_data_directory = os.path.join(ROOT_PATH, "BelgiumTS_dataset/Testing")

images, labels = load_data(train_data_directory)

import numpy as np

images = np.array(images)
labels = np.array(labels)

# Print the `images` dimensions
print(images.ndim)
# Print the number of `images`'s elements
print(images.size)
# Print the first instance of `images`
images[0]

# Print the `labels` dimensions
print(labels.ndim)
# Print the number of `labels`'s elements
print(labels.size)
# Count the number of labels
print(len(set(labels)))

print(images.flags)
print(images.itemsize)
print(images.nbytes)

# Import the `pyplot` module
import matplotlib.pyplot as plt # PROBLEMS HERE

# Make a histogram with 62 bins of the `labels` data
plt.hist(labels, 62)

# Show the plot
plt.show()