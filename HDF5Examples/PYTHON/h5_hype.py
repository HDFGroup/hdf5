#
# This example shows how to write a hyperslab to an existing dataset.
#
import h5py
import numpy as np
#
# Create a file using default properties.
#
file = h5py.File('hype.h5','w')
#
# Create "IntArray" dataset.
#
dim0 = 8
dim1 = 10
dataset = file.create_dataset("IntArray", (dim0,dim1), "i")
#
# Initialize data object with 0.
#
data = np.zeros((dim0, dim1))
#
# Initialize data for writing.
#
for i in range(dim0):
    for j in range(dim1):
        if j < dim1/2: 
            data[i][j]= 1
        else:
            data[i][j] = 2 	 
#
# Write data
#
dataset[...] = data
print("Data written to file:")
print(dataset[...])
#
# Close the file before exiting
#
file.close()
#
# Open the file and dataset.
#
file = h5py.File('hype.h5','r+')
dataset = file['IntArray']
#
# Write a selection.
#
dataset[1:4, 2:6] = 5
print("Data after selection is written:")
print(dataset[...])
#
# Close the file before exiting
#
file.close()

