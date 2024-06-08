#
# This example creates a dataset and then extends it by rows and then by columns.
#
import h5py
import numpy as np
#
# Create  unlim.h5 file.
#
file = h5py.File('unlim.h5','w')
#
# Create /DS1 dataset; in order to use compression, dataset has to be chunked.
#
dataset = file.create_dataset('DS1',(4,7),'i',chunks=(3,3), maxshape=(None, None)) 
#
# Initialize data.
#
data = np.zeros((4,7))
#
# Write data.
#
print("Writing data...")
dataset[...] = data
file.close()
#
# Read data back; display compression properties and dataset max value. 
#
file = h5py.File('unlim.h5','r+')
dataset = file['DS1']
data = dataset[...]
print("Data before extension: ")
print(data)
#
# Add two rows filled with 1
#
dataset.resize((6,7))
dataset[4:6] = 1
#
# Add three columns filled with 2 
#
dataset.resize((6,10))
dataset[:,7:10] = 2 
data = dataset[...]
print("Data after extension: ")
print(data)
file.close()

