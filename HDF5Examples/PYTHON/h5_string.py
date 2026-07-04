#
# This example creates an HDF5 file string.h5 and DSfixed dataset in it.
# Then it opens the file and reads data back.
#
import h5py
import numpy as np
#
# Create a new file using default properties.
#
file = h5py.File('string.h5','w')
#
# Create a dataset under the Root group using variable-length string type.
#

fixed_string = np.dtype('a10')
dataset = file.create_dataset("DSfixed",(4,), dtype=fixed_string)
data = ("Parting   ", ".is such  ", ".sweet    ", ".sorrow...")
dataset[...] = data
#
# Close the file before exiting
#
file.close()
file = h5py.File('string.h5', 'r')
dataset = file['DSfixed']
data_out = dataset[...]
for i in range(4):	
    print("DSfixed[i] = ",data_out[i])

file.close()
