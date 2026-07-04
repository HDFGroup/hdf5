#
# This example shows how to read a hyperslab from an existing dataset.
#
import h5py
import numpy as np
#
# Open file and read dataset.
#
file = h5py.File('hype.h5', 'r')
dataset = file['IntArray']
data_in_file = dataset[...]
print("Data in file ...")
print(data_in_file[...])
#
# Initialize data with 0s.
#
data_selected = np.zeros((8,10), dtype=np.int32)
#
# Read selection.
#
space_id = dataset.id.get_space()
space_id.select_hyperslab((1,1), (2,2), stride=(4,4), block=(2,2))
#---> Doesn't work dataset.id.read(space_id, space_id, data_selected, h5py.h5t.STD_I32LE) 
dataset.id.read(space_id, space_id, data_selected) 
print("Selected data read from file....")
print(data_selected[...])
#
# Close the file before exiting
#
file.close()

