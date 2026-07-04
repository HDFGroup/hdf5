#
# This example reads integer data from dset.h5 file into Python floatng buffers.
#
import h5py
import numpy as np
#
# Open an existing file using default properties.
#
file = h5py.File('dset.h5','r+')
#
# Open "dset" dataset under the root group.
#
dataset = file['/dset']
#
# Initialize buffers,read and print data.
#
# Python float type is 64-bit, one needs to use NATIVE_DOUBLE HDF5 type to read data. 
data_read64 = np.zeros((4,6,), dtype=float)
dataset.id.read(h5py.h5s.ALL, h5py.h5s.ALL, data_read64, mtype=h5py.h5t.NATIVE_DOUBLE)
print("Printing data 64-bit floating numbers...")
print(data_read64)

data_read32 = np.zeros((4,6,), dtype=np.float32)
dataset.id.read(h5py.h5s.ALL, h5py.h5s.ALL, data_read32, mtype=h5py.h5t.NATIVE_FLOAT)
print("Printing data 32-bit floating numbers...")
print(data_read32)
#
# Close the file before exiting
#
file.close()

