#
# This example creates and writes GZIP compressed dataset.
#
import h5py
import numpy as np
#
# Create  gzip.h5 file.
#
file = h5py.File('gzip.h5','w')
#
# Create /DS1 dataset; in order to use compression, dataset has to be chunked.
#
dataset = file.create_dataset('DS1',(32,64),'i',chunks=(4,8),compression='gzip',compression_opts=9) 
#
# Initialize data.
#
data = np.zeros((32,64))
for i in range(32):
    for j in range(64):
        data[i][j]= i*j-j	 
#
# Write data.
#
print("Writing data...")
dataset[...] = data
file.close()
#
# Read data back; display compression properties and dataset max value. 
#
file = h5py.File('gzip.h5','r')
dataset = file['DS1']
print("Compression method is", dataset.compression)
print("Compression parameter is", dataset.compression_opts)
data = dataset[...]
print("Maximum value in", dataset.name, "is:", max(data.ravel()))
file.close()

