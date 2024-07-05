#
# This example creates an HDF5 file vlstring.h5 and DSvariable dataset in it.
# Then it opens the file and reads data back.
#
import h5py
#
# Create a new file using default properties.
#
file = h5py.File('vlstring.h5','w')
#
# Create a dataset under the Root group using variable-length string type.
#
str_type = h5py.special_dtype(vlen=str)
dataset = file.create_dataset("DSvariable",(4,), dtype=str_type)
data = ("Parting", " is such", " sweet", " sorrow...")
dataset[...] = data
#
# Close the file before exiting
#
file.close()
file = h5py.File('vlstring.h5', 'r')
dataset = file['DSvariable']
data_out = dataset[...]
for i in range(4):	
    print("DSvariable[",i,"]", "'"+data_out[i].decode('utf-8')+"'", "has length", len(data_out[i]))

print(data_out)
file.close()
