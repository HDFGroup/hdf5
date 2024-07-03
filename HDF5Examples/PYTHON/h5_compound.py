#
# This example creates an HDF5 file compound.h5 and an empty datasets /DSC in it.
#
import h5py
import numpy as np
#
# Create a new file using default properties.
#
file = h5py.File('compound.h5','w')
#
# Create a dataset under the Root group.
#
comp_type = np.dtype([('Orbit', 'i'), ('Location', np.str_, 6), ('Temperature (F)', 'f8'), ('Pressure (inHg)', 'f8')])
dataset = file.create_dataset("DSC",(4,), comp_type)
data = np.array([(1153, "Sun   ", 53.23, 24.57), (1184, "Moon  ", 55.12, 22.95), (1027, "Venus ", 103.55, 31.23), (1313, "Mars  ", 1252.89, 84.11)], dtype = comp_type)
dataset[...] = data
#
# Close the file before exiting
#
file.close()
file = h5py.File('compound.h5', 'r')
dataset = file["DSC"]
print("Reading Orbit and Location fields...")
orbit = dataset['Orbit']
print("Orbit: ", orbit)
location = dataset['Location']
print("Location: ", location)
data = dataset[...]
print("Reading all records:")
print(data)
print("Second element of the third record:", dataset[2, 'Location'])
file.close()

