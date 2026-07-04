#
# This example shows how to create a dataset with region references.
#
import h5py
import numpy as np
#
# Create a new file using default properties.
#
file = h5py.File('regref.h5','w')
#
# Create a group and (3x2) dataset under the Root group.
#
dataset = file.create_dataset("DS2",(3,2), h5py.h5t.STD_I8LE)
dataset[...] = np.array([[1,1], [2,2], [3,3]])
#
# Create references to each row in the dataset.
#
refs = (dataset.regionref[0,:],dataset.regionref[1,:],dataset.regionref[2,:])
#
# Create a dataset to store region references.
#
ref_type = h5py.h5t.special_dtype(ref=h5py.RegionReference)
dataset_ref = file.create_dataset("DS1", (3,),ref_type)
dataset_ref[...] = refs
#
# Close the file before exiting.
#
file.close()
#
# Open the file, read the second element of the dataset with the region references
# and dereference it to get data.
#
file = h5py.File('regref.h5', 'r')
dataset = file["DS1"]
regref = dataset[1]
#
# Region reference can be used to find a dataset it points to.
#
dataset_name = file[regref].name
print(dataset_name)
#
# Get hyperslab the reference points to.
#
data = file[dataset_name]
#
# Region reference can be used as a slicing argument!
print(data[regref])
file.close()




