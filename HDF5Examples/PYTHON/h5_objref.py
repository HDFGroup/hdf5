#
# This example shows how to create dataset with object references
#
import h5py
import numpy as np
#
# Create a new file using default properties.
#
file = h5py.File('objref.h5','w')
#
# Create a group and scalar datasets under the Root group.
#
group = file.create_group("G1")
dataset = file.create_dataset("DS2",(), 'i')
#
# Create references to the group and the dataset and store them in another dataset.
#
refs = (group.ref, dataset.ref)
ref_type = h5py.h5t.special_dtype(ref=h5py.Reference)
dataset_ref = file.create_dataset("DS1", (2,),ref_type)
dataset_ref[...] = refs

#
# Close the file before exiting
#
file.close()

file = h5py.File('objref.h5','r')
dataset_ref = file["DS1"]
refs = dataset_ref[...]
refs_list = list(refs)
for obj in refs_list:
    index = refs_list.index(obj)
    print("DS["+str(index)+"]:")
    print(file[obj])
file.close()

