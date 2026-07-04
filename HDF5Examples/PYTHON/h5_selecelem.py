#
# This example demonstrates how to do point selection in Python.
#
import h5py
import numpy as np
file1 = h5py.File('copy1.h5','w')
file2 = h5py.File('copy2.h5','w')
dataset1 = file1.create_dataset('Copy1', (3,4), 'i')
dataset2 = file2.create_dataset('Copy2', (3,4), 'i')
#
# Initialize data object with 0.
#
data1 = np.zeros((3,4))
data2 = np.ones((3,4))
val = (55,59)
#
# Write data
#
dataset1[...] = data1
dataset2[...] = data2
#
# Modify two elements with the new values. We can choose any number of elements along one dimension.
#
dataset1[0, [1,3]] = val
dataset2[0, [1,3]] = val
file1.close()
file2.close()
#
# Reopen the files  and read data back
#
file1 = h5py.File('copy1.h5', 'r')
dataset1 = file1['Copy1']
data1 = dataset1[...]
print "Dataset Copy1 in copy1.h5:"
print data1

file2 = h5py.File('copy2.h5', 'r')
dataset2 = file2['Copy2']
data2 = dataset2[...]
print "Dataset Copy2 in copy2.h5:"
print data2

file1.close()
file2.close()
