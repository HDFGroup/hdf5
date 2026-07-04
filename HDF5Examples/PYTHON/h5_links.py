#
# This example demonstrates the concepts of hard, soft and external links.
#
# We will create file links.h5 with the following members and then will try to access objects
# using hard, soft and external links.
# /                        Group
# /A                       Group
# /A/a                     Dataset {10}
# /B                       Group
# /B/External              External Link {dset.h5//dset}
# /a                       Dataset, same as /A/a
# /dangling                Soft Link {/B/XXX}
# /soft                    Soft Link {/A/a}

import h5py
import numpy as np
file = h5py.File('links.h5', 'w')
#
# Create a group structure in the file
#
A = file.create_group("A")
B = file.create_group("B")
a = A.create_dataset("a", (10,), 'i')
#
# Create a hard link in a root group pointing to dataset  /A/a
#
file["a"] = a
#
# Create a soft link (alias) in a root group with a value  /A/a
#
file["soft"] = h5py.SoftLink('/A/a')
#
# Create a soft link (alias) in a root group with a value /B/XXX that cannot be resolved
#
file["dangling"] = h5py.SoftLink('/B/XXX')
#
# Create an external link to a dataset "dset" in file dset.h5
#
B['External'] = h5py.ExternalLink("dset.h5", "/dset")
#
# List objects in the root group in the file
#
print("Root group members in links.h5:")
try:
        print("Trying to get the items...")
        print(list(file.items()))
except:
        print("...but can only get the keys...")
        print(list(file.keys()))
        print("          ")
        print("Why? Because the library cannot resolve the dangling link.")
        print("We will delete the 'dangling' link and try again.")
        del file["dangling"]
print(list(file.items()))
print("          ")
print("Group A members:")
print(list(A.items()))
print("          ")
print("Group B members:")
print(list(B.items()))
print("          ")
print("Reading dataset pointed by the external link...")
dset = B['External']
data = np.zeros((4,6))
data = dset[...]
print(data)
#
# Copy link to /A/a to /B/b
#
B["b"]=A["a"]
file.close()
