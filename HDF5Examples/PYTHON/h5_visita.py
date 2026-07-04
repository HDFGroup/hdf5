import h5py
def print_info(name, obj):
    print(name) 
    for name, value in obj.attrs.items():
        print(name+":", value)
	
f = h5py.File('GATMO-SATMS-npp.h5', 'r+')
f.visititems(print_info)
f.close()
