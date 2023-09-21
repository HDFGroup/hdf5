HDF5 "tall.h5" {
FILE_CONTENTS {
 group      /
 attribute  /attr2
 attribute  /attr1
 group      /g2
 unknown type of UD link /g2/udlink -> ???
 dataset    /g2/dset2.2
 dataset    /g2/dset2.1
 group      /g1
 group      /g1/g1.2
 group      /g1/g1.2/g1.2.1
 link       /g1/g1.2/g1.2.1/slink -> somevalue
 ext link   /g1/g1.2/extlink -> somefile somepath
 group      /g1/g1.1
 dataset    /g1/g1.1/dset1.1.2
 dataset    /g1/g1.1/dset1.1.1
 attribute  /g1/g1.1/dset1.1.1/attr2
 attribute  /g1/g1.1/dset1.1.1/attr1
 }
}
