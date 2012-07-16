HDF5 "tall.h5" {
FILE_CONTENTS {
 group      /
 attribute  /attr1
 attribute  /attr2
 group      /g1
 group      /g1/g1.1
 dataset    /g1/g1.1/dset1.1.1
 attribute  /g1/g1.1/dset1.1.1/attr1
 attribute  /g1/g1.1/dset1.1.1/attr2
 dataset    /g1/g1.1/dset1.1.2
 group      /g1/g1.2
 ext link   /g1/g1.2/extlink -> somefile somepath
 group      /g1/g1.2/g1.2.1
 link       /g1/g1.2/g1.2.1/slink -> somevalue
 group      /g2
 dataset    /g2/dset2.1
 dataset    /g2/dset2.2
 unknown type of UD link /g2/udlink -> ???
 }
}
