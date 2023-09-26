HDF5 "tfcontents1.h5" {
FILE_CONTENTS {
 datatype   /#5616
 group      /
 unknown type of UD link /udlink -> ???
 link       /softlink -> /dset
 datatype   /mytype
 link       /mylink -> mylink
 group      /g2
 dataset    /g2/dset2
 group      /g1
 group      /g1/g1.1 -> /g2
 dataset    /g1/dset1 -> /g2/dset2
 ext link   /extlink -> fname oname
 dataset    /dsetmytype2
 dataset    /dset4 -> /g2/dset2
 dataset    /dset3 -> /g2/dset2
 dataset    /dset -> /g2/dset2
 }
}
