#include "hdf5.h"

int
main(void)
{
    herr_t   retval;
    unsigned majnum, minnum, relnum;

    if ((retval = H5get_libversion(&majnum, &minnum, &relnum)) >= 0) {
        printf("Hello, HDF5 %d.%d.%d!\n", majnum, minnum, relnum);
    }
    return retval;
}
