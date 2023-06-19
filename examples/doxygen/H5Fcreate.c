#include "hdf5.h"

int
main(void)
{
    hid_t file;
    if ((file = H5Fcreate("foo.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return -1;
    /* Do something good with this file. */
    if (H5Fclose(file) < 0)
        return -2;
    return 0;
}
