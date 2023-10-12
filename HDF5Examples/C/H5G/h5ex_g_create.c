/************************************************************

  This example shows how to create, open, and close a group.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"

#define FILE "h5ex_g_create.h5"

int
main(void)
{
    hid_t  file  = H5I_INVALID_HID;
    hid_t  group = H5I_INVALID_HID;
    herr_t status;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a group named "G1" in the file.
     */
    group = H5Gcreate(file, "/G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Close the group.  The handle "group" can no longer be used.
     */
    status = H5Gclose(group);

    /*
     * Re-open the group, obtaining a new handle.
     */
    group = H5Gopen(file, "/G1", H5P_DEFAULT);

    /*
     * Close and release resources.
     */
    status = H5Gclose(group);
    status = H5Fclose(file);
}
