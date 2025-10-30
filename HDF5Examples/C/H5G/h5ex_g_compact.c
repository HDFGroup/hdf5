/************************************************************

  This example shows how to create "compact-or-indexed"
  format groups, new to 1.8.  This example also illustrates
  the space savings of compact groups by creating 2 files
  which are identical except for the group format, and
  displaying the file size of each.  Both files have one
  empty group in the root group.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>

#define FILENAME1 "h5ex_g_compact1.h5"
#define FILENAME2 "h5ex_g_compact2.h5"
#define GROUP     "G1"

int
main(void)
{
    hid_t      file  = H5I_INVALID_HID;
    hid_t      group = H5I_INVALID_HID;
    hid_t      fapl  = H5I_INVALID_HID;
    herr_t     status;
    H5G_info_t ginfo;
    hsize_t    size;

    /*
     * Set file access property list to use the earliest file format.
     * This will force the library to create original format groups.
     */
    fapl   = H5Pcreate(H5P_FILE_ACCESS);
    status = H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST);

    /*
     * Create file 1.  This file will use original format groups.
     */
    file  = H5Fcreate(FILENAME1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    group = H5Gcreate(file, GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Obtain the group info and print the group storage type.
     */
    status = H5Gget_info(group, &ginfo);
    printf("Group storage type for %s is: ", FILENAME1);
    switch (ginfo.storage_type) {
        case H5G_STORAGE_TYPE_COMPACT:
            printf("H5G_STORAGE_TYPE_COMPACT\n"); /* New compact format */
            break;
        case H5G_STORAGE_TYPE_DENSE:
            printf("H5G_STORAGE_TYPE_DENSE\n"); /* New dense (indexed) format */
            break;
        case H5G_STORAGE_TYPE_SYMBOL_TABLE:
            printf("H5G_STORAGE_TYPE_SYMBOL_TABLE\n"); /* Original format */
            break;
        case H5G_STORAGE_TYPE_UNKNOWN:
            printf("H5G_STORAGE_TYPE_UNKNOWN\n"); /* Unknown format */
    }

    /*
     * Close and re-open file.  Needed to get the correct file size.
     */
    status = H5Gclose(group);
    status = H5Fclose(file);
    file   = H5Fopen(FILENAME1, H5F_ACC_RDONLY, H5P_DEFAULT);

    /*
     * Obtain and print the file size.
     */
    status = H5Fget_filesize(file, &size);
    printf("File size for %s is: %d bytes\n\n", FILENAME1, (int)size);

    /*
     * Close FILE1.
     */
    status = H5Fclose(file);

    /*
     * Now use the default file access property list to allow the latest file
     * format. This will allow the library to create new compact format groups.
     * Since HDF5 2.0, the default is to use the 1.8 file format as the low
     * bound, which includes compact groups.
     */

    /*
     * Create file 2 using the default access property list.
     */
    file  = H5Fcreate(FILENAME2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    group = H5Gcreate(file, GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Obtain the group info and print the group storage type.
     */
    status = H5Gget_info(group, &ginfo);
    printf("Group storage type for %s is: ", FILENAME2);
    switch (ginfo.storage_type) {
        case H5G_STORAGE_TYPE_COMPACT:
            printf("H5G_STORAGE_TYPE_COMPACT\n"); /* New compact format */
            break;
        case H5G_STORAGE_TYPE_DENSE:
            printf("H5G_STORAGE_TYPE_DENSE\n"); /* New dense (indexed) format */
            break;
        case H5G_STORAGE_TYPE_SYMBOL_TABLE:
            printf("H5G_STORAGE_TYPE_SYMBOL_TABLE\n"); /* Original format */
            break;
        case H5G_STORAGE_TYPE_UNKNOWN:
            printf("H5G_STORAGE_TYPE_UNKNOWN\n"); /* Unknown format */
    }

    /*
     * Close and re-open file.  Needed to get the correct file size.
     */
    status = H5Gclose(group);
    status = H5Fclose(file);
    file   = H5Fopen(FILENAME2, H5F_ACC_RDONLY, fapl);

    /*
     * Obtain and print the file size.
     */
    status = H5Fget_filesize(file, &size);
    printf("File size for %s is: %d bytes\n", FILENAME2, (int)size);
    printf("\n");

    /*
     * Close and release resources.
     */
    status = H5Pclose(fapl);
    status = H5Fclose(file);

    return 0;
}
