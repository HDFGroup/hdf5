/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              Feb 27, 2002
 *
 * Purpose:     This program is run to generate a HDF5 data file with fill
 *              value property.  A new fill value design has been put into
 *              library v1.5.  To test compatibility between v1.4 and v1.5,
 *              compile and run this program, it will generate a file called
 *              fill_new.h5.  You need to move it to the /test directory
 *              in HDF5 v1.4 source codes.  The fillval.c program will read it.
 *
 */

#include "h5test.h"

#define FILENAME "fill_new.h5"

int
main(void)
{
    hid_t   file=-1, dcpl=-1, space=-1, dset1=-1, dset2=-1;
    hsize_t cur_size[2]={8, 8};
    H5D_space_status_t  allocation;
    int     fill_val1 = 4444, fill_val2=5555;

    if((file=H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) <0) goto error;
    if((space=H5Screate_simple(2, cur_size, cur_size)) < 0) goto error;
    if((dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Create a dataset with space being allocated and fill value written */
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) goto error;
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill_val1) < 0) goto error;
    if((dset1 = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
          goto error;
    if(H5Dget_space_status(dset1, &allocation) < 0) goto error;
    if(allocation == H5D_SPACE_STATUS_NOT_ALLOCATED) {
          puts("    Got unallocated space instead of allocated.");
          printf("    Got %d\n", allocation);
          goto error;
    }
    if(H5Dclose(dset1) < 0) goto error;

    /* Create a dataset with space allocation being delayed */
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE) < 0) goto error;
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) goto error;
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill_val2) < 0) goto error;
    if((dset2 = H5Dcreate2(file, "dset2", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
          goto error;
    if(H5Dget_space_status(dset2, &allocation) < 0) goto error;
    if(allocation != H5D_SPACE_STATUS_NOT_ALLOCATED) {
          puts("    Got allocated space instead of unallocated.");
          printf("    Got %d\n", allocation);
          goto error;
    }
    if(H5Dclose(dset2) < 0) goto error;

    if(H5Sclose(space) < 0) goto error;
    if(H5Pclose(dcpl) < 0) goto error;
    if(H5Fclose(file) < 0) goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(space);
        H5Dclose(dset1);
        H5Dclose(dset2);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}

