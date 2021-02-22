/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * The example below illustrates the use of the new API with a file that was
 * written using the old-style reference API, showing how one can take
 * advantage of the automatic type conversion from old reference type to new
 * reference type.
 */

#include <stdlib.h>

#include "hdf5.h"
#include <assert.h>

#define H5FILE_NAME "refer_deprec.h5"

#define NDIMS    1 /* Number of dimensions */
#define BUF_SIZE 4 /* Size of example buffer */
#define NREFS    1 /* Number of references */

int
main(void)
{
    hid_t   file1, dset1, space1;
    hsize_t dset1_dims[NDIMS] = {BUF_SIZE};
    int     dset_buf[BUF_SIZE];

    hid_t      dset2, space2;
    hsize_t    dset2_dims[NDIMS]  = {NREFS};
    hobj_ref_t ref_buf[NREFS]     = {0};
    H5R_ref_t  new_ref_buf[NREFS] = {0};
    H5O_type_t obj_type;
    int        i;

    for (i = 0; i < BUF_SIZE; i++)
        dset_buf[i] = i;

    /* Create file with one dataset and close it */
    file1 = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    space1 = H5Screate_simple(NDIMS, dset1_dims, NULL);
    dset1  = H5Dcreate2(file1, "dataset1", H5T_NATIVE_INT, space1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_buf);
    H5Dclose(dset1);
    H5Sclose(space1);

    /**
     * Create reference to dataset1 with deprecated API
     * (reminder: there is no destroy call for those references)
     */
    H5Rcreate(&ref_buf[0], file1, "dataset1", H5R_OBJECT, H5I_INVALID_HID);

    /* Store reference in separate dataset using deprecated reference type */
    space2 = H5Screate_simple(NDIMS, dset2_dims, NULL);
    dset2  = H5Dcreate2(file1, "references", H5T_STD_REF_OBJ, space2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dset2, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_buf);
    H5Dclose(dset2);
    H5Sclose(space2);
    H5Fclose(file1);

    /* Read reference from file using new reference type */
    file1 = H5Fopen(H5FILE_NAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset2 = H5Dopen2(file1, "references", H5P_DEFAULT);
    H5Dread(dset2, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, new_ref_buf);
    H5Dclose(dset2);

    /* Access reference and read dataset data through new API */
    assert(H5Rget_type((const H5R_ref_t *)&new_ref_buf[0]) == H5R_OBJECT2);
    H5Rget_obj_type3(&new_ref_buf[0], H5P_DEFAULT, &obj_type);
    assert(obj_type == H5O_TYPE_DATASET);
    dset1 = H5Ropen_object(&new_ref_buf[0], H5P_DEFAULT, H5P_DEFAULT);
    H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_buf);
    H5Dclose(dset1);
    H5Rdestroy(&new_ref_buf[0]);

    for (i = 0; i < BUF_SIZE; i++)
        assert(dset_buf[i] == i);
    return 0;
}
