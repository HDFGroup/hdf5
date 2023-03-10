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

#include "hdf5.h"
#include "hdf5_hl.h"
#include <stdlib.h>

#define ATTR_SIZE 5

int
main(void)
{
    hid_t   file_id;
    hid_t   dset_id;
    hid_t   space_id;
    hsize_t dims[1]         = {ATTR_SIZE};
    int     data[ATTR_SIZE] = {1, 2, 3, 4, 5};
    int     i;

    /* create a file */
    file_id = H5Fcreate("ex_lite3.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* create a data space  */
    space_id = H5Screate_simple(1, dims, NULL);

    /* create a dataset named "dset" */
    dset_id = H5Dcreate2(file_id, "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* close */
    H5Dclose(dset_id);
    H5Sclose(space_id);

    /*-------------------------------------------------------------------------
     * example of H5LTset_attribute_int
     *-------------------------------------------------------------------------
     */

    /* create and write the attribute "attr1" on the dataset "dset" */
    H5LTset_attribute_int(file_id, "dset", "attr1", data, ATTR_SIZE);

    /*-------------------------------------------------------------------------
     * example of H5LTget_attribute_int
     *-------------------------------------------------------------------------
     */

    /* get the attribute "attr1" from the dataset "dset" */
    H5LTget_attribute_int(file_id, "dset", "attr1", data);

    for (i = 0; i < ATTR_SIZE; i++)
        printf("  %d", data[i]);
    printf("\n");

    /* close file */
    H5Fclose(file_id);

    return 0;
}
