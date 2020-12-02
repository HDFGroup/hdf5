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
 * Programmer:  Jan-Willem Blokland
 *              December 1, 2020
 *
 * Purpose:     Test writing compounded attribute followed by
 *              writing data with a data transform function.
 */

#include "h5test.h"

#define FILENAME "cmpd_dtransform.h5"
#define LENGTH   11

typedef struct {
  char name[64];
  char unit[64];
} att_t;

int
main(void)
{
    hsize_t  dima[] = { 1 };
    hsize_t  dims[] = { LENGTH };
    hid_t    str_dtyp_id, att_dtyp_id, file_id, fspace_id, dset_id, att_dspc_id, att_attr_id, dxpl_id;

    /* Compound datatype */
    att_t *atts = HDmalloc(sizeof(att_t));
    HDstrcpy(atts[0].name, "Name");
    HDstrcpy(atts[0].unit, "Unit");

    /* String type */
    if ((str_dtyp_id = H5Tcopy(H5T_C_S1)) < 0)
      TEST_ERROR;
    H5Tset_size(str_dtyp_id, 64);

    /* Attribute type */
    if ((att_dtyp_id = H5Tcreate(H5T_COMPOUND, sizeof(att_t))) < 0)
      TEST_ERROR;
    H5Tinsert(att_dtyp_id, "NAME", HOFFSET(att_t, name), str_dtyp_id);
    H5Tinsert(att_dtyp_id, "UNIT", HOFFSET(att_t, unit), str_dtyp_id);

    /* Create file. */
    if ((file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
      TEST_ERROR;

    /* Create file dataspace. */
    if ((fspace_id = H5Screate_simple(1, dims, NULL)) < 0)
      TEST_ERROR;

    /* Create dataset. */
    if ((dset_id = H5Dcreate2(file_id, "test_dset", H5T_NATIVE_INT, fspace_id, 
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
      TEST_ERROR;

    /* Write the attribute (compound) to the dataset */
    if ((att_dspc_id = H5Screate_simple(1, dima, NULL)) < 0)
      TEST_ERROR;
    if ((att_attr_id = H5Acreate2(dset_id, "ATTRIBUTES", att_dtyp_id, att_dspc_id, 
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0)
      TEST_ERROR;
    if (H5Awrite(att_attr_id, att_dtyp_id, atts) < 0)
      TEST_ERROR;

    /* Create dataset transfer property list */
    const char *expr = "2*x";
    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
      TEST_ERROR;
    if (H5Pset_data_transform(dxpl_id, expr) < 0) {
      HDprintf("**** ERROR: H5Pset_data_transform (expression: %s) ****\n", expr);
      TEST_ERROR;
    }

    int *data     = HDmalloc(LENGTH * sizeof(int));
    int *data_res = HDmalloc(LENGTH * sizeof(int));
    for (unsigned i = 0; i < LENGTH; i++) {
      data[i]     = 10;
      data_res[i] = 2 * data[i];
    }

    /* Write the data */
    if (H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id, data) < 0)
      TEST_ERROR;
    
    /* Read attribute */
    att_t *atts_res  =HDmalloc(sizeof(att_t));
    if (H5Aread(att_attr_id, att_dtyp_id, atts_res) < 0)
      TEST_ERROR;
    
    /* Verify attribute */
    if (HDstrcmp(atts_res[0].name, atts[0].name) != 0)
      TEST_ERROR;
    if (HDstrcmp(atts_res[0].unit, atts[0].unit) != 0)
      TEST_ERROR;

    /* Read the data */
    if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
      TEST_ERROR;

    /* Verify data */
    for (unsigned idx = 0; idx < LENGTH; idx++) {
      if (data[idx] != data_res[idx])
        TEST_ERROR;
    }

    HDfree(atts);
    HDfree(atts_res);
    HDfree(data);
    HDfree(data_res);

    /* Close all identifiers. */
    H5Pclose(dxpl_id);
    H5Aclose(att_attr_id);
    H5Sclose(att_dspc_id);
    H5Dclose(dset_id);
    H5Sclose(fspace_id);
    H5Fclose(file_id);
    H5Tclose(att_dtyp_id);
    H5Tclose(str_dtyp_id);

    return 0;

 error:
    return 1;
}
