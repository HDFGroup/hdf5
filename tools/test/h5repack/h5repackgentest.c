/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* For generating files for verifying h5repack with external storage. . .
 *
 * Each case file should follow the format of:
 * + h5repack_<NAME>.h5
 * + h5repack_<NAME>_ex.h5
 * + h5repack_<NAME>_ex-<N>.dat
 * ...where NAME idenfities the type, and N is a positive decimal number;
 * multiple external files (*.dat) are allowed per file, but they must
 * follow the pattern and be in contiguous numerical sequence starting at 0.
 *
 * Each file typename must be added to the listing for
 * `VERIFY_EXTERNAL_CONSOLIDATION` in h5repack.sh
 *
 * There is no restriction on the name, number, or structure of datasets and
 * groups in HDF5 file.
 */

#include "hdf5.h"
#include "H5private.h"

#define MAX_NAME_SIZE 256
#define FILE_INT32LE "h5repack_int32le"
#define FILE_UINT8BE "h5repack_uint8be"

#define H5REPACKGENTEST_OOPS ret_value = -1; goto done;

#define H5REPACKGENTEST_COMMON_CLEANUP(dcpl, file, space)  {                  \
    if ((dcpl) != H5P_DEFAULT && (dcpl) != H5I_INVALID_HID) {                 \
        (void)H5Pclose((dcpl));                                               \
    }                                                                         \
    if ((file) != H5I_INVALID_HID) { (void)H5Fclose((file)); }                \
    if ((space) != H5I_INVALID_HID) { (void)H5Sclose((space)); }              \
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Helper function to create and write a dataset to file.
 * Returns 0 on success, -1 on failure.
 */
static int
make_dataset(
        hid_t       file,
        const char *dset_name,
        hid_t       mem_type_id,
        hid_t       space,
        hid_t       dcpl,
        void       *wdata)
{
    hid_t dset      = H5I_INVALID_HID;
    int   ret_value = 0;

    dset = H5Dcreate2(
            file,
            "dset",
            mem_type_id,
            space,
            H5P_DEFAULT,
            dcpl,
            H5P_DEFAULT);
    if (dset == H5I_INVALID_HID) {
        H5REPACKGENTEST_OOPS;
    }

    if (H5Dwrite(
            dset,
            mem_type_id,
            H5S_ALL,
            H5S_ALL,
            H5P_DEFAULT,
            wdata)
        < 0)
    {
        H5REPACKGENTEST_OOPS;
    }

done:
    if (dset != H5I_INVALID_HID) { (void)H5Dclose(dset); }
    return ret_value;
} /* end make_dataset() */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Helper function to populate the DCPL external storage list.
 * Returns 0 on success, -1 on failure.
 */
static int
set_dcpl_external_list(
        hid_t       dcpl,
        const char *filename,
        hsize_t n_external_files,
        hsize_t n_elts,
        hsize_t elt_size)
{
    hsize_t i = 0;
    char    name[MAX_NAME_SIZE];

    for (i = 0; i < n_external_files; i++) {
        snprintf(name, MAX_NAME_SIZE, "%s_ex-%llu.dat", filename, i);
        if (H5Pset_external(dcpl, name, 0, n_elts * elt_size) < 0) {
            return -1;
        }
    }
    return 0;
} /* end set_dcpl_external_list() */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
static int
generate_int32le(hbool_t external)
{
    char    filename[MAX_NAME_SIZE];
    hid_t   file = H5I_INVALID_HID;
    hid_t   dcpl = H5P_DEFAULT;
    hid_t   space = H5I_INVALID_HID;
    hsize_t dims[] = {8, 8, 8};
    int32_t wdata[512]; /* 8^3, from dims */
    int32_t n = 0;
    int     i = 0;
    int     j = 0;
    int     k = 0;
    int     ret_value = 0;

    /* generate values, alternating positive and negative
     */
    for (i=0, n=0; i < 8; i++) {
        for (j=0; j < 8; j++) {
            for (k=0; k < 8; k++, n++) {
                wdata[n] = (k + j*512 + i*4096) * ((n&1) ? (-1) : (1));
            }
        }
    }

    snprintf(filename,
            MAX_NAME_SIZE,
            "%s%s.h5",
            FILE_INT32LE,
            (external) ? "_ex" : "");

    if (external) {
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        if (dcpl == H5I_INVALID_HID) {
            H5REPACKGENTEST_OOPS;
        }
        if (set_dcpl_external_list(
                dcpl,
                FILE_INT32LE,
                1,
                512,
                sizeof(int32_t))
            < 0)
        {
            H5REPACKGENTEST_OOPS;
        }
    }

    space = H5Screate_simple(3, dims, NULL);
    if (space == H5I_INVALID_HID) {
        H5REPACKGENTEST_OOPS;
    }

    file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file == H5I_INVALID_HID) {
        H5REPACKGENTEST_OOPS;
    }

    if (make_dataset(
            file,
            "dset",
            H5T_STD_I32LE,
            space,
            dcpl,
            wdata)
        < 0)
    {
        H5REPACKGENTEST_OOPS;
    }

done:
    H5REPACKGENTEST_COMMON_CLEANUP(dcpl, file, space);
    return ret_value;
} /* end generate_int32le() */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
static int
generate_uint8be(hbool_t external)
{
    char    filename[MAX_NAME_SIZE];
    hid_t   file = H5I_INVALID_HID;
    hid_t   dcpl = H5P_DEFAULT;
    hid_t   space = H5I_INVALID_HID;
    hsize_t dims[] = {8, 8, 8};
    uint8_t wdata[512]; /* 8^3, from dims */
    uint8_t n = 0;
    int     i = 0;
    int     j = 0;
    int     k = 0;
    int     ret_value = 0;

    /* Generate values, alternating positive and negative
     * The latter half of the dataset is "overflow garbage" (TODO?)
     */
    for (i=0, n=0; i < 8; i++) {
        for (j=0; j < 8; j++) {
            for (k=0; k < 8; k++, n++) {
                wdata[n] = n * ((n&1) ? (-1) : (1));
            }
        }
    }

    snprintf(filename,
            MAX_NAME_SIZE,
            "%s%s.h5",
            FILE_UINT8BE,
            (external) ? "_ex" : "");

    if (external) {
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        if (dcpl == H5I_INVALID_HID) {
            H5REPACKGENTEST_OOPS;
        }
        if (set_dcpl_external_list(
                dcpl,
                FILE_UINT8BE,
                8,
                64,
                sizeof(uint8_t))
            < 0)
        {
            H5REPACKGENTEST_OOPS;
        }
    }

    space = H5Screate_simple(3, dims, NULL);
    if (space == H5I_INVALID_HID) {
        H5REPACKGENTEST_OOPS;
    }

    file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file == H5I_INVALID_HID) {
        H5REPACKGENTEST_OOPS;
    }

    if (make_dataset(
            file,
            "dset",
            H5T_STD_U8BE,
            space,
            dcpl,
            wdata)
        < 0)
    {
        H5REPACKGENTEST_OOPS;
    }

done:
    H5REPACKGENTEST_COMMON_CLEANUP(dcpl, file, space);
    return ret_value;
} /* end generate_uint8be() */


/* ----------------------------------------------------------------------------
 */
int
main(void)
{
    int i         = 0; /* loop variable */
    int ret_value = 0;

    for (i=0; i < 2; i++) {
        hbool_t external = (i&1) ? TRUE : FALSE;
        if (ret_value == 0) { ret_value -= generate_int32le(external); }
        if (ret_value == 0) { ret_value -= generate_uint8be(external); }
    } /* end for external data storage or not */

    if (ret_value != 0) {
        /* TODO: print warning message? */
    }

    return ret_value;
} /* end main() */


