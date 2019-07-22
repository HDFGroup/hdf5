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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * TODO: error handling is a joke, but this seems normal for the tools'
 * gentest operations.
 */
static void
generate_int32le(hbool_t external)
{
    char    filename[MAX_NAME_SIZE];
    hid_t   file = H5I_INVALID_HID;
    hid_t   dset = H5I_INVALID_HID;
    hid_t   dcpl = H5P_DEFAULT;
    hid_t   dspace = H5I_INVALID_HID;
    hsize_t dims[] = {8, 8, 8};
    int32_t wdata[512]; /* 8^3, from dims */
    int32_t n = 0;
    int     i = 0;
    int     j = 0;
    int     k = 0;
    herr_t  ret = FAIL; /* error-checking */

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
        char name[MAX_NAME_SIZE];
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        HDassert(dcpl != H5I_INVALID_HID);
        snprintf(name, MAX_NAME_SIZE, "%s_ex-0.dat", FILE_INT32LE);
        ret = H5Pset_external(dcpl, name, 0, H5F_UNLIMITED);
        HDassert(ret >= 0);
    }

    dspace = H5Screate_simple(3, dims, NULL);
    HDassert(dspace != H5I_INVALID_HID);

    file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(file != H5I_INVALID_HID);

    dset = H5Dcreate2(
            file,
            "dset",
            H5T_STD_I32LE,
            dspace,
            H5P_DEFAULT,
            dcpl,
            H5P_DEFAULT);
    HDassert(dset != H5I_INVALID_HID);

    ret = H5Dwrite(
            dset,
            H5T_STD_I32LE,
            H5S_ALL,
            H5S_ALL,
            H5P_DEFAULT,
            wdata);
    HDassert(ret >= 0);

    if (dcpl != H5P_DEFAULT) {
        ret = H5Pclose(dcpl);
        HDassert(ret >= 0);
    }
    ret = H5Dclose(dset);
    HDassert(ret >= 0);
    ret = H5Fclose(file);
    HDassert(ret >= 0);
    ret = H5Sclose(dspace);
    HDassert(ret >= 0);

} /* end generate_int32le() */


static void
generate_uint8be(hbool_t external)
{
    char    filename[MAX_NAME_SIZE];
    hid_t   file = H5I_INVALID_HID;
    hid_t   dset = H5I_INVALID_HID;
    hid_t   dcpl = H5P_DEFAULT;
    hid_t   dspace = H5I_INVALID_HID;
    hsize_t dims[] = {8, 8, 8};
    uint8_t wdata[512]; /* 8^3, from dims */
    uint8_t n = 0;
    int     i = 0;
    int     j = 0;
    int     k = 0;
    herr_t  ret; /* error-checking */

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
        char name[MAX_NAME_SIZE];
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        HDassert(dcpl != H5I_INVALID_HID);
        for (i = 0; i < 8; i++) {
            snprintf(name, MAX_NAME_SIZE, "%s_ex-%d.dat", FILE_UINT8BE, i);
            ret = H5Pset_external(dcpl, name, 0, 64 * sizeof(char));
            HDassert(ret >= 0);
        }
    }

    dspace = H5Screate_simple(3, dims, NULL);
    HDassert(dspace != H5I_INVALID_HID);

    file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(file != H5I_INVALID_HID);

    dset = H5Dcreate2(
            file,
            "dset",
            H5T_STD_U8BE,
            dspace,
            H5P_DEFAULT,
            dcpl,
            H5P_DEFAULT);
    HDassert(dset != H5I_INVALID_HID);

    ret = H5Dwrite(
            dset,
            H5T_STD_U8BE,
            H5S_ALL,
            H5S_ALL,
            H5P_DEFAULT,
            wdata);
    HDassert(ret >= 0);

    if (dcpl != H5P_DEFAULT) {
        ret = H5Pclose(dcpl);
        HDassert(ret >= 0);
    }
    ret = H5Dclose(dset);
    HDassert(ret >= 0);
    ret = H5Fclose(file);
    HDassert(ret >= 0);
    ret = H5Sclose(dspace);
    HDassert(ret >= 0);

} /* end generate_uint8be() */


/* ----------------------------------------------------------------------------
 */
int
main(void)
{
    int i = 0;

    for (i=0; i < 2; i++) {
        hbool_t external = (i&1) ? TRUE : FALSE;
        generate_int32le(external);
        generate_uint8be(external);
    }
    return 0;
} /* end main() */


