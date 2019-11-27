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
 *
 * The inluded datatypes should be more than adequate to verify the correctness
 * of the behavior -- if one type can be consolidated from external storage,
 * then thay all can.
 */

#include "hdf5.h"
#include "H5private.h"

#define MAX_NAME_SIZE 256
#define FILE_INT32LE_1 "h5repack_int32le_1d"
#define FILE_INT32LE_2 "h5repack_int32le_2d"
#define FILE_INT32LE_3 "h5repack_int32le_3d"
#define FILE_UINT8BE "h5repack_uint8be"
#define FILE_F32LE   "h5repack_f32le"

#define H5REPACKGENTEST_OOPS { \
    ret_value = -1;            \
    goto done;                 \
}

#define H5REPACKGENTEST_COMMON_CLEANUP(dcpl, file, space)  {                  \
    if ((dcpl) != H5P_DEFAULT && (dcpl) != H5I_INVALID_HID) {                 \
        (void)H5Pclose((dcpl));                                               \
    }                                                                         \
    if ((file) != H5I_INVALID_HID) { (void)H5Fclose((file)); }                \
    if ((space) != H5I_INVALID_HID) { (void)H5Sclose((space)); }              \
}

struct external_def {
    hsize_t type_size;
    unsigned n_elts_per_file;
    unsigned n_elts_total;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Helper function to create and write a dataset to file.
 * Returns 0 on success, -1 on failure.
 */
static int
__make_dataset(hid_t file_id, const char *dset_name,
        hid_t mem_type_id, hid_t space_id, hid_t dcpl_id, void *wdata) {
    hid_t dset_id = H5I_INVALID_HID;
    int   ret_value = 0;

    dset_id = H5Dcreate2(file_id, dset_name, mem_type_id, space_id,
    H5P_DEFAULT, dcpl_id,
    H5P_DEFAULT);
    if (dset_id == H5I_INVALID_HID)
        H5REPACKGENTEST_OOPS;

    if (H5Dwrite(dset_id, mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        H5REPACKGENTEST_OOPS;

done:
    if (dset_id != H5I_INVALID_HID)
        (void) H5Dclose(dset_id);

    return ret_value;
} /* end __make_dataset() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Helper function to populate the DCPL external storage list.
 * Creates external files for the DCPL, with each file name following the
 * convention "<filename>_ex-<num>.dat". Will append `n_external_files` to
 * the filename list, with each file having space for `n_elts` items of the
 * type (of size `elt_size`). The numeric inputs are not sanity-checked.
 * Returns 0 on success, -1 on failure.
 */
static int
__set_dcpl_external_list(hid_t dcpl, const char *filename,
        unsigned n_elts_per_file, unsigned n_elts_total, hsize_t elt_size) {
    char     name[MAX_NAME_SIZE];
    unsigned n_external_files = 0;
    unsigned i = 0;

    if (NULL == filename || '\0' == *filename)
        return -1;

    n_external_files = n_elts_total / n_elts_per_file;
    if (n_elts_total != (n_external_files * n_elts_per_file))
        return -1;


    for (i = 0; i < n_external_files; i++) {
        if (HDsnprintf(name, MAX_NAME_SIZE, "%s_ex-%u.dat", filename, i) >= MAX_NAME_SIZE)
            return -1;

        if (H5Pset_external(dcpl, name, 0, n_elts_per_file * elt_size) < 0)
            return -1;
    }
    return 0;
} /* end __set_dcpl_external_list() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Generalized utility function to write a file with the specified data and
 * dataset configuration. If `ext` is provided, will attempt to use external
 * storage.
 * Returns 0 on success, -1 on failure.
 */
static int
__make_file(const char *basename, struct external_def *ext,
        hid_t type_id, hsize_t rank, hsize_t *dims, void *wdata) {
    char  filename[MAX_NAME_SIZE];
    hid_t file_id = H5I_INVALID_HID;
    hid_t dcpl_id = H5P_DEFAULT;
    hid_t space_id = H5I_INVALID_HID;
    int   ret_value = 0;

    if (HDsnprintf(filename, MAX_NAME_SIZE, "%s%s.h5", basename, (NULL != ext) ? "_ex" : "") >= MAX_NAME_SIZE)
        H5REPACKGENTEST_OOPS;

    if (NULL != ext) {
        dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        if (dcpl_id == H5I_INVALID_HID)
            H5REPACKGENTEST_OOPS;

        if (__set_dcpl_external_list(dcpl_id, basename, ext->n_elts_per_file, ext->n_elts_total, ext->type_size) < 0)
            H5REPACKGENTEST_OOPS;
    }

    space_id = H5Screate_simple(rank, dims, NULL);
    if (space_id == H5I_INVALID_HID)
        H5REPACKGENTEST_OOPS;

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file_id == H5I_INVALID_HID)
        H5REPACKGENTEST_OOPS;


    if (__make_dataset(file_id, "dset", type_id, space_id, dcpl_id, wdata) < 0)
        H5REPACKGENTEST_OOPS;

done:
    H5REPACKGENTEST_COMMON_CLEANUP(dcpl_id, file_id, space_id);
return ret_value;
} /* end __make_file() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Returns 0 on success, -1 on failure.
 */
static int
generate_int32le_1d(hbool_t external) {
    int32_t wdata[12];
    hsize_t dims[] = { 12 };
    struct external_def *def_ptr = NULL;
    struct external_def  def = { (hsize_t) sizeof(int32_t), 6, 12 };
    int32_t n = 0;
    int     ret_value = 0;

    /* Generate values
    */
    for (n = 0; n < 12; n++) {
        wdata[n] = n - 6;
    }

    def_ptr = (TRUE == external) ? (&def) : NULL;
    if (__make_file(FILE_INT32LE_1, def_ptr, H5T_STD_I32LE, 1, dims, wdata) < 0)
        ret_value = -1;

    return ret_value;
} /* end generate_int32le_1d() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Returns 0 on success, -1 on failure.
 */
static int
generate_int32le_2d(hbool_t external) {
    int32_t wdata[64];
    hsize_t dims[] = { 8, 8 };
    struct external_def *def_ptr = NULL;
    struct external_def  def = { (hsize_t) sizeof(int32_t), 64, 64 };
    int32_t n = 0;
    int     ret_value = 0;

    /* Generate values
    */
    for (n = 0; n < 64; n++) {
        wdata[n] = n - 32;
    }

    def_ptr = (TRUE == external) ? (&def) : NULL;
    if (__make_file(FILE_INT32LE_2, def_ptr, H5T_STD_I32LE, 2, dims, wdata) < 0)
        ret_value = -1;

    return ret_value;
} /* end generate_int32le_2d() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Returns 0 on success, -1 on failure.
 */
static int
generate_int32le_3d(hbool_t external) {
    hsize_t dims[] = { 8, 8, 8 };
    int32_t wdata[512]; /* 8^3, from dims */
    struct external_def *def_ptr = NULL;
    struct external_def  def = { (hsize_t) sizeof(int32_t), 512, 512 };
    int32_t n = 0;
    int     i = 0;
    int     j = 0;
    int     k = 0;
    int     ret_value = 0;

    /* generate values, alternating positive and negative
    */
    for (i = 0, n = 0; i < dims[0]; i++) {
        for (j = 0; j < dims[1]; j++) {
            for (k = 0; k < dims[2]; k++, n++) {
                wdata[n] = (k + j * 512 + i * 4096) * ((n & 1) ? (-1) : (1));
            }
        }
    }

    def_ptr = (TRUE == external) ? (&def) : NULL;
    if (__make_file(FILE_INT32LE_3, def_ptr, H5T_STD_I32LE, 3, dims, wdata) < 0)
        ret_value = -1;

    return ret_value;
} /* end generate_int32le_3d() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Returns 0 on success, -1 on failure.
 */
static int
generate_uint8be(hbool_t external) {
    hsize_t dims[] = { 4, 8, 8 };
    uint8_t wdata[256]; /* 4*8*8, from dims */
    struct external_def *def_ptr = NULL;
    struct external_def def = { (hsize_t) sizeof(uint8_t), 64, 256 };
    uint8_t n = 0;
    int     i = 0;
    int     j = 0;
    int     k = 0;
    int     ret_value = 0;

    /* Generate values, ping-pong from ends of range
    */
    for (i = 0, n = 0; i < dims[0]; i++) {
        for (j = 0; j < dims[1]; j++) {
            for (k = 0; k < dims[2]; k++, n++) {
                wdata[n] = n * ((n & 1) ? (-1) : (1));
            }
        }
    }

    def_ptr = (TRUE == external) ? (&def) : NULL;
    if (__make_file(FILE_UINT8BE, def_ptr, H5T_STD_U8BE, 3, dims, wdata) < 0)
        ret_value = -1;

    return ret_value;
} /* end generate_uint8be() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Returns 0 on success, -1 on failure.
 */
static int
generate_f32le(hbool_t external) {
    hsize_t dims[] = { 12, 6 };
    float wdata[72]; /* 12*6, from dims */
    struct external_def *def_ptr = NULL;
    struct external_def  def = { (hsize_t) sizeof(float), 72, 72 };
    float n = 0;
    int   i = 0;
    int   j = 0;
    int   k = 0;
    int   ret_value = 0;

    /* Generate values */
    for (i = 0, k = 0, n = 0; i < dims[0]; i++) {
        for (j = 0; j < dims[1]; j++, k++, n++) {
            wdata[k] = n * 801.1 * ((k % 5 == 1) ? (-1) : (1));
        }
    }

    def_ptr = (TRUE == external) ? (&def) : NULL;
    if (__make_file(FILE_F32LE, def_ptr, H5T_IEEE_F32LE, 2, dims, wdata) < 0)
        ret_value = -1;

    return ret_value;
} /* end generate_f32le() */

/* ----------------------------------------------------------------------------
 * Create files.
 * Return 0 on success, nonzero on failure.
 */
int
main(void) {
    int i = 0;
    int ret_value = 0;

    for (i = 0; i < 2; i++) {
        hbool_t external = (i & 1) ? TRUE : FALSE;
        if (generate_int32le_1d(external) < 0)
            HDprintf("A generate_int32le_1d failed!\n");

        if (generate_int32le_2d(external) < 0)
            HDprintf("A generate_int32le_2d failed!\n");

        if (generate_int32le_3d(external) < 0)
            HDprintf("A generate_int32le_3d failed!\n");

        if (generate_uint8be(external) < 0)
            HDprintf("A generate_uint8be failed!\n");

        if (generate_f32le(external) < 0)
            HDprintf("A generate_f32le failed!\n");

    } /* end for external data storage or not */

    return EXIT_SUCCESS;
} /* end main() */

