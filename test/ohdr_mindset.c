/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Tests to verify behavior of minimized dataset object headers.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "h5test.h"

/******************
 * TESTING MACROS *
 ******************/

#define DEBUG_OH_SIZE 0 /* toggle some debug printing (0 off, 1 on)*/

/* basenames of test files created in this test suite */
#define OHMIN_FILENAME_A "ohdr_min_a"
#define OHMIN_FILENAME_B "ohdr_min_b"

/* used for object header size comparison */
#define EQ 1
#define LT 2
#define GT 3


/* ---------------------------------------------------------------------------
 * Macro: PRINT_DSET_OH_COMPARISON(...)
 *
 * Pretty-print metadata information about two dataset object headers.
 * Please use only at "top level" of test function.
 * ---------------------------------------------------------------------------
 */
#define PRINT_DSET_OH_COMPARISON(did1, did2)                      \
{   H5O_info_t info1;                                             \
    H5O_info_t info2;                                             \
                                                                  \
    if(H5Oget_info2((did1), &info1, H5O_INFO_HDR) < 0) TEST_ERROR \
    if(H5Oget_info2((did2), &info2, H5O_INFO_HDR) < 0) TEST_ERROR \
                                                                  \
    HDprintf("\n==HEADERS==  UNMINIMIZED  MINIMIZED\n");          \
    HDprintf("    version: %11u  %9u\n",                          \
            info1.hdr.version,                                    \
            info2.hdr.version);                                   \
    HDprintf(" # messages: %11u  %9u\n",                          \
            info1.hdr.nmesgs,                                     \
            info2.hdr.nmesgs);                                    \
    HDprintf("       meta: %11llu  %9llu\n",                      \
            info1.hdr.space.meta,                                 \
            info2.hdr.space.meta);                                \
    HDprintf("       free: %11llu  %9llu\n",                      \
            info1.hdr.space.free,                                 \
            info2.hdr.space.free);                                \
    HDprintf("      total: %11llu  %9llu\n",                      \
            info1.hdr.space.total,                                \
            info2.hdr.space.total);                               \
}


/*********************
 * UTILITY FUNCTIONS *
 *********************/


/* ---------------------------------------------------------------------------
 * Function:  put_attribute()
 *
 * Purpose:   Set an attribute with the given information.
 *
 *     If the out parameter `attr_id` is negative, a new attribute will be
 *     created with the given information. Else, it will attempt to update the
 *     attribute with the new value.
 *
 *     `dataspace_id` ignored if `attribute_id` >= 0
 *
 * Return: 0 (success) or -1 (failure)
 *
 * ---------------------------------------------------------------------------
 */
static herr_t
put_attribute(hid_t loc_id, const char *attrname, const void *attrvalue, hid_t datatype_id, hid_t dataspace_id, hid_t *attribute_id)
{
    if ((*attribute_id) < 0) {
        hid_t id = -1;
        id = H5Acreate(loc_id, attrname, datatype_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);
        if (id < 0)
            return FAIL;
        *attribute_id = id;
    }
    return H5Awrite(*attribute_id, datatype_id, attrvalue);
} /* put_attribute */


/* ---------------------------------------------------------------------------
 * Function:  count_attributes()
 *
 * Purpose: Count the number of attributes attached to an object.
 *
 *          TODO: If the location id is that of a file, tries to count all the
 *                attributes present in the file.
 *
 * Return: -1 if an error occurred, else the number of attributes.
 *
 * ---------------------------------------------------------------------------
 */
static int
count_attributes(hid_t dset_id)
{
    H5O_info_t info;

    if (0 > H5Oget_info(dset_id, &info, H5O_INFO_ALL))
        return -1;
    else
        return (int)info.num_attrs; /* should never exceed int bounds */
} /* count_attributes */


/* ---------------------------------------------------------------------------
 * Function:  _oh_getsize()
 *
 * Purpose: Get the total space used by the object header
 *
 *
 * Return: SUCCEED/FAIL. On success, stores size in `size_out` pointer.
 *
 * ---------------------------------------------------------------------------
 */
static herr_t
_oh_getsize(hid_t did, hsize_t *size_out)
{
    H5O_info_t info;
    if (FAIL == H5Oget_info2(did, &info, H5O_INFO_HDR))
        return FAIL;
    *size_out = info.hdr.space.total;
    return SUCCEED;
} /* _oh_getsize */


/* ---------------------------------------------------------------------------
 * Function:  oh_compare()
 *
 * Purpose: Compare the TOTAL space used by datasets' object headers.
 *
 *
 * Return: negative value if an error occurred,
 *         else positive #defined indicator value EQ, LT, GT.
 *
 * ---------------------------------------------------------------------------
 */
static int
oh_compare(hid_t did1, hid_t did2)
{
    hsize_t space1 = 0;
    hsize_t space2 = 0;

    if (FAIL == _oh_getsize(did1, &space1))
        return -1;
    if (FAIL == _oh_getsize(did2, &space2))
        return -2;

    if (space1 < space2)
        return LT;
    else if (space1 > space2)
        return GT;
    else
        return EQ;
}

/******************
 * TEST FUNCTIONS *
 ******************/


/* ---------------------------------------------------------------------------
 * Function:  test_attribute_addition()
 *
 * Purpose: Demonstrate attribute addition to datasets.
 *
 * Return: 0 (pass) or 1 (failure)
 *
 * ---------------------------------------------------------------------------
 */
static int
test_attribute_addition(void)
{
    hsize_t array_10[1]      = {10}; /* dataspace extent */
    char    buffer[10]       = "";   /* to inspect string attribute */
    int     a_out            = 0;
    char    filename[512]    = "";
    hid_t   int_type_id      = -1;
    hid_t   char_type_id     = -1;
    hid_t   dcpl_id          = -1;
    hid_t   dspace_id        = -1;
    hid_t   dspace_scalar_id = -1;
    hid_t   dset_id          = -1;
    hid_t   mindset_id       = -1;
    hid_t   attr_1_id        = -1;
    hid_t   attr_1a_id       = -1;
    hid_t   attr_2_id        = -1;
    hid_t   attr_2a_id       = -1;
    hid_t   attr_3_id        = -1;
    hid_t   attr_3a_id       = -1;
    hid_t   file_id          = -1;
    herr_t  ret;
    int     count = 0;

    TESTING("attribute additions to [un]minimized dataset")

    /*********
     * SETUP *
     *********/

    if(h5_fixname(OHMIN_FILENAME_A, H5P_DEFAULT, filename, sizeof(filename)) == NULL)
        TEST_ERROR

    dspace_id = H5Screate_simple(
            1,        /* rank */
            array_10, /* current dimensions */
            NULL);    /* maximum dimensions */
    if(dspace_id < 0) TEST_ERROR

    dspace_scalar_id = H5Screate(H5S_SCALAR);
    if(dspace_scalar_id < 0) TEST_ERROR

    char_type_id = H5Tcopy(H5T_NATIVE_CHAR);
    if(char_type_id < 0) TEST_ERROR

    int_type_id = H5Tcopy(H5T_NATIVE_INT);
    if(int_type_id < 0) TEST_ERROR

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_id < 0) TEST_ERROR

    ret = H5Pset_dset_no_attrs_hint(dcpl_id, TRUE);
    if(ret < 0) TEST_ERROR

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file_id < 0) TEST_ERROR

    H5E_BEGIN_TRY {
        count = count_attributes(dset_id);
    } H5E_END_TRY;
    if(count != -1) TEST_ERROR

    dset_id = H5Dcreate(file_id, "dataset", int_type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset_id < 0) TEST_ERROR

    mindset_id  = H5Dcreate(file_id, "mindataset", int_type_id, dspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if(mindset_id < 0) TEST_ERROR

    /********************
     * TEST/DEMONSTRATE *
     ********************/

    /* -------------------
     * no attributes added
     */

    count = count_attributes(dset_id);
    if(count != 0) TEST_ERROR
    count = count_attributes(mindset_id);
    if(count != 0) TEST_ERROR

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /* -----------------
     * add one attribute
     */
    ret = put_attribute(dset_id, "PURPOSE", "DEMO", char_type_id, dspace_id, &attr_1_id);
    if(ret < 0) TEST_ERROR

    ret = put_attribute(mindset_id, "PURPOSE", "DEMO", char_type_id, dspace_id, &attr_1a_id);
    if(ret < 0) TEST_ERROR

    count = count_attributes(dset_id);
    if(count != 1) TEST_ERROR
    count = count_attributes(mindset_id);
    if(count != 1) TEST_ERROR

    ret = H5Aread(attr_1_id, char_type_id, buffer);
    if(ret < 0) TEST_ERROR
    if(HDstrcmp("DEMO", buffer)) TEST_ERROR

    ret = H5Aread(attr_1a_id, char_type_id, buffer);
    if(ret < 0) TEST_ERROR
    if(HDstrcmp("DEMO", buffer)) TEST_ERROR

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /* -----------------
     * modify one attribute
     */

    ret = put_attribute(dset_id, "PURPOSE", "REWRITE", char_type_id, -1, &attr_1_id);
    if(ret < 0) TEST_ERROR

    ret = put_attribute(mindset_id, "PURPOSE", "REWRITE", char_type_id, -1, &attr_1a_id);
    if(ret < 0) TEST_ERROR

    count = count_attributes(dset_id);
    if(count != 1) TEST_ERROR
    count = count_attributes(mindset_id);
    if(count != 1) TEST_ERROR

    ret = H5Aread(attr_1_id, char_type_id, buffer);
    if(ret < 0) TEST_ERROR
    if(HDstrcmp("REWRITE", buffer)) TEST_ERROR

    ret = H5Aread(attr_1a_id, char_type_id, buffer);
    if(ret < 0) TEST_ERROR
    if(HDstrcmp("REWRITE", buffer)) TEST_ERROR

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /* -----------------
     * add second attribute
     */

    a_out = 5;
    ret = put_attribute(dset_id, "RANK", &a_out, int_type_id, dspace_scalar_id, &attr_2_id);
    if(ret < 0) TEST_ERROR

    a_out = 3;
    ret = put_attribute(mindset_id, "RANK", &a_out, int_type_id, dspace_scalar_id, &attr_2a_id);
    if(ret < 0) TEST_ERROR

    count = count_attributes(dset_id);
    if(count != 2) TEST_ERROR
    count = count_attributes(mindset_id);
    if(count != 2) TEST_ERROR

    ret = H5Aread(attr_2_id, int_type_id, &a_out);
    if(ret < 0) TEST_ERROR
    if(a_out != 5) TEST_ERROR

    ret = H5Aread(attr_2a_id, int_type_id, &a_out);
    if(ret < 0) TEST_ERROR
    if(a_out != 3) TEST_ERROR

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /* -----------------
     * add third attribute
     */

    a_out = -86;
    ret = put_attribute(dset_id, "FLAVOR", &a_out, int_type_id, dspace_scalar_id, &attr_3_id);
    if(ret < 0) TEST_ERROR

    a_out = 2185;
    ret = put_attribute(mindset_id, "FLAVOR", &a_out, int_type_id, dspace_scalar_id, &attr_3a_id);
    if(ret < 0) TEST_ERROR

    count = count_attributes(dset_id);
    if(count != 3) TEST_ERROR
    count = count_attributes(mindset_id);
    if(count != 3) TEST_ERROR

    ret = H5Aread(attr_3_id, int_type_id, &a_out);
    if(ret < 0) TEST_ERROR
    if(a_out != -86) TEST_ERROR

    ret = H5Aread(attr_3a_id, int_type_id, &a_out);
    if(ret < 0) TEST_ERROR
    if(a_out != 2185) TEST_ERROR

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /************
     * TEARDOWN *
     ************/

    if(H5Tclose(int_type_id) < 0) TEST_ERROR
    if(H5Tclose(char_type_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_id) < 0) TEST_ERROR
    if(H5Sclose(dspace_id) < 0) TEST_ERROR
    if(H5Dclose(dset_id) < 0) TEST_ERROR
    if(H5Dclose(mindset_id) < 0) TEST_ERROR
    if(H5Aclose(attr_1_id) < 0) TEST_ERROR
    if(H5Aclose(attr_1a_id) < 0) TEST_ERROR
    if(H5Aclose(attr_2_id) < 0) TEST_ERROR
    if(H5Aclose(attr_2a_id) < 0) TEST_ERROR
    if(H5Aclose(attr_3_id) < 0) TEST_ERROR
    if(H5Aclose(attr_3a_id) < 0) TEST_ERROR
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED()
    return 0;

error :
    H5E_BEGIN_TRY {
        (void)H5Tclose(int_type_id);
        (void)H5Tclose(char_type_id);
        (void)H5Pclose(dcpl_id);
        (void)H5Sclose(dspace_id);
        (void)H5Dclose(dset_id);
        (void)H5Dclose(mindset_id);
        (void)H5Aclose(attr_1_id);
        (void)H5Aclose(attr_1a_id);
        (void)H5Aclose(attr_2_id);
        (void)H5Aclose(attr_2a_id);
        (void)H5Aclose(attr_3_id);
        (void)H5Aclose(attr_3a_id);
        (void)H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* test_attribute_addition */


/* ---------------------------------------------------------------------------
 * Function:  test_size_comparisons()
 *
 * Purpose: Examine when headers have been minimized.
 *
 * Return: 0 (pass) or 1 (failure)
 *
 * ---------------------------------------------------------------------------
 */
static int
test_size_comparisons(void)
{
    hsize_t array_10[1] = {10}; /* dataspace extents */

    /* IDs that are file-agnostic */
    hid_t dspace_id     = -1;
    hid_t int_type_id   = -1;
    hid_t dcpl_minimize = -1;
    hid_t dcpl_dontmin  = -1;

    /* IDs for non-minimzed file open */
    hid_t file_f_id   = -1; /* lower 'f' for standard file setting */
    hid_t dset_f_x_id = -1; /* 'x' for default */
    hid_t dset_f_N_id = -1; /* 'N' for explcit non-minimized dset */
    hid_t dset_f_Y_id = -1; /* 'Y' for minimzed dset */

    /* IDs for minimzed file open */
    hid_t file_F_id   = -1; /* upper 'F' for minimzed file setting */
    hid_t dset_F_x_id = -1; /* 'x' for default */
    hid_t dset_F_N_id = -1; /* 'N' for explcit non-minimized dset */
    hid_t dset_F_Y_id = -1; /* 'Y' for minimzed dset */

    char filename_a[512] = "";
    char filename_b[512] = "";

    herr_t ret;

    TESTING("default size comparisons");

    /*********
     * SETUP *
     *********/

    if(h5_fixname(OHMIN_FILENAME_A, H5P_DEFAULT, filename_a, sizeof(filename_a)) == NULL)
        TEST_ERROR

    if(h5_fixname(OHMIN_FILENAME_B, H5P_DEFAULT, filename_b, sizeof(filename_b)) == NULL)
        TEST_ERROR

    dcpl_minimize = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_minimize < 0) TEST_ERROR

    ret = H5Pset_dset_no_attrs_hint(dcpl_minimize, TRUE);
    if(ret < 0) TEST_ERROR

    dcpl_dontmin = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_dontmin < 0) TEST_ERROR

    ret = H5Pset_dset_no_attrs_hint(dcpl_dontmin, FALSE);
    if(ret < 0) TEST_ERROR

    dspace_id = H5Screate_simple(1, array_10, NULL);
    if(dspace_id < 0) TEST_ERROR

    int_type_id = H5Tcopy(H5T_NATIVE_INT);
    if(int_type_id < 0) TEST_ERROR

    file_f_id = H5Fcreate(filename_a, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file_f_id < 0) TEST_ERROR

    dset_f_x_id = H5Dcreate(file_f_id, "default", int_type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset_f_x_id < 0) TEST_ERROR

    dset_f_N_id = H5Dcreate(file_f_id, "dsetNOT", int_type_id, dspace_id, H5P_DEFAULT, dcpl_dontmin, H5P_DEFAULT);
    if(dset_f_N_id < 0) TEST_ERROR

    dset_f_Y_id = H5Dcreate(file_f_id, "dsetMIN", int_type_id, dspace_id, H5P_DEFAULT, dcpl_minimize, H5P_DEFAULT);
    if(dset_f_x_id < 0) TEST_ERROR

    file_F_id = H5Fcreate(filename_b, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file_F_id < 0) TEST_ERROR
    ret = H5Fset_dset_no_attrs_hint(file_F_id, TRUE);
    if(ret < 0) TEST_ERROR

    dset_F_x_id = H5Dcreate(file_F_id, "default", int_type_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset_F_x_id < 0) TEST_ERROR

    dset_F_N_id = H5Dcreate(file_F_id, "dsetNOT", int_type_id, dspace_id, H5P_DEFAULT, dcpl_dontmin, H5P_DEFAULT);
    if(dset_F_N_id < 0) TEST_ERROR

    dset_F_Y_id = H5Dcreate(file_F_id, "dsetMIN", int_type_id, dspace_id, H5P_DEFAULT, dcpl_minimize, H5P_DEFAULT);
    if(dset_F_Y_id < 0) TEST_ERROR

    /*********
     * TESTS *
     *********/

    if(oh_compare(dset_f_x_id, dset_f_x_id) != EQ) TEST_ERROR /* identity */

    if(oh_compare(dset_f_x_id, dset_f_N_id) != EQ) TEST_ERROR
    if(oh_compare(dset_f_x_id, dset_f_Y_id) != GT) TEST_ERROR
    if(oh_compare(dset_f_N_id, dset_f_Y_id) != GT) TEST_ERROR

    if(oh_compare(dset_F_x_id, dset_F_N_id) != EQ) TEST_ERROR
    if(oh_compare(dset_F_x_id, dset_F_Y_id) != EQ) TEST_ERROR
    if(oh_compare(dset_F_N_id, dset_F_Y_id) != EQ) TEST_ERROR

    if(oh_compare(dset_F_x_id, dset_f_Y_id) != EQ) TEST_ERROR
    if(oh_compare(dset_F_x_id, dset_f_x_id) != LT) TEST_ERROR

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_f_x_id, dset_F_x_id)

    /************
     * TEARDOWN *
     ************/

    if(H5Sclose(dspace_id) < 0) TEST_ERROR
    if(H5Tclose(int_type_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_minimize) < 0) TEST_ERROR
    if(H5Pclose(dcpl_dontmin) < 0) TEST_ERROR

    if(H5Fclose(file_f_id) < 0) TEST_ERROR
    if(H5Dclose(dset_f_x_id) < 0) TEST_ERROR
    if(H5Dclose(dset_f_N_id) < 0) TEST_ERROR
    if(H5Dclose(dset_f_Y_id) < 0) TEST_ERROR

    if(H5Fclose(file_F_id) < 0) TEST_ERROR
    if(H5Dclose(dset_F_x_id) < 0) TEST_ERROR
    if(H5Dclose(dset_F_N_id) < 0) TEST_ERROR
    if(H5Dclose(dset_F_Y_id) < 0) TEST_ERROR

    PASSED()
    return 0;

error :
    H5E_BEGIN_TRY {
        (void)H5Pclose(dcpl_minimize);
        (void)H5Pclose(dcpl_dontmin);
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(int_type_id);

        (void)H5Fclose(file_f_id);
        (void)H5Dclose(dset_f_x_id);
        (void)H5Dclose(dset_f_N_id);
        (void)H5Dclose(dset_f_Y_id);

        (void)H5Fclose(file_F_id);
        (void)H5Dclose(dset_F_x_id);
        (void)H5Dclose(dset_F_N_id);
        (void)H5Dclose(dset_F_Y_id);
    } H5E_END_TRY;
    return 1;
} /* test_size_comparisons */


/* ---------------------------------------------------------------------------
 * Test minimized dataset header with filter/pipeline message
 * ---------------------------------------------------------------------------
 */
static int
test_minimized_with_filter(void)
{
    char           filename[512]   = "";
    const hsize_t  extents[1]      = {1024}; /* extents of dataspace */
    const unsigned filter_values[] = {0};  /* TBD */
    const hsize_t  chunk_dim[]     = {32};  /* needed for filter */
    const int      ndims           = 1;
    hid_t          dspace_id       = -1;
    hid_t          dtype_id        = -1;
    hid_t          dcpl_xZ_id      = -1;
    hid_t          dcpl_mx_id      = -1;
    hid_t          dcpl_mZ_id      = -1;
    hid_t          dset_xx_id      = -1;
    hid_t          dset_xZ_id      = -1;
    hid_t          dset_mx_id      = -1;
    hid_t          dset_mZ_id      = -1;
    hid_t          file_id         = -1;
    herr_t         ret;

/*           | default | minimize
 * ----------+---------+---------
 * no filter |    xx   |   mx
 * ----------+---------+---------
 * filter    |    xZ   |   mZ
 */

    TESTING("with filter message");

    /*********
     * SETUP *
     *********/

    if(h5_fixname(OHMIN_FILENAME_A, H5P_DEFAULT, filename, sizeof(filename)) == NULL)
        TEST_ERROR

    dcpl_mx_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_mx_id < 0) TEST_ERROR
    ret = H5Pset_dset_no_attrs_hint(dcpl_mx_id, TRUE);
    if(ret < 0) TEST_ERROR

    dcpl_xZ_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_xZ_id < 0) TEST_ERROR
    ret = H5Pset_chunk(dcpl_xZ_id, ndims, chunk_dim);
    if(ret < 0) TEST_ERROR
    ret = H5Pset_filter(dcpl_xZ_id, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, 0, filter_values);
    if(ret < 0) TEST_ERROR

    dcpl_mZ_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_mZ_id < 0) TEST_ERROR
    ret = H5Pset_dset_no_attrs_hint(dcpl_mZ_id, TRUE);
    if(ret < 0) TEST_ERROR
    ret = H5Pset_chunk(dcpl_mZ_id, ndims, chunk_dim);
    if(ret < 0) TEST_ERROR
    ret = H5Pset_filter( dcpl_mZ_id, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, 0, filter_values);
    if(ret < 0) TEST_ERROR

    dspace_id = H5Screate_simple(1, extents, extents);
    if(dspace_id < 0) TEST_ERROR

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    if(dtype_id < 0) TEST_ERROR

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file_id < 0) TEST_ERROR

    dset_xx_id = H5Dcreate(file_id, "xx", dtype_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset_xx_id < 0) TEST_ERROR

    dset_mx_id = H5Dcreate(file_id, "Mx", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mx_id, H5P_DEFAULT);
    if(dset_mx_id < 0) TEST_ERROR

    dset_xZ_id = H5Dcreate(file_id, "xZ", dtype_id, dspace_id, H5P_DEFAULT, dcpl_xZ_id, H5P_DEFAULT);
    if(dset_xZ_id < 0) TEST_ERROR

    dset_mZ_id = H5Dcreate(file_id, "MZ", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mZ_id, H5P_DEFAULT);
    if(dset_mZ_id < 0) TEST_ERROR

    /*********
     * TESTS *
     *********/

    if(oh_compare(dset_mx_id, dset_xx_id) != LT) TEST_ERROR
    if(oh_compare(dset_mx_id, dset_xZ_id) != LT) TEST_ERROR
    if(oh_compare(dset_mZ_id, dset_mx_id) != GT) TEST_ERROR
    if(oh_compare(dset_mZ_id, dset_xZ_id) != LT) TEST_ERROR

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_xZ_id, dset_mZ_id)

    /************
     * TEARDOWN *
     ************/

    if(H5Sclose(dspace_id) < 0) TEST_ERROR
    if(H5Tclose(dtype_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_xZ_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_mx_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_mZ_id) < 0) TEST_ERROR
    if(H5Dclose(dset_xx_id) < 0) TEST_ERROR
    if(H5Dclose(dset_xZ_id) < 0) TEST_ERROR
    if(H5Dclose(dset_mx_id) < 0) TEST_ERROR
    if(H5Dclose(dset_mZ_id) < 0) TEST_ERROR
    if(H5Fclose(file_id) < 0) TEST_ERROR

    PASSED()
    return 0;

error:
    H5E_BEGIN_TRY {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(dcpl_xZ_id);
        (void)H5Pclose(dcpl_mx_id);
        (void)H5Pclose(dcpl_mZ_id);
        (void)H5Dclose(dset_xx_id);
        (void)H5Dclose(dset_xZ_id);
        (void)H5Dclose(dset_mx_id);
        (void)H5Dclose(dset_mZ_id);
        (void)H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* test_minimized_with_filter */


/* ---------------------------------------------------------------------------
 * Test minimized and recording modification times.
 * ---------------------------------------------------------------------------
 */
static int
test_modification_times(void)
{
    /* test-local structure for parameterized testing
     */
    struct testcase {
        unsigned oh_version;
    };

    char          filename[512] = "";
    const hsize_t extents[1]    = {128}; /* extents of dataspace */
    hid_t         dspace_id     = -1;
    hid_t         dtype_id      = -1;
    hid_t         dcpl_xT_id    = -1; /* Track modtime */
    hid_t         dcpl_mx_id    = -1; /* minimized */
    hid_t         dcpl_mT_id    = -1; /* minimized, Track */
    hid_t         dcpl_mN_id    = -1; /* minimized, do Not track */
    hid_t         dset_xx_id    = -1;
    hid_t         dset_xT_id    = -1;
    hid_t         dset_mx_id    = -1;
    hid_t         dset_mT_id    = -1;
    hid_t         dset_mN_id    = -1;
    hid_t         file_id       = -1;
    hid_t         fapl_id       = -1;
    herr_t        ret;

    unsigned i       = 0; /* for testcase loop */
    unsigned n_cases = 2; /* must match `cases` array size below */
    struct testcase cases[2] = {
        { 1, }, /* version 1 object header */
        { 2, }, /* version 2 object header */
    };

    TESTING("with modification times");

    /*********
     * SETUP *
     *********/

    if(h5_fixname(OHMIN_FILENAME_A, H5P_DEFAULT, filename, sizeof(filename)) == NULL)
        TEST_ERROR

    dcpl_mx_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_mx_id < 0) TEST_ERROR
    ret = H5Pset_dset_no_attrs_hint(dcpl_mx_id, TRUE);
    if(ret < 0) TEST_ERROR

    dcpl_xT_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_xT_id < 0) TEST_ERROR
    ret = H5Pset_obj_track_times(dcpl_xT_id, TRUE);
    if(ret < 0) TEST_ERROR

    dcpl_mT_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_mT_id < 0) TEST_ERROR
    ret = H5Pset_dset_no_attrs_hint(dcpl_mT_id, TRUE);
    if(ret < 0) TEST_ERROR
    ret = H5Pset_obj_track_times(dcpl_mT_id, TRUE);
    if(ret < 0) TEST_ERROR

    dcpl_mN_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_mN_id < 0) TEST_ERROR
    ret = H5Pset_dset_no_attrs_hint(dcpl_mN_id, TRUE);
    if(ret < 0) TEST_ERROR
    ret = H5Pset_obj_track_times(dcpl_mN_id, FALSE);
    if(ret < 0) TEST_ERROR

    dspace_id = H5Screate_simple(1, extents, extents);
    if(dspace_id < 0) TEST_ERROR

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    if(dtype_id < 0) TEST_ERROR

    for (i = 0; i < n_cases; i++) {

        /* -------------- *
         * per-case setup *
         * -------------- */

        fapl_id = H5P_DEFAULT;

        if (cases[i].oh_version > 1) {
            fapl_id = H5Pcreate(H5P_FILE_ACCESS);
            if(fapl_id < 0) TEST_ERROR
            ret = H5Pset_libver_bounds(fapl_id, H5F_LIBVER_V18, H5F_LIBVER_V110);
            if(ret < 0) TEST_ERROR
        }

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        if(file_id < 0) TEST_ERROR

        dset_xx_id = H5Dcreate( file_id, "xx", dtype_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if(dset_xx_id < 0) TEST_ERROR

        dset_mx_id = H5Dcreate(file_id, "mx", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mx_id, H5P_DEFAULT);
        if(dset_mx_id < 0) TEST_ERROR

        dset_xT_id = H5Dcreate(file_id, "xT", dtype_id, dspace_id, H5P_DEFAULT, dcpl_xT_id, H5P_DEFAULT);
        if(dset_xT_id < 0) TEST_ERROR

        dset_mT_id = H5Dcreate(file_id, "mT", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mT_id, H5P_DEFAULT);
        if(dset_mT_id < 0) TEST_ERROR

        dset_mN_id = H5Dcreate(file_id, "mN", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mN_id, H5P_DEFAULT);
        if(dset_mN_id < 0) TEST_ERROR

        /* ----- *
         * TESTS *
         * ----- */

        /* sanity check */
        if(oh_compare(dset_mx_id, dset_xx_id) != LT) TEST_ERROR
        if(oh_compare(dset_mx_id, dset_xT_id) != LT) TEST_ERROR

        if (DEBUG_OH_SIZE) {
            PRINT_DSET_OH_COMPARISON(dset_xx_id, dset_mx_id)
            PRINT_DSET_OH_COMPARISON(dset_xT_id, dset_mT_id)
            PRINT_DSET_OH_COMPARISON(dset_mT_id, dset_mN_id)
        }

        if(oh_compare(dset_xx_id, dset_xT_id) != EQ) TEST_ERROR
        if(oh_compare(dset_mx_id, dset_mT_id) != EQ) TEST_ERROR
        if(oh_compare(dset_mN_id, dset_mT_id) != LT) TEST_ERROR

        if(oh_compare(dset_mT_id, dset_xT_id) != LT) TEST_ERROR

        /* ----------------- *
         * per-case teardown *
         * ----------------- */

        if(H5Dclose(dset_xx_id) < 0) TEST_ERROR
        if(H5Dclose(dset_xT_id) < 0) TEST_ERROR
        if(H5Dclose(dset_mx_id) < 0) TEST_ERROR
        if(H5Dclose(dset_mT_id) < 0) TEST_ERROR
        if(H5Dclose(dset_mN_id) < 0) TEST_ERROR
        if(H5Fclose(file_id) < 0) TEST_ERROR

        if ((fapl_id != H5P_DEFAULT) && (H5Pclose(fapl_id) < 0))
            TEST_ERROR

    } /* for each version tested */

    /************
     * TEARDOWN *
     ************/

    if(H5Sclose(dspace_id) < 0) TEST_ERROR
    if(H5Tclose(dtype_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_xT_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_mx_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_mT_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_mN_id) < 0) TEST_ERROR

    PASSED()
    return 0;

error:
    H5E_BEGIN_TRY {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(dcpl_xT_id);
        (void)H5Pclose(dcpl_mx_id);
        (void)H5Pclose(dcpl_mT_id);
        (void)H5Pclose(dcpl_mN_id);
        (void)H5Dclose(dset_xx_id);
        (void)H5Dclose(dset_xT_id);
        (void)H5Dclose(dset_mx_id);
        (void)H5Dclose(dset_mT_id);
        (void)H5Dclose(dset_mN_id);
        (void)H5Fclose(file_id);
        (void)H5Pclose(fapl_id);
    } H5E_END_TRY;
    return 1;
} /* test_modification_times */


/* ---------------------------------------------------------------------------
 * Test minimized dataset header with a fill value set.
 * ---------------------------------------------------------------------------
 */
static int
test_fillvalue_backwards_compatability(void)
{
    char          filename[512] = "";
    const hsize_t extents[1]    = {64}; /* extents of dataspace */
    const int     fill[1]       = {343}; /* fill value of dataset */
    hid_t         file_id       = -1;
    hid_t         dtype_id      = -1;
    hid_t         dspace_id     = -1;
    hid_t         dcpl_id       = -1;
    hid_t         fapl_id       = -1;
    hid_t         dset_0_id     = -1;
    hid_t         dset_1_id     = -1;
    herr_t        ret;

    /*********
     * SETUP *
     *********/

    TESTING("with fill values and different libver support");

    if(h5_fixname(OHMIN_FILENAME_A, H5P_DEFAULT, filename, sizeof(filename)) == NULL)
        TEST_ERROR

    dspace_id = H5Screate_simple(1, extents, extents);
    if(dspace_id < 0) TEST_ERROR

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    if(dtype_id < 0) TEST_ERROR

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl_id < 0) TEST_ERROR

    ret = H5Pset_dset_no_attrs_hint(dcpl_id, TRUE);
    if(ret == FAIL) TEST_ERROR;

    ret = H5Pset_fill_value(dcpl_id, dtype_id, fill);
    if(ret == FAIL) TEST_ERROR;

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if(fapl_id < 0) TEST_ERROR

    ret = H5Pset_libver_bounds(fapl_id, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST);
    if(ret == FAIL) TEST_ERROR;

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if(file_id < 0) TEST_ERROR

    dset_0_id = H5Dcreate(file_id, "fullrange", dtype_id, dspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if(dset_0_id < 0) TEST_ERROR

    /* Close file and re-open with different libver bounds.
     * Dataset "fullrange" must also be closed for expected reopen behavior.
     */
    if(H5Fclose(file_id) < 0) TEST_ERROR;
    if(H5Dclose(dset_0_id) < 0) TEST_ERROR

    ret = H5Pset_libver_bounds(fapl_id, H5F_LIBVER_V18, H5F_LIBVER_LATEST);
    if(ret == FAIL) TEST_ERROR;

    file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);
    if(file_id < 0) TEST_ERROR

    dset_1_id = H5Dcreate(file_id, "upperrange", dtype_id, dspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if(dset_1_id < 0) TEST_ERROR

    /* re-open "fullrange" dataset
     */
     dset_0_id = H5Dopen2(file_id, "fullrange", H5P_DEFAULT);
     if(dset_0_id < 0) TEST_ERROR

    /*********
     * TESTS *
     *********/

    if(DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_1_id, dset_0_id)

    /* dset not supporting pre-1.08 should be smaller? */
    if(oh_compare(dset_1_id, dset_0_id) != LT) TEST_ERROR

    /************
     * TEARDOWN *
     ************/

    if(H5Sclose(dspace_id) < 0) TEST_ERROR
    if(H5Tclose(dtype_id) < 0) TEST_ERROR
    if(H5Pclose(dcpl_id) < 0) TEST_ERROR
    if(H5Pclose(fapl_id) < 0) TEST_ERROR
    if(H5Dclose(dset_0_id) < 0) TEST_ERROR
    if(H5Dclose(dset_1_id) < 0) TEST_ERROR
    if(H5Fclose(file_id) < 0) TEST_ERROR;

    PASSED()
    return 0;

error:
    H5E_BEGIN_TRY {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(dcpl_id);
        (void)H5Pclose(fapl_id);
        (void)H5Dclose(dset_0_id);
        (void)H5Dclose(dset_1_id);
        (void)H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* test_fillvalue_backwards_compatability */

/********
 * MAIN *
 ********/


/* ---------------------------------------------------------------------------
 * Main function is main. Runs tests.
 *
 * Returns number of failed tests.
 * ---------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    HDprintf("Testing minimized dataset object headers.\n");

    nerrors += test_attribute_addition();
    nerrors += test_size_comparisons();
    nerrors += test_minimized_with_filter();
    nerrors += test_modification_times();
    nerrors += test_fillvalue_backwards_compatability();

    if (nerrors > 0) {
        HDprintf("***** %d MINIMIZED DATASET OHDR TEST%s FAILED! *****\n",
                nerrors,
                nerrors > 1 ? "S" : "");
    } else {
        HDprintf("All minimized dataset object header tests passed.\n");
    }

    return nerrors;
} /* main */


