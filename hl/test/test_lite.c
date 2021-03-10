/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5hltest.h"
#include "H5srcdir.h"
#include "H5LTpublic.h"

#define FILE_NAME  "test_lite1.h5"
#define FILE_NAME2 "test_lite2.h5"
#define FILE_NAME3 "test_lite3.h5"
#define FILE_NAME4 "test_lite4.h5"
#define INPUT_FILE "dtype_file.txt"

#define DSET0_NAME "2D int array"
#define DSET1_NAME "dataset char"
#define DSET2_NAME "dataset short"
#define DSET3_NAME "dataset int"
#define DSET4_NAME "dataset long"
#define DSET5_NAME "dataset float"
#define DSET6_NAME "dataset double"
#define DSET7_NAME "dataset string"

#define DIM 6

#define ATTR_NAME_SUB "att"
#define ATTR1_NAME    "attr string"
#define ATTR2_NAME    "attr char"
#define ATTR3_NAME    "attr short"
#define ATTR4_NAME    "attr int"
#define ATTR_NAME_EXT "att int ext"
#define ATTR5_NAME    "attr long"
#define ATTR6_NAME    "attr uchar"
#define ATTR7_NAME    "attr ushort"
#define ATTR8_NAME    "attr uint"
#define ATTR9_NAME    "attr ulong"
#define ATTR10_NAME   "attr float"
#define ATTR11_NAME   "attr double"

static herr_t make_attributes(hid_t loc_id, const char *obj_name);

/*-------------------------------------------------------------------------
 * test dataset functions
 *-------------------------------------------------------------------------
 */

static int
test_dsets(void)
{
    int         rank    = 2;
    hsize_t     dims[2] = {2, 3};
    hid_t       file_id;
    hid_t       dataset_id;
    char        data_char_in[DIM] = {1, 2, 3, 4, 5, 6};
    char        data_char_out[DIM];
    short       data_short_in[DIM] = {1, 2, 3, 4, 5, 6};
    short       data_short_out[DIM];
    int         data_int_in[DIM] = {1, 2, 3, 4, 5, 6};
    int         data_int_out[DIM];
    long        data_long_in[DIM] = {1, 2, 3, 4, 5, 6};
    long        data_long_out[DIM];
    float       data_float_in[DIM] = {1, 2, 3, 4, 5, 6};
    float       data_float_out[DIM];
    double      data_double_in[DIM] = {1, 2, 3, 4, 5, 6};
    double      data_double_out[DIM];
    const char *data_string_in = "This is a string";
    char        data_string_out[20];
    int         i;

    /* Create a new file using default properties. */
    file_id = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * H5LTmake_dataset test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTmake_dataset");

    /* Make dataset */
    if (H5LTmake_dataset(file_id, DSET0_NAME, rank, dims, H5T_NATIVE_INT, data_int_in) < 0)
        goto out;

    /* Read dataset using the basic HDF5 API */

    if ((dataset_id = H5Dopen2(file_id, DSET0_NAME, H5P_DEFAULT)) < 0)
        goto out;

    if (H5Dread(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_int_out) < 0)
        goto out;

    if (H5Dclose(dataset_id) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_int_in[i] != data_int_out[i]) {
            goto out;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * read using the LT function H5LTread_dataset
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTread_dataset");

    if (H5LTread_dataset(file_id, DSET0_NAME, H5T_NATIVE_INT, data_int_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_int_in[i] != data_int_out[i]) {
            goto out;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * test the H5LTmake_dataset_ functions
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5LTmake_dataset_char
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTmake_dataset_char");

    /* Make dataset char */
    if (H5LTmake_dataset_char(file_id, DSET1_NAME, rank, dims, data_char_in) < 0)
        goto out;

    /* Read dataset */
    if (H5LTread_dataset(file_id, DSET1_NAME, H5T_NATIVE_CHAR, data_char_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_char_in[i] != data_char_out[i]) {
            goto out;
        }
    }

    /* Read dataset */
    if (H5LTread_dataset_char(file_id, DSET1_NAME, data_char_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_char_in[i] != data_char_out[i]) {
            goto out;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTmake_dataset_short
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTmake_dataset_short");

    /* Make dataset short */
    if (H5LTmake_dataset_short(file_id, DSET2_NAME, rank, dims, data_short_in) < 0)
        goto out;

    /* Read dataset */
    if (H5LTread_dataset(file_id, DSET2_NAME, H5T_NATIVE_SHORT, data_short_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_short_in[i] != data_short_out[i]) {
            goto out;
        }
    }

    /* Read dataset */
    if (H5LTread_dataset_short(file_id, DSET2_NAME, data_short_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_short_in[i] != data_short_out[i]) {
            goto out;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTmake_dataset_int
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTmake_dataset_int");

    /* Make dataset int */
    if (H5LTmake_dataset_int(file_id, DSET3_NAME, rank, dims, data_int_in) < 0)
        goto out;

    /* Read dataset */
    if (H5LTread_dataset(file_id, DSET3_NAME, H5T_NATIVE_INT, data_int_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_int_in[i] != data_int_out[i]) {
            goto out;
        }
    }

    /* Read dataset */
    if (H5LTread_dataset_int(file_id, DSET3_NAME, data_int_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_int_in[i] != data_int_out[i]) {
            goto out;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTmake_dataset_long
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTmake_dataset_long");

    /* Make dataset long */
    if (H5LTmake_dataset_long(file_id, DSET4_NAME, rank, dims, data_long_in) < 0)
        goto out;

    /* Read dataset */
    if (H5LTread_dataset(file_id, DSET4_NAME, H5T_NATIVE_LONG, data_long_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_long_in[i] != data_long_out[i]) {
            goto out;
        }
    }

    /* Read dataset */
    if (H5LTread_dataset_long(file_id, DSET4_NAME, data_long_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (data_long_in[i] != data_long_out[i]) {
            goto out;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTmake_dataset_float
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTmake_dataset_float");

    /* Make dataset float */
    if (H5LTmake_dataset_float(file_id, DSET5_NAME, rank, dims, data_float_in) < 0)
        goto out;

    /* Read dataset */
    if (H5LTread_dataset(file_id, DSET5_NAME, H5T_NATIVE_FLOAT, data_float_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (!H5_FLT_ABS_EQUAL(data_float_in[i], data_float_out[i])) {
            goto out;
        }
    }

    /* Read dataset */
    if (H5LTread_dataset_float(file_id, DSET5_NAME, data_float_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (!H5_FLT_ABS_EQUAL(data_float_in[i], data_float_out[i])) {
            goto out;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTmake_dataset_double
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTmake_dataset_double");

    /* Make dataset double */
    if (H5LTmake_dataset_double(file_id, DSET6_NAME, rank, dims, data_double_in) < 0)
        goto out;

    /* Read dataset */
    if (H5LTread_dataset(file_id, DSET6_NAME, H5T_NATIVE_DOUBLE, data_double_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (!H5_DBL_ABS_EQUAL(data_double_in[i], data_double_out[i])) {
            goto out;
        }
    }

    /* Read dataset */
    if (H5LTread_dataset_double(file_id, DSET6_NAME, data_double_out) < 0)
        goto out;

    for (i = 0; i < DIM; i++) {
        if (!H5_DBL_ABS_EQUAL(data_double_in[i], data_double_out[i])) {
            goto out;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTmake_dataset_string
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTmake_dataset_string");

    /* Make dataset string */
    if (H5LTmake_dataset_string(file_id, DSET7_NAME, data_string_in) < 0)
        goto out;

    /* Read dataset */
    if (H5LTread_dataset_string(file_id, DSET7_NAME, data_string_out) < 0)
        goto out;

    if (HDstrcmp(data_string_in, data_string_out) != 0)
        goto out;

    /*-------------------------------------------------------------------------
     * end tests
     *-------------------------------------------------------------------------
     */

    /* Close the file. */
    H5Fclose(file_id);

    PASSED();

    return 0;

out:
    /* Close the file. */
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * test attribute functions
 *-------------------------------------------------------------------------
 */

static int
test_attr(void)
{
    hid_t   file_id;
    hid_t   dataset_id;
    hid_t   group_id;
    hid_t   space_id;
    hsize_t dims[1] = {5};

    /* Create a new file using default properties. */
    file_id = H5Fcreate(FILE_NAME2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * Create a dataset named "dset" on the root group
     *-------------------------------------------------------------------------
     */

    /* Create the data space  */
    if ((space_id = H5Screate_simple(1, dims, NULL)) < 0)
        goto out;

    /* Create the dataset */
    if ((dataset_id = H5Dcreate2(file_id, "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        goto out;

    /* Close */
    H5Dclose(dataset_id);

    /*-------------------------------------------------------------------------
     * Create a group named "grp" on the root group
     *-------------------------------------------------------------------------
     */

    /* Create a group. */
    if ((group_id = H5Gcreate2(file_id, "grp", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* Close */
    H5Gclose(group_id);

    /*-------------------------------------------------------------------------
     *
     * Create attributes in the root group
     * Note that we are calling the H5LTset_attribute functions with the name "."
     *
     *-------------------------------------------------------------------------
     */
    if (make_attributes(file_id, ".") < 0)
        goto out;

    /*-------------------------------------------------------------------------
     *
     * Create attributes in the dataset "dset"
     *
     *-------------------------------------------------------------------------
     */
    if (make_attributes(file_id, "dset") < 0)
        goto out;

    /*-------------------------------------------------------------------------
     *
     * Create attributes in the group "grp"
     *
     *-------------------------------------------------------------------------
     */
    if (make_attributes(file_id, "grp") < 0)
        goto out;

    /*-------------------------------------------------------------------------
     * end
     *-------------------------------------------------------------------------
     */
    /* Close the file. */
    H5Fclose(file_id);

    return 0;

out:
    /* Close the file. */
    H5Fclose(file_id);

    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * make_attributes
 *-------------------------------------------------------------------------
 */

static herr_t
make_attributes(hid_t loc_id, const char *obj_name)
{

    int         rank_out;
    hsize_t *   dims_out = 0;
    H5T_class_t type_class;
    size_t      type_size;
    int         i;

    char           attr_str_in[] = {"My attribute"};
    char           attr_str_out[20];
    char           attr_char_in[5] = {1, 2, 3, 4, 5};
    char           attr_char_out[5];
    short          attr_short_in[5] = {1, 2, 3, 4, 5};
    short          attr_short_out[5];
    int            attr_int_in[5] = {1, 2, 3, 4, 5};
    int            attr_int_out[5];
    long           attr_long_in[5] = {1, 2, 3, 4, 5};
    long           attr_long_out[5];
    float          attr_float_in[5] = {1, 2, 3, 4, 5};
    float          attr_float_out[5];
    double         attr_double_in[5] = {1, 2, 3, 4, 5};
    double         attr_double_out[5];
    unsigned char  attr_uchar_in[5] = {1, 2, 3, 4, 5};
    unsigned char  attr_uchar_out[5];
    unsigned short attr_ushort_in[5] = {1, 2, 3, 4, 5};
    unsigned short attr_ushort_out[5];
    unsigned int   attr_uint_in[5] = {1, 2, 3, 4, 5};
    unsigned int   attr_uint_out[5];
    unsigned long  attr_ulong_in[5] = {1, 2, 3, 4, 5};
    unsigned long  attr_ulong_out[5];

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_string test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_string");

    /* Set the attribute */
    if (H5LTset_attribute_string(loc_id, obj_name, ATTR1_NAME, attr_str_in) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_string test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_string");

    /* Get the attribute */
    if (H5LTget_attribute_string(loc_id, obj_name, ATTR1_NAME, attr_str_out) < 0)
        return -1;

    if (HDstrcmp(attr_str_in, attr_str_out) != 0) {
        return -1;
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_char test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_char");

    /* Set the attribute */
    if (H5LTset_attribute_char(loc_id, obj_name, ATTR2_NAME, attr_char_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_char test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_char");

    /* Get the attribute */
    if (H5LTget_attribute_char(loc_id, obj_name, ATTR2_NAME, attr_char_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_char_in[i] != attr_char_out[i]) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR2_NAME, H5T_NATIVE_CHAR, attr_char_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_char_in[i] != attr_char_out[i]) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_short test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_short");

    /* Set the attribute */
    if (H5LTset_attribute_short(loc_id, obj_name, ATTR3_NAME, attr_short_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_short test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_short");

    /* Get the attribute */
    if (H5LTget_attribute_short(loc_id, obj_name, ATTR3_NAME, attr_short_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_short_in[i] != attr_short_out[i]) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR3_NAME, H5T_NATIVE_SHORT, attr_short_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_short_in[i] != attr_short_out[i]) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_int test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_int");

    /* Set the attribute */
    if (H5LTset_attribute_int(loc_id, obj_name, ATTR4_NAME, attr_int_in, (size_t)5) < 0)
        return -1;

    /* Set the attribute which is a substring of an existing attribute */
    if (H5LTset_attribute_int(loc_id, obj_name, ATTR_NAME_SUB, attr_int_in, (size_t)5) < 0)
        return -1;

    /* Set the attribute which is an extension of an existing attribute */
    if (H5LTset_attribute_int(loc_id, obj_name, ATTR_NAME_EXT, attr_int_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_int test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_int");

    /* Get the attribute */
    if (H5LTget_attribute_int(loc_id, obj_name, ATTR4_NAME, attr_int_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_int_in[i] != attr_int_out[i]) {
            return -1;
        }
    }

    if (H5LTget_attribute_int(loc_id, obj_name, ATTR_NAME_SUB, attr_int_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_int_in[i] != attr_int_out[i]) {
            return -1;
        }
    }

    if (H5LTget_attribute_int(loc_id, obj_name, ATTR_NAME_EXT, attr_int_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_int_in[i] != attr_int_out[i]) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR4_NAME, H5T_NATIVE_INT, attr_int_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_int_in[i] != attr_int_out[i]) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_long test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_long");

    /* Set the attribute */
    if (H5LTset_attribute_long(loc_id, obj_name, ATTR5_NAME, attr_long_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_long test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_long");

    /* Get the attribute */
    if (H5LTget_attribute_long(loc_id, obj_name, ATTR5_NAME, attr_long_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_long_in[i] != attr_long_out[i]) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR5_NAME, H5T_NATIVE_LONG, attr_long_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_long_in[i] != attr_long_out[i]) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_uchar test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_uchar");

    /* Set the attribute */
    if (H5LTset_attribute_uchar(loc_id, obj_name, ATTR6_NAME, attr_uchar_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_uchar test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_uchar");

    /* Get the attribute */
    if (H5LTget_attribute_uchar(loc_id, obj_name, ATTR6_NAME, attr_uchar_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_uchar_in[i] != attr_uchar_out[i]) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR6_NAME, H5T_NATIVE_UCHAR, attr_uchar_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_uchar_in[i] != attr_uchar_out[i]) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_ushort test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_ushort");

    /* Set the attribute */
    if (H5LTset_attribute_ushort(loc_id, obj_name, ATTR7_NAME, attr_ushort_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_ushort test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_ushort");

    /* Get the attribute */
    if (H5LTget_attribute_ushort(loc_id, obj_name, ATTR7_NAME, attr_ushort_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_ushort_in[i] != attr_ushort_out[i]) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR7_NAME, H5T_NATIVE_USHORT, attr_ushort_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_ushort_in[i] != attr_ushort_out[i]) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_int test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_uint");

    /* Set the attribute */
    if (H5LTset_attribute_uint(loc_id, obj_name, ATTR8_NAME, attr_uint_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_int test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_uint");

    /* Get the attribute */
    if (H5LTget_attribute_uint(loc_id, obj_name, ATTR8_NAME, attr_uint_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_uint_in[i] != attr_uint_out[i]) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR8_NAME, H5T_NATIVE_UINT, attr_uint_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_uint_in[i] != attr_uint_out[i]) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_ulong test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_ulong");

    /* Set the attribute */
    if (H5LTset_attribute_ulong(loc_id, obj_name, ATTR9_NAME, attr_ulong_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_long test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_ulong");

    /* Get the attribute */
    if (H5LTget_attribute_ulong(loc_id, obj_name, ATTR9_NAME, attr_ulong_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_ulong_in[i] != attr_ulong_out[i]) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR9_NAME, H5T_NATIVE_ULONG, attr_ulong_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (attr_ulong_in[i] != attr_ulong_out[i]) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_float test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_float");

    /* Set the attribute */
    if (H5LTset_attribute_float(loc_id, obj_name, ATTR10_NAME, attr_float_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_float test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_float");

    /* Get the attribute */
    if (H5LTget_attribute_float(loc_id, obj_name, ATTR10_NAME, attr_float_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (!H5_FLT_ABS_EQUAL(attr_float_in[i], attr_float_out[i])) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR10_NAME, H5T_NATIVE_FLOAT, attr_float_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (!H5_FLT_ABS_EQUAL(attr_float_in[i], attr_float_out[i])) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTset_attribute_double test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTset_attribute_double");

    /* Set the attribute */
    if (H5LTset_attribute_double(loc_id, obj_name, ATTR11_NAME, attr_double_in, (size_t)5) < 0)
        return -1;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_double test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_double");

    /* Get the attribute */
    if (H5LTget_attribute_double(loc_id, obj_name, ATTR11_NAME, attr_double_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (!H5_DBL_ABS_EQUAL(attr_double_in[i], attr_double_out[i])) {
            return -1;
        }
    }

    /* Get the attribute */
    if (H5LTget_attribute(loc_id, obj_name, ATTR11_NAME, H5T_NATIVE_DOUBLE, attr_double_out) < 0)
        return -1;

    for (i = 0; i < 5; i++) {
        if (!H5_DBL_ABS_EQUAL(attr_double_in[i], attr_double_out[i])) {
            return -1;
        }
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_ndims test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_ndims");

    if (H5LTget_attribute_ndims(loc_id, obj_name, ATTR2_NAME, &rank_out) < 0)
        return -1;

    if (rank_out != 1) {
        return -1;
    }

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LTget_attribute_info test
     *-------------------------------------------------------------------------
     */

    HL_TESTING2("H5LTget_attribute_info");

    if (NULL == (dims_out = (hsize_t *)HDmalloc(sizeof(hsize_t) * (size_t)rank_out)))
        return -1;

    if (H5LTget_attribute_info(loc_id, obj_name, ATTR2_NAME, dims_out, &type_class, &type_size) < 0) {
        HDfree(dims_out);
        return -1;
    }

    for (i = 0; i < rank_out; i++) {
        if (dims_out[i] != 5) {
            HDfree(dims_out);
            return -1;
        }
    }

    if (type_class != H5T_INTEGER) {
        HDfree(dims_out);
        return -1;
    }
    HDfree(dims_out);

    PASSED();

    return 0;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_integers().
 *-------------------------------------------------------------------------
 */
static int
test_integers(void)
{
    hid_t  dtype;
    char * dt_str;
    size_t str_len;

    HL_TESTING3("\n        text for integer types");

    if ((dtype = H5LTtext_to_dtype("H5T_NATIVE_INT\n", H5LT_DDL)) < 0)
        goto out;
    if (!H5Tequal(dtype, H5T_NATIVE_INT))
        goto out;
    if (H5Tclose(dtype) < 0)
        goto out;

    if ((dtype = H5LTtext_to_dtype("H5T_STD_I8BE\n", H5LT_DDL)) < 0)
        goto out;
    if (!H5Tequal(dtype, H5T_STD_I8BE))
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;

    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(dt_str, "H5T_STD_I8BE") != 0) {
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    if ((dtype = H5LTtext_to_dtype("H5T_STD_U16LE\n", H5LT_DDL)) < 0)
        goto out;
    if (!H5Tequal(dtype, H5T_STD_U16LE))
        goto out;
    if (H5Tclose(dtype) < 0)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_fps().
 *-------------------------------------------------------------------------
 */
static int
test_fps(void)
{
    hid_t  dtype;
    char * dt_str;
    size_t str_len;

    HL_TESTING3("        text for floating-point types");

    if ((dtype = H5LTtext_to_dtype("H5T_NATIVE_LDOUBLE\n", H5LT_DDL)) < 0)
        goto out;
    if (!H5Tequal(dtype, H5T_NATIVE_LDOUBLE))
        goto out;
    if (H5Tclose(dtype) < 0)
        goto out;

    if ((dtype = H5LTtext_to_dtype("H5T_IEEE_F32BE\n", H5LT_DDL)) < 0)
        goto out;
    if (!H5Tequal(dtype, H5T_IEEE_F32BE))
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;

    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(dt_str, "H5T_IEEE_F32BE") != 0) {
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    if ((dtype = H5LTtext_to_dtype("H5T_IEEE_F64LE\n", H5LT_DDL)) < 0)
        goto out;
    if (!H5Tequal(dtype, H5T_IEEE_F64LE))
        goto out;
    if (H5Tclose(dtype) < 0)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_strings().
 *-------------------------------------------------------------------------
 */
static int
test_strings(void)
{
    hid_t       dtype;
    size_t      str_size;
    H5T_str_t   str_pad;
    H5T_cset_t  str_cset;
    H5T_class_t type_class;
    char *      dt_str = NULL;
    size_t      str_len;

    HL_TESTING3("        text for string types");

    if ((dtype = H5LTtext_to_dtype(
             "H5T_STRING { STRSIZE 13; STRPAD H5T_STR_NULLTERM; CSET H5T_CSET_ASCII; CTYPE H5T_C_S1; }",
             H5LT_DDL)) < 0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_STRING)
        goto out;

    str_size = H5Tget_size(dtype);
    if (str_size != 13)
        goto out;

    str_pad = H5Tget_strpad(dtype);
    if (str_pad != H5T_STR_NULLTERM)
        goto out;

    str_cset = H5Tget_cset(dtype);
    if (str_cset != H5T_CSET_ASCII)
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(dt_str, "H5T_STRING {\n      STRSIZE 13;\n      STRPAD H5T_STR_NULLTERM;\n      CSET "
                         "H5T_CSET_ASCII;\n      CTYPE H5T_C_S1;\n   }") != 0) {
        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    if ((dtype = H5LTtext_to_dtype("H5T_STRING { STRSIZE H5T_VARIABLE; STRPAD H5T_STR_NULLPAD; CSET "
                                   "H5T_CSET_ASCII; CTYPE H5T_C_S1; }",
                                   H5LT_DDL)) < 0)
        goto out;

    if (!H5Tis_variable_str(dtype))
        goto out;

    str_pad = H5Tget_strpad(dtype);
    if (str_pad != H5T_STR_NULLPAD)
        goto out;

    str_cset = H5Tget_cset(dtype);
    if (str_cset != H5T_CSET_ASCII)
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(dt_str, "H5T_STRING {\n      STRSIZE H5T_VARIABLE;\n      STRPAD H5T_STR_NULLPAD;\n      "
                         "CSET H5T_CSET_ASCII;\n      CTYPE H5T_C_S1;\n   }") != 0) {
        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    /* Length of the character buffer is larger then needed */
    str_len = str_len + 10;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;

    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrncmp(dt_str,
                  "H5T_STRING {\n      STRSIZE H5T_VARIABLE;\n      STRPAD H5T_STR_NULLPAD;\n      CSET "
                  "H5T_CSET_ASCII;\n      CTYPE H5T_C_S1;\n   }",
                  str_len - 1) != 0) {
        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    /* Length of the character buffer is smaller then needed */
    str_len = 21;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;

    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    /* check the truncated string */
    if (HDstrlen(dt_str) != str_len - 1)
        goto out;
    str_len = HDstrlen(dt_str);
    if (HDstrncmp(dt_str,
                  "H5T_STRING {\n      STRSIZE H5T_VARIABLE;\n      STRPAD H5T_STR_NULLPAD;\n      CSET "
                  "H5T_CSET_ASCII;\n      CTYPE H5T_C_S1;\n   }",
                  str_len) != 0) {
        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }

    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    PASSED();
    return 0;

out:
    if (dt_str)
        HDfree(dt_str);

    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_opaques().
 *-------------------------------------------------------------------------
 */
static int
test_opaques(void)
{
    hid_t       dtype;
    size_t      opq_size;
    H5T_class_t type_class;
    char *      dt_str;
    size_t      str_len;

    HL_TESTING3("        text for opaque types");

    if ((dtype = H5LTtext_to_dtype("H5T_OPAQUE { OPQ_SIZE 19; OPQ_TAG \"This is a tag for opaque type\"; }",
                                   H5LT_DDL)) < 0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_OPAQUE)
        goto out;

    if ((opq_size = H5Tget_size(dtype)) == 0)
        goto out;
    if (opq_size != 19)
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(
            dt_str,
            "H5T_OPAQUE {\n      OPQ_SIZE 19;\n      OPQ_TAG \"This is a tag for opaque type\";\n   }") !=
        0) {
        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_enums().
 *-------------------------------------------------------------------------
 */
static int
test_enums(void)
{
    hid_t       dtype;
    size_t      size = 16;
    char        name1[16];
    int         value1 = 7;
    const char *name2  = "WHITE";
    int         value2;
    H5T_class_t type_class;
    char *      dt_str;
    size_t      str_len;

    HL_TESTING3("        text for enum types");

    if ((dtype = H5LTtext_to_dtype(
             "H5T_ENUM { H5T_STD_I32LE; \"RED\" 5; \"GREEN\" 6; \"BLUE\" 7; \"WHITE\" 8; }", H5LT_DDL)) < 0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_ENUM)
        goto out;

    /* Convert the variable before using it */
    if (!H5Tequal(H5T_STD_I32LE, H5T_NATIVE_INT)) {
        if (H5Tconvert(H5T_NATIVE_INT, H5T_STD_I32LE, 1, &value1, NULL, H5P_DEFAULT) < 0)
            goto out;
    }

    if (H5Tenum_nameof(dtype, &value1, name1, size) < 0)
        goto out;
    if (HDstrcmp(name1, "BLUE") != 0)
        goto out;

    if (H5Tenum_valueof(dtype, name2, &value2) < 0)
        goto out;

    /* Convert the variable before comparing it */
    if (!H5Tequal(H5T_STD_I32LE, H5T_NATIVE_INT)) {
        if (H5Tconvert(H5T_NATIVE_INT, H5T_STD_I32LE, 1, &value2, NULL, H5P_DEFAULT) < 0)
            goto out;
    }

    if (value2 != 8)
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(dt_str,
                 "H5T_ENUM {\n      H5T_STD_I32LE;\n      \"RED\"              5;\n      \"GREEN\"   "
                 "         6;\n      \"BLUE\"             7;\n      \"WHITE\"            8;\n   }") != 0) {

        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }

    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_variables().
 *-------------------------------------------------------------------------
 */
static int
test_variables(void)
{
    hid_t       dtype;
    H5T_class_t type_class;
    char *      dt_str;
    size_t      str_len;

    HL_TESTING3("        text for variable types");

    if ((dtype = H5LTtext_to_dtype("H5T_VLEN { H5T_NATIVE_CHAR }\n", H5LT_DDL)) < 0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_VLEN)
        goto out;

    if (H5Tis_variable_str(dtype))
        goto out;

    if (H5Tclose(dtype) < 0)
        goto out;

    if ((dtype = H5LTtext_to_dtype("H5T_VLEN { H5T_VLEN { H5T_STD_I32BE } }", H5LT_DDL)) < 0)
        goto out;

    if (H5Tis_variable_str(dtype))
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(dt_str, "H5T_VLEN {\n      H5T_VLEN {\n         H5T_STD_I32BE\n      }\n   }") != 0) {
        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_arrays().
 *-------------------------------------------------------------------------
 */
static int
test_arrays(void)
{
    hid_t       dtype;
    int         ndims;
    hsize_t     dims[3];
    H5T_class_t type_class;
    char *      dt_str;
    size_t      str_len;

    HL_TESTING3("        text for array types");

    if ((dtype = H5LTtext_to_dtype("H5T_ARRAY { [5][7][13] H5T_ARRAY { [17][19] H5T_COMPOUND { H5T_STD_I8BE "
                                   "\"arr_compound_1\"; H5T_STD_I32BE \"arr_compound_2\"; } } }",
                                   H5LT_DDL)) < 0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_ARRAY)
        goto out;

    if ((ndims = H5Tget_array_ndims(dtype)) < 0)
        goto out;
    if (ndims != 3)
        goto out;

    if (H5Tget_array_dims2(dtype, dims) < 0)
        goto out;
    if (dims[0] != 5 || dims[1] != 7 || dims[2] != 13)
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(dt_str, "H5T_ARRAY {\n      [5][7][13] H5T_ARRAY {\n         [17][19] H5T_COMPOUND {\n      "
                         "      H5T_STD_I8BE \"arr_compound_1\" : 0;\n            H5T_STD_I32BE "
                         "\"arr_compound_2\" : 1;\n         }\n      }\n   }") != 0) {
        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }

    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_compounds().
 *-------------------------------------------------------------------------
 */
static int
test_compounds(void)
{
    hid_t       dtype;
    int         nmembs;
    char *      memb_name = NULL;
    H5T_class_t memb_class;
    H5T_class_t type_class;
    char *      dt_str;
    size_t      str_len;

    HL_TESTING3("        text for compound types");

    if ((dtype = H5LTtext_to_dtype(
             "H5T_COMPOUND { H5T_STD_I16BE \"one_field\" : 2; H5T_STD_U8LE \"two_field\" : 6; }", H5LT_DDL)) <
        0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_COMPOUND)
        goto out;

    if ((nmembs = H5Tget_nmembers(dtype)) < 0)
        goto out;
    if (nmembs != 2)
        goto out;

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;
    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    if (HDstrcmp(dt_str, "H5T_COMPOUND {\n      H5T_STD_I16BE \"one_field\" : 2;\n      H5T_STD_U8LE "
                         "\"two_field\" : 6;\n   }") != 0) {
        HDprintf("dt=\n%s\n", dt_str);
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    if ((dtype = H5LTtext_to_dtype(
             "H5T_COMPOUND { H5T_STD_I32BE \"i32_field\"; H5T_STD_I16BE \"i16_field\"; H5T_COMPOUND  { "
             "H5T_STD_I16BE \"sec_field\"; H5T_COMPOUND { H5T_STD_I32BE \"thd_field\"; } \"grandchild\"; } "
             "\"child_compound\"; H5T_STD_I8BE  \"i8_field\"; }",
             H5LT_DDL)) < 0)
        goto out;

    if ((memb_name = H5Tget_member_name(dtype, 1)) == NULL)
        goto out;
    if (HDstrcmp(memb_name, "i16_field") != 0) {
        H5free_memory(memb_name);
        goto out;
    }
    H5free_memory(memb_name);

    if ((memb_class = H5Tget_member_class(dtype, 2)) < 0)
        goto out;
    if (memb_class != H5T_COMPOUND)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_compound_bug(). Test case for
 * issue 7701.
 *-------------------------------------------------------------------------
 */
static int
test_compound_bug(void)
{
    hid_t       dtype;
    H5T_class_t type_class;
    int         nmembs;
    char *      memb_name = NULL;
    char *      dt_str;
    size_t      str_len;
    char        text[] = "H5T_COMPOUND { H5T_STD_I32LE "
                  "\"state_________________________________________________________________________________"
                  "\"; H5T_STD_I32LE "
                  "\"desc____________________________________________________________________________________"
                  "_____\"; H5T_VLEN { H5T_COMPOUND { H5T_ENUM { H5T_STD_I16LE; \"ZERO\" 0; \"ONE\" 1; "
                  "\"TWO\" 2;  \"THREE\" 3; } \"type____\"; H5T_STD_I32LE "
                  "\"sub_____________________________________________________________________________________"
                  "__________________________\"; H5T_STRING { STRSIZE H5T_VARIABLE; STRPAD H5T_STR_SPACEPAD; "
                  "CSET H5T_CSET_ASCII; CTYPE H5T_C_S1; } \"sub_desc\"; H5T_STD_I32LE "
                  "\"final___________________________________________________________________________________"
                  "________________\"; } } \"sub\"; }";
    char text2[] = "H5T_COMPOUND {\n"
                   "  H5T_STD_I16LE \"state___________________________"
                   "__________________________________________________"
                   "____\" : 0;\n"
                   "  H5T_STD_I16LE \"desc____________________________"
                   "__________________________________________________"
                   "___________\" : 2;\n"
                   "  H5T_VLEN { H5T_COMPOUND {\n"
                   "    H5T_ENUM { H5T_STD_I16LE; \"ZERO\" 0; \"ONE\" "
                   "1; \"TWO\" 2;  \"THREE\" 3; } \"type____\" : 0;\n"
                   "    H5T_STD_I32LE \"sub___________________________"
                   "__________________________________________________"
                   "__________________________________1\" : 4;\n"
                   "    H5T_STRING { STRSIZE H5T_VARIABLE; STRPAD H5T_"
                   "STR_SPACEPAD; CSET H5T_CSET_ASCII; CTYPE H5T_C_S1;"
                   " } \"sub_desc\" : 8;\n"
                   "    H5T_STD_I32LE \"final_________________________"
                   "__________________________________________________"
                   "________________________\" : 16;\n"
                   "  } } \"sub\" : 8;\n"
                   "}\n";

    HL_TESTING3("        text for compound type of bug fix");

    if ((dtype = H5LTtext_to_dtype(text, H5LT_DDL)) < 0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_COMPOUND)
        goto out;

    if ((memb_name = H5Tget_member_name(dtype, 2)) == NULL)
        goto out;
    if (HDstrcmp(memb_name, "sub") != 0) {
        H5free_memory(memb_name);
        goto out;
    }
    H5free_memory(memb_name);

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;

    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }
    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    /* Test similar datatype in another format */
    if ((dtype = H5LTtext_to_dtype(text2, H5LT_DDL)) < 0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_COMPOUND)
        goto out;

    if ((nmembs = H5Tget_nmembers(dtype)) < 0)
        goto out;
    if (nmembs != 3)
        goto out;

    if ((memb_name = H5Tget_member_name(dtype, 1)) == NULL)
        goto out;
    if (HDstrcmp(memb_name, "desc____________________________________________________________________________"
                            "_____________") != 0) {
        H5free_memory(memb_name);
        goto out;
    }
    H5free_memory(memb_name);

    if (H5LTdtype_to_text(dtype, NULL, H5LT_DDL, &str_len) < 0)
        goto out;

    if (NULL == (dt_str = (char *)HDcalloc(str_len, sizeof(char))))
        goto out;
    if (H5LTdtype_to_text(dtype, dt_str, H5LT_DDL, &str_len) < 0) {
        HDfree(dt_str);
        goto out;
    }

    HDfree(dt_str);

    if (H5Tclose(dtype) < 0)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * subroutine for test_text_dtype(): test_complicated_compound().
 *-------------------------------------------------------------------------
 */
static int
test_complicated_compound(void)
{
    hid_t       dtype;
    int         nmembs;
    H5T_class_t type_class;
    char *      line     = NULL;
    FILE *      fp       = NULL;
    size_t      size     = 1024;
    const char *filename = H5_get_srcdir_filename(INPUT_FILE);

    HL_TESTING3("        text for complicated compound types");

    /* Open input file */
    fp = HDfopen(filename, "r");
    if (fp == NULL) {
        HDprintf("Could not find file %s. Try set $srcdir \n", filename);
        goto out;
    }

    /* This part reads in the input as a string in a slow manner.  GNU C
     * Library has convenient function getline() but isn't available on
     * all machines.
     */
    if ((line = (char *)HDcalloc(size, sizeof(char))) == NULL)
        goto out;
    if (HDfgets(line, (int)size, fp) == NULL)
        goto out;
    while (HDstrlen(line) == size - 1) {
        size *= 2;
        if (line)
            HDfree(line);
        if ((line = (char *)HDcalloc(size, sizeof(char))) == NULL)
            goto out;
        if (HDfseek(fp, 0L, SEEK_SET) != 0)
            goto out;
        if (HDfgets(line, (int)size, fp) == NULL)
            goto out;
    }

    HDfclose(fp);
    fp = NULL;

    if ((dtype = H5LTtext_to_dtype(line, H5LT_DDL)) < 0)
        goto out;

    if ((type_class = H5Tget_class(dtype)) < 0)
        goto out;
    if (type_class != H5T_COMPOUND)
        goto out;

    /* There should be 101 compound members */
    if ((nmembs = H5Tget_nmembers(dtype)) < 0)
        goto out;
    if (nmembs != 101)
        goto out;

    if (line)
        HDfree(line);

    PASSED();
    return 0;

out:

    if (line)
        HDfree(line);
    if (fp)
        HDfclose(fp);

    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * test H5LTtext_to_dtype function
 *-------------------------------------------------------------------------
 */
static int
test_text_dtype(void)
{
    HL_TESTING2("H5LTtext_to_dtype");

    if (test_integers() < 0)
        goto out;

    if (test_fps() < 0)
        goto out;

    if (test_strings() < 0)
        goto out;

    if (test_opaques() < 0)
        goto out;

    if (test_enums() < 0)
        goto out;

    if (test_variables() < 0)
        goto out;

    if (test_arrays() < 0)
        goto out;

    if (test_compounds() < 0)
        goto out;

    if (test_compound_bug() < 0)
        goto out;

    if (test_complicated_compound() < 0)
        goto out;

    return 0;

out:
    return -1;
}

/*-------------------------------------------------------------------------
 * test H5LTpath_valid function
 *-------------------------------------------------------------------------
 */
static int
test_valid_path(void)
{
    hid_t       file_id, group;
    htri_t      path_valid;
    const char *data_string_in = "test";

    HL_TESTING2("H5LTpath_valid");

    /* Create a new file using default properties. */

    /**************************************************************
     *  The file structure should look like this:
     *
     *        +----------------------------------+
     *        |                 /                |
     *        +----------------------------------+
     *                  /       |   \       \
     *                 /        |    \       \
     *                /         |     \       \
     *               /          |      \       G8 (dangled external link)
     *              /          DS       \
     *             /                     \
     *            G1                    G2
     *            | --> DS1              |
     *           / \--> DS3             / \
     *          /                      /   \
     *        G2                     DS4    G7
     *         |                 (hard link   (dangled soft link
     *         |                 to /G1/DS3)  to /G1/G20 )
     *         |
     *         |
     *         | --- Gcyc (soft link to /G1)
     *        /  \
     *       /    \
     *     G5      \
     *  (soft link  G6 (external link /G1 in FILENAME4)
     *  to /G2)
     *
     ****************************************************************/

    file_id = H5Fcreate(FILE_NAME3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataset "/DS"
     */
    if (H5LTmake_dataset_string(file_id, "DS", data_string_in) < 0)
        goto out;

    /*
     * Create an external dangled link
     */
    if (H5Lcreate_external("NonExistant_File.h5", "G8", file_id, "DangledExternalLink", H5P_DEFAULT,
                           H5P_DEFAULT) < 0)
        goto out;

    /*
     * Create a group named "G2" in the file.
     */
    if ((group = H5Gcreate2(file_id, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /*
     * Create a dataset named "G2/DS4" in the file.
     */
    if (H5LTmake_dataset_string(group, "/G2/DS4", data_string_in) < 0)
        goto out;

    /*
     * Create a soft link
     */
    if (H5Lcreate_soft("/G1/G20", file_id, "/G2/G7", H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto out;

    if (H5Gclose(group) < 0)
        goto out;

    /*
     * Create a group named "G1" in the file.
     */
    if ((group = H5Gcreate2(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /*
     * Create a group named "G1/DS1" in the file.
     */
    if (H5LTmake_dataset_string(group, "/G1/DS1", data_string_in) < 0)
        goto out;

    if (H5Gclose(group) < 0)
        goto out;

    /*
     * Create a hard link
     */
    if (H5Lcreate_hard(file_id, "/G2/DS4", file_id, "/G1/DS3", H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto out;
    /*
     * Create a group named "/G1/G2" in the file.
     */
    if ((group = H5Gcreate2(file_id, "/G1/G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /*
     * Create a soft link
     */
    if (H5Lcreate_soft("/G2", file_id, "/G1/G2/G5", H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto out;

    /*
     * Create a cyclic soft link
     */
    if (H5Lcreate_soft("/G1", file_id, "/G1/G2/Gcyc", H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto out;

    if (H5Gclose(group) < 0)
        goto out;

    /*
     * Create a group named "/G1/G2/G6" in the file.
     */
    if ((group = H5Gcreate2(file_id, "/G1/G2/G6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /*
     * Create an external link
     */
    if (H5Lcreate_external(FILE_NAME4, "G1", group, "ExternalLink", H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto out;

    if (H5Gclose(group) < 0)
        goto out;
    /*
     * Close the file.
     */
    if (H5Fclose(file_id) < 0)
        goto out;

    /* Create another file for checking external links */

    /**************************************************************
     *  The file structure should look like this:
     *
     *               +----+
     *               |  / |
     *               +----+
     *                 |
     *                 |
     *                 |
     *                 G1
     *                /  \
     *               /    \
     *            DS1      G2
     *                    (dangled soft link to /G1/G20)
     *
     ****************************************************************/

    /* Make external link file */
    file_id = H5Fcreate(FILE_NAME4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a group named "G1" in the file.
     */
    if ((group = H5Gcreate2(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    /*
     * Create a dataset named "G1/DS1" in the file.
     */
    if (H5LTmake_dataset_string(group, "/G1/DS1", data_string_in) < 0)
        goto out;

    /*
     * Create a dangling soft link
     */

    if (H5Lcreate_soft("/G1/G2", file_id, "/G1/G20", H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto out;

    if (H5Gclose(group) < 0)
        goto out;

    H5Fclose(file_id);

    /* Open input file */
    if ((file_id = H5Fopen(FILE_NAME3, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        goto out;

    /**************************************
     * CHECK ABSOLUTE PATHS
     **************************************/

    if ((path_valid = H5LTpath_valid(file_id, "/", TRUE)) != TRUE) {
        goto out;
    }

    if ((path_valid = H5LTpath_valid(file_id, "/", FALSE)) != TRUE) {
        goto out;
    }

    if ((path_valid = H5LTpath_valid(file_id, "/G1", TRUE)) != TRUE) {
        goto out;
    }

    if ((path_valid = H5LTpath_valid(file_id, "/G1/DS1", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/DS3", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/G5", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/Gcyc/DS1", FALSE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/Gcyc/DS1", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G2", TRUE)) != TRUE)
        goto out;

    /* check soft link points to a valid object*/
    if ((path_valid = H5LTpath_valid(file_id, "/G2/DS4", TRUE)) != TRUE)
        goto out;

    /* check if path exist, but not the object */
    if ((path_valid = H5LTpath_valid(file_id, "/G2/G7", FALSE)) != TRUE)
        goto out;
    /* check if path exist and if the object exists. It should fail
     * since it is a dangling soft link
     */
    if ((path_valid = H5LTpath_valid(file_id, "/G2/G7", TRUE)) == TRUE)
        goto out;

    /* check soft links */
    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/G5/DS4", TRUE)) != TRUE)
        goto out;

    /**************************************
     * CHECK RELATIVE PATHS
     ***************************************/

    if ((group = H5Gopen2(file_id, "/", H5P_DEFAULT)) < 0)
        goto out;

    if ((path_valid = H5LTpath_valid(group, "/", TRUE)) != TRUE) {
        goto out;
    }

    if ((path_valid = H5LTpath_valid(group, "/", FALSE)) != TRUE) {
        goto out;
    }

    if (H5Gclose(group) < 0)
        goto out;

    if ((group = H5Gopen2(file_id, "/G1", H5P_DEFAULT)) < 0)
        goto out;

    /* The identifier (file id) is the object itself, i.e. "." */

    if ((path_valid = H5LTpath_valid(file_id, ".", FALSE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, ".", TRUE)) != TRUE)
        goto out;

    /* The identifier (group id) is the object itself, i.e. "." */

    if ((path_valid = H5LTpath_valid(group, ".", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(group, "DS3", FALSE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(group, "DS3", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(group, "G2/G5", TRUE)) != TRUE)
        goto out;

    /* Check the "./" case */
    if ((path_valid = H5LTpath_valid(group, "./DS3", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(group, "./G2/G5", TRUE)) != TRUE)
        goto out;

    /* Should fail, does not exist */
    if ((path_valid = H5LTpath_valid(group, "./G2/G20", FALSE)) == TRUE)
        goto out;

    /* Should fail, does not exist */
    if ((path_valid = H5LTpath_valid(group, "./G2/G20", TRUE)) == TRUE)
        goto out;

    if (H5Gclose(group) < 0)
        goto out;

    /*****************************
     * Check external links
     *****************************/

    /* The dangled external link path is valid */
    if ((path_valid = H5LTpath_valid(file_id, "/DangledExternalLink", FALSE)) != TRUE)
        goto out;

    /* The file however does not exists, so the link dangles -> should return false */
    if ((path_valid = H5LTpath_valid(file_id, "/DangledExternalLink", TRUE)) == TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/G6/ExternalLink", FALSE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/G6/ExternalLink", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/Gcyc/G2/G6/ExternalLink/DS1", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/Gcyc/G2/G6/ExternalLink/G20", FALSE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/G6/ExternalLink/DS1", TRUE)) != TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/G6/ExternalLink/G20", FALSE)) != TRUE)
        goto out;

    /* Should fail, does not exist */
    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/G6/ExternalLink/G20", TRUE)) == TRUE)
        goto out;

    if ((path_valid = H5LTpath_valid(file_id, "/G1/G2/Gcyc/G2/G6/ExternalLink/G20", TRUE)) == TRUE)
        goto out;

    if (H5Fclose(file_id) < 0)
        goto out;

    PASSED();
    return 0;

out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
 * the main program
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    /* test dataset functions */
    nerrors += test_dsets();

    /* test attribute functions */
    nerrors += test_attr();

    /* test valid path functions */
    nerrors += test_valid_path();

    /* test text-dtype functions */
    nerrors += test_text_dtype();

    /* check for errors */
    if (nerrors)
        goto error;

    return 0;

error:
    return 1;
}
