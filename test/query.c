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

/* Programmer:  Jerome Soumagne <jsoumagne@hdfgroup.org>
 *              Wednesday, Sep 24, 2014
 */

#include "h5test.h"

static herr_t
apply_elem(hid_t query, hbool_t *result, hid_t type_id, const void *value)
{
    H5Q_combine_op_t op_type;

    H5Qget_combine_op(query, &op_type);
    if (op_type == H5Q_SINGLETON) {
        H5Q_type_t query_type;

        if (H5Qget_type(query, &query_type) < 0) FAIL_STACK_ERROR;

        /* Query type should be H5Q_TYPE_DATA_ELEM */
        if (query_type != H5Q_TYPE_DATA_ELEM) FAIL_STACK_ERROR;
        if (H5Qapply(query, result, type_id, value) < 0) FAIL_STACK_ERROR;
    } else {
        hbool_t sub_result1, sub_result2;
        hid_t sub_query1_id, sub_query2_id;

        if (H5Qget_components(query, &sub_query1_id, &sub_query2_id) < 0)
            FAIL_STACK_ERROR;
        if (apply_elem(sub_query1_id, &sub_result1, type_id, value) < 0)
            FAIL_STACK_ERROR;
        if (apply_elem(sub_query2_id, &sub_result2, type_id, value) < 0)
            FAIL_STACK_ERROR;

        *result = (op_type == H5Q_COMBINE_AND) ? sub_result1 && sub_result2 :
                sub_result1 || sub_result2;

        if (H5Qclose(sub_query1_id) < 0) FAIL_STACK_ERROR;
        if (H5Qclose(sub_query2_id) < 0) FAIL_STACK_ERROR;
    }

    return 0;

error:
    H5E_BEGIN_TRY {
        /* Nothing */
    } H5E_END_TRY;
    return -1;
}

static herr_t
apply(hid_t query)
{
    int data_elem1 = 15, data_elem2 = 20, data_elem3 = 25;
    double data_elem4 = 21.2;
    float data_elem5 = 17.2f;
    double data_elem6 = 18.0;
    double data_elem7 = 2.4;
    double data_elem8 = 25.0;
    hbool_t result = 0;

    if (apply_elem(query, &result, H5T_NATIVE_INT, &data_elem1) < 0)
        FAIL_STACK_ERROR;
    if (!result) {
//        printf("Data element (%d) does not match query\n", data_elem1);
    } else {
        FAIL_STACK_ERROR;
    }

    if (apply_elem(query, &result, H5T_NATIVE_INT, &data_elem2) < 0)
        FAIL_STACK_ERROR;
    if (result) {
//        printf("Data element (%d) matches query\n", data_elem2);
    } else {
        FAIL_STACK_ERROR;
    }

    if (apply_elem(query, &result, H5T_NATIVE_INT, &data_elem3) < 0)
        FAIL_STACK_ERROR;
    if (result) {
//        printf("Data element (%d) matches query\n", data_elem3);
    } else {
        FAIL_STACK_ERROR;
    }

    if (apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem4) < 0)
        FAIL_STACK_ERROR;
    if (!result) {
//        printf("Data element (%lf) does not match query\n", data_elem4);
    } else {
        FAIL_STACK_ERROR;
    }

    if (apply_elem(query, &result, H5T_NATIVE_FLOAT, &data_elem5) < 0)
        FAIL_STACK_ERROR;
    if (result) {
//        printf("Data element (%f) matches query\n", data_elem5);
    } else {
        FAIL_STACK_ERROR;
    }

    if (apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem6) < 0)
        FAIL_STACK_ERROR;
    if (result) {
//        printf("Data element (%f) matches query\n", data_elem6);
    } else {
        FAIL_STACK_ERROR;
    }

    if (apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem7) < 0)
        FAIL_STACK_ERROR;
    if (!result) {
//        printf("Data element (%f) does not match query\n", data_elem7);
    } else {
        FAIL_STACK_ERROR;
    }

    if (apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem8) < 0)
        FAIL_STACK_ERROR;
    if (result) {
//        printf("Data element (%f) matches query\n", data_elem8);
    } else {
        FAIL_STACK_ERROR;
    }

    return 0;

error:
    H5E_BEGIN_TRY {
        /* Nothing */
    } H5E_END_TRY;
    return -1;
}

int
main(int argc, char **argv)
{
    int min = 17;
    int max = 22;
    double value1 = 21.2;
    int value2 = 25;
    hid_t q1, q2, q3, q4, q5, q6, q7, q8;
    char *buf = NULL;
    size_t nalloc = 0;

    (void) argc;
    (void) argv;

    /* Reset library */
    h5_reset();

    TESTING("query create and combine");

    /* Create and combine a bunch of queries
     * Query is: ((17 < x < 22) && (x != 21.2)) || (x == 25)
     */
    if ((q1 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_INT, &min)) < 0) FAIL_STACK_ERROR;
    if ((q2 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_LESS_THAN, H5T_NATIVE_INT,
            &max)) < 0) FAIL_STACK_ERROR;
    if ((q3 = H5Qcombine(q1, H5Q_COMBINE_AND, q2)) < 0) FAIL_STACK_ERROR;
    if ((q4 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_NOT_EQUAL,
            H5T_NATIVE_DOUBLE, &value1)) < 0) FAIL_STACK_ERROR;
    if ((q5 = H5Qcombine(q3, H5Q_COMBINE_AND, q4)) < 0) FAIL_STACK_ERROR;
    if ((q6 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_EQUAL, H5T_NATIVE_INT,
            &value2)) < 0) FAIL_STACK_ERROR;
    if ((q7 = H5Qcombine(q5, H5Q_COMBINE_OR, q6)) < 0) FAIL_STACK_ERROR;

    PASSED();

    TESTING("query apply");

    /* Apply queries */
    if (apply(q7) < 0) FAIL_STACK_ERROR;

    PASSED();

    TESTING("query encode/decode");

    if (H5Qencode(q7, NULL, &nalloc) < 0) FAIL_STACK_ERROR;
    buf = (char *) malloc(nalloc);
    if (H5Qencode(q7, buf, &nalloc) < 0) FAIL_STACK_ERROR;
    if ((q8 = H5Qdecode(buf)) < 0) FAIL_STACK_ERROR;

    /* Check/apply decoded query */
    if (apply(q8) < 0) FAIL_STACK_ERROR;

    PASSED();

    if (H5Qclose(q8) < 0) goto error;
    if (H5Qclose(q1) < 0) goto error;
    if (H5Qclose(q2) < 0) goto error;
    if (H5Qclose(q3) < 0) goto error;
    if (H5Qclose(q4) < 0) goto error;
    if (H5Qclose(q5) < 0) goto error;
    if (H5Qclose(q6) < 0) goto error;
    if (H5Qclose(q7) < 0) goto error;

    HDfree(buf);

    puts("All query tests passed.");

    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5Qclose(q8);
        H5Qclose(q1);
        H5Qclose(q2);
        H5Qclose(q3);
        H5Qclose(q4);
        H5Qclose(q5);
        H5Qclose(q6);
        H5Qclose(q7);
        HDfree(buf);
    } H5E_END_TRY;

    return 1;
}
