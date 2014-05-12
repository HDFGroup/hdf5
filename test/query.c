/* 
 * h5ff_query.c: Test H5Q extensions.
 */

#include "hdf5.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

static void
apply_elem(hid_t query, hbool_t *result, hid_t type_id, const void *value)
{
    H5Q_combine_op_t op_type;

    H5Qget_combine_op(query, &op_type);
    if (op_type == H5Q_SINGLETON) {
        H5Q_type_t query_type;

        H5Qget_match_info(query, &query_type, NULL);
        if (query_type != H5Q_TYPE_DATA_ELEM)
            fprintf(stderr, "Error, wrong query type\n");
        H5Qapply(query, result, type_id, value);
    } else {
        hbool_t sub_result1, sub_result2;
        hid_t sub_query1_id, sub_query2_id;

        H5Qget_components(query, &sub_query1_id, &sub_query2_id);
        apply_elem(sub_query1_id, &sub_result1, type_id, value);
        apply_elem(sub_query2_id, &sub_result2, type_id, value);

        *result = (op_type == H5Q_COMBINE_AND) ? sub_result1 && sub_result2 :
                sub_result1 || sub_result2;

        H5Qclose(sub_query1_id);
        H5Qclose(sub_query2_id);
    }
}

static int
apply(hid_t query)
{
    int data_elem1 = 15, data_elem2 = 20, data_elem3 = 25;
    double data_elem4 = 21.2;
    float data_elem5 = 17.2f;
    double data_elem6 = 18.0;
    double data_elem7 = 2.4;
    double data_elem8 = 25.0;
    hbool_t result = 0;
    int ret = EXIT_SUCCESS;

    apply_elem(query, &result, H5T_NATIVE_INT, &data_elem1);
    if (!result) {
        printf("Data element (%d) does not match query\n", data_elem1);
    } else {
        ret = EXIT_FAILURE;
        goto done;
    }

    apply_elem(query, &result, H5T_NATIVE_INT, &data_elem2);
    if (result) {
        printf("Data element (%d) matches query\n", data_elem2);
    } else {
        ret = EXIT_FAILURE;
        goto done;
    }

    apply_elem(query, &result, H5T_NATIVE_INT, &data_elem3);
    if (result) {
        printf("Data element (%d) matches query\n", data_elem3);
    } else {
        ret = EXIT_FAILURE;
        goto done;
    }

    apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem4);
    if (!result) {
        printf("Data element (%lf) does not match query\n", data_elem4);
    } else {
        ret = EXIT_FAILURE;
        goto done;
    }

    apply_elem(query, &result, H5T_NATIVE_FLOAT, &data_elem5);
    if (result) {
        printf("Data element (%f) matches query\n", data_elem5);
    } else {
        ret = EXIT_FAILURE;
        goto done;
    }

    apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem6);
    if (result) {
        printf("Data element (%f) matches query\n", data_elem6);
    } else {
        ret = EXIT_FAILURE;
        goto done;
    }

    apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem7);
    if (!result) {
        printf("Data element (%f) does not match query\n", data_elem7);
    } else {
        ret = EXIT_FAILURE;
        goto done;
    }

    apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem8);
    if (result) {
        printf("Data element (%f) matches query\n", data_elem8);
    } else {
        ret = EXIT_FAILURE;
        goto done;
    }

done:
    return ret;
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
    int ret = EXIT_SUCCESS;

    /* Create and combine a bunch of queries */
    q1 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN, H5T_NATIVE_INT, &min);
    q2 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_LESS_THAN, H5T_NATIVE_INT, &max);
    q3 = H5Qcombine(q1, H5Q_COMBINE_AND, q2);
    q4 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_NOT_EQUAL, H5T_NATIVE_DOUBLE, &value1);
    q5 = H5Qcombine(q3, H5Q_COMBINE_AND, q4);
    q6 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_EQUAL, H5T_NATIVE_INT, &value2);
    q7 = H5Qcombine(q5, H5Q_COMBINE_OR, q6);

    printf("Query is: ((17 < x < 22) && (x != 21.2)) || (x == 25)\n");

    /* Apply queries */
    ret = apply(q7);
    if (ret == EXIT_SUCCESS) {
        printf("------------------------------------------\n");
        printf("Query apply tests passed on original query\n");
        printf("------------------------------------------\n");
    } else {
        goto done;
    }

    H5Qencode(q7, NULL, &nalloc);
    buf = (char *) malloc(nalloc);
    H5Qencode(q7, buf, &nalloc);
    q8 = H5Qdecode(buf);

    /* Apply queries */
    apply(q8);
    if (ret == EXIT_SUCCESS) {
        printf("------------------------------------------\n");
        printf("Query apply tests passed on decoded query\n");
        printf("------------------------------------------\n");
    } else {
        goto done;
    }

done:
    free(buf);

    H5Qclose(q8);
    H5Qclose(q1);
    H5Qclose(q2);
    H5Qclose(q3);
    H5Qclose(q4);
    H5Qclose(q5);
    H5Qclose(q6);
    H5Qclose(q7);

    return ret;
}
