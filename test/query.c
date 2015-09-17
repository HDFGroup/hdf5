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

const char *FILENAME[] = {
    "query",
    NULL
};

#define NTUPLES 256
#define NCOMPONENTS 1

static hid_t
test_query_create(void)
{
    int min = 17;
    int max = 22;
    double value1 = 21.2f;
    int value2 = 25;
    hid_t q1 = H5I_BADID, q2 = H5I_BADID, q3 = H5I_BADID, q4 = H5I_BADID;
    hid_t q5 = H5I_BADID, q6 = H5I_BADID, q7 = H5I_BADID;

    /* Create and combine a bunch of queries
     * Query is: ((17 < x < 22) AND (x != 21.2)) OR (x == 25)
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

    return q7;

error:
    H5E_BEGIN_TRY {
        H5Qclose(q1);
        H5Qclose(q2);
        H5Qclose(q3);
        H5Qclose(q4);
        H5Qclose(q5);
        H5Qclose(q6);
        H5Qclose(q7);
    } H5E_END_TRY;
    return -1;
}

static hid_t
test_query_create2(void)
{
    int min = 17;
    int max = 22;
    const char *link_name = "Pressure";
    const char *attr_name = "SensorID";
    int attr_value = 2;
    hid_t q1 = H5I_BADID, q2 = H5I_BADID, q3 = H5I_BADID, q4 = H5I_BADID;
    hid_t q5 = H5I_BADID, q6 = H5I_BADID, q7 = H5I_BADID, q8 = H5I_BADID;
    hid_t q9 = H5I_BADID;

    /* Create and combine a bunch of queries
     * Query is: ((17 < x < 22) AND (link="Pressure") AND (attr="SensorID") AND (attr=2)
     */
    if ((q1 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_INT, &min)) < 0) FAIL_STACK_ERROR;
    if ((q2 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_LESS_THAN, H5T_NATIVE_INT,
            &max)) < 0) FAIL_STACK_ERROR;
    if ((q3 = H5Qcombine(q1, H5Q_COMBINE_AND, q2)) < 0) FAIL_STACK_ERROR;

    if ((q4 = H5Qcreate(H5Q_TYPE_ATTR_NAME, H5Q_MATCH_EQUAL, attr_name)) < 0) FAIL_STACK_ERROR;
    if ((q5 = H5Qcreate(H5Q_TYPE_ATTR_VALUE, H5Q_MATCH_EQUAL, H5T_NATIVE_INT, &attr_value)) < 0) FAIL_STACK_ERROR;
    if ((q6 = H5Qcombine(q4, H5Q_COMBINE_AND, q5)) < 0) FAIL_STACK_ERROR;

    if ((q7 = H5Qcreate(H5Q_TYPE_LINK_NAME, H5Q_MATCH_EQUAL, link_name)) < 0) FAIL_STACK_ERROR;
    if ((q8 = H5Qcombine(q6, H5Q_COMBINE_AND, q7)) < 0) FAIL_STACK_ERROR;

    if ((q9 = H5Qcombine(q3, H5Q_COMBINE_AND, q8)) < 0) FAIL_STACK_ERROR;

    return q9;

error:
    H5E_BEGIN_TRY {
        H5Qclose(q1);
        H5Qclose(q2);
        H5Qclose(q3);
        H5Qclose(q4);
        H5Qclose(q5);
        H5Qclose(q6);
        H5Qclose(q7);
        H5Qclose(q8);
        H5Qclose(q9);
    } H5E_END_TRY;
    return -1;
}

static herr_t
test_query_close(hid_t query)
{
    H5Q_combine_op_t op_type;

    H5Qget_combine_op(query, &op_type);
    if (op_type == H5Q_SINGLETON) {
        if (H5Qclose(query) < 0) FAIL_STACK_ERROR;
    } else {
        hid_t sub_query1, sub_query2;

        if (H5Qget_components(query, &sub_query1, &sub_query2) < 0)
            FAIL_STACK_ERROR;

        if (test_query_close(sub_query1) < 0) FAIL_STACK_ERROR;
        if (test_query_close(sub_query2) < 0) FAIL_STACK_ERROR;
    }

    return 0;

error:
    H5E_BEGIN_TRY {
        /* Nothing */
    } H5E_END_TRY;
    return -1;
}

static herr_t
test_query_apply_elem(hid_t query, hbool_t *result, hid_t type_id, const void *value)
{
    H5Q_combine_op_t op_type;

    H5Qget_combine_op(query, &op_type);
    if (op_type == H5Q_SINGLETON) {
        H5Q_type_t query_type;

        if (H5Qget_type(query, &query_type) < 0) FAIL_STACK_ERROR;

        /* Query type should be H5Q_TYPE_DATA_ELEM */
        if (query_type != H5Q_TYPE_DATA_ELEM) FAIL_STACK_ERROR;
        if (H5Qapply_atom(query, result, type_id, value) < 0) FAIL_STACK_ERROR;
    } else {
        hbool_t sub_result1, sub_result2;
        hid_t sub_query1_id, sub_query2_id;

        if (H5Qget_components(query, &sub_query1_id, &sub_query2_id) < 0)
            FAIL_STACK_ERROR;
        if (test_query_apply_elem(sub_query1_id, &sub_result1, type_id, value) < 0)
            FAIL_STACK_ERROR;
        if (test_query_apply_elem(sub_query2_id, &sub_result2, type_id, value) < 0)
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
test_query_apply(hid_t query)
{
    int data_elem1 = 15, data_elem2 = 20, data_elem3 = 25;
    double data_elem4 = 21.2f;
    float data_elem5 = 17.2f;
    double data_elem6 = 18.0f;
    double data_elem7 = 2.4f;
    double data_elem8 = 25.0f;
    hbool_t result = 0;

    if (test_query_apply_elem(query, &result, H5T_NATIVE_INT, &data_elem1) < 0)
        FAIL_STACK_ERROR;
    if (result) FAIL_STACK_ERROR;

    if (test_query_apply_elem(query, &result, H5T_NATIVE_INT, &data_elem2) < 0)
        FAIL_STACK_ERROR;
    if (!result) FAIL_STACK_ERROR;

    if (test_query_apply_elem(query, &result, H5T_NATIVE_INT, &data_elem3) < 0)
        FAIL_STACK_ERROR;
    if (!result) FAIL_STACK_ERROR;

    if (test_query_apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem4) < 0)
        FAIL_STACK_ERROR;
    if (result) FAIL_STACK_ERROR;

    if (test_query_apply_elem(query, &result, H5T_NATIVE_FLOAT, &data_elem5) < 0)
        FAIL_STACK_ERROR;
    if (!result) FAIL_STACK_ERROR;

    if (test_query_apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem6) < 0)
        FAIL_STACK_ERROR;
    if (!result) FAIL_STACK_ERROR;

    if (test_query_apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem7) < 0)
        FAIL_STACK_ERROR;
    if (result) FAIL_STACK_ERROR;

    if (test_query_apply_elem(query, &result, H5T_NATIVE_DOUBLE, &data_elem8) < 0)
        FAIL_STACK_ERROR;
    if (!result) FAIL_STACK_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
        /* Nothing */
    } H5E_END_TRY;
    return -1;
}

static herr_t
test_query_encode(hid_t query)
{
    hid_t dec_query = H5I_BADID;
    char *buf = NULL;
    size_t nalloc = 0;

    if (H5Qencode(query, NULL, &nalloc) < 0) FAIL_STACK_ERROR;
    buf = (char *) malloc(nalloc);
    if (H5Qencode(query, buf, &nalloc) < 0) FAIL_STACK_ERROR;
    if ((dec_query = H5Qdecode(buf)) < 0) FAIL_STACK_ERROR;

    /* Check/apply decoded query */
    if (test_query_apply(dec_query) < 0) FAIL_STACK_ERROR;

    if (H5Qclose(dec_query) < 0) FAIL_STACK_ERROR;
    HDfree(buf);

    return 0;

error:
    H5E_BEGIN_TRY {
        if (dec_query != H5I_BADID) H5Qclose(dec_query);
        HDfree(buf);
    } H5E_END_TRY;
    return -1;
}

static herr_t
test_query_create_simple_file(const char *filename, hid_t fapl, unsigned idx_plugin)
{
    hid_t file = H5I_BADID, obj1 = H5I_BADID, obj2 = H5I_BADID, obj3 = H5I_BADID;
    hid_t pres1 = H5I_BADID, pres2 = H5I_BADID, pres3 = H5I_BADID;
    hid_t temp1 = H5I_BADID, temp2 = H5I_BADID, temp3 = H5I_BADID;
    hid_t id_pres1 = H5I_BADID, id_pres2 = H5I_BADID, id_pres3 = H5I_BADID;
    hid_t id_temp1 = H5I_BADID, id_temp2 = H5I_BADID, id_temp3 = H5I_BADID;
    hid_t aspace = H5I_BADID, dspace = H5I_BADID;
    hid_t dcpl = H5P_DEFAULT;
    hsize_t adim[1] = {1};
    hsize_t dims[2] = {NTUPLES, NCOMPONENTS};
    int rank = (NCOMPONENTS == 1) ? 1 : 2;
    int i, j;
    float *data = NULL;
    int id1_val = 1, id2_val = 2, id3_val = 3;

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create simple group and dataset */
    if ((obj1 = H5Gcreate(file, "Object1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((obj2 = H5Gcreate(file, "Object2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((obj3 = H5Gcreate(file, "Object3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialize the data. */
    data = (float *) HDmalloc(sizeof(float) * NCOMPONENTS * NTUPLES);
    for (i = 0; i < NTUPLES; i++) {
        for (j = 0; j < NCOMPONENTS; j++) {
            data[NCOMPONENTS * i + j] = (float) i;
        }
    }

    /* Create dataspace for datasets */
    if ((dspace = H5Screate_simple(rank, dims, NULL)) < 0) FAIL_STACK_ERROR;

    /* Create some datasets and use index if told to */
    if (idx_plugin && ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)) FAIL_STACK_ERROR;
    if (idx_plugin && (H5Pset_index_plugin(dcpl, idx_plugin)) < 0) FAIL_STACK_ERROR;
    if ((pres1 = H5Dcreate(obj1, "Pressure", H5T_NATIVE_FLOAT, dspace, H5P_DEFAULT,
            dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;
    if ((temp1 = H5Dcreate(obj1, "Temperature", H5T_NATIVE_FLOAT, dspace, H5P_DEFAULT,
            dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;
    if ((pres2 = H5Dcreate(obj2, "Pressure", H5T_NATIVE_FLOAT, dspace, H5P_DEFAULT,
            dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;
    if ((temp2 = H5Dcreate(obj2, "Temperature", H5T_NATIVE_FLOAT, dspace, H5P_DEFAULT,
            dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;
    if ((pres3 = H5Dcreate(obj3, "Pressure", H5T_NATIVE_FLOAT, dspace, H5P_DEFAULT,
            dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;
    if ((temp3 = H5Dcreate(obj3, "Temperature", H5T_NATIVE_FLOAT, dspace, H5P_DEFAULT,
            dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;
    if (idx_plugin && (H5Pclose(dcpl) < 0)) FAIL_STACK_ERROR;

    /* Add attributes to datasets */
    if ((aspace = H5Screate_simple(1, adim, NULL)) < 0) FAIL_STACK_ERROR;
    if ((id_pres1 = H5Acreate(pres1, "SensorID", H5T_NATIVE_INT, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((id_temp1 = H5Acreate(temp1, "SensorID", H5T_NATIVE_INT, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((id_pres2 = H5Acreate(pres2, "SensorID", H5T_NATIVE_INT, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((id_temp2 = H5Acreate(temp2, "SensorID", H5T_NATIVE_INT, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((id_pres3 = H5Acreate(pres3, "SensorID", H5T_NATIVE_INT, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((id_temp3 = H5Acreate(temp3, "SensorID", H5T_NATIVE_INT, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    if (H5Awrite(id_pres1, H5T_NATIVE_INT, &id1_val) < 0) FAIL_STACK_ERROR;
    if (H5Awrite(id_temp1, H5T_NATIVE_INT, &id1_val) < 0) FAIL_STACK_ERROR;
    if (H5Awrite(id_pres2, H5T_NATIVE_INT, &id2_val) < 0) FAIL_STACK_ERROR;
    if (H5Awrite(id_temp2, H5T_NATIVE_INT, &id2_val) < 0) FAIL_STACK_ERROR;
    if (H5Awrite(id_pres3, H5T_NATIVE_INT, &id3_val) < 0) FAIL_STACK_ERROR;
    if (H5Awrite(id_temp3, H5T_NATIVE_INT, &id3_val) < 0) FAIL_STACK_ERROR;

    if (H5Aclose(id_pres1) < 0) FAIL_STACK_ERROR;
    if (H5Aclose(id_temp1) < 0) FAIL_STACK_ERROR;
    if (H5Aclose(id_pres2) < 0) FAIL_STACK_ERROR;
    if (H5Aclose(id_temp2) < 0) FAIL_STACK_ERROR;
    if (H5Aclose(id_pres3) < 0) FAIL_STACK_ERROR;
    if (H5Aclose(id_temp3) < 0) FAIL_STACK_ERROR;
    if (H5Sclose(aspace) < 0) FAIL_STACK_ERROR;

    /* Write data */
    if (H5Dwrite(pres1, H5T_NATIVE_FLOAT, H5S_ALL, dspace, H5P_DEFAULT, data) < 0)
        FAIL_STACK_ERROR;
    if (H5Dwrite(temp1, H5T_NATIVE_FLOAT, H5S_ALL, dspace, H5P_DEFAULT, data) < 0)
        FAIL_STACK_ERROR;
    if (H5Dwrite(pres2, H5T_NATIVE_FLOAT, H5S_ALL, dspace, H5P_DEFAULT, data) < 0)
        FAIL_STACK_ERROR;
    if (H5Dwrite(temp2, H5T_NATIVE_FLOAT, H5S_ALL, dspace, H5P_DEFAULT, data) < 0)
        FAIL_STACK_ERROR;
    if (H5Dwrite(pres3, H5T_NATIVE_FLOAT, H5S_ALL, dspace, H5P_DEFAULT, data) < 0)
        FAIL_STACK_ERROR;
    if (H5Dwrite(temp3, H5T_NATIVE_FLOAT, H5S_ALL, dspace, H5P_DEFAULT, data) < 0)
        FAIL_STACK_ERROR;

    if (H5Dclose(pres1) < 0) FAIL_STACK_ERROR;
    if (H5Dclose(temp1) < 0) FAIL_STACK_ERROR;
    if (H5Dclose(pres2) < 0) FAIL_STACK_ERROR;
    if (H5Dclose(temp2) < 0) FAIL_STACK_ERROR;
    if (H5Dclose(pres3) < 0) FAIL_STACK_ERROR;
    if (H5Dclose(temp3) < 0) FAIL_STACK_ERROR;
    if (H5Sclose(dspace) < 0) FAIL_STACK_ERROR;
    HDfree(data);

    if (H5Gclose(obj1) < 0) FAIL_STACK_ERROR;
    if (H5Gclose(obj2) < 0) FAIL_STACK_ERROR;
    if (H5Gclose(obj3) < 0) FAIL_STACK_ERROR;

    if (H5Fclose(file) < 0) FAIL_STACK_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(pres1);
        H5Dclose(temp1);
        H5Dclose(pres2);
        H5Dclose(temp2);
        H5Dclose(pres3);
        H5Dclose(temp3);
        H5Sclose(dspace);
        HDfree(data);
        H5Aclose(id_pres1);
        H5Aclose(id_temp1);
        H5Aclose(id_pres2);
        H5Aclose(id_temp2);
        H5Aclose(id_pres3);
        H5Aclose(id_temp3);
        H5Sclose(aspace);
        H5Gclose(obj1);
        H5Gclose(obj2);
        H5Gclose(obj3);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}

static herr_t
test_query_read_region(hid_t view)
{
    hid_t refs = H5I_BADID, ref_type = H5I_BADID, ref_space = H5I_BADID;
    size_t n_refs, ref_size, ref_buf_size;
    void *ref_buf= NULL;
    hid_t dset = H5I_BADID, type = H5I_BADID, space = H5I_BADID, mem_space = H5I_BADID;
    size_t n_elem, elem_size, buf_size;
    float *buf = NULL;
    char dset_path[64];
    unsigned int i;

    if ((refs = H5Dopen(view, H5Q_VIEW_REF_REG_NAME, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;
    if ((ref_type = H5Dget_type(refs)) < 0) FAIL_STACK_ERROR;
    if ((ref_space = H5Dget_space(refs)) < 0) FAIL_STACK_ERROR;
    if (0 == (n_refs = (size_t) H5Sget_select_npoints(ref_space))) FAIL_STACK_ERROR;
    if (0 == (ref_size = H5Tget_size(ref_type))) FAIL_STACK_ERROR;

    /* Allocate buffer to hold data */
    ref_buf_size = n_refs * ref_size;
    if (NULL == (ref_buf = HDmalloc(ref_buf_size))) FAIL_STACK_ERROR;

    if ((H5Dread(refs, ref_type, H5S_ALL, ref_space, H5P_DEFAULT, ref_buf)) < 0) FAIL_STACK_ERROR;

    if (H5Sclose(ref_space) < 0) FAIL_STACK_ERROR;
    if (H5Tclose(ref_type) < 0) FAIL_STACK_ERROR;
    if (H5Dclose(refs) < 0) FAIL_STACK_ERROR;

    if ((dset = H5Rdereference(view, H5P_DEFAULT, H5R_DATASET_REGION, ref_buf)) < 0) FAIL_STACK_ERROR;
    if ((space = H5Rget_region(view, H5R_DATASET_REGION, ref_buf)) < 0) FAIL_STACK_ERROR;
    if ((type = H5Dget_type(dset)) < 0) FAIL_STACK_ERROR;

    HDfree(ref_buf);
    ref_buf = NULL;

    if (0 == (n_elem = (size_t) H5Sget_select_npoints(space))) FAIL_STACK_ERROR;
    if (0 == (elem_size = H5Tget_size(type))) FAIL_STACK_ERROR;

    /* Get name of dataset */
    if (H5Iget_name(dset, dset_path, 64) < 0) FAIL_STACK_ERROR;
    printf("Dataset name: %s\n", dset_path);
    printf("Region has %zu elements of size %zu\n", n_elem, elem_size);

    /* Allocate buffer to hold data */
    buf_size = n_elem * elem_size;
    if (NULL == (buf = (float *) HDmalloc(buf_size))) FAIL_STACK_ERROR;

    if ((mem_space = H5Screate_simple(1, (hsize_t *) &n_elem, NULL)) < 0) FAIL_STACK_ERROR;

    if ((H5Dread(dset, type, mem_space, space, H5P_DEFAULT, buf)) < 0) FAIL_STACK_ERROR;

    printf("Elements that match the query are:\n");
    for (i = 0; i < n_elem; i++)
        printf("%f ", buf[i]);

    if (H5Sclose(mem_space) < 0) FAIL_STACK_ERROR;
    if (H5Dclose(dset) < 0) FAIL_STACK_ERROR;
    if (H5Sclose(space) < 0) FAIL_STACK_ERROR;
    if (H5Tclose(type) < 0) FAIL_STACK_ERROR;
    HDfree(buf);
    buf = NULL;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(mem_space);
        H5Dclose(dset);
        H5Sclose(space);
        H5Tclose(type);
        HDfree(buf);
        H5Dclose(refs);
        H5Sclose(ref_space);
        H5Tclose(ref_type);
        HDfree(ref_buf);
    } H5E_END_TRY;

    return -1;
}

static herr_t
test_query_apply_view(const char *filename, hid_t fapl)
{
    hid_t file = H5I_BADID;
    hid_t view = H5I_BADID;
    hid_t query = H5I_BADID;
    unsigned result = 0;

    if ((test_query_create_simple_file(filename, fapl, H5X_PLUGIN_NONE)) < 0) FAIL_STACK_ERROR;

    if ((query = test_query_create2()) < 0) FAIL_STACK_ERROR;

    /* TODO test works with H5F_ACC_RDONLY */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR;

    printf(" ...\n---\n");

    if ((view = H5Qapply(file, query, &result, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;

    if (result & H5Q_REF_ATTR) FAIL_STACK_ERROR;
    if (result & H5Q_REF_OBJ) FAIL_STACK_ERROR;
    if (result & H5Q_REF_REG)
        if (test_query_read_region(view) < 0) FAIL_STACK_ERROR;

    printf("\n---\n...");

    if (H5Gclose(view) < 0) FAIL_STACK_ERROR;
    if (H5Fclose(file) < 0) FAIL_STACK_ERROR;
    if (test_query_close(query)) FAIL_STACK_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Gclose(view);
        H5Fclose(file);
        test_query_close(query);
    } H5E_END_TRY;
    return -1;
}

int
main(int argc, char **argv)
{
    char filename[1024]; /* file name */
    hid_t query = H5I_BADID, fapl = H5I_BADID;

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));
    /* Check that no object is left open */
    H5Pset_fclose_degree(fapl, H5F_CLOSE_SEMI);

    TESTING("query create and combine");

    if ((query = test_query_create()) < 0) FAIL_STACK_ERROR;

    PASSED();

    TESTING("query apply");

    if (test_query_apply(query) < 0) FAIL_STACK_ERROR;

    PASSED();

    TESTING("query encode/decode");

    if (test_query_encode(query) < 0) FAIL_STACK_ERROR;

    PASSED();

    TESTING("query close");

    if (test_query_close(query) < 0) FAIL_STACK_ERROR;

    PASSED();

    TESTING("query apply view");

    if (test_query_apply_view(filename, fapl) < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Verify symbol table messages are cached */
    if(h5_verify_cached_stabs(FILENAME, fapl) < 0) TEST_ERROR

    puts("All query tests passed.");
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        test_query_close(query);
    } H5E_END_TRY;

    return 1;
}
