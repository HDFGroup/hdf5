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

/*
 * Purpose:	Routines to support Analysis Shipping.
 */

#include "H5VLiod_server.h"
#include "H5Qpublic.h"
#include "H5RCpublic.h"
#include "H5Vpublic.h"

#ifdef H5_HAVE_PYTHON
#include <Python.h>
#include <numpy/ndarrayobject.h>
#endif

#ifdef H5_HAVE_EFF

/* do not change order */
typedef struct {
    analysis_farm_out_t farm_out;
    void *data;
} H5VLiod_farm_data_t;

#ifdef H5_HAVE_PYTHON

pthread_mutex_t h5python_mutex;
static hbool_t numpy_initialized = FALSE;

static void H5VL__iod_numpy_init(void);

static PyObject *H5VL__iod_load_script(const char *script, const char *func_name);

static herr_t H5VL__iod_translate_h5_type(hid_t data_type_id,
        enum NPY_TYPES *npy_type);

static herr_t H5VL__iod_translate_numpy_type(enum NPY_TYPES npy_type,
        hid_t *data_type_id, size_t *data_type_size);

static PyObject *H5VL__iod_create_numpy_array(size_t num_elmts, hid_t data_type_id,
        void *data);

static herr_t H5VL__iod_extract_numpy_array(PyObject *numpy_array, void **data,
        size_t *num_elmts, hid_t *data_type_id);

/* TODO define as static but leave it like this for testing */
herr_t H5VL__iod_split(const char *split_script, void *data,
                       size_t num_elmts, hid_t data_type_id, void **split_data,
                       size_t *split_num_elmts, hid_t *split_data_type_id);

herr_t H5VL__iod_combine(const char *combine_script, 
                         void **split_data, size_t *split_num_elmts,
                         size_t num_targets, hid_t data_type_id,
                         void **combine_data, size_t *combine_num_elmts,
                         hid_t *combine_data_type_id);
#endif

static herr_t H5VL__iod_request_container_open(const char *file_name, iod_handle_t **cohs);
static herr_t H5VL__iod_request_container_close(iod_handle_t *cohs);

static herr_t H5VL__iod_farm_work(iod_obj_map_t *obj_map, iod_handle_t *cohs,
                                  hid_t dset_id, hid_t region, iod_trans_id_t rtid,
                                  const char *split_script, const char *combine_script,
                                  void **combine_data, size_t *combine_num_elmts, 
                                  hid_t *combine_type_id);

static herr_t H5VL__iod_farm_split(iod_handle_t coh, iod_obj_id_t obj_id, iod_trans_id_t rtid, 
                                   hid_t space_id, iod_size_t num_cells, hid_t type_id, 
                                   const char *split_script, void **split_data, 
                                   size_t *split_num_elmts, hid_t *split_type_id);

static hid_t H5VL__iod_get_space_layout(coords_t coords, hid_t space_id);

static herr_t H5VL__iod_read_selection(iod_handle_t coh, iod_obj_id_t obj_id, 
                                       iod_trans_id_t rtid, hid_t space_id,
                                       hid_t type_id, void *buf);

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_invoke_cb
 *
 * Purpose:     Invokes analysis on a given container, query, 
 *              and python scripts.
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_analysis_invoke_cb(AXE_engine_t UNUSED axe_engine, 
                                   size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                   size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                   void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    analysis_invoke_in_t *input = (analysis_invoke_in_t *)op_data->input;
    analysis_invoke_out_t output;
    const char *file_name = input->file_name;
    hid_t query_id = input->query_id;
    const char *split_script = input->split_script;
    const char *combine_script = input->combine_script;
    const char *integrate_script = input->integrate_script;
    iod_handle_t *cohs; /* the container handles */
    hid_t file_id = FAIL, rcxt_id = FAIL, fapl_id = FAIL, view_id = FAIL;
    hsize_t attr_count, obj_count, reg_count;
    herr_t ret, ret_value = SUCCEED;
    iod_trans_id_t rtid;

    /* ****************** TEMP THING (as IOD requires collective container open) */
    ret = H5VL__iod_request_container_open(file_name, &cohs);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't request container open");
    /* ***************** END TEMP THING */

    /* get an hid_t from the IOD container handle */
    fapl_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_SELF, MPI_INFO_NULL);
    file_id = H5VLiod_get_file_id(file_name, cohs[0], fapl_id, &rcxt_id);
    if(FAIL == file_id)
        HGOTO_ERROR_FF(FAIL, "cannot open H5 file");
    H5Pclose(fapl_id);

    if(H5RCget_version(rcxt_id, &rtid) < 0)
        HGOTO_ERROR_FF(FAIL, "cannot get container version");

    /* create a view from the query on the entire container
       leveraging any indexing on the datasets */
    view_id = H5Vcreate_ff(file_id, query_id, H5P_DEFAULT, rcxt_id, H5_EVENT_STACK_NULL);
    if(FAIL == view_id)
        HGOTO_ERROR_FF(FAIL, "cannot create view");

    ret = H5Vget_counts(view_id, &attr_count, &obj_count, &reg_count);
    if(ret < 0)
        HGOTO_ERROR_FF(FAIL, "cannot retrieve View component counts");

#if H5_EFF_DEBUG 
    fprintf(stderr, "found %zu region counts\n", (size_t)reg_count);
#endif

    if(reg_count != 0) {
        hid_t *dset_ids = NULL;
        hid_t *region_ids = NULL;
        hsize_t i;
        void **combine_data;
        size_t *combine_num_elmts;
        hid_t *combine_type_id;

        if(NULL == (dset_ids = (hid_t *)malloc (sizeof(hid_t) * reg_count)))
            HGOTO_ERROR_FF(FAIL, "Can't allocate space");
        if(NULL == (region_ids = (hid_t *)malloc (sizeof(hid_t) * reg_count)))
            HGOTO_ERROR_FF(FAIL, "Can't allocate space");
        if(NULL == (combine_type_id = (hid_t *)malloc (sizeof(hid_t) * reg_count)))
            HGOTO_ERROR_FF(FAIL, "Can't allocate space");
        if(NULL == (combine_num_elmts = (size_t *)malloc (sizeof(size_t) * reg_count)))
            HGOTO_ERROR_FF(FAIL, "Can't allocate space");
        if(NULL == (combine_data = (void **)malloc (sizeof(void *) * reg_count)))
            HGOTO_ERROR_FF(FAIL, "Can't allocate space");

        /* get all the datasets and regions from the View */
        ret = H5Vget_elem_regions_ff(view_id, 0, reg_count, dset_ids, 
                                     region_ids, H5_EVENT_STACK_NULL);
        if(ret < 0)
            HGOTO_ERROR_FF(FAIL, "cannot retrieve Datasets and regions");

        /* Farm work for each dataset/region */
        for(i=0 ; i<reg_count ; i++) {
            hid_t dset_id = dset_ids[i];
            hid_t region = region_ids[i];
            iod_obj_map_t *obj_map = NULL;

            /* retrieve the IOD layout */
            if(H5VLiod_query_map(dset_id, rtid, &obj_map) < 0)
                HGOTO_ERROR_FF(FAIL, "can't obtain object map");
#if H5_EFF_DEBUG 
            print_iod_obj_map(obj_map);
#endif
            assert(obj_map->type == IOD_OBJ_ARRAY);

            if(H5VL__iod_farm_work(obj_map, cohs, dset_id, region, rtid,
                                   split_script, combine_script,
                                   &combine_data[i], &combine_num_elmts[i], 
                                   &combine_type_id[i]) < 0)
                HGOTO_ERROR_FF(FAIL, "can't farm work to workers");

            if(H5VLiod_close_map(dset_id, obj_map) < 0)
                HGOTO_ERROR_FF(FAIL, "can't obtain dset identifier");
        }

        /* MSC - apply Integrate script */

        for(i=0 ; i<reg_count ; i++) {
            if(H5Dclose(dset_ids[i]) < 0)
                HGOTO_ERROR_FF(FAIL, "cannot close dataset");
            if(H5Sclose(region_ids[i]) < 0)
                HGOTO_ERROR_FF(FAIL, "cannot close region");
            if(combine_data[i]) {
                free(combine_data[i]);
                combine_data[i] = NULL;
            }
        }

        free(dset_ids);
        dset_ids = NULL;
        free(region_ids);
        region_ids = NULL;
        free(combine_type_id);
        combine_type_id= NULL;
        free(combine_num_elmts);
        combine_num_elmts = NULL;
        free(combine_data);
        combine_data = NULL;
    }

    if(H5RCrelease(rcxt_id, H5_EVENT_STACK_NULL) < 0)
        HGOTO_ERROR_FF(FAIL, "cannot release read context");
    if(H5VLiod_close_file_id(file_id) < 0)
        HGOTO_ERROR_FF(FAIL, "cannot release file_id");

    /* ****************** TEMP THING (as IOD requires collective container open) */
    ret = H5VL__iod_request_container_close(cohs);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't request container close");
    /* ***************** END TEMP THING */

done:
    output.ret = ret_value;
    HG_Handler_start_output(op_data->hg_handle, &output);

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (analysis_invoke_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    if(view_id != FAIL && H5Vclose(view_id) < 0)
        HGOTO_ERROR_FF(FAIL, "cannot close view");
    if(rcxt_id != FAIL && H5RCclose(rcxt_id) < 0)
        HGOTO_ERROR_FF(FAIL, "cannot close view");

} /* end H5VL_iod_server_analysis_invoke_cb() */

#ifdef H5_HAVE_PYTHON

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_numpy_init
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static void
H5VL__iod_numpy_init(void)
{
    /* Needed for numpy stuff */
    /* Note: This function must be called in the initialization section of a
     * module that will make use of the C-API. It imports the module where the
     * function-pointer table is stored and points the correct variable to it.
     * If, however, the extension module involves multiple files where the C-API
     * is needed then some additional steps must be taken. */
    import_array();
    numpy_initialized = TRUE;
} /* end H5VL__iod_numpy_init() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_load_script
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static PyObject *
H5VL__iod_load_script(const char *script, const char *func_name)
{
    PyObject *po_func = NULL, *ret_value = NULL; /* Return value */
    PyObject *po_main = NULL, *po_main_dict = NULL, *po_rstring = NULL;
    /* for reference in case we need it
        PyObject *main = PyImport_AddModule("__main__");
        PyObject *dlfl = PyImport_AddModule("dlfl");
        PyObject* main_dict = PyModule_GetDict( main );
        PyObject* dlfl_dict = PyModule_GetDict( dlfl );
    */

    /* Get reference to main */
    if(NULL == (po_main = PyImport_AddModule("__main__"))) {
        fprintf(stderr, "can't get reference to main module\n");
        ret_value = NULL;
        goto done;
    }

    if(NULL == (po_main_dict = PyModule_GetDict(po_main))) {
        fprintf(stderr, "can't get dictionary from main module\n");
        ret_value = NULL;
        goto done;
    }

    /* Load script */
    if(NULL == (po_rstring = PyRun_String(script, Py_file_input, po_main_dict, po_main_dict))) {
        fprintf(stderr, "can't load script into main module\n");
        ret_value = NULL;
        goto done;
    }

    /* Get reference to function name */
    if(NULL == (po_func = PyObject_GetAttrString(po_main, func_name))) {
        fprintf(stderr, "can't get reference to function\n");
        ret_value = NULL;
        goto done;
    }

    ret_value = po_func;

done:
    //Py_DECREF(po_rstring);

    return ret_value;
} /* end H5VL__iod_load_script() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_translate_h5_type
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_translate_h5_type(hid_t data_type_id, enum NPY_TYPES *npy_type)
{
    herr_t ret_value = SUCCEED; /* Return value */
    enum NPY_TYPES translated_type;
    hid_t native_datatype_id;

    if((native_datatype_id = H5Tget_native_type(data_type_id, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR_FF(FAIL, "cannot retrieve native type");

    if(H5Tequal(native_datatype_id, H5T_NATIVE_LLONG_g))
        translated_type = NPY_LONGLONG;
    else if(H5Tequal(native_datatype_id, H5T_NATIVE_LONG_g))
        translated_type = NPY_LONG;
    else if(H5Tequal(native_datatype_id, H5T_NATIVE_INT_g))
        translated_type = NPY_INT;
    else if(H5Tequal(native_datatype_id, H5T_NATIVE_SHORT_g))
        translated_type = NPY_SHORT;
    else if(H5Tequal(native_datatype_id, H5T_NATIVE_SCHAR_g))
        translated_type = NPY_BYTE;
    else if(H5Tequal(native_datatype_id, H5T_NATIVE_DOUBLE_g))
        translated_type = NPY_DOUBLE;
    else if(H5Tequal(native_datatype_id, H5T_NATIVE_FLOAT_g))
        translated_type = NPY_FLOAT;
    else 
        HGOTO_ERROR_FF(FAIL, "not a valid type")

    if(FAIL == H5Tclose(native_datatype_id))
        HGOTO_ERROR_FF(FAIL, "unable to free datatype");

    *npy_type = translated_type;

done:
    return ret_value;
} /* end H5VL__iod_translate_h5_type() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_translate_numpy_type
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_translate_numpy_type(enum NPY_TYPES npy_type, hid_t *data_type_id,
        size_t *data_type_size)
{
    herr_t ret_value = SUCCEED; /* Return value */
    hid_t translated_type_id;
    size_t translated_type_size;

    switch (npy_type) {
        case NPY_LONGLONG:
            translated_type_id = H5T_NATIVE_LLONG_g;
            break;
        case NPY_LONG:
            translated_type_id = H5T_NATIVE_LONG_g;
            break;
        case NPY_INT:
            translated_type_id = H5T_NATIVE_INT_g;
            break;
        case NPY_SHORT:
            translated_type_id = H5T_NATIVE_SHORT_g;
            break;
        case NPY_BYTE:
            translated_type_id = H5T_NATIVE_SCHAR_g;
            break;
        case NPY_DOUBLE:
            translated_type_id = H5T_NATIVE_DOUBLE_g;
            break;
        case NPY_FLOAT:
            translated_type_id = H5T_NATIVE_FLOAT_g;
            break;
        default:
            HGOTO_ERROR_FF(FAIL, "not a valid type")
            break;
    }

    if (0 == (translated_type_size = H5Tget_size(translated_type_id)))
        HGOTO_ERROR_FF(FAIL, "not a valid size")

    *data_type_id = translated_type_id;
    *data_type_size = translated_type_size;

done:
    return ret_value;
} /* end H5VL__iod_translate_numpy_type() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_create_numpy_array
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static PyObject *
H5VL__iod_create_numpy_array(size_t num_elmts, hid_t data_type_id, void *data)
{
    PyObject *ret_value = NULL; /* Return value */
    npy_intp dim[1] = {(npy_intp) num_elmts};
    enum NPY_TYPES npy_type;

    HDassert(data);

    /* Convert data_type to numpy type */
    if(FAIL == H5VL__iod_translate_h5_type(data_type_id, &npy_type)) {
        fprintf(stderr, "unable to translate datatype to NPY type\n");
        ret_value = NULL;
        goto done;
    }

    ret_value = PyArray_SimpleNewFromData(1, dim, npy_type, data);

done:
    return ret_value;
} /* end H5VL__iod_create_numpy_array() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_extract_numpy_array
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_extract_numpy_array(PyObject *numpy_array, void **data,
        size_t *num_elmts, hid_t *data_type_id)
{
    herr_t ret_value = SUCCEED; /* Return value */
    void *numpy_data = NULL, *extracted_data = NULL;
    hid_t extracted_datatype_id;
    npy_intp *numpy_dims;
    int numpy_datatype;
    size_t numpy_num_elmts, extracted_datatype_size;

    HDassert(numpy_array);
    HDassert(data);
    HDassert(num_elmts);
    HDassert(data_type_id);

    /* Input array should be 1D */
    if(PyArray_NDIM(numpy_array) > 1)
        HGOTO_ERROR_FF(FAIL, "was expecting 1-dimensional array")

    if(NULL == (numpy_data = PyArray_DATA(numpy_array)))
        HGOTO_ERROR_FF(FAIL, "can't get data from numpy array")

    if(NULL == (numpy_dims = PyArray_DIMS(numpy_array)))
        HGOTO_ERROR_FF(FAIL, "can't get dimensions from numpy array")
     numpy_num_elmts = (size_t) numpy_dims[0];

    if(0 == (numpy_datatype = PyArray_TYPE(numpy_array)))
        HGOTO_ERROR_FF(FAIL, "can't get type from numpy array")

    /* Translate NPY type to hid_t */
    if(FAIL == H5VL__iod_translate_numpy_type(numpy_datatype, &extracted_datatype_id,
            &extracted_datatype_size))
        HGOTO_ERROR_FF(FAIL, "can't translate NPY type")

    /* Copy data from NPY array */
    if(NULL == (extracted_data = H5MM_malloc(numpy_num_elmts * extracted_datatype_size)))
        HGOTO_ERROR_FF(FAIL, "can't allocate extracted data")
    HDmemcpy(extracted_data, numpy_data, numpy_num_elmts * extracted_datatype_size);

    *data = extracted_data;
    *num_elmts = numpy_num_elmts;
    *data_type_id = extracted_datatype_id;

done:
    return ret_value;
} /* H5VL__iod_extract_numpy_array */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_split
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__iod_split(const char *split_script, void *data, size_t num_elmts,
        hid_t data_type_id, void **split_data, size_t *split_num_elmts,
        hid_t *split_data_type_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    PyObject *po_func = NULL, *po_numpy_array = NULL, *po_args_tup = NULL;
    PyObject *po_numpy_array_split = NULL;

    if(!numpy_initialized) H5VL__iod_numpy_init();

    /* Create numpy array */
    if(NULL == (po_numpy_array = H5VL__iod_create_numpy_array(num_elmts, data_type_id, data)))
        HGOTO_ERROR_FF(FAIL, "can't create numpy array from data")

    /* Load script */
    if(NULL == (po_func = H5VL__iod_load_script(split_script, "split")))
        HGOTO_ERROR_FF(FAIL, "can't load split script")

    /* Check whether split is defined as function in given script */
    if(0 == PyCallable_Check(po_func))
        HGOTO_ERROR_FF(FAIL, "can't find split function")

    /* Get the input to function */
    if(NULL == (po_args_tup = PyTuple_Pack(1, po_numpy_array)))
        HGOTO_ERROR_FF(FAIL, "can't set input to split function")

    /* Invoke the split function */
    if(NULL == (po_numpy_array_split = PyObject_CallObject(po_func, po_args_tup)))
        HGOTO_ERROR_FF(FAIL, "returned NULL python object")

    /* Extract data from numpy array */
    if(FAIL == H5VL__iod_extract_numpy_array(po_numpy_array_split, split_data,
            split_num_elmts, split_data_type_id))
        HGOTO_ERROR_FF(FAIL, "can't extract data from numpy array")

done:
    /* Cleanup */
    //Py_XDECREF(po_func);
    //Py_XDECREF(po_args_tup);
    //Py_XDECREF(po_numpy_array);
    //Py_XDECREF(po_numpy_array_split);

    return ret_value;
} /* end H5VL__iod_split() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_combine
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__iod_combine(const char *combine_script, void **split_data, size_t *split_num_elmts,
        size_t num_targets, hid_t data_type_id, void **combine_data,
        size_t *combine_num_elmts, hid_t *combine_data_type_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    PyObject *po_func = NULL, *po_numpy_arrays = NULL, *po_args_tup = NULL;
    PyObject *po_numpy_array_combine = NULL;
    size_t count = 0;
    size_t i, k = 0;

    if(!numpy_initialized) H5VL__iod_numpy_init();

    for(i = 0; i < num_targets; i++) {
        if(0 != split_num_elmts[i])
            count ++;
    }

    /* Create numpy arrays */
    if(NULL == (po_numpy_arrays = PyList_New((Py_ssize_t) count)))
        HGOTO_ERROR_FF(FAIL, "can't create list of arrays")

    for(i = 0; i < num_targets; i++) {
        PyObject *po_numpy_array = NULL;
        int py_ret = 0;

        if(0 != split_num_elmts[i]) {
            if(NULL == (po_numpy_array = 
                        H5VL__iod_create_numpy_array(split_num_elmts[i],
                                                     data_type_id, 
                                                     split_data[i])))
                HGOTO_ERROR_FF(FAIL, "can't create numpy array from data")

            if(0 != (py_ret = PyList_SetItem(po_numpy_arrays, (Py_ssize_t) k, po_numpy_array)))
                HGOTO_ERROR_FF(FAIL, "can't set item to array list")
            k ++;
        }
    }

    /* Load script */
    if(NULL == (po_func = H5VL__iod_load_script(combine_script, "combine")))
        HGOTO_ERROR_FF(FAIL, "can't load combine script")

    /* Check whether split is defined as function in given script */
    if(0 == PyCallable_Check(po_func))
        HGOTO_ERROR_FF(FAIL, "can't find combine function")

    /* Get the input to function */
    if(NULL == (po_args_tup = PyTuple_Pack(1, po_numpy_arrays)))
        HGOTO_ERROR_FF(FAIL, "can't set input to combine function")

    /* Invoke the split function */
    if(NULL == (po_numpy_array_combine = PyObject_CallObject(po_func, po_args_tup)))
        HGOTO_ERROR_FF(FAIL, "returned NULL python object")

    /* Extract data from numpy array */
    if(FAIL == H5VL__iod_extract_numpy_array(po_numpy_array_combine, combine_data,
            combine_num_elmts, combine_data_type_id))
        HGOTO_ERROR_FF(FAIL, "can't extract data from numpy array")

done:
    /* Cleanup */
    //Py_XDECREF(po_func);
    //Py_XDECREF(po_args_tup);
    for(i = 0; i < count; i++) {
        //Py_XDECREF(po_numpy_arrays + i);
    }
    //Py_XDECREF(po_numpy_array_combine);

    return ret_value;
}

#endif /* H5_HAVE_PYTHON */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_request_container_open
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_request_container_open(const char *file_name, iod_handle_t **cohs)
{
    herr_t ret_value = SUCCEED; /* Return value */
    hg_request_t *hg_reqs = NULL;
    iod_handle_t *temp_cohs = NULL;
    iod_ret_t ret;
    int i;

    if(pthread_mutex_init(&h5python_mutex, NULL)) 
        HGOTO_ERROR_FF(FAIL, "can't init AS mutex");
    
    if(NULL == (hg_reqs = (hg_request_t *)malloc(sizeof(hg_request_t) * (unsigned int) num_ions_g)))
        HGOTO_ERROR_FF(FAIL, "can't allocate HG requests");

    if(NULL == (temp_cohs = (iod_handle_t *)malloc(sizeof(iod_handle_t) * (unsigned int) num_ions_g)))
        HGOTO_ERROR_FF(FAIL, "can't allocate container handles");

    /* forward a call to every ION to open the container */
    for(i = 1; i < num_ions_g; i++) {
        if(HG_Forward(server_addr_g[i], H5VL_EFF_OPEN_CONTAINER, &file_name,
                      &temp_cohs[i], &hg_reqs[i]) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to ship operation");
    }

    /* open the container */
#if H5_EFF_DEBUG 
    fprintf(stderr, "(%d) Calling iod_container_open on %s\n", my_rank_g, file_name);
#endif
    ret = iod_container_open(file_name, NULL, IOD_CONT_RW, &temp_cohs[0], NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open file");

    for(i = 1; i < num_ions_g; i++) {
        if(HG_Wait(hg_reqs[i], HG_MAX_IDLE_TIME, HG_STATUS_IGNORE) < 0)
            HGOTO_ERROR_FF(FAIL, "HG_Wait Failed");

        /* Free Mercury request */
        if(HG_Request_free(hg_reqs[i]) != HG_SUCCESS)
            HGOTO_ERROR_FF(FAIL, "Can't Free Mercury Request");
    }
    free(hg_reqs);

    *cohs = temp_cohs;

done:
    return ret_value;
} /* end H5VL__iod_request_container_open */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_request_container_close
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_request_container_close(iod_handle_t *cohs)
{
    herr_t ret_value = SUCCEED; /* Return value */
    hg_request_t *hg_reqs = NULL;
    iod_ret_t ret;
    int i;

    if(NULL == (hg_reqs = (hg_request_t *)malloc(sizeof(hg_request_t) * (unsigned int) num_ions_g)))
        HGOTO_ERROR_FF(FAIL, "can't allocate HG requests");

    /* forward a call to every ION to close the container */
    for(i = 1; i < num_ions_g; i++) {
        if(HG_Forward(server_addr_g[i], H5VL_EFF_CLOSE_CONTAINER, &cohs[i],
                &ret_value, &hg_reqs[i]) < 0)
            HGOTO_ERROR_FF(FAIL, "failed to ship operation");
    }

    /* close the container */
    ret = iod_container_close(cohs[0], NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close container");

    for(i = 1; i < num_ions_g; i++) {
        if(HG_Wait(hg_reqs[i], HG_MAX_IDLE_TIME, HG_STATUS_IGNORE) < 0)
            HGOTO_ERROR_FF(FAIL, "HG_Wait Failed");

        /* Free Mercury request */
        if(HG_Request_free(hg_reqs[i]) != HG_SUCCESS)
            HGOTO_ERROR_FF(FAIL, "Can't Free Mercury Request");
    }
    free(hg_reqs);
    free(cohs);

    if(pthread_mutex_destroy(&h5python_mutex))
        HGOTO_ERROR_FF(FAIL, "can't destroy AS mutex");
done:
    return ret_value;
} /* end H5VL__iod_request_container_close */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_farm_work
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_farm_work(iod_obj_map_t *obj_map, iod_handle_t *cohs,
                    hid_t dset_id, hid_t region, iod_trans_id_t rtid,
                    const char *split_script, const char *combine_script,
                    void **combine_data, size_t *combine_num_elmts, hid_t *combine_type_id)
{
    herr_t ret_value = SUCCEED; /* Return value */
    void **split_data = NULL;
    size_t *split_num_elmts;
    hid_t split_type_id = FAIL;
    hg_request_t *hg_reqs = NULL;
    uint32_t u;
    unsigned int num_targets = obj_map->u_map.array_map.n_range;
    coords_t coords;
    analysis_farm_in_t farm_input;
    analysis_farm_out_t *farm_output = NULL;

    /* function shipper requests */
    if(NULL == (hg_reqs = (hg_request_t *) malloc(sizeof(hg_request_t) * num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't allocate HG requests");

    if(NULL == (farm_output = (analysis_farm_out_t *) malloc
                (sizeof(analysis_farm_out_t) * num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't allocate HG requests");

    if(NULL == (split_data = (void **) malloc(sizeof(void *) * num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't allocate array for split data");

    if(NULL == (split_num_elmts = (size_t *) malloc(sizeof(size_t) * num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't allocate array for num elmts");

    farm_input.obj_id = obj_map->oid;
    farm_input.rtid = rtid;
    farm_input.type_id = H5Dget_type(dset_id);
    farm_input.split_script = split_script;

    coords.rank = H5Sget_simple_extent_ndims(region);

#if 0
    /* Farm work to the IONs and combine the result */
    for(i=0 ; i<num_ions_g ; i++) {
        hid_t space_id;

        farm_input.num_cells = 0;

        /* Get all ranges that are on Node i into a dataspace */
        for(u = 0; u < obj_map->u_map.array_map.n_range ; u++) {
            uint32_t worker = obj_map->u_map.array_map.array_range[u].nearest_rank;

            if(i == worker) {
                hid_t space_layout;

                coords.start_cell = obj_map->u_map.array_map.array_range[u].start_cell;
                coords.end_cell = obj_map->u_map.array_map.array_range[u].end_cell;

                if(FAIL == (space_layout = H5VL__iod_get_space_layout(coords, region)))
                    HGOTO_ERROR_FF(FAIL, "can't generate local dataspace selection");

                if(0 == farm_input.num_cells) {
                    space_id = H5Scopy(space_layout);
                }
                else {
                    /* MSC - OR the space with the current one */
                }

                farm_input.num_cells += obj_map->u_map.array_map.array_range[u].n_cell;

                if(H5Sclose(space_layout) < 0)
                    HGOTO_ERROR_FF(FAIL, "cannot close region");
            }
        }

        if(farm_input.num_cells) {
            farm_input.space_id = space_id;
            farm_input.coh = cohs[i];

            /* forward the call to the target server */
            if (i == 0) {
                hg_reqs[i] = HG_REQUEST_NULL;
                /* Do a local split */
                if(FAIL == H5VL__iod_farm_split(cohs[0], obj_map->oid, rtid, space_layout,
                                                farm_input.num_cells, farm_input.type_id, split_script,
                                                &split_data[i], &split_num_elmts[i], &split_type_id))
                    HGOTO_ERROR_FF(FAIL, "can't split in farmed job");
            } else {
                if(HG_Forward(server_addr_g[i], H5VL_EFF_ANALYSIS_FARM, &farm_input, 
                              &farm_output[i], &hg_reqs[i]) < 0)
                    HGOTO_ERROR_FF(FAIL, "failed to ship operation");
            }

            if(H5Sclose(space_id) < 0)
                HGOTO_ERROR_FF(FAIL, "cannot close region");
        }
    }
#endif

    /* farm work for a specific range on a specific node */
    for(u = 0; u < obj_map->u_map.array_map.n_range ; u++) {
        hid_t space_layout;
        uint32_t worker = obj_map->u_map.array_map.array_range[u].nearest_rank;

#if H5_EFF_DEBUG 
        fprintf(stderr, "Farming to %d\n", worker);
#endif
        farm_input.num_cells =  obj_map->u_map.array_map.array_range[u].n_cell;
        coords.start_cell = obj_map->u_map.array_map.array_range[u].start_cell;
        coords.end_cell = obj_map->u_map.array_map.array_range[u].end_cell;

        if(FAIL == (space_layout = H5VL__iod_get_space_layout(coords, region)))
            HGOTO_ERROR_FF(FAIL, "can't generate local dataspace selection");

        /* MSC - AND region and space_layout */

        farm_input.space_id = space_layout;
        farm_input.coh = cohs[worker];

        /* forward the call to the target server */
        if (worker == 0) {
            hg_reqs[u] = HG_REQUEST_NULL;
            /* Do a local split */
            if(FAIL == H5VL__iod_farm_split(cohs[0], obj_map->oid, rtid, space_layout,
                                            farm_input.num_cells, farm_input.type_id, split_script,
                                            &split_data[u], &split_num_elmts[u], &split_type_id))
                HGOTO_ERROR_FF(FAIL, "can't split in farmed job");
        } else {
            if(HG_Forward(server_addr_g[worker], H5VL_EFF_ANALYSIS_FARM, &farm_input, 
                          &farm_output[u], &hg_reqs[u]) < 0)
                HGOTO_ERROR_FF(FAIL, "failed to ship operation");
        }

        if(H5Sclose(space_layout) < 0)
            HGOTO_ERROR_FF(FAIL, "cannot close region");
    }

    for(u = 0; u < obj_map->u_map.array_map.n_range ; u++) {
        int worker = obj_map->u_map.array_map.array_range[u].nearest_rank;

        if (hg_reqs[u] == HG_REQUEST_NULL) {
            /* No request / was local */
        } 
        else {
            analysis_transfer_in_t transfer_input;
            analysis_transfer_out_t transfer_output;
            hg_bulk_t bulk_handle;
            size_t split_data_size;

            /* Wait for the farmed work to complete */
            if(HG_Wait(hg_reqs[u], HG_MAX_IDLE_TIME, HG_STATUS_IGNORE) < 0)
                HGOTO_ERROR_FF(FAIL, "HG_Wait Failed");

            if(0 != farm_output[u].num_elmts) {
                /* Get split type ID and num_elemts 
                   (all the arrays should have the same native type id) */
                split_type_id = farm_output[u].type_id;
                split_num_elmts[u] = farm_output[u].num_elmts;
                split_data_size = split_num_elmts[u] * H5Tget_size(split_type_id);

                if(NULL == (split_data[u] = malloc(split_data_size)))
                    HGOTO_ERROR_FF(FAIL, "can't allocate farm buffer");

                HG_Bulk_handle_create(1, &split_data[u], &split_data_size,
                                      HG_BULK_READWRITE, &bulk_handle);

                transfer_input.axe_id = farm_output[u].axe_id;
                transfer_input.bulk_handle = bulk_handle;

                /* forward a free call to the target server */
                if(HG_Forward(server_addr_g[worker], H5VL_EFF_ANALYSIS_FARM_TRANSFER,
                              &transfer_input, &transfer_output, &hg_reqs[u]) < 0)
                    HGOTO_ERROR_FF(FAIL, "failed to ship operation");

                /* Wait for the farmed work to complete */
                if(HG_Wait(hg_reqs[u], HG_MAX_IDLE_TIME, HG_STATUS_IGNORE) < 0)
                    HGOTO_ERROR_FF(FAIL, "HG_Wait Failed");

                /* Free bulk handle */
                HG_Bulk_handle_free(bulk_handle);
            }
            else {
                split_num_elmts[u] = 0;
                split_data[u] = NULL;
            }
            /* Free Mercury request */
            if(HG_Request_free(hg_reqs[u]) != HG_SUCCESS)
                HGOTO_ERROR_FF(FAIL, "Can't Free Mercury Request");
        }
    }

#if H5_EFF_DEBUG 
    fprintf(stderr, "(%d) Applying combine on data\n", my_rank_g);
#endif
#ifdef H5_HAVE_PYTHON
    if (H5VL__iod_combine(combine_script, split_data, split_num_elmts,
            num_targets, split_type_id, combine_data,
            combine_num_elmts, combine_type_id) < 0)
        HGOTO_ERROR_FF(FAIL, "can't combine split data");
#endif

done:

    if(hg_reqs)
        free(hg_reqs);
    if(farm_output)
        free(farm_output);

    /* free farm data */
    if (split_data) {
        for (u = 0; u < num_targets; u++) {
            if(split_data[u]) {
                free(split_data[u]);
                split_data[u] = NULL;
            }
        }
        free(split_data);
        split_data = NULL;
    }
    if(split_num_elmts) {
        free(split_num_elmts);
        split_num_elmts = NULL;
    }

    return ret_value;
} /* end H5VL__iod_farm_work */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_farm_split
 *
 * Purpose:
 *
 * Return:  Success:    SUCCEED
 *      Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_farm_split(iod_handle_t coh, iod_obj_id_t obj_id, iod_trans_id_t rtid, 
                     hid_t space_id, iod_size_t num_cells, hid_t type_id, 
                     const char *split_script, void **split_data, 
                     size_t *split_num_elmts, hid_t *split_type_id)
{
    void *data = NULL;
    size_t elmt_size, buf_size;
    size_t nelmts;
    herr_t ret, ret_value = SUCCEED;

    nelmts = (size_t) H5Sget_select_npoints(space_id);
    elmt_size = H5Tget_size(type_id);
    buf_size = nelmts * elmt_size;

    assert(num_cells == nelmts);

    /* allocate buffer to hold data */
    if(NULL == (data = malloc(buf_size)))
        HGOTO_ERROR_FF(FAIL, "can't allocate read buffer");

    /* read the data local on the ION specified in the layout selection */
    ret = H5VL__iod_read_selection(coh, obj_id, rtid, space_id, type_id, data);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't read local data");


#if H5_EFF_DEBUG
    fprintf(stderr, "(%d) Applying split on data\n", my_rank_g);
#endif

    /* Apply split python script on data from query */
#ifdef H5_HAVE_PYTHON
    if(nelmts) {
        if (pthread_mutex_lock(&h5python_mutex))
            HGOTO_ERROR_FF(FAIL, "can't lock AS mutex");
        if(FAIL == H5VL__iod_split(split_script, data, nelmts, type_id,
                                   split_data, split_num_elmts, split_type_id))
            HGOTO_ERROR_FF(FAIL, "can't apply split script to data");
        if (pthread_mutex_unlock(&h5python_mutex))
            HGOTO_ERROR_FF(FAIL, "can't unlock AS mutex");
    }
#endif

    /* Free the data after split operation */
    if(data) {
        free(data);
        data = NULL;
    }
done:
    return ret_value;
} /* end H5VL__iod_farm_split */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_farm_cb
 *
 * Purpose:	Retrieves layout of object
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_analysis_farm_cb(AXE_engine_t UNUSED axe_engine, 
                                 size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                 size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                 void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    analysis_farm_in_t *input = (analysis_farm_in_t *)op_data->input;
    H5VLiod_farm_data_t *output = NULL;
    iod_handle_t coh = input->coh;
    hid_t space_id = input->space_id;
    hid_t type_id = input->type_id;
    iod_trans_id_t rtid = input->rtid;
    iod_obj_id_t obj_id = input->obj_id; /* The ID of the object */
    const char *split_script = input->split_script;
    iod_size_t num_cells = input->num_cells;
    void *split_data = NULL;
    size_t split_num_elmts = 0;
    hid_t split_type_id = FAIL;
    herr_t ret_value = SUCCEED;

    if(H5VL__iod_farm_split(coh, obj_id, rtid, space_id, num_cells, type_id, split_script,
                            &split_data, &split_num_elmts, &split_type_id) < 0)
        HGOTO_ERROR_FF(FAIL, "can't split in farmed job");

    /* allocate output struct */
    if(NULL == (output = (H5VLiod_farm_data_t *)H5MM_malloc(sizeof(H5VLiod_farm_data_t))))
        HGOTO_ERROR_FF(FAIL, "No Space");

    /* set output, and return to master */
    output->farm_out.ret = ret_value;
    output->farm_out.axe_id = op_data->axe_id;
    output->farm_out.num_elmts = split_num_elmts;
    output->farm_out.type_id = split_type_id;
    output->data = split_data;
    op_data->output = output;

    HG_Handler_start_output(op_data->hg_handle, &output->farm_out);

done:

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);

    if(0 == split_num_elmts) {
        output = (H5VLiod_farm_data_t *)H5MM_xfree(output);
        op_data = (op_data_t *)H5MM_xfree(op_data);
    }

    input = (analysis_farm_in_t *)H5MM_xfree(input);
} /* end H5VL_iod_server_analysis_farm_cb() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_farm_transfer_cb
 *
 * Purpose:	Frees the output from the split operation
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_analysis_transfer_cb(AXE_engine_t axe_engine,
        size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[],
        size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[],
        void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    analysis_transfer_in_t *input = (analysis_transfer_in_t *)op_data->input;
    op_data_t *farm_op_data = NULL;
    void *farm_op_data_ptr = NULL;
    H5VLiod_farm_data_t *farm_output = NULL;
    herr_t ret_value = SUCCEED;
    hg_bulk_t bulk_block_handle;
    size_t data_size;
    hg_bulk_request_t bulk_request;
    na_addr_t source = HG_Handler_get_addr(op_data->hg_handle);

    if(AXE_SUCCEED != AXEget_op_data(axe_engine, input->axe_id, &farm_op_data_ptr))
        HGOTO_ERROR_FF(FAIL, "failed to get farm op_data");

    farm_op_data = (op_data_t *)farm_op_data_ptr;
    farm_output = (H5VLiod_farm_data_t *)farm_op_data->output;

    data_size = HG_Bulk_handle_get_size(input->bulk_handle);
#if H5_EFF_DEBUG 
    fprintf(stderr, "(%d) Transferring split data back to master\n", my_rank_g);
#endif
    /* Create bulk handle */
    if(HG_SUCCESS != HG_Bulk_handle_create(1, &farm_output->data, &data_size, 
                                           HG_BULK_READ_ONLY, &bulk_block_handle))
        HGOTO_ERROR_FF(FAIL, "can't create bulk handle");

    /* Push data to the client */
    if(HG_SUCCESS != HG_Bulk_transfer(HG_BULK_PUSH, source, input->bulk_handle, 0, 
                                      bulk_block_handle, 0, data_size, &bulk_request))
        HGOTO_ERROR_FF(FAIL, "Transfer data failed");

    /* Wait for bulk data read to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
        HGOTO_ERROR_FF(FAIL, "can't wait for bulk data operation");

    /* free the bds block handle */
    if(HG_SUCCESS != HG_Bulk_handle_free(bulk_block_handle))
        HGOTO_ERROR_FF(FAIL, "can't free bds block handle");

    free(farm_output->data);
    farm_op_data = (op_data_t *)H5MM_xfree(farm_op_data);

done:
    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    HG_Handler_free_input(op_data->hg_handle, input);
    HG_Handler_free(op_data->hg_handle);
    input = (analysis_transfer_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);
} /* end H5VL_iod_server_analysis_transfer_cb() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_get_space_layout
 *
 * Purpose:     Generates a dataspace from the IOD layout for a particular
 *              ION or OST. The dataspace returned must be closed with H5Sclose().
 *
 * Return:	Success:	space id
 *		Failure:	FAIL
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL__iod_get_space_layout(coords_t coords, hid_t space_id)
{
    int i;
    hsize_t start[H5S_MAX_RANK];
    hsize_t block[H5S_MAX_RANK];
    hsize_t count[H5S_MAX_RANK];

    hid_t space_layout = FAIL, ret_value = FAIL;

    /* copy the original dataspace and reset selection to NONE */
    if((space_layout = H5Scopy(space_id)) < 0)
        HGOTO_ERROR_FF(FAIL, "unable to copy dataspace")

    for(i=0 ; i<coords.rank ; i++) {
        start[i] = coords.start_cell[i];
        block[i] = (coords.end_cell[i] - coords.start_cell[i]) + 1;
        count[i] = 1;
    }

    if(H5Sselect_hyperslab(space_layout, H5S_SELECT_SET, start, NULL, count, block) < 0)
        HGOTO_ERROR_FF(FAIL, "unable to add point to selection");

    ret_value = space_layout;

done:
    if(ret_value < 0) {
        if(FAIL != space_layout && H5Sclose(space_layout) < 0)
            HDONE_ERROR_FF(FAIL, "unable to release dataspace")
    } /* end if */

    return ret_value;
} /* end H5VL__iod_get_space_layout() */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_read_selection
 *
 * Purpose: 
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_read_selection(iod_handle_t coh, iod_obj_id_t obj_id, 
                         iod_trans_id_t rtid, hid_t space_id,
                         hid_t type_id, void *buf)
{
    iod_handle_t obj_oh;
    size_t buf_size=0;
    size_t elmt_size;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* open the array object */
    ret = iod_obj_open_read(coh, obj_id, rtid, NULL, &obj_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open array object fo read");

    /* read the data selection from IOD. */
    /* MSC - will need to do it in pieces, not it one shot. */
    elmt_size = H5Tget_size(type_id);
    ret = H5VL__iod_server_final_io(obj_oh, space_id, elmt_size, FALSE, 
                                    buf, buf_size, (uint64_t)0, 0, rtid);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't read from array object");

done:
    if(obj_oh.cookie != IOD_OH_UNDEFINED) {
        ret = iod_obj_close(obj_oh, NULL, NULL);
        if(ret < 0)
            HDONE_ERROR_FF(ret, "can't close Array object");
    }
    return ret_value;
} /* end H5VL__iod_read_selection() */

int
H5VL_iod_server_container_open(hg_handle_t handle)
{
    const char *file_name;
    iod_handle_t coh;
    int ret_value = HG_SUCCESS;

    if(pthread_mutex_init(&h5python_mutex, NULL)) 
        HGOTO_ERROR_FF(FAIL, "can't init AS mutex");

    if(HG_FAIL == HG_Handler_get_input(handle, &file_name))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    /* open the container */
    printf("Calling iod_container_open on %s\n", file_name);
    if(iod_container_open(file_name, NULL, IOD_CONT_R, &coh, NULL))
        HGOTO_ERROR_FF(FAIL, "can't open file");

    HG_Handler_start_output(handle, &coh);

done:
    if(ret_value < 0) {
        coh.cookie = IOD_OH_UNDEFINED;
        HG_Handler_start_output(handle, &coh);
    }
    return ret_value;
} /* end H5VL_iod_server_container_open() */

int
H5VL_iod_server_container_close(hg_handle_t handle)
{
    iod_handle_t coh;
    int ret_value = HG_SUCCESS;

    if(HG_FAIL == HG_Handler_get_input(handle, &coh))
	HGOTO_ERROR_FF(FAIL, "can't get input parameters");

    /* open the container */
    if(iod_container_close(coh, NULL, NULL))
        HGOTO_ERROR_FF(FAIL, "can't open file");

    if(pthread_mutex_destroy(&h5python_mutex))
        HGOTO_ERROR_FF(FAIL, "can't destroy AS mutex");
done:
    HG_Handler_start_output(handle, &ret_value);
    return ret_value;
} /* end H5VL_iod_server_container_open() */

#endif /* H5_HAVE_EFF */
