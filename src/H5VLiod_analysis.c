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

static herr_t H5VL__iod_farm_split(iod_handle_t coh, iod_obj_id_t obj_id, iod_trans_id_t rtid, 
                                   hid_t space_id, coords_t coords, iod_size_t num_cells,
                                   hid_t type_id, hid_t query_id, 
                                   const char *split_script, void **split_data, 
                                   size_t *split_num_elmts, hid_t *split_type_id);

static hid_t H5VL__iod_get_space_layout(coords_t coords, iod_size_t num_cells, hid_t space_id);

static herr_t H5VL__iod_get_query_data(iod_handle_t coh, iod_obj_id_t dset_id, 
                                       iod_trans_id_t rtid, hid_t query_id, 
                                       hid_t space_id, hid_t type_id, 
                                       size_t *num_elmts, void **data);

static herr_t H5VL__iod_read_selection(iod_handle_t coh, iod_obj_id_t obj_id, 
                                       iod_trans_id_t rtid, hid_t space_id,
                                       hid_t type_id, void *buf);

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
    Py_DECREF(po_rstring);

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
    Py_XDECREF(po_func);
    Py_XDECREF(po_args_tup);
    Py_XDECREF(po_numpy_array);
    Py_XDECREF(po_numpy_array_split);

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
    size_t i;

    if(!numpy_initialized) H5VL__iod_numpy_init();

    /* Create numpy arrays */
    if(NULL == (po_numpy_arrays = PyList_New((Py_ssize_t) num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't create list of arrays")
    for(i = 0; i < num_targets; i++) {
        PyObject *po_numpy_array = NULL;
        int py_ret = 0;

        if(NULL == (po_numpy_array = 
                    H5VL__iod_create_numpy_array(split_num_elmts[i],
                                                 data_type_id, 
                                                 split_data[i])))
            HGOTO_ERROR_FF(FAIL, "can't create numpy array from data")

        if(0 != (py_ret = PyList_SetItem(po_numpy_arrays, (Py_ssize_t) i, po_numpy_array)))
            HGOTO_ERROR_FF(FAIL, "can't set item to array list")
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
    Py_XDECREF(po_func);
    Py_XDECREF(po_args_tup);
    for(i = 0; i < num_targets; i++) {
        Py_XDECREF(po_numpy_arrays + i);
    }
    Py_XDECREF(po_numpy_array_combine);

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
    printf("(%d) Calling iod_container_open on %s\n", my_rank_g, file_name);
    ret = iod_container_open(file_name, NULL, IOD_CONT_R, &temp_cohs[0], NULL);
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
                    iod_obj_id_t obj_id, iod_trans_id_t rtid,
                    hid_t space_id, hid_t type_id, hid_t query_id,
                    const char *split_script, const char *combine_script)
{
    herr_t ret_value = SUCCEED; /* Return value */
    void **split_data;
    size_t *split_num_elmts;
    void *combine_data;
    size_t combine_num_elmts;
    hid_t split_type_id = FAIL, combine_type_id = FAIL;
    hg_request_t *hg_reqs = NULL;
    unsigned int i;
    unsigned int num_targets = obj_map->u_map.array_map.n_range;
    analysis_farm_in_t farm_input;
    analysis_farm_out_t *farm_output = NULL;

    /* function shipper requests */
    if(NULL == (hg_reqs = (hg_request_t *) malloc(sizeof(hg_request_t) * num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't allocate HG requests");

    if(NULL == (farm_output = (analysis_farm_out_t *) malloc(sizeof(analysis_farm_out_t) * num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't allocate HG requests");

    if(NULL == (split_data = (void **) malloc(sizeof(void *) * num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't allocate array for split data");

    if(NULL == (split_num_elmts = (size_t *) malloc(sizeof(size_t) * num_targets)))
        HGOTO_ERROR_FF(FAIL, "can't allocate array for num elmts");

    farm_input.obj_id = obj_id;
    farm_input.rtid = rtid;
    farm_input.space_id = space_id;
    farm_input.type_id = type_id;
    farm_input.query_id = query_id;
    farm_input.split_script = split_script;

    for (i = 0; i < obj_map->u_map.array_map.n_range; i++) {
        unsigned int j;
        unsigned int server_idx = 0;
        farm_input.num_cells =  obj_map->u_map.array_map.array_range[i].n_cell;
        farm_input.coords.rank = H5Sget_simple_extent_ndims(space_id);
        farm_input.coords.start_cell = obj_map->u_map.array_map.array_range[i].start_cell;
        farm_input.coords.end_cell = obj_map->u_map.array_map.array_range[i].end_cell;

        for (j = 0; j < (unsigned int) num_ions_g; j++) {
            if (0 == strcmp(obj_map->u_map.array_map.array_range[i].loc,
                    server_loc_g[j])) {
                server_idx = j;
                printf("(%d) Server %d owns this object\n", my_rank_g, server_idx);
                break;
            }
        }

        farm_input.server_idx = server_idx;
        farm_input.coh = cohs[server_idx];

        /* forward the call to the target server */
        if (server_idx == 0) {
            hg_reqs[i] = HG_REQUEST_NULL;
            /* Do a local split */
            if(FAIL == H5VL__iod_farm_split(cohs[0], obj_id, rtid, space_id,
                    farm_input.coords, farm_input.num_cells, type_id, query_id, split_script,
                    &split_data[i], &split_num_elmts[i], &split_type_id))
                HGOTO_ERROR_FF(FAIL, "can't split in farmed job");
        } else {
            if(HG_Forward(server_addr_g[server_idx],
                    H5VL_EFF_ANALYSIS_FARM, &farm_input, &farm_output[i],
                    &hg_reqs[i]) < 0)
                HGOTO_ERROR_FF(FAIL, "failed to ship operation");
        }
    }

    for (i = 0; i < num_targets; i++) {
        if (hg_reqs[i] == HG_REQUEST_NULL) {
            /* No request / was local */
        } else {
            analysis_transfer_in_t transfer_input;
            analysis_transfer_out_t transfer_output;
            hg_bulk_t bulk_handle;
            size_t split_data_size;
            unsigned int server_idx;

            /* Wait for the farmed work to complete */
            if(HG_Wait(hg_reqs[i], HG_MAX_IDLE_TIME, HG_STATUS_IGNORE) < 0)
                HGOTO_ERROR_FF(FAIL, "HG_Wait Failed");

            /* Get split type ID and num_elemts (all the arrays should have the same native type id) */
            server_idx = farm_output[i].server_idx;
            split_type_id = farm_output[i].type_id;
            split_num_elmts[i] = farm_output[i].num_elmts;
            split_data_size = split_num_elmts[i] * H5Tget_size(split_type_id);
//            printf("Getting %d elements of size %zu from server %zu\n",
//                    split_num_elmts[i], H5Tget_size(split_type_id), server_idx);

            if(NULL == (split_data[i] = malloc(split_data_size)))
                HGOTO_ERROR_FF(FAIL, "can't allocate farm buffer");

            HG_Bulk_handle_create(split_data[i], split_data_size,
                    HG_BULK_READWRITE, &bulk_handle);

            transfer_input.axe_id = farm_output[i].axe_id;
            transfer_input.bulk_handle = bulk_handle;

            /* forward a free call to the target server */
            if(HG_Forward(server_addr_g[server_idx], H5VL_EFF_ANALYSIS_FARM_TRANSFER,
                    &transfer_input, &transfer_output, &hg_reqs[i]) < 0)
                HGOTO_ERROR_FF(FAIL, "failed to ship operation");

            /* Wait for the farmed work to complete */
            if(HG_Wait(hg_reqs[i], HG_MAX_IDLE_TIME, HG_STATUS_IGNORE) < 0)
                HGOTO_ERROR_FF(FAIL, "HG_Wait Failed");

            /* Free bulk handle */
            HG_Bulk_handle_free(bulk_handle);

            /* Free Mercury request */
            if(HG_Request_free(hg_reqs[i]) != HG_SUCCESS)
                HGOTO_ERROR_FF(FAIL, "Can't Free Mercury Request");
        }
    }

    if(hg_reqs)
        free(hg_reqs);
    if(farm_output)
        free(farm_output);

    printf("(%d) Applying combine on data\n", my_rank_g);
#ifdef H5_HAVE_PYTHON
    if (H5VL__iod_combine(combine_script, split_data, split_num_elmts,
            num_targets, split_type_id, &combine_data,
            &combine_num_elmts, &combine_type_id) < 0)
        HGOTO_ERROR_FF(FAIL, "can't combine split data");
#endif

    /* free farm data */
    if (split_data) {
        for (i = 0; i < num_targets; i++) {
            free(split_data[i]);
        }
        free(split_data);
    }
    free(split_num_elmts);

done:
    return ret_value;
} /* end H5VL__iod_farm_work */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_execute_cb
 *
 * Purpose:	Retrieves layout of object
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_analysis_execute_cb(AXE_engine_t UNUSED axe_engine, 
                                    size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                    size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                    void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    analysis_execute_in_t *input = (analysis_execute_in_t *)op_data->input;
    analysis_execute_out_t output;
    const char *file_name = input->file_name;
    const char *obj_name = input->obj_name;
    hid_t query_id = input->query_id;
    const char *split_script = input->split_script;
    const char *combine_script = input->combine_script;
    hid_t space_id = FAIL, type_id = FAIL;
    iod_cont_trans_stat_t *tids = NULL;
    iod_trans_id_t rtid;
    iod_handle_t *cohs; /* the container handle */
    iod_handles_t root_handle; /* root handle */
    iod_obj_id_t obj_id; /* The ID of the object */
    iod_handles_t obj_oh; /* object handle */
    iod_handle_t mdkv_oh;
    scratch_pad sp;
    iod_obj_map_t *obj_map = NULL;
    unsigned int i;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    /* ****************** TEMP THING (as IOD requires collective container open) */

    ret = H5VL__iod_request_container_open(file_name, &cohs);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't request container open");

    /* ***************** END TEMP THING */

    ret = iod_query_cont_trans_stat(cohs[0], &tids, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't get container tids status");

    rtid = tids->latest_rdable;

    ret = iod_free_cont_trans_stat(cohs[0], tids);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't free container transaction status object");

    ret = iod_trans_start(cohs[0], &rtid, NULL, 0, IOD_TRANS_R, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't start transaction");

    root_handle.rd_oh.cookie = IOD_OH_UNDEFINED;
    root_handle.wr_oh.cookie = IOD_OH_UNDEFINED;

    /* Traverse Path to retrieve object ID, and open object */
    ret = H5VL_iod_server_open_path(cohs[0], ROOT_ID, root_handle, obj_name,
                                    rtid, 7, &obj_id, &obj_oh);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't open object");

#if H5_EFF_DEBUG
    fprintf(stderr, "(%d) coh %"PRIu64" objoh %"PRIu64" objid %"PRIx64" rtid %"PRIu64"\n",
            my_rank_g, cohs[0].cookie, obj_oh.rd_oh.cookie, obj_id, rtid);
    fprintf(stderr, "Calling  iod_obj_query_map\n");
#endif

    ret = iod_obj_query_map(obj_oh.rd_oh, rtid, &obj_map, NULL);
    if (ret != 0)
        HGOTO_ERROR_FF(ret, "iod_obj_query_map failed");

#if H5_EFF_DEBUG
    fprintf(stderr, "(%d) %-10d\n", my_rank_g, obj_map->u_map.array_map.n_range);
#endif

    for (i = 0; i < obj_map->u_map.array_map.n_range; i++) {
#if H5_EFF_DEBUG
        fprintf(stderr, "(%d) range: %d, start: %zu %zu, "
                "end: %zu %zu, n_cell: %zu, "
                "loc: %s\n", my_rank_g, i,
                obj_map->u_map.array_map.array_range[i].start_cell[0],
                obj_map->u_map.array_map.array_range[i].start_cell[1],
                obj_map->u_map.array_map.array_range[i].end_cell[0],
                obj_map->u_map.array_map.array_range[i].end_cell[1],
                obj_map->u_map.array_map.array_range[i].n_cell,
                obj_map->u_map.array_map.array_range[i].loc);
#endif
    }

    /* get scratch pad */
    ret = iod_obj_get_scratch(obj_oh.rd_oh, rtid, (char *) &sp, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't get scratch pad for object");

    /* retrieve datatype and dataspace */
    /* MSC - This applies only to DATASETS for Q6 */

    /* open the metadata scratch pad */
    ret = iod_obj_open_read(cohs[0], sp[0], rtid, NULL, &mdkv_oh, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't open scratch pad");

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                                H5VL_IOD_KEY_OBJ_DATATYPE, 7, NULL, &type_id);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "failed to retrieve datatype");

    ret = H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, 
                                H5VL_IOD_KEY_OBJ_DATASPACE, 7, NULL, &space_id);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(FAIL, "failed to retrieve dataspace");

    /*******************************************/
    /* Farm work */
    ret = H5VL__iod_farm_work(obj_map, cohs, obj_id, rtid, space_id, type_id,
                              query_id, split_script, combine_script);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't farm work");

    /********************************************/

    ret = iod_obj_free_map(obj_oh.rd_oh, obj_map);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't free IOD map");

    ret = iod_obj_close(mdkv_oh, NULL, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't close object");

    ret = iod_obj_close(obj_oh.rd_oh, NULL, NULL);
    if(ret < 0)
        HDONE_ERROR_FF(ret, "can't close Array object");

    ret = iod_trans_finish(cohs[0], rtid, NULL, 0, NULL);
    if(ret < 0)
        HGOTO_ERROR_FF(ret, "can't finish transaction 0");

    /* ****************** TEMP THING (as IOD requires collective container open) */

    ret = H5VL__iod_request_container_close(cohs);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't request container close");

    /* ***************** END TEMP THING */

#if H5_EFF_DEBUG
    fprintf(stderr, "Analysis DONE\n");
#endif

    /* set output, and return to AS client */
    output.ret = ret_value;
    HG_Handler_start_output(op_data->hg_handle, &output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(op_data->hg_handle, &ret_value);

    if(space_id)
        H5Sclose(space_id);
    if(type_id)
        H5Tclose(type_id);

    input = (analysis_execute_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_analysis_execute_cb() */

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
                     hid_t space_id, coords_t coords, iod_size_t num_cells,
                     hid_t type_id, hid_t query_id, 
                     const char *split_script, void **split_data, 
                     size_t *split_num_elmts, hid_t *split_type_id)
{
    void *data = NULL;
    size_t num_elmts;
    herr_t ret_value = SUCCEED;
    hid_t space_layout;

    if(FAIL == (space_layout = H5VL__iod_get_space_layout(coords, num_cells, space_id)))
        HGOTO_ERROR_FF(FAIL, "can't generate local dataspace selection");

    if(H5VL__iod_get_query_data(coh, obj_id, rtid, query_id, space_layout,
                                type_id, &num_elmts, &data) < 0)
        HGOTO_ERROR_FF(FAIL, "can't read local data");

#if H5_EFF_DEBUG
    fprintf(stderr, "(%d) Applying split on data\n", my_rank_g);
#endif

    /* Apply split python script on data from query */
#ifdef H5_HAVE_PYTHON
    if(FAIL == H5VL__iod_split(split_script, data, num_elmts, type_id,
                               split_data, split_num_elmts, split_type_id))
        HGOTO_ERROR_FF(FAIL, "can't apply split script to data");
#endif

    /* Free the data after split operation */
    H5MM_free(data);
    data = NULL;

done:
    if(space_layout)
        H5Sclose(space_layout);

    return ret_value;
} /* end H5VL__iod_farm_work */

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
    hid_t query_id = input->query_id;
    hid_t space_id = input->space_id;
    hid_t type_id = input->type_id;
    iod_trans_id_t rtid = input->rtid;
    iod_obj_id_t obj_id = input->obj_id; /* The ID of the object */
    coords_t coords = input->coords;
    const char *split_script = input->split_script;
    iod_size_t num_cells = input->num_cells;
    void *split_data = NULL;
    size_t split_num_elmts;
    hid_t split_type_id;
    herr_t ret_value = SUCCEED;

    if(H5VL__iod_farm_split(coh, obj_id, rtid, space_id, coords, num_cells,
                            type_id, query_id, split_script,
                            &split_data, &split_num_elmts, &split_type_id) < 0)
        HGOTO_ERROR_FF(FAIL, "can't split in farmed job");

    /* allocate output struct */
    if(NULL == (output = (H5VLiod_farm_data_t *)H5MM_malloc(sizeof(H5VLiod_farm_data_t))))
        HGOTO_ERROR_FF(FAIL, "No Space");

    /* set output, and return to master */
    output->farm_out.ret = ret_value;
    output->farm_out.axe_id = op_data->axe_id;
    output->farm_out.server_idx = input->server_idx;
    output->farm_out.num_elmts = split_num_elmts;
    output->farm_out.type_id = split_type_id;
    output->data = split_data;
    op_data->output = output;

    HG_Handler_start_output(op_data->hg_handle, &output->farm_out);

done:
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
    printf("(%d) Transferring split data back to master\n", my_rank_g);

    HG_Bulk_handle_create(farm_output->data, data_size, HG_BULK_READ_ONLY,
            &bulk_block_handle);

    /* Write bulk data here and wait for the data to be there  */
    if(HG_SUCCESS != HG_Bulk_write_all(source,
            input->bulk_handle, bulk_block_handle, &bulk_request))
        HGOTO_ERROR_FF(FAIL, "can't get data from function shipper");

    /* wait for it to complete */
    if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
        HGOTO_ERROR_FF(FAIL, "can't get data from function shipper");

    /* free the bds block handle */
    if(HG_SUCCESS != HG_Bulk_handle_free(bulk_block_handle))
        HGOTO_ERROR_FF(FAIL, "can't free bds block handle");

    free(farm_output->data);
    farm_op_data = (op_data_t *)H5MM_xfree(farm_op_data);

done:
    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    input = (analysis_transfer_in_t *)H5MM_xfree(input);
    op_data = (op_data_t *)H5MM_xfree(op_data);

} /* end H5VL_iod_server_analysis_farm_cb() */

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
H5VL__iod_get_space_layout(coords_t coords, iod_size_t num_cells, hid_t space_id)
{
    int ndims, i;
    hsize_t start[H5S_MAX_RANK];
    hsize_t block[H5S_MAX_RANK];
    hsize_t count[H5S_MAX_RANK];

    hid_t space_layout = FAIL, ret_value = FAIL;

    /* retrieve number of dimensions and dimensions. */
    ndims = H5Sget_simple_extent_ndims(space_id);

    /* copy the original dataspace and reset selection to NONE */
    if(FAIL == (space_layout = H5Scopy(space_id)))
        HGOTO_ERROR_FF(FAIL, "unable to copy dataspace");

    for(i=0 ; i<ndims ; i++) {
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
 * Function:    H5VL__iod_get_query_data_cb
 *
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5VL__iod_get_query_data_cb(void *elem, hid_t type_id, unsigned ndim, 
                            const hsize_t *point, void *_udata)
{
    H5VL__iod_get_query_data_t *udata = (H5VL__iod_get_query_data_t *)_udata;
    hbool_t result;
    herr_t ret_value = SUCCEED;

    /* Apply the query */
    if(H5Qapply(udata->query_id, &result, type_id, elem) < 0)
        HGOTO_ERROR_FF(FAIL, "unable to apply query to data element");

    /* If element satisfies query, add it to the selection */
    if (result) {
        /* TODO remove that after demo */
#if H5_EFF_DEBUG
        fprintf(stderr, "(%d) Element |%d| matches query\n", my_rank_g, *((int *) elem));
#endif
        udata->num_elmts ++;
        if(H5Sselect_elements(udata->space_query, H5S_SELECT_APPEND, 1, point) < 0)
            HGOTO_ERROR_FF(FAIL, "unable to add point to selection")
    }

done:
    return ret_value;
} /* end H5VL__iod_get_query_data_cb */

/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_get_query_data
 *
 * Purpose:     Generates a dataspace from the query specified. The dataspace 
 *              returned must be closed with H5Sclose().
 *
 * Return:	Success:	space id
 *		Failure:	FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5VL__iod_get_query_data(iod_handle_t coh, iod_obj_id_t dset_id, 
                         iod_trans_id_t rtid, hid_t query_id, 
                         hid_t space_id, hid_t type_id, 
                         size_t *num_elmts, void **data)
{
    hsize_t dims[1];
    size_t nelmts;
    size_t elmt_size=0, buf_size=0;
    H5VL__iod_get_query_data_t udata;
    void *buf = NULL;
    hid_t space_query = FAIL, mem_space = FAIL;
    iod_ret_t ret;
    herr_t ret_value = SUCCEED;

    nelmts = (size_t) H5Sget_select_npoints(space_id);
    elmt_size = H5Tget_size(type_id);
    buf_size = nelmts * elmt_size;

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR_FF(FAIL, "can't allocate read buffer");

    /* read the data local on the ION specified in the layout selection */
    ret = H5VL__iod_read_selection(coh, dset_id, rtid, space_id, type_id, buf);
    if(SUCCEED != ret)
        HGOTO_ERROR_FF(ret, "can't read local data");

    dims[0] = (hsize_t)nelmts;
    /* create a 1-D selection to describe the data read in memory */
    if(FAIL == (mem_space = H5Screate_simple(1, dims, NULL)))
        HGOTO_ERROR_FF(FAIL, "can't create simple dataspace");

    if(FAIL == (space_query = H5Scopy(mem_space)))
        HGOTO_ERROR_FF(FAIL, "unable to copy dataspace");

    udata.query_id = query_id;
    udata.space_query = space_query;
    udata.num_elmts = 0;

    /* iterate over every element and apply the query on it. If the
       query is not satisfied, then remove it from the query selection */
    if(H5Diterate(buf, type_id, mem_space, H5VL__iod_get_query_data_cb, &udata) < 0)
        HGOTO_ERROR_FF(FAIL, "failed to compute buffer size");

    if(udata.num_elmts) {
        buf_size = udata.num_elmts * elmt_size;

        /* allocate buffer to hold data */
        if(NULL == (*data = malloc(buf_size)))
            HGOTO_ERROR_FF(FAIL, "can't allocate data buffer");

        if(H5Dgather(space_query, buf, type_id, buf_size, *data, NULL, NULL) < 0)
            HGOTO_ERROR_FF(FAIL, "gather failed")
    }

    *num_elmts = udata.num_elmts;

done:
    if(space_query && H5Sclose(space_query) < 0)
        HDONE_ERROR_FF(FAIL, "unable to release dataspace")
    if(mem_space && H5Sclose(mem_space) < 0)
        HDONE_ERROR_FF(FAIL, "unable to release dataspace")
    if(buf != NULL)
        free(buf);

    return ret_value;
} /* end H5VL__iod_get_query_data() */

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

#endif /* H5_HAVE_EFF */
