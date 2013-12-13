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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              October, 2013
 *
 * Purpose:	Routines to support Analysis Shipping.
 */

#include "H5VLiod_server.h"

#include "H5Qpublic.h"

#ifdef H5_HAVE_PYTHON
#include <Python.h>
#include <numpy/ndarrayobject.h>
#endif

#ifdef H5_HAVE_EFF

/* User data for dataspace iteration to query elements. */
typedef struct {
    size_t num_elmts;
    hid_t query_id;
    hid_t space_query;
} H5VL__iod_get_query_data_t;

/* do not change order */
typedef struct {
    int32_t ret;
    uint64_t axe_id;
    hg_bulk_t bulk_handle;
    hid_t type_id;
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

static hid_t H5VL__iod_get_space_layout(iod_layout_t layout, hid_t space, 
                                        uint32_t target_index);

static herr_t H5VL__iod_get_query_data_cb(void *elem, hid_t type_id, unsigned ndim, 
                                          const hsize_t *point, void *_udata);

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

    FUNC_ENTER_NOAPI_NOINIT

    /* Get reference to main */
    if(NULL == (po_main = PyImport_AddModule("__main__")))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, NULL, "can't get reference to main module");

    if(NULL == (po_main_dict = PyModule_GetDict(po_main)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, NULL, "can't get dictionary from main module");

    /* Load script */
    if(NULL == (po_rstring = PyRun_String(script, Py_file_input, po_main_dict, po_main_dict)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTLOAD, NULL, "can't load script into main module");

    /* Get reference to function name */
    if(NULL == (po_func = PyObject_GetAttrString(po_main, func_name)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, NULL, "can't get reference to function");

    ret_value = po_func;

done:
    Py_DECREF(po_rstring);

    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_NOAPI_NOINIT

    if((native_datatype_id = H5Tget_native_type(data_type_id, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR2(H5E_ARGS, H5E_BADTYPE, FAIL, "cannot retrieve native type");

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
        HGOTO_ERROR2(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid type")

    if(FAIL == H5Tclose(native_datatype_id))
        HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTFREE, FAIL, "unable to free datatype");

    *npy_type = translated_type;

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_NOAPI_NOINIT

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
            HGOTO_ERROR2(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid type")
            break;
    }

    if (0 == (translated_type_size = H5Tget_size(translated_type_id)))
        HGOTO_ERROR2(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid size")

    *data_type_id = translated_type_id;
    *data_type_size = translated_type_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(data);

    /* Convert data_type to numpy type */
    if(FAIL == H5VL__iod_translate_h5_type(data_type_id, &npy_type))
        HGOTO_ERROR2(H5E_DATATYPE, H5E_CANTCONVERT, NULL, "unable to translate datatype to NPY type");

    ret_value = PyArray_SimpleNewFromData(1, dim, npy_type, data);

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(numpy_array);
    HDassert(data);
    HDassert(num_elmts);
    HDassert(data_type_id);

    /* Input array should be 1D */
    if(PyArray_NDIM(numpy_array) > 1)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "was expecting 1-dimensional array")

    if(NULL == (numpy_data = PyArray_DATA(numpy_array)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get data from numpy array")

    if(NULL == (numpy_dims = PyArray_DIMS(numpy_array)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get dimensions from numpy array")
     numpy_num_elmts = (size_t) numpy_dims[0];

    if(0 == (numpy_datatype = PyArray_TYPE(numpy_array)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get type from numpy array")

    /* Translate NPY type to hid_t */
    if(FAIL == H5VL__iod_translate_numpy_type(numpy_datatype, &extracted_datatype_id,
            &extracted_datatype_size))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTCONVERT, FAIL, "can't translate NPY type")

    /* Copy data from NPY array */
    if(NULL == (extracted_data = H5MM_malloc(numpy_num_elmts * extracted_datatype_size)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTALLOC, FAIL, "can't allocate extracted data")
    HDmemcpy(extracted_data, numpy_data, numpy_num_elmts * extracted_datatype_size);

    *data = extracted_data;
    *num_elmts = numpy_num_elmts;
    *data_type_id = extracted_datatype_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_NOAPI_NOINIT

    if(!numpy_initialized) H5VL__iod_numpy_init();

    /* Create numpy array */
    if(NULL == (po_numpy_array = H5VL__iod_create_numpy_array(num_elmts, data_type_id, data)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTCREATE, FAIL, "can't create numpy array from data")

    /* Load script */
    if(NULL == (po_func = H5VL__iod_load_script(split_script, "split")))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTLOAD, FAIL, "can't load split script")

    /* Check whether split is defined as function in given script */
    if(0 == PyCallable_Check(po_func))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTLOAD, FAIL, "can't find split function")

    /* Get the input to function */
    if(NULL == (po_args_tup = PyTuple_Pack(1, po_numpy_array)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTLOAD, FAIL, "can't set input to split function")

    /* Invoke the split function */
    if(NULL == (po_numpy_array_split = PyObject_CallObject(po_func, po_args_tup)))
        HGOTO_ERROR2(H5E_SYM, H5E_BADVALUE, FAIL, "returned NULL python object")

    /* Extract data from numpy array */
    if(FAIL == H5VL__iod_extract_numpy_array(po_numpy_array_split, split_data,
            split_num_elmts, split_data_type_id))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTCREATE, FAIL, "can't extract data from numpy array")

done:
    /* Cleanup */
    Py_XDECREF(po_func);
    Py_XDECREF(po_args_tup);
    Py_XDECREF(po_numpy_array);
    Py_XDECREF(po_numpy_array_split);

    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_NOAPI_NOINIT

    if(!numpy_initialized) H5VL__iod_numpy_init();

    /* Create numpy arrays */
    if(NULL == (po_numpy_arrays = PyList_New((Py_ssize_t) num_targets)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTCREATE, FAIL, "can't create list of arrays")
    for(i = 0; i < num_targets; i++) {
        PyObject *po_numpy_array = NULL;
        int py_ret = 0;

        if(NULL == (po_numpy_array = 
                    H5VL__iod_create_numpy_array(split_num_elmts[i],
                                                 data_type_id, 
                                                 split_data[i])))
            HGOTO_ERROR2(H5E_SYM, H5E_CANTCREATE, FAIL, "can't create numpy array from data")

        if(0 != (py_ret = PyList_SetItem(po_numpy_arrays, (Py_ssize_t) i, po_numpy_array)))
            HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't set item to array list")
    }

    /* Load script */
    if(NULL == (po_func = H5VL__iod_load_script(combine_script, "combine")))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTLOAD, FAIL, "can't load combine script")

    /* Check whether split is defined as function in given script */
    if(0 == PyCallable_Check(po_func))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTLOAD, FAIL, "can't find combine function")

    /* Get the input to function */
    if(NULL == (po_args_tup = PyTuple_Pack(1, po_numpy_arrays)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTLOAD, FAIL, "can't set input to combine function")

    /* Invoke the split function */
    if(NULL == (po_numpy_array_combine = PyObject_CallObject(po_func, po_args_tup)))
        HGOTO_ERROR2(H5E_SYM, H5E_BADVALUE, FAIL, "returned NULL python object")

    /* Extract data from numpy array */
    if(FAIL == H5VL__iod_extract_numpy_array(po_numpy_array_combine, combine_data,
            combine_num_elmts, combine_data_type_id))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTCREATE, FAIL, "can't extract data from numpy array")

done:
    /* Cleanup */
    Py_XDECREF(po_func);
    Py_XDECREF(po_args_tup);
    for(i = 0; i < num_targets; i++) {
        Py_XDECREF(po_numpy_arrays + i);
    }
    Py_XDECREF(po_numpy_array_combine);

    FUNC_LEAVE_NOAPI(ret_value)
}

#endif /* H5_HAVE_PYTHON */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_execute_cb
 *
 * Purpose:	Retrieves layout of object
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              September, 2013
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
    hid_t space_id = FAIL, type_id = FAIL, split_type_id = FAIL, combine_type_id = FAIL;
    void **split_data;
    size_t *split_num_elmts;
    void *combine_data;
    size_t combine_num_elmts;
    iod_cont_trans_stat_t *tids = NULL;
    iod_trans_id_t rtid;
    iod_handle_t coh; /* the container handle */
    iod_handles_t root_handle; /* root handle */
    iod_obj_id_t obj_id; /* The ID of the object */
    iod_handles_t obj_oh; /* object handle */
    iod_handle_t mdkv_oh;
    iod_layout_t layout;
    int i;
    hg_request_t *hg_reqs = NULL;
    iod_handle_t *temp_cohs;
    hg_status_t status;
    scratch_pad sp;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* *********************** TEMP THING */
    /* forward a call to every ION to open the container */
    if(NULL == (hg_reqs = (hg_request_t *)malloc
                (sizeof(hg_request_t) * num_ions_g)))
        HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate HG requests");

    if(NULL == (temp_cohs = (iod_handle_t *)malloc
                (sizeof(iod_handle_t) * num_ions_g)))
        HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate container handles");

    for(i=1 ; i<num_ions_g ; i++) {
        if(HG_Forward(server_addr_g[i], H5VL_EFF_OPEN_CONTAINER, &file_name, 
                      &temp_cohs[i], &hg_reqs[i]) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship operation");
    }

    for(i=1 ; i<num_ions_g ; i++) {
        if(HG_Wait(hg_reqs[i], HG_MAX_IDLE_TIME, &status) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "HG_Wait Failed");
        if(!status) {
            fprintf(stderr, "Wait timeout reached\n");
            ret_value = FAIL;
            goto done;
        }
        /* Free Mercury request */
        if(HG_Request_free(hg_reqs[i]) != HG_SUCCESS)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTFREE, FAIL, "Can't Free Mercury Request");
    }
    free(hg_reqs);
    /* *********************** END TEMP THING */

    /* open the container */
    if(iod_container_open(file_name, NULL, IOD_CONT_R, &coh, NULL))
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't open file");

    if(iod_query_cont_trans_stat(coh, &tids, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get container tids status");

    rtid = tids->latest_rdable;

    if(iod_free_cont_trans_stat(coh, tids) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't free container transaction status object");

    if(iod_trans_start(coh, &rtid, NULL, 0, IOD_TRANS_R, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't start transaction");

    root_handle.rd_oh.cookie = IOD_OH_UNDEFINED;
    root_handle.wr_oh.cookie = IOD_OH_UNDEFINED;

    /* Traverse Path to retrieve object ID, and open object */
    if(H5VL_iod_server_open_path(coh, ROOT_ID, root_handle, obj_name, 
                                 rtid, &obj_id, &obj_oh) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't open object");

    /* retrieve layout of object */
    if(iod_obj_get_layout(obj_oh.rd_oh, rtid, &layout, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't get object layout");

    /* get scratch pad */
    if(iod_obj_get_scratch(obj_oh.rd_oh, rtid, &sp, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_FILE, H5E_CANTINIT, FAIL, "can't get scratch pad for object");

    /* retrieve datatype and dataspace */
    /* MSC - This applies only to DATASETS for Q6 */
    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATATYPE, 
                             H5VL_IOD_KEY_OBJ_DATATYPE,
                             NULL, NULL, &type_id) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve datatype");

    if(H5VL_iod_get_metadata(mdkv_oh, rtid, H5VL_IOD_DATASPACE, 
                             H5VL_IOD_KEY_OBJ_DATASPACE,
                             NULL, NULL, &space_id) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "failed to retrieve dataspace");

    /* close the metadata scratch pad */
    if(iod_obj_close(mdkv_oh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't close object");
    /* close object */
    if(iod_obj_close(obj_oh.rd_oh, NULL, NULL) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");

    /* Farm work */
    {
        analysis_farm_in_t farm_input;
        analysis_farm_out_t *farm_output = NULL;

        /* function shipper requests */
        if(NULL == (hg_reqs = (hg_request_t *)malloc
                    (sizeof(hg_request_t) * layout.target_num)))
            HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate HG requests");

        if(NULL == (farm_output = (analysis_farm_out_t *)malloc
                    (sizeof(analysis_farm_out_t) * layout.target_num)))
            HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate HG requests");

        if(NULL == (split_data = (void **)malloc
                    (sizeof(void *) * layout.target_num)))
            HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array for split data");

        if(NULL == (split_num_elmts = (size_t *)malloc
                    (sizeof(size_t) * layout.target_num)))
            HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate array for num elmts");

        farm_input.obj_id = obj_id;
        farm_input.rtid = rtid;
        farm_input.layout = layout;
        farm_input.space_id = space_id;
        farm_input.type_id = type_id;
        farm_input.query_id = query_id;
        farm_input.split_script = split_script;

        for(i=0 ; i<layout.target_num ; i++) {
            farm_input.coh = temp_cohs[i];
            farm_input.target_idx = i+layout.target_start;
            /* forward the call to the target server */
            if(HG_Forward(server_addr_g[i+layout.target_start], 
                          H5VL_EFF_ANALYSIS_FARM, &farm_input, 
                          &farm_output[i], &hg_reqs[i]) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship operation");
        }

        for(i=0 ; i<layout.target_num ; i++) {
            hg_bulk_block_t bulk_block_handle; /* HG block handle */
            hg_bulk_request_t bulk_request; /* HG request */
            size_t split_data_size;

            /* Wait for the farmed work to complete */
            if(HG_Wait(hg_reqs[i], HG_MAX_IDLE_TIME, &status) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "HG_Wait Failed");
            if(!status) {
                fprintf(stderr, "Wait timeout reached\n");
                ret_value = FAIL;
                goto done;
            }

            split_data_size = HG_Bulk_handle_get_size(farm_output[i].bulk_handle);

            /* Get split type ID and num_elemts (all the arrays should have the same native type id) */
            split_type_id = farm_output[i].type_id;
            split_num_elmts[i] = split_data_size / H5Tget_size(split_type_id);

            if(NULL == (split_data[i] = malloc(split_data_size)))
                HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate farm buffer");

            HG_Bulk_block_handle_create(split_data[i], split_data_size,
                                        HG_BULK_READWRITE, &bulk_block_handle);

            /* Write bulk data here and wait for the data to be there  */
            if(HG_SUCCESS != HG_Bulk_read_all(server_addr_g[i+layout.target_start], 
                                              farm_output[i].bulk_handle, 
                                              bulk_block_handle, &bulk_request))
                HGOTO_ERROR2(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");
            /* wait for it to complete */
            if(HG_SUCCESS != HG_Bulk_wait(bulk_request, HG_MAX_IDLE_TIME, HG_STATUS_IGNORE))
                HGOTO_ERROR2(H5E_SYM, H5E_WRITEERROR, FAIL, "can't get data from function shipper");

            /* free the bds block handle */
            if(HG_SUCCESS != HG_Bulk_block_handle_free(bulk_block_handle))
                HGOTO_ERROR2(H5E_SYM, H5E_WRITEERROR, FAIL, "can't free bds block handle");

            /* Free Mercury request */
            if(HG_Request_free(hg_reqs[i]) != HG_SUCCESS)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTFREE, FAIL, "Can't Free Mercury Request");

            /* forward a free call to the target server */
            if(HG_Forward(server_addr_g[i+layout.target_start], H5VL_EFF_ANALYSIS_FARM_FREE, 
                          &farm_output[i].axe_id, &farm_output[i], &hg_reqs[i]) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship operation");
        }

        /* Wait for the free calls to complete. */
        for(i=0 ; i<layout.target_num ; i++) {
            /* Wait for the farmed work to complete */
            if(HG_Wait(hg_reqs[i], HG_MAX_IDLE_TIME, &status) < 0)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "HG_Wait Failed");
            if(!status) {
                fprintf(stderr, "Wait timeout reached\n");
                ret_value = FAIL;
                goto done;
            }

            /* Free Mercury request */
            if(HG_Request_free(hg_reqs[i]) != HG_SUCCESS)
                HGOTO_ERROR2(H5E_SYM, H5E_CANTFREE, FAIL, "Can't Free Mercury Request");
        }

        if(hg_reqs)
            free(hg_reqs);
        if(farm_output)
            free(farm_output);

#ifdef H5_HAVE_PYTHON
        if (H5VL__iod_combine(combine_script, split_data, split_num_elmts,
                layout.target_num, split_type_id, &combine_data,
                &combine_num_elmts, &combine_type_id) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "can't combine split data");
#endif

        /* free farm data */
        if (split_data) {
            for(i=0 ; i<layout.target_num ; i++) {
                free(split_data[i]);
            }
            free(split_data);
        }
        free(split_num_elmts);
    }
    if(iod_trans_finish(coh, rtid, NULL, 0, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTSET, FAIL, "can't finish transaction 0");

    if(NULL == (hg_reqs = (hg_request_t *)malloc
                (sizeof(hg_request_t) * num_ions_g)))
        HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate HG requests");

    /* *********************** TEMP THING */
    for(i=1 ; i<num_ions_g ; i++) {
        if(HG_Forward(server_addr_g[i], H5VL_EFF_CLOSE_CONTAINER, &temp_cohs[i], &ret_value, 
                      &hg_reqs[i]) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ship operation");
    }

    for(i=1 ; i<num_ions_g ; i++) {
        if(HG_Wait(hg_reqs[i], HG_MAX_IDLE_TIME, &status) < 0)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "HG_Wait Failed");
        if(!status) {
            fprintf(stderr, "Wait timeout reached\n");
            ret_value = FAIL;
            goto done;
        }
        /* Free Mercury request */
        if(HG_Request_free(hg_reqs[i]) != HG_SUCCESS)
            HGOTO_ERROR2(H5E_SYM, H5E_CANTFREE, FAIL, "Can't Free Mercury Request");
    }
    free(hg_reqs);
    free(temp_cohs);
    /* *********************** END TEMP THING */

    /* close the container */
    if(iod_container_close(coh, NULL, NULL) < 0)
        HGOTO_ERROR2(H5E_FILE, H5E_CANTDEC, FAIL, "can't close container");

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


    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_analysis_execute_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_farm_cb
 *
 * Purpose:	Retrieves layout of object
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              September, 2013
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
    iod_layout_t layout = input->layout;
    uint32_t target_index = input->target_idx;
    const char *split_script = input->split_script;
    hid_t space_layout;
    void *data = NULL, *split_data = NULL;
    size_t num_elmts, split_num_elmts;
    hid_t split_type_id;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(FAIL == (space_layout = H5VL__iod_get_space_layout(layout, space_id, target_index)))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't generate local dataspace selection");

    if(H5VL__iod_get_query_data(coh, obj_id, rtid, query_id, space_layout, 
                                type_id, &num_elmts, &data) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_READERROR, FAIL, "can't read local data");

    /* Apply split python script on data from query */
#ifdef H5_HAVE_PYTHON
    if(FAIL == H5VL__iod_split(split_script, data, num_elmts, type_id,
            &split_data, &split_num_elmts, &split_type_id))
        HGOTO_ERROR2(H5E_SYM, H5E_CANAPPLY, FAIL, "can't apply split script to data")
#endif

    /* Free the data after split operation */
    H5MM_free(data);
    data = NULL;

    /* allocate output struct */
    if(NULL == (output = (H5VLiod_farm_data_t *)H5MM_malloc(sizeof(H5VLiod_farm_data_t))))
        HGOTO_ERROR2(H5E_DATASET, H5E_NOSPACE, FAIL, "No Space");

    /* Register memory */
    if(HG_SUCCESS != HG_Bulk_handle_create(split_data, split_num_elmts*H5Tget_size(split_type_id),
            HG_BULK_READ_ONLY, &output->bulk_handle))
        HGOTO_ERROR2(H5E_ATTR, H5E_WRITEERROR, FAIL, "can't create Bulk Data Handle");

    /* set output, and return to AS client */
    output->ret = ret_value;
    output->axe_id = op_data->axe_id;
    output->data = split_data;
    output->type_id = split_type_id;
    op_data->output = output;
    HG_Handler_start_output(op_data->hg_handle, output);

done:
    if(ret_value < 0)
        HG_Handler_start_output(op_data->hg_handle, &ret_value);

    if(space_layout)
        H5Sclose(space_layout);
    input = (analysis_farm_in_t *)H5MM_xfree(input);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5VL_iod_server_analysis_farm_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_analysis_farm_free_cb
 *
 * Purpose:	Frees the output from the split operation
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              September, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_server_analysis_farm_free_cb(AXE_engine_t axe_engine, 
                                      size_t UNUSED num_n_parents, AXE_task_t UNUSED n_parents[], 
                                      size_t UNUSED num_s_parents, AXE_task_t UNUSED s_parents[], 
                                      void *_op_data)
{
    op_data_t *op_data = (op_data_t *)_op_data;
    AXE_task_t *farm_id = (AXE_task_t *)op_data->input;
    op_data_t *farm_op_data = NULL;
    void *farm_op_data_ptr = NULL;
    H5VLiod_farm_data_t *farm_output = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(AXE_SUCCEED != AXEget_op_data(axe_engine, *farm_id, &farm_op_data_ptr))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "failed to get farm op_data");

    farm_op_data = (op_data_t *)farm_op_data_ptr;
    farm_output = (H5VLiod_farm_data_t *)farm_op_data->output;

    /* Free memory handle */
    if(HG_SUCCESS != HG_Bulk_handle_free(farm_output->bulk_handle))
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "failed to free Bulk handle");

    free(farm_output->data);
    farm_op_data = (op_data_t *)H5MM_xfree(farm_op_data);

done:
    HG_Handler_start_output(op_data->hg_handle, &ret_value);

    farm_id = (AXE_task_t *)H5MM_xfree(farm_id);
    op_data = (op_data_t *)H5MM_xfree(op_data);

    FUNC_LEAVE_NOAPI_VOID
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
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5VL__iod_get_space_layout(iod_layout_t layout, hid_t space_id, uint32_t target_index)
{
    hsize_t dims[H5S_MAX_RANK];
    int ndims, i;
    size_t nelmts, u, start_elmt;
    hid_t space_layout = FAIL, ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    /* retrieve number of dimensions and dimensions. */
    ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);

    /* number of elements striped on every ION. If the target ION is
       that last ION, special processing is required. */
    if((layout.target_start+layout.target_num-1) == target_index) {
        hssize_t npoints;

        npoints = H5Sget_simple_extent_npoints(space_id);
        nelmts = npoints % layout.stripe_size;
    }
    else {
        nelmts = layout.stripe_size;
    }

    /* copy the original dataspace and reset selection to NONE */
    if(FAIL == (space_layout = H5Scopy(space_id)))
        HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");
    if(H5Sselect_none(space_layout) < 0)
        HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't delete default selection");

    start_elmt = nelmts * target_index;
    /* Add each element as one hyperslab to the selection */
    for(u=0 ; u<nelmts ; u++) {
        hsize_t start[H5S_MAX_RANK];
        hsize_t cur;

        /* generate hyperslab stuff for each point. only start is
           required; the stride, block, count are 1, i.e. can be
           NULL */
        cur = dims[0];
        start[0] = start_elmt % dims[0];

        for(i=1 ; i<ndims ; i++) {
            start[i] = start_elmt / cur;
            cur *= dims[i];
        }

        if(H5Sselect_hyperslab(space_layout, H5S_SELECT_OR, start, NULL, NULL, NULL))
            HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to add point to selection")
    }

    ret_value = space_layout;

done:
    if(ret_value < 0) {
        if(FAIL != space_layout && H5Sclose(space_layout) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_get_space_layout() */


/*-------------------------------------------------------------------------
 * Function:    H5VL__iod_get_query_data_cb
 *
 *
 * Return:	Success:	SUCCEED 
 *		Failure:	Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5VL__iod_get_query_data_cb(void *elem, hid_t type_id, unsigned ndim, 
                            const hsize_t *point, void *_udata)
{
    H5VL__iod_get_query_data_t *udata = (H5VL__iod_get_query_data_t *)_udata;
    hbool_t result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(ndim == 1);

    /* Apply the query */
    if(H5Qapply(udata->query_id, &result, type_id, elem) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTGET, FAIL, "unable to apply query to data element");

    /* If element satisfies query, add it to the selection */
    if (result) {
        udata->num_elmts ++;
        if(H5Sselect_elements(udata->space_query, H5S_SELECT_APPEND, 1, point))
            HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to add point to selection")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
 * Programmer:  Mohamad Chaarawi
 *              October, 2013
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
    hssize_t nelmts;
    size_t elmt_size=0, buf_size=0;
    H5VL__iod_get_query_data_t udata;
    void *buf = NULL;
    hid_t space_query = FAIL, mem_space = FAIL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    nelmts = (size_t)H5Sget_select_npoints(space_id);
    elmt_size = H5Tget_size(type_id);
    buf_size = nelmts * elmt_size;

    /* allocate buffer to hold data */
    if(NULL == (buf = malloc(buf_size)))
        HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* read the data local on the ION specified in the layout selection */
    if(H5VL__iod_read_selection(coh, dset_id, rtid, space_id, type_id, buf) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_READERROR, FAIL, "can't read local data");

    dims[0] = (hsize_t)nelmts;
    /* create a 1-D selection to describe the data read in memory */
    if(FAIL == (mem_space = H5Screate_simple(1, dims, NULL)))
        HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't create simple dataspace");

    if(FAIL == (space_query = H5Scopy(mem_space)))
        HGOTO_ERROR2(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");

    udata.query_id = query_id;
    udata.space_query = space_query;
    udata.num_elmts = 0;

    /* iterate over every element and apply the query on it. If the
       query is not satisfied, then remove it from the query selection */
    if(H5Diterate(buf, type_id, mem_space, H5VL__iod_get_query_data_cb, &udata) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "failed to compute buffer size");

    if(udata.num_elmts) {
        buf_size = udata.num_elmts * elmt_size;

        /* allocate buffer to hold data */
        if(NULL == (*data = malloc(buf_size)))
            HGOTO_ERROR2(H5E_SYM, H5E_NOSPACE, FAIL, "can't allocate data buffer");

        if(H5Dgather(space_query, buf, type_id, buf_size, *data, NULL, NULL) < 0)
            HGOTO_ERROR2(H5E_IO, H5E_CANTCOPY, FAIL, "gather failed")
    }

    *num_elmts = udata.num_elmts;

done:
    if(space_query && H5Sclose(space_query) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    if(mem_space && H5Sclose(mem_space) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    if(buf != NULL)
        free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
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
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* open the array object */
        if (iod_obj_open_read(coh, obj_id, rtid, NULL /*hints*/, &obj_oh, NULL) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_CANTINIT, FAIL, "can't open current group");

    /* read the data selection from IOD. */
    /* MSC - will need to do it in pieces, not it one shot. */
    if(H5VL__iod_server_final_io(coh, obj_oh, space_id, type_id, 
                                 FALSE, buf, buf_size, (uint64_t)0, 0, rtid) < 0)
        HGOTO_ERROR2(H5E_SYM, H5E_READERROR, FAIL, "can't read from array object");

done:
    if(obj_oh.cookie != IOD_OH_UNDEFINED && iod_obj_close(obj_oh, NULL, NULL) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't close Array object");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__iod_read_selection() */

#endif /* H5_HAVE_EFF */
