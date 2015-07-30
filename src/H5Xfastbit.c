/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose:	Alacrity index routines.
 */

/****************/
/* Module Setup */
/****************/

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions */
#include "H5Xprivate.h"     /* Index */
#include "H5Eprivate.h"		/* Error handling */
#include "H5Iprivate.h"		/* IDs */
#include "H5MMprivate.h"	/* Memory management */
#include "H5Pprivate.h"
#include "H5FFprivate.h"
#include "H5RCprivate.h"
#include "H5TRprivate.h"
#include "H5Qprivate.h"
#include "H5VMprivate.h"
#include "H5Sprivate.h"
/* TODO using private headers but could use public ones */

#ifdef H5_HAVE_FASTBIT
/**
 * Header file that defines an in-memory C API for accessing the querying
 * functionality of FastBit IBIS implementation.  It is primarily for
 * in memory data.
 */
#include <iapi.h>

/****************/
/* Local Macros */
/****************/
#define H5X_FASTBIT_DEBUG

#ifdef H5X_FASTBIT_DEBUG
#define H5X_FASTBIT_DEBUG_LVL 6
#define H5X_FASTBIT_LOG_DEBUG(...) do {                         \
      fprintf(stdout, " # %s(): ", __func__);                   \
      fprintf(stdout, __VA_ARGS__);                             \
      fprintf(stdout, "\n");                                    \
      fflush(stdout);                                           \
  } while (0)
#else
#define H5X_FASTBIT_DEBUG_LVL 0
#define H5X_FASTBIT_LOG_DEBUG
#endif

/******************/
/* Local Typedefs */
/******************/
typedef struct H5X_fastbit_t {
    void *private_metadata;     /* Internal metadata */

    hid_t file_id;              /* ID of the indexed dataset file */
    hid_t dataset_id;           /* ID of the indexed dataset */
    unsigned dataset_ndims;     /* dataset number of dimensions */
    hsize_t *dataset_dims;      /* dataset dimensions */
    hsize_t *dataset_down_dims; /* dataset downed dimensions */

    hid_t opaque_type_id;   /* Datatype used for index datasets */

    hid_t keys_id;          /* Array for keys */
    uint64_t nkeys;
    double *keys;

    hid_t offsets_id;       /* Array for offset */
    uint64_t noffsets;
    int64_t *offsets;

    hid_t bitmaps_id;       /* Array for bitmaps */
    uint64_t nbitmaps;
    uint32_t *bitmaps;

    char column_name[64];

    hbool_t idx_reconstructed;
} H5X_fastbit_t;

struct H5X_fastbit_scatter_info {
    void *src_buf;       /* Source data buffer */
    size_t src_buf_size; /* Remaining number of elements to return */
};

/********************/
/* Local Prototypes */
/********************/

static H5X_fastbit_t *
H5X__fastbit_init(hid_t file_id, hid_t dataset_id);

static herr_t
H5X__fastbit_term(H5X_fastbit_t *fastbit);

static enum FastBitDataType
H5X__fastbit_convert_type(hid_t type);

static herr_t
H5X__fastbit_read_data(hid_t dataset_id, hid_t rcxt_id, const char *column_name,
        void **buf, size_t *buf_size);

static herr_t
H5X__fastbit_create_index(H5X_fastbit_t *fastbit, hid_t file_id, hid_t trans_id);

static herr_t
H5X__fastbit_update_index(H5X_fastbit_t *fastbit, hid_t trans_id);

static herr_t
H5X__fastbit_serialize_metadata(H5X_fastbit_t *fastbit, void *buf,
        size_t *buf_size);

static herr_t
H5X__fastbit_deserialize_metadata(H5X_fastbit_t *fastbit, void *buf,
        hid_t trans_id);

static herr_t
H5X__fastbit_reconstruct_index(H5X_fastbit_t *fastbit, hid_t rcxt_id);

static double
H5X__fasbit_query_get_value_as_double(H5Q_t *query);

static herr_t
H5X__fastbit_evaluate_query(H5X_fastbit_t *fastbit, hid_t query_id,
        uint64_t **coords, uint64_t *ncoords);

static herr_t
H5X__fastbit_create_selection(H5X_fastbit_t *fastbit, hid_t dataspace_id,
        uint64_t *coords, uint64_t ncoords);

static void *
H5X_fastbit_create(hid_t file_id, hid_t dataset_id, hid_t xcpl_id,
        hid_t xapl_id, size_t *metadata_size, void **metadata);

static herr_t
H5X_fastbit_remove(hid_t file_id, hid_t dataset_id, size_t metadata_size,
        void *metadata);

static void *
H5X_fastbit_open(hid_t file_id, hid_t dataset_id, hid_t xapl_id,
        size_t metadata_size, void *metadata);

static herr_t
H5X_fastbit_close(void *idx_handle);

static herr_t
H5X_fastbit_pre_update(void *idx_handle, hid_t dataspace_id, hid_t xxpl_id);

static herr_t
H5X_fastbit_post_update(void *idx_handle, const void *buf, hid_t dataspace_id,
        hid_t xxpl_id);

static herr_t
H5X_fastbit_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
        hid_t *dataspace_id);

static herr_t
H5X_fastbit_refresh(void *idx_handle, size_t *metadata_size, void **metadata);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Alacrity index class */
const H5X_class_t H5X_FASTBIT[1] = {{
    H5X_CLASS_T_VERS,          /* (From the H5Xpublic.h header file) */
    H5X_PLUGIN_FASTBIT,        /* (Or whatever number is assigned) */
    "FASTBIT index plugin",    /* Whatever name desired */
    H5X_TYPE_DATA_ELEM,        /* This plugin operates on dataset elements */
    H5X_fastbit_create,        /* create */
    H5X_fastbit_remove,        /* remove */
    H5X_fastbit_open,          /* open */
    H5X_fastbit_close,         /* close */
    H5X_fastbit_pre_update,    /* pre_update */
    H5X_fastbit_post_update,   /* post_update */
    H5X_fastbit_query,         /* query */
    H5X_fastbit_refresh        /* refresh */
}};

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_init
 *
 * Purpose: Configure and set up and the FastBit encoder.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static H5X_fastbit_t *
H5X__fastbit_init(hid_t file_id, hid_t dataset_id)
{
    static int column_number = 0; /* TODO remove that once John fixes attach */
    H5X_fastbit_t *fastbit = NULL;
    hid_t space_id = FAIL;
    H5X_fastbit_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (fastbit = (H5X_fastbit_t *) H5MM_malloc(sizeof(H5X_fastbit_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate FastBit struct");
    fastbit->private_metadata = NULL;

    fastbit->file_id = file_id;
    fastbit->dataset_id = dataset_id;

    fastbit->keys_id = FAIL;
    fastbit->nkeys = 0;
    fastbit->keys = NULL;

    fastbit->offsets_id = FAIL;
    fastbit->noffsets = 0;
    fastbit->offsets = NULL;

    fastbit->nbitmaps = 0;
    fastbit->bitmaps = NULL;
    fastbit->bitmaps_id = FAIL;

    sprintf(fastbit->column_name, "test%d", column_number);
    column_number++;

    fastbit->idx_reconstructed = FALSE;

    /* Initialize FastBit (no config file for now) */
    fastbit_init(NULL);
    fastbit_set_verbose_level(H5X_FASTBIT_DEBUG_LVL);

    /* Get dimensions of dataset */
    if (FAIL == (space_id = H5Dget_space(dataset_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "can't get dataspace from dataset");
    if (0 == (fastbit->dataset_ndims = (unsigned) H5Sget_simple_extent_ndims(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, NULL, "invalid number of dimensions");
    if (NULL == (fastbit->dataset_dims = H5MM_malloc(fastbit->dataset_ndims * sizeof(hsize_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, NULL, "can't allocate dim array");
    if (FAIL == H5Sget_simple_extent_dims(space_id, fastbit->dataset_dims, NULL))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, NULL, "can't get dataspace dims");

    /* Useful for coordinate conversion */
    if (NULL == (fastbit->dataset_down_dims = H5MM_malloc(fastbit->dataset_ndims * sizeof(hsize_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, NULL, "can't allocate dim array");
    if (FAIL == H5VM_array_down(fastbit->dataset_ndims, fastbit->dataset_dims,
            fastbit->dataset_down_dims))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, NULL, "can't get dataspace down dims");

    /* Create an opaque type to handle FastBit data */
    if (FAIL == (fastbit->opaque_type_id = H5Tcreate(H5T_OPAQUE, 1)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, NULL, "can't create type");
    if (FAIL == H5Tset_tag(fastbit->opaque_type_id, "alacrity metadata type"))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTSET, NULL, "can't set tag to type");

    ret_value = fastbit;

done:
    if (space_id != FAIL)
        H5Sclose(space_id);
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_init() */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_term
 *
 * Purpose: Free plugin resources.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_term(H5X_fastbit_t *fastbit)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (!fastbit)
        HGOTO_DONE(SUCCEED);

    H5MM_free(fastbit->private_metadata);

    /* Free dim arrays */
    H5MM_free(fastbit->dataset_dims);
    H5MM_free(fastbit->dataset_down_dims);

    /* Free metadata if created */
    H5MM_free(fastbit->keys);
    H5MM_free(fastbit->offsets);
    H5MM_free(fastbit->bitmaps);

    /* Free FastBit resources */
    fastbit_cleanup();

    /* Close opaque type */
    if ((FAIL != fastbit->opaque_type_id) &&
            (FAIL == H5Tclose(fastbit->opaque_type_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close opaque type");

    /* Close anonymous datasets */
    if ((FAIL != fastbit->keys_id) &&
            (FAIL == H5Dclose_ff(fastbit->keys_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");
    if ((FAIL != fastbit->offsets_id) &&
            (FAIL == H5Dclose_ff(fastbit->offsets_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");
    if ((FAIL != fastbit->bitmaps_id) &&
            (FAIL == H5Dclose_ff(fastbit->bitmaps_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");

    H5MM_free(fastbit);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_term() */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_convert_type
 *
 * Purpose: Convert H5 type to FastBit type.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static FastBitDataType
H5X__fastbit_convert_type(hid_t type_id)
{
    FastBitDataType ret_value = FastBitDataTypeUnknown; /* Return value */
    hid_t native_type_id = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    if((native_type_id = H5Tget_native_type(type_id, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FastBitDataTypeUnknown, "can't get native type");

    if(H5Tequal(native_type_id, H5T_NATIVE_LLONG_g))
        ret_value = FastBitDataTypeULong;
    else if(H5Tequal(native_type_id, H5T_NATIVE_LONG_g))
        ret_value = FastBitDataTypeLong;
    else if(H5Tequal(native_type_id, H5T_NATIVE_INT_g))
        ret_value = FastBitDataTypeInt;
    else if(H5Tequal(native_type_id, H5T_NATIVE_SHORT_g))
        ret_value = FastBitDataTypeShort;
    else if(H5Tequal(native_type_id, H5T_NATIVE_SCHAR_g))
        ret_value = FastBitDataTypeByte;
    else if(H5Tequal(native_type_id, H5T_NATIVE_DOUBLE_g))
        ret_value = FastBitDataTypeDouble;
    else if(H5Tequal(native_type_id, H5T_NATIVE_FLOAT_g))
        ret_value = FastBitDataTypeFloat;
    else
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FastBitDataTypeUnknown, "unsupported type");

    H5X_FASTBIT_LOG_DEBUG("FastBit converted type: %d\n", ret_value);

done:
    if(FAIL == native_type_id)
        H5Tclose(native_type_id);
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_convert_type */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_read_data
 *
 * Purpose: Read data from dataset.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_read_data(hid_t dataset_id, hid_t rcxt_id, const char *column_name,
        void **buf, size_t *buf_size)
{
    herr_t ret_value = SUCCEED; /* Return value */
    hid_t type_id = FAIL, space_id = FAIL;
    size_t nelmts, elmt_size;
    void *data = NULL;
    size_t data_size;
    FastBitDataType fastbit_type;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get space info */
    if (FAIL == (type_id = H5Dget_type(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get type from dataset");
    if (FAIL == (space_id = H5Dget_space(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
    if (0 == (nelmts = (size_t) H5Sget_select_npoints(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    if (0 == (elmt_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid size of element");

    /* Allocate buffer to hold data */
    data_size = nelmts * elmt_size;
    if (NULL == (data = H5MM_malloc(data_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* Read data from dataset */
    if (FAIL == H5Dread_ff(dataset_id, type_id, H5S_ALL, space_id,
            H5P_DEFAULT, data, rcxt_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

    /* Convert type to FastBit type */
    if (FastBitDataTypeUnknown == (fastbit_type = H5X__fastbit_convert_type(type_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCONVERT, FAIL, "can't convert type");

    /* Register array */
    if (0 != fastbit_iapi_register_array(column_name, fastbit_type, data, nelmts))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREGISTER, FAIL, "can't register array");

    H5X_FASTBIT_LOG_DEBUG("Registered array %s", column_name);
    *buf = data;
    *buf_size = data_size;

done:
    if (type_id != FAIL)
        H5Tclose(type_id);
    if (space_id != FAIL)
        H5Sclose(space_id);
    if (ret_value == FAIL)
        H5MM_free(data);
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__fastbit_read_data() */

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_create_index
 *
 * Purpose: Create a new index from a dataset.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_create_index(H5X_fastbit_t *fastbit, hid_t file_id,
        hid_t trans_id)
{
    hid_t key_space_id = FAIL, offset_space_id = FAIL, bitmap_space_id = FAIL;
    hsize_t key_array_size, offset_array_size, bitmap_array_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_FASTBIT_LOG_DEBUG("Calling FastBit build index");

    /* Build index */
    if (0 != fastbit_iapi_build_index(fastbit->column_name, (const char *)0))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, FAIL, "FastBit build index failed");

    /* Get arrays from index */
    if (0 != fastbit_iapi_deconstruct_index(fastbit->column_name, &fastbit->keys, &fastbit->nkeys,
            &fastbit->offsets, &fastbit->noffsets, &fastbit->bitmaps, &fastbit->nbitmaps))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "Can't get FastBit index arrays");

    H5X_FASTBIT_LOG_DEBUG("FastBit build index, nkeys=%lu, noffsets=%lu, nbitmaps=%lu\n",
            fastbit->nkeys, fastbit->noffsets, fastbit->nbitmaps);

    /* Create key array with opaque type */
    key_array_size = fastbit->nkeys * sizeof(double);
    if (FAIL == (key_space_id = H5Screate_simple(1, &key_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->keys_id = H5Dcreate_anon_ff(file_id, fastbit->opaque_type_id,
            key_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Create offset array with opaque type */
    offset_array_size = fastbit->noffsets * sizeof(int64_t);
    if (FAIL == (offset_space_id = H5Screate_simple(1, &offset_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->offsets_id = H5Dcreate_anon_ff(file_id, fastbit->opaque_type_id,
            offset_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Create bitmap array with opaque type */
    bitmap_array_size = fastbit->nbitmaps * sizeof(uint32_t);
    if (FAIL == (bitmap_space_id = H5Screate_simple(1, &bitmap_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->bitmaps_id = H5Dcreate_anon_ff(file_id, fastbit->opaque_type_id,
            bitmap_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Write keys */
    if (FAIL == H5Dwrite_ff(fastbit->keys_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->keys, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    /* Write offsets */
    if (FAIL == H5Dwrite_ff(fastbit->offsets_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->offsets, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    /* Write bitmaps */
    if (FAIL == H5Dwrite_ff(fastbit->bitmaps_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->bitmaps, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

done:
    if (key_space_id != FAIL)
        H5Sclose(key_space_id);
    if (offset_space_id != FAIL)
        H5Sclose(offset_space_id);
    if (bitmap_space_id != FAIL)
        H5Sclose(bitmap_space_id);
    if (err_occurred) {
        H5MM_free(fastbit->keys);
        fastbit->keys = NULL;
        H5MM_free(fastbit->offsets);
        fastbit->offsets = NULL;
        H5MM_free(fastbit->bitmaps);
        fastbit->bitmaps = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_merge_data
 *
 * Purpose: Merge buffers.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_scatter_cb(const void **src_buf/*out*/, size_t *src_buf_bytes_used/*out*/,
        void *_info)
{
    struct H5X_fastbit_scatter_info *info = (struct H5X_fastbit_scatter_info *) _info;

    /* Set output variables */
    *src_buf = info->src_buf;
    *src_buf_bytes_used = info->src_buf_size;

    return SUCCEED;
}

static herr_t
H5X__fastbit_merge_data(H5X_fastbit_t *fastbit, const void *data,
        hid_t dataspace_id, void *buf)
{
    hid_t type_id = FAIL, space_id = FAIL;
    struct H5X_fastbit_scatter_info info;
    size_t nelmts_data, data_elmt_size;

    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    //    void *dest_data = H5MM_malloc(buf_size);
    //    if (FAIL == H5Dgather(space_id, data, type_id, buf_size, dest_data, NULL, NULL))
    //        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "cannot gather data");

    if (FAIL == (type_id = H5Dget_type(fastbit->dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get type from dataset");
    if (FAIL == (space_id = H5Dget_space(fastbit->dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
    if (0 == (nelmts_data = (size_t) H5Sget_select_npoints(dataspace_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    if (0 == (data_elmt_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid size of element");

    info.src_buf = data;
    info.src_buf_size = nelmts_data * data_elmt_size;

    if (FAIL == H5Dscatter(H5X__fastbit_scatter_cb, &info, type_id, dataspace_id, buf))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "cannot scatter data");

done:
    if (type_id != FAIL)
        H5Tclose(type_id);
    if (space_id != FAIL)
        H5Sclose(space_id);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_update_index
 *
 * Purpose: Update an existing index from a dataset.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_update_index(H5X_fastbit_t *fastbit, hid_t trans_id)
{
    hid_t key_space_id = FAIL, offset_space_id = FAIL, bitmap_space_id = FAIL;
    hsize_t key_array_size, offset_array_size, bitmap_array_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_FASTBIT_LOG_DEBUG("Calling FastBit build index");

    /* Build index */
    if (0 != fastbit_iapi_build_index(fastbit->column_name, (const char *)0))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, FAIL, "FastBit build index failed");

    /* Get arrays from index */
    if (0 != fastbit_iapi_deconstruct_index(fastbit->column_name, &fastbit->keys, &fastbit->nkeys,
            &fastbit->offsets, &fastbit->noffsets, &fastbit->bitmaps, &fastbit->nbitmaps))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "Can't get FastBit index arrays");

    H5X_FASTBIT_LOG_DEBUG("FastBit build index, nkeys=%lu, noffsets=%lu, nbitmaps=%lu\n",
            fastbit->nkeys, fastbit->noffsets, fastbit->nbitmaps);

    /* Create key array with opaque type */
    key_array_size = fastbit->nkeys * sizeof(double);
    H5Dclose_ff(fastbit->keys_id, H5_EVENT_STACK_NULL);
    if (FAIL == (key_space_id = H5Screate_simple(1, &key_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->keys_id = H5Dcreate_anon_ff(fastbit->file_id, fastbit->opaque_type_id,
            key_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Create offset array with opaque type */
    offset_array_size = fastbit->noffsets * sizeof(int64_t);
    H5Dclose_ff(fastbit->offsets_id, H5_EVENT_STACK_NULL);
    if (FAIL == (offset_space_id = H5Screate_simple(1, &offset_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->offsets_id = H5Dcreate_anon_ff(fastbit->file_id, fastbit->opaque_type_id,
            offset_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Create bitmap array with opaque type */
    bitmap_array_size = fastbit->nbitmaps * sizeof(uint32_t);
    H5Dclose_ff(fastbit->bitmaps_id, H5_EVENT_STACK_NULL);
    if (FAIL == (bitmap_space_id = H5Screate_simple(1, &bitmap_array_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (fastbit->bitmaps_id = H5Dcreate_anon_ff(fastbit->file_id, fastbit->opaque_type_id,
            bitmap_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Write keys */
    if (FAIL == H5Dwrite_ff(fastbit->keys_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->keys, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    /* Write offsets */
    if (FAIL == H5Dwrite_ff(fastbit->offsets_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->offsets, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

    /* Write bitmaps */
    if (FAIL == H5Dwrite_ff(fastbit->bitmaps_id, fastbit->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, fastbit->bitmaps, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");

done:
    if (err_occurred) {
        H5MM_free(fastbit->keys);
        fastbit->keys = NULL;
        H5MM_free(fastbit->offsets);
        fastbit->offsets = NULL;
        H5MM_free(fastbit->bitmaps);
        fastbit->bitmaps = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_serialize_metadata
 *
 * Purpose: Serialize index plugin metadata into buffer.
 * NB. This is not FastBit metadata but only H5X plugin private metadata.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_serialize_metadata(H5X_fastbit_t *fastbit, void *buf,
        size_t *buf_size)
{
    size_t keys_token_size, offsets_token_size, bitmaps_token_size;
    size_t alacrity_metadata_size;

    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get token sizes */
    if (FAIL == H5Oget_token(fastbit->keys_id, NULL, &keys_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token size for anonymous dataset");
    if (FAIL == H5Oget_token(fastbit->offsets_id, NULL, &offsets_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token size for anonymous dataset");
    if (FAIL == H5Oget_token(fastbit->bitmaps_id, NULL, &bitmaps_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token size for anonymous dataset");

    /* Make some space for the metadata (tokens + sizes) */
    alacrity_metadata_size = keys_token_size + offsets_token_size + bitmaps_token_size
            + 3 * sizeof(size_t);

    if (buf) {
        char *buf_ptr = buf;

        /* Encode metadata token info */
        HDmemcpy(buf_ptr, &keys_token_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if (FAIL == H5Oget_token(fastbit->keys_id, buf_ptr, &keys_token_size))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token for anonymous dataset");
        buf_ptr += keys_token_size;

        /* Encode index token info */
        HDmemcpy(buf_ptr, &offsets_token_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if (FAIL == H5Oget_token(fastbit->offsets_id, buf_ptr, &offsets_token_size))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token for anonymous dataset");
        buf_ptr += offsets_token_size;

        /* Encode index token info */
        HDmemcpy(buf_ptr, &bitmaps_token_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if (FAIL == H5Oget_token(fastbit->bitmaps_id, buf_ptr, &bitmaps_token_size))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token for anonymous dataset");
        buf_ptr += bitmaps_token_size;
    }

    if (buf_size) *buf_size = alacrity_metadata_size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_deserialize_metadata
 *
 * Purpose: Deserialize index plugin metadata from buffer.
 * NB. This is not FastBit metadata but only H5X plugin private metadata.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_deserialize_metadata(H5X_fastbit_t *fastbit, void *buf,
        hid_t trans_id)
{
    char *buf_ptr = buf;
    size_t keys_token_size, offsets_token_size, bitmaps_token_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Decode metadata token info */
    HDmemcpy(&keys_token_size, buf_ptr, sizeof(size_t));
    buf_ptr += sizeof(size_t);
    if (FAIL == (fastbit->keys_id = H5Oopen_by_token(buf_ptr,
            trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += keys_token_size;

    /* Decode index token info */
    HDmemcpy(&offsets_token_size, buf_ptr, sizeof(size_t));
    buf_ptr += sizeof(size_t);
    if (FAIL == (fastbit->offsets_id = H5Oopen_by_token(buf_ptr,
            trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += offsets_token_size;

    /* Decode index token info */
    HDmemcpy(&bitmaps_token_size, buf_ptr, sizeof(size_t));
    buf_ptr += sizeof(size_t);
    if (FAIL == (fastbit->bitmaps_id = H5Oopen_by_token(buf_ptr,
            trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += bitmaps_token_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_reconstruct_index
 *
 * Purpose: Reconstruct FastBit index
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_reconstruct_index(H5X_fastbit_t *fastbit, hid_t rcxt_id)
{
    void *buf;
    size_t buf_size;
    hid_t keys_space_id = FAIL, offsets_space_id = FAIL, bitmaps_space_id = FAIL;
    size_t key_array_size, offset_array_size, bitmap_array_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* TODO don't read keys and offsets if already present */
    HDassert(!fastbit->keys);
    HDassert(!fastbit->offsets);

    if (FAIL == (keys_space_id = H5Dget_space(fastbit->keys_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
    if (0 == (key_array_size = (size_t) H5Sget_select_npoints(keys_space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    fastbit->nkeys = key_array_size / sizeof(double);

    if (FAIL == (offsets_space_id = H5Dget_space(fastbit->offsets_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
    if (0 == (offset_array_size = (size_t) H5Sget_select_npoints(offsets_space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    fastbit->noffsets = offset_array_size / sizeof(int64_t);

    if (FAIL == (bitmaps_space_id = H5Dget_space(fastbit->bitmaps_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
    if (0 == (bitmap_array_size = (size_t) H5Sget_select_npoints(bitmaps_space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    fastbit->nbitmaps = bitmap_array_size / sizeof(uint32_t);

    /* allocate buffer to hold data */
    if (NULL == (fastbit->keys = H5MM_malloc(key_array_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate keys");
    if (NULL == (fastbit->offsets = H5MM_malloc(offset_array_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate offsets");
    if (NULL == (fastbit->bitmaps = H5MM_malloc(bitmap_array_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate offsets");

    /* Read FastBit keys */
    if (FAIL == H5Dread_ff(fastbit->keys_id, fastbit->opaque_type_id,
            H5S_ALL, keys_space_id, H5P_DEFAULT, fastbit->keys, rcxt_id,
            H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

    /* Read FastBit offsets */
    if (FAIL == H5Dread_ff(fastbit->offsets_id, fastbit->opaque_type_id,
            H5S_ALL, offsets_space_id, H5P_DEFAULT, fastbit->offsets, rcxt_id,
            H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

    /* Read FastBit bitmaps */
    if (FAIL == H5Dread_ff(fastbit->bitmaps_id, fastbit->opaque_type_id,
            H5S_ALL, bitmaps_space_id, H5P_DEFAULT, fastbit->bitmaps, rcxt_id,
            H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

    /* Reconstruct index */
    H5X_FASTBIT_LOG_DEBUG("Reconstructing index with nkeys=%lu, noffsets=%lu,"
            "nbitmaps=%lu", fastbit->nkeys, fastbit->noffsets, fastbit->nbitmaps);

    /* Get data from dataset */
    if (FAIL == H5X__fastbit_read_data(fastbit->dataset_id, rcxt_id,
            fastbit->column_name, &buf, &buf_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get data from dataset");

    if (0 != fastbit_iapi_attach_full_index(fastbit->column_name, fastbit->keys, fastbit->nkeys,
            fastbit->offsets, fastbit->noffsets, fastbit->bitmaps, fastbit->nbitmaps))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDECODE, FAIL, "can't reconstruct index");

    H5X_FASTBIT_LOG_DEBUG("Reconstructed index");
    fastbit->idx_reconstructed = TRUE;

done:
    if (FAIL != keys_space_id)
        H5Sclose(keys_space_id);
    if (FAIL != offsets_space_id)
        H5Sclose(offsets_space_id);
    if (FAIL != bitmaps_space_id)
        H5Sclose(bitmaps_space_id);
    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fasbit_query_get_value_as_double
 *
 * Purpose: Convert query element value to double that can be used by
 *          FastBit selection.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static double
H5X__fasbit_query_get_value_as_double(H5Q_t *query)
{
    H5T_t *type = query->query.select.elem.data_elem.type;
    double ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_LLONG_g), FALSE)) {
        ret_value = (double) *((long long *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_LONG_g), FALSE)) {
        ret_value = (double) *((long *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_INT_g), FALSE)) {
        ret_value = (double) *((int *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_SHORT_g), FALSE)) {
        ret_value = (double) *((short *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_SCHAR_g), FALSE)) {
        ret_value = (double) *((char *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_DOUBLE_g), FALSE)) {
        ret_value = *((double *) query->query.select.elem.data_elem.value);
    }
    else if (0 == H5T_cmp(type, (H5T_t *)H5I_object(H5T_NATIVE_FLOAT_g), FALSE)) {
        ret_value = (double) *((float *) query->query.select.elem.data_elem.value);
    }

    H5X_FASTBIT_LOG_DEBUG("Query element value is: %lf", ret_value);

    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_evaluate_query
 *
 * Purpose: Evaluate query and get matching coordinates.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_evaluate_query(H5X_fastbit_t *fastbit, hid_t query_id,
        uint64_t **coords, uint64_t *ncoords)
{
    H5Q_t *query;
    double cv;
    FastBitSelectionHandle selection_handle;
    FastBitCompareType compare_type;
    int64_t nhits;
    uint64_t *hit_coords = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (query->is_combined)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "combined queries are not supported");

    /* Translate match op to FastBit op */
    switch(query->query.select.match_op) {
        case H5Q_MATCH_GREATER_THAN:
            compare_type = FastBitCompareGreater;
            break;
        case H5Q_MATCH_LESS_THAN:
            compare_type = FastBitCompareLess;
            break;
        case H5Q_MATCH_EQUAL:
            compare_type = FastBitCompareEqual;
            break;
        case H5Q_MATCH_NOT_EQUAL:
            compare_type = FastBitCompareNotEqual;
            break;
        default:
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "unsupported query op");
    }

    cv = H5X__fasbit_query_get_value_as_double(query);
    selection_handle = fastbit_selection_osr(fastbit->column_name, compare_type, cv);

    H5X_FASTBIT_LOG_DEBUG("Selection estimate: %lu",
            fastbit_selection_estimate(selection_handle));

    if (0 > (nhits = fastbit_selection_evaluate(selection_handle)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCOMPUTE, FAIL, "can't evaluate selection");

    if (NULL == (hit_coords = H5MM_malloc(((size_t) nhits) * sizeof(uint64_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate hit coordinates");

    if (0 > fastbit_selection_get_coordinates(selection_handle, hit_coords,
            (uint64_t) nhits, 0U))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get coordinates");

#ifdef H5X_FASTBIT_DEBUG
    /* TODO remove debug */
    {
        int i;
        printf(" # %s(): Index read contains following coords: ",  __func__);
        for (i = 0; i < nhits; i++) {
            printf("%lu ", hit_coords[i]);
        }
        printf("\n");
    }
#endif

    *coords = hit_coords;
    *ncoords = (uint64_t) nhits;

done:
    if (selection_handle)
        fastbit_selection_free(selection_handle);
    if (FAIL == ret_value) {
        H5MM_free(hit_coords);
    }
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__fastbit_create_selection
 *
 * Purpose: Create dataspace selection from coordinates.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__fastbit_create_selection(H5X_fastbit_t *fastbit, hid_t dataspace_id,
        uint64_t *coords, uint64_t ncoords)
{
    hsize_t count[H5S_MAX_RANK + 1];
    hsize_t start_coord[H5S_MAX_RANK + 1], end_coord[H5S_MAX_RANK + 1], nelmts;
    herr_t ret_value = SUCCEED; /* Return value */
    size_t i;

    FUNC_ENTER_NOAPI_NOINIT

    /* Initialize count */
    for (i = 0; i < H5S_MAX_RANK; i++)
        count[i] = 1;

    for (i = 0; i < ncoords; i++) {
        hsize_t new_coords[H5S_MAX_RANK + 1];
        const hsize_t point = coords[i];

        /* Convert coordinates */
        if (FAIL == H5VM_array_calc_pre(point, fastbit->dataset_ndims,
                fastbit->dataset_down_dims, new_coords))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate coord array");

        /* Add converted coordinate to selection */
        if (H5Sselect_hyperslab(dataspace_id, H5S_SELECT_OR, new_coords, NULL, count, NULL))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to add point to selection");
    }

    if (FAIL == H5Sget_select_bounds(dataspace_id, start_coord, end_coord))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to get bounds");
    if (0 == (nelmts = (hsize_t) H5Sget_select_npoints(dataspace_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    H5X_FASTBIT_LOG_DEBUG("Created dataspace from index with %llu elements [(%llu, %llu):(%llu, %llu)]",
            nelmts, start_coord[0], start_coord[1], end_coord[0], end_coord[1]);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_create
 *
 * Purpose: This function creates a new instance of a FastBit plugin index.
 *
 * Return:  Success:    Pointer to the new index
 *          Failure:    NULL
 *
 *------------------------------------------------------------------------
 */
static void *
H5X_fastbit_create(hid_t file_id, hid_t dataset_id, hid_t H5_ATTR_UNUSED xcpl_id,
        hid_t xapl_id, size_t *metadata_size, void **metadata)
{
    H5X_fastbit_t *fastbit = NULL;
    void *ret_value = NULL; /* Return value */
    hid_t trans_id = FAIL, rcxt_id = FAIL;
    size_t private_metadata_size;
    void *buf = NULL;
    size_t buf_size;
    uint64_t version;

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    /* Initialize FastBit plugin */
    if (NULL == (fastbit = H5X__fastbit_init(file_id, dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, NULL, "can't initialize FastBit");

    /* Get transaction ID from xapl */
    if (FAIL == H5Pget_xapl_transaction(xapl_id, &trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get trans_id from xapl");

    /* Create read context from version */
    if (FAIL == H5TRget_version(trans_id, &version))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get version from transaction ID");
    if (FAIL == (rcxt_id =  H5RCcreate(file_id, version)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create read context");

    /* Get data from dataset */
    if (FAIL == H5X__fastbit_read_data(dataset_id, rcxt_id,
            fastbit->column_name, &buf, &buf_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get data from dataset");

    /* Index data */
    if (FAIL == H5X__fastbit_create_index(fastbit, file_id, trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create index data from dataset");

    /* Serialize metadata for H5X interface */
    if (FAIL == H5X__fastbit_serialize_metadata(fastbit, NULL,
            &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get plugin metadata size");

    if (NULL == (fastbit->private_metadata = H5MM_malloc(private_metadata_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate plugin metadata");

    /* Serialize plugin metadata */
    if (FAIL == H5X__fastbit_serialize_metadata(fastbit, fastbit->private_metadata,
            &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, NULL, "can't serialize plugin metadata");

    /* Metadata is token for anonymous dataset */
    *metadata = fastbit->private_metadata;
    *metadata_size = private_metadata_size;

    ret_value = fastbit;

done:
    if (FAIL != rcxt_id)
        H5RCclose(rcxt_id);
    H5MM_free(buf);
    if (NULL == ret_value)
        H5X__fastbit_term(fastbit);
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_remove
 *
 * Purpose: This function removes the FastBit plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_remove(hid_t H5_ATTR_UNUSED file_id, hid_t H5_ATTR_UNUSED dataset_id,
        size_t H5_ATTR_UNUSED metadata_size, void H5_ATTR_UNUSED *metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR
    H5X_FASTBIT_LOG_DEBUG("Enter");

    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_open
 *
 * Purpose: This function opens an already existing FastBit index from a file.
 *
 * Return:  Success:    Pointer to the index
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5X_fastbit_open(hid_t file_id, hid_t dataset_id, hid_t xapl_id,
        size_t metadata_size, void *metadata)
{
    H5X_fastbit_t *fastbit = NULL;
    hid_t trans_id = FAIL, rcxt_id = FAIL;
    uint64_t c_version;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata");

    /* Get read context from xapl */
    if (FAIL == H5Pget_xapl_read_context(xapl_id, &rcxt_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get rc_id from xapl");

    /* Initialize FastBit plugin */
    if (NULL == (fastbit = H5X__fastbit_init(file_id, dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, NULL, "can't initialize FastBit");

    /* Create transaction from version (for open_by_token) */
    if (FAIL == H5RCget_version(rcxt_id, &c_version))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get version from read context ID");
    if (FAIL == (trans_id = H5TRcreate(file_id, rcxt_id, c_version)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create transaction");

    /* Deserialize plugin metadata */
    if (FAIL == H5X__fastbit_deserialize_metadata(fastbit, metadata, trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDECODE, NULL, "can't deserialize plugin metadata");

    ret_value = fastbit;

done:
    if (FAIL != trans_id)
        H5TRclose(trans_id);
    if (NULL == ret_value)
        H5X__fastbit_term(fastbit);
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_close
 *
 * Purpose: This function closes an FastBit index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_close(void *idx_handle)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    if (FAIL == H5X__fastbit_term(fastbit))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTFREE, FAIL, "Cannot terminate FastBit");

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_close() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_pre_update
 *
 * Purpose: This function does a pre_update of indexing information.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_pre_update(void *idx_handle, hid_t H5_ATTR_UNUSED dataspace_id, hid_t H5_ATTR_UNUSED xxpl_id)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_pre_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_post_update
 *
 * Purpose: This function does a post_update of indexing information.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_post_update(void *idx_handle, const void *data,
        hid_t dataspace_id, hid_t xxpl_id)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */
    hid_t trans_id = FAIL, rcxt_id = FAIL;
    uint64_t version;
    void *buf;
    size_t buf_size;

    FUNC_ENTER_NOAPI_NOINIT

    H5X_FASTBIT_LOG_DEBUG("Calling H5X_fastbit_post_update");

    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* Get transaction ID from xxpl */
    if (FAIL == H5Pget_xxpl_transaction(xxpl_id, &trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get trans_id from xxpl");

    /* Create read context from version */
    if (FAIL == H5TRget_version(trans_id, &version))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get version from transaction ID");
    if (FAIL == (rcxt_id =  H5RCcreate(fastbit->file_id, version)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create read context");

    /* Get data from dataset */
    if (FAIL == H5X__fastbit_read_data(fastbit->dataset_id, rcxt_id,
            fastbit->column_name, &buf, &buf_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get data from dataset");

    /* Merge old and new data */
    if (FAIL == H5X__fastbit_merge_data(fastbit, data, dataspace_id, buf))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "can't merge data");

    /* Update index */
    if (FAIL == H5X__fastbit_update_index(fastbit, trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create index data from dataset");

done:
    if (FAIL != rcxt_id)
        H5RCclose(rcxt_id);
    H5MM_free(buf);
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_post_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_query
 *
 * Purpose: This function retrieves indexing information that matches
 * the query and returns results under the form of a dataspace ID.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
        hid_t *dataspace_id)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    hid_t rcxt_id;
    hid_t space_id = FAIL, ret_space_id = FAIL;
    uint64_t *coords = NULL;
    uint64_t ncoords = 0;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* Get read context ID from xxpl */
    if (FAIL == H5Pget_xxpl_read_context(xxpl_id, &rcxt_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get rcxt_id from xxpl");

    /* If metadata has not been read already, read it */
    if (!fastbit->idx_reconstructed && (FAIL == H5X__fastbit_reconstruct_index(fastbit, rcxt_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read FastBit metadata");

    /* Create a copy of the original dataspace */
    if (FAIL == (space_id = H5Dget_space(fastbit->dataset_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
    if (FAIL == (ret_space_id = H5Scopy(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy dataspace");
    if (FAIL == H5Sselect_none(ret_space_id))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't reset selection");

    /* Get matching coordinates values from query */
    if (FAIL == H5X__fastbit_evaluate_query(fastbit, query_id, &coords, &ncoords))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get query range");

    /* Create selection */
    if (FAIL == H5X__fastbit_create_selection(fastbit, ret_space_id,
            coords, ncoords))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get query range");

    *dataspace_id = ret_space_id;

done:
    if (FAIL != space_id)
        H5Sclose(space_id);
    if ((FAIL == ret_value) && (FAIL != ret_space_id))
        H5Sclose(ret_space_id);
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_query() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_refresh
 *
 * Purpose: This function refreshes metadata.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_refresh(void *idx_handle, size_t *metadata_size, void **metadata)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    size_t private_metadata_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

    if (fastbit->private_metadata)
        H5MM_free(fastbit->private_metadata);

    /* Serialize metadata for H5X interface */
    if (FAIL == H5X__fastbit_serialize_metadata(fastbit, NULL,
            &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get plugin metadata size");

    if (NULL == (fastbit->private_metadata = H5MM_malloc(private_metadata_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate plugin metadata");

    /* Serialize plugin metadata */
    if (FAIL == H5X__fastbit_serialize_metadata(fastbit, fastbit->private_metadata,
            &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, FAIL, "can't serialize plugin metadata");

    /* Metadata is token for anonymous dataset */
    *metadata = fastbit->private_metadata;
    *metadata_size = private_metadata_size;

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
}

#endif /* H5_HAVE_FASTBIT */
