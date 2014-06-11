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
/* TODO using private headers but could use public ones */

#include <alacrity.h>

/****************/
/* Local Macros */
/****************/
#define H5X_ALACRITY_DEBUG

#ifdef H5X_ALACRITY_DEBUG
#define H5X_ALACRITY_LOG_DEBUG(...) do {                        \
      fprintf(stdout, " # %s(): ", __func__);                   \
      fprintf(stdout, __VA_ARGS__);                             \
      fprintf(stdout, "\n");                                    \
      fflush(stdout);                                           \
  } while (0)
#else
#define H5X_ALACRITY_LOG_DEBUG
#endif

/******************/
/* Local Typedefs */
/******************/
typedef struct H5X_alacrity_t {
    void *private_metadata;     /* Internal metadata */

    hid_t dataset_id;           /* ID of the indexed dataset */
    unsigned dataset_ndims;     /* dataset number of dimensions */
    hsize_t *dataset_dims;      /* dataset dimensions */
    hsize_t *dataset_down_dims; /* dataset downed dimensions */

    ALMetadata *metadata;       /* Alacrity metadata */
    ALEncoderConfig config;     /* Alacrity config */
    ALPartitionData *output;    /* Alacrity output */
    hid_t opaque_type_id;       /* Datatype used for index datasets */
    hid_t metadata_id;          /* Array for metadata */
    hid_t index_id;             /* Array for index data */
} H5X_alacrity_t;

typedef struct H5X_alacrity_range_t {
    value_types_t lb;   /* Lower bound */
    value_types_t ub;   /* Upper bound */
} H5X_alacrity_range_t;

/********************/
/* Local Prototypes */
/********************/

static H5X_alacrity_t *
H5X__alacrity_init(hid_t dataset_id);

static herr_t
H5X__alacrity_term(H5X_alacrity_t *alacrity);

static herr_t
H5X__alacrity_read_data(hid_t dataset_id, hid_t rcxt_id, void **buf,
        size_t *buf_size);

static herr_t
H5X__alacrity_create_index(H5X_alacrity_t *alacrity, hid_t file_id,
        hid_t trans_id, const void *buf, size_t buf_size);

static herr_t
H5X__alacrity_serialize_metadata(H5X_alacrity_t *alacrity, void *buf,
        size_t *buf_size);

static herr_t
H5X__alacrity_deserialize_metadata(H5X_alacrity_t *alacrity, void *buf,
        hid_t trans_id);

static herr_t
H5X__alacrity_get_query_value(H5Q_t *query, value_types_t *value);

static herr_t
H5X__alacrity_get_query_ranges(hid_t query_id,
        H5X_alacrity_range_t *query_ranges, size_t *nranges);

static hbool_t
H5X__alacrity_find_bin_range(ALMetadata *metadata, value_types_t query_lb,
        value_types_t query_ub, bin_id_t *start_bin, bin_id_t *end_bin);

static herr_t
H5X__alacrity_read_metadata(H5X_alacrity_t *alacrity, hid_t rcxt_id);

static herr_t
H5X__alacrity_read_index(H5X_alacrity_t *alacrity, bin_id_t start_bin,
        bin_id_t end_bin, hid_t rcxt_id, ALIndex *al_index,
        size_t *al_index_size);

static herr_t
H5X__alacrity_query_range(H5X_alacrity_t *alacrity, hid_t dataspace_id,
        H5X_alacrity_range_t query_range, hid_t rcxt_id);

static void *
H5X_alacrity_create(hid_t file_id, hid_t dataset_id, hid_t xcpl_id,
        hid_t xapl_id, size_t *metadata_size, void **metadata);

static herr_t
H5X_alacrity_remove(hid_t file_id, hid_t dataset_id, size_t metadata_size,
        void *metadata);

static void *
H5X_alacrity_open(hid_t file_id, hid_t dataset_id, hid_t xapl_id,
        size_t metadata_size, void *metadata);

static herr_t
H5X_alacrity_close(void *idx_handle);

static herr_t
H5X_alacrity_pre_update(void *idx_handle, hid_t dataspace_id, hid_t xxpl_id);

static herr_t
H5X_alacrity_post_update(void *idx_handle, const void *buf, hid_t dataspace_id,
        hid_t xxpl_id);

static herr_t
H5X_alacrity_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
        hid_t *dataspace_id);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*
 * ALACRITY library private routine.
 *
 * Finds all bins that intersect the given query range, assuming the elements
 * are one's-complement-based (float, double). Both query bounds are void*
 * pointers because their type size differs depending on the element size
 * (they must both be of size exactly meta->elementSize).
 */
extern _Bool findBinRange1C(const ALMetadata *meta, ALUnivariateQuery *query,
        bin_id_t *start_bin, bin_id_t *end_bin);

/*******************/
/* Local Variables */
/*******************/

/* Alacrity index class */
const H5X_class_t H5X_ALACRITY[1] = {{
    H5X_CLASS_T_VERS,          /* (From the H5Xpublic.h header file) */
    H5X_PLUGIN_ALACRITY,       /* (Or whatever number is assigned) */
    "ALACRITY index plugin",   /* Whatever name desired */
    H5X_TYPE_DATA_ELEM,        /* This plugin operates on dataset elements */
    H5X_alacrity_create,       /* create */
    H5X_alacrity_remove,       /* remove */
    H5X_alacrity_open,         /* open */
    H5X_alacrity_close,        /* close */
    H5X_alacrity_pre_update,   /* pre_update */
    H5X_alacrity_post_update,  /* post_update */
    H5X_alacrity_query         /* query */
}};

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_init
 *
 * Purpose: Configure and set up and the ALACRITY encoder.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static H5X_alacrity_t *
H5X__alacrity_init(hid_t dataset_id)
{
    H5X_alacrity_t *alacrity = NULL;
    hid_t type_id = FAIL, space_id = FAIL;
    size_t type_size;
    ALDatatype al_type;
    H5X_alacrity_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (alacrity = (H5X_alacrity_t *) H5MM_malloc(sizeof(H5X_alacrity_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate alacrity struct");
    alacrity->private_metadata = NULL;
    alacrity->dataset_id = dataset_id;

    /* Get dimensions of dataset */
    if (FAIL == (space_id = H5Dget_space(dataset_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "can't get dataspace from dataset");
    if (0 == (alacrity->dataset_ndims = (unsigned) H5Sget_simple_extent_ndims(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, NULL, "invalid number of dimensions");
    if (NULL == (alacrity->dataset_dims = H5MM_malloc(alacrity->dataset_ndims * sizeof(hsize_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, NULL, "can't allocate dim array");
    if (FAIL == H5Sget_simple_extent_dims(space_id, alacrity->dataset_dims, NULL))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, NULL, "can't get dataspace dims");

    /* Useful for coordinate conversion */
    if (NULL == (alacrity->dataset_down_dims = H5MM_malloc(alacrity->dataset_ndims * sizeof(hsize_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, NULL, "can't allocate dim array");
    if (FAIL == H5VM_array_down(alacrity->dataset_ndims, alacrity->dataset_dims,
            alacrity->dataset_down_dims))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, NULL, "can't get dataspace down dims");

    alacrity->metadata = NULL;
    alacrity->output = NULL;

    if (FAIL == (type_id = H5Dget_type(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get type from dataset");
    if (0 == (type_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get type size");

    al_type = (type_size == 4) ? DATATYPE_FLOAT32 : DATATYPE_FLOAT64;

    if (ALErrorNone != ALEncoderConfigure(&alacrity->config, 16, al_type,
            ALInvertedIndex))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, NULL, "can't configure ALACRITY encoder");

    /* Create an opaque type to handle ALACRITY data */
    if (FAIL == (alacrity->opaque_type_id = H5Tcreate(H5T_OPAQUE, 1)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, NULL, "can't create type");
    if (FAIL == H5Tset_tag(alacrity->opaque_type_id, "alacrity metadata type"))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTSET, NULL, "can't set tag to type");

    alacrity->metadata_id = FAIL;
    alacrity->index_id = FAIL;

    ret_value = alacrity;

done:
    if (type_id != FAIL)
        H5Tclose(type_id);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_term
 *
 * Purpose: Free plugin resources.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_term(H5X_alacrity_t *alacrity)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (!alacrity)
        HGOTO_DONE(SUCCEED);

    H5MM_free(alacrity->private_metadata);

    /* Free dim arrays */
    H5MM_free(alacrity->dataset_dims);
    H5MM_free(alacrity->dataset_down_dims);

    /* Free metadata if created */
    if (alacrity->metadata) {
        H5MM_free(alacrity->metadata->binLayout.binStartOffsets);
        H5MM_free(alacrity->metadata->binLayout.binValues);
        H5MM_free(alacrity->metadata);
    }

    /* Free output if created */
    if (alacrity->output) {
        ALPartitionDataDestroy(alacrity->output);
        H5MM_free(alacrity->output);
    }

    /* Close opaque type */
    if ((FAIL != alacrity->opaque_type_id) &&
            (FAIL == H5Tclose(alacrity->opaque_type_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close opaque type");

    /* Close anonymous dataset */
    if ((FAIL != alacrity->metadata_id) &&
            (FAIL == H5Dclose_ff(alacrity->metadata_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");
    if ((FAIL != alacrity->index_id) &&
            (FAIL == H5Dclose_ff(alacrity->index_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");

    H5MM_free(alacrity);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_read_data
 *
 * Purpose: Read data from dataset.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_read_data(hid_t dataset_id, hid_t rcxt_id, void **buf,
        size_t *buf_size)
{
    herr_t ret_value = SUCCEED; /* Return value */
    hid_t type_id = FAIL, space_id = FAIL;
    size_t nelmts, elmt_size;
    void *data = NULL;
    size_t data_size;

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
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_create_index
 *
 * Purpose: Create a new index from a dataset.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_create_index(H5X_alacrity_t *alacrity, hid_t file_id,
        hid_t trans_id, const void *buf, size_t buf_size)
{
    hid_t metadata_space_id = FAIL, index_space_id = FAIL;
    hsize_t metadata_size, index_size;
    memstream_t memstream;
    size_t nelmts;
    void *metadata_buf;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Allocate PartitionData struct */
    if (NULL == (alacrity->output = H5MM_malloc(sizeof(ALPartitionData))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate ALACRITY output");

    /* Get number of elements */
    nelmts = buf_size / ((size_t) alacrity->config.elementSize);

    /* Call ALACRITY encoder */
    H5X_ALACRITY_LOG_DEBUG("Calling ALACRITY encoder on data (%zu elements)", nelmts);

    if (ALErrorNone != ALEncode(&alacrity->config, buf, nelmts, alacrity->output))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, FAIL, "ALACRITY encoder failed");

    /* Get sizes */
    if (0 == (metadata_size = ALGetMetadataSize(&alacrity->output->metadata)))
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "ALACRITY metadata size is NULL");
    if (0 == (index_size = ALGetIndexSize(&alacrity->output->index,
            &alacrity->output->metadata)))
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "ALACRITY index size is NULL");

    H5X_ALACRITY_LOG_DEBUG("Metadata size: %zu", (size_t) metadata_size);
    H5X_ALACRITY_LOG_DEBUG("Index size: %zu", (size_t) index_size);

    /* Create metadata array with opaque type */
    if (FAIL == (metadata_space_id = H5Screate_simple(1, &metadata_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (alacrity->metadata_id = H5Dcreate_anon_ff(file_id, alacrity->opaque_type_id,
            metadata_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Create index array with opaque type */
    if (FAIL == (index_space_id = H5Screate_simple(1, &index_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");
    if (FAIL == (alacrity->index_id = H5Dcreate_anon_ff(file_id, alacrity->opaque_type_id,
            index_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Serialize and write ALACRITY metadata */
    if (NULL == (metadata_buf = H5MM_malloc(metadata_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate metadata buffer");
    memstreamInit(&memstream, metadata_buf);
    if (ALErrorNone != ALSerializeMetadata(&alacrity->output->metadata, &memstream))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSERIALIZE, FAIL, "can't serialize ALACRITY metadata");
    if (FAIL == H5Dwrite_ff(alacrity->metadata_id, alacrity->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, memstream.buf, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index metadata");
    memstreamDestroy(&memstream, 0);

    /* Write ALACRITY index */
    if (FAIL == H5Dwrite_ff(alacrity->index_id, alacrity->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, alacrity->output->index, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't write index data");

done:
    H5MM_free(metadata_buf);
    if (metadata_space_id != FAIL)
        H5Sclose(metadata_space_id);
    if (index_space_id != FAIL)
        H5Sclose(index_space_id);
    if (err_occurred) {
        H5MM_free(alacrity->output);
        alacrity->output = NULL;
    }
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_serialize_metadata
 *
 * Purpose: Serialize index plugin metadata into buffer.
 * NB. This is not ALACRITY metadata but only H5X plugin private metadata.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_serialize_metadata(H5X_alacrity_t *alacrity, void *buf,
        size_t *buf_size)
{
    size_t metadata_token_size, index_token_size;
    size_t alacrity_metadata_size;

    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get token sizes */
    if (FAIL == H5Oget_token(alacrity->metadata_id, NULL, &metadata_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token size for anonymous dataset");
    if (FAIL == H5Oget_token(alacrity->index_id, NULL, &index_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token size for anonymous dataset");

    /* Make some space for the metadata (tokens + sizes) */
    alacrity_metadata_size = metadata_token_size + index_token_size
            + 2 * sizeof(size_t);

    if (buf) {
        char *buf_ptr = buf;

        /* Encode metadata token info */
        HDmemcpy(buf_ptr, &metadata_token_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if (FAIL == H5Oget_token(alacrity->metadata_id, buf_ptr, &metadata_token_size))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token for anonymous dataset");
        buf_ptr += metadata_token_size;

        /* Encode index token info */
        HDmemcpy(buf_ptr, &index_token_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if (FAIL == H5Oget_token(alacrity->index_id, buf_ptr, &index_token_size))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token for anonymous dataset");
        buf_ptr += index_token_size;
    }

    if (buf_size) *buf_size = alacrity_metadata_size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_deserialize_metadata
 *
 * Purpose: Deserialize index plugin metadata from buffer.
 * NB. This is not ALACRITY metadata but only H5X plugin private metadata.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_deserialize_metadata(H5X_alacrity_t *alacrity, void *buf,
        hid_t trans_id)
{
    char *buf_ptr = buf;
    size_t metadata_token_size, index_token_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Decode metadata token info */
    HDmemcpy(&metadata_token_size, buf_ptr, sizeof(size_t));
    buf_ptr += sizeof(size_t);
    if (FAIL == (alacrity->metadata_id = H5Oopen_by_token(buf_ptr,
            trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += metadata_token_size;

    /* Decode index token info */
    HDmemcpy(&index_token_size, buf_ptr, sizeof(size_t));
    buf_ptr += sizeof(size_t);
    if (FAIL == (alacrity->index_id = H5Oopen_by_token(buf_ptr,
            trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
    buf_ptr += index_token_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_get_query_value
 *
 * Purpose: Get value from query.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_get_query_value(H5Q_t *query, value_types_t *value)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    switch (query->query.select.elem.data_elem.type_size) {
        case sizeof(double):
            value->asDouble =
                *((double *) query->query.select.elem.data_elem.value);
            H5X_ALACRITY_LOG_DEBUG("Double %lf\n", value->asDouble);
            break;
        case sizeof(float):
            value->asFloat =
                *((float *) query->query.select.elem.data_elem.value);
            H5X_ALACRITY_LOG_DEBUG("Float %f\n", value->asFloat);
            break;
        default:
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "unsupported query element type");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_get_query_ranges
 *
 * Purpose: Get range value from query.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_get_query_range(H5Q_t *query, H5X_alacrity_range_t *query_range)
{
    herr_t ret_value = SUCCEED; /* Return value */
    /* Would also need min and max values */

    FUNC_ENTER_NOAPI_NOINIT

    if (query->is_combined) {
        H5X__alacrity_get_query_range(query->query.combine.l_query, query_range);
        H5X__alacrity_get_query_range(query->query.combine.r_query, query_range);
    } else {
        if (H5Q_TYPE_DATA_ELEM != query->query.select.type)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "unsupported query type");
        if (query->query.select.match_op == H5Q_MATCH_GREATER_THAN) {
            /* This is a lower bound */
            H5X__alacrity_get_query_value(query, &query_range->lb);
        }
        if (query->query.select.match_op == H5Q_MATCH_LESS_THAN) {
            /* This is a higher bound */
            H5X__alacrity_get_query_value(query, &query_range->ub);
        }
        if (query->query.select.match_op == H5Q_MATCH_EQUAL) {
            /* Lower bound is equal to higher bound */
            H5X__alacrity_get_query_value(query, &query_range->lb);
            query_range->ub = query_range->lb;
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_get_query_ranges
 *
 * Purpose: Get range value from query.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_get_query_ranges(hid_t query_id,
        H5X_alacrity_range_t *query_ranges, size_t *nranges)
{
    H5Q_t *query;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");

    H5X__alacrity_get_query_range(query, query_ranges);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_find_bin_range
 *
 * Purpose: Get bin range from query range.
 *
 * Return:  TRUE if bins were found / FALSE otherwise
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5X__alacrity_find_bin_range(ALMetadata *metadata, value_types_t query_lb,
        value_types_t query_ub, bin_id_t *start_bin, bin_id_t *end_bin)
{
    hbool_t ret_value = FALSE; /* Return value */
    ALUnivariateQuery univariate_query;
    ALBinLayout *bl = &metadata->binLayout;
    uint64_t resultCount;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* For convenience use ALACRITY univariate query */
    univariate_query.lb = query_lb;
    univariate_query.ub = query_ub;
    univariate_query.qe = NULL;
    univariate_query.queryType = 0;

    H5X_ALACRITY_LOG_DEBUG("NumBins: %d", bl->numBins);

    /* Call internal ALACRITY findBinRange1C routine */
    ret_value = findBinRange1C(metadata, &univariate_query, start_bin,
            end_bin) ? TRUE : FALSE;
    if (ret_value) {
        H5X_ALACRITY_LOG_DEBUG("Start bin: %d", *start_bin);
        H5X_ALACRITY_LOG_DEBUG("End bin: %d", *end_bin);
        resultCount = bl->binStartOffsets[*end_bin] - bl->binStartOffsets[*start_bin];
        H5X_ALACRITY_LOG_DEBUG("Result count: %lu", resultCount);
    }

    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_read_metadata
 *
 * Purpose: Read ALACRITY metadata
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_read_metadata(H5X_alacrity_t *alacrity, hid_t rcxt_id)
{
    memstream_t memstream; /* Alacrity Memstream */
    hid_t metadata_space_id = FAIL;
    void *buf;
    size_t buf_size;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* allocate space for ALACRITY metadata */
    if (NULL == (alacrity->metadata = H5MM_malloc(sizeof(ALMetadata))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate ALACRITY metadata");

    if (FAIL == (metadata_space_id = H5Dget_space(alacrity->metadata_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
    if (0 == (buf_size = (size_t) H5Sget_select_npoints(metadata_space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

    /* allocate buffer to hold data */
    if (NULL == (buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* read metadata */
    if (FAIL == H5Dread_ff(alacrity->metadata_id, alacrity->opaque_type_id,
            H5S_ALL, metadata_space_id, H5P_DEFAULT, buf, rcxt_id,
            H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

    memstreamInit(&memstream, buf);
    if (ALErrorNone != ALDeserializeMetadata(alacrity->metadata, &memstream))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDECODE, FAIL, "can't deserialize metadata");
    memstreamDestroy(&memstream, 0);

done:
    H5MM_free(buf);
    if (FAIL != metadata_space_id)
        H5Sclose(metadata_space_id);
    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_read_index
 *
 * Purpose: Read ALACRITY index that correspond to selected bins
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_read_index(H5X_alacrity_t *alacrity, bin_id_t start_bin,
        bin_id_t end_bin, hid_t rcxt_id, ALIndex *al_index,
        size_t *al_index_size)
{
    const ALMetadata *meta = alacrity->metadata;
    const hsize_t first_bin_off = (hsize_t) ALGetIndexBinOffset(meta, start_bin);
    const hsize_t last_bin_off = (hsize_t) ALGetIndexBinOffset(meta, end_bin);
    const hsize_t bin_read_len = last_bin_off - first_bin_off;
    hid_t file_space_id = FAIL, mem_space_id = FAIL;
    size_t nelmts;
    int rank = 1;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_ALACRITY_LOG_DEBUG("Start Offset: %llu", first_bin_off);
    H5X_ALACRITY_LOG_DEBUG("End Offset: %llu", last_bin_off);

    if (0 == bin_read_len)
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "NULL index read length");

    /* Get space info */
    if (FAIL == (file_space_id = H5Dget_space(alacrity->index_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
    if (0 == (nelmts = (size_t) H5Sget_select_npoints(file_space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

    H5X_ALACRITY_LOG_DEBUG("Index elements to be read: %llu / %zu", bin_read_len, nelmts);

    if ((NULL == *al_index) && (NULL == (*al_index = H5MM_malloc(bin_read_len))))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate index buffer");

    /* Do not retrieve all the index, only what corresponds to the bins */
    if (FAIL == H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET,
            &first_bin_off, NULL, &bin_read_len, NULL))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "can't set offset");
    if (FAIL == (mem_space_id = H5Screate_simple(rank, &bin_read_len, NULL)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't create simple dataspace");

    /* Read data from dataset */
    if (FAIL == H5Dread_ff(alacrity->index_id, alacrity->opaque_type_id, mem_space_id,
            file_space_id, H5P_DEFAULT, *al_index, rcxt_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

    *al_index_size = bin_read_len;

done:
    if (FAIL != file_space_id)
        H5Sclose(file_space_id);
    if (FAIL != mem_space_id)
        H5Sclose(mem_space_id);
    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_query_range
 *
 * Purpose: Read index that correspond to selected bins
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__alacrity_query_range(H5X_alacrity_t *alacrity, hid_t dataspace_id,
        H5X_alacrity_range_t query_range, hid_t rcxt_id)
{
    ALIndex al_index = NULL;
    bool found_bin;
    bin_id_t start_bin = 0, end_bin = 0;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /**
     * First, find which bins are touched by the query (all elements in the
     * query range will fall into these bins, however not all elements in these
     * bins fall into the query range).
     */
    found_bin = H5X__alacrity_find_bin_range(alacrity->metadata, query_range.lb,
            query_range.ub, &start_bin, &end_bin);

    /* If bins were found, read index and create dataspace */
    if (found_bin) {
        rid_t *al_index_rids = NULL;
        size_t al_index_size;
        hsize_t count[H5S_MAX_RANK + 1];
        hsize_t start_coord[H5S_MAX_RANK + 1], end_coord[H5S_MAX_RANK + 1], nelmts;
        unsigned int i;

        if (FAIL == H5X__alacrity_read_index(alacrity, start_bin, end_bin,
                rcxt_id, &al_index, &al_index_size))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read index");
        al_index_rids = (rid_t *) al_index;

#ifdef H5X_ALACRITY_DEBUG
        /* TODO remove debug */
        printf(" # %s(): Index read contains following rIDs: ",  __func__);
        for (i = 0; i < al_index_size / (sizeof(rid_t)); i++) {
            printf("%d ", al_index_rids[i]);
        }
        printf("\n");
#endif

        /* Initialize count */
        for (i = 0; i < H5S_MAX_RANK; i++)
            count[i] = 1;

        for (i = 0; i < al_index_size / (sizeof(rid_t)); i++) {
            hsize_t coords[H5S_MAX_RANK + 1];
            const hsize_t point = al_index_rids[i];

            /* Convert coordinates */
            if (FAIL == H5VM_array_calc_pre(point, alacrity->dataset_ndims,
                    alacrity->dataset_down_dims, coords))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate coord array");

            /* Add converted coordinate to selection */
            if (H5Sselect_hyperslab(dataspace_id, H5S_SELECT_OR, coords, NULL, count, NULL))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to add point to selection");
        }

        if (FAIL == H5Sget_select_bounds(dataspace_id, start_coord, end_coord))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to get bounds");
        if (0 == (nelmts = (hsize_t) H5Sget_select_npoints(dataspace_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
        H5X_ALACRITY_LOG_DEBUG("Created dataspace from index with %llu elements [(%llu, %llu):(%llu, %llu)]",
                nelmts, start_coord[0], start_coord[1], end_coord[0], end_coord[1]);
    }

done:
    H5MM_free(al_index);
    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_create
 *
 * Purpose: This function creates a new instance of a ALACRITY plugin index.
 *
 * Return:  Success:    Pointer to the new index
 *          Failure:    NULL
 *
 *------------------------------------------------------------------------
 */
static void *
H5X_alacrity_create(hid_t file_id, hid_t dataset_id, hid_t UNUSED xcpl_id,
        hid_t xapl_id, size_t *metadata_size, void **metadata)
{
    H5X_alacrity_t *alacrity = NULL;
    hid_t trans_id = FAIL, rcxt_id = FAIL;
    void *ret_value = NULL; /* Return value */
    size_t private_metadata_size;
    void *buf = NULL;
    size_t buf_size;
    uint64_t version;

    FUNC_ENTER_NOAPI_NOINIT
    H5X_ALACRITY_LOG_DEBUG("Enter");

    /* Initialize ALACRITY plugin */
    if (NULL == (alacrity = H5X__alacrity_init(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, NULL, "can't initialize ALACRITY");

    /* Get transaction ID from xapl */
    if (FAIL == H5Pget_xapl_transaction(xapl_id, &trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get trans_id from xapl");

    /* Create read context from version */
    if (FAIL == H5TRget_version(trans_id, &version))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get version from transaction ID");
    if (FAIL == (rcxt_id =  H5RCcreate(file_id, version)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create read context");

    /* Get data from dataset */
    if (FAIL == H5X__alacrity_read_data(dataset_id, rcxt_id, &buf, &buf_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get data from dataset");

    /* Index data */
    if (FAIL == H5X__alacrity_create_index(alacrity, file_id, trans_id, buf,
            buf_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create index data from dataset");

    /* Serialize metadata for H5X interface */
    if (FAIL == H5X__alacrity_serialize_metadata(alacrity, NULL,
            &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get plugin metadata size");

    if (NULL == (alacrity->private_metadata = H5MM_malloc(private_metadata_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate plugin metadata");

    /* Serialize plugin metadata */
    if (FAIL == H5X__alacrity_serialize_metadata(alacrity, alacrity->private_metadata,
            &private_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, NULL, "can't serialize plugin metadata");

    /* Metadata is token for anonymous dataset */
    *metadata = alacrity->private_metadata;
    *metadata_size = private_metadata_size;

    ret_value = alacrity;

done:
    if (FAIL != rcxt_id)
        H5RCclose(rcxt_id);
    H5MM_free(buf);
    if (NULL == ret_value)
        H5X__alacrity_term(alacrity);
    H5X_ALACRITY_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_remove
 *
 * Purpose: This function removes the ALACRITY plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_alacrity_remove(hid_t UNUSED file_id, hid_t UNUSED dataset_id,
        size_t UNUSED metadata_size, void UNUSED *metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR
    H5X_ALACRITY_LOG_DEBUG("Enter");

    H5X_ALACRITY_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_open
 *
 * Purpose: This function opens an already existing ALACRITY index from a file.
 *
 * Return:  Success:    Pointer to the index
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5X_alacrity_open(hid_t file_id, hid_t dataset_id, hid_t xapl_id,
        size_t metadata_size, void *metadata)
{
    H5X_alacrity_t *alacrity = NULL;
    hid_t trans_id = FAIL, rcxt_id = FAIL;
    uint64_t c_version;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_ALACRITY_LOG_DEBUG("Enter");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata");

    /* Get read context from xapl */
    if (FAIL == H5Pget_xapl_read_context(xapl_id, &rcxt_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get rc_id from xapl");

    /* Initialize ALACRITY plugin */
    if (NULL == (alacrity = H5X__alacrity_init(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, NULL, "can't initialize ALACRITY");

    /* Create transaction from version (for open_by_token) */
    if (FAIL == H5RCget_version(rcxt_id, &c_version))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get version from read context ID");
    if (FAIL == (trans_id = H5TRcreate(file_id, rcxt_id, c_version)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create transaction");

    /* Deserialize plugin metadata */
    if (FAIL == H5X__alacrity_deserialize_metadata(alacrity, metadata, trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDECODE, NULL, "can't deserialize plugin metadata");

    ret_value = alacrity;

done:
    if (FAIL != trans_id)
        H5TRclose(trans_id);
    if (NULL == ret_value)
        H5X__alacrity_term(alacrity);
    H5X_ALACRITY_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_close
 *
 * Purpose: This function closes an ALACRITY index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_alacrity_close(void *idx_handle)
{
    H5X_alacrity_t *alacrity = (H5X_alacrity_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_ALACRITY_LOG_DEBUG("Enter");

    if (NULL == alacrity)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    if (FAIL == H5X__alacrity_term(alacrity))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTFREE, FAIL, "Cannot terminate ALACRITY");

done:
    H5X_ALACRITY_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_close() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_pre_update
 *
 * Purpose: This function does a pre_update of indexing information.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_alacrity_pre_update(void *idx_handle, hid_t UNUSED dataspace_id, hid_t UNUSED xxpl_id)
{
    H5X_alacrity_t *alacrity = (H5X_alacrity_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_ALACRITY_LOG_DEBUG("Enter");

    if (NULL == alacrity)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

done:
    H5X_ALACRITY_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_pre_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_post_update
 *
 * Purpose: This function does a post_update of indexing information.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_alacrity_post_update(void *idx_handle, const void UNUSED *buf,
        hid_t UNUSED dataspace_id, hid_t UNUSED xxpl_id)
{
    H5X_alacrity_t *alacrity = (H5X_alacrity_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_ALACRITY_LOG_DEBUG("Calling H5X_alacrity_post_update");

    if (NULL == alacrity)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* Not needed here */
done:
    H5X_ALACRITY_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_post_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_query
 *
 * Purpose: This function retrieves indexing information that matches
 * the query and returns results under the form of a dataspace ID.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_alacrity_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
        hid_t *dataspace_id)
{
    H5X_alacrity_t *alacrity = (H5X_alacrity_t *) idx_handle;
    hid_t rcxt_id;
    hid_t space_id = FAIL, ret_space_id = FAIL;
    H5X_alacrity_range_t query_range;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_ALACRITY_LOG_DEBUG("Enter");

    if (NULL == alacrity)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* Get read context ID from xxpl */
    if (FAIL == H5Pget_xxpl_read_context(xxpl_id, &rcxt_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get rcxt_id from xxpl");

    /* If metadata has not been read already, read it */
    if (!alacrity->metadata && (FAIL == H5X__alacrity_read_metadata(alacrity, rcxt_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read ALACRITY metadata");

    /* Create a copy of the original dataspace */
    if (FAIL == (space_id = H5Dget_space(alacrity->dataset_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
    if (FAIL == (ret_space_id = H5Scopy(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy dataspace");
    if (FAIL == H5Sselect_none(ret_space_id))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSELECT, FAIL, "can't reset selection");

    /* Get range values from query */
    if (FAIL == H5X__alacrity_get_query_ranges(query_id, &query_range, NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get query ranges");

    /* Query range */
    if (FAIL == H5X__alacrity_query_range(alacrity, ret_space_id, query_range,
            rcxt_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get query range");

    *dataspace_id = ret_space_id;

done:
    if (FAIL != space_id)
        H5Sclose(space_id);
    if ((FAIL == ret_value) && (FAIL != ret_space_id))
        H5Sclose(ret_space_id);
    H5X_ALACRITY_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_query() */
