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
/* TODO using private headers but could use public ones */

#include <alacrity-types.h>
#include <alacrity-serialization.h>
#include <alacrity.h>

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/
typedef struct H5X_alacrity_t {
    hbool_t metadata_read;
    ALMetadata metadata;    /* Alacrity metadata */
    ALEncoderConfig config; /* Alacrity config */
    ALPartitionData output; /* Alacrity output */
    hid_t opaque_type_id;   /* Datatype used for index datasets */
    hid_t metadata_id;      /* Array for metadata */
    hid_t data_id;          /* Array for data */
    hid_t index_id;         /* Array for index data */
} H5X_alacrity_t;

typedef struct {
    // true if the metadata file pointer from the parent ALStore still points the beginning of the
    // partition's metadata (i.e., ALPartitionStoreReadMetadata() hasn't been called yet to move it)
    _Bool metafp_unchanged;
    uint64_t meta_offset;
    uint64_t data_offset;
    uint64_t index_offset;

    _Bool iindexfp_changed;
    _Bool cindexfp_changed;
} H5X__alacrity_partition_state_t;

typedef struct H5X__alacrity_query_data_t {
    size_t num_elmts;
    hid_t query_id;
    hid_t space_query;
} H5X__alacrity_query_data_t;

/********************/
/* Local Prototypes */
/********************/

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

/*******************/
/* Local Variables */
/*******************/

/* Alacrity index class */
const H5X_class_t H5X_ALACRITY[1] = {{
    H5X_CLASS_T_VERS,          /* (From the H5Xpublic.h header file) */
    H5X_PLUGIN_ALACRITY,       /* (Or whatever number is assigned) */
    "alacrity index plugin",   /* Whatever name desired */
    H5X_TYPE_DATA_ELEM,        /* This plugin operates on dataset elements */
    H5X_alacrity_create,       /* create */
    H5X_alacrity_remove,       /* remove */
    H5X_alacrity_open,         /* open */
    H5X_alacrity_close,        /* close */
    H5X_alacrity_pre_update,   /* pre_update */
    H5X_alacrity_post_update,  /* post_update */
    H5X_alacrity_query         /* query */
}};

static herr_t
H5X__alacrity_configure(H5X_alacrity_t *alacrity, hid_t dataset_id)
{
    hid_t dataset_type_id;
    size_t dataset_type_size;
    ALDatatype alacrity_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (FAIL == (alacrity->opaque_type_id = H5Tcreate(H5T_OPAQUE, 1)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, FAIL, "can't create type");
    if (FAIL == H5Tset_tag(alacrity->opaque_type_id, "alacrity metadata type"))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTSET, FAIL, "can't set tag to type");

    if (FAIL == (dataset_type_id = H5Dget_type(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get type from dataset");
    if (0 == (dataset_type_size = H5Tget_size(dataset_type_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get type size");
    printf("Type size: %zu\n", dataset_type_size);
    alacrity_type = (dataset_type_size == 4) ? DATATYPE_FLOAT32 : DATATYPE_FLOAT64;

    ALEncoderConfigure(&alacrity->config, 16, alacrity_type, ALInvertedIndex);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t
H5X__alacrity_get_dataset_data(hid_t file_id, hid_t dataset_id, hid_t trans_id,
        void **buf, size_t *buf_size)
{
    herr_t ret_value = SUCCEED; /* Return value */
    hid_t rcxt_id, type_id, space_id;
    size_t nelmts, elmt_size;
    uint64_t version;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get read context */
    if (FAIL == H5TRget_version(trans_id, &version))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get version from transaction ID");
    if (FAIL == (rcxt_id =  H5RCcreate(file_id, version)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create read context");

    /* Get space info */
    if (FAIL == (type_id = H5Dget_type(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get type from dataset");
    if (FAIL == (space_id = H5Dget_space(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
    if (0 == (nelmts = (size_t) H5Sget_select_npoints(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    if (0 == (elmt_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid size of element");

    printf("*** npoints: %zu\n", (size_t) nelmts);

    /* Allocate buffer to hold data */
    *buf_size = nelmts * elmt_size;
    if(NULL == (*buf = H5MM_malloc(*buf_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* Read data from dataset */
    if (FAIL == H5Dread_ff(dataset_id, type_id, H5S_ALL, space_id,
            H5P_DEFAULT, *buf, rcxt_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

    /* Close read context */
    if (FAIL == H5RCclose(rcxt_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close read context");

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t
H5X__alacrity_free_data(void *buf)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t
H5X__alacrity_create_index(H5X_alacrity_t *alacrity, hid_t file_id, hid_t dataset_id,
        hid_t trans_id, const void *buf, size_t buf_size)
{
    hid_t type_id, space_id;
    hid_t metadata_space_id, data_space_id, index_space_id;
    hsize_t metadata_size, index_size, data_size;
    memstream_t memstream;
    size_t nelmts;
    void *metadata_buf;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get space info */
    if (FAIL == (type_id = H5Dget_type(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get type from dataset");
    if (FAIL == (space_id = H5Dget_space(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from dataset");
    if (0 == (nelmts = (size_t) H5Sget_select_npoints(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

    ALEncode(&alacrity->config, buf, nelmts, &alacrity->output);

    /* Get sizes */
    metadata_size = ALGetMetadataSize(&alacrity->output.metadata);
    data_size = ALGetDataSize(&alacrity->output.data,
            &alacrity->output.metadata);
    index_size = ALGetIndexSize(&alacrity->output.index,
            &alacrity->output.metadata);

    printf("*** Metadata size: %zu\n", (size_t) metadata_size);
    printf("*** Data size: %zu\n", (size_t) data_size);
    printf("*** Index size: %zu\n", (size_t) index_size);

    /* Create metadata array with opaque type */
    if (FAIL == (metadata_space_id = H5Screate_simple(1, &metadata_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");

    if (FAIL == (alacrity->metadata_id = H5Dcreate_anon_ff(file_id, alacrity->opaque_type_id,
            metadata_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Create data array with opaque type */
    if (FAIL == (data_space_id = H5Screate_simple(1, &data_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");

    if (FAIL == (alacrity->data_id = H5Dcreate_anon_ff(file_id, alacrity->opaque_type_id,
            data_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Create index array with opaque type */
    if (FAIL == (index_space_id = H5Screate_simple(1, &index_size, NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create simple dataspace");

    if (FAIL == (alacrity->index_id = H5Dcreate_anon_ff(file_id, alacrity->opaque_type_id,
            index_space_id, H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

    /* Serialize and write ALACRITY metadata */
    metadata_buf = H5MM_malloc(metadata_size);
    if (NULL == metadata_buf)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate index elements");
    memstreamInit(&memstream, metadata_buf);
    ALSerializeMetadata(&alacrity->output.metadata, &memstream);
    if (FAIL == H5Dwrite_ff(alacrity->metadata_id, alacrity->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, memstream.buf, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't update index metadata");
    memstreamDestroy(&memstream, 0);

    /* Write ALACRITY compressed data */
    if (FAIL == H5Dwrite_ff(alacrity->data_id, alacrity->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, alacrity->output.data, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't update index data");

    /* Write ALACRITY index */
    if (FAIL == H5Dwrite_ff(alacrity->index_id, alacrity->opaque_type_id, H5S_ALL,
            H5S_ALL, H5P_DEFAULT, alacrity->output.index, trans_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTUPDATE, FAIL, "can't update index data");
done:
    H5MM_free(metadata_buf);
//    if (metadata_space_id != FAIL) H5Sclose(metadata_space_id);
    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t
H5X__alacrity_serialize_metadata(H5X_alacrity_t *alacrity, void *buf,
        size_t *buf_size)
{
    size_t metadata_token_size, data_token_size, index_token_size;
    size_t alacrity_metadata_size;

    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get tokens */
    if (FAIL == H5Oget_token(alacrity->metadata_id, NULL, &metadata_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token size for anonymous dataset");
    if (FAIL == H5Oget_token(alacrity->data_id, NULL, &data_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token size for anonymous dataset");
    if (FAIL == H5Oget_token(alacrity->index_id, NULL, &index_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token size for anonymous dataset");

    /* Make some space for the metadata (tokens + sizes) */
    alacrity_metadata_size = metadata_token_size + data_token_size +
            index_token_size + 3 * sizeof(size_t);

    if (buf) {
        char *buf_ptr = buf;

        /* Encode metadata token info */
        HDmemcpy(buf_ptr, &metadata_token_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if (FAIL == H5Oget_token(alacrity->metadata_id, buf_ptr, &metadata_token_size))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token for anonymous dataset");
        buf_ptr += metadata_token_size;

        /* Encode data token info */
        HDmemcpy(buf_ptr, &data_token_size, sizeof(size_t));
        buf_ptr += sizeof(size_t);
        if (FAIL == H5Oget_token(alacrity->data_id, buf_ptr, &data_token_size))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get token for anonymous dataset");
        buf_ptr += data_token_size;

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

static herr_t
H5X__alacrity_findBinRange1C(ALMetadata metadata, hid_t query_id,
        bin_id_t *start_bin, bin_id_t *end_bin)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_create
 *
 * Purpose: This function creates a new instance of a alacrity plugin index.
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
    hid_t trans_id;
    void *ret_value = NULL; /* Return value */
    size_t dataset_type_size;
    size_t alacrity_metadata_size;
    void *alacrity_metadata;
    void *buf = NULL;
    size_t buf_size;

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_alacrity_create\n");

    if (NULL == (alacrity = (H5X_alacrity_t *) H5MM_malloc(sizeof(H5X_alacrity_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate alacrity struct");
    alacrity->metadata_read = FALSE;

    /* Configure the ALACRITY encoder */
    if (FAIL == H5X__alacrity_configure(alacrity, dataset_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTSET, NULL, "can't configure ALACRITY");

#ifdef H5_HAVE_INDEXING
    if (FAIL == H5Pget_xapl_transaction(xapl_id, &trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get trans_id from xapl");
#endif

    /* Get data from dataset */
    if (FAIL == H5X__alacrity_get_dataset_data(file_id, dataset_id, trans_id,
            &buf, &buf_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get data from dataset");

    /* Index data */
    if (FAIL == H5X__alacrity_create_index(alacrity, file_id, dataset_id,
            trans_id, buf, buf_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't index data from dataset");

    /* Serialize metadata for H5X interface */
    if (FAIL == H5X__alacrity_serialize_metadata(alacrity, NULL,
            &alacrity_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate token metadata");

    if (NULL == (alacrity_metadata = H5MM_malloc(alacrity_metadata_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate token metadata");

    if (FAIL == H5X__alacrity_serialize_metadata(alacrity, alacrity_metadata,
            &alacrity_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate token metadata");

    /* Metadata is token for anonymous dataset */
    *metadata = alacrity_metadata;
    *metadata_size = alacrity_metadata_size;

    ret_value = alacrity;

done:
    H5X__alacrity_free_data(buf);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_remove
 *
 * Purpose: This function removes the alacrity plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_alacrity_remove(hid_t UNUSED file_id, hid_t UNUSED dataset_id, size_t UNUSED metadata_size,
        void UNUSED *metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    printf("Calling H5X_alacrity_remove\n");

    /* TODO Does not do anything */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_open
 *
 * Purpose: This function opens an already existing alacrity index from a file.
 *
 * Return:  Success:    Pointer to the index
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5X_alacrity_open(hid_t file_id, hid_t UNUSED dataset_id, hid_t xapl_id,
        size_t metadata_size, void *metadata)
{
    H5X_alacrity_t *alacrity = NULL;
    hid_t trans_id = FAIL, rc_id;
    uint64_t c_version;
    void *ret_value = NULL; /* Return value */
    char *alacrity_metadata_ptr = metadata;
    size_t metadata_token_size, data_token_size, index_token_size;

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_alacrity_open\n");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata");
#ifdef H5_HAVE_INDEXING
    if (FAIL == H5Pget_xapl_read_context(xapl_id, &rc_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get rc_id from xapl");
#endif
    if (NULL == (alacrity = (H5X_alacrity_t *) H5MM_malloc(sizeof(H5X_alacrity_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate alacrity struct");
    alacrity->metadata_read = FALSE;

    /* TODO move that */
    if (FAIL == (alacrity->opaque_type_id = H5Tcreate(H5T_OPAQUE, 1)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCREATE, FAIL, "can't create type");
    if (FAIL == H5Tset_tag(alacrity->opaque_type_id, "alacrity metadata type"))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTSET, FAIL, "can't set tag to type");

    /* TODO do that like this for now */
    H5RCget_version(rc_id, &c_version);
    trans_id = H5TRcreate(file_id, rc_id, c_version);

    /* Decode metadata token info */
    HDmemcpy(&metadata_token_size, alacrity_metadata_ptr, sizeof(size_t));
    alacrity_metadata_ptr += sizeof(size_t);
    if (FAIL == (alacrity->metadata_id = H5Oopen_by_token(alacrity_metadata_ptr,
            trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, NULL, "can't open anonymous dataset");
    alacrity_metadata_ptr += metadata_token_size;

    /* Decode data token info */
    HDmemcpy(&data_token_size, alacrity_metadata_ptr, sizeof(size_t));
    alacrity_metadata_ptr += sizeof(size_t);
    if (FAIL == (alacrity->data_id = H5Oopen_by_token(alacrity_metadata_ptr,
            trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, NULL, "can't open anonymous dataset");
    alacrity_metadata_ptr += data_token_size;

    /* Decode index token info */
    HDmemcpy(&index_token_size, alacrity_metadata_ptr, sizeof(size_t));
    alacrity_metadata_ptr += sizeof(size_t);
    if (FAIL == (alacrity->index_id = H5Oopen_by_token(alacrity_metadata_ptr,
            trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, NULL, "can't open anonymous dataset");
    alacrity_metadata_ptr += index_token_size;

    ret_value = alacrity;

done:
    if (FAIL != trans_id) H5TRclose(trans_id);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_close
 *
 * Purpose: This function unregisters an index class.
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

    printf("Calling H5X_alacrity_close\n");

    if (NULL == alacrity)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

//    ALPartitionDataDestroy(&alacrity->output);

    /* Close anonymous dataset */
    if (FAIL == H5Dclose_ff(alacrity->metadata_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");
    if (FAIL == H5Dclose_ff(alacrity->data_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");
    if (FAIL == H5Dclose_ff(alacrity->index_id, H5_EVENT_STACK_NULL))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close anonymous dataset for index");

    if ((FAIL != alacrity->opaque_type_id) && (FAIL == H5Tclose(alacrity->opaque_type_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close opaque type");

    H5MM_free(alacrity);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_close() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_pre_update
 *
 * Purpose: This function unregisters an index class.
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

    printf("Calling H5X_alacrity_pre_update\n");

    if (NULL == alacrity)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* Not needed here */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_pre_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_post_update
 *
 * Purpose: This function unregisters an index class.
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

    printf("Calling H5X_alacrity_post_update\n");

    if (NULL == alacrity)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* Not needed here */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_post_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X__alacrity_get_query_data_cb
 *
 * Purpose: This function unregisters an index class.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
//static herr_t
//H5X__alacrity_get_query_data_cb(void *elem, hid_t type_id, unsigned UNUSED ndim,
//        const hsize_t *point, void *_udata)
//{
//    H5X__alacrity_query_data_t *udata = (H5X__alacrity_query_data_t *)_udata;
//    hbool_t result;
//    herr_t ret_value = SUCCEED;
//
//    FUNC_ENTER_NOAPI_NOINIT
//
//    /* Apply the query */
//    if (H5Qapply(udata->query_id, &result, type_id, elem) < 0)
//        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query to data element");
//
//    /* If element satisfies query, add it to the selection */
//    if (result) {
//        /* TODO remove that after demo */
//        printf("Element |%d| matches query\n", *((int *) elem));
//        udata->num_elmts++;
//        if(H5Sselect_elements(udata->space_query, H5S_SELECT_APPEND, 1, point))
//            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to add point to selection");
//    }
//
//done:
//    FUNC_LEAVE_NOAPI(ret_value)
//} /* end H5X__alacrity_get_query_data_cb */

/*-------------------------------------------------------------------------
 * Function:    H5X_alacrity_query
 *
 * Purpose: This function unregisters an index class.
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
    ALIndex alacrity_index;
    bin_id_t start_bin, end_bin;
    hid_t rcxt_id;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_alacrity_query\n");

    if (NULL == alacrity)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");
#ifdef H5_HAVE_INDEXING
    if (FAIL == H5Pget_xxpl_read_context(xxpl_id, &rcxt_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get rcxt_id from xxpl");
#endif

    if (!alacrity->metadata_read) {
        memstream_t memstream; /* Alacrity Memstream */
        hid_t metadata_space_id;
        void *buf;
        size_t buf_size;

        if (FAIL == (metadata_space_id = H5Dget_space(alacrity->metadata_id)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataspace from index");
        if (0 == (buf_size = (size_t) H5Sget_select_npoints(metadata_space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");

        /* allocate buffer to hold data */
        if(NULL == (buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate read buffer");
        memset(buf, '\0', buf_size);

        /* read metadata */
        if (FAIL == H5Dread_ff(alacrity->metadata_id, alacrity->opaque_type_id,
                H5S_ALL, metadata_space_id, H5P_DEFAULT, buf, rcxt_id,
                H5_EVENT_STACK_NULL))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read data");

        memstreamInit(&memstream, buf);
        ALDeserializeMetadata(&alacrity->metadata, &memstream);
        memstreamDestroy(&memstream, 0);
        printf("**** Partition length: %zu\n", alacrity->metadata.partitionLength);
        H5MM_free(buf);

        /* Set this to TRUE so we don't read metadata again */
        alacrity->metadata_read = TRUE;
    }

    /**
     * First, find which bins are touched by the query (all elements in the
     * query range will fall into these bins, however not all elements in these
     * bins fall into the query range).
     */
    H5X__alacrity_findBinRange1C(alacrity->metadata, query_id, &start_bin, &end_bin);

    /* Second readIndex */
    ALGetIndexBinOffset(alacrity->metadata, start_bin);
    ALGetIndexBinOffset(alacrity->metadata, end_bin);

    H5X__alacrity_readIndex(alacrity, start_bin, end_bin, &alacrity_index);

    if (alacrity->metadata->indexMeta.indexForm == ALCompressedInvertedIndex)
        ALConvertPartialIndexForm(&alacrity->metadata, &alacrity_index,
                ALInvertedIndex, start_bin, end_bin);


//    if(FAIL == (udata.space_query = H5Scopy(space_id)))
//        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace");
//    if(H5Sselect_none(udata.space_query) < 0)
//        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to reset selection");
//
//    udata.num_elmts = 0;
//    udata.query_id = query_id;
//
//    /* iterate over every element and apply the query on it. If the
//       query is not satisfied, then remove it from the query selection */
//    if (H5Diterate(buf, type_id, space_id, H5X__alacrity_get_query_data_cb, &udata) < 0)
//        HGOTO_ERROR(H5E_INDEX, H5E_CANTCOMPUTE, FAIL, "failed to compute buffer size");
//
//    *dataspace_id = udata.space_query;
//    printf("Created dataspace from index with %d elements\n",
//            (int) H5Sget_select_npoints(*dataspace_id));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_alacrity_query() */
