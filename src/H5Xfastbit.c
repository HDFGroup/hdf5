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
#define H5X_FASTBIT_LOG_DEBUG(...) do {                        \
      fprintf(stdout, " # %s(): ", __func__);                   \
      fprintf(stdout, __VA_ARGS__);                             \
      fprintf(stdout, "\n");                                    \
      fflush(stdout);                                           \
  } while (0)
#else
#define H5X_FASTBIT_LOG_DEBUG
#endif

/******************/
/* Local Typedefs */
/******************/
typedef struct H5X_fastbit_t {
    void *private_metadata;     /* Internal metadata */

    hid_t opaque_type_id;       /* Datatype used for index datasets */
    hid_t metadata_id;          /* Array for metadata */
    hid_t index_id;             /* Array for index data */
} H5X_fastbit_t;

/********************/
/* Local Prototypes */
/********************/

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
    H5X_fastbit_query          /* query */
}};


/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_create
 *
 * Purpose: This function creates a new instance of a FASTBIT plugin index.
 *
 * Return:  Success:    Pointer to the new index
 *          Failure:    NULL
 *
 *------------------------------------------------------------------------
 */
static void *
H5X_fastbit_create(hid_t file_id, hid_t dataset_id, hid_t UNUSED xcpl_id,
        hid_t xapl_id, size_t *metadata_size, void **metadata)
{
    H5X_fastbit_t *fastbit = NULL;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_remove
 *
 * Purpose: This function removes the FASTBIT plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_fastbit_remove(hid_t UNUSED file_id, hid_t UNUSED dataset_id,
        size_t UNUSED metadata_size, void UNUSED *metadata)
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
 * Purpose: This function opens an already existing FASTBIT index from a file.
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
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");

done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_fastbit_close
 *
 * Purpose: This function closes an FASTBIT index.
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
H5X_fastbit_pre_update(void *idx_handle, hid_t UNUSED dataspace_id, hid_t UNUSED xxpl_id)
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
H5X_fastbit_post_update(void *idx_handle, const void UNUSED *buf,
        hid_t UNUSED dataspace_id, hid_t UNUSED xxpl_id)
{
    H5X_fastbit_t *fastbit = (H5X_fastbit_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_FASTBIT_LOG_DEBUG("Calling H5X_fastbit_post_update");

    if (NULL == fastbit)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

done:
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
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT
    H5X_FASTBIT_LOG_DEBUG("Enter");


done:
    H5X_FASTBIT_LOG_DEBUG("Leave");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_fastbit_query() */

#endif /* H5_HAVE_FASTBIT */
