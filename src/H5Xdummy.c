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
 * Purpose:	Dummy index routines.
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

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/
typedef struct H5X_dummy_t {
    hid_t dataset_id;
    hid_t idx_anon_id;
    void *idx_token;
    size_t idx_token_size;
} H5X_dummy_t;

/********************/
/* Local Prototypes */
/********************/

static void *
H5X_dummy_create(hid_t file_id, hid_t dataset_id, hid_t xcpl_id,
        hid_t xapl_id, size_t *metadata_size, void **metadata);

static herr_t
H5X_dummy_remove(hid_t file_id, hid_t dataset_id, size_t metadata_size,
        void *metadata);

static void *
H5X_dummy_open(hid_t file_id, hid_t dataset_id, hid_t xapl_id,
        size_t metadata_size, void *metadata);

static herr_t
H5X_dummy_close(void *idx_handle);

static herr_t
H5X_dummy_pre_update(void *idx_handle, hid_t dataspace_id, hid_t xxpl_id);

static herr_t
H5X_dummy_post_update(void *idx_handle, const void *buf, hid_t dataspace_id,
        hid_t xxpl_id);

static herr_t
H5X_dummy_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
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

/* Dummy index class */
const H5X_class_t H5X_DUMMY[1] = {
    H5X_CLASS_T_VERS,       /* (From the H5Xpublic.h header file) */
    1,                      /* (Or whatever number is assigned) */
    "dummy index plugin",   /* Whatever name desired */
    H5X_TYPE_DATA_ELEM,     /* This plugin operates on dataset elements */
    H5X_dummy_create,       /* create */
    H5X_dummy_remove,       /* remove */
    H5X_dummy_open,         /* open */
    H5X_dummy_close,        /* close */
    H5X_dummy_pre_update,   /* pre_update */
    H5X_dummy_post_update,  /* post_update */
    H5X_dummy_query         /* query */
};

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_create
 *
 * Purpose: This function creates a new instance of a dummy plugin index.
 *
 * Return:  Success:    Pointer to the new index
 *          Failure:    NULL
 *
 *------------------------------------------------------------------------
 */
static void *
H5X_dummy_create(hid_t file_id, hid_t dataset_id, hid_t xcpl_id,
        hid_t xapl_id, size_t *metadata_size, void **metadata)
{
    H5X_dummy_t *dummy = NULL;
    hid_t type_id, space_id, trans_id;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_dummy_create\n");

    if (NULL == (dummy = (H5X_dummy_t *) H5MM_malloc(sizeof(H5X_dummy_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate dummy struct");

    dummy->dataset_id = dataset_id;

    if (FAIL == (type_id = H5Dget_type(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get type from dataset");
    if (FAIL == (space_id = H5Dget_space(dataset_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get dataspace from dataset");

#ifdef H5_HAVE_INDEXING
    if (FAIL == H5Pget_xapl_transaction(xapl_id, &trans_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get trans_id from xapl");
#endif

    if (FAIL == (dummy->idx_anon_id = H5Dcreate_anon_ff(file_id, type_id, space_id,
            H5P_DEFAULT, H5P_DEFAULT, trans_id, H5_EVENT_STACK_NULL)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create anonymous dataset");

    if (FAIL == H5Oget_token(dummy->idx_anon_id, NULL, &dummy->idx_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get token size for anonymous dataset");

    if (NULL == (dummy->idx_token = H5MM_malloc(dummy->idx_token_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate token  for anonymous dataset");

    if (FAIL == H5Oget_token(dummy->idx_anon_id, dummy->idx_token, &dummy->idx_token_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "can't get token for anonymous dataset");

    /* Metadata is token for anonymous dataset */
    *metadata = dummy->idx_token;
    *metadata_size = dummy->idx_token_size;

    ret_value = dummy;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_remove
 *
 * Purpose: This function removes the dummy plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_dummy_remove(hid_t file_id, hid_t dataset_id, size_t metadata_size,
        void *metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* TODO Does not do anything */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_open
 *
 * Purpose: This function open an already existing dummy index from a file.
 *
 * Return:  Success:    Pointer to the index
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5X_dummy_open(hid_t file_id, hid_t dataset_id, hid_t xapl_id,
        size_t metadata_size, void *metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_dummy_open\n");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_close
 *
 * Purpose: This function unregisters an index class.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_dummy_close(void *idx_handle)
{
    H5X_dummy_t *dummy = (H5X_dummy_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_dummy_close\n");
    if (FAIL == H5Dclose_ff(dummy->idx_anon_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, NULL, "can't close anonymous dataset for index");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_close() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_pre_update
 *
 * Purpose: This function unregisters an index class.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_dummy_pre_update(void *idx_handle, hid_t dataspace_id, hid_t xxpl_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_dummy_pre_update\n");
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_pre_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_post_update
 *
 * Purpose: This function unregisters an index class.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_dummy_post_update(void *idx_handle, const void *buf, hid_t dataspace_id,
        hid_t xxpl_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_dummy_post_update\n");

    /* Update index elements */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_post_update() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_query
 *
 * Purpose: This function unregisters an index class.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X_dummy_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
        hid_t *dataspace_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    printf("Calling H5X_dummy_query\n");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_query() */
