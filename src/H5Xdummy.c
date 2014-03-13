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


/********************/
/* Local Prototypes */
/********************/

static void *
H5X_dummy_create(hid_t fid, const char *dataset_name, hid_t icpl_id,
        hid_t iapl_id, size_t *metadata_size, void **metadata);

static herr_t
H5X_dummy_remove(hid_t fid, const char *dataset_name, size_t metadata_size,
        void *metadata);

static void *
H5X_dummy_open(hid_t fid, const char *dataset_name, hid_t iapl_id,
        size_t metadata_size, void *metadata);

static herr_t
H5X_dummy_close(void *idx_handle);

static herr_t
H5X_dummy_pre_update(void *idx_handle, hid_t buffer_dataspace, hid_t ixpl_id);

static herr_t
H5X_dummy_post_update(void *idx_handle, const void *buffer, hid_t buffer_dataspace,
        hid_t ixpl_id);

static herr_t
H5X_dummy_query(void *idx_handle, hid_t query_id, hid_t idxpl_id,
        hid_t *dataspace_selection_id);

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
H5X_dummy_create(hid_t fid, const char *dataset_name, hid_t icpl_id,
        hid_t iapl_id, size_t *metadata_size, void **metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* H5Dcreate_anon_ff + H5Oincr_refcount() */

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
H5X_dummy_remove(hid_t fid, const char *dataset_name, size_t metadata_size,
        void *metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

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
H5X_dummy_open(hid_t fid, const char *dataset_name, hid_t iapl_id,
        size_t metadata_size, void *metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* H5Oget_info */
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
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

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
H5X_dummy_pre_update(void *idx_handle, hid_t buffer_dataspace, hid_t ixpl_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

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
H5X_dummy_post_update(void *idx_handle, const void *buffer, hid_t buffer_dataspace,
        hid_t ixpl_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

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
H5X_dummy_query(void *idx_handle, hid_t query_id, hid_t idxpl_id,
        hid_t *dataspace_selection_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_query() */
