/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/****************/
/* Module Setup */
/****************/

#include "H5SCmodule.h" /* This source code file is part of the H5SC module */
#define H5D_FRIEND      /* Suppress error about including H5Dpkg */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions            */
#include "H5Dpkg.h"      /* Datasets                     */
#include "H5Eprivate.h"  /* Error handling               */
#include "H5Fprivate.h"  /* Files                        */
#include "H5MMprivate.h" /* Memory management            */
#include "H5SCpkg.h"     /* Shared chunk cache           */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
bool H5_PKG_INIT_VAR = false;

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function: H5SC_create
 *
 * Purpose:  Creates a new, empty shared chunk cache.
 *
 * Return:   Pointer to newly created cache on success, NULL on failure
 *-------------------------------------------------------------------------
 */
H5SC_t *
H5SC_create(H5F_t *file, hid_t fapl_id)
{
    H5SC_t *cache     = NULL;
    H5SC_t *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    assert(file);

    /* Allocated cache struct */
    if (NULL == (cache = H5MM_malloc(sizeof(H5SC_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "unable to allocate buffer for shared chunk cache");

    /* Success */
    ret_value = cache;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_create() */

/*-------------------------------------------------------------------------
 * Function: H5SC_destroy
 *
 * Purpose:  Destroys a shared chunk cache, freeing all data used. Does
 *           not flush chunks.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_destroy(H5SC_t *cache)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);

    H5MM_free(cache);
    cache = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_destroy() */

/*-------------------------------------------------------------------------
 * Function: H5SC_flush
 *
 * Purpose:  Flushes all cached data from a shared chunk cache.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_flush(H5SC_t *cache)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_flush() */

/*-------------------------------------------------------------------------
 * Function: H5SC_flush_dset
 *
 * Purpose:  Flushes all data cached for a single dataset. If evict is
 *           true, also evicts all cached data.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_flush_dset(H5SC_t *cache, H5D_t *dset, bool evict)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_flush_dset() */

/*-------------------------------------------------------------------------
 * Function: H5SC_read
 *
 * Purpose:  Reads raw data through a shared chunk cache.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_read(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info, H5D_io_type_info_t *io_type_info)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(count == 0 || dset_info);
    assert(io_type_info);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_read() */

/*-------------------------------------------------------------------------
 * Function: H5SC_write
 *
 * Purpose:  Writes raw data through a shared chunk cache.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_write(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info, H5D_io_type_info_t *io_type_info)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(count == 0 || dset_info);
    assert(io_type_info);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_write() */

/*-------------------------------------------------------------------------
 * Function: H5SC_direct_chunk_read
 *
 * Purpose:  Reads the chunk that starts at coordinates give by offset
 *           directly from disk to buf, without any decoding or
 *           conversion. First flushes that chunk if it is dirty in the
 *           cache.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_direct_chunk_read(H5SC_t *cache, H5D_t *dset, hsize_t *offset, void *buf)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(offset);
    assert(buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_direct_chunk_read() */

/*-------------------------------------------------------------------------
 * Function: H5SC_direct_chunk_write
 *
 * Purpose:  Writes the chunk that starts at coordinates give by offset
 *           directly from buf to disk, without any decoding or
 *           conversion. First evicts that chunk from cache if it is
 *           present.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_direct_chunk_write(H5SC_t *cache, H5D_t *dset, hsize_t *offset, const void *buf)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(offset);
    assert(buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_direct_chunk_write() */

/*-------------------------------------------------------------------------
 * Function: H5SC_set_extent_notify
 *
 * Purpose:  Called after H5Dset_extent() has been called for a dataset,
 *           so the cache can recompute chunk indices, delete chunks,
 *           clear unused sections of chunks, etc.
 *
 * Return:   SUCCEED on success, FAIL on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5SC_set_extent_notify(H5SC_t *cache, H5D_t *dset, hsize_t *old_dims)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(cache);
    assert(dset);
    assert(old_dims);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SC_set_extent_notify() */
