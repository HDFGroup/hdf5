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

/*-------------------------------------------------------------------------
 *
 * Created:             H5ACdbg.c
 *
 * Purpose:             Functions for debugging the metadata cache
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#include "H5ACmodule.h"         /* This source code file is part of the H5AC module */
#define H5F_FRIEND		/* Suppress error about including H5Fpkg            */

/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5ACpkg.h"            /* Metadata cache                       */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Fpkg.h"		/* Files				*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5AC_stats
 *
 * Purpose:     Prints statistics about the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Thursday, October 30, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_stats(const H5F_t *f)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);

    /* at present, this can't fail */
    (void)H5C_stats(f->shared->cache, H5F_OPEN_NAME(f), FALSE);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5AC_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5AC_dump_cache
 *
 * Purpose:     Dumps a summary of the contents of the metadata cache
 *              to stdout.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              Sunday, October 10, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC_dump_cache(const H5F_t *f)
{
    herr_t              ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->cache);

    if(H5C_dump_cache(f->shared->cache, H5F_OPEN_NAME(f)) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_dump_cache() failed.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_dump_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__close_trace_file()
 *
 * Purpose:     If a trace file is open, stop logging calls to the cache,
 *              and close the file.
 *
 *              Note that the function does nothing if there is no trace
 *              file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__close_trace_file(H5AC_t *cache_ptr)
{
    FILE *   trace_file_ptr;
    herr_t   ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_PACKAGE

    if(cache_ptr == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "NULL cache_ptr on entry.")

    if(NULL == (trace_file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_get_trace_file_ptr() failed.")

    if(trace_file_ptr != NULL) {
        if(H5C_set_trace_file_ptr(cache_ptr, NULL) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_set_trace_file_ptr() failed.")

        if(HDfclose(trace_file_ptr) != 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close metadata cache trace file")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__close_trace_file() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__open_trace_file()
 *
 * Purpose:     Open a trace file, and start logging calls to the cache.
 *
 * 		This logging is done at the H5C level, and will only take
 * 		place if H5C_TRACE_FILE_ENABLED (defined in H5Cprivate.h)
 * 		is TRUE.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/1/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__open_trace_file(H5AC_t *cache_ptr, const char *trace_file_name)
{
    char     file_name[H5AC__MAX_TRACE_FILE_NAME_LEN + H5C__PREFIX_LEN + 2];
    FILE *   file_ptr;
    herr_t   ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(cache_ptr);

    /* Check args */
    if(cache_ptr == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "cache_ptr NULL on entry.")
    if(trace_file_name == NULL)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "NULL trace_file_name on entry.")
    if(HDstrlen(trace_file_name) > H5AC__MAX_TRACE_FILE_NAME_LEN)
        HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "trace file name too long.")
    if(NULL != (file_ptr = H5C_get_trace_file_ptr(cache_ptr)))
        HGOTO_ERROR(H5E_CACHE, H5E_FILEOPEN, FAIL, "trace file already open.")

#ifdef H5_HAVE_PARALLEL
{
    H5AC_aux_t * aux_ptr;

    aux_ptr = (H5AC_aux_t *)H5C_get_aux_ptr(cache_ptr);
    if(aux_ptr == NULL)
        sprintf(file_name, "%s", trace_file_name);
    else {
	if(aux_ptr->magic != H5AC__H5AC_AUX_T_MAGIC)
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad aux_ptr->magic.")

        sprintf(file_name, "%s.%d", trace_file_name, aux_ptr->mpi_rank);
    } /* end else */

    if(HDstrlen(file_name) > H5AC__MAX_TRACE_FILE_NAME_LEN + H5C__PREFIX_LEN + 1)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "cooked trace file name too long.")
}
#else /* H5_HAVE_PARALLEL */
    HDsnprintf(file_name, (size_t)(H5AC__MAX_TRACE_FILE_NAME_LEN + H5C__PREFIX_LEN + 1), 
               "%s", trace_file_name);
#endif /* H5_HAVE_PARALLEL */

    if((file_ptr = HDfopen(file_name, "w")) == NULL)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "trace file open failed.")

    HDfprintf(file_ptr, "### HDF5 metadata cache trace file version 1 ###\n");

    if(H5C_set_trace_file_ptr(cache_ptr, file_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5C_set_trace_file_ptr() failed.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__open_trace_file() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5AC_cache_is_clean()
 *
 * Purpose:     Debugging function that verifies that all rings in the
 *              metadata cache are clean from the outermost ring, inwards
 *              to the inner ring specified.
 *
 *              Returns TRUE if all specified rings are clean, and FALSE
 *              if not.  Throws an assertion failure on error.
 *
 * Return:      TRUE if the indicated ring(s) are clean, and FALSE otherwise.
 *
 * Programmer:  John Mainzer, 6/18/16
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
hbool_t
H5AC_cache_is_clean(const H5F_t *f, H5AC_ring_t inner_ring)
{
    H5C_t *cache_ptr;
    hbool_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    cache_ptr = f->shared->cache;

    ret_value = H5C_cache_is_clean(cache_ptr, inner_ring);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC_cache_is_clean() */
#endif /* NDEBUG */

