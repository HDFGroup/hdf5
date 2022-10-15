/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:     H5MM.c
 *
 * Purpose:     Memory management functions
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions			*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5MMprivate.h" /* Memory management			*/

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

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
 * Function:    H5MM_xstrdup
 *
 * Purpose:     Duplicates a string, including memory allocation.
 *              NULL is an acceptable value for the input string.
 *
 * Return:      Success:    Pointer to a new string (NULL if s is NULL).
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
char *
H5MM_xstrdup(const char *s)
{
    char *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    if (s)
        if (NULL == (ret_value = HDstrdup(s)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "string duplication failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MM_xstrdup() */

/*-------------------------------------------------------------------------
 * Function:    H5MM_xfree
 *
 * Purpose:     Just like free(3) except the return value (always NULL) can
 *              be assigned to the pointer whose memory was just freed:
 *
 *              thing = H5MM_xfree(thing);
 *
 * Return:      Success:    NULL
 *              Failure:    never fails
 *-------------------------------------------------------------------------
 */
void *
H5MM_xfree(void *mem)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (mem)
        HDfree(mem);

    FUNC_LEAVE_NOAPI(NULL)
} /* end H5MM_xfree() */

/*-------------------------------------------------------------------------
 * Function:    H5MM_xfree_const
 *
 * Purpose:     H5MM_xfree() wrapper that handles const pointers without
 *              warnings. Used for freeing buffers that should be regarded
 *              as const in use but need to be freed when no longer needed.
 *
 * Return:      Success:    NULL
 *              Failure:    never fails
 *-------------------------------------------------------------------------
 */
void *
H5MM_xfree_const(const void *mem)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Cast through uintptr_t to de-const memory */
    H5MM_xfree((void *)(uintptr_t)mem);

    FUNC_LEAVE_NOAPI(NULL)
} /* end H5MM_xfree_const() */
