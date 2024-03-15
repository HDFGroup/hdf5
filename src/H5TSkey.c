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

/*
 * Purpose: This file contains support for thread-local key operations,
 *        equivalent to the pthread 'pthread_key_t' type and capabilities.
 *
 * Note:  Because this threadsafety framework operates outside the library,
 *        it does not use the error stack (although it does use error macros
 *        that don't push errors on a stack) and only uses the "namecheck only"
 *        FUNC_ENTER_* / FUNC_LEAVE_* macros.
 */

/****************/
/* Module Setup */
/****************/

#include "H5TSmodule.h" /* This source code file is part of the H5TS module */

/***********/
/* Headers */
/***********/
#include "H5private.h"  /* Generic Functions                   */
#include "H5Eprivate.h" /* Error handling                      */
#include "H5TSpkg.h"    /* Threadsafety                        */

#ifdef H5_HAVE_THREADSAFE

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/
#ifdef H5_HAVE_WIN_THREADS

/* Forward declarations of kwd node structs */
struct _H5TS_win_kwd_tid_node_t;
struct _H5TS_win_kwd_node_t;

/* Global list of all kwd's */
typedef struct _H5TS_win_kwd_node_t {
    struct _H5TS_win_kwd_node_t     *next;
    H5TS_key_t                       key;
    H5TS_key_destructor_func_t       dtor;
    struct _H5TS_win_kwd_tid_node_t *head_tid_node;
} H5TS_win_kwd_node_t;

/* Sub-list of all threads that have set a value for a kwd */
typedef struct _H5TS_win_kwd_tid_node_t {
    struct _H5TS_win_kwd_tid_node_t *next;
    struct _H5TS_win_kwd_tid_node_t *prev;
    uint64_t                         tid;
    H5TS_win_kwd_node_t             *kwd_node;
} H5TS_win_kwd_tid_node_t;
#endif

/********************/
/* Local Prototypes */
/********************/
#ifdef H5_HAVE_WIN_THREADS
static herr_t H5TS__add_kwd(H5TS_key_t key, H5TS_key_destructor_func_t dtor);
static herr_t H5TS__set_kwd(H5TS_key_t key, const void *value);
#endif

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

#ifdef H5_HAVE_WIN_THREADS
/*-------------------------------------------------------------------------
 * Function: H5TS_key_create
 *
 * Purpose:  Thread-local key creation
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_key_create(H5TS_key_t *key, H5TS_key_destructor_func_t dtor)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Sanity check */
    if (H5_UNLIKELY(NULL == key))
        HGOTO_DONE(FAIL);

    /* Fail if the key has a destructor callback, this is not supported by Windows */
    if (NULL != dtor)
        HGOTO_DONE(FAIL);

    /* Create the key */
    if (H5_UNLIKELY(TLS_OUT_OF_INDEXES == (*key = TlsAlloc())))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_create() */

/*-------------------------------------------------------------------------
 * Function: H5TS_key_set_value
 *
 * Purpose:  Set a thread-specific value for a thread-local key
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_key_set_value(H5TS_key_t key, const void *value)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Set the value for this thread */
    if (H5_UNLIKELY(0 != TlsSetValue(key, (LPVOID)value)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_set_value() */

/*-------------------------------------------------------------------------
 * Function: H5TS_key_get_value
 *
 * Purpose:  Get a thread-specific value for a thread-local key
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_key_get_value(H5TS_key_t key, void **value)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Get the value for this thread */
    if (H5_UNLIKELY(NULL == (*value = TlsGetValue(key))))
        /* Check for possible error, when NULL value is returned */
        if (H5_UNLIKELY(ERROR_SUCCESS != GetLastError()))
            HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_get_value() */

/*-------------------------------------------------------------------------
 * Function: H5TS_key_delete
 *
 * Purpose:  Thread-local key deletion
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_key_delete(H5TS_key_t key)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Delete the key */
    if (TLS_OUT_OF_INDEXES != key)
        if (H5_UNLIKELY(0 == TlsFree(key)))
            HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_delete() */

#else
/*-------------------------------------------------------------------------
 * Function: H5TS_key_create
 *
 * Purpose:  Thread-local key creation
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_key_create(H5TS_key_t *key, H5TS_key_destructor_func_t dtor)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Sanity check */
    if (H5_UNLIKELY(NULL == key))
        HGOTO_DONE(FAIL);

    /* Create the key */
    if (H5_UNLIKELY(pthread_key_create(key, dtor)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_create() */

/*-------------------------------------------------------------------------
 * Function: H5TS_key_set_value
 *
 * Purpose:  Set a thread-specific value for a thread-local key
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_key_set_value(H5TS_key_t key, const void *value)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Set the value for this thread */
    if (H5_UNLIKELY(pthread_setspecific(key, value)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_set_value() */

/*-------------------------------------------------------------------------
 * Function: H5TS_key_get_value
 *
 * Purpose:  Get a thread-specific value for a thread-local key
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_key_get_value(H5TS_key_t key, void **value)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Get the value for this thread */
    /* NOTE: pthread_getspecific() can't fail */
    *value = pthread_getspecific(key);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(SUCCEED)
} /* end H5TS_key_get_value() */

/*-------------------------------------------------------------------------
 * Function: H5TS_key_delete
 *
 * Purpose:  Thread-local key deletion
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_key_delete(H5TS_key_t key)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Delete the key */
    if (H5_UNLIKELY(pthread_key_delete(key)))
        HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_delete() */

#endif

#endif /* H5_HAVE_THREADSAFE */
