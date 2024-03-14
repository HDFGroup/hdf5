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
#include "H5private.h"   /* Generic Functions                   */
#include "H5Eprivate.h"  /* Error handling                      */
#include "H5TSpkg.h"     /* Threadsafety                        */

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
    struct _H5TS_win_kwd_node_t *next;
    H5TS_key_t key;
    H5TS_key_destructor_func_t dtor;
    struct _H5TS_win_kwd_tid_node_t *head_tid_node;
} H5TS_win_kwd_node_t;

/* Sub-list of all threads that have set a value for a kwd */
typedef struct _H5TS_win_kwd_tid_node_t {
    struct _H5TS_win_kwd_tid_node_t *next;
    struct _H5TS_win_kwd_tid_node_t *prev;
    uint64_t tid;
    H5TS_win_kwd_node_t *kwd_node;
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
/* Per-thread "key with destructor" ("kwd") info */
static H5TS_key_t H5TS_win_kwd_info_key_s;

/* Pointer to global list of thread-specific kwd's */
static H5TS_win_kwd_node_t *H5TS_win_kwd_list_head_s = NULL;

/* Mutices for access to H5TS_win_kwd_list_head_s & its sub-lists */
static H5TS_mutex_t H5TS_win_kwd_list_mtx_s;
static H5TS_mutex_t H5TS_win_kwd_sublist_mtx_s;
#endif


#ifdef H5_HAVE_WIN_THREADS
/*--------------------------------------------------------------------------
 * Function:    H5TS__win_kwd_init
 *
 * Purpose:     Initialize thread-local "key with destructors"  infrastructure
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5TS__win_kwd_init(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NAMECHECK_ONLY

    /* Initialize the mutices for modifying the kwd list & sub-list */
    H5TS_mutex_init(&H5TS_win_kwd_list_mtx_s);
    H5TS_mutex_init(&H5TS_win_kwd_sublist_mtx_s);

    /* Initialize "base" key for all the thread-specific "keys w/dtors" lists */
    if (H5_UNLIKELY(TLS_OUT_OF_INDEXES == (H5TS_win_kwd_info_key_s = TlsAlloc())))
        ret_value = FAIL;

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)                                                           \
} /* end H5TS__win_kwd_init() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__add_kwd
 *
 * Purpose:     Add a newly created key w/dtor to the global list of keys
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
static herr_t
H5TS__add_kwd(H5TS_key_t key, H5TS_key_destructor_func_t dtor)
{
    H5TS_win_kwd_node_t *kwd_node;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Sanity checks */
    assert(dtor);

    /* Create the kwd node for the key */
    if (H5_UNLIKELY(NULL == (kwd_node = H5MM_malloc(sizeof(*kwd_node)))))
        HGOTO_DONE(FAIL);
    kwd_node->key = key;
    kwd_node->dtor = dtor;

    /* Acquire the lock for accessing the kwd list */
    H5TS_mutex_lock(&H5TS_win_kwd_list_mtx_s);

#ifdef H5TS_DEBUG
    {
        H5TS_win_kwd_node_t *tmp_kwd_node;

        /* Sanity check that the key isn't already in the list */
        tmp_kwd_node = H5TS_win_kwd_list_head_s;
        while (NULL != tmp_kwd_node) {
            if (H5_UNLIKELY(key == tmp_kwd_node->key))
                HGOTO_DONE(FAIL);
            tmp_kwd_node = tmp_kwd_node->next;
        }
    }
#endif

    /* Add the kwd node to the list */
    kwd_node->next = H5TS_win_kwd_list_head_s;
    H5TS_win_kwd_list_head_s = kwd_node;

    /* Release the lock for accessing the kwd list */
    H5TS_mutex_unlock(&H5TS_win_kwd_list_mtx_s);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)                                                           \
} /* H5TS__add_kwd() */

/*--------------------------------------------------------------------------
 * Function:    H5TS__set_kwd
 *
 * Purpose:     Add a newly set key to the list of keys w/dtors for a thread
 *              (if the key has a dtor)
 *
 * Return:      Non-negative on success / Negative on failure
 *
 *--------------------------------------------------------------------------
 */
static herr_t
H5TS__set_kwd(H5TS_key_t key, const void *value)
{
    H5TS_win_kwd_node_t *kwd_node;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Sanity checks */
    assert(value);

    /* Acquire the lock for accessing the kwd list */
    H5TS_mutex_lock(&H5TS_win_kwd_list_mtx_s);

    /* Search the kwd list for the key */
    kwd_node = H5TS_win_kwd_list_head_s;
    while (NULL != kwd_node) {
        if (key == kwd_node->key)
            break;
        kwd_node = kwd_node->next;
    }

    /* Release the lock for accessing the kwd list */
    H5TS_mutex_unlock(&H5TS_win_kwd_list_mtx_s);

    /* Check if this thread has been inserted already */
    if (NULL != kwd_node) {
        H5TS_win_kwd_tid_node_t *kwd_tid_node;
        uint64_t thread_id;

        /* Get the ID for this thread */
        if (H5_UNLIKELY(0 == (thread_id = H5TS_thread_id())))
            HGOTO_DONE(FAIL);

        /* Acquire the lock for accessing the kwd sub-list */
        H5TS_mutex_lock(&H5TS_win_kwd_sublist_mtx_s);

        kwd_tid_node = kwd_node->head_tid_node;
        while (NULL != kwd_tid_node) {
            if (thread_id == kwd_tid_node->tid)
                break;
            kwd_tid_node = kwd_tid_node->next;
        }

        /* Release the lock for accessing the kwd sub-list */
        H5TS_mutex_unlock(&H5TS_win_kwd_sublist_mtx_s);

        /* If this thread isn't in the kwd tid sub-list, add it */
        if (NULL == kwd_tid_node) {
            /* Create the kwd tid node for the thread */
            if (H5_UNLIKELY(NULL == (kwd_tid_node = H5MM_calloc(sizeof(*kwd_tid_node)))))
                HGOTO_DONE(FAIL);
            kwd_tid_node->tid = thread_id;
            kwd_tid_node->kwd_node = kwd_node;

            /* Acquire both locks for accessing the kwd list & sub-lists */
            H5TS_mutex_lock(&H5TS_win_kwd_list_mtx_s);
            H5TS_mutex_lock(&H5TS_win_kwd_sublist_mtx_s);

            /* Insert the new kwd tid node in the sub-list */
            kwd_tid_node->next = kwd_node->head_tid_node;
            if (NULL != kwd_node->head_tid_node)
                kwd_node->head_tid_node->prev = kwd_tid_node;
            kwd_node->head_tid_node = kwd_tid_node;

            /* Release both locks for accessing the kwd list & sub-lists */
            H5TS_mutex_unlock(&H5TS_win_kwd_sublist_mtx_s);
            H5TS_mutex_unlock(&H5TS_win_kwd_list_mtx_s);
        }
    }

    /* Add the kwd node to the list */
    kwd_node->next = H5TS_win_kwd_list_head_s;
    H5TS_win_kwd_list_head_s = kwd_node;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)                                                           \
} /* H5TS__add_kwd() */

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

    if (H5_UNLIKELY(NULL == key))
        HGOTO_DONE(FAIL);

    /* Create the key */
    if (H5_UNLIKELY(TLS_OUT_OF_INDEXES == (*key = TlsAlloc())))
        HGOTO_DONE(FAIL);

    /* If the key has a destructor callback, add it to the list of keys w/dtors */
    if (NULL != dtor)
        if (H5TS__add_kwd(*key, dtor) < 0)
            HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_create() */

/*-------------------------------------------------------------------------
 * Function: H5TS_key_set_value
 *
 * Purpose:  Set a thread-specific value for a key
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

    /* Add the key to the kwd list for this thread, if non-NULL */
    if (NULL != value)
        if (H5_UNLIKELY(H5TS_set_kwd(key, value)))
            HGOTO_DONE(FAIL);

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_key_create() */



#endif

#endif /* H5_HAVE_THREADSAFE */

