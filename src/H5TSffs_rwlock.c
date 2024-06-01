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
 * Purpose: This file contains support for "fast, fair, scalable" non-recursive
 *        R/W locks, equivalent to the pthread 'pthread_rwlock_t' type and
 *        capabilities.  These locks are designed to spin on local memory to
 *        a thread that is waiting to acquire the lock, instead of contending
 *        for a resource through a common memory location(s).
 *
 *        They are based on the "A Fair Fast Scalable Reader-Writer Lock." paper
 *        by Krieger, Stumm, Umrau, and Hanna:
 *        https://www.researchgate.net/publication/221083709_A_Fair_Fast_Scalable_Reader-Writer_Lock
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

#if defined(H5_HAVE_THREADS) && defined(H5_HAVE_STDATOMIC_H)

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
 * Function: H5TS_ffs_rwlock_init
 *
 * Purpose:  Initialize a H5TS_ffs_rwlock_t (does not allocate it)
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_ffs_rwlock_init(H5TS_ffs_rwlock_t *lock)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check argument */
    if (H5_UNLIKELY(NULL == lock))
        HGOTO_DONE(FAIL);

    /* Init tail pointer */
    *lock = NULL;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_ffs_rwlock_init() */

/*-------------------------------------------------------------------------
 * Function: H5TS_ffs_rwlock_rdlock
 *
 * Purpose:  Acquire a read lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_ffs_rwlock_rdlock(H5TS_ffs_rwlock_t *lock, H5TS_ffs_rwlock_local_t *local)
{
    H5TS_ffs_rwlock_local_t *pred;
    herr_t                   ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check argument */
    if (H5_UNLIKELY(NULL == lock || NULL == local))
        HGOTO_DONE(FAIL);

    /* Initialize local node */
    local->state = H5TS_FFS_RWLOCK_READER;
    local->spin  = true;
    local->next = local->prev = NULL;

    /* Add local node to end of list */
    /* Use atomic_exchange to ensure clean swap in case another thread is
     * also attempting to become the tail of the list.
     */
    pred = H5TS_atomic_exchange_voidp(lock, local);

    /* Check if list has other nodes */
    if (NULL != pred) {
        /* Connect linked list */
        local->prev = pred;
        pred->next  = local;

        /* Spin if the previous node is not an active reader */
        if (H5TS_FFS_RWLOCK_ACTIVE_READER != pred->state)
            /* Spin until we are unblocked by prior lock holder */
            while (local->spin)
                ;
    }

    /* We now have a read lock */

    /* Set our state to active */
    local->state = H5TS_FFS_RWLOCK_ACTIVE_READER;

    /* Unblock the next node if it's a waiting reader before we update our state */
    if (NULL != local->next && H5TS_FFS_RWLOCK_READER == local->next->state)
        local->next->spin = false;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_ffs_rwlock_rdlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_ffs_rwlock_rdunlock
 *
 * Purpose:  Release a read lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_ffs_rwlock_rdunlock(H5TS_ffs_rwlock_t *lock, H5TS_ffs_rwlock_local_t *local)
{
    H5TS_ffs_rwlock_local_t *pred;
    herr_t                   ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check argument */
    if (H5_UNLIKELY(NULL == lock || NULL == local))
        HGOTO_DONE(FAIL);

    /* If there's a previous node, it must be a reader */
    pred = local->prev;
    if (NULL != pred) {
        /* Spin until we're confident that we have a lock on the previous node
         * and that another thread hasn't raced in and removed itself as our
         * previous node.
         */
        H5TS_SPINLOCK_LOCK(&pred->lock);
        while (pred != local->prev) {
            H5TS_SPINLOCK_UNLOCK(&pred->lock);
            pred = local->prev;
            if (NULL == pred)
                break;
            H5TS_SPINLOCK_LOCK(&pred->lock);
        }

        /* Disconnect reader from middle (or end) of list */
        if (NULL != pred) {
            /* Acquire lock on local node */
            H5TS_SPINLOCK_LOCK(&local->lock);

            /* Disconnect from previous node */
            pred->next = NULL;

            /* Check if local node is tail of list */
            if (NULL == local->next)
                /* Attempt to remove local node from tail */
                /* Use atomic compare_and_swap to detect race from another thread
                 * that becomes the tail after we checked.
                 */
                if (!H5TS_atomic_compare_exchange_strong_voidp(lock, &local, local->prev))
                    /* Spin until (new) next node links local node in */
                    while (NULL == local->next)
                        ;

            /* Take local node out of list */
            if (NULL != local->next) {
                local->next->prev = local->prev;
                local->prev->next = local->next;
            }

            /* Release locks, in reverse order acquired */
            H5TS_SPINLOCK_UNLOCK(&local->lock);
            H5TS_SPINLOCK_UNLOCK(&pred->lock);

            /* We've released our lock and can leave now */
            HGOTO_DONE(SUCCEED);
        }
    }

    /* Local node is at head of list */

    /* Acquire lock on local node */
    H5TS_SPINLOCK_LOCK(&local->lock);

    /* Check if local node is tail of list */
    if (NULL == local->next)
        /* Attempt to remove local node from tail */
        /* Use atomic compare_and_swap to detect race from another thread
         * that becomes the tail after we checked.
         */
        if (!H5TS_atomic_compare_exchange_strong_voidp(lock, &local, NULL))
            /* Spin until (new) next node links local node in */
            while (NULL == local->next)
                ;

    /* Check for a following node */
    if (NULL != local->next) {
        /* Unblock next node */
        local->next->spin = false;
        local->next->prev = NULL; /* 'I->prev->prev' in paper is incorrect */
    }

    /* Release lock on local node */
    H5TS_SPINLOCK_UNLOCK(&local->lock);

    /* We've released our read lock now */

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_ffs_rwlock_rdunlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_ffs_rwlock_wrlock
 *
 * Purpose:  Acquire a write lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_ffs_rwlock_wrlock(H5TS_ffs_rwlock_t *lock, H5TS_ffs_rwlock_local_t *local)
{
    H5TS_ffs_rwlock_local_t *pred;
    herr_t                   ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check arguments */
    if (H5_UNLIKELY(NULL == lock || NULL == local))
        HGOTO_DONE(FAIL);

    /* Initialize local node for lock */
    local->state = H5TS_FFS_RWLOCK_WRITER;
    local->spin  = true;
    local->next  = NULL;

    /* Add local node to end of list */
    /* Use atomic_exchange to ensure clean swap in case another thread is
     * also attempting to become the tail of the list.
     */
    pred = H5TS_atomic_exchange_voidp(lock, local);

    /* Check if list has other nodes */
    if (NULL != pred) {
        /* Connect linked list */
        /* (Setting this could unblock a thread in unlock) */
        pred->next = local;

        /* Spin until we are unblocked by prior lock holder */
        while (local->spin)
            ;
    }

    /* We now have a write lock */

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_ffs_rwlock_wrlock() */

/*-------------------------------------------------------------------------
 * Function: H5TS_rwlock_wrunlock
 *
 * Purpose:  Release a write lock
 *
 * Return:   Non-negative on success / Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5TS_ffs_rwlock_wrunlock(H5TS_ffs_rwlock_t *lock, H5TS_ffs_rwlock_local_t *local)
{
    H5TS_ffs_rwlock_local_t *pred      = local;
    herr_t                   ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Check arguments */
    if (H5_UNLIKELY(NULL == lock || NULL == local))
        HGOTO_DONE(FAIL);

    /* Check if local node is tail of list */
    if (NULL == local->next)
        /* Attempt to remove local node from tail */
        /* Use atomic compare_and_swap to detect race from another thread
         * that becomes the tail after we checked.
         */
        /* Note use of 'pred' here, instead of 'local', so that value of
         * 'local' is not overwritten with another node's address, if a
         * different thread races in and becomes the tail of the list.
         */
        if (H5TS_atomic_compare_exchange_strong_voidp(lock, &pred, NULL))
            HGOTO_DONE(SUCCEED);

    /* Someone may have raced in and become the tail of the list.  Spin
     * until they link themselves into the list.
     */
    while (NULL == local->next)
        ; /* spin, until another thread changes local->next */

    /* Unblock next node */
    local->next->prev = NULL;
    local->next->spin = false;

done:
    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_ffs_rwlock_wrunlock() */

#endif /* H5_HAVE_THREADS && H5_HAVE_STDATOMIC_H */
