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

/*-------------------------------------------------------------------------
 *
 * Created:     H5TSprivate.h
 *
 * Purpose:     Thread-safety abstractions used by the library
 *
 *-------------------------------------------------------------------------
 */
#ifndef H5TSprivate_H_
#define H5TSprivate_H_

#ifdef H5_HAVE_THREADSAFE
/* Include package's public headers */
#include "H5TSdevelop.h"

/**************************/
/* Library Private Macros */
/**************************/

/* Thread-safety sanity-checking annotations */
#define H5TS_CAPABILITY(x)           H5_ATTR_THREAD_ANNOT(capability(x))
#define H5TS_ACQUIRE(...)            H5_ATTR_THREAD_ANNOT(acquire_capability(__VA_ARGS__))
#define H5TS_ACQUIRE_SHARED(...)     H5_ATTR_THREAD_ANNOT(acquire_shared_capability(__VA_ARGS__))
#define H5TS_RELEASE(...)            H5_ATTR_THREAD_ANNOT(release_capability(__VA_ARGS__))
#define H5TS_RELEASE_SHARED(...)     H5_ATTR_THREAD_ANNOT(release_shared_capability(__VA_ARGS__))
#define H5TS_TRY_ACQUIRE(...)        H5_ATTR_THREAD_ANNOT(try_acquire_capability(__VA_ARGS__))
#define H5TS_TRY_ACQUIRE_SHARED(...) H5_ATTR_THREAD_ANNOT(try_acquire_shared_capability(__VA_ARGS__))

/* Static initialization values */
#ifdef H5_HAVE_WIN_THREADS
#define H5TS_KEY_INITIALIZER { NULL, 0, NULL }
#define H5TS_MUTEX_INITIALIZER { NULL }
#define H5TS_COND_INITIALIZER CONDITION_VARIABLE_INIT
#define H5TS_ONCE_INITIALIZER INIT_ONCE_STATIC_INIT
#else
#define H5TS_KEY_INITIALIZER   (pthread_key_t)0
#define H5TS_MUTEX_INITIALIZER PTHREAD_MUTEX_INITIALIZER
#define H5TS_COND_INITIALIZER  PTHREAD_COND_INITIALIZER
#define H5TS_ONCE_INITIALIZER  PTHREAD_ONCE_INIT
#endif

/* Thread macros */
#ifdef H5_HAVE_WIN_THREADS
#define H5TS_thread_self()        GetCurrentThread()
#define H5TS_thread_equal(t1, t2) (GetThreadId(t1) == GetThreadId(t2))
#else
#define H5TS_thread_self()        pthread_self()
#define H5TS_thread_equal(t1, t2) pthread_equal(t1, t2)
#endif

/****************************/
/* Library Private Typedefs */
/****************************/

/* Key destructor callback */
typedef void (*H5TS_key_destructor_func_t)(void *);

/* Portability aliases */
#ifdef H5_HAVE_WIN_THREADS
typedef HANDLE                 H5TS_thread_t;
typedef LPTHREAD_START_ROUTINE H5TS_thread_start_func_t;
typedef DWORD                  H5TS_key_t;
typedef CRITICAL_SECTION       H5TS_CAPABILITY("mutex") H5TS_mutex_t;
typedef CONDITION_VARIABLE     H5TS_cond_t;
typedef PINIT_ONCE             H5TS_once_t;
typedef PINIT_ONCE_FN          H5TS_once_init_func_t
#else
typedef pthread_t H5TS_thread_t;
typedef void *(*H5TS_thread_start_func_t)(void *);
typedef pthread_key_t   H5TS_key_t;
typedef pthread_mutex_t H5TS_CAPABILITY("mutex") H5TS_mutex_t;
typedef pthread_cond_t  H5TS_cond_t;
typedef pthread_once_t  H5TS_once_t;
typedef void (*H5TS_once_init_func_t)(void);
#endif

/*****************************/
/* Library-private Variables */
/*****************************/

/***************************************/
/* Library-private Function Prototypes */
/***************************************/

/* Library/thread init/term operations */
H5_DLL void H5TS_term_package(void);
#ifdef H5_HAVE_WIN_THREADS
H5_DLL herr_t H5TS_win32_thread_enter(void);
H5_DLL herr_t H5TS_win32_thread_exit(void);
#endif /* H5_HAVE_WIN_THREADS */

/* API locking */
H5_DLL herr_t H5TS_api_lock(void);
H5_DLL herr_t H5TS_api_unlock(void);

/* Retrieve per-thread info */
H5_DLL uint64_t             H5TS_thread_id(void);
H5_DLL struct H5CX_node_t **H5TS_get_api_ctx_ptr(void);
H5_DLL struct H5E_t        *H5TS_get_err_stack(void);

/* 'Once' operationss */
H5_DLL herr_t H5TS_once(H5TS_once_t *once, H5TS_once_init_func_t func);

/* Mutex operations */
H5_DLL herr_t H5TS_mutex_init(H5TS_mutex_t *mutex);
H5_DLL herr_t H5TS_mutex_lock(H5TS_mutex_t *mutex) H5TS_ACQUIRE(*mutex);
H5_DLL herr_t H5TS_mutex_try_lock(H5TS_mutex_t *mutex, bool *acquired) H5TS_TRY_ACQUIRE(SUCCEED, *mutex);
H5_DLL herr_t H5TS_mutex_unlock(H5TS_mutex_t *mutex) H5TS_RELEASE(*mutex);
H5_DLL herr_t H5TS_mutex_destroy(H5TS_mutex_t *mutex);

/* Condition variable operations */
H5_DLL herr_t H5TS_cond_init(H5TS_cond_t *cond);
H5_DLL herr_t H5TS_cond_wait(H5TS_cond_t *cond, H5TS_mutex_t *mutex);
H5_DLL herr_t H5TS_cond_signal(H5TS_cond_t *cond);
H5_DLL herr_t H5TS_cond_broadcast(H5TS_cond_t *cond);
H5_DLL herr_t H5TS_cond_destroy(H5TS_cond_t *cond);

/* Thread-specific keys */
H5_DLL herr_t H5TS_key_create(H5TS_key_t *key, H5TS_key_destructor_func_t dtor);
H5_DLL herr_t H5TS_key_set_value(H5TS_key_t key, const void *value);
H5_DLL herr_t H5TS_key_get_value(H5TS_key_t key, void **value);
H5_DLL herr_t H5TS_key_delete(H5TS_key_t key);

/* Threads */
H5_DLL herr_t H5TS_thread_create(H5TS_thread_t *thread, H5TS_thread_start_func_t func, void *udata);
H5_DLL herr_t H5TS_thread_join(H5TS_thread_t thread);

#else /* H5_HAVE_THREADSAFE */

/* Non-threadsafe code needs this */
#define H5TS_thread_id() ((uint64_t)0)

#endif /* H5_HAVE_THREADSAFE */

#endif /* H5TSprivate_H_ */
