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

#ifdef H5_HAVE_THREADS

#ifdef H5_HAVE_THREADSAFE
/* Include package's public headers */
#include "H5TSdevelop.h"
#endif /* H5_HAVE_THREADSAFE */

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

#ifdef H5_HAVE_C11_THREADS
/* Static initialization values */
#define H5TS_ONCE_INITIALIZER ONCE_FLAG_INIT

/* Thread macros */
#define H5TS_thread_self()         thrd_current()
#define H5TS_thread_equal(t1, t2)  thrd_equal((t1), (t2))
#define H5TS_THREAD_RETURN_TYPE    H5TS_thread_ret_t
#define H5TS_THREAD_CANCEL_DISABLE 0

/* Mutex macros */
#define H5TS_MUTEX_TYPE_PLAIN     mtx_plain
#define H5TS_MUTEX_TYPE_RECURSIVE (mtx_plain | mtx_recursive)
#else
#ifdef H5_HAVE_WIN_THREADS
/* Static initialization values */
#define H5TS_ONCE_INITIALIZER      INIT_ONCE_STATIC_INIT

/* Thread macros */
#define H5TS_thread_self()         GetCurrentThread()
#define H5TS_thread_equal(t1, t2)  (GetThreadId(t1) == GetThreadId(t2))
#define H5TS_THREAD_RETURN_TYPE    H5TS_thread_ret_t WINAPI
#define H5TS_THREAD_CANCEL_DISABLE 0

/* Mutex macros */
#define H5TS_MUTEX_TYPE_PLAIN      0
#define H5TS_MUTEX_TYPE_RECURSIVE  1
#else
/* Static initialization values */
#define H5TS_ONCE_INITIALIZER      PTHREAD_ONCE_INIT

/* Thread macros */
#define H5TS_thread_self()         pthread_self()
#define H5TS_thread_equal(t1, t2)  pthread_equal((t1), (t2))
#define H5TS_THREAD_RETURN_TYPE    H5TS_thread_ret_t
#define H5TS_THREAD_CANCEL_DISABLE PTHREAD_CANCEL_DISABLE

/* Mutex macros */
#define H5TS_MUTEX_TYPE_PLAIN      0
#define H5TS_MUTEX_TYPE_RECURSIVE  1
#endif
#endif

/* Atomics macros */
#ifdef H5_HAVE_STDATOMIC_H
/* atomic_int */
#define H5TS_atomic_init_int(obj, desired)  atomic_init((obj), (desired))
#define H5TS_atomic_load_int(obj)           atomic_load(obj)
#define H5TS_atomic_store_int(obj, desired) atomic_store((obj), (desired))
#define H5TS_atomic_fetch_add_int(obj, arg) atomic_fetch_add((obj), (arg))
#define H5TS_atomic_fetch_sub_int(obj, arg) atomic_fetch_sub((obj), (arg))
#define H5TS_atomic_destroy_int(obj)        /* void */

/* atomic_uint */
#define H5TS_atomic_init_uint(obj, desired)  atomic_init((obj), (desired))
#define H5TS_atomic_load_uint(obj)           atomic_load(obj)
#define H5TS_atomic_store_uint(obj, desired) atomic_store((obj), (desired))
#define H5TS_atomic_fetch_add_uint(obj, arg) atomic_fetch_add((obj), (arg))
#define H5TS_atomic_fetch_sub_uint(obj, arg) atomic_fetch_sub((obj), (arg))
#define H5TS_atomic_destroy_uint(obj)        /* void */
#endif                                       /* H5_HAVE_STDATOMIC_H */

/****************************/
/* Library Private Typedefs */
/****************************/

/* Key destructor callback */
typedef void (*H5TS_key_destructor_func_t)(void *);

/* Thread pool */
typedef struct H5TS_pool_t H5TS_pool_t;

/* Portability aliases */
#ifdef H5_HAVE_C11_THREADS
typedef thrd_t H5TS_thread_t;
typedef int (*H5TS_thread_start_func_t)(void *);
typedef int       H5TS_thread_ret_t;
typedef tss_t     H5TS_key_t;
typedef mtx_t     H5TS_CAPABILITY("mutex") H5TS_mutex_t;
typedef cnd_t     H5TS_cond_t;
typedef once_flag H5TS_once_t;
typedef void (*H5TS_once_init_func_t)(void);
#else
#ifdef H5_HAVE_WIN_THREADS
typedef HANDLE                 H5TS_thread_t;
typedef LPTHREAD_START_ROUTINE H5TS_thread_start_func_t;
typedef DWORD                  H5TS_thread_ret_t;
typedef DWORD                  H5TS_key_t;
typedef CRITICAL_SECTION       H5TS_CAPABILITY("mutex") H5TS_mutex_t;
typedef CONDITION_VARIABLE     H5TS_cond_t;
typedef PINIT_ONCE             H5TS_once_t;
typedef PINIT_ONCE_FN          H5TS_once_init_func_t
#else
typedef pthread_t H5TS_thread_t;
typedef void *(*H5TS_thread_start_func_t)(void *);
typedef void           *H5TS_thread_ret_t;
typedef pthread_key_t   H5TS_key_t;
typedef pthread_mutex_t H5TS_CAPABILITY("mutex") H5TS_mutex_t;
typedef pthread_cond_t  H5TS_cond_t;
typedef pthread_once_t  H5TS_once_t;
typedef void (*H5TS_once_init_func_t)(void);
#endif
#endif

/* Atomics */
#ifdef H5_HAVE_STDATOMIC_H
typedef atomic_int  H5TS_atomic_int_t;
typedef atomic_uint H5TS_atomic_uint_t;
#else
    typedef struct {
    H5TS_mutex_t mutex;
    int          value;
} H5TS_atomic_int_t;
typedef struct {
    H5TS_mutex_t mutex;
    unsigned     value;
} H5TS_atomic_uint_t;
#endif

/* Thread Barrier */
#ifdef H5_HAVE_PTHREAD_BARRIER
typedef pthread_barrier_t H5TS_barrier_t;
#else
typedef struct H5TS_barrier_t {
    H5TS_mutex_t mutex;
    H5TS_cond_t  cv;
    uint64_t     count;
    uint64_t     entered;
    uint64_t     threshold;
} H5TS_barrier_t;
#endif

/*****************************/
/* Library-private Variables */
/*****************************/

/***************************************/
/* Library-private Function Prototypes */
/***************************************/

#ifdef H5_HAVE_THREADSAFE
/* Library/thread init/term operations */
H5_DLL void H5TS_term_package(void);

/* API locking */
H5_DLL herr_t H5TS_api_lock(void);
H5_DLL herr_t H5TS_api_unlock(void);

/* Retrieve per-thread info */
H5_DLL uint64_t             H5TS_thread_id(void);
H5_DLL struct H5CX_node_t **H5TS_get_api_ctx_ptr(void);
H5_DLL struct H5E_t        *H5TS_get_err_stack(void);
#endif /* H5_HAVE_THREADSAFE */

/* 'Once' operationss */
H5_DLL herr_t H5TS_once(H5TS_once_t *once, H5TS_once_init_func_t func);

/* Mutex operations */
H5_DLL herr_t H5TS_mutex_init(H5TS_mutex_t *mutex, int type);
H5_DLL herr_t H5TS_mutex_lock(H5TS_mutex_t *mutex) H5TS_ACQUIRE(*mutex);
H5_DLL herr_t H5TS_mutex_trylock(H5TS_mutex_t *mutex, bool *acquired) H5TS_TRY_ACQUIRE(SUCCEED, *mutex);
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
H5_DLL herr_t H5TS_key_set_value(H5TS_key_t key, void *value);
H5_DLL herr_t H5TS_key_get_value(H5TS_key_t key, void **value);
H5_DLL herr_t H5TS_key_delete(H5TS_key_t key);

/* Threads */
H5_DLL herr_t H5TS_thread_create(H5TS_thread_t *thread, H5TS_thread_start_func_t func, void *udata);
H5_DLL herr_t H5TS_thread_join(H5TS_thread_t thread, H5TS_thread_ret_t *ret_val);
H5_DLL herr_t H5TS_thread_detach(H5TS_thread_t thread);
H5_DLL herr_t H5TS_thread_setcancelstate(int state, int *oldstate);

/* Thread pools */
H5_DLL herr_t H5TS_pool_create(H5TS_pool_t **pool, unsigned num_threads);
H5_DLL herr_t H5TS_pool_add_task(H5TS_pool_t *pool, H5TS_thread_start_func_t func, void *ctx);
H5_DLL herr_t H5TS_pool_destroy(H5TS_pool_t *pool);

/* Emulated C11 atomics */
#ifndef H5_HAVE_STDATOMIC_H
/* atomic_int */
H5_DLL void H5TS_atomic_init_int(H5TS_atomic_int_t *obj, int desired);
H5_DLL int  H5TS_atomic_load_int(H5TS_atomic_int_t *obj);
H5_DLL void H5TS_atomic_store_int(H5TS_atomic_int_t *obj, int desired);
H5_DLL int  H5TS_atomic_fetch_add_int(H5TS_atomic_int_t *obj, int arg);
H5_DLL int  H5TS_atomic_fetch_sub_int(H5TS_atomic_int_t *obj, int arg);
H5_DLL void H5TS_atomic_destroy_int(H5TS_atomic_int_t *obj);

/* atomic_uint */
H5_DLL void     H5TS_atomic_init_uint(H5TS_atomic_uint_t *obj, unsigned desired);
H5_DLL unsigned H5TS_atomic_load_uint(H5TS_atomic_uint_t *obj);
H5_DLL void     H5TS_atomic_store_uint(H5TS_atomic_uint_t *obj, unsigned desired);
H5_DLL unsigned H5TS_atomic_fetch_add_uint(H5TS_atomic_uint_t *obj, unsigned arg);
H5_DLL unsigned H5TS_atomic_fetch_sub_uint(H5TS_atomic_uint_t *obj, unsigned arg);
H5_DLL void     H5TS_atomic_destroy_uint(H5TS_atomic_uint_t *obj);
#endif /* H5_HAVE_STDATOMIC_H */

/* Barrier related function declarations */
H5_DLL herr_t H5TS__barrier_init(H5TS_barrier_t *barrier, uint64_t count);
H5_DLL herr_t H5TS__barrier_wait(H5TS_barrier_t *barrier);
H5_DLL herr_t H5TS__barrier_destroy(H5TS_barrier_t *barrier);

#endif /* H5_HAVE_THREADS */

#endif /* H5TSprivate_H_ */
