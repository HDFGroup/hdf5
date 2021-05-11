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
 * Created:		H5TSprivate.h
 *			May 2 2000
 *			Chee Wai LEE
 *
 * Purpose:		Private non-prototype header.
 *
 *-------------------------------------------------------------------------
 */
#ifndef H5TSprivate_H_
#define H5TSprivate_H_

#ifdef H5_HAVE_THREADSAFE
/* Public headers needed by this file */
#ifdef LATER
#include "H5TSpublic.h" /* Public API prototypes */
#endif                  /* LATER */

#ifdef H5_HAVE_WIN_THREADS

/* Library level data structures */

/* Mutexes, Threads, and Attributes */
typedef struct H5TS_mutex_struct {
    CRITICAL_SECTION CriticalSection;
} H5TS_mutex_t;

/* Portability wrappers around Windows Threads types */
typedef CRITICAL_SECTION H5TS_mutex_simple_t;
typedef HANDLE           H5TS_thread_t;
typedef HANDLE           H5TS_attr_t;
typedef DWORD            H5TS_key_t;
typedef INIT_ONCE        H5TS_once_t;

/* Defines */
/* not used on windows side, but need to be defined to something */
#define H5TS_SCOPE_SYSTEM  0
#define H5TS_SCOPE_PROCESS 0
#define H5TS_CALL_CONV     WINAPI

/* Portability function aliases */
#define H5TS_get_thread_local_value(key)        TlsGetValue(key)
#define H5TS_set_thread_local_value(key, value) TlsSetValue(key, value)
#define H5TS_attr_init(attr_ptr)                0
#define H5TS_attr_setscope(attr_ptr, scope)     0
#define H5TS_attr_destroy(attr_ptr)             0
#define H5TS_wait_for_thread(thread)            WaitForSingleObject(thread, INFINITE)
#define H5TS_mutex_init(mutex)                  InitializeCriticalSection(mutex)
#define H5TS_mutex_lock_simple(mutex)           EnterCriticalSection(mutex)
#define H5TS_mutex_unlock_simple(mutex)         LeaveCriticalSection(mutex)

/* Functions called from DllMain */
H5_DLL BOOL CALLBACK H5TS_win32_process_enter(PINIT_ONCE InitOnce, PVOID Parameter, PVOID *lpContex);
H5_DLL void          H5TS_win32_process_exit(void);
H5_DLL herr_t        H5TS_win32_thread_enter(void);
H5_DLL herr_t        H5TS_win32_thread_exit(void);

#define H5TS_thread_id() ((uint64_t)GetCurrentThreadId())

#else /* H5_HAVE_WIN_THREADS */

/* Defines */

/******************************************************************************
 *
 * p-thread recursive R/W lock stats collection macros
 *
 * Macros to maintain statistics on the p-threads recursive R/W lock.
 *
 ******************************************************************************/

/* clang-format off */

#ifdef H5_USE_RECURSIVE_WRITER_LOCKS

#define REC_RW_LOCK_STATS__UPDATE_FOR_RD_LOCK(rw, count_ptr)                  \
do {                                                                          \
    HDassert(rw);                                                             \
    HDassert((rw)->magic == H5TS_RW_LOCK_MAGIC);                       \
    HDassert(count_ptr);                                                      \
    HDassert((count_ptr)->magic == H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC);     \
    HDassert((count_ptr)->rec_lock_count >= 1);                               \
    HDassert(!(count_ptr)->write_lock);                                       \
                                                                              \
    (rw)->stats.read_locks_granted++;                                         \
                                                                              \
    if ( (count_ptr)->rec_lock_count == 1) {                                  \
                                                                              \
        (rw)->stats.real_read_locks_granted++;                                \
                                                                              \
        if ( (rw)->active_readers > (rw)->stats.max_read_locks ) {            \
                                                                              \
            (rw)->stats.max_read_locks = (rw)->active_readers;                \
        }                                                                     \
    }                                                                         \
                                                                              \
    if ( (count_ptr)->rec_lock_count >                                        \
         (rw)->stats.max_read_lock_recursion_depth ) {                        \
                                                                              \
        (rw)->stats.max_read_lock_recursion_depth =                           \
            (count_ptr)->rec_lock_count;                                      \
    }                                                                         \
} while ( FALSE ) /* end REC_RW_LOCK_STATS__UPDATE_FOR_RD_LOCK */


#define REC_RW_LOCK_STATS__UPDATE_FOR_RD_LOCK_DELAY(rw, waiting_count)        \
do {                                                                          \
    HDassert(rw);                                                             \
    HDassert((rw)->magic == H5TS_RW_LOCK_MAGIC);                       \
    HDassert((waiting_count) > 0);                                            \
                                                                              \
    (rw)->stats.read_locks_delayed++;                                         \
                                                                              \
    if ( (rw)->stats.max_read_locks_pending < (waiting_count) ) {             \
                                                                              \
        (rw)->stats.max_read_locks_pending = (waiting_count);                 \
    }                                                                         \
} while ( FALSE ) /* REC_RW_LOCK_STATS__UPDATE_FOR_RD_LOCK_DELAY */


#define REC_RW_LOCK_STATS__UPDATE_FOR_RD_UNLOCK(rw, count_ptr)                \
do {                                                                          \
    HDassert(rw);                                                             \
    HDassert((rw)->magic == H5TS_RW_LOCK_MAGIC);                       \
    HDassert(count_ptr);                                                      \
    HDassert((count_ptr)->magic == H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC);     \
    HDassert((count_ptr)->rec_lock_count >= 0);                               \
    HDassert(!(count_ptr)->write_lock);                                       \
                                                                              \
    (rw)->stats.read_locks_released++;                                        \
                                                                              \
    if ( count_ptr->rec_lock_count == 0) {                                    \
                                                                              \
        (rw)->stats.real_read_locks_released++;                               \
    }                                                                         \
} while ( FALSE ) /* end REC_RW_LOCK_STATS__UPDATE_FOR_RD_UNLOCK */



#define REC_RW_LOCK_STATS__UPDATE_FOR_WR_LOCK(rw, count_ptr)                  \
do {                                                                          \
    HDassert(rw);                                                             \
    HDassert((rw)->magic == H5TS_RW_LOCK_MAGIC);                       \
    HDassert(count_ptr);                                                      \
    HDassert((count_ptr)->magic == H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC);     \
    HDassert((count_ptr)->rec_lock_count >= 1);                               \
    HDassert((count_ptr)->write_lock);                                        \
                                                                              \
    (rw)->stats.write_locks_granted++;                                        \
                                                                              \
    if ( (count_ptr)->rec_lock_count == 1) {                                  \
                                                                              \
        (rw)->stats.real_write_locks_granted++;                               \
                                                                              \
        if ( (rw)->active_writers > (rw)->stats.max_write_locks ) {           \
                                                                              \
            (rw)->stats.max_write_locks = (rw)->active_writers;               \
        }                                                                     \
    }                                                                         \
                                                                              \
    if ( (count_ptr)->rec_lock_count >                                        \
         (rw)->stats.max_write_lock_recursion_depth ) {                       \
                                                                              \
        (rw)->stats.max_write_lock_recursion_depth =                          \
            (count_ptr)->rec_lock_count;                                      \
    }                                                                         \
} while ( FALSE ) /* end REC_RW_LOCK_STATS__UPDATE_FOR_WR_LOCK */


#define REC_RW_LOCK_STATS__UPDATE_FOR_WR_LOCK_DELAY(rw, waiting_count)        \
do {                                                                          \
    HDassert(rw);                                                             \
    HDassert((rw)->magic == H5TS_RW_LOCK_MAGIC);                       \
    HDassert((waiting_count) > 0);                                            \
                                                                              \
    (rw)->stats.write_locks_delayed++;                                        \
                                                                              \
    if ( (rw)->stats.max_write_locks_pending < (waiting_count) ) {            \
                                                                              \
        (rw)->stats.max_write_locks_pending = (waiting_count);                \
    }                                                                         \
} while ( FALSE ) /* REC_RW_LOCK_STATS__UPDATE_FOR_WR_LOCK_DELAY */


#define REC_RW_LOCK_STATS__UPDATE_FOR_WR_UNLOCK(rw, count_ptr)                \
do {                                                                          \
    HDassert(rw);                                                             \
    HDassert((rw)->magic == H5TS_RW_LOCK_MAGIC);                       \
    HDassert(count_ptr);                                                      \
    HDassert((count_ptr)->magic == H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC);     \
    HDassert((count_ptr)->rec_lock_count >= 0);                               \
    HDassert((count_ptr)->write_lock);                                        \
                                                                              \
    (rw)->stats.write_locks_released++;                                       \
                                                                              \
    if ( (count_ptr)->rec_lock_count == 0) {                                  \
                                                                              \
        (rw)->stats.real_write_locks_released++;                              \
    }                                                                         \
} while ( FALSE ) /* end REC_RW_LOCK_STATS__UPDATE_FOR_WR_UNLOCK */

#endif /* H5_USE_RECURSIVE_WRITER_LOCKS */

/* clang-format on */

/* Library level data structures */

/* Mutexes, Threads, and Attributes */
typedef struct H5TS_mutex_struct {
    pthread_t       owner_thread; /* current lock owner */
    pthread_mutex_t atomic_lock;  /* lock for atomicity of new mechanism */
    pthread_cond_t  cond_var;     /* condition variable */
    unsigned int    lock_count;
} H5TS_mutex_t;

/* Portability wrappers around pthread types */
typedef pthread_t       H5TS_thread_t;
typedef pthread_attr_t  H5TS_attr_t;
typedef pthread_mutex_t H5TS_mutex_simple_t;
typedef pthread_key_t   H5TS_key_t;
typedef pthread_once_t  H5TS_once_t;

#ifdef H5_USE_RECURSIVE_WRITER_LOCKS

/******************************************************************************
 *
 * Structure H5TS_rw_lock_stats_t
 *
 * Catchall structure for statistics on the recursive p-threads based
 * recursive R/W lock (see declaration of H5TS_rw_lock_t below).
 *
 * Since the mutex must be held when reading a consistent set of statistics
 * from the recursibe R/W lock, it simplifies matters to bundle them into
 * a single structure.  This structure exists for that purpose.
 *
 * If you modify this structure, be sure to make equivalent changes to
 * the reset_stats initializer in H5TS_rw_lock_reset_stats().
 *
 * Individual fields are discussed below.
 *
 *                                           JRM -- 8/28/20
 *
 * Read lock stats:
 *
 * read_locks_granted: 64 bit integer used to count the total number of read
 *              locks granted.  Note that this includes recursive lock
 *              requests.
 *
 * read_locks_released: 64 bit integer used to count the total number of read
 *              locks released.  Note that this includes recursive lock
 *              release requests.
 *
 * real_read_locks_granted: 64 bit integer used to count the total number of
 *              read locks granted, less any recursive lock requests.
 *
 * real_read_locks_released:  64 bit integer used to count the total number of
 *              read locks released, less any recursive lock releases.
 *
 * max_read_locks; 64 bit integer used to track the maximum number of read
 *              locks active at any point in time.
 *
 * max_read_lock_recursion_depth; 64 bit integer used to track the maximum
 *              recursion depth observed for any read lock.
 *
 * read_locks_delayed: 64 bit integer used to track the number of read locks
 *              that were not granted immediately.
 *
 * max_read_locks_delayed; 64 bit integer used to track the maximum number of
 *              pending read locks at any point in time.
 *
 *
 * Write lock stats:
 *
 * write_locks_granted: 64 bit integer used to count the total number of write
 *              locks granted.  Note that this includes recursive lock
 *              requests.
 *
 * write_locks_released: 64 bit integer used to count the total number of write
 *              locks released.  Note that this includes recursive lock
 *              release requests.
 *
 * real_write_locks_granted: 64 bit integer used to count the total number of
 *              write locks granted, less any recursive lock requests.
 *
 * real_write_locks_released:  64 bit integer used to count the total number of
 *              write locks released, less any recursive lock releases.
 *
 * max_write_locks; 64 bit integer used to track the maximum number of write
 *              locks active at any point in time.  Must be either zero or one.
 *
 * max_write_lock_recursion_depth; 64 bit integer used to track the maximum
 *              recursion depth observed for any write lock.
 *
 * write_locks_delayed: 64 bit integer used to track the number of write locks
 *              that were not granted immediately.
 *
 * max_write_locks_delayed; 64 bit integer used to track the maximum number of
 *              pending write locks at any point in time.
 *
 ******************************************************************************/

typedef struct H5TS_rw_lock_stats_t {

    int64_t read_locks_granted;
    int64_t read_locks_released;
    int64_t real_read_locks_granted;
    int64_t real_read_locks_released;
    int64_t max_read_locks;
    int64_t max_read_lock_recursion_depth;
    int64_t read_locks_delayed;
    int64_t max_read_locks_pending;
    int64_t write_locks_granted;
    int64_t write_locks_released;
    int64_t real_write_locks_granted;
    int64_t real_write_locks_released;
    int64_t max_write_locks;
    int64_t max_write_lock_recursion_depth;
    int64_t write_locks_delayed;
    int64_t max_write_locks_pending;

} H5TS_rw_lock_stats_t;

/******************************************************************************
 *
 * Structure H5TS_rw_lock_t
 *
 * A read / write lock, is a lock that allows either an arbitrary number
 * of readers, or a single writer into a critical region.  A recurssive
 * lock is one that allows a thread that already has a lock (be it read or
 * write) to successfully request the lock again, only droping the lock
 * when the number of un-lock calls equals the number of lock calls.
 *
 * Note that we can't use the p-threads R/W lock, as while it permits
 * recursive read locks, it disallows recursive write locks.
 *
 * This structure is a catchall for the fields needed to implement a
 * p-threads based recursive R/W lock, and for the associate statistics
 * collection fields.
 *
 * This recursive R/W lock implementation is an extension of the R/W lock
 * implementation given in "UNIX network programming" Volume 2, Chapter 8
 * by w. Richard Stevens, 2nd edition.
 *
 * Individual fields are discussed below.
 *
 *                                           JRM  -- 8/28/20
 *
 * magic:       Unsigned 32 bit integer field used for sanity checking.  This
 *              fields must always be set to H5TS_RW_LOCK_MAGIC.
 *              If this structure is allocated dynamically, remember to set
 *              it to some invalid value before discarding the structure.
 *
 * policy       Integer containing a code indicating the precidence policy
 *              used by the R/W lock.  The supported policies are listed
 *              below:
 *
 *              H5TS__RW_LOCK_POLICY__FAVOR_WRITERS:
 *
 *              If selected, the R/W lock will grant access to a pending
 *              writer if there are both pending readers and writers.
 *
 *
 *              --- Define other policies here ---
 *
 *
 * mutex:       Mutex used to maintain mutual exclusion on the fields of
 *              of this structure.
 *
 * readers_cv:  Condition variable used for waiting readers.
 *
 * writers_cv:  Condition variable used for waiting writers.
 *
 * waiting_readers_count: 32 bit integer used to maintain a count of
 *              waiting readers.  This value should always be non-negative.
 *
 * waiting_writers_count: 32 bit integer used to maintain a count of
 *              waiting writers.  This value should always be non-negative.
 *
 * The following two fields could be combined into a single field, with
 * the count of active readers being represented by a positive value, and
 * the number of writers by a negative value.  Two fields are used to
 * facilitate sanity checking.
 *
 * active_readers: 32 bit integer used to maintain a count of
 *              readers that currently hold a read lock.  This value
 *              must be zero if active_writers is positive. It should
 *              never be negative.
 *
 * active_writers: 32 bit integer used to maintain a count of
 *              writers that currently hold a write lock.  This value
 *              must always be either 0 or 1, and must be zero if
 *              active_readers is positive.  It should never be negative.
 *
 * rec_entry_count_key: Instance of pthread_key_t used to maintain
 *              a thread specific lock type and recursive entry count
 *              for all threads holding a lock.
 *
 * stats:       Instance of H5TS_rw_lock_stats_t used to track
 *              statistics on the recursive R/W lock.  See the declaration
 *              of the structure for discussion of its fields.
 *
 *              Note that the stats are gathered into a structure because
 *              we must obtain the mutex when reading the statistics to
 *              avoid changes while the statistics are being read.  Collecting
 *              them into a structure facilitates this.
 *
 ******************************************************************************/

#define H5TS_RW_LOCK_MAGIC 0XABCD

#define H5TS__RW_LOCK_POLICY__FAVOR_WRITERS 0

typedef struct H5TS_rw_lock_t {

    uint32_t                    magic;
    int32_t                     policy;
    pthread_mutex_t             mutex;
    pthread_cond_t              readers_cv;
    pthread_cond_t              writers_cv;
    int32_t                     waiting_readers_count;
    int32_t                     waiting_writers_count;
    int32_t                     active_readers;
    int32_t                     active_writers;
    pthread_key_t               rec_entry_count_key;
    int32_t                     writer_rec_entry_count;
    struct H5TS_rw_lock_stats_t stats;

} H5TS_rw_lock_t;

/******************************************************************************
 *
 * Structure H5TS_rec_entry_count
 *
 * Strucure associated with the reader_rec_entry_count_key defined in
 * H5TS_rw_lock_t.
 *
 * The primary purpose of this structure is to maintain a count of recursive
 * locks so that the lock can be dropped when the count drops to zero.
 *
 * Aditional fields are included for purposes of sanity checking.
 *
 * Individual fields are discussed below.
 *
 *                                           JRM  -- 8/28/20
 *
 * magic:       Unsigned 32 bit integer field used for sanity checking.  This
 *              fields must always be set to
 *              H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC, and should be set to
 *              some invalid value just before the structure is freed.
 *
 * write_lock:  Boolean field that is set to TRUE if the count is for a write
 *              lock, and to FALSE if it is for a read lock.
 *
 * rec_leock_count: Count of ehe number of recursive lock calls, less
 *              the number of recursive unlock calls.  The lock in question
 *              is dropped when the count drops to zero.
 *
 ******************************************************************************/

#define H5TS_PT_REC_RW_REC_ENTRY_COUNT_MAGIC 0XABBA

typedef struct H5TS_rec_entry_count {

    uint32_t magic;
    hbool_t  write_lock;
    int64_t  rec_lock_count;

} H5TS_rec_entry_count;

#endif /* H5_USE_RECURSIVE_WRITER_LOCKS */

/* Scope Definitions */
#define H5TS_SCOPE_SYSTEM                       PTHREAD_SCOPE_SYSTEM
#define H5TS_SCOPE_PROCESS                      PTHREAD_SCOPE_PROCESS
#define H5TS_CALL_CONV                          /* unused - Windows only */

/* Portability function aliases */
#define H5TS_get_thread_local_value(key)        pthread_getspecific(key)
#define H5TS_set_thread_local_value(key, value) pthread_setspecific(key, value)
#define H5TS_attr_init(attr_ptr)                pthread_attr_init((attr_ptr))
#define H5TS_attr_setscope(attr_ptr, scope)     pthread_attr_setscope(attr_ptr, scope)
#define H5TS_attr_destroy(attr_ptr)             pthread_attr_destroy(attr_ptr)
#define H5TS_wait_for_thread(thread)            pthread_join(thread, NULL)
#define H5TS_mutex_init(mutex)                  pthread_mutex_init(mutex, NULL)
#define H5TS_mutex_lock_simple(mutex)           pthread_mutex_lock(mutex)
#define H5TS_mutex_unlock_simple(mutex)         pthread_mutex_unlock(mutex)

/* Pthread-only routines */
H5_DLL uint64_t H5TS_thread_id(void);
H5_DLL void     H5TS_pthread_first_thread_init(void);

#endif /* H5_HAVE_WIN_THREADS */

/* Library-scope global variables */
extern H5TS_once_t H5TS_first_init_g; /* Library initialization */
extern H5TS_key_t  H5TS_errstk_key_g; /* Error stacks */
#ifdef H5_HAVE_CODESTACK
extern H5TS_key_t H5TS_funcstk_key_g; /* Function stacks */
#endif                                /* H5_HAVE_CODESTACK */
extern H5TS_key_t H5TS_apictx_key_g;  /* API contexts */

/* Library-scope routines */
/* (Only used within H5private.h macros) */
H5_DLL herr_t H5TS_mutex_lock(H5TS_mutex_t *mutex);
H5_DLL herr_t H5TS_mutex_unlock(H5TS_mutex_t *mutex);
H5_DLL herr_t H5TS_cancel_count_inc(void);
H5_DLL herr_t H5TS_cancel_count_dec(void);

/* Testing routines */
H5_DLL H5TS_thread_t H5TS_create_thread(void *(*func)(void *), H5TS_attr_t *attr, void *udata);

/* Fully recursive R/W lock related function declarations */
#ifdef H5_USE_RECURSIVE_WRITER_LOCKS
H5_DLL H5TS_rec_entry_count *H5TS_alloc_rec_entry_count(hbool_t write_lock);
H5_DLL void                  H5TS_free_rec_entry_count(void *target_ptr);
H5_DLL herr_t                H5TS_rw_lock_init(H5TS_rw_lock_t *rw_lock_ptr, int policy);
H5_DLL herr_t                H5TS_rw_lock_takedown(H5TS_rw_lock_t *rw_lock_ptr);
H5_DLL herr_t                H5TS_rw_rdlock(H5TS_rw_lock_t *rw_lock_ptr);
H5_DLL herr_t                H5TS_rw_wrlock(H5TS_rw_lock_t *rw_lock_ptr);
H5_DLL herr_t                H5TS_rw_unlock(H5TS_rw_lock_t *rw_lock_ptr);
H5_DLL herr_t H5TS_rw_lock_get_stats(H5TS_rw_lock_t *rw_lock_ptr, H5TS_rw_lock_stats_t *stats_ptr);
H5_DLL herr_t H5TS_rw_lock_reset_stats(H5TS_rw_lock_t *rw_lock_ptr);
H5_DLL herr_t H5TS_rw_lock_print_stats(const char *header_str, H5TS_rw_lock_stats_t *stats_ptr);
#endif

#else /* H5_HAVE_THREADSAFE */

#define H5TS_thread_id() ((uint64_t)0)

#endif /* H5_HAVE_THREADSAFE */

#endif /* H5TSprivate_H_ */
