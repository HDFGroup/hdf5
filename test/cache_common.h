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
 *        This file contains common #defines, type definitions, and
 *        externs for tests of the cache implemented in H5C.c
 */
#ifndef CACHE_COMMON_H
#define CACHE_COMMON_H

#define H5C_FRIEND /*suppress error about including H5Cpkg   */
#define H5F_FRIEND /*suppress error about including H5Fpkg      */

/* Include library header files */
#include "H5ACprivate.h"
#include "H5MFprivate.h"
#include "H5Cpkg.h"
#include "H5Fpkg.h"
#include "H5Iprivate.h"
#include "H5VLprivate.h" /* Virtual Object Layer                     */

/* Include test header files */
#include "h5test.h"

/* Macro to make error reporting easier */
#define CACHE_ERROR(s)                                                                                       \
    {                                                                                                        \
        failure_mssg = "Line #" H5_TOSTRING(__LINE__) ": " s;                                                \
        pass         = false;                                                                                \
        goto done;                                                                                           \
    }

#define NO_CHANGE -1

/* with apologies for the abuse of terminology... */

#define PICO_ENTRY_TYPE     0
#define NANO_ENTRY_TYPE     1
#define MICRO_ENTRY_TYPE    2
#define TINY_ENTRY_TYPE     3
#define SMALL_ENTRY_TYPE    4
#define MEDIUM_ENTRY_TYPE   5
#define LARGE_ENTRY_TYPE    6
#define HUGE_ENTRY_TYPE     7
#define MONSTER_ENTRY_TYPE  8
#define VARIABLE_ENTRY_TYPE 9
#define NOTIFY_ENTRY_TYPE   10

#define NUMBER_OF_ENTRY_TYPES 11

#define PICO_ENTRY_SIZE     (size_t)1
#define NANO_ENTRY_SIZE     (size_t)4
#define MICRO_ENTRY_SIZE    (size_t)16
#define TINY_ENTRY_SIZE     (size_t)64
#define SMALL_ENTRY_SIZE    (size_t)256
#define MEDIUM_ENTRY_SIZE   (size_t)1024
#define LARGE_ENTRY_SIZE    (size_t)(4 * 1024)
#define HUGE_ENTRY_SIZE     (size_t)(16 * 1024)
#define MONSTER_ENTRY_SIZE  (size_t)(64 * 1024)
#define VARIABLE_ENTRY_SIZE (size_t)(10 * 1024)
#define NOTIFY_ENTRY_SIZE   (size_t)1

#define NUM_PICO_ENTRIES     (10 * 1024)
#define NUM_NANO_ENTRIES     (10 * 1024)
#define NUM_MICRO_ENTRIES    (10 * 1024)
#define NUM_TINY_ENTRIES     (10 * 1024)
#define NUM_SMALL_ENTRIES    (10 * 1024)
#define NUM_MEDIUM_ENTRIES   (10 * 1024)
#define NUM_LARGE_ENTRIES    (10 * 1024)
#define NUM_HUGE_ENTRIES     (10 * 1024)
#define NUM_MONSTER_ENTRIES  (10 * 1024)
#define NUM_VARIABLE_ENTRIES (10 * 1024)
#define NUM_NOTIFY_ENTRIES   (10 * 1024)

#define MAX_ENTRIES (10 * 1024)

/* The choice of the BASE_ADDR below is arbitrary -- it just has to be
 * larger than the superblock.
 */
#define BASE_ADDR          (haddr_t)1024
#define PICO_BASE_ADDR     BASE_ADDR
#define NANO_BASE_ADDR     (haddr_t)(PICO_BASE_ADDR + (PICO_ENTRY_SIZE * NUM_PICO_ENTRIES))
#define MICRO_BASE_ADDR    (haddr_t)(NANO_BASE_ADDR + (NANO_ENTRY_SIZE * NUM_NANO_ENTRIES))
#define TINY_BASE_ADDR     (haddr_t)(MICRO_BASE_ADDR + (MICRO_ENTRY_SIZE * NUM_MICRO_ENTRIES))
#define SMALL_BASE_ADDR    (haddr_t)(TINY_BASE_ADDR + (TINY_ENTRY_SIZE * NUM_TINY_ENTRIES))
#define MEDIUM_BASE_ADDR   (haddr_t)(SMALL_BASE_ADDR + (SMALL_ENTRY_SIZE * NUM_SMALL_ENTRIES))
#define LARGE_BASE_ADDR    (haddr_t)(MEDIUM_BASE_ADDR + (MEDIUM_ENTRY_SIZE * NUM_MEDIUM_ENTRIES))
#define HUGE_BASE_ADDR     (haddr_t)(LARGE_BASE_ADDR + (LARGE_ENTRY_SIZE * NUM_LARGE_ENTRIES))
#define MONSTER_BASE_ADDR  (haddr_t)(HUGE_BASE_ADDR + (HUGE_ENTRY_SIZE * NUM_HUGE_ENTRIES))
#define VARIABLE_BASE_ADDR (haddr_t)(MONSTER_BASE_ADDR + (MONSTER_ENTRY_SIZE * NUM_MONSTER_ENTRIES))
#define NOTIFY_BASE_ADDR   (haddr_t)(VARIABLE_BASE_ADDR + (VARIABLE_ENTRY_SIZE * NUM_VARIABLE_ENTRIES))

#define PICO_ALT_BASE_ADDR     (haddr_t)(NOTIFY_BASE_ADDR + (NOTIFY_ENTRY_SIZE * NUM_NOTIFY_ENTRIES))
#define NANO_ALT_BASE_ADDR     (haddr_t)(PICO_ALT_BASE_ADDR + (PICO_ENTRY_SIZE * NUM_PICO_ENTRIES))
#define MICRO_ALT_BASE_ADDR    (haddr_t)(NANO_ALT_BASE_ADDR + (NANO_ENTRY_SIZE * NUM_NANO_ENTRIES))
#define TINY_ALT_BASE_ADDR     (haddr_t)(MICRO_ALT_BASE_ADDR + (MICRO_ENTRY_SIZE * NUM_MICRO_ENTRIES))
#define SMALL_ALT_BASE_ADDR    (haddr_t)(TINY_ALT_BASE_ADDR + (TINY_ENTRY_SIZE * NUM_TINY_ENTRIES))
#define MEDIUM_ALT_BASE_ADDR   (haddr_t)(SMALL_ALT_BASE_ADDR + (SMALL_ENTRY_SIZE * NUM_SMALL_ENTRIES))
#define LARGE_ALT_BASE_ADDR    (haddr_t)(MEDIUM_ALT_BASE_ADDR + (MEDIUM_ENTRY_SIZE * NUM_MEDIUM_ENTRIES))
#define HUGE_ALT_BASE_ADDR     (haddr_t)(LARGE_ALT_BASE_ADDR + (LARGE_ENTRY_SIZE * NUM_LARGE_ENTRIES))
#define MONSTER_ALT_BASE_ADDR  (haddr_t)(HUGE_ALT_BASE_ADDR + (HUGE_ENTRY_SIZE * NUM_HUGE_ENTRIES))
#define VARIABLE_ALT_BASE_ADDR (haddr_t)(MONSTER_ALT_BASE_ADDR + (MONSTER_ENTRY_SIZE * NUM_MONSTER_ENTRIES))
#define NOTIFY_ALT_BASE_ADDR   (haddr_t)(VARIABLE_ALT_BASE_ADDR + (VARIABLE_ENTRY_SIZE * NUM_VARIABLE_ENTRIES))
#define MAX_ADDR               (haddr_t)(NOTIFY_ALT_BASE_ADDR + (NOTIFY_ENTRY_SIZE * NUM_NOTIFY_ENTRIES))
#define ADDR_SPACE_SIZE        (haddr_t)(MAX_ADDR - BASE_ADDR)

/***********************************************************************
 *
 * Macro:       H5C_FLUSH_CACHE
 *
 * Purpose:     Wrap a call to H5C_flush_cache() in calls to
 *              H5C_set_slist_enabled() to setup and take down the slist.
 *
 *              This is necessary, as H5C_flush_cache() needs the
 *              slist to be active.  Further, since it is called
 *              repeatedly during file flush, it would be inefficient
 *              for it to setup the slist on entry, and take it down
 *              on exit.
 *
 *              On error, set pass to false, and set failure_mssg
 *              to the supplied error message.
 *
 * Return:      N/A
 *
 ***********************************************************************/

#define H5C_FLUSH_CACHE(file, flags, fail_mssg)                                                              \
    {                                                                                                        \
        herr_t rslt;                                                                                         \
                                                                                                             \
        rslt = H5C_set_slist_enabled((file)->shared->cache, true, true);                                     \
                                                                                                             \
        if (rslt >= 0) {                                                                                     \
                                                                                                             \
            rslt = H5C_flush_cache((file), (flags));                                                         \
        }                                                                                                    \
                                                                                                             \
        if (rslt >= 0) {                                                                                     \
                                                                                                             \
            rslt = H5C_set_slist_enabled((file)->shared->cache, false, false);                               \
        }                                                                                                    \
                                                                                                             \
        if (rslt < 0) {                                                                                      \
                                                                                                             \
            pass         = false;                                                                            \
            failure_mssg = (fail_mssg);                                                                      \
        }                                                                                                    \
    } /* H5C_FLUSH_CACHE */

#define MAX_PINS                                                                                             \
    8 /* Maximum number of entries that can be                                                               \
       * directly pinned by a single entry.                                                                  \
       */

#define FLUSH_OP__NO_OP          0
#define FLUSH_OP__DIRTY          1
#define FLUSH_OP__RESIZE         2
#define FLUSH_OP__MOVE           3
#define FLUSH_OP__ORDER          4
#define FLUSH_OP__EXPUNGE        5
#define FLUSH_OP__DEST_FLUSH_DEP 6
#define FLUSH_OP__MAX_OP         6

#define MAX_FLUSH_OPS                                                                                        \
    10 /* Maximum number of flush operations                                                                 \
        * that can be associated with a                                                                      \
        * cache entry.                                                                                       \
        */

#define MAX_FLUSH_DEP_PARS                                                                                   \
    8 /* Maximum number of flush dependency                                                                  \
       * parents in the test */

typedef struct flush_op {
    int op_code;         /* integer op code indicating the
                          * operation to be performed.  At
                          * present it must be one of:
                          *
                          *   FLUSH_OP__NO_OP
                          *   FLUSH_OP__DIRTY
                          *   FLUSH_OP__RESIZE
                          *   FLUSH_OP__MOVE
                          *   FLUSH_OP__ORDER
                          */
    int type;            /* type code of the cache entry that
                          * is the target of the operation.
                          * This value is passed into the
                          * function implementing the flush
                          * operation.
                          */
    int idx;             /* index of the cache entry that
                          * is the target of the operation.
                          * This value is passed into the
                          * function implementing the flush
                          * operation.
                          */
    bool flag;           /* boolean flag passed into the
                          * function implementing the flush
                          * operation.  The meaning of the
                          * flag is dependent upon the flush
                          * operation:
                          *
                          * FLUSH_OP__DIRTY: true iff the
                          *   target is pinned, and is to
                          *   be dirtied via the
                          *   H5C_mark_entry_dirty()
                          *   call.
                          *
                          * FLUSH_OP__RESIZE: true iff the
                          *   target is pinned, and is to
                          *   be resized via the
                          *   H5C_resize_entry()
                          *   call.
                          *
                          * FLUSH_OP__MOVE: true iff the
                          *    target is to be moved to
                          *    its main address.
                          */
    size_t size;         /* New target size in the
                          * FLUSH_OP__MOVE operation.
                          * Unused elsewhere.
                          */
    unsigned *order_ptr; /* Pointer to outside counter for
                          * recording the order of entries
                          * flushed.
                          */
} flush_op;

typedef enum test_entry_action_t {
    TEST_ENTRY_ACTION_NUL = 0, /* No action on entry */
    TEST_ENTRY_ACTION_MOVE     /* Entry is being moved */
} test_entry_action_t;

typedef struct test_entry_t {
    H5C_cache_entry_t header;     /* entry data used by the cache
                                   * -- must be first
                                   */
    struct test_entry_t *self;    /* pointer to this entry -- used for
                                   * sanity checking.
                                   */
    test_entry_action_t action;   /* Action being performed on a test entry */
    H5F_t              *file_ptr; /* pointer to the file in which the
                                   * entry resides, or NULL if the entry
                                   * is not in a file.
                                   */
    H5C_t *cache_ptr;             /* pointer to the cache in which
                                   * the entry resides, or NULL if the
                                   * entry is not in cache.
                                   */
    bool written_to_main_addr;
    /* Flag indicating whether an image
     * of the entry has been written to
     * its main address.  Since we no
     * longer have a flush callback, we
     * set this field to true whenever the
     * entry is serialized while at its
     * main address.
     */
    bool written_to_alt_addr;
    /* Flag indicating whether an image
     * of the entry has been written to
     * its alternate address.  Since we no
     * longer have a flush callback, we
     * set this field to true whenever the
     * entry is serialized while at its
     * alternate address.
     */
    haddr_t addr;                                    /* where the cache thinks this entry
                                                      * is located
                                                      */
    bool at_main_addr;                               /* boolean flag indicating whether
                                                      * the entry is supposed to be at
                                                      * either its main or alternate
                                                      * address.
                                                      */
    haddr_t main_addr;                               /* initial location of the entry
                                                      */
    haddr_t alt_addr;                                /* location to which the entry
                                                      * can be relocated or "moved"
                                                      */
    size_t size;                                     /* how big the cache thinks this
                                                      * entry is
                                                      */
    int32_t type;                                    /* indicates which entry array this
                                                      * entry is in
                                                      */
    int32_t index;                                   /* index in its entry array
                                                      */
    int32_t serializes;                              /* number of times this entry has
                                                      * been serialized.
                                                      */
    int32_t deserializes;                            /* number of times this entry has
                                                      * been deserialized
                                                      */
    bool is_dirty;                                   /* entry has been modified since
                                                      * last write
                                                      */
    bool is_protected;                               /* entry should currently be on
                                                      * the cache's protected list.
                                                      */
    bool is_read_only;                               /* true iff the entry should be
                                                      * protected read only.
                                                      */
    int ro_ref_count;                                /* Number of outstanding read only
                                                      * protects on the entry.
                                                      */
    bool is_pinned;                                  /* entry is currently pinned in
                                                      * the cache.
                                                      */
    haddr_t tag;                                     /* the base_addr as tag for corking entries */
    bool    is_corked;                               /* entry is currently corked or not */
    int     pinning_ref_count;                       /* Number of entries that
                                                      * pin this entry in the cache.
                                                      * When this count drops to zero,
                                                      * this entry should be unpinned.
                                                      */
    int num_pins;                                    /* Number of entries that this
                                                      * entry pins in the cache.  This
                                                      * value must be in the range
                                                      * [0, MAX_PINS].
                                                      */
    int pin_type[MAX_PINS];                          /* array of the types of entries
                                                      * pinned by this entry.
                                                      */
    int pin_idx[MAX_PINS];                           /* array of the indices of
                                                      * entries pinned by this entry.
                                                      */
    int num_flush_ops;                               /* integer field containing the
                                                      * number of flush operations to
                                                      * be executed when the entry is
                                                      * flushed.  This value must lie in
                                                      * the closed interval
                                                      * [0, MAX_FLUSH_OPS].
                                                      */
    struct flush_op flush_ops[MAX_FLUSH_OPS];        /* Array of instances
                                                      * of struct flush_op detailing the
                                                      * flush operations (if any) that
                                                      * are to be executed when the entry
                                                      * is flushed from the cache.
                                                      *
                                                      * num_flush_ops contains the number
                                                      * of valid entries in this array.
                                                      */
    bool flush_op_self_resize_in_progress;           /* Boolean flag
                                                      * that is set to true iff this
                                                      * entry is being flushed, it has
                                                      * been resized by a resize flush
                                                      * op, and the flush function has
                                                      * not yet returned,  This field is
                                                      * used to turn off overactive sanity
                                                      * checking code that would otherwise
                                                      * cause a false test failure.
                                                      */
    bool deserialized;                               /* entry has been deserialized since
                                                      * the last time it was reset.
                                                      */
    bool serialized;                                 /* entry has been serialized since the
                                                      * last time it was reset.
                                                      */
    bool destroyed;                                  /* entry has been destroyed since the
                                                      * last time it was reset.
                                                      */
    bool expunged;                                   /* entry has been expunged since the
                                                      * last time it was reset.
                                                      */
    int      flush_dep_par_type[MAX_FLUSH_DEP_PARS]; /* Entry types of flush dependency parents */
    int      flush_dep_par_idx[MAX_FLUSH_DEP_PARS];  /* Indices of flush dependency parents */
    unsigned flush_dep_npar;                         /* Number of flush dependency parents */
    unsigned flush_dep_nchd;                         /* Number of flush dependency children */
    unsigned
         flush_dep_ndirty_chd; /* Number of dirty flush dependency children (including grandchildren, etc.) */
    bool pinned_from_client;   /* entry was pinned by client call */
    bool pinned_from_cache;    /* entry was pinned by cache internally */
    unsigned flush_order;      /* Order that entry was flushed in */

    unsigned notify_after_insert_count; /* Count of times that entry was inserted in cache */
    unsigned notify_before_evict_count; /* Count of times that entry was removed in cache */
    size_t   actual_len;                /* Simulate the entry's actual size for a speculative load */
    unsigned max_verify_ct;             /* Maximum # of times to verify an entry's checksum */
    unsigned verify_ct;                 /* Count the # of checksum verification for an entry */
} test_entry_t;

#define H5C_TEST__PRE_HT_SEARCH_SC(cache_ptr, Addr)                                                          \
    if (H5C__PRE_HT_SEARCH_SC_CMP(cache_ptr, Addr)) {                                                        \
        fprintf(stdout, "Pre HT search SC failed.\n");                                                       \
    }

#define H5C_TEST__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, k)                                             \
    if (H5C__POST_SUC_HT_SEARCH_SC_CMP(cache_ptr, entry_ptr, k)) {                                           \
        fprintf(stdout, "Post successful HT search SC failed.\n");                                           \
    }

#define H5C_TEST__POST_HT_SHIFT_TO_FRONT_SC(cache_ptr, entry_ptr, k)                                         \
    if (H5C__POST_HT_SHIFT_TO_FRONT_SC_CMP(cache_ptr, entry_ptr, k)) {                                       \
        fprintf(stdout, "Post HT shift to front failed.\n");                                                 \
    }

#define H5C_TEST__SEARCH_INDEX(cache_ptr, Addr, entry_ptr)                                                   \
    {                                                                                                        \
        int k;                                                                                               \
        H5C_TEST__PRE_HT_SEARCH_SC(cache_ptr, Addr)                                                          \
        k           = H5C__HASH_FCN(Addr);                                                                   \
        (entry_ptr) = (cache_ptr)->index[k];                                                                 \
        while (entry_ptr) {                                                                                  \
            if (H5_addr_eq(Addr, (entry_ptr)->addr)) {                                                       \
                H5C_TEST__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, k)                                     \
                if ((entry_ptr) != (cache_ptr)->index[k]) {                                                  \
                    if ((entry_ptr)->ht_next)                                                                \
                        (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;                                \
                    assert((entry_ptr)->ht_prev != NULL);                                                    \
                    (entry_ptr)->ht_prev->ht_next  = (entry_ptr)->ht_next;                                   \
                    (cache_ptr)->index[k]->ht_prev = (entry_ptr);                                            \
                    (entry_ptr)->ht_next           = (cache_ptr)->index[k];                                  \
                    (entry_ptr)->ht_prev           = NULL;                                                   \
                    (cache_ptr)->index[k]          = (entry_ptr);                                            \
                    H5C_TEST__POST_HT_SHIFT_TO_FRONT_SC(cache_ptr, entry_ptr, k)                             \
                }                                                                                            \
                break;                                                                                       \
            }                                                                                                \
            (entry_ptr) = (entry_ptr)->ht_next;                                                              \
        }                                                                                                    \
    }

/* Macros used in H5AC level tests */

#define CACHE_CONFIGS_EQUAL(a, b, cmp_set_init, cmp_init_size)                                               \
    (((a).version == (b).version) && ((a).rpt_fcn_enabled == (b).rpt_fcn_enabled) &&                         \
     ((a).open_trace_file == (b).open_trace_file) && ((a).close_trace_file == (b).close_trace_file) &&       \
     (((a).open_trace_file == false) || (strcmp((a).trace_file_name, (b).trace_file_name) == 0)) &&          \
     ((a).evictions_enabled == (b).evictions_enabled) &&                                                     \
     ((!cmp_set_init) || ((a).set_initial_size == (b).set_initial_size)) &&                                  \
     ((!cmp_init_size) || ((a).initial_size == (b).initial_size)) &&                                         \
     (H5_DBL_ABS_EQUAL((a).min_clean_fraction, (b).min_clean_fraction)) && ((a).max_size == (b).max_size) && \
     ((a).min_size == (b).min_size) && ((a).epoch_length == (b).epoch_length) &&                             \
     ((a).incr_mode == (b).incr_mode) &&                                                                     \
     (H5_DBL_ABS_EQUAL((a).lower_hr_threshold, (b).lower_hr_threshold)) &&                                   \
     (H5_DBL_ABS_EQUAL((a).increment, (b).increment)) &&                                                     \
     ((a).apply_max_increment == (b).apply_max_increment) && ((a).max_increment == (b).max_increment) &&     \
     ((a).flash_incr_mode == (b).flash_incr_mode) &&                                                         \
     (H5_DBL_ABS_EQUAL((a).flash_multiple, (b).flash_multiple)) &&                                           \
     (H5_DBL_ABS_EQUAL((a).flash_threshold, (b).flash_threshold)) && ((a).decr_mode == (b).decr_mode) &&     \
     (H5_DBL_ABS_EQUAL((a).upper_hr_threshold, (b).upper_hr_threshold)) &&                                   \
     (H5_DBL_ABS_EQUAL((a).decrement, (b).decrement)) &&                                                     \
     ((a).apply_max_decrement == (b).apply_max_decrement) && ((a).max_decrement == (b).max_decrement) &&     \
     ((a).epochs_before_eviction == (b).epochs_before_eviction) &&                                           \
     ((a).apply_empty_reserve == (b).apply_empty_reserve) &&                                                 \
     (H5_DBL_ABS_EQUAL((a).empty_reserve, (b).empty_reserve)) &&                                             \
     ((a).dirty_bytes_threshold == (b).dirty_bytes_threshold) &&                                             \
     ((a).metadata_write_strategy == (b).metadata_write_strategy))

#define XLATE_EXT_TO_INT_MDC_CONFIG(i, e)                                                                    \
    {                                                                                                        \
        (i).version = H5C__CURR_AUTO_SIZE_CTL_VER;                                                           \
        if ((e).rpt_fcn_enabled)                                                                             \
            (i).rpt_fcn = H5C_def_auto_resize_rpt_fcn;                                                       \
        else                                                                                                 \
            (i).rpt_fcn = NULL;                                                                              \
        (i).set_initial_size       = (e).set_initial_size;                                                   \
        (i).initial_size           = (e).initial_size;                                                       \
        (i).min_clean_fraction     = (e).min_clean_fraction;                                                 \
        (i).max_size               = (e).max_size;                                                           \
        (i).min_size               = (e).min_size;                                                           \
        (i).epoch_length           = (long int)((e).epoch_length);                                           \
        (i).incr_mode              = (e).incr_mode;                                                          \
        (i).lower_hr_threshold     = (e).lower_hr_threshold;                                                 \
        (i).increment              = (e).increment;                                                          \
        (i).apply_max_increment    = (e).apply_max_increment;                                                \
        (i).max_increment          = (e).max_increment;                                                      \
        (i).flash_incr_mode        = (e).flash_incr_mode;                                                    \
        (i).flash_multiple         = (e).flash_multiple;                                                     \
        (i).flash_threshold        = (e).flash_threshold;                                                    \
        (i).decr_mode              = (e).decr_mode;                                                          \
        (i).upper_hr_threshold     = (e).upper_hr_threshold;                                                 \
        (i).decrement              = (e).decrement;                                                          \
        (i).apply_max_decrement    = (e).apply_max_decrement;                                                \
        (i).max_decrement          = (e).max_decrement;                                                      \
        (i).epochs_before_eviction = (int)((e).epochs_before_eviction);                                      \
        (i).apply_empty_reserve    = (e).apply_empty_reserve;                                                \
        (i).empty_reserve          = (e).empty_reserve;                                                      \
    }

/* misc type definitions */

struct expected_entry_status {
    int           entry_type;
    int           entry_index;
    size_t        size;
    bool          in_cache;
    bool          at_main_addr;
    bool          is_dirty;
    bool          is_protected;
    bool          is_pinned;
    bool          deserialized;
    bool          serialized;
    bool          destroyed;
    int           flush_dep_par_type[MAX_FLUSH_DEP_PARS]; /* Entry types of flush dependency parents */
    int           flush_dep_par_idx[MAX_FLUSH_DEP_PARS];  /* Indices of flush dependency parents */
    unsigned      flush_dep_npar;                         /* Number of flush dependency parents */
    unsigned      flush_dep_nchd;                         /* Number of flush dependency children */
    unsigned      flush_dep_ndirty_chd;                   /* Number of dirty flush dependency children */
    int           flush_order;                            /* flush order of entry */
    unsigned char is_corked;                              /* cork status of entry */
};

/* global variable externs: */
H5TEST_DLLVAR bool        pass; /* set to false on error */
H5TEST_DLLVAR const char *failure_mssg;

H5TEST_DLLVAR test_entry_t *entries[NUMBER_OF_ENTRY_TYPES];
H5TEST_DLLVAR const int32_t max_indices[NUMBER_OF_ENTRY_TYPES];
H5TEST_DLLVAR const size_t  entry_sizes[NUMBER_OF_ENTRY_TYPES];
H5TEST_DLLVAR const haddr_t base_addrs[NUMBER_OF_ENTRY_TYPES];
H5TEST_DLLVAR const haddr_t alt_base_addrs[NUMBER_OF_ENTRY_TYPES];

/* callback table extern */

H5TEST_DLLVAR const H5C_class_t *types[NUMBER_OF_ENTRY_TYPES];

#ifdef __cplusplus
extern "C" {
#endif

/* function declarations: */

H5TEST_DLL void add_flush_op(int target_type, int target_idx, int op_code, int type, int idx, bool flag,
                             size_t size, unsigned *order);

H5TEST_DLL void addr_to_type_and_index(haddr_t addr, int32_t *type_ptr, int32_t *index_ptr);

H5TEST_DLL void dirty_entry(H5F_t *file_ptr, int32_t type, int32_t idx, bool dirty_pin);

H5TEST_DLL void expunge_entry(H5F_t *file_ptr, int32_t type, int32_t idx);

H5TEST_DLL void insert_entry(H5F_t *file_ptr, int32_t type, int32_t idx, unsigned int flags);

H5TEST_DLL void mark_entry_dirty(int32_t type, int32_t idx);

H5TEST_DLL void move_entry(H5C_t *cache_ptr, int32_t type, int32_t idx, bool main_addr);

H5TEST_DLL void protect_entry(H5F_t *file_ptr, int32_t type, int32_t idx);

H5TEST_DLL void protect_entry_ro(H5F_t *file_ptr, int32_t type, int32_t idx);

H5TEST_DLL void pin_entry(int32_t type, int32_t idx);

H5TEST_DLL bool entry_in_cache(H5C_t *cache_ptr, int32_t type, int32_t idx);

H5TEST_DLL void create_pinned_entry_dependency(H5F_t *file_ptr, int pinning_type, int pinning_idx,
                                               int pinned_type, int pinned_idx);

H5TEST_DLL herr_t create_entry_arrays(void);

H5TEST_DLL void free_entry_arrays(void);

H5TEST_DLL void reset_entries(void);

H5TEST_DLL void cork_entry_type(H5F_t *file_ptr, int32_t type);

H5TEST_DLL void uncork_entry_type(H5F_t *file_ptr, int32_t type);

H5TEST_DLL void resize_entry(H5F_t *file_ptr, int32_t type, int32_t idx, size_t new_size, bool in_cache);

H5TEST_DLL void row_major_scan_forward(H5F_t *file_ptr, int32_t max_index, int32_t lag, bool verbose,
                                       bool reset_stats, bool display_stats, bool display_detailed_stats,
                                       bool do_inserts, bool do_moves, bool move_to_main_addr,
                                       bool do_destroys, bool do_mult_ro_protects, int dirty_destroys,
                                       int dirty_unprotects);

H5TEST_DLL void hl_row_major_scan_forward(H5F_t *file_ptr, int32_t max_index, bool verbose, bool reset_stats,
                                          bool display_stats, bool display_detailed_stats, bool do_inserts);

H5TEST_DLL void row_major_scan_backward(H5F_t *file_ptr, int32_t max_index, int32_t lag, bool verbose,
                                        bool reset_stats, bool display_stats, bool display_detailed_stats,
                                        bool do_inserts, bool do_moves, bool move_to_main_addr,
                                        bool do_destroys, bool do_mult_ro_protects, int dirty_destroys,
                                        int dirty_unprotects);

H5TEST_DLL void hl_row_major_scan_backward(H5F_t *file_ptr, int32_t max_index, bool verbose, bool reset_stats,
                                           bool display_stats, bool display_detailed_stats, bool do_inserts);

H5TEST_DLL void col_major_scan_forward(H5F_t *file_ptr, int32_t max_index, int32_t lag, bool verbose,
                                       bool reset_stats, bool display_stats, bool display_detailed_stats,
                                       bool do_inserts, int dirty_unprotects);

H5TEST_DLL void hl_col_major_scan_forward(H5F_t *file_ptr, int32_t max_index, bool verbose, bool reset_stats,
                                          bool display_stats, bool display_detailed_stats, bool do_inserts,
                                          int dirty_unprotects);

H5TEST_DLL void col_major_scan_backward(H5F_t *file_ptr, int32_t max_index, int32_t lag, bool verbose,
                                        bool reset_stats, bool display_stats, bool display_detailed_stats,
                                        bool do_inserts, int dirty_unprotects);

H5TEST_DLL void hl_col_major_scan_backward(H5F_t *file_ptr, int32_t max_index, bool verbose, bool reset_stats,
                                           bool display_stats, bool display_detailed_stats, bool do_inserts,
                                           int dirty_unprotects);

H5TEST_DLL void flush_cache(H5F_t *file_ptr, bool destroy_entries, bool dump_stats, bool dump_detailed_stats);

H5TEST_DLL void unpin_entry(int32_t type, int32_t idx);

H5TEST_DLL void unprotect_entry(H5F_t *file_ptr, int32_t type, int32_t idx, unsigned int flags);

H5TEST_DLL void verify_clean(void);

H5TEST_DLL void verify_entry_status(H5C_t *cache_ptr, int tag, int num_entries,
                                    struct expected_entry_status expected[]);

H5TEST_DLL void verify_unprotected(void);

H5TEST_DLL void create_flush_dependency(int32_t parent_type, int32_t parent_idx, int32_t child_type,
                                        int32_t child_idx);

H5TEST_DLL void destroy_flush_dependency(int32_t parent_type, int32_t parent_idx, int32_t child_type,
                                         int32_t child_idx);

/*** H5AC level utility functions ***/

H5TEST_DLL bool resize_configs_are_equal(const H5C_auto_size_ctl_t *a, const H5C_auto_size_ctl_t *b,
                                         bool compare_init);

H5TEST_DLL void check_and_validate_cache_hit_rate(hid_t file_id, double *hit_rate_ptr, bool dump_data,
                                                  int64_t min_accesses, double min_hit_rate);

H5TEST_DLL void check_and_validate_cache_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr,
                                              size_t *cur_size_ptr, int32_t *cur_num_entries_ptr,
                                              bool dump_data);

H5TEST_DLL void validate_mdc_config(hid_t file_id, H5AC_cache_config_t *ext_config_ptr, bool compare_init,
                                    int test_num);

/** Debugging functions -- normally commented out ***/
#if 0
H5TEST_DLL void dump_LRU(H5F_t * file_ptr);
#endif

#ifdef __cplusplus
}
#endif

#endif /* CACHE_COMMON_H */
