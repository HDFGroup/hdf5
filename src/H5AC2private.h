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
 * Created:		H5AC2private.h
 *			Jul  9 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Constants and typedefs available to the rest of the
 *			library.
 *
 * Modifications:	JRM - 6/4/04
 *			Complete re-write for a new caching algorithm
 *			located in H5C.c
 *
 *			JRM - 10/18/07
 *			Copied H5ACprivate.h to H5AC2private.h and reworked
 *			to use H5C2 instead of H5C.  All this is in support
 *			of cache API modifications needed for journaling.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5AC2private_H
#define _H5AC2private_H

#include "H5AC2public.h"	/*public prototypes			*/

/* Pivate headers needed by this header */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Oprivate.h"         /* Object headers                       */
#include "H5Fprivate.h"		/* File access				*/
#include "H5C2private.h"	/* cache				*/

#ifdef H5_METADATA_TRACE_FILE
#define H5AC2__TRACE_FILE_ENABLED	1
#else /* H5_METADATA_TRACE_FILE */
#define H5AC2__TRACE_FILE_ENABLED	0
#endif /* H5_METADATA_TRACE_FILE */

/* Types of metadata objects cached */
typedef enum {
    H5AC2_BT_ID = 0, 	  /*B-tree nodes			       */
    H5AC2_SNODE_ID,	  /*symbol table nodes			       */
    H5AC2_LHEAP_ID,	  /*local heap				       */
    H5AC2_GHEAP_ID,	  /*global heap				       */
    H5AC2_OHDR_ID,	  /*object header			       */
    H5AC2_BT2_HDR_ID,	  /*v2 B-tree header			       */
    H5AC2_BT2_INT_ID,	  /*v2 B-tree internal node		       */
    H5AC2_BT2_LEAF_ID,	  /*v2 B-tree leaf node			       */
    H5AC2_FHEAP_HDR_ID,	  /*fractal heap header			       */
    H5AC2_FHEAP_DBLOCK_ID,/*fractal heap direct block		       */
    H5AC2_FHEAP_IBLOCK_ID,/*fractal heap indirect block		       */
    H5AC2_FSPACE_HDR_ID,  /*free space header			       */
    H5AC2_FSPACE_SINFO_ID,/*free space sections			       */
    H5AC2_SOHM_TABLE_ID,  /*shared object header message master table  */
    H5AC2_SOHM_LIST_ID,   /*shared message index stored as a list      */
    H5AC2_TEST_ID,	  /*test entry -- not used for actual files    */
    H5AC2_NTYPES	  /* Number of types, must be last             */
} H5AC2_type_t;

/* H5AC2_DUMP_STATS_ON_CLOSE should always be FALSE when
 * H5C2_COLLECT_CACHE_STATS is FALSE.
 *
 * When H5C2_COLLECT_CACHE_STATS is TRUE, H5AC2_DUMP_STATS_ON_CLOSE must
 * be FALSE for "make check" to succeed, but may be set to TRUE at other
 * times for debugging purposes.
 *
 * Hence the following, somewhat odd set of #defines.
 */
#if H5C2_COLLECT_CACHE_STATS

#define H5AC2_DUMP_STATS_ON_CLOSE	0

#else /* H5C2_COLLECT_CACHE_STATS */

#define H5AC2_DUMP_STATS_ON_CLOSE	0

#endif /* H5C2_COLLECT_CACHE_STATS */

/* Default max metadata cache size and min clean size are give here.
 * At present, these are the same as those given in H5C2private.h.
 */

#define H5AC2__DEFAULT_MAX_CACHE_SIZE	H5C2__DEFAULT_MAX_CACHE_SIZE
#define H5AC2__DEFAULT_MIN_CLEAN_SIZE	H5C2__DEFAULT_MIN_CLEAN_SIZE


/*
 * Class methods pertaining to caching.	 Each type of cached object will
 * have a constant variable with permanent life-span that describes how
 * to cache the object.	 That variable will be of type H5AC2_class_t and
 * have the following required fields...
 *
 * LOAD:	Loads an object from disk to memory.  The function
 *		should allocate some data structure and return it.
 *
 * FLUSH:	Writes some data structure back to disk.  It would be
 *		wise for the data structure to include dirty flags to
 *		indicate whether it really needs to be written.	 This
 *		function is also responsible for freeing memory allocated
 *		by the LOAD method if the DEST argument is non-zero (by
 *              calling the DEST method).
 *
 * DEST:	Just frees memory allocated by the LOAD method.
 *
 * CLEAR:	Just marks object as non-dirty.
 *
 * SIZE:	Report the size (on disk) of the specified cache object.
 *		Note that the space allocated on disk may not be contiguous.
 */

#define H5AC2__SERIALIZE_RESIZED_FLAG	H5C2__SERIALIZE_RESIZED_FLAG
#define H5AC2__SERIALIZE_RENAMED_FLAG	H5C2__SERIALIZE_RENAMED_FLAG

typedef H5C2_deserialize_func_t		H5AC2_deserialize_func_t;
typedef H5C2_image_len_func_t		H5AC2_image_len_func_t;
typedef H5C2_serialize_func_t		H5AC2_serialize_func_t;
typedef H5C2_free_icr_func_t		H5AC2_free_icr_func_t;
typedef H5C2_clear_dirty_bits_func_t	H5AC2_clear_dirty_bits_func_t;

typedef H5C2_class_t			H5AC2_class_t;



typedef H5C2_cache_entry_t		H5AC2_info_t;


/*===----------------------------------------------------------------------===
 *                             Protect Types
 *===----------------------------------------------------------------------===
 *
 * These are for the wrapper functions to H5AC2_protect. They specify what
 * type of operation you're planning on doing to the metadata. The
 * Flexible Parallel HDF5 locking can then act accordingly.
 */

typedef enum H5AC2_protect_t {
    H5AC2_WRITE,                 /* Protect object for writing                */
    H5AC2_READ                   /* Protect object for reading                */
} H5AC2_protect_t;


/* Typedef for metadata cache (defined in H5C2pkg.h) */
typedef H5C2_t	H5AC2_t;

/* Metadata specific properties for FAPL */
/* (Only used for parallel I/O) */
#ifdef H5_HAVE_PARALLEL
/* Definitions for "block before metadata write" property */
#define H5AC2_BLOCK_BEFORE_META_WRITE_NAME       "H5AC2_block_before_meta_write"
#define H5AC2_BLOCK_BEFORE_META_WRITE_SIZE       sizeof(unsigned)
#define H5AC2_BLOCK_BEFORE_META_WRITE_DEF        0

/* Definitions for "library internal" property */
#define H5AC2_LIBRARY_INTERNAL_NAME       "H5AC2_library_internal"
#define H5AC2_LIBRARY_INTERNAL_SIZE       sizeof(unsigned)
#define H5AC2_LIBRARY_INTERNAL_DEF        0
#endif /* H5_HAVE_PARALLEL */

/* Dataset transfer property list for flush calls */
/* (Collective set, "block before metadata write" set and "library internal" set) */
/* (Global variable declaration, definition is in H5AC2.c) */
extern hid_t H5AC2_dxpl_id;

/* Dataset transfer property list for independent metadata I/O calls */
/* (just "library internal" set - i.e. independent transfer mode) */
/* (Global variable declaration, definition is in H5AC2.c) */
extern hid_t H5AC2_ind_dxpl_id;


/* Cache config field limit #defines */

#define H5AC2__MIN_JBRB_BUF_SIZE        H5C2__MIN_JBRB_BUF_SIZE
#define H5AC2__MAX_JBRB_BUF_SIZE        H5C2__MAX_JBRB_BUF_SIZE

#define H5AC2__MIN_JBRB_NUM_BUFS        H5C2__MIN_JBRB_NUM_BUFS
#define H5AC2__MAX_JBRB_NUM_BUFS        H5C2__MAX_JBRB_NUM_BUFS


/* Default cache configuration. */

#define H5AC2__DEFAULT_CACHE_CONFIG                                           \
{                                                                             \
  /* int         version                 = */ H5C2__CURR_AUTO_SIZE_CTL_VER,   \
  /* hbool_t     rpt_fcn_enabled         = */ FALSE,                          \
  /* hbool_t     open_trace_file         = */ FALSE,                          \
  /* hbool_t     close_trace_file        = */ FALSE,                          \
  /* char        trace_file_name[]       = */ "",                             \
  /* hbool_t     evictions_enabled       = */ TRUE,                           \
  /* hbool_t     set_initial_size        = */ TRUE,                           \
  /* size_t      initial_size            = */ ( 1 * 1024 * 1024 ),            \
  /* double      min_clean_fraction      = */ 0.5,                            \
  /* size_t      max_size                = */ (16 * 1024 * 1024 ),            \
  /* size_t      min_size                = */ ( 1 * 1024 * 1024 ),            \
  /* long int    epoch_length            = */ 50000,                          \
  /* enum H5C2_cache_incr_mode incr_mode = */ H5C2_incr__threshold,           \
  /* double      lower_hr_threshold      = */ 0.9,                            \
  /* double      increment               = */ 2.0,                            \
  /* hbool_t     apply_max_increment     = */ TRUE,                           \
  /* size_t      max_increment           = */ (4 * 1024 * 1024),              \
  /* enum H5C2_cache_flash_incr_mode       */                                 \
  /*                    flash_incr_mode  = */ H5C2_flash_incr__add_space,     \
  /* double      flash_multiple          = */ 1.0,                            \
  /* double      flash_threshold         = */ 0.25,                           \
  /* enum H5C2_cache_decr_mode decr_mode = */                                 \
	                                   H5C2_decr__age_out_with_threshold, \
  /* double      upper_hr_threshold      = */ 0.999,                          \
  /* double      decrement               = */ 0.9,                            \
  /* hbool_t     apply_max_decrement     = */ TRUE,                           \
  /* size_t      max_decrement           = */ (1 * 1024 * 1024),              \
  /* int         epochs_before_eviction  = */ 3,                              \
  /* hbool_t     apply_empty_reserve     = */ TRUE,                           \
  /* double      empty_reserve           = */ 0.1,                            \
  /* int	 dirty_bytes_threshold   = */ (256 * 1024),                   \
  /* hbool_t     enable_journaling       = */ FALSE,                          \
  /* char        journal_file_path[]     = */ "",                             \
  /* hbool_t     journal_recovered       = */ FALSE,                          \
  /* size_t      jbrb_buf_size           = */ (8 * 1024),                     \
  /* int         jbrb_num_bufs           = */ 2,                              \
  /* hbool_t     jbrb_use_aio            = */ FALSE,                          \
  /* hbool_t     jbrb_human_readable     = */ TRUE                            \
}


/*
 * Library prototypes.
 */

/* #defines of flags used in the flags parameters in some of the
 * following function calls.  Note that they are just copies of
 * the equivalent flags from H5C2private.h.
 */

#define H5AC2__NO_FLAGS_SET		   H5C2__NO_FLAGS_SET
#define H5AC2__SET_FLUSH_MARKER_FLAG	   H5C2__SET_FLUSH_MARKER_FLAG
#define H5AC2__DELETED_FLAG		   H5C2__DELETED_FLAG
#define H5AC2__DIRTIED_FLAG		   H5C2__DIRTIED_FLAG
#define H5AC2__SIZE_CHANGED_FLAG	   H5C2__SIZE_CHANGED_FLAG
#define H5AC2__PIN_ENTRY_FLAG		   H5C2__PIN_ENTRY_FLAG
#define H5AC2__UNPIN_ENTRY_FLAG		   H5C2__UNPIN_ENTRY_FLAG
#define H5AC2__FLUSH_INVALIDATE_FLAG	   H5C2__FLUSH_INVALIDATE_FLAG
#define H5AC2__FLUSH_CLEAR_ONLY_FLAG	   H5C2__FLUSH_CLEAR_ONLY_FLAG
#define H5AC2__FLUSH_MARKED_ENTRIES_FLAG   H5C2__FLUSH_MARKED_ENTRIES_FLAG
#define H5AC2__FLUSH_IGNORE_PROTECTED_FLAG H5C2__FLUSH_IGNORE_PROTECTED_FLAG


/* #defines of flags used to report entry status in the
 * H5AC2_get_entry_status() call.
 */

#define H5AC2_ES__IN_CACHE	0x0001
#define H5AC2_ES__IS_DIRTY	0x0002
#define H5AC2_ES__IS_PROTECTED	0x0004
#define H5AC2_ES__IS_PINNED	0x0008


/* external function declarations: */

H5_DLL herr_t H5AC2_init(void);
H5_DLL herr_t H5AC2_check_for_journaling(H5F_t * f,
                                         hid_t dxpl_id,
                                         H5C2_t * cache_ptr,
                                         hbool_t journal_recovered);
H5_DLL herr_t H5AC2_create(H5F_t *f, 
		           hid_t dxpl_id, 
			   H5AC2_cache_config_t *config_ptr);
H5_DLL herr_t H5AC2_begin_transaction(hid_t id,
                                      hbool_t * do_transaction_ptr,
                                      H5O_loc_t * id_oloc_ptr,
                                      hbool_t * id_oloc_open_ptr,
                                      hbool_t * transaction_begun_ptr,
                                      uint64_t * trans_num_ptr,
                                      const char * api_call_name);
H5_DLL herr_t H5AC2_end_transaction(hbool_t do_transaction,
                                    H5O_loc_t * id_oloc_ptr,
                                    hbool_t id_oloc_open,
                                    hbool_t transaction_begun,
                                    uint64_t trans_num,
                                    const char * api_call_name);
H5_DLL herr_t H5AC2_get_entry_status(H5F_t * f, haddr_t addr,
				    unsigned * status_ptr);
H5_DLL herr_t H5AC2_set(H5F_t *f, hid_t dxpl_id, const H5AC2_class_t *type,
                        haddr_t addr, size_t len, void *thing, 
			unsigned int flags);
H5_DLL herr_t H5AC2_pin_protected_entry(H5F_t * f, void *  thing);
H5_DLL void * H5AC2_protect(H5F_t *f, hid_t dxpl_id, const H5AC2_class_t *type,
                           haddr_t addr, size_t len, const void *udata,
                           H5AC2_protect_t rw);
H5_DLL herr_t H5AC2_resize_pinned_entry(H5F_t * f,
                                       void *  thing,
                                       size_t  new_size);
H5_DLL herr_t H5AC2_unpin_entry(void * thing);
H5_DLL herr_t H5AC2_unprotect(H5F_t *f, hid_t dxpl_id,
                             const H5AC2_class_t *type, haddr_t addr,
			     size_t new_size, void *thing, unsigned flags);
H5_DLL herr_t H5AC2_flush(H5F_t *f, hid_t dxpl_id, unsigned flags);
H5_DLL herr_t H5AC2_mark_pinned_entry_dirty(H5F_t * f,
		                           void *  thing,
					   hbool_t size_changed,
                                           size_t  new_size);
H5_DLL herr_t H5AC2_mark_pinned_or_protected_entry_dirty(H5F_t * f,
		                                        void *  thing);
H5_DLL herr_t H5AC2_rename(H5F_t *f, const H5AC2_class_t *type,
			   haddr_t old_addr, haddr_t new_addr);

H5_DLL herr_t H5AC2_dest(H5F_t *f, hid_t dxpl_id);

H5_DLL herr_t H5AC2_expunge_entry(H5F_t *f, hid_t dxpl_id,
                                 const H5AC2_class_t *type, haddr_t addr);

H5_DLL herr_t H5AC2_set_write_done_callback(H5C2_t * cache_ptr,
                                           void (* write_done)(void));
H5_DLL herr_t H5AC2_stats(const H5F_t *f);

H5_DLL herr_t H5AC2_get_cache_auto_resize_config(H5AC2_t * cache_ptr,
                                              H5AC2_cache_config_t *config_ptr);

H5_DLL herr_t H5AC2_get_cache_size(H5AC2_t * cache_ptr,
                                  size_t * max_size_ptr,
                                  size_t * min_clean_size_ptr,
                                  size_t * cur_size_ptr,
                                  int32_t * cur_num_entries_ptr);

H5_DLL herr_t H5AC2_get_cache_hit_rate(H5AC2_t * cache_ptr,
                                      double * hit_rate_ptr);

H5_DLL herr_t H5AC2_reset_cache_hit_rate_stats(H5AC2_t * cache_ptr);

H5_DLL herr_t H5AC2_set_cache_auto_resize_config(H5F_t * f,
                                                 hid_t dxpl_id,
                                                 H5AC2_cache_config_t *config_ptr);

H5_DLL herr_t H5AC2_set_cache_journaling_config(H5F_t * f,
                                                hid_t dxpl_id,
                                                H5AC2_cache_config_t *config_ptr,
                                                hbool_t show_trace);

H5_DLL herr_t H5AC2_validate_config(H5AC2_cache_config_t * config_ptr);

H5_DLL herr_t H5AC2_close_trace_file(H5AC2_t * cache_ptr);

H5_DLL herr_t H5AC2_open_trace_file(H5AC2_t * cache_ptr,
		                   const char * trace_file_name);

#endif /* !_H5AC2private_H */

