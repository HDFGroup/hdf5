/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/*
 * This file contains private information about the H5D module
 */
#ifndef _H5Dprivate_H
#define _H5Dprivate_H

#include "H5Dpublic.h"

/* Private headers needed by this file */
#include "H5private.h"
#include "H5Fprivate.h"		/*for the H5F_t type			     */
#include "H5Gprivate.h"		/*symbol tables				     */
#include "H5MMpublic.h"		/*for H5MM_allocate_t and H5MM_free_t types  */
#include "H5Oprivate.h"		/*object Headers			     */
#include "H5Sprivate.h"		/*for the H5S_t type			     */
#include "H5Tprivate.h"		/*for the H5T_t type			     */

/*
 * Feature: Define H5D_DEBUG on the compiler command line if you want to
 *	    debug dataset I/O. NDEBUG must not be defined in order for this
 *	    to have any effect.
 */
#ifdef NDEBUG
#  undef H5D_DEBUG
#endif

#define H5D_RESERVED_ATOMS  0

/* Set the minimum object header size to create objects with */
#define H5D_MINHDR_SIZE 256

/* ========  Dataset creation properties ======== */
/* Definitions for storage layout property */
#define H5D_CRT_LAYOUT_NAME        "layout"
#define H5D_CRT_LAYOUT_SIZE        sizeof(H5D_layout_t)
#define H5D_CRT_LAYOUT_DEF         H5D_CONTIGUOUS    
/* Definitions for chunk dimensionality property */
#define H5D_CRT_CHUNK_DIM_NAME     "chunk_ndims"
#define H5D_CRT_CHUNK_DIM_SIZE     sizeof(int)
#define H5D_CRT_CHUNK_DIM_DEF      1
/* Definitions for chunk size */
#define H5D_CRT_CHUNK_SIZE_NAME    "chunk_size"
#define H5D_CRT_CHUNK_SIZE_SIZE    sizeof(hsize_t[32])
#define H5D_CRT_CHUNK_SIZE_DEF     {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,\
                                   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}
/* Definitions for fill value.  size=0 means fill value will be 0 as 
 * library default; size=-1 means fill value is undefined. */
#define H5D_CRT_FILL_VALUE_NAME    "fill_value"
#define H5D_CRT_FILL_VALUE_SIZE    sizeof(H5O_fill_t)
#define H5D_CRT_FILL_VALUE_DEF     {NULL, 0, NULL}
/* Definitions for space allocation time */
#define H5D_CRT_ALLOC_TIME_NAME   "alloc_time"
#define H5D_CRT_ALLOC_TIME_SIZE   sizeof(H5D_alloc_time_t)
#define H5D_CRT_ALLOC_TIME_DEF    H5D_ALLOC_TIME_DEFAULT
/* Definitions for time of fill value writing */
#define H5D_CRT_FILL_TIME_NAME     "fill_time"
#define H5D_CRT_FILL_TIME_SIZE     sizeof(H5D_fill_time_t)
#define H5D_CRT_FILL_TIME_DEF      H5D_FILL_TIME_ALLOC 
/* Definitions for external file list */
#define H5D_CRT_EXT_FILE_LIST_NAME "efl"
#define H5D_CRT_EXT_FILE_LIST_SIZE sizeof(H5O_efl_t)
#define H5D_CRT_EXT_FILE_LIST_DEF  {HADDR_UNDEF, 0, 0, NULL}
/* Definitions for data filter pipeline */
#define H5D_CRT_DATA_PIPELINE_NAME "pline"
#define H5D_CRT_DATA_PIPELINE_SIZE sizeof(H5O_pline_t)
#define H5D_CRT_DATA_PIPELINE_DEF  {0, 0, NULL} 

/* ======== Data transfer properties ======== */
/* Definitions for maximum temp buffer size property */
#define H5D_XFER_MAX_TEMP_BUF_NAME       "max_temp_buf"
#define H5D_XFER_MAX_TEMP_BUF_SIZE       sizeof(size_t)
#define H5D_XFER_MAX_TEMP_BUF_DEF  (1024*1024)
/* Definitions for type conversion buffer property */
#define H5D_XFER_TCONV_BUF_NAME       "tconv_buf"
#define H5D_XFER_TCONV_BUF_SIZE       sizeof(void *)
#define H5D_XFER_TCONV_BUF_DEF      NULL
/* Definitions for background buffer property */
#define H5D_XFER_BKGR_BUF_NAME       "bkgr_buf"
#define H5D_XFER_BKGR_BUF_SIZE       sizeof(void *)
#define H5D_XFER_BKGR_BUF_DEF      NULL
/* Definitions for background buffer type property */
#define H5D_XFER_BKGR_BUF_TYPE_NAME       "bkgr_buf_type"
#define H5D_XFER_BKGR_BUF_TYPE_SIZE       sizeof(H5T_bkg_t)
#define H5D_XFER_BKGR_BUF_TYPE_DEF      H5T_BKG_NO
/* Definitions for B-tree node splitting ratio property */
#define H5D_XFER_BTREE_SPLIT_RATIO_NAME       "btree_split_ratio"
#define H5D_XFER_BTREE_SPLIT_RATIO_SIZE       sizeof(double[3])
#define H5D_XFER_BTREE_SPLIT_RATIO_DEF      {0.1, 0.5, 0.9}
#ifdef H5_WANT_H5_V1_4_COMPAT
/* Definitions for hyperslab caching property */
#define H5D_XFER_HYPER_CACHE_NAME       "hyper_cache"
#define H5D_XFER_HYPER_CACHE_SIZE       sizeof(unsigned)
#ifndef H5_HAVE_PARALLEL
#define H5D_XFER_HYPER_CACHE_DEF  1
#else
#define H5D_XFER_HYPER_CACHE_DEF  0
#endif
/* Definitions for hyperslab cache limit property */
#define H5D_XFER_HYPER_CACHE_LIM_NAME       "hyper_cache_limit"
#define H5D_XFER_HYPER_CACHE_LIM_SIZE       sizeof(unsigned)
#define H5D_XFER_HYPER_CACHE_LIM_DEF  0
#endif /* H5_WANT_H5_V1_4_COMPAT */
/* Definitions for vlen allocation function property */
#define H5D_XFER_VLEN_ALLOC_NAME       "vlen_alloc"
#define H5D_XFER_VLEN_ALLOC_SIZE       sizeof(H5MM_allocate_t)
#define H5D_XFER_VLEN_ALLOC_DEF  NULL
/* Definitions for vlen allocation info property */
#define H5D_XFER_VLEN_ALLOC_INFO_NAME       "vlen_alloc_info"
#define H5D_XFER_VLEN_ALLOC_INFO_SIZE       sizeof(void *)
#define H5D_XFER_VLEN_ALLOC_INFO_DEF  NULL
/* Definitions for vlen free function property */
#define H5D_XFER_VLEN_FREE_NAME       "vlen_free"
#define H5D_XFER_VLEN_FREE_SIZE       sizeof(H5MM_free_t)
#define H5D_XFER_VLEN_FREE_DEF  NULL
/* Definitions for vlen free info property */
#define H5D_XFER_VLEN_FREE_INFO_NAME       "vlen_free_info"
#define H5D_XFER_VLEN_FREE_INFO_SIZE       sizeof(void *)
#define H5D_XFER_VLEN_FREE_INFO_DEF  NULL
/* Definitions for file driver ID property */
#define H5D_XFER_VFL_ID_NAME       "vfl_id"
#define H5D_XFER_VFL_ID_SIZE       sizeof(hid_t)
#define H5D_XFER_VFL_ID_DEF  H5FD_VFD_DEFAULT
/* Definitions for file driver info property */
#define H5D_XFER_VFL_INFO_NAME       "vfl_info"
#define H5D_XFER_VFL_INFO_SIZE       sizeof(void *)
#define H5D_XFER_VFL_INFO_DEF  NULL
/* Definitions for hyperslab vector size property */
#define H5D_XFER_HYPER_VECTOR_SIZE_NAME       "vec_size"
#define H5D_XFER_HYPER_VECTOR_SIZE_SIZE       sizeof(size_t)
#define H5D_XFER_HYPER_VECTOR_SIZE_DEF        1024

typedef struct H5D_t H5D_t;
  
/* Functions defined in H5D.c */
H5_DLL herr_t H5D_init(void);
H5_DLL H5D_t *H5D_create(H5G_entry_t *loc, const char *name, 
                          const H5T_t *type, const H5S_t *space, 
                          hid_t dcpl_id);
H5_DLL H5D_t *H5D_open(H5G_entry_t *loc, const char *name);
H5_DLL herr_t H5D_close(H5D_t *dataset);
H5_DLL htri_t H5D_isa(H5G_entry_t *ent);
H5_DLL herr_t H5D_read(H5D_t *dataset, const H5T_t *mem_type,
			const H5S_t *mem_space, const H5S_t *file_space,
			hid_t dset_xfer_plist, void *buf/*out*/);
H5_DLL herr_t H5D_write(H5D_t *dataset, const H5T_t *mem_type,
			 const H5S_t *mem_space, const H5S_t *file_space,
			 hid_t dset_xfer_plist, const void *buf);
H5_DLL herr_t H5D_extend(H5D_t *dataset, const hsize_t *size);
H5_DLL H5G_entry_t *H5D_entof(H5D_t *dataset);
H5_DLL H5T_t *H5D_typeof(H5D_t *dset);
H5_DLL H5S_t *H5D_get_space(H5D_t *dset);
H5_DLL H5D_t * H5D_open_oid(H5G_entry_t *ent);
H5_DLL H5F_t * H5D_get_file(const H5D_t *dset);
H5_DLL hsize_t H5D_get_storage_size(H5D_t *dset);
H5_DLL void *H5D_vlen_get_buf_size_alloc(size_t size, void *info);
H5_DLL herr_t H5D_vlen_get_buf_size(void *elem, hid_t type_id, hsize_t ndim, 
                                     hssize_t *point, void *op_data);
H5_DLL herr_t H5D_crt_copy(hid_t new_plist_t, hid_t old_plist_t, 
                            void *copy_data);
H5_DLL herr_t H5D_crt_close(hid_t dxpl_id, void *close_data);
H5_DLL herr_t H5D_xfer_create(hid_t dxpl_id, void *create_data);
H5_DLL herr_t H5D_xfer_copy(hid_t new_plist_id, hid_t old_plist_id, 
                             void *copy_data);
H5_DLL herr_t H5D_xfer_close(hid_t dxpl_id, void *close_data);
H5_DLL herr_t H5D_set_extent(H5D_t *dataset, const hsize_t *size);
H5_DLL herr_t H5D_flush(H5F_t *f);


#endif
