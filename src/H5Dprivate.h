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
#define H5D_MINHDR_SIZE 512

/* Dataset creation property list */
typedef struct H5D_create_t {
    H5D_layout_t	layout;		/*storage layout		     */
    intn		chunk_ndims;	/*chunk dimensionality		     */
    hsize_t		chunk_size[32];	/*chunk size if chunked storage	     */
    H5O_fill_t		fill;		/*fill value			     */
    H5O_efl_t		efl;		/*external file list		     */
    H5O_pline_t		pline;		/*data filter pipeline		     */
} H5D_create_t;

/* Data transfer properties */
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
/* Definitions for hyperslab caching property */
#define H5D_XFER_HYPER_CACHE_NAME       "hyper_cache"
#define H5D_XFER_HYPER_CACHE_SIZE       sizeof(uintn)
#ifndef H5_HAVE_PARALLEL
#define H5D_XFER_HYPER_CACHE_DEF  1
#else
#define H5D_XFER_HYPER_CACHE_DEF  0
#endif
/* Definitions for hyperslab cache limit property */
#define H5D_XFER_HYPER_CACHE_LIM_NAME       "hyper_cache_limit"
#define H5D_XFER_HYPER_CACHE_LIM_SIZE       sizeof(uintn)
#define H5D_XFER_HYPER_CACHE_LIM_DEF  0
/* Definitions for hyperslab cache limit property */
#define H5D_XFER_HYPER_CACHE_LIM_NAME       "hyper_cache_limit"
#define H5D_XFER_HYPER_CACHE_LIM_SIZE       sizeof(uintn)
#define H5D_XFER_HYPER_CACHE_LIM_DEF  0
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
#ifdef COALESCE_READS
/* Definitions for 'gather reads' property */
#define H5D_XFER_GATHER_READS_NAME       "gather_reads"
#define H5D_XFER_GATHER_READS_SIZE       sizeof(uintn)
#define H5D_XFER_GATHER_READS_DEF  0
#endif /* COALESCE_READS */
/* Definitions for hyperslab vector size property */
#define H5D_XFER_HYPER_VECTOR_SIZE_NAME       "vec_size"
#define H5D_XFER_HYPER_VECTOR_SIZE_SIZE       sizeof(size_t)
#define H5D_XFER_HYPER_VECTOR_SIZE_DEF        1024

typedef struct H5D_t H5D_t;

/* library variables */
__DLLVAR__ const H5D_create_t H5D_create_dflt;

/* Functions defined in H5D.c */
__DLL__ herr_t H5D_init(void);
__DLL__ H5D_t *H5D_create(H5G_entry_t *loc, const char *name,
			  const H5T_t *type, const H5S_t *space,
			  const H5D_create_t *create_parms);
__DLL__ H5D_t *H5D_open(H5G_entry_t *loc, const char *name);
__DLL__ herr_t H5D_close(H5D_t *dataset);
__DLL__ htri_t H5D_isa(H5G_entry_t *ent);
__DLL__ herr_t H5D_read(H5D_t *dataset, const H5T_t *mem_type,
			const H5S_t *mem_space, const H5S_t *file_space,
			hid_t dset_xfer_plist, void *buf/*out*/);
__DLL__ herr_t H5D_write(H5D_t *dataset, const H5T_t *mem_type,
			 const H5S_t *mem_space, const H5S_t *file_space,
			 hid_t dset_xfer_plist, const void *buf);
__DLL__ herr_t H5D_extend(H5D_t *dataset, const hsize_t *size);
__DLL__ H5G_entry_t *H5D_entof(H5D_t *dataset);
__DLL__ H5T_t *H5D_typeof(H5D_t *dset);
__DLL__ H5S_t *H5D_get_space(H5D_t *dset);
__DLL__ H5D_t * H5D_open_oid(H5G_entry_t *ent);
__DLL__ H5F_t * H5D_get_file(const H5D_t *dset);
__DLL__ hsize_t H5D_get_storage_size(H5D_t *dset);
__DLL__ void *H5D_vlen_get_buf_size_alloc(size_t size, void *info);
__DLL__ herr_t H5D_vlen_get_buf_size(void *elem, hid_t type_id, hsize_t ndim, hssize_t *point, void *op_data);
__DLL__ herr_t H5D_xfer_create(hid_t dxpl_id, void *create_data);
__DLL__ herr_t H5D_xfer_close(hid_t dxpl_id, void *close_data);

#endif
