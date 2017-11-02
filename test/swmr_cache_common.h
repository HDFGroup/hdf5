/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Houjun Tang
 *              Oct 9, 2017
 *
 *		This file contains common #defines, type definitions, and
 *		externs for tests of the cache implemented in H5C.c
 */
#ifndef _SWMR_CACHE_COMMON_H
#define _SWMR_CACHE_COMMON_H

#define H5C_FRIEND		/*suppress error about including H5Cpkg   */
#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */

/* Include library header files */
#include "H5ACprivate.h"
#include "H5Cpkg.h"
#include "H5Fpkg.h"
#include "H5Iprivate.h"
#include "H5MFprivate.h"
#include "H5MMprivate.h"
#include "H5Cprivate.h"

/* Include test header files */
#include "h5test.h"

/* Macro to make error reporting easier */
#define CACHE_ERROR(s)      {failure_mssg_g = "Line #" H5_TOSTRING(__LINE__) ": " s ; pass_g = FALSE; goto done;}

#define FULLSWMR_CACHE_ENTRY_TYPE		H5AC_SOHM_TABLE_ID
#define FULLSWMR_DELTAT_SECONDS         		1

typedef struct fullswmr_cache_entry_t {
    H5C_cache_entry_t	              header;       /* Information for H5C cache functions */
                                                    /* (MUST be first field in structure) */
    /* General fields */
    hsize_t		             size;         /* how big the cache is */
    int                               index;        /* index number within this setup allocation */
    hbool_t		              is_dirty;	    /* entry has been modified since last write */
} fullswmr_cache_entry_t;

H5TEST_DLLVAR int                      entry_type_id_g;
H5TEST_DLLVAR fullswmr_cache_entry_t  *entry_array_g;  /* stores the pointer to each cache entry allocation */
H5TEST_DLLVAR H5F_t                   *entry_file_ptr_array_g;     
H5TEST_DLLVAR hsize_t                  entry_array_nelem_g;
H5TEST_DLLVAR hsize_t                  entry_array_freed_g;
H5TEST_DLLVAR hsize_t                  entry_array_size_g;
H5TEST_DLLVAR haddr_t                  allocated_base_addr_array_g; 


/* global variable externs: */
H5TEST_DLLVAR const char *failure_mssg_g;
H5TEST_DLLVAR hbool_t pass_g; /* set to false on error */


#ifdef __cplusplus
extern "C" {
#endif

H5TEST_DLL H5F_t *fullswmr_setup_cache(hsize_t entry_size, int n_entry, unsigned file_flags);
H5TEST_DLL void   fullswmr_takedown_cache(H5F_t * file_ptr, hbool_t dump_stats, hbool_t dump_detailed_stats);
H5TEST_DLL void   fullswmr_insert_entry(H5F_t * file_ptr, int idx, unsigned flags);
H5TEST_DLL void   fullswmr_flush_cache(H5F_t * file_ptr, hbool_t destroy_entries, hbool_t dump_stats, hbool_t dump_detailed_stats);
H5TEST_DLL void   fullswmr_expunge_entry(H5F_t * file_ptr, int32_t idx);
H5TEST_DLL void   fullswmr_addr_to_index(haddr_t addr, int *index_ptr);

#ifdef __cplusplus
}
#endif

#endif /* _SWMR_CACHE_COMMON_H */
