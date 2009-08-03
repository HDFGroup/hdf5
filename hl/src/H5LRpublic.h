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

#ifndef _H5LRpublic_H
#define _H5LRpublic_H

typedef enum H5LR_lang_t {
    H5LR_LANG_ERR = -1, /*this is the first*/
    H5LR_DDL      = 0,  /*for DDL*/
    H5LR_C        = 1,  /*for C*/
    H5LR_FORTRAN  = 2,  /*for Fortran*/
    H5LR_NO_LANG  = 3   /*this is the last*/
} H5LR_lang_t;

#ifdef __cplusplus
extern "C" {
#endif

/*-------------------------------------------------------------------------
 *
 * Make dataset functions
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t H5LRmake_dataset(hid_t loc_id, 
				 const char *path, 
				 hid_t type_id, const size_t buf_size,
				 const hid_t *loc_id_ref,
				 const hdset_reg_ref_t *buf);

/*-------------------------------------------------------------------------
 *
 * Make and manipulate dataset region references functions
 *
 *-------------------------------------------------------------------------
 */
H5_HLDLL herr_t H5LRcreate_region_references(hid_t obj_id,
					     size_t num_elem,
					     const char **path,
					     const hsize_t *block_coord,
					     hdset_reg_ref_t *buf);

H5_HLDLL herr_t H5LRcopy_references(hid_t obj_id, hdset_reg_ref_t *ref, const char *file,
				    const char *path, const hsize_t *block_coord, 
				    hdset_reg_ref_t *ref_new);

H5_HLDLL herr_t H5LRcopy_region(hid_t obj_id,
				hdset_reg_ref_t *ref,
				const char *file,
				const char *path,
				const hsize_t *block_coord);

H5_HLDLL herr_t H5LRcreate_ref_to_all(hid_t loc_id, const char *group_path,
					 const char *ds_path, H5_index_t index_type, H5_iter_order_t order, H5R_type_t ref_type);

/*-------------------------------------------------------------------------
 *
 * Read dataset functions
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t H5LRread_region(hid_t obj_id,
				const hdset_reg_ref_t *ref, 
				hid_t mem_type,
				size_t *numelem,
				void *buf );

/*-------------------------------------------------------------------------
 *
 * Query dataset functions
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t H5LRget_region_info(hid_t obj_id,
				    const hdset_reg_ref_t *ref,  
				    size_t *len, 
				    char *path,
				    int *rank,
				    hid_t *dtype,
				    H5S_sel_type *sel_type,
				    size_t *numelem,
				    hsize_t *buf );

/*-------------------------------------------------------------------------
 *
 * General functions
 *
 *-------------------------------------------------------------------------
 */


/*-------------------------------------------------------------------------
 *
 * Utility functions
 *
 *-------------------------------------------------------------------------
 */


#ifdef __cplusplus
}
#endif

#endif

