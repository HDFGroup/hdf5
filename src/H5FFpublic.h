/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

/*
 * This file contains function prototypes for each exported function in the
 * H5FF module.
 */
#ifndef _H5FFpublic_H
#define _H5FFpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5VLpublic.h" 	/* Public VOL header file		*/

/*****************/
/* Public Macros */
/*****************/

#ifdef __cplusplus
extern "C" {
#endif

#ifdef H5_HAVE_EFF

/*******************/
/* Public Typedefs */
/*******************/

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

/* API wrappers */
H5_DLL hid_t H5Fcreate_ff(const char *filename, unsigned flags, hid_t fcpl,
                          hid_t fapl, hid_t eq_id);
H5_DLL hid_t H5Fopen_ff(const char *filename, unsigned flags, hid_t fapl_id,
                        hid_t eq_id);
H5_DLL herr_t H5Fflush_ff(hid_t object_id, H5F_scope_t scope, hid_t eq_id);
H5_DLL herr_t H5Fclose_ff(hid_t file_id, hid_t eq_id);

H5_DLL hid_t H5Gcreate_ff(hid_t loc_id, const char *name, hid_t lcpl_id,
                          hid_t gcpl_id, hid_t gapl_id,
                          uint64_t trans, hid_t eq_id);
H5_DLL hid_t H5Gopen_ff(hid_t loc_id, const char *name, hid_t gapl_id,
                        uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Gclose_ff(hid_t group_id, hid_t eq_id);

H5_DLL hid_t H5Dcreate_ff(hid_t loc_id, const char *name, hid_t type_id,
                          hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id,
                          uint64_t trans, hid_t eq_id);
H5_DLL hid_t H5Dopen_ff(hid_t loc_id, const char *name, hid_t dapl_id,
                        uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Dwrite_ff(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
                          hid_t file_space_id, hid_t dxpl_id, const void *buf,
                          uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Dread_ff(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
                         hid_t file_space_id, hid_t dxpl_id, void *buf/*out*/,
                         uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Dset_extent_ff(hid_t dset_id, const hsize_t size[], hid_t eq_id);
H5_DLL herr_t H5Dclose_ff(hid_t dset_id, hid_t eq_id);

H5_DLL herr_t H5Tcommit_ff(hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id,
                           hid_t tcpl_id, hid_t tapl_id, uint64_t trans, hid_t eq_id);
H5_DLL hid_t H5Topen_ff(hid_t loc_id, const char *name, hid_t tapl_id, 
                        uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Tclose_ff(hid_t type_id, hid_t eq_id);

H5_DLL hid_t H5Acreate_ff(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id,
                          hid_t acpl_id, hid_t aapl_id, uint64_t trans, hid_t eq_id);
H5_DLL hid_t H5Acreate_by_name_ff(hid_t loc_id, const char *obj_name, const char *attr_name,
                                  hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id,
                                  hid_t lapl_id, uint64_t trans, hid_t eq_id);
H5_DLL hid_t H5Aopen_ff(hid_t loc_id, const char *attr_name, hid_t aapl_id, 
                        uint64_t trans, hid_t eq_id);
H5_DLL hid_t H5Aopen_by_name_ff(hid_t loc_id, const char *obj_name, const char *attr_name,
                                hid_t aapl_id, hid_t lapl_id, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Awrite_ff(hid_t attr_id, hid_t dtype_id, const void *buf, 
                          uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Aread_ff(hid_t attr_id, hid_t dtype_id, void *buf, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Arename_ff(hid_t loc_id, const char *old_name, const char *new_name, 
                           uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Arename_by_name_ff(hid_t loc_id, const char *obj_name, const char *old_attr_name,
                                   const char *new_attr_name, hid_t lapl_id, 
                                   uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Adelete_ff(hid_t loc_id, const char *name, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Adelete_by_name_ff(hid_t loc_id, const char *obj_name, const char *attr_name,
                                   hid_t lapl_id, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Aexists_by_name_ff(hid_t loc_id, const char *obj_name, const char *attr_name,
                                   hid_t lapl_id, htri_t *ret, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Aexists_ff(hid_t obj_id, const char *attr_name, htri_t *ret, 
                           uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Aclose_ff(hid_t attr_id, hid_t eq_id);

H5_DLL herr_t H5Lmove_ff(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, 
                         const char *dst_name, hid_t lcpl_id, hid_t lapl_id, 
                         uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Lcopy_ff(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
                         const char *dst_name, hid_t lcpl_id, hid_t lapl_id, 
                         uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Lcreate_soft_ff(const char *link_target, hid_t link_loc_id, const char *link_name, 
                                hid_t lcpl_id, hid_t lapl_id, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Lcreate_hard_ff(hid_t cur_loc_id, const char *cur_name, hid_t new_loc_id, 
                                const char *new_name, hid_t lcpl_id, hid_t lapl_id, 
                                uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Ldelete_ff(hid_t loc_id, const char *name, hid_t lapl_id, 
                           uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Lexists_ff(hid_t loc_id, const char *name, hid_t lapl_id, htri_t *ret, 
                           uint64_t trans, hid_t eq_id);

H5_DLL herr_t H5Olink_ff(hid_t obj_id, hid_t new_loc_id, const char *new_name, hid_t lcpl_id,
                         hid_t lapl_id, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Oexists_by_name_ff(hid_t loc_id, const char *name, htri_t *ret, 
                                   hid_t lapl_id, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Oset_comment_ff(hid_t obj_id, const char *comment, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Oset_comment_by_name_ff(hid_t loc_id, const char *name, const char *comment,
                                        hid_t lapl_id, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Oget_comment_ff(hid_t loc_id, char *comment, size_t bufsize, ssize_t *ret,
                                uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Oget_comment_by_name_ff(hid_t loc_id, const char *name, char *comment, size_t bufsize,
                                        ssize_t *ret, hid_t lapl_id, uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Ocopy_ff(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
                         const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id, 
                         uint64_t trans, hid_t eq_id);
H5_DLL herr_t H5Oclose_ff(hid_t object_id, hid_t eq_id);

/* New Routines for Dynamic Data Structures Use Case (ACG) */
herr_t H5DOappend(hid_t dataset_id, hid_t dxpl_id, unsigned axis, size_t extension, 
                  hid_t memtype, const void *buffer);
herr_t H5DOappend_ff(hid_t dataset_id, hid_t dxpl_id, unsigned axis, size_t extension, 
                     hid_t memtype, const void *buffer, uint64_t transaction_number, 
                     H5_request_t *request_ptr);
herr_t H5DOsequence(hid_t dataset_id, hid_t dxpl_id, unsigned axis, hsize_t start, 
                    size_t sequence, hid_t memtype, void *buffer);
herr_t H5DOsequence_ff(hid_t dataset_id, hid_t dxpl_id, unsigned axis, hsize_t start, 
                       size_t sequence, hid_t memtype, void *buffer, 
                       uint64_t transaction_number, H5_request_t *request_ptr);
herr_t H5DOset(hid_t dataset_id, hid_t dxpl_id, const hsize_t coord[],
               hid_t memtype, const void *buffer);
herr_t H5DOset_ff(hid_t dataset_id, hid_t dxpl_id, const hsize_t coord[],hid_t memtype, 
                  const void *buffer, uint64_t transaction_number, H5_request_t *request_ptr);
herr_t H5DOget(hid_t dataset_id, hid_t dxpl_id, const hsize_t coord[],hid_t memtype, void *buffer);
herr_t H5DOget_ff(hid_t dataset_id, hid_t dxpl_id, const hsize_t coord[],hid_t memtype, 
                  void *buffer, uint64_t transaction_number, H5_request_t *request_ptr);

#endif /* H5_HAVE_EFF */

#ifdef __cplusplus
}
#endif

#endif /* _H5FFpublic_H */

