/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  * Copyright by the Board of Trustees of the University of Illinois.         *
  * All rights reserved.                                                      *
  *                                                                           *
  * This file is part of HDF5.  The full HDF5 copyright notice, including     *
  * terms governing use, modification, and redistribution, is contained in    *
  * the files COPYING and Copyright.html.  COPYING can be found at the root   *
  * of the source code distribution tree; Copyright.html can be found at the  *
  * root level of an installed copy of the electronic HDF5 document set and   *
  * is linked from the top-level documents page.  It can also be found at     *
  * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
  * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


#ifndef _H5f90proto_H
#define _H5f90proto_H

#include "H5Git.h"
__DLL__ int HD5c2fstr(char *str, int len);
__DLL__ char * HD5f2cstring (_fcd fdesc, int len);
__DLL__ int HD5packFstring(char *src, char *dest, int len);

/*
 *  Functions from H5Ff.c
 */
#ifndef H5Ff90_FNAMES
#    define H5Ff90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5fcreate_c              FNAME(H5FCREATE_C)
#   define nh5fflush_c               FNAME(H5FFLUSH_C)
#   define nh5fclose_c               FNAME(H5FCLOSE_C)
#   define nh5fopen_c                FNAME(H5FOPEN_C)
#   define nh5fis_hdf5_c             FNAME(H5FIS_HDF5_C)
#   define nh5fmount_c               FNAME(H5FMOUNT_C)
#   define nh5funmount_c             FNAME(H5FUNMOUNT_C)
#   define nh5freopen_c              FNAME(H5FREOPEN_C)
#   define nh5fget_create_plist_c    FNAME(H5FGET_CREATE_PLIST_C)
#   define nh5fget_access_plist_c    FNAME(H5FGET_ACCESS_PLIST_C)
#else                                              /* !DF_CAPFNAMES */
#   define nh5fcreate_c            FNAME(h5fcreate_c)
#   define nh5fflush_c             FNAME(h5fflush_c)
#   define nh5fclose_c             FNAME(h5fclose_c)
#   define nh5fopen_c              FNAME(h5fopen_c)
#   define nh5fis_hdf5_c           FNAME(h5fis_hdf5_c)
#   define nh5fmount_c             FNAME(h5fmount_c)
#   define nh5funmount_c           FNAME(h5funmount_c)
#   define nh5freopen_c            FNAME(h5freopen_c)
#   define nh5fget_create_plist_c  FNAME(h5fget_create_plist_c)
#   define nh5fget_access_plist_c  FNAME(h5fget_access_plist_c)
#endif                                             /* DF_CAPFNAMES */
#endif                                             /* H5Ff90_FNAMES */

__DLL__ int_f nh5fcreate_c 
(_fcd name, int_f *namelen, int_f *access_flags, hid_t_f *crt_prp, hid_t_f *acc_prp, hid_t_f *file_id);

__DLL__ int_f nh5fopen_c 
(_fcd name, int_f *namelen, int_f *access_flags, hid_t_f *acc_prp, hid_t_f *file_id);

__DLL__ int_f nh5fis_hdf5_c 
(_fcd name, int_f *namelen, int_f *flag);

__DLL__ int_f nh5fclose_c (hid_t_f *file_id);
__DLL__ int_f nh5fmount_c 
(hid_t_f *loc_id, _fcd dsetname, int_f *namelen, hid_t_f *file_id, hid_t_f *acc_prp);
__DLL__ int_f nh5funmount_c 
(hid_t_f *loc_id, _fcd dsetname, int_f *namelen);
__DLL__ int_f nh5freopen_c (hid_t_f *file_id1, hid_t_f *file_id2);
__DLL__ int_f nh5fget_create_plist_c (hid_t_f *file_id, hid_t_f *prop_id);
__DLL__ int_f nh5fget_access_plist_c (hid_t_f *file_id, hid_t_f *access_id);
/*
 * Functions from H5Sf.c
 */
#ifndef H5Sf90_FNAMES
#    define H5Sf90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5screate_simple_c      FNAME(H5SCREATE_SIMPLE_C)
#   define nh5sclose_c              FNAME(H5SCLOSE_C)
#   define nh5screate_c             FNAME(H5SCREATE_C)
#   define nh5scopy_c               FNAME(H5SCOPY_C)
#   define nh5sget_select_hyper_nblocks_c FNAME(H5SGET_SELECT_HYPER_NBLOCKS_C)
#   define nh5sget_select_hyper_blocklist_c FNAME(H5SGET_SELECT_HYPER_BLOCKLIST_C)
#   define nh5sget_select_elem_npoints_c FNAME(H5SGET_SELECT_ELEM_NPOINTS_C)
#   define nh5sget_select_elem_pointlist_c FNAME(H5SGET_SELECT_ELEM_POINTLIST_C)
#   define nh5sget_select_bounds_c  FNAME(H5SGET_SELECT_BOUNDS_C)
#   define nh5sselect_all_c         FNAME(H5SSELECT_ALL_C)
#   define nh5sselect_none_c        FNAME(H5SSELECT_NONE_C)
#   define nh5sselect_valid_c       FNAME(H5SSELECT_VALID_C)
#   define nh5sget_simple_extent_npoints_c FNAME(H5SGET_SIMPLE_EXTENT_NPOINTS_C)
#   define nh5sget_select_npoints_c FNAME(H5SGET_SELECT_NPOINTS_C)
#   define nh5sget_simple_extent_ndims_c FNAME(H5SGET_SIMPLE_EXTENT_NDIMS_C)
#   define nh5sget_simple_extent_type_c  FNAME(H5SGET_SIMPLE_EXTENT_TYPE_C)
#   define nh5soffset_simple_c      FNAME(H5SOFFSET_SIMPLE_C)
#   define nh5sset_extent_simple_c  FNAME(H5SSET_EXTENT_SIMPLE_C)
#   define nh5sis_simple_c          FNAME(H5SIS_SIMPLE_C)
#   define nh5sextent_class_c       FNAME(H5SEXTENT_CLASS_C)
#   define nh5sget_simple_extent_dims_c FNAME(H5SGET_SIMPLE_EXTENT_DIMS_C)
#   define nh5sextent_copy_c        FNAME(H5SEXTENT_COPY_C)
#   define nh5sset_extent_none_c    FNAME(H5SSET_EXTENT_NONE_C)
#   define nh5sselect_hyperslab_c   FNAME(H5SSELECT_HYPERSLAB_C)
#   define nh5sselect_elements_c    FNAME(H5SSELECT_ELEMENTS_C)
#else                                              /* !DF_CAPFNAMES */
#   define nh5screate_simple_c      FNAME(h5screate_simple_c)
#   define nh5sclose_c              FNAME(h5sclose_c)
#   define nh5screate_c             FNAME(h5screate_c)
#   define nh5scopy_c               FNAME(h5scopy_c)
#   define nh5sget_select_hyper_nblocks_c FNAME(h5sget_select_hyper_nblocks_c)
#   define nh5sget_select_hyper_blocklist_c FNAME(h5sget_select_hyper_blocklist_c)
#   define nh5sget_select_elem_npoints_c FNAME(h5sget_select_elem_npoints_c)
#   define nh5sget_select_bounds_c  FNAME(h5sget_select_bounds_c)
#   define nh5sget_select_elem_pointlist_c FNAME(h5sget_select_elem_pointlist_c)
#   define nh5sselect_all_c         FNAME(h5sselect_all_c)
#   define nh5sselect_none_c        FNAME(h5sselect_none_c)
#   define nh5sselect_valid_c       FNAME(h5sselect_valid_c)
#   define nh5sget_simple_extent_npoints_c FNAME(h5sget_simple_extent_npoints_c)
#   define nh5sget_select_npoints_c FNAME(h5sget_select_npoints_c)
#   define nh5sget_simple_extent_ndims_c FNAME(h5sget_simple_extent_ndims_c)
#   define nh5sget_simple_extent_type_c  FNAME(h5sget_simple_extent_type_c)
#   define nh5soffset_simple_c      FNAME(h5soffset_simple_c)
#   define nh5sset_extent_simple_c  FNAME(h5sset_extent_simple_c)
#   define nh5sis_simple_c          FNAME(h5sis_simple_c)
#   define nh5sextent_class_c       FNAME(h5sextent_class_c)
#   define nh5sget_simple_extent_dims_c FNAME(h5sget_simple_extent_dims_c)
#   define nh5sextent_copy_c        FNAME(h5sextent_copy_c)
#   define nh5sset_extent_none_c    FNAME(h5sset_extent_none_c)
#   define nh5sselect_hyperslab_c   FNAME(h5sselect_hyperslab_c)
#   define nh5sselect_elements_c    FNAME(h5sselect_elements_c)
#endif                                             /* DF_CAPFNAMES */
#endif

__DLL__ int_f nh5screate_simple_c 
( int_f *rank, hsize_t_f *dims, hsize_t_f *maxdims, hid_t_f *space_id );

__DLL__ int_f nh5sclose_c ( hid_t_f *space_id );

__DLL__ int_f nh5screate_c ( int_f *classtype, hid_t_f *space_id );

__DLL__ int_f nh5scopy_c ( hid_t_f *space_id , hid_t_f *new_space_id);
__DLL__ int_f nh5sget_select_hyper_nblocks_c( hid_t_f *space_id , hssize_t_f * num_blocks);
__DLL__ int_f nh5sget_select_hyper_blocklist_c( hid_t_f *space_id ,hsize_t_f * startblock, hsize_t_f * num_blocks, hsize_t_f * buf);

__DLL__ int_f nh5sget_select_bounds_c( hid_t_f *space_id , hsize_t_f * start, hsize_t_f * end);

__DLL__ int_f nh5sget_select_elem_npoints_c( hid_t_f *space_id , hssize_t_f * num_points);

__DLL__ int_f nh5sget_select_elem_pointlist_c( hid_t_f *space_id ,hsize_t_f * startpoint, hsize_t_f * numpoints, hsize_t_f * buf);
__DLL__ int_f nh5sselect_all_c ( hid_t_f *space_id );

__DLL__ int_f nh5sselect_none_c ( hid_t_f *space_id );

__DLL__ int_f nh5sselect_valid_c ( hid_t_f *space_id , int_f *flag );

__DLL__ int_f nh5sget_simple_extent_npoints_c ( hid_t_f *space_id , hsize_t_f *npoints );

__DLL__ int_f nh5sget_select_npoints_c ( hid_t_f *space_id , hssize_t_f *npoints );

__DLL__ int_f nh5sget_simple_extent_ndims_c ( hid_t_f *space_id , int_f *ndims );

__DLL__ int_f nh5sget_simple_extent_type_c ( hid_t_f *space_id , int_f *classtype);

__DLL__ int_f nh5soffset_simple_c ( hid_t_f *space_id , hssize_t_f *offset);

__DLL__ int_f nh5sset_extent_simple_c ( hid_t_f *space_id , int_f *rank, hsize_t_f * current_size, hsize_t_f *maximum_size);

__DLL__ int_f nh5sis_simple_c ( hid_t_f *space_id , int_f *flag );

__DLL__ int_f nh5sextent_class_c ( hid_t_f *space_id , int_f *classtype);

__DLL__ int_f nh5sget_simple_extent_dims_c ( hid_t_f *space_id , hsize_t_f *dims, hsize_t_f *maxdims);

__DLL__ int_f nh5sextent_copy_c ( hid_t_f *dest_space_id , hid_t_f *source_space_id);

__DLL__ int_f nh5sset_extent_none_c ( hid_t_f *space_id );

__DLL__ int_f nh5sselect_hyperslab_c ( hid_t_f *space_id , int_f *op, hssize_t_f *start, hsize_t_f *count, hsize_t_f *stride, hsize_t_f *block);

__DLL__ int_f nh5sselect_elements_c ( hid_t_f *space_id , int_f *op, size_t_f *nelements, hssize_t_f *coord);


/*
 * Functions from H5Df.c
 */

#ifndef H5Df90_FNAMES
#    define H5Df90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5dcreate_c      FNAME(H5DCREATE_C)
#   define nh5dclose_c       FNAME(H5DCLOSE_C)
#   define nh5dopen_c        FNAME(H5DOPEN_C)
#   define nh5dwrite_c       FNAME(H5DWRITE_C)
#   define nh5dwrite_ref_obj_c       FNAME(H5DWRITE_REF_OBJ_C)
#   define nh5dwrite_ref_reg_c       FNAME(H5DWRITE_REF_REG_C)
#   define nh5dwritec_c      FNAME(H5DWRITEC_C)
#   define nh5dread_c        FNAME(H5DREAD_C)
#   define nh5dread_ref_reg_c        FNAME(H5DREAD_REF_REG_C)
#   define nh5dread_ref_obj_c        FNAME(H5DREAD_REF_OBJ_C)
#   define nh5dreadc_c       FNAME(H5DREADC_C)
#   define nh5dget_space_c   FNAME(H5DGET_SPACE_C)
#   define nh5dget_type_c    FNAME(H5DGET_TYPE_C)
#   define nh5dget_create_plist_c  FNAME(H5DGET_CREATE_PLIST_C)
#   define nh5dextend_c      FNAME(H5DEXTEND_C)
#else                                              /* !DF_CAPFNAMES */
#   define nh5dcreate_c      FNAME(h5dcreate_c)
#   define nh5dclose_c       FNAME(h5dclose_c)
#   define nh5dopen_c        FNAME(h5dopen_c)
#   define nh5dwrite_c       FNAME(h5dwrite_c)
#   define nh5dwritec_c      FNAME(h5dwritec_c)
#   define nh5dwrite_ref_obj_c       FNAME(h5dwrite_ref_obj_c)
#   define nh5dwrite_ref_reg_c       FNAME(h5dwrite_ref_reg_c)
#   define nh5dread_c        FNAME(h5dread_c)
#   define nh5dread_ref_reg_c        FNAME(h5dread_ref_reg_c)
#   define nh5dread_ref_obj_c        FNAME(h5dread_ref_obj_c)
#   define nh5dreadc_c       FNAME(h5dreadc_c)
#   define nh5dget_space_c   FNAME(h5dget_space_c)
#   define nh5dget_type_c    FNAME(h5dget_type_c)
#   define nh5dget_create_plist_c  FNAME(h5dget_create_plist_c)
#   define nh5dextend_c      FNAME(h5dextend_c)
#endif                                             /* DF_CAPFNAMES */
#endif

__DLL__ int_f nh5dcreate_c
(hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *space_id, hid_t_f *crt_prp,  hid_t_f *dset_id);

__DLL__ int_f nh5dopen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *dset_id);

__DLL__ int_f nh5dclose_c ( hid_t_f *dset_id );

__DLL__ int_f nh5dwrite_c
(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, int_f *dims);

__DLL__ int_f nh5dwrite_ref_obj_c
(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, int_f *dims);

__DLL__ int_f nh5dwrite_ref_reg_c
(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, int_f *dims);

__DLL__ int_f nh5dwritec_c
(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, int_f *dims);

__DLL__ int_f nh5dread_c
(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, int_f *dims);


__DLL__ int_f nh5dread_ref_obj_c
(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, int_f *dims);

__DLL__ int_f nh5dread_ref_reg_c
(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, int_f *dims);

__DLL__ int_f nh5dreadc_c
(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, int_f *dims);

__DLL__ int_f nh5dget_space_c ( hid_t_f *dset_id , hid_t_f *space_id);

__DLL__ int_f nh5dget_type_c ( hid_t_f *dset_id , hid_t_f *type_id);

__DLL__ int_f nh5dget_create_plist_c ( hid_t_f *dset_id , hid_t_f *plist_id);

__DLL__ int_f nh5dextend_c ( hid_t_f *dset_id , hsize_t_f *dims);
/*
 * Functions from H5Gf.c
 */

#ifndef H5Gf90_FNAMES
#    define H5Gf90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5gcreate_c      FNAME(H5GCREATE_C)
#   define nh5gclose_c       FNAME(H5GCLOSE_C)
#   define nh5gopen_c        FNAME(H5GOPEN_C)
#   define nh5gget_obj_info_idx_c FNAME(H5GGET_OBJ_INFO_IDX_C)
#   define nh5gn_members_c   FNAME(H5GN_MEMBERS_C)
#   define nh5glink_c        FNAME(H5GLINK_C)
#   define nh5gunlink_c      FNAME(H5GUNLINK_C)
#   define nh5gmove_c        FNAME(H5GMOVE_C)
#   define nh5gget_linkval_c   FNAME(H5GGET_LINKVAL_C)
#   define nh5gset_comment_c   FNAME(H5GSET_COMMENT_C)
#   define nh5gget_comment_c   FNAME(H5GGET_COMMENT_C)
#else                                              /* !DF_CAPFNAMES */
#   define nh5gcreate_c      FNAME(h5gcreate_c)
#   define nh5gclose_c       FNAME(h5gclose_c)
#   define nh5gopen_c        FNAME(h5gopen_c)
#   define nh5gget_obj_info_idx_c FNAME(h5gget_obj_info_idx_c)
#   define nh5gn_members_c   FNAME(h5gn_members_c)
#   define nh5glink_c        FNAME(h5glink_c)
#   define nh5gunlink_c      FNAME(h5gunlink_c)
#   define nh5gmove_c        FNAME(h5gmove_c)
#   define nh5gget_linkval_c   FNAME(h5gget_linkval_c)
#   define nh5gset_comment_c   FNAME(h5gset_comment_c)
#   define nh5gget_comment_c   FNAME(h5gget_comment_c)
#endif                                             /* DF_CAPFNAMES */
#endif

__DLL__ int_f nh5gcreate_c 
(hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *size_hint,  hid_t_f *grp_id);

__DLL__ int_f nh5gopen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *grp_id);

__DLL__ int_f nh5gclose_c ( hid_t_f *grp_id );

__DLL__ int_f nh5gget_obj_info_idx_c
(hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *idx, _fcd obj_name, int_f *obj_namelen, int_f *obj_type);

__DLL__ int_f nh5gn_members_c 
(hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *nmembers);

__DLL__ int_f nh5glink_c
(hid_t_f *loc_id, int_f *link_type, _fcd current_name, int_f *current_namelen, _fcd new_name, int_f *new_namelen);

__DLL__ int_f nh5gunlink_c
(hid_t_f *loc_id, _fcd name, int_f *namelen);

__DLL__ int_f nh5gmove_c
(hid_t_f *loc_id, _fcd src_name, int_f *src_namelen, _fcd dst_name, int_f *dst_namelen);

__DLL__ int_f nh5gget_linkval_c
(hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *size, _fcd value );

__DLL__ int_f nh5gset_comment_c
(hid_t_f *loc_id, _fcd name, int_f *namelen, _fcd comment, int_f *commentlen);

__DLL__ int_f nh5gget_comment_c
(hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *bufsize, _fcd comment);


/*
 * Functions from H5Af.c
 */

#ifndef H5Af90_FNAMES
#    define H5Af90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5acreate_c      FNAME(H5ACREATE_C)
#   define nh5aclose_c       FNAME(H5ACLOSE_C)
#   define nh5aopen_name_c   FNAME(H5AOPEN_NAME_C)
#   define nh5awrite_c       FNAME(H5AWRITE_C)
#   define nh5awritec_c      FNAME(H5AWRITEC_C)
#   define nh5aread_c        FNAME(H5AREAD_C)
#   define nh5areadc_c       FNAME(H5AREADC_C)
#   define nh5aget_name_c    FNAME(H5AGET_NAME_C)
#   define nh5aopen_idx_c    FNAME(H5AOPEN_IDX_C)
#   define nh5aget_space_c   FNAME(H5AGET_SPACE_C)
#   define nh5aget_type_c    FNAME(H5AGET_TYPE_C)
#   define nh5aget_num_attrs_c FNAME(H5AGET_NUM_ATTRS_C)
#   define nh5adelete_c      FNAME(H5ADELETE_C)
#else                                              /* !DF_CAPFNAMES */
#   define nh5acreate_c      FNAME(h5acreate_c)
#   define nh5aclose_c       FNAME(h5aclose_c)
#   define nh5aopen_name_c   FNAME(h5aopen_name_c)
#   define nh5awrite_c       FNAME(h5awrite_c)
#   define nh5awritec_c      FNAME(h5awritec_c)
#   define nh5aread_c        FNAME(h5aread_c)
#   define nh5areadc_c       FNAME(h5areadc_c)
#   define nh5aget_name_c    FNAME(h5aget_name_c)
#   define nh5aopen_idx_c    FNAME(h5aopen_idx_c)
#   define nh5aget_space_c   FNAME(h5aget_space_c)
#   define nh5aget_type_c    FNAME(h5aget_type_c)
#   define nh5aget_num_attrs_c FNAME(h5aget_num_attrs_c)
#   define nh5adelete_c      FNAME(h5adelete_c)
#endif                                             /* DF_CAPFNAMES */
#endif


__DLL__ int_f nh5acreate_c (hid_t_f *obj_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *space_id, hid_t_f *crt_prp,  hid_t_f *attr_id);

__DLL__ int_f 
nh5aopen_name_c (hid_t_f *obj_id, _fcd name, int_f *namelen, hid_t_f *attr_id);

__DLL__ int_f nh5awritec_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, int_f *dims);

__DLL__ int_f nh5awrite_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, int_f *dims);

__DLL__ int_f nh5areadc_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, int_f *dims);

__DLL__ int_f nh5aread_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, int_f *dims);

__DLL__ int_f nh5aclose_c ( hid_t_f *attr_id );

__DLL__ int_f nh5adelete_c (hid_t_f *obj_id, _fcd name, int_f *namelen); 

__DLL__ int_f nh5aopen_idx_c (hid_t_f *obj_id, int_f *idx, hid_t_f *attr_id);

__DLL__ int_f nh5aget_space_c (hid_t_f *attr_id, hid_t_f *space_id);

__DLL__ int_f nh5aget_type_c (hid_t_f *attr_id, hid_t_f *type_id);

__DLL__ int_f nh5aget_num_attrs_c (hid_t_f *obj_id, int_f *attr_num);

__DLL__ int_f nh5aget_name_c(hid_t_f *attr_id, size_t_f *size, _fcd buf);

/* 
 * Functions form H5Tf.c file
 */
#ifndef H5Tf90_FNAMES
#    define H5Tf90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5topen_c         FNAME(H5TOPEN_C)
#   define nh5tcommit_c       FNAME(H5TCOMMIT_C)
#   define nh5tcommitted_c    FNAME(H5TCOMMITTED_C)
#   define nh5tclose_c        FNAME(H5TCLOSE_C)
#   define nh5tcopy_c         FNAME(H5TCOPY_C)
#   define nh5tequal_c        FNAME(H5TEQUAL_C)
#   define nh5tget_class_c    FNAME(H5TGET_CLASS_C)
#   define nh5tget_order_c    FNAME(H5TGET_ORDER_C)
#   define nh5tset_order_c    FNAME(H5TSET_ORDER_C)
#   define nh5tget_size_c     FNAME(H5TGET_SIZE_C)
#   define nh5tset_size_c     FNAME(H5TSET_SIZE_C)
#   define nh5tget_precision_c     FNAME(H5TGET_PRECISION_C)
#   define nh5tset_precision_c     FNAME(H5TSET_PRECISION_C)
#   define nh5tget_offset_c        FNAME(H5TGET_OFFSET_C)
#   define nh5tset_offset_c        FNAME(H5TSET_OFFSET_C)
#   define nh5tget_pad_c        FNAME(H5TGET_PAD_C)
#   define nh5tset_pad_c        FNAME(H5TSET_PAD_C)
#   define nh5tget_sign_c       FNAME(H5TGET_SIGN_C)
#   define nh5tset_sign_c       FNAME(H5TSET_SIGN_C)
#   define nh5tget_fields_c       FNAME(H5TGET_FIELDS_C)
#   define nh5tset_fields_c       FNAME(H5TSET_FIELDS_C)
#   define nh5tget_ebias_c     FNAME(H5TGET_EBIAS_C)
#   define nh5tset_ebias_c     FNAME(H5TSET_EBIAS_C)
#   define nh5tget_norm_c     FNAME(H5TGET_NORM_C)
#   define nh5tset_norm_c     FNAME(H5TSET_NORM_C)
#   define nh5tget_inpad_c        FNAME(H5TGET_INPAD_C)
#   define nh5tset_inpad_c        FNAME(H5TSET_INPAD_C)
#   define nh5tget_cset_c        FNAME(H5TGET_CSET_C)
#   define nh5tset_cset_c        FNAME(H5TSET_CSET_C)
#   define nh5tget_strpad_c        FNAME(H5TGET_STRPAD_C)
#   define nh5tset_strpad_c        FNAME(H5TSET_STRPAD_C)
#   define nh5tget_nmembers_c        FNAME(H5TGET_NMEMBERS_C)
#   define nh5tget_member_name_c        FNAME(H5TGET_MEMBER_NAME_C)
#   define nh5tget_member_offset_c        FNAME(H5TGET_MEMBER_OFFSET_C)
#   define nh5tget_member_dims_c        FNAME(H5TGET_MEMBER_DIMS_C)
#   define nh5tget_member_type_c        FNAME(H5TGET_MEMBER_TYPE_C)
#   define nh5tinsert_c        FNAME(H5TINSERT_C)
#   define nh5tcreate_c        FNAME(H5TCREATE_C)
#   define nh5tpack_c                   FNAME(H5TPACK_C)
#   define nh5tinsert_array_c           FNAME(H5TINSERT_ARRAY_C)
#   define nh5tinsert_array_c2           FNAME(H5TINSERT_ARRAY_C2)
#   define nh5tenum_create_c             FNAME(H5TENUM_CREATE_C)
#   define nh5tenum_insert_c             FNAME(H5TENUM_INSERT_C)
#   define nh5tenum_nameof_c             FNAME(H5TENUM_NAMEOF_C)
#   define nh5tenum_valueof_c             FNAME(H5TENUM_VALUEOF_C)
#   define nh5tget_member_value_c         FNAME(H5TGET_MEMBER_VALUE_C)
#   define nh5tset_tag_c                  FNAME(H5TSET_TAG_C)
#   define nh5tget_tag_c                  FNAME(H5TGET_TAG_C)
#   define nh5tarray_create_c             FNAME(H5TARRAY_CREATE_C)
#   define nh5tget_array_ndims_c          FNAME(H5TGET_ARRAY_NDIMS_C)
#   define nh5tget_array_dims_c          FNAME(H5TGET_ARRAY_DIMS_C)
#   define nh5tget_super_c               FNAME(H5TGET_SUPER_C)

#else
#   define nh5topen_c         FNAME(h5topen_c)
#   define nh5tcommit_c       FNAME(h5tcommit_c)
#   define nh5tcommitted_c    FNAME(h5tcommitted_c)
#   define nh5tclose_c        FNAME(h5tclose_c)
#   define nh5tcopy_c         FNAME(h5tcopy_c)
#   define nh5tequal_c        FNAME(h5tequal_c)
#   define nh5tget_class_c    FNAME(h5tget_class_c)
#   define nh5tget_order_c    FNAME(h5tget_order_c)
#   define nh5tset_order_c    FNAME(h5tset_order_c)
#   define nh5tget_size_c     FNAME(h5tget_size_c)
#   define nh5tset_size_c     FNAME(h5tset_size_c)
#   define nh5tget_precision_c     FNAME(h5tget_precision_c)
#   define nh5tset_precision_c     FNAME(h5tset_precision_c)
#   define nh5tget_offset_c        FNAME(h5tget_offset_c)
#   define nh5tset_offset_c        FNAME(h5tset_offset_c)
#   define nh5tget_pad_c        FNAME(h5tget_pad_c)
#   define nh5tset_pad_c        FNAME(h5tset_pad_c)
#   define nh5tget_sign_c       FNAME(h5tget_sign_c)
#   define nh5tset_sign_c       FNAME(h5tset_sign_c)
#   define nh5tget_fields_c       FNAME(h5tget_fields_c)
#   define nh5tset_fields_c       FNAME(h5tset_fields_c)
#   define nh5tget_ebias_c     FNAME(h5tget_ebias_c)
#   define nh5tset_ebias_c     FNAME(h5tset_ebias_c)
#   define nh5tget_norm_c     FNAME(h5tget_norm_c)
#   define nh5tset_norm_c     FNAME(h5tset_norm_c)
#   define nh5tget_inpad_c        FNAME(h5tget_inpad_c)
#   define nh5tset_inpad_c        FNAME(h5tset_inpad_c)
#   define nh5tget_cset_c        FNAME(h5tget_cset_c)
#   define nh5tset_cset_c        FNAME(h5tset_cset_c)
#   define nh5tget_strpad_c        FNAME(h5tget_strpad_c)
#   define nh5tset_strpad_c        FNAME(h5tset_strpad_c)
#   define nh5tget_nmembers_c        FNAME(h5tget_nmembers_c)
#   define nh5tget_member_name_c        FNAME(h5tget_member_name_c)
#   define nh5tget_member_offset_c        FNAME(h5tget_member_offset_c)
#   define nh5tget_member_dims_c        FNAME(h5tget_member_dims_c)
#   define nh5tget_member_type_c        FNAME(h5tget_member_type_c)
#   define nh5tinsert_c                 FNAME(h5tinsert_c)
#   define nh5tcreate_c        FNAME(h5tcreate_c)
#   define nh5tpack_c                   FNAME(h5tpack_c)
#   define nh5tinsert_array_c           FNAME(h5tinsert_array_c)
#   define nh5tinsert_array_c2           FNAME(h5tinsert_array_c2)
#   define nh5tenum_create_c             FNAME(h5tenum_create_c)
#   define nh5tenum_insert_c             FNAME(h5tenum_insert_c)
#   define nh5tenum_nameof_c             FNAME(h5tenum_nameof_c)
#   define nh5tenum_valueof_c             FNAME(h5tenum_valueof_c)
#   define nh5tget_member_value_c             FNAME(h5tget_member_value_c)
#   define nh5tset_tag_c                  FNAME(h5tset_tag_c)
#   define nh5tget_tag_c                  FNAME(h5tget_tag_c)
#   define nh5tarray_create_c             FNAME(h5tarray_create_c)
#   define nh5tget_array_ndims_c          FNAME(h5tget_array_ndims_c)
#   define nh5tget_array_dims_c          FNAME(h5tget_array_dims_c)
#   define nh5tget_super_c               FNAME(h5tget_super_c)
#endif
#endif

__DLL__ int_f nh5tcreate_c(int_f *class, size_t_f *size, hid_t_f *type_id);

__DLL__ int_f nh5topen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id);

__DLL__ int_f 
nh5tcommit_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id);

__DLL__ int_f nh5tclose_c ( hid_t_f *type_id );

__DLL__ int_f nh5tequal_c ( hid_t_f *type1_id , hid_t_f *type2_id, int_f *c_flag);

__DLL__ int_f nh5tcopy_c ( hid_t_f *type_id , hid_t_f *new_type_id);

__DLL__ int_f nh5tget_class_c ( hid_t_f *type_id , int_f *classtype);

__DLL__ int_f nh5tget_order_c ( hid_t_f *type_id , int_f *order);

__DLL__ int_f nh5tset_order_c ( hid_t_f *type_id , int_f *order);

__DLL__ int_f nh5tget_size_c ( hid_t_f *type_id , size_t_f *size);

__DLL__ int_f nh5tset_size_c ( hid_t_f *type_id , size_t_f *size);
__DLL__ int_f nh5tcommitted_c (hid_t_f *type_id);
__DLL__ int_f nh5tget_precision_c ( hid_t_f *type_id , size_t_f *precision);
__DLL__ int_f nh5tset_precision_c ( hid_t_f *type_id , size_t_f *precision);
__DLL__ int_f nh5tget_offset_c ( hid_t_f *type_id , size_t_f *offset);
__DLL__ int_f nh5tset_offset_c ( hid_t_f *type_id , size_t_f *offset);
__DLL__ int_f nh5tget_pad_c ( hid_t_f *type_id , int_f * lsbpad, int_f * msbpad);
__DLL__ int_f nh5tset_pad_c ( hid_t_f *type_id, int_f * lsbpad, int_f * msbpad );
__DLL__ int_f nh5tget_sign_c ( hid_t_f *type_id , int_f* sign);
__DLL__ int_f nh5tset_sign_c ( hid_t_f *type_id , int_f *sign);
__DLL__ int_f nh5tget_fields_c ( hid_t_f *type_id, size_t_f *spos, size_t_f *epos, size_t_f* esize, size_t_f* mpos, size_t_f* msize);
__DLL__ int_f nh5tset_fields_c ( hid_t_f *type_id, size_t_f *spos, size_t_f *epos, size_t_f* esize, size_t_f* mpos, size_t_f* msize);
__DLL__ int_f nh5tget_ebias_c ( hid_t_f *type_id , size_t_f *ebias);

__DLL__ int_f nh5tset_ebias_c ( hid_t_f *type_id , size_t_f *ebias);
__DLL__ int_f nh5tget_norm_c ( hid_t_f *type_id , int_f *norm);

__DLL__ int_f nh5tset_norm_c ( hid_t_f *type_id , int_f *norm);
__DLL__ int_f nh5tget_inpad_c ( hid_t_f *type_id, int_f * padtype);
__DLL__ int_f nh5tset_inpad_c ( hid_t_f *type_id, int_f * padtype);
__DLL__ int_f nh5tget_cset_c ( hid_t_f *type_id, int_f * cset);
__DLL__ int_f nh5tset_cset_c ( hid_t_f *type_id, int_f * cset);
__DLL__ int_f nh5tget_strpad_c ( hid_t_f *type_id, int_f * strpad);
__DLL__ int_f nh5tset_strpad_c ( hid_t_f *type_id, int_f * strpad);
__DLL__ int_f nh5tget_nmembers_c ( hid_t_f *type_id , int_f * num_members);
__DLL__ int_f nh5tget_member_name_c ( hid_t_f *type_id ,int_f* index, _fcd member_name, int_f *namelen);
__DLL__ int_f nh5tget_member_dims_c ( hid_t_f *type_id ,int_f* field_idx, int_f * dims, size_t_f * field_dims, int_f * perm );
__DLL__ int_f nh5tget_member_offset_c ( hid_t_f *type_id ,int_f* member_no, size_t_f* offset);
__DLL__ int_f nh5tget_member_type_c ( hid_t_f *type_id ,int_f* field_idx, hid_t_f * datatype);
__DLL__ int_f nh5tinsert_c(hid_t_f *type_id, _fcd name, int_f* namelen, size_t_f *offset, hid_t_f * field_id);
__DLL__ int_f nh5tpack_c(hid_t_f * type_id);

__DLL__ int_f nh5tinsert_array_c(hid_t_f * parent_id, _fcd name, int_f* namelen, size_t_f* offset, int_f* ndims, size_t_f* dims, hid_t_f* member_id, int_f* perm );

__DLL__ int_f nh5tinsert_array_c2(hid_t_f * parent_id, _fcd name, int_f* namelen, size_t_f* offset, int_f* ndims, size_t_f* dims, hid_t_f* member_id);

__DLL__ int_f nh5tenum_create_c ( hid_t_f *parent_id , hid_t_f *new_type_id);

__DLL__ int_f nh5tenum_insert_c(hid_t_f *type_id, _fcd name, int_f* namelen, int_f* value);
__DLL__ int_f
nh5tenum_nameof_c(hid_t_f *type_id, int_f* value, _fcd name, size_t_f* namelen);
__DLL__ int_f
nh5tenum_valueof_c(hid_t_f *type_id, _fcd name, int_f* namelen, int_f* value);
__DLL__ int_f
nh5tget_member_value_c(hid_t_f *type_id, int_f* member_no, int_f* value);
__DLL__ int_f
nh5tset_tag_c(hid_t_f* type_id, _fcd tag, int_f* namelen);
__DLL__ int_f
nh5tget_tag_c(hid_t_f* type_id, _fcd tag, int_f* namelen);
__DLL__ int_f
nh5tarray_create_c(hid_t_f * base_id, int_f *rank, hsize_t_f* dims, hid_t_f* type_id);
__DLL__ int_f
nh5tget_array_dims_c ( hid_t_f *type_id , hsize_t_f * dims);
__DLL__ int_f
nh5tget_array_ndims_c ( hid_t_f *type_id , int_f * ndims);
__DLL__ int_f
nh5tget_super_c ( hid_t_f *type_id , hid_t_f *base_type_id);


/* 
 * Functions from H5Pf.c
 */

#ifndef H5Pf90_FNAMES
#    define H5Pf90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5pcreate_c       FNAME(H5PCREATE_C)
#   define nh5pclose_c        FNAME(H5PCLOSE_C)
#   define nh5pcopy_c         FNAME(H5PCOPY_C)
#   define nh5pget_class_c    FNAME(H5PGET_CLASS_C)
#   define nh5pset_deflate_c  FNAME(H5PSET_DEFLATE_C)
#   define nh5pset_preserve_c  FNAME(H5PSET_PRESERVE_C)
#   define nh5pget_preserve_c  FNAME(H5PGET_PRESERVE_C)
#   define nh5pset_chunk_c    FNAME(H5PSET_CHUNK_C)
#   define nh5pget_chunk_c    FNAME(H5PGET_CHUNK_C)
#   define nh5pset_fill_valuec_c FNAME(H5PSET_FILL_VALUEC_C)
#   define nh5pset_fill_value_c FNAME(H5PSET_FILL_VALUE_C)
#   define nh5pget_fill_valuec_c FNAME(H5PGET_FILL_VALUEC_C)
#   define nh5pget_fill_value_c FNAME(H5PGET_FILL_VALUE_C)
#   define nh5pget_version_c    FNAME(H5PGET_VERSION_C)
#   define nh5pget_userblock_c    FNAME(H5PGET_USERBLOCK_C)
#   define nh5pset_userblock_c    FNAME(H5PSET_USERBLOCK_C)
#   define nh5pset_sizes_c        FNAME(H5PSET_SIZES_C)
#   define nh5pget_sizes_c         FNAME(H5PGET_SIZES_C)
#   define nh5pget_sym_k_c         FNAME(H5PGET_SYM_K_C)
#   define nh5pset_sym_k_c         FNAME(H5PSET_SYM_K_C)
#   define nh5pget_istore_k_c         FNAME(H5PGET_ISTORE_K_C)
#   define nh5pset_istore_k_c         FNAME(H5PSET_ISTORE_K_C)
#   define nh5pget_driver_c         FNAME(H5PGET_DRIVER_C)
#   define nh5pset_fapl_stdio_c         FNAME(H5PSET_FAPL_STDIO_C)
#   define nh5pget_fapl_stdio_c         FNAME(H5PGET_FAPL_STDIO_C)
#   define nh5pset_fapl_sec2_c         FNAME(H5PSET_FAPL_SEC2_C)
#   define nh5pget_fapl_sec2_c         FNAME(H5PGET_FAPL_SEC2_C)
#   define nh5pset_alignment_c         FNAME(H5PSET_ALIGNMENT_C)
#   define nh5pget_alignment_c         FNAME(H5PGET_ALIGNMENT_C)
#   define nh5pset_fapl_core_c         FNAME(H5PSET_FAPL_CORE_C)
#   define nh5pget_fapl_core_c         FNAME(H5PGET_FAPL_CORE_C)
#   define nh5pset_fapl_family_c         FNAME(H5PSET_FAPL_FAMILY_C)
#   define nh5pget_fapl_family_c         FNAME(H5PGET_FAPL_FAMILY_C)
#   define nh5pset_cache_c         FNAME(H5PSET_CACHE_C)
#   define nh5pget_cache_c         FNAME(H5PGET_CACHE_C)
#   define nh5pset_fapl_split_c         FNAME(H5PSET_FAPL_SPLIT_C)
#   define nh5pget_fapl_split_c         FNAME(H5PGET_FAPL_SPLIT_C)
#   define nh5pset_gc_references_c         FNAME(H5PSET_GC_REFERENCES_C)
#   define nh5pget_gc_references_c         FNAME(H5PGET_GC_REFERENCES_C)
#   define nh5pset_layout_c         FNAME(H5PSET_LAYOUT_C)
#   define nh5pget_layout_c         FNAME(H5PGET_LAYOUT_C)
#   define nh5pset_filter_c         FNAME(H5PSET_FILTER_C)
#   define nh5pget_nfilters_c         FNAME(H5PGET_NFILTERS_C)
#   define nh5pget_filter_c         FNAME(H5PGET_FILTER_C)
#   define nh5pset_external_c         FNAME(H5PSET_EXTERNAL_C)
#   define nh5pget_external_count_c         FNAME(H5PGET_EXTERNAL_COUNT_C)
#   define nh5pget_external_c         FNAME(H5PGET_EXTERNAL_C)
#   define nh5pset_hyper_cache_c         FNAME(H5PSET_HYPER_CACHE_C)
#   define nh5pget_hyper_cache_c         FNAME(H5PGET_HYPER_CACHE_C)
#   define nh5pget_btree_ratios_c         FNAME(H5PGET_BTREE_RATIOS_C)
#   define nh5pset_btree_ratios_c         FNAME(H5PSET_BTREE_RATIOS_C)
#   define nh5pset_fapl_mpio_c         FNAME(H5PSET_FAPL_MPIO_C)
#   define nh5pget_fapl_mpio_c         FNAME(H5PGET_FAPL_MPIO_C)
#   define nh5pset_dxpl_mpio_c        FNAME(H5PSET_DXPL_MPIO_C)
#   define nh5pget_dxpl_mpio_c        FNAME(H5PGET_DXPL_MPIO_C)

#else
#   define nh5pcreate_c       FNAME(h5pcreate_c)
#   define nh5pclose_c        FNAME(h5pclose_c)
#   define nh5pcopy_c         FNAME(h5pcopy_c)
#   define nh5pget_class_c    FNAME(h5pget_class_c)
#   define nh5pset_deflate_c  FNAME(h5pset_deflate_c)
#   define nh5pset_preserve_c  FNAME(h5pset_preserve_c)
#   define nh5pget_preserve_c  FNAME(h5pget_preserve_c)
#   define nh5pset_chunk_c    FNAME(h5pset_chunk_c)
#   define nh5pget_chunk_c    FNAME(h5pget_chunk_c)
#   define nh5pset_fill_valuec_c FNAME(h5pset_fill_valuec_c)
#   define nh5pset_fill_value_c FNAME(h5pset_fill_value_c)
#   define nh5pget_fill_valuec_c FNAME(h5pget_fill_valuec_c)
#   define nh5pget_fill_value_c FNAME(h5pget_fill_value_c)
#   define nh5pget_version_c    FNAME(h5pget_version_c)
#   define nh5pget_userblock_c    FNAME(h5pget_userblock_c)
#   define nh5pset_userblock_c    FNAME(h5pset_userblock_c)
#   define nh5pset_sizes_c        FNAME(h5pset_sizes_c)
#   define nh5pget_sizes_c         FNAME(h5pget_sizes_c)
#   define nh5pget_sym_k_c         FNAME(h5pget_sym_k_c)
#   define nh5pset_sym_k_c         FNAME(h5pset_sym_k_c)
#   define nh5pget_istore_k_c         FNAME(h5pget_istore_k_c)
#   define nh5pset_istore_k_c         FNAME(h5pset_istore_k_c)
#   define nh5pget_driver_c         FNAME(h5pget_driver_c)
#   define nh5pset_fapl_stdio_c         FNAME(h5pset_fapl_stdio_c)
#   define nh5pget_fapl_stdio_c         FNAME(h5pget_fapl_stdio_c)
#   define nh5pset_fapl_sec2_c         FNAME(h5pset_fapl_sec2_c)
#   define nh5pget_fapl_sec2_c         FNAME(h5pget_fapl_sec2_c)
#   define nh5pset_alignment_c         FNAME(h5pset_alignment_c)
#   define nh5pget_alignment_c         FNAME(h5pget_alignment_c)
#   define nh5pset_fapl_core_c         FNAME(h5pset_fapl_core_c)
#   define nh5pget_fapl_core_c         FNAME(h5pget_fapl_core_c)
#   define nh5pset_fapl_family_c         FNAME(h5pset_fapl_family_c)
#   define nh5pget_fapl_family_c         FNAME(h5pget_fapl_family_c)
#   define nh5pset_cache_c         FNAME(h5pset_cache_c)
#   define nh5pget_cache_c         FNAME(h5pget_cache_c)
#   define nh5pset_fapl_split_c         FNAME(h5pset_fapl_split_c)
#   define nh5pget_fapl_split_c         FNAME(h5pget_fapl_split_c)
#   define nh5pset_gc_references_c         FNAME(h5pset_gc_references_c)
#   define nh5pget_gc_references_c         FNAME(h5pget_gc_references_c)
#   define nh5pset_layout_c         FNAME(h5pset_layout_c)
#   define nh5pget_layout_c         FNAME(h5pget_layout_c)
#   define nh5pset_filter_c         FNAME(h5pset_filter_c)
#   define nh5pget_nfilters_c         FNAME(h5pget_nfilters_c)
#   define nh5pget_filter_c         FNAME(h5pget_filter_c)
#   define nh5pset_external_c         FNAME(h5pset_external_c)
#   define nh5pget_external_count_c         FNAME(h5pget_external_count_c)
#   define nh5pget_external_c         FNAME(h5pget_external_c)
#   define nh5pset_hyper_cache_c         FNAME(h5pset_hyper_cache_c)
#   define nh5pget_hyper_cache_c         FNAME(h5pget_hyper_cache_c)
#   define nh5pget_btree_ratios_c         FNAME(h5pget_btree_ratios_c)
#   define nh5pset_btree_ratios_c         FNAME(h5pset_btree_ratios_c)
#   define nh5pset_fapl_mpio_c         FNAME(h5pset_fapl_mpio_c)
#   define nh5pget_fapl_mpio_c         FNAME(h5pget_fapl_mpio_c)
#   define nh5pset_dxpl_mpio_c        FNAME(h5pset_dxpl_mpio_c)
#   define nh5pget_dxpl_mpio_c        FNAME(h5pget_dxpl_mpio_c)

#endif
#endif

__DLL__ int_f nh5pcreate_c ( int_f *classtype, hid_t_f *prp_id );

__DLL__ int_f nh5pclose_c ( hid_t_f *prp_id );

__DLL__ int_f nh5pcopy_c ( hid_t_f *prp_id , hid_t_f *new_prp_id);

__DLL__ int_f nh5pget_class_c ( hid_t_f *prp_id , int_f *classtype);

__DLL__ int_f nh5pset_deflate_c ( hid_t_f *prp_id , int_f *level);

__DLL__ int_f nh5pset_chunk_c ( hid_t_f *prp_id, int_f *rank, hsize_t_f *dims );

__DLL__ int_f nh5pget_chunk_c ( hid_t_f *prp_id, int_f *max_rank, hsize_t_f *dims );

__DLL__ int_f 
nh5pset_fill_valuec_c (hid_t_f *prp_id, hid_t_f *type_id, _fcd fillvalue);

__DLL__ int_f
nh5pset_fill_value_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);

__DLL__ int_f 
nh5pget_fill_valuec_c (hid_t_f *prp_id, hid_t_f *type_id, _fcd fillvalue);

__DLL__ int_f 
nh5pget_fill_value_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);

__DLL__ int_f
nh5pset_preserve_c ( hid_t_f *prp_id , int_f *flag);

__DLL__ int_f
nh5pget_preserve_c ( hid_t_f *prp_id , int_f *flag);
__DLL__ int_f
nh5pget_version_c (hid_t_f *prp_id, int_f * boot,int_f * freelist, int_f * stab, int_f *shhdr);
__DLL__ int_f
nh5pset_userblock_c (hid_t_f *prp_id, hsize_t_f * size);
__DLL__ int_f 
nh5pget_userblock_c (hid_t_f *prp_id, hsize_t_f * size);
__DLL__ int_f
nh5pget_sizes_c (hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size);
__DLL__ int_f
nh5pset_sizes_c (hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size);
__DLL__ int_f
nh5pset_sym_k_c (hid_t_f *prp_id, int_f* ik, int_f* lk);
__DLL__ int_f 
nh5pget_sym_k_c (hid_t_f *prp_id, int_f* ik, int_f* lk);
__DLL__ int_f
nh5pset_istore_k_c (hid_t_f *prp_id, int_f* ik);
__DLL__ int_f 
nh5pget_istore_k_c (hid_t_f *prp_id, int_f* ik);
__DLL__ int_f 
nh5pget_driver_c (hid_t_f *prp_id, hid_t_f*driver);
__DLL__ int_f 
nh5pset_fapl_stdio_c (hid_t_f *prp_id);
__DLL__ int_f 
nh5pget_fapl_stdio_c (hid_t_f *prp_id, int_f* io);
__DLL__ int_f 
nh5pset_fapl_sec2_c (hid_t_f *prp_id);
__DLL__ int_f 
nh5pget_fapl_sec2_c (hid_t_f *prp_id, int_f* sec2);
__DLL__ int_f 
nh5pset_alignment_c(hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment);
__DLL__ int_f 
nh5pget_alignment_c(hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment);
__DLL__ int_f
nh5pget_fapl_core_c (hid_t_f *prp_id, size_t_f* increment, int_f *flag);
__DLL__ int_f
nh5pset_fapl_core_c (hid_t_f *prp_id, size_t_f* increment, int_f *flag);
__DLL__ int_f
nh5pset_fapl_family_c (hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist );
__DLL__ int_f
nh5pget_fapl_family_c (hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist );
__DLL__ int_f
nh5pset_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, int_f* rdcc_nelmts, size_t_f* rdcc_nbytes, real_f* rdcc_w0);
__DLL__ int_f
nh5pget_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, int_f* rdcc_nelmts, size_t_f* rdcc_nbytes, real_f* rdcc_w0);
__DLL__ int_f
nh5pget_fapl_split_c(hid_t_f *prp_id, size_t_f* meta_ext_size , _fcd meta_ext, hid_t_f* meta_plist, size_t_f* raw_ext_size, _fcd raw_ext, hid_t_f * raw_plist);
__DLL__ int_f
nh5pset_fapl_split_c(hid_t_f *prp_id, int_f* meta_len, _fcd meta_ext, hid_t_f* meta_plist, int_f* raw_len, _fcd raw_ext, hid_t_f * raw_plist);
__DLL__ int_f
nh5pset_gc_references_c(hid_t_f *prp_id, int_f* gc_references);
__DLL__ int_f
nh5pget_gc_references_c(hid_t_f *prp_id, int_f* gc_references);
__DLL__ int_f
nh5pset_layout_c (hid_t_f *prp_id, int_f* layout);
__DLL__ int_f
nh5pget_layout_c (hid_t_f *prp_id, int_f* layout);
__DLL__ int_f
nh5pset_filter_c (hid_t_f *prp_id, int_f* filter, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values );
__DLL__ int_f
nh5pget_nfilters_c (hid_t_f *prp_id, int_f* nfilters);
__DLL__ int_f
nh5pget_filter_c(hid_t_f *prp_id, int_f* filter_number, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values, size_t_f *namelen, _fcd name, int_f* filter_id);
__DLL__ int_f
nh5pset_external_c (hid_t_f *prp_id, _fcd name, int_f* namelen, int_f* offset, hsize_t_f*bytes);
__DLL__ int_f
nh5pget_external_count_c (hid_t_f *prp_id, int_f* count);
__DLL__ int_f
nh5pget_external_c(hid_t_f *prp_id,int*idx, size_t_f* name_size, _fcd name, int_f* offset, hsize_t_f*bytes);
__DLL__ int_f
nh5pset_hyper_cache_c(hid_t_f *prp_id, int_f* cache, int_f* limit);
__DLL__ int_f
nh5pget_hyper_cache_c(hid_t_f *prp_id, int_f* cache, int_f* limit);
__DLL__ int_f 
nh5pget_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right);
__DLL__ int_f 
nh5pset_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right);
__DLL__ int_f
nh5pget_fapl_mpio_c(hid_t_f *prp_id, int_f* comm, int_f* info);
__DLL__ int_f
nh5pset_fapl_mpio_c(hid_t_f *prp_id, int_f* comm, int_f* info);
__DLL__ int_f
nh5pget_dxpl_mpio_rc(hid_t_f *prp_id, int_f* data_xfer_mode);
__DLL__ int_f
nh5pset_dxpl_mpio_c(hid_t_f *prp_id, int_f* data_xfer_mode);

/*
 * Functions frome H5Rf.c 
 */
#ifndef H5Rf90_FNAMES
#    define H5Rf90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5rcreate_object_c     FNAME(H5RCREATE_OBJECT_C)
#   define nh5rcreate_region_c     FNAME(H5RCREATE_REGION_C)
#   define nh5rdereference_region_c FNAME(H5RDEREFERENCE_REGION_C)
#   define nh5rdereference_object_c FNAME(H5RDEREFERENCE_OBJECT_C)
#   define nh5rget_region_region_c FNAME(H5RGET_REGION_REGION_C)
#   define nh5rget_object_type_obj_c FNAME(H5RGET_OBJECT_TYPE_OBJ_C)
#else                                              /* !DF_CAPFNAMES */
#   define nh5rcreate_object_c     FNAME(h5rcreate_object_c)
#   define nh5rcreate_region_c     FNAME(h5rcreate_region_c)
#   define nh5rdereference_region_c FNAME(h5rdereference_region_c)
#   define nh5rdereference_object_c FNAME(h5rdereference_object_c)
#   define nh5rget_region_region_c FNAME(h5rget_region_region_c)
#   define nh5rget_object_type_obj_c FNAME(h5rget_object_type_obj_c)
#endif                                             /* DF_CAPFNAMES */
#endif                                             /* H5Rf90_FNAMES */

__DLL__ int_f
nh5rcreate_object_c (int_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen);


__DLL__ int_f
nh5rcreate_region_c (int_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *space_id);

__DLL__ int_f
nh5rdereference_region_c (hid_t_f *dset_id, int_f *ref, hid_t_f *obj_id);

__DLL__ int_f
nh5rdereference_object_c (hid_t_f *dset_id, int_f *ref, hid_t_f *obj_id);

__DLL__ int_f
nh5rget_region_region_c (hid_t_f *dset_id, int_f *ref, hid_t_f *space_id);

__DLL__ int_f
nh5rget_object_type_obj_c (hid_t_f *dset_id, int_f *ref, int_f *obj_type);

/*
 * Functions from H5If.c
 */
#ifndef H5If90_FNAMES
#    define H5If90_FNAMES
#ifdef DF_CAPFNAMES
#  define nh5iget_type_c    FNAME(H5IGET_TYPE_C)
#else
#  define nh5iget_type_c    FNAME(h5iget_type_c)
#endif
#endif

__DLL__ int_f nh5iget_type_c(hid_t_f *obj_id, int_f *type);


#ifndef H5Ef90_FNAMES
#    define H5Ef90_FNAMES
#ifdef DF_CAPFNAMES
#  define nh5eclear_c    FNAME(H5ECLEAR_C)
#  define nh5eprint_c1   FNAME(H5EPRINT_C1)
#  define nh5eprint_c2   FNAME(H5EPRINT_C2)
#  define nh5eget_major_c FNAME(H5EGET_MAJOR_C)
#  define nh5eget_minor_c FNAME(H5EGET_MINOR_C)
#  define nh5eset_auto_c  FNAME(H5ESET_AUTO_C)
#else
#  define nh5eclear_c    FNAME(h5eclear_c)
#  define nh5eprint_c1   FNAME(h5eprint_c1)
#  define nh5eprint_c2   FNAME(h5eprint_c2) 
#  define nh5eget_major_c FNAME(h5eget_major_c)
#  define nh5eget_minor_c FNAME(h5eget_minor_c)
#  define nh5eset_auto_c  FNAME(h5eset_auto_c) 
#endif
#endif

__DLL__ int_f nh5eclear_c();
__DLL__ int_f nh5eprint_c1(_fcd name, int_f* namelen);
__DLL__ int_f nh5eprint_c2();
__DLL__ int_f nh5eget_major_c(int_f* error_no, _fcd name);
__DLL__ int_f nh5eget_minor_c(int_f* error_no, _fcd name);
__DLL__ int_f nh5eset_auto_c(int_f* printflag);

/*
 * Functions from H5f.c 
 */
#ifndef H5_FNAMES
#    define H5_FNAMES
#ifdef DF_CAPFNAMES 
#   define nh5open_c         FNAME(H5OPEN_C)
#   define nh5close_c         FNAME(H5CLOSE_C)
#   define nh5init_types_c    FNAME(H5INIT_TYPES_C)
#   define nh5close_types_c   FNAME(H5CLOSE_TYPES_C)
#   define nh5init_flags_c    FNAME(H5INIT_FLAGS_C)
#else 
#   define nh5open_c         FNAME(h5open_c)
#   define nh5close_c         FNAME(h5close_c)
#   define nh5init_types_c    FNAME(h5init_types_c)
#   define nh5close_types_c   FNAME(h5close_types_c)
#   define nh5init_flags_c    FNAME(h5init_flags_c)
#endif
#endif
__DLL__ int_f nh5open_c(void);
__DLL__ int_f nh5close_c(void);
__DLL__ int_f nh5init_types_c(hid_t_f *types, hid_t_f * floatingtypes, hid_t_f * integertypes);
__DLL__ int_f nh5close_types_c(hid_t_f *types, int_f *lentypes, hid_t_f * floatingtypes, int_f * floatinglen, hid_t_f * integertypes,  int_f * integerlen);
 
__DLL__ int_f nh5init_flags_c( int_f *h5d_flags, int_f *h5e_flags, int_f *h5f_flags,
                              int_f *h5fd_flags, int_f *h5g_flags, int_f *h5i_flags,
                              int_f *h5p_flags, int_f *h5r_flags, int_f *h5s_flags,
                              int_f *h5t_flags); 
#endif /* _H5f90proto_H */
