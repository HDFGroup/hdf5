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


#ifndef _H5f90proto_H
#define _H5f90proto_H

H5_FCDLL char * HD5f2cstring (_fcd fdesc, size_t len);
H5_FCDLL void HD5packFstring(char *src, char *dest, size_t len);

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
#   define nh5fget_obj_count_c       FNAME(H5FGET_OBJ_COUNT_C)
#   define nh5fget_obj_ids_c         FNAME(H5FGET_OBJ_IDS_C)
#   define nh5fget_freespace_c       FNAME(H5FGET_FREESPACE_C)
#   define nh5fget_name_c            FNAME(H5FGET_NAME_C)
#   define nh5fget_filesize_c        FNAME(H5FGET_FILESIZE_C)
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
#   define nh5fget_obj_count_c     FNAME(h5fget_obj_count_c)
#   define nh5fget_obj_ids_c       FNAME(h5fget_obj_ids_c)
#   define nh5fget_freespace_c     FNAME(h5fget_freespace_c)
#   define nh5fget_name_c          FNAME(h5fget_name_c)
#   define nh5fget_filesize_c      FNAME(h5fget_filesize_c)
#endif                                             /* DF_CAPFNAMES */
#endif                                             /* H5Ff90_FNAMES */

H5_FCDLL int_f nh5fcreate_c (_fcd name, int_f *namelen, int_f *access_flags, hid_t_f *crt_prp, hid_t_f *acc_prp, hid_t_f *file_id);
H5_FCDLL int_f nh5fopen_c (_fcd name, int_f *namelen, int_f *access_flags, hid_t_f *acc_prp, hid_t_f *file_id);
H5_FCDLL int_f nh5fis_hdf5_c (_fcd name, int_f *namelen, int_f *flag);
H5_FCDLL int_f nh5fclose_c (hid_t_f *file_id);
H5_FCDLL int_f nh5fmount_c (hid_t_f *loc_id, _fcd dsetname, int_f *namelen, hid_t_f *file_id, hid_t_f *acc_prp);
H5_FCDLL int_f nh5funmount_c (hid_t_f *loc_id, _fcd dsetname, int_f *namelen);
H5_FCDLL int_f nh5freopen_c (hid_t_f *file_id1, hid_t_f *file_id2);
H5_FCDLL int_f nh5fget_create_plist_c (hid_t_f *file_id, hid_t_f *prop_id);
H5_FCDLL int_f nh5fget_access_plist_c (hid_t_f *file_id, hid_t_f *access_id);
H5_FCDLL int_f nh5fget_obj_count_c (hid_t_f *file_id, int_f *obj_type, size_t_f *obj_count);
H5_FCDLL int_f nh5fget_obj_ids_c (hid_t_f *file_id, int_f *obj_type, size_t_f *max_objs, hid_t_f *obj_ids, size_t_f *num_objs);
H5_FCDLL int_f nh5fget_freespace_c (hid_t_f *file_id, hssize_t_f *free_space);
H5_FCDLL int_f nh5fflush_c (hid_t_f *obj_id, int_f *scope);
H5_FCDLL int_f nh5fget_name_c(hid_t_f *obj_id, size_t_f *size, _fcd buf, size_t_f *buflen);
H5_FCDLL int_f nh5fget_filesize_c(hid_t_f *file_id, hsize_t_f *size);

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
#   define nh5sget_select_type_c   FNAME(H5SGET_SELECT_TYPE_C)
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
#   define nh5sget_select_type_c   FNAME(h5sget_select_type_c)
#   define nh5sselect_elements_c    FNAME(h5sselect_elements_c)
#endif                                             /* DF_CAPFNAMES */
#endif

H5_FCDLL int_f nh5screate_simple_c ( int_f *rank, hsize_t_f *dims, hsize_t_f *maxdims, hid_t_f *space_id );
H5_FCDLL int_f nh5sclose_c ( hid_t_f *space_id );
H5_FCDLL int_f nh5screate_c ( int_f *classtype, hid_t_f *space_id );
H5_FCDLL int_f nh5scopy_c ( hid_t_f *space_id , hid_t_f *new_space_id);
H5_FCDLL int_f nh5sget_select_hyper_nblocks_c( hid_t_f *space_id , hssize_t_f * num_blocks);
H5_FCDLL int_f nh5sget_select_hyper_blocklist_c( hid_t_f *space_id ,hsize_t_f * startblock, hsize_t_f * num_blocks, hsize_t_f * buf);
H5_FCDLL int_f nh5sget_select_bounds_c( hid_t_f *space_id , hsize_t_f * start, hsize_t_f * end);
H5_FCDLL int_f nh5sget_select_elem_npoints_c( hid_t_f *space_id , hssize_t_f * num_points);
H5_FCDLL int_f nh5sget_select_elem_pointlist_c( hid_t_f *space_id ,hsize_t_f * startpoint, hsize_t_f * numpoints, hsize_t_f * buf);
H5_FCDLL int_f nh5sselect_all_c ( hid_t_f *space_id );
H5_FCDLL int_f nh5sselect_none_c ( hid_t_f *space_id );
H5_FCDLL int_f nh5sselect_valid_c ( hid_t_f *space_id , int_f *flag );
H5_FCDLL int_f nh5sget_simple_extent_npoints_c ( hid_t_f *space_id , hsize_t_f *npoints );
H5_FCDLL int_f nh5sget_select_npoints_c ( hid_t_f *space_id , hssize_t_f *npoints );
H5_FCDLL int_f nh5sget_simple_extent_ndims_c ( hid_t_f *space_id , int_f *ndims );
H5_FCDLL int_f nh5sget_simple_extent_type_c ( hid_t_f *space_id , int_f *classtype);
H5_FCDLL int_f nh5soffset_simple_c ( hid_t_f *space_id , hssize_t_f *offset);
H5_FCDLL int_f nh5sset_extent_simple_c ( hid_t_f *space_id , int_f *rank, hsize_t_f * current_size, hsize_t_f *maximum_size);
H5_FCDLL int_f nh5sis_simple_c ( hid_t_f *space_id , int_f *flag );
H5_FCDLL int_f nh5sextent_class_c ( hid_t_f *space_id , int_f *classtype);
H5_FCDLL int_f nh5sget_simple_extent_dims_c ( hid_t_f *space_id , hsize_t_f *dims, hsize_t_f *maxdims);
H5_FCDLL int_f nh5sextent_copy_c ( hid_t_f *dest_space_id , hid_t_f *source_space_id);
H5_FCDLL int_f nh5sset_extent_none_c ( hid_t_f *space_id );
H5_FCDLL int_f nh5sselect_hyperslab_c ( hid_t_f *space_id , int_f *op, hsize_t_f *start, hsize_t_f *count, hsize_t_f *stride, hsize_t_f *block);
H5_FCDLL int_f nh5sget_select_type_c ( hid_t_f *space_id , int_f *op);
H5_FCDLL int_f nh5sselect_elements_c ( hid_t_f *space_id , int_f *op, size_t_f *nelements, hsize_t_f *coord);


/*
 * Functions from H5Df.c
 */
#ifndef H5Df90_FNAMES
#    define H5Df90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5dcreate_c                FNAME(H5DCREATE_C)
#   define nh5dclose_c                 FNAME(H5DCLOSE_C)
#   define nh5dopen_c                  FNAME(H5DOPEN_C)
#   define nh5dwrite_c                 FNAME(H5DWRITE_C)
#   define nh5dwrite_integer_s_c              FNAME(H5DWRITE_INTEGER_S_C)
#   define nh5dwrite_integer_1_c              FNAME(H5DWRITE_INTEGER_1_C)
#   define nh5dwrite_integer_2_c              FNAME(H5DWRITE_INTEGER_2_C)
#   define nh5dwrite_integer_3_c              FNAME(H5DWRITE_INTEGER_3_C)
#   define nh5dwrite_integer_4_c              FNAME(H5DWRITE_INTEGER_4_C)
#   define nh5dwrite_integer_5_c              FNAME(H5DWRITE_INTEGER_5_C)
#   define nh5dwrite_integer_6_c              FNAME(H5DWRITE_INTEGER_6_C)
#   define nh5dwrite_integer_7_c              FNAME(H5DWRITE_INTEGER_7_C)
#   define nh5dwrite_real_s_c                 FNAME(H5DWRITE_REAL_S_C)
#   define nh5dwrite_real_1_c                 FNAME(H5DWRITE_REAL_1_C)
#   define nh5dwrite_real_2_c                 FNAME(H5DWRITE_REAL_2_C)
#   define nh5dwrite_real_3_c                 FNAME(H5DWRITE_REAL_3_C)
#   define nh5dwrite_real_4_c                 FNAME(H5DWRITE_REAL_4_C)
#   define nh5dwrite_real_5_c                 FNAME(H5DWRITE_REAL_5_C)
#   define nh5dwrite_real_6_c                 FNAME(H5DWRITE_REAL_6_C)
#   define nh5dwrite_real_7_c                 FNAME(H5DWRITE_REAL_7_C)
#   define nh5dwrite_double_s_c               FNAME(H5DWRITE_DOUBLE_S_C)
#   define nh5dwrite_double_1_c               FNAME(H5DWRITE_DOUBLE_1_C)
#   define nh5dwrite_double_2_c               FNAME(H5DWRITE_DOUBLE_2_C)
#   define nh5dwrite_double_3_c               FNAME(H5DWRITE_DOUBLE_3_C)
#   define nh5dwrite_double_4_c               FNAME(H5DWRITE_DOUBLE_4_C)
#   define nh5dwrite_double_5_c               FNAME(H5DWRITE_DOUBLE_5_C)
#   define nh5dwrite_double_6_c               FNAME(H5DWRITE_DOUBLE_6_C)
#   define nh5dwrite_double_7_c               FNAME(H5DWRITE_DOUBLE_7_C)
#   define nh5dwrite_ref_obj_c         FNAME(H5DWRITE_REF_OBJ_C)
#   define nh5dwrite_ref_reg_c         FNAME(H5DWRITE_REF_REG_C)
#   define nh5dwritec_c                FNAME(H5DWRITEC_C)
#   define nh5dwritec_s_c                     FNAME(H5DWRITEC_S_C)
#   define nh5dwritec_1_c                     FNAME(H5DWRITEC_1_C)
#   define nh5dwritec_2_c                     FNAME(H5DWRITEC_2_C)
#   define nh5dwritec_3_c                     FNAME(H5DWRITEC_3_C)
#   define nh5dwritec_4_c                     FNAME(H5DWRITEC_4_C)
#   define nh5dwritec_5_c                     FNAME(H5DWRITEC_5_C)
#   define nh5dwritec_6_c                     FNAME(H5DWRITEC_6_C)
#   define nh5dwritec_7_c                     FNAME(H5DWRITEC_7_C)
#   define nh5dread_c                  FNAME(H5DREAD_C)
#   define nh5dread_integer_s_c              FNAME(H5DREAD_INTEGER_S_C)
#   define nh5dread_integer_1_c              FNAME(H5DREAD_INTEGER_1_C)
#   define nh5dread_integer_2_c              FNAME(H5DREAD_INTEGER_2_C)
#   define nh5dread_integer_3_c              FNAME(H5DREAD_INTEGER_3_C)
#   define nh5dread_integer_4_c              FNAME(H5DREAD_INTEGER_4_C)
#   define nh5dread_integer_5_c              FNAME(H5DREAD_INTEGER_5_C)
#   define nh5dread_integer_6_c              FNAME(H5DREAD_INTEGER_6_C)
#   define nh5dread_integer_7_c              FNAME(H5DREAD_INTEGER_7_C)
#   define nh5dread_real_s_c                 FNAME(H5DREAD_REAL_S_C)
#   define nh5dread_real_1_c                 FNAME(H5DREAD_REAL_1_C)
#   define nh5dread_real_2_c                 FNAME(H5DREAD_REAL_2_C)
#   define nh5dread_real_3_c                 FNAME(H5DREAD_REAL_3_C)
#   define nh5dread_real_4_c                 FNAME(H5DREAD_REAL_4_C)
#   define nh5dread_real_5_c                 FNAME(H5DREAD_REAL_5_C)
#   define nh5dread_real_6_c                 FNAME(H5DREAD_REAL_6_C)
#   define nh5dread_real_7_c                 FNAME(H5DREAD_REAL_7_C)
#   define nh5dread_double_s_c               FNAME(H5DREAD_DOUBLE_S_C)
#   define nh5dread_double_1_c               FNAME(H5DREAD_DOUBLE_1_C)
#   define nh5dread_double_2_c               FNAME(H5DREAD_DOUBLE_2_C)
#   define nh5dread_double_3_c               FNAME(H5DREAD_DOUBLE_3_C)
#   define nh5dread_double_4_c               FNAME(H5DREAD_DOUBLE_4_C)
#   define nh5dread_double_5_c               FNAME(H5DREAD_DOUBLE_5_C)
#   define nh5dread_double_6_c               FNAME(H5DREAD_DOUBLE_6_C)
#   define nh5dread_double_7_c               FNAME(H5DREAD_DOUBLE_7_C)
#   define nh5dread_c_b                FNAME(H5DREAD_C_B)
#   define nh5dread_ref_reg_c          FNAME(H5DREAD_REF_REG_C)
#   define nh5dread_ref_obj_c          FNAME(H5DREAD_REF_OBJ_C)
#   define nh5dreadc_c                 FNAME(H5DREADC_C)
#   define nh5dreadc_s_c                     FNAME(H5DREADC_S_C)
#   define nh5dreadc_1_c                     FNAME(H5DREADC_1_C)
#   define nh5dreadc_2_c                     FNAME(H5DREADC_2_C)
#   define nh5dreadc_3_c                     FNAME(H5DREADC_3_C)
#   define nh5dreadc_4_c                     FNAME(H5DREADC_4_C)
#   define nh5dreadc_5_c                     FNAME(H5DREADC_5_C)
#   define nh5dreadc_6_c                     FNAME(H5DREADC_6_C)
#   define nh5dreadc_7_c                     FNAME(H5DREADC_7_C)
#   define nh5dreadc_c_b               FNAME(H5DREADC_C_B)
#   define nh5dget_space_c             FNAME(H5DGET_SPACE_C)
#   define nh5dget_type_c              FNAME(H5DGET_TYPE_C)
#   define nh5dget_create_plist_c      FNAME(H5DGET_CREATE_PLIST_C)
#   define nh5dextend_c                FNAME(H5DEXTEND_C)
#   define nh5dget_storage_size_c      FNAME(H5DGET_STORAGE_SIZE_C)
#   define nh5dvlen_get_max_len_c      FNAME(H5DVLEN_GET_MAX_LEN_C)
#   define nh5dwrite_vl_integer_c      FNAME(H5DWRITE_VL_INTEGER_C)
#   define nh5dread_vl_integer_c       FNAME(H5DREAD_VL_INTEGER_C)
#   define nh5dwrite_vl_real_c         FNAME(H5DWRITE_VL_REAL_C)
#   define nh5dread_vl_real_c          FNAME(H5DREAD_VL_REAL_C)
#   define nh5dwrite_vl_string_c       FNAME(H5DWRITE_VL_STRING_C)
#   define nh5dread_vl_string_c        FNAME(H5DREAD_VL_STRING_C)
#   define nh5dfillc_c                 FNAME(H5DFILLC_C)
#   define nh5dfill_c                  FNAME(H5DFILL_C)
#   define nh5dfill_integer_c               FNAME(H5DFILL_INTEGER_C)
#   define nh5dfill_real_c                  FNAME(H5DFILL_REAL_C)
#   define nh5dfill_double_c                FNAME(H5DFILL_DOUBLE_C)
#   define nh5dget_space_status_c      FNAME(H5DGET_SPACE_STATUS_C)
#else                                              /* !DF_CAPFNAMES */
#   define nh5dcreate_c                FNAME(h5dcreate_c)
#   define nh5dclose_c                 FNAME(h5dclose_c)
#   define nh5dopen_c                  FNAME(h5dopen_c)
#   define nh5dwrite_c                 FNAME(h5dwrite_c)
#   define nh5dwrite_integer_s_c              FNAME(h5dwrite_integer_s_c)
#   define nh5dwrite_integer_1_c              FNAME(h5dwrite_integer_1_c)
#   define nh5dwrite_integer_2_c              FNAME(h5dwrite_integer_2_c)
#   define nh5dwrite_integer_3_c              FNAME(h5dwrite_integer_3_c)
#   define nh5dwrite_integer_4_c              FNAME(h5dwrite_integer_4_c)
#   define nh5dwrite_integer_5_c              FNAME(h5dwrite_integer_5_c)
#   define nh5dwrite_integer_6_c              FNAME(h5dwrite_integer_6_c)
#   define nh5dwrite_integer_7_c              FNAME(h5dwrite_integer_7_c)
#   define nh5dwrite_real_s_c                 FNAME(h5dwrite_real_s_c)
#   define nh5dwrite_real_1_c                 FNAME(h5dwrite_real_1_c)
#   define nh5dwrite_real_2_c                 FNAME(h5dwrite_real_2_c)
#   define nh5dwrite_real_3_c                 FNAME(h5dwrite_real_3_c)
#   define nh5dwrite_real_4_c                 FNAME(h5dwrite_real_4_c)
#   define nh5dwrite_real_5_c                 FNAME(h5dwrite_real_5_c)
#   define nh5dwrite_real_6_c                 FNAME(h5dwrite_real_6_c)
#   define nh5dwrite_real_7_c                 FNAME(h5dwrite_real_7_c)
#   define nh5dwrite_double_s_c               FNAME(h5dwrite_double_s_c)
#   define nh5dwrite_double_1_c               FNAME(h5dwrite_double_1_c)
#   define nh5dwrite_double_2_c               FNAME(h5dwrite_double_2_c)
#   define nh5dwrite_double_3_c               FNAME(h5dwrite_double_3_c)
#   define nh5dwrite_double_4_c               FNAME(h5dwrite_double_4_c)
#   define nh5dwrite_double_5_c               FNAME(h5dwrite_double_5_c)
#   define nh5dwrite_double_6_c               FNAME(h5dwrite_double_6_c)
#   define nh5dwrite_double_7_c               FNAME(h5dwrite_double_7_c)
#   define nh5dwritec_c                FNAME(h5dwritec_c)
#   define nh5dwritec_s_c                     FNAME(h5dwritec_s_c)
#   define nh5dwritec_1_c                     FNAME(h5dwritec_1_c)
#   define nh5dwritec_2_c                     FNAME(h5dwritec_2_c)
#   define nh5dwritec_3_c                     FNAME(h5dwritec_3_c)
#   define nh5dwritec_4_c                     FNAME(h5dwritec_4_c)
#   define nh5dwritec_5_c                     FNAME(h5dwritec_5_c)
#   define nh5dwritec_6_c                     FNAME(h5dwritec_6_c)
#   define nh5dwritec_7_c                     FNAME(h5dwritec_7_c)
#   define nh5dwrite_ref_obj_c         FNAME(h5dwrite_ref_obj_c)
#   define nh5dwrite_ref_reg_c         FNAME(h5dwrite_ref_reg_c)
#   define nh5dread_c                  FNAME(h5dread_c)
#   define nh5dread_integer_s_c              FNAME(h5dread_integer_s_c)
#   define nh5dread_integer_1_c              FNAME(h5dread_integer_1_c)
#   define nh5dread_integer_2_c              FNAME(h5dread_integer_2_c)
#   define nh5dread_integer_3_c              FNAME(h5dread_integer_3_c)
#   define nh5dread_integer_4_c              FNAME(h5dread_integer_4_c)
#   define nh5dread_integer_5_c              FNAME(h5dread_integer_5_c)
#   define nh5dread_integer_6_c              FNAME(h5dread_integer_6_c)
#   define nh5dread_integer_7_c              FNAME(h5dread_integer_7_c)
#   define nh5dread_real_s_c                 FNAME(h5dread_real_s_c)
#   define nh5dread_real_1_c                 FNAME(h5dread_real_1_c)
#   define nh5dread_real_2_c                 FNAME(h5dread_real_2_c)
#   define nh5dread_real_3_c                 FNAME(h5dread_real_3_c)
#   define nh5dread_real_4_c                 FNAME(h5dread_real_4_c)
#   define nh5dread_real_5_c                 FNAME(h5dread_real_5_c)
#   define nh5dread_real_6_c                 FNAME(h5dread_real_6_c)
#   define nh5dread_real_7_c                 FNAME(h5dread_real_7_c)
#   define nh5dread_double_s_c               FNAME(h5dread_double_s_c)
#   define nh5dread_double_1_c               FNAME(h5dread_double_1_c)
#   define nh5dread_double_2_c               FNAME(h5dread_double_2_c)
#   define nh5dread_double_3_c               FNAME(h5dread_double_3_c)
#   define nh5dread_double_4_c               FNAME(h5dread_double_4_c)
#   define nh5dread_double_5_c               FNAME(h5dread_double_5_c)
#   define nh5dread_double_6_c               FNAME(h5dread_double_6_c)
#   define nh5dread_double_7_c               FNAME(h5dread_double_7_c)
#   define nh5dread_ref_reg_c          FNAME(h5dread_ref_reg_c)
#   define nh5dread_ref_obj_c          FNAME(h5dread_ref_obj_c)
#   define nh5dreadc_c                 FNAME(h5dreadc_c)
#   define nh5dreadc_s_c                     FNAME(h5dreadc_s_c)
#   define nh5dreadc_1_c                     FNAME(h5dreadc_1_c)
#   define nh5dreadc_2_c                     FNAME(h5dreadc_2_c)
#   define nh5dreadc_3_c                     FNAME(h5dreadc_3_c)
#   define nh5dreadc_4_c                     FNAME(h5dreadc_4_c)
#   define nh5dreadc_5_c                     FNAME(h5dreadc_5_c)
#   define nh5dreadc_6_c                     FNAME(h5dreadc_6_c)
#   define nh5dreadc_7_c                     FNAME(h5dreadc_7_c)
#   define nh5dget_space_c             FNAME(h5dget_space_c)
#   define nh5dget_type_c              FNAME(h5dget_type_c)
#   define nh5dget_create_plist_c      FNAME(h5dget_create_plist_c)
#   define nh5dextend_c                FNAME(h5dextend_c)
#   define nh5dget_storage_size_c      FNAME(h5dget_storage_size_c)
#   define nh5dvlen_get_max_len_c      FNAME(h5dvlen_get_max_len_c)
#   define nh5dwrite_vl_integer_c      FNAME(h5dwrite_vl_integer_c)
#   define nh5dread_vl_integer_c       FNAME(h5dread_vl_integer_c)
#   define nh5dwrite_vl_real_c         FNAME(h5dwrite_vl_real_c)
#   define nh5dread_vl_real_c          FNAME(h5dread_vl_real_c)
#   define nh5dwrite_vl_string_c       FNAME(h5dwrite_vl_string_c)
#   define nh5dread_vl_string_c        FNAME(h5dread_vl_string_c)
#   define nh5dfillc_c                 FNAME(h5dfillc_c)
#   define nh5dfill_c                  FNAME(h5dfill_c)
#   define nh5dfill_integer_c               FNAME(h5dfill_integer_c)
#   define nh5dfill_real_c                  FNAME(h5dfill_real_c)
#   define nh5dfill_double_c                FNAME(h5dfill_double_c)
#   define nh5dget_space_status_c      FNAME(h5dget_space_status_c)
#endif                                             /* DF_CAPFNAMES */
#endif

H5_FCDLL int_f nh5dcreate_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *space_id, hid_t_f *crt_prp,  hid_t_f *dset_id);
H5_FCDLL int_f nh5dopen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *dset_id);
H5_FCDLL int_f nh5dclose_c ( hid_t_f *dset_id );
H5_FCDLL int_f nh5dwrite_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dwrite_integer_s_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_integer_1_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_integer_2_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_integer_3_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_integer_4_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_integer_5_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_integer_6_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_integer_7_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dwrite_real_s_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_real_1_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_real_2_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_real_3_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_real_4_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_real_5_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_real_6_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_real_7_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dwrite_double_s_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_double_1_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_double_2_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_double_3_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_double_4_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_double_5_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_double_6_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_double_7_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dwrite_vl_integer_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f nh5dwrite_vl_real_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, real_f *buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f nh5dwrite_vl_string_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f nh5dwrite_ref_obj_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, haddr_t_f *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwrite_ref_reg_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwritec_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dwritec_s_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwritec_1_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwritec_2_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwritec_3_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwritec_4_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwritec_5_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwritec_6_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dwritec_7_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dread_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dread_integer_s_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_integer_1_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_integer_2_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_integer_3_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_integer_4_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_integer_5_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_integer_6_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_integer_7_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dread_real_s_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_real_1_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_real_2_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_real_3_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_real_4_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_real_5_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_real_6_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_real_7_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dread_double_s_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_double_1_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_double_2_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_double_3_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_double_4_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_double_5_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_double_6_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_double_7_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dread_vl_integer_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f nh5dread_vl_real_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, real_f *buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f nh5dread_vl_string_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f nh5dread_ref_obj_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, haddr_t_f * buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dread_ref_reg_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f * buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dreadc_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dreadc_s_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dreadc_1_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dreadc_2_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dreadc_3_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dreadc_4_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dreadc_5_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dreadc_6_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);
H5_FCDLL int_f nh5dreadc_7_c (hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims);

H5_FCDLL int_f nh5dget_space_c ( hid_t_f *dset_id , hid_t_f *space_id);
H5_FCDLL int_f nh5dget_type_c ( hid_t_f *dset_id , hid_t_f *type_id);
H5_FCDLL int_f nh5dget_create_plist_c ( hid_t_f *dset_id , hid_t_f *plist_id);
H5_FCDLL int_f nh5dextend_c ( hid_t_f *dset_id , hsize_t_f *dims);
H5_FCDLL int_f nh5dvlen_get_max_len_c(hid_t_f *dataset_id, hid_t_f *type_id, hid_t_f *space_id, size_t_f *len);
H5_FCDLL int_f nh5dget_storage_size_c(hid_t_f *dataset_id, hsize_t_f *size);
H5_FCDLL int_f nh5dfillc_c(_fcd fill_value, hid_t_f *fill_type_id, hid_t_f *space_id, _fcd buf, hid_t_f *mem_type_id);
H5_FCDLL int_f nh5dfill_c(void * fill_value, hid_t_f *fill_type_id, hid_t_f *space_id, void * buf, hid_t_f *mem_type_id);
H5_FCDLL int_f nh5dfill_integer_c(void * fill_value, hid_t_f *fill_type_id, hid_t_f *space_id, void * buf, hid_t_f *mem_type_id);
H5_FCDLL int_f nh5dfill_real_c(void * fill_value, hid_t_f *fill_type_id, hid_t_f *space_id, void * buf, hid_t_f *mem_type_id);
H5_FCDLL int_f nh5dfill_double_c(void * fill_value, hid_t_f *fill_type_id, hid_t_f *space_id, void * buf, hid_t_f *mem_type_id);
H5_FCDLL int_f nh5dget_space_status_c ( hid_t_f *dset_id, int_f *flag);

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
#   define nh5glink2_c        FNAME(H5GLINK2_C)
#   define nh5gunlink_c      FNAME(H5GUNLINK_C)
#   define nh5gmove_c        FNAME(H5GMOVE_C)
#   define nh5gmove2_c        FNAME(H5GMOVE2_C)
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
#   define nh5glink2_c        FNAME(h5glink2_c)
#   define nh5gunlink_c      FNAME(h5gunlink_c)
#   define nh5gmove_c        FNAME(h5gmove_c)
#   define nh5gmove2_c        FNAME(h5gmove2_c)
#   define nh5gget_linkval_c   FNAME(h5gget_linkval_c)
#   define nh5gset_comment_c   FNAME(h5gset_comment_c)
#   define nh5gget_comment_c   FNAME(h5gget_comment_c)
#endif                                             /* DF_CAPFNAMES */
#endif

H5_FCDLL int_f nh5gcreate_c (hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *size_hint,  hid_t_f *grp_id);
H5_FCDLL int_f nh5gopen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *grp_id);
H5_FCDLL int_f nh5gclose_c ( hid_t_f *grp_id );
H5_FCDLL int_f nh5gget_obj_info_idx_c (hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *idx, _fcd obj_name, int_f *obj_namelen, int_f *obj_type);
H5_FCDLL int_f nh5gn_members_c (hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *nmembers);
H5_FCDLL int_f nh5glink_c (hid_t_f *loc_id, int_f *link_type, _fcd current_name, int_f *current_namelen, _fcd new_name, int_f *new_namelen);
H5_FCDLL int_f nh5glink2_c (hid_t_f *cur_loc_id, _fcd cur_name, int_f *cur_namelen, int_f *link_type, hid_t_f *new_loc_id, _fcd new_name, int_f *new_namelen);
H5_FCDLL int_f nh5gunlink_c (hid_t_f *loc_id, _fcd name, int_f *namelen);
H5_FCDLL int_f nh5gmove_c (hid_t_f *loc_id, _fcd src_name, int_f *src_namelen, _fcd dst_name, int_f *dst_namelen);
H5_FCDLL int_f nh5gmove2_c (hid_t_f *src_loc_id, _fcd src_name, int_f *src_namelen, hid_t_f *dst_loc_id,_fcd dst_name, int_f *dst_namelen);
H5_FCDLL int_f nh5gget_linkval_c (hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *size, _fcd value );
H5_FCDLL int_f nh5gset_comment_c (hid_t_f *loc_id, _fcd name, int_f *namelen, _fcd comment, int_f *commentlen);
H5_FCDLL int_f nh5gget_comment_c (hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *bufsize, _fcd comment);

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
#   define nh5awrite_integer_s_c       FNAME(H5AWRITE_INTEGER_S_C)
#   define nh5awrite_integer_1_c       FNAME(H5AWRITE_INTEGER_1_C)
#   define nh5awrite_integer_2_c       FNAME(H5AWRITE_INTEGER_2_C)
#   define nh5awrite_integer_3_c       FNAME(H5AWRITE_INTEGER_3_C)
#   define nh5awrite_integer_4_c       FNAME(H5AWRITE_INTEGER_4_C)
#   define nh5awrite_integer_5_c       FNAME(H5AWRITE_INTEGER_5_C)
#   define nh5awrite_integer_6_c       FNAME(H5AWRITE_INTEGER_6_C)
#   define nh5awrite_integer_7_c       FNAME(H5AWRITE_INTEGER_7_C)
#   define nh5awrite_real_s_c          FNAME(H5AWRITE_REAL_S_C)
#   define nh5awrite_real_1_c          FNAME(H5AWRITE_REAL_1_C)
#   define nh5awrite_real_2_c          FNAME(H5AWRITE_REAL_2_C)
#   define nh5awrite_real_3_c          FNAME(H5AWRITE_REAL_3_C)
#   define nh5awrite_real_4_c          FNAME(H5AWRITE_REAL_4_C)
#   define nh5awrite_real_5_c          FNAME(H5AWRITE_REAL_5_C)
#   define nh5awrite_real_6_c          FNAME(H5AWRITE_REAL_6_C)
#   define nh5awrite_real_7_c          FNAME(H5AWRITE_REAL_7_C)
#   define nh5awrite_double_s_c        FNAME(H5AWRITE_DOUBLE_S_C)
#   define nh5awrite_double_1_c        FNAME(H5AWRITE_DOUBLE_1_C)
#   define nh5awrite_double_2_c        FNAME(H5AWRITE_DOUBLE_2_C)
#   define nh5awrite_double_3_c        FNAME(H5AWRITE_DOUBLE_3_C)
#   define nh5awrite_double_4_c        FNAME(H5AWRITE_DOUBLE_4_C)
#   define nh5awrite_double_5_c        FNAME(H5AWRITE_DOUBLE_5_C)
#   define nh5awrite_double_6_c        FNAME(H5AWRITE_DOUBLE_6_C)
#   define nh5awrite_double_7_c        FNAME(H5AWRITE_DOUBLE_7_C)
#   define nh5awritec_c      FNAME(H5AWRITEC_C)
#   define nh5awritec_s_c              FNAME(H5AWRITEC_S_C)
#   define nh5awritec_1_c              FNAME(H5AWRITEC_1_C)
#   define nh5awritec_2_c              FNAME(H5AWRITEC_2_C)
#   define nh5awritec_3_c              FNAME(H5AWRITEC_3_C)
#   define nh5awritec_4_c              FNAME(H5AWRITEC_4_C)
#   define nh5awritec_5_c              FNAME(H5AWRITEC_5_C)
#   define nh5awritec_6_c              FNAME(H5AWRITEC_6_C)
#   define nh5awritec_7_c              FNAME(H5AWRITEC_7_C)
#   define nh5aread_c        FNAME(H5AREAD_C)
#   define nh5aread_integer_s_c       FNAME(H5AREAD_INTEGER_S_C)
#   define nh5aread_integer_1_c       FNAME(H5AREAD_INTEGER_1_C)
#   define nh5aread_integer_2_c       FNAME(H5AREAD_INTEGER_2_C)
#   define nh5aread_integer_3_c       FNAME(H5AREAD_INTEGER_3_C)
#   define nh5aread_integer_4_c       FNAME(H5AREAD_INTEGER_4_C)
#   define nh5aread_integer_5_c       FNAME(H5AREAD_INTEGER_5_C)
#   define nh5aread_integer_6_c       FNAME(H5AREAD_INTEGER_6_C)
#   define nh5aread_integer_7_c       FNAME(H5AREAD_INTEGER_7_C)
#   define nh5aread_real_s_c          FNAME(H5AREAD_REAL_S_C)
#   define nh5aread_real_1_c          FNAME(H5AREAD_REAL_1_C)
#   define nh5aread_real_2_c          FNAME(H5AREAD_REAL_2_C)
#   define nh5aread_real_3_c          FNAME(H5AREAD_REAL_3_C)
#   define nh5aread_real_4_c          FNAME(H5AREAD_REAL_4_C)
#   define nh5aread_real_5_c          FNAME(H5AREAD_REAL_5_C)
#   define nh5aread_real_6_c          FNAME(H5AREAD_REAL_6_C)
#   define nh5aread_real_7_c          FNAME(H5AREAD_REAL_7_C)
#   define nh5aread_double_s_c        FNAME(H5AREAD_DOUBLE_S_C)
#   define nh5aread_double_1_c        FNAME(H5AREAD_DOUBLE_1_C)
#   define nh5aread_double_2_c        FNAME(H5AREAD_DOUBLE_2_C)
#   define nh5aread_double_3_c        FNAME(H5AREAD_DOUBLE_3_C)
#   define nh5aread_double_4_c        FNAME(H5AREAD_DOUBLE_4_C)
#   define nh5aread_double_5_c        FNAME(H5AREAD_DOUBLE_5_C)
#   define nh5aread_double_6_c        FNAME(H5AREAD_DOUBLE_6_C)
#   define nh5aread_double_7_c        FNAME(H5AREAD_DOUBLE_7_C)
#   define nh5areadc_c       FNAME(H5AREADC_C)
#   define nh5areadc_s_c              FNAME(H5AREADC_S_C)
#   define nh5areadc_1_c              FNAME(H5AREADC_1_C)
#   define nh5areadc_2_c              FNAME(H5AREADC_2_C)
#   define nh5areadc_3_c              FNAME(H5AREADC_3_C)
#   define nh5areadc_4_c              FNAME(H5AREADC_4_C)
#   define nh5areadc_5_c              FNAME(H5AREADC_5_C)
#   define nh5areadc_6_c              FNAME(H5AREADC_6_C)
#   define nh5areadc_7_c              FNAME(H5AREADC_7_C)
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
#   define nh5awrite_integer_s_c       FNAME(h5awrite_integer_s_c)
#   define nh5awrite_integer_1_c       FNAME(h5awrite_integer_1_c)
#   define nh5awrite_integer_2_c       FNAME(h5awrite_integer_2_c)
#   define nh5awrite_integer_3_c       FNAME(h5awrite_integer_3_c)
#   define nh5awrite_integer_4_c       FNAME(h5awrite_integer_4_c)
#   define nh5awrite_integer_5_c       FNAME(h5awrite_integer_5_c)
#   define nh5awrite_integer_6_c       FNAME(h5awrite_integer_6_c)
#   define nh5awrite_integer_7_c       FNAME(h5awrite_integer_7_c)
#   define nh5awrite_real_s_c          FNAME(h5awrite_real_s_c)
#   define nh5awrite_real_1_c          FNAME(h5awrite_real_1_c)
#   define nh5awrite_real_2_c          FNAME(h5awrite_real_2_c)
#   define nh5awrite_real_3_c          FNAME(h5awrite_real_3_c)
#   define nh5awrite_real_4_c          FNAME(h5awrite_real_4_c)
#   define nh5awrite_real_5_c          FNAME(h5awrite_real_5_c)
#   define nh5awrite_real_6_c          FNAME(h5awrite_real_6_c)
#   define nh5awrite_real_7_c          FNAME(h5awrite_real_7_c)
#   define nh5awrite_double_s_c        FNAME(h5awrite_double_s_c)
#   define nh5awrite_double_1_c        FNAME(h5awrite_double_1_c)
#   define nh5awrite_double_2_c        FNAME(h5awrite_double_2_c)
#   define nh5awrite_double_3_c        FNAME(h5awrite_double_3_c)
#   define nh5awrite_double_4_c        FNAME(h5awrite_double_4_c)
#   define nh5awrite_double_5_c        FNAME(h5awrite_double_5_c)
#   define nh5awrite_double_6_c        FNAME(h5awrite_double_6_c)
#   define nh5awrite_double_7_c        FNAME(h5awrite_double_7_c)
#   define nh5awritec_c      FNAME(h5awritec_c)
#   define nh5awritec_s_c              FNAME(h5awritec_s_c)
#   define nh5awritec_1_c              FNAME(h5awritec_1_c)
#   define nh5awritec_2_c              FNAME(h5awritec_2_c)
#   define nh5awritec_3_c              FNAME(h5awritec_3_c)
#   define nh5awritec_4_c              FNAME(h5awritec_4_c)
#   define nh5awritec_5_c              FNAME(h5awritec_5_c)
#   define nh5awritec_6_c              FNAME(h5awritec_6_c)
#   define nh5awritec_7_c              FNAME(h5awritec_7_c)
#   define nh5areadc_c       FNAME(h5areadc_c)
#   define nh5areadc_s_c               FNAME(h5areadc_s_c)
#   define nh5areadc_1_c               FNAME(h5areadc_1_c)
#   define nh5areadc_2_c               FNAME(h5areadc_2_c)
#   define nh5areadc_3_c               FNAME(h5areadc_3_c)
#   define nh5areadc_4_c               FNAME(h5areadc_4_c)
#   define nh5areadc_5_c               FNAME(h5areadc_5_c)
#   define nh5areadc_6_c               FNAME(h5areadc_6_c)
#   define nh5areadc_7_c               FNAME(h5areadc_7_c)
#   define nh5aread_c        FNAME(h5aread_c)
#   define nh5aread_integer_s_c        FNAME(h5aread_integer_s_c)
#   define nh5aread_integer_1_c        FNAME(h5aread_integer_1_c)
#   define nh5aread_integer_2_c        FNAME(h5aread_integer_2_c)
#   define nh5aread_integer_3_c        FNAME(h5aread_integer_3_c)
#   define nh5aread_integer_4_c        FNAME(h5aread_integer_4_c)
#   define nh5aread_integer_5_c        FNAME(h5aread_integer_5_c)
#   define nh5aread_integer_6_c        FNAME(h5aread_integer_6_c)
#   define nh5aread_integer_7_c        FNAME(h5aread_integer_7_c)
#   define nh5aread_real_s_c           FNAME(h5aread_real_s_c)
#   define nh5aread_real_1_c           FNAME(h5aread_real_1_c)
#   define nh5aread_real_2_c           FNAME(h5aread_real_2_c)
#   define nh5aread_real_3_c           FNAME(h5aread_real_3_c)
#   define nh5aread_real_4_c           FNAME(h5aread_real_4_c)
#   define nh5aread_real_5_c           FNAME(h5aread_real_5_c)
#   define nh5aread_real_6_c           FNAME(h5aread_real_6_c)
#   define nh5aread_real_7_c           FNAME(h5aread_real_7_c)
#   define nh5aread_double_s_c         FNAME(h5aread_double_s_c)
#   define nh5aread_double_1_c         FNAME(h5aread_double_1_c)
#   define nh5aread_double_2_c         FNAME(h5aread_double_2_c)
#   define nh5aread_double_3_c         FNAME(h5aread_double_3_c)
#   define nh5aread_double_4_c         FNAME(h5aread_double_4_c)
#   define nh5aread_double_5_c         FNAME(h5aread_double_5_c)
#   define nh5aread_double_6_c         FNAME(h5aread_double_6_c)
#   define nh5aread_double_7_c         FNAME(h5aread_double_7_c)
#   define nh5aget_name_c    FNAME(h5aget_name_c)
#   define nh5aopen_idx_c    FNAME(h5aopen_idx_c)
#   define nh5aget_space_c   FNAME(h5aget_space_c)
#   define nh5aget_type_c    FNAME(h5aget_type_c)
#   define nh5aget_num_attrs_c FNAME(h5aget_num_attrs_c)
#   define nh5adelete_c      FNAME(h5adelete_c)
#endif                                             /* DF_CAPFNAMES */
#endif

H5_FCDLL int_f nh5acreate_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *type_id, hid_t_f *space_id, hid_t_f *crt_prp,  hid_t_f *attr_id);
H5_FCDLL int_f nh5aopen_name_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *attr_id);
H5_FCDLL int_f nh5awritec_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awrite_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awritec_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awritec_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awritec_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awritec_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awritec_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awritec_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awritec_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awritec_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5awrite_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_integer_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_integer_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_integer_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_integer_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_integer_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_integer_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_integer_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_integer_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_real_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_real_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_real_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_real_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_real_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_real_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_real_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_real_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_double_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_double_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_double_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_double_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_double_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_double_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_double_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5awrite_double_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5areadc_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5aread_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5areadc_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5areadc_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5areadc_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5areadc_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5areadc_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5areadc_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5areadc_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5areadc_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims);
H5_FCDLL int_f nh5aread_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_integer_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_integer_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_integer_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_integer_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_integer_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_integer_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_integer_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_integer_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_real_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_real_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_real_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_real_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_real_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_real_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_real_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_real_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_double_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_double_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_double_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_double_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_double_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_double_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_double_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aread_double_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void *dims);
H5_FCDLL int_f nh5aclose_c ( hid_t_f *attr_id );
H5_FCDLL int_f nh5adelete_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen);
H5_FCDLL int_f nh5aopen_idx_c (hid_t_f *obj_id, int_f *idx, hid_t_f *attr_id);
H5_FCDLL int_f nh5aget_space_c (hid_t_f *attr_id, hid_t_f *space_id);
H5_FCDLL int_f nh5aget_type_c (hid_t_f *attr_id, hid_t_f *type_id);
H5_FCDLL int_f nh5aget_num_attrs_c (hid_t_f *obj_id, int_f *attr_num);
H5_FCDLL int_f nh5aget_name_c(hid_t_f *attr_id, size_t_f *size, _fcd buf);

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
#   define nh5tget_member_index_c        FNAME(H5TGET_MEMBER_INDEX_C)
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
#   define nh5tvlen_create_c               FNAME(H5TVLEN_CREATE_C)
#   define nh5tis_variable_str_c         FNAME(H5TIS_VARIABLE_STR_C)
#   define nh5tget_member_class_c         FNAME(H5TGET_MEMBER_CLASS_C)
#   define nh5tget_native_type_c         FNAME(H5TGET_NATIVE_TYPE_C)
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
#   define nh5tget_member_index_c        FNAME(h5tget_member_index_c)
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
#   define nh5tvlen_create_c               FNAME(h5tvlen_create_c)
#   define nh5tis_variable_str_c         FNAME(h5tis_variable_str_c)
#   define nh5tget_member_class_c         FNAME(h5tget_member_class_c)
#   define nh5tget_native_type_c         FNAME(h5tget_native_type_c)
#endif
#endif

H5_FCDLL int_f nh5tcreate_c(int_f *class, size_t_f *size, hid_t_f *type_id);
H5_FCDLL int_f nh5topen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id);
H5_FCDLL int_f nh5tcommit_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id);
H5_FCDLL int_f nh5tclose_c ( hid_t_f *type_id );
H5_FCDLL int_f nh5tequal_c ( hid_t_f *type1_id , hid_t_f *type2_id, int_f *c_flag);
H5_FCDLL int_f nh5tcopy_c ( hid_t_f *type_id , hid_t_f *new_type_id);
H5_FCDLL int_f nh5tget_class_c ( hid_t_f *type_id , int_f *classtype);
H5_FCDLL int_f nh5tget_order_c ( hid_t_f *type_id , int_f *order);
H5_FCDLL int_f nh5tset_order_c ( hid_t_f *type_id , int_f *order);
H5_FCDLL int_f nh5tget_size_c ( hid_t_f *type_id , size_t_f *size);
H5_FCDLL int_f nh5tset_size_c ( hid_t_f *type_id , size_t_f *size);
H5_FCDLL int_f nh5tcommitted_c (hid_t_f *type_id);
H5_FCDLL int_f nh5tget_precision_c ( hid_t_f *type_id , size_t_f *precision);
H5_FCDLL int_f nh5tset_precision_c ( hid_t_f *type_id , size_t_f *precision);
H5_FCDLL int_f nh5tget_offset_c ( hid_t_f *type_id , size_t_f *offset);
H5_FCDLL int_f nh5tset_offset_c ( hid_t_f *type_id , size_t_f *offset);
H5_FCDLL int_f nh5tget_pad_c ( hid_t_f *type_id , int_f * lsbpad, int_f * msbpad);
H5_FCDLL int_f nh5tset_pad_c ( hid_t_f *type_id, int_f * lsbpad, int_f * msbpad );
H5_FCDLL int_f nh5tget_sign_c ( hid_t_f *type_id , int_f* sign);
H5_FCDLL int_f nh5tset_sign_c ( hid_t_f *type_id , int_f *sign);
H5_FCDLL int_f nh5tget_fields_c ( hid_t_f *type_id, size_t_f *spos, size_t_f *epos, size_t_f* esize, size_t_f* mpos, size_t_f* msize);
H5_FCDLL int_f nh5tset_fields_c ( hid_t_f *type_id, size_t_f *spos, size_t_f *epos, size_t_f* esize, size_t_f* mpos, size_t_f* msize);
H5_FCDLL int_f nh5tget_ebias_c ( hid_t_f *type_id , size_t_f *ebias);
H5_FCDLL int_f nh5tset_ebias_c ( hid_t_f *type_id , size_t_f *ebias);
H5_FCDLL int_f nh5tget_norm_c ( hid_t_f *type_id , int_f *norm);
H5_FCDLL int_f nh5tset_norm_c ( hid_t_f *type_id , int_f *norm);
H5_FCDLL int_f nh5tget_inpad_c ( hid_t_f *type_id, int_f * padtype);
H5_FCDLL int_f nh5tset_inpad_c ( hid_t_f *type_id, int_f * padtype);
H5_FCDLL int_f nh5tget_cset_c ( hid_t_f *type_id, int_f * cset);
H5_FCDLL int_f nh5tset_cset_c ( hid_t_f *type_id, int_f * cset);
H5_FCDLL int_f nh5tget_strpad_c ( hid_t_f *type_id, int_f * strpad);
H5_FCDLL int_f nh5tset_strpad_c ( hid_t_f *type_id, int_f * strpad);
H5_FCDLL int_f nh5tget_nmembers_c ( hid_t_f *type_id , int_f * num_members);
H5_FCDLL int_f nh5tget_member_name_c ( hid_t_f *type_id ,int_f* idx, _fcd member_name, int_f *namelen);
H5_FCDLL int_f nh5tget_member_dims_c ( hid_t_f *type_id ,int_f* field_idx, int_f * dims, size_t_f * field_dims, int_f * perm );
H5_FCDLL int_f nh5tget_member_offset_c ( hid_t_f *type_id ,int_f* member_no, size_t_f* offset);
H5_FCDLL int_f nh5tget_member_type_c ( hid_t_f *type_id ,int_f* field_idx, hid_t_f * datatype);
H5_FCDLL int_f nh5tget_member_index_c ( hid_t_f *type_id ,_fcd name, int_f* namelen, int_f *idx);
H5_FCDLL int_f nh5tinsert_c(hid_t_f *type_id, _fcd name, int_f* namelen, size_t_f *offset, hid_t_f * field_id);
H5_FCDLL int_f nh5tpack_c(hid_t_f * type_id);
H5_FCDLL int_f nh5tinsert_array_c(hid_t_f * parent_id, _fcd name, int_f* namelen, size_t_f* offset, int_f* ndims, size_t_f* dims, hid_t_f* member_id, int_f* perm );
H5_FCDLL int_f nh5tinsert_array_c2(hid_t_f * parent_id, _fcd name, int_f* namelen, size_t_f* offset, int_f* ndims, size_t_f* dims, hid_t_f* member_id);
H5_FCDLL int_f nh5tenum_create_c ( hid_t_f *parent_id , hid_t_f *new_type_id);
H5_FCDLL int_f nh5tenum_insert_c(hid_t_f *type_id, _fcd name, int_f* namelen, int_f* value);
H5_FCDLL int_f nh5tenum_nameof_c(hid_t_f *type_id, int_f* value, _fcd name, size_t_f* namelen);
H5_FCDLL int_f nh5tenum_valueof_c(hid_t_f *type_id, _fcd name, int_f* namelen, int_f* value);
H5_FCDLL int_f nh5tget_member_value_c(hid_t_f *type_id, int_f* member_no, int_f* value);
H5_FCDLL int_f nh5tset_tag_c(hid_t_f* type_id, _fcd tag, int_f* namelen);
H5_FCDLL int_f nh5tget_tag_c(hid_t_f* type_id, _fcd tag, int_f* namelen);
H5_FCDLL int_f nh5tarray_create_c(hid_t_f * base_id, int_f *rank, hsize_t_f* dims, hid_t_f* type_id);
H5_FCDLL int_f nh5tget_array_dims_c ( hid_t_f *type_id , hsize_t_f * dims);
H5_FCDLL int_f nh5tget_array_ndims_c ( hid_t_f *type_id , int_f * ndims);
H5_FCDLL int_f nh5tget_super_c ( hid_t_f *type_id , hid_t_f *base_type_id);
H5_FCDLL int_f nh5tvlen_create_c ( hid_t_f *type_id , hid_t_f *vltype_id);
H5_FCDLL int_f nh5tis_variable_str_c ( hid_t_f *type_id , int_f *flag );
H5_FCDLL int_f nh5tget_member_class_c ( hid_t_f *type_id ,  int_f *member_no, int_f *class );
H5_FCDLL int_f nh5tget_native_type_c( hid_t_f *dtype_id, int_f *direction, hid_t_f *native_dtype_id);

/*
 * Functions from H5Pf.c
 */
#ifndef H5Pf90_FNAMES
#    define H5Pf90_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5pcreate_c       FNAME(H5PCREATE_C)
#   define nh5pclose_c        FNAME(H5PCLOSE_C)
#   define nh5pcopy_c         FNAME(H5PCOPY_C)
#   define nh5pequal_c         FNAME(H5PEQUAL_C)
#   define nh5pget_class_c    FNAME(H5PGET_CLASS_C)
#   define nh5pset_deflate_c  FNAME(H5PSET_DEFLATE_C)
#   define nh5pset_preserve_c  FNAME(H5PSET_PRESERVE_C)
#   define nh5pget_preserve_c  FNAME(H5PGET_PRESERVE_C)
#   define nh5pset_chunk_c    FNAME(H5PSET_CHUNK_C)
#   define nh5pget_chunk_c    FNAME(H5PGET_CHUNK_C)
#   define nh5pset_fill_valuec_c FNAME(H5PSET_FILL_VALUEC_C)
#   define nh5pset_fill_value_c  FNAME(H5PSET_FILL_VALUE_C)
#   define nh5pset_fill_value_integer_c FNAME(H5PSET_FILL_VALUE_INTEGER_C)
#   define nh5pset_fill_value_real_c    FNAME(H5PSET_FILL_VALUE_REAL_C)
#   define nh5pset_fill_value_double_c  FNAME(H5PSET_FILL_VALUE_DOUBLE_C)
#   define nh5pget_fill_valuec_c FNAME(H5PGET_FILL_VALUEC_C)
#   define nh5pget_fill_value_c  FNAME(H5PGET_FILL_VALUE_C)
#   define nh5pget_fill_value_integer_c FNAME(H5PGET_FILL_VALUE_INTEGER_C)
#   define nh5pget_fill_value_real_c    FNAME(H5PGET_FILL_VALUE_REAL_C)
#   define nh5pget_fill_value_double_c  FNAME(H5PGET_FILL_VALUE_DOUBLE_C)
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
#   define nh5premove_filter_c         FNAME(H5PREMOVE_FILTER_C)
#   define nh5pmodify_filter_c         FNAME(H5PMODIFY_FILTER_C)
#   define nh5pget_nfilters_c         FNAME(H5PGET_NFILTERS_C)
#   define nh5pget_filter_c         FNAME(H5PGET_FILTER_C)
#   define nh5pget_filter_by_id_c         FNAME(H5PGET_FILTER_BY_ID_C)
#   define nh5pset_external_c         FNAME(H5PSET_EXTERNAL_C)
#   define nh5pget_external_count_c         FNAME(H5PGET_EXTERNAL_COUNT_C)
#   define nh5pget_external_c         FNAME(H5PGET_EXTERNAL_C)
#   define nh5pset_hyper_cache_c         FNAME(H5PSET_HYPER_CACHE_C)
#   define nh5pget_hyper_cache_c         FNAME(H5PGET_HYPER_CACHE_C)
#   define nh5pget_btree_ratios_c         FNAME(H5PGET_BTREE_RATIOS_C)
#   define nh5pset_btree_ratios_c         FNAME(H5PSET_BTREE_RATIOS_C)
#   define nh5pset_fapl_mpio_c         FNAME(H5PSET_FAPL_MPIO_C)
#   define nh5pget_fapl_mpio_c         FNAME(H5PGET_FAPL_MPIO_C)
#   define nh5pset_fapl_mpiposix_c     FNAME(H5PSET_FAPL_MPIPOSIX_C)
#   define nh5pget_fapl_mpiposix_c     FNAME(H5PGET_FAPL_MPIPOSIX_C)
#   define nh5pset_dxpl_mpio_c        FNAME(H5PSET_DXPL_MPIO_C)
#   define nh5pget_dxpl_mpio_c        FNAME(H5PGET_DXPL_MPIO_C)
#   define nh5pget_fclose_degree_c    FNAME(H5PGET_FCLOSE_DEGREE_C)
#   define nh5pset_fclose_degree_c    FNAME(H5PSET_FCLOSE_DEGREE_C)
#   define nh5pset_buffer_c    FNAME(H5PSET_BUFFER_C)
#   define nh5pget_buffer_c    FNAME(H5PGET_BUFFER_C)
#   define nh5pfill_value_defined_c    FNAME(H5PFILL_VALUE_DEFINED_C)
#   define nh5pset_alloc_time_c    FNAME(H5PSET_ALLOC_TIME_C)
#   define nh5pget_alloc_time_c    FNAME(H5PGET_ALLOC_TIME_C)
#   define nh5pset_fill_time_c    FNAME(H5PSET_FILL_TIME_C)
#   define nh5pget_fill_time_c    FNAME(H5PGET_FILL_TIME_C)
#   define nh5pset_meta_block_size_c    FNAME(H5PSET_META_BLOCK_SIZE_C)
#   define nh5pget_meta_block_size_c    FNAME(H5PGET_META_BLOCK_SIZE_C)
#   define nh5pset_sieve_buf_size_c    FNAME(H5PSET_SIEVE_BUF_SIZE_C)
#   define nh5pget_sieve_buf_size_c    FNAME(H5PGET_SIEVE_BUF_SIZE_C)
#   define nh5pset_hyper_vector_size_c    FNAME(H5PSET_HYPER_VECTOR_SIZE_C)
#   define nh5pget_hyper_vector_size_c    FNAME(H5PGET_HYPER_VECTOR_SIZE_C)
#   define nh5pset_small_data_block_size_c    FNAME(H5PSET_SMALL_DATA_BLOCK_SIZE_C)
#   define nh5pget_small_data_block_size_c    FNAME(H5PGET_SMALL_DATA_BLOCK_SIZE_C)
#   define nh5pcreate_class_c             FNAME(H5PCREATE_CLASS_C)
#   define nh5pregister_c                 FNAME(H5PREGISTER_C)
#   define nh5pregister_integer_c              FNAME(H5PREGISTER_INTEGER_C)
#   define nh5pregister_real_c                 FNAME(H5PREGISTER_REAL_C)
#   define nh5pregister_double_c               FNAME(H5PREGISTER_DOUBLE_C)
#   define nh5pregisterc_c                FNAME(H5PREGISTERC_C)
#   define nh5pinsert_c                   FNAME(H5PINSERT_C)
#   define nh5pinsert_integer_c                FNAME(H5PINSERT_INTEGER_C)
#   define nh5pinsert_real_c                   FNAME(H5PINSERT_REAL_C)
#   define nh5pinsert_double_c                 FNAME(H5PINSERT_DOUBLE_C)
#   define nh5pinsertc_c                  FNAME(H5PINSERTC_C)
#   define nh5pset_c                      FNAME(H5PSET_C)
#   define nh5pset_integer_c                   FNAME(H5PSET_INTEGER_C)
#   define nh5pset_real_c                      FNAME(H5PSET_REAL_C)
#   define nh5pset_double_c                    FNAME(H5PSET_DOUBLE_C)
#   define nh5psetc_c                     FNAME(H5PSETC_C)
#   define nh5pget_c                      FNAME(H5PGET_C)
#   define nh5pget_integer_c                   FNAME(H5PGET_INTEGER_C)
#   define nh5pget_real_c                      FNAME(H5PGET_REAL_C)
#   define nh5pget_double_c                    FNAME(H5PGET_DOUBLE_C)
#   define nh5pgetc_c                     FNAME(H5PGETC_C)
#   define nh5pexist_c                    FNAME(H5PEXIST_C)
#   define nh5pget_size_c                 FNAME(H5PGET_SIZE_C)
#   define nh5pget_nprops_c               FNAME(H5PGET_NPROPS_C)
#   define nh5pget_class_parent_c         FNAME(H5PGET_CLASS_PARENT_C)
#   define nh5pisa_class_c                FNAME(H5PISA_CLASS_C)
#   define nh5pcopy_prop_c                FNAME(H5PCOPY_PROP_C)
#   define nh5premove_c                   FNAME(H5PREMOVE_C)
#   define nh5punregister_c               FNAME(H5PUNREGISTER_C)
#   define nh5pclose_class_c              FNAME(H5PCLOSE_CLASS_C)
#   define nh5pget_class_name_c           FNAME(H5PGET_CLASS_NAME_C)
#   define nh5pset_shuffle_c               FNAME(H5PSET_SHUFFLE_C)
#   define nh5pset_fletcher32_c           FNAME(H5PSET_FLETCHER32_C)
#   define nh5pset_edc_check_c            FNAME(H5PSET_EDC_CHECK_C)
#   define nh5pget_edc_check_c            FNAME(H5PGET_EDC_CHECK_C)
#   define nh5pset_family_offset_c       FNAME(H5PSET_FAMILY_OFFSET_C)
#   define nh5pget_fapl_multi_c          FNAME(H5PGET_FAPL_MULTI_C)
#   define nh5pset_fapl_multi_c          FNAME(H5PSET_FAPL_MULTI_C)
#   define nh5pset_fapl_multi_sc          FNAME(H5PSET_FAPL_MULTI_SC)
#   define nh5pset_szip_c                 FNAME(H5PSET_SZIP_C)
#   define nh5pall_filters_avail_c        FNAME(H5PALL_FILTERS_AVAIL_C)

#else
#   define nh5pcreate_c       FNAME(h5pcreate_c)
#   define nh5pclose_c        FNAME(h5pclose_c)
#   define nh5pcopy_c         FNAME(h5pcopy_c)
#   define nh5pequal_c         FNAME(h5pequal_c)
#   define nh5pget_class_c    FNAME(h5pget_class_c)
#   define nh5pset_deflate_c  FNAME(h5pset_deflate_c)
#   define nh5pset_preserve_c  FNAME(h5pset_preserve_c)
#   define nh5pget_preserve_c  FNAME(h5pget_preserve_c)
#   define nh5pset_chunk_c    FNAME(h5pset_chunk_c)
#   define nh5pget_chunk_c    FNAME(h5pget_chunk_c)
#   define nh5pset_fill_valuec_c FNAME(h5pset_fill_valuec_c)
#   define nh5pset_fill_value_c  FNAME(h5pset_fill_value_c)
#   define nh5pset_fill_value_integer_c FNAME(h5pset_fill_value_integer_c)
#   define nh5pset_fill_value_real_c    FNAME(h5pset_fill_value_real_c)
#   define nh5pset_fill_value_double_c  FNAME(h5pset_fill_value_double_c)
#   define nh5pget_fill_valuec_c FNAME(h5pget_fill_valuec_c)
#   define nh5pget_fill_value_c  FNAME(h5pget_fill_value_c)
#   define nh5pget_fill_value_integer_c FNAME(h5pget_fill_value_integer_c)
#   define nh5pget_fill_value_real_c    FNAME(h5pget_fill_value_real_c)
#   define nh5pget_fill_value_double_c  FNAME(h5pget_fill_value_double_c)
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
#   define nh5premove_filter_c         FNAME(h5premove_filter_c)
#   define nh5pmodify_filter_c         FNAME(h5pmodify_filter_c)
#   define nh5pget_nfilters_c         FNAME(h5pget_nfilters_c)
#   define nh5pget_filter_c         FNAME(h5pget_filter_c)
#   define nh5pget_filter_by_id_c         FNAME(h5pget_filter_by_id_c)
#   define nh5pset_external_c         FNAME(h5pset_external_c)
#   define nh5pget_external_count_c         FNAME(h5pget_external_count_c)
#   define nh5pget_external_c         FNAME(h5pget_external_c)
#   define nh5pset_hyper_cache_c         FNAME(h5pset_hyper_cache_c)
#   define nh5pget_hyper_cache_c         FNAME(h5pget_hyper_cache_c)
#   define nh5pget_btree_ratios_c         FNAME(h5pget_btree_ratios_c)
#   define nh5pset_btree_ratios_c         FNAME(h5pset_btree_ratios_c)
#   define nh5pset_fapl_mpio_c         FNAME(h5pset_fapl_mpio_c)
#   define nh5pget_fapl_mpio_c         FNAME(h5pget_fapl_mpio_c)
#   define nh5pset_fapl_mpiposix_c     FNAME(h5pset_fapl_mpiposix_c)
#   define nh5pget_fapl_mpiposix_c     FNAME(h5pget_fapl_mpiposix_c)
#   define nh5pset_dxpl_mpio_c        FNAME(h5pset_dxpl_mpio_c)
#   define nh5pget_dxpl_mpio_c        FNAME(h5pget_dxpl_mpio_c)
#   define nh5pget_fclose_degree_c    FNAME(h5pget_fclose_degree_c)
#   define nh5pset_fclose_degree_c    FNAME(h5pset_fclose_degree_c)
#   define nh5pset_buffer_c    FNAME(h5pset_buffer_c)
#   define nh5pget_buffer_c    FNAME(h5pget_buffer_c)
#   define nh5pfill_value_defined_c    FNAME(h5pfill_value_defined_c)
#   define nh5pset_alloc_time_c    FNAME(h5pset_alloc_time_c)
#   define nh5pget_alloc_time_c    FNAME(h5pget_alloc_time_c)
#   define nh5pset_fill_time_c    FNAME(h5pset_fill_time_c)
#   define nh5pget_fill_time_c    FNAME(h5pget_fill_time_c)
#   define nh5pset_meta_block_size_c    FNAME(h5pset_meta_block_size_c)
#   define nh5pget_meta_block_size_c    FNAME(h5pget_meta_block_size_c)
#   define nh5pset_sieve_buf_size_c    FNAME(h5pset_sieve_buf_size_c)
#   define nh5pget_sieve_buf_size_c    FNAME(h5pget_sieve_buf_size_c)
#   define nh5pset_hyper_vector_size_c    FNAME(h5pset_hyper_vector_size_c)
#   define nh5pget_hyper_vector_size_c    FNAME(h5pget_hyper_vector_size_c)
#   define nh5pset_small_data_block_size_c    FNAME(h5pset_small_data_block_size_c)
#   define nh5pget_small_data_block_size_c    FNAME(h5pget_small_data_block_size_c)
#   define nh5pcreate_class_c             FNAME(h5pcreate_class_c)
#   define nh5pregister_c                 FNAME(h5pregister_c)
#   define nh5pregister_integer_c              FNAME(h5pregister_integer_c)
#   define nh5pregister_real_c                 FNAME(h5pregister_real_c)
#   define nh5pregister_double_c               FNAME(h5pregister_double_c)
#   define nh5pregisterc_c                FNAME(h5pregisterc_c)
#   define nh5pinsert_c                   FNAME(h5pinsert_c)
#   define nh5pinsert_integer_c                FNAME(h5pinsert_integer_c)
#   define nh5pinsert_real_c                   FNAME(h5pinsert_real_c)
#   define nh5pinsert_double_c                 FNAME(h5pinsert_double_c)
#   define nh5pinsertc_c                  FNAME(h5pinsertc_c)
#   define nh5pset_c                      FNAME(h5pset_c)
#   define nh5pset_integer_c                   FNAME(h5pset_integer_c)
#   define nh5pset_real_c                      FNAME(h5pset_real_c)
#   define nh5pset_double_c                    FNAME(h5pset_double_c)
#   define nh5psetc_c                     FNAME(h5psetc_c)
#   define nh5pget_c                      FNAME(h5pget_c)
#   define nh5pget_integer_c                   FNAME(h5pget_integer_c)
#   define nh5pget_real_c                      FNAME(h5pget_real_c)
#   define nh5pget_double_c                    FNAME(h5pget_double_c)
#   define nh5pgetc_c                     FNAME(h5pgetc_c)
#   define nh5pexist_c                    FNAME(h5pexist_c)
#   define nh5pget_size_c                 FNAME(h5pget_size_c)
#   define nh5pget_nprops_c               FNAME(h5pget_nprops_c)
#   define nh5pget_class_parent_c         FNAME(h5pget_class_parent_c)
#   define nh5pisa_class_c                FNAME(h5pisa_class_c)
#   define nh5pcopy_prop_c                FNAME(h5pcopy_prop_c)
#   define nh5premove_c                   FNAME(h5premove_c)
#   define nh5punregister_c               FNAME(h5punregister_c)
#   define nh5pclose_class_c              FNAME(h5pclose_class_c)
#   define nh5pget_class_name_c           FNAME(h5pget_class_name_c)
#   define nh5pset_shuffle_c               FNAME(h5pset_shuffle_c)
#   define nh5pset_fletcher32_c           FNAME(h5pset_fletcher32_c)
#   define nh5pset_edc_check_c            FNAME(h5pset_edc_check_c)
#   define nh5pget_edc_check_c            FNAME(h5pget_edc_check_c)
#   define nh5pset_family_offset_c       FNAME(h5pset_family_offset_c)
#   define nh5pget_fapl_multi_c          FNAME(h5pget_fapl_multi_c)
#   define nh5pset_fapl_multi_c          FNAME(h5pset_fapl_multi_c)
#   define nh5pset_fapl_multi_sc          FNAME(h5pset_fapl_multi_sc)
#   define nh5pset_szip_c                 FNAME(h5pset_szip_c)
#   define nh5pall_filters_avail_c        FNAME(h5pall_filters_avail_c)
#endif
#endif

H5_FCDLL int_f nh5pcreate_c ( hid_t_f *class, hid_t_f *prp_id );
H5_FCDLL int_f nh5pclose_c ( hid_t_f *prp_id );
H5_FCDLL int_f nh5pcopy_c ( hid_t_f *prp_id , hid_t_f *new_prp_id);
H5_FCDLL int_f nh5pequal_c ( hid_t_f *plist1_id , hid_t_f *plist2_id, int_f *c_flag);
H5_FCDLL int_f nh5pget_class_c ( hid_t_f *prp_id , int_f *classtype);
H5_FCDLL int_f nh5pset_deflate_c ( hid_t_f *prp_id , int_f *level);
H5_FCDLL int_f nh5pset_chunk_c ( hid_t_f *prp_id, int_f *rank, hsize_t_f *dims );
H5_FCDLL int_f nh5pget_chunk_c ( hid_t_f *prp_id, int_f *max_rank, hsize_t_f *dims );
H5_FCDLL int_f nh5pset_fill_valuec_c (hid_t_f *prp_id, hid_t_f *type_id, _fcd fillvalue);
H5_FCDLL int_f nh5pset_fill_value_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f nh5pset_fill_value_integer_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f nh5pset_fill_value_real_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f nh5pset_fill_value_double_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f nh5pget_fill_valuec_c (hid_t_f *prp_id, hid_t_f *type_id, _fcd fillvalue);
H5_FCDLL int_f nh5pget_fill_value_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f nh5pget_fill_value_integer_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f nh5pget_fill_value_real_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f nh5pget_fill_value_double_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f nh5pset_preserve_c ( hid_t_f *prp_id , int_f *flag);
H5_FCDLL int_f nh5pget_preserve_c ( hid_t_f *prp_id , int_f *flag);
H5_FCDLL int_f nh5pget_version_c (hid_t_f *prp_id, int_f * boot,int_f * freelist, int_f * stab, int_f *shhdr);
H5_FCDLL int_f nh5pset_userblock_c (hid_t_f *prp_id, hsize_t_f * size);
H5_FCDLL int_f nh5pget_userblock_c (hid_t_f *prp_id, hsize_t_f * size);
H5_FCDLL int_f nh5pget_sizes_c (hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size);
H5_FCDLL int_f nh5pset_sizes_c (hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size);
H5_FCDLL int_f nh5pset_sym_k_c (hid_t_f *prp_id, int_f* ik, int_f* lk);
H5_FCDLL int_f nh5pget_sym_k_c (hid_t_f *prp_id, int_f* ik, int_f* lk);
H5_FCDLL int_f nh5pset_istore_k_c (hid_t_f *prp_id, int_f* ik);
H5_FCDLL int_f nh5pget_istore_k_c (hid_t_f *prp_id, int_f* ik);
H5_FCDLL int_f nh5pget_driver_c (hid_t_f *prp_id, hid_t_f*driver);
H5_FCDLL int_f nh5pset_fapl_stdio_c (hid_t_f *prp_id);
H5_FCDLL int_f nh5pget_fapl_stdio_c (hid_t_f *prp_id, int_f* io);
H5_FCDLL int_f nh5pset_fapl_sec2_c (hid_t_f *prp_id);
H5_FCDLL int_f nh5pget_fapl_sec2_c (hid_t_f *prp_id, int_f* sec2);
H5_FCDLL int_f nh5pset_alignment_c(hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment);
H5_FCDLL int_f nh5pget_alignment_c(hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment);
H5_FCDLL int_f nh5pget_fapl_core_c (hid_t_f *prp_id, size_t_f* increment, int_f *flag);
H5_FCDLL int_f nh5pset_fapl_core_c (hid_t_f *prp_id, size_t_f* increment, int_f *flag);
H5_FCDLL int_f nh5pset_fapl_family_c (hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist );
H5_FCDLL int_f nh5pget_fapl_family_c (hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist );
H5_FCDLL int_f nh5pset_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, size_t_f* rdcc_nelmts, size_t_f* rdcc_nbytes, real_f* rdcc_w0);
H5_FCDLL int_f nh5pget_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, size_t_f* rdcc_nelmts, size_t_f* rdcc_nbytes, real_f* rdcc_w0);
H5_FCDLL int_f nh5pget_fapl_split_c(hid_t_f *prp_id, size_t_f* meta_ext_size , _fcd meta_ext, hid_t_f* meta_plist, size_t_f* raw_ext_size, _fcd raw_ext, hid_t_f * raw_plist);
H5_FCDLL int_f nh5pset_fapl_split_c(hid_t_f *prp_id, int_f* meta_len, _fcd meta_ext, hid_t_f* meta_plist, int_f* raw_len, _fcd raw_ext, hid_t_f * raw_plist);
H5_FCDLL int_f nh5pset_gc_references_c(hid_t_f *prp_id, int_f* gc_references);
H5_FCDLL int_f nh5pget_gc_references_c(hid_t_f *prp_id, int_f* gc_references);
H5_FCDLL int_f nh5pset_layout_c (hid_t_f *prp_id, int_f* layout);
H5_FCDLL int_f nh5pget_layout_c (hid_t_f *prp_id, int_f* layout);
H5_FCDLL int_f nh5pset_filter_c (hid_t_f *prp_id, int_f* filter, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values );
H5_FCDLL int_f nh5premove_filter_c (hid_t_f *prp_id, int_f* filter);
H5_FCDLL int_f nh5pmodify_filter_c (hid_t_f *prp_id, int_f* filter, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values );
H5_FCDLL int_f nh5pget_nfilters_c (hid_t_f *prp_id, int_f* nfilters);
H5_FCDLL int_f nh5pget_filter_c(hid_t_f *prp_id, int_f* filter_number, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values, size_t_f *namelen, _fcd name, int_f* filter_id);
H5_FCDLL int_f nh5pget_filter_by_id_c(hid_t_f *prp_id, int_f* filter_id, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values, size_t_f *namelen, _fcd name);
H5_FCDLL int_f nh5pset_external_c (hid_t_f *prp_id, _fcd name, int_f* namelen, int_f* offset, hsize_t_f*bytes);
H5_FCDLL int_f nh5pget_external_count_c (hid_t_f *prp_id, int_f* count);
H5_FCDLL int_f nh5pget_external_c(hid_t_f *prp_id, int_f *idx, size_t_f* name_size, _fcd name, int_f* offset, hsize_t_f*bytes);
H5_FCDLL int_f nh5pset_hyper_cache_c(hid_t_f *prp_id, int_f* cache, int_f* limit);
H5_FCDLL int_f nh5pget_hyper_cache_c(hid_t_f *prp_id, int_f* cache, int_f* limit);
H5_FCDLL int_f nh5pget_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right);
H5_FCDLL int_f nh5pset_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right);
H5_FCDLL int_f nh5pget_fapl_mpio_c(hid_t_f *prp_id, int_f* comm, int_f* info);
H5_FCDLL int_f nh5pset_fapl_mpio_c(hid_t_f *prp_id, int_f* comm, int_f* info);
H5_FCDLL int_f nh5pget_fapl_mpiposix_c(hid_t_f *prp_id, int_f* comm, int_f* flag);
H5_FCDLL int_f nh5pset_fapl_mpiposix_c(hid_t_f *prp_id, int_f* comm, int_f* flag);
H5_FCDLL int_f nh5pget_dxpl_mpio_rc(hid_t_f *prp_id, int_f* data_xfer_mode);
H5_FCDLL int_f nh5pset_dxpl_mpio_c(hid_t_f *prp_id, int_f* data_xfer_mode);
H5_FCDLL int_f nh5pset_fclose_degree_c(hid_t_f *fapl, int_f *degree);
H5_FCDLL int_f nh5pget_fclose_degree_c(hid_t_f *fapl, int_f *degree);
H5_FCDLL int_f nh5pget_buffer_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f nh5pset_buffer_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f nh5pfill_value_define_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f nh5pset_alloc_time_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f nh5pget_alloc_time_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f nh5pset_fill_time_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f nh5pget_fill_time_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f nh5pset_meta_block_size_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f nh5pget_meta_block_size_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f nh5pset_sieve_buf_size_c(hid_t_f *plist, size_t_f *size);
H5_FCDLL int_f nh5pget_sieve_buf_size_c(hid_t_f *plist, size_t_f *size);
H5_FCDLL int_f nh5pset_small_data_block_size_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f nh5pget_small_data_block_size_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f nh5pset_hyper_vector_size_c(hid_t_f *plist, size_t_f *size);
H5_FCDLL int_f nh5pget_hyper_vector_size_c(hid_t_f *plist, size_t_f *size);
H5_FCDLL int_f nh5pcreate_class_c(hid_t_f *parent, _fcd name, int_f *name_len, hid_t_f *class);
H5_FCDLL int_f nh5pregister_c(hid_t_f *class, _fcd name, int_f * name_len, size_t_f *size, void *value);
H5_FCDLL int_f nh5pregister_integer_c(hid_t_f *class, _fcd name, int_f * name_len, size_t_f *size, void *value);
H5_FCDLL int_f nh5pregister_real_c(hid_t_f *class, _fcd name, int_f * name_len, size_t_f *size, void *value);
H5_FCDLL int_f nh5pregister_double_c(hid_t_f *class, _fcd name, int_f * name_len, size_t_f *size, void *value);
H5_FCDLL int_f nh5pregisterc_c(hid_t_f *class, _fcd name, int_f * name_len, size_t_f *size, _fcd value, int_f *value_len);
H5_FCDLL int_f nh5pinsert_c(hid_t_f  *plist, _fcd name, int_f *name_len, size_t_f *size, void *value);
H5_FCDLL int_f nh5pinsert_integer_c(hid_t_f  *plist, _fcd name, int_f *name_len, size_t_f *size, void *value);
H5_FCDLL int_f nh5pinsert_real_c(hid_t_f  *plist, _fcd name, int_f *name_len, size_t_f *size, void *value);
H5_FCDLL int_f nh5pinsert_double_c(hid_t_f  *plist, _fcd name, int_f *name_len, size_t_f *size, void *value);
H5_FCDLL int_f nh5pinsertc_c(hid_t_f  *plist, _fcd name, int_f *name_len, size_t_f *size, _fcd value, int_f *value_len);
H5_FCDLL int_f nh5pset_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f nh5pset_integer_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f nh5pset_real_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f nh5pset_double_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f nh5psetc_c(hid_t_f *prp_id, _fcd name, int_f *name_len, _fcd value, int_f *value_len);
H5_FCDLL int_f nh5pget_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f nh5pget_integer_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f nh5pget_real_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f nh5pget_double_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f nh5pgetc_c(hid_t_f *prp_id, _fcd name, int_f *name_len, _fcd value, int_f *value_len);
H5_FCDLL int_f nh5pexist_c(hid_t_f *prp_id, _fcd name, int_f *name_len);
H5_FCDLL int_f nh5pget_size_c(hid_t_f *prp_id, _fcd name, int_f *name_len, size_t_f *size);
H5_FCDLL int_f nh5pget_nprops_c(hid_t_f *prp_id, size_t_f *nprops);
H5_FCDLL int_f nh5pget_class_parent_c(hid_t_f *prp_id, hid_t_f *parent_id);
H5_FCDLL int_f nh5pisa_class_c(hid_t_f *plist, hid_t_f *pclass);
H5_FCDLL int_f nh5pcopy_prop_c(hid_t_f *dst_id, hid_t_f *src_id, _fcd name, int_f *name_len);
H5_FCDLL int_f nh5premove_c(hid_t_f *plid, _fcd name, int_f *name_len);
H5_FCDLL int_f nh5punregister_c(hid_t_f *class, _fcd name, int_f *name_len);
H5_FCDLL int_f nh5pclose_class_c(hid_t_f * class);
H5_FCDLL int_f nh5pget_class_name_c(hid_t_f *prp_id, _fcd name, int_f *name_len);
H5_FCDLL int_f nh5pset_shuffle_c ( hid_t_f *prp_id);
H5_FCDLL int_f nh5pset_fletcher32_c ( hid_t_f *prp_id );
H5_FCDLL int_f nh5pset_edc_check_c ( hid_t_f *prp_id, int_f *flag );
H5_FCDLL int_f nh5pget_edc_check_c ( hid_t_f *prp_id, int_f *flag );
H5_FCDLL int_f nh5pset_family_offset_c ( hid_t_f *prp_id , hsize_t_f *offset);
H5_FCDLL int_f nh5pget_fapl_multi_c ( hid_t_f *prp_id , int_f *mem_map, hid_t_f *memb_fapl, _fcd memb_name, int_f *len, int_f *lenmax, real_f *memb_addr, int_f *flag, int_f *maxlen_out);
H5_FCDLL int_f nh5pset_fapl_multi_c ( hid_t_f *prp_id , int_f *mem_map, hid_t_f *memb_fapl, _fcd memb_name, int_f *len, int_f *lenmax, real_f *memb_addr, int_f *flag);
H5_FCDLL int_f nh5pset_fapl_multi_sc ( hid_t_f *prp_id , int_f *flag);
H5_FCDLL int_f nh5pset_szip_c ( hid_t_f *prp_id , int_f *options_mask, int_f *pixels_per_block);
H5_FCDLL int_f nh5pall_filters_avail_c ( hid_t_f *prp_id , int_f *status);
H5_FCDLL int_f nh5pfill_value_defined_c ( hid_t_f *prp_id , int_f *flag);

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

H5_FCDLL int_f nh5rcreate_object_c (haddr_t_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen);
H5_FCDLL int_f nh5rcreate_region_c (int_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *space_id);
H5_FCDLL int_f nh5rdereference_region_c (hid_t_f *dset_id, int_f *ref, hid_t_f *obj_id);
H5_FCDLL int_f nh5rdereference_object_c (hid_t_f *dset_id, haddr_t_f *ref, hid_t_f *obj_id);
H5_FCDLL int_f nh5rget_region_region_c (hid_t_f *dset_id, int_f *ref, hid_t_f *space_id);
H5_FCDLL int_f nh5rget_object_type_obj_c (hid_t_f *dset_id, haddr_t_f *ref, int_f *obj_type);

/*
 * Functions from H5If.c
 */
#ifndef H5If90_FNAMES
#    define H5If90_FNAMES
#ifdef DF_CAPFNAMES
#  define nh5iget_type_c    FNAME(H5IGET_TYPE_C)
#  define nh5iget_name_c    FNAME(H5IGET_NAME_C)
#  define nh5iinc_ref_c     FNAME(H5IINC_REF_C)
#  define nh5idec_ref_c     FNAME(H5IDEC_REF_C)
#  define nh5iget_ref_c     FNAME(H5IGET_REF_C)
#  define nh5iget_file_id_c FNAME(H5IGET_FILE_ID_C)
#else
#  define nh5iget_type_c    FNAME(h5iget_type_c)
#  define nh5iget_name_c    FNAME(h5iget_name_c)
#  define nh5iinc_ref_c     FNAME(h5iinc_ref_c)
#  define nh5idec_ref_c     FNAME(h5idec_ref_c)
#  define nh5iget_ref_c     FNAME(h5iget_ref_c)
#  define nh5iget_file_id_c FNAME(h5iget_file_id_c)
#endif
#endif

H5_FCDLL int_f nh5iget_type_c(hid_t_f *obj_id, int_f *type);
H5_FCDLL int_f nh5iget_name_c(hid_t_f *obj_id, _fcd buf, size_t_f *buf_size, size_t_f *name_size);
H5_FCDLL int_f nh5iinc_ref_c(hid_t_f *obj_id, int_f *ref_count);
H5_FCDLL int_f nh5idec_ref_c(hid_t_f *obj_id, int_f *ref_count);
H5_FCDLL int_f nh5iget_ref_c(hid_t_f *obj_id, int_f *ref_count);
H5_FCDLL int_f nh5iget_file_id_c(hid_t_f *obj_id, hid_t_f *file_id);

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

H5_FCDLL int_f nh5eclear_c(void);
H5_FCDLL int_f nh5eprint_c1(_fcd name, int_f* namelen);
H5_FCDLL int_f nh5eprint_c2(void);
H5_FCDLL int_f nh5eget_major_c(int_f* error_no, _fcd name);
H5_FCDLL int_f nh5eget_minor_c(int_f* error_no, _fcd name);
H5_FCDLL int_f nh5eset_auto_c(int_f* printflag);

/*
 * Functions from H5f.c
 */
#ifndef H5_FNAMES
#    define H5_FNAMES
#ifdef DF_CAPFNAMES
#   define nh5open_c          FNAME(H5OPEN_C)
#   define nh5close_c         FNAME(H5CLOSE_C)
#   define nh5init_types_c    FNAME(H5INIT_TYPES_C)
#   define nh5close_types_c   FNAME(H5CLOSE_TYPES_C)
#   define nh5init_flags_c    FNAME(H5INIT_FLAGS_C)
#   define nh5init1_flags_c    FNAME(H5INIT1_FLAGS_C)
#   define nh5get_libversion_c  FNAME(H5GET_LIBVERSION_C)
#   define nh5check_version_c   FNAME(H5CHECK_VERSION_C)
#   define nh5garbage_collect_c FNAME(H5GARBAGE_COLLECT_C)
#   define nh5dont_atexit_c     FNAME(H5DONT_ATEXIT_C)
#else
#   define nh5open_c         FNAME(h5open_c)
#   define nh5close_c         FNAME(h5close_c)
#   define nh5init_types_c    FNAME(h5init_types_c)
#   define nh5close_types_c   FNAME(h5close_types_c)
#   define nh5init_flags_c    FNAME(h5init_flags_c)
#   define nh5init1_flags_c    FNAME(h5init1_flags_c)
#   define nh5get_libversion_c  FNAME(h5get_libversion_c)
#   define nh5check_version_c   FNAME(h5check_version_c)
#   define nh5garbage_collect_c FNAME(h5garbage_collect_c)
#   define nh5dont_atexit_c     FNAME(h5dont_atexit_c)
#endif
#endif

H5_FCDLL int_f nh5open_c(void);
H5_FCDLL int_f nh5close_c(void);
H5_FCDLL int_f nh5init_types_c(hid_t_f *types, hid_t_f * floatingtypes, hid_t_f * integertypes);
H5_FCDLL int_f nh5close_types_c(hid_t_f *types, int_f *lentypes, hid_t_f * floatingtypes, int_f * floatinglen, hid_t_f * integertypes,  int_f * integerlen);
 H5_FCDLL int_f nh5init_flags_c( int_f *h5d_flags, int_f *h5e_flags, int_f *h5f_flags,
                              int_f *h5fd_flags, hid_t_f *h5fd_hid_flags,
                              int_f *h5g_flags, int_f *h5i_flags,
                              hid_t_f *h5p_flags, int_f *h5r_flags, int_f *h5s_flags,
                              int_f *h5t_flags, int_f *h5z_flags);
H5_FCDLL int_f nh5init1_flags_c(int_f *h5lib_flags);
H5_FCDLL int_f nh5get_libversion_c(int_f *majnum, int_f *minnum, int_f *relnum);
H5_FCDLL int_f nh5check_version_c(int_f *majnum, int_f *minnum, int_f *relnum);
H5_FCDLL int_f nh5garbage_collect_c(void);
H5_FCDLL int_f nh5dont_atexit_c(void);

/*
 * Functions from H5Zf.c
 */
#ifndef H5Zf90_FNAMES
#    define H5Zf90_FNAMES
#ifdef DF_CAPFNAMES
#  define nh5zunregister_c    FNAME(H5ZUNREGISTER_C)
#  define nh5zfilter_avail_c  FNAME(H5ZFILTER_AVAIL_C)
#  define nh5zget_filter_info_c FNAME(H5ZGET_FILTER_INFO_C)
#else
#  define nh5zunregister_c    FNAME(h5zunregister_c)
#  define nh5zfilter_avail_c  FNAME(h5zfilter_avail_c)
#  define nh5zget_filter_info_c FNAME(h5zget_filter_info_c)
#endif
#endif

H5_FCDLL int_f nh5zunregister_c (int_f *filter);
H5_FCDLL int_f nh5zfilter_avail_c (int_f *filter, int_f *flag);
H5_FCDLL int_f nh5zget_filter_info_c (int_f *filter, int_f *flag);


#endif /* _H5f90proto_H */
