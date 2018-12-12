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


#ifndef _H5f90proto_H
#define _H5f90proto_H

#include "H5public.h"
#include "H5f90.h"

H5_FCDLL char * HD5f2cstring (_fcd fdesc, size_t len);
H5_FCDLL void HD5packFstring(char *src, char *dest, size_t len);


/*
 * Storage info struct used by H5O_info_t and H5F_info_t 
 * interoperable with Fortran.
 */
typedef struct H5_ih_info_t_f {
    hsize_t     index_size;     /* btree and/or list */
    hsize_t     heap_size;
} H5_ih_info_t_f;

/* Information struct for object header metadata (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx) 
 *  interoperable with Fortran.
 */
typedef struct H5O_hdr_info_t_f {
    int_f version;		/* Version number of header format in file */
    int_f nmesgs;		/* Number of object header messages */
    int_f nchunks;		/* Number of object header chunks */
    int_f flags;                /* Object header status flags */
    struct {
        hsize_t total;		/* Total space for storing object header in file */
        hsize_t meta;		/* Space within header for object header metadata information */
        hsize_t mesg;		/* Space within header for actual message information */
        hsize_t free;		/* Free space within object header */
    } space;
    struct {
        uint64_t present;	/* Flags to indicate presence of message type in header */
        uint64_t shared;	/* Flags to indicate message type is shared in header */
    } mesg;
} H5O_hdr_info_t_f;

/* Information struct for object (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx) 
 *  interoperable with Fortran.
 */
typedef struct H5O_info_t_f {
    unsigned long 	fileno;		/* File number that object is located in */
    haddr_t_f 		addr;		/* Object address in file	*/
    int 		type;		/* Basic object type (group, dataset, etc.) */
    int_f 		rc;		/* Reference count of object    */
    int_f	        atime[8];	/* Access time			*/
    int_f		mtime[8];	/* Modification time		*/
    int_f		ctime[8];	/* Change time			*/
    int_f		btime[8];	/* Birth time			*/
    hsize_t 		num_attrs;	/* # of attributes attached to object */
    H5O_hdr_info_t_f    hdr;            /* Object header information */
    /* Extra metadata storage for obj & attributes */
    struct {
        H5_ih_info_t_f   obj;             /* v1/v2 B-tree & local/fractal heap for groups, B-tree for chunked datasets */
        H5_ih_info_t_f   attr;            /* v2 B-tree & heap for attributes */
    } meta_size;
} H5O_info_t_f;


/*
 *  Functions from H5Ff.c
 */
H5_FCDLL int_f h5fcreate_c(_fcd name, int_f *namelen, int_f *access_flags, hid_t_f *crt_prp, hid_t_f *acc_prp, hid_t_f *file_id);
H5_FCDLL int_f h5fopen_c(_fcd name, int_f *namelen, int_f *access_flags, hid_t_f *acc_prp, hid_t_f *file_id);
H5_FCDLL int_f h5fis_hdf5_c(_fcd name, int_f *namelen, int_f *flag);
H5_FCDLL int_f h5fclose_c(hid_t_f *file_id);
H5_FCDLL int_f h5fmount_c(hid_t_f *loc_id, _fcd dsetname, int_f *namelen, hid_t_f *file_id, hid_t_f *acc_prp);
H5_FCDLL int_f h5funmount_c(hid_t_f *loc_id, _fcd dsetname, int_f *namelen);
H5_FCDLL int_f h5freopen_c(hid_t_f *file_id1, hid_t_f *file_id2);
H5_FCDLL int_f h5fget_create_plist_c(hid_t_f *file_id, hid_t_f *prop_id);
H5_FCDLL int_f h5fget_access_plist_c(hid_t_f *file_id, hid_t_f *access_id);
H5_FCDLL int_f h5fget_obj_count_c(hid_t_f *file_id, int_f *obj_type, size_t_f *obj_count);
H5_FCDLL int_f h5fget_obj_ids_c(hid_t_f *file_id, int_f *obj_type, size_t_f *max_objs, hid_t_f *obj_ids, size_t_f *num_objs);
H5_FCDLL int_f h5fget_freespace_c(hid_t_f *file_id, hssize_t_f *free_space);
H5_FCDLL int_f h5fget_file_image_c(hid_t_f *file_id, void *buf_ptr, size_t_f *buf_len, size_t_f *buf_req);
H5_FCDLL int_f h5fflush_c(hid_t_f *obj_id, int_f *scope);
H5_FCDLL int_f h5fget_name_c(hid_t_f *obj_id, size_t_f *size, _fcd buf, size_t_f *buflen);
H5_FCDLL int_f h5fget_filesize_c(hid_t_f *file_id, hsize_t_f *size);

/*
 * Functions from H5Sf.c
 */
H5_FCDLL int_f h5screate_simple_c( int_f *rank, hsize_t_f *dims, hsize_t_f *maxdims, hid_t_f *space_id );
H5_FCDLL int_f h5sclose_c( hid_t_f *space_id );
H5_FCDLL int_f h5screate_c( int_f *classtype, hid_t_f *space_id );
H5_FCDLL int_f h5scopy_c( hid_t_f *space_id , hid_t_f *new_space_id);
H5_FCDLL int_f h5sget_select_hyper_nblocks_c( hid_t_f *space_id , hssize_t_f * num_blocks);
H5_FCDLL int_f h5sget_select_hyper_blocklist_c( hid_t_f *space_id ,hsize_t_f * startblock, hsize_t_f * num_blocks, hsize_t_f * buf);
H5_FCDLL int_f h5sget_select_bounds_c( hid_t_f *space_id , hsize_t_f * start, hsize_t_f * end);
H5_FCDLL int_f h5sget_select_elem_npoints_c( hid_t_f *space_id , hssize_t_f * num_points);
H5_FCDLL int_f h5sget_select_elem_pointlist_c( hid_t_f *space_id ,hsize_t_f * startpoint, hsize_t_f * numpoints, hsize_t_f * buf);
H5_FCDLL int_f h5sselect_all_c( hid_t_f *space_id );
H5_FCDLL int_f h5sselect_none_c( hid_t_f *space_id );
H5_FCDLL int_f h5sselect_valid_c( hid_t_f *space_id , int_f *flag );
H5_FCDLL int_f h5sget_simple_extent_npoints_c( hid_t_f *space_id , hsize_t_f *npoints );
H5_FCDLL int_f h5sget_select_npoints_c( hid_t_f *space_id , hssize_t_f *npoints );
H5_FCDLL int_f h5sget_simple_extent_ndims_c( hid_t_f *space_id , int_f *ndims );
H5_FCDLL int_f h5sget_simple_extent_type_c( hid_t_f *space_id , int_f *classtype);
H5_FCDLL int_f h5soffset_simple_c( hid_t_f *space_id , hssize_t_f *offset);
H5_FCDLL int_f h5sset_extent_simple_c( hid_t_f *space_id , int_f *rank, hsize_t_f * current_size, hsize_t_f *maximum_size);
H5_FCDLL int_f h5sis_simple_c( hid_t_f *space_id , int_f *flag );
H5_FCDLL int_f h5sget_simple_extent_dims_c( hid_t_f *space_id , hsize_t_f *dims, hsize_t_f *maxdims);
H5_FCDLL int_f h5sextent_copy_c( hid_t_f *dest_space_id , hid_t_f *source_space_id);
H5_FCDLL int_f h5sset_extent_none_c( hid_t_f *space_id );
H5_FCDLL int_f h5sselect_hyperslab_c( hid_t_f *space_id , int_f *op, hsize_t_f *start, hsize_t_f *count, hsize_t_f *stride, hsize_t_f *block);
H5_FCDLL int_f h5sget_select_type_c( hid_t_f *space_id , int_f *op);
H5_FCDLL int_f h5sselect_elements_c( hid_t_f *space_id , int_f *op, size_t_f *nelements, hsize_t_f *coord);
H5_FCDLL int_f h5scombine_hyperslab_c( hid_t_f *space_id , int_f *op, hsize_t_f *start, hsize_t_f *count, hsize_t_f *stride, hsize_t_f *block, hid_t_f *hyper_id);
H5_FCDLL int_f h5scombine_select_c( hid_t_f *space1_id , int_f *op, hid_t_f *space2_id, hid_t_f *ds_id);
H5_FCDLL int_f h5sselect_select_c( hid_t_f *space1_id , int_f *op, hid_t_f *space2_id);
H5_FCDLL int_f h5sdecode_c( _fcd buf, hid_t_f *obj_id );
H5_FCDLL int_f h5sencode_c(_fcd buf, hid_t_f *obj_id, size_t_f *nalloc );
H5_FCDLL int_f h5sextent_equal_c( hid_t_f * space1_id, hid_t_f *space2_id, hid_t_f *c_equal);

/*
 * Functions from H5Df.c
 */

H5_FCDLL int_f h5dcreate_c(hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *space_id,
			     hid_t_f *lcpl_id, hid_t_f *dcpl_id, hid_t_f *dapl_id, hid_t_f *dset_id);
H5_FCDLL int_f h5dopen_c(hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *dapl_id, hid_t_f *dset_id);
H5_FCDLL int_f h5dclose_c( hid_t_f *dset_id );

H5_FCDLL int_f h5dwrite_vl_integer_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f h5dwrite_vl_real_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, real_f *buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f h5dwrite_vl_string_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f h5dwrite_ref_reg_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims);
H5_FCDLL int_f h5dread_vl_integer_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f *buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f h5dread_vl_real_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, real_f *buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f h5dread_vl_string_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, _fcd buf, hsize_t_f *dims, size_t_f *len);
H5_FCDLL int_f h5dread_ref_reg_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, hid_t_f *file_space_id, hid_t_f *xfer_prp, int_f * buf, hsize_t_f *dims);
H5_FCDLL int_f h5dget_access_plist_c(hid_t_f *dset_id, hid_t_f *plist_id);
H5_FCDLL int_f h5dget_space_c( hid_t_f *dset_id , hid_t_f *space_id);
H5_FCDLL int_f h5dget_type_c( hid_t_f *dset_id , hid_t_f *type_id);
H5_FCDLL int_f h5dget_create_plist_c( hid_t_f *dset_id , hid_t_f *plist_id);
H5_FCDLL int_f h5dset_extent_c( hid_t_f *dset_id , hsize_t_f *dims);
H5_FCDLL int_f h5dvlen_get_max_len_c(hid_t_f *dataset_id, hid_t_f *type_id, hid_t_f *space_id, size_t_f *len);
H5_FCDLL int_f h5dget_storage_size_c(hid_t_f *dataset_id, hsize_t_f *size);
H5_FCDLL int_f h5dfill_c(void * fill_value, hid_t_f *fill_type_id, hid_t_f *space_id, void * buf, hid_t_f *mem_type_id);
H5_FCDLL int_f h5dget_space_status_c( hid_t_f *dset_id, int_f *flag);
H5_FCDLL int_f h5dcreate_anon_c(hid_t_f *loc_id, hid_t_f *type_id, hid_t_f *space_id,
				  hid_t_f *dcpl_id, hid_t_f *dapl_id, hid_t_f *dset_id);
H5_FCDLL int_f h5dwrite_f_c(hid_t_f *dset_id, hid_t_f *mem_type_id, hid_t_f *mem_space_id, 
				   hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf);
H5_FCDLL int_f h5dread_f_c( hid_t_f *dset_id ,  hid_t_f *mem_type_id, hid_t_f *mem_space_id, 
				   hid_t_f *file_space_id, hid_t_f *xfer_prp, void *buf);
H5_FCDLL int_f h5dvlen_reclaim_c(hid_t_f *type_id ,  hid_t_f *space_id, hid_t_f *plist_id, void *buf);

/*
 * Functions from H5Gf.c
 */
H5_FCDLL int_f h5gcreate_c(hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *size_hint,  hid_t_f *grp_id,
			     hid_t_f *lcpl_id, hid_t_f *gcpl_id, hid_t_f *gapl_id);
H5_FCDLL int_f h5gopen_c(hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *gapl_id, hid_t_f *grp_id);
H5_FCDLL int_f h5gclose_c( hid_t_f *grp_id );
H5_FCDLL int_f h5gget_obj_info_idx_c(hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *idx, _fcd obj_name, int_f *obj_namelen, int_f *obj_type);
H5_FCDLL int_f h5gn_members_c(hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *nmembers);
H5_FCDLL int_f h5glink_c(hid_t_f *loc_id, int_f *link_type, _fcd current_name, int_f *current_namelen, _fcd new_name, int_f *new_namelen);
H5_FCDLL int_f h5glink2_c(hid_t_f *cur_loc_id, _fcd cur_name, int_f *cur_namelen, int_f *link_type, hid_t_f *new_loc_id, _fcd new_name, int_f *new_namelen);
H5_FCDLL int_f h5gunlink_c(hid_t_f *loc_id, _fcd name, int_f *namelen);
H5_FCDLL int_f h5gmove_c(hid_t_f *loc_id, _fcd src_name, int_f *src_namelen, _fcd dst_name, int_f *dst_namelen);
H5_FCDLL int_f h5gmove2_c(hid_t_f *src_loc_id, _fcd src_name, int_f *src_namelen, hid_t_f *dst_loc_id,_fcd dst_name, int_f *dst_namelen);
H5_FCDLL int_f h5gget_linkval_c(hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *size, _fcd value );
H5_FCDLL int_f h5gset_comment_c(hid_t_f *loc_id, _fcd name, int_f *namelen, _fcd comment, int_f *commentlen);
H5_FCDLL int_f h5gget_comment_c(hid_t_f *loc_id, _fcd name, int_f *namelen, size_t_f *bufsize, _fcd comment);
H5_FCDLL int_f h5gcreate_anon_c(hid_t_f *loc_id, hid_t_f *gcpl_id, hid_t_f *gapl_id, hid_t_f *grp_id);
H5_FCDLL int_f h5gget_create_plist_c(hid_t_f *grp_id, hid_t_f *gcpl_id );
H5_FCDLL int_f h5gget_info_c(hid_t_f *group_id, int_f *storage_type, int_f *nlinks, int_f *max_corder, int_f *mounted);
H5_FCDLL int_f h5gget_info_by_idx_c(hid_t_f *loc_id, _fcd group_name, size_t_f *group_namelen,
				     int_f *index_type, int_f *order, hsize_t_f *n, hid_t_f *lapl_id,
				     int_f *storage_type, int_f *nlinks, int_f *max_corder, int_f *mounted);
H5_FCDLL int_f h5gget_info_by_name_c(hid_t_f *loc_id, _fcd group_name, size_t_f *group_namelen, hid_t_f *lapl_id,
				      int_f *storage_type, int_f *nlinks, int_f *max_corder, int_f *mounted);

/*
 * Functions from H5Af.c
 */

H5_FCDLL int_f h5awrite_f_c(hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf);
H5_FCDLL int_f h5aread_f_c(hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf);
H5_FCDLL int_f h5acreate_c(hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *type_id,
                        hid_t_f *space_id, hid_t_f *crt_prp, hid_t_f *aapl, hid_t_f *attr_id);
H5_FCDLL int_f h5adelete_c(hid_t_f *obj_id, _fcd name, size_t_f *namelen);
H5_FCDLL int_f h5aget_num_attrs_c(hid_t_f *obj_id, int_f *attr_num);
H5_FCDLL int_f h5aget_name_c(hid_t_f *attr_id, size_t_f *size, _fcd buf);
H5_FCDLL int_f h5arename_by_name_c( hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
				      _fcd old_attr_name, size_t_f *old_attr_namelen,
				      _fcd new_attr_name, size_t_f *new_attr_namelen,
				      hid_t_f *lapl_id );
H5_FCDLL int_f h5aopen_c( hid_t_f *obj_id, _fcd attr_name, size_t_f *attr_namelen,
			    hid_t_f *aapl_id, hid_t_f *attr_id);
H5_FCDLL int_f h5adelete_by_name_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
				     _fcd attr_name, size_t_f *attr_namelen, hid_t_f *lapl_id);
H5_FCDLL int_f h5adelete_by_idx_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
				    int_f *idx_type, int_f *order, hsize_t_f *n, hid_t_f *lapl_id);
H5_FCDLL int_f h5aget_name_by_idx_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
				      int_f *idx_type, int_f *order, hsize_t_f *n, _fcd name,
				      size_t_f *size, hid_t_f *lapl_id);
H5_FCDLL int_f h5aopen_by_idx_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
		     int_f *idx_type, int_f *order, hsize_t_f *n, hid_t_f *aapl_id, hid_t_f *lapl_id, hid_t_f *attr_id);
H5_FCDLL int_f h5aget_info_c(hid_t_f *loc_id, int_f *corder_valid, int_f *corder,
			       int_f *cset, hsize_t_f *data_size );
H5_FCDLL int_f h5aget_info_by_idx_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
				      int_f *idx_type, int_f *order, hsize_t_f *n, hid_t_f *lapl_id,
				      int_f *corder_valid, int_f *corder,
				      int_f *cset, hsize_t_f *data_size );
H5_FCDLL int_f h5aget_info_by_name_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
				       _fcd attr_name, size_t_f *attr_namelen, hid_t_f *lapl_id,
				       int_f *corder_valid, int_f *corder,
				       int_f *cset, hsize_t_f *data_size );
H5_FCDLL int_f h5acreate_by_name_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen,
				    _fcd attr_name, size_t_f *attr_namelen,  hid_t_f *type_id,
				    hid_t_f *space_id, hid_t_f *acpl_id, hid_t_f *aapl_id,
				    hid_t_f *lapl_id, hid_t_f *attr_id );
H5_FCDLL int_f h5aexists_c(hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *attr_exists);
H5_FCDLL int_f h5aexists_by_name_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, _fcd attr_name, size_t_f *attr_namelen,
		      hid_t_f *lapl_id, int_f *attr_exists);
H5_FCDLL int_f h5aopen_by_name_c(hid_t_f *loc_id, _fcd obj_name, size_t_f *obj_namelen, _fcd attr_name, size_t_f *attr_namelen,
				   hid_t_f *aapl_id, hid_t_f *lapl_id, hid_t_f *attr_id);
H5_FCDLL int_f h5arename_c( hid_t_f *loc_id,
		      _fcd old_attr_name, size_t_f *old_attr_namelen,
		      _fcd new_attr_name, size_t_f *new_attr_namelen);

/*
 * Functions form H5Tf.c file
 */

H5_FCDLL int_f h5tcreate_c(int_f *cls, size_t_f *size, hid_t_f *type_id);
H5_FCDLL int_f h5topen_c(hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *tapl_id );
H5_FCDLL int_f h5tcommit_c(hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *lcpl_id, hid_t_f *tcpl_id, hid_t_f *tapl_id);
H5_FCDLL int_f h5tclose_c( hid_t_f *type_id );
H5_FCDLL int_f h5tequal_c( hid_t_f *type1_id , hid_t_f *type2_id, int_f *c_flag);
H5_FCDLL int_f h5tcopy_c( hid_t_f *type_id , hid_t_f *new_type_id);
H5_FCDLL int_f h5tget_class_c( hid_t_f *type_id , int_f *classtype);
H5_FCDLL int_f h5tget_order_c( hid_t_f *type_id , int_f *order);
H5_FCDLL int_f h5tset_order_c( hid_t_f *type_id , int_f *order);
H5_FCDLL int_f h5tget_size_c( hid_t_f *type_id , size_t_f *size);
H5_FCDLL int_f h5tset_size_c( hid_t_f *type_id , size_t_f *size);
H5_FCDLL int_f h5tcommitted_c(hid_t_f *dtype_id);
H5_FCDLL int_f h5tget_precision_c( hid_t_f *type_id , size_t_f *precision);
H5_FCDLL int_f h5tset_precision_c( hid_t_f *type_id , size_t_f *precision);
H5_FCDLL int_f h5tget_offset_c( hid_t_f *type_id , size_t_f *offset);
H5_FCDLL int_f h5tset_offset_c( hid_t_f *type_id , size_t_f *offset);
H5_FCDLL int_f h5tget_pad_c( hid_t_f *type_id , int_f * lsbpad, int_f * msbpad);
H5_FCDLL int_f h5tset_pad_c( hid_t_f *type_id, int_f * lsbpad, int_f * msbpad );
H5_FCDLL int_f h5tget_sign_c( hid_t_f *type_id , int_f* sign);
H5_FCDLL int_f h5tset_sign_c( hid_t_f *type_id , int_f *sign);
H5_FCDLL int_f h5tget_fields_c( hid_t_f *type_id, size_t_f *spos, size_t_f *epos, size_t_f* esize, size_t_f* mpos, size_t_f* msize);
H5_FCDLL int_f h5tset_fields_c( hid_t_f *type_id, size_t_f *spos, size_t_f *epos, size_t_f* esize, size_t_f* mpos, size_t_f* msize);
H5_FCDLL int_f h5tget_ebias_c( hid_t_f *type_id , size_t_f *ebias);
H5_FCDLL int_f h5tset_ebias_c( hid_t_f *type_id , size_t_f *ebias);
H5_FCDLL int_f h5tget_norm_c( hid_t_f *type_id , int_f *norm);
H5_FCDLL int_f h5tset_norm_c( hid_t_f *type_id , int_f *norm);
H5_FCDLL int_f h5tget_inpad_c( hid_t_f *type_id, int_f * padtype);
H5_FCDLL int_f h5tset_inpad_c( hid_t_f *type_id, int_f * padtype);
H5_FCDLL int_f h5tget_cset_c( hid_t_f *type_id, int_f * cset);
H5_FCDLL int_f h5tset_cset_c( hid_t_f *type_id, int_f * cset);
H5_FCDLL int_f h5tget_strpad_c( hid_t_f *type_id, int_f * strpad);
H5_FCDLL int_f h5tset_strpad_c( hid_t_f *type_id, int_f * strpad);
H5_FCDLL int_f h5tget_nmembers_c( hid_t_f *type_id , int_f * num_members);
H5_FCDLL int_f h5tget_member_name_c( hid_t_f *type_id ,int_f* idx, _fcd member_name, int_f *namelen);
H5_FCDLL int_f h5tget_member_dims_c( hid_t_f *type_id ,int_f* field_idx, int_f * dims, size_t_f * field_dims, int_f * perm );
H5_FCDLL int_f h5tget_member_offset_c( hid_t_f *type_id ,int_f* member_no, size_t_f* offset);
H5_FCDLL int_f h5tget_member_type_c( hid_t_f *type_id ,int_f* field_idx, hid_t_f * datatype);
H5_FCDLL int_f h5tget_member_index_c( hid_t_f *type_id ,_fcd name, int_f* namelen, int_f *idx);
H5_FCDLL int_f h5tinsert_c(hid_t_f *type_id, _fcd name, int_f* namelen, size_t_f *offset, hid_t_f * field_id);
H5_FCDLL int_f h5tpack_c(hid_t_f * type_id);
H5_FCDLL int_f h5tinsert_array_c(hid_t_f * parent_id, _fcd name, int_f* namelen, size_t_f* offset, int_f* ndims, size_t_f* dims, hid_t_f* member_id, int_f* perm );
H5_FCDLL int_f h5tinsert_array_c2(hid_t_f * parent_id, _fcd name, int_f* namelen, size_t_f* offset, int_f* ndims, size_t_f* dims, hid_t_f* member_id);
H5_FCDLL int_f h5tenum_create_c( hid_t_f *parent_id , hid_t_f *new_type_id);
H5_FCDLL int_f h5tenum_insert_c(hid_t_f *type_id, _fcd name, int_f* namelen, int_f* value);
H5_FCDLL int_f h5tenum_insert_ptr_c(hid_t_f *type_id, _fcd name, int_f* namelen, void *value);
H5_FCDLL int_f h5tenum_nameof_c(hid_t_f *type_id, int_f* value, _fcd name, size_t_f* namelen);
H5_FCDLL int_f h5tenum_valueof_c(hid_t_f *type_id, _fcd name, int_f* namelen, int_f* value);
H5_FCDLL int_f h5tget_member_value_c(hid_t_f *type_id, int_f* member_no, int_f* value);
H5_FCDLL int_f h5tset_tag_c(hid_t_f* type_id, _fcd tag, int_f* namelen);
H5_FCDLL int_f h5tget_tag_c(hid_t_f* type_id, _fcd tag, size_t_f* tag_size, int_f* namelen);
H5_FCDLL int_f h5tarray_create_c(hid_t_f * base_id, int_f *rank, hsize_t_f* dims, hid_t_f* type_id);
H5_FCDLL int_f h5tget_array_dims_c( hid_t_f *type_id , hsize_t_f * dims);
H5_FCDLL int_f h5tget_array_ndims_c( hid_t_f *type_id , int_f * ndims);
H5_FCDLL int_f h5tget_super_c( hid_t_f *type_id , hid_t_f *base_type_id);
H5_FCDLL int_f h5tvlen_create_c( hid_t_f *type_id , hid_t_f *vltype_id);
H5_FCDLL int_f h5tis_variable_str_c( hid_t_f *type_id , int_f *flag );
H5_FCDLL int_f h5tget_member_class_c( hid_t_f *type_id ,  int_f *member_no, int_f *cls );
H5_FCDLL int_f h5tcommit_anon_c(hid_t_f *loc_id, hid_t_f *dtype_id, hid_t_f *tcpl_id, hid_t_f *tapl_id);
H5_FCDLL int_f h5tdecode_c( _fcd buf, hid_t_f *obj_id );
H5_FCDLL int_f h5tencode_c(_fcd buf, hid_t_f *obj_id, size_t_f *nalloc );
H5_FCDLL int_f h5tget_create_plist_c( hid_t_f *dtype_id,  hid_t_f *dtpl_id);
H5_FCDLL int_f h5tcompiler_conv_c( hid_t_f *src_id, hid_t_f *dst_id, int_f *c_flag);
H5_FCDLL int_f h5tget_native_type_c(hid_t_f *dtype_id, int_f *direction, hid_t_f *native_dtype_id);
H5_FCDLL int_f h5tconvert_c(hid_t_f *src_id, hid_t_f *dst_id, size_t_f *nelmts, void *buf, void *background, hid_t_f *plist_id);

/*
 * Functions from H5Of.c
 */

H5_FCDLL int_f h5oopen_c(hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id, hid_t_f *obj_id);
H5_FCDLL int_f h5oclose_c(hid_t_f *object_id );
H5_FCDLL int_f h5oopen_by_addr_c(hid_t_f *loc_id, haddr_t_f *addr, hid_t_f *obj_id);
H5_FCDLL int_f h5olink_c(hid_t_f *object_id, hid_t_f *new_loc_id, _fcd name, size_t_f *namelen,
			   hid_t_f *lcpl_id, hid_t_f *lapl_id);
H5_FCDLL int_f h5ovisit_c(hid_t_f *group_id, int_f *index_type, int_f *order, H5O_iterate_t op, void *op_data, int_f *fields);
H5_FCDLL int_f h5ovisit_by_name_c(hid_t_f *loc_id,  _fcd object_name, size_t_f *namelen, int_f *index_type, int_f *order,
				   H5O_iterate_t op, void *op_data, hid_t_f *lapl_id, int_f *fields );
H5_FCDLL int_f h5oget_info_c(hid_t_f *object_id, H5O_info_t_f *object_info, int_f *fields);
H5_FCDLL int_f h5oget_info_by_idx_c(hid_t_f *loc_id, _fcd  group_name, size_t_f *namelen, 
				      int_f *index_field, int_f *order, hsize_t_f *n, hid_t_f *lapl_id, H5O_info_t_f *object_info, int_f *fields);
H5_FCDLL int_f h5oget_info_by_name_c(hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id,
				       H5O_info_t_f *object_info, int_f *fields);
H5_FCDLL int_f h5ocopy_c(hid_t_f *src_loc_id, _fcd src_name, size_t_f *src_name_len,
			   hid_t_f *dst_loc_id, _fcd dst_name, size_t_f *dst_name_len, 
			   hid_t_f *ocpypl_id, hid_t_f *lcpl_id );
H5_FCDLL int_f h5odecr_refcount_c(hid_t_f *object_id);
H5_FCDLL int_f h5oincr_refcount_c(hid_t_f *object_id);
H5_FCDLL int_f h5oexists_by_name_c(hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id);
H5_FCDLL int_f h5oset_comment_c(hid_t_f *object_id, _fcd comment, size_t_f *commentlen);
H5_FCDLL int_f h5oset_comment_by_name_c(hid_t_f *object_id, _fcd name, size_t_f *namelen,  _fcd comment, size_t_f *commentlen, hid_t_f *lapl_id);
H5_FCDLL int_f h5oopen_by_idx_c(hid_t_f *loc_id, _fcd  group_name, size_t_f *group_namelen, 
				      int_f *index_type, int_f *order, hsize_t_f *n, hid_t_f *obj_id, hid_t_f *lapl_id);
H5_FCDLL int_f h5oget_comment_c(hid_t_f *object_id, _fcd comment, size_t_f *commentsize, hssize_t_f *bufsize);
H5_FCDLL int_f h5oget_comment_by_name_c(hid_t_f *loc_id, _fcd name, size_t_f *name_size, 
					  _fcd comment, size_t_f *commentsize, size_t_f *bufsize, hid_t_f *lapl_id);
/*
 * Functions from H5Pf.c
 */
H5_FCDLL int_f h5pcreate_c( hid_t_f *cls, hid_t_f *prp_id );
H5_FCDLL int_f h5pclose_c( hid_t_f *prp_id );
H5_FCDLL int_f h5pcopy_c( hid_t_f *prp_id , hid_t_f *new_prp_id);
H5_FCDLL int_f h5pequal_c( hid_t_f *plist1_id , hid_t_f *plist2_id, int_f *c_flag);
H5_FCDLL int_f h5pget_class_c( hid_t_f *prp_id , hid_t_f *classtype);
H5_FCDLL int_f h5pset_deflate_c( hid_t_f *prp_id , int_f *level);
H5_FCDLL int_f h5pset_chunk_c( hid_t_f *prp_id, int_f *rank, hsize_t_f *dims );
H5_FCDLL int_f h5pget_chunk_c( hid_t_f *prp_id, int_f *max_rank, hsize_t_f *dims );
H5_FCDLL int_f h5pset_file_image_c(hid_t_f *fapl_id, void *buf_ptr, size_t_f *buf_len);
H5_FCDLL int_f h5pset_fill_value_c(hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f h5pget_file_image_c(hid_t_f *fapl_id, void **buf_ptr, size_t_f *buf_len);
H5_FCDLL int_f h5pget_fill_value_c(hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue);
H5_FCDLL int_f h5pset_preserve_c( hid_t_f *prp_id , int_f *flag);
H5_FCDLL int_f h5pget_preserve_c( hid_t_f *prp_id , int_f *flag);
H5_FCDLL int_f h5pget_version_c(hid_t_f *prp_id, int_f * boot,int_f * freelist, int_f * stab, int_f *shhdr);
H5_FCDLL int_f h5pset_userblock_c(hid_t_f *prp_id, hsize_t_f * size);
H5_FCDLL int_f h5pget_userblock_c(hid_t_f *prp_id, hsize_t_f * size);
H5_FCDLL int_f h5pget_sizes_c(hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size);
H5_FCDLL int_f h5pset_sizes_c(hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size);
H5_FCDLL int_f h5pset_sym_k_c(hid_t_f *prp_id, int_f* ik, int_f* lk);
H5_FCDLL int_f h5pget_sym_k_c(hid_t_f *prp_id, int_f* ik, int_f* lk);
H5_FCDLL int_f h5pset_istore_k_c(hid_t_f *prp_id, int_f* ik);
H5_FCDLL int_f h5pget_istore_k_c(hid_t_f *prp_id, int_f* ik);
H5_FCDLL int_f h5pget_driver_c(hid_t_f *prp_id, hid_t_f*driver);
H5_FCDLL int_f h5pset_fapl_stdio_c(hid_t_f *prp_id);
#ifdef NO_SUCH_F90_FUNCTION
H5_FCDLL int_f h5pget_fapl_stdio_c(hid_t_f *prp_id, int_f* io);
#endif
H5_FCDLL int_f h5pset_fapl_sec2_c(hid_t_f *prp_id);
#ifdef NO_SUCH_F90_FUNCTION
H5_FCDLL int_f h5pget_fapl_sec2_c(hid_t_f *prp_id, int_f* sec2);
#endif
H5_FCDLL int_f h5pset_alignment_c(hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment);
H5_FCDLL int_f h5pget_alignment_c(hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment);
H5_FCDLL int_f h5pget_fapl_core_c(hid_t_f *prp_id, size_t_f* increment, int_f *flag);
H5_FCDLL int_f h5pset_fapl_core_c(hid_t_f *prp_id, size_t_f* increment, int_f *flag);
H5_FCDLL int_f h5pset_fapl_family_c(hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist );
H5_FCDLL int_f h5pget_fapl_family_c(hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist );
H5_FCDLL int_f h5pset_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, size_t_f* rdcc_nelmts, size_t_f* rdcc_nbytes, real_f* rdcc_w0);
H5_FCDLL int_f h5pget_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, size_t_f* rdcc_nelmts, size_t_f* rdcc_nbytes, real_f* rdcc_w0);
#ifdef NO_SUCH_F90_FUNCTION
H5_FCDLL int_f h5pget_fapl_split_c(hid_t_f *prp_id, size_t_f* meta_ext_size , _fcd meta_ext, hid_t_f* meta_plist, size_t_f* raw_ext_size, _fcd raw_ext, hid_t_f * raw_plist);
#endif
H5_FCDLL int_f h5pset_fapl_split_c(hid_t_f *prp_id, int_f* meta_len, _fcd meta_ext, hid_t_f* meta_plist, int_f* raw_len, _fcd raw_ext, hid_t_f * raw_plist);
H5_FCDLL int_f h5pset_gc_references_c(hid_t_f *prp_id, int_f* gc_references);
H5_FCDLL int_f h5pget_gc_references_c(hid_t_f *prp_id, int_f* gc_references);
H5_FCDLL int_f h5pset_layout_c(hid_t_f *prp_id, int_f* layout);
H5_FCDLL int_f h5pget_layout_c(hid_t_f *prp_id, int_f* layout);
H5_FCDLL int_f h5pset_filter_c(hid_t_f *prp_id, int_f* filter, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values );
H5_FCDLL int_f h5premove_filter_c(hid_t_f *prp_id, int_f* filter);
H5_FCDLL int_f h5pmodify_filter_c(hid_t_f *prp_id, int_f* filter, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values );
H5_FCDLL int_f h5pget_nfilters_c(hid_t_f *prp_id, int_f* nfilters);
H5_FCDLL int_f h5pget_filter_c(hid_t_f *prp_id, int_f* filter_number, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values, size_t_f *namelen, _fcd name, int_f* filter_id);
H5_FCDLL int_f h5pget_filter_by_id_c(hid_t_f *prp_id, int_f* filter_id, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values, size_t_f *namelen, _fcd name);
H5_FCDLL int_f h5pset_external_c(hid_t_f *prp_id, _fcd name, int_f* namelen, off_t_f* offset, hsize_t_f*bytes);
H5_FCDLL int_f h5pget_external_count_c(hid_t_f *prp_id, int_f* count);
H5_FCDLL int_f h5pget_external_c(hid_t_f *prp_id, int_f *idx, size_t_f* name_size, _fcd name, off_t_f* offset, hsize_t_f*bytes);
H5_FCDLL int_f h5pget_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right);
H5_FCDLL int_f h5pset_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right);
H5_FCDLL int_f h5pset_fclose_degree_c(hid_t_f *fapl, int_f *degree);
H5_FCDLL int_f h5pget_fclose_degree_c(hid_t_f *fapl, int_f *degree);
H5_FCDLL int_f h5pget_buffer_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f h5pset_buffer_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f h5pset_alloc_time_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f h5pget_alloc_time_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f h5pset_fill_time_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f h5pget_fill_time_c(hid_t_f *plist, int_f *flag);
H5_FCDLL int_f h5pset_meta_block_size_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f h5pget_meta_block_size_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f h5pset_sieve_buf_size_c(hid_t_f *plist, size_t_f *size);
H5_FCDLL int_f h5pget_sieve_buf_size_c(hid_t_f *plist, size_t_f *size);
H5_FCDLL int_f h5pset_small_data_block_size_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f h5pget_small_data_block_size_c(hid_t_f *plist, hsize_t_f *size);
H5_FCDLL int_f h5pset_hyper_vector_size_c(hid_t_f *plist, size_t_f *size);
H5_FCDLL int_f h5pget_hyper_vector_size_c(hid_t_f *plist, size_t_f *size);
H5_FCDLL int_f h5pcreate_class_c(hid_t_f *parent, _fcd name, int_f *name_len, hid_t_f *cls,
				  H5P_cls_create_func_t create, void *create_data,
				  H5P_cls_copy_func_t copy, void *copy_data,
				  H5P_cls_close_func_t close, void *close_data);
H5_FCDLL int_f h5pregister_c(hid_t_f *cls, _fcd name, int_f * name_len, size_t_f *size, void *value);
H5_FCDLL int_f h5pinsert_c(hid_t_f  *plist, _fcd name, int_f *name_len, size_t_f *size, void *value);
H5_FCDLL int_f h5pset_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f h5pget_c(hid_t_f *prp_id, _fcd name, int_f *name_len, void *value);
H5_FCDLL int_f h5pexist_c(hid_t_f *prp_id, _fcd name, int_f *name_len);
H5_FCDLL int_f h5pget_size_c(hid_t_f *prp_id, _fcd name, int_f *name_len, size_t_f *size);
H5_FCDLL int_f h5pget_nprops_c(hid_t_f *prp_id, size_t_f *nprops);
H5_FCDLL int_f h5pget_class_parent_c(hid_t_f *prp_id, hid_t_f *parent_id);
H5_FCDLL int_f h5pisa_class_c(hid_t_f *plist, hid_t_f *pclass);
H5_FCDLL int_f h5pcopy_prop_c(hid_t_f *dst_id, hid_t_f *src_id, _fcd name, int_f *name_len);
H5_FCDLL int_f h5premove_c(hid_t_f *plid, _fcd name, int_f *name_len);
H5_FCDLL int_f h5punregister_c(hid_t_f *cls, _fcd name, int_f *name_len);
H5_FCDLL int_f h5pclose_class_c(hid_t_f * cls);
H5_FCDLL int_f h5pget_class_name_c(hid_t_f *prp_id, _fcd name, int_f *name_len);
H5_FCDLL int_f h5pset_shuffle_c( hid_t_f *prp_id);
H5_FCDLL int_f h5pset_fletcher32_c( hid_t_f *prp_id );
H5_FCDLL int_f h5pset_edc_check_c( hid_t_f *prp_id, int_f *flag );
H5_FCDLL int_f h5pget_edc_check_c( hid_t_f *prp_id, int_f *flag );
H5_FCDLL int_f h5pset_family_offset_c( hid_t_f *prp_id , hsize_t_f *offset);
H5_FCDLL int_f h5pget_fapl_multi_c( hid_t_f *prp_id , int_f *mem_map, hid_t_f *memb_fapl, _fcd memb_name, int_f *len, int_f *lenmax, real_f *memb_addr, int_f *flag, int_f *maxlen_out);
H5_FCDLL int_f h5pset_fapl_multi_c( hid_t_f *prp_id , int_f *mem_map, hid_t_f *memb_fapl, _fcd memb_name, int_f *len, int_f *lenmax, real_f *memb_addr, int_f *flag);
H5_FCDLL int_f h5pset_fapl_multi_sc ( hid_t_f *prp_id , int_f *flag);
H5_FCDLL int_f h5pset_szip_c( hid_t_f *prp_id , int_f *options_mask, int_f *pixels_per_block);
H5_FCDLL int_f h5pall_filters_avail_c( hid_t_f *prp_id , int_f *status);
H5_FCDLL int_f h5pfill_value_defined_c( hid_t_f *prp_id , int_f *flag);
H5_FCDLL int_f h5pget_attr_phase_change_c(hid_t_f *ocpl_id, int_f *max_compact, int_f *min_dense );
H5_FCDLL int_f h5pset_attr_creation_order_c(hid_t_f *ocpl_id, int_f *crt_order_flags );
H5_FCDLL int_f h5pset_shared_mesg_nindexes_c(hid_t_f *plist_id, int_f *nindexes );
H5_FCDLL int_f h5pset_shared_mesg_index_c(hid_t_f *fcpl_id, int_f *index_num, int_f *mesg_type_flags, int_f *min_mesg_size);
H5_FCDLL int_f h5pget_attr_creation_order_c(hid_t_f *ocpl_id, int_f *crt_order_flags);
H5_FCDLL int_f h5pset_libver_bounds_c(hid_t_f *fapl_id, int_f *low, int_f *high);
H5_FCDLL int_f h5pset_link_creation_order_c(hid_t_f *gcpl_id, int_f *crt_order_flags);
H5_FCDLL int_f h5pget_link_phase_change_c(hid_t_f *gcpl_id, int_f *max_compact, int_f *min_dense );
H5_FCDLL int_f h5pget_obj_track_times_c(hid_t_f *plist_id, int_f *flag);
H5_FCDLL int_f h5pset_obj_track_times_c(hid_t_f *plist_id, int_f *flag);
H5_FCDLL int_f h5pset_create_inter_group_c(hid_t_f *lcpl_id, int_f *crt_intermed_group);
H5_FCDLL int_f h5pget_create_inter_group_c(hid_t_f *lcpl_id, int_f *crt_intermed_group);
H5_FCDLL int_f h5pget_link_creation_order_c(hid_t_f *gcpl_id, int_f *crt_order_flags);
H5_FCDLL int_f h5pset_char_encoding_c(hid_t_f *plist_id, int_f *encoding);
H5_FCDLL int_f h5pget_char_encoding_c(hid_t_f *plist_id, int_f *encoding);
H5_FCDLL int_f h5pset_copy_object_c(hid_t_f *ocp_plist_id, int_f *copy_options);
H5_FCDLL int_f h5pget_copy_object_c(hid_t_f *ocp_plist_id, int_f *copy_options);
H5_FCDLL int_f h5pget_data_transform_c(hid_t_f *plist_id, _fcd expression, int_f *expression_len, size_t_f *size);
H5_FCDLL int_f h5pset_data_transform_c(hid_t_f *plist_id, _fcd expression, int_f *expression_len);
H5_FCDLL int_f h5pget_local_heap_size_hint_c(hid_t_f *gcpl_id, size_t_f *size_hint);
H5_FCDLL int_f h5pget_est_link_info_c(hid_t_f *gcpl_id, int_f *est_num_entries, int_f *est_name_len);
H5_FCDLL int_f h5pset_local_heap_size_hint_c(hid_t_f *gcpl_id, size_t_f *size_hint);
H5_FCDLL int_f h5pset_est_link_info_c(hid_t_f *gcpl_id, int_f *est_num_entries, int_f *est_name_len);
H5_FCDLL int_f h5pset_link_phase_change_c(hid_t_f *gcpl_id, int_f *max_compact, int_f *min_dense );
H5_FCDLL int_f h5pset_fapl_direct_c(hid_t_f *fapl_id, size_t_f *alignment, size_t_f *block_size, size_t_f *cbuf_size );
H5_FCDLL int_f h5pget_fapl_direct_c(hid_t_f *fapl_id, size_t_f *alignment, size_t_f *block_size, size_t_f *cbuf_size );
H5_FCDLL int_f h5pset_attr_phase_change_c(hid_t_f *ocpl_id, int_f *max_compact, int_f *min_dense );
H5_FCDLL int_f h5pset_nbit_c(hid_t_f *plist_id );
H5_FCDLL int_f h5pset_scaleoffset_c(hid_t_f *plist_id, int_f *scale_type, int_f *scale_factor );
H5_FCDLL int_f h5pset_nlinks_c(hid_t_f *lapl_id, size_t_f *nlinks);
H5_FCDLL int_f h5pget_nlinks_c(hid_t_f *lapl_id, size_t_f *nlinks);
H5_FCDLL int_f h5pset_chunk_cache_c(hid_t_f *dapl_id, size_t_f *rdcc_nslots, size_t_f *rdcc_nbytes, real_f *rdcc_w0);
H5_FCDLL int_f h5pget_chunk_cache_c(hid_t_f *dapl_id, size_t_f *rdcc_nslots, size_t_f *rdcc_nbytes, real_f *rdcc_w0);
#ifdef H5_HAVE_PARALLEL
H5_FCDLL int_f h5pget_mpio_actual_io_mode_c(hid_t_f *dxpl_id, int_f *actual_io_mode);
H5_FCDLL int_f h5pget_fapl_mpio_c(hid_t_f *prp_id, int_f* comm, int_f* info);
H5_FCDLL int_f h5pset_fapl_mpio_c(hid_t_f *prp_id, int_f* comm, int_f* info);
H5_FCDLL int_f h5pget_dxpl_mpio_c(hid_t_f *prp_id, int_f* data_xfer_mode);
H5_FCDLL int_f h5pset_dxpl_mpio_c(hid_t_f *prp_id, int_f* data_xfer_mode);
#endif
/*
 * Functions frome H5Rf.c
 */
H5_FCDLL int_f h5rcreate_region_c(int_f *ref, hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *space_id);
H5_FCDLL int_f h5rcreate_ptr_c(void *ref, hid_t_f *loc_id, _fcd name, int_f *namelen, int_f *ref_type, hid_t_f *space_id);
H5_FCDLL int_f h5rdereference_ptr_c(hid_t_f *obj_id, int_f *ref_type, void *ref, hid_t_f *ref_obj_id);
H5_FCDLL int_f h5rget_region_region_c(hid_t_f *dset_id, int_f *ref, hid_t_f *space_id);
H5_FCDLL int_f h5rget_region_ptr_c(hid_t_f *dset_id, void *ref, hid_t_f *space_id);
H5_FCDLL int_f h5rget_object_type_obj_c(hid_t_f *dset_id, haddr_t_f *ref, int_f *obj_type);
H5_FCDLL int_f h5rget_name_ptr_c(hid_t_f *loc_id, int_f *ref_type, void *ref, _fcd name, size_t_f *name_len, size_t_f *size_default);
H5_FCDLL int_f h5rget_obj_type_c(hid_t_f *loc_id, int_f *ref_type, void *ref, int_f *obj_type);
/*
 * Functions from H5If.c
 */
H5_FCDLL int_f h5iget_type_c(hid_t_f *obj_id, int_f *type);
H5_FCDLL int_f h5iget_name_c(hid_t_f *obj_id, _fcd buf, size_t_f *buf_size, size_t_f *name_size);
H5_FCDLL int_f h5iinc_ref_c(hid_t_f *obj_id, int_f *ref_count);
H5_FCDLL int_f h5idec_ref_c(hid_t_f *obj_id, int_f *ref_count);
H5_FCDLL int_f h5iget_ref_c(hid_t_f *obj_id, int_f *ref_count);
H5_FCDLL int_f h5iget_file_id_c(hid_t_f *obj_id, hid_t_f *file_id);
H5_FCDLL int_f h5iis_valid_c(hid_t_f *obj_id, int_f *c_valid);

/*
 * Functions from H5Ef.c
 */

H5_FCDLL int_f h5eclear_c(hid_t_f *estack_id);
H5_FCDLL int_f h5eprint_c1(_fcd name, int_f* namelen);
H5_FCDLL int_f h5eprint_c2(void);
H5_FCDLL int_f h5eget_major_c(int_f* error_no, _fcd name, size_t_f* namelen);
H5_FCDLL int_f h5eget_minor_c(int_f* error_no, _fcd name, size_t_f* namelen);
H5_FCDLL int_f h5eset_auto2_c(int_f* printflag, hid_t_f *estack_id, H5E_auto2_t func, void *client_data);

/*
 * Functions from H5f.c
 */
H5_FCDLL int_f h5open_c(void);
H5_FCDLL int_f h5close_c(void);
H5_FCDLL int_f h5init_types_c(hid_t_f *types, hid_t_f *floatingtypes, hid_t_f *integertypes);
H5_FCDLL int_f h5close_types_c(hid_t_f *types, int_f *lentypes, hid_t_f *floatingtypes, int_f *floatinglen, hid_t_f *integertypes, int_f *integerlen);
H5_FCDLL int_f h5init_flags_c( int_f *h5d_flags, size_t_f *h5d_size_flags, 
		int_f *h5e_flags, hid_t_f *h5e_hid_flags, int_f *h5f_flags,
                int_f *h5fd_flags, hid_t_f *h5fd_hid_flags,
                int_f *h5g_flags, int_f *h5i_flags, int_f *h5l_flags, int_f *h5o_flags,
                hid_t_f *h5p_flags, int_f *h5p_flags_int, int_f *h5r_flags, 
                int_f *h5s_flags, hid_t_f *h5s_hid_flags, hsize_t_f *h5s_hsize_flags, 
		int_f *h5t_flags, int_f *h5z_flags, int_f *h5_generic_flags,
                haddr_t_f *h5_haddr_generic_flags);
H5_FCDLL int_f h5init1_flags_c(int_f *h5lib_flags);
H5_FCDLL int_f h5get_libversion_c(int_f *majnum, int_f *minnum, int_f *relnum);
H5_FCDLL int_f h5check_version_c(int_f *majnum, int_f *minnum, int_f *relnum);
H5_FCDLL int_f h5garbage_collect_c(void);
H5_FCDLL int_f h5dont_atexit_c(void);

/*
 * Functions from H5Zf.c
 */
H5_FCDLL int_f h5zunregister_c(int_f *filter);
H5_FCDLL int_f h5zfilter_avail_c(int_f *filter, int_f *flag);
H5_FCDLL int_f h5zget_filter_info_c(int_f *filter, int_f *flag);

/*
 * Functions from H5Lf.c
 */

H5_FCDLL int_f h5lcopy_c(hid_t_f *src_loc_id, _fcd src_name, size_t_f *src_namelen, hid_t_f *dest_loc_id,
			  _fcd dest_name, size_t_f *dest_namelen,
			  hid_t_f *lcpl_id, hid_t_f *lapl_id);
H5_FCDLL int_f h5lcreate_external_c(_fcd file_name, size_t_f *file_namelen, _fcd obj_name, size_t_f *obj_namelen,
				     hid_t_f *link_loc_id, _fcd link_name, size_t_f *link_namelen,
				     hid_t_f *lcpl_id, hid_t_f *lapl_id);
H5_FCDLL int_f h5lcreate_hard_c(hid_t_f *obj_loc_id, _fcd obj_name, size_t_f *obj_namelen,
				 hid_t_f *link_loc_id,
				 _fcd link_name, size_t_f *link_namelen,
				 hid_t_f *lcpl_id, hid_t_f *lapl_id );
H5_FCDLL int_f h5lcreate_soft_c(_fcd target_path, size_t_f *target_path_len,
				 hid_t_f *link_loc_id,
				 _fcd link_name, size_t_f *link_name_len,
				 hid_t_f *lcpl_id, hid_t_f *lapl_id );
H5_FCDLL int_f h5ldelete_c( hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id );
H5_FCDLL int_f h5ldelete_by_idx_c(hid_t_f *loc_id, _fcd group_name, size_t_f *group_namelen,
				    int_f *index_field, int_f *order, hsize_t_f *n, hid_t_f *lapl_id);
H5_FCDLL int_f h5lexists_c(hid_t_f *loc_id, _fcd name, size_t_f *namelen, hid_t_f *lapl_id, int_f *link_exists);
H5_FCDLL int_f h5lget_info_c(hid_t_f *link_loc_id, _fcd link_name, size_t_f *link_namelen,
			       int_f *cset, int_f *corder, int_f *corder_valid, int_f *link_type,
			       haddr_t_f *address, size_t_f *val_size,
			       hid_t_f *lapl_id);
H5_FCDLL int_f h5lget_info_by_idx_c(hid_t_f *loc_id, _fcd group_name, size_t_f *group_namelen,
		      int_f *index_field, int_f *order, hsize_t_f *n,
		      int_f *link_type, int_f *corder_valid, int_f *corder, int_f *cset, haddr_t_f *address, size_t_f *val_size, hid_t_f *lapl_id);
H5_FCDLL int_f h5lis_registered_c(int_f *link_cls_id);
H5_FCDLL int_f h5lmove_c(hid_t_f *src_loc_id, _fcd src_name, size_t_f *src_namelen, hid_t_f *dest_loc_id,
			  _fcd dest_name, size_t_f *dest_namelen, hid_t_f *lcpl_id, hid_t_f *lapl_id);
H5_FCDLL int_f h5lget_name_by_idx_c(hid_t_f *loc_id, _fcd group_name, size_t_f *group_namelen,
				     int_f *index_field, int_f *order, hsize_t_f *n,
				     size_t_f *size, _fcd name, hid_t_f *lapl_id);
H5_FCDLL int_f h5lget_val_c(hid_t_f *link_loc_id, _fcd link_name, size_t_f *link_namelen, size_t_f *size,
			     void *linkval_buff, hid_t_f *lapl_id) ;

H5_FCDLL int_f h5literate_c(hid_t_f *group_id, int_f *index_type, int_f *order, hsize_t_f *idx, H5L_iterate_t op, void *op_data );
H5_FCDLL int_f h5literate_by_name_c(hid_t_f *loc_id, _fcd name, size_t_f *namelen, int_f *index_type, int_f *order, hsize_t_f *idx, H5L_iterate_t op, void *op_data, hid_t_f *lapl_id);


#endif /* _H5f90proto_H */
