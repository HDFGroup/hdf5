/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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


#ifndef _H5LTf90proto_H
#define _H5LTf90proto_H

#include "H5public.h"
#include "H5f90i.h"
#include <stdlib.h>
#include <string.h>

/* These definitions should match those in fortran/src/H5f90kit.c */

H5_FCDLL char*  HD5f2cstring (_fcd fdesc, size_t len);
H5_FCDLL void HD5packFstring (char *src, char *dest, size_t len);

/*
 *  Functions from H5DSfc.c
 */
HDF5_HL_F90CSTUBDLL
int_f
h5dsset_scale_c(hid_t_f *dsid, _fcd dimname, size_t_f *dimnamelen);

HDF5_HL_F90CSTUBDLL
int_f
h5dsattach_scale_c( hid_t_f *did, hid_t_f *dsid, int_f *idx);

HDF5_HL_F90CSTUBDLL
int_f
h5dsdetach_scale_c( hid_t_f *did, hid_t_f *dsid, int_f *idx);

HDF5_HL_F90CSTUBDLL
int_f
h5dsis_attached_c( hid_t_f *did, hid_t_f *dsid, int_f *idx, int_f *c_is_attached);

HDF5_HL_F90CSTUBDLL
int_f
h5dsis_scale_c(hid_t_f *did, int_f *is_scale);

HDF5_HL_F90CSTUBDLL
int_f
h5dsset_label_c(hid_t_f *did, int_f *idx, _fcd label, size_t_f *labellen);

HDF5_HL_F90CSTUBDLL
int_f
h5dsget_label_c(hid_t_f *did, int_f *idx, _fcd label, size_t_f *size);

HDF5_HL_F90CSTUBDLL
int_f
h5dsget_scale_name_c(hid_t_f *did, _fcd label, size_t_f *size);

HDF5_HL_F90CSTUBDLL
int_f
h5dsget_num_scales_c( hid_t_f *did, int_f *idx, int_f *num_scales);

/*
 *  Functions from H5LTfc.c
 */

HDF5_HL_F90CSTUBDLL
int_f
h5ltmake_dataset_c (hid_t_f *loc_id,
                     size_t_f *namelen,
                     _fcd name,
                     int_f *rank,
                     hsize_t_f *dims,
                     hid_t_f *type_id,
                     void *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5ltread_dataset_c (hid_t_f *loc_id,
                     size_t_f *namelen,
                     _fcd name,
                     hid_t_f *type_id,
                     void *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5ltset_attribute_c(hid_t_f *loc_id,
		    size_t_f *namelen,
		    _fcd dsetname,
		    size_t_f *attrnamelen,
		    _fcd attrname,
		    size_t_f *size,
		    void *buf, char *dtype, size_t_f *sizeof_val);


HDF5_HL_F90CSTUBDLL
int_f
h5ltget_attribute_c(hid_t_f *loc_id,
                         size_t_f *namelen,
                         _fcd dsetname,
                         size_t_f *attrnamelen,
                         _fcd attrname,
		         void *buf, char* dtype, size_t_f *sizeof_val);

HDF5_HL_F90CSTUBDLL
int_f
h5ltget_attribute_string_c(hid_t_f *loc_id,
			   size_t_f *namelen,
			   _fcd dsetname,
			   size_t_f *attrnamelen,
			   _fcd attrname,
			   _fcd buf,
			   size_t_f *buf_size);

HDF5_HL_F90CSTUBDLL
int_f
h5ltget_dataset_ndims_c(hid_t_f *loc_id,
                         size_t_f *namelen,
                         _fcd name,
                         int_f *rank);

HDF5_HL_F90CSTUBDLL
int_f
h5ltfind_dataset_c(hid_t_f *loc_id,
                    size_t_f *namelen,
                    _fcd name);

HDF5_HL_F90CSTUBDLL
int_f
h5ltget_dataset_info_c(hid_t_f *loc_id,
                        size_t_f *namelen,
                        _fcd name,
                        hsize_t_f *dims,
                        int_f *type_class,
                        size_t_f *type_size);

HDF5_HL_F90CSTUBDLL
int_f
h5ltget_attribute_ndims_c(hid_t_f *loc_id,
                           size_t_f *namelen,
                           _fcd dsetname,
                           size_t_f *attrnamelen,
                           _fcd attrname,
                           int_f *rank);
HDF5_HL_F90CSTUBDLL
int_f
h5ltget_attribute_info_c(hid_t_f *loc_id,
                          size_t_f *namelen,
                          _fcd name,
                          size_t_f *attrnamelen,
                          _fcd attrname,
                          hsize_t_f *dims,
                          int_f *type_class,
                          size_t_f *type_size);

HDF5_HL_F90CSTUBDLL
int_f
h5ltmake_dataset_string_c (hid_t_f *loc_id,
                            size_t_f *namelen,
                            _fcd name,
                            size_t_f *buflen,
                            char *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5ltread_dataset_string_c (hid_t_f *loc_id,
                            size_t_f *namelen,
                            _fcd name,
                            char *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5ltpath_valid_c(hid_t_f *loc_id, 
                  _fcd path, 
                  size_t_f *pathlen, 
                  int_f *check_object_valid_c);

/*-------------------------------------------------------------------------
* Image
*-------------------------------------------------------------------------
*/
HDF5_HL_F90CSTUBDLL
int_f
h5immake_image_8bit_c (hid_t_f *loc_id,
                        size_t_f *namelen,
                        _fcd name,
                        hsize_t_f *width,
                        hsize_t_f *height,
                        int_f *buf);
HDF5_HL_F90CSTUBDLL
int_f
h5imread_image_c (hid_t_f *loc_id,
                   size_t_f *namelen,
                   _fcd name,
                   int_f *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5immake_image_24bit_c (hid_t_f *loc_id,
                         size_t_f *namelen,
                         _fcd name,
                         size_t_f *ilen,
                         _fcd il,
                         hsize_t_f *width,
                         hsize_t_f *height,
                         void *buf);
HDF5_HL_F90CSTUBDLL
int_f
h5imget_image_info_c(hid_t_f *loc_id,
                      size_t_f *namelen,
                      _fcd name,
                      hsize_t_f *width,
                      hsize_t_f *height,
                      hsize_t_f *planes,
                      hsize_t_f *npals,
                      size_t_f *ilen,
                      _fcd interlace);


HDF5_HL_F90CSTUBDLL
int_f
h5imis_image_c(hid_t_f *loc_id,
                size_t_f *namelen,
                _fcd name);


HDF5_HL_F90CSTUBDLL
int_f
h5immake_palette_c (hid_t_f *loc_id,
                     size_t_f *namelen,
                     _fcd name,
                     hsize_t_f *dims,
                     void *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5imlink_palette_c (hid_t_f *loc_id,
                     size_t_f *namelen,
                     _fcd name,
                     size_t_f *ilen,
                     _fcd pal_name);

HDF5_HL_F90CSTUBDLL
int_f
h5imunlink_palette_c (hid_t_f *loc_id,
                       size_t_f *namelen,
                       _fcd name,
                       size_t_f *ilen,
                       _fcd pal_name);

HDF5_HL_F90CSTUBDLL
int_f
h5imget_npalettes_c(hid_t_f *loc_id,
                     size_t_f *namelen,
                     _fcd name,
                     hsize_t_f *npals);


HDF5_HL_F90CSTUBDLL
int_f
h5imget_palette_info_c(hid_t_f *loc_id,
                        size_t_f *namelen,
                        _fcd name,
                        int_f *pal_number,
                        hsize_t_f *dims);

HDF5_HL_F90CSTUBDLL
int_f
h5imget_palette_c(hid_t_f *loc_id,
                   size_t_f *namelen,
                   _fcd name,
                   int_f *pal_number,
                   void *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5imis_palette_c(hid_t_f *loc_id,
                  size_t_f *namelen,
                  _fcd name);



/*-------------------------------------------------------------------------
* Table
*-------------------------------------------------------------------------
*/

HDF5_HL_F90CSTUBDLL
int_f
h5tbmake_table_c(size_t_f *namelen1,
                  _fcd name1,
                  hid_t_f *loc_id,
                  size_t_f *namelen,
                  _fcd name,
                  hsize_t_f *nfields,
                  hsize_t_f *nrecords,
                  size_t_f *type_size,
                  size_t_f *field_offset,
                  hid_t_f *field_types,
                  hsize_t_f *chunk_size,
                  int_f *compress,
                  size_t_f *char_len_field_names, /* field_names lengths */
                  size_t_f *max_char_size_field_names, /* char len of fields */
                  _fcd buf);          /* field_names */

HDF5_HL_F90CSTUBDLL
int_f
h5tbread_table_c(hid_t_f *loc_id,
                  _fcd name,
                  size_t_f *namelen,
		  hsize_t_f *nfields,
                  size_t_f *dst_size,
                  size_t_f *dst_offset,
                  size_t_f *dst_sizes,
		 void *dst_buf);


HDF5_HL_F90CSTUBDLL
int_f
h5tbmake_table_ptr_c(size_t_f *namelen1,
		     _fcd name1,
		     hid_t_f *loc_id,
		     size_t_f *namelen,
		     _fcd name,
		     hsize_t_f *nfields,
		     hsize_t_f *nrecords,
		     size_t_f *type_size,
		     size_t_f *field_offset,
		     hid_t_f *field_types,
		     hsize_t_f *chunk_size,
		     void *fill_data,
		     int_f *compress,
		     size_t_f *char_len_field_names, /* field_names lengths */
		     size_t_f *max_char_size_field_names, /* char len of fields */
		     char *field_names, /* field_names */
		     void *data);

HDF5_HL_F90CSTUBDLL
int_f
h5tbwrite_field_name_c(hid_t_f *loc_id,
                        size_t_f *namelen,
                        _fcd name,
                        size_t_f *namelen1,
                        _fcd field_name,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf);



HDF5_HL_F90CSTUBDLL
int_f
h5tbread_field_name_c(hid_t_f *loc_id,
                       size_t_f *namelen,
                       _fcd name,
                       size_t_f *namelen1,
                       _fcd field_name,
                       hsize_t_f *start,
                       hsize_t_f *nrecords,
                       size_t_f *type_size,
                       void *buf);



HDF5_HL_F90CSTUBDLL
int_f
h5tbwrite_field_index_c(hid_t_f *loc_id,
                         size_t_f *namelen,
                         _fcd name,
                         int_f *field_index,
                         hsize_t_f *start,
                         hsize_t_f *nrecords,
                         size_t_f *type_size,
                         void *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5tbread_field_index_c(hid_t_f *loc_id,
                        size_t_f *namelen,
                        _fcd name,
                        int_f *field_index,
                        hsize_t_f *start,
                        hsize_t_f *nrecords,
                        size_t_f *type_size,
                        void *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5tbinsert_field_c(hid_t_f *loc_id,
                    size_t_f *namelen,
                    _fcd name,
                    size_t_f *namelen1,
                    _fcd field_name,
                    hid_t_f *field_type,
                    int_f *position,
                    void *buf);

HDF5_HL_F90CSTUBDLL
int_f
h5tbdelete_field_c(hid_t_f *loc_id,
                    size_t_f *namelen,
                    _fcd name,
                    size_t_f *namelen1,
                    _fcd field_name);


HDF5_HL_F90CSTUBDLL
int_f
h5tbget_table_info_c(hid_t_f *loc_id,
                      size_t_f *namelen,
                      _fcd name,
                      hsize_t_f *nfields,
                      hsize_t_f *nrecords);

HDF5_HL_F90CSTUBDLL
int_f
h5tbget_field_info_c(hid_t_f *loc_id,
                      size_t_f *namelen,
                      _fcd name,
                      hsize_t_f *nfields,
                      size_t_f *field_sizes,
                      size_t_f *field_offsets,
                      size_t_f *type_size,
                      size_t_f *namelen2,
                      size_t_f *lenmax,
                      _fcd field_names,
                      size_t_f *maxlen_out);    


#endif /* _H5LTf90proto_H */
