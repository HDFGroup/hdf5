
/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/


#ifndef _H5Lite_H
#define _H5Lite_H


herr_t H5Lmake_dataset( hid_t loc_id, 
                        const char *dset_name, 
                        int rank, 
                        const hsize_t *dims,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        const void *buffer );

herr_t H5Lattach_attribute( hid_t loc_id, 
                        const char *dset_name, 
                        const char *attr_name,
                        const char *attr_data );


herr_t H5Lattach_attribute_numerical( hid_t loc_id, 
                        const char *dset_name, 
                        const char *attr_name,
                        hsize_t dim,
                        hid_t file_type_id,
                        hid_t mem_type_id,
                        void *buffer );

herr_t H5Lmake_groups( hid_t loc_id, 
                       int ngroups,
                       const char *names[] );

herr_t H5Lget_groups( hid_t loc_id,
                      const char *group_name );

herr_t H5Lget_attributes( hid_t loc_id );





#endif
