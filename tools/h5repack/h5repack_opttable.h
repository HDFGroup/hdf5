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


#ifndef H5REPACK_OPTTABLE_H__
#define H5REPACK_OPTTABLE_H__


#include "h5repack.h"


#ifdef __cplusplus
extern "C" {
#endif


int          options_table_init( pack_opttbl_t **tbl );
int          options_table_free( pack_opttbl_t *table );
int          options_add_chunk ( obj_list_t *obj_list,
                                 int n_objs,
                                 hsize_t *chunk_lengths,
                                 int chunk_rank,
                                 pack_opttbl_t *table );
int          options_add_comp  ( obj_list_t *obj_list,
                                 int n_objs,
                                 comp_info_t comp,
                                 pack_opttbl_t *table );
pack_info_t* options_get_object( const char *path,
                                 pack_opttbl_t *table);


#ifdef __cplusplus
}
#endif


#endif  /* H5REPACK_OPTTABLE_H__ */
