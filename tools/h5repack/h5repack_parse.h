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


#ifndef H5REPACK_PARSE_H__
#define H5REPACK_PARSE_H__


#include "h5repack.h"


/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */

int         parse_number(char *str);
obj_list_t* parse_comp(char *str, int *n_objs, comp_info_t *comp);
char*       get_scomp(int code);
obj_list_t* parse_chunk(char *str, int *n_objs, hsize_t *chunk_lengths, int *chunk_rank);


#endif  /* H5REPACK_PARSE_H__ */
