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



#ifndef H5REPACK_LIST_H__
#define H5REPACK_LIST_H__

#include "h5repack.h"
#include "h5trav.h"


/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */


int get_objlist(const char* infname, 
                pack_opt_t *options);

int copy_file(const char* fnamein, 
              const char* fnameout,
              pack_opt_t *options);

void print_objlist(const char *filename, 
                   int nobjects, 
                   trav_info_t *info );

int do_copy_file(hid_t fidin, 
                 hid_t fidout, 
                 int nobjects, 
                 trav_info_t *info,
                 pack_opt_t *options);

int copy_attr(hid_t loc_in, 
              hid_t loc_out, 
              pack_opt_t *options
              );





#endif  /* H5REPACK_LIST_H__ */
