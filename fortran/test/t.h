
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

#include "../../src/hdf5.h"
#include "../src/H5f90i.h"

/*
 *  Functions from t.c
 */
#ifdef DF_CAPFNAMES
#   define nh5_fixname_c              FNAME(H5_FIXNAME_C)
#   define nh5_cleanup_c              FNAME(H5_CLEANUP_C)
#else                                              /* !DF_CAPFNAMES */
#   define nh5_fixname_c              FNAME(h5_fixname_c)
#   define nh5_cleanup_c              FNAME(h5_cleanup_c)
#endif                                             /* DF_CAPFNAMES */

 H5_DLL int_f nh5_fixname_c 
(_fcd base_name, int_f *base_namelen, hid_t_f *fapl, _fcd full_name, int_f *full_namelen);

 H5_DLL int_f nh5_cleanup_c 
(_fcd base_name, int_f *base_namelen, hid_t_f *fapl);
