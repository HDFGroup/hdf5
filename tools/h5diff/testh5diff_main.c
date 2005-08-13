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

#include "testh5diff.h"




int main(int UNUSED argc, const UNUSED char *argv[])
{

 test_basic ("file1.h5","file2.h5");
 test_types ("file3.h5",NULL);
 test_native("file4.h5",NULL);

 /* generate 2 files with attribute differences */
 test_attr("file5.h5",0);
 test_attr("file6.h5",1);

 /* generate 2 files with all datatype differences */
 test_dsetall("file7.h5",0);
 test_dsetall("file8.h5",1);
 return 0;
}

