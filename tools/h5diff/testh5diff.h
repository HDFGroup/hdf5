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

#include <stdio.h>
#include <stdlib.h>
#include "hdf5.h"
#include "H5private.h"


int test_basic(const char *file1,
               const char *file2);

int test_types(const char *file1,
               const char *file2);

int test_native(const char *file1,
                const char *file2);


int test_dsetall(const char *file,
                 int make_diffs /* flag to modify data buffers */);

int test_attr(const char *file,
              int make_diffs /* flag to modify data buffers */);


int write_attr(hid_t loc_id,
               int rank,
               hsize_t *dims,
               const char *attr_name,
               hid_t type_id,
               void *buf);


int write_dset( hid_t loc_id,
                int rank,
                hsize_t *dims,
                const char *dset_name,
                hid_t type_id,
                void *buf );


