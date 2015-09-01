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

/*
 * This file contains private information about the H5R module
 */
#ifndef _H5Rprivate_H
#define _H5Rprivate_H

/* Include package's public header */
#include "H5Rpublic.h"

/* Private headers needed by this file */
#include "H5Gprivate.h"

/* Internal data structures */

/* Private functions */
H5_DLL herr_t H5R_create(void *ref, H5G_loc_t *loc, const char *name,
        hid_t dxpl_id, H5R_type_t ref_type, ...);

#endif  /* _H5Rprivate_H */

