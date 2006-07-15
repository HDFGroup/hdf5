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

/*
 * This file contains private information about the H5DL module
 * for dealing with links in an HDF5 file.
 */
#ifndef _H5Lprivate_H
#define _H5Lprivate_H

/* Include package's public header */
#include "H5Lpublic.h"

/* Private headers needed by this file */
#include "H5Gprivate.h"
#include "H5Oprivate.h"

/* Definitions for creating intermediate groups */
#define H5L_CRT_INTERMEDIATE_GROUP_NAME         "intermediate_group"
#define H5L_CRT_INTERMEDIATE_GROUP_SIZE         sizeof(unsigned)
#define H5L_CRT_INTERMEDIATE_GROUP_DEF          0


/* Functions that understand link messages */
/* forward reference for later use */
struct H5HL_t; /* defined in H5HLprivate.h */

H5_DLL herr_t H5L_link(H5G_loc_t *new_loc, const char *new_name,
    H5G_loc_t *obj_loc, hid_t dxpl, hid_t lcpl_id);
H5_DLL hid_t H5L_get_default_lcpl(void);

#endif /* _H5Lprivate_H */
