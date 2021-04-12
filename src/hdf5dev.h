/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This is the main public HDF5 include file for component authors.
 */

#ifndef _HDF5DEV_H
#define _HDF5DEV_H

/* Include general purpose application developer interfaces */
#include "hdf5.h"

/* Application developer headers for various interfaces */
#include "H5ESdevelop.h" /* Event Sets */
#include "H5FDdevelop.h" /* File drivers */
#include "H5Idevelop.h"  /* ID management */
#include "H5Ldevelop.h"  /* Links */
#include "H5PLextern.h"  /* Plugins */
#include "H5Tdevelop.h"  /* Datatypes */
#include "H5TSdevelop.h" /* Threadsafety */
#include "H5Zdevelop.h"  /* Data filters */

/* Virtual object layer (VOL) connector developer support */
#include "H5VLconnector.h"          /* VOL connector author routines */
#include "H5VLconnector_passthru.h" /* Pass-through VOL connector author routines */
#include "H5VLnative.h"             /* Native VOL connector macros, for VOL connector authors */

#endif /* _HDF5DEV_H */
