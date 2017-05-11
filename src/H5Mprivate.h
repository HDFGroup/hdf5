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

/*-------------------------------------------------------------------------
 *
 * Created:             H5Mprivate.h
 *
 * Purpose:             Library-visible declarations.
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5Mprivate_H
#define _H5Mprivate_H

/* Include package's public header */
#include "H5Mpublic.h"

/* Private headers needed by this file */
#include "H5private.h"		/* Generic Functions			*/

/*
 * Library prototypes...  These are the ones that other packages routinely
 * call.
 */
H5_DLL herr_t H5M_init(void);

#endif /* _H5Mprivate_H */
