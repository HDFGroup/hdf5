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
 * Programmer:	James Laird <jlaird@ncsa.uiuc.edu>
 *		Monday, October 9, 2006
 *
 * Purpose:	This file contains public declarations for the H5SM
 *              shared object header messages module.
 */
#ifndef _H5SMpublic_H
#define _H5SMpublic_H

/* Flags indicating which kinds of object header messages a given SOHM index
 * holds.
 * Pass these flags in using the mesg_type_flags array in
 * H5P_set_shared_mesgs.
 * (Developers: These flags correspond to object header message type_ids,
 * but we need to assign each kind of message to a different bit so that
 * one index can hold multiple types.)
 */
#define H5SM_NONE_FLAG     0x0000          /* No shared messages */
#define H5SM_SDSPACE_FLAG  0x0001          /* Simple Dataspace Message.  */
#define H5SM_DTYPE_FLAG	   0x0002          /* Datatype Message.  */
#define H5SM_FILL_FLAG     0x0004          /* Fill Value Message. */
#define H5SM_PLINE_FLAG	   0x0008          /* Filter pipeline message.  */
#define H5SM_ATTR_FLAG	   0x0010          /* Attribute Message.  */
#define H5SM_ALL_FLAG      (H5SM_SDSPACE_FLAG | H5SM_DTYPE_FLAG | H5SM_FILL_FLAG | H5SM_PLINE_FLAG | H5SM_ATTR_FLAG)

#endif /*_H5SMpublic_H*/

