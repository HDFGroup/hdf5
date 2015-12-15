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

#ifndef _H5LDpublic_H
#define _H5LDpublic_H

#ifdef __cplusplus
extern "C" {
#endif

H5_HLDLL herr_t H5LDget_dset_dims(hid_t did, hsize_t *cur_dims);
H5_HLDLL size_t H5LDget_dset_type_size(hid_t did, const char *fields);
H5_HLDLL herr_t H5LDget_dset_elmts(hid_t did, const hsize_t *prev_dims,
    const hsize_t *cur_dims, const char *fields, void *buf);

#ifdef __cplusplus
}
#endif

#endif /* _H5LDpublic_H */

