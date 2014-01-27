/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * This file contains function prototypes for each exported function in the
 * H5TR module.
 */
#ifndef _H5TRpublic_H
#define _H5TRpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/
#define H5TR_START_NUM_PEERS_NAME "number_of_peers_name"

/*******************/
/* Public Typedefs */
/*******************/

/********************/
/* Public Variables */
/********************/


#ifdef __cplusplus
extern "C" {
#endif

#ifdef H5_HAVE_EFF

/*********************/
/* Public Prototypes */
/*********************/

/* API wrappers */
H5_DLL hid_t H5TRcreate(hid_t file_id, hid_t rc_id, uint64_t trans_num);
H5_DLL herr_t H5TRget_trans_num(hid_t trans_id, uint64_t *trans_num);
H5_DLL herr_t H5TRget_version_num(hid_t trans_id, uint64_t *version);
H5_DLL herr_t H5TRstart(hid_t trans_id, hid_t trspl_id, hid_t estack_id);
H5_DLL herr_t H5TRfinish(hid_t trans_id, hid_t trfpl_id, hid_t *rcntxt_id, hid_t estack_id);
H5_DLL herr_t H5TRskip(hid_t file_id, uint64_t start_trans_num, uint64_t count, hid_t estack_id);
H5_DLL herr_t H5TRset_dependency(hid_t trans_id, uint64_t trans_num, hid_t estack_id);
H5_DLL herr_t H5TRabort(hid_t trans_id, hid_t estack_id);
H5_DLL herr_t H5TRclose(hid_t trans_id);

H5_DLL herr_t H5Pset_trspl_num_peers(hid_t trspl_id, unsigned num_peers);
H5_DLL herr_t H5Pget_trspl_num_peers(hid_t trspl_id, unsigned *num_peers);

#endif /* H5_HAVE_EFF */

#ifdef __cplusplus
}
#endif
#endif /* _H5TRpublic_H */
