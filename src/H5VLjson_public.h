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
 * Programmer:  Frank Willmore
 *              December, 2017
 *
 * Purpose: The public header file for the JSON VOL plugin.
 */

#ifndef H5VLjson_public_H
#define H5VLjson_public_H

#ifdef __cplusplus
extern "C" {
#endif

#define PLUGIN_DEBUG

H5_DLL herr_t H5VLjson_init(void);
H5_DLL herr_t H5VLjson_term(void);
H5_DLL herr_t H5Pset_fapl_json_vol(hid_t fapl_id);
//FTWH5_DLL herr_t H5Pset_fapl_json_vol(hid_t fapl_id, const char *URL, const char *username, const char *password);

#ifdef __cplusplus
}
#endif

#endif /* H5VLjson_public_H */
