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
 * This file contains public declarations for authoring VOL connectors
 * which act as "passthrough" connectors that forward their API calls to
 * an underlying connector. 
 *
 * An example of this might be a logging connector, which creates log messages
 * and then passes the call on to an underlying VOL connector.
 *
 * The functionality required to implement such a connector is specialized
 * and non-trivial so it has been split into this header in an effort to keep
 * the H5VLpublic_dev.h header easier to understand.
 */

#ifndef _H5VLconnector_passthru_H
#define _H5VLconnector_passthru_H

/* Public headers needed by this file */
#include "H5public.h"           /* Generic Functions                    */
#include "H5Ipublic.h"          /* IDs                                  */
#include "H5VLpublic.h"         /* Virtual Object Layer                 */

/* Semi-public headers mainly for VOL connector authors */
#include "H5VLconnector.h"


/*****************/
/* Public Macros */
/*****************/

/*******************/
/* Public Typedefs */
/*******************/

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/* Helper routines for VOL connector authors */
H5_DLL herr_t H5VLcmp_connector_cls(int *cmp, hid_t connector_id1, hid_t connector_id2);
H5_DLL hid_t H5VLwrap_register(void *obj, H5I_type_t type);
H5_DLL herr_t H5VLretrieve_lib_state(void **state);
H5_DLL herr_t H5VLrestore_lib_state(const void *state);
H5_DLL herr_t H5VLreset_lib_state(void);
H5_DLL herr_t H5VLfree_lib_state(void *state);

/* Pass-through callbacks */
H5_DLL void *H5VLget_object(void *obj, hid_t connector_id);
H5_DLL herr_t H5VLget_wrap_ctx(void *obj, hid_t connector_id, void **wrap_ctx);
H5_DLL void *H5VLwrap_object(void *obj, H5I_type_t obj_type, hid_t connector_id,
    void *wrap_ctx);
H5_DLL void *H5VLunwrap_object(void *obj, hid_t connector_id);
H5_DLL herr_t H5VLfree_wrap_ctx(void *wrap_ctx, hid_t connector_id);


#ifdef __cplusplus
}
#endif

#endif /* _H5VLconnector_passthru_H */

