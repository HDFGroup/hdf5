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
 * This file contains public declarations for the H5VL (VOL) module.
 */

#ifndef _H5VLpublic_H
#define _H5VLpublic_H

/* Public headers needed by this file */
#include "H5public.h"           /* Generic Functions                    */
#include "H5Ipublic.h"          /* IDs                                  */

/*****************/
/* Public Macros */
/*****************/

/* VOL connector identifier values
 * These are H5VL_class_value_t values, NOT hid_t values!
 */
#define H5_VOL_INVALID  (-1)    /* Invalid ID for VOL connector ID */
#define H5_VOL_NATIVE   0       /* Native HDF5 file format VOL connector */
#define H5_VOL_RESERVED 256     /* VOL connector IDs below this value are reserved for library use */
#define H5_VOL_MAX      65535   /* Maximum VOL connector ID */


/*******************/
/* Public Typedefs */
/*******************/


/*
 * VOL connector identifiers.  Values 0 through 255 are for connectors defined
 * by the HDF5 library.  Values 256 through 511 are available for testing new
 * filters.  Subsequent values should be obtained from the HDF5 development
 * team at help@hdfgroup.org.
 */
typedef int H5VL_class_value_t;


/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/* The H5VL types uses in the API calls are not opaque - they are defined in
 * H5VLconnector.h, which is included at the top of this file.
 */
H5_DLL hid_t H5VLregister_connector_by_name(const char *connector_name, hid_t vipl_id);
H5_DLL hid_t H5VLregister_connector_by_value(H5VL_class_value_t connector_value, hid_t vipl_id);
H5_DLL htri_t H5VLis_connector_registered(const char *name);
H5_DLL hid_t H5VLget_connector_id(const char *name);
H5_DLL ssize_t H5VLget_connector_name(hid_t id, char *name/*out*/, size_t size);
H5_DLL herr_t H5VLclose(hid_t connector_id);
H5_DLL herr_t H5VLunregister_connector(hid_t connector_id);


#ifdef __cplusplus
}
#endif

/* Semi-public headers mainly for VOL connector authors */
#include "H5VLconnector.h"              /* VOL connector author routines */
#include "H5VLconnector_passthru.h"     /* Pass-through VOL connector author routines */
#include "H5VLnative.h"                 /* Native VOL connector macros, for VOL connector authors */

#endif /* _H5VLpublic_H */

