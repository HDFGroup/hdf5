/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5P (property list) developer
 *      support routines.
 */

#ifndef _H5Pdevelop_H
#define _H5Pdevelop_H

/* Include package's public header */
#include "H5Ppublic.h"

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

/**
 *
 * \ingroup  DXPL
 *
 * \brief Sets "new API context" flag for plugin API wrapper call.
 *
 * \dxpl_id{plist_id}
 * \param[in]  new_api_ctx  Indicate that API wrapper should create new API context
 * \return \herr_t
 *
 * \details Set flag to indicate that an API wrapper for a plugin's
 *              public wrapper API call (e.g. H5VLfile_create, H5FDopen, etc)
 *              should open a new API context for the API call.
 *
 */
H5_DLL herr_t H5Pset_plugin_new_api_context(hid_t plist_id, hbool_t new_api_ctx);

/**
 * \ingroup  DXPL
 *
 * \brief Gets "new API context" flag for plugin API wrapper call.
 *
 * \dxpl_id{plist_id}
 * \param[out]  new_api_ctx  Flag indicating API wrapper should create new API context
 * \return \herr_t
 *
 * \details Retrieve "new API context" flag for plugin wrapper API calls.
 *
 */
H5_DLL herr_t H5Pget_plugin_new_api_context(hid_t plist_id, hbool_t *new_api_ctx);

#ifdef __cplusplus
}
#endif

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#endif /* _H5Pdevelop_H */
