/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef NA_SM_H
#define NA_SM_H

#include "na_types.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

#ifdef NA_SM_HAS_UUID
typedef unsigned char na_sm_id_t[16];
#else
typedef long na_sm_id_t;
#endif

/*****************/
/* Public Macros */
/*****************/

/* String length of Host ID */
#ifdef NA_SM_HAS_UUID
#define NA_SM_HOST_ID_LEN 36
#else
#define NA_SM_HOST_ID_LEN 11
#endif

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Get the current host ID (generate a new one if none exists).
 *
 * \param id [IN/OUT]           pointer to SM host ID
 *
 * \return NA_SUCCESS or corresponding NA error code
 */
NA_PUBLIC na_return_t NA_SM_Host_id_get(na_sm_id_t *id);

/**
 * Convert host ID to string. String size must be NA_SM_HOST_ID_LEN + 1.
 *
 * \param id [IN]               SM host ID
 * \param string [IN/OUT]       pointer to string
 *
 * \return NA_SUCCESS or corresponding NA error code
 */
NA_PUBLIC na_return_t NA_SM_Host_id_to_string(na_sm_id_t id, char *string);

/**
 * Convert string to host ID. String size must be NA_SM_HOST_ID_LEN + 1.
 *
 * \param string [IN]           pointer to string
 * \param id [IN/OUT]           pointer to SM host ID
 *
 * \return NA_SUCCESS or corresponding NA error code
 */
NA_PUBLIC na_return_t NA_SM_String_to_host_id(const char *string, na_sm_id_t *id);

/**
 * Copy src host ID to dst.
 *
 * \param dst [IN/OUT]          pointer to destination SM host ID
 * \param src [IN]              source SM host ID
 */
NA_PUBLIC void NA_SM_Host_id_copy(na_sm_id_t *dst, na_sm_id_t src);

/**
 * Compare two host IDs.
 *
 * \param id1 [IN]              SM host ID
 * \param id2 [IN]              SM host ID
 *
 * \return NA_TRUE if equal or NA_FALSE otherwise
 */
NA_PUBLIC na_bool_t NA_SM_Host_id_cmp(na_sm_id_t id1, na_sm_id_t id2);

#ifdef __cplusplus
}
#endif

#endif /* NA_SM_H */
