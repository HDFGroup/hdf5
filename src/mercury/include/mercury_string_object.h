/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_STRING_OBJECT_H
#define MERCURY_STRING_OBJECT_H

#include "mercury_types.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

typedef struct hg_string_object {
    char *    data;
    hg_bool_t is_const;
    hg_bool_t is_owned;
} hg_string_object_t;

/*****************/
/* Public Macros */
/*****************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize a string object.
 *
 * \param string [OUT]          pointer to string structure
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_string_object_init(hg_string_object_t *string);

/**
 * Initialize a string object from the string pointed to by s.
 *
 * \param string [OUT]          pointer to string structure
 * \param s [IN]                pointer to string
 * \param is_owned [IN]         boolean
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_string_object_init_char(hg_string_object_t *string, char *s, hg_bool_t is_owned);

/**
 * Initialize a string object from the const string pointed to by s.
 *
 * \param string [OUT]          pointer to string structure
 * \param s [IN]                pointer to string
 * \param is_owned [IN]         boolean
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_string_object_init_const_char(hg_string_object_t *string, const char *s,
                                                       hg_bool_t is_owned);

/**
 * Free a string object.
 *
 * \param string [IN/OUT]       pointer to string structure
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_string_object_free(hg_string_object_t *string);

/**
 * Duplicate a string object.
 *
 * \param string [IN]           pointer to string structure
 * \param new_string [OUT]      pointer to string structure
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_string_object_dup(hg_string_object_t string, hg_string_object_t *new_string);

/**
 * Exchange the content of the string structure by the content of s.
 *
 * \param string [IN/OUT]       pointer to string structure
 *
 * \return Pointer to string contained by string before the swap
 */
HG_PUBLIC char *hg_string_object_swap(hg_string_object_t *string, char *s);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_STRING_OBJECT_H */
