/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/*
 * This file contains function prototypes for each exported function in
 * the H5I module.
 */
#ifndef _H5Ipublic_H
#define _H5Ipublic_H

/* Public headers needed by this file */
#include <H5public.h>

/*
 * Group values allowed.  Start with `1' instead of `0' because it makes the
 * tracing output look better when hid_t values are large numbers.
 */
typedef enum {
    BADGROUP             = (-1),/*invalid Group                              */
    H5_FILE              = 1,   /*group ID for File objects                  */
    H5_TEMPLATE_0,              /*group ID for Template objects              */
    H5_TEMPLATE_1,              /*group ID for Template objects              */
    H5_TEMPLATE_2,              /*group ID for Template objects              */
    H5_TEMPLATE_3,              /*group ID for Template objects              */
    H5_TEMPLATE_4,              /*group ID for Template objects              */
    H5_TEMPLATE_5,              /*group ID for Template objects              */
    H5_TEMPLATE_6,              /*group ID for Template objects              */
    H5_TEMPLATE_7,              /*group ID for Template objects              */
#ifndef NDEBUG
    H5_TEMPLATE_MAX,            /*not really a group ID                      */
#endif
    H5_GROUP,                   /*group ID for Group objects                 */
    H5_DATATYPE,                /*group ID for Datatype objects              */
    H5_DATASPACE,               /*group ID for Dataspace objects             */
    H5_DATASET,                 /*group ID for Dataset objects               */
    H5_ATTR,                    /*group ID for Attribute objects             */
    H5_TEMPBUF,                 /*group ID for Temporary buffer objects      */
    H5_RAGGED,			/*group ID for Ragged array objects	     */
    MAXGROUP               /*highest group in group_t (Invalid as true group)*/
} H5I_group_t;

/* Type of atoms to return to users */
typedef int hid_t;

#ifdef __cplusplus
extern "C" {
#endif


#ifdef __cplusplus
}
#endif
#endif
