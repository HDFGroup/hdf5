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
 * the H5A module.
 */
#ifndef _H5Apublic_H
#define _H5Apublic_H

/* Public headers needed by this file */
#include <H5public.h>

/* Group values allowed */
typedef enum {
    BADGROUP             = (-1),/*invalid Group                              */
    H5_FILE              = 0,   /*group ID for File objects                  */
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
    H5_DIRECTORY,               /*group ID for Directory objects             */
    MAXGROUP               /*highest group in group_t (Invalid as true group)*/
} group_t;

/* Type of atoms to return to users */
typedef int32 hid_t;

/* Type of the function to compare objects & keys */
typedef intn (*H5Asearch_func_t) (const void * obj, const void * key);

/* # of bits to use for Group ID in each atom (change if MAXGROUP>16) */
#define GROUP_BITS  8
#define GROUP_MASK  0xFF

/* # of bits to use for the Atom index in each atom (assumes 8-bit bytes) */
#define ATOM_BITS   ((sizeof(hid_t)*8)-GROUP_BITS)
#define ATOM_MASK   0x0FFFFFFF

/* Combine a Group number and an atom index into an atom */
#define MAKE_ATOM(g,i)      ((((hid_t)(g)&GROUP_MASK)<<ATOM_BITS)|      \
                             ((hid_t)(i)&ATOM_MASK))

#ifdef __cplusplus
extern "C" {
#endif


#ifdef __cplusplus
}
#endif
#endif
