/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/*
 * This file contains private information about the H5RS module
 */
#ifndef _H5RSprivate_H
#define _H5RSprivate_H

#ifdef LATER
#include "H5RSpublic.h"
#endif /* LATER */

/* Private headers needed by this file */
#include "H5private.h"

/* Typedefs */

/* Typedef for reference counted string */
typedef struct {
    char *s;            /* String to be reference counted */
    unsigned wrapped;   /* Indicates that the string to be ref-counted is not copied */
    unsigned n;         /* Reference count of number of pointers sharing string */
} H5RS_str_t;

/* Macros */

/* Get the pointer to the actual string */
#define H5RS_GET_STR(rs)        ((rs)->s)
#define H5RS_GET_COUNT(rs)      ((rs)->n)

/* Private routines */
H5_DLL H5RS_str_t *H5RS_create(const char *s);
H5_DLL H5RS_str_t *H5RS_wrap(const char *s);
H5_DLL H5RS_str_t *H5RS_own(char *s);
H5_DLL herr_t H5RS_decr(H5RS_str_t *rs);
H5_DLL herr_t H5RS_incr(H5RS_str_t *rs);
H5_DLL H5RS_str_t *H5RS_dup(H5RS_str_t *s);
H5_DLL int H5RS_cmp(const H5RS_str_t *rs1, const H5RS_str_t *rs2);
H5_DLL ssize_t H5RS_len(const H5RS_str_t *rs);

#endif /* _H5STprivate_H */

