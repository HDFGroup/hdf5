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

/* $Id$ */

/*
 * This file contains private information about the H5P module
 */

#ifndef _H5Pprivate_H
#define _H5Pprivate_H
#include <H5Ppublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Gprivate.h>         /*for H5G_entry_t */

/* Flags to indicate special dataspace features are active */
#define H5P_VALID_MAX   0x01
#define H5P_VALID_PERM  0x02

typedef struct H5P_simple_t {
    intn                    rank;       /*number of dimensions                  */
    intn                    dim_flags;  /*dimension flags                       */
    size_t                 *size;       /*dimension sizes                       */
    size_t                 *max;        /*maximum dimension sizes               */
    intn                   *perm;       /*dimension permutations                */
} H5P_simple_t;

typedef struct {
    H5P_class_t             type;       /*type of dimensionality object         */
    union {
        H5P_simple_t            simple;         /*simple dimensionality information   */
    } u;
} H5P_t;

#define H5P_RESERVED_ATOMS  2

H5P_t                  *H5P_copy(const H5P_t *src);
herr_t                  H5P_close(H5P_t *ds);
size_t                  H5P_get_npoints(const H5P_t *ds);
intn                    H5P_get_ndims(const H5P_t *ds);
intn                    H5P_get_dims(const H5P_t *ds, size_t dims[] /*out */ );
herr_t                  H5P_modify(H5F_t *f, H5G_entry_t *ent, const H5P_t *space);
H5P_t                  *H5P_read(H5F_t *f, H5G_entry_t *ent);
intn                    H5P_cmp(const H5P_t *ds1, const H5P_t *ds2);

hbool_t                 H5P_is_simple(const H5P_t *sdim);
uintn                   H5P_nelem(const H5P_t *space);

#endif
