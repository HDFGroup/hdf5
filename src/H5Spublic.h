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
 * This file contains public declarations for the H5S module.
 */
#ifndef _H5pproto_H
#define _H5Sproto_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Ipublic.h>

/* Define atomic datatypes */
#define H5S_ALL         (-2)
#define H5S_UNLIMITED	((hsize_t)(hssize_t)(-1))

/* Different types of dataspaces */
typedef enum H5S_class_t {
    H5S_NO_CLASS         = -1,  /*error                                      */
    H5S_SCALAR           = 0,   /*scalar variable                            */
    H5S_SIMPLE           = 1,   /*simple data space                          */
    H5S_COMPLEX          = 2    /*complex data space                         */
} H5S_class_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5S.c */
hid_t H5Screate_simple (int rank, const hsize_t dims[],
			const hsize_t maxdims[]);
hid_t H5Scopy (hid_t space_id);
herr_t H5Sclose (hid_t space_id);
hsize_t H5Sget_npoints (hid_t space_id);
int H5Sget_ndims (hid_t space_id);
int H5Sget_dims (hid_t space_id, hsize_t dims[]);
hbool_t H5Sis_simple (hid_t space_id);
herr_t H5Sset_space (hid_t space_id, int rank, const hsize_t *dims);
herr_t H5Sset_hyperslab(hid_t sid, const hssize_t *start, const hsize_t *count,
			const hsize_t *stride);
int H5Sget_hyperslab (hid_t sid, hssize_t offset[]/*out*/,
		      hsize_t size[]/*out*/, hsize_t stride[]/*out*/);

#ifdef __cplusplus
}
#endif
#endif
