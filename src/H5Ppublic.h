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
 * This file contains public declarations for the H5P module.
 */
#ifndef _H5pproto_H
#define _H5Pproto_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

/* Define atomic datatypes */
#define H5P_ALL         (-2)

/* Different types of dataspaces */
typedef enum H5P_class_t {
    H5P_NO_CLASS         = -1,  /*error                                      */
    H5P_SCALAR           = 0,   /*scalar variable                            */
    H5P_SIMPLE           = 1,   /*simple data space                          */
    H5P_COMPLEX          = 2    /*complex data space                         */
} H5P_class_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5P.c */
hid_t H5Pcreate_simple (int rank, size_t dims[]);
herr_t H5Pclose (hid_t space_id);
size_t H5Pget_npoints (hid_t space_id);
int H5Pget_ndims (hid_t space_id);
int H5Pget_dims (hid_t space_id, size_t dims[]);
hbool_t H5Pis_simple (hid_t space_id);
herr_t H5Pset_space (hid_t space_id, int rank, const size_t *dims);
herr_t H5Pset_hyperslab(hid_t sid, const int *start, const int *count,
			const int *stride);
int H5Pget_hyperslab (hid_t sid, int offset[]/*out*/,
		      int size[]/*out*/, int stride[]/*out*/);

#ifdef __cplusplus
}
#endif
#endif
