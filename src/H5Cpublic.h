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
 * This file contains function prototypes for each exported function in the
 * H5C module.
 */
#ifndef _H5Cpublic_H
#define _H5Cpublic_H

/* Default Template for creation, access, etc. templates */
#define H5C_DEFAULT     (-2)

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>
#include <H5Dpublic.h>

/* Template classes */
typedef enum H5C_class_t {
    H5C_NO_CLASS = -1,          /* Error return value                   */
    H5C_FILE_CREATE = 0,        /* File creation template               */
    H5C_FILE_ACCESS = 1,        /* File access template                 */
    H5C_DATASET_CREATE = 2,     /* Dataset creation template            */
    H5C_DATASET_XFER = 3,       /* Dataset transfer template            */

    H5C_NCLASSES = 4            /* This must be last!                   */
} H5C_class_t;

#ifdef __cplusplus
extern                  "C" {
#endif

/* Public functions */
    hid_t                   H5Ccreate(H5C_class_t type);
    herr_t                  H5Cclose(hid_t template);
    hid_t                   H5Ccopy(hid_t template);

    H5C_class_t             H5Cget_class(hid_t template);

    herr_t                  H5Cget_version(hid_t template, int *boot /*out */ , int *heap /*out */ ,
                               int *freelist /*out */ , int *stab /*out */ ,
                                           int *shhdr /*out */ );

    herr_t                  H5Cset_userblock(hid_t template, size_t size);
    herr_t                  H5Cget_userblock(hid_t template, size_t *size);

    herr_t                  H5Cset_sizes(hid_t template, size_t sizeof_addr, size_t sizeof_size);
    herr_t                  H5Cget_sizes(hid_t template, size_t *sizeof_addr /*out */ ,
                                         size_t *sizeof_size /*out */ );

    herr_t                  H5Cset_sym_k(hid_t template, int ik, int lk);
    herr_t                  H5Cget_sym_k(hid_t template, int *ik /*out */ , int *lk /*out */ );

    herr_t                  H5Cset_istore_k(hid_t template, int ik);
    herr_t                  H5Cget_istore_k(hid_t template, int *ik /*out */ );

    herr_t                  H5Cset_layout(hid_t template, H5D_layout_t layout);
    H5D_layout_t            H5Cget_layout(hid_t template);

    herr_t                  H5Cset_chunk(hid_t template, int ndims, size_t dim[]);
    int                     H5Cget_chunk(hid_t template, int max_ndims, size_t dim[] /*out */ );

#ifdef __cplusplus
}

#endif
#endif
