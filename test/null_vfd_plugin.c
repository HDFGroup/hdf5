/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Purpose:     A simple Virtual File Driver (VFD) with almost no functionality
 *              that is used for testing basic VFD plugin handling (registration, etc.).
 */

/* Public HDF5 headers */
#include "hdf5.h"

/* For HDF5 plugin functionality */
#include "H5PLextern.h"

/* This driver's header */
#include "null_vfd_plugin.h"

/* Prototypes */
static H5FD_t *H5FD_null_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static herr_t  H5FD_null_close(H5FD_t *_file);
static herr_t  H5FD_null_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                              void *buf);
static herr_t  H5FD_null_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                               const void *buf);
static haddr_t H5FD_null_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD_null_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_null_get_eof(const H5FD_t *_file, H5FD_mem_t type);

static const H5FD_class_t H5FD_null_g = {
    H5FD_CLASS_VERSION,  /* struct version  */
    NULL_VFD_VALUE,      /* value           */
    NULL_VFD_NAME,       /* name            */
    1,                   /* maxaddr         */
    H5F_CLOSE_WEAK,      /* fc_degree       */
    NULL,                /* terminate       */
    NULL,                /* sb_size         */
    NULL,                /* sb_encode       */
    NULL,                /* sb_decode       */
    0,                   /* fapl_size       */
    NULL,                /* fapl_get        */
    NULL,                /* fapl_copy       */
    NULL,                /* fapl_free       */
    0,                   /* dxpl_size       */
    NULL,                /* dxpl_copy       */
    NULL,                /* dxpl_free       */
    H5FD_null_open,      /* open            */
    H5FD_null_close,     /* close           */
    NULL,                /* cmp             */
    NULL,                /* query           */
    NULL,                /* get_type_map    */
    NULL,                /* alloc           */
    NULL,                /* free            */
    H5FD_null_get_eoa,   /* get_eoa         */
    H5FD_null_set_eoa,   /* set_eoa         */
    H5FD_null_get_eof,   /* get_eof         */
    NULL,                /* get_handle      */
    H5FD_null_read,      /* read            */
    H5FD_null_write,     /* write           */
    NULL,                /* read_vector     */
    NULL,                /* write_vector    */
    NULL,                /* read_selection  */
    NULL,                /* write_selection */
    NULL,                /* flush           */
    NULL,                /* truncate        */
    NULL,                /* lock            */
    NULL,                /* unlock          */
    NULL,                /* del             */
    NULL,                /* ctl             */
    H5FD_FLMAP_DICHOTOMY /* fl_map          */
};

static H5FD_t *
H5FD_null_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    (void)name;
    (void)flags;
    (void)fapl_id;
    (void)maxaddr;

    return NULL;
}

static herr_t
H5FD_null_close(H5FD_t *_file)
{
    (void)_file;

    return 0;
}

static herr_t
H5FD_null_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size, void *buf)
{
    (void)_file;
    (void)type;
    (void)dxpl_id;
    (void)addr;
    (void)size;
    (void)buf;

    return 0;
}

static herr_t
H5FD_null_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size, const void *buf)
{
    (void)_file;
    (void)type;
    (void)dxpl_id;
    (void)addr;
    (void)size;
    (void)buf;

    return 0;
}

static haddr_t
H5FD_null_get_eoa(const H5FD_t *_file, H5FD_mem_t type)
{
    (void)_file;
    (void)type;

    return HADDR_UNDEF;
}

static herr_t
H5FD_null_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr)
{
    (void)_file;
    (void)type;
    (void)addr;

    return 0;
}

static haddr_t
H5FD_null_get_eof(const H5FD_t *_file, H5FD_mem_t type)
{
    (void)_file;
    (void)type;

    return HADDR_UNDEF;
}

/* These two functions are necessary to load this plugin using
 * the HDF5 library.
 */

H5PL_type_t
H5PLget_plugin_type(void)
{
    return H5PL_TYPE_VFD;
}

const void *
H5PLget_plugin_info(void)
{
    return &H5FD_null_g;
}
