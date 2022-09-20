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

/* Purpose:     A simple virtual object layer (VOL) connector with almost no
 *              functionality that is used for testing basic plugin handling
 *              (registration, etc.).
 */

/* For HDF5 plugin functionality */
#include "H5PLextern.h"

/* This connector's header */
#include "null_vol_connector.h"

/* The VOL class struct */
static const H5VL_class_t null_vol_g = {
    H5VL_VERSION,             /* VOL class struct version */
    NULL_VOL_CONNECTOR_VALUE, /* value            */
    NULL_VOL_CONNECTOR_NAME,  /* name             */
    0,                        /* connector version */
    H5VL_CAP_FLAG_NONE,       /* capability flags */
    NULL,                     /* initialize       */
    NULL,                     /* terminate        */
    {
        /* info_cls */
        (size_t)0, /* size             */
        NULL,      /* copy             */
        NULL,      /* compare          */
        NULL,      /* free             */
        NULL,      /* to_str           */
        NULL,      /* from_str         */
    },
    {
        /* wrap_cls */
        NULL, /* get_object       */
        NULL, /* get_wrap_ctx     */
        NULL, /* wrap_object      */
        NULL, /* unwrap_object    */
        NULL, /* free_wrap_ctx    */
    },
    {
        /* attribute_cls */
        NULL, /* create           */
        NULL, /* open             */
        NULL, /* read             */
        NULL, /* write            */
        NULL, /* get              */
        NULL, /* specific         */
        NULL, /* optional         */
        NULL  /* close            */
    },
    {
        /* dataset_cls */
        NULL, /* create           */
        NULL, /* open             */
        NULL, /* read             */
        NULL, /* write            */
        NULL, /* get              */
        NULL, /* specific         */
        NULL, /* optional         */
        NULL  /* close            */
    },
    {
        /* datatype_cls */
        NULL, /* commit           */
        NULL, /* open             */
        NULL, /* get_size         */
        NULL, /* specific         */
        NULL, /* optional         */
        NULL  /* close            */
    },
    {
        /* file_cls */
        NULL, /* create           */
        NULL, /* open             */
        NULL, /* get              */
        NULL, /* specific         */
        NULL, /* optional         */
        NULL  /* close            */
    },
    {
        /* group_cls */
        NULL, /* create           */
        NULL, /* open             */
        NULL, /* get              */
        NULL, /* specific         */
        NULL, /* optional         */
        NULL  /* close            */
    },
    {
        /* link_cls */
        NULL, /* create           */
        NULL, /* copy             */
        NULL, /* move             */
        NULL, /* get              */
        NULL, /* specific         */
        NULL  /* optional         */
    },
    {
        /* object_cls */
        NULL, /* open             */
        NULL, /* copy             */
        NULL, /* get              */
        NULL, /* specific         */
        NULL  /* optional         */
    },
    {
        /* introspect_cls */
        NULL, /* get_conn_cls     */
        NULL, /* get_cap_flags    */
        NULL, /* opt_query        */
    },
    {
        /* request_cls */
        NULL, /* wait             */
        NULL, /* notify           */
        NULL, /* cancel           */
        NULL, /* specific         */
        NULL, /* optional         */
        NULL  /* free             */
    },
    {
        /* blob_cls */
        NULL, /* put              */
        NULL, /* get              */
        NULL, /* specific         */
        NULL  /* optional         */
    },
    {
        /* token_cls */
        NULL, /* cmp              */
        NULL, /* to_str           */
        NULL  /* from_str         */
    },
    NULL /* optional         */
};

/* These two functions are necessary to load this plugin using
 * the HDF5 library.
 */

H5PL_type_t
H5PLget_plugin_type(void)
{
    return H5PL_TYPE_VOL;
}
const void *
H5PLget_plugin_info(void)
{
    return &null_vol_g;
}
