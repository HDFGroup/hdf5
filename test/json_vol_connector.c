/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Purpose:     A virtual object layer (VOL) connector that uses JSON to
 *              store HDF5 data and metadata.
 */

/* Until we get a platform-independence library working, we'll cheat and
 * use H5private.h functionality as needed.
 */
#include "H5private.h"

#include "H5PLextern.h"

#include "json_vol_connector.h"

/* The Jansson JSON parser */
#include <jansson.h>

/* Callbacks */
/* File */
static void *jvc_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *jvc_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
static herr_t jvc_file_close(void *file, hid_t dxpl_id, void **req);

/* The VOL class struct */
static const H5VL_class_t json_vol_g = {
    0,                                              /* version      */
    JSON_VOL_CONNECTOR_VALUE,                       /* value        */
    JSON_VOL_CONNECTOR_NAME,                        /* name         */
    0,                                              /* capability flags */
    NULL,                                           /* initialize   */
    NULL,                                           /* terminate    */
    (size_t)0,                                      /* info size    */
    NULL,                                           /* info copy    */
    NULL,                                           /* info compare */
    NULL,                                           /* info free    */
    NULL,                                           /* info to str  */
    NULL,                                           /* str to info  */
    NULL,                                           /* get_object   */
    NULL,                                           /* get_wrap_ctx */
    NULL,                                           /* wrap_object  */
    NULL,                                           /* free_wrap_ctx */
    {   /* attribute_cls */
        NULL,                                       /* create       */
        NULL,                                       /* open         */
        NULL,                                       /* read         */
        NULL,                                       /* write        */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* dataset_cls */
        NULL,                                       /* create       */
        NULL,                                       /* open         */
        NULL,                                       /* read         */
        NULL,                                       /* write        */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* datatype_cls */
        NULL,                                       /* commit       */
        NULL,                                       /* open         */
        NULL,                                       /* get_size     */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* file_cls */
        jvc_file_create,                            /* create       */
        jvc_file_open,                              /* open         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        jvc_file_close                              /* close        */
    },
    {   /* group_cls */
        NULL,                                       /* create       */
        NULL,                                       /* open         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* link_cls */
        NULL,                                       /* create       */
        NULL,                                       /* copy         */
        NULL,                                       /* move         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL                                        /* optional     */
    },
    {   /* object_cls */
        NULL,                                       /* open         */
        NULL,                                       /* copy         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL                                        /* optional     */
    },
    {   /* request_cls */
        NULL,                                       /* wait         */
        NULL,                                       /* notify       */
        NULL,                                       /* cancel       */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* free         */
    },
    NULL                                            /* optional     */
};

/* These two functions are necessary to load this plugin using
 * the HDF5 library.
 */
H5PL_type_t H5PLget_plugin_type(void) {return H5PL_TYPE_VOL;}
const void *H5PLget_plugin_info(void) {return &json_vol_g;}

/******************/
/* IMPLEMENTATION */
/******************/

typedef struct json_vol_file_t {
    FILE *fp;                /* File pointer to JSON file */
} json_vol_file_t;

/* File callback implementation */
static void *
jvc_file_create(const char *name, unsigned H5_ATTR_UNUSED flags, hid_t H5_ATTR_UNUSED fcpl_id,
        hid_t H5_ATTR_UNUSED fapl_id, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    json_vol_file_t *jfile = NULL;

    /* Set up */
    if(NULL == (jfile = (json_vol_file_t *)HDcalloc((size_t)1, sizeof(json_vol_file_t))))
        goto error;

    /* Create the file */
    if(NULL == (jfile->fp = HDfopen(name, "w")))
        goto error;

    return (void *)jfile;

error:
    if(jfile->fp)
        HDfclose(jfile->fp);
    if(jfile)
        HDfree(jfile);
    return NULL;
} /* end jvc_file_create() */

static void *
jvc_file_open(const char *name, unsigned H5_ATTR_UNUSED flags, hid_t H5_ATTR_UNUSED fapl_id,
        hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    json_vol_file_t *jfile = NULL;

    /* Set up */
    if(NULL == (jfile = (json_vol_file_t *)HDcalloc((size_t)1, sizeof(json_vol_file_t))))
        goto error;

    /* Open the file */
    if(NULL == (jfile->fp = HDfopen(name, "r+")))
        goto error;

    return (void *)jfile;

error:
    if(jfile->fp)
        HDfclose(jfile->fp);
    if(jfile)
        HDfree(jfile);
    return NULL;
} /* end jvc_file_open() */

static herr_t
jvc_file_close(void *file, hid_t H5_ATTR_UNUSED dxpl_id, void H5_ATTR_UNUSED **req)
{
    json_vol_file_t *jfile = (json_vol_file_t *)file;

    /* Close the file */
    if(EOF == HDfclose(jfile->fp))
        goto error;

    /* Tear down */
    HDfree(jfile);

    return SUCCEED;

error:
    return FAIL;
} /* end jvc_file_close() */

