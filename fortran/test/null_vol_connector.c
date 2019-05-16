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

/* Purpose:     A simple virtual object layer (VOL) connector with almost no
 *              functionality that is used for testing basic plugin handling
 *              (registration, etc.).
 */

#include <stdlib.h>
#include "H5PLextern.h"

#include "null_vol_connector.h"

static void *H5VL_file_create(const char *name);
static herr_t H5VL_file_close(void *file);

/* The VOL class struct */
static const H5VL_class_t null_vol_g = {
    0,                                              /* version      */
    NULL_VOL_CONNECTOR_VALUE,                       /* value        */
    NULL_VOL_CONNECTOR_NAME,                        /* name         */
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
        H5VL_file_create,                           /* create       */
        NULL,                                       /* open         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        H5VL_file_close                             /* close        */
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

typedef struct H5VL_t {
    void   *under_object;
} H5VL_t;

/* These two functions are necessary to load this plugin using
 * the HDF5 library.
 */

H5PL_type_t H5PLget_plugin_type(void) {return H5PL_TYPE_VOL;}
const void *H5PLget_plugin_info(void) {return &null_vol_g;}

static void *
H5VL_file_create(const char *name)
{
    hid_t under_fapl;
    H5VL_t *file;
    
    printf(" H5VL_file_create \n");

    file = (H5VL_t *)calloc(1, sizeof(H5VL_t));

    file->under_object = fopen(name, "w");

    return (void *)file;
}

static herr_t 
H5VL_file_close(void *file)
{
    H5VL_t *f = (H5VL_t *)file;

    fclose(f->under_object);
    free(f);

    return 1;
}

