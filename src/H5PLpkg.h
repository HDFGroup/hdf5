/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5PL_PACKAGE
#error "Do not include this file outside the H5PL package!"
#endif

#ifndef _H5PLpkg_H
#define _H5PLpkg_H

/* Include private header file */
#include "H5PLprivate.h"       

#define   MAX_PATH_NUM              16

/****************************/
/* Local typedefs */
/****************************/

typedef struct H5PL_table_t {
    H5PL_type_t pl_type;			/* plugin type	     */
    int         pl_id;                          /* ID for the plugin */
    void        *handle;			/* plugin handle     */
} H5PL_table_t;

/****************************/
/* Local variables */
/****************************/

static size_t		H5PL_table_alloc_g = 0;
static size_t		H5PL_table_used_g = 0;
static H5PL_table_t     *H5PL_table_g = NULL;

static char             *path_table[MAX_PATH_NUM];
static size_t           num_paths = 0;
static htri_t           path_found = FALSE;

/******************************/
/* Package Private Prototypes */
/******************************/

/* Function prototypes for H5PL package scope */
htri_t H5PL_find(H5PL_type_t plugin_type, int type_id, char *dir, void **info);
htri_t H5PL_search_table(H5PL_type_t plugin_type, int type_id, void **info);

#endif /* _H5PLpkg_H */

