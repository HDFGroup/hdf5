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

#ifndef H5TRAV_H__
#define H5TRAV_H__

#include "hdf5.h"

/*-------------------------------------------------------------------------
 * public enum to specify type of an object
 * the TYPE can be:
 *    H5TRAV_TYPE_UNKNOWN = -1,
 *    H5TRAV_TYPE_GROUP,            Object is a group
 *    H5TRAV_TYPE_DATASET,          Object is a dataset
 *    H5TRAV_TYPE_TYPE,             Object is a named datatype
 *    H5TRAV_TYPE_LINK,             Object is a symbolic link
 *    H5TRAV_TYPE_UDLINK,           Object is a user-defined link
 *-------------------------------------------------------------------------
 */
typedef enum {
    H5TRAV_TYPE_UNKNOWN = -1,   /* Unknown object type */
    H5TRAV_TYPE_GROUP,          /* Object is a group */
    H5TRAV_TYPE_DATASET,        /* Object is a dataset */
    H5TRAV_TYPE_NAMED_DATATYPE, /* Object is a named datatype */
    H5TRAV_TYPE_LINK,           /* Object is a symbolic link */
    H5TRAV_TYPE_UDLINK          /* Object is a user-defined link */
} h5trav_type_t;

/*-------------------------------------------------------------------------
 * public struct to store name and type of an object
 *-------------------------------------------------------------------------
 */
typedef struct trav_path_t {
    char      *path;
    h5trav_type_t type;
} trav_path_t;

typedef struct trav_info_t {
    size_t      nalloc;
    size_t      nused;
    trav_path_t *paths;
} trav_info_t;


/*-------------------------------------------------------------------------
 * keep record of hard link information
 *-------------------------------------------------------------------------
 */
typedef struct trav_link_t {
    char      *new_name;
} trav_link_t;


/*-------------------------------------------------------------------------
 * struct to store basic info needed for the h5trav table traversal algorythm
 *-------------------------------------------------------------------------
 */

typedef struct trav_obj_t {
    haddr_t     objno;     /* object address */
    unsigned    flags[2];  /* h5diff.object is present or not in both files*/
    char        *name;     /* name */
    h5trav_type_t type;    /* type of object */
    trav_link_t *links;    /* array of possible link names */
    size_t      sizelinks; /* size of links array */
    size_t      nlinks;    /* number of links */
} trav_obj_t;


/*-------------------------------------------------------------------------
 * private struct that stores all objects
 *-------------------------------------------------------------------------
 */

typedef struct trav_table_t {
    size_t      size;
    size_t      nobjs;
    trav_obj_t *objs;
} trav_table_t;


/*-------------------------------------------------------------------------
 * public functions
 *-------------------------------------------------------------------------
 */

#ifdef __cplusplus
extern "C" {
#endif

/*-------------------------------------------------------------------------
 * "h5trav info" public functions
 *-------------------------------------------------------------------------
 */
int h5trav_getinfo(hid_t file_id, trav_info_t *info);
ssize_t h5trav_getindex(const trav_info_t *info, const char *obj);

/*-------------------------------------------------------------------------
 * "h5trav table" public functions
 *-------------------------------------------------------------------------
 */

int  h5trav_gettable(hid_t fid, trav_table_t *travt);
int  h5trav_getindext(const char *obj, const trav_table_t *travt);

/*-------------------------------------------------------------------------
 * "h5trav print" public functions
 *-------------------------------------------------------------------------
 */
int h5trav_print(hid_t fid);

#ifdef __cplusplus
}
#endif

/*-------------------------------------------------------------------------
 * info private functions
 *-------------------------------------------------------------------------
 */

void trav_info_init(trav_info_t **info);

void trav_info_free(trav_info_t *info);

/*-------------------------------------------------------------------------
 * table private functions
 *-------------------------------------------------------------------------
 */

void trav_table_init(trav_table_t **table);

void trav_table_free(trav_table_t *table);

void trav_table_addflags(unsigned *flags,
                         char *objname,
                         h5trav_type_t type,
                         trav_table_t *table);

#endif  /* H5TRAV_H__ */

