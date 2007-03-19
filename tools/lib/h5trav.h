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
 * public struct to store name and type of an object
 * the TYPE can be:
 *    H5G_UNKNOWN = -1,
 *    H5G_GROUP,            Object is a group
 *    H5G_DATASET,          Object is a dataset
 *    H5G_TYPE,             Object is a named data type
 *    H5G_LINK,             Object is a symbolic link
 *-------------------------------------------------------------------------
 */

/* hack to alow v1.4 compability */
#ifdef H5_WANT_H5_V1_4_COMPAT
 typedef int H5G_obj_t1;
#else
 typedef H5G_obj_t H5G_obj_t1;
#endif

typedef struct trav_info_t {
    char      *name;
    H5G_obj_t1 type;
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
    haddr_t     objno;     /* object number from H5Gget_objinfo */
    unsigned    flags[2];  /* h5diff.object is present or not in both files*/
    char        *name;     /* name */
    int         displayed; /* hard link already traversed once */
    H5G_obj_t1  type;      /* type of object */
    trav_link_t *links;    /* array of possible link names */
    int         sizelinks; /* size of links array */
    unsigned    nlinks;    /* number of links */
} trav_obj_t;


/*-------------------------------------------------------------------------
 * private struct that stores all objects
 *-------------------------------------------------------------------------
 */

typedef struct trav_table_t {
    unsigned        size;
    unsigned        nobjs;
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
int  h5trav_getinfo( hid_t fid, trav_info_t *info, int print );
int  h5trav_getindex( const char *obj, int nobjs, trav_info_t *info );
void h5trav_freeinfo( trav_info_t *info, int nobjs );
void h5trav_printinfo(int nobjs, trav_info_t *info);

/*-------------------------------------------------------------------------
 * "h5trav table" public functions
 *-------------------------------------------------------------------------
 */

int  h5trav_getindext(const char *obj,trav_table_t *travt);
int  h5trav_gettable(hid_t fid, trav_table_t *travt);
void h5trav_printtable(trav_table_t *table);

#ifdef __cplusplus
}
#endif

/*-------------------------------------------------------------------------
 * table private functions
 *-------------------------------------------------------------------------
 */

void trav_table_init(trav_table_t **table);

void trav_table_free(trav_table_t *table);

int  trav_table_search(haddr_t objno,
                       trav_table_t *table );

void trav_table_add(haddr_t objno,
                    char *objname,
                    H5G_obj_t1 type,
                    trav_table_t *table);

void trav_table_addflags(unsigned *flags,
                         char *objname,
                         H5G_obj_t1 type,
                         trav_table_t *table);


void trav_table_addlink(trav_table_t *table,
                        int j /* the object index */,
                        char *path );


#endif  /* H5TRAV_H__ */


