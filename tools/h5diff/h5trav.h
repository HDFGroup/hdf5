/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Pedro Vicente, pvn@ncsa.uiuc.edu
 *              Monday, 4. November 2002
 */
#ifndef H5TRAV_H__
#define H5TRAV_H__


#ifdef __cplusplus
extern "C" {
#endif


/*struct to store name and type of an object */
typedef struct info_t {
	char *name;
	int type;
} info_t;


/*struct to store basic info about an object */
typedef struct obj_t {
    unsigned long objno[2];
    char *objname;
    int displayed;
				int type;
} obj_t;

/*struct that stores all objects, excluding shared objects */
typedef struct table_t {
	int size;
	int nobjs;
	obj_t *objs;
} table_t;


/* public methods */
int H5get_object_info( hid_t file_id, info_t *info );

/* table methods */
void table_init(table_t **table);
void table_free(table_t *table);
int  table_search(unsigned long *objno, table_t *table );
void table_add(unsigned long *objno, char *objname, int type, table_t *table);
void info_free(info_t *info, int nobjs);




#ifdef __cplusplus
}
#endif


#endif  /* H5TRAV_H__ */
