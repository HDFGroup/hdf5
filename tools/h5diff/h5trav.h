/*
 * Copyright (c) 2001 National Center for Supercomputing Applications
 *                    All rights reserved.
 *
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


/* public methods */
int H5get_object_info( hid_t file_id, info_t *info );



/*struct to store basic info about an object */
typedef struct obj_t {
    unsigned long objno[2];
    char *objname;
    int displayed;
				int recorded;
} obj_t;

/*struct that stores all objects, excluding shared objects */
typedef struct table_t {
	int size;
	int nobjs;
	obj_t *objs;
} table_t;


#ifdef __cplusplus
}
#endif


#endif  /* H5TRAV_H__ */
