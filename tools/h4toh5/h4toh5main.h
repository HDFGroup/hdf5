/*-------------------------------------------------------------------------
 *
 * Copyright (C) 2000   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 */

/******************************************************************************

Description: 

1. converter

See HDF4 to HDF5 mapping specification at
(http://hdf.ncsa.uiuc.edu/HDF5/papers/h4toh5) for the default mapping 
from HDF4 object to HDF5 object.
 
The whole converter includes 10 files, h4toh5util.h, h4toh5main.h, h4toh5util.c, h4toh5main.c, h4toh5sds.c, h4toh5image.c,h4toh5vdata.c,h4toh5vgroup.c,h4toh5pal.c and h4toh5anno.c.

2. this file 

including declarations of subroutines of all .c files excluding h4toh5util.c.

Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/


#ifndef H4TOH5MAIN_H
#define H4TOH5MAIN_H
#include "hdf.h"
#include "mfhdf.h"
#include "hdf5.h"
#include "h4toh5util.h"
#include <fcntl.h>
#include <errno.h>
#endif

/* For windows support.*/
#if WIN32
typedef unsigned int mode_t;
#endif

#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode)&0xF000) == S_IFDIR)
#endif

/* subroutines to check initial settings and inputting parameters.
Adapted from h5toh4 tools and used for h4toh5main.c */

void PrintOptions_h4toh5(void);
int test_file(char *filename,int oflag,mode_t mode);
int test_dir(char *);
char *BuildFilename(char *filename, char *ext);

/* subroutines for h4toh5main.c */
int h4toh5(char*,char*);
int get_numof_hdf4obj(char*,int32);
int set_hashtables(void);
int set_helpgroups(hid_t,hid_t*,hid_t*);
int h4toh5lonevds(int32,hid_t);
int h4toh5lonevgs(int32,int32,hid_t,hid_t,hid_t);
int h4toh5vgrings(int32,int32,hid_t,hid_t,hid_t);
int h4toh5unvisitedimages(int32,hid_t,hid_t);
int h4toh5unvisitedsds(int32,int32,hid_t,hid_t);
void free_allhashmemory(void);

/*subroutines for h4toh5vgroup.c*/

int Vgroup_h4_to_h5(int32,int32,int32,hid_t,hid_t,hid_t);
int convert_vgroup(int32,int32, int32,char* ,hid_t,hid_t,hid_t);
int convert_vdata(int32,int32,char*,hid_t);
int convert_sds(int32,int32,int32,char*,hid_t,hid_t);
int convert_image(int32,int32,char*,hid_t,hid_t);

/*subroutines for h4toh5vdata.c*/

int Vdata_h4_to_h5(int32,int32,hid_t);
int vdata_transattrs(int32,hid_t,int,int,char*);
int gen_h5comptype(int32,int32,size_t *,size_t*,hid_t*,hid_t*,hid_t,hid_t);

/* subroutines for h4toh5sds.c*/
int Sds_h4_to_h5(int32,int32,hid_t,hid_t);
int sds_transattrs(int32, hid_t,int,int);
int sdsdim_to_h5dataset(int32,int32,hid_t,hid_t,int32);


/*subroutines for h4toh5image.c*/
int Image_h4_to_h5(int32,int32,hid_t,hid_t); 
int gr_tranattrs(int32, hid_t,int,int);
int gr_palette(int32,int32,hid_t,hid_t);
int create_pal_objref(hid_t ,hid_t ,char *);

/*subroutines for h4toh5anno.c*/
char* trans_tag_name(int32,ann_type);
int Annofil_h4_to_h5(int32,hid_t);
int Annoobj_h4_to_h5(int32,int32,int32,hid_t);

/*subroutines for h4toh5pal.c*/
int Palette_h4_to_h5(int32,int32 ,hid_t,char *);







