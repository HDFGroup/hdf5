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

Including declarations of global variables,global hashtables,constant
and utility subroutines of h4toh5util.c

Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/


#ifndef UTILITY_H
#define UTILITY_H
#include "hdf5.h"
#include "hdf.h"

/**********************************************/
/*************** section I *******************/
/*This section of the file describes reserved
name and global parameters used in h4-h5 
converter.*/
/*********************************************/

/* 0. if "/" is found in hdf4 object name, we will use another
   character "_" to replace it, since "/" is a reserved symbol for hdf5. */

#define ORI_SLASH '/'
#define CHA_SLASH '_'

/* 1. character string used for default attribute name. */
#define ATTR "ATTR"

/* 2. Predefined HDF5 Attribute name for HDF4 file */

#define HDF4_OBJECT_NAME    "HDF4_OBJECT_NAME"
#define HDF4_OBJECT_TYPE    "HDF4_OBJECT_TYPE"
#define HDF4_REF_NUM        "HDF4_REF_NUM"
#define HDF4_VGROUP_CLASS   "HDF4_VGROUP_CLASS"
#define HDF4_IMAGE_CLASS    "HDF4_IMAGE_CLASS"
#define HDF4_IMAGE_SUBCLASS "HDF4_IMAGE_SUBCLASS"
#define HDF4_PALETTE_CLASS  "HDF4_PALETTE_CLASS"
#define HDF4_PALETTE_TYPE   "PAL_TYPE"
#define PAL_TYPE            "STANDARD8"

/* 3. reserved name for HDF5 object name when meeting with name clashing. */

#define HDF4_VGROUP         "HDF4_VGROUP"
#define HDF4_PALETTE        "HDF4_PALETTE"
#define HDF4_SDS            "HDF4_SDS"
#define HDF4_VDATA          "HDF4_VDATA"
#define HDF4_IMAGE          "HDF4_IMAGE"
#define HDF4_DIMSCALE       "HDF4_DIMSCALE"

/* 4. global palette and dimension_list name. */
#define HDF4_IMAGE_PALETTE  "HDF4_IMAGE_PALETTE"
#define HDF4_DIMENSION_LIST "HDF4_DIMENSION_LIST"
#define PALETTE             "PALETTE"

#define DIMSCALE            "DIMSCALE"

/* 5. define affix GLO for sds and image file attributes. these file attributes
 will be put under root group. */
#define GLOSDS "GLOSDS"
#define GLOIMAGE  "GLOIMAGE"

/* 6. define HDF object label.*/
#define SDSLABEL    "SDS"
#define VDATALABEL  "Vdata"
#define VGROUPLABEL "Vgroup"
#define GRLABEL     "GR"
#define RAST8LABEL  "raster8"
#define RAST24LABEL "raster24"
#define PALABEL     "palette"

/* 7. define "IMAGE" CLASS required by image spec. */
#define IM_CLASS    "IMAGE"

/* 8. reserved group name for HDF4 dimensional scale and palette. */
#define HDF4_DIMG           "/HDF4_DIMGROUP"
#define HDF4_PALG           "/HDF4_PALGROUP"

/* 9. reserved name for hdf4 file label,file description, object label,
   object description. */
#define HDF4_FILE_LABEL     "HDF4_FILE_LABEL"
#define HDF4_FILE_DESC      "HDF4_FILE_DESCRIPTION"
#define HDF4_OBJECT_LABEL   "HDF4_OBJECT_LABEL"
#define HDF4_OBJECT_DESC    "HDF4_OBJECT_DESCRIPTION"
#define HDF4_SDS_LABEL      "HDF4_SDS_LABEL"
#define HDF4_SDS_DESC       "HDF4_SDS_DESC"
#define HDF4_IMAGE_LABEL    "HDF4_IMAGE_LABEL"
#define HDF4_IMAGE_DESC     "HDF4_IMAGE_DESC"
#define HDF4_VDATA_LABEL    "HDF4_VDATA_LABEL"
#define HDF4_VDATA_DESC     "HDF4_VDATA_DESC"
#define HDF4_VGROUP_LABEL   "HDF4_VGROUP_LABEL"
#define HDF4_VGROUP_DESC    "HDF4_VGROUP_DESC"
#define HDF4_PAL_LABEL      "HDF4_PAL_LABEL"
#define HDF4_PAL_DESC       "HDF4_PAL_DESC"
#define HDF4_IMAGE_INDEXED  "HDF4_IMAGE_INDEXED"

/*10. palette and dimensional scale hash size and the
  maximum length of object reference number in string format.
  global variables of vgroup, vdata, sds, image and total number of
  the object, number of global sds attributes and GR attributes.*/

#define PAL_HASHSIZE   64
#define DIM_HASHSIZE   64
#define VG_DEFHASHSIZE 64
#define VD_DEFHASHSIZE 64
#define MAXREF_LENGTH  5
/*considering the string size of HDF4_DIMGROUP. we add this into 276.*/
#define MAX_DIM_NAME   276

extern int32 estnum_vg;
extern int32 estnum_vd;
extern int32 num_sds;
extern int32 num_images;
extern int   num_objects;
extern int32 num_glsdsattrs;
extern int32 num_glgrattrs;

/**********************************************/
/*************** section II *******************/
/*This section  describes hash tables and their
  functions used in h4-h5 converter.*/
/*********************************************/
/*define two kinds of hashtables. 
  1. struct table uses object reference as the key to handle whether this
  object is visited or not.
  2. struct name_table uses object name as the key to handle name clashings and
  dimensional scale dataset.
*/
  
struct table {
  int ref;
  struct table *next;
  char *name;
};

struct name_table {
  char *name;
  struct name_table *next;
};

extern struct table* sds_hashtab;
extern struct table* gr_hashtab;
extern struct table* vg_hashtab;
extern struct table* vd_hashtab;
extern struct table* pal_hashtab;
extern struct name_table* name_hashtab;
extern struct name_table* dim_hashtab;

/* routine for zeroing out the memory. */
void h4toh5_ZeroMemory(void*s,size_t n);

/* look-up table, object reference is the key.*/
int lookup(int,int,struct table*);

/*look-up table, key is name. */
int hash_fun(char*name,int size);
int lookup_name(char*, int,struct name_table*);

/* routines that initialize the tables and name tables.*/
int init_tab(int,struct table*);
int init_nametab(int,struct name_table*);

/* get name and set name for table. */
char* get_name(int,int,struct table *,int*);
int   set_name(int,int,struct table *,char*);

/* free table routines. */
int   freetable(int,struct table *);
int   freenametable(int, struct name_table*);
void  freehashmemory(void);

/**********************************************/
/*************** section III *******************/
/*This section  describes other common routines and their
  functions used in h4-h5 converter.*/
/*********************************************/

/* this routine defines the conversion of data type from h4 to h5. */
herr_t h4type_to_h5type(const int32 h4type, hid_t* h5memtype, 
			size_t* h4memsize, size_t* h4size, hid_t *h5type);

/* routines for translating predefined hdf4 attributes into hdf5 attributes*/
int h4_transpredattrs(hid_t ,const char *,char*data);
int h4_transnumattr(hid_t h5g,const char *,uint16 group_ref);
int vg_transattrs(int32,hid_t);

/*string and int conversion routines.*/
hid_t mkstr(int size, H5T_str_t pad);
herr_t h5string_to_int(const int32, hid_t*,const size_t,hid_t* );
int conv_int_str(uint16, char*);

/* these routines were utility functions for other routines at h4toh5util.c */
char* trans_obj_name(int32,int32);
char* get_obj_aboname(char*,char*,char*,const char*);
char* make_objname_no(char*,char*,const char*);
char* make_objname_yes(char*,char*);
char* correct_name(char*);

#endif




