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


#ifndef H5REPACK_H__
#define H5REPACK_H__

#include "hdf5.h"

#define PFORMAT  "%-7s %-7s %-7s\n" /*chunk info, compression info, name*/
#define PFORMAT1 "%-7s %-7s %-7s"     /*chunk info, compression info, name*/

#define MAX_NC_NAME 256 /* max length of a name */
#define MAX_VAR_DIMS 32 /* max per variable dimensions */

#if 1
#define H5_REPACK_DEBUG
#endif


/*-------------------------------------------------------------------------
 * data structures for command line options
 *-------------------------------------------------------------------------
 */

/* a list of names */
typedef struct {
 char obj[MAX_NC_NAME]; 
} obj_list_t;

/* 
 the type of compression and additional parameter 
 type can be one of the filters
 H5Z_FILTER_NONE    0,  uncompress if compressed
 H5Z_FILTER_DEFLATE	1 , deflation like gzip	   
 H5Z_FILTER_SZIP    4 , szip compression 
*/
typedef struct {
 int type;
 int info;
} comp_info_t;

/* chunk lengths along each dimension and rank */
typedef struct {
 hsize_t chunk_lengths[MAX_VAR_DIMS]; 
 int     rank;
} chunk_info_t;

/* information for one object, contains PATH, CHUNK info and COMP info */
typedef struct {
 char         path[MAX_NC_NAME];            /* name of object */
 comp_info_t  comp;                         /* compression information */
 chunk_info_t chunk;                        /* chunk information */
} pack_info_t;

/* store a table of all objects */
typedef struct {
 int        size;
 int        nelems;
 pack_info_t *objs;
} pack_opttbl_t;


/*-------------------------------------------------------------------------
 * command line options
 *-------------------------------------------------------------------------
 */

/* all the above, ready to go to the hrepack call */
typedef struct {
 pack_opttbl_t   *op_tbl;     /*table with all -c and -t options */
 int             all_chunk;   /*chunk all objects, input of "*" */
 int             all_comp;    /*comp all objects, input of "*" */
 comp_info_t     comp_g;      /*global compress INFO for the ALL case */
 chunk_info_t    chunk_g;     /*global chunk INFO for the ALL case */
 int verbose;                 /*verbose mode */
 int trip;                    /*which cycle are we in */
	int threshold;               /*minimum size to compress, in bytes */
 
} pack_opt_t;



/*-------------------------------------------------------------------------
 * public functions
 *-------------------------------------------------------------------------
 */

#ifdef __cplusplus
extern "C" {
#endif

int h5repack         (char* infile, char* outfile, pack_opt_t *options);
int h5repack_addcomp (char* str, pack_opt_t *options);
int h5repack_addchunk(char* str, pack_opt_t *options);
int h5repack_init    (pack_opt_t *options, int verbose);
int h5repack_end     (pack_opt_t *options);

#ifdef __cplusplus
}
#endif

/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */


void read_info(char *filename,pack_opt_t *options);



#endif  /* H5REPACK_H__ */
