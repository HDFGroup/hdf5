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

#ifndef H5DIFF_H__
#define H5DIFF_H__

#include "hdf5.h"
#include "h5trav.h"


#if 0
#define H5DIFF_DEBUG
#endif


/*-------------------------------------------------------------------------
 * printf formatting
 *-------------------------------------------------------------------------
 */

#define FFORMAT "%-15.10g %-15.10g %-15.10g\n"
#define IFORMAT "%-15d %-15d %-15d\n"
#define UIFORMAT "%-15u %-15u %-15u\n"
#define LIFORMAT "%-15ld %-15ld %-15ld\n"
#define ULIFORMAT "%-15lu %-15lu %-15lu\n"
/* with -p option */
#define FPFORMAT "%-15.10g %-15.10g %-15.10g %-14.10g\n"
#define IPFORMAT "%-15d %-15d %-15d %-14d\n"
#define UIPFORMAT "%-15u %-15u %-15u %-14u\n"
#define LPIFORMAT "%-15ld %-15ld %-15ld %-14ld\n"
#define ULPIFORMAT "%-15lu %-15lu %-15lu %-14lu\n"
#define SPACES  "          "


/*-------------------------------------------------------------------------
 * command line options
 *-------------------------------------------------------------------------
 */

typedef struct {
 int    r;       /* report only what objects differ */
 int    d;       /* delta */
 double delta;   /* delta value */
 int    p;       /* relative error */
 double percent; /* relative error value */
 int    n;       /* count */
 int    count;   /* count value */
 int    verbose; /* print information */
 int    attr;    /* compare attributes */
} diff_opt_t;



/*-------------------------------------------------------------------------
 * public functions
 *-------------------------------------------------------------------------
 */

#ifdef __cplusplus
extern "C" {
#endif

int  h5diff(const char *fname1, 
            const char *fname2, 
            const char *objname1, 
            const char *objname2, 
            diff_opt_t *options);


#ifdef __cplusplus
}
#endif



/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */


int diff_dataset( hid_t file1_id, 
                  hid_t file2_id, 
                  const char *obj1_name, 
                  const char *obj2_name,
                  diff_opt_t *options );

int diff( hid_t file1_id, 
          const char *obj1_name, 
          hid_t file2_id, 
          const char *obj2_name, 
          diff_opt_t *options, 
          int type );

int diff_compare( hid_t file1_id, 
                  const char *file1_name, 
                  const char *obj1_name, 
                  int nobjects1, 
                  trav_info_t *info1,
                  hid_t file2_id, 
                  const char *file2_name, 
                  const char *obj2_name, 
                  int nobjects2, 
                  trav_info_t *info2,
                  diff_opt_t *options );

int diff_match( hid_t file1_id, 
                int nobjects1, 
                trav_info_t *info1,
                hid_t file2_id, 
                int nobjects2, 
                trav_info_t *info2, 
                diff_opt_t *options );

int diff_array( void *buf1, 
                void *buf2, 
                hsize_t tot_cnt, 
                int rank, 
                hsize_t *dims, 
                diff_opt_t *options, 
                const char *obj1, 
                const char *obj2,
                hid_t m_type );


int diff_can_type( hid_t       f_type1, /* file data type */ 
                   hid_t       f_type2, /* file data type */
                   int         rank1, 
                   int         rank2,
                   hsize_t     *dims1, 
                   hsize_t     *dims2,
                   hsize_t     *maxdim1, 
                   hsize_t     *maxdim2,
                   const char  *obj1_name, 
                   const char  *obj2_name, 
                   diff_opt_t  *options );


int diff_attr(hid_t loc1_id, 
              hid_t loc2_id, 
              diff_opt_t *options
              );


/*-------------------------------------------------------------------------
 * utility functions
 *-------------------------------------------------------------------------
 */

int         diff_can( hid_t type_id );
void        print_type(hid_t type);
const char* diff_basename(const char *name);
const char* get_type(int type);
const char* get_class(H5T_class_t tclass);
const char* get_sign(H5T_sign_t sign);
void        print_dims( int r, hsize_t *d );
void        print_pos( int *ph, int p, unsigned int curr_pos, int *acc, 
             int *pos, int rank, const char *obj1, const char *obj2 );

#if defined (H5DIFF_DEBUG)
void print_sizes( const char *obj1, const char *obj2,
 hid_t f_type1, hid_t f_type2,
 hid_t m_type1, hid_t m_type2 );
#endif

#ifdef NOT_YET
void diff_list( const char *filename, int nobjects, trav_info_t *info );
#endif /* NOT_YET */



#endif  /* H5DIFF_H__ */
