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

#define FFORMAT "%-15f %-15f %-15f\n"
#define IFORMAT "%-15d %-15d %-15d\n"
#define CFORMAT "%-16c %-17c\n"
#define SFORMAT "%-16s %-17s\n"
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
 int    m_quiet;   /* quiet mide: no output at all */
 int    m_report;  /* report mode: print the data */
 int    m_verbose; /* verbose mode: print the data, list of objcets, warnings */
 int    d;         /* delta, absolute value to compare */
 double delta;     /* delta value */
 int    p;         /* relative error to compare*/
 double percent;   /* relative error value */
 int    n;         /* count, compare up to count */
 hsize_t count;    /* count value */
 int    err_stat;  /* an error ocurred (1, error, 0, no error) */
 int    cmn_objs;  /* do we have comparable objects */
 int    not_cmp;   /* are the objects comparable */

} diff_opt_t;



/*-------------------------------------------------------------------------
 * public functions
 *-------------------------------------------------------------------------
 */

#ifdef __cplusplus
extern "C" {
#endif

hsize_t  h5diff(const char *fname1,
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


hsize_t diff_dataset( hid_t file1_id,
                      hid_t file2_id,
                      const char *obj1_name,
                      const char *obj2_name,
                      diff_opt_t *options );

hsize_t diff_datasetid( hid_t dset1_id,
                        hid_t dset2_id,
                        const char *obj1_name,
                        const char *obj2_name,
                        diff_opt_t *options );

hsize_t diff( hid_t      file1_id,
              const char *path1,
              hid_t      file2_id,
              const char *path2,
              diff_opt_t *options,
              H5G_obj_t1  type );

hsize_t diff_compare( hid_t file1_id,
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

hsize_t diff_match( hid_t file1_id,
                    int nobjects1,
                    trav_info_t *info1,
                    hid_t file2_id,
                    int nobjects2,
                    trav_info_t *info2,
                    diff_opt_t *options );

hsize_t diff_array( void *_mem1,
                    void *_mem2,
                    hsize_t nelmts,
                    int rank,
                    hsize_t *dims,
                    diff_opt_t *options,
                    const char *name1,
                    const char *name2,
                    hid_t m_type,
                    hid_t container1_id,
                    hid_t container2_id); /* dataset where the reference came from*/


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


hsize_t
diff_attr(hid_t      loc1_id,
          hid_t      loc2_id,
          const char *path1,
          const char *path2,
          diff_opt_t *options
          );


/*-------------------------------------------------------------------------
 * utility functions
 *-------------------------------------------------------------------------
 */

void        print_found(hsize_t nfound);
void        print_type(hid_t type);
const char* diff_basename(const char *name);
const char* get_type(int type);
const char* get_class(H5T_class_t tclass);
const char* get_sign(H5T_sign_t sign);
void        print_dims( int r, hsize_t *d );
void        print_pos( int        *ph,
                       int        per,
                       hsize_t    curr_pos,
                       hsize_t    *acc,
                       hsize_t    *pos,
                       int        rank,
                       const char *obj1,
                       const char *obj2 );

int print_objname(diff_opt_t *options, hsize_t nfound);


#if defined (H5DIFF_DEBUG)
void print_sizes( const char *obj1, const char *obj2,
 hid_t f_type1, hid_t f_type2,
 hid_t m_type1, hid_t m_type2 );
#endif


hsize_t diff_native_uchar(unsigned char *mem1,
                          unsigned char *mem2,
                          hsize_t       i,
                          int           rank,
                          hsize_t       *acc,
                          hsize_t       *pos,
                          diff_opt_t    *options,
                          const char    *obj1,
                          const char    *obj2,
                          int           *ph);


hsize_t diff_char(unsigned char *mem1,
                  unsigned char *mem2,
                  hsize_t       i,
                  int           rank,
                  hsize_t       *acc,
                  hsize_t       *pos,
                  diff_opt_t    *options,
                  const char    *obj1,
                  const char    *obj2,
                  int           *ph);

hsize_t diff_datum(void       *_mem1,
                   void       *_mem2,
                   hid_t      m_type,
                   hsize_t    i,
                   int        rank,
                   hsize_t    *acc,
                   hsize_t    *pos,
                   diff_opt_t *options,
                   const char *obj1,
                   const char *obj2,
                   hid_t      container1_id,
                   hid_t      container2_id, /*where the reference came from*/
                   int        *ph);           /*print header */



hsize_t diff_float(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_double(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_schar(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_uchar(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_short(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_ushort(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_int(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_uint(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_long(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_ulong(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_llong(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);

hsize_t diff_ullong(unsigned char *mem1,
	unsigned char *mem2,
	hsize_t       nelmts,
	int           rank,
	hsize_t       *acc,
	hsize_t       *pos,
	diff_opt_t    *options,
	const char    *obj1,
	const char    *obj2,
	int           *ph);



#endif  /* H5DIFF_H__ */
