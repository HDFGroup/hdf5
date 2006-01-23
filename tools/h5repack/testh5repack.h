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


#ifndef TESTH5REPACK_H__
#define TESTH5REPACK_H__

/*-------------------------------------------------------------------------
 * tests
 *-------------------------------------------------------------------------
 */

#define FNAME0     "test0.h5"
#define FNAME0OUT  "test0out.h5"
#define FNAME1     "test1.h5"
#define FNAME1DST  "test1_dst.h5"
#define FNAME1OUT  "test1out.h5"
#define FNAME2     "test2.h5"
#define FNAME2OUT  "test2out.h5"
#define FNAME3     "test3.h5"
#define FNAME3OUT  "test3out.h5"
#define FNAME4     "test4.h5"
#define FNAME4OUT  "test4out.h5"
#define FNAME5     "test5.h5"
#define FNAME5OUT  "test5out.h5"
#define FNAME6     "test6.h5"
#define FNAME7     "test_szip.h5"
#define FNAME8     "test_deflate.h5"
#define FNAME9     "test_shuffle.h5"
#define FNAME10    "test_fletcher32.h5"
#define FNAME11    "test_all.h5"
#define FNAME7OUT  "test_szipout.h5"
#define FNAME8OUT  "test_deflateout.h5"
#define FNAME9OUT  "test_shuffleout.h5"
#define FNAME10OUT "test_fletcher32out.h5"
#define FNAME11OUT "test_allout.h5"
#define FNAME12    "test_nbit.h5"
#define FNAME12OUT "test_nbitout.h5"
#define FNAME13    "test_scaleoffset.h5"
#define FNAME13OUT "test_scaleoffsetout.h5"

int make_testfiles(void);

int write_dset( hid_t loc_id,
                int rank,
                hsize_t *dims,
                const char *dset_name,
                hid_t type_id,
                void *buf );
int write_attr(hid_t loc_id,
               int rank,
               hsize_t *dims,
               const char *attr_name,
               hid_t type_id,
               void *buf);
void write_attr_in(hid_t loc_id,
                   const char* dset_name, /* for saving reference to dataset*/
                   hid_t fid, /* for reference create */
                   int make_diffs /* flag to modify data buffers */);
void write_dset_in(hid_t loc_id,
                   const char* dset_name, /* for saving reference to dataset*/
                   hid_t file_id,
                   int make_diffs /* flag to modify data buffers */);



/*-------------------------------------------------------------------------
 * tests utils
 *-------------------------------------------------------------------------
 */
int make_dset(hid_t loc_id,
              const char *name,
              hid_t sid,
              hid_t dcpl,
              void *buf);

int make_attr(hid_t loc_id,
               int rank,
               hsize_t *dims,
               const char *attr_name,
               hid_t type_id,
               void *buf);


#endif  /* TESTH5REPACK_H__ */

