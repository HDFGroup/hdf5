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

#include "h5test.h"

#define TESTFILE1   "test_filters.h5"
#define TESTFILE2   "filter_error.h5"
#define DSETNAME    "dataset_with_filter"

/* Temporary filter IDs used for testing */
#define H5Z_FILTER_BOGUS	305

/* Local prototypes for filter functions */
static size_t filter_bogus(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);


/*-------------------------------------------------------------------------
 * Function:	create_file_with_bogus_filter
 *
 * Purpose:	Create a dataset with the fletcher filter.
 *	        This function is used to create the test file `test_filters.h5' 
 *              which has a dataset with the "fletcher" I/O filter.  This dataset 
 *              will be used to verify the correct behavior of the library in 
 *              the test "dsets"
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:  Pedro Vicente <pvn@ncsa.uiuc.edu>
 *              Thursday, March 25, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters_endianess(void)
{
#if defined H5_HAVE_FILTER_FLETCHER32
    hid_t     fid = -1;              /* file ID */
    hid_t     dsid = -1;             /* dataset ID */
    hid_t     sid = -1;              /* dataspace ID */
    hid_t     dcpl = -1;             /* dataset creation property list ID */
    hsize_t   dims[1] = {20};        /* dataspace dimensions */
    hsize_t   chunk_dims[1] = {10};  /* chunk dimensions */
    int       buf[20];
    int       rank = 1;
    int       i;

    for(i = 0; i < 20; i++)
        buf[i] = 1;

    /* create a file using default properties */
    if((fid = H5Fcreate(TESTFILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* create a data space */
    if((sid = H5Screate_simple(rank, dims, NULL)) < 0) goto error;

    /* create dcpl  */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dcpl, rank, chunk_dims) < 0) goto error;

    if(H5Pset_fletcher32(dcpl) < 0) goto error;

    /* create a dataset */
    if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;

    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) goto error;

    /* close */
    if(H5Pclose(dcpl) < 0) goto error;
    if(H5Dclose(dsid) < 0) goto error;
    if(H5Sclose(sid) < 0) goto error;
    if(H5Fclose(fid) < 0) goto error;

#endif /* H5_HAVE_FILTER_FLETCHER32 */
    return 0;

#if defined H5_HAVE_FILTER_FLETCHER32
error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
#endif /* H5_HAVE_FILTER_FLETCHER32 */
} /* end test_filters_endianess() */

/* This message derives from H5Z */
const H5Z_class2_t H5Z_BOGUS[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_BOGUS,		/* Filter id number		*/
    1, 1,               /* Encoding and decoding enabled */
    "bogus",			/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:	filter_bogus
 *
 * Purpose:	A bogus compression method that doesn't do anything.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Raymond Lu
 *              2 June 2011
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus(unsigned int UNUSED flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf)
{
    return nbytes;
}


/*-------------------------------------------------------------------------
 * Function:	create_file_with_bogus_filter
 *
 * Purpose:	Create a file and a dataset with a bogus filter enabled
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              2 June 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
create_file_with_bogus_filter(void)
{
    hid_t     fid = -1;              /* file ID */
    hid_t     dsid = -1;             /* dataset ID */
    hid_t     sid = -1;              /* dataspace ID */
    hid_t     dcpl = -1;             /* dataset creation property list ID */
    hsize_t   dims[1] = {20};        /* dataspace dimensions */
    hsize_t   chunk_dims[1] = {10};  /* chunk dimensions */
    int       buf[20];
    int       rank = 1;
    int       i;

    for(i = 0; i < 20; i++)
        buf[i] = 1;

    /* create a file using default properties */
    if((fid = H5Fcreate(TESTFILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* create a data space */
    if((sid = H5Screate_simple(rank, dims, NULL)) < 0) goto error;

    /* create dcpl  */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* create chunking */ 
    if(H5Pset_chunk(dcpl, rank, chunk_dims) < 0) goto error;

    /* register bogus filter */
    if(H5Zregister (H5Z_BOGUS) < 0) goto error;
    if(H5Pset_filter(dcpl, H5Z_FILTER_BOGUS, 0, (size_t)0, NULL) < 0) goto error;

    /* create a dataset */
    if((dsid = H5Dcreate2(fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;

    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) goto error;

    /* close */
    if(H5Pclose(dcpl) < 0) goto error;
    if(H5Dclose(dsid) < 0) goto error;
    if(H5Sclose(sid) < 0) goto error;
    if(H5Fclose(fid) < 0) goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	call the generator function
 *
 * Return:	Success:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
   int nerrors = 0;

   nerrors += test_filters_endianess() < 0 ? 1 : 0;
   nerrors += create_file_with_bogus_filter() < 0 ? 1 : 0;

   if(nerrors)
       goto error;
   printf("All tests passed.\n");

   return 0;

error:
   nerrors = MAX(1, nerrors);
   printf("***** %d GEN_FILTERS FAILURES *****\n", nerrors);
   return 1;
}

