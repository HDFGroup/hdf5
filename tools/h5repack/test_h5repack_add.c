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

#include "hdf5.h"
#include "h5test.h"
#include "test_h5repack_add.h"


/*-------------------------------------------------------------------------
 * Function:	make_deflate
 *
 * Purpose:	make a dataset using DEFLATE (GZIP) compression
 *
 * Return:	Success:	zero
 *		Failure:	1
 *
 * Programmer:	Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
make_deflate(hid_t fid)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    dsid; /* dataset ID */
 hid_t    sid;  /* dataspace ID */
 int      rank=2;
 hsize_t  dims[2]={4,2};
 hsize_t  chunk_dims[2]={2,1};
 int      buf[4][2]={{1,2},{3,4},{5,6},{7,8}};
 
 /* create a space */
 if((sid = H5Screate_simple(rank, dims, NULL))<0)
  TEST_ERROR;

 /* create the dataset creation property list */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  TEST_ERROR;
 
 /* set up for deflated data */
 if(H5Pset_chunk(dcpl, rank, chunk_dims)<0)
  TEST_ERROR;
 if(H5Pset_deflate(dcpl, 9)<0)
  TEST_ERROR;

 /* create the dataset */
 if((dsid = H5Dcreate (fid, "dset1", H5T_NATIVE_INT, sid, dcpl))<0)
  TEST_ERROR;

 /* write the data to the dataset */
 if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
  TEST_ERROR;
 
 /* close */
 if(H5Dclose(dsid)<0)
  TEST_ERROR;
 if(H5Pclose(dcpl)<0)
  TEST_ERROR;
 if(H5Sclose(sid)<0)
  TEST_ERROR;
 
 return 0;                                                 
 
error:                                                       
 return 1;
}

/*-------------------------------------------------------------------------
 * Function:	make_szip
 *
 * Purpose:	make a dataset using SZIP compression
 *
 * Return:	Success:	zero
 *		Failure:	1
 *
 * Programmer:	Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
make_szip(hid_t fid)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    dsid; /* dataset ID */
 hid_t    sid;  /* dataspace ID */
 int      rank=2;
 hsize_t  dims[2]={4,2};
 hsize_t  chunk_dims[2]={2,1};
 int      buf[4][2]={{1,2},{3,4},{5,6},{7,8}};
 unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
 unsigned szip_pixels_per_block;

  /* 
  pixels_per_block must be an even number, and <= pixels_per_scanline 
  and <= MAX_PIXELS_PER_BLOCK
  */
 szip_pixels_per_block=16;
 
 /* create a space */
 if((sid = H5Screate_simple(rank, dims, NULL))<0)
  TEST_ERROR;

 /* create the dataset creation property list */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  TEST_ERROR;
 
 /* set up for sziped data */
 if(H5Pset_chunk(dcpl, rank, chunk_dims)<0)
  TEST_ERROR;
 if(H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block)<0)
  TEST_ERROR;

 /* create the dataset */
 if((dsid = H5Dcreate (fid, "dset2", H5T_NATIVE_INT, sid, dcpl))<0)
  TEST_ERROR;

 /* write the data to the dataset */
 if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
  TEST_ERROR;
 
 /* close */
 if(H5Dclose(dsid)<0)
  TEST_ERROR;
 if(H5Pclose(dcpl)<0)
  TEST_ERROR;
 if(H5Sclose(sid)<0)
  TEST_ERROR;
 
 return 0;                                                 
 
error:                                                       
 return 1;
}


/*-------------------------------------------------------------------------
 * Function:	make_dsets
 *
 * Purpose:	make several datasets with DEFLATE and SZIP filters
 *
 * Return:	Success:	zero
 *		Failure:	1
 *
 * Programmer:	Pedro Vicente <pvn@ncsa.uiuc.edu>
 *             September, 19, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int make_dsets(void)
{
 hid_t  fid;  /* file ID */
 int		  nerrors=0;

 TESTING("    generating datasets");

 /* create a file */
 if((fid = H5Fcreate (FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0)
  TEST_ERROR;
 
 nerrors += make_deflate(fid);
 nerrors += make_szip(fid);
 
 /* close */
 if(H5Fclose(fid)<0)
  TEST_ERROR;
 
 if (nerrors)
  goto error;

 PASSED();   
 return 0;                                                 
 
error:                                                       
 return 1;
}

