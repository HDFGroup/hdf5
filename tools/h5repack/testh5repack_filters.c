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
#include "h5repack.h"

#define DIM1  40
#define DIM2  20
#define CDIM1 DIM1/2
#define CDIM2 DIM2/2
#define RANK  2



/*-------------------------------------------------------------------------
 * Function: make_deflate
 *
 * Purpose: make a dataset using DEFLATE (GZIP) compression in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
int make_deflate(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    dsid; /* dataset ID */
 hid_t    sid;  /* dataspace ID */
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[40][20];
 int      i, j, n=0;

 for (i=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;

 /* create the dataset creation property list */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 
 /* set up for deflated data */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;
 if(H5Pset_deflate(dcpl, 9)<0)
  goto out;

 /* create the dataset */
 if((dsid = H5Dcreate (loc_id, "dset_gzip", H5T_NATIVE_INT, sid, dcpl))<0)
  goto out;

 /* write the data to the dataset */
 if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
  goto out;
 
 /* close */
 if(H5Dclose(dsid)<0)
  goto out;

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;
 
 return 0;                                                 
 
out:
 H5E_BEGIN_TRY {
  H5Dclose(dsid);
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: make_szip
 *
 * Purpose: make a dataset using SZIP compression in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
int make_szip(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    dsid; /* dataset ID */
 hid_t    sid;  /* dataspace ID */
 unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
 unsigned szip_pixels_per_block;
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[40][20];
 int      i, j, n=0;

 for (i=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 
 memset(buf,0,sizeof buf);

  /* 
  pixels_per_block must be an even number, and <= pixels_per_scanline 
  and <= MAX_PIXELS_PER_BLOCK
  */
 szip_pixels_per_block=10;
 
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;

 /* create the dataset creation property list */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 
 /* set up for sziped data */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;
 if(H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block)<0)
  goto out;

 /* create the dataset */
 if((dsid = H5Dcreate (loc_id, "dset_szip", H5T_NATIVE_INT, sid, dcpl))<0)
  goto out;

 /* write the data to the dataset */
 if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
  goto out;
 
 /* close */
 if(H5Dclose(dsid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;
 if(H5Sclose(sid)<0)
  goto out;
 
 return 0;                                                 
 
out:
 H5E_BEGIN_TRY {
  H5Dclose(dsid);
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: make_nofilters
 *
 * Purpose: make several dataset with no filters
 *
 *-------------------------------------------------------------------------
 */
int make_nofilters(hid_t loc_id)
{
 char     name[5];
 hsize_t  dims[RANK]={DIM1,DIM2};
 int      buf[40][20];
 int      i, j, n=0;

 for (i=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 
 for (i=0; i<4; i++)
 {
  sprintf(name,"dset%d",i+1);
  if (write_dset(loc_id,RANK,dims,name,H5T_NATIVE_INT,buf)<0)
   return -1;
 }


 return 0;
}


