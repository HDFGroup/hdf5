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
 * Function: make_dset
 *
 * Purpose: utility function to create and write a dataset in LOC_ID
 *-------------------------------------------------------------------------
 */

static 
int make_dset(hid_t loc_id,const char *name,hid_t sid, hid_t dcpl,void *buf)
{
 hid_t   dsid;
 /* create the dataset */
 if((dsid = H5Dcreate (loc_id,name,H5T_NATIVE_INT,sid,dcpl))<0)
  return -1;
 /* write */
 if(H5Dwrite(dsid,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
  goto out;
 /* close */
 if(H5Dclose(dsid)<0)
  return -1;
 return 0;
 out:
 H5E_BEGIN_TRY {
  H5Dclose(dsid);
 } H5E_END_TRY;
 return -1;
  
}

/*-------------------------------------------------------------------------
 * Function: make_filters
 *
 * Purpose: make several datasets with filters in location LOC_ID
 *
 *-------------------------------------------------------------------------
 */
int make_filters(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
 unsigned szip_pixels_per_block=8;
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 char     name[5];
 int      i, j, n;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 
/*-------------------------------------------------------------------------
 * make several dataset with no filters
 *-------------------------------------------------------------------------
 */
 for (i=0; i<4; i++)
 {
  sprintf(name,"dset%d",i+1);
  if (write_dset(loc_id,RANK,dims,name,H5T_NATIVE_INT,buf)<0)
   return -1;
 }
 
/*-------------------------------------------------------------------------
 * make several dataset with filters
 *-------------------------------------------------------------------------
 */
  
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dataset creation property list; the same DCPL is used for all dsets */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 /* set up chunk */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;



/*-------------------------------------------------------------------------
 * SZIP
 *-------------------------------------------------------------------------
 */
 /* set szip data */
 if(H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block)<0)
  goto out;
 if (make_dset(loc_id,"dset_szip",sid,dcpl,buf)<0)
  goto out;

/*-------------------------------------------------------------------------
 * GZIP
 *-------------------------------------------------------------------------
 */
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0) 
  goto out;
 /* set deflate data */
 if(H5Pset_deflate(dcpl, 9)<0)
  goto out;
 if (make_dset(loc_id,"dset_gzip",sid,dcpl,buf)<0)
  goto out;


/*-------------------------------------------------------------------------
 * shuffle
 *-------------------------------------------------------------------------
 */
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0) 
  goto out;
 /* set the shuffle filter */
 if (H5Pset_shuffle(dcpl)<0) 
  goto out;
 if (make_dset(loc_id,"dset_shuffle",sid,dcpl,buf)<0)
  goto out;

/*-------------------------------------------------------------------------
 * checksum
 *-------------------------------------------------------------------------
 */
#if 1
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0) 
  goto out;
 /* set the checksum filter */
 if (H5Pset_fletcher32(dcpl)<0) 
  goto out;
 if (make_dset(loc_id,"dset_fletcher32",sid,dcpl,buf)<0)
  goto out;
#endif

/*-------------------------------------------------------------------------
 * shuffle + SZIP
 *-------------------------------------------------------------------------
 */
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0) 
  goto out;
 /* set the shuffle filter */
 if (H5Pset_shuffle(dcpl)<0) 
  goto out;
 /* set szip data */
 if(H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block)<0)
  goto out;
 if (make_dset(loc_id,"dset_all",sid,dcpl,buf)<0)
  goto out;


/*-------------------------------------------------------------------------
 * close space and dcpl
 *-------------------------------------------------------------------------
 */
 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;
 
 return 0;                                                 
 
out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}





