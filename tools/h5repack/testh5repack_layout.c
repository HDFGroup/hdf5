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

#if 0
#define CHUNK_DEBUG
#endif

#if defined(CHUNK_DEBUG)
#define DIM1  39
#define DIM2  20
#define CDIM1 19
#define CDIM2 10
#else
#define DIM1  40
#define DIM2  20
#define CDIM1 20
#define CDIM2 10
#endif

#define RANK  2


/*-------------------------------------------------------------------------
 * Function: make_layout
 *
 * Purpose: make several datasets with several layouts in location LOC_ID
 *
 *-------------------------------------------------------------------------
 */
int make_layout(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
/*-------------------------------------------------------------------------
 * make several dataset with several layout options
 *-------------------------------------------------------------------------
 */
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dataset creation property list; the same DCPL is used for all dsets */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;

/*-------------------------------------------------------------------------
 * H5D_COMPACT
 *-------------------------------------------------------------------------
 */
 if(H5Pset_layout (dcpl,H5D_COMPACT)<0)
  goto out;
 if (make_dset(loc_id,"dset_compact",sid,dcpl,buf)<0)
  goto out;

/*-------------------------------------------------------------------------
 * H5D_COMPACT
 *-------------------------------------------------------------------------
 */
 if(H5Pset_layout (dcpl,H5D_CONTIGUOUS)<0)
  goto out;
 if (make_dset(loc_id,"dset_contiguous",sid,dcpl,buf)<0)
  goto out;

/*-------------------------------------------------------------------------
 * H5D_CHUNKED
 *-------------------------------------------------------------------------
 */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;
 if (make_dset(loc_id,"dset_chunk",sid,dcpl,buf)<0)
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





