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

/*
 * Generate the binary hdf5 file for the h5copy tests
 */

#include "hdf5.h"
#define FILENAME         "h5copytst.h5"
#define DATASET_SIMPLE   "simple"
#define DATASET_CHUNK    "chunk"
#define DATASET_COMPACT  "compact"
#define DATASET_COMPOUND "compound"



/*-------------------------------------------------------------------------
 * Function:    gent_simple_dataset
 *
 * Purpose:     Generate a simple dataset in FID
 *
 *-------------------------------------------------------------------------
 */
static void gent_simple_dataset(hid_t fid)
{
 hid_t   sid, did;
 hsize_t dims[1] = {6};
 int     buf[6]  = {1,2,3,4,5,6};

 /* create dataspace */
 sid = H5Screate_simple(1, dims, NULL);
 
 /* create dataset */
 did = H5Dcreate(fid, DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT);
 
 /* write */
 H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
 
 /* close */
 H5Sclose(sid);
 H5Dclose(did);
}

/*-------------------------------------------------------------------------
 * Function:    gent_chunked_dataset
 *
 * Purpose:     Generate a chunked dataset in FID
 *
 *-------------------------------------------------------------------------
 */
static void gent_chunked_dataset(hid_t fid)
{
 hid_t   sid, did, pid;
 hsize_t dims[1] = {6};
 hsize_t chunk_dims[1] = {2};
 int     buf[6]  = {1,2,3,4,5,6};

 /* create dataspace */
 sid = H5Screate_simple(1, dims, NULL);

 /* create property plist for chunk*/
 pid = H5Pcreate(H5P_DATASET_CREATE);
 H5Pset_chunk(pid, 1, chunk_dims);
 
 /* create dataset */
 did = H5Dcreate(fid, DATASET_CHUNK, H5T_NATIVE_INT, sid, pid);
 
 /* write */
 H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
 
 /* close */
 H5Sclose(sid);
 H5Dclose(did);
 H5Pclose(pid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_compact_dataset
 *
 * Purpose:     Generate a compact dataset in FID
 *
 *-------------------------------------------------------------------------
 */
static void gent_compact_dataset(hid_t fid)
{
 hid_t   sid, did, pid;
 hsize_t dims[1] = {6};
 int     buf[6]  = {1,2,3,4,5,6};

 /* create dataspace */
 sid = H5Screate_simple(1, dims, NULL);

 /* create property plist for chunk*/
 pid = H5Pcreate(H5P_DATASET_CREATE);
 H5Pset_layout (pid,H5D_COMPACT);
 
 /* create dataset */
 did = H5Dcreate(fid, DATASET_COMPACT, H5T_NATIVE_INT, sid, pid);
 
 /* write */
 H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
 
 /* close */
 H5Sclose(sid);
 H5Dclose(did);
 H5Pclose(pid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_compound_dataset
 *
 * Purpose:     Generate a compound dataset in FID
 *
 *-------------------------------------------------------------------------
 */
static void gent_compound_dataset(hid_t fid)
{
 typedef struct s_t
 {
  char str1[20];
  char str2[20];
 } s_t;
 hid_t   sid, did, tid_c, tid_s;
 hsize_t dims[1] = {2};
 s_t     buf[2]  = {{"str1","str2"},{"str3","str4"}};

 /* create dataspace */
 sid = H5Screate_simple(1, dims, NULL);

 /* create a compound type */
 tid_c = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 tid_s = H5Tcopy (H5T_C_S1);
 H5Tset_size (tid_s, 20);
   
 H5Tinsert (tid_c, "str1", HOFFSET(s_t,str1), tid_s);
 H5Tinsert (tid_c, "str2", HOFFSET(s_t,str2), tid_s);

 /* create dataset */
 did = H5Dcreate(fid, DATASET_COMPOUND, tid_c, sid, H5P_DEFAULT);
 
 /* write */
 H5Dwrite(did, tid_c, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
 
 /* close */
 H5Sclose(sid);
 H5Dclose(did);
 H5Tclose(tid_c);
 H5Tclose(tid_s);
}




/*-------------------------------------------------------------------------
 * Function: main
 *
 *-------------------------------------------------------------------------
 */

int main(void)
{
 hid_t fid;
 
 
 fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

 gent_simple_dataset(fid);
 gent_chunked_dataset(fid);
 gent_compact_dataset(fid);
 gent_compound_dataset(fid);
 
 H5Fclose(fid);
 return 0;
}

