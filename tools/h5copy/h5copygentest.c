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

/*
 * Generate the binary hdf5 file for the h5copy tests
 */
#include <stdlib.h>
#include "hdf5.h"

#define FILENAME           "h5copytst.h5"
#define DATASET_SIMPLE     "simple"
#define DATASET_CHUNK      "chunk"
#define DATASET_COMPACT    "compact"
#define DATASET_COMPOUND   "compound"
#define DATASET_COMPRESSED "compressed"
#define DATASET_NAMED_VL   "named_vl"
#define DATASET_NESTED_VL  "nested_vl"




/*-------------------------------------------------------------------------
 * Function:    gent_simple
 *
 * Purpose:     Generate a simple dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_simple(hid_t loc_id)
{
    hid_t   sid, did;
    hsize_t dims[1] = {6};
    int     buf[6]  = {1,2,3,4,5,6};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create dataset */
    did = H5Dcreate(loc_id, DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
}

/*-------------------------------------------------------------------------
 * Function:    gent_chunked
 *
 * Purpose:     Generate a chunked dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_chunked(hid_t loc_id)
{
    hid_t   sid, did, pid;
    hsize_t dims[1] = {6};
    hsize_t chunk_dims[1] = {2};
    int     buf[6]  = {1,2,3,4,5,6};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create property plist */
    pid = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_chunk(pid, 1, chunk_dims);

    /* create dataset */
    did = H5Dcreate(loc_id, DATASET_CHUNK, H5T_NATIVE_INT, sid, pid);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
    H5Pclose(pid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_compact
 *
 * Purpose:     Generate a compact dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_compact(hid_t loc_id)
{
    hid_t   sid, did, pid;
    hsize_t dims[1] = {6};
    int     buf[6]  = {1,2,3,4,5,6};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create property plist  */
    pid = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_layout (pid,H5D_COMPACT);

    /* create dataset */
    did = H5Dcreate(loc_id, DATASET_COMPACT, H5T_NATIVE_INT, sid, pid);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
    H5Pclose(pid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_compound
 *
 * Purpose:     Generate a compound dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_compound(hid_t loc_id)
{
    typedef struct s_t
    {
        char str1[20];
        char str2[20];
    } s_t;
    hid_t   sid, did, tid_c, tid_s;
    hsize_t dims[1] = {2};
    s_t     buf[2]  = {{"str1", "str2"}, {"str3", "str4"}};

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create a compound type */
    tid_c = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    tid_s = H5Tcopy(H5T_C_S1);
    H5Tset_size(tid_s, 20);

    H5Tinsert(tid_c, "str1", HOFFSET(s_t,str1), tid_s);
    H5Tinsert(tid_c, "str2", HOFFSET(s_t,str2), tid_s);

    /* create dataset */
    did = H5Dcreate(loc_id, DATASET_COMPOUND, tid_c, sid, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, tid_c, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
    H5Tclose(tid_c);
    H5Tclose(tid_s);
}

/*-------------------------------------------------------------------------
 * Function:    gent_compressed
 *
 * Purpose:     Generate a compressed dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_compressed(hid_t loc_id)
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

    /* set the deflate filter */
#if defined (H5_HAVE_FILTER_DEFLATE)
    H5Pset_deflate(pid, 1);
#endif

    /* create dataset */
    did = H5Dcreate(loc_id, DATASET_COMPRESSED, H5T_NATIVE_INT, sid, pid);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Sclose(sid);
    H5Dclose(did);
    H5Pclose(pid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_named_vl
 *
 * Purpose:     Generate a variable lenght named datatype for a dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_named_vl(hid_t loc_id)
{
    hid_t   sid, did, tid;
    hsize_t dims[1] = {2};
    hvl_t   buf[2];                        

    /* allocate and initialize VL dataset to write */
    buf[0].len = 1;
    buf[0].p = malloc( 1 * sizeof(int));
    ((int *)buf[0].p)[0]=1;
    buf[1].len = 2;
    buf[1].p = malloc( 2 * sizeof(int));
    ((int *)buf[1].p)[0]=2;
    ((int *)buf[1].p)[1]=3;

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create datatype */
    tid = H5Tvlen_create(H5T_NATIVE_INT);

    /* create named datatype */
    H5Tcommit(loc_id, "vl", tid);

    /* create dataset */
    did = H5Dcreate(loc_id, DATASET_NAMED_VL, tid, sid, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf);
    H5Sclose(sid);
    H5Dclose(did);
    H5Tclose(tid);
}


/*-------------------------------------------------------------------------
 * Function:    gent_nested_vl
 *
 * Purpose:     Generate a nested variable length dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void gent_nested_vl(hid_t loc_id)
{
    hid_t   sid, did, tid1, tid2;
    hsize_t dims[1] = {2};
    hvl_t   buf[2];
    hvl_t   *tvl;                                 

    /* allocate and initialize VL dataset to write */
    buf[0].len = 1;
    buf[0].p = malloc( 1 * sizeof(hvl_t));
    tvl = buf[0].p;
    tvl->p = malloc( 1 * sizeof(int) );
    tvl->len = 1;
    ((int *)tvl->p)[0]=1;

    buf[1].len = 1;
    buf[1].p = malloc( 1 * sizeof(hvl_t));
    tvl = buf[1].p;
    tvl->p = malloc( 2 * sizeof(int) );
    tvl->len = 2;
    ((int *)tvl->p)[0]=2;
    ((int *)tvl->p)[1]=3;

    /* create dataspace */
    sid = H5Screate_simple(1, dims, NULL);

    /* create datatype */
    tid1 = H5Tvlen_create(H5T_NATIVE_INT);

    /* create nested VL datatype */
    tid2 = H5Tvlen_create(tid1);

    /* create dataset */
    did = H5Dcreate(loc_id, DATASET_NESTED_VL, tid2, sid, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* close */
    H5Dvlen_reclaim(tid2,sid,H5P_DEFAULT,buf);
    H5Sclose(sid);
    H5Dclose(did);
    H5Tclose(tid1);
    H5Tclose(tid2);
}


/*-------------------------------------------------------------------------
 * Function: main
 *
 *-------------------------------------------------------------------------
 */

int main(void)
{
    hid_t fid;

    /* Create source file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    gent_simple(fid);
    gent_chunked(fid);
    gent_compact(fid);
    gent_compound(fid);
    gent_compressed(fid);
    gent_named_vl(fid);
    gent_nested_vl(fid);
    H5Fclose(fid);

    /* Create destination file with all datasets in root group */

    return 0;
}

