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
#include "testh5repack.h"

static void make_dset_reg_ref(hid_t loc_id);


/*-------------------------------------------------------------------------
 * Function: write_dset_in
 *
 * Purpose: write datasets in LOC_ID
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 12, 2003
 *
 *-------------------------------------------------------------------------
 */


void write_dset_in(hid_t loc_id,
                   const char* dset_name, /* for saving reference to dataset*/
                   hid_t file_id,
                   int make_diffs /* flag to modify data buffers */)
{
 /* Compound datatype */
 typedef struct s_t
 {
  char   a;
  double b;
 } s_t;

 typedef enum
 {
  RED,
  GREEN
 } e_t;

 hid_t   dset_id;
 hid_t   space_id;
 hid_t   type_id;
 hid_t   plist_id;
 herr_t  status;
 int     val, i, j, k, n;
 float   f;

 /* create 1D attributes with dimension [2], 2 elements */
 hsize_t    dims[1]={2};
 hsize_t    dims1r[1]={1};
 char       buf1[2][2]= {"ab","de"};        /* string */
 char       buf2[2]= {1,2};                 /* bitfield, opaque */
 s_t        buf3[2]= {{1,2},{3,4}};         /* compound */
 hobj_ref_t buf4[1];                        /* reference */
 e_t        buf45[2]= {RED,GREEN};          /* enum */
 hvl_t      buf5[2];                        /* vlen */
 hsize_t    dimarray[1]={3};                /* array dimension */
 int        buf6[2][3]= {{1,2,3},{4,5,6}};  /* array */
 int        buf7[2]= {1,2};                 /* integer */
 float      buf8[2]= {1,2};                 /* float */

 /* create 2D attributes with dimension [3][2], 6 elements */
 hsize_t    dims2[2]={3,2};
 hsize_t    dims2r[2]={1,1};
 char       buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};         /* string */
 char       buf22[3][2]= {{1,2},{3,4},{5,6}};                     /* bitfield, opaque */
 s_t        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};   /* compound */
 hobj_ref_t buf42[1][1];                                          /* reference */
 hvl_t      buf52[3][2];                                          /* vlen */
 int        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};  /* array */
 int        buf72[3][2]= {{1,2},{3,4},{5,6}};                     /* integer */
 float      buf82[3][2]= {{1,2},{3,4},{5,6}};                     /* float */

 /* create 3D attributes with dimension [4][3][2], 24 elements */
 hsize_t    dims3[3]={4,3,2};
 hsize_t    dims3r[3]={1,1,1};
 char       buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
 "rs","tu","vw","xz","AB","CD","EF","GH",
 "IJ","KL","MN","PQ","RS","TU","VW","XZ"};  /* string */
 char       buf23[4][3][2];    /* bitfield, opaque */
 s_t        buf33[4][3][2];    /* compound */
 hobj_ref_t buf43[1][1][1];    /* reference */
 hvl_t      buf53[4][3][2];    /* vlen */
 int        buf63[24][3];      /* array */
 int        buf73[4][3][2];    /* integer */
 float      buf83[4][3][2];    /* float */


/*-------------------------------------------------------------------------
 * 1D
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */


 if (make_diffs)
 {
  for (i=0; i<2; i++)
   for (j=0; j<2; j++)
   {
    buf1[i][j]='z';
   }
 }


 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_dset(loc_id,1,dims,"string",type_id,buf1);
 status = H5Tclose(type_id);


 /* create hard link */
 status = H5Glink(loc_id, H5L_TYPE_HARD, "string", "string_link");

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  for (i=0; i<2; i++)
   buf2[i]=buf2[1]=0;
 }

 type_id = H5Tcopy(H5T_STD_B8LE);
 write_dset(loc_id,1,dims,"bitfield",type_id,buf2);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  for (i=0; i<2; i++)
  {
   buf3[i].a=0; buf3[i].b=0;
  }
 }

 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_dset(loc_id,1,dims,"opaque",type_id,buf2);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */


 if (make_diffs)
 {
  for (i=0; i<2; i++)
  {
   buf45[i]=GREEN;
  }
 }

 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_dset(loc_id,1,dims,"compound",type_id,buf3);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* object references ( H5R_OBJECT ) */
 if (dset_name)
 {
  status=H5Rcreate(&buf4[0],file_id,dset_name,H5R_OBJECT,-1);
  write_dset(loc_id,1,dims1r,"refobj",H5T_STD_REF_OBJ,buf4);
 }

 /* Dataset region reference ( H5R_DATASET_REGION  )  */
 make_dset_reg_ref(loc_id);


/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_dset(loc_id,1,dims,"enum",type_id,buf45);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */

 /* Allocate and initialize VL dataset to write */

 buf5[0].len = 1;
 buf5[0].p = malloc( 1 * sizeof(int));
 ((int *)buf5[0].p)[0]=1;
 buf5[1].len = 2;
 buf5[1].p = malloc( 2 * sizeof(int));
 ((int *)buf5[1].p)[0]=2;
 ((int *)buf5[1].p)[1]=3;

 if (make_diffs)
 {
  ((int *)buf5[0].p)[0]=0;
  ((int *)buf5[1].p)[0]=0;
  ((int *)buf5[1].p)[1]=0;
 }

 space_id = H5Screate_simple(1,dims,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 dset_id = H5Dcreate(loc_id,"vlen",type_id,space_id,H5P_DEFAULT);
 status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf5);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf5);
 assert(status>=0);
 status = H5Dclose(dset_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  for (i=0; i<2; i++)
   for (j=0; j<3; j++)
   {
    buf6[i][j]=0;
   }
 }

 type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
 write_dset(loc_id,1,dims,"array",type_id,buf6);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER and H5T_FLOAT
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  for (i=0; i<2; i++)
  {
   buf7[i]=0;
   buf8[i]=0;
  }
 }

 write_dset(loc_id,1,dims,"integer",H5T_NATIVE_INT,buf7);
 write_dset(loc_id,1,dims,"float",H5T_NATIVE_FLOAT,buf8);


/*-------------------------------------------------------------------------
 * 2D
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  memset(buf12, 'z', sizeof buf12);
 }


 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_dset(loc_id,2,dims2,"string2D",type_id,buf12);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */


 if (make_diffs)
 {
  memset(buf22,0,sizeof buf22);
 }

 type_id = H5Tcopy(H5T_STD_B8LE);
 write_dset(loc_id,2,dims2,"bitfield2D",type_id,buf22);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_dset(loc_id,2,dims2,"opaque2D",type_id,buf22);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  memset(buf32,0,sizeof buf32);
 }

 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_dset(loc_id,2,dims2,"compound2D",type_id,buf32);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  for (i = 0; i < 1; i++) {
   for (j = 0; j < 1; j++) {
    status=H5Rcreate(&buf42[i][j],file_id,dset_name,H5R_OBJECT,-1);
   }
  }
  write_dset(loc_id,2,dims2r,"refobj2D",H5T_STD_REF_OBJ,buf42);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */

 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_dset(loc_id,2,dims2,"enum2D",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */

/* Allocate and initialize VL dataset to write */
 n=0;
 for (i = 0; i < 3; i++) {
  for (j = 0; j < 2; j++) {
    int l;
    buf52[i][j].p = malloc((i + 1) * sizeof(int));
    buf52[i][j].len = i + 1;
    for (l = 0; l < i + 1; l++)
    if (make_diffs)((int *)buf52[i][j].p)[l] = 0;
    else ((int *)buf52[i][j].p)[l] = n++;
  }
 }

 space_id = H5Screate_simple(2,dims2,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 dset_id = H5Dcreate(loc_id,"vlen2D",type_id,space_id,H5P_DEFAULT);
 status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf52);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf52);
 assert(status>=0);
 status = H5Dclose(dset_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  memset(buf62,0,sizeof buf62);
 }


 type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
 write_dset(loc_id,2,dims2,"array2D",type_id,buf62);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER, write a fill value
 *-------------------------------------------------------------------------
 */


 if (make_diffs)
 {
  memset(buf72,0,sizeof buf72);
  memset(buf82,0,sizeof buf82);
 }


 plist_id = H5Pcreate(H5P_DATASET_CREATE);
 space_id = H5Screate_simple(2,dims2,NULL);
 dset_id = H5Dcreate(loc_id,"integer2D",H5T_NATIVE_INT,space_id,plist_id);
 status = H5Dwrite(dset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf72);
 status = H5Pclose(plist_id);
 status = H5Dclose(dset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_FLOAT
 *-------------------------------------------------------------------------
 */

 write_dset(loc_id,2,dims2,"float2D",H5T_NATIVE_FLOAT,buf82);


/*-------------------------------------------------------------------------
 * 3D
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  memset(buf13,'z',sizeof buf13);
 }

 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 write_dset(loc_id,3,dims3,"string3D",type_id,buf13);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */


 n=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    if (make_diffs) buf23[i][j][k]=0;
    else buf23[i][j][k]=n++;
   }
  }
 }


 type_id = H5Tcopy(H5T_STD_B8LE);
 write_dset(loc_id,3,dims3,"bitfield3D",type_id,buf23);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_dset(loc_id,3,dims3,"opaque3D",type_id,buf23);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */

 n=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    if (make_diffs) {
     buf33[i][j][k].a=0;
     buf33[i][j][k].b=0;
    }
    else {
     buf33[i][j][k].a=n++;
     buf33[i][j][k].b=n++;
    }
   }
  }
 }


 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 write_dset(loc_id,3,dims3,"compound3D",type_id,buf33);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  for (i = 0; i < 1; i++) {
   for (j = 0; j < 1; j++) {
    for (k = 0; k < 1; k++)
     status=H5Rcreate(&buf43[i][j][k],file_id,dset_name,H5R_OBJECT,-1);
   }
  }
 write_dset(loc_id,3,dims3r,"refobj3D",H5T_STD_REF_OBJ,buf43);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */

 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_dset(loc_id,3,dims3,"enum3D",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */

 /* Allocate and initialize VL dataset to write */
 n=0;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    int l;
    buf53[i][j][k].p = malloc((i + 1) * sizeof(int));
    buf53[i][j][k].len = i + 1;
    for (l = 0; l < i + 1; l++)
    if (make_diffs)((int *)buf53[i][j][k].p)[l] = 0;
    else ((int *)buf53[i][j][k].p)[l] = n++;
   }
  }
 }

 space_id = H5Screate_simple(3,dims3,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 dset_id = H5Dcreate(loc_id,"vlen3D",type_id,space_id,H5P_DEFAULT);
 status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf53);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf53);
 assert(status>=0);
 status = H5Dclose(dset_id);
 status = H5Tclose(type_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */


 n=1;
 for (i = 0; i < 24; i++) {
  for (j = 0; j < (int)dimarray[0]; j++) {
    if (make_diffs) buf63[i][j]=0;
    else buf63[i][j]=n++;
  }
 }

 type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
 write_dset(loc_id,3,dims3,"array3D",type_id,buf63);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER and H5T_FLOAT
 *-------------------------------------------------------------------------
 */
 n=1; f=1;
 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    if (make_diffs) {
     buf73[i][j][k]=0;
     buf83[i][j][k]=0;
    }
    else {
     buf73[i][j][k]=n++;
     buf83[i][j][k]=f++;
    }
   }
  }
 }
 write_dset(loc_id,3,dims3,"integer3D",H5T_NATIVE_INT,buf73);
 write_dset(loc_id,3,dims3,"float3D",H5T_NATIVE_FLOAT,buf83);
}



/*-------------------------------------------------------------------------
 * Function: make_dset_reg_ref
 *
 * Purpose: write dataset region references
 *
 *-------------------------------------------------------------------------
 */

#define SPACE1_RANK 1
#define SPACE1_DIM1 1
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10
#define NPOINTS 10

static void make_dset_reg_ref(hid_t loc_id)
{
 hid_t           dset1; /* Dataset ID   */
 hid_t           dset2; /* Dereferenced dataset ID */
 hid_t           sid1;  /* Dataspace ID #1  */
 hid_t           sid2;  /* Dataspace ID #2  */
 hsize_t         dims1[] = {SPACE1_DIM1};
 hsize_t         dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
 hsize_t         start[SPACE2_RANK];     /* Starting location of hyperslab */
 hsize_t         stride[SPACE2_RANK];    /* Stride of hyperslab */
 hsize_t         count[SPACE2_RANK];     /* Element count of hyperslab */
 hsize_t         block[SPACE2_RANK];     /* Block size of hyperslab */
 hdset_reg_ref_t *wbuf;  /* buffer to write to disk */
 int             *dwbuf; /* Buffer for writing numeric data to disk */
 int             i;      /* counting variables */
 herr_t          ret;    /* Generic return value  */

 /* Allocate write & read buffers */
 wbuf=calloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
 dwbuf=malloc(sizeof(int)*SPACE2_DIM1*SPACE2_DIM2);

 /* Create dataspace for datasets */
 sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);

 /* Create a dataset */
 dset2=H5Dcreate(loc_id,"dsetreg",H5T_NATIVE_UCHAR,sid2,H5P_DEFAULT);

 for(i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++)
  dwbuf[i]=i*3;

 /* Write selection to disk */
 ret=H5Dwrite(dset2,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,dwbuf);

 /* Close Dataset */
 ret = H5Dclose(dset2);

 /* Create dataspace for the reference dataset */
 sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

 /* Create a dataset */
 dset1=H5Dcreate(loc_id,"refreg",H5T_STD_REF_DSETREG,sid1,H5P_DEFAULT);

 /* Create references */

 /* Select 6x6 hyperslab for first reference */
 start[0]=2; start[1]=2;
 stride[0]=1; stride[1]=1;
 count[0]=6; count[1]=6;
 block[0]=1; block[1]=1;
 ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);

 /* Store dataset region */
 ret = H5Rcreate(&wbuf[0],loc_id,"dsetreg",H5R_DATASET_REGION,sid2);

 /* Write selection to disk */
 ret=H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

 /* Close all objects */
 ret = H5Sclose(sid1);
 ret = H5Dclose(dset1);
 ret = H5Sclose(sid2);

 free(wbuf);
 free(dwbuf);
}
