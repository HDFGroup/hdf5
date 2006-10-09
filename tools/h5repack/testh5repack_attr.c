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

/*-------------------------------------------------------------------------
 * Function: write_attr_in
 *
 * Purpose: write attributes in LOC_ID (dataset, group, named datatype)
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 12, 2003
 *
 *-------------------------------------------------------------------------
 */


void write_attr_in(hid_t loc_id,
                   const char* dset_name, /* for saving reference to dataset*/
                   hid_t fid, /* for reference create */
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

 hid_t   attr_id;
 hid_t   space_id;
 hid_t   type_id;
 herr_t  status;
 int     val, i, j, k, n;
 float   f;

 /* create 1D attributes with dimension [2], 2 elements */
 hsize_t    dims[1]={2};
 char       buf1[2][2]= {"ab","de"};        /* string */
 char       buf2[2]= {1,2};                 /* bitfield, opaque */
 s_t        buf3[2]= {{1,2},{3,4}};         /* compound */
 hobj_ref_t buf4[2];                        /* reference */
 e_t        buf45[2]= {RED,RED};            /* enum */
 hvl_t      buf5[2];                        /* vlen */
 hsize_t    dimarray[1]={3};                /* array dimension */
 int        buf6[2][3]= {{1,2,3},{4,5,6}};  /* array */
 int        buf7[2]= {1,2};                 /* integer */
 float      buf8[2]= {1,2};                 /* float */

 /* create 2D attributes with dimension [3][2], 6 elements */
 hsize_t    dims2[2]={3,2};
 char       buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};         /* string */
 char       buf22[3][2]= {{1,2},{3,4},{5,6}};                     /* bitfield, opaque */
 s_t        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};   /* compound */
 hobj_ref_t buf42[3][2];                                          /* reference */
 e_t        buf452[3][2];                                         /* enum */
 hvl_t      buf52[3][2];                                          /* vlen */
 int        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};  /* array */
 int        buf72[3][2]= {{1,2},{3,4},{5,6}};                     /* integer */
 float      buf82[3][2]= {{1,2},{3,4},{5,6}};                     /* float */

 /* create 3D attributes with dimension [4][3][2], 24 elements */
 hsize_t    dims3[3]={4,3,2};
 char       buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
 "rs","tu","vw","xz","AB","CD","EF","GH",
 "IJ","KL","MN","PQ","RS","TU","VW","XZ"};  /* string */
 char       buf23[4][3][2];    /* bitfield, opaque */
 s_t        buf33[4][3][2];    /* compound */
 hobj_ref_t buf43[4][3][2];    /* reference */
 e_t        buf453[4][3][2];   /* enum */
 hvl_t      buf53[4][3][2];    /* vlen */
 int        buf63[24][3];      /* array */
 int        buf73[4][3][2];    /* integer */
 float      buf83[4][3][2];    /* float */


/*-------------------------------------------------------------------------
 * 1D attributes
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
 /*
 buf1[2][2]= {"ab","de"};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
 Attribute:   <string> and <string>
 position      string of </g1>  string of </g1> difference
 ------------------------------------------------------------
[ 0 ]          a                z
[ 0 ]          b                z
[ 1 ]          d                z
[ 1 ]          e                z
 */
 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 make_attr(loc_id,1,dims,"string",type_id,buf1);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  for (i=0; i<2; i++)
   buf2[i]=buf2[1]=0;
 }
 /*
 buf2[2]= {1,2};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
 Attribute:   <bitfield> and <bitfield>
 position      bitfield of </g1> bitfield of </g1> difference
 position        opaque of </g1> opaque of </g1> difference
------------------------------------------------------------
[ 0 ]          1               0               1
[ 1 ]          2               0               2
 */

 type_id = H5Tcopy(H5T_STD_B8LE);
 make_attr(loc_id,1,dims,"bitfield",type_id,buf2);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */

 /*
 buf2[2]= {1,2};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
 Attribute:   <opaque> and <opaque>
 position     opaque of </g1> opaque of </g1> difference
 position        opaque of </g1> opaque of </g1> difference
------------------------------------------------------------
[ 0 ]          1               0               1
[ 1 ]          2               0               2
*/

 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 make_attr(loc_id,1,dims,"opaque",type_id,buf2);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  for (i=0; i<2; i++)
  {
   buf3[i].a=0; buf3[i].b=0;
  }
 }

 /*
 buf3[2]= {{1,2},{3,4}};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
 Attribute:   <compound> and <compound>
 position        compound of </g1> compound of </g1> difference
 ------------------------------------------------------------
 [ 0 ]          1               5               4
 [ 0 ]          2               5               3
 [ 1 ]          3               5               2
 [ 1 ]          4               5               1
 */

 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 make_attr(loc_id,1,dims,"compound",type_id,buf3);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* object references ( H5R_OBJECT  */
 if (dset_name)
 {
  status=H5Rcreate(&buf4[0],fid,dset_name,H5R_OBJECT,-1);
  status=H5Rcreate(&buf4[1],fid,dset_name,H5R_OBJECT,-1);
  make_attr(loc_id,1,dims,"reference to object",H5T_STD_REF_OBJ,buf4);
 }


/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 if (make_diffs)
 {
  for (i=0; i<2; i++)
  {
   buf45[i]=GREEN;
  }
 }
 /*
 buf45[2]= {RED,RED};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
 Attribute:   <enum> and <enum>
 position     enum of </g1>   enum of </g1>   difference
------------------------------------------------------------
[ 0 ]          RED              GREEN
[ 1 ]          RED              GREEN
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 make_attr(loc_id,1,dims,"enum",type_id,buf45);
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
 /*
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
 position        vlen of </g1>   vlen of </g1>   difference
------------------------------------------------------------
[ 0 ]          1               0               1
[ 1 ]          2               0               2
[ 1 ]          3               0               3
 */

 space_id = H5Screate_simple(1,dims,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 attr_id = H5Acreate(loc_id,"vlen",type_id,space_id,H5P_DEFAULT);
 status = H5Awrite(attr_id,type_id,buf5);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf5);
 assert(status>=0);
 status = H5Aclose(attr_id);
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
 /*
 buf6[2][3]= {{1,2,3},{4,5,6}};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
 Attribute:   <array> and <array>
position        array of </g1>  array of </g1>  difference
------------------------------------------------------------
[ 0 ]          1               0               1
[ 0 ]          2               0               2
[ 0 ]          3               0               3
[ 1 ]          4               0               4
[ 1 ]          5               0               5
[ 1 ]          6               0               6
 */
 type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
 make_attr(loc_id,1,dims,"array",type_id,buf6);
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
 /*
 buf7[2]= {1,2};
 buf8[2]= {1,2};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
 position        integer of </g1> integer of </g1> difference
 ------------------------------------------------------------
 [ 0 ]          1               0               1
 [ 1 ]          2               0               2
 position        float of </g1>  float of </g1>  difference
 ------------------------------------------------------------
 [ 0 ]          1               0               1
 [ 1 ]          2               0               2
 */
 make_attr(loc_id,1,dims,"integer",H5T_NATIVE_INT,buf7);
 make_attr(loc_id,1,dims,"float",H5T_NATIVE_FLOAT,buf8);


/*-------------------------------------------------------------------------
 * 2D attributes
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

 /*
 buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Attribute:   <string2D> and <string2D>
 position        string2D of </g1> string2D of </g1> difference
 ------------------------------------------------------------
[ 0 0 ]          a                z
[ 0 0 ]          b                z
[ 0 1 ]          c                z
[ 0 1 ]          d                z
[ 1 0 ]          e                z
[ 1 0 ]          f                z
[ 1 1 ]          g                z
[ 1 1 ]          h                z
[ 2 0 ]          i                z
[ 2 0 ]          j                z
[ 2 1 ]          k                z
[ 2 1 ]          l                z
 */

 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 make_attr(loc_id,2,dims2,"string2D",type_id,buf12);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  memset(buf22,0,sizeof buf22);
 }

 /*
 buf22[3][2]= {{1,2},{3,4},{5,6}};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Attribute:   <bitfield2D> and <bitfield2D>
 position        bitfield2D of </g1> bitfield2D of </g1> difference
------------------------------------------------------------
[ 0 0 ]          1               0               1
[ 0 1 ]          2               0               2
[ 1 0 ]          3               0               3
[ 1 1 ]          4               0               4
[ 2 0 ]          5               0               5
[ 2 1 ]          6               0               6
 */


 type_id = H5Tcopy(H5T_STD_B8LE);
 make_attr(loc_id,2,dims2,"bitfield2D",type_id,buf22);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */

 /*
 buf22[3][2]= {{1,2},{3,4},{5,6}};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Attribute:   <opaque2D> and <opaque2D>
 position        opaque2D of </g1> opaque2D of </g1> difference
------------------------------------------------------------
[ 0 0 ]          1               0               1
[ 0 1 ]          2               0               2
[ 1 0 ]          3               0               3
[ 1 1 ]          4               0               4
[ 2 0 ]          5               0               5
[ 2 1 ]          6               0               6
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 make_attr(loc_id,2,dims2,"opaque2D",type_id,buf22);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */
 if (make_diffs)
 {
  memset(buf32,0,sizeof buf32);
 }

 /*
 buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Attribute:   <opaque2D> and <opaque2D>
 position        opaque2D of </g1> opaque2D of </g1> difference
------------------------------------------------------------
[ 0 0 ]          1               0               1
[ 0 1 ]          2               0               2
[ 1 0 ]          3               0               3
[ 1 1 ]          4               0               4
[ 2 0 ]          5               0               5
[ 2 1 ]          6               0               6
 */


 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 make_attr(loc_id,2,dims2,"compound2D",type_id,buf32);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  for (i = 0; i < 3; i++) {
   for (j = 0; j < 2; j++) {
    status=H5Rcreate(&buf42[i][j],fid,dset_name,H5R_OBJECT,-1);
   }
  }
  make_attr(loc_id,2,dims2,"reference2D",H5T_STD_REF_OBJ,buf42);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 for (i=0; i<3; i++)
  for (j=0; j<2; j++)
  {
   if (make_diffs) buf452[i][j]=GREEN; else buf452[i][j]=RED;
  }

/*
Attribute:   <enum2D> and <enum2D>
position        enum2D of </g1> enum2D of </g1> difference
------------------------------------------------------------
[ 0 0 ]          RED              GREEN
[ 0 1 ]          RED              GREEN
[ 1 0 ]          RED              GREEN
[ 1 1 ]          RED              GREEN
[ 2 0 ]          RED              GREEN
[ 2 1 ]          RED              GREEN
*/

 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 make_attr(loc_id,2,dims2,"enum2D",type_id,buf452);
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

 /*
 position        vlen2D of </g1> vlen2D of </g1> difference
------------------------------------------------------------
[ 0 1 ]          1               0               1
[ 1 0 ]          2               0               2
[ 1 0 ]          3               0               3
[ 1 1 ]          4               0               4
[ 1 1 ]          5               0               5
[ 2 0 ]          6               0               6
[ 2 0 ]          7               0               7
[ 2 0 ]          8               0               8
[ 2 1 ]          9               0               9
[ 2 1 ]          10              0               10
[ 2 1 ]          11              0               11
*/

 space_id = H5Screate_simple(2,dims2,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 attr_id = H5Acreate(loc_id,"vlen2D",type_id,space_id,H5P_DEFAULT);
 status = H5Awrite(attr_id,type_id,buf52);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf52);
 assert(status>=0);
 status = H5Aclose(attr_id);
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
 /*
 buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};
 $h5diff file7.h5 file6.h5 g1 g1 -v
 Group:       </g1> and </g1>
Attribute:   <array2D> and <array2D>
position        array2D of </g1> array2D of </g1> difference
------------------------------------------------------------
[ 0 0 ]          1               0               1
[ 0 0 ]          2               0               2
[ 0 0 ]          3               0               3
[ 0 1 ]          4               0               4
[ 0 1 ]          5               0               5
[ 0 1 ]          6               0               6
[ 1 0 ]          7               0               7
[ 1 0 ]          8               0               8
[ 1 0 ]          9               0               9
[ 1 1 ]          10              0               10
[ 1 1 ]          11              0               11
[ 1 1 ]          12              0               12
[ 2 0 ]          13              0               13
[ 2 0 ]          14              0               14
[ 2 0 ]          15              0               15
[ 2 1 ]          16              0               16
[ 2 1 ]          17              0               17
[ 2 1 ]          18              0               18
 */
 type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
 make_attr(loc_id,2,dims2,"array2D",type_id,buf62);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER and H5T_FLOAT
 *-------------------------------------------------------------------------
 */

 if (make_diffs)
 {
  memset(buf72,0,sizeof buf72);
  memset(buf82,0,sizeof buf82);
 }
/*
Attribute:   <integer2D> and <integer2D>
position        integer2D of </g1> integer2D of </g1> difference
------------------------------------------------------------
[ 0 0 ]          1               0               1
[ 0 1 ]          2               0               2
[ 1 0 ]          3               0               3
[ 1 1 ]          4               0               4
[ 2 0 ]          5               0               5
[ 2 1 ]          6               0               6
6 differences found
Attribute:   <float2D> and <float2D>
position        float2D of </g1> float2D of </g1> difference
------------------------------------------------------------
[ 0 0 ]          1               0               1
[ 0 1 ]          2               0               2
[ 1 0 ]          3               0               3
[ 1 1 ]          4               0               4
[ 2 0 ]          5               0               5
[ 2 1 ]          6               0               6
*/

 make_attr(loc_id,2,dims2,"integer2D",H5T_NATIVE_INT,buf72);
 make_attr(loc_id,2,dims2,"float2D",H5T_NATIVE_FLOAT,buf82);


/*-------------------------------------------------------------------------
 * 3D attributes
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

 /*
 buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
 "rs","tu","vw","xz","AB","CD","EF","GH",
 "IJ","KL","MN","PQ","RS","TU","VW","XZ"};

Attribute:   <string3D> and <string3D>
position        string3D of </g1> string3D of </g1> difference
------------------------------------------------------------
[ 0 0 0 ]          a                z
[ 0 0 0 ]          b                z
[ 0 0 1 ]          c                z
[ 0 0 1 ]          d                z
[ 0 1 0 ]          e                z
[ 0 1 0 ]          f                z
[ 0 1 1 ]          g                z
[ 0 1 1 ]          h                z
[ 0 2 0 ]          i                z
[ 0 2 0 ]          j                z
[ 0 2 1 ]          k                z
[ 0 2 1 ]          l                z
[ 1 0 0 ]          m                z
[ 1 0 0 ]          n                z
[ 1 0 1 ]          p                z
[ 1 0 1 ]          q                z
[ 1 1 0 ]          r                z
[ 1 1 0 ]          s                z
[ 1 1 1 ]          t                z
[ 1 1 1 ]          u                z
[ 1 2 0 ]          v                z
[ 1 2 0 ]          w                z
[ 1 2 1 ]          x                z
[ 2 0 0 ]          A                z
[ 2 0 0 ]          B                z
[ 2 0 1 ]          C                z
[ 2 0 1 ]          D                z
[ 2 1 0 ]          E                z
[ 2 1 0 ]          F                z
[ 2 1 1 ]          G                z
[ 2 1 1 ]          H                z
[ 2 2 0 ]          I                z
[ 2 2 0 ]          J                z
[ 2 2 1 ]          K                z
[ 2 2 1 ]          L                z
[ 3 0 0 ]          M                z
[ 3 0 0 ]          N                z
[ 3 0 1 ]          P                z
[ 3 0 1 ]          Q                z
[ 3 1 0 ]          R                z
[ 3 1 0 ]          S                z
[ 3 1 1 ]          T                z
[ 3 1 1 ]          U                z
[ 3 2 0 ]          V                z
[ 3 2 0 ]          W                z
[ 3 2 1 ]          X                z
[ 3 2 1 ]          Z                z
 */

 type_id = H5Tcopy(H5T_C_S1);
 status  = H5Tset_size(type_id, 2);
 make_attr(loc_id,3,dims3,"string3D",type_id,buf13);
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

/*
position        bitfield3D of </g1> bitfield3D of </g1> difference
------------------------------------------------------------
[ 0 0 0 ]          1               0               1
[ 0 0 1 ]          2               0               2
[ 0 1 0 ]          3               0               3
[ 0 1 1 ]          4               0               4
[ 0 2 0 ]          5               0               5
[ 0 2 1 ]          6               0               6
[ 1 0 0 ]          7               0               7
[ 1 0 1 ]          8               0               8
[ 1 1 0 ]          9               0               9
[ 1 1 1 ]          10              0               10
[ 1 2 0 ]          11              0               11
[ 1 2 1 ]          12              0               12
[ 2 0 0 ]          13              0               13
[ 2 0 1 ]          14              0               14
[ 2 1 0 ]          15              0               15
[ 2 1 1 ]          16              0               16
[ 2 2 0 ]          17              0               17
[ 2 2 1 ]          18              0               18
[ 3 0 0 ]          19              0               19
[ 3 0 1 ]          20              0               20
[ 3 1 0 ]          21              0               21
[ 3 1 1 ]          22              0               22
[ 3 2 0 ]          23              0               23
[ 3 2 1 ]          24              0               24
*/

 type_id = H5Tcopy(H5T_STD_B8LE);
 make_attr(loc_id,3,dims3,"bitfield3D",type_id,buf23);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 make_attr(loc_id,3,dims3,"opaque3D",type_id,buf23);
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
/*position        compound3D of </g1> compound3D of </g1> difference
------------------------------------------------------------
[ 0 0 0 ]          1               0               1
[ 0 0 0 ]          2               0               2
[ 0 0 1 ]          3               0               3
[ 0 0 1 ]          4               0               4
[ 0 1 0 ]          5               0               5
[ 0 1 0 ]          6               0               6
[ 0 1 1 ]          7               0               7
[ 0 1 1 ]          8               0               8
[ 0 2 0 ]          9               0               9
[ 0 2 0 ]          10              0               10
[ 0 2 1 ]          11              0               11
[ 0 2 1 ]          12              0               12
[ 1 0 0 ]          13              0               13
[ 1 0 0 ]          14              0               14
[ 1 0 1 ]          15              0               15
[ 1 0 1 ]          16              0               16
[ 1 1 0 ]          17              0               17
[ 1 1 0 ]          18              0               18
[ 1 1 1 ]          19              0               19
[ 1 1 1 ]          20              0               20
[ 1 2 0 ]          21              0               21
[ 1 2 0 ]          22              0               22
[ 1 2 1 ]          23              0               23
[ 1 2 1 ]          24              0               24
[ 2 0 0 ]          25              0               25
[ 2 0 0 ]          26              0               26
[ 2 0 1 ]          27              0               27
[ 2 0 1 ]          28              0               28
[ 2 1 0 ]          29              0               29
[ 2 1 0 ]          30              0               30
[ 2 1 1 ]          31              0               31
[ 2 1 1 ]          32              0               32
[ 2 2 0 ]          33              0               33
[ 2 2 0 ]          34              0               34
[ 2 2 1 ]          35              0               35
[ 2 2 1 ]          36              0               36
[ 3 0 0 ]          37              0               37
[ 3 0 0 ]          38              0               38
[ 3 0 1 ]          39              0               39
[ 3 0 1 ]          40              0               40
[ 3 1 0 ]          41              0               41
[ 3 1 0 ]          42              0               42
[ 3 1 1 ]          43              0               43
[ 3 1 1 ]          44              0               44
[ 3 2 0 ]          45              0               45
[ 3 2 0 ]          46              0               46
[ 3 2 1 ]          47              0               47
[ 3 2 1 ]          48              0               48
*/



 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
 make_attr(loc_id,3,dims3,"compound3D",type_id,buf33);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 /* Create references to dataset */
 if (dset_name)
 {
  for (i = 0; i < 4; i++) {
   for (j = 0; j < 3; j++) {
    for (k = 0; k < 2; k++)
     status=H5Rcreate(&buf43[i][j][k],fid,dset_name,H5R_OBJECT,-1);
   }
  }
 make_attr(loc_id,3,dims3,"reference3D",H5T_STD_REF_OBJ,buf43);
 }

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */

 for (i = 0; i < 4; i++) {
  for (j = 0; j < 3; j++) {
   for (k = 0; k < 2; k++) {
    if (make_diffs) buf453[i][j][k]=RED; else buf453[i][j][k]=GREEN;
   }
  }
 }

/*
position        enum3D of </g1> enum3D of </g1> difference
------------------------------------------------------------
[ 0 0 0 ]          GREEN            RED
[ 0 0 1 ]          GREEN            RED
[ 0 1 0 ]          GREEN            RED
[ 0 1 1 ]          GREEN            RED
[ 0 2 0 ]          GREEN            RED
[ 0 2 1 ]          GREEN            RED
[ 1 0 0 ]          GREEN            RED
[ 1 0 1 ]          GREEN            RED
[ 1 1 0 ]          GREEN            RED
[ 1 1 1 ]          GREEN            RED
[ 1 2 0 ]          GREEN            RED
[ 1 2 1 ]          GREEN            RED
[ 2 0 0 ]          GREEN            RED
[ 2 0 1 ]          GREEN            RED
[ 2 1 0 ]          GREEN            RED
[ 2 1 1 ]          GREEN            RED
[ 2 2 0 ]          GREEN            RED
[ 2 2 1 ]          GREEN            RED
[ 3 0 0 ]          GREEN            RED
[ 3 0 1 ]          GREEN            RED
[ 3 1 0 ]          GREEN            RED
[ 3 1 1 ]          GREEN            RED
[ 3 2 0 ]          GREEN            RED
[ 3 2 1 ]          GREEN            RED
*/


 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 make_attr(loc_id,3,dims3,"enum3D",type_id,buf453);
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
/*
position        vlen3D of </g1> vlen3D of </g1> difference
------------------------------------------------------------
[ 0 0 1 ]          1               0               1
[ 0 1 0 ]          2               0               2
[ 0 1 1 ]          3               0               3
[ 0 2 0 ]          4               0               4
[ 0 2 1 ]          5               0               5
[ 1 0 0 ]          6               0               6
[ 1 0 0 ]          7               0               7
[ 1 0 1 ]          8               0               8
[ 1 0 1 ]          9               0               9
[ 1 1 0 ]          10              0               10
etc
*/
 space_id = H5Screate_simple(3,dims3,NULL);
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 attr_id = H5Acreate(loc_id,"vlen3D",type_id,space_id,H5P_DEFAULT);
 status = H5Awrite(attr_id,type_id,buf53);
 assert(status>=0);
 status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf53);
 assert(status>=0);
 status = H5Aclose(attr_id);
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
 /*
 position        array3D of </g1> array3D of </g1> difference
------------------------------------------------------------
[ 0 0 0 ]          1               0               1
[ 0 0 0 ]          2               0               2
[ 0 0 0 ]          3               0               3
[ 0 0 1 ]          4               0               4
[ 0 0 1 ]          5               0               5
[ 0 0 1 ]          6               0               6
[ 0 1 0 ]          7               0               7
etc
*/

 type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
 make_attr(loc_id,3,dims3,"array3D",type_id,buf63);
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

 /*
 position        integer3D of </g1> integer3D of </g1> difference
------------------------------------------------------------
[ 0 0 0 ]          1               0               1
[ 0 0 1 ]          2               0               2
[ 0 1 0 ]          3               0               3
[ 0 1 1 ]          4               0               4
[ 0 2 0 ]          5               0               5
[ 0 2 1 ]          6               0               6
[ 1 0 0 ]          7               0               7
[ 1 0 1 ]          8               0               8
[ 1 1 0 ]          9               0               9
[ 1 1 1 ]          10              0               10
etc
*/
 make_attr(loc_id,3,dims3,"integer3D",H5T_NATIVE_INT,buf73);
 make_attr(loc_id,3,dims3,"float3D",H5T_NATIVE_FLOAT,buf83);
}



