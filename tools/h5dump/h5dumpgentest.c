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
 * Generate the binary hdf5 files for the h5dump tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files in the local directory.
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */
#include <limits.h>

#include "hdf5.h"
#include "H5private.h"

#define FILE1	"tgroup.h5"
#define FILE2	"tdset.h5"
#define FILE3	"tattr.h5"
#define FILE4	"tslink.h5"
#define FILE5	"thlink.h5"
#define FILE6	"tcompound.h5"
#define FILE7	"tall.h5"
#define FILE8	"tdset2.h5"
#define FILE9	"tcompound2.h5"
#define FILE10	"tloop.h5"
#define FILE11	"tloop2.h5"
#define FILE12	"tmany.h5"
#define FILE13	"tstr.h5"
#define FILE14	"tstr2.h5"
#define FILE15	"tenum.h5"
#define FILE16	"tobjref.h5"
#define FILE17	"tdatareg.h5"
#define FILE18	"tnestedcomp.h5"
#define FILE19	"topaque.h5"
#define FILE20	"tbitfields.h5"
#define FILE21	"tvldtypes1.h5"
#define FILE22	"tvldtypes2.h5"
#define FILE23	"tvldtypes3.h5"
#define FILE24	"tvldtypes4.h5"
#define FILE25	"tarray1.h5"
#define FILE26	"tarray2.h5"
#define FILE27	"tarray3.h5"
#define FILE28	"tarray4.h5"
#define FILE29	"tarray5.h5"
#define FILE30	"tarray6.h5"
#define FILE31	"tarray7.h5"
#define FILE32	"tempty.h5"
#define FILE33  "tgrp_comments.h5"
#define FILE34  "tsplit_file"
#define FILE35  "tfamily%05d.h5"
#define FILE36  "tmulti"
#define FILE37  "tlarge_objname.h5"

#define LENSTR		50
#define LENSTR2		11

#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

#define SPACE1_RANK	1
#define SPACE1_DIM1	4

/* Element selection information */
#define POINT1_NPOINTS	10

typedef enum{
     RED,
     GREEN,
     BLUE,
     WHITE,
     BLACK
} enumtype;

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;


/* 1-D array datatype */
#define ARRAY1_RANK	1
#define ARRAY1_DIM1 4

/* 3-D array datatype */
#define ARRAY2_RANK	3
#define ARRAY2_DIM1 3
#define ARRAY2_DIM2 4
#define ARRAY2_DIM3 5

/* 2-D array datatype */
#define ARRAY3_RANK	2
#define ARRAY3_DIM1 6
#define ARRAY3_DIM2 3

static void gent_group(void)
{
    hid_t fid, group;
  
    fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
    /* / */
    group = H5Gcreate (fid, "/g1", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3", 0);
    H5Gclose(group);
  
    /* /g1 */
    group = H5Gcreate (fid, "/g1/g1.1", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g1/g1.2", 0);
    H5Gclose(group);
  
    /* /g2 */
    group = H5Gcreate (fid, "/g2/g2.1", 0);
    H5Gclose(group);
  
    /* /g3 */
    group = H5Gcreate (fid, "/g3/g3.1", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.2", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.3", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.4", 0);
    H5Gclose(group);
  
    /* /g2/g2.1 */
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.1", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.2", 0);
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.3", 0);
    H5Gclose(group);
  
    H5Fclose(fid);
}

static void gent_dataset(void)
{
    hid_t fid, dataset, space;
    hsize_t dims[2];
    int dset1[10][20];
    double dset2[30][20];
    int i, j;
  
    fid = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
    /* dset1 */
    dims[0] = 10; dims[1] = 20;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 20; j++)
              dset1[i][j] = j+i;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Sclose(space);
    H5Dclose(dataset);
  
    /* dset2 */
    dims[0] = 30; dims[1] = 20;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset2", H5T_IEEE_F64BE, space, H5P_DEFAULT);

    for (i = 0; i < 30; i++)
         for (j = 0; j < 20; j++)
              dset2[i][j] = 0.0001*j+i;

    H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
}

static void gent_dataset2(void)
{
    hid_t fid, dataset, space, create_plist;
    hsize_t dims[2];
    hsize_t maxdims[2];
    int dset1[10][20];
    double dset2[30][10];
    int i, j;
  
    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    create_plist = H5Pcreate(H5P_DATASET_CREATE);
    dims[0] = 5; dims[1] = 5;
    H5Pset_chunk(create_plist, 2, dims);
  
    /* dset1 */
    dims[0] = 10; dims[1] = 20;
    maxdims[0] = H5S_UNLIMITED; maxdims[1] = 20;
    space = H5Screate_simple(2, dims,  maxdims);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, create_plist);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 20; j++)
              dset1[i][j] = j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Sclose(space);
    H5Dclose(dataset);
  
    /* dset2 */
    dims[0] = 30; dims[1] = 10;
    maxdims[0] = 30; maxdims[1] = H5S_UNLIMITED;
    space = H5Screate_simple(2, dims, maxdims);
    dataset = H5Dcreate(fid, "/dset2", H5T_IEEE_F64BE, space, create_plist);

    for (i = 0; i < 30; i++)
         for (j = 0; j < 10; j++)
              dset2[i][j] = j;

    H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

    H5Sclose(space);
    H5Dclose(dataset);
    H5Pclose(create_plist);
    H5Fclose(fid);
}


static void gent_attribute(void)
{
    hid_t fid, root, space, attr, type;
    hsize_t dims[2];
    char buf[60];
    int i, data[10];
    double d[10];
    char string[]= "string attribute";
    int point = 100;
  
    fid = H5Fcreate(FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen (fid, "/");
  
    /* attribute 1 */
    dims[0] = 24;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate (root, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT);
    sprintf(buf, "attribute of root group");
    H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
    H5Sclose(space);
    H5Aclose(attr);
  
    /* attribute 2 */
    dims[0] = 10;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate (root, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++) data[i] = i+1;

    H5Awrite(attr, H5T_NATIVE_INT, data);
    H5Sclose(space);
    H5Aclose(attr);
  
    /* attribute 3 */
    dims[0] = 10;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate (root, "attr3", H5T_IEEE_F64BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++) d[i] = 0.1 * i;

    H5Awrite(attr, H5T_NATIVE_DOUBLE, d);
    H5Sclose(space);
    H5Aclose(attr);
  
    /* attribute 4 */
    space = H5Screate(H5S_SCALAR);
    attr = H5Acreate (root, "attr4", H5T_STD_I32BE, space, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT, &point);
    H5Sclose(space);
    H5Aclose(attr);
  
    /* attribute 5 */
    space = H5Screate(H5S_SCALAR);
    type = H5Tcopy(H5T_C_S1);
    H5Tset_size(type, 17);
    attr = H5Acreate (root, "attr5", type, space, H5P_DEFAULT);
    H5Awrite(attr, type, string);

    H5Tclose(type);
    H5Sclose(space);
    H5Aclose(attr);
    H5Gclose(root);
    H5Fclose(fid);
}

static void gent_softlink(void)
{
    hid_t fid, root;
  
    fid = H5Fcreate(FILE4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen (fid, "/");
    H5Glink (root, H5G_LINK_SOFT, "somevalue", "slink1");
    H5Glink (root, H5G_LINK_SOFT, "linkvalue", "slink2");

    H5Gclose(root);
    H5Fclose(fid);
}

/*
            /

       /    |   \      the dataset is hardlinked to three names
                       /dset1, /g1/dset2, and /g1/g1.1/dset3
     dset1 g1    g2
                       /g2 and /g1/g1.1 are hardlinked to the same object.
          /  \
       dset2 g1.1
              |
             dset3
*/

static void gent_hardlink(void)
{
    hid_t fid, group, dataset, space;
    hsize_t dim = 5;
    int i, dset[5];
  
    fid = H5Fcreate(FILE5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
    space = H5Screate_simple(1, &dim, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 5; i++) dset[i] = i;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
  
    group = H5Gcreate (fid, "/g1", 0);
    H5Glink (group, H5G_LINK_HARD, "/dset1", "dset2");
    H5Gclose(group);
  
    group = H5Gcreate (fid, "/g2", 0);
    H5Glink (group, H5G_LINK_HARD, "/dset1", "dset3");
    H5Gclose(group);
  
    group = H5Gopen(fid, "/g1");
    H5Glink (group, H5G_LINK_HARD, "/g2", "g1.1");
    H5Gclose(group);
    H5Fclose(fid);
}

/*
               /
     /     |       \     \
   dset1  group1  type1 type2
           |
          dset2

*/
static void gent_compound_dt(void) {       /* test compound data type */
    hid_t fid, group, dataset, space, space3, type, type2;
    hid_t array_dt;
    typedef struct {
      int a;
      float b;
      double c;
    } dset1_t;
    dset1_t dset1[5];

    typedef struct {
      int a;
      float b;
    } dset2_t;
    dset2_t dset2[5];

    typedef struct {
      int a[4];
      float b[5][6];
    } dset3_t;
    dset3_t dset3[3][6];

    typedef struct {
      int a;
      float b;
    } dset4_t;
    dset4_t dset4[5];

    typedef struct {
      int a;
      float b;
    } dset5_t;
    dset5_t dset5[5];

    int i, j, k, l, ndims;
    hsize_t dim[2];

    hsize_t sdim = 5;
    hsize_t dset3_dim[2];

  
  for (i = 0; i < (int)sdim; i++) {
       dset1[i].a = i; 
       dset1[i].b = (float)(i*i);
       dset1[i].c = (float)(1./(i+1));
       
       dset2[i].a = i;
       dset2[i].b = (float)(i+ i*0.1);

       dset4[i].a = i;
       dset4[i].b = (float)(i+3);

       dset5[i].a = i;
       dset5[i].b = (float)(i*0.1);
  }


  fid = H5Fcreate(FILE6, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  space = H5Screate_simple(1, &sdim, NULL);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
  type2 = H5Tcreate(H5T_COMPOUND, sizeof(dset1[0])); 
  H5Tinsert(type, "a_name", HOFFSET(dset1_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "b_name", HOFFSET(dset1_t, b), H5T_IEEE_F32BE);
  H5Tinsert(type, "c_name", HOFFSET(dset1_t, c), H5T_IEEE_F64BE);
  H5Tinsert(type2, "a_name", HOFFSET(dset1_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "b_name", HOFFSET(dset1_t, b), H5T_NATIVE_FLOAT);
  H5Tinsert(type2, "c_name", HOFFSET(dset1_t, c), H5T_NATIVE_DOUBLE);
  dataset = H5Dcreate(fid, "/dset1", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
  H5Tclose(type2);
  H5Tclose(type);
  H5Dclose(dataset);

  /* shared data type 1 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type, "int_name", HOFFSET(dset2_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float_name", HOFFSET(dset2_t, b), H5T_IEEE_F32BE);
  H5Tcommit(fid, "type1", type);
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type2, "int_name", HOFFSET(dset2_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float_name", HOFFSET(dset2_t, b), H5T_NATIVE_FLOAT);
  group = H5Gcreate (fid, "/group1", 0);

  dataset = H5Dcreate(group, "dset2", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Tclose(type2);
  H5Tclose(type);
  H5Dclose(dataset);


  /* shared data type 2 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));

  ndims = 1; dim[0] = 4;

  array_dt=H5Tarray_create(H5T_STD_I32BE,ndims,dim,NULL);
  H5Tinsert(type, "int_array", HOFFSET(dset3_t, a), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_INT,ndims,dim,NULL);
  H5Tinsert(type2, "int_array", HOFFSET(dset3_t, a), array_dt);
  H5Tclose(array_dt);

  ndims = 2; dim[0] = 5; dim[1] = 6;

  array_dt=H5Tarray_create(H5T_IEEE_F32BE,ndims,dim,NULL);
  H5Tinsert(type, "float_array", HOFFSET(dset3_t, b), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_FLOAT,ndims,dim,NULL);
  H5Tinsert(type2, "float_array", HOFFSET(dset3_t, b), array_dt);
  H5Tclose(array_dt);

  H5Tcommit(fid, "type2", type);


  dset3_dim[0] = 3;  dset3_dim[1] = 6;
  space3 = H5Screate_simple(2, dset3_dim, NULL);
  dataset = H5Dcreate(group, "dset3", type, space3, H5P_DEFAULT);
  for (i = 0; i < (int)dset3_dim[0]; i++) {
       for (j = 0; j < (int)dset3_dim[1]; j++) {
            for (k = 0; k < 4; k++)
                 dset3[i][j].a[k] = k+j+i;
            for (k = 0; k < 5; k++)
                 for (l = 0; l < 6; l++)
                      dset3[i][j].b[k][l] = (float)((k+1)+l+j+i);
       }
  }
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);
  H5Sclose(space3);
  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);

  /* shared data type 3 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  H5Tinsert(type, "int", HOFFSET(dset4_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset4_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type3", type);
  H5Tinsert(type2, "int", HOFFSET(dset4_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float", HOFFSET(dset4_t, b), H5T_NATIVE_FLOAT);
  dataset = H5Dcreate(group, "dset4", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);
  H5Gclose(group);


  /* unamed data type */
  group = H5Gcreate (fid, "/group2", 0);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type, "int", HOFFSET(dset5_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset5_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type4", type);
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type2, "int", HOFFSET(dset5_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float", HOFFSET(dset5_t, b), H5T_NATIVE_FLOAT);
  dataset = H5Dcreate(group, "dset5", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset5);

  H5Gunlink(group,"type4");

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);
  H5Sclose(space);
  H5Gclose(group);

  H5Fclose(fid);

}

/*
               /
     /     |       \     \
   dset1  group1  type1 type2
           |
          dset2

*/
static void gent_compound_dt2(void) {       /* test compound data type */
    hid_t fid, group, dataset, space, type, create_plist, type2;
    hid_t array_dt;

    typedef struct {
      int a;
      float b;
      double c;
    } dset1_t;
    dset1_t dset1[10];

    typedef struct {
      int a;
      float b;
    } dset2_t;
    dset2_t dset2[10];

    typedef struct {
      int a[4];
      float b[5][6];
    } dset3_t;

    typedef struct {
      int a;
      float b;
    } dset4_t;
    dset4_t dset4[10];

    typedef struct {
      int a;
      float b;
    } dset5_t;
    dset5_t dset5[10];

    int i, ndims;
    const int perm[2]={0,1};
    hsize_t dim[2];

    hsize_t sdim, maxdim;

  sdim = 10;
  for (i = 0; i < (int)sdim; i++) {
       dset1[i].a = i; 
       dset1[i].b = (float)(i*i);
       dset1[i].c = (float)(1./(i+1));
       
       dset2[i].a = i;
       dset2[i].b = (float)(i+ i*0.1);

       dset4[i].a = i;
       dset4[i].b = (float)(i*1.0);

       dset5[i].a = i;
       dset5[i].b = (float)(i*1.0);
  }

  fid = H5Fcreate(FILE9, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  create_plist = H5Pcreate(H5P_DATASET_CREATE);

  sdim = 2;
  H5Pset_chunk(create_plist, 1, &sdim);

  sdim = 6;
  maxdim = H5S_UNLIMITED;

  space = H5Screate_simple(1, &sdim, &maxdim);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
  
  H5Tinsert(type, "a_name", HOFFSET(dset1_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "b_name", HOFFSET(dset1_t, b), H5T_IEEE_F32BE);
  H5Tinsert(type, "c_name", HOFFSET(dset1_t, c), H5T_IEEE_F64BE);

  dataset = H5Dcreate(fid, "/dset1", type, space, create_plist);

  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
  
  H5Tinsert(type2, "a_name", HOFFSET(dset1_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "b_name", HOFFSET(dset1_t, b), H5T_NATIVE_FLOAT);
  H5Tinsert(type2, "c_name", HOFFSET(dset1_t, c), H5T_NATIVE_DOUBLE);

  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

  H5Tclose(type);
  H5Tclose(type2);
  H5Sclose(space);
  H5Dclose(dataset);

  sdim = 6;
  maxdim = 10;

  space = H5Screate_simple(1, &sdim, &maxdim);

  /* shared data type 1 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type, "int_name", HOFFSET(dset2_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float_name", HOFFSET(dset2_t, b), H5T_IEEE_F32BE);
  H5Tcommit(fid, "type1", type);

  group = H5Gcreate (fid, "/group1", 0);

  dataset = H5Dcreate(group, "dset2", type, space, create_plist);

  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type2, "int_name", HOFFSET(dset2_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float_name", HOFFSET(dset2_t, b), H5T_NATIVE_FLOAT); 
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);


  /* shared data type 2 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));

  ndims = 1; dim[0] = 4;
  array_dt=H5Tarray_create(H5T_STD_I32BE,ndims,dim,perm);
  H5Tinsert(type, "int_array", HOFFSET(dset3_t, a), array_dt);
  H5Tclose(array_dt);

  ndims = 2; dim[0] = 5; dim[1] = 6;
  array_dt=H5Tarray_create(H5T_IEEE_F32BE,ndims,dim,perm);
  H5Tinsert(type, "float_array", HOFFSET(dset3_t, b), array_dt);
  H5Tclose(array_dt);

  H5Tcommit(fid, "type2", type);
  H5Tclose(type);

  /* shared data type 3 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  H5Tinsert(type, "int", HOFFSET(dset4_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset4_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type3", type);

  dataset = H5Dcreate(group, "dset4", type, space, create_plist);

  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  H5Tinsert(type2, "int", HOFFSET(dset4_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float", HOFFSET(dset4_t, b), H5T_NATIVE_FLOAT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);
  H5Gclose(group);


  /* unamed data type */
  group = H5Gcreate (fid, "/group2", 0);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type, "int", HOFFSET(dset5_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset5_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type4", type);
  dataset = H5Dcreate(group, "dset5", type, space, create_plist);
  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type2, "int", HOFFSET(dset5_t, a), H5T_NATIVE_INT);
  H5Tinsert(type2, "float", HOFFSET(dset5_t, b), H5T_NATIVE_FLOAT);
  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset5);

  H5Gunlink(group,"type4");

  H5Tclose(type);
  H5Tclose(type2);
  H5Dclose(dataset);
  H5Sclose(space);
  H5Gclose(group);
  H5Pclose(create_plist);

  H5Fclose(fid);

}


/*

/	: g1  g2  attr1  attr2
g1	: g1.1  g1.2
g1.1	: dset1.1.1(attr1, attr2)   dset1.1.2
g1.2	: g1.2.1
g1.2.1	: slink
g2	: dset2.1  dset2.2

*/

static void gent_all(void) {
hid_t fid, group, attr, dataset, space;
hsize_t dims[2];
int data[2][2], dset1[10][10], dset2[20];
char buf[60];
int i, j;
float dset2_1[10], dset2_2[3][5];

  fid = H5Fcreate(FILE7, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /* create groups */
  group = H5Gcreate (fid, "/g1", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g2", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g1/g1.1", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g1/g1.2", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g1/g1.2/g1.2.1", 0);
  H5Gclose(group);

  /* root attributes */
  group = H5Gopen (fid, "/");

  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (group, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT);
  sprintf(buf, "abcdefghi");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 2; dims[1] = 2;
  space = H5Screate_simple(2, dims, NULL);
  attr = H5Acreate (group, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT);
  data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
  H5Awrite(attr, H5T_NATIVE_INT, data);
  H5Sclose(space);
  H5Aclose(attr);

  H5Gclose(group);

  group = H5Gopen (fid, "/g1/g1.1");

  /* dset1.1.1 */
  dims[0] = 10; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(group, "dset1.1.1", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
            dset1[i][j] = j*i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
  H5Sclose(space);

  /* attributes of dset1.1.1 */
  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT);
  sprintf(buf, "1st attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr2", H5T_STD_I8BE, space, H5P_DEFAULT);
  sprintf(buf, "2nd attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  H5Dclose(dataset);

  /* dset1.1.2 */
  dims[0] = 20;
  space = H5Screate_simple(1, dims, NULL);
  dataset = H5Dcreate(group, "dset1.1.2", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 20; i++)
       dset2[i] = i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Gclose(group);

  /* soft link */
  group = H5Gopen (fid, "/g1/g1.2/g1.2.1");
  H5Glink (group, H5G_LINK_SOFT, "somevalue", "slink");
  H5Gclose(group);

  group = H5Gopen (fid, "/g2");

  /* dset2.1 */
  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  dataset = H5Dcreate(group, "dset2.1", H5T_IEEE_F32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       dset2_1[i] = (float)(i*0.1+1);
  H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_1);
  H5Sclose(space);
  H5Dclose(dataset);
 
  /* dset2.2 */
  dims[0] = 3; dims[1] = 5;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(group, "dset2.2", H5T_IEEE_F32BE, space, H5P_DEFAULT);
  for (i = 0; i < 3; i++)
       for (j = 0; j < 5; j++)
            dset2_2[i][j] = (float)((i+1)*j*0.1);
  H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Gclose(group);

  H5Fclose(fid);

}

/*
            o
          /___\
      g1 o/   \o g2
          \___/  

   
o - group objects

*/

static void gent_loop(void) {
hid_t fid, group;

  fid = H5Fcreate(FILE10, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  group = H5Gcreate (fid, "/g1", 0);
  H5Gclose(group);
  group = H5Gcreate (fid, "/g2", 0);
  H5Gclose(group);

  H5Glink(fid, H5G_LINK_HARD, "/g2", "/g1/g1.1");
  H5Glink(fid, H5G_LINK_HARD, "/g1", "/g2/g2.1");

  H5Fclose(fid);
}

static void gent_loop2(void) {
hid_t fid, group;

  fid = H5Fcreate(FILE11, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /* create group object g1 and implcit path from root object */
  group = H5Gcreate (fid, "/g1", 0);
  H5Gclose(group);

  /* create group object g2 and implcit path from root object */
  group = H5Gcreate (fid, "/g2", 0);
  H5Gclose(group);

  /* create path from object at /g1 to object at /g2 and name it g1.1 */
  H5Glink (fid, H5G_LINK_HARD, "/g2", "/g1/g1.1"); 

  /* create path from object at /g2 to object at /g1 and name it g2.1 */
  H5Glink (fid, H5G_LINK_SOFT, "/g1", "/g2/g2.1");

  H5Fclose(fid);

}

/*
                  /
     |       |       |   \    \    \
     g1     g2      g3   g4   g5    g6
    / \      |       |    \     \    \
 g1.1 g1.2 slink2  link3 dset2 slink4 dset3
  |    |    (g1)  (dset2)      (dset3)
 dset1 link1
      (dset1)
*/

static void gent_many(void) {
    hid_t fid, group, attr, dataset, space, space2, type, create_plist, type2;
    hid_t array_dt;
    hsize_t dims[2];
    int data[2][2], dset2[10][10], dset3[10][10];
    double d[10];

    char buf[60];
    int i, j;
    int i0, i1, i2, i3;
    hsize_t sdim, maxdim;

    typedef struct {	/* compound type has members with rank > 1	*/
      int a[2][2][2][2];	/* arrays are 2x2x2x2				*/
      double b[2][2][2][2];
      double c[2][2][2][2];
    } dset1_t;
    dset1_t dset1[6];

    hsize_t dim[4];
    int idx[4] = {0,1,2,3};  /* normal indicies */
    const int perm[4] = {0,1,2,3};  /* the 0'th and the 3'rd indices are permuted */

  fid = H5Fcreate(FILE12, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  group = H5Gcreate (fid, "/g1", 0);
  H5Gclose(group);

  create_plist = H5Pcreate(H5P_DATASET_CREATE);

  sdim = 2;
  H5Pset_chunk(create_plist, 1, &sdim);

  group = H5Gcreate (fid, "/g1/g1.1", 0);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));

  dim[0] = dim[1] = dim[2] = dim[3] = 2;
  array_dt=H5Tarray_create(H5T_STD_I32BE,4,dim,perm);
  H5Tinsert(type, "a_array", HOFFSET(dset1_t, a), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_IEEE_F64BE,4,dim,perm);
  H5Tinsert(type, "b_array", HOFFSET(dset1_t, b), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_IEEE_F64BE,4,dim,perm);
  H5Tinsert(type, "c_array", HOFFSET(dset1_t, c), array_dt);
  H5Tclose(array_dt);

  type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));

  array_dt=H5Tarray_create(H5T_NATIVE_INT,4,dim,perm);
  H5Tinsert(type2, "a_array", HOFFSET(dset1_t, a), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_DOUBLE,4,dim,perm);
  H5Tinsert(type2, "b_array", HOFFSET(dset1_t, b), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_DOUBLE,4,dim,perm);
  H5Tinsert(type2, "c_array", HOFFSET(dset1_t, c), array_dt);
  H5Tclose(array_dt);


  /* dset1 */
  sdim = 6;
  maxdim = H5S_UNLIMITED;
  space = H5Screate_simple(1, &sdim, &maxdim);
  dataset = H5Dcreate(group, "dset1", type, space, create_plist);

  /* add attributes to dset1 */
  dims[0] = 10;
  space2 = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr1", H5T_STD_I8BE, space2, H5P_DEFAULT);
  sprintf(buf, "abcdefghi");
  H5Awrite(attr, H5T_NATIVE_CHAR, buf);
  H5Sclose(space2);
  H5Aclose(attr);

  dims[0] = 2; dims[1] = 2;
  space2 = H5Screate_simple(2, dims, NULL);
  attr = H5Acreate (dataset, "attr2", H5T_STD_I32BE, space2, H5P_DEFAULT);
  data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
  H5Awrite(attr, H5T_NATIVE_INT, data);
  H5Sclose(space2);
  H5Aclose(attr);

  dims[0] = 10;
  space2 = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr3", H5T_IEEE_F64BE, space2, H5P_DEFAULT);
  for (i = 0; i < 10; i++) d[i] = 0.1 * i;
  H5Awrite(attr, H5T_NATIVE_DOUBLE, d);
  H5Sclose(space2);
  H5Aclose(attr);

  for (j=0; j<(int)sdim; j++) {
	for (i3 = 0; i3 < 2; i3++) {
		idx[perm[3]] = i3;
	for (i2 = 0; i2 < 2; i2++) {
		idx[perm[2]] = i2;
	for (i1 = 0; i1 < 2; i1++) {
		idx[perm[1]] = i1;
	for (i0 = 0; i0 < 2; i0++) {
		idx[perm[0]] = i0;
		
		dset1[j].a[idx[3]][idx[2]][idx[1]][idx[0]] = i0+j;
		dset1[j].b[idx[3]][idx[2]][idx[1]][idx[0]] = (double)(i0+j);
#ifdef WIN32
		dset1[j].c[idx[3]][idx[2]][idx[1]][idx[0]] = (double)(i0+j+(signed __int64)sdim);
#else
		dset1[j].c[idx[3]][idx[2]][idx[1]][idx[0]] = (double)(i0+j+sdim);
#endif
	}
	}
	}
	}
  }

  H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

  H5Dclose(dataset);
  H5Sclose(space);

  H5Tclose(type);
  H5Tclose(type2);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g1/g1.2", 0);
  H5Glink (group, H5G_LINK_HARD, "/g1/g1.1/dset1", "link1");
  H5Gclose(group);

  group = H5Gcreate (fid, "/g2", 0);
  H5Glink (group, H5G_LINK_SOFT, "/g1", "slink2");
  H5Gclose(group);

  group = H5Gcreate (fid, "/g3", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g4", 0);

  /* dset2 */
  dims[0] = 10; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);

  dataset = H5Dcreate(group, "dset2", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
            dset2[i][j] = j;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

  H5Dclose(dataset);

  H5Sclose(space);
  H5Gclose(group);

  group = H5Gopen(fid, "/g3");
  H5Glink (group, H5G_LINK_HARD, "/g4/dset2", "link3");
  H5Gclose(group);

  group = H5Gcreate (fid, "/g5", 0);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g6", 0);
  /* dset3 */
  dims[0] = 10; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);

  dataset = H5Dcreate(group, "dset3", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
            dset3[i][j] = i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);

  H5Dclose(dataset);

  H5Sclose(space);
  H5Gclose(group);

  group = H5Gopen(fid, "/g5");
  H5Glink (group, H5G_LINK_SOFT, "/g6/dset3", "slink4");
  H5Gclose(group);
  H5Pclose(create_plist);

  H5Fclose(fid);

}
static hid_t mkstr(int size, H5T_str_t pad) {
hid_t type;

  if ((type=H5Tcopy(H5T_C_S1))<0) return -1;
  if (H5Tset_size(type, (size_t)size)<0) return -1;
  if (H5Tset_strpad(type, pad)<0) return -1;

  return type;
}

static void gent_str(void) {
    hid_t fid, dataset, space, f_type, m_type, str_type, f_type2;
    hid_t array_dt;

    hsize_t dims1[] = { 3, 4};
    char string1[12][2] = {"s1","s2","s3","s4","s5","s6","s7","s8","s9",
                           "s0","s1","s2"};

    hsize_t dims2[]={20};
    char string2[20][9] = {"ab cd ef1", "ab cd ef2", "ab cd ef3", "ab cd ef4",
                        "ab cd ef5", "ab cd ef6", "ab cd ef7", "ab cd ef8", 
                        "ab cd ef9", "ab cd ef0", "ab cd ef1", "ab cd ef2", 
                        "ab cd ef3", "ab cd ef4", "ab cd ef5", "ab cd ef6", 
                        "ab cd ef7", "ab cd ef8", "ab cd ef9", "ab cd ef0"};

    hsize_t dims3[] = { 27};
    char string3[27][5] = {"abcd0", "abcd1", "abcd2", "abcd3", 
                       "abcd4", "abcd5", "abcd6", "abcd7", 
                       "abcd8", "abcd9", "abcd0", "abcd1", 
                       "abcd2", "abcd3", "abcd4", "abcd5", 
                       "abcd6", "abcd7", "abcd8", "abcd9", 
                       "abcd0", "abcd1", "abcd2", "abcd3", 
                       "abcd4", "abcd5", "abcd6"};

    int i, j, k, l;

    hsize_t dims4[] = { 3 };
    char string4[3][20] = { "s1234567890123456789", "s1234567890123456789",
                            "s1234567890123456789"};

    hsize_t dims5[] = { 3, 6};
    typedef struct {
      int a[8][10];
      char s[12][32];
    } compound_t;
    compound_t comp1[3][6];
    hsize_t mdims[2];

  fid = H5Fcreate(FILE13, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /* string 1 : nullterm string */
  space = H5Screate_simple(2, dims1, NULL);
  f_type = mkstr(5, H5T_STR_NULLTERM);
  m_type = mkstr(2, H5T_STR_NULLTERM);
  dataset = H5Dcreate(fid, "/string1", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string1);
  H5Tclose(m_type);
  H5Tclose(f_type);
  H5Sclose(space);
  H5Dclose(dataset);

  /* string 2 : space pad string */
  space = H5Screate_simple(1, dims2, NULL);
  f_type = mkstr(11, H5T_STR_SPACEPAD);
  m_type = mkstr(9, H5T_STR_NULLTERM);
  dataset = H5Dcreate(fid, "/string2", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string2);
  H5Tclose(m_type);
  H5Tclose(f_type);
  H5Sclose(space);
  H5Dclose(dataset);

  /* string 3 : null pad string */
  space = H5Screate_simple(1, dims3, NULL);
  f_type = mkstr(8, H5T_STR_NULLPAD);
  m_type = mkstr(5, H5T_STR_NULLTERM);
  dataset = H5Dcreate(fid, "/string3", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string3);
  H5Tclose(m_type);
  H5Tclose(f_type);
  H5Sclose(space);
  H5Dclose(dataset);

  /* string 4 : space pad long string */
  space = H5Screate_simple(1, dims4, NULL);
  f_type = mkstr(168, H5T_STR_SPACEPAD);
  m_type = mkstr(20, H5T_STR_NULLTERM);
  dataset = H5Dcreate(fid, "/string4", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string4);
  H5Tclose(m_type);
  H5Tclose(f_type);
  H5Sclose(space);
  H5Dclose(dataset);

  /* compound data */
  space = H5Screate_simple(2, dims5, NULL);
  f_type = H5Tcreate (H5T_COMPOUND, sizeof(compound_t));
  f_type2 = H5Tcreate (H5T_COMPOUND, sizeof(compound_t));

  mdims[0] = 8; mdims[1] = 10;

  array_dt=H5Tarray_create(H5T_STD_I32BE,2,mdims,NULL);
  H5Tinsert(f_type, "int_array", HOFFSET(compound_t, a), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(H5T_NATIVE_INT,2,mdims,NULL);
  H5Tinsert(f_type2, "int_array", HOFFSET(compound_t, a), array_dt);
  H5Tclose(array_dt);

  str_type = mkstr(32, H5T_STR_SPACEPAD);
  mdims[0] = 3; mdims[1] = 4;

  array_dt=H5Tarray_create(str_type,2,mdims,NULL);
  H5Tinsert(f_type, "string", HOFFSET(compound_t, s), array_dt);
  H5Tclose(array_dt);

  array_dt=H5Tarray_create(str_type,2,mdims,NULL);
  H5Tinsert(f_type2, "string", HOFFSET(compound_t, s), array_dt);
  H5Tclose(array_dt);

  for (i = 0; i < 3; i++)
      for (j = 0; j < 6; j++) {
           for (k = 0 ; k < 8; k++)
                for (l = 0; l < 10; l++)
                   comp1[i][j].a[k][l] = (l+j+k) * (l+j+k);
           for (k = 0 ; k < 12; k++)
               sprintf(comp1[i][j].s[k], "abcdefgh12345678abcdefgh12345678");
      }
           
  dataset = H5Dcreate(fid, "/comp1", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, f_type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, comp1);

  H5Tclose(str_type);
  H5Tclose(f_type);
  H5Tclose(f_type2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Fclose(fid);
}

/*
                      /
       /     /     |    \    \     \
     g1     g2    g3    g4    g5    g6
     |       |     |     |     \     \ 
  string1       string3       string5 
         string2       string4       string6
*/

static void gent_str2(void)
{
hid_t fid, group, attr, dataset, space, space2, mem_space, hyper_space;
hid_t fxdlenstr, fxdlenstr2, memtype;
hsize_t dims[1], size[1], stride[1], count[1], block[1];
hssize_t start[1];


int i;
char buf[LENSTR+10];
char buf2[3*LENSTR2];
hsize_t sdim;

  fid = H5Fcreate(FILE14, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  fxdlenstr = H5Tcopy(H5T_C_S1);
  H5Tset_size(fxdlenstr, LENSTR);
  H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
  H5Tset_strpad(fxdlenstr, H5T_STR_NULLTERM);

  memtype = H5Tcopy(H5T_C_S1);
  H5Tset_size(memtype, LENSTR);
  H5Tset_cset(memtype, H5T_CSET_ASCII);
  H5Tset_strpad(memtype, H5T_STR_NULLTERM);

  sdim = 10;
  size[0] = sdim;
  space = H5Screate_simple(1, size, NULL);
  size[0] = 1;
  mem_space = H5Screate_simple(1,size,NULL);
  hyper_space = H5Scopy(space);

  /* dset1 */

  group = H5Gcreate (fid, "/g1", 0);
  dataset = H5Dcreate(group, "dset1", fxdlenstr, space, H5P_DEFAULT);

  /* add attributes to dset1 */

  fxdlenstr2 = H5Tcopy(H5T_C_S1);
  H5Tset_size(fxdlenstr2, LENSTR2);
  H5Tset_cset(fxdlenstr2, H5T_CSET_ASCII);
  H5Tset_strpad(fxdlenstr2, H5T_STR_NULLTERM);

  dims[0] = 3;
  space2 = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr1", fxdlenstr2, space2, H5P_DEFAULT);
  sprintf(&(buf2[0*LENSTR2]), "0123456789");
  sprintf(&(buf2[1*LENSTR2]), "abcdefghij");
  sprintf(&(buf2[2*LENSTR2]), "ABCDEFGHIJ");
  H5Awrite(attr, fxdlenstr2, buf2);
  H5Sclose(space2);
  H5Tclose(fxdlenstr2);
  H5Aclose(attr);

  stride[0]=1;
  count[0]=1;
  block[0]=1;

  for (i = 0; (hsize_t)i < sdim; i++) {
	start[0] = i;
	sprintf(buf,"This is row %1d of type H5T_STR_NULLTERM of",i);
  	H5Tset_size(memtype, HDstrlen(buf)+1);
	H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
  	H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);

  group = H5Gcreate (fid, "/g2", 0);
  dataset = H5Dcreate(group, "dset2", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0; (hsize_t)i < sdim; i++) {
	start[0] = i;
	sprintf(buf,"This is row %1d of type H5T_STR_NULLTERM of string array",i);
  	H5Tset_size(memtype, HDstrlen(buf)+1);
	H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
  	H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);


  H5Tclose(fxdlenstr);
  fxdlenstr = H5Tcopy(H5T_C_S1);
  H5Tset_size(fxdlenstr, LENSTR);
  H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
  H5Tset_strpad(fxdlenstr, H5T_STR_NULLPAD);

  group = H5Gcreate (fid, "/g3", 0);
  dataset = H5Dcreate(group, "dset3", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0;(hsize_t) i < sdim; i++) {
	start[0] = i;
	sprintf(buf,"This is row %1d of type H5T_STR_NULLPAD of",i);
  	H5Tset_size(memtype, HDstrlen(buf)+1);
	H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
  	H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);


  group = H5Gcreate (fid, "/g4", 0);
  dataset = H5Dcreate(group, "dset4", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0; (hsize_t)i < sdim; i++) {
	start[0] = i;
	sprintf(buf,"This is row %1d of type H5T_STR_NULLPAD of string array",i);
  	H5Tset_size(memtype, HDstrlen(buf)+1);
	H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
  	H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);

  H5Tclose(fxdlenstr);
  fxdlenstr = H5Tcopy(H5T_C_S1);
  H5Tset_size(fxdlenstr, LENSTR);
  H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
  H5Tset_strpad(fxdlenstr, H5T_STR_SPACEPAD);

  group = H5Gcreate (fid, "/g5", 0);
  dataset = H5Dcreate(group, "dset5", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0; (hsize_t)i < sdim; i++) {
	start[0] = i;
	sprintf(buf,"This is row %1d of type H5T_STR_SPACEPAD of",i);
  	H5Tset_size(memtype, HDstrlen(buf)+1);
	H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
  	H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);


  group = H5Gcreate (fid, "/g6", 0);
  dataset = H5Dcreate(group, "dset6", fxdlenstr, space, H5P_DEFAULT);

  for (i = 0; (hsize_t)i < sdim; i++) {
	start[0] = i;
	sprintf(buf,"This is row %1d of type H5T_STR_SPACEPAD of string array",i);
  	H5Tset_size(memtype, HDstrlen(buf)+1);
	H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
  	H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }

  H5Dclose(dataset);
  H5Tclose(fxdlenstr);
  H5Tclose(memtype);
  H5Sclose(mem_space);
  H5Sclose(hyper_space);
  H5Sclose(space);
  H5Fclose(fid);
}

static void gent_enum(void)
{
    /*some code is taken from enum.c in the test dir */
    hid_t file, type, space, dset;
    int val;
    enumtype data[] = {RED,   GREEN, BLUE,  GREEN, WHITE,
		       WHITE, BLACK, GREEN, BLUE,  RED,
		       RED,   BLUE,  GREEN, BLACK, WHITE,
		       RED,   WHITE, GREEN, GREEN, BLUE};
    hsize_t size[1] = {NELMTS(data)};

    file = H5Fcreate(FILE15,H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);


    type = H5Tcreate(H5T_ENUM, sizeof(enumtype));
    H5Tenum_insert(type, "RED",   (val = 0, &val));
    H5Tenum_insert(type, "GREEN", (val = 1, &val));
    H5Tenum_insert(type, "BLUE",  (val = 2, &val));
    H5Tenum_insert(type, "WHITE", (val = 3, &val));
    H5Tenum_insert(type, "BLACK", (val = 4, &val));
    H5Tcommit(file, "enum normal", type);

    space = H5Screate_simple(1,size,NULL);
    dset = H5Dcreate(file,"table",type, space, H5P_DEFAULT);
    H5Dwrite(dset,type,space,space,H5P_DEFAULT,data);

    H5Dclose(dset);
    H5Sclose(space);
    H5Fclose(file);
}

static void gent_objref(void)
{
/*some code is taken from enum.c in the test dir */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/

    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    hobj_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temp. buffer read from disk */
    uint32_t   *tu32;      /* Temporary pointer to uint32 data */
    int        i;          /* counting variables */
    const char *write_comment="Foo!"; /* Comments for group */

    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    rbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    tbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE16, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a group */
    group=H5Gcreate(fid1,"Group1",(size_t)-1);

    /* Set group's comment */
    H5Gset_comment(group,".",write_comment);

    /* Create a dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset1",H5T_STD_U32BE,sid1,H5P_DEFAULT);

    for(tu32=(uint32_t *)((void*)wbuf),i=0; i<SPACE1_DIM1; i++)
        *tu32++=i*3;

    /* Write selection to disk */
    H5Dwrite(dataset,H5T_NATIVE_UINT,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close Dataset */
    H5Dclose(dataset);

    /* Create another dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset2",H5T_STD_U8BE,sid1,H5P_DEFAULT);

    /* Close Dataset */
    H5Dclose(dataset);

    /* Create a datatype to refer to */
    tid1 = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));

    /* Insert fields */
    H5Tinsert (tid1, "a", HOFFSET(s1_t,a), H5T_STD_I32BE);

    H5Tinsert (tid1, "b", HOFFSET(s1_t,b), H5T_IEEE_F32BE);

    H5Tinsert (tid1, "c", HOFFSET(s1_t,c), H5T_IEEE_F32BE);

    /* Save datatype for later */
    H5Tcommit (group, "Datatype1", tid1);

    /* Close datatype */
    H5Tclose(tid1);

    /* Close group */
    H5Gclose(group);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset3",H5T_STD_REF_OBJ,sid1,H5P_DEFAULT);

    /* Create reference to dataset */
    H5Rcreate(&wbuf[0],fid1,"/Group1/Dataset1",H5R_OBJECT,-1);

    /* Create reference to dataset */
    H5Rcreate(&wbuf[1],fid1,"/Group1/Dataset2",H5R_OBJECT,-1);

    /* Create reference to group */
    H5Rcreate(&wbuf[2],fid1,"/Group1",H5R_OBJECT,-1);

    /* Create reference to named datatype */
    H5Rcreate(&wbuf[3],fid1,"/Group1/Datatype1",H5R_OBJECT,-1);

    /* Write selection to disk */
    H5Dwrite(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close disk dataspace */
    H5Sclose(sid1);
   
    /* Close Dataset */
    H5Dclose(dataset);

    /* Close file */
    H5Fclose(fid1);

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(tbuf);

}

static void gent_datareg(void)
{
    /*some code is taken from enum.c in the test dir */

    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dset1,	/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t		sid1,       /* Dataspace ID	#1		*/
                sid2;       /* Dataspace ID	#2		*/
    hsize_t		dims1[] = {SPACE1_DIM1},
            	dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hssize_t	start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t		stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t		count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t		block[SPACE2_RANK];     /* Block size of hyperslab */
    hssize_t	coord1[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hdset_reg_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf;       /* buffer read from disk */
    uint8_t    *dwbuf,      /* Buffer for writing numeric data to disk */
               *drbuf;      /* Buffer for reading numeric data from disk */
    uint8_t    *tu8;        /* Temporary pointer to uint8 data */
    int        i;          /* counting variables */

    /* Allocate write & read buffers */
    wbuf=calloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    rbuf=malloc(sizeof(hdset_reg_ref_t)*SPACE1_DIM1);
    dwbuf=malloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    drbuf=calloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Create file */
    fid1 = H5Fcreate(FILE17, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);

    /* Create a dataset */
    dset2=H5Dcreate(fid1,"Dataset2",H5T_STD_U8BE,sid2,H5P_DEFAULT);

    for(tu8=dwbuf,i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++)
        *tu8++=i*3;

    /* Write selection to disk */
    H5Dwrite(dset2,H5T_NATIVE_UCHAR,H5S_ALL,H5S_ALL,H5P_DEFAULT,dwbuf);

    /* Close Dataset */
    H5Dclose(dset2);

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a dataset */
    dset1=H5Dcreate(fid1,"Dataset1",H5T_STD_REF_DSETREG,sid1,H5P_DEFAULT);

    /* Create references */

    /* Select 6x6 hyperslab for first reference */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=6; count[1]=6;
    block[0]=1; block[1]=1;
    H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);

    H5Sget_select_npoints(sid2);

    /* Store first dataset region */
    H5Rcreate(&wbuf[0],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Select sequence of ten points for second reference */
    coord1[0][0]=6; coord1[0][1]=9;
    coord1[1][0]=2; coord1[1][1]=2;
    coord1[2][0]=8; coord1[2][1]=4;
    coord1[3][0]=1; coord1[3][1]=6;
    coord1[4][0]=2; coord1[4][1]=8;
    coord1[5][0]=3; coord1[5][1]=2;
    coord1[6][0]=0; coord1[6][1]=4;
    coord1[7][0]=9; coord1[7][1]=0;
    coord1[8][0]=7; coord1[8][1]=1;
    coord1[9][0]=3; coord1[9][1]=3;
    H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord1);

    H5Sget_select_npoints(sid2);

    /* Store second dataset region */
    H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Write selection to disk */
    H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close disk dataspace */
    H5Sclose(sid1);
    
    /* Close Dataset */
    H5Dclose(dset1);

    /* Close uint8 dataset dataspace */
    H5Sclose(sid2);
    
    /* Close file */
    H5Fclose(fid1);

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(dwbuf);
    free(drbuf);
}

/*taken from Elena's compound test file*/
static void gent_nestcomp(void)
{
   /* Compound memeber of the compound datatype*/
    typedef struct cmp_t {
        char   a;
        float  b[2];
    } cmp_t;

    /* First structure  and dataset*/
    typedef struct s1_t {
	int    a;
	float  b;
	double c; 
        cmp_t  d; 
    } s2_t;
    hid_t      cmp_tid;    /* Handle for the compound datatype */
    hid_t      char_id;    /* Handle for the string datatype */
    hid_t      array_dt;
    hsize_t     array_dims[] = {2};    /* Dataspace dimensions */
    int        ndims = 1;    /* Number of dimensions in the array field */

    s2_t       s1[10];
    hid_t      s2_tid;     /* File datatype identifier */

    int        i;
    hid_t      file, dataset, space; /* Handles */
    herr_t     status;
    hsize_t    dim[] = {10};   /* Dataspace dimensions */

    char datasetname[] = "ArrayOfStructures";


    /*
     * Initialize the data
     */
    for (i = 0; i< 10; i++) {
        s1[i].a = i;
        s1[i].b = (float)(i*i);
        s1[i].c = 1./(i+1);
        s1[i].d.a = 65 + i;
        s1[i].d.b[0] = -100.;
        s1[i].d.b[1] =  100.;
    }

    /*
     * Create the data space.
     */
    space = H5Screate_simple(1, dim, NULL);

    /*
     * Create the file.
     */
    file = H5Fcreate(FILE18, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create the memory data type. 
     */
    /* 
     * Create a datatype for compound field first.
     */
    cmp_tid = H5Tcreate (H5T_COMPOUND, sizeof(cmp_t));

    /* We are using C string of length one to represent "real" character */
    char_id = H5Tcopy(H5T_C_S1);
    H5Tset_strpad(char_id, H5T_STR_NULLTERM);       
    H5Tinsert(cmp_tid, "char_name", HOFFSET(cmp_t, a), char_id);

    array_dt=H5Tarray_create(H5T_NATIVE_FLOAT,ndims,array_dims,NULL);
    H5Tinsert(cmp_tid, "array_name", HOFFSET(cmp_t, b), array_dt);
    H5Tclose(array_dt);

    s2_tid = H5Tcreate (H5T_COMPOUND, sizeof(s2_t));
    H5Tinsert(s2_tid, "a_name", HOFFSET(s2_t, a), H5T_NATIVE_INT);
    H5Tinsert(s2_tid, "c_name", HOFFSET(s2_t, c), H5T_NATIVE_DOUBLE);
    H5Tinsert(s2_tid, "b_name", HOFFSET(s2_t, b), H5T_NATIVE_FLOAT);

    /* Insert compound memeber created above */
    H5Tinsert(s2_tid, "d_name", HOFFSET(s2_t, d), cmp_tid);

    /* 
     * Create the dataset.
     */
    dataset = H5Dcreate(file, datasetname, s2_tid, space, H5P_DEFAULT);

    /*
     * Wtite data to the dataset; 
     */
    status = H5Dwrite(dataset, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1);
    if (status < 0)
	fprintf(stderr, "gent_nestcomp H5Dwrite failed\n");

    /*
     * Release resources
     */
    H5Tclose(s2_tid);
    H5Tclose(cmp_tid);
    H5Tclose(char_id);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(file);
}

static void gent_opaque(void)
{
    hid_t file, type, dataset, space;
    char test[100][2];
    int x;
    hsize_t dim = 2;

    for (x = 0; x < 100; x++){
        test[x][0] = x;
        test[x][1] = 99 - x;
    }
	
    /*
     * Create the data space.
     */
    space = H5Screate_simple(1, &dim, NULL);

    /*
     * Create the file.
     */
    file = H5Fcreate(FILE19, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create the memory datatype. 
     */
    type = H5Tcreate (H5T_OPAQUE, sizeof(char)*100*2);
    H5Tset_tag(type, "test opaque type");

    /* 
     * Create the dataset.
     */
    dataset = H5Dcreate(file, "opaque test", type, space, H5P_DEFAULT);

    /*
     * Write data to the dataset; 
     */
    H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, test);

    H5Tclose(type);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(file);	
}

static void gent_bitfields(void)
{
    hid_t		file, grp=-1, type=-1, space=-1, dset=-1;
    size_t		i;
    hsize_t		nelmts;
    unsigned char	buf[32];

    file = H5Fcreate(FILE20, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   
    if ((grp=H5Gcreate(file, "typetests", 0))<0) goto error;

    /* bitfield_1 */
    nelmts = sizeof(buf);
    if ((type=H5Tcopy(H5T_STD_B8LE))<0 ||
	(space=H5Screate_simple(1, &nelmts, NULL))<0 ||
	(dset=H5Dcreate(grp, "bitfield_1", type, space, H5P_DEFAULT))<0)
	goto error;

    for (i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
	goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Dclose(dset)<0) goto error;

    /* bitfield_2 */
    nelmts = sizeof(buf)/2;
    if ((type=H5Tcopy(H5T_STD_B16LE))<0 ||
	(space=H5Screate_simple(1, &nelmts, NULL))<0 ||
	(dset=H5Dcreate(grp, "bitfield_2", type, space, H5P_DEFAULT))<0)
	goto error;
    for (i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
	goto error;
    if (H5Sclose(space)<0) goto error;
    if (H5Tclose(type)<0) goto error;
    if (H5Dclose(dset)<0) goto error;
    if (H5Gclose(grp)<0) goto error;
    H5Fclose(file);

 error:
    H5E_BEGIN_TRY {
	H5Gclose(grp);
	H5Tclose(type);
	H5Sclose(space);
	H5Dclose(dset);
    } H5E_END_TRY;
}

static void gent_vldatatypes(void)
{
    hvl_t adata, wdata[SPACE1_DIM1];
    hid_t file, dset, space, type;
    hsize_t dims[] = { SPACE1_DIM1 };
    int i;
    herr_t ret=0;

    file = H5Fcreate(FILE21, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
   
    /* Allocate and initialize VL dataset to write */
    for(i = 0; i < SPACE1_DIM1; i++) {
	int j;

        wdata[i].p = malloc((i + 1) * sizeof(int));
        wdata[i].len = i + 1;

        for (j = 0; j < i + 1; j++)
            ((int *)wdata[i].p)[j] = i * 10 + j;
    }

    /* write out the integers in little-endian format */
    space = H5Screate_simple(SPACE1_RANK, dims, NULL);
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate(file, "Dataset1.0", type, space, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret>=0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, wdata);
    assert(ret>=0);

    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);
    ret = H5Sclose(space);
    assert(ret>=0);

    /* Allocate and initialize VL dataset to write */
    for(i = 0; i < SPACE1_DIM1; i++) {
	int j;

        wdata[i].p = malloc((i + 1) * sizeof(float));
        wdata[i].len = i + 1;

        for (j = 0; j < i + 1; j++)
            ((float *)wdata[i].p)[j] = (float)(i * 10 + ((float)j) / 10.0);
    }

    /* write out the floats in little-endian format */
    space = H5Screate_simple(SPACE1_RANK, dims, NULL);
    type = H5Tvlen_create(H5T_NATIVE_FLOAT);
    dset = H5Dcreate(file, "Dataset2.0", type, space, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret>=0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, wdata);
    assert(ret>=0);

    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);
    ret = H5Sclose(space);
    assert(ret>=0);

    /* Allocate and initialize a scalar VL dataset to write */
    adata.p = malloc(37 * sizeof(int));
    adata.len = 37;

    for (i = 0; i < 37; i++)
        ((int *)adata.p)[i] = i * 2;

    /* write out scalar VL dataset in little-endian format */
    space = H5Screate_simple(0, NULL, NULL);
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate(file, "Dataset3.0", type, space, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, &adata);
    assert(ret>=0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, &adata);
    assert(ret>=0);

    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);
    ret = H5Sclose(space);
    assert(ret>=0);
    ret = H5Fclose(file);
    assert(ret>=0);
}

static void gent_vldatatypes2(void)
{
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t *t1;              /* Temporary pointer to VL information */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1, tid2; /* Datatype IDs         */
    hsize_t		dims1[] = {SPACE1_DIM1};
    unsigned       i,j,k;      /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(hvl_t));
        if(wdata[i].p==NULL) {
            printf("Cannot allocate memory for VL data! i=%u\n",i);
            return;
        } /* end if */
        wdata[i].len=i+1;
        for(t1=wdata[i].p,j=0; j<(i+1); j++, t1++) {
            t1->p=malloc((j+1)*sizeof(unsigned int));
            if(t1->p==NULL) {
                printf("Cannot allocate memory for VL data! i=%u, j=%u\n",i,j);
                return;
            } /* end if */
            t1->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)t1->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE22, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);

    /* Create the base VL type */
    tid2 = H5Tvlen_create (tid1);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid2,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid2,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid2,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid2);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);

}

static void gent_vldatatypes3(void)
{
    typedef struct {             /* Struct that the VL sequences are composed of */
        int i;
        float f;
        hvl_t v;
    } s1;
    s1 wdata[SPACE1_DIM1];   /* Information to write */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1, tid2; /* Datatype IDs         */
    hsize_t		dims1[] = {SPACE1_DIM1};
    unsigned       i,j;        /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].i=i*10;
        wdata[i].f=(float)((i*20)/3.0);
        wdata[i].v.p=malloc((i+1)*sizeof(unsigned int));
        wdata[i].v.len=i+1;
        for(j=0; j<(i+1); j++)
            ((unsigned int *)wdata[i].v.p)[j]=i*10+j;
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE23, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create (H5T_NATIVE_UINT);

    /* Create the base compound type */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1));

    /* Insert fields */
    ret=H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_INT);
    assert(ret>=0);
    ret=H5Tinsert(tid2, "f", HOFFSET(s1, f), H5T_NATIVE_FLOAT);
    assert(ret>=0);
    ret=H5Tinsert(tid2, "v", HOFFSET(s1, v), tid1);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid2,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid2,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid2,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid2);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_vldatatypes4(void)
{
    typedef struct {             /* Struct that the VL sequences are composed of */
        int i;
        float f;
    } s1;
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1, tid2; /* Datatype IDs         */
    hsize_t		dims1[] = {SPACE1_DIM1};
    unsigned       i,j;        /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p=malloc((i+1)*sizeof(s1));
        wdata[i].len=i+1;
        for(j=0; j<(i+1); j++) {
            ((s1 *)wdata[i].p)[j].i=i*10+j;
            ((s1 *)wdata[i].p)[j].f=(float)((i*20+j)/3.0);
          } /* end for */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE24, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create the base compound type */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1));

    /* Insert fields */
    ret=H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_INT);
    assert(ret>=0);
    ret=H5Tinsert(tid2, "f", HOFFSET(s1, f), H5T_NATIVE_FLOAT);
    assert(ret>=0);

    /* Create a datatype to refer to */
    tid1 = H5Tvlen_create (tid2);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Tclose(tid2);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array1(void)
{
    int wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hsize_t		sdims1[] = {SPACE1_DIM1};
    hsize_t		tdims1[] = {ARRAY1_DIM1};
    int        i,j;        /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++)
            wdata[i][j]=i*10+j;

    /* Create file */
    fid1 = H5Fcreate(FILE25, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a datatype to refer to */
    tid1 = H5Tarray_create (H5T_NATIVE_INT,ARRAY1_RANK,tdims1,NULL);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array2(void)
{
    int wdata[SPACE1_DIM1][ARRAY2_DIM1][ARRAY2_DIM2][ARRAY2_DIM3];   /* Information to write */
    hid_t		fid;        /* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid;        /* Dataspace ID			*/
    hid_t		tid;        /* Datatype ID			*/
    hsize_t		sdims1[] = {SPACE1_DIM1};
    hsize_t		tdims2[] = {ARRAY2_DIM1,ARRAY2_DIM2,ARRAY2_DIM3};
    int        i,j,k,l;    /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY2_DIM1; j++)
            for(k=0; k<ARRAY2_DIM2; k++)
                for(l=0; l<ARRAY2_DIM3; l++)
                    wdata[i][j][k][l]=i*1000+j*100+k*10+l;

    /* Create file */
    fid = H5Fcreate(FILE26, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a datatype to refer to */
    tid = H5Tarray_create (H5T_NATIVE_INT,ARRAY2_RANK,tdims2,NULL);

    /* Create a dataset */
    dataset=H5Dcreate(fid,"Dataset1",tid,sid,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid);
    assert(ret>=0);
    ret = H5Sclose(sid);
    assert(ret>=0);
    ret = H5Fclose(fid);
    assert(ret>=0);
}

static void gent_array3(void)
{
    int wdata[SPACE1_DIM1][ARRAY1_DIM1][ARRAY3_DIM1][ARRAY3_DIM2];   /* Information to write */
    hid_t		fid;        /* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid;        /* Dataspace ID			*/
    hid_t		tid1;       /* 1-D array Datatype ID */
    hid_t		tid2;       /* 2-D array Datatype ID */
    hsize_t		sdims1[] = {SPACE1_DIM1};
    hsize_t		tdims1[] = {ARRAY1_DIM1};
    hsize_t		tdims2[] = {ARRAY3_DIM1,ARRAY3_DIM2};
    int        i,j,k,l;    /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++)
            for(k=0; k<ARRAY3_DIM1; k++)
                for(l=0; l<ARRAY3_DIM2; l++)
                    wdata[i][j][k][l]=i*1000+j*100+k*10+l;

    /* Create file */
    fid = H5Fcreate(FILE27, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a 2-D datatype to refer to */
    tid2 = H5Tarray_create (H5T_NATIVE_INT,ARRAY3_RANK,tdims2,NULL);

    /* Create a 1-D datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Create a dataset */
    dataset=H5Dcreate(fid,"Dataset1",tid1,sid,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Tclose(tid2);
    assert(ret>=0);
    ret = H5Sclose(sid);
    assert(ret>=0);
    ret = H5Fclose(fid);
    assert(ret>=0);
}

static void gent_array4(void)
{
    typedef struct {        /* Typedef for compound datatype */
        int i;
        float f;
    } s2_t;
    s2_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Array Datatype ID			*/
    hid_t		tid2;       /* Compound Datatype ID			*/
    hsize_t		sdims1[] = {SPACE1_DIM1};
    hsize_t		tdims1[] = {ARRAY1_DIM1};
    int        i,j;        /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].i=i*10+j;
            wdata[i][j].f=(float)(i*2.5+j);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE28, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));

    /* Insert integer field */
    ret = H5Tinsert (tid2, "i", HOFFSET(s2_t,i), H5T_NATIVE_INT);
    assert(ret>=0);

    /* Insert float field */
    ret = H5Tinsert (tid2, "f", HOFFSET(s2_t,f), H5T_NATIVE_FLOAT);
    assert(ret>=0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Close compound datatype */
    ret=H5Tclose(tid2);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array5(void)
{
    typedef struct {        /* Typedef for compound datatype */
        int i;
        float f[ARRAY1_DIM1];
    } s2_t;
    s2_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Array Datatype ID	*/
    hid_t		tid2;       /* Compound Datatype ID	*/
    hid_t		tid3;       /* Nested Array Datatype ID	*/
    hsize_t		sdims1[] = {SPACE1_DIM1};
    hsize_t		tdims1[] = {ARRAY1_DIM1};
    int        i,j,k;      /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].i=i*10+j;
            for(k=0; k<ARRAY1_DIM1; k++)
                wdata[i][j].f[k]=(float)(i*10+j*2.5+k);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE29, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));

    /* Insert integer field */
    ret = H5Tinsert (tid2, "i", HOFFSET(s2_t,i), H5T_NATIVE_INT);
    assert(ret>=0);

    /* Create an array of floats datatype */
    tid3 = H5Tarray_create (H5T_NATIVE_FLOAT,ARRAY1_RANK,tdims1,NULL);

    /* Insert float array field */
    ret = H5Tinsert (tid2, "f", HOFFSET(s2_t,f), tid3);
    assert(ret>=0);

    /* Close array of floats field datatype */
    ret=H5Tclose(tid3);
    assert(ret>=0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Close compound datatype */
    ret=H5Tclose(tid2);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array6(void)
{
    hvl_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Array Datatype ID			*/
    hid_t		tid2;       /* VL Datatype ID       */
    hsize_t		sdims1[] = {SPACE1_DIM1};
    hsize_t		tdims1[] = {ARRAY1_DIM1};
    int        i,j,k;      /* counting variables */
    herr_t		ret;		/* Generic return value		*/

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].p=malloc((i+j+1)*sizeof(unsigned int));
            wdata[i][j].len=i+j+1;
            for(k=0; k<(i+j+1); k++)
                ((unsigned int *)wdata[i][j].p)[k]=i*100+j*10+k;
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE30, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tvlen_create(H5T_NATIVE_UINT);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Close VL datatype */
    ret=H5Tclose(tid2);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_array7(void)
{
    hvl_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Array Datatype ID			*/
    hid_t		tid2;       /* VL Datatype ID       */
    hid_t		tid3;       /* Nested Array Datatype ID   */
    hsize_t		sdims1[] = {SPACE1_DIM1};
    hsize_t		tdims1[] = {ARRAY1_DIM1};
    int        i,j,k,l;    /* Index variables */
    herr_t		ret;		/* Generic return value		*/

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].p=malloc((i+j+1)*(sizeof(unsigned int)*ARRAY1_DIM1));
            wdata[i][j].len=i+j+1;
            for(k=0; k<(i+j+1); k++)
                for(l=0; l<ARRAY1_DIM1; l++)
                    ((unsigned int *)wdata[i][j].p)[k*ARRAY1_DIM1+l]=i*1000+j*100+k*10+l;
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE31, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create the nested array datatype to refer to */
    tid3 = H5Tarray_create(H5T_NATIVE_UINT,ARRAY1_RANK,tdims1,NULL);

    /* Create a VL datatype of 1-D arrays to refer to */
    tid2 = H5Tvlen_create(tid3);

    /* Close nested array datatype */
    ret=H5Tclose(tid3);
    assert(ret>=0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create (tid2,ARRAY1_RANK,tdims1,NULL);

    /* Close VL datatype */
    ret=H5Tclose(tid2);
    assert(ret>=0);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);

    /* Write dataset to disk */
    ret=H5Dwrite(dataset,tid1,H5S_ALL,H5S_ALL,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Reclaim the write VL data */
    ret=H5Dvlen_reclaim(tid1,sid1,H5P_DEFAULT,wdata);
    assert(ret>=0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    assert(ret>=0);
    ret = H5Tclose(tid1);
    assert(ret>=0);
    ret = H5Sclose(sid1);
    assert(ret>=0);
    ret = H5Fclose(fid1);
    assert(ret>=0);
}

static void gent_empty(void)
{
    typedef struct {
        int a;
        float b;
        char c;
    } empty_struct;
    hid_t file, dset, space, type;
    hsize_t dims[] = { SPACE1_DIM1 };
    herr_t ret=0;

    file = H5Fcreate(FILE32, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    space = H5Screate_simple(SPACE1_RANK, dims, NULL);

    /* write out an empty vlen dataset */
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate(file, "Dataset1.0", type, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);

    /* write out an empty native integer dataset dataset */
    dset = H5Dcreate(file, "Dataset2.0", H5T_NATIVE_INT, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);

    /* write out an empty native floating-point dataset dataset */
    dset = H5Dcreate(file, "Dataset3.0", H5T_NATIVE_FLOAT, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);

    /* write out an empty array dataset */
    type = H5Tarray_create(H5T_NATIVE_INT,SPACE1_RANK,dims,NULL);
    dset = H5Dcreate(file, "Dataset4.0", type, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);

    /* write out an empty compound dataset */
    type = H5Tcreate(H5T_COMPOUND,sizeof(empty_struct));
    H5Tinsert(type, "a", HOFFSET(empty_struct, a),H5T_NATIVE_INT);
    H5Tinsert(type, "b", HOFFSET(empty_struct, b),H5T_NATIVE_FLOAT);
    H5Tinsert(type, "c", HOFFSET(empty_struct, c),H5T_NATIVE_CHAR);
    dset = H5Dcreate(file, "Dataset5.0", type, space, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    assert(ret>=0);
    ret = H5Tclose(type);
    assert(ret>=0);

    ret = H5Sclose(space);
    assert(ret>=0);

    ret = H5Fclose(file);
    assert(ret>=0);
}

static void gent_group_comments(void)
{
    hid_t fid, group;
  
    fid = H5Fcreate(FILE33, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
    /* / */
    group = H5Gcreate (fid, "/g1", 0);
    H5Gset_comment(group, "/g1", "Comment for group /g1");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2", 0);
    H5Gset_comment(group, "/g2", "Comment for group /g2");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3", 0);
    H5Gset_comment(group, "/g3", "Comment for group /g3");
    H5Gclose(group);
  
    /* /g1 */
    group = H5Gcreate (fid, "/g1/g1.1", 0);
    H5Gset_comment(group, "/g1/g1.1", "Comment for group /g1/g1.1");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g1/g1.2", 0);
    H5Gset_comment(group, "/g1/g1.2", "Comment for group /g1/g1.2");
    H5Gclose(group);
  
    /* /g2 */
    group = H5Gcreate (fid, "/g2/g2.1", 0);
    H5Gset_comment(group, "/g2/g2.1", "Comment for group /g2/g2.1");
    H5Gclose(group);
  
    /* /g3 */
    group = H5Gcreate (fid, "/g3/g3.1", 0);
    H5Gset_comment(group, "/g3/g3.1", "Comment for group /g3/g3.1");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.2", 0);
    H5Gset_comment(group, "/g3/g3.2", "Comment for group /g3/g3.2");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.3", 0);
    H5Gset_comment(group, "/g3/g3.3", "Comment for group /g3/g3.3");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g3/g3.4", 0);
    H5Gset_comment(group, "/g3/g3.4", "Comment for group /g3/g3.4");
    H5Gclose(group);
  
    /* /g2/g2.1 */
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.1", 0);
    H5Gset_comment(group, "/g2/g2.1/g2.1.1", "Comment for group /g2/g2.1/g2.1.1");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.2", 0);
    H5Gset_comment(group, "/g2/g2.1/g2.1.2", "Comment for group /g2/g2.1/g2.1.2");
    H5Gclose(group);
    group = H5Gcreate (fid, "/g2/g2.1/g2.1.3", 0);
    H5Gset_comment(group, "/g2/g2.1/g2.1.3", "Comment for group /g2/g2.1/g2.1.3");
    H5Gclose(group);
  
    H5Fclose(fid);
}

static
void gent_split_file(void)
{
    hid_t fapl, fid, root, attr, space, dataset, atype;
    char meta[] = "this is some metadata on this file";
    hsize_t dims[2];
    int i, j, dset[10][15];

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_split(fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT);
    fid = H5Fcreate(FILE34, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    root = H5Gopen(fid, "/");

    atype = H5Tcopy(H5T_C_S1);
    H5Tset_size(atype, strlen(meta) + 1);
    H5Tset_strpad(atype, H5T_STR_NULLTERM);

    dims[0] = 1;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate(root, "Metadata", atype, space, H5P_DEFAULT);
    H5Awrite(attr, atype, meta);
    H5Tclose(atype);
    H5Sclose(space);
    H5Aclose(attr);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 15; j++)
              dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Gclose(root);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static
void gent_family(void)
{
    hid_t fapl, fid, space, dataset;
    hsize_t dims[2];
    int i, j, dset[10][15];

#define FAMILY_SIZE     256

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT);

    fid = H5Fcreate(FILE35, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 15; j++)
              dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static const char *multi_letters = "msbrglo";

static
void gent_multi(void)
{
    hid_t fapl, fid, space, dataset;
    hsize_t dims[2];
    int i, j, dset[10][15];

    /* Multi-file driver, general case of the split driver */
    H5FD_mem_t mt, memb_map[H5FD_MEM_NTYPES];
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    const char *memb_name[H5FD_MEM_NTYPES];
    char sv[H5FD_MEM_NTYPES][1024];
    haddr_t memb_addr[H5FD_MEM_NTYPES];

    fapl = H5Pcreate(H5P_FILE_ACCESS);

    HDmemset(memb_map, 0, sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);

    assert(HDstrlen(multi_letters) == H5FD_MEM_NTYPES);

    for (mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; ++mt) {
        memb_fapl[mt] = H5P_DEFAULT;
        sprintf(sv[mt], "%%s-%c.h5", multi_letters[mt]);
        memb_name[mt] = sv[mt];
        memb_addr[mt] = MAX(mt - 1,0) * (HADDR_MAX / 10);
    }

    H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name,
                      memb_addr, FALSE);

    fid = H5Fcreate(FILE36, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);

    for (i = 0; i < 10; i++)
         for (j = 0; j < 15; j++)
              dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static void gent_large_objname(void)
{
    hid_t fid, group;
    char grp_name[128];
    register int i;
  
    fid = H5Fcreate(FILE37, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    group = H5Gcreate(fid, "this_is_a_large_group_name", 0);

    for (i = 0; i < 50; ++i) {
        sprintf(grp_name, "this_is_a_large_group_name%d", i);
        group = H5Gcreate(group, grp_name, 0);
    }

    H5Gclose(group);
    H5Fclose(fid);
}

int main(void)
{
    gent_group();
    gent_attribute();
    gent_softlink();
    gent_dataset();
    gent_hardlink();
    gent_compound_dt();
    gent_all();
    gent_loop();

    gent_dataset2();
    gent_compound_dt2();
    gent_loop2();
    gent_many();

    gent_str();
    gent_str2();

    gent_enum();

    gent_objref();
    gent_datareg();

    gent_nestcomp();

    gent_opaque();

    gent_bitfields();

    gent_vldatatypes();
    gent_vldatatypes2();
    gent_vldatatypes3();
    gent_vldatatypes4();

    gent_array1();
    gent_array2();
    gent_array3();
    gent_array4();
    gent_array5();
    gent_array6();
    gent_array7();

    gent_empty();
    gent_group_comments();
    gent_split_file();
    gent_family();
    gent_multi();

    gent_large_objname();

    return 0;
}
