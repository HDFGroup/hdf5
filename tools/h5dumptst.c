/*
 * Generate the binary hdf5 files for the h5dump tests.
 */
#include <limits.h>
#include "hdf5.h"
#include <H5private.h>

#define FILE1 "tgroup.h5"
#define FILE2 "tdset.h5"
#define FILE3 "tattr.h5"
#define FILE4 "tslink.h5"
#define FILE5 "thlink.h5"
#define FILE6 "tcompound.h5"
#define FILE7 "tall.h5"
#define FILE8 "tdset2.h5"
#define FILE9 "tcompound2.h5"
#define FILE10 "tloop.h5"
#define FILE11 "tloop2.h5"
#define FILE12 "tmany.h5"
#define FILE13 "tstr.h5"
#define FILE14 "tstr2.h5"
#define FILE15 "enum.h5"
#define FILE16 "objref.h5"
#define FILE17 "datareg.h5"
#define LENSTR 50
#define LENSTR2 11

#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

#define SPACE1_RANK	1
#define SPACE1_DIM1	4


/* Element selection information */
#define POINT1_NPOINTS 10


typedef enum{
	RED,
	GREEN,
	BLUE,
	WHITE,
	BLACK,
} enumtype;

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;


static void test_group(void) {
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

static void test_dataset(void) {
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
            dset1[i][j] = j;
  H5Dwrite(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
  H5Sclose(space);
  H5Dclose(dataset);

  /* dset2 */
  dims[0] = 30; dims[1] = 20;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(fid, "/dset2", H5T_IEEE_F64BE, space, H5P_DEFAULT);
  for (i = 0; i < 30; i++)
       for (j = 0; j < 20; j++)
            dset2[i][j] = 0.0001*j;
  H5Dwrite(dataset, H5T_IEEE_F64BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Sclose(space);
  H5Dclose(dataset);


  H5Fclose(fid);
}

static void test_dataset2(void) {
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
  H5Dwrite(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
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
  H5Dwrite(dataset, H5T_IEEE_F64BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Sclose(space);
  H5Dclose(dataset);


  H5Fclose(fid);
}


static void test_attribute(void) {
hid_t fid, root, space, attr, type;
hsize_t dims[2];
char buf[60];
int i, data[20];
double d[10];
char string[]= "string attribute";
int point = 100;

  fid = H5Fcreate(FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  root = H5Gopen (fid, "/");

  /* attribute 1 */
  dims[0] = 24;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (root, "attr1", H5T_NATIVE_SCHAR, space, H5P_DEFAULT);
  sprintf(buf, "attribute of root group");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);


  /* attribute 2 */
  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (root, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++) data[i] = i+1;
  H5Awrite(attr, H5T_STD_I32BE, data);
  H5Sclose(space);
  H5Aclose(attr);

  /* attribute 3 */
  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (root, "attr3", H5T_IEEE_F64BE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++) d[i] = 0.1 * i;
  H5Awrite(attr, H5T_IEEE_F64BE, d);
  H5Sclose(space);
  H5Aclose(attr);

  /* attribute 4 */
  space = H5Screate(H5S_SCALAR);
  attr = H5Acreate (root, "attr4", H5T_STD_I32BE, space, H5P_DEFAULT);
  H5Awrite(attr, H5T_STD_I32BE, &point);
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

static void test_softlink(void) {
hid_t fid, root;
herr_t status;

  fid = H5Fcreate(FILE4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  root = H5Gopen (fid, "/");

  status = H5Glink (root, H5G_LINK_SOFT, "somevalue", "slink1");

  status = H5Glink (root, H5G_LINK_SOFT, "linkvalue", "slink2");

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


static void test_hardlink(void) {
hid_t fid, group, dataset, space;
hsize_t dim;
int i, dset[5];

  fid = H5Fcreate(FILE5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  dim = 5;
  space = H5Screate_simple(1, &dim, NULL);
  dataset = H5Dcreate(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 5; i++) dset[i] = i;
  H5Dwrite(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
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
static void test_compound_dt(void) {       /* test compound data type */
hid_t fid, group, dataset, space, space3, type;
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
size_t dim[2];

hsize_t sdim = 5;
hsize_t dset3_dim[2];

  
  for (i = 0; i < (int)sdim; i++) {
       dset1[i].a = i; 
       dset1[i].b = i*i;
       dset1[i].c = 1./(i+1);
       
       dset2[i].a = i;
       dset2[i].b = i+ i*0.1;

       dset4[i].a = i;
       dset4[i].b = i*1.0;

       dset5[i].a = i;
       dset5[i].b = i*0.1;
  }


  fid = H5Fcreate(FILE6, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  space = H5Screate_simple(1, &sdim, NULL);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
  
  H5Tinsert(type, "a_name", HOFFSET(dset1_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "b_name", HOFFSET(dset1_t, b), H5T_IEEE_F32BE);
  H5Tinsert(type, "c_name", HOFFSET(dset1_t, c), H5T_IEEE_F64BE);

  dataset = H5Dcreate(fid, "/dset1", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

  H5Tclose(type);
  H5Dclose(dataset);

  /* shared data type 1 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
  H5Tinsert(type, "int_name", HOFFSET(dset2_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float_name", HOFFSET(dset2_t, b), H5T_IEEE_F32BE);
  H5Tcommit(fid, "type1", type);

  group = H5Gcreate (fid, "/group1", 0);

  dataset = H5Dcreate(group, "dset2", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

  H5Tclose(type);
  H5Dclose(dataset);


  /* shared data type 2 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));
  ndims = 1; dim[0] = 4;
  H5Tinsert_array(type, "int_array", HOFFSET(dset3_t, a), ndims, dim, NULL, H5T_STD_I32BE);
  ndims = 2; dim[0] = 5; dim[1] = 6;
  H5Tinsert_array(type, "float_array", HOFFSET(dset3_t, b), ndims, dim, NULL, H5T_IEEE_F32BE);
  H5Tcommit(fid, "type2", type);

  dset3_dim[0] = 3;  dset3_dim[1] = 6;
  space3 = H5Screate_simple(2, dset3_dim, NULL);
  dataset = H5Dcreate(group, "dset3", type, space3, H5P_DEFAULT);
  for (i = 0; i < (int)dset3_dim[0]; i++) {
       for (j = 0; j < (int)dset3_dim[1]; j++) {
            for (k = 0; k < 4; k++)
                 dset3[i][j].a[k] = k;
            for (k = 0; k < 5; k++)
                 for (l = 0; l < 6; l++)
                      dset3[i][j].b[k][l] = 0.1* (k+1);
       }
  }
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);
  H5Sclose(space3);
  H5Tclose(type);
  H5Dclose(dataset);

  /* shared data type 3 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  H5Tinsert(type, "int", HOFFSET(dset4_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset4_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type3", type);

  dataset = H5Dcreate(group, "dset4", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

  H5Tclose(type);
  H5Dclose(dataset);
  H5Gclose(group);


  /* unamed data type */
  group = H5Gcreate (fid, "/group2", 0);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type, "int", HOFFSET(dset5_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset5_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type4", type);
  dataset = H5Dcreate(group, "dset5", type, space, H5P_DEFAULT);
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset5);

  H5Gunlink(group,"type4");

  H5Tclose(type);
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
static void test_compound_dt2(void) {       /* test compound data type */
hid_t fid, group, dataset, space, type, create_plist;
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
size_t dim[2];

hsize_t sdim, maxdim;

  sdim = 10;
  for (i = 0; i < (int)sdim; i++) {
       dset1[i].a = i; 
       dset1[i].b = i*i;
       dset1[i].c = 1./(i+1);
       
       dset2[i].a = i;
       dset2[i].b = i+ i*0.1;

       dset4[i].a = i;
       dset4[i].b = i*1.0;

       dset5[i].a = i;
       dset5[i].b = i*1.0;
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
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

  H5Tclose(type);
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
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

  H5Tclose(type);
  H5Dclose(dataset);


  /* shared data type 2 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));
  ndims = 1; dim[0] = 4;
  H5Tinsert_array(type, "int_array", HOFFSET(dset3_t, a), ndims, dim, perm, H5T_STD_I32BE);
  ndims = 2; dim[0] = 5; dim[1] = 6;
  H5Tinsert_array(type, "float_array", HOFFSET(dset3_t, b), ndims, dim, perm, H5T_STD_I32BE);
  H5Tcommit(fid, "type2", type);
  H5Tclose(type);

  /* shared data type 3 */
  type = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
  H5Tinsert(type, "int", HOFFSET(dset4_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset4_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type3", type);

  dataset = H5Dcreate(group, "dset4", type, space, create_plist);
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

  H5Tclose(type);
  H5Dclose(dataset);
  H5Gclose(group);


  /* unamed data type */
  group = H5Gcreate (fid, "/group2", 0);

  type = H5Tcreate (H5T_COMPOUND, sizeof(dset5_t));
  H5Tinsert(type, "int", HOFFSET(dset5_t, a), H5T_STD_I32BE);
  H5Tinsert(type, "float", HOFFSET(dset5_t, b), H5T_IEEE_F32BE);
  H5Tcommit(group, "type4", type);
  dataset = H5Dcreate(group, "dset5", type, space, create_plist);
  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset5);

  H5Gunlink(group,"type4");

  H5Tclose(type);
  H5Dclose(dataset);
  H5Sclose(space);
  H5Gclose(group);

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

static void test_all(void) {
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
  attr = H5Acreate (group, "attr1", H5T_NATIVE_SCHAR, space, H5P_DEFAULT);
  sprintf(buf, "abcdefghi");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 2; dims[1] = 2;
  space = H5Screate_simple(2, dims, NULL);
  attr = H5Acreate (group, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT);
  data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
  H5Awrite(attr, H5T_STD_I32BE, data);
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
            dset1[i][j] = j;
  H5Dwrite(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
  H5Sclose(space);

  /* attributes of dset1.1.1 */
  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr1", H5T_NATIVE_SCHAR, space, H5P_DEFAULT);
  sprintf(buf, "1st attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr2", H5T_NATIVE_SCHAR, space, H5P_DEFAULT);
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
  H5Dwrite(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
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
       dset2_1[i] = i*0.1+1;
  H5Dwrite(dataset, H5T_IEEE_F32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_1);
  H5Sclose(space);
  H5Dclose(dataset);
 
  /* dset2.2 */
  dims[0] = 3; dims[1] = 5;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(group, "dset2.2", H5T_IEEE_F32BE, space, H5P_DEFAULT);
  for (i = 0; i < 3; i++)
       for (j = 0; j < 5; j++)
            dset2_2[i][j] = i*0.1;
  H5Dwrite(dataset, H5T_IEEE_F32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_2);
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

static void test_loop(void) {
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

static void test_loop2(void) {
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

static void test_many(void) {
hid_t fid, group, attr, dataset, space, space2, type, create_plist;
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

size_t dim[4];
int index[4] = {0,1,2,3};  /* normal indicies */
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
  H5Tinsert_array(type, "a_array", HOFFSET(dset1_t, a), 4, dim, perm, H5T_STD_I32BE);
  H5Tinsert_array(type, "b_array", HOFFSET(dset1_t, b), 4, dim, perm, H5T_IEEE_F64BE);
  H5Tinsert_array(type, "c_array", HOFFSET(dset1_t, c), 4, dim, perm, H5T_IEEE_F64BE);

  H5Tcommit(group, "type1", type);

  /* dset1 */
  sdim = 6;
  maxdim = H5S_UNLIMITED;
  space = H5Screate_simple(1, &sdim, &maxdim);
  dataset = H5Dcreate(group, "dset1", type, space, create_plist);

  /* add attributes to dset1 */
  dims[0] = 10;
  space2 = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr1", H5T_NATIVE_CHAR, space2, H5P_DEFAULT);
  sprintf(buf, "abcdefghi");
  H5Awrite(attr, H5T_NATIVE_CHAR, buf);
  H5Sclose(space2);
  H5Aclose(attr);

  dims[0] = 2; dims[1] = 2;
  space2 = H5Screate_simple(2, dims, NULL);
  attr = H5Acreate (dataset, "attr2", H5T_STD_I32BE, space2, H5P_DEFAULT);
  data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
  H5Awrite(attr, H5T_STD_I32BE, data);
  H5Sclose(space2);
  H5Aclose(attr);

  dims[0] = 10;
  space2 = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr3", H5T_IEEE_F64BE, space2, H5P_DEFAULT);
  for (i = 0; i < 10; i++) d[i] = 0.1 * i;
  H5Awrite(attr, H5T_IEEE_F64BE, d);
  H5Sclose(space2);
  H5Aclose(attr);

  for (j=0; j<(int)sdim; j++) {
	for (i3 = 0; i3 < 2; i3++) {
		index[perm[3]] = i3;
	for (i2 = 0; i2 < 2; i2++) {
		index[perm[2]] = i2;
	for (i1 = 0; i1 < 2; i1++) {
		index[perm[1]] = i1;
	for (i0 = 0; i0 < 2; i0++) {
		index[perm[0]] = i0;
		
		dset1[j].a[index[3]][index[2]][index[1]][index[0]] = i0+j;
		dset1[j].b[index[3]][index[2]][index[1]][index[0]] = (double)(i0+j);
#if WIN32
		dset1[j].c[index[3]][index[2]][index[1]][index[0]] = (double)(i0+j+(signed __int64)sdim);
#else
		dset1[j].c[index[3]][index[2]][index[1]][index[0]] = (double)(i0+j+sdim);
#endif
	}
	}
	}
	}
  }

  H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

  H5Dclose(dataset);
  H5Sclose(space);

  H5Tclose(type);
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
  H5Dwrite(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

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
  H5Dwrite(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);

  H5Dclose(dataset);

  H5Sclose(space);
  H5Gclose(group);

  group = H5Gopen(fid, "/g5");
  H5Glink (group, H5G_LINK_SOFT, "/g6/dset3", "slink4");
  H5Gclose(group);

  H5Fclose(fid);

}
static hid_t mkstr(int size, int pad) {
hid_t type;

  if ((type=H5Tcopy(H5T_C_S1))<0) return -1;
  if (H5Tset_size(type, size)<0) return -1;
  if (H5Tset_strpad(type, pad)<0) return -1;

  return type;
}

static void test_str(void) {
hid_t fid, dataset, space, f_type, m_type, str_type;

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
  char s[12][32];
  int a[8][10];
} compound_t;
compound_t comp1[3][6];
size_t mdims[2];

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
  mdims[0] = 8; mdims[1] = 10;
  H5Tinsert_array(f_type, "int_array", HOFFSET(compound_t, a), 2, mdims,
                  NULL, H5T_STD_I32BE);
  str_type = mkstr(32, H5T_STR_SPACEPAD);
  mdims[0] = 3; mdims[1] = 4;
  H5Tinsert_array(f_type, "string", HOFFSET(compound_t, s), 2, mdims, 
                  NULL, str_type);

  for (i = 0; i < 3; i++)
      for (j = 0; j < 6; j++) {
           for (k = 0 ; k < 8; k++)
                for (l = 0; l < 10; l++)
                   comp1[i][j].a[k][l] = l;
           for (k = 0 ; k < 12; k++)
               sprintf(comp1[i][j].s[k], "abcdefgh12345678abcdefgh12345678");
      }
           
  dataset = H5Dcreate(fid, "/comp1", f_type, space, H5P_DEFAULT);
  H5Dwrite(dataset, f_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, comp1);

  H5Tclose(str_type);
  H5Tclose(f_type);
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

static void test_str2(void) {
hid_t fid, group, attr, dataset, space, space2, mem_space, hyper_space, type;
hid_t fxdlenstr, fxdlenstr2, memtype;
hsize_t dims[1], size[1], start[1], stride[1], count[1], block[1];


int i, j;
int i0, i1, i2, i3;
char buf[LENSTR+10];
char buf2[3*LENSTR2];
hsize_t sdim, maxdim;

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

  for (i = 0; i < sdim; i++) {
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

  for (i = 0; i < sdim; i++) {
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

  for (i = 0; i < sdim; i++) {
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

  for (i = 0; i < sdim; i++) {
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

  for (i = 0; i < sdim; i++) {
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

  for (i = 0; i < sdim; i++) {
	start[0] = i;
	sprintf(buf,"This is row %1d of type H5T_STR_SPACEPAD of string array",i);
  	H5Tset_size(memtype, HDstrlen(buf)+1);
	H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
  	H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
  }
  H5Dclose(dataset);
  H5Gclose(group);

  H5Tclose(fxdlenstr);
  H5Tclose(memtype);
  H5Sclose(mem_space);
  H5Sclose(hyper_space);
  H5Sclose(space);

  H5Fclose(fid);

}


void test_enum(){
/*some code is taken from enum.c in the test dir */
	hid_t file, type, space, dset;
	int val;
	signed char val8;
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



void test_objref(){
/*some code is taken from enum.c in the test dir */
	hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset,	/* Dataset ID			*/
                dset2;      /* Dereferenced dataset ID */
    hid_t		group;      /* Group ID             */
    hid_t		sid1;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1};
    hobj_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf,       /* buffer read from disk */
               *tbuf;       /* temp. buffer read from disk */
    uint32_t   *tu32;      /* Temporary pointer to uint32 data */
    intn        i;          /* counting variables */
    const char *write_comment="Foo!"; /* Comments for group */
    char read_comment[10];
    herr_t		ret;		/* Generic return value		*/


    /* Allocate write & read buffers */
    wbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    rbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    tbuf=malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE16, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a group */
    group=H5Gcreate(fid1,"Group1",-1);

    /* Set group's comment */
    ret=H5Gset_comment(group,".",write_comment);

    /* Create a dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset1",H5T_STD_U32LE,sid1,H5P_DEFAULT);

    for(tu32=(uint32_t *)wbuf,i=0; i<SPACE1_DIM1; i++)
        *tu32++=i*3;

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_STD_U32LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Create another dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset2",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Create a datatype to refer to */
    tid1 = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));

    /* Insert fields */
    ret=H5Tinsert (tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);

    ret=H5Tinsert (tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);

    ret=H5Tinsert (tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT);

    /* Save datatype for later */
    ret=H5Tcommit (group, "Datatype1", tid1);

    /* Close datatype */
    ret = H5Tclose(tid1);

    /* Close group */
    ret = H5Gclose(group);

    /* Create a dataset */
    dataset=H5Dcreate(fid1,"Dataset3",H5T_STD_REF_OBJ,sid1,H5P_DEFAULT);

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf[0],fid1,"/Group1/Dataset1",H5R_OBJECT,-1);
    ret = H5Rget_object_type(dataset,&wbuf[0]);

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf[1],fid1,"/Group1/Dataset2",H5R_OBJECT,-1);

    ret = H5Rget_object_type(dataset,&wbuf[1]);
 
    /* Create reference to group */
    ret = H5Rcreate(&wbuf[2],fid1,"/Group1",H5R_OBJECT,-1);

    ret = H5Rget_object_type(dataset,&wbuf[2]);


    /* Create reference to named datatype */
    ret = H5Rcreate(&wbuf[3],fid1,"/Group1/Datatype1",H5R_OBJECT,-1);

    ret = H5Rget_object_type(dataset,&wbuf[3]);


    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);


    /* Close disk dataspace */
    ret = H5Sclose(sid1);
   
    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Close file */
    ret = H5Fclose(fid1);

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(tbuf);

}


void test_datareg(){

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
    hsize_t *   coords;             /* Coordinate buffer */
    hsize_t		low[SPACE2_RANK];   /* Selection bounds */
    hsize_t		high[SPACE2_RANK];     /* Selection bounds */
    hdset_reg_ref_t      *wbuf,      /* buffer to write to disk */
               *rbuf;       /* buffer read from disk */
    uint8_t    *dwbuf,      /* Buffer for writing numeric data to disk */
               *drbuf;      /* Buffer for reading numeric data from disk */
    uint8_t    *tu8;        /* Temporary pointer to uint8 data */
    intn        i;          /* counting variables */
    herr_t		ret;		/* Generic return value		*/

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
    dset2=H5Dcreate(fid1,"Dataset2",H5T_STD_U8LE,sid2,H5P_DEFAULT);

    for(tu8=dwbuf,i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++)
        *tu8++=i*3;

    /* Write selection to disk */
    ret=H5Dwrite(dset2,H5T_STD_U8LE,H5S_ALL,H5S_ALL,H5P_DEFAULT,dwbuf);

    /* Close Dataset */
    ret = H5Dclose(dset2);

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
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);

    ret = H5Sget_select_npoints(sid2);

    /* Store first dataset region */
    ret = H5Rcreate(&wbuf[0],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

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
    ret = H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(const hssize_t **)coord1);

    ret = H5Sget_select_npoints(sid2);

    /* Store second dataset region */
    ret = H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Write selection to disk */
    ret=H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    
    /* Close Dataset */
    ret = H5Dclose(dset1);

    /* Close uint8 dataset dataspace */
    ret = H5Sclose(sid2);
    
    /* Close file */
    ret = H5Fclose(fid1);

    /* Free memory buffers */
    free(wbuf);
    free(rbuf);
    free(dwbuf);
    free(drbuf);

}

int main(void){

test_group();
test_attribute();
test_softlink();
test_dataset();
test_hardlink();
test_compound_dt();
test_all();
test_loop();

test_dataset2();
test_compound_dt2();
test_loop2();
test_many();

test_str();
test_str2();

test_enum();

test_objref();
test_datareg();
return 0;

}
