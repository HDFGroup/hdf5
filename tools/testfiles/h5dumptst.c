/*
 * Generate the binary hdf5 files for the h5dump tests.
 */
#include "hdf5.h"

#define FILE1 "tgroup.h5"
#define FILE2 "tdset.h5"
#define FILE3 "tattr.h5"
#define FILE4 "tslink.h5"
#define FILE5 "thlink.h5"
#define FILE6 "tcompound.h5"
#define FILE7 "tall.h5"

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
double dset2[30][10];
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
  dims[0] = 30; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(fid, "/dset2", H5T_IEEE_F64BE, space, H5P_DEFAULT);
  for (i = 0; i < 30; i++)
       for (j = 0; j < 10; j++)
            dset2[i][j] = j;
  H5Dwrite(dataset, H5T_IEEE_F64BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Sclose(space);
  H5Dclose(dataset);


  H5Fclose(fid);
}


static void test_attribute(void) {
hid_t fid, root, space, attr;
hsize_t dims[2];
char buf[60];
int i, data[20];
double d[10];

  fid = H5Fcreate(FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  root = H5Gopen (fid, "/");


  /* attribute 1 */
  dims[0] = 24;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (root, "attr1", H5T_NATIVE_CHAR, space, H5P_DEFAULT);
  sprintf(buf, "attribute of root group");
  H5Awrite(attr, H5T_NATIVE_CHAR, buf);
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
         /   |   \
      g1    g2    dset

    /   \     |
 link1  link2 link3
(g2)   (dset) (dset)     */

static void test_hardlink(void) {
hid_t fid, group, dataset, space;
hsize_t dim;
int i, dset[5];

  fid = H5Fcreate(FILE5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  dim = 5;
  space = H5Screate_simple(1, &dim, NULL);
  dataset = H5Dcreate(fid, "/dset", H5T_STD_I32BE, space, H5P_DEFAULT);
  for (i = 0; i < 5; i++) dset[i] = i;
  H5Dwrite(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
  H5Sclose(space);
  H5Dclose(dataset);

  group = H5Gcreate (fid, "/g1", 0);
  H5Glink (group, H5G_LINK_HARD, "/dset", "link2");
  H5Gclose(group);

  group = H5Gcreate (fid, "/g2", 0);
  H5Glink (group, H5G_LINK_HARD, "/dset", "link3");
  H5Gclose(group);

  group = H5Gopen(fid, "/g1");
  H5Glink (group, H5G_LINK_HARD, "/g2", "link1");
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
hid_t fid, group, dataset, space, type;
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

int i, ndims;
const int perm[2];
size_t dim[2];

hsize_t sdim = 5;

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
  attr = H5Acreate (group, "attr1", H5T_NATIVE_CHAR, space, H5P_DEFAULT);
  sprintf(buf, "abcdefghi");
  H5Awrite(attr, H5T_NATIVE_CHAR, buf);
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
  attr = H5Acreate (dataset, "attr1", H5T_NATIVE_CHAR, space, H5P_DEFAULT);
  sprintf(buf, "1st attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_CHAR, buf);
  H5Sclose(space);
  H5Aclose(attr);

  dims[0] = 27;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (dataset, "attr2", H5T_NATIVE_CHAR, space, H5P_DEFAULT);
  sprintf(buf, "2nd attribute of dset1.1.1");
  H5Awrite(attr, H5T_NATIVE_CHAR, buf);
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


int main(void){

test_group();
test_attribute();
test_softlink();
test_dataset();
test_hardlink();
test_compound_dt();
test_all();
return 0;

}
