/*
 * Generate the binary hdf5 files for the h5dump tests.
 */

#include "hdf5.h"

#define FILE1 "tgroup.h5"
#define FILE2 "tdset.h5"
#define FILE3 "tattr.h5"
#define FILE4 "tslink.h5"
#define FILE5 "tall.h5"

static void test_group() {
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

static void test_dataset() {
hid_t fid, dataset, space;
hsize_t dims[2];
int dset1[10][20];
double dset2[30][10];
int i, j;

  fid = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  /* dset2 - compound dt, array ds */
  /* dset3 - named compound dt, array ds */
  /* dset4 - shared dt, array ds */

  /* dset1 */
  dims[0] = 10; dims[1] = 20;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(fid, "/dset1", H5T_NATIVE_INT, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 20; j++)
            dset1[i][j] = j;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
  H5Sclose(space);
  H5Dclose(dataset);

  /* dset2 */
  dims[0] = 30; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(fid, "/dset2", H5T_NATIVE_DOUBLE, space, H5P_DEFAULT);
  for (i = 0; i < 30; i++)
       for (j = 0; j < 10; j++)
            dset2[i][j] = j*1.1 + i;
  H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Sclose(space);
  H5Dclose(dataset);


  H5Fclose(fid);
}

/*
static void test_nameddt() {
}
*/

static void test_attribute() {
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
  dims[0] = 20;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (root, "attr2", H5T_NATIVE_INT, space, H5P_DEFAULT);
  for (i = 0; i < 20; i++) data[i] = i+1;
  H5Awrite(attr, H5T_NATIVE_INT, data);
  H5Sclose(space);
  H5Aclose(attr);

  /* attribute 3 */
  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  attr = H5Acreate (root, "attr3", H5T_NATIVE_DOUBLE, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++) d[i] = 0.0001 * i+ 0.0001;
  H5Awrite(attr, H5T_NATIVE_DOUBLE, d);
  H5Sclose(space);
  H5Aclose(attr);

  H5Gclose(root);

  H5Fclose(fid);
}

static void test_softlink() {
hid_t fid, root;
herr_t status;

  fid = H5Fcreate(FILE4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  root = H5Gopen (fid, "/");

  status = H5Glink (root, H5G_LINK_SOFT, "../../somevalue", "slink1");

  status = H5Glink (root, H5G_LINK_SOFT, "linkvalue", "slink2");

  H5Gclose(root);

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

static void test_all() {
hid_t fid, group, attr, dataset, space;
hsize_t dims[2];
int data[2][2], dset1[10][10], dset2[20];
char buf[60];
int i, j;
float dset2_1[10], dset2_2[3][5];

  fid = H5Fcreate(FILE5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

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
  attr = H5Acreate (group, "attr2", H5T_NATIVE_INT, space, H5P_DEFAULT);
  data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
  H5Awrite(attr, H5T_NATIVE_INT, data);
  H5Sclose(space);
  H5Aclose(attr);

  H5Gclose(group);

  group = H5Gopen (fid, "/g1/g1.1");

  /* dset1.1.1 */
  dims[0] = 10; dims[1] = 10;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(group, "dset1.1.1", H5T_NATIVE_INT, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       for (j = 0; j < 10; j++)
            dset1[i][j] = j;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
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
  dataset = H5Dcreate(group, "dset1.1.2", H5T_NATIVE_INT, space, H5P_DEFAULT);
  for (i = 0; i < 20; i++)
       dset2[i] = i;
  H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Gclose(group);

  /* soft link */
  group = H5Gopen (fid, "/g1/g1.2/g1.2.1");
  H5Glink (group, H5G_LINK_SOFT, "../somevalue/.", "slink");
  H5Gclose(group);

  group = H5Gopen (fid, "/g2");

  /* dset2.1 */
  dims[0] = 10;
  space = H5Screate_simple(1, dims, NULL);
  dataset = H5Dcreate(group, "dset2.1", H5T_NATIVE_FLOAT, space, H5P_DEFAULT);
  for (i = 0; i < 10; i++)
       dset2_1[i] = i*0.1+1;
  H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_1);
  H5Sclose(space);
  H5Dclose(dataset);
 
  /* dset2.2 */
  dims[0] = 3; dims[1] = 5;
  space = H5Screate_simple(2, dims, NULL);
  dataset = H5Dcreate(group, "dset2.2", H5T_NATIVE_FLOAT, space, H5P_DEFAULT);
  for (i = 0; i < 3; i++)
       for (j = 0; j < 5; j++)
            dset2_2[i][j] = i*0.1;
  H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_2);
  H5Sclose(space);
  H5Dclose(dataset);

  H5Gclose(group);

  H5Fclose(fid);

}


void main(){

test_group();
test_attribute();
test_softlink();
test_dataset();
test_all();

}


