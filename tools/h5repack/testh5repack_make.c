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


int make_all_objects(hid_t loc_id);
int make_attributes(hid_t loc_id);
int make_special_objects(hid_t loc_id);


/*-------------------------------------------------------------------------
 * Function: make_testfiles
 *
 * Purpose: make a test file with all types of HDF5 objects, 
 *   datatypes and filters
 *
 *-------------------------------------------------------------------------
 */
int make_testfiles(void)
{
 hid_t  loc_id;  /* file ID */

 TESTING("    generating datasets");

/*-------------------------------------------------------------------------
 * create a file for general copy test 
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME1,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_all_objects(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;

/*-------------------------------------------------------------------------
 * create a file for attributes copy test
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME2,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_attributes(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;
/*-------------------------------------------------------------------------
 * create a file for special items test
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME3,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_special_objects(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;
/*-------------------------------------------------------------------------
 * create a file for the filters and layouts test
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME4,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_filters(loc_id)<0)
  goto out;
 if (make_layout(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;
 
 PASSED();   
 return 0;                                                 
 
out:
 H5E_BEGIN_TRY {
  H5Fclose(loc_id);
 } H5E_END_TRY;
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: make_all_objects
 *
 * Purpose: make a test file with all types of HDF5 objects, datatypes 
 *
 *-------------------------------------------------------------------------
 */
int make_all_objects(hid_t loc_id)
{
 hid_t   dset_id;
 hid_t   group_id;
 hid_t   type_id;  
 hid_t   root_id;
 hid_t   space_id;
 hsize_t dims[1]={2};
 /* Compound datatype */
 typedef struct s_t 
 {
  int    a;
  float  b;
 } s_t;
 
/*-------------------------------------------------------------------------
 * H5G_DATASET
 *-------------------------------------------------------------------------
 */
 space_id = H5Screate_simple(1,dims,NULL);
 dset_id  = H5Dcreate(loc_id,"dset_referenced",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
 H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */ 
 group_id  = H5Gcreate(loc_id,"g1",0);
 root_id   = H5Gopen(loc_id, "/");

/*-------------------------------------------------------------------------
 * H5G_TYPE
 *-------------------------------------------------------------------------
 */

 /* Create a memory compound datatype */
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_INT);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_FLOAT);
 /* Commit compound datatype and close it */
 H5Tcommit(loc_id, "type", type_id);
 H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5G_LINK
 *-------------------------------------------------------------------------
 */
 
 H5Glink(loc_id, H5G_LINK_SOFT, "dset", "link");

/*-------------------------------------------------------------------------
 * write a series of datasetes on the group, and root group
 *-------------------------------------------------------------------------
 */

 write_dset_in(root_id,"dset_referenced",loc_id,0);
 write_dset_in(group_id,"dset_referenced",loc_id,0);


 /* Close */
 H5Dclose(dset_id);
 H5Gclose(group_id);
 H5Gclose(root_id);

 return 0;                                                 
 
}


/*-------------------------------------------------------------------------
 * Function: make_attributes
 *
 * Purpose: make a test file with all types of attributes 
 *
 *-------------------------------------------------------------------------
 */
int make_attributes(hid_t loc_id)
{
 hid_t   dset_id;
 hid_t   group_id;
 hid_t   root_id;
 hid_t   space_id;
 hsize_t dims[1]={2};
 
 
/*-------------------------------------------------------------------------
 * H5G_DATASET
 *-------------------------------------------------------------------------
 */

 space_id = H5Screate_simple(1,dims,NULL);
 dset_id  = H5Dcreate(loc_id,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
 H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */ 
 group_id  = H5Gcreate(loc_id,"g1",0);
 root_id   = H5Gopen(loc_id, "/");

/*-------------------------------------------------------------------------
 * write a series of attributes on the dataset, group, and root group
 *-------------------------------------------------------------------------
 */

 write_attr_in(dset_id,"dset",loc_id,0);
 write_attr_in(group_id,"dset",loc_id,0);
 write_attr_in(root_id,"dset",loc_id,0);

 /* Close */
 H5Dclose(dset_id);
 H5Gclose(group_id);
 H5Gclose(root_id);

 return 0;                                                 
 
}



/*-------------------------------------------------------------------------
 * Function: make_special_objects
 *
 * Purpose: make a test file with non common items  
 *
 *-------------------------------------------------------------------------
 */
int make_special_objects(hid_t loc_id)
{
 hid_t   group1_id;
 hid_t   group2_id;
 hid_t   group3_id;
 hsize_t dims[2]={3,2};
 int     buf[3][2]= {{1,1},{1,2},{2,2}};  
 hid_t   dset_id;
 hid_t   space_id;  
	hid_t   plist_id;
 herr_t  status;
	int     fillvalue=2;

/*-------------------------------------------------------------------------
 * create a dataset and some hard links to it
 *-------------------------------------------------------------------------
 */

 if (write_dset(loc_id,2,dims,"dset",H5T_NATIVE_INT,buf)<0)
  return -1;
 if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link1 to dset")<0)
  return -1;
 if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link2 to dset")<0)
  return -1;
 if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link3 to dset")<0)
  return -1;


/*-------------------------------------------------------------------------
 * create a group and some hard links to it
 *-------------------------------------------------------------------------
 */ 
 
 if ((group1_id = H5Gcreate(loc_id,"g1",0))<0)
  return -1;
 if ((group2_id = H5Gcreate(group1_id,"g2",0))<0)
  return -1;
 if ((group3_id = H5Gcreate(group2_id,"g3",0))<0)
  return -1;

 if (H5Glink2(loc_id, "g1", H5G_LINK_HARD, group2_id, "link1 to g1")<0)
  return -1;
 if (H5Glink2(group1_id, "g2", H5G_LINK_HARD, group3_id, "link1 to g2")<0)
  return -1;
 
 H5Gclose(group1_id);
 H5Gclose(group2_id);
 H5Gclose(group3_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER, write a fill value
 *-------------------------------------------------------------------------
 */

 plist_id = H5Pcreate(H5P_DATASET_CREATE);
	status = H5Pset_fill_value(plist_id, H5T_NATIVE_INT, &fillvalue);
	space_id = H5Screate_simple(2,dims,NULL);
	dset_id = H5Dcreate(loc_id,"dset_fill",H5T_NATIVE_INT,space_id,plist_id);
	status = H5Dwrite(dset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf);
	status = H5Pclose(plist_id);
	status = H5Dclose(dset_id);
 status = H5Sclose(space_id);

 return 0;                                                 
 
}


