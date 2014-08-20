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
 *  This example illustrates the creation of groups using absolute and 
 *  relative names.  It is used in the HDF5 Tutorial.
 */

#include "hdf5.h"
#define FILE "groups.h5"

int main() {

   hid_t       file_id, group1_id, group2_id, group3_id;  /* identifiers */
   herr_t      status;

   /* Create a new file using default properties. */
   file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

   /* Create group "MyGroup" in the root group using absolute name. */
   group1_id = H5Gcreate2(file_id, "/MyGroup", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

   /* Create group "Group_A" in group "MyGroup" using absolute name. */
   group2_id = H5Gcreate2(file_id, "/MyGroup/Group_A", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

   /* Create group "Group_B" in group "MyGroup" using relative name. */
   group3_id = H5Gcreate2(group1_id, "Group_B", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

   /* Close groups. */
   status = H5Gclose(group1_id);
   status = H5Gclose(group2_id);
   status = H5Gclose(group3_id);

   /* Close the file. */
   status = H5Fclose(file_id);
}
