/*
 *   Creating groups using absolute and relative names.
 */

#include <hdf5.h>
#define FILE "groups.h5"

main() {

   hid_t       file_id, group1_id, group2_id, group3_id;  /* identifiers */
   herr_t      status;

   /* Create a new file using default properties. */
   file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

   /* Create group "MyGroup" in the root group using absolute name. */
   group1_id = H5Gcreate(file_id, "/MyGroup", 0);

   /* Create group "Group_A" in group "MyGroup" using absolute name. */
   group2_id = H5Gcreate(file_id, "/MyGroup/Group_A", 0);

   /* Create group "Group_B" in group "MyGroup" using relative name. */
   group3_id = H5Gcreate(group1_id, "Group_B", 0);

   /* Close groups. */
   status = H5Gclose(group1_id);
   status = H5Gclose(group2_id);
   status = H5Gclose(group3_id);

   /* Close the file. */
   status = H5Fclose(file_id);
}
