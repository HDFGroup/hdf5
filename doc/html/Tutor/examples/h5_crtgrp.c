/*
 *    Creating and closing a group.
 */

#include "hdf5.h"
#define FILE "group.h5"

main() {

   hid_t       file_id, group_id;  /* identifiers */
   herr_t      status;

   /* Create a new file using default properties. */
   file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

   /* Create a group named "/MyGroup" in the file. */
   group_id = H5Gcreate(file_id, "/MyGroup", 0);

   /* Close the group. */
   status = H5Gclose(group_id);

   /* Terminate access to the file. */
   status = H5Fclose(file_id);
}
