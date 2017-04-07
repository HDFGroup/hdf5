/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/************************************************************
 This example shows how to create intermediate groups with
 a single call to H5Gcreate.
 ************************************************************/
package examples.groups;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.callbacks.H5O_iterate_cb;
import hdf.hdf5lib.callbacks.H5O_iterate_t;
import hdf.hdf5lib.structs.H5O_info_t;

import java.util.ArrayList;

public class H5Ex_G_Intermediate {

    private static String FILE = "H5Ex_G_Intermediate.h5";

    private void CreateGroup() throws Exception {

        long file_id = -1;
        long group_id = -1;
        long gcpl_id = -1;

        try {
            // Create a new file_id using the default properties.
            file_id = H5.H5Fcreate(FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);

            // Create group_id creation property list and set it to allow creation of intermediate group_ids.
            gcpl_id = H5.H5Pcreate(HDF5Constants.H5P_LINK_CREATE);
            H5.H5Pset_create_intermediate_group(gcpl_id, true);

            /*
             * Create the group_id /G1/G2/G3. Note that /G1 and /G1/G2 do not exist yet. This call would cause an error
             * if we did not use the previously created property list.
             */
            group_id = H5
                    .H5Gcreate(file_id, "/G1/G2/G3", gcpl_id, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            // Print all the objects in the file_ids to show that intermediate group_ids have been created.
            System.out.println("Objects in the file_id:");

            // H5O_iterate_t iter_data = null;
            H5O_iterate_t iter_data = new H5O_iter_data();
            H5O_iterate_cb iter_cb = new H5O_iter_callback();

            H5.H5Ovisit(file_id, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_NATIVE, iter_cb, iter_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            // Close and release resources.
            if (gcpl_id >= 0)
                H5.H5Pclose(gcpl_id);
            if (group_id >= 0)
                H5.H5Gclose(group_id);
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
    }

    public static void main(String[] args) {
        try {
            (new H5Ex_G_Intermediate()).CreateGroup();
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private class idata {
        public String link_name = null;
        public int link_type = -1;

        idata(String name, int type) {
            this.link_name = name;
            this.link_type = type;
        }
    }

    private class H5O_iter_data implements H5O_iterate_t {
        public ArrayList<idata> iterdata = new ArrayList<idata>();
    }

    private class H5O_iter_callback implements H5O_iterate_cb {
        public int callback(long group, String name, H5O_info_t info, H5O_iterate_t op_data) {
            idata id = new idata(name, info.type);
            ((H5O_iter_data) op_data).iterdata.add(id);

            System.out.print("/"); /* Print root group in object path */

            // Check if the current object is the root group, and if not print the full path name and type.

            if (name.charAt(0) == '.') /* Root group, do not print '.' */
                System.out.println("  (Group)");
            else if (info.type == HDF5Constants.H5O_TYPE_GROUP)
                System.out.println(name + "  (Group)");
            else if (info.type == HDF5Constants.H5O_TYPE_DATASET)
                System.out.println(name + "  (Dataset)");
            else if (info.type == HDF5Constants.H5O_TYPE_NAMED_DATATYPE)
                System.out.println(name + "  (Datatype)");
            else
                System.out.println(name + "  (Unknown)");

            return 0;
        }
    }

}
