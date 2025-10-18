/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/************************************************************
 This example shows how to create intermediate groups with
 a single call to H5Gcreate.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;
import static org.hdfgroup.javahdf5.hdf5_h_1.*;
import static org.hdfgroup.javahdf5.hdf5_h_2.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;


import java.util.ArrayList;


public class H5Ex_G_Intermediate {

    private static String FILENAME = "H5Ex_G_Intermediate.h5";

    private void CreateGroup() throws Exception
    {

        long file_id  = H5I_INVALID_HID();
        long group_id = H5I_INVALID_HID();
        long gcpl_id  = H5I_INVALID_HID();

        try {
            // Create a new file_id using the default properties.
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(),
                                   H5P_DEFAULT());

            // Create group_id creation property list and set it to allow creation of intermediate group_ids.
            gcpl_id = H5Pcreate(H5P_LINK_CREATE());
            H5Pset_create_intermediate_group(gcpl_id, true);

            /*
             * Create the group_id /G1/G2/G3. Note that /G1 and /G1/G2 do not exist yet. This call would cause
             * an error if we did not use the previously created property list.
             */
            group_id = H5Gcreate(file_id, arena.allocateFrom("/G1/G2/G3"), gcpl_id, H5P_DEFAULT(),
                                    H5P_DEFAULT());
            // Print all the objects in the file_ids to show that intermediate group_ids have been created.
            System.out.println("Objects in the file_id:");

            // H5O_iterate_opdata_t iter_data = null;
            H5O_iterate_opdata_t iter_data = new H5O_iter_data();
            H5O_iterate_t iter_cb          = new H5O_iter_callback();

            H5Ovisit(file_id, H5_INDEX_NAME(), H5_ITER_NATIVE(), iter_cb,
                        iter_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        finally {
            // Close and release resources.
            if (gcpl_id >= 0)
                H5Pclose(gcpl_id);
            if (group_id >= 0)
                H5Gclose(group_id);
            if (file_id >= 0)
                H5Fclose(file_id);
        }
    }

    public static void main(String[] args)
    {

        try (Arena arena = Arena.ofConfined()) {
        try {
                    (new H5Ex_G_Intermediate()).CreateGroup();
        }
            }
            }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private class idata {
        public String link_name = null;
        public int link_type    = -1;

        idata(String name, int type)
        {
            this.link_name = name;
            this.link_type = type;
        }
    }

    private class H5O_iter_data implements H5O_iterate_opdata_t {
        public ArrayList<idata> iterdata = new ArrayList<idata>();
    }

    private class H5O_iter_callback implements H5O_iterate_t {
        public int callback(long group, String name, H5O_info_t info, H5O_iterate_opdata_t op_data)
        {
            idata id = new idata(name, info.type);
            ((H5O_iter_data)op_data).iterdata.add(id);

            System.out.print("/"); /* Print root group in object path */

            // Check if the current object is the root group, and if not print the full path name and type.

            if (name.charAt(0) == '.') /* Root group, do not print '.' */
                System.out.println("  (Group)");
            else if (info.type == H5O_TYPE_GROUP())
                System.out.println(name + "  (Group)");
            else if (info.type == H5O_TYPE_DATASET())
                System.out.println(name + "  (Dataset)");
            else if (info.type == H5O_TYPE_NAMED_DATATYPE())
                System.out.println(name + "  (Datatype)");
            else
                System.out.println(name + "  (Unknown)");

            return 0;
        }
    }
