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
  This example shows how to iterate over group members using
  H5Gget_obj_info_all.
 ************************************************************/
package examples.groups;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public class H5Ex_G_Iterate {
    private static String FILENAME = "groups/h5ex_g_iterate.h5";
    private static String DATASETNAME = "/";

    enum H5O_type {
        H5O_TYPE_UNKNOWN(-1), // Unknown object type
        H5O_TYPE_GROUP(0), // Object is a group
        H5O_TYPE_DATASET(1), // Object is a dataset
        H5O_TYPE_NAMED_DATATYPE(2), // Object is a named data type
        H5O_TYPE_NTYPES(3); // Number of different object types
        private static final Map<Integer, H5O_type> lookup = new HashMap<Integer, H5O_type>();

        static {
            for (H5O_type s : EnumSet.allOf(H5O_type.class))
                lookup.put(s.getCode(), s);
        }

        private int code;

        H5O_type(int layout_type) {
            this.code = layout_type;
        }

        public int getCode() {
            return this.code;
        }

        public static H5O_type get(int code) {
            return lookup.get(code);
        }
    }

    private static void do_iterate() {
        long file_id = -1;

        // Open a file using default properties.
        try {
            file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Begin iteration.
        System.out.println("Objects in root group:");
        try {
            if (file_id >= 0) {
                int count = (int) H5.H5Gn_members(file_id, DATASETNAME);
                String[] oname = new String[count];
                int[] otype = new int[count];
                int[] ltype = new int[count];
                long[] orefs = new long[count];
                H5.H5Gget_obj_info_all(file_id, DATASETNAME, oname, otype, ltype, orefs, HDF5Constants.H5_INDEX_NAME);

                // Get type of the object and display its name and type.
                for (int indx = 0; indx < otype.length; indx++) {
                    switch (H5O_type.get(otype[indx])) {
                    case H5O_TYPE_GROUP:
                        System.out.println("  Group: " + oname[indx]);
                        break;
                    case H5O_TYPE_DATASET:
                        System.out.println("  Dataset: " + oname[indx]);
                        break;
                    case H5O_TYPE_NAMED_DATATYPE:
                        System.out.println("  Datatype: " + oname[indx]);
                        break;
                    default:
                        System.out.println("  Unknown: " + oname[indx]);
                    }
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
        try {
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        H5Ex_G_Iterate.do_iterate();
    }

}
