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
  This example shows how to read and write object references
  to a dataset.  The program first creates objects in the
  file and writes references to those objects to a dataset
  with a dataspace of DIM0, then closes the file.  Next, it
  reopens the file, dereferences the references, and outputs
  the names of their targets to the screen.
 ************************************************************/
package examples.datatypes;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class H5Ex_T_ObjectReference {
    private static String FILENAME = "H5Ex_T_ObjectReference.h5";
    private static String DATASETNAME = "DS1";
    private static String DATASETNAME2 = "DS2";
    private static String GROUPNAME = "G1";
    private static final int DIM0 = 2;
    private static final int RANK = 1;

    // Values for the status of space allocation
    enum H5G_obj {
        H5G_UNKNOWN(HDF5Constants.H5O_TYPE_UNKNOWN), /* Unknown object type */
        H5G_GROUP(HDF5Constants.H5O_TYPE_GROUP), /* Object is a group */
        H5G_DATASET(HDF5Constants.H5O_TYPE_DATASET), /* Object is a dataset */
        H5G_TYPE(HDF5Constants.H5O_TYPE_NAMED_DATATYPE); /* Object is a named data type */
        private static final Map<Integer, H5G_obj> lookup = new HashMap<Integer, H5G_obj>();

        static {
            for (H5G_obj s : EnumSet.allOf(H5G_obj.class))
                lookup.put(s.getCode(), s);
        }

        private int code;

        H5G_obj(int layout_type) {
            this.code = layout_type;
        }

        public int getCode() {
            return this.code;
        }

        public static H5G_obj get(int code) {
            return lookup.get(code);
        }
    }

    private static void writeObjRef() {
        long file_id = -1;
        long dataspace_id = -1;
        long filespace_id = -1;
        long group_id = -1;
        long dataset_id = -1;
        long[] dims = { DIM0 };
        byte[][] dset_data = new byte[DIM0][8];

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataset with a scalar dataspace.
        try {
            dataspace_id = H5.H5Screate(HDF5Constants.H5S_SCALAR);
            if ((file_id >= 0) && (dataspace_id >= 0)) {
                dataset_id = H5.H5Dcreate(file_id, DATASETNAME2, HDF5Constants.H5T_STD_I32LE, dataspace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                if (dataset_id >= 0)
                    H5.H5Dclose(dataset_id);
                dataset_id = -1;
                H5.H5Sclose(dataspace_id);
                dataspace_id = -1;
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create a group in the file.
        try {
            if (file_id >= 0)
                group_id = H5.H5Gcreate(file_id, GROUPNAME, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
            if (group_id >= 0)
                H5.H5Gclose(group_id);
            group_id = -1;
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create references to the previously created objects. Passing -1
        // as space_id causes this parameter to be ignored. Other values
        // besides valid dataspaces result in an error.
        try {
            if (file_id >= 0) {
                byte rbuf0[] = H5.H5Rcreate(file_id, GROUPNAME, HDF5Constants.H5R_OBJECT, -1);
                byte rbuf1[] = H5.H5Rcreate(file_id, DATASETNAME2, HDF5Constants.H5R_OBJECT, -1);
                for (int indx = 0; indx < 8; indx++) {
                    dset_data[0][indx] = rbuf0[indx];
                    dset_data[1][indx] = rbuf1[indx];
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            filespace_id = H5.H5Screate_simple(RANK, dims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset.
        try {
            if ((file_id >= 0) && (filespace_id >= 0))
                dataset_id = H5.H5Dcreate(file_id, DATASETNAME, HDF5Constants.H5T_STD_REF_OBJ, filespace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the object references to it.
        try {
            if (dataset_id >= 0)
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_STD_REF_OBJ, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.

        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (filespace_id >= 0)
                H5.H5Sclose(filespace_id);
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

    private static void readObjRef() {
        long file_id = -1;
        long dataset_id = -1;
        long dataspace_id = -1;
        int object_type = -1;
        long object_id = -1;
        long[] dims = { DIM0 };
        byte[][] dset_data;

        // Open an existing file.
        try {
            file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open an existing dataset.
        try {
            if (file_id >= 0)
                dataset_id = H5.H5Dopen(file_id, DATASETNAME, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Get dataspace and allocate memory for read buffer.
        try {
            if (dataset_id >= 0)
                dataspace_id = H5.H5Dget_space(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataspace_id >= 0)
                H5.H5Sget_simple_extent_dims(dataspace_id, dims, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Allocate array of pointers to two-dimensional arrays (the
        // elements of the dataset.
        dset_data = new byte[(int)dims[0]][8];

        // Read the data using the default properties.
        try {
            if (dataset_id >= 0) {
                H5.H5Dread(dataset_id, HDF5Constants.H5T_STD_REF_OBJ, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, dset_data);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.println(DATASETNAME + "[" + indx + "]:");
            System.out.print("  ->");
            // Open the referenced object, get its name and type.
            try {
                if (dataset_id >= 0) {
                    object_id = H5.H5Rdereference(dataset_id, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_OBJECT, dset_data[indx]);
                    object_type = H5.H5Rget_obj_type(dataset_id, HDF5Constants.H5R_OBJECT, dset_data[indx]);
                }
                String obj_name = null;
                if (object_type >= 0) {
                    // Get the length of the name and retrieve the name.
                    obj_name = H5.H5Iget_name(object_id);
                }
                if ((object_id >= 0) && (object_type >= -1)) {
                    switch (H5G_obj.get(object_type)) {
                    case H5G_GROUP:
                        System.out.print("H5G_GROUP");
                        try {
                            if (object_id >= 0)
                                H5.H5Gclose(object_id);
                        }
                        catch (Exception e) {
                            e.printStackTrace();
                        }
                        break;
                    case H5G_DATASET:
                        System.out.print("H5G_DATASET");
                        try {
                            if (object_id >= 0)
                                H5.H5Dclose(object_id);
                        }
                        catch (Exception e) {
                            e.printStackTrace();
                        }
                        break;
                    case H5G_TYPE:
                        System.out.print("H5G_TYPE");
                        try {
                            if (object_id >= 0)
                                H5.H5Tclose(object_id);
                        }
                        catch (Exception e) {
                            e.printStackTrace();
                        }
                        break;
                    default:
                        System.out.print("UNHANDLED");
                    }
                }
                // Print the name.
                System.out.println(": " + obj_name);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }

        // End access to the dataset and release resources used by it.
        try {
            if (dataspace_id >= 0)
                H5.H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
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
        // Check if gzip compression is available and can be used for both
        // compression and decompression. Normally we do not perform error
        // checking in these examples for the sake of clarity, but in this
        // case we will make an exception because this filter is an
        // optional part of the hdf5 library.
        H5Ex_T_ObjectReference.writeObjRef();
        H5Ex_T_ObjectReference.readObjRef();
    }

}
