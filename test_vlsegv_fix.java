import java.util.ArrayList;

import hdf.hdf5lib.*;
import hdf.hdf5lib.exceptions.*;

public class test_vlsegv_fix {
    public static void main(String[] args)
    {
        System.out.println("Testing VL SIGSEGV fix...");

        try {
            // Initialize HDF5 library
            H5.H5open();
            System.out.println("HDF5 library initialized successfully");

            // Create a simple file for testing
            long file_id = H5.H5Fcreate("test_vl.h5", HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                        HDF5Constants.H5P_DEFAULT);
            if (file_id < 0) {
                System.err.println("Failed to create file");
                return;
            }

            // Create a variable-length integer datatype
            long vltype_id = H5.H5Tvlen_create(HDF5Constants.H5T_NATIVE_INT);
            if (vltype_id < 0) {
                System.err.println("Failed to create VL datatype");
                H5.H5Fclose(file_id);
                return;
            }

            // Create dataspace
            long[] dims   = {2};
            long space_id = H5.H5Screate_simple(1, dims, null);
            if (space_id < 0) {
                System.err.println("Failed to create dataspace");
                H5.H5Tclose(vltype_id);
                H5.H5Fclose(file_id);
                return;
            }

            // Create attribute
            long attr_id = H5.H5Acreate(file_id, "test_vl_attr", vltype_id, space_id,
                                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            if (attr_id < 0) {
                System.err.println("Failed to create attribute");
                H5.H5Sclose(space_id);
                H5.H5Tclose(vltype_id);
                H5.H5Fclose(file_id);
                return;
            }

            // Create test data
            ArrayList[] writeData = new ArrayList[2];
            writeData[0]          = new ArrayList<Integer>();
            writeData[0].add(1);
            writeData[0].add(2);
            writeData[0].add(3);

            writeData[1] = new ArrayList<Integer>();
            writeData[1].add(10);
            writeData[1].add(20);

            // Write VL data - this should work with our VLDataConverter
            System.out.println("Writing VL data...");
            int status = H5.H5AwriteVL(attr_id, vltype_id, writeData);
            if (status < 0) {
                System.err.println("Failed to write VL data");
            }
            else {
                System.out.println("VL data written successfully");
            }

            // Read VL data back - this was failing with SIGSEGV before our fix
            System.out.println("Reading VL data back...");
            ArrayList[] readData = new ArrayList[2];
            status               = H5.H5AreadVL(attr_id, vltype_id, readData);
            if (status < 0) {
                System.err.println("Failed to read VL data");
            }
            else {
                System.out.println("VL data read successfully!");
                System.out.println("Read data[0]: " + readData[0]);
                System.out.println("Read data[1]: " + readData[1]);
            }

            // Cleanup
            H5.H5Aclose(attr_id);
            H5.H5Sclose(space_id);
            H5.H5Tclose(vltype_id);
            H5.H5Fclose(file_id);

            System.out.println("Test completed successfully - no SIGSEGV!");
        }
        catch (Exception e) {
            System.err.println("Test failed with exception: " + e.getMessage());
            e.printStackTrace();
        }
    }
}