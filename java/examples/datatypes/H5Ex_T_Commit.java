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

/************************************************************
  This example shows how to commit a named datatype to a
  file, and read back that datatype.  The program first
  defines a compound datatype, commits it to a file, then
  closes the file.  Next, it reopens the file, opens the
  datatype, and outputs the names of its fields to the
  screen.
 ************************************************************/

package examples.datatypes;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public class H5Ex_T_Commit {
    private static String FILENAME = "H5Ex_T_Commit.h5";
    private static String DATATYPENAME = "Sensor_Type";
    protected static final int INTEGERSIZE = 4;
    protected static final int DOUBLESIZE = 8;
    protected final static int MAXSTRINGSIZE = 80;

    // Values for the various classes of datatypes
    enum H5T_class {
        H5T_NO_CLASS(HDF5Constants.H5T_NO_CLASS), // error
        H5T_INTEGER(HDF5Constants.H5T_INTEGER), // integer types
        H5T_FLOAT(HDF5Constants.H5T_FLOAT), // floating-point types
        H5T_TIME(HDF5Constants.H5T_TIME), // date and time types
        H5T_STRING(HDF5Constants.H5T_STRING), // character string types
        H5T_BITFIELD(HDF5Constants.H5T_BITFIELD), // bit field types
        H5T_OPAQUE(HDF5Constants.H5T_OPAQUE), // opaque types
        H5T_COMPOUND(HDF5Constants.H5T_COMPOUND), // compound types
        H5T_REFERENCE(HDF5Constants.H5T_REFERENCE), // reference types
        H5T_ENUM(HDF5Constants.H5T_ENUM), // enumeration types
        H5T_VLEN(HDF5Constants.H5T_VLEN), // Variable-Length types
        H5T_ARRAY(HDF5Constants.H5T_ARRAY), // Array types
        H5T_NCLASSES(11); // this must be last

        private static final Map<Long, H5T_class> lookup = new HashMap<Long, H5T_class>();

        static {
            for (H5T_class s : EnumSet.allOf(H5T_class.class))
                lookup.put(s.getCode(), s);
        }

        private long code;

        H5T_class(long layout_type) {
            this.code = layout_type;
        }

        public long getCode() {
            return this.code;
        }

        public static H5T_class get(long typeclass_id) {
            return lookup.get(typeclass_id);
        }
    }

    // The supporting Sensor_Datatype class.
    private static class Sensor_Datatype {
        static int numberMembers = 4;
        static int[] memberDims = { 1, 1, 1, 1 };

        String[] memberNames = { "Serial number", "Location", "Temperature (F)", "Pressure (inHg)" };
        long[] memberFileTypes = { HDF5Constants.H5T_STD_I32BE, HDF5Constants.H5T_C_S1, HDF5Constants.H5T_IEEE_F64BE,
                HDF5Constants.H5T_IEEE_F64BE };
        static int[] memberStorage = { INTEGERSIZE, MAXSTRINGSIZE, DOUBLESIZE, DOUBLESIZE };

        // Data size is the storage size for the members not the object.
        static long getDataSize() {
            long data_size = 0;
            for (int indx = 0; indx < numberMembers; indx++)
                data_size += memberStorage[indx] * memberDims[indx];
            return data_size;
        }

        static int getOffset(int memberItem) {
            int data_offset = 0;
            for (int indx = 0; indx < memberItem; indx++)
                data_offset += memberStorage[indx];
            return data_offset;
        }
    }

    private static void CreateDataType() {
        long file_id = -1;
        long strtype_id = -1;
        long filetype_id = -1;
        Sensor_Datatype datatypes = new Sensor_Datatype();
        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create string datatype.
        try {
            strtype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            if (strtype_id >= 0)
                H5.H5Tset_size(strtype_id, MAXSTRINGSIZE);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the compound datatype for the file. Because the standard
        // types we are using for the file may have different sizes than
        // the corresponding native types, we must manually calculate the
        // offset of each member.
        try {
            filetype_id = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, Sensor_Datatype.getDataSize());
            if (filetype_id >= 0) {
                for (int indx = 0; indx < Sensor_Datatype.numberMembers; indx++) {
                    long type_id = datatypes.memberFileTypes[indx];
                    if (type_id == HDF5Constants.H5T_C_S1)
                        type_id = strtype_id;
                    H5.H5Tinsert(filetype_id, datatypes.memberNames[indx], Sensor_Datatype.getOffset(indx), type_id);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Commit the compound datatype to the file, creating a named datatype.
        try {
            if ((file_id >= 0) && (filetype_id >= 0))
                H5.H5Tcommit(file_id, DATATYPENAME, filetype_id, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the file type.
        try {
            if (filetype_id >= 0)
                H5.H5Tclose(filetype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the str type.
        try {
            if (strtype_id >= 0)
                H5.H5Tclose(strtype_id);
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

    private static void ReadDataType() {
        long file_id = -1;
        long typeclass_id = -1;
        long filetype_id = -1;

        // Open an existing file.
        try {
            file_id = H5.H5Fopen(FILENAME, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open named datatype.
        try {
            if (file_id >= 0)
                filetype_id = H5.H5Topen(file_id, DATATYPENAME, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Named datatype:  " + DATATYPENAME + ":");

        // Get datatype class. If it isn't compound, we won't print anything.
        try {
            if (filetype_id >= 0)
                typeclass_id = H5.H5Tget_class(filetype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        // Read data.
        try {
            if (H5T_class.get(typeclass_id) == H5T_class.H5T_COMPOUND) {
                System.out.println("   Class: H5T_COMPOUND");
                int nmembs = H5.H5Tget_nmembers(filetype_id);
                // Iterate over compound datatype members.
                for (int indx = 0; indx < nmembs; indx++) {
                    String member_name = H5.H5Tget_member_name(filetype_id, indx);
                    System.out.println("    " + member_name);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the mem type.
        try {
            if (filetype_id >= 0)
                H5.H5Tclose(filetype_id);
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
        H5Ex_T_Commit.CreateDataType();
        // Now we begin the read section of this example. Here we assume
        // the dataset and array have the same name and rank, but can have
        // any size. Therefore we must allocate a new array to read in
        // data using malloc().
        H5Ex_T_Commit.ReadDataType();
    }

}
