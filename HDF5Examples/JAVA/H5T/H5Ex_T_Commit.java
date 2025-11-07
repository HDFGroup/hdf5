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
  This example shows how to commit a named datatype to a
  file, and read back that datatype.  The program first
  defines a compound datatype, commits it to a file, then
  closes the file.  Next, it reopens the file, opens the
  datatype, and outputs the names of its fields to the
  screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public class H5Ex_T_Commit {
    private static String FILENAME           = "H5Ex_T_Commit.h5";
    private static String DATATYPENAME       = "Sensor_Type";
    protected static final int INTEGERSIZE   = 4;
    protected static final int DOUBLESIZE    = 8;
    protected final static int MAXSTRINGSIZE = 80;

    // Values for the various classes of datatypes
    enum H5T_class {
        H5T_NO_CLASS(H5T_NO_CLASS()),   // error
        H5T_INTEGER(H5T_INTEGER()),     // integer types
        H5T_FLOAT(H5T_FLOAT()),         // floating-point types
        H5T_TIME(H5T_TIME()),           // date and time types
        H5T_STRING(H5T_STRING()),       // character string types
        H5T_BITFIELD(H5T_BITFIELD()),   // bit field types
        H5T_OPAQUE(H5T_OPAQUE()),       // opaque types
        H5T_COMPOUND(H5T_COMPOUND()),   // compound types
        H5T_REFERENCE(H5T_REFERENCE()), // reference types
        H5T_ENUM(H5T_ENUM()),           // enumeration types
        H5T_VLEN(H5T_VLEN()),           // Variable-Length types
        H5T_ARRAY(H5T_ARRAY()),         // Array types
        H5T_COMPLEX(H5T_COMPLEX()),     // Complex number types
        H5T_NCLASSES(12);               // this must be last

        private static final Map<Long, H5T_class> lookup = new HashMap<Long, H5T_class>();

        static
        {
            for (H5T_class s : EnumSet.allOf(H5T_class.class))
                lookup.put(s.getCode(), s);
        }

        private long code;

        H5T_class(long layout_type) { this.code = layout_type; }

        public long getCode() { return this.code; }

        public static H5T_class get(long typeclass_id) { return lookup.get(typeclass_id); }
    }

    // The supporting Sensor_Datatype class.
    private static class Sensor_Datatype {
        static int numberMembers = 4;
        static int[] memberDims  = {1, 1, 1, 1};

        String[] memberNames   = {"Serial number", "Location", "Temperature (F)", "Pressure (inHg)"};
        long[] memberFileTypes = {H5T_STD_I32BE_g(), H5T_C_S1_g(), H5T_IEEE_F64BE_g(), H5T_IEEE_F64BE_g()};
        static int[] memberStorage = {INTEGERSIZE, MAXSTRINGSIZE, DOUBLESIZE, DOUBLESIZE};

        // Data size is the storage size for the members not the object.
        static long getDataSize()
        {
            long data_size = 0;
            for (int indx = 0; indx < numberMembers; indx++)
                data_size += memberStorage[indx] * memberDims[indx];
            return data_size;
        }

        static int getOffset(int memberItem)
        {
            int data_offset = 0;
            for (int indx = 0; indx < memberItem; indx++)
                data_offset += memberStorage[indx];
            return data_offset;
        }
    }

    private static void CreateDataType(Arena arena)
    {
        long file_id              = H5I_INVALID_HID();
        long strtype_id           = H5I_INVALID_HID();
        long filetype_id          = H5I_INVALID_HID();
        Sensor_Datatype datatypes = new Sensor_Datatype();
        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create string datatype.
        try {
            strtype_id = H5Tcopy(H5T_C_S1_g());
            if (strtype_id >= 0)
                H5Tset_size(strtype_id, MAXSTRINGSIZE);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the compound datatype for the file. Because the standard
        // types we are using for the file may have different sizes than
        // the corresponding native types, we must manually calculate the
        // offset of each member.
        try {
            filetype_id = H5Tcreate(H5T_COMPOUND(), Sensor_Datatype.getDataSize());
            if (filetype_id >= 0) {
                for (int indx = 0; indx < Sensor_Datatype.numberMembers; indx++) {
                    long type_id = datatypes.memberFileTypes[indx];
                    if (type_id == H5T_C_S1_g())
                        type_id = strtype_id;
                    H5Tinsert(filetype_id, arena.allocateFrom(datatypes.memberNames[indx]),
                              Sensor_Datatype.getOffset(indx), type_id);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Commit the compound datatype to the file, creating a named datatype.
        try {
            if ((file_id >= 0) && (filetype_id >= 0))
                H5Tcommit2(file_id, arena.allocateFrom(DATATYPENAME), filetype_id, H5P_DEFAULT(),
                           H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the file type.
        try {
            if (filetype_id >= 0)
                H5Tclose(filetype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the str type.
        try {
            if (strtype_id >= 0)
                H5Tclose(strtype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
        try {
            if (file_id >= 0)
                H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void ReadDataType(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long typeclass_id = H5I_INVALID_HID();
        long filetype_id  = H5I_INVALID_HID();

        // Open an existing file.
        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open named datatype.
        try {
            if (file_id >= 0)
                filetype_id = H5Topen2(file_id, arena.allocateFrom(DATATYPENAME), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        System.out.println("Named datatype:  " + DATATYPENAME + ":");

        // Get datatype class. If it isn't compound, we won't print anything.
        try {
            if (filetype_id >= 0)
                typeclass_id = H5Tget_class(filetype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        // Read data.
        try {
            if (H5T_class.get(typeclass_id) == H5T_class.H5T_COMPOUND) {
                System.out.println("   Class: H5T_COMPOUND");
                int nmembs = H5Tget_nmembers(filetype_id);
                // Iterate over compound datatype members.
                for (int indx = 0; indx < nmembs; indx++) {
                    MemorySegment nameSeg = H5Tget_member_name(filetype_id, indx);
                    String member_name    = nameSeg.getString(0);
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
                H5Tclose(filetype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
        try {
            if (file_id >= 0)
                H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args)
    {

        try (Arena arena = Arena.ofConfined()) {
            H5Ex_T_Commit.CreateDataType(arena);
            H5Ex_T_Commit.ReadDataType(arena);
        }
    }
}
