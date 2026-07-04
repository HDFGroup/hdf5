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
  This example shows how to read and write compound
  datatypes to an attribute.  The program first writes
  compound structures to an attribute with a dataspace of
  DIM0, then closes the file.  Next, it reopens the file,
  reads back the data, and outputs it to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import hdf.hdf5lib.H5;

public class H5Ex_T_CompoundAttribute {
    private static String FILENAME           = "H5Ex_T_CompoundAttribute.h5";
    private static String DATASETNAME        = "DS1";
    private static String ATTRIBUTENAME      = "A1";
    private static final int DIM0            = 4;
    private static final int RANK            = 1;
    protected static final int INTEGERSIZE   = 4;
    protected static final int DOUBLESIZE    = 8;
    protected final static int MAXSTRINGSIZE = 80;

    static class Sensor_Datatype {
        static int numberMembers = 4;
        static int[] memberDims  = {1, 1, 1, 1};

        static String[] memberNames   = {"Serial number", "Location", "Temperature (F)", "Pressure (inHg)"};
        static long[] memberMemTypes  = {H5T_NATIVE_INT_g(), H5T_C_S1_g(), H5T_NATIVE_DOUBLE_g(),
                                         H5T_NATIVE_DOUBLE_g()};
        static long[] memberFileTypes = {H5T_STD_I32BE_g(), H5T_C_S1_g(), H5T_IEEE_F64BE_g(),
                                         H5T_IEEE_F64BE_g()};
        static int[] memberStorage    = {INTEGERSIZE, MAXSTRINGSIZE, DOUBLESIZE, DOUBLESIZE};

        // Data size is the storage size for the members not the object.
        static long getTotalDataSize()
        {
            long data_size = 0;
            for (int indx = 0; indx < numberMembers; indx++)
                data_size += memberStorage[indx] * memberDims[indx];
            return DIM0 * data_size;
        }

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

    static class Sensor {
        public int serial_no;
        public String location;
        public double temperature;
        public double pressure;

        Sensor(int serial_no, String location, double temperature, double pressure)
        {
            this.serial_no   = serial_no;
            this.location    = location;
            this.temperature = temperature;
            this.pressure    = pressure;
        }

        Sensor(List data)
        {
            this.serial_no   = (int)data.get(0);
            this.location    = (String)data.get(1);
            this.temperature = (double)data.get(2);
            this.pressure    = (double)data.get(3);
        }

        Sensor(ByteBuffer databuf, int dbposition) { readBuffer(databuf, dbposition); }

        void writeBuffer(ByteBuffer databuf, int dbposition)
        {
            databuf.putInt(dbposition + Sensor_Datatype.getOffset(0), serial_no);
            byte[] temp_str = location.getBytes(Charset.forName("UTF-8"));
            int arraylen    = (temp_str.length > MAXSTRINGSIZE) ? MAXSTRINGSIZE : temp_str.length;
            for (int ndx = 0; ndx < arraylen; ndx++)
                databuf.put(dbposition + Sensor_Datatype.getOffset(1) + ndx, temp_str[ndx]);
            for (int ndx = arraylen; ndx < MAXSTRINGSIZE; ndx++)
                databuf.put(dbposition + Sensor_Datatype.getOffset(1) + arraylen, (byte)0);
            databuf.putDouble(dbposition + Sensor_Datatype.getOffset(2), temperature);
            databuf.putDouble(dbposition + Sensor_Datatype.getOffset(3), pressure);
        }

        void readBuffer(ByteBuffer databuf, int dbposition)
        {
            this.serial_no       = databuf.getInt(dbposition + Sensor_Datatype.getOffset(0));
            ByteBuffer stringbuf = databuf.duplicate();
            stringbuf.position(dbposition + Sensor_Datatype.getOffset(1));
            stringbuf.limit(dbposition + Sensor_Datatype.getOffset(1) + MAXSTRINGSIZE);
            byte[] bytearr = new byte[stringbuf.remaining()];
            stringbuf.get(bytearr);
            this.location    = new String(bytearr, Charset.forName("UTF-8")).trim();
            this.temperature = databuf.getDouble(dbposition + Sensor_Datatype.getOffset(2));
            this.pressure    = databuf.getDouble(dbposition + Sensor_Datatype.getOffset(3));
        }

        List get()
        {
            List data = new ArrayList<>();
            data.add(this.serial_no);
            data.add(this.location);
            data.add(this.temperature);
            data.add(this.pressure);
            return data;
        }

        void put(List data)
        {
            this.serial_no   = (int)data.get(0);
            this.location    = (String)data.get(1);
            this.temperature = (double)data.get(2);
            this.pressure    = (double)data.get(3);
        }

        @Override
        public String toString()
        {
            return String.format("Serial number   : " + serial_no + "%n"
                                 + "Location        : " + location + "%n"
                                 + "Temperature (F) : " + temperature + "%n"
                                 + "Pressure (inHg) : " + pressure + "%n");
        }
    }

    private static void CreateDataset(Arena arena)
    {
        long file_id            = H5I_INVALID_HID();
        long strtype_id         = H5I_INVALID_HID();
        long memtype_id         = H5I_INVALID_HID();
        long filetype_id        = H5I_INVALID_HID();
        long dataspace_id       = H5I_INVALID_HID();
        long dataset_id         = H5I_INVALID_HID();
        long attribute_id       = H5I_INVALID_HID();
        long[] dims             = {DIM0};
        ArrayList[] object_data = new ArrayList[DIM0];
        byte[] dset_data        = null;

        // Initialize data.
        object_data[0] = (ArrayList) new Sensor(1153, new String("Exterior (static)"), 53.23, 24.57).get();
        object_data[1] = (ArrayList) new Sensor(1184, new String("Intake"), 55.12, 22.95).get();
        object_data[2] = (ArrayList) new Sensor(1027, new String("Intake manifold"), 103.55, 31.23).get();
        object_data[3] = (ArrayList) new Sensor(1313, new String("Exhaust manifold"), 1252.89, 84.11).get();

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

        // Create the compound datatype for memory.
        try {
            memtype_id = H5Tcreate(H5T_COMPOUND(), Sensor_Datatype.getDataSize());
            if (memtype_id >= 0) {
                for (int indx = 0; indx < Sensor_Datatype.numberMembers; indx++) {
                    long type_id = Sensor_Datatype.memberMemTypes[indx];
                    if (type_id == H5T_C_S1_g())
                        type_id = strtype_id;
                    H5Tinsert(memtype_id, arena.allocateFrom(Sensor_Datatype.memberNames[indx]),
                              Sensor_Datatype.getOffset(indx), type_id);
                }
            }
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
                    long type_id = Sensor_Datatype.memberFileTypes[indx];
                    if (type_id == H5T_C_S1_g())
                        type_id = strtype_id;
                    H5Tinsert(filetype_id, arena.allocateFrom(Sensor_Datatype.memberNames[indx]),
                              Sensor_Datatype.getOffset(indx), type_id);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataset with a scalar dataspace.
        try {
            dataspace_id = H5Screate(H5S_SCALAR());
            if (dataspace_id >= 0) {
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_STD_I32LE_g(),
                                        dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                H5Sclose(dataspace_id);
                dataspace_id = H5I_INVALID_HID();
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataspace. Setting maximum size to NULL sets the maximum
        // size to be the current size.
        try {
            dataspace_id =
                H5Screate_simple(RANK, arena.allocateFrom(ValueLayout.JAVA_LONG, dims), MemorySegment.NULL);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the attribute.
        try {
            if ((dataset_id >= 0) && (dataspace_id >= 0) && (filetype_id >= 0))
                attribute_id = H5Acreate2(dataset_id, arena.allocateFrom(ATTRIBUTENAME), filetype_id,
                                          dataspace_id, H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Write the compound data.
        try {
            if ((attribute_id >= 0) && (memtype_id >= 0))
                H5.H5AwriteVL(attribute_id, memtype_id, (Object[])object_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            if (attribute_id >= 0)
                H5Aclose(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the data space.
        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
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

        // Terminate access to the mem type.
        try {
            if (memtype_id >= 0)
                H5Tclose(memtype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

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

    private static void ReadDataset(Arena arena)
    {
        long file_id          = H5I_INVALID_HID();
        long strtype_id       = H5I_INVALID_HID();
        long memtype_id       = H5I_INVALID_HID();
        long dataspace_id     = H5I_INVALID_HID();
        long dataset_id       = H5I_INVALID_HID();
        long attribute_id     = H5I_INVALID_HID();
        long[] dims           = {DIM0};
        Sensor[] object_data2 = new Sensor[(int)dims[0]];

        // Open an existing file.
        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open an existing dataset.
        try {
            if (file_id >= 0)
                dataset_id = H5Dopen2(file_id, arena.allocateFrom(DATASETNAME), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                attribute_id =
                    H5Aopen_by_name(dataset_id, arena.allocateFrom("."), arena.allocateFrom(ATTRIBUTENAME),
                                    H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Get dataspace and allocate memory for read buffer. This is a
        // three dimensional dataset when the array datatype is included so
        // the dynamic allocation must be done in steps.
        try {
            if (attribute_id >= 0)
                dataspace_id = H5Aget_space(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataspace_id >= 0) {
                MemorySegment dimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
                H5Sget_simple_extent_dims(dataspace_id, dimsSeg, MemorySegment.NULL);
                // Read back the dimensions
                for (int i = 0; i < dims.length; i++) {
                    dims[i] = dimsSeg.getAtIndex(ValueLayout.JAVA_LONG, i);
                }
            }
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

        // Create the compound datatype for memory.
        try {
            memtype_id = H5Tcreate(H5T_COMPOUND(), Sensor_Datatype.getDataSize());
            if (memtype_id >= 0) {
                for (int indx = 0; indx < Sensor_Datatype.numberMembers; indx++) {
                    long type_id = Sensor_Datatype.memberMemTypes[indx];
                    if (type_id == H5T_C_S1_g())
                        type_id = strtype_id;
                    H5Tinsert(memtype_id, arena.allocateFrom(Sensor_Datatype.memberNames[indx]),
                              Sensor_Datatype.getOffset(indx), type_id);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        ArrayList[] object_data = new ArrayList[(int)dims[0]];

        // Read data.
        try {
            if ((attribute_id >= 0) && (memtype_id >= 0))
                H5.H5AreadVL(attribute_id, memtype_id, (Object[])object_data);

            for (int indx = 0; indx < (int)dims[0]; indx++) {
                object_data2[indx] = new Sensor(object_data[indx]);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Output the data to the screen.
        for (int indx = 0; indx < dims[0]; indx++) {
            System.out.println(ATTRIBUTENAME + " [" + indx + "]:");
            System.out.println(object_data2[indx].toString());
        }
        System.out.println();

        try {
            if (attribute_id >= 0)
                H5Aclose(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the data space.
        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the mem type.
        try {
            if (memtype_id >= 0)
                H5Tclose(memtype_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

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

    public static void main(String[] args)
    {

        try (Arena arena = Arena.ofConfined()) {
            H5Ex_T_CompoundAttribute.CreateDataset(arena);
            H5Ex_T_CompoundAttribute.ReadDataset(arena);
        }
    }
}
