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

package hdf.hdf5lib;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

import hdf.hdf5lib.exceptions.HDF5JavaException;

import org.hdfgroup.javahdf5.hvl_t;

/**
 * Utility class for converting between Java ArrayList arrays and HDF5 hvl_t structures
 * for variable-length (VL) data operations in the FFM implementation.
 */
public class VLDataConverter {

    // Logging removed for compilation simplicity - can add back if needed

    /**
     * Container for raw VL data copied from HDF5-managed memory.
     * This prevents any access to HDF5 memory after H5Treclaim.
     */
    private static class RawVLData {
        public final byte[] data;
        public final int length;

        public RawVLData(byte[] data, int length)
        {
            this.data   = data;
            this.length = length;
        }
    }

    /**
     * Convert Java ArrayList array to HDF5 hvl_t MemorySegment array
     *
     * @param javaData Array of ArrayLists containing the variable-length data
     * @param arena Arena for memory allocation
     * @return MemorySegment containing hvl_t array
     * @throws HDF5JavaException if conversion fails
     */
    public static MemorySegment convertToHVL(ArrayList[] javaData, Arena arena) throws HDF5JavaException
    {
        if (javaData == null || javaData.length == 0) {
            throw new HDF5JavaException("Input data array is null or empty");
        }

        MemorySegment hvlArray = hvl_t.allocateArray(javaData.length, arena);

        for (int i = 0; i < javaData.length; i++) {
            MemorySegment hvlElement = hvl_t.asSlice(hvlArray, i);
            convertSingleElement(javaData[i], hvlElement, arena);
        }

        return hvlArray;
    }

    /**
     * Convert HDF5 hvl_t MemorySegment array back to Java ArrayList array.
     * Uses two-phase approach: immediately extract all raw data from HDF5 memory,
     * then process the copied data to prevent access after H5Treclaim.
     *
     * @param hvlArray MemorySegment containing hvl_t array
     * @param arrayLength Number of elements in the array
     * @param elementType HDF5 datatype of the elements (for type inference)
     * @return Array of ArrayLists
     * @throws HDF5JavaException if conversion fails
     */
    public static ArrayList[] convertFromHVL(MemorySegment hvlArray, int arrayLength, long elementType)
        throws HDF5JavaException
    {

        if (hvlArray == null) {
            throw new HDF5JavaException("Input hvl_t array is null");
        }

        ArrayList[] result       = new ArrayList[arrayLength];
        RawVLData[] rawDataArray = new RawVLData[arrayLength];
        boolean isStringType     = isStringType(elementType) || isVLOfStrings(elementType);

        for (int i = 0; i < arrayLength; i++) {
            MemorySegment hvlElement = hvl_t.asSlice(hvlArray, i);
            long len                 = hvl_t.len(hvlElement);
            MemorySegment dataPtr    = hvl_t.p(hvlElement);

            if (len == 0 || dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
                rawDataArray[i] = new RawVLData(new byte[0], 0);
            }
            else {
                if (isStringType) {
                    // For VL strings, hvl_t.p contains a char* directly
                    try {
                        ArrayList<String> directResult = new ArrayList<>(1);
                        if (dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
                            directResult.add("");
                        }
                        else {
                            String str = dataPtr.getString(0, java.nio.charset.StandardCharsets.UTF_8);
                            directResult.add(str);
                        }
                        result[i]       = directResult;
                        rawDataArray[i] = null;
                    }
                    catch (Exception e) {
                        rawDataArray[i] = copyStringVLDataImmediately(dataPtr, (int)len);
                    }
                }
                else {
                    rawDataArray[i] = copyRawVLData(dataPtr, (int)len, elementType);
                }
            }
        }

        long baseElementType        = elementType;
        boolean needToCloseBaseType = false;
        try {
            if (isVLType(elementType)) {
                baseElementType     = getVLBaseType(elementType);
                needToCloseBaseType = true;
            }
        }
        catch (Exception e) {
        }

        try {
            for (int i = 0; i < arrayLength; i++) {
                if (rawDataArray[i] != null) {
                    result[i] = convertRawDataToArrayList(rawDataArray[i], baseElementType);
                }
            }
        }
        finally {
            if (needToCloseBaseType && baseElementType != elementType) {
                try {
                    H5.H5Tclose(baseElementType);
                }
                catch (Exception e) {
                }
            }
        }

        return result;
    }

    /**
     * Convert a single ArrayList to hvl_t structure
     */
    private static void convertSingleElement(ArrayList<?> list, MemorySegment hvlElement, Arena arena)
        throws HDF5JavaException
    {

        if (list == null) {
            // Empty VL element
            hvl_t.len(hvlElement, 0);
            hvl_t.p(hvlElement, MemorySegment.NULL);
            return;
        }

        int size = list.size();
        hvl_t.len(hvlElement, size);

        if (size == 0) {
            hvl_t.p(hvlElement, MemorySegment.NULL);
            return;
        }

        Object firstElement  = list.get(0);
        Class<?> elementType = firstElement.getClass();

        if (elementType == Integer.class) {
            MemorySegment dataArray = convertIntegerVL(list, arena);
            hvl_t.p(hvlElement, dataArray);
        }
        else if (elementType == Double.class) {
            MemorySegment dataArray = convertDoubleVL(list, arena);
            hvl_t.p(hvlElement, dataArray);
        }
        else if (elementType == String.class) {
            MemorySegment dataArray = convertStringVL(list, arena);
            hvl_t.p(hvlElement, dataArray);
        }
        else if (elementType == byte[].class) {
            MemorySegment dataArray = convertByteArrayVL(list, arena);
            hvl_t.p(hvlElement, dataArray);
        }
        else if (firstElement instanceof ArrayList) {
            // Nested VL structure
            MemorySegment dataArray = convertNestedVL(list, arena);
            hvl_t.p(hvlElement, dataArray);
        }
        else {
            throw new HDF5JavaException("Unsupported ArrayList element type: " + elementType.getName());
        }
    }

    /**
     * Convert ArrayList<Integer> to native int array
     */
    @SuppressWarnings("unchecked")
    private static MemorySegment convertIntegerVL(ArrayList<?> list, Arena arena)
    {
        ArrayList<Integer> intList = (ArrayList<Integer>)list;
        MemorySegment dataArray    = arena.allocate(ValueLayout.JAVA_INT, intList.size());

        for (int i = 0; i < intList.size(); i++) {
            dataArray.setAtIndex(ValueLayout.JAVA_INT, i, intList.get(i));
        }

        return dataArray;
    }

    /**
     * Convert ArrayList<Double> to native double array
     */
    @SuppressWarnings("unchecked")
    private static MemorySegment convertDoubleVL(ArrayList<?> list, Arena arena)
    {
        ArrayList<Double> doubleList = (ArrayList<Double>)list;
        MemorySegment dataArray      = arena.allocate(ValueLayout.JAVA_DOUBLE, doubleList.size());

        for (int i = 0; i < doubleList.size(); i++) {
            dataArray.setAtIndex(ValueLayout.JAVA_DOUBLE, i, doubleList.get(i));
        }

        return dataArray;
    }

    /**
     * Convert ArrayList<String> to native array format for HDF5 array datatypes
     * For array datatypes, each ArrayList<String> becomes a fixed-size array of string pointers
     */
    @SuppressWarnings("unchecked")
    private static MemorySegment convertStringVL(ArrayList<?> list, Arena arena)
    {
        ArrayList<String> stringList = (ArrayList<String>)list;

        // For array datatypes containing strings, create a packed array of string pointers
        // This is different from VL strings - array types have fixed size arrays
        MemorySegment stringArray = arena.allocate(ValueLayout.ADDRESS, stringList.size());

        for (int i = 0; i < stringList.size(); i++) {
            String str = stringList.get(i);
            if (str != null) {
                MemorySegment stringSegment = arena.allocateFrom(str, StandardCharsets.UTF_8);
                stringArray.setAtIndex(ValueLayout.ADDRESS, i, stringSegment);
            }
            else {
                stringArray.setAtIndex(ValueLayout.ADDRESS, i, MemorySegment.NULL);
            }
        }

        return stringArray;
    }

    /**
     * Convert ArrayList<byte[]> to native array format for HDF5
     * Used for VL reference data where each element is a byte array (reference)
     */
    @SuppressWarnings("unchecked")
    private static MemorySegment convertByteArrayVL(ArrayList<?> list, Arena arena)
    {
        ArrayList<byte[]> byteArrayList = (ArrayList<byte[]>)list;

        // Calculate total size needed for all byte arrays
        long totalSize = 0;
        for (byte[] array : byteArrayList) {
            if (array != null) {
                totalSize += array.length;
            }
        }

        if (totalSize == 0) {
            return MemorySegment.NULL;
        }

        // For VL reference data, we need to create a contiguous array of all bytes
        // References are typically fixed-size, so we can pack them sequentially
        MemorySegment dataArray = arena.allocate(totalSize);

        long offset = 0;
        for (byte[] array : byteArrayList) {
            if (array != null && array.length > 0) {
                MemorySegment arraySegment = MemorySegment.ofArray(array);
                dataArray.asSlice(offset, array.length).copyFrom(arraySegment);
                offset += array.length;
            }
        }

        return dataArray;
    }

    /**
     * Convert ArrayList array to array datatype buffer (not hvl_t)
     * Used for H5T_ARRAY datatypes where each element is a fixed-size array
     */
    public static MemorySegment convertArrayDatatype(ArrayList[] data, long mem_type_id, Arena arena)
        throws HDF5JavaException
    {
        try {
            // Get the array type information
            long baseTypeId = org.hdfgroup.javahdf5.hdf5_h.H5Tget_super(mem_type_id);
            if (baseTypeId < 0) {
                throw new HDF5JavaException("Failed to get array base type");
            }

            // Get array dimensions
            int ndims = org.hdfgroup.javahdf5.hdf5_h.H5Tget_array_ndims(mem_type_id);
            if (ndims != 1) {
                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Only 1D arrays are supported, got " + ndims + "D");
            }

            // Get the array size (number of elements per array)
            MemorySegment dims = arena.allocate(ValueLayout.JAVA_LONG, 1);
            int result         = org.hdfgroup.javahdf5.hdf5_h.H5Tget_array_dims2(mem_type_id, dims);
            if (result < 0) {
                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Failed to get array dimensions");
            }
            int arraySize = (int)dims.get(ValueLayout.JAVA_LONG, 0);

            // Check if the base type is variable-length string
            int isVLStringResult = org.hdfgroup.javahdf5.hdf5_h.H5Tis_variable_str(baseTypeId);
            boolean isVLString   = isVLStringResult > 0;

            if (isVLString) {
                // Each entry in data[] is an ArrayList<String> with arraySize elements
                // Pack as array of string pointers
                MemorySegment buffer = arena.allocate(ValueLayout.ADDRESS, data.length * arraySize);

                for (int i = 0; i < data.length; i++) {
                    ArrayList<String> stringArray = (ArrayList<String>)data[i];
                    if (stringArray.size() != arraySize) {
                        org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                        throw new HDF5JavaException("Array element " + i + " has " + stringArray.size() +
                                                    " elements, expected " + arraySize);
                    }

                    // Pack string pointers for this array element
                    for (int j = 0; j < arraySize; j++) {
                        String str = stringArray.get(j);
                        if (str != null) {
                            MemorySegment stringSegment = arena.allocateFrom(str, StandardCharsets.UTF_8);
                            buffer.setAtIndex(ValueLayout.ADDRESS, i * arraySize + j, stringSegment);
                        }
                        else {
                            buffer.setAtIndex(ValueLayout.ADDRESS, i * arraySize + j, MemorySegment.NULL);
                        }
                    }
                }

                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                return buffer;
            }
            else {
                // Check for other supported base types
                int baseTypeClass = org.hdfgroup.javahdf5.hdf5_h.H5Tget_class(baseTypeId);

                if (baseTypeClass == HDF5Constants.H5T_INTEGER) {
                    // Support integer arrays
                    MemorySegment buffer = arena.allocate(ValueLayout.JAVA_INT, data.length * arraySize);

                    for (int i = 0; i < data.length; i++) {
                        ArrayList<Integer> intArray = (ArrayList<Integer>)data[i];
                        if (intArray.size() != arraySize) {
                            org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                            throw new HDF5JavaException("Array element " + i + " has " + intArray.size() +
                                                        " elements, expected " + arraySize);
                        }

                        for (int j = 0; j < arraySize; j++) {
                            Integer val = intArray.get(j);
                            buffer.setAtIndex(ValueLayout.JAVA_INT, i * arraySize + j, val != null ? val : 0);
                        }
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    return buffer;
                }
                else if (baseTypeClass == HDF5Constants.H5T_FLOAT) {
                    // Support double arrays
                    MemorySegment buffer = arena.allocate(ValueLayout.JAVA_DOUBLE, data.length * arraySize);

                    for (int i = 0; i < data.length; i++) {
                        ArrayList<Double> doubleArray = (ArrayList<Double>)data[i];
                        if (doubleArray.size() != arraySize) {
                            org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                            throw new HDF5JavaException("Array element " + i + " has " + doubleArray.size() +
                                                        " elements, expected " + arraySize);
                        }

                        for (int j = 0; j < arraySize; j++) {
                            Double val = doubleArray.get(j);
                            buffer.setAtIndex(ValueLayout.JAVA_DOUBLE, i * arraySize + j,
                                              val != null ? val : 0.0);
                        }
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    return buffer;
                }
                else if (baseTypeClass == HDF5Constants.H5T_ENUM) {
                    // Support enum arrays - treat enums as integers
                    MemorySegment buffer = arena.allocate(ValueLayout.JAVA_INT, data.length * arraySize);

                    for (int i = 0; i < data.length; i++) {
                        ArrayList<Integer> enumArray = (ArrayList<Integer>)data[i];
                        if (enumArray.size() != arraySize) {
                            org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                            throw new HDF5JavaException("Array element " + i + " has " + enumArray.size() +
                                                        " elements, expected " + arraySize);
                        }

                        for (int j = 0; j < arraySize; j++) {
                            Integer val = enumArray.get(j);
                            buffer.setAtIndex(ValueLayout.JAVA_INT, i * arraySize + j, val != null ? val : 0);
                        }
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    return buffer;
                }
                else {
                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    throw new HDF5JavaException("Unsupported array base type for FFM conversion: " +
                                                baseTypeClass);
                }
            }
        }
        catch (Exception e) {
            if (e instanceof HDF5JavaException) {
                throw e;
            }
            throw new HDF5JavaException("Array datatype conversion failed: " + e.getMessage());
        }
    }

    /**
     * Read array datatype data from HDF5 attribute (not hvl_t)
     * Used for H5T_ARRAY datatypes where each element is a fixed-size array
     */
    public static ArrayList[] readArrayDatatype(long attr_id, long mem_type_id, int count, Arena arena)
        throws HDF5JavaException
    {
        try {
            // Get the array type information
            long baseTypeId = org.hdfgroup.javahdf5.hdf5_h.H5Tget_super(mem_type_id);
            if (baseTypeId < 0) {
                throw new HDF5JavaException("Failed to get array base type");
            }

            // Get array dimensions
            int ndims = org.hdfgroup.javahdf5.hdf5_h.H5Tget_array_ndims(mem_type_id);
            if (ndims != 1) {
                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Only 1D arrays are supported, got " + ndims + "D");
            }

            // Get the array size (number of elements per array)
            MemorySegment dims = arena.allocate(ValueLayout.JAVA_LONG, 1);
            int result         = org.hdfgroup.javahdf5.hdf5_h.H5Tget_array_dims2(mem_type_id, dims);
            if (result < 0) {
                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Failed to get array dimensions");
            }
            int arraySize = (int)dims.get(ValueLayout.JAVA_LONG, 0);

            // Check if the base type is variable-length string
            int isVLStringResult = org.hdfgroup.javahdf5.hdf5_h.H5Tis_variable_str(baseTypeId);
            boolean isVLString   = isVLStringResult > 0;

            if (isVLString) {
                // Allocate buffer for array of string pointers
                MemorySegment buffer = arena.allocate(ValueLayout.ADDRESS, count * arraySize);

                // Read data from HDF5
                int status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, buffer);
                if (status < 0) {
                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    throw new HDF5JavaException("Failed to read VL string array data");
                }

                // IMMEDIATELY copy string data before any potential reclaim
                String[][] copiedStringData = new String[count][arraySize];
                for (int i = 0; i < count; i++) {
                    for (int j = 0; j < arraySize; j++) {
                        MemorySegment stringPtr = buffer.getAtIndex(ValueLayout.ADDRESS, i * arraySize + j);
                        if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                            try {
                                // Reinterpret the pointer with global scope to access the string data
                                MemorySegment stringData =
                                    MemorySegment.ofAddress(stringPtr.address()).reinterpret(Long.MAX_VALUE);
                                copiedStringData[i][j] = stringData.getString(0, StandardCharsets.UTF_8);
                            }
                            catch (Exception e) {
                                copiedStringData[i][j] = null;
                            }
                        }
                        else {
                            copiedStringData[i][j] = null;
                        }
                    }
                }

                // Clean up VL string memory AFTER copying
                long space_id = org.hdfgroup.javahdf5.hdf5_h.H5Aget_space(attr_id);
                if (space_id >= 0) {
                    try {
                        // Reclaim memory
                        int reclaim_status = org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(
                            mem_type_id, space_id, HDF5Constants.H5P_DEFAULT, buffer);
                        if (reclaim_status < 0) {
                            System.err.println("Warning: Failed to reclaim VL string memory");
                        }
                    }
                    finally {
                        org.hdfgroup.javahdf5.hdf5_h.H5Sclose(space_id);
                    }
                }

                // Convert copied string data to ArrayList array
                ArrayList[] resultArray = new ArrayList[count];
                for (int i = 0; i < count; i++) {
                    ArrayList<String> stringArray = new ArrayList<>(arraySize);
                    for (int j = 0; j < arraySize; j++) {
                        stringArray.add(copiedStringData[i][j]);
                    }
                    resultArray[i] = stringArray;
                }

                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                return resultArray;
            }
            else {
                // Check for other supported base types
                int baseTypeClass = org.hdfgroup.javahdf5.hdf5_h.H5Tget_class(baseTypeId);

                if (baseTypeClass == HDF5Constants.H5T_INTEGER) {
                    // Support integer arrays
                    MemorySegment buffer = arena.allocate(ValueLayout.JAVA_INT, count * arraySize);

                    // Read data from HDF5
                    int status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, buffer);
                    if (status < 0) {
                        org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                        throw new HDF5JavaException("Failed to read array data");
                    }

                    // Convert to ArrayList array
                    ArrayList[] resultArray = new ArrayList[count];
                    for (int i = 0; i < count; i++) {
                        ArrayList<Integer> intArray = new ArrayList<>(arraySize);
                        for (int j = 0; j < arraySize; j++) {
                            int value = buffer.getAtIndex(ValueLayout.JAVA_INT, i * arraySize + j);
                            intArray.add(value);
                        }
                        resultArray[i] = intArray;
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    return resultArray;
                }
                else if (baseTypeClass == HDF5Constants.H5T_FLOAT) {
                    // Support double arrays
                    MemorySegment buffer = arena.allocate(ValueLayout.JAVA_DOUBLE, count * arraySize);

                    // Read data from HDF5
                    int status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, buffer);
                    if (status < 0) {
                        org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                        throw new HDF5JavaException("Failed to read array data");
                    }

                    // Convert to ArrayList array
                    ArrayList[] resultArray = new ArrayList[count];
                    for (int i = 0; i < count; i++) {
                        ArrayList<Double> doubleArray = new ArrayList<>(arraySize);
                        for (int j = 0; j < arraySize; j++) {
                            double value = buffer.getAtIndex(ValueLayout.JAVA_DOUBLE, i * arraySize + j);
                            doubleArray.add(value);
                        }
                        resultArray[i] = doubleArray;
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    return resultArray;
                }
                else {
                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    throw new HDF5JavaException("Unsupported array base type for FFM reading: " +
                                                baseTypeClass);
                }
            }
        }
        catch (Exception e) {
            if (e instanceof HDF5JavaException) {
                throw e;
            }
            throw new HDF5JavaException("Array datatype reading failed: " + e.getMessage());
        }
    }

    /**
     * Read array datatype data from HDF5 dataset (not hvl_t)
     * Used for H5T_ARRAY datatypes where each element is a fixed-size array
     */
    public static ArrayList[] readArrayDatatypeFromDataset(long dataset_id, long mem_type_id,
                                                           long mem_space_id, long file_space_id,
                                                           long xfer_plist_id, int count, Arena arena)
        throws HDF5JavaException
    {
        try {
            // Get the array type information
            long baseTypeId = org.hdfgroup.javahdf5.hdf5_h.H5Tget_super(mem_type_id);
            if (baseTypeId < 0) {
                throw new HDF5JavaException("Failed to get array base type");
            }

            // Get array dimensions
            int ndims = org.hdfgroup.javahdf5.hdf5_h.H5Tget_array_ndims(mem_type_id);
            if (ndims != 1) {
                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Only 1D arrays are supported, got " + ndims + "D");
            }

            // Get the array size (number of elements per array)
            MemorySegment dims = arena.allocate(ValueLayout.JAVA_LONG, 1);
            int result         = org.hdfgroup.javahdf5.hdf5_h.H5Tget_array_dims2(mem_type_id, dims);
            if (result < 0) {
                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Failed to get array dimensions");
            }
            int arraySize = (int)dims.get(ValueLayout.JAVA_LONG, 0);

            // Check if the base type is variable-length string
            int isVLStringResult = org.hdfgroup.javahdf5.hdf5_h.H5Tis_variable_str(baseTypeId);
            boolean isVLString   = isVLStringResult > 0;

            if (isVLString) {
                // Allocate buffer for array of string pointers
                MemorySegment buffer = arena.allocate(ValueLayout.ADDRESS, count * arraySize);

                // Read data from HDF5
                int status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                                  file_space_id, xfer_plist_id, buffer);
                if (status < 0) {
                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    throw new HDF5JavaException("Failed to read array data");
                }

                // IMMEDIATELY copy string data before any potential reclaim
                String[][] copiedStringData = new String[count][arraySize];
                for (int i = 0; i < count; i++) {
                    for (int j = 0; j < arraySize; j++) {
                        MemorySegment stringPtr = buffer.getAtIndex(ValueLayout.ADDRESS, i * arraySize + j);
                        if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                            // Reinterpret the pointer with global scope to access the string data
                            MemorySegment stringData =
                                MemorySegment.ofAddress(stringPtr.address()).reinterpret(Long.MAX_VALUE);
                            copiedStringData[i][j] = stringData.getString(0, StandardCharsets.UTF_8);
                        }
                        else {
                            copiedStringData[i][j] = null;
                        }
                    }
                }

                // Clean up VL string memory AFTER copying
                long space_id = (mem_space_id >= 0) ? mem_space_id : file_space_id;
                try {
                    org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(
                        mem_type_id, space_id, org.hdfgroup.javahdf5.hdf5_h.H5P_DEFAULT(), buffer);
                }
                finally {
                    // space_id is parameter, don't close it
                }

                // Now convert copied data to ArrayList array
                ArrayList[] resultArray = new ArrayList[count];
                for (int i = 0; i < count; i++) {
                    ArrayList<String> stringArray = new ArrayList<>(arraySize);
                    for (int j = 0; j < arraySize; j++) {
                        stringArray.add(copiedStringData[i][j]);
                    }
                    resultArray[i] = stringArray;
                }

                org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                return resultArray;
            }
            else {
                // Check for other supported base types
                int baseTypeClass = org.hdfgroup.javahdf5.hdf5_h.H5Tget_class(baseTypeId);

                if (baseTypeClass == HDF5Constants.H5T_INTEGER) {
                    // Support integer arrays
                    MemorySegment buffer = arena.allocate(ValueLayout.JAVA_INT, count * arraySize);

                    // Read data from HDF5
                    int status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                                      file_space_id, xfer_plist_id, buffer);
                    if (status < 0) {
                        org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                        throw new HDF5JavaException("Failed to read array data");
                    }

                    // Convert to ArrayList array
                    ArrayList[] resultArray = new ArrayList[count];
                    for (int i = 0; i < count; i++) {
                        ArrayList<Integer> intArray = new ArrayList<>(arraySize);
                        for (int j = 0; j < arraySize; j++) {
                            int value = buffer.getAtIndex(ValueLayout.JAVA_INT, i * arraySize + j);
                            intArray.add(value);
                        }
                        resultArray[i] = intArray;
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    return resultArray;
                }
                else if (baseTypeClass == HDF5Constants.H5T_FLOAT) {
                    // Support double arrays
                    MemorySegment buffer = arena.allocate(ValueLayout.JAVA_DOUBLE, count * arraySize);

                    // Read data from HDF5
                    int status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                                      file_space_id, xfer_plist_id, buffer);
                    if (status < 0) {
                        org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                        throw new HDF5JavaException("Failed to read array data");
                    }

                    // Convert to ArrayList array
                    ArrayList[] resultArray = new ArrayList[count];
                    for (int i = 0; i < count; i++) {
                        ArrayList<Double> doubleArray = new ArrayList<>(arraySize);
                        for (int j = 0; j < arraySize; j++) {
                            double value = buffer.getAtIndex(ValueLayout.JAVA_DOUBLE, i * arraySize + j);
                            doubleArray.add(value);
                        }
                        resultArray[i] = doubleArray;
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    return resultArray;
                }
                else if (baseTypeClass == HDF5Constants.H5T_ENUM) {
                    // Support enum arrays - treat enums as integers
                    MemorySegment buffer = arena.allocate(ValueLayout.JAVA_INT, count * arraySize);

                    // Read data from HDF5
                    int status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                                      file_space_id, xfer_plist_id, buffer);
                    if (status < 0) {
                        org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                        throw new HDF5JavaException("Failed to read array data");
                    }

                    // Convert to ArrayList array
                    ArrayList[] resultArray = new ArrayList[count];
                    for (int i = 0; i < count; i++) {
                        ArrayList<Integer> enumArray = new ArrayList<>(arraySize);
                        for (int j = 0; j < arraySize; j++) {
                            int value = buffer.getAtIndex(ValueLayout.JAVA_INT, i * arraySize + j);
                            enumArray.add(value);
                        }
                        resultArray[i] = enumArray;
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    return resultArray;
                }
                else {
                    org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseTypeId);
                    throw new HDF5JavaException("Unsupported array base type for FFM reading: " +
                                                baseTypeClass);
                }
            }
        }
        catch (Exception e) {
            if (e instanceof HDF5JavaException) {
                throw e;
            }
            throw new HDF5JavaException("Array datatype reading failed: " + e.getMessage());
        }
    }

    /**
     * Convert nested ArrayList<ArrayList<?>> to hvl_t array
     */
    @SuppressWarnings("unchecked")
    private static MemorySegment convertNestedVL(ArrayList<?> list, Arena arena) throws HDF5JavaException
    {
        ArrayList<ArrayList<?>> nestedList = (ArrayList<ArrayList<?>>)list;
        MemorySegment nestedHvlArray       = hvl_t.allocateArray(nestedList.size(), arena);

        for (int i = 0; i < nestedList.size(); i++) {
            MemorySegment hvlElement = hvl_t.asSlice(nestedHvlArray, i);
            convertSingleElement(nestedList.get(i), hvlElement, arena);
        }

        return nestedHvlArray;
    }

    /**
     * Copy raw bytes from HDF5-managed memory to Java-managed memory immediately.
     */
    private static RawVLData copyRawVLData(MemorySegment dataPtr, int len, long elementType)
        throws HDF5JavaException
    {
        if (len == 0 || dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
            return new RawVLData(new byte[0], 0);
        }

        try {
            long baseType               = elementType;
            boolean needToCloseBaseType = false;

            try {
                if (hdf.hdf5lib.H5.H5Tdetect_class(elementType, hdf.hdf5lib.HDF5Constants.H5T_VLEN)) {
                    baseType            = org.hdfgroup.javahdf5.hdf5_h.H5Tget_super(elementType);
                    needToCloseBaseType = true;
                }
            }
            catch (Exception e) {
            }

            try {
                long elementSize = org.hdfgroup.javahdf5.hdf5_h.H5Tget_size(baseType);
                long totalSize   = (long)len * elementSize;

                if (elementSize == 0) {
                    throw new RuntimeException("Zero element size - trigger fallback");
                }

                byte[] rawData = copyWithReinterpret(dataPtr, totalSize, len);
                if (rawData.length > 0) {
                    return new RawVLData(rawData, len);
                }

                return new RawVLData(new byte[0], len);
            }
            finally {
                if (needToCloseBaseType && baseType != elementType) {
                    try {
                        org.hdfgroup.javahdf5.hdf5_h.H5Tclose(baseType);
                    }
                    catch (Exception ex) {
                    }
                }
            }
        }
        catch (Exception e) {
            try {
                byte[] rawData = copyWithReinterpret(dataPtr, (long)len * 8, len);
                if (rawData.length > 0) {
                    return new RawVLData(rawData, len);
                }
                rawData = copyWithReinterpret(dataPtr, (long)len * 4, len);
                if (rawData.length > 0) {
                    return new RawVLData(rawData, len);
                }
            }
            catch (Exception fallbackEx) {
            }
            return new RawVLData(new byte[0], len);
        }
    }

    /**
     * Copy data using FFM reinterpret with proper sizing.
     */
    private static byte[] copyWithReinterpret(MemorySegment dataPtr, long totalSize, int len)
    {
        try {
            MemorySegment reinterpretedSegment = dataPtr.reinterpret(totalSize, Arena.global(), null);
            byte[] rawData                     = new byte[(int)totalSize];
            for (int i = 0; i < totalSize; i++) {
                rawData[i] = reinterpretedSegment.get(ValueLayout.JAVA_BYTE, i);
            }
            return rawData;
        }
        catch (Exception e) {
            return new byte[0];
        }
    }

    /**
     * Copy string VL data immediately, extracting actual string content.
     */
    private static RawVLData copyStringVLDataImmediately(MemorySegment dataPtr, int len)
        throws HDF5JavaException
    {
        if (len == 0 || dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
            return new RawVLData(new byte[0], 0);
        }

        try {
            java.util.List<Byte> allStringBytes = new java.util.ArrayList<>();
            allStringBytes.add((byte)(len & 0xFF));
            allStringBytes.add((byte)((len >> 8) & 0xFF));
            allStringBytes.add((byte)((len >> 16) & 0xFF));
            allStringBytes.add((byte)((len >> 24) & 0xFF));

            try {
                String str      = dataPtr.getString(0, StandardCharsets.UTF_8);
                byte[] strBytes = str.getBytes(StandardCharsets.UTF_8);

                allStringBytes.set(0, (byte)1);
                allStringBytes.set(1, (byte)0);
                allStringBytes.set(2, (byte)0);
                allStringBytes.set(3, (byte)0);

                int strLen = strBytes.length;
                allStringBytes.add((byte)(strLen & 0xFF));
                allStringBytes.add((byte)((strLen >> 8) & 0xFF));
                allStringBytes.add((byte)((strLen >> 16) & 0xFF));
                allStringBytes.add((byte)((strLen >> 24) & 0xFF));

                for (byte b : strBytes) {
                    allStringBytes.add(b);
                }
            }
            catch (Exception directStringEx) {
                try {
                    long pointerArraySize = (long)len * 8;
                    MemorySegment reinterpretedArray =
                        dataPtr.reinterpret(pointerArraySize, Arena.global(), null);

                    for (int i = 0; i < len; i++) {
                        try {
                            MemorySegment stringPtr = reinterpretedArray.getAtIndex(ValueLayout.ADDRESS, i);

                            if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                                String str      = stringPtr.getString(0, StandardCharsets.UTF_8);
                                byte[] strBytes = str.getBytes(StandardCharsets.UTF_8);

                                int strLen = strBytes.length;
                                allStringBytes.add((byte)(strLen & 0xFF));
                                allStringBytes.add((byte)((strLen >> 8) & 0xFF));
                                allStringBytes.add((byte)((strLen >> 16) & 0xFF));
                                allStringBytes.add((byte)((strLen >> 24) & 0xFF));

                                for (byte b : strBytes) {
                                    allStringBytes.add(b);
                                }
                            }
                            else {
                                allStringBytes.add((byte)0);
                                allStringBytes.add((byte)0);
                                allStringBytes.add((byte)0);
                                allStringBytes.add((byte)0);
                            }
                        }
                        catch (Exception e) {
                            allStringBytes.add((byte)0);
                            allStringBytes.add((byte)0);
                            allStringBytes.add((byte)0);
                            allStringBytes.add((byte)0);
                        }
                    }
                }
                catch (Exception reinterpretEx) {
                    allStringBytes.add((byte)0);
                    allStringBytes.add((byte)0);
                    allStringBytes.add((byte)0);
                    allStringBytes.add((byte)0);
                }
            }

            byte[] result = new byte[allStringBytes.size()];
            for (int i = 0; i < result.length; i++) {
                result[i] = allStringBytes.get(i);
            }

            return new RawVLData(result, 1);
        }
        catch (Exception e) {
            throw new HDF5JavaException("Failed to copy string VL data: " + e.getMessage());
        }
    }

    /**
     * Convert copied raw data to ArrayList using Java-managed memory only.
     */
    private static ArrayList<?> convertRawDataToArrayList(RawVLData rawData, long elementType)
        throws HDF5JavaException
    {
        if (rawData.length == 0) {
            return new ArrayList<>();
        }

        if (rawData.data.length == 0 && rawData.length > 0) {
            ArrayList<Object> fallback = new ArrayList<>(rawData.length);
            for (int i = 0; i < rawData.length; i++) {
                fallback.add(null);
            }
            return fallback;
        }

        try {
            if (isIntegerType(elementType)) {
                return convertRawDataToIntegerList(rawData);
            }
            else if (isDoubleType(elementType)) {
                return convertRawDataToDoubleList(rawData);
            }
            else if (isStringType(elementType)) {
                return convertRawDataToStringList(rawData);
            }
            else if (isReferenceType(elementType)) {
                return convertRawDataToByteArrayList(rawData);
            }
            else if (isVLType(elementType)) {
                return convertRawDataToNestedVLList(rawData, elementType);
            }
            else {
                return detectAndConvertUnknownType(rawData, elementType);
            }
        }
        catch (Exception e) {
            ArrayList<Object> fallback = new ArrayList<>();
            for (int i = 0; i < rawData.length; i++) {
                fallback.add(null);
            }
            return fallback;
        }
    }

    /**
     * Convert raw bytes to Integer ArrayList
     */
    private static ArrayList<Integer> convertRawDataToIntegerList(RawVLData rawData)
    {
        ArrayList<Integer> result = new ArrayList<>(rawData.length);

        byte[] data     = rawData.data;
        int bytesPerInt = Integer.BYTES;
        int maxInts     = Math.min(rawData.length, data.length / bytesPerInt);

        for (int i = 0; i < maxInts; i++) {
            int offset = i * bytesPerInt;
            if (offset + bytesPerInt <= data.length) {
                // Reconstruct integer from bytes (little-endian)
                int value = (data[offset] & 0xFF) | ((data[offset + 1] & 0xFF) << 8) |
                            ((data[offset + 2] & 0xFF) << 16) | (data[offset + 3] << 24);
                result.add(value);
            }
        }

        return result;
    }

    /**
     * Convert raw bytes to Double ArrayList
     */
    private static ArrayList<Double> convertRawDataToDoubleList(RawVLData rawData)
    {
        ArrayList<Double> result = new ArrayList<>(rawData.length);

        byte[] data        = rawData.data;
        int bytesPerDouble = Double.BYTES;
        int maxDoubles     = Math.min(rawData.length, data.length / bytesPerDouble);

        for (int i = 0; i < maxDoubles; i++) {
            int offset = i * bytesPerDouble;
            if (offset + bytesPerDouble <= data.length) {
                // Reconstruct double from bytes (little-endian)
                long longBits = 0;
                for (int j = 0; j < 8; j++) {
                    longBits |= ((long)(data[offset + j] & 0xFF)) << (j * 8);
                }
                double value = Double.longBitsToDouble(longBits);
                result.add(value);
            }
        }

        return result;
    }

    /**
     * Convert raw bytes to String ArrayList, decoding packed format.
     */
    private static ArrayList<String> convertRawDataToStringList(RawVLData rawData)
    {
        ArrayList<String> result = new ArrayList<>();

        if (rawData.length == 0 || rawData.data.length < 4) {
            for (int i = 0; i < rawData.length; i++) {
                result.add("");
            }
            return result;
        }

        byte[] data = rawData.data;

        try {
            if (data.length >= 4) {
                int numStrings =
                    (data[0] & 0xFF) | ((data[1] & 0xFF) << 8) | ((data[2] & 0xFF) << 16) | (data[3] << 24);

                if (numStrings == rawData.length && numStrings > 0) {
                    int offset = 4;

                    for (int i = 0; i < numStrings && offset + 4 <= data.length; i++) {
                        int strLen = (data[offset] & 0xFF) | ((data[offset + 1] & 0xFF) << 8) |
                                     ((data[offset + 2] & 0xFF) << 16) | (data[offset + 3] << 24);
                        offset += 4;

                        if (strLen == 0) {
                            result.add("");
                        }
                        else if (offset + strLen <= data.length) {
                            byte[] strBytes = new byte[strLen];
                            System.arraycopy(data, offset, strBytes, 0, strLen);
                            String str = new String(strBytes, StandardCharsets.UTF_8);
                            result.add(str);
                            offset += strLen;
                        }
                        else {
                            result.add("");
                        }
                    }
                }
            }

            while (result.size() < rawData.length) {
                result.add("");
            }
        }
        catch (Exception e) {
            result.clear();
            for (int i = 0; i < rawData.length; i++) {
                result.add("");
            }
        }

        return result;
    }

    /**
     * Convert raw bytes to byte array ArrayList (for HDF5 references).
     */
    private static ArrayList<byte[]> convertRawDataToByteArrayList(RawVLData rawData)
    {
        ArrayList<byte[]> result = new ArrayList<>();

        if (rawData.length == 0 || rawData.data.length == 0) {
            return result;
        }

        byte[] data = rawData.data;

        try {
            if (data.length >= 4) {
                int numRefs =
                    (data[0] & 0xFF) | ((data[1] & 0xFF) << 8) | ((data[2] & 0xFF) << 16) | (data[3] << 24);

                if (numRefs == rawData.length && numRefs > 0) {
                    int offset = 4;

                    for (int i = 0; i < numRefs && offset + 4 <= data.length; i++) {
                        int refLen = (data[offset] & 0xFF) | ((data[offset + 1] & 0xFF) << 8) |
                                     ((data[offset + 2] & 0xFF) << 16) | (data[offset + 3] << 24);
                        offset += 4;

                        if (offset + refLen <= data.length) {
                            byte[] refData = new byte[refLen];
                            System.arraycopy(data, offset, refData, 0, refLen);
                            result.add(refData);
                            offset += refLen;
                        }
                        else {
                            result.add(new byte[0]);
                        }
                    }
                }
                else {
                    result.add(data.clone());
                }
            }
            else {
                result.add(data.clone());
            }
        }
        catch (Exception e) {
            result.clear();
            result.add(data.clone());
        }

        return result;
    }

    /**
     * Convert raw bytes to nested VL ArrayList.
     */
    private static ArrayList<ArrayList<?>> convertRawDataToNestedVLList(RawVLData rawData, long elementType)
        throws HDF5JavaException
    {
        ArrayList<ArrayList<?>> result = new ArrayList<>(rawData.length);

        if (rawData.length == 0 || rawData.data.length == 0) {
            return result;
        }

        try {
            Arena tempArena   = Arena.global();
            byte[] data       = rawData.data;
            int hvlSize       = 16;
            int maxStructs    = data.length / hvlSize;
            int actualStructs = Math.min(maxStructs, rawData.length);

            if (actualStructs > 0) {
                MemorySegment reconstructedHvlArray = tempArena.allocate(data.length);
                reconstructedHvlArray.copyFrom(MemorySegment.ofArray(data));
                result = convertNestedVLImmediately(reconstructedHvlArray, actualStructs, elementType);
            }

            while (result.size() < rawData.length) {
                result.add(new ArrayList<>());
            }
        }
        catch (Exception e) {
            result.clear();
            for (int i = 0; i < rawData.length; i++) {
                result.add(new ArrayList<>());
            }
        }

        return result;
    }

    /**
     * Detect and convert unknown HDF5 datatypes by examining the raw data
     */
    private static ArrayList<?> detectAndConvertUnknownType(RawVLData rawData, long elementType)
    {
        // Try to detect the data type from content
        if (rawData.data.length == 0) {
            return new ArrayList<>();
        }

        // Check if it looks like packed string data (starts with count)
        if (rawData.data.length >= 4) {
            int possibleCount = (rawData.data[0] & 0xFF) | ((rawData.data[1] & 0xFF) << 8) |
                                ((rawData.data[2] & 0xFF) << 16) | (rawData.data[3] << 24);

            if (possibleCount == rawData.length && possibleCount > 0 && possibleCount < 1000) {
                // Looks like string data
                return convertRawDataToStringList(rawData);
            }
        }

        // Check if it looks like integer data
        if (rawData.data.length >= rawData.length * 4) {
            try {
                return convertRawDataToIntegerList(rawData);
            }
            catch (Exception e) {
                // Not integer data
            }
        }

        // Check if it looks like double data
        if (rawData.data.length >= rawData.length * 8) {
            try {
                return convertRawDataToDoubleList(rawData);
            }
            catch (Exception e) {
                // Not double data
            }
        }

        // Fallback: create empty list
        ArrayList<Object> result = new ArrayList<>();
        for (int i = 0; i < rawData.length; i++) {
            result.add(""); // Use empty string as safe fallback
        }
        return result;
    }

    /**
     * IMMEDIATE conversion that extracts all data before any H5Treclaim can invalidate memory
     * This follows the JNI translate pattern of immediate data copying.
     * DEPRECATED: Use the two-phase approach instead
     */
    @Deprecated
    private static ArrayList<?> convertSingleElementImmediately(MemorySegment dataPtr, int len,
                                                                long elementType) throws HDF5JavaException
    {

        if (len == 0 || dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
            return new ArrayList<>();
        }

        // IMMEDIATE data extraction based on HDF5 datatype
        if (isIntegerType(elementType)) {
            return convertIntegerVLFromHVL(dataPtr, len);
        }
        else if (isDoubleType(elementType)) {
            return convertDoubleVLFromHVL(dataPtr, len);
        }
        else if (isStringType(elementType)) {
            return convertStringVLFromHVL(dataPtr, len);
        }
        else if (isVLType(elementType)) {
            // For nested VL, we need to extract all nested hvl_t data IMMEDIATELY
            return convertNestedVLImmediately(dataPtr, len, elementType);
        }
        else {
            throw new HDF5JavaException("Unsupported HDF5 datatype for VL conversion: " + elementType);
        }
    }

    /**
     * Legacy method kept for compatibility - now delegates to immediate conversion
     * CRITICAL: This method should not be used for new code - use convertSingleElementImmediately instead
     * This method exists only for backward compatibility and should be avoided
     */
    @Deprecated
    private static ArrayList<?> convertSingleElementFromHVL(MemorySegment hvlElement, long elementType)
        throws HDF5JavaException
    {

        // CRITICAL: Extract hvl_t data IMMEDIATELY to prevent access after H5Treclaim
        long len              = hvl_t.len(hvlElement);
        MemorySegment dataPtr = hvl_t.p(hvlElement);

        return convertSingleElementImmediately(dataPtr, (int)len, elementType);
    }

    /**
     * Convert native int array back to ArrayList<Integer>
     */
    private static ArrayList<Integer> convertIntegerVLFromHVL(MemorySegment dataPtr, int len)
    {
        ArrayList<Integer> result = new ArrayList<>(len);

        // Check if we have a valid memory segment
        if (dataPtr == null || dataPtr.equals(MemorySegment.NULL) || len <= 0) {
            return result;
        }

        // IMPORTANT: For nested VL structures, we cannot trust the byteSize()
        // since HDF5 may invalidate memory at any time. We must be more defensive.
        long requiredBytes = (long)len * Integer.BYTES;

        // Use safe bounds checking without relying on byteSize() for HDF5 managed memory
        boolean canCheckSize = true;
        try {
            // Only check size for non-HDF5 managed memory segments
            if (dataPtr.byteSize() != Long.MAX_VALUE && dataPtr.byteSize() > 0) {
                if (dataPtr.byteSize() < requiredBytes) {
                    throw new HDF5JavaException("Memory segment too small: has " + dataPtr.byteSize() +
                                                " bytes, need " + requiredBytes + " for " + len +
                                                " integers");
                }
            }
        }
        catch (Exception e) {
            // If we can't check the size safely, proceed with caution
            canCheckSize = false;
        }

        for (int i = 0; i < len; i++) {
            // Handle unaligned memory by reading as bytes and reconstructing integer
            long offset = (long)i * Integer.BYTES;
            int value;
            try {
                value = dataPtr.getAtIndex(ValueLayout.JAVA_INT, i);
            }
            catch (IllegalArgumentException e) {
                // Memory is not aligned for direct int access, read as bytes with bounds checking
                try {
                    // Extra safety: check if we can read each byte before accessing
                    if (offset + 3 >= 0) { // Basic sanity check
                        byte b0 = dataPtr.get(ValueLayout.JAVA_BYTE, offset);
                        byte b1 = dataPtr.get(ValueLayout.JAVA_BYTE, offset + 1);
                        byte b2 = dataPtr.get(ValueLayout.JAVA_BYTE, offset + 2);
                        byte b3 = dataPtr.get(ValueLayout.JAVA_BYTE, offset + 3);
                        // Reconstruct integer in native byte order (little-endian on x86)
                        value = (b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | (b3 << 24);
                    }
                    else {
                        throw new HDF5JavaException("Invalid offset for integer at index " + i);
                    }
                }
                catch (Exception ex) {
                    // If we get a SIGSEGV-type error, the memory is no longer valid
                    throw new HDF5JavaException("Memory access violation at index " + i + " (offset " +
                                                offset +
                                                ") - memory may have been freed by HDF5: " + ex.getMessage());
                }
            }
            catch (Exception e) {
                // Catch any other access violations
                throw new HDF5JavaException("Memory access error at index " + i +
                                            " - memory segment may be invalid: " + e.getMessage());
            }
            result.add(value);
        }

        return result;
    }

    /**
     * Convert native double array back to ArrayList<Double>
     */
    private static ArrayList<Double> convertDoubleVLFromHVL(MemorySegment dataPtr, int len)
    {
        ArrayList<Double> result = new ArrayList<>(len);

        // Check if we have a valid memory segment
        if (dataPtr == null || dataPtr.equals(MemorySegment.NULL) || len <= 0) {
            return result;
        }

        // IMPORTANT: For nested VL structures, we cannot trust the byteSize()
        // since HDF5 may invalidate memory at any time. We must be more defensive.
        long requiredBytes = (long)len * Double.BYTES;

        // Use safe bounds checking without relying on byteSize() for HDF5 managed memory
        boolean canCheckSize = true;
        try {
            // Only check size for non-HDF5 managed memory segments
            if (dataPtr.byteSize() != Long.MAX_VALUE && dataPtr.byteSize() > 0) {
                if (dataPtr.byteSize() < requiredBytes) {
                    throw new HDF5JavaException("Memory segment too small: has " + dataPtr.byteSize() +
                                                " bytes, need " + requiredBytes + " for " + len + " doubles");
                }
            }
        }
        catch (Exception e) {
            // If we can't check the size safely, proceed with caution
            canCheckSize = false;
        }

        for (int i = 0; i < len; i++) {
            // Handle unaligned memory by reading as bytes and reconstructing double
            long offset = (long)i * Double.BYTES;
            double value;
            try {
                value = dataPtr.getAtIndex(ValueLayout.JAVA_DOUBLE, i);
            }
            catch (IllegalArgumentException e) {
                // Memory is not aligned for direct double access, read as bytes with bounds checking
                try {
                    long longBits = 0;
                    for (int j = 0; j < 8; j++) {
                        byte b = dataPtr.get(ValueLayout.JAVA_BYTE, offset + j);
                        longBits |= ((long)(b & 0xFF)) << (j * 8);
                    }
                    value = Double.longBitsToDouble(longBits);
                }
                catch (Exception ex) {
                    throw new HDF5JavaException("Failed to read double at index " + i + " (offset " + offset +
                                                "): " + ex.getMessage());
                }
            }
            result.add(value);
        }

        return result;
    }

    /**
     * Convert native char** array back to ArrayList<String>
     */
    private static ArrayList<String> convertStringVLFromHVL(MemorySegment dataPtr, int len)
    {
        // For variable-length strings (H5T_C_S1 + H5T_VARIABLE),
        // the hvl_t structure is interpreted differently than for regular VL data
        ArrayList<String> result = new ArrayList<>(1);

        // Check if we have a valid memory segment
        if (dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
            result.add(""); // Add empty string for invalid data
            return result;
        }

        try {
            // For VL strings, check if 'len' might be a string pointer address
            if (len > 0x1000000) {
                try {
                    MemorySegment stringPtr =
                        MemorySegment.ofAddress(len).reinterpret(100, Arena.global(), null);
                    String str = stringPtr.getString(0, java.nio.charset.StandardCharsets.UTF_8);
                    result.add(str);
                    return result;
                }
                catch (Exception addrException) {
                    // Fall through to direct read
                }
            }

            String str = dataPtr.getString(0, java.nio.charset.StandardCharsets.UTF_8);
            result.add(str);
        }
        catch (Exception e) {
            result.add("");
        }

        return result;
    }

    /**
     * Extract nested VL data immediately before H5Treclaim.
     */
    private static ArrayList<ArrayList<?>> convertNestedVLImmediately(MemorySegment dataPtr, int len,
                                                                      long elementType)
        throws HDF5JavaException
    {
        ArrayList<ArrayList<?>> result = new ArrayList<>(len);
        long baseType                  = getVLBaseType(elementType);

        try {
            for (int i = 0; i < len; i++) {
                MemorySegment nestedHvlElement = hvl_t.asSlice(dataPtr, i);
                long nestedLen                 = hvl_t.len(nestedHvlElement);
                MemorySegment nestedDataPtr    = hvl_t.p(nestedHvlElement);

                if (nestedLen == 0 || nestedDataPtr == null || nestedDataPtr.equals(MemorySegment.NULL)) {
                    result.add(new ArrayList<>());
                    continue;
                }

                ArrayList<?> nestedList =
                    convertSingleElementImmediately(nestedDataPtr, (int)nestedLen, baseType);
                result.add(nestedList);
            }
        }
        finally {
            try {
                H5.H5Tclose(baseType);
            }
            catch (Exception e) {
            }
        }

        return result;
    }

    /**
     * Legacy nested VL conversion - now delegates to immediate version
     */
    private static ArrayList<ArrayList<?>> convertNestedVLFromHVL(MemorySegment dataPtr, int len,
                                                                  long elementType) throws HDF5JavaException
    {

        return convertNestedVLImmediately(dataPtr, len, elementType);
    }

    /**
     * Safely convert a nested VL element by immediately copying data
     */
    private static ArrayList<?> convertNestedElementSafely(MemorySegment dataPtr, int len, long elementType)
        throws HDF5JavaException
    {

        // Type detection based on HDF5 datatype
        if (isIntegerType(elementType)) {
            return convertIntegerVLFromHVL(dataPtr, len);
        }
        else if (isDoubleType(elementType)) {
            return convertDoubleVLFromHVL(dataPtr, len);
        }
        else if (isStringType(elementType)) {
            return convertStringVLFromHVL(dataPtr, len);
        }
        else if (isVLType(elementType)) {
            // Recursively nested VL - handle with care
            long baseType = getVLBaseType(elementType);
            try {
                return convertNestedVLFromHVL(dataPtr, len, baseType);
            }
            finally {
                // CRITICAL: Close the base type to prevent memory leaks
                try {
                    H5.H5Tclose(baseType);
                }
                catch (Exception e) {
                    // Log but don't fail - we've already done the main work
                }
            }
        }
        else {
            throw new HDF5JavaException("Unsupported nested HDF5 datatype for VL conversion: " + elementType);
        }
    }

    // Helper methods for HDF5 datatype detection
    private static boolean isIntegerType(long datatype)
    {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER;
        }
        catch (Exception e) {
            return false;
        }
    }

    private static boolean isDoubleType(long datatype)
    {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_FLOAT;
        }
        catch (Exception e) {
            return false;
        }
    }

    private static boolean isStringType(long datatype)
    {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_STRING;
        }
        catch (Exception e) {
            return false;
        }
    }

    private static boolean isVLType(long datatype)
    {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_VLEN;
        }
        catch (Exception e) {
            return false;
        }
    }

    private static boolean isReferenceType(long datatype)
    {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_REFERENCE;
        }
        catch (Exception e) {
            return false;
        }
    }

    private static boolean isVLOfStrings(long datatype)
    {
        try {
            // Check if this is a VL type
            if (H5.H5Tget_class(datatype) != HDF5Constants.H5T_VLEN) {
                return false;
            }
            // Get the base type
            long baseType = H5.H5Tget_super(datatype);
            // Check if base type is a string
            boolean isVLOfString = H5.H5Tget_class(baseType) == HDF5Constants.H5T_STRING;
            H5.H5Tclose(baseType);
            return isVLOfString;
        }
        catch (Exception e) {
            return false;
        }
    }

    private static long getVLBaseType(long vlDatatype) throws HDF5JavaException
    {
        try {
            return H5.H5Tget_super(vlDatatype);
        }
        catch (Exception e) {
            throw new HDF5JavaException("Failed to get VL base type: " + e.getMessage());
        }
    }

    /**
     * Convert ArrayList[] of strings to variable-length string array for HDF5
     * Variable-length strings use string pointer arrays, not hvl_t structures
     *
     * @param javaData Array of ArrayLists containing string data
     * @param arena Arena for memory allocation
     * @return MemorySegment containing string pointer array
     * @throws HDF5JavaException if conversion fails
     */
    public static MemorySegment convertVLStrings(ArrayList[] javaData, Arena arena) throws HDF5JavaException
    {
        if (javaData == null || javaData.length == 0) {
            throw new HDF5JavaException("Input data array is null or empty");
        }

        // Allocate array of string pointers using Arena (this part is OK)
        MemorySegment stringArray = arena.allocate(ValueLayout.ADDRESS, javaData.length);

        for (int i = 0; i < javaData.length; i++) {
            if (javaData[i] == null || javaData[i].size() == 0) {
                // Set null pointer for empty/null strings
                stringArray.setAtIndex(ValueLayout.ADDRESS, i, MemorySegment.NULL);
            }
            else {
                // Get the first string from the ArrayList (VL string format)
                String str = (String)javaData[i].get(0);
                if (str == null) {
                    stringArray.setAtIndex(ValueLayout.ADDRESS, i, MemorySegment.NULL);
                }
                else {
                    // CRITICAL FIX: Use HDF5's memory allocator instead of Arena
                    // This prevents conflicts between HDF5's VL memory cleanup and Java's Arena
                    byte[] strBytes = str.getBytes(java.nio.charset.StandardCharsets.UTF_8);
                    // Allocate with extra byte for null terminator
                    MemorySegment hdf5StringMem =
                        org.hdfgroup.javahdf5.hdf5_h.H5allocate_memory(strBytes.length + 1, false);
                    if (hdf5StringMem == null || hdf5StringMem.equals(MemorySegment.NULL)) {
                        throw new HDF5JavaException("Failed to allocate HDF5 memory for string: " + str);
                    }
                    // Copy string bytes with proper scope management
                    MemorySegment boundedMem =
                        hdf5StringMem.reinterpret(strBytes.length + 1, Arena.global(), null);
                    boundedMem.copyFrom(MemorySegment.ofArray(strBytes));
                    // Add null terminator
                    boundedMem.set(ValueLayout.JAVA_BYTE, strBytes.length, (byte)0);
                    stringArray.setAtIndex(ValueLayout.ADDRESS, i, boundedMem);
                }
            }
        }

        return stringArray;
    }

    /**
     * Read variable-length strings from HDF5 dataset
     * Variable-length strings are read as string pointer arrays, not hvl_t structures
     *
     * @param dataset_id HDF5 dataset identifier
     * @param mem_type_id Memory datatype identifier
     * @param mem_space_id Memory dataspace identifier
     * @param file_space_id File dataspace identifier
     * @param xfer_plist_id Transfer property list identifier
     * @param arrayLength Number of strings to read
     * @param arena Arena for memory allocation
     * @return ArrayList array containing the read strings
     * @throws HDF5JavaException if reading fails
     */
    public static ArrayList[] readVLStrings(long dataset_id, long mem_type_id, long mem_space_id,
                                            long file_space_id, long xfer_plist_id, int arrayLength,
                                            Arena arena) throws HDF5JavaException
    {
        // Allocate array of string pointers for reading
        MemorySegment stringArray = arena.allocate(ValueLayout.ADDRESS, arrayLength);

        try {
            // Call native H5Dread to read string pointers
            int status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                              file_space_id, xfer_plist_id, stringArray);
            if (status < 0) {
                throw new HDF5JavaException("H5Dread failed for VL strings");
            }

            // Convert string pointers to ArrayList array
            ArrayList[] result = new ArrayList[arrayLength];
            for (int i = 0; i < arrayLength; i++) {
                result[i] = new ArrayList<String>();

                MemorySegment stringPtr = stringArray.getAtIndex(ValueLayout.ADDRESS, i);
                if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                    try {
                        // Read null-terminated string from pointer with bounds checking
                        String str = stringPtr.getString(0, java.nio.charset.StandardCharsets.UTF_8);
                        result[i].add(str);
                    }
                    catch (Exception e) {
                        // If string reading fails, add empty string
                        result[i].add("");
                    }
                }
                else {
                    // Empty string case
                    result[i].add("");
                }
            }

            return result;
        }
        catch (Exception e) {
            throw new HDF5JavaException("Failed to read VL strings: " + e.getMessage());
        }
    }

    /**
     * Read variable-length strings from HDF5 attribute
     * Attributes and datasets may handle VL strings slightly differently
     *
     * @param attr_id HDF5 attribute identifier
     * @param mem_type_id Memory datatype identifier
     * @param arrayLength Number of strings to read
     * @param arena Arena for memory allocation
     * @return ArrayList array containing the read strings
     * @throws HDF5JavaException if reading fails
     */
    public static ArrayList[] readVLStringsFromAttribute(long attr_id, long mem_type_id, int arrayLength,
                                                         Arena arena) throws HDF5JavaException
    {
        // For attributes, allocate array of string pointers for reading
        MemorySegment stringArray = arena.allocate(ValueLayout.ADDRESS, arrayLength);

        try {
            // Call native H5Aread to read string pointers
            int status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, stringArray);
            if (status < 0) {
                throw new HDF5JavaException("H5Aread failed for VL strings");
            }

            // Convert string pointers to ArrayList array
            ArrayList[] result = new ArrayList[arrayLength];
            for (int i = 0; i < arrayLength; i++) {
                result[i] = new ArrayList<String>();

                MemorySegment stringPtr = stringArray.getAtIndex(ValueLayout.ADDRESS, i);
                if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                    try {
                        // Read null-terminated string from pointer with bounds checking
                        String str = stringPtr.getString(0, java.nio.charset.StandardCharsets.UTF_8);
                        result[i].add(str);
                    }
                    catch (Exception e) {
                        // If string reading fails, add empty string
                        result[i].add("");
                    }
                }
                else {
                    // Empty string case
                    result[i].add("");
                }
            }

            return result;
        }
        catch (Exception e) {
            throw new HDF5JavaException("Failed to read VL strings from attribute: " + e.getMessage());
        }
    }

    /**
     * Convert ArrayList array with heterogeneous types to compound datatype buffer
     * Used for H5T_COMPOUND datatypes where each ArrayList contains mixed field types
     *
     * @param data Array of ArrayLists containing compound field data
     * @param mem_type_id HDF5 compound datatype identifier
     * @param arena Arena for memory allocation
     * @return MemorySegment containing packed compound structures
     * @throws HDF5JavaException if conversion fails
     */
    public static MemorySegment convertCompoundDatatype(ArrayList[] data, long mem_type_id, Arena arena)
        throws HDF5JavaException
    {
        try {
            // Get compound type information
            int nmembers = org.hdfgroup.javahdf5.hdf5_h.H5Tget_nmembers(mem_type_id);
            if (nmembers < 0) {
                throw new HDF5JavaException("Failed to get number of compound members");
            }

            // Get total compound structure size
            long compoundSize = org.hdfgroup.javahdf5.hdf5_h.H5Tget_size(mem_type_id);
            if (compoundSize < 0) {
                throw new HDF5JavaException("Failed to get compound size");
            }

            // Allocate buffer for all compound structures
            MemorySegment buffer = arena.allocate(compoundSize * data.length);

            // Get member information for each field
            long[] memberTypeIds  = new long[nmembers];
            int[] memberClasses   = new int[nmembers];
            long[] memberOffsets  = new long[nmembers];
            long[] memberSizes    = new long[nmembers];
            boolean[] isVLStrings = new boolean[nmembers];

            for (int i = 0; i < nmembers; i++) {
                memberTypeIds[i] = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_type(mem_type_id, i);
                memberClasses[i] = org.hdfgroup.javahdf5.hdf5_h.H5Tget_class(memberTypeIds[i]);
                memberOffsets[i] = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_offset(mem_type_id, i);
                memberSizes[i]   = org.hdfgroup.javahdf5.hdf5_h.H5Tget_size(memberTypeIds[i]);
                isVLStrings[i]   = (memberClasses[i] == HDF5Constants.H5T_STRING &&
                                  org.hdfgroup.javahdf5.hdf5_h.H5Tis_variable_str(memberTypeIds[i]) > 0);
            }

            try {
                // Pack each ArrayList into the compound buffer
                for (int structIdx = 0; structIdx < data.length; structIdx++) {
                    ArrayList<?> record = data[structIdx];

                    if (record == null || record.size() != nmembers) {
                        throw new HDF5JavaException("ArrayList at index " + structIdx + " has " +
                                                    (record == null ? "null" : record.size()) +
                                                    " elements, expected " + nmembers);
                    }

                    long structOffset = structIdx * compoundSize;

                    // Pack each field into the compound structure
                    for (int fieldIdx = 0; fieldIdx < nmembers; fieldIdx++) {
                        Object fieldValue  = record.get(fieldIdx);
                        long fieldOffset   = structOffset + memberOffsets[fieldIdx];
                        int memberClass    = memberClasses[fieldIdx];
                        long memberSize    = memberSizes[fieldIdx];
                        boolean isVLString = isVLStrings[fieldIdx];

                        if (memberClass == HDF5Constants.H5T_INTEGER) {
                            // Integer field - write bytes for unaligned HDF5 compound offsets
                            int intValue = (fieldValue instanceof Integer) ? (Integer)fieldValue : 0;
                            buffer.set(ValueLayout.JAVA_BYTE, fieldOffset, (byte)(intValue & 0xFF));
                            buffer.set(ValueLayout.JAVA_BYTE, fieldOffset + 1,
                                       (byte)((intValue >> 8) & 0xFF));
                            buffer.set(ValueLayout.JAVA_BYTE, fieldOffset + 2,
                                       (byte)((intValue >> 16) & 0xFF));
                            buffer.set(ValueLayout.JAVA_BYTE, fieldOffset + 3,
                                       (byte)((intValue >> 24) & 0xFF));
                        }
                        else if (memberClass == HDF5Constants.H5T_FLOAT) {
                            // Double field - write bytes for unaligned HDF5 compound offsets
                            double doubleValue = (fieldValue instanceof Double) ? (Double)fieldValue : 0.0;
                            long longBits      = Double.doubleToRawLongBits(doubleValue);
                            for (int byteIdx = 0; byteIdx < 8; byteIdx++) {
                                buffer.set(ValueLayout.JAVA_BYTE, fieldOffset + byteIdx,
                                           (byte)((longBits >> (byteIdx * 8)) & 0xFF));
                            }
                        }
                        else if (memberClass == HDF5Constants.H5T_STRING) {
                            if (isVLString) {
                                // Variable-length string - store as pointer
                                String strValue = (fieldValue instanceof String) ? (String)fieldValue : "";
                                byte[] strBytes = strValue.getBytes(StandardCharsets.UTF_8);

                                // Allocate with HDF5's memory allocator for VL strings
                                MemorySegment hdf5StringMem = org.hdfgroup.javahdf5.hdf5_h.H5allocate_memory(
                                    strBytes.length + 1, false);
                                if (hdf5StringMem == null || hdf5StringMem.equals(MemorySegment.NULL)) {
                                    throw new HDF5JavaException(
                                        "Failed to allocate HDF5 memory for string: " + strValue);
                                }

                                MemorySegment boundedMem =
                                    hdf5StringMem.reinterpret(strBytes.length + 1, Arena.global(), null);
                                boundedMem.copyFrom(MemorySegment.ofArray(strBytes));
                                boundedMem.set(ValueLayout.JAVA_BYTE, strBytes.length, (byte)0);

                                // Store pointer
                                buffer.set(ValueLayout.ADDRESS, fieldOffset, boundedMem);
                            }
                            else {
                                // Fixed-length string - copy bytes directly
                                String strValue = (fieldValue instanceof String) ? (String)fieldValue : "";
                                byte[] strBytes = strValue.getBytes(StandardCharsets.UTF_8);
                                int copyLen     = (int)Math.min(strBytes.length, memberSize);

                                // Copy string bytes
                                for (int j = 0; j < copyLen; j++) {
                                    buffer.set(ValueLayout.JAVA_BYTE, fieldOffset + j, strBytes[j]);
                                }
                                // Pad with zeros
                                for (int j = copyLen; j < memberSize; j++) {
                                    buffer.set(ValueLayout.JAVA_BYTE, fieldOffset + j, (byte)0);
                                }
                            }
                        }
                        else {
                            throw new HDF5JavaException("Unsupported compound member type class: " +
                                                        memberClass);
                        }
                    }
                }
            }
            finally {
                // Close member type IDs
                for (int i = 0; i < nmembers; i++) {
                    if (memberTypeIds[i] >= 0) {
                        try {
                            org.hdfgroup.javahdf5.hdf5_h.H5Tclose(memberTypeIds[i]);
                        }
                        catch (Exception e) {
                            // Ignore close errors
                        }
                    }
                }
            }

            return buffer;
        }
        catch (Exception e) {
            if (e instanceof HDF5JavaException) {
                throw e;
            }
            throw new HDF5JavaException("Compound datatype conversion failed: " + e.getMessage());
        }
    }

    /**
     * Read compound datatype data from HDF5 attribute or dataset
     * Used for H5T_COMPOUND datatypes where each ArrayList contains mixed field types
     *
     * @param attr_or_dataset_id HDF5 attribute or dataset identifier
     * @param mem_type_id HDF5 compound datatype identifier
     * @param count Number of compound structures to read
     * @param arena Arena for memory allocation
     * @param isDataset true if reading from dataset, false if reading from attribute
     * @param mem_space_id Memory dataspace (for datasets only)
     * @param file_space_id File dataspace (for datasets only)
     * @param xfer_plist_id Transfer property list (for datasets only)
     * @return ArrayList array containing compound field data
     * @throws HDF5JavaException if reading fails
     */
    public static ArrayList[] readCompoundDatatype(long attr_or_dataset_id, long mem_type_id, int count,
                                                   Arena arena, boolean isDataset, long mem_space_id,
                                                   long file_space_id, long xfer_plist_id)
        throws HDF5JavaException
    {
        try {
            // Get compound type information
            int nmembers = org.hdfgroup.javahdf5.hdf5_h.H5Tget_nmembers(mem_type_id);
            if (nmembers < 0) {
                throw new HDF5JavaException("Failed to get number of compound members");
            }

            // Get total compound structure size
            long compoundSize = org.hdfgroup.javahdf5.hdf5_h.H5Tget_size(mem_type_id);
            if (compoundSize < 0) {
                throw new HDF5JavaException("Failed to get compound size");
            }

            // Get member information for each field
            long[] memberTypeIds  = new long[nmembers];
            int[] memberClasses   = new int[nmembers];
            long[] memberOffsets  = new long[nmembers];
            long[] memberSizes    = new long[nmembers];
            boolean[] isVLStrings = new boolean[nmembers];

            for (int i = 0; i < nmembers; i++) {
                memberTypeIds[i] = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_type(mem_type_id, i);
                memberClasses[i] = org.hdfgroup.javahdf5.hdf5_h.H5Tget_class(memberTypeIds[i]);
                memberOffsets[i] = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_offset(mem_type_id, i);
                memberSizes[i]   = org.hdfgroup.javahdf5.hdf5_h.H5Tget_size(memberTypeIds[i]);
                isVLStrings[i]   = (memberClasses[i] == HDF5Constants.H5T_STRING &&
                                  org.hdfgroup.javahdf5.hdf5_h.H5Tis_variable_str(memberTypeIds[i]) > 0);
            }

            // Allocate buffer for all compound structures
            MemorySegment buffer = arena.allocate(compoundSize * count);

            // Read data from HDF5
            int status;
            if (isDataset) {
                status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(attr_or_dataset_id, mem_type_id, mem_space_id,
                                                              file_space_id, xfer_plist_id, buffer);
            }
            else {
                status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_or_dataset_id, mem_type_id, buffer);
            }

            if (status < 0) {
                throw new HDF5JavaException("H5 read failed with status: " + status);
            }

            // Parse buffer into ArrayList array
            ArrayList[] result = new ArrayList[count];

            try {
                for (int structIdx = 0; structIdx < count; structIdx++) {
                    ArrayList<Object> record = new ArrayList<>();
                    long structOffset        = structIdx * compoundSize;

                    // Read each field from the compound structure
                    for (int fieldIdx = 0; fieldIdx < nmembers; fieldIdx++) {
                        long fieldOffset   = structOffset + memberOffsets[fieldIdx];
                        int memberClass    = memberClasses[fieldIdx];
                        long memberSize    = memberSizes[fieldIdx];
                        boolean isVLString = isVLStrings[fieldIdx];

                        if (memberClass == HDF5Constants.H5T_INTEGER) {
                            // Read integer field (little-endian byte order)
                            int intValue =
                                (buffer.get(ValueLayout.JAVA_BYTE, fieldOffset) & 0xFF) |
                                ((buffer.get(ValueLayout.JAVA_BYTE, fieldOffset + 1) & 0xFF) << 8) |
                                ((buffer.get(ValueLayout.JAVA_BYTE, fieldOffset + 2) & 0xFF) << 16) |
                                (buffer.get(ValueLayout.JAVA_BYTE, fieldOffset + 3) << 24);
                            record.add(intValue);
                        }
                        else if (memberClass == HDF5Constants.H5T_FLOAT) {
                            // Read double field (8 bytes, little-endian)
                            long longBits = 0;
                            for (int byteIdx = 0; byteIdx < 8; byteIdx++) {
                                long byteVal =
                                    buffer.get(ValueLayout.JAVA_BYTE, fieldOffset + byteIdx) & 0xFFL;
                                longBits |= (byteVal << (byteIdx * 8));
                            }
                            double doubleValue = Double.longBitsToDouble(longBits);
                            record.add(doubleValue);
                        }
                        else if (memberClass == HDF5Constants.H5T_STRING) {
                            if (isVLString) {
                                // Variable-length string - read pointer
                                MemorySegment stringPtr = buffer.get(ValueLayout.ADDRESS, fieldOffset);
                                if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                                    try {
                                        String str = stringPtr.getString(0, StandardCharsets.UTF_8);
                                        record.add(str);
                                    }
                                    catch (Exception e) {
                                        record.add("");
                                    }
                                }
                                else {
                                    record.add("");
                                }
                            }
                            else {
                                // Fixed-length string - read bytes
                                byte[] strBytes = new byte[(int)memberSize];
                                for (int j = 0; j < memberSize; j++) {
                                    strBytes[j] = buffer.get(ValueLayout.JAVA_BYTE, fieldOffset + j);
                                }
                                // Convert to string and trim null terminators
                                String strValue = new String(strBytes, StandardCharsets.UTF_8);
                                int nullIdx     = strValue.indexOf('\0');
                                if (nullIdx >= 0) {
                                    strValue = strValue.substring(0, nullIdx);
                                }
                                record.add(strValue);
                            }
                        }
                        else {
                            throw new HDF5JavaException("Unsupported compound member type class for read: " +
                                                        memberClass);
                        }
                    }

                    result[structIdx] = record;
                }
            }
            finally {
                // Close member type IDs
                for (int i = 0; i < nmembers; i++) {
                    if (memberTypeIds[i] >= 0) {
                        try {
                            org.hdfgroup.javahdf5.hdf5_h.H5Tclose(memberTypeIds[i]);
                        }
                        catch (Exception e) {
                            // Ignore close errors
                        }
                    }
                }
            }

            return result;
        }
        catch (Exception e) {
            if (e instanceof HDF5JavaException) {
                throw e;
            }
            throw new HDF5JavaException("Compound datatype read failed: " + e.getMessage());
        }
    }
}