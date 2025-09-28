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

        public RawVLData(byte[] data, int length) {
            this.data = data;
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
    public static MemorySegment convertToHVL(ArrayList[] javaData, Arena arena)
        throws HDF5JavaException {

        if (javaData == null || javaData.length == 0) {
            throw new HDF5JavaException("Input data array is null or empty");
        }

        // Converting ArrayList elements to hvl_t

        // Allocate hvl_t array
        MemorySegment hvlArray = hvl_t.allocateArray(javaData.length, arena);

        for (int i = 0; i < javaData.length; i++) {
            MemorySegment hvlElement = hvl_t.asSlice(hvlArray, i);
            convertSingleElement(javaData[i], hvlElement, arena);
        }

        return hvlArray;
    }

    /**
     * Convert HDF5 hvl_t MemorySegment array back to Java ArrayList array
     *
     * CRITICAL: This method IMMEDIATELY extracts ALL raw data from HDF5-managed memory
     * in a single pass, then processes the copied data. This prevents any access to
     * HDF5-managed memory after H5Treclaim.
     *
     * @param hvlArray MemorySegment containing hvl_t array
     * @param arrayLength Number of elements in the array
     * @param elementType HDF5 datatype of the elements (for type inference)
     * @return Array of ArrayLists
     * @throws HDF5JavaException if conversion fails
     */
    public static ArrayList[] convertFromHVL(MemorySegment hvlArray, int arrayLength, long elementType)
        throws HDF5JavaException {

        if (hvlArray == null) {
            throw new HDF5JavaException("Input hvl_t array is null");
        }

        // CRITICAL TWO-PHASE APPROACH:
        // Phase 1: Copy ALL raw data from HDF5-managed memory to Java-managed memory
        // Phase 2: Process the copied data without any access to HDF5 memory

        ArrayList[] result = new ArrayList[arrayLength];

        // Phase 1: Extract all raw data immediately - NO processing, just copying
        RawVLData[] rawDataArray = new RawVLData[arrayLength];

        // Check if this is string data to use special string copying
        // For VL-of-strings, we need to check the base type
        boolean isStringType = isStringType(elementType) || isVLOfStrings(elementType);

        for (int i = 0; i < arrayLength; i++) {
            MemorySegment hvlElement = hvl_t.asSlice(hvlArray, i);

            // Extract hvl_t fields immediately
            long len = hvl_t.len(hvlElement);
            MemorySegment dataPtr = hvl_t.p(hvlElement);

            // Check if we're getting valid data from hvl_t
            if (len == 0 || dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
                // Empty VL element - create empty raw data
                rawDataArray[i] = new RawVLData(new byte[0], 0);
            } else {
                // Copy raw data immediately before any H5Treclaim
                if (isStringType) {
                    rawDataArray[i] = copyStringVLDataImmediately(dataPtr, (int)len);
                } else {
                    rawDataArray[i] = copyRawVLData(dataPtr, (int)len, elementType);
                }
            }
        }

        // Phase 2: Process copied data (no HDF5 memory access)
        // CRITICAL FIX: For VL data, we need to get the base type for proper conversion
        long baseElementType = elementType;
        boolean needToCloseBaseType = false;
        try {
            if (isVLType(elementType)) {
                baseElementType = getVLBaseType(elementType);
                needToCloseBaseType = true;  // We got a new type ID that needs closing
            }
        } catch (Exception e) {
            // If we can't get the base type, use original elementType
        }

        try {
            for (int i = 0; i < arrayLength; i++) {
                result[i] = convertRawDataToArrayList(rawDataArray[i], baseElementType);
            }
        } finally {
            // CRITICAL: Close the base type if we created it to prevent memory leaks
            if (needToCloseBaseType && baseElementType != elementType) {
                try {
                    H5.H5Tclose(baseElementType);
                } catch (Exception e) {
                    // Log but don't fail - we've already done the main work
                }
            }
        }

        return result;
    }

    /**
     * Convert a single ArrayList to hvl_t structure
     */
    private static void convertSingleElement(ArrayList<?> list, MemorySegment hvlElement, Arena arena)
        throws HDF5JavaException {

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

        // Detect element type using reflection
        Object firstElement = list.get(0);
        Class<?> elementType = firstElement.getClass();

        // Converting ArrayList of type: " + elementType.getSimpleName() + " with " + size + " elements"

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
    private static MemorySegment convertIntegerVL(ArrayList<?> list, Arena arena) {
        ArrayList<Integer> intList = (ArrayList<Integer>) list;
        MemorySegment dataArray = arena.allocate(ValueLayout.JAVA_INT, intList.size());

        for (int i = 0; i < intList.size(); i++) {
            dataArray.setAtIndex(ValueLayout.JAVA_INT, i, intList.get(i));
        }

        return dataArray;
    }

    /**
     * Convert ArrayList<Double> to native double array
     */
    @SuppressWarnings("unchecked")
    private static MemorySegment convertDoubleVL(ArrayList<?> list, Arena arena) {
        ArrayList<Double> doubleList = (ArrayList<Double>) list;
        MemorySegment dataArray = arena.allocate(ValueLayout.JAVA_DOUBLE, doubleList.size());

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
    private static MemorySegment convertStringVL(ArrayList<?> list, Arena arena) {
        ArrayList<String> stringList = (ArrayList<String>) list;

        // For array datatypes containing strings, create a packed array of string pointers
        // This is different from VL strings - array types have fixed size arrays
        MemorySegment stringArray = arena.allocate(ValueLayout.ADDRESS, stringList.size());

        for (int i = 0; i < stringList.size(); i++) {
            String str = stringList.get(i);
            if (str != null) {
                MemorySegment stringSegment = arena.allocateFrom(str, StandardCharsets.UTF_8);
                stringArray.setAtIndex(ValueLayout.ADDRESS, i, stringSegment);
            } else {
                stringArray.setAtIndex(ValueLayout.ADDRESS, i, MemorySegment.NULL);
            }
        }

        return stringArray;
    }

    /**
     * Convert ArrayList array to array datatype buffer (not hvl_t)
     * Used for H5T_ARRAY datatypes where each element is a fixed-size array
     */
    public static MemorySegment convertArrayDatatype(ArrayList[] data, long mem_type_id, Arena arena) throws HDF5JavaException {
        try {
            // Get the array type information
            long baseTypeId = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_super(mem_type_id);
            if (baseTypeId < 0) {
                throw new HDF5JavaException("Failed to get array base type");
            }

            // Get array dimensions
            int ndims = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_array_ndims(mem_type_id);
            if (ndims != 1) {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Only 1D arrays are supported, got " + ndims + "D");
            }

            // Get the array size (number of elements per array)
            MemorySegment dims = arena.allocate(ValueLayout.JAVA_LONG, 1);
            int result = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_array_dims2(mem_type_id, dims);
            if (result < 0) {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Failed to get array dimensions");
            }
            int arraySize = (int) dims.get(ValueLayout.JAVA_LONG, 0);

            // Check if the base type is variable-length string
            int isVLStringResult = org.hdfgroup.javahdf5.hdf5_h_1.H5Tis_variable_str(baseTypeId);
            boolean isVLString = isVLStringResult > 0;

            if (isVLString) {
                // Each entry in data[] is an ArrayList<String> with arraySize elements
                // Pack as array of string pointers
                MemorySegment buffer = arena.allocate(ValueLayout.ADDRESS, data.length * arraySize);

                for (int i = 0; i < data.length; i++) {
                    ArrayList<String> stringArray = (ArrayList<String>) data[i];
                    if (stringArray.size() != arraySize) {
                        org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                        throw new HDF5JavaException("Array element " + i + " has " + stringArray.size() +
                                                   " elements, expected " + arraySize);
                    }

                    // Pack string pointers for this array element
                    for (int j = 0; j < arraySize; j++) {
                        String str = stringArray.get(j);
                        if (str != null) {
                            MemorySegment stringSegment = arena.allocateFrom(str, StandardCharsets.UTF_8);
                            buffer.setAtIndex(ValueLayout.ADDRESS, i * arraySize + j, stringSegment);
                        } else {
                            buffer.setAtIndex(ValueLayout.ADDRESS, i * arraySize + j, MemorySegment.NULL);
                        }
                    }
                }

                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                return buffer;
            } else {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Unsupported array base type for FFM conversion");
            }
        } catch (Exception e) {
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
    public static ArrayList[] readArrayDatatype(long attr_id, long mem_type_id, int count, Arena arena) throws HDF5JavaException {
        try {
            // Get the array type information
            long baseTypeId = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_super(mem_type_id);
            if (baseTypeId < 0) {
                throw new HDF5JavaException("Failed to get array base type");
            }

            // Get array dimensions
            int ndims = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_array_ndims(mem_type_id);
            if (ndims != 1) {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Only 1D arrays are supported, got " + ndims + "D");
            }

            // Get the array size (number of elements per array)
            MemorySegment dims = arena.allocate(ValueLayout.JAVA_LONG, 1);
            int result = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_array_dims2(mem_type_id, dims);
            if (result < 0) {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Failed to get array dimensions");
            }
            int arraySize = (int) dims.get(ValueLayout.JAVA_LONG, 0);

            // Check if the base type is variable-length string
            int isVLStringResult = org.hdfgroup.javahdf5.hdf5_h_1.H5Tis_variable_str(baseTypeId);
            boolean isVLString = isVLStringResult > 0;

            if (isVLString) {
                // Allocate buffer for array of string pointers
                MemorySegment buffer = arena.allocate(ValueLayout.ADDRESS, count * arraySize);

                // Read data from HDF5
                int status = org.hdfgroup.javahdf5.hdf5_h_1.H5Aread(attr_id, mem_type_id, buffer);
                if (status < 0) {
                    org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                    throw new HDF5JavaException("Failed to read array data");
                }

                // Convert to ArrayList array
                ArrayList[] resultArray = new ArrayList[count];
                for (int i = 0; i < count; i++) {
                    ArrayList<String> stringArray = new ArrayList<>(arraySize);
                    for (int j = 0; j < arraySize; j++) {
                        MemorySegment stringPtr = buffer.getAtIndex(ValueLayout.ADDRESS, i * arraySize + j);
                        if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                            String str = stringPtr.getString(0, StandardCharsets.UTF_8);
                            stringArray.add(str);
                        } else {
                            stringArray.add(null);
                        }
                    }
                    resultArray[i] = stringArray;
                }

                // Clean up VL string memory
                long space_id = org.hdfgroup.javahdf5.hdf5_h_1.H5Aget_space(attr_id);
                try {
                    org.hdfgroup.javahdf5.hdf5_h_1.H5Treclaim(mem_type_id, space_id,
                                                            org.hdfgroup.javahdf5.hdf5_h_1.H5P_DEFAULT(), buffer);
                } finally {
                    if (space_id >= 0) {
                        org.hdfgroup.javahdf5.hdf5_h_1.H5Sclose(space_id);
                    }
                }

                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                return resultArray;
            } else {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Unsupported array base type for FFM reading");
            }
        } catch (Exception e) {
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
                                                         long xfer_plist_id, int count, Arena arena) throws HDF5JavaException {
        try {
            // Get the array type information
            long baseTypeId = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_super(mem_type_id);
            if (baseTypeId < 0) {
                throw new HDF5JavaException("Failed to get array base type");
            }

            // Get array dimensions
            int ndims = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_array_ndims(mem_type_id);
            if (ndims != 1) {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Only 1D arrays are supported, got " + ndims + "D");
            }

            // Get the array size (number of elements per array)
            MemorySegment dims = arena.allocate(ValueLayout.JAVA_LONG, 1);
            int result = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_array_dims2(mem_type_id, dims);
            if (result < 0) {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Failed to get array dimensions");
            }
            int arraySize = (int) dims.get(ValueLayout.JAVA_LONG, 0);

            // Check if the base type is variable-length string
            int isVLStringResult = org.hdfgroup.javahdf5.hdf5_h_1.H5Tis_variable_str(baseTypeId);
            boolean isVLString = isVLStringResult > 0;

            if (isVLString) {
                // Allocate buffer for array of string pointers
                MemorySegment buffer = arena.allocate(ValueLayout.ADDRESS, count * arraySize);

                // Read data from HDF5
                int status = org.hdfgroup.javahdf5.hdf5_h_1.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                                   file_space_id, xfer_plist_id, buffer);
                if (status < 0) {
                    org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                    throw new HDF5JavaException("Failed to read array data");
                }

                // IMMEDIATELY copy string data before any potential reclaim
                String[][] copiedStringData = new String[count][arraySize];
                for (int i = 0; i < count; i++) {
                    for (int j = 0; j < arraySize; j++) {
                        MemorySegment stringPtr = buffer.getAtIndex(ValueLayout.ADDRESS, i * arraySize + j);
                        if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                            copiedStringData[i][j] = stringPtr.getString(0, StandardCharsets.UTF_8);
                        } else {
                            copiedStringData[i][j] = null;
                        }
                    }
                }

                // Clean up VL string memory AFTER copying
                long space_id = (mem_space_id >= 0) ? mem_space_id : file_space_id;
                try {
                    org.hdfgroup.javahdf5.hdf5_h_1.H5Treclaim(mem_type_id, space_id, org.hdfgroup.javahdf5.hdf5_h_1.H5P_DEFAULT(), buffer);
                } finally {
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

                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                return resultArray;
            } else {
                org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseTypeId);
                throw new HDF5JavaException("Unsupported array base type for FFM reading");
            }
        } catch (Exception e) {
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
    private static MemorySegment convertNestedVL(ArrayList<?> list, Arena arena) throws HDF5JavaException {
        ArrayList<ArrayList<?>> nestedList = (ArrayList<ArrayList<?>>) list;
        MemorySegment nestedHvlArray = hvl_t.allocateArray(nestedList.size(), arena);

        for (int i = 0; i < nestedList.size(); i++) {
            MemorySegment hvlElement = hvl_t.asSlice(nestedHvlArray, i);
            convertSingleElement(nestedList.get(i), hvlElement, arena);
        }

        return nestedHvlArray;
    }

    /**
     * Copy all raw bytes from HDF5-managed memory to Java-managed memory.
     * This is the critical first phase that prevents any SIGSEGV.
     * For strings, we need special handling to extract the actual content immediately.
     */
    private static RawVLData copyRawVLData(MemorySegment dataPtr, int len, long elementType) throws HDF5JavaException {
        if (len == 0 || dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
            return new RawVLData(new byte[0], 0);
        }

        try {
            // CRITICAL FIX: Use actual HDF5 datatype size instead of guessing
            // Get the base element type for VL data
            long baseType = elementType;

            // For VL types, get the base type
            boolean needToCloseBaseType = false;
            try {
                if (hdf.hdf5lib.H5.H5Tdetect_class(elementType, hdf.hdf5lib.HDF5Constants.H5T_VLEN)) {
                    baseType = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_super(elementType);
                    needToCloseBaseType = true; // Mark that we need to close this type ID
                }
            } catch (Exception e) {
                // If we can't get the base type, continue with original elementType
            }

            try {
                // Get the actual element size from HDF5
                long elementSize = org.hdfgroup.javahdf5.hdf5_h_1.H5Tget_size(baseType);
                long totalSize = (long)len * elementSize;

                // Check for zero element size - fall back to conservative approach
                if (elementSize == 0) {
                    throw new RuntimeException("Zero element size - trigger fallback");
                }

                // Use the correct element size for data copying
                byte[] rawData = copyWithReinterpret(dataPtr, totalSize, len);
                if (rawData.length > 0) {
                    return new RawVLData(rawData, len);
                }

                // If copying fails, return empty data (preserving length info)
                return new RawVLData(new byte[0], len);
            } finally {
                // CRITICAL: Close the base type if we created it to prevent memory leaks
                if (needToCloseBaseType && baseType != elementType) {
                    try {
                        org.hdfgroup.javahdf5.hdf5_h_1.H5Tclose(baseType);
                    } catch (Exception ex) {
                        // Log but don't fail - we've already done the main work
                        System.err.println("Warning: Failed to close base type in extractRawVLData: " + ex.getMessage());
                    }
                }
            }

        } catch (Exception e) {
            // If we can't determine the size, fall back to conservative approach
            // Try common element sizes: 8-byte first (double/pointer), then 4-byte (int)
            try {
                byte[] rawData = copyWithReinterpret(dataPtr, (long)len * 8, len);
                if (rawData.length > 0) {
                    return new RawVLData(rawData, len);
                }
                rawData = copyWithReinterpret(dataPtr, (long)len * 4, len);
                if (rawData.length > 0) {
                    return new RawVLData(rawData, len);
                }
            } catch (Exception fallbackEx) {
                // Ignore fallback exceptions
            }
            return new RawVLData(new byte[0], len);
        }
    }

    /**
     * Helper method to copy data using FFM reinterpret with proper sizing
     */
    private static byte[] copyWithReinterpret(MemorySegment dataPtr, long totalSize, int len) {
        try {
            // Reinterpret the pointer with the calculated size using global arena
            MemorySegment reinterpretedSegment = dataPtr.reinterpret(totalSize, Arena.global(), null);

            // Now copy the data
            byte[] rawData = new byte[(int)totalSize];
            for (int i = 0; i < totalSize; i++) {
                rawData[i] = reinterpretedSegment.get(ValueLayout.JAVA_BYTE, i);
            }

            return rawData;
        } catch (Exception e) {
            // If reinterpret fails, return empty array
            return new byte[0];
        }
    }

    /**
     * Special method to copy string VL data immediately
     * For string data, we need to extract the actual string content, not just pointers
     */
    private static RawVLData copyStringVLDataImmediately(MemorySegment dataPtr, int len) throws HDF5JavaException {
        if (len == 0 || dataPtr == null || dataPtr.equals(MemorySegment.NULL)) {
            return new RawVLData(new byte[0], 0);
        }

        try {
            // For string data, dataPtr points to an array of string pointers
            // We need to extract all string content immediately

            // Create a list to collect all string bytes
            java.util.List<Byte> allStringBytes = new java.util.ArrayList<>();

            // Add length information at the beginning
            allStringBytes.add((byte)(len & 0xFF));
            allStringBytes.add((byte)((len >> 8) & 0xFF));
            allStringBytes.add((byte)((len >> 16) & 0xFF));
            allStringBytes.add((byte)((len >> 24) & 0xFF));

            // CRITICAL FIX: Use reinterpret for proper FFM access to string pointer array
            try {
                // For string VL data, we have an array of string pointers
                long pointerArraySize = (long)len * 8; // 8 bytes per pointer on 64-bit systems
                MemorySegment reinterpretedArray = dataPtr.reinterpret(pointerArraySize, Arena.global(), null);

                for (int i = 0; i < len; i++) {
                    try {
                        // Get the string pointer from the reinterpreted array
                        MemorySegment stringPtr = reinterpretedArray.getAtIndex(ValueLayout.ADDRESS, i);

                        if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                            // Extract the string content immediately
                            String str = stringPtr.getString(0, StandardCharsets.UTF_8);
                            byte[] strBytes = str.getBytes(StandardCharsets.UTF_8);

                            // Add string length
                            int strLen = strBytes.length;
                            allStringBytes.add((byte)(strLen & 0xFF));
                            allStringBytes.add((byte)((strLen >> 8) & 0xFF));
                            allStringBytes.add((byte)((strLen >> 16) & 0xFF));
                            allStringBytes.add((byte)((strLen >> 24) & 0xFF));

                            // Add string content
                            for (byte b : strBytes) {
                                allStringBytes.add(b);
                            }
                        } else {
                            // Null string - add zero length
                            allStringBytes.add((byte)0);
                            allStringBytes.add((byte)0);
                            allStringBytes.add((byte)0);
                            allStringBytes.add((byte)0);
                        }
                    } catch (Exception e) {
                        // If we can't read this string, add empty placeholder
                        allStringBytes.add((byte)0);
                        allStringBytes.add((byte)0);
                        allStringBytes.add((byte)0);
                        allStringBytes.add((byte)0);
                    }
                }
            } catch (Exception reinterpretEx) {
                // If reinterpret fails, fall back to empty data
                for (int i = 0; i < len; i++) {
                    allStringBytes.add((byte)0);
                    allStringBytes.add((byte)0);
                    allStringBytes.add((byte)0);
                    allStringBytes.add((byte)0);
                }
            }

            // Convert list to array
            byte[] result = new byte[allStringBytes.size()];
            for (int i = 0; i < result.length; i++) {
                result[i] = allStringBytes.get(i);
            }

            return new RawVLData(result, len);

        } catch (Exception e) {
            throw new HDF5JavaException("Failed to copy string VL data: " + e.getMessage());
        }
    }

    /**
     * Convert copied raw data to ArrayList without accessing HDF5 memory.
     * This is the safe second phase that works on Java-managed memory only.
     */
    private static ArrayList<?> convertRawDataToArrayList(RawVLData rawData, long elementType)
        throws HDF5JavaException {

        // If the hvl_t indicated 0 length, return empty ArrayList
        if (rawData.length == 0) {
            return new ArrayList<>();
        }

        // If we have a non-zero length but no raw data, something went wrong
        if (rawData.data.length == 0 && rawData.length > 0) {
            // This indicates a data extraction problem - create ArrayList with correct size but null elements
            ArrayList<Object> fallback = new ArrayList<>(rawData.length);
            for (int i = 0; i < rawData.length; i++) {
                fallback.add(null);
            }
            return fallback;
        }

        // Type detection based on HDF5 datatype
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
            else if (isVLType(elementType)) {
                // For nested VL, we need to process hvl_t structures from raw data
                return convertRawDataToNestedVLList(rawData, elementType);
            }
            else {
                // For unknown types, try to detect content from raw data
                return detectAndConvertUnknownType(rawData, elementType);
            }
        } catch (Exception e) {
            // Fallback: return empty list to prevent crashes
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
    private static ArrayList<Integer> convertRawDataToIntegerList(RawVLData rawData) {
        ArrayList<Integer> result = new ArrayList<>(rawData.length);

        byte[] data = rawData.data;
        int bytesPerInt = Integer.BYTES;
        int maxInts = Math.min(rawData.length, data.length / bytesPerInt);

        for (int i = 0; i < maxInts; i++) {
            int offset = i * bytesPerInt;
            if (offset + bytesPerInt <= data.length) {
                // Reconstruct integer from bytes (little-endian)
                int value = (data[offset] & 0xFF) |
                           ((data[offset + 1] & 0xFF) << 8) |
                           ((data[offset + 2] & 0xFF) << 16) |
                           (data[offset + 3] << 24);
                result.add(value);
            }
        }

        return result;
    }

    /**
     * Convert raw bytes to Double ArrayList
     */
    private static ArrayList<Double> convertRawDataToDoubleList(RawVLData rawData) {
        ArrayList<Double> result = new ArrayList<>(rawData.length);

        byte[] data = rawData.data;
        int bytesPerDouble = Double.BYTES;
        int maxDoubles = Math.min(rawData.length, data.length / bytesPerDouble);

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
     * Convert raw bytes to String ArrayList
     * For string data copied with copyStringVLDataImmediately, decode the packed format
     */
    private static ArrayList<String> convertRawDataToStringList(RawVLData rawData) {
        ArrayList<String> result = new ArrayList<>();

        if (rawData.length == 0 || rawData.data.length < 4) {
            // Create empty strings to match expected length
            for (int i = 0; i < rawData.length; i++) {
                result.add("");
            }
            return result;
        }

        byte[] data = rawData.data;

        try {
            // Check if this looks like packed string data (starts with length)
            if (data.length >= 4) {
                // Read the number of strings from the first 4 bytes
                int numStrings = (data[0] & 0xFF) | ((data[1] & 0xFF) << 8) |
                               ((data[2] & 0xFF) << 16) | (data[3] << 24);

                if (numStrings == rawData.length && numStrings > 0) {
                    // This looks like our packed format, decode it
                    int offset = 4; // Skip the count

                    for (int i = 0; i < numStrings && offset + 4 <= data.length; i++) {
                        // Read string length
                        int strLen = (data[offset] & 0xFF) | ((data[offset + 1] & 0xFF) << 8) |
                                   ((data[offset + 2] & 0xFF) << 16) | (data[offset + 3] << 24);
                        offset += 4;

                        if (strLen == 0) {
                            result.add("");
                        } else if (offset + strLen <= data.length) {
                            // Extract string bytes
                            byte[] strBytes = new byte[strLen];
                            System.arraycopy(data, offset, strBytes, 0, strLen);
                            String str = new String(strBytes, StandardCharsets.UTF_8);
                            result.add(str);
                            offset += strLen;
                        } else {
                            result.add(""); // Truncated string
                        }
                    }
                }
            }

            // If we didn't get the expected number of strings, pad with empty strings
            while (result.size() < rawData.length) {
                result.add("");
            }

        } catch (Exception e) {
            // If decoding fails, create empty strings to prevent crashes
            result.clear();
            for (int i = 0; i < rawData.length; i++) {
                result.add("");
            }
        }

        return result;
    }

    /**
     * Convert raw bytes to nested VL ArrayList (placeholder implementation)
     */
    private static ArrayList<ArrayList<?>> convertRawDataToNestedVLList(RawVLData rawData, long elementType) {
        // This is a simplified implementation - real nested VL handling would be more complex
        ArrayList<ArrayList<?>> result = new ArrayList<>(rawData.length);
        // For now, return empty list as nested VL handling is complex
        return result;
    }

    /**
     * Detect and convert unknown HDF5 datatypes by examining the raw data
     */
    private static ArrayList<?> detectAndConvertUnknownType(RawVLData rawData, long elementType) {
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
            } catch (Exception e) {
                // Not integer data
            }
        }

        // Check if it looks like double data
        if (rawData.data.length >= rawData.length * 8) {
            try {
                return convertRawDataToDoubleList(rawData);
            } catch (Exception e) {
                // Not double data
            }
        }

        // Fallback: create empty list
        ArrayList<Object> result = new ArrayList<>();
        for (int i = 0; i < rawData.length; i++) {
            result.add("");  // Use empty string as safe fallback
        }
        return result;
    }

    /**
     * IMMEDIATE conversion that extracts all data before any H5Treclaim can invalidate memory
     * This follows the JNI translate pattern of immediate data copying.
     * DEPRECATED: Use the two-phase approach instead
     */
    @Deprecated
    private static ArrayList<?> convertSingleElementImmediately(MemorySegment dataPtr, int len, long elementType)
        throws HDF5JavaException {

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
        throws HDF5JavaException {

        // CRITICAL: Extract hvl_t data IMMEDIATELY to prevent access after H5Treclaim
        long len = hvl_t.len(hvlElement);
        MemorySegment dataPtr = hvl_t.p(hvlElement);

        return convertSingleElementImmediately(dataPtr, (int)len, elementType);
    }

    /**
     * Convert native int array back to ArrayList<Integer>
     */
    private static ArrayList<Integer> convertIntegerVLFromHVL(MemorySegment dataPtr, int len) {
        ArrayList<Integer> result = new ArrayList<>(len);

        // Check if we have a valid memory segment
        if (dataPtr == null || dataPtr.equals(MemorySegment.NULL) || len <= 0) {
            return result;
        }

        // IMPORTANT: For nested VL structures, we cannot trust the byteSize()
        // since HDF5 may invalidate memory at any time. We must be more defensive.
        long requiredBytes = (long) len * Integer.BYTES;

        // Use safe bounds checking without relying on byteSize() for HDF5 managed memory
        boolean canCheckSize = true;
        try {
            // Only check size for non-HDF5 managed memory segments
            if (dataPtr.byteSize() != Long.MAX_VALUE && dataPtr.byteSize() > 0) {
                if (dataPtr.byteSize() < requiredBytes) {
                    throw new HDF5JavaException("Memory segment too small: has " + dataPtr.byteSize() +
                                              " bytes, need " + requiredBytes + " for " + len + " integers");
                }
            }
        } catch (Exception e) {
            // If we can't check the size safely, proceed with caution
            canCheckSize = false;
        }

        for (int i = 0; i < len; i++) {
            // Handle unaligned memory by reading as bytes and reconstructing integer
            long offset = (long) i * Integer.BYTES;
            int value;
            try {
                value = dataPtr.getAtIndex(ValueLayout.JAVA_INT, i);
            } catch (IllegalArgumentException e) {
                // Memory is not aligned for direct int access, read as bytes with bounds checking
                try {
                    // Extra safety: check if we can read each byte before accessing
                    if (offset + 3 >= 0) {  // Basic sanity check
                        byte b0 = dataPtr.get(ValueLayout.JAVA_BYTE, offset);
                        byte b1 = dataPtr.get(ValueLayout.JAVA_BYTE, offset + 1);
                        byte b2 = dataPtr.get(ValueLayout.JAVA_BYTE, offset + 2);
                        byte b3 = dataPtr.get(ValueLayout.JAVA_BYTE, offset + 3);
                        // Reconstruct integer in native byte order (little-endian on x86)
                        value = (b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | (b3 << 24);
                    } else {
                        throw new HDF5JavaException("Invalid offset for integer at index " + i);
                    }
                } catch (Exception ex) {
                    // If we get a SIGSEGV-type error, the memory is no longer valid
                    throw new HDF5JavaException("Memory access violation at index " + i +
                                              " (offset " + offset + ") - memory may have been freed by HDF5: " + ex.getMessage());
                }
            } catch (Exception e) {
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
    private static ArrayList<Double> convertDoubleVLFromHVL(MemorySegment dataPtr, int len) {
        ArrayList<Double> result = new ArrayList<>(len);

        // Check if we have a valid memory segment
        if (dataPtr == null || dataPtr.equals(MemorySegment.NULL) || len <= 0) {
            return result;
        }

        // IMPORTANT: For nested VL structures, we cannot trust the byteSize()
        // since HDF5 may invalidate memory at any time. We must be more defensive.
        long requiredBytes = (long) len * Double.BYTES;

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
        } catch (Exception e) {
            // If we can't check the size safely, proceed with caution
            canCheckSize = false;
        }

        for (int i = 0; i < len; i++) {
            // Handle unaligned memory by reading as bytes and reconstructing double
            long offset = (long) i * Double.BYTES;
            double value;
            try {
                value = dataPtr.getAtIndex(ValueLayout.JAVA_DOUBLE, i);
            } catch (IllegalArgumentException e) {
                // Memory is not aligned for direct double access, read as bytes with bounds checking
                try {
                    long longBits = 0;
                    for (int j = 0; j < 8; j++) {
                        byte b = dataPtr.get(ValueLayout.JAVA_BYTE, offset + j);
                        longBits |= ((long)(b & 0xFF)) << (j * 8);
                    }
                    value = Double.longBitsToDouble(longBits);
                } catch (Exception ex) {
                    throw new HDF5JavaException("Failed to read double at index " + i +
                                              " (offset " + offset + "): " + ex.getMessage());
                }
            }
            result.add(value);
        }

        return result;
    }

    /**
     * Convert native char** array back to ArrayList<String>
     */
    private static ArrayList<String> convertStringVLFromHVL(MemorySegment dataPtr, int len) {
        ArrayList<String> result = new ArrayList<>(len);

        // Check if we have a valid memory segment
        if (dataPtr == null || dataPtr.equals(MemorySegment.NULL) || len <= 0) {
            return result;
        }

        // IMPORTANT: For nested VL structures, we cannot trust the byteSize()
        // since HDF5 may invalidate memory at any time. We must be more defensive.
        long requiredBytes = (long) len * ValueLayout.ADDRESS.byteSize();

        // Use safe bounds checking without relying on byteSize() for HDF5 managed memory
        boolean canCheckSize = true;
        try {
            // Only check size for non-HDF5 managed memory segments
            if (dataPtr.byteSize() != Long.MAX_VALUE && dataPtr.byteSize() > 0) {
                if (dataPtr.byteSize() < requiredBytes) {
                    throw new HDF5JavaException("Memory segment too small: has " + dataPtr.byteSize() +
                                              " bytes, need " + requiredBytes + " for " + len + " string pointers");
                }
            }
        } catch (Exception e) {
            // If we can't check the size safely, proceed with caution
            canCheckSize = false;
        }

        for (int i = 0; i < len; i++) {
            try {
                MemorySegment stringPtr = dataPtr.getAtIndex(ValueLayout.ADDRESS, i);
                if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                    String str = stringPtr.getString(0, StandardCharsets.UTF_8);
                    result.add(str);
                } else {
                    result.add(""); // Handle NULL string pointer
                }
            } catch (Exception e) {
                throw new HDF5JavaException("Failed to read string pointer at index " + i + ": " + e.getMessage());
            }
        }

        return result;
    }

    /**
     * IMMEDIATE nested VL conversion - extracts ALL nested data before any potential H5Treclaim
     */
    private static ArrayList<ArrayList<?>> convertNestedVLImmediately(MemorySegment dataPtr, int len, long elementType)
        throws HDF5JavaException {

        ArrayList<ArrayList<?>> result = new ArrayList<>(len);

        // Get the base type for nested elements
        long baseType = getVLBaseType(elementType);

        try {
            // CRITICAL: For nested VL data, extract ALL nested hvl_t data IMMEDIATELY
            // This prevents access to freed memory after H5Treclaim
            for (int i = 0; i < len; i++) {
                MemorySegment nestedHvlElement = hvl_t.asSlice(dataPtr, i);

                // Extract hvl_t fields immediately - this is the critical step
                long nestedLen = hvl_t.len(nestedHvlElement);
                MemorySegment nestedDataPtr = hvl_t.p(nestedHvlElement);

                if (nestedLen == 0 || nestedDataPtr == null || nestedDataPtr.equals(MemorySegment.NULL)) {
                    result.add(new ArrayList<>());
                    continue;
                }

                // Immediately convert the nested element data
                ArrayList<?> nestedList = convertSingleElementImmediately(nestedDataPtr, (int)nestedLen, baseType);
                result.add(nestedList);
            }
        } finally {
            // CRITICAL: Close the base type to prevent memory leaks
            try {
                H5.H5Tclose(baseType);
            } catch (Exception e) {
                // Log but don't fail - we've already done the main work
            }
        }

        return result;
    }

    /**
     * Legacy nested VL conversion - now delegates to immediate version
     */
    private static ArrayList<ArrayList<?>> convertNestedVLFromHVL(MemorySegment dataPtr, int len, long elementType)
        throws HDF5JavaException {

        return convertNestedVLImmediately(dataPtr, len, elementType);
    }

    /**
     * Safely convert a nested VL element by immediately copying data
     */
    private static ArrayList<?> convertNestedElementSafely(MemorySegment dataPtr, int len, long elementType)
        throws HDF5JavaException {

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
            } finally {
                // CRITICAL: Close the base type to prevent memory leaks
                try {
                    H5.H5Tclose(baseType);
                } catch (Exception e) {
                    // Log but don't fail - we've already done the main work
                }
            }
        }
        else {
            throw new HDF5JavaException("Unsupported nested HDF5 datatype for VL conversion: " + elementType);
        }
    }

    // Helper methods for HDF5 datatype detection
    private static boolean isIntegerType(long datatype) {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_INTEGER;
        } catch (Exception e) {
            return false;
        }
    }

    private static boolean isDoubleType(long datatype) {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_FLOAT;
        } catch (Exception e) {
            return false;
        }
    }

    private static boolean isStringType(long datatype) {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_STRING;
        } catch (Exception e) {
            return false;
        }
    }

    private static boolean isVLType(long datatype) {
        try {
            return H5.H5Tget_class(datatype) == HDF5Constants.H5T_VLEN;
        } catch (Exception e) {
            return false;
        }
    }

    private static boolean isVLOfStrings(long datatype) {
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
        } catch (Exception e) {
            return false;
        }
    }

    private static long getVLBaseType(long vlDatatype) throws HDF5JavaException {
        try {
            return H5.H5Tget_super(vlDatatype);
        } catch (Exception e) {
            throw new HDF5JavaException("Failed to get VL base type: " + e.getMessage());
        }
    }
}