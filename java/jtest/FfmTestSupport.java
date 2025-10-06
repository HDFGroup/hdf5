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

package jtest;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.hdf5_h;

/**
 * Support utilities for FFM-only HDF5 tests.
 *
 * This class provides common patterns for working with HDF5 through the FFM API,
 * including error checking, memory management, and data conversion utilities.
 */
public class FfmTestSupport {

    /**
     * Check if an HDF5 return value indicates success.
     *
     * @param retVal The return value from an HDF5 function
     * @return true if the operation succeeded (retVal >= 0), false otherwise
     */
    public static boolean isSuccess(int retVal) { return retVal >= 0; }

    /**
     * Check if an HDF5 return value indicates success.
     *
     * @param retVal The return value from an HDF5 function (long version)
     * @return true if the operation succeeded (retVal >= 0), false otherwise
     */
    public static boolean isSuccess(long retVal) { return retVal >= 0; }

    /**
     * Check if an HDF5 identifier is valid.
     *
     * @param hid The HDF5 identifier to check
     * @return true if the identifier is valid (>= 0), false otherwise
     */
    public static boolean isValidId(long hid) { return hid >= 0; }

    /**
     * Create a MemorySegment from a Java String using the provided Arena.
     * The string will be null-terminated.
     *
     * @param arena The Arena to use for allocation
     * @param str The Java String to convert
     * @return A MemorySegment containing the null-terminated string
     */
    public static MemorySegment stringToSegment(Arena arena, String str)
    {
        if (str == null) {
            return MemorySegment.NULL;
        }
        return arena.allocateFrom(str);
    }

    /**
     * Convert a MemorySegment containing a null-terminated string to a Java String.
     *
     * @param segment The MemorySegment containing the string
     * @return The Java string, or null if segment is NULL
     */
    public static String segmentToString(MemorySegment segment)
    {
        if (segment == null || segment == MemorySegment.NULL) {
            return null;
        }
        return segment.getString(0);
    }

    /**
     * Create a MemorySegment for an integer output parameter.
     *
     * @param arena The Arena to use for allocation
     * @return A MemorySegment that can hold one integer value
     */
    public static MemorySegment allocateInt(Arena arena) { return arena.allocate(ValueLayout.JAVA_INT); }

    /**
     * Create a MemorySegment for a long output parameter.
     *
     * @param arena The Arena to use for allocation
     * @return A MemorySegment that can hold one long value
     */
    public static MemorySegment allocateLong(Arena arena) { return arena.allocate(ValueLayout.JAVA_LONG); }

    /**
     * Create a MemorySegment for an integer array.
     *
     * @param arena The Arena to use for allocation
     * @param length The number of integers in the array
     * @return A MemorySegment that can hold the integer array
     */
    public static MemorySegment allocateIntArray(Arena arena, int length)
    {
        return arena.allocate(ValueLayout.JAVA_INT, length);
    }

    /**
     * Create a MemorySegment for a long array.
     *
     * @param arena The Arena to use for allocation
     * @param length The number of longs in the array
     * @return A MemorySegment that can hold the long array
     */
    public static MemorySegment allocateLongArray(Arena arena, int length)
    {
        return arena.allocate(ValueLayout.JAVA_LONG, length);
    }

    /**
     * Create a MemorySegment for a double array.
     *
     * @param arena The Arena to use for allocation
     * @param length The number of doubles in the array
     * @return A MemorySegment that can hold the double array
     */
    public static MemorySegment allocateDoubleArray(Arena arena, int length)
    {
        return arena.allocate(ValueLayout.JAVA_DOUBLE, length);
    }

    /**
     * Copy data from a Java int array to a MemorySegment.
     *
     * @param segment The destination MemorySegment
     * @param data The source int array
     */
    public static void copyToSegment(MemorySegment segment, int[] data)
    {
        for (int i = 0; i < data.length; i++) {
            segment.setAtIndex(ValueLayout.JAVA_INT, i, data[i]);
        }
    }

    /**
     * Copy data from a Java long array to a MemorySegment.
     *
     * @param segment The destination MemorySegment
     * @param data The source long array
     */
    public static void copyToSegment(MemorySegment segment, long[] data)
    {
        for (int i = 0; i < data.length; i++) {
            segment.setAtIndex(ValueLayout.JAVA_LONG, i, data[i]);
        }
    }

    /**
     * Copy data from a MemorySegment to a Java int array.
     *
     * @param segment The source MemorySegment
     * @param data The destination int array
     */
    public static void copyFromSegment(MemorySegment segment, int[] data)
    {
        for (int i = 0; i < data.length; i++) {
            data[i] = segment.getAtIndex(ValueLayout.JAVA_INT, i);
        }
    }

    /**
     * Copy data from a MemorySegment to a Java long array.
     *
     * @param segment The source MemorySegment
     * @param data The destination long array
     */
    public static void copyFromSegment(MemorySegment segment, long[] data)
    {
        for (int i = 0; i < data.length; i++) {
            data[i] = segment.getAtIndex(ValueLayout.JAVA_LONG, i);
        }
    }

    /**
     * Get an integer value from a MemorySegment.
     *
     * @param segment The MemorySegment to read from
     * @return The integer value at offset 0
     */
    public static int getInt(MemorySegment segment) { return segment.get(ValueLayout.JAVA_INT, 0); }

    /**
     * Get a long value from a MemorySegment.
     *
     * @param segment The MemorySegment to read from
     * @return The long value at offset 0
     */
    public static long getLong(MemorySegment segment) { return segment.get(ValueLayout.JAVA_LONG, 0); }

    /**
     * Get a double value from a MemorySegment.
     *
     * @param segment The MemorySegment to read from
     * @return The double value at offset 0
     */
    public static double getDouble(MemorySegment segment) { return segment.get(ValueLayout.JAVA_DOUBLE, 0); }

    /**
     * Set an integer value in a MemorySegment.
     *
     * @param segment The MemorySegment to write to
     * @param value The integer value to write
     */
    public static void setInt(MemorySegment segment, int value)
    {
        segment.set(ValueLayout.JAVA_INT, 0, value);
    }

    /**
     * Set a long value in a MemorySegment.
     *
     * @param segment The MemorySegment to write to
     * @param value The long value to write
     */
    public static void setLong(MemorySegment segment, long value)
    {
        segment.set(ValueLayout.JAVA_LONG, 0, value);
    }

    /**
     * Format an error message for a failed HDF5 operation.
     *
     * @param operation The name of the operation that failed
     * @param retVal The error return value
     * @return A formatted error message
     */
    public static String formatError(String operation, int retVal)
    {
        return String.format("%s failed with return value: %d", operation, retVal);
    }

    /**
     * Format an error message for a failed HDF5 operation.
     *
     * @param operation The name of the operation that failed
     * @param retVal The error return value (long version)
     * @return A formatted error message
     */
    public static String formatError(String operation, long retVal)
    {
        return String.format("%s failed with return value: %d", operation, retVal);
    }

    /**
     * Close an HDF5 identifier if it's valid.
     * Uses the appropriate close function based on the identifier type.
     *
     * @param hid The HDF5 identifier to close
     * @param closeFunc A function that closes the identifier (returns int)
     * @return true if close succeeded or id was invalid, false if close failed
     */
    public static boolean closeQuietly(long hid, java.util.function.LongToIntFunction closeFunc)
    {
        if (hid >= 0) {
            try {
                return closeFunc.applyAsInt(hid) >= 0;
            }
            catch (Exception e) {
                return false;
            }
        }
        return true;
    }
}
