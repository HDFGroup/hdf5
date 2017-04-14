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

package hdf.hdf5lib;

import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5JavaException;

/**
 * This class encapsulates native methods to deal with arrays of numbers,
 * converting from numbers to bytes and bytes to numbers.
 * <p>
 * These routines are used by class <b>HDFArray</b> to pass data to and from the
 * HDF-5 library.
 * <p>
 * Methods xxxToByte() convert a Java array of primitive numbers (int, short,
 * ...) to a Java array of bytes. Methods byteToXxx() convert from a Java array
 * of bytes into a Java array of primitive numbers (int, short, ...)
 * <p>
 * Variant interfaces convert a section of an array, and also can convert to
 * sub-classes of Java <b>Number</b>.
 * <P>
 * <b>See also:</b> hdf.hdf5lib.HDFArray.
 */

public class HDFNativeData {

    /**
     * Convert an array of bytes into an array of ints
     *
     * @param data
     *            The input array of bytes
     * @return an array of int
     */
    public synchronized static native int[] byteToInt(byte[] data);

    /**
     * Convert an array of bytes into an array of floats
     *
     * @param data
     *            The input array of bytes
     * @return an array of float
     */
    public synchronized static native float[] byteToFloat(byte[] data);

    /**
     * Convert an array of bytes into an array of shorts
     *
     * @param data
     *            The input array of bytes
     * @return an array of short
     */
    public synchronized static native short[] byteToShort(byte[] data);

    /**
     * Convert an array of bytes into an array of long
     *
     * @param data
     *            The input array of bytes
     * @return an array of long
     */
    /*
     * does this really work? C 'long' is 32 bits, Java 'long' is 64-bits. What
     * does this routine actually do?
     */
    public synchronized static native long[] byteToLong(byte[] data);

    /**
     * Convert an array of bytes into an array of double
     *
     * @param data
     *            The input array of bytes
     * @return an array of double
     */
    public synchronized static native double[] byteToDouble(byte[] data);

    /**
     * Convert a range from an array of bytes into an array of int
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param len
     *            The number of 'int' to convert
     * @param data
     *            The input array of bytes
     * @return an array of 'len' int
     */
    public synchronized static native int[] byteToInt(int start, int len,
            byte[] data);

    /**
     * Convert 4 bytes from an array of bytes into a single int
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param data
     *            The input array of bytes
     * @return The integer value of the bytes.
     */
    public synchronized static int byteToInt(byte[] data, int start) {
        int[] ival = new int[1];
        ival = byteToInt(start, 1, data);
        return (ival[0]);
    }

    /**
     * Convert a range from an array of bytes into an array of short
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param len
     *            The number of 'short' to convert
     * @param data
     *            The input array of bytes
     * @return an array of 'len' short
     */
    public synchronized static native short[] byteToShort(int start, int len,
            byte[] data);

    /**
     * Convert 2 bytes from an array of bytes into a single short
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param data
     *            The input array of bytes
     * @return The short value of the bytes.
     */
    public synchronized static short byteToShort(byte[] data, int start) {
        short[] sval = new short[1];
        sval = byteToShort(start, 1, data);
        return (sval[0]);
    }

    /**
     * Convert a range from an array of bytes into an array of float
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param len
     *            The number of 'float' to convert
     * @param data
     *            The input array of bytes
     * @return an array of 'len' float
     */
    public synchronized static native float[] byteToFloat(int start, int len,
            byte[] data);

    /**
     * Convert 4 bytes from an array of bytes into a single float
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param data
     *            The input array of bytes
     * @return The float value of the bytes.
     */
    public synchronized static float byteToFloat(byte[] data, int start) {
        float[] fval = new float[1];
        fval = byteToFloat(start, 1, data);
        return (fval[0]);
    }

    /**
     * Convert a range from an array of bytes into an array of long
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param len
     *            The number of 'long' to convert
     * @param data
     *            The input array of bytes
     * @return an array of 'len' long
     */
    public synchronized static native long[] byteToLong(int start, int len,
            byte[] data);

    /**
     * Convert 8 bytes from an array of bytes into a single long
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param data
     *            The input array of bytes
     * @return The long value of the bytes.
     */
    public synchronized static long byteToLong(byte[] data, int start) {
        long[] lval = new long[1];
        lval = byteToLong(start, 1, data);
        return (lval[0]);
    }

    /**
     * Convert a range from an array of bytes into an array of double
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param len
     *            The number of 'double' to convert
     * @param data
     *            The input array of bytes
     * @return an array of 'len' double
     */
    public synchronized static native double[] byteToDouble(int start, int len,
            byte[] data);

    /**
     * Convert 8 bytes from an array of bytes into a single double
     *
     * @param start
     *            The position in the input array of bytes to start
     * @param data
     *            The input array of bytes
     * @return The double value of the bytes.
     */
    public synchronized static double byteToDouble(byte[] data, int start) {
        double[] dval = new double[1];
        dval = byteToDouble(start, 1, data);
        return (dval[0]);
    }

    /**
     * Convert a range from an array of int into an array of bytes.
     *
     * @param start
     *            The position in the input array of int to start
     * @param len
     *            The number of 'int' to convert
     * @param data
     *            The input array of int
     * @return an array of bytes
     */
    public synchronized static native byte[] intToByte(int start, int len,
            int[] data);

    /**
     * Convert a range from an array of short into an array of bytes.
     *
     * @param start
     *            The position in the input array of int to start
     * @param len
     *            The number of 'short' to convert
     * @param data
     *            The input array of short
     * @return an array of bytes
     */
    public synchronized static native byte[] shortToByte(int start, int len,
            short[] data);

    /**
     * Convert a range from an array of float into an array of bytes.
     *
     * @param start
     *            The position in the input array of int to start
     * @param len
     *            The number of 'float' to convert
     * @param data
     *            The input array of float
     * @return an array of bytes
     */
    public synchronized static native byte[] floatToByte(int start, int len,
            float[] data);

    /**
     * Convert a range from an array of long into an array of bytes.
     *
     * @param start
     *            The position in the input array of int to start
     * @param len
     *            The number of 'long' to convert
     * @param data
     *            The input array of long
     * @return an array of bytes
     */
    public synchronized static native byte[] longToByte(int start, int len,
            long[] data);

    /**
     * Convert a range from an array of double into an array of bytes.
     *
     * @param start
     *            The position in the input array of double to start
     * @param len
     *            The number of 'double' to convert
     * @param data
     *            The input array of double
     * @return an array of bytes
     */
    public synchronized static native byte[] doubleToByte(int start, int len,
            double[] data);

    /**
     * Convert a single byte into an array of one byte.
     * <p>
     * (This is a trivial method.)
     *
     * @param data
     *            The input byte
     * @return an array of bytes
     */
    public synchronized static native byte[] byteToByte(byte data);

    /**
     * Convert a single Byte object into an array of one byte.
     * <p>
     * (This is an almost trivial method.)
     *
     * @param data
     *            The input Byte
     * @return an array of bytes
     */
    public synchronized static byte[] byteToByte(Byte data) {
        return byteToByte(data.byteValue());
    }

    /**
     * Convert a single int into an array of 4 bytes.
     *
     * @param data
     *            The input int
     * @return an array of bytes
     */
    public synchronized static native byte[] intToByte(int data);

    /**
     * Convert a single Integer object into an array of 4 bytes.
     *
     * @param data
     *            The input Integer
     * @return an array of bytes
     */
    public synchronized static byte[] intToByte(Integer data) {
        return intToByte(data.intValue());
    }

    /**
     * Convert a single short into an array of 2 bytes.
     *
     * @param data
     *            The input short
     * @return an array of bytes
     */
    public synchronized static native byte[] shortToByte(short data);

    /**
     * Convert a single Short object into an array of 2 bytes.
     *
     * @param data
     *            The input Short
     * @return an array of bytes
     */
    public synchronized static byte[] shortToByte(Short data) {
        return shortToByte(data.shortValue());
    }

    /**
     * Convert a single float into an array of 4 bytes.
     *
     * @param data
     *            The input float
     * @return an array of bytes
     */
    public synchronized static native byte[] floatToByte(float data);

    /**
     * Convert a single Float object into an array of 4 bytes.
     *
     * @param data
     *            The input Float
     * @return an array of bytes
     */
    public synchronized static byte[] floatToByte(Float data) {
        return floatToByte(data.floatValue());
    };

    /**
     * Convert a single long into an array of 8 bytes.
     *
     * @param data
     *            The input long
     * @return an array of bytes
     */
    public synchronized static native byte[] longToByte(long data);

    /**
     * Convert a single Long object into an array of 8 bytes.
     *
     * @param data
     *            The input Long
     * @return an array of bytes
     */
    public synchronized static byte[] longToByte(Long data) {
        return longToByte(data.longValue());
    }

    /**
     * Convert a single double into an array of 8 bytes.
     *
     * @param data
     *            The input double
     * @return an array of bytes
     */
    public synchronized static native byte[] doubleToByte(double data);

    /**
     * Convert a single Double object into an array of 8 bytes.
     *
     * @param data
     *            The input Double
     * @return an array of bytes
     */
    public synchronized static byte[] doubleToByte(Double data) {
        return doubleToByte(data.doubleValue());
    }

    /**
     * Create a Number object from an array of bytes.
     *
     * @param barray
     *            The bytes to be converted
     * @param obj
     *            Input object of the desired output class. Must be a sub-class
     *            of Number.
     * @return A Object of the type of obj.
     *
     * @exception HDF5Exception
     *                - Error unsupported type.
     */
    public synchronized static Object byteToNumber(byte[] barray, Object obj)
            throws HDF5Exception {
        Class theClass = obj.getClass();
        String type = theClass.getName();
        Object retobj = null;

        if (type.equals("java.lang.Integer")) {
            int[] i = hdf.hdf5lib.HDFNativeData.byteToInt(0, 1, barray);
            retobj = new Integer(i[0]);
        }
        else if (type.equals("java.lang.Byte")) {
            retobj = new Byte(barray[0]);
        }
        else if (type.equals("java.lang.Short")) {
            short[] f = hdf.hdf5lib.HDFNativeData
                    .byteToShort(0, 1, barray);
            retobj = new Short(f[0]);
        }
        else if (type.equals("java.lang.Float")) {
            float[] f = hdf.hdf5lib.HDFNativeData
                    .byteToFloat(0, 1, barray);
            retobj = new Float(f[0]);
        }
        else if (type.equals("java.lang.Long")) {
            long[] f = hdf.hdf5lib.HDFNativeData.byteToLong(0, 1, barray);
            retobj = new Long(f[0]);
        }
        else if (type.equals("java.lang.Double")) {
            double[] f = hdf.hdf5lib.HDFNativeData.byteToDouble(0, 1,
                    barray);
            retobj = new Double(f[0]);
        }
        else {
            /* exception: unsupported type */
            HDF5Exception ex = new HDF5JavaException(
                    "byteToNumber: setfield bad type: " + obj + " " + type);
            throw (ex);
        }
        return (retobj);
    }
}
