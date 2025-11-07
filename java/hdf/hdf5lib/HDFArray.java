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

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.LongBuffer;
import java.nio.ShortBuffer;
import java.util.Arrays;

import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5JavaException;

/**
 * \page HDFARRAY Java Array Conversion This is a class for handling multidimensional arrays for HDF.
 * <p>
 * The purpose is to allow the storage and retrieval of arbitrary array types containing scientific data.
 * <p>
 * The methods support the conversion of an array to and from Java to a one-dimensional array of bytes
 * suitable for I/O by the C library. <p> This class heavily uses the
 *
 * @ref HDFNATIVE class to convert between Java and C representations.
 */

public class HDFArray {
    private Object _theArray      = null;
    private ArrayDescriptor _desc = null;
    private byte[] _barray        = null;

    // public HDFArray() {}

    /**
     * The input must be a Java Array (possibly multidimensional) of primitive numbers or sub-classes of
     * Number. <p> The input is analysed to determine the number of dimensions and size of each dimension, as
     * well as the type of the elements. <p> The description is saved in private variables, and used to
     * convert data.
     *
     * @param anArray The array object.
     * @exception hdf.hdf5lib.exceptions.HDF5JavaException object is not an array.
     */
    public HDFArray(Object anArray) throws HDF5JavaException
    {
        if (anArray == null) {
            HDF5JavaException ex = new HDF5JavaException("HDFArray: array is null?: ");
        }
        Class tc = anArray.getClass();
        if (tc.isArray() == false) {
            /* exception: not an array */
            HDF5JavaException ex = new HDF5JavaException("HDFArray: not an array?: ");
            throw(ex);
        }
        _theArray = anArray;
        _desc     = new ArrayDescriptor(_theArray);

        /* extra error checking -- probably not needed */
        if (_desc == null) {
            HDF5JavaException ex =
                new HDF5JavaException("HDFArray: internal error: array description failed?: ");
            throw(ex);
        }
    }

    /**
     * Allocate a one-dimensional array of bytes sufficient to store the array.
     *
     * @return A one-D array of bytes, filled with zeroes. The bytes are sufficient to hold the data of the
     *     Array passed
     *         to the constructor.
     * @exception hdf.hdf5lib.exceptions.HDF5JavaException Allocation failed.
     */

    public byte[] emptyBytes() throws HDF5JavaException
    {
        byte[] b = null;

        if ((ArrayDescriptor.dims == 1) && (ArrayDescriptor.NT == 'B')) {
            b = (byte[])_theArray;
        }
        else {
            b = new byte[ArrayDescriptor.totalSize];
        }
        if (b == null) {
            HDF5JavaException ex = new HDF5JavaException("HDFArray: emptyBytes: allocation failed");
            throw(ex);
        }
        return (b);
    }

    /**
     * Given a Java array of numbers, convert it to a one-dimensional array of bytes in correct native order.
     *
     * @return A one-D array of bytes, constructed from the Array passed to the constructor.
     * @exception hdf.hdf5lib.exceptions.HDF5JavaException the object not an array or other internal error.
     */
    public byte[] byteify() throws HDF5JavaException
    {
        if (_barray != null) {
            return _barray;
        }

        if (_theArray == null) {
            /* exception: not an array */
            HDF5JavaException ex = new HDF5JavaException("HDFArray: byteify not an array?: ");
            throw(ex);
        }

        if (ArrayDescriptor.dims == 1) {
            /* special case */
            if (ArrayDescriptor.NT == 'B') {
                /* really special case! */
                _barray = (byte[])_theArray;
                return _barray;
            }
            else {
                try {
                    _barray = new byte[ArrayDescriptor.totalSize];

                    byte[] therow;
                    if (ArrayDescriptor.NT == 'I') {
                        ByteBuffer byteBuffer = ByteBuffer.allocate(ArrayDescriptor.dimlen[1] * Integer.SIZE);
                        byteBuffer.order(ByteOrder.nativeOrder());
                        IntBuffer intBuffer = byteBuffer.asIntBuffer();
                        intBuffer.put((int[])_theArray);
                        therow = byteBuffer.array();
                    }
                    else if (ArrayDescriptor.NT == 'S') {
                        ByteBuffer byteBuffer = ByteBuffer.allocate(ArrayDescriptor.dimlen[1] * Short.SIZE);
                        byteBuffer.order(ByteOrder.nativeOrder());
                        ShortBuffer shortBuffer = byteBuffer.asShortBuffer();
                        shortBuffer.put((short[])_theArray);
                        therow = byteBuffer.array();
                    }
                    else if (ArrayDescriptor.NT == 'F') {
                        ByteBuffer byteBuffer = ByteBuffer.allocate(ArrayDescriptor.dimlen[1] * Float.SIZE);
                        byteBuffer.order(ByteOrder.nativeOrder());
                        FloatBuffer floatBuffer = byteBuffer.asFloatBuffer();
                        floatBuffer.put((float[])_theArray);
                        therow = byteBuffer.array();
                    }
                    else if (ArrayDescriptor.NT == 'J') {
                        ByteBuffer byteBuffer = ByteBuffer.allocate(ArrayDescriptor.dimlen[1] * Long.SIZE);
                        byteBuffer.order(ByteOrder.nativeOrder());
                        LongBuffer longBuffer = byteBuffer.asLongBuffer();
                        longBuffer.put((long[])_theArray);
                        therow = byteBuffer.array();
                    }
                    else if (ArrayDescriptor.NT == 'D') {
                        ByteBuffer byteBuffer = ByteBuffer.allocate(ArrayDescriptor.dimlen[1] * Double.SIZE);
                        byteBuffer.order(ByteOrder.nativeOrder());
                        DoubleBuffer doubleBuffer = byteBuffer.asDoubleBuffer();
                        doubleBuffer.put((double[])_theArray);
                        therow = byteBuffer.array();
                    }
                    else if (ArrayDescriptor.NT == 'L') {
                        if (ArrayDescriptor.className.equals("java.lang.Byte")) {
                            therow = ByteObjToByte((Byte[])_theArray);
                        }
                        else if (ArrayDescriptor.className.equals("java.lang.Integer")) {
                            therow = IntegerToByte((Integer[])_theArray);
                        }
                        else if (ArrayDescriptor.className.equals("java.lang.Short")) {
                            therow = ShortToByte((Short[])_theArray);
                        }
                        else if (ArrayDescriptor.className.equals("java.lang.Float")) {
                            therow = FloatObjToByte((Float[])_theArray);
                        }
                        else if (ArrayDescriptor.className.equals("java.lang.Double")) {
                            therow = DoubleObjToByte((Double[])_theArray);
                        }
                        else if (ArrayDescriptor.className.equals("java.lang.Long")) {
                            therow = LongObjToByte((Long[])_theArray);
                        }
                        else {
                            HDF5JavaException ex = new HDF5JavaException("HDFArray: unknown type of Object?");
                            throw(ex);
                        }
                    }
                    else {
                        HDF5JavaException ex = new HDF5JavaException("HDFArray: unknown type of data?");
                        throw(ex);
                    }
                    System.arraycopy(therow, 0, _barray, 0,
                                     (ArrayDescriptor.dimlen[1] * ArrayDescriptor.NTsize));
                    return _barray;
                }
                catch (OutOfMemoryError err) {
                    HDF5JavaException ex = new HDF5JavaException("HDFArray: byteify array too big?");
                    throw(ex);
                }
            }
        }

        try {
            _barray = new byte[ArrayDescriptor.totalSize];
        }
        catch (OutOfMemoryError err) {
            HDF5JavaException ex = new HDF5JavaException("HDFArray: byteify array too big?");
            throw(ex);
        }

        Object oo = _theArray;
        int n     = 0; /* the current byte */
        int index = 0;
        int i;
        while (n < ArrayDescriptor.totalSize) {
            oo    = ArrayDescriptor.objs[0];
            index = n / ArrayDescriptor.bytetoindex[0];
            index %= ArrayDescriptor.dimlen[0];
            for (i = 0; i < (ArrayDescriptor.dims); i++) {
                index = n / ArrayDescriptor.bytetoindex[i];
                index %= ArrayDescriptor.dimlen[i];

                if (index == ArrayDescriptor.currentindex[i]) {
                    /* then use cached copy */
                    oo = ArrayDescriptor.objs[i];
                }
                else {
                    /* check range of index */
                    if (index > (ArrayDescriptor.dimlen[i] - 1)) {
                        throw new java.lang.IndexOutOfBoundsException("HDFArray: byteify index OOB?");
                    }
                    oo                              = java.lang.reflect.Array.get(oo, index);
                    ArrayDescriptor.currentindex[i] = index;
                    ArrayDescriptor.objs[i]         = oo;
                }
            }

            /* byte-ify */
            byte arow[];
            try {
                if (ArrayDescriptor.NT == 'J') {
                    ByteBuffer byteBuffer =
                        ByteBuffer.allocate(ArrayDescriptor.dimlen[ArrayDescriptor.dims] * Long.BYTES);
                    byteBuffer.order(ByteOrder.nativeOrder());
                    LongBuffer longBuffer = byteBuffer.asLongBuffer();
                    longBuffer.put((long[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    arow = byteBuffer.array();
                }
                else if (ArrayDescriptor.NT == 'I') {
                    ByteBuffer byteBuffer =
                        ByteBuffer.allocate(ArrayDescriptor.dimlen[ArrayDescriptor.dims] * Integer.BYTES);
                    byteBuffer.order(ByteOrder.nativeOrder());
                    IntBuffer intBuffer = byteBuffer.asIntBuffer();
                    intBuffer.put((int[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    arow = byteBuffer.array();
                }
                else if (ArrayDescriptor.NT == 'S') {
                    ByteBuffer byteBuffer =
                        ByteBuffer.allocate(ArrayDescriptor.dimlen[ArrayDescriptor.dims] * Short.BYTES);
                    byteBuffer.order(ByteOrder.nativeOrder());
                    ShortBuffer shortBuffer = byteBuffer.asShortBuffer();
                    shortBuffer.put((short[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    arow = byteBuffer.array();
                }
                else if (ArrayDescriptor.NT == 'B') {
                    arow = (byte[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1];
                }
                else if (ArrayDescriptor.NT == 'F') {
                    /* 32 bit float */
                    ByteBuffer byteBuffer =
                        ByteBuffer.allocate(ArrayDescriptor.dimlen[ArrayDescriptor.dims] * Float.BYTES);
                    byteBuffer.order(ByteOrder.nativeOrder());
                    FloatBuffer floatBuffer = byteBuffer.asFloatBuffer();
                    floatBuffer.put((float[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    arow = byteBuffer.array();
                }
                else if (ArrayDescriptor.NT == 'D') {
                    /* 64 bit float */
                    ByteBuffer byteBuffer =
                        ByteBuffer.allocate(ArrayDescriptor.dimlen[ArrayDescriptor.dims] * Double.BYTES);
                    byteBuffer.order(ByteOrder.nativeOrder());
                    DoubleBuffer doubleBuffer = byteBuffer.asDoubleBuffer();
                    doubleBuffer.put((double[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    arow = byteBuffer.array();
                }
                else if (ArrayDescriptor.NT == 'L') {
                    if (ArrayDescriptor.className.equals("java.lang.Byte")) {
                        arow = ByteObjToByte((Byte[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    }
                    else if (ArrayDescriptor.className.equals("java.lang.Integer")) {
                        arow = IntegerToByte((Integer[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    }
                    else if (ArrayDescriptor.className.equals("java.lang.Short")) {
                        arow = ShortToByte((Short[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    }
                    else if (ArrayDescriptor.className.equals("java.lang.Float")) {
                        arow = FloatObjToByte((Float[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    }
                    else if (ArrayDescriptor.className.equals("java.lang.Double")) {
                        arow = DoubleObjToByte((Double[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    }
                    else if (ArrayDescriptor.className.equals("java.lang.Long")) {
                        arow = LongObjToByte((Long[])ArrayDescriptor.objs[ArrayDescriptor.dims - 1]);
                    }
                    else {
                        HDF5JavaException ex =
                            new HDF5JavaException("HDFArray: byteify Object type not implemented?");
                        throw(ex);
                    }
                }
                else {
                    HDF5JavaException ex =
                        new HDF5JavaException("HDFArray: byteify unknown type not implemented?");
                    throw(ex);
                }
                System.arraycopy(arow, 0, _barray, n,
                                 (ArrayDescriptor.dimlen[ArrayDescriptor.dims] * ArrayDescriptor.NTsize));
                n += ArrayDescriptor.bytetoindex[ArrayDescriptor.dims - 1];
            }
            catch (OutOfMemoryError err) {
                HDF5JavaException ex = new HDF5JavaException("HDFArray: byteify array too big?");
                throw(ex);
            }
        }
        /* assert: the whole array is completed--currentindex should == len - 1 */
        /* error checks */
        if (n < ArrayDescriptor.totalSize) {
            throw new java.lang.InternalError(
                new String("HDFArray::byteify: Panic didn't complete all input data: n=  " + n +
                           " size = " + ArrayDescriptor.totalSize));
        }
        for (i = 0; i < ArrayDescriptor.dims; i++) {
            if (ArrayDescriptor.currentindex[i] != ArrayDescriptor.dimlen[i] - 1) {
                throw new java.lang.InternalError(new String("Panic didn't complete all data: currentindex[" +
                                                             i + "] = " + ArrayDescriptor.currentindex[i] +
                                                             " (should be " +
                                                             (ArrayDescriptor.dimlen[i] - 1) + " ?)"));
            }
        }
        return _barray;
    }

    /**
     * Given a one-dimensional array of bytes representing numbers, convert it to a java array of the shape
     * and size passed to the constructor.
     *
     * @param bytes The bytes to construct the Array.
     * @return An Array (possibly multidimensional) of primitive or number objects.
     * @exception hdf.hdf5lib.exceptions.HDF5JavaException the object not an array or other internal error.
     */
    public Object arrayify(byte[] bytes) throws HDF5JavaException
    {
        if (_theArray == null) {
            /* exception: not an array */
            HDF5JavaException ex = new HDF5JavaException("arrayify: not an array?: ");
            throw(ex);
        }

        if (java.lang.reflect.Array.getLength(bytes) != ArrayDescriptor.totalSize) {
            /* exception: array not right size */
            HDF5JavaException ex = new HDF5JavaException("arrayify: array is wrong size?: ");
            throw(ex);
        }
        _barray   = bytes; /* hope that the bytes are correct.... */
        Object oo = _theArray;
        int n     = 0; /* the current byte */
        int m     = 0; /* the current array index */
        int index = 0;
        int i;
        Object flattenedArray = null;

        // Wrap the byte array in a ByteBuffer
        ByteBuffer byteBuffer = ByteBuffer.wrap(_barray);
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN); // Set byte order to little-endian

        switch (ArrayDescriptor.NT) {
        case 'J': {
            // Calculate the size of the new long array
            int longArraySize = _barray.length / Long.BYTES;
            long[] flatArray  = new long[longArraySize];

            // Populate the long array
            for (i = 0; i < longArraySize; i++) {
                flatArray[i] = byteBuffer.getLong();
            }
            flattenedArray = (Object)flatArray;
        } break;
        case 'S': {
            // Calculate the size of the new short array
            int shortArraySize = _barray.length / Short.BYTES;
            short[] flatArray  = new short[shortArraySize];

            // Populate the short array
            for (i = 0; i < shortArraySize; i++) {
                flatArray[i] = byteBuffer.getShort();
            }
            flattenedArray = (Object)flatArray;
        } break;
        case 'I': {
            // Calculate the size of the new int array
            int intArraySize = _barray.length / Integer.BYTES;
            int[] flatArray  = new int[intArraySize];

            // Populate the int array
            for (i = 0; i < intArraySize; i++) {
                flatArray[i] = byteBuffer.getInt();
            }
            flattenedArray = (Object)flatArray;
        } break;
        case 'F': {
            // Calculate the size of the new float array
            int floatArraySize = _barray.length / Float.BYTES;
            float[] flatArray  = new float[floatArraySize];

            // Populate the float array
            for (i = 0; i < floatArraySize; i++) {
                flatArray[i] = byteBuffer.getFloat();
            }
            flattenedArray = (Object)flatArray;
        } break;
        case 'D': {
            // Calculate the size of the new double array
            int doubleArraySize = _barray.length / Double.BYTES;
            double[] flatArray  = new double[doubleArraySize];

            // Populate the double array
            for (i = 0; i < doubleArraySize; i++) {
                flatArray[i] = byteBuffer.getDouble();
            }
            flattenedArray = (Object)flatArray;
        } break;
        case 'B':
            flattenedArray = (Object)_barray;
            break;
        case 'L': {
            if (ArrayDescriptor.className.equals("java.lang.Byte"))
                flattenedArray = (Object)ByteToByteObj(_barray);
            else if (ArrayDescriptor.className.equals("java.lang.Short"))
                flattenedArray = (Object)ByteToShort(_barray);
            else if (ArrayDescriptor.className.equals("java.lang.Integer"))
                flattenedArray = (Object)ByteToInteger(_barray);
            else if (ArrayDescriptor.className.equals("java.lang.Long"))
                flattenedArray = (Object)ByteToLongObj(_barray);
            else if (ArrayDescriptor.className.equals("java.lang.Float"))
                flattenedArray = (Object)ByteToFloatObj(_barray);
            else if (ArrayDescriptor.className.equals("java.lang.Double"))
                flattenedArray = (Object)ByteToDoubleObj(_barray);
            else {
                HDF5JavaException ex =
                    new HDF5JavaException("HDFArray: unsupported Object type: " + ArrayDescriptor.NT);
                throw(ex);
            }
            break;
        } // end of statement for arrays of boxed objects
        default:
            HDF5JavaException ex =
                new HDF5JavaException("HDFArray: unknown or unsupported type: " + ArrayDescriptor.NT);
            throw(ex);
        } // end of switch statement for arrays of primitives

        while (n < ArrayDescriptor.totalSize) {
            oo    = ArrayDescriptor.objs[0];
            index = n / ArrayDescriptor.bytetoindex[0];
            index %= ArrayDescriptor.dimlen[0];
            for (i = 0; i < (ArrayDescriptor.dims); i++) {
                index = n / ArrayDescriptor.bytetoindex[i];
                index %= ArrayDescriptor.dimlen[i];

                if (index == ArrayDescriptor.currentindex[i]) {
                    /* then use cached copy */
                    oo = ArrayDescriptor.objs[i];
                }
                else {
                    /* check range of index */
                    if (index > (ArrayDescriptor.dimlen[i] - 1)) {
                        System.out.println("out of bounds?");
                        return null;
                    }
                    oo                              = java.lang.reflect.Array.get(oo, index);
                    ArrayDescriptor.currentindex[i] = index;
                    ArrayDescriptor.objs[i]         = oo;
                }
            }

            /* array-ify */
            try {
                Object arow = null;
                int mm      = m + ArrayDescriptor.dimlen[ArrayDescriptor.dims];
                switch (ArrayDescriptor.NT) {
                case 'B':
                    arow = (Object)Arrays.copyOfRange((byte[])flattenedArray, m, mm);
                    break;
                case 'S':
                    arow = (Object)Arrays.copyOfRange((short[])flattenedArray, m, mm);
                    break;
                case 'I':
                    arow = (Object)Arrays.copyOfRange((int[])flattenedArray, m, mm);
                    break;
                case 'J':
                    arow = (Object)Arrays.copyOfRange((long[])flattenedArray, m, mm);
                    break;
                case 'F':
                    arow = (Object)Arrays.copyOfRange((float[])flattenedArray, m, mm);
                    break;
                case 'D':
                    arow = (Object)Arrays.copyOfRange((double[])flattenedArray, m, mm);
                    break;
                case 'L': {
                    if (ArrayDescriptor.className.equals("java.lang.Byte"))
                        arow = (Object)Arrays.copyOfRange((Byte[])flattenedArray, m, mm);
                    else if (ArrayDescriptor.className.equals("java.lang.Short"))
                        arow = (Object)Arrays.copyOfRange((Short[])flattenedArray, m, mm);
                    else if (ArrayDescriptor.className.equals("java.lang.Integer"))
                        arow = (Object)Arrays.copyOfRange((Integer[])flattenedArray, m, mm);
                    else if (ArrayDescriptor.className.equals("java.lang.Long"))
                        arow = (Object)Arrays.copyOfRange((Long[])flattenedArray, m, mm);
                    else if (ArrayDescriptor.className.equals("java.lang.Float"))
                        arow = (Object)Arrays.copyOfRange((Float[])flattenedArray, m, mm);
                    else if (ArrayDescriptor.className.equals("java.lang.Double"))
                        arow = (Object)Arrays.copyOfRange((Double[])flattenedArray, m, mm);
                    else {
                        HDF5JavaException ex =
                            new HDF5JavaException("HDFArray: unsupported Object type: " + ArrayDescriptor.NT);
                        throw(ex);
                    }
                    break;
                } // end of statement for arrays of boxed numerics
                } // end of switch statement for arrays of primitives

                if (ArrayDescriptor.dims > 1) {
                    java.lang.reflect.Array.set(ArrayDescriptor.objs[ArrayDescriptor.dims - 2],
                                                (ArrayDescriptor.currentindex[ArrayDescriptor.dims - 1]),
                                                arow);
                }
                n += ArrayDescriptor.bytetoindex[ArrayDescriptor.dims - 1];
                ArrayDescriptor.currentindex[ArrayDescriptor.dims - 1]++;
                m = mm;
            }
            catch (OutOfMemoryError err) {
                HDF5JavaException ex = new HDF5JavaException("HDFArray: arrayify array too big?");
                throw(ex);
            }
        }

        /* assert: the whole array is completed--currentindex should == len - 1 */
        /* error checks */
        if (n < ArrayDescriptor.totalSize) {
            throw new java.lang.InternalError(
                new String("HDFArray::arrayify Panic didn't complete all input data: n=  " + n +
                           " size = " + ArrayDescriptor.totalSize));
        }
        for (i = 0; i <= ArrayDescriptor.dims - 2; i++) {
            if (ArrayDescriptor.currentindex[i] != ArrayDescriptor.dimlen[i] - 1) {
                throw new java.lang.InternalError(
                    new String("HDFArray::arrayify Panic didn't complete all data: currentindex[" + i +
                               "] = " + ArrayDescriptor.currentindex[i] + " (should be " +
                               (ArrayDescriptor.dimlen[i] - 1) + "?"));
            }
        }
        if (ArrayDescriptor.currentindex[ArrayDescriptor.dims - 1] !=
            ArrayDescriptor.dimlen[ArrayDescriptor.dims - 1]) {
            throw new java.lang.InternalError(new String(
                "HDFArray::arrayify Panic didn't complete all data: currentindex[" + i + "] = " +
                ArrayDescriptor.currentindex[i] + " (should be " + (ArrayDescriptor.dimlen[i]) + "?"));
        }

        return _theArray;
    }

    public static byte[] intToBytes(int value)
    {
        ByteBuffer byteBuffer = ByteBuffer.allocate(Integer.BYTES);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Put the integer value into the buffer
        byteBuffer.putInt(value);
        // System.out.println("intToBytes: int= " + value + " bytes= " + Arrays.toString(byteBuffer.array()));

        // Return the backing byte array
        return byteBuffer.array();
    }

    public static int bytesToInt(byte[] bytes) throws HDF5Exception
    {
        if (bytes.length != Integer.BYTES) {
            throw new HDF5Exception("Invalid byte array length for an integer: " + bytes.length);
        }

        // Wrap the byte array in a ByteBuffer
        ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Read and return the integer value from the buffer
        return byteBuffer.getInt();
    }

    public static byte[] IntegerToByte(Integer in[])
    {
        int nelems       = java.lang.reflect.Array.getLength(in);
        byte[] byteArray = new byte[nelems * Integer.BYTES];

        for (int i = 0; i < nelems; i++) {
            int out    = in[i].intValue();
            byte[] tmp = intToBytes(out);
            // System.out.println("IntegerToByte: " + i + " of " + nelems + " int= " + out + " bytes= " +
            //                    Arrays.toString(tmp));
            System.arraycopy(tmp, 0, byteArray, i * Integer.BYTES, Integer.BYTES);
        }
        return byteArray;
    }

    public static Integer[] ByteToInteger(byte[] bin)
    {
        int nelems    = bin.length / Integer.BYTES;
        byte in[]     = new byte[Integer.BYTES];
        Integer[] out = new Integer[nelems];

        for (int i = 0; i < nelems; i++) {
            System.arraycopy(bin, i * Integer.BYTES, in, 0, Integer.BYTES);
            out[i] = Integer.valueOf(bytesToInt(in));
        }
        return out;
    }

    public static byte[] shortToBytes(short value)
    {
        ByteBuffer byteBuffer = ByteBuffer.allocate(Short.BYTES);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Put the short value into the buffer
        byteBuffer.putShort(value);

        // Return the backing byte array
        return byteBuffer.array();
    }

    public static short bytesToShort(byte[] bytes) throws HDF5Exception
    {
        if (bytes.length != Short.BYTES) {
            throw new HDF5Exception("Invalid byte array length for an short: " + bytes.length);
        }

        // Wrap the byte array in a ByteBuffer
        ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Read and return the short value from the buffer
        return byteBuffer.getShort();
    }

    public static byte[] ShortToByte(Short in[])
    {
        int nelems       = java.lang.reflect.Array.getLength(in);
        byte[] byteArray = new byte[nelems * Short.BYTES];

        for (int i = 0; i < nelems; i++) {
            short out = in[i].shortValue();
            System.arraycopy(shortToBytes(out), 0, byteArray, i * Short.BYTES, Short.BYTES);
        }
        return byteArray;
    }

    public static Short[] ByteToShort(byte[] bin)
    {
        int nelems  = bin.length / Short.BYTES;
        byte in[]   = new byte[Short.BYTES];
        Short[] out = new Short[nelems];

        for (int i = 0; i < nelems; i++) {
            System.arraycopy(bin, i * Short.BYTES, in, 0, Short.BYTES);
            out[i] = Short.valueOf(bytesToShort(in));
        }
        return out;
    }

    public static byte[] ByteObjToByte(Byte in[])
    {
        int nelems = java.lang.reflect.Array.getLength((Object)in);
        byte[] out = new byte[nelems];

        for (int i = 0; i < nelems; i++) {
            out[i] = in[i].byteValue();
        }
        return out;
    }

    public static Byte[] ByteToByteObj(byte[] bin)
    {
        int nelems = java.lang.reflect.Array.getLength((Object)bin);
        Byte[] out = new Byte[nelems];

        for (int i = 0; i < nelems; i++) {
            out[i] = Byte.valueOf(bin[0]);
        }
        return out;
    }

    public static Byte[] ByteToByteObj(int start, int len, byte[] bin)
    {
        Byte[] out = new Byte[len];

        for (int i = 0; i < len; i++) {
            out[i] = Byte.valueOf(bin[0]);
        }
        return out;
    }

    public static byte[] floatToBytes(float value)
    {
        ByteBuffer byteBuffer = ByteBuffer.allocate(Float.BYTES);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Put the float value into the buffer
        byteBuffer.putFloat(value);

        // Return the backing byte array
        return byteBuffer.array();
    }

    public static float bytesToFloat(byte[] bytes) throws HDF5Exception
    {
        if (bytes.length != Float.BYTES) {
            throw new HDF5Exception("Invalid byte array length for an float: " + bytes.length);
        }

        // Wrap the byte array in a ByteBuffer
        ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Read and return the float value from the buffer
        return byteBuffer.getFloat();
    }

    public static byte[] FloatObjToByte(Float in[])
    {
        int nelems       = java.lang.reflect.Array.getLength((Object)in);
        byte[] byteArray = new byte[nelems * Float.BYTES];

        for (int i = 0; i < nelems; i++) {
            float out = in[i].floatValue();
            System.arraycopy(floatToBytes(out), 0, byteArray, i * Float.BYTES, Float.BYTES);
        }
        return byteArray;
    }

    public static Float[] ByteToFloatObj(byte[] bin)
    {
        int nelems  = bin.length / Float.BYTES;
        byte in[]   = new byte[Float.BYTES];
        Float[] out = new Float[nelems];

        for (int i = 0; i < nelems; i++) {
            System.arraycopy(bin, i * Float.BYTES, in, 0, Float.BYTES);
            out[i] = Float.valueOf(bytesToFloat(in));
        }
        return out;
    }

    public static byte[] doubleToBytes(double value)
    {
        // Allocate a ByteBuffer with a capacity of 8 bytes (for a double)
        ByteBuffer byteBuffer = ByteBuffer.allocate(Double.BYTES);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Put the double value into the buffer
        byteBuffer.putDouble(value);

        // Return the backing byte array
        return byteBuffer.array();
    }

    public static double bytesToDouble(byte[] bytes) throws HDF5Exception
    {
        if (bytes.length != Double.BYTES) {
            throw new HDF5Exception("Invalid byte array length for an double: " + bytes.length);
        }

        // Wrap the byte array in a ByteBuffer
        ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Read and return the double value from the buffer
        return byteBuffer.getDouble();
    }

    public static byte[] DoubleToByte(Double in[])
    {
        int nelems       = java.lang.reflect.Array.getLength(in);
        byte[] byteArray = new byte[nelems * Double.BYTES];

        for (int i = 0; i < nelems; i++) {
            double out = in[i].doubleValue();
            System.arraycopy(doubleToBytes(out), 0, byteArray, i * Double.BYTES, Double.BYTES);
        }
        return byteArray;
    }

    public static Double[] ByteToDouble(byte[] bin)
    {
        int nelems   = bin.length / Double.BYTES;
        byte in[]    = new byte[Double.BYTES];
        Double[] out = new Double[nelems];

        for (int i = 0; i < nelems; i++) {
            System.arraycopy(bin, i * Double.BYTES, in, 0, Double.BYTES);
            out[i] = Double.valueOf(bytesToDouble(in));
        }
        return out;
    }

    public static byte[] DoubleObjToByte(Double in[])
    {
        int nelems       = java.lang.reflect.Array.getLength((Object)in);
        byte[] byteArray = new byte[nelems * Double.BYTES];

        for (int i = 0; i < nelems; i++) {
            double out = in[i].doubleValue();
            System.arraycopy(doubleToBytes(out), 0, byteArray, i * Double.BYTES, Double.BYTES);
        }
        return byteArray;
    }

    public static Double[] ByteToDoubleObj(byte[] bin)
    {
        int nelems   = bin.length / Double.BYTES;
        byte in[]    = new byte[Double.BYTES];
        Double[] out = new Double[nelems];

        for (int i = 0; i < nelems; i++) {
            System.arraycopy(bin, i * Double.BYTES, in, 0, Double.BYTES);
            out[i] = Double.valueOf(bytesToDouble(in));
        }
        return out;
    }

    public static byte[] longToBytes(long value)
    {
        ByteBuffer byteBuffer = ByteBuffer.allocate(Long.BYTES);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Put the long value into the buffer
        byteBuffer.putLong(value);

        // Return the backing byte array
        return byteBuffer.array();
    }

    public static long bytesToLong(byte[] bytes) throws HDF5Exception
    {
        if (bytes.length != Long.BYTES) {
            throw new HDF5Exception("Invalid byte array length for an long: " + bytes.length);
        }

        // Wrap the byte array in a ByteBuffer
        ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
        byteBuffer.order(ByteOrder.nativeOrder());

        // Read and return the long value from the buffer
        return byteBuffer.getLong();
    }

    public static byte[] LongObjToByte(Long in[])
    {
        int nelems       = java.lang.reflect.Array.getLength((Object)in);
        byte[] byteArray = new byte[nelems * Long.BYTES];

        for (int i = 0; i < nelems; i++) {
            long out = in[i].longValue();
            System.arraycopy(longToBytes(out), 0, byteArray, i * Long.BYTES, Long.BYTES);
        }
        return byteArray;
    }

    public static Long[] ByteToLongObj(byte[] bin)
    {
        int nelems = bin.length / Long.BYTES;
        byte in[]  = new byte[Long.BYTES];
        Long[] out = new Long[nelems];

        for (int i = 0; i < nelems; i++) {
            System.arraycopy(bin, i * Long.BYTES, in, 0, Long.BYTES);
            out[i] = Long.valueOf(bytesToLong(in));
        }
        return out;
    }
}

/**
 * This private class is used by HDFArray to discover the shape and type of an arbitrary array.
 * <p>
 * We use java.lang.reflection here.
 */
class ArrayDescriptor {
    static String theType     = "";
    static Class theClass     = null;
    static int[] dimlen       = null;
    static int[] dimstart     = null;
    static int[] currentindex = null;
    static int[] bytetoindex  = null;
    static int totalSize      = 0;
    static int totalElements  = 0;
    static Object[] objs      = null;
    static char NT            = ' '; /* must be B,S,I,L,F,D, else error */
    static int NTsize         = 0;
    static int dims           = 0;
    static String className;

    public ArrayDescriptor(Object anArray) throws HDF5JavaException
    {
        Class tc = anArray.getClass();
        if (tc.isArray() == false) {
            /* exception: not an array */
            HDF5JavaException ex = new HDF5JavaException("ArrayDescriptor: not an array?: ");
            throw(ex);
        }

        theClass = tc;

        /*
         * parse the type descriptor to discover the shape of the array
         */
        String ss = tc.toString();
        theType   = ss;
        int n     = 6;
        dims      = 0;
        char c    = ' ';
        while (n < ss.length()) {
            c = ss.charAt(n);
            n++;
            if (c == '[') {
                dims++;
            }
        }

        String css  = ss.substring(ss.lastIndexOf('[') + 1);
        Class compC = tc.getComponentType();
        String cs   = compC.toString();
        NT          = c; /* must be B,S,I,L,F,D, else error */
        if (NT == 'B') {
            NTsize = 1;
        }
        else if (NT == 'S') {
            NTsize = 2;
        }
        else if ((NT == 'I') || (NT == 'F')) {
            NTsize = 4;
        }
        else if ((NT == 'J') || (NT == 'D')) {
            NTsize = 8;
        }
        else if (css.startsWith("Ljava.lang.Byte")) {
            NT        = 'L';
            className = "java.lang.Byte";
            NTsize    = 1;
        }
        else if (css.startsWith("Ljava.lang.Short")) {
            NT        = 'L';
            className = "java.lang.Short";
            NTsize    = 2;
        }
        else if (css.startsWith("Ljava.lang.Integer")) {
            NT        = 'L';
            className = "java.lang.Integer";
            NTsize    = 4;
        }
        else if (css.startsWith("Ljava.lang.Float")) {
            NT        = 'L';
            className = "java.lang.Float";
            NTsize    = 4;
        }
        else if (css.startsWith("Ljava.lang.Double")) {
            NT        = 'L';
            className = "java.lang.Double";
            NTsize    = 8;
        }
        else if (css.startsWith("Ljava.lang.Long")) {
            NT        = 'L';
            className = "java.lang.Long";
            NTsize    = 8;
        }
        else if (css.startsWith("Ljava.lang.String")) {
            NT        = 'L';
            className = "java.lang.String";
            NTsize    = 1;
            throw new HDF5JavaException(
                new String("ArrayDesciptor: Warning:  String array not fully supported yet"));
        }
        else {
            /*
             * exception: not a numeric type
             */
            throw new HDF5JavaException(
                new String("ArrayDesciptor: Error:  array is not numeric (type is " + css + ") ?"));
        }

        /* fill in the table */
        dimlen       = new int[dims + 1];
        dimstart     = new int[dims + 1];
        currentindex = new int[dims + 1];
        bytetoindex  = new int[dims + 1];
        objs         = new Object[dims + 1];

        Object o        = anArray;
        objs[0]         = o;
        dimlen[0]       = 1;
        dimstart[0]     = 0;
        currentindex[0] = 0;
        int elements    = 1;
        int i;
        for (i = 1; i <= dims; i++) {
            dimlen[i]       = java.lang.reflect.Array.getLength((Object)o);
            o               = java.lang.reflect.Array.get((Object)o, 0);
            objs[i]         = o;
            dimstart[i]     = 0;
            currentindex[i] = 0;
            elements *= dimlen[i];
        }
        totalElements = elements;

        int j;
        int dd;
        bytetoindex[dims] = NTsize;
        for (i = dims; i >= 0; i--) {
            dd = NTsize;
            for (j = i; j < dims; j++) {
                dd *= dimlen[j + 1];
            }
            bytetoindex[i] = dd;
        }

        totalSize = bytetoindex[0];
    }

    /**
     * Debug dump
     */
    public void dumpInfo()
    {
        System.out.println("Type: " + theType);
        System.out.println("Class: " + theClass);
        System.out.println("NT: " + NT + " NTsize: " + NTsize);
        System.out.println("Array has " + dims + " dimensions (" + totalSize + " bytes, " + totalElements +
                           " elements)");
        int i;
        for (i = 0; i <= dims; i++) {
            Class tc  = objs[i].getClass();
            String ss = tc.toString();
            System.out.println(i + ":  start " + dimstart[i] + ": len " + dimlen[i] + " current " +
                               currentindex[i] + " bytetoindex " + bytetoindex[i] + " object " + objs[i] +
                               " otype " + ss);
        }
    }
}
