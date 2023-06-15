/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/**
 @page HDF5LIB_UG HDF5 Java Package
 * This package is the Java interface for the HDF5 library.
 * <p>
 * This code is the called by Java programs to access the entry points of the HDF5 library.
 * Each routine wraps a single
 * HDF5 entry point, generally with the arguments and return codes analogous to the C interface.
 * <p>
 * For details of the HDF5 library, see the HDF5 Documentation at:
 *     <a href="http://hdfgroup.org/HDF5/">http://hdfgroup.org/HDF5/</a>
 * <hr>
 * <p>
 * <b>Mapping of arguments for Java</b>
 *
 * <p>
 * In general, arguments to the HDF Java API are straightforward translations from the 'C' API described
 * in the HDF Reference Manual.
 *
 * <table border=1>
 * <caption><b>HDF5 C types to Java types</b> </caption>
 * <tr>
 * <td><b>HDF5</b></td>
 * <td><b>Java</b></td>
 * </tr>
 * <tr>
 * <td>H5T_NATIVE_INT</td>
 * <td>int, Integer</td>
 * </tr>
 * <tr>
 * <td>H5T_NATIVE_SHORT</td>
 * <td>short, Short</td>
 * </tr>
 * <tr>
 * <td>H5T_NATIVE_FLOAT</td>
 * <td>float, Float</td>
 * </tr>
 * <tr>
 * <td>H5T_NATIVE_DOUBLE</td>
 * <td>double, Double</td>
 * </tr>
 * <tr>
 * <td>H5T_NATIVE_CHAR</td>
 * <td>byte, Byte</td>
 * </tr>
 * <tr>
 * <td>H5T_C_S1</td>
 * <td>java.lang.String</td>
 * </tr>
 * <tr>
 * <td>void * <br />
 * (i.e., pointer to `Any')</td>
 * <td>Special -- see HDFArray</td>
 * </tr>
 * </table>
 * <b>General Rules for Passing Arguments and Results</b>
 * <p>
 * In general, arguments passed <b>IN</b> to Java are the analogous basic types, as above. The exception
 * is for arrays, which are discussed below.
 * <p>
 * The <i>return value</i> of Java methods is also the analogous type, as above. A major exception to that
 * rule is that
 * all HDF functions that return SUCCEED/FAIL are declared <i>boolean</i> in the Java version, rather than
 * <i>int</i> as
 * in the C. Functions that return a value or else FAIL are declared the equivalent to the C function.
 * However, in most
 * cases the Java method will raise an exception instead of returning an error code.
 * @see @ref ERRORS.
 * <p>
 * Java does not support pass by reference of arguments, so arguments that are returned through <b>OUT</b>
 * parameters
 * must be wrapped in an object or array. The Java API for HDF consistently wraps arguments in arrays.
 * <p>
 * For instance, a function that returns two integers is declared:
 *
 * <pre>
 *       h_err_t HDF5dummy( int *a1, int *a2)
 * </pre>
 *
 * For the Java interface, this would be declared:
 *
 * <pre>
 * public synchronized static native void HDF5dummy(int args[]);
 * </pre>
 *
 * where <i>a1</i> is <i>args[0]</i> and <i>a2</i> is <i>args[1]</i>, and would be invoked:
 *
 * <pre>
 * H5.HDF5dummy(a);
 * </pre>
 *
 * <p>
 * All the routines where this convention is used will have specific documentation of the details, given
 * below.
 * <p>
 * <b>Arrays</b>
 * <p>
 * HDF5 needs to read and write multi-dimensional arrays of any number type (and records). The HDF5 API
 * describes the
 * layout of the source and destination, and the data for the array passed as a block of bytes, for instance,
 *
 * <pre>
 *      herr_t H5Dread(long fid, long filetype, long memtype, long memspace, void *data);
 * </pre>
 *
 * <p>
 * where ``void *'' means that the data may be any valid numeric type, and is a contiguous block of bytes that
 * is the
 * data for a multi-dimensional array. The other parameters describe the dimensions, rank, and datatype of the
 * array on
 * disk (source) and in memory (destination).
 * <p>
 * For Java, this ``ANY'' is a problem, as the type of data must always be declared. Furthermore,
 * multidimensional
 * arrays are definitely <i>not</i> laid out contiguously in memory. It would be infeasible to declare a
 * separate
 * routine for every combination of number type and dimensionality. For that reason, the
 *  <a href="./HDFArray.html"><b>HDFArray</b></a> class is used to discover the type, shape, and
 * size of the
 * data array at run time, and to convert to and from a contiguous array of bytes in synchronized static
 * native C order.
 * <p>
 * The upshot is that any Java array of numbers (either primitive or sub-classes of type <b>Number</b>) can be
 * passed as
 * an ``Object'', and the Java API will translate to and from the appropriate packed array of bytes needed by
 * the C
 * library. So the function above would be declared:
 *
 * <pre>
 * public synchronized static native int H5Dread(long fid, long filetype, long memtype, long memspace, Object
 * data);
 * </pre>
 *            OPEN_IDS.addElement(id);

 * and the parameter <i>data</i> can be any multi-dimensional array of numbers, such as float[][], or
 * int[][][], or
 * Double[][].
 * <p>
 * <b>HDF5 Constants</b>
 * <p>
 * The HDF5 API defines a set of constants and enumerated values. Most of these values are available to Java
 * programs
 * via the class <a href="./HDF5Constants.html"> <b>HDF5Constants</b></a>. For example, the
 * parameters for
 * the h5open() call include two numeric values, <b><i>HDFConstants.H5F_ACC_RDWR</i></b> and
 * <b><i>HDF5Constants.H5P_DEFAULT</i></b>. As would be expected, these numbers correspond to the C constants
 * <b><i>H5F_ACC_RDWR</i></b> and <b><i>H5P_DEFAULT</i></b>.
 * <p>
 * The HDF5 API defines a set of values that describe number types and sizes, such as "H5T_NATIVE_INT" and
 * "hsize_t".
 * These values are determined at run time by the HDF5 C library. To support these parameters, the Java class
 * <a href="./HDF5CDataTypes.html"> <b>HDF5CDataTypes</b></a> looks up the values when initiated.
 * The values
 * can be accessed as public variables of the Java class, such as:
 *
 * <pre>
 * long data_type = HDF5CDataTypes.JH5T_NATIVE_INT;
 * </pre>
 *
 * The Java application uses both types of constants the same way, the only difference is that the
 * <b><i>HDF5CDataTypes</i></b> may have different values on different platforms.
 * <p>
 * <b>Error handling and Exceptions</b>
 * <p>
 * The HDF5 error API (H5E) manages the behavior of the error stack in the HDF5 library. This API is
 * available from the
 * JHI5. Errors are converted into Java exceptions. This is totally different from the C interface, but is
 * very natural
 * for Java programming.
 * <p>
 * The exceptions of the JHI5 are organized as sub-classes of the class
 * <a href="./HDF5Exception.html"> <b>HDF5Exception</b></a>. There are two subclasses
 * of
 * <b>HDF5Exception</b>, @ref ERRORSLIB <b>HDF5LibraryException</b>
 * and @ref ERRORSJAVA <b>HDF5JavaException</b>.
 * The sub-classes of the former represent errors from the HDF5 C library,
 * while sub-classes of the latter represent errors in the JHI5 wrapper and support code.
 * <p>
 * The super-class <b><i>HDF5LibraryException</i></b> implements the method
 * '<b><i>printStackTrace()</i></b>', which prints out the HDF5 error stack, as described
 *  in the HDF5 C API <i><b>@ref H5Eprint()</b>.</i> This may be
 * used by Java
 * exception handlers to print out the HDF5 error stack.
 * <hr>
 *
 * @ref HDF5LIB
 *
 * <b>@see: <a href="http://hdfgroup.org/HDF5/"> HDF5"</a></b>
 *
 */
package hdf.hdf5lib;
