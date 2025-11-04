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

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.io.File;
import java.lang.foreign.Arena;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemoryLayout;
import java.lang.foreign.MemoryLayout.PathElement;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.SegmentAllocator;
import java.lang.foreign.SequenceLayout;
import java.lang.foreign.StructLayout;
import java.lang.foreign.SymbolLookup;
import java.lang.foreign.ValueLayout;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.VarHandle;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.LongBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.function.Consumer;

import hdf.hdf5lib.callbacks.H5L_iterate_opdata_t;
import hdf.hdf5lib.callbacks.H5L_iterate_t;
import hdf.hdf5lib.exceptions.HDF5AttributeException;
import hdf.hdf5lib.exceptions.HDF5BtreeException;
import hdf.hdf5lib.exceptions.HDF5DataFiltersException;
import hdf.hdf5lib.exceptions.HDF5DataStorageException;
import hdf.hdf5lib.exceptions.HDF5DatasetInterfaceException;
import hdf.hdf5lib.exceptions.HDF5DataspaceInterfaceException;
import hdf.hdf5lib.exceptions.HDF5DatatypeInterfaceException;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5ExternalFileListException;
import hdf.hdf5lib.exceptions.HDF5FileInterfaceException;
import hdf.hdf5lib.exceptions.HDF5FunctionArgumentException;
import hdf.hdf5lib.exceptions.HDF5FunctionEntryExitException;
import hdf.hdf5lib.exceptions.HDF5HeapException;
import hdf.hdf5lib.exceptions.HDF5IdException;
import hdf.hdf5lib.exceptions.HDF5InternalErrorException;
import hdf.hdf5lib.exceptions.HDF5JavaException;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.exceptions.HDF5LowLevelIOException;
import hdf.hdf5lib.exceptions.HDF5MetaDataCacheException;
import hdf.hdf5lib.exceptions.HDF5ObjectHeaderException;
import hdf.hdf5lib.exceptions.HDF5PropertyListInterfaceException;
import hdf.hdf5lib.exceptions.HDF5ReferenceException;
import hdf.hdf5lib.exceptions.HDF5ResourceUnavailableException;
import hdf.hdf5lib.exceptions.HDF5SymbolTableException;
// import hdf.hdf5lib.structs.H5AC_cache_config_t;
// import hdf.hdf5lib.structs.H5A_info_t;
// import hdf.hdf5lib.structs.H5E_error2_t;
// import hdf.hdf5lib.structs.H5FD_hdfs_fapl_t;
import hdf.hdf5lib.structs.H5FD_ros3_fapl_t;
import hdf.hdf5lib.structs.H5F_info2_t;
import hdf.hdf5lib.structs.H5G_info_t;
import hdf.hdf5lib.structs.H5L_info_t;
// import hdf.hdf5lib.structs.H5O_info_t;
// import hdf.hdf5lib.structs.H5O_native_info_t;
import hdf.hdf5lib.structs.H5O_token_t;
import hdf.hdf5lib.structs.H5_ih_info_t;

import org.hdfgroup.javahdf5.*;
import org.hdfgroup.javahdf5.hvl_t;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * \page HDF5LIB HDF5 Java API Package
 * This class is the Java interface for the HDF5 library.
 * <p>
 * This code is the called by Java programs to access the entry points of the HDF5 library. Each routine wraps
 * a single HDF5 entry point, generally with the arguments and return codes analogous to the C interface.
 * <p>
 * For details of the HDF5 library, @see @ref RM
 * <hr>
 * <p>
 * <b>Mapping of arguments for Java</b>
 *
 * <p>
 * In general, arguments to the HDF Java API are straightforward translations from the 'C' API described in
 * the @ref RM.
 *
 * <table border=1>
 * <caption><b>HDF5 C types to Java types</b> </caption>
 * <tr>
 * <td><b>HDF5</b></td>
 * <td><b>Java</b></td>
 * </tr>
 * <tr>
 * <td>@ref H5T_NATIVE_INT</td>
 * <td>int, Integer</td>
 * </tr>
 * <tr>
 * <td>@ref H5T_NATIVE_SHORT</td>
 * <td>short, Short</td>
 * </tr>
 * <tr>
 * <td>@ref H5T_NATIVE_FLOAT</td>
 * <td>float, Float</td>
 * </tr>
 * <tr>
 * <td>@ref H5T_NATIVE_DOUBLE</td>
 * <td>double, Double</td>
 * </tr>
 * <tr>
 * <td>@ref H5T_NATIVE_CHAR</td>
 * <td>byte, Byte</td>
 * </tr>
 * <tr>
 * <td>@ref H5T_C_S1</td>
 * <td>java.lang.String</td>
 * </tr>
 * <tr>
 * <td>void * <br />
 * (i.e., pointer to `Any')</td>
 * <td>Special -- see @ref HDFARRAY</td>
 * </tr>
 * </table>
 * <b>General Rules for Passing Arguments and Results</b>
 * <p>
 * In general, arguments passed <b>IN</b> to Java are the analogous basic types, as above. The exception is
 * for arrays, which are discussed below.
 * <p>
 * The <i>return value</i> of Java methods is also the analogous type, as above. A major exception to that
 * rule is that all HDF Java functions will raise an exception upon failure in the Java version,
 * rather than just return <i>int</i> as in the C. Functions that return a value are declared
 * equivalent to the C function.
 * However, in most cases the Java method will raise an exception instead of returning an error code.
 * @see @ref ERRORS.
 * <p>
 * Java does not support pass by reference of arguments, so arguments that are returned through <b>OUT</b>
 * parameters must be wrapped in an object or array. The Java API for HDF consistently wraps arguments in
 * arrays. Where possible the Java function may return the OUT parameter as an object or basic type.
 * <p>
 * For instance, a function that returns two integers declared as:
 *
 * <pre>
 *       h_err_t HDF5dummy( int *a1, int *a2)
 * </pre>
 *
 * For the Java interface, this would be declared:
 *
 * <pre>
 * public  static  int HDF5dummy(int args[]);
 * </pre>
 * OR
 * <pre>
 * public  static  int[] HDF5dummy();
 * </pre>
 *
 * where <i>a1</i> is <i>args[0]</i> and <i>a2</i> is <i>args[1]</i>, and would be invoked:
 *
 * <pre>
 * H5.HDF5dummy(a);
 * </pre>
 * OR
 * <pre>
 * a = H5.HDF5dummy();
 * </pre>
 *
 * <p>
 * All the routines where this convention is used will have specific documentation of the details, given
 * below.
 * <p>
 * <b>@ref HDFARRAY</b>
 * <p>
 * HDF5 needs to read and write multi-dimensional arrays of any number type (and records). The HDF5 API
 * describes the layout of the source and destination, and the data for the array passed as a block of
 * bytes, for instance,
 *
 * @code
 *      herr_t H5Dread(long fid, long filetype, long memtype, long memspace, void *data);
 * @endcode
 *
 * <p>
 * where ``void *'' means that the data may be any valid numeric type, and is a contiguous block of bytes that
 * is the data for a multi-dimensional array. The other parameters describe the dimensions, rank, and datatype
 * of the array ondisk (source) and in memory (destination).
 * <p>
 * For Java, this ``ANY'' is a problem, as the type of data must always be declared. Furthermore,
 * multidimensional arrays are definitely <i>not</i> laid out contiguously in memory. It would be infeasible
 * to declare a separate routine for every combination of number type and dimensionality. For that reason, the
 * @ref HDFARRAY <b>HDFArray</b> class is used to discover the type, shape, and
 * size of the data array at run time, and to convert to and from a contiguous array of bytes in
 * static  C order.
 * <p>
 * The upshot is that any Java array of numbers (either primitive or sub-classes of type <b>Number</b>) can be
 * passed as an ``Object'', and the Java API will translate to and from the appropriate packed array of bytes
 * needed by the C library. So the function above would be declared:
 *
 * @code
 *     public  static int H5Dread(long dataset_id, long mem_type_id, long mem_space_id,
 *                                            long file_space_id, long xfer_plist_id, Object obj,
 *                                            boolean isCriticalPinning)
 *       throws HDF5Exception, HDF5LibraryException, NullPointerException {}
 * @endcode
 *
 * and the parameter <i>data</i> can be any multi-dimensional array of numbers, such as float[][], or
 * int[][][], or Double[][].
 * <p>
 * <b>@ref HDF5CONST</b>
 * <p>
 * The HDF5 API defines a set of constants and enumerated values. Most of these values are available to Java
 * programs via the class @ref HDF5CONST <b>HDF5Constants</b></a>. For example,
 * the parameters for the h5open() call include two numeric values, <b><i>HDFConstants.H5F_ACC_RDWR</i></b>
 * and <b><i>HDF5Constants.H5P_DEFAULT</i></b>.
 * As would be expected, these numbers correspond to the C constants
 * <b><i>#H5F_ACC_RDWR</i></b> and <b><i>#H5P_DEFAULT</i></b>.
 * <p>
 * The HDF5 API defines a set of values that describe number types and sizes, such as "H5T_NATIVE_INT" and
 * "hsize_t". These values are determined at run time by the HDF5 C library. To support these parameters,
 * the Java HDFConstants class looks up the values
 * when initiated. The values can be accessed as public variables of the Java class, such as:
 *
 * @code
 * long data_type = HDFConstants.H5T_NATIVE_INT;
 * @endcode
 *
 * The Java application uses both types of constants the same way, the only difference is that the
 * <b><i>HDFConstants</i></b> may have different values on different platforms.
 * <p>
 * <b>@ref ERRORS</b>
 * <p>
 * The HDF5 error API (@ref H5E) manages the behavior of the error stack in the HDF5 library. This API is
 * omitted from the JHI5. Errors are converted into Java exceptions. This is totally different from the C
 * interface, but is very natural for Java programming. <p> The exceptions of the JHI5 are organized as
 * sub-classes of the class
 * @ref ERRORS <b>HDF5Exception</b>. There are two subclasses of
 * <b>HDF5Exception</b>, @ref ERRORSLIB <b>HDF5LibraryException</b>
 * and @ref ERRORSJAVA <b>HDF5JavaException</b>. The
 * sub-classes of the former represent errors from the HDF5 C library, while sub-classes of the latter
 * represent errors in the JHI5 wrapper and support code.
 * <p>
 * The super-class <b><i>HDF5LibraryException</i></b> implements the method '<b><i>printStackTrace()</i></b>',
 * which prints out the HDF5 error stack, as described in the HDF5 C API <i><b>@ref H5Eprint()</b>.</i> This
 * may be used by Java exception handlers to print out the HDF5 error stack. <hr>
 *
 * @version HDF5 2.0.0 <BR>
 *          <b>See also: </b>
 *          @ref HDFARRAY hdf.hdf5lib.HDFArray<br />
 *          @ref HDF5CONST hdf.hdf5lib.HDF5Constants<br />
 *          @ref ERRORS hdf.hdf5lib.HDF5Exception<br />
 *          <a href="https://hdfgroup.org/HDF5/">HDF5</a>
 *
 * For details of the HDF5 library, @see @ref RM
 */

/**
 * This class is the Java interface for the HDF5 library.
 *
 * @defgroup JH5 HDF5 Library Java Interface
 *
 * This code is the called by Java programs to access the entry points of the HDF5 library. Each routine wraps
 * a single HDF5 entry point, generally with the arguments and return codes analogous to the C interface.
 *
 * @see H5, C-API
 *
 * @see @ref H5_UG, User Guide
 *
 */
public class H5 implements java.io.Serializable {
    /**
     * Serialization ID
     */
    private static final long serialVersionUID = 6129888282117053288L;

    private final static Logger log = LoggerFactory.getLogger(H5.class);

    /**
     * @ingroup JH5
     *
     * The version number of the HDF5 library:
     * <ul>
     * <li>LIB_VERSION[0]: The major version of the library.</li>
     * <li>LIB_VERSION[1]: The minor version of the library.</li>
     * <li>LIB_VERSION[2]: The release number of the library.</li>
     * </ul>
     * Make sure to update the versions number when a different library is used.
     */
    public final static int LIB_VERSION[] = {2, 0, 0};

    private final static LinkedHashSet<Long> OPEN_IDS = new LinkedHashSet<Long>();
    private static boolean isLibraryLoaded            = false;

    private static MemorySegment errorFunc;
    private static MemorySegment errorData;
    private static Arena arena;

    static
    {
        loadH5Lib();
        arena     = Arena.ofAuto(); // Or Arena.ofAuto(), or Arena.ofConfined(), or Arena.global()
        errorFunc = arena.allocate(ValueLayout.ADDRESS);
        errorData = arena.allocate(ValueLayout.ADDRESS);
    }

    /**
     * @ingroup JH5
     *
     *  load native library
     */
    public static void loadH5Lib()
    {
        // Make sure that the library is loaded only once
        if (isLibraryLoaded)
            return;

        try {
            H5.H5open();
            isLibraryLoaded = true;
        }
        catch (Throwable err) {
            err.printStackTrace();
            isLibraryLoaded = false;
        }
        finally {
            log.info("HDF5 library: ");
            log.info((isLibraryLoaded ? "" : " NOT") + " successfully opened.");
        }

        /* Important! Exit quietly */
        try {
            H5.H5dont_atexit();
        }
        catch (HDF5LibraryException e) {
            System.exit(1);
        }

        /* Important! Disable error output to C stdout */
        if (!log.isDebugEnabled())
            H5.H5error_off();

        /*
         * Optional: confirm the version This will crash immediately if not the specified version.
         */
        Integer majnum = Integer.getInteger("hdf.hdf5lib.H5.hdf5maj", null);
        Integer minnum = Integer.getInteger("hdf.hdf5lib.H5.hdf5min", null);
        Integer relnum = Integer.getInteger("hdf.hdf5lib.H5.hdf5rel", null);
        if ((majnum != null) && (minnum != null) && (relnum != null)) {
            H5.H5check_version(majnum.intValue(), minnum.intValue(), relnum.intValue());
        }
    }

    // ////////////////////////////////////////////////////////////
    // //
    // H5: General Library Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * @ingroup JH5
     *
     * Get library error information.
     */
    public static void h5libraryError() throws HDF5LibraryException
    {
        HDF5LibraryException H5LibEx = new HDF5LibraryException();

        try (Arena arena = Arena.ofConfined()) {
            // Extract major and minor error numbers
            long majNum = H5LibEx.getMajorErrorNumber();
            long minNum = H5LibEx.getMinorErrorNumber();
            /* No error detected in HDF5 error stack. */
            if (majNum == 0 && minNum == 0)
                return;

            String errorMessage = "HDF5 Error: " + H5LibEx.getMessage();
            log.info("HDF5 Error: Major {}, Minor {}, Message: {}", majNum, minNum, errorMessage);
            HDF5LibraryException.throwHDF5LibraryException(majNum, errorMessage);
        }
    }

    /**
     * @ingroup JH5
     *
     * Get number of open IDs.
     *
     * @return Returns a count of open IDs
     */
    public final static int getOpenIDCount() { return OPEN_IDS.size(); }

    /**
     * @ingroup JH5
     *
     * Get the open IDs
     *
     * @return Returns a collection of open IDs
     */
    public final static Collection<Long> getOpenIDs() { return OPEN_IDS; }

    /**
     * @ingroup JH5
     *
     * FFM equivalent of h5str_detect_vlen function from JNI implementation.
     * Detects whether a datatype contains any variable length data or variable length strings.
     * This follows the same logic as the native JNI h5str_detect_vlen function.
     *
     * @param type_id
     *            IN: Datatype identifier to check for VL data
     *
     * @return true if datatype contains VL data; false otherwise
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    private static boolean detectVLData(long type_id) throws HDF5LibraryException
    {
        // Recursively detect any vlen data values in type (compound, array ...)
        if (H5Tdetect_class(type_id, HDF5Constants.H5T_VLEN)) {
            return true;
        }

        // Recursively detect any vlen string in type (compound, array ...)
        if (H5Tis_variable_str(type_id)) {
            return true;
        }

        return false;
    }

    /**
     * @ingroup JH5
     *
     * H5check_version verifies that the arguments match the version numbers compiled into the library.
     *
     * @param majnum
     *            The major version of the library.
     * @param minnum
     *            The minor version of the library.
     * @param relnum
     *            The release number of the library.
     * @return a non-negative value if successful. Upon failure (when the versions do not match), this
     *            function causes the application to abort (i.e., crash)
     *
     * See C API function: @ref herr_t H5check_version(unsigned majnum, unsigned minnum, unsigned relnum)
     **/
    public static int H5check_version(int majnum, int minnum, int relnum)
    {

        return org.hdfgroup.javahdf5.hdf5_h.H5check_version(majnum, minnum, relnum);
    }

    /**
     * @ingroup JH5
     *
     * H5close flushes all data to disk, closes all file identifiers, and cleans up all memory used by the
     * library.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5close() throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5close();
        if (retVal < 0)
            h5libraryError();
        return retVal;
    }

    /**
     * @ingroup JH5
     *
     * H5open initialize the library.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5open() throws HDF5LibraryException
    {
        int retVal = -1;

        if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5open()) < 0)
            h5libraryError();

        return retVal;
    }

    /**
     * @ingroup JH5
     *
     * H5dont_atexit indicates to the library that an atexit() cleanup routine should not be installed. In
     * order to be effective, this routine must be called before any other HDF function calls, and must be
     * called each time the library is loaded/linked into the application (the first time and after it's been
     * unloaded). <p> This is called by the static initializer, so this should never need to be explicitly
     * called by a Java program.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    private static int H5dont_atexit() throws HDF5LibraryException
    {
        int retVal = -1;

        if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5dont_atexit()) < 0)
            h5libraryError();

        return retVal;
    }

    /**
     * @ingroup JH5
     *
     * Turn off error handling. By default, the C library prints the error stack of the HDF5 C library on
     * stdout. This behavior may be disabled by calling H5error_off().
     *
     * @return a non-negative value if successful
     */
    public static int H5error_off()
    {
        // TODO: Implement error handling callbacks
        //         if (org.hdfgroup.javahdf5.hdf5_h.H5Eget_auto2(H5E_DEFAULT(), errorFunc, errorData) < 0)
        //             return -1;
        //
        //         if (org.hdfgroup.javahdf5.hdf5_h.H5Eset_auto2(H5E_DEFAULT(), MemorySegment.NULL,
        //         MemorySegment.NULL) < 0)
        //             return -1;

        return 0;
    }

    /**
     * @ingroup JH5
     *
     * Turn on error handling. By default, the C library prints the error stack of the HDF5 C library on
     * stdout. This behavior may be re-enabled by calling H5error_on().
     */
    public static void H5error_on()
    {
        org.hdfgroup.javahdf5.hdf5_h.H5Eset_auto2(H5E_DEFAULT(), errorFunc, errorData);
    }

    /**
     * @ingroup JH5
     *
     * H5garbage_collect collects on all free-lists of all types.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5garbage_collect() throws HDF5LibraryException
    {
        int retVal = -1;

        if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5garbage_collect()) < 0)
            h5libraryError();

        return retVal;
    }

    /**
     * @ingroup JH5
     *
     * H5get_libversion retrieves the major, minor, and release numbers of the version of the HDF library
     * which is linked to the application.
     *
     * @param libversion
     *            The version information of the HDF library.
     *
     * <pre>
     *      libversion[0] = The major version of the library.
     *      libversion[1] = The minor version of the library.
     *      libversion[2] = The release number of the library.
     * </pre>
     * @return a non-negative value if successful, along with the version information.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           If the libversion array is null or has less than 3 elements.
     **/
    public static int H5get_libversion(int[] libversion) throws HDF5LibraryException, NullPointerException
    {
        if (libversion == null || libversion.length < 3) {
            throw new NullPointerException("libversion array must be non-null and have at least 3 elements");
        }

        int retVal = -1;

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the array bytes
            MemorySegment majnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[0]);
            MemorySegment minnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[1]);
            MemorySegment relnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[2]);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5get_libversion(majnum_segment, minnum_segment,
                                                                        relnum_segment)) < 0)
                h5libraryError();

            // Set the version numbers
            libversion[0] = majnum_segment.get(ValueLayout.JAVA_INT, 0);
            libversion[1] = minnum_segment.get(ValueLayout.JAVA_INT, 0);
            libversion[2] = relnum_segment.get(ValueLayout.JAVA_INT, 0);
        }

        return retVal;
    }

    /**
     * @ingroup JH5
     *
     * H5set_free_list_limits
     *      Sets limits on the different kinds of free lists.  Setting a value
     *      of -1 for a limit means no limit of that type.  These limits are global
     *      for the entire library.  Each "global" limit only applies to free lists
     *      of that type, so if an application sets a limit of 1 MB on each of the
     *      global lists, up to 3 MB of total storage might be allocated (1MB on
     *      each of regular, array and block type lists).
     *
     *      The settings for block free lists are duplicated to factory free lists.
     *      Factory free list limits cannot be set independently currently.
     *
     * @param reg_global_lim
     *           The limit on all "regular" free list memory used
     * @param reg_list_lim
     *           The limit on memory used in each "regular" free list
     * @param arr_global_lim
     *           The limit on all "array" free list memory used
     * @param arr_list_lim
     *           The limit on memory used in each "array" free list
     * @param blk_global_lim
     *           The limit on all "block" free list memory used
     * @param blk_list_lim
     *           The limit on memory used in each "block" free list
     * @return a non-negative value if successful, along with the version information.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static int H5set_free_list_limits(int reg_global_lim, int reg_list_lim, int arr_global_lim,
                                             int arr_list_lim, int blk_global_lim, int blk_list_lim)
        throws HDF5LibraryException
    {
        int retVal = -1;
        if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5set_free_list_limits(reg_global_lim, reg_list_lim,
                                                                          arr_global_lim, arr_list_lim,
                                                                          blk_global_lim, blk_list_lim)) < 0)
            h5libraryError();

        return retVal;
    }

    /**
     * @ingroup JH5
     *
     * H5export_dataset is a utility function to save data in a file.
     *
     * @param file_export_name
     *            The file name to export data into.
     * @param file_id
     *            The identifier of the HDF5 file containing the dataset.
     * @param object_path
     *            The full path of the dataset to be exported.
     * @param binary_order
     *            99 - export data as text.
     *            1 - export data as binary Native Order.
     *            2 - export data as binary Little Endian.
     *            3 - export data as binary Big Endian.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5export_dataset(String file_export_name, long file_id, String object_path,
                                        int binary_order) throws HDF5LibraryException
    {
        throw new HDF5LibraryException("H5export_dataset not implemented yet");
    }

    /**
     * @ingroup JH5
     *
     * H5export_attribute is a utility function to save data in a file.
     *
     * @param file_export_name
     *            The file name to export data into.
     * @param dataset_id
     *            The identifier of the dataset containing the attribute.
     * @param attribute_name
     *            The attribute to be exported.
     * @param binary_order
     *            99 - export data as text.
     *            1 - export data as binary Native Order.
     *            2 - export data as binary Little Endian.
     *            3 - export data as binary Big Endian.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5export_attribute(String file_export_name, long dataset_id, String attribute_name,
                                          int binary_order) throws HDF5LibraryException
    {
        throw new HDF5LibraryException("H5export_attribute not implemented yet");
    }

    /**
     * @ingroup JH5
     *
     * H5is_library_threadsafe Checks to see if the library was built with thread-safety enabled.
     *
     * @return true if hdf5 library implements threadsafe
     *
     **/
    private static boolean H5is_library_threadsafe()
    {
        boolean is_ts = false;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the boolean value
            MemorySegment is_ts_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN);
            // Call the native function to check if the library is thread-safe
            if (org.hdfgroup.javahdf5.hdf5_h.H5is_library_threadsafe(is_ts_segment) < 0)
                h5libraryError();

            // Read the boolean value from the segment
            is_ts = is_ts_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }

        return is_ts;
    }

    /**
     * Helper method to copy a Java String into a fixed-size char array in a MemorySegment.
     * The string is null-terminated and truncated if it exceeds maxLen-1 characters.
     *
     * @param arena
     *            Arena for temporary allocations
     * @param str
     *            Java String to copy (null is treated as empty string)
     * @param dest
     *            Destination MemorySegment (fixed-size char array)
     * @param maxLen
     *            Maximum length of the char array (including null terminator)
     */
    private static void copyStringToCharArray(Arena arena, String str, MemorySegment dest, int maxLen)
    {
        // Handle null or empty strings
        if (str == null || str.isEmpty()) {
            // Write null terminator at position 0
            dest.set(ValueLayout.JAVA_BYTE, 0, (byte)0);
            return;
        }

        // Convert string to null-terminated C string
        MemorySegment srcSegment = arena.allocateFrom(str, StandardCharsets.UTF_8);
        long srcLen              = srcSegment.byteSize();

        // Calculate copy length (leave room for null terminator)
        long copyLen = Math.min(srcLen, maxLen - 1);

        // Copy string bytes
        MemorySegment.copy(srcSegment, 0, dest, 0, copyLen);

        // Ensure null terminator
        if (copyLen < maxLen) {
            dest.set(ValueLayout.JAVA_BYTE, copyLen, (byte)0);
        }
    }

    /**
     * Helper method to extract a Java String from a fixed-size null-terminated char array in a
     * MemorySegment.
     *
     * @param charArray
     *            Source MemorySegment containing null-terminated char array
     * @return Java String extracted from the char array
     */
    private static String extractStringFromCharArray(MemorySegment charArray)
    {
        // Find null terminator
        long length = 0;
        long maxLen = charArray.byteSize();
        while (length < maxLen && charArray.get(ValueLayout.JAVA_BYTE, length) != 0) {
            length++;
        }

        if (length == 0) {
            return "";
        }

        // Extract bytes up to null terminator
        byte[] bytes = new byte[(int)length];
        MemorySegment.copy(charArray, ValueLayout.JAVA_BYTE, 0, bytes, 0, (int)length);

        // Convert to Java String
        return new String(bytes, StandardCharsets.UTF_8);
    }

    // /////// unimplemented ////////
    //  herr_t H5free_memory(void *mem);
    //  SEE  int org.hdfgroup.javahdf5.hdf5_h.H5free_memory(MemorySegment mem)
    //  void *H5allocate_memory(size_t size, hbool_t clear);
    //  SEE  MemorySegment org.hdfgroup.javahdf5.hdf5_h.H5allocate_memory(long size, boolean clear)
    //  void *H5resize_memory(void *mem, size_t size);
    //  SEE  MemorySegment org.hdfgroup.javahdf5.hdf5_h.H5resize_memory(MemorySegment mem, long size)

    // ////////////////////////////////////////////////////////////
    // //
    // H5A: HDF5 Attribute Interface API Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5A Java Attribute (H5A) Interface
     *
     * An HDF5 attribute is a small metadata object describing the nature and/or intended usage of a primary
     *data object. A primary data object may be a dataset, group, or committed datatype.
     *
     * @see H5A, C-API
     *
     * @see @ref H5A_UG, User Guide
     **/

    /**
     * @ingroup JH5A
     *
     * H5Aclose terminates access to the attribute specified by its identifier, attr_id.
     *
     * @param attr_id
     *            IN: Attribute to release access to.
     *
     * @return a non-negative value if successful
     **/
    public static int H5Aclose(long attr_id)
    {
        log.trace("OPEN_IDS: H5Aclose remove {}", attr_id);
        OPEN_IDS.remove(attr_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Aclose(attr_id);
        return retVal;
    }

    /**
     * @ingroup JH5A
     *
     * H5Acopy copies the content of one attribute to another.
     *
     * @param src_aid
     *            the identifier of the source attribute
     * @param dst_aid
     *            the identifier of the destination attribute
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static int H5Acopy(long src_aid, long dst_aid) throws HDF5LibraryException
    {
        int retVal = -1;
        if (src_aid < 0 || dst_aid < 0) {
            throw new HDF5FunctionArgumentException("Source or destination attribute ID is negative");
        }
        throw new HDF5LibraryException("H5Acopy not implemented yet");
        // This is a stub implementation, as the actual implementation is not provided in the original code.
        // int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Acopy(src_aid, dst_aid);
        // if (retVal < 0) {
        //            h5libraryError();
        //}
        // return retVal;
    }

    /**
     * @ingroup JH5A
     *
     * H5Acreate creates an attribute, attr_name, which is attached to the object specified by the identifier
     * loc_id.
     *
     * @param loc_id
     *            IN: Location or object identifier; may be dataset or group
     * @param attr_name
     *            IN: Attribute name
     * @param type_id
     *            IN: Attribute datatype identifier
     * @param space_id
     *            IN: Attribute dataspace identifier
     * @param acpl_id
     *            IN: Attribute creation property list identifier
     * @param aapl_id
     *            IN: Attribute access property list identifier
     *
     * @return An attribute identifier if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            Name is null.
     **/
    public static long H5Acreate(long loc_id, String attr_name, long type_id, long space_id, long acpl_id,
                                 long aapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (attr_name == null)
            throw new NullPointerException("attr_name cannot be null");

        long attr_id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(attr_name);
            if ((attr_id = H5Acreate2(loc_id, name_segment, type_id, space_id, acpl_id, aapl_id)) < 0) {
                h5libraryError();
            }
        }
        if (attr_id > 0) {
            log.trace("OPEN_IDS: H5A create add {}", attr_id);
            OPEN_IDS.add(attr_id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }

        return attr_id;
    }

    /**
     * @ingroup JH5A
     *
     * H5Acreate_by_name creates an attribute, attr_name, which is attached to the object specified by loc_id
     * and obj_name.
     *
     * @param loc_id
     *            IN: Location or object identifier; may be dataset or group
     * @param obj_name
     *            IN: Name, relative to loc_id, of object that attribute is to be attached to
     * @param attr_name
     *            IN: Attribute name
     * @param type_id
     *            IN: Attribute datatype identifier
     * @param space_id
     *            IN: Attribute dataspace identifier
     * @param acpl_id
     *            IN: Attribute creation property list identifier (currently not used).
     * @param aapl_id
     *            IN: Attribute access property list identifier (currently not used).
     * @param lapl_id
     *            IN: Link access property list
     *
     * @return An attribute identifier if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Acreate_by_name(long loc_id, String obj_name, String attr_name, long type_id,
                                         long space_id, long acpl_id, long aapl_id, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (attr_name == null)
            throw new NullPointerException("attr_name cannot be null");
        if (obj_name == null)
            throw new NullPointerException("obj_name cannot be null");

        long attr_id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
            MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
            attr_id                         = org.hdfgroup.javahdf5.hdf5_h.H5Acreate_by_name(
                loc_id, obj_name_segment, attr_name_segment, type_id, space_id, acpl_id, aapl_id, lapl_id);
        }
        if (attr_id > 0) {
            log.trace("OPEN_IDS: H5Acreate_by_name add {}", attr_id);
            OPEN_IDS.add(attr_id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();
        return attr_id;
    }

    /**
     * @ingroup JH5A
     *
     * H5Adelete removes the attribute specified by its name, name, from a dataset, group, or named datatype.
     *
     * @param loc_id
     *            IN: Identifier of the dataset, group, or named datatype.
     * @param name
     *            IN: Name of the attribute to delete.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Adelete(long loc_id, String name) throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("Attribute name cannot be null");
        }

        int retVal = -1;

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            retVal                     = org.hdfgroup.javahdf5.hdf5_h.H5Adelete(loc_id, name_segment);
        }

        if (retVal < 0) {
            h5libraryError();
        }

        return retVal;
    }

    /**
     * @ingroup JH5A
     *
     * H5Adelete_by_idx removes an attribute, specified by its location in an index, from an object.
     *
     * @param loc_id
     *            IN: Location or object identifier; may be dataset or group
     * @param obj_name
     *            IN: Name of object, relative to location, from which attribute is to be removed
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order in which to iterate over index
     * @param n
     *            IN: Offset within index
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            obj_name is null.
     **/
    public static void H5Adelete_by_idx(long loc_id, String obj_name, int idx_type, int order, long n,
                                        long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (obj_name == null) {
            throw new NullPointerException("Object name cannot be null");
        }

        int retVal = -1;

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment obj_name_segment = arena.allocateFrom(obj_name);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Adelete_by_idx(loc_id, obj_name_segment, idx_type, order,
                                                                   n, lapl_id);
        }

        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5A
     *
     * H5Adelete_by_name removes the attribute attr_name from an object specified by location and name, loc_id
     * and obj_name, respectively.
     *
     * @param loc_id
     *            IN: Location or object identifier; may be dataset or group
     * @param obj_name
     *            IN: Name of object, relative to location, from which attribute is to be removed
     * @param attr_name
     *            IN: Name of attribute to delete
     * @param lapl_id
     *            IN: Link access property list identifier.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Adelete_by_name(long loc_id, String obj_name, String attr_name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (obj_name == null || attr_name == null) {
            throw new NullPointerException("Object name or attribute name cannot be null");
        }

        int retVal = -1;

        try (Arena arena = Arena.ofConfined()) {
            // Allocate MemorySegments to hold the string bytes
            MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
            MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Adelete_by_name(loc_id, obj_name_segment,
                                                                    attr_name_segment, lapl_id);
        }

        if (retVal < 0) {
            h5libraryError();
        }

        return retVal;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aexists determines whether the attribute attr_name exists on the object specified by obj_id.
     *
     * @param obj_id
     *            IN: Object identifier.
     * @param attr_name
     *            IN: Name of the attribute.
     *
     * @return boolean true if an attribute with a given name exists.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            attr_name is null.
     **/
    public static boolean H5Aexists(long obj_id, String attr_name)
        throws HDF5LibraryException, NullPointerException
    {
        if (attr_name == null) {
            throw new NullPointerException("Attribute name cannot be null");
        }

        boolean exists = false;
        try (Arena arena = Arena.ofConfined()) {
            int retVal = -1;

            // Allocate a MemorySegment to hold the string bytes
            MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Aexists(obj_id, attr_name_segment)) < 0)
                h5libraryError();
            if (retVal > 0)
                exists = true;
            else
                exists = false;
        }

        return exists;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aexists_by_name determines whether the attribute attr_name exists on an object. That object is
     * specified by its location and name, loc_id and obj_name, respectively.
     *
     * @param loc_id
     *            IN: Location of object to which attribute is attached .
     * @param obj_name
     *            IN: Name, relative to loc_id, of object that attribute is attached to.
     * @param attr_name
     *            IN: Name of attribute.
     * @param lapl_id
     *            IN: Link access property list identifier.
     *
     * @return boolean true if an attribute with a given name exists, otherwise returns false.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static boolean H5Aexists_by_name(long loc_id, String obj_name, String attr_name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;

        if (obj_name == null || attr_name == null) {
            throw new NullPointerException("Object name or attribute name cannot be null");
        }

        boolean exists = false;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate MemorySegments to hold the string bytes
            MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
            MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Aexists_by_name(loc_id, obj_name_segment,
                                                                    attr_name_segment, lapl_id);
        }
        if (retVal < 0) {
            h5libraryError();
        }
        else if (retVal > 0) {
            exists = true;
        }
        else {
            exists = false;
        }

        return exists;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_info retrieves attribute information, by attribute identifier.
     *
     * @param attr_id
     *            IN: Attribute identifier
     *
     * @return A buffer(H5A_info_t) for Attribute information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static hdf.hdf5lib.structs.H5A_info_t H5Aget_info(long attr_id) throws HDF5LibraryException
    {
        hdf.hdf5lib.structs.H5A_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ainfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5A_info_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Aget_info(attr_id, ainfo_segment) < 0)
                h5libraryError();

            // Unpack the H5A_info_t from the MemorySegment
            info = new hdf.hdf5lib.structs.H5A_info_t(
                org.hdfgroup.javahdf5.H5A_info_t.corder_valid(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.corder(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.cset(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.data_size(ainfo_segment));
        }
        return info;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_info_by_idx Retrieves attribute information, by attribute index position.
     *
     * @param loc_id
     *            IN: Location of object to which attribute is attached
     * @param obj_name
     *            IN: Name of object to which attribute is attached, relative to location
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Index traversal order
     * @param n
     *            IN: Attribute's position in index
     * @param lapl_id
     *            IN: Link access property list
     *
     * @return A buffer(H5A_info_t) for Attribute information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            obj_name is null.
     **/
    public static hdf.hdf5lib.structs.H5A_info_t
    H5Aget_info_by_idx(long loc_id, String obj_name, int idx_type, int order, long n, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (obj_name == null) {
            throw new NullPointerException("Object name cannot be null");
        }

        hdf.hdf5lib.structs.H5A_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ainfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5A_info_t.sizeof());
            // Allocate MemorySegments to hold the string bytes
            MemorySegment obj_name_segment = arena.allocateFrom(obj_name);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Aget_info_by_idx(loc_id, obj_name_segment, idx_type, order, n,
                                                                ainfo_segment, lapl_id) < 0)
                h5libraryError();

            // Unpack the H5A_info_t from the MemorySegment
            info = new hdf.hdf5lib.structs.H5A_info_t(
                org.hdfgroup.javahdf5.H5A_info_t.corder_valid(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.corder(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.cset(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.data_size(ainfo_segment));
        }

        return info;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_info_by_name Retrieves attribute information, by attribute name.
     *
     * @param loc_id
     *            IN: Location of object to which attribute is attached
     * @param obj_name
     *            IN: Name of object to which attribute is attached, relative to location
     * @param attr_name
     *            IN: Attribute name
     * @param lapl_id
     *            IN: Link access property list
     *
     * @return A buffer(H5A_info_t) for Attribute information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            obj_name is null.
     **/
    public static hdf.hdf5lib.structs.H5A_info_t H5Aget_info_by_name(long loc_id, String obj_name,
                                                                     String attr_name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (obj_name == null || attr_name == null) {
            throw new NullPointerException("Object name or attribute name cannot be null");
        }

        hdf.hdf5lib.structs.H5A_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ainfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5A_info_t.sizeof());
            // Allocate MemorySegments to hold the string bytes
            MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
            MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Aget_info_by_name(loc_id, obj_name_segment, attr_name_segment,
                                                                 ainfo_segment, lapl_id) < 0)
                h5libraryError();

            // Unpack the H5A_info_t from the MemorySegment
            info = new hdf.hdf5lib.structs.H5A_info_t(
                org.hdfgroup.javahdf5.H5A_info_t.corder_valid(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.corder(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.cset(ainfo_segment),
                org.hdfgroup.javahdf5.H5A_info_t.data_size(ainfo_segment));
        }

        return info;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_name retrieves the name of an attribute specified by the identifier, attr_id.
     *
     * @param attr_id
     *            IN: Identifier of the attribute.
     *
     * @return String for Attribute name.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5Aget_name(long attr_id) throws HDF5LibraryException
    {
        long buf_size = -1;

        if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Aget_name(attr_id, 0, MemorySegment.NULL)) < 0)
            h5libraryError();

        String ret_name = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocate(buf_size + 1);
            /* Get the attribute name */
            if (org.hdfgroup.javahdf5.hdf5_h.H5Aget_name(attr_id, buf_size + 1, name_segment) < 0)
                h5libraryError();

            ret_name = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return ret_name;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_name_by_idx retrieves the name of an attribute that is attached to an object, which is specified
     * by its location and name, loc_id and obj_name, respectively.
     *
     * @param attr_id
     *            IN: Attribute identifier
     * @param obj_name
     *            IN: Name of object to which attribute is attached, relative to location
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Index traversal order
     * @param n
     *            IN: Attribute's position in index
     * @param lapl_id
     *            IN: Link access property list
     *
     * @return String for Attribute name.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            obj_name is null.
     **/
    public static String H5Aget_name_by_idx(long attr_id, String obj_name, int idx_type, int order, long n,
                                            long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        long status_size = -1;

        if (obj_name == null) {
            throw new NullPointerException("Object name cannot be null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment obj_name_segment = arena.allocateFrom(obj_name);
            if ((status_size = org.hdfgroup.javahdf5.hdf5_h.H5Aget_name_by_idx(
                     attr_id, obj_name_segment, idx_type, order, n, MemorySegment.NULL, 0, lapl_id)) < 0)
                h5libraryError();
        }

        String ret_name = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment obj_name_segment = arena.allocateFrom(obj_name);
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocate(status_size + 1);
            /* Get the attribute name */
            if (org.hdfgroup.javahdf5.hdf5_h.H5Aget_name_by_idx(attr_id, obj_name_segment, idx_type, order, n,
                                                                name_segment, status_size + 1, lapl_id) < 0)
                h5libraryError();

            ret_name = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return ret_name;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_space retrieves a copy of the dataspace for an attribute.
     *
     * @param attr_id
     *            IN: Identifier of an attribute.
     *
     * @return attribute dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Aget_space(long attr_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Aget_space(attr_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aget_space add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_storage_size returns the amount of storage that is required for the specified attribute,
     * attr_id.
     *
     * @param attr_id
     *            IN: Identifier of the attribute to query.
     *
     * @return the amount of storage size allocated for the attribute; otherwise returns 0 (zero)
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Aget_storage_size(long attr_id) throws HDF5LibraryException
    {
        long size = org.hdfgroup.javahdf5.hdf5_h.H5Aget_storage_size(attr_id);
        if (size == 0) {
            h5libraryError();
        }

        return size;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_type retrieves a copy of the datatype for an attribute.
     *
     * @param attr_id
     *            IN: Identifier of an attribute.
     *
     * @return a datatype identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Aget_type(long attr_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Aget_type(attr_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aget_type add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aopen opens an existing attribute, attr_name, that is attached to an object specified an object
     * identifier, object_id.
     *
     * @param obj_id
     *            IN: Identifier for object to which attribute is attached
     * @param attr_name
     *            IN: Name of attribute to open
     * @param aapl_id
     *            IN: Attribute access property list identifier
     *
     * @return An attribute identifier if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            Name is null.
     **/
    public static long H5Aopen(long obj_id, String attr_name, long aapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (attr_name == null) {
            throw new NullPointerException("Attribute name cannot be null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(attr_name);
            id                         = org.hdfgroup.javahdf5.hdf5_h.H5Aopen(obj_id, name_segment, aapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aopen_by_idx opens an existing attribute that is attached to an object specified by location and
     * name, loc_id and obj_name, respectively
     *
     * @param loc_id
     *            IN: Location of object to which attribute is attached
     * @param obj_name
     *            IN: Name of object to which attribute is attached, relative to location
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Index traversal order
     * @param n
     *            IN: Attribute's position in index
     * @param aapl_id
     *            IN: Attribute access property list
     * @param lapl_id
     *            IN: Link access property list
     *
     * @return An attribute identifier if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            Name is null.
     **/
    public static long H5Aopen_by_idx(long loc_id, String obj_name, int idx_type, int order, long n,
                                      long aapl_id, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (obj_name == null) {
            throw new NullPointerException("Object name cannot be null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(obj_name);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Aopen_by_idx(loc_id, name_segment, idx_type, order, n,
                                                             aapl_id, lapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aopen_by_idx add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aopen_by_name Opens an attribute for an object by object name and attribute name
     *
     * @param loc_id
     *            IN: Location from which to find object to which attribute is attached
     * @param obj_name
     *            IN: Name of object to which attribute is attached, relative to loc_id
     * @param attr_name
     *            IN: Name of attribute to open
     * @param aapl_id
     *            IN: Attribute access property list
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return Returns an attribute identifier if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            obj_name is null.
     **/
    public static long H5Aopen_by_name(long loc_id, String obj_name, String attr_name, long aapl_id,
                                       long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (obj_name == null || attr_name == null) {
            throw new NullPointerException("Object name or attribute name cannot be null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
            MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Aopen_by_name(loc_id, obj_name_segment, attr_name_segment,
                                                              aapl_id, lapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aopen_by_name add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param obj
     *            Buffer to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread(long attr_id, long mem_type_id, byte[] obj, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (obj == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the byte array
            MemorySegment obj_segment = arena.allocate(ValueLayout.JAVA_BYTE, obj.length);
            if (isCriticalPinning) {
                obj_segment.copyFrom(MemorySegment.ofArray(obj));
            }
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, obj_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(obj_segment, ValueLayout.JAVA_BYTE, 0L, obj, 0, obj.length);
        }

        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread(long attr_id, long mem_type_id, byte[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Aread(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param obj
     *            Buffer to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread(long attr_id, long mem_type_id, Object obj)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        return H5Aread(attr_id, mem_type_id, obj, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into data object from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param obj
     *            IN: Object for data to be read.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Failure in the data conversion.
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null. See public  static  int H5Aread( )
     **/
    public static int H5Aread(long attr_id, long mem_type_id, Object obj, boolean isCriticalPinning)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        int status   = -1;
        boolean is1D = false;

        Class dataClass = obj.getClass();
        if (!dataClass.isArray()) {
            throw new HDF5JavaException("data is not an array");
        }

        String cname = dataClass.getName();
        is1D         = (cname.lastIndexOf('[') == cname.indexOf('['));
        char dname   = cname.charAt(cname.lastIndexOf("[") + 1);
        log.trace("H5Aread: cname={} is1D={} dname={}", cname, is1D, dname);

        if (is1D && (dname == 'B')) {
            log.trace("H5Aread_dname_B");
            status = H5Aread(attr_id, mem_type_id, (byte[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'S')) {
            log.trace("H5Aread_dname_S");
            status = H5Aread_short(attr_id, mem_type_id, (short[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'I')) {
            log.trace("H5Aread_dname_I");
            status = H5Aread_int(attr_id, mem_type_id, (int[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'J')) {
            log.trace("H5Aread_dname_J");
            status = H5Aread_long(attr_id, mem_type_id, (long[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'F')) {
            log.trace("H5Aread_dname_F");
            status = H5Aread_float(attr_id, mem_type_id, (float[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'D')) {
            log.trace("H5Aread_dname_D");
            status = H5Aread_double(attr_id, mem_type_id, (double[])obj, isCriticalPinning);
        }
        else if ((H5.H5Tdetect_class(mem_type_id, HDF5Constants.H5T_REFERENCE) &&
                  (is1D && (dataClass.getComponentType() == String.class))) ||
                 H5.H5Tequal(mem_type_id, HDF5Constants.H5T_STD_REF_DSETREG)) {
            log.trace("H5Aread_reg_ref");
            status = H5Aread(attr_id, mem_type_id, (String[])obj);
        }
        else if (is1D && (dataClass.getComponentType() == String.class)) {
            log.trace("H5Aread_string type");
            status = H5Aread(attr_id, mem_type_id, (String[])obj);
        }
        else if (H5.H5Tget_class(mem_type_id) == HDF5Constants.H5T_VLEN) {
            log.trace("H5AreadVL type - using H5AreadVL directly");
            status = H5AreadVL(attr_id, mem_type_id, (Object[])obj);
        }
        else {
            // Create a data buffer to hold the data into a Java Array
            HDFArray theArray = new HDFArray(obj);
            byte[] buf        = theArray.emptyBytes();
            log.trace("H5Aread_else");

            // This will raise an exception if there is an error
            status = H5Aread(attr_id, mem_type_id, buf, isCriticalPinning);

            // No exception: status really ought to be OK
            if (status >= 0) {
                obj = theArray.arrayify(buf);
            }

            // clean up these: assign 'null' as hint to gc()
            buf      = null;
            theArray = null;
        }

        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of double from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of double to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_double(long attr_id, long mem_type_id, double[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the double array
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_DOUBLE, buf.length);
            if (isCriticalPinning) {
                buf_segment.copyFrom(MemorySegment.ofArray(buf));
            }
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, buf_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_DOUBLE, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of double from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of double to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_double(long attr_id, long mem_type_id, double[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Aread_double(attr_id, mem_type_id, buf, true);
    }

    /**
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of float from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of float to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_float(long attr_id, long mem_type_id, float[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the float array
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_FLOAT, buf.length);
            if (isCriticalPinning) {
                buf_segment.copyFrom(MemorySegment.ofArray(buf));
            }
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, buf_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_FLOAT, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of float from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of float to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_float(long attr_id, long mem_type_id, float[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Aread_float(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of int from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of int to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_int(long attr_id, long mem_type_id, int[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the int array
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_INT, buf.length);
            if (isCriticalPinning) {
                buf_segment.copyFrom(MemorySegment.ofArray(buf));
            }
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, buf_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_INT, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of int from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of int to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_int(long attr_id, long mem_type_id, int[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Aread_int(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of long from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of long to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_long(long attr_id, long mem_type_id, long[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the long array
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_LONG, buf.length);
            if (isCriticalPinning) {
                buf_segment.copyFrom(MemorySegment.ofArray(buf));
            }
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, buf_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_LONG, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of long from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of long to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_long(long attr_id, long mem_type_id, long[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Aread_long(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of String from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of String to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_reg_ref(long attr_id, long mem_type_id, String[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Aread_reg_ref not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of short from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of short to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_short(long attr_id, long mem_type_id, short[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the short array
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_SHORT, buf.length);
            if (isCriticalPinning) {
                buf_segment.copyFrom(MemorySegment.ofArray(buf));
            }
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, buf_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_SHORT, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer  of shortfrom the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of short to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_short(long attr_id, long mem_type_id, short[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Aread_short(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of variable-lenght from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of variable-lenght to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5AreadVL(long attr_id, long mem_type_id, Object[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }

        int status            = -1;
        boolean vl_data_class = false;
        // CRITICAL FIX: Use global Arena to prevent automatic cleanup conflicts with HDF5 VL memory
        Arena arena = Arena.global();

        try {
            // Detect VL data to determine if H5Treclaim is needed (JNI pattern)
            vl_data_class = detectVLData(mem_type_id);
            // Check the datatype class to determine reading strategy
            int typeClass = H5Tget_class(mem_type_id);

            if (typeClass == HDF5Constants.H5T_COMPOUND) {
                // For compound datatypes, read heterogeneous field structures
                ArrayList[] result = VLDataConverter.readCompoundDatatype(attr_id, mem_type_id, buf.length,
                                                                          arena, false, -1, -1, -1);
                System.arraycopy(result, 0, buf, 0, buf.length);
                status = 0; // Success
            }
            else if (typeClass == HDF5Constants.H5T_ARRAY) {
                // For array datatypes, read directly into array buffer (not hvl_t)
                ArrayList[] result =
                    VLDataConverter.readArrayDatatype(attr_id, mem_type_id, buf.length, arena);
                System.arraycopy(result, 0, buf, 0, buf.length);
                status = 0; // Success
            }
            else if (typeClass == HDF5Constants.H5T_STRING && H5Tis_variable_str(mem_type_id)) {
                // CRITICAL FIX: For variable-length string datatypes, use string pointer array
                // to match the write path format - this fixes the off-by-one indexing issue
                MemorySegment stringArray = arena.allocate(ValueLayout.ADDRESS, buf.length);

                // Call native H5Aread to read string pointers
                status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, stringArray);

                if (status >= 0) {
                    // Convert string pointer array back to ArrayList array
                    for (int i = 0; i < buf.length; i++) {
                        ArrayList<String> stringList = new ArrayList<>();
                        MemorySegment stringPtr      = stringArray.getAtIndex(ValueLayout.ADDRESS, i);

                        if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                            // Reinterpret the string pointer with proper scope for reading
                            // Use a large but safe size limit for string reading
                            MemorySegment boundedStringPtr =
                                stringPtr.reinterpret(4096, Arena.global(), null);
                            String str =
                                boundedStringPtr.getString(0, java.nio.charset.StandardCharsets.UTF_8);
                            stringList.add(str);
                        }
                        else {
                            stringList.add(""); // Empty string for null pointers
                        }
                        buf[i] = stringList;
                    }
                }
            }
            else {
                // For VL datatypes, use hvl_t structures
                MemorySegment hvlArray = hvl_t.allocateArray(buf.length, arena);

                // Call native H5Aread
                status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, hvlArray);

                if (status >= 0) {
                    // Convert hvl_t data back to ArrayList array IMMEDIATELY while memory is valid
                    ArrayList[] result = VLDataConverter.convertFromHVL(hvlArray, buf.length, mem_type_id);

                    // Get dataspace for H5Treclaim
                    long space_id = HDF5Constants.H5I_INVALID_HID;

                    // Reclaim VL memory allocated by HDF5 only if VL data is detected (JNI pattern)
                    try {
                        // Get dataspace for H5Treclaim
                        space_id = org.hdfgroup.javahdf5.hdf5_h.H5Aget_space(attr_id);
                        if ((status >= 0) && vl_data_class) {
                            org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(
                                mem_type_id, space_id, org.hdfgroup.javahdf5.hdf5_h.H5P_DEFAULT(), hvlArray);
                        }
                    }
                    finally {
                        if (space_id >= 0) {
                            org.hdfgroup.javahdf5.hdf5_h.H5Sclose(space_id);
                        }
                    }

                    System.arraycopy(result, 0, buf, 0, buf.length);
                }
                else {
                    h5libraryError();
                }
            }
        }
        catch (HDF5JavaException ex) {
            throw new HDF5LibraryException("VL data conversion failed: " + ex.getMessage());
        }

        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of String from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of String to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Aread_string(long attr_id, long mem_type_id, String[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Aread_string not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread_VLStrings reads an attribute, specified with attr_id. The attribute's memory datatype is
     * specified with mem_type_id. The entire attribute is read into buffer of variable-length strings from
     *the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory, must be variable-length string type).
     * @param buf
     *            OUT: Buffer of variable-length strings to store data read from the file.
     *                 Must be pre-allocated with sufficient size.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     * @exception IllegalArgumentException
     *            buf is too small or type is not variable-length string.
     *
     * @note Uses Arena.global() for memory management
     * @note H5Treclaim is called automatically to free HDF5-managed VL memory
     * @note Buffer must be pre-allocated with size matching attribute dataspace
     *
     * @example
     * <pre>
     * // Create variable-length string type
     * long strType = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
     * H5.H5Tset_size(strType, HDF5Constants.H5T_VARIABLE);
     *
     * // Determine buffer size from attribute dataspace
     * long space_id = H5.H5Aget_space(attr_id);
     * long[] dims = new long[1];
     * H5.H5Sget_simple_extent_dims(space_id, dims, null);
     *
     * // Read VL strings
     * String[] buffer = new String[(int)dims[0]];
     * H5.H5Aread_VLStrings(attr_id, strType, buffer);
     *
     * // Clean up
     * H5.H5Sclose(space_id);
     * H5.H5Tclose(strType);
     * </pre>
     **/
    public static int H5Aread_VLStrings(long attr_id, long mem_type_id, Object[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        // Input validation
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }

        // Type validation - ensure it's a variable-length string type
        if (!H5.H5Tis_variable_str(mem_type_id)) {
            throw new HDF5LibraryException("Type is not a variable-length string");
        }

        if (log.isTraceEnabled()) {
            log.trace("H5Aread_VLStrings: attr_id={}, mem_type_id={}, buffer size={}", attr_id, mem_type_id,
                      buf.length);
        }

        // Get attribute dataspace to determine size
        long space_id = HDF5Constants.H5I_INVALID_HID;
        long[] dims   = new long[1];
        int status    = -1;

        try {
            space_id = H5.H5Aget_space(attr_id);
            if (space_id < 0) {
                throw new HDF5LibraryException("Failed to get attribute dataspace");
            }

            int rank = H5.H5Sget_simple_extent_ndims(space_id);
            if (rank > 0) {
                long[] fullDims = new long[rank];
                H5.H5Sget_simple_extent_dims(space_id, fullDims, null);

                // Calculate total size
                long totalSize = 1;
                for (long dim : fullDims) {
                    totalSize *= dim;
                }

                // Buffer size validation
                if (buf.length < totalSize) {
                    throw new IllegalArgumentException("Buffer too small: " + buf.length + " < " + totalSize);
                }
                dims[0] = totalSize;
            }
            else {
                // Scalar attribute
                dims[0] = 1;
            }

            if (log.isTraceEnabled()) {
                log.trace("H5Aread_VLStrings: reading {} elements", dims[0]);
            }

            // Use global Arena to prevent automatic cleanup conflicts with HDF5 VL memory
            Arena arena = Arena.global();

            // Allocate hvl_t array for reading
            MemorySegment hvlArray = org.hdfgroup.javahdf5.hvl_t.allocateArray((int)dims[0], arena);

            // Read from HDF5
            status = org.hdfgroup.javahdf5.hdf5_h.H5Aread(attr_id, mem_type_id, hvlArray);

            if (status < 0) {
                h5libraryError();
            }

            // Convert from hvl_t to Java ArrayList[]
            ArrayList[] vlResult = VLDataConverter.convertFromHVL(hvlArray, (int)dims[0], mem_type_id);

            // Extract strings from ArrayList[] to String[]
            for (int i = 0; i < dims[0] && i < buf.length; i++) {
                if (vlResult[i] != null && !vlResult[i].isEmpty()) {
                    buf[i] = (String)vlResult[i].get(0);
                }
                else {
                    buf[i] = "";
                }
            }

            if (log.isTraceEnabled()) {
                log.trace("H5Aread_VLStrings: successfully read {} strings, status={}", dims[0], status);
            }

            // CRITICAL: Reclaim VL memory managed by HDF5
            // This must be called to prevent memory leaks
            int reclaim_status = org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(
                mem_type_id, space_id, org.hdfgroup.javahdf5.hdf5_h.H5P_DEFAULT(), hvlArray);
            if (reclaim_status < 0 && log.isTraceEnabled()) {
                log.trace("H5Aread_VLStrings: H5Treclaim returned {}", reclaim_status);
            }
        }
        catch (HDF5JavaException ex) {
            throw new HDF5LibraryException("VL string read failed: " + ex.getMessage());
        }
        finally {
            // Clean up dataspace
            if (space_id >= 0) {
                try {
                    H5.H5Sclose(space_id);
                }
                catch (Exception ex) {
                    if (log.isTraceEnabled()) {
                        log.trace("H5Aread_VLStrings: H5Sclose failed: {}", ex.getMessage());
                    }
                }
            }
        }

        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buffer of string from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            Buffer of string to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5AreadComplex(long attr_id, long mem_type_id, String[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5AreadComplex not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Arename changes the name of attribute that is attached to the object specified by loc_id. The
     * attribute named old_attr_name is renamed new_attr_name.
     *
     * @param loc_id
     *            IN: Location or object identifier; may be dataset or group
     * @param old_attr_name
     *            IN: Prior attribute name
     * @param new_attr_name
     *            IN: New attribute name
     *
     * @return A non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            Name is null.
     **/
    public static int H5Arename(long loc_id, String old_attr_name, String new_attr_name)
        throws HDF5LibraryException, NullPointerException
    {
        int status = -1;
        if (old_attr_name == null || new_attr_name == null) {
            throw new NullPointerException("old_attr_name or new_attr_name is null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate MemorySegments for the old and new attribute names
            MemorySegment old_attr_name_segment = arena.allocateFrom(old_attr_name);
            MemorySegment new_attr_name_segment = arena.allocateFrom(new_attr_name);

            status =
                org.hdfgroup.javahdf5.hdf5_h.H5Arename(loc_id, old_attr_name_segment, new_attr_name_segment);
        }
        if (status < 0) {
            h5libraryError();
        }
        log.trace("H5Arename: {} renamed to {}", old_attr_name, new_attr_name);
        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Arename_by_name changes the name of attribute that is attached to the object specified by loc_id and
     * obj_name. The attribute named old_attr_name is renamed new_attr_name.
     *
     * @param loc_id
     *            IN: Location or object identifier; may be dataset or group
     * @param obj_name
     *            IN: Name of object, relative to location, whose attribute is to be renamed
     * @param old_attr_name
     *            IN: Prior attribute name
     * @param new_attr_name
     *            IN: New attribute name
     * @param lapl_id
     *            IN: Link access property list
     *
     * @return A non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            Name is null.
     **/
    public static int H5Arename_by_name(long loc_id, String obj_name, String old_attr_name,
                                        String new_attr_name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        int status = -1;
        if (obj_name == null || old_attr_name == null || new_attr_name == null) {
            throw new NullPointerException(
                "H5Arename_by_name: obj_name, old_attr_name or new_attr_name is null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate MemorySegments for the object name and attribute names
            MemorySegment obj_name_segment      = arena.allocateFrom(obj_name);
            MemorySegment old_attr_name_segment = arena.allocateFrom(old_attr_name);
            MemorySegment new_attr_name_segment = arena.allocateFrom(new_attr_name);

            status = org.hdfgroup.javahdf5.hdf5_h.H5Arename_by_name(
                loc_id, obj_name_segment, old_attr_name_segment, new_attr_name_segment, lapl_id);
        }
        if (status < 0) {
            h5libraryError();
        }
        log.trace("H5Arename_by_name: {} renamed to {}", old_attr_name, new_attr_name);
        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buf to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer with data to be written to the file.
     * @param isCriticalPinning
     *            IN: request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite(long attr_id, long mem_type_id, byte[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data is null");
        }
        int status = -1;

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment for the data buffer
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_BYTE, buf.length);
            buf_segment.copyFrom(MemorySegment.ofArray(buf));

            status = org.hdfgroup.javahdf5.hdf5_h.H5Awrite(attr_id, mem_type_id, buf_segment);
            if (status < 0) {
                h5libraryError();
            }
        }

        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buf to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite(long attr_id, long mem_type_id, byte[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Awrite(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buf to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param obj
     *            IN: Buffer with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite(long attr_id, long mem_type_id, Object obj)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        return H5Awrite(attr_id, mem_type_id, obj, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from data object to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param obj
     *            IN: Data object to be written.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Failure in the data conversion.
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data object is null
     **/
    public static int H5Awrite(long attr_id, long mem_type_id, Object obj, boolean isCriticalPinning)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        int status   = -1;
        boolean is1D = false;

        Class dataClass = obj.getClass();
        if (!dataClass.isArray()) {
            throw new HDF5JavaException("data is not an array");
        }

        String cname = dataClass.getName();
        is1D         = (cname.lastIndexOf('[') == cname.indexOf('['));
        char dname   = cname.charAt(cname.lastIndexOf("[") + 1);

        if (is1D && (dataClass.getComponentType() == String.class)) {
            log.trace("H5Awrite_string type - routing to H5Awrite_VLStrings");
            status = H5Awrite_VLStrings(attr_id, mem_type_id, (String[])obj);
        }
        else if (H5.H5Tget_class(mem_type_id) == HDF5Constants.H5T_VLEN) {
            log.trace("H5AwriteVL type - using H5AwriteVL directly");
            status = H5AwriteVL(attr_id, mem_type_id, (Object[])obj);
        }
        else {
            HDFArray theArray = new HDFArray(obj);
            byte[] buf        = theArray.byteify();

            status   = H5Awrite(attr_id, mem_type_id, buf);
            buf      = null;
            theArray = null;
        }

        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of double to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of double with data to be written to the file.
     * @param isCriticalPinning
     *            IN: request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *             Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_double(long attr_id, long mem_type_id, double[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Awrite_double not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of double to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of double with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_double(long attr_id, long mem_type_id, double[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Awrite_double(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of float to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of float with data to be written to the file.
     * @param isCriticalPinning
     *            IN: request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_float(long attr_id, long mem_type_id, float[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Awrite_float not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of float to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of float with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_float(long attr_id, long mem_type_id, float[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Awrite_float(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of int to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of int with data to be written to the file.
     * @param isCriticalPinning
     *            IN: request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_int(long attr_id, long mem_type_id, int[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Awrite_int not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of int to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of int with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_int(long attr_id, long mem_type_id, int[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Awrite_int(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of long to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of long with data to be written to the file.
     * @param isCriticalPinning
     *            IN: request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_long(long attr_id, long mem_type_id, long[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Awrite_long not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of long to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of long with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_long(long attr_id, long mem_type_id, long[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Awrite_long(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of short to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of short with data to be written to the file.
     * @param isCriticalPinning
     *            IN: request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_short(long attr_id, long mem_type_id, short[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Awrite_short not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of short to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of short with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_short(long attr_id, long mem_type_id, short[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Awrite_short(attr_id, mem_type_id, buf, true);
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of string to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of string with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5Awrite_string(long attr_id, long mem_type_id, String[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Awrite_string not implemented yet");
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buffer of variable-lenght to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer of variable-lenght with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data is null.
     **/
    public static int H5AwriteVL(long attr_id, long mem_type_id, Object[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }

        int status            = -1;
        boolean vl_data_class = false;
        // CRITICAL FIX: Use global Arena to prevent automatic cleanup conflicts with HDF5 VL memory
        Arena arena               = Arena.global();
        MemorySegment hvlArray    = null;
        MemorySegment arrayBuffer = null;
        MemorySegment stringArray = null;
        long space_id             = HDF5Constants.H5I_INVALID_HID;

        try {
            // Detect VL data to determine if H5Treclaim is needed (JNI pattern)
            vl_data_class = detectVLData(mem_type_id);

            ArrayList[] arrayData = (ArrayList[])buf;

            // Check the datatype class to determine conversion strategy
            int typeClass = H5Tget_class(mem_type_id);

            if (typeClass == HDF5Constants.H5T_COMPOUND) {
                // For compound datatypes, convert ArrayList array to packed compound structures
                arrayBuffer = VLDataConverter.convertCompoundDatatype(arrayData, mem_type_id, arena);
                status      = org.hdfgroup.javahdf5.hdf5_h.H5Awrite(attr_id, mem_type_id, arrayBuffer);
            }
            else if (typeClass == HDF5Constants.H5T_ARRAY) {
                // For array datatypes, convert to packed array elements (not hvl_t)
                arrayBuffer = VLDataConverter.convertArrayDatatype(arrayData, mem_type_id, arena);
                status      = org.hdfgroup.javahdf5.hdf5_h.H5Awrite(attr_id, mem_type_id, arrayBuffer);
            }
            else if (typeClass == HDF5Constants.H5T_STRING && H5Tis_variable_str(mem_type_id)) {
                // For variable-length string datatypes, convert to string pointers
                stringArray = VLDataConverter.convertVLStrings(arrayData, arena);
                status      = org.hdfgroup.javahdf5.hdf5_h.H5Awrite(attr_id, mem_type_id, stringArray);
            }
            else {
                // For VL datatypes, convert to hvl_t structures
                hvlArray = VLDataConverter.convertToHVL(arrayData, arena);
                status   = org.hdfgroup.javahdf5.hdf5_h.H5Awrite(attr_id, mem_type_id, hvlArray);
            }

            if (status < 0) {
                h5libraryError();
            }
        }
        catch (HDF5JavaException ex) {
            throw new HDF5LibraryException("VL data conversion failed: " + ex.getMessage());
        }
        catch (ClassCastException ex) {
            throw new HDF5LibraryException("Input data must be ArrayList array: " + ex.getMessage());
        }
        finally {
            // CRITICAL: Reclaim VL memory after write (JNI pattern)
            // This allows HDF5 to properly free any VL memory it allocated during the write
            if ((status >= 0) && vl_data_class && hvlArray != null) {
                try {
                    // Get dataspace for H5Treclaim
                    space_id = org.hdfgroup.javahdf5.hdf5_h.H5Aget_space(attr_id);
                    org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(
                        mem_type_id, space_id, org.hdfgroup.javahdf5.hdf5_h.H5P_DEFAULT(), hvlArray);
                }
                catch (Exception reclaimEx) {
                    // Log but don't fail if reclaim has issues
                    System.err.println("Warning: H5Treclaim failed in H5AwriteVL: " + reclaimEx.getMessage());
                }
                finally {
                    if (space_id >= 0) {
                        try {
                            org.hdfgroup.javahdf5.hdf5_h.H5Sclose(space_id);
                        }
                        catch (Exception ex) {
                            // Ignore close errors
                        }
                    }
                }
            }
        }

        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Awrite_VLStrings writes a variable length String attribute, specified by its identifier attr_id, from
     * the application memory buffer buffer of variable-length strings into the file.
     *
     * ---- contributed by Rosetta Biosoftware
     *
     * @param attr_id
     *            Identifier of the attribute to write to.
     * @param mem_type_id
     *            Identifier of the memory datatype (must be variable-length string type).
     * @param buf
     *            Buffer of variable-length strings with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     * @exception IllegalArgumentException
     *            buf is empty or type is not variable-length string.
     *
     * @note Uses Arena.global() for memory management
     * @note Null strings in input are converted to empty strings
     * @note Memory is automatically reclaimed by HDF5
     *
     **/

    public static int H5Awrite_VLStrings(long attr_id, long mem_type_id, Object[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        // Input validation
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        if (buf.length == 0) {
            throw new IllegalArgumentException("Buffer cannot be empty");
        }

        // Type validation - ensure it's a variable-length string type
        if (!H5.H5Tis_variable_str(mem_type_id)) {
            throw new HDF5LibraryException("Type is not a variable-length string");
        }

        // Validate buffer elements are Strings
        for (int i = 0; i < buf.length; i++) {
            if (buf[i] != null && !(buf[i] instanceof String)) {
                throw new IllegalArgumentException("Buffer element " + i +
                                                   " is not a String: " + buf[i].getClass().getName());
            }
        }

        if (log.isTraceEnabled()) {
            log.trace("H5Awrite_VLStrings: attr_id={}, mem_type_id={}, writing {} strings", attr_id,
                      mem_type_id, buf.length);
        }

        int status = -1;
        // Use global Arena to prevent automatic cleanup conflicts with HDF5 VL memory
        Arena arena = Arena.global();

        try {
            // Convert String array to VL format for HDF5
            String[] stringArray = (String[])buf;

            // Handle null strings - convert to empty strings
            for (int i = 0; i < stringArray.length; i++) {
                if (stringArray[i] == null) {
                    if (log.isTraceEnabled()) {
                        log.trace("H5Awrite_VLStrings: converting null at index {} to empty string", i);
                    }
                    stringArray[i] = "";
                }
            }

            // For VL strings, we need to manually create hvl_t structures
            // VLDataConverter is for array types, not VL string types
            MemorySegment hvlArray = org.hdfgroup.javahdf5.hvl_t.allocateArray(stringArray.length, arena);

            for (int i = 0; i < stringArray.length; i++) {
                MemorySegment hvlElement = org.hdfgroup.javahdf5.hvl_t.asSlice(hvlArray, i);
                String str               = stringArray[i];

                if (str == null || str.isEmpty()) {
                    // Empty string: set length to 1 (for null terminator) and allocate single byte
                    org.hdfgroup.javahdf5.hvl_t.len(hvlElement, 1);
                    MemorySegment emptyStr = arena.allocate(1);
                    emptyStr.set(ValueLayout.JAVA_BYTE, 0, (byte)0);
                    org.hdfgroup.javahdf5.hvl_t.p(hvlElement, emptyStr);
                }
                else {
                    // Convert string to null-terminated C string
                    MemorySegment stringSegment = arena.allocateFrom(str, StandardCharsets.UTF_8);
                    // Set length to string length + 1 for null terminator
                    org.hdfgroup.javahdf5.hvl_t.len(hvlElement, str.length() + 1);
                    org.hdfgroup.javahdf5.hvl_t.p(hvlElement, stringSegment);
                }
            }

            // Call H5Awrite (not H5Dwrite) - attributes don't use space parameters
            status = org.hdfgroup.javahdf5.hdf5_h.H5Awrite(attr_id, mem_type_id, hvlArray);

            if (status < 0) {
                h5libraryError();
            }

            if (log.isTraceEnabled()) {
                log.trace("H5Awrite_VLStrings: successfully wrote {} strings, status={}", buf.length, status);
            }
        }
        catch (HDF5JavaException ex) {
            throw new HDF5LibraryException("VL string write failed: " + ex.getMessage());
        }

        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aget_create_plist retrieves a copy of the attribute creation property list identifier.
     *
     * @param attr_id
     *            IN: Identifier of an attribute.
     *
     * @return identifier for the attribute's creation property list if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Aget_create_plist(long attr_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Aget_create_plist(attr_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aiterate iterates over the attributes attached to a dataset, named datatype, or group, as
     * specified by obj_id. For each attribute, user-provided data, op_data, with additional information
     * as defined below, is passed to a user-defined function, op, which operates on that attribute.
     *
     * @param loc_id
     *            IN: Identifier for object to which attributes are attached; may be group, dataset, or named
     *                datatype.
     * @param idx_type
     *            IN: The type of index specified by idx_type can be one of the following:
     *                      H5_INDEX_NAME             An alphanumeric index by attribute name.
     *                      H5_INDEX_CRT_ORDER        An index by creation order.
     * @param order
     *            IN: The order in which the index is to be traversed, as specified by order, can be one of
     *                the following:
     *                      H5_ITER_INC     Iteration is from beginning to end, i.e., a top-down iteration
     *                                      incrementing the index position at each step.
     *                      H5_ITER_DEC     Iteration starts at the end of the index, i.e., a bottom-up
     *                                      iteration decrementing the index position at each step.
     *                      H5_ITER_NATIVE  HDF5 iterates in the fastest-available order. No information is
     *                                      provided as to the order, but HDF5 ensures that each element in
     *                                      the index will be visited if the iteration completes successfully.
     * @param idx
     *            IN/OUT: Initial and returned offset within index.
     * @param op
     *            IN: Callback function to operate on each value.
     * @param op_data
     *            IN/OUT: Pointer to any user-efined data for use by operator function.
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static int H5Aiterate(long loc_id, int idx_type, int order, long idx,
                                 hdf.hdf5lib.callbacks.H5A_iterate_cb op,
                                 hdf.hdf5lib.callbacks.H5A_iterate_t op_data)
        throws HDF5LibraryException, NullPointerException
    {
        if (op == null) {
            throw new NullPointerException("op is null");
        }

        int status     = -1;
        long start_idx = idx;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment start_idx_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            start_idx_segment.set(ValueLayout.JAVA_LONG, 0, start_idx);
            MemorySegment op_segment = H5A_operator2_t.allocate(op, arena);
            MemorySegment op_data_segment =
                Linker.nativeLinker().upcallStub(H5Aiterate2$handle(), H5Aiterate2$descriptor(), arena);

            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Aiterate2(loc_id, idx_type, order, start_idx_segment,
                                                                   op_segment, op_data_segment)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5A
     *
     * H5Aiterate_by_name iterates over the attributes attached to the dataset or group specified with loc_id
     * and obj_name. For each attribute, user-provided data, op_data, with additional information as defined
     * below, is passed to a user-defined function, op, which operates on that attribute.
     *
     * @param loc_id
     *            IN: Identifier for object to which attributes are attached; may be group, dataset, or named
     *                datatype.
     * @param obj_name
     *            IN: Name of object, relative to location.
     * @param idx_type
     *            IN: The type of index specified by idx_type can be one of the following:
     *                      H5_INDEX_NAME             An alphanumeric index by attribute name.
     *                      H5_INDEX_CRT_ORDER        An index by creation order.
     * @param order
     *            IN: The order in which the index is to be traversed, as specified by order, can be one of
     *                the following:
     *                H5_ITER_INC     Iteration is from beginning to end, i.e., a top-down
     *                iteration incrementing the index position at each step.
     *                H5_ITER_DEC     Iteration starts at the end of the index, i.e., a bottom-up iteration
     *                                decrementing the index position at each step.
     *                H5_ITER_NATIVE  HDF5 iterates in the fastest-available order. No information is provided
     *                                as to the order, but HDF5 ensures that each element in the index will be
     *                                visited if the iteration completes successfully.
     * @param idx
     *            IN/OUT: Initial and returned offset within index.
     * @param op
     *            IN: Callback function to operate on each value.
     * @param op_data
     *            IN/OUT: Pointer to any user-efined data for use by operator function.
     * @param lapl_id
     *            IN: Link access property list
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static int H5Aiterate_by_name(long loc_id, String obj_name, int idx_type, int order, long idx,
                                         hdf.hdf5lib.callbacks.H5A_iterate_cb op,
                                         hdf.hdf5lib.callbacks.H5A_iterate_t op_data, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (null == obj_name)
            throw new HDF5FunctionArgumentException("object name is NULL");
        if (op == null) {
            throw new NullPointerException("op is null");
        }

        int status     = -1;
        long start_idx = idx;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment      = arena.allocateFrom(obj_name);
            MemorySegment start_idx_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            start_idx_segment.set(ValueLayout.JAVA_LONG, 0, start_idx);
            MemorySegment op_segment      = H5A_operator2_t.allocate(op, arena);
            MemorySegment op_data_segment = Linker.nativeLinker().upcallStub(
                H5Aiterate_by_name$handle(), H5Aiterate_by_name$descriptor(), arena);

            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Aiterate_by_name(
                     loc_id, name_segment, idx_type, order, start_idx_segment, op_segment, op_data_segment,
                     lapl_id)) < 0)
                h5libraryError();
        }
        return status;
    }

    // ////////////////////////////////////////////////////////////
    // //
    // H5AC: Cache Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // No public Functions

    // ////////////////////////////////////////////////////////////
    // //
    // H5B: B-link-tree Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // No public Functions

    // ////////////////////////////////////////////////////////////
    // //
    // H5B2: v2 B-tree Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // No public Functions

    // ////////////////////////////////////////////////////////////
    // //
    // H5C: Cache Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // No public Functions

    // ////////////////////////////////////////////////////////////
    // //
    // H5D: Datasets Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5D Java Datasets (H5D) Interface
     *
     * @see H5D, C-API
     *
     * @see @ref H5D_UG, User Guide
     **/

    /**
     * @ingroup JH5D
     *
     * H5Dcopy copies the content of one dataset to another dataset.
     *
     * @param src_did
     *            the identifier of the source dataset
     * @param dst_did
     *            the identifier of the destination dataset
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static int H5Dcopy(long src_did, long dst_did) throws HDF5LibraryException
    {
        if (src_did < 0 || dst_did < 0) {
            throw new HDF5FunctionArgumentException("Negative dataset identifier");
        }
        throw new HDF5LibraryException("H5Dcopy not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dclose ends access to a dataset specified by dataset_id and releases resources used by it.
     *
     * @param dataset_id
     *            Identifier of the dataset to finish access to.
     *
     * @return a non-negative value if successful
     **/
    public static int H5Dclose(long dataset_id)
    {
        log.trace("OPEN_IDS: H5Dclose remove {}", dataset_id);
        OPEN_IDS.remove(dataset_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Dclose(dataset_id);
        if (retVal < 0)
            h5libraryError();
        return retVal;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dcreate creates a new dataset named name at the location specified by loc_id.
     *
     * @param loc_id
     *            IN: Location identifier
     * @param name
     *            IN: Dataset name
     * @param type_id
     *            IN: Datatype identifier
     * @param space_id
     *            IN: Dataspace identifier
     * @param lcpl_id
     *            IN: Identifier of link creation property list.
     * @param dcpl_id
     *            IN: Identifier of dataset creation property list.
     * @param dapl_id
     *            IN: Identifier of dataset access property list.
     *
     * @return a dataset identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Dcreate(long loc_id, String name, long type_id, long space_id, long lcpl_id,
                                 long dcpl_id, long dapl_id) throws HDF5LibraryException, NullPointerException
    {
        long dset_id = H5I_INVALID_HID();
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            if ((dset_id = H5Dcreate2(loc_id, name_segment, type_id, space_id, lcpl_id, dcpl_id, dapl_id)) <
                0) {
                h5libraryError();
            }
        }
        if (dset_id > 0) {
            log.trace("OPEN_IDS: H5Dcreate add {}", dset_id);
            OPEN_IDS.add(dset_id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }

        return dset_id;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dcreate_anon creates a dataset in the file specified by loc_id.
     *
     * @param loc_id
     *            IN: Location identifier
     * @param type_id
     *            IN: Datatype identifier
     * @param space_id
     *            IN: Dataspace identifier
     * @param dcpl_id
     *            IN: Identifier of dataset creation property list.
     * @param dapl_id
     *            IN: Identifier of dataset access property list.
     *
     * @return a dataset identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Dcreate_anon(long loc_id, long type_id, long space_id, long dcpl_id, long dapl_id)
        throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Dcreate_anon(loc_id, type_id, space_id, dcpl_id, dapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dcreate_anon add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dfill explicitly fills the dataspace selection in memory, space_id, with the fill value specified in
     * fill.
     *
     * @param fill
     *            IN: Pointer to the fill value to be used.
     * @param fill_type
     *            IN: Fill value datatype identifier.
     * @param buf
     *            IN/OUT: Pointer to the memory buffer containing the selection to be filled.
     * @param buf_type
     *            IN: Datatype of dataspace elements to be filled.
     * @param space_id
     *            IN: Dataspace describing memory buffer and containing the selection to be filled.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static void H5Dfill(byte[] fill, long fill_type, byte[] buf, long buf_type, long space_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment fill_segment = MemorySegment.NULL;
            if (fill != null) {
                fill_segment = arena.allocate(ValueLayout.JAVA_BYTE, fill.length);
                for (int i = 0; i < fill.length; i++) {
                    fill_segment.set(ValueLayout.JAVA_BYTE, i, fill[i]);
                }
            }
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_BYTE, buf.length);
            for (int i = 0; i < buf.length; i++) {
                buf_segment.set(ValueLayout.JAVA_BYTE, i, buf[i]);
            }

            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Dfill(fill_segment, fill_type, buf_segment, buf_type,
                                                               space_id)) < 0)
                h5libraryError();

            for (int i = 0; i < buf.length; i++) {
                buf[i] = buf_segment.get(ValueLayout.JAVA_BYTE, i);
            }
        }
    }

    /**
     * @ingroup JH5D
     *
     * H5Dget_access_plist returns an identifier for a copy of the dataset access property list for a dataset.
     *
     * @param dataset_id
     *            IN: Identifier of the dataset to query.
     *
     * @return a dataset access property list identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Dget_access_plist(long dataset_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Dget_access_plist(dataset_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dget_access_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dget_create_plist returns an identifier for a copy of the dataset creation property list for a
     * dataset.
     *
     * @param dataset_id
     *            Identifier of the dataset to query.
     * @return a dataset creation property list identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Dget_create_plist(long dataset_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Dget_create_plist(dataset_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dget_offset returns the address in the file of the dataset dset_id.
     *
     * @param dataset_id
     *            IN: Identifier of the dataset in question
     *
     * @return the offset in bytes.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Dget_offset(long dataset_id) throws HDF5LibraryException
    {
        long offset = org.hdfgroup.javahdf5.hdf5_h.H5Dget_offset(dataset_id);
        if (offset < 0) {
            h5libraryError();
        }
        return offset;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dget_space returns an identifier for a copy of the dataspace for a dataset.
     *
     * @param dataset_id
     *            Identifier of the dataset to query.
     *
     * @return a dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Dget_space(long dataset_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Dget_space(dataset_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dget_space add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dget_space_status determines whether space has been allocated for the dataset dset_id.
     *
     * @param dataset_id
     *            IN: Identifier of the dataset to query.
     *
     * @return the space allocation status
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Dget_space_status(long dataset_id) throws HDF5LibraryException
    {
        int space_status = HDF5Constants.H5D_SPACE_STATUS_ERROR;

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment int_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            int status = org.hdfgroup.javahdf5.hdf5_h.H5Dget_space_status(dataset_id, int_segment);
            if (status < 0)
                h5libraryError();
            space_status = int_segment.get(ValueLayout.JAVA_INT, 0);
        }

        return space_status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dget_storage_size returns the amount of storage that is required for the dataset.
     *
     * @param dataset_id
     *            Identifier of the dataset in question
     *
     * @return he amount of storage space allocated for the dataset.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Dget_storage_size(long dataset_id) throws HDF5LibraryException
    {
        long size = org.hdfgroup.javahdf5.hdf5_h.H5Dget_storage_size(dataset_id);
        if (size == 0)
            h5libraryError();

        return size;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dget_type returns an identifier for a copy of the datatype for a dataset.
     *
     * @param dataset_id
     *            Identifier of the dataset to query.
     *
     * @return a datatype identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Dget_type(long dataset_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Dget_type(dataset_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dget_type add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5D
     *
     * H5Diterate iterates over all the data elements in the memory buffer buf, executing the callback
     * function operator once for each such data element.
     *
     * @param buf
     *            IN/OUT: Pointer to the memory containing the elements to iterate over.
     * @param buf_type
     *            IN: Buffer datatype identifier.
     * @param space_id
     *            IN: Dataspace describing memory buffer.
     * @param op
     *            IN: Callback function to operate on each value.
     * @param op_data
     *            IN/OUT: Pointer to any user-efined data for use by operator function.
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static int H5Diterate(byte[] buf, long buf_type, long space_id,
                                 hdf.hdf5lib.callbacks.H5D_iterate_cb op,
                                 hdf.hdf5lib.callbacks.H5D_iterate_t op_data)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        if (op == null) {
            throw new NullPointerException("op is null");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, buf);
            MemorySegment op_segment  = H5D_operator_t.allocate(op, arena);
            MemorySegment op_data_segment =
                Linker.nativeLinker().upcallStub(H5Diterate$handle(), H5Diterate$descriptor(), arena);

            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Diterate(buf_segment, buf_type, space_id, op_segment,
                                                                  op_data_segment)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dopen opens the existing dataset specified by a location identifier and name, loc_id and name,
     * respectively.
     *
     * @param loc_id
     *            IN: Location identifier
     * @param name
     *            IN: Dataset name
     * @param dapl_id
     *            IN: Identifier of dataset access property list.
     *
     * @return a dataset identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Dopen(long loc_id, String name, long dapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (name == null) {
            throw new NullPointerException("Dataset name cannot be null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            id                         = H5Dopen2(loc_id, name_segment, dapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer buf.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param obj
     *            Buffer to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                              long xfer_plist_id, byte[] obj, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (obj == null) {
            throw new NullPointerException("data buffer is null");
        }

        boolean vl_data_class = false;
        //        if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        //            H5_LIBRARY_ERROR(ENVONLY);

        if (vl_data_class) {
            long typeSize = -1;
            if ((typeSize = H5Tget_size(mem_type_id)) < 0)
                h5libraryError();
            System.out.println("typeSize = " + typeSize);
            int rank = -1;
            if ((rank = H5Sget_simple_extent_ndims(mem_space_id)) < 0)
                h5libraryError();
            System.out.println("rank = " + rank);
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment obj_segment = arena.allocate(ValueLayout.JAVA_BYTE, obj.length);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                          file_space_id, xfer_plist_id, obj_segment);
            if (status < 0)
                h5libraryError();
            MemorySegment.copy(obj_segment, ValueLayout.JAVA_BYTE, 0L, obj, 0, obj.length);
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer buf.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                              long xfer_plist_id, byte[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }

        boolean vl_data_class = false;
        //        if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        //            H5_LIBRARY_ERROR(ENVONLY);

        if (vl_data_class) {
            long typeSize = -1;
            if ((typeSize = H5Tget_size(mem_type_id)) < 0)
                h5libraryError();
            System.out.println("typeSize = " + typeSize);
            int rank = -1;
            if ((rank = H5Sget_simple_extent_ndims(mem_space_id)) < 0)
                h5libraryError();
            System.out.println("rank = " + rank);
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment obj_segment = arena.allocate(ValueLayout.JAVA_BYTE, buf.length);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                          file_space_id, xfer_plist_id, obj_segment);
            if (status < 0)
                h5libraryError();
            MemorySegment.copy(obj_segment, ValueLayout.JAVA_BYTE, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer buf.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param obj
     *            Buffer to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                              long xfer_plist_id, Object obj)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        return H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, obj, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application data object.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param obj
     *            Object to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Failure in the data conversion.
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data object is null.
     **/
    public static int H5Dread(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                              long xfer_plist_id, Object obj, boolean isCriticalPinning)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        int status   = -1;
        boolean is1D = false;

        Class dataClass = obj.getClass();
        if (!dataClass.isArray()) {
            throw new HDF5JavaException("data is not an array");
        }

        String cname = dataClass.getName();
        is1D         = (cname.lastIndexOf('[') == cname.indexOf('['));
        char dname   = cname.charAt(cname.lastIndexOf("[") + 1);
        log.trace("H5Dread: cname={} is1D={} dname={}", cname, is1D, dname);

        if (is1D && (dname == 'B')) {
            log.trace("H5Dread_dname_B");
            status = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (byte[])obj,
                             isCriticalPinning);
        }
        else if (is1D && (dname == 'S')) {
            log.trace("H5Dread_dname_S");
            status = H5Dread_short(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                                   (short[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'I')) {
            log.trace("H5Dread_dname_I");
            status = H5Dread_int(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                                 (int[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'J')) {
            log.trace("H5Dread_dname_J");
            status = H5Dread_long(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                                  (long[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'F')) {
            System.err.println("H5Dread_dname_F");
            log.trace("H5Dread_dname_F");
            status = H5Dread_float(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                                   (float[])obj, isCriticalPinning);
        }
        else if (is1D && (dname == 'D')) {
            log.trace("H5Dread_dname_D");
            status = H5Dread_double(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                                    (double[])obj, isCriticalPinning);
        }
        else if ((H5.H5Tdetect_class(mem_type_id, HDF5Constants.H5T_REFERENCE) &&
                  (is1D && (dataClass.getComponentType() == String.class))) ||
                 H5.H5Tequal(mem_type_id, HDF5Constants.H5T_STD_REF_DSETREG)) {
            log.trace("H5Dread_reg_ref - routing to H5DreadVL");
            status =
                H5DreadVL(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (Object[])obj);
        }
        else if (is1D && (dataClass.getComponentType() == String.class)) {
            log.trace("H5Dread_string type - routing to H5DreadVL");
            status =
                H5DreadVL(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (Object[])obj);
        }
        else if (H5.H5Tget_class(mem_type_id) == HDF5Constants.H5T_VLEN) {
            log.trace("H5DreadVL type - using H5DreadVL directly");
            status =
                H5DreadVL(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (Object[])obj);
        }
        else {
            // Create a data buffer to hold the data into a Java Array
            HDFArray theArray = new HDFArray(obj);
            byte[] buf        = theArray.emptyBytes();
            log.trace("H5Dread_else");

            // will raise exception if read fails
            status = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf,
                             isCriticalPinning);
            if (status >= 0) {
                // convert the data into a Java Array
                obj = theArray.arrayify(buf);
            }

            // clean up these: assign 'null' as hint to gc()
            buf      = null;
            theArray = null;
        }

        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of type double.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of type double to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_double(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                     long xfer_plist_id, double[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_DOUBLE, buf.length);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                          file_space_id, xfer_plist_id, buf_segment);
            if (status < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_DOUBLE, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of type double.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of double to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_double(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                     long xfer_plist_id, double[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dread_double(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of float.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of float to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_float(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                    long xfer_plist_id, float[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_FLOAT, buf.length);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                          file_space_id, xfer_plist_id, buf_segment);
            if (status < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_FLOAT, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of float.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of float to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_float(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                    long xfer_plist_id, float[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dread_float(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of int.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of int to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_int(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                  long xfer_plist_id, int[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_INT, buf.length);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                          file_space_id, xfer_plist_id, buf_segment);
            if (status < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_INT, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of int.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of int to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_int(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                  long xfer_plist_id, int[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dread_int(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of long.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of long to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_long(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                   long xfer_plist_id, long[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_LONG, buf.length);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                          file_space_id, xfer_plist_id, buf_segment);
            if (status < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_LONG, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of long.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of long to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_long(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                   long xfer_plist_id, long[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dread_long(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of string.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of string to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_reg_ref(long dataset_id, long mem_type_id, long mem_space_id,
                                      long file_space_id, long xfer_plist_id, String[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Dread_reg_ref not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of short.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of short to store data read from the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_short(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                    long xfer_plist_id, short[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_SHORT, buf.length);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                          file_space_id, xfer_plist_id, buf_segment);
            if (status < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_SHORT, 0L, buf, 0, buf.length);
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of short.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of short to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_short(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                    long xfer_plist_id, short[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dread_short(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of variable-lenght.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of variable-lenght to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5DreadVL(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                long xfer_plist_id, Object[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }

        int status            = -1;
        boolean vl_data_class = false;
        // CRITICAL FIX: Use global Arena to prevent automatic cleanup conflicts with HDF5 VL memory
        Arena arena = Arena.global();

        try {
            // Detect VL data to determine if H5Treclaim is needed (JNI pattern)
            vl_data_class = detectVLData(mem_type_id);
            // Check the datatype class to determine reading strategy
            int typeClass = H5Tget_class(mem_type_id);

            if (typeClass == HDF5Constants.H5T_COMPOUND) {
                // For compound datatypes, read heterogeneous field structures
                ArrayList[] result =
                    VLDataConverter.readCompoundDatatype(dataset_id, mem_type_id, buf.length, arena, true,
                                                         mem_space_id, file_space_id, xfer_plist_id);
                System.arraycopy(result, 0, buf, 0, buf.length);
                status = 0; // Success
            }
            else if (typeClass == HDF5Constants.H5T_ARRAY) {
                // For array datatypes, read directly into array buffer (not hvl_t)
                ArrayList[] result = VLDataConverter.readArrayDatatypeFromDataset(
                    dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf.length, arena);
                System.arraycopy(result, 0, buf, 0, buf.length);
                status = 0; // Success
            }
            else if (typeClass == HDF5Constants.H5T_STRING && H5Tis_variable_str(mem_type_id)) {
                // CRITICAL FIX: For variable-length string datatypes, use string pointer array
                // to match the write path format - this fixes the off-by-one indexing issue
                MemorySegment stringArray = arena.allocate(ValueLayout.ADDRESS, buf.length);

                // Call native H5Dread to read string pointers
                status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                              file_space_id, xfer_plist_id, stringArray);

                if (status >= 0) {
                    // Convert string pointer array back to ArrayList array
                    for (int i = 0; i < buf.length; i++) {
                        ArrayList<String> stringList = new ArrayList<>();
                        MemorySegment stringPtr      = stringArray.getAtIndex(ValueLayout.ADDRESS, i);

                        if (stringPtr != null && !stringPtr.equals(MemorySegment.NULL)) {
                            // Reinterpret the string pointer with proper scope for reading
                            // Use a large but safe size limit for string reading
                            MemorySegment boundedStringPtr =
                                stringPtr.reinterpret(4096, Arena.global(), null);
                            String str =
                                boundedStringPtr.getString(0, java.nio.charset.StandardCharsets.UTF_8);
                            stringList.add(str);
                        }
                        else {
                            stringList.add(""); // Empty string for null pointers
                        }
                        buf[i] = stringList;
                    }
                }
            }
            else {
                // For VL datatypes, use hvl_t structures
                MemorySegment hvlArray = hvl_t.allocateArray(buf.length, arena);

                // Call native H5Dread
                status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                              file_space_id, xfer_plist_id, hvlArray);

                if (status >= 0) {
                    // Convert hvl_t data back to ArrayList array IMMEDIATELY while memory is valid
                    ArrayList[] result = VLDataConverter.convertFromHVL(hvlArray, buf.length, mem_type_id);

                    // Get dataspace for H5Treclaim
                    long space_id = org.hdfgroup.javahdf5.hdf5_h.H5Dget_space(dataset_id);

                    // Reclaim VL memory allocated by HDF5 only if VL data is detected (JNI pattern)
                    try {
                        if ((status >= 0) && vl_data_class) {
                            org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(
                                mem_type_id, space_id, org.hdfgroup.javahdf5.hdf5_h.H5P_DEFAULT(), hvlArray);
                        }
                    }
                    catch (Exception reclaimEx) {
                        // Log but don't fail if reclaim has issues
                        System.err.println("Warning: H5Treclaim failed in H5DreadVL: " +
                                           reclaimEx.getMessage());
                    }
                    finally {
                        if (space_id >= 0) {
                            org.hdfgroup.javahdf5.hdf5_h.H5Sclose(space_id);
                        }
                    }

                    System.arraycopy(result, 0, buf, 0, buf.length);
                }
                else {
                    h5libraryError();
                }
            }
        }
        catch (HDF5JavaException ex) {
            throw new HDF5LibraryException("VL data conversion failed: " + ex.getMessage());
        }

        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the
     * application memory buffer of string.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of string to store data read from the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     **/
    public static int H5Dread_string(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                     long xfer_plist_id, String[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Dread_string not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dread_VLStrings reads a (partial) dataset, specified by its identifier dataset_id, from the file into
     *the application memory buffer of variable-length strings.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype (must be variable-length string type).
     * @param mem_space_id
     *            Identifier of the memory dataspace (use H5S_ALL for entire dataset).
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file (use H5S_ALL for entire dataset).
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            OUT: Buffer of variable-length strings to store data read from the file.
     *                 Must be pre-allocated with sufficient size.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data buffer is null.
     * @exception IllegalArgumentException
     *            buf is too small or type is not variable-length string.
     *
     * @note Uses Arena.global() for memory management
     * @note H5Treclaim is called automatically to free HDF5-managed VL memory
     * @note Buffer must be pre-allocated with size matching memory dataspace
     *
     **/
    public static int H5Dread_VLStrings(long dataset_id, long mem_type_id, long mem_space_id,
                                        long file_space_id, long xfer_plist_id, Object[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        // Input validation
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }

        // Type validation - ensure it's a variable-length string type
        if (!H5.H5Tis_variable_str(mem_type_id)) {
            throw new HDF5LibraryException("Type is not a variable-length string");
        }

        if (log.isTraceEnabled()) {
            log.trace(
                "H5Dread_VLStrings: dataset_id={}, mem_type_id={}, mem_space_id={}, file_space_id={}, buffer size={}",
                dataset_id, mem_type_id, mem_space_id, file_space_id, buf.length);
        }

        // Determine buffer size from memory space (or dataset space if H5S_ALL)
        long space_id      = mem_space_id;
        boolean closeSpace = false;
        long[] dims        = new long[1];
        int status         = -1;

        try {
            // If mem_space_id is H5S_ALL, get the dataset's dataspace
            if (mem_space_id == HDF5Constants.H5S_ALL) {
                space_id   = H5.H5Dget_space(dataset_id);
                closeSpace = true;
                if (space_id < 0) {
                    throw new HDF5LibraryException("Failed to get dataset dataspace");
                }
            }

            if (space_id < 0 || space_id == HDF5Constants.H5S_ALL) {
                throw new HDF5LibraryException("Invalid dataspace");
            }

            int rank = H5.H5Sget_simple_extent_ndims(space_id);
            if (rank > 0) {
                long[] fullDims = new long[rank];
                H5.H5Sget_simple_extent_dims(space_id, fullDims, null);

                // Calculate total size
                long totalSize = 1;
                for (long dim : fullDims) {
                    totalSize *= dim;
                }

                // Buffer size validation
                if (buf.length < totalSize) {
                    throw new IllegalArgumentException("Buffer too small: " + buf.length + " < " + totalSize);
                }
                dims[0] = totalSize;
            }
            else {
                // Scalar dataset
                dims[0] = 1;
            }

            if (log.isTraceEnabled()) {
                log.trace("H5Dread_VLStrings: reading {} elements", dims[0]);
            }

            // Use global Arena to prevent automatic cleanup conflicts with HDF5 VL memory
            Arena arena = Arena.global();

            // Allocate hvl_t array for reading
            MemorySegment hvlArray = org.hdfgroup.javahdf5.hvl_t.allocateArray((int)dims[0], arena);

            // Read from HDF5
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dread(dataset_id, mem_type_id, mem_space_id,
                                                          file_space_id, xfer_plist_id, hvlArray);

            if (status < 0) {
                h5libraryError();
            }

            // Convert from hvl_t to Java ArrayList[]
            ArrayList[] vlResult = VLDataConverter.convertFromHVL(hvlArray, (int)dims[0], mem_type_id);

            // Extract strings from ArrayList[] to String[]
            for (int i = 0; i < dims[0] && i < buf.length; i++) {
                if (vlResult[i] != null && !vlResult[i].isEmpty()) {
                    buf[i] = (String)vlResult[i].get(0);
                }
                else {
                    buf[i] = "";
                }
            }

            if (log.isTraceEnabled()) {
                log.trace("H5Dread_VLStrings: successfully read {} strings, status={}", dims[0], status);
            }

            // CRITICAL: Reclaim VL memory managed by HDF5
            // This must be called to prevent memory leaks
            int reclaim_status = org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(
                mem_type_id, space_id, org.hdfgroup.javahdf5.hdf5_h.H5P_DEFAULT(), hvlArray);
            if (reclaim_status < 0 && log.isTraceEnabled()) {
                log.trace("H5Dread_VLStrings: H5Treclaim returned {}", reclaim_status);
            }
        }
        catch (HDF5JavaException ex) {
            throw new HDF5LibraryException("VL string read failed: " + ex.getMessage());
        }
        finally {
            // Clean up dataspace if we allocated it
            if (closeSpace && space_id >= 0) {
                try {
                    H5.H5Sclose(space_id);
                }
                catch (Exception ex) {
                    if (log.isTraceEnabled()) {
                        log.trace("H5Dread_VLStrings: H5Sclose failed: {}", ex.getMessage());
                    }
                }
            }
        }

        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dset_extent sets the current dimensions of the chunked dataset dset_id to the sizes specified in
     * size.
     *
     * @param dset_id
     *            IN: Chunked dataset identifier.
     * @param size
     *            IN: Array containing the new magnitude of each dimension of the dataset.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     **/
    public static void H5Dset_extent(long dset_id, long size[])
        throws HDF5LibraryException, NullPointerException
    {
        if (size == null) {
            throw new NullPointerException("size is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, size);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Dset_extent(dset_id, size_segment) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5D
     *
     * H5Dvlen_get_buf_size determines the number of bytes required to store the VL data from the dataset,
     * using the space_id for the selection in the dataset on disk and the type_id for the memory
     * representation of the VL data in memory.
     *
     * @param dset_id
     *            IN: Identifier of the dataset read from.
     * @param type_id
     *            IN: Identifier of the datatype.
     * @param space_id
     *            IN: Identifier of the dataspace.
     *
     * @return the size in bytes of the memory buffer required to store the VL data.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Dvlen_get_buf_size(long dset_id, long type_id, long space_id)
        throws HDF5LibraryException
    {
        long size = -1; // Default value for size
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a buffer for the size
            MemorySegment sizeSegment = arena.allocate(ValueLayout.JAVA_LONG);
            // Call the native method to get the buffer size for VL data
            int status =
                org.hdfgroup.javahdf5.hdf5_h.H5Dvlen_get_buf_size(dset_id, type_id, space_id, sizeSegment);
            if (status < 0) {
                h5libraryError();
            }
            size = sizeSegment.get(ValueLayout.JAVA_LONG, 0);
        }

        return size;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dvlen_reclaim reclaims buffer used for VL data.
     *
     * @param type_id
     *            Identifier of the datatype.
     * @param space_id
     *            Identifier of the dataspace.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer with data to be reclaimed.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     *
     * @deprecated As of HDF5 1.12.0 in favor of H5Treclaim
     **/
    @Deprecated
    public static int H5Dvlen_reclaim(long type_id, long space_id, long xfer_plist_id, byte[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        throw new HDF5LibraryException("H5Dvlen_reclaim not implemented as it is deprecated");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer with data to be written to the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                               long xfer_plist_id, byte[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_BYTE, buf.length);
            buf_segment.copyFrom(MemorySegment.ofArray(buf));
            status = org.hdfgroup.javahdf5.hdf5_h.H5Dwrite(dataset_id, mem_type_id, mem_space_id,
                                                           file_space_id, xfer_plist_id, buf_segment);
            if (status < 0) {
                h5libraryError();
            }
        }
        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                               long xfer_plist_id, byte[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param obj
     *            Buffer with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                               long xfer_plist_id, Object obj)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        return H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, obj, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory data object into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param obj
     *            Object with data to be written to the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Failure in the data conversion.
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            data object is null.
     **/
    public static int H5Dwrite(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                               long xfer_plist_id, Object obj, boolean isCriticalPinning)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        int status   = -1;
        boolean is1D = false;

        Class dataClass = obj.getClass();
        if (!dataClass.isArray()) {
            throw new HDF5JavaException("data is not an array");
        }

        String cname = dataClass.getName();
        is1D         = (cname.lastIndexOf('[') == cname.indexOf('['));
        char dname   = cname.charAt(cname.lastIndexOf("[") + 1);

        if (is1D && (dataClass.getComponentType() == String.class)) {
            log.trace("H5Dwrite_string type - routing to H5Dwrite_VLStrings");
            status = H5Dwrite_VLStrings(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                                        (String[])obj);
        }
        else if (H5.H5Tget_class(mem_type_id) == HDF5Constants.H5T_VLEN) {
            log.trace("H5DwriteVL type - using H5DwriteVL directly");
            status = H5DwriteVL(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                                (Object[])obj);
        }
        else {
            HDFArray theArray = new HDFArray(obj);
            byte[] buf        = theArray.byteify();

            // will raise exception on error
            status = H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf);

            // clean up these: assign 'null' as hint to gc()
            buf      = null;
            theArray = null;
        }

        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of double with data to be written to the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_double(long dataset_id, long mem_type_id, long mem_space_id,
                                      long file_space_id, long xfer_plist_id, double[] buf,
                                      boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Dwrite_double not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of double with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_double(long dataset_id, long mem_type_id, long mem_space_id,
                                      long file_space_id, long xfer_plist_id, double[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dwrite_double(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf,
                               true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of float with data to be written to the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_float(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                     long xfer_plist_id, float[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Dwrite_float not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of float with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_float(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                     long xfer_plist_id, float[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dwrite_float(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of int with data to be written to the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_int(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                   long xfer_plist_id, int[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Dwrite_int not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of int with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_int(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                   long xfer_plist_id, int[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dwrite_int(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of long with data to be written to the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_long(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                    long xfer_plist_id, long[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Dwrite_long not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of long with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_long(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                    long xfer_plist_id, long[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dwrite_long(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of short with data to be written to the file.
     * @param isCriticalPinning
     *            request lock on data reference.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_short(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                     long xfer_plist_id, short[] buf, boolean isCriticalPinning)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Dwrite_short not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of short with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_short(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                     long xfer_plist_id, short[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Dwrite_short(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of string with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Dwrite_string(long dataset_id, long mem_type_id, long mem_space_id,
                                      long file_space_id, long xfer_plist_id, String[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        throw new HDF5LibraryException("H5Dwrite_string not implemented yet");
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application
     * memory buffer into the file.
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer of variable-length with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5DwriteVL(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
                                 long xfer_plist_id, Object[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }

        int status            = -1;
        boolean vl_data_class = false;
        // CRITICAL FIX: Use global Arena to prevent automatic cleanup conflicts with HDF5 VL memory
        Arena arena               = Arena.global();
        MemorySegment hvlArray    = null;
        MemorySegment arrayBuffer = null;
        MemorySegment stringArray = null;

        try {
            // Detect VL data to determine if H5Treclaim is needed (JNI pattern)
            vl_data_class = detectVLData(mem_type_id);

            ArrayList[] arrayData = (ArrayList[])buf;

            // Check the datatype class to determine conversion strategy
            int typeClass = H5Tget_class(mem_type_id);

            if (typeClass == HDF5Constants.H5T_COMPOUND) {
                // For compound datatypes, convert ArrayList array to packed compound structures
                arrayBuffer = VLDataConverter.convertCompoundDatatype(arrayData, mem_type_id, arena);
                status      = org.hdfgroup.javahdf5.hdf5_h.H5Dwrite(dataset_id, mem_type_id, mem_space_id,
                                                                    file_space_id, xfer_plist_id, arrayBuffer);
            }
            else if (typeClass == HDF5Constants.H5T_ARRAY) {
                // For array datatypes, convert to packed array elements (not hvl_t)
                arrayBuffer = VLDataConverter.convertArrayDatatype(arrayData, mem_type_id, arena);
                status      = org.hdfgroup.javahdf5.hdf5_h.H5Dwrite(dataset_id, mem_type_id, mem_space_id,
                                                                    file_space_id, xfer_plist_id, arrayBuffer);
            }
            else if (typeClass == HDF5Constants.H5T_STRING && H5Tis_variable_str(mem_type_id)) {
                // For variable-length string datatypes, convert to string pointers
                stringArray = VLDataConverter.convertVLStrings(arrayData, arena);
                status      = org.hdfgroup.javahdf5.hdf5_h.H5Dwrite(dataset_id, mem_type_id, mem_space_id,
                                                                    file_space_id, xfer_plist_id, stringArray);
            }
            else {
                // For VL datatypes, convert to hvl_t structures
                hvlArray = VLDataConverter.convertToHVL(arrayData, arena);
                status   = org.hdfgroup.javahdf5.hdf5_h.H5Dwrite(dataset_id, mem_type_id, mem_space_id,
                                                                 file_space_id, xfer_plist_id, hvlArray);
            }

            if (status < 0) {
                h5libraryError();
            }
        }
        catch (HDF5JavaException ex) {
            throw new HDF5LibraryException("VL data conversion failed: " + ex.getMessage());
        }
        catch (ClassCastException ex) {
            throw new HDF5LibraryException("Input data must be ArrayList array or String array: " +
                                           ex.getMessage());
        }
        finally {
            // CRITICAL: Reclaim VL memory after write (JNI pattern)
            // This allows HDF5 to properly free any VL memory it allocated during the write
            if ((status >= 0) && vl_data_class && hvlArray != null) {
                long space_for_reclaim = mem_space_id;
                try {
                    // If mem_space_id is H5S_ALL, we need to get the actual dataspace
                    if (mem_space_id == HDF5Constants.H5S_ALL) {
                        space_for_reclaim = org.hdfgroup.javahdf5.hdf5_h.H5Dget_space(dataset_id);
                    }

                    org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(
                        mem_type_id, space_for_reclaim, org.hdfgroup.javahdf5.hdf5_h.H5P_DEFAULT(), hvlArray);
                }
                catch (Exception reclaimEx) {
                    // Log but don't fail if reclaim has issues
                    System.err.println("Warning: H5Treclaim failed in H5DwriteVL: " + reclaimEx.getMessage());
                }
                finally {
                    // Close the space if we opened it
                    if (space_for_reclaim != mem_space_id && space_for_reclaim >= 0) {
                        try {
                            org.hdfgroup.javahdf5.hdf5_h.H5Sclose(space_for_reclaim);
                        }
                        catch (Exception closeEx) {
                            // Ignore close errors
                        }
                    }
                }
            }
        }

        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dwrite_VLStrings writes a (partial) variable length String dataset, specified by its identifier
     * dataset_id, from the application memory buffer buf into the file.
     *
     * ---- contributed by Rosetta Biosoftware
     *
     * @param dataset_id
     *            Identifier of the dataset read from.
     * @param mem_type_id
     *            Identifier of the memory datatype.
     * @param mem_space_id
     *            Identifier of the memory dataspace.
     * @param file_space_id
     *            Identifier of the dataset's dataspace in the file.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer with data to be written to the file.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/

    public static int H5Dwrite_VLStrings(long dataset_id, long mem_type_id, long mem_space_id,
                                         long file_space_id, long xfer_plist_id, Object[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("data buffer is null");
        }

        int status = -1;
        // CRITICAL FIX: Use global Arena to prevent automatic cleanup conflicts with HDF5 VL memory
        Arena arena = Arena.global();

        try {
            // Convert String array to VL format for HDF5
            String[] stringArray = (String[])buf;

            // Handle null strings - convert to empty strings
            for (int i = 0; i < stringArray.length; i++) {
                if (stringArray[i] == null) {
                    stringArray[i] = "";
                }
            }

            // For VL strings, we need to manually create hvl_t structures
            // VLDataConverter is for array types, not VL string types
            MemorySegment hvlArray = org.hdfgroup.javahdf5.hvl_t.allocateArray(stringArray.length, arena);

            for (int i = 0; i < stringArray.length; i++) {
                MemorySegment hvlElement = org.hdfgroup.javahdf5.hvl_t.asSlice(hvlArray, i);
                String str               = stringArray[i];

                if (str == null || str.isEmpty()) {
                    // Empty string: set length to 1 (for null terminator) and allocate single byte
                    org.hdfgroup.javahdf5.hvl_t.len(hvlElement, 1);
                    MemorySegment emptyStr = arena.allocate(1);
                    emptyStr.set(ValueLayout.JAVA_BYTE, 0, (byte)0);
                    org.hdfgroup.javahdf5.hvl_t.p(hvlElement, emptyStr);
                }
                else {
                    // Convert string to null-terminated C string
                    MemorySegment stringSegment = arena.allocateFrom(str, StandardCharsets.UTF_8);
                    // Set length to string length + 1 for null terminator
                    org.hdfgroup.javahdf5.hvl_t.len(hvlElement, str.length() + 1);
                    org.hdfgroup.javahdf5.hvl_t.p(hvlElement, stringSegment);
                }
            }

            status = org.hdfgroup.javahdf5.hdf5_h.H5Dwrite(dataset_id, mem_type_id, mem_space_id,
                                                           file_space_id, xfer_plist_id, hvlArray);

            if (status < 0) {
                h5libraryError();
            }
        }
        catch (HDF5JavaException ex) {
            throw new HDF5LibraryException("VL string conversion failed: " + ex.getMessage());
        }

        return status;
    }

    /**
     * @ingroup JH5D
     *
     * H5Dflush causes all buffers associated with a dataset to be immediately flushed to disk without
     * removing the data from the cache.
     *
     * @param dataset_id
     *            IN: Identifier of the dataset to be flushed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Dflush(long dataset_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Dflush(dataset_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5D
     *
     * H5Drefresh causes all buffers associated with a dataset to be cleared and immediately re-loaded with
     * updated contents from disk. This function essentially closes the dataset, evicts all metadata
     * associated with it from the cache, and then re-opens the dataset. The reopened dataset is automatically
     * re-registered with the same ID.
     *
     * @param dataset_id
     *            IN: Identifier of the dataset to be refreshed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Drefresh(long dataset_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Drefresh(dataset_id) < 0) {
            h5libraryError();
        }
    }

    // /////// unimplemented ////////
    // herr_t H5Ddebug(hid_t dset_id);
    // herr_t H5Dget_chunk_storage_size(hid_t dset_id, const hsize_t *offset, hsize_t *chunk_bytes);
    // herr_t H5Dformat_convert(hid_t dset_id);
    // herr_t H5Dget_chunk_index_type(hid_t did, H5D_chunk_index_t *idx_type);

    // herr_t H5Dgather(hid_t src_space_id, const void *src_buf, hid_t type_id,
    //                  size_t dst_buf_size, void *dst_buf, H5D_gather_func_t op, void *op_data);
    // herr_t H5Dscatter(H5D_scatter_func_t op, void *op_data, hid_t type_id, hid_t dst_space_id, void
    // *dst_buf);

    // ////////////////////////////////////////////////////////////
    // //
    // H5E: Error Stack //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     *
     * @defgroup JH5E Java Error (H5E) Interface
     *
     * @see H5E, C-API
     *
     * @see @ref H5E_UG, User Guide
     */

    /**
     * @ingroup JH5E
     *
     * H5Eauto_is_v2 determines whether the error auto reporting function for an error stack conforms to the
     * H5E_auto2_t typedef or the H5E_auto1_t typedef.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @return boolean true if the error stack conforms to H5E_auto2_t and false if it conforms to
     *            H5E_auto1_t.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Eauto_is_v2(long stack_id) throws HDF5LibraryException
    {
        boolean isV2 = false;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate MemorySegment for the int
            MemorySegment int_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            // Call the native method to check the error stack type
            if (org.hdfgroup.javahdf5.hdf5_h.H5Eauto_is_v2(stack_id, int_segment) < 0) {
                h5libraryError();
            }
            // Read the result from the MemorySegment
            isV2 = int_segment.get(ValueLayout.JAVA_INT, 0) != 0;
        }
        return isV2;
    }

    /**
     * @ingroup JH5E
     *
     * H5Eclear clears the error stack for the current thread. H5Eclear can fail if there are problems
     * initializing the library. <p> This may be used by exception handlers to assure that the error condition
     * in the HDF5 library has been reset.
     *
     * @return Returns a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Eclear() throws HDF5LibraryException
    {
        H5Eclear2(HDF5Constants.H5E_DEFAULT);
        return 0;
    }

    /**
     * @ingroup JH5E
     *
     * H5Eclear clears the error stack specified by estack_id, or, if estack_id is set to H5E_DEFAULT, the
     * error stack for the current thread.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Eclear(long stack_id) throws HDF5LibraryException { H5Eclear2(stack_id); }

    /**
     * @ingroup JH5E
     *
     * H5Eclear2 clears the error stack specified by estack_id, or, if estack_id is set to H5E_DEFAULT, the
     * error stack for the current thread.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Eclear2(long stack_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Eclear2(stack_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5E
     *
     * H5Eclose_msg closes an error message identifier, which can be either a major or minor message.
     *
     * @param err_id
     *            IN: Error message identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Eclose_msg(long err_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Eclose_msg(err_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5E
     *
     * H5Eclose_stack closes the object handle for an error stack and releases its resources.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Eclose_stack(long stack_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Eclose_stack(stack_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5E
     *
     * H5Ecreate_msg adds an error message to an error class defined by client library or application program.
     *
     * @param cls_id
     *            IN: Error class identifier.
     * @param msg_type
     *            IN: The type of the error message.
     * @param msg
     *            IN: The error message.
     *
     * @return a message identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            msg is null.
     **/
    public static long H5Ecreate_msg(long cls_id, int msg_type, String msg)
        throws HDF5LibraryException, NullPointerException
    {
        if (msg == null) {
            throw new NullPointerException("msg must not be null");
        }
        long msg_id = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate MemorySegment for the msg
            MemorySegment msg_segment = arena.allocateFrom(msg);
            msg_id = org.hdfgroup.javahdf5.hdf5_h.H5Ecreate_msg(cls_id, msg_type, msg_segment);
            if (msg_id < 0)
                h5libraryError();
        }
        return msg_id;
    }

    /**
     * @ingroup JH5E
     *
     * H5Ecreate_stack creates a new empty error stack and returns the new stack's identifier.
     *
     * @return an error stack identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Ecreate_stack() throws HDF5LibraryException
    {
        long stack_id = org.hdfgroup.javahdf5.hdf5_h.H5Ecreate_stack();
        if (stack_id < 0) {
            h5libraryError();
        }
        return stack_id;
    }

    /**
     * @ingroup JH5E
     *
     * H5Eget_class_name retrieves the name of the error class specified by the class identifier.
     *
     * @param class_id
     *            IN: Error class identifier.
     *
     * @return the name of the error class
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5Eget_class_name(long class_id) throws HDF5LibraryException
    {
        String className = null;
        long buf_size    = -1;
        if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Eget_class_name(class_id, MemorySegment.NULL, 0)) < 0)
            h5libraryError();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocate(buf_size + 1);
            // Call the native method to get the error class name
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Eget_class_name(class_id, name_segment,
                                                                           buf_size + 1)) < 0)
                h5libraryError();
            className = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return className;
    }

    /**
     * @ingroup JH5E
     *
     * H5Eget_current_stack copies the current error stack and returns an error stack identifier for the new
     * copy.
     *
     * @return an error stack identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Eget_current_stack() throws HDF5LibraryException
    {
        long stack_id = org.hdfgroup.javahdf5.hdf5_h.H5Eget_current_stack();
        if (stack_id < 0) {
            h5libraryError();
        }
        return stack_id;
    }

    /**
     * @ingroup JH5E
     *
     * H5Eset_current_stack replaces the content of the current error stack with a copy of the content of the
     * error stack specified by estack_id.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Eset_current_stack(long stack_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Eset_current_stack(stack_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5E
     *
     * H5Eget_msg retrieves the error message including its length and type.
     *
     * @param msg_id
     *            IN: Name of the error class.
     * @param type_list
     *            OUT: The type of the error message. Valid values are H5E_MAJOR and H5E_MINOR.
     *
     * @return the error message
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           type_list is null or not of length 1.
     **/
    public static String H5Eget_msg(long msg_id, int[] type_list)
        throws HDF5LibraryException, NullPointerException
    {
        if (type_list == null || type_list.length != 1) {
            throw new NullPointerException("type_list must be a non-null array of length 1");
        }
        String msg    = null;
        long buf_size = -1;
        if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Eget_msg(msg_id, MemorySegment.NULL,
                                                                MemorySegment.NULL, 0L)) < 0) {
            h5libraryError();
        }
        else if (buf_size > 0) {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment msg_segment       = arena.allocate(buf_size + 1);
                MemorySegment type_list_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
                // Call the native method to get the error message
                if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Eget_msg(msg_id, type_list_segment,
                                                                        msg_segment, buf_size + 1)) < 0)
                    h5libraryError();
                msg          = msg_segment.getString(0);
                type_list[0] = type_list_segment.get(ValueLayout.JAVA_INT, 0);
            }
        }

        return msg;
    }

    /**
     * @ingroup JH5E
     *
     * H5Eget_num retrieves the number of error records in the error stack specified by estack_id (including
     * major, minor messages and description).
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @return the number of error messages
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Eget_num(long stack_id) throws HDF5LibraryException, NullPointerException
    {
        long num = org.hdfgroup.javahdf5.hdf5_h.H5Eget_num(stack_id);
        if (num < 0) {
            h5libraryError();
        }
        return num;
    }

    /**
     * @ingroup JH5E
     *
     * H5Eprint2 prints the error stack specified by estack_id on the specified stream, stream.
     *
     * @param stack_id
     *            IN: Error stack identifier.If the identifier is H5E_DEFAULT, the current error stack will be
     *                printed.
     * @param stream
     *            IN: File pointer, or stderr if null.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Eprint2(long stack_id, Object stream) throws HDF5LibraryException
    {
        if (stream != null) {
            throw new HDF5FunctionArgumentException("Print error stack to file not implemented");
        }
        if (stack_id < 0)
            throw new HDF5FunctionArgumentException("Invalid error stack identifier: " + stack_id);
        // TODO need to add FILE* stream parameter handling
        org.hdfgroup.javahdf5.hdf5_h.H5Eprint2(stack_id, MemorySegment.NULL);
    }

    /**
     * @ingroup JH5E
     *
     * H5Epop deletes the number of error records specified in count from the top of the error stack specified
     * by estack_id (including major, minor messages and description).
     *
     * @param stack_id
     *            IN: Error stack identifier.
     * @param count
     *            IN: Version of the client library or application to which the error class belongs.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Epop(long stack_id, long count) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Epop(stack_id, count) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5E
     *
     * H5Epush pushes a new error record onto the error stack specified by estack_id.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     * @param file
     *            IN: Name of the file in which the error was detected.
     * @param func
     *            IN: Name of the function in which the error was detected.
     * @param line
     *            IN: Line number within the file at which the error was detected.
     * @param cls_id
     *            IN: Error class identifier.
     * @param maj_id
     *            IN: Major error identifier.
     * @param min_id
     *            IN: Minor error identifier.
     * @param msg
     *            IN: Error description string.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            file, func, or msg is null.
     **/
    public static void H5Epush(long stack_id, String file, String func, int line, long cls_id, long maj_id,
                               long min_id, String msg) throws HDF5LibraryException, NullPointerException
    {
        H5Epush2(stack_id, file, func, line, cls_id, maj_id, min_id, msg);
    }
    /**
     * @ingroup JH5E
     *
     * H5Epush2 pushes a new error record onto the error stack specified by estack_id.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     * @param file
     *            IN: Name of the file in which the error was detected.
     * @param func
     *            IN: Name of the function in which the error was detected.
     * @param line
     *            IN: Line number within the file at which the error was detected.
     * @param cls_id
     *            IN: Error class identifier.
     * @param maj_id
     *            IN: Major error identifier.
     * @param min_id
     *            IN: Minor error identifier.
     * @param msg
     *            IN: Error description string.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            file, func, or msg is null.
     **/
    public static void H5Epush2(long stack_id, String file, String func, int line, long cls_id, long maj_id,
                                long min_id, String msg) throws HDF5LibraryException, NullPointerException
    {
        if (file == null || func == null || msg == null) {
            throw new NullPointerException("file, func, or msg is null");
        }
        if (stack_id < 0)
            throw new HDF5FunctionArgumentException("Invalid error stack identifier: " + stack_id);
        if (cls_id < 0)
            throw new HDF5FunctionArgumentException("Invalid error class identifier: " + cls_id);
        if (maj_id < 0)
            throw new HDF5FunctionArgumentException("Invalid major error identifier: " + maj_id);
        if (min_id < 0)
            throw new HDF5FunctionArgumentException("Invalid minor error identifier: " + min_id);

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate MemorySegments to hold the string bytes
            MemorySegment file_segment = arena.allocateFrom(file);
            MemorySegment func_segment = arena.allocateFrom(func);
            MemorySegment msg_segment  = arena.allocateFrom(msg);

            // Create a variadic invoker with no additional arguments
            // The message string is treated as a format string with no format arguments
            var invoker = org.hdfgroup.javahdf5.hdf5_h.H5Epush2.makeInvoker();

            // Call the native method with empty variadic args
            if ((retVal = invoker.apply(stack_id, file_segment, func_segment, line, cls_id, maj_id, min_id,
                                        msg_segment)) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5E
     *
     * H5Eregister_class registers a client library or application program to the HDF5 error API so that the
     * client library or application program can report errors together with HDF5 library.
     *
     * @param cls_name
     *            IN: Name of the error class.
     * @param lib_name
     *            IN: Name of the client library or application to which the error class belongs.
     * @param version
     *            IN: Version of the client library or application to which the error class belongs.
     *
     * @return a class identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Eregister_class(String cls_name, String lib_name, String version)
        throws HDF5LibraryException, NullPointerException
    {
        if (cls_name == null || lib_name == null || version == null) {
            throw new NullPointerException("cls_name, lib_name, or version is null");
        }

        long class_id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment    = arena.allocateFrom(cls_name);
            MemorySegment lib_segment     = arena.allocateFrom(lib_name);
            MemorySegment version_segment = arena.allocateFrom(version);
            // Call the native method to register the error class
            class_id =
                org.hdfgroup.javahdf5.hdf5_h.H5Eregister_class(name_segment, lib_segment, version_segment);
            if (class_id < 0) {
                h5libraryError();
            }
        }
        return class_id;
    }

    /**
     * @ingroup JH5E
     *
     * H5Eunregister_class removes the error class specified by class_id.
     *
     * @param class_id
     *            IN: Error class identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Eunregister_class(long class_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Eunregister_class(class_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5E
     *
     * H5Ewalk walks the error stack specified by estack_id for the current thread and calls the
     * function specified in func for each error along the way.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     * @param direction
     *            IN: Direction in which the error stack is to be walked.
     * @param func
     *            IN: Function to be called for each error encountered.
     * @param client_data
     *            IN: Data to be passed with func.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            func is null.
     **/
    public static void H5Ewalk(long stack_id, long direction, hdf.hdf5lib.callbacks.H5E_walk_cb func,
                               hdf.hdf5lib.callbacks.H5E_walk_t client_data)
        throws HDF5LibraryException, NullPointerException
    {
        H5Ewalk2(stack_id, direction, func, client_data);
    }
    /**
     * @ingroup JH5E
     *
     * H5Ewalk2 walks the error stack specified by estack_id for the current thread and calls the
     * function specified in func for each error along the way.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     * @param direction
     *            IN: Direction in which the error stack is to be walked.
     * @param func
     *            IN: Function to be called for each error encountered.
     * @param client_data
     *            IN: Data to be passed with func.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            func is null.
     **/
    public static void H5Ewalk2(long stack_id, long direction, hdf.hdf5lib.callbacks.H5E_walk_cb func,
                                hdf.hdf5lib.callbacks.H5E_walk_t client_data)
        throws HDF5LibraryException, NullPointerException
    {
        if (func == null) {
            throw new NullPointerException("func must not be null");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment func_segment = H5E_walk2_t.allocate(func, arena);
            MemorySegment client_data_segment =
                Linker.nativeLinker().upcallStub(H5Ewalk2$handle(), H5Ewalk2$descriptor(), arena);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Ewalk2(stack_id, (int)direction, func_segment,
                                                      client_data_segment) < 0)
                h5libraryError();
        }
    }

    // /////// unimplemented ////////
    // public interface H5E_auto2_t extends Callback
    // {
    //         int callback(int estack, Pointer client_data);
    // }

    // int H5Eget_auto(long estack_id, H5E_auto2_t func, PointerByReference client_data);
    // {
    //         return H5Eget_auto2(estack_id, func, client_data);
    // }
    // int H5Eget_auto2(long estack_id, H5E_auto2_t func, PointerByReference client_data);

    // int H5Eset_auto(long estack_id, H5E_auto2_t func, Pointer client_data);
    // {
    //         return H5Eset_auto2(estack_id, func, client_data);
    // }
    // int H5Eset_auto2(long estack_id, H5E_auto2_t func, Pointer client_data);

    // public static void H5Epush(long err_stack, String file, String func, int line,
    //             long cls_id, long maj_id, long min_id, String msg, ...)
    // {
    //         H5Epush2(err_stack, file, func, line, cls_id, maj_id, min_id, msg, ...);
    // }
    // public  static  void H5Epush2(long err_stack, String file, String func, int line,
    //             long cls_id, long maj_id, long min_id, String msg, ...);

    // ////////////////////////////////////////////////////////////
    // //
    // H5ES: Event Set Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     *
     * @defgroup JH5ES Java Event Set (H5ES) Interface
     *
     * @see H5ES, C-API
     *
     * @see @ref H5ES_UG, User Guide
     */

    // /////// unimplemented ////////
    // H5_DLL hid_t H5EScreate(void);
    // H5_DLL herr_t H5ESwait(hid_t es_id, uint64_t timeout, size_t *num_in_progress, hbool_t *err_occurred);
    // H5_DLL herr_t H5EScancel(hid_t es_id, size_t *num_not_canceled, hbool_t *err_occurred);
    // H5_DLL herr_t H5ESget_count(hid_t es_id, size_t *count);
    // H5_DLL herr_t H5ESget_op_counter(hid_t es_id, uint64_t *counter);
    // H5_DLL herr_t H5ESget_err_status(hid_t es_id, hbool_t *err_occurred);
    // H5_DLL herr_t H5ESget_err_count(hid_t es_id, size_t *num_errs);
    // H5_DLL herr_t H5ESget_err_info(hid_t es_id, size_t num_err_info, H5ES_err_info_t err_info[],
    //                                size_t *err_cleared);
    // H5_DLL herr_t H5ESfree_err_info(size_t num_err_info, H5ES_err_info_t err_info[]);
    // H5_DLL herr_t H5ESregister_insert_func(hid_t es_id, H5ES_event_insert_func_t func, void *ctx);
    // H5_DLL herr_t H5ESregister_complete_func(hid_t es_id, H5ES_event_complete_func_t func, void *ctx);
    // H5_DLL herr_t H5ESclose(hid_t es_id);
    //

    // ////////////////////////////////////////////////////////////
    // //
    // H5F: File Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     *
     * @defgroup JH5F Java File (H5F) Interface
     *
     * @see H5F, C-API
     *
     * @see @ref H5F_UG, User Guide
     */

    /**
     * @ingroup JH5F
     *
     * H5Fclose terminates access to an HDF5 file.
     *
     * @param file_id
     *            Identifier of a file to terminate access to.
     *
     * @return a non-negative value if successful
     **/
    public static int H5Fclose(long file_id)
    {
        log.trace("OPEN_IDS: H5Fclose remove {}", file_id);
        OPEN_IDS.remove(file_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Fclose(file_id);
        if (retVal < 0)
            h5libraryError();
        return retVal;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fopen opens an existing file and is the primary function for accessing existing HDF5 files.
     *
     * @param name
     *            Name of the file to access.
     * @param flags
     *            File access flags.
     * @param access_id
     *            Identifier for the file access properties list.
     *
     * @return a file identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Fopen(String name, int flags, long access_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            id                         = org.hdfgroup.javahdf5.hdf5_h.H5Fopen(name_segment, flags, access_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Fopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5F
     *
     * H5Freopen reopens an HDF5 file.
     *
     * @param file_id
     *            Identifier of a file to terminate and reopen access to.
     *
     * @return a new file identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Freopen(long file_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Freopen(file_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Freopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fcreate is the primary function for creating HDF5 files.
     *
     * @param name
     *            Name of the file to access.
     * @param flags
     *            File access flags. Possible values include:
     *            <ul>
     *            <li>
     *            @ref H5F_ACC_RDWR Allow read and write access to file.</li>
     *            <li>
     *            @ref H5F_ACC_RDONLY Allow read-only access to file.</li>
     *            <li>
     *            @ref H5F_ACC_TRUNC Truncate file, if it already exists, erasing all data previously stored
     *                               in the file.</li>
     *            <li>
     *            @ref H5F_ACC_EXCL Fail if file already exists.</li>
     *            <li>
     *            @ref H5P_DEFAULT Apply default file access and creation properties.</li>
     *            </ul>
     *
     * @param create_id
     *            File creation property list identifier, used when modifying default file meta-data. Use
     *            H5P_DEFAULT for default access properties.
     * @param access_id
     *            File access property list identifier. If parallel file access is desired, this is a
     *            collective call according to the communicator stored in the access_id (not supported
     *            in Java). Use H5P_DEFAULT for default access properties.
     *
     * @return a file identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Fcreate(String name, int flags, long create_id, long access_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Fcreate(name_segment, flags, create_id, access_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Fcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fflush causes all buffers associated with a file or object to be immediately flushed (written) to
     * disk without removing the data from the (memory) cache. <p> After this call completes, the file (or
     * object) is in a consistent state and all data written to date is assured to be permanent.
     *
     * @param object_id
     *            Identifier of object used to identify the file. <b>object_id</b> can be any object
     *            associated with the file, including the file itself, a dataset, a group, an attribute,
     *            or a named data type.
     * @param scope
     *            specifies the scope of the flushing action, in the case that the HDF5 file is not a single
     *            physical file.
     *            <p> Valid values are:
     *            <UL>
     *            <LI> H5F_SCOPE_GLOBAL Flushes the entire virtual file.</LI>
     *            <LI> H5F_SCOPE_LOCAL Flushes only the specified file.</LI>
     *            </UL>
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Fflush(long object_id, int scope) throws HDF5LibraryException
    {
        int ret = org.hdfgroup.javahdf5.hdf5_h.H5Fflush(object_id, scope);
        if (ret < 0) {
            h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_access_plist returns the file access property list identifier of the specified file.
     *
     * @param file_id
     *            Identifier of file to get access property list of
     *
     * @return a file access property list identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Fget_access_plist(long file_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Fget_access_plist(file_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Fget_access_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_create_plist returns a file creation property list identifier identifying the creation
     * properties used to create this file.
     *
     * @param file_id
     *            Identifier of the file to get creation property list
     *
     * @return a file creation property list identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Fget_create_plist(long file_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Fget_create_plist(file_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Fget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_filesize retrieves the file size of the HDF5 file. This function
     *              is called after an existing file is opened in order
     *              to learn the true size of the underlying file.
     *
     * @param file_id
     *            IN: File identifier for a currently-open HDF5 file
     *
     * @return the file size of the HDF5 file
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Fget_filesize(long file_id) throws HDF5LibraryException
    {
        long filesize = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a buffer for the size
            MemorySegment sizeSegment = arena.allocate(ValueLayout.JAVA_LONG);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Fget_filesize(file_id, sizeSegment) < 0) {
                h5libraryError();
            }
            filesize = sizeSegment.get(ValueLayout.JAVA_LONG, 0);
        }
        return filesize;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_freespace returns the amount of space that is unused by any objects in the file.
     *
     * @param file_id
     *            IN: File identifier for a currently-open HDF5 file
     *
     * @return the amount of free space in the file
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Fget_freespace(long file_id) throws HDF5LibraryException
    {
        long freespace = org.hdfgroup.javahdf5.hdf5_h.H5Fget_freespace(file_id);
        if (freespace < 0) {
            h5libraryError();
        }
        return freespace;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_intent retrieves the intended access mode flag passed with H5Fopen when the file was opened.
     *
     * @param file_id
     *            IN: File identifier for a currently-open HDF5 file
     *
     * @return the intended access mode flag, as originally passed with H5Fopen.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Fget_intent(long file_id) throws HDF5LibraryException
    {
        int intent = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the intent value
            MemorySegment intentSegment = arena.allocate(ValueLayout.JAVA_INT);
            // Call the native method to get the intent
            if (org.hdfgroup.javahdf5.hdf5_h.H5Fget_intent(file_id, intentSegment) < 0) {
                h5libraryError();
            }
            intent = intentSegment.get(ValueLayout.JAVA_INT, 0);
        }

        return intent;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_fileno retrieves the "file number" for an open file.
     *
     * @param file_id
     *            IN: File identifier for a currently-open HDF5 file
     *
     * @return the unique file number for the file.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Fget_fileno(long file_id) throws HDF5LibraryException
    {
        long fileno = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a buffer for the size
            MemorySegment filenoSegment = arena.allocate(ValueLayout.JAVA_LONG);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Fget_fileno(file_id, filenoSegment) < 0) {
                h5libraryError();
            }
            fileno = filenoSegment.get(ValueLayout.JAVA_LONG, 0);
        }

        return fileno;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_mdc_hit_rate queries the metadata cache of the target file to obtain its hit rate (cache hits /
     * (cache hits + cache misses)) since the last time hit rate statistics were reset.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @return the double in which the hit rate is returned.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static double H5Fget_mdc_hit_rate(long file_id) throws HDF5LibraryException
    {
        double hit_rate;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a buffer for the size
            MemorySegment hit_rateSegment = arena.allocate(ValueLayout.JAVA_DOUBLE);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Fget_mdc_hit_rate(file_id, hit_rateSegment) < 0) {
                h5libraryError();
            }
            hit_rate = hit_rateSegment.get(ValueLayout.JAVA_DOUBLE, 0);
        }

        return hit_rate;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_mdc_size queries the metadata cache of the target file for the desired size information.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     * @param metadata_cache
     *            OUT: Current metadata cache information
     *            <ul>
     *            <li>metadata_cache[0] = max_size_ptr // current cache maximum size</li>
     *            <li>metadata_cache[1] = min_clean_size_ptr // current cache minimum clean size</li>
     *            <li>metadata_cache[2] = cur_size_ptr // current cache size</li>
     *            </ul>
     *
     * @return current number of entries in the cache
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            metadata_cache is null.
     **/
    public static int H5Fget_mdc_size(long file_id, long[] metadata_cache)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (metadata_cache == null) {
            throw new NullPointerException("metadata_cache is null");
        }
        if (metadata_cache.length < 3) {
            throw new HDF5FunctionArgumentException("metadata_cache must have at least 3 elements");
        }
        int retVal = -1;

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the array bytes
            MemorySegment max_size_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, metadata_cache[0]);
            MemorySegment min_clean_size_segment =
                arena.allocateFrom(ValueLayout.JAVA_LONG, metadata_cache[1]);
            MemorySegment cur_size_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, metadata_cache[2]);
            MemorySegment cur_num_entries_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Fget_mdc_size(file_id, max_size_segment,
                                                             min_clean_size_segment, cur_size_segment,
                                                             cur_num_entries_segment) < 0)
                h5libraryError();

            // Set the version numbers
            metadata_cache[0] = max_size_segment.get(ValueLayout.JAVA_LONG, 0);
            metadata_cache[1] = min_clean_size_segment.get(ValueLayout.JAVA_LONG, 0);
            metadata_cache[2] = cur_size_segment.get(ValueLayout.JAVA_LONG, 0);
            retVal            = cur_num_entries_segment.get(ValueLayout.JAVA_INT, 0);
        }

        return retVal;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_name retrieves the name of the file to which the object obj_id belongs.
     *
     * @param obj_id
     *            IN: Identifier of the object for which the associated filename is sought.
     *
     * @return the filename.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5Fget_name(long obj_id) throws HDF5LibraryException
    {
        long name_size = -1;
        if ((name_size = org.hdfgroup.javahdf5.hdf5_h.H5Fget_name(obj_id, MemorySegment.NULL, 0)) < 0)
            h5libraryError();

        String ret_name = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocate(name_size + 1);
            /* Get the attribute name */
            if (org.hdfgroup.javahdf5.hdf5_h.H5Fget_name(obj_id, name_segment, name_size + 1) < 0)
                h5libraryError();

            ret_name = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return ret_name;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_obj_count returns the number of open object identifiers for the file.
     *
     * @param file_id
     *            IN: File identifier for a currently-open HDF5 file
     * @param types
     *            IN: Type of object for which identifiers are to be returned.
     *            <ul>
     *            <li>H5F_OBJ_FILE Files only</li>
     *            <li>H5F_OBJ_DATASET Datasets only</li>
     *            <li>H5F_OBJ_GROUP Groups only</li>
     *            <li>H5F_OBJ_DATATYPE Named datatypes only</li>
     *            <li>H5F_OBJ_ATTR Attributes only</li>
     *            <li>H5F_OBJ_ALL All of the above</li>
     *            <li>H5F_OBJ_LOCAL Restrict search to objects opened through current file identifier.</li>
     *            </ul>
     *
     * @return the number of open objects.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Fget_obj_count(long file_id, int types) throws HDF5LibraryException
    {
        long count_size = -1;
        if ((count_size = org.hdfgroup.javahdf5.hdf5_h.H5Fget_obj_count(file_id, types)) < 0)
            h5libraryError();

        return count_size;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_obj_ids returns the list of identifiers for all open HDF5 objects fitting the specified
     * criteria.
     *
     * @param file_id
     *            IN: File identifier for a currently-open HDF5 file
     * @param types
     *            IN: Type of object for which identifiers are to be returned.
     * @param max_objs
     *            IN: Maximum number of object identifiers to place into obj_id_list.
     * @param obj_id_list
     *            OUT: Pointer to the returned list of open object identifiers.
     *
     * @return the number of objects placed into obj_id_list.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            obj_id_list is null.
     **/
    public static long H5Fget_obj_ids(long file_id, int types, long max_objs, long[] obj_id_list)
        throws HDF5LibraryException, NullPointerException
    {
        if (obj_id_list == null) {
            throw new NullPointerException("obj_id_list is null");
        }
        long retCount = -1;

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the array bytes
            MemorySegment obj_id_list_segment = arena.allocate(ValueLayout.JAVA_LONG, max_objs);
            if ((retCount = org.hdfgroup.javahdf5.hdf5_h.H5Fget_obj_ids(file_id, types, max_objs,
                                                                        obj_id_list_segment)) < 0)
                h5libraryError();
            // Read the data from the memory segment
            MemorySegment.copy(obj_id_list_segment, ValueLayout.JAVA_LONG, 0L, obj_id_list, 0, (int)retCount);
        }

        return retCount;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fis_hdf5 determines whether a file is in the HDF5 format.
     *
     * @param name
     *            File name to check format.
     *
     * @return true if is HDF5, false if not.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     *
     * @deprecated As of HDF5 1.10.5 in favor of H5Fis_accessible.
     **/
    @Deprecated
    public static boolean H5Fis_hdf5(String name) throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        boolean isHDF5 = false;
        try (Arena arena = Arena.ofConfined()) {
            int retVal = -1;
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            // Call the native method to check the file type
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Fis_hdf5(name_segment)) < 0) {
                h5libraryError();
            }
            if (retVal > 0)
                isHDF5 = true;
            else
                isHDF5 = false;
        }
        return isHDF5;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fis_accessible determines if the file can be opened with the given fapl.
     *
     * @param name
     *            IN: File name to check.
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return true if file is accessible, false if not.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static boolean H5Fis_accessible(String name, long fapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        boolean isAccessible = false;
        try (Arena arena = Arena.ofConfined()) {
            int retVal = -1;
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Fis_accessible(name_segment, fapl_id)) < 0)
                h5libraryError();
            if (retVal > 0)
                isAccessible = true;
            else
                isAccessible = false;
        }

        return isAccessible;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fmount mounts the file specified by child_id onto the group specified by loc_id and name using the
     * mount properties plist_id.
     *
     * @param loc_id
     *            The identifier for the group onto which the file specified by child_id is to be mounted.
     * @param name
     *            The name of the group onto which the file specified by child_id is to be mounted.
     * @param child_id
     *            The identifier of the file to be mounted.
     * @param plist_id
     *            The identifier of the property list to be used.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Fmount(long loc_id, String name, long child_id, long plist_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);

            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Fmount(loc_id, name_segment, child_id, plist_id)) <
                0)
                h5libraryError();
        }

        return retVal;
    }

    /**
     * @ingroup JH5F
     *
     * Given a mount point, H5Funmount disassociates the mount point's file from the file mounted there.
     *
     * @param loc_id
     *            The identifier for the location at which the specified file is to be unmounted.
     * @param name
     *            The name of the file to be unmounted.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Funmount(long loc_id, String name) throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);

            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Funmount(loc_id, name_segment)) < 0)
                h5libraryError();
        }

        return retVal;
    }

    /**
     * @ingroup JH5F
     *
     * H5Freset_mdc_hit_rate_stats resets the hit rate statistics counters in the metadata cache associated
     * with the specified file.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Freset_mdc_hit_rate_stats(long file_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Freset_mdc_hit_rate_stats(file_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_info returns global information for the file associated with the
     * object identifier obj_id.
     *
     * @param obj_id IN: Object identifier for any object in the file.
     *
     * @return A buffer(H5F_info2_t) for current "global" information about file
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static hdf.hdf5lib.structs.H5F_info2_t H5Fget_info(long obj_id) throws HDF5LibraryException
    {
        hdf.hdf5lib.structs.H5F_info2_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment finfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5F_info2_t.sizeof());
            if (H5Fget_info2(obj_id, finfo_segment) < 0) {
                h5libraryError();
            }
            // Unpack the H5F_info2_t from the MemorySegment
            info = new hdf.hdf5lib.structs.H5F_info2_t(finfo_segment);
        }
        return info;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fclear_elink_file_cache evicts all the cached child files in the specified file's external file
     * cache, causing them to be closed if there is nothing else holding them open.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Fclear_elink_file_cache(long file_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Fclear_elink_file_cache(file_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5F
     *
     * H5Fstart_swmr_write will activate SWMR writing mode for a file associated with file_id. This routine
     * will prepare and ensure the file is safe for SWMR writing.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Fstart_swmr_write(long file_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Fstart_swmr_write(file_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5F
     *
     * H5Fstart_mdc_logging starts logging metadata cache events if logging was previously enabled.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Fstart_mdc_logging(long file_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Fstart_mdc_logging(file_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5F
     *
     * H5Fstop_mdc_logging stops logging metadata cache events if logging was previously enabled and is
     * currently ongoing.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Fstop_mdc_logging(long file_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Fstop_mdc_logging(file_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_mdc_logging_status gets the current metadata cache logging status.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @param mdc_logging_status
     *          the status
     *             mdc_logging_status[0] = is_enabled, whether logging is enabled
     *             mdc_logging_status[1] = is_currently_logging, whether events are currently being logged
     *
     * @exception HDF5LibraryException
     *             Error from the HDF5 Library.
     * @exception NullPointerException
     *            mdc_logging_status is null.
     **/
    public static void H5Fget_mdc_logging_status(long file_id, boolean[] mdc_logging_status)
        throws HDF5LibraryException, NullPointerException
    {
        if (mdc_logging_status == null || mdc_logging_status.length < 2) {
            throw new NullPointerException("mdc_logging_status is null or has insufficient length");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment is_enabled_segment           = arena.allocate(ValueLayout.JAVA_BOOLEAN.byteSize());
            MemorySegment is_currently_logging_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN.byteSize());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Fget_mdc_logging_status(file_id, is_enabled_segment,
                                                                       is_currently_logging_segment) < 0)
                h5libraryError();
            mdc_logging_status[0] = is_enabled_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
            mdc_logging_status[1] = is_currently_logging_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
    }

    /**
     * @ingroup JH5F
     *
     * H5Fget_dset_no_attrs_hint gets the file-level setting to create minimized dataset object headers.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @return true if the file-level is set to create minimized dataset object headers, false if not.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Fget_dset_no_attrs_hint(long file_id) throws HDF5LibraryException
    {
        boolean minimize = false;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate MemorySegment for the int
            MemorySegment minimize_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            // Call the native method to check the error stack type
            if (org.hdfgroup.javahdf5.hdf5_h.H5Fget_dset_no_attrs_hint(file_id, minimize_segment) < 0) {
                h5libraryError();
            }
            // Read the result from the MemorySegment
            minimize = minimize_segment.get(ValueLayout.JAVA_INT, 0) != 0;
        }
        return minimize;
    }

    /**
     * @ingroup JH5F
     *
     * H5Fset_dset_no_attrs_hint sets the file-level setting to create minimized dataset object headers.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     * @param minimize
     *            the minimize hint setting
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Fset_dset_no_attrs_hint(long file_id, boolean minimize) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Fset_dset_no_attrs_hint(file_id, minimize) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5F
     *
     * H5Fset_libver_bounds sets a different low and high bounds while a file is open.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     * @param low
     *            IN: The earliest version of the library that will be used for writing objects
     * @param high
     *            IN: The latest version of the library that will be used for writing objects.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Fset_libver_bounds(long file_id, int low, int high) throws HDF5LibraryException
    {
        if (low < 0 || high < 0) {
            throw new HDF5FunctionArgumentException("low and high must be non-negative");
        }
        if (org.hdfgroup.javahdf5.hdf5_h.H5Fset_libver_bounds(file_id, low, high) < 0)
            h5libraryError();
    }

    // /////// unimplemented ////////
    //  herr_t H5Fget_eoa(hid_t file_id, haddr_t *eoa);
    //  herr_t H5Fincrement_filesize(hid_t file_id, hsize_t increment);
    // ssize_t H5Fget_file_image(hid_t file_id, void * buf_ptr, size_t buf_len);
    // herr_t H5Fget_metadata_read_retry_info(hid_t file_id, H5F_retry_info_t *info);
    // ssize_t H5Fget_free_sections(hid_t file_id, H5F_mem_t type, size_t nsects, H5F_sect_info_t
    // *sect_info/*out*/);
    //  herr_t H5Fformat_convert(hid_t fid);
    //  herr_t H5Freset_page_buffering_stats(hid_t file_id);
    //  herr_t H5Fget_page_buffering_stats(hid_t file_id, unsigned accesses[2],
    //     unsigned hits[2], unsigned misses[2], unsigned evictions[2], unsigned bypasses[2]);
    //  herr_t H5Fget_mdc_image_info(hid_t file_id, haddr_t *image_addr, hsize_t *image_size);
    // #ifdef H5_HAVE_PARALLEL
    //    herr_t H5Fset_mpi_atomicity(hid_t file_id, hbool_t flag);
    //    herr_t H5Fget_mpi_atomicity(hid_t file_id, hbool_t *flag);
    // #endif /* H5_HAVE_PARALLEL */

    // /*
    // * H5Fget_vfd_handle returns a pointer to the file handle from the
    // low-level file driver
    // * currently being used by the HDF5 library for file I/O.
    // *
    // * @param file_id IN: Identifier of the file to be queried.
    // * @param fapl IN: File access property list identifier.
    // *
    // * @return a pointer to the file handle being used by the low-level
    // virtual file driver.
    // *
    // * @exception HDF5LibraryException - Error from the HDF5 Library.
    // **/
    // public  static  Pointer file_handle
    // H5Fget_vfd_handle(int file_id, int fapl)
    //             throws HDF5LibraryException {}

    // /*
    // * H5Fget_mdc_config loads the current metadata cache configuration into
    // * the instance of H5AC_cache_config_t pointed to by the config_ptr
    // parameter.
    // *
    // * @param file_id IN: Identifier of the target file
    // * @param config_ptr IN/OUT: Pointer to the instance of
    // H5AC_cache_config_t in which the current metadata cache configuration is to be reported.
    // *
    // * @return none
    // *
    // * @exception HDF5LibraryException - Error from the HDF5 Library.
    // * @exception NullPointerException - config_ptr is null.
    // **/
    // public  static  void H5Fget_mdc_config(int file_id, H5AC_cache_config_t config_ptr)
    //             throws HDF5LibraryException, NullPointerException {}

    // /*
    // * H5Fset_mdc_config attempts to configure the file's metadata cache
    // according to the configuration supplied.
    // *
    // * @param file_id IN: Identifier of the target file
    // * @param config_ptr IN: Pointer to the instance of H5AC_cache_config_t
    // containing the desired configuration.
    // *
    // * @return none
    // *
    // * @exception HDF5LibraryException - Error from the HDF5 Library.
    // * @exception NullPointerException - config_ptr is null.
    // **/
    // public  static  int H5Fset_mdc_config(int file_id, H5AC_cache_config_t config_ptr)
    //             throws HDF5LibraryException, NullPointerException {}

    // ////////////////////////////////////////////////////////////
    // //
    // H5FD: File Driver Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // /////// unimplemented ////////
    //  hid_t H5FDregister(const H5FD_class_t *cls);
    //  herr_t H5FDunregister(hid_t driver_id);
    //  H5FD_t *H5FDopen(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
    //  herr_t H5FDclose(H5FD_t *file);
    //  int H5FDcmp(const H5FD_t *f1, const H5FD_t *f2);
    //  int H5FDquery(const H5FD_t *f, unsigned long *flags);
    //  haddr_t H5FDalloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
    //  herr_t H5FDfree(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, hsize_t size);
    //  haddr_t H5FDget_eoa(H5FD_t *file, H5FD_mem_t type);
    //  herr_t H5FDset_eoa(H5FD_t *file, H5FD_mem_t type, haddr_t eoa);
    //  haddr_t H5FDget_eof(H5FD_t *file, H5FD_mem_t type);
    //  herr_t H5FDget_vfd_handle(H5FD_t *file, hid_t fapl, void**file_handle);
    //  herr_t H5FDread(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size, void
    //  *buf/*out*/); herr_t H5FDwrite(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t
    //  size, const void *buf); herr_t H5FDflush(H5FD_t *file, hid_t dxpl_id, hbool_t closing); herr_t
    //  H5FDtruncate(H5FD_t *file, hid_t dxpl_id, hbool_t closing); herr_t H5FDlock(H5FD_t *file, hbool_t rw);
    //  herr_t H5FDunlock(H5FD_t *file);
    //  herr_t H5FDdriver_query(hid_t driver_id, unsigned long *flags/*out*/);

    // ////////////////////////////////////////////////////////////
    // //
    // H5FS: File Free Space Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // No public Functions

    // ////////////////////////////////////////////////////////////
    // //
    // H5G: Group Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5G Java Group (H5G) Interface
     *
     * @see H5G, C-API
     *
     * @see @ref H5G_UG, User Guide
     **/

    /**
     * @ingroup JH5G
     *
     * H5Gclose releases resources used by a group which was opened by a call to H5Gcreate() or H5Gopen().
     *
     * @param group_id
     *            Group identifier to release.
     *
     * @return a non-negative value if successful
     **/
    public static int H5Gclose(long group_id)
    {
        log.trace("OPEN_IDS: H5Gclose remove {}", group_id);
        OPEN_IDS.remove(group_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Gclose(group_id);
        if (retVal < 0)
            h5libraryError();
        return retVal;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gcreate creates a new group with the specified name at the specified location, loc_id.
     *
     * @param loc_id
     *            IN: The file or group identifier.
     * @param name
     *            IN: The absolute or relative name of the new group.
     * @param lcpl_id
     *            IN: Identifier of link creation property list.
     * @param gcpl_id
     *            IN: Identifier of group creation property list.
     * @param gapl_id
     *            IN: Identifier of group access property list. (No group access properties have been
     *implemented at this time; use H5P_DEFAULT.)
     *
     * @return a valid group identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Gcreate(long loc_id, String name, long lcpl_id, long gcpl_id, long gapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Gcreate2(loc_id, name_segment, lcpl_id, gcpl_id, gapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Gcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gcreate_anon creates a new empty group in the file specified by loc_id.
     *
     * @param loc_id
     *            IN: File or group identifier specifying the file in which the new group is to be created.
     * @param gcpl_id
     *            IN: Identifier of group creation property list.
     * @param gapl_id
     *            IN: Identifier of group access property list. (No group access properties have been
     *                implemented at this time; use H5P_DEFAULT.)
     *
     * @return a valid group identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Gcreate_anon(long loc_id, long gcpl_id, long gapl_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Gcreate_anon(loc_id, gcpl_id, gapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Gcreate_anon add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gget_create_plist returns an identifier for the group creation property list associated with the
     * group specified by group_id.
     *
     * @param group_id
     *            IN: Identifier of the group.
     *
     * @return an identifier for the group's creation property list
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Gget_create_plist(long group_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Gget_create_plist(group_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gget_info retrieves information about the group specified by group_id. The information is returned in
     * the group_info struct.
     *
     * @param group_id
     *            IN: Identifier of the group.
     *
     * @return a structure in which group information is returned
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static hdf.hdf5lib.structs.H5G_info_t H5Gget_info(long group_id) throws HDF5LibraryException
    {
        hdf.hdf5lib.structs.H5G_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ginfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5G_info_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Gget_info(group_id, ginfo_segment) < 0) {
                h5libraryError();
            }
            // Unpack the H5G_info_t from the MemorySegment
            info = new hdf.hdf5lib.structs.H5G_info_t(
                org.hdfgroup.javahdf5.H5G_info_t.storage_type(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.nlinks(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.max_corder(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.mounted(ginfo_segment));
        }
        return info;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gget_info_by_idx retrieves information about a group, according to the group's position within an
     * index.
     *
     * @param group_id
     *            IN: File or group identifier.
     * @param group_name
     *            IN: Name of group for which information is to be retrieved.
     * @param idx_type
     *            IN: Type of index by which objects are ordered
     * @param order
     *            IN: Order of iteration within index
     * @param n
     *            IN: Attribute's position in index
     * @param lapl_id
     *            IN: Link access property list.
     *
     * @return a structure in which group information is returned
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5G_info_t
    H5Gget_info_by_idx(long group_id, String group_name, int idx_type, int order, long n, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("name is null");
        }

        hdf.hdf5lib.structs.H5G_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom(group_name);
            MemorySegment ginfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5G_info_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Gget_info_by_idx(group_id, name_segment, idx_type, order, n,
                                                                ginfo_segment, lapl_id) < 0) {
                h5libraryError();
            }
            // Unpack the H5G_info_t from the MemorySegment
            info = new hdf.hdf5lib.structs.H5G_info_t(
                org.hdfgroup.javahdf5.H5G_info_t.storage_type(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.nlinks(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.max_corder(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.mounted(ginfo_segment));
            log.trace("H5Gget_info_by_idx: type={}", info.storage_type);
        }
        return info;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gget_info_by_name retrieves information about the group group_name located in the file or group
     * specified by loc_id.
     *
     * @param group_id
     *            IN: File or group identifier.
     * @param name
     *            IN: Name of group for which information is to be retrieved.
     * @param lapl_id
     *            IN: Link access property list.
     *
     * @return a structure in which group information is returned
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5G_info_t H5Gget_info_by_name(long group_id, String name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        hdf.hdf5lib.structs.H5G_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom(name);
            MemorySegment ginfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5G_info_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Gget_info_by_name(group_id, name_segment, ginfo_segment,
                                                                 lapl_id) < 0) {
                h5libraryError();
            }
            // Unpack the H5G_info_t from the MemorySegment
            info = new hdf.hdf5lib.structs.H5G_info_t(
                org.hdfgroup.javahdf5.H5G_info_t.storage_type(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.nlinks(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.max_corder(ginfo_segment),
                org.hdfgroup.javahdf5.H5G_info_t.mounted(ginfo_segment));
            log.trace("H5Gget_info_by_name: type={}", info.storage_type);
        }
        return info;
    }

    /**
     * @ingroup JH5G
     *
     * retrieves information of all objects under the group (name) located in the file or group specified by
     * loc_id.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param name
     *            IN: Name of group for which information is to be retrieved
     * @param objNames
     *            OUT: Names of all objects under the group, name.
     * @param objTypes
     *            OUT: Types of all objects under the group, name.
     * @param tokens
     *            OUT: Object token of all objects under the group, name.
     *
     * @return the number of items found
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     */
    public static int H5Gget_obj_info_all(long loc_id, String name, String[] objNames, int[] objTypes,
                                          H5O_token_t[] tokens)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Gget_obj_info_all(loc_id, name, objNames, objTypes, null, null, tokens,
                                   HDF5Constants.H5_INDEX_NAME);
    }

    /**
     * @ingroup JH5G
     *
     * retrieves information of all objects under the group (name) located in the file or group specified by
     * loc_id.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param name
     *            IN: Name of group for which information is to be retrieved
     * @param objNames
     *            OUT: Names of all objects under the group, name.
     * @param objTypes
     *            OUT: Types of all objects under the group, name.
     * @param ltype
     *            OUT: Link type
     * @param tokens
     *            OUT: Object token of all objects under the group, name.
     * @param indx_type
     *            IN: Index type for iterate
     *
     * @return the number of items found
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     */
    public static int H5Gget_obj_info_all(long loc_id, String name, String[] objNames, int[] objTypes,
                                          int[] ltype, H5O_token_t[] tokens, int indx_type)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Gget_obj_info_full(loc_id, name, objNames, objTypes, ltype, null, tokens, indx_type, -1);
    }

    /**
     * @ingroup JH5G
     *
     * retrieves information of all objects under the group (name) located in the file or group specified by
     * loc_id.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param name
     *            IN: Name of group for which information is to be retrieved
     * @param objNames
     *            OUT: Names of all objects under the group, name.
     * @param objTypes
     *            OUT: Types of all objects under the group, name.
     * @param ltype
     *            OUT: Link type
     * @param fno
     *            OUT: File number
     * @param tokens
     *            OUT: Object token of all objects under the group, name.
     * @param indx_type
     *            IN: Index type for iterate
     *
     * @return the number of items found
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     */
    public static int H5Gget_obj_info_all(long loc_id, String name, String[] objNames, int[] objTypes,
                                          int[] ltype, long[] fno, H5O_token_t[] tokens, int indx_type)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Gget_obj_info_full(loc_id, name, objNames, objTypes, ltype, fno, tokens, indx_type, -1);
    }

    /**
     * @ingroup JH5G
     *
     * retrieves information of all objects under the group (name) located in the file or group specified by
     * loc_id.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param name
     *            IN: Name of group for which information is to be retrieved
     * @param objNames
     *            OUT: Names of all objects under the group, name.
     * @param objTypes
     *            OUT: Types of all objects under the group, name.
     * @param ltype
     *            OUT: Link type
     * @param fno
     *            OUT: File number
     * @param tokens
     *            OUT: Object token of all objects under the group, name.
     * @param indx_type
     *            IN: Index type for iterate
     * @param indx_order
     *            IN: Index order for iterate
     *
     * @return the number of items found
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     */
    public static int H5Gget_obj_info_full(long loc_id, String name, String[] objNames, int[] objTypes,
                                           int[] ltype, long[] fno, H5O_token_t[] tokens, int indx_type,
                                           int indx_order) throws HDF5LibraryException, NullPointerException
    {
        if (objNames == null) {
            throw new NullPointerException("name array is null");
        }
        if (objTypes == null) {
            throw new NullPointerException("object type array is null");
        }
        if (objNames.length == 0) {
            throw new HDF5LibraryException("H5Gget_obj_info_full(): array size is zero");
        }
        if (objNames.length != objTypes.length) {
            throw new HDF5LibraryException("H5Gget_obj_info_full(): name and type array sizes are different");
        }
        if (ltype == null)
            ltype = new int[objTypes.length];
        if (fno == null)
            fno = new long[tokens.length];
        if (indx_type < 0)
            indx_type = HDF5Constants.H5_INDEX_NAME;
        if (indx_order < 0)
            indx_order = HDF5Constants.H5_ITER_INC;

        log.trace("H5Gget_obj_info_full: objNames_len={}", objNames.length);
        int status = H5Gget_obj_info_full(loc_id, name, objNames, objTypes, ltype, fno, tokens,
                                          objNames.length, indx_type, indx_order);
        for (int indx = 0; indx < objNames.length; indx++)
            log.trace("H5Gget_obj_info_full: objNames={}", objNames[indx]);
        return status;
    }

    /*
     * NOTE: This is a dangerous call! The caller can supply any value they'd like
     * for 'n' and if it exceeds the number of links in the group, we will most likely
     * end up overwriting memory heap-tracking info.
     */
    private static int H5Gget_obj_info_full(long loc_id, String group_name, String[] objNames, int[] objTypes,
                                            int[] ltype, long[] fno, H5O_token_t[] tokens, int n,
                                            int indx_type, int indx_order)
        throws HDF5LibraryException, NullPointerException
    {
        if (objNames == null) {
            throw new NullPointerException("objNames is null");
        }
        if (objTypes == null) {
            throw new NullPointerException("objTypes is null");
        }
        if (ltype == null) {
            throw new NullPointerException("ltype ais null");
        }
        if (fno == null) {
            throw new NullPointerException("fno is null");
        }
        if (tokens == null) {
            throw new NullPointerException("tokens is null");
        }
        if (n < 0) {
            throw new HDF5FunctionArgumentException("n is negative");
        }
        long gid = HDF5Constants.H5I_INVALID_HID;
        if (group_name != null) {
            gid = hdf.hdf5lib.H5.H5Gopen(loc_id, group_name, HDF5Constants.H5P_DEFAULT);
        }
        else {
            gid = loc_id;
        }

        StructLayout info_ptr_t = MemoryLayout.structLayout(
            ValueLayout.ADDRESS.withName("objname"), ValueLayout.ADDRESS.withName("obj_token"),
            ValueLayout.JAVA_LONG.withName("fno"), ValueLayout.JAVA_INT.withName("otype"),
            ValueLayout.JAVA_INT.withName("ltype"));

        StructLayout info_all_t = MemoryLayout.structLayout(
            MemoryLayout.sequenceLayout(n, MemoryLayout
                                               .structLayout(ValueLayout.ADDRESS.withName("objname"),
                                                             ValueLayout.ADDRESS.withName("obj_token"),
                                                             ValueLayout.JAVA_LONG.withName("fno"),
                                                             ValueLayout.JAVA_INT.withName("otype"),
                                                             ValueLayout.JAVA_INT.withName("ltype"))
                                               .withName("data")),
            ValueLayout.JAVA_LONG.withName("idxnum"), ValueLayout.JAVA_INT.withName("count"));

        long DATA_OFFSET          = 0L; // info_all_t.byteOffset(PathElement.groupElement("data"));
        VarHandle objnameHandle   = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("objname"));
        VarHandle otypeHandle     = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("otype"));
        VarHandle ltypeHandle     = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("ltype"));
        VarHandle obj_tokenHandle = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("obj_token"));
        VarHandle fnoHandle       = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("fno"));
        VarHandle idxnumHandle    = info_all_t.varHandle(PathElement.groupElement("idxnum"));
        VarHandle countHandle     = info_all_t.varHandle(PathElement.groupElement("count"));

        int ret = -1;
        try (Arena arena = Arena.ofConfined()) {
            class H5L_iter_callback implements H5L_iterate_t {
                public int apply(long group, MemorySegment name, MemorySegment info, MemorySegment op_data)
                {
                    int count              = (int)countHandle.get(op_data, 0);
                    MemorySegment name_seg = arena.allocateFrom(name.getString(0));
                    objnameHandle.set(op_data, DATA_OFFSET, (long)count, name_seg);
                    int ltype = (int)H5L_info2_t.type(info);
                    ltypeHandle.set(op_data, DATA_OFFSET, (long)count, ltype);

                    int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Oexists_by_name(loc_id, name,
                                                                                HDF5Constants.H5P_DEFAULT);
                    if (retVal < 0) {
                        h5libraryError();
                    }
                    else if (retVal > 0) {
                        MemorySegment info_segment = arena.allocate(H5O_info2_t.sizeof());
                        if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_info_by_name3(loc_id, name, info_segment,
                                                                              HDF5Constants.H5O_INFO_ALL,
                                                                              HDF5Constants.H5P_DEFAULT) < 0)
                            h5libraryError();
                        int otype = (int)H5O_info2_t.type(info_segment);
                        otypeHandle.set(op_data, DATA_OFFSET, (long)count, otype);
                        obj_tokenHandle.set(op_data, DATA_OFFSET, (long)count,
                                            H5O_info2_t.token(info_segment));
                        long fno = (long)H5O_info2_t.fileno(info_segment);
                        fnoHandle.set(op_data, DATA_OFFSET, (long)count, fno);
                    }
                    else {
                        otypeHandle.set(op_data, DATA_OFFSET, (long)count, HDF5Constants.H5O_TYPE_UNKNOWN);
                        obj_tokenHandle.set(op_data, DATA_OFFSET, (long)count, MemorySegment.NULL);
                        fnoHandle.set(op_data, DATA_OFFSET, (long)count, -1L);
                    }

                    count++;
                    countHandle.set(op_data, 0, count); // count
                    return 0;
                }
            }
            H5L_iterate_t obj_info_all = new H5L_iter_callback();
            MemorySegment info         = arena.allocate(info_all_t);

            // Set up the info struct
            idxnumHandle.set(info, 0L, 0); // idxnum
            countHandle.set(info, 0L, 0);  // count

            MemorySegment op_segment = H5L_iterate2_t.allocate(obj_info_all, arena);
            // Call H5Literate2
            if (org.hdfgroup.javahdf5.hdf5_h.H5Literate2(gid, indx_type, indx_order, MemorySegment.NULL,
                                                         op_segment, info) < 0) {
                /*
                 * Reset info stats; most importantly, reset the count.
                 */
                idxnumHandle.set(info, 0, 0); // idxnum
                countHandle.set(info, 0, 0);  // count

                /* Iteration failed, try normal alphabetical order */
                if (org.hdfgroup.javahdf5.hdf5_h.H5Literate2(gid, HDF5Constants.H5_INDEX_NAME,
                                                             HDF5Constants.H5_ITER_INC, MemorySegment.NULL,
                                                             op_segment, info) < 0) {
                    h5libraryError();
                }
            }

            int count = (int)countHandle.get(info, 0);
            log.trace("H5Gget_obj_info_full: count={}", count);

            // Read the results from the MemorySegments
            for (int i = 0; i < count; i++) {
                // Read object name
                MemorySegment objname_ptr = (MemorySegment)objnameHandle.get(info, DATA_OFFSET, (long)i);
                if (objname_ptr != null) {
                    MemorySegment cStringSegment = objname_ptr.reinterpret(256); // or a more precise length
                    objNames[i]                  = cStringSegment.getString(0);
                    log.trace("H5Gget_obj_info_full: objNames[{}]={}", i, objNames[i]);
                }
                else {
                    objNames[i] = null;
                }
                // Read object type
                int otype   = (int)otypeHandle.get(info, DATA_OFFSET, (long)i);
                objTypes[i] = otype;
                log.trace("H5Gget_obj_info_full: objTypes[{}]={}", i, objTypes[i]);
                // Read link type
                int ltype_val = (int)ltypeHandle.get(info, DATA_OFFSET, (long)i);
                ltype[i]      = ltype_val;
                log.trace("H5Gget_obj_info_full: ltype[{}]={}", i, ltype[i]);
                // Read file number
                long fno_val = (long)fnoHandle.get(info, DATA_OFFSET, (long)i);
                fno[i]       = fno_val;
                log.trace("H5Gget_obj_info_full: fno[{}]={}", i, fno[i]);
                // Read object token
                MemorySegment token_ptr = (MemorySegment)obj_tokenHandle.get(info, DATA_OFFSET, (long)i);
                tokens[i]               = new hdf.hdf5lib.structs.H5O_token_t(token_ptr);
                log.trace("H5Gget_obj_info_full: tokens[{}]={}", i, tokens[i]);
            }
            ret = count;
        }
        finally {
            if (group_name != null) {
                hdf.hdf5lib.H5.H5Gclose(gid);
            }
        }
        return ret;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gget_obj_info_idx report the name and type of object with index 'idx' in a Group. The 'idx'
     * corresponds to the index maintained by H5Giterate. Each link is returned, so objects with multiple
     * links will be counted once for each link.
     *
     * @param loc_id
     *            IN: file or group ID.
     * @param name
     *            IN: name of the group to iterate, relative to the loc_id
     * @param idx
     *            IN: the index of the object to iterate.
     * @param oname
     *            OUT: the name of the object
     * @param type
     *            OUT: the type of the object
     *
     * @return non-negative if successful, -1 if not.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     */
    public static int H5Gget_obj_info_idx(long loc_id, String name, int idx, String[] oname, int[] type)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        oname[0] = H5Lget_name_by_idx(loc_id, name, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC,
                                      idx, HDF5Constants.H5P_DEFAULT);
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom(name);
            MemorySegment linfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5L_info2_t.sizeof());
            if (H5Lget_info_by_idx2(loc_id, name_segment, HDF5Constants.H5_INDEX_NAME,
                                    HDF5Constants.H5_ITER_INC, idx, linfo_segment,
                                    HDF5Constants.H5P_DEFAULT) < 0) {
                h5libraryError();
            }
            type[0] = org.hdfgroup.javahdf5.H5L_info2_t.type(linfo_segment);
        }

        return 0;
    }

    /*
     * Add these methods so that we don't need to call
     * in a loop to get information for all the object in a group, which takes
     * a lot of time to finish if the number of objects is more than 10,000
     */
    /**
     * @ingroup JH5G
     *
     * retrieves information of all objects (recurvisely) under the group (name) located in the file or group
     * specified by loc_id up to maximum specified by objMax.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param objNames
     *            OUT: Names of all objects under the group, name.
     * @param objTypes
     *            OUT: Types of all objects under the group, name.
     * @param lnkTypes
     *            OUT: Types of all links under the group, name.
     * @param objToken
     *            OUT: Object token of all objects under the group, name.
     * @param objMax
     *            IN: Maximum number of all objects under the group, name.
     *
     * @return the number of items found
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     */
    public static int H5Gget_obj_info_max(long loc_id, String[] objNames, int[] objTypes, int[] lnkTypes,
                                          H5O_token_t[] objToken, long objMax)
        throws HDF5LibraryException, NullPointerException
    {
        if (objNames == null) {
            throw new NullPointerException("name array is null");
        }
        if (objTypes == null) {
            throw new NullPointerException("object type array is null");
        }
        if (lnkTypes == null) {
            throw new NullPointerException("link type array is null");
        }
        if (objToken == null) {
            throw new NullPointerException("object token array is null");
        }
        if (objMax <= 0) {
            throw new HDF5FunctionArgumentException("maximum array size is zero");
        }
        if (objNames.length <= 0) {
            throw new HDF5LibraryException("H5Gget_obj_info_max(): array size is zero");
        }
        if (objNames.length != objTypes.length) {
            throw new HDF5LibraryException("H5Gget_obj_info_max(): name and type array sizes are different");
        }
        return H5Gget_obj_info_max(loc_id, objNames, objTypes, lnkTypes, objToken, objMax, objNames.length);
    }

    /*
     * NOTE: This is a dangerous call! The caller can supply any value they'd like
     * for 'n' and if it exceeds the number of links reachable from the group, we
     * will most likely end up overwriting memory heap-tracking info.
     */
    private static int H5Gget_obj_info_max(long loc_id, String[] objNames, int[] objTypes, int[] ltype,
                                           H5O_token_t[] tokens, long amax, int n)
        throws HDF5LibraryException, NullPointerException
    {
        if (objNames == null) {
            throw new NullPointerException("objNames is null");
        }
        if (objTypes == null) {
            throw new NullPointerException("objTypes is null");
        }
        if (ltype == null) {
            throw new NullPointerException("ltype ais null");
        }
        if (tokens == null) {
            throw new NullPointerException("tokens is null");
        }
        if (n < 0) {
            throw new HDF5FunctionArgumentException("n is negative");
        }

        StructLayout info_ptr_t =
            MemoryLayout.structLayout(ValueLayout.ADDRESS.withName("objname"),   // char         **objname
                                      ValueLayout.ADDRESS.withName("obj_token"), // H5O_token_t   *obj_token
                                      ValueLayout.JAVA_INT.withName("otype"),    // int           *otype
                                      ValueLayout.JAVA_INT.withName("ltype")     // int           *ltype
            );

        StructLayout info_all_t = MemoryLayout.structLayout(
            MemoryLayout.sequenceLayout(n, MemoryLayout
                                               .structLayout(ValueLayout.ADDRESS.withName("objname"),
                                                             ValueLayout.ADDRESS.withName("obj_token"),
                                                             ValueLayout.JAVA_INT.withName("otype"),
                                                             ValueLayout.JAVA_INT.withName("ltype"))
                                               .withName("data")),
            ValueLayout.JAVA_LONG.withName("idxnum"), // unsigned long  idxnum
            ValueLayout.JAVA_INT.withName("count")    // int            count
        );

        long DATA_OFFSET          = 0L; // info_all_t.byteOffset(PathElement.groupElement("data"));
        VarHandle objnameHandle   = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("objname"));
        VarHandle otypeHandle     = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("otype"));
        VarHandle ltypeHandle     = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("ltype"));
        VarHandle obj_tokenHandle = info_ptr_t.arrayElementVarHandle(PathElement.groupElement("obj_token"));
        VarHandle idxnumHandle    = info_all_t.varHandle(PathElement.groupElement("idxnum"));
        VarHandle countHandle     = info_all_t.varHandle(PathElement.groupElement("count"));

        int ret = -1;
        try (Arena arena = Arena.ofConfined()) {
            class H5L_iter_callback implements H5L_iterate_t {
                public int apply(long loc_id, MemorySegment name, MemorySegment info, MemorySegment op_data)
                {
                    int ret                = -1;
                    long idxnum            = (long)idxnumHandle.get(op_data, 0);
                    int count              = (int)countHandle.get(op_data, 0);
                    MemorySegment name_seg = arena.allocateFrom(name.getString(0));
                    objnameHandle.set(op_data, DATA_OFFSET, (long)count, name_seg);
                    int ltype = (int)H5L_info2_t.type(info);
                    ltypeHandle.set(op_data, DATA_OFFSET, (long)count, ltype);

                    MemorySegment info_segment = arena.allocate(H5O_info2_t.sizeof());
                    if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_info3(loc_id, info_segment,
                                                                  HDF5Constants.H5O_INFO_ALL) < 0)
                        h5libraryError();
                    int otype = (int)H5O_info2_t.type(info_segment);
                    otypeHandle.set(op_data, DATA_OFFSET, (long)count, otype);
                    obj_tokenHandle.set(op_data, DATA_OFFSET, (long)count, H5O_info2_t.token(info_segment));

                    count++;
                    countHandle.set(op_data, 0, count); // count
                    if (count >= (int)idxnum)
                        ret = 1;
                    else
                        ret = 0;

                    return ret;
                }
            }
            H5L_iterate_t obj_info_all = new H5L_iter_callback();
            MemorySegment info         = arena.allocate(info_all_t);

            // Set up the info struct
            idxnumHandle.set(info, 0L, amax); // idxnum
            countHandle.set(info, 0L, 0);     // count

            MemorySegment op_segment = H5L_iterate2_t.allocate(obj_info_all, arena);
            // Call H5Literate2
            if ((ret = org.hdfgroup.javahdf5.hdf5_h.H5Lvisit2(
                     loc_id, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, op_segment, info)) < 0) {
                h5libraryError();
            }

            int count = (int)countHandle.get(info, 0);
            log.trace("H5Gget_obj_info_max: count={}", count);

            // Read the results from the MemorySegments
            for (int i = 0; i < count; i++) {
                // Read object name
                MemorySegment objname_ptr = (MemorySegment)objnameHandle.get(info, DATA_OFFSET, (long)i);
                if (objname_ptr != null) {
                    MemorySegment cStringSegment = objname_ptr.reinterpret(256); // or a more precise length
                    objNames[i]                  = cStringSegment.getString(0);
                    log.trace("H5Gget_obj_info_max: objNames[{}]={}", i, objNames[i]);
                }
                else {
                    objNames[i] = null;
                }
                // Read object type
                int otype   = (int)otypeHandle.get(info, DATA_OFFSET, (long)i);
                objTypes[i] = otype;
                log.trace("H5Gget_obj_info_max: objTypes[{}]={}", i, objTypes[i]);
                // Read link type
                int ltype_val = (int)ltypeHandle.get(info, DATA_OFFSET, (long)i);
                ltype[i]      = ltype_val;
                log.trace("H5Gget_obj_info_max: ltype[{}]={}", i, ltype[i]);
                // Read object token
                MemorySegment token_ptr = (MemorySegment)obj_tokenHandle.get(info, DATA_OFFSET, (long)i);
                tokens[i]               = new hdf.hdf5lib.structs.H5O_token_t(token_ptr);
                log.trace("H5Gget_obj_info_full: tokens[{}]={}", i, tokens[i]);
            }
            ret = count;
        }
        return ret;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gn_members report the number of objects in a Group. The 'objects' include everything that will be
     * visited by H5Giterate. Each link is returned, so objects with multiple links will be counted once for
     * each link.
     *
     * @param loc_id
     *            file or group ID.
     * @param name
     *            name of the group to iterate, relative to the loc_id
     *
     * @return the number of members in the group or -1 if error.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     */
    public static long H5Gn_members(long loc_id, String name)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        long n      = -1; // Default to -1 for error case
        long grp_id = H5Gopen(loc_id, name, H5P_DEFAULT());
        try {
            // Get the group information
            // Note: H5Gget_info returns the number of links in the group, not the number of objects.
            // To get the number of objects, we need to iterate through the group.
            hdf.hdf5lib.structs.H5G_info_t info = H5Gget_info(grp_id);

            n = info.nlinks;
        }
        finally {
            H5Gclose(grp_id);
        }

        return n;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gopen opens an existing group, name, at the location specified by loc_id.
     *
     * @param loc_id
     *            IN: File or group identifier specifying the location of the group to be opened.
     * @param name
     *            IN: Name of group to open.
     * @param gapl_id
     *            IN: Identifier of group access property list.
     *
     * @return a valid group identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Gopen(long loc_id, String name, long gapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name array is null");
        }

        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);

            id = H5Gopen2(loc_id, name_segment, gapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Gopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5G
     *
     * H5Gflush causes all buffers associated with a group to be immediately flushed to disk without
     * removing the data from the cache.
     *
     * @param group_id
     *            IN: Identifier of the group to be flushed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Gflush(long group_id) throws HDF5LibraryException
    {
        log.trace("H5Gflush: group_id={}", group_id);
        if (org.hdfgroup.javahdf5.hdf5_h.H5Gflush(group_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5G
     *
     * H5Grefresh causes all buffers associated with a group to be cleared and immediately re-loaded
     * with updated contents from disk. This function essentially closes the group, evicts all metadata
     * associated with it from the cache, and then re-opens the group. The reopened group is automatically
     * re-registered with the same ID.
     *
     * @param group_id
     *            IN: Identifier of the group to be refreshed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Grefresh(long group_id) throws HDF5LibraryException
    {
        log.trace("H5Grefresh: group_id={}", group_id);
        if (org.hdfgroup.javahdf5.hdf5_h.H5Grefresh(group_id) < 0)
            h5libraryError();
    }

    // ////////////////////////////////////////////////////////////
    // //
    // H5HF: Fractal Heap Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // No public Functions

    // ////////////////////////////////////////////////////////////
    // //
    // H5HG: Global Heap Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // No public Functions

    // ////////////////////////////////////////////////////////////
    // //
    // H5HL: Local Heap Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // No public Functions

    // ////////////////////////////////////////////////////////////
    // //
    // H5I: HDF5 Identifier Interface API Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5I Java Identifier (H5I) Interface
     *
     * @see H5I, C-API
     *
     * @see @ref H5I_UG, User Guide
     **/

    /**
     * @ingroup JH5I
     *
     * H5Iget_file_id obtains the file ID specified by the identifier, obj_id.
     *
     * @param obj_id
     *            IN: Identifier of the object.
     *
     * @return the file ID.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Iget_file_id(long obj_id) throws HDF5LibraryException
    {
        long file_id = org.hdfgroup.javahdf5.hdf5_h.H5Iget_file_id(obj_id);
        log.trace("H5Iget_file_id: obj_id={}, file_id={}", obj_id, file_id);
        return file_id;
    }

    /**
     * @ingroup JH5I
     *
     * H5Iget_name_long retrieves the name of an object specified by the identifier, obj_id.
     * @deprecated
     *
     * @param obj_id
     *            IN: Identifier of the object.
     * @param name
     *            OUT: Attribute name buffer.
     * @param size
     *            IN: Maximum length of the name to retrieve.
     *
     * @return the length of the name retrieved.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    @Deprecated
    public static long H5Iget_name_long(long obj_id, String[] name, long size)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        throw new HDF5LibraryException("H5Dvlen_reclaim not implemented as it is deprecated");
    }

    /**
     * @ingroup JH5I
     *
     * H5Iget_name retrieves the name of an object specified by the identifier, obj_id.
     *
     * @param obj_id
     *            IN: Identifier of the object.
     *
     * @return String for Attribute name.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5Iget_name(long obj_id) throws HDF5LibraryException
    {
        long name_size = -1;
        if ((name_size = org.hdfgroup.javahdf5.hdf5_h.H5Iget_name(obj_id, MemorySegment.NULL, 0)) < 0)
            h5libraryError();

        String ret_name = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocate(name_size + 1);
            /* Get the attribute name */
            if (org.hdfgroup.javahdf5.hdf5_h.H5Iget_name(obj_id, name_segment, name_size + 1) < 0)
                h5libraryError();

            ret_name = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return ret_name;
    }

    /**
     * @ingroup JH5I
     *
     * H5Iget_ref obtains the number of references outstanding specified by the identifier, obj_id.
     *
     * @param obj_id
     *            IN: Identifier of the object.
     *
     * @return the reference count.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Iget_ref(long obj_id) throws HDF5LibraryException
    {
        int ref_count = org.hdfgroup.javahdf5.hdf5_h.H5Iget_ref(obj_id);
        if (ref_count < 0) {
            h5libraryError();
        }

        return ref_count;
    }

    /**
     * @ingroup JH5I
     *
     * H5Idec_ref decrements the reference count specified by the identifier, obj_id.
     * If the reference count for an ID reaches zero, the object will be closed.
     *
     * @param obj_id
     *            IN: Identifier of the object.
     *
     * @return the reference count.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Idec_ref(long obj_id) throws HDF5LibraryException
    {
        int ref_count = org.hdfgroup.javahdf5.hdf5_h.H5Idec_ref(obj_id);
        if (ref_count < 0) {
            h5libraryError();
        }

        return ref_count;
    }

    /**
     * @ingroup JH5I
     *
     * H5Iinc_ref increments the reference count specified by the identifier, obj_id.
     *
     * @param obj_id
     *            IN: Identifier of the object.
     *
     * @return the reference count.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Iinc_ref(long obj_id) throws HDF5LibraryException
    {
        int ref_count = org.hdfgroup.javahdf5.hdf5_h.H5Iinc_ref(obj_id);
        if (ref_count < 0) {
            h5libraryError();
        }

        return ref_count;
    }

    /**
     * @ingroup JH5I
     *
     * H5Iget_type retrieves the type of the object identified by obj_id.
     *
     * @param obj_id
     *            IN: Object identifier whose type is to be determined.
     *
     * @return the object type if successful; otherwise H5I_BADID.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Iget_type(long obj_id) throws HDF5LibraryException
    {
        int type = org.hdfgroup.javahdf5.hdf5_h.H5Iget_type(obj_id);
        if (type < 0) {
            h5libraryError();
        }

        return type;
    }

    /**
     * @ingroup JH5I
     *
     * H5Iget_type_ref retrieves the reference count on an ID type. The reference count is used by the library
     * to indicate when an ID type can be destroyed.
     *
     * @param type_id
     *            IN: The identifier of the type whose reference count is to be retrieved
     *
     * @return The current reference count on success, negative on failure.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Iget_type_ref(int type_id) throws HDF5LibraryException
    {
        int ref_count = org.hdfgroup.javahdf5.hdf5_h.H5Iget_type_ref(type_id);
        if (ref_count < 0) {
            h5libraryError();
        }

        return ref_count;
    }

    /**
     * @ingroup JH5I
     *
     * H5Idec_type_ref decrements the reference count on an identifier type. The reference count is used by
     * the library to indicate when an identifier type can be destroyed. If the reference count reaches zero,
     * this function will destroy it.
     *
     * @param type_id
     *            IN: The identifier of the type whose reference count is to be decremented
     *
     * @return The current reference count on success, negative on failure.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Idec_type_ref(int type_id) throws HDF5LibraryException
    {
        int ref_count = org.hdfgroup.javahdf5.hdf5_h.H5Idec_type_ref(type_id);
        if (ref_count < 0) {
            h5libraryError();
        }

        return ref_count;
    }

    /**
     * @ingroup JH5I
     *
     * H5Iinc_type_ref increments the reference count on an ID type. The reference count is used by the
     * library to indicate when an ID type can be destroyed.
     *
     * @param type_id
     *            IN: The identifier of the type whose reference count is to be incremented
     *
     * @return The current reference count on success, negative on failure.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Iinc_type_ref(int type_id) throws HDF5LibraryException
    {
        int ref_count = org.hdfgroup.javahdf5.hdf5_h.H5Iinc_type_ref(type_id);
        if (ref_count < 0) {
            h5libraryError();
        }

        return ref_count;
    }

    /**
     * @ingroup JH5I
     *
     * H5Inmembers returns the number of identifiers of the identifier type specified in type.
     *
     * @param type_id
     *            IN: Identifier for the identifier type whose member count will be retrieved
     *
     * @return Number of identifiers of the specified identifier type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Inmembers(int type_id) throws HDF5LibraryException
    {
        int n_members = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment int_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Inmembers(type_id, int_segment) < 0)
                h5libraryError();
            n_members = int_segment.get(ValueLayout.JAVA_INT, 0);
        }

        return n_members;
    }

    /**
     * @ingroup JH5I
     *
     * H5Iis_valid indicates if the identifier type specified in obj_id is valid.
     *
     * @param obj_id
     *            IN: Identifier to be checked
     *
     * @return a boolean, true if the specified identifier id is valid
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Iis_valid(long obj_id) throws HDF5LibraryException
    {
        boolean is_valid = false;
        int retVal       = -1;
        if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Iis_valid(obj_id)) < 0) {
            h5libraryError();
        }
        if (retVal == 0) {
            is_valid = false; // ID is not valid
        }
        else {
            is_valid = true; // ID is valid
        }

        return is_valid;
    }

    /**
     * @ingroup JH5I
     *
     * H5Itype_exists indicates if the identifier type specified in type exists.
     *
     * @param type_id
     *            IN: the identifier type to be checked
     *
     * @return a boolean, true if the specified identifier type exists
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Itype_exists(int type_id) throws HDF5LibraryException
    {
        boolean exists = false;
        int retVal     = -1;
        if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Itype_exists(type_id)) < 0) {
            h5libraryError();
        }
        if (retVal == 0) {
            exists = false; // Type does not exist
        }
        else {
            exists = true; // Type exists
        }

        return exists;
    }

    /**
     * @ingroup JH5I
     *
     * H5Iclear_type deletes all identifiers of the type identified by the argument type.
     *
     * @param type_id
     *            IN: Identifier of identifier type which is to be cleared of identifiers
     * @param force
     *            IN: Whether or not to force deletion of all identifiers
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Iclear_type(int type_id, boolean force) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Iclear_type(type_id, force) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5I
     *
     * H5Idestroy_type deletes an entire identifier type. All identifiers of this type are destroyed
     * and no new identifiers of this type can be registered.
     *
     * @param type_id
     *            IN: Identifier of identifier type which is to be destroyed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Idestroy_type(int type_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Idestroy_type(type_id) < 0) {
            h5libraryError();
        }
    }

    // /////// unimplemented ////////

    // void *H5Iobject_verify(hid_t id, H5I_type_t id_type);

    // hid_t H5Iregister(H5I_type_t type, const void *object);

    // typedef herr_t (*H5I_free_t)(void *);
    // H5I_type_t H5Iregister_type2(unsigned reserved, H5I_free_t free_func);

    // void *H5Iremove_verify(hid_t id, H5I_type_t id_type);

    // Type of the function to compare objects & keys
    // typedef int (*H5I_search_func_t)(void *obj, hid_t id, void *key);
    // void *H5Isearch(H5I_type_t type, H5I_search_func_t func, void *key);

    // Type of the H5Iiterate callback function
    // typedef herr_t (*H5I_iterate_func_t)(hid_t id, void *udata);
    // herr_t H5Iiterate(H5I_type_t type, H5I_iterate_func_t op, void *op_data);

    // //////////////////////////////////////////////////////////////////
    // H5L: Link Interface Functions //
    // //////////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5L Java Link (H5L) Interface
     *
     * @see H5L, C-API
     *
     * @see @ref H5L_UG, User Guide
     **/

    /**
     * @ingroup JH5L
     *
     * H5Lcopy copies a link from one location to another.
     *
     * @param src_loc
     *            IN: Location identifier of the source link
     * @param src_name
     *            IN: Name of the link to be copied
     * @param dst_loc
     *            IN: Location identifier specifying the destination of the copy
     * @param dst_name
     *            IN: Name to be assigned to the new copy
     * @param lcpl_id
     *            IN: Link creation property list identifier
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static void H5Lcopy(long src_loc, String src_name, long dst_loc, String dst_name, long lcpl_id,
                               long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (src_name == null || dst_name == null) {
            throw new NullPointerException("src_name or dst_name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment src_name_segment = arena.allocateFrom(src_name);
            MemorySegment dst_name_segment = arena.allocateFrom(dst_name);

            if (org.hdfgroup.javahdf5.hdf5_h.H5Lcopy(src_loc, src_name_segment, dst_loc, dst_name_segment,
                                                     lcpl_id, lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5L
     *
     * H5Lcreate_external creates a new soft link to an external object, which is an object in a different
     * HDF5 file from the location of the link.
     *
     * @param file_name
     *            IN: Name of the target file containing the target object.
     * @param obj_name
     *            IN: Path within the target file to the target object.
     * @param link_loc_id
     *            IN: The file or group identifier for the new link.
     * @param link_name
     *            IN: The name of the new link.
     * @param lcpl_id
     *            IN: Link creation property list identifier
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static void H5Lcreate_external(String file_name, String obj_name, long link_loc_id,
                                          String link_name, long lcpl_id, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (file_name == null || obj_name == null || link_name == null) {
            throw new NullPointerException("file_name, obj_name or link_name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment file_name_segment = arena.allocateFrom(file_name);
            MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
            MemorySegment link_name_segment = arena.allocateFrom(link_name);
            // Call the native method to create the external link
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lcreate_external(file_name_segment, obj_name_segment,
                                                                link_loc_id, link_name_segment, lcpl_id,
                                                                lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5L
     *
     * H5Lcreate_hard creates a new hard link to a pre-existing object in an HDF5 file.
     *
     * @param cur_loc
     *            IN: The file or group identifier for the target object.
     * @param cur_name
     *            IN: Name of the target object, which must already exist.
     * @param dst_loc
     *            IN: The file or group identifier for the new link.
     * @param dst_name
     *            IN: The name of the new link.
     * @param lcpl_id
     *            IN: Link creation property list identifier
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            cur_name or dst_name is null.
     **/
    public static void H5Lcreate_hard(long cur_loc, String cur_name, long dst_loc, String dst_name,
                                      long lcpl_id, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (cur_name == null || dst_name == null) {
            throw new NullPointerException("cur_name or dst_name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment cur_name_segment = arena.allocateFrom(cur_name);
            MemorySegment dst_name_segment = arena.allocateFrom(dst_name);
            // Call the native method to create the hard link
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lcreate_hard(cur_loc, cur_name_segment, dst_loc,
                                                            dst_name_segment, lcpl_id, lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5L
     *
     * H5Lcreate_soft creates a new soft link to an object in an HDF5 file.
     *
     * @param link_target
     *            IN: Path to the target object, which is not required to exist.
     * @param link_loc_id
     *            IN: The file or group identifier for the new link.
     * @param link_name
     *            IN: The name of the new link.
     * @param lcpl_id
     *            IN: Link creation property list identifier
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            link_name is null.
     **/
    public static void H5Lcreate_soft(String link_target, long link_loc_id, String link_name, long lcpl_id,
                                      long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (link_name == null) {
            throw new NullPointerException("link_name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment link_target_segment = arena.allocateFrom(link_target);
            MemorySegment link_name_segment   = arena.allocateFrom(link_name);
            // Call the native method to create the soft link
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lcreate_soft(link_target_segment, link_loc_id,
                                                            link_name_segment, lcpl_id, lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5L
     *
     * H5Ldelete removes the link specified from a group.
     *
     * @param loc_id
     *            IN: Identifier of the file or group containing the object.
     * @param name
     *            IN: Name of the link to delete.
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static void H5Ldelete(long loc_id, String name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            // Call the native method to delete the link
            if (org.hdfgroup.javahdf5.hdf5_h.H5Ldelete(loc_id, name_segment, lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5L
     *
     * H5Ldelete_by_idx removes the nth link in a group according to the specified order and in the specified
     * index.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of subject group
     * @param group_name
     *            IN: Name of subject group
     * @param idx_type
     *            IN: Index or field which determines the order
     * @param order
     *            IN: Order within field or index
     * @param n
     *            IN: Link for which to retrieve information
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            group_name is null.
     **/
    public static void H5Ldelete_by_idx(long loc_id, String group_name, int idx_type, int order, long n,
                                        long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("group_name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(group_name);
            // Call the native method to delete the link by index
            if (org.hdfgroup.javahdf5.hdf5_h.H5Ldelete_by_idx(loc_id, name_segment, idx_type, order, n,
                                                              lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5L
     *
     * H5Lexists checks if a link with a particular name exists in a group.
     *
     * @param loc_id
     *            IN: Identifier of the file or group to query.
     * @param name
     *            IN: The name of the link to check.
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return a boolean, true if the name exists, otherwise false.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static boolean H5Lexists(long loc_id, String name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        boolean exists = false;
        int retVal     = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            // Call the native method to check if the link exists
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Lexists(loc_id, name_segment, lapl_id)) < 0) {
                h5libraryError();
            }
            if (retVal == 0)
                exists = false; // Name does not exist
            else
                exists = true; // Name exists
        }
        return exists;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lget_info returns information about the specified link.
     *
     * @param loc_id
     *            IN: Identifier of the file or group.
     * @param name
     *            IN: Name of the link for which information is being sought.
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return a buffer(H5L_info_t) for the link information.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5L_info_t H5Lget_info(long loc_id, String name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        hdf.hdf5lib.structs.H5L_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom(name);
            MemorySegment linfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5L_info2_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lget_info2(loc_id, name_segment, linfo_segment, lapl_id) < 0) {
                h5libraryError();
            }
            info = new hdf.hdf5lib.structs.H5L_info_t(linfo_segment);
            log.trace("H5Lget_info2: type={}", info.type);
            if (info.type == H5L_TYPE_ERROR()) {
                throw new HDF5LibraryException("Invalid link type");
            }
        }
        return info;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lget_info_by_idx opens a named datatype at the location specified by loc_id and return an identifier
     * for the datatype.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of subject group
     * @param group_name
     *            IN: Name of subject group
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order within field or index
     * @param n
     *            IN: Link for which to retrieve information
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return a buffer(H5L_info_t) for the link information.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            group_name is null.
     **/
    public static hdf.hdf5lib.structs.H5L_info_t
    H5Lget_info_by_idx(long loc_id, String group_name, int idx_type, int order, long n, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("group_name is null");
        }

        hdf.hdf5lib.structs.H5L_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom(group_name);
            MemorySegment linfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5L_info2_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lget_info_by_idx2(loc_id, name_segment, idx_type, order, n,
                                                                 linfo_segment, lapl_id) < 0) {
                h5libraryError();
            }
            info = new hdf.hdf5lib.structs.H5L_info_t(linfo_segment);
            log.trace("H5Lget_info_by_idx2: type={}", info.type);
            if (info.type == H5L_TYPE_ERROR()) {
                throw new HDF5LibraryException("Invalid link type");
            }
        }
        return info;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lget_name_by_idx retrieves name of the nth link in a group, according to the order within a specified
     * field or index.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of subject group
     * @param group_name
     *            IN: Name of subject group
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order within field or index
     * @param n
     *            IN: Link for which to retrieve information
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return a String for the link name.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            group_name is null.
     **/
    public static String H5Lget_name_by_idx(long loc_id, String group_name, int idx_type, int order, long n,
                                            long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("group_name is null");
        }

        String ret_name = null;
        try (Arena arena = Arena.ofConfined()) {
            long buf_size = -1;

            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(group_name);
            /* Get the length of the link name */
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Lget_name_by_idx(
                     loc_id, name_segment, idx_type, order, n, MemorySegment.NULL, 0, lapl_id)) < 0)
                h5libraryError();
            MemorySegment link_segment = arena.allocate(buf_size + 1); // Allocate space for the link name
            /* Get the link name */
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lget_name_by_idx(loc_id, name_segment, idx_type, order, n,
                                                                link_segment, buf_size + 1, lapl_id) < 0) {
                h5libraryError();
            }
            ret_name = link_segment.getString(0, StandardCharsets.UTF_8);
        }
        return ret_name;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lget_value returns the link value of a symbolic link. Note that this function is a combination
     * of H5Lget_info(), H5Lget_val() and for external links, H5Lunpack_elink_val.
     *
     * @param loc_id
     *            IN: Identifier of the file or group containing the object.
     * @param name
     *            IN: Name of the symbolic link.
     * @param link_value
     *            OUT: Path of the symbolic link, or the file_name and path of an external file.
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return the link type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Lget_value(long loc_id, String name, String[] link_value, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (link_value == null || link_value.length < 2) {
            throw new HDF5FunctionArgumentException("link_value is null or not of length 2");
        }

        int link_type = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom(name);
            MemorySegment linfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5L_info2_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lget_info2(loc_id, name_segment, linfo_segment, lapl_id) < 0) {
                h5libraryError();
            }
            // Unpack the H5L_info2_t from the MemorySegment
            MemorySegment u_segment = org.hdfgroup.javahdf5.H5L_info2_t.u(linfo_segment);
            long val_size           = org.hdfgroup.javahdf5.H5L_info2_t.u.val_size(u_segment);
            link_type               = org.hdfgroup.javahdf5.H5L_info2_t.type(linfo_segment);
            if (link_type == H5L_TYPE_ERROR()) {
                throw new HDF5LibraryException("Invalid link type");
            }
            if (link_type == H5L_TYPE_HARD()) {
                throw new HDF5FunctionArgumentException("hard links are unsupported");
            }
            MemorySegment link_value_segment =
                arena.allocate(val_size + 1); // Allocate space for the link path

            // Call the native method to get the link value
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lget_val(loc_id, name_segment, link_value_segment,
                                                        val_size + 1, lapl_id) < 0) {
                h5libraryError();
            }
            if (link_type == H5L_TYPE_EXTERNAL()) {
                MemorySegment file_name_segment = arena.allocate(val_size + 1);
                MemorySegment obj_name_segment  = arena.allocate(val_size + 1);
                if (org.hdfgroup.javahdf5.hdf5_h.H5Lunpack_elink_val(link_value_segment, val_size + 1,
                                                                     MemorySegment.NULL, file_name_segment,
                                                                     obj_name_segment) < 0)
                    h5libraryError();
                // Convert the MemorySegment to a String
                MemorySegment obj_name_segment_Address = obj_name_segment.getAtIndex(ValueLayout.ADDRESS, 0);
                MemorySegment retrieved_obj_name_segment =
                    obj_name_segment_Address.reinterpret(val_size, arena, null);
                MemorySegment file_name_segment_Address =
                    file_name_segment.getAtIndex(ValueLayout.ADDRESS, 0);
                MemorySegment retrieved_file_name_segment =
                    file_name_segment_Address.reinterpret(val_size, arena, null);

                link_value[0] = retrieved_obj_name_segment.getString(0, StandardCharsets.UTF_8);
                link_value[1] = retrieved_file_name_segment.getString(0, StandardCharsets.UTF_8);
            }
            else if (link_type == H5L_TYPE_SOFT()) {
                // Convert the MemorySegment to a String
                link_value[0] = link_value_segment.getString(0, StandardCharsets.UTF_8);
                link_value[1] = null;
            }
            else
                throw new HDF5LibraryException("H5Lget_val: invalid link type");
        }
        return link_type;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lget_value_by_idx retrieves value of the nth link in a group, according to the order within an index.
     * Note that this function is a combination of H5Lget_info(), H5Lget_val() and for external links,
     * H5Lunpack_elink_val.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of subject group
     * @param group_name
     *            IN: Name of subject group
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order within field or index
     * @param n
     *            IN: Link for which to retrieve information
     * @param link_value
     *            OUT: Path of the symbolic link, or the file_name and path of an external file.
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return the link type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            group_name is null.
     **/
    public static int H5Lget_value_by_idx(long loc_id, String group_name, int idx_type, int order, long n,
                                          String[] link_value, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("group_name is null");
        }

        int link_type = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom(group_name);
            MemorySegment linfo_segment = arena.allocate(org.hdfgroup.javahdf5.H5L_info2_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lget_info_by_idx2(loc_id, name_segment, idx_type, order, n,
                                                                 linfo_segment, lapl_id) < 0) {
                h5libraryError();
            }
            // Unpack the H5L_info2_t from the MemorySegment
            MemorySegment u_segment = org.hdfgroup.javahdf5.H5L_info2_t.u(linfo_segment);
            long val_size           = org.hdfgroup.javahdf5.H5L_info2_t.u.val_size(u_segment);
            link_type               = org.hdfgroup.javahdf5.H5L_info2_t.type(linfo_segment);
            if (link_type == H5L_TYPE_ERROR()) {
                throw new HDF5LibraryException("Invalid link type");
            }
            if (link_type == H5L_TYPE_HARD()) {
                throw new HDF5FunctionArgumentException("hard links are unsupported");
            }
            MemorySegment link_value_segment =
                arena.allocate(val_size + 1); // Allocate space for the link path

            // Call the native method to get the link value
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lget_val_by_idx(loc_id, name_segment, idx_type, order, n,
                                                               link_value_segment, val_size + 1,
                                                               lapl_id) < 0) {
                h5libraryError();
            }
            if (link_type == H5L_TYPE_EXTERNAL()) {
                MemorySegment file_name_segment = arena.allocate(val_size + 1);
                MemorySegment obj_name_segment  = arena.allocate(val_size + 1);
                if (org.hdfgroup.javahdf5.hdf5_h.H5Lunpack_elink_val(link_value_segment, val_size + 1,
                                                                     MemorySegment.NULL, file_name_segment,
                                                                     obj_name_segment) < 0)
                    h5libraryError();
                // Convert the MemorySegment to a String
                MemorySegment obj_name_segment_Address = obj_name_segment.getAtIndex(ValueLayout.ADDRESS, 0);
                MemorySegment retrieved_obj_name_segment =
                    obj_name_segment_Address.reinterpret(val_size, arena, null);
                MemorySegment file_name_segment_Address =
                    file_name_segment.getAtIndex(ValueLayout.ADDRESS, 0);
                MemorySegment retrieved_file_name_segment =
                    file_name_segment_Address.reinterpret(val_size, arena, null);

                link_value[0] = retrieved_obj_name_segment.getString(0, StandardCharsets.UTF_8);
                link_value[1] = retrieved_file_name_segment.getString(0, StandardCharsets.UTF_8);
            }
            else {
                // Convert the MemorySegment to a String
                link_value[0] = link_value_segment.getString(0, StandardCharsets.UTF_8);
                link_value[1] = null;
            }
        }
        return link_type;
    }

    /**
     * @ingroup JH5L
     *
     * H5Literate iterates through links in a group.
     *
     * @param grp_id
     *            IN: Identifier specifying subject group
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order of iteration within index
     * @param idx
     *            IN: Iteration position at which to start
     * @param op
     *            IN: Callback function passing data regarding the link to the calling application
     * @param op_data
     *            IN: User-defined pointer to data required by the application for its processing of the link
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Literate(long grp_id, int idx_type, int order, long idx,
                                 hdf.hdf5lib.callbacks.H5L_iterate_t op,
                                 hdf.hdf5lib.callbacks.H5L_iterate_opdata_t op_data)
        throws HDF5LibraryException
    {
        if (op == null) {
            throw new NullPointerException("op is null");
        }
        if (op_data == null) {
            throw new NullPointerException("op_data is null");
        }
        int status     = -1;
        long start_idx = idx;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment start_idx_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            start_idx_segment.set(ValueLayout.JAVA_LONG, 0, start_idx);
            MemorySegment op_segment = H5L_iterate2_t.allocate(op, arena);
            MemorySegment op_data_segment =
                Linker.nativeLinker().upcallStub(H5Literate2$handle(), H5Literate2$descriptor(), arena);

            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Literate2(grp_id, idx_type, order, start_idx_segment,
                                                                   op_segment, op_data_segment)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5L
     *
     * H5Literate_by_name iterates through links in a group.
     *
     * @param loc_id
     *            IN: Identifier specifying subject group
     * @param group_name
     *            IN: Name of subject group
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order of iteration within index
     * @param idx
     *            IN: Iteration position at which to start
     * @param op
     *            IN: Callback function passing data regarding the link to the calling application
     * @param op_data
     *            IN: User-defined pointer to data required by the application for its processing of the link
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            group_name is null.
     **/
    public static int H5Literate_by_name(long loc_id, String group_name, int idx_type, int order, long idx,
                                         hdf.hdf5lib.callbacks.H5L_iterate_t op,
                                         hdf.hdf5lib.callbacks.H5L_iterate_opdata_t op_data, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("group_name is null");
        }
        if (op == null) {
            throw new NullPointerException("op is null");
        }
        if (op_data == null) {
            throw new NullPointerException("op_data is null");
        }

        int status     = -1;
        long start_idx = idx;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment      = arena.allocateFrom(group_name);
            MemorySegment start_idx_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            start_idx_segment.set(ValueLayout.JAVA_LONG, 0, start_idx);
            MemorySegment op_segment      = H5L_iterate2_t.allocate(op, arena);
            MemorySegment op_data_segment = Linker.nativeLinker().upcallStub(
                H5Literate_by_name2$handle(), H5Literate_by_name2$descriptor(), arena);
            // Call the native method to visit the links
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Literate_by_name2(
                     loc_id, name_segment, idx_type, order, start_idx_segment, op_segment, op_data_segment,
                     lapl_id)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lmove renames a link within an HDF5 file.
     *
     * @param src_loc
     *            IN: Original file or group identifier.
     * @param src_name
     *            IN: Original link name.
     * @param dst_loc
     *            IN: Destination file or group identifier.
     * @param dst_name
     *            IN: New link name.
     * @param lcpl_id
     *            IN: Link creation property list identifier to be associated with the new link.
     * @param lapl_id
     *            IN: Link access property list identifier to be associated with the new link.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static void H5Lmove(long src_loc, String src_name, long dst_loc, String dst_name, long lcpl_id,
                               long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (src_name == null || dst_name == null) {
            throw new NullPointerException("src_name or dst_name is null");
        }
        if (src_loc < 0) {
            throw new HDF5FunctionArgumentException("Negative src_loc");
        }
        if (dst_loc < 0) {
            throw new HDF5FunctionArgumentException("Negative dst_loc");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment src_name_segment = arena.allocateFrom(src_name);
            MemorySegment dst_name_segment = arena.allocateFrom(dst_name);
            // Call the native method to move the link
            if (org.hdfgroup.javahdf5.hdf5_h.H5Lmove(src_loc, src_name_segment, dst_loc, dst_name_segment,
                                                     lcpl_id, lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5L
     *
     * H5Lvisit recursively visits all links starting from a specified group.
     *
     * @param grp_id
     *            IN: Identifier specifying subject group
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order of iteration within index
     * @param op
     *            IN: Callback function passing data regarding the link to the calling application
     * @param op_data
     *            IN: User-defined pointer to data required by the application for its processing of the link
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Lvisit(long grp_id, int idx_type, int order, hdf.hdf5lib.callbacks.H5L_iterate_t op,
                               hdf.hdf5lib.callbacks.H5L_iterate_opdata_t op_data) throws HDF5LibraryException
    {
        if (op == null) {
            throw new NullPointerException("op is null");
        }
        if (op_data == null) {
            throw new NullPointerException("op_data is null");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Call the native method to visit the links
            MemorySegment op_segment = H5L_iterate2_t.allocate(op, arena);
            MemorySegment op_data_segment =
                Linker.nativeLinker().upcallStub(H5Lvisit2$handle(), H5Lvisit2$descriptor(), arena);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Lvisit2(grp_id, idx_type, order, op_segment,
                                                                 op_data_segment)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lvisit_by_name recursively visits all links starting from a specified group.
     *
     * @param loc_id
     *            IN: Identifier specifying subject group
     * @param group_name
     *            IN: Name of subject group
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order of iteration within index
     * @param op
     *            IN: Callback function passing data regarding the link to the calling application
     * @param op_data
     *            IN: User-defined pointer to data required by the application for its processing of the link
     * @param lapl_id
     *            IN: link access property
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            group_name is null.
     **/
    public static int H5Lvisit_by_name(long loc_id, String group_name, int idx_type, int order,
                                       hdf.hdf5lib.callbacks.H5L_iterate_t op,
                                       hdf.hdf5lib.callbacks.H5L_iterate_opdata_t op_data, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("group_name is null");
        }
        if (op == null) {
            throw new NullPointerException("op is null");
        }
        if (op_data == null) {
            throw new NullPointerException("op_data is null");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(group_name);
            MemorySegment op_segment   = H5L_iterate2_t.allocate(op, arena);
            MemorySegment op_data_segment =
                Linker.nativeLinker().upcallStub(H5Lvisit2$handle(), H5Lvisit2$descriptor(), arena);
            // Call the native method to visit the links
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Lvisit_by_name2(
                     loc_id, name_segment, idx_type, order, op_segment, op_data_segment, lapl_id)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lis_registered tests whether a user-defined link class is currently registered,
     * either by the HDF5 Library or by the user through the use of H5Lregister.
     *
     * @param link_cls_id
     *            IN: User-defined link class identifier
     *
     * @return Returns a positive value if the link class has been registered and zero if it is unregistered.
     *            Otherwise returns a negative value; this may mean that the identifier is not a valid
     *            user-defined class identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Lis_registered(int link_cls_id) throws HDF5LibraryException
    {
        int status = -1;
        if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Lis_registered(link_cls_id)) < 0) {
            h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5L
     *
     * H5Lunregister unregisters a class of user-defined links, preventing them from being traversed, queried,
     * moved, etc.
     *
     * @param link_cls_id
     *            IN: User-defined link class identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Lunregister(int link_cls_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Lunregister(link_cls_id) < 0) {
            h5libraryError();
        }
    }

    // /////// unimplemented ////////
    // herr_t H5Lcreate_ud(hid_t link_loc_id, const char *link_name,
    //         H5L_type_t link_type, const void *udata, size_t udata_size, hid_t lcpl_id,
    //         hid_t lapl_id);

    // herr_t H5Lregister(const H5L_class_t *cls);

    // herr_t H5Lunpack_elink_val(const void *ext_linkval/*in*/, size_t link_size,
    //         unsigned *flags, const char **filename/*out*/, const char **obj_path /*out*/);
    // herr_t H5Lget_val(hid_t loc_id, const char *name, void *buf/*out*/,
    //        size_t size, hid_t lapl_id);
    // herr_t H5Lget_val_by_idx(hid_t loc_id, const char *group_name,
    //        H5_index_t idx_type, H5_iter_order_t order, hsize_t n,
    //        void *buf/*out*/, size_t size, hid_t lapl_id);

    // ////////////////////////////////////////////////////////////
    // //
    // H5MM: Memory Management Interface API Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // /////// unimplemented ////////
    // typedef void *(*H5MM_allocate_t)(size_t size, void *alloc_info);
    // typedef void (*H5MM_free_t)(void *mem, void *free_info);

    // ////////////////////////////////////////////////////////////
    // //
    // H5O: HDF5 1.8 Object Interface API Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5O Java Object (H5O) Interface
     *
     * @see H5O, C-API
     *
     * @see @ref H5O_UG, User Guide
     **/

    /**
     * @ingroup JH5O
     *
     * H5Oclose closes the group, dataset, or named datatype specified.
     *
     * @param object_id
     *            IN: Object identifier
     *
     * @return non-negative on success
     **/
    public static int H5Oclose(long object_id)
    {
        log.trace("OPEN_IDS: H5Oclose remove {}", object_id);
        OPEN_IDS.remove(object_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Oclose(object_id);
        if (retVal < 0)
            h5libraryError();
        return retVal;
    }

    /**
     * @ingroup JH5O
     *
     * H5Ocopy copies the group, dataset or named datatype specified from the file or group specified by
     * source location to the destination location.
     *
     * @param src_loc_id
     *            IN: Object identifier indicating the location of the source object to be copied
     * @param src_name
     *            IN: Name of the source object to be copied
     * @param dst_loc_id
     *            IN: Location identifier specifying the destination
     * @param dst_name
     *            IN: Name to be assigned to the new copy
     * @param ocpypl_id
     *            IN: Object copy property list
     * @param lcpl_id
     *            IN: Link creation property list for the new hard link
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static void H5Ocopy(long src_loc_id, String src_name, long dst_loc_id, String dst_name,
                               long ocpypl_id, long lcpl_id) throws HDF5LibraryException, NullPointerException
    {
        if (src_name == null || dst_name == null) {
            throw new NullPointerException("src_name or dst_name is null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment src_name_segment = arena.allocateFrom(src_name);
            MemorySegment dst_name_segment = arena.allocateFrom(dst_name);
            // Call the native method to copy the object
            if (org.hdfgroup.javahdf5.hdf5_h.H5Ocopy(src_loc_id, src_name_segment, dst_loc_id,
                                                     dst_name_segment, ocpypl_id, lcpl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_comment retrieves the comment for the specified object.
     *
     * @param obj_id
     *            IN: File or group identifier
     *
     * @return the comment
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5Oget_comment(long obj_id)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        long buf_size = -1;

        if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Oget_comment(obj_id, MemorySegment.NULL, 0)) < 0)
            h5libraryError();

        String comment = null;
        if (buf_size > 0) {
            try (Arena arena = Arena.ofConfined()) {
                MemorySegment comment_segment = arena.allocate(buf_size + 1);
                if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_comment(obj_id, comment_segment, buf_size + 1) < 0)
                    h5libraryError();
                if (comment_segment != MemorySegment.NULL)
                    comment = comment_segment.getString(0, StandardCharsets.UTF_8);
            }
        }
        return comment;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oset_comment sets the comment for the specified object.
     *
     * @param obj_id
     *            IN: Identifier of the target object
     * @param comment
     *            IN: The new comment.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            comment is null.
     *
     * @deprecated As of HDF5 1.8 in favor of object attributes.
     **/
    @Deprecated
    public static void H5Oset_comment(long obj_id, String comment)
        throws HDF5LibraryException, NullPointerException
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment comment_segment = MemorySegment.NULL;
            if (comment != null)
                comment_segment = arena.allocateFrom(comment);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oset_comment(obj_id, comment_segment) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_comment_by_name retrieves the comment for an object.
     *
     * @param loc_id
     *            IN: Identifier of a file, group, dataset, or named datatype.
     * @param name
     *            IN: Relative name of the object whose comment is to be set or reset.
     * @param lapl_id
     *            IN: Link access property list identifier.
     *
     * @return the comment
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static String H5Oget_comment_by_name(long loc_id, String name, long lapl_id)
        throws HDF5LibraryException, HDF5FunctionArgumentException, NullPointerException
    {
        long buf_size = -1;

        if (name == null) {
            throw new NullPointerException("name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Oget_comment_by_name(
                     loc_id, name_segment, MemorySegment.NULL, 0, lapl_id)) < 0)
                h5libraryError();
        }

        String comment = null;
        if (buf_size > 0) {
            try (Arena arena = Arena.ofConfined()) {
                MemorySegment name_segment    = arena.allocateFrom(name);
                MemorySegment comment_segment = arena.allocate(buf_size + 1);
                if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_comment_by_name(loc_id, name_segment, comment_segment,
                                                                        buf_size + 1, lapl_id) < 0)
                    h5libraryError();
                if (comment_segment != MemorySegment.NULL)
                    comment = comment_segment.getString(0, StandardCharsets.UTF_8);
            }
        }
        return comment;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oset_comment_by_name sets the comment for the specified object.
     *
     * @param loc_id
     *            IN: Identifier of a file, group, dataset, or named datatype.
     * @param name
     *            IN: Relative name of the object whose comment is to be set or reset.
     * @param comment
     *            IN: The new comment.
     * @param lapl_id
     *            IN: Link access property list identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     *
     * @deprecated As of HDF5 1.8 in favor of object attributes.
     **/
    @Deprecated
    public static void H5Oset_comment_by_name(long loc_id, String name, String comment, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment    = arena.allocateFrom(name);
            MemorySegment comment_segment = MemorySegment.NULL;
            if (comment != null)
                comment_segment = arena.allocateFrom(comment);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oset_comment_by_name(loc_id, name_segment, comment_segment,
                                                                    lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_info retrieves the metadata for an object specified by an identifier.
     *
     * @param loc_id
     *            IN: Identifier for target object
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static hdf.hdf5lib.structs.H5O_info_t H5Oget_info(long loc_id) throws HDF5LibraryException
    {
        return H5Oget_info(loc_id, HDF5Constants.H5O_INFO_ALL);
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_info retrieves the metadata for an object specified by an identifier.
     *
     * @param loc_id
     *            IN: Identifier for target object
     * @param fields
     *            IN: Object fields to select
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static hdf.hdf5lib.structs.H5O_info_t H5Oget_info(long loc_id, int fields)
        throws HDF5LibraryException
    {
        hdf.hdf5lib.structs.H5O_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment info_segment = arena.allocate(H5O_info2_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_info3(loc_id, info_segment, fields) < 0)
                h5libraryError();
            // Unpack the H5O_info_t from the MemorySegment
            hdf.hdf5lib.structs.H5O_token_t token = new hdf.hdf5lib.structs.H5O_token_t(
                H5O_info2_t.token(info_segment).toArray(ValueLayout.JAVA_BYTE));
            info = new hdf.hdf5lib.structs.H5O_info_t(
                H5O_info2_t.fileno(info_segment), token, H5O_info2_t.type(info_segment),
                H5O_info2_t.rc(info_segment), H5O_info2_t.atime(info_segment),
                H5O_info2_t.mtime(info_segment), H5O_info2_t.ctime(info_segment),
                H5O_info2_t.btime(info_segment), H5O_info2_t.num_attrs(info_segment));
        }
        return info;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_info_by_idx retrieves the metadata for an object, identifying the object by an index position.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param group_name
     *            IN: Name of group, relative to loc_id, in which object is located
     * @param idx_type
     *            IN: Type of index by which objects are ordered
     * @param order
     *            IN: Order of iteration within index
     * @param n
     *            IN: Object to open
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used;
     *                pass as H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5O_info_t
    H5Oget_info_by_idx(long loc_id, String group_name, int idx_type, int order, long n, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Oget_info_by_idx(loc_id, group_name, idx_type, order, n, HDF5Constants.H5O_INFO_ALL,
                                  lapl_id);
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_info_by_idx retrieves the metadata for an object, identifying the object by an index position.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param group_name
     *            IN: Name of group, relative to loc_id, in which object is located
     * @param idx_type
     *            IN: Type of index by which objects are ordered
     * @param order
     *            IN: Order of iteration within index
     * @param n
     *            IN: Object to open
     * @param fields
     *            IN: Object fields to select
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used;
     *                pass as H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5O_info_t H5Oget_info_by_idx(long loc_id, String group_name,
                                                                    int idx_type, int order, long n,
                                                                    int fields, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("group_name is null");
        }

        hdf.hdf5lib.structs.H5O_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(group_name);
            MemorySegment info_segment = arena.allocate(H5O_info2_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_info_by_idx3(loc_id, name_segment, idx_type, order, n,
                                                                 info_segment, fields, lapl_id) < 0)
                h5libraryError();
            // Unpack the H5O_info_t from the MemorySegment
            hdf.hdf5lib.structs.H5O_token_t token = new hdf.hdf5lib.structs.H5O_token_t(
                H5O_info2_t.token(info_segment).toArray(ValueLayout.JAVA_BYTE));
            info = new hdf.hdf5lib.structs.H5O_info_t(
                H5O_info2_t.fileno(info_segment), token, H5O_info2_t.type(info_segment),
                H5O_info2_t.rc(info_segment), H5O_info2_t.atime(info_segment),
                H5O_info2_t.mtime(info_segment), H5O_info2_t.ctime(info_segment),
                H5O_info2_t.btime(info_segment), H5O_info2_t.num_attrs(info_segment));
        }
        return info;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_info_by_name retrieves the metadata for an object, identifying the object by location and
     * relative name.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of group in which object is located
     * @param name
     *            IN: Relative name of group
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used;
     *                pass as H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5O_info_t H5Oget_info_by_name(long loc_id, String name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Oget_info_by_name(loc_id, name, HDF5Constants.H5O_INFO_ALL, lapl_id);
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_info_by_name retrieves the metadata for an object, identifying the object by location and
     * relative name.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of group in which object is located
     * @param name
     *            IN: Relative name of group
     * @param fields
     *            IN: Object fields to select
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used;
     *                pass as H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5O_info_t H5Oget_info_by_name(long loc_id, String name, int fields,
                                                                     long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        hdf.hdf5lib.structs.H5O_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            MemorySegment info_segment = arena.allocate(H5O_info2_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_info_by_name3(loc_id, name_segment, info_segment, fields,
                                                                  lapl_id) < 0)
                h5libraryError();
            // Unpack the H5O_info_t from the MemorySegment
            hdf.hdf5lib.structs.H5O_token_t token = new hdf.hdf5lib.structs.H5O_token_t(
                H5O_info2_t.token(info_segment).toArray(ValueLayout.JAVA_BYTE));
            info = new hdf.hdf5lib.structs.H5O_info_t(
                H5O_info2_t.fileno(info_segment), token, H5O_info2_t.type(info_segment),
                H5O_info2_t.rc(info_segment), H5O_info2_t.atime(info_segment),
                H5O_info2_t.mtime(info_segment), H5O_info2_t.ctime(info_segment),
                H5O_info2_t.btime(info_segment), H5O_info2_t.num_attrs(info_segment));
        }
        return info;
    }

    /**
     * @ingroup JH5O
     *
     * H5Olink creates a new hard link to an object in an HDF5 file.
     *
     * @param obj_id
     *            IN: Object to be linked.
     * @param new_loc_id
     *            IN: File or group identifier specifying location at which object is to be linked.
     * @param new_name
     *            IN: Relative name of link to be created.
     * @param lcpl_id
     *            IN: Link creation property list identifier.
     * @param lapl_id
     *            IN: Access property list identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static void H5Olink(long obj_id, long new_loc_id, String new_name, long lcpl_id, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (new_name == null) {
            throw new NullPointerException("new_name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(new_name);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Olink(obj_id, new_loc_id, name_segment, lcpl_id, lapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5O
     *
     * H5Oopen opens a group, dataset, or named datatype specified by a location and a path name.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param name
     *            IN: Relative path to the object
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object
     *
     * @return an object identifier for the opened object
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Oopen(long loc_id, String name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (name == null) {
            throw new NullPointerException("Object name cannot be null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            id                         = org.hdfgroup.javahdf5.hdf5_h.H5Oopen(loc_id, name_segment, lapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Oopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5O
     *
     * H5Ovisit recursively visits all objects accessible from a specified object.
     *
     * @param obj_id
     *            IN: Identifier of the object at which the recursive iteration begins.
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order of iteration within index
     * @param op
     *            IN: Callback function passing data regarding the object to the calling application
     * @param op_data
     *            IN: User-defined pointer to data required by the application for its processing of the
     *                object
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Ovisit(long obj_id, int idx_type, int order, hdf.hdf5lib.callbacks.H5O_iterate_t op,
                               hdf.hdf5lib.callbacks.H5O_iterate_opdata_t op_data)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Ovisit(obj_id, idx_type, order, op, op_data, HDF5Constants.H5O_INFO_ALL);
    }

    /**
     * @ingroup JH5O
     *
     * H5Ovisit recursively visits all objects accessible from a specified object.
     *
     * @param obj_id
     *            IN: Identifier of the object at which the recursive iteration begins.
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order of iteration within index
     * @param op
     *            IN: Callback function passing data regarding the object to the calling application
     * @param op_data
     *            IN: User-defined pointer to data required by the application for its processing of the
     *                object
     * @param fields
     *            IN: Object fields to select
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Ovisit(long obj_id, int idx_type, int order, hdf.hdf5lib.callbacks.H5O_iterate_t op,
                               hdf.hdf5lib.callbacks.H5O_iterate_opdata_t op_data, int fields)
        throws HDF5LibraryException, NullPointerException
    {
        if (op == null) {
            throw new NullPointerException("op is null");
        }
        if (op_data == null) {
            throw new NullPointerException("op_data is null");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Call the native method to visit the objects
            MemorySegment op_segment = H5O_iterate2_t.allocate(op, arena);
            MemorySegment op_data_segment =
                Linker.nativeLinker().upcallStub(H5Ovisit3$handle(), H5Ovisit3$descriptor(), arena);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Ovisit3(obj_id, idx_type, order, op_segment,
                                                                 op_data_segment, fields)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5O
     *
     * H5Ovisit_by_name recursively visits all objects starting from a specified object.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param obj_name
     *            IN: Relative path to the object
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order of iteration within index
     * @param op
     *            IN: Callback function passing data regarding the object to the calling application
     * @param op_data
     *            IN: User-defined pointer to data required by the application for its processing of the
     *                object
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Ovisit_by_name(long loc_id, String obj_name, int idx_type, int order,
                                       hdf.hdf5lib.callbacks.H5O_iterate_t op,
                                       hdf.hdf5lib.callbacks.H5O_iterate_opdata_t op_data, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Ovisit_by_name(loc_id, obj_name, idx_type, order, op, op_data, HDF5Constants.H5O_INFO_ALL,
                                lapl_id);
    }

    /**
     * @ingroup JH5O
     *
     * H5Ovisit_by_name recursively visits all objects starting from a specified object.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param obj_name
     *            IN: Relative path to the object
     * @param idx_type
     *            IN: Type of index
     * @param order
     *            IN: Order of iteration within index
     * @param op
     *            IN: Callback function passing data regarding the object to the calling application
     * @param op_data
     *            IN: User-defined pointer to data required by the application for its processing of the
     *                object
     * @param fields
     *            IN: Object fields to select
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all
     *            members were processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Ovisit_by_name(long loc_id, String obj_name, int idx_type, int order,
                                       hdf.hdf5lib.callbacks.H5O_iterate_t op,
                                       hdf.hdf5lib.callbacks.H5O_iterate_opdata_t op_data, int fields,
                                       long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (obj_name == null) {
            throw new NullPointerException("obj_name is null");
        }
        if (op == null) {
            throw new NullPointerException("op is null");
        }
        if (op_data == null) {
            throw new NullPointerException("op_data is null");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment    = arena.allocateFrom(obj_name);
            MemorySegment op_segment      = H5O_iterate2_t.allocate(op, arena);
            MemorySegment op_data_segment = Linker.nativeLinker().upcallStub(
                H5Ovisit_by_name3$handle(), H5Ovisit_by_name3$descriptor(), arena);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Ovisit_by_name3(loc_id, name_segment, idx_type,
                                                                         order, op_segment, op_data_segment,
                                                                         fields, lapl_id)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oexists_by_name is used by an application to check that an existing link resolves to an object.
     * Primarily, it is designed to check for dangling soft, external, or user-defined links.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param obj_name
     *            IN: Relative path to the object
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return Returns TRUE or FALSE if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static boolean H5Oexists_by_name(long loc_id, String obj_name, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;

        if (obj_name == null) {
            throw new NullPointerException("obj_name is null");
        }

        boolean exists = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(obj_name);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Oexists_by_name(loc_id, name_segment, lapl_id);
        }
        if (retVal < 0) {
            h5libraryError();
        }
        else if (retVal > 0) {
            exists = true;
        }
        else {
            exists = false;
        }
        return exists;
    }

    /**
     * @ingroup JH5O
     *
     * H5Odecr_refcount decrements the hard link reference count for an object.
     *
     * @param object_id
     *            IN: Object identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Odecr_refcount(long object_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Odecr_refcount(object_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5O
     *
     * H5Oincr_refcount increments the hard link reference count for an object.
     *
     * @param object_id
     *            IN: Object identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Oincr_refcount(long object_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Oincr_refcount(object_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5O
     *
     * H5Oopen_by_token opens a group, dataset, or named datatype using its object token within an HDF5 file.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param token
     *            IN: Object's token in the file
     *
     * @return an object identifier for the opened object
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            token is null
     **/
    public static long H5Oopen_by_token(long loc_id, hdf.hdf5lib.structs.H5O_token_t token)
        throws HDF5LibraryException, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (token == null) {
            throw new NullPointerException("Token cannot be null");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment token_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, token.data);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Oopen_by_token(loc_id, token_segment);
        }

        if (id > 0) {
            log.trace("OPEN_IDS: H5Oopen_by_token add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oopen_by_idx opens the nth object in the group specified.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param group_name
     *            IN: Name of group, relative to loc_id, in which object is located
     * @param idx_type
     *            IN: Type of index by which objects are ordered
     * @param order
     *            IN: Order of iteration within index
     * @param n
     *            IN: Object to open
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object
     *
     * @return an object identifier for the opened object
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            group_name is null.
     **/
    public static long H5Oopen_by_idx(long loc_id, String group_name, int idx_type, int order, long n,
                                      long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (group_name == null) {
            throw new NullPointerException("Group name cannot be null");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(group_name);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Oopen_by_idx(loc_id, name_segment, idx_type, order, n,
                                                             lapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Oopen_by_idx add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oflush causes all buffers associated with an object to be immediately flushed to disk without
     * removing the data from the cache. object_id can be any named object associated with an HDF5 file
     * including a dataset, a group, or a committed datatype.
     *
     * @param object_id
     *            IN: Identifier of the object to be flushed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Oflush(long object_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Oflush(object_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5O
     *
     * H5Orefresh causes all buffers associated with an object to be cleared and immediately re-loaded with
     * updated contents from disk. This function essentially closes the object, evicts all metadata associated
     * with it from the cache, and then re-opens the object. The reopened object is automatically
     * re-registered with the same ID. object_id can be any named object associated with an HDF5 file
     * including a dataset, a group, or a committed datatype.
     *
     * @param object_id
     *            IN: Identifier of the object to be refreshed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Orefresh(long object_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Orefresh(object_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5O
     *
     * H5Odisable_mdc_flushes corks an object, keeping dirty entries associated with the object in the
     * metadata cache.
     *
     * @param object_id
     *            IN: Identifier of the object to be corked.
     **/
    public static void H5Odisable_mdc_flushes(long object_id)
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Odisable_mdc_flushes(object_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5O
     *
     * H5Oenable_mdc_flushes uncorks an object, keeping dirty entries associated with the object in the
     * metadata cache.
     *
     * @param object_id
     *            IN: Identifier of the object to be uncorked.
     **/
    public static void H5Oenable_mdc_flushes(long object_id)
    {
        // Call the native function to enable metadata cache flushes
        if (org.hdfgroup.javahdf5.hdf5_h.H5Oenable_mdc_flushes(object_id) < 0)
            h5libraryError();
    }

    /**
     * @ingroup JH5O
     *
     * H5Oare_mdc_flushes_disabled retrieve the object's "cork" status.
     *
     * @param object_id
     *            IN: Identifier of the object to be flushed.
     *
     * @return the cork status
     *            TRUE if mdc flushes for the object is disabled
     *            FALSE if mdc flushes for the object is not disabled
     **/
    public static boolean H5Oare_mdc_flushes_disabled(long object_id)
    {
        boolean are_mdc_flushes_disabled = false;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the boolean value
            MemorySegment are_mdc_flushes_disabled_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            // Call the native function to check if the library is thread-safe
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oare_mdc_flushes_disabled(
                    object_id, are_mdc_flushes_disabled_segment) < 0)
                h5libraryError();

            // Read the boolean value from the segment
            are_mdc_flushes_disabled = are_mdc_flushes_disabled_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }

        return are_mdc_flushes_disabled;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_native_info retrieves the native HDF5-specific metadata for an HDF5 object specified by an
     * identifier. Native HDF5-specific metadata includes things like object header information and object
     * storage layout information.
     *
     * @param loc_id
     *            IN: Identifier for target object
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static hdf.hdf5lib.structs.H5O_native_info_t H5Oget_native_info(long loc_id)
        throws HDF5LibraryException
    {
        hdf.hdf5lib.structs.H5O_native_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment info_segment = arena.allocate(org.hdfgroup.javahdf5.H5O_native_info_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_native_info(loc_id, info_segment,
                                                                HDF5Constants.H5O_NATIVE_INFO_ALL) < 0)
                h5libraryError();
            // Unpack the H5O_native_info_t from the MemorySegment
            MemorySegment hdr_segment       = org.hdfgroup.javahdf5.H5O_native_info_t.hdr(info_segment);
            MemorySegment space_segment     = org.hdfgroup.javahdf5.H5O_hdr_info_t.space(hdr_segment);
            MemorySegment mesg_segment      = org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg(hdr_segment);
            MemorySegment meta_size_segment = org.hdfgroup.javahdf5.H5O_native_info_t.meta_size(info_segment);
            MemorySegment obj_ih_segment =
                org.hdfgroup.javahdf5.H5O_native_info_t.meta_size.obj(meta_size_segment);
            MemorySegment attr_ih_segment =
                org.hdfgroup.javahdf5.H5O_native_info_t.meta_size.attr(meta_size_segment);
            hdf.hdf5lib.structs.H5O_hdr_info_t hdr = new hdf.hdf5lib.structs.H5O_hdr_info_t(
                org.hdfgroup.javahdf5.H5O_hdr_info_t.version(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.nmesgs(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.nchunks(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.flags(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.total(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.meta(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.mesg(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.free(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg.present(mesg_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg.shared(mesg_segment));
            hdf.hdf5lib.structs.H5_ih_info_t obj = new hdf.hdf5lib.structs.H5_ih_info_t(
                org.hdfgroup.javahdf5.H5_ih_info_t.index_size(obj_ih_segment),
                org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(obj_ih_segment));
            hdf.hdf5lib.structs.H5_ih_info_t attr = new hdf.hdf5lib.structs.H5_ih_info_t(
                org.hdfgroup.javahdf5.H5_ih_info_t.index_size(attr_ih_segment),
                org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(attr_ih_segment));
            info = new hdf.hdf5lib.structs.H5O_native_info_t(hdr, obj, attr);
        }
        return info;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_native_info retrieves the native HDF5-specific metadata for an HDF5 object specified by an
     * identifier. Native HDF5-specific metadata includes things like object header information and object
     * storage layout information.
     *
     * @param loc_id
     *            IN: Identifier for target object
     * @param fields
     *            IN: Object fields to select
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static hdf.hdf5lib.structs.H5O_native_info_t H5Oget_native_info(long loc_id, int fields)
        throws HDF5LibraryException
    {
        hdf.hdf5lib.structs.H5O_native_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment info_segment = arena.allocate(org.hdfgroup.javahdf5.H5O_native_info_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_native_info(loc_id, info_segment, fields) < 0)
                h5libraryError();
            // Unpack the H5O_native_info_t from the MemorySegment
            MemorySegment hdr_segment       = org.hdfgroup.javahdf5.H5O_native_info_t.hdr(info_segment);
            MemorySegment space_segment     = org.hdfgroup.javahdf5.H5O_hdr_info_t.space(hdr_segment);
            MemorySegment mesg_segment      = org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg(hdr_segment);
            MemorySegment meta_size_segment = org.hdfgroup.javahdf5.H5O_native_info_t.meta_size(info_segment);
            MemorySegment obj_ih_segment =
                org.hdfgroup.javahdf5.H5O_native_info_t.meta_size.obj(meta_size_segment);
            MemorySegment attr_ih_segment =
                org.hdfgroup.javahdf5.H5O_native_info_t.meta_size.attr(meta_size_segment);
            hdf.hdf5lib.structs.H5O_hdr_info_t hdr = new hdf.hdf5lib.structs.H5O_hdr_info_t(
                org.hdfgroup.javahdf5.H5O_hdr_info_t.version(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.nmesgs(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.nchunks(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.flags(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.total(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.meta(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.mesg(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.free(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg.present(mesg_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg.shared(mesg_segment));
            hdf.hdf5lib.structs.H5_ih_info_t obj = new hdf.hdf5lib.structs.H5_ih_info_t(
                org.hdfgroup.javahdf5.H5_ih_info_t.index_size(obj_ih_segment),
                org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(obj_ih_segment));
            hdf.hdf5lib.structs.H5_ih_info_t attr = new hdf.hdf5lib.structs.H5_ih_info_t(
                org.hdfgroup.javahdf5.H5_ih_info_t.index_size(attr_ih_segment),
                org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(attr_ih_segment));
            info = new hdf.hdf5lib.structs.H5O_native_info_t(hdr, obj, attr);
        }
        return info;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_native_info_by_idx retrieves the native HDF5-specific metadata for an HDF5 object, identifying
     * the object by an index position. Native HDF5-specific metadata includes things like object header
     * information and object storage layout information.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param group_name
     *            IN: Name of group, relative to loc_id, in which object is located
     * @param idx_type
     *            IN: Type of index by which objects are ordered
     * @param order
     *            IN: Order of iteration within index
     * @param n
     *            IN: Object to open
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used;
     *                pass as H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5O_native_info_t
    H5Oget_native_info_by_idx(long loc_id, String group_name, int idx_type, int order, long n, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Oget_native_info_by_idx(loc_id, group_name, idx_type, order, n,
                                         HDF5Constants.H5O_NATIVE_INFO_ALL, lapl_id);
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_native_info_by_idx retrieves the native HDF5-specific metadata for an HDF5 object, identifying
     * the object by an index position. Native HDF5-specific metadata includes things like object header
     * information and object storage layout information.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param group_name
     *            IN: Name of group, relative to loc_id, in which object is located
     * @param idx_type
     *            IN: Type of index by which objects are ordered
     * @param order
     *            IN: Order of iteration within index
     * @param n
     *            IN: Object to open
     * @param fields
     *            IN: Object fields to select
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used;
     *                pass as H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5O_native_info_t
    H5Oget_native_info_by_idx(long loc_id, String group_name, int idx_type, int order, long n, int fields,
                              long lapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (group_name == null) {
            throw new NullPointerException("group_name is null");
        }

        hdf.hdf5lib.structs.H5O_native_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(group_name);
            MemorySegment info_segment = arena.allocate(org.hdfgroup.javahdf5.H5O_native_info_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_native_info_by_idx(loc_id, name_segment, idx_type, order,
                                                                       n, info_segment, fields, lapl_id) < 0)
                h5libraryError();
            // Unpack the H5O_native_info_t from the MemorySegment
            MemorySegment hdr_segment       = org.hdfgroup.javahdf5.H5O_native_info_t.hdr(info_segment);
            MemorySegment space_segment     = org.hdfgroup.javahdf5.H5O_hdr_info_t.space(hdr_segment);
            MemorySegment mesg_segment      = org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg(hdr_segment);
            MemorySegment meta_size_segment = org.hdfgroup.javahdf5.H5O_native_info_t.meta_size(info_segment);
            MemorySegment obj_ih_segment =
                org.hdfgroup.javahdf5.H5O_native_info_t.meta_size.obj(meta_size_segment);
            MemorySegment attr_ih_segment =
                org.hdfgroup.javahdf5.H5O_native_info_t.meta_size.attr(meta_size_segment);
            hdf.hdf5lib.structs.H5O_hdr_info_t hdr = new hdf.hdf5lib.structs.H5O_hdr_info_t(
                org.hdfgroup.javahdf5.H5O_hdr_info_t.version(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.nmesgs(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.nchunks(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.flags(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.total(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.meta(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.mesg(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.free(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg.present(mesg_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg.shared(mesg_segment));
            hdf.hdf5lib.structs.H5_ih_info_t obj = new hdf.hdf5lib.structs.H5_ih_info_t(
                org.hdfgroup.javahdf5.H5_ih_info_t.index_size(obj_ih_segment),
                org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(obj_ih_segment));
            hdf.hdf5lib.structs.H5_ih_info_t attr = new hdf.hdf5lib.structs.H5_ih_info_t(
                org.hdfgroup.javahdf5.H5_ih_info_t.index_size(attr_ih_segment),
                org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(attr_ih_segment));
            info = new hdf.hdf5lib.structs.H5O_native_info_t(hdr, obj, attr);
        }
        return info;
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_native_info_by_name retrieves the native HDF5-specific metadata for an HDF5 object, identifying
     * the object by location and relative name. Native HDF5-specific metadata includes things like object
     * header information and object storage layout information.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of group in which object is located
     * @param name
     *            IN: Relative name of group
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used;
     *                pass as H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5O_native_info_t H5Oget_native_info_by_name(long loc_id, String name,
                                                                                   long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        return H5Oget_native_info_by_name(loc_id, name, HDF5Constants.H5O_NATIVE_INFO_ALL, lapl_id);
    }

    /**
     * @ingroup JH5O
     *
     * H5Oget_native_info_by_name retrieves the native HDF5-specific metadata for an HDF5 object, identifying
     * the object by location and relative name. Native HDF5-specific metadata includes things like object
     * header information and object storage layout information.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of group in which object is located
     * @param name
     *            IN: Relative name of group
     * @param fields
     *            IN: Object fields to select
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used;
     *                pass as H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static hdf.hdf5lib.structs.H5O_native_info_t H5Oget_native_info_by_name(long loc_id, String name,
                                                                                   int fields, long lapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        hdf.hdf5lib.structs.H5O_native_info_t info = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            MemorySegment info_segment = arena.allocate(org.hdfgroup.javahdf5.H5O_native_info_t.sizeof());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Oget_native_info_by_name(loc_id, name_segment, info_segment,
                                                                        fields, lapl_id) < 0)
                h5libraryError();
            // Unpack the H5O_native_info_t from the MemorySegment
            MemorySegment hdr_segment       = org.hdfgroup.javahdf5.H5O_native_info_t.hdr(info_segment);
            MemorySegment space_segment     = org.hdfgroup.javahdf5.H5O_hdr_info_t.space(hdr_segment);
            MemorySegment mesg_segment      = org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg(hdr_segment);
            MemorySegment meta_size_segment = org.hdfgroup.javahdf5.H5O_native_info_t.meta_size(info_segment);
            MemorySegment obj_ih_segment =
                org.hdfgroup.javahdf5.H5O_native_info_t.meta_size.obj(meta_size_segment);
            MemorySegment attr_ih_segment =
                org.hdfgroup.javahdf5.H5O_native_info_t.meta_size.attr(meta_size_segment);
            hdf.hdf5lib.structs.H5O_hdr_info_t hdr = new hdf.hdf5lib.structs.H5O_hdr_info_t(
                org.hdfgroup.javahdf5.H5O_hdr_info_t.version(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.nmesgs(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.nchunks(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.flags(hdr_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.total(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.meta(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.mesg(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.space.free(space_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg.present(mesg_segment),
                org.hdfgroup.javahdf5.H5O_hdr_info_t.mesg.shared(mesg_segment));
            hdf.hdf5lib.structs.H5_ih_info_t obj = new hdf.hdf5lib.structs.H5_ih_info_t(
                org.hdfgroup.javahdf5.H5_ih_info_t.index_size(obj_ih_segment),
                org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(obj_ih_segment));
            hdf.hdf5lib.structs.H5_ih_info_t attr = new hdf.hdf5lib.structs.H5_ih_info_t(
                org.hdfgroup.javahdf5.H5_ih_info_t.index_size(attr_ih_segment),
                org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(attr_ih_segment));
            info = new hdf.hdf5lib.structs.H5O_native_info_t(hdr, obj, attr);
        }
        return info;
    }

    // /////// unimplemented ////////
    // herr_t H5Otoken_cmp(hid_t loc_id, const H5O_token_t *token1, const H5O_token_t *token2,
    //            int *cmp_value);
    // herr_t H5Otoken_to_str(hid_t loc_id, const H5O_token_t *token, char **token_str);
    // herr_t H5Otoken_from_str(hid_t loc_id, const char *token_str, H5O_token_t *token);

    // ////////////////////////////////////////////////////////////
    // //
    // H5P: Property List Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // /////// Generic property list routines ///////
    /**
     * @defgroup JH5P Java Property List (H5P) Interface
     *
     * @see H5P, C-API
     *
     * @see @ref H5P_UG, User Guide
     **/

    /**
     * @ingroup JH5P
     *
     * H5Pget_class_name retrieves the name of a generic property list class
     *
     * @param plid
     *            IN: Identifier of property object to query
     * @return name of a property list if successful; null if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static String H5Pget_class_name(long plid) throws HDF5LibraryException
    {
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        String ret_name = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment class_segment = org.hdfgroup.javahdf5.hdf5_h.H5Pget_class_name(plid);
            if (class_segment == null)
                h5libraryError();
            ret_name = class_segment.reinterpret(Long.MAX_VALUE).getString(0L);
            org.hdfgroup.javahdf5.hdf5_h.H5free_memory(class_segment);
        }
        return ret_name;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pcreate creates a new property as an instance of some property list class.
     *
     * @param type
     *            IN: The type of property list to create.
     *
     * @return a property list identifier (plist) if successful; otherwise Fail (-1).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Pcreate(long type) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Pcreate(type);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Pcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget retrieves a copy of the value for a property in a property list (support integer only)
     *
     * @param plid
     *            IN: Identifier of property object to query
     * @param name
     *            IN: Name of property to query
     * @return value for a property if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     */
    public static int H5Pget(long plid, String name) throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int value = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment  = arena.allocateFrom(name);
            MemorySegment value_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget(plid, name_segment, value_segment) < 0)
                h5libraryError();
            value = value_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return value;
    }

    /**
     * @ingroup JH5P
     *
     * Sets a property list value (support integer only)
     *
     * @param plid
     *            IN: Property list identifier to modify
     * @param name
     *            IN: Name of property to modify
     * @param value
     *            IN: value to set the property to
     * @return a non-negative value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     */
    public static int H5Pset(long plid, String name, int value)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int ret = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment  = arena.allocateFrom(name);
            MemorySegment value_segment = arena.allocate(ValueLayout.JAVA_INT);
            value_segment.set(ValueLayout.JAVA_INT, 0, value);
            ret = org.hdfgroup.javahdf5.hdf5_h.H5Pset(plid, name_segment, value_segment);
            if (ret < 0)
                h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pexist determines whether a property exists within a property list or class
     *
     * @param plid
     *            IN: Identifier for the property to query
     * @param name
     *            IN: Name of property to check for
     * @return a true value if the property exists in the property object; false if the property does not
     *     exist;
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     */
    public static boolean H5Pexist(long plid, String name) throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;

        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        boolean exists = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            retVal                     = org.hdfgroup.javahdf5.hdf5_h.H5Pexist(plid, name_segment);
        }
        if (retVal < 0) {
            h5libraryError();
        }
        else if (retVal > 0) {
            exists = true;
        }
        else {
            exists = false;
        }
        return exists;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_size retrieves the size of a property's value in bytes
     *
     * @param plid
     *            IN: Identifier of property object to query
     * @param name
     *            IN: Name of property to query
     * @return size of a property's value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     */
    public static long H5Pget_size(long plid, String name) throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        long size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_size(plid, name_segment, size_segment) < 0)
                h5libraryError();
            size = size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_nprops retrieves the number of properties in a property list or class
     *
     * @param plid
     *            IN: Identifier of property object to query
     * @return number of properties if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static long H5Pget_nprops(long plid) throws HDF5LibraryException
    {
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        long nprops = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment nprops_segment = arena.allocate(ValueLayout.JAVA_LONG);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_nprops(plid, nprops_segment) < 0)
                h5libraryError();
            nprops = nprops_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return nprops;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_class returns the property list class for the property list identified by the plist parameter.
     *
     * @param plist
     *            IN: Identifier of property list to query.
     * @return a property list class if successful. Otherwise returns H5P_ROOT (-1).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Pget_class(long plist) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        long classId = org.hdfgroup.javahdf5.hdf5_h.H5Pget_class(plist);
        if (classId < 0) {
            h5libraryError();
        }
        log.trace("OPEN_IDS: H5Pget_class add {}", classId);
        OPEN_IDS.add(classId);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return classId;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_class_parent retrieves an identifier for the parent class of a property class
     *
     * @param plid
     *            IN: Identifier of the property class to query
     * @return a valid parent class object identifier if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static long H5Pget_class_parent(long plid) throws HDF5LibraryException
    {
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        long parentId = org.hdfgroup.javahdf5.hdf5_h.H5Pget_class_parent(plid);
        if (parentId < 0) {
            h5libraryError();
        }
        log.trace("OPEN_IDS: H5Pget_class_parent add {}", parentId);
        OPEN_IDS.add(parentId);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return parentId;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pequal determines if two property lists or classes are equal
     *
     * @param plid1
     *            IN: First property object to be compared
     * @param plid2
     *            IN: Second property object to be compared
     *
     * @return positive value if equal; zero if unequal, a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static int H5Pequal(long plid1, long plid2) throws HDF5LibraryException
    {
        if (plid1 < 0 || plid2 < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int ret = org.hdfgroup.javahdf5.hdf5_h.H5Pequal(plid1, plid2);
        if (ret < 0) {
            h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pequal determines if two property lists or classes are equal
     *
     * @param plid1
     *            IN: First property object to be compared
     * @param plid2
     *            IN: Second property object to be compared
     *
     * @return TRUE if equal, FALSE if unequal
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static boolean H5P_equal(long plid1, long plid2) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Pequal(plid1, plid2) == 1)
            return true;
        return false;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pisa_class checks to determine whether a property list is a member of the specified class
     *
     * @param plist
     *            IN: Identifier of the property list
     * @param pclass
     *            IN: Identifier of the property class
     * @return a positive value if equal; zero if unequal; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static int H5Pisa_class(long plist, long pclass) throws HDF5LibraryException
    {
        if (plist < 0 || pclass < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int ret = org.hdfgroup.javahdf5.hdf5_h.H5Pisa_class(plist, pclass);
        if (ret < 0) {
            h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pcopy_prop copies a property from one property list or class to another
     *
     * @param dst_id
     *            IN: Identifier of the destination property list or class
     * @param src_id
     *            IN: Identifier of the source property list or class
     * @param name
     *            IN: Name of the property to copy
     * @return a non-negative value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     */
    public static int H5Pcopy_prop(long dst_id, long src_id, String name)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        int ret = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            ret = org.hdfgroup.javahdf5.hdf5_h.H5Pcopy_prop(dst_id, src_id, name_segment);
            if (ret < 0)
                h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5P
     *
     * H5Premove removes a property from a property list
     *
     * @param plid
     *            IN: Identifier of the property list to modify
     * @param name
     *            IN: Name of property to remove
     * @return a non-negative value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     */
    public static int H5Premove(long plid, String name) throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int ret = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            ret                        = org.hdfgroup.javahdf5.hdf5_h.H5Premove(plid, name_segment);
            if (ret < 0)
                h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5P
     *
     * H5Punregister removes a property from a property list class
     *
     * @param plid
     *            IN: Property list class from which to remove permanent property
     * @param name
     *            IN: Name of property to remove
     * @return a non-negative value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     */
    public static int H5Punregister(long plid, String name) throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (plid < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int ret = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            ret                        = org.hdfgroup.javahdf5.hdf5_h.H5Punregister(plid, name_segment);
            if (ret < 0)
                h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5P
     *
     * Closes an existing property list class
     *
     * @param plid
     *            IN: Property list class to close
     * @return a non-negative value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static int H5Pclose_class(long plid) throws HDF5LibraryException
    {
        if (plid < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Pclose_class remove {}", plid);
        OPEN_IDS.remove(plid);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return org.hdfgroup.javahdf5.hdf5_h.H5Pclose_class(plid);
    }

    /**
     * @ingroup JH5P
     *
     * H5Pclose terminates access to a property list.
     *
     * @param plist
     *            IN: Identifier of the property list to terminate access to.
     * @return a non-negative value if successful
     **/
    public static int H5Pclose(long plist)
    {
        log.trace("OPEN_IDS: H5Pclose remove {}", plist);
        OPEN_IDS.remove(plist);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pclose(plist);
        if (retVal < 0)
            h5libraryError();
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pcopy copies an existing property list to create a new property list.
     *
     * @param plist
     *            IN: Identifier of property list to duplicate.
     *
     * @return a property list identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Pcopy(long plist) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Pcopy(plist);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Pcopy add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    // Define property list class callback function pointer types
    // typedef herr_t (*H5P_cls_create_func_t)(hid_t prop_id, void *create_data);
    // typedef herr_t (*H5P_cls_copy_func_t)(hid_t new_prop_id, hid_t old_prop_id, void *copy_data);
    // typedef herr_t (*H5P_cls_close_func_t)(hid_t prop_id, void *close_data);

    // Define property list callback function pointer types
    // typedef herr_t (*H5P_prp_cb1_t)(const char *name, size_t size, void *value);
    // typedef herr_t (*H5P_prp_cb2_t)(hid_t prop_id, const char *name, size_t size, void *value);
    // typedef H5P_prp_cb1_t H5P_prp_create_func_t;
    // typedef H5P_prp_cb2_t H5P_prp_set_func_t;
    // typedef H5P_prp_cb2_t H5P_prp_get_func_t;
    // typedef herr_t (*H5P_prp_encode_func_t)(const void *value, void **buf, size_t *size);
    // typedef herr_t (*H5P_prp_decode_func_t)(const void **buf, void *value);
    // typedef H5P_prp_cb2_t H5P_prp_delete_func_t;
    // typedef H5P_prp_cb1_t H5P_prp_copy_func_t;
    // typedef int (*H5P_prp_compare_func_t)(const void *value1, const void *value2, size_t size);
    // typedef H5P_prp_cb1_t H5P_prp_close_func_t;

    // Define property list iteration function type
    // typedef herr_t (*H5P_iterate_t)(hid_t id, const char *name, void *iter_data);

    /**
     * @ingroup JH5P
     *
     * H5Pcreate_class_nocb creates an new property class with no callback functions.
     *
     * @param parent_class
     *            IN: Identifier of the parent property class.
     * @param name
     *            IN: Name of the property class.
     *
     * @return a property list identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     **/
    public static long H5Pcreate_class_nocb(long parent_class, String name)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (parent_class < 0) {
            throw new HDF5FunctionArgumentException("Negative parent class identifier");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            id                         = org.hdfgroup.javahdf5.hdf5_h.H5Pcreate_class(
                parent_class, name_segment, MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL,
                MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Pcreate_class_nocb add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();
        return id;
    }

    //    public static long H5Pcreate_class(long parent_class, String name, H5P_cls_create_func_cb create_op,
    //    H5P_cls_create_func_t create_data,
    //             H5P_cls_copy_func_cb copy_op, H5P_cls_copy_func_t copy_data, H5P_cls_close_func_cb
    //             close_op, H5P_cls_close_func_t close_data) throws HDF5LibraryException {
    //        long id = _H5Pcreate_class(parent_class, name, create_op, create_data, copy_op, copy_data,
    //        close_op, close_data);
    //          if (id > 0) {
    //            log.trace("OPEN_IDS: H5Pcreate_class add {}", id);
    //            OPEN_IDS.add(id);
    //            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
    //        }
    //        return id;
    //    }
    //
    //    private  static  long _H5Pcreate_class(long parent_class, String name,
    //    H5P_cls_create_func_cb create_op, H5P_cls_create_func_t create_data,
    //            H5P_cls_copy_func_cb copy_op, H5P_cls_copy_func_t copy_data, H5P_cls_close_func_cb close_op,
    //            H5P_cls_close_func_t close_data) throws HDF5LibraryException {}

    /**
     * @ingroup JH5P
     *
     * H5Pregister2_nocb registers a property list with no callback functions.
     *
     * @param plist_class
     *            IN: Identifier of the property list.
     * @param name
     *            IN: Name of the property.
     * @param size
     *            IN: Size the property value.
     * @param def_value
     *            IN: Default value of the property
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     **/
    public static void H5Pregister2_nocb(long plist_class, String name, long size, byte[] def_value)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (def_value == null) {
            throw new NullPointerException("def_value is null");
        }
        if (plist_class < 0) {
            throw new HDF5FunctionArgumentException("Negative property list class identifier");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            // Allocate a MemorySegment for the default value
            MemorySegment def_value_segment = arena.allocate(size);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pregister2(
                    plist_class, name_segment, size, def_value_segment, MemorySegment.NULL,
                    MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL,
                    MemorySegment.NULL, MemorySegment.NULL) < 0)
                h5libraryError();
        }
    }

    //    public  static  void H5Pregister2(long plist_class, String name, long size, byte[]
    //    def_value, H5P_prp_create_func_cb prp_create, H5P_prp_set_func_cb prp_set,
    //          H5P_prp_get_func_cb prp_get, H5P_prp_delete_func_cb prp_delete, H5P_prp_copy_func_cb prp_copy,
    //          H5P_prp_compare_func_cb prp_cmp, H5P_prp_close_func_cb prp_close) throws HDF5LibraryException
    //          {}

    /**
     * @ingroup JH5P
     *
     * H5Pinsert2_nocb inserts a property list with no callback functions.
     *
     * @param plist
     *            IN: Identifier of the property list.
     * @param name
     *            IN: Name of the property.
     * @param size
     *            IN: Size the property value.
     * @param value
     *            IN: Default value of the property
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *           name is null.
     **/
    public static void H5Pinsert2_nocb(long plist, String name, long size, byte[] value)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom(name);
            // Allocate a MemorySegment for the default value
            MemorySegment value_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, size);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pinsert2(
                    plist, name_segment, size, value_segment, MemorySegment.NULL, MemorySegment.NULL,
                    MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL) < 0)
                h5libraryError();
        }
    }

    // public  static  void H5Pinsert2(long plist, String name, long size,  byte[] value,
    // H5P_prp_set_func_cb prp_set, H5P_prp_get_func_cb prp_get,
    //      H5P_prp_delete_func_cb prp_delete, H5P_prp_copy_func_cb prp_copy, H5P_prp_compare_func_cb prp_cmp,
    //      H5P_prp_close_func_cb prp_close) throws HDF5LibraryException {}

    /**
     * @ingroup JH5P
     *
     * H5Piterate iterates over the properties in a property list or class
     *
     * @param  plist
     *            IN: ID of property object to iterate over
     * @param  idx
     *            IN/OUT: index of the property to begin with
     * @param  op
     *            IN: function to be called with each property iterated over.
     * @param  op_data
     *            IN: iteration data from user
     *
     * @return    the return value of the last call to op if it was non-zero,
     *            zero if all properties have been processed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     *
     **/
    public static int H5Piterate(long plist, int[] idx, hdf.hdf5lib.callbacks.H5P_iterate_cb op,
                                 hdf.hdf5lib.callbacks.H5P_iterate_t op_data)
        throws HDF5LibraryException, NullPointerException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment idx_segment = MemorySegment.NULL;
            if (idx != null)
                idx_segment = arena.allocateFrom(ValueLayout.JAVA_INT, idx);
            MemorySegment op_segment = H5P_iterate_t.allocate(op, arena);
            MemorySegment op_data_segment =
                Linker.nativeLinker().upcallStub(H5Piterate$handle(), H5Piterate$descriptor(), arena);

            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Piterate(plist, idx_segment, op_segment,
                                                                  op_data_segment)) < 0)
                h5libraryError();
            if (idx != null)
                idx[0] = idx_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return status;
    }

    // /////// Object creation property list (OCPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_attr_phase_change retrieves attribute storage phase change thresholds.
     *
     * @param ocpl_id
     *            IN: Object (dataset or group) creation property list identifier
     * @param attributes
     *            The maximum and minimum no. of attributes to be stored.
     *
     * <pre>
     *      attributes[0] =  The maximum number of attributes to be stored in compact storage
     *      attributes[1] =  The minimum number of attributes to be stored in dense storage
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     *
     **/
    public static int H5Pget_attr_phase_change(long ocpl_id, int[] attributes)
        throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;
        if (attributes == null || attributes.length < 2) {
            throw new NullPointerException("attributes is null or has less than 2 elements");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment max_compact_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment min_dense_segment   = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_attr_phase_change(ocpl_id, max_compact_segment,
                                                                                min_dense_segment)) < 0)
                h5libraryError();
            attributes[0] = max_compact_segment.get(ValueLayout.JAVA_INT, 0);
            attributes[1] = min_dense_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_attr_phase_change sets threshold values for attribute storage on an object. These
     *      thresholds determine the point at which attribute storage changes
     *      from compact storage (i.e., storage in the object header)
     *      to dense storage (i.e., storage in a heap and indexed with a B-tree).
     *
     * @param ocpl_id
     *            IN: : Object (dataset or group) creation property list identifier
     * @param max_compact
     *            IN: Maximum number of attributes to be stored in compact storage (Default: 8)
     * @param min_dense
     *            IN: Minimum number of attributes to be stored in dense storage (Default: 6)
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_attr_phase_change(long ocpl_id, int max_compact, int min_dense)
        throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_attr_phase_change(ocpl_id, max_compact, min_dense) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_attr_creation_order retrieves the settings for tracking and indexing attribute creation order on
     * an object.
     *
     * @param ocpl_id
     *            IN: Object (group or dataset) creation property list identifier
     *
     * @return Flags specifying whether to track and index attribute creation order
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_attr_creation_order(long ocpl_id) throws HDF5LibraryException
    {
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment int_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_attr_creation_order(ocpl_id, int_segment)) < 0)
                h5libraryError();
            retVal = int_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_attr_creation_order sets flags specifying whether to track and index attribute creation order on
     * an object.
     *
     * @param ocpl_id
     *            IN: Object creation property list identifier
     * @param crt_order_flags
     *            IN: Flags specifying whether to track and index attribute creation order
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_attr_creation_order(long ocpl_id, int crt_order_flags)
        throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_attr_creation_order(ocpl_id, crt_order_flags);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_obj_track_times queries the object creation property list, ocpl_id, to determine whether object
     * times are being recorded.
     *
     * @param ocpl_id
     *            IN: Object creation property list identifier
     *
     * @return TRUE or FALSE, specifying whether object times are being recorded
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static boolean H5Pget_obj_track_times(long ocpl_id) throws HDF5LibraryException
    {
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment int_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_obj_track_times(ocpl_id, int_segment)) < 0)
                h5libraryError();
            retVal = int_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal == 1;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_obj_track_times sets a property in the object creation property list, ocpl_id, that governs the
     * recording of times associated with an object.
     *
     * @param ocpl_id
     *            IN: Object creation property list identifier
     *
     * @param track_times
     *            IN: TRUE or FALSE, specifying whether object times are to be tracked
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_obj_track_times(long ocpl_id, boolean track_times) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_obj_track_times(ocpl_id, track_times);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pmodify_filter modifies the specified FILTER in the transient or permanent output filter pipeline
     *              depending on whether PLIST is a dataset creation or dataset
     *              transfer property list.  The FLAGS argument specifies certain
     *              general properties of the filter and is documented below.
     *              The CD_VALUES is an array of CD_NELMTS integers which are
     *              auxiliary data for the filter.  The integer values will be
     *              stored in the dataset object header as part of the filter
     *              information.
     *<p>
     *              The FLAGS argument is a bit vector of the following fields:
     *<p>
     *              H5Z_FLAG_OPTIONAL(0x0001)
     *              If this bit is set then the filter is optional.  If the
     *              filter fails during an H5Dwrite() operation then the filter
     *              is just excluded from the pipeline for the chunk for which it
     *              failed; the filter will not participate in the pipeline
     *              during an H5Dread() of the chunk.  If this bit is clear and
     *              the filter fails then the entire I/O operation fails.
     *              If this bit is set but encoding is disabled for a filter,
     *              attempting to write will generate an error.
     *<p>
     * Note:        This function currently supports only the permanent filter
     *              pipeline.  That is, PLIST_ID must be a dataset creation
     *              property list.
     *
     * @param plist
     *            IN: Property list identifier.
     * @param filter
     *            IN: Filter to be modified to the pipeline.
     * @param flags
     *            IN: Bit vector specifying certain general properties of the filter.
     * @param cd_nelmts
     *            IN: Number of elements in cd_values
     * @param cd_values
     *            IN: Auxiliary data for the filter.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name or an array is null.
     *
     **/
    public static int H5Pmodify_filter(long plist, int filter, int flags, long cd_nelmts, int[] cd_values)
        throws HDF5LibraryException, NullPointerException
    {
        if (cd_values == null) {
            throw new NullPointerException("cd_values is null");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment cd_values_segment = arena.allocateFrom(ValueLayout.JAVA_INT, cd_values);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pmodify_filter(plist, filter, flags, cd_nelmts,
                                                                        cd_values_segment)) < 0) {
                h5libraryError();
            }
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_filter adds the specified filter and corresponding properties to the end of an output filter
     * pipeline.
     *
     * @param plist
     *            IN: Property list identifier.
     * @param filter
     *            IN: Filter to be added to the pipeline.
     * @param flags
     *            IN: Bit vector specifying certain general properties of the filter.
     * @param cd_nelmts
     *            IN: Number of elements in cd_values
     * @param cd_values
     *            IN: Auxiliary data for the filter.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            cd_values array is null.
     **/
    public static int H5Pset_filter(long plist, int filter, int flags, long cd_nelmts, int[] cd_values)
        throws HDF5LibraryException, NullPointerException
    {
        if (cd_values == null) {
            throw new NullPointerException("cd_values is null");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment cd_values_segment = arena.allocateFrom(ValueLayout.JAVA_INT, cd_values);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_filter(plist, filter, flags, cd_nelmts,
                                                                     cd_values_segment)) < 0) {
                h5libraryError();
            }
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_nfilters returns the number of filters defined in the filter pipeline associated with the
     * property list plist.
     *
     * @param plist
     *            IN: Property list identifier.
     *
     * @return the number of filters in the pipeline if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pget_nfilters(long plist) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_nfilters(plist);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_filter returns information about a filter, specified by its filter number, in a filter pipeline,
     * specified by the property list with which it is associated.
     *
     * @param plist
     *            IN: Property list identifier.
     * @param filter_number
     *            IN: Sequence number within the filter pipeline of the filter for which information is
     *                sought.
     * @param flags
     *            OUT: Bit vector specifying certain general properties of the filter.
     * @param cd_nelmts
     *            IN/OUT: Number of elements in cd_values
     * @param cd_values
     *            OUT: Auxiliary data for the filter.
     * @param namelen
     *            IN: Anticipated number of characters in name.
     * @param name
     *            OUT: Name of the filter.
     * @param filter_config
     *            OUT:A bit field encoding the returned filter information
     *
     * @return the filter identification number if successful. Otherwise returns H5Z_FILTER_ERROR (-1).
     *
     * @exception ArrayIndexOutOfBoundsException
     *            Fatal error on Copyback
     * @exception ArrayStoreException
     *            Fatal error on Copyback
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name or an array is null.
     *
     **/
    public static int H5Pget_filter(long plist, int filter_number, int[] flags, long[] cd_nelmts,
                                    int[] cd_values, long namelen, String[] name, int[] filter_config)
        throws ArrayIndexOutOfBoundsException, ArrayStoreException, HDF5LibraryException, NullPointerException
    {
        return hdf.hdf5lib.H5.H5Pget_filter2(plist, filter_number, flags, cd_nelmts, cd_values, namelen, name,
                                             filter_config);
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_filter2 returns information about a filter, specified by its filter number, in a filter
     * pipeline, specified by the property list with which it is associated.
     *
     * @see public static int H5Pget_filter(int plist, int filter_number, int[] flags, int[] cd_nelmts, int[]
     *      cd_values, int namelen, String[] name, int[] filter_config)
     *
     **/
    private static int H5Pget_filter2(long plist, int filter_number, int[] flags, long[] cd_nelmts,
                                      int[] cd_values, long namelen, String[] name, int[] filter_config)
        throws ArrayIndexOutOfBoundsException, ArrayStoreException, HDF5LibraryException, NullPointerException
    {
        if (flags == null || cd_nelmts == null || cd_values == null || name == null ||
            filter_config == null) {
            throw new NullPointerException("One or more arrays are null");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flags_segment         = arena.allocateFrom(ValueLayout.JAVA_INT, flags);
            MemorySegment cd_nelmts_segment     = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment cd_values_segment     = arena.allocateFrom(ValueLayout.JAVA_INT, cd_values);
            MemorySegment name_segment          = arena.allocate(namelen + 1);
            MemorySegment filter_config_segment = arena.allocate(ValueLayout.JAVA_INT, 1);

            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_filter2(
                     plist, filter_number, flags_segment, cd_nelmts_segment, cd_values_segment, namelen,
                     name_segment, filter_config_segment)) < 0) {
                h5libraryError();
            }
            cd_nelmts[0]     = cd_values_segment.get(ValueLayout.JAVA_INT, 0);
            filter_config[0] = filter_config_segment.get(ValueLayout.JAVA_INT, 0);
            name[0]          = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_filter_by_id returns information about the filter specified in filter_id, a filter identifier.
     * plist_id must be a dataset or group creation property list and filter_id must be in the associated
     * filter pipeline. The filter_id and flags parameters are used in the same manner as described in the
     * discussion of H5Pset_filter. Aside from the fact that they are used for output, the parameters
     * cd_nelmts and cd_values[] are used in the same manner as described in the discussion of H5Pset_filter.
     * On input, the cd_nelmts parameter indicates the number of entries in the cd_values[] array allocated by
     * the calling program; on exit it contains the number of values defined by the filter. On input, the
     * namelen parameter indicates the number of characters allocated for the filter name by the calling
     * program in the array name[]. On exit name[] contains the name of the filter with one character of the
     * name in each element of the array. If the filter specified in filter_id is not set for the property
     * list, an error will be returned and H5Pget_filter_by_id1 will fail.
     *
     * @param plist_id
     *            IN: Property list identifier.
     * @param filter_id
     *            IN: Filter identifier.
     * @param flags
     *            OUT: Bit vector specifying certain general properties of the filter.
     * @param cd_nelmts
     *            N/OUT: Number of elements in cd_values
     * @param cd_values
     *            OUT: Auxiliary data for the filter.
     * @param namelen
     *            IN: Anticipated number of characters in name.
     * @param name
     *            OUT: Name of the filter.
     * @param filter_config
     *            OUT: A bit field encoding the returned filter information
     *
     * @return the filter identification number if successful. Otherwise returns H5Z_FILTER_ERROR (-1).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception ArrayIndexOutOfBoundsException
     *            Fatal error on Copyback
     * @exception ArrayStoreException
     *            Fatal error on Copyback
     * @exception NullPointerException
     *            name or an array is null.
     *
     **/
    public static int H5Pget_filter_by_id(long plist_id, int filter_id, int[] flags, long[] cd_nelmts,
                                          int[] cd_values, long namelen, String[] name, int[] filter_config)
        throws ArrayIndexOutOfBoundsException, ArrayStoreException, HDF5LibraryException, NullPointerException
    {
        return hdf.hdf5lib.H5.H5Pget_filter_by_id2(plist_id, filter_id, flags, cd_nelmts, cd_values, namelen,
                                                   name, filter_config);
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_filter_by_id2 returns information about a filter, specified by its filter id, in a filter
     * pipeline, specified by the property list with which it is associated.
     *
     * @param plist_id
     *            IN: Property list identifier.
     * @param filter_id
     *            IN: Filter identifier.
     * @param flags
     *            OUT: Bit vector specifying certain general properties of the filter.
     * @param cd_nelmts
     *            N/OUT: Number of elements in cd_values
     * @param cd_values
     *            OUT: Auxiliary data for the filter.
     * @param namelen
     *            IN: Anticipated number of characters in name.
     * @param name
     *            OUT: Name of the filter.
     * @param filter_config
     *            OUT: A bit field encoding the returned filter information
     *
     * @return the filter identification number if successful. Otherwise returns H5Z_FILTER_ERROR (-1).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name or an array is null.
     *
     **/
    public static int H5Pget_filter_by_id2(long plist_id, int filter_id, int[] flags, long[] cd_nelmts,
                                           int[] cd_values, long namelen, String[] name, int[] filter_config)
        throws HDF5LibraryException, NullPointerException
    {
        if (flags == null || cd_nelmts == null || cd_values == null || name == null ||
            filter_config == null) {
            throw new NullPointerException("One or more arrays are null");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flags_segment         = arena.allocateFrom(ValueLayout.JAVA_INT, flags);
            MemorySegment cd_nelmts_segment     = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment cd_values_segment     = arena.allocateFrom(ValueLayout.JAVA_INT, cd_values);
            MemorySegment name_segment          = arena.allocate(namelen + 1);
            MemorySegment filter_config_segment = arena.allocate(ValueLayout.JAVA_INT, 1);

            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_filter_by_id2(
                     plist_id, filter_id, flags_segment, cd_nelmts_segment, cd_values_segment, namelen,
                     name_segment, filter_config_segment)) < 0) {
                h5libraryError();
            }
            cd_nelmts[0]     = cd_nelmts_segment.get(ValueLayout.JAVA_INT, 0);
            filter_config[0] = filter_config_segment.get(ValueLayout.JAVA_INT, 0);
            name[0]          = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pall_filters_avail query to verify that all the filters set
     *                      in the dataset creation property list are available currently.
     *
     * @param dcpl_id
     *            IN: Property list identifier.
     *
     * @return
     *            TRUE if all filters available
     *            FALSE if one or more filters not currently available.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Pall_filters_avail(long dcpl_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pall_filters_avail(dcpl_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal == 1;
    }

    /**
     * @ingroup JH5P
     *
     * H5Premove_filter deletes a filter from the dataset creation property list;
     *                  deletes all filters if filter is H5Z_FILTER_ALL
     *
     * @param obj_id
     *            IN: Property list identifier.
     * @param filter
     *            IN: Filter identifier.
     *
     * @return a non-negative value and the size of the user block; if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Premove_filter(long obj_id, int filter) throws HDF5LibraryException
    {
        if (filter < 0) {
            throw new HDF5FunctionArgumentException("Negative filter identifier");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Premove_filter(obj_id, filter);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_deflate sets the compression method for a dataset.
     *
     * @param plist
     *            IN: Identifier for the dataset creation property list.
     * @param level
     *            IN: Compression level.
     *
     * @return non-negative if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_deflate(long plist, int level) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (level < 0 || level > 9) {
            throw new HDF5FunctionArgumentException("Compression level must be between 0 and 9");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_deflate(plist, level);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fletcher32 sets Fletcher32 checksum of EDC for a dataset creation
     *                   property list or group creation property list.
     *
     * @param plist
     *            IN: Property list identifier.
     *
     * @return a non-negative value and the size of the user block; if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_fletcher32(long plist) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_fletcher32(plist);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    // /////// File creation property list (FCPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_userblock retrieves the size of a user block in a file creation property list.
     *
     * @param plist
     *            IN: Identifier for property list to query.
     * @param size
     *            OUT: Pointer to location to return user-block size.
     *
     * @return a non-negative value and the size of the user block; if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     **/
    public static int H5Pget_userblock(long plist, long[] size)
        throws HDF5LibraryException, NullPointerException
    {
        if (size == null || size.length < 1) {
            throw new NullPointerException("size is null or has less than 1 element");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_userblock(plist, size_segment)) < 0)
                h5libraryError();
            size[0] = size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_userblock sets the user block size of a file creation property list.
     *
     * @param plist
     *            IN: Identifier of property list to modify.
     * @param size
     *            IN: Size of the user-block in bytes.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_userblock(long plist, long size) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (size < 0) {
            throw new HDF5FunctionArgumentException("User block size must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_userblock(plist, size);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_sizes retrieves the size of the offsets and lengths used in an HDF5 file. This function is only
     * valid for file creation property lists.
     *
     * @param plist
     *            IN: Identifier of property list to query.
     * @param size
     *            OUT: the size of the offsets and length.
     *
     *            <pre>
     *      size[0] = sizeof_addr // offset size in bytes
     *      size[1] = sizeof_size // length size in bytes
     * </pre>
     * @return a non-negative value with the sizes initialized; if successful;
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     * @exception HDF5FunctionArgumentException
     *            size is invalid.
     **/
    public static int H5Pget_sizes(long plist, long[] size)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (size == null || size.length < 2) {
            throw new NullPointerException("size is null or has less than 2 elements");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment addr_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_sizes(plist, addr_segment, size_segment)) < 0)
                h5libraryError();
            size[0] = addr_segment.get(ValueLayout.JAVA_LONG, 0);
            size[1] = size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_sizes sets the byte size of the offsets and lengths used to address objects in an HDF5 file.
     *
     * @param plist
     *            IN: Identifier of property list to modify.
     * @param sizeof_addr
     *            IN: Size of an object offset in bytes.
     * @param sizeof_size
     *            IN: Size of an object length in bytes.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_sizes(long plist, int sizeof_addr, int sizeof_size) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (sizeof_addr <= 0 || sizeof_size <= 0) {
            throw new HDF5FunctionArgumentException("Sizes must be positive integers");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_sizes(plist, sizeof_addr, sizeof_size);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_sym_k retrieves the size of the symbol table B-tree 1/2 rank and the symbol table leaf node 1/2
     * size.
     *
     * @param plist
     *            IN: Property list to query.
     * @param size
     *            OUT: the symbol table's B-tree 1/2 rank and leaf node 1/2size.
     *
     *            <pre>
     *      size[0] = ik // the symbol table's B-tree 1/2 rank
     *      size[1] = lk // leaf node 1/2 size
     * </pre>
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     * @exception HDF5FunctionArgumentException
     *            size is invalid.
     **/
    public static int H5Pget_sym_k(long plist, int[] size)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (size == null || size.length < 2) {
            throw new NullPointerException("size is null or has less than 2 elements");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ik_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment lk_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_sym_k(plist, ik_segment, lk_segment)) < 0)
                h5libraryError();
            size[0] = ik_segment.get(ValueLayout.JAVA_INT, 0);
            size[1] = lk_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_sym_k sets the size of parameters used to control the symbol table nodes.
     *
     * @param plist
     *            IN: Identifier for property list to query.
     * @param ik
     *            IN: Symbol table tree rank.
     * @param lk
     *            IN: Symbol table node size.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_sym_k(long plist, int ik, int lk) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (ik <= 0 || lk <= 0) {
            throw new HDF5FunctionArgumentException("ik and lk must be positive integers");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_sym_k(plist, ik, lk);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_istore_k queries the 1/2 rank of an indexed storage B-tree.
     *
     * @param plist
     *            IN: Identifier of property list to query.
     * @param ik
     *            OUT: Pointer to location to return the chunked storage B-tree 1/2 rank.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            ik array is null.
     **/
    public static int H5Pget_istore_k(long plist, int[] ik) throws HDF5LibraryException, NullPointerException
    {
        if (ik == null || ik.length < 1) {
            throw new NullPointerException("ik is null or has less than 1 element");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ik_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_istore_k(plist, ik_segment)) < 0)
                h5libraryError();
            ik[0] = ik_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_istore_k sets the size of the parameter used to control the B-trees for indexing chunked
     * datasets.
     *
     * @param plist
     *            IN: Identifier of property list to query.
     * @param ik
     *            IN: 1/2 rank of chunked storage B-tree.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_istore_k(long plist, int ik) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (ik <= 0) {
            throw new HDF5FunctionArgumentException("ik must be a positive integer");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_istore_k(plist, ik);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_shared_mesg_nindexes retrieves number of shared object header message indexes in file creation
     * property list.
     *
     * @param fcpl_id
     *            IN: : File creation property list identifier
     *
     * @return nindexes, the number of shared object header message indexes available in files created with
     *         this property list
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_shared_mesg_nindexes(long fcpl_id) throws HDF5LibraryException
    {
        int retVal   = -1;
        int nindexes = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment nindexes_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal =
                     org.hdfgroup.javahdf5.hdf5_h.H5Pget_shared_mesg_nindexes(fcpl_id, nindexes_segment)) < 0)
                h5libraryError();
            nindexes = nindexes_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return nindexes;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_shared_mesg_nindexes sets the number of shared object header message indexes in the specified
     * file creation property list.
     *
     * @param plist_id
     *            IN: File creation property list
     * @param nindexes
     *            IN: Number of shared object header message indexes to be available in files created with
     *                this property list
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid value of nindexes
     *
     **/
    public static int H5Pset_shared_mesg_nindexes(long plist_id, int nindexes)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (nindexes < 0 || nindexes > 64) {
            throw new HDF5FunctionArgumentException("nindexes must be between 0 and 64");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_shared_mesg_nindexes(plist_id, nindexes);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_shared_mesg_index Retrieves the configuration settings for a shared message index.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     * @param index_num
     *            IN: Index being configured.
     * @param mesg_info
     *            The message type and minimum message size
     *
     *            <pre>
     *      mesg_info[0] =  Types of messages that may be stored in this index.
     *      mesg_info[1] =  Minimum message size.
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            mesg_info is null.
     * @exception HDF5FunctionArgumentException
     *            Invalid value of nindexes
     *
     **/
    public static int H5Pget_shared_mesg_index(long fcpl_id, int index_num, int[] mesg_info)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (mesg_info == null || mesg_info.length < 2) {
            throw new NullPointerException("mesg_info is null or has less than 2 elements");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment mesg_type_flags_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment min_mesg_size_segment   = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_shared_mesg_index(
                     fcpl_id, index_num, mesg_type_flags_segment, min_mesg_size_segment)) < 0) {
                h5libraryError();
            }
            mesg_info[0] = mesg_type_flags_segment.get(ValueLayout.JAVA_INT, 0);
            mesg_info[1] = min_mesg_size_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_shared_mesg_index Configures the specified shared object header message index
     *
     * @param fcpl_id
     *            IN: File creation property list identifier.
     * @param index_num
     *            IN: Index being configured.
     * @param mesg_type_flags
     *            IN: Types of messages that should be stored in this index.
     * @param min_mesg_size
     *            IN: Minimum message size.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid value of nindexes
     *
     **/
    public static int H5Pset_shared_mesg_index(long fcpl_id, int index_num, int mesg_type_flags,
                                               int min_mesg_size)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (index_num < 0 || index_num > 63) {
            throw new HDF5FunctionArgumentException("index_num must be between 0 and 63");
        }
        if (mesg_type_flags < 0) {
            throw new HDF5FunctionArgumentException("mesg_type_flags must be non-negative");
        }
        if (min_mesg_size < 0) {
            throw new HDF5FunctionArgumentException("min_mesg_size must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_shared_mesg_index(fcpl_id, index_num,
                                                                           mesg_type_flags, min_mesg_size);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_shared_mesg_phase_change retrieves shared object header message phase change information.
     *
     * @param fcpl_id
     *            IN: : File creation property list identifier
     * @param size
     *            The threshold values for storage of shared object header message indexes in a file.
     *
     *            <pre>
     *      size[0] =  Threshold above which storage of a shared object header message index shifts from list
     *      to B-tree size[1] =  Threshold below which storage of a shared object header message index reverts
     *      to list format
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     *
     **/
    public static int H5Pget_shared_mesg_phase_change(long fcpl_id, int[] size)
        throws HDF5LibraryException, NullPointerException
    {
        if (size == null || size.length < 2) {
            throw new NullPointerException("size is null or has less than 2 elements");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment max_list_segment  = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment min_btree_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_shared_mesg_phase_change(
                     fcpl_id, max_list_segment, min_btree_segment)) < 0) {
                h5libraryError();
            }
            size[0] = max_list_segment.get(ValueLayout.JAVA_INT, 0);
            size[1] = min_btree_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_shared_mesg_phase_change sets shared object header message storage phase change thresholds.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     * @param max_list
     *            IN: Threshold above which storage of a shared object header message index shifts from list
     *                to B-tree
     * @param min_btree
     *            IN: Threshold below which storage of a shared object header message index reverts to list
     *                format
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values of max_list and min_btree.
     *
     **/
    public static int H5Pset_shared_mesg_phase_change(long fcpl_id, int max_list, int min_btree)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (max_list < 0 || min_btree < 0) {
            throw new HDF5FunctionArgumentException("max_list and min_btree must be non-negative");
        }
        if (max_list < min_btree) {
            throw new HDF5FunctionArgumentException("max_list must be greater than or equal to min_btree");
        }

        int retVal =
            org.hdfgroup.javahdf5.hdf5_h.H5Pset_shared_mesg_phase_change(fcpl_id, max_list, min_btree);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_file_space_strategy sets the file space management strategy for the file associated with fcpl_id
     * to strategy. There are four strategies that applications can select and they are described in the
     * Parameters section.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     * @param strategy
     *            IN: The strategy for file space management.
     *                H5F_FSPACE_STRATEGY_FSM_AGGR
     *                        Mechanisms: free-space managers, aggregators, and virtual file drivers
     *                        This is the library default when not set.
     *                H5F_FSPACE_STRATEGY_PAGE
     *                        Mechanisms: free-space managers with embedded paged aggregation and virtual file
     *                                    drivers
     *                H5F_FSPACE_STRATEGY_AGGR
     *                        Mechanisms: aggregators and virtual file drivers
     *                H5F_FSPACE_STRATEGY_NONE
     *                        Mechanisms: virtual file drivers
     * @param persist
     *            IN: True to persist free-space.
     * @param threshold
     *            IN: The free-space section threshold. The library default is 1, which is to track all
     *                free-space sections. Passing a value of zero (0) indicates that the value of threshold
     *                is not to be modified.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values of max_list and min_btree.
     *
     **/
    public static void H5Pset_file_space_strategy(long fcpl_id, int strategy, boolean persist, long threshold)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (strategy < 0 || strategy > 3) {
            throw new HDF5FunctionArgumentException("Invalid strategy value");
        }

        int retVal =
            org.hdfgroup.javahdf5.hdf5_h.H5Pset_file_space_strategy(fcpl_id, strategy, persist, threshold);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_file_space_strategy provides the means for applications to manage the HDF5 file's file space
     * strategy for their specific needs.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     * @param persist
     *            IN/OUT: The current free-space persistence. NULL, persist not queried.
     * @param threshold
     *            IN/OUT: The current free-space section threshold. NULL, threshold not queried.
     *
     * @return the current free-space strategy.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values of max_list and min_btree.
     *
     **/
    public static int H5Pget_file_space_strategy(long fcpl_id, boolean[] persist, long[] threshold)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment strategy_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment persist_segment  = MemorySegment.NULL;
            if (persist != null && persist.length > 0)
                persist_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            MemorySegment threshold_segment = MemorySegment.NULL;
            if (threshold != null && threshold.length > 0)
                threshold_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_file_space_strategy(
                     fcpl_id, strategy_segment, persist_segment, threshold_segment)) < 0) {
                h5libraryError();
            }
            retVal = strategy_segment.get(ValueLayout.JAVA_INT, 0);
            if (persist != null && persist.length > 0)
                persist[0] = persist_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
            if (threshold != null && threshold.length > 0)
                threshold[0] = threshold_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_file_space_strategy_persist provides the means for applications to manage the HDF5 file's file
     * space strategy for their specific needs.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     *
     * @return the current free-space persistence.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values of max_list and min_btree.
     *
     **/
    public static boolean H5Pget_file_space_strategy_persist(long fcpl_id)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        boolean persist[] = {false};
        if (hdf.hdf5lib.H5.H5Pget_file_space_strategy(fcpl_id, persist, null) < 0) {
            h5libraryError();
        }
        return persist[0];
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_file_space_strategy_threshold provides the means for applications to manage the HDF5 file's file
     * space strategy for their specific needs.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     *
     * @return the current free-space section threshold.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values of max_list and min_btree.
     *
     **/
    public static long H5Pget_file_space_strategy_threshold(long fcpl_id)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        long threshold[] = {-1};
        if (hdf.hdf5lib.H5.H5Pget_file_space_strategy(fcpl_id, null, threshold) < 0) {
            h5libraryError();
        }
        return threshold[0];
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_file_space_page_size retrieves the file space page size for aggregating small metadata or raw
     * data.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     * @param page_size
     *            IN: the file space page size.
     *
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values of max_list and min_btree.
     *
     **/
    public static void H5Pset_file_space_page_size(long fcpl_id, long page_size)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_file_space_page_size(fcpl_id, page_size);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_file_space_page_size Sets the file space page size for paged aggregation.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     *
     * @return the current file space page size.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values of max_list and min_btree.
     *
     **/
    public static long H5Pget_file_space_page_size(long fcpl_id)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        long page_size = -1;
        int retVal     = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment page_size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_file_space_page_size(fcpl_id, page_size_segment) < 0)
                h5libraryError();
            page_size = page_size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return page_size;
    }

    // /////// File access property list (FAPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_alignment retrieves the current settings for alignment properties from a file access property
     * list.
     *
     * @param plist
     *            IN: Identifier of a file access property list.
     * @param alignment
     *            OUT: threshold value and alignment value.
     *
     *            <pre>
     *      alignment[0] = threshold // threshold value
     *      alignment[1] = alignment // alignment value
     * </pre>
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            alignment array is null.
     * @exception HDF5FunctionArgumentException
     *            alignment array is invalid.
     **/
    public static int H5Pget_alignment(long plist, long[] alignment)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (alignment == null || alignment.length < 2) {
            throw new NullPointerException("alignment is null or has less than 2 elements");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment threshold_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment alignment_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_alignment(plist, threshold_segment,
                                                                        alignment_segment)) < 0)
                h5libraryError();
            alignment[0] = threshold_segment.get(ValueLayout.JAVA_LONG, 0);
            alignment[1] = alignment_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_alignment sets the alignment properties of a file access property list so that any file object
     * &gt;= THRESHOLD bytes will be aligned on an address which is a multiple of ALIGNMENT.
     *
     * @param plist
     *            IN: Identifier for a file access property list.
     * @param threshold
     *            IN: Threshold value.
     * @param alignment
     *            IN: Alignment value.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_alignment(long plist, long threshold, long alignment) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (threshold < 0 || alignment <= 0)
            throw new HDF5FunctionArgumentException(
                "threshold must be non-negative and alignment must be positive");

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_alignment(plist, threshold, alignment);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_driver returns the identifier of the low-level file driver associated with the file access
     * property list or data transfer property list plid.
     *
     * @param plid
     *            IN: File access or data transfer property list identifier.
     * @return a valid low-level driver identifier if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     */
    public static long H5Pget_driver(long plid) throws HDF5LibraryException
    {
        long driver_id = org.hdfgroup.javahdf5.hdf5_h.H5Pget_driver(plid);
        if (driver_id < 0) {
            h5libraryError();
        }
        return driver_id;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_family_offset gets offset for family driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return the offset.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static long H5Pget_family_offset(long fapl_id) throws HDF5LibraryException
    {
        long offset = -1;
        int retVal  = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment offset_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_family_offset(fapl_id, offset_segment)) < 0)
                h5libraryError();
            offset = offset_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return offset;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_family_offset sets the offset for family driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param offset
     *            IN: the offset value
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_family_offset(long fapl_id, long offset) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_family_offset(fapl_id, offset);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * Retrieves the maximum possible number of elements in the meta data cache and the maximum possible
     * number of bytes and the RDCC_W0 value in the raw data chunk cache.
     *
     * @param plist
     *            IN: Identifier of the file access property list.
     * @param mdc_nelmts
     *            IN/OUT: No longer used, will be ignored.
     * @param rdcc_nelmts
     *            IN/OUT: Number of elements (objects) in the raw data chunk cache.
     * @param rdcc_nbytes
     *            IN/OUT: Total size of the raw data chunk cache, in bytes.
     * @param rdcc_w0
     *            IN/OUT: Preemption policy.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an array is null.
     **/
    public static int H5Pget_cache(long plist, int[] mdc_nelmts, long[] rdcc_nelmts, long[] rdcc_nbytes,
                                   double[] rdcc_w0) throws HDF5LibraryException, NullPointerException
    {
        if (rdcc_nelmts == null || rdcc_nbytes == null || rdcc_w0 == null || rdcc_nelmts.length < 1 ||
            rdcc_nbytes.length < 1 || rdcc_w0.length < 1) {
            throw new NullPointerException("One or more arrays are null or have insufficient length");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment mdc_nelmts_segment  = MemorySegment.NULL;
            MemorySegment rdcc_nelmts_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment rdcc_nbytes_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment rdcc_w0_segment     = arena.allocate(ValueLayout.JAVA_DOUBLE, 1);

            if ((retVal =
                     org.hdfgroup.javahdf5.hdf5_h.H5Pget_cache(plist, mdc_nelmts_segment, rdcc_nelmts_segment,
                                                               rdcc_nbytes_segment, rdcc_w0_segment)) < 0) {
                h5libraryError();
            }

            rdcc_nelmts[0] = rdcc_nelmts_segment.get(ValueLayout.JAVA_LONG, 0);
            rdcc_nbytes[0] = rdcc_nbytes_segment.get(ValueLayout.JAVA_LONG, 0);
            rdcc_w0[0]     = rdcc_w0_segment.get(ValueLayout.JAVA_DOUBLE, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_cache sets the number of elements (objects) in the meta data cache and the total number of bytes
     * in the raw data chunk cache.
     *
     * @param plist
     *            IN: Identifier of the file access property list.
     * @param mdc_nelmts
     *            IN: No longer used, will be ignored.
     * @param rdcc_nelmts
     *            IN: Number of elements (objects) in the raw data chunk cache.
     * @param rdcc_nbytes
     *            IN: Total size of the raw data chunk cache, in bytes.
     * @param rdcc_w0
     *            IN: Preemption policy.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_cache(long plist, int mdc_nelmts, long rdcc_nelmts, long rdcc_nbytes,
                                   double rdcc_w0) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (rdcc_nelmts < 0 || rdcc_nbytes < 0 || rdcc_w0 < 0.0) {
            throw new HDF5FunctionArgumentException(
                "rdcc_nelmts, rdcc_nbytes must be non-negative and rdcc_w0 must be non-negative");
        }

        int retVal =
            org.hdfgroup.javahdf5.hdf5_h.H5Pset_cache(plist, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_mdc_config gets the initial metadata cache configuration contained in a file access property
     * list. This configuration is used when the file is opened.
     *
     * @param plist_id
     *            IN: Identifier of the file access property list.
     *
     * @return A buffer(H5AC_cache_config_t) for the current metadata cache configuration information
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static hdf.hdf5lib.structs.H5AC_cache_config_t H5Pget_mdc_config(long plist_id)
        throws HDF5LibraryException
    {
        hdf.hdf5lib.structs.H5AC_cache_config_t config = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment config_segment = arena.allocate(org.hdfgroup.javahdf5.H5AC_cache_config_t.sizeof());
            org.hdfgroup.javahdf5.H5AC_cache_config_t.version(config_segment,
                                                              HDF5Constants.H5AC_CURR_CACHE_CONFIG_VERSION);

            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_mdc_config(plist_id, config_segment) < 0)
                h5libraryError();

            // Unpack the H5AC_cache_config_t from the MemorySegment
            config = new hdf.hdf5lib.structs.H5AC_cache_config_t(
                org.hdfgroup.javahdf5.H5AC_cache_config_t.version(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.rpt_fcn_enabled(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.open_trace_file(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.close_trace_file(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.trace_file_name(config_segment)
                    .getString(0, StandardCharsets.UTF_8),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.evictions_enabled(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.set_initial_size(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.initial_size(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.min_clean_fraction(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.max_size(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.min_size(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.epoch_length(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.incr_mode(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.lower_hr_threshold(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.increment(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.apply_max_increment(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.max_increment(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.flash_incr_mode(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.flash_multiple(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.flash_threshold(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.decr_mode(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.upper_hr_threshold(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.decrement(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.apply_max_decrement(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.max_decrement(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.epochs_before_eviction(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.apply_empty_reserve(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.empty_reserve(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.dirty_bytes_threshold(config_segment),
                org.hdfgroup.javahdf5.H5AC_cache_config_t.metadata_write_strategy(config_segment));
        }
        return config;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_mdc_config sets the initial metadata cache configuration contained in a file access property
     * list and loads it into the instance of H5AC_cache_config_t pointed to by the config_ptr parameter. This
     * configuration is used when the file is opened.
     *
     * @param plist_id
     *            IN: Identifier of the file access property list.
     * @param config_ptr
     *            IN: H5AC_cache_config_t, the initial metadata cache configuration.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            config_ptr is null.
     **/
    public static void H5Pset_mdc_config(long plist_id, hdf.hdf5lib.structs.H5AC_cache_config_t config_ptr)
        throws HDF5LibraryException, NullPointerException
    {
        if (config_ptr == null) {
            throw new NullPointerException("config_ptr is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment config_segment = arena.allocate(org.hdfgroup.javahdf5.H5AC_cache_config_t.sizeof());
            org.hdfgroup.javahdf5.H5AC_cache_config_t.version(config_segment, config_ptr.version);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.rpt_fcn_enabled(config_segment,
                                                                      config_ptr.rpt_fcn_enabled);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.open_trace_file(config_segment,
                                                                      config_ptr.open_trace_file);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.close_trace_file(config_segment,
                                                                       config_ptr.close_trace_file);
            MemorySegment trace_file_name_segment = arena.allocate(1025);
            trace_file_name_segment.fill((byte)0);
            MemorySegment.copy(config_ptr.trace_file_name.getBytes(), 0, trace_file_name_segment,
                               ValueLayout.JAVA_BYTE, 0L, config_ptr.trace_file_name.length());
            org.hdfgroup.javahdf5.H5AC_cache_config_t.trace_file_name(config_segment,
                                                                      trace_file_name_segment);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.evictions_enabled(config_segment,
                                                                        config_ptr.evictions_enabled);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.set_initial_size(config_segment,
                                                                       config_ptr.set_initial_size);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.initial_size(config_segment, config_ptr.initial_size);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.min_clean_fraction(config_segment,
                                                                         config_ptr.min_clean_fraction);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.max_size(config_segment, config_ptr.max_size);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.min_size(config_segment, config_ptr.min_size);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.epoch_length(config_segment,
                                                                   (int)config_ptr.epoch_length);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.incr_mode(config_segment, config_ptr.incr_mode);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.lower_hr_threshold(config_segment,
                                                                         config_ptr.lower_hr_threshold);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.increment(config_segment, config_ptr.increment);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.apply_max_increment(config_segment,
                                                                          config_ptr.apply_max_increment);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.max_increment(config_segment, config_ptr.max_increment);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.flash_incr_mode(config_segment,
                                                                      config_ptr.flash_incr_mode);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.flash_multiple(config_segment,
                                                                     config_ptr.flash_multiple);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.flash_threshold(config_segment,
                                                                      config_ptr.flash_threshold);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.decr_mode(config_segment, config_ptr.decr_mode);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.upper_hr_threshold(config_segment,
                                                                         config_ptr.upper_hr_threshold);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.decrement(config_segment, config_ptr.decrement);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.apply_max_decrement(config_segment,
                                                                          config_ptr.apply_max_decrement);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.max_decrement(config_segment, config_ptr.max_decrement);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.epochs_before_eviction(
                config_segment, config_ptr.epochs_before_eviction);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.apply_empty_reserve(config_segment,
                                                                          config_ptr.apply_empty_reserve);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.empty_reserve(config_segment, config_ptr.empty_reserve);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.dirty_bytes_threshold(config_segment,
                                                                            config_ptr.dirty_bytes_threshold);
            org.hdfgroup.javahdf5.H5AC_cache_config_t.metadata_write_strategy(
                config_segment, config_ptr.metadata_write_strategy);

            if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_mdc_config(plist_id, config_segment) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_gc_references Returns the current setting for the garbage collection references property from a
     * file access property list.
     *
     * @param fapl_id
     *            IN File access property list
     *
     * @return GC is on (true) or off (false)
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Pget_gc_references(long fapl_id) throws HDF5LibraryException
    {
        boolean gc_ref = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment gc_ref_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_gc_references(fapl_id, gc_ref_segment) < 0)
                h5libraryError();
            gc_ref = gc_ref_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
        return gc_ref;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_gc_references Sets the flag for garbage collecting references for the file. Default value for
     * garbage collecting references is off.
     *
     * @param fapl_id
     *            IN File access property list
     * @param gc_ref
     *            IN set GC on (true) or off (false)
     *
     * @return non-negative if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_gc_references(long fapl_id, boolean gc_ref) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_gc_references(fapl_id, gc_ref ? 1 : 0);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_fclose_degree returns the degree for the file close behavior for a file access
     * property list.
     *
     * @param fapl_id
     *            IN File access property list
     *
     * @return the degree for the file close behavior
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pget_fclose_degree(long fapl_id) throws HDF5LibraryException
    {
        int degree = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment degree_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_fclose_degree(fapl_id, degree_segment) < 0)
                h5libraryError();
            degree = degree_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return degree;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fclose_degree sets the degree for the file close behavior.
     *
     * @param fapl_id
     *            IN File access property list
     * @param degree
     *            IN the degree for the file close behavior
     *
     * @return non-negative if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_fclose_degree(long fapl_id, int degree) throws HDF5LibraryException
    {
        if (degree < 0 || degree > HDF5Constants.H5F_CLOSE_STRONG) {
            throw new HDF5FunctionArgumentException(
                "degree must be from H5F_CLOSE_DEFAULT to H5F_CLOSE_STRONG");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_fclose_degree(fapl_id, degree);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_meta_block_size the current metadata block size setting.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return the minimum size, in bytes, of metadata block allocations.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static long H5Pget_meta_block_size(long fapl_id) throws HDF5LibraryException
    {
        long size  = -1;
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_meta_block_size(fapl_id, size_segment)) < 0)
                h5libraryError();
            size = size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_meta_block_size sets the minimum metadata block size.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param size
     *            IN: Minimum size, in bytes, of metadata block allocations.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_meta_block_size(long fapl_id, long size) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_meta_block_size(fapl_id, size);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_sieve_buf_size retrieves the current settings for the data sieve buffer size
     * property from a file access property list.
     *
     * @param fapl_id
     *            IN: Identifier for property list to query.
     *
     * @return a non-negative value and the size of the user block; if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Pget_sieve_buf_size(long fapl_id) throws HDF5LibraryException
    {
        long size  = -1;
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_sieve_buf_size(fapl_id, size_segment)) < 0)
                h5libraryError();
            size = size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_sieve_buf_size Sets the maximum size of the data sieve buffer used for file
     *      drivers which are capable of using data sieving.  The data sieve
     *      buffer is used when performing I/O on datasets in the file.  Using a
     *      buffer which is large enough to hold several pieces of the dataset
     *      being read in for hyperslab selections boosts performance by quite a
     *      bit.
     * <p>
     *      The default value is set to 64KB, indicating that file I/O for raw data
     *      reads and writes will occur in at least 64KB blocks. Setting the value to 0
     *      with this function will turn off the data sieving
     *
     * @param fapl_id
     *            IN: Identifier of property list to modify.
     * @param size
     *            IN: maximum size of the data sieve buffer.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Pset_sieve_buf_size(long fapl_id, long size) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_sieve_buf_size(fapl_id, size);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_small_data_block_size retrieves the size of a block of small data in a file creation property
     * list.
     *
     * @param plist
     *            IN: Identifier for property list to query.
     *
     * @return a non-negative value and the size of the user block; if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Pget_small_data_block_size(long plist) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        long size  = -1;
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_small_data_block_size(plist, size_segment)) < 0)
                h5libraryError();
            size = size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_small_data_block_size reserves blocks of size bytes for the contiguous storage of the raw data
     * portion of small datasets.
     *
     * @param plist
     *            IN: Identifier of property list to modify.
     * @param size
     *            IN: Size of the blocks in bytes.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_small_data_block_size(long plist, long size) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (size < 0) {
            throw new HDF5FunctionArgumentException("size must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_small_data_block_size(plist, size);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_libver_bounds retrieves the lower and upper bounds on the HDF5 Library versions that indirectly
     * determine the object formats versions used when creating objects in the file.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param libver
     *            The earliest/latest version of the library that will be used for writing objects.
     *
     *            <pre>
     *      libver[0] =  The earliest version of the library that will be used for writing objects
     *      libver[1] =  The latest version of the library that will be used for writing objects.
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     *
     **/
    public static int H5Pget_libver_bounds(long fapl_id, int[] libver)
        throws HDF5LibraryException, NullPointerException
    {
        if (libver == null || libver.length < 2) {
            throw new NullPointerException("libver is null or has insufficient length");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment low_segment  = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment high_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_libver_bounds(fapl_id, low_segment,
                                                                            high_segment)) < 0)
                h5libraryError();
            libver[0] = low_segment.get(ValueLayout.JAVA_INT, 0);
            libver[1] = high_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_libver_bounds Sets bounds on library versions, and indirectly format versions, to be used when
     * creating objects
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param low
     *            IN: The earliest version of the library that will be used for writing objects
     * @param high
     *            IN: The latest version of the library that will be used for writing objects.
     *
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Argument is Illegal
     *
     **/
    public static int H5Pset_libver_bounds(long fapl_id, int low, int high)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (low < 0 || high < 0 || low > high) {
            throw new HDF5FunctionArgumentException("low and high must be non-negative and low <= high");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_libver_bounds(fapl_id, low, high);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_elink_file_cache_size retrieves the size of the external link open file cache.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return External link open file cache size in number of files.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_elink_file_cache_size(long fapl_id) throws HDF5LibraryException
    {
        int efc_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment efc_size_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_elink_file_cache_size(fapl_id, efc_size_segment) < 0)
                h5libraryError();
            efc_size = efc_size_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return efc_size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_elink_file_cache_size sets the number of files that can be held open in an external link open
     * file cache.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param efc_size
     *            IN: External link open file cache size in number of files.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_elink_file_cache_size(long fapl_id, int efc_size) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_elink_file_cache_size(fapl_id, efc_size);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_mdc_log_options sets metadata cache logging options.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param is_enabled
     *            IN: Whether logging is enabled.
     * @param location
     *            IN: Location of log in UTF-8/ASCII (file path/name) (On Windows, this must be ASCII).
     * @param start_on_access
     *            IN: Whether the logging begins as soon as the file is opened or created.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            location is null.
     *
     **/
    public static void H5Pset_mdc_log_options(long fapl_id, boolean is_enabled, String location,
                                              boolean start_on_access)
        throws HDF5LibraryException, NullPointerException
    {
        if (location == null) {
            throw new NullPointerException("location is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment location_segment = arena.allocateFrom(location);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_mdc_log_options(fapl_id, is_enabled, location_segment,
                                                                    start_on_access) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_mdc_log_options gets metadata cache logging options.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param mdc_log_options
     *         the options
     *             mdc_logging_options[0] = is_enabled, whether logging is enabled
     *             mdc_logging_options[1] = start_on_access, whether the logging begins as soon as the file is
     *                                      opened or created
     *
     * @return the location of log in UTF-8/ASCII (file path/name) (On Windows, this must be ASCII).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static String H5Pget_mdc_log_options(long fapl_id, boolean[] mdc_log_options)
        throws HDF5LibraryException
    {
        long buf_size = -1;
        if (mdc_log_options == null || mdc_log_options.length < 2) {
            throw new NullPointerException("mdc_log_options is null or has insufficient length");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_size_segment        = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment is_enabled_segment      = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            MemorySegment start_on_access_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_mdc_log_options(fapl_id, is_enabled_segment,
                                                                    MemorySegment.NULL, buf_size_segment,
                                                                    start_on_access_segment) < 0)
                h5libraryError();
            buf_size = buf_size_segment.get(ValueLayout.JAVA_LONG, 0) + 1; // +1 for null terminator;
        }

        String location = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_size_segment        = arena.allocate(ValueLayout.JAVA_LONG, buf_size);
            MemorySegment location_segment        = arena.allocate(buf_size);
            MemorySegment is_enabled_segment      = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            MemorySegment start_on_access_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);

            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_mdc_log_options(fapl_id, is_enabled_segment,
                                                                    location_segment, buf_size_segment,
                                                                    start_on_access_segment) < 0)
                h5libraryError();

            mdc_log_options[0] = is_enabled_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
            mdc_log_options[1] = start_on_access_segment.get(ValueLayout.JAVA_BOOLEAN, 0);

            location = location_segment.getString(0, StandardCharsets.UTF_8);
        }
        return location;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_metadata_read_attempts retrieves the number of read attempts that is set in the file access
     * property list plist_id.
     *
     * @param plist_id
     *            IN: File access property list identifier
     *
     * @return The number of read attempts.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_metadata_read_attempts(long plist_id) throws HDF5LibraryException
    {
        int attempts = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment attempts_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_metadata_read_attempts(plist_id, attempts_segment) < 0)
                h5libraryError();
            attempts = attempts_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return attempts;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_metadata_read_attempts sets the number of reads that the library will try when reading
     * checksummed metadata in an HDF5 file opened with SWMR access. When reading such metadata, the library
     * will compare the checksum computed for the metadata just read with the checksum stored within the piece
     * of checksum. When performing SWMR operations on a file, the checksum check might fail when the library
     * reads data on a system that is not atomic. To remedy such situations, the library will repeatedly read
     * the piece of metadata until the check passes or finally fails the read when the allowed number of
     * attempts is reached.
     *
     * @param plist_id
     *            IN: File access property list identifier
     * @param attempts
     *            IN: The number of read attempts which is a value greater than 0.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_metadata_read_attempts(long plist_id, int attempts) throws HDF5LibraryException
    {
        if (attempts <= 0) {
            throw new HDF5FunctionArgumentException("attempts must be greater than 0");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_metadata_read_attempts(plist_id, attempts);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_evict_on_close retrieves the file access property list setting that determines whether an HDF5
     * object will be evicted from the library's metadata cache when it is closed.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return indication if the object will be evicted on close.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static boolean H5Pget_evict_on_close(long fapl_id) throws HDF5LibraryException
    {
        boolean evict_on_close = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment evict_on_close_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_evict_on_close(fapl_id, evict_on_close_segment) < 0)
                h5libraryError();
            evict_on_close = evict_on_close_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
        return evict_on_close;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_evict_on_close controls the library's behavior of evicting metadata associated with a closed
     * object.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param evict_on_close
     *            IN: Whether the HDF5 object should be evicted on close.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_evict_on_close(long fapl_id, boolean evict_on_close) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_evict_on_close(fapl_id, evict_on_close);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_use_file_locking retrieves whether we are using file locking.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return indication if file locking is used.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static boolean H5Pget_use_file_locking(long fapl_id) throws HDF5LibraryException
    {
        boolean use_file_locking = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment use_file_locking_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            MemorySegment unused_segment           = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_file_locking(fapl_id, use_file_locking_segment,
                                                                 unused_segment) < 0)
                h5libraryError();
            use_file_locking = use_file_locking_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
        return use_file_locking;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_use_file_locking retrieves whether we ignore file locks when they are disabled.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return indication if file locking is ignored.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static boolean H5Pget_ignore_disabled_file_locking(long fapl_id) throws HDF5LibraryException
    {
        boolean ignore_when_disabled = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ignore_when_disabled_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            MemorySegment unused_segment               = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_file_locking(fapl_id, unused_segment,
                                                                 ignore_when_disabled_segment) < 0)
                h5libraryError();
            ignore_when_disabled = ignore_when_disabled_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
        return ignore_when_disabled;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_file_locking sets parameters related to file locking.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @param use_file_locking
     *            IN: Whether the library will use file locking when opening files (mainly for SWMR
     *semantics).
     *
     * @param ignore_when_disabled
     *            IN: Whether file locking will be ignored when disabled on a file system (useful for Lustre).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_file_locking(long fapl_id, boolean use_file_locking,
                                           boolean ignore_when_disabled) throws HDF5LibraryException
    {
        int retVal =
            org.hdfgroup.javahdf5.hdf5_h.H5Pset_file_locking(fapl_id, use_file_locking, ignore_when_disabled);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    //  /////  unimplemented /////
    // herr_t H5Pset_vol(hid_t plist_id, hid_t new_vol_id, const void *new_vol_info);
    // herr_t H5Pget_vol_id(hid_t plist_id, hid_t *vol_id);
    // herr_t H5Pget_vol_info(hid_t plist_id, void **vol_info);

    // Dataset creation property list (DCPL) routines //

    /**
     * @ingroup JH5P
     *
     * H5Pget_layout returns the layout of the raw data for a dataset.
     *
     * @param plist
     *            IN: Identifier for property list to query.
     *
     * @return the layout type of a dataset creation property list if successful. Otherwise returns
     *         H5D_LAYOUT_ERROR (-1).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pget_layout(long plist) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int layout = org.hdfgroup.javahdf5.hdf5_h.H5Pget_layout(plist);
        if (layout < 0) {
            h5libraryError();
        }
        return layout;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_layout sets the type of storage used store the raw data for a dataset.
     *
     * @param plist
     *            IN: Identifier of property list to query.
     * @param layout
     *            IN: Type of storage layout for raw data.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_layout(long plist, int layout) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (layout < 0) {
            throw new HDF5FunctionArgumentException("Invalid layout type specified");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_layout(plist, layout);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_chunk retrieves the size of chunks for the raw data of a chunked layout dataset.
     *
     * @param plist
     *            IN: Identifier of property list to query.
     * @param max_ndims
     *            IN: Size of the dims array.
     * @param dims
     *            OUT: Array to store the chunk dimensions.
     *
     * @return chunk dimensionality successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            dims array is null.
     * @exception HDF5FunctionArgumentException
     *            max_ndims &lt;=0
     **/
    public static int H5Pget_chunk(long plist, int max_ndims, long[] dims)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (dims == null || dims.length < max_ndims) {
            throw new NullPointerException("dims is null or has insufficient length");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (max_ndims <= 0) {
            throw new HDF5FunctionArgumentException("max_ndims must be greater than 0");
        }
        int ndims = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dims_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
            ndims = org.hdfgroup.javahdf5.hdf5_h.H5Pget_chunk(plist, max_ndims, dims_segment);
            if (ndims < 0) {
                h5libraryError();
            }
            for (int i = 0; i < ndims; i++) {
                dims[i] = dims_segment.get(ValueLayout.JAVA_LONG, i * Long.BYTES);
            }
        }
        return ndims;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_chunk sets the size of the chunks used to store a chunked layout dataset.
     *
     * @param plist
     *            IN: Identifier for property list to query.
     * @param ndims
     *            IN: The number of dimensions of each chunk.
     * @param dim
     *            IN: An array containing the size of each chunk.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            dims array is null.
     * @exception HDF5FunctionArgumentException
     *            dims &lt;=0
     **/
    public static int H5Pset_chunk(long plist, int ndims, byte[] dim)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (dim == null || dim.length < ndims * Long.BYTES) {
            throw new NullPointerException("dim is null or has insufficient length");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (ndims <= 0) {
            throw new HDF5FunctionArgumentException("ndims must be greater than 0");
        }
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dim_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, dim);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_chunk(plist, ndims, dim_segment)) < 0)
                h5libraryError();
        }

        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_chunk sets the size of the chunks used to store a chunked layout dataset.
     *
     * @param plist
     *            IN: Identifier for property list to query.
     * @param ndims
     *            IN: The number of dimensions of each chunk.
     * @param dim
     *            IN: An array containing the size of each chunk.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            dims array is null.
     * @exception HDF5FunctionArgumentException
     *            dims &lt;=0
     **/
    public static int H5Pset_chunk(long plist, int ndims, long[] dim)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (dim == null) {
            throw new NullPointerException("dim is null");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dim_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, dim);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_chunk(plist, ndims, dim_segment)) < 0)
                h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_virtual maps elements of the virtual dataset (VDS) described by the
     * virtual dataspace identifier vspace_id to the elements of the source dataset
     * described by the source dataset dataspace identifier src_space_id. The source
     * dataset is identified by the name of the file where it is located, src_file_name,
     * and the name of the dataset, src_dset_name.
     *
     * @param dcpl_id
     *            IN: The identifier of the dataset creation property list that will be used when creating the
     *                virtual dataset.
     * @param vspace_id
     *            IN: The dataspace identifier with the selection within the virtual dataset applied, possibly
     *                an unlimited selection.
     * @param src_file_name
     *            IN: The name of the HDF5 file where the source dataset is located. The file might not exist
     *                yet. The name can be specified using a C-style printf statement.
     * @param src_dset_name
     *            IN: The path to the HDF5 dataset in the file specified by src_file_name. The dataset might
     *                not exist yet. The dataset name can be specified using a C-style printf statement.
     * @param src_space_id
     *            IN: The source dataset dataspace identifier with a selection applied, possibly an unlimited
     *                selection.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an name string is null.
     * @exception HDF5FunctionArgumentException
     *            an id is &lt;=0
     **/
    public static void H5Pset_virtual(long dcpl_id, long vspace_id, String src_file_name,
                                      String src_dset_name, long src_space_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (src_file_name == null || src_dset_name == null) {
            throw new NullPointerException("src_file_name or src_dset_name is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment src_file_name_segment = arena.allocateFrom(src_file_name);
            MemorySegment src_dset_name_segment = arena.allocateFrom(src_dset_name);

            if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_virtual(dcpl_id, vspace_id, src_file_name_segment,
                                                            src_dset_name_segment, src_space_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_count gets the number of mappings for a virtual dataset that has the creation property
     * list specified by dcpl_id.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     *
     * @return a non-negative number of mappings if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            An id is &lt;=0
     **/
    public static long H5Pget_virtual_count(long dcpl_id)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        long count = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment count_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_count(dcpl_id, count_segment) < 0)
                h5libraryError();
            count = count_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return count;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_vspace takes the dataset creation property list for the virtual dataset, dcpl_id, and
     * the mapping index, index, and returns a dataspace identifier for the selection within the virtual
     * dataset used in the mapping.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     * @param index
     *            IN: Mapping index.
     *
     * @return a valid dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            An id is &lt;=0
     **/
    public static long H5Pget_virtual_vspace(long dcpl_id, long index)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (index < 0) {
            throw new HDF5FunctionArgumentException("index must be non-negative");
        }

        long vspace_id = org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_vspace(dcpl_id, index);
        if (vspace_id < 0)
            h5libraryError();

        return vspace_id;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_srcspace takes the dataset creation property list for the virtual dataset, dcpl_id, and
     * the mapping index, index, and returns a dataspace identifier for the selection within the source
     * dataset used in the mapping.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     * @param index
     *            IN: Mapping index.
     *
     * @return a valid dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            An id is &lt;=0
     **/
    public static long H5Pget_virtual_srcspace(long dcpl_id, long index)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (index < 0) {
            throw new HDF5FunctionArgumentException("index must be non-negative");
        }

        long src_space_id = org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_srcspace(dcpl_id, index);
        if (src_space_id < 0)
            h5libraryError();

        return src_space_id;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_filename takes the dataset creation property list for the virtual dataset, dcpl_id, the
     * mapping index, index, the size of the filename for a source dataset, size, and retrieves the name of
     * the file for a source dataset used in the mapping.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     * @param index
     *            IN: Mapping index.
     *
     * @return the name of the file containing the source dataset if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            An id is &lt;=0
     **/
    public static String H5Pget_virtual_filename(long dcpl_id, long index)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (index < 0) {
            throw new HDF5FunctionArgumentException("index must be non-negative");
        }

        long buf_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_filename(dcpl_id, index,
                                                                                 MemorySegment.NULL, 0)) < 0)
                h5libraryError();
        }

        String filename = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment filename_segment = arena.allocate(buf_size + 1); // +1 for null terminator
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_filename(dcpl_id, index, filename_segment,
                                                                     buf_size + 1) < 0)
                h5libraryError();
            filename = filename_segment.getString(0, StandardCharsets.UTF_8);
        }
        return filename;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_dsetname takes the dataset creation property list for the virtual dataset, dcpl_id, the
     * mapping index, index, the size of the dataset name for a source dataset, size, and retrieves the name
     * of the source dataset used in the mapping.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     * @param index
     *            IN: Mapping index.
     *
     * @return the name of the source dataset if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            An id is &lt;=0
     **/
    public static String H5Pget_virtual_dsetname(long dcpl_id, long index)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (index < 0) {
            throw new HDF5FunctionArgumentException("index must be non-negative");
        }

        long buf_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_dsetname(dcpl_id, index,
                                                                                 MemorySegment.NULL, 0)) < 0)
                h5libraryError();
        }

        String dset_name = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dset_name_segment = arena.allocate(buf_size + 1); // +1 for null terminator
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_dsetname(dcpl_id, index, dset_name_segment,
                                                                     buf_size + 1) < 0)
                h5libraryError();
            dset_name = dset_name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return dset_name;
    }

    //    /////  unimplemented /////
    //    /*
    //     * H5Pget_vds_file_cache_size retrieves the size of the vds link open file cache.
    //     *
    //     * @param fapl_id
    //     *            IN: File access property list identifier
    //     *
    //     * @return VDS link open file cache size in number of files.
    //     *
    //     * @exception HDF5LibraryException
    //     *            Error from the HDF5 Library.
    //     *
    //     **/
    //    public  static  int H5Pget_vds_file_cache_size(long fapl_id) throws
    //    HDF5LibraryException {}
    //
    //    /*
    //     * H5Pset_vds_file_cache_size sets the number of files that can be held open in an vds link open
    //     * file cache.
    //     *
    //     * @param fapl_id
    //     *            IN: File access property list identifier
    //     * @param efc_size
    //     *            IN: VDS link open file cache size in number of files.
    //     *
    //     * @exception HDF5LibraryException
    //     *            Error from the HDF5 Library.
    //     *
    //     **/
    //    public  static  void H5Pset_vds_file_cache_size(long fapl_id, int efc_size)
    //            throws HDF5LibraryException {}

    /**
     * @ingroup JH5P
     *
     * H5Pget_external returns information about an external file.
     *
     * @param plist
     *            IN: Identifier of a dataset creation property list.
     * @param idx
     *            IN: External file index.
     * @param name_size
     *            IN: Maximum length of name array.
     * @param name
     *            OUT: Name of the external file.
     * @param size
     *            OUT: the offset value and the size of the external file data.
     *
     * <pre>
     *    size[0] = offset // a location to return an offset value
     *    size[1] = size // a location to return the size of the external file data.
     * </pre>
     *
     * @return a non-negative value if successful
     *
     * @exception ArrayIndexOutOfBoundsException
     *            Fatal error on Copyback
     * @exception ArrayStoreException
     *            Fatal error on Copyback
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name or size is null.
     * @exception HDF5FunctionArgumentException
     *            name_size &lt;= 0 .
     *
     **/
    public static int H5Pget_external(long plist, int idx, long name_size, String[] name, long[] size)
        throws ArrayIndexOutOfBoundsException, ArrayStoreException, HDF5LibraryException,
               NullPointerException, HDF5FunctionArgumentException
    {
        if (size == null || size.length < 2) {
            throw new NullPointerException("name or size is null or has insufficient length");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (idx < 0) {
            throw new HDF5FunctionArgumentException("idx must be non-negative");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment;
            if (name == null || name.length < 1 || name_size <= 0) {
                name_segment = null;
            }
            else
                name_segment = arena.allocate(ValueLayout.JAVA_CHAR, name_size);
            MemorySegment offset_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment size_segment   = arena.allocate(ValueLayout.JAVA_LONG, 1);

            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_external(plist, idx, name_size, name_segment,
                                                                  offset_segment, size_segment);
            if (retVal < 0) {
                h5libraryError();
            }

            // Copy the external file name to the provided array
            if (name != null && name.length > 0 && name_size > 0)
                name[0] = name_segment.getString(0, StandardCharsets.UTF_8);

            // Copy the offset and size values to the provided array
            size[0] = offset_segment.get(ValueLayout.JAVA_LONG, 0);
            size[1] = size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_external adds an external file to the list of external files.
     *
     * @param plist
     *            IN: Identifier of a dataset creation property list.
     * @param name
     *            IN: Name of an external file.
     * @param offset
     *            IN: Offset, in bytes, from the beginning of the file to the location in the file where the
     *                data starts.
     * @param size
     *            IN: Number of bytes reserved in the file for the data.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Pset_external(long plist, String name, long offset, long size)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);

            int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_external(plist, name_segment, offset, size);
            if (retVal < 0) {
                h5libraryError();
            }
            return retVal;
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_external_count returns the number of external files for the specified dataset.
     *
     * @param plist
     *            IN: Identifier of a dataset creation property list.
     *
     * @return the number of external files if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pget_external_count(long plist) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int count = org.hdfgroup.javahdf5.hdf5_h.H5Pget_external_count(plist);
        if (count < 0) {
            h5libraryError();
        }
        return count;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_szip Sets up the use of the szip filter.
     *
     * @param plist
     *            IN: Dataset creation property list identifier.
     * @param options_mask
     *            IN: Bit vector specifying certain general properties of the filter.
     * @param pixels_per_block
     *            IN: Number of pixels in blocks
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_szip(long plist, int options_mask, int pixels_per_block)
        throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (options_mask < 0 || pixels_per_block < 0) {
            throw new HDF5FunctionArgumentException("options_mask or pixels_per_block must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_szip(plist, options_mask, pixels_per_block);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_shuffle Sets up the use of the shuffle filter.
     *
     * @param plist_id
     *            IN: Dataset creation property list identifier.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_shuffle(long plist_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_shuffle(plist_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_nbit Sets up the use of the N-Bit filter.
     *
     * @param plist_id
     *            IN: Dataset creation property list identifier.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_nbit(long plist_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_nbit(plist_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_scaleoffset sets the Scale-Offset filter for a dataset.
     *
     * @param plist_id
     *            IN: Dataset creation property list identifier.
     * @param scale_type
     *            IN: Flag indicating compression method.
     * @param scale_factor
     *            IN: Parameter related to scale.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid arguments
     *
     **/
    public static int H5Pset_scaleoffset(long plist_id, int scale_type, int scale_factor)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (scale_type < 0 || scale_factor < 0) {
            throw new HDF5FunctionArgumentException("scale_type or scale_factor must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_scaleoffset(plist_id, scale_type, scale_factor);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_fill_value queries the fill value property of a dataset creation property list.
     *
     * @param plist_id
     *            IN: Property list identifier.
     * @param type_id
     *            IN: The datatype identifier of value.
     * @param value
     *            IN: The fill value.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Error converting data array.
     * @exception NullPointerException
     *            value is null.
     **/
    public static int H5Pget_fill_value(long plist_id, long type_id, byte[] value)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (value == null) {
            throw new NullPointerException("value is null");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment value_segment = arena.allocate(ValueLayout.JAVA_BYTE, value.length);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Pget_fill_value(plist_id, type_id, value_segment)) <
                0)
                h5libraryError();
            MemorySegment.copy(value_segment, ValueLayout.JAVA_BYTE, 0L, value, 0, value.length);
        }

        return status;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_fill_value queries the fill value property of a dataset creation property list.
     *
     * @param plist_id
     *            IN: Property list identifier.
     * @param type_id
     *            IN: The datatype identifier of value.
     * @param obj
     *            IN: The fill value.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Error converting data array.
     **/
    public static int H5Pget_fill_value(long plist_id, long type_id, Object obj) throws HDF5Exception
    {
        HDFArray theArray = new HDFArray(obj);
        byte[] buf        = theArray.emptyBytes();

        int status = hdf.hdf5lib.H5.H5Pget_fill_value(plist_id, type_id, buf);
        if (status >= 0)
            obj = theArray.arrayify(buf);

        return status;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fill_value sets the fill value for a dataset creation property list.
     *
     * @param plist_id
     *            IN: Property list identifier.
     * @param type_id
     *            IN: The datatype identifier of value.
     * @param value
     *            IN: The fill value.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error converting data array
     * @exception NullPointerException
     *            value is null.
     **/
    public static int H5Pset_fill_value(long plist_id, long type_id, byte[] value)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (value == null) {
            throw new NullPointerException("value is null");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment value_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, value);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_fill_value(plist_id, type_id, value_segment)) <
                0)
                h5libraryError();
        }

        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fill_value sets the fill value for a dataset creation property list.
     *
     * @param plist_id
     *            IN: Property list identifier.
     * @param type_id
     *            IN: The datatype identifier of value.
     * @param obj
     *            IN: The fill value.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Error converting data array
     **/
    public static int H5Pset_fill_value(long plist_id, long type_id, Object obj) throws HDF5Exception
    {
        HDFArray theArray = new HDFArray(obj);
        byte[] buf        = theArray.byteify();

        // TODO: Add Arena support for buf
        int retVal = hdf.hdf5lib.H5.H5Pset_fill_value(plist_id, type_id, buf);

        buf      = null;
        theArray = null;
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fill_value checks if the fill value is defined for a dataset creation property list.
     *
     * @param plist_id
     *            IN: Property list identifier.
     * @param status
     *            IN: The fill value setting:
     *                H5D_FILL_VALUE_UNDEFINED
     *                H5D_FILL_VALUE_DEFAULT
     *                H5D_FILL_VALUE_USER_DEFINED
     *                H5D_FILL_VALUE_ERROR
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Error converting data array
     * @exception NullPointerException
     *            status is null.
     **/
    public static int H5Pfill_value_defined(long plist_id, int[] status)
        throws HDF5LibraryException, NullPointerException
    {
        if (status == null || status.length < 1) {
            throw new NullPointerException("status is null or has insufficient length");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment status_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pfill_value_defined(plist_id, status_segment)) < 0)
                h5libraryError();
            status[0] = status_segment.get(ValueLayout.JAVA_INT, 0);
        }

        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_alloc_time Gets space allocation time for dataset during creation.
     *
     * @param plist_id
     *            IN: Dataset creation property list identifier.
     * @param alloc_time
     *            OUT: allocation time.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            alloc_time is null.
     *
     **/
    public static int H5Pget_alloc_time(long plist_id, int[] alloc_time)
        throws HDF5LibraryException, NullPointerException
    {
        if (alloc_time == null || alloc_time.length < 1) {
            throw new NullPointerException("alloc_time is null or has insufficient length");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment alloc_time_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_alloc_time(plist_id, alloc_time_segment)) < 0)
                h5libraryError();
            alloc_time[0] = alloc_time_segment.get(ValueLayout.JAVA_INT, 0);
        }

        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_alloc_time Sets space allocation time for dataset during creation.
     *
     * @param plist_id
     *            IN: Dataset creation property list identifier.
     * @param alloc_time
     *            IN: allocation time.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_alloc_time(long plist_id, int alloc_time) throws HDF5LibraryException
    {
        if (alloc_time < 0) {
            throw new HDF5FunctionArgumentException("alloc_time must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_alloc_time(plist_id, alloc_time);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fill_time Gets fill value writing time.
     *
     * @param plist_id
     *            IN: Dataset creation property list identifier.
     * @param fill_time
     *            OUT: fill time.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            fill_time is null.
     *
     **/
    public static int H5Pget_fill_time(long plist_id, int[] fill_time)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (fill_time == null || fill_time.length < 1) {
            throw new NullPointerException("fill_time is null or has insufficient length");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fill_time_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_fill_time(plist_id, fill_time_segment)) < 0)
                h5libraryError();
            fill_time[0] = fill_time_segment.get(ValueLayout.JAVA_INT, 0);
        }

        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fill_time Sets the fill value writing time.
     *
     * @param plist_id
     *            IN: Dataset creation property list identifier.
     * @param fill_time
     *            IN: fill time.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fill_time(long plist_id, int fill_time) throws HDF5LibraryException
    {
        if (fill_time < 0) {
            throw new HDF5FunctionArgumentException("fill_time must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_fill_time(plist_id, fill_time);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_chunk_opts Sets the edge chunk option in a dataset creation property list.
     *
     * @param dcpl_id
     *            IN: Dataset creation property list identifier
     * @param opts
     *            IN: Edge chunk option flag. Valid values are:
     *                H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS - filters are not applied to partial edge chunks.
     *                0 - Disables option; partial edge chunks will be compressed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library
     **/
    public static void H5Pset_chunk_opts(long dcpl_id, int opts) throws HDF5LibraryException
    {
        if (opts < 0) {
            throw new HDF5FunctionArgumentException("opts must be non-negative");
        }

        if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_chunk_opts(dcpl_id, opts) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_chunk_opts retrieves the edge chunk option setting stored in the dataset creation property list
     *
     * @param dcpl_id
     *            IN: Dataset creation property list

     * @return The edge chunk option setting.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library
     *
    */
    public static int H5Pget_chunk_opts(long dcpl_id) throws HDF5LibraryException
    {
        int opts = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment opts_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_chunk_opts(dcpl_id, opts_segment) < 0)
                h5libraryError();
            opts = opts_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return opts;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_dset_no_attrs_hint accesses the flag for whether or not datasets created by the given dcpl
     * will be created with a "minimized" object header.
     *
     * @param dcpl_id
     *            IN: Dataset creation property list
     *
     * @return true if the given dcpl is set to create minimized dataset object headers, false if not.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Pget_dset_no_attrs_hint(long dcpl_id) throws HDF5LibraryException
    {
        boolean minimize = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment minimize_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_dset_no_attrs_hint(dcpl_id, minimize_segment) < 0)
                h5libraryError();
            minimize = minimize_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
        return minimize;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_dset_no_attrs_hint sets the dcpl to minimize (or explicitly to not minimized) dataset object
     * headers upon creation.
     *
     * @param dcpl_id
     *            IN: Dataset creation property list
     *
     * @param minimize
     *            IN: the minimize hint setting
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Pset_dset_no_attrs_hint(long dcpl_id, boolean minimize) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_dset_no_attrs_hint(dcpl_id, minimize) < 0) {
            h5libraryError();
        }
    }

    // /////// Dataset access property list (DAPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * Retrieves the maximum possible number of elements in the meta data cache and the maximum possible
     * number of bytes and the RDCC_W0 value in the raw data chunk cache on a per-datset basis.
     *
     * @param dapl_id
     *            IN: Identifier of the dataset access property list.
     * @param rdcc_nslots
     *            IN/OUT: Number of elements (objects) in the raw data chunk cache.
     * @param rdcc_nbytes
     *            IN/OUT: Total size of the raw data chunk cache, in bytes.
     * @param rdcc_w0
     *            IN/OUT: Preemption policy.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an array is null.
     **/
    public static void H5Pget_chunk_cache(long dapl_id, long[] rdcc_nslots, long[] rdcc_nbytes,
                                          double[] rdcc_w0) throws HDF5LibraryException, NullPointerException
    {
        if (rdcc_nslots == null || rdcc_nbytes == null || rdcc_w0 == null || rdcc_nslots.length < 1 ||
            rdcc_nbytes.length < 1 || rdcc_w0.length < 1) {
            throw new NullPointerException("one or more arrays are null or have insufficient length");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment nslots_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment nbytes_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment w0_segment     = arena.allocate(ValueLayout.JAVA_DOUBLE, 1);

            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_chunk_cache(dapl_id, nslots_segment, nbytes_segment,
                                                                w0_segment) < 0)
                h5libraryError();

            rdcc_nslots[0] = nslots_segment.get(ValueLayout.JAVA_LONG, 0);
            rdcc_nbytes[0] = nbytes_segment.get(ValueLayout.JAVA_LONG, 0);
            rdcc_w0[0]     = w0_segment.get(ValueLayout.JAVA_DOUBLE, 0);
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_chunk_cache sets the number of elements (objects) in the meta data cache and the total number of
     * bytes in the raw data chunk cache on a per-datset basis.
     *
     * @param dapl_id
     *            IN: Identifier of the dataset access property list.
     * @param rdcc_nslots
     *            IN: Number of elements (objects) in the raw data chunk cache.
     * @param rdcc_nbytes
     *            IN: Total size of the raw data chunk cache, in bytes.
     * @param rdcc_w0
     *            IN: Preemption policy.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Pset_chunk_cache(long dapl_id, long rdcc_nslots, long rdcc_nbytes, double rdcc_w0)
        throws HDF5LibraryException
    {
        if (rdcc_nslots < 0 || rdcc_nbytes < 0 || rdcc_w0 < 0.0) {
            throw new HDF5FunctionArgumentException(
                "rdcc_nslots, rdcc_nbytes or rdcc_w0 must be non-negative");
        }

        int retVal =
            org.hdfgroup.javahdf5.hdf5_h.H5Pset_chunk_cache(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_virtual_view takes the access property list for the virtual dataset, dapl_id, and the flag,
     * view, and sets the VDS view according to the flag value.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier for the virtual dataset
     * @param view
     *            IN: Flag specifying the extent of the data to be included in the view.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library
     **/
    public static void H5Pset_virtual_view(long dapl_id, int view) throws HDF5LibraryException
    {
        if (view < 0) {
            throw new HDF5FunctionArgumentException("view must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_virtual_view(dapl_id, view);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_view takes the virtual dataset access property list, dapl_id, and retrieves the flag,
     * view, set by the H5Pset_virtual_view call.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier for the virtual dataset

     * @return The flag specifying the view of the virtual dataset.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library
     *
    */
    public static int H5Pget_virtual_view(long dapl_id) throws HDF5LibraryException
    {
        int view = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment view_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_view(dapl_id, view_segment) < 0)
                h5libraryError();
            view = view_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return view;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_virtual_printf_gap sets the access property list for the virtual dataset, dapl_id, to instruct
     * the library to stop looking for the mapped data stored in the files and/or datasets with the
     * printf-style names after not finding gap_size files and/or datasets. The found source files and
     * datasets will determine the extent of the unlimited virtual dataset with the printf-style mappings.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier for the virtual dataset
     * @param gap_size
     *            IN: Maximum number of files and/or datasets allowed to be missing for determining
     *                the extent of an unlimited virtual dataset with printf-style mappings.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library
     **/
    public static void H5Pset_virtual_printf_gap(long dapl_id, long gap_size) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_virtual_printf_gap(dapl_id, gap_size) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_printf_gap returns the maximum number of missing printf-style files and/or datasets for
     * determining the extent of an unlimited virtual dataaset, gap_size, using the access property list for
     * the virtual dataset, dapl_id.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier for the virtual dataset

     * @return Maximum number of files and/or datasets allowed to be missing for determining
     *            the extent of an unlimited virtual dataset with printf-style mappings.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library
     *
    */
    public static long H5Pget_virtual_printf_gap(long dapl_id) throws HDF5LibraryException
    {
        long gap_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment gap_size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_printf_gap(dapl_id, gap_size_segment) < 0)
                h5libraryError();
            gap_size = gap_size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return gap_size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_prefix Retrieves prefix applied to virtual file paths.
     *
     * @param dapl_id
     *            IN: Link access property list identifier
     *
     * @return the prefix to be applied to virtual file paths.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static String H5Pget_virtual_prefix(long dapl_id) throws HDF5LibraryException
    {
        long buf_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((buf_size =
                     org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_prefix(dapl_id, MemorySegment.NULL, 0)) < 0)
                h5libraryError();
        }

        String prefix = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment prefix_segment = arena.allocate(buf_size + 1); // +1 for null terminator
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_prefix(dapl_id, prefix_segment, buf_size + 1) < 0)
                h5libraryError();
            prefix = prefix_segment.getString(0, StandardCharsets.UTF_8);
        }
        return prefix;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_virtual_prefix Sets prefix to be applied to virtual file paths.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier
     * @param prefix
     *            IN: Prefix to be applied to virtual file paths
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            prefix is null.
     *
     **/
    public static void H5Pset_virtual_prefix(long dapl_id, String prefix)
        throws HDF5LibraryException, NullPointerException
    {
        if (prefix == null) {
            throw new NullPointerException("prefix is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment prefix_segment = arena.allocateFrom(prefix);

            if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_virtual_prefix(dapl_id, prefix_segment) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_efile_prefix Retrieves prefix applied to external file paths.
     *
     * @param dapl_id
     *            IN: Link access property list identifier
     *
     * @return the prefix to be applied to external file paths.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static String H5Pget_efile_prefix(long dapl_id) throws HDF5LibraryException
    {
        long buf_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((buf_size =
                     org.hdfgroup.javahdf5.hdf5_h.H5Pget_efile_prefix(dapl_id, MemorySegment.NULL, 0)) < 0)
                h5libraryError();
        }

        String prefix = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment prefix_segment = arena.allocate(buf_size + 1); // +1 for null terminator
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_efile_prefix(dapl_id, prefix_segment, buf_size + 1) < 0)
                h5libraryError();
            prefix = prefix_segment.getString(0, StandardCharsets.UTF_8);
        }
        return prefix;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_efile_prefix Sets prefix to be applied to external file paths.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier
     * @param prefix
     *            IN: Prefix to be applied to external file paths
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            prefix is null.
     *
     **/
    public static void H5Pset_efile_prefix(long dapl_id, String prefix)
        throws HDF5LibraryException, NullPointerException
    {
        if (prefix == null) {
            throw new NullPointerException("prefix is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment prefix_segment = arena.allocateFrom(prefix);

            if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_efile_prefix(dapl_id, prefix_segment) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_virtual_spatial_tree accesses the flag for whether to use/not use a spatial tree
     * during mapping operations on a Virtual Dataset. The default value is true.
     *
     * Use of a spatial tree will accelerate the process of searching through mappings
     * to determine which contain intersections with the user's selection region.
     * With the tree disabled, all mappings will simply be iterated through and
     * checked directly.
     *
     * Certain workflows may find that tree creation overhead outweighs the time saved
     * on reads. In this case, disabling this property will lead to a performance improvement,
     * though it is expected that almost all cases will benefit from the tree on net.
     *
     * @param dapl_id
     *            IN: Dataset access property list
     *
     * @return true if the given dapl is set to use a spatial tree, false if not.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Pget_virtual_spatial_tree(long dapl_id) throws HDF5LibraryException
    {
        boolean use_tree = false;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the boolean value
            MemorySegment use_tree_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_virtual_spatial_tree(dapl_id, use_tree_segment) < 0)
                h5libraryError();
            // Read the boolean value from the segment
            use_tree = use_tree_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
        return use_tree;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_virtual_spatial_tree sets the dapl to use/not use a spatial tree
     * during mapping operations on a Virtual Dataset. The default value is true.
     *
     * Use of a spatial tree will accelerate the process of searching through mappings
     * to determine which contain intersections with the user's selection region.
     * With the tree disabled, all mappings will simply be iterated through and
     * checked directly.
     *
     * Certain workflows may find that tree creation overhead outweighs the time saved
     * on reads. In this case, disabling this property will lead to a performance improvement,
     * though it is expected that almost all cases will benefit from the tree on net.
     *
     * @param dapl_id
     *            IN: Dataset access property list
     *
     * @param use_tree
     *            IN: the use_tree flag setting
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Pset_virtual_spatial_tree(long dapl_id, boolean use_tree) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_virtual_spatial_tree(dapl_id, use_tree) < 0) {
            h5libraryError();
        }
    }

    // public  static  void H5Pset_append_flush(long plist_id, int ndims, long[] boundary,
    // H5D_append_cb func, H5D_append_t udata) throws HDF5LibraryException {}

    // public  static  void H5Pget_append_flush(long plist_id, int dims, long[] boundary,
    // H5D_append_cb func, H5D_append_t udata) throws HDF5LibraryException {}

    // /////// Dataset xfer property list (DXPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_data_transform retrieves the data transform expression previously set in the dataset transfer
     * property list plist_id by H5Pset_data_transform.
     *
     * @param plist_id
     *            IN: Identifier of the property list or class
     * @param size
     *            IN: Number of bytes of the transform expression to copy to
     * @param expression
     *            OUT: A data transform expression
     *
     * @return The size of the transform expression if successful; 0(zero) if no transform expression exists.
     *         Otherwise returns a negative value.
     *
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Size is &lt;= 0.
     * @exception NullPointerException
     *            expression is null.
     *
     **/
    public static long H5Pget_data_transform(long plist_id, String[] expression, long size)
        throws HDF5LibraryException, HDF5FunctionArgumentException, NullPointerException
    {
        if (expression == null || expression.length < 1) {
            throw new NullPointerException("expression is null or has insufficient length");
        }
        if (size <= 0) {
            throw new HDF5FunctionArgumentException("Size is <= 0");
        }

        long buf_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Pget_data_transform(plist_id, MemorySegment.NULL,
                                                                               size)) < 0)
                h5libraryError();
        }
        String xpression = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment xpression_segment = arena.allocate(buf_size + 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_data_transform(plist_id, xpression_segment,
                                                                   buf_size + 1) < 0)
                h5libraryError();
            xpression = xpression_segment.getString(0, StandardCharsets.UTF_8);
        }
        if (xpression == null) {
            throw new HDF5LibraryException("Failed to retrieve data transform expression");
        }
        expression[0] = xpression;

        return buf_size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_data_transform sets a data transform expression
     *
     * @param plist_id
     *            IN: Identifier of the property list or class
     * @param expression
     *            IN: Pointer to the null-terminated data transform expression
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            expression is null.
     *
     **/
    public static int H5Pset_data_transform(long plist_id, String expression)
        throws HDF5LibraryException, NullPointerException
    {
        if (expression == null) {
            throw new NullPointerException("expression is null");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment expression_segment = arena.allocateFrom(expression);

            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_data_transform(plist_id, expression_segment);
            if (retVal < 0) {
                h5libraryError();
            }
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_buffer gets type conversion and background buffers. Returns buffer size, in bytes, if
     * successful; otherwise 0 on failure.
     *
     * @param plist
     *            Identifier for the dataset transfer property list.
     * @param tconv
     *            byte array of application-allocated type conversion buffer.
     * @param bkg
     *            byte array of application-allocated background buffer.
     *
     * @return buffer size, in bytes, if successful; otherwise 0 on failure
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            plist is invalid.
     * @exception NullPointerException
     *           tconv or bkg is null.
     **/
    public static int H5Pget_buffer(long plist, byte[] tconv, byte[] bkg)
        throws HDF5LibraryException, HDF5FunctionArgumentException, NullPointerException
    {
        if (tconv == null || bkg == null || tconv.length < 1 || bkg.length < 1) {
            throw new NullPointerException("tconv or bkg is null or has insufficient length");
        }
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        long buf_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment tconv_segment = arena.allocate(tconv.length);
            MemorySegment bkg_segment   = arena.allocate(bkg.length);

            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Pget_buffer(plist, tconv_segment, bkg_segment)) <
                0)
                h5libraryError();

            tconv = tconv_segment.toArray(ValueLayout.JAVA_BYTE);
            bkg   = bkg_segment.toArray(ValueLayout.JAVA_BYTE);
        }
        return (int)buf_size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_buffer_size gets type conversion and background buffer size, in bytes, if successful;
     * otherwise 0 on failure.
     *
     * @param plist
     *            Identifier for the dataset transfer property list.
     *
     * @return buffer size, in bytes, if successful; otherwise 0 on failure
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            plist is invalid.
     **/
    public static long H5Pget_buffer_size(long plist)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {

        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        long buf_size = -1;
        if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Pget_buffer(plist, MemorySegment.NULL,
                                                                   MemorySegment.NULL)) < 0)
            h5libraryError();

        return buf_size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_buffer sets type conversion and background buffers. status to TRUE or FALSE.
     *
     * Given a dataset transfer property list, H5Pset_buffer sets the maximum size for the type conversion
     * buffer and background buffer and optionally supplies pointers to application-allocated buffers. If the
     * buffer size is smaller than the entire amount of data being transferred between the application and the
     * file, and a type conversion buffer or background buffer is required, then strip mining will be used.
     *
     * Note that there are minimum size requirements for the buffer. Strip mining can only break the data up
     * along the first dimension, so the buffer must be large enough to accommodate a complete slice that
     * encompasses all of the remaining dimensions. For example, when strip mining a 100x200x300 hyperslab of
     * a simple data space, the buffer must be large enough to hold 1x200x300 data elements. When strip mining
     * a 100x200x300x150 hyperslab of a simple data space, the buffer must be large enough to hold
     * 1x200x300x150 data elements.
     *
     * @param plist
     *            Identifier for the dataset transfer property list.
     * @param size
     *            Size, in bytes, of the type conversion and background buffers.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            plist is invalid.
     **/
    public static void H5Pset_buffer_size(long plist, long size)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (size < 0) {
            throw new HDF5FunctionArgumentException("size must be non-negative");
        }

        int retVal =
            org.hdfgroup.javahdf5.hdf5_h.H5Pset_buffer(plist, size, MemorySegment.NULL, MemorySegment.NULL);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_edc_check gets the error-detecting algorithm in use.
     *
     * @param plist
     *            Identifier for the dataset transfer property list.
     *
     * @return the error-detecting algorithm
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pget_edc_check(long plist) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }

        int check = org.hdfgroup.javahdf5.hdf5_h.H5Pget_edc_check(plist);
        if (check < 0)
            h5libraryError();
        return check;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_edc_check sets the error-detecting algorithm.
     *
     * @param plist
     *            Identifier for the dataset transfer property list.
     * @param check
     *            the error-detecting algorithm to use.
     *
     * @return non-negative if succeed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_edc_check(long plist, int check) throws HDF5LibraryException
    {
        if (plist < 0) {
            throw new HDF5FunctionArgumentException("Negative property list identifier");
        }
        if (check < 0) {
            throw new HDF5FunctionArgumentException("check must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_edc_check(plist, check);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_btree_ratio Get the B-tree split ratios for a dataset transfer property list.
     *
     * @param plist_id
     *            IN Dataset transfer property list
     * @param left
     *            OUT split ratio for leftmost nodes
     * @param right
     *            OUT split ratio for righttmost nodes
     * @param middle
     *            OUT split ratio for all other nodes
     *
     * @return non-negative if succeed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     **/
    public static int H5Pget_btree_ratios(long plist_id, double[] left, double[] middle, double[] right)
        throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;
        if (left == null || middle == null || right == null || left.length < 1 || middle.length < 1 ||
            right.length < 1) {
            throw new NullPointerException("one or more arrays are null or have insufficient length");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment left_segment   = arena.allocate(ValueLayout.JAVA_DOUBLE, 1);
            MemorySegment middle_segment = arena.allocate(ValueLayout.JAVA_DOUBLE, 1);
            MemorySegment right_segment  = arena.allocate(ValueLayout.JAVA_DOUBLE, 1);

            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_btree_ratios(plist_id, left_segment, middle_segment,
                                                                      right_segment);
            if (retVal < 0)
                h5libraryError();

            left[0]   = left_segment.get(ValueLayout.JAVA_DOUBLE, 0);
            middle[0] = middle_segment.get(ValueLayout.JAVA_DOUBLE, 0);
            right[0]  = right_segment.get(ValueLayout.JAVA_DOUBLE, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_btree_ratio Sets B-tree split ratios for a dataset transfer property list. The split ratios
     * determine what percent of children go in the first node when a node splits.
     *
     * @param plist_id
     *            IN Dataset transfer property list
     * @param left
     *            IN split ratio for leftmost nodes
     * @param right
     *            IN split ratio for righttmost nodes
     * @param middle
     *            IN split ratio for all other nodes
     *
     * @return non-negative if succeed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Pset_btree_ratios(long plist_id, double left, double middle, double right)
        throws HDF5LibraryException
    {
        if (left < 0.0 || middle < 0.0 || right < 0.0) {
            throw new HDF5FunctionArgumentException("left, middle or right must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_btree_ratios(plist_id, left, middle, right);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_hyper_vector_size reads values previously set with H5Pset_hyper_vector_size.
     *
     * @param dxpl_id
     *            IN: Dataset transfer property list identifier.
     * @param vector_size
     *            OUT:  hyper vector size.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            vector_size is null.
     *
     **/
    public static int H5Pget_hyper_vector_size(long dxpl_id, long[] vector_size)
        throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;
        if (vector_size == null || vector_size.length < 1) {
            throw new NullPointerException("vector_size is null or has insufficient length");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment vector_size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_hyper_vector_size(dxpl_id, vector_size_segment);
            if (retVal < 0)
                h5libraryError();
            vector_size[0] = vector_size_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_hyper_vector_size sets the number of
     *              "I/O vectors" (offset and length pairs) which are to be
     *              accumulated in memory before being issued to the lower levels
     *              of the library for reading or writing the actual data.
     *              Increasing the number should give better performance, but use
     *              more memory during hyperslab I/O.  The vector size must be
     *              greater than 1.
     *<p>
     *              The default is to use 1024 vectors for I/O during hyperslab
     *              reading/writing.
     *
     * @param dxpl_id
     *            IN: Dataset transfer property list identifier.
     * @param vector_size
     *            IN: hyper vestor size.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_hyper_vector_size(long dxpl_id, long vector_size) throws HDF5LibraryException
    {
        if (vector_size <= 1) {
            throw new HDF5FunctionArgumentException("vector_size must be greater than 1");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_hyper_vector_size(dxpl_id, vector_size);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    // /////// Link creation property list (LCPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_create_intermediate_group determines whether property is set to enable creating missing
     * intermediate groups.
     *
     * @param lcpl_id
     *            IN: Link creation property list identifier
     *
     * @return Boolean true or false
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static boolean H5Pget_create_intermediate_group(long lcpl_id) throws HDF5LibraryException
    {

        boolean crt_intermed_group = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment crt_intermed_group_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_create_intermediate_group(lcpl_id,
                                                                              crt_intermed_group_segment) < 0)
                h5libraryError();
            crt_intermed_group = crt_intermed_group_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
        return crt_intermed_group;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_create_intermediate_group specifies in property list whether to create missing intermediate
     * groups
     *
     * @param lcpl_id
     *            IN: Link creation property list identifier
     * @param crt_intermed_group
     *            IN: Flag specifying whether to create intermediate groups upon the creation of an object
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_create_intermediate_group(long lcpl_id, boolean crt_intermed_group)
        throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_create_intermediate_group(
            lcpl_id, crt_intermed_group ? 1 : 0);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    // /////// Group creation property list (GCPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_local_heap_size_hint Retrieves the anticipated size of the local heap for original-style groups.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     *
     * @return size_hint, the anticipated size of local heap
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static long H5Pget_local_heap_size_hint(long gcpl_id) throws HDF5LibraryException
    {
        long size_hint = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_hint_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_local_heap_size_hint(gcpl_id, size_hint_segment) < 0)
                h5libraryError();
            size_hint = size_hint_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return size_hint;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_local_heap_size_hint Specifies the anticipated maximum size of a local heap.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     * @param size_hint
     *            IN: Anticipated maximum size in bytes of local heap
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_local_heap_size_hint(long gcpl_id, long size_hint) throws HDF5LibraryException
    {
        if (size_hint < 0) {
            throw new HDF5FunctionArgumentException("size_hint must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_local_heap_size_hint(gcpl_id, size_hint);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_link_phase_change Queries the settings for conversion between compact and dense groups.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     * @param links
     *            The max. no. of compact links &amp; the min. no. of dense links, which are used for storing
     *            groups
     *
     *            <pre>
     *      links[0] =  The maximum number of links for compact storage
     *      links[1] =  The minimum number of links for dense storage
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     *
     **/
    public static int H5Pget_link_phase_change(long gcpl_id, int[] links)
        throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;
        if (links == null || links.length < 2) {
            throw new NullPointerException("links is null or has insufficient length");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment compact_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment dense_segment   = arena.allocate(ValueLayout.JAVA_INT, 1);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_link_phase_change(gcpl_id, compact_segment,
                                                                           dense_segment);
            if (retVal < 0)
                h5libraryError();
            links[0] = compact_segment.get(ValueLayout.JAVA_INT, 0);
            links[1] = dense_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_link_phase_change Sets the parameters for conversion between compact and dense groups.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     * @param max_compact
     *            IN: Maximum number of links for compact storage(Default: 8)
     * @param min_dense
     *            IN: Minimum number of links for dense storage(Default: 6)
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values of max_compact and min_dense.
     *
     **/
    public static int H5Pset_link_phase_change(long gcpl_id, int max_compact, int min_dense)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (max_compact < 0 || min_dense < 0) {
            throw new HDF5FunctionArgumentException("max_compact and min_dense must be non-negative");
        }
        if (max_compact < min_dense) {
            throw new HDF5FunctionArgumentException("max_compact must be greater than or equal to min_dense");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_link_phase_change(gcpl_id, max_compact, min_dense);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_est_link_info Queries data required to estimate required local heap or object header size.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     * @param link_info
     *            Estimated number of links to be inserted into group And the estimated average length of link
     *            names
     *
     *            <pre>
     *      link_info[0] =  Estimated number of links to be inserted into group
     *      link_info[1] =  Estimated average length of link names
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            link_info is null.
     *
     **/
    public static int H5Pget_est_link_info(long gcpl_id, int[] link_info)
        throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;
        if (link_info == null || link_info.length < 2) {
            throw new NullPointerException("link_info is null or has insufficient length");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment est_num_entries_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment est_name_len_segment    = arena.allocate(ValueLayout.JAVA_INT, 1);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_est_link_info(gcpl_id, est_num_entries_segment,
                                                                       est_name_len_segment);
            if (retVal < 0)
                h5libraryError();
            link_info[0] = est_num_entries_segment.get(ValueLayout.JAVA_INT, 0);
            link_info[1] = est_name_len_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_est_link_info Sets estimated number of links and length of link names in a group.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     * @param est_num_entries
     *            IN: Estimated number of links to be inserted into group
     * @param est_name_len
     *            IN: Estimated average length of link names
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid values to est_num_entries and est_name_len.
     *
     **/
    public static int H5Pset_est_link_info(long gcpl_id, int est_num_entries, int est_name_len)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (est_num_entries < 0 || est_name_len < 0) {
            throw new HDF5FunctionArgumentException("est_num_entries and est_name_len must be non-negative");
        }

        int retVal =
            org.hdfgroup.javahdf5.hdf5_h.H5Pset_est_link_info(gcpl_id, est_num_entries, est_name_len);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_link_creation_order queries the group creation property list, gcpl_id, and returns a flag
     * indicating whether link creation order is tracked and/or indexed in a group.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     *
     * @return crt_order_flags -Creation order flag(s)
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_link_creation_order(long gcpl_id) throws HDF5LibraryException
    {
        int crt_order_flags = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment crt_order_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_link_creation_order(gcpl_id, crt_order_segment) < 0)
                h5libraryError();
            crt_order_flags = crt_order_segment.get(ValueLayout.JAVA_INT, 0);
        }
        if (crt_order_flags < 0)
            h5libraryError();
        return crt_order_flags;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_link_creation_order Sets flags in a group creation property list, gcpl_id, for tracking and/or
     * indexing links on creation order.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     * @param crt_order_flags
     *            IN: Creation order flag(s)
     *
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_link_creation_order(long gcpl_id, int crt_order_flags)
        throws HDF5LibraryException
    {
        if (crt_order_flags < 0) {
            throw new HDF5FunctionArgumentException("crt_order_flags must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_link_creation_order(gcpl_id, crt_order_flags);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    // /////// String creation property list (STRCPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_char_encoding gets the character encoding of the string.
     *
     * @param plist_id
     *            IN: the property list identifier
     *
     * @return Returns the character encoding of the string.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_char_encoding(long plist_id) throws HDF5LibraryException
    {
        int encoding = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment encoding_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_char_encoding(plist_id, encoding_segment) < 0)
                h5libraryError();
            encoding = encoding_segment.get(ValueLayout.JAVA_INT, 0);
        }
        if (encoding < 0)
            h5libraryError();
        return encoding;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_char_encoding sets the character encoding of the string.
     *
     * @param plist_id
     *            IN: the property list identifier
     * @param encoding
     *            IN: the character encoding of the string
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_char_encoding(long plist_id, int encoding) throws HDF5LibraryException
    {
        if (encoding < 0) {
            throw new HDF5FunctionArgumentException("encoding must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_char_encoding(plist_id, encoding);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    // /////// Link access property list (LAPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_nlinks retrieves the maximum number of soft or user-defined link traversals allowed, nlinks,
     * before the library assumes it has found a cycle and aborts the traversal. This value is retrieved from
     * the link access property list lapl_id.
     *
     * @param lapl_id
     *            IN: File access property list identifier
     *
     * @return Returns a Maximum number of links to traverse.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static long H5Pget_nlinks(long lapl_id) throws HDF5LibraryException
    {
        long nlinks = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment nlinks_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_nlinks(lapl_id, nlinks_segment) < 0)
                h5libraryError();
            nlinks = nlinks_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        if (nlinks < 0)
            h5libraryError();
        return nlinks;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_nlinks sets the maximum number of soft or user-defined link traversals allowed, nlinks, before
     * the library assumes it has found a cycle and aborts the traversal. This value is set in the link access
     * property list lapl_id.
     *
     * @param lapl_id
     *            IN: File access property list identifier
     * @param nlinks
     *            IN: Maximum number of links to traverse
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Argument is Illegal
     *
     **/
    public static int H5Pset_nlinks(long lapl_id, long nlinks)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (nlinks < 0) {
            throw new HDF5FunctionArgumentException("nlinks must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_nlinks(lapl_id, nlinks);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_elink_prefix Retrieves prefix applied to external link paths.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     * @param prefix
     *            OUT: Prefix applied to external link paths
     *
     * @return If successful, returns a non-negative value specifying the size in bytes of the prefix without
     *         the NULL terminator; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            prefix is null.
     *
     **/
    public static long H5Pget_elink_prefix(long lapl_id, String[] prefix)
        throws HDF5LibraryException, NullPointerException
    {
        if (prefix == null || prefix.length < 1) {
            throw new NullPointerException("prefix is null or has insufficient length");
        }

        long buf_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Pget_elink_prefix(lapl_id, MemorySegment.NULL, 0);
            if (buf_size < 0)
                h5libraryError();
            MemorySegment prefix_segment = arena.allocate(buf_size + 1);
            buf_size =
                org.hdfgroup.javahdf5.hdf5_h.H5Pget_elink_prefix(lapl_id, prefix_segment, buf_size + 1);
            if (buf_size < 0)
                h5libraryError();
            prefix[0] = prefix_segment.getString(0, StandardCharsets.UTF_8);
        }
        return buf_size;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_elink_prefix Sets prefix to be applied to external link paths.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     * @param prefix
     *            IN: Prefix to be applied to external link paths
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            prefix is null.
     *
     **/
    public static int H5Pset_elink_prefix(long lapl_id, String prefix)
        throws HDF5LibraryException, NullPointerException
    {
        if (prefix == null) {
            throw new NullPointerException("prefix is null");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment prefix_segment = arena.allocateFrom(prefix);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_elink_prefix(lapl_id, prefix_segment);
            if (retVal < 0) {
                h5libraryError();
            }
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_elink_fapl Retrieves the file access property list identifier associated with the link access
     * property list.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static long H5Pget_elink_fapl(long lapl_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Pget_elink_fapl(lapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Pget_elink_fapl add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_elink_fapl sets a file access property list for use in accessing a file pointed to by an
     * external link.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_elink_fapl(long lapl_id, long fapl_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_elink_fapl(lapl_id, fapl_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_elink_acc_flags retrieves the external link traversal file access flag from the specified link
     * access property list.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return File access flag for link traversal.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_elink_acc_flags(long lapl_id) throws HDF5LibraryException
    {
        int flags = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flags_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_elink_acc_flags(lapl_id, flags_segment) < 0)
                h5libraryError();
            flags = flags_segment.get(ValueLayout.JAVA_INT, 0);
        }
        if (flags < 0)
            h5libraryError();
        return flags;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_elink_acc_flags Sets the external link traversal file access flag in a link access property
     * list.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     * @param flags
     *            IN: The access flag for external link traversal.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception HDF5FunctionArgumentException
     *            Invalid Flag values.
     *
     **/
    public static int H5Pset_elink_acc_flags(long lapl_id, int flags)
        throws HDF5LibraryException, HDF5FunctionArgumentException
    {
        if (flags < 0) {
            throw new HDF5FunctionArgumentException("flags must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_elink_acc_flags(lapl_id, flags);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    // /////// Object copy property list (OCPYPL) routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_copy_object retrieves the properties to be used when an object is copied.
     *
     * @param ocp_plist_id
     *            IN: Object copy property list identifier
     *
     * @return Copy option(s) set in the object copy property list
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_copy_object(long ocp_plist_id) throws HDF5LibraryException
    {
        int copy_options = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment copy_options_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_copy_object(ocp_plist_id, copy_options_segment) < 0)
                h5libraryError();
            copy_options = copy_options_segment.get(ValueLayout.JAVA_INT, 0);
        }
        if (copy_options < 0)
            h5libraryError();
        return copy_options;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_copy_object Sets properties to be used when an object is copied.
     *
     * @param ocp_plist_id
     *            IN: Object copy property list identifier
     * @param copy_options
     *            IN: Copy option(s) to be set
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static void H5Pset_copy_object(long ocp_plist_id, int copy_options) throws HDF5LibraryException
    {
        if (copy_options < 0) {
            throw new HDF5FunctionArgumentException("copy_options must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_copy_object(ocp_plist_id, copy_options);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    // /////// file drivers property list routines ///////

    /**
     * @ingroup JH5P
     *
     * H5Pget_fapl_core retrieve H5FD_CORE I/O settings.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param increment
     *            OUT: how much to grow the memory each time
     * @param backing_store
     *            OUT: write to file name on flush setting
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            increment or backing_store is null.
     *
     **/
    public static void H5Pget_fapl_core(long fapl_id, long[] increment, boolean[] backing_store)
        throws HDF5LibraryException, NullPointerException
    {
        if (increment == null || increment.length < 1 || backing_store == null || backing_store.length < 1) {
            throw new NullPointerException("increment or backing_store is null or has insufficient length");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment increment_segment     = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment backing_store_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pget_fapl_core(fapl_id, increment_segment,
                                                                       backing_store_segment);
            if (retVal < 0)
                h5libraryError();
            increment[0]     = increment_segment.get(ValueLayout.JAVA_LONG, 0);
            backing_store[0] = backing_store_segment.get(ValueLayout.JAVA_BOOLEAN, 0);
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_core modifies the file access property list to use the H5FD_CORE driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param increment
     *            IN: how much to grow the memory each time
     * @param backing_store
     *            IN: write to file name on flush setting
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fapl_core(long fapl_id, long increment, boolean backing_store)
        throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_fapl_core(fapl_id, increment, backing_store);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_fapl_direct queries properties set by the H5Pset_fapl_direct.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param info
     *            OUT: Returned property list information
     *                 info[0] = increment -how much to grow the memory each time
     *                 info[1] = backing_store - write to file name on flush setting
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pget_fapl_direct(long fapl_id, long[] info) throws HDF5LibraryException
    {
        if (info == null || info.length < 3) {
            throw new NullPointerException("info is null or has insufficient length (needs 3 elements)");
        }

        try {
            // Use reflection to check if H5Pget_fapl_direct exists
            java.lang.reflect.Method method = org.hdfgroup.javahdf5.hdf5_h.class.getMethod(
                "H5Pget_fapl_direct", long.class, MemorySegment.class, MemorySegment.class,
                MemorySegment.class);

            int retVal = -1;
            try (Arena arena = Arena.ofConfined()) {
                MemorySegment alignment_segment  = arena.allocate(ValueLayout.JAVA_LONG, 1);
                MemorySegment block_size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
                MemorySegment cbuf_size_segment  = arena.allocate(ValueLayout.JAVA_LONG, 1);
                if ((retVal = (int)method.invoke(null, fapl_id, alignment_segment, block_size_segment,
                                                 cbuf_size_segment)) < 0)
                    h5libraryError();
                info[0] = alignment_segment.get(ValueLayout.JAVA_LONG, 0);
                info[1] = block_size_segment.get(ValueLayout.JAVA_LONG, 0);
                info[2] = cbuf_size_segment.get(ValueLayout.JAVA_LONG, 0);
            }
            return retVal;
        }
        catch (NoSuchMethodException e) {
            throw new HDF5LibraryException(
                "H5Pget_fapl_direct not available (Direct VFD not enabled in this build)");
        }
        catch (Exception e) {
            throw new HDF5LibraryException("H5Pget_fapl_direct failed: " + e.getMessage());
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_direct Sets up use of the direct I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param alignment
     *            IN: Required memory alignment boundary
     * @param block_size
     *            IN: File system block size
     * @param cbuf_size
     *            IN: Copy buffer size
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fapl_direct(long fapl_id, long alignment, long block_size, long cbuf_size)
        throws HDF5LibraryException
    {
        if (alignment < 0 || block_size < 0 || cbuf_size < 0) {
            throw new HDF5FunctionArgumentException(
                "alignment, block_size, and cbuf_size must be non-negative");
        }

        try {
            // Use reflection to check if H5Pset_fapl_direct exists
            java.lang.reflect.Method method = org.hdfgroup.javahdf5.hdf5_h.class.getMethod(
                "H5Pset_fapl_direct", long.class, long.class, long.class, long.class);

            int retVal = (int)method.invoke(null, fapl_id, alignment, block_size, cbuf_size);
            if (retVal < 0) {
                h5libraryError();
            }
            return retVal;
        }
        catch (NoSuchMethodException e) {
            throw new HDF5LibraryException(
                "H5Pset_fapl_direct not available (Direct VFD not enabled in this build)");
        }
        catch (Exception e) {
            throw new HDF5LibraryException("H5Pset_fapl_direct failed: " + e.getMessage());
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_fapl_family Returns information about the family file access property list.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param memb_size
     *            OUT: the size in bytes of each file member (used only when creating a new file)
     * @param memb_fapl_id
     *            OUT: the file access property list to be used for each family member
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            memb_size or memb_fapl_id is null.
     *
     **/
    public static int H5Pget_fapl_family(long fapl_id, long[] memb_size, long[] memb_fapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (memb_size == null || memb_size.length < 1 || memb_fapl_id == null || memb_fapl_id.length < 1) {
            throw new NullPointerException("memb_size or memb_fapl_id is null or has insufficient length");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment memb_size_segment    = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment memb_fapl_id_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((org.hdfgroup.javahdf5.hdf5_h.H5Pget_fapl_family(fapl_id, memb_size_segment,
                                                                 memb_fapl_id_segment)) < 0)
                h5libraryError();
            memb_size[0]    = memb_size_segment.get(ValueLayout.JAVA_LONG, 0);
            memb_fapl_id[0] = memb_fapl_id_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_family Sets up use of the direct I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param memb_size
     *            IN: the size in bytes of each file member (used only when creating a new file)
     * @param memb_fapl_id
     *            IN: the file access property list to be used for each family member
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fapl_family(long fapl_id, long memb_size, long memb_fapl_id)
        throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_fapl_family(fapl_id, memb_size, memb_fapl_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_hdfs Modify the file access property list to use the H5FD_HDFS driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param fapl_conf
     *            IN: the properties of the hdfs driver
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fapl_hdfs(long fapl_id, Object fapl_conf)
        throws HDF5LibraryException, NullPointerException
    {
        try {
            // Check if H5Pset_fapl_hdfs exists using reflection
            java.lang.reflect.Method method = org.hdfgroup.javahdf5.hdf5_h.class.getMethod(
                "H5Pset_fapl_hdfs", long.class, MemorySegment.class);

            // Method exists, but struct conversion not yet implemented
            throw new HDF5LibraryException("H5Pset_fapl_hdfs struct conversion not yet implemented for FFM");
        }
        catch (NoSuchMethodException e) {
            throw new HDF5LibraryException(
                "H5Pset_fapl_hdfs not available (HDFS VFD not enabled in this build)");
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_fapl_hdfs gets the properties hdfs I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return the properties of the hdfs driver.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static Object H5Pget_fapl_hdfs(long fapl_id) throws HDF5LibraryException
    {
        try {
            // Check if H5Pget_fapl_hdfs exists using reflection
            java.lang.reflect.Method method = org.hdfgroup.javahdf5.hdf5_h.class.getMethod(
                "H5Pget_fapl_hdfs", long.class, MemorySegment.class);

            // Method exists, but struct conversion not yet implemented
            throw new HDF5LibraryException("H5Pget_fapl_hdfs struct conversion not yet implemented for FFM");
        }
        catch (NoSuchMethodException e) {
            throw new HDF5LibraryException(
                "H5Pget_fapl_hdfs not available (HDFS VFD not enabled in this build)");
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_fapl_multi Sets up use of the multi I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param memb_map
     *            IN: Maps memory usage types to other memory usage types.
     * @param memb_fapl
     *            IN: Property list for each memory usage type.
     * @param memb_name
     *            IN: Name generator for names of member files.
     * @param memb_addr
     *            IN: The offsets within the virtual address space, from 0 (zero) to HADDR_MAX, at which each
     *                type of data storage begins.
     *
     * @return a boolean value; Allows read-only access to incomplete file sets when TRUE.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an array is null.
     *
     **/
    public static boolean H5Pget_fapl_multi(long fapl_id, int[] memb_map, long[] memb_fapl,
                                            String[] memb_name, long[] memb_addr)
        throws HDF5LibraryException, NullPointerException
    {
        boolean relax = false; // Default to false
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment map_segment = MemorySegment.NULL;
            if (memb_map != null)
                map_segment = arena.allocate(ValueLayout.JAVA_INT, HDF5Constants.H5FD_MEM_NTYPES);
            MemorySegment fapl_segment = MemorySegment.NULL;
            if (memb_fapl != null)
                fapl_segment = arena.allocate(ValueLayout.JAVA_LONG, HDF5Constants.H5FD_MEM_NTYPES);
            MemorySegment name_segment = MemorySegment.NULL;
            if (memb_name != null) {
                SequenceLayout stringArrayLayout =
                    MemoryLayout.sequenceLayout(HDF5Constants.H5FD_MEM_NTYPES, ValueLayout.ADDRESS);
                name_segment = arena.allocate(stringArrayLayout, HDF5Constants.H5FD_MEM_NTYPES);
                for (int i = 0; i < memb_name.length; i++) {
                    // allocateFrom converts the Java string to a null-terminated C string in off-heap memory
                    MemorySegment cString = MemorySegment.NULL;
                    if (memb_name[i] != null)
                        cString = arena.allocateFrom(memb_name[i], StandardCharsets.UTF_8);
                    // Store the address of the C string in the 'pointers' segment
                    name_segment.setAtIndex(ValueLayout.ADDRESS, i, cString);
                }
            }
            MemorySegment addr_segment = MemorySegment.NULL;
            if (memb_addr != null)
                addr_segment = arena.allocate(ValueLayout.JAVA_LONG, HDF5Constants.H5FD_MEM_NTYPES);
            MemorySegment relax_segment = arena.allocate(ValueLayout.JAVA_BOOLEAN, 1);
            relax_segment.set(ValueLayout.JAVA_BOOLEAN, 0, relax); // Default to false
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pget_fapl_multi(fapl_id, map_segment, fapl_segment,
                                                               name_segment, addr_segment, relax_segment) < 0)
                h5libraryError();
            if (memb_map != null)
                MemorySegment.copy(map_segment, ValueLayout.JAVA_INT, 0L, memb_map, 0,
                                   HDF5Constants.H5FD_MEM_NTYPES);
            if (memb_fapl != null)
                MemorySegment.copy(fapl_segment, ValueLayout.JAVA_LONG, 0L, memb_fapl, 0,
                                   HDF5Constants.H5FD_MEM_NTYPES);
            if (memb_addr != null)
                MemorySegment.copy(addr_segment, ValueLayout.JAVA_LONG, 0L, memb_addr, 0,
                                   HDF5Constants.H5FD_MEM_NTYPES);
            if (memb_name != null) {
                for (int i = 0; i < HDF5Constants.H5FD_MEM_NTYPES; i++) {
                    MemorySegment cStringSegment = name_segment.getAtIndex(ValueLayout.ADDRESS, i);
                    memb_name[i]                 = null;
                    if (cStringSegment.address() != 0)
                        memb_name[i] = cStringSegment.reinterpret(Integer.MAX_VALUE)
                                           .getString(0, StandardCharsets.UTF_8);
                }
            }
            relax = relax_segment.get(ValueLayout.JAVA_BOOLEAN, 0); // Default to false
        }
        return relax;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_multi Sets up use of the multi I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param memb_map
     *            IN: Maps memory usage types to other memory usage types.
     * @param memb_fapl
     *            IN: Property list for each memory usage type.
     * @param memb_name
     *            IN: Name generator for names of member files.
     * @param memb_addr
     *            IN: The offsets within the virtual address space, from 0 (zero) to HADDR_MAX, at which each
     *                type of data storage begins.
     * @param relax
     *            IN: Allows read-only access to incomplete file sets when TRUE.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an array is null.
     *
     **/
    public static void H5Pset_fapl_multi(long fapl_id, int[] memb_map, long[] memb_fapl, String[] memb_name,
                                         long[] memb_addr, boolean relax)
        throws HDF5LibraryException, NullPointerException
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment memb_map_segment = MemorySegment.NULL;
            if (memb_map != null)
                memb_map_segment = arena.allocateFrom(ValueLayout.JAVA_INT, memb_map);
            MemorySegment memb_fapl_segment = MemorySegment.NULL;
            if (memb_fapl != null)
                memb_fapl_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, memb_fapl);
            MemorySegment memb_name_segment = MemorySegment.NULL;
            if (memb_name != null) {
                memb_name_segment = arena.allocate(ValueLayout.ADDRESS, memb_name.length);
                for (int i = 0; i < memb_name.length; i++) {
                    // allocateFrom converts the Java string to a null-terminated C string in off-heap memory
                    MemorySegment cString = MemorySegment.NULL;
                    if (memb_name[i] != null)
                        cString = arena.allocateFrom(memb_name[i], StandardCharsets.UTF_8);
                    // Store the address of the C string in the 'pointers' segment
                    memb_name_segment.setAtIndex(ValueLayout.ADDRESS, i, cString);
                }
            }
            MemorySegment memb_addr_segment = MemorySegment.NULL;
            if (memb_addr != null)
                memb_addr_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, memb_addr);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_fapl_multi(fapl_id, memb_map_segment, memb_fapl_segment,
                                                               memb_name_segment, memb_addr_segment,
                                                               relax) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_log Sets up the logging virtual file driver (H5FD_LOG) for use. H5Pset_fapl_log modifies
     * the file access property list to use the logging driver, H5FD_LOG. The logging virtual file driver
     * (VFD) is a clone of the standard SEC2 (H5FD_SEC2) driver with additional facilities for logging VFD
     * metrics and activity to a file.
     *
     * @param fapl_id
     *            IN: File access property list identifier.
     * @param logfile
     *            IN: logfile is the name of the file in which the logging entries are to be recorded.
     * @param flags
     *            IN: Flags specifying the types of logging activity.
     * @param buf_size
     *            IN: The size of the logging buffers, in bytes.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            logfile is null.
     **/
    public static void H5Pset_fapl_log(long fapl_id, String logfile, long flags, long buf_size)
        throws HDF5LibraryException, NullPointerException
    {
        if (logfile == null) {
            throw new NullPointerException("logfile is null");
        }
        if (buf_size < 0) {
            throw new HDF5FunctionArgumentException("buf_size must be non-negative");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment logfile_segment = arena.allocateFrom(logfile);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_fapl_log(fapl_id, logfile_segment, flags, buf_size) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_sec2 Sets up use of the sec2 I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fapl_sec2(long fapl_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_fapl_sec2(fapl_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_split Sets up use of the split I/O driver. Makes the multi driver act like the
     *        old split driver which stored meta data in one file and raw
     *        data in another file
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param meta_ext
     *            IN: meta filename extension
     * @param meta_plist_id
     *            IN: File access property list identifier for metadata
     * @param raw_ext
     *            IN: raw data filename extension
     * @param raw_plist_id
     *            IN: File access property list identifier raw data
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            meta_ext or raw_ext is null.
     *
     **/
    public static void H5Pset_fapl_split(long fapl_id, String meta_ext, long meta_plist_id, String raw_ext,
                                         long raw_plist_id) throws HDF5LibraryException, NullPointerException
    {
        if (meta_ext == null || raw_ext == null) {
            throw new NullPointerException("meta_ext or raw_ext is null");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment meta_ext_segment = arena.allocateFrom(meta_ext);
            MemorySegment raw_ext_segment  = arena.allocateFrom(raw_ext);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Pset_fapl_split(fapl_id, meta_ext_segment, meta_plist_id,
                                                               raw_ext_segment, raw_plist_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_stdio Sets up use of the stdio I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fapl_stdio(long fapl_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Pset_fapl_stdio(fapl_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_windows Sets up use of the windows I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fapl_windows(long fapl_id) throws HDF5LibraryException
    {
        try {
            // Use reflection to check if H5Pset_fapl_windows exists
            java.lang.reflect.Method method =
                org.hdfgroup.javahdf5.hdf5_h.class.getMethod("H5Pset_fapl_windows", long.class);

            int retVal = (int)method.invoke(null, fapl_id);
            if (retVal < 0) {
                h5libraryError();
            }
            return retVal;
        }
        catch (NoSuchMethodException e) {
            throw new HDF5LibraryException(
                "H5Pset_fapl_windows not available (Windows VFD not enabled in this build)");
        }
        catch (Exception e) {
            throw new HDF5LibraryException("H5Pset_fapl_windows failed: " + e.getMessage());
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pset_fapl_ros3 Modify the file access property list to use the H5FD_ROS3 driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param fapl_conf
     *            IN: the properties of the ros3 driver
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static int H5Pset_fapl_ros3(long fapl_id, hdf.hdf5lib.structs.H5FD_ros3_fapl_t fapl_conf)
        throws HDF5LibraryException, NullPointerException
    {
        if (fapl_conf == null) {
            throw new NullPointerException("fapl_conf is null");
        }

        try {
            // Use reflection to access FFM ROS3 classes (they may not exist if ROS3 disabled)
            Class<?> ros3Class = Class.forName("org.hdfgroup.javahdf5.H5FD_ros3_fapl_t");
            Class<?> hdf5Class = org.hdfgroup.javahdf5.hdf5_h.class;

            // Get method handles via reflection
            java.lang.reflect.Method h5psetMethod =
                hdf5Class.getMethod("H5Pset_fapl_ros3", long.class, MemorySegment.class);
            java.lang.reflect.Method allocateMethod = ros3Class.getMethod("allocate", SegmentAllocator.class);
            java.lang.reflect.Method versionMethod =
                ros3Class.getMethod("version", MemorySegment.class, int.class);
            java.lang.reflect.Method authenticateMethod =
                ros3Class.getMethod("authenticate", MemorySegment.class, boolean.class);
            java.lang.reflect.Method awsRegionMethod = ros3Class.getMethod("aws_region", MemorySegment.class);
            java.lang.reflect.Method secretIdMethod  = ros3Class.getMethod("secret_id", MemorySegment.class);
            java.lang.reflect.Method secretKeyMethod = ros3Class.getMethod("secret_key", MemorySegment.class);

            int retVal = -1;
            try (Arena arena = Arena.ofConfined()) {
                // Allocate FFM struct using reflection
                MemorySegment ffmConfig = (MemorySegment)allocateMethod.invoke(null, arena);

                // Set version and authenticate fields
                versionMethod.invoke(null, ffmConfig, fapl_conf.version);
                authenticateMethod.invoke(null, ffmConfig, fapl_conf.authenticate);

                // Get char array segments and copy strings
                MemorySegment awsRegionSeg = (MemorySegment)awsRegionMethod.invoke(null, ffmConfig);
                MemorySegment secretIdSeg  = (MemorySegment)secretIdMethod.invoke(null, ffmConfig);
                MemorySegment secretKeySeg = (MemorySegment)secretKeyMethod.invoke(null, ffmConfig);

                copyStringToCharArray(arena, fapl_conf.aws_region, awsRegionSeg, 33);
                copyStringToCharArray(arena, fapl_conf.secret_id, secretIdSeg, 129);
                copyStringToCharArray(arena, fapl_conf.secret_key, secretKeySeg, 129);

                // Call native H5Pset_fapl_ros3
                retVal = (int)h5psetMethod.invoke(null, fapl_id, ffmConfig);
                if (retVal < 0) {
                    h5libraryError();
                }
            }
            return retVal;
        }
        catch (ClassNotFoundException e) {
            throw new HDF5LibraryException(
                "H5Pset_fapl_ros3 not available (ros3 VFD not enabled in this build)");
        }
        catch (NoSuchMethodException e) {
            throw new HDF5LibraryException(
                "H5Pset_fapl_ros3 not available (ros3 VFD not enabled in this build)");
        }
        catch (java.lang.reflect.InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof HDF5LibraryException) {
                throw (HDF5LibraryException)cause;
            }
            throw new HDF5LibraryException("H5Pset_fapl_ros3 failed: " + e.getMessage());
        }
        catch (Exception e) {
            throw new HDF5LibraryException("H5Pset_fapl_ros3 failed: " + e.getMessage());
        }
    }

    /**
     * @ingroup JH5P
     *
     * H5Pget_fapl_ros3 gets the properties of the ros3 I/O driver.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return the properties of the ros3 driver.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     *
     **/
    public static hdf.hdf5lib.structs.H5FD_ros3_fapl_t H5Pget_fapl_ros3(long fapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        try {
            // Use reflection to access FFM ROS3 classes (they may not exist if ROS3 disabled)
            Class<?> ros3Class = Class.forName("org.hdfgroup.javahdf5.H5FD_ros3_fapl_t");
            Class<?> hdf5Class = org.hdfgroup.javahdf5.hdf5_h.class;

            // Get method handles via reflection
            java.lang.reflect.Method h5pgetMethod =
                hdf5Class.getMethod("H5Pget_fapl_ros3", long.class, MemorySegment.class);
            java.lang.reflect.Method allocateMethod = ros3Class.getMethod("allocate", SegmentAllocator.class);
            java.lang.reflect.Method versionGetMethod = ros3Class.getMethod("version", MemorySegment.class);
            java.lang.reflect.Method authenticateGetMethod =
                ros3Class.getMethod("authenticate", MemorySegment.class);
            java.lang.reflect.Method awsRegionMethod = ros3Class.getMethod("aws_region", MemorySegment.class);
            java.lang.reflect.Method secretIdMethod  = ros3Class.getMethod("secret_id", MemorySegment.class);
            java.lang.reflect.Method secretKeyMethod = ros3Class.getMethod("secret_key", MemorySegment.class);

            hdf.hdf5lib.structs.H5FD_ros3_fapl_t faplConfig = new hdf.hdf5lib.structs.H5FD_ros3_fapl_t();
            try (Arena arena = Arena.ofConfined()) {
                // Allocate FFM struct using reflection
                MemorySegment ffmConfig = (MemorySegment)allocateMethod.invoke(null, arena);

                // Call native H5Pget_fapl_ros3
                int status = (int)h5pgetMethod.invoke(null, fapl_id, ffmConfig);
                if (status < 0) {
                    h5libraryError();
                }

                // Extract version and authenticate fields
                faplConfig.version      = (int)versionGetMethod.invoke(null, ffmConfig);
                faplConfig.authenticate = (boolean)authenticateGetMethod.invoke(null, ffmConfig);

                // Get char array segments and extract strings
                MemorySegment awsRegionSeg = (MemorySegment)awsRegionMethod.invoke(null, ffmConfig);
                MemorySegment secretIdSeg  = (MemorySegment)secretIdMethod.invoke(null, ffmConfig);
                MemorySegment secretKeySeg = (MemorySegment)secretKeyMethod.invoke(null, ffmConfig);

                faplConfig.aws_region = extractStringFromCharArray(awsRegionSeg);
                faplConfig.secret_id  = extractStringFromCharArray(secretIdSeg);
                faplConfig.secret_key = extractStringFromCharArray(secretKeySeg);
            }
            return faplConfig;
        }
        catch (ClassNotFoundException e) {
            throw new HDF5LibraryException(
                "H5Pget_fapl_ros3 not available (ros3 VFD not enabled in this build)");
        }
        catch (NoSuchMethodException e) {
            throw new HDF5LibraryException(
                "H5Pget_fapl_ros3 not available (ros3 VFD not enabled in this build)");
        }
        catch (java.lang.reflect.InvocationTargetException e) {
            Throwable cause = e.getCause();
            if (cause instanceof HDF5LibraryException) {
                throw (HDF5LibraryException)cause;
            }
            throw new HDF5LibraryException("H5Pget_fapl_ros3 failed: " + e.getMessage());
        }
        catch (Exception e) {
            throw new HDF5LibraryException("H5Pget_fapl_ros3 failed: " + e.getMessage());
        }
    }

    // /////// unimplemented ////////

    // Generic property list routines //
    // herr_t H5Pencode(hid_t plist_id, void *buf, size_t *nalloc);
    // hid_t  H5Pdecode(const void *buf);

    // Object creation property list (OCPL) routines //

    // File creation property list (FCPL) routines //

    // File access property list (FAPL) routines //
    // herr_t H5Pset_driver(hid_t plist_id, hid_t new_driver_id, const void *new_driver_info)
    // const void *H5Pget_driver_info(hid_t plist_id)
    // herr_t H5Pget_multi_type(hid_t fapl_id, H5FD_mem_t *type)
    // herr_t H5Pset_multi_type(hid_t fapl_id, H5FD_mem_t type)
    // herr_t H5Pget_file_image(hid_t fapl_id, void **buf_ptr_ptr, size_t *buf_len_ptr);
    // herr_t H5Pset_file_image(hid_t fapl_id, void *buf_ptr, size_t buf_len);
    // herr_t H5Pget_file_image_callbacks(hid_t fapl_id, H5FD_file_image_callbacks_t *callbacks_ptr);
    // herr_t H5Pset_file_image_callbacks(hid_t fapl_id, H5FD_file_image_callbacks_t *callbacks_ptr);
    // herr_t H5Pset_core_write_tracking(hid_t fapl_id, hbool_t is_enabled, size_t page_size);
    // herr_t H5Pget_core_write_tracking(hid_t fapl_id, hbool_t *is_enabled, size_t *page_size);
    // #ifdef H5_HAVE_PARALLEL
    // herr_t H5Pset_all_coll_metadata_ops(hid_t accpl_id, hbool_t is_collective);
    //  herr_t H5Pget_all_coll_metadata_ops(hid_t plist_id, hbool_t *is_collective);
    // herr_t H5Pset_coll_metadata_write(hid_t fapl_id, hbool_t is_collective);
    // herr_t H5Pget_coll_metadata_write(hid_t fapl_id, hbool_t *is_collective);
    // #endif /* H5_HAVE_PARALLEL */
    //  herr_t H5Pset_mdc_image_config(hid_t plist_id, H5AC_cache_image_config_t *config_ptr);
    //  herr_t H5Pget_mdc_image_config(hid_t plist_id, H5AC_cache_image_config_t *config_ptr /*out*/);
    //  herr_t H5Pset_page_buffer_size(hid_t plist_id, size_t buf_size, unsigned min_meta_per, unsigned
    //  min_raw_per);
    // herr_t H5Pget_page_buffer_size(hid_t fapl_id, size_t *buf_size, unsigned *min_meta_perc, unsigned
    // *min_raw_perc); herr_t H5Pset_object_flush_cb (hid_t fapl_id, H5F_flush_cb_t func, void *user_data);
    // herr_t H5Pget_object_flush_cb (hid_t fapl_id, H5F_flush_cb_t *func, void **user_data);

    // Dataset creation property list (DCPL) routines //

    // Dataset access property list (DAPL) routines //
    // herr_t H5Pset_append_flush (hid_t dapl_id, int ndims, const hsize_t boundary[], H5D_append_cb_t func,
    // void *user_data); herr_t H5Pget_append_flush(hid_t dapl_id, int ndims, hsize_t boundary[],
    // H5D_append_cb_t *func, void **user_data)

    // Dataset xfer property list (DXPL) routines //
    // herr_t H5Pset_buffer(hid_t plist_id, size_t size, void *tconv, void *bkg);
    // herr_t H5Pset_preserve(hid_t plist_id, hbool_t status);
    // int H5Pget_preserve(hid_t plist_id);
    // herr_t H5Pset_filter_callback(hid_t plist, H5Z_filter_func_t func, void *op_data)
    // herr_t H5Pget_vlen_mem_manager(hid_t plist, H5MM_allocate_t *alloc, void **alloc_info, H5MM_free_t
    // *free, void
    // **free_info )
    // herr_t H5Pset_vlen_mem_manager(hid_t plist, H5MM_allocate_t alloc, void *alloc_info, H5MM_free_t free,
    // void *free_info ) herr_t H5Pget_type_conv_cb(hid_t plist, H5T_conv_except_func_t *func, void **op_data)
    // herr_t H5Pset_type_conv_cb( hid_t plist, H5T_conv_except_func_t func, void *op_data)
    // #ifdef H5_HAVE_PARALLEL
    //  herr_t H5Pget_mpio_actual_chunk_opt_mode(hid_t plist_id, H5D_mpio_actual_chunk_opt_mode_t
    //  *actual_chunk_opt_mode); herr_t H5Pget_mpio_actual_io_mode(hid_t plist_id, H5D_mpio_actual_io_mode_t
    //  *actual_io_mode); herr_t H5Pget_mpio_no_collective_cause(hid_t plist_id, uint32_t
    //  *local_no_collective_cause, uint32_t *global_no_collective_cause);
    // #endif /* H5_HAVE_PARALLEL */

    // Link creation property list (LCPL) routines //

    // Group creation property list (GCPL) routines //

    // String creation property list (STRCPL) routines //

    // Link access property list (LAPL) routines //
    // herr_t H5Pget_elink_cb( hid_t lapl_id, H5L_elink_traverse_t *func, void **op_data )
    // herr_t H5Pset_elink_cb( hid_t lapl_id, H5L_elink_traverse_t func, void *op_data )

    // Object copy property list (OCPYPL) routines //
    // herr_t H5Padd_merge_committed_dtype_path(hid_t plist_id, const char *path);
    // herr_t H5Pfree_merge_committed_dtype_paths(hid_t plist_id);
    // herr_t H5Pget_mcdt_search_cb(hid_t plist_id, H5O_mcdt_search_cb_t *func, void **op_data);
    // herr_t H5Pset_mcdt_search_cb(hid_t plist_id, H5O_mcdt_search_cb_t func, void *op_data);

    // ////////////////////////////////////////////////////////////
    // //
    // H5PL: HDF5 1.8 Plugin API Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5PL Java Plugin (H5PL) Interface
     *
     * @see H5PL, C-API
     *
     * @see @ref H5PL_UG, User Guide
     **/

    /**
     * @ingroup JH5PL
     *
     * H5PLset_loading_state uses one argument to enable or disable individual plugins.
     *        The plugin_flags parameter is an encoded integer in which each bit controls a specific
     *        plugin or class of plugins.
     *        A plugin bit set to 0 (zero) prevents the use of the dynamic plugin corresponding
     *        to that bit position. A plugin bit set to 1 (one) allows the use of that dynamic plugin.
     *        All dynamic plugins can be enabled by setting plugin_flags to a negative value.
     *        A value of 0 (zero) will disable all dynamic plugins.
     *
     *        H5PLset_loading_state inspects the HDF5_PLUGIN_PRELOAD environment variable every
     *        time it is called. If the environment variable is set to the special :: string,
     *        all dynamic plugins will be disabled.
     *
     * @param plugin_flags
     *            IN: The list of dynamic plugin types to enable or disable.
     *                A plugin bit set to 0 (zero) prevents use of that dynamic plugin.
     *                A plugin bit set to 1 (one) enables use of that dynamic plugin.
     *                Setting plugin_flags to a negative value enables all dynamic plugins.
     *                Setting plugin_flags to 0 (zero) disables all dynamic plugins.
     *
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5PLset_loading_state(int plugin_flags) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5PLset_loading_state(plugin_flags);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5PL
     *
     * H5PLget_loading_state retrieves the state of the dynamic plugins flag, plugin_flags..
     *
     * @return the list of dynamic plugin types that are enabled or disabled.
     *             A plugin bit set to 0 (zero) indicates that that dynamic plugin is disabled.
     *             A plugin bit set to 1 (one) indicates that that dynamic plugin is enabled.
     *             If the value of plugin_flags is negative, all dynamic plugins are enabled.
     *             If the value of plugin_flags is 0 (zero), all dynamic plugins are disabled.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5PLget_loading_state() throws HDF5LibraryException
    {
        int plugin_flags = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flags_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5PLget_loading_state(flags_segment) < 0)
                h5libraryError();
            plugin_flags = flags_segment.get(ValueLayout.JAVA_INT, 0);
        }
        if (plugin_flags < 0) {
            h5libraryError();
        }
        return plugin_flags;
    }

    /**
     * @ingroup JH5PL
     *
     * H5PLappend inserts the plugin path at the end of the table.
     *
     * @param plugin_path
     *            IN: Path for location of filter plugin libraries.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            plugin_path is null.
     **/
    public static void H5PLappend(String plugin_path) throws HDF5LibraryException, NullPointerException
    {
        if (plugin_path == null) {
            throw new NullPointerException("plugin_path cannot be null");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment path_segment = arena.allocateFrom(plugin_path);
            retVal                     = org.hdfgroup.javahdf5.hdf5_h.H5PLappend(path_segment);
            if (retVal < 0) {
                h5libraryError();
            }
        }
    }

    /**
     * @ingroup JH5PL
     *
     * H5PLprepend inserts the plugin path at the beginning of the table.
     *
     * @param plugin_path
     *            IN: Path for location of filter plugin libraries.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            plugin_path is null.
     **/
    public static void H5PLprepend(String plugin_path) throws HDF5LibraryException, NullPointerException
    {
        if (plugin_path == null) {
            throw new NullPointerException("plugin_path cannot be null");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment path_segment = arena.allocateFrom(plugin_path);
            retVal                     = org.hdfgroup.javahdf5.hdf5_h.H5PLprepend(path_segment);
            if (retVal < 0) {
                h5libraryError();
            }
        }
    }

    /**
     * @ingroup JH5PL
     *
     * H5PLreplace replaces the plugin path at the specified index.
     *
     * @param plugin_path
     *            IN: Path for location of filter plugin libraries.
     * @param index
     *            IN: The table index (0-based).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            plugin_path is null.
     **/
    public static void H5PLreplace(String plugin_path, int index)
        throws HDF5LibraryException, NullPointerException
    {
        if (plugin_path == null) {
            throw new NullPointerException("plugin_path cannot be null");
        }
        if (index < 0) {
            throw new HDF5FunctionArgumentException("index must be non-negative");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment path_segment = arena.allocateFrom(plugin_path);
            retVal                     = org.hdfgroup.javahdf5.hdf5_h.H5PLreplace(path_segment, index);
            if (retVal < 0) {
                h5libraryError();
            }
        }
    }

    /**
     * @ingroup JH5PL
     *
     * H5PLinsert inserts the plugin path at the specified index.
     *
     * @param plugin_path
     *            IN: Path for location of filter plugin libraries.
     * @param index
     *            IN: The table index (0-based).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            plugin_path is null.
     **/
    public static void H5PLinsert(String plugin_path, int index)
        throws HDF5LibraryException, NullPointerException
    {
        if (plugin_path == null) {
            throw new NullPointerException("plugin_path cannot be null");
        }
        if (index < 0) {
            throw new HDF5FunctionArgumentException("index must be non-negative");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment path_segment = arena.allocateFrom(plugin_path);
            retVal                     = org.hdfgroup.javahdf5.hdf5_h.H5PLinsert(path_segment, index);
            if (retVal < 0) {
                h5libraryError();
            }
        }
    }

    /**
     * @ingroup JH5PL
     *
     * H5PLremove removes the plugin path at the specified index.
     *
     * @param index
     *            IN: The table index (0-based).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5PLremove(int index) throws HDF5LibraryException
    {
        if (index < 0) {
            throw new HDF5FunctionArgumentException("index must be non-negative");
        }

        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5PLremove(index);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5PL
     *
     * H5PLget retrieves the plugin path at the specified index.
     *
     * @param index
     *            IN: The table index (0-based).
     *
     * @return the current path at the index in plugin path table
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5PLget(int index) throws HDF5LibraryException
    {
        long buf_size = -1;

        if (index < 0) {
            throw new HDF5FunctionArgumentException("index must be non-negative");
        }

        if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5PLget(index, MemorySegment.NULL, 0)) < 0)
            h5libraryError();

        String plugin_path = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment path_segment = arena.allocate(buf_size + 1);
            /* Get the attribute name */
            if (org.hdfgroup.javahdf5.hdf5_h.H5PLget(index, path_segment, buf_size + 1) < 0)
                h5libraryError();

            plugin_path = path_segment.getString(0, StandardCharsets.UTF_8);
        }
        return plugin_path;
    }

    /**
     * @ingroup JH5PL
     *
     * H5PLsize retrieves the size of the current list of plugin paths.
     *
     * @return the current number of paths in the plugin path table
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5PLsize() throws HDF5LibraryException
    {
        int size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5PLsize(size_segment) < 0)
                h5libraryError();
            size = size_segment.get(ValueLayout.JAVA_INT, 0);
        }
        if (size < 0) {
            h5libraryError();
        }
        return size;
    }

    // ////////////////////////////////////////////////////////////
    // //
    // H5R: HDF5 1.8 Reference API Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * @defgroup JH5R Java Reference (H5R) Interface
     *
     * @see H5R, C-API
     *
     * @see @ref H5R_UG, User Guide
     * @deprecated As of HDF5 1.12.0 in favor of H5Rcreate_object(), H5Rcreate_region() and H5Rcreate_attr()
     **/
    @Deprecated
    private static int H5Rcreate(byte[] ref, long loc_id, String name, int ref_type, long space_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (ref == null) {
            throw new NullPointerException("ref is null");
        }
        if ((ref_type == org.hdfgroup.javahdf5.hdf5_h.H5R_OBJECT() &&
             ref.length != org.hdfgroup.javahdf5.hdf5_h.H5R_OBJ_REF_BUF_SIZE()) ||
            (ref_type == org.hdfgroup.javahdf5.hdf5_h.H5R_DATASET_REGION() &&
             ref.length != org.hdfgroup.javahdf5.hdf5_h.H5R_DSET_REG_REF_BUF_SIZE())) {
            throw new HDF5FunctionArgumentException("ref length is invalid");
        }

        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            MemorySegment ref_segment  = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Rcreate(ref_segment, loc_id, name_segment, ref_type,
                                                                 space_id)) < 0)
                h5libraryError();
            // Read the data from the memory segment
            for (int i = 0; i < ref.length; i++) {
                ref[i] = ref_segment.get(ValueLayout.JAVA_BYTE, i);
            }
        }
        return retVal;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rcreate creates the reference, ref, of the type specified in ref_type, pointing to the object name
     * located at loc_id.
     *
     * @param loc_id
     *            IN: Location identifier used to locate the object being pointed to.
     * @param name
     *            IN: Name of object at location loc_id.
     * @param ref_type
     *            IN: Type of reference.
     * @param space_id
     *            IN: Dataspace identifier with selection.
     *
     * @return the reference (byte[]) if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     *
     * @deprecated As of HDF5 1.12.0 in favor of H5Rcreate_object(), H5Rcreate_region() and H5Rcreate_attr()
     **/
    @Deprecated
    public static byte[] H5Rcreate(long loc_id, String name, int ref_type, long space_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        byte rbuf[] = null;
        if (ref_type == org.hdfgroup.javahdf5.hdf5_h.H5R_DATASET_REGION())
            rbuf = new byte[(int)org.hdfgroup.javahdf5.hdf5_h.H5R_DSET_REG_REF_BUF_SIZE()];
        else if (ref_type == org.hdfgroup.javahdf5.hdf5_h.H5R_OBJECT())
            rbuf = new byte[(int)org.hdfgroup.javahdf5.hdf5_h.H5R_OBJ_REF_BUF_SIZE()];
        else
            throw new HDF5FunctionArgumentException("Invalid ref_type");

        /* will raise an exception if fails */
        hdf.hdf5lib.H5.H5Rcreate(rbuf, loc_id, name, ref_type, space_id);

        return rbuf;
    }

    /**
     * @ingroup JH5R
     *
     * Given a reference to some object, H5Rdereference opens that object and return an identifier.
     *
     * @param dataset
     *            IN: Dataset containing reference object.
     * @param access_list
     *            IN: Property list of the object being referenced.
     * @param ref_type
     *            IN: The reference type of ref.
     * @param ref
     *            IN: reference to an object
     *
     * @return valid identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            output array is null.
     * @exception HDF5FunctionArgumentException
     *            output array is invalid.
     *
     * @deprecated As of HDF5 1.12.0 in favor of H5Rcreate_object(), H5Rcreate_region() and H5Rcreate_attr()
     **/
    @Deprecated
    public static long H5Rdereference(long dataset, long access_list, int ref_type, byte[] ref)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref == null) {
            throw new NullPointerException("ref is null");
        }
        long id = H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Rdereference2(dataset, access_list, ref_type, ref_segment);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Rdereference add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget_name retrieves a name for the object identified by ref.
     *
     * @param loc_id
     *            IN: Identifier for the dataset containing the reference or for the group that dataset is in.
     * @param ref_type
     *            IN: Type of reference.
     * @param ref
     *            IN: An object or dataset region reference.
     * @param name
     *            OUT: A name associated with the referenced object or dataset region.
     * @param size
     *            IN: The size of the name buffer.
     *
     * @return Returns the length of the name if successful, returning 0 (zero) if no name is associated with
     *         the identifier. Otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     * @exception HDF5FunctionArgumentException
     *            Argument is illegal.
     **/
    public static long H5Rget_name(long loc_id, int ref_type, byte[] ref, String[] name, long size)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (name == null || size <= 0) {
            throw new NullPointerException("name is null or size is invalid");
        }
        if (ref == null) {
            throw new NullPointerException("ref is null");
        }
        if (ref.length != org.hdfgroup.javahdf5.hdf5_h.H5R_OBJ_REF_BUF_SIZE() &&
            ref.length != org.hdfgroup.javahdf5.hdf5_h.H5R_DSET_REG_REF_BUF_SIZE()) {
            throw new HDF5FunctionArgumentException("ref length is invalid");
        }

        long retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_segment  = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref);
            MemorySegment name_segment = arena.allocateFrom(name[0]);
            retVal =
                org.hdfgroup.javahdf5.hdf5_h.H5Rget_name(loc_id, ref_type, ref_segment, name_segment, size);
            if (retVal < 0)
                h5libraryError();
            // Read the data from the memory segment
            name[0] = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return retVal;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget_name_string retrieves a name for the object identified by ref.
     *
     * @param loc_id
     *            IN: Identifier for the dataset containing the reference or for the group that dataset is in.
     * @param ref_type
     *            IN: Type of reference.
     * @param ref
     *            IN: An object or dataset region reference.
     *
     * @return Returns the name if successful, returning null if no name is associated with
     *         the identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            size is null.
     * @exception HDF5FunctionArgumentException
     *            Argument is illegal.
     **/
    public static String H5Rget_name_string(long loc_id, int ref_type, byte[] ref)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref == null) {
            throw new NullPointerException("ref is null");
        }
        if (ref.length != org.hdfgroup.javahdf5.hdf5_h.H5R_OBJ_REF_BUF_SIZE() &&
            ref.length != org.hdfgroup.javahdf5.hdf5_h.H5R_DSET_REG_REF_BUF_SIZE()) {
            throw new HDF5FunctionArgumentException("ref length is invalid");
        }

        String name   = null;
        long buf_size = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_segment  = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref);
            MemorySegment size_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Rget_name(loc_id, ref_type, ref_segment,
                                                                     MemorySegment.NULL, 0)) < 0)
                h5libraryError();

            MemorySegment name_segment = arena.allocate(buf_size + 1); // +1 for null terminator
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rget_name(loc_id, ref_type, ref_segment, name_segment,
                                                         buf_size + 1) < 0)
                h5libraryError();
            // Read the data from the memory segment
            name = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return name;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget_obj_type Given a reference to an object ref, H5Rget_obj_type returns the type of the object
     * pointed to.
     *
     * @param loc_id
     *            IN: loc_id of the reference object.
     * @param ref_type
     *            IN: Type of reference to query.
     * @param ref
     *            IN: the reference
     *
     * @return Returns the object type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static int H5Rget_obj_type(long loc_id, int ref_type, byte ref[])
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref == null) {
            throw new NullPointerException("ref is null");
        }

        int obj_type = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_segment      = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref);
            MemorySegment obj_type_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rget_obj_type2(loc_id, ref_type, ref_segment,
                                                              obj_type_segment) < 0)
                h5libraryError();
            obj_type = obj_type_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return obj_type;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget_obj_type2 Retrieves the type of object that an object reference points to.
     *
     * @see public static int H5Rget_obj_type(int loc_id, int ref_type, byte ref[])
     **/
    private static int H5Rget_obj_type2(long loc_id, int ref_type, byte ref[], int[] obj_type)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref == null) {
            throw new NullPointerException("ref is null");
        }
        throw new HDF5LibraryException("H5Rget_obj_type2 not implemented");
    }

    /**
     * @ingroup JH5R
     *
     * Given a reference to an object ref, H5Rget_region creates a copy of the dataspace of the dataset
     * pointed to and defines a selection in the copy which is the region pointed to.
     *
     * @param loc_id
     *            IN: loc_id of the reference object.
     * @param ref_type
     *            IN: The reference type of ref.
     * @param ref
     *            OUT: the reference to the object and region
     *
     * @return a valid identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static long H5Rget_region(long loc_id, int ref_type, byte[] ref)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref == null) {
            throw new NullPointerException("ref is null");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Rget_region(loc_id, ref_type, ref_segment);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Rget_region add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    // ////////////////////////////////////////////////////////////
    // //
    // H5R: HDF5 1.12 Reference API Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * @ingroup JH5R
     *
     * H5Rcreate_object creates a reference pointing to the object named name located at loc id.
     *
     * @param loc_id
     *            IN: Location identifier used to locate the object being pointed to.
     * @param name
     *            IN: Name of object at location loc_id.
     * @param access_id
     *            IN: Object access identifier to the object being pointed to.
     *
     * @return the reference (byte[]) if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static byte[] H5Rcreate_object(long loc_id, String name, long access_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        byte rbuf[] = new byte[org.hdfgroup.javahdf5.hdf5_h.H5R_REF_BUF_SIZE()];

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment rbuf_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, rbuf);
            MemorySegment name_segment = arena.allocateFrom(name);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rcreate_object(loc_id, name_segment, access_id, rbuf_segment) <
                0)
                h5libraryError();
            // Read the data from the memory segment
            for (int i = 0; i < rbuf.length; i++) {
                rbuf[i] = rbuf_segment.get(ValueLayout.JAVA_BYTE, i);
            }
        }
        return rbuf;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rcreate_region creates the reference, pointing to the region represented by
     * space id within the object named name located at loc id.
     *
     * @param loc_id
     *            IN: Location identifier used to locate the object being pointed to.
     * @param name
     *            IN: Name of object at location loc_id.
     * @param space_id
     *            IN: Identifies the dataset region that a dataset region reference points to.
     * @param access_id
     *            IN: Object access identifier to the object being pointed to.
     *
     * @return the reference (byte[]) if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static byte[] H5Rcreate_region(long loc_id, String name, long space_id, long access_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }

        byte rbuf[] = new byte[org.hdfgroup.javahdf5.hdf5_h.H5R_REF_BUF_SIZE()];

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment rbuf_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, rbuf);
            MemorySegment name_segment = arena.allocateFrom(name);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rcreate_region(loc_id, name_segment, space_id, access_id,
                                                              rbuf_segment) < 0)
                h5libraryError();
            // Read the data from the memory segment
            for (int i = 0; i < rbuf.length; i++) {
                rbuf[i] = rbuf_segment.get(ValueLayout.JAVA_BYTE, i);
            }
        }
        return rbuf;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rcreate_attr creates the reference, pointing to the attribute named attr name
     * and attached to the object named name located at loc id.
     *
     * @param loc_id
     *            IN: Location identifier used to locate the object being pointed to.
     * @param name
     *            IN: Name of object at location loc_id.
     * @param attr_name
     *            IN: Name of the attribute within the object.
     * @param access_id
     *            IN: Object access identifier to the object being pointed to.
     *
     * @return the reference (byte[]) if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static byte[] H5Rcreate_attr(long loc_id, String name, String attr_name, long access_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (name == null || attr_name == null) {
            throw new NullPointerException("name or attr_name is null");
        }

        byte rbuf[] = new byte[org.hdfgroup.javahdf5.hdf5_h.H5R_REF_BUF_SIZE()];

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment rbuf_segment      = arena.allocateFrom(ValueLayout.JAVA_BYTE, rbuf);
            MemorySegment name_segment      = arena.allocateFrom(name);
            MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rcreate_attr(loc_id, name_segment, attr_name_segment,
                                                            access_id, rbuf_segment) < 0)
                h5libraryError();
            // Read the data from the memory segment
            for (int i = 0; i < rbuf.length; i++) {
                rbuf[i] = rbuf_segment.get(ValueLayout.JAVA_BYTE, i);
            }
        }
        return rbuf;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rdestroy destroys a reference and releases resources.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static void H5Rdestroy(byte[] ref_ptr)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }
        if (ref_ptr.length != org.hdfgroup.javahdf5.hdf5_h.H5R_REF_BUF_SIZE()) {
            throw new HDF5FunctionArgumentException("ref_ptr length is invalid");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rdestroy(ref_ptr_segment) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget_type retrieves the type of a reference.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     *
     * @return a valid reference type if successful; otherwise returns H5R UNKNOWN.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static int H5Rget_type(byte[] ref_ptr)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }

        int ref_type = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            if ((ref_type = org.hdfgroup.javahdf5.hdf5_h.H5Rget_type(ref_ptr_segment)) < 0)
                h5libraryError();
        }
        return ref_type;
    }

    /**
     * @ingroup JH5R
     *
     * H5Requal determines whether two references point to the same object, region or attribute.
     *
     * @param ref1_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     * @param ref2_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     *
     * @return true if equal, else false
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static boolean H5Requal(byte[] ref1_ptr, byte[] ref2_ptr)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref1_ptr == null || ref2_ptr == null) {
            throw new NullPointerException("ref1_ptr or ref2_ptr is null");
        }

        boolean equal = false;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref1_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref1_ptr);
            MemorySegment ref2_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref2_ptr);
            int retVal                 = org.hdfgroup.javahdf5.hdf5_h.H5Requal(ref1_segment, ref2_segment);
            if (retVal < 0)
                h5libraryError();
            if (retVal > 0)
                equal = true;
            else
                equal = false;
        }
        return equal;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rcopy creates a copy of an existing reference.
     *
     * @param src_ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     *
     * @return a valid copy of the reference (byte[]) if successful.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static byte[] H5Rcopy(byte[] src_ref_ptr)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (src_ref_ptr == null) {
            throw new NullPointerException("src_ref_ptr is null");
        }
        if (src_ref_ptr.length != org.hdfgroup.javahdf5.hdf5_h.H5R_REF_BUF_SIZE()) {
            throw new HDF5FunctionArgumentException("src_ref_ptr length is invalid");
        }

        byte[] dest_ref_ptr = new byte[org.hdfgroup.javahdf5.hdf5_h.H5R_REF_BUF_SIZE()];
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment src_segment  = arena.allocateFrom(ValueLayout.JAVA_BYTE, src_ref_ptr);
            MemorySegment dest_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, dest_ref_ptr);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rcopy(src_segment, dest_segment) < 0)
                h5libraryError();
        }
        return dest_ref_ptr;
    }

    /**
     * @ingroup JH5R
     *
     * H5Ropen_object opens that object and returns an identifier.
     * The object opened with this function should be closed when it is no longer needed
     * so that resource leaks will not develop. Use the appropriate close function such
     * as H5Oclose or H5Dclose for datasets.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     * @param rapl_id
     *            IN: A reference access property list identifier for the reference. The access property
     *                list can be used to access external files that the reference points
     *                to (through a file access property list).
     * @param oapl_id
     *            IN: An object access property list identifier for the reference. The access property
     *                property list must be of the same type as the object being referenced,
     *                that is a group or dataset property list.
     *
     * @return a valid identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static long H5Ropen_object(byte[] ref_ptr, long rapl_id, long oapl_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Ropen_object(ref_ptr_segment, rapl_id, oapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Ropen_object add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5R
     *
     * H5Ropen region creates a copy of the dataspace of the dataset pointed to by a region reference,
     * ref ptr, and defines a selection matching the selection pointed to by ref ptr within the dataspace
     * copy. Use H5Sclose to release the dataspace identifier returned by this function when the identifier is
     * no longer needed.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     * @param rapl_id
     *            IN: A reference access property list identifier for the reference. The access property
     *                list can be used to access external files that the reference points
     *                to (through a file access property list).
     * @param oapl_id
     *            IN: An object access property list identifier for the reference. The access property
     *                property list must be of the same type as the object being referenced,
     *                that is a group or dataset property list.
     *
     * @return a valid dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static long H5Ropen_region(byte[] ref_ptr, long rapl_id, long oapl_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Ropen_region(ref_ptr_segment, rapl_id, oapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Ropen_region add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5R
     *
     * H5Ropen_attr opens the attribute attached to the object and returns an identifier.
     * The attribute opened with this function should be closed with H5Aclose when it is no longer needed
     * so that resource leaks will not develop.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     * @param rapl_id
     *            IN: A reference access property list identifier for the reference. The access property
     *                list can be used to access external files that the reference points
     *                to (through a file access property list).
     * @param aapl_id
     *            IN: An attribute access property list identifier for the reference. The access property
     *                property list must be of the same type as the object being referenced,
     *                that is a group or dataset property list.
     *
     * @return a valid attribute identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static long H5Ropen_attr(byte[] ref_ptr, long rapl_id, long aapl_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Ropen_attr(ref_ptr_segment, rapl_id, aapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Ropen_attr add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget obj type3 retrieves the type of the referenced object pointed to.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     * @param rapl_id
     *            IN: A reference access property list identifier for the reference. The access property
     *                list can be used to access external files that the reference points
     *                to (through a file access property list).
     *
     * @return Returns the object type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            array is null.
     * @exception HDF5FunctionArgumentException
     *            array is invalid.
     **/
    public static int H5Rget_obj_type3(byte[] ref_ptr, long rapl_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }
        int obj_type = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment  = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            MemorySegment obj_type_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rget_obj_type3(ref_ptr_segment, rapl_id, obj_type_segment) < 0)
                h5libraryError();
            obj_type = obj_type_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return obj_type;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget_file_name retrieves the file name for the object, region or attribute reference pointed to.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     *
     * @return Returns the file name of the reference
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            array is null.
     * @exception HDF5FunctionArgumentException
     *            array is invalid.
     **/
    public static String H5Rget_file_name(byte[] ref_ptr)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }
        String fileName = null;
        long buf_size   = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Rget_file_name(ref_ptr_segment, MemorySegment.NULL,
                                                                          0)) < 0)
                h5libraryError();

            MemorySegment name_segment = arena.allocate(buf_size + 1); // +1 for null terminator
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rget_file_name(ref_ptr_segment, name_segment, buf_size + 1) <
                0)
                h5libraryError();
            // Read the data from the memory segment
            fileName = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return fileName;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget_obj_name retrieves the object name for the object, region or attribute reference pointed to.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     * @param rapl_id
     *            IN: A reference access property list identifier for the reference. The access property
     *                list can be used to access external files that the reference points
     *                to (through a file access property list).
     *
     * @return Returns the object name of the reference
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            array is null.
     * @exception HDF5FunctionArgumentException
     *            array is invalid.
     **/
    public static String H5Rget_obj_name(byte[] ref_ptr, long rapl_id)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }
        String objName = null;
        long buf_size  = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Rget_obj_name(ref_ptr_segment, rapl_id,
                                                                         MemorySegment.NULL, 0)) < 0)
                h5libraryError();

            MemorySegment name_segment = arena.allocate(buf_size + 1); // +1 for null terminator
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rget_obj_name(ref_ptr_segment, rapl_id, name_segment,
                                                             buf_size + 1) < 0)
                h5libraryError();
            // Read the data from the memory segment
            objName = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return objName;
    }

    /**
     * @ingroup JH5R
     *
     * H5Rget_attr_name retrieves the attribute name for the object, region or attribute reference pointed to.
     *
     * @param ref_ptr
     *            IN: Reference to an object, region or attribute attached to an object.
     *
     * @return Returns the attribute name of the reference
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            array is null.
     * @exception HDF5FunctionArgumentException
     *            array is invalid.
     **/
    public static String H5Rget_attr_name(byte[] ref_ptr)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (ref_ptr == null) {
            throw new NullPointerException("ref_ptr is null");
        }
        String attrName = null;
        long buf_size   = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment ref_ptr_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, ref_ptr);
            if ((buf_size = org.hdfgroup.javahdf5.hdf5_h.H5Rget_attr_name(ref_ptr_segment, MemorySegment.NULL,
                                                                          0)) < 0)
                h5libraryError();

            MemorySegment name_segment = arena.allocate(buf_size + 1); // +1 for null terminator
            if (org.hdfgroup.javahdf5.hdf5_h.H5Rget_attr_name(ref_ptr_segment, name_segment, buf_size + 1) <
                0)
                h5libraryError();
            // Read the data from the memory segment
            attrName = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return attrName;
    }

    // ////////////////////////////////////////////////////////////
    // //
    // H5S: Dataspace Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5S Java Dataspace (H5S) Interface
     *
     * @see H5S, C-API
     *
     * @see @ref H5S_UG, User Guide
     **/

    /**
     * @defgroup JH5S Java Dataspace (H5S) Interface
     **/

    /**************** Operations on dataspaces ********************/

    /**
     * @ingroup JH5S
     *
     * H5Screate creates a new dataspace of a particular type.
     *
     * @param type
     *            IN: The type of dataspace to be created.
     *
     * @return a dataspace identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Screate(int type) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Screate(type);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Screate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5S
     *
     * H5Screate_simple creates a new simple data space and opens it for access.
     *
     * @param rank
     *            IN: Number of dimensions of dataspace.
     * @param dims
     *            IN: An array of the size of each dimension.
     * @param maxdims
     *            IN: An array of the maximum size of each dimension.
     *
     * @return a dataspace identifier
     *
     * @exception HDF5Exception
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            dims or maxdims is null.
     **/
    public static long H5Screate_simple(int rank, long[] dims, long[] maxdims)
        throws HDF5Exception, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (dims == null) {
            throw new NullPointerException("Null dims array");
        }
        if (rank < 0) {
            throw new HDF5FunctionArgumentException("Negative rank");
        }
        if (dims.length != rank) {
            throw new HDF5FunctionArgumentException("Invalid dims array");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dims_segment = arena.allocate(ValueLayout.JAVA_LONG, rank);
            MemorySegment.copy((Object)dims, 0, dims_segment, ValueLayout.JAVA_LONG, 0, rank);
            MemorySegment maxdims_segment = MemorySegment.NULL;
            if (maxdims != null) {
                maxdims_segment = arena.allocate(ValueLayout.JAVA_LONG, rank);
                MemorySegment.copy((Object)maxdims, 0, maxdims_segment, ValueLayout.JAVA_LONG, 0, rank);
            }
            id = org.hdfgroup.javahdf5.hdf5_h.H5Screate_simple(rank, dims_segment, maxdims_segment);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Screate_simple add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sset_extent_simple sets or resets the size of an existing dataspace.
     *
     * @param space_id
     *            Dataspace identifier.
     * @param rank
     *            Rank, or dimensionality, of the dataspace.
     * @param current_size
     *            Array containing current size of dataspace.
     * @param maximum_size
     *            Array containing maximum size of dataspace.
     *
     * @return a dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Sset_extent_simple(long space_id, int rank, long[] current_size, long[] maximum_size)
        throws HDF5LibraryException, NullPointerException
    {
        if (current_size == null || maximum_size == null) {
            throw new NullPointerException("current_size or maximum_size is null");
        }
        if (current_size.length != rank || maximum_size.length != rank) {
            throw new HDF5FunctionArgumentException("current_size or maximum_size length is invalid");
        }

        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment cs_segment   = arena.allocateFrom(ValueLayout.JAVA_LONG, current_size);
            MemorySegment maxs_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, maximum_size);
            if ((id = org.hdfgroup.javahdf5.hdf5_h.H5Sset_extent_simple(space_id, rank, cs_segment,
                                                                        maxs_segment)) < 0)
                h5libraryError();
        }
        return id;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sset_extent_simple sets or resets the size of an existing dataspace.
     *
     * @param space_id
     *            Dataspace identifier.
     * @param rank
     *            Rank, or dimensionality, of the dataspace.
     * @param current_size
     *            Array containing current size of dataspace.
     * @param maximum_size
     *            Array containing maximum size of dataspace.
     *
     * @return a dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Sset_extent_simple(long space_id, int rank, byte[] current_size, byte[] maximum_size)
        throws HDF5LibraryException, NullPointerException
    {
        ByteBuffer csbb   = ByteBuffer.wrap(current_size);
        long[] lacs       = (csbb.asLongBuffer()).array();
        ByteBuffer maxsbb = ByteBuffer.wrap(maximum_size);
        long[] lamaxs     = (maxsbb.asLongBuffer()).array();

        return hdf.hdf5lib.H5.H5Sset_extent_simple(space_id, rank, lacs, lamaxs);
    }

    /**
     * @ingroup JH5S
     *
     * H5Scopy creates a new dataspace which is an exact copy of the dataspace identified by space_id.
     *
     * @param space_id
     *            Identifier of dataspace to copy.
     * @return a dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Scopy(long space_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Scopy(space_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Scopy add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sclose releases a dataspace.
     *
     * @param space_id
     *            Identifier of dataspace to release.
     *
     * @return a non-negative value if successful
     **/
    public static int H5Sclose(long space_id)
    {
        log.trace("OPEN_IDS: H5Sclose remove {}", space_id);
        OPEN_IDS.remove(space_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Sclose(space_id);
        if (retVal < 0)
            h5libraryError();
        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sencode converts a data space description into binary form in a buffer.
     *
     * @param obj_id
     *            IN: Identifier of the object to be encoded.
     *
     * @return the buffer for the object to be encoded into.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static byte[] H5Sencode(long obj_id) throws HDF5LibraryException
    {
        byte[] buf = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a buffer for the size
            MemorySegment nalloc_segment = arena.allocate(ValueLayout.JAVA_LONG);
            org.hdfgroup.javahdf5.hdf5_h.H5Sencode2(obj_id, MemorySegment.NULL, nalloc_segment,
                                                    H5P_DEFAULT());
            long buf_size = nalloc_segment.get(ValueLayout.JAVA_LONG, 0);

            MemorySegment buf_segment = arena.allocate(buf_size);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Sencode2(obj_id, buf_segment, nalloc_segment, H5P_DEFAULT()) <
                0)
                h5libraryError();
            buf = buf_segment.toArray(ValueLayout.JAVA_BYTE);
        }
        if (buf == null || buf.length == 0) {
            throw new HDF5LibraryException("Failed to encode dataspace");
        }
        return buf;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sdecode reconstructs the HDF5 data space object and returns a new object handle for it.
     *
     * @param buf
     *            IN: Buffer for the data space object to be decoded.
     *
     * @return a new object handle
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static long H5Sdecode(byte[] buf) throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, buf);
            id                        = org.hdfgroup.javahdf5.hdf5_h.H5Sdecode(buf_segment);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Sdecode add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_simple_extent_npoints determines the number of elements in a dataspace.
     *
     * @param space_id
     *            ID of the dataspace object to query
     *
     * @return the number of elements in the dataspace if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Sget_simple_extent_npoints(long space_id) throws HDF5LibraryException
    {
        long npoints = org.hdfgroup.javahdf5.hdf5_h.H5Sget_simple_extent_npoints(space_id);
        if (npoints < 0) {
            h5libraryError();
        }
        return npoints;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_simple_extent_ndims determines the dimensionality (or rank) of a dataspace.
     *
     * @param space_id
     *            IN: Identifier of the dataspace
     *
     * @return the number of dimensions in the dataspace if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Sget_simple_extent_ndims(long space_id) throws HDF5LibraryException
    {
        int ndims = org.hdfgroup.javahdf5.hdf5_h.H5Sget_simple_extent_ndims(space_id);
        if (ndims < 0) {
            h5libraryError();
        }
        return ndims;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_simple_extent_dims returns the size and maximum sizes of each dimension of a dataspace through
     * the dims and maxdims parameters.
     *
     * @param space_id
     *            IN: Identifier of the dataspace object to query
     * @param dims
     *            OUT: Pointer to array to store the size of each dimension.
     * @param maxdims
     *            OUT: Pointer to array to store the maximum size of each dimension.
     *
     * @return the number of dimensions in the dataspace if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            dims or maxdims is null.
     **/
    public static int H5Sget_simple_extent_dims(long space_id, long[] dims, long[] maxdims)
        throws HDF5LibraryException, NullPointerException
    {
        if (dims != null && dims.length < 0) {
            throw new NullPointerException("dims length is invalid");
        }
        if (maxdims != null && maxdims.length < 0) {
            throw new NullPointerException("maxdims length is invalid");
        }
        int ndims = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dims_segment = MemorySegment.NULL;
            if (dims != null)
                dims_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
            MemorySegment maxdims_segment = MemorySegment.NULL;
            if (maxdims != null)
                maxdims_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, maxdims);
            if ((ndims = org.hdfgroup.javahdf5.hdf5_h.H5Sget_simple_extent_dims(space_id, dims_segment,
                                                                                maxdims_segment)) < 0)
                h5libraryError();
        }

        return ndims;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sis_simple determines whether a dataspace is a simple dataspace.
     *
     * @param space_id
     *            Identifier of the dataspace to query
     *
     * @return true if is a simple dataspace
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Sis_simple(long space_id) throws HDF5LibraryException
    {
        boolean isSimple = false;
        int status       = org.hdfgroup.javahdf5.hdf5_h.H5Sis_simple(space_id);
        if (status < 0) {
            h5libraryError();
        }
        isSimple = (status > 0);
        return isSimple;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_simple_extent_type queries a dataspace to determine the current class of a dataspace.
     *
     * @param space_id
     *            Dataspace identifier.
     *
     * @return a dataspace class name if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Sget_simple_extent_type(long space_id) throws HDF5LibraryException
    {
        int type = org.hdfgroup.javahdf5.hdf5_h.H5Sget_simple_extent_type(space_id);
        if (type < 0) {
            h5libraryError();
        }
        return type;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sset_extent_none removes the extent from a dataspace and sets the type to H5S_NONE.
     *
     * @param space_id
     *            The identifier for the dataspace from which the extent is to be removed.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Sset_extent_none(long space_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Sset_extent_none(space_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sextent_copy copies the extent from source_space_id to dest_space_id. This action may change the type
     * of the dataspace.
     *
     * @param dest_space_id
     *            IN: The identifier for the dataspace from which the extent is copied.
     * @param source_space_id
     *            IN: The identifier for the dataspace to which the extent is copied.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Sextent_copy(long dest_space_id, long source_space_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Sextent_copy(dest_space_id, source_space_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sextent_equal determines whether the dataspace extents of two dataspaces, space1_id and space2_id,
     * are equal.
     *
     * @param first_space_id
     *            IN: The identifier for the first dataspace.
     * @param second_space_id
     *            IN: The identifier for the seconddataspace.
     *
     * @return true if successful, else false
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Sextent_equal(long first_space_id, long second_space_id)
        throws HDF5LibraryException
    {
        boolean isEqual = false;
        int status      = org.hdfgroup.javahdf5.hdf5_h.H5Sextent_equal(first_space_id, second_space_id);
        if (status < 0) {
            h5libraryError();
        }
        isEqual = (status > 0);
        return isEqual;
    }

    /***************** Operations on dataspace selections *****************/

    /**
     * @ingroup JH5S
     *
     * H5Sget_select_type retrieves the type of selection currently defined for the dataspace space_id.
     *
     * @param space_id
     *            IN: Identifier of the dataspace object to query
     *
     * @return the dataspace selection type if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Sget_select_type(long space_id) throws HDF5LibraryException
    {
        int type = org.hdfgroup.javahdf5.hdf5_h.H5Sget_select_type(space_id);
        if (type < 0) {
            h5libraryError();
        }
        return type;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_select_npoints determines the number of elements in the current selection of a dataspace.
     *
     * @param space_id
     *            IN: Identifier of the dataspace object to query
     *
     * @return the number of elements in the selection if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Sget_select_npoints(long space_id) throws HDF5LibraryException
    {
        long npoints = org.hdfgroup.javahdf5.hdf5_h.H5Sget_select_npoints(space_id);
        if (npoints < 0) {
            h5libraryError();
        }
        return npoints;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_copy copies all the selection information (including offset) from the source
     * dataspace to the destination dataspace.
     *
     * @param dst_id
     *            ID of the destination dataspace
     * @param src_id
     *            ID of the source dataspace
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Sselect_copy(long dst_id, long src_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Sselect_copy(dst_id, src_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_valid verifies that the selection for the dataspace.
     *
     * @param space_id
     *            The identifier for the dataspace in which the selection is being reset.
     *
     * @return true if the selection is contained within the extent and FALSE if it is not or is an error.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Sselect_valid(long space_id) throws HDF5LibraryException
    {
        boolean isValid = false;
        int status      = org.hdfgroup.javahdf5.hdf5_h.H5Sselect_valid(space_id);
        if (status < 0) {
            h5libraryError();
        }
        isValid = (status > 0);
        return isValid;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_adjust moves a selection by subtracting an offset from it.
     *
     * @param space_id
     *            ID of dataspace to adjust
     * @param offset
     *            Offset to subtract
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            offset is null.
     **/
    public static void H5Sselect_adjust(long space_id, long[][] offset)
        throws HDF5LibraryException, NullPointerException
    {
        if (offset == null) {
            throw new NullPointerException("offset is null");
        }
        if (offset.length == 0 || offset[0].length == 0) {
            throw new HDF5FunctionArgumentException("offset array is empty");
        }
        int rank = -1;
        if ((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
            h5libraryError();
        if (offset.length != rank)
            throw new HDF5FunctionArgumentException("offset array rank does not match rank");

        try (Arena arena = Arena.ofConfined()) {
            SequenceLayout offset_layout = MemoryLayout.sequenceLayout(
                offset.length, MemoryLayout.sequenceLayout(offset[0].length, ValueLayout.JAVA_LONG));
            MemorySegment offset_segment = arena.allocate(offset_layout);
            for (int i = 0; i < offset.length; i++) {
                MemorySegment row_segment =
                    offset_segment.asSlice(i * offset[0].length * Long.BYTES, offset[0].length * Long.BYTES);
                for (int j = 0; j < offset[i].length; j++) {
                    row_segment.set(ValueLayout.JAVA_LONG, j * Long.BYTES, offset[i][j]);
                }
            }
            if (org.hdfgroup.javahdf5.hdf5_h.H5Sselect_adjust(space_id, offset_segment) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_select_bounds retrieves the coordinates of the bounding box containing the current selection and
     * places them into user-supplied buffers. <p> The start and end buffers must be large enough to hold the
     * dataspace rank number of coordinates.
     *
     * @param space_id
     *            Identifier of dataspace to release.
     * @param start
     *            coordinates of lowest corner of bounding box.
     * @param end
     *            coordinates of highest corner of bounding box.
     *
     * @return a non-negative value if successful,with start and end initialized.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            start or end is null.
     **/
    public static int H5Sget_select_bounds(long space_id, long[] start, long[] end)
        throws HDF5LibraryException, NullPointerException
    {
        if (start == null || end == null) {
            throw new NullPointerException("Negative ID or null array");
        }
        if (start.length != end.length) {
            throw new HDF5FunctionArgumentException("start and end length is invalid");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment start_segment = arena.allocate(ValueLayout.JAVA_LONG, start.length);
            MemorySegment end_segment   = arena.allocate(ValueLayout.JAVA_LONG, end.length);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Sget_select_bounds(space_id, start_segment,
                                                                            end_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(start_segment, ValueLayout.JAVA_LONG, 0L, start, 0, start.length);
            MemorySegment.copy(end_segment, ValueLayout.JAVA_LONG, 0L, end, 0, end.length);
        }
        return status;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_shape_same checks to see if the current selection in the dataspaces are the same
     * dimensionality and shape.
     * This is primarily used for reading the entire selection in one swoop.
     *
     * @param   space1_id
     *            ID of 1st Dataspace pointer to compare
     * @param   space2_id
     *            ID of 2nd Dataspace pointer to compare
     *
     * @return true if the selection is the same dimensionality and shape;
     *         false otherwise
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Sselect_shape_same(long space1_id, long space2_id) throws HDF5LibraryException
    {
        boolean isSame = false;
        int status     = org.hdfgroup.javahdf5.hdf5_h.H5Sselect_shape_same(space1_id, space2_id);
        if (status < 0) {
            h5libraryError();
        }
        isSame = (status > 0);
        return isSame;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_intersect_block checks to see if the current selection in the
     * dataspace intersects with the block given.
     *
     * @param space_id
     *            ID of dataspace pointer to compare
     * @param start
     *            Starting coordinate of block
     * @param end
     *            Opposite ("ending") coordinate of block
     *
     * @return a TRUE  if the current selection in the dataspace intersects with the block given
     *           FALSE otherwise
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            offset is null.
     **/
    public static boolean H5Sselect_intersect_block(long space_id, long[] start, long[] end)
        throws HDF5LibraryException, NullPointerException
    {
        if (start == null || end == null) {
            throw new NullPointerException("start or end is null");
        }
        if (start.length != end.length) {
            throw new HDF5FunctionArgumentException("start and end length is invalid");
        }

        boolean isIntersect = false;
        int status          = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment start_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, start);
            MemorySegment end_segment   = arena.allocateFrom(ValueLayout.JAVA_LONG, end);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Sselect_intersect_block(space_id, start_segment,
                                                                                 end_segment)) < 0)
                h5libraryError();
        }
        isIntersect = (status > 0);
        return isIntersect;
    }

    /**
     * @ingroup JH5S
     *
     * H5Soffset_simple sets the offset of a simple dataspace space_id.
     *
     * @param space_id
     *            IN: The identifier for the dataspace object to reset.
     * @param offset
     *            IN: The offset at which to position the selection.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            offset array is null.
     **/
    public static int H5Soffset_simple(long space_id, byte[] offset)
        throws HDF5LibraryException, NullPointerException
    {
        if (offset == null) {
            throw new NullPointerException("offset array is null");
        }
        if (offset.length == 0) {
            throw new HDF5FunctionArgumentException("offset array is empty");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment offset_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, offset);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Soffset_simple(space_id, offset_segment)) < 0)
                h5libraryError();
        }

        return status;
    }

    /**
     * @ingroup JH5S
     *
     * H5Soffset_simple sets the offset of a simple dataspace space_id.
     *
     * @param space_id
     *            IN: The identifier for the dataspace object to reset.
     * @param offset
     *            IN: The offset at which to position the selection.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            offset array is null.
     **/
    public static int H5Soffset_simple(long space_id, long[] offset)
        throws HDF5Exception, NullPointerException
    {
        if (offset == null)
            throw new NullPointerException("offset array is null");

        HDFArray theArray = new HDFArray(offset);
        byte[] theArr     = theArray.byteify();

        int retVal = H5Soffset_simple(space_id, theArr);

        theArr   = null;
        theArray = null;
        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_all selects the entire extent of the dataspace space_id.
     *
     * @param space_id
     *            IN: The identifier of the dataspace to be selected.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Sselect_all(long space_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Sselect_all(space_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_none resets the selection region for the dataspace space_id to include no elements.
     *
     * @param space_id
     *            IN: The identifier of the dataspace to be reset.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Sselect_none(long space_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Sselect_none(space_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_elements selects array elements to be included in the selection for the space_id dataspace.
     *
     * @param space_id
     *            Identifier of the dataspace.
     * @param op
     *            operator specifying how the new selection is combined.
     * @param num_elements
     *            Number of elements to be selected.
     * @param coord
     *            A 2-dimensional array specifying the coordinates of the elements.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    private static int H5Sselect_elements(long space_id, int op, int num_elements, byte[] coord)
        throws HDF5LibraryException, NullPointerException
    {
        int retVal = -1;

        if (coord == null) {
            throw new NullPointerException("coord array is null");
        }
        if (coord.length == 0) {
            throw new HDF5FunctionArgumentException("coord array is empty");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment coord_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, coord);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Sselect_elements(space_id, op, (long)num_elements,
                                                                          coord_segment)) < 0)
                h5libraryError();
        }

        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_elements selects array elements to be included in the selection for the space_id dataspace.
     *
     * @param space_id
     *            Identifier of the dataspace.
     * @param op
     *            operator specifying how the new selection is combined.
     * @param num_elements
     *            Number of elements to be selected.
     * @param coord2D
     *            A 2-dimensional array specifying the coordinates of the elements.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5Exception
     *            Error in the data conversion
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            cord array is
     **/
    public static int H5Sselect_elements(long space_id, int op, int num_elements, long[][] coord2D)
        throws HDF5Exception, HDF5LibraryException, NullPointerException
    {
        if (coord2D == null)
            throw new NullPointerException("coord2D array is null");
        if (coord2D.length != num_elements)
            throw new HDF5FunctionArgumentException("coord2D length does not match num_elements");
        int rows       = coord2D.length;
        int cols       = coord2D[0].length;
        int totalLongs = rows * cols;
        // Create a 1D long array to hold the flattened data.
        long[] flattenedArray = new long[totalLongs];
        int index             = 0;
        for (int i = 0; i < rows; i++) {
            System.arraycopy(coord2D[i], 0, flattenedArray, index, cols);
            index += cols;
        }
        ByteBuffer byteBuffer = ByteBuffer.allocate(flattenedArray.length * Long.BYTES);
        byteBuffer.order(ByteOrder.nativeOrder());

        LongBuffer longBuffer = byteBuffer.asLongBuffer();
        longBuffer.put(flattenedArray);

        byte[] coord = byteBuffer.array();

        int retVal = H5Sselect_elements(space_id, op, num_elements, coord);
        coord      = null;

        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_select_elem_npoints returns the number of element points in the current dataspace selection.
     *
     * @param spaceid
     *            Identifier of dataspace to release.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Sget_select_elem_npoints(long spaceid) throws HDF5LibraryException
    {
        if (spaceid < 0) {
            throw new HDF5FunctionArgumentException("Negative ID");
        }
        long npoints = org.hdfgroup.javahdf5.hdf5_h.H5Sget_select_elem_npoints(spaceid);
        if (npoints < 0) {
            h5libraryError();
        }
        return npoints;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_select_elem_pointlist returns an array of of element points in the current dataspace selection.
     * The point coordinates have the same dimensionality (rank) as the dataspace they are located within, one
     * coordinate per point.
     *
     * @param spaceid
     *            Identifier of dataspace to release.
     * @param startpoint
     *            first point to retrieve
     * @param numpoints
     *            number of points to retrieve
     * @param buf
     *            returns points startblock to startblock+num-1, each points is <i>rank</i> longs.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static int H5Sget_select_elem_pointlist(long spaceid, long startpoint, long numpoints, long[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        if (spaceid < 0) {
            throw new HDF5FunctionArgumentException("Negative ID");
        }
        if (buf.length == 0) {
            throw new HDF5FunctionArgumentException("buf array is empty");
        }
        int rank = -1;
        if ((rank = H5Sget_simple_extent_ndims(spaceid)) < 0)
            h5libraryError();

        if (rank == 0)
            rank = 1;
        if (buf.length < (numpoints * rank))
            throw new HDF5FunctionArgumentException("buffer input array too small");
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_LONG, numpoints * rank);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Sget_select_elem_pointlist(
                     spaceid, startpoint, numpoints, buf_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_LONG, 0L, buf, 0, (int)numpoints * rank);
        }
        return status;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_hyperslab selects a hyperslab region to add to the current selected region for the dataspace
     * specified by space_id. The start, stride, count, and block arrays must be the same size as the rank of
     * the dataspace.
     *
     * @param space_id
     *            IN: Identifier of dataspace selection to modify
     * @param op
     *            IN: Operation to perform on current selection.
     * @param start
     *            IN: Offset of start of hyperslab
     * @param stride
     *            IN: Hyperslab stride.
     * @param count
     *            IN: Number of blocks included in hyperslab.
     * @param block
     *            IN: Size of block in hyperslab.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static int H5Sselect_hyperslab(long space_id, int op, byte[] start, byte[] stride, byte[] count,
                                          byte[] block)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        ByteBuffer startbb  = ByteBuffer.wrap(start);
        long[] lastart      = (startbb.asLongBuffer()).array();
        ByteBuffer stridebb = ByteBuffer.wrap(stride);
        long[] lastride     = (stridebb.asLongBuffer()).array();
        ByteBuffer countbb  = ByteBuffer.wrap(count);
        long[] lacount      = (countbb.asLongBuffer()).array();
        ByteBuffer blockbb  = ByteBuffer.wrap(block);
        long[] lablock      = (blockbb.asLongBuffer()).array();

        return H5Sselect_hyperslab(space_id, op, lastart, lastride, lacount, lablock);
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_hyperslab selects a hyperslab region to add to the current selected region for the dataspace
     * specified by space_id. The start, stride, count, and block arrays must be the same size as the rank of
     * the dataspace.
     *
     * @param space_id
     *            IN: Identifier of dataspace selection to modify
     * @param op
     *            IN: Operation to perform on current selection.
     * @param start
     *            IN: Offset of start of hyperslab
     * @param stride
     *            IN: Hyperslab stride.
     * @param count
     *            IN: Number of blocks included in hyperslab.
     * @param block
     *            IN: Size of block in hyperslab.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static int H5Sselect_hyperslab(long space_id, int op, long[] start, long[] stride, long[] count,
                                          long[] block)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (start == null || count == null) {
            throw new NullPointerException("start, stride, count, or block is null");
        }
        if (start.length != count.length) {
            throw new HDF5FunctionArgumentException("start, stride, count, and block length is invalid");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment start_segment  = arena.allocateFrom(ValueLayout.JAVA_LONG, start);
            MemorySegment count_segment  = arena.allocateFrom(ValueLayout.JAVA_LONG, count);
            MemorySegment stride_segment = MemorySegment.NULL;
            if (stride != null) {
                stride_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, stride);
            }
            MemorySegment block_segment = MemorySegment.NULL;
            if (block != null) {
                block_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, block);
            }
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Sselect_hyperslab(
                     space_id, op, start_segment, stride_segment, count_segment, block_segment)) < 0)
                h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5S
     *
     * H5Scombine_hyperslab combines a hyperslab selection with the current selection for a dataspace,
     * creating a new dataspace to return the generated selection.
     * If the current selection is not a hyperslab, it is freed and the hyperslab
     * parameters passed in are combined with the H5S_SEL_ALL hyperslab (ie. a
     * selection composing the entire current extent).  If STRIDE or BLOCK is
     * NULL, they are assumed to be set to all '1'.
     *
     * @param space_id
     *            IN: Dataspace ID of selection to use
     * @param op
     *            IN: Operation to perform on current selection.
     * @param start
     *            IN: Offset of start of hyperslab
     * @param stride
     *            IN: Hyperslab stride.
     * @param count
     *            IN: Number of blocks included in hyperslab.
     * @param block
     *            IN: Size of block in hyperslab.
     *
     * @return a dataspace ID on success / H5I_INVALID_HID on failure
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an input array is null.
     * @exception HDF5FunctionArgumentException
     *            an input array is invalid.
     **/
    public static long H5Scombine_hyperslab(long space_id, int op, long[] start, long[] stride, long[] count,
                                            long[] block)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (start == null || count == null) {
            throw new NullPointerException("start or count is null");
        }
        if (start.length != count.length) {
            throw new HDF5FunctionArgumentException("startand  count length is invalid");
        }
        if (stride != null && start.length != stride.length)
            throw new HDF5FunctionArgumentException("start and stride length is invalid");
        if (block != null && start.length != block.length)
            throw new HDF5FunctionArgumentException("start and block length is invalid");

        long retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment start_segment  = arena.allocateFrom(ValueLayout.JAVA_LONG, start);
            MemorySegment count_segment  = arena.allocateFrom(ValueLayout.JAVA_LONG, count);
            MemorySegment stride_segment = MemorySegment.NULL;
            if (stride != null) {
                stride_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, stride);
            }
            MemorySegment block_segment = MemorySegment.NULL;
            if (block != null) {
                block_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, block);
            }
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Scombine_hyperslab(
                space_id, op, start_segment, stride_segment, count_segment, block_segment);
            if (retVal < 0) {
                h5libraryError();
            }
        }
        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Smodify_select refine an existing hyperslab selection with an operation, using a second
     * hyperslab. The first selection is modified to contain the result of
     * space1 operated on by space2.
     *
     * @param space1_id
     *            ID of the destination dataspace
     * @param op
     *            Operation to perform on current selection.
     * @param space2_id
     *            ID of the source dataspace
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Smodify_select(long space1_id, int op, long space2_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Smodify_select(space1_id, op, space2_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5S
     *
     * H5Scombine_select combines two existing hyperslab selections with an operation, returning
     * a new dataspace with the resulting selection.  The dataspace extent from
     * space1 is copied for the dataspace extent of the newly created dataspace.
     *
     * @param space1_id
     *            ID of the first dataspace
     * @param op
     *            Operation to perform on current selection.
     * @param space2_id
     *            ID of the second dataspace
     *
     * @return a dataspace ID on success / H5I_INVALID_HID on failure
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Scombine_select(long space1_id, int op, long space2_id) throws HDF5LibraryException
    {
        long retVal = org.hdfgroup.javahdf5.hdf5_h.H5Scombine_select(space1_id, op, space2_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sis_regular_hyperslab retrieves a regular hyperslab selection for the dataspace specified
     * by space_id.
     *
     * @param space_id
     *            IN: Identifier of dataspace selection to query
     *
     * @return a TRUE/FALSE for hyperslab selection if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Sis_regular_hyperslab(long space_id) throws HDF5LibraryException
    {
        boolean isRegular = false;
        int status        = org.hdfgroup.javahdf5.hdf5_h.H5Sis_regular_hyperslab(space_id);
        if (status < 0) {
            h5libraryError();
        }
        isRegular = (status > 0);
        return isRegular;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_regular_hyperslab determines if a hyperslab selection is regular for the dataspace specified
     * by space_id. The start, stride, count, and block arrays must be the same size as the rank of the
     * dataspace.
     *
     * @param space_id
     *            IN: Identifier of dataspace selection to modify
     * @param start
     *            OUT: Offset of start of hyperslab
     * @param stride
     *            OUT: Hyperslab stride.
     * @param count
     *            OUT: Number of blocks included in hyperslab.
     * @param block
     *            OUT: Size of block in hyperslab.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            an output array is null.
     * @exception HDF5FunctionArgumentException
     *            an output array is invalid.
     **/
    public static void H5Sget_regular_hyperslab(long space_id, long[] start, long[] stride, long[] count,
                                                long[] block)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        int rank = -1;
        if ((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
            h5libraryError();

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment start_segment = MemorySegment.NULL;
            if (start != null)
                start_segment = arena.allocate(ValueLayout.JAVA_LONG, rank);
            MemorySegment count_segment = MemorySegment.NULL;
            if (count != null)
                count_segment = arena.allocate(ValueLayout.JAVA_LONG, rank);
            MemorySegment stride_segment = MemorySegment.NULL;
            if (stride != null)
                stride_segment = arena.allocate(ValueLayout.JAVA_LONG, rank);
            MemorySegment block_segment = MemorySegment.NULL;
            if (block != null)
                block_segment = arena.allocate(ValueLayout.JAVA_LONG, rank);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Sget_regular_hyperslab(space_id, start_segment, stride_segment,
                                                                      count_segment, block_segment) < 0)
                h5libraryError();
            MemorySegment.copy(start_segment, ValueLayout.JAVA_LONG, 0L, start, 0, start.length);
            MemorySegment.copy(count_segment, ValueLayout.JAVA_LONG, 0L, count, 0, count.length);
            MemorySegment.copy(stride_segment, ValueLayout.JAVA_LONG, 0L, stride, 0, stride.length);
            MemorySegment.copy(block_segment, ValueLayout.JAVA_LONG, 0L, block, 0, block.length);
        }
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_select_hyper_nblocks returns the number of hyperslab blocks in the current dataspace selection.
     *
     * @param spaceid
     *            Identifier of dataspace to release.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Sget_select_hyper_nblocks(long spaceid) throws HDF5LibraryException
    {
        if (spaceid < 0) {
            throw new HDF5FunctionArgumentException("Negative ID");
        }
        long nblocks = org.hdfgroup.javahdf5.hdf5_h.H5Sget_select_hyper_nblocks(spaceid);
        if (nblocks < 0) {
            h5libraryError();
        }
        return nblocks;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sget_select_hyper_blocklist returns an array of hyperslab blocks. The block coordinates have the same
     * dimensionality (rank) as the dataspace they are located within. The list of blocks is formatted as
     * follows:
     *
     * <pre>
     *    &lt;"start" coordinate&gt;, immediately followed by
     *    &lt;"opposite" corner coordinate&gt;, followed by
     *   the next "start" and "opposite" coordinates,
     *   etc.
     *   until all of the selected blocks have been listed.
     * </pre>
     *
     * @param spaceid
     *            Identifier of dataspace to release.
     * @param startblock
     *            first block to retrieve
     * @param numblocks
     *            number of blocks to retrieve
     * @param buf
     *            returns blocks startblock to startblock+num-1, each block is <i>rank</i> * 2 (corners)
     *            longs.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static int H5Sget_select_hyper_blocklist(long spaceid, long startblock, long numblocks, long[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        if (spaceid < 0) {
            throw new HDF5FunctionArgumentException("Negative ID or null buf");
        }
        if (buf.length == 0) {
            throw new HDF5FunctionArgumentException("buf array is empty");
        }
        int rank = -1;
        if ((rank = H5Sget_simple_extent_ndims(spaceid)) < 0)
            h5libraryError();
        if (rank == 0)
            rank = 1;
        if (buf.length < (numblocks * rank))
            throw new HDF5FunctionArgumentException("buffer input array too small");

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocate(ValueLayout.JAVA_LONG, numblocks * rank * 2);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Sget_select_hyper_blocklist(
                     spaceid, startblock, numblocks, buf_segment)) < 0)
                h5libraryError();
            MemorySegment.copy(buf_segment, ValueLayout.JAVA_LONG, 0L, buf, 0, (int)numblocks * rank * 2);
        }
        return status;
    }

    /**
     * @ingroup JH5S
     *
     * H5Sselect_project_intersection projects the intersection of the selections of src_space_id and
     * src_intersect_space_id within the selection of src_space_id as a
     * selection within the selection of dst_space_id.
     *
     * @param src_space_id
     *            Selection that is mapped to dst_space_id, and intersected with src_intersect_space_id
     * @param dst_space_id
     *            Selection that is mapped to src_space_id
     * @param src_intersect_space_id
     *            Selection whose intersection with src_space_id is projected to dst_space_id to obtain the
     *            result
     *
     * @return a dataspace with a selection equal to the intersection of
     *         src_intersect_space_id and src_space_id projected from src_space to dst_space on success
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Sselect_project_intersection(long src_space_id, long dst_space_id,
                                                      long src_intersect_space_id) throws HDF5LibraryException
    {
        long retVal = org.hdfgroup.javahdf5.hdf5_h.H5Sselect_project_intersection(src_space_id, dst_space_id,
                                                                                  src_intersect_space_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    // /////// unimplemented ////////
    ///// Operations on dataspace selections /////

    //
    ///// Operations on dataspace selection iterators /////
    // public  static      H5Ssel_iter_create(hid_t spaceid, size_t elmt_size, unsigned
    // flags); public  static     H5Ssel_iter_get_seq_list(hid_t sel_iter_id, size_t maxseq,
    // size_t maxbytes, size_t *nseq,
    //                                        size_t *nbytes, hsize_t *off, size_t *len);
    // public  static     H5Ssel_iter_reset(hid_t sel_iter_id, hid_t space_id);
    // public  static     H5Ssel_iter_close(hid_t sel_iter_id);

    // ////////////////////////////////////////////////////////////
    // //
    // H5T: Datatype Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5T Java Datatype (H5T) Interface
     *
     * @see H5T, C-API
     *
     * @see @ref H5T_UG, User Guide
     **/

    /**
     * @defgroup JH5T Java Datatype (H5T) Interface
     **/

    /**
     * @ingroup JH5T
     *
     * H5Tarray_create creates a new array datatype object.
     *
     * @param base_id
     *            IN: Datatype identifier for the array base datatype.
     * @param ndims
     *            IN: Rank of the array.
     * @param dim
     *            IN: Size of each array dimension.
     *
     * @return a valid datatype identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            dim is null.
     **/
    public static long H5Tarray_create(long base_id, int ndims, long[] dim)
        throws HDF5LibraryException, NullPointerException
    {
        if (dim == null)
            throw new NullPointerException("dim is null");
        long id = HDF5Constants.H5I_INVALID_HID;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dim_segment = arena.allocateFrom(ValueLayout.JAVA_LONG, dim);
            id = org.hdfgroup.javahdf5.hdf5_h.H5Tarray_create2(base_id, ndims, dim_segment);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tarray_create add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tclose releases a datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to release.
     *
     * @return a non-negative value if successful
     **/
    public static int H5Tclose(long type_id)
    {
        log.trace("OPEN_IDS: H5Tclose remove {}", type_id);
        OPEN_IDS.remove(type_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tclose(type_id);
        if (retVal < 0)
            h5libraryError();
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tcommit saves a transient datatype as an immutable named datatype in a file.
     *
     * @param loc_id
     *            IN: Location identifier.
     * @param name
     *            IN: Name given to committed datatype.
     * @param type_id
     *            IN: Identifier of datatype to be committed.
     * @param lcpl_id
     *            IN: Link creation property list.
     * @param tcpl_id
     *            IN: Datatype creation property list.
     * @param tapl_id
     *            IN: Datatype access property list.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static void H5Tcommit(long loc_id, String name, long type_id, long lcpl_id, long tcpl_id,
                                 long tapl_id) throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Tcommit2(loc_id, name_segment, type_id, lcpl_id, tcpl_id,
                                                        tapl_id) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tcommit_anon commits a transient datatype (not immutable) to a file, turning it into a named datatype
     * with the specified creation and property lists.
     *
     * @param loc_id
     *            IN: Location identifier.
     * @param type_id
     *            IN: Identifier of datatype to be committed.
     * @param tcpl_id
     *            IN: Datatype creation property list.
     * @param tapl_id
     *            IN: Datatype access property list.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Tcommit_anon(long loc_id, long type_id, long tcpl_id, long tapl_id)
        throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Tcommit_anon(loc_id, type_id, tcpl_id, tapl_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tcommitted queries a type to determine whether the type specified by the type identifier is a named
     * type or a transient type.
     *
     * @param type_id
     *            IN: Identifier of datatype.
     *
     * @return true the datatype has been committed
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Tcommitted(long type_id) throws HDF5LibraryException
    {
        boolean isCommitted = false;
        int result          = org.hdfgroup.javahdf5.hdf5_h.H5Tcommitted(type_id);
        if (result < 0) {
            h5libraryError();
        }
        else if (result > 0) {
            isCommitted = true;
        }
        else {
            isCommitted = false;
        }
        return isCommitted;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tcompiler_conv finds out whether the library's conversion function from type src_id to type dst_id is
     * a compiler (hard) conversion.
     *
     * @param src_id
     *            IN: Identifier of source datatype.
     * @param dst_id
     *            IN: Identifier of destination datatype.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Tcompiler_conv(long src_id, long dst_id) throws HDF5LibraryException
    {
        boolean hardconvert = false;
        int result          = org.hdfgroup.javahdf5.hdf5_h.H5Tcompiler_conv(src_id, dst_id);
        if (result < 0) {
            h5libraryError();
        }
        else if (result > 0) {
            hardconvert = true;
        }
        else {
            hardconvert = false;
        }
        return hardconvert;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tcomplex_create creates a new complex number datatype object.
     *
     * @param base_id
     *            IN: Datatype identifier for the complex number base datatype.
     *                Must be a floating-point datatype.
     *
     * @return a valid datatype identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tcomplex_create(long base_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tcomplex_create(base_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tcomplex_create add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tconvert converts nelmts elements from the type specified by the src_id identifier to type dst_id.
     *
     * @param src_id
     *            IN: Identifier of source datatype.
     * @param dst_id
     *            IN: Identifier of destination datatype.
     * @param nelmts
     *            IN: Size of array buf.
     * @param buf
     *            IN: Array containing pre- and post-conversion values.
     * @param background
     *            IN: Optional background buffer.
     * @param plist_id
     *            IN: Dataset transfer property list identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static void H5Tconvert(long src_id, long dst_id, long nelmts, byte[] buf, byte[] background,
                                  long plist_id) throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment        = arena.allocateFrom(ValueLayout.JAVA_BYTE, buf);
            MemorySegment background_segment = MemorySegment.NULL;
            if (background != null)
                background_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, background);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Tconvert(src_id, dst_id, nelmts, buf_segment,
                                                        background_segment, plist_id) < 0)
                h5libraryError();
            for (int i = 0; i < buf.length; i++) {
                buf[i] = buf_segment.get(ValueLayout.JAVA_BYTE, i);
            }
        }
    }

    // int H5Tconvert(int src_id, int dst_id, long nelmts, Pointer buf, Pointer background, int plist_id);

    /**
     * @ingroup JH5T
     *
     * H5Tcopy copies an existing datatype. The returned type is always transient and unlocked.
     *
     * @param type_id
     *            IN: Identifier of datatype to copy. Can be a datatype identifier, a predefined datatype
     *                (defined in H5Tpublic.h), or a dataset Identifier.
     *
     * @return a datatype identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tcopy(long type_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tcopy(type_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tcopy add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tcreate creates a new datatype of the specified class with the specified number of bytes.
     *
     * @param tclass
     *            IN: Class of datatype to create.
     * @param size
     *            IN: The number of bytes in the datatype to create.
     *
     * @return datatype identifier
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tcreate(int tclass, long size) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tcreate(tclass, size);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tdecode reconstructs the HDF5 data type object and returns a new object handle for it.
     *
     * @param buf
     *            IN: Buffer for the data type object to be decoded.
     *
     * @param buf_size
     *           IN: Size of the buffer.
     *
     * @return a new object handle
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static long H5Tdecode(byte[] buf, long buf_size) throws HDF5LibraryException, NullPointerException
    {
        long id = H5I_INVALID_HID();
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, buf);
            id                        = org.hdfgroup.javahdf5.hdf5_h.H5Tdecode2(buf_segment, buf_size);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tdecode add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tdetect_class determines whether the datatype specified in dtype_id contains any datatypes of the
     * datatype class specified in dtype_class.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param cls
     *            IN: Identifier of datatype cls.
     *
     * @return true if the datatype specified in dtype_id contains any datatypes of the datatype class
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Tdetect_class(long type_id, int cls) throws HDF5LibraryException
    {
        boolean isClass = false;
        int result      = org.hdfgroup.javahdf5.hdf5_h.H5Tdetect_class(type_id, cls);
        if (result < 0) {
            h5libraryError();
        }
        else if (result > 0) {
            isClass = true;
        }
        else {
            isClass = false;
        }
        return isClass;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tencode converts a data type description into binary form in a buffer.
     *
     * @param obj_id
     *            IN: Identifier of the object to be encoded.
     * @param buf
     *            OUT: Buffer for the object to be encoded into. If the provided buffer is NULL, only the size
     *                 of buffer needed is returned.
     * @param nalloc
     *            IN: The size of the allocated buffer.
     *
     * @return the size needed for the allocated buffer.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static int H5Tencode(long obj_id, byte[] buf, long nalloc)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a buffer for the size
            MemorySegment nalloc_segment = arena.allocate(ValueLayout.JAVA_LONG);
            org.hdfgroup.javahdf5.hdf5_h.H5Tencode(obj_id, MemorySegment.NULL, nalloc_segment);

            MemorySegment buf_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, buf);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Tencode(obj_id, buf_segment, nalloc_segment)) < 0)
                h5libraryError();
            nalloc = nalloc_segment.get(ValueLayout.JAVA_LONG, 0);
        }
        return status;
    }

    // /*
    //  * @ingroup JH5T
    //  *
    //  * H5Tencode converts a data type description into binary form in a buffer.
    //  *
    //  * @param obj_id
    //  *            IN: Identifier of the object to be encoded.
    //  *
    //  * @return the buffer for the object to be encoded into.
    //  *
    //  * @exception HDF5LibraryException
    //  *            Error from the HDF5 Library.
    //  **/
    // public  static  byte[] H5Tencode(int obj_id)
    //     throws HDF5LibraryException {}

    /**
     * @ingroup JH5T
     *
     * H5Tenum_create creates a new enumeration datatype based on the specified base datatype, parent_id,
     * which must be an integer type.
     *
     * @param base_id
     *            IN: Identifier of the parent datatype to release.
     *
     * @return the datatype identifier for the new enumeration datatype
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tenum_create(long base_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tenum_create(base_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tenum_create add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tenum_insert inserts a new enumeration datatype member into an enumeration datatype.
     *
     * @param type
     *            IN: Identifier of datatype.
     * @param name
     *            IN: The name of the member
     * @param value
     *            IN: The value of the member, data of the correct type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static void H5Tenum_insert(long type, String name, byte[] value)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        if (type < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment  = arena.allocateFrom(name);
            MemorySegment value_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, value);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Tenum_insert(type, name_segment, value_segment) < 0)
                h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tenum_insert inserts a new enumeration datatype member into an enumeration datatype.
     *
     * @param type
     *            IN: Identifier of datatype.
     * @param name
     *            IN: The name of the member
     * @param value
     *            IN: The value of the member, data of the correct type
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Tenum_insert(long type, String name, int[] value)
        throws HDF5LibraryException, NullPointerException
    {
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        return hdf.hdf5lib.H5.H5Tenum_insert_int(type, name, value);
    }

    /**
     * @ingroup JH5T
     *
     * H5Tenum_insert inserts a new enumeration datatype member into an enumeration datatype.
     *
     * @param type
     *            IN: Identifier of datatype.
     * @param name
     *            IN: The name of the member
     * @param value
     *            IN: The value of the member, data of the correct type
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Tenum_insert(long type, String name, int value)
        throws HDF5LibraryException, NullPointerException
    {
        int[] val = {value};
        return hdf.hdf5lib.H5.H5Tenum_insert_int(type, name, val);
    }

    private static int H5Tenum_insert_int(long type, String name, int[] intvalue)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (intvalue == null) {
            throw new NullPointerException("intvalue is null");
        }
        if (type < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }
        byte[] byteArray = new byte[intvalue.length * 4]; // Each int is 4 bytes

        for (int i = 0; i < intvalue.length; i++) {
            int value = intvalue[i];
            // Extract bytes using bit shifts and store them in the byte array
            byteArray[i * 4]     = (byte)((value >> 24) & 0xFF);
            byteArray[i * 4 + 1] = (byte)((value >> 16) & 0xFF);
            byteArray[i * 4 + 2] = (byte)((value >> 8) & 0xFF);
            byteArray[i * 4 + 3] = (byte)(value & 0xFF);
        }
        hdf.hdf5lib.H5.H5Tenum_insert(type, name, byteArray);
        return 0;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tenum_nameof finds the symbol name that corresponds to the specified value of the enumeration
     * datatype type.
     *
     * @param type
     *            IN: Identifier of datatype.
     * @param value
     *            IN: The value of the member, data of the correct
     * @param size
     *            IN: The probable length of the name
     *
     * @return the symbol name.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            value is null.
     **/
    public static String H5Tenum_nameof(long type, byte[] value, long size)
        throws HDF5LibraryException, NullPointerException
    {
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        if (type < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }
        String name = new String();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment  = arena.allocate(size + 1);
            MemorySegment value_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, value);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Tenum_nameof(type, value_segment, name_segment, size) < 0)
                h5libraryError();
            name = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return name;
    }

    // int H5Tenum_nameof(int type, Pointer value, Buffer name/* out */, long size);

    /**
     * @ingroup JH5T
     *
     * H5Tenum_nameof finds the symbol name that corresponds to the specified value of the enumeration
     * datatype type.
     *
     * @param type
     *            IN: Identifier of datatype.
     * @param value
     *            IN: The value of the member, data of the correct
     * @param name
     *            OUT: The name of the member
     * @param size
     *            IN: The max length of the name
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Tenum_nameof(long type, int[] value, String[] name, int size)
        throws HDF5LibraryException, NullPointerException
    {
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        if (type < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment  = arena.allocate(size + 1);
            MemorySegment value_segment = arena.allocateFrom(ValueLayout.JAVA_INT, value);
            if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Tenum_nameof(type, value_segment, name_segment,
                                                                      size)) < 0)
                h5libraryError();
            name[0] = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return status;
    }

    private static int H5Tenum_nameof_int(long type, int[] value, String[] name, int size)
        throws HDF5LibraryException, NullPointerException
    {
        return hdf.hdf5lib.H5.H5Tenum_nameof(type, value, name, size);
    }

    /**
     * @ingroup JH5T
     *
     * H5Tenum_valueof finds the value that corresponds to the specified name of the enumeration datatype
     * type.
     *
     * @param type
     *            IN: Identifier of datatype.
     * @param name
     *            IN: The name of the member
     * @param value
     *            OUT: The value of the member
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Tenum_valueof(long type, String name, byte[] value)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        if (type < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment  = arena.allocateFrom(name);
            MemorySegment value_segment = arena.allocate(ValueLayout.JAVA_BYTE, value.length);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Tenum_valueof(type, name_segment, value_segment) < 0)
                h5libraryError();
            MemorySegment.copy(value_segment, ValueLayout.JAVA_BYTE, 0L, value, 0, value.length);
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tenum_valueof finds the value that corresponds to the specified name of the enumeration datatype
     * type.
     *
     * @param type
     *            IN: Identifier of datatype.
     * @param name
     *            IN: The name of the member
     * @param value
     *            OUT: The value of the member
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Tenum_valueof(long type, String name, int[] value)
        throws HDF5LibraryException, NullPointerException
    {
        hdf.hdf5lib.H5.H5Tenum_valueof_int(type, name, value);
        return 0;
    }

    private static int H5Tenum_valueof_int(long type, String name, int[] value)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        if (type < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }
        byte[] byteArray = new byte[value.length * 4]; // Each int is 4 bytes

        hdf.hdf5lib.H5.H5Tenum_valueof(type, name, byteArray);
        ByteBuffer buffer = ByteBuffer.wrap(byteArray);

        for (int i = 0; i < value.length; i++) {
            value[i] = buffer.getInt();
        }
        return 0;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tequal determines whether two datatype identifiers refer to the same datatype.
     *
     * @param type_id1
     *            IN: Identifier of datatype to compare.
     * @param type_id2
     *            IN: Identifier of datatype to compare.
     *
     * @return true if the datatype identifiers refer to the same datatype, else false.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Tequal(long type_id1, long type_id2) throws HDF5LibraryException
    {
        if (type_id1 < 0 || type_id2 < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }
        boolean isEqual = false;
        int result      = org.hdfgroup.javahdf5.hdf5_h.H5Tequal(type_id1, type_id2);
        if (result < 0) {
            h5libraryError();
        }
        else if (result > 0) {
            isEqual = true;
        }
        else {
            isEqual = false;
        }
        return isEqual;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_array_dims returns the sizes of the dimensions of the specified array datatype object.
     *
     * @param type_id
     *            IN: Datatype identifier of array object.
     * @param dims
     *            OUT: Sizes of array dimensions.
     *
     * @return the non-negative number of dimensions of the array type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            dims is null.
     **/
    public static int H5Tget_array_dims(long type_id, long[] dims)
        throws HDF5LibraryException, NullPointerException
    {
        return hdf.hdf5lib.H5.H5Tget_array_dims2(type_id, dims);
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_array_dims2 returns the sizes of the dimensions of the specified array datatype object.
     *
     * @param type_id
     *            IN: Datatype identifier of array object.
     * @param dims
     *            OUT: Sizes of array dimensions.
     *
     * @return the non-negative number of dimensions of the array type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            dims is null.
     **/
    public static int H5Tget_array_dims2(long type_id, long[] dims)
        throws HDF5LibraryException, NullPointerException
    {
        if (dims == null) {
            throw new NullPointerException("dims is null");
        }
        int ndims = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dims_segment = arena.allocate(ValueLayout.JAVA_LONG, dims.length);
            ndims = org.hdfgroup.javahdf5.hdf5_h.H5Tget_array_dims2(type_id, dims_segment);
            if (ndims < 0)
                h5libraryError();
            for (int i = 0; i < ndims; i++)
                dims[i] = dims_segment.get(ValueLayout.JAVA_LONG, i * Long.BYTES);
        }
        return ndims;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_array_ndims returns the rank, the number of dimensions, of an array datatype object.
     *
     * @param type_id
     *            IN: Datatype identifier of array object.
     *
     * @return the rank of the array
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_array_ndims(long type_id) throws HDF5LibraryException
    {
        int ndims = org.hdfgroup.javahdf5.hdf5_h.H5Tget_array_ndims(type_id);
        if (ndims < 0) {
            h5libraryError();
        }
        return ndims;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_class returns the datatype class identifier.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return datatype class identifier if successful; otherwise H5T_NO_CLASS(-1).
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_class(long type_id) throws HDF5LibraryException
    {
        int class_id = org.hdfgroup.javahdf5.hdf5_h.H5Tget_class(type_id);
        if (class_id < 0) {
            h5libraryError();
        }
        return class_id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_class_name returns the datatype class identifier.
     *
     * @param class_id
     *            IN: Identifier of class from H5Tget_class.
     *
     * @return class name if successful; otherwise H5T_NO_CLASS.
     *
     **/
    public static String H5Tget_class_name(long class_id)
    {
        String retValue = null;
        if (HDF5Constants.H5T_INTEGER == class_id) /* integer types */
            retValue = "H5T_INTEGER";
        else if (HDF5Constants.H5T_FLOAT == class_id) /* floating-point types */
            retValue = "H5T_FLOAT";
        else if (HDF5Constants.H5T_TIME == class_id) /* date and time types */
            retValue = "H5T_TIME";
        else if (HDF5Constants.H5T_STRING == class_id) /* character string types */
            retValue = "H5T_STRING";
        else if (HDF5Constants.H5T_BITFIELD == class_id) /* bit field types */
            retValue = "H5T_BITFIELD";
        else if (HDF5Constants.H5T_OPAQUE == class_id) /* opaque types */
            retValue = "H5T_OPAQUE";
        else if (HDF5Constants.H5T_COMPOUND == class_id) /* compound types */
            retValue = "H5T_COMPOUND";
        else if (HDF5Constants.H5T_REFERENCE == class_id) /* reference types */
            retValue = "H5T_REFERENCE";
        else if (HDF5Constants.H5T_ENUM == class_id) /* enumeration types */
            retValue = "H5T_ENUM";
        else if (HDF5Constants.H5T_VLEN == class_id) /* Variable-Length types */
            retValue = "H5T_VLEN";
        else if (HDF5Constants.H5T_ARRAY == class_id) /* Array types */
            retValue = "H5T_ARRAY";
        else if (HDF5Constants.H5T_COMPLEX == class_id) /* Complex number types */
            retValue = "H5T_COMPLEX";
        else
            retValue = "H5T_NO_CLASS";

        return retValue;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_create_plist returns a property list identifier for the datatype creation property list
     * associated with the datatype specified by type_id.
     *
     * @param type_id
     *            IN: Identifier of datatype.
     *
     * @return a datatype property list identifier.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tget_create_plist(long type_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tget_create_plist(type_id);
        if (id > 0) {
            log.trace("OPEN_IDS: _H5Tget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_cset retrieves the character set type of a string datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid character set type if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_cset(long type_id) throws HDF5LibraryException
    {
        int cset = org.hdfgroup.javahdf5.hdf5_h.H5Tget_cset(type_id);
        if (cset < 0) {
            h5libraryError();
        }
        return cset;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_cset the character set to be used.
     *
     * @param type_id
     *            IN: Identifier of datatype to modify.
     * @param cset
     *            IN: Character set type.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_cset(long type_id, int cset) throws HDF5LibraryException
    {
        if (cset < 0) {
            throw new HDF5FunctionArgumentException("Negative character set value");
        }
        int status = org.hdfgroup.javahdf5.hdf5_h.H5Tset_cset(type_id, cset);
        if (status < 0) {
            h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_ebias retrieves the exponent bias of a floating-point type.
     *
     * @param type_id
     *            Identifier of datatype to query.
     *
     * @return the bias if successful; otherwise 0.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_ebias(long type_id) throws HDF5LibraryException
    {
        int ebias = (int)hdf.hdf5lib.H5.H5Tget_ebias_long(type_id);
        return ebias;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_ebias sets the exponent bias of a floating-point type.
     *
     * @param type_id
     *            Identifier of datatype to set.
     * @param ebias
     *            Exponent bias value.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_ebias(long type_id, int ebias) throws HDF5LibraryException
    {
        int status = org.hdfgroup.javahdf5.hdf5_h.H5Tset_ebias(type_id, (long)ebias);
        if (status < 0) {
            h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_ebias retrieves the exponent bias of a floating-point type.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return the bias
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tget_ebias_long(long type_id) throws HDF5LibraryException
    {
        long ebias = org.hdfgroup.javahdf5.hdf5_h.H5Tget_ebias(type_id);
        if (ebias == 0) {
            h5libraryError();
        }
        return ebias;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_ebias sets the exponent bias of a floating-point type.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param ebias
     *            IN: Exponent bias value.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Tset_ebias(long type_id, long ebias) throws HDF5LibraryException
    {
        int status = org.hdfgroup.javahdf5.hdf5_h.H5Tset_ebias(type_id, ebias);
        if (status < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_fields retrieves information about the locations of the various bit fields of a floating point
     * datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param fields
     *            OUT: location of size and bit-position.
     *            <ul>
     *            <li>fields[0] = spos OUT: location to return size of in bits.</li>
     *            <li>fields[1] = epos OUT: location to return exponent bit-position.</li>
     *            <li>fields[2] = esize OUT: location to return size of exponent in bits.</li>
     *            <li>fields[3] = mpos OUT: location to return mantissa bit-position.</li>
     *            <li>fields[4] = msize OUT: location to return size of mantissa in bits.</li>
     *            </ul>
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            fields is null.
     * @exception HDF5FunctionArgumentException
     *            fields array is invalid.
     **/
    public static void H5Tget_fields(long type_id, long[] fields)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (fields == null) {
            throw new NullPointerException("fields is null");
        }
        if (fields.length < 5) {
            throw new HDF5FunctionArgumentException("fields array is invalid");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment spos_segment  = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment epos_segment  = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment esize_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment mpos_segment  = arena.allocate(ValueLayout.JAVA_LONG, 1);
            MemorySegment msize_segment = arena.allocate(ValueLayout.JAVA_LONG, 1);
            int status                  = org.hdfgroup.javahdf5.hdf5_h.H5Tget_fields(
                type_id, spos_segment, epos_segment, esize_segment, mpos_segment, msize_segment);
            if (status < 0)
                h5libraryError();
            fields[0] = spos_segment.get(ValueLayout.JAVA_LONG, 0);
            fields[1] = epos_segment.get(ValueLayout.JAVA_LONG, 0);
            fields[2] = esize_segment.get(ValueLayout.JAVA_LONG, 0);
            fields[3] = mpos_segment.get(ValueLayout.JAVA_LONG, 0);
            fields[4] = msize_segment.get(ValueLayout.JAVA_LONG, 0);
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_fields retrieves information about the locations of the various bit fields of a floating point
     * datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param fields
     *            OUT: location of size and bit-position.
     *
     * <pre>
     *      fields[0] = spos  OUT: location to return size of in bits.
     *      fields[1] = epos  OUT: location to return exponent bit-position.
     *      fields[2] = esize OUT: location to return size of exponent in bits.
     *      fields[3] = mpos  OUT: location to return mantissa bit-position.
     *      fields[4] = msize OUT: location to return size of mantissa in bits.
     * </pre>
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            fields is null.
     * @exception HDF5FunctionArgumentException
     *            fields array is invalid.
     **/
    public static int H5Tget_fields(long type_id, int[] fields)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        return H5Tget_fields_int(type_id, fields);
    }

    private static int H5Tget_fields_int(long type_id, int[] fields)
        throws HDF5LibraryException, NullPointerException, HDF5FunctionArgumentException
    {
        if (fields == null) {
            throw new NullPointerException("fields is null");
        }
        if (fields.length < 5) {
            throw new HDF5FunctionArgumentException("fields array is invalid");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment spos_segment  = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment epos_segment  = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment esize_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment mpos_segment  = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment msize_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Tget_fields(type_id, spos_segment, epos_segment,
                                                                esize_segment, mpos_segment, msize_segment);
            if (status < 0)
                h5libraryError();
            fields[0] = spos_segment.get(ValueLayout.JAVA_INT, 0);
            fields[1] = epos_segment.get(ValueLayout.JAVA_INT, 0);
            fields[2] = esize_segment.get(ValueLayout.JAVA_INT, 0);
            fields[3] = mpos_segment.get(ValueLayout.JAVA_INT, 0);
            fields[4] = msize_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return status;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_fields sets the locations and sizes of the various floating point bit fields.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param spos
     *            IN: Size position.
     * @param epos
     *            IN: Exponent bit position.
     * @param esize
     *            IN: Size of exponent in bits.
     * @param mpos
     *            IN: Mantissa bit position.
     * @param msize
     *            IN: Size of mantissa in bits.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Tset_fields(long type_id, long spos, long epos, long esize, long mpos, long msize)
        throws HDF5LibraryException
    {
        int status = org.hdfgroup.javahdf5.hdf5_h.H5Tset_fields(type_id, spos, epos, esize, mpos, msize);
        if (status < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_fields sets the locations and sizes of the various floating point bit fields.
     *
     * @param type_id
     *            Identifier of datatype to set.
     * @param spos
     *            Size position.
     * @param epos
     *            Exponent bit position.
     * @param esize
     *            Size of exponent in bits.
     * @param mpos
     *            Mantissa bit position.
     * @param msize
     *            Size of mantissa in bits.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_fields(long type_id, int spos, int epos, int esize, int mpos, int msize)
        throws HDF5LibraryException
    {
        hdf.hdf5lib.H5.H5Tset_fields(type_id, (long)spos, (long)epos, (long)esize, (long)mpos, (long)msize);
        return 0;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_inpad retrieves the internal padding type for unused bits in floating-point datatypes.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid padding type if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_inpad(long type_id) throws HDF5LibraryException
    {
        int inpad = org.hdfgroup.javahdf5.hdf5_h.H5Tget_inpad(type_id);
        if (inpad < 0) {
            h5libraryError();
        }
        return inpad;
    }

    /**
     * @ingroup JH5T
     *
     * If any internal bits of a floating point type are unused (that is, those significant bits which are not
     * part of the sign, exponent, or mantissa), then H5Tset_inpad will be filled according to the value of
     * the padding value property inpad.
     *
     * @param type_id
     *            IN: Identifier of datatype to modify.
     * @param inpad
     *            IN: Padding type.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *             Error from the HDF5 Library.
     **/
    public static int H5Tset_inpad(long type_id, int inpad) throws HDF5LibraryException
    {
        int status = org.hdfgroup.javahdf5.hdf5_h.H5Tset_inpad(type_id, inpad);
        if (status < 0) {
            h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_member_class returns the class of datatype of the specified member.
     *
     * @param type_id
     *            IN: Datatype identifier of compound object.
     * @param membno
     *            IN: Compound object member number.
     *
     * @return the class of the datatype of the field if successful;
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_member_class(long type_id, int membno) throws HDF5LibraryException
    {
        int classId = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_class(type_id, membno);
        if (classId < 0) {
            h5libraryError();
        }
        return classId;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_member_index retrieves the index of a field of a compound datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param field_name
     *            IN: Field name of the field index to retrieve.
     *
     * @return if field is defined, the index; else negative.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_member_index(long type_id, String field_name) throws HDF5LibraryException
    {
        if (field_name == null) {
            throw new NullPointerException("field_name is null");
        }
        int index = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment field_name_segment = arena.allocateFrom(field_name);
            index = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_index(type_id, field_name_segment);
        }
        if (index < 0) {
            h5libraryError();
        }
        return index;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_member_name retrieves the name of a field of a compound datatype or an element of an enumeration
     * datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param field_idx
     *            IN: Field index (0-based) of the field name to retrieve.
     *
     * @return a valid pointer to the name if successful; otherwise null.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5Tget_member_name(long type_id, int field_idx) throws HDF5LibraryException
    {
        String name = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_name(type_id, field_idx);
            if (name_segment == null)
                h5libraryError();
            name = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return name;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_member_offset returns the byte offset of the specified member of the compound datatype. This is
     * the byte offset in the HDF5 file/library, NOT the offset of any Java object which might be mapped to
     * this data item.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param membno
     *            IN: Field index (0-based) of the field type to retrieve.
     *
     * @return the offset of the member.
     **/
    public static long H5Tget_member_offset(long type_id, int membno)
    {
        long offset = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_offset(type_id, membno);
        if (offset < 0) {
            h5libraryError();
        }
        return offset;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_member_type returns the datatype of the specified member.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param field_idx
     *            IN: Field index (0-based) of the field type to retrieve.
     *
     * @return the identifier of a copy of the datatype of the field if successful;
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tget_member_type(long type_id, int field_idx) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_type(type_id, field_idx);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tget_member_type add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_member_value returns the value of the enumeration datatype member memb_no.
     *
     * @param type_id
     *            IN: Datatype identifier for the enumeration datatype.
     * @param membno
     *            IN: Number of the enumeration datatype member.
     * @param value
     *            OUT: The value of the member
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            value is null.
     **/
    public static void H5Tget_member_value(long type_id, int membno, byte[] value)
        throws HDF5LibraryException, NullPointerException
    {
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        if (value.length != 4) {
            throw new HDF5FunctionArgumentException("value array must have length 4");
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment value_segment = arena.allocate(value.length * ValueLayout.JAVA_BYTE.byteSize());
            if (org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_value(type_id, membno, value_segment) < 0)
                h5libraryError();
            for (int i = 0; i < value.length; i++) {
                value[i] = value_segment.get(ValueLayout.JAVA_BYTE, i);
            }
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_member_value returns the value of the enumeration datatype member memb_no.
     *
     * @param type_id
     *            IN: Identifier of datatype.
     * @param membno
     *            IN: The name of the member
     * @param value
     *            OUT: The value of the member
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            value is null.
     **/
    public static int H5Tget_member_value(long type_id, int membno, int[] value)
        throws HDF5LibraryException, NullPointerException
    {
        return hdf.hdf5lib.H5.H5Tget_member_value_int(type_id, membno, value);
    }

    private static int H5Tget_member_value_int(long type_id, int membno, int[] value)
        throws HDF5LibraryException, NullPointerException
    {
        if (value == null) {
            throw new NullPointerException("value is null");
        }
        if (value.length != 1) {
            throw new HDF5FunctionArgumentException("value array must have length 1");
        }

        int status = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment value_segment = arena.allocate(ValueLayout.JAVA_INT, value.length);
            status = org.hdfgroup.javahdf5.hdf5_h.H5Tget_member_value(type_id, membno, value_segment);
            if (status < 0)
                h5libraryError();
            for (int i = 0; i < value.length; i++) {
                value[i] = value_segment.get(ValueLayout.JAVA_INT, i * Integer.BYTES);
            }
        }
        return status;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_native_type returns the equivalent native datatype for the datatype specified in type_id.
     *
     * @param type_id
     *            IN: Identifier of datatype to query. Direction of search is assumed to be in ascending
     *                order.
     *
     * @return the native datatype identifier for the specified dataset datatype.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tget_native_type(long type_id) throws HDF5LibraryException
    {
        return hdf.hdf5lib.H5.H5Tget_native_type(type_id, HDF5Constants.H5T_DIR_ASCEND);
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_native_type returns the equivalent native datatype for the datatype specified in type_id.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param direction
     *            IN: Direction of search.
     *
     * @return the native datatype identifier for the specified dataset datatype.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tget_native_type(long type_id, int direction) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tget_native_type(type_id, direction);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tget_native_type add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_nmembers retrieves the number of fields a compound datatype has.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return number of members datatype has if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_nmembers(long type_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_nmembers(type_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_norm retrieves the mantissa normalization of a floating-point datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid normalization type if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_norm(long type_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_norm(type_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_norm sets the mantissa normalization of a floating-point datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param norm
     *            IN: Mantissa normalization type.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_norm(long type_id, int norm) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tset_norm(type_id, norm);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_offset retrieves the bit offset of the first significant bit.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a positive offset value if successful; otherwise 0.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_offset(long type_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_offset(type_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_offset sets the bit offset of the first significant bit.
     *
     * @param type_id
     *            Identifier of datatype to set.
     * @param offset
     *            Offset of first significant bit.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_offset(long type_id, int offset) throws HDF5LibraryException
    {
        hdf.hdf5lib.H5.H5Tset_offset(type_id, (long)offset);
        return 0;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_offset sets the bit offset of the first significant bit.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param offset
     *            IN: Offset of first significant bit.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Tset_offset(long type_id, long offset) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tset_offset(type_id, offset);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_order returns the byte order of an atomic datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a byte order constant if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_order(long type_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_order(type_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_order sets the byte ordering of an atomic datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param order
     *            IN: Byte ordering constant.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_order(long type_id, int order) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tset_order(type_id, order);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_pad retrieves the padding type of the least and most-significant bit padding.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param pad
     *            OUT: locations to return least-significant and most-significant bit padding type.
     *
     *            <pre>
     *      pad[0] = lsb // least-significant bit padding type
     *      pad[1] = msb // most-significant bit padding type
     * </pre>
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            pad is null.
     **/
    public static int H5Tget_pad(long type_id, int[] pad) throws HDF5LibraryException, NullPointerException
    {
        if (pad == null || pad.length < 2) {
            throw new NullPointerException("pad is null or has less than 2 elements");
        }
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment lsb_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            MemorySegment msb_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_pad(type_id, lsb_segment, msb_segment)) < 0)
                h5libraryError();
            pad[0] = lsb_segment.get(ValueLayout.JAVA_INT, 0);
            pad[1] = msb_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_pad sets the least and most-significant bits padding types.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param lsb
     *            IN: Padding type for least-significant bits.
     * @param msb
     *            IN: Padding type for most-significant bits.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_pad(long type_id, int lsb, int msb) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tset_pad(type_id, lsb, msb);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_precision returns the precision of an atomic datatype.
     *
     * @param type_id
     *            Identifier of datatype to query.
     *
     * @return the number of significant bits if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_precision(long type_id) throws HDF5LibraryException
    {
        int retVal = (int)hdf.hdf5lib.H5.H5Tget_precision_long(type_id);
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_precision sets the precision of an atomic datatype.
     *
     * @param type_id
     *            Identifier of datatype to set.
     * @param precision
     *            Number of bits of precision for datatype.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_precision(long type_id, int precision) throws HDF5LibraryException
    {
        hdf.hdf5lib.H5.H5Tset_precision(type_id, (long)precision);
        return 0;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_precision returns the precision of an atomic datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return the number of significant bits if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tget_precision_long(long type_id) throws HDF5LibraryException
    {
        long retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_precision(type_id);
        if (retVal == 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_precision sets the precision of an atomic datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param precision
     *            IN: Number of bits of precision for datatype.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Tset_precision(long type_id, long precision) throws HDF5LibraryException
    {
        if (precision < 0) {
            throw new HDF5FunctionArgumentException("Negative precision value");
        }
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tset_precision(type_id, precision);
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_sign retrieves the sign type for an integer type.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid sign type if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_sign(long type_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_sign(type_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_sign sets the sign property for an integer type.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param sign
     *            IN: Sign type.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_sign(long type_id, int sign) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tset_sign(type_id, sign);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_size returns the size of a datatype in bytes.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return the size of the datatype in bytes
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tget_size(long type_id) throws HDF5FunctionArgumentException
    {
        long retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_size(type_id);
        if (retVal == 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_size sets the total size in bytes, size, for an atomic datatype (this operation is not permitted
     * on compound datatypes).
     *
     * @param type_id
     *            IN: Identifier of datatype to change size.
     * @param size
     *            IN: Size in bytes to modify datatype.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_size(long type_id, long size) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tset_size(type_id, size);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_strpad retrieves the string padding method for a string datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid string padding type if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tget_strpad(long type_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tget_strpad(type_id);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_strpad defines the storage mechanism for the string.
     *
     * @param type_id
     *            IN: Identifier of datatype to modify.
     * @param strpad
     *            IN: String padding type.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_strpad(long type_id, int strpad) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tset_strpad(type_id, strpad);
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_super returns the type from which TYPE is derived.
     *
     * @param type
     *            IN: Identifier of datatype.
     *
     * @return the parent type
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tget_super(long type) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tget_super(type);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tget_super add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tget_tag returns the tag associated with datatype type_id.
     *
     * @param type
     *            IN: Identifier of datatype.
     *
     * @return the tag
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5Tget_tag(long type) throws HDF5LibraryException
    {
        if (type < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }
        String tag = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment tag_segment = org.hdfgroup.javahdf5.hdf5_h.H5Tget_tag(type);
            if (tag_segment != null) {
                tag = tag_segment.getString(0, StandardCharsets.UTF_8);
            }
        }
        if (tag == null) {
            h5libraryError();
        }
        return tag;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tset_tag tags an opaque datatype type_id with a unique ASCII identifier tag.
     *
     * @param type
     *            IN: Datatype identifier for the opaque datatype to be tagged.
     * @param tag
     *            IN: Descriptive ASCII string with which the opaque datatype is to be tagged.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tset_tag(long type, String tag) throws HDF5LibraryException
    {
        if (tag == null) {
            throw new NullPointerException("tag is null");
        }
        if (type < 0) {
            throw new HDF5FunctionArgumentException("Negative type id value");
        }
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment tag_segment = arena.allocateFrom(tag);
            retVal                    = org.hdfgroup.javahdf5.hdf5_h.H5Tset_tag(type, tag_segment);
        }
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tinsert adds another member to the compound datatype type_id.
     *
     * @param type_id
     *            IN: Identifier of compound datatype to modify.
     * @param name
     *            IN: Name of the field to insert.
     * @param offset
     *            IN: Offset in memory structure of the field to insert.
     * @param field_id
     *            IN: Datatype identifier of the field to insert.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static int H5Tinsert(long type_id, String name, long offset, long field_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tinsert(type_id, name_segment, offset, field_id);
        }
        if (retVal < 0) {
            h5libraryError();
        }
        return retVal;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tis_variable_str determines whether the datatype identified in type_id is a variable-length string.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return true if type_id is a variable-length string.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5Tis_variable_str(long type_id) throws HDF5LibraryException
    {
        boolean is_vstr = false;
        int retVal      = -1;
        if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5Tis_variable_str(type_id)) < 0)
            h5libraryError();
        if (retVal > 0) {
            is_vstr = true;
        }
        return is_vstr;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tlock locks the datatype specified by the type_id identifier, making it read-only and
     * non-destrucible.
     *
     * @param type_id
     *            IN: Identifier of datatype to lock.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tlock(long type_id) throws HDF5LibraryException
    {
        int ret = org.hdfgroup.javahdf5.hdf5_h.H5Tlock(type_id);
        if (ret < 0) {
            h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5T
     *
     * H5Topen opens a named datatype at the location specified by loc_id and return an identifier for the
     * datatype.
     *
     * @param loc_id
     *            IN: A file, group, or datatype identifier.
     * @param name
     *            IN: A datatype name, defined within the file or group identified by loc_id.
     * @param tapl_id
     *            IN: Datatype access property list.
     *
     * @return a named datatype identifier if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            name is null.
     **/
    public static long H5Topen(long loc_id, String name, long tapl_id)
        throws HDF5LibraryException, NullPointerException
    {
        if (name == null) {
            throw new NullPointerException("name is null");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            id                         = org.hdfgroup.javahdf5.hdf5_h.H5Topen2(loc_id, name_segment, tapl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5Topen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tpack recursively removes padding from within a compound datatype to make it more efficient
     * (space-wise) to store that data. <P> <b>WARNING:</b> This call only affects the C-data, even if it
     * succeeds, there may be no visible effect on Java objects.
     *
     * @param type_id
     *            IN: Identifier of datatype to modify.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Tpack(long type_id) throws HDF5LibraryException
    {
        int ret = org.hdfgroup.javahdf5.hdf5_h.H5Tpack(type_id);
        if (ret < 0) {
            h5libraryError();
        }
        return ret;
    }

    /**
     * @ingroup JH5T
     *
     * H5Treclaim reclaims buffer used for VL data.
     *
     * @param type_id
     *            Identifier of the datatype.
     * @param space_id
     *            Identifier of the dataspace.
     * @param xfer_plist_id
     *            Identifier of a transfer property list for this I/O operation.
     * @param buf
     *            Buffer with data to be reclaimed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     * @exception NullPointerException
     *            buf is null.
     **/
    public static void H5Treclaim(long type_id, long space_id, long xfer_plist_id, byte[] buf)
        throws HDF5LibraryException, NullPointerException
    {
        if (buf == null) {
            throw new NullPointerException("buf is null");
        }
        int retVal = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment buf_segment = arena.allocateFrom(ValueLayout.JAVA_BYTE, buf);
            retVal = org.hdfgroup.javahdf5.hdf5_h.H5Treclaim(type_id, space_id, xfer_plist_id, buf_segment);
        }
        if (retVal < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Tvlen_create creates a new variable-length (VL) datatype.
     *
     * @param base_id
     *            IN: Identifier of parent datatype.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5Tvlen_create(long base_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5Tvlen_create(base_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tvlen_create add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }

    /**
     * @ingroup JH5T
     *
     * H5Tflush causes all buffers associated with a committed datatype to be immediately flushed to disk
     * without removing the data from the cache.
     *
     * @param dtype_id
     *            IN: Identifier of the committed datatype to be flushed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Tflush(long dtype_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Tflush(dtype_id) < 0) {
            h5libraryError();
        }
    }

    /**
     * @ingroup JH5T
     *
     * H5Trefresh causes all buffers associated with a committed datatype to be cleared and immediately
     * re-loaded with updated contents from disk. This function essentially closes the datatype, evicts
     * all metadata associated with it from the cache, and then re-opens the datatype. The reopened datatype
     * is automatically re-registered with the same ID.
     *
     * @param dtype_id
     *            IN: Identifier of the committed datatype to be refreshed.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5Trefresh(long dtype_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5Trefresh(dtype_id) < 0) {
            h5libraryError();
        }
    }

    // /////// unimplemented ////////

    // H5T_conv_t H5Tfind(int src_id, int dst_id, H5T_cdata_t *pcdata);

    // public  static  int H5Tregister(H5T_pers_t pers, String name, int src_id, int dst_id,
    // H5T_conv_t func)
    // throws HDF5LibraryException, NullPointerException {}

    // public  static  int H5Tunregister(H5T_pers_t pers, String name, int src_id, int
    // dst_id, H5T_conv_t func) throws HDF5LibraryException, NullPointerException {}

    // ////////////////////////////////////////////////////////////
    // //
    // H5VL: VOL Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * @defgroup JH5VL Java VOL Connector (H5VL) Interface
     *
     * @see H5VL, C-API
     *
     * @see @ref H5VL_UG, User Guide
     **/

    /**
     * @defgroup JH5VL Java VOL Connector (H5VL) Interface
     **/

    /**
     * @ingroup JH5VL
     *
     * H5VLregister_connector_by_name registers a new VOL connector as a member of the virtual object layer
     * class.
     *
     * @param connector_name
     *            IN: name of the connector.
     * @param vipl_id
     *            IN: VOL initialization property list which must be
     *                created with H5Pcreate(H5P_VOL_INITIALIZE) (or H5P_DEFAULT).
     *
     * @return a VOL connector ID
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5VLregister_connector_by_name(String connector_name, long vipl_id)
        throws HDF5LibraryException
    {
        long id = H5I_INVALID_HID();
        if (connector_name == null) {
            throw new NullPointerException("Connector name cannot be null");
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment connector_name_segment = arena.allocateFrom(connector_name);
            id = org.hdfgroup.javahdf5.hdf5_h.H5VLregister_connector_by_name(connector_name_segment, vipl_id);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5VLregister_connector_by_name add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLregister_connector_by_value registers a new VOL connector as a member of the virtual object layer
     * class.
     *
     * @param connector_value
     *            IN: value of the connector.
     * @param vipl_id
     *            IN: VOL initialization property list which must be
     *                created with H5Pcreate(H5P_VOL_INITIALIZE) (or H5P_DEFAULT).
     *
     * @return a VOL connector ID
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5VLregister_connector_by_value(int connector_value, long vipl_id)
        throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5VLregister_connector_by_value(connector_value, vipl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5VLregister_connector_by_value add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLis_connector_registered_by_name tests whether a VOL class has been registered.
     *
     * @param name
     *            IN: name of the connector.
     *
     * @return true if a VOL connector with that name has been registered
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5VLis_connector_registered_by_name(String name) throws HDF5LibraryException
    {
        if (name == null) {
            throw new NullPointerException("Connector name cannot be null");
        }
        boolean is_registered = false;
        int retVal            = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5VLis_connector_registered_by_name(name_segment)) < 0)
                h5libraryError();
        }
        if (retVal > 0) {
            is_registered = true;
        }
        return is_registered;
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLis_connector_registered_by_value tests whether a VOL class has been registered.
     *
     * @param connector_value
     *            IN: value of the connector.
     *
     * @return  true if a VOL connector with that value has been registered
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5VLis_connector_registered_by_value(int connector_value)
        throws HDF5LibraryException
    {
        int retVal = -1;

        if (connector_value < 0) {
            throw new HDF5LibraryException("Invalid connector value: " + connector_value);
        }
        boolean is_registered = false;
        if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5VLis_connector_registered_by_value(connector_value)) < 0)
            h5libraryError();
        if (retVal > 0) {
            is_registered = true;
        }
        return is_registered;
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLget_connector_id retrieves the ID for a registered VOL connector for a given object.
     *
     * @param object_id
     *            IN: Identifier of the object.
     *
     * @return a VOL connector ID
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5VLget_connector_id(long object_id) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5VLget_connector_id(object_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5VLget_connector_id add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLget_connector_id_by_name retrieves the ID for a registered VOL connector.
     *
     * @param name
     *            IN: name of the connector.
     *
     * @return a VOL connector ID
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5VLget_connector_id_by_name(String name) throws HDF5LibraryException
    {
        if (name == null) {
            throw new NullPointerException("Connector name cannot be null");
        }
        long id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocateFrom(name);
            id = org.hdfgroup.javahdf5.hdf5_h.H5VLget_connector_id_by_name(name_segment);
        }
        if (id > 0) {
            log.trace("OPEN_IDS: H5VLget_connector_id_by_name add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLget_connector_id_by_value retrieves the ID for a registered VOL connector.
     *
     * @param connector_value
     *            IN: value of the connector.
     *
     * @return a VOL connector ID
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static long H5VLget_connector_id_by_value(int connector_value) throws HDF5LibraryException
    {
        long id = org.hdfgroup.javahdf5.hdf5_h.H5VLget_connector_id_by_value(connector_value);
        if (id > 0) {
            log.trace("OPEN_IDS: H5VLget_connector_id_by_value add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        else
            h5libraryError();

        return id;
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLget_connector_name returns the connector name for the VOL associated with the
     * object or file ID.
     *
     * @param object_id
     *            IN: Identifier of the object.
     *
     * @return the connector name
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static String H5VLget_connector_name(long object_id) throws HDF5LibraryException
    {
        long buf_size = -1;
        buf_size      = org.hdfgroup.javahdf5.hdf5_h.H5VLget_connector_name(object_id, MemorySegment.NULL, 0);
        String ret_name = null;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment name_segment = arena.allocate(buf_size + 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5VLget_connector_name(object_id, name_segment, buf_size + 1) <
                0)
                h5libraryError();
            ret_name = name_segment.getString(0, StandardCharsets.UTF_8);
        }
        return ret_name;
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLclose closes a VOL connector ID.
     *
     * @param connector_id
     *            IN: Identifier of the connector.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5VLclose(long connector_id) throws HDF5LibraryException
    {
        int retVal = org.hdfgroup.javahdf5.hdf5_h.H5VLclose(connector_id);
        log.trace("OPEN_IDS: H5VLclose remove {}", connector_id);
        OPEN_IDS.remove(connector_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        if (retVal < 0) {
            h5libraryError();
        }
    }
    /**
     * @ingroup JH5VL
     *
     * H5VLunregister_connector removes a VOL connector ID from the library.
     *
     * @param connector_id
     *            IN: Identifier of the connector.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static void H5VLunregister_connector(long connector_id) throws HDF5LibraryException
    {
        if (org.hdfgroup.javahdf5.hdf5_h.H5VLunregister_connector(connector_id) < 0) {
            h5libraryError();
        }
        log.trace("OPEN_IDS: H5VLunregister_connector remove {}", connector_id);
        OPEN_IDS.remove(connector_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
    }

    /**
     * @ingroup JH5VL
     *
     * H5VLcmp_connector_cls Determines whether two connector identifiers refer to the same connector.
     *
     * @param conn_id1
     *            IN: Identifier of connector to compare.
     * @param conn_id2
     *            IN: Identifier of connector to compare.
     *
     * @return true if the connector identifiers refer to the same connector, else false.
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static boolean H5VLcmp_connector_cls(long conn_id1, long conn_id2) throws HDF5LibraryException
    {
        if (conn_id1 < 0 || conn_id2 < 0) {
            throw new HDF5LibraryException("Invalid connector ID: " + conn_id1 + ", " + conn_id2);
        }
        boolean is_equal = false;
        int cmp_value    = 0;
        int retVal       = -1;
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment cmp_value_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if ((retVal = org.hdfgroup.javahdf5.hdf5_h.H5VLcmp_connector_cls(cmp_value_segment, conn_id1,
                                                                             conn_id2)) < 0)
                h5libraryError();
            cmp_value = cmp_value_segment.get(ValueLayout.JAVA_INT, 0);
        }
        if (cmp_value == 0) {
            is_equal = true;
        }
        return is_equal;
    }

    // /////// unimplemented ////////
    // hid_t H5VLregister_connector(const H5VL_class_t *cls, hid_t vipl_id);

    // ////////////////////////////////////////////////////////////
    // //
    // H5Z: Filter Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
     * @defgroup JH5Z Java Filter (H5Z) Interface
     *
     * @see H5Z, C-API
     *
     * @see @ref H5Z_UG, User Guide
     **/

    /**
     * @ingroup JH5Z
     *
     * H5Zfilter_avail checks if a filter is available.
     *
     * @param filter
     *            IN: filter number.
     *
     * @return a non-negative(TRUE/FALSE) value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Zfilter_avail(int filter) throws HDF5LibraryException
    {
        int status = -1;
        if (filter < 0) {
            throw new HDF5LibraryException("Invalid filter number: " + filter);
        }
        if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Zfilter_avail(filter)) < 0) {
            h5libraryError();
        }
        return status;
    }

    /**
     * @ingroup JH5Z
     *
     * H5Zget_filter_info gets information about a pipeline data filter.
     *
     * @param filter
     *            IN: filter number.
     *
     * @return the filter information flags
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Zget_filter_info(int filter) throws HDF5LibraryException
    {
        int flags = -1;
        if (filter < 0) {
            throw new HDF5LibraryException("Invalid filter number: " + filter);
        }
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment flags_segment = arena.allocate(ValueLayout.JAVA_INT, 1);
            if (org.hdfgroup.javahdf5.hdf5_h.H5Zget_filter_info(filter, flags_segment) < 0)
                h5libraryError();
            flags = flags_segment.get(ValueLayout.JAVA_INT, 0);
        }
        return flags;
    }

    /**
     * @ingroup JH5Z
     *
     * H5Zunregister unregisters a filter.
     *
     * @param filter
     *            IN: filter number.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *            Error from the HDF5 Library.
     **/
    public static int H5Zunregister(int filter) throws HDF5LibraryException
    {
        int status = -1;
        if (filter < 0) {
            throw new HDF5LibraryException("Invalid filter number: " + filter);
        }
        if ((status = org.hdfgroup.javahdf5.hdf5_h.H5Zunregister(filter)) < 0) {
            h5libraryError();
        }
        return status;
    }

    // /////// unimplemented ////////

    // herr_t H5Zregister(const void *cls);
}
