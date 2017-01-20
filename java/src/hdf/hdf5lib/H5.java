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


package hdf.hdf5lib;

import java.io.File;
import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.LinkedHashSet;
import hdf.hdf5lib.callbacks.H5A_iterate_cb;
import hdf.hdf5lib.callbacks.H5A_iterate_t;
import hdf.hdf5lib.callbacks.H5D_iterate_cb;
import hdf.hdf5lib.callbacks.H5D_iterate_t;
import hdf.hdf5lib.callbacks.H5E_walk_cb;
import hdf.hdf5lib.callbacks.H5E_walk_t;
import hdf.hdf5lib.callbacks.H5L_iterate_cb;
import hdf.hdf5lib.callbacks.H5L_iterate_t;
import hdf.hdf5lib.callbacks.H5O_iterate_cb;
import hdf.hdf5lib.callbacks.H5O_iterate_t;
import hdf.hdf5lib.callbacks.H5P_cls_close_func_cb;
import hdf.hdf5lib.callbacks.H5P_cls_close_func_t;
import hdf.hdf5lib.callbacks.H5P_cls_copy_func_cb;
import hdf.hdf5lib.callbacks.H5P_cls_copy_func_t;
import hdf.hdf5lib.callbacks.H5P_cls_create_func_cb;
import hdf.hdf5lib.callbacks.H5P_cls_create_func_t;
import hdf.hdf5lib.callbacks.H5P_prp_set_func_cb;
import hdf.hdf5lib.callbacks.H5P_prp_get_func_cb;
import hdf.hdf5lib.callbacks.H5P_prp_delete_func_cb;
import hdf.hdf5lib.callbacks.H5P_prp_copy_func_cb;
import hdf.hdf5lib.callbacks.H5P_prp_compare_func_cb;
import hdf.hdf5lib.callbacks.H5P_prp_close_func_cb;
import hdf.hdf5lib.callbacks.H5P_prp_create_func_cb;
import hdf.hdf5lib.callbacks.H5P_iterate_cb;
import hdf.hdf5lib.callbacks.H5P_iterate_t;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5JavaException;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5AC_cache_config_t;
import hdf.hdf5lib.structs.H5A_info_t;
import hdf.hdf5lib.structs.H5E_error2_t;
import hdf.hdf5lib.structs.H5F_info2_t;
import hdf.hdf5lib.structs.H5G_info_t;
import hdf.hdf5lib.structs.H5L_info_t;
import hdf.hdf5lib.structs.H5O_info_t;

/**
 * This class is the Java interface for the HDF5 library.
 * <p>
 * This code is the called by Java programs to access the entry points of the HDF5 library. Each routine wraps a single
 * HDF5 entry point, generally with the arguments and return codes analogous to the C interface.
 * <p>
 * For details of the HDF5 library, see the HDF5 Documentation at: <a
 * href="http://hdfgroup.org/HDF5/">http://hdfgroup.org/HDF5/</a>
 * <hr>
 * <p>
 * <b>Mapping of arguments for Java</b>
 *
 * <p>
 * In general, arguments to the HDF Java API are straightforward translations from the 'C' API described in the HDF
 * Reference Manual.
 *
 * <center>
 * <table border=2 cellpadding=2>
 * <caption><b>HDF-5 C types to Java types</b> </caption>
 * <tr>
 * <td><b>HDF-5</b></td>
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
 * <td>void * <BR>
 * (i.e., pointer to `Any')</td>
 * <td>Special -- see HDFArray</td>
 * </tr>
 * </table>
 * </center>
 * <center> <b>General Rules for Passing Arguments and Results</b> </center>
 * <p>
 * In general, arguments passed <b>IN</b> to Java are the analogous basic types, as above. The exception is for arrays,
 * which are discussed below.
 * <p>
 * The <i>return value</i> of Java methods is also the analogous type, as above. A major exception to that rule is that
 * all HDF functions that return SUCCEED/FAIL are declared <i>boolean</i> in the Java version, rather than <i>int</i> as
 * in the C. Functions that return a value or else FAIL are declared the equivalent to the C function. However, in most
 * cases the Java method will raise an exception instead of returning an error code. See <a href="#ERRORS">Errors and
 * Exceptions</a> below.
 * <p>
 * Java does not support pass by reference of arguments, so arguments that are returned through <b>OUT</b> parameters
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
 * public synchronized static native int HDF5dummy(int args[]);
 * </pre>
 *
 * where <i>a1</i> is <i>args[0]</i> and <i>a2</i> is <i>args[1]</i>, and would be invoked:
 *
 * <pre>
 * H5.HDF5dummy(a);
 * </pre>
 *
 * <p>
 * All the routines where this convention is used will have specific documentation of the details, given below.
 * <p>
 * <a NAME="ARRAYS"> <b>Arrays</b> </a>
 * <p>
 * HDF5 needs to read and write multi-dimensional arrays of any number type (and records). The HDF5 API describes the
 * layout of the source and destination, and the data for the array passed as a block of bytes, for instance,
 *
 * <pre>
 *      herr_t H5Dread(long fid, long filetype, long memtype, long memspace,
 *      void * data);
 * </pre>
 *
 * <p>
 * where ``void *'' means that the data may be any valid numeric type, and is a contiguous block of bytes that is the
 * data for a multi-dimensional array. The other parameters describe the dimensions, rank, and datatype of the array on
 * disk (source) and in memory (destination).
 * <p>
 * For Java, this ``ANY'' is a problem, as the type of data must always be declared. Furthermore, multidimensional
 * arrays are definitely <i>not</i> layed out contiguously in memory. It would be infeasible to declare a separate
 * routine for every combination of number type and dimensionality. For that reason, the <a
 * href="./hdf.hdf5lib.HDFArray.html"><b>HDFArray</b></a> class is used to discover the type, shape, and size of the
 * data array at run time, and to convert to and from a contiguous array of bytes in synchronized static native C order.
 * <p>
 * The upshot is that any Java array of numbers (either primitive or sub-classes of type <b>Number</b>) can be passed as
 * an ``Object'', and the Java API will translate to and from the appropriate packed array of bytes needed by the C
 * library. So the function above would be declared:
 *
 * <pre>
 * public synchronized static native int H5Dread(long fid, long filetype, long memtype, long memspace, Object data);
 * </pre>
 *            OPEN_IDS.addElement(id);

 * and the parameter <i>data</i> can be any multi-dimensional array of numbers, such as float[][], or int[][][], or
 * Double[][].
 * <p>
 * <a NAME="CONSTANTS"> <b>HDF-5 Constants</b></a>
 * <p>
 * The HDF-5 API defines a set of constants and enumerated values. Most of these values are available to Java programs
 * via the class <a href="./hdf.hdf5lib.HDF5Constants.html"> <b>HDF5Constants</b></a>. For example, the parameters for
 * the h5open() call include two numeric values, <b><i>HDFConstants.H5F_ACC_RDWR</i></b> and
 * <b><i>HDF5Constants.H5P_DEFAULT</i></b>. As would be expected, these numbers correspond to the C constants
 * <b><i>H5F_ACC_RDWR</i></b> and <b><i>H5P_DEFAULT</i></b>.
 * <p>
 * The HDF-5 API defines a set of values that describe number types and sizes, such as "H5T_NATIVE_INT" and "hsize_t".
 * These values are determined at run time by the HDF-5 C library. To support these parameters, the Java class <a
 * href="./hdf.hdf5lib.HDF5CDataTypes.html"> <b>HDF5CDataTypes</b></a> looks up the values when initiated. The values
 * can be accessed as public variables of the Java class, such as:
 *
 * <pre>
 * long data_type = HDF5CDataTypes.JH5T_NATIVE_INT;
 * </pre>
 *
 * The Java application uses both types of constants the same way, the only difference is that the
 * <b><i>HDF5CDataTypes</i></b> may have different values on different platforms.
 * <p>
 * <a NAME="ERRORS"> <b>Error handling and Exceptions</b></a>
 * <p>
 * The HDF5 error API (H5E) manages the behavior of the error stack in the HDF-5 library. This API is omitted from the
 * JHI5. Errors are converted into Java exceptions. This is totally different from the C interface, but is very natural
 * for Java programming.
 * <p>
 * The exceptions of the JHI5 are organized as sub-classes of the class <a
 * href="./hdf.hdf5lib.exceptions.HDF5Exception.html"> <b>HDF5Exception</b></a>. There are two subclasses of
 * <b>HDF5Exception</b>, <a href="./hdf.hdf5lib.exceptions.HDF5LibraryException.html"> <b>HDF5LibraryException</b></a>
 * and <a href="./hdf.hdf5lib.exceptions.HDF5JavaException.html"> <b>HDF5JavaException</b></a>. The sub-classes of the
 * former represent errors from the HDF-5 C library, while sub-classes of the latter represent errors in the JHI5
 * wrapper and support code.
 * <p>
 * The super-class <b><i>HDF5LibraryException</i></b> implements the method '<b><i>printStackTrace()</i></b>', which
 * prints out the HDF-5 error stack, as described in the HDF-5 C API <i><b>H5Eprint()</b>.</i> This may be used by Java
 * exception handlers to print out the HDF-5 error stack.
 * <hr>
 *
 * @version HDF5 1.9 <BR>
 *          <b>See also: <a href ="./hdf.hdf5lib.HDFArray.html"> hdf.hdf5lib.HDFArray</a> </b><BR>
 *          <a href ="./hdf.hdf5lib.HDF5Constants.html"> hdf.hdf5lib.HDF5Constants</a><BR>
 *          <a href ="./hdf.hdf5lib.HDF5CDataTypes.html"> hdf.hdf5lib.HDF5CDataTypes</a><BR>
 *          <a href ="./hdf.hdf5lib.HDF5Exception.html"> hdf.hdf5lib.HDF5Exception</a><BR>
 *          <a href="http://hdfgroup.org/HDF5/"> http://hdfgroup.org/HDF5"</a>
 **/
public class H5 implements java.io.Serializable {
    /**
     *
     */
    private static final long serialVersionUID = 6129888282117053288L;

    private final static org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(H5.class);

    /**
     * The version number of the HDF5 library:
     * LIB_VERSION[0]: The major version of the library.
     * LIB_VERSION[1]: The minor version of the library.
     * LIB_VERSION[2]: The release number of the library.
     *
     * Make sure to update the versions number when a different library is used.
     */
    public final static int LIB_VERSION[] = { 1, 9, 9999 };

    public final static String H5PATH_PROPERTY_KEY = "hdf.hdf5lib.H5.hdf5lib";

    // add system property to load library by name from library path, via
    // System.loadLibrary()
    public final static String H5_LIBRARY_NAME_PROPERTY_KEY = "hdf.hdf5lib.H5.loadLibraryName";
    private static String s_libraryName;
    private static boolean isLibraryLoaded = false;

    private final static boolean IS_CRITICAL_PINNING = true;
    // change from Vector to LinkedHashSet - jp 6-Oct-2014
    private final static LinkedHashSet<Long> OPEN_IDS = new LinkedHashSet<Long>();

    static {
        loadH5Lib();
    }

    public static void loadH5Lib() {
        // Make sure that the library is loaded only once
        if (isLibraryLoaded)
            return;

        // first try loading library by name from user supplied library path
        s_libraryName = System.getProperty(H5_LIBRARY_NAME_PROPERTY_KEY, null);
        String mappedName = null;
        if ((s_libraryName != null) && (s_libraryName.length() > 0)) {
            try {
                mappedName = System.mapLibraryName(s_libraryName);
                System.loadLibrary(s_libraryName);
                isLibraryLoaded = true;
            }
            catch (Throwable err) {
                err.printStackTrace();
                isLibraryLoaded = false;
            }
            finally {
                log.info("HDF5 library: " + s_libraryName);
                log.debug(" resolved to: " + mappedName + "; ");
                log.info((isLibraryLoaded ? "" : " NOT") + " successfully loaded from system property");
            }
        }

        if (!isLibraryLoaded) {
            // else try loading library via full path
            String filename = System.getProperty(H5PATH_PROPERTY_KEY, null);
            if ((filename != null) && (filename.length() > 0)) {
                File h5dll = new File(filename);
                if (h5dll.exists() && h5dll.canRead() && h5dll.isFile()) {
                    try {
                        System.load(filename);
                        isLibraryLoaded = true;
                    }
                    catch (Throwable err) {
                        err.printStackTrace();
                        isLibraryLoaded = false;
                    }
                    finally {
                        log.info("HDF5 library: ");
                        log.debug(filename);
                        log.info((isLibraryLoaded ? "" : " NOT") + " successfully loaded.");
                    }
                }
                else {
                    isLibraryLoaded = false;
                    throw (new UnsatisfiedLinkError("Invalid HDF5 library, " + filename));
                }
            }
        }

        // else load standard library
        if (!isLibraryLoaded) {
            try {
                s_libraryName = "hdf5_java";
                mappedName = System.mapLibraryName(s_libraryName);
                System.loadLibrary("hdf5_java");
                isLibraryLoaded = true;
            }
            catch (Throwable err) {
                err.printStackTrace();
                isLibraryLoaded = false;
            }
            finally {
                log.info("HDF5 library: " + s_libraryName);
                log.debug(" resolved to: " + mappedName + "; ");
                log.info((isLibraryLoaded ? "" : " NOT") + " successfully loaded from java.library.path");
            }
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
     * Get number of open IDs.
     *
     * @return Returns a count of open IDs
     */
    public final static int getOpenIDCount() {
        return OPEN_IDS.size();
    }

    /**
     * Get the open IDs
     *
     * @return Returns a collection of open IDs
     */
    public final static Collection<Long> getOpenIDs() {
        return OPEN_IDS;
    }

    /**
     * H5check_version verifies that the arguments match the version numbers compiled into the library.
     *
     * @param majnum
     *            The major version of the library.
     * @param minnum
     *            The minor version of the library.
     * @param relnum
     *            The release number of the library.
     * @return a non-negative value if successful. Upon failure (when the versions do not match), this function causes
     *         the application to abort (i.e., crash)
     *
     *         See C API function: herr_t H5check_version()
     **/
    public synchronized static native int H5check_version(int majnum, int minnum, int relnum);

    /**
     * H5close flushes all data to disk, closes all file identifiers, and cleans up all memory used by the library.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5close() throws HDF5LibraryException;

    /**
     * H5open initialize the library.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5open() throws HDF5LibraryException;

    /**
     * H5dont_atexit indicates to the library that an atexit() cleanup routine should not be installed. In order to be
     * effective, this routine must be called before any other HDF function calls, and must be called each time the
     * library is loaded/linked into the application (the first time and after it's been unloaded).
     * <P>
     * This is called by the static initializer, so this should never need to be explicitly called by a Java program.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    private synchronized static native int H5dont_atexit() throws HDF5LibraryException;

    /**
     * Turn off error handling By default, the C library prints the error stack of the HDF-5 C library on stdout. This
     * behavior may be disabled by calling H5error_off().
     *
     * @return a non-negative value if successful
     */
    public synchronized static native int H5error_off();

    /**
     * H5garbage_collect collects on all free-lists of all types.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5garbage_collect() throws HDF5LibraryException;

    /**
     * H5get_libversion retrieves the major, minor, and release numbers of the version of the HDF library which is
     * linked to the application.
     *
     * @param libversion
     *            The version information of the HDF library.
     *
     *            <pre>
     *      libversion[0] = The major version of the library.
     *      libversion[1] = The minor version of the library.
     *      libversion[2] = The release number of the library.
     * </pre>
     * @return a non-negative value if successful, along with the version information.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5get_libversion(int[] libversion) throws HDF5LibraryException;

    public synchronized static native int H5set_free_list_limits(int reg_global_lim, int reg_list_lim,
            int arr_global_lim, int arr_list_lim, int blk_global_lim, int blk_list_lim) throws HDF5LibraryException;

    /**
     * H5export_dataset is a utility function to save data in a file.
     *
     * @param file_export_name
     *            The file name to export data into.
     * @param file_name
     *            The name of the HDF5 file containing the dataset.
     * @param object_path
     *            The full path of the dataset to be exported.
     * @param binary_order
     *            99 - export data as text.
     *            1 - export data as binary Native Order.
     *            2 - export data as binary Little Endian.
     *            3 - export data as binary Big Endian.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5export_dataset(String file_export_name, String file_name,
            String object_path, int binary_order) throws HDF5LibraryException;

    /**
     * H5is_library_threadsafe Checks to see if the library was built with thread-safety enabled.
     *
     * @return true if hdf5 library implements threadsafe
     *
     **/
    private synchronized static native boolean H5is_library_threadsafe();

    // /////// unimplemented ////////
    // H5_DLL herr_t H5free_memory(void *mem);
    // H5_DLL void *H5allocate_memory(size_t size, hbool_t clear);
    // H5_DLL void *H5resize_memory(void *mem, size_t size);

    // ////////////////////////////////////////////////////////////
    // //
    // H5A: HDF5 1.8 Attribute Interface API Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * H5Aclose terminates access to the attribute specified by its identifier, attr_id.
     *
     * @param attr_id
     *            IN: Attribute to release access to.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Aclose(long attr_id) throws HDF5LibraryException {
        if (attr_id < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Aclose remove {}", attr_id);
        OPEN_IDS.remove(attr_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Aclose(attr_id);
    }

    private synchronized static native int _H5Aclose(long attr_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Acopy(long src_aid, long dst_aid) throws HDF5LibraryException;

    /**
     * H5Acreate creates an attribute, attr_name, which is attached to the object specified by the identifier loc_id.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - Name is null.
     **/
    public static long H5Acreate(long loc_id, String attr_name, long type_id, long space_id, long acpl_id, long aapl_id)
            throws HDF5LibraryException, NullPointerException {
        long id = _H5Acreate2(loc_id, attr_name, type_id, space_id, acpl_id, aapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5A create add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    /**
     * H5Acreate2 an attribute, attr_name, which is attached to the object specified by the identifier loc_id.
     *
     * @see public static long H5Acreate( long loc_id, String attr_name, long type_id, long space_id, long acpl_id, long
     *      aapl_id )
     **/
    private synchronized static native long _H5Acreate2(long loc_id, String attr_name, long type_id, long space_id,
            long acpl_id, long aapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Acreate_by_name creates an attribute, attr_name, which is attached to the object specified by loc_id and
     * obj_name.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Acreate_by_name(long loc_id, String obj_name, String attr_name, long type_id, long space_id,
            long acpl_id, long aapl_id, long lapl_id) throws HDF5LibraryException, NullPointerException {
        long id = _H5Acreate_by_name(loc_id, obj_name, attr_name, type_id, space_id, acpl_id, aapl_id, lapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Acreate_by_name add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Acreate_by_name(long loc_id, String obj_name, String attr_name,
            long type_id, long space_id, long acpl_id, long aapl_id, long lapl_id) throws HDF5LibraryException,
            NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Adelete(long loc_id, String name) throws HDF5LibraryException,
            NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - obj_name is null.
     **/
    public synchronized static native void H5Adelete_by_idx(long loc_id, String obj_name, int idx_type, int order,
            long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Adelete_by_name removes the attribute attr_name from an object specified by location and name, loc_id and
     * obj_name, respectively.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Adelete_by_name(long loc_id, String obj_name, String attr_name, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - attr_name is null.
     **/
    public synchronized static native boolean H5Aexists(long obj_id, String attr_name) throws HDF5LibraryException,
            NullPointerException;

    /**
     * H5Aexists_by_name determines whether the attribute attr_name exists on an object. That object is specified by its
     * location and name, loc_id and obj_name, respectively.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native boolean H5Aexists_by_name(long loc_id, String obj_name, String attr_name,
            long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Aget_info retrieves attribute information, by attribute identifier.
     *
     * @param attr_id
     *            IN: Attribute identifier
     *
     * @return A buffer(H5A_info_t) for Attribute information
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native H5A_info_t H5Aget_info(long attr_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - obj_name is null.
     **/
    public synchronized static native H5A_info_t H5Aget_info_by_idx(long loc_id, String obj_name, int idx_type,
            int order, long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - obj_name is null.
     **/
    public synchronized static native H5A_info_t H5Aget_info_by_name(long loc_id, String obj_name, String attr_name,
            long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Aget_name retrieves the name of an attribute specified by the identifier, attr_id.
     *
     * @param attr_id
     *            IN: Identifier of the attribute.
     *
     * @return String for Attribute name.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native String H5Aget_name(long attr_id)
            throws HDF5LibraryException;

    /**
     * H5Aget_name_by_idx retrieves the name of an attribute that is attached to an object, which is specified by its
     * location and name, loc_id and obj_name, respectively.
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
     *                - Error from the HDF5 Library.
     * @exception NullPointerException
     *                - obj_name is null.
     **/
    public synchronized static native String H5Aget_name_by_idx(long attr_id, String obj_name, int idx_type, int order,
            long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Aget_space retrieves a copy of the dataspace for an attribute.
     *
     * @param attr_id
     *            IN: Identifier of an attribute.
     *
     * @return attribute dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Aget_space(long attr_id) throws HDF5LibraryException {
        long id = _H5Aget_space(attr_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aget_space add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Aget_space(long attr_id) throws HDF5LibraryException;

    /**
     * H5Aget_storage_size returns the amount of storage that is required for the specified attribute, attr_id.
     *
     * @param attr_id
     *            IN: Identifier of the attribute to query.
     *
     * @return the amount of storage size allocated for the attribute; otherwise returns 0 (zero)
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Aget_storage_size(long attr_id) throws HDF5LibraryException;

    /**
     * H5Aget_type retrieves a copy of the datatype for an attribute.
     *
     * @param attr_id
     *            IN: Identifier of an attribute.
     *
     * @return a datatype identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Aget_type(long attr_id) throws HDF5LibraryException {
        long id = _H5Aget_type(attr_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aget_type add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Aget_type(long attr_id) throws HDF5LibraryException;

    /**
     * H5Aopen opens an existing attribute, attr_name, that is attached to an object specified an object identifier,
     * object_id.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - Name is null.
     **/
    public static long H5Aopen(long obj_id, String attr_name, long aapl_id) throws HDF5LibraryException,
            NullPointerException {
        long id = _H5Aopen(obj_id, attr_name, aapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Aopen(long obj_id, String attr_name, long aapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Aopen_by_idx opens an existing attribute that is attached to an object specified by location and name, loc_id
     * and obj_name, respectively
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - Name is null.
     **/
    public static long H5Aopen_by_idx(long loc_id, String obj_name, int idx_type, int order, long n, long aapl_id,
            long lapl_id) throws HDF5LibraryException, NullPointerException {
        long id = _H5Aopen_by_idx(loc_id, obj_name, idx_type, order, n, aapl_id, lapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aopen_by_idx add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Aopen_by_idx(long loc_id, String obj_name, int idx_type, int order,
            long n, long aapl_id, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - obj_name is null.
     **/
    public static long H5Aopen_by_name(long loc_id, String obj_name, String attr_name, long aapl_id, long lapl_id)
            throws HDF5LibraryException, NullPointerException {
        long id = _H5Aopen_by_name(loc_id, obj_name, attr_name, aapl_id, lapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aopen_by_name add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Aopen_by_name(long loc_id, String obj_name, String attr_name,
            long aapl_id, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into buf from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Buffer for data to be read.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - data buffer is null.
     **/
    public synchronized static native int H5Aread(long attr_id, long mem_type_id, byte[] buf)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Aread reads an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is read into data object from the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to read.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param obj
     *            IN: Object for data to be read.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - data buffer is null. See public synchronized static native int H5Aread( )
     **/
    public synchronized static int H5Aread(long attr_id, long mem_type_id, Object obj) throws HDF5Exception,
            NullPointerException {
        HDFArray theArray = new HDFArray(obj);
        byte[] buf = theArray.emptyBytes();

        // This will raise an exception if there is an error
        int status = H5Aread(attr_id, mem_type_id, buf);

        // No exception: status really ought to be OK
        if (status >= 0) {
            obj = theArray.arrayify(buf);
        }

        return status;
    }

    public synchronized static native int H5AreadVL(long attr_id, long mem_type_id, String[] buf)
            throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5AreadComplex(long attr_id, long mem_type_id, String[] buf)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Arename changes the name of attribute that is attached to the object specified by loc_id. The attribute named
     * old_attr_name is renamed new_attr_name.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - Name is null.
     **/
    public synchronized static native int H5Arename(long loc_id, String old_attr_name, String new_attr_name)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Arename_by_name changes the name of attribute that is attached to the object specified by loc_id and obj_name.
     * The attribute named old_attr_name is renamed new_attr_name.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - Name is null.
     **/
    public synchronized static native int H5Arename_by_name(long loc_id, String obj_name, String old_attr_name,
            String new_attr_name, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from buf to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param buf
     *            IN: Data to be written.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - data is null.
     **/
    public synchronized static native int H5Awrite(long attr_id, long mem_type_id, byte[] buf)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Awrite writes an attribute, specified with attr_id. The attribute's memory datatype is specified with
     * mem_type_id. The entire attribute is written from data object to the file.
     *
     * @param attr_id
     *            IN: Identifier of an attribute to write.
     * @param mem_type_id
     *            IN: Identifier of the attribute datatype (in memory).
     * @param obj
     *            IN: Data object to be written.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - data object is null. See public synchronized static native int H5Awrite(int attr_id, int
     *                mem_type_id, byte[] buf);
     **/
    public synchronized static int H5Awrite(long attr_id, long mem_type_id, Object obj)
            throws HDF5Exception, NullPointerException
    {
        HDFArray theArray = new HDFArray(obj);
        byte[] buf = theArray.byteify();

        int retVal = H5Awrite(attr_id, mem_type_id, buf);
        buf = null;
        theArray = null;
        return retVal;
    }

    public synchronized static native int H5AwriteVL(long attr_id, long mem_type_id, String[] buf)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Aget_create_plist retrieves a copy of the attribute creation property list identifier.
     *
     * @param attr_id
     *            IN: Identifier of an attribute.
     *
     * @return identifier for the attribute's creation property list if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Aget_create_plist(long attr_id)
            throws HDF5LibraryException
    {
        long id = _H5Aget_create_plist(attr_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Aget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Aget_create_plist(long attr_id) throws HDF5LibraryException;

    /**
     * H5Aiterate2 iterates over the attributes attached to a dataset, named datatype, or group, as
     * specified by obj_id. For each attribute, user-provided data, op_data, with additional information
     * as defined below, is passed to a user-defined function, op, which operates on that attribute.
     *
     * @param loc_id
     *            IN: Identifier for object to which attributes are attached; may be group, dataset, or named datatype.
     * @param idx_type
     *            IN: The type of index specified by idx_type can be one of the following:
     *                      H5_INDEX_NAME             An alpha-numeric index by attribute name.
     *                      H5_INDEX_CRT_ORDER        An index by creation order.
     * @param order
     *            IN: The order in which the index is to be traversed, as specified by order, can be one of the following:
     *                      H5_ITER_INC     Iteration is from beginning to end, i.e., a top-down iteration
     *                                      incrementing the index position at each step.
     *                      H5_ITER_DEC     Iteration starts at the end of the index, i.e., a bottom-up iteration
     *                                      decrementing the index position at each step.
     *                      H5_ITER_NATIVE  HDF5 iterates in the fastest-available order. No information is provided
     *                                      as to the order, but HDF5 ensures that each element in the index will be
     *                                      visited if the iteration completes successfully.
     * @param idx
     *            IN/OUT: Initial and returned offset within index.
     * @param op
     *            IN: Callback function to operate on each value.
     * @param op_data
     *            IN/OUT: Pointer to any user-efined data for use by operator function.
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
     public synchronized static native int H5Aiterate(long loc_id, int idx_type, int order, long idx,
                 H5A_iterate_cb op, H5A_iterate_t op_data) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Aiterate_by_name iterates over the attributes attached to the dataset or group specified with loc_id
     * and obj_name. For each attribute, user-provided data, op_data, with additional information as defined
     * below, is passed to a user-defined function, op, which operates on that attribute.
     *
     * @param loc_id
     *            IN: Identifier for object to which attributes are attached; may be group, dataset, or named datatype.
     * @param obj_name
     *            IN: Name of object, relative to location.
     * @param idx_type
     *            IN: The type of index specified by idx_type can be one of the following:
     *                      H5_INDEX_NAME             An alpha-numeric index by attribute name.
     *                      H5_INDEX_CRT_ORDER        An index by creation order.
     * @param order
     *            IN: The order in which the index is to be traversed, as specified by order, can be one of the following:
     *                      H5_ITER_INC     Iteration is from beginning to end, i.e., a top-down iteration
     *                                      incrementing the index position at each step.
     *                      H5_ITER_DEC     Iteration starts at the end of the index, i.e., a bottom-up iteration
     *                                      decrementing the index position at each step.
     *                      H5_ITER_NATIVE  HDF5 iterates in the fastest-available order. No information is provided
     *                                      as to the order, but HDF5 ensures that each element in the index will be
     *                                      visited if the iteration completes successfully.
     * @param idx
     *            IN/OUT: Initial and returned offset within index.
     * @param op
     *            IN: Callback function to operate on each value.
     * @param op_data
     *            IN/OUT: Pointer to any user-efined data for use by operator function.
     * @param lapl_id
     *            IN: Link access property list
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
     public synchronized static native int H5Aiterate_by_name(long loc_id, String obj_name, int idx_type,
                        int order, long idx, H5A_iterate_cb op, H5A_iterate_t op_data, long lapl_id) throws HDF5LibraryException, NullPointerException;

    // ////////////////////////////////////////////////////////////
    // //
    // H5D: Datasets Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * H5Dcopy copies the content of one dataset to another dataset.
     *
     * @param src_did
     *            the identifier of the source dataset
     * @param dst_did
     *            the identifier of the destinaiton dataset
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Dcopy(long src_did, long dst_did) throws HDF5LibraryException;

    /**
     * H5Dclose ends access to a dataset specified by dataset_id and releases resources used by it.
     *
     * @param dataset_id
     *            Identifier of the dataset to finish access to.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Dclose(long dataset_id) throws HDF5LibraryException {
        if (dataset_id < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");

        log.trace("OPEN_IDS: H5Dclose remove {}", dataset_id);
        OPEN_IDS.remove(dataset_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Dclose(dataset_id);
    }

    private synchronized static native int _H5Dclose(long dataset_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Dcreate(long loc_id, String name, long type_id, long space_id, long lcpl_id, long dcpl_id,
            long dapl_id) throws HDF5LibraryException, NullPointerException {
        long id = _H5Dcreate2(loc_id, name, type_id, space_id, lcpl_id, dcpl_id, dapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    /**
     * H5Dcreate2 creates a new dataset named name at the location specified by loc_id.
     *
     * @see public static int H5Dcreate(int loc_id, String name, int type_id, int space_id, int lcpl_id, int dcpl_id,
     *      int dapl_id)
     **/
    private synchronized static native long _H5Dcreate2(long loc_id, String name, long type_id, long space_id,
            long lcpl_id, long dcpl_id, long dapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Dcreate_anon(long loc_id, long type_id, long space_id, long dcpl_id, long dapl_id)
            throws HDF5LibraryException {
        long id = _H5Dcreate_anon(loc_id, type_id, space_id, dcpl_id, dapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dcreate_anon add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Dcreate_anon(long loc_id, long type_id, long space_id, long dcpl_id,
            long dapl_id) throws HDF5LibraryException;

    /**
     * H5Dfill explicitly fills the dataspace selection in memory, space_id, with the fill value specified in fill.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native void H5Dfill(byte[] fill, long fill_type, byte[] buf, long buf_type, long space_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Dget_access_plist returns an identifier for a copy of the dataset access property list for a dataset.
     *
     * @param dset_id
     *            IN: Identifier of the dataset to query.
     *
     * @return a dataset access property list identifier
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Dget_access_plist(long dset_id) throws HDF5LibraryException;

    /**
     * H5Dget_create_plist returns an identifier for a copy of the dataset creation property list for a dataset.
     *
     * @param dataset_id
     *            Identifier of the dataset to query.
     * @return a dataset creation property list identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Dget_create_plist(long dataset_id) throws HDF5LibraryException {
        long id = _H5Dget_create_plist(dataset_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Dget_create_plist(long dataset_id) throws HDF5LibraryException;

    /**
     * H5Dget_offset returns the address in the file of the dataset dset_id.
     *
     * @param dset_id
     *            IN: Identifier of the dataset in question
     *
     * @return the offset in bytes.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Dget_offset(long dset_id) throws HDF5LibraryException;

    /**
     * H5Dget_space returns an identifier for a copy of the dataspace for a dataset.
     *
     * @param dataset_id
     *            Identifier of the dataset to query.
     *
     * @return a dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Dget_space(long dataset_id) throws HDF5LibraryException {
        long id = _H5Dget_space(dataset_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dget_space add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Dget_space(long dataset_id) throws HDF5LibraryException;

    /**
     * H5Dget_space_status determines whether space has been allocated for the dataset dset_id.
     *
     * @param dset_id
     *            IN: Identifier of the dataset to query.
     *
     * @return the space allocation status
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Dget_space_status(long dset_id) throws HDF5LibraryException;

    /**
     * H5Dget_storage_size returns the amount of storage that is required for the dataset.
     *
     * @param dataset_id
     *            Identifier of the dataset in question
     *
     * @return he amount of storage space allocated for the dataset.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Dget_storage_size(long dataset_id) throws HDF5LibraryException,
            IllegalArgumentException;

    /**
     * H5Dget_type returns an identifier for a copy of the datatype for a dataset.
     *
     * @param dataset_id
     *            Identifier of the dataset to query.
     *
     * @return a datatype identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Dget_type(long dataset_id) throws HDF5LibraryException {
        long id = _H5Dget_type(dataset_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dget_type add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Dget_type(long dataset_id) throws HDF5LibraryException;

    /**
     * H5Diterate iterates over all the data elements in the memory buffer buf, executing the callback function operator
     * once for each such data element.
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
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native int H5Diterate(byte[] buf, long buf_type, long space_id, H5D_iterate_cb op,
            H5D_iterate_t op_data) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Dopen opens the existing dataset specified by a location identifier and name, loc_id and name, respectively.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Dopen(long loc_id, String name, long dapl_id) throws HDF5LibraryException,
    NullPointerException {
        long id = _H5Dopen2(loc_id, name, dapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Dopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    /**
     * H5Dopen2 opens the existing dataset specified by a location identifier and name, loc_id and name, respectively.
     *
     * @see public static int H5Dopen(int loc_id, String name, int dapl_id)
     **/
    private synchronized static native long _H5Dopen2(long loc_id, String name, long dapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the application
     * memory buffer buf.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - data buffer is null.
     **/
    public synchronized static native int H5Dread(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, byte[] obj, boolean isCriticalPinning) throws HDF5LibraryException,
            NullPointerException;

    public synchronized static int H5Dread(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
            long xfer_plist_id, byte[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static int H5Dread(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
            long xfer_plist_id, Object obj) throws HDF5Exception, HDF5LibraryException, NullPointerException {
        return H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, obj, true);
    }

    /**
     * H5Dread reads a (partial) dataset, specified by its identifier dataset_id, from the file into the application
     * data object.
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
     *                - Failure in the data conversion.
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - data object is null.
     **/
    public synchronized static int H5Dread(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
            long xfer_plist_id, Object obj, boolean isCriticalPinning) throws HDF5Exception, HDF5LibraryException,
            NullPointerException {
        int status = -1;
        boolean is1D = false;

        Class dataClass = obj.getClass();
        if (!dataClass.isArray()) {
            throw (new HDF5JavaException("H5Dread: data is not an array"));
        }

        String cname = dataClass.getName();
        is1D = (cname.lastIndexOf('[') == cname.indexOf('['));
        char dname = cname.charAt(cname.lastIndexOf("[") + 1);
        log.trace("H5Dread: cname={} is1D={} dname={}", cname, is1D, dname);

        if (is1D && (dname == 'B')) {
            log.trace("H5Dread_dname_B");
            status = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (byte[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'S')) {
            log.trace("H5Dread_dname_S");
            status = H5Dread_short(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (short[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'I')) {
            log.trace("H5Dread_dname_I");
            status = H5Dread_int(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (int[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'J')) {
            log.trace("H5Dread_dname_J");
            status = H5Dread_long(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (long[]) obj);
        }
        else if (is1D && (dname == 'F')) {
            log.trace("H5Dread_dname_F");
            status = H5Dread_float(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (float[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'D')) {
            log.trace("H5Dread_dname_D");
            status = H5Dread_double(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                    (double[]) obj, isCriticalPinning);
        }
        else if (H5.H5Tequal(mem_type_id, HDF5Constants.H5T_STD_REF_DSETREG)) {
            log.trace("H5Dread_reg_ref");
            status = H5Dread_reg_ref(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                    (String[]) obj);
        }
        else if (is1D && (dataClass.getComponentType() == String.class)) {
            log.trace("H5Dread_string type");
            if (H5.H5Tis_variable_str(mem_type_id)) {
                status = H5Dread_VLStrings(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (Object[]) obj);
            }
            else {
                status = H5Dread_string(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                        (String[]) obj);
            }
        }
        else {
            // Create a data buffer to hold the data into a Java Array
            HDFArray theArray = new HDFArray(obj);
            byte[] buf = theArray.emptyBytes();
            log.trace("H5Dread_else");

            // will raise exception if read fails
            status = H5Dread(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf,
                    isCriticalPinning);
            if (status >= 0) {
                // convert the data into a Java Array
                obj = theArray.arrayify(buf);
            }

            // clean up these: assign 'null' as hint to gc()
            buf = null;
            theArray = null;
        }

        return status;
    }

    public synchronized static native int H5Dread_double(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, double[] buf, boolean isCriticalPinning)
                    throws HDF5LibraryException, NullPointerException;

    public synchronized static int H5Dread_double(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, double[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dread_double(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dread_float(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, float[] buf, boolean isCriticalPinning)
                    throws HDF5LibraryException, NullPointerException;

    public synchronized static int H5Dread_float(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, float[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dread_float(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dread_int(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, int[] buf, boolean isCriticalPinning) throws HDF5LibraryException,
            NullPointerException;

    public synchronized static int H5Dread_int(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, int[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dread_int(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dread_long(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, long[] buf, boolean isCriticalPinning) throws HDF5LibraryException,
            NullPointerException;

    public synchronized static int H5Dread_long(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, long[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dread_long(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dread_reg_ref(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, String[] buf) throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Dread_reg_ref_data(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, String[] buf) throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Dread_short(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, short[] buf, boolean isCriticalPinning)
                    throws HDF5LibraryException, NullPointerException;

    public synchronized static int H5Dread_short(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, short[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dread_short(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dread_string(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, String[] buf) throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Dread_VLStrings(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, Object[] buf) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Dset_extent sets the current dimensions of the chunked dataset dset_id to the sizes specified in size.
     *
     * @param dset_id
     *            IN: Chunked dataset identifier.
     * @param size
     *            IN: Array containing the new magnitude of each dimension of the dataset.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     **/
    public synchronized static native void H5Dset_extent(long dset_id, long size[]) throws HDF5LibraryException,
    NullPointerException;

    /**
     * H5Dvlen_get_buf_size determines the number of bytes required to store the VL data from the dataset, using the
     * space_id for the selection in the dataset on disk and the type_id for the memory representation of the VL data in
     * memory.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native long H5Dvlen_get_buf_size(long dset_id, long type_id, long space_id)
            throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native int H5Dvlen_reclaim(long type_id, long space_id, long xfer_plist_id, byte[] buf)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application memory buffer
     * buf into the file.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Dwrite(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, byte[] buf, boolean isCriticalPinning) throws HDF5LibraryException,
            NullPointerException;

    public synchronized static int H5Dwrite(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
            long xfer_plist_id, byte[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static int H5Dwrite(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
            long xfer_plist_id, Object obj) throws HDF5Exception, HDF5LibraryException, NullPointerException {
        return H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, obj, true);
    }

    /**
     * H5Dwrite writes a (partial) dataset, specified by its identifier dataset_id, from the application memory data
     * object into the file.
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
     *                - Failure in the data conversion.
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - data object is null.
     **/
    public synchronized static int H5Dwrite(long dataset_id, long mem_type_id, long mem_space_id, long file_space_id,
            long xfer_plist_id, Object obj, boolean isCriticalPinning) throws HDF5Exception, HDF5LibraryException,
            NullPointerException {
        int status = -1;
        boolean is1D = false;

        Class dataClass = obj.getClass();
        if (!dataClass.isArray()) {
            throw (new HDF5JavaException("H5Dread: data is not an array"));
        }

        String cname = dataClass.getName();
        is1D = (cname.lastIndexOf('[') == cname.indexOf('['));
        char dname = cname.charAt(cname.lastIndexOf("[") + 1);

        if (is1D && (dname == 'B')) {
            status = H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (byte[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'S')) {
            status = H5Dwrite_short(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (short[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'I')) {
            status = H5Dwrite_int(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (int[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'J')) {
            status = H5Dwrite_long(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (long[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'F')) {
            status = H5Dwrite_float(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (float[]) obj,
                    isCriticalPinning);
        }
        else if (is1D && (dname == 'D')) {
            status = H5Dwrite_double(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                    (double[]) obj, isCriticalPinning);
        }
        else if (is1D && (dataClass.getComponentType() == String.class)) {
            log.trace("H5Dwrite_string type");
            if (H5.H5Tis_variable_str(mem_type_id)) {
                status = H5Dwrite_VLStrings(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, (Object[]) obj);
            }
            else {
                status = H5Dwrite_string(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id,
                        (String[]) obj);
            }
        }
        else {
            HDFArray theArray = new HDFArray(obj);
            byte[] buf = theArray.byteify();

            // will raise exception on error
            status = H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf,
                    isCriticalPinning);

            // clean up these: assign 'null' as hint to gc()
            buf = null;
            theArray = null;
        }

        return status;
    }

    public synchronized static native int H5Dwrite_double(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, double[] buf, boolean isCriticalPinning)
                    throws HDF5LibraryException, NullPointerException;

    public synchronized static int H5Dwrite_double(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, double[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dwrite_double(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dwrite_float(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, float[] buf, boolean isCriticalPinning)
                    throws HDF5LibraryException, NullPointerException;

    public synchronized static int H5Dwrite_float(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, float[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dwrite_float(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dwrite_int(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, int[] buf, boolean isCriticalPinning) throws HDF5LibraryException,
            NullPointerException;

    public synchronized static int H5Dwrite_int(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, int[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dwrite_int(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dwrite_long(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, long[] buf, boolean isCriticalPinning) throws HDF5LibraryException,
            NullPointerException;

    public synchronized static int H5Dwrite_long(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, long[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dwrite_long(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dwrite_short(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, short[] buf, boolean isCriticalPinning)
                    throws HDF5LibraryException, NullPointerException;

    public synchronized static int H5Dwrite_short(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, short[] buf) throws HDF5LibraryException, NullPointerException {
        return H5Dwrite_short(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf, true);
    }

    public synchronized static native int H5Dwrite_string(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, String[] buf) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Dwrite_VLStrings writes a (partial) variable length String dataset, specified by its identifier dataset_id, from
     * the application memory buffer buf into the file.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/

    public synchronized static native int H5Dwrite_VLStrings(long dataset_id, long mem_type_id, long mem_space_id,
            long file_space_id, long xfer_plist_id, Object[] buf) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Dflush causes all buffers associated with a dataset to be immediately flushed to disk without removing the
     * data from the cache.
     *
     * @param dset_id
     *            IN: Identifier of the dataset to be flushed.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Dflush(long dset_id) throws HDF5LibraryException;

    /**
     * H5Drefresh causes all buffers associated with a dataset to be cleared and immediately re-loaded with updated
     * contents from disk. This function essentially closes the dataset, evicts all metadata associated with it
     * from the cache, and then re-opens the dataset. The reopened dataset is automatically re-registered with the same ID.
     *
     * @param dset_id
     *            IN: Identifier of the dataset to be refreshed.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Drefresh(long dset_id) throws HDF5LibraryException;

    // /////// unimplemented ////////
    // H5_DLL herr_t H5Ddebug(hid_t dset_id);
    // herr_t H5Dgather(hid_t src_space_id, const void *src_buf, hid_t type_id,
    //                  size_t dst_buf_size, void *dst_buf, H5D_gather_func_t op, void *op_data);
    // herr_t H5Dscatter(H5D_scatter_func_t op, void *op_data, hid_t type_id, hid_t dst_space_id, void *dst_buf);

    // ////////////////////////////////////////////////////////////
    // //
    // H5E: Error Stack //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * H5Eauto_is_v2 determines whether the error auto reporting function for an error stack conforms to the H5E_auto2_t
     * typedef or the H5E_auto1_t typedef.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @return boolean true if the error stack conforms to H5E_auto2_t and false if it conforms to H5E_auto1_t.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Eauto_is_v2(long stack_id) throws HDF5LibraryException;

    /**
     * H5Eclear clears the error stack for the current thread. H5Eclear can fail if there are problems initializing the
     * library.
     * <p>
     * This may be used by exception handlers to assure that the error condition in the HDF-5 library has been reset.
     *
     * @return Returns a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Eclear() throws HDF5LibraryException {
        H5Eclear2(HDF5Constants.H5E_DEFAULT);
        return 0;
    }

    /**
     * H5Eclear clears the error stack specified by estack_id, or, if estack_id is set to H5E_DEFAULT, the error stack
     * for the current thread.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static void H5Eclear(long stack_id) throws HDF5LibraryException {
        H5Eclear2(stack_id);
    }

    /**
     * H5Eclear2 clears the error stack specified by estack_id, or, if estack_id is set to H5E_DEFAULT, the error stack
     * for the current thread.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Eclear2(long stack_id) throws HDF5LibraryException;

    /**
     * H5Eclose_msg closes an error message identifier, which can be either a major or minor message.
     *
     * @param err_id
     *            IN: Error message identifier.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Eclose_msg(long err_id) throws HDF5LibraryException;

    /**
     * H5Eclose_stack closes the object handle for an error stack and releases its resources.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Eclose_stack(long stack_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - msg is null.
     **/
    public synchronized static native long H5Ecreate_msg(long cls_id, int msg_type, String msg)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Ecreate_stack creates a new empty error stack and returns the new stack's identifier.
     *
     * @return an error stack identifier
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Ecreate_stack() throws HDF5LibraryException;

    /**
     * H5Eget_class_name retrieves the name of the error class specified by the class identifier.
     *
     * @param class_id
     *            IN: Error class identifier.
     *
     * @return the name of the error class
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native String H5Eget_class_name(long class_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Eget_current_stack copies the current error stack and returns an error stack identifier for the new copy.
     *
     * @return an error stack identifier
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Eget_current_stack() throws HDF5LibraryException;

    /**
     * H5Eset_current_stack replaces the content of the current error stack with a copy of the content of the error
     * stack specified by estack_id.
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Eset_current_stack(long stack_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native String H5Eget_msg(long msg_id, int[] type_list) throws HDF5LibraryException;

    /**
     * H5Eget_num retrieves the number of error records in the error stack specified by estack_id (including major,
     * minor messages and description).
     *
     * @param stack_id
     *            IN: Error stack identifier.
     *
     * @return the number of error messages
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Eget_num(long stack_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Eprint2 prints the error stack specified by estack_id on the specified stream, stream.
     *
     * @param stack_id
     *            IN: Error stack identifier.If the identifier is H5E_DEFAULT, the current error stack will be printed.
     * @param stream
     *            IN: File pointer, or stderr if null.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Eprint2(long stack_id, Object stream) throws HDF5LibraryException;

    /**
     * H5Epop deletes the number of error records specified in count from the top of the error stack specified by
     * estack_id (including major, minor messages and description).
     *
     * @param stack_id
     *            IN: Error stack identifier.
     * @param count
     *            IN: Version of the client library or application to which the error class belongs.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Epop(long stack_id, long count) throws HDF5LibraryException;


    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - file, func, or msg is null.
      **/
    public static void H5Epush(long stack_id, String file, String func, int line,
                long cls_id, long maj_id, long min_id, String msg) throws HDF5LibraryException, NullPointerException
    {
             H5Epush2(stack_id, file, func, line, cls_id, maj_id, min_id, msg);
    }
    public synchronized static native void H5Epush2(long stack_id, String file, String func, int line,
                long cls_id, long maj_id, long min_id, String msg) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Eregister_class registers a client library or application program to the HDF5 error API so that the client
     * library or application program can report errors together with HDF5 library.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native long H5Eregister_class(String cls_name, String lib_name, String version)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Eunregister_class removes the error class specified by class_id.
     *
     * @param class_id
     *            IN: Error class identifier.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Eunregister_class(long class_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - func is null.
     **/
    public static void H5Ewalk(long stack_id, long direction, H5E_walk_cb func, H5E_walk_t client_data) throws HDF5LibraryException, NullPointerException
    {
            H5Ewalk2(stack_id, direction, func, client_data);
    }
    public synchronized static native void H5Ewalk2(long stack_id, long direction, H5E_walk_cb func, H5E_walk_t client_data)
            throws HDF5LibraryException, NullPointerException;

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
    // public synchronized static native void H5Epush2(long err_stack, String file, String func, int line,
    //             long cls_id, long maj_id, long min_id, String msg, ...);

    // ////////////////////////////////////////////////////////////
    // //
    // H5F: File Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * H5Fclose terminates access to an HDF5 file.
     *
     * @param file_id
     *            Identifier of a file to terminate access to.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Fclose(long file_id) throws HDF5LibraryException {
        if (file_id < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Fclose remove {}", file_id);
        OPEN_IDS.remove(file_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Fclose(file_id);
    }

    private synchronized static native int _H5Fclose(long file_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Fopen(String name, int flags, long access_id) throws HDF5LibraryException,
            NullPointerException {
        long id = _H5Fopen(name, flags, access_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Fopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Fopen(String name, int flags, long access_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Freopen reopens an HDF5 file.
     *
     * @param file_id
     *            Identifier of a file to terminate and reopen access to.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @return a new file identifier if successful
     **/
    public static long H5Freopen(long file_id) throws HDF5LibraryException {
        long id = _H5Freopen(file_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Freopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Freopen(long file_id) throws HDF5LibraryException;

    /**
     * H5Fcreate is the primary function for creating HDF5 files.
     *
     * @param name
     *            Name of the file to access.
     * @param flags
     *            File access flags. Possible values include:
     *            <UL>
     *            <LI>
     *            H5F_ACC_RDWR Allow read and write access to file.</LI>
     *            <LI>
     *            H5F_ACC_RDONLY Allow read-only access to file.</LI>
     *            <LI>
     *            H5F_ACC_TRUNC Truncate file, if it already exists, erasing all data previously stored in the file.</LI>
     *            <LI>
     *            H5F_ACC_EXCL Fail if file already exists.</LI>
     *            <LI>
     *            H5P_DEFAULT Apply default file access and creation properties.</LI>
     *            </UL>
     *
     * @param create_id
     *            File creation property list identifier, used when modifying default file meta-data. Use H5P_DEFAULT
     *            for default access properties.
     * @param access_id
     *            File access property list identifier. If parallel file access is desired, this is a collective call
     *            according to the communicator stored in the access_id (not supported in Java). Use H5P_DEFAULT for
     *            default access properties.
     *
     * @return a file identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Fcreate(String name, int flags, long create_id, long access_id) throws HDF5LibraryException,
            NullPointerException {
        long id = _H5Fcreate(name, flags, create_id, access_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Fcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Fcreate(String name, int flags, long create_id, long access_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Fflush causes all buffers associated with a file or object to be immediately flushed (written) to disk without
     * removing the data from the (memory) cache.
     * <P>
     * After this call completes, the file (or object) is in a consistent state and all data written to date is assured
     * to be permanent.
     *
     * @param object_id
     *            Identifier of object used to identify the file. <b>object_id</b> can be any object associated with the
     *            file, including the file itself, a dataset, a group, an attribute, or a named data type.
     * @param scope
     *            specifies the scope of the flushing action, in the case that the HDF-5 file is not a single physical
     *            file.
     *            <P>
     *            Valid values are:
     *            <UL>
     *            <LI>
     *            H5F_SCOPE_GLOBAL Flushes the entire virtual file.</LI>
     *            <LI>
     *            H5F_SCOPE_LOCAL Flushes only the specified file.</LI>
     *            </UL>
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Fflush(long object_id, int scope) throws HDF5LibraryException;

    /**
     * H5Fget_access_plist returns the file access property list identifier of the specified file.
     *
     * @param file_id
     *            Identifier of file to get access property list of
     *
     * @return a file access property list identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Fget_access_plist(long file_id) throws HDF5LibraryException {
        long id = _H5Fget_access_plist(file_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Fget_access_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Fget_access_plist(long file_id) throws HDF5LibraryException;

    /**
     * H5Fget_create_plist returns a file creation property list identifier identifying the creation properties used to
     * create this file.
     *
     * @param file_id
     *            Identifier of the file to get creation property list
     *
     * @return a file creation property list identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Fget_create_plist(long file_id) throws HDF5LibraryException {
        long id = _H5Fget_create_plist(file_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Fget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Fget_create_plist(long file_id) throws HDF5LibraryException;

    public synchronized static native long H5Fget_filesize(long file_id) throws HDF5LibraryException;

    /**
     * H5Fget_freespace returns the amount of space that is unused by any objects in the file.
     *
     * @param file_id
     *            IN: File identifier for a currently-open HDF5 file
     *
     * @return the amount of free space in the file
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Fget_freespace(long file_id) throws HDF5LibraryException;

    /**
     * H5Fget_intent retrieves the intended access mode flag passed with H5Fopen when the file was opened.
     *
     * @param file_id
     *            IN: File identifier for a currently-open HDF5 file
     *
     * @return the intended access mode flag, as originally passed with H5Fopen.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Fget_intent(long file_id) throws HDF5LibraryException;

    /**
     * H5Fget_mdc_hit_rate queries the metadata cache of the target file to obtain its hit rate (cache hits / (cache
     * hits + cache misses)) since the last time hit rate statistics were reset.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @return the double in which the hit rate is returned.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native double H5Fget_mdc_hit_rate(long file_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - metadata_cache is null.
     **/
    public synchronized static native int H5Fget_mdc_size(long file_id, long[] metadata_cache)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
     * H5Fget_name retrieves the name of the file to which the object obj_id belongs.
     *
     * @param obj_id
     *            IN: Identifier of the object for which the associated filename is sought.
     *
     * @return the filename.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native String H5Fget_name(long obj_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Fget_obj_count(long file_id, int types) throws HDF5LibraryException;

    /**
     * H5Fget_obj_ids returns the list of identifiers for all open HDF5 objects fitting the specified criteria.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - obj_id_list is null.
     **/
    public synchronized static native long H5Fget_obj_ids(long file_id, int types, long max_objs, long[] obj_id_list)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Fis_hdf5 determines whether a file is in the HDF5 format.
     *
     * @param name
     *            File name to check format.
     *
     * @return true if is HDF-5, false if not.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native boolean H5Fis_hdf5(String name) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Fmount mounts the file specified by child_id onto the group specified by loc_id and name using the mount
     * properties plist_id.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Fmount(long loc_id, String name, long child_id, long plist_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * Given a mount point, H5Funmount dissassociates the mount point's file from the file mounted there.
     *
     * @param loc_id
     *            The identifier for the location at which the specified file is to be unmounted.
     * @param name
     *            The name of the file to be unmounted.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Funmount(long loc_id, String name) throws HDF5LibraryException,
    NullPointerException;

    /**
     * H5Freset_mdc_hit_rate_stats resets the hit rate statistics counters in the metadata cache associated with the
     * specified file.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Freset_mdc_hit_rate_stats(long file_id) throws HDF5LibraryException;

    /**
     * H5Fget_info returns global information for the file associated with the
     * object identifier obj_id.
     *
     * @param obj_id IN: Object identifier for any object in the file.
     *
     * @return A buffer(H5F_info2_t) for current "global" information about file
     *
     * @exception HDF5LibraryException - Error from the HDF-5 Library.
     **/
    public synchronized static native H5F_info2_t H5Fget_info(long obj_id) throws HDF5LibraryException;

    /**
     * H5Fclear_elink_file_cache evicts all the cached child files in the specified file's external file
     * cache, causing them to be closed if there is nothing else holding them open.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Fclear_elink_file_cache(long file_id) throws HDF5LibraryException;

    /**
     * H5Fstart_swmr_write will activate SWMR writing mode for a file associated with file_id. This routine will
     * prepare and ensure the file is safe for SWMR writing.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Fstart_swmr_write(long file_id) throws HDF5LibraryException;

    /**
     * H5Fstart_mdc_logging starts logging metadata cache events if logging was previously enabled.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Fstart_mdc_logging(long file_id) throws HDF5LibraryException;

    /**
     * H5Fstop_mdc_logging stops logging metadata cache events if logging was previously enabled and is currently ongoing.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Fstop_mdc_logging(long file_id) throws HDF5LibraryException;

    /**
     * H5Fget_mdc_logging_status gets the current metadata cache logging status.
     *
     * @param file_id
     *            IN: Identifier of the target file.
     *
     * @param mdc_logging_status, the status
     *             mdc_logging_status[0] = is_enabled, whether logging is enabled
     *             mdc_logging_status[1] = is_currently_logging, whether events are currently being logged
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - mdc_logging_status is null.
     **/
    public synchronized static native void H5Fget_mdc_logging_status(long file_id, boolean[] mdc_logging_status)
            throws HDF5LibraryException, NullPointerException;



    // /////// unimplemented ////////
    // ssize_t H5Fget_file_image(hid_t file_id, void * buf_ptr, size_t buf_len);
    // herr_t H5Fget_metadata_read_retry_info(hid_t file_id, H5F_retry_info_t *info);
    // ssize_t H5Fget_free_sections(hid_t file_id, H5F_mem_t type, size_t nsects, H5F_sect_info_t *sect_info/*out*/);

    // /**
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
    // * @exception HDF5LibraryException - Error from the HDF-5 Library.
    // **/
    // public synchronized static native Pointer file_handle
    // H5Fget_vfd_handle(int file_id, int fapl)
    //             throws HDF5LibraryException;

    // /**
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
    // * @exception HDF5LibraryException - Error from the HDF-5 Library.
    // * @exception NullPointerException - config_ptr is null.
    // **/
    // public synchronized static native void H5Fget_mdc_config(int file_id, H5AC_cache_config_t config_ptr)
    //             throws HDF5LibraryException, NullPointerException;

    // /**
    // * H5Fset_mdc_config attempts to configure the file's metadata cache
    // according to the configuration supplied.
    // *
    // * @param file_id IN: Identifier of the target file
    // * @param config_ptr IN: Pointer to the instance of H5AC_cache_config_t
    // containing the desired configuration.
    // *
    // * @return none
    // *
    // * @exception HDF5LibraryException - Error from the HDF-5 Library.
    // * @exception NullPointerException - config_ptr is null.
    // **/
    // public synchronized static native int H5Fset_mdc_config(int file_id, H5AC_cache_config_t config_ptr)
    //             throws HDF5LibraryException, NullPointerException;

    // ////////////////////////////////////////////////////////////
    // //
    // H5G: Group Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * H5Gclose releases resources used by a group which was opened by a call to H5Gcreate() or H5Gopen().
     *
     * @param group_id
     *            Group identifier to release.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Gclose(long group_id) throws HDF5LibraryException {
        if (group_id < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Gclose remove {}", group_id);
        OPEN_IDS.remove(group_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Gclose(group_id);
    }

    private synchronized static native int _H5Gclose(long group_id) throws HDF5LibraryException;

    /**
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
     *            IN: Identifier of group access property list. (No group access properties have been implemented at
     *            this time; use H5P_DEFAULT.)
     *
     * @return a valid group identifier
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Gcreate(long loc_id, String name, long lcpl_id, long gcpl_id, long gapl_id)
            throws HDF5LibraryException, NullPointerException {
        long id = _H5Gcreate2(loc_id, name, lcpl_id, gcpl_id, gapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Gcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Gcreate2(long loc_id, String name, long lcpl_id, long gcpl_id,
            long gapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Gcreate_anon creates a new empty group in the file specified by loc_id.
     *
     * @param loc_id
     *            IN: File or group identifier specifying the file in which the new group is to be created.
     * @param gcpl_id
     *            IN: Identifier of group creation property list.
     * @param gapl_id
     *            IN: Identifier of group access property list. (No group access properties have been implemented at
     *            this time; use H5P_DEFAULT.)
     *
     * @return a valid group identifier
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Gcreate_anon(long loc_id, long gcpl_id, long gapl_id) throws HDF5LibraryException {
        long id = _H5Gcreate_anon(loc_id, gcpl_id, gapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Gcreate_anon add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Gcreate_anon(long loc_id, long gcpl_id, long gapl_id)
            throws HDF5LibraryException;

    /**
     * H5Gget_create_plist returns an identifier for the group creation property list associated with the group
     * specified by group_id.
     *
     * @param group_id
     *            IN: Identifier of the group.
     *
     * @return an identifier for the group's creation property list
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Gget_create_plist(long group_id) throws HDF5LibraryException;

    /**
     * H5Gget_info retrieves information about the group specified by group_id. The information is returned in the
     * group_info struct.
     *
     * @param group_id
     *            IN: Identifier of the group.
     *
     * @return a structure in which group information is returned
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native H5G_info_t H5Gget_info(long group_id) throws HDF5LibraryException;

    /**
     * H5Gget_info_by_idx retrieves information about a group, according to the group's position within an index.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native H5G_info_t H5Gget_info_by_idx(long group_id, String group_name, int idx_type,
            int order, long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Gget_info_by_name retrieves information about the group group_name located in the file or group specified by
     * loc_id.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native H5G_info_t H5Gget_info_by_name(long group_id, String name, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * retrieves information of all objects under the group (name) located in the file or group specified by loc_id.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param name
     *            IN: Name of group for which information is to be retrieved
     * @param objNames
     *            OUT: Names of all objects under the group, name.
     * @param objTypes
     *            OUT: Types of all objects under the group, name.
     * @param objRef
     *            OUT: Reference number of all objects under the group, name.
     *
     * @return the number of items found
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     */
    public synchronized static int H5Gget_obj_info_all(long loc_id, String name, String[] objNames, int[] objTypes,
            long[] objRef) throws HDF5LibraryException, NullPointerException {
        if (objNames == null) {
            throw new NullPointerException("H5Gget_obj_info_all(): name array is null");
        }

        return H5Gget_obj_info_all(loc_id, name, objNames, objTypes, null, null, objRef, HDF5Constants.H5_INDEX_NAME);
    }

    public synchronized static int H5Gget_obj_info_all(long loc_id, String name, String[] oname, int[] otype,
            int[] ltype, long[] ref, int indx_type) throws HDF5LibraryException, NullPointerException {
        return H5Gget_obj_info_full(loc_id, name, oname, otype, ltype, null, ref, indx_type, -1);
    }

    public synchronized static int H5Gget_obj_info_all(long loc_id, String name, String[] oname, int[] otype,
            int[] ltype, long[] fno, long[] ref, int indx_type) throws HDF5LibraryException, NullPointerException {
        return H5Gget_obj_info_full(loc_id, name, oname, otype, ltype, fno, ref, oname.length, indx_type, -1);
    }

    public synchronized static int H5Gget_obj_info_full(long loc_id, String name, String[] oname, int[] otype,
            int[] ltype, long[] fno, long[] ref, int indx_type, int indx_order) throws HDF5LibraryException,
            NullPointerException {
        if (oname == null) {
            throw new NullPointerException("H5Gget_obj_info_full(): name array is null");
        }

        if (otype == null) {
            throw new NullPointerException("H5Gget_obj_info_full(): object type array is null");
        }

        if (oname.length == 0) {
            throw new HDF5LibraryException("H5Gget_obj_info_full(): array size is zero");
        }

        if (oname.length != otype.length) {
            throw new HDF5LibraryException("H5Gget_obj_info_full(): name and type array sizes are different");
        }

        if (ltype == null)
            ltype = new int[otype.length];

        if (fno == null)
            fno = new long[ref.length];

        if (indx_type < 0)
            indx_type = HDF5Constants.H5_INDEX_NAME;

        if (indx_order < 0)
            indx_order = HDF5Constants.H5_ITER_INC;

        log.trace("H5Gget_obj_info_full: oname_len={}", oname.length);
        int status = H5Gget_obj_info_full(loc_id, name, oname, otype, ltype, fno, ref, oname.length, indx_type,
                indx_order);
        for (int indx = 0; indx < oname.length; indx++)
            log.trace("H5Gget_obj_info_full: oname={}", oname[indx]);
        return status;
    }

    private synchronized static native int H5Gget_obj_info_full(long loc_id, String name, String[] oname, int[] otype,
            int[] ltype, long[] fno, long[] ref, int n, int indx_type, int indx_order) throws HDF5LibraryException,
            NullPointerException;

    /**
     * H5Gget_obj_info_idx report the name and type of object with index 'idx' in a Group. The 'idx' corresponds to the
     * index maintained by H5Giterate. Each link is returned, so objects with multiple links will be counted once for
     * each link.
     *
     * @param loc_id
     *            IN: file or group ID.
     * @param name
     *            IN: name of the group to iterate, relative to the loc_id
     * @param idx
     *            IN: the index of the object to iterate.
     * @param oname
     *            the name of the object [OUT]
     * @param type
     *            the type of the object [OUT]
     *
     * @return non-negative if successful, -1 if not.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     */
    public synchronized static int H5Gget_obj_info_idx(long loc_id, String name, int idx, String[] oname, int[] type)
            throws HDF5LibraryException, NullPointerException {
        String n[] = new String[1];
        n[0] = new String("");
        oname[0] = H5Lget_name_by_idx(loc_id, name, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, idx,
                HDF5Constants.H5P_DEFAULT);
        H5L_info_t info = H5Lget_info_by_idx(loc_id, name, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, idx,
                HDF5Constants.H5P_DEFAULT);
        type[0] = info.type;
        return 0;
    }

    /*
     * Add these methods so that we don't need to call
     * in a loop to get information for all the object in a group, which takes
     * a lot of time to finish if the number of objects is more than 10,000
     */
    /**
     * retrieves information of all objects (recurvisely) under the group (name) located in the file or group specified
     * by loc_id upto maximum specified by objMax.
     *
     * @param loc_id
     *            IN: File or group identifier
     * @param objNames
     *            OUT: Names of all objects under the group, name.
     * @param objTypes
     *            OUT: Types of all objects under the group, name.
     * @param lnkTypes
     *            OUT: Types of all links under the group, name.
     * @param objRef
     *            OUT: Reference number of all objects under the group, name.
     * @param objMax
     *            IN: Maximum number of all objects under the group, name.
     *
     * @return the number of items found
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     */
    public synchronized static int H5Gget_obj_info_max(long loc_id, String[] objNames, int[] objTypes, int[] lnkTypes,
            long[] objRef, long objMax) throws HDF5LibraryException, NullPointerException {
        if (objNames == null) {
            throw new NullPointerException("H5Gget_obj_info_max(): name array is null");
        }

        if (objTypes == null) {
            throw new NullPointerException("H5Gget_obj_info_max(): object type array is null");
        }

        if (lnkTypes == null) {
            throw new NullPointerException("H5Gget_obj_info_max(): link type array is null");
        }

        if (objNames.length <= 0) {
            throw new HDF5LibraryException("H5Gget_obj_info_max(): array size is zero");
        }

        if (objMax <= 0) {
            throw new HDF5LibraryException("H5Gget_obj_info_max(): maximum array size is zero");
        }

        if (objNames.length != objTypes.length) {
            throw new HDF5LibraryException("H5Gget_obj_info_max(): name and type array sizes are different");
        }

        return H5Gget_obj_info_max(loc_id, objNames, objTypes, lnkTypes, objRef, objMax, objNames.length);
    }

    private synchronized static native int H5Gget_obj_info_max(long loc_id, String[] oname, int[] otype, int[] ltype,
            long[] ref, long amax, int n) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Gn_members report the number of objects in a Group. The 'objects' include everything that will be visited by
     * H5Giterate. Each link is returned, so objects with multiple links will be counted once for each link.
     *
     * @param loc_id
     *            file or group ID.
     * @param name
     *            name of the group to iterate, relative to the loc_id
     *
     * @return the number of members in the group or -1 if error.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     */
    public synchronized static long H5Gn_members(long loc_id, String name) throws HDF5LibraryException,
            NullPointerException {
        long grp_id = H5Gopen(loc_id, name, HDF5Constants.H5P_DEFAULT);
        long n = -1;

        try {
            H5G_info_t info = H5.H5Gget_info(grp_id);
            n = info.nlinks;
        }
        finally {
            H5Gclose(grp_id);
        }

        return n;
    }

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Gopen(long loc_id, String name, long gapl_id) throws HDF5LibraryException,
    NullPointerException {
        long id = _H5Gopen2(loc_id, name, gapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Gopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Gopen2(long loc_id, String name, long gapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Gflush causes all buffers associated with a group to be immediately flushed to disk without
     * removing the data from the cache.
     *
     * @param group_id
     *            IN: Identifier of the group to be flushed.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Gflush(long group_id) throws HDF5LibraryException;

    /**
     * H5Grefresh causes all buffers associated with a group to be cleared and immediately re-loaded
     * with updated contents from disk. This function essentially closes the group, evicts all metadata
     * associated with it from the cache, and then re-opens the group. The reopened group is automatically
     * re-registered with the same ID.
     *
     * @param group_id
     *            IN: Identifier of the group to be refreshed.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Grefresh(long group_id) throws HDF5LibraryException;

    // ////////////////////////////////////////////////////////////
    // //
    // H5I: HDF5 1.8 Identifier Interface API Functions //
    // //
    // ////////////////////////////////////////////////////////////

    public synchronized static native long H5Iget_file_id(long obj_id) throws HDF5LibraryException;

    @Deprecated
    public synchronized static native long H5Iget_name_long(long obj_id, String[] name, long size)
            throws HDF5LibraryException, NullPointerException;
    /**
     * H5Iget_name_str retrieves the name of an object specified by the identifier, obj_id.
     *
     * @param obj_id
     *            IN: Identifier of the object.
     *
     * @return String for Attribute name.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native String H5Iget_name(long obj_id)
            throws HDF5LibraryException;

    public synchronized static native int H5Iget_ref(long obj_id) throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Idec_ref(long obj_id) throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Iinc_ref(long obj_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Iget_type retrieves the type of the object identified by obj_id.
     *
     * @param obj_id
     *            IN: Object identifier whose type is to be determined.
     *
     * @return the object type if successful; otherwise H5I_BADID.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Iget_type(long obj_id) throws HDF5LibraryException;

    /**
     * H5Iget_type_ref retrieves the reference count on an ID type. The reference count is used by the library to
     * indicate when an ID type can be destroyed.
     *
     * @param type_id
     *            IN: The identifier of the type whose reference count is to be retrieved
     *
     * @return The current reference count on success, negative on failure.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Iget_type_ref(long type_id) throws HDF5LibraryException;

    /**
     * H5Idec_type_ref decrements the reference count on an identifier type. The reference count is used by the
     * library to indicate when an identifier type can be destroyed. If the reference count reaches zero,
     * this function will destroy it.
     *
     * @param type_id
     *            IN: The identifier of the type whose reference count is to be decremented
     *
     * @return The current reference count on success, negative on failure.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Idec_type_ref(long type_id) throws HDF5LibraryException;

    /**
     * H5Iinc_type_ref increments the reference count on an ID type. The reference count is used by the library
     * to indicate when an ID type can be destroyed.
     *
     * @param type_id
     *            IN: The identifier of the type whose reference count is to be incremented
     *
     * @return The current reference count on success, negative on failure.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Iinc_type_ref(long type_id) throws HDF5LibraryException;

    /**
     * H5Inmembers returns the number of identifiers of the identifier type specified in type.
     *
     * @param type_id
     *            IN: Identifier for the identifier type whose member count will be retrieved
     *
     * @return Number of identifiers of the specified identifier type
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Inmembers(long type_id) throws HDF5LibraryException;

    /**
     * H5Iis_valid indicates if the identifier type specified in obj_id is valid.
     *
     * @param obj_id
     *            IN: Identifier to be checked
     *
     * @return a boolean, true if the specified identifier id is valid
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Iis_valid(long obj_id) throws HDF5LibraryException;

    /**
     * H5Itype_exists indicates if the identifier type specified in type exists.
     *
     * @param type_id
     *            IN: the identifier type to be checked
     *
     * @return a boolean, true if the specified identifier type exists
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Itype_exists(int type_id) throws HDF5LibraryException;


    /**
     * H5Iclear_type deletes all identifiers of the type identified by the argument type.
     *
     * @param type_id
     *            IN: Identifier of identifier type which is to be cleared of identifiers
     * @param force
     *            IN: Whether or not to force deletion of all identifiers
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Iclear_type(int type_id, boolean force) throws HDF5LibraryException;

    /**
     * H5Idestroy_type deletes an entire identifier type. All identifiers of this type are destroyed
     * and no new identifiers of this type can be registered.
     *
     * @param type_id
     *            IN: Identifier of identifier type which is to be destroyed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Idestroy_type(int type_id) throws HDF5LibraryException;

    // /////// unimplemented ////////

    // void *H5Iobject_verify(hid_t id, H5I_type_t id_type);

    // hid_t H5Iregister(H5I_type_t type, const void *object);

    // H5I_type_t H5Iregister_type(size_t hash_size, unsigned reserved, H5I_free_t free_func);

    // void *H5Iremove_verify(hid_t id, H5I_type_t id_type);

    // void *H5Isearch(H5I_type_t type, H5I_search_func_t func, void *key);

    // //////////////////////////////////////////////////////////////////
    // H5L: Link Interface Functions //
    // //////////////////////////////////////////////////////////////////

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native void H5Lcopy(long src_loc, String src_name, long dst_loc, String dst_name,
            long lcpl_id, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Lcreate_external creates a new soft link to an external object, which is an object in a different HDF5 file
     * from the location of the link.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native void H5Lcreate_external(String file_name, String obj_name, long link_loc_id,
            String link_name, long lcpl_id, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - cur_name or dst_name is null.
     **/
    public synchronized static native void H5Lcreate_hard(long cur_loc, String cur_name, long dst_loc, String dst_name,
            long lcpl_id, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - link_name is null.
     **/
    public synchronized static native void H5Lcreate_soft(String link_target, long link_loc_id, String link_name,
            long lcpl_id, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native void H5Ldelete(long loc_id, String name, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Ldelete_by_idx removes the nth link in a group according to the specified order and in the specified index.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - group_name is null.
     **/
    public synchronized static native void H5Ldelete_by_idx(long loc_id, String group_name, int idx_type, int order,
            long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native boolean H5Lexists(long loc_id, String name, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native H5L_info_t H5Lget_info(long loc_id, String name, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Lget_info_by_idx opens a named datatype at the location specified by loc_id and return an identifier for the
     * datatype.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - group_name is null.
     **/
    public synchronized static native H5L_info_t H5Lget_info_by_idx(long loc_id, String group_name, int idx_type,
            int order, long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Lget_name_by_idx retrieves name of the nth link in a group, according to the order within a specified field or
     * index.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - group_name is null.
     **/
    public synchronized static native String H5Lget_name_by_idx(long loc_id, String group_name, int idx_type,
            int order, long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Lget_value(long loc_id, String name, String[] link_value, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - group_name is null.
     **/
    public synchronized static native int H5Lget_value_by_idx(long loc_id, String group_name, int idx_type, int order,
            long n, String[] link_value, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Literate(long grp_id, int idx_type, int order, long idx, H5L_iterate_cb op,
            H5L_iterate_t op_data) throws HDF5LibraryException;

    /**
     * H5Literate_by_name iterates through links in a group.
     *
     * @param grp_id
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
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - group_name is null.
     **/
    public synchronized static native int H5Literate_by_name(long grp_id, String group_name, int idx_type, int order,
            long idx, H5L_iterate_cb op, H5L_iterate_t op_data, long lapl_id) throws HDF5LibraryException,
            NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native void H5Lmove(long src_loc, String src_name, long dst_loc, String dst_name,
            long lcpl_id, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Lvisit(long grp_id, int idx_type, int order, H5L_iterate_cb op,
            H5L_iterate_t op_data) throws HDF5LibraryException;

    /**
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
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - group_name is null.
     **/
    public synchronized static native int H5Lvisit_by_name(long loc_id, String group_name, int idx_type, int order,
            H5L_iterate_cb op, H5L_iterate_t op_data, long lapl_id) throws HDF5LibraryException, NullPointerException;


    /**
     * H5Lis_registered tests whether a user-defined link class is currently registered,
     * either by the HDF5 Library or by the user through the use of H5Lregister.
     *
     * @param link_cls_id
     *            IN: User-defined link class identifier
     *
     * @return Returns a positive value if the link class has been registered and zero if it is unregistered.
     *         Otherwise returns a negative value; this may mean that the identifier is not a valid user-defined class identifier.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Lis_registered(int link_cls_id) throws HDF5LibraryException;

    /**
     * H5Lunregister unregisters a class of user-defined links, preventing them from being traversed, queried, moved, etc.
     *
     * @param link_cls_id
     *            IN: User-defined link class identifier
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Lunregister(int link_cls_id) throws HDF5LibraryException;

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
    // H5O: HDF5 1.8 Object Interface API Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * H5Oclose closes the group, dataset, or named datatype specified.
     *
     * @param object_id
     *            IN: Object identifier
     *
     * @return non-negative on success
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Oclose(long object_id) throws HDF5LibraryException {
        if (object_id < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Oclose remove {}", object_id);
        OPEN_IDS.remove(object_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Oclose(object_id);
    }

    private synchronized static native int _H5Oclose(long object_id) throws HDF5LibraryException;

    /**
     * H5Ocopy copies the group, dataset or named datatype specified from the file or group specified by source location
     * to the destination location.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native void H5Ocopy(long src_loc_id, String src_name, long dst_loc_id, String dst_name,
            long ocpypl_id, long lcpl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Oget_comment retrieves the comment for the specified object.
     *
     * @param obj_id
     *            IN: File or group identifier
     *
     * @return the comment
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native String H5Oget_comment(long obj_id) throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Oset_comment sets the comment for the specified object.
     *
     * @param obj_id
     *            IN: Identifier of the target object
     * @param comment
     *            IN: The new comment.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     * @deprecated As of HDF5 1.8 in favor of object attributes.
     **/
    @Deprecated
    public synchronized static native void H5Oset_comment(long obj_id, String comment) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native String H5Oget_comment_by_name(long loc_id, String name, long lapl_id)
            throws HDF5LibraryException, IllegalArgumentException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     *
     * @deprecated As of HDF5 1.8 in favor of object attributes.
     **/
    @Deprecated
    public synchronized static native void H5Oset_comment_by_name(long loc_id, String name, String comment, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Oget_info retrieves the metadata for an object specified by an identifier.
     *
     * @param loc_id
     *            IN: Identifier for target object
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native H5O_info_t H5Oget_info(long loc_id) throws HDF5LibraryException,
            NullPointerException;

    /**
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
     *            IN: Access property list identifier for the link pointing to the object (Not currently used; pass as
     *            H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native H5O_info_t H5Oget_info_by_idx(long loc_id, String group_name, int idx_type,
            int order, long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Oget_info_by_name retrieves the metadata for an object, identifying the object by location and relative name.
     *
     * @param loc_id
     *            IN: File or group identifier specifying location of group in which object is located
     * @param name
     *            IN: Relative name of group
     * @param lapl_id
     *            IN: Access property list identifier for the link pointing to the object (Not currently used; pass as
     *            H5P_DEFAULT.)
     *
     * @return object information
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native H5O_info_t H5Oget_info_by_name(long loc_id, String name, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native void H5Olink(long obj_id, long new_loc_id, String new_name, long lcpl_id,
            long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Oopen(long loc_id, String name, long lapl_id) throws HDF5LibraryException, NullPointerException {
        long id = _H5Oopen(loc_id, name, lapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Oopen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Oopen(long loc_id, String name, long lapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *            IN: User-defined pointer to data required by the application for its processing of the object
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Ovisit(long obj_id, int idx_type, int order, H5O_iterate_cb op,
            H5O_iterate_t op_data) throws HDF5LibraryException, NullPointerException;

    /**
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
     *            IN: User-defined pointer to data required by the application for its processing of the object
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return returns the return value of the first operator that returns a positive value, or zero if all members were
     *         processed with no operator returning non-zero.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Ovisit_by_name(long loc_id, String obj_name, int idx_type, int order,
            H5O_iterate_cb op, H5O_iterate_t op_data, long lapl_id) throws HDF5LibraryException, NullPointerException;


    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native boolean H5Oexists_by_name(long loc_id, String obj_name, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Odecr_refcount decrements the hard link reference count for an object.
     *
     * @param object_id IN: Object identifier
     *
     * @exception HDF5LibraryException - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Odecr_refcount(long object_id) throws HDF5LibraryException;

    /**
     * H5Oincr_refcount increments the hard link reference count for an object.
     *
     * @param object_id IN: Object identifier
     *
     * @exception HDF5LibraryException - Error from the HDF-5 Library.
     **/
     public synchronized static native void H5Oincr_refcount(long object_id) throws HDF5LibraryException;

    /**
     * H5Oopen_by_addr opens a group, dataset, or named datatype using its address within an HDF5 file.
     *
     * @param loc_id IN: File or group identifier
     * @param addr IN: Object's address in the file
     *
     * @return an object identifier for the opened object
     *
     * @exception HDF5LibraryException - Error from the HDF-5 Library.
     **/
    public static long H5Oopen_by_addr(long loc_id, long addr) throws HDF5LibraryException {
        long id = _H5Oopen_by_addr(loc_id, addr);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Oopen_by_addr add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Oopen_by_addr(long loc_id, long addr)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Oopen_by_idx opens the nth object in the group specified.
     *
     * @param loc_id IN: File or group identifier
     * @param group_name IN: Name of group, relative to loc_id, in which object is located
     * @param idx_type IN: Type of index by which objects are ordered
     * @param order IN: Order of iteration within index
     * @param n IN: Object to open
     * @param lapl_id IN: Access property list identifier for the link pointing to the object
     *
     * @return an object identifier for the opened object
     *
     * @exception HDF5LibraryException - Error from the HDF-5 Library.
     * @exception NullPointerException - group_name is null.
     **/
    public static long H5Oopen_by_idx(long loc_id, String group_name,
            int idx_type, int order, long n, long lapl_id) throws HDF5LibraryException, NullPointerException {
        long id = _H5Oopen_by_idx(loc_id, group_name, idx_type, order, n, lapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Oopen_by_idx add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    public synchronized static native long _H5Oopen_by_idx(long loc_id, String group_name,
            int idx_type, int order, long n, long lapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Oflush causes all buffers associated with an object to be immediately flushed to disk without removing
     * the data from the cache. object_id can be any named object associated with an HDF5 file including a
     * dataset, a group, or a committed datatype.
     *
     * @param object_id
     *            IN: Identifier of the object to be flushed.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Oflush(long object_id) throws HDF5LibraryException;

    /**
     * H5Orefresh causes all buffers associated with an object to be cleared and immediately re-loaded with
     * updated contents from disk. This function essentially closes the object, evicts all metadata associated
     * with it from the cache, and then re-opens the object. The reopened object is automatically re-registered
     * with the same ID. object_id can be any named object associated with an HDF5 file including a
     * dataset, a group, or a committed datatype.
     *
     * @param object_id
     *            IN: Identifier of the object to be refreshed.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Orefresh(long object_id) throws HDF5LibraryException;

    // /////// unimplemented ////////

    // ////////////////////////////////////////////////////////////
    // //
    // H5P: Property List Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    // Generic property list routines

    /**
     * H5Pget_class_name retrieves the name of a generic property list class
     *
     * @param plid
     *            IN: Identifier of property object to query
     * @return name of a property list if successful; null if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native String H5Pget_class_name(long plid) throws HDF5LibraryException;

    /**
     * H5Pcreate creates a new property as an instance of some property list class.
     *
     * @param type
     *            IN: The type of property list to create.
     *
     * @return a property list identifier (plist) if successful; otherwise Fail (-1).
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Pcreate(long type) throws HDF5LibraryException {
        long id = _H5Pcreate(type);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Pcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Pcreate(long type) throws HDF5LibraryException;

    /**
     * H5Pget retrieves a copy of the value for a property in a property list (support integer only)
     *
     * @param plid
     *            IN: Identifier of property object to query
     * @param name
     *            IN: Name of property to query
     * @return value for a property if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Pget(long plid, String name) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Pset(long plid, String name, int value) throws HDF5LibraryException;

    /**
     * H5Pexist determines whether a property exists within a property list or class
     *
     * @param plid
     *            IN: Identifier for the property to query
     * @param name
     *            IN: Name of property to check for
     * @return a true value if the property exists in the property object; false if the property does not exist;
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native boolean H5Pexist(long plid, String name) throws HDF5LibraryException;

    /**
     * H5Pget_size retrieves the size of a property's value in bytes
     *
     * @param plid
     *            IN: Identifier of property object to query
     * @param name
     *            IN: Name of property to query
     * @return size of a property's value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native long H5Pget_size(long plid, String name) throws HDF5LibraryException;

    /**
     * H5Pget_nprops retrieves the number of properties in a property list or class
     *
     * @param plid
     *            IN: Identifier of property object to query
     * @return number of properties if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native long H5Pget_nprops(long plid) throws HDF5LibraryException;

    /**
     * H5Pget_class returns the property list class for the property list identified by the plist parameter.
     *
     * @param plist
     *            IN: Identifier of property list to query.
     * @return a property list class if successful. Otherwise returns H5P_ROOT (-1).
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Pget_class(long plist) throws HDF5LibraryException;

    /**
     * H5Pget_class_parent retrieves an identifier for the parent class of a property class
     *
     * @param plid
     *            IN: Identifier of the property class to query
     * @return a valid parent class object identifier if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native long H5Pget_class_parent(long plid) throws HDF5LibraryException;

    /**
     * H5Pequal determines if two property lists or classes are equal
     *
     * @param plid1
     *            IN: First property object to be compared
     * @param plid2
     *            IN: Second property object to be compared
     * @return positive value if equal; zero if unequal, a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Pequal(long plid1, long plid2) throws HDF5LibraryException;

    public static boolean H5P_equal(long plid1, long plid2) throws HDF5LibraryException {
        if (H5Pequal(plid1, plid2) == 1)
            return true;
        return false;
    }

    /**
     * H5Pisa_class checks to determine whether a property list is a member of the specified class
     *
     * @param plist
     *            IN: Identifier of the property list
     * @param pclass
     *            IN: Identifier of the property class
     * @return a positive value if equal; zero if unequal; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Pisa_class(long plist, long pclass) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Pcopy_prop(long dst_id, long src_id, String name)
            throws HDF5LibraryException;

    /**
     * H5Premove removes a property from a property list
     *
     * @param plid
     *            IN: Identifier of the property list to modify
     * @param name
     *            IN: Name of property to remove
     * @return a non-negative value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Premove(long plid, String name) throws HDF5LibraryException;

    /**
     * H5Punregister removes a property from a property list class
     *
     * @param plid
     *            IN: Property list class from which to remove permanent property
     * @param name
     *            IN: Name of property to remove
     * @return a non-negative value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native int H5Punregister(long plid, String name) throws HDF5LibraryException;

    /**
     * Closes an existing property list class
     *
     * @param plid
     *            IN: Property list class to close
     * @return a non-negative value if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public static int H5Pclose_class(long plid) throws HDF5LibraryException {
        if (plid < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Pclose_class remove {}", plid);
        OPEN_IDS.remove(plid);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Pclose_class(plid);
    }

    public synchronized static native int _H5Pclose_class(long plid) throws HDF5LibraryException;

    /**
     * H5Pclose terminates access to a property list.
     *
     * @param plist
     *            IN: Identifier of the property list to terminate access to.
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Pclose(long plist) throws HDF5LibraryException {
        if (plist < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Pclose remove {}", plist);
        OPEN_IDS.remove(plist);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Pclose(plist);
    }

    private synchronized static native int _H5Pclose(long plist) throws HDF5LibraryException;

    /**
     * H5Pcopy copies an existing property list to create a new property list.
     *
     * @param plist
     *            IN: Identifier of property list to duplicate.
     *
     * @return a property list identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Pcopy(long plist) throws HDF5LibraryException {
        long id = _H5Pcopy(plist);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Pcopy add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Pcopy(long plist) throws HDF5LibraryException;

    public static long H5Pcreate_class_nocb(long parent_class, String name) throws HDF5LibraryException {
        long id = _H5Pcreate_class_nocb(parent_class, name);
          if (id > 0) {
            log.trace("OPEN_IDS: H5Pcreate_class_nocb add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Pcreate_class_nocb(long parent_class, String name) throws HDF5LibraryException;

//    public static long H5Pcreate_class(long parent_class, String name, H5P_cls_create_func_cb create_op, H5P_cls_create_func_t create_data,
//             H5P_cls_copy_func_cb copy_op, H5P_cls_copy_func_t copy_data, H5P_cls_close_func_cb close_op, H5P_cls_close_func_t close_data) throws HDF5LibraryException {
//        long id = _H5Pcreate_class(parent_class, name, create_op, create_data, copy_op, copy_data, close_op, close_data);
//          if (id > 0) {
//            log.trace("OPEN_IDS: H5Pcreate_class add {}", id);
//            OPEN_IDS.add(id);
//            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
//        }
//        return id;
//    }
//
//    private synchronized static native long _H5Pcreate_class(long parent_class, String name, H5P_cls_create_func_cb create_op, H5P_cls_create_func_t create_data,
//            H5P_cls_copy_func_cb copy_op, H5P_cls_copy_func_t copy_data, H5P_cls_close_func_cb close_op, H5P_cls_close_func_t close_data) throws HDF5LibraryException;

    public synchronized static native void H5Pregister2_nocb(long plist_class, String name, long size, byte[] def_value) throws HDF5LibraryException;

//    public synchronized static native void H5Pregister2(long plist_class, String name, long size, byte[] def_value, H5P_prp_create_func_cb prp_create, H5P_prp_set_func_cb prp_set,
//          H5P_prp_get_func_cb prp_get, H5P_prp_delete_func_cb prp_delete, H5P_prp_copy_func_cb prp_copy, H5P_prp_compare_func_cb prp_cmp, H5P_prp_close_func_cb prp_close) throws HDF5LibraryException;

     public synchronized static native void H5Pinsert2_nocb(long plist, String name, long size,  byte[] value) throws HDF5LibraryException;


    // public synchronized static native void H5Pinsert2(long plist, String name, long size,  byte[] value, H5P_prp_set_func_cb prp_set, H5P_prp_get_func_cb prp_get,
    //      H5P_prp_delete_func_cb prp_delete, H5P_prp_copy_func_cb prp_copy, H5P_prp_compare_func_cb prp_cmp, H5P_prp_close_func_cb prp_close) throws HDF5LibraryException;

    public synchronized static native int H5Piterate(long plist, int[] idx, H5P_iterate_cb op, H5P_iterate_t op_data) throws HDF5LibraryException;

    // Object creation property list (OCPL) routines

    /**
     * H5Pget_attr_phase_change retrieves attribute storage phase change thresholds.
     *
     * @param ocpl_id
     *            IN: : Object (dataset or group) creation property list identifier
     * @param attributes
     *            The maximun and minimum no. of attributes to be stored.
     *
     *            <pre>
     *      attributes[0] =  The maximum number of attributes to be stored in compact storage
     *      attributes[1] =  The minimum number of attributes to be stored in dense storage
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     *
     **/
    public synchronized static native int H5Pget_attr_phase_change(long ocpl_id, int[] attributes)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native void H5Pset_attr_phase_change(long ocpl_id, int max_compact, int min_dense)
            throws HDF5LibraryException;

    /**
     * H5Pget_attr_creation_order retrieves the settings for tracking and indexing attribute creation order on an object
     *
     * @param ocpl_id
     *            IN: Object (group or dataset) creation property list identifier
     *
     * @return Flags specifying whether to track and index attribute creation order
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pget_attr_creation_order(long ocpl_id) throws HDF5LibraryException;

    /**
     * H5Pset_attr_creation_order sets flags specifying whether to track and index attribute creation order on an
     * object.
     *
     * @param ocpl_id
     *            IN: Object creation property list identifier
     * @param crt_order_flags
     *            IN: Flags specifying whether to track and index attribute creation order
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pset_attr_creation_order(long ocpl_id, int crt_order_flags)
            throws HDF5LibraryException;

    /**
     * H5Pget_obj_track_times queries the object creation property list, ocpl_id, to determine whether object times are
     * being recorded.
     *
     * @param ocpl_id
     *            IN: Object creation property list identifier
     *
     * @return TRUE or FALSE, specifying whether object times are being recorded
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native boolean H5Pget_obj_track_times(long ocpl_id) throws HDF5LibraryException;

    /**
     * H5Pset_obj_track_times sets a property in the object creation property list, ocpl_id, that governs the recording
     * of times associated with an object.
     *
     * @param ocpl_id
     *            IN: Object creation property list identifier
     *
     * @param track_times
     *            IN: TRUE or FALSE, specifying whether object times are to be tracked
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native void H5Pset_obj_track_times(long ocpl_id, boolean track_times)
            throws HDF5LibraryException;

    public synchronized static native int H5Pmodify_filter(long plist, long filter, int flags, long cd_nelmts,
            int[] cd_values) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Pset_filter adds the specified filter and corresponding properties to the end of an output filter pipeline.
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_filter(long plist, int filter, int flags, long cd_nelmts,
            int[] cd_values) throws HDF5LibraryException;

    /**
     * H5Pget_nfilters returns the number of filters defined in the filter pipeline associated with the property list
     * plist.
     *
     * @param plist
     *            IN: Property list identifier.
     *
     * @return the number of filters in the pipeline if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pget_nfilters(long plist) throws HDF5LibraryException;

    /**
     * H5Pget_filter returns information about a filter, specified by its filter number, in a filter pipeline, specified
     * by the property list with which it is associated.
     *
     * @param plist
     *            IN: Property list identifier.
     * @param filter_number
     *            IN: Sequence number within the filter pipeline of the filter for which information is sought.
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
     *                Fatal error on Copyback
     * @exception ArrayStoreException
     *                Fatal error on Copyback
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name or an array is null.
     *
     **/
    public static int H5Pget_filter(long plist, int filter_number, int[] flags, long[] cd_nelmts, int[] cd_values,
            long namelen, String[] name, int[] filter_config) throws ArrayIndexOutOfBoundsException,
            ArrayStoreException, HDF5LibraryException, NullPointerException {
        return H5Pget_filter2(plist, filter_number, flags, cd_nelmts, cd_values, namelen, name, filter_config);
    }

    /**
     * H5Pget_filter2 returns information about a filter, specified by its filter number, in a filter pipeline,
     * specified by the property list with which it is associated.
     *
     * @see public static int H5Pget_filter(int plist, int filter_number, int[] flags, int[] cd_nelmts, int[] cd_values,
     *      int namelen, String[] name, int[] filter_config)
     *
     **/
    private synchronized static native int H5Pget_filter2(long plist, int filter_number, int[] flags, long[] cd_nelmts,
            int[] cd_values, long namelen, String[] name, int[] filter_config) throws ArrayIndexOutOfBoundsException,
            ArrayStoreException, HDF5LibraryException, NullPointerException;

    /**
     * H5Pget_filter_by_id returns information about the filter specified in filter_id, a filter identifier. plist_id
     * must be a dataset or group creation property list and filter_id must be in the associated filter pipeline. The
     * filter_id and flags parameters are used in the same manner as described in the discussion of H5Pset_filter. Aside
     * from the fact that they are used for output, the parameters cd_nelmts and cd_values[] are used in the same manner
     * as described in the discussion of H5Pset_filter. On input, the cd_nelmts parameter indicates the number of
     * entries in the cd_values[] array allocated by the calling program; on exit it contains the number of values
     * defined by the filter. On input, the namelen parameter indicates the number of characters allocated for the
     * filter name by the calling program in the array name[]. On exit name[] contains the name of the filter with one
     * character of the name in each element of the array. If the filter specified in filter_id is not set for the
     * property list, an error will be returned and H5Pget_filter_by_id1 will fail.
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
     *                - Error from the HDF-5 Library.
     * @exception ArrayIndexOutOfBoundsException
     *                Fatal error on Copyback
     * @exception ArrayStoreException
     *                Fatal error on Copyback
     * @exception NullPointerException
     *                - name or an array is null.
     *
     **/
    public static int H5Pget_filter_by_id(long plist_id, long filter_id, int[] flags, long[] cd_nelmts,
            int[] cd_values, long namelen, String[] name, int[] filter_config) throws ArrayIndexOutOfBoundsException,
            ArrayStoreException, HDF5LibraryException, NullPointerException {
        return H5Pget_filter_by_id2(plist_id, filter_id, flags, cd_nelmts, cd_values, namelen, name, filter_config);
    }

    /**
     * H5Pget_filter_by_id2 returns information about a filter, specified by its filter id, in a filter pipeline,
     * specified by the property list with which it is associated.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name or an array is null.
     *
     **/
    public synchronized static native int H5Pget_filter_by_id2(long plist_id, long filter_id, int[] flags,
            long[] cd_nelmts, int[] cd_values, long namelen, String[] name, int[] filter_config)
                    throws HDF5LibraryException, NullPointerException;


    public synchronized static native boolean H5Pall_filters_avail(long dcpl_id) throws HDF5LibraryException,
    NullPointerException;

    public synchronized static native int H5Premove_filter(long obj_id, long filter) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_deflate(long plist, int level) throws HDF5LibraryException;

    public synchronized static native int H5Pset_fletcher32(long plist) throws HDF5LibraryException,
    NullPointerException;

    // File creation property list (FCPL) routines

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     **/
    public synchronized static native int H5Pget_userblock(long plist, long[] size) throws HDF5LibraryException,
    NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_userblock(long plist, long size) throws HDF5LibraryException;

    /**
     * H5Pget_sizes retrieves the size of the offsets and lengths used in an HDF5 file. This function is only valid for
     * file creation property lists.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     * @exception IllegalArgumentException
     *                - size is invalid.
     **/
    public synchronized static native int H5Pget_sizes(long plist, long[] size) throws HDF5LibraryException,
    NullPointerException, IllegalArgumentException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_sizes(long plist, int sizeof_addr, int sizeof_size)
            throws HDF5LibraryException;

    /**
     * H5Pget_sym_k retrieves the size of the symbol table B-tree 1/2 rank and the symbol table leaf node 1/2 size.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     * @exception IllegalArgumentException
     *                - size is invalid.
     **/
    public synchronized static native int H5Pget_sym_k(long plist, int[] size) throws HDF5LibraryException,
    NullPointerException, IllegalArgumentException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_sym_k(long plist, int ik, int lk) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - ik array is null.
     **/
    public synchronized static native int H5Pget_istore_k(long plist, int[] ik) throws HDF5LibraryException,
    NullPointerException;

    /**
     * H5Pset_istore_k sets the size of the parameter used to control the B-trees for indexing chunked datasets.
     *
     * @param plist
     *            IN: Identifier of property list to query.
     * @param ik
     *            IN: 1/2 rank of chunked storage B-tree.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_istore_k(long plist, int ik) throws HDF5LibraryException;

    /**
     * H5Pget_shared_mesg_nindexes retrieves number of shared object header message indexes in file creation property
     * list.
     *
     * @param fcpl_id
     *            IN: : File creation property list identifier
     *
     * @return nindexes, the number of shared object header message indexes available in files created with this
     *         property list
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pget_shared_mesg_nindexes(long fcpl_id) throws HDF5LibraryException;

    /**
     * H5Pset_shared_mesg_nindexes sets the number of shared object header message indexes in the specified file
     * creation property list.
     *
     * @param plist_id
     *            IN: File creation property list
     * @param nindexes
     *            IN: Number of shared object header message indexes to be available in files created with this property
     *            list
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid value of nindexes
     *
     **/
    public synchronized static native int H5Pset_shared_mesg_nindexes(long plist_id, int nindexes)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - mesg_info is null.
     * @exception IllegalArgumentException
     *                - Invalid value of nindexes
     *
     **/
    public synchronized static native int H5Pget_shared_mesg_index(long fcpl_id, int index_num, int[] mesg_info)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid value of nindexes
     *
     **/
    public synchronized static native int H5Pset_shared_mesg_index(long fcpl_id, int index_num, int mesg_type_flags,
            int min_mesg_size) throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_shared_mesg_phase_change retrieves shared object header message phase change information.
     *
     * @param fcpl_id
     *            IN: : File creation property list identifier
     * @param size
     *            The threshold values for storage of shared object header message indexes in a file.
     *
     *            <pre>
     *      size[0] =  Threshold above which storage of a shared object header message index shifts from list to B-tree
     *      size[1] =  Threshold below which storage of a shared object header message index reverts to list format
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     *
     **/
    public synchronized static native int H5Pget_shared_mesg_phase_change(long fcpl_id, int[] size)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Pset_shared_mesg_phase_change sets shared object header message storage phase change thresholds.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     * @param max_list
     *            IN: Threshold above which storage of a shared object header message index shifts from list to B-tree
     * @param min_btree
     *            IN: Threshold below which storage of a shared object header message index reverts to list format
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid values of max_list and min_btree.
     *
     **/
    public synchronized static native int H5Pset_shared_mesg_phase_change(long fcpl_id, int max_list, int min_btree)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pset_file_space sets the file space management strategy for the file associated with fcpl_id to strategy.
     * There are four strategies that applications can select and they are described in the Parameters section.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     * @param strategy
     *            IN: The strategy for file space management.
     *                Passing a value of zero (0) indicates that the value of strategy is not to be modified.
     *                H5F_FILE_SPACE_ALL_PERSIST
     *                        With this strategy, the free-space managers track the free space that results from the
     *                        manipulation of HDF5 objects in the HDF5 file. The free space information is saved when the
     *                        file is closed, and reloaded when the file is reopened. When space is needed for file metadata
     *                        or raw data, the HDF5 library first requests space from the library's free-space managers.
     *                        If the request is not satisfied, the library requests space from the aggregators. If the request
     *                        is still not satisfied, the library requests space from the virtual file driver. That is, the
     *                        library will use all of the mechanisms for allocating space.
     *                H5F_FILE_SPACE_ALL     (Default file space management strategy)
     *                        With this strategy, the free-space managers track the free space that results from the manipulation
     *                        of HDF5 objects in the HDF5 file. The free space information is NOT saved when the file is closed
     *                        and the free space that exists upon file closing becomes unaccounted space in the file.
     *                        Like the previous strategy, the library will try all of the mechanisms for allocating space. When
     *                        space is needed for file metadata or raw data, the library first requests space from the free-space
     *                        managers. If the request is not satisfied, the library requests space from the aggregators. If the
     *                        request is still not satisfied, the library requests space from the virtual file driver.
     *                H5F_FILE_SPACE_AGGR_VFD
     *                        With this strategy, the library does not track free space that results from the manipulation of HDF5
     *                        obejcts in the HDF5 file and the free space becomes unaccounted space in the file.
     *                        When space is needed for file metadata or raw data, the library first requests space from the
     *                        aggregators. If the request is not satisfied, the library requests space from the virtual file driver.
     *                H5F_FILE_SPACE_VFD
     *                        With this strategy, the library does not track free space that results from the manipulation of HDF5
     *                        obejcts in the HDF5 file and the free space becomes unaccounted space in the file.
     *                        When space is needed for file metadata or raw data, the library requests space from the virtual file driver.
     * @param threshold
     *            IN: The free-space section threshold. The library default is 1, which is to track all free-space sections.
     *                Passing a value of zero (0) indicates that the value of threshold is not to be modified.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid values of max_list and min_btree.
     *
     **/
    public synchronized static native void H5Pset_file_space(long fcpl_id, int strategy, long threshold)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_file_space provides the means for applications to manage the HDF5 file's file space for their specific needs.
     *
     * @param fcpl_id
     *            IN: File creation property list identifier
     * @param strategy
     *            IN/OUT: The current file space management strategy in use for the file. NULL, strategy not queried.
     * @param threshold
     *            IN/OUT: The current free-space section threshold. NULL, threshold not queried.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid values of max_list and min_btree.
     *
     **/
    public synchronized static native void H5Pget_file_space(long fcpl_id, int[] strategy, long[] threshold)
            throws HDF5LibraryException, IllegalArgumentException;

    // File access property list (FAPL) routines

    /**
     * H5Pget_alignment retrieves the current settings for alignment properties from a file access property list.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - aligment array is null.
     * @exception IllegalArgumentException
     *                - aligment array is invalid.
     **/
    public synchronized static native int H5Pget_alignment(long plist, long[] alignment) throws HDF5LibraryException,
    NullPointerException, IllegalArgumentException;

    /**
     * H5Pset_alignment sets the alignment properties of a file access property list so that any file object &gt;=
     * THRESHOLD bytes will be aligned on an address which is a multiple of ALIGNMENT.
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_alignment(long plist, long threshold, long alignment)
            throws HDF5LibraryException;

    /**
     * H5Pget_driver returns the identifier of the low-level file driver associated with the file access property list
     * or data transfer property list plid.
     *
     * @param plid
     *            IN: File access or data transfer property list identifier.
     * @return a valid low-level driver identifier if successful; a negative value if failed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     */
    public synchronized static native long H5Pget_driver(long plid) throws HDF5LibraryException;

    public synchronized static native long H5Pget_family_offset(long fapl_id) throws HDF5LibraryException,
    NullPointerException;

    public synchronized static native int H5Pset_family_offset(long fapl_id, long offset) throws HDF5LibraryException,
    NullPointerException;

    /**
     * Retrieves the maximum possible number of elements in the meta data cache and the maximum possible number of bytes
     * and the RDCC_W0 value in the raw data chunk cache.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an array is null.
     **/
    public synchronized static native int H5Pget_cache(long plist, int[] mdc_nelmts, long[] rdcc_nelmts,
            long[] rdcc_nbytes, double[] rdcc_w0) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Pset_cache sets the number of elements (objects) in the meta data cache and the total number of bytes in the
     * raw data chunk cache.
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_cache(long plist, int mdc_nelmts, long rdcc_nelmts, long rdcc_nbytes,
            double rdcc_w0) throws HDF5LibraryException;

    /**
     * H5Pget_mdc_config gets the initial metadata cache configuration contained in a file access property list and
     * loads it into the instance of H5AC_cache_config_t pointed to by the config_ptr parameter. This configuration is
     * used when the file is opened.
     *
     * @param plist_id
     *            IN: Identifier of the file access property list.
     *
     * @return A buffer(H5AC_cache_config_t) for the current metadata cache configuration information
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native H5AC_cache_config_t H5Pget_mdc_config(long plist_id) throws HDF5LibraryException;

    public synchronized static native void H5Pset_mdc_config(long plist_id, H5AC_cache_config_t config_ptr)
            throws HDF5LibraryException;

    /**
     * H5Pget_gc_references Returns the current setting for the garbage collection refernces property from a file access
     * property list.
     *
     * @param fapl_id
     *            IN File access property list
     *
     * @return GC is on (true) or off (false)
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Pget_gc_references(long fapl_id) throws HDF5LibraryException;

    /**
     * H5Pset_gc_references Sets the flag for garbage collecting references for the file. Default value for garbage
     * collecting references is off.
     *
     * @param fapl_id
     *            IN File access property list
     * @param gc_ref
     *            IN set GC on (true) or off (false)
     *
     * @return non-negative if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_gc_references(long fapl_id, boolean gc_ref)
            throws HDF5LibraryException;

    public synchronized static native int H5Pget_fclose_degree(long plist_id) throws HDF5LibraryException,
    NullPointerException;

    public synchronized static native int H5Pset_fclose_degree(long plist, int degree) throws HDF5LibraryException,
    NullPointerException;

    /**
     * H5Pget_meta_block_size the current metadata block size setting.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return the minimum size, in bytes, of metadata block allocations.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native long H5Pget_meta_block_size(long fapl_id) throws HDF5LibraryException;

    /**
     * H5Pset_meta_block_size sets the minimum metadata block size.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param size
     *            IN: Minimum size, in bytes, of metadata block allocations.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native void H5Pset_meta_block_size(long fapl_id, long size) throws HDF5LibraryException;

    public synchronized static native long H5Pget_sieve_buf_size(long fapl_id) throws HDF5LibraryException;

    public synchronized static native void H5Pset_sieve_buf_size(long fapl_id, long size) throws HDF5LibraryException;

    /**
     * H5Pget_small_data_block_size retrieves the size of a block of small data in a file creation property list.
     *
     * @param plist
     *            IN: Identifier for property list to query.
     *
     * @return a non-negative value and the size of the user block; if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Pget_small_data_block_size(long plist) throws HDF5LibraryException;

    /**
     * H5Pset_small_data_block_size reserves blocks of size bytes for the contiguous storage of the raw data portion of
     * small datasets.
     *
     * @param plist
     *            IN: Identifier of property list to modify.
     * @param size
     *            IN: Size of the blocks in bytes.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_small_data_block_size(long plist, long size)
            throws HDF5LibraryException;

    /**
     * H5Pget_libver_bounds retrieves the lower and upper bounds on the HDF5 Library versions that indirectly determine
     * the object formats versions used when creating objects in the file.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     *
     **/
    public synchronized static native int H5Pget_libver_bounds(long fapl_id, int[] libver) throws HDF5LibraryException,
    NullPointerException;

    /**
     * H5Pset_libver_bounds Sets bounds on library versions, and indirectly format versions, to be used when creating
     * objects
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
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Argument is Illegal
     *
     **/
    public synchronized static native int H5Pset_libver_bounds(long fapl_id, int low, int high)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_elink_file_cache_size retrieves the size of the external link open file cache.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return External link open file cache size in number of files.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pget_elink_file_cache_size(long fapl_id) throws HDF5LibraryException;

    /**
     * H5Pset_elink_file_cache_size sets the number of files that can be held open in an external link open file cache.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param efc_size
     *            IN: External link open file cache size in number of files.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native void H5Pset_elink_file_cache_size(long fapl_id, int efc_size)
            throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - location is null.
     *
     **/
    public synchronized static native void H5Pset_mdc_log_options(long fapl_id, boolean is_enabled, String location, boolean start_on_access)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Pget_mdc_log_options gets metadata cache logging options.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param mdc_log_options, the options
     *             mdc_logging_options[0] = is_enabled, whether logging is enabled
     *             mdc_logging_options[1] = start_on_access, whether the logging begins as soon as the file is opened or created
     *
     * @return the location of log in UTF-8/ASCII (file path/name) (On Windows, this must be ASCII).
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native String H5Pget_mdc_log_options(long fapl_id, boolean[] mdc_log_options)
            throws HDF5LibraryException;

    /**
     * H5Pget_metadata_read_attempts retrieves the number of read attempts that is set in the file access property list plist_id.
     *
     * @param plist_id
     *            IN: File access property list identifier
     *
     * @return The number of read attempts.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native long H5Pget_metadata_read_attempts(long plist_id) throws HDF5LibraryException;

    /**
     * H5Pset_metadata_read_attempts sets the number of reads that the library will try when reading checksummed
     * metadata in an HDF5 file opened with SWMR access. When reading such metadata, the library will compare the
     * checksum computed for the metadata just read with the checksum stored within the piece of checksum. When
     * performing SWMR operations on a file, the checksum check might fail when the library reads data on a system
     * that is not atomic. To remedy such situations, the library will repeatedly read the piece of metadata until
     * the check passes or finally fails the read when the allowed number of attempts is reached.
     *
     * @param plist_id
     *            IN: File access property list identifier
     * @param attempts
     *            IN: The number of read attempts which is a value greater than 0.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native void H5Pset_metadata_read_attempts(long plist_id, long attempts)
            throws HDF5LibraryException;

    // Dataset creation property list (DCPL) routines //

    /**
     * H5Pget_layout returns the layout of the raw data for a dataset.
     *
     * @param plist
     *            IN: Identifier for property list to query.
     *
     * @return the layout type of a dataset creation property list if successful. Otherwise returns H5D_LAYOUT_ERROR
     *         (-1).
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pget_layout(long plist) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_layout(long plist, int layout) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - dims array is null.
     * @exception IllegalArgumentException
     *                - max_ndims &lt;=0
     **/
    public synchronized static native int H5Pget_chunk(long plist, int max_ndims, long[] dims)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - dims array is null.
     * @exception IllegalArgumentException
     *                - dims &lt;=0
     **/
    public synchronized static native int H5Pset_chunk(long plist, int ndims, byte[] dim) throws HDF5LibraryException,
    NullPointerException, IllegalArgumentException;

    public synchronized static int H5Pset_chunk(long plist, int ndims, long[] dim) throws HDF5Exception,
    NullPointerException, IllegalArgumentException {
        if (dim == null) {
            return -1;
        }

        HDFArray theArray = new HDFArray(dim);
        byte[] thedims = theArray.byteify();

        int retVal = H5Pset_chunk(plist, ndims, thedims);

        thedims = null;
        theArray = null;
        return retVal;
    }

    /**
     * H5Pset_virtual maps elements of the virtual dataset (VDS) described by the
     * virtual dataspace identifier vspace_id to the elements of the source dataset
     * described by the source dataset dataspace identifier src_space_id. The source
     * dataset is identified by the name of the file where it is located, src_file_name,
     * and the name of the dataset, src_dset_name.
     *
     * @param dcpl_id
     *            IN: The identifier of the dataset creation property list that will be used when creating the virtual dataset.
     * @param vspace_id
     *            IN: The dataspace identifier with the selection within the virtual dataset applied, possibly an unlimited selection.
     * @param src_file_name
     *            IN: The name of the HDF5 file where the source dataset is located. The file might not exist yet. The name can be specified using a C-style printf statement.
     * @param src_dset_name
     *            IN: The path to the HDF5 dataset in the file specified by src_file_name. The dataset might not exist yet. The dataset name can be specified using a C-style printf statement.
     * @param src_space_id
     *            IN: The source dataset dataspace identifier with a selection applied, possibly an unlimited selection.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an name string is null.
     * @exception IllegalArgumentException
     *                - An id is &lt;=0
     **/
    public synchronized static native void H5Pset_virtual(long dcpl_id, long vspace_id, String src_file_name, String src_dset_name, long src_space_id) throws HDF5LibraryException,
    NullPointerException, IllegalArgumentException;

    /**
     * H5Pget_virtual_count gets the number of mappings for a virtual dataset that has the creation property list specified by dcpl_id.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     *
     * @return a non-negative number of mappings if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - An id is &lt;=0
     **/
    public synchronized static native long H5Pget_virtual_count(long dcpl_id) throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_virtual_vspace takes the dataset creation property list for the virtual dataset, dcpl_id, and the mapping index, index,
     *     and returns a dataspace identifier for the selection within the virtual dataset used in the mapping.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     * @param index
     *            IN: Mapping index.
     *
     * @return a valid dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - An id is &lt;=0
     **/
    public synchronized static native long H5Pget_virtual_vspace(long dcpl_id, long index) throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_virtual_srcspace takes the dataset creation property list for the virtual dataset, dcpl_id, and the mapping index, index,
     *    and returns a dataspace identifier for the selection within the source dataset used in the mapping.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     * @param index
     *            IN: Mapping index.
     *
     * @return a valid dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - An id is &lt;=0
     **/
    public synchronized static native long H5Pget_virtual_srcspace(long dcpl_id, long index) throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_virtual_filename takes the dataset creation property list for the virtual dataset, dcpl_id, the mapping index, index,
     * the size of the filename for a source dataset, size, and retrieves the name of the file for a source dataset used in the mapping.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     * @param index
     *            IN: Mapping index.
     *
     * @return the name of the file containing the source dataset if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - An id is &lt;=0
     **/
    public synchronized static native String H5Pget_virtual_filename(long dcpl_id, long index) throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_virtual_dsetname takes the dataset creation property list for the virtual dataset, dcpl_id, the mapping index, index, the
     * size of the dataset name for a source dataset, size, and retrieves the name of the source dataset used in the mapping.
     *
     * @param dcpl_id
     *            IN: The identifier of the virtual dataset creation property list.
     * @param index
     *            IN: Mapping index.
     *
     * @return the name of the source dataset if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - An id is &lt;=0
     **/
    public synchronized static native String H5Pget_virtual_dsetname(long dcpl_id, long index) throws HDF5LibraryException, IllegalArgumentException;

    /**
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
     *            <pre>
     *      size[0] = offset // a location to return an offset value
     *      size[1] = size // a location to return the size of
     *                // the external file data.
     * </pre>
     *
     * @return a non-negative value if successful
     *
     * @exception ArrayIndexOutOfBoundsException
     *                Fatal error on Copyback
     * @exception ArrayStoreException
     *                Fatal error on Copyback
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name or size is null.
     * @exception IllegalArgumentException
     *                - name_size &lt;= 0 .
     *
     **/
    public synchronized static native int H5Pget_external(long plist, int idx, long name_size, String[] name,
            long[] size) throws ArrayIndexOutOfBoundsException, ArrayStoreException, HDF5LibraryException,
            NullPointerException, IllegalArgumentException;

    /**
     * H5Pset_external adds an external file to the list of external files.
     *
     * @param plist
     *            IN: Identifier of a dataset creation property list.
     * @param name
     *            IN: Name of an external file.
     * @param offset
     *            IN: Offset, in bytes, from the beginning of the file to the location in the file where the data
     *            starts.
     * @param size
     *            IN: Number of bytes reserved in the file for the data.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Pset_external(long plist, String name, long offset, long size)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Pget_external_count returns the number of external files for the specified dataset.
     *
     * @param plist
     *            IN: Identifier of a dataset creation property list.
     *
     * @return the number of external files if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pget_external_count(long plist) throws HDF5LibraryException;

    public synchronized static native int H5Pset_szip(long plist, int options_mask, int pixels_per_block)
            throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pset_shuffle(long plist_id) throws HDF5LibraryException,
    NullPointerException;

    /**
     * H5Pset_nbit Sets up the use of the N-Bit filter.
     *
     * @param plist_id
     *            IN: Dataset creation property list identifier.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pset_nbit(long plist_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid arguments
     *
     **/
    public synchronized static native int H5Pset_scaleoffset(long plist_id, int scale_type, int scale_factor)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
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
     *                - Error converting data array.
     **/
    public synchronized static native int H5Pget_fill_value(long plist_id, long type_id, byte[] value)
            throws HDF5Exception;

    /**
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
     *                - Error converting data array.
     **/
    public synchronized static int H5Pget_fill_value(long plist_id, long type_id, Object obj) throws HDF5Exception {
        HDFArray theArray = new HDFArray(obj);
        byte[] buf = theArray.emptyBytes();

        int status = H5Pget_fill_value(plist_id, type_id, buf);
        if (status >= 0) {
            obj = theArray.arrayify(buf);
        }

        return status;
    }

    /**
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
     * @exception HDF5Exception
     *                - Error converting data array
     **/
    public synchronized static native int H5Pset_fill_value(long plist_id, long type_id, byte[] value)
            throws HDF5Exception;

    /**
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
     *                - Error converting data array
     **/
    public synchronized static int H5Pset_fill_value(long plist_id, long type_id, Object obj) throws HDF5Exception {
        HDFArray theArray = new HDFArray(obj);
        byte[] buf = theArray.byteify();

        int retVal = H5Pset_fill_value(plist_id, type_id, buf);

        buf = null;
        theArray = null;
        return retVal;
    }

    public synchronized static native int H5Pfill_value_defined(long plist_id, int[] status)
            throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pget_alloc_time(long plist_id, int[] alloc_time)
            throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pset_alloc_time(long plist_id, int alloc_time) throws HDF5LibraryException,
    NullPointerException;

    public synchronized static native int H5Pget_fill_time(long plist_id, int[] fill_time) throws HDF5LibraryException,
    NullPointerException;

    public synchronized static native int H5Pset_fill_time(long plist_id, int fill_time) throws HDF5LibraryException,
    NullPointerException;

    // Dataset access property list (DAPL) routines //

    /**
     * Retrieves the maximum possible number of elements in the meta data cache and the maximum possible number of bytes
     * and the RDCC_W0 value in the raw data chunk cache on a per-datset basis.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an array is null.
     **/
    public synchronized static native void H5Pget_chunk_cache(long dapl_id, long[] rdcc_nslots, long[] rdcc_nbytes,
            double[] rdcc_w0) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Pset_chunk_cache sets the number of elements (objects) in the meta data cache and the total number of bytes in
     * the raw data chunk cache on a per-datset basis.
     *
     * @param dapl_id
     *            IN: Identifier of the datset access property list.
     * @param rdcc_nslots
     *            IN: Number of elements (objects) in the raw data chunk cache.
     * @param rdcc_nbytes
     *            IN: Total size of the raw data chunk cache, in bytes.
     * @param rdcc_w0
     *            IN: Preemption policy.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Pset_chunk_cache(long dapl_id, long rdcc_nslots, long rdcc_nbytes,
            double rdcc_w0) throws HDF5LibraryException;

    /**
     * H5Pset_virtual_view takes the access property list for the virtual dataset, dapl_id, and the flag,
     * view, and sets the VDS view according to the flag value.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier for the virtual dataset
     * @param view
     *            IN: Flag specifying the extent of the data to be included in the view.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library
     **/
    public synchronized static native void H5Pset_virtual_view(long dapl_id, int view) throws HDF5LibraryException;

    /**
     * H5Pget_virtual_view takes the virtual dataset access property list, dapl_id, and retrieves the flag,
     * view, set by the H5Pset_virtual_view call.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier for the virtual dataset

     * @return The flag specifying the view of the virtual dataset.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library
     **/
    public synchronized static native int H5Pget_virtual_view(long dapl_id) throws HDF5LibraryException;

    /**
     * H5Pset_virtual_printf_gap sets the access property list for the virtual dataset, dapl_id, to instruct the
     * library to stop looking for the mapped data stored in the files and/or datasets with the printf-style names
     * after not finding gap_size files and/or datasets. The found source files and datasets will determine the
     * extent of the unlimited virtual dataset with the printf-style mappings.
     *
     * @param dapl_id
     *            IN: Dataset access property list identifier for the virtual dataset
     * @param gap_size
     *            IN: Maximum number of files and/or datasets allowed to be missing for determining
     *            the extent of an unlimited virtual dataset with printf-style mappings.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library
     **/
    public synchronized static native void H5Pset_virtual_printf_gap(long dapl_id, long gap_size) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library
     **/
    public synchronized static native long H5Pget_virtual_printf_gap(long dapl_id) throws HDF5LibraryException;

    // public synchronized static native void H5Pset_append_flush(long plist_id, int ndims, long[] boundary, H5D_append_cb func, H5D_append_t udata) throws HDF5LibraryException;

    // public synchronized static native void H5Pget_append_flush(long plist_id, int dims, long[] boundary, H5D_append_cb func, H5D_append_t udata) throws HDF5LibraryException;


    // Dataset xfer property list (DXPL) routines //

    /**
     * H5Pget_data_transform retrieves the data transform expression previously set in the dataset transfer property
     * list plist_id by H5Pset_data_transform.
     *
     * @param plist_id
     *            IN: Identifier of the property list or class
     * @param size
     *            IN: Number of bytes of the transform expression to copy to
     * @param expression
     *            OUT: A data transform expression
     *
     * @return The size of the transform expression if successful; 0(zero) if no transform expression exists. Otherwise
     *         returns a negative value.
     *
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Size is &lt;= 0.
     *
     **/
    public synchronized static native long H5Pget_data_transform(long plist_id, String[] expression, long size)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pset_data_transform sets a data transform expression
     *
     * @param plist_id
     *            IN: Identifier of the property list or class
     * @param expression
     *            IN: Pointer to the null-terminated data transform expression
     *
     * @return a non-negative valule if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - expression is null.
     *
     **/
    public synchronized static native int H5Pset_data_transform(long plist_id, String expression)
            throws HDF5LibraryException, NullPointerException;

    /**
     * HH5Pget_buffer gets type conversion and background buffers. Returns buffer size, in bytes, if successful;
     * otherwise 0 on failure.
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
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - plist is invalid.
     **/
    public synchronized static native int H5Pget_buffer(long plist, byte[] tconv, byte[] bkg)
            throws HDF5LibraryException, IllegalArgumentException;

    public synchronized static native long H5Pget_buffer_size(long plist)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pset_buffer sets type conversion and background buffers. status to TRUE or FALSE.
     *
     * Given a dataset transfer property list, H5Pset_buffer sets the maximum size for the type conversion buffer and
     * background buffer and optionally supplies pointers to application-allocated buffers. If the buffer size is
     * smaller than the entire amount of data being transferred between the application and the file, and a type
     * conversion buffer or background buffer is required, then strip mining will be used.
     *
     * Note that there are minimum size requirements for the buffer. Strip mining can only break the data up along the
     * first dimension, so the buffer must be large enough to accommodate a complete slice that encompasses all of the
     * remaining dimensions. For example, when strip mining a 100x200x300 hyperslab of a simple data space, the buffer
     * must be large enough to hold 1x200x300 data elements. When strip mining a 100x200x300x150 hyperslab of a simple
     * data space, the buffer must be large enough to hold 1x200x300x150 data elements.
     *
     * @param plist
     *            Identifier for the dataset transfer property list.
     * @param size
     *            Size, in bytes, of the type conversion and background buffers.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - plist is invalid.
     **/
    public synchronized static native void H5Pset_buffer_size(long plist, long size) throws HDF5LibraryException,
    IllegalArgumentException;

    public synchronized static native int H5Pget_edc_check(long plist) throws HDF5LibraryException,
    NullPointerException;

    public synchronized static native int H5Pset_edc_check(long plist, int check) throws HDF5LibraryException,
    NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an input array is null.
     **/
    public synchronized static native int H5Pget_btree_ratios(long plist_id, double[] left, double[] middle,
            double[] right) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Pset_btree_ratio Sets B-tree split ratios for a dataset transfer property list. The split ratios determine what
     * percent of children go in the first node when a node splits.
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Pset_btree_ratios(long plist_id, double left, double middle, double right)
            throws HDF5LibraryException;

    public synchronized static native int H5Pget_hyper_vector_size(long dxpl_id, long[] vector_size)
            throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pset_hyper_vector_size(long dxpl_id, long vector_size)
            throws HDF5LibraryException, NullPointerException;

    // Link creation property list (LCPL) routines //

    /**
     * H5Pget_create_intermediate_group determines whether property is set to enable creating missing intermediate
     * groups.
     *
     * @param lcpl_id
     *            IN: Link creation property list identifier
     *
     * @return Boolean true or false
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native boolean H5Pget_create_intermediate_group(long lcpl_id)
            throws HDF5LibraryException;

    /**
     * H5Pset_create_intermediate_group specifies in property list whether to create missing intermediate groups
     *
     * @param lcpl_id
     *            IN: Link creation property list identifier
     * @param crt_intermed_group
     *            IN: Flag specifying whether to create intermediate groups upon the creation of an object
     *
     * @return a non-negative valule if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pset_create_intermediate_group(long lcpl_id, boolean crt_intermed_group)
            throws HDF5LibraryException;

    // Group creation property list (GCPL) routines //

    /**
     * H5Pget_local_heap_size_hint Retrieves the anticipated size of the local heap for original-style groups.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     *
     * @return size_hint, the anticipated size of local heap
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native long H5Pget_local_heap_size_hint(long gcpl_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pset_local_heap_size_hint(long gcpl_id, long size_hint)
            throws HDF5LibraryException;

    /**
     * H5Pget_link_phase_change Queries the settings for conversion between compact and dense groups.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     * @param links
     *            The max. no. of compact links &amp; the min. no. of dense links, which are used for storing groups
     *
     *            <pre>
     *      links[0] =  The maximum number of links for compact storage
     *      links[1] =  The minimum number of links for dense storage
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     *
     **/
    public synchronized static native int H5Pget_link_phase_change(long gcpl_id, int[] links)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid values of max_compact and min_dense.
     *
     **/
    public synchronized static native int H5Pset_link_phase_change(long gcpl_id, int max_compact, int min_dense)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_est_link_info Queries data required to estimate required local heap or object header size.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     * @param link_info
     *            Estimated number of links to be inserted into group And the estimated average length of link names
     *
     *            <pre>
     *      link_info[0] =  Estimated number of links to be inserted into group
     *      link_info[1] =  Estimated average length of link names
     * </pre>
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - link_info is null.
     *
     **/
    public synchronized static native int H5Pget_est_link_info(long gcpl_id, int[] link_info)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid values to est_num_entries and est_name_len.
     *
     **/
    public synchronized static native int H5Pset_est_link_info(long gcpl_id, int est_num_entries, int est_name_len)
            throws HDF5LibraryException, IllegalArgumentException;

    /**
     * H5Pget_link_creation_order queries the group creation property list, gcpl_id, and returns a flag indicating
     * whether link creation order is tracked and/or indexed in a group.
     *
     * @param gcpl_id
     *            IN: Group creation property list identifier
     *
     * @return crt_order_flags -Creation order flag(s)
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pget_link_creation_order(long gcpl_id) throws HDF5LibraryException;

    /**
     * H5Pset_link_creation_order Sets flags in a group creation property list, gcpl_id, for tracking and/or indexing
     * links on creation order.
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
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pset_link_creation_order(long gcpl_id, int crt_order_flags)
            throws HDF5LibraryException;

    // String creation property list (STRCPL) routines //

    public synchronized static native int H5Pget_char_encoding(long plist_id) throws HDF5LibraryException;

    public synchronized static native void H5Pset_char_encoding(long plist_id, int encoding)
            throws HDF5LibraryException;

    // Link access property list (LAPL) routines //

    /**
     * H5Pget_nlinks retrieves the maximum number of soft or user-defined link traversals allowed, nlinks, before the
     * library assumes it has found a cycle and aborts the traversal. This value is retrieved from the link access
     * property list lapl_id.
     *
     * @param lapl_id
     *            IN: File access property list identifier
     *
     * @return Returns a Maximum number of links to traverse.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native long H5Pget_nlinks(long lapl_id) throws HDF5LibraryException;

    /**
     * H5Pset_nlinks sets the maximum number of soft or user-defined link traversals allowed, nlinks, before the library
     * assumes it has found a cycle and aborts the traversal. This value is set in the link access property list
     * lapl_id.
     *
     * @param lapl_id
     *            IN: File access property list identifier
     * @param nlinks
     *            IN: Maximum number of links to traverse
     *
     * @return Returns a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Argument is Illegal
     *
     **/
    public synchronized static native int H5Pset_nlinks(long lapl_id, long nlinks) throws HDF5LibraryException,
    IllegalArgumentException;

    /**
     * H5Pget_elink_prefix Retrieves prefix applied to external link paths.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     * @param prefix
     *            OUT: Prefix applied to external link paths
     *
     * @return If successful, returns a non-negative value specifying the size in bytes of the prefix without the NULL
     *         terminator; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - prefix is null.
     *
     **/
    public synchronized static native long H5Pget_elink_prefix(long lapl_id, String[] prefix)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - prefix is null.
     *
     **/
    public synchronized static native int H5Pset_elink_prefix(long lapl_id, String prefix) throws HDF5LibraryException,
    NullPointerException;

    /**
     * H5Pget_elink_fapl Retrieves the file access property list identifier associated with the link access property
     * list.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public static long H5Pget_elink_fapl(long lapl_id) throws HDF5LibraryException {
        long id = _H5Pget_elink_fapl(lapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Pget_elink_fapl add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Pget_elink_fapl(long lapl_id) throws HDF5LibraryException;

    /**
     * H5Pset_elink_fapl sets a file access property list for use in accessing a file pointed to by an external link.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     * @param fapl_id
     *            IN: File access property list identifier
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pset_elink_fapl(long lapl_id, long fapl_id) throws HDF5LibraryException;

    /**
     * H5Pget_elink_acc_flags retrieves the external link traversal file access flag from the specified link access
     * property list.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     *
     * @return File access flag for link traversal.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pget_elink_acc_flags(long lapl_id) throws HDF5LibraryException;

    /**
     * H5Pset_elink_acc_flags Sets the external link traversal file access flag in a link access property list.
     *
     * @param lapl_id
     *            IN: Link access property list identifier
     * @param flags
     *            IN: The access flag for external link traversal.
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception IllegalArgumentException
     *                - Invalid Flag values.
     *
     **/
    public synchronized static native int H5Pset_elink_acc_flags(long lapl_id, int flags) throws HDF5LibraryException,
    IllegalArgumentException;

    // Object copy property list (OCPYPL) routines //

    /**
     * H5Pget_copy_object retrieves the properties to be used when an object is copied.
     *
     * @param ocp_plist_id
     *            IN: Object copy property list identifier
     *
     * @return Copy option(s) set in the object copy property list
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pget_copy_object(long ocp_plist_id) throws HDF5LibraryException;

    /**
     * H5Pset_copy_object Sets properties to be used when an object is copied.
     *
     * @param ocp_plist_id
     *            IN: Object copy property list identifier
     * @param copy_options
     *            IN: Copy option(s) to be set
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native void H5Pset_copy_object(long ocp_plist_id, int copy_options)
            throws HDF5LibraryException;

    // Other/Older property list routines //

    /**
     * H5Pget_version retrieves the version information of various objects for a file creation property list.
     *
     * @param plist
     *            IN: Identifier of the file creation property list.
     * @param version_info
     *            OUT: version information.
     *
     *            <pre>
     *      version_info[0] = boot  // boot block version number
     *      version_info[1] = freelist  // global freelist version
     *      version_info[2] = stab  // symbol tabl version number
     *      version_info[3] = shhdr  // shared object header version
     * </pre>
     * @return a non-negative value, with the values of version_info initialized, if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - version_info is null.
     * @exception IllegalArgumentException
     *                - version_info is illegal.
     **/
    public synchronized static native int H5Pget_version(long plist, int[] version_info) throws HDF5LibraryException,
    NullPointerException, IllegalArgumentException;

    // file drivers property list routines //

    public synchronized static native void H5Pget_fapl_core(long fapl_id, long[] increment, boolean[] backing_store)
            throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pset_fapl_core(long fapl_id, long increment, boolean backing_store)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Pget_fapl_direct Retrieve direct I/O settings.
     *
     * @param fapl_id
     *            IN: File access property list identifier
     * @param info
     *            OUT: Returned property list information info[0] = alignment Required memory alignment boundary info[1]
     *            = block_size File system block size info[2] = cbuf_size Copy buffer size
     *
     * @return a non-negative value if successful; otherwise returns a negative value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pget_fapl_direct(long fapl_id, long[] info) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     *
     **/
    public synchronized static native int H5Pset_fapl_direct(long fapl_id, long alignment, long block_size,
            long cbuf_size) throws HDF5LibraryException;

    public synchronized static native int H5Pget_fapl_family(long fapl_id, long[] memb_size, long[] memb_fapl_id)
            throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pset_fapl_family(long fapl_id, long memb_size, long memb_fapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *            IN: The offsets within the virtual address space, from 0 (zero) to HADDR_MAX, at which each type of
     *            data storage begins.
     *
     * @return a boolean value; Allows read-only access to incomplete file sets when TRUE.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an array is null.
     *
     **/
    public synchronized static native boolean H5Pget_fapl_multi(long fapl_id, int[] memb_map, long[] memb_fapl,
            String[] memb_name, long[] memb_addr) throws HDF5LibraryException, NullPointerException;

    /**
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
     *            IN: The offsets within the virtual address space, from 0 (zero) to HADDR_MAX, at which each type of
     *            data storage begins.
     * @param relax
     *            IN: Allows read-only access to incomplete file sets when TRUE.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an array is null.
     *
     **/
    public synchronized static native void H5Pset_fapl_multi(long fapl_id, int[] memb_map, long[] memb_fapl,
            String[] memb_name, long[] memb_addr, boolean relax) throws HDF5LibraryException, NullPointerException;


    /**
     * H5Pset_fapl_log Sets up the logging virtual file driver (H5FD_LOG) for use. H5Pset_fapl_log modifies the file
     * access property list to use the logging driver, H5FD_LOG. The logging virtual file driver (VFD) is a clone of the
     * standard SEC2 (H5FD_SEC2) driver with additional facilities for logging VFD metrics and activity to a file.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - logfile is null.
     **/
    public synchronized static native void H5Pset_fapl_log(long fapl_id, String logfile, long flags, long buf_size)
            throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pset_fapl_sec2(long fapl_id) throws HDF5LibraryException, NullPointerException;

    public synchronized static native void H5Pset_fapl_split(long fapl_id, String meta_ext, long meta_plist_id,
            String raw_ext, long raw_plist_id) throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pset_fapl_stdio(long fapl_id) throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Pset_fapl_windows(long fapl_id) throws HDF5LibraryException, NullPointerException;

    // /////// unimplemented ////////

    // Generic property list routines //
    // herr_t H5Pencode(hid_t plist_id, void *buf, size_t *nalloc);
    // hid_t  H5Pdecode(const void *buf);

    // Object creation property list (OCPL) routines //

    // File creation property list (FCPL) routines //

    // File access property list (FAPL) routines //
    // herr_t H5Pset_driver( hid_t plist_id, hid_t new_driver_id, const void *new_driver_info )
    // const void *H5Pget_driver_info( hid_t plist_id )
    // herr_t H5Pget_multi_type ( hid_t fapl_id, H5FD_mem_t *type )
    // herr_t H5Pset_multi_type ( hid_t fapl_id, H5FD_mem_t type )
    // herr_t H5Pget_file_image(hid_t fapl_id, void **buf_ptr_ptr, size_t *buf_len_ptr);
    // herr_t H5Pset_file_image(hid_t fapl_id, void *buf_ptr, size_t buf_len);
    // herr_t H5Pget_file_image_callbacks(hid_t fapl_id, H5FD_file_image_callbacks_t *callbacks_ptr);
    // herr_t H5Pset_file_image_callbacks(hid_t fapl_id, H5FD_file_image_callbacks_t *callbacks_ptr);
    // herr_t H5Pset_core_write_tracking(hid_t fapl_id, hbool_t is_enabled, size_t page_size);
    // herr_t H5Pget_core_write_tracking(hid_t fapl_id, hbool_t *is_enabled, size_t *page_size);

    // Dataset creation property list (DCPL) routines //

    // Dataset access property list (DAPL) routines //

    // Dataset xfer property list (DXPL) routines //
    // herr_t H5Pset_buffer(hid_t plist_id, size_t size, void *tconv, void *bkg);
    // herr_t H5Pset_preserve(hid_t plist_id, hbool_t status);
    // int H5Pget_preserve(hid_t plist_id);
    // herr_t H5Pset_filter_callback(hid_t plist, H5Z_filter_func_t func, void *op_data)
    // herr_t H5Pget_vlen_mem_manager(hid_t plist, H5MM_allocate_t *alloc, void **alloc_info, H5MM_free_t *free, void
    // **free_info )
    // herr_t H5Pset_vlen_mem_manager(hid_t plist, H5MM_allocate_t alloc, void *alloc_info, H5MM_free_t free, void
    // *free_info )
    // herr_t H5Pget_type_conv_cb(hid_t plist, H5T_conv_except_func_t *func, void **op_data)
    // herr_t H5Pset_type_conv_cb( hid_t plist, H5T_conv_except_func_t func, void *op_data)

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


    // Other/Older property list routines //
    // herr_t H5Pget_fapl_mpio( int fapl_id, MPI_Comm *comm, MPI_Info *info )
    // herr_t H5Pset_fapl_mpio( int fapl_id, MPI_Comm comm, MPI_Info info )

    // herr_t H5Pget_fapl_mpiposix( int fapl_id, MPI_Comm *comm, hbool_t *use_gpfs_hints )
    // herr_t H5Pset_fapl_mpiposix( int fapl_id, MPI_Comm comm, hbool_t use_gpfs_hints )

    // herr_t H5Pget_dxpl_mpio( hid_t dxpl_id, H5FD_mpio_xfer_t *xfer_mode )
    // herr_t H5Pset_dxpl_mpio( hid_t dxpl_id, H5FD_mpio_xfer_t xfer_mode )
    // herr_t H5Pset_dxpl_mpio_chunk_opt (hid_t dxpl_id, H5FD_mpio_chunk_opt_t opt_mode)
    // herr_t H5Pset_dxpl_mpio_chunk_opt_num (hid_t dxpl_id, unsigned num_chunk_per_proc)
    // herr_t H5Pset_dxpl_mpio_chunk_opt_ratio (hid_t dxpl_id, unsigned percent_proc_per_chunk)
    // herr_t H5Pset_dxpl_mpio_collective_opt (hid_t dxpl_id, H5FD_mpio_collective_opt_t opt_mode)

    // ////////////////////////////////////////////////////////////
    // //
    // H5PL: HDF5 1.8 Plugin API Functions //
    // //
    // ////////////////////////////////////////////////////////////
    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5PLset_loading_state(int plugin_flags) throws HDF5LibraryException;

    /**
     * H5PLget_loading_state retrieves the state of the dynamic plugins flag, plugin_flags..
     *
     * @return the list of dynamic plugin types that are enabled or disabled.
     *             A plugin bit set to 0 (zero) indicates that that dynamic plugin is disabled.
     *             A plugin bit set to 1 (one) indicates that that dynamic plugin is enabled.
     *             If the value of plugin_flags is negative, all dynamic plugins are enabled.
     *             If the value of plugin_flags is 0 (zero), all dynamic plugins are disabled.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5PLget_loading_state() throws HDF5LibraryException;

    // ////////////////////////////////////////////////////////////
    // //
    // H5R: HDF5 1.8 Reference API Functions //
    // //
    // ////////////////////////////////////////////////////////////

    private synchronized static native int H5Rcreate(byte[] ref, long loc_id, String name, int ref_type, long space_id)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
     * H5Rcreate creates the reference, ref, of the type specified in ref_type, pointing to the object name located at
     * loc_id.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an input array is null.
     * @exception IllegalArgumentException
     *                - an input array is invalid.
     **/
    public synchronized static byte[] H5Rcreate(long loc_id, String name, int ref_type, long space_id)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException {
        /* These sizes are correct for HDF5.1.2 */
        int ref_size = 8;
        if (ref_type == HDF5Constants.H5R_DATASET_REGION) {
            ref_size = 12;
        }
        byte rbuf[] = new byte[ref_size];

        /* will raise an exception if fails */
        H5Rcreate(rbuf, loc_id, name, ref_type, space_id);

        return rbuf;
    }

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - output array is null.
     * @exception IllegalArgumentException
     *                - output array is invalid.
     **/
    public static long H5Rdereference(long dataset, long access_list, int ref_type, byte[] ref)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException {
        long id = _H5Rdereference(dataset, access_list, ref_type, ref);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Rdereference add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Rdereference(long dataset, long access_list, int ref_type, byte[] ref)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
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
     * @return Returns the length of the name if successful, returning 0 (zero) if no name is associated with the
     *         identifier. Otherwise returns a negative value.
     *
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - size is null.
     * @exception IllegalArgumentException
     *                - Argument is illegal.
     *
     **/
    public synchronized static native long H5Rget_name(long loc_id, int ref_type, byte[] ref, String[] name, long size)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
     * H5Rget_obj_type Given a reference to an object ref, H5Rget_obj_type returns the type of the object pointed to.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - array is null.
     * @exception IllegalArgumentException
     *                - array is invalid.
     **/
    public synchronized static native int H5Rget_obj_type(long loc_id, int ref_type, byte ref[])
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
     * H5Rget_obj_type2 Retrieves the type of object that an object reference points to.
     *
     * @see public static int H5Rget_obj_type(int loc_id, int ref_type, byte ref[])
     **/
    private synchronized static native int H5Rget_obj_type2(long loc_id, int ref_type, byte ref[], int[] obj_type)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
     * Given a reference to an object ref, H5Rget_region creates a copy of the dataspace of the dataset pointed to and
     * defines a selection in the copy which is the region pointed to.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - output array is null.
     * @exception IllegalArgumentException
     *                - output array is invalid.
     **/
    public static long H5Rget_region(long loc_id, int ref_type, byte[] ref) throws HDF5LibraryException,
    NullPointerException, IllegalArgumentException {
        long id = _H5Rget_region(loc_id, ref_type, ref);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Rget_region add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Rget_region(long loc_id, int ref_type, byte[] ref)
            throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    // ////////////////////////////////////////////////////////////
    // //
    // H5S: Dataspace Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
     * H5Sclose releases a dataspace.
     *
     * @param space_id
     *            Identifier of dataspace to release.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Sclose(long space_id) throws HDF5LibraryException {
        if (space_id < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Sclose remove {}", space_id);
        OPEN_IDS.remove(space_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Sclose(space_id);
    }

    private synchronized static native int _H5Sclose(long space_id) throws HDF5LibraryException;

    /**
     * H5Scopy creates a new dataspace which is an exact copy of the dataspace identified by space_id.
     *
     * @param space_id
     *            Identifier of dataspace to copy.
     * @return a dataspace identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Scopy(long space_id) throws HDF5LibraryException {
        long id = _H5Scopy(space_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Scopy add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Scopy(long space_id) throws HDF5LibraryException;

    /**
     * H5Screate creates a new dataspace of a particular type.
     *
     * @param type
     *            IN: The type of dataspace to be created.
     *
     * @return a dataspace identifier
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Screate(int type) throws HDF5LibraryException {
        long id = _H5Screate(type);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Screate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Screate(int type) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - dims or maxdims is null.
     **/
    public static long H5Screate_simple(int rank, long[] dims, long[] maxdims) throws HDF5Exception,
            NullPointerException {
        long id = _H5Screate_simple(rank, dims, maxdims);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Screate_simple add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Screate_simple(int rank, long[] dims, long[] maxdims)
            throws HDF5Exception, NullPointerException;

    /**
     * H5Sdecode reconstructs the HDF5 data space object and returns a new object handle for it.
     *
     * @param buf
     *            IN: Buffer for the data space object to be decoded.
     *
     * @return a new object handle
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native long H5Sdecode(byte[] buf) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Sencode converts a data space description into binary form in a buffer.
     *
     * @param obj_id
     *            IN: Identifier of the object to be encoded.
     *
     * @return the buffer for the object to be encoded into.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native byte[] H5Sencode(long obj_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Sextent_copy copies the extent from source_space_id to dest_space_id. This action may change the type of the
     * dataspace.
     *
     * @param dest_space_id
     *            IN: The identifier for the dataspace from which the extent is copied.
     * @param source_space_id
     *            IN: The identifier for the dataspace to which the extent is copied.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Sextent_copy(long dest_space_id, long source_space_id)
            throws HDF5LibraryException;

    /**
     * H5Sextent_equal determines whether the dataspace extents of two dataspaces, space1_id and space2_id, are equal.
     *
     * @param first_space_id
     *            IN: The identifier for the first dataspace.
     * @param second_space_id
     *            IN: The identifier for the seconddataspace.
     *
     * @return true if successful, else false
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Sextent_equal(long first_space_id, long second_space_id)
            throws HDF5LibraryException;

    /**
     * H5Sget_select_bounds retrieves the coordinates of the bounding box containing the current selection and places
     * them into user-supplied buffers.
     * <P>
     * The start and end buffers must be large enough to hold the dataspace rank number of coordinates.
     *
     * @param spaceid
     *            Identifier of dataspace to release.
     * @param start
     *            coordinates of lowest corner of bounding box.
     * @param end
     *            coordinates of highest corner of bounding box.
     *
     * @return a non-negative value if successful,with start and end initialized.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - start or end is null.
     **/
    public synchronized static native int H5Sget_select_bounds(long spaceid, long[] start, long[] end)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Sget_select_elem_npoints returns the number of element points in the current dataspace selection.
     *
     * @param spaceid
     *            Identifier of dataspace to release.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Sget_select_elem_npoints(long spaceid) throws HDF5LibraryException;

    /**
     * H5Sget_select_elem_pointlist returns an array of of element points in the current dataspace selection. The point
     * coordinates have the same dimensionality (rank) as the dataspace they are located within, one coordinate per
     * point.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native int H5Sget_select_elem_pointlist(long spaceid, long startpoint, long numpoints,
            long[] buf) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Sget_select_hyper_blocklist returns an array of hyperslab blocks. The block coordinates have the same
     * dimensionality (rank) as the dataspace they are located within. The list of blocks is formatted as follows:
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
     *            returns blocks startblock to startblock+num-1, each block is <i>rank</i> * 2 (corners) longs.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native int H5Sget_select_hyper_blocklist(long spaceid, long startblock, long numblocks,
            long[] buf) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Sget_select_hyper_nblocks returns the number of hyperslab blocks in the current dataspace selection.
     *
     * @param spaceid
     *            Identifier of dataspace to release.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Sget_select_hyper_nblocks(long spaceid) throws HDF5LibraryException;

    /**
     * H5Sget_select_npoints determines the number of elements in the current selection of a dataspace.
     *
     * @param space_id
     *            IN: Identifier of the dataspace object to query
     *
     * @return the number of elements in the selection if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Sget_select_npoints(long space_id) throws HDF5LibraryException;

    /**
     * H5Sget_select_type retrieves the type of selection currently defined for the dataspace space_id.
     *
     * @param space_id
     *            IN: Identifier of the dataspace object to query
     *
     * @return the dataspace selection type if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Sget_select_type(long space_id) throws HDF5LibraryException;

    /**
     * H5Sget_simple_extent_dims returns the size and maximum sizes of each dimension of a dataspace through the dims
     * and maxdims parameters.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - dims or maxdims is null.
     **/
    public synchronized static native int H5Sget_simple_extent_dims(long space_id, long[] dims, long[] maxdims)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Sget_simple_extent_ndims determines the dimensionality (or rank) of a dataspace.
     *
     * @param space_id
     *            IN: Identifier of the dataspace
     *
     * @return the number of dimensions in the dataspace if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Sget_simple_extent_ndims(long space_id) throws HDF5LibraryException;

    /**
     * H5Sget_simple_extent_npoints determines the number of elements in a dataspace.
     *
     * @param space_id
     *            ID of the dataspace object to query
     * @return the number of elements in the dataspace if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Sget_simple_extent_npoints(long space_id) throws HDF5LibraryException;

    /**
     * H5Sget_simple_extent_type queries a dataspace to determine the current class of a dataspace.
     *
     * @param space_id
     *            Dataspace identifier.
     *
     * @return a dataspace class name if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Sget_simple_extent_type(long space_id) throws HDF5LibraryException;

    /**
     * H5Sis_simple determines whether a dataspace is a simple dataspace.
     *
     * @param space_id
     *            Identifier of the dataspace to query
     *
     * @return true if is a simple dataspace
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Sis_simple(long space_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - offset array is null.
     **/
    public synchronized static native int H5Soffset_simple(long space_id, byte[] offset) throws HDF5LibraryException,
            NullPointerException;

    public synchronized static int H5Soffset_simple(long space_id, long[] offset) throws HDF5Exception,
            NullPointerException {
        if (offset == null) {
            return -1;
        }

        HDFArray theArray = new HDFArray(offset);
        byte[] theArr = theArray.byteify();

        int retVal = H5Soffset_simple(space_id, theArr);

        theArr = null;
        theArray = null;
        return retVal;
    }

    /**
     * H5Sselect_all selects the entire extent of the dataspace space_id.
     *
     * @param space_id
     *            IN: The identifier of the dataspace to be selected.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Sselect_all(long space_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    private synchronized static native int H5Sselect_elements(long space_id, int op, int num_elements, byte[] coord)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error in the data conversion
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - cord array is
     **/
    public synchronized static int H5Sselect_elements(long space_id, int op, int num_elements, long[][] coord2D)
            throws HDF5Exception, HDF5LibraryException, NullPointerException {
        if (coord2D == null) {
            return -1;
        }

        HDFArray theArray = new HDFArray(coord2D);
        byte[] coord = theArray.byteify();

        int retVal = H5Sselect_elements(space_id, op, num_elements, coord);

        coord = null;
        theArray = null;
        return retVal;
    }

    /**
     * H5Sselect_hyperslab selects a hyperslab region to add to the current selected region for the dataspace specified
     * by space_id. The start, stride, count, and block arrays must be the same size as the rank of the dataspace.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an input array is null.
     * @exception IllegalArgumentException
     *                - an input array is invalid.
     **/
    public synchronized static int H5Sselect_hyperslab(long space_id, int op, byte[] start, byte[] stride,
            byte[] count, byte[] block) throws HDF5LibraryException, NullPointerException, IllegalArgumentException {
        ByteBuffer startbb = ByteBuffer.wrap(start);
        long[] lastart = (startbb.asLongBuffer()).array();
        ByteBuffer stridebb = ByteBuffer.wrap(stride);
        long[] lastride = (stridebb.asLongBuffer()).array();
        ByteBuffer countbb = ByteBuffer.wrap(count);
        long[] lacount = (countbb.asLongBuffer()).array();
        ByteBuffer blockbb = ByteBuffer.wrap(block);
        long[] lablock = (blockbb.asLongBuffer()).array();

        return H5Sselect_hyperslab(space_id, op, lastart, lastride, lacount, lablock);
    }

    public synchronized static native int H5Sselect_hyperslab(long space_id, int op, long[] start, long[] stride,
            long[] count, long[] block) throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
     * H5Sselect_none resets the selection region for the dataspace space_id to include no elements.
     *
     * @param space_id
     *            IN: The identifier of the dataspace to be reset.
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Sselect_none(long space_id) throws HDF5LibraryException;

    /**
     * H5Sselect_valid verifies that the selection for the dataspace.
     *
     * @param space_id
     *            The identifier for the dataspace in which the selection is being reset.
     *
     * @return true if the selection is contained within the extent and FALSE if it is not or is an error.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Sselect_valid(long space_id) throws HDF5LibraryException;

    /**
     * H5Sset_extent_none removes the extent from a dataspace and sets the type to H5S_NONE.
     *
     * @param space_id
     *            The identifier for the dataspace from which the extent is to be removed.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Sset_extent_none(long space_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Sset_extent_simple(long space_id, int rank, long[] current_size,
            long[] maximum_size) throws HDF5LibraryException, NullPointerException;

    public synchronized static long H5Sset_extent_simple(long space_id, int rank, byte[] current_size,
            byte[] maximum_size) throws HDF5LibraryException, NullPointerException {
        ByteBuffer csbb = ByteBuffer.wrap(current_size);
        long[] lacs = (csbb.asLongBuffer()).array();
        ByteBuffer maxsbb = ByteBuffer.wrap(maximum_size);
        long[] lamaxs = (maxsbb.asLongBuffer()).array();

        return H5Sset_extent_simple(space_id, rank, lacs, lamaxs);
    }

    /**
     * H5Sget_regular_hyperslab determines if a hyperslab selection is regular for the dataspace specified
     * by space_id. The start, stride, count, and block arrays must be the same size as the rank of the dataspace.
     *
     * @param space_id
     *            IN: Identifier of dataspace selection to modify
     * @param start
     *           OUT: Offset of start of hyperslab
     * @param stride
     *           OUT: Hyperslab stride.
     * @param count
     *           OUT: Number of blocks included in hyperslab.
     * @param block
     *           OUT: Size of block in hyperslab.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - an output array is null.
     * @exception IllegalArgumentException
     *                - an output array is invalid.
     **/
     public synchronized static native void H5Sget_regular_hyperslab(long space_id, long[] start, long[] stride, long[] count, long[] block) throws HDF5LibraryException, NullPointerException, IllegalArgumentException;

    /**
     * H5Sis_regular_hyperslab retrieves a regular hyperslab selection for the dataspace specified
     * by space_id.
     *
     * @param space_id
     *            IN: Identifier of dataspace selection to query
     *
     * @return a TRUE/FALSE for hyperslab selection if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
     public synchronized static native boolean H5Sis_regular_hyperslab(long space_id) throws HDF5LibraryException;

    // /////// unimplemented ////////



    // ////////////////////////////////////////////////////////////
    // //
    // H5T: Datatype Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - dim is null.
     **/
    public static long H5Tarray_create(long base_id, int ndims, long[] dim) throws HDF5LibraryException,
            NullPointerException {
        long id = _H5Tarray_create2(base_id, ndims, dim);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tarray_create add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tarray_create2(long base_id, int ndims, long[] dim)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tclose releases a datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to release.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Tclose(long type_id) throws HDF5LibraryException {
        if (type_id < 0)
            return 0; // throw new HDF5LibraryException("Negative ID");;

        log.trace("OPEN_IDS: H5Tclose remove {}", type_id);
        OPEN_IDS.remove(type_id);
        log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        return _H5Tclose(type_id);
    }

    private synchronized static native int _H5Tclose(long type_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native void H5Tcommit(long loc_id, String name, long type_id, long lcpl_id,
            long tcpl_id, long tapl_id) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tcommit_anon commits a transient datatype (not immutable) to a file, turning it into a named datatype with the
     * specified creation and property lists.
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Tcommit_anon(long loc_id, long type_id, long tcpl_id, long tapl_id)
            throws HDF5LibraryException;

    /**
     * H5Tcommitted queries a type to determine whether the type specified by the type identifier is a named type or a
     * transient type.
     *
     * @param type_id
     *            IN: Identifier of datatype.
     *
     * @return true the datatype has been committed
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Tcommitted(long type_id) throws HDF5LibraryException;

    /**
     * H5Tcompiler_conv finds out whether the library's conversion function from type src_id to type dst_id is a
     * compiler (hard) conversion.
     *
     * @param src_id
     *            IN: Identifier of source datatype.
     * @param dst_id
     *            IN: Identifier of destination datatype.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Tcompiler_conv(long src_id, long dst_id) throws HDF5LibraryException;

    /**
     ** H5Tconvert converts nelmts elements from the type specified by the src_id identifier to type dst_id.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native void H5Tconvert(long src_id, long dst_id, long nelmts, byte[] buf,
            byte[] background, long plist_id) throws HDF5LibraryException, NullPointerException;

    // int H5Tconvert(int src_id, int dst_id, long nelmts, Pointer buf, Pointer background, int plist_id);

    /**
     * H5Tcopy copies an existing datatype. The returned type is always transient and unlocked.
     *
     * @param type_id
     *            IN: Identifier of datatype to copy. Can be a datatype identifier, a predefined datatype (defined in
     *            H5Tpublic.h), or a dataset Identifier.
     *
     * @return a datatype identifier if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Tcopy(long type_id) throws HDF5LibraryException {
        long id = _H5Tcopy(type_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tcopy add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tcopy(long type_id) throws HDF5LibraryException;

    /**
     * H5Tcreate creates a new dataype of the specified class with the specified number of bytes.
     *
     * @param tclass
     *            IN: Class of datatype to create.
     * @param size
     *            IN: The number of bytes in the datatype to create.
     *
     * @return datatype identifier
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Tcreate(int tclass, long size) throws HDF5LibraryException {
        long id = _H5Tcreate(tclass, size);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tcreate add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tcreate(int type, long size) throws HDF5LibraryException;

    /**
     * H5Tdecode reconstructs the HDF5 data type object and returns a new object handle for it.
     *
     * @param buf
     *            IN: Buffer for the data type object to be decoded.
     *
     * @return a new object handle
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public static long H5Tdecode(byte[] buf) throws HDF5LibraryException, NullPointerException {
        long id = _H5Tdecode(buf);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tdecode add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tdecode(byte[] buf) throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tdetect_class determines whether the datatype specified in dtype_id contains any datatypes of the datatype
     * class specified in dtype_class.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param cls
     *            IN: Identifier of datatype cls.
     *
     * @return true if the datatype specified in dtype_id contains any datatypes of the datatype class
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Tdetect_class(long type_id, int cls) throws HDF5LibraryException;

    /**
     * H5Tencode converts a data type description into binary form in a buffer.
     *
     * @param obj_id
     *            IN: Identifier of the object to be encoded.
     * @param buf
     *            OUT: Buffer for the object to be encoded into. If the provided buffer is NULL, only the size of buffer
     *            needed is returned.
     * @param nalloc
     *            IN: The size of the allocated buffer.
     *
     * @return the size needed for the allocated buffer.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - buf is null.
     **/
    public synchronized static native int H5Tencode(long obj_id, byte[] buf, long nalloc) throws HDF5LibraryException,
            NullPointerException;

    // /**
    // * H5Tencode converts a data type description into binary form in a buffer.
    // *
    // * @param obj_id IN: Identifier of the object to be encoded.
    // *
    // * @return the buffer for the object to be encoded into.
    // *
    // * @exception HDF5LibraryException - Error from the HDF-5 Library.
    // **/
    // public synchronized static native byte[] H5Tencode(int obj_id)
    // throws HDF5LibraryException;

    /**
     * H5Tenum_create creates a new enumeration datatype based on the specified base datatype, parent_id, which must be
     * an integer type.
     *
     * @param base_id
     *            IN: Identifier of the parent datatype to release.
     *
     * @return the datatype identifier for the new enumeration datatype
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Tenum_create(long base_id) throws HDF5LibraryException {
        long id = _H5Tenum_create(base_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tenum_create add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tenum_create(long base_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native void H5Tenum_insert(long type, String name, byte[] value)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static int H5Tenum_insert(long type, String name, int[] value) throws HDF5LibraryException,
            NullPointerException {
        return H5Tenum_insert_int(type, name, value);
    }

    public static int H5Tenum_insert(long type, String name, int value) throws HDF5LibraryException,
            NullPointerException {
        int[] val = { value };
        return H5Tenum_insert_int(type, name, val);
    }

    private synchronized static native int H5Tenum_insert_int(long type, String name, int[] value)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tenum_nameof finds the symbol name that corresponds to the specified value of the enumeration datatype type.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - value is null.
     **/
    public synchronized static native String H5Tenum_nameof(long type, byte[] value, long size)
            throws HDF5LibraryException, NullPointerException;

    // int H5Tenum_nameof(int type, Pointer value, Buffer name/* out */, long size);

    /**
     * H5Tenum_nameof finds the symbol name that corresponds to the specified value of the enumeration datatype type.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static int H5Tenum_nameof(long type, int[] value, String[] name, int size) throws HDF5LibraryException,
            NullPointerException {
        return H5Tenum_nameof_int(type, value, name, size);
    }

    private synchronized static native int H5Tenum_nameof_int(long type, int[] value, String[] name, int size)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tenum_valueof finds the value that corresponds to the specified name of the enumeration datatype type.
     *
     * @param type
     *            IN: Identifier of datatype.
     * @param name
     *            IN: The name of the member
     * @param value
     *            OUT: The value of the member
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Tenum_valueof(long type, String name, byte[] value)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tenum_valueof finds the value that corresponds to the specified name of the enumeration datatype type.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static int H5Tenum_valueof(long type, String name, int[] value) throws HDF5LibraryException,
            NullPointerException {
        return H5Tenum_valueof_int(type, name, value);
    }

    private synchronized static native int H5Tenum_valueof_int(long type, String name, int[] value)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Tequal(long type_id1, long type_id2) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - dims is null.
     **/
    public static int H5Tget_array_dims(long type_id, long[] dims) throws HDF5LibraryException, NullPointerException {
        return H5Tget_array_dims2(type_id, dims);
    }

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - dims is null.
     **/
    public synchronized static native int H5Tget_array_dims2(long type_id, long[] dims) throws HDF5LibraryException,
            NullPointerException;

    /**
     * H5Tget_array_ndims returns the rank, the number of dimensions, of an array datatype object.
     *
     * @param type_id
     *            IN: Datatype identifier of array object.
     *
     * @return the rank of the array
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_array_ndims(long type_id) throws HDF5LibraryException;

    /**
     * H5Tget_class returns the datatype class identifier.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return datatype class identifier if successful; otherwise H5T_NO_CLASS(-1).
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_class(long type_id) throws HDF5LibraryException;

    /**
     * H5Tget_class_name returns the datatype class identifier.
     *
     * @param class_id
     *            IN: Identifier of class from H5Tget_class.
     *
     * @return class name if successful; otherwise H5T_NO_CLASS.
     *
     **/
    public static String H5Tget_class_name(long class_id) {
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
        else if (HDF5Constants.H5T_REFERENCE == class_id)/* reference types */
            retValue = "H5T_REFERENCE";
        else if (HDF5Constants.H5T_ENUM == class_id) /* enumeration types */
            retValue = "H5T_ENUM";
        else if (HDF5Constants.H5T_VLEN == class_id) /* Variable-Length types */
            retValue = "H5T_VLEN";
        else if (HDF5Constants.H5T_ARRAY == class_id) /* Array types */
            retValue = "H5T_ARRAY";
        else
            retValue = "H5T_NO_CLASS";

        return retValue;
    }

    /**
     * H5Tget_create_plist returns a property list identifier for the datatype creation property list associated with
     * the datatype specified by type_id.
     *
     * @param type_id
     *            IN: Identifier of datatype.
     *
     * @return a datatype property list identifier.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Tget_create_plist(long type_id) throws HDF5LibraryException {
        long id = _H5Tget_create_plist(type_id);
        if (id > 0) {
            log.trace("OPEN_IDS: _H5Tget_create_plist add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tget_create_plist(long type_id) throws HDF5LibraryException;

    /**
     * H5Tget_cset retrieves the character set type of a string datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid character set type if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_cset(long type_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_cset(long type_id, int cset) throws HDF5LibraryException;

    /**
     * H5Tget_ebias retrieves the exponent bias of a floating-point type.
     *
     * @param type_id
     *            Identifier of datatype to query.
     *
     * @return the bias if successful; otherwise 0.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_ebias(long type_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Tset_ebias(long type_id, int ebias) throws HDF5LibraryException {
        H5Tset_ebias(type_id, (long) ebias);
        return 0;
    }

    /**
     * H5Tget_ebias retrieves the exponent bias of a floating-point type.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return the bias
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Tget_ebias_long(long type_id) throws HDF5LibraryException;

    /**
     * H5Tset_ebias sets the exponent bias of a floating-point type.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param ebias
     *            IN: Exponent bias value.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Tset_ebias(long type_id, long ebias) throws HDF5LibraryException;

    /**
     * H5Tget_fields retrieves information about the locations of the various bit fields of a floating point datatype.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - fields is null.
     * @exception IllegalArgumentException
     *                - fields array is invalid.
     **/
    public synchronized static native void H5Tget_fields(long type_id, long[] fields) throws HDF5LibraryException,
            NullPointerException, IllegalArgumentException;

    /**
     * H5Tget_fields retrieves information about the locations of the various bit fields of a floating point datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param fields
     *            OUT: location of size and bit-position.
     *
     *            <pre>
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - fields is null.
     * @exception IllegalArgumentException
     *                - fields array is invalid.
     **/
    public static int H5Tget_fields(long type_id, int[] fields) throws HDF5LibraryException, NullPointerException,
            IllegalArgumentException {
        return H5Tget_fields_int(type_id, fields);
    }

    private synchronized static native int H5Tget_fields_int(long type_id, int[] fields) throws HDF5LibraryException,
            NullPointerException, IllegalArgumentException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Tset_fields(long type_id, long spos, long epos, long esize, long mpos,
            long msize) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Tset_fields(long type_id, int spos, int epos, int esize, int mpos, int msize)
            throws HDF5LibraryException {
        H5Tset_fields(type_id, (long) spos, (long) epos, (long) esize, (long) mpos, (long) msize);
        return 0;
    }

    /**
     * H5Tget_inpad retrieves the internal padding type for unused bits in floating-point datatypes.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid padding type if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_inpad(long type_id) throws HDF5LibraryException;

    /**
     * If any internal bits of a floating point type are unused (that is, those significant bits which are not part of
     * the sign, exponent, or mantissa), then H5Tset_inpad will be filled according to the value of the padding value
     * property inpad.
     *
     * @param type_id
     *            IN: Identifier of datatype to modify.
     * @param inpad
     *            IN: Padding type.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_inpad(long type_id, int inpad) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_member_class(long type_id, int membno) throws HDF5LibraryException;

    /**
     * H5Tget_member_index retrieves the index of a field of a compound datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param field_name
     *            IN: Field name of the field index to retrieve.
     *
     * @return if field is defined, the index; else negative.
     **/
    public synchronized static native int H5Tget_member_index(long type_id, String field_name);

    /**
     * H5Tget_member_name retrieves the name of a field of a compound datatype or an element of an enumeration datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param field_idx
     *            IN: Field index (0-based) of the field name to retrieve.
     *
     * @return a valid pointer to the name if successful; otherwise null.
     **/
    public synchronized static native String H5Tget_member_name(long type_id, int field_idx);

    /**
     * H5Tget_member_offset returns the byte offset of the specified member of the compound datatype. This is the byte
     * offset in the HDF-5 file/library, NOT the offset of any Java object which might be mapped to this data item.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     * @param membno
     *            IN: Field index (0-based) of the field type to retrieve.
     *
     * @return the offset of the member.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Tget_member_offset(long type_id, int membno) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Tget_member_type(long type_id, int field_idx) throws HDF5LibraryException {
        long id = _H5Tget_member_type(type_id, field_idx);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tget_member_type add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tget_member_type(long type_id, int field_idx)
            throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - value is null.
     **/
    public synchronized static native void H5Tget_member_value(long type_id, int membno, byte[] value)
            throws HDF5LibraryException, NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - value is null.
     **/
    public static int H5Tget_member_value(long type_id, int membno, int[] value) throws HDF5LibraryException,
            NullPointerException {
        return H5Tget_member_value_int(type_id, membno, value);
    }

    private synchronized static native int H5Tget_member_value_int(long type_id, int membno, int[] value)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tget_native_type returns the equivalent native datatype for the datatype specified in type_id.
     *
     * @param type_id
     *            IN: Identifier of datatype to query. Direction of search is assumed to be in ascending order.
     *
     * @return the native datatype identifier for the specified dataset datatype.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static long H5Tget_native_type(long type_id) throws HDF5LibraryException {
        return H5Tget_native_type(type_id, HDF5Constants.H5T_DIR_ASCEND);
    }

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Tget_native_type(long type_id, int direction) throws HDF5LibraryException {
        long id = _H5Tget_native_type(type_id, direction);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tget_native_type add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tget_native_type(long tid, int direction) throws HDF5LibraryException;

    /**
     * H5Tget_nmembers retrieves the number of fields a compound datatype has.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return number of members datatype has if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_nmembers(long type_id) throws HDF5LibraryException;

    /**
     * H5Tget_norm retrieves the mantissa normalization of a floating-point datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid normalization type if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_norm(long type_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_norm(long type_id, int norm) throws HDF5LibraryException;

    /**
     * H5Tget_offset retrieves the bit offset of the first significant bit.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a positive offset value if successful; otherwise 0.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_offset(long type_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Tset_offset(long type_id, int offset) throws HDF5LibraryException {
        H5Tset_offset(type_id, (long) offset);
        return 0;
    }

    /**
     * H5Tset_offset sets the bit offset of the first significant bit.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param offset
     *            IN: Offset of first significant bit.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Tset_offset(long type_id, long offset) throws HDF5LibraryException;

    /**
     * H5Tget_order returns the byte order of an atomic datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a byte order constant if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_order(long type_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_order(long type_id, int order) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - pad is null.
     **/
    public synchronized static native int H5Tget_pad(long type_id, int[] pad) throws HDF5LibraryException,
            NullPointerException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_pad(long type_id, int lsb, int msb) throws HDF5LibraryException;

    /**
     * H5Tget_precision returns the precision of an atomic datatype.
     *
     * @param type_id
     *            Identifier of datatype to query.
     *
     * @return the number of significant bits if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_precision(long type_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public static int H5Tset_precision(long type_id, int precision) throws HDF5LibraryException {
        H5Tset_precision(type_id, (long) precision);
        return 0;
    }

    /**
     * H5Tget_precision returns the precision of an atomic datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return the number of significant bits if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Tget_precision_long(long type_id) throws HDF5LibraryException;

    /**
     * H5Tset_precision sets the precision of an atomic datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param precision
     *            IN: Number of bits of precision for datatype.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Tset_precision(long type_id, long precision) throws HDF5LibraryException;

    /**
     * H5Tget_sign retrieves the sign type for an integer type.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid sign type if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_sign(long type_id) throws HDF5LibraryException;

    /**
     * H5Tset_sign sets the sign proprety for an integer type.
     *
     * @param type_id
     *            IN: Identifier of datatype to set.
     * @param sign
     *            IN: Sign type.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_sign(long type_id, int sign) throws HDF5LibraryException;

    /**
     * H5Tget_size returns the size of a datatype in bytes.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return the size of the datatype in bytes
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native long H5Tget_size(long type_id) throws HDF5LibraryException;

    /**
     * H5Tset_size sets the total size in bytes, size, for an atomic datatype (this operation is not permitted on
     * compound datatypes).
     *
     * @param type_id
     *            IN: Identifier of datatype to change size.
     * @param size
     *            IN: Size in bytes to modify datatype.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_size(long type_id, long size) throws HDF5LibraryException;

    /**
     * H5Tget_strpad retrieves the string padding method for a string datatype.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return a valid string padding type if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tget_strpad(long type_id) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_strpad(long type_id, int strpad) throws HDF5LibraryException;

    /**
     * H5Tget_super returns the type from which TYPE is derived.
     *
     * @param type
     *            IN: Identifier of datatype.
     *
     * @return the parent type
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Tget_super(long type) throws HDF5LibraryException {
        long id = _H5Tget_super(type);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tget_super add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tget_super(long type) throws HDF5LibraryException;

    /**
     * H5Tget_tag returns the tag associated with datatype type_id.
     *
     * @param type
     *            IN: Identifier of datatype.
     *
     * @return the tag
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native String H5Tget_tag(long type) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tset_tag(long type, String tag) throws HDF5LibraryException;

    /**
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public synchronized static native int H5Tinsert(long type_id, String name, long offset, long field_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tis_variable_str determines whether the datatype identified in type_id is a variable-length string.
     *
     * @param type_id
     *            IN: Identifier of datatype to query.
     *
     * @return true if type_id is a variable-length string.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native boolean H5Tis_variable_str(long type_id) throws HDF5LibraryException;

    /**
     * H5Tlock locks the datatype specified by the type_id identifier, making it read-only and non-destrucible.
     *
     * @param type_id
     *            IN: Identifier of datatype to lock.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tlock(long type_id) throws HDF5LibraryException;

    /**
     * H5Topen opens a named datatype at the location specified by loc_id and return an identifier for the datatype.
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
     *                - Error from the HDF-5 Library.
     * @exception NullPointerException
     *                - name is null.
     **/
    public static long H5Topen(long loc_id, String name, long tapl_id) throws HDF5LibraryException,
    NullPointerException {
        long id = _H5Topen2(loc_id, name, tapl_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Topen add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Topen2(long loc_id, String name, long tapl_id)
            throws HDF5LibraryException, NullPointerException;

    /**
     * H5Tpack recursively removes padding from within a compound datatype to make it more efficient (space-wise) to
     * store that data.
     * <P>
     * <b>WARNING:</b> This call only affects the C-data, even if it succeeds, there may be no visible effect on Java
     * objects.
     *
     * @param type_id
     *            IN: Identifier of datatype to modify.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native int H5Tpack(long type_id) throws HDF5LibraryException;

    /**
     * H5Tvlen_create creates a new variable-length (VL) dataype.
     *
     * @param base_id
     *            IN: Identifier of parent datatype.
     *
     * @return a non-negative value if successful
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public static long H5Tvlen_create(long base_id) throws HDF5LibraryException {
        long id = _H5Tvlen_create(base_id);
        if (id > 0) {
            log.trace("OPEN_IDS: H5Tvlen_create add {}", id);
            OPEN_IDS.add(id);
            log.trace("OPEN_IDS: {}", OPEN_IDS.size());
        }
        return id;
    }

    private synchronized static native long _H5Tvlen_create(long base_id) throws HDF5LibraryException;

    /**
     * H5Tflush causes all buffers associated with a committed datatype to be immediately flushed to disk
     * without removing the data from the cache.
     *
     * @param dtype_id
     *            IN: Identifier of the committed datatype to be flushed.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Tflush(long dtype_id) throws HDF5LibraryException;

    /**
     * H5Trefresh causes all buffers associated with a committed datatype to be cleared and immediately
     * re-loaded with updated contents from disk. This function essentially closes the datatype, evicts
     * all metadata associated with it from the cache, and then re-opens the datatype. The reopened datatype
     * is automatically re-registered with the same ID.
     *
     * @param dtype_id
     *            IN: Identifier of the committed datatype to be refreshed.
     *
     * @exception HDF5LibraryException
     *                - Error from the HDF-5 Library.
     **/
    public synchronized static native void H5Trefresh(long dtype_id) throws HDF5LibraryException;

    // /////// unimplemented ////////

    // H5T_conv_t H5Tfind(int src_id, int dst_id, H5T_cdata_t *pcdata);

    // public synchronized static native int H5Tregister(H5T_pers_t pers, String name, int src_id, int dst_id,
    // H5T_conv_t func)
    // throws HDF5LibraryException, NullPointerException;

    // public synchronized static native int H5Tunregister(H5T_pers_t pers, String name, int src_id, int dst_id,
    // H5T_conv_t func)
    // throws HDF5LibraryException, NullPointerException;

    // ////////////////////////////////////////////////////////////
    // //
    // H5Z: Filter Interface Functions //
    // //
    // ////////////////////////////////////////////////////////////

    public synchronized static native int H5Zfilter_avail(int filter) throws HDF5LibraryException, NullPointerException;

    public synchronized static native int H5Zget_filter_info(int filter) throws HDF5LibraryException;

    public synchronized static native int H5Zunregister(int filter) throws HDF5LibraryException, NullPointerException;

}

// /////// unimplemented ////////

// herr_t H5Zregister(const void *cls);

