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

package hdf.hdf5lib.exceptions;

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemoryLayout;
import java.lang.foreign.MemoryLayout.PathElement;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.StructLayout;
import java.lang.foreign.ValueLayout;
import java.lang.invoke.VarHandle;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

import org.hdfgroup.javahdf5.*;

/**
 * \page ERRORSLIB HDF5 Library Errors and Exceptions
 * The class HDF5LibraryException returns errors raised by the HDF5 library.
 *
 * Each major error code from the HDF5 Library is represented by a sub-class of
 * this class, and by default the 'detailedMessage' is set according to the
 * minor error code from the HDF5 Library.
 * <p>
 * For major and minor error codes, @see <b>@ref H5E</b> in the HDF5 library.
 *
 * @defgroup JERRLIB HDF5 Library JNI Exception Interface
 *
 */

@SuppressWarnings("serial")
public class HDF5LibraryException extends HDF5Exception {
    /** major error number of the first error on the HDF5 library error stack. */
    private long majorErrorNumber = 0;
    /** minor error number of the first error on the HDF5 library error stack. */
    private long minorErrorNumber = 0;

    static class WalkData {
        public String err_desc  = null;
        public String func_name = null;
        public int line         = -1;
        WalkData(String desc, String func, int lineno)
        {
            this.err_desc  = new String(desc);
            this.func_name = new String(func);
            this.line      = lineno;
        }
    }

    static class H5EWalkCallback implements H5E_walk2_t.Function {
        private static final ArrayList<WalkData> walkDataList = new ArrayList<>();
        /** major error number of the first error on the HDF5 library error stack. */
        private long majorErrorNumber = 0;
        /** minor error number of the first error on the HDF5 library error stack. */
        private long minorErrorNumber = 0;
        public long getMajor() { return this.majorErrorNumber; }
        public long getMinor() { return this.minorErrorNumber; }

        /* major and minor error numbers */
        final StructLayout H5E_num_t = MemoryLayout.structLayout(ValueLayout.JAVA_LONG.withName("maj_num"),
                                                                 ValueLayout.JAVA_LONG.withName("min_num"));

        /**
         * This method is called by the HDF5 library during the error stack walk. It extracts the error
         * description, function name, and line number from the H5E_error2_t structure and stores it in a
         * list.
         *
         * @param nidx        The index of the error in the stack.
         * @param err_desc    A MemorySegment containing the error description.
         * @param err_nums    The major and minor error numbers not used.
         * @return 0 to continue walking the stack.
         */
        // err_desc is a pointer to the H5E_error2_t structure
        public int apply(int nidx, MemorySegment err_desc, MemorySegment err_nums)
        {
            try (Arena arena = Arena.ofConfined()) {
                // Extract error information from the native structure
                String errDesc  = H5E_error2_t.desc(err_desc).getString(0);
                String funcName = H5E_error2_t.func_name(err_desc).getString(0);
                int line        = H5E_error2_t.line(err_desc);
                walkDataList.add(new WalkData(errDesc, funcName, line));
                this.majorErrorNumber = H5E_error2_t.maj_num(err_desc);
                this.minorErrorNumber = H5E_error2_t.min_num(err_desc);
            }
            return 0; // Continue walking
        }
    }

    /**
     * @ingroup JERRLIB
     *
     * Constructs an <code>HDF5LibraryException</code> with no specified detail
     * message.
     */
    public HDF5LibraryException()
    {
        super();

        H5EWalkCallback callback = new H5EWalkCallback();

        long stk_id = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment callbackSegment = H5E_walk2_t.allocate(callback, arena);

            final StructLayout H5E_num_t = MemoryLayout.structLayout(
                ValueLayout.JAVA_LONG.withName("maj_num"), ValueLayout.JAVA_LONG.withName("min_num"));
            // Walk the error stack
            MemorySegment errorNums = arena.allocate(H5E_num_t); // For maj_num and min_num
            /* Save current stack contents for future use */
            if ((stk_id = H5Eget_current_stack()) >= 0) {
                /* This will clear current stack */
                int walkResult = (int)H5Ewalk2(stk_id, H5E_WALK_DOWNWARD(), callbackSegment, errorNums);
                if (walkResult < 0) {
                    throw new IllegalStateException("Failed to walk HDF5 error stack.");
                }
                H5Eset_current_stack(stk_id);
            }
            else
                throw new IllegalStateException("Failed to get current HDF5 error stack.");

            this.majorErrorNumber = callback.getMajor();
            this.minorErrorNumber = callback.getMinor();
            if (detailMessage == null)
                detailMessage = getMinorError(this.minorErrorNumber);
        }
        catch (Throwable e) {
            e.printStackTrace();
        }
    }

    /**
     * @ingroup JERRLIB
     *
     * Constructs an <code>HDF5LibraryException</code> with the specified detail
     * message.
     *
     * @param s
     *            the detail message.
     */
    public HDF5LibraryException(String s)
    {
        this();
        detailMessage = s;
    }

    /**
     * @ingroup JERRLIB
     *
     * Get the major error number of the first error on the HDF5 library error
     * stack.
     *
     * @return the major error number
     */
    public long getMajorErrorNumber() { return this.majorErrorNumber; }

    /**
     * @ingroup JERRLIB
     *
     * Get the minor error number of the first error on the HDF5 library error
     * stack.
     *
     * @return the minor error number
     */
    public long getMinorErrorNumber() { return this.minorErrorNumber; }

    /**
     * @ingroup JERRLIB
     *
     * Return an error message for the minor error number.
     *
     * These messages come from <b>@ref H5E</b>.
     *
     * @param err_code
     *            the error code
     *
     * @return the string of the minor error
     */
    public String getMinorError(long err_code)
    {
        if (err_code == 0) {
            return "special zero no error";
        }
        else if (err_code == HDF5Constants.H5E_UNINITIALIZED) {
            return "information is uninitialized";
        }
        else if (err_code == HDF5Constants.H5E_UNSUPPORTED) {
            return "feature is unsupported";
        }
        else if (err_code == HDF5Constants.H5E_BADTYPE) {
            return "incorrect type found";
        }
        else if (err_code == HDF5Constants.H5E_BADRANGE) {
            return "argument out of range";
        }
        else if (err_code == HDF5Constants.H5E_BADVALUE) {
            return "bad value for argument";
        }
        else if (err_code == HDF5Constants.H5E_NOSPACE) {
            return "no space available for allocation";
        }
        else if (err_code == HDF5Constants.H5E_CANTCOPY) {
            return "unable to copy object";
        }
        else if (err_code == HDF5Constants.H5E_CANTFREE) {
            return "unable to free object";
        }
        else if (err_code == HDF5Constants.H5E_ALREADYEXISTS) {
            return "Object already exists";
        }
        else if (err_code == HDF5Constants.H5E_CANTLOCK) {
            return "Unable to lock object";
        }
        else if (err_code == HDF5Constants.H5E_CANTUNLOCK) {
            return "Unable to unlock object";
        }
        else if (err_code == HDF5Constants.H5E_FILEEXISTS) {
            return "file already exists";
        }
        else if (err_code == HDF5Constants.H5E_FILEOPEN) {
            return "file already open";
        }
        else if (err_code == HDF5Constants.H5E_CANTCREATE) {
            return "Can't create file";
        }
        else if (err_code == HDF5Constants.H5E_CANTOPENFILE) {
            return "Can't open file";
        }
        else if (err_code == HDF5Constants.H5E_CANTCLOSEFILE) {
            return "Can't close file";
        }
        else if (err_code == HDF5Constants.H5E_NOTHDF5) {
            return "not an HDF5 format file";
        }
        else if (err_code == HDF5Constants.H5E_BADFILE) {
            return "bad file ID accessed";
        }
        else if (err_code == HDF5Constants.H5E_TRUNCATED) {
            return "file has been truncated";
        }
        else if (err_code == HDF5Constants.H5E_MOUNT) {
            return "file mount error";
        }
        else if (err_code == HDF5Constants.H5E_CANTDELETEFILE) {
            return "Unable to delete file";
        }
        else if (err_code == HDF5Constants.H5E_SEEKERROR) {
            return "seek failed";
        }
        else if (err_code == HDF5Constants.H5E_READERROR) {
            return "read failed";
        }
        else if (err_code == HDF5Constants.H5E_WRITEERROR) {
            return "write failed";
        }
        else if (err_code == HDF5Constants.H5E_CLOSEERROR) {
            return "close failed";
        }
        else if (err_code == HDF5Constants.H5E_OVERFLOW) {
            return "address overflowed";
        }
        else if (err_code == HDF5Constants.H5E_FCNTL) {
            return "file fcntl failed";
        }
        else if (err_code == HDF5Constants.H5E_CANTINIT) {
            return "Can't initialize object";
        }
        else if (err_code == HDF5Constants.H5E_ALREADYINIT) {
            return "object already initialized";
        }
        else if (err_code == HDF5Constants.H5E_CANTRELEASE) {
            return "Can't release object";
        }
        else if (err_code == HDF5Constants.H5E_BADID) {
            return "Can't find ID information";
        }
        else if (err_code == HDF5Constants.H5E_BADGROUP) {
            return "Can't find group information";
        }
        else if (err_code == HDF5Constants.H5E_CANTREGISTER) {
            return "Can't register new ID";
        }
        else if (err_code == HDF5Constants.H5E_CANTINC) {
            return "Can't increment reference count";
        }
        else if (err_code == HDF5Constants.H5E_CANTDEC) {
            return "Can't decrement reference count";
        }
        else if (err_code == HDF5Constants.H5E_NOIDS) {
            return "Out of IDs for group";
        }
        else if (err_code == HDF5Constants.H5E_CANTFLUSH) {
            return "Can't flush object from cache";
        }
        else if (err_code == HDF5Constants.H5E_CANTLOAD) {
            return "Can't load object into cache";
        }
        else if (err_code == HDF5Constants.H5E_PROTECT) {
            return "protected object error";
        }
        else if (err_code == HDF5Constants.H5E_NOTCACHED) {
            return "object not currently cached";
        }
        else if (err_code == HDF5Constants.H5E_NOTFOUND) {
            return "object not found";
        }
        else if (err_code == HDF5Constants.H5E_EXISTS) {
            return "object already exists";
        }
        else if (err_code == HDF5Constants.H5E_CANTENCODE) {
            return "Can't encode value";
        }
        else if (err_code == HDF5Constants.H5E_CANTDECODE) {
            return "Can't decode value";
        }
        else if (err_code == HDF5Constants.H5E_CANTSPLIT) {
            return "Can't split node";
        }
        else if (err_code == HDF5Constants.H5E_CANTINSERT) {
            return "Can't insert object";
        }
        else if (err_code == HDF5Constants.H5E_CANTLIST) {
            return "Can't list node";
        }
        else if (err_code == HDF5Constants.H5E_LINKCOUNT) {
            return "bad object header link count";
        }
        else if (err_code == HDF5Constants.H5E_VERSION) {
            return "wrong version number";
        }
        else if (err_code == HDF5Constants.H5E_ALIGNMENT) {
            return "alignment error";
        }
        else if (err_code == HDF5Constants.H5E_BADMESG) {
            return "unrecognized message";
        }
        else if (err_code == HDF5Constants.H5E_CANTDELETE) {
            return "Can't delete message";
        }
        else if (err_code == HDF5Constants.H5E_CANTOPENOBJ) {
            return "Can't open object";
        }
        else if (err_code == HDF5Constants.H5E_COMPLEN) {
            return "name component is too long";
        }
        else if (err_code == HDF5Constants.H5E_LINK) {
            return "link count failure";
        }
        else if (err_code == HDF5Constants.H5E_CANTCONVERT) {
            return "Can't convert datatypes";
        }
        else if (err_code == HDF5Constants.H5E_BADSIZE) {
            return "Bad size for object";
        }
        else if (err_code == HDF5Constants.H5E_CANTCLIP) {
            return "Can't clip hyperslab region";
        }
        else if (err_code == HDF5Constants.H5E_CANTCOUNT) {
            return "Can't count elements";
        }
        else if (err_code == HDF5Constants.H5E_CANTSELECT) {
            return "Can't select hyperslab";
        }
        else if (err_code == HDF5Constants.H5E_CANTNEXT) {
            return "Can't move to next iterator location";
        }
        else if (err_code == HDF5Constants.H5E_BADSELECT) {
            return "Invalid selection";
        }
        else if (err_code == HDF5Constants.H5E_CANTGET) {
            return "Can't get value";
        }
        else if (err_code == HDF5Constants.H5E_CANTSET) {
            return "Can't set value";
        }
        else if (err_code == HDF5Constants.H5E_DUPCLASS) {
            return "Duplicate class name in parent class";
        }
        else if (err_code == HDF5Constants.H5E_MPI) {
            return "some MPI function failed";
        }
        else if (err_code == HDF5Constants.H5E_MPIERRSTR) {
            return "MPI Error String";
        }
        else if (err_code == HDF5Constants.H5E_CANTRECV) {
            return "can't receive messages from processes";
        }
        else if (err_code == HDF5Constants.H5E_CANTALLOC) {
            return "can't allocate from file";
        }
        else if (err_code == HDF5Constants.H5E_NOFILTER) {
            return "requested filter is not available";
        }
        else if (err_code == HDF5Constants.H5E_CALLBACK) {
            return "callback failed";
        }
        else if (err_code == HDF5Constants.H5E_CANAPPLY) {
            return "error from filter \"can apply\" callback";
        }
        else if (err_code == HDF5Constants.H5E_SETLOCAL) {
            return "error from filter \"set local\" callback";
        }
        else {
            return "undefined error(" + err_code + ")";
        }
    }

    /**
     * @ingroup JERRLIB
     *
     * Prints this <code>HDF5LibraryException</code>, the HDF5 Library error
     * stack, and and the Java stack trace to the standard error stream.
     */
    @Override
    public void printStackTrace()
    {
        System.err.println(this);
        printStackTrace0(null);  // the HDF5 Library error stack
        super.printStackTrace(); // the Java stack trace
    }

    /**
     * @ingroup JERRLIB
     *
     * Prints this <code>HDF5LibraryException</code> the HDF5 Library error
     * stack, and and the Java stack trace to the specified print stream.
     *
     * @param f
     *            the file print stream.
     */
    public void printStackTrace(java.io.File f)
    {
        if ((f == null) || !f.exists() || f.isDirectory() || !f.canWrite()) {
            printStackTrace();
        }
        else {
            try {
                java.io.FileOutputStream o = new java.io.FileOutputStream(f);
                java.io.PrintWriter p      = new java.io.PrintWriter(o);
                p.println(this);
                p.close();
            }
            catch (Exception ex) {
                System.err.println(this);
            };
            // the HDF5 Library error stack
            printStackTrace0(f.getPath());
            super.printStackTrace(); // the Java stack trace
        }
    }

    /*
     * This private method calls the HDF5 library to extract the error codes
     * and error stack.
     */
    private void printStackTrace0(String file_name)
    {
        hdf.hdf5lib.H5.H5Eprint2(HDF5Constants.H5E_DEFAULT, null);
    }

    /*
     *  throwHDF5LibraryException()  throws the sub-class Exception
     *  corresponding to the HDF5 error code.
     */
    public static void throwHDF5LibraryException(long err_num, String errorMessage)
        throws HDF5LibraryException
    {
        if (HDF5Constants.H5E_ARGS == err_num)
            throw new HDF5FunctionArgumentException(errorMessage);
        else if (HDF5Constants.H5E_RESOURCE == err_num)
            throw new HDF5ResourceUnavailableException(errorMessage);
        else if (HDF5Constants.H5E_INTERNAL == err_num)
            throw new HDF5InternalErrorException(errorMessage);
        else if (HDF5Constants.H5E_FILE == err_num)
            throw new HDF5FileInterfaceException(errorMessage);
        else if (HDF5Constants.H5E_IO == err_num)
            throw new HDF5LowLevelIOException(errorMessage);
        else if (HDF5Constants.H5E_FUNC == err_num)
            throw new HDF5FunctionEntryExitException(errorMessage);
        else if (HDF5Constants.H5E_ID == err_num)
            throw new HDF5IdException(errorMessage);
        else if (HDF5Constants.H5E_CACHE == err_num)
            throw new HDF5MetaDataCacheException(errorMessage);
        else if (HDF5Constants.H5E_BTREE == err_num)
            throw new HDF5BtreeException(errorMessage);
        else if (HDF5Constants.H5E_SYM == err_num)
            throw new HDF5SymbolTableException(errorMessage);
        else if (HDF5Constants.H5E_HEAP == err_num)
            throw new HDF5HeapException(errorMessage);
        else if (HDF5Constants.H5E_OHDR == err_num)
            throw new HDF5ObjectHeaderException(errorMessage);
        else if (HDF5Constants.H5E_DATATYPE == err_num)
            throw new HDF5DatatypeInterfaceException(errorMessage);
        else if (HDF5Constants.H5E_DATASPACE == err_num)
            throw new HDF5DataspaceInterfaceException(errorMessage);
        else if (HDF5Constants.H5E_DATASET == err_num)
            throw new HDF5DatasetInterfaceException(errorMessage);
        else if (HDF5Constants.H5E_STORAGE == err_num)
            throw new HDF5DataStorageException(errorMessage);
        else if (HDF5Constants.H5E_PLIST == err_num)
            throw new HDF5PropertyListInterfaceException(errorMessage);
        else if (HDF5Constants.H5E_ATTR == err_num)
            throw new HDF5AttributeException(errorMessage);
        else if (HDF5Constants.H5E_PLINE == err_num)
            throw new HDF5DataFiltersException(errorMessage);
        else if (HDF5Constants.H5E_EFL == err_num)
            throw new HDF5ExternalFileListException(errorMessage);
        else if (HDF5Constants.H5E_REFERENCE == err_num)
            throw new HDF5ReferenceException(errorMessage);

        throw new HDF5LibraryException(errorMessage);
    }
}
