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

package hdf.hdf5lib.exceptions;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

/**
 * <p>
 * The class HDF5LibraryException returns errors raised by the HDF5 library.
 * <p>
 * Each major error code from the HDF-5 Library is represented by a sub-class of
 * this class, and by default the 'detailedMessage' is set according to the
 * minor error code from the HDF-5 Library.
 * <p>
 * For major and minor error codes, see <b>H5Epublic.h</b> in the HDF-5 library.
 *
 */

@SuppressWarnings("serial")
public class HDF5LibraryException extends HDF5Exception {
    private final long majorErrorNumber;
    private final long minorErrorNumber;

    /**
     * Constructs an <code>HDF5LibraryException</code> with no specified detail
     * message.
     */
    public HDF5LibraryException() {
        super();

        // this code forces the loading of the HDF-5 library
        // to assure that the native methods are available
        try {
            H5.H5open();
        }
        catch (Exception e) {
        }

        this.majorErrorNumber = _getMajorErrorNumber();
        this.minorErrorNumber = _getMinorErrorNumber();
        detailMessage = getMinorError(minorErrorNumber);
    }

    /**
     * Constructs an <code>HDF5LibraryException</code> with the specified detail
     * message.
     *
     * @param s
     *            the detail message.
     */
    public HDF5LibraryException(String s) {
        super(s);
        // this code forces the loading of the HDF-5 library
        // to assure that the native methods are available
        try {
            H5.H5open();
        }
        catch (Exception e) {
        }
        this.majorErrorNumber = _getMajorErrorNumber();
        this.minorErrorNumber = _getMinorErrorNumber();
    }

    /**
     * Get the major error number of the first error on the HDF5 library error
     * stack.
     *
     * @return the major error number
     */
    public long getMajorErrorNumber()
    {
        return majorErrorNumber;
    }
    private native long _getMajorErrorNumber();

    /**
     * Get the minor error number of the first error on the HDF5 library error
     * stack.
     *
     * @return the minor error number
     */
    public long getMinorErrorNumber()
    {
        return minorErrorNumber;
    }
    private native long _getMinorErrorNumber();

    /**
     * Return a error message for the minor error number.
     * <p>
     * These messages come from <b>H5Epublic.h</b>.
     *
     * @param err_code
     *            the error code
     *
     * @return the string of the minor error
     */
    public String getMinorError(long err_code) {
        if (err_code == 0) {
            return "special zero no error";
        }
        else if (err_code == HDF5Constants.H5E_UNINITIALIZED) {
            return "information is unitialized";
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
        else if (err_code == HDF5Constants.H5E_BADATOM) {
            return "Can't find atom information";
        }
        else if (err_code == HDF5Constants.H5E_BADGROUP) {
            return "Can't find group information";
        }
        else if (err_code == HDF5Constants.H5E_CANTREGISTER) {
            return "Can't register new atom";
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
            // }
            // else
            // if
            // (err_code
            // ==
            // HDF5Constants.H5E_CANTRECV
            // )
            // {
            // return
            // "can't receive messages from processes";
            // }
            // else
            // if
            // (err_code
            // ==
            // HDF5Constants.H5E_CANTALLOC
            // )
            // {
            // return
            // "can't allocate from file";
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
     * Prints this <code>HDF5LibraryException</code>, the HDF-5 Library error
     * stack, and and the Java stack trace to the standard error stream.
     */
    @Override
    public void printStackTrace() {
        System.err.println(this);
        printStackTrace0(null); // the HDF-5 Library error stack
        super.printStackTrace(); // the Java stack trace
    }

    /**
     * Prints this <code>HDF5LibraryException</code> the HDF-5 Library error
     * stack, and and the Java stack trace to the specified print stream.
     *
     * @param f
     *            the file print stream.
     */
    public void printStackTrace(java.io.File f) {
        if ((f == null) || !f.exists() || f.isDirectory() || !f.canWrite()) {
            printStackTrace();
        }
        else {
            try {
                java.io.FileOutputStream o = new java.io.FileOutputStream(f);
                java.io.PrintWriter p = new java.io.PrintWriter(o);
                p.println(this);
                p.close();
            }
            catch (Exception ex) {
                System.err.println(this);
            }
            ;
            // the HDF-5 Library error stack
            printStackTrace0(f.getPath());
            super.printStackTrace(); // the Java stack trace
        }
    }

    /*
     * This private method calls the HDF-5 library to extract the error codes
     * and error stack.
     */
    private native void printStackTrace0(String s);

}
