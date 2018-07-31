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

/**
 * <p>
 * The class HDF5Exception returns errors from the Java HDF5 Interface.
 * <p>
 * Two sub-classes of HDF5Exception are defined:
 * <ol>
 * <li>
 * HDF5LibraryException -- errors raised the HDF5 library code
 * <li>
 * HDF5JavaException -- errors raised the HDF5 Java wrapper code
 * </ol>
 * <p>
 * These exceptions are sub-classed to represent specific error conditions, as
 * needed. In particular, HDF5LibraryException has a sub-class for each major
 * error code returned by the HDF5 library.
 *
 */
public class HDF5Exception extends RuntimeException {
    protected String detailMessage;

    /**
     * Constructs an <code>HDF5Exception</code> with no specified detail
     * message.
     */
    public HDF5Exception() {
        super();
    }

    /**
     * Constructs an <code>HDF5Exception</code> with the specified detail
     * message.
     *
     * @param message
     *            the detail message.
     */
    public HDF5Exception(String message) {
        super();
        detailMessage = message;
    }

    /**
     * Returns the detail message of this exception
     *
     * @return the detail message or <code>null</code> if this object does not
     *         have a detail message.
     */
    @Override
    public String getMessage() {
        return detailMessage;
    }
}
