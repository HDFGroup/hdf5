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

public class HDF5ReferenceException extends HDF5LibraryException {
    /**
     * Constructs an <code>HDF5ReferenceException</code> with no specified
     * detail message.
     */
    public HDF5ReferenceException() {
        super();
    }

    /**
     * Constructs an <code>HDF5ReferenceException</code> with the specified
     * detail message.
     *
     * @param s
     *            the detail message.
     */
    public HDF5ReferenceException(String s) {
        super(s);
    }
}
