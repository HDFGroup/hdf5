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

package hdf.hdf5lib.exceptions;

/**
 * The class HDF5LibraryException returns errors raised by the HDF5 library.
 * <p>
 * This sub-class represents HDF-5 major error code <b>H5E_EFL</b>
 */

public class HDF5ExternalFileListException extends HDF5LibraryException {
    /**
     * Constructs an <code>HDF5ExternalFileListException</code> with no
     * specified detail message.
     */
    public HDF5ExternalFileListException() {
        super();
    }

    /**
     * Constructs an <code>HDF5ExternalFileListException</code> with the
     * specified detail message.
     *
     * @param s
     *            the detail message.
     */
    public HDF5ExternalFileListException(String s) {
        super(s);
    }
}
