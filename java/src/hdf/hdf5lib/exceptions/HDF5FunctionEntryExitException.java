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

package hdf.hdf5lib.exceptions;

/**
 * The class HDF5LibraryException returns errors raised by the HDF5 library.
 * <p>
 * This sub-class represents HDF5 major error code <b>H5E_FUNC</b>
 */

public class HDF5FunctionEntryExitException extends HDF5LibraryException {
    /**
     * @ingroup JERRLIB
     *
     * Constructs an <code>HDF5FunctionEntryExitException</code> with no
     * specified detail message.
     */
    public HDF5FunctionEntryExitException() { super(); }

    /**
     * @ingroup JERRLIB
     *
     * Constructs an <code>HDF5FunctionEntryExitException</code> with the
     * specified detail message.
     *
     * @param s
     *            the detail message.
     */
    public HDF5FunctionEntryExitException(String s) { super(s); }
}
