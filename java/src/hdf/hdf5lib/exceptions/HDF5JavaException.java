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
 * @page ERRORSJAVA Java Wrapper Errors and Exceptions
 * The class HDF5JavaException returns errors from the Java wrapper of theHDF5
 * library.
 * <p>
 * These errors include Java configuration errors, security violations, and
 * resource exhaustion.
 *
 * @defgroup JERRJAVA HDF5 Library Java Exception Interface
 */
public class HDF5JavaException extends HDF5Exception {
    /**
     * @ingroup JERRJAVA
     *
     * Constructs an <code>HDF5JavaException</code> with no specified detail
     * message.
     */
    public HDF5JavaException() { super(); }

    /**
     * @ingroup JERRJAVA
     *
     * Constructs an <code>HDF5JavaException</code> with the specified detail
     * message.
     *
     * @param s
     *            the detail message.
     */
    public HDF5JavaException(String s) { super(s); }
}
