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
 * @page ERRORS_UG Errors and Exceptions
 * <p>
 * The package exceptions contains error classes for the Java HDF5 Interface.
 * <p>
 * There are two sub-classes of exceptions defined:
 * <ol>
 * <li>
 * HDF5LibraryException -- errors raised the HDF5 library code
 * <li>
 * HDF5JavaException -- errors raised the HDF5 Java wrapper code
 * </ol>
 * <p>
 * The HDF5LibraryException is the base class for the classes that represent specific error conditions.
 * In particular, HDF5LibraryException has a sub-class for each major
 * error code returned by the HDF5 library.
 *
 */
package hdf.hdf5lib.exceptions;
