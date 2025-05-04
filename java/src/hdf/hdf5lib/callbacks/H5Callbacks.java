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

package hdf.hdf5lib.callbacks;

/**
 * \page CALLBACKS HDF5 Java H5Callbacks Interface
 * All callback definitions must derive from this interface.  Any
 * derived interfaces must define a single public method  named "callback".
 * You are responsible for deregistering your callback (if necessary)
 * in its Object finalize method.  If native code attempts to call
 * a callback which has been GC'd, you will likely crash the VM.  If
 * there is no method to deregister the callback (e.g. <code>atexit</code>
 * in the C library), you must ensure that you always keep a live reference
 * to the callback object.
 *
 * A callback should generally never throw an exception, since it doesn't
 * necessarily have an encompassing Java environment to catch it.  Any
 * exceptions thrown will be passed to the default callback exception
 * handler.
 *
 * @defgroup JCALLBK HDF5 Library Java H5Callbacks
 */
public interface H5Callbacks {
}
