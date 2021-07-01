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

package test;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5JavaException;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

/**
 * This class contains testing routines for the Java interface which are
 * public, but are not meant for general use.
 **/
public class H5TestUtils {

    private final static org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(H5TestUtils.class);

    private static boolean isLibraryLoaded = false;

    static {
        loadH5TestLib();
    }

    /**
     *  load native testing library
     */
    public static void loadH5TestLib() {
        String s_libraryName = null;
        String mappedName = null;

        // Make sure that the library is loaded only once
        if (isLibraryLoaded)
            return;

        try {
            s_libraryName = "hdf5_java_test";
            mappedName = System.mapLibraryName(s_libraryName);
            System.loadLibrary("hdf5_java_test");
            isLibraryLoaded = true;
        }
        catch (Throwable err) {
            err.printStackTrace();
            isLibraryLoaded = false;
        }
        finally {
            log.info("HDF5 Java test library: " + s_libraryName);
            log.debug(" resolved to: " + mappedName + "; ");
            log.info((isLibraryLoaded ? "" : " NOT") + " successfully loaded from java.library.path");
        }

        /* Important! Disable error output to C stdout */
        if (!log.isDebugEnabled())
            H5.H5error_off();
    }

    /**
     * H5VLfapl_is_native queries if a FAPL will use the native VOL connector.
     *
     * @param fapl_id
     *            The ID of the FAPL to query.
     * @return true if fapl_id uses the native VOL connector.
     *
     **/
    public synchronized static native boolean H5VLfapl_is_native(long fapl_id) throws HDF5LibraryException;

}
