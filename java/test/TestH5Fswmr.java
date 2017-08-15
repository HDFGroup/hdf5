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

package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Fswmr {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "testswmr.h5";

    long H5fid = -1;
    long H5fapl = -1;
    long H5fcpl = -1;

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    @Before
    public void createH5file()
            throws HDF5LibraryException, NullPointerException {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        H5fapl = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);
        H5fcpl = H5.H5Pcreate(HDF5Constants.H5P_FILE_CREATE);
        H5.H5Pset_libver_bounds(H5fapl, HDF5Constants.H5F_LIBVER_LATEST, HDF5Constants.H5F_LIBVER_LATEST);

        H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, H5fcpl, H5fapl);
        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5fapl > 0) {
            try {H5.H5Pclose(H5fapl);} catch (Exception ex) {}
            H5fapl = -1;
        }
        if (H5fcpl > 0) {
            try {H5.H5Pclose(H5fcpl);} catch (Exception ex) {}
            H5fcpl = -1;
        }
       if (H5fid > 0) {
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
            H5fid = -1;
        }
        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test
    public void testH5Fstart_swmr_write() {
        try {
            H5.H5Fstart_swmr_write(H5fid);
        }
        catch (Throwable err) {
            fail("H5.H5Fstart_swmr_write: " + err);
        }
    }

    @Test
    public void testH5Fswmr_read_attempts() {
        long read_attempts = 0;

        try {
            read_attempts = H5.H5Pget_metadata_read_attempts(H5fapl);
        }
        catch (Throwable err) {
            fail("H5.testH5Fswmr_read_attempts: " + err);
        }
        assertTrue(read_attempts == 1);

        try {
            H5.H5Pset_metadata_read_attempts(H5fapl, 20);
        }
        catch (Throwable err) {
            fail("H5.testH5Fswmr_read_attempts: " + err);
        }
        try {
            read_attempts = H5.H5Pget_metadata_read_attempts(H5fapl);
        }
        catch (Throwable err) {
            fail("H5.testH5Fswmr_read_attempts: " + err);
        }
        assertTrue(read_attempts == 20);
    }
}
