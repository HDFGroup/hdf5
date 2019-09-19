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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5FD_hdfs_fapl_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Pfaplhdfs {
    @Rule public TestName testname = new TestName();

    long fapl_id = -1;
    long plapl_id = -1;
    long dapl_id = -1;
    long plist_id = -1;
    long btplist_id = -1;

    @Before
    public void createFileAccess() throws NullPointerException, HDF5Exception
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());

        try {
            fapl_id = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createFileAccess: " + err);
        }
        assertTrue(fapl_id > 0);
        try {
            plapl_id = H5.H5Pcreate(HDF5Constants.H5P_LINK_ACCESS);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createFileAccess: " + err);
        }
        assertTrue(plapl_id > 0);
        try {
            plist_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_XFER);
            btplist_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_XFER);
            dapl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_ACCESS);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createFileAccess: " + err);
        }
        assertTrue(plist_id > 0);
        assertTrue(btplist_id > 0);
        assertTrue(dapl_id > 0);
    }

    @After
    public void deleteFileAccess() throws HDF5LibraryException
    {
        if (fapl_id > 0)
            try {H5.H5Pclose(fapl_id);} catch (Exception ex) {}
        if (plapl_id > 0)
            try {H5.H5Pclose(plapl_id);} catch (Exception ex) {}
        if (dapl_id > 0)
            try {H5.H5Pclose(dapl_id);} catch (Exception ex) {}
        if (plist_id > 0)
            try {H5.H5Pclose(plist_id);} catch (Exception ex) {}
        if (btplist_id > 0)
            try {H5.H5Pclose(btplist_id);} catch (Exception ex) {}
        System.out.println();
    }

    @Test
    public void testHDFS_fapl() throws Exception
    {
        if (HDF5Constants.H5FD_HDFS < 0)
            throw new HDF5LibraryException("skip");

        String nodename = "blues";
        int    nodeport = 12345;
        String username = "sparticus";
        String kerbcache = "/dev/null";
        int    streamsize = 1024;

        final H5FD_hdfs_fapl_t config = new H5FD_hdfs_fapl_t(nodename, nodeport, username, kerbcache, streamsize);
        assertTrue("setting fapl should succeed", -1 < H5.H5Pset_fapl_hdfs(fapl_id, config));

        assertEquals("driver types should match", HDF5Constants.H5FD_HDFS, H5.H5Pget_driver(fapl_id));

        H5FD_hdfs_fapl_t copy = H5.H5Pget_fapl_hdfs(fapl_id);
        assertEquals("fapl contents should match", new H5FD_hdfs_fapl_t(nodename, nodeport, username, kerbcache, streamsize), copy);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pget_fapl_hdfs_invalid_fapl_id() throws Exception
    {
        if (HDF5Constants.H5FD_HDFS < 0)
            throw new HDF5LibraryException("skip");
        H5FD_hdfs_fapl_t fails = H5.H5Pget_fapl_hdfs(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pget_fapl_hdfs_fapl_id_of_wrong_driver_type() throws Exception
    {
        if (HDF5Constants.H5FD_HDFS < 0)
            throw new HDF5LibraryException("skip");
        if (HDF5Constants.H5FD_SEC2 < 0 )
            throw new HDF5LibraryException("skip");
            /* TODO: for now, test against a sec2 fapl only */

        H5.H5Pset_fapl_sec2(fapl_id);
        assertEquals("fapl_id was not set properly", HDF5Constants.H5FD_SEC2, H5.H5Pget_driver(fapl_id));
        H5FD_hdfs_fapl_t fails = H5.H5Pget_fapl_hdfs(fapl_id);
    }

}
