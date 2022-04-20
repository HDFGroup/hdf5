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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
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

public class TestH5VL {
    @Rule
    public TestName testname = new TestName();

    private final void _deleteFile(String filename)
    {
        File file = new File(filename);

        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
            }
        }
    }

    @Before
    public void checkOpenIDs()
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName()
    {
        System.out.println();
    }

    @Test
    public void testH5VLnative_init()
    {
        try {
            boolean is_registered;

            is_registered = H5.H5VLis_connector_registered_by_name(HDF5Constants.H5VL_NATIVE_NAME);
            assertTrue("H5.H5VLis_connector_registered_by_name H5VL_NATIVE_NAME", is_registered);

            is_registered = H5.H5VLis_connector_registered_by_name("FAKE_VOL_NAME");
            assertFalse("H5.H5VLis_connector_registered_by_name FAKE_VOL_NAME", is_registered);

            is_registered = H5.H5VLis_connector_registered_by_value(HDF5Constants.H5VL_NATIVE_VALUE);
            assertTrue("H5.H5VLis_connector_registered_by_value H5VL_NATIVE_VALUE", is_registered);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5VLnative_init(): " + err);
        }
    }

    @Test
    public void testH5VLget_connector_id()
    {
        String H5_FILE = "testFvl.h5";

        long H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                  HDF5Constants.H5P_DEFAULT);

        try {
            long native_id = H5.H5VLget_connector_id(H5fid);
            assertTrue("H5.H5VLget_connector_id", native_id >= 0);

            /*
             * If HDF5_VOL_CONNECTOR is set, this might not be the
             * native connector. Only check for the native connector
             * if this isn't set.
             */
            String connector = System.getenv("HDF5_VOL_CONNECTOR");
            if (connector == null)
                assertEquals(HDF5Constants.H5VL_NATIVE, native_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5VLget_connector_id " + err);
        }
        finally {
            if (H5fid > 0) {
                try {
                    H5.H5Fclose(H5fid);
                }
                catch (Exception ex) {
                }
            }
            _deleteFile(H5_FILE);
        }
    }

    @Test
    public void testH5VLget_connector_id_by_name()
    {
        try {
            long native_id = H5.H5VLget_connector_id_by_name(HDF5Constants.H5VL_NATIVE_NAME);
            assertTrue("H5.H5VLget_connector_id_by_name H5VL_NATIVE_NAME", native_id >= 0);
            assertEquals(HDF5Constants.H5VL_NATIVE, native_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5VLget_connector_id_by_name " + err);
        }
    }

    @Test
    public void testH5VLget_connector_id_by_value()
    {
        try {
            long native_id = H5.H5VLget_connector_id_by_value(HDF5Constants.H5VL_NATIVE_VALUE);
            assertTrue("H5.H5VLget_connector_id_by_value H5VL_NATIVE_VALUE", native_id >= 0);
            assertEquals(HDF5Constants.H5VL_NATIVE, native_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5VLget_connector_id_by_value " + err);
        }
    }

    @Test
    public void testH5VLget_connector_name()
    {
        String H5_FILE = "testFvl.h5";

        long H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                  HDF5Constants.H5P_DEFAULT);
        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

        try {
            String native_name = H5.H5VLget_connector_name(H5fid);

            /*
             * If HDF5_VOL_CONNECTOR is set, this might not be the
             * native connector. Only check for the native connector
             * if this isn't set.
             */
            String connector = System.getenv("HDF5_VOL_CONNECTOR");
            if (connector == null)
                assertTrue("H5.H5VLget_connector_name H5VL_NATIVE",
                           native_name.compareToIgnoreCase(HDF5Constants.H5VL_NATIVE_NAME) == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5VLget_connector_name " + err);
        }
        finally {
            if (H5fid > 0) {
                try {
                    H5.H5Fclose(H5fid);
                }
                catch (Exception ex) {
                }
            }
            _deleteFile(H5_FILE);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5VLclose_NegativeID() throws Throwable
    {
        H5.H5VLclose(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5VLunregister_connector_NegativeID() throws Throwable
    {
        H5.H5VLunregister_connector(-1);
    }
}
