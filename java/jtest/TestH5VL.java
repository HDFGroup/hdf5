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

package test;

import static org.hdfgroup.javahdf5.hdf5_h.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;

import org.hdfgroup.javahdf5.*;
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
            int is_registered;

            is_registered = H5VLis_connector_registered_by_name(H5VL_NATIVE_NAME());
            assertTrue("H5VLis_connector_registered_by_name H5VL_NATIVE_NAME", is_registered == 1);

            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("FAKE_VOL_NAME");
                is_registered              = H5VLis_connector_registered_by_name(name_segment);
            }
            assertFalse("H5VLis_connector_registered_by_name FAKE_VOL_NAME", is_registered == 1);

            is_registered = H5VLis_connector_registered_by_value(H5VL_NATIVE_VALUE());
            assertTrue("H5VLis_connector_registered_by_value H5VL_NATIVE_VALUE", is_registered == 1);
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
        long H5fid     = H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment filename_segment = arena.allocateFrom(H5_FILE);
            H5fid = H5Fcreate(filename_segment, H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }

        try {
            long native_id = H5VLget_connector_id(H5fid);
            assertTrue("H5VLget_connector_id", native_id >= 0);

            /*
             * If HDF5_VOL_CONNECTOR is set, this might not be the
             * native connector. Only check for the native connector
             * if this isn't set.
             */
            String connector = System.getenv("HDF5_VOL_CONNECTOR");
            if (connector == null)
                assertEquals(H5VL_NATIVE_g(), native_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5VLget_connector_id " + err);
        }
        finally {
            if (H5fid > 0) {
                try {
                    H5Fclose(H5fid);
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
            long native_id = H5VLget_connector_id_by_name(H5VL_NATIVE_NAME());
            assertTrue("H5VLget_connector_id_by_name H5VL_NATIVE_NAME", native_id >= 0);
            assertEquals(H5VL_NATIVE_g(), native_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5VLget_connector_id_by_name " + err);
        }
    }

    @Test
    public void testH5VLget_connector_id_by_value()
    {
        try {
            long native_id = H5VLget_connector_id_by_value(H5VL_NATIVE_VALUE());
            assertTrue("H5VLget_connector_id_by_value H5VL_NATIVE_VALUE", native_id >= 0);
            assertEquals(H5VL_NATIVE_g(), native_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5VLget_connector_id_by_value " + err);
        }
    }

    @Test
    public void testH5VLget_connector_name()
    {
        String H5_FILE = "testFvl.h5";
        long H5fid     = H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment filename_segment = arena.allocateFrom(H5_FILE);
            H5fid = H5Fcreate(filename_segment, H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
        H5Fflush(H5fid, H5F_SCOPE_LOCAL());

        try {
            long buf_size      = H5VLget_connector_name(H5fid, null, 0);
            String native_name = null;
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment path_name_segment = arena.allocate(buf_size + 1);
                H5VLget_connector_name(H5fid, path_name_segment, buf_size);
                native_name = path_name_segment.getString(0);
            }

            /*
             * If HDF5_VOL_CONNECTOR is set, this might not be the
             * native connector. Only check for the native connector
             * if this isn't set.
             */
            String connector = System.getenv("HDF5_VOL_CONNECTOR");
            if (connector == null)
                assertTrue("H5VLget_connector_name H5VL_NATIVE",
                           native_name.compareToIgnoreCase(H5VL_NATIVE_NAME().getString(0)) == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5VLget_connector_name " + err);
        }
        finally {
            if (H5fid > 0) {
                try {
                    H5Fclose(H5fid);
                }
                catch (Exception ex) {
                }
            }
            _deleteFile(H5_FILE);
        }
    }

    @Test
    public void testH5VLclose_NegativeID() throws Throwable
    {
        assertTrue("testH5VLclose_NegativeID", H5VLclose(-1) < 0);
    }

    @Test
    public void testH5VLunregister_connector_NegativeID() throws Throwable
    {
        assertTrue("testH5VLclose_NegativeID", H5VLunregister_connector(-1) < 0);
    }
}
