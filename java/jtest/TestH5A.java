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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.hdfgroup.javahdf5.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5A {
    @Rule
    public TestName testname            = new TestName();
    private static final String H5_FILE = "testA.h5";
    private static final int DIM_X      = 4;
    private static final int DIM_Y      = 6;
    long H5fid                          = H5I_INVALID_HID();
    long H5dsid                         = H5I_INVALID_HID();
    long H5atid                         = H5I_INVALID_HID();
    long H5did                          = H5I_INVALID_HID();
    long[] H5dims                       = {DIM_X, DIM_Y};
    long type_id                        = H5I_INVALID_HID();
    long space_id                       = H5I_INVALID_HID();
    long lapl_id                        = H5I_INVALID_HID();
    long aapl_id                        = H5I_INVALID_HID();

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

    private final long _createDataset(long fid, long dsid, String name, long dapl)
    {
        long did = H5I_INVALID_HID();
        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom(name);
                did = H5Dcreate2(fid, name_segment, H5T_STD_I32BE_g(), dsid, H5P_DEFAULT(), H5P_DEFAULT(),
                                 dapl);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("Arena: " + err);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Dcreate: " + err);
        }
        assertTrue("TestH5A._createDataset: ", did > 0);

        return did;
    }

    @Before
    public void createH5file() throws NullPointerException, HDF5Exception
    {
        System.out.print(testname.getMethodName());

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment filename_segment = arena.allocateFrom(H5_FILE);
                H5fid = H5Fcreate(filename_segment, H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("Arena: " + err);
            }
            assertTrue("TestH5A.createH5file: H5Fcreate: ", H5fid > 0);
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the dims bytes
                MemorySegment H5dims_segment = MemorySegment.ofArray(H5dims);
                H5dsid                       = H5Screate_simple(2, H5dims_segment, null);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("Arena: " + err);
            }
            assertTrue("TestH5A.createH5file: H5Screate_simple: ", H5dsid > 0);
            H5did = _createDataset(H5fid, H5dsid, "dset", H5P_DEFAULT());
            assertTrue("TestH5A.createH5file: _createDataset: ", H5did > 0);
            space_id = H5Screate(H5S_NULL());
            assertTrue(space_id > 0);
            lapl_id = H5Pcreate(H5P_CLS_LINK_ACCESS_ID_g());
            assertTrue(lapl_id > 0);
            aapl_id = H5Pcreate(H5P_CLS_ATTRIBUTE_ACCESS_ID_g());
            assertTrue(aapl_id > 0);
            type_id = H5Tenum_create(H5T_STD_I32LE_g());
            assertTrue(type_id > 0);
            int status = -1;
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the enum bytes
                MemorySegment name_segment = arena.allocateFrom("test");
                MemorySegment enum_segment = arena.allocate(ValueLayout.JAVA_INT);
                // Set the int value
                enum_segment.set(ValueLayout.JAVA_INT, 0, 1);
                status = H5Tenum_insert(type_id, name_segment, enum_segment);
            }
            assertTrue(status >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5A.createH5file: " + err);
        }

        H5Fflush(H5fid, H5F_SCOPE_LOCAL());
    }

    @After
    public void deleteH5file() throws HDF5LibraryException
    {
        if (H5dsid > 0)
            try {
                H5Sclose(H5dsid);
            }
            catch (Exception ex) {
            }
        if (H5did > 0)
            try {
                H5Dclose(H5did);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }
        if (H5atid > 0)
            try {
                H5Tclose(H5atid);
            }
            catch (Exception ex) {
            }

        if (type_id > 0)
            try {
                H5Tclose(type_id);
            }
            catch (Exception ex) {
            }
        if (space_id > 0)
            try {
                H5Sclose(space_id);
            }
            catch (Exception ex) {
            }
        if (lapl_id > 0)
            try {
                H5Pclose(lapl_id);
            }
            catch (Exception ex) {
            }
        if (aapl_id > 0)
            try {
                H5Pclose(aapl_id);
            }
            catch (Exception ex) {
            }

        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test
    public void testH5Acreate2()
    {
        long attr_id = H5I_INVALID_HID();
        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("dset");
                attr_id = H5Acreate2(H5did, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }
            assertTrue("testH5Acreate2", attr_id >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Acreate2: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    public void testH5Acreate2_invalidobject() throws Throwable
    {
        long attr_id = H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom("dset");
            attr_id = H5Acreate2(H5dsid, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
        }
        assertTrue("H5Acreate2", attr_id < 0);
    }

    public void testH5Acreate2_nullname() throws Throwable
    {
        long attr_id = H5I_INVALID_HID();

        attr_id = H5Acreate2(H5did, null, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
        assertTrue("H5Acreate2", attr_id < 0);
    }

    @Test
    public void testH5Aopen()
    {
        String attr_name  = "dset";
        long attribute_id = H5I_INVALID_HID();
        long attr_id      = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom(attr_name);
                attr_id = H5Acreate2(H5did, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }

            // Opening the existing attribute, attr_name(Created by H5ACreate2)
            // attached to an object identifier.
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment filename_segment = arena.allocateFrom(attr_name);
                attribute_id                   = H5Aopen(H5did, filename_segment, H5P_DEFAULT());
            }
            assertTrue("testH5Aopen: H5Aopen", attribute_id >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aopen: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    public void testH5Aopen_invalidname() throws Throwable
    {
        long aid = H5I_INVALID_HID();
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment filename_segment = arena.allocateFrom("attr_name");
            aid                            = H5Aopen(H5did, filename_segment, H5P_DEFAULT());
        }
        assertTrue("H5Aopen", aid < 0);
    }

    @Test
    public void testH5Aopen_by_idx()
    {
        long loc_id       = H5did;
        String obj_name   = ".";
        int idx_type      = H5_INDEX_CRT_ORDER();
        int order         = H5_ITER_INC();
        long n            = 0;
        long attr_id      = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("file");
                attr_id = H5Acreate2(H5did, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }

            // Opening the existing attribute, obj_name(Created by H5ACreate2)
            // by index, attached to an object identifier.
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment obj_name_segment = arena.allocateFrom(".");
                attribute_id = H5Aopen_by_idx(loc_id, obj_name_segment, H5_INDEX_CRT_ORDER(), H5_ITER_INC(),
                                              0, aapl_id, lapl_id);
            }
            assertTrue("testH5Aopen_by_idx: H5Aopen_by_idx", attribute_id >= 0);

            // Negative test- Error should be thrown when H5Aopen_by_idx is
            // called
            // with n=5 and we do not have 5 attributes created.
            try {
                n = 5;
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment obj_name_segment = arena.allocateFrom(obj_name);
                    attribute_id =
                        H5Aopen_by_idx(loc_id, obj_name_segment, idx_type, order, n, aapl_id, lapl_id);
                }
                assertTrue("H5Aopen_by_idx did not fail", attribute_id < 0);
                // fail("Negative Test Failed:- Error not Thrown when n is invalid.");
            }
            catch (AssertionError err) {
                fail("H5Aopen_by_idx: " + err);
            }
            catch (HDF5LibraryException err) {
            }

            // Negative test- Error should be thrown when H5Aopen_by_idx is
            // called
            // with an invalid object name(which hasn't been created).
            try {
                n        = 0;
                obj_name = "file";
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment obj_name_segment = arena.allocateFrom(obj_name);
                    attribute_id =
                        H5Aopen_by_idx(loc_id, obj_name_segment, idx_type, order, n, aapl_id, lapl_id);
                }
                assertTrue("H5Aopen_by_idx did not fail", attribute_id < 0);
                // fail("Negative Test Failed:- Error not Thrown when attribute name is invalid.");
            }
            catch (AssertionError err) {
                fail("H5Aopen_by_idx: " + err);
            }
            catch (HDF5LibraryException err) {
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aopen_by_idx: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Acreate_by_name()
    {
        String obj_name   = ".";
        String attr_name  = "DATASET";
        long attribute_id = H5I_INVALID_HID();
        boolean bool_val  = false;

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
                MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                attribute_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id,
                                                 space_id, H5P_DEFAULT(), aapl_id, lapl_id);
            }
            assertTrue("testH5Acreate_by_name: H5Acreate_by_name", attribute_id >= 0);

            // Check if the name of attribute attached to the object specified
            // by loc_id and obj_name exists.It should be true.
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
                MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                bool_val = H5Aexists_by_name(H5fid, obj_name_segment, attr_name_segment, lapl_id);
            }
            assertTrue("testH5Acreate_by_name: H5Aexists_by_name", bool_val == true);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Acreate_by_name " + err);
        }
        finally {
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Arename() throws Throwable, HDF5LibraryException, NullPointerException
    {
        long loc_id          = H5fid;
        String old_attr_name = "old";
        String new_attr_name = "new";
        long attr_id         = H5I_INVALID_HID();
        int ret_val          = -1;
        boolean bool_val     = false;

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom(old_attr_name);
                attr_id = H5Acreate2(loc_id, name_segment, type_id, space_id, H5P_DEFAULT(), aapl_id);
            }

            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment old_name_segment = arena.allocateFrom(old_attr_name);
                MemorySegment new_name_segment = arena.allocateFrom(new_attr_name);
                ret_val                        = H5Arename(loc_id, old_name_segment, new_name_segment);
            }

            // Check the return value.It should be non negative.
            assertTrue("testH5Arename: H5Arename", ret_val >= 0);

            // Check if the new name of attribute attached to the object
            // specified by loc_id and obj_name exists.It should be true.
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment new_name_segment = arena.allocateFrom(new_attr_name);
                bool_val                       = H5Aexists(loc_id, new_name_segment);
            }
            assertTrue("testH5Arename: H5Aexists", bool_val == true);

            // Check if the old name of attribute attached to the object
            // specified by loc_id and obj_name exists. It should equal false.
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment old_name_segment = arena.allocateFrom(old_attr_name);
                bool_val                       = H5Aexists(loc_id, old_name_segment);
            }
            assertEquals(bool_val, false);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Arename " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Arename_by_name()
    {
        long loc_id          = H5fid;
        String obj_name      = ".";
        String old_attr_name = "old";
        String new_attr_name = "new";
        long attr_id         = H5I_INVALID_HID();
        int ret_val          = -1;
        boolean bool_val     = false;

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom(old_attr_name);
                MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                attr_id = H5Acreate_by_name(loc_id, obj_name_segment, attr_name_segment, type_id, space_id,
                                            H5P_DEFAULT(), aapl_id, lapl_id);
            }

            ret_val = H5Arename_by_name(loc_id, obj_name, old_attr_name, new_attr_name, lapl_id);

            // Check the return value.It should be non negative.
            assertTrue("testH5Arename_by_name: H5Arename_by_name", ret_val >= 0);

            // Check if the new name of attribute attached to the object
            // specified by loc_id and obj_name exists.It should be true.
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment new_name_segment = arena.allocateFrom(new_attr_name);
                MemorySegment obj_name_segment = arena.allocateFrom(obj_name);
                bool_val = H5Aexists_by_name(loc_id, obj_name_segment, new_name_segment, lapl_id);
            }
            assertTrue("testH5Arename_by_name: H5Aexists_by_name", bool_val == true);

            // Check if the old name of attribute attached to the object
            // specified by loc_id and obj_name exists. It should equal false.
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment old_name_segment = arena.allocateFrom(old_attr_name);
                MemorySegment obj_name_segment = arena.allocateFrom(obj_name);
                bool_val = H5Aexists_by_name(loc_id, obj_name_segment, old_name_segment, lapl_id);
            }
            assertEquals(bool_val, false);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Arename_by_name " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_name()
    {
        String obj_name   = ".";
        String attr_name  = "DATASET1";
        String ret_name   = null;
        long attribute_id = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
                MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                attribute_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id,
                                                 space_id, H5P_DEFAULT(), aapl_id, lapl_id);
            }
            assertTrue("testH5Aget_name: H5Acreate_by_name ", attribute_id > 0);
            ret_name = H5Aget_name(attribute_id);
            assertEquals(ret_name, attr_name);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aget_name " + err);
        }
        finally {
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_name_by_idx()
    {
        long loc_id      = H5fid;
        String obj_name  = ".";
        String attr_name = "DATASET1", attr2_name = "DATASET2";
        String ret_name = null;
        int name_size   = 1024;
        int idx_type    = H5_INDEX_NAME();
        int order       = H5_ITER_INC();
        int n           = 0;
        long attr1_id   = H5I_INVALID_HID();
        long attr2_id   = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
                MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                attr1_id = H5Acreate_by_name(loc_id, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom(attr2_name);
                MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                attr2_id = H5Acreate_by_name(loc_id, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }

            // getting the 1st attribute name(n=0).
            H5Aget_name_by_idx(loc_id, obj_name, idx_type, order, n, ret_name, name_size, lapl_id);
            assertFalse("H5Aget_name_by_idx ", ret_name == null);
            assertEquals(ret_name, attr_name);

            // getting the second attribute name(n=1)
            ret_name = H5Aget_name_by_idx(loc_id, obj_name, idx_type, order, 1, ret_name, name_size, lapl_id);
            assertFalse("H5Aget_name_by_idx ", ret_name == null);
            assertEquals(ret_name, attr2_name);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aget_name_by_idx " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_storage_size()
    {
        long attr_id   = H5I_INVALID_HID();
        long attr_size = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("dset");
                attr_id = H5Acreate2(loc_id, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }

            attr_size = H5Aget_storage_size(attr_id);
            assertTrue("The size of attribute is :", attr_size == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aget_storage_size: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_info()
    {
        H5A_info_t attr_info = null;
        long attribute_id    = H5I_INVALID_HID();
        long attr_id         = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("dset");
                attr_id = H5Acreate2(H5did, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("dset");
                attribute_id               = H5Aopen(H5did, name_segment, H5P_DEFAULT());
            }
            // Calling H5Aget_info with attribute_id returned from H5Aopen.
            attr_info = H5Aget_info(attribute_id);
            assertFalse("H5Aget_info ", attr_info == null);
            assertTrue("Corder_Valid should be false", attr_info.corder_valid == false);
            assertTrue("Character set used for attribute name", attr_info.cset == H5T_CSET_ASCII());
            assertTrue("Corder ", attr_info.corder == 0);
            assertEquals(attr_info.data_size, H5Aget_storage_size(attr_id));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aget_info: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_info1()
    {
        H5A_info_t attr_info = null;
        long attribute_id    = H5I_INVALID_HID();
        long attr_id         = H5I_INVALID_HID();
        int order            = H5_ITER_INC();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom(".");
                attr_id = H5Acreate2(H5did, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom(".");
                attribute_id = H5Aopen_by_idx(H5did, name_segment, H5_INDEX_CRT_ORDER(), order, 0,
                                              H5P_DEFAULT(), lapl_id);
            }
            // Calling H5Aget_info with attribute_id returned from
            // H5Aopen_by_idx.
            attr_info = H5Aget_info(attribute_id);

            assertFalse("H5Aget_info ", attr_info == null);
            assertTrue("Corder_Valid should be true", attr_info.corder_valid == true);
            assertTrue("Character set", attr_info.cset == H5T_CSET_ASCII());
            assertTrue("Corder ", attr_info.corder == 0);
            assertEquals(attr_info.data_size, H5Aget_storage_size(attr_id));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aget_info1: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_info_by_idx()
    {
        long attr_id  = H5I_INVALID_HID();
        long attr2_id = H5I_INVALID_HID();
        ;
        H5A_info_t attr_info = null;

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("dset1");
                attr_id = H5Acreate2(H5did, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("dataset2");
                attr2_id = H5Acreate2(H5did, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }

            // Verify info for 1st attribute, in increasing creation order
            attr_info = H5Aget_info_by_idx(H5did, ".", H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 0, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder ", attr_info.corder ==
                                      0); // should equal 0 as this is the order of 1st attribute created.
            assertEquals(attr_info.data_size, H5Aget_storage_size(attr_id));

            // Verify info for 2nd attribute, in increasing creation order
            attr_info = H5Aget_info_by_idx(H5did, ".", H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 1, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder == 1);
            assertEquals(attr_info.data_size, H5Aget_storage_size(attr2_id));

            // verify info for 2nd attribute, in decreasing creation order
            attr_info = H5Aget_info_by_idx(H5did, ".", H5_INDEX_CRT_ORDER(), H5_ITER_DEC(), 0, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder ==
                                     1); // should equal 1 as this is the order of 2nd attribute created.

            // verify info for 1st attribute, in decreasing creation order
            attr_info = H5Aget_info_by_idx(H5did, ".", H5_INDEX_CRT_ORDER(), H5_ITER_DEC(), 1, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder ==
                                     0); // should equal 0 as this is the order of 1st attribute created.

            // verify info for 1st attribute, in increasing name order
            attr_info = H5Aget_info_by_idx(H5did, ".", H5_INDEX_NAME(), H5_ITER_INC(), 1, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder ==
                                     0); // should equal 0 as this is the order of 1st attribute created.

            // verify info for 2nd attribute, in decreasing name order
            attr_info = H5Aget_info_by_idx(H5did, ".", H5_INDEX_NAME(), H5_ITER_DEC(), 1, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder ==
                                     1); // should equal 1 as this is the order of 2nd attribute created.
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aget_info_by_idx:" + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_info_by_name()
    {
        long attr_id         = H5I_INVALID_HID();
        H5A_info_t attr_info = null;
        String obj_name      = ".";
        String attr_name     = "DATASET";

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
                MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                attr_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                            H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            attr_info = H5Aget_info_by_name(H5fid, obj_name, attr_name, lapl_id);
            assertNotNull(attr_info);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aget_info_by_name:" + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Adelete_by_name()
    {
        long attr_id     = H5I_INVALID_HID();
        int ret_val      = -1;
        boolean bool_val = false;
        boolean exists   = false;

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("DATASET");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                            H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("DATASET");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                ret_val = H5Adelete_by_name(H5fid, obj_name_segment, attr_name_segment, lapl_id);
            }
            assertTrue("H5Adelete_by_name", ret_val >= 0);

            // Check if the Attribute still exists.
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment new_name_segment = arena.allocateFrom("DATASET");
                MemorySegment obj_name_segment = arena.allocateFrom(".");
                bool_val = H5Aexists_by_name(H5fid, obj_name_segment, new_name_segment, lapl_id);
            }
            assertFalse("testH5Adelete_by_name: H5Aexists_by_name", bool_val);
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("DATASET");
                exists                     = H5Aexists(H5fid, name_segment);
            }
            assertFalse("testH5Adelete_by_name: H5Aexists ", exists);

            // Negative test. Error thrown when we try to delete an attribute
            // that has already been deleted.
            try {
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment attr_name_segment = arena.allocateFrom("DATASET");
                    MemorySegment obj_name_segment  = arena.allocateFrom(".");
                    ret_val = H5Adelete_by_name(H5fid, obj_name_segment, attr_name_segment, lapl_id);
                }
                assertTrue("H5Adelete_by_name", ret_val < 0);
                // fail("Negative Test Failed: Error Not thrown.");
            }
            catch (AssertionError err) {
                fail("H5Adelete_by_name: " + err);
            }
            catch (HDF5LibraryException err) {
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Adelete_by_name " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aexists()
    {
        boolean exists    = false;
        long attr_id      = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("None");
                exists                     = H5Aexists(H5fid, name_segment);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aexists: " + err);
        }
        assertFalse("H5Aexists ", exists);

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("dset");
                attr_id = H5Acreate2(H5did, name_segment, type_id, space_id, H5P_DEFAULT(), H5P_DEFAULT());
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("dset");
                exists                     = H5Aexists(H5fid, name_segment);
            }
            assertTrue("H5Aexists ", exists);

            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attribute_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id,
                                                 space_id, H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("attribute");
                exists                     = H5Aexists(H5fid, name_segment);
            }
            assertTrue("H5Aexists ", exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aexists: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Adelete_by_idx_order()
    {
        boolean exists = false;
        long attr1_id  = H5I_INVALID_HID();
        long attr2_id  = H5I_INVALID_HID();
        long attr3_id  = H5I_INVALID_HID();
        long attr4_id  = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute1");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr1_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute2");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr2_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute3");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr3_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute4");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr4_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }

            H5Adelete_by_idx(H5fid, ".", H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 3, lapl_id);
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("attribute4");
                exists                     = H5Aexists(H5fid, name_segment);
            }
            assertFalse("H5Adelete_by_idx: H5Aexists", exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Adelete_by_idx: " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
            if (attr4_id > 0)
                try {
                    H5Aclose(attr4_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Adelete_by_idx_name1()
    {
        boolean exists = false;
        long attr1_id  = H5I_INVALID_HID();
        long attr2_id  = H5I_INVALID_HID();
        long attr3_id  = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute1");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr1_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute2");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr2_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute3");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr3_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            H5Adelete_by_idx(H5fid, ".", H5_INDEX_NAME(), H5_ITER_INC(), 2, lapl_id);
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("attribute3");
                exists                     = H5Aexists(H5fid, name_segment);
            }
            assertFalse("H5Adelete_by_idx: H5Aexists", exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Adelete_by_idx: " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Adelete_by_idx_name2()
    {
        boolean exists = false;
        long attr1_id  = H5I_INVALID_HID();
        long attr2_id  = H5I_INVALID_HID();
        long attr3_id  = H5I_INVALID_HID();
        long attr4_id  = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute1");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr1_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute2");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr2_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute3");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr3_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute4");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr4_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }

            H5Adelete_by_idx(H5fid, ".", H5_INDEX_NAME(), H5_ITER_DEC(), 3, lapl_id);
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom("attribute1");
                exists                     = H5Aexists(H5fid, name_segment);
            }
            assertFalse("H5Adelete_by_idx: H5Aexists", exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Adelete_by_idx: " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
            if (attr4_id > 0)
                try {
                    H5Aclose(attr4_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test(expected = NullPointerException.class)
    public void testH5Adelete_by_idx_null() throws Throwable
    {
        H5Adelete_by_idx(H5fid, null, H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 0, lapl_id);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Adelete_by_idx_invalidobject() throws Throwable
    {
        H5Adelete_by_idx(H5fid, "invalid", H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 0, lapl_id);
    }

    @Test
    public void testH5Aopen_by_name()
    {
        String obj_name   = ".";
        String attr_name  = "DATASET";
        long attribute_id = H5I_INVALID_HID();
        long aid          = H5I_INVALID_HID();

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
                MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                attribute_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id,
                                                 space_id, H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }

            // open Attribute by name
            if (attribute_id >= 0) {
                try {
                    try (Arena arena = Arena.ofConfined()) {
                        // Allocate a MemorySegment to hold the string bytes
                        MemorySegment attr_name_segment = arena.allocateFrom(attr_name);
                        MemorySegment obj_name_segment  = arena.allocateFrom(obj_name);
                        aid = H5Aopen_by_name(H5fid, obj_name_segment, attr_name_segment, H5P_DEFAULT(),
                                              lapl_id);
                    }
                    assertTrue("testH5Aopen_by_name: ", aid >= 0);
                }
                catch (Throwable err) {
                    err.printStackTrace();
                    fail("H5Aopen_by_name " + err);
                }
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Aopen_by_name " + err);
        }
        finally {
            if (aid > 0)
                try {
                    H5Aclose(aid);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Awrite_readVL()
    {
        String attr_name  = "VLdata";
        long attr_id      = H5I_INVALID_HID();
        long atype_id     = H5I_INVALID_HID();
        long aspace_id    = H5I_INVALID_HID();
        String[] str_data = {"Parting", "is such", "sweet", "sorrow."};
        long[] dims       = {str_data.length};
        long lsize        = 1;

        try {
            atype_id = H5Tcopy(H5T_C_S1());
            assertTrue("testH5Awrite_readVL.H5Tcopy: ", atype_id >= 0);
            H5Tset_size(atype_id, H5T_VARIABLE());
            assertTrue("testH5Awrite_readVL.H5Tis_variable_str", H5Tis_variable_str(atype_id));
        }
        catch (Exception err) {
            if (atype_id > 0)
                try {
                    H5Tclose(atype_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5Awrite_readVL: " + err);
        }

        try {
            aspace_id = H5Screate_simple(1, dims, null);
            assertTrue(aspace_id > 0);
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom(attr_name);
                attr_id = H5Acreate2(H5did, name_segment, atype_id, aspace_id, H5P_DEFAULT(), H5P_DEFAULT());
            }
            assertTrue("testH5Awrite_readVL: ", attr_id >= 0);

            H5AwriteVL(attr_id, atype_id, str_data);

            H5Fflush(H5fid, H5F_SCOPE_LOCAL());

            for (int j = 0; j < dims.length; j++) {
                lsize *= dims[j];
            }
            String[] strs = new String[(int)lsize];
            for (int j = 0; j < lsize; j++) {
                strs[j] = "";
            }
            try {
                H5AreadVL(attr_id, atype_id, strs);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5Awrite_readVL:", str_data[0].equals(strs[0]));
            assertTrue("testH5Awrite_readVL:", str_data[1].equals(strs[1]));
            assertTrue("testH5Awrite_readVL:", str_data[2].equals(strs[2]));
            assertTrue("testH5Awrite_readVL:", str_data[3].equals(strs[3]));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Awrite_readVL: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (aspace_id > 0)
                try {
                    H5Sclose(aspace_id);
                }
                catch (Exception ex) {
                }
            if (atype_id > 0)
                try {
                    H5Tclose(atype_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_create_plist()
    {
        String attr_name  = "DATASET1";
        int char_encoding = 0;
        long plist_id     = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();

        try {
            plist_id = H5Pcreate(H5P_CLS_ATTRIBUTE_CREATE_ID_g());
            assertTrue(plist_id > 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aget_create_plist: H5Pcreate " + err);
        }
        try {
            // Get the character encoding and ensure that it is the default (ASCII)
            try {
                char_encoding = H5Pget_char_encoding(plist_id);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5Pget_char_encoding: " + err);
            }
            assertTrue("testH5Aget_create_plist: get_char_encoding", char_encoding == H5T_CSET_ASCII());

            // Create an attribute for the dataset using the property list
            try {
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment name_segment = arena.allocateFrom(attr_name);
                    attribute_id =
                        H5Acreate2(H5did, name_segment, type_id, space_id, plist_id, H5P_DEFAULT());
                }
                assertTrue("testH5Aget_create_plist: H5Acreate", attribute_id >= 0);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5Acreate: " + err);
            }

            // Close the property list, and get the attribute's property list
            H5Pclose(plist_id);
            plist_id = H5Aget_create_plist(attribute_id);
            assertTrue(plist_id > 0);

            // Get the character encoding and ensure that it is the default (ASCII)
            try {
                char_encoding = H5Pget_char_encoding(plist_id);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5Pget_char_encoding: " + err);
            }
            assertTrue("testH5Aget_create_plist: get_char_encoding", char_encoding == H5T_CSET_ASCII());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aget_create_plist " + err);
        }
        finally {
            if (plist_id > 0)
                try {
                    H5Pclose(plist_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aiterate()
    {
        long attr1_id = H5I_INVALID_HID();
        long attr2_id = H5I_INVALID_HID();
        long attr3_id = H5I_INVALID_HID();
        long attr4_id = H5I_INVALID_HID();

        class idata {
            public String attr_name = null;
            idata(String name) { this.attr_name = name; }
        }
        class H5A_iter_data extends H5A_operator2_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5A_operator2_t iter_data = new H5A_iter_data();
        class H5A_iter_callback implements H5A_operator2_t.Function {
            public int apply(long group, String name, H5A_info_t info, H5A_operator2_t op_data)
            {
                idata id = new idata(name);
                ((H5A_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute1");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr1_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute2");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr2_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute3");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr3_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute4");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr4_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            H5A_operator2_t iter_cb = new H5A_iter_callback();
            try {
                H5Aiterate(H5fid, H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 0L, iter_cb, iter_data);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5Aiterate: " + err);
            }
            assertFalse("H5Aiterate ", ((H5A_iter_data)iter_data).iterdata.isEmpty());
            assertTrue("H5Aiterate " + ((H5A_iter_data)iter_data).iterdata.size(),
                       ((H5A_iter_data)iter_data).iterdata.size() == 4);
            assertTrue(
                "H5Aiterate " + (((H5A_iter_data)iter_data).iterdata.get(0)).attr_name,
                (((H5A_iter_data)iter_data).iterdata.get(0)).attr_name.compareToIgnoreCase("attribute1") ==
                    0);
            assertTrue(
                "H5Aiterate " + (((H5A_iter_data)iter_data).iterdata.get(1)).attr_name,
                (((H5A_iter_data)iter_data).iterdata.get(1)).attr_name.compareToIgnoreCase("attribute2") ==
                    0);
            assertTrue(
                "H5Aiterate " + ((idata)((H5A_iter_data)iter_data).iterdata.get(2)).attr_name,
                (((H5A_iter_data)iter_data).iterdata.get(2)).attr_name.compareToIgnoreCase("attribute3") ==
                    0);
            assertTrue("H5Aiterate " + ((idata)((H5A_iter_data)iter_data).iterdata.get(3)).attr_name,
                       ((idata)((H5A_iter_data)iter_data).iterdata.get(3))
                               .attr_name.compareToIgnoreCase("attribute4") == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aiterate: " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
            if (attr4_id > 0)
                try {
                    H5Aclose(attr4_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aiterate_by_name()
    {
        long attr1_id = H5I_INVALID_HID();
        long attr2_id = H5I_INVALID_HID();
        long attr3_id = H5I_INVALID_HID();
        long attr4_id = H5I_INVALID_HID();

        class idata {
            public String attr_name = null;
            idata(String name) { this.attr_name = name; }
        }
        class H5A_iter_data extends H5A_operator2_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5A_operator2_t iter_data = new H5A_iter_data();
        class H5A_iter_callback implements H5A_operator2_t.Function {
            public int apply(long group, String name, H5A_info_t info, H5A_operator2_t op_data)
            {
                idata id = new idata(name);
                ((H5A_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute4");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr1_id = H5Acreate_by_name(loc_id, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute3");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr2_id = H5Acreate_by_name(loc_id, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute2");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr3_id = H5Acreate_by_name(loc_id, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment attr_name_segment = arena.allocateFrom("attribute1");
                MemorySegment obj_name_segment  = arena.allocateFrom(".");
                attr4_id = H5Acreate_by_name(H5fid, obj_name_segment, attr_name_segment, type_id, space_id,
                                             H5P_DEFAULT(), H5P_DEFAULT(), lapl_id);
            }
            H5A_operator2_t iter_cb = new H5A_iter_callback();
            try {
                H5Aiterate_by_name(H5fid, ".", H5_INDEX_NAME(), H5_ITER_INC(), 0L, iter_cb, iter_data,
                                   H5P_DEFAULT());
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5Aiterate_by_name: " + err);
            }
            assertFalse("H5Aiterate_by_name ", ((H5A_iter_data)iter_data).iterdata.isEmpty());
            assertTrue("H5Aiterate_by_name " + ((H5A_iter_data)iter_data).iterdata.size(),
                       ((H5A_iter_data)iter_data).iterdata.size() == 4);
            assertTrue("H5Aiterate_by_name " + ((idata)((H5A_iter_data)iter_data).iterdata.get(1)).attr_name,
                       ((idata)((H5A_iter_data)iter_data).iterdata.get(1))
                               .attr_name.compareToIgnoreCase("attribute2") == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aiterate: " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
            if (attr4_id > 0)
                try {
                    H5Aclose(attr4_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5AVLwr()
    {
        String attr_int_name = "VLIntdata";
        String attr_dbl_name = "VLDbldata";
        long attr_int_id     = H5I_INVALID_HID();
        long attr_dbl_id     = H5I_INVALID_HID();
        long atype_int_id    = H5I_INVALID_HID();
        long atype_dbl_id    = H5I_INVALID_HID();
        long aspace_id       = H5I_INVALID_HID();
        long[] dims          = {4};
        long lsize           = 1;

        ArrayList[] vl_int_data = new ArrayList[4];
        ArrayList[] vl_dbl_data = new ArrayList[4];
        try {
            // Write Integer data
            vl_int_data[0]  = new ArrayList<Integer>(Arrays.asList(1));
            vl_int_data[1]  = new ArrayList<Integer>(Arrays.asList(2, 3));
            vl_int_data[2]  = new ArrayList<Integer>(Arrays.asList(4, 5, 6));
            vl_int_data[3]  = new ArrayList<Integer>(Arrays.asList(7, 8, 9, 10));
            Class dataClass = vl_int_data.getClass();
            assertTrue("testH5AVLwr.getClass: " + dataClass, dataClass.isArray());

            try {
                atype_int_id = H5Tvlen_create(H5T_STD_U32LE_g());
                assertTrue("testH5AVLwr.H5Tvlen_create: ", atype_int_id >= 0);
            }
            catch (Exception err) {
                if (atype_int_id > 0)
                    try {
                        H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("testH5AVLwr: " + err);
            }

            try {
                aspace_id = H5Screate_simple(1, dims, null);
                assertTrue(aspace_id > 0);
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment name_segment = arena.allocateFrom(attr_int_name);
                    attr_int_id = H5Acreate2(H5did, name_segment, atype_int_id, aspace_id, H5P_DEFAULT(),
                                             H5P_DEFAULT());
                }
                assertTrue("testH5AVLwr: ", attr_int_id >= 0);

                H5AwriteVL(attr_int_id, atype_int_id, vl_int_data);
            }
            catch (Exception err) {
                if (attr_int_id > 0)
                    try {
                        H5Aclose(attr_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_int_id > 0)
                    try {
                        H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("testH5AVLwr: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5Sclose(aspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            // Write Double data
            vl_dbl_data[0] = new ArrayList<Double>(Arrays.asList(1.1));
            vl_dbl_data[1] = new ArrayList<Double>(Arrays.asList(2.2, 3.3));
            vl_dbl_data[2] = new ArrayList<Double>(Arrays.asList(4.4, 5.5, 6.6));
            vl_dbl_data[3] = new ArrayList<Double>(Arrays.asList(7.7, 8.8, 9.9, 10.0));
            dataClass      = vl_dbl_data.getClass();
            assertTrue("testH5AVLwr.getClass: " + dataClass, dataClass.isArray());

            try {
                atype_dbl_id = H5Tvlen_create(H5T_NATIVE_DOUBLE_g());
                assertTrue("testH5AVLwr.H5Tvlen_create: ", atype_dbl_id >= 0);
            }
            catch (Exception err) {
                if (atype_dbl_id > 0)
                    try {
                        H5Tclose(atype_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AVLwr: " + err);
            }

            try {
                aspace_id = H5Screate_simple(1, dims, null);
                assertTrue(aspace_id > 0);
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment name_segment = arena.allocateFrom(attr_dbl_name);
                    attr_dbl_id = H5Acreate2(H5did, name_segment, atype_dbl_id, aspace_id, H5P_DEFAULT(),
                                             H5P_DEFAULT());
                }
                assertTrue("testH5AVLwr: ", attr_dbl_id >= 0);

                H5AwriteVL(attr_dbl_id, atype_dbl_id, vl_dbl_data);
            }
            catch (Exception err) {
                if (attr_dbl_id > 0)
                    try {
                        H5Aclose(attr_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_dbl_id > 0)
                    try {
                        H5Tclose(atype_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("testH5AVLwr: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5Sclose(aspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5Fflush(H5fid, H5F_SCOPE_LOCAL());

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_readbuf[j] = new ArrayList<Integer>();

            try {
                H5AreadVL(attr_int_id, atype_int_id, vl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5AVLwr:" + vl_readbuf[0].get(0),
                       vl_int_data[0].get(0).equals(vl_readbuf[0].get(0)));
            assertTrue("testH5AVLwr:" + vl_readbuf[1].get(0),
                       vl_int_data[1].get(0).equals(vl_readbuf[1].get(0)));
            assertTrue("testH5AVLwr:" + vl_readbuf[2].get(0),
                       vl_int_data[2].get(0).equals(vl_readbuf[2].get(0)));
            assertTrue("testH5AVLwr:" + vl_readbuf[3].get(0),
                       vl_int_data[3].get(0).equals(vl_readbuf[3].get(0)));

            // Read Double data
            vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_readbuf[j] = new ArrayList<Double>();

            try {
                H5AreadVL(attr_dbl_id, atype_dbl_id, vl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5AVLwr:" + vl_readbuf[0].get(0),
                       vl_dbl_data[0].get(0).equals(vl_readbuf[0].get(0)));
            assertTrue("testH5AVLwr:" + vl_readbuf[1].get(0),
                       vl_dbl_data[1].get(0).equals(vl_readbuf[1].get(0)));
            assertTrue("testH5AVLwr:" + vl_readbuf[2].get(0),
                       vl_dbl_data[2].get(0).equals(vl_readbuf[2].get(0)));
            assertTrue("testH5AVLwr:" + vl_readbuf[3].get(0),
                       vl_dbl_data[3].get(0).equals(vl_readbuf[3].get(0)));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5AVLwr: " + err);
        }
        finally {
            if (attr_dbl_id > 0)
                try {
                    H5Aclose(attr_dbl_id);
                }
                catch (Exception ex) {
                }
            if (attr_int_id > 0)
                try {
                    H5Aclose(attr_int_id);
                }
                catch (Exception ex) {
                }
            if (atype_dbl_id > 0)
                try {
                    H5Tclose(atype_dbl_id);
                }
                catch (Exception ex) {
                }
            if (atype_int_id > 0)
                try {
                    H5Tclose(atype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5AVLwrVL()
    {
        String attr_int_name   = "VLIntdata";
        long attr_int_id       = H5I_INVALID_HID();
        long atype_int_id      = H5I_INVALID_HID();
        long base_atype_int_id = H5I_INVALID_HID();
        long aspace_id         = H5I_INVALID_HID();
        long[] dims            = {4};
        long lsize             = 1;

        ArrayList[] base_vl_int_data = new ArrayList[4];
        ArrayList[] vl_int_data      = new ArrayList[4];
        try {
            // Write Integer data
            vl_int_data[0]  = new ArrayList<Integer>(Arrays.asList(1));
            vl_int_data[1]  = new ArrayList<Integer>(Arrays.asList(2, 3));
            vl_int_data[2]  = new ArrayList<Integer>(Arrays.asList(4, 5, 6));
            vl_int_data[3]  = new ArrayList<Integer>(Arrays.asList(7, 8, 9, 10));
            Class dataClass = vl_int_data.getClass();
            assertTrue("testH5AVLwrVL.getClass: " + dataClass, dataClass.isArray());

            // Write VL data
            base_vl_int_data[0] = new ArrayList<ArrayList<Integer>>();
            base_vl_int_data[0].add(vl_int_data[0]);
            base_vl_int_data[1] = new ArrayList<ArrayList<Integer>>();
            base_vl_int_data[1].add(vl_int_data[0]);
            base_vl_int_data[1].add(vl_int_data[1]);
            base_vl_int_data[2] = new ArrayList<ArrayList<Integer>>();
            base_vl_int_data[2].add(vl_int_data[0]);
            base_vl_int_data[2].add(vl_int_data[1]);
            base_vl_int_data[2].add(vl_int_data[2]);
            base_vl_int_data[3] = new ArrayList<ArrayList<Integer>>();
            base_vl_int_data[3].add(vl_int_data[0]);
            base_vl_int_data[3].add(vl_int_data[1]);
            base_vl_int_data[3].add(vl_int_data[2]);
            base_vl_int_data[3].add(vl_int_data[3]);

            try {
                atype_int_id = H5Tvlen_create(H5T_STD_U32LE_g());
                assertTrue("testH5AVLwr.H5Tvlen_create: ", atype_int_id >= 0);
                base_atype_int_id = H5Tvlen_create(atype_int_id);
                assertTrue("testH5AVLwrVL.H5Tvlen_create: ", base_atype_int_id >= 0);
            }
            catch (Exception err) {
                if (base_atype_int_id > 0)
                    try {
                        H5Tclose(base_atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_int_id > 0)
                    try {
                        H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("testH5AVLwrVL: " + err);
            }

            try {
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the dims bytes
                    MemorySegment dims_segment = MemorySegment.ofArray(dims);
                    aspace_id                  = H5Screate_simple(1, dims_segment, null);
                }
                catch (Throwable err) {
                    err.printStackTrace();
                    fail("Arena: " + err);
                }
                assertTrue(aspace_id > 0);
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment name_segment = arena.allocateFrom(attr_int_name);
                    attr_int_id = H5Acreate2(H5did, name_segment, base_atype_int_id, aspace_id, H5P_DEFAULT(),
                                             H5P_DEFAULT());
                }
                assertTrue("testH5AVLwrVL: ", attr_int_id >= 0);

                H5AwriteVL(attr_int_id, base_atype_int_id, base_vl_int_data);
            }
            catch (Exception err) {
                if (attr_int_id > 0)
                    try {
                        H5Aclose(attr_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_int_id > 0)
                    try {
                        H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("testH5AVLwrVL: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5Sclose(aspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5Fflush(H5fid, H5F_SCOPE_LOCAL());

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] base_vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                base_vl_readbuf[j] = new ArrayList<ArrayList<Integer>>();

            try {
                H5AreadVL(attr_int_id, base_atype_int_id, base_vl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            ArrayList<ArrayList<Integer>> vl_readbuf = (ArrayList<ArrayList<Integer>>)base_vl_readbuf[0];
            assertTrue("vl_readbuf 0 exists", vl_readbuf != null);
            ArrayList<Integer> vl_readbuf_int = (ArrayList<Integer>)(vl_readbuf.get(0));
            /*
             * System.out.println(); System.out.println("vl_readbuf: " + vl_readbuf);
             * System.out.println("vl_readbuf_int: " + vl_readbuf_int);
             */
            assertTrue("testHADVLwrVL:" + vl_readbuf_int.get(0),
                       vl_int_data[0].get(0).equals(vl_readbuf_int.get(0)));

            vl_readbuf     = (ArrayList<ArrayList<Integer>>)base_vl_readbuf[1];
            vl_readbuf_int = (ArrayList<Integer>)(vl_readbuf.get(1));
            /*
             * System.out.println("vl_readbuf: " + vl_readbuf); System.out.println("vl_readbuf_int: " +
             * vl_readbuf_int);
             */
            assertTrue("testH5AVLwrVL:" + vl_readbuf_int.get(1),
                       vl_int_data[1].get(1).equals(vl_readbuf_int.get(1)));

            vl_readbuf     = (ArrayList<ArrayList<Integer>>)base_vl_readbuf[2];
            vl_readbuf_int = (ArrayList<Integer>)(vl_readbuf.get(2));
            /*
             * System.out.println("vl_readbuf: " + vl_readbuf); System.out.println("vl_readbuf_int: " +
             * vl_readbuf_int);
             */
            assertTrue("testH5AVLwrVL:" + vl_readbuf_int.get(2),
                       vl_int_data[2].get(2).equals(vl_readbuf_int.get(2)));

            vl_readbuf     = (ArrayList<ArrayList<Integer>>)base_vl_readbuf[3];
            vl_readbuf_int = (ArrayList<Integer>)(vl_readbuf.get(3));
            /*
             * System.out.println("vl_readbuf: " + vl_readbuf); System.out.println("vl_readbuf_int: " +
             * vl_readbuf_int);
             */
            assertTrue("testH5AVLwrVL:" + vl_readbuf_int.get(3),
                       vl_int_data[3].get(3).equals(vl_readbuf_int.get(3)));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5AVLwrVL: " + err);
        }
        finally {
            if (attr_int_id > 0)
                try {
                    H5Aclose(attr_int_id);
                }
                catch (Exception ex) {
                }
            if (atype_int_id > 0)
                try {
                    H5Tclose(atype_int_id);
                }
                catch (Exception ex) {
                }
            if (base_atype_int_id > 0)
                try {
                    H5Tclose(base_atype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5AArraywr()
    {
        String att_int_name = "ArrayIntdata";
        long att_int_id     = H5I_INVALID_HID();
        long atype_int_id   = H5I_INVALID_HID();
        long aspace_id      = H5I_INVALID_HID();
        long[] dims         = {4};
        long lsize          = 1;

        ArrayList[] arr_int_data = new ArrayList[4];
        try {
            // Write Integer data
            arr_int_data[0] = new ArrayList<Integer>(Arrays.asList(1, 2, 3, 4));
            arr_int_data[1] = new ArrayList<Integer>(Arrays.asList(2, 3, 4, 5));
            arr_int_data[2] = new ArrayList<Integer>(Arrays.asList(4, 5, 6, 7));
            arr_int_data[3] = new ArrayList<Integer>(Arrays.asList(7, 8, 9, 10));
            Class dataClass = arr_int_data.getClass();
            assertTrue("testH5AArraywr.getClass: " + dataClass, dataClass.isArray());

            try {
                atype_int_id = H5Tarray_create(H5T_STD_U32LE_g(), 1, dims);
                assertTrue("testH5AArraywr.H5Tarray_create: ", atype_int_id >= 0);
            }
            catch (Exception err) {
                if (atype_int_id > 0)
                    try {
                        H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("testH5AArraywr: " + err);
            }

            try {
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the dims bytes
                    MemorySegment dims_segment = MemorySegment.ofArray(dims);
                    aspace_id                  = H5Screate_simple(1, dims_segment, null);
                }
                catch (Throwable err) {
                    err.printStackTrace();
                    fail("Arena: " + err);
                }
                assertTrue(aspace_id > 0);
                try (Arena arena = Arena.ofConfined()) {
                    // Allocate a MemorySegment to hold the string bytes
                    MemorySegment name_segment = arena.allocateFrom(att_int_name);
                    att_int_id = H5Acreate2(H5did, name_segment, atype_int_id, aspace_id, H5P_DEFAULT(),
                                            H5P_DEFAULT());
                }
                assertTrue("testH5AVLwr: ", att_int_id >= 0);

                H5AwriteVL(att_int_id, atype_int_id, arr_int_data);
            }
            catch (Exception err) {
                if (att_int_id > 0)
                    try {
                        H5Aclose(att_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_int_id > 0)
                    try {
                        H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("testH5AVLwr: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5Sclose(aspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5Fflush(H5fid, H5F_SCOPE_LOCAL());

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] arr_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                arr_readbuf[j] = new ArrayList<Integer>();

            try {
                H5AreadVL(att_int_id, atype_int_id, arr_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5AVLwr:" + arr_readbuf[0].get(0),
                       arr_int_data[0].get(0).equals(arr_readbuf[0].get(0)));
            assertTrue("testH5AVLwr:" + arr_readbuf[1].get(0),
                       arr_int_data[1].get(0).equals(arr_readbuf[1].get(0)));
            assertTrue("testH5AVLwr:" + arr_readbuf[2].get(0),
                       arr_int_data[2].get(0).equals(arr_readbuf[2].get(0)));
            assertTrue("testH5AVLwr:" + arr_readbuf[3].get(0),
                       arr_int_data[3].get(0).equals(arr_readbuf[3].get(0)));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5AArraywr: " + err);
        }
        finally {
            if (att_int_id > 0)
                try {
                    H5Aclose(att_int_id);
                }
                catch (Exception ex) {
                }
            if (atype_int_id > 0)
                try {
                    H5Tclose(atype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5AArray_string_buffer() throws Throwable
    {
        String att_str_name = "ArrayStringdata";
        long att_str_id     = H5I_INVALID_HID();
        long atype_str_id   = H5I_INVALID_HID();
        long aspace_id      = H5I_INVALID_HID();
        long[] strdims      = {4};
        long[] dims         = {6};
        long lsize          = 1;

        String[] str_data0 = {"Parting", "is such", "sweet", "sorrow."};
        String[] str_data1 = {"Testing", "one", "two", "three."};
        String[] str_data2 = {"Dog,", "man's", "best", "friend."};
        String[] str_data3 = {"Diamonds", "are", "a", "girls!"};
        String[] str_data4 = {"S A", "T U R", "D A Y", "night"};
        String[] str_data5 = {"That's", "all", "folks", "!!!"};

        ArrayList[] arr_str_data = new ArrayList[6];
        arr_str_data[0]          = new ArrayList<String>(Arrays.asList(str_data0));
        arr_str_data[1]          = new ArrayList<String>(Arrays.asList(str_data1));
        arr_str_data[2]          = new ArrayList<String>(Arrays.asList(str_data2));
        arr_str_data[3]          = new ArrayList<String>(Arrays.asList(str_data3));
        arr_str_data[4]          = new ArrayList<String>(Arrays.asList(str_data4));
        arr_str_data[5]          = new ArrayList<String>(Arrays.asList(str_data5));

        try {
            H5atid = H5Tcopy(H5T_C_S1());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5AArray_string_buffer.H5Tcopy: " + err);
        }
        assertTrue("testH5AArray_string_buffer.H5Tcopy: ", H5atid >= 0);
        try {
            H5Tset_size(H5atid, H5T_VARIABLE());
            assertTrue("testH5AArray_string_buffer.H5Tis_variable_str", H5Tis_variable_str(H5atid));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5DArray_string_buffer.H5Tset_size: " + err);
        }
        try {
            atype_str_id = H5Tarray_create(H5atid, 1, strdims);
            assertTrue("testH5AArray_string_buffer.H5Tarray_create: ", atype_str_id >= 0);
        }
        catch (Exception err) {
            if (atype_str_id > 0)
                try {
                    H5Tclose(atype_str_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5AArray_string_buffer: " + err);
        }

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the dims bytes
                MemorySegment dims_segment = MemorySegment.ofArray(dims);
                aspace_id                  = H5Screate_simple(1, dims_segment, null);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("Arena: " + err);
            }
            assertTrue(aspace_id > 0);
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom(att_str_name);
                att_str_id =
                    H5Acreate2(H5did, name_segment, atype_str_id, aspace_id, H5P_DEFAULT(), H5P_DEFAULT());
            }
            assertTrue("testH5AArray_string_buffer: ", att_str_id >= 0);

            H5AwriteVL(att_str_id, atype_str_id, arr_str_data);
        }
        catch (Exception err) {
            if (att_str_id > 0)
                try {
                    H5Dclose(att_str_id);
                }
                catch (Exception ex) {
                }
            if (atype_str_id > 0)
                try {
                    H5Tclose(atype_str_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5AArray_string_buffer: " + err);
        }
        finally {
            if (aspace_id > 0)
                try {
                    H5Sclose(aspace_id);
                }
                catch (Exception ex) {
                }
        }

        H5Fflush(H5fid, H5F_SCOPE_LOCAL());

        for (int j = 0; j < dims.length; j++)
            lsize *= dims[j];

        ArrayList[] arr_readbuf = new ArrayList[6];
        for (int j = 0; j < lsize; j++)
            arr_readbuf[j] = new ArrayList<String>();

        try {
            H5AreadVL(att_str_id, atype_str_id, arr_readbuf);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            if (att_str_id > 0)
                try {
                    H5Aclose(att_str_id);
                }
                catch (Exception ex) {
                }
            if (atype_str_id > 0)
                try {
                    H5Tclose(atype_str_id);
                }
                catch (Exception ex) {
                }
        }
        assertTrue("testH5AArray_string_buffer:" + arr_readbuf[0].get(0),
                   arr_str_data[0].get(0).equals(arr_readbuf[0].get(0)));
        assertTrue("testH5AArray_string_buffer:" + arr_readbuf[1].get(0),
                   arr_str_data[1].get(0).equals(arr_readbuf[1].get(0)));
        assertTrue("testH5AArray_string_buffer:" + arr_readbuf[2].get(0),
                   arr_str_data[2].get(0).equals(arr_readbuf[2].get(0)));
        assertTrue("testH5AArray_string_buffer:" + arr_readbuf[3].get(0),
                   arr_str_data[3].get(0).equals(arr_readbuf[3].get(0)));
    }
}
