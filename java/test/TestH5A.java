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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.callbacks.H5A_iterate_cb;
import hdf.hdf5lib.callbacks.H5A_iterate_t;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5A_info_t;

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
    long H5fid                          = HDF5Constants.H5I_INVALID_HID;
    long H5dsid                         = HDF5Constants.H5I_INVALID_HID;
    long H5atid                         = HDF5Constants.H5I_INVALID_HID;
    long H5did                          = HDF5Constants.H5I_INVALID_HID;
    long[] H5dims                       = {DIM_X, DIM_Y};
    long type_id                        = HDF5Constants.H5I_INVALID_HID;
    long space_id                       = HDF5Constants.H5I_INVALID_HID;
    long lapl_id                        = HDF5Constants.H5I_INVALID_HID;
    long aapl_id                        = HDF5Constants.H5I_INVALID_HID;

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
        long did = HDF5Constants.H5I_INVALID_HID;
        try {
            did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_STD_I32BE, dsid, HDF5Constants.H5P_DEFAULT,
                               HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5A._createDataset: ", did > 0);

        return did;
    }

    @Before
    public void createH5file() throws NullPointerException, HDF5Exception
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());

        try {
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT);
            assertTrue("TestH5A.createH5file: H5.H5Fcreate: ", H5fid > 0);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            assertTrue("TestH5A.createH5file: H5.H5Screate_simple: ", H5dsid > 0);
            H5did = _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);
            assertTrue("TestH5A.createH5file: _createDataset: ", H5did > 0);
            space_id = H5.H5Screate(HDF5Constants.H5S_NULL);
            assertTrue(space_id > 0);
            lapl_id = H5.H5Pcreate(HDF5Constants.H5P_LINK_ACCESS);
            assertTrue(lapl_id > 0);
            aapl_id = H5.H5Pcreate(HDF5Constants.H5P_ATTRIBUTE_ACCESS);
            assertTrue(aapl_id > 0);
            type_id = H5.H5Tenum_create(HDF5Constants.H5T_STD_I32LE);
            assertTrue(type_id > 0);
            int status = H5.H5Tenum_insert(type_id, "test", 1);
            assertTrue(status >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5A.createH5file: " + err);
        }

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException
    {
        if (H5dsid > 0)
            try {
                H5.H5Sclose(H5dsid);
            }
            catch (Exception ex) {
            }
        if (H5did > 0)
            try {
                H5.H5Dclose(H5did);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                H5.H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }
        if (H5atid > 0)
            try {
                H5.H5Tclose(H5atid);
            }
            catch (Exception ex) {
            }

        if (type_id > 0)
            try {
                H5.H5Tclose(type_id);
            }
            catch (Exception ex) {
            }
        if (space_id > 0)
            try {
                H5.H5Sclose(space_id);
            }
            catch (Exception ex) {
            }
        if (lapl_id > 0)
            try {
                H5.H5Pclose(lapl_id);
            }
            catch (Exception ex) {
            }
        if (aapl_id > 0)
            try {
                H5.H5Pclose(aapl_id);
            }
            catch (Exception ex) {
            }

        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test
    public void testH5Acreate2()
    {
        long attr_id = HDF5Constants.H5I_INVALID_HID;
        try {
            attr_id = H5.H5Acreate(H5did, "dset", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Acreate2", attr_id >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Acreate2: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Acreate2_invalidobject() throws Throwable
    {
        H5.H5Acreate(H5dsid, "dset", type_id, space_id, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Acreate2_nullname() throws Throwable
    {
        H5.H5Acreate(H5did, null, type_id, space_id, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Aopen()
    {
        String attr_name  = "dset";
        long attribute_id = HDF5Constants.H5I_INVALID_HID;
        long attr_id      = HDF5Constants.H5I_INVALID_HID;

        try {
            attr_id = H5.H5Acreate(H5did, attr_name, type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);

            // Opening the existing attribute, attr_name(Created by H5ACreate2)
            // attached to an object identifier.
            attribute_id = H5.H5Aopen(H5did, attr_name, HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Aopen: H5Aopen", attribute_id >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aopen: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Aopen_invalidname() throws Throwable
    {
        H5.H5Aopen(H5did, "attr_name", HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Aopen_by_idx()
    {
        long loc_id       = H5did;
        String obj_name   = ".";
        int idx_type      = HDF5Constants.H5_INDEX_CRT_ORDER;
        int order         = HDF5Constants.H5_ITER_INC;
        long n            = 0;
        long attr_id      = HDF5Constants.H5I_INVALID_HID;
        long attribute_id = HDF5Constants.H5I_INVALID_HID;

        try {
            attr_id = H5.H5Acreate(H5did, "file", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);

            // Opening the existing attribute, obj_name(Created by H5ACreate2)
            // by index, attached to an object identifier.
            attribute_id = H5.H5Aopen_by_idx(H5did, ".", HDF5Constants.H5_INDEX_CRT_ORDER,
                                             HDF5Constants.H5_ITER_INC, 0, aapl_id, lapl_id);

            assertTrue("testH5Aopen_by_idx: H5Aopen_by_idx", attribute_id >= 0);

            // Negative test- Error should be thrown when H5Aopen_by_idx is
            // called
            // with n=5 and we do not have 5 attributes created.
            try {
                n = 5;
                H5.H5Aopen_by_idx(loc_id, obj_name, idx_type, order, n, aapl_id, lapl_id);
                fail("Negative Test Failed:- Error not Thrown when n is invalid.");
            }
            catch (AssertionError err) {
                fail("H5.H5Aopen_by_idx: " + err);
            }
            catch (HDF5LibraryException err) {
            }

            // Negative test- Error should be thrown when H5Aopen_by_idx is
            // called
            // with an invalid object name(which hasn't been created).
            try {
                n        = 0;
                obj_name = "file";
                H5.H5Aopen_by_idx(loc_id, obj_name, idx_type, order, n, aapl_id, lapl_id);
                fail("Negative Test Failed:- Error not Thrown when attribute name is invalid.");
            }
            catch (AssertionError err) {
                fail("H5.H5Aopen_by_idx: " + err);
            }
            catch (HDF5LibraryException err) {
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aopen_by_idx: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
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
        long attribute_id = HDF5Constants.H5I_INVALID_HID;
        boolean bool_val  = false;

        try {
            attribute_id = H5.H5Acreate_by_name(H5fid, obj_name, attr_name, type_id, space_id,
                                                HDF5Constants.H5P_DEFAULT, aapl_id, lapl_id);
            assertTrue("testH5Acreate_by_name: H5Acreate_by_name", attribute_id >= 0);

            // Check if the name of attribute attached to the object specified
            // by loc_id and obj_name exists.It should be true.
            bool_val = H5.H5Aexists_by_name(H5fid, obj_name, attr_name, lapl_id);
            assertTrue("testH5Acreate_by_name: H5Aexists_by_name", bool_val == true);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Acreate_by_name " + err);
        }
        finally {
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
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
        long attr_id         = HDF5Constants.H5I_INVALID_HID;
        int ret_val          = -1;
        boolean bool_val     = false;

        try {
            attr_id =
                H5.H5Acreate(loc_id, old_attr_name, type_id, space_id, HDF5Constants.H5P_DEFAULT, aapl_id);

            ret_val = H5.H5Arename(loc_id, old_attr_name, new_attr_name);

            // Check the return value.It should be non negative.
            assertTrue("testH5Arename: H5Arename", ret_val >= 0);

            // Check if the new name of attribute attached to the object
            // specified by loc_id and obj_name exists.It should be true.
            bool_val = H5.H5Aexists(loc_id, new_attr_name);
            assertTrue("testH5Arename: H5Aexists", bool_val == true);

            // Check if the old name of attribute attached to the object
            // specified by loc_id and obj_name exists. It should equal false.
            bool_val = H5.H5Aexists(loc_id, old_attr_name);
            assertEquals(bool_val, false);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Arename " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
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
        long attr_id         = HDF5Constants.H5I_INVALID_HID;
        int ret_val          = -1;
        boolean bool_val     = false;

        try {
            attr_id = H5.H5Acreate_by_name(loc_id, obj_name, old_attr_name, type_id, space_id,
                                           HDF5Constants.H5P_DEFAULT, aapl_id, lapl_id);

            ret_val = H5.H5Arename_by_name(loc_id, obj_name, old_attr_name, new_attr_name, lapl_id);

            // Check the return value.It should be non negative.
            assertTrue("testH5Arename_by_name: H5Arename_by_name", ret_val >= 0);

            // Check if the new name of attribute attached to the object
            // specified by loc_id and obj_name exists.It should be true.
            bool_val = H5.H5Aexists_by_name(loc_id, obj_name, new_attr_name, lapl_id);
            assertTrue("testH5Arename_by_name: H5Aexists_by_name", bool_val == true);

            // Check if the old name of attribute attached to the object
            // specified by loc_id and obj_name exists. It should equal false.
            bool_val = H5.H5Aexists_by_name(loc_id, obj_name, old_attr_name, lapl_id);
            assertEquals(bool_val, false);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Arename_by_name " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
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
        long attribute_id = HDF5Constants.H5I_INVALID_HID;

        try {
            attribute_id = H5.H5Acreate_by_name(H5fid, obj_name, attr_name, type_id, space_id,
                                                HDF5Constants.H5P_DEFAULT, aapl_id, lapl_id);
            assertTrue("testH5Aget_name: H5Acreate_by_name ", attribute_id > 0);
            ret_name = H5.H5Aget_name(attribute_id);
            assertEquals(ret_name, attr_name);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aget_name " + err);
        }
        finally {
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
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
        int idx_type    = HDF5Constants.H5_INDEX_NAME;
        int order       = HDF5Constants.H5_ITER_INC;
        int n           = 0;
        long attr1_id   = HDF5Constants.H5I_INVALID_HID;
        long attr2_id   = HDF5Constants.H5I_INVALID_HID;

        try {
            attr1_id = H5.H5Acreate_by_name(loc_id, obj_name, attr_name, type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr2_id = H5.H5Acreate_by_name(loc_id, obj_name, attr2_name, type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);

            // getting the 1st attribute name(n=0).
            ret_name = H5.H5Aget_name_by_idx(loc_id, obj_name, idx_type, order, n, lapl_id);
            assertFalse("H5Aget_name_by_idx ", ret_name == null);
            assertEquals(ret_name, attr_name);

            // getting the second attribute name(n=1)
            ret_name = H5.H5Aget_name_by_idx(loc_id, obj_name, idx_type, order, 1, lapl_id);
            assertFalse("H5Aget_name_by_idx ", ret_name == null);
            assertEquals(ret_name, attr2_name);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aget_name_by_idx " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5.H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5.H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_storage_size()
    {
        long attr_id   = HDF5Constants.H5I_INVALID_HID;
        long attr_size = HDF5Constants.H5I_INVALID_HID;

        try {
            attr_id = H5.H5Acreate(H5did, "dset", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);

            attr_size = H5.H5Aget_storage_size(attr_id);
            assertTrue("The size of attribute is :", attr_size == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aget_storage_size: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_info()
    {
        H5A_info_t attr_info = null;
        long attribute_id    = HDF5Constants.H5I_INVALID_HID;
        long attr_id         = HDF5Constants.H5I_INVALID_HID;

        try {
            attr_id      = H5.H5Acreate(H5did, "dset", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                        HDF5Constants.H5P_DEFAULT);
            attribute_id = H5.H5Aopen(H5did, "dset", HDF5Constants.H5P_DEFAULT);
            // Calling H5Aget_info with attribute_id returned from H5Aopen.
            attr_info = H5.H5Aget_info(attribute_id);
            assertFalse("H5Aget_info ", attr_info == null);
            assertTrue("Corder_Valid should be false", attr_info.corder_valid == false);
            assertTrue("Character set used for attribute name",
                       attr_info.cset == HDF5Constants.H5T_CSET_ASCII);
            assertTrue("Corder ", attr_info.corder == 0);
            assertEquals(attr_info.data_size, H5.H5Aget_storage_size(attr_id));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aget_info: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_info1()
    {
        H5A_info_t attr_info = null;
        long attribute_id    = HDF5Constants.H5I_INVALID_HID;
        long attr_id         = HDF5Constants.H5I_INVALID_HID;
        int order            = HDF5Constants.H5_ITER_INC;

        try {
            attr_id      = H5.H5Acreate(H5did, ".", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                        HDF5Constants.H5P_DEFAULT);
            attribute_id = H5.H5Aopen_by_idx(H5did, ".", HDF5Constants.H5_INDEX_CRT_ORDER, order, 0,
                                             HDF5Constants.H5P_DEFAULT, lapl_id);
            // Calling H5Aget_info with attribute_id returned from
            // H5Aopen_by_idx.
            attr_info = H5.H5Aget_info(attribute_id);

            assertFalse("H5Aget_info ", attr_info == null);
            assertTrue("Corder_Valid should be true", attr_info.corder_valid == true);
            assertTrue("Character set", attr_info.cset == HDF5Constants.H5T_CSET_ASCII);
            assertTrue("Corder ", attr_info.corder == 0);
            assertEquals(attr_info.data_size, H5.H5Aget_storage_size(attr_id));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aget_info1: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_info_by_idx()
    {
        long attr_id  = HDF5Constants.H5I_INVALID_HID;
        long attr2_id = HDF5Constants.H5I_INVALID_HID;
        ;
        H5A_info_t attr_info = null;

        try {
            attr_id  = H5.H5Acreate(H5did, "dset1", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                    HDF5Constants.H5P_DEFAULT);
            attr2_id = H5.H5Acreate(H5did, "dataset2", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                    HDF5Constants.H5P_DEFAULT);

            // Verify info for 1st attribute, in increasing creation order
            attr_info = H5.H5Aget_info_by_idx(H5did, ".", HDF5Constants.H5_INDEX_CRT_ORDER,
                                              HDF5Constants.H5_ITER_INC, 0, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder ", attr_info.corder ==
                                      0); // should equal 0 as this is the order of 1st attribute created.
            assertEquals(attr_info.data_size, H5.H5Aget_storage_size(attr_id));

            // Verify info for 2nd attribute, in increasing creation order
            attr_info = H5.H5Aget_info_by_idx(H5did, ".", HDF5Constants.H5_INDEX_CRT_ORDER,
                                              HDF5Constants.H5_ITER_INC, 1, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder == 1);
            assertEquals(attr_info.data_size, H5.H5Aget_storage_size(attr2_id));

            // verify info for 2nd attribute, in decreasing creation order
            attr_info = H5.H5Aget_info_by_idx(H5did, ".", HDF5Constants.H5_INDEX_CRT_ORDER,
                                              HDF5Constants.H5_ITER_DEC, 0, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder ==
                                     1); // should equal 1 as this is the order of 2nd attribute created.

            // verify info for 1st attribute, in decreasing creation order
            attr_info = H5.H5Aget_info_by_idx(H5did, ".", HDF5Constants.H5_INDEX_CRT_ORDER,
                                              HDF5Constants.H5_ITER_DEC, 1, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder ==
                                     0); // should equal 0 as this is the order of 1st attribute created.

            // verify info for 1st attribute, in increasing name order
            attr_info = H5.H5Aget_info_by_idx(H5did, ".", HDF5Constants.H5_INDEX_NAME,
                                              HDF5Constants.H5_ITER_INC, 1, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder ==
                                     0); // should equal 0 as this is the order of 1st attribute created.

            // verify info for 2nd attribute, in decreasing name order
            attr_info = H5.H5Aget_info_by_idx(H5did, ".", HDF5Constants.H5_INDEX_NAME,
                                              HDF5Constants.H5_ITER_DEC, 1, lapl_id);
            assertNotNull(attr_info);
            assertTrue("Corder", attr_info.corder ==
                                     1); // should equal 1 as this is the order of 2nd attribute created.
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aget_info_by_idx:" + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5.H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aget_info_by_name()
    {
        long attr_id         = HDF5Constants.H5I_INVALID_HID;
        H5A_info_t attr_info = null;
        String obj_name      = ".";
        String attr_name     = "DATASET";

        try {
            attr_id   = H5.H5Acreate_by_name(H5fid, obj_name, attr_name, type_id, space_id,
                                             HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr_info = H5.H5Aget_info_by_name(H5fid, obj_name, attr_name, lapl_id);
            assertNotNull(attr_info);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aget_info_by_name:" + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Adelete_by_name()
    {
        long attr_id     = HDF5Constants.H5I_INVALID_HID;
        int ret_val      = -1;
        boolean bool_val = false;
        boolean exists   = false;

        try {
            attr_id = H5.H5Acreate_by_name(H5fid, ".", "DATASET", type_id, space_id,
                                           HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            ret_val = H5.H5Adelete_by_name(H5fid, ".", "DATASET", lapl_id);
            assertTrue("H5Adelete_by_name", ret_val >= 0);

            // Check if the Attribute still exists.
            bool_val = H5.H5Aexists_by_name(H5fid, ".", "DATASET", lapl_id);
            assertFalse("testH5Adelete_by_name: H5Aexists_by_name", bool_val);
            exists = H5.H5Aexists(H5fid, "DATASET");
            assertFalse("testH5Adelete_by_name: H5Aexists ", exists);

            // Negative test. Error thrown when we try to delete an attribute
            // that has already been deleted.
            try {
                ret_val = H5.H5Adelete_by_name(H5fid, ".", "DATASET", lapl_id);
                fail("Negative Test Failed: Error Not thrown.");
            }
            catch (AssertionError err) {
                fail("H5.H5Adelete_by_name: " + err);
            }
            catch (HDF5LibraryException err) {
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Adelete_by_name " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aexists()
    {
        boolean exists    = false;
        long attr_id      = HDF5Constants.H5I_INVALID_HID;
        long attribute_id = HDF5Constants.H5I_INVALID_HID;

        try {
            exists = H5.H5Aexists(H5fid, "None");
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aexists: " + err);
        }
        assertFalse("H5Aexists ", exists);

        try {
            attr_id = H5.H5Acreate(H5fid, "dset", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
            exists  = H5.H5Aexists(H5fid, "dset");
            assertTrue("H5Aexists ", exists);

            attribute_id =
                H5.H5Acreate_by_name(H5fid, ".", "attribute", type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                     HDF5Constants.H5P_DEFAULT, lapl_id);
            exists = H5.H5Aexists(H5fid, "attribute");
            assertTrue("H5Aexists ", exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aexists: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Adelete_by_idx_order()
    {
        boolean exists = false;
        long attr1_id  = HDF5Constants.H5I_INVALID_HID;
        long attr2_id  = HDF5Constants.H5I_INVALID_HID;
        long attr3_id  = HDF5Constants.H5I_INVALID_HID;
        long attr4_id  = HDF5Constants.H5I_INVALID_HID;

        try {
            attr1_id = H5.H5Acreate_by_name(H5fid, ".", "attribute1", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr2_id = H5.H5Acreate_by_name(H5fid, ".", "attribute2", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr3_id = H5.H5Acreate_by_name(H5fid, ".", "attribute3", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr4_id = H5.H5Acreate_by_name(H5fid, ".", "attribute4", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);

            H5.H5Adelete_by_idx(H5fid, ".", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 3,
                                lapl_id);
            exists = H5.H5Aexists(H5fid, "attribute4");
            assertFalse("H5Adelete_by_idx: H5Aexists", exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Adelete_by_idx: " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5.H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5.H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5.H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
            if (attr4_id > 0)
                try {
                    H5.H5Aclose(attr4_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Adelete_by_idx_name1()
    {
        boolean exists = false;
        long attr1_id  = HDF5Constants.H5I_INVALID_HID;
        long attr2_id  = HDF5Constants.H5I_INVALID_HID;
        long attr3_id  = HDF5Constants.H5I_INVALID_HID;

        try {
            attr1_id = H5.H5Acreate_by_name(H5fid, ".", "attribute1", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr2_id = H5.H5Acreate_by_name(H5fid, ".", "attribute2", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr3_id = H5.H5Acreate_by_name(H5fid, ".", "attribute3", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            H5.H5Adelete_by_idx(H5fid, ".", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 2,
                                lapl_id);
            exists = H5.H5Aexists(H5fid, "attribute3");
            assertFalse("H5Adelete_by_idx: H5Aexists", exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Adelete_by_idx: " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5.H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5.H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5.H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Adelete_by_idx_name2()
    {
        boolean exists = false;
        long attr1_id  = HDF5Constants.H5I_INVALID_HID;
        long attr2_id  = HDF5Constants.H5I_INVALID_HID;
        long attr3_id  = HDF5Constants.H5I_INVALID_HID;
        long attr4_id  = HDF5Constants.H5I_INVALID_HID;

        try {
            attr1_id = H5.H5Acreate_by_name(H5fid, ".", "attribute1", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr2_id = H5.H5Acreate_by_name(H5fid, ".", "attribute2", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr3_id = H5.H5Acreate_by_name(H5fid, ".", "attribute3", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr4_id = H5.H5Acreate_by_name(H5fid, ".", "attribute4", type_id, space_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);

            H5.H5Adelete_by_idx(H5fid, ".", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_DEC, 3,
                                lapl_id);
            exists = H5.H5Aexists(H5fid, "attribute1");
            assertFalse("H5Adelete_by_idx: H5Aexists", exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Adelete_by_idx: " + err);
        }
        finally {
            if (attr1_id > 0)
                try {
                    H5.H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5.H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5.H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
            if (attr4_id > 0)
                try {
                    H5.H5Aclose(attr4_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test(expected = NullPointerException.class)
    public void testH5Adelete_by_idx_null() throws Throwable
    {
        H5.H5Adelete_by_idx(H5fid, null, HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0,
                            lapl_id);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Adelete_by_idx_invalidobject() throws Throwable
    {
        H5.H5Adelete_by_idx(H5fid, "invalid", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0,
                            lapl_id);
    }

    @Test
    public void testH5Aopen_by_name()
    {
        String obj_name   = ".";
        String attr_name  = "DATASET";
        long attribute_id = HDF5Constants.H5I_INVALID_HID;
        long aid          = HDF5Constants.H5I_INVALID_HID;

        try {
            attribute_id =
                H5.H5Acreate_by_name(H5fid, obj_name, attr_name, type_id, space_id, HDF5Constants.H5P_DEFAULT,
                                     HDF5Constants.H5P_DEFAULT, lapl_id);

            // open Attribute by name
            if (attribute_id >= 0) {
                try {
                    aid = H5.H5Aopen_by_name(H5fid, obj_name, attr_name, HDF5Constants.H5P_DEFAULT, lapl_id);
                    assertTrue("testH5Aopen_by_name: ", aid >= 0);
                }
                catch (Throwable err) {
                    err.printStackTrace();
                    fail("H5.H5Aopen_by_name " + err);
                }
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Aopen_by_name " + err);
        }
        finally {
            if (aid > 0)
                try {
                    H5.H5Aclose(aid);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Awrite_readVL()
    {
        String attr_name  = "VLdata";
        long attr_id      = HDF5Constants.H5I_INVALID_HID;
        long atype_id     = HDF5Constants.H5I_INVALID_HID;
        long aspace_id    = HDF5Constants.H5I_INVALID_HID;
        String[] str_data = {"Parting", "is such", "sweet", "sorrow."};
        long[] dims       = {str_data.length};
        long lsize        = 1;

        try {
            atype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            assertTrue("testH5Awrite_readVL.H5Tcopy: ", atype_id >= 0);
            H5.H5Tset_size(atype_id, HDF5Constants.H5T_VARIABLE);
            assertTrue("testH5Awrite_readVL.H5Tis_variable_str", H5.H5Tis_variable_str(atype_id));
        }
        catch (Exception err) {
            if (atype_id > 0)
                try {
                    H5.H5Tclose(atype_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("H5.testH5Awrite_readVL: " + err);
        }

        try {
            aspace_id = H5.H5Screate_simple(1, dims, null);
            assertTrue(aspace_id > 0);
            attr_id = H5.H5Acreate(H5did, attr_name, atype_id, aspace_id, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Awrite_readVL: ", attr_id >= 0);

            H5.H5AwriteVL(attr_id, atype_id, str_data);

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++) {
                lsize *= dims[j];
            }
            String[] strs = new String[(int)lsize];
            for (int j = 0; j < lsize; j++) {
                strs[j] = "";
            }
            try {
                H5.H5AreadVL(attr_id, atype_id, strs);
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
            fail("H5.testH5Awrite_readVL: " + err);
        }
        finally {
            if (attr_id > 0)
                try {
                    H5.H5Aclose(attr_id);
                }
                catch (Exception ex) {
                }
            if (aspace_id > 0)
                try {
                    H5.H5Sclose(aspace_id);
                }
                catch (Exception ex) {
                }
            if (atype_id > 0)
                try {
                    H5.H5Tclose(atype_id);
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
        long plist_id     = HDF5Constants.H5I_INVALID_HID;
        long attribute_id = HDF5Constants.H5I_INVALID_HID;

        try {
            plist_id = H5.H5Pcreate(HDF5Constants.H5P_ATTRIBUTE_CREATE);
            assertTrue(plist_id > 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aget_create_plist: H5Pcreate " + err);
        }
        try {
            // Get the character encoding and ensure that it is the default (ASCII)
            try {
                char_encoding = H5.H5Pget_char_encoding(plist_id);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5Pget_char_encoding: " + err);
            }
            assertTrue("testH5Aget_create_plist: get_char_encoding",
                       char_encoding == HDF5Constants.H5T_CSET_ASCII);

            // Create an attribute for the dataset using the property list
            try {
                attribute_id =
                    H5.H5Acreate(H5fid, attr_name, type_id, space_id, plist_id, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5Aget_create_plist: H5Acreate", attribute_id >= 0);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5.H5Acreate: " + err);
            }

            // Close the property list, and get the attribute's property list
            H5.H5Pclose(plist_id);
            plist_id = H5.H5Aget_create_plist(attribute_id);
            assertTrue(plist_id > 0);

            // Get the character encoding and ensure that it is the default (ASCII)
            try {
                char_encoding = H5.H5Pget_char_encoding(plist_id);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5Pget_char_encoding: " + err);
            }
            assertTrue("testH5Aget_create_plist: get_char_encoding",
                       char_encoding == HDF5Constants.H5T_CSET_ASCII);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aget_create_plist " + err);
        }
        finally {
            if (plist_id > 0)
                try {
                    H5.H5Pclose(plist_id);
                }
                catch (Exception ex) {
                }
            if (attribute_id > 0)
                try {
                    H5.H5Aclose(attribute_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aiterate()
    {
        long attr1_id = HDF5Constants.H5I_INVALID_HID;
        long attr2_id = HDF5Constants.H5I_INVALID_HID;
        long attr3_id = HDF5Constants.H5I_INVALID_HID;
        long attr4_id = HDF5Constants.H5I_INVALID_HID;

        class idata {
            public String attr_name = null;
            idata(String name) { this.attr_name = name; }
        }
        class H5A_iter_data implements H5A_iterate_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5A_iterate_t iter_data = new H5A_iter_data();
        class H5A_iter_callback implements H5A_iterate_cb {
            public int callback(long group, String name, H5A_info_t info, H5A_iterate_t op_data)
            {
                idata id = new idata(name);
                ((H5A_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        try {
            attr1_id               = H5.H5Acreate_by_name(H5fid, ".", "attribute1", type_id, space_id,
                                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr2_id               = H5.H5Acreate_by_name(H5fid, ".", "attribute2", type_id, space_id,
                                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr3_id               = H5.H5Acreate_by_name(H5fid, ".", "attribute3", type_id, space_id,
                                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr4_id               = H5.H5Acreate_by_name(H5fid, ".", "attribute4", type_id, space_id,
                                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            H5A_iterate_cb iter_cb = new H5A_iter_callback();
            try {
                H5.H5Aiterate(H5fid, HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0L, iter_cb,
                              iter_data);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5.H5Aiterate: " + err);
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
                    H5.H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5.H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5.H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
            if (attr4_id > 0)
                try {
                    H5.H5Aclose(attr4_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5Aiterate_by_name()
    {
        long attr1_id = HDF5Constants.H5I_INVALID_HID;
        long attr2_id = HDF5Constants.H5I_INVALID_HID;
        long attr3_id = HDF5Constants.H5I_INVALID_HID;
        long attr4_id = HDF5Constants.H5I_INVALID_HID;

        class idata {
            public String attr_name = null;
            idata(String name) { this.attr_name = name; }
        }
        class H5A_iter_data implements H5A_iterate_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5A_iterate_t iter_data = new H5A_iter_data();
        class H5A_iter_callback implements H5A_iterate_cb {
            public int callback(long group, String name, H5A_info_t info, H5A_iterate_t op_data)
            {
                idata id = new idata(name);
                ((H5A_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        try {
            attr1_id               = H5.H5Acreate_by_name(H5fid, ".", "attribute4", type_id, space_id,
                                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr2_id               = H5.H5Acreate_by_name(H5fid, ".", "attribute3", type_id, space_id,
                                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr3_id               = H5.H5Acreate_by_name(H5fid, ".", "attribute2", type_id, space_id,
                                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            attr4_id               = H5.H5Acreate_by_name(H5fid, ".", "attribute1", type_id, space_id,
                                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, lapl_id);
            H5A_iterate_cb iter_cb = new H5A_iter_callback();
            try {
                H5.H5Aiterate_by_name(H5fid, ".", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0L,
                                      iter_cb, iter_data, HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5.H5Aiterate_by_name: " + err);
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
                    H5.H5Aclose(attr1_id);
                }
                catch (Exception ex) {
                }
            if (attr2_id > 0)
                try {
                    H5.H5Aclose(attr2_id);
                }
                catch (Exception ex) {
                }
            if (attr3_id > 0)
                try {
                    H5.H5Aclose(attr3_id);
                }
                catch (Exception ex) {
                }
            if (attr4_id > 0)
                try {
                    H5.H5Aclose(attr4_id);
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
        long attr_int_id     = HDF5Constants.H5I_INVALID_HID;
        long attr_dbl_id     = HDF5Constants.H5I_INVALID_HID;
        long atype_int_id    = HDF5Constants.H5I_INVALID_HID;
        long atype_dbl_id    = HDF5Constants.H5I_INVALID_HID;
        long aspace_id       = HDF5Constants.H5I_INVALID_HID;
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
                atype_int_id = H5.H5Tvlen_create(HDF5Constants.H5T_STD_U32LE);
                assertTrue("testH5AVLwr.H5Tvlen_create: ", atype_int_id >= 0);
            }
            catch (Exception err) {
                if (atype_int_id > 0)
                    try {
                        H5.H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AVLwr: " + err);
            }

            try {
                aspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(aspace_id > 0);
                attr_int_id = H5.H5Acreate(H5did, attr_int_name, atype_int_id, aspace_id,
                                           HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5AVLwr: ", attr_int_id >= 0);

                H5.H5AwriteVL(attr_int_id, atype_int_id, vl_int_data);
            }
            catch (Exception err) {
                if (attr_int_id > 0)
                    try {
                        H5.H5Aclose(attr_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_int_id > 0)
                    try {
                        H5.H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AVLwr: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5.H5Sclose(aspace_id);
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
                atype_dbl_id = H5.H5Tvlen_create(HDF5Constants.H5T_NATIVE_DOUBLE);
                assertTrue("testH5AVLwr.H5Tvlen_create: ", atype_dbl_id >= 0);
            }
            catch (Exception err) {
                if (atype_dbl_id > 0)
                    try {
                        H5.H5Tclose(atype_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AVLwr: " + err);
            }

            try {
                aspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(aspace_id > 0);
                attr_dbl_id = H5.H5Acreate(H5did, attr_dbl_name, atype_dbl_id, aspace_id,
                                           HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5AVLwr: ", attr_dbl_id >= 0);

                H5.H5AwriteVL(attr_dbl_id, atype_dbl_id, vl_dbl_data);
            }
            catch (Exception err) {
                if (attr_dbl_id > 0)
                    try {
                        H5.H5Aclose(attr_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_dbl_id > 0)
                    try {
                        H5.H5Tclose(atype_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AVLwr: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5.H5Sclose(aspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_readbuf[j] = new ArrayList<Integer>();

            try {
                H5.H5AreadVL(attr_int_id, atype_int_id, vl_readbuf);
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
                H5.H5AreadVL(attr_dbl_id, atype_dbl_id, vl_readbuf);
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
            fail("H5.testH5AVLwr: " + err);
        }
        finally {
            if (attr_dbl_id > 0)
                try {
                    H5.H5Aclose(attr_dbl_id);
                }
                catch (Exception ex) {
                }
            if (attr_int_id > 0)
                try {
                    H5.H5Aclose(attr_int_id);
                }
                catch (Exception ex) {
                }
            if (atype_dbl_id > 0)
                try {
                    H5.H5Tclose(atype_dbl_id);
                }
                catch (Exception ex) {
                }
            if (atype_int_id > 0)
                try {
                    H5.H5Tclose(atype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5AVLwrVL()
    {
        String attr_int_name   = "VLIntdata";
        long attr_int_id       = HDF5Constants.H5I_INVALID_HID;
        long atype_int_id      = HDF5Constants.H5I_INVALID_HID;
        long base_atype_int_id = HDF5Constants.H5I_INVALID_HID;
        long aspace_id         = HDF5Constants.H5I_INVALID_HID;
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
                atype_int_id = H5.H5Tvlen_create(HDF5Constants.H5T_STD_U32LE);
                assertTrue("testH5AVLwr.H5Tvlen_create: ", atype_int_id >= 0);
                base_atype_int_id = H5.H5Tvlen_create(atype_int_id);
                assertTrue("testH5AVLwrVL.H5Tvlen_create: ", base_atype_int_id >= 0);
            }
            catch (Exception err) {
                if (base_atype_int_id > 0)
                    try {
                        H5.H5Tclose(base_atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_int_id > 0)
                    try {
                        H5.H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AVLwrVL: " + err);
            }

            try {
                aspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(aspace_id > 0);
                attr_int_id = H5.H5Acreate(H5did, attr_int_name, base_atype_int_id, aspace_id,
                                           HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5AVLwrVL: ", attr_int_id >= 0);

                H5.H5AwriteVL(attr_int_id, base_atype_int_id, base_vl_int_data);
            }
            catch (Exception err) {
                if (attr_int_id > 0)
                    try {
                        H5.H5Aclose(attr_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_int_id > 0)
                    try {
                        H5.H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AVLwrVL: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5.H5Sclose(aspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] base_vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                base_vl_readbuf[j] = new ArrayList<ArrayList<Integer>>();

            try {
                H5.H5AreadVL(attr_int_id, base_atype_int_id, base_vl_readbuf);
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
            fail("H5.testH5AVLwrVL: " + err);
        }
        finally {
            if (attr_int_id > 0)
                try {
                    H5.H5Aclose(attr_int_id);
                }
                catch (Exception ex) {
                }
            if (atype_int_id > 0)
                try {
                    H5.H5Tclose(atype_int_id);
                }
                catch (Exception ex) {
                }
            if (base_atype_int_id > 0)
                try {
                    H5.H5Tclose(base_atype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5AArraywr()
    {
        String att_int_name = "ArrayIntdata";
        long att_int_id     = HDF5Constants.H5I_INVALID_HID;
        long atype_int_id   = HDF5Constants.H5I_INVALID_HID;
        long aspace_id      = HDF5Constants.H5I_INVALID_HID;
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
                atype_int_id = H5.H5Tarray_create(HDF5Constants.H5T_STD_U32LE, 1, dims);
                assertTrue("testH5AArraywr.H5Tarray_create: ", atype_int_id >= 0);
            }
            catch (Exception err) {
                if (atype_int_id > 0)
                    try {
                        H5.H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AArraywr: " + err);
            }

            try {
                aspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(aspace_id > 0);
                att_int_id = H5.H5Acreate(H5did, att_int_name, atype_int_id, aspace_id,
                                          HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5AVLwr: ", att_int_id >= 0);

                H5.H5AwriteVL(att_int_id, atype_int_id, arr_int_data);
            }
            catch (Exception err) {
                if (att_int_id > 0)
                    try {
                        H5.H5Aclose(att_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_int_id > 0)
                    try {
                        H5.H5Tclose(atype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5AVLwr: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5.H5Sclose(aspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] arr_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                arr_readbuf[j] = new ArrayList<Integer>();

            try {
                H5.H5AreadVL(att_int_id, atype_int_id, arr_readbuf);
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
            fail("H5.testH5AArraywr: " + err);
        }
        finally {
            if (att_int_id > 0)
                try {
                    H5.H5Aclose(att_int_id);
                }
                catch (Exception ex) {
                }
            if (atype_int_id > 0)
                try {
                    H5.H5Tclose(atype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5AArray_string_buffer() throws Throwable
    {
        String att_str_name = "ArrayStringdata";
        long att_str_id     = HDF5Constants.H5I_INVALID_HID;
        long atype_str_id   = HDF5Constants.H5I_INVALID_HID;
        long aspace_id      = HDF5Constants.H5I_INVALID_HID;
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
            H5atid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5AArray_string_buffer.H5.H5Tcopy: " + err);
        }
        assertTrue("testH5AArray_string_buffer.H5Tcopy: ", H5atid >= 0);
        try {
            H5.H5Tset_size(H5atid, HDF5Constants.H5T_VARIABLE);
            assertTrue("testH5AArray_string_buffer.H5Tis_variable_str", H5.H5Tis_variable_str(H5atid));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5DArray_string_buffer.H5Tset_size: " + err);
        }
        try {
            atype_str_id = H5.H5Tarray_create(H5atid, 1, strdims);
            assertTrue("testH5AArray_string_buffer.H5Tarray_create: ", atype_str_id >= 0);
        }
        catch (Exception err) {
            if (atype_str_id > 0)
                try {
                    H5.H5Tclose(atype_str_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5AArray_string_buffer: " + err);
        }

        try {
            aspace_id = H5.H5Screate_simple(1, dims, null);
            assertTrue(aspace_id > 0);
            att_str_id = H5.H5Acreate(H5did, att_str_name, atype_str_id, aspace_id, HDF5Constants.H5P_DEFAULT,
                                      HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5AArray_string_buffer: ", att_str_id >= 0);

            H5.H5AwriteVL(att_str_id, atype_str_id, arr_str_data);
        }
        catch (Exception err) {
            if (att_str_id > 0)
                try {
                    H5.H5Dclose(att_str_id);
                }
                catch (Exception ex) {
                }
            if (atype_str_id > 0)
                try {
                    H5.H5Tclose(atype_str_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5AArray_string_buffer: " + err);
        }
        finally {
            if (aspace_id > 0)
                try {
                    H5.H5Sclose(aspace_id);
                }
                catch (Exception ex) {
                }
        }

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

        for (int j = 0; j < dims.length; j++)
            lsize *= dims[j];

        ArrayList[] arr_readbuf = new ArrayList[6];
        for (int j = 0; j < lsize; j++)
            arr_readbuf[j] = new ArrayList<String>();

        try {
            H5.H5AreadVL(att_str_id, atype_str_id, arr_readbuf);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            if (att_str_id > 0)
                try {
                    H5.H5Aclose(att_str_id);
                }
                catch (Exception ex) {
                }
            if (atype_str_id > 0)
                try {
                    H5.H5Tclose(atype_str_id);
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
