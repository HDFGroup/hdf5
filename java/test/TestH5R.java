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
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5FunctionArgumentException;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5R {
    @Rule
    public TestName testname            = new TestName();
    private static final String H5_FILE = "testH5R.h5";
    private static final int DIM_X      = 4;
    private static final int DIM_Y      = 6;
    long H5fid                          = HDF5Constants.H5I_INVALID_HID;
    long H5dsid                         = HDF5Constants.H5I_INVALID_HID;
    long H5did                          = HDF5Constants.H5I_INVALID_HID;
    long H5gid                          = HDF5Constants.H5I_INVALID_HID;
    long H5did2                         = HDF5Constants.H5I_INVALID_HID;
    long[] H5dims                       = {DIM_X, DIM_Y};
    int[][] dset_data                   = new int[DIM_X][DIM_Y];
    int FILLVAL                         = 99;

    private final void _deleteFile(String filename)
    {
        File file = null;
        try {
            file = new File(filename);
        }
        catch (Throwable err) {
        }

        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                e.printStackTrace();
            }
        }
        assertFalse("TestH5R._deleteFile file still exists ", file.exists());
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
        assertTrue("TestH5R._createDataset: ", did > 0);

        return did;
    }

    private final long _createGroup(long fid, String name)
    {
        long gid = HDF5Constants.H5I_INVALID_HID;
        try {
            gid = H5.H5Gcreate(fid, name, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                               HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate: " + err);
        }
        assertTrue("TestH5R._createGroup: ", gid > 0);

        return gid;
    }

    @Before
    public void createH5file() throws NullPointerException, HDF5Exception
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());

        try {
            H5fid  = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                  HDF5Constants.H5P_DEFAULT);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            H5gid  = _createGroup(H5fid, "Group1");
            H5did2 = _createDataset(H5gid, H5dsid, "dset2", HDF5Constants.H5P_DEFAULT);
            H5did  = _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

            // Initialize the dataset.
            for (int indx = 0; indx < DIM_X; indx++)
                for (int jndx = 0; jndx < DIM_Y; jndx++)
                    dset_data[indx][jndx] = FILLVAL;

            try {
                if (H5did >= 0)
                    H5.H5Dwrite(H5did, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL,
                                HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, dset_data[0]);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5R.createH5file: " + err);
        }
        assertTrue("TestH5R.createH5file: H5.H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5R.createH5file: H5.H5Screate_simple: ", H5dsid > 0);
        assertTrue("TestH5R.createH5file: _createDataset: ", H5did > 0);

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
        if (H5gid > 0)
            try {
                H5.H5Gclose(H5gid);
            }
            catch (Exception ex) {
            }
        if (H5did2 > 0)
            try {
                H5.H5Dclose(H5did2);
            }
            catch (Exception ex) {
            }

        _deleteFile(H5_FILE);
        System.out.println();
    }

    // Test v1.12 APIs params

    @Test
    public void testH5Rget_object()
    {
        int ref_type   = HDF5Constants.H5R_OBJECT2;
        long ret_val   = -1;
        byte[] ref     = null;
        String name    = "";
        String objName = "/dset";

        try {
            ref = H5.H5Rcreate_object(H5fid, objName, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: H5Rcreate_object " + err);
        }

        try {
            ret_val = H5.H5Rget_type(ref);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: H5Rget_type: " + err);
        }
        assertTrue("testH5Rget_object: H5Rget_type", ret_val == ref_type);

        try {
            name = H5.H5Rget_file_name(ref);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: H5Rget_file_name: " + err);
        }
        assertTrue("testH5Rget_object: H5Rget_file_name", H5_FILE.equals(name));

        try {
            name = H5.H5Rget_obj_name(ref, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: H5Rget_obj_name: " + err);
        }
        assertTrue("The name of the object: ", objName.equals(name));
        H5.H5Rdestroy(ref);
    }

    @Test
    public void testH5Rget_obj_type3()
    {
        int obj_type   = -1;
        byte[] ref     = null;
        String objName = "/dset";

        try {
            ref = H5.H5Rcreate_object(H5fid, objName, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_obj_type3: H5Rcreate_object " + err);
        }

        try {
            obj_type = H5.H5Rget_obj_type3(ref, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_obj_type3: H5.H5Rget_obj_type3: " + err);
        }
        assertEquals(obj_type, HDF5Constants.H5O_TYPE_DATASET);
        H5.H5Rdestroy(ref);
    }

    @Test
    public void testH5Rcreate_regionref_object()
    {
        byte[] ref     = null;
        String objName = "/dset";
        long start[]   = {2, 2}; // Starting location of hyperslab
        long stride[]  = {1, 1}; // Stride of hyperslab
        long count[]   = {1, 1}; // Element count of hyperslab
        long block[]   = {3, 3}; // Block size of hyperslab

        // Select 3x3 hyperslab for reference
        try {
            H5.H5Sselect_hyperslab(H5dsid, HDF5Constants.H5S_SELECT_SET, start, stride, count, block);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: H5Sselect_hyperslab " + err);
        }
        try {
            ref = H5.H5Rcreate_region(H5fid, objName, H5dsid, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: H5Rcreate_region " + err);
        }
        assertNotNull(ref);
        H5.H5Rdestroy(ref);
    }

    // These tests need to be updated with new APIs
    //    @Test//
    //    public void testH5Rget_group() {
    //        long loc_id = H5fid;
    //        int ref_type = HDF5Constants.H5R_OBJECT2;
    //        long ret_val = -1;
    //        byte[] ref = null;
    //        String name = "";
    //        String objName = "/dset";
    //
    //        try {
    //            ref = H5.H5Rcreate_object(H5fid, objName, HDF5Constants.H5P_DEFAULT);
    //        }
    //        catch (Throwable err) {
    //            err.printStackTrace();
    //            fail("testH5Rget_object: H5Rcreate_object " + err);
    //        }
    //        try {
    //            dataset_id= H5.H5Rdereference(H5fid, HDF5Constants.H5P_DEFAULT,
    //            HDF5Constants.H5R_DATASET_REGION, ref1);
    //
    //            //Create reference on group
    //            ref2 = H5.H5Rcreate(H5gid, "/Group1", HDF5Constants.H5R_OBJECT, -1);
    //            group_id= H5.H5Rdereference(H5gid, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_OBJECT,
    //            ref2); assertNotNull(ref1); assertNotNull(ref2); assertTrue(dataset_id >= 0);
    //            assertTrue(group_id >= 0);
    //        }
    //        catch (Throwable err) {
    //            err.printStackTrace();
    //            fail("TestH5Rdereference " + err);
    //        }
    //        finally {
    //            try {H5.H5Dclose(dataset_id);} catch (Exception ex) {}
    //            try {H5.H5Gclose(group_id);} catch (Exception ex) {}
    //        }
    //    }

    //    @Test//
    //    public void testH5Rget_region_dataset() {
    //        long loc_id = H5fid;
    //        int ref_type = HDF5Constants.H5R_OBJECT2;
    //        long ret_val = -1;
    //        byte[] ref = null;
    //        String name = "";
    //        String objName = "/dset";
    //
    //        try {
    //            ref = H5.H5Rcreate_object(H5fid, objName, HDF5Constants.H5P_DEFAULT);
    //        }
    //        catch (Throwable err) {
    //            err.printStackTrace();
    //            fail("testH5Rget_object: H5Rcreate_object " + err);
    //        }
    //        try {
    //            dsid = H5.H5Rget_region(H5fid, HDF5Constants.H5R_DATASET_REGION, ref);
    //            assertNotNull(ref);
    //            assertTrue(dsid >= 0);
    //        }
    //        catch (Throwable err) {
    //            err.printStackTrace();
    //            fail("TestH5Rget_region: " + err);
    //        }
    //        finally {
    //            try {H5.H5Sclose(dsid);} catch (Exception ex) {}
    //        }
    //    }

    //    @Test//
    //    public void testH5Rget_attr() {
    //        long loc_id = H5fid;
    //        int ref_type = HDF5Constants.H5R_OBJECT2;
    //        long ret_val = -1;
    //        byte[] ref = null;
    //        String name = "";
    //        String objName = "/dset";
    //
    //        try {
    //            ref = H5.H5Rcreate_object(H5fid, objName, HDF5Constants.H5P_DEFAULT);
    //        }
    //        catch (Throwable err) {
    //            err.printStackTrace();
    //            fail("testH5Rget_object: H5Rcreate_object " + err);
    //        }
    //        try {
    //            dsid = H5.H5Rget_region(H5fid, HDF5Constants.H5R_DATASET_REGION, ref);
    //            assertNotNull(ref);
    //            assertTrue(dsid >= 0);
    //        }
    //        catch (Throwable err) {
    //            err.printStackTrace();
    //            fail("TestH5Rget_region: " + err);
    //        }
    //        finally {
    //            try {H5.H5Sclose(dsid);} catch (Exception ex) {}
    //        }
    //    }

    // Test parameters to H5Rcreate_object
    @Test(expected = NullPointerException.class)
    public void testH5Rcreate_object_Nullname() throws Throwable
    {
        String name = null;
        H5.H5Rcreate_object(H5fid, name, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Rget_name_Invalidloc() throws Throwable
    {
        String name = "";
        H5.H5Rcreate_object(-1, name, HDF5Constants.H5P_DEFAULT);
    }

    // Test parameters to H5Rcreate_region
    @Test(expected = NullPointerException.class)
    public void testH5Rcreate_region_Nullname() throws Throwable
    {
        String name = null;
        H5.H5Rcreate_region(H5fid, name, -1, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Rcreate_region_Invalidloc() throws Throwable
    {
        String name = "";
        H5.H5Rcreate_region(-1, name, -1, HDF5Constants.H5P_DEFAULT);
    }

    // Test parameters to H5Rcreate_attr
    @Test(expected = NullPointerException.class)
    public void testH5Rcreate_attr_Nullname() throws Throwable
    {
        String name     = null;
        String attrname = "";
        H5.H5Rcreate_attr(H5fid, name, attrname, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Rcreate_attr_Nullattrname() throws Throwable
    {
        String name     = "";
        String attrname = null;
        H5.H5Rcreate_attr(H5fid, name, attrname, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Rcreate_attr_Invalidloc() throws Throwable
    {
        String name     = "";
        String attrname = "";
        H5.H5Rcreate_attr(-1, name, attrname, HDF5Constants.H5P_DEFAULT);
    }

    // Test parameters to H5Rdestroy
    @Test(expected = NullPointerException.class)
    public void testH5Rdestroy_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Rdestroy(ref);
    }

    // Test parameters to H5Rget_type
    @Test(expected = NullPointerException.class)
    public void testH5Rget_type_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Rget_type(ref);
    }

    // Test parameters to H5Requal
    @Test(expected = NullPointerException.class)
    public void testH5Requal_Nullref1() throws Throwable
    {
        byte[] ref1 = null;
        byte[] ref2 = {0, 0, 0, 0};
        H5.H5Requal(ref1, ref2);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Requal_Nullref2() throws Throwable
    {
        byte[] ref1 = {0, 0, 0, 0};
        byte[] ref2 = null;
        H5.H5Requal(ref1, ref2);
    }

    // Test parameters to H5Rcopy
    @Test(expected = NullPointerException.class)
    public void testH5Rcopy_Nullref1() throws Throwable
    {
        byte[] ref1 = null;
        byte[] ref2 = H5.H5Rcopy(ref1);
    }

    // Test parameters to H5Ropen_object
    @Test(expected = NullPointerException.class)
    public void testH5Ropen_object_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Ropen_object(ref, -1, -1);
    }

    // Test parameters to H5Ropen_region
    @Test(expected = NullPointerException.class)
    public void testH5Ropen_region_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Ropen_region(ref, -1, -1);
    }

    // Test parameters to H5Ropen_attr
    @Test(expected = NullPointerException.class)
    public void testH5Ropen_attr_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Ropen_attr(ref, -1, -1);
    }

    // Test parameters to H5Rget_obj_type3
    @Test(expected = NullPointerException.class)
    public void testH5Rget_obj_type3_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Rget_obj_type3(ref, -1);
    }

    // Test parameters to H5Rget_file_name
    @Test(expected = NullPointerException.class)
    public void testH5Rget_file_name_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Rget_file_name(ref);
    }

    // Test parameters to H5Rget_obj_name
    @Test(expected = NullPointerException.class)
    public void testH5Rget_obj_name_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Rget_obj_name(ref, -1);
    }

    // Test parameters to H5Rget_attr_name
    @Test(expected = NullPointerException.class)
    public void testH5Rget_attr_name_Nullref() throws Throwable
    {
        byte[] ref = null;
        H5.H5Rget_attr_name(ref);
    }

    @Test
    public void testH5RVLattr_ref()
    {
        String attr_obj_name = "VLObjRefdata";
        String attr_reg_name = "VLRegRefdata";
        long attr_obj_id     = HDF5Constants.H5I_INVALID_HID;
        long attr_reg_id     = HDF5Constants.H5I_INVALID_HID;
        long atype_obj_id    = HDF5Constants.H5I_INVALID_HID;
        long atype_reg_id    = HDF5Constants.H5I_INVALID_HID;
        long aspace_id       = HDF5Constants.H5I_INVALID_HID;
        long[] dims          = {4};
        long lsize           = 1;
        byte[] ref1          = null;
        byte[] ref2          = null;
        byte[] ref3          = null;
        byte[] ref4          = null;

        try {
            // Create reference on dataset
            ref1 = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid);
            assertNotNull(ref1);
            ref2 = H5.H5Rcreate(H5gid, "dset2", HDF5Constants.H5R_DATASET_REGION, H5dsid);
            assertNotNull(ref2);
            ref3 = H5.H5Rcreate(H5gid, "/dset", HDF5Constants.H5R_OBJECT, -1);
            assertNotNull(ref3);

            // Create reference on group
            ref4 = H5.H5Rcreate(H5gid, "/Group1", HDF5Constants.H5R_OBJECT, -1);
            assertNotNull(ref3);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5RVLattr_ref: " + err);
        }

        ArrayList[] vl_obj_data = new ArrayList[4];
        ArrayList[] vl_reg_data = new ArrayList[4];
        try {
            // Write Object Reference data
            vl_obj_data[0]  = new ArrayList<byte[]>(Arrays.asList(ref3));
            vl_obj_data[1]  = new ArrayList<byte[]>(Arrays.asList(ref3, ref4));
            vl_obj_data[2]  = new ArrayList<byte[]>(Arrays.asList(ref3, ref3, ref3));
            vl_obj_data[3]  = new ArrayList<byte[]>(Arrays.asList(ref4, ref4, ref4, ref4));
            Class dataClass = vl_obj_data.getClass();
            assertTrue("testH5RVLattr_ref.getClass: " + dataClass, dataClass.isArray());

            try {
                atype_obj_id = H5.H5Tvlen_create(HDF5Constants.H5T_STD_REF_OBJ);
                assertTrue("testH5RVLattr_ref.H5Tvlen_create: ", atype_obj_id >= 0);
            }
            catch (Exception err) {
                if (atype_obj_id > 0)
                    try {
                        H5.H5Tclose(atype_obj_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5RVLattr_ref: " + err);
            }

            try {
                aspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(aspace_id > 0);
                attr_obj_id = H5.H5Acreate(H5did, attr_obj_name, atype_obj_id, aspace_id,
                                           HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5RVLattr_ref: ", attr_obj_id >= 0);

                H5.H5Awrite(attr_obj_id, atype_obj_id, vl_obj_data);
            }
            catch (Exception err) {
                if (attr_obj_id > 0)
                    try {
                        H5.H5Aclose(attr_obj_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_obj_id > 0)
                    try {
                        H5.H5Tclose(atype_obj_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5RVLattr_ref: " + err);
            }
            finally {
                if (aspace_id > 0)
                    try {
                        H5.H5Sclose(aspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            // Write Region Reference data
            vl_reg_data[0] = new ArrayList<byte[]>(Arrays.asList(ref1));
            vl_reg_data[1] = new ArrayList<byte[]>(Arrays.asList(ref1, ref2));
            vl_reg_data[2] = new ArrayList<byte[]>(Arrays.asList(ref1, ref1, ref1));
            vl_reg_data[3] = new ArrayList<byte[]>(Arrays.asList(ref2, ref2, ref2, ref2));
            dataClass      = vl_reg_data.getClass();
            assertTrue("testH5RVLattr_ref.getClass: " + dataClass, dataClass.isArray());

            try {
                atype_reg_id = H5.H5Tvlen_create(HDF5Constants.H5T_STD_REF_DSETREG);
                assertTrue("testH5RVLattr_ref.H5Tvlen_create: ", atype_reg_id >= 0);
            }
            catch (Exception err) {
                if (atype_reg_id > 0)
                    try {
                        H5.H5Tclose(atype_reg_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5RVLattr_ref: " + err);
            }

            try {
                aspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(aspace_id > 0);
                attr_reg_id = H5.H5Acreate(H5did, attr_reg_name, atype_reg_id, aspace_id,
                                           HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5RVLattr_ref: ", attr_reg_id >= 0);

                H5.H5Awrite(attr_reg_id, atype_reg_id, vl_reg_data);
            }
            catch (Exception err) {
                if (attr_reg_id > 0)
                    try {
                        H5.H5Aclose(attr_reg_id);
                    }
                    catch (Exception ex) {
                    }
                if (atype_reg_id > 0)
                    try {
                        H5.H5Tclose(atype_reg_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5RVLattr_ref: " + err);
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

            for (int j = 0; j < dims.length; j++) {
                lsize *= dims[j];
            }

            // Read Object Reference data
            ArrayList[] vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_readbuf[j] = new ArrayList<byte[]>();

            try {
                H5.H5Aread(attr_obj_id, atype_obj_id, vl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5RVLattr_ref:" + ((byte[])vl_readbuf[0].get(0))[0],
                       ((byte[])vl_obj_data[0].get(0))[0] == ((byte[])vl_readbuf[0].get(0))[0]);
            assertTrue("testH5RVLattr_ref:" + ((byte[])vl_readbuf[1].get(0))[0],
                       ((byte[])vl_obj_data[1].get(0))[0] == ((byte[])vl_readbuf[1].get(0))[0]);
            assertTrue("testH5RVLattr_ref:" + ((byte[])vl_readbuf[2].get(0))[0],
                       ((byte[])vl_obj_data[2].get(0))[0] == ((byte[])vl_readbuf[2].get(0))[0]);
            assertTrue("testH5RVLattr_ref:" + ((byte[])vl_readbuf[3].get(0))[0],
                       ((byte[])vl_obj_data[3].get(0))[0] == ((byte[])vl_readbuf[3].get(0))[0]);

            // Read Region Reference data
            vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_readbuf[j] = new ArrayList<byte[]>();

            try {
                H5.H5Aread(attr_reg_id, atype_reg_id, vl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5RVLattr_ref:" + ((byte[])vl_readbuf[0].get(0))[0],
                       ((byte[])vl_reg_data[0].get(0))[0] == ((byte[])vl_readbuf[0].get(0))[0]);
            assertTrue("testH5RVLattr_ref:" + ((byte[])vl_readbuf[1].get(0))[0],
                       ((byte[])vl_reg_data[1].get(0))[0] == ((byte[])vl_readbuf[1].get(0))[0]);
            assertTrue("testH5RVLattr_ref:" + ((byte[])vl_readbuf[2].get(0))[0],
                       ((byte[])vl_reg_data[2].get(0))[0] == ((byte[])vl_readbuf[2].get(0))[0]);
            assertTrue("testH5RVLattr_ref:" + ((byte[])vl_readbuf[3].get(0))[0],
                       ((byte[])vl_reg_data[3].get(0))[0] == ((byte[])vl_readbuf[3].get(0))[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.testH5RVLattr_ref: " + err);
        }
        finally {
            if (attr_reg_id > 0)
                try {
                    H5.H5Aclose(attr_reg_id);
                }
                catch (Exception ex) {
                }
            if (attr_obj_id > 0)
                try {
                    H5.H5Aclose(attr_obj_id);
                }
                catch (Exception ex) {
                }
            if (atype_reg_id > 0)
                try {
                    H5.H5Tclose(atype_reg_id);
                }
                catch (Exception ex) {
                }
            if (atype_obj_id > 0)
                try {
                    H5.H5Tclose(atype_obj_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5RVLdset_ref()
    {
        String dset_obj_name = "VLObjRefdata";
        String dset_reg_name = "VLRegRefdata";
        long dset_obj_id     = HDF5Constants.H5I_INVALID_HID;
        long dset_reg_id     = HDF5Constants.H5I_INVALID_HID;
        long dtype_obj_id    = HDF5Constants.H5I_INVALID_HID;
        long dtype_reg_id    = HDF5Constants.H5I_INVALID_HID;
        long dspace_id       = HDF5Constants.H5I_INVALID_HID;
        long[] dims          = {4};
        long lsize           = 1;
        byte[] ref1          = null;
        byte[] ref2          = null;
        byte[] ref3          = null;
        byte[] ref4          = null;

        try {
            // Create reference on dataset
            ref1 = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid);
            assertNotNull(ref1);
            ref2 = H5.H5Rcreate(H5gid, "dset2", HDF5Constants.H5R_DATASET_REGION, H5dsid);
            assertNotNull(ref2);
            ref3 = H5.H5Rcreate(H5gid, "/dset", HDF5Constants.H5R_OBJECT, -1);
            assertNotNull(ref3);

            // Create reference on group
            ref4 = H5.H5Rcreate(H5gid, "/Group1", HDF5Constants.H5R_OBJECT, -1);
            assertNotNull(ref3);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5RVLattr_ref: " + err);
        }

        ArrayList[] vl_obj_data = new ArrayList[4];
        ArrayList[] vl_reg_data = new ArrayList[4];
        try {
            // Write Object Reference data
            vl_obj_data[0]  = new ArrayList<byte[]>(Arrays.asList(ref3));
            vl_obj_data[1]  = new ArrayList<byte[]>(Arrays.asList(ref3, ref4));
            vl_obj_data[2]  = new ArrayList<byte[]>(Arrays.asList(ref3, ref3, ref3));
            vl_obj_data[3]  = new ArrayList<byte[]>(Arrays.asList(ref4, ref4, ref4, ref4));
            Class dataClass = vl_obj_data.getClass();
            assertTrue("testH5RVLdset_ref.getClass: " + dataClass, dataClass.isArray());

            try {
                dtype_obj_id = H5.H5Tvlen_create(HDF5Constants.H5T_STD_REF_OBJ);
                assertTrue("testH5RVLdset_ref.H5Tvlen_create: ", dtype_obj_id >= 0);
            }
            catch (Exception err) {
                if (dtype_obj_id > 0)
                    try {
                        H5.H5Tclose(dtype_obj_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5RVLdset_ref: " + err);
            }

            try {
                dspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(dspace_id > 0);
                dset_obj_id =
                    H5.H5Dcreate(H5fid, dset_obj_name, dtype_obj_id, dspace_id, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5RVLdset_ref: ", dset_obj_id >= 0);

                H5.H5Dwrite(dset_obj_id, dtype_obj_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5P_DEFAULT, vl_obj_data);
            }
            catch (Exception err) {
                if (dset_obj_id > 0)
                    try {
                        H5.H5Dclose(dset_obj_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_obj_id > 0)
                    try {
                        H5.H5Tclose(dtype_obj_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5RVLdset_ref: " + err);
            }
            finally {
                if (dspace_id > 0)
                    try {
                        H5.H5Sclose(dspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            // Write Region Reference data
            vl_reg_data[0] = new ArrayList<byte[]>(Arrays.asList(ref1));
            vl_reg_data[1] = new ArrayList<byte[]>(Arrays.asList(ref1, ref2));
            vl_reg_data[2] = new ArrayList<byte[]>(Arrays.asList(ref1, ref1, ref1));
            vl_reg_data[3] = new ArrayList<byte[]>(Arrays.asList(ref2, ref2, ref2, ref2));
            dataClass      = vl_reg_data.getClass();
            assertTrue("testH5RVLdset_ref.getClass: " + dataClass, dataClass.isArray());

            try {
                dtype_reg_id = H5.H5Tvlen_create(HDF5Constants.H5T_STD_REF_DSETREG);
                assertTrue("testH5RVLdset_ref.H5Tvlen_create: ", dtype_reg_id >= 0);
            }
            catch (Exception err) {
                if (dtype_reg_id > 0)
                    try {
                        H5.H5Tclose(dtype_reg_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5RVLdset_ref: " + err);
            }

            try {
                dspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(dspace_id > 0);
                dset_reg_id =
                    H5.H5Dcreate(H5fid, dset_reg_name, dtype_reg_id, dspace_id, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5RVLdset_ref: ", dset_reg_id >= 0);

                H5.H5Dwrite(dset_reg_id, dtype_reg_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5P_DEFAULT, vl_reg_data);
            }
            catch (Exception err) {
                if (dset_reg_id > 0)
                    try {
                        H5.H5Dclose(dset_reg_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_reg_id > 0)
                    try {
                        H5.H5Tclose(dtype_reg_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5RVLdset_ref: " + err);
            }
            finally {
                if (dspace_id > 0)
                    try {
                        H5.H5Sclose(dspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++) {
                lsize *= dims[j];
            }

            // Read Object Reference data
            ArrayList[] vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_readbuf[j] = new ArrayList<byte[]>();

            try {
                H5.H5Dread(dset_obj_id, dtype_obj_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                           HDF5Constants.H5P_DEFAULT, vl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5RVLdset_ref:" + ((byte[])vl_readbuf[0].get(0))[0],
                       ((byte[])vl_obj_data[0].get(0))[0] == ((byte[])vl_readbuf[0].get(0))[0]);
            assertTrue("testH5RVLdset_ref:" + ((byte[])vl_readbuf[1].get(0))[0],
                       ((byte[])vl_obj_data[1].get(0))[0] == ((byte[])vl_readbuf[1].get(0))[0]);
            assertTrue("testH5RVLdset_ref:" + ((byte[])vl_readbuf[2].get(0))[0],
                       ((byte[])vl_obj_data[2].get(0))[0] == ((byte[])vl_readbuf[2].get(0))[0]);
            assertTrue("testH5RVLdset_ref:" + ((byte[])vl_readbuf[3].get(0))[0],
                       ((byte[])vl_obj_data[3].get(0))[0] == ((byte[])vl_readbuf[3].get(0))[0]);

            // Read Region Reference data
            vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_readbuf[j] = new ArrayList<byte[]>();

            try {
                H5.H5Dread(dset_reg_id, dtype_reg_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                           HDF5Constants.H5P_DEFAULT, vl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5RVLdset_ref:" + ((byte[])vl_readbuf[0].get(0))[0],
                       ((byte[])vl_reg_data[0].get(0))[0] == ((byte[])vl_readbuf[0].get(0))[0]);
            assertTrue("testH5RVLdset_ref:" + ((byte[])vl_readbuf[1].get(0))[0],
                       ((byte[])vl_reg_data[1].get(0))[0] == ((byte[])vl_readbuf[1].get(0))[0]);
            assertTrue("testH5RVLdset_ref:" + ((byte[])vl_readbuf[2].get(0))[0],
                       ((byte[])vl_reg_data[2].get(0))[0] == ((byte[])vl_readbuf[2].get(0))[0]);
            assertTrue("testH5RVLdset_ref:" + ((byte[])vl_readbuf[3].get(0))[0],
                       ((byte[])vl_reg_data[3].get(0))[0] == ((byte[])vl_readbuf[3].get(0))[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.testH5RVLdset_ref: " + err);
        }
        finally {
            if (dset_reg_id > 0)
                try {
                    H5.H5Dclose(dset_reg_id);
                }
                catch (Exception ex) {
                }
            if (dset_obj_id > 0)
                try {
                    H5.H5Dclose(dset_obj_id);
                }
                catch (Exception ex) {
                }
            if (dtype_reg_id > 0)
                try {
                    H5.H5Tclose(dtype_reg_id);
                }
                catch (Exception ex) {
                }
            if (dtype_obj_id > 0)
                try {
                    H5.H5Tclose(dtype_obj_id);
                }
                catch (Exception ex) {
                }
        }
    }
}
