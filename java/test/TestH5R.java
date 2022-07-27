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

    @Test
    public void testH5Rget_name()
    {
        long loc_id    = H5fid;
        int ref_type   = HDF5Constants.H5R_OBJECT;
        long ret_val   = -1;
        byte[] ref     = null;
        String[] name  = {""};
        String objName = "/dset";

        try {
            ref = H5.H5Rcreate(H5fid, objName, ref_type, -1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Rget_name:H5Rcreate " + err);
        }

        try {
            ret_val = H5.H5Rget_name(loc_id, ref_type, ref, name, 16);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Rget_name: " + err);
        }

        assertTrue("testH5Rget_name: H5Rget_name", ret_val > 0);
        assertTrue("The name of the object: ", objName.equals(name[0]));
    }

    @Test
    public void testH5Rget_obj_type2()
    {
        int ref_type   = HDF5Constants.H5R_OBJECT;
        byte[] ref     = null;
        String objName = "/dset";
        int obj_type   = -1;
        ;

        try {
            ref = H5.H5Rcreate(H5fid, objName, ref_type, -1);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }

        try {
            obj_type = H5.H5Rget_obj_type(H5fid, HDF5Constants.H5R_OBJECT, ref);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Rget_obj_type2: " + err);
        }
        assertEquals(obj_type, HDF5Constants.H5O_TYPE_DATASET);
    }

    @Test
    public void testH5Rcreate_refobj()
    {
        byte[] ref = null;

        try {
            ref = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_OBJECT, -1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Rcreate: " + err);
        }
        assertNotNull(ref);
    }

    @Test
    public void testH5Rcreate_regionrefobj()
    {
        byte[] ref = null;
        try {
            ref = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Rcreate: " + err);
        }
        assertNotNull(ref);
    }

    @Test
    public void testH5Rdereference()
    {
        byte[] ref1     = null;
        byte[] ref2     = null;
        long dataset_id = HDF5Constants.H5I_INVALID_HID;
        long group_id   = HDF5Constants.H5I_INVALID_HID;
        try {
            // Create reference on dataset
            ref1 = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid);
            dataset_id =
                H5.H5Rdereference(H5fid, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_DATASET_REGION, ref1);

            // Create reference on group
            ref2     = H5.H5Rcreate(H5gid, "/Group1", HDF5Constants.H5R_OBJECT, -1);
            group_id = H5.H5Rdereference(H5gid, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_OBJECT, ref2);
            assertNotNull(ref1);
            assertNotNull(ref2);
            assertTrue(dataset_id >= 0);
            assertTrue(group_id >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Rdereference " + err);
        }
        finally {
            try {
                H5.H5Dclose(dataset_id);
            }
            catch (Exception ex) {
            }
            try {
                H5.H5Gclose(group_id);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5Rget_region()
    {
        byte[] ref = null;
        long dsid  = HDF5Constants.H5I_INVALID_HID;
        try {
            ref  = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid);
            dsid = H5.H5Rget_region(H5fid, HDF5Constants.H5R_DATASET_REGION, ref);
            assertNotNull(ref);
            assertTrue(dsid >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Rget_region: " + err);
        }
        finally {
            try {
                H5.H5Sclose(dsid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Rget_name_Invalidreftype() throws Throwable
    {
        byte[] ref    = null;
        String[] name = {""};
        ref           = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_OBJECT, -1);
        H5.H5Rget_name(H5fid, HDF5Constants.H5R_DATASET_REGION, ref, name, 16);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Rget_name_NULLreference() throws Throwable
    {
        byte[] ref    = null;
        String[] name = {""};
        H5.H5Rget_name(H5fid, HDF5Constants.H5R_OBJECT, ref, name, 16);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Rget_obj_type2_Invalidreftype() throws Throwable
    {
        byte[] ref = null;
        ref        = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_OBJECT, -1);
        H5.H5Rget_obj_type(H5fid, HDF5Constants.H5R_DATASET_REGION, ref);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Rcreate_InvalidObjectName() throws Throwable
    {
        H5.H5Rcreate(H5fid, "/GROUPS", HDF5Constants.H5R_OBJECT, -1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Rcreate_Invalidspace_id() throws Throwable
    {
        H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, -1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Rcreate_Invalidreftype() throws Throwable
    {
        H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_BADTYPE, -1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Rgetregion_Invalidreftype() throws Throwable
    {
        byte[] ref = null;
        ref        = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_OBJECT, H5dsid);
        H5.H5Rget_region(H5fid, HDF5Constants.H5R_DATASET_REGION, ref);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Rgetregion_Badreferencetype() throws Throwable
    {
        byte[] ref = null;
        ref        = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_OBJECT, H5dsid);
        H5.H5Rget_region(H5fid, HDF5Constants.H5R_OBJECT, ref);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Rgetregion_Nullreference() throws Throwable
    {
        byte[] ref = null;
        H5.H5Rget_region(H5fid, HDF5Constants.H5R_DATASET_REGION, ref);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Rdereference_Nullreference() throws Throwable
    {
        byte[] ref = null;
        H5.H5Rdereference(H5did2, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_OBJECT, ref);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Rdereference_Invalidreference() throws Throwable
    {
        byte[] ref1 = null;
        byte[] ref2 = null;
        ref1        = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid);
        ref2        = H5.H5Rcreate(H5gid, "/Group1", HDF5Constants.H5R_OBJECT, -1);
        H5.H5Rdereference(H5gid, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_OBJECT, ref1);
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

                H5.H5AwriteVL(attr_obj_id, atype_obj_id, vl_obj_data);
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

                H5.H5AwriteVL(attr_reg_id, atype_reg_id, vl_reg_data);
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
                H5.H5AreadVL(attr_obj_id, atype_obj_id, vl_readbuf);
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
                H5.H5AreadVL(attr_reg_id, atype_reg_id, vl_readbuf);
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

                H5.H5DwriteVL(dset_obj_id, dtype_obj_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
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

                H5.H5DwriteVL(dset_reg_id, dtype_reg_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
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
                H5.H5DreadVL(dset_obj_id, dtype_obj_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
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
                H5.H5DreadVL(dset_reg_id, dtype_reg_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
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
