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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.exceptions.HDF5FunctionArgumentException;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5R {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "testH5R.h5";
    private static final int DIM_X = 4;
    private static final int DIM_Y = 6;
    long H5fid = -1;
    long H5dsid = -1;
    long H5did = -1;
    long H5gid = -1;
    long H5did2 = -1;
    long[] H5dims = { DIM_X, DIM_Y };
    int[][] dset_data = new int[DIM_X][DIM_Y];
    int FILLVAL = 99;

    private final void _deleteFile(String filename) {
        File file = null;
        try {
            file = new File(filename);
        }
        catch (Throwable err) {}

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {e.printStackTrace();}
        }
        assertFalse("TestH5R._deleteFile file still exists ", file.exists());
    }

    private final long _createDataset(long fid, long dsid, String name, long dapl) {
        long did = -1;
        try {
            did = H5.H5Dcreate(fid, name,
                        HDF5Constants.H5T_STD_I32BE, dsid,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5R._createDataset: ", did > 0);

        return did;
    }

    private final long _createGroup(long fid, String name) {
        long gid = -1;
        try {
            gid = H5.H5Gcreate(fid, name, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate: " + err);
        }
        assertTrue("TestH5R._createGroup: ",gid > 0);

        return gid;
    }

    @Before
    public void createH5file()
            throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        try {
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            H5gid = _createGroup(H5fid, "Group1");
            H5did2 = _createDataset(H5gid, H5dsid, "dset2", HDF5Constants.H5P_DEFAULT);
            H5did = _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

            // Initialize the dataset.
            for (int indx = 0; indx < DIM_X; indx++)
                for (int jndx = 0; jndx < DIM_Y; jndx++)
                    dset_data[indx][jndx] = FILLVAL;

            try {
                if (H5did >= 0)
                    H5.H5Dwrite(H5did, HDF5Constants.H5T_NATIVE_INT,
                            HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5P_DEFAULT, dset_data[0]);
            }
            catch (Exception e) {
                e.printStackTrace();
            }

        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5R.createH5file: " + err);
        }
        assertTrue("TestH5R.createH5file: H5.H5Fcreate: ",H5fid > 0);
        assertTrue("TestH5R.createH5file: H5.H5Screate_simple: ",H5dsid > 0);
        assertTrue("TestH5R.createH5file: _createDataset: ",H5did > 0);

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5dsid > 0)
            try {H5.H5Sclose(H5dsid);} catch (Exception ex) {}
        if (H5did > 0)
            try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        if (H5fid > 0)
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
        if (H5gid > 0)
            try {H5.H5Gclose(H5gid);} catch (Exception ex) {}
        if (H5did2 > 0)
            try {H5.H5Dclose(H5did2);} catch (Exception ex) {}

        _deleteFile(H5_FILE);
        System.out.println();
    }

    // Test v1.8 APIs params
    /*
     * @Ignore public void testH5Rget_name() { long loc_id = H5fid; int ref_type =
     * HDF5Constants.H5R_OBJECT; long ret_val = -1; byte[] ref = null; String[] name = {""}; String
     * objName = "/dset";
     *
     * try { ref = H5.H5Rcreate(H5fid, objName, ref_type, -1); } catch (Throwable err) {
     * err.printStackTrace(); fail("H5.H5Rget_name:H5Rcreate " + err); }
     *
     * try { ret_val = H5.H5Rget_name(loc_id, ref_type, ref, name, 16); } catch (Throwable err) {
     * err.printStackTrace(); fail("H5.H5Rget_name: " + err); }
     *
     * assertTrue("testH5Rget_name: H5Rget_name", ret_val>0); assertTrue("The name of the object: ",
     * objName.equals(name[0])); }
     *
     * @Ignore public void testH5Rget_obj_type2() { int ref_type=HDF5Constants.H5R_OBJECT; byte[]
     * ref=null;
     *
     * String objName = "/dset"; int obj_type = -1;
     *
     * try { ref = H5.H5Rcreate(H5fid, objName, ref_type, -1); } catch(Throwable err) {
     * err.printStackTrace(); }
     *
     * try { obj_type = H5.H5Rget_obj_type(H5fid, HDF5Constants.H5R_OBJECT, ref); } catch (Throwable
     * err) { err.printStackTrace(); fail("H5.H5Rget_obj_type2: " + err); } assertEquals(obj_type,
     * HDF5Constants.H5O_TYPE_DATASET); }
     *
     * @Ignore public void testH5Rcreate_refobj() { byte[] ref = null;
     *
     * try { ref = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_OBJECT, -1); } catch (Throwable err) {
     * err.printStackTrace(); fail("H5.H5Rcreate: " + err); } assertNotNull(ref); }
     *
     * @Ignore public void testH5Rcreate_regionrefobj() { byte[] ref = null; try { ref =
     * H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid); } catch (Throwable err) {
     * err.printStackTrace(); fail("H5.H5Rcreate: " + err); } assertNotNull(ref); }
     *
     * @Ignore public void testH5Rdereference() { byte[] ref1 = null; byte[] ref2 = null; long
     * dataset_id = -1; long group_id = -1; try { //Create reference on dataset ref1 =
     * H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid); dataset_id=
     * H5.H5Rdereference(H5fid, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_DATASET_REGION, ref1);
     *
     * //Create reference on group ref2 = H5.H5Rcreate(H5gid, "/Group1", HDF5Constants.H5R_OBJECT, -1);
     * group_id= H5.H5Rdereference(H5gid, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_OBJECT, ref2);
     * assertNotNull(ref1); assertNotNull(ref2); assertTrue(dataset_id >= 0); assertTrue(group_id >= 0);
     * } catch (Throwable err) { err.printStackTrace(); fail("TestH5Rdereference " + err); } finally {
     * try {H5.H5Dclose(dataset_id);} catch (Exception ex) {} try {H5.H5Gclose(group_id);} catch
     * (Exception ex) {} } }
     *
     * @Ignore public void testH5Rget_region() { byte[] ref = null; long dsid = -1; try { ref =
     * H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid); dsid =
     * H5.H5Rget_region(H5fid, HDF5Constants.H5R_DATASET_REGION, ref); assertNotNull(ref);
     * assertTrue(dsid >= 0); } catch (Throwable err) { err.printStackTrace();
     * fail("TestH5Rget_region: " + err); } finally { try {H5.H5Sclose(dsid);} catch (Exception ex) {} }
     * }
     *
     * @Ignore//(expected = IllegalArgumentException.class) public void testH5Rget_name_Invalidreftype()
     * throws Throwable { byte[] ref = null; String[] name = {""}; ref = H5.H5Rcreate(H5fid, "/dset",
     * HDF5Constants.H5R_OBJECT, -1); H5.H5Rget_name(H5fid, HDF5Constants.H5R_DATASET_REGION, ref, name,
     * 16); }
     *
     * @Ignore//(expected = NullPointerException.class) public void testH5Rget_name_NULLreference()
     * throws Throwable { byte[] ref = null; String[] name = {""}; H5.H5Rget_name(H5fid,
     * HDF5Constants.H5R_OBJECT, ref, name, 16); }
     *
     * @Ignore//(expected = HDF5LibraryException.class) public void
     * testH5Rget_obj_type2_Invalidreftype() throws Throwable { byte[] ref = null; ref =
     * H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_OBJECT, -1); H5.H5Rget_obj_type(H5fid,
     * HDF5Constants.H5R_DATASET_REGION, ref); }
     *
     * @Ignore//(expected = HDF5LibraryException.class) public void testH5Rcreate_InvalidObjectName()
     * throws Throwable { byte[] ref=H5.H5Rcreate(H5fid, "/GROUPS", HDF5Constants.H5R_OBJECT, -1); }
     *
     * @Ignore//(expected = HDF5LibraryException.class) public void testH5Rcreate_Invalidspace_id()
     * throws Throwable { byte[] ref=H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, -1);
     * }
     *
     * @Ignore//(expected = IllegalArgumentException.class) public void testH5Rcreate_Invalidreftype()
     * throws Throwable { byte[] ref=H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_BADTYPE, -1); }
     *
     * @Ignore//(expected = IllegalArgumentException.class) public void
     * testH5Rgetregion_Invalidreftype() throws Throwable { byte[] ref = null; ref = H5.H5Rcreate(H5fid,
     * "/dset", HDF5Constants.H5R_OBJECT, H5dsid); H5.H5Rget_region(H5fid,
     * HDF5Constants.H5R_DATASET_REGION, ref); }
     *
     * @Ignore//(expected = IllegalArgumentException.class) public void
     * testH5Rgetregion_Badreferencetype() throws Throwable { byte[] ref = null; ref =
     * H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_OBJECT, H5dsid); H5.H5Rget_region(H5fid,
     * HDF5Constants.H5R_OBJECT, ref); }
     *
     * @Ignore//(expected = NullPointerException.class) public void testH5Rgetregion_Nullreference()
     * throws Throwable { byte[] ref = null; H5.H5Rget_region(H5fid, HDF5Constants.H5R_DATASET_REGION,
     * ref); }
     *
     * @Ignore//(expected = NullPointerException.class) public void testH5Rdereference_Nullreference()
     * throws Throwable { byte[] ref = null; H5.H5Rdereference(H5did2, HDF5Constants.H5P_DEFAULT,
     * HDF5Constants.H5R_OBJECT, ref); }
     *
     * @Ignore//(expected = IllegalArgumentException.class) public void
     * testH5Rdereference_Invalidreference() throws Throwable { byte[] ref1 = null; byte[] ref2 = null;
     * ref1 = H5.H5Rcreate(H5fid, "/dset", HDF5Constants.H5R_DATASET_REGION, H5dsid); ref2 =
     * H5.H5Rcreate(H5gid, "/Group1", HDF5Constants.H5R_OBJECT, -1); H5.H5Rdereference(H5gid,
     * HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_OBJECT, ref1); }
     */
    // Test v1.12 APIs params

    @Test
    public void testH5Rget_object() {
        int ref_type = HDF5Constants.H5R_OBJECT2;
        long ret_val = -1;
        byte[] ref = null;
        String name = "";
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
    public void testH5Rget_obj_type3() {
        int obj_type = -1;
        byte[] ref = null;
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
    public void testH5Rcreate_regionref_object() {
        byte[] ref = null;
        String objName = "/dset";
        long start[] = {2,2};     // Starting location of hyperslab
        long stride[] = {1,1};    // Stride of hyperslab
        long count[] = {1,1};     // Element count of hyperslab
        long block[] = {3,3};     // Block size of hyperslab

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
//            dataset_id= H5.H5Rdereference(H5fid, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_DATASET_REGION, ref1);
//
//            //Create reference on group
//            ref2 = H5.H5Rcreate(H5gid, "/Group1", HDF5Constants.H5R_OBJECT, -1);
//            group_id= H5.H5Rdereference(H5gid, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5R_OBJECT, ref2);
//            assertNotNull(ref1);
//            assertNotNull(ref2);
//            assertTrue(dataset_id >= 0);
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
    public void testH5Rcreate_object_Nullname() throws Throwable {
        String name = null;
        H5.H5Rcreate_object(H5fid, name, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Rget_name_Invalidloc() throws Throwable {
        String name= "";
        H5.H5Rcreate_object(-1, name, HDF5Constants.H5P_DEFAULT);
    }

    // Test parameters to H5Rcreate_region
    @Test(expected = NullPointerException.class)
    public void testH5Rcreate_region_Nullname() throws Throwable {
        String name = null;
        H5.H5Rcreate_region(H5fid, name, -1, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Rcreate_region_Invalidloc() throws Throwable {
        String name= "";
        H5.H5Rcreate_region(-1, name, -1, HDF5Constants.H5P_DEFAULT);
    }

    // Test parameters to H5Rcreate_attr
    @Test(expected = NullPointerException.class)
    public void testH5Rcreate_attr_Nullname() throws Throwable {
        String name = null;
        String attrname = "";
        H5.H5Rcreate_attr(H5fid, name, attrname, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Rcreate_attr_Nullattrname() throws Throwable {
        String name = "";
        String attrname = null;
        H5.H5Rcreate_attr(H5fid, name, attrname, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Rcreate_attr_Invalidloc() throws Throwable {
        String name= "";
        String attrname= "";
        H5.H5Rcreate_attr(-1, name, attrname, HDF5Constants.H5P_DEFAULT);
    }

    // Test parameters to H5Rdestroy
    @Test(expected = NullPointerException.class)
    public void testH5Rdestroy_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Rdestroy(ref);
    }

    // Test parameters to H5Rget_type
    @Test(expected = NullPointerException.class)
    public void testH5Rget_type_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Rget_type(ref);
    }

    // Test parameters to H5Requal
    @Test(expected = NullPointerException.class)
    public void testH5Requal_Nullref1() throws Throwable {
        byte[] ref1 = null;
        byte[] ref2 = {0,0,0,0};
        H5.H5Requal(ref1, ref2);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Requal_Nullref2() throws Throwable {
        byte[] ref1 = {0,0,0,0};
        byte[] ref2 = null;
        H5.H5Requal(ref1, ref2);
    }

    // Test parameters to H5Rcopy
    @Test(expected = NullPointerException.class)
    public void testH5Rcopy_Nullref1() throws Throwable {
        byte[] ref1 = null;
        byte[] ref2 = H5.H5Rcopy(ref1);
    }

    // Test parameters to H5Ropen_object
    @Test(expected = NullPointerException.class)
    public void testH5Ropen_object_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Ropen_object(ref, -1, -1);
    }

    // Test parameters to H5Ropen_region
    @Test(expected = NullPointerException.class)
    public void testH5Ropen_region_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Ropen_region(ref, -1, -1);
    }

    // Test parameters to H5Ropen_attr
    @Test(expected = NullPointerException.class)
    public void testH5Ropen_attr_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Ropen_attr(ref, -1, -1);
    }

    // Test parameters to H5Rget_obj_type3
    @Test(expected = NullPointerException.class)
    public void testH5Rget_obj_type3_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Rget_obj_type3(ref, -1);
    }

    // Test parameters to H5Rget_file_name
    @Test(expected = NullPointerException.class)
    public void testH5Rget_file_name_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Rget_file_name(ref);
    }

    // Test parameters to H5Rget_obj_name
    @Test(expected = NullPointerException.class)
    public void testH5Rget_obj_name_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Rget_obj_name(ref, -1);
    }

    // Test parameters to H5Rget_attr_name
    @Test(expected = NullPointerException.class)
    public void testH5Rget_attr_name_Nullref() throws Throwable {
        byte[] ref = null;
        H5.H5Rget_attr_name(ref);
    }

}
