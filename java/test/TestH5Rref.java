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

public class TestH5Rref {
    @Rule
    public TestName testname                 = new TestName();
    private static final String H5_DREG_FILE = "trefer_reg.h5";
    private static final String H5_AREG_FILE = "trefer_attr.h5";
    long H5fid                               = HDF5Constants.H5I_INVALID_HID;
    long H5dsid                              = HDF5Constants.H5I_INVALID_HID;
    long H5did                               = HDF5Constants.H5I_INVALID_HID;

    private boolean byteArrayCheck(final byte[] array)
    {
        for (byte b : array) {
            if (b != 0) {
                return false;
            }
        }
        return true;
    }

    public void openH5file(String filename, String dsetname)
    {
        try {
            H5fid = H5.H5Fopen(filename, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5R._openH5file: " + err);
        }
        assertTrue("TestH5R._openH5file: H5.H5Fopen: ", H5fid >= 0);
        try {
            H5did = H5.H5Dopen(H5fid, dsetname, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5R._openH5file: " + err);
        }
        assertTrue("TestH5R._openH5file: H5.H5Dopen: ", H5did >= 0);
        try {
            H5dsid = H5.H5Dget_space(H5did);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5R._openH5file: " + err);
        }
        assertTrue("TestH5R._openH5file: H5.H5Screate_simple: ", H5dsid > 0);
    }

    @After
    public void closeH5file() throws HDF5LibraryException
    {
        if (H5did >= 0)
            try {
                H5.H5Dclose(H5did);
            }
            catch (Exception ex) {
            }
        if (H5dsid > 0)
            try {
                H5.H5Sclose(H5dsid);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                H5.H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }
        H5fid  = HDF5Constants.H5I_INVALID_HID;
        H5dsid = HDF5Constants.H5I_INVALID_HID;
        H5did  = HDF5Constants.H5I_INVALID_HID;
        System.out.println();
    }

    @Before
    public void verifyCount() throws NullPointerException, HDF5Exception
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());
    }

    // Test v1.12 APIs params

    @Test
    public void testH5Rget_object()
    {
        int ref_type    = HDF5Constants.H5R_OBJECT1;
        long f_type     = HDF5Constants.H5I_INVALID_HID;
        int obj_type    = -1;
        int ndims       = 1;
        long ret_val    = -1;
        byte[][] refbuf = null;
        String name     = "";
        String objName  = "/DS_NA";

        try {
            openH5file(H5_DREG_FILE, objName);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: openH5file: " + err);
        }
        try {
            f_type     = H5.H5Dget_type(H5did);
            int result = H5.H5Tget_class(f_type);
            assertTrue("testH5Rget_object: H5Tget_class", result > 0);
            String class_name = H5.H5Tget_class_name(result);
            assertTrue("testH5Rget_object: H5Tget_class", class_name.compareTo("H5T_REFERENCE") == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: " + err);
        }
        finally {
            try {
                H5.H5Tclose(f_type);
            }
            catch (Exception ex) {
            }
        }
        try {
            ndims = (int)H5.H5Sget_simple_extent_npoints(H5dsid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_object: H5Sget_simple_extent_ndims: " + err);
        }
        refbuf = new byte[ndims][HDF5Constants.H5R_REF_BUF_SIZE];
        // Read the reference from the dataset.
        try {
            H5.H5Dread(H5did, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                       HDF5Constants.H5P_DEFAULT, refbuf);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Rget_object: H5Dread: " + err);
        }

        for (int i = 0; i < ndims; i++) {
            try {
                ret_val = H5.H5Rget_type(refbuf[i]);
                assertTrue("testH5Rget_object: H5Rget_type[" + i + "]=" + ret_val, ret_val == ref_type);
                if (!byteArrayCheck(refbuf[i])) {
                    try {
                        obj_type = H5.H5Rget_obj_type3(refbuf[i], HDF5Constants.H5P_DEFAULT);
                        assertEquals(obj_type, HDF5Constants.H5O_TYPE_DATASET);
                    }
                    catch (Throwable err2) {
                        err2.printStackTrace();
                        fail("testH5Rget_object: H5.H5Rget_obj_type3: " + err2);
                    }
                }
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Rget_object: H5Rget_type: " + err);
            }
            finally {
                H5.H5Rdestroy(refbuf[i]);
            }
        }
    }

    @Test
    public void testH5Rget_obj_type3()
    {
        long f_type     = HDF5Constants.H5I_INVALID_HID;
        int ref_type    = HDF5Constants.H5R_DATASET_REGION2;
        int obj_type    = -1;
        int ndims       = 1;
        long ret_val    = -1;
        byte[][] refbuf = null;
        String objName  = "/Dataset1";

        try {
            openH5file(H5_DREG_FILE, objName);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_obj_type3: openH5file: " + err);
        }
        try {
            f_type     = H5.H5Dget_type(H5did);
            int result = H5.H5Tget_class(f_type);
            assertTrue("testH5Rget_obj_type3: H5Tget_class", result > 0);
            String class_name = H5.H5Tget_class_name(result);
            assertTrue("testH5Rget_obj_type3: H5Tget_class=" + class_name,
                       class_name.compareTo("H5T_REFERENCE") == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_obj_type3: " + err);
        }
        finally {
            try {
                H5.H5Tclose(f_type);
            }
            catch (Exception ex) {
            }
        }
        try {
            ndims = (int)H5.H5Sget_simple_extent_npoints(H5dsid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_obj_type3: H5Sget_simple_extent_ndims: " + err);
        }
        refbuf = new byte[ndims][HDF5Constants.H5R_REF_BUF_SIZE];
        // Read the reference from the dataset.
        try {
            H5.H5Dread(H5did, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                       HDF5Constants.H5P_DEFAULT, refbuf);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Rget_obj_type3: H5Dread: " + err);
        }

        for (int i = 0; i < ndims; i++) {
            try {
                ret_val = H5.H5Rget_type(refbuf[i]);
                assertTrue("testH5Rget_obj_type3: H5Rget_type[" + i + "]=" + ret_val, ret_val == ref_type);
                if (!byteArrayCheck(refbuf[i])) {
                    try {
                        obj_type = H5.H5Rget_obj_type3(refbuf[i], HDF5Constants.H5P_DEFAULT);
                        assertEquals(obj_type, HDF5Constants.H5O_TYPE_DATASET);
                    }
                    catch (Throwable err2) {
                        err2.printStackTrace();
                        fail("testH5Rget_obj_type3: H5.H5Rget_obj_type3: " + err2);
                    }
                }
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Rget_obj_type3: H5Rget_type: " + err);
            }
            finally {
                H5.H5Rdestroy(refbuf[i]);
            }
        }
    }

    @Test
    public void testH5Rget_region_dataset()
    {
        long f_type     = HDF5Constants.H5I_INVALID_HID;
        long loc_id     = HDF5Constants.H5I_INVALID_HID;
        long loc_sid    = HDF5Constants.H5I_INVALID_HID;
        int ref_type    = HDF5Constants.H5R_DATASET_REGION2;
        int ndims       = 1;
        long ret_val    = -1;
        byte[][] refbuf = null;
        String name     = "";
        String objName  = "/Dataset1";

        try {
            openH5file(H5_DREG_FILE, objName);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_region_dataset: openH5file: " + err);
        }
        try {
            f_type     = H5.H5Dget_type(H5did);
            int result = H5.H5Tget_class(f_type);
            assertTrue("testH5Rget_region_dataset: H5Tget_class", result > 0);
            String class_name = H5.H5Tget_class_name(result);
            assertTrue("testH5Rget_region_dataset: H5Tget_class=" + class_name,
                       class_name.compareTo("H5T_REFERENCE") == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_region_dataset: " + err);
        }
        finally {
            try {
                H5.H5Tclose(f_type);
            }
            catch (Exception ex) {
            }
        }
        try {
            ndims = (int)H5.H5Sget_simple_extent_npoints(H5dsid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_region_dataset: H5Sget_simple_extent_ndims: " + err);
        }
        refbuf = new byte[ndims][HDF5Constants.H5R_REF_BUF_SIZE];
        // Read the reference from the dataset.
        try {
            H5.H5Dread(H5did, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                       HDF5Constants.H5P_DEFAULT, refbuf);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Rget_obj_type3: H5Dread: " + err);
        }
        for (int i = 0; i < ndims; i++) {
            try {
                try {
                    ret_val = H5.H5Rget_type(refbuf[i]);
                }
                catch (Throwable err) {
                    err.printStackTrace();
                    fail("testH5Rget_region_dataset: H5Rget_type[" + i + "]: " + err);
                }
                assertTrue("testH5Rget_region_dataset: H5Rget_type[" + i + "]=" + ret_val,
                           ret_val == ref_type);
                try {
                    loc_id =
                        H5.H5Ropen_object(refbuf[i], HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                    assertTrue(loc_id >= 0);
                    try {
                        loc_sid = H5.H5Ropen_region(refbuf[i], HDF5Constants.H5P_DEFAULT,
                                                    HDF5Constants.H5P_DEFAULT);
                        assertTrue(loc_sid >= 0);
                        int region_type = -1;
                        try {
                            int reg_ndims = H5.H5Sget_simple_extent_ndims(loc_sid);
                            region_type   = H5.H5Sget_select_type(loc_sid);
                            if (i == 1)
                                assertTrue(region_type == HDF5Constants.H5S_SEL_POINTS);
                            else
                                assertTrue(region_type == HDF5Constants.H5S_SEL_HYPERSLABS);
                            if (region_type == HDF5Constants.H5S_SEL_POINTS) {
                                long reg_npoints = H5.H5Sget_select_elem_npoints(loc_sid);
                                // Coordinates for get point selection
                                long getcoord[] = new long[reg_ndims * (int)reg_npoints];
                                // Known coordinates for point selection
                                long coord[][] = {{6, 9}, {2, 2}, {8, 4}, {1, 6}, {2, 8},
                                                  {3, 2}, {0, 4}, {9, 0}, {7, 1}, {3, 3}};
                                try {
                                    H5.H5Sget_select_elem_pointlist(loc_sid, 0, reg_npoints, getcoord);
                                    assertTrue("H5.H5Sget_select_elem_pointlist", coord[0][0] == getcoord[0]);
                                    assertTrue("H5.H5Sget_select_elem_pointlist", coord[0][1] == getcoord[1]);
                                    assertTrue("H5.H5Sget_select_elem_pointlist", coord[1][0] == getcoord[2]);
                                    assertTrue("H5.H5Sget_select_elem_pointlist", coord[1][1] == getcoord[3]);
                                    assertTrue("H5.H5Sget_select_elem_pointlist", coord[2][0] == getcoord[4]);
                                    assertTrue("H5.H5Sget_select_elem_pointlist", coord[2][1] == getcoord[5]);
                                }
                                catch (Throwable err3) {
                                    err3.printStackTrace();
                                    fail("H5.H5Sget_select_elem_pointlist: " + err3);
                                }
                            }
                            else if (region_type == HDF5Constants.H5S_SEL_HYPERSLABS) {
                                long reg_nblocks = H5.H5Sget_select_hyper_nblocks(loc_sid);
                                assertTrue("H5Sget_select_hyper_nblocks", reg_nblocks == 1);
                                // Coordinates for get block selection
                                long getblocks[] = new long[reg_ndims * (int)reg_nblocks * 2];
                                long start[]     = {2, 2};
                                long block[]     = {8, 8};
                                try {
                                    H5.H5Sget_select_hyper_blocklist(loc_sid, 0, reg_nblocks, getblocks);
                                    assertTrue("H5.H5Sget_select_hyper_blocklist", start[0] == getblocks[0]);
                                    assertTrue("H5.H5Sget_select_hyper_blocklist", start[1] == getblocks[1]);
                                    assertTrue("H5.H5Sget_select_hyper_blocklist",
                                               (block[0] - 1) == getblocks[2]);
                                    assertTrue("H5.H5Sget_select_hyper_blocklist",
                                               (block[1] - 1) == getblocks[3]);
                                }
                                catch (Throwable err3) {
                                    err3.printStackTrace();
                                    fail("H5.H5Sget_select_hyper_blocklist: " + err3);
                                }
                            }
                        }
                        catch (Throwable err2) {
                            err2.printStackTrace();
                            assertTrue("testH5Rget_region_dataset: H5Sget_select_type: " + err2, i > 1);
                        }
                    }
                    catch (Throwable err1) {
                        err1.printStackTrace();
                        fail("testH5Rget_region_dataset: " + err1);
                    }
                    finally {
                        try {
                            H5.H5Sclose(loc_sid);
                        }
                        catch (Exception ex) {
                        }
                    }
                }
                catch (Throwable err0) {
                    err0.printStackTrace();
                    fail("testH5Rget_region_dataset: " + err0);
                }
                finally {
                    try {
                        H5.H5Dclose(loc_id);
                    }
                    catch (Exception ex) {
                    }
                }
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Rget_region_dataset: H5Rget_type: " + err);
            }
            finally {
                H5.H5Rdestroy(refbuf[i]);
            }
        } // for (int i = 0; i < ndims; i++)
    }

    @Test
    public void testH5Rget_region_attribute()
    {
        long f_type     = HDF5Constants.H5I_INVALID_HID;
        long loc_id     = HDF5Constants.H5I_INVALID_HID;
        long loc_sid    = HDF5Constants.H5I_INVALID_HID;
        int ref_type    = HDF5Constants.H5R_ATTR;
        int obj_type    = -1;
        int ndims       = 1;
        long ret_val    = -1;
        byte[][] refbuf = null;
        String objName  = "/Dataset3";

        try {
            openH5file(H5_AREG_FILE, objName);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_region_attribute: openH5file: " + err);
        }
        try {
            f_type     = H5.H5Dget_type(H5did);
            int result = H5.H5Tget_class(f_type);
            assertTrue("testH5Rget_region_attribute: H5Tget_class", result > 0);
            String class_name = H5.H5Tget_class_name(result);
            assertTrue("testH5Rget_region_attribute: H5Tget_class=" + class_name,
                       class_name.compareTo("H5T_REFERENCE") == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_region_attribute: " + err);
        }
        finally {
            try {
                H5.H5Tclose(f_type);
            }
            catch (Exception ex) {
            }
        }
        try {
            ndims = (int)H5.H5Sget_simple_extent_npoints(H5dsid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Rget_region_attribute: H5Sget_simple_extent_ndims: " + err);
        }
        refbuf = new byte[ndims][HDF5Constants.H5R_REF_BUF_SIZE];
        // Read the reference from the dataset.
        try {
            H5.H5Dread(H5did, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                       HDF5Constants.H5P_DEFAULT, refbuf);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Rget_region_attribute: H5Dread: " + err);
        }

        for (int i = 0; i < ndims; i++) {
            try {
                ret_val = H5.H5Rget_type(refbuf[i]);
                assertTrue("testH5Rget_region_attribute: H5Rget_type[" + i + "]=" + ret_val,
                           ret_val == ref_type);
                try {
                    loc_id = H5.H5Ropen_attr(refbuf[i], HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                    assertTrue(loc_id >= 0);
                    if (!byteArrayCheck(refbuf[i])) {
                        try {
                            loc_sid = H5.H5Aget_space(loc_id);
                            assertTrue(loc_sid >= 0);
                        }
                        catch (Throwable err1) {
                            err1.printStackTrace();
                            fail("testH5Rget_region_attribute: " + err1);
                        }
                        finally {
                            try {
                                H5.H5Sclose(loc_sid);
                            }
                            catch (Exception ex) {
                            }
                        }
                    }
                }
                catch (Throwable err0) {
                    err0.printStackTrace();
                    // second attribute is null
                    assertTrue("testH5Rget_region_attribute: " + err0, i == 1);
                }
                finally {
                    try {
                        H5.H5Aclose(loc_id);
                    }
                    catch (Exception ex) {
                    }
                }
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Rget_region_attribute: H5Rget_type: " + err);
            }
            finally {
                H5.H5Rdestroy(refbuf[i]);
            }
        }
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
}
