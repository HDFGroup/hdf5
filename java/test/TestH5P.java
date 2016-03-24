/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
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
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5P {
    @Rule public TestName testname = new TestName();

    private static final String H5_FILE = "test.h5";
    private static final int DIM_X = 4;
    private static final int DIM_Y = 6;
    long[] H5dims = { DIM_X, DIM_Y };
    long H5fid = -1;
    long H5dsid = -1;
    long H5did = -1;
    long lapl_id = -1;
    long fapl_id = -1;
    long fcpl_id = -1;
    long ocpl_id = -1;
    long ocp_plist_id = -1;
    long lcpl_id = -1;
    long plapl_id = -1;
    long plist_id = -1;
    long gapl_id = -1;
    long gcpl_id = -1;
    long acpl_id = -1;

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    private final long _createDataset(long fid, long dsid, String name, long dapl) {
        long did = -1;
        try {
            did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_STD_I32BE, dsid,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5P._createDataset: ", did > 0);

        return did;
    }

    private final void _createH5File(long fcpl, long fapl) {
        try {
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC,
                    fcpl, fapl);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            H5did = _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5P.createH5file: " + err);
        }
        assertTrue("TestH5P.createH5file: H5.H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5P.createH5file: H5.H5Screate_simple: ", H5dsid > 0);
        assertTrue("TestH5P.createH5file: _createDataset: ", H5did > 0);

        try {
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    public void deleteH5file() throws HDF5LibraryException {
        _deleteFile(H5_FILE);
    }

    @Before
    public void createH5fileProperties()
            throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        try {
            lapl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_ACCESS);
            fapl_id = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);
            fcpl_id = H5.H5Pcreate(HDF5Constants.H5P_FILE_CREATE);
            ocpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            ocp_plist_id = H5.H5Pcreate(HDF5Constants.H5P_OBJECT_COPY);
            lcpl_id = H5.H5Pcreate(HDF5Constants.H5P_LINK_CREATE);
            plapl_id = H5.H5Pcreate(HDF5Constants.H5P_LINK_ACCESS);
            plist_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_XFER);
            gapl_id = H5.H5Pcreate(HDF5Constants.H5P_GROUP_ACCESS);
            gcpl_id = H5.H5Pcreate(HDF5Constants.H5P_GROUP_CREATE);
            acpl_id = H5.H5Pcreate(HDF5Constants.H5P_ATTRIBUTE_CREATE);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5P.createH5file: " + err);
        }
        assertTrue(lapl_id > 0);
        assertTrue(fapl_id > 0);
        assertTrue(fcpl_id > 0);
        assertTrue(ocpl_id > 0);
        assertTrue(ocp_plist_id > 0);
        assertTrue(lcpl_id > 0);
        assertTrue(plapl_id>0);
        assertTrue(plist_id > 0);
        assertTrue(gapl_id > 0);
        assertTrue(gcpl_id >0);
        assertTrue(acpl_id >0);
    }

    @After
    public void deleteH5fileProperties() throws HDF5LibraryException {
        if (lapl_id >0)
            try {H5.H5Pclose(lapl_id);} catch (Exception ex) {}
        if (fapl_id >0)
            try {H5.H5Pclose(fapl_id);} catch (Exception ex) {}
        if (fcpl_id >0)
            try {H5.H5Pclose(fcpl_id);} catch (Exception ex) {}
        if (ocpl_id >0)
            try {H5.H5Pclose(ocpl_id);} catch (Exception ex) {}
        if (ocp_plist_id >0)
            try {H5.H5Pclose(ocp_plist_id);} catch (Exception ex) {}
        if (lcpl_id >0)
            try {H5.H5Pclose(lcpl_id);} catch (Exception ex) {}
        if (plapl_id >0)
            try {H5.H5Pclose(plapl_id);} catch (Exception ex) {}
        if (plist_id >0)
            try {H5.H5Pclose(plist_id);} catch (Exception ex) {}
        if (gapl_id >0)
            try {H5.H5Pclose(gapl_id);} catch (Exception ex) {}
        if (gcpl_id >0)
            try {H5.H5Pclose(gcpl_id);} catch (Exception ex) {}
        if (acpl_id >0)
            try {H5.H5Pclose(acpl_id);} catch (Exception ex) {}
        if (H5dsid > 0)
            try {H5.H5Sclose(H5dsid);} catch (Exception ex) {}
        if (H5did > 0)
            try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        if (H5fid > 0)
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
        System.out.println();
    }

    @Test
    public void testH5Pget_nlinks() {
        long nlinks = -1;
        try {
            nlinks = (long) H5.H5Pget_nlinks(lapl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Pget_nlinks: " + err);
        }
        assertTrue("testH5Pget_nlinks", nlinks > 0);
        // Check the default value of nlinks.
        assertEquals(nlinks, 16L);
    }

    @Test
    public void testH5Pset_nlinks() {
        long nlinks = 20;
        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_nlinks(lapl_id, nlinks);
            nlinks = (long) H5.H5Pget_nlinks(lapl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Pset_nlinks: " + err);
        }
        assertTrue("testH5Pset_nlinks", ret_val >= 0);
        // Check the value of nlinks retrieved from H5Pget_nlinks function.
        assertEquals(nlinks, 20L);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_libver_bounds_invalidlow() throws Throwable {
        H5.H5Pset_libver_bounds(fapl_id, 5, HDF5Constants.H5F_LIBVER_LATEST);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_libver_bounds_invalidhigh() throws Throwable {
        H5.H5Pset_libver_bounds(fapl_id, HDF5Constants.H5F_LIBVER_LATEST, 5);
    }

    @Test
    public void testH5Pget_link_creation_order() {
        int crt_order_flags = 0;
        try {
            crt_order_flags = H5.H5Pget_link_creation_order(fcpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_link_creation_order: " + err);
        }
        assertTrue("testH5Pget_link_creation_order", crt_order_flags >= 0);
    }

    @Test
    public void testH5Pset_link_creation_order_trackedPLUSindexed() {
        int ret_val = -1;
        int crt_order_flags = HDF5Constants.H5P_CRT_ORDER_TRACKED + HDF5Constants.H5P_CRT_ORDER_INDEXED;
        int crtorderflags = 0;

        try {
            ret_val = H5.H5Pset_link_creation_order(fcpl_id, crt_order_flags);
            crtorderflags = H5.H5Pget_link_creation_order(fcpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_link_creation_order: " + err);
        }
        assertTrue("testH5Pset_link_creation_order_trackedPLUSindexed",ret_val >= 0);
        assertEquals(crt_order_flags, crtorderflags);
    }

    @Test
    public void testH5Pset_link_creation_order_tracked() {
        int ret_val = -1;
        int crtorderflags = 0;

        try {
            ret_val = H5.H5Pset_link_creation_order(fcpl_id, HDF5Constants.H5P_CRT_ORDER_TRACKED);
            crtorderflags = H5.H5Pget_link_creation_order(fcpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_link_creation_order: " + err);
        }
        assertTrue("testH5Pset_link_creation_order_tracked",ret_val >= 0);
        assertEquals(HDF5Constants.H5P_CRT_ORDER_TRACKED, crtorderflags);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pset_link_creation_order_invalidvalue() throws Throwable {
        H5.H5Pset_link_creation_order(fcpl_id, HDF5Constants.H5P_CRT_ORDER_INDEXED);
    }

    @Test
    public void testH5Pget_attr_creation_order() {
        int crt_order_flags = 0;

        try {
            crt_order_flags = H5.H5Pget_attr_creation_order(ocpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_attr_creation_order: " + err);
        }
        assertTrue("testH5Pget_attr_creation_order", crt_order_flags >= 0);
    }

    @Test
    public void testH5Pset_attr_creation_order_trackedPLUSindexed() {
        int ret_val = -1;
        int crt_order_flags = HDF5Constants.H5P_CRT_ORDER_TRACKED + HDF5Constants.H5P_CRT_ORDER_INDEXED;
        int crtorderflags = 0;

        try {
            ret_val = H5.H5Pset_attr_creation_order(ocpl_id, crt_order_flags);
            crtorderflags = H5.H5Pget_attr_creation_order(ocpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_attr_creation_order: " + err);
        }
        assertTrue("testH5Pset_attr_creation_order_trackedPLUSindexed", ret_val >= 0);
        assertEquals(crt_order_flags, crtorderflags);
    }

    @Test
    public void testH5Pset_attr_creation_order_tracked() {
        int ret_val = -1;
        int crtorderflags = 0;

        try {
            ret_val = H5.H5Pset_attr_creation_order(ocpl_id, HDF5Constants.H5P_CRT_ORDER_TRACKED);
            crtorderflags = H5.H5Pget_attr_creation_order(ocpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_attr_creation_order: " + err);
        }
        assertTrue("testH5Pset_attr_creation_order_tracked", ret_val >= 0);
        assertEquals(HDF5Constants.H5P_CRT_ORDER_TRACKED, crtorderflags);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pset_attr_creation_order_invalidvalue() throws Throwable {
        H5.H5Pset_attr_creation_order(ocpl_id, HDF5Constants.H5P_CRT_ORDER_INDEXED);
    }

    @Test
    public void testH5Pset_copy_object() {

        int cpy_option = -1;

        try {
            H5.H5Pset_copy_object(ocp_plist_id, HDF5Constants.H5O_COPY_SHALLOW_HIERARCHY_FLAG);
            cpy_option = H5.H5Pget_copy_object(ocp_plist_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_copy_object: " + err);
        }
        assertEquals(HDF5Constants.H5O_COPY_SHALLOW_HIERARCHY_FLAG, cpy_option);

        try {
            H5.H5Pset_copy_object(ocp_plist_id, HDF5Constants.H5O_COPY_EXPAND_REFERENCE_FLAG);
            cpy_option = H5.H5Pget_copy_object(ocp_plist_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_copy_object: " + err);
        }
        assertEquals(HDF5Constants.H5O_COPY_EXPAND_REFERENCE_FLAG, cpy_option);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pset_copy_object_invalidobject() throws Throwable {
        H5.H5Pset_copy_object(HDF5Constants.H5P_DEFAULT, HDF5Constants.H5O_COPY_SHALLOW_HIERARCHY_FLAG);
    }

    @Test
    public void testH5Pset_create_intermediate_group() {

        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_create_intermediate_group(lcpl_id, true);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_create_intermediate_group: " + err);
        }
        assertTrue(ret_val>=0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pset_create_intermediate_group_invalidobject() throws Throwable {
        H5.H5Pset_create_intermediate_group(ocp_plist_id, true);
    }

    @Test
    public void testH5Pget_create_intermediate_group() {
        boolean flag = false;
        try {
            H5.H5Pset_create_intermediate_group(lcpl_id, true);
            flag = H5.H5Pget_create_intermediate_group(lcpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_create_intermediate_group: " + err);
        }
        assertEquals(true, flag);
    }

    @Test
    public void testH5Pget_create_intermediate_group_notcreated() {
        boolean flag = true;
        try {
            flag = H5.H5Pget_create_intermediate_group(lcpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_create_intermediate_group_notcreated: " + err);
        }
        assertEquals(false, flag);
    }

    @Test
    public void testH5Pset_data_transform() {

        String expression = "(5/9.0)*(x-32)";
        int ret_val = -1;

        try {
            ret_val= H5.H5Pset_data_transform(plist_id, expression);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_data_transform: " + err);
        }
        assertTrue(ret_val>=0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pset_data_transform_NullExpression() throws Throwable {
        H5.H5Pset_data_transform(plist_id, null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pset_data_transform_InvalidExpression1() throws Throwable {
        H5.H5Pset_data_transform(plist_id, "");
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pset_data_transform_InvalidExpression2() throws Throwable {
        H5.H5Pset_data_transform(plist_id, "hello");
    }

    @Test
    public void testH5Pget_data_transform() {

        String expression = "(5/9.0)*(x-32)";
        String [] express = {""};
        long express_size = 0;
        long size = 20;

        try {
            H5.H5Pset_data_transform(plist_id, expression);
            express_size = H5.H5Pget_data_transform(plist_id, express, size);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_data_transform: " + err);
        }
        assertTrue(express_size>=0);
        assertTrue("The data transform expression: ", expression.equals(express[0]));
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pget_data_transform_ExpressionNotSet() throws Throwable {
        String [] express = {""};
        H5.H5Pget_data_transform(plist_id, express, 20);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pget_data_transform_IllegalSize() throws Throwable {
        String [] express = {""};
        H5.H5Pset_data_transform(plist_id, "(5/9.0)*(x-32)");
        H5.H5Pget_data_transform(plist_id, express, 0);
    }

    @Test
    public void testH5Pget_elink_acc_flags() {

        int get_flags = -1;
        try {
            get_flags = H5.H5Pget_elink_acc_flags(gapl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_elink_acc_flags: " + err);
        }
        assertTrue("H5Pget_elink_acc_flags", get_flags >= 0);
        assertEquals(HDF5Constants.H5F_ACC_DEFAULT, get_flags);
    }

    @Test
    public void testH5Pset_elink_acc_flags() {

        int get_flags = -1;
        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_elink_acc_flags(lapl_id, HDF5Constants.H5F_ACC_RDWR);
            get_flags = H5.H5Pget_elink_acc_flags(lapl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_elink_acc_flags: " + err);
        }
        assertTrue("H5Pset_elink_acc_flags", ret_val >= 0);
        assertEquals(HDF5Constants.H5F_ACC_RDWR, get_flags);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Pset_elink_acc_flags_InvalidFlag1() throws Throwable {
        H5.H5Pset_elink_acc_flags(lapl_id, HDF5Constants.H5F_ACC_TRUNC);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Pset_elink_acc_flags_InvalidFlag2() throws Throwable {
        H5.H5Pset_elink_acc_flags(lapl_id, -1);
    }

    @Test
    public void testH5Pset_link_phase_change() {

        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_link_phase_change(fcpl_id , 2, 2);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_link_phase_change: " + err);
        }
        assertTrue("H5Pset_link_phase_change", ret_val >= 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_link_phase_change_Highmax_Compact() throws Throwable {
        H5.H5Pset_link_phase_change(fcpl_id , 70000000, 3);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_link_phase_change_max_compactLESSTHANmin_dense() throws Throwable {
        H5.H5Pset_link_phase_change(fcpl_id , 5, 6);
    }

    @Test
    public void testH5Pget_link_phase_change() {
        int ret_val = -1;
        int[] links = new int[2];

        try {
            ret_val = H5.H5Pget_link_phase_change(fcpl_id, links);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_link_phase_change: " + err);
        }
        assertTrue("testH5Pget_link_phase_change", ret_val >= 0);
        assertEquals("Default value of maximum compact storage", 8, links[0]);
        assertEquals("Default value of minimum dense storage", 6, links[1]);
    }

    @Test
    public void testH5Pget_link_phase_change_EqualsSet() {
        int[] links = new int[2];
        try {
            H5.H5Pset_link_phase_change(fcpl_id , 10, 7);
            H5.H5Pget_link_phase_change(fcpl_id, links);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_link_phase_change_EqualsSet: " + err);
        }
        assertEquals("Value of maximum compact storage set", 10, links[0]);
        assertEquals("Value of minimum dense storage set", 7, links[1]);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pget_link_phase_change_Null() throws Throwable {
        H5.H5Pget_link_phase_change(fcpl_id, null);
    }

    @Test
    public void testH5Pget_attr_phase_change() {
        int ret_val = -1;
        int[] attributes = new int[2];

        try {
            ret_val = H5.H5Pget_attr_phase_change(ocpl_id, attributes);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_attr_phase_change: " + err);
        }
        assertTrue("testH5Pget_attr_phase_change", ret_val >= 0);
        assertEquals("Default value of the max. no. of attributes stored in compact storage", 8, attributes[0]);
        assertEquals("Default value of the min. no. of attributes stored in dense storage", 6, attributes[1]);
        try {
            H5.H5Pset_attr_phase_change(ocpl_id, 9, 5);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_attr_phase_change: " + err);
        }
        try {
            ret_val = H5.H5Pget_attr_phase_change(ocpl_id, attributes);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_attr_phase_change: " + err);
        }
        assertTrue("testH5Pget_attr_phase_change", ret_val >= 0);
        assertEquals("Default value of the max. no. of attributes stored in compact storage", 9, attributes[0]);
        assertEquals("Default value of the min. no. of attributes stored in dense storage", 5, attributes[1]);
    }

    @Test
    public void testH5Pget_shared_mesg_phase_change() {
        int ret_val = -1;
        int[] size = new int[2];

        try {
            ret_val = H5.H5Pget_shared_mesg_phase_change(fcpl_id, size);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_shared_mesg_phase_change: " + err);
        }
        assertTrue("testH5Pget_shared_mesg_phase_change", ret_val >= 0);
    }

    @Test
    public void testH5Pget_shared_mesg_phase_change_EqualsSET() {
        int[] size = new int[2];

        try {
            H5.H5Pset_shared_mesg_phase_change(fcpl_id,50, 40);
            H5.H5Pget_shared_mesg_phase_change(fcpl_id, size);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_shared_mesg_phase_change_EqualsSET: " + err);
        }
        assertEquals("Value of maximum list set", 50, size[0]);
        assertEquals("Value of minimum btree set", 40, size[1]);
    }

    @Test
    public void testH5Pset_shared_mesg_phase_change() {

        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_shared_mesg_phase_change(fcpl_id,2, 1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_shared_mesg_phase_change: " + err);
        }
        assertTrue("H5Pset_shared_mesg_phase_change", ret_val >= 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5PH5Pset_shared_mesg_phase_change_HighMaxlistValue() throws Throwable {
        H5.H5Pset_shared_mesg_phase_change(fcpl_id, 5001, 4000);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5PH5Pset_shared_mesg_phase_change_HighMinbtreeValue() throws Throwable {
        H5.H5Pset_shared_mesg_phase_change(fcpl_id, 5000, 5001);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5PH5Pset_shared_mesg_phase_change_MinbtreeGreaterThanMaxlist() throws Throwable {
        H5.H5Pset_link_phase_change(fcpl_id , 3, 7);
    }

    @Test
    public void testH5Pget_shared_mesg_nindexes() {

        int nindexes = -1;
        try {
            nindexes = H5.H5Pget_shared_mesg_nindexes(fcpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_shared_mesg_nindexes: " + err);
        }
        assertTrue("H5Pget_shared_mesg_nindexes", nindexes >= 0);
    }

    @Test
    public void testH5Pset_shared_mesg_nindexes() {

        int nindexes = -1;
        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_shared_mesg_nindexes(fcpl_id, 7);
            nindexes = H5.H5Pget_shared_mesg_nindexes(fcpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_shared_mesg_nindexes: " + err);
        }
        assertTrue("H5Pset_shared_mesg_nindexes", ret_val >= 0);
        assertEquals("Value of nindexes is equal to value set",7 ,nindexes);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_shared_mesg_nindexes_InvalidHIGHnindexes()throws Throwable {
        H5.H5Pset_shared_mesg_nindexes(fcpl_id, 9);
    }

    @Test
    public void testH5Pset_shared_mesg_index() {

        int ret_val = -1;
        try {
            H5.H5Pset_shared_mesg_nindexes(fcpl_id, 2);
            ret_val = H5.H5Pset_shared_mesg_index(fcpl_id, 0,HDF5Constants.H5O_SHMESG_ATTR_FLAG, 10);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_shared_mesg_index: " + err);
        }
        assertTrue("H5Pset_shared_mesg_index", ret_val >= 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_shared_mesg_index_Invalid_indexnum() throws Throwable {
        H5.H5Pset_shared_mesg_index(fcpl_id, 2,HDF5Constants.H5O_SHMESG_ATTR_FLAG, 10);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_shared_mesg_index_InvalidFlag() throws Throwable {
        H5.H5Pset_shared_mesg_nindexes(fcpl_id, 7);
        H5.H5Pset_shared_mesg_index(fcpl_id, 2,HDF5Constants.H5O_SHMESG_ALL_FLAG + 1, 10);
    }

    @Test
    public void testH5Pget_shared_mesg_index() {

        int ret_val = -1;
        int[] mesg_info = new int[2];
        try {
            H5.H5Pset_shared_mesg_nindexes(fcpl_id, 2);
            H5.H5Pset_shared_mesg_index(fcpl_id, 0,HDF5Constants.H5O_SHMESG_ATTR_FLAG, 10);
            ret_val = H5.H5Pget_shared_mesg_index(fcpl_id, 0, mesg_info);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_shared_mesg_index: " + err);
        }
        assertTrue("H5Pget_shared_mesg_index", ret_val >= 0);
        assertEquals("Type of message", HDF5Constants.H5O_SHMESG_ATTR_FLAG, mesg_info[0]);
        assertEquals("minimum message size", 10, mesg_info[1]);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pget_shared_mesg_index_Invalid_indexnum() throws Throwable {
        int[] mesg_info = new int[2];
        H5.H5Pget_shared_mesg_index(fcpl_id, 0, mesg_info);
    }

    @Test
    public void testH5Pset_local_heap_size_hint() {
        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_local_heap_size_hint(gcpl_id, 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_local_heap_size_hint: " + err);
        }
        assertTrue("H5Pset_local_heap_size_hint", ret_val >= 0);
    }

    @Test
    public void testH5Pget_local_heap_size_hint() {
        long size_hint = -1;
        try {
            size_hint = H5.H5Pget_local_heap_size_hint(gcpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_local_heap_size_hint: " + err);
        }
        assertTrue("H5Pget_local_heap_size_hint", size_hint >= 0);
    }

    @Test
    public void testH5Pset_nbit() {
        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_nbit(ocpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_nbit: " + err);
        }
        assertTrue("H5Pset_nbit", ret_val >= 0);
    }

    @Test
    public void testH5Pset_scaleoffset() {
        int ret_val = -1;
        int scale_type = HDF5Constants.H5Z_SO_FLOAT_DSCALE;
        int scale_factor = HDF5Constants.H5Z_SO_INT_MINBITS_DEFAULT;
        try {
            ret_val = H5.H5Pset_scaleoffset(ocpl_id, scale_type, scale_factor);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_scaleoffset: " + err);
        }
        assertTrue("H5Pset_scaleoffset", ret_val >= 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_scaleoffset_Invalidscale_type() throws Throwable {
        H5.H5Pset_scaleoffset(ocpl_id, 3, 1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_scaleoffset_Invalidscale_factor() throws Throwable {
        H5.H5Pset_scaleoffset(ocpl_id, HDF5Constants.H5Z_SO_INT, -1);
    }

    @Test
    public void testH5Pset_est_link_info() {
        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_est_link_info(gcpl_id, 0,10);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_est_link_info: " + err);
        }
        assertTrue("H5Pset_est_link_info", ret_val >= 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Pset_est_link_info_InvalidValues() throws Throwable {
        H5.H5Pset_est_link_info(gcpl_id, 100000,10);
    }

    @Test
    public void testH5Pget_est_link_info() {
        int ret_val = -1;
        int[] link_info = new int[2];
        try {
            ret_val = H5.H5Pget_est_link_info(gcpl_id, link_info);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_est_link_info: " + err);
        }
        assertTrue("H5Pget_est_link_info", ret_val >= 0);
    }

    @Test
    public void testH5Pset_elink_prefix() {
        int ret_val = -1;
        String prefix = "tmp";
        try {
            ret_val = H5.H5Pset_elink_prefix(plapl_id, prefix);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_est_link_info: " + err);
        }
        assertTrue("H5Pset_elink_prefix", ret_val >= 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pset_elink_prefix_null() throws Throwable{
        H5.H5Pset_elink_prefix(plapl_id, null);
    }

    @Test
    public void testH5Pget_elink_prefix() {
        String prefix = "tmp";
        String[] pre = {""};
        long prefix_size = 0;

        try {
            H5.H5Pset_elink_prefix(plapl_id, prefix);
            prefix_size = H5.H5Pget_elink_prefix(plapl_id, pre);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_elink_prefix: " + err);
        }
        assertTrue(prefix_size>=0);
        assertTrue("The prefix: ", prefix.equals(pre[0]));
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pget_elink_prefix_null() throws Throwable {
        H5.H5Pget_elink_prefix(plapl_id, null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pget_version_null() throws Throwable {
        H5.H5Pget_version(fcpl_id, null);
    }

    @Test
    public void testH5Pget_version() {
        int[] version_info = {255,255,255,255};

        try {
            _createH5File(fcpl_id, fapl_id);
            H5.H5Pget_version(fcpl_id, version_info);
            deleteH5file();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_version: " + err);
        }
        assertTrue("super block version: "+version_info[0], version_info[0] == 0);
        assertTrue("global freelist version: "+version_info[1], version_info[1] == 0);
        assertTrue("symbol table version: "+version_info[2], version_info[2] == 0);
        assertTrue("shared object header version: "+version_info[3], version_info[3] == 0);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pget_userblock_null() throws Throwable {
        H5.H5Pget_userblock(fcpl_id, null);
    }

    @Test
    public void testH5P_userblock() {
        int[] version_info = {255,255,255,255};
        long[] size = {0};

        try {
            H5.H5Pset_userblock(fcpl_id, 1024);
            _createH5File(fcpl_id, fapl_id);

            /* Close FCPL */
            H5.H5Pclose(fcpl_id);

            /* Get the file's dataset creation property list */
            fcpl_id =  H5.H5Fget_create_plist(H5fid);

            /* Get the file's version information */
            H5.H5Pget_version(fcpl_id, version_info);
            H5.H5Pget_userblock(fcpl_id, size);
            deleteH5file();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_userblock: " + err);
        }
        assertTrue("super block version: "+version_info[0], version_info[0] == 0);
        assertTrue("global freelist version: "+version_info[1], version_info[1] == 0);
        assertTrue("symbol table version: "+version_info[2], version_info[2] == 0);
        assertTrue("shared object header version: "+version_info[3], version_info[3] == 0);
        assertTrue("user block size: "+size[0], size[0] == 1024);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pget_sizes_null() throws Throwable {
        H5.H5Pget_sizes(fcpl_id, null);
    }

    @Test
    public void testH5P_sizes() {
        int[] version_info = {255,255,255,255};
        long[] size = {0,0};

        try {
            H5.H5Pset_sizes(fcpl_id, 4, 8);
            _createH5File(fcpl_id, fapl_id);

            /* Close FCPL */
            H5.H5Pclose(fcpl_id);

            /* Get the file's dataset creation property list */
            fcpl_id =  H5.H5Fget_create_plist(H5fid);

            /* Get the file's version information */
            H5.H5Pget_version(fcpl_id, version_info);
            H5.H5Pget_sizes(fcpl_id, size);
            deleteH5file();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_sizes: " + err);
        }
        assertTrue("super block version: "+version_info[0], version_info[0] == 0);
        assertTrue("global freelist version: "+version_info[1], version_info[1] == 0);
        assertTrue("symbol table version: "+version_info[2], version_info[2] == 0);
        assertTrue("shared object header version: "+version_info[3], version_info[3] == 0);
        assertTrue("sizeof_addr size: "+size[0], size[0] == 4);
        assertTrue("sizeof_size size: "+size[1], size[1] == 8);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pget_sym_k_null() throws Throwable {
        H5.H5Pget_sym_k(fcpl_id, null);
    }

    @Test
    public void testH5P_sym_k() {
        int[] version_info = {255,255,255,255};
        int[] size = {0,0};

        try {
            H5.H5Pset_sym_k(fcpl_id, 32, 8);
            _createH5File(fcpl_id, fapl_id);

            /* Close FCPL */
            H5.H5Pclose(fcpl_id);

            /* Get the file's dataset creation property list */
            fcpl_id =  H5.H5Fget_create_plist(H5fid);

            /* Get the file's version information */
            H5.H5Pget_version(fcpl_id, version_info);
            H5.H5Pget_sym_k(fcpl_id, size);
            deleteH5file();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_sym_k: " + err);
        }
        assertTrue("super block version: "+version_info[0], version_info[0] == 0);
        assertTrue("global freelist version: "+version_info[1], version_info[1] == 0);
        assertTrue("symbol table version: "+version_info[2], version_info[2] == 0);
        assertTrue("shared object header version: "+version_info[3], version_info[3] == 0);
        assertTrue("symbol table tree rank: "+size[0], size[0] == 32);
        assertTrue("symbol table node size: "+size[1], size[1] == 8);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pget_istore_k_null() throws Throwable {
        H5.H5Pget_istore_k(fcpl_id, null);
    }

    @Test
    public void testH5P_istore_k() {
        int[] version_info = {255,255,255,255};
        int[] size = {0};

        try {
            H5.H5Pset_istore_k(fcpl_id, 64);
            _createH5File(fcpl_id, fapl_id);

            /* Close FCPL */
            H5.H5Pclose(fcpl_id);

            /* Get the file's dataset creation property list */
            fcpl_id =  H5.H5Fget_create_plist(H5fid);

            /* Get the file's version information */
            H5.H5Pget_version(fcpl_id, version_info);
            H5.H5Pget_istore_k(fcpl_id, size);
            deleteH5file();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_sym_k: " + err);
        }
        assertTrue("super block version: "+version_info[0], version_info[0] == 1);
        assertTrue("global freelist version: "+version_info[1], version_info[1] == 0);
        assertTrue("symbol table version: "+version_info[2], version_info[2] == 0);
        assertTrue("shared object header version: "+version_info[3], version_info[3] == 0);
        assertTrue("chunked storage b-tree 1/2-rank: "+size[0], size[0] == 64);
    }

    @Test
    public void testH5P_obj_track_times() {
        boolean default_ret_val = false;
        boolean ret_val = true;
        try {
            default_ret_val = H5.H5Pget_obj_track_times(ocpl_id);
            H5.H5Pset_obj_track_times(ocpl_id, false);
            ret_val = H5.H5Pget_obj_track_times(ocpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_obj_track_times: " + err);
        }
        assertTrue("H5Pget_obj_track_times default", default_ret_val);
        assertFalse("H5Pget_obj_track_times", ret_val);
    }

    @Test
    public void testH5Pget_char_encoding() {
        int char_encoding = 0;

        try {
            char_encoding = H5.H5Pget_char_encoding(acpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_char_encoding: " + err);
        }
        assertTrue("testH5Pget_char_encoding", char_encoding == HDF5Constants.H5T_CSET_ASCII);
        try {
            H5.H5Pset_char_encoding(acpl_id, HDF5Constants.H5T_CSET_UTF8);
            char_encoding = H5.H5Pget_char_encoding(acpl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_char_encoding: " + err);
        }
        assertTrue("testH5Pget_char_encoding", char_encoding == HDF5Constants.H5T_CSET_UTF8);
    }

    @Test
    public void testH5P_fill_time() {
        int[] fill_time = {0};

        try {
            H5.H5Pget_fill_time(ocpl_id, fill_time);
            assertTrue("fill_time: "+fill_time[0], fill_time[0] == HDF5Constants.H5D_FILL_TIME_IFSET);
            H5.H5Pset_fill_time(ocpl_id, HDF5Constants.H5D_FILL_TIME_ALLOC);
            H5.H5Pget_fill_time(ocpl_id, fill_time);
            assertTrue("fill_time: "+fill_time[0], fill_time[0] == HDF5Constants.H5D_FILL_TIME_ALLOC);
            H5.H5Pset_fill_time(ocpl_id, HDF5Constants.H5D_FILL_TIME_NEVER);
            H5.H5Pget_fill_time(ocpl_id, fill_time);
            assertTrue("fill_time: "+fill_time[0], fill_time[0] == HDF5Constants.H5D_FILL_TIME_NEVER);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fill_time: " + err);
        }
    }

    @Test
    public void testH5P_alloc_time() {
        int[] alloc_time = {0};

        try {
            H5.H5Pget_alloc_time(ocpl_id, alloc_time);
            assertTrue("alloc_time: "+alloc_time[0], alloc_time[0] == HDF5Constants.H5D_ALLOC_TIME_LATE);
            H5.H5Pset_alloc_time(ocpl_id, HDF5Constants.H5D_ALLOC_TIME_EARLY);
            H5.H5Pget_alloc_time(ocpl_id, alloc_time);
            assertTrue("alloc_time: "+alloc_time[0], alloc_time[0] == HDF5Constants.H5D_ALLOC_TIME_EARLY);
            H5.H5Pset_alloc_time(ocpl_id, HDF5Constants.H5D_ALLOC_TIME_INCR);
            H5.H5Pget_alloc_time(ocpl_id, alloc_time);
            assertTrue("alloc_time: "+alloc_time[0], alloc_time[0] == HDF5Constants.H5D_ALLOC_TIME_INCR);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_alloc_time: " + err);
        }
    }

    @Test
    public void testH5P_fill_value() {
        int[] fill_value = {-1};
        int[] fill_value_status = {-1};

        try {
            H5.H5Pfill_value_defined(ocpl_id, fill_value_status);
            assertTrue("fill_value_status: "+fill_value_status[0], fill_value_status[0] == HDF5Constants.H5D_FILL_VALUE_DEFAULT);
            H5.H5Pget_fill_value(ocpl_id, HDF5Constants.H5T_NATIVE_INT, fill_value);
            assertTrue("fill_value: "+fill_value[0], fill_value[0] == 0);
            fill_value[0] = 255;
            H5.H5Pset_fill_value(ocpl_id, HDF5Constants.H5T_NATIVE_INT, fill_value);
            H5.H5Pget_fill_value(ocpl_id, HDF5Constants.H5T_NATIVE_INT, fill_value);
            assertTrue("fill_value: "+fill_value[0], fill_value[0] == 255);
            H5.H5Pfill_value_defined(ocpl_id, fill_value_status);
            assertTrue("fill_value_status: "+fill_value_status[0], fill_value_status[0] == HDF5Constants.H5D_FILL_VALUE_USER_DEFINED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fill_value: " + err);
        }
    }

    @Test
    public void testH5P_layout() {
        int layout_type = -1;

        try {
            layout_type = H5.H5Pget_layout(ocpl_id);
            assertTrue("layout: "+layout_type, layout_type == HDF5Constants.H5D_CONTIGUOUS);
            H5.H5Pset_layout(ocpl_id, HDF5Constants.H5D_COMPACT);
            layout_type = H5.H5Pget_layout(ocpl_id);
            assertTrue("layout: "+layout_type, layout_type == HDF5Constants.H5D_COMPACT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_layout: " + err);
        }
    }

    @Test
    public void testH5P_chunk() {
        long[] chunk_size = {0,0};
        long[] chunk_new_size = {2,3};
        int layout_type = -1;

        try {
            H5.H5Pset_chunk(ocpl_id, 2, chunk_new_size);
            H5.H5Pget_chunk(ocpl_id, 2, chunk_size);
            assertTrue("chunk: "+chunk_size[0], chunk_size[0] == chunk_new_size[0]);
            assertTrue("chunk: "+chunk_size[1], chunk_size[1] == chunk_new_size[1]);
            layout_type = H5.H5Pget_layout(ocpl_id);
            assertTrue("layout: "+layout_type, layout_type == HDF5Constants.H5D_CHUNKED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_chunk: " + err);
        }
    }

    @Test
    public void testH5P_file_space() {
        long[] threshold = {0};
        int[] strategy = {0};
        try {
            H5.H5Pget_file_space(fcpl_id, strategy, threshold);
            assertTrue("strategy: "+strategy[0], strategy[0] == HDF5Constants.H5F_FILE_SPACE_ALL);
            assertTrue("theshold: "+threshold[0], threshold[0] == 1);
            H5.H5Pset_file_space(fcpl_id, HDF5Constants.H5F_FILE_SPACE_ALL_PERSIST, 10);
            H5.H5Pget_file_space(fcpl_id, strategy, threshold);
            assertTrue("strategy: "+strategy[0], strategy[0] == HDF5Constants.H5F_FILE_SPACE_ALL_PERSIST);
            assertTrue("theshold: "+threshold[0], threshold[0] == 10);
            H5.H5Pset_file_space(fcpl_id, HDF5Constants.H5F_FILE_SPACE_VFD, 0);
            H5.H5Pget_file_space(fcpl_id, strategy, threshold);
            assertTrue("strategy: "+strategy[0], strategy[0] == HDF5Constants.H5F_FILE_SPACE_VFD);
            assertTrue("theshold: "+threshold[0], threshold[0] == 10);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5P_file_space: " + err);
        }
   }
}
