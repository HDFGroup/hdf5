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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.ArrayList;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.callbacks.H5L_iterate_cb;
import hdf.hdf5lib.callbacks.H5L_iterate_t;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5L_info_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Lcreate {
    @Rule public TestName testname = new TestName();
    private static final String H5_EXTFILE = "h5ex_g_iterate.hdf";
    private static final String H5_FILE = "test.h5";
    private static final int DIM_X = 4;
    private static final int DIM_Y = 6;
    long H5fcpl = -1;
    long H5fid = -1;
    long H5dsid = -1;
    long H5did1 = -1;
    long H5did2 = -1;
    long H5gcpl = -1;
    long H5gid = -1;
    long[] H5dims = { DIM_X, DIM_Y };

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {
                file.delete();
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
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
        assertTrue("TestH5L._createDataset: ",did > 0);

        return did;
    }

    private final long _createGroup(long fid, String name) {
        long gid = -1;
        try {
            H5gcpl = HDF5Constants.H5P_DEFAULT;
            gid = H5.H5Gcreate(fid, name, HDF5Constants.H5P_DEFAULT,
                    H5gcpl, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate: " + err);
        }
        assertTrue("TestH5L._createGroup: ",gid > 0);

        return gid;
    }

    private final void _createHardLink(long fid, long cid, String curname, long did, String dstname, long lcpl, long lapl) {
        boolean link_exists = false;
        try {
            H5.H5Lcreate_hard(cid, curname, did, dstname, lcpl, lapl);
            H5.H5Fflush(fid, HDF5Constants.H5F_SCOPE_LOCAL);
            link_exists = H5.H5Lexists(did, dstname, lapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lcreate_hard: " + err);
        }
        assertTrue("TestH5L._createHardLink ", link_exists);
    }

    private final void _createSoftLink(long fid, String curname, long did, String dstname, long lcpl, long lapl) {
        boolean link_exists = false;
        try {
            H5.H5Lcreate_soft(curname, did, dstname, lcpl, lapl);
            H5.H5Fflush(fid, HDF5Constants.H5F_SCOPE_LOCAL);
            link_exists = H5.H5Lexists(did, dstname, lapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lcreate_soft: " + err);
        }
        assertTrue("TestH5L._createSoftLink ", link_exists);
    }

    private final void _createExternalLink(long fid, String ext_filename, String curname, long did, String dstname, long lcpl, long lapl) {
        boolean link_exists = false;
        try {
            H5.H5Lcreate_external(ext_filename, curname, did, dstname, lcpl, lapl);
            H5.H5Fflush(fid, HDF5Constants.H5F_SCOPE_LOCAL);
            link_exists = H5.H5Lexists(did, dstname, lapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lcreate_external: " + err);
        }
        assertTrue("TestH5L._createExternalLink ", link_exists);
    }

    @Before
    public void createH5file()
            throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());
        try {
            H5fcpl = H5.H5Pcreate(HDF5Constants.H5P_FILE_CREATE);
            H5.H5Pset_link_creation_order(H5fcpl, HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC,
                    H5fcpl, HDF5Constants.H5P_DEFAULT);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            H5did1 = _createDataset(H5fid, H5dsid, "DS1", HDF5Constants.H5P_DEFAULT);
            H5gid = _createGroup(H5fid, "/G1");
            H5did2 = _createDataset(H5gid, H5dsid, "DS2", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5L.createH5file: " + err);
        }
        assertTrue("TestH5L.createH5file: H5.H5Fcreate: ",H5fid > 0);
        assertTrue("TestH5L.createH5file: H5.H5Screate_simple: ",H5dsid > 0);
        assertTrue("TestH5L.createH5file: H5.H5Gcreate: ",H5gid > 0);

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5gid > 0)
            try {H5.H5Gclose(H5gid);} catch (Exception ex) {}
        if (H5gcpl > 0)
            try {H5.H5Pclose(H5gcpl);} catch (Exception ex) {}
        if (H5did2 > 0)
            try {H5.H5Dclose(H5did2);} catch (Exception ex) {}
        if (H5dsid > 0)
            try {H5.H5Sclose(H5dsid);} catch (Exception ex) {}
        if (H5did1 > 0)
            try {H5.H5Dclose(H5did1);} catch (Exception ex) {}
        if (H5fid > 0)
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
        if (H5fcpl > 0)
            try {H5.H5Pclose(H5fcpl);} catch (Exception ex) {}

        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test
    public void testH5Lget_info_by_idx_n0_create() {
        H5L_info_t link_info = null;
        try {
            int order = H5.H5Pget_link_creation_order(H5fcpl);
            assertTrue("creation order :"+order, order == HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx_n0_create:H5Pget_link_creation_order " + err);
        }
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("H5Lget_info_by_idx ", link_info==null);
        assertTrue("H5Lget_info_by_idx link type", link_info.type==HDF5Constants.H5L_TYPE_HARD);
    }

    @Test
    public void testH5Lget_info_by_idx_n1_create() {
        H5L_info_t link_info = null;
        try {
            int order = H5.H5Pget_link_creation_order(H5fcpl);
            assertTrue("creation order :"+order, order == HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx_n1_create:H5Pget_link_creation_order " + err);
        }
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 1, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("H5Lget_info_by_idx ", link_info==null);
        assertTrue("H5Lget_info_by_idx link type", link_info.type==HDF5Constants.H5L_TYPE_HARD);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcreate_hard_cur_not_exists() throws Throwable {
        H5.H5Lcreate_hard(H5fid, "None", H5fid, "DS1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Lcreate_hard() {
        try {
            H5.H5Lcreate_hard(H5fid, "DS1", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Lcreate_hard:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcreate_hard_dst_link_exists() throws Throwable {
        _createHardLink(H5fid, H5fid, "/G1/DS2", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        H5.H5Lcreate_hard(H5fid, "L1", H5fid, "/G1/DS2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Ldelete_hard_link() {
        _createHardLink(H5fid, H5fid, "/G1/DS2", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            H5.H5Ldelete(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            assertFalse("testH5Lcreate_hard:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
    }

    @Test
    public void testH5Lcreate_soft() {
        try {
            H5.H5Lcreate_soft("DS1", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Lcreate_soft:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcreate_soft_dst_link_exists() throws Throwable {
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        H5.H5Lcreate_soft("L1", H5fid, "/G1/DS2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Ldelete_soft_link() {
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            H5.H5Ldelete(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            assertFalse("testH5Lcreate_soft:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
    }

    @Test
    public void testH5Lget_info_softlink() {
        H5L_info_t link_info = null;
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info: " + err);
        }
        assertFalse("H5Lget_info ", link_info==null);
        assertTrue("H5Lget_info link type", link_info.type==HDF5Constants.H5L_TYPE_SOFT);
        assertTrue("Link Address ", link_info.address_val_size>0);
    }

    @Test
    public void testH5Lget_value_soft() {
        String[] link_value = {null, null};
        int link_type = -1;

        _createSoftLink(H5fid, "/G1/DS2", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_type = H5.H5Lget_value(H5fid, "L1", link_value, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_value: " + err);
        }
        assertTrue("Link Type", link_type == HDF5Constants.H5L_TYPE_SOFT);
        assertFalse("H5Lget_value ", link_value[0]==null);
        assertTrue("Link Value ", link_value[0].compareTo("/G1/DS2")==0);
    }

    @Test
    public void testH5Lcreate_soft_dangle() {
        try {
            H5.H5Lcreate_soft("DS3", H5fid, "L2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "L2", HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Lcreate_soft:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
    }

    @Test
    public void testH5Ldelete_soft_link_dangle() {
        _createSoftLink(H5fid, "DS3", H5fid, "L2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            H5.H5Ldelete(H5fid, "L2", HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "L2", HDF5Constants.H5P_DEFAULT);
            assertFalse("testH5Lcreate_soft:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
    }

    @Test
    public void testH5Lget_info_softlink_dangle() {
        H5L_info_t link_info = null;
        _createSoftLink(H5fid, "DS3", H5fid, "L2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info(H5fid, "L2", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info: " + err);
        }
        assertFalse("H5Lget_info ", link_info==null);
        assertTrue("H5Lget_info link type", link_info.type==HDF5Constants.H5L_TYPE_SOFT);
        assertTrue("Link Address ", link_info.address_val_size>0);
    }

    @Test
    public void testH5Lget_value_dangle() {
        String[] link_value = {null,null};
        int link_type = -1;

        _createSoftLink(H5fid, "DS3", H5fid, "L2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_type = H5.H5Lget_value(H5fid, "L2", link_value, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_value: " + err);
        }
        assertTrue("Link Type", link_type == HDF5Constants.H5L_TYPE_SOFT);
        assertFalse("H5Lget_value ", link_value[0]==null);
        assertTrue("Link Value ", link_value[0].compareTo("DS3")==0);
    }

    @Test
    public void testH5Lcreate_external() {
        try {
            H5.H5Lcreate_external(H5_EXTFILE, "DT1", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Lcreate_external:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
    }

    @Test
    public void testH5Lget_info_externallink() {
        H5L_info_t link_info = null;
        _createExternalLink(H5fid, H5_EXTFILE, "DT1", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info: " + err);
        }
        assertFalse("H5Lget_info ", link_info==null);
        assertTrue("H5Lget_info link type", link_info.type==HDF5Constants.H5L_TYPE_EXTERNAL);
        assertTrue("Link Address ", link_info.address_val_size>0);
    }

    @Test
    public void testH5Lget_value_external(){
        String[] link_value = {null,null};
        int link_type = -1;

        _createExternalLink(H5fid, H5_EXTFILE, "DT1", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_type = H5.H5Lget_value(H5fid, "L1", link_value, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_value: " + err);
        }
        assertTrue("Link Type", link_type == HDF5Constants.H5L_TYPE_EXTERNAL);
        assertFalse("H5Lget_value ", link_value[0]==null);
        assertFalse("H5Lget_value ", link_value[1]==null);
        assertTrue("Link Value ", link_value[0].compareTo("DT1")==0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcopy_cur_not_exists() throws Throwable {
        H5.H5Lcopy(H5fid, "None", H5fid, "DS1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Lcopy() {
        try {
            H5.H5Lcopy(H5fid, "DS1", H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "CPY1", HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Lcopy:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Lcopy:H5Lexists: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lcopy_dst_link_exists() throws Throwable {
        _createHardLink(H5fid, H5fid, "/G1/DS2", H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        H5.H5Lcopy(H5fid, "CPY1", H5fid, "/G1/DS2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lmove_cur_not_exists() throws Throwable {
        H5.H5Lmove(H5fid, "None", H5fid, "DS1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Lmove() {
        try {
            H5.H5Lmove(H5fid, "DS1", H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "CPY1", HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Lmove:H5Lexists ", link_exists);
            link_exists = H5.H5Lexists(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
            assertFalse("testH5Lmove:H5Lexists ", link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Lmove:H5Lexists: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lmove_dst_link_exists() throws Throwable {
        _createHardLink(H5fid, H5fid, "/G1/DS2", H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        H5.H5Lmove(H5fid, "CPY1", H5fid, "/G1/DS2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_value_by_idx_not_exist_name() throws Throwable {
        String[] link_value = {null,null};
        H5.H5Lget_value_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0, link_value, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_value_by_idx_not_exist_create() throws Throwable {
        String[] link_value = {null,null};
        H5.H5Lget_value_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0, link_value, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Lget_value_by_idx_n2_name() {
        H5L_info_t link_info = null;
        String[] link_value = {null,null};
        int link_type = -1;

        _createSoftLink(H5fid, "/G1/DS2", H5fid, "LS", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("testH5Lget_value_by_idx_n2 ",link_info==null);
        assertTrue("testH5Lget_value_by_idx_n2 link type", link_info.type==HDF5Constants.H5L_TYPE_SOFT);
        try {
            link_type = H5.H5Lget_value_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 2, link_value, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_value_by_idx: " + err);
        }
        assertTrue("Link Type", link_type == HDF5Constants.H5L_TYPE_SOFT);
        assertFalse("testH5Lget_value_by_idx_n2 ", link_value[0]==null);
        assertTrue("testH5Lget_value_by_idx_n2 Link Value ", link_value[0].compareTo("/G1/DS2")==0);
    }

    @Test
    public void testH5Lget_value_by_idx_n2_create() {
        H5L_info_t link_info = null;
        String[] link_value = {null,null};
        int link_type = -1;

        try {
            int order = H5.H5Pget_link_creation_order(H5fcpl);
            assertTrue("creation order :"+order, order == HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_value_by_idx_n2_create: H5Pget_link_creation_order " + err);
        }
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "LS", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("testH5Lget_value_by_idx_n2 ", link_info==null);
        assertTrue("testH5Lget_value_by_idx_n2 link type", link_info.type==HDF5Constants.H5L_TYPE_SOFT);
        try {
            link_type = H5.H5Lget_value_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 2, link_value, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_value_by_idx: " + err);
        }
        assertTrue("Link Type", link_type == HDF5Constants.H5L_TYPE_SOFT);
        assertFalse("testH5Lget_value_by_idx_n2 ", link_value[0]==null);
        assertTrue("testH5Lget_value_by_idx_n2 Link Value ", link_value[0].compareTo("/G1/DS2")==0);
    }

    @Test
    public void testH5Lget_value_by_idx_external_name() {
        H5L_info_t link_info = null;
        String[] link_value = {null,null};
        int link_type = -1;

        _createExternalLink(H5fid, H5_EXTFILE, "DT1", H5fid, "LE", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("testH5Lget_value_by_idx_ext ", link_info==null);
        assertTrue("testH5Lget_value_by_idx_ext link type "+link_info.type, link_info.type==HDF5Constants.H5L_TYPE_EXTERNAL);
        try {
            link_type = H5.H5Lget_value_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 2, link_value, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_value_by_idx: " + err);
        }
        assertTrue("Link Type", link_type == HDF5Constants.H5L_TYPE_EXTERNAL);
        assertFalse("testH5Lget_value_by_idx_ext ", link_value[0]==null);
        assertFalse("testH5Lget_value_by_idx_ext ", link_value[1]==null);
        assertTrue("testH5Lget_value_by_idx_ext Link Value ", link_value[0].compareTo("DT1")==0);
    }

    @Test
    public void testH5Lget_value_by_idx_external_create() {
        H5L_info_t link_info = null;
        String[] link_value = {null,null};
        int link_type = -1;

        _createExternalLink(H5fid, H5_EXTFILE, "DT1", H5fid, "LE", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("testH5Lget_value_by_idx_ext ", link_info==null);
        assertTrue("testH5Lget_value_by_idx_ext link type "+link_info.type, link_info.type==HDF5Constants.H5L_TYPE_EXTERNAL);
        try {
            link_type = H5.H5Lget_value_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 2, link_value, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_value_by_idx: " + err);
        }
        assertTrue("Link Type", link_type == HDF5Constants.H5L_TYPE_EXTERNAL);
        assertFalse("testH5Lget_value_by_idx_ext ", link_value[0]==null);
        assertFalse("testH5Lget_value_by_idx_ext ", link_value[1]==null);
        assertTrue("testH5Lget_value_by_idx_ext Link Value ", link_value[0].compareTo("DT1")==0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Ldelete_by_idx_not_exist_name() throws Throwable {
        H5.H5Ldelete_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Ldelete_by_idx_not_exist_create() throws Throwable {
        H5.H5Ldelete_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Ldelete_by_idx_n2_name() {
        H5L_info_t link_info = null;
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "LS", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("testH5Ldelete_by_idx_n2 ", link_info==null);
        assertTrue("testH5Ldelete_by_idx_n2 link type", link_info.type==HDF5Constants.H5L_TYPE_SOFT);
        try {
            H5.H5Ldelete_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ldelete_by_idx: " + err);
        }
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (HDF5LibraryException err) {
            link_info = null;
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ldelete_by_idx: " + err);
        }
        assertTrue("testH5Ldelete_by_idx_n2 ",link_info==null);
    }

    @Test
    public void testH5Ldelete_by_idx_n2_create() {
        H5L_info_t link_info = null;
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "LS", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("testH5Ldelete_by_idx_n2 ", link_info==null);
        assertTrue("testH5Ldelete_by_idx_n2 link type", link_info.type==HDF5Constants.H5L_TYPE_SOFT);
        try {
            H5.H5Ldelete_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ldelete_by_idx: " + err);
        }
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 2, HDF5Constants.H5P_DEFAULT);
        }
        catch (HDF5LibraryException err) {
            link_info = null;
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ldelete_by_idx: " + err);
        }
        assertTrue("testH5Ldelete_by_idx_n2 ",link_info==null);
    }

    @Test
    public void testH5Lvisit_create() {
        try {
            int order = H5.H5Pget_link_creation_order(H5fcpl);
            assertTrue("creation order :"+order, order == HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lvisit_create: H5Pget_link_creation_order " + err);
        }

        _createHardLink(H5fid, H5fid, "/G1/DS2", H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        _createExternalLink(H5fid, H5_EXTFILE, "DT1", H5fid, "LE", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "LS", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);

        class idata {
            public String link_name = null;
            public int link_type = -1;
            idata(String name, int type) {
                this.link_name = name;
                this.link_type = type;
            }
        }
        class H5L_iter_data implements H5L_iterate_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5L_iterate_t iter_data = new H5L_iter_data();
        class H5L_iter_callback implements H5L_iterate_cb {
            public int callback(long group, String name, H5L_info_t info, H5L_iterate_t op_data) {
                idata id = new idata(name, info.type);
                ((H5L_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        H5L_iterate_cb iter_cb = new H5L_iter_callback();
        try {
            H5.H5Lvisit(H5fid, HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lvisit: " + err);
        }
        assertFalse("H5Lvisit ",((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Lvisit "+((H5L_iter_data)iter_data).iterdata.size(),((H5L_iter_data)iter_data).iterdata.size()==6);
        assertTrue("H5Lvisit "+((idata)((H5L_iter_data)iter_data).iterdata.get(0)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS1")==0);
        assertTrue("H5Lvisit "+((idata)((H5L_iter_data)iter_data).iterdata.get(1)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("G1")==0);
        assertTrue("H5Lvisit "+((idata)((H5L_iter_data)iter_data).iterdata.get(2)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(2)).link_name.compareToIgnoreCase("G1/DS2")==0);
        assertTrue("H5Lvisit "+((idata)((H5L_iter_data)iter_data).iterdata.get(3)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(3)).link_name.compareToIgnoreCase("CPY1")==0);
        assertTrue("H5Lvisit "+((idata)((H5L_iter_data)iter_data).iterdata.get(4)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(4)).link_name.compareToIgnoreCase("LE")==0);
        assertTrue("H5Lvisit "+((idata)((H5L_iter_data)iter_data).iterdata.get(5)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(5)).link_name.compareToIgnoreCase("LS")==0);
    }

    @Test
    public void testH5Literate_create() {
        try {
            int order = H5.H5Pget_link_creation_order(H5fcpl);
            assertTrue("creation order :"+order, order == HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Literate_create: H5Pget_link_creation_order " + err);
        }

        _createHardLink(H5fid, H5fid, "/G1/DS2", H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        _createExternalLink(H5fid, H5_EXTFILE, "DT1", H5fid, "LE", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "LS", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);

        class idata {
            public String link_name = null;
            public int link_type = -1;
            idata(String name, int type) {
                this.link_name = name;
                this.link_type = type;
            }
        }
        class H5L_iter_data implements H5L_iterate_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5L_iterate_t iter_data = new H5L_iter_data();
        class H5L_iter_callback implements H5L_iterate_cb {
            public int callback(long group, String name, H5L_info_t info, H5L_iterate_t op_data) {
                idata id = new idata(name, info.type);
                ((H5L_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        H5L_iterate_cb iter_cb = new H5L_iter_callback();
        try {
            H5.H5Literate(H5fid, HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Literate: " + err);
        }
        assertFalse("H5Literate ",((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Literate "+((H5L_iter_data)iter_data).iterdata.size(),((H5L_iter_data)iter_data).iterdata.size()==5);
        assertTrue("H5Literate "+((idata)((H5L_iter_data)iter_data).iterdata.get(0)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS1")==0);
        assertTrue("H5Literate "+((idata)((H5L_iter_data)iter_data).iterdata.get(1)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("G1")==0);
        assertTrue("H5Literate "+((idata)((H5L_iter_data)iter_data).iterdata.get(2)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(2)).link_name.compareToIgnoreCase("CPY1")==0);
        assertTrue("H5Literate "+((idata)((H5L_iter_data)iter_data).iterdata.get(3)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(3)).link_name.compareToIgnoreCase("LE")==0);
        assertTrue("H5Literate "+((idata)((H5L_iter_data)iter_data).iterdata.get(4)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(4)).link_name.compareToIgnoreCase("LS")==0);
    }

}
