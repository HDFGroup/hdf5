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
import hdf.hdf5lib.callbacks.H5O_iterate_cb;
import hdf.hdf5lib.callbacks.H5O_iterate_t;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5O_info_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Ocreate {
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
        assertTrue("TestH5O._createDataset: ",did > 0);

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
        assertTrue("TestH5O._createGroup: ",gid > 0);

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
        assertTrue("TestH5O._createHardLink ", link_exists);
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
        assertTrue("TestH5O._createSoftLink ", link_exists);
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
        assertTrue("TestH5O._createExternalLink ", link_exists);
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
            fail("TestH5O.createH5file: " + err);
        }
        assertTrue("TestH5O.createH5file: H5.H5Fcreate: ",H5fid > 0);
        assertTrue("TestH5O.createH5file: H5.H5Screate_simple: ",H5dsid > 0);
        assertTrue("TestH5O.createH5file: H5.H5Gcreate: ",H5gid > 0);

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

    @Test(expected = HDF5LibraryException.class)
    public void testH5Ocopy_cur_not_exists() throws Throwable {
        H5.H5Ocopy(H5fid, "None", H5fid, "DS1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Ocopy() {
        try {
            H5.H5Ocopy(H5fid, "DS1", H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
            boolean link_exists = H5.H5Lexists(H5fid, "CPY1", HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Ocopy:H5Lexists ",link_exists);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ocopy: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Ocopy_dst_link_exists() throws Throwable {
        _createHardLink(H5fid, H5fid, "/G1/DS2", H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        H5.H5Ocopy(H5fid, "CPY1", H5fid, "/G1/DS2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Oget_info_by_idx_n0_create() {
        H5O_info_t obj_info = null;
        try {
            int order = H5.H5Pget_link_creation_order(H5fcpl);
            assertTrue("creation order :"+order, order == HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info_by_idx_n0:H5Pget_link_creation_order " + err);
        }
        try {
            obj_info = H5.H5Oget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info_by_idx: " + err);
        }
        assertFalse("H5Oget_info_by_idx ", obj_info==null);
        assertTrue("H5Oget_info_by_idx link type", obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
    }

    @Test
    public void testH5Oget_info_by_idx_n1_create() {
        H5O_info_t obj_info = null;
        try {
            int order = H5.H5Pget_link_creation_order(H5fcpl);
            assertTrue("creation order :"+order, order == HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info_by_idx_n1:H5Pget_link_creation_order " + err);
        }
        try {
            obj_info = H5.H5Oget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 1, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info_by_idx: " + err);
        }
        assertFalse("H5Oget_info_by_idx ", obj_info==null);
        assertTrue("H5Oget_info_by_idx link type", obj_info.type==HDF5Constants.H5O_TYPE_GROUP);
    }

    @Test
    public void testH5Oget_info_softlink() {
        H5O_info_t obj_info = null;
        _createSoftLink(H5fid, "/G1/DS2", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            obj_info = H5.H5Oget_info_by_name(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ", obj_info==null);
        assertTrue("H5Oget_info link type", obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
        assertTrue("Link Address ", obj_info.addr>0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_softlink_dangle() throws Throwable {
        _createSoftLink(H5fid, "DS3", H5fid, "L2", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        H5.H5Oget_info_by_name(H5fid, "L2", HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Oget_info_externallink() {
        H5O_info_t obj_info = null;
        _createExternalLink(H5fid, H5_EXTFILE, "DT1", H5fid, "L1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        try {
            obj_info = H5.H5Oget_info_by_name(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ", obj_info==null);
        assertTrue("H5Oget_info link type", obj_info.type==HDF5Constants.H5O_TYPE_NAMED_DATATYPE);
        assertTrue("Link Address ", obj_info.addr>0);
    }

    @Test
    public void testH5Olink() {
        long oid = -1;
        H5O_info_t obj_info = null;
        H5O_info_t dst_obj_info = null;
        try {
            oid = H5.H5Oopen(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
            obj_info = H5.H5Oget_info(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        try {
            H5.H5Olink(oid, H5fid, "CPY1", HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Olink: " + err);
        }
        try {H5.H5Oclose(oid);} catch (Exception ex) {}

        assertFalse("H5Oget_info ", obj_info==null);
        assertTrue("H5Oget_info object type", obj_info.type==HDF5Constants.H5O_TYPE_DATASET);

        try {
            dst_obj_info = H5.H5Oget_info_by_name(H5fid, "CPY1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info_by_name: " + err);
        }
        assertFalse("H5Oget_info ", dst_obj_info==null);
        assertTrue("H5Oget_info object type", dst_obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
    }

    @Test
    public void testH5Ovisit_create() {
        try {
            int order = H5.H5Pget_link_creation_order(H5fcpl);
            assertTrue("creation order :"+order, order == HDF5Constants.H5P_CRT_ORDER_TRACKED+HDF5Constants.H5P_CRT_ORDER_INDEXED);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ovisit_create:H5Pget_link_creation_order " + err);
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
        class H5O_iter_data implements H5O_iterate_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5O_iterate_t iter_data = new H5O_iter_data();
        class H5O_iter_callback implements H5O_iterate_cb {
            public int callback(long group, String name, H5O_info_t info, H5O_iterate_t op_data) {
                idata id = new idata(name, info.type);
                ((H5O_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        H5O_iterate_cb iter_cb = new H5O_iter_callback();
        try {
            H5.H5Ovisit(H5fid, HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ovisit: " + err);
        }
        assertFalse("H5Ovisit ", ((H5O_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Ovisit "+((H5O_iter_data)iter_data).iterdata.size(), ((H5O_iter_data)iter_data).iterdata.size()==4);
        assertTrue("H5Ovisit "+((idata)((H5O_iter_data)iter_data).iterdata.get(0)).link_name, ((idata)((H5O_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase(".")==0);
        assertTrue("H5Ovisit "+((idata)((H5O_iter_data)iter_data).iterdata.get(1)).link_name, ((idata)((H5O_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("DS1")==0);
        assertTrue("H5Ovisit "+((idata)((H5O_iter_data)iter_data).iterdata.get(2)).link_name, ((idata)((H5O_iter_data)iter_data).iterdata.get(2)).link_name.compareToIgnoreCase("G1")==0);
        assertTrue("H5Ovisit "+((idata)((H5O_iter_data)iter_data).iterdata.get(3)).link_name, ((idata)((H5O_iter_data)iter_data).iterdata.get(3)).link_name.compareToIgnoreCase("G1/DS2")==0);
    }

    @Test
    public void testH5Ocomment() {
        long oid = -1;
        String obj_comment = null;
        try {
            oid = H5.H5Oopen(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
            H5.H5Oset_comment(oid, "Test Comment");
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oset_comment: " + err);
        }
        try {
            obj_comment = H5.H5Oget_comment(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_comment: " + err);
        }
        try {H5.H5Oclose(oid);} catch (Exception ex) {}
        assertFalse("H5Oget_comment: ", obj_comment==null);
        assertTrue("H5Oget_comment: ", obj_comment.compareTo("Test Comment")==0);
    }

    @Test
    public void testH5Ocomment_clear() {
        long oid = -1;
        String obj_comment = null;
        try {
            oid = H5.H5Oopen(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
            H5.H5Oset_comment(oid, "Test Comment");
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oset_comment: " + err);
        }
        try {
            obj_comment = H5.H5Oget_comment(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_comment: " + err);
        }
        assertFalse("H5Oget_comment: ", obj_comment==null);
        assertTrue("H5Oget_comment: ", obj_comment.compareTo("Test Comment")==0);
        try {
            H5.H5Oset_comment(oid, null);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oset_comment: " + err);
        }
        try {
            obj_comment = H5.H5Oget_comment(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_comment: " + err);
        }
        try {H5.H5Oclose(oid);} catch (Exception ex) {}
        assertTrue("H5Oget_comment: ", obj_comment==null);
    }

    @Test
    public void testH5Ocomment_by_name() {
        String obj_comment = null;
        try {
            H5.H5Oset_comment_by_name(H5fid, "DS1", "Test Comment", HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oset_comment_by_name: " + err);
        }
        try {
            obj_comment = H5.H5Oget_comment_by_name(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_comment_by_name: " + err);
        }
        assertFalse("H5Oget_comment_by_name: ", obj_comment==null);
        assertTrue("H5Oget_comment_by_name: ", obj_comment.compareTo("Test Comment")==0);
    }

    @Test
    public void testH5Ocomment_by_name_clear() {
        String obj_comment = null;
        try {
            H5.H5Oset_comment_by_name(H5fid, "DS1", "Test Comment", HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oset_comment_by_name: " + err);
        }
        try {
            obj_comment = H5.H5Oget_comment_by_name(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_comment_by_name: " + err);
        }
        assertFalse("H5Oget_comment_by_name: ", obj_comment==null);
        assertTrue("H5Oget_comment_by_name: ", obj_comment.compareTo("Test Comment")==0);
        try {
            H5.H5Oset_comment_by_name(H5fid, "DS1", null, HDF5Constants.H5P_DEFAULT);
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oset_comment_by_name: " + err);
        }
        try {
            obj_comment = H5.H5Oget_comment_by_name(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_comment_by_name: " + err);
        }
        assertTrue("H5Oget_comment_by_name: ", obj_comment==null);
    }

    @Test
    public void testH5Oinc_dec_count() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            try {
                oid = H5.H5Oopen(H5fid, "G1", HDF5Constants.H5P_DEFAULT);
                obj_info = H5.H5Oget_info(oid);
                assertFalse("testH5Oinc_dec_count: H5Oget_info ",obj_info==null);
                assertTrue("testH5Oinc_dec_count: H5Oget_info reference count",obj_info.rc==1);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oinc_dec_count: H5.H5Oget_info: " + err);
            }
            try {
                H5.H5Oincr_refcount(oid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oinc_dec_count: H5.H5Oincr_refcount: " + err);
            }
            try {
                obj_info = H5.H5Oget_info(oid);
                assertFalse("testH5Oinc_dec_count: H5Oget_info ",obj_info==null);
                assertTrue("testH5Oinc_dec_count: H5Oget_info reference count",obj_info.rc==2);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oinc_dec_count: H5.H5Oget_info: " + err);
            }
            try {
                H5.H5Odecr_refcount(oid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oinc_dec_count: H5.H5Odecr_refcount: " + err);
            }
            try {
                obj_info = H5.H5Oget_info(oid);
                assertFalse("testH5Oinc_dec_count: H5Oget_info ",obj_info==null);
                assertTrue("testH5Oinc_dec_count: H5Oget_info reference count",obj_info.rc==1);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oinc_dec_count: H5.H5Oget_info: " + err);
            }
        }
        finally {
            try{H5.H5Oclose(oid);} catch (Exception ex) {}
        }
    }

}
