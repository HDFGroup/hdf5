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

import java.util.ArrayList;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.callbacks.H5O_iterate_cb;
import hdf.hdf5lib.callbacks.H5O_iterate_t;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5O_info_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Obasic {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "h5ex_g_iterate.hdf";
    private static long H5la_ds1 = -1;
    private static long H5la_l1 = -1;
    private static long H5la_dt1 = -1;
    private static long H5la_g1 = -1;
    long H5fid = -1;

    @Before
    public void openH5file()
            throws HDF5LibraryException, NullPointerException {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        try {
            H5fid = H5.H5Fopen(H5_FILE, HDF5Constants.H5F_ACC_RDONLY,
                HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Fopen: openH5file: " + err);
        }
    }

    @After
    public void closeH5file() throws HDF5LibraryException {
        if (H5fid > 0) {
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
        }
        System.out.println();
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oopen_not_exists() throws Throwable {
        long oid = -1;

        oid = H5.H5Oopen(H5fid, "Never_created", HDF5Constants.H5P_DEFAULT);

        try {H5.H5Oclose(oid);} catch (Exception ex) {}
    }

    @Test
    public void testH5Oget_info_dataset() {
        long oid = -1;
        H5O_info_t obj_info = null;

        try {
            oid = H5.H5Oopen(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
            obj_info = H5.H5Oget_info(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ",obj_info==null);
        assertTrue("H5Oget_info object type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
        try {H5.H5Oclose(oid);} catch (Exception ex) {}
    }

    @Test
    public void testH5Oget_info_hardlink() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            oid = H5.H5Oopen(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            obj_info = H5.H5Oget_info(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ",obj_info==null);
        assertTrue("H5Oget_info object type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
        try {H5.H5Oclose(oid);} catch (Exception ex) {}
    }

    @Test
    public void testH5Oget_info_group() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            oid = H5.H5Oopen(H5fid, "G1", HDF5Constants.H5P_DEFAULT);
            obj_info = H5.H5Oget_info(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ",obj_info==null);
        assertTrue("H5Oget_info object type",obj_info.type==HDF5Constants.H5O_TYPE_GROUP);
        try {H5.H5Oclose(oid);} catch (Exception ex) {}
    }

    @Test
    public void testH5Oget_info_datatype() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            oid = H5.H5Oopen(H5fid, "DT1", HDF5Constants.H5P_DEFAULT);
            obj_info = H5.H5Oget_info(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ",obj_info==null);
        assertTrue("H5Oget_info object type",obj_info.type==HDF5Constants.H5O_TYPE_NAMED_DATATYPE);
        try {H5.H5Oclose(oid);} catch (Exception ex) {}
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_by_name_not_exist_name() throws Throwable {
        H5.H5Oget_info_by_name(H5fid, "None", HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_by_name_not_exists() throws Throwable {
        H5.H5Oget_info_by_name(H5fid, "Bogus", HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Oget_info_by_name_dataset() {
        H5O_info_t obj_info = null;

        try {
            obj_info = H5.H5Oget_info_by_name(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ",obj_info==null);
        assertTrue("H5Oget_info object type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
    }

    @Test
    public void testH5Oget_info_by_name_hardlink() {
        H5O_info_t obj_info = null;
        try {
            obj_info = H5.H5Oget_info_by_name(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ",obj_info==null);
        assertTrue("H5Oget_info object type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
    }

    @Test
    public void testH5Oget_info_by_name_group() {
        H5O_info_t obj_info = null;
        try {
            obj_info = H5.H5Oget_info_by_name(H5fid, "G1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ",obj_info==null);
        assertTrue("H5Oget_info object type",obj_info.type==HDF5Constants.H5O_TYPE_GROUP);
    }

    @Test
    public void testH5Oget_info_by_name_datatype() {
        H5O_info_t obj_info = null;
        try {
            obj_info = H5.H5Oget_info_by_name(H5fid, "DT1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oget_info: " + err);
        }
        assertFalse("H5Oget_info ",obj_info==null);
        assertTrue("H5Oget_info object type",obj_info.type==HDF5Constants.H5O_TYPE_NAMED_DATATYPE);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_by_idx_name_not_exist_name() throws Throwable {
        H5.H5Oget_info_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_by_idx_name_not_exist_create() throws Throwable {
        H5.H5Oget_info_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_by_idx_not_exist_name() throws Throwable {
        H5.H5Oget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 5, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Oget_info_by_idx_not_exist_create() throws Throwable {
        H5.H5Oget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 5, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Oget_info_by_idx_n0() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            oid = H5.H5Oopen(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
            obj_info = H5.H5Oget_info(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Oget_info_by_idx_n0:H5.H5Oget_info: " + err);
        }
        H5la_ds1 = obj_info.addr;
        try {H5.H5Oclose(oid);} catch (Exception ex) {}
        try {
            obj_info = H5.H5Oget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Oget_info_by_idx_n0:H5.H5Oget_info_by_idx: " + err);
        }
        assertFalse("testH5Oget_info_by_idx_n0:H5Oget_info_by_idx ",obj_info==null);
        assertTrue("testH5Oget_info_by_idx_n0:H5Oget_info_by_idx link type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
        assertTrue("testH5Oget_info_by_idx_n0:Link Address ",obj_info.addr==H5la_ds1);
    }

    @Test
    public void testH5Oget_info_by_idx_n3() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            oid = H5.H5Oopen(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
            obj_info = H5.H5Oget_info(oid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Oget_info_by_idx_n3:H5.H5Oget_info: " + err);
        }
        H5la_l1 = obj_info.addr;
        try {H5.H5Oclose(oid);} catch (Exception ex) {}
        try {
            obj_info = H5.H5Oget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 3, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Oget_info_by_idx_n3:H5.H5Oget_info_by_idx: " + err);
        }
        assertFalse("testH5Oget_info_by_idx_n3:H5Oget_info_by_idx ",obj_info==null);
        assertTrue("testH5Oget_info_by_idx_n3:H5Oget_info_by_idx link type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
        assertTrue("testH5Oget_info_by_idx_n3:Link Address ",obj_info.addr==H5la_l1);
    }

    @Test
    public void testH5Ovisit() {
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
            H5.H5Ovisit(H5fid, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ovisit: " + err);
        }
        assertFalse("H5Ovisit ",((H5O_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Ovisit "+((H5O_iter_data)iter_data).iterdata.size(),((H5O_iter_data)iter_data).iterdata.size()==5);
        assertTrue("H5Ovisit "+(((H5O_iter_data)iter_data).iterdata.get(0)).link_name,(((H5O_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase(".")==0);
        assertTrue("H5Ovisit "+(((H5O_iter_data)iter_data).iterdata.get(1)).link_name,(((H5O_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("DS1")==0);
        assertTrue("H5Ovisit "+(((H5O_iter_data)iter_data).iterdata.get(2)).link_name,(((H5O_iter_data)iter_data).iterdata.get(2)).link_name.compareToIgnoreCase("DT1")==0);
        assertTrue("H5Ovisit "+(((H5O_iter_data)iter_data).iterdata.get(3)).link_name,(((H5O_iter_data)iter_data).iterdata.get(3)).link_name.compareToIgnoreCase("G1")==0);
        assertTrue("H5Ovisit "+(((H5O_iter_data)iter_data).iterdata.get(4)).link_name,(((H5O_iter_data)iter_data).iterdata.get(4)).link_name.compareToIgnoreCase("G1/DS2")==0);
//        assertTrue("H5Ovisit "+((idata)((H5O_iter_data)iter_data).iterdata.get(5)).link_name,((idata)((H5O_iter_data)iter_data).iterdata.get(5)).link_name.compareToIgnoreCase("L1")==0);
    }

    @Test
    public void testH5Ovisit_by_name() {
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
            H5.H5Ovisit_by_name(H5fid, "G1", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, iter_cb, iter_data, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ovisit_by_name: " + err);
        }
        assertFalse("H5Ovisit_by_name ",((H5O_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Ovisit_by_name "+((H5O_iter_data)iter_data).iterdata.size(),((H5O_iter_data)iter_data).iterdata.size()==2);
        assertTrue("H5Ovisit_by_name "+(((H5O_iter_data)iter_data).iterdata.get(0)).link_name,(((H5O_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase(".")==0);
        assertTrue("H5Ovisit_by_name "+(((H5O_iter_data)iter_data).iterdata.get(1)).link_name,(((H5O_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("DS2")==0);
    }

    @Test
    public void testH5Oexists_by_name() {
        boolean name_exists = false;
        try {
            name_exists = H5.H5Oexists_by_name(H5fid, "G1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Oexists_by_name: " + err);
        }
        assertTrue("H5Oexists_by_name ", name_exists);
        //TODO get dangling link result
    }

    @Test
    public void testH5Oopen_by_addr() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            try {
                oid = H5.H5Oopen(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
                obj_info = H5.H5Oget_info(oid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_addr: H5.H5Oget_info: " + err);
            }
            H5la_ds1 = obj_info.addr;
            try {H5.H5Oclose(oid);} catch (Exception ex) {}
            try {
                oid = H5.H5Oopen_by_addr(H5fid, H5la_ds1);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_addr: H5.H5Oopen_by_addr: " + err);
            }
            try {
                obj_info = H5.H5Oget_info(oid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_addr: H5.H5Oget_info: " + err);
            }
            assertFalse("testH5Oopen_by_addr: H5Oget_info ",obj_info==null);
            assertTrue("testH5Oopen_by_addr: H5Oget_info link type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
            assertTrue("testH5Oopen_by_addr: Link Address ",obj_info.addr==H5la_ds1);
        }
        finally {
            try{H5.H5Oclose(oid);} catch (Exception ex) {}
        }
   }

    @Test
    public void testH5Oopen_by_idx_n0() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            try {
                oid = H5.H5Oopen(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
                obj_info = H5.H5Oget_info(oid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_idx_n0: H5.H5Oget_info: " + err);
            }
            H5la_ds1 = obj_info.addr;
            try {H5.H5Oclose(oid);} catch (Exception ex) {}
            try {
                oid = H5.H5Oopen_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_addr: H5.H5Oopen_by_addr: " + err);
            }
            try {
                obj_info = H5.H5Oget_info(oid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_idx_n0: H5.H5Oget_info_by_idx: " + err);
            }
            assertFalse("testH5Oopen_by_idx_n0: H5Oget_info_by_idx ",obj_info==null);
            assertTrue("testH5Oopen_by_idx_n0: H5Oget_info_by_idx link type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
            assertTrue("testH5Oopen_by_idx_n0: Link Address ",obj_info.addr==H5la_ds1);
        }
        finally {
            try{H5.H5Oclose(oid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Oopen_by_idx_n3() {
        long oid = -1;
        H5O_info_t obj_info = null;
        try {
            try {
                oid = H5.H5Oopen(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
                obj_info = H5.H5Oget_info(oid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_idx_n3:H5.H5Oget_info: " + err);
            }
            H5la_l1 = obj_info.addr;
            try {H5.H5Oclose(oid);} catch (Exception ex) {}
            try {
                oid = H5.H5Oopen_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 3, HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_addr: H5.H5Oopen_by_addr: " + err);
            }
            try {
                obj_info = H5.H5Oget_info(oid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Oopen_by_idx_n3:H5.H5Oget_info_by_idx: " + err);
            }
            assertFalse("testH5Oopen_by_idx_n3:H5Oget_info_by_idx ",obj_info==null);
            assertTrue("testH5Oopen_by_idx_n3:H5Oget_info_by_idx link type",obj_info.type==HDF5Constants.H5O_TYPE_DATASET);
            assertTrue("testH5Oopen_by_idx_n3:Link Address ",obj_info.addr==H5la_l1);
        }
        finally {
            try{H5.H5Oclose(oid);} catch (Exception ex) {}
        }
    }
}
