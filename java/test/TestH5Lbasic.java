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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.callbacks.H5L_iterate_cb;
import hdf.hdf5lib.callbacks.H5L_iterate_t;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5L_info_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Lbasic {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "h5ex_g_iterateL1.hdf";
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

    @Test
    public void testH5Lexists() {
        boolean link_exists = false;
        try {
            link_exists = H5.H5Lexists(H5fid, "None", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
        assertFalse("H5Lexists ",link_exists);
        try {
            link_exists = H5.H5Lexists(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
        assertTrue("H5Lexists ",link_exists);
        try {
            link_exists = H5.H5Lexists(H5fid, "G1/DS2", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lexists: " + err);
        }
        assertTrue("H5Lexists ",link_exists);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_not_exist() throws Throwable {
        H5.H5Lget_info(H5fid, "None", HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Lget_info_dataset() {
        H5L_info_t link_info = null;
        try {
            link_info = H5.H5Lget_info(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info: " + err);
        }
        assertFalse("H5Lget_info ",link_info==null);
        assertTrue("H5Lget_info link type",link_info.type==HDF5Constants.H5L_TYPE_HARD);
    }

    @Test
    public void testH5Lget_info_hardlink() {
        H5L_info_t link_info = null;
        try {
            link_info = H5.H5Lget_info(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info: " + err);
        }
        assertFalse("H5Lget_info ",link_info==null);
        assertTrue("H5Lget_info link type",link_info.type==HDF5Constants.H5L_TYPE_HARD);
        assertTrue("Link Address ",link_info.address_val_size>0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx_name_not_exist_name() throws Throwable {
        H5.H5Lget_info_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx_name_not_exist_create() throws Throwable {
        H5.H5Lget_info_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx_not_exist_name() throws Throwable {
        H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 5, HDF5Constants.H5P_DEFAULT);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx_not_exist_create() throws Throwable {
        H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 5, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Lget_info_by_idx_n0() {
        H5L_info_t link_info = null;
        H5L_info_t link_info2 = null;
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("H5Lget_info_by_idx ",link_info==null);
        assertTrue("H5Lget_info_by_idx link type",link_info.type==HDF5Constants.H5L_TYPE_HARD);
        try {
            link_info2 = H5.H5Lget_info(H5fid, "DS1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info: " + err);
        }
        assertTrue("Link Address ",link_info.address_val_size==link_info2.address_val_size);
    }

    @Test
    public void testH5Lget_info_by_idx_n3() {
        H5L_info_t link_info = null;
        H5L_info_t link_info2 = null;
        try {
            link_info = H5.H5Lget_info_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 3, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info_by_idx: " + err);
        }
        assertFalse("H5Lget_info_by_idx ",link_info==null);
        assertTrue("H5Lget_info_by_idx link type",link_info.type==HDF5Constants.H5L_TYPE_HARD);
        try {
            link_info2 = H5.H5Lget_info(H5fid, "L1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_info: " + err);
        }
        assertTrue("Link Address ",link_info.address_val_size==link_info2.address_val_size);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_name_by_idx_not_exist() throws Throwable {
        H5.H5Lget_name_by_idx(H5fid, "None", HDF5Constants.H5_INDEX_CRT_ORDER, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
    }

    @Test
    public void testH5Lget_name_by_idx_n0() {
        String link_name = null;
        try {
            link_name = H5.H5Lget_name_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_name_by_idx: " + err);
        }
        assertFalse("H5Lget_name_by_idx ",link_name==null);
        assertTrue("Link Name ",link_name.compareTo("DS1")==0);
    }

    @Test
    public void testH5Lget_name_by_idx_n3() {
        String link_name = null;
        try {
            link_name = H5.H5Lget_name_by_idx(H5fid, "/", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 3, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lget_name_by_idx: " + err);
        }
        assertFalse("H5Lget_name_by_idx ",link_name==null);
        assertTrue("Link Name ",link_name.compareTo("L1")==0);
    }

    @Test
    public void testH5Lvisit() {
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
            H5.H5Lvisit(H5fid, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lvisit: " + err);
        }
        assertFalse("H5Lvisit ",((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Lvisit "+((H5L_iter_data)iter_data).iterdata.size(),((H5L_iter_data)iter_data).iterdata.size()==5);
        assertTrue("H5Lvisit "+(((H5L_iter_data)iter_data).iterdata.get(0)).link_name,(((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS1")==0);
        assertTrue("H5Lvisit "+(((H5L_iter_data)iter_data).iterdata.get(1)).link_name,(((H5L_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("DT1")==0);
        assertTrue("H5Lvisit "+(((H5L_iter_data)iter_data).iterdata.get(2)).link_name,(((H5L_iter_data)iter_data).iterdata.get(2)).link_name.compareToIgnoreCase("G1")==0);
        assertTrue("H5Lvisit "+(((H5L_iter_data)iter_data).iterdata.get(3)).link_name,(((H5L_iter_data)iter_data).iterdata.get(3)).link_name.compareToIgnoreCase("G1/DS2")==0);
        assertTrue("H5Lvisit "+(((H5L_iter_data)iter_data).iterdata.get(4)).link_name,(((H5L_iter_data)iter_data).iterdata.get(4)).link_name.compareToIgnoreCase("L1")==0);
    }

    @Test
    public void testH5Lvisit_by_name() {
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
            H5.H5Lvisit_by_name(H5fid, "G1", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, iter_cb, iter_data, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Lvisit_by_name: " + err);
        }
        assertFalse("H5Lvisit_by_name ",((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Lvisit_by_name "+((H5L_iter_data)iter_data).iterdata.size(),((H5L_iter_data)iter_data).iterdata.size()==1);
        assertTrue("H5Lvisit_by_name "+(((H5L_iter_data)iter_data).iterdata.get(0)).link_name,(((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS2")==0);
    }

    @Test
    public void testH5Literate() {
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
            H5.H5Literate(H5fid, HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0L, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Literate: " + err);
        }
        assertFalse("H5Literate ",((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Literate "+((H5L_iter_data)iter_data).iterdata.size(),((H5L_iter_data)iter_data).iterdata.size()==4);
        assertTrue("H5Literate "+(((H5L_iter_data)iter_data).iterdata.get(0)).link_name,(((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS1")==0);
        assertTrue("H5Literate "+(((H5L_iter_data)iter_data).iterdata.get(1)).link_name,(((H5L_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("DT1")==0);
        assertTrue("H5Literate "+((idata)((H5L_iter_data)iter_data).iterdata.get(2)).link_name,(((H5L_iter_data)iter_data).iterdata.get(2)).link_name.compareToIgnoreCase("G1")==0);
        assertTrue("H5Literate "+((idata)((H5L_iter_data)iter_data).iterdata.get(3)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(3)).link_name.compareToIgnoreCase("L1")==0);
    }

    @Test
    public void testH5Literate_by_name() {
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
            H5.H5Literate_by_name(H5fid, "G1", HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC, 0L, iter_cb, iter_data, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Literate_by_name: " + err);
        }
        assertFalse("H5Literate_by_name ",((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Literate_by_name "+((H5L_iter_data)iter_data).iterdata.size(),((H5L_iter_data)iter_data).iterdata.size()==1);
        assertTrue("H5Literate_by_name "+((idata)((H5L_iter_data)iter_data).iterdata.get(0)).link_name,((idata)((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS2")==0);
    }

}
