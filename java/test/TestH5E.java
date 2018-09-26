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

import java.util.ArrayList;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.callbacks.H5E_walk_cb;
import hdf.hdf5lib.callbacks.H5E_walk_t;
import hdf.hdf5lib.structs.H5E_error2_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5E {
    @Rule public TestName testname = new TestName();

    public static final int ERRSTACK_CNT = 3;
    public static final int ERRSTACK_MTY_CNT = 0;

    long hdf_java_classid = -1;
    long current_stackid = -1;

    @Before
    public void H5Eget_stack_class() {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        hdf_java_classid = -1;
        try {
            hdf_java_classid = H5.H5Eregister_class("HDF-Java-Error", "hdf-java", "2.5");
            current_stackid = H5.H5Eget_current_stack();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_stack_class: " + err);
        }
    }

    @After
    public void H5Erestore_stack_class() {
        try {
            H5.H5Eunregister_class(hdf_java_classid);
            hdf_java_classid = -1;
            H5.H5Eclose_stack(current_stackid);
            current_stackid = -1;
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Erestore_stack_class: " + err);
        }
        System.out.println();
    }

    @Test
    public void testH5Eget_msg_major() {

        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (HDF5LibraryException hdferr) {
            long errnum = hdferr.getMajorErrorNumber();
            int[] error_msg_type = { HDF5Constants.H5E_MAJOR };
            String msg = null;
            try {
                msg = H5.H5Eget_msg(errnum, error_msg_type);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5.H5Eget_msg(Throwable): " + err);
            }
            assertNotNull("H5.H5Eget_msg: " + msg, msg);
            assertEquals("H5.H5Eget_msg: ", "Object atom", msg);
            assertEquals("H5.H5Eget_msg: ", HDF5Constants.H5E_MAJOR, error_msg_type[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_msg(Other): " + err);
        }
    }

    @Test
    public void testH5Eget_msg_minor() {
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (HDF5LibraryException hdferr) {
            long errnum = hdferr.getMinorErrorNumber();
            int[] error_msg_type = { HDF5Constants.H5E_MINOR };
            String msg = null;
            try {
                msg = H5.H5Eget_msg(errnum, error_msg_type);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5.H5Eget_msg: " + err);
            }
            assertNotNull("H5.H5Eget_msg: " + msg, msg);
            assertEquals("H5.H5Eget_msg: ", "Unable to find atom information (already closed?)", msg);
            assertEquals("H5.H5Eget_msg: ", HDF5Constants.H5E_MINOR, error_msg_type[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_msg: " + err);
        }
    }

    @Test
    public void testH5Epop() {
        try {
            H5.H5Eset_current_stack(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }

        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
        }

        // save current stack contents
        try {
            current_stackid = H5.H5Eget_current_stack();
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }

        long num_msg = -1;
        try {
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }

        assertTrue("H5.H5Epop #:" + num_msg, num_msg == TestH5E.ERRSTACK_MTY_CNT);

        try {
            num_msg = H5.H5Eget_num(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }

        assertTrue("H5.H5Epop #:" + num_msg, num_msg == TestH5E.ERRSTACK_CNT);

        try {
            H5.H5Epop(current_stackid, 1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }

        try {
            num_msg = H5.H5Eget_num(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }

        assertTrue("H5.H5Epop", num_msg == TestH5E.ERRSTACK_CNT - 1);
    }

    @Test
    public void testH5Epush() {
        String      err_func = "testH5Epush";
        String      err_msg = "Error message";
        long        estack_id = -1;
        long        maj_err_id = -1;
        long        min_err_id = -1;
        long        num_msg = -1;

        try {
            try {
                maj_err_id = H5.H5Ecreate_msg(hdf_java_classid, HDF5Constants.H5E_MAJOR, "Error in Test");
                assertFalse("testH5Epush: H5.H5Ecreate_msg_major: " + maj_err_id, maj_err_id < 0);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Epush: H5.H5Ecreate_msg_major: " + err);
            }
            try {
                min_err_id = H5.H5Ecreate_msg(hdf_java_classid, HDF5Constants.H5E_MINOR, "Error in Test Function");
                assertFalse("H5.H5Ecreate_msg_minor: " + min_err_id, min_err_id < 0);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Epush: H5.H5Ecreate_msg_minor: " + err);
            }

            try {
                estack_id = H5.H5Ecreate_stack();
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Epush: H5.H5Ecreate_stack: " + err);
            }
            assertFalse("testH5Epush: H5.H5Ecreate_stack: " + estack_id, estack_id < 0);

            try {
                num_msg = H5.H5Eget_num(estack_id);
                assertTrue("testH5Epush #:" + num_msg, num_msg == TestH5E.ERRSTACK_MTY_CNT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Epush: H5.H5Eget_num: " + err);
            }

            H5.H5Epush(estack_id, "TestH5E.java", err_func, 354, hdf_java_classid, maj_err_id, min_err_id, err_msg);

            try {
                num_msg = H5.H5Eget_num(estack_id);
                assertTrue("testH5Epush #:" + num_msg, num_msg == TestH5E.ERRSTACK_MTY_CNT + 1);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Epush: H5.H5Eget_num: " + err);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Epush: " + err);
        }
        finally {
            if (estack_id >= 0)
                try {H5.H5Eclose_stack(estack_id);} catch (Exception ex) {}
            if (maj_err_id >= 0)
                try {H5.H5Eclose_msg(maj_err_id);} catch (Exception ex) {}
            if (min_err_id >= 0)
                try {H5.H5Eclose_msg(min_err_id);} catch (Exception ex) {}
        }
    } /* end test_create() */

    @Test
    public void testH5Ewalk() {
        class wdata {
            public String err_desc = null;
            public String func_name = null;
            public int line = -1;
            wdata(String desc, String func, int lineno) {
                this.err_desc = new String(desc);
                this.func_name = new String(func);
                this.line = lineno;
            }
        }
        class H5E_walk_data implements H5E_walk_t {
            public ArrayList<wdata> walkdata = new ArrayList<wdata>();
        }
        H5E_walk_t walk_data = new H5E_walk_data();
        class H5E_walk_callback implements H5E_walk_cb {
            public int callback(int nidx, H5E_error2_t info, H5E_walk_t op_data) {
                wdata wd = new wdata(info.desc, info.func_name, info.line);
                ((H5E_walk_data)op_data).walkdata.add(wd);
                return 0;
            }
        }
        H5E_walk_cb walk_cb = new H5E_walk_callback();
        long num_msg = -1;

        try {
            H5.H5Eset_current_stack(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Ewalk:H5Eset_current_stack " + err);
        }
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
        }

        // save current stack contents
        try {
            current_stackid = H5.H5Eget_current_stack();
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }

        try {
            num_msg = H5.H5Eget_num(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Ewalk:H5Eget_num " + err);
        }
        assertTrue("testH5Ewalk #:" + num_msg, num_msg == TestH5E.ERRSTACK_CNT);

        try {
            H5.H5Ewalk2(current_stackid, HDF5Constants.H5E_WALK_UPWARD, walk_cb, walk_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Ewalk:H5Ewalk2 " + err);
        }
        assertFalse("testH5Ewalk:H5Ewalk2 ",((H5E_walk_data)walk_data).walkdata.isEmpty());
        assertTrue("testH5Ewalk:H5Ewalk2 "+((H5E_walk_data)walk_data).walkdata.size(),((H5E_walk_data)walk_data).walkdata.size()==TestH5E.ERRSTACK_CNT);
    }

}
