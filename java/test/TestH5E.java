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
    long hdf_java_classid = -1;
    long current_stackid = -1;

    @Before
    public void H5Eget_stack_class() {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        hdf_java_classid = -1;
        try {
            hdf_java_classid = H5.H5Eregister_class("HDF-Java-Error",
                    "hdf-java", "2.5");
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
    public void testH5Eget_class_name() {
        try {
            String class_name = H5.H5Eget_class_name(hdf_java_classid);
            assertNotNull("H5.H5Eget_class_name: " + class_name, class_name);
            assertEquals("H5.H5Eget_class_name: ", "HDF-Java-Error", class_name);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_class_name: " + err);
        }
    }

    @Test
    public void testH5Eprint2() {
        try {
            assertFalse(current_stackid < 0);
            H5.H5Eprint2(current_stackid, null);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eprint2: " + err);
        }
    }

    @Ignore("Tested with create_msg_major[minor]")
    public void testH5Eclose_msg() {
        fail("Not yet implemented");
    }

    @Test(expected = NullPointerException.class)
    public void testH5Ecreate_msg_name_null() throws Throwable {
        H5.H5Ecreate_msg(hdf_java_classid, HDF5Constants.H5E_MAJOR, null);
    }

    @Test
    public void testH5Ecreate_msg_major() {
        try {
            long err_id = H5.H5Ecreate_msg(hdf_java_classid,
                    HDF5Constants.H5E_MAJOR, "Error in Test");
            assertFalse("H5.H5Ecreate_msg_major: " + err_id, err_id < 0);
            H5.H5Eclose_msg(err_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ecreate_msg_major: " + err);
        }
    }

    @Test
    public void testH5Ecreate_msg_minor() {
        try {
            long err_id = H5.H5Ecreate_msg(hdf_java_classid,
                    HDF5Constants.H5E_MINOR, "Error in Test Function");
            assertFalse("H5.H5Ecreate_msg_minor: " + err_id, err_id < 0);
            H5.H5Eclose_msg(err_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ecreate_msg_minor: " + err);
        }
    }

    @Test
    public void testH5Eget_msg() {
        int[] error_msg_type = { HDF5Constants.H5E_MINOR };
        long err_id = -1;
        String msg = null;
        try {
            err_id = H5.H5Ecreate_msg(hdf_java_classid,
                    HDF5Constants.H5E_MAJOR, "Error in Test");
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_msg: " + err);
        }
        assertFalse("H5.H5Eget_msg: H5Ecreate_msg - " + err_id, err_id < 0);
        try {
            msg = H5.H5Eget_msg(err_id, error_msg_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_msg: " + err);
        }
        assertNotNull("H5.H5Eget_msg: " + msg, msg);
        assertEquals("H5.H5Eget_msg: ", "Error in Test", msg);
        assertEquals("H5.H5Eget_msg: ", HDF5Constants.H5E_MAJOR,
                    error_msg_type[0]);
        try {
            H5.H5Eclose_msg(err_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_msg: " + err);
        }
    }

    @Test
    public void testH5Eget_msg_major() {

        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (HDF5LibraryException hdferr) {
            int[] error_msg_type = { HDF5Constants.H5E_MAJOR };
            String msg = null;
            try {
                msg = H5.H5Eget_msg(hdferr.getMajorErrorNumber(),
                        error_msg_type);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5.H5Eget_msg: " + err);
            }
            assertNotNull("H5.H5Eget_msg: " + msg, msg);
            assertEquals("H5.H5Eget_msg: ", "Invalid arguments to routine",
                        msg);
            assertEquals("H5.H5Eget_msg: ", HDF5Constants.H5E_MAJOR,
                        error_msg_type[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_msg: " + err);
        }
    }

    @Test
    public void testH5Eget_msg_minor() {
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (HDF5LibraryException hdferr) {
            int[] error_msg_type = { HDF5Constants.H5E_MINOR };
            String msg = null;
            try {
                msg = H5.H5Eget_msg(hdferr.getMinorErrorNumber(),
                        error_msg_type);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5.H5Eget_msg: " + err);
            }
            assertNotNull("H5.H5Eget_msg: " + msg, msg);
            assertEquals("H5.H5Eget_msg: ", "Inappropriate type", msg);
            assertEquals("H5.H5Eget_msg: ", HDF5Constants.H5E_MINOR,
                        error_msg_type[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_msg: " + err);
        }
    }

    @Test
    public void testH5Ecreate_stack() {
        long stk_id = -1;
        try {
            stk_id = H5.H5Ecreate_stack();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ecreate_stack: " + err);
        }
        assertFalse("H5.H5Ecreate_stack: " + stk_id, stk_id < 0);
        try {
            H5.H5Eclose_stack(stk_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ecreate_stack: " + err);
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

        assertTrue("H5.H5Epop #:" + num_msg, num_msg == 0);

        try {
            num_msg = H5.H5Eget_num(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }

        assertTrue("H5.H5Epop #:" + num_msg, num_msg == 3);

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

        assertTrue("H5.H5Epop", num_msg == 2);
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
                assertTrue("testH5Epush #:" + num_msg, num_msg == 0);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5Epush: H5.H5Eget_num: " + err);
            }

            H5.H5Epush(estack_id, "TestH5E.java", err_func, 354, hdf_java_classid, maj_err_id, min_err_id, err_msg);

            try {
                num_msg = H5.H5Eget_num(estack_id);
                assertTrue("testH5Epush #:" + num_msg, num_msg == 1);
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
    public void testH5EprintInt() {
        assertFalse(current_stackid < 0);
        try {
            H5.H5Eprint2(current_stackid, null);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5EprintInt: " + err);
        }
    }

    @Test
    public void testH5EclearInt() {
        try {
            H5.H5Eclear(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5EclearInt: " + err);
        }
    }

    @Test
    public void testH5Eclear2() {
        try {
            H5.H5Eclear2(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eclear2: " + err);
        }
    }

    @Test
    public void testH5Eauto_is_v2() {
        boolean is_v2 = false;
        try {
            is_v2 = H5.H5Eauto_is_v2(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eauto_is_v2: " + err);
        }
        assertTrue("H5.H5Eauto_is_v2: ", is_v2);
    }

    @Test
    public void testH5Eget_num() {
        long num_msg = -1;
        try {
            num_msg = H5.H5Eget_num(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_num: " + err);
        }
        assertTrue("H5.H5Eget_num", num_msg == 0);
    }

    @Test
    public void testH5Eget_num_with_msg() {
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
            num_msg = H5.H5Eget_num(current_stackid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }
        assertTrue("H5.H5Eget_num_with_msg #:" + num_msg, num_msg > 0);
    }

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
        assertTrue("testH5Ewalk #:" + num_msg, num_msg == 3);

        try {
            H5.H5Ewalk2(current_stackid, HDF5Constants.H5E_WALK_UPWARD, walk_cb, walk_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Ewalk:H5Ewalk2 " + err);
        }
        assertFalse("testH5Ewalk:H5Ewalk2 ",((H5E_walk_data)walk_data).walkdata.isEmpty());
        assertTrue("testH5Ewalk:H5Ewalk2 "+((H5E_walk_data)walk_data).walkdata.size(),((H5E_walk_data)walk_data).walkdata.size()==3);
        assertTrue("testH5Ewalk:H5Ewalk2 "+((wdata)((H5E_walk_data)walk_data).walkdata.get(0)).line,((wdata)((H5E_walk_data)walk_data).walkdata.get(0)).line==3767);
        assertTrue("testH5Ewalk:H5Ewalk2 "+((wdata)((H5E_walk_data)walk_data).walkdata.get(1)).line,((wdata)((H5E_walk_data)walk_data).walkdata.get(1)).line==5506);
        assertTrue("testH5Ewalk:H5Ewalk2 "+((wdata)((H5E_walk_data)walk_data).walkdata.get(1)).func_name,((wdata)((H5E_walk_data)walk_data).walkdata.get(1)).func_name.compareToIgnoreCase("H5P_verify_apl_and_dxpl")==0);
        assertTrue("testH5Ewalk:H5Ewalk2 "+((wdata)((H5E_walk_data)walk_data).walkdata.get(0)).err_desc,((wdata)((H5E_walk_data)walk_data).walkdata.get(0)).err_desc.compareToIgnoreCase("not a property list")==0);
        assertTrue("testH5Ewalk:H5Ewalk2 "+((wdata)((H5E_walk_data)walk_data).walkdata.get(1)).err_desc,((wdata)((H5E_walk_data)walk_data).walkdata.get(1)).err_desc.compareToIgnoreCase("not the required access property list")==0);
    }

}
