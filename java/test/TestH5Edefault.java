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
import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Edefault {
    @Rule public TestName testname = new TestName();

    @Before
    public void H5Eset_default_stack() {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        try {
            // Clear any active stack messages
            H5.H5Eclear2(HDF5Constants.H5E_DEFAULT);
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5Eset_default_stack: " + err);
        }
    }
    @After
    public void nextTestName() {
        System.out.println();
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eprint2_invalid_classid() throws Throwable {
        H5.H5Eprint2(-1, null);
    }

    @Test
    public void testH5Eprint() {
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
        }
        try {
            H5.H5Eprint2(HDF5Constants.H5E_DEFAULT, null);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eprint: " + err);
        }
    }

    @Test
    public void testH5Eget_current_stack() {
        long num_msg = -1;
        long num_msg_default = -1;
        long stack_id = -1;
        long stack_id_default = HDF5Constants.H5E_DEFAULT;
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
            //default stack id will be different after exception
            stack_id_default = HDF5Constants.H5E_DEFAULT;
            //err.printStackTrace(); //This will clear the error stack
        }
        // Verify we have the correct number of messages
        try {
            num_msg_default = H5.H5Eget_num(stack_id_default);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: get_num #:" + num_msg_default,
                num_msg_default == 3);

        //Save a copy of the current stack and clears the current stack
        try {
            stack_id = H5.H5Eget_current_stack();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertFalse("H5.H5Eget_current_stack: get_current_stack - "
                + stack_id, stack_id < 0);
        assertFalse("H5.H5Eget_current_stack: get_current_stack - "
                + stack_id, stack_id == stack_id_default);

        // Verify we have the correct number of messages
        try {
            num_msg_default = H5.H5Eget_num(stack_id_default);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: get_num #:" + num_msg_default,
                num_msg_default == 0);

        //Verify the copy has the correct number of messages
        try {
            num_msg = H5.H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: get_num #:" + num_msg,
                num_msg == 3);

        try {
            H5.H5Eclose_stack(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
    }

    @Test
    public void testH5Eget_current_stack_pop() {
        long num_msg = -1;
        long num_msg_default = -1;
        long stack_id = -1;
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
            //err.printStackTrace(); //This will clear the error stack
        }

        // Verify we have the correct number of messages
        try {
            num_msg_default = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: get_num #:" + num_msg_default,
                num_msg_default == 3);

        //Save a copy of the current stack and clears the current stack
        try {
            stack_id = H5.H5Eget_current_stack();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertFalse("H5.H5Eget_current_stack: get_current_stack - "
                + stack_id, stack_id < 0);
        assertFalse("H5.H5Eget_current_stack: get_current_stack - "
                + stack_id, stack_id == HDF5Constants.H5E_DEFAULT);

        // Verify we have the correct number of messages
        try {
            num_msg_default = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: get_num #:" + num_msg_default,
                num_msg_default == 0);

        //Verify the copy has the correct number of messages
        try {
            num_msg = H5.H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: get_num #:" + num_msg,
                num_msg == 3);

        //Generate errors on default stack
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
            //err.printStackTrace(); //This will clear the error stack
        }

        // Verify we have the correct number of messages
        try {
            num_msg_default = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: get_num #:" + num_msg_default,
                num_msg_default == 3);

        //Remove one message from the current stack
        try {
            H5.H5Epop(HDF5Constants.H5E_DEFAULT, 1);
            num_msg_default = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: pop #:" + num_msg_default,
                num_msg_default == 2);

        //Verify the copy still has the correct number of messages
        try {
            num_msg = H5.H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eget_current_stack: get_num #:" + num_msg,
                num_msg == 3);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eclose_stack_invalid_stackid() throws Throwable {
        H5.H5Eclose_stack(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eget_class_name_invalid_classid() throws Throwable {
        H5.H5Eget_class_name(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eget_class_name_invalid_classname() throws Throwable {
        H5.H5Eget_class_name(HDF5Constants.H5E_DEFAULT);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eclose_msg_invalid_errid() throws Throwable {
        H5.H5Eclose_msg(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Ecreate_msg_invalid_errid() throws Throwable {
        H5.H5Ecreate_msg(-1, HDF5Constants.H5E_MAJOR, "null");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eget_msg_invalid_msgid() throws Throwable {
        H5.H5Eget_msg(-1, null);
    }

    @Test
    public void testH5Ecreate_stack() {
        try {
            long stack_id = H5.H5Ecreate_stack();
            assertTrue("H5.H5Ecreate_stack", stack_id > 0);
            H5.H5Eclose_stack(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Ecreate_stack: " + err);
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eset_current_stack_invalid_stkid() throws Throwable {
        H5.H5Eset_current_stack(-1);
    }

    @Test
    public void testH5Eset_current_stack() {
        long num_msg = -1;
        long stack_id = -1;
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
            //err.printStackTrace(); //This will clear the error stack
        }

        // Verify we have the correct number of messages
        try {
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eset_current_stack: " + err);
        }
        assertTrue("H5.H5Eset_current_stack: get_num #:" + num_msg,
                    num_msg == 3);

        //Save a copy of the current stack
        try {
            stack_id = H5.H5Eget_current_stack();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eset_current_stack: " + err);
        }
        assertFalse("H5.H5Eset_current_stack: get_current_stack - "
                    + stack_id, stack_id < 0);
        assertFalse("H5.H5Eset_current_stack: get_current_stack - "
                + stack_id, stack_id == HDF5Constants.H5E_DEFAULT);

        //Verify the copy has the correct number of messages
        try {
            num_msg = H5.H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eset_current_stack: " + err);
        }
        assertTrue("H5.H5Eset_current_stack: get_num #:" + num_msg,
                    num_msg == 3);

        //Generate errors on default stack
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
            //err.printStackTrace(); //This will clear the error stack
        }

        // Verify we have the correct number of messages
        try {
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_current_stack: " + err);
        }
        assertTrue("H5.H5Eset_current_stack: get_num #:" + num_msg,
                num_msg == 3);

        //Remove one message from the current stack
        try {
            H5.H5Epop(HDF5Constants.H5E_DEFAULT, 1);
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eset_current_stack: " + err);
        }
        assertTrue("H5.H5Eset_current_stack: pop #:" + num_msg,
                    num_msg == 2);

        //Verify the copy still has the correct number of messages
        try {
            num_msg = H5.H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eset_current_stack: " + err);
        }
        assertTrue("H5.H5Eset_current_stack: get_num #:" + num_msg,
                    num_msg == 3);

        try {
            H5.H5Eset_current_stack(stack_id);
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eset_current_stack: " + err);
        }
        assertTrue("H5.H5Eset_current_stack: get_num - " + num_msg,
                    num_msg == 3);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Epop_invalid_stkid() throws Throwable {
        H5.H5Epop(-1, 0);
    }

    @Test
    public void testH5Epop() throws Throwable {
        long num_msg = -1;
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
        }
        try {
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }
        assertTrue("H5.H5Epop before #:" + num_msg, num_msg == 3);
        try {
            H5.H5Epop(HDF5Constants.H5E_DEFAULT, 1);
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Epop: " + err);
        }
        assertTrue("H5.H5Epop after #:" + num_msg, num_msg == 2);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Epush_invalid_stkid() throws Throwable {
        H5.H5Epush(-1, "Invalid", "Invalid", 0, -1, -1, -1, "Invalid message");
    }

    @Test(expected = NullPointerException.class)
    public void testH5Epush_null_name() throws Throwable {
        H5.H5Epush(HDF5Constants.H5E_DEFAULT, null, "Invalid", 0, HDF5Constants.H5E_DEFAULT, HDF5Constants.H5E_DEFAULT, HDF5Constants.H5E_DEFAULT, "Invalid message");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5EprintInt_invalid_classid() throws Throwable {
        H5.H5Eprint2(-1, null);
    }

    @Test
    public void testH5EprintInt() {
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
        }
        try {
            H5.H5Eprint2(HDF5Constants.H5E_DEFAULT, null);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5EprintInt: " + err);
        }
    }

    @Test
    public void testH5EclearInt() {
        try {
            H5.H5Eclear(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5EclearInt: " + err);
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eclear2_invalid_stkid() throws Throwable {
        H5.H5Eclear2(-1);
    }

    @Test
    public void testH5Eclear() {
        try {
            H5.H5Eclear2(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eclear2: " + err);
        }
    }

    @Test
    public void testH5Eclear2_with_msg() {
        long num_msg = -1;
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
        }
        try {
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eclear2_with_msg: " + err);
        }
        assertTrue("H5.H5Eclear2_with_msg before #:" + num_msg,
                    num_msg == 3);
        try {
            H5.H5Eclear2(HDF5Constants.H5E_DEFAULT);
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eclear2_with_msg: " + err);
        }
            assertTrue("H5.H5Eclear2_with_msg after #:" + num_msg, num_msg == 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eauto_is_v2_invalid_stkid() throws Throwable {
        H5.H5Eauto_is_v2(-1);
    }

    @Test
    public void testH5Eauto_is_v2() {
        boolean is_v2 = false;
        try {
            is_v2 = H5.H5Eauto_is_v2(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eauto_is_v2: " + err);
        }
        assertTrue("H5.H5Eauto_is_v2: ", is_v2);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eget_num_invalid_stkid() throws Throwable {
        H5.H5Eget_num(-1);
    }

    @Test
    public void testH5Eget_num() {
        long num_msg = -1;
        try {
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_num: " + err);
        }
        assertTrue("H5.H5Eget_num #:" + num_msg, num_msg == 0);
    }

    @Test
    public void testH5Eget_num_with_msg() {
        long num_msg = -1;
        try {
            H5.H5Fopen("test", 0, 1);
        }
        catch (Throwable err) {
        }
        try {
            num_msg = H5.H5Eget_num(HDF5Constants.H5E_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Eget_num_with_msg: " + err);
        }
        assertTrue("H5.H5Eget_num_with_msg #:" + num_msg, num_msg > 0);
    }

}
