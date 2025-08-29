/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
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
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Edefault {
    @Rule
    public TestName testname = new TestName();

    public static final int ERRSTACK_CNT = 6;

    @Before
    public void H5Eset_default_stack()
    {
        System.out.print(testname.getMethodName());

        try {
            // Clear any active stack messages
            H5Eclear2(H5E_DEFAULT());
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5Eset_default_stack: " + err);
        }
    }
    @After
    public void nextTestName()
    {
        System.out.println();
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eprint2_invalid_classid() throws Throwable
    {
        H5Eprint2(-1, null);
    }

    @Ignore
    public void testH5Eprint()
    {
        /*
         * If HDF5_VOL_CONNECTOR is set, this might not be the
         * native connector and the error stack might be different.
         * Only check for the specific error stack if the native
         * connector is being used.
         */
        String connector = System.getenv("HDF5_VOL_CONNECTOR");
        if (connector == null) {
            try {
                H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
            }
            catch (Throwable err) {
            }
            try {
                H5Eprint2(H5E_DEFAULT(), null);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5Eprint: " + err);
            }
        }
    }

    @Test
    public void testH5Eget_current_stack()
    {
        long num_msg          = -1;
        long num_msg_default  = -1;
        long saved_num_msg    = -1;
        long stack_id         = -1;
        long stack_id_default = H5E_DEFAULT();
        try {
            H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Throwable err) {
            // default stack id will be different after exception
            stack_id_default = H5E_DEFAULT();
            // err.printStackTrace(); //This will clear the error stack
        }
        // Verify we have messages on the error stack
        try {
            num_msg_default = H5Eget_num(stack_id_default);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg_default, num_msg_default > 0);
        saved_num_msg = num_msg_default;

        // Save a copy of the current stack and clear the current stack
        try {
            stack_id = H5Eget_current_stack();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_current_stack: " + err);
        }
        assertFalse("H5Eget_current_stack: get_current_stack - " + stack_id, stack_id < 0);
        assertFalse("H5Eget_current_stack: get_current_stack - " + stack_id, stack_id == stack_id_default);

        // Verify the default stack is empty
        try {
            num_msg_default = H5Eget_num(stack_id_default);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg_default, num_msg_default == 0);

        // Verify the copy has the same number of messages as the original
        try {
            num_msg = H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg, num_msg == saved_num_msg);

        try {
            H5Eclose_stack(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eclose_stack: " + err);
        }
    }

    @Test
    public void testH5Eget_current_stack_pop()
    {
        long num_msg         = -1;
        long num_msg_default = -1;
        long saved_num_msg   = -1;
        long stack_id        = -1;
        try {
            H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Throwable err) {
            // err.printStackTrace(); //This will clear the error stack
        }

        // Verify there are error messages on the stack and save it
        try {
            num_msg_default = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg_default, num_msg_default > 0);
        saved_num_msg = num_msg_default;

        // Save a copy of the current stack and clear the current stack
        try {
            stack_id = H5Eget_current_stack();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_current_stack: " + err);
        }
        assertFalse("H5Eget_current_stack: get_current_stack - " + stack_id, stack_id < 0);
        assertFalse("H5Eget_current_stack: get_current_stack - " + stack_id, stack_id == H5E_DEFAULT());

        // Verify the stack is empty
        try {
            num_msg_default = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg_default, num_msg_default == 0);

        // Verify the copy has the correct number of messages
        try {
            num_msg = H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg, num_msg == saved_num_msg);

        // Generate errors on default stack
        try {
            H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Throwable err) {
            // err.printStackTrace(); //This will clear the error stack
        }

        // Verify we have a nonzero number of messages and save it
        try {
            num_msg_default = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg_default, num_msg_default > 0);
        saved_num_msg = num_msg;

        // Remove one message from the current stack
        try {
            H5Epop(H5E_DEFAULT(), 1);
            num_msg_default = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: pop #:" + num_msg_default, num_msg_default == saved_num_msg - 1);

        // Verify the copy still has the old number of messages
        try {
            num_msg = H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg, num_msg == saved_num_msg);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eclose_stack_invalid_stackid() throws Throwable
    {
        H5Eclose_stack(-1);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eget_class_name_invalid_classid() throws Throwable
    {
        H5Eget_class_name(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Eget_class_name_invalid_classname() throws Throwable
    {
        H5Eget_class_name(H5E_DEFAULT());
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eclose_msg_invalid_errid() throws Throwable
    {
        H5Eclose_msg(-1);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Ecreate_msg_invalid_errid() throws Throwable
    {
        H5Ecreate_msg(-1, H5E_MAJOR(), "null");
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eget_msg_invalid_msgid() throws Throwable
    {
        H5Eget_msg(-1, null);
    }

    @Test
    public void testH5Ecreate_stack()
    {
        try {
            long stack_id = H5Ecreate_stack();
            assertTrue("H5Ecreate_stack", stack_id > 0);
            H5Eclose_stack(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Ecreate_stack: " + err);
        }
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eset_current_stack_invalid_stkid() throws Throwable
    {
        H5Eset_current_stack(-1);
    }

    @Test
    public void testH5Eset_current_stack()
    {
        long num_msg       = -1;
        long stack_id      = -1;
        long saved_num_msg = -1;

        // Generate errors on the default stack
        try {
            H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Throwable err) {
            // err.printStackTrace(); //This will clear the error stack
        }

        // Verify we have a nonzero number of messages and save it
        try {
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg, num_msg > 0);
        saved_num_msg = num_msg;

        // Save a copy of the current stack
        try {
            stack_id = H5Eget_current_stack();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eset_current_stack: " + err);
        }
        assertFalse("H5Eset_current_stack: get_current_stack - " + stack_id, stack_id < 0);
        assertFalse("H5Eset_current_stack: get_current_stack - " + stack_id, stack_id == H5E_DEFAULT());

        // Verify the copy has the same number of messages as the original stack
        try {
            num_msg = H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg, num_msg == saved_num_msg);

        // Generate errors on default stack (again, in the same way)
        try {
            H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Throwable err) {
            // err.printStackTrace(); //This will clear the error stack
        }

        // Verify we have the same number of messages as before
        try {
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg, num_msg == saved_num_msg);

        // Remove one message from the current stack
        try {
            H5Epop(H5E_DEFAULT(), 1);
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: pop #:" + num_msg, num_msg == saved_num_msg - 1);

        // Verify the copy still has the correct number of messages
        try {
            num_msg = H5Eget_num(stack_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num: get_num #:" + num_msg, num_msg == saved_num_msg);

        // Set the current stack to be the default and try that again
        try {
            .H5Eset_current_stack(stack_id);
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eset_current_stack: " + err);
        }
        assertTrue("H5Eset_current_stack: get_num - " + num_msg, num_msg == saved_num_msg);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Epop_invalid_stkid() throws Throwable
    {
        H5Epop(-1, 0);
    }

    @Test
    public void testH5Epop() throws Throwable
    {
        long num_msg       = -1;
        long saved_num_msg = -1;
        try {
            H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Throwable err) {
        }
        try {
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num before #:" + num_msg, num_msg > 0);
        saved_num_msg = num_msg;
        try {
            H5Epop(H5E_DEFAULT(), 1);
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Epop: " + err);
        }
        assertTrue("H5Epop after #:" + num_msg, num_msg == saved_num_msg - 1);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Epush_invalid_stkid() throws Throwable
    {
        H5Epush(-1, "Invalid", "Invalid", 0, -1, -1, -1, "Invalid message");
    }

    @Test(expected = NullPointerException.class)
    public void testH5Epush_null_name() throws Throwable
    {
        H5Epush(H5E_DEFAULT(), null, "Invalid", 0, H5E_DEFAULT(), H5E_DEFAULT(), H5E_DEFAULT(),
                "Invalid message");
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5EprintInt_invalid_classid() throws Throwable
    {
        H5Eprint2(-1, null);
    }

    @Ignore
    public void testH5EprintInt()
    {
        /*
         * If HDF5_VOL_CONNECTOR is set, this might not be the
         * native connector and the error stack might be different.
         * Only check for the specific error stack if the native
         * connector is being used.
         */
        String connector = System.getenv("HDF5_VOL_CONNECTOR");
        if (connector == null) {
            try {
                H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
            }
            catch (Throwable err) {
            }
            try {
                H5Eprint2(H5E_DEFAULT(), null);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5EprintInt: " + err);
            }
        }
    }

    @Test
    public void testH5EclearInt()
    {
        try {
            H5Eclear(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5EclearInt: " + err);
        }
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eclear2_invalid_stkid() throws Throwable
    {
        H5Eclear2(-1);
    }

    @Test
    public void testH5Eclear()
    {
        try {
            H5Eclear2(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eclear2: " + err);
        }
    }

    @Test
    public void testH5Eclear2_with_msg()
    {
        long num_msg = -1;
        try {
            H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Throwable err) {
        }
        try {
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num before #:" + num_msg, num_msg > 0);
        try {
            H5Eclear2(H5E_DEFAULT());
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eclear2_with_msg: " + err);
        }
        assertTrue("H5Eclear2_with_msg after #:" + num_msg, num_msg == 0);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eauto_is_v2_invalid_stkid() throws Throwable
    {
        H5Eauto_is_v2(-1);
    }

    @Test
    public void testH5Eauto_is_v2()
    {
        boolean is_v2 = false;
        try {
            is_v2 = H5Eauto_is_v2(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eauto_is_v2: " + err);
        }
        assertTrue("H5Eauto_is_v2: ", is_v2);
    }

    @Test(expected = HDF5FunctionArgumentException.class)
    public void testH5Eget_num_invalid_stkid() throws Throwable
    {
        H5Eget_num(-1);
    }

    @Test
    public void testH5Eget_num()
    {
        long num_msg = -1;
        try {
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num: " + err);
        }
        assertTrue("H5Eget_num #:" + num_msg, num_msg == 0);
    }

    @Test
    public void testH5Eget_num_with_msg()
    {
        long num_msg = -1;
        try {
            H5Fopen("test", H5F_ACC_RDWR(), H5P_DEFAULT());
        }
        catch (Throwable err) {
        }
        try {
            num_msg = H5Eget_num(H5E_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Eget_num_with_msg: " + err);
        }
        assertTrue("H5Eget_num_with_msg #:" + num_msg, num_msg > 0);
    }
}
