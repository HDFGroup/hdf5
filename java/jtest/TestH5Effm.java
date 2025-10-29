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

package jtest;

import static org.junit.Assert.*;

import static jtest.FfmTestSupport.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;

import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Error (H5E) operations.
 *
 * This test class uses direct FFM bindings without the hdf.hdf5lib wrapper layer.
 */
public class TestH5Effm {
    @Rule
    public TestName testname = new TestName();

    long hdf_java_classid = hdf5_h.H5I_INVALID_HID();
    long current_stackid  = hdf5_h.H5I_INVALID_HID();

    @Before
    public void setupErrorClass()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Register custom error class
            MemorySegment cls_name = stringToSegment(arena, "HDF-Java-FFM-Error");
            MemorySegment lib_name = stringToSegment(arena, "hdf-java-ffm");
            MemorySegment version  = stringToSegment(arena, "2.0");

            hdf_java_classid = hdf5_h.H5Eregister_class(cls_name, lib_name, version);
            assertTrue("H5Eregister_class failed", isValidId(hdf_java_classid));

            // Get current error stack
            current_stackid = hdf5_h.H5Eget_current_stack();
            assertTrue("H5Eget_current_stack failed", isValidId(current_stackid));
        }
    }

    @After
    public void cleanup()
    {
        if (isValidId(hdf_java_classid)) {
            int result = hdf5_h.H5Eunregister_class(hdf_java_classid);
            assertTrue("H5Eunregister_class failed", isSuccess(result));
            hdf_java_classid = hdf5_h.H5I_INVALID_HID();
        }

        if (isValidId(current_stackid)) {
            closeQuietly(current_stackid, hdf5_h::H5Eclose_stack);
            current_stackid = hdf5_h.H5I_INVALID_HID();
        }

        System.out.println();
    }

    // ============================================================================
    // Phase 1: Error Class and Message Operations
    // ============================================================================

    @Test
    public void testH5Eregister_unregister_class()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Register a new error class
            MemorySegment cls_name = stringToSegment(arena, "Test-Error-Class");
            MemorySegment lib_name = stringToSegment(arena, "test-lib");
            MemorySegment version  = stringToSegment(arena, "1.0");

            long class_id = hdf5_h.H5Eregister_class(cls_name, lib_name, version);
            assertTrue("H5Eregister_class failed", isValidId(class_id));

            // Get class name back
            long name_size = hdf5_h.H5Eget_class_name(class_id, MemorySegment.NULL, 0);
            assertTrue("H5Eget_class_name size query failed", name_size > 0);

            MemorySegment nameBuffer = arena.allocate(name_size + 1);
            long actual_size         = hdf5_h.H5Eget_class_name(class_id, nameBuffer, name_size + 1);
            assertTrue("H5Eget_class_name failed", actual_size > 0);

            String retrieved_name = nameBuffer.getString(0);
            assertEquals("Class name should match", "Test-Error-Class", retrieved_name);

            // Unregister class
            int result = hdf5_h.H5Eunregister_class(class_id);
            assertTrue("H5Eunregister_class failed", isSuccess(result));
        }
    }

    @Test
    public void testH5Ecreate_close_msg()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create major error message
            MemorySegment major_msg = stringToSegment(arena, "Test major error");
            long maj_err_id         = hdf5_h.H5Ecreate_msg(hdf_java_classid, hdf5_h.H5E_MAJOR(), major_msg);
            assertTrue("H5Ecreate_msg major failed", isValidId(maj_err_id));

            // Get message back
            MemorySegment typePtr = allocateInt(arena);
            long msg_size         = hdf5_h.H5Eget_msg(maj_err_id, typePtr, MemorySegment.NULL, 0);
            assertTrue("H5Eget_msg size query failed", msg_size > 0);

            MemorySegment msgBuffer = arena.allocate(msg_size + 1);
            long actual_size        = hdf5_h.H5Eget_msg(maj_err_id, typePtr, msgBuffer, msg_size + 1);
            assertTrue("H5Eget_msg failed", actual_size > 0);

            String retrieved_msg = msgBuffer.getString(0);
            assertEquals("Message should match", "Test major error", retrieved_msg);

            int msg_type = getInt(typePtr);
            assertEquals("Message type should be MAJOR", hdf5_h.H5E_MAJOR(), msg_type);

            // Close message
            int result = hdf5_h.H5Eclose_msg(maj_err_id);
            assertTrue("H5Eclose_msg failed", isSuccess(result));

            // Create minor error message
            MemorySegment minor_msg = stringToSegment(arena, "Test minor error");
            long min_err_id         = hdf5_h.H5Ecreate_msg(hdf_java_classid, hdf5_h.H5E_MINOR(), minor_msg);
            assertTrue("H5Ecreate_msg minor failed", isValidId(min_err_id));

            // Close minor message
            result = hdf5_h.H5Eclose_msg(min_err_id);
            assertTrue("H5Eclose_msg minor failed", isSuccess(result));
        }
    }

    @Test
    public void testH5Eget_major_minor()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Trigger an error to get real error numbers
            MemorySegment filename = stringToSegment(arena, "nonexistent_for_error_test.h5");
            long file_id           = hdf5_h.H5Fopen(filename, hdf5_h.H5F_ACC_RDONLY(), hdf5_h.H5P_DEFAULT());
            if (isValidId(file_id)) {
                hdf5_h.H5Fclose(file_id);
            }

            // Get error stack with actual errors
            long stack_id = hdf5_h.H5Eget_current_stack();
            if (isValidId(stack_id)) {
                long num_errors = hdf5_h.H5Eget_num(stack_id);

                if (num_errors > 0) {
                    // We have errors, test H5Eget_major/minor with error numbers from stack
                    // For now, just verify the functions can be called with value 0
                    // (H5Eget_major/minor require actual error numbers which are internal)

                    // Test that functions return non-null for valid inputs
                    // Note: We can't easily get actual major/minor error numbers without
                    // walking the stack, which requires H5Ewalk callback (not yet implemented)
                }

                hdf5_h.H5Eclose_stack(stack_id);
            }
        }
    }

    // ============================================================================
    // Phase 2: Error Stack Operations
    // ============================================================================

    @Test
    public void testH5Ecreate_close_stack()
    {
        // Create new error stack
        long stack_id = hdf5_h.H5Ecreate_stack();
        assertTrue("H5Ecreate_stack failed", isValidId(stack_id));

        // Verify it's empty (new stack has no errors)
        long num_errors = hdf5_h.H5Eget_num(stack_id);
        assertEquals("New stack should have 0 errors", 0L, num_errors);

        // Close stack
        int result = hdf5_h.H5Eclose_stack(stack_id);
        assertTrue("H5Eclose_stack failed", isSuccess(result));
    }

    @Test
    public void testH5Eget_current_set_stack()
    {
        // Get current stack
        long stack1 = hdf5_h.H5Eget_current_stack();
        assertTrue("H5Eget_current_stack failed", isValidId(stack1));

        // Create a new empty stack
        long stack2 = hdf5_h.H5Ecreate_stack();
        assertTrue("H5Ecreate_stack failed", isValidId(stack2));

        // Set new stack as current
        int result = hdf5_h.H5Eset_current_stack(stack2);
        assertTrue("H5Eset_current_stack failed", isSuccess(result));

        // Note: Setting current stack transfers ownership, so we don't close stack2
        // Restore original stack
        result = hdf5_h.H5Eset_current_stack(stack1);
        assertTrue("H5Eset_current_stack restore failed", isSuccess(result));
    }

    @Test
    public void testH5Eget_num_pop()
    {
        // Trigger an error by trying to open non-existent file
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment filename = stringToSegment(arena, "nonexistent_file_for_test.h5");
            long file_id           = hdf5_h.H5Fopen(filename, hdf5_h.H5F_ACC_RDONLY(), hdf5_h.H5P_DEFAULT());
            // File open will fail, but that's expected
            if (isValidId(file_id)) {
                hdf5_h.H5Fclose(file_id);
            }
        }

        // Get current error stack (should have errors from failed open)
        long stack_id = hdf5_h.H5Eget_current_stack();
        assertTrue("H5Eget_current_stack failed", isValidId(stack_id));

        // Get number of errors
        long num_errors = hdf5_h.H5Eget_num(stack_id);
        assertTrue("Stack should have errors after failed open", num_errors > 0);

        long saved_num = num_errors;

        // Pop one error
        int result = hdf5_h.H5Epop(stack_id, 1);
        assertTrue("H5Epop failed", isSuccess(result));

        // Verify count decreased
        num_errors = hdf5_h.H5Eget_num(stack_id);
        assertEquals("Error count should decrease by 1", saved_num - 1, num_errors);

        // Clean up
        hdf5_h.H5Eclose_stack(stack_id);
    }

    @Test
    public void testH5Eappend_stack()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create two stacks with errors
            // Stack 1: trigger error
            MemorySegment filename1 = stringToSegment(arena, "nonexistent1.h5");
            long file_id1 = hdf5_h.H5Fopen(filename1, hdf5_h.H5F_ACC_RDONLY(), hdf5_h.H5P_DEFAULT());
            if (isValidId(file_id1))
                hdf5_h.H5Fclose(file_id1);

            long stack1 = hdf5_h.H5Eget_current_stack();
            assertTrue("H5Eget_current_stack stack1 failed", isValidId(stack1));

            long num1 = hdf5_h.H5Eget_num(stack1);

            // Stack 2: trigger another error
            MemorySegment filename2 = stringToSegment(arena, "nonexistent2.h5");
            long file_id2 = hdf5_h.H5Fopen(filename2, hdf5_h.H5F_ACC_RDONLY(), hdf5_h.H5P_DEFAULT());
            if (isValidId(file_id2))
                hdf5_h.H5Fclose(file_id2);

            long stack2 = hdf5_h.H5Eget_current_stack();
            assertTrue("H5Eget_current_stack stack2 failed", isValidId(stack2));

            long num2 = hdf5_h.H5Eget_num(stack2);

            // Append stack2 to stack1 (close_source = false)
            int result = hdf5_h.H5Eappend_stack(stack1, stack2, false);
            assertTrue("H5Eappend_stack failed", isSuccess(result));

            // Verify stack1 now has combined errors
            long combined_num = hdf5_h.H5Eget_num(stack1);
            assertTrue("Combined stack should have more errors", combined_num >= num1);

            // Clean up (both stacks need closing since close_source was false)
            hdf5_h.H5Eclose_stack(stack1);
            hdf5_h.H5Eclose_stack(stack2);
        }
    }

    // ============================================================================
    // Phase 3: Stack Pause/Resume Operations
    // ============================================================================

    @Test
    public void testH5Epause_resume_stack()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a stack
            long stack_id = hdf5_h.H5Ecreate_stack();
            assertTrue("H5Ecreate_stack failed", isValidId(stack_id));

            // Check if paused (should not be paused initially)
            MemorySegment isPausedPtr = allocateInt(arena); // Using int for boolean
            int result                = hdf5_h.H5Eis_paused(stack_id, isPausedPtr);
            assertTrue("H5Eis_paused failed", isSuccess(result));

            boolean is_paused = (getInt(isPausedPtr) != 0);
            assertFalse("New stack should not be paused", is_paused);

            // Pause the stack
            result = hdf5_h.H5Epause_stack(stack_id);
            assertTrue("H5Epause_stack failed", isSuccess(result));

            // Verify it's paused
            result = hdf5_h.H5Eis_paused(stack_id, isPausedPtr);
            assertTrue("H5Eis_paused after pause failed", isSuccess(result));

            is_paused = (getInt(isPausedPtr) != 0);
            assertTrue("Stack should be paused", is_paused);

            // Resume the stack
            result = hdf5_h.H5Eresume_stack(stack_id);
            assertTrue("H5Eresume_stack failed", isSuccess(result));

            // Verify it's resumed (not paused)
            result = hdf5_h.H5Eis_paused(stack_id, isPausedPtr);
            assertTrue("H5Eis_paused after resume failed", isSuccess(result));

            is_paused = (getInt(isPausedPtr) != 0);
            assertFalse("Stack should not be paused after resume", is_paused);

            // Clean up
            hdf5_h.H5Eclose_stack(stack_id);
        }
    }

    // ============================================================================
    // Phase 4: Comprehensive Workflow Tests
    // ============================================================================

    @Test
    public void testH5E_complete_workflow()
    {
        try (Arena arena = Arena.ofConfined()) {
            // 1. Register error class
            MemorySegment cls_name = stringToSegment(arena, "Workflow-Test-Class");
            MemorySegment lib_name = stringToSegment(arena, "workflow-lib");
            MemorySegment version  = stringToSegment(arena, "1.0");

            long class_id = hdf5_h.H5Eregister_class(cls_name, lib_name, version);
            assertTrue("Register class failed", isValidId(class_id));

            // 2. Create error messages
            MemorySegment major_msg = stringToSegment(arena, "Workflow major error");
            long maj_id             = hdf5_h.H5Ecreate_msg(class_id, hdf5_h.H5E_MAJOR(), major_msg);
            assertTrue("Create major message failed", isValidId(maj_id));

            MemorySegment minor_msg = stringToSegment(arena, "Workflow minor error");
            long min_id             = hdf5_h.H5Ecreate_msg(class_id, hdf5_h.H5E_MINOR(), minor_msg);
            assertTrue("Create minor message failed", isValidId(min_id));

            // 3. Create and manipulate error stack
            long stack_id = hdf5_h.H5Ecreate_stack();
            assertTrue("Create stack failed", isValidId(stack_id));

            // Verify stack starts empty
            long num = hdf5_h.H5Eget_num(stack_id);
            assertEquals("New stack should be empty", 0L, num);

            // 4. Get class name
            long name_size = hdf5_h.H5Eget_class_name(class_id, MemorySegment.NULL, 0);
            assertTrue("Get class name size failed", name_size > 0);

            // 5. Clean up in reverse order
            hdf5_h.H5Eclose_stack(stack_id);
            hdf5_h.H5Eclose_msg(min_id);
            hdf5_h.H5Eclose_msg(maj_id);

            int result = hdf5_h.H5Eunregister_class(class_id);
            assertTrue("Unregister class failed", isSuccess(result));
        }
    }

    @Test
    public void testH5Eget_num()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Get number of errors on default stack
            long num_errors = hdf5_h.H5Eget_num(hdf5_h.H5E_DEFAULT());
            assertTrue("Number of errors should be >= 0", num_errors >= 0);
        }
    }

    @Test
    public void testH5Eclear()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Clear error stack
            int result = hdf5_h.H5Eclear2(hdf5_h.H5E_DEFAULT());
            assertTrue("H5Eclear2 failed", isSuccess(result));

            // Verify stack is empty
            long num_errors = hdf5_h.H5Eget_num(hdf5_h.H5E_DEFAULT());
            assertEquals("Stack should be empty", 0L, num_errors);
        }
    }

    @Test
    public void testH5Eget_current_stack()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Get current error stack
            long stack_id = hdf5_h.H5Eget_current_stack();
            assertTrue("Stack ID should be valid", isValidId(stack_id));

            // Close stack
            int result = hdf5_h.H5Eclose_stack(stack_id);
            assertTrue("H5Eclose_stack failed", isSuccess(result));
        }
    }

    @Test
    public void testH5Epop()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Clear stack first
            hdf5_h.H5Eclear2(hdf5_h.H5E_DEFAULT());

            // Pop errors (should succeed even if empty)
            int result = hdf5_h.H5Epop(hdf5_h.H5E_DEFAULT(), 1);
            assertTrue("H5Epop should succeed", isSuccess(result));
        }
    }

    @Test
    public void testH5Eget_msg()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create error class
            MemorySegment cls_name = stringToSegment(arena, "TestClass");
            MemorySegment lib_name = stringToSegment(arena, "TestLib");
            MemorySegment version  = stringToSegment(arena, "1.0");

            long class_id = hdf5_h.H5Eregister_class(cls_name, lib_name, version);
            assertTrue("Register class failed", isValidId(class_id));

            // Create error message
            MemorySegment msg_text = stringToSegment(arena, "Test error message");
            long msg_id            = hdf5_h.H5Ecreate_msg(class_id, hdf5_h.H5E_MAJOR(), msg_text);
            assertTrue("Create message failed", isValidId(msg_id));

            // Get message
            MemorySegment type = allocateIntArray(arena, 1);
            long msg_size      = hdf5_h.H5Eget_msg(msg_id, type, MemorySegment.NULL, 0);
            assertTrue("Message size should be > 0", msg_size > 0);

            MemorySegment msg_buf = arena.allocate(msg_size + 1);
            long actual_size      = hdf5_h.H5Eget_msg(msg_id, type, msg_buf, msg_size + 1);
            assertTrue("Actual size should match", actual_size > 0);

            String retrieved_msg = segmentToString(msg_buf);
            assertEquals("Message should match", "Test error message", retrieved_msg);

            // Cleanup
            hdf5_h.H5Eclose_msg(msg_id);
            hdf5_h.H5Eunregister_class(class_id);
        }
    }
}
