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
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.H5I_free_t;
import org.hdfgroup.javahdf5.H5I_iterate_func_t;
import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Identifier (H5I) operations.
 */
public class TestH5Iffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE = "test_H5Iffm.h5";

    long H5fid = hdf5_h.H5I_INVALID_HID();
    long H5gid = hdf5_h.H5I_INVALID_HID();

    @Before
    public void createH5file()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment filename = stringToSegment(arena, H5_FILE);
            H5fid                  = hdf5_h.H5Fcreate(filename, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            MemorySegment groupname = stringToSegment(arena, "Group1");
            H5gid = hdf5_h.H5Gcreate2(H5fid, groupname, hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(),
                                      hdf5_h.H5P_DEFAULT());
            assertTrue("H5Gcreate2 failed", isValidId(H5gid));
        }
    }

    @After
    public void deleteH5file()
    {
        if (isValidId(H5gid)) {
            closeQuietly(H5gid, hdf5_h::H5Gclose);
            H5gid = hdf5_h.H5I_INVALID_HID();
        }
        if (isValidId(H5fid)) {
            closeQuietly(H5fid, hdf5_h::H5Fclose);
            H5fid = hdf5_h.H5I_INVALID_HID();
        }
        System.out.println();
    }

    @Test
    public void testH5Iget_type()
    {
        int file_type = hdf5_h.H5Iget_type(H5fid);
        assertEquals("File type should be H5I_FILE", hdf5_h.H5I_FILE(), file_type);

        int group_type = hdf5_h.H5Iget_type(H5gid);
        assertEquals("Group type should be H5I_GROUP", hdf5_h.H5I_GROUP(), group_type);
    }

    @Test
    public void testH5Iis_valid()
    {
        int result = hdf5_h.H5Iis_valid(H5fid);
        assertTrue("File ID should be valid", result > 0);

        result = hdf5_h.H5Iis_valid(H5gid);
        assertTrue("Group ID should be valid", result > 0);

        result = hdf5_h.H5Iis_valid(hdf5_h.H5I_INVALID_HID());
        assertEquals("Invalid ID should not be valid", 0, result);
    }

    @Test
    public void testH5Iget_name()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Get group name size
            long name_size = hdf5_h.H5Iget_name(H5gid, MemorySegment.NULL, 0);
            assertTrue("H5Iget_name size query failed", name_size > 0);

            // Get group name
            MemorySegment nameBuffer = arena.allocate(name_size + 1);
            long actual_size         = hdf5_h.H5Iget_name(H5gid, nameBuffer, name_size + 1);
            assertTrue("H5Iget_name failed", actual_size > 0);

            String name = nameBuffer.getString(0);
            assertEquals("Group name should be /Group1", "/Group1", name);
        }
    }

    @Test
    public void testH5Iget_file_id()
    {
        long file_id = hdf5_h.H5Iget_file_id(H5gid);
        assertTrue("H5Iget_file_id failed", isValidId(file_id));

        int type = hdf5_h.H5Iget_type(file_id);
        assertEquals("Should be file type", hdf5_h.H5I_FILE(), type);

        hdf5_h.H5Fclose(file_id);
    }

    @Test
    public void testH5Iinc_dec_ref()
    {
        // Get initial ref count
        int ref_count = hdf5_h.H5Iget_ref(H5gid);
        assertTrue("Initial ref count should be positive", ref_count > 0);

        // Increment ref count
        int new_count = hdf5_h.H5Iinc_ref(H5gid);
        assertEquals("Ref count should increase by 1", ref_count + 1, new_count);

        // Decrement ref count
        new_count = hdf5_h.H5Idec_ref(H5gid);
        assertEquals("Ref count should decrease by 1", ref_count, new_count);
    }

    @Test
    public void testH5I_complete_workflow()
    {
        try (Arena arena = Arena.ofConfined()) {
            // 1. Verify ID is valid
            int result = hdf5_h.H5Iis_valid(H5gid);
            assertTrue("ID should be valid", result > 0);

            // 2. Get type
            int type = hdf5_h.H5Iget_type(H5gid);
            assertEquals("Type should be GROUP", hdf5_h.H5I_GROUP(), type);

            // 3. Get name
            long name_size = hdf5_h.H5Iget_name(H5gid, MemorySegment.NULL, 0);
            assertTrue("Name size should be positive", name_size > 0);

            // 4. Get file ID
            long file_id = hdf5_h.H5Iget_file_id(H5gid);
            assertTrue("File ID should be valid", isValidId(file_id));

            // 5. Get ref count
            int ref_count = hdf5_h.H5Iget_ref(H5gid);
            assertTrue("Ref count should be positive", ref_count > 0);

            hdf5_h.H5Fclose(file_id);
        }
    }

    // ============================================================================
    // User-Defined ID Type Tests (H5Iregister_type2)
    // ============================================================================

    @Test
    public void testH5Iregister_type2()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Define free function callback (no-op for this test)
            H5I_free_t.Function freeFunc = (MemorySegment obj, MemorySegment request) ->
            {
                // Simple free function that does nothing
                // In real usage, this would free memory associated with the object
                return 0; // Success
            };

            // Allocate callback function pointer
            MemorySegment freeFuncPtr = H5I_free_t.allocate(freeFunc, arena);

            // Register a new user-defined type
            int myType = hdf5_h.H5Iregister_type2(0, freeFuncPtr);
            assertTrue("H5Iregister_type2 should succeed", myType >= hdf5_h.H5I_NTYPES());

            // Verify type exists
            int exists = hdf5_h.H5Itype_exists(myType);
            assertTrue("User type should exist", exists > 0);

            // Get initial member count (should be 0)
            MemorySegment numMembers = allocateLongArray(arena, 1);
            int result               = hdf5_h.H5Inmembers(myType, numMembers);
            assertTrue("H5Inmembers should succeed", isSuccess(result));
            assertEquals("Should have 0 members initially", 0L, getLong(numMembers));

            // Destroy the type
            result = hdf5_h.H5Idestroy_type(myType);
            assertTrue("H5Idestroy_type should succeed", isSuccess(result));

            // Verify type no longer exists
            exists = hdf5_h.H5Itype_exists(myType);
            assertEquals("User type should not exist after destroy", 0, exists);
        }
    }

    @Test
    public void testH5Iregister_and_operations()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Track whether free function was called
            MemorySegment freeCalled = allocateIntArray(arena, 1);
            freeCalled.set(ValueLayout.JAVA_INT, 0, 0);

            // Define free function callback that tracks calls
            H5I_free_t.Function freeFunc = (MemorySegment obj, MemorySegment request) ->
            {
                freeCalled.set(ValueLayout.JAVA_INT, 0, 1);
                return 0; // Success
            };

            MemorySegment freeFuncPtr = H5I_free_t.allocate(freeFunc, arena);

            // Register user-defined type
            int myType = hdf5_h.H5Iregister_type2(0, freeFuncPtr);
            assertTrue("H5Iregister_type2 should succeed", myType >= hdf5_h.H5I_NTYPES());

            // Create a test object (just a simple integer in memory)
            MemorySegment testObj = allocateIntArray(arena, 1);
            testObj.set(ValueLayout.JAVA_INT, 0, 42);

            // Register the object with the user type
            long objId = hdf5_h.H5Iregister(myType, testObj);
            assertTrue("H5Iregister should succeed", isValidId(objId));

            // Verify ID is valid
            int valid = hdf5_h.H5Iis_valid(objId);
            assertTrue("Object ID should be valid", valid > 0);

            // Verify ID type matches
            int idType = hdf5_h.H5Iget_type(objId);
            assertEquals("ID type should match registered type", myType, idType);

            // Check member count (should be 1 now)
            MemorySegment numMembers = allocateLongArray(arena, 1);
            int result               = hdf5_h.H5Inmembers(myType, numMembers);
            assertTrue("H5Inmembers should succeed", isSuccess(result));
            assertEquals("Should have 1 member", 1L, getLong(numMembers));

            // Increment reference count
            int refCount = hdf5_h.H5Iinc_ref(objId);
            assertEquals("Ref count should be 2", 2, refCount);

            // Decrement reference count
            refCount = hdf5_h.H5Idec_ref(objId);
            assertEquals("Ref count should be 1", 1, refCount);

            // Clear type (should call free function)
            result = hdf5_h.H5Iclear_type(myType, false);
            assertTrue("H5Iclear_type should succeed", isSuccess(result));

            // Verify free function was called
            assertEquals("Free function should have been called", 1, getInt(freeCalled));

            // Verify member count is now 0
            result = hdf5_h.H5Inmembers(myType, numMembers);
            assertTrue("H5Inmembers should succeed", isSuccess(result));
            assertEquals("Should have 0 members after clear", 0L, getLong(numMembers));

            // Destroy type
            result = hdf5_h.H5Idestroy_type(myType);
            assertTrue("H5Idestroy_type should succeed", isSuccess(result));
        }
    }

    @Test
    public void testH5Iiterate_user_type()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Free function
            H5I_free_t.Function freeFunc = (MemorySegment obj, MemorySegment request) -> 0;
            MemorySegment freeFuncPtr    = H5I_free_t.allocate(freeFunc, arena);

            // Register user type
            int myType = hdf5_h.H5Iregister_type2(0, freeFuncPtr);
            assertTrue("H5Iregister_type2 should succeed", myType >= hdf5_h.H5I_NTYPES());

            // Register 3 objects
            MemorySegment obj1 = allocateIntArray(arena, 1);
            MemorySegment obj2 = allocateIntArray(arena, 1);
            MemorySegment obj3 = allocateIntArray(arena, 1);
            obj1.set(ValueLayout.JAVA_INT, 0, 10);
            obj2.set(ValueLayout.JAVA_INT, 0, 20);
            obj3.set(ValueLayout.JAVA_INT, 0, 30);

            long id1 = hdf5_h.H5Iregister(myType, obj1);
            long id2 = hdf5_h.H5Iregister(myType, obj2);
            long id3 = hdf5_h.H5Iregister(myType, obj3);
            assertTrue("All IDs should be valid", isValidId(id1) && isValidId(id2) && isValidId(id3));

            // Iterate and count IDs
            MemorySegment counter = allocateIntArray(arena, 1);
            counter.set(ValueLayout.JAVA_INT, 0, 0);

            H5I_iterate_func_t.Function callback = (long id, MemorySegment udata) ->
            {
                int current = udata.get(ValueLayout.JAVA_INT, 0);
                udata.set(ValueLayout.JAVA_INT, 0, current + 1);
                return 0; // Continue
            };

            MemorySegment callbackPtr = H5I_iterate_func_t.allocate(callback, arena);
            int result                = hdf5_h.H5Iiterate(myType, callbackPtr, counter);
            assertTrue("H5Iiterate should succeed", isSuccess(result));

            // Should have iterated over all 3 objects
            assertEquals("Should iterate over 3 IDs", 3, getInt(counter));

            // Cleanup - clear type first to free all IDs
            result = hdf5_h.H5Iclear_type(myType, false);
            assertTrue("H5Iclear_type should succeed", isSuccess(result));
            result = hdf5_h.H5Idestroy_type(myType);
            assertTrue("H5Idestroy_type should succeed", isSuccess(result));
        }
    }

    @Test
    public void testH5Itype_ref_counting()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Free function
            H5I_free_t.Function freeFunc = (MemorySegment obj, MemorySegment request) -> 0;
            MemorySegment freeFuncPtr    = H5I_free_t.allocate(freeFunc, arena);

            // Register user type
            int myType = hdf5_h.H5Iregister_type2(0, freeFuncPtr);
            assertTrue("H5Iregister_type2 should succeed", myType >= hdf5_h.H5I_NTYPES());

            // Get initial type ref count
            int initialRef = hdf5_h.H5Iget_type_ref(myType);
            assertTrue("Initial ref count should be positive", initialRef > 0);

            // Increment type ref count
            int newRef = hdf5_h.H5Iinc_type_ref(myType);
            assertEquals("Ref count should increase by 1", initialRef + 1, newRef);

            // Verify with get
            int currentRef = hdf5_h.H5Iget_type_ref(myType);
            assertEquals("Ref count should match", newRef, currentRef);

            // Decrement type ref count
            newRef = hdf5_h.H5Idec_type_ref(myType);
            assertEquals("Ref count should decrease by 1", initialRef, newRef);

            // Verify type still exists
            int exists = hdf5_h.H5Itype_exists(myType);
            assertTrue("Type should still exist", exists > 0);

            // Final cleanup
            int result = hdf5_h.H5Idestroy_type(myType);
            assertTrue("H5Idestroy_type should succeed", isSuccess(result));
        }
    }

    @Test
    public void testH5Iget_type_ref()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a user type (library types like H5I_FILE cannot be used with this API)
            int user_type = hdf5_h.H5Iregister_type2(0, MemorySegment.NULL);
            assertTrue("Register type failed", isValidId(user_type));

            // Get reference count for the user type
            int ref_count = hdf5_h.H5Iget_type_ref(user_type);
            assertTrue("Type ref count should be >= 0", ref_count >= 0);

            // Cleanup
            hdf5_h.H5Idestroy_type(user_type);
        }
    }

    @Test
    public void testH5Iinc_type_ref()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a user type (library types like H5I_DATATYPE cannot be used with this API)
            int user_type = hdf5_h.H5Iregister_type2(0, MemorySegment.NULL);
            assertTrue("Register type failed", isValidId(user_type));

            // Get initial ref count
            int initial_ref = hdf5_h.H5Iget_type_ref(user_type);
            assertTrue("Initial ref should be >= 0", initial_ref >= 0);

            // Increment type ref count
            int new_ref = hdf5_h.H5Iinc_type_ref(user_type);
            assertEquals("Ref should increment", initial_ref + 1, new_ref);

            // Decrement back
            int dec_ref = hdf5_h.H5Idec_type_ref(user_type);
            assertEquals("Ref should decrement", initial_ref, dec_ref);

            // Cleanup
            hdf5_h.H5Idestroy_type(user_type);
        }
    }

    @Test
    public void testH5Inmembers()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a user type (library types like H5I_FILE cannot be used with this API)
            int user_type = hdf5_h.H5Iregister_type2(0, MemorySegment.NULL);
            assertTrue("Register type failed", isValidId(user_type));

            // Get number of members of the user type
            MemorySegment num_members = allocateLongArray(arena, 1);
            int result                = hdf5_h.H5Inmembers(user_type, num_members);
            assertTrue("H5Inmembers should succeed", isSuccess(result));

            long count = getLong(num_members);
            assertTrue("Member count should be >= 0", count >= 0);

            // Cleanup
            hdf5_h.H5Idestroy_type(user_type);
        }
    }

    @Test
    public void testH5Iclear_type()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Register a user-defined type
            int user_type = hdf5_h.H5Iregister_type2(0, MemorySegment.NULL);
            assertTrue("Register type failed", isValidId(user_type));

            // Clear type (remove all objects of this type)
            int result = hdf5_h.H5Iclear_type(user_type, false);
            assertTrue("H5Iclear_type should succeed", isSuccess(result));

            // Destroy type
            result = hdf5_h.H5Idestroy_type(user_type);
            assertTrue("Destroy type should succeed", isSuccess(result));
        }
    }

    @Test
    public void testH5Itype_exists()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create a user type to test existence
            int user_type = hdf5_h.H5Iregister_type2(0, MemorySegment.NULL);
            assertTrue("Register type failed", isValidId(user_type));

            // Check if the user type exists
            int exists = hdf5_h.H5Itype_exists(user_type);
            assertTrue("User type should exist", exists > 0);

            // Destroy the type
            int result = hdf5_h.H5Idestroy_type(user_type);
            assertTrue("Destroy type should succeed", isSuccess(result));

            // After destruction, type should not exist
            exists = hdf5_h.H5Itype_exists(user_type);
            assertTrue("Destroyed type should not exist", exists == 0);
        }
    }
}
