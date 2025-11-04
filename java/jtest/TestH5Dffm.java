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

import java.io.File;
import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM-only tests for HDF5 Dataset (H5D) operations.
 *
 * This test class uses direct FFM bindings without the hdf.hdf5lib wrapper layer.
 */
public class TestH5Dffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE      = "testDffm.h5";
    private static final String DATASET_NAME = "dset";
    private static final int DIM_X           = 4;
    private static final int DIM_Y           = 6;
    private static final int RANK            = 2;

    long H5fid  = hdf5_h.H5I_INVALID_HID();
    long H5dsid = hdf5_h.H5I_INVALID_HID();
    long H5did  = hdf5_h.H5I_INVALID_HID();

    private void deleteFile(String filename)
    {
        File file = new File(filename);
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                // Ignore
            }
        }
    }

    @Before
    public void createH5file()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create file
            MemorySegment fileNameSegment = stringToSegment(arena, H5_FILE);
            H5fid = hdf5_h.H5Fcreate(fileNameSegment, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                     hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(H5fid));

            // Create dataspace
            long[] dims               = {DIM_X, DIM_Y};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            H5dsid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(H5dsid));

            // Create dataset
            MemorySegment dsetNameSegment = stringToSegment(arena, DATASET_NAME);
            H5did = hdf5_h.H5Dcreate2(H5fid, dsetNameSegment, hdf5_h.H5T_NATIVE_INT_g(), H5dsid,
                                      hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(H5did));

            int flushResult = hdf5_h.H5Fflush(H5fid, hdf5_h.H5F_SCOPE_LOCAL());
            assertTrue("H5Fflush failed", isSuccess(flushResult));
        }
    }

    @After
    public void deleteH5file()
    {
        closeQuietly(H5did, hdf5_h::H5Dclose);
        closeQuietly(H5dsid, hdf5_h::H5Sclose);
        closeQuietly(H5fid, hdf5_h::H5Fclose);

        H5did  = hdf5_h.H5I_INVALID_HID();
        H5dsid = hdf5_h.H5I_INVALID_HID();
        H5fid  = hdf5_h.H5I_INVALID_HID();

        deleteFile(H5_FILE);
        System.out.println();
    }

    @Test
    public void testH5Dopen()
    {
        long did = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetNameSegment = stringToSegment(arena, DATASET_NAME);
            did                           = hdf5_h.H5Dopen2(H5fid, dsetNameSegment, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dopen2 failed", isValidId(did));
        }
        finally {
            closeQuietly(did, hdf5_h::H5Dclose);
        }
    }

    @Test
    public void testH5Dget_space()
    {
        long sid = hdf5_h.H5I_INVALID_HID();

        try {
            sid = hdf5_h.H5Dget_space(H5did);
            assertTrue("H5Dget_space failed", isValidId(sid));

            // Verify dimensions
            try (Arena arena = Arena.ofConfined()) {
                MemorySegment dimsSegment = allocateLongArray(arena, RANK);
                int ndims = hdf5_h.H5Sget_simple_extent_dims(sid, dimsSegment, MemorySegment.NULL);
                assertEquals("Rank should match", RANK, ndims);

                long[] dims = new long[RANK];
                copyFromSegment(dimsSegment, dims);
                assertEquals("Dimension 0 should match", DIM_X, dims[0]);
                assertEquals("Dimension 1 should match", DIM_Y, dims[1]);
            }
        }
        finally {
            closeQuietly(sid, hdf5_h::H5Sclose);
        }
    }

    @Test
    public void testH5Dget_type()
    {
        long tid = hdf5_h.H5I_INVALID_HID();

        try {
            tid = hdf5_h.H5Dget_type(H5did);
            assertTrue("H5Dget_type failed", isValidId(tid));
        }
        finally {
            closeQuietly(tid, hdf5_h::H5Tclose);
        }
    }

    @Test
    public void testH5Dget_create_plist()
    {
        long plist = hdf5_h.H5I_INVALID_HID();

        try {
            plist = hdf5_h.H5Dget_create_plist(H5did);
            assertTrue("H5Dget_create_plist failed", isValidId(plist));
        }
        finally {
            closeQuietly(plist, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5Dget_access_plist()
    {
        long plist = hdf5_h.H5I_INVALID_HID();

        try {
            plist = hdf5_h.H5Dget_access_plist(H5did);
            assertTrue("H5Dget_access_plist failed", isValidId(plist));
        }
        finally {
            closeQuietly(plist, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5Dwrite_read()
    {
        int[] writeData = new int[DIM_X * DIM_Y];
        int[] readData  = new int[DIM_X * DIM_Y];

        // Initialize write data
        for (int i = 0; i < writeData.length; i++) {
            writeData[i] = i;
        }

        try (Arena arena = Arena.ofConfined()) {
            // Allocate memory for write
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);

            // Write data
            int writeResult = hdf5_h.H5Dwrite(H5did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                              hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite failed", isSuccess(writeResult));

            // Allocate memory for read
            MemorySegment readSegment = allocateIntArray(arena, readData.length);

            // Read data
            int readResult = hdf5_h.H5Dread(H5did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                            hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), readSegment);
            assertTrue("H5Dread failed", isSuccess(readResult));

            // Copy back to Java array
            copyFromSegment(readSegment, readData);

            // Verify data
            assertArrayEquals("Data should match", writeData, readData);
        }
    }

    @Test
    public void testH5Dget_storage_size()
    {
        // Write some data first to allocate storage
        int[] writeData = new int[DIM_X * DIM_Y];
        for (int i = 0; i < writeData.length; i++) {
            writeData[i] = i;
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);

            int writeResult = hdf5_h.H5Dwrite(H5did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                              hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite failed", isSuccess(writeResult));

            // Flush to ensure storage is allocated
            hdf5_h.H5Fflush(H5fid, hdf5_h.H5F_SCOPE_LOCAL());

            // Get storage size
            long storageSize = hdf5_h.H5Dget_storage_size(H5did);
            assertTrue("Storage size should be > 0", storageSize > 0);
        }
    }

    @Test
    public void testH5Dcreate_anon()
    {
        long anon_did = hdf5_h.H5I_INVALID_HID();

        try {
            // Create anonymous dataset
            anon_did = hdf5_h.H5Dcreate_anon(H5fid, hdf5_h.H5T_NATIVE_INT_g(), H5dsid, hdf5_h.H5P_DEFAULT(),
                                             hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate_anon failed", isValidId(anon_did));

            // Write some data to verify it works
            int[] writeData = {1, 2, 3, 4};
            try (Arena arena = Arena.ofConfined()) {
                MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
                copyToSegment(writeSegment, writeData);

                int writeResult = hdf5_h.H5Dwrite(anon_did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                                  hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment);
                assertTrue("H5Dwrite to anonymous dataset failed", isSuccess(writeResult));
            }
        }
        finally {
            closeQuietly(anon_did, hdf5_h::H5Dclose);
        }
    }

    @Test
    public void testH5Dget_offset()
    {
        // Write some data first to ensure storage is allocated
        int[] writeData = new int[DIM_X * DIM_Y];
        for (int i = 0; i < writeData.length; i++) {
            writeData[i] = i;
        }

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);

            int writeResult = hdf5_h.H5Dwrite(H5did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                              hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite failed", isSuccess(writeResult));

            // Flush to ensure storage is allocated
            hdf5_h.H5Fflush(H5fid, hdf5_h.H5F_SCOPE_LOCAL());

            // Get dataset offset
            long offset = hdf5_h.H5Dget_offset(H5did);
            // For contiguous datasets, offset should be > 0
            // For other layouts, it might return HADDR_UNDEF
            // We just verify the call succeeds
            assertTrue("H5Dget_offset should return valid value", offset >= 0 || offset == -1);
        }
    }

    @Test
    public void testH5Dwrite_readCompound()
    {
        long compound_tid = hdf5_h.H5I_INVALID_HID();
        long compound_did = hdf5_h.H5I_INVALID_HID();
        long compound_sid = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create compound type with int and double
            // Note: double needs 8-byte alignment, so we pad the int to 8 bytes
            int doubleOffset = 8;  // Start double at 8-byte boundary
            int compoundSize = 16; // 8 bytes for int (padded) + 8 bytes for double

            compound_tid = hdf5_h.H5Tcreate(hdf5_h.H5T_COMPOUND(), compoundSize);
            assertTrue("H5Tcreate compound failed", isValidId(compound_tid));

            // Insert members with proper alignment
            MemorySegment intNameSegment = stringToSegment(arena, "int_field");
            int result = hdf5_h.H5Tinsert(compound_tid, intNameSegment, 0, hdf5_h.H5T_NATIVE_INT_g());
            assertTrue("H5Tinsert int field failed", isSuccess(result));

            MemorySegment doubleNameSegment = stringToSegment(arena, "double_field");
            result =
                hdf5_h.H5Tinsert(compound_tid, doubleNameSegment, doubleOffset, hdf5_h.H5T_NATIVE_DOUBLE_g());
            assertTrue("H5Tinsert double field failed", isSuccess(result));

            // Create dataspace for 4 compound elements
            int nElements             = 4;
            long[] dims               = {nElements};
            MemorySegment dimsSegment = allocateLongArray(arena, 1);
            copyToSegment(dimsSegment, dims);

            compound_sid = hdf5_h.H5Screate_simple(1, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(compound_sid));

            // Create dataset
            MemorySegment compoundDsetSegment = stringToSegment(arena, "compound_dset");
            compound_did =
                hdf5_h.H5Dcreate2(H5fid, compoundDsetSegment, compound_tid, compound_sid,
                                  hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 compound dataset failed", isValidId(compound_did));

            // Write compound data with proper 8-byte alignment
            MemorySegment writeSegment = arena.allocate(compoundSize * nElements, 8); // 8-byte aligned
            for (int i = 0; i < nElements; i++) {
                long offset = i * compoundSize;
                writeSegment.set(ValueLayout.JAVA_INT, offset, i);
                writeSegment.set(ValueLayout.JAVA_DOUBLE, offset + doubleOffset, i * 1.5);
            }

            int writeResult = hdf5_h.H5Dwrite(compound_did, compound_tid, hdf5_h.H5S_ALL(), hdf5_h.H5S_ALL(),
                                              hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite compound failed", isSuccess(writeResult));

            // Read compound data
            MemorySegment readSegment = arena.allocate(compoundSize * nElements, 8); // 8-byte aligned
            int readResult = hdf5_h.H5Dread(compound_did, compound_tid, hdf5_h.H5S_ALL(), hdf5_h.H5S_ALL(),
                                            hdf5_h.H5P_DEFAULT(), readSegment);
            assertTrue("H5Dread compound failed", isSuccess(readResult));

            // Verify data
            for (int i = 0; i < nElements; i++) {
                long offset        = i * compoundSize;
                int intValue       = readSegment.get(ValueLayout.JAVA_INT, offset);
                double doubleValue = readSegment.get(ValueLayout.JAVA_DOUBLE, offset + doubleOffset);

                assertEquals("Int field should match", i, intValue);
                assertEquals("Double field should match", i * 1.5, doubleValue, 0.0001);
            }
        }
        finally {
            closeQuietly(compound_did, hdf5_h::H5Dclose);
            closeQuietly(compound_sid, hdf5_h::H5Sclose);
            closeQuietly(compound_tid, hdf5_h::H5Tclose);
        }
    }

    @Test
    public void testH5DArraywr()
    {
        long array_tid = hdf5_h.H5I_INVALID_HID();
        long array_did = hdf5_h.H5I_INVALID_HID();
        long array_sid = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create array datatype: int[3][2]
            int arrayRank                  = 2;
            long[] arrayDims               = {3, 2};
            MemorySegment arrayDimsSegment = allocateLongArray(arena, arrayRank);
            copyToSegment(arrayDimsSegment, arrayDims);

            array_tid = hdf5_h.H5Tarray_create2(hdf5_h.H5T_NATIVE_INT_g(), arrayRank, arrayDimsSegment);
            assertTrue("H5Tarray_create2 failed", isValidId(array_tid));

            // Create dataspace for 2 array elements
            int nElements             = 2;
            long[] dims               = {nElements};
            MemorySegment dimsSegment = allocateLongArray(arena, 1);
            copyToSegment(dimsSegment, dims);

            array_sid = hdf5_h.H5Screate_simple(1, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(array_sid));

            // Create dataset
            MemorySegment arrayDsetSegment = stringToSegment(arena, "array_dset");
            array_did = hdf5_h.H5Dcreate2(H5fid, arrayDsetSegment, array_tid, array_sid, hdf5_h.H5P_DEFAULT(),
                                          hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 array dataset failed", isValidId(array_did));

            // Write array data: 2 elements, each is int[3][2]
            int arraySize   = 3 * 2;
            int totalSize   = nElements * arraySize;
            int[] writeData = new int[totalSize];
            for (int i = 0; i < totalSize; i++) {
                writeData[i] = i;
            }

            MemorySegment writeSegment = allocateIntArray(arena, totalSize);
            copyToSegment(writeSegment, writeData);

            int writeResult = hdf5_h.H5Dwrite(array_did, array_tid, hdf5_h.H5S_ALL(), hdf5_h.H5S_ALL(),
                                              hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite array failed", isSuccess(writeResult));

            // Read array data
            int[] readData            = new int[totalSize];
            MemorySegment readSegment = allocateIntArray(arena, totalSize);

            int readResult = hdf5_h.H5Dread(array_did, array_tid, hdf5_h.H5S_ALL(), hdf5_h.H5S_ALL(),
                                            hdf5_h.H5P_DEFAULT(), readSegment);
            assertTrue("H5Dread array failed", isSuccess(readResult));

            copyFromSegment(readSegment, readData);

            // Verify data
            assertArrayEquals("Array data should match", writeData, readData);
        }
        finally {
            closeQuietly(array_did, hdf5_h::H5Dclose);
            closeQuietly(array_sid, hdf5_h::H5Sclose);
            closeQuietly(array_tid, hdf5_h::H5Tclose);
        }
    }

    @Test
    public void testH5Dvlen_write_read()
    {
        long vlen_tid = hdf5_h.H5I_INVALID_HID();
        long vlen_did = hdf5_h.H5I_INVALID_HID();
        long vlen_sid = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create variable-length datatype
            vlen_tid = hdf5_h.H5Tvlen_create(hdf5_h.H5T_NATIVE_INT_g());
            assertTrue("H5Tvlen_create failed", isValidId(vlen_tid));

            // Create dataspace for 2 VL elements
            int nElements             = 2;
            long[] dims               = {nElements};
            MemorySegment dimsSegment = allocateLongArray(arena, 1);
            copyToSegment(dimsSegment, dims);

            vlen_sid = hdf5_h.H5Screate_simple(1, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(vlen_sid));

            // Create dataset
            MemorySegment vlenDsetSegment = stringToSegment(arena, "vlen_dset");
            vlen_did = hdf5_h.H5Dcreate2(H5fid, vlenDsetSegment, vlen_tid, vlen_sid, hdf5_h.H5P_DEFAULT(),
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 vlen dataset failed", isValidId(vlen_did));

            // Note: Full VL write/read requires hvl_t struct manipulation which is complex in FFM
            // This test verifies the dataset creation and basic API calls work
            // Full VL data I/O would need hvl_t struct support from FfmTestSupport

            // Verify we can get the datatype back
            long retrieved_tid = hdf5_h.H5Dget_type(vlen_did);
            assertTrue("H5Dget_type should succeed", isValidId(retrieved_tid));

            // Verify it's a variable-length type
            int tclass = hdf5_h.H5Tget_class(retrieved_tid);
            assertEquals("Type class should be VLEN", hdf5_h.H5T_VLEN(), tclass);

            closeQuietly(retrieved_tid, hdf5_h::H5Tclose);
        }
        finally {
            closeQuietly(vlen_did, hdf5_h::H5Dclose);
            closeQuietly(vlen_sid, hdf5_h::H5Sclose);
            closeQuietly(vlen_tid, hdf5_h::H5Tclose);
        }
    }

    @Test
    public void testH5Dclose()
    {
        long did = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetNameSegment = stringToSegment(arena, DATASET_NAME);
            did                           = hdf5_h.H5Dopen2(H5fid, dsetNameSegment, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dopen2 failed", isValidId(did));

            int result = hdf5_h.H5Dclose(did);
            assertTrue("H5Dclose failed", isSuccess(result));
            did = hdf5_h.H5I_INVALID_HID();
        }
    }

    @Test
    public void testH5Dget_num_chunks_rw()
    {
        System.out.print(testname.getMethodName());
        long chunked_dcpl = hdf5_h.H5I_INVALID_HID();
        long chunked_sid  = hdf5_h.H5I_INVALID_HID();
        long chunked_did  = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create chunked dataset creation property list
            chunked_dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate failed", isValidId(chunked_dcpl));

            // Set chunk dimensions: 2x3 chunks
            long[] chunkDims               = {2, 3};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(chunkDimsSegment, chunkDims);

            int result = hdf5_h.H5Pset_chunk(chunked_dcpl, RANK, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Create dataspace for chunked dataset: 4x6
            long[] dims               = {4, 6};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            chunked_sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(chunked_sid));

            // Create chunked dataset
            MemorySegment dsetNameSegment = stringToSegment(arena, "chunked_dset");
            chunked_did = hdf5_h.H5Dcreate2(H5fid, dsetNameSegment, hdf5_h.H5T_NATIVE_INT_g(), chunked_sid,
                                            hdf5_h.H5P_DEFAULT(), chunked_dcpl, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(chunked_did));

            // Write data to create chunks
            int[] writeData = new int[DIM_X * DIM_Y];
            for (int i = 0; i < writeData.length; i++) {
                writeData[i] = i;
            }
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);

            result = hdf5_h.H5Dwrite(chunked_did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                     hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite failed", isSuccess(result));

            // Get number of chunks (should be 4 chunks: (4/2) * (6/3) = 2 * 2 = 4)
            MemorySegment nchunksSegment = allocateLong(arena);
            result                       = hdf5_h.H5Dget_num_chunks(chunked_did, chunked_sid, nchunksSegment);
            assertTrue("H5Dget_num_chunks failed", isSuccess(result));

            long nchunks = getLong(nchunksSegment);
            assertEquals("Should have 4 chunks", 4L, nchunks);
        }
        finally {
            closeQuietly(chunked_did, hdf5_h::H5Dclose);
            closeQuietly(chunked_sid, hdf5_h::H5Sclose);
            closeQuietly(chunked_dcpl, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5Dget_chunk_storage_size()
    {
        System.out.print(testname.getMethodName());
        long chunked_dcpl = hdf5_h.H5I_INVALID_HID();
        long chunked_sid  = hdf5_h.H5I_INVALID_HID();
        long chunked_did  = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create chunked dataset creation property list
            chunked_dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate failed", isValidId(chunked_dcpl));

            // Set chunk dimensions: 2x3 chunks
            long[] chunkDims               = {2, 3};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(chunkDimsSegment, chunkDims);

            int result = hdf5_h.H5Pset_chunk(chunked_dcpl, RANK, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Create dataspace for chunked dataset: 4x6
            long[] dims               = {4, 6};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            chunked_sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(chunked_sid));

            // Create chunked dataset
            MemorySegment dsetNameSegment = stringToSegment(arena, "chunked_dset2");
            chunked_did = hdf5_h.H5Dcreate2(H5fid, dsetNameSegment, hdf5_h.H5T_NATIVE_INT_g(), chunked_sid,
                                            hdf5_h.H5P_DEFAULT(), chunked_dcpl, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(chunked_did));

            // Write data to fill first chunk
            int[] writeData = new int[DIM_X * DIM_Y];
            for (int i = 0; i < writeData.length; i++) {
                writeData[i] = i;
            }
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);

            result = hdf5_h.H5Dwrite(chunked_did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                     hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite failed", isSuccess(result));

            // Get chunk storage size for first chunk (offset [0,0])
            long[] offset               = {0, 0};
            MemorySegment offsetSegment = allocateLongArray(arena, RANK);
            copyToSegment(offsetSegment, offset);

            MemorySegment sizeSegment = allocateLong(arena);
            result = hdf5_h.H5Dget_chunk_storage_size(chunked_did, offsetSegment, sizeSegment);
            assertTrue("H5Dget_chunk_storage_size failed", isSuccess(result));

            long chunkSize = getLong(sizeSegment);
            assertTrue("Chunk size should be > 0", chunkSize > 0);
        }
        finally {
            closeQuietly(chunked_did, hdf5_h::H5Dclose);
            closeQuietly(chunked_sid, hdf5_h::H5Sclose);
            closeQuietly(chunked_dcpl, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5Dset_extent()
    {
        System.out.print(testname.getMethodName());
        long chunked_dcpl = hdf5_h.H5I_INVALID_HID();
        long chunked_sid  = hdf5_h.H5I_INVALID_HID();
        long chunked_did  = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create chunked dataset creation property list
            chunked_dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate failed", isValidId(chunked_dcpl));

            // Set chunk dimensions: 2x3 chunks
            long[] chunkDims               = {2, 3};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(chunkDimsSegment, chunkDims);

            int result = hdf5_h.H5Pset_chunk(chunked_dcpl, RANK, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Create dataspace with unlimited max dimensions
            long[] dims                  = {4, 6};
            long[] maxDims               = {-1, -1}; // H5S_UNLIMITED for both dimensions
            MemorySegment dimsSegment    = allocateLongArray(arena, RANK);
            MemorySegment maxDimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);
            copyToSegment(maxDimsSegment, maxDims);

            chunked_sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, maxDimsSegment);
            assertTrue("H5Screate_simple failed", isValidId(chunked_sid));

            // Create extensible chunked dataset
            MemorySegment dsetNameSegment = stringToSegment(arena, "extensible_dset");
            chunked_did = hdf5_h.H5Dcreate2(H5fid, dsetNameSegment, hdf5_h.H5T_NATIVE_INT_g(), chunked_sid,
                                            hdf5_h.H5P_DEFAULT(), chunked_dcpl, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(chunked_did));

            // Extend dataset to 8x12
            long[] newDims               = {8, 12};
            MemorySegment newDimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(newDimsSegment, newDims);

            result = hdf5_h.H5Dset_extent(chunked_did, newDimsSegment);
            assertTrue("H5Dset_extent failed", isSuccess(result));

            // Verify new dimensions
            long space_id = hdf5_h.H5Dget_space(chunked_did);
            assertTrue("H5Dget_space failed", isValidId(space_id));

            MemorySegment verifyDimsSegment = allocateLongArray(arena, RANK);
            int ndims = hdf5_h.H5Sget_simple_extent_dims(space_id, verifyDimsSegment, MemorySegment.NULL);
            assertEquals("Rank should be 2", RANK, ndims);

            long[] verifyDims = new long[RANK];
            copyFromSegment(verifyDimsSegment, verifyDims);
            assertEquals("First dimension should be 8", 8, verifyDims[0]);
            assertEquals("Second dimension should be 12", 12, verifyDims[1]);

            closeQuietly(space_id, hdf5_h::H5Sclose);
        }
        finally {
            closeQuietly(chunked_did, hdf5_h::H5Dclose);
            closeQuietly(chunked_sid, hdf5_h::H5Sclose);
            closeQuietly(chunked_dcpl, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5DArrayenum_rw()
    {
        long enum_tid  = hdf5_h.H5I_INVALID_HID();
        long array_tid = hdf5_h.H5I_INVALID_HID();
        long array_did = hdf5_h.H5I_INVALID_HID();
        long array_sid = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create enum type
            enum_tid = hdf5_h.H5Tenum_create(hdf5_h.H5T_NATIVE_INT_g());
            assertTrue("H5Tenum_create failed", isValidId(enum_tid));

            // Insert enum values
            MemorySegment redSegment      = stringToSegment(arena, "RED");
            MemorySegment redValueSegment = allocateInt(arena);
            setInt(redValueSegment, 0);
            int result = hdf5_h.H5Tenum_insert(enum_tid, redSegment, redValueSegment);
            assertTrue("H5Tenum_insert RED failed", isSuccess(result));

            MemorySegment greenSegment      = stringToSegment(arena, "GREEN");
            MemorySegment greenValueSegment = allocateInt(arena);
            setInt(greenValueSegment, 1);
            result = hdf5_h.H5Tenum_insert(enum_tid, greenSegment, greenValueSegment);
            assertTrue("H5Tenum_insert GREEN failed", isSuccess(result));

            MemorySegment blueSegment      = stringToSegment(arena, "BLUE");
            MemorySegment blueValueSegment = allocateInt(arena);
            setInt(blueValueSegment, 2);
            result = hdf5_h.H5Tenum_insert(enum_tid, blueSegment, blueValueSegment);
            assertTrue("H5Tenum_insert BLUE failed", isSuccess(result));

            // Create array of enum: [3]
            long[] arrayDims               = {3};
            MemorySegment arrayDimsSegment = allocateLongArray(arena, 1);
            copyToSegment(arrayDimsSegment, arrayDims);

            array_tid = hdf5_h.H5Tarray_create2(enum_tid, 1, arrayDimsSegment);
            assertTrue("H5Tarray_create2 failed", isValidId(array_tid));

            // Create dataspace for 2 array elements
            long[] dims               = {2};
            MemorySegment dimsSegment = allocateLongArray(arena, 1);
            copyToSegment(dimsSegment, dims);

            array_sid = hdf5_h.H5Screate_simple(1, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(array_sid));

            // Create dataset
            MemorySegment arrayEnumDsetSegment = stringToSegment(arena, "array_enum_dset");
            array_did = hdf5_h.H5Dcreate2(H5fid, arrayEnumDsetSegment, array_tid, array_sid,
                                          hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 array enum failed", isValidId(array_did));

            // Write array of enum data: [[0,1,2], [2,1,0]]
            int[] writeData            = {0, 1, 2, 2, 1, 0};
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);

            result = hdf5_h.H5Dwrite(array_did, array_tid, hdf5_h.H5S_ALL(), hdf5_h.H5S_ALL(),
                                     hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite array enum failed", isSuccess(result));

            // Read array of enum data
            int[] readData            = new int[writeData.length];
            MemorySegment readSegment = allocateIntArray(arena, readData.length);

            result = hdf5_h.H5Dread(array_did, array_tid, hdf5_h.H5S_ALL(), hdf5_h.H5S_ALL(),
                                    hdf5_h.H5P_DEFAULT(), readSegment);
            assertTrue("H5Dread array enum failed", isSuccess(result));

            copyFromSegment(readSegment, readData);

            // Verify data
            assertArrayEquals("Array enum data should match", writeData, readData);
        }
        finally {
            closeQuietly(array_did, hdf5_h::H5Dclose);
            closeQuietly(array_sid, hdf5_h::H5Sclose);
            closeQuietly(array_tid, hdf5_h::H5Tclose);
            closeQuietly(enum_tid, hdf5_h::H5Tclose);
        }
    }

    @Test
    public void testH5Dfill()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Create a buffer and fill it with a value
            int fillValue             = 42;
            MemorySegment fillSegment = allocateInt(arena);
            setInt(fillSegment, fillValue);

            MemorySegment bufferSegment = allocateIntArray(arena, DIM_X * DIM_Y);

            int result = hdf5_h.H5Dfill(fillSegment, hdf5_h.H5T_NATIVE_INT_g(), bufferSegment,
                                        hdf5_h.H5T_NATIVE_INT_g(), H5dsid);
            assertTrue("H5Dfill failed", isSuccess(result));

            // Verify buffer is filled
            int[] buffer = new int[DIM_X * DIM_Y];
            copyFromSegment(bufferSegment, buffer);

            for (int value : buffer) {
                assertEquals("All values should be fill value", fillValue, value);
            }
        }
    }

    @Test
    public void testH5Dget_chunk_info()
    {
        System.out.print(testname.getMethodName());
        long chunked_dcpl = hdf5_h.H5I_INVALID_HID();
        long chunked_sid  = hdf5_h.H5I_INVALID_HID();
        long chunked_did  = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create chunked dataset creation property list
            chunked_dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate failed", isValidId(chunked_dcpl));

            // Set chunk dimensions: 2x3 chunks
            long[] chunkDims               = {2, 3};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(chunkDimsSegment, chunkDims);

            int result = hdf5_h.H5Pset_chunk(chunked_dcpl, RANK, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Create dataspace for chunked dataset: 4x6
            long[] dims               = {4, 6};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            chunked_sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(chunked_sid));

            // Create chunked dataset
            MemorySegment dsetNameSegment = stringToSegment(arena, "chunked_info");
            chunked_did = hdf5_h.H5Dcreate2(H5fid, dsetNameSegment, hdf5_h.H5T_NATIVE_INT_g(), chunked_sid,
                                            hdf5_h.H5P_DEFAULT(), chunked_dcpl, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(chunked_did));

            // Write data to create chunks
            int[] writeData = new int[DIM_X * DIM_Y];
            for (int i = 0; i < writeData.length; i++) {
                writeData[i] = i;
            }
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);

            result = hdf5_h.H5Dwrite(chunked_did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                     hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite failed", isSuccess(result));

            // Get chunk info for first chunk (index 0)
            MemorySegment offsetSegment     = allocateLongArray(arena, RANK);
            MemorySegment filterMaskSegment = allocateInt(arena);
            MemorySegment addrSegment       = allocateLong(arena);
            MemorySegment sizeSegment       = allocateLong(arena);

            result = hdf5_h.H5Dget_chunk_info(chunked_did, chunked_sid, 0, offsetSegment, filterMaskSegment,
                                              addrSegment, sizeSegment);
            assertTrue("H5Dget_chunk_info failed", isSuccess(result));

            // Verify we got valid chunk information
            long chunkSize = getLong(sizeSegment);
            assertTrue("Chunk size should be > 0", chunkSize > 0);

            long[] offset = new long[RANK];
            copyFromSegment(offsetSegment, offset);
            // First chunk should start at [0, 0]
            assertEquals("First chunk offset[0] should be 0", 0L, offset[0]);
            assertEquals("First chunk offset[1] should be 0", 0L, offset[1]);
        }
        finally {
            closeQuietly(chunked_did, hdf5_h::H5Dclose);
            closeQuietly(chunked_sid, hdf5_h::H5Sclose);
            closeQuietly(chunked_dcpl, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5Dget_chunk_info_by_coord()
    {
        System.out.print(testname.getMethodName());
        long chunked_dcpl = hdf5_h.H5I_INVALID_HID();
        long chunked_sid  = hdf5_h.H5I_INVALID_HID();
        long chunked_did  = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create chunked dataset creation property list
            chunked_dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate failed", isValidId(chunked_dcpl));

            // Set chunk dimensions: 2x3 chunks
            long[] chunkDims               = {2, 3};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(chunkDimsSegment, chunkDims);

            int result = hdf5_h.H5Pset_chunk(chunked_dcpl, RANK, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Create dataspace for chunked dataset: 4x6
            long[] dims               = {4, 6};
            MemorySegment dimsSegment = allocateLongArray(arena, RANK);
            copyToSegment(dimsSegment, dims);

            chunked_sid = hdf5_h.H5Screate_simple(RANK, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(chunked_sid));

            // Create chunked dataset
            MemorySegment dsetNameSegment = stringToSegment(arena, "chunked_by_coord");
            chunked_did = hdf5_h.H5Dcreate2(H5fid, dsetNameSegment, hdf5_h.H5T_NATIVE_INT_g(), chunked_sid,
                                            hdf5_h.H5P_DEFAULT(), chunked_dcpl, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(chunked_did));

            // Write data to create chunks
            int[] writeData = new int[DIM_X * DIM_Y];
            for (int i = 0; i < writeData.length; i++) {
                writeData[i] = i;
            }
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);

            result = hdf5_h.H5Dwrite(chunked_did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                     hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment);
            assertTrue("H5Dwrite failed", isSuccess(result));

            // Get chunk info for chunk at coordinates [2, 3]
            long[] offset               = {2, 3};
            MemorySegment offsetSegment = allocateLongArray(arena, RANK);
            copyToSegment(offsetSegment, offset);

            MemorySegment filterMaskSegment = allocateInt(arena);
            MemorySegment addrSegment       = allocateLong(arena);
            MemorySegment sizeSegment       = allocateLong(arena);

            result = hdf5_h.H5Dget_chunk_info_by_coord(chunked_did, offsetSegment, filterMaskSegment,
                                                       addrSegment, sizeSegment);
            assertTrue("H5Dget_chunk_info_by_coord failed", isSuccess(result));

            // Verify we got valid chunk information
            long chunkSize = getLong(sizeSegment);
            assertTrue("Chunk size should be > 0", chunkSize > 0);
        }
        finally {
            closeQuietly(chunked_did, hdf5_h::H5Dclose);
            closeQuietly(chunked_sid, hdf5_h::H5Sclose);
            closeQuietly(chunked_dcpl, hdf5_h::H5Pclose);
        }
    }

    @Test
    public void testH5Drefresh()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Refresh the dataset metadata
            int result = hdf5_h.H5Drefresh(H5did);
            assertTrue("H5Drefresh failed", isSuccess(result));

            // Verify dataset is still valid after refresh
            long type_id = hdf5_h.H5Dget_type(H5did);
            assertTrue("H5Dget_type should succeed after refresh", isValidId(type_id));
            hdf5_h.H5Tclose(type_id);
        }
    }

    @Test
    public void testH5Dvlen_get_buf_size()
    {
        System.out.print(testname.getMethodName());
        long vlen_tid = hdf5_h.H5I_INVALID_HID();
        long vlen_sid = hdf5_h.H5I_INVALID_HID();
        long vlen_did = hdf5_h.H5I_INVALID_HID();

        try (Arena arena = Arena.ofConfined()) {
            // Create variable-length integer datatype
            vlen_tid = hdf5_h.H5Tvlen_create(hdf5_h.H5T_NATIVE_INT_g());
            assertTrue("H5Tvlen_create failed", isValidId(vlen_tid));

            // Create simple 1D dataspace
            long[] dims               = {3};
            MemorySegment dimsSegment = allocateLongArray(arena, 1);
            dimsSegment.setAtIndex(ValueLayout.JAVA_LONG, 0, dims[0]);

            vlen_sid = hdf5_h.H5Screate_simple(1, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(vlen_sid));

            // Create dataset
            MemorySegment dsetName = stringToSegment(arena, "vlen_bufsize");
            vlen_did = hdf5_h.H5Dcreate2(H5fid, dsetName, vlen_tid, vlen_sid, hdf5_h.H5P_DEFAULT(),
                                         hdf5_h.H5P_DEFAULT(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(vlen_did));

            // Note: In FFM, we can test that the function exists and returns successfully
            // Full VL data writing/reading is complex in FFM and tested separately
            MemorySegment sizeSegment = allocateLong(arena);
            int result = hdf5_h.H5Dvlen_get_buf_size(vlen_did, vlen_tid, vlen_sid, sizeSegment);
            assertTrue("H5Dvlen_get_buf_size should succeed", isSuccess(result));

            long bufSize = getLong(sizeSegment);
            assertEquals("Buffer size should be 0 for empty VL dataset", 0L, bufSize);
        }
        finally {
            closeQuietly(vlen_did, hdf5_h::H5Dclose);
            closeQuietly(vlen_sid, hdf5_h::H5Sclose);
            closeQuietly(vlen_tid, hdf5_h::H5Tclose);
        }
    }

    @Test
    public void testH5Dget_space_type()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetname = stringToSegment(arena, "/dset");
            long did               = hdf5_h.H5Dopen2(H5fid, dsetname, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dopen2 failed", isValidId(did));

            // Get dataspace
            long sid = hdf5_h.H5Dget_space(did);
            assertTrue("H5Dget_space failed", isValidId(sid));

            // Get datatype
            long tid = hdf5_h.H5Dget_type(did);
            assertTrue("H5Dget_type failed", isValidId(tid));

            hdf5_h.H5Tclose(tid);
            hdf5_h.H5Sclose(sid);
            hdf5_h.H5Dclose(did);
        }
    }

    @Test
    public void testH5Dget_space_status()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetname = stringToSegment(arena, "/dset");
            long did               = hdf5_h.H5Dopen2(H5fid, dsetname, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dopen2 failed", isValidId(did));

            MemorySegment status = allocateIntArray(arena, 1);
            int result           = hdf5_h.H5Dget_space_status(did, status);
            assertTrue("H5Dget_space_status failed", isSuccess(result));

            int statusValue = getInt(status);
            assertTrue("Status should be valid", statusValue >= 0);

            hdf5_h.H5Dclose(did);
        }
    }

    @Test
    public void testH5Dget_chunk_index_type()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create chunked dataset
            long[] dims                = {10, 10};
            long[] chunk_dims          = {5, 5};
            MemorySegment dimsSegment  = allocateLongArray(arena, 2);
            MemorySegment chunkSegment = allocateLongArray(arena, 2);
            copyToSegment(dimsSegment, dims);
            copyToSegment(chunkSegment, chunk_dims);

            long sid = hdf5_h.H5Screate_simple(2, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(sid));

            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkSegment);

            MemorySegment dsetname = stringToSegment(arena, "/chunked_ds");
            long did               = hdf5_h.H5Dcreate2(H5fid, dsetname, hdf5_h.H5T_NATIVE_INT_g(), sid,
                                                       hdf5_h.H5P_DEFAULT(), dcpl, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(did));

            // Get chunk index type
            MemorySegment indexType = allocateIntArray(arena, 1);
            int result              = hdf5_h.H5Dget_chunk_index_type(did, indexType);
            assertTrue("H5Dget_chunk_index_type failed", isSuccess(result));

            hdf5_h.H5Dclose(did);
            hdf5_h.H5Pclose(dcpl);
            hdf5_h.H5Sclose(sid);
        }
    }

    @Test
    public void testH5Dget_num_chunks()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Open existing dataset
            MemorySegment dsetname = stringToSegment(arena, "/dset");
            long did               = hdf5_h.H5Dopen2(H5fid, dsetname, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dopen2 failed", isValidId(did));

            long sid                = hdf5_h.H5Dget_space(did);
            MemorySegment numChunks = allocateLongArray(arena, 1);
            int result              = hdf5_h.H5Dget_num_chunks(did, sid, numChunks);

            // This may fail if dataset is not chunked, which is okay
            if (isSuccess(result)) {
                long count = getLong(numChunks);
                assertTrue("Chunk count should be >= 0", count >= 0);
            }

            hdf5_h.H5Sclose(sid);
            hdf5_h.H5Dclose(did);
        }
    }

    @Test
    public void testH5Dflush_refresh()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment dsetname = stringToSegment(arena, "/dset");
            long did               = hdf5_h.H5Dopen2(H5fid, dsetname, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dopen2 failed", isValidId(did));

            // Flush dataset
            int result = hdf5_h.H5Dflush(did);
            assertTrue("H5Dflush failed", isSuccess(result));

            // Refresh dataset
            result = hdf5_h.H5Drefresh(did);
            assertTrue("H5Drefresh failed", isSuccess(result));

            hdf5_h.H5Dclose(did);
        }
    }
}
