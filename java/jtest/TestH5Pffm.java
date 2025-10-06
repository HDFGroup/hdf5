package jtest;

import static org.junit.Assert.*;

import static jtest.FfmTestSupport.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.hdf5_h;
import org.hdfgroup.javahdf5.hdf5_h_1;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Pffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE = "testPffm.h5";

    long H5fid  = hdf5_h.H5I_INVALID_HID();
    long H5fcpl = hdf5_h.H5I_INVALID_HID();
    long H5fapl = hdf5_h.H5I_INVALID_HID();
    long H5dcpl = hdf5_h.H5I_INVALID_HID();
    long H5dxpl = hdf5_h.H5I_INVALID_HID();

    private static void _deleteFile(String filename)
    {
        java.io.File file = new java.io.File(filename);
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
        // Ensure HDF5 library is initialized (prevents FFM constant initialization issues)
        hdf5_h.H5open();

        H5fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
        assertTrue("H5Pcreate fcpl", isValidId(H5fcpl));

        H5fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
        assertTrue("H5Pcreate fapl", isValidId(H5fapl));

        try (Arena arena = Arena.ofConfined()) {
            H5fid = hdf5_h.H5Fcreate(stringToSegment(arena, H5_FILE), hdf5_h.H5F_ACC_TRUNC(), H5fcpl, H5fapl);
        }
        assertTrue("H5Fcreate", isValidId(H5fid));
    }

    @After
    public void deleteH5file()
    {
        if (H5dxpl > 0)
            try {
                hdf5_h.H5Pclose(H5dxpl);
            }
            catch (Exception ex) {
            }
        if (H5dcpl > 0)
            try {
                hdf5_h.H5Pclose(H5dcpl);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                hdf5_h.H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }
        if (H5fapl > 0)
            try {
                hdf5_h.H5Pclose(H5fapl);
            }
            catch (Exception ex) {
            }
        if (H5fcpl > 0)
            try {
                hdf5_h.H5Pclose(H5fcpl);
            }
            catch (Exception ex) {
            }

        _deleteFile(H5_FILE);
    }

    // =========================
    // Generic Property List Tests
    // =========================

    @Test
    public void testH5Pcreate()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create dataset creation property list
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Verify it's the right class
            long cls = hdf5_h.H5Pget_class(dcpl);
            assertTrue("H5Pget_class failed", isValidId(cls));

            int equal = hdf5_h.H5Pequal(cls, hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("Class should match H5P_DATASET_CREATE", equal > 0);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pclose()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create property list
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Close it
            int result = hdf5_h.H5Pclose(dcpl);
            assertTrue("H5Pclose failed", isSuccess(result));

            // Verify it's closed (H5Iis_valid should return false)
            int valid = hdf5_h.H5Iis_valid(dcpl);
            assertEquals("Property list should be invalid after close", 0, valid);
        }
    }

    @Test
    public void testH5Pcopy()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create dataset creation property list with chunk settings
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Set chunk dimensions
            long[] chunkDims       = {10, 20};
            MemorySegment chunkSeg = allocateLongArray(arena, 2);
            copyToSegment(chunkSeg, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkSeg);

            // Copy property list
            long dcpl_copy = hdf5_h.H5Pcopy(dcpl);
            assertTrue("H5Pcopy failed", isValidId(dcpl_copy));

            // Verify copy has same settings
            MemorySegment outChunk = allocateLongArray(arena, 2);
            int ndims              = hdf5_h.H5Pget_chunk(dcpl_copy, 2, outChunk);
            assertEquals("Should have 2 dimensions", 2, ndims);

            long[] retrieved = new long[2];
            copyFromSegment(outChunk, retrieved);
            assertArrayEquals("Chunk dimensions should match in copy", chunkDims, retrieved);

            hdf5_h.H5Pclose(dcpl_copy);
            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pequal()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create two identical property lists
            long dcpl1 = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            long dcpl2 = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());

            // They should be equal (both are default dataset create plists)
            int equal = hdf5_h.H5Pequal(dcpl1, dcpl2);
            assertTrue("Default property lists should be equal", equal > 0);

            // Modify one
            long[] chunkDims       = {10, 20};
            MemorySegment chunkSeg = allocateLongArray(arena, 2);
            copyToSegment(chunkSeg, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl1, 2, chunkSeg);

            // Now they should be different
            equal = hdf5_h.H5Pequal(dcpl1, dcpl2);
            assertEquals("Modified property lists should not be equal", 0, equal);

            hdf5_h.H5Pclose(dcpl2);
            hdf5_h.H5Pclose(dcpl1);
        }
    }

    @Test
    public void testH5Pget_class()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create different types of property lists
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());

            // Get their classes
            long fcpl_class = hdf5_h.H5Pget_class(fcpl);
            long dcpl_class = hdf5_h.H5Pget_class(dcpl);

            // Verify correct classes
            int fcpl_equal = hdf5_h.H5Pequal(fcpl_class, hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("FCPL class should match FILE_CREATE", fcpl_equal > 0);

            int dcpl_equal = hdf5_h.H5Pequal(dcpl_class, hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("DCPL class should match DATASET_CREATE", dcpl_equal > 0);

            // Verify they're different classes
            int different = hdf5_h.H5Pequal(fcpl_class, dcpl_class);
            assertEquals("FILE_CREATE and DATASET_CREATE should be different classes", 0, different);

            hdf5_h.H5Pclose(dcpl);
            hdf5_h.H5Pclose(fcpl);
        }
    }

    // =========================
    // File Creation Property Tests
    // =========================

    @Test
    public void testH5Pset_userblock()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("H5Pcreate fcpl failed", isValidId(fcpl));

            // Set user block size (must be power of 2 >= 512)
            long userblock_size = 1024;
            int result          = hdf5_h.H5Pset_userblock(fcpl, userblock_size);
            assertTrue("H5Pset_userblock failed", isSuccess(result));

            // Get user block size back
            MemorySegment sizeSeg = arena.allocate(ValueLayout.JAVA_LONG);
            result                = hdf5_h.H5Pget_userblock(fcpl, sizeSeg);
            assertTrue("H5Pget_userblock failed", isSuccess(result));

            long retrieved = getLong(sizeSeg);
            assertEquals("User block size should match", userblock_size, retrieved);

            hdf5_h.H5Pclose(fcpl);
        }
    }

    @Test
    public void testH5Pset_sizes()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("H5Pcreate fcpl failed", isValidId(fcpl));

            // Set sizes (sizeof_addr=8, sizeof_size=8 for 64-bit addressing)
            long sizeof_addr = 8;
            long sizeof_size = 8;
            int result       = hdf5_h.H5Pset_sizes(fcpl, sizeof_addr, sizeof_size);
            assertTrue("H5Pset_sizes failed", isSuccess(result));

            // Get sizes back
            MemorySegment addrSeg = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment sizeSeg = arena.allocate(ValueLayout.JAVA_LONG);
            result                = hdf5_h.H5Pget_sizes(fcpl, addrSeg, sizeSeg);
            assertTrue("H5Pget_sizes failed", isSuccess(result));

            long addr_retrieved = getLong(addrSeg);
            long size_retrieved = getLong(sizeSeg);
            assertEquals("Address size should match", sizeof_addr, addr_retrieved);
            assertEquals("Size size should match", sizeof_size, size_retrieved);

            hdf5_h.H5Pclose(fcpl);
        }
    }

    @Test
    public void testH5Pset_sym_k()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("H5Pcreate fcpl failed", isValidId(fcpl));

            // Set symbol table parameters (ik=tree rank, lk=node size)
            int ik     = 32;
            int lk     = 16;
            int result = hdf5_h.H5Pset_sym_k(fcpl, ik, lk);
            assertTrue("H5Pset_sym_k failed", isSuccess(result));

            // Get parameters back
            MemorySegment ikSeg = arena.allocate(ValueLayout.JAVA_INT);
            MemorySegment lkSeg = arena.allocate(ValueLayout.JAVA_INT);
            result              = hdf5_h.H5Pget_sym_k(fcpl, ikSeg, lkSeg);
            assertTrue("H5Pget_sym_k failed", isSuccess(result));

            int ik_retrieved = getInt(ikSeg);
            int lk_retrieved = getInt(lkSeg);
            assertEquals("ik parameter should match", ik, ik_retrieved);
            assertEquals("lk parameter should match", lk, lk_retrieved);

            hdf5_h.H5Pclose(fcpl);
        }
    }

    @Test
    public void testH5Pset_istore_k()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("H5Pcreate fcpl failed", isValidId(fcpl));

            // Set indexed storage B-tree parameter
            int ik     = 64;
            int result = hdf5_h.H5Pset_istore_k(fcpl, ik);
            assertTrue("H5Pset_istore_k failed", isSuccess(result));

            // Get parameter back
            MemorySegment ikSeg = arena.allocate(ValueLayout.JAVA_INT);
            result              = hdf5_h.H5Pget_istore_k(fcpl, ikSeg);
            assertTrue("H5Pget_istore_k failed", isSuccess(result));

            int ik_retrieved = getInt(ikSeg);
            assertEquals("istore_k parameter should match", ik, ik_retrieved);

            hdf5_h.H5Pclose(fcpl);
        }
    }

    @Test
    public void testH5Pset_shared_mesg_nindexes()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("H5Pcreate fcpl failed", isValidId(fcpl));

            // Set number of shared object header message indexes
            int nindexes = 3;
            int result   = hdf5_h.H5Pset_shared_mesg_nindexes(fcpl, nindexes);
            assertTrue("H5Pset_shared_mesg_nindexes failed", isSuccess(result));

            // Get number back
            MemorySegment nindexSeg = arena.allocate(ValueLayout.JAVA_INT);
            result                  = hdf5_h.H5Pget_shared_mesg_nindexes(fcpl, nindexSeg);
            assertTrue("H5Pget_shared_mesg_nindexes failed", isSuccess(result));

            int nindexes_retrieved = getInt(nindexSeg);
            assertEquals("Number of indexes should match", nindexes, nindexes_retrieved);

            hdf5_h.H5Pclose(fcpl);
        }
    }

    // =========================
    // File Access Property Tests
    // =========================

    @Test
    public void testH5Pset_fclose_degree()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set file close degree to STRONG (close all objects when file closes)
            int degree = hdf5_h.H5F_CLOSE_STRONG();
            int result = hdf5_h.H5Pset_fclose_degree(fapl, degree);
            assertTrue("H5Pset_fclose_degree failed", isSuccess(result));

            // Get degree back
            MemorySegment degreeSeg = arena.allocate(ValueLayout.JAVA_INT);
            result                  = hdf5_h.H5Pget_fclose_degree(fapl, degreeSeg);
            assertTrue("H5Pget_fclose_degree failed", isSuccess(result));

            int degree_retrieved = getInt(degreeSeg);
            assertEquals("File close degree should match", degree, degree_retrieved);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_alignment()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set alignment (threshold=1024, alignment=512)
            long threshold = 1024;
            long alignment = 512;
            int result     = hdf5_h.H5Pset_alignment(fapl, threshold, alignment);
            assertTrue("H5Pset_alignment failed", isSuccess(result));

            // Get alignment back
            MemorySegment threshSeg = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment alignSeg  = arena.allocate(ValueLayout.JAVA_LONG);
            result                  = hdf5_h.H5Pget_alignment(fapl, threshSeg, alignSeg);
            assertTrue("H5Pget_alignment failed", isSuccess(result));

            long threshold_retrieved = getLong(threshSeg);
            long alignment_retrieved = getLong(alignSeg);
            assertEquals("Threshold should match", threshold, threshold_retrieved);
            assertEquals("Alignment should match", alignment, alignment_retrieved);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_cache()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set cache parameters
            int mdc_nelmts   = 0; // Not used, set to 0
            long rdcc_nslots = 521;
            long rdcc_nbytes = 1048576;
            double rdcc_w0   = 0.75;
            int result       = hdf5_h.H5Pset_cache(fapl, mdc_nelmts, rdcc_nslots, rdcc_nbytes, rdcc_w0);
            assertTrue("H5Pset_cache failed", isSuccess(result));

            // Get cache parameters back
            MemorySegment mdcSeg   = arena.allocate(ValueLayout.JAVA_INT);
            MemorySegment nslotSeg = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment nbyteSeg = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment w0Seg    = arena.allocate(ValueLayout.JAVA_DOUBLE);
            result                 = hdf5_h.H5Pget_cache(fapl, mdcSeg, nslotSeg, nbyteSeg, w0Seg);
            assertTrue("H5Pget_cache failed", isSuccess(result));

            long nslots_retrieved = getLong(nslotSeg);
            long nbytes_retrieved = getLong(nbyteSeg);
            double w0_retrieved   = getDouble(w0Seg);
            assertEquals("rdcc_nslots should match", rdcc_nslots, nslots_retrieved);
            assertEquals("rdcc_nbytes should match", rdcc_nbytes, nbytes_retrieved);
            assertEquals("rdcc_w0 should match", rdcc_w0, w0_retrieved, 0.001);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_sieve_buf_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set data sieve buffer size
            long size  = 262144; // 256KB
            int result = hdf5_h.H5Pset_sieve_buf_size(fapl, size);
            assertTrue("H5Pset_sieve_buf_size failed", isSuccess(result));

            // Get size back
            MemorySegment sizeSeg = arena.allocate(ValueLayout.JAVA_LONG);
            result                = hdf5_h.H5Pget_sieve_buf_size(fapl, sizeSeg);
            assertTrue("H5Pget_sieve_buf_size failed", isSuccess(result));

            long size_retrieved = getLong(sizeSeg);
            assertEquals("Sieve buffer size should match", size, size_retrieved);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_meta_block_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set metadata block size
            long size  = 8192;
            int result = hdf5_h.H5Pset_meta_block_size(fapl, size);
            assertTrue("H5Pset_meta_block_size failed", isSuccess(result));

            // Get size back
            MemorySegment sizeSeg = arena.allocate(ValueLayout.JAVA_LONG);
            result                = hdf5_h.H5Pget_meta_block_size(fapl, sizeSeg);
            assertTrue("H5Pget_meta_block_size failed", isSuccess(result));

            long size_retrieved = getLong(sizeSeg);
            assertEquals("Meta block size should match", size, size_retrieved);

            hdf5_h.H5Pclose(fapl);
        }
    }

    // ================================================================================
    // Phase 6B - Dataset Creation Properties
    // ================================================================================

    @Test
    public void testH5Pset_chunk()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Set chunk dimensions: 10x20
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);

            int result = hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Get chunk dimensions back
            MemorySegment outChunkSegment = allocateLongArray(arena, 2);
            int ndims                     = hdf5_h.H5Pget_chunk(dcpl, 2, outChunkSegment);
            assertEquals("Should have 2 dimensions", 2, ndims);

            long[] retrieved = new long[2];
            copyFromSegment(outChunkSegment, retrieved);
            assertArrayEquals("Chunk dimensions should match", chunkDims, retrieved);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_layout()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Set layout to compact
            int result = hdf5_h.H5Pset_layout(dcpl, hdf5_h.H5D_COMPACT());
            assertTrue("H5Pset_layout failed", isSuccess(result));

            // Get layout back
            int layout = hdf5_h.H5Pget_layout(dcpl);
            assertEquals("Layout should be H5D_COMPACT", hdf5_h.H5D_COMPACT(), layout);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_fill_value()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Set fill value to 42
            int fillValue             = 42;
            MemorySegment fillSegment = allocateInt(arena);
            setInt(fillSegment, fillValue);

            int result = hdf5_h.H5Pset_fill_value(dcpl, hdf5_h_1.H5T_NATIVE_INT_g(), fillSegment);
            assertTrue("H5Pset_fill_value failed", isSuccess(result));

            // Get fill value back
            MemorySegment outFillSegment = allocateInt(arena);
            result = hdf5_h.H5Pget_fill_value(dcpl, hdf5_h_1.H5T_NATIVE_INT_g(), outFillSegment);
            assertTrue("H5Pget_fill_value failed", isSuccess(result));

            int retrieved = getInt(outFillSegment);
            assertEquals("Fill value should match", fillValue, retrieved);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_fill_time()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Set fill time to ALLOC (fill on allocation)
            int result = hdf5_h.H5Pset_fill_time(dcpl, hdf5_h.H5D_FILL_TIME_ALLOC());
            assertTrue("H5Pset_fill_time failed", isSuccess(result));

            // Get fill time back
            MemorySegment fillTimeSeg = arena.allocate(ValueLayout.JAVA_INT);
            result                    = hdf5_h.H5Pget_fill_time(dcpl, fillTimeSeg);
            assertTrue("H5Pget_fill_time failed", isSuccess(result));

            int fillTime = getInt(fillTimeSeg);
            assertEquals("Fill time should be H5D_FILL_TIME_ALLOC", hdf5_h.H5D_FILL_TIME_ALLOC(), fillTime);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_alloc_time()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Set allocation time to EARLY (allocate on creation)
            int result = hdf5_h.H5Pset_alloc_time(dcpl, hdf5_h.H5D_ALLOC_TIME_EARLY());
            assertTrue("H5Pset_alloc_time failed", isSuccess(result));

            // Get allocation time back
            MemorySegment allocTimeSeg = arena.allocate(ValueLayout.JAVA_INT);
            result                     = hdf5_h.H5Pget_alloc_time(dcpl, allocTimeSeg);
            assertTrue("H5Pget_alloc_time failed", isSuccess(result));

            int allocTime = getInt(allocTimeSeg);
            assertEquals("Allocation time should be H5D_ALLOC_TIME_EARLY", hdf5_h.H5D_ALLOC_TIME_EARLY(),
                         allocTime);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    // ================================================================================
    // Phase 6C - Compression and Filters
    // ================================================================================

    @Test
    public void testH5Pset_deflate()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Must set chunk first for compression
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);
            int result = hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Set deflate compression (gzip) with level 6
            int compressionLevel = 6;
            result               = hdf5_h.H5Pset_deflate(dcpl, compressionLevel);
            assertTrue("H5Pset_deflate failed", isSuccess(result));

            // Get number of filters
            int nfilters = hdf5_h.H5Pget_nfilters(dcpl);
            assertEquals("Should have 1 filter", 1, nfilters);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pget_nfilters()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Initially no filters
            int nfilters = hdf5_h.H5Pget_nfilters(dcpl);
            assertEquals("Should have 0 filters initially", 0, nfilters);

            // Add chunk (required for filters)
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);

            // Add deflate filter
            hdf5_h.H5Pset_deflate(dcpl, 6);

            // Now should have 1 filter
            nfilters = hdf5_h.H5Pget_nfilters(dcpl);
            assertEquals("Should have 1 filter after adding deflate", 1, nfilters);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pall_filters_avail()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Add chunk (required for filters)
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);

            // Add deflate filter (should be available in standard builds)
            hdf5_h.H5Pset_deflate(dcpl, 6);

            // Check if all filters are available
            int avail = hdf5_h.H5Pall_filters_avail(dcpl);
            // Note: Result depends on HDF5 build configuration
            // Just verify the function works (returns 0 or 1)
            assertTrue("H5Pall_filters_avail should return valid result", avail == 0 || avail > 0);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_shuffle()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Add chunk (required for filters)
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);

            // Set shuffle filter (improves compression)
            int result = hdf5_h.H5Pset_shuffle(dcpl);
            assertTrue("H5Pset_shuffle failed", isSuccess(result));

            // Verify filter was added
            int nfilters = hdf5_h.H5Pget_nfilters(dcpl);
            assertEquals("Should have 1 filter", 1, nfilters);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_fletcher32()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Add chunk (required for filters)
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);

            // Set Fletcher32 checksum filter (error detection)
            int result = hdf5_h.H5Pset_fletcher32(dcpl);
            assertTrue("H5Pset_fletcher32 failed", isSuccess(result));

            // Verify filter was added
            int nfilters = hdf5_h.H5Pget_nfilters(dcpl);
            assertEquals("Should have 1 filter", 1, nfilters);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    // ================================================================================
    // Phase 6D - Data Transfer and Advanced Properties
    // ================================================================================

    // Note: H5Pget_filter might not be available in FFM bindings yet
    // Skipping this test until API is available
    /*
    @Test
    public void testH5Pget_filter()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Must set chunk first
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);
            int result = hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Add deflate filter
            int compressionLevel = 6;
            result               = hdf5_h.H5Pset_deflate(dcpl, compressionLevel);
            assertTrue("H5Pset_deflate failed", isSuccess(result));

            // Get filter information
            MemorySegment flags  = allocateIntArray(arena, 1);
            MemorySegment cdNelts = allocateLongArray(arena, 1);
            MemorySegment cdValues = allocateIntArray(arena, 10); // Space for filter params
            MemorySegment nameSegment = arena.allocate(256);
            MemorySegment filterConfig = allocateIntArray(arena, 1);

            // Set initial cd_nelmts to max size
            copyToSegment(cdNelts, new long[]{10});

            int filterId = hdf5_h.H5Pget_filter(dcpl, 0, flags, cdNelts, cdValues,
                                                256, nameSegment, filterConfig);
            assertTrue("H5Pget_filter should return valid filter ID", filterId >= 0);

            // Verify it's the deflate filter
            assertEquals("Should be deflate filter", hdf5_h.H5Z_FILTER_DEFLATE(), filterId);

            hdf5_h.H5Pclose(dcpl);
        }
    }
    */

    @Test
    public void testH5Premove_filter()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Must set chunk first
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);
            int result = hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);
            assertTrue("H5Pset_chunk failed", isSuccess(result));

            // Add deflate filter
            result = hdf5_h.H5Pset_deflate(dcpl, 6);
            assertTrue("H5Pset_deflate failed", isSuccess(result));

            // Verify filter was added
            int nfilters = hdf5_h.H5Pget_nfilters(dcpl);
            assertEquals("Should have 1 filter", 1, nfilters);

            // Remove the deflate filter
            result = hdf5_h.H5Premove_filter(dcpl, hdf5_h.H5Z_FILTER_DEFLATE());
            assertTrue("H5Premove_filter failed", isSuccess(result));

            // Verify filter was removed
            nfilters = hdf5_h.H5Pget_nfilters(dcpl);
            assertEquals("Should have 0 filters after removal", 0, nfilters);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_chunk_cache()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_ACCESS_ID_g());
            assertTrue("H5Pcreate dapl failed", isValidId(dapl));

            // Set chunk cache parameters
            long rdccNslots = 521;     // Number of chunk slots in cache
            long rdccNbytes = 1048576; // Size of chunk cache in bytes (1 MB)
            double rdccW0   = 0.75;    // Preemption policy

            int result = hdf5_h.H5Pset_chunk_cache(dapl, rdccNslots, rdccNbytes, rdccW0);
            assertTrue("H5Pset_chunk_cache failed", isSuccess(result));

            // Get chunk cache parameters back
            MemorySegment outNslots = allocateLongArray(arena, 1);
            MemorySegment outNbytes = allocateLongArray(arena, 1);
            MemorySegment outW0     = allocateDoubleArray(arena, 1);

            result = hdf5_h.H5Pget_chunk_cache(dapl, outNslots, outNbytes, outW0);
            assertTrue("H5Pget_chunk_cache failed", isSuccess(result));

            // Verify values
            assertEquals("Nslots should match", rdccNslots, getLong(outNslots));
            assertEquals("Nbytes should match", rdccNbytes, getLong(outNbytes));
            assertEquals("W0 should match", rdccW0, getDouble(outW0), 0.01);

            hdf5_h.H5Pclose(dapl);
        }
    }

    @Test
    public void testH5Pset_hyper_vector_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Set hyperslab vector size
            long vectorSize = 1024;
            int result      = hdf5_h.H5Pset_hyper_vector_size(dxpl, vectorSize);
            assertTrue("H5Pset_hyper_vector_size failed", isSuccess(result));

            // Get vector size back
            MemorySegment outSize = allocateLongArray(arena, 1);
            result                = hdf5_h.H5Pget_hyper_vector_size(dxpl, outSize);
            assertTrue("H5Pget_hyper_vector_size failed", isSuccess(result));

            assertEquals("Vector size should match", vectorSize, getLong(outSize));

            hdf5_h.H5Pclose(dxpl);
        }
    }

    @Test
    public void testH5Pset_btree_ratios()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Set B-tree split ratios
            double left   = 0.1;
            double middle = 0.5;
            double right  = 0.9;

            int result = hdf5_h.H5Pset_btree_ratios(dxpl, left, middle, right);
            assertTrue("H5Pset_btree_ratios failed", isSuccess(result));

            // Get ratios back
            MemorySegment outLeft   = allocateDoubleArray(arena, 1);
            MemorySegment outMiddle = allocateDoubleArray(arena, 1);
            MemorySegment outRight  = allocateDoubleArray(arena, 1);

            result = hdf5_h.H5Pget_btree_ratios(dxpl, outLeft, outMiddle, outRight);
            assertTrue("H5Pget_btree_ratios failed", isSuccess(result));

            assertEquals("Left ratio should match", left, getDouble(outLeft), 0.01);
            assertEquals("Middle ratio should match", middle, getDouble(outMiddle), 0.01);
            assertEquals("Right ratio should match", right, getDouble(outRight), 0.01);

            hdf5_h.H5Pclose(dxpl);
        }
    }

    @Test
    public void testH5Pset_edc_check()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Enable error detection (EDC)
            int result = hdf5_h.H5Pset_edc_check(dxpl, hdf5_h.H5Z_ENABLE_EDC());
            assertTrue("H5Pset_edc_check failed", isSuccess(result));

            // Get EDC check setting
            int edcCheck = hdf5_h.H5Pget_edc_check(dxpl);
            assertEquals("EDC check should be enabled", hdf5_h.H5Z_ENABLE_EDC(), edcCheck);

            // Disable error detection
            result = hdf5_h.H5Pset_edc_check(dxpl, hdf5_h.H5Z_DISABLE_EDC());
            assertTrue("H5Pset_edc_check (disable) failed", isSuccess(result));

            edcCheck = hdf5_h.H5Pget_edc_check(dxpl);
            assertEquals("EDC check should be disabled", hdf5_h.H5Z_DISABLE_EDC(), edcCheck);

            hdf5_h.H5Pclose(dxpl);
        }
    }

    @Test
    public void testH5Pset_buffer()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Set type conversion buffer size (1 MB)
            long bufferSize = 1048576;
            int result      = hdf5_h.H5Pset_buffer(dxpl, bufferSize, MemorySegment.NULL, MemorySegment.NULL);
            assertTrue("H5Pset_buffer failed", isSuccess(result));

            // Get buffer size back
            long retrievedSize = hdf5_h.H5Pget_buffer(dxpl, MemorySegment.NULL, MemorySegment.NULL);
            assertEquals("Buffer size should match", bufferSize, retrievedSize);

            hdf5_h.H5Pclose(dxpl);
        }
    }

    @Test
    public void testH5Pset_libver_bounds()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set library version bounds to latest
            int result =
                hdf5_h.H5Pset_libver_bounds(fapl, hdf5_h.H5F_LIBVER_LATEST(), hdf5_h.H5F_LIBVER_LATEST());
            assertTrue("H5Pset_libver_bounds failed", isSuccess(result));

            // Get library version bounds back
            MemorySegment low  = allocateIntArray(arena, 1);
            MemorySegment high = allocateIntArray(arena, 1);

            result = hdf5_h.H5Pget_libver_bounds(fapl, low, high);
            assertTrue("H5Pget_libver_bounds failed", isSuccess(result));

            assertEquals("Low bound should be latest", hdf5_h.H5F_LIBVER_LATEST(), getInt(low));
            assertEquals("High bound should be latest", hdf5_h.H5F_LIBVER_LATEST(), getInt(high));

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_small_data_block_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set small data block size (2048 bytes)
            long blockSize = 2048;
            int result     = hdf5_h.H5Pset_small_data_block_size(fapl, blockSize);
            assertTrue("H5Pset_small_data_block_size failed", isSuccess(result));

            // Get block size back
            MemorySegment outSize = allocateLongArray(arena, 1);
            result                = hdf5_h.H5Pget_small_data_block_size(fapl, outSize);
            assertTrue("H5Pget_small_data_block_size failed", isSuccess(result));

            assertEquals("Block size should match", blockSize, getLong(outSize));

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_gc_references()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Enable garbage collection for references
            int result = hdf5_h.H5Pset_gc_references(fapl, 1);
            assertTrue("H5Pset_gc_references failed", isSuccess(result));

            // Get GC references setting
            MemorySegment gcRefs = allocateIntArray(arena, 1);
            result               = hdf5_h.H5Pget_gc_references(fapl, gcRefs);
            assertTrue("H5Pget_gc_references failed", isSuccess(result));

            assertEquals("GC references should be enabled", 1, getInt(gcRefs));

            // Disable garbage collection
            result = hdf5_h.H5Pset_gc_references(fapl, 0);
            assertTrue("H5Pset_gc_references (disable) failed", isSuccess(result));

            result = hdf5_h.H5Pget_gc_references(fapl, gcRefs);
            assertTrue("H5Pget_gc_references (after disable) failed", isSuccess(result));

            assertEquals("GC references should be disabled", 0, getInt(gcRefs));

            hdf5_h.H5Pclose(fapl);
        }
    }

    // ================================================================================
    // Phase 6E - Link, Attribute, and Advanced Properties
    // ================================================================================

    @Test
    public void testH5Pset_create_intermediate_group()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long lcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_LINK_CREATE_ID_g());
            assertTrue("H5Pcreate lcpl failed", isValidId(lcpl));

            // Enable intermediate group creation
            int result = hdf5_h.H5Pset_create_intermediate_group(lcpl, 1);
            assertTrue("H5Pset_create_intermediate_group failed", isSuccess(result));

            // Get setting back
            MemorySegment crtIntmd = allocateIntArray(arena, 1);
            result                 = hdf5_h.H5Pget_create_intermediate_group(lcpl, crtIntmd);
            assertTrue("H5Pget_create_intermediate_group failed", isSuccess(result));

            assertEquals("Create intermediate should be enabled", 1, getInt(crtIntmd));

            hdf5_h.H5Pclose(lcpl);
        }
    }

    @Test
    public void testH5Pset_char_encoding()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long lcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_LINK_CREATE_ID_g());
            assertTrue("H5Pcreate lcpl failed", isValidId(lcpl));

            // Set character encoding to UTF-8
            int result = hdf5_h.H5Pset_char_encoding(lcpl, hdf5_h.H5T_CSET_UTF8());
            assertTrue("H5Pset_char_encoding failed", isSuccess(result));

            // Get encoding back
            MemorySegment encoding = allocateIntArray(arena, 1);
            result                 = hdf5_h.H5Pget_char_encoding(lcpl, encoding);
            assertTrue("H5Pget_char_encoding failed", isSuccess(result));

            assertEquals("Encoding should be UTF-8", hdf5_h.H5T_CSET_UTF8(), getInt(encoding));

            hdf5_h.H5Pclose(lcpl);
        }
    }

    @Test
    public void testH5Pset_link_creation_order()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long gcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_GROUP_CREATE_ID_g());
            assertTrue("H5Pcreate gcpl failed", isValidId(gcpl));

            // Set link creation order tracking and indexing
            int crtOrderFlags = hdf5_h.H5P_CRT_ORDER_TRACKED() | hdf5_h.H5P_CRT_ORDER_INDEXED();
            int result        = hdf5_h.H5Pset_link_creation_order(gcpl, crtOrderFlags);
            assertTrue("H5Pset_link_creation_order failed", isSuccess(result));

            // Get flags back
            MemorySegment flags = allocateIntArray(arena, 1);
            result              = hdf5_h.H5Pget_link_creation_order(gcpl, flags);
            assertTrue("H5Pget_link_creation_order failed", isSuccess(result));

            assertEquals("Link creation order flags should match", crtOrderFlags, getInt(flags));

            hdf5_h.H5Pclose(gcpl);
        }
    }

    @Test
    public void testH5Pset_attr_creation_order()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long ocpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_CREATE_ID_g());
            assertTrue("H5Pcreate ocpl failed", isValidId(ocpl));

            // Set attribute creation order tracking and indexing
            int crtOrderFlags = hdf5_h.H5P_CRT_ORDER_TRACKED() | hdf5_h.H5P_CRT_ORDER_INDEXED();
            int result        = hdf5_h.H5Pset_attr_creation_order(ocpl, crtOrderFlags);
            assertTrue("H5Pset_attr_creation_order failed", isSuccess(result));

            // Get flags back
            MemorySegment flags = allocateIntArray(arena, 1);
            result              = hdf5_h.H5Pget_attr_creation_order(ocpl, flags);
            assertTrue("H5Pget_attr_creation_order failed", isSuccess(result));

            assertEquals("Attr creation order flags should match", crtOrderFlags, getInt(flags));

            hdf5_h.H5Pclose(ocpl);
        }
    }

    @Test
    public void testH5Pset_link_phase_change()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long gcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_GROUP_CREATE_ID_g());
            assertTrue("H5Pcreate gcpl failed", isValidId(gcpl));

            // Set link phase change thresholds
            int maxCompact = 8;  // Max links in compact storage
            int minDense   = 6;  // Min links for dense storage
            int result     = hdf5_h.H5Pset_link_phase_change(gcpl, maxCompact, minDense);
            assertTrue("H5Pset_link_phase_change failed", isSuccess(result));

            // Get thresholds back
            MemorySegment outMaxCompact = allocateIntArray(arena, 1);
            MemorySegment outMinDense   = allocateIntArray(arena, 1);
            result = hdf5_h.H5Pget_link_phase_change(gcpl, outMaxCompact, outMinDense);
            assertTrue("H5Pget_link_phase_change failed", isSuccess(result));

            assertEquals("Max compact should match", maxCompact, getInt(outMaxCompact));
            assertEquals("Min dense should match", minDense, getInt(outMinDense));

            hdf5_h.H5Pclose(gcpl);
        }
    }

    @Test
    public void testH5Pset_attr_phase_change()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long ocpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_CREATE_ID_g());
            assertTrue("H5Pcreate ocpl failed", isValidId(ocpl));

            // Set attribute phase change thresholds
            int maxCompact = 8;
            int minDense   = 6;
            int result     = hdf5_h.H5Pset_attr_phase_change(ocpl, maxCompact, minDense);
            assertTrue("H5Pset_attr_phase_change failed", isSuccess(result));

            // Get thresholds back
            MemorySegment outMaxCompact = allocateIntArray(arena, 1);
            MemorySegment outMinDense   = allocateIntArray(arena, 1);
            result = hdf5_h.H5Pget_attr_phase_change(ocpl, outMaxCompact, outMinDense);
            assertTrue("H5Pget_attr_phase_change failed", isSuccess(result));

            assertEquals("Max compact should match", maxCompact, getInt(outMaxCompact));
            assertEquals("Min dense should match", minDense, getInt(outMinDense));

            hdf5_h.H5Pclose(ocpl);
        }
    }

    @Test
    public void testH5Pset_nlinks()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long lapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_LINK_ACCESS_ID_g());
            assertTrue("H5Pcreate lapl failed", isValidId(lapl));

            // Set maximum number of soft/external link traversals
            long nlinks = 100;
            int result  = hdf5_h.H5Pset_nlinks(lapl, nlinks);
            assertTrue("H5Pset_nlinks failed", isSuccess(result));

            // Get nlinks back
            MemorySegment outNlinks = allocateLongArray(arena, 1);
            result                  = hdf5_h.H5Pget_nlinks(lapl, outNlinks);
            assertTrue("H5Pget_nlinks failed", isSuccess(result));

            assertEquals("Nlinks should match", nlinks, getLong(outNlinks));

            hdf5_h.H5Pclose(lapl);
        }
    }

    @Test
    public void testH5Pset_elink_prefix()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long lapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_LINK_ACCESS_ID_g());
            assertTrue("H5Pcreate lapl failed", isValidId(lapl));

            // Set external link prefix
            String prefix           = "/tmp/data";
            MemorySegment prefixSeg = stringToSegment(arena, prefix);
            int result              = hdf5_h.H5Pset_elink_prefix(lapl, prefixSeg);
            assertTrue("H5Pset_elink_prefix failed", isSuccess(result));

            // Get prefix back
            long size               = hdf5_h.H5Pget_elink_prefix(lapl, MemorySegment.NULL, 0);
            MemorySegment outPrefix = arena.allocate(size + 1);
            hdf5_h.H5Pget_elink_prefix(lapl, outPrefix, size + 1);

            String retrieved = segmentToString(outPrefix);
            assertEquals("Prefix should match", prefix, retrieved);

            hdf5_h.H5Pclose(lapl);
        }
    }

    @Test
    public void testH5Pset_efile_prefix()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_ACCESS_ID_g());
            assertTrue("H5Pcreate dapl failed", isValidId(dapl));

            // Set external file prefix
            String prefix           = "/tmp/external";
            MemorySegment prefixSeg = stringToSegment(arena, prefix);
            int result              = hdf5_h.H5Pset_efile_prefix(dapl, prefixSeg);
            assertTrue("H5Pset_efile_prefix failed", isSuccess(result));

            // Get prefix back
            long size               = hdf5_h.H5Pget_efile_prefix(dapl, MemorySegment.NULL, 0);
            MemorySegment outPrefix = arena.allocate(size + 1);
            hdf5_h.H5Pget_efile_prefix(dapl, outPrefix, size + 1);

            String retrieved = segmentToString(outPrefix);
            assertEquals("Prefix should match", prefix, retrieved);

            hdf5_h.H5Pclose(dapl);
        }
    }

    @Test
    public void testH5Pset_chunk_opts()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Must set chunking first
            long[] chunkDims               = {10, 20};
            MemorySegment chunkDimsSegment = allocateLongArray(arena, 2);
            copyToSegment(chunkDimsSegment, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkDimsSegment);

            // Set chunk optimization options (don't filter partial edge chunks)
            int opts   = hdf5_h.H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS();
            int result = hdf5_h.H5Pset_chunk_opts(dcpl, opts);
            assertTrue("H5Pset_chunk_opts failed", isSuccess(result));

            // Get options back
            MemorySegment outOpts = allocateIntArray(arena, 1);
            result                = hdf5_h.H5Pget_chunk_opts(dcpl, outOpts);
            assertTrue("H5Pget_chunk_opts failed", isSuccess(result));

            assertEquals("Chunk opts should match", opts, getInt(outOpts));

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_file_space_strategy()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("H5Pcreate fcpl failed", isValidId(fcpl));

            // Set file space strategy (aggregation strategy)
            int strategy    = hdf5_h.H5F_FSPACE_STRATEGY_FSM_AGGR(); // Free-space manager with aggregation
            boolean persist = true;                                   // Persist free-space
            long threshold  = 1;                                      // Threshold
            int result      = hdf5_h.H5Pset_file_space_strategy(fcpl, strategy, persist, threshold);
            assertTrue("H5Pset_file_space_strategy failed", isSuccess(result));

            // Get strategy back
            MemorySegment outStrategy  = allocateIntArray(arena, 1);
            MemorySegment outPersist   = allocateIntArray(arena, 1);
            MemorySegment outThreshold = allocateLongArray(arena, 1);
            result = hdf5_h.H5Pget_file_space_strategy(fcpl, outStrategy, outPersist, outThreshold);
            assertTrue("H5Pget_file_space_strategy failed", isSuccess(result));

            assertEquals("Strategy should match", strategy, getInt(outStrategy));
            assertEquals("Persist should be true", 1, getInt(outPersist)); // true = 1
            assertEquals("Threshold should match", threshold, getLong(outThreshold));

            hdf5_h.H5Pclose(fcpl);
        }
    }

    @Test
    public void testH5Pset_file_space_page_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("H5Pcreate fcpl failed", isValidId(fcpl));

            // Set file space page size (4KB)
            long pageSize = 4096;
            int result    = hdf5_h.H5Pset_file_space_page_size(fcpl, pageSize);
            assertTrue("H5Pset_file_space_page_size failed", isSuccess(result));

            // Get page size back
            MemorySegment outPageSize = allocateLongArray(arena, 1);
            result                    = hdf5_h.H5Pget_file_space_page_size(fcpl, outPageSize);
            assertTrue("H5Pget_file_space_page_size failed", isSuccess(result));

            assertEquals("Page size should match", pageSize, getLong(outPageSize));

            hdf5_h.H5Pclose(fcpl);
        }
    }

    @Test
    public void testH5Pset_local_heap_size_hint()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long gcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_GROUP_CREATE_ID_g());
            assertTrue("H5Pcreate gcpl failed", isValidId(gcpl));

            // Set local heap size hint (1KB)
            long sizeHint = 1024;
            int result    = hdf5_h.H5Pset_local_heap_size_hint(gcpl, sizeHint);
            assertTrue("H5Pset_local_heap_size_hint failed", isSuccess(result));

            // Get size hint back
            MemorySegment outSizeHint = allocateLongArray(arena, 1);
            result                    = hdf5_h.H5Pget_local_heap_size_hint(gcpl, outSizeHint);
            assertTrue("H5Pget_local_heap_size_hint failed", isSuccess(result));

            assertEquals("Size hint should match", sizeHint, getLong(outSizeHint));

            hdf5_h.H5Pclose(gcpl);
        }
    }

    @Test
    public void testH5Pset_est_link_info()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long gcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_GROUP_CREATE_ID_g());
            assertTrue("H5Pcreate gcpl failed", isValidId(gcpl));

            // Set estimated link info
            int estNumEntries  = 100;  // Estimated number of links
            int estNameLen     = 20;   // Estimated link name length
            int result         = hdf5_h.H5Pset_est_link_info(gcpl, estNumEntries, estNameLen);
            assertTrue("H5Pset_est_link_info failed", isSuccess(result));

            // Get estimates back
            MemorySegment outNumEntries = allocateIntArray(arena, 1);
            MemorySegment outNameLen    = allocateIntArray(arena, 1);
            result = hdf5_h.H5Pget_est_link_info(gcpl, outNumEntries, outNameLen);
            assertTrue("H5Pget_est_link_info failed", isSuccess(result));

            assertEquals("Num entries should match", estNumEntries, getInt(outNumEntries));
            assertEquals("Name length should match", estNameLen, getInt(outNameLen));

            hdf5_h.H5Pclose(gcpl);
        }
    }

    @Test
    public void testH5Pset_shared_mesg_index()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_CREATE_ID_g());
            assertTrue("H5Pcreate fcpl failed", isValidId(fcpl));

            // First set number of indexes
            hdf5_h.H5Pset_shared_mesg_nindexes(fcpl, 2);

            // Set shared message index (index 0, dataspace + datatype messages, min size 100)
            int indexNum  = 0;
            int mesgTypes = hdf5_h.H5O_SHMESG_SDSPACE_FLAG() | hdf5_h.H5O_SHMESG_DTYPE_FLAG();
            int minSize   = 100;
            int result    = hdf5_h.H5Pset_shared_mesg_index(fcpl, indexNum, mesgTypes, minSize);
            assertTrue("H5Pset_shared_mesg_index failed", isSuccess(result));

            // Get index info back
            MemorySegment outMesgTypes = allocateIntArray(arena, 1);
            MemorySegment outMinSize   = allocateIntArray(arena, 1);
            result = hdf5_h.H5Pget_shared_mesg_index(fcpl, indexNum, outMesgTypes, outMinSize);
            assertTrue("H5Pget_shared_mesg_index failed", isSuccess(result));

            assertEquals("Message types should match", mesgTypes, getInt(outMesgTypes));
            assertEquals("Min size should match", minSize, getInt(outMinSize));

            hdf5_h.H5Pclose(fcpl);
        }
    }
}
