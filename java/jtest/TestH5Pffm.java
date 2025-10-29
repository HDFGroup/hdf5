package jtest;

import static org.junit.Assert.*;

import static jtest.FfmTestSupport.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.hdf5_h;
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

            int result = hdf5_h.H5Pset_fill_value(dcpl, hdf5_h.H5T_NATIVE_INT_g(), fillSegment);
            assertTrue("H5Pset_fill_value failed", isSuccess(result));

            // Get fill value back
            MemorySegment outFillSegment = allocateInt(arena);
            result = hdf5_h.H5Pget_fill_value(dcpl, hdf5_h.H5T_NATIVE_INT_g(), outFillSegment);
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
            long prefixSize = hdf5_h.H5Pget_efile_prefix(dapl, MemorySegment.NULL, 0);
            assertTrue("H5Pget_efile_prefix size query failed", prefixSize > 0);

            MemorySegment prefixBuf = arena.allocate(prefixSize + 1);
            result                  = (int)hdf5_h.H5Pget_efile_prefix(dapl, prefixBuf, prefixSize + 1);
            assertTrue("H5Pget_efile_prefix failed", result >= 0);

            String retrievedPrefix = segmentToString(prefixBuf);
            assertEquals("Prefix should match", prefix, retrievedPrefix);

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
            boolean persist = true;                                  // Persist free-space
            long threshold  = 1;                                     // Threshold
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

    // =========================
    // Additional Property List Tests for C API Coverage
    // =========================

    @Test
    public void testH5Pset_data_transform()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Set a data transform expression (multiply by 2)
            String transform           = "x*2";
            MemorySegment transformSeg = stringToSegment(arena, transform);
            int result                 = hdf5_h.H5Pset_data_transform(dxpl, transformSeg);
            assertTrue("H5Pset_data_transform failed", isSuccess(result));

            // Get size of transform expression
            long transformSize = hdf5_h.H5Pget_data_transform(dxpl, MemorySegment.NULL, 0);
            assertTrue("H5Pget_data_transform size query failed", transformSize > 0);

            // Get transform expression back
            MemorySegment outTransform = arena.allocate(transformSize + 1);
            long actualSize            = hdf5_h.H5Pget_data_transform(dxpl, outTransform, transformSize + 1);
            assertTrue("H5Pget_data_transform failed", actualSize > 0);

            String retrievedTransform = segmentToString(outTransform);
            assertEquals("Transform should match", transform, retrievedTransform);

            hdf5_h.H5Pclose(dxpl);
        }
    }

    @Test
    public void testH5Pset_copy_object()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long ocpypl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_COPY_ID_g());
            assertTrue("H5Pcreate ocpypl failed", isValidId(ocpypl));

            // Set copy object flags (shallow hierarchy copy, copy without attributes)
            int copyFlags = hdf5_h.H5O_COPY_SHALLOW_HIERARCHY_FLAG() | hdf5_h.H5O_COPY_WITHOUT_ATTR_FLAG();
            int result    = hdf5_h.H5Pset_copy_object(ocpypl, copyFlags);
            assertTrue("H5Pset_copy_object failed", isSuccess(result));

            // Get copy object flags back
            MemorySegment outFlags = allocateIntArray(arena, 1);
            result                 = hdf5_h.H5Pget_copy_object(ocpypl, outFlags);
            assertTrue("H5Pget_copy_object failed", isSuccess(result));

            assertEquals("Copy flags should match", copyFlags, getInt(outFlags));

            hdf5_h.H5Pclose(ocpypl);
        }
    }

    @Test
    public void testH5Pget_filter_by_id()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Set chunking (required for filters)
            long[] chunkDims       = {10, 20};
            MemorySegment chunkSeg = allocateLongArray(arena, 2);
            copyToSegment(chunkSeg, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkSeg);

            // Add deflate filter with compression level 6
            hdf5_h.H5Pset_deflate(dcpl, 6);

            // Query the deflate filter by ID
            int filterId            = hdf5_h.H5Z_FILTER_DEFLATE();
            MemorySegment flags     = allocateIntArray(arena, 1);
            MemorySegment nElements = allocateLongArray(arena, 1);
            nElements.set(ValueLayout.JAVA_LONG, 0, 8); // Max 8 cd_values
            MemorySegment cdValues     = allocateIntArray(arena, 8);
            long nameSize              = 256;
            MemorySegment filterName   = arena.allocate(nameSize);
            MemorySegment filterConfig = allocateIntArray(arena, 1);

            int result = hdf5_h.H5Pget_filter_by_id2(dcpl, filterId, flags, nElements, cdValues, nameSize,
                                                     filterName, filterConfig);
            assertTrue("H5Pget_filter_by_id2 failed", isSuccess(result));

            // Verify deflate was found
            long actualNElements = nElements.get(ValueLayout.JAVA_LONG, 0);
            assertTrue("Should have cd_values for deflate", actualNElements > 0);

            // First cd_value should be compression level (6)
            int compressionLevel = cdValues.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Compression level should be 6", 6, compressionLevel);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pget_chunk_cache()
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

            assertEquals("Nslots should match", rdccNslots, getLong(outNslots));
            assertEquals("Nbytes should match", rdccNbytes, getLong(outNbytes));
            assertEquals("W0 should match", rdccW0, getDouble(outW0), 0.001);

            hdf5_h.H5Pclose(dapl);
        }
    }

    @Test
    public void testH5Pmodify_filter()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Set chunking (required for filters)
            long[] chunkDims       = {10, 20};
            MemorySegment chunkSeg = allocateLongArray(arena, 2);
            copyToSegment(chunkSeg, chunkDims);
            hdf5_h.H5Pset_chunk(dcpl, 2, chunkSeg);

            // Add deflate filter with compression level 6
            hdf5_h.H5Pset_deflate(dcpl, 6);

            // Modify deflate filter to use compression level 9
            int filterId           = hdf5_h.H5Z_FILTER_DEFLATE();
            int flags              = 0; // Mandatory filter
            long nElements         = 1; // One cd_value (compression level)
            MemorySegment cdValues = allocateIntArray(arena, 1);
            cdValues.set(ValueLayout.JAVA_INT, 0, 9); // Level 9

            int result = hdf5_h.H5Pmodify_filter(dcpl, filterId, flags, nElements, cdValues);
            assertTrue("H5Pmodify_filter failed", isSuccess(result));

            // Verify the filter was modified
            MemorySegment outFlags     = allocateIntArray(arena, 1);
            MemorySegment outNElements = allocateLongArray(arena, 1);
            outNElements.set(ValueLayout.JAVA_LONG, 0, 8);
            MemorySegment outCdValues = allocateIntArray(arena, 8);
            MemorySegment outName     = arena.allocate(256);
            MemorySegment outConfig   = allocateIntArray(arena, 1);

            result = hdf5_h.H5Pget_filter_by_id2(dcpl, filterId, outFlags, outNElements, outCdValues, 256,
                                                 outName, outConfig);
            assertTrue("H5Pget_filter_by_id2 failed", isSuccess(result));

            // Verify compression level is now 9
            int compressionLevel = outCdValues.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Compression level should be 9 after modify", 9, compressionLevel);

            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_fapl_core()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set core (memory) VFD with 1MB increment and backing store enabled
            long increment       = 1024 * 1024; // 1MB increments
            boolean backingStore = true;        // Enable backing store
            int result           = hdf5_h.H5Pset_fapl_core(fapl, increment, backingStore);
            assertTrue("H5Pset_fapl_core failed", isSuccess(result));

            // Get core VFD settings
            MemorySegment incrementSeg    = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment backingStoreSeg = arena.allocate(ValueLayout.JAVA_BOOLEAN);
            result                        = hdf5_h.H5Pget_fapl_core(fapl, incrementSeg, backingStoreSeg);
            assertTrue("H5Pget_fapl_core failed", isSuccess(result));

            long retIncrement = incrementSeg.get(ValueLayout.JAVA_LONG, 0);
            assertEquals("Increment should match", increment, retIncrement);

            boolean retBackingStore = backingStoreSeg.get(ValueLayout.JAVA_BOOLEAN, 0);
            assertEquals("Backing store should match", backingStore, retBackingStore);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_fapl_log()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set log VFD with log file and flags
            String logFile = "test_h5pffm.log";
            long flags     = hdf5_h.H5FD_LOG_LOC_IO() | hdf5_h.H5FD_LOG_ALLOC(); // Log I/O and allocation
            long bufSize   = 4096;                                               // 4KB buffer

            int result = hdf5_h.H5Pset_fapl_log(fapl, stringToSegment(arena, logFile), flags, bufSize);
            assertTrue("H5Pset_fapl_log failed", isSuccess(result));

            // Note: H5Pget_fapl_log doesn't exist, just verify VFD was set
            long driverId = hdf5_h.H5Pget_driver(fapl);
            assertTrue("Driver ID should be valid", isValidId(driverId));

            hdf5_h.H5Pclose(fapl);

            // Clean up log file
            _deleteFile(logFile);
        }
    }

    @Test
    public void testH5Pset_fapl_sec2()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set sec2 (standard I/O) VFD
            int result = hdf5_h.H5Pset_fapl_sec2(fapl);
            assertTrue("H5Pset_fapl_sec2 failed", isSuccess(result));

            // Verify VFD was set
            long driverId = hdf5_h.H5Pget_driver(fapl);
            assertTrue("Driver ID should be valid", isValidId(driverId));

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_fapl_family()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Create member FAPL (use default)
            long memberFapl = hdf5_h.H5P_DEFAULT();

            // Set family VFD with 1MB member size
            long memberSize = 1024 * 1024; // 1MB per family member
            int result      = hdf5_h.H5Pset_fapl_family(fapl, memberSize, memberFapl);
            assertTrue("H5Pset_fapl_family failed", isSuccess(result));

            // Get family VFD settings
            MemorySegment membSizeSeg = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment membFaplSeg = arena.allocate(ValueLayout.JAVA_LONG);
            result                    = hdf5_h.H5Pget_fapl_family(fapl, membSizeSeg, membFaplSeg);
            assertTrue("H5Pget_fapl_family failed", isSuccess(result));

            long retMembSize = membSizeSeg.get(ValueLayout.JAVA_LONG, 0);
            assertEquals("Member size should match", memberSize, retMembSize);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pget_driver()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Get default driver (should be sec2)
            long driverId = hdf5_h.H5Pget_driver(fapl);
            assertTrue("Default driver ID should be valid", isValidId(driverId));

            // Set core VFD
            int result = hdf5_h.H5Pset_fapl_core(fapl, 1024, false);
            assertTrue("H5Pset_fapl_core failed", isSuccess(result));

            // Get driver again (should be core)
            long coreDriverId = hdf5_h.H5Pget_driver(fapl);
            assertTrue("Core driver ID should be valid", isValidId(coreDriverId));

            // Driver IDs should be different
            assertNotEquals("Driver IDs should differ after changing VFD", driverId, coreDriverId);

            hdf5_h.H5Pclose(fapl);
        }
    }

    // =========================
    // File Image + MDC Configuration Tests (Batch 2)
    // =========================

    @Test
    public void testH5Pset_file_image()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Create a small file image buffer (simulating an in-memory HDF5 file)
            long imageSize            = 1024; // 1KB
            MemorySegment imageBuffer = arena.allocate(imageSize);

            // Initialize buffer with some data
            for (long i = 0; i < imageSize; i++) {
                imageBuffer.set(ValueLayout.JAVA_BYTE, i, (byte)(i % 256));
            }

            // Set file image
            int result = hdf5_h.H5Pset_file_image(fapl, imageBuffer, imageSize);
            assertTrue("H5Pset_file_image failed", isSuccess(result));

            // Get file image back
            MemorySegment outBufferPtr = allocateLongArray(arena, 1);
            MemorySegment outSize      = allocateLongArray(arena, 1);
            result                     = hdf5_h.H5Pget_file_image(fapl, outBufferPtr, outSize);
            assertTrue("H5Pget_file_image failed", isSuccess(result));

            // Verify size matches
            long retrievedSize = getLong(outSize);
            assertEquals("File image size should match", imageSize, retrievedSize);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_mdc_log_options()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set MDC (metadata cache) log options
            boolean isEnabled     = true;
            String location       = "test_mdc.log";
            boolean startOnAccess = true;

            int result = hdf5_h.H5Pset_mdc_log_options(fapl, isEnabled, stringToSegment(arena, location),
                                                       startOnAccess);
            assertTrue("H5Pset_mdc_log_options failed", isSuccess(result));

            // Get MDC log options back
            MemorySegment outIsEnabled     = allocateIntArray(arena, 1);
            MemorySegment outLocation      = arena.allocate(256);
            MemorySegment outLocationSize  = allocateLongArray(arena, 1);
            MemorySegment outStartOnAccess = allocateIntArray(arena, 1);

            result = hdf5_h.H5Pget_mdc_log_options(fapl, outIsEnabled, outLocation, outLocationSize,
                                                   outStartOnAccess);
            assertTrue("H5Pget_mdc_log_options failed", isSuccess(result));

            // Verify settings
            boolean retrievedEnabled       = getInt(outIsEnabled) != 0;
            boolean retrievedStartOnAccess = getInt(outStartOnAccess) != 0;
            assertEquals("MDC logging should be enabled", isEnabled, retrievedEnabled);
            assertEquals("Start on access should match", startOnAccess, retrievedStartOnAccess);

            hdf5_h.H5Pclose(fapl);

            // Clean up log file if created
            _deleteFile(location);
        }
    }

    // =========================
    // DXPL Enhancement Tests (Batch 3)
    // =========================

    @Test
    public void testH5Pset_edc_check_disable()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Disable error detection (EDC)
            int result = hdf5_h.H5Pset_edc_check(dxpl, hdf5_h.H5Z_DISABLE_EDC());
            assertTrue("H5Pset_edc_check failed", isSuccess(result));

            // Get EDC check setting back
            int edcCheck = hdf5_h.H5Pget_edc_check(dxpl);
            assertEquals("EDC should be disabled", hdf5_h.H5Z_DISABLE_EDC(), edcCheck);

            hdf5_h.H5Pclose(dxpl);
        }
    }

    @Test
    public void testH5Pset_edc_check_enable()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Enable error detection (EDC)
            int result = hdf5_h.H5Pset_edc_check(dxpl, hdf5_h.H5Z_ENABLE_EDC());
            assertTrue("H5Pset_edc_check failed", isSuccess(result));

            // Get EDC check setting back
            int edcCheck = hdf5_h.H5Pget_edc_check(dxpl);
            assertEquals("EDC should be enabled", hdf5_h.H5Z_ENABLE_EDC(), edcCheck);

            hdf5_h.H5Pclose(dxpl);
        }
    }

    @Test
    public void testH5Pset_selection_io()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Enable selection I/O
            int result = hdf5_h.H5Pset_selection_io(dxpl, hdf5_h.H5D_SELECTION_IO_MODE_ON());
            assertTrue("H5Pset_selection_io failed", isSuccess(result));

            // Get selection I/O mode back
            MemorySegment outMode = allocateIntArray(arena, 1);
            result                = hdf5_h.H5Pget_selection_io(dxpl, outMode);
            assertTrue("H5Pget_selection_io failed", isSuccess(result));

            int mode = getInt(outMode);
            assertEquals("Selection I/O should be enabled", hdf5_h.H5D_SELECTION_IO_MODE_ON(), mode);

            hdf5_h.H5Pclose(dxpl);
        }
    }

    @Test
    public void testH5Pset_selection_io_off()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dxpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_XFER_ID_g());
            assertTrue("H5Pcreate dxpl failed", isValidId(dxpl));

            // Disable selection I/O
            int result = hdf5_h.H5Pset_selection_io(dxpl, hdf5_h.H5D_SELECTION_IO_MODE_OFF());
            assertTrue("H5Pset_selection_io failed", isSuccess(result));

            // Get selection I/O mode back
            MemorySegment outMode = allocateIntArray(arena, 1);
            result                = hdf5_h.H5Pget_selection_io(dxpl, outMode);
            assertTrue("H5Pget_selection_io failed", isSuccess(result));

            int mode = getInt(outMode);
            assertEquals("Selection I/O should be disabled", hdf5_h.H5D_SELECTION_IO_MODE_OFF(), mode);

            hdf5_h.H5Pclose(dxpl);
        }
    }

    // =========================
    // Virtual Dataset Property Tests
    // =========================

    @Test
    public void testH5Pset_virtual_basic()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create dataset creation property list
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Create virtual dataspace (10x20)
            long[] vdimsArray   = {10, 20};
            MemorySegment vdims = allocateLongArray(arena, 2);
            copyToSegment(vdims, vdimsArray);
            long vspace = hdf5_h.H5Screate_simple(2, vdims, MemorySegment.NULL);
            assertTrue("H5Screate_simple vspace failed", isValidId(vspace));

            // Create source dataspace (10x20)
            long[] sdimsArray   = {10, 20};
            MemorySegment sdims = allocateLongArray(arena, 2);
            copyToSegment(sdims, sdimsArray);
            long srcspace = hdf5_h.H5Screate_simple(2, sdims, MemorySegment.NULL);
            assertTrue("H5Screate_simple srcspace failed", isValidId(srcspace));

            // Set virtual mapping
            MemorySegment srcFile = stringToSegment(arena, "source.h5");
            MemorySegment srcDset = stringToSegment(arena, "/source_dataset");
            int result            = hdf5_h.H5Pset_virtual(dcpl, vspace, srcFile, srcDset, srcspace);
            assertTrue("H5Pset_virtual failed", isSuccess(result));

            // Get virtual count
            MemorySegment count = allocateLongArray(arena, 1);
            result              = hdf5_h.H5Pget_virtual_count(dcpl, count);
            assertTrue("H5Pget_virtual_count failed", isSuccess(result));
            assertEquals("Should have 1 virtual mapping", 1L, getLong(count));

            // Cleanup
            hdf5_h.H5Sclose(srcspace);
            hdf5_h.H5Sclose(vspace);
            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pget_virtual_filename()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Create dataspaces
            long[] dimsArray   = {100};
            MemorySegment dims = allocateLongArray(arena, 1);
            copyToSegment(dims, dimsArray);
            long vspace   = hdf5_h.H5Screate_simple(1, dims, MemorySegment.NULL);
            long srcspace = hdf5_h.H5Screate_simple(1, dims, MemorySegment.NULL);

            // Set virtual mapping with specific filename
            String expectedFilename = "virtual_source_file.h5";
            MemorySegment srcFile   = stringToSegment(arena, expectedFilename);
            MemorySegment srcDset   = stringToSegment(arena, "/data");
            hdf5_h.H5Pset_virtual(dcpl, vspace, srcFile, srcDset, srcspace);

            // Query filename length
            long nameLen = hdf5_h.H5Pget_virtual_filename(dcpl, 0, MemorySegment.NULL, 0);
            assertTrue("H5Pget_virtual_filename length query failed", nameLen > 0);

            // Get filename
            MemorySegment nameBuf = arena.allocate(nameLen + 1);
            long actualLen        = hdf5_h.H5Pget_virtual_filename(dcpl, 0, nameBuf, nameLen + 1);
            assertEquals("Filename length should match", nameLen, actualLen);

            String actualFilename = segmentToString(nameBuf);
            assertEquals("Filename should match", expectedFilename, actualFilename);

            // Cleanup
            hdf5_h.H5Sclose(srcspace);
            hdf5_h.H5Sclose(vspace);
            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pget_virtual_dsetname()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Create dataspaces
            long[] dimsArray   = {50, 100};
            MemorySegment dims = allocateLongArray(arena, 2);
            copyToSegment(dims, dimsArray);
            long vspace   = hdf5_h.H5Screate_simple(2, dims, MemorySegment.NULL);
            long srcspace = hdf5_h.H5Screate_simple(2, dims, MemorySegment.NULL);

            // Set virtual mapping with specific dataset name
            String expectedDsetName = "/group/virtual_dataset";
            MemorySegment srcFile   = stringToSegment(arena, "source.h5");
            MemorySegment srcDset   = stringToSegment(arena, expectedDsetName);
            hdf5_h.H5Pset_virtual(dcpl, vspace, srcFile, srcDset, srcspace);

            // Query dataset name length
            long nameLen = hdf5_h.H5Pget_virtual_dsetname(dcpl, 0, MemorySegment.NULL, 0);
            assertTrue("H5Pget_virtual_dsetname length query failed", nameLen > 0);

            // Get dataset name
            MemorySegment nameBuf = arena.allocate(nameLen + 1);
            long actualLen        = hdf5_h.H5Pget_virtual_dsetname(dcpl, 0, nameBuf, nameLen + 1);
            assertEquals("Dataset name length should match", nameLen, actualLen);

            String actualDsetName = segmentToString(nameBuf);
            assertEquals("Dataset name should match", expectedDsetName, actualDsetName);

            // Cleanup
            hdf5_h.H5Sclose(srcspace);
            hdf5_h.H5Sclose(vspace);
            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pget_virtual_vspace_and_srcspace()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Create virtual dataspace with specific dimensions
            long[] vdimsArray   = {30, 40};
            MemorySegment vdims = allocateLongArray(arena, 2);
            copyToSegment(vdims, vdimsArray);
            long vspace = hdf5_h.H5Screate_simple(2, vdims, MemorySegment.NULL);

            // Create source dataspace with different dimensions
            long[] sdimsArray   = {30, 40};
            MemorySegment sdims = allocateLongArray(arena, 2);
            copyToSegment(sdims, sdimsArray);
            long srcspace = hdf5_h.H5Screate_simple(2, sdims, MemorySegment.NULL);

            // Set virtual mapping
            hdf5_h.H5Pset_virtual(dcpl, vspace, stringToSegment(arena, "src.h5"),
                                  stringToSegment(arena, "/dset"), srcspace);

            // Get virtual dataspace back
            long retrieved_vspace = hdf5_h.H5Pget_virtual_vspace(dcpl, 0);
            assertTrue("H5Pget_virtual_vspace failed", isValidId(retrieved_vspace));

            // Verify virtual dataspace dimensions
            MemorySegment retrieved_vdims = allocateLongArray(arena, 2);
            hdf5_h.H5Sget_simple_extent_dims(retrieved_vspace, retrieved_vdims, MemorySegment.NULL);
            assertEquals("Virtual dim 0 should match", 30L, retrieved_vdims.get(ValueLayout.JAVA_LONG, 0));
            assertEquals("Virtual dim 1 should match", 40L, retrieved_vdims.get(ValueLayout.JAVA_LONG, 8));

            // Get source dataspace back
            long retrieved_srcspace = hdf5_h.H5Pget_virtual_srcspace(dcpl, 0);
            assertTrue("H5Pget_virtual_srcspace failed", isValidId(retrieved_srcspace));

            // Verify source dataspace dimensions
            MemorySegment retrieved_sdims = allocateLongArray(arena, 2);
            hdf5_h.H5Sget_simple_extent_dims(retrieved_srcspace, retrieved_sdims, MemorySegment.NULL);
            assertEquals("Source dim 0 should match", 30L, retrieved_sdims.get(ValueLayout.JAVA_LONG, 0));
            assertEquals("Source dim 1 should match", 40L, retrieved_sdims.get(ValueLayout.JAVA_LONG, 8));

            // Cleanup
            hdf5_h.H5Sclose(retrieved_srcspace);
            hdf5_h.H5Sclose(retrieved_vspace);
            hdf5_h.H5Sclose(srcspace);
            hdf5_h.H5Sclose(vspace);
            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_virtual_view()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create dataset access property list
            long dapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_ACCESS_ID_g());
            assertTrue("H5Pcreate dapl failed", isValidId(dapl));

            // Set virtual view to FIRST_MISSING
            int result = hdf5_h.H5Pset_virtual_view(dapl, hdf5_h.H5D_VDS_FIRST_MISSING());
            assertTrue("H5Pset_virtual_view failed", isSuccess(result));

            // Get virtual view back
            MemorySegment view = allocateIntArray(arena, 1);
            result             = hdf5_h.H5Pget_virtual_view(dapl, view);
            assertTrue("H5Pget_virtual_view failed", isSuccess(result));
            assertEquals("View should be FIRST_MISSING", hdf5_h.H5D_VDS_FIRST_MISSING(), getInt(view));

            // Change to LAST_AVAILABLE
            result = hdf5_h.H5Pset_virtual_view(dapl, hdf5_h.H5D_VDS_LAST_AVAILABLE());
            assertTrue("H5Pset_virtual_view (LAST_AVAILABLE) failed", isSuccess(result));

            result = hdf5_h.H5Pget_virtual_view(dapl, view);
            assertTrue("H5Pget_virtual_view (2nd call) failed", isSuccess(result));
            assertEquals("View should be LAST_AVAILABLE", hdf5_h.H5D_VDS_LAST_AVAILABLE(), getInt(view));

            hdf5_h.H5Pclose(dapl);
        }
    }

    @Test
    public void testH5Pset_copy_object_basic()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create object copy property list
            long ocpypl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_COPY_ID_g());
            assertTrue("H5Pcreate ocpypl failed", isValidId(ocpypl));

            // Set copy options - shallow hierarchy
            int copyOptions = hdf5_h.H5O_COPY_SHALLOW_HIERARCHY_FLAG();
            int result      = hdf5_h.H5Pset_copy_object(ocpypl, copyOptions);
            assertTrue("H5Pset_copy_object failed", isSuccess(result));

            // Get copy options back
            MemorySegment options = allocateIntArray(arena, 1);
            result                = hdf5_h.H5Pget_copy_object(ocpypl, options);
            assertTrue("H5Pget_copy_object failed", isSuccess(result));

            int retrievedOptions = getInt(options);
            assertEquals("Copy options should match", copyOptions, retrievedOptions);

            // Cleanup
            hdf5_h.H5Pclose(ocpypl);
        }
    }

    @Test
    public void testH5Pset_copy_object_multiple_flags()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long ocpypl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_COPY_ID_g());
            assertTrue("H5Pcreate ocpypl failed", isValidId(ocpypl));

            // Set multiple copy options with bitwise OR
            int copyOptions = hdf5_h.H5O_COPY_SHALLOW_HIERARCHY_FLAG() | hdf5_h.H5O_COPY_WITHOUT_ATTR_FLAG();
            int result      = hdf5_h.H5Pset_copy_object(ocpypl, copyOptions);
            assertTrue("H5Pset_copy_object failed", isSuccess(result));

            // Verify
            MemorySegment options = allocateIntArray(arena, 1);
            result                = hdf5_h.H5Pget_copy_object(ocpypl, options);
            assertTrue("H5Pget_copy_object failed", isSuccess(result));

            int retrievedOptions = getInt(options);
            assertEquals("Copy options should match", copyOptions, retrievedOptions);

            // Verify individual flags are set
            assertTrue("Should have SHALLOW_HIERARCHY flag",
                       (retrievedOptions & hdf5_h.H5O_COPY_SHALLOW_HIERARCHY_FLAG()) != 0);
            assertTrue("Should have WITHOUT_ATTR flag",
                       (retrievedOptions & hdf5_h.H5O_COPY_WITHOUT_ATTR_FLAG()) != 0);

            hdf5_h.H5Pclose(ocpypl);
        }
    }

    @Test
    public void testH5Pset_copy_object_expand_links()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long ocpypl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_COPY_ID_g());
            assertTrue("H5Pcreate ocpypl failed", isValidId(ocpypl));

            // Set options to expand soft and external links
            int copyOptions =
                hdf5_h.H5O_COPY_EXPAND_SOFT_LINK_FLAG() | hdf5_h.H5O_COPY_EXPAND_EXT_LINK_FLAG();
            int result = hdf5_h.H5Pset_copy_object(ocpypl, copyOptions);
            assertTrue("H5Pset_copy_object failed", isSuccess(result));

            // Verify
            MemorySegment options = allocateIntArray(arena, 1);
            result                = hdf5_h.H5Pget_copy_object(ocpypl, options);
            assertTrue("H5Pget_copy_object failed", isSuccess(result));

            int retrievedOptions = getInt(options);
            assertTrue("Should have EXPAND_SOFT_LINK flag",
                       (retrievedOptions & hdf5_h.H5O_COPY_EXPAND_SOFT_LINK_FLAG()) != 0);
            assertTrue("Should have EXPAND_EXT_LINK flag",
                       (retrievedOptions & hdf5_h.H5O_COPY_EXPAND_EXT_LINK_FLAG()) != 0);

            hdf5_h.H5Pclose(ocpypl);
        }
    }

    @Test
    public void testH5Pset_attr_phase_change()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create object creation property list
            long ocpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_CREATE_ID_g());
            assertTrue("H5Pcreate ocpl failed", isValidId(ocpl));

            // Set attribute phase change thresholds
            // max_compact: maximum number of attributes in compact storage
            // min_dense: minimum number of attributes in dense storage
            int maxCompact = 10;
            int minDense   = 8;
            int result     = hdf5_h.H5Pset_attr_phase_change(ocpl, maxCompact, minDense);
            assertTrue("H5Pset_attr_phase_change failed", isSuccess(result));

            // Get settings back
            MemorySegment maxCompactOut = allocateIntArray(arena, 1);
            MemorySegment minDenseOut   = allocateIntArray(arena, 1);
            result                      = hdf5_h.H5Pget_attr_phase_change(ocpl, maxCompactOut, minDenseOut);
            assertTrue("H5Pget_attr_phase_change failed", isSuccess(result));

            int retrievedMaxCompact = getInt(maxCompactOut);
            int retrievedMinDense   = getInt(minDenseOut);
            assertEquals("Max compact should match", maxCompact, retrievedMaxCompact);
            assertEquals("Min dense should match", minDense, retrievedMinDense);

            hdf5_h.H5Pclose(ocpl);
        }
    }

    @Test
    public void testH5Pset_copy_object_preserve_null()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long ocpypl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_COPY_ID_g());
            assertTrue("H5Pcreate ocpypl failed", isValidId(ocpypl));

            // Test PRESERVE_NULL flag
            int copyOptions = hdf5_h.H5O_COPY_PRESERVE_NULL_FLAG();
            int result      = hdf5_h.H5Pset_copy_object(ocpypl, copyOptions);
            assertTrue("H5Pset_copy_object failed", isSuccess(result));

            // Verify
            MemorySegment options = allocateIntArray(arena, 1);
            result                = hdf5_h.H5Pget_copy_object(ocpypl, options);
            assertTrue("H5Pget_copy_object failed", isSuccess(result));

            int retrievedOptions = getInt(options);
            assertEquals("Should have PRESERVE_NULL flag", copyOptions, retrievedOptions);

            // Test with ALL flags
            result = hdf5_h.H5Pset_copy_object(ocpypl, hdf5_h.H5O_COPY_ALL());
            assertTrue("H5Pset_copy_object (ALL) failed", isSuccess(result));

            result = hdf5_h.H5Pget_copy_object(ocpypl, options);
            assertTrue("H5Pget_copy_object failed", isSuccess(result));

            retrievedOptions = getInt(options);
            assertEquals("Should have ALL flags", hdf5_h.H5O_COPY_ALL(), retrievedOptions);

            hdf5_h.H5Pclose(ocpypl);
        }
    }

    @Test
    public void testH5Pset_elink_prefix()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create link access property list
            long lapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_LINK_ACCESS_ID_g());
            assertTrue("H5Pcreate lapl failed", isValidId(lapl));

            // Set external link prefix
            String prefix           = "/path/to/external/files";
            MemorySegment prefixSeg = stringToSegment(arena, prefix);
            int result              = hdf5_h.H5Pset_elink_prefix(lapl, prefixSeg);
            assertTrue("H5Pset_elink_prefix failed", isSuccess(result));

            // Query prefix length
            long prefixLen = hdf5_h.H5Pget_elink_prefix(lapl, MemorySegment.NULL, 0);
            assertTrue("Prefix length should be > 0", prefixLen > 0);

            // Get prefix
            MemorySegment prefixBuf = arena.allocate(prefixLen + 1);
            long actualLen          = hdf5_h.H5Pget_elink_prefix(lapl, prefixBuf, prefixLen + 1);
            assertEquals("Prefix length should match", prefixLen, actualLen);

            String retrievedPrefix = segmentToString(prefixBuf);
            assertEquals("Prefix should match", prefix, retrievedPrefix);

            hdf5_h.H5Pclose(lapl);
        }
    }

    @Test
    public void testH5Pset_elink_fapl()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create link access property list
            long lapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_LINK_ACCESS_ID_g());
            assertTrue("H5Pcreate lapl failed", isValidId(lapl));

            // Create file access property list to use for external links
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set external link file access property list
            int result = hdf5_h.H5Pset_elink_fapl(lapl, fapl);
            assertTrue("H5Pset_elink_fapl failed", isSuccess(result));

            // Get external link fapl
            long retrievedFapl = hdf5_h.H5Pget_elink_fapl(lapl);
            assertTrue("Retrieved fapl should be valid", isValidId(retrievedFapl));

            // Cleanup
            hdf5_h.H5Pclose(retrievedFapl);
            hdf5_h.H5Pclose(fapl);
            hdf5_h.H5Pclose(lapl);
        }
    }

    @Test
    public void testH5Pset_link_creation_order()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create group creation property list
            long gcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_GROUP_CREATE_ID_g());
            assertTrue("H5Pcreate gcpl failed", isValidId(gcpl));

            // Set link creation order tracking and indexing
            int crtOrderFlags = hdf5_h.H5P_CRT_ORDER_TRACKED() | hdf5_h.H5P_CRT_ORDER_INDEXED();
            int result        = hdf5_h.H5Pset_link_creation_order(gcpl, crtOrderFlags);
            assertTrue("H5Pset_link_creation_order failed", isSuccess(result));

            // Get link creation order flags
            MemorySegment flags = allocateIntArray(arena, 1);
            result              = hdf5_h.H5Pget_link_creation_order(gcpl, flags);
            assertTrue("H5Pget_link_creation_order failed", isSuccess(result));

            int retrievedFlags = getInt(flags);
            assertEquals("Flags should match", crtOrderFlags, retrievedFlags);

            // Verify individual flags
            assertTrue("Should have TRACKED flag", (retrievedFlags & hdf5_h.H5P_CRT_ORDER_TRACKED()) != 0);
            assertTrue("Should have INDEXED flag", (retrievedFlags & hdf5_h.H5P_CRT_ORDER_INDEXED()) != 0);

            hdf5_h.H5Pclose(gcpl);
        }
    }

    @Test
    public void testH5Pset_est_link_info()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create group creation property list
            long gcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_GROUP_CREATE_ID_g());
            assertTrue("H5Pcreate gcpl failed", isValidId(gcpl));

            // Set estimated link info (number of links, length of link names)
            int estNumEntries = 50;
            int estNameLen    = 20;
            int result        = hdf5_h.H5Pset_est_link_info(gcpl, estNumEntries, estNameLen);
            assertTrue("H5Pset_est_link_info failed", isSuccess(result));

            // Get estimated link info
            MemorySegment numEntries = allocateIntArray(arena, 1);
            MemorySegment nameLen    = allocateIntArray(arena, 1);
            result                   = hdf5_h.H5Pget_est_link_info(gcpl, numEntries, nameLen);
            assertTrue("H5Pget_est_link_info failed", isSuccess(result));

            assertEquals("Number of entries should match", estNumEntries, getInt(numEntries));
            assertEquals("Name length should match", estNameLen, getInt(nameLen));

            hdf5_h.H5Pclose(gcpl);
        }
    }

    @Test
    public void testH5Pset_link_phase_change()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create group creation property list
            long gcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_GROUP_CREATE_ID_g());
            assertTrue("H5Pcreate gcpl failed", isValidId(gcpl));

            // Set link phase change thresholds
            // max_compact: maximum number of links in compact storage
            // min_dense: minimum number of links in dense storage
            int maxCompact = 12;
            int minDense   = 10;
            int result     = hdf5_h.H5Pset_link_phase_change(gcpl, maxCompact, minDense);
            assertTrue("H5Pset_link_phase_change failed", isSuccess(result));

            // Get link phase change thresholds
            MemorySegment maxCompactOut = allocateIntArray(arena, 1);
            MemorySegment minDenseOut   = allocateIntArray(arena, 1);
            result                      = hdf5_h.H5Pget_link_phase_change(gcpl, maxCompactOut, minDenseOut);
            assertTrue("H5Pget_link_phase_change failed", isSuccess(result));

            assertEquals("Max compact should match", maxCompact, getInt(maxCompactOut));
            assertEquals("Min dense should match", minDense, getInt(minDenseOut));

            hdf5_h.H5Pclose(gcpl);
        }
    }

    @Test
    public void testH5Pset_evict_on_close()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set evict on close to true
            int result = hdf5_h.H5Pset_evict_on_close(fapl, true);
            assertTrue("H5Pset_evict_on_close failed", isSuccess(result));

            // Get evict on close setting
            MemorySegment evictSeg = arena.allocate(ValueLayout.JAVA_BOOLEAN);
            result                 = hdf5_h.H5Pget_evict_on_close(fapl, evictSeg);
            assertTrue("H5Pget_evict_on_close failed", isSuccess(result));

            boolean evict = evictSeg.get(ValueLayout.JAVA_BOOLEAN, 0);
            assertTrue("Evict on close should be true", evict);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_file_locking()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set file locking (use_file_locking=true, ignore_when_disabled=false)
            int result = hdf5_h.H5Pset_file_locking(fapl, true, false);
            assertTrue("H5Pset_file_locking failed", isSuccess(result));

            // Get file locking settings
            MemorySegment useLockingSeg = arena.allocate(ValueLayout.JAVA_BOOLEAN);
            MemorySegment ignoreFailSeg = arena.allocate(ValueLayout.JAVA_BOOLEAN);
            result                      = hdf5_h.H5Pget_file_locking(fapl, useLockingSeg, ignoreFailSeg);
            assertTrue("H5Pget_file_locking failed", isSuccess(result));

            boolean useLocking = useLockingSeg.get(ValueLayout.JAVA_BOOLEAN, 0);
            boolean ignoreFail = ignoreFailSeg.get(ValueLayout.JAVA_BOOLEAN, 0);
            assertTrue("Use locking should be true", useLocking);
            assertFalse("Ignore fail should be false", ignoreFail);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_page_buffer_size()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set page buffer size (4MB buffer, 50% metadata, 25% raw data)
            long bufSize   = 4 * 1024 * 1024; // 4MB
            int minMetaPct = 50;
            int minRawPct  = 25;
            int result     = hdf5_h.H5Pset_page_buffer_size(fapl, bufSize, minMetaPct, minRawPct);
            assertTrue("H5Pset_page_buffer_size failed", isSuccess(result));

            // Get page buffer size
            MemorySegment bufSizeSeg    = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment minMetaPctSeg = arena.allocate(ValueLayout.JAVA_INT);
            MemorySegment minRawPctSeg  = arena.allocate(ValueLayout.JAVA_INT);
            result = hdf5_h.H5Pget_page_buffer_size(fapl, bufSizeSeg, minMetaPctSeg, minRawPctSeg);
            assertTrue("H5Pget_page_buffer_size failed", isSuccess(result));

            long retBufSize = bufSizeSeg.get(ValueLayout.JAVA_LONG, 0);
            int retMetaPct  = minMetaPctSeg.get(ValueLayout.JAVA_INT, 0);
            int retRawPct   = minRawPctSeg.get(ValueLayout.JAVA_INT, 0);

            assertEquals("Buffer size should match", bufSize, retBufSize);
            assertEquals("Metadata percent should match", minMetaPct, retMetaPct);
            assertEquals("Raw data percent should match", minRawPct, retRawPct);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_metadata_read_attempts()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long fapl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_FILE_ACCESS_ID_g());
            assertTrue("H5Pcreate fapl failed", isValidId(fapl));

            // Set metadata read attempts to 5
            int attempts = 5;
            int result   = hdf5_h.H5Pset_metadata_read_attempts(fapl, attempts);
            assertTrue("H5Pset_metadata_read_attempts failed", isSuccess(result));

            // Get metadata read attempts
            MemorySegment attemptsSeg = arena.allocate(ValueLayout.JAVA_INT);
            result                    = hdf5_h.H5Pget_metadata_read_attempts(fapl, attemptsSeg);
            assertTrue("H5Pget_metadata_read_attempts failed", isSuccess(result));

            int retAttempts = attemptsSeg.get(ValueLayout.JAVA_INT, 0);
            assertEquals("Attempts should match", attempts, retAttempts);

            hdf5_h.H5Pclose(fapl);
        }
    }

    @Test
    public void testH5Pset_obj_track_times()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            long ocpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_OBJECT_CREATE_ID_g());
            assertTrue("H5Pcreate ocpl failed", isValidId(ocpl));

            // Set object time tracking to false
            int result = hdf5_h.H5Pset_obj_track_times(ocpl, false);
            assertTrue("H5Pset_obj_track_times failed", isSuccess(result));

            // Get object time tracking setting
            MemorySegment trackSeg = arena.allocate(ValueLayout.JAVA_BOOLEAN);
            result                 = hdf5_h.H5Pget_obj_track_times(ocpl, trackSeg);
            assertTrue("H5Pget_obj_track_times failed", isSuccess(result));

            boolean track = trackSeg.get(ValueLayout.JAVA_BOOLEAN, 0);
            assertFalse("Track times should be false", track);

            hdf5_h.H5Pclose(ocpl);
        }
    }

    @Test
    public void testH5Pget_virtual_info()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create DCPL with virtual dataset mapping
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            long[] dims           = {5, 10};
            MemorySegment dimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
            long vspace           = hdf5_h.H5Screate_simple(2, dimsSeg, MemorySegment.NULL);
            long srcspace         = hdf5_h.H5Screate_simple(2, dimsSeg, MemorySegment.NULL);

            MemorySegment srcFileName = stringToSegment(arena, "test_source.h5");
            MemorySegment srcDsetName = stringToSegment(arena, "/data");
            hdf5_h.H5Pset_virtual(dcpl, vspace, srcFileName, srcDsetName, srcspace);

            // Get virtual dataset info
            MemorySegment count = allocateLongArray(arena, 1);
            int result          = hdf5_h.H5Pget_virtual_count(dcpl, count);
            assertTrue("H5Pget_virtual_count failed", isSuccess(result));
            long vcount = count.get(ValueLayout.JAVA_LONG, 0);
            assertEquals("Should have 1 virtual mapping", 1, vcount);

            // Get virtual vspace for index 0
            long retrieved_vspace = hdf5_h.H5Pget_virtual_vspace(dcpl, 0);
            assertTrue("H5Pget_virtual_vspace should succeed", isValidId(retrieved_vspace));

            // Get virtual source space for index 0
            long retrieved_srcspace = hdf5_h.H5Pget_virtual_srcspace(dcpl, 0);
            assertTrue("H5Pget_virtual_srcspace should succeed", isValidId(retrieved_srcspace));

            // Cleanup
            hdf5_h.H5Sclose(retrieved_vspace);
            hdf5_h.H5Sclose(retrieved_srcspace);
            hdf5_h.H5Sclose(vspace);
            hdf5_h.H5Sclose(srcspace);
            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_external()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create DCPL for external storage
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Add external file
            MemorySegment extFile = stringToSegment(arena, "external_data.bin");
            long offset           = 0;
            long size             = 1024; // 1KB
            int result            = hdf5_h.H5Pset_external(dcpl, extFile, offset, size);
            assertTrue("H5Pset_external failed", isSuccess(result));

            // Get external file count
            int extCount = hdf5_h.H5Pget_external_count(dcpl);
            assertEquals("Should have 1 external file", 1, extCount);

            // Cleanup
            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pget_external()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create DCPL and add external file
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            String extFileName    = "my_external.bin";
            MemorySegment extFile = stringToSegment(arena, extFileName);
            long offset           = 1024;
            long size             = 4096;
            hdf5_h.H5Pset_external(dcpl, extFile, offset, size);

            // Get external file info
            int nameSize            = 256;
            MemorySegment nameBuf   = arena.allocate(nameSize);
            MemorySegment offsetSeg = arena.allocate(ValueLayout.JAVA_LONG);
            MemorySegment sizeSeg   = arena.allocate(ValueLayout.JAVA_LONG);

            long retval = hdf5_h.H5Pget_external(dcpl, 0, nameSize, nameBuf, offsetSeg, sizeSeg);
            assertTrue("H5Pget_external should succeed", retval >= 0);

            // Verify retrieved values
            String retrievedName = segmentToString(nameBuf);
            assertEquals("File name should match", extFileName, retrievedName);

            long retrievedOffset = offsetSeg.get(ValueLayout.JAVA_LONG, 0);
            assertEquals("Offset should match", offset, retrievedOffset);

            long retrievedSize = sizeSeg.get(ValueLayout.JAVA_LONG, 0);
            assertEquals("Size should match", size, retrievedSize);

            // Cleanup
            hdf5_h.H5Pclose(dcpl);
        }
    }

    @Test
    public void testH5Pset_external_multiple()
    {
        System.out.print(testname.getMethodName());

        try (Arena arena = Arena.ofConfined()) {
            // Create DCPL
            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            // Add multiple external files
            hdf5_h.H5Pset_external(dcpl, stringToSegment(arena, "ext1.bin"), 0, 1024);
            hdf5_h.H5Pset_external(dcpl, stringToSegment(arena, "ext2.bin"), 0, 2048);
            hdf5_h.H5Pset_external(dcpl, stringToSegment(arena, "ext3.bin"), 0, 4096);

            // Verify count
            int extCount = hdf5_h.H5Pget_external_count(dcpl);
            assertEquals("Should have 3 external files", 3, extCount);

            // Cleanup
            hdf5_h.H5Pclose(dcpl);
        }
    }
}
