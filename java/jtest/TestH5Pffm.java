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
}
