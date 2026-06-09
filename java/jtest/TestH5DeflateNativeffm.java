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

import hdf.hdf5lib.Hdf5NativeLoader;

import org.hdfgroup.javahdf5.hdf5_h;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * FFM smoke tests for Maven-shipped native stacks: {@link Hdf5NativeLoader} must load bundled zlib
 * before libhdf5 so the built-in deflate (gzip) filter is registered, and chunked+gzip datasets
 * must round-trip through the FFM API.
 */
public class TestH5DeflateNativeffm {
    @Rule
    public TestName testname = new TestName();

    private static final String H5_FILE      = "testDeflateNativeffm.h5";
    private static final String DATASET_NAME = "gzip_chunked_dset";

    @Before
    public void setup()
    {
        System.out.print(testname.getMethodName());

        // Same native bootstrap path Maven consumers use (bundled zlib/szip then libhdf5).
        Hdf5NativeLoader.loadBundledDependenciesBeforeHdf5();
        hdf5_h.H5open();
    }

    @After
    public void cleanup()
    {
        deleteFile(H5_FILE);
        System.out.println();
    }

    private static void deleteFile(String filename)
    {
        File file = new File(filename);
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                // ignore
            }
        }
    }

    @Test
    public void testBundledLoaderAndDeflateFilterAvail()
    {
        int deflateAvail = hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_DEFLATE());
        assertTrue("DEFLATE filter must be available when libhdf5 is built with zlib support "
                       + "(bundledZlib=" + Hdf5NativeLoader.bundledZlibLoadSucceeded() + ")",
                   deflateAvail > 0);
    }

    @Test
    public void testChunkedGzipDatasetRoundTrip()
    {
        assertTrue("DEFLATE filter required for this test",
                   hdf5_h.H5Zfilter_avail(hdf5_h.H5Z_FILTER_DEFLATE()) > 0);

        final int dimX         = 8;
        final int dimY         = 12;
        final int rank         = 2;
        final long[] chunkDims = {4, 6};

        try (Arena arena = Arena.ofConfined()) {
            MemorySegment fileNameSegment = stringToSegment(arena, H5_FILE);
            long fid = hdf5_h.H5Fcreate(fileNameSegment, hdf5_h.H5F_ACC_TRUNC(), hdf5_h.H5P_DEFAULT(),
                                        hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fcreate failed", isValidId(fid));

            long dcpl = hdf5_h.H5Pcreate(hdf5_h.H5P_CLS_DATASET_CREATE_ID_g());
            assertTrue("H5Pcreate dcpl failed", isValidId(dcpl));

            MemorySegment chunkDimsSegment = allocateLongArray(arena, rank);
            copyToSegment(chunkDimsSegment, chunkDims);
            assertTrue("H5Pset_chunk failed", isSuccess(hdf5_h.H5Pset_chunk(dcpl, rank, chunkDimsSegment)));
            assertTrue("H5Pset_deflate failed", isSuccess(hdf5_h.H5Pset_deflate(dcpl, 6)));

            long[] dims               = {dimX, dimY};
            MemorySegment dimsSegment = allocateLongArray(arena, rank);
            copyToSegment(dimsSegment, dims);
            long sid = hdf5_h.H5Screate_simple(rank, dimsSegment, MemorySegment.NULL);
            assertTrue("H5Screate_simple failed", isValidId(sid));

            MemorySegment dsetNameSegment = stringToSegment(arena, DATASET_NAME);
            long did = hdf5_h.H5Dcreate2(fid, dsetNameSegment, hdf5_h.H5T_NATIVE_INT_g(), sid,
                                         hdf5_h.H5P_DEFAULT(), dcpl, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dcreate2 failed", isValidId(did));

            int[] writeData = new int[dimX * dimY];
            for (int i = 0; i < writeData.length; i++) {
                writeData[i] = i;
            }
            MemorySegment writeSegment = allocateIntArray(arena, writeData.length);
            copyToSegment(writeSegment, writeData);
            assertTrue("H5Dwrite failed",
                       isSuccess(hdf5_h.H5Dwrite(did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                                 hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), writeSegment)));

            hdf5_h.H5Fflush(fid, hdf5_h.H5F_SCOPE_LOCAL());
            closeQuietly(did, hdf5_h::H5Dclose);
            closeQuietly(sid, hdf5_h::H5Sclose);
            closeQuietly(dcpl, hdf5_h::H5Pclose);
            closeQuietly(fid, hdf5_h::H5Fclose);

            // Reopen and read through the gzip chunk pipeline (decode path).
            fid = hdf5_h.H5Fopen(fileNameSegment, hdf5_h.H5F_ACC_RDONLY(), hdf5_h.H5P_DEFAULT());
            assertTrue("H5Fopen failed", isValidId(fid));

            did = hdf5_h.H5Dopen2(fid, dsetNameSegment, hdf5_h.H5P_DEFAULT());
            assertTrue("H5Dopen2 failed", isValidId(did));

            int[] readData            = new int[writeData.length];
            MemorySegment readSegment = allocateIntArray(arena, readData.length);
            assertTrue("H5Dread failed",
                       isSuccess(hdf5_h.H5Dread(did, hdf5_h.H5T_NATIVE_INT_g(), hdf5_h.H5S_ALL(),
                                                hdf5_h.H5S_ALL(), hdf5_h.H5P_DEFAULT(), readSegment)));
            copyFromSegment(readSegment, readData);
            assertArrayEquals("gzip chunked dataset round-trip", writeData, readData);

            closeQuietly(did, hdf5_h::H5Dclose);
            closeQuietly(fid, hdf5_h::H5Fclose);
        }
    }
}
