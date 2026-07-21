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

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.lang.foreign.MemorySegment;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.callbacks.H5D_chunk_iter_cb;
import hdf.hdf5lib.callbacks.H5D_chunk_iter_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Quick benchmark comparing H5Dchunk_iter against looping over H5Dget_chunk_info by
 * chunk index, following the methodology from
 * https://github.com/JuliaIO/HDF5.jl/pull/1031#issuecomment-1407749686 which found that
 * H5Dchunk_iter scales much better than repeated by-index lookups as the chunk count grows.
 */
public class TestH5DChunkIterPerf {
    private static final String H5_FILE = "testDChunkIterPerf.h5";
    private static final int CHUNK      = 16;
    private static final int[] SIZES    = {32, 64, 128, 256, 512, 1024};

    long H5fid = HDF5Constants.H5I_INVALID_HID;

    private final void _deleteFile(String filename)
    {
        File file = new File(filename);
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
            }
        }
    }

    @Before
    public void createH5file() throws Exception
    {
        _deleteFile(H5_FILE);
        H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                             HDF5Constants.H5P_DEFAULT);
    }

    @After
    public void deleteH5file() throws Exception
    {
        if (H5fid >= 0)
            H5.H5Fclose(H5fid);
        _deleteFile(H5_FILE);
    }

    private long createChunkedDataset(int size) throws Exception
    {
        long dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
        H5.H5Pset_alloc_time(dcpl_id, HDF5Constants.H5D_ALLOC_TIME_EARLY);
        H5.H5Pset_chunk(dcpl_id, 2, new long[] {CHUNK, CHUNK});

        long sid = H5.H5Screate_simple(2, new long[] {size, size}, null);
        long did = H5.H5Dcreate(H5fid, "dset" + size, HDF5Constants.H5T_NATIVE_UINT8, sid,
                                HDF5Constants.H5P_DEFAULT, dcpl_id, HDF5Constants.H5P_DEFAULT);
        H5.H5Pclose(dcpl_id);
        H5.H5Sclose(sid);
        return did;
    }

    private long countByIterate(long did) throws Exception
    {
        final long[] count = {0};
        class Data implements H5D_chunk_iter_t {
        }
        H5D_chunk_iter_cb cb = new H5D_chunk_iter_cb() {
            public int apply(MemorySegment offset, int filter_mask, long addr, long size,
                             MemorySegment op_data)
            {
                count[0]++;
                return 0;
            }
        };
        H5.H5Dchunk_iter(did, HDF5Constants.H5P_DEFAULT, cb, new Data());
        return count[0];
    }

    private long countByIndex(long did) throws Exception
    {
        long sid          = H5.H5Dget_space(did);
        long nchunks      = H5.H5Dget_num_chunks(did, sid);
        long[] offset     = new long[2];
        int[] filter_mask = new int[1];
        long[] addr       = new long[1];
        long[] size       = new long[1];

        for (long i = 0; i < nchunks; i++)
            H5.H5Dget_chunk_info(did, sid, i, offset, filter_mask, addr, size);

        H5.H5Sclose(sid);
        return nchunks;
    }

    @Test
    public void testH5Dchunk_iter_vs_get_chunk_info_perf() throws Exception
    {
        System.out.println();
        System.out.printf("%10s %14s %14s %10s%n", "size", "iter_ms", "indx_ms", "ratio");

        for (int size : SIZES) {
            long did = createChunkedDataset(size);

            long t0          = System.nanoTime();
            long count_iter  = countByIterate(did);
            long t1          = System.nanoTime();
            long count_index = countByIndex(did);
            long t2          = System.nanoTime();

            H5.H5Dclose(did);

            double iter_ms = (t1 - t0) / 1.0e6;
            double indx_ms = (t2 - t1) / 1.0e6;

            assertEquals("chunk counts must agree for size " + size, count_iter, count_index);
            long chunks_per_dim = (size + CHUNK - 1) / CHUNK;
            assertEquals("chunk count for size " + size, chunks_per_dim * chunks_per_dim, count_iter);

            System.out.printf("%10d %14.3f %14.3f %10.3f%n", size, iter_ms, indx_ms, indx_ms / iter_ms);
        }
    }
}
