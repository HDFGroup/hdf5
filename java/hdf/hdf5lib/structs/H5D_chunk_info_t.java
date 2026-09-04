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

package hdf.hdf5lib.structs;

import java.io.Serializable;

/**
 * Bulk result of H5Dchunk_iter_all(), holding information about every chunk of a chunked dataset.
 *
 * To avoid one Java object allocation per chunk, the per-chunk fields are stored as parallel
 * primitive arrays rather than an array of per-chunk objects; use {@link #getOffset(int, int)} to
 * read a single chunk's offset coordinate.
 *
 */
public class H5D_chunk_info_t implements Serializable {
    private static final long serialVersionUID = -8091628506238589401L;

    /** Number of dimensions (rank) of the dataset; used to interpret offset. */
    public final int rank;
    /** Flattened chunk offsets: chunk i's coordinate in dimension d is offset[i * rank + d]. */
    public final long[] offset;
    /** Bitmask indicating the filters used when each chunk was written; filterMask[i] for chunk i. */
    public final int[] filterMask;
    /** Chunk address in the file for each chunk; addr[i] for chunk i. */
    public final long[] addr;
    /** Chunk size in bytes for each chunk, 0 if the chunk does not exist; size[i] for chunk i. */
    public final long[] size;

    public H5D_chunk_info_t(int rank, long[] offset, int[] filterMask, long[] addr, long[] size)
    {
        this.rank       = rank;
        this.offset     = offset;
        this.filterMask = filterMask;
        this.addr       = addr;
        this.size       = size;
    }

    /**
     * @return the number of chunks described by this object.
     */
    public int getNumChunks() { return filterMask.length; }

    /**
     * @param chunkIndex
     *            index of the chunk, between 0 and getNumChunks() - 1.
     * @param dim
     *            dimension, between 0 and rank - 1.
     * @return the logical position of the given chunk's first element in dimension dim.
     */
    public long getOffset(int chunkIndex, int dim) { return offset[chunkIndex * rank + dim]; }
}
