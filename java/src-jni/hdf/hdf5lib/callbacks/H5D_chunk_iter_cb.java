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

package hdf.hdf5lib.callbacks;

/**
 * Information class for the chunk iterator callback for H5Dchunk_iter.
 *
 */
public interface H5D_chunk_iter_cb extends H5Callbacks {
    /**
     * @ingroup JCALLBK
     *
     *  application callback for each chunk of a chunked dataset
     *
     *  @param offset      the logical position of the chunk's first element in units of dataset
     *                     elements. For performance, the same array object is reused for every chunk
     *                     in a given H5Dchunk_iter() call; its contents are only valid for the
     *                     duration of this callback invocation. Copy the values out if they need to
     *                     be retained after the callback returns.
     *  @param filter_mask  bitmask indicating the filters used when the chunk was written
     *  @param addr        the chunk address in the file, taking the user block (if any) into account
     *  @param size        the chunk size in bytes, 0 if the chunk does not exist
     *  @param op_data     the operator data passed in to H5Dchunk_iter
     *
     *  @return operation status
     *      A. Zero causes the iterator to continue, returning zero when all
     *          chunks have been processed.
     *      B. Positive causes the iterator to immediately return that positive
     *          value, indicating short-circuit success.
     *      C. Negative causes the iterator to immediately return that value,
     *          indicating failure.
     */
    int callback(long[] offset, int filter_mask, long addr, long size, H5D_chunk_iter_t op_data);
}
