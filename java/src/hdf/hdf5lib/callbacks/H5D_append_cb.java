/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package hdf.hdf5lib.callbacks;

/**
 * Information class for link callback for H5Pset/get_append_flush.
 *
 */
public interface H5D_append_cb extends Callbacks {
    /**
     * @ingroup JCALL
     *
     *  application callback for each dataset access property list
     *
     *  @param dataset_id    the ID for the dataset being iterated over
     *  @param cur_dims      the dimension sizes for determining boundary
     *  @param op_data       the operator data passed in to H5Pset/get_append_flush
     *
     *  @return operation status
     *      A. Zero causes the iterator to continue, returning zero when all
     *          attributes have been processed.
     *      B. Positive causes the iterator to immediately return that positive
     *          value, indicating short-circuit success.  The iterator can be
     *          restarted at the next attribute.
     *      C. Negative causes the iterator to immediately return that value,
     *          indicating failure.  The iterator can be restarted at the next
     *          attribute.
     */
    int callback(long dataset_id, long[] cur_dims, H5D_append_t op_data);
}
