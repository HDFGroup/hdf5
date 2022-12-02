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
 * Information class for link callback for H5Diterate.
 *
 */
public interface H5D_iterate_cb extends Callbacks {
    /**
     * @ingroup JCALL
     *
     *  application callback for each dataset element
     *
     *  @param elem      the pointer to the element in memory containing the current point
     *  @param elem_type the datatype ID for the elements stored in elem
     *  @param ndim      the number of dimensions for POINT array
     *  @param point     the array containing the location of the element within the original dataspace
     *  @param op_data   the operator data passed in to H5Diterate
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
    int callback(byte[] elem, long elem_type, int ndim, long[] point, H5D_iterate_t op_data);
}
