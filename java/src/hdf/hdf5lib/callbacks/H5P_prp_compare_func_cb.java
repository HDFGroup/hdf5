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
 * Information class for link callback for H5Pregister2.
 *
 */
public interface H5P_prp_compare_func_cb extends Callbacks {
    /**
     *  application callback for each property list
     *
     *  @param value1  the value of the first property being compared
     *  @param value2  the value of the second property being compared
     *  @param size    the size of the property value
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
    int callback(byte[] value1, byte[] value2, long size);
}
