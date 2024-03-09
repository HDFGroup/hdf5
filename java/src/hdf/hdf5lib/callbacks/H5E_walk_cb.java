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

import hdf.hdf5lib.structs.H5E_error2_t;

/**
 * Information class for link callback for H5Ewalk.
 *
 */
public interface H5E_walk_cb extends Callbacks {
    /**
     * @ingroup JCALLBK
     *
     *  application callback for each error stack element
     *
     *  @param nidx      the index of the current error stack element
     *  @param info      the error stack "info" struct
     *  @param op_data   the operator data passed in to H5Ewalk
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
    int callback(int nidx, H5E_error2_t info, H5E_walk_t op_data);
}
