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

import hdf.hdf5lib.structs.H5O_info_t;

/**
 * Information class for link callback for H5Ovisit/H5Ovisit_by_name.
 *
 */
public interface H5O_iterate_t extends Callbacks {
    /**
     * @ingroup JCALL
     *
     *  application callback for each group
     *
     *  @param loc_id    the ID for the group or dataset being iterated over
     *  @param name      the name of the current object
     *  @param info      the object's "info" struct
     *  @param op_data   the operator data passed in to H5Oiterate
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
    int callback(long loc_id, String name, H5O_info_t info, H5O_iterate_opdata_t op_data);
}
