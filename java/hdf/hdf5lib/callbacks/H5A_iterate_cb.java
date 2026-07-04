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

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.MemorySegment;

import org.hdfgroup.javahdf5.*;

/**
 * Information class for link callback for H5Aiterate.
 *
 */
public interface H5A_iterate_cb extends org.hdfgroup.javahdf5.H5A_operator2_t.Function {
    /**
     * @ingroup JCALLBK
     *
     *  application callback for each attribute
     *
     *  @param loc_id    the ID for the group or dataset being iterated over
     *  @param name      the name of the current attribute about the object
     *  @param info      the attribute's "info" struct
     *  @param op_data   the operator data passed in to H5Aiterate
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
    int apply(long location_id, MemorySegment attr_name, MemorySegment ainfo, MemorySegment op_data);
}
