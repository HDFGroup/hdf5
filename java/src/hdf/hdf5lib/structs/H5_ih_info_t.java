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

package hdf.hdf5lib.structs;

import java.io.Serializable;

/**
 * Information struct for group (for H5Gget_info/H5Gget_info_by_name/H5Gget_info_by_idx)
 *
 */
public class H5_ih_info_t implements Serializable {
    private static final long serialVersionUID = -142238015615462707L;
    /** btree and/or list size of index */
    public long index_size;
    /** btree and/or list size of hp */
    public long heap_size;

    H5_ih_info_t(long index_size, long heap_size)
    {
        this.index_size = index_size;
        this.heap_size  = heap_size;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
            return true;

        if (!(o instanceof H5_ih_info_t))
            return false;

        H5_ih_info_t info = (H5_ih_info_t)o;

        if (this.index_size != info.index_size)
            return false;
        if (this.heap_size != info.heap_size)
            return false;

        return true;
    }
}
