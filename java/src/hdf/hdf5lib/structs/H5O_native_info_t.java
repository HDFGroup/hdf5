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
 * Information struct for native HDF5 object info, such as object header metadata (for
 * H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx).
 *
 */
public class H5O_native_info_t implements Serializable {
    private static final long serialVersionUID = 7883826382952577189L;
    /** Object header information */
    public H5O_hdr_info_t hdr_info;

    /* Extra metadata storage for obj & attributes */
    /** v1/v2 B-tree and local/fractal heap for groups, B-tree for chunked datasets */
    public H5_ih_info_t obj_info;
    /** v2 B-tree and heap for attributes */
    public H5_ih_info_t attr_info;

    H5O_native_info_t(H5O_hdr_info_t oheader_info, H5_ih_info_t obj_info, H5_ih_info_t attr_info)
    {
        this.hdr_info  = oheader_info;
        this.obj_info  = obj_info;
        this.attr_info = attr_info;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
            return true;

        if (!(o instanceof H5O_native_info_t))
            return false;

        H5O_native_info_t info = (H5O_native_info_t)o;

        if (!this.hdr_info.equals(info.hdr_info) || !this.obj_info.equals(info.obj_info) ||
            !this.attr_info.equals(info.attr_info))
            return false;

        return true;
    }
}
