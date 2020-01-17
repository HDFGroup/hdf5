/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package hdf.hdf5lib.structs;

import java.io.Serializable;
import java.util.Arrays;

import hdf.hdf5lib.HDF5Constants;

// Object token, which is a unique and permanent identifier, for an HDF5 object within a container.
public class H5O_token_t implements Serializable {
    private static final long serialVersionUID = -4754320605310155032L;
    public byte[] data;

    H5O_token_t (byte[] data) {
        this.data = data;
    }

    public boolean isUndefined() {
        return this.equals(HDF5Constants.H5O_TOKEN_UNDEF);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;

        if (!(o instanceof H5O_token_t))
            return false;

        H5O_token_t token = (H5O_token_t) o;

        return Arrays.equals(this.data, token.data);
    }
}