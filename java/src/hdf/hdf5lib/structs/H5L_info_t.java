/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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

import hdf.hdf5lib.HDF5Constants;

// Information struct for link (for H5Lget_info/H5Lget_info_by_idx)
public class H5L_info_t implements Serializable {
    private static final long serialVersionUID = -4754320605310155033L;
    public int         type;
    public boolean     corder_valid;
    public long        corder;
    public int         cset;
    public H5O_token_t token;
    public long        val_size;

    // Constructor for using object token portion of C union
    H5L_info_t (int type, boolean corder_valid, long corder,
        int cset, H5O_token_t token)
    {
        this.type = type;
        this.corder_valid = corder_valid;
        this.corder = corder;
        this.cset = cset;
        this.token = token;
        this.val_size = -1;
    }

    // Constructor for using val_size portion of C union
    H5L_info_t (int type, boolean corder_valid, long corder,
        int cset, long val_size)
    {
        this.type = type;
        this.corder_valid = corder_valid;
        this.corder = corder;
        this.cset = cset;
        this.token = HDF5Constants.H5O_TOKEN_UNDEF;
        this.val_size = val_size;
    }
}
