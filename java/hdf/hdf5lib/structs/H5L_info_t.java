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

package hdf.hdf5lib.structs;

import java.io.Serializable;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.structs.H5O_token_t;

/**
 * Information struct for link (for H5Lget_info/H5Lget_info_by_idx)
 *
 */
public class H5L_info_t implements Serializable {
    private static final long serialVersionUID = -4754320605310155033L;
    /** Type of link */
    public int type;
    /** Indicate if creation order is valid */
    public boolean corder_valid;
    /** Creation order */
    public long corder;
    /** Character set of link name */
    public int cset;
    /** Character set of link name */
    public H5O_token_t token;
    /** Size of a soft link or user-defined link value */
    public long val_size;

    /** Constructor for using object token portion of C union */
    public H5L_info_t(int type, boolean corder_valid, long corder, int cset, H5O_token_t token)
    {
        this.type         = type;
        this.corder_valid = corder_valid;
        this.corder       = corder;
        this.cset         = cset;
        this.token        = token;
        this.val_size     = -1;
    }

    /** Constructor for using val_size portion of C union */
    public H5L_info_t(int type, boolean corder_valid, long corder, int cset, long val_size)
    {
        this.type         = type;
        this.corder_valid = corder_valid;
        this.corder       = corder;
        this.cset         = cset;
        this.token        = HDF5Constants.H5O_TOKEN_UNDEF;
        this.val_size     = val_size;
    }

    /** Constructor for using val_size portion of C union */
    public H5L_info_t(MemorySegment linfo_segment)
    {
        // Unpack the H5L_info2_t from the MemorySegment
        MemorySegment u_segment = org.hdfgroup.javahdf5.H5L_info2_t.u(linfo_segment);
        if (org.hdfgroup.javahdf5.H5L_info2_t.type(linfo_segment) == HDF5Constants.H5L_TYPE_HARD) {
            this.token =
                new hdf.hdf5lib.structs.H5O_token_t(org.hdfgroup.javahdf5.H5L_info2_t.u.token(u_segment));
            this.type         = org.hdfgroup.javahdf5.H5L_info2_t.type(linfo_segment);
            this.corder_valid = org.hdfgroup.javahdf5.H5L_info2_t.corder_valid(linfo_segment);
            this.corder       = org.hdfgroup.javahdf5.H5L_info2_t.corder(linfo_segment);
            this.cset         = org.hdfgroup.javahdf5.H5L_info2_t.cset(linfo_segment);
            this.val_size     = -1;
        }
        else {
            this.type         = org.hdfgroup.javahdf5.H5L_info2_t.type(linfo_segment);
            this.corder_valid = org.hdfgroup.javahdf5.H5L_info2_t.corder_valid(linfo_segment);
            this.corder       = org.hdfgroup.javahdf5.H5L_info2_t.corder(linfo_segment);
            this.cset         = org.hdfgroup.javahdf5.H5L_info2_t.cset(linfo_segment);
            this.token        = HDF5Constants.H5O_TOKEN_UNDEF;
            this.val_size     = org.hdfgroup.javahdf5.H5L_info2_t.u.val_size(u_segment);
        }
    }
}
