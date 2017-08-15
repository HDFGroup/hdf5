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

//Information struct for Attribute (For H5Aget_info/H5Aget_info_by_idx/H5Aget_info_by_name)
public class H5A_info_t implements Serializable{
    private static final long serialVersionUID = 2791443594041667613L;
    public boolean corder_valid; // Indicate if creation order is valid
    public long corder; // Creation order of attribute
    public int cset; // Character set of attribute name
    public long data_size; // Size of raw data

    H5A_info_t(boolean corder_valid, long corder, int cset, long data_size) {
        this.corder_valid = corder_valid;
        this.corder = corder;
        this.cset = cset;
        this.data_size = data_size;
    }
}
