/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package hdf.hdf5lib.structs;

import java.io.Serializable;

//Information struct for link (for H5Lget_info/H5Lget_info_by_idx)
public class H5L_info_t implements Serializable{
    private static final long serialVersionUID = -4754320605310155033L;
    public int     type;
    public boolean corder_valid;
    public long    corder;
    public int     cset;
    public long    address_val_size;

    H5L_info_t (int type, boolean corder_valid, long corder,
        int cset, long address_val_size)
    {
        this.type = type;
        this.corder_valid = corder_valid;
        this.corder = corder;
        this.cset = cset;
        this.address_val_size = address_val_size;
    }
}
