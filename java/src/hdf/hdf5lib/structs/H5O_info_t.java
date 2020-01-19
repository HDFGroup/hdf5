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

// Information struct for object (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx)
public class H5O_info_t implements Serializable {
    private static final long serialVersionUID = 4691681163544054518L;
    public long        fileno;     /* File number that object is located in */
    public H5O_token_t token;      /* Object token in file */
    public int         type;       /* Basic object type (group, dataset, etc.) */
    public int         rc;         /* Reference count of object    */
    public long        atime;      /* Access time          */
    public long        mtime;      /* Modification time        */
    public long        ctime;      /* Change time          */
    public long        btime;      /* Birth time           */
    public long        num_attrs;  /* # of attributes attached to object */

    public H5O_info_t (long fileno, H5O_token_t token, int type,
        int rc, long atime, long mtime, long ctime, long btime, long num_attrs)
    {
        this.fileno = fileno;
        this.token = token;
        this.type = type;
        this.rc = rc;
        this.atime = atime;
        this.mtime = mtime;
        this.ctime = ctime;
        this.btime = btime;
        this.num_attrs = num_attrs;
    }
}
