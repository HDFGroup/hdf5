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
 * Information struct for object (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx)
 *
 */
public class H5O_info_t implements Serializable {
    private static final long serialVersionUID = 4691681163544054518L;
    /** File number that object is located in */
    public long fileno;
    /** Object token in file */
    public H5O_token_t token;
    /** Basic object type (group, dataset, etc.) */
    public int type;
    /** Reference count of object */
    public int rc;
    /** Access time */
    public long atime;
    /** Modification time */
    public long mtime;
    /** Change time */
    public long ctime;
    /** Birth time */
    public long btime;
    /** Number of attributes attached to object */
    public long num_attrs;

    /**
     * Constructor for data model information struct for objects
     *
     * @param fileno: File number that object is located in
     * @param token: Object token in file
     * @param type: Basic object type
     * @param rc: Reference count of object
     * @param atime: Access time
     * @param mtime: Modification time
     * @param ctime: Change time
     * @param btime: Birth time
     * @param num_attrs: Number of attributes attached to object
     */
    public H5O_info_t(long fileno, H5O_token_t token, int type, int rc, long atime, long mtime, long ctime,
                      long btime, long num_attrs)
    {
        this.fileno    = fileno;
        this.token     = token;
        this.type      = type;
        this.rc        = rc;
        this.atime     = atime;
        this.mtime     = mtime;
        this.ctime     = ctime;
        this.btime     = btime;
        this.num_attrs = num_attrs;
    }
}
