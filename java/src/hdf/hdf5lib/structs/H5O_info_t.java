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
    public long   fileno;
    /** Object address in file */
    public long   addr;
    /** Basic object type (group, dataset, etc.) */
    public int    type;
    /** Reference count of object */
    public int    rc;
    /** Access time */
    public long   atime;
    /** Modification time */
    public long   mtime;
    /** Change time */
    public long   ctime;
    /** Birth time */
    public long   btime;
    /** # of attributes attached to object */
    public long   num_attrs;
    /** Object header information */
    public H5O_hdr_info_t   hdr;
    /** Extra metadata storage for obj
     * v1/v2 B-tree and local/fractal heap for groups, B-tree for chunked datasets
     */
    public H5_ih_info_t     meta_size_obj;
    /** Extra metadata storage for attributes
     * v2 B-tree and heap for attributes
     */
    public H5_ih_info_t     meta_size_attr;

    /** Constructor for data model information struct for objects
     *
     * @param fileno: File number that object is located in
     * @param addr: Object address in file
     * @param type: Basic object type
     * @param rc: Reference count of object
     * @param num_attrs: Number of attributes attached to object
     * @param atime: Access time
     * @param mtime: Modification time
     * @param ctime: Change time
     * @param btime: Birth time
     * @param hdr: Object header information
     * @param meta_size_obj: v1/v2 B-tree and local/fractal heap for groups, B-tree for chunked datasets
     * @param meta_size_attr: v2 B-tree and heap for attributes
     */
    public H5O_info_t (long fileno, long addr, int type,
        int rc, long num_attrs, long atime, long mtime, long ctime, long btime,
        H5O_hdr_info_t hdr, H5_ih_info_t meta_size_obj, H5_ih_info_t meta_size_attr)
    {
        this.fileno = fileno;
        this.addr = addr;
        this.type = type;
        this.rc = rc;
        this.num_attrs = num_attrs;
        this.atime = atime;
        this.mtime = mtime;
        this.ctime = ctime;
        this.btime = btime;
        this.hdr = hdr;
        this.meta_size_obj = meta_size_obj;
        this.meta_size_attr = meta_size_attr;
    }
}
