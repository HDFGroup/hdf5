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
 * Information struct for object (for H5Fget_info)
 *
 */
public class H5F_info2_t implements Serializable {
    private static final long serialVersionUID = 4691681162544054518L;
    /** Superblock version number */
    public int super_version;
    /** Superblock size */
    public long super_size;
    /** Superblock extension size */
    public long super_ext_size;
    /** Version number of file free space management */
    public int free_version;
    /** Free space manager metadata size */
    public long free_meta_size;
    /** Amount of free space in the file */
    public long free_tot_space;
    /** Version number of shared object header info */
    public int sohm_version;
    /** Shared object header message header size */
    public long sohm_hdr_size;
    /** Shared object header message index and heap size */
    public H5_ih_info_t sohm_msgs_info;

    /**
     * Constructor for current "global" information about file
     * @param super_version: Superblock version number
     * @param super_size: Superblock size
     * @param super_ext_size: Superblock extension size
     * @param free_version: Version number of file free space management
     * @param free_meta_size: Free space manager metadata size
     * @param free_tot_space: Amount of free space in the file
     * @param sohm_version: Version number of shared object header info
     * @param sohm_hdr_size: Shared object header message header size
     * @param sohm_msgs_info: Shared object header message index and heap size
     */
    public H5F_info2_t(int super_version, long super_size, long super_ext_size, int free_version,
                       long free_meta_size, long free_tot_space, int sohm_version, long sohm_hdr_size,
                       H5_ih_info_t sohm_msgs_info)
    {
        this.super_version  = super_version;
        this.super_size     = super_size;
        this.super_ext_size = super_ext_size;
        this.free_version   = free_version;
        this.free_meta_size = free_meta_size;
        this.free_tot_space = free_tot_space;
        this.sohm_version   = sohm_version;
        this.sohm_hdr_size  = sohm_hdr_size;
        this.sohm_msgs_info = sohm_msgs_info;
    }
}
