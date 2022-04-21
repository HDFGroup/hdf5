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
 * Information struct for object header metadata (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx)
 *
 */
public class H5O_hdr_info_t implements Serializable {
    private static final long serialVersionUID = 7883826382952577189L;
    /** Version number of header format in file */
    public int version;
    /** Number of object header messages */
    public int nmesgs;
    /** Number of object header chunks */
    public int nchunks;
    /** Object header status flags */
    public int flags;
    /** Total space for storing object header in file */
    public long space_total;
    /** Space within header for object header metadata information */
    public long space_meta;
    /** Space within header for actual message information */
    public long space_mesg;
    /** Free space within object header */
    public long space_free;
    /** Flags to indicate presence of message type in header */
    public long mesg_present;
    /** Flags to indicate message type is shared in header */
    public long mesg_shared;

    H5O_hdr_info_t(int version, int nmesgs, int nchunks, int flags, long space_total, long space_meta,
                   long space_mesg, long space_free, long mesg_present, long mesg_shared)
    {
        this.version      = version;
        this.nmesgs       = nmesgs;
        this.nchunks      = nchunks;
        this.flags        = flags;
        this.space_total  = space_total;
        this.space_meta   = space_meta;
        this.space_mesg   = space_mesg;
        this.space_free   = space_free;
        this.mesg_present = mesg_present;
        this.mesg_shared  = mesg_shared;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
            return true;

        if (!(o instanceof H5O_hdr_info_t))
            return false;

        H5O_hdr_info_t info = (H5O_hdr_info_t)o;

        if (this.version != info.version)
            return false;
        if (this.nmesgs != info.nmesgs)
            return false;
        if (this.nchunks != info.nchunks)
            return false;
        if (this.flags != info.flags)
            return false;
        if (this.space_total != info.space_total)
            return false;
        if (this.space_meta != info.space_meta)
            return false;
        if (this.space_mesg != info.space_mesg)
            return false;
        if (this.space_free != info.space_free)
            return false;
        if (this.mesg_present != info.mesg_present)
            return false;
        if (this.mesg_shared != info.mesg_shared)
            return false;

        return true;
    }
}
