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

// Information struct for object header metadata (for H5Oget_info/H5Oget_info_by_name/H5Oget_info_by_idx)
public class H5O_hdr_info_t implements Serializable{
    private static final long serialVersionUID = 7883826382952577189L;
    public int version;       /* Version number of header format in file */
    public int nmesgs;        /* Number of object header messages */
    public int nchunks;       /* Number of object header chunks */
    public int flags;         /* Object header status flags */
    public long space_total;  /* Total space for storing object header in file */
    public long space_meta;   /* Space within header for object header metadata information */
    public long space_mesg;   /* Space within header for actual message information */
    public long space_free;   /* Free space within object header */
    public long mesg_present; /* Flags to indicate presence of message type in header */
    public long mesg_shared;  /* Flags to indicate message type is shared in header */

    H5O_hdr_info_t (int version, int nmesgs, int nchunks, int flags,
        long space_total, long space_meta, long space_mesg, long space_free,
        long mesg_present, long mesg_shared)
    {
        this.version = version;
        this.nmesgs = nmesgs;
        this.nchunks = nchunks;
        this.flags = flags;
        this.space_total = space_total;
        this.space_meta = space_meta;
        this.space_mesg = space_mesg;
        this.space_free = space_free;
        this.mesg_present = mesg_present;
        this.mesg_shared = mesg_shared;
    }
}
