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
import java.lang.foreign.Arena;
import java.lang.foreign.MemoryLayout;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.SequenceLayout;
import java.lang.foreign.SymbolLookup;
import java.lang.foreign.ValueLayout;

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
    public hdf.hdf5lib.structs.H5_ih_info_t sohm_msgs_info;

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
                       hdf.hdf5lib.structs.H5_ih_info_t sohm_msgs_info)
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
    /**
     * Constructor for current "global" information about file
     * @param info_segment: Memory segment for H5F_info2_t
     */
    public H5F_info2_t(MemorySegment info_segment)
    {
        MemorySegment seg = info_segment.reinterpret(MemoryLayout.ofStruct(
            ValueLayout.JAVA_INT.withName("super_version"),
            ValueLayout.JAVA_LONG.withName("super_size"),
            ValueLayout.JAVA_LONG.withName("super_ext_size"),
            ValueLayout.JAVA_INT.withName("free_version"),
            ValueLayout.JAVA_LONG.withName("free_meta_size"),
            ValueLayout.JAVA_LONG.withName("free_tot_space"),
            ValueLayout.JAVA_INT.withName("sohm_version"),
            ValueLayout.JAVA_LONG.withName("sohm_hdr_size"),
            org.hdfgroup.javahdf5.H5_ih_info_t.h5_ih_info_t_layout.withName("sohm_msgs_info")
        ).byteSize(), 0);

        this.super_version  = (int)seg.get(ValueLayout.JAVA_INT, seg.layout().indexOf("super_version").offset());
        this.super_size     = seg.get(ValueLayout.JAVA_LONG, seg.layout().indexOf("super_size").offset());
        this.super_ext_size = seg.get(ValueLayout.JAVA_LONG, seg.layout().indexOf("super_ext_size").offset());
        this.free_version   = (int)seg.get(ValueLayout.JAVA_INT, seg.layout().indexOf("free_version").offset());
        this.free_meta_size = seg.get(ValueLayout.JAVA_LONG, seg.layout().indexOf("free_meta_size").offset());
        this.free_tot_space = seg.get(ValueLayout.JAVA_LONG, seg.layout().indexOf("free_tot_space").offset());
        this.sohm_version   = (int)seg.get(ValueLayout.JAVA_INT, seg.layout().indexOf("sohm_version").offset());
        this.sohm_hdr_size  = seg.get(ValueLayout.JAVA_LONG, seg.layout().indexOf("sohm_hdr_size").offset());
        MemorySegment sohm_msgs_info_seg = seg.asSlice(seg.layout().indexOf("sohm_msgs_info").offset(),
                org.hdfgroup.javahdf5.H5_ih_info_t.h5_ih_info_t_layout.byteSize());
        this.sohm_msgs_info = new hdf.hdf5lib.structs.H5_ih_info_t(sohm_msgs_info_seg);
    }
    // Unpack the H5F_info2_t from the MemorySegment
    MemorySegment super_segment   = org.hdfgroup.javahdf5.H5F_info2_t.super_(finfo_segment);
    MemorySegment free_segment    = org.hdfgroup.javahdf5.H5F_info2_t.free(finfo_segment);
    MemorySegment sohm_segment    = org.hdfgroup.javahdf5.H5F_info2_t.sohm(finfo_segment);
    MemorySegment sohm_ih_segment = org.hdfgroup.javahdf5.H5F_info2_t.sohm.msgs_info(sohm_segment);
    hdf.hdf5lib.structs.H5_ih_info_t sizes = new hdf.hdf5lib.structs.H5_ih_info_t(
        org.hdfgroup.javahdf5.H5_ih_info_t.index_size(sohm_ih_segment),
        org.hdfgroup.javahdf5.H5_ih_info_t.heap_size(sohm_ih_segment));
    info = new hdf.hdf5lib.structs.H5F_info2_t(
        org.hdfgroup.javahdf5.H5F_info2_t.super_.version(super_segment),
        org.hdfgroup.javahdf5.H5F_info2_t.super_.super_size(super_segment),
        org.hdfgroup.javahdf5.H5F_info2_t.super_.super_ext_size(super_segment),
        org.hdfgroup.javahdf5.H5F_info2_t.free.version(free_segment),
        org.hdfgroup.javahdf5.H5F_info2_t.free.meta_size(free_segment),
        org.hdfgroup.javahdf5.H5F_info2_t.free.tot_space(free_segment),
        org.hdfgroup.javahdf5.H5F_info2_t.sohm.version(sohm_segment),
        org.hdfgroup.javahdf5.H5F_info2_t.sohm.hdr_size(sohm_segment), sizes);
}
