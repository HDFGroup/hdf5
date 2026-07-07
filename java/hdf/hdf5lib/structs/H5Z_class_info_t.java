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

/**
 * Registry-level information about a filter (returned by H5Zget_filter_class_info).
 */
public class H5Z_class_info_t implements Serializable {
    private static final long serialVersionUID = 1L;
    /** Numeric filter identifier */
    public int id;
    /** Bitwise OR of H5Z_FILTER_CONFIG_ENCODE_ENABLED / H5Z_FILTER_CONFIG_DECODE_ENABLED */
    public int config_flags;
    /** Canonical name; null for class-2 (legacy) plugins */
    public String name;
    /** Human-readable description; null if not provided */
    public String description;
    /** True if the plugin implements the set_config callback */
    public boolean has_set_config;
    /** True if the plugin implements the get_config callback */
    public boolean has_get_config;
    /** True if the plugin implements the write_blob callback (reserved for future use) */
    public boolean has_blob_callbacks;

    public H5Z_class_info_t(int id, int config_flags, String name, String description, boolean has_set_config,
                            boolean has_get_config, boolean has_blob_callbacks)
    {
        this.id                 = id;
        this.config_flags       = config_flags;
        this.name               = name;
        this.description        = description;
        this.has_set_config     = has_set_config;
        this.has_get_config     = has_get_config;
        this.has_blob_callbacks = has_blob_callbacks;
    }
}
