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

//Information struct for H5Pget_mdc_config/H5Pset_mdc_config
public class H5AC_cache_config_t implements Serializable{
    private static final long serialVersionUID = -6748085696476149972L;
    // general configuration fields:
    public int              version;
    public boolean          rpt_fcn_enabled;
    public boolean          open_trace_file;
    public boolean          close_trace_file;
    public String           trace_file_name;
    public boolean          evictions_enabled;
    public boolean          set_initial_size;
    public long             initial_size;
    public double           min_clean_fraction;
    public long             max_size;
    public long             min_size;
    public long             epoch_length;
    // size increase control fields:
    public int              incr_mode;  // H5C_cache_incr_mode
    public double           lower_hr_threshold;
    public double           increment;
    public boolean          apply_max_increment;
    public long             max_increment;
    public int              flash_incr_mode;    // H5C_cache_flash_incr_mode
    public double           flash_multiple;
    public double           flash_threshold;
    // size decrease control fields:
    public int              decr_mode;  // H5C_cache_decr_mode
    public double           upper_hr_threshold;
    public double           decrement;
    public boolean          apply_max_decrement;
    public long             max_decrement;
    public int              epochs_before_eviction;
    public boolean          apply_empty_reserve;
    public double           empty_reserve;
    // parallel configuration fields:
    public long             dirty_bytes_threshold;
    public int              metadata_write_strategy;

    public H5AC_cache_config_t (int version, boolean rpt_fcn_enabled, boolean open_trace_file,
            boolean close_trace_file, String trace_file_name, boolean evictions_enabled,
            boolean set_initial_size, long initial_size, double min_clean_fraction, long max_size,
            long min_size, long epoch_length, int incr_mode, double lower_hr_threshold,
            double increment, boolean apply_max_increment, long max_increment, int flash_incr_mode,
            double flash_multiple, double flash_threshold, int decr_mode, double upper_hr_threshold,
            double decrement, boolean apply_max_decrement, long max_decrement,
            int epochs_before_eviction, boolean apply_empty_reserve, double empty_reserve,
            long dirty_bytes_threshold, int metadata_write_strategy)
    {
        this.version = version;
        this.rpt_fcn_enabled = rpt_fcn_enabled;
        this.open_trace_file = open_trace_file;
        this.close_trace_file = close_trace_file;
        this.trace_file_name = trace_file_name;
        this.evictions_enabled = evictions_enabled;
        this.set_initial_size = set_initial_size;
        this.initial_size = initial_size;
        this.min_clean_fraction = min_clean_fraction;
        this.max_size = max_size;
        this.min_size = min_size;
        this.epoch_length = epoch_length;
        this.incr_mode = incr_mode;
        this.lower_hr_threshold = lower_hr_threshold;
        this.increment = increment;
        this.apply_max_increment = apply_max_increment;
        this.max_increment = flash_incr_mode;
        this.flash_incr_mode = flash_incr_mode;
        this.flash_multiple = flash_multiple;
        this.flash_threshold = flash_threshold;
        this.decr_mode = decr_mode;
        this.upper_hr_threshold = upper_hr_threshold;
        this.decrement = decrement;
        this.apply_max_decrement = apply_max_decrement;
        this.max_decrement = max_decrement;
        this.epochs_before_eviction = epochs_before_eviction;
        this.apply_empty_reserve = apply_empty_reserve;
        this.empty_reserve = empty_reserve;
        this.dirty_bytes_threshold = dirty_bytes_threshold;
        this.metadata_write_strategy = metadata_write_strategy;
    }
}
