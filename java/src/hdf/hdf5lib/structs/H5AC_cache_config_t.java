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
 * Information struct for H5Pget_mdc_config/H5Pset_mdc_config
 *
 */
public class H5AC_cache_config_t implements Serializable {
    private static final long serialVersionUID = -6748085696476149972L;
    // general configuration fields
    /**
     *  version: Integer field containing the version number of this version
     *      of the H5AC_cache_config_t structure.  Any instance of
     *      H5AC_cache_config_t passed to the cache must have a known
     *      version number, or an error will be flagged.
     */
    public int version;
    /**
     * rpt_fcn_enabled: Boolean field used to enable and disable the default
     *    reporting function.  This function is invoked every time the
     *    automatic cache resize code is run, and reports on its activities.
     *
     *    This is a debugging function, and should normally be turned off.
     */
    public boolean rpt_fcn_enabled;
    /**
     * open_trace_file: Boolean field indicating whether the trace_file_name
     *     field should be used to open a trace file for the cache.
     *
     *      *** DEPRECATED *** Use H5Fstart/stop logging functions instead
     */
    public boolean open_trace_file;
    /**
     * close_trace_file: Boolean field indicating whether the current trace
     *     file (if any) should be closed.
     *
     *      *** DEPRECATED *** Use H5Fstart/stop logging functions instead
     */
    public boolean close_trace_file;
    /**
     * trace_file_name: Full path of the trace file to be opened if the
     *     open_trace_file field is TRUE.
     *
     *      *** DEPRECATED *** Use H5Fstart/stop logging functions instead
     */
    public String trace_file_name;
    /**
     * evictions_enabled:  Boolean field used to either report the current
     *     evictions enabled status of the cache, or to set the cache's
     *    evictions enabled status.
     */
    public boolean evictions_enabled;
    /**
     * set_initial_size: Boolean flag indicating whether the size of the
     *      initial size of the cache is to be set to the value given in
     *      the initial_size field.  If set_initial_size is FALSE, the
     *      initial_size field is ignored.
     */
    public boolean set_initial_size;
    /**
     * initial_size: If enabled, this field contain the size the cache is
     *      to be set to upon receipt of this structure.  Needless to say,
     *      initial_size must lie in the closed interval [min_size, max_size].
     */
    public long initial_size;
    /**
     * min_clean_fraction: double in the range 0 to 1 indicating the fraction
     *      of the cache that is to be kept clean.  This field is only used
     *      in parallel mode.  Typical values are 0.1 to 0.5.
     */
    public double min_clean_fraction;
    /**
     * max_size: Maximum size to which the cache can be adjusted.  The
     *      supplied value must fall in the closed interval
     *      [MIN_MAX_CACHE_SIZE, MAX_MAX_CACHE_SIZE].  Also, max_size must
     *      be greater than or equal to min_size.
     */
    public long max_size;
    /**
     * min_size: Minimum size to which the cache can be adjusted.  The
     *      supplied value must fall in the closed interval
     *      [H5C__MIN_MAX_CACHE_SIZE, H5C__MAX_MAX_CACHE_SIZE].  Also, min_size
     *      must be less than or equal to max_size.
     */
    public long min_size;
    /**
     * epoch_length: Number of accesses on the cache over which to collect
     *      hit rate stats before running the automatic cache resize code,
     *      if it is enabled.
     */
    public long epoch_length;
    // size increase control fields
    /**
     * incr_mode: Instance of the H5C_cache_incr_mode enumerated type whose
     *      value indicates how we determine whether the cache size should be
     *      increased.  At present there are two possible values.
     */
    public int incr_mode;
    /**
     * lower_hr_threshold: Lower hit rate threshold.  If the increment mode
     *      (incr_mode) is H5C_incr__threshold and the hit rate drops below the
     *      value supplied in this field in an epoch, increment the cache size by
     *      size_increment.  Note that cache size may not be incremented above
     *      max_size, and that the increment may be further restricted by the
     *      max_increment field if it is enabled.
     */
    public double lower_hr_threshold;
    /**
     * increment:  Double containing the multiplier used to derive the new
     *      cache size from the old if a cache size increment is triggered.
     *      The increment must be greater than 1.0, and should not exceed 2.0.
     */
    public double increment;
    /**
     * apply_max_increment:  Boolean flag indicating whether the max_increment
     *      field should be used to limit the maximum cache size increment.
     */
    public boolean apply_max_increment;
    /**
     * max_increment: If enabled by the apply_max_increment field described
     *      above, this field contains the maximum number of bytes by which the
     *      cache size can be increased in a single re-size.
     */
    public long max_increment;
    /**
     * flash_incr_mode:  Instance of the H5C_cache_flash_incr_mode enumerated
     *      type whose value indicates whether and by which algorithm we should
     *      make flash increases in the size of the cache to accommodate insertion
     *      of large entries and large increases in the size of a single entry.
     */
    public int flash_incr_mode;
    /**
     * flash_multiple: Double containing the multiple described above in the
     *      H5C_flash_incr__add_space section of the discussion of the
     *      flash_incr_mode section.  This field is ignored unless flash_incr_mode
     *      is H5C_flash_incr__add_space.
     */
    public double flash_multiple;
    /**
     * flash_threshold: Double containing the factor by which current max cache
     *      size is multiplied to obtain the size threshold for the add_space flash
     *      increment algorithm.  The field is ignored unless flash_incr_mode is
     *      H5C_flash_incr__add_space.
     */
    public double flash_threshold;
    // size decrease control fields
    /**
     * decr_mode: Instance of the H5C_cache_decr_mode enumerated type whose
     *      value indicates how we determine whether the cache size should be
     *      decreased.  At present there are four possibilities.
     */
    public int decr_mode;
    /**
     * upper_hr_threshold: Upper hit rate threshold.  The use of this field
     *      varies according to the current decr_mode.
     */
    public double upper_hr_threshold;
    /**
     * decrement: This field is only used when the decr_mode is
     *      H5C_decr__threshold.
     */
    public double decrement;
    /**
     * apply_max_decrement:  Boolean flag used to determine whether decrements
     *      in cache size are to be limited by the max_decrement field.
     */
    public boolean apply_max_decrement;
    /**
     * max_decrement: Maximum number of bytes by which the cache size can be
     *      decreased in a single re-size.  Note that decrements may also be
     *      restricted by the min_size of the cache, and (in age out modes) by
     *      the empty_reserve field.
     */
    public long max_decrement;
    /**
     * epochs_before_eviction:  Integer field used in H5C_decr__age_out and
     *      H5C_decr__age_out_with_threshold decrement modes.
     */
    public int epochs_before_eviction;
    /**
     * apply_empty_reserve:  Boolean field controlling whether the empty_reserve
     *      field is to be used in computing the new cache size when the
     *      decr_mode is H5C_decr__age_out or H5C_decr__age_out_with_threshold.
     */
    public boolean apply_empty_reserve;
    /**
     * empty_reserve:  To avoid a constant racheting down of cache size by small
     *      amounts in the H5C_decr__age_out and H5C_decr__age_out_with_threshold
     *      modes, this field allows one to require that any cache size
     *      reductions leave the specified fraction of unused space in the cache.
     */
    public double empty_reserve;
    // parallel configuration fields
    /**
     * dirty_bytes_threshold:  Threshold of dirty byte creation used to
     *     synchronize updates between caches.
     */
    public long dirty_bytes_threshold;
    /**
     * metadata_write_strategy: Integer field containing a code indicating the
     *    desired metadata write strategy.
     */
    public int metadata_write_strategy;

    /**
     * H5AC_cache_config_t is a public structure intended for use in public APIs.
     * At least in its initial incarnation, it is basically a copy of struct
     * H5C_auto_size_ctl_t, minus the report_fcn field, and plus the
     * dirty_bytes_threshold field.
     *
     * @param version: Integer field containing the version number of this version
     * @param rpt_fcn_enabled: Boolean field used to enable and disable the default reporting function.
     * @param open_trace_file: Boolean field indicating whether the trace_file_name
     *     field should be used to open a trace file for the cache.
     * @param close_trace_file: Boolean field indicating whether the current trace
     *     file (if any) should be closed.
     * @param trace_file_name: Full path of the trace file to be opened if the
     *     open_trace_file field is TRUE.
     * @param evictions_enabled:  Boolean field used to either report or set the current
     *     evictions enabled status of the cache.
     * @param set_initial_size: Boolean flag indicating whether the size of the
     *      initial size of the cache is to be set to the value given in
     *      the initial_size field.
     * @param initial_size: If enabled, this field contain the size the cache is
     *      to be set to upon receipt of this structure.
     * @param min_clean_fraction: double in the range 0 to 1 indicating the fraction
     *      of the cache that is to be kept clean.
     * @param max_size: Maximum size to which the cache can be adjusted.
     * @param min_size: Minimum size to which the cache can be adjusted.
     * @param epoch_length: Number of accesses on the cache over which to collect
     *      hit rate stats before running the automatic cache resize code.
     * @param incr_mode: Instance of the H5C_cache_incr_mode enumerated type.
     * @param lower_hr_threshold: Lower hit rate threshold.
     * @param increment:  Double containing the multiplier used to derive the new
     *      cache size from the old if a cache size increment is triggered.
     * @param apply_max_increment:  Boolean flag indicating whether the max_increment
     *      field should be used to limit the maximum cache size increment.
     * @param max_increment: If enabled by the apply_max_increment field described
     *      above, this field contains the maximum number of bytes by which the
     *      cache size can be increased in a single re-size.
     * @param flash_incr_mode:  Instance of the H5C_cache_flash_incr_mode enumerated
     *      type whose value indicates whether and by which algorithm we should
     *      make flash increases in the size of the cache to accommodate insertion
     *      of large entries and large increases in the size of a single entry.
     * @param flash_multiple: Double containing the multiple described above in the
     *      H5C_flash_incr__add_space section of the discussion of the
     *      flash_incr_mode section.
     * @param flash_threshold: Double containing the factor by which current max cache
     *      size is multiplied to obtain the size threshold for the add_space flash
     *      increment algorithm.
     * @param decr_mode: Instance of the H5C_cache_decr_mode enumerated type whose
     *      value indicates how we determine whether the cache size should be
     *      decreased.
     * @param upper_hr_threshold: Upper hit rate threshold.  The use of this field
     *      varies according to the current decr_mode.
     * @param decrement: This field is only used when the decr_mode is
     *      H5C_decr__threshold.
     * @param apply_max_decrement:  Boolean flag used to determine whether decrements
     *      in cache size are to be limited by the max_decrement field.
     * @param max_decrement: Maximum number of bytes by which the cache size can be
     *      decreased in a single re-size.
     * @param epochs_before_eviction:  Integer field used in H5C_decr__age_out and
     *      H5C_decr__age_out_with_threshold decrement modes.
     * @param apply_empty_reserve:  Boolean field controlling whether the empty_reserve
     *      field is to be used in computing the new cache size when the
     *      decr_mode is H5C_decr__age_out or H5C_decr__age_out_with_threshold.
     * @param empty_reserve:  To avoid a constant racheting down of cache size by small
     *      amounts in the H5C_decr__age_out and H5C_decr__age_out_with_threshold
     *      modes.
     * @param dirty_bytes_threshold:  Threshold of dirty byte creation used to
     *     synchronize updates between caches.
     * @param metadata_write_strategy: Integer field containing a code indicating the
     *    desired metadata write strategy.
     */
    public H5AC_cache_config_t(int version, boolean rpt_fcn_enabled, boolean open_trace_file,
                               boolean close_trace_file, String trace_file_name, boolean evictions_enabled,
                               boolean set_initial_size, long initial_size, double min_clean_fraction,
                               long max_size, long min_size, long epoch_length, int incr_mode,
                               double lower_hr_threshold, double increment, boolean apply_max_increment,
                               long max_increment, int flash_incr_mode, double flash_multiple,
                               double flash_threshold, int decr_mode, double upper_hr_threshold,
                               double decrement, boolean apply_max_decrement, long max_decrement,
                               int epochs_before_eviction, boolean apply_empty_reserve, double empty_reserve,
                               long dirty_bytes_threshold, int metadata_write_strategy)
    {
        this.version                 = version;
        this.rpt_fcn_enabled         = rpt_fcn_enabled;
        this.open_trace_file         = open_trace_file;
        this.close_trace_file        = close_trace_file;
        this.trace_file_name         = trace_file_name;
        this.evictions_enabled       = evictions_enabled;
        this.set_initial_size        = set_initial_size;
        this.initial_size            = initial_size;
        this.min_clean_fraction      = min_clean_fraction;
        this.max_size                = max_size;
        this.min_size                = min_size;
        this.epoch_length            = epoch_length;
        this.incr_mode               = incr_mode;
        this.lower_hr_threshold      = lower_hr_threshold;
        this.increment               = increment;
        this.apply_max_increment     = apply_max_increment;
        this.max_increment           = flash_incr_mode;
        this.flash_incr_mode         = flash_incr_mode;
        this.flash_multiple          = flash_multiple;
        this.flash_threshold         = flash_threshold;
        this.decr_mode               = decr_mode;
        this.upper_hr_threshold      = upper_hr_threshold;
        this.decrement               = decrement;
        this.apply_max_decrement     = apply_max_decrement;
        this.max_decrement           = max_decrement;
        this.epochs_before_eviction  = epochs_before_eviction;
        this.apply_empty_reserve     = apply_empty_reserve;
        this.empty_reserve           = empty_reserve;
        this.dirty_bytes_threshold   = dirty_bytes_threshold;
        this.metadata_write_strategy = metadata_write_strategy;
    }
}
