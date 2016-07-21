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

/* Programmer:  John Mainzer
 *              11/10/05
 *
 *		This file contains tests for the API calls associated
 *		with the cache implemented in H5C.c
 */

#include "h5test.h"
#include "H5Iprivate.h"
#include "H5ACprivate.h"
#include "cache_common.h"

/* extern declarations */

/* global variable declarations: */

/* macro definitions */

/* private function declarations: */

static hbool_t check_fapl_mdc_api_calls(void);
static hbool_t check_file_mdc_api_calls(void);
static hbool_t mdc_api_call_smoke_check(int express_test);
static H5AC_cache_config_t * init_invalid_configs(void);
static hbool_t check_fapl_mdc_api_errs(void);
static hbool_t check_file_mdc_api_errs(void);



/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    check_fapl_mdc_api_calls()
 *
 * Purpose:     Verify that the file access property list related
 *              metadata cache related API calls are functioning
 *              correctly.
 *
 *              Since we have tested the H5C code elsewhere, it should
 *              be sufficient to verify that the desired configuration
 *              data is getting to the cache.
 *
 * Return:      Test pass status (TRUE/FALSE)
 *
 * Programmer:  John Mainzer
 *              4/12/04
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
check_fapl_mdc_api_calls(void)
{
    char filename[512];
    herr_t result;
    hid_t fapl_id = -1;
    hid_t test_fapl_id = -1;
    hid_t file_id = -1;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    H5AC_cache_config_t default_config = H5AC__DEFAULT_CACHE_CONFIG;
    H5AC_cache_config_t mod_config =
    {
      /* int         version                = */ H5AC__CURR_CACHE_CONFIG_VERSION,
      /* hbool_t     rpt_fcn_enabled        = */ FALSE,
      /* hbool_t     open_trace_file        = */ FALSE,
      /* hbool_t     close_trace_file       = */ FALSE,
      /* char        trace_file_name[]      = */ "",
      /* hbool_t     evictions_enabled      = */ TRUE,
      /* hbool_t     set_initial_size       = */ TRUE,
      /* size_t      initial_size           = */ (1 * 1024 * 1024 + 1),
      /* double      min_clean_fraction     = */ 0.2f,
      /* size_t      max_size               = */ (16 * 1024 * 1024 + 1),
      /* size_t      min_size               = */ ( 1 * 1024 * 1024 + 1),
      /* long int    epoch_length           = */ 50001,
      /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,
      /* double      lower_hr_threshold     = */ 0.91f,
      /* double      increment              = */ 2.1f,
      /* hbool_t     apply_max_increment    = */ TRUE,
      /* size_t      max_increment          = */ (4 * 1024 * 1024 + 1),
      /* enum H5C_cache_flash_incr_mode       */
      /*                    flash_incr_mode = */ H5C_flash_incr__off,
      /* double      flash_multiple         = */ 2.0f,
      /* double      flash_threshold        = */ 0.5f,
      /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__age_out,
      /* double      upper_hr_threshold     = */ 0.998f,
      /* double      decrement              = */ 0.91f,
      /* hbool_t     apply_max_decrement    = */ TRUE,
      /* size_t      max_decrement          = */ (1 * 1024 * 1024 - 1),
      /* int         epochs_before_eviction = */ 4,
      /* hbool_t     apply_empty_reserve    = */ TRUE,
      /* double      empty_reserve          = */ 0.05f,
      /* int         dirty_bytes_threshold  = */ (256 * 1024),
      /* int	    metadata_write_strategy = */
					H5AC__DEFAULT_METADATA_WRITE_STRATEGY
    };
    H5AC_cache_config_t scratch;
    H5C_auto_size_ctl_t default_auto_size_ctl;
    H5C_auto_size_ctl_t mod_auto_size_ctl;

    TESTING("MDC/FAPL related API calls");

    pass = TRUE;

    XLATE_EXT_TO_INT_MDC_CONFIG(default_auto_size_ctl, default_config)
    XLATE_EXT_TO_INT_MDC_CONFIG(mod_auto_size_ctl, mod_config)

    /* Create a FAPL and verify that it contains the default
     * initial mdc configuration
     */

    if ( pass ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        }
    }

    if ( pass ) {

        scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;

        result = H5Pget_mdc_config(fapl_id, &scratch);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_config() failed.\n";

        } else if (!CACHE_CONFIGS_EQUAL(default_config, scratch, TRUE, TRUE)) {

            pass = FALSE;
            failure_mssg = "retrieved config doesn't match default.";
        }
    }


    /* Modify the initial mdc configuration in a FAPL, and verify that
     * the changes can be read back
     */

    if ( pass ) {

        result = H5Pset_mdc_config(fapl_id, &mod_config);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_mdc_config() failed.\n";
        }
    }

    if ( pass ) {

        scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;

        result = H5Pget_mdc_config(fapl_id, &scratch);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_config() failed.\n";

        } else if ( ! CACHE_CONFIGS_EQUAL(mod_config, scratch, TRUE, TRUE) ) {

            pass = FALSE;
            failure_mssg = "retrieved config doesn't match mod config.";
        }
    }

    if ( pass ) {

        if ( H5Pclose(fapl_id) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pclose() failed.\n";
        }
    }

    /* Open a file using the default FAPL.  Verify that the resulting
     * metadata cache uses the default configuration as well.  Get a
     * copy of the FAPL from the file, and verify that it contains the
     * default initial meta data cache configuration.  Close and delete
     * the file.
     */

    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    /* create the file using the default FAPL */
    if ( pass ) {

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

        if ( file_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fcreate() failed.\n";
        }
    }

    /* get a pointer to the files internal data structure */
    if ( pass ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "Can't get file_ptr.\n";

        } else {

            cache_ptr = file_ptr->shared->cache;
        }
    }

    /* verify that we can access the internal version of the cache config */
    if ( pass ) {

        if ( ( cache_ptr == NULL ) ||
             ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ||
             ( cache_ptr->resize_ctl.version != H5C__CURR_AUTO_SIZE_CTL_VER ) ){

            pass = FALSE;
            failure_mssg = "Can't access cache resize_ctl.\n";
        }
    }

    /* conpare the cache's internal configuration with the expected value */
    if ( pass ) {

        if ( ! resize_configs_are_equal(&default_auto_size_ctl, \
                                        &cache_ptr->resize_ctl, TRUE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected value(s) in cache resize_ctl 1.\n";
        }
    }

    /* get a copy of the files FAPL */
    if ( pass ) {

        fapl_id = H5Fget_access_plist(file_id);

        if ( fapl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_access_plist() failed.\n";
        }
    }

    /* compare the initial cache config from the copy of the file's FAPL
     * to the expected value.  If all goes well, close the copy of the FAPL.
     */
    if ( pass ) {

        scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;

        result = H5Pget_mdc_config(fapl_id, &scratch);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_config() failed.\n";

        } else if (!CACHE_CONFIGS_EQUAL(default_config, scratch, TRUE, TRUE)) {

            pass = FALSE;
            failure_mssg = "config retrieved from file doesn't match default.";

        } else if ( H5Pclose(fapl_id) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pclose() failed.\n";
        }
    }

    /* close the file and delete it */
    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        } else if ( HDremove(filename) < 0 ) {

            pass = FALSE;
            failure_mssg = "HDremove() failed.\n";
        }
    }


    /* Open a file using a FAPL with a modified initial metadata cache
     * configuration.  Verify that the resulting metadata cache uses the
     * modified configuration as well.  Get a copy of the FAPL from the
     * file, and verify that it contains the modified initial meta data
     * cache configuration.  Close and delete the file.
     */

    /* Create a FAPL */
    if ( pass ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        }
    }

    /* Modify the initial mdc configuration in the FAPL. */

    if ( pass ) {

        result = H5Pset_mdc_config(fapl_id, &mod_config);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_mdc_config() failed.\n";
        }
    }

    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    /* create the file using the modified FAPL */
    if ( pass ) {

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

        if ( file_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fcreate() failed.\n";
        }
    }

    /* get a pointer to the files internal data structure */
    if ( pass ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "Can't get file_ptr.\n";

        } else {

            cache_ptr = file_ptr->shared->cache;
        }
    }

    /* verify that we can access the internal version of the cache config */
    if ( pass ) {

        if ( ( cache_ptr == NULL ) ||
             ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ||
             ( cache_ptr->resize_ctl.version != H5C__CURR_AUTO_SIZE_CTL_VER ) ){

            pass = FALSE;
            failure_mssg = "Can't access cache resize_ctl.\n";
        }
    }

    /* conpare the cache's internal configuration with the expected value */
    if ( pass ) {

        if ( ! resize_configs_are_equal(&mod_auto_size_ctl, \
                                        &cache_ptr->resize_ctl, TRUE) ) {

            pass = FALSE;
            failure_mssg = "Unexpected value(s) in cache resize_ctl 2.\n";
        }
    }

    /* get a copy of the files FAPL */
    if ( pass ) {

        test_fapl_id = H5Fget_access_plist(file_id);

        if ( test_fapl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_access_plist() failed.\n";
        }
    }

    /* compare the initial cache config from the copy of the file's FAPL
     * to the expected value.  If all goes well, close the copy of the FAPL.
     */
    if ( pass ) {

        scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;

        result = H5Pget_mdc_config(test_fapl_id, &scratch);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_config() failed.\n";

        } else if ( ! CACHE_CONFIGS_EQUAL(mod_config, scratch, TRUE, TRUE) ) {

            pass = FALSE;
            failure_mssg = "config retrieved from file doesn't match.";

        } else if ( H5Pclose(test_fapl_id) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pclose() failed.\n";
        }
    }

    /* close the fapl used to create the file */
    if ( pass ) {

        if ( H5Pclose(fapl_id) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pclose() failed.\n";
        }
    }

    /* close the file and delete it */
    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        } else if ( HDremove(filename) < 0 ) {

            pass = FALSE;
            failure_mssg = "HDremove() failed.\n";
        }
    }

    if ( pass ) {

        PASSED();

    } else {

        H5_FAILED();
    }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", FUNC, failure_mssg);
    }

    return pass;

} /* check_fapl_mdc_api_calls() */


/*-------------------------------------------------------------------------
 * Function:    check_file_mdc_api_calls()
 *
 * Purpose:     Verify that the file related metadata cache API calls are
 *              functioning correctly.
 *
 *              Since we have tested the H5C code elsewhere, it should
 *              be sufficient to verify that the desired configuration
 *              data is getting in and out of the cache.  Similarly,
 *              we need only verify that the cache monitoring calls
 *              return the data that the cache thinks they should return.
 *              We shouldn't need to verify data correctness beyond that
 *              point.
 *
 * Return:      Test pass status (TRUE/FALSE)
 *
 * Programmer:  John Mainzer
 *              4/14/04
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
check_file_mdc_api_calls(void)
{
    char filename[512];
    hid_t file_id = -1;
    size_t max_size;
    size_t min_clean_size;
    size_t cur_size;
    int cur_num_entries;
    double hit_rate;
    H5AC_cache_config_t default_config = H5AC__DEFAULT_CACHE_CONFIG;
    H5AC_cache_config_t mod_config_1 =
    {
      /* int         version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
      /* hbool_t     rpt_fcn_enabled        = */ FALSE,
      /* hbool_t     open_trace_file        = */ FALSE,
      /* hbool_t     close_trace_file       = */ FALSE,
      /* char        trace_file_name[]      = */ "",
      /* hbool_t     evictions_enabled      = */ TRUE,
      /* hbool_t     set_initial_size       = */ TRUE,
      /* size_t      initial_size           = */ (1 * 1024 * 1024 + 1),
      /* double      min_clean_fraction     = */ 0.2f,
      /* size_t      max_size               = */ (16 * 1024 * 1024 + 1),
      /* size_t      min_size               = */ ( 1 * 1024 * 1024 + 1),
      /* long int    epoch_length           = */ 50001,
      /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,
      /* double      lower_hr_threshold     = */ 0.91f,
      /* double      increment              = */ 2.1f,
      /* hbool_t     apply_max_increment    = */ TRUE,
      /* size_t      max_increment          = */ (4 * 1024 * 1024 + 1),
      /* enum H5C_cache_flash_incr_mode       */
      /*                    flash_incr_mode = */ H5C_flash_incr__off,
      /* double      flash_multiple         = */ 2.0f,
      /* double      flash_threshold        = */ 0.5f,
      /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__age_out,
      /* double      upper_hr_threshold     = */ 0.998f,
      /* double      decrement              = */ 0.91f,
      /* hbool_t     apply_max_decrement    = */ TRUE,
      /* size_t      max_decrement          = */ (1 * 1024 * 1024 - 1),
      /* int         epochs_before_eviction = */ 4,
      /* hbool_t     apply_empty_reserve    = */ TRUE,
      /* double      empty_reserve          = */ 0.05f,
      /* int         dirty_bytes_threshold  = */ (256 * 1024),
      /* int	    metadata_write_strategy = */
					H5AC__DEFAULT_METADATA_WRITE_STRATEGY
    };
    H5AC_cache_config_t mod_config_2 =
    {
      /* int         version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
      /* hbool_t     rpt_fcn_enabled        = */ TRUE,
      /* hbool_t     open_trace_file        = */ FALSE,
      /* hbool_t     close_trace_file       = */ FALSE,
      /* char        trace_file_name[]      = */ "",
      /* hbool_t     evictions_enabled      = */ TRUE,
      /* hbool_t     set_initial_size       = */ TRUE,
      /* size_t      initial_size           = */ (512 * 1024),
      /* double      min_clean_fraction     = */ 0.1f,
      /* size_t      max_size               = */ ( 8 * 1024 * 1024),
      /* size_t      min_size               = */ (      512 * 1024),
      /* long int    epoch_length           = */ 25000,
      /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,
      /* double      lower_hr_threshold     = */ 0.9f,
      /* double      increment              = */ 2.0f,
      /* hbool_t     apply_max_increment    = */ TRUE,
      /* size_t      max_increment          = */ (2 * 1024 * 1024),
      /* enum H5C_cache_flash_incr_mode       */
      /*                    flash_incr_mode = */ H5C_flash_incr__off,
      /* double      flash_multiple         = */ 1.5f,
      /* double      flash_threshold        = */ 0.4f,
      /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__threshold,
      /* double      upper_hr_threshold     = */ 0.9995f,
      /* double      decrement              = */ 0.95f,
      /* hbool_t     apply_max_decrement    = */ TRUE,
      /* size_t      max_decrement          = */ (512 * 1024),
      /* int         epochs_before_eviction = */ 4,
      /* hbool_t     apply_empty_reserve    = */ TRUE,
      /* double      empty_reserve          = */ 0.05f,
      /* int         dirty_bytes_threshold  = */ (256 * 1024),
      /* int	    metadata_write_strategy = */
					H5AC__DEFAULT_METADATA_WRITE_STRATEGY
    };
    H5AC_cache_config_t mod_config_3 =
    {
      /* int         version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
      /* hbool_t     rpt_fcn_enabled        = */ FALSE,
      /* hbool_t     open_trace_file        = */ FALSE,
      /* hbool_t     close_trace_file       = */ FALSE,
      /* char        trace_file_name[]      = */ "",
      /* hbool_t     evictions_enabled      = */ TRUE,
      /* hbool_t     set_initial_size       = */ TRUE,
      /* size_t      initial_size           = */ (1 * 1024 * 1024),
      /* double      min_clean_fraction     = */ 0.2f,
      /* size_t      max_size               = */ (16 * 1024 * 1024),
      /* size_t      min_size               = */ ( 1 * 1024 * 1024),
      /* long int    epoch_length           = */ 50000,
      /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__off,
      /* double      lower_hr_threshold     = */ 0.90f,
      /* double      increment              = */ 2.0f,
      /* hbool_t     apply_max_increment    = */ TRUE,
      /* size_t      max_increment          = */ (4 * 1024 * 1024),
      /* enum H5C_cache_flash_incr_mode       */
      /*                    flash_incr_mode = */ H5C_flash_incr__off,
      /* double      flash_multiple         = */ 2.1f,
      /* double      flash_threshold        = */ 0.6f,
      /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__off,
      /* double      upper_hr_threshold     = */ 0.999f,
      /* double      decrement              = */ 0.9f,
      /* hbool_t     apply_max_decrement    = */ FALSE,
      /* size_t      max_decrement          = */ (1 * 1024 * 1024 - 1),
      /* int         epochs_before_eviction = */ 3,
      /* hbool_t     apply_empty_reserve    = */ FALSE,
      /* double      empty_reserve          = */ 0.05f,
      /* int         dirty_bytes_threshold  = */ (256 * 1024),
      /* int	    metadata_write_strategy = */
				      H5AC__DEFAULT_METADATA_WRITE_STRATEGY
    };
    H5AC_cache_config_t mod_config_4 =
    {
      /* int         version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
      /* hbool_t     rpt_fcn_enabled        = */ FALSE,
      /* hbool_t     open_trace_file        = */ FALSE,
      /* hbool_t     close_trace_file       = */ FALSE,
      /* char        trace_file_name[]      = */ "",
      /* hbool_t     evictions_enabled      = */ TRUE,
      /* hbool_t     set_initial_size       = */ TRUE,
      /* size_t      initial_size           = */ (1 * 1024 * 1024),
      /* double      min_clean_fraction     = */ 0.15f,
      /* size_t      max_size               = */ (20 * 1024 * 1024),
      /* size_t      min_size               = */ ( 1 * 1024 * 1024),
      /* long int    epoch_length           = */ 75000,
      /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__threshold,
      /* double      lower_hr_threshold     = */ 0.9f,
      /* double      increment              = */ 2.0f,
      /* hbool_t     apply_max_increment    = */ TRUE,
      /* size_t      max_increment          = */ (2 * 1024 * 1024),
      /* enum H5C_cache_flash_incr_mode       */
      /*                    flash_incr_mode = */ H5C_flash_incr__off,
      /* double      flash_multiple         = */ 1.1f,
      /* double      flash_threshold        = */ 0.3f,
      /* enum H5C_cache_decr_mode decr_mode = */
                                               H5C_decr__age_out_with_threshold,
      /* double      upper_hr_threshold     = */ 0.999f,
      /* double      decrement              = */ 0.9f,
      /* hbool_t     apply_max_decrement    = */ TRUE,
      /* size_t      max_decrement          = */ (1 * 1024 * 1024),
      /* int         epochs_before_eviction = */ 3,
      /* hbool_t     apply_empty_reserve    = */ TRUE,
      /* double      empty_reserve          = */ 0.1f,
      /* int         dirty_bytes_threshold  = */ (256 * 1024),
      /* int	    metadata_write_strategy = */
				      H5AC__DEFAULT_METADATA_WRITE_STRATEGY
    };

    TESTING("MDC/FILE related API calls");

    pass = TRUE;

    /* Open a file with the default FAPL.  Verify that the cache is
     * configured as per the default both by looking at its internal
     * configuration, and via the H5Fget_mdc_config() call.
     *
     * Then set serveral different configurations, and verify that
     * they took as per above.
     */

    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    /* create the file using the default FAPL */
    if ( pass ) {

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

        if ( file_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fcreate() failed.\n";
        }
    }

    /* verify that the cache is set to the default config */
    validate_mdc_config(file_id, &default_config, TRUE, 1);

    /* set alternate config 1 */
    if ( pass ) {

        if ( H5Fset_mdc_config(file_id, &mod_config_1) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() failed 1.\n";
        }
    }

    /* verify that the cache is now set to the alternate config */
    validate_mdc_config(file_id, &mod_config_1, TRUE, 2);

    /* set alternate config 2 */
    if ( pass ) {

        if ( H5Fset_mdc_config(file_id, &mod_config_2) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() failed 2.\n";
        }
    }

    /* verify that the cache is now set to the alternate config */
    validate_mdc_config(file_id, &mod_config_2, TRUE, 3);

    /* set alternate config 3 */
    if ( pass ) {

        if ( H5Fset_mdc_config(file_id, &mod_config_3) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() failed 3.\n";
        }
    }

    /* verify that the cache is now set to the alternate config */
    validate_mdc_config(file_id, &mod_config_3, TRUE, 4);

    /* set alternate config 4 */
    if ( pass ) {

        if ( H5Fset_mdc_config(file_id, &mod_config_4) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() failed 4.\n";
        }
    }

    /* verify that the cache is now set to the alternate config */
    validate_mdc_config(file_id, &mod_config_4, TRUE, 5);


    /* Run some quick smoke checks on the cache status monitoring
     * calls -- no interesting data as the cache hasn't had a
     * chance to do much yet.
     */

    if ( pass ) {

        if ( H5Fget_mdc_hit_rate(file_id, &hit_rate) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_hit_rate() failed 1.\n";

        } else if ( !H5_DBL_ABS_EQUAL(hit_rate, (double)0.0f) ) {

            pass = FALSE;
            failure_mssg =
                "H5Fget_mdc_hit_rate() returned unexpected hit rate.\n";

        }
#if 0 /* this may be useful now and then -- keep it around */
        else {

            HDfprintf(stdout,
                      "H5Fget_mdc_hit_rate() reports hit_rate = %lf:\n",
                      hit_rate);
        }
#endif
    }

    if ( pass ) {

        if ( H5Fget_mdc_size(file_id, &max_size, &min_clean_size,
                             &cur_size, &cur_num_entries) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_size() failed 1.\n";

        } else if ( ( mod_config_4.initial_size != max_size ) ||
                    ( min_clean_size != (size_t)
                      ((double)max_size * mod_config_4.min_clean_fraction) ) ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_size() returned unexpected value(s).\n";

        }
#if 0 /* this may be useful now and then -- keep it around */
        else {

            HDfprintf(stdout, "H5Fget_mdc_size() reports:\n");
            HDfprintf(stdout, "	max_size: %ld, min_clean_size: %ld\n",
                      (long)max_size, (long)min_clean_size);
	    HDfprintf(stdout, "	cur_size: %ld, cur_num_entries: %d\n",
                      (long)cur_size, cur_num_entries);
        }
#endif
    }

    /* close the file and delete it */
    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        } else if ( HDremove(filename) < 0 ) {

            pass = FALSE;
            failure_mssg = "HDremove() failed.\n";
        }
    }

    if ( pass ) {

        PASSED();

    } else {

        H5_FAILED();
    }

    if ( ! pass ) {
        
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", FUNC, failure_mssg);
    }

    return pass;

} /* check_file_mdc_api_calls() */


/*-------------------------------------------------------------------------
 * Function:	mdc_api_call_smoke_check()
 *
 * Purpose:     Initial basic functional test to see if there are problems
 *              with the cache API calls.
 *
 *              NOTE: This test takes some time to run and checks the 
 *                    HDF5TestExpress environment variable.
 *
 * Return:      Test pass status (TRUE/FALSE)
 *
 * Programmer:  John Mainzer
 *              4/14/04
 *
 *-------------------------------------------------------------------------
 */

#define CHUNK_SIZE              2
#define DSET_SIZE               (200 * CHUNK_SIZE)
#define NUM_DSETS               6
#define NUM_RANDOM_ACCESSES     200000

static hbool_t
mdc_api_call_smoke_check(int express_test)
{
    char filename[512];
    hbool_t valid_chunk;
    hbool_t dump_hit_rate = FALSE;
    int64_t min_accesses = 1000;
    double min_hit_rate = 0.90f;
    hbool_t dump_cache_size = FALSE;
    hid_t file_id = -1;
    hid_t dataspace_id = -1;
    hid_t filespace_ids[NUM_DSETS];
    hid_t memspace_id = -1;
    hid_t dataset_ids[NUM_DSETS];
    hid_t properties = -1;
    char dset_name[64];
    int i, j, k, l, m, n;
    herr_t status;
    hsize_t dims[2];
    hsize_t a_size[2];
    hsize_t offset[2];
    hsize_t chunk_size[2];
    int data_chunk[CHUNK_SIZE][CHUNK_SIZE];
    H5AC_cache_config_t default_config = H5AC__DEFAULT_CACHE_CONFIG;
    H5AC_cache_config_t mod_config_1 =
    {
      /* int         version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
      /* hbool_t     rpt_fcn_enabled        = */ FALSE,
      /* hbool_t     open_trace_file        = */ FALSE,
      /* hbool_t     close_trace_file       = */ FALSE,
      /* char        trace_file_name[]      = */ "",
      /* hbool_t     evictions_enabled      = */ TRUE,
      /* hbool_t     set_initial_size       = */ TRUE,
      /* size_t      initial_size           = */ 500000,
      /* double      min_clean_fraction     = */ 0.1f,
      /* size_t      max_size               = */ 16000000,
      /* size_t      min_size               = */ 250000,
      /* long int    epoch_length           = */ 50000,
      /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__off,
      /* double      lower_hr_threshold     = */ 0.95f,
      /* double      increment              = */ 2.0f,
      /* hbool_t     apply_max_increment    = */ FALSE,
      /* size_t      max_increment          = */ 4000000,
      /* enum H5C_cache_flash_incr_mode       */
      /*                    flash_incr_mode = */ H5C_flash_incr__off,
      /* double      flash_multiple         = */ 2.0f,
      /* double      flash_threshold        = */ 0.5f,
      /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__off,
      /* double      upper_hr_threshold     = */ 0.999f,
      /* double      decrement              = */ 0.9f,
      /* hbool_t     apply_max_decrement    = */ FALSE,
      /* size_t      max_decrement          = */ 1000000,
      /* int         epochs_before_eviction = */ 2,
      /* hbool_t     apply_empty_reserve    = */ TRUE,
      /* double      empty_reserve          = */ 0.05f,
      /* int         dirty_bytes_threshold  = */ (256 * 1024),
      /* int	    metadata_write_strategy = */
				      H5AC__DEFAULT_METADATA_WRITE_STRATEGY
    };
    H5AC_cache_config_t mod_config_2 =
    {
      /* int         version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
      /* hbool_t     rpt_fcn_enabled        = */ FALSE,
      /* hbool_t     open_trace_file        = */ FALSE,
      /* hbool_t     close_trace_file       = */ FALSE,
      /* char        trace_file_name[]      = */ "",
      /* hbool_t     evictions_enabled      = */ TRUE,
      /* hbool_t     set_initial_size       = */ TRUE,
      /* size_t      initial_size           = */ 12000000,
      /* double      min_clean_fraction     = */ 0.1f,
      /* size_t      max_size               = */ 16000000,
      /* size_t      min_size               = */ 250000,
      /* long int    epoch_length           = */ 50000,
      /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__off,
      /* double      lower_hr_threshold     = */ 0.95f,
      /* double      increment              = */ 2.0f,
      /* hbool_t     apply_max_increment    = */ FALSE,
      /* size_t      max_increment          = */ 4000000,
      /* enum H5C_cache_flash_incr_mode       */
      /*                    flash_incr_mode = */ H5C_flash_incr__off,
      /* double      flash_multiple         = */ 2.0f,
      /* double      flash_threshold        = */ 0.5f,
      /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__off,
      /* double      upper_hr_threshold     = */ 0.999f,
      /* double      decrement              = */ 0.9f,
      /* hbool_t     apply_max_decrement    = */ FALSE,
      /* size_t      max_decrement          = */ 1000000,
      /* int         epochs_before_eviction = */ 2,
      /* hbool_t     apply_empty_reserve    = */ TRUE,
      /* double      empty_reserve          = */ 0.05f,
      /* int         dirty_bytes_threshold  = */ (256 * 1024),
      /* int	    metadata_write_strategy = */
				      H5AC__DEFAULT_METADATA_WRITE_STRATEGY
    };
    H5AC_cache_config_t mod_config_3 =
    {
      /* int         version                = */ H5C__CURR_AUTO_SIZE_CTL_VER,
      /* hbool_t     rpt_fcn_enabled        = */ FALSE,
      /* hbool_t     open_trace_file        = */ FALSE,
      /* hbool_t     close_trace_file       = */ FALSE,
      /* char        trace_file_name[]      = */ "",
      /* hbool_t     evictions_enabled      = */ TRUE,
      /* hbool_t     set_initial_size       = */ TRUE,
      /* size_t      initial_size           = */ 2000000,
      /* double      min_clean_fraction     = */ 0.1f,
      /* size_t      max_size               = */ 16000000,
      /* size_t      min_size               = */ 250000,
      /* long int    epoch_length           = */ 50000,
      /* enum H5C_cache_incr_mode incr_mode = */ H5C_incr__off,
      /* double      lower_hr_threshold     = */ 0.95f,
      /* double      increment              = */ 2.0f,
      /* hbool_t     apply_max_increment    = */ FALSE,
      /* size_t      max_increment          = */ 4000000,
      /* enum H5C_cache_flash_incr_mode       */
      /*                    flash_incr_mode = */ H5C_flash_incr__off,
      /* double      flash_multiple         = */ 2.0f,
      /* double      flash_threshold        = */ 0.5f,
      /* enum H5C_cache_decr_mode decr_mode = */ H5C_decr__off,
      /* double      upper_hr_threshold     = */ 0.999f,
      /* double      decrement              = */ 0.9f,
      /* hbool_t     apply_max_decrement    = */ FALSE,
      /* size_t      max_decrement          = */ 1000000,
      /* int         epochs_before_eviction = */ 2,
      /* hbool_t     apply_empty_reserve    = */ TRUE,
      /* double      empty_reserve          = */ 0.05f,
      /* int         dirty_bytes_threshold  = */ (256 * 1024),
      /* int	    metadata_write_strategy = */
				      H5AC__DEFAULT_METADATA_WRITE_STRATEGY
    };

    TESTING("MDC API smoke check");

    pass = TRUE;

    if ( express_test > 0 ) {

        SKIPPED();

        HDfprintf(stdout, "     Long tests disabled.\n");

        return pass;
    }

    /* Open a file with the default FAPL.  Verify that the cache is
     * configured as per the default both by looking at its internal
     * configuration, and via the H5Fget_mdc_config() call.
     *
     * Then set the cache to mod_config_1, which fixes cache size at
     * 500000 bytes, and turns off automatic cache resize.
     */

    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    /* create the file using the default FAPL */
    if ( pass ) {

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

        if ( file_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fcreate() failed.\n";
        }
    }

    /* verify that the cache is set to the default config */
    validate_mdc_config(file_id, &default_config, TRUE, 1);

    /* set alternate config 1 */
    if ( pass ) {

        if ( H5Fset_mdc_config(file_id, &mod_config_1) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() failed 1.\n";
        }
    }

    /* verify that the cache is now set to the alternate config */
    validate_mdc_config(file_id, &mod_config_1, TRUE, 2);

    /* create the datasets */
    if ( pass ) {

        i = 0;

        while ( ( pass ) && ( i < NUM_DSETS ) )
        {
            /* create a dataspace for the chunked dataset */
            dims[0] = DSET_SIZE;
            dims[1] = DSET_SIZE;
            dataspace_id = H5Screate_simple(2, dims, NULL);

            if ( dataspace_id < 0 ) {

                pass = FALSE;
                failure_mssg = "H5Screate_simple() failed.";
            }

            /* set the dataset creation plist to specify that the raw data is
             * to be partioned into 10X10 element chunks.
             */

            if ( pass ) {

                chunk_size[0] = CHUNK_SIZE;
                chunk_size[1] = CHUNK_SIZE;
                properties = H5Pcreate(H5P_DATASET_CREATE);

                if ( properties < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Pcreate() failed.";
                }
            }

            if ( pass ) {

                if ( H5Pset_chunk(properties, 2, chunk_size) < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Pset_chunk() failed.";
                }
            }

            /* create the dataset */
            if ( pass ) {

                sprintf(dset_name, "/dset%03d", i);
                dataset_ids[i] = H5Dcreate2(file_id, dset_name, H5T_STD_I32BE,
				            dataspace_id, H5P_DEFAULT, properties, H5P_DEFAULT);

                if ( dataset_ids[i] < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Dcreate2() failed.";
                }
            }

            /* get the file space ID */
            if ( pass ) {

                filespace_ids[i] = H5Dget_space(dataset_ids[i]);

                if ( filespace_ids[i] < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Dget_space() failed.";
                }
            }

            i++;
        }
    }

    /* create the mem space to be used to read and write chunks */
    if ( pass ) {

        dims[0] = CHUNK_SIZE;
        dims[1] = CHUNK_SIZE;
        memspace_id = H5Screate_simple(2, dims, NULL);

        if ( memspace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Screate_simple() failed.";
        }
    }

    /* select in memory hyperslab */
    if ( pass ) {

        offset[0] = 0;  /*offset of hyperslab in memory*/
        offset[1] = 0;
        a_size[0] = CHUNK_SIZE;  /*size of hyperslab*/
        a_size[1] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, NULL,
                                     a_size, NULL);

        if ( status < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sselect_hyperslab() failed.";
        }
    }

    /* initialize all datasets on a round robin basis */
    i = 0;

    while ( ( pass ) && ( i < DSET_SIZE ) )
    {
        j = 0;
        while ( ( pass ) && ( j < DSET_SIZE ) )
        {
            m = 0;
            while ( ( pass ) && ( m < NUM_DSETS ) )
            {
                /* initialize the slab */
                for ( k = 0; k < CHUNK_SIZE; k++ )
                {
                    for ( l = 0; l < CHUNK_SIZE; l++ )
                    {
                        data_chunk[k][l] = (DSET_SIZE * DSET_SIZE * m) +
                                           (DSET_SIZE * (i + k)) + j + l;
                    }
                }

                /* select on disk hyperslab */
                offset[0] = (hsize_t)i; /*offset of hyperslab in file*/
                offset[1] = (hsize_t)j;
                a_size[0] = CHUNK_SIZE;   /*size of hyperslab*/
                a_size[1] = CHUNK_SIZE;
                status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                         offset, NULL, a_size, NULL);

                if ( status < 0 ) {

                    pass = FALSE;
                    failure_mssg = "disk H5Sselect_hyperslab() failed.";
                }

                /* write the chunk to file */
                status = H5Dwrite(dataset_ids[m], H5T_NATIVE_INT, memspace_id,
                                  filespace_ids[m], H5P_DEFAULT, data_chunk);

                if ( status < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Dwrite() failed.";
                }
                m++;
            }
            j += CHUNK_SIZE;
        }

        /* check the cache hit rate, and reset the counters.
         * Hit rate should be just about unity here, so we will just
         * get the data and (possibly) print it without checking it
         * beyond ensuring that it agrees with the cache internal
         * data structures.
         *
         * similarly, check cache size.
         */

        if ( ( pass ) && ( i % (DSET_SIZE / 4) == 0 ) ) {

            check_and_validate_cache_hit_rate(file_id, NULL, dump_hit_rate,
                                              min_accesses, min_hit_rate);

            check_and_validate_cache_size(file_id, NULL, NULL, NULL, NULL,
                                          dump_cache_size);
        }

        i += CHUNK_SIZE;

    }

    /* set alternate config 2 */
    if ( pass ) {

        if ( H5Fset_mdc_config(file_id, &mod_config_2) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() failed 2.\n";
        }
    }

    /* verify that the cache is now set to the alternate config */
    validate_mdc_config(file_id, &mod_config_2, TRUE, 3);

    /* do random reads on all datasets */
    n = 0;
    while ( ( pass ) && ( n < NUM_RANDOM_ACCESSES ) )
    {
        m = rand() % NUM_DSETS;
        i = (rand() % (DSET_SIZE / CHUNK_SIZE)) * CHUNK_SIZE;
        j = (rand() % (DSET_SIZE / CHUNK_SIZE)) * CHUNK_SIZE;

        /* select on disk hyperslab */
        offset[0] = (hsize_t)i; /*offset of hyperslab in file*/
        offset[1] = (hsize_t)j;
        a_size[0] = CHUNK_SIZE;   /*size of hyperslab*/
        a_size[1] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                     offset, NULL, a_size, NULL);

        if ( status < 0 ) {

           pass = FALSE;
           failure_mssg = "disk hyperslab create failed.";
        }

        /* read the chunk from file */
        if ( pass ) {

            status = H5Dread(dataset_ids[m], H5T_NATIVE_INT, memspace_id,
                             filespace_ids[m], H5P_DEFAULT, data_chunk);

            if ( status < 0 ) {

               pass = FALSE;
               failure_mssg = "disk hyperslab create failed.";
            }
        }

        /* validate the slab */
        if ( pass ) {

            valid_chunk = TRUE;
            for ( k = 0; k < CHUNK_SIZE; k++ )
            {
                for ( l = 0; l < CHUNK_SIZE; l++ )
                {
                     if ( data_chunk[k][l]
                          !=
                          ((DSET_SIZE * DSET_SIZE * m) +
                           (DSET_SIZE * (i + k)) + j + l) ) {

                         valid_chunk = FALSE;
#if 0 /* this will be useful from time to time -- lets keep it*/
                         HDfprintf(stdout,
                                   "data_chunk[%0d][%0d] = %0d, expect %0d.\n",
                                   k, l, data_chunk[k][l],
                                   ((DSET_SIZE * DSET_SIZE * m) +
                                    (DSET_SIZE * (i + k)) + j + l));
                         HDfprintf(stdout,
                                   "m = %d, i = %d, j = %d, k = %d, l = %d\n",
                                   m, i, j, k, l);
#endif
                    }
                }
            }

            if ( ! valid_chunk ) {
#if 1
                pass = FALSE;
                failure_mssg = "slab validation failed.";
#else /* as above */
                fprintf(stdout, "Chunk (%0d, %0d) in /dset%03d is invalid.\n",
                        i, j, m);
#endif
            }
        }

        if ( ( pass ) && ( n % (NUM_RANDOM_ACCESSES / 4) == 0 ) ) {

            check_and_validate_cache_hit_rate(file_id, NULL, dump_hit_rate,
                                              min_accesses, min_hit_rate);

            check_and_validate_cache_size(file_id, NULL, NULL, NULL, NULL,
                                          dump_cache_size);
        }

        n++;
    }


    /* close the file spaces we are done with */
    i = 1;
    while ( ( pass ) && ( i < NUM_DSETS ) )
    {
        if ( H5Sclose(filespace_ids[i]) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sclose() failed.";
        }
        i++;
    }


    /* close the datasets we are done with */
    i = 1;
    while ( ( pass ) && ( i < NUM_DSETS ) )
    {
        if ( H5Dclose(dataset_ids[i]) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dclose() failed.";
        }
        i++;
    }

    /* set alternate config 3 */
    if ( pass ) {

        if ( H5Fset_mdc_config(file_id, &mod_config_3) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() failed 3.\n";
        }
    }

    /* verify that the cache is now set to the alternate config */
    validate_mdc_config(file_id, &mod_config_3, TRUE, 4);

    /* do random reads on data set 0 only */
    m = 0;
    n = 0;
    while ( ( pass ) && ( n < NUM_RANDOM_ACCESSES ) )
    {
        i = (rand() % (DSET_SIZE / CHUNK_SIZE)) * CHUNK_SIZE;
        j = (rand() % (DSET_SIZE / CHUNK_SIZE)) * CHUNK_SIZE;

        /* select on disk hyperslab */
        offset[0] = (hsize_t)i; /*offset of hyperslab in file*/
        offset[1] = (hsize_t)j;
        a_size[0] = CHUNK_SIZE;   /*size of hyperslab*/
        a_size[1] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                     offset, NULL, a_size, NULL);

        if ( status < 0 ) {

           pass = FALSE;
           failure_mssg = "disk hyperslab create failed.";
        }

        /* read the chunk from file */
        if ( pass ) {

            status = H5Dread(dataset_ids[m], H5T_NATIVE_INT, memspace_id,
                             filespace_ids[m], H5P_DEFAULT, data_chunk);

            if ( status < 0 ) {

               pass = FALSE;
               failure_mssg = "disk hyperslab create failed.";
            }
        }

        /* validate the slab */
        if ( pass ) {

            valid_chunk = TRUE;
            for ( k = 0; k < CHUNK_SIZE; k++ )
            {
               for ( l = 0; l < CHUNK_SIZE; l++ )
               {
                   if ( data_chunk[k][l]
                        !=
                        ((DSET_SIZE * DSET_SIZE * m) +
                         (DSET_SIZE * (i + k)) + j + l) ) {

                       valid_chunk = FALSE;
                  }
#if 0 /* this will be useful from time to time -- lets keep it */
                  HDfprintf(stdout, "data_chunk[%0d][%0d] = %0d, expect %0d.\n",
                            k, l, data_chunk[k][l],
                            ((DSET_SIZE * DSET_SIZE * m) +
                             (DSET_SIZE * (i + k)) + j + l));
#endif
                }
            }

            if ( ! valid_chunk ) {

                pass = FALSE;
                failure_mssg = "slab validation failed.";
#if 0 /* as above */
                fprintf(stdout, "Chunk (%0d, %0d) in /dset%03d is invalid.\n",
                        i, j, m);
#endif
            }
        }

        if ( ( pass ) && ( n % (NUM_RANDOM_ACCESSES / 4) == 0 ) ) {

            check_and_validate_cache_hit_rate(file_id, NULL, dump_hit_rate,
                                              min_accesses, min_hit_rate);

            check_and_validate_cache_size(file_id, NULL, NULL, NULL, NULL,
                                          dump_cache_size);
        }

        n++;
    }

    /* close file space 0 */
    if ( pass ) {

        if ( H5Sclose(filespace_ids[0]) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sclose(filespace_ids[0]) failed.";
        }
    }

    /* close the data space */
    if ( pass ) {

        if ( H5Sclose(dataspace_id) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sclose(dataspace) failed.";
        }
    }

    /* close the mem space */
    if ( pass ) {

        if ( H5Sclose(memspace_id) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sclose(memspace_id) failed.";
        }
    }

    /* close dataset 0 */
    if ( pass ) {

        if ( H5Dclose(dataset_ids[0]) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dclose(dataset_ids[0]) failed.";
        }
    }

    /* close the file and delete it */
    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        }
        else if ( HDremove(filename) < 0 ) {

            pass = FALSE;
            failure_mssg = "HDremove() failed.\n";
        }
    }

    if ( pass ) {
        
        PASSED();
        
    } else {

        H5_FAILED();
    }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", FUNC, failure_mssg);
    }

    return pass;

} /* mdc_api_call_smoke_check() */



/*-------------------------------------------------------------------------
 * Function:    init_invalid_configs()
 *
 * Purpose:     Initialize an array of invalid external MDC configurations
 *              that will be used to test error rejection in the MDC-
 *              related API calls.
 *
 *              Note: It is assumed that boolean parameters are only set
 *                    to TRUE/FALSE.
 *
 * Return:      Success:    Pointer to an array of cache configurations.
 *              Failure:    NULL
 *
 * Programmer:  Dana Robinson
 *              Spring 2016
 *
 *-------------------------------------------------------------------------
 */

#define NUM_INVALID_CONFIGS	36
static H5AC_cache_config_t * invalid_configs = NULL;

static H5AC_cache_config_t *
init_invalid_configs(void) {

    int i;
    H5AC_cache_config_t * configs = NULL;

    /* Allocate memory */
    if ( NULL == (configs = (H5AC_cache_config_t *)HDcalloc(
            NUM_INVALID_CONFIGS, sizeof(H5AC_cache_config_t) ) ) ) {

        return NULL;
    }

    /* Set defaults for all configs */
    for ( i = 0; i < NUM_INVALID_CONFIGS; i++ ) {

        configs[i].version                  = H5C__CURR_AUTO_SIZE_CTL_VER;
        configs[i].rpt_fcn_enabled          = FALSE;
        configs[i].open_trace_file          = FALSE;
        configs[i].close_trace_file         = FALSE;
        /* trace file name set to all ASCII NUL by calloc() */
        configs[i].evictions_enabled        = TRUE;
        configs[i].set_initial_size         = TRUE;
        configs[i].initial_size             = (1 * 1024 * 1024);
        configs[i].min_clean_fraction       = 0.25F;
        configs[i].max_size                 = (16 * 1024 * 1024);
        configs[i].min_size                 = (1 * 1024 * 1024);
        configs[i].epoch_length             = 50000;
        configs[i].incr_mode                = H5C_incr__threshold;
        configs[i].lower_hr_threshold       = 0.9F;
        configs[i].increment                = 2.0F;
        configs[i].apply_max_increment      = TRUE;
        configs[i].max_increment            = (4 * 1024 * 1024);
        configs[i].flash_incr_mode          = H5C_flash_incr__off;
        configs[i].flash_multiple           = 2.0F;
        configs[i].flash_threshold          = 0.5F;
        configs[i].decr_mode                = H5C_decr__age_out_with_threshold;
        configs[i].upper_hr_threshold       = 0.999F;
        configs[i].decrement                = 0.9F;
        configs[i].apply_max_decrement      = TRUE;
        configs[i].max_decrement            = (1 * 1024 * 1024);
        configs[i].epochs_before_eviction   = 3;
        configs[i].apply_empty_reserve      = TRUE;
        configs[i].empty_reserve            = 0.1F;
        configs[i].dirty_bytes_threshold    = (256 * 1024);
        configs[i].metadata_write_strategy  = H5AC__DEFAULT_METADATA_WRITE_STRATEGY;
    }

    /* Set badness for each config */

    /* 0 -- bad version */
    configs[0].version                      = -1;

    /* 1 -- open_trace_file == TRUE and empty trace_file_name */
    configs[1].open_trace_file              = TRUE;
    /* trace file name set to all ASCII NUL by calloc() */

    /* 2 -- max_size too big */
    configs[2].max_size                     = H5C__MAX_MAX_CACHE_SIZE + 1;

    /* 3 -- min_size too small */
    configs[3].min_size                     = H5C__MIN_MAX_CACHE_SIZE - 1;

    /* 4 -- min_size > max_size */
    configs[4].max_size                     = (16 * 1024 * 1024);
    configs[4].min_size                     = (16 * 1024 * 1024 + 1);

    /* 5 -- initial size out of range (too big) */
    configs[5].initial_size                 = (16 * 1024 * 1024 + 1);

    /* 6 -- initial_size out of range (too small) */
    configs[6].initial_size                 = (1 * 1024 * 1024 - 1);

    /* 7 -- min_clean_fraction too big */
    configs[7].min_clean_fraction           = 1.000001f;

    /* 8 -- min_clean_fraction too small */
    configs[8].min_clean_fraction           = -0.00000001f;

    /* 9 -- epoch_length too small */
    configs[9].epoch_length                 = H5C__MIN_AR_EPOCH_LENGTH - 1;

    /* 10 -- epoch_length too big */
    configs[10].epoch_length                = H5C__MAX_AR_EPOCH_LENGTH + 1;

    /* 11 -- invalid incr_mode */
    configs[11].incr_mode                   = (enum H5C_cache_incr_mode)-1;

    /* 12 -- lower_hr_threshold too small */
    configs[12].lower_hr_threshold          = -0.000001f;

    /* 13 -- lower_hr_threshold too big */
    configs[13].lower_hr_threshold          = 1.00000001f;

    /* 14 -- increment too small */
    configs[14].increment                   = H5_DOUBLE(0.999999999999);

    /* 15 -- invalid flash_incr_mode */
    configs[15].flash_incr_mode             = (enum H5C_cache_flash_incr_mode)-1;

    /* 16 -- flash_multiple too small */
    configs[16].flash_incr_mode             = H5C_flash_incr__add_space;
    configs[16].flash_multiple              = 0.09f;

    /* 17 -- flash_multiple too big */
    configs[17].flash_incr_mode             = H5C_flash_incr__add_space;
    configs[17].flash_multiple              = 10.001f;

    /* 18 -- flash_threshold too small */
    configs[18].flash_incr_mode             = H5C_flash_incr__add_space;
    configs[18].flash_threshold             = 0.099f;

    /* 19 -- flash_threshold too big */
    configs[19].flash_incr_mode             = H5C_flash_incr__add_space;
    configs[19].flash_threshold             = 1.001f;

    /* 20 -- bad decr_mode */
    configs[20].decr_mode                   = (enum H5C_cache_decr_mode)-1;

    /* 21 -- upper_hr_threshold too big */
    configs[21].upper_hr_threshold          = 1.00001f;

    /* 22 -- decrement too small */
    configs[22].decr_mode                   = H5C_decr__threshold;
    configs[22].decrement                   = -0.0000000001f;

    /* 23 -- decrement too big */
    configs[23].decr_mode                   = H5C_decr__threshold;
    configs[23].decrement                   = H5_DOUBLE(1.0000000001);

    /* 24 -- epochs_before_eviction too small */
    configs[24].epochs_before_eviction      = 0;

    /* 25 -- epochs_before_eviction too big */
    configs[25].epochs_before_eviction      = H5C__MAX_EPOCH_MARKERS + 1;

    /* 26 -- empty_reserve too small */
    configs[26].empty_reserve               = -0.0000000001f;

    /* 27 -- empty_reserve too big */
    configs[27].empty_reserve               = H5_DOUBLE(1.00000000001);

    /* 28 -- upper_hr_threshold too small */
    configs[28].upper_hr_threshold          = -0.000000001f;

    /* 29 -- upper_hr_threshold too big */
    configs[29].upper_hr_threshold          = H5_DOUBLE(1.00000001);

    /* 30 -- upper_hr_threshold <= lower_hr_threshold */
    configs[30].lower_hr_threshold          = 0.9f;
    configs[30].upper_hr_threshold          = 0.9f;

    /* 31 -- dirty_bytes_threshold too small */
    configs[31].dirty_bytes_threshold       = (H5C__MIN_MAX_CACHE_SIZE / 2) - 1;

    /* 32 -- dirty_bytes_threshold too big */
    configs[32].dirty_bytes_threshold       = (H5C__MAX_MAX_CACHE_SIZE / 4) + 1;

    /* 33 -- attempt to disable evictions when auto incr enabled */
    configs[33].evictions_enabled           = FALSE;
    configs[33].decr_mode                   = H5C_decr__off;

    /* 34 -- attempt to disable evictions when auto decr enabled */
    configs[34].evictions_enabled           = FALSE;
    configs[34].decr_mode                   = H5C_decr__age_out;

    /* 35 -- unknown metadata write strategy */
    configs[35].metadata_write_strategy     = -1;

    return configs;

} /* initialize_invalid_configs() */


/*-------------------------------------------------------------------------
 * Function:    check_fapl_mdc_api_errs()
 *
 * Purpose:     Verify that the FAPL related MDC API calls reject input
 *              errors gracefully.
 *
 * Return:      Test pass status (TRUE/FALSE)
 *
 * Programmer:  John Mainzer
 *              4/19/04
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
check_fapl_mdc_api_errs(void)
{
    static char msg[128];
    int i;
    herr_t result;
    hid_t fapl_id = -1;
    H5AC_cache_config_t default_config = H5AC__DEFAULT_CACHE_CONFIG;
    H5AC_cache_config_t scratch;

    TESTING("MDC/FAPL related API input errors");

    pass = TRUE;

    /* first test H5Pget_mdc_config().
     */

    scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;
    if  ( pass ) {

        H5E_BEGIN_TRY {
            result = H5Pget_mdc_config((hid_t)-1, &scratch);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_config() accepted invalid plist_id.";
        }
    }

    /* Create a FAPL for test purposes, and verify that it contains the
     * default MDC configuration.
     */

    if ( pass ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        }
    }

    scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;
    if ( ( pass ) &&
         ( ( H5Pget_mdc_config(fapl_id, &scratch) < 0) ||
           ( !CACHE_CONFIGS_EQUAL(default_config, scratch, TRUE, TRUE) ) ) ) {

        pass = FALSE;
        failure_mssg = "New FAPL has unexpected metadata cache config?!?!?.\n";
    }

    if  ( pass ) {

        H5E_BEGIN_TRY {
            result = H5Pget_mdc_config(fapl_id, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_config() accepted NULL config_ptr.";
        }
    }

    /* one last test for H5Pget_mdc_config() */

    scratch.version = -1; /* a convenient, invalid value */
    if  ( pass ) {

        H5E_BEGIN_TRY {
            result = H5Pget_mdc_config(fapl_id, &scratch);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_config() accepted bad config version.";
        }
    }


    /* now test H5Pset_mdc_config()
     */

    scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5Pset_mdc_config((hid_t)-1, &default_config);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_mdc_config() accepted bad invalid plist_id.";
        }
    }

    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5Pset_mdc_config(fapl_id, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_mdc_config() accepted NULL config_ptr.";
        }
    }

    i = 0;
    while ( ( pass ) && ( i < NUM_INVALID_CONFIGS ) )
    {
        H5E_BEGIN_TRY {
            result = H5Pset_mdc_config(fapl_id, &(invalid_configs[i]));
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                   "H5Pset_mdc_config() accepted invalid_configs[%d].", i);
            failure_mssg = msg;
        }
        i++;
    }

    /* verify that none of the above calls to H5Pset_mdc_config() changed
     * the configuration in the FAPL.
     */
    scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;
    if ( ( pass ) &&
         ( ( H5Pget_mdc_config(fapl_id, &scratch) < 0 ) ||
           ( !CACHE_CONFIGS_EQUAL(default_config, scratch, TRUE, TRUE) ) ) ) {

        pass = FALSE;
        failure_mssg = "FAPL metadata cache config changed???.\n";
    }

    if ( pass ) {

        PASSED();

    } else {

        H5_FAILED();
    }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", FUNC, failure_mssg);
    }

    return pass;

} /* check_fapl_mdc_api_errs() */


/*-------------------------------------------------------------------------
 * Function:    check_file_mdc_api_errs()
 *
 * Purpose:     Verify that the file related MDC API calls reject input
 *              errors gracefully.
 *
 * Return:      Test pass status (TRUE/FALSE)
 *
 * Programmer:  John Mainzer
 *              4/19/04
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
check_file_mdc_api_errs(void)
{
    char filename[512];
    static char msg[128];
    hbool_t show_progress = FALSE;
    int i;
    herr_t result;
    hid_t file_id = -1;
    size_t max_size;
    size_t min_clean_size;
    size_t cur_size;
    int cur_num_entries;
    double hit_rate;
    H5AC_cache_config_t default_config = H5AC__DEFAULT_CACHE_CONFIG;
    H5AC_cache_config_t scratch;

    TESTING("MDC/FILE related API input errors");

    pass = TRUE;

    /* Create a file for test purposes, and veify that its metadata cache
     * set to the default MDC configuration.
     */

    /* setup the file name */
    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: calling h5_fixname().\n", FUNC);
        }

        if ( h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: calling H5Fcreate().\n", FUNC);
        }

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

        if ( file_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fcreate() failed.\n";
        }
    }

    validate_mdc_config(file_id, &default_config, TRUE, 1);


    /* test H5Fget_mdc_config().  */

    scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;
    if  ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fget_mdc_config() 1.\n", FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Fget_mdc_config((hid_t)-1, &scratch);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_config() accepted invalid file_id.";
        }
    }

    if  ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fget_mdc_config() 2.\n", FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Fget_mdc_config(file_id, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_config() accepted NULL config_ptr.";
        }
    }

    scratch.version = -1; /* a convenient, invalid value */
    if  ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fget_mdc_config() 3.\n", FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Fget_mdc_config(file_id, &scratch);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_config() accepted bad config version.";
        }
    }


    /* test H5Fset_mdc_config() */

    scratch.version = H5C__CURR_AUTO_SIZE_CTL_VER;
    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fset_mdc_config() 1.\n", FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Fset_mdc_config((hid_t)-1, &default_config);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() accepted bad invalid file_id.";
        }
    }

    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fset_mdc_config() 2.\n", FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Fset_mdc_config(file_id, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fset_mdc_config() accepted NULL config_ptr.";
        }
    }

    i = 0;
    while ( ( pass ) && ( i < NUM_INVALID_CONFIGS ) )
    {
        if ( show_progress ) {

            HDfprintf(stdout,
                "%s: testing H5Fset_mdc_config() with invalid config %d.\n",
                FUNC, i);
        }

        H5E_BEGIN_TRY {
            result = H5Fset_mdc_config(file_id, &(invalid_configs[i]));
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                   "H5Fset_mdc_config() accepted invalid_configs[%d].", i);
            failure_mssg = msg;
        }
        i++;
    }

    /* verify that none of the above calls to H5Fset_mdc_config() changed
     * the configuration in the FAPL.
     */
    validate_mdc_config(file_id, &default_config, TRUE, 2);


    /* test H5Fget_mdc_hit_rate() */
    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fget_mdc_hit_rate() 1.\n",
                FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Fget_mdc_hit_rate((hid_t)-1, &hit_rate);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_hit_rate() accepted bad file_id.";
        }
    }

    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fget_mdc_hit_rate() 2.\n",
                FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Fget_mdc_hit_rate(file_id, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_hit_rate() accepted NULL hit_rate_ptr.";
        }
    }


    /* test H5Freset_mdc_hit_rate_stats() */
    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Freset_mdc_hit_rate_stats().\n",
                FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Freset_mdc_hit_rate_stats((hid_t)-1);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg =
                "H5Freset_mdc_hit_rate_stats() accepted bad file_id.";
        }
    }


    /* test H5Fget_mdc_size() */
    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fget_mdc_size() 1.\n", FUNC);
        }

        H5E_BEGIN_TRY {
            result = H5Fget_mdc_size((hid_t)-1, &max_size, &min_clean_size,
                                     &cur_size, &cur_num_entries);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_size() accepted bad file_id.";
        }
    }

    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: testing H5Fget_mdc_size() 2.\n", FUNC);
        }

        if ( ( H5Fget_mdc_size(file_id, &max_size, NULL, NULL, NULL) < 0 ) ||
             ( H5Fget_mdc_size(file_id, NULL, &min_clean_size,
                               NULL, NULL) < 0 ) ||
             ( H5Fget_mdc_size(file_id, NULL, NULL, &cur_size, NULL) < 0 ) ||
             ( H5Fget_mdc_size(file_id, NULL, NULL, NULL,
                               &cur_num_entries) < 0 ) ||
             ( H5Fget_mdc_size(file_id, NULL, NULL, NULL, NULL) < 0 ) ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_size() failed to handle NULL params.";
        }
    }


    /* close the file and delete it */
    if ( pass ) {

        if ( show_progress ) {

            HDfprintf(stdout, "%s: cleaning up from tests.\n", FUNC);
        }

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        } else if ( HDremove(filename) < 0 ) {

            pass = FALSE;
            failure_mssg = "HDremove() failed.\n";
        }
    }

    if ( pass ) {

        PASSED();

    } else {

        H5_FAILED();
    }

    if ( ! pass ) {

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", FUNC, failure_mssg);
    }

    return pass;

} /* check_file_mdc_api_errs() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run tests on the cache code contained in H5C.c
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:  John Mainzer
 *              6/24/04
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned nerrs = 0;
    int express_test;

    H5open();

    express_test = GetTestExpress();

    printf("===================================\n");
    printf("Cache API tests\n");
    printf("        express_test = %d\n", express_test);
    printf("===================================\n");


    /* Initialize invalid configurations.
     */
    invalid_configs = init_invalid_configs();

    if ( NULL == invalid_configs ) {

        failure_mssg = "Unable to allocate memory for invalid configs.";
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n", FUNC, failure_mssg);
        return EXIT_FAILURE;
    }

    if ( !check_fapl_mdc_api_calls() ) {

        nerrs += 1;
    }

    if ( !check_file_mdc_api_calls() ) {

        nerrs += 1;
    }

    if ( !mdc_api_call_smoke_check(express_test) ) {

        nerrs += 1;
    }

    if ( !check_fapl_mdc_api_errs() ) {

        nerrs += 1;
    }

    if ( !check_file_mdc_api_errs() ) {

        nerrs += 1;
    }

    if ( invalid_configs ) {

        HDfree(invalid_configs);
    }

    if ( nerrs > 0 ) {

        return EXIT_FAILURE;

    } else {

        return EXIT_SUCCESS;
    }

} /* main() */

