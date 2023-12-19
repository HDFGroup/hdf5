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

/*
 * Purpose: A library for routines that are common
 *          amongst the various HDF5 tools.
 */

#include "h5tools.h"
#include "h5tools_dump.h"
#include "h5tools_ref.h"
#include "h5tools_utils.h"
#include "H5private.h"

#ifdef H5_TOOLS_DEBUG
/* global debug variables */
int H5tools_INDENT_g = 0;
#endif

/* global variables */
H5E_auto2_t lib_func;
H5E_auto2_t tools_func;
void       *lib_edata;
void       *tools_edata;

hid_t H5tools_ERR_STACK_g     = H5I_INVALID_HID;
hid_t H5tools_ERR_CLS_g       = H5I_INVALID_HID;
hid_t H5E_tools_g             = H5I_INVALID_HID;
hid_t H5E_tools_min_id_g      = H5I_INVALID_HID;
hid_t H5E_tools_min_info_id_g = H5I_INVALID_HID;
hid_t H5E_tools_min_dbg_id_g  = H5I_INVALID_HID;

FILE *rawattrstream  = NULL; /* should initialize to stdout but gcc moans about it */
FILE *rawdatastream  = NULL; /* should initialize to stdout but gcc moans about it */
FILE *rawinstream    = NULL; /* should initialize to stdin but gcc moans about it */
FILE *rawoutstream   = NULL; /* should initialize to stdout but gcc moans about it */
FILE *rawerrorstream = NULL; /* should initialize to stderr but gcc moans about it */

int bin_output;       /* binary output */
int bin_form = 0;     /* binary form, default NATIVE */
int region_output;    /* region output */
int oid_output;       /* oid output */
int data_output;      /* data output */
int attr_data_output; /* attribute data output */

unsigned           packed_bits_num;    /* number of packed bits to display */
unsigned           packed_data_offset; /* offset of packed bits to display */
unsigned           packed_data_length; /* length of packed bits to display */
unsigned long long packed_data_mask;   /* mask in which packed bits to display */

int enable_error_stack = 0; /* re-enable error stack; disable=0 enable=1 */

/* sort parameters */
H5_index_t      sort_by    = H5_INDEX_NAME; /* sort_by [creation_order | name]  */
H5_iter_order_t sort_order = H5_ITER_INC;   /* sort_order [ascending | descending] */

/* module-scoped variables */
static int h5tools_init_g; /* if h5tools lib has been initialized */

/* Names of VOL connectors */
const char *volnames[] = {
    H5VL_NATIVE_NAME,
    H5VL_PASSTHRU_NAME,
};

/* Names of VFDs. These names are always available so that
 * the tools can emit special messages when a VFD is asked
 * for by name but is not compiled into the library or is
 * somehow otherwise not enabled.
 *
 */
const char *drivernames[] = {
    [SEC2_VFD_IDX]      = "sec2",
    [DIRECT_VFD_IDX]    = "direct",
    [LOG_VFD_IDX]       = "log",
    [WINDOWS_VFD_IDX]   = "windows",
    [STDIO_VFD_IDX]     = "stdio",
    [CORE_VFD_IDX]      = "core",
    [FAMILY_VFD_IDX]    = "family",
    [SPLIT_VFD_IDX]     = "split",
    [MULTI_VFD_IDX]     = "multi",
    [MPIO_VFD_IDX]      = "mpio",
    [ROS3_VFD_IDX]      = "ros3",
    [HDFS_VFD_IDX]      = "hdfs",
    [SUBFILING_VFD_IDX] = H5FD_SUBFILING_NAME,
    [ONION_VFD_IDX]     = "onion",
};

#define NUM_VOLS    (sizeof(volnames) / sizeof(volnames[0]))
#define NUM_DRIVERS (sizeof(drivernames) / sizeof(drivernames[0]))

/*-------------------------------------------------------------------------
 * Function: h5tools_init
 *
 * Purpose:  This should be called before any other h5tools function is called.
 *           Effect of any h5tools function called before this has been called is
 *           undetermined.
 *
 * Return    None
 *-------------------------------------------------------------------------
 */
void
h5tools_init(void)
{
    /* Disable error reporting */
    H5Eget_auto2(H5E_DEFAULT, &lib_func, &lib_edata);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    if (!h5tools_init_g) {
        H5TOOLS_INIT_ERROR();

        if (!rawattrstream)
            rawattrstream = stdout;
        if (!rawdatastream)
            rawdatastream = stdout;
        if (!rawinstream)
            rawinstream = stdin;
        if (!rawoutstream)
            rawoutstream = stdout;
        if (!rawerrorstream)
            rawerrorstream = stderr;

        h5tools_dump_init();

        h5tools_init_g++;
    }

    /* Disable tools error reporting */
    H5Eget_auto2(H5tools_ERR_STACK_g, &tools_func, &tools_edata);
    H5Eset_auto2(H5tools_ERR_STACK_g, NULL, NULL);
}

/*-------------------------------------------------------------------------
 * Function: h5tools_error_report
 *
 * Purpose:  Enable error stack reporting after command line is parsed.
 *
 * Return:   None
 *-------------------------------------------------------------------------
 */
void
h5tools_error_report(void)
{
    if (h5tools_init_g) {
        if (enable_error_stack > 0) {
            H5Eset_auto2(H5E_DEFAULT, lib_func, lib_edata);
            H5Eset_auto2(H5tools_ERR_STACK_g, tools_func, tools_edata);
        }
    }
}

/*-------------------------------------------------------------------------
 * Function: h5tools_close
 *
 * Purpose:  Close or release resources such as files opened by the library. This
 *           should be called after all other h5tools functions have been called.
 *           Effect of any h5tools function called after this has been called is
 *           undetermined.
 *
 * Return:   None
 *-------------------------------------------------------------------------
 */
void
h5tools_close(void)
{
    if (h5tools_init_g) {
        /* special case where only data is output to stdout */
        if ((rawoutstream == NULL) && rawdatastream && (rawdatastream == stdout))
            fprintf(rawdatastream, "\n");

        if (tools_func)
            H5Eprint2(H5tools_ERR_STACK_g, rawerrorstream);

        if (rawattrstream && rawattrstream != stdout) {
            if (fclose(rawattrstream))
                perror("closing rawattrstream");
            else
                rawattrstream = NULL;
        }
        if (rawdatastream && rawdatastream != stdout) {
            if (fclose(rawdatastream))
                perror("closing rawdatastream");
            else
                rawdatastream = NULL;
        }
        if (rawinstream && rawinstream != stdin) {
            if (fclose(rawinstream))
                perror("closing rawinstream");
            else
                rawinstream = NULL;
        }
        if (rawoutstream && rawoutstream != stdout) {
            if (fclose(rawoutstream))
                perror("closing rawoutstream");
            else
                rawoutstream = NULL;
        }
        if (rawerrorstream && rawerrorstream != stderr) {
            if (fclose(rawerrorstream))
                perror("closing rawerrorstream");
            else
                rawerrorstream = NULL;
        }

        /* Clean up the reference path table, if it's been used */
        term_ref_path_table();

        /* Restore error stacks from init */
        H5Eset_auto2(H5tools_ERR_STACK_g, tools_func, tools_edata);
        H5Eset_auto2(H5E_DEFAULT, lib_func, lib_edata);

        H5TOOLS_CLOSE_ERROR();

        /* Shut down the library */
        H5close();

        h5tools_init_g = 0;
    }
}

/*-------------------------------------------------------------------------
 * Function: h5tools_set_data_output_file
 *
 * Purpose:  Open fname as the output file for dataset raw data.
 *           Set rawdatastream as its file stream.
 *
 * Return:   0 -- succeeded
 *           negative -- failed
 *-------------------------------------------------------------------------
 */
int
h5tools_set_data_output_file(const char *fname, int is_bin)
{
    int   retvalue = FAIL;
    FILE *f; /* temporary holding place for the stream pointer
              * so that rawdatastream is changed only when succeeded */

    if (rawdatastream && rawdatastream != stdout) {
        if (fclose(rawdatastream))
            perror("closing rawdatastream");
        else
            rawdatastream = NULL;
    }

    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
            if ((f = fopen(fname, "wb")) != NULL) {
                rawdatastream = f;
                retvalue      = SUCCEED;
            }
        }
        else {
            if ((f = fopen(fname, "w")) != NULL) {
                rawdatastream = f;
                retvalue      = SUCCEED;
            }
        }
    }
    else {
        rawdatastream = NULL;
        retvalue      = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_set_attr_output_file
 *
 * Purpose:  Open fname as the output file for attribute raw data.
 *           Set rawattrstream as its file stream.
 *
 * Return:   0 -- succeeded
 *           negative -- failed
 *-------------------------------------------------------------------------
 */
int
h5tools_set_attr_output_file(const char *fname, int is_bin)
{
    int   retvalue = FAIL;
    FILE *f; /* temporary holding place for the stream pointer
              * so that rawattrstream is changed only when succeeded */

    if (rawattrstream && rawattrstream != stdout) {
        if (fclose(rawattrstream))
            perror("closing rawattrstream");
        else
            rawattrstream = NULL;
    }

    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
            if ((f = fopen(fname, "wb")) != NULL) {
                rawattrstream = f;
                retvalue      = SUCCEED;
            }
        }
        else {
            if ((f = fopen(fname, "w")) != NULL) {
                rawattrstream = f;
                retvalue      = SUCCEED;
            }
        }
    }
    else {
        rawattrstream = NULL;
        retvalue      = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_set_input_file
 *
 * Purpose:  Open fname as the input file for raw input.
 *           Set rawinstream as its file stream.
 *
 * Return:   0 -- succeeded
 *           negative -- failed
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_set_input_file(const char *fname, int is_bin)
{
    int   retvalue = FAIL;
    FILE *f; /* temporary holding place for the stream pointer
              * so that rawinstream is changed only when succeeded */

    if (rawinstream && rawinstream != stdin) {
        if (fclose(rawinstream))
            perror("closing rawinstream");
        else
            rawinstream = NULL;
    }
    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
            if ((f = fopen(fname, "rb")) != NULL) {
                rawinstream = f;
                retvalue    = SUCCEED;
            }
        }
        else {
            if ((f = fopen(fname, "r")) != NULL) {
                rawinstream = f;
                retvalue    = SUCCEED;
            }
        }
    }
    else {
        rawinstream = NULL;
        retvalue    = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_set_output_file
 *
 * Purpose:  Open fname as the output file for raw output.
 *           Set rawoutstream as its file stream.
 *
 * Return:   0 -- succeeded
 *           negative -- failed
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_set_output_file(const char *fname, int is_bin)
{
    int   retvalue = FAIL;
    FILE *f; /* temporary holding place for the stream pointer
              * so that rawoutstream is changed only when succeeded */

    if (rawoutstream && rawoutstream != stdout) {
        if (fclose(rawoutstream))
            perror("closing rawoutstream");
        else
            rawoutstream = NULL;
    }
    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
            if ((f = fopen(fname, "wb")) != NULL) {
                rawoutstream = f;
                retvalue     = SUCCEED;
            }
        }
        else {
            if ((f = fopen(fname, "w")) != NULL) {
                rawoutstream = f;
                retvalue     = SUCCEED;
            }
        }
    }
    else {
        rawoutstream = NULL;
        retvalue     = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_set_error_file
 *
 * Purpose:  Open fname as the error output file for dataset raw error.
 *           Set rawerrorstream as its file stream.
 *
 * Return:   0 -- succeeded
 *           negative -- failed
 *-------------------------------------------------------------------------
 */
int
h5tools_set_error_file(const char *fname, int is_bin)
{
    int   retvalue = FAIL;
    FILE *f; /* temporary holding place for the stream pointer
              * so that rawerrorstream is changed only when succeeded */

    if (rawerrorstream && rawerrorstream != stderr) {
        if (fclose(rawerrorstream))
            perror("closing rawerrorstream");
        else
            rawerrorstream = NULL;
    }

    /* First check if filename is string "NULL" */
    if (fname != NULL) {
        /* binary output */
        if (is_bin) {
            if ((f = fopen(fname, "wb")) != NULL) {
                rawerrorstream = f;
                retvalue       = SUCCEED;
            }
        }
        else {
            if ((f = fopen(fname, "w")) != NULL) {
                rawerrorstream = f;
                retvalue       = SUCCEED;
            }
        }
    }
    else {
        rawerrorstream = NULL;
        retvalue       = SUCCEED;
    }

    return retvalue;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_set_fapl_vfd
 *
 * Purpose:  Given a VFL driver name or ID, sets the appropriate driver on
 *           the specified FAPL.
 *
 * Return:   positive - succeeded
 *           negative - failed
 *-------------------------------------------------------------------------
 */
static herr_t
h5tools_set_fapl_vfd(hid_t fapl_id, h5tools_vfd_info_t *vfd_info)
{
    herr_t ret_value = SUCCEED;

    switch (vfd_info->type) {
        case VFD_BY_NAME:
            /* Determine which driver the user wants to open the file with */
            if (!strcmp(vfd_info->u.name, drivernames[SEC2_VFD_IDX])) {
                /* SEC2 Driver */
                if (H5Pset_fapl_sec2(fapl_id) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_sec2 failed");
            }
            else if (!strcmp(vfd_info->u.name, drivernames[DIRECT_VFD_IDX])) {
#ifdef H5_HAVE_DIRECT
                /* Direct Driver */
                if (H5Pset_fapl_direct(fapl_id, 1024, 4096, 8 * 4096) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_direct failed");
#else
                H5TOOLS_GOTO_ERROR(FAIL, "Direct VFD is not enabled");
#endif
            }
            else if (!strcmp(vfd_info->u.name, drivernames[LOG_VFD_IDX])) {
                unsigned long long log_flags = H5FD_LOG_LOC_IO | H5FD_LOG_ALLOC;

                /* Log Driver */
                if (H5Pset_fapl_log(fapl_id, NULL, log_flags, (size_t)0) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_log failed");
            }
            else if (!strcmp(vfd_info->u.name, drivernames[WINDOWS_VFD_IDX])) {
#ifdef H5_HAVE_WINDOWS
                /* There is no Windows VFD - use SEC2 */
                if (H5Pset_fapl_sec2(fapl_id) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_sec2 failed");
#else
                H5TOOLS_GOTO_ERROR(FAIL, "Windows VFD is not enabled");
#endif
            }
            else if (!strcmp(vfd_info->u.name, drivernames[STDIO_VFD_IDX])) {
                /* Stdio Driver */
                if (H5Pset_fapl_stdio(fapl_id) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_stdio failed");
            }
            else if (!strcmp(vfd_info->u.name, drivernames[CORE_VFD_IDX])) {
                /* Core Driver */
                if (H5Pset_fapl_core(fapl_id, (size_t)H5_MB, true) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_core failed");
            }
            else if (!strcmp(vfd_info->u.name, drivernames[FAMILY_VFD_IDX])) {
                /* FAMILY Driver */
                /* Set member size to be 0 to indicate the current first member size
                 * is the member size.
                 */
                if (H5Pset_fapl_family(fapl_id, (hsize_t)0, H5P_DEFAULT) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_family failed");
            }
            else if (!strcmp(vfd_info->u.name, drivernames[SPLIT_VFD_IDX])) {
                /* SPLIT Driver */
                if (H5Pset_fapl_split(fapl_id, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_split failed");
            }
            else if (!strcmp(vfd_info->u.name, drivernames[MULTI_VFD_IDX])) {
                /* MULTI Driver */
                if (H5Pset_fapl_multi(fapl_id, NULL, NULL, NULL, NULL, true) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_multi failed");
            }
            else if (!strcmp(vfd_info->u.name, drivernames[MPIO_VFD_IDX])) {
#ifdef H5_HAVE_PARALLEL
                int mpi_initialized, mpi_finalized;

                /* MPI-I/O Driver */

                /* check if MPI is available. */
                MPI_Initialized(&mpi_initialized);
                MPI_Finalized(&mpi_finalized);

                if (mpi_initialized && !mpi_finalized) {
                    if (H5Pset_fapl_mpio(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL) < 0)
                        H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_mpio failed");
                }
#else
                H5TOOLS_GOTO_ERROR(FAIL, "MPI-I/O VFD is not enabled");
#endif /* H5_HAVE_PARALLEL */
            }
            else if (!strcmp(vfd_info->u.name, drivernames[ROS3_VFD_IDX])) {
#ifdef H5_HAVE_ROS3_VFD
                if (!vfd_info->info)
                    H5TOOLS_GOTO_ERROR(FAIL, "Read-only S3 VFD info is invalid");
                if (H5Pset_fapl_ros3(fapl_id, &((const H5FD_ros3_fapl_ext_t *)vfd_info->info)->fa) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_ros3() failed");

                if (H5Pset_fapl_ros3_token(fapl_id, ((const H5FD_ros3_fapl_ext_t *)vfd_info->info)->token) <
                    0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_ros3_token() failed");
#else
                H5TOOLS_GOTO_ERROR(FAIL, "Read-only S3 VFD is not enabled");
#endif
            }
            else if (!strcmp(vfd_info->u.name, drivernames[HDFS_VFD_IDX])) {
#ifdef H5_HAVE_LIBHDFS
                if (!vfd_info->info)
                    H5TOOLS_GOTO_ERROR(FAIL, "HDFS VFD info is invalid");
                if (H5Pset_fapl_hdfs(fapl_id, (H5FD_hdfs_fapl_t *)vfd_info->info) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_hdfs() failed");
#else
                H5TOOLS_GOTO_ERROR(FAIL, "The HDFS VFD is not enabled");
#endif
            }
            else if (!strcmp(vfd_info->u.name, drivernames[SUBFILING_VFD_IDX])) {
#if defined(H5_HAVE_PARALLEL) && defined(H5_HAVE_SUBFILING_VFD)
                if (H5Pset_fapl_subfiling(fapl_id, (const H5FD_subfiling_config_t *)vfd_info->info) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_subfiling() failed");
#else
                H5TOOLS_GOTO_ERROR(FAIL, "The Subfiling VFD is not enabled");
#endif
            }
            else if (!strcmp(vfd_info->u.name, drivernames[ONION_VFD_IDX])) {
                /* Onion driver */
                if (!vfd_info->info)
                    H5TOOLS_GOTO_ERROR(FAIL, "Onion VFD info is invalid");
                if (H5Pset_fapl_onion(fapl_id, (const H5FD_onion_fapl_info_t *)vfd_info->info) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_onion() failed");
            }
            else {
                /*
                 * Try to load VFD plugin.
                 *
                 * Currently, driver configuration strings are unsupported.
                 */
                if (H5Pset_driver_by_name(fapl_id, vfd_info->u.name, (const char *)vfd_info->info) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "can't load VFD plugin by driver name '%s'", vfd_info->u.name);
            }

            break;

        case VFD_BY_VALUE:
            /*
             * Try to load VFD plugin.
             *
             * Currently, driver configuration strings are unsupported.
             */

            if (vfd_info->u.value == H5_VFD_SUBFILING) {
#if defined(H5_HAVE_PARALLEL) && defined(H5_HAVE_SUBFILING_VFD)
                if (H5Pset_fapl_subfiling(fapl_id, (const H5FD_subfiling_config_t *)vfd_info->info) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "H5Pset_fapl_subfiling() failed");
#else
                H5TOOLS_GOTO_ERROR(FAIL, "The Subfiling VFD is not enabled");
#endif
            }
            else {
                if (H5Pset_driver_by_value(fapl_id, vfd_info->u.value, (const char *)vfd_info->info) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "can't load VFD plugin by driver value '%ld'",
                                       (long int)vfd_info->u.value);
            }
            break;

        default:
            H5TOOLS_GOTO_ERROR(FAIL, "invalid VFD retrieval type");
    }

done:
    if (ret_value < 0) {
        /* Clear error message unless asked for */
        if ((H5tools_ERR_STACK_g >= 0) && (enable_error_stack <= 1))
            H5Epop(H5tools_ERR_STACK_g, 1);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_set_fapl_vol
 *
 * Purpose:  Given a VOL connector name or ID, sets the appropriate
 *           connector on the specified FAPL.
 *
 * Return:   positive - succeeded
 *           negative - failed
 *-------------------------------------------------------------------------
 */
static herr_t
h5tools_set_fapl_vol(hid_t fapl_id, h5tools_vol_info_t *vol_info)
{
    htri_t connector_is_registered;
    hid_t  connector_id   = H5I_INVALID_HID;
    void  *connector_info = NULL;
    herr_t ret_value      = SUCCEED;

    switch (vol_info->type) {
        case VOL_BY_NAME:
            /* Retrieve VOL connector by name */
            if ((connector_is_registered = H5VLis_connector_registered_by_name(vol_info->u.name)) < 0)
                H5TOOLS_GOTO_ERROR(FAIL, "can't check if VOL connector is registered");
            if (connector_is_registered) {
                if ((connector_id = H5VLget_connector_id_by_name(vol_info->u.name)) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "can't get VOL connector ID");
            }
            else {
                /* Check for VOL connectors that ship with the library, then try
                 * registering by name if that fails.
                 */
                if (!strcmp(vol_info->u.name, H5VL_NATIVE_NAME)) {
                    connector_id = H5VL_NATIVE;
                }
                else if (!strcmp(vol_info->u.name, H5VL_PASSTHRU_NAME)) {
                    connector_id = H5VL_PASSTHRU;
                }
                else {
                    /* NOTE: Not being able to pass in a VIPL may be a limitation for some
                     * connectors.
                     */
                    if ((connector_id = H5VLregister_connector_by_name(vol_info->u.name, H5P_DEFAULT)) < 0)
                        H5TOOLS_GOTO_ERROR(FAIL, "can't register VOL connector");
                }
            }

            break;

        case VOL_BY_VALUE:
            /* Retrieve VOL connector by ID */
            if ((connector_is_registered = H5VLis_connector_registered_by_value(vol_info->u.value)) < 0)
                H5TOOLS_GOTO_ERROR(FAIL, "can't check if VOL connector is registered");
            if (connector_is_registered) {
                if ((connector_id = H5VLget_connector_id_by_value(vol_info->u.value)) < 0)
                    H5TOOLS_GOTO_ERROR(FAIL, "can't get VOL connector ID");
            }
            else {
                /* Check for VOL connectors that ship with the library */
                if (vol_info->u.value == H5VL_NATIVE_VALUE) {
                    connector_id = H5VL_NATIVE;
                }
                else if (vol_info->u.value == H5VL_PASSTHRU_VALUE) {
                    connector_id = H5VL_PASSTHRU;
                }
                else {
                    /* NOTE: Not being able to pass in a VIPL may be a limitation for some
                     * connectors.
                     */
                    if ((connector_id = H5VLregister_connector_by_value(vol_info->u.value, H5P_DEFAULT)) < 0)
                        H5TOOLS_GOTO_ERROR(FAIL, "can't register VOL connector");
                }
            }

            break;

        default:
            H5TOOLS_GOTO_ERROR(FAIL, "invalid VOL retrieval type");
    }

    /* Convert the info string, if provided */
    if (vol_info->info_string)
        if (H5VLconnector_str_to_info(vol_info->info_string, connector_id, &connector_info) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "can't get VOL connector info from string");

    /* Set the VOL connector on the fapl */
    if (H5Pset_vol(fapl_id, connector_id, connector_info) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "can't set VOL connector on FAPL");

done:
    if (connector_info)
        if (H5VLfree_connector_info(connector_id, connector_info))
            H5TOOLS_ERROR(FAIL, "failed to free VOL connector-specific info");

    if (ret_value < 0) {
        if (connector_id >= 0 && H5Idec_ref(connector_id) < 0)
            H5TOOLS_ERROR(FAIL, "failed to decrement refcount on VOL connector ID");

        /* Clear error message unless asked for */
        if ((H5tools_ERR_STACK_g >= 0) && (enable_error_stack <= 1))
            H5Epop(H5tools_ERR_STACK_g, 1);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_get_fapl
 *
 * Purpose:  Copies an input fapl and then sets a VOL and/or a VFD on it.
 *
 *           The returned fapl must be closed by the caller.
 *
 * Return:   positive - succeeded
 *           negative - failed
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_get_fapl(hid_t prev_fapl_id, h5tools_vol_info_t *vol_info, h5tools_vfd_info_t *vfd_info)
{
    hid_t new_fapl_id = H5I_INVALID_HID;
    hid_t ret_value   = H5I_INVALID_HID;

    if (prev_fapl_id < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "invalid FAPL");

    /* Make a copy of the FAPL or create one if H5P_DEFAULT is specified. */
    if (H5P_DEFAULT == prev_fapl_id) {
        if ((new_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
            H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "H5Pcreate failed");
    }
    else {
        if ((new_fapl_id = H5Pcopy(prev_fapl_id)) < 0)
            H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "H5Pcopy failed");
    }

    /* Set non-default VOL connector, if requested */
    if (vol_info)
        if (h5tools_set_fapl_vol(new_fapl_id, vol_info) < 0)
            H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "failed to set VOL on FAPL");

    /* Set non-default virtual file driver, if requested */
    if (vfd_info)
        if (h5tools_set_fapl_vfd(new_fapl_id, vfd_info) < 0)
            H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "failed to set VFD on FAPL");

    ret_value = new_fapl_id;

done:
    if (ret_value < 0) {
        if (new_fapl_id >= 0) {
            H5Pclose(new_fapl_id);
            new_fapl_id = H5I_INVALID_HID;
        }

        /* Clear error message unless asked for */
        if ((H5tools_ERR_STACK_g >= 0) && (enable_error_stack <= 1))
            H5Epop(H5tools_ERR_STACK_g, 1);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_get_vfd_name
 *
 * Purpose:  Given a FAPL, retrieves the name of the VFL driver set on it
 *           if using a native-terminal VOL connector. If a
 *           non-native-terminal VOL connector is set on the FAPL, the
 *           first byte of the returned driver name will be set to the null
 *           terminator.
 *
 * Return:   SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
h5tools_get_vfd_name(hid_t fid, hid_t fapl_id, char *drivername, size_t drivername_size)
{
    hid_t  fapl_vol_id = H5I_INVALID_HID;
    bool   is_native   = false;
    herr_t ret_value   = SUCCEED;

    if (fapl_id < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "invalid FAPL");
    if (!drivername)
        H5TOOLS_GOTO_ERROR(FAIL, "drivername is NULL");
    if (drivername && !drivername_size)
        H5TOOLS_GOTO_ERROR(FAIL, "drivername_size must be non-zero");

    /* Initialize the driver name */
    drivername[0] = '\0';

    if (fapl_id == H5P_DEFAULT)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;

    /* Retrieve ID of the VOL connector set on the FAPL */
    if (H5Pget_vol_id(fapl_id, &fapl_vol_id) < 0)
        H5TOOLS_ERROR(FAIL, "failed to retrieve VOL ID from FAPL");

    /* Query if the file ID is native-terminal */
    if (H5VLobject_is_native(fid, &is_native) < 0)
        H5TOOLS_ERROR(FAIL, "failed to determine if file ID is native-terminal");

    if (is_native) {
        const char *driver_name;
        hid_t       driver_id;

        if ((driver_id = H5Pget_driver(fapl_id)) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "failed to retrieve VFL driver ID from FAPL");

        if (driver_id == H5FD_SEC2)
            driver_name = drivernames[SEC2_VFD_IDX];
#ifdef H5_HAVE_DIRECT
        else if (driver_id == H5FD_DIRECT)
            driver_name = drivernames[DIRECT_VFD_IDX];
#endif
        else if (driver_id == H5FD_LOG)
            driver_name = drivernames[LOG_VFD_IDX];
#ifdef H5_HAVE_WINDOWS
        else if (driver_id == H5FD_WINDOWS)
            driver_name = drivernames[WINDOWS_VFD_IDX];
#endif
        else if (driver_id == H5FD_STDIO)
            driver_name = drivernames[STDIO_VFD_IDX];
        else if (driver_id == H5FD_CORE)
            driver_name = drivernames[CORE_VFD_IDX];
        else if (driver_id == H5FD_FAMILY)
            driver_name = drivernames[FAMILY_VFD_IDX];
        else if (driver_id == H5FD_MULTI)
            driver_name = drivernames[MULTI_VFD_IDX];
#ifdef H5_HAVE_PARALLEL
        else if (driver_id == H5FD_MPIO)
            driver_name = drivernames[MPIO_VFD_IDX];
#endif
#ifdef H5_HAVE_ROS3_VFD
        else if (driver_id == H5FD_ROS3)
            driver_name = drivernames[ROS3_VFD_IDX];
#endif
#ifdef H5_HAVE_LIBHDFS
        else if (driver_id == H5FD_HDFS)
            driver_name = drivernames[HDFS_VFD_IDX];
#endif
#ifdef H5_HAVE_SUBFILING_VFD
        else if (driver_id == H5FD_SUBFILING)
            driver_name = drivernames[SUBFILING_VFD_IDX];
#endif
        else if (driver_id == H5FD_ONION)
            driver_name = drivernames[ONION_VFD_IDX];
        else
            driver_name = "unknown";

        strncpy(drivername, driver_name, drivername_size);
        drivername[drivername_size - 1] = '\0';
    }

done:
    /* Close retrieved VOL ID */
    if (fapl_vol_id >= 0)
        if (H5VLclose(fapl_vol_id) < 0)
            H5TOOLS_ERROR(FAIL, "failed to close VOL ID");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_fopen
 *
 * Purpose:  Opens file FNAME using the specified flags and FAPL.
 *
 *           The 'use_specific_driver' parameter is used to control the
 *           VFD/VOL connector that this routine uses to open the file
 *           with. If 'use_specific_driver' is set to true, this routine
 *           assumes that the caller has already set a specific VFD or VOL
 *           connector on the given FAPL and will attempt to directly use
 *           the FAPL for opening the file. We assume that the caller knows
 *           what they are doing; if the file is unable to be opened using
 *           that FAPL, this routine will return H5I_INVALID_HID.
 *
 *           However, if 'use_specific_driver' is set to false, this
 *           routine assumes that the caller HAS NOT set a specific VFD or
 *           VOL connector on the given FAPL and will instead loop through
 *           the various available VFL drivers and VOL connectors trying to
 *           open FNAME.
 *
 *           The list of available VFL drivers is as follows:
 *             - If the HDF5 library is version 1.2 or less, then we have
 *               only the SEC2 driver to try out.
 *             - If the HDF5 library is greater than version 1.2, then we
 *               have the FAMILY, SPLIT, and MULTI drivers to play with.
 *
 *           The list of available VOL connectors is as follows:
 *             - "Native" VOL connector
 *             - Pass-through VOL connector
 *
 * Return:
 *      On success, returns a file ID for the opened file. If DRIVERNAME is
 *      non-null and the native VOL connector is the terminal connector,
 *      then the first DRIVERNAME_SIZE-1 characters of the driver name are
 *      copied into the DRIVERNAME array and null terminated. If the
 *      native VOL connector is NOT the terminal connector, then the first
 *      byte of DRIVERNAME will be set to the null terminator.
 *
 *      On failure, the function returns H5I_INVALID_HID and DRIVERNAME
 *      will not be set.
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_fopen(const char *fname, unsigned flags, hid_t fapl_id, bool use_specific_driver, char *drivername,
              size_t drivername_size)
{
    hid_t    fid          = H5I_INVALID_HID;
    hid_t    tmp_fapl_id  = H5I_INVALID_HID;
    hid_t    used_fapl_id = H5I_INVALID_HID;
    unsigned volnum, drivernum;
    hid_t    ret_value = H5I_INVALID_HID;

    /*
     * First try to open the file using just the given FAPL. If the
     * HDF5_VOL_CONNECTOR environment variable has been set, this will
     * allow us to attempt to open the file using the specified VOL
     * connector before we go looping through all available ones,
     * which will override any VOL connector set by use of the
     * environment variable.
     */

    /* Allow error stack display if --enable-error-stack has optional arg number */
    if (enable_error_stack > 1) {
        fid = H5Fopen(fname, flags, fapl_id);
    }
    else {
        H5E_BEGIN_TRY
        {
            fid = H5Fopen(fname, flags, fapl_id);
        }
        H5E_END_TRY
    }

    /* If we succeeded in opening the file, we're done. */
    if (fid >= 0) {
        used_fapl_id = fapl_id;
        H5TOOLS_GOTO_DONE(fid);
    }

    /*
     * If we failed to open the file and the caller specified 'use_specific_driver'
     * as true, we should return failure now since the file couldn't be opened with
     * the VFL driver/VOL connector that was set on the FAPL by the caller.
     */
    if (use_specific_driver)
        H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "failed to open file using specified FAPL");

    /*
     * As a final resort, try to open the file using each of the available
     * VOL connectors. When the native VOL connector is the current "terminal"
     * connector being looked at, also try using each of the available VFL drivers.
     */
    for (volnum = 0; volnum < NUM_VOLS; volnum++) {
        h5tools_vol_info_t vol_info;

        vol_info.type        = VOL_BY_NAME;
        vol_info.info_string = NULL;
        vol_info.u.name      = volnames[volnum];

        /* TODO: For now, we have no way of determining if an arbitrary
         * VOL connector is native-terminal so we only try VFDs with the
         * actual native VOL connector.
         */
        if (NATIVE_VOL_IDX == volnum) {
            /*
             * If using the native VOL connector, or a VOL connector which has the
             * native connector as its terminal connector, loop through all of the
             * VFL drivers as well.
             */
            for (drivernum = 0; drivernum < NUM_DRIVERS; drivernum++) {
                h5tools_vfd_info_t vfd_info;

                /* Skip the log VFD as it prints out to standard out
                 * and is fundamentally SEC2 anyway.
                 */
                if (drivernum == LOG_VFD_IDX)
                    continue;

                vfd_info.type   = VFD_BY_NAME;
                vfd_info.info   = NULL;
                vfd_info.u.name = drivernames[drivernum];

                /* Get a fapl reflecting the selected VOL connector and VFD */
                if ((tmp_fapl_id = h5tools_get_fapl(fapl_id, &vol_info, &vfd_info)) < 0)
                    continue;

                /* Can we open the file with this combo? */
                H5E_BEGIN_TRY
                {
                    fid = h5tools_fopen(fname, flags, tmp_fapl_id, true, drivername, drivername_size);
                }
                H5E_END_TRY

                if (fid >= 0) {
                    used_fapl_id = tmp_fapl_id;
                    H5TOOLS_GOTO_DONE(fid);
                }
                else {
                    /* Close the temporary fapl */
                    H5Pclose(tmp_fapl_id);
                    tmp_fapl_id = H5I_INVALID_HID;
                }
            }
        }
        else {
            /* NOT the native VOL connector */

            /* Get a FAPL for the current VOL connector */
            if ((tmp_fapl_id = h5tools_get_fapl(fapl_id, &vol_info, NULL)) < 0)
                continue;

            /* Can we open the file with this connector? */
            if ((fid = h5tools_fopen(fname, flags, tmp_fapl_id, true, drivername, drivername_size)) >= 0) {
                used_fapl_id = tmp_fapl_id;
                H5TOOLS_GOTO_DONE(fid);
            }
            else {
                /* Close the temporary VOL FAPL */
                H5Pclose(tmp_fapl_id);
                tmp_fapl_id = H5I_INVALID_HID;
            }
        }
    }

    /* File was unable to be opened at all */
    ret_value = H5I_INVALID_HID;

done:
    /* Save the driver name if using a native-terminal VOL connector */
    if (drivername && drivername_size && ret_value >= 0)
        if (used_fapl_id >= 0 &&
            h5tools_get_vfd_name(ret_value, used_fapl_id, drivername, drivername_size) < 0)
            H5TOOLS_ERROR(H5I_INVALID_HID, "failed to retrieve name of VFD used to open file");

    if (tmp_fapl_id >= 0)
        H5Pclose(tmp_fapl_id);

    /* Clear error message unless asked for */
    if (ret_value < 0) {
        if ((H5tools_ERR_STACK_g >= 0) && (enable_error_stack <= 1))
            H5Epop(H5tools_ERR_STACK_g, 1);
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_count_ncols
 *
 * Purpose:  Count the number of columns in a string. This is the number of
 *           characters in the string not counting line-control characters.
 *
 * Return:   success - returns the width of the string.
 *           failure - 0.
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE static size_t
h5tools_count_ncols(const char *s)
{
    size_t i;

    for (i = 0; *s; s++)
        if (*s >= ' ')
            i++;

    return i;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_detect_vlen
 *
 * Purpose:  Recursive check for any variable length data in given type.
 *
 * Return:   true : type contains any variable length data
 *           false : type doesn't contain any variable length data
 *           Negative value: failed
 *-------------------------------------------------------------------------
 */
htri_t
h5tools_detect_vlen(hid_t tid)
{
    htri_t ret = false;

    /* recursive detect any vlen data values in type (compound, array ...) */
    ret = H5Tdetect_class(tid, H5T_VLEN);
    if ((ret == true) || (ret < 0))
        goto done;

    /* recursive detect any vlen string in type (compound, array ...) */
    ret = h5tools_detect_vlen_str(tid);
    if ((ret == true) || (ret < 0))
        goto done;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_detect_vlen_str
 *
 * Purpose:  Recursive check for variable length string of a datatype.
 *
 * Return:   true : type contains any variable length string
 *           false : type doesn't contain any variable length string
 *           Negative value: failed
 *-------------------------------------------------------------------------
 */
htri_t
h5tools_detect_vlen_str(hid_t tid)
{
    H5T_class_t tclass = -1;
    htri_t      ret    = false;

    ret = H5Tis_variable_str(tid);
    if ((ret == true) || (ret < 0))
        goto done;

    tclass = H5Tget_class(tid);
    if (tclass == H5T_ARRAY || tclass == H5T_VLEN) {
        hid_t btid = H5Tget_super(tid);

        if (btid < 0) {
            ret = (htri_t)btid;
            goto done;
        }
        ret = h5tools_detect_vlen_str(btid);
        if ((ret == true) || (ret < 0)) {
            H5Tclose(btid);
            goto done;
        }
    }
    else if (tclass == H5T_COMPOUND) {
        unsigned nmembs;
        int      snmembs = H5Tget_nmembers(tid);
        unsigned u;

        if (snmembs < 0) {
            ret = FAIL;
            goto done;
        }
        nmembs = (unsigned)snmembs;

        for (u = 0; u < nmembs; u++) {
            hid_t mtid = H5Tget_member_type(tid, u);

            ret = h5tools_detect_vlen_str(mtid);
            if ((ret == true) || (ret < 0)) {
                H5Tclose(mtid);
                goto done;
            }
            H5Tclose(mtid);
        }
    }

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_simple_prefix
 *
 * Purpose:  If /ctx->need_prefix/ is set then terminate the current line (if
 *           applicable), calculate the prefix string, and display it at the start
 *           of a line.
 *
 * Return:   None
 *-------------------------------------------------------------------------
 */
void
h5tools_simple_prefix(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hsize_t elmtno,
                      int secnum)
{
    h5tools_str_t prefix;
    h5tools_str_t str; /*temporary for indentation */
    size_t        templength = 0;
    unsigned      u, indentlevel = 0;

    if (stream == NULL)
        return;

    if (!ctx->need_prefix)
        return;

    H5TOOLS_START_DEBUG(" ");

    memset(&prefix, 0, sizeof(h5tools_str_t));
    memset(&str, 0, sizeof(h5tools_str_t));

    /* Terminate previous line, if any */
    H5TOOLS_DEBUG("before CR elmtno=%ld, ctx->cur_column=%d, info->idx_fmt=%s, info->line_suf=%s", elmtno,
                  ctx->cur_column, info->idx_fmt, info->line_suf);
    if (ctx->cur_column) {
        PUTSTREAM(OPT(info->line_suf, ""), stream);
        putc('\n', stream);
        PUTSTREAM(OPT(info->line_sep, ""), stream);
    }
    H5TOOLS_DEBUG("after CR elmtno=%ld, ctx->ndims=%d", elmtno, ctx->ndims);

    /* Calculate new prefix */
    h5tools_str_prefix(&prefix, info, elmtno, ctx);
    H5TOOLS_DEBUG("prefix=%s - str=%s", prefix.s, str.s);

    /* Write new prefix to output */
    if (ctx->indent_level > 0)
        indentlevel = ctx->indent_level;
    else
        /*
         * This is because sometimes we don't print out all the header
         * info for the data (like the tattr-2.ddl example). If that happens
         * the ctx->indent_level is negative so we need to skip the above and
         * just print out the default indent levels.
         */
        indentlevel = ctx->default_indent_level;

    /* when printing array indices, print the indentation before the prefix
       the prefix is printed one indentation level before */
    if (info->pindex)
        for (u = 0; u < indentlevel - 1; u++)
            PUTSTREAM(h5tools_str_fmt(&str, (size_t)0, info->line_indent), stream);

    if (elmtno == 0 && secnum == 0 && info->line_1st)
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_1st), stream);
    else if (secnum && info->line_cont)
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_cont), stream);
    else
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_pre), stream);

    templength = h5tools_str_len(&prefix);
    H5TOOLS_DEBUG("prefix=%s - templength=%d", prefix.s, templength);

    for (u = 0; u < indentlevel; u++) {
        /*we already made the indent for the array indices case */
        if (!info->pindex) {
            PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_indent), stream);
            templength += h5tools_str_len(&prefix);
        }
        else {
            /*we cannot count the prefix for the array indices case */
            templength += h5tools_str_len(&str);
        }
    }
    H5TOOLS_DEBUG("prefix=%s - templength=%d", prefix.s, templength);

    ctx->cur_column = ctx->prev_prefix_len = templength;
    ctx->cur_elmt                          = 0;
    ctx->need_prefix                       = 0;
    H5TOOLS_DEBUG("prefix=%s - str=%s", prefix.s, str.s);

    /* Free string */
    h5tools_str_close(&prefix);
    h5tools_str_close(&str);

    H5TOOLS_ENDDEBUG(" ");
}

/*-------------------------------------------------------------------------
 * Function: h5tools_region_simple_prefix
 *
 * Purpose:  If /ctx->need_prefix/ is set then terminate the current line (if
 *           applicable), calculate the prefix string, and display it at the start
 *           of a line. Calls region specific function.
 *
 * Return:   None
 *-------------------------------------------------------------------------
 */
void
h5tools_region_simple_prefix(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                             hsize_t elmtno, hsize_t *ptdata, int secnum)
{
    h5tools_str_t prefix;
    h5tools_str_t str; /*temporary for indentation */
    size_t        templength = 0;
    unsigned      u, indentlevel = 0;

    if (stream == NULL)
        return;

    if (!ctx->need_prefix)
        return;

    memset(&prefix, 0, sizeof(h5tools_str_t));
    memset(&str, 0, sizeof(h5tools_str_t));

    /* Terminate previous line, if any */
    if (ctx->cur_column) {
        PUTSTREAM(OPT(info->line_suf, ""), stream);
        putc('\n', stream);
        PUTSTREAM(OPT(info->line_sep, ""), stream);
    }

    /* Calculate new prefix */
    h5tools_str_region_prefix(&prefix, info, elmtno, ptdata, ctx);

    /* Write new prefix to output */
    if (ctx->indent_level > 0)
        indentlevel = ctx->indent_level;
    else
        /*
         * This is because sometimes we don't print out all the header
         * info for the data (like the tattr-2.ddl example). If that happens
         * the ctx->indent_level is negative so we need to skip the above and
         * just print out the default indent levels.
         */
        indentlevel = ctx->default_indent_level;

    /* when printing array indices, print the indentation before the prefix
       the prefix is printed one indentation level before */
    if (info->pindex)
        for (u = 0; u < indentlevel - 1; u++)
            PUTSTREAM(h5tools_str_fmt(&str, (size_t)0, info->line_indent), stream);

    if (elmtno == 0 && secnum == 0 && info->line_1st)
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_1st), stream);
    else if (secnum && info->line_cont)
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_cont), stream);
    else
        PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_pre), stream);

    templength = h5tools_str_len(&prefix);

    for (u = 0; u < indentlevel; u++)
        /*we already made the indent for the array indices case */
        if (!info->pindex) {
            PUTSTREAM(h5tools_str_fmt(&prefix, (size_t)0, info->line_indent), stream);
            templength += h5tools_str_len(&prefix);
        }
        else {
            /*we cannot count the prefix for the array indices case */
            templength += h5tools_str_len(&str);
        }

    ctx->cur_column = ctx->prev_prefix_len = templength;
    ctx->cur_elmt                          = 0;
    ctx->need_prefix                       = 0;

    /* Free string */
    h5tools_str_close(&prefix);
    h5tools_str_close(&str);
}

/*-------------------------------------------------------------------------
 * Function: h5tools_render_element
 *
 * Purpose:  Prints the string buffer to the output STREAM. The string is
 *           printed according to the format described in INFO. The CTX struct
 *           contains context information shared between calls to this function.
 *
 * Return:   False if a dimension end is reached
 *           True otherwise
 *
 * In/Out:
 *           h5tools_context_t *ctx
 *           h5tools_str_t *buffer
 *           hsize_t *curr_pos
 *
 * Parameters Description:
 *           h5tools_str_t *buffer is the string into which to render
 *           hsize_t curr_pos is the total data element position
 *           size_t ncols
 *           hsize_t local_elmt_counter is the local element loop counter
 *           hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
bool
h5tools_render_element(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                       h5tools_str_t *buffer, hsize_t *curr_pos, size_t ncols, hsize_t local_elmt_counter,
                       hsize_t elmt_counter)
{
    bool  dimension_break = true;
    char *s               = NULL;
    char *section         = NULL; /* a section of output */
    int   secnum;                 /* section sequence number */
    int   multiline;              /* datum was multiline */

    if (stream == NULL)
        return dimension_break;

    H5TOOLS_START_DEBUG(" need_prefix=%d", ctx->need_prefix);
    H5TOOLS_DEBUG("elmt_counter=%ld - local_elmt_counter=%ld", elmt_counter, local_elmt_counter);

    s = h5tools_str_fmt(buffer, (size_t)0, "%s");
    H5TOOLS_DEBUG("s=%s", s);

    /*
     * If the element would split on multiple lines if printed at our
     * current location...
     */
    if (info->line_multi_new == 1 &&
        (ctx->cur_column + h5tools_count_ncols(s) + strlen(OPT(info->elmt_suf2, " ")) +
         strlen(OPT(info->line_suf, ""))) > ncols) {
        if (ctx->prev_multiline) {
            /*
             * ... and the previous element also occupied more than one
             * line, then start this element at the beginning of a line.
             */
            ctx->need_prefix = true;
        }
        else if ((ctx->prev_prefix_len + h5tools_count_ncols(s) + strlen(OPT(info->elmt_suf2, " ")) +
                  strlen(OPT(info->line_suf, ""))) <= ncols) {
            /*
             * ...but *could* fit on one line otherwise, then we
             * should end the current line and start this element on its
             * own line.
             */
            ctx->need_prefix = true;
        }
        H5TOOLS_DEBUG("ctx->need_prefix=%d", ctx->need_prefix);
    }

    /*
     * We need to break after each row of a dimension---> we should
     * break at the end of the each last dimension well that is the
     * way the dumper did it before
     */
    if (info->arr_linebreak && ctx->cur_elmt) {
        if (ctx->size_last_dim && (ctx->cur_elmt % ctx->size_last_dim) == 0)
            ctx->need_prefix = true;

        if (elmt_counter == ctx->size_last_dim) {
            ctx->need_prefix = true;
            dimension_break  = false;
        }
        H5TOOLS_DEBUG("ctx->need_prefix=%d", ctx->need_prefix);
    }
    H5TOOLS_DEBUG("elmt_counter=%ld - ctx->size_last_dim=%ld info->line_suf=%s", elmt_counter,
                  ctx->size_last_dim, info->line_suf);

    /*
     * If the previous element occupied multiple lines and this element
     * is too long to fit on a line then start this element at the
     * beginning of the line.
     */
    if (info->line_multi_new == 1 && ctx->prev_multiline &&
        (ctx->cur_column + h5tools_count_ncols(s) + strlen(OPT(info->elmt_suf2, " ")) +
         strlen(OPT(info->line_suf, ""))) > ncols)
        ctx->need_prefix = true;
    H5TOOLS_DEBUG("ctx->need_prefix=%d", ctx->need_prefix);

    /*
     * If too many elements have already been printed then we need to
     * start a new line.
     */
    if (info->line_per_line > 0 && ctx->cur_elmt >= info->line_per_line)
        ctx->need_prefix = true;
    H5TOOLS_DEBUG("ctx->need_prefix=%d", ctx->need_prefix);

    /*
     * Each OPTIONAL_LINE_BREAK embedded in the rendered string can cause
     * the data to split across multiple lines.  We display the sections
     * one-at a time.
     */
    multiline = 0;
    for (secnum = 0, multiline = 0; (section = strtok(secnum ? NULL : s, OPTIONAL_LINE_BREAK)); secnum++) {
        /*
         * If the current section plus possible suffix and end-of-line
         * information would cause the output to wrap then we need to
         * start a new line.
         */

        /*
         * check for displaying prefix for each section
         */
        if ((ctx->cur_column + strlen(section) + strlen(OPT(info->elmt_suf2, " ")) +
             strlen(OPT(info->line_suf, ""))) > ncols)
            ctx->need_prefix = 1;

        /*
         * Print the prefix or separate the beginning of this element
         * from the previous element.
         */
        H5TOOLS_DEBUG("ctx->need_prefix=%d", ctx->need_prefix);
        if (ctx->need_prefix) {
            if (secnum)
                multiline++;

            /* pass to the prefix in h5tools_simple_prefix the total
             * position instead of the current stripmine position i;
             * this is necessary to print the array indices
             */
            *curr_pos = ctx->sm_pos + local_elmt_counter;
            H5TOOLS_DEBUG("curr_pos=%ld - ctx->sm_pos=%ld - ctx->ndims=%ld", *curr_pos, ctx->sm_pos,
                          ctx->ndims);

            h5tools_simple_prefix(stream, info, ctx, *curr_pos, secnum);
        }
        else if ((local_elmt_counter || ctx->continuation) && secnum == 0) {
            PUTSTREAM(OPT(info->elmt_suf2, " "), stream);
            ctx->cur_column += strlen(OPT(info->elmt_suf2, " "));
        }
        H5TOOLS_DEBUG("section=%s", section);

        /* Print the section */
        PUTSTREAM(section, stream);
        ctx->cur_column += strlen(section);
    }

    ctx->prev_multiline = multiline;

    H5TOOLS_ENDDEBUG(" ");

    return dimension_break;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_render_region_element
 *
 * Purpose:  Prints the string buffer to the output STREAM. The string is
 *           printed according to the format described in INFO. The CTX struct
 *           contains context information shared between calls to this function.
 *
 * Return:
 *           False if a dimension end is reached
 *           True otherwise
 *
 * In/Out:
 *           h5tools_context_t *ctx
 *           h5tools_str_t *buffer
 *           hsize_t *curr_pos
 *
 * Parameters Description:
 *           h5tools_str_t *buffer is the string into which to render
 *           hsize_t curr_pos is the total data element position
 *           size_t ncols
 *           hsize_t *ptdata
 *           hsize_t local_elmt_counter is the local element loop counter
 *           hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
bool
h5tools_render_region_element(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                              h5tools_str_t *buffer, hsize_t *curr_pos, size_t ncols, hsize_t *ptdata,
                              hsize_t local_elmt_counter, hsize_t elmt_counter)
{
    bool  dimension_break = true;
    char *s               = NULL;
    char *section         = NULL; /* a section of output */
    int   secnum;                 /* section sequence number */
    int   multiline;              /* datum was multiline */

    H5TOOLS_START_DEBUG(" ");
    H5TOOLS_DEBUG("elmt_counter=%ld - local_elmt_counter=%ld", elmt_counter, local_elmt_counter);

    s = h5tools_str_fmt(buffer, (size_t)0, "%s");

    /*
     * If the element would split on multiple lines if printed at our
     * current location...
     */
    if (info->line_multi_new == 1 &&
        (ctx->cur_column + h5tools_count_ncols(s) + strlen(OPT(info->elmt_suf2, " ")) +
         strlen(OPT(info->line_suf, ""))) > ncols) {
        if (ctx->prev_multiline) {
            /*
             * ... and the previous element also occupied more than one
             * line, then start this element at the beginning of a line.
             */
            ctx->need_prefix = true;
        }
        else if ((ctx->prev_prefix_len + h5tools_count_ncols(s) + strlen(OPT(info->elmt_suf2, " ")) +
                  strlen(OPT(info->line_suf, ""))) <= ncols) {
            /*
             * ...but *could* fit on one line otherwise, then we
             * should end the current line and start this element on its
             * own line.
             */
            ctx->need_prefix = true;
        }
    }

    /*
     * We need to break after each row of a dimension---> we should
     * break at the end of the each last dimension well that is the
     * way the dumper did it before
     */
    if (info->arr_linebreak && ctx->cur_elmt) {
        if (ctx->size_last_dim && (ctx->cur_elmt % ctx->size_last_dim) == 0)
            ctx->need_prefix = true;

        if (elmt_counter == ctx->size_last_dim) {
            ctx->need_prefix = true;
            dimension_break  = false;
        }
    }

    /*
     * If the previous element occupied multiple lines and this element
     * is too long to fit on a line then start this element at the
     * beginning of the line.
     */
    if (info->line_multi_new == 1 && ctx->prev_multiline &&
        (ctx->cur_column + h5tools_count_ncols(s) + strlen(OPT(info->elmt_suf2, " ")) +
         strlen(OPT(info->line_suf, ""))) > ncols)
        ctx->need_prefix = true;

    /*
     * If too many elements have already been printed then we need to
     * start a new line.
     */
    if (info->line_per_line > 0 && ctx->cur_elmt >= info->line_per_line)
        ctx->need_prefix = true;

    /*
     * Each OPTIONAL_LINE_BREAK embedded in the rendered string can cause
     * the data to split across multiple lines.  We display the sections
     * one-at a time.
     */
    multiline = 0;
    for (secnum = 0, multiline = 0; (section = strtok(secnum ? NULL : s, OPTIONAL_LINE_BREAK)); secnum++) {
        /*
         * If the current section plus possible suffix and end-of-line
         * information would cause the output to wrap then we need to
         * start a new line.
         */

        /*
         * Added the info->skip_first because the dumper does not want
         * this check to happen for the first line
         */
        if ((!info->skip_first || local_elmt_counter) &&
            (ctx->cur_column + strlen(section) + strlen(OPT(info->elmt_suf2, " ")) +
             strlen(OPT(info->line_suf, ""))) > ncols)
            ctx->need_prefix = 1;

        /*
         * Print the prefix or separate the beginning of this element
         * from the previous element.
         */
        if (ctx->need_prefix) {
            if (secnum)
                multiline++;

            /* pass to the prefix in h5tools_region_simple_prefix the total
             * position instead of the current stripmine position i;
             * this is necessary to print the array indices
             */
            *curr_pos = ctx->sm_pos + local_elmt_counter;
            H5TOOLS_DEBUG("curr_pos=%ld - ctx->sm_pos=%ld", *curr_pos, ctx->sm_pos);

            h5tools_region_simple_prefix(stream, info, ctx, local_elmt_counter, ptdata, secnum);
        }
        else if ((local_elmt_counter || ctx->continuation) && secnum == 0) {
            PUTSTREAM(OPT(info->elmt_suf2, " "), stream);
            ctx->cur_column += strlen(OPT(info->elmt_suf2, " "));
        }

        /* Print the section */
        PUTSTREAM(section, stream);
        ctx->cur_column += strlen(section);
    }

    ctx->prev_multiline = multiline;

    H5TOOLS_ENDDEBUG(" ");

    return dimension_break;
}

/*-------------------------------------------------------------------------
 * Function: init_acc_pos
 *
 * Purpose:  initialize accumulator and matrix position
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
void
init_acc_pos(unsigned ndims, const hsize_t *dims, hsize_t *acc, hsize_t *pos, hsize_t *p_min_idx)
{
    int      i;
    unsigned j;

    H5TOOLS_START_DEBUG(" ");

    for (i = 0; (unsigned)i < ndims; i++)
        p_min_idx[i] = 0;

    if (ndims > 0) {
        acc[ndims - 1] = 1;
        for (i = ((int)ndims - 2); i >= 0; i--) {
            acc[i] = acc[i + 1] * dims[i + 1];
            H5TOOLS_DEBUG("acc[%d]=%ld", i, acc[i]);
        }
        for (j = 0; j < ndims; j++)
            pos[j] = 0;
    }

    H5TOOLS_ENDDEBUG(" ");
}

/*-------------------------------------------------------------------------
 * Function: calc_acc_pos
 *
 * Purpose:  Calculate the number of elements represented by a unit change
 *           in a certain index position.
 *
 * Return:   void
 *-------------------------------------------------------------------------
 */
hsize_t
calc_acc_pos(unsigned ndims, hsize_t elmtno, const hsize_t *acc, hsize_t *pos)
{
    int     i;
    hsize_t curr_pos = elmtno;

    H5TOOLS_START_DEBUG(" ");

    if (ndims > 0) {
        for (i = 0; i < (int)ndims; i++) {
            if (curr_pos > 0) {
                H5TOOLS_DEBUG("curr_pos=%ld - ctx->acc[%d]=%ld", curr_pos, i, acc[i]);
                pos[i] = curr_pos / acc[i];
                curr_pos -= acc[i] * pos[i];
            }
            else
                pos[i] = 0;
            H5TOOLS_DEBUG("curr_pos=%ld - pos[%d]=%ld - acc[%d]=%ld", curr_pos, i, pos[i], i, acc[i]);
        }
    }

    H5TOOLS_ENDDEBUG(" ");

    return curr_pos;
}

/*-------------------------------------------------------------------------
 * Function: render_bin_output
 *
 * Purpose:  Write one element of memory buffer to a binary file stream
 *
 * Return:   Success:    SUCCEED
 *           Failure:    FAIL
 *-------------------------------------------------------------------------
 */
int
render_bin_output(FILE *stream, hid_t container, hid_t tid, void *_mem, hsize_t block_nelmts)
{
    unsigned char *mem = (unsigned char *)_mem;
    size_t         size; /* datum size */
    hsize_t        block_index;
    H5T_class_t    type_class;
    bool           past_catch = false;
    int            ret_value  = 0;

    H5TOOLS_START_DEBUG(" ");
    if ((size = H5Tget_size(tid)) == 0)
        H5TOOLS_THROW((-1), "H5Tget_size failed");

    if ((type_class = H5Tget_class(tid)) < 0)
        H5TOOLS_THROW((-1), "H5Tget_class failed");

    switch (type_class) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_ENUM:
        case H5T_BITFIELD:
            H5TOOLS_DEBUG("numbers");
            block_index = block_nelmts * size;
            while (block_index > 0) {
                size_t bytes_in    = 0; /* # of bytes to write  */
                size_t bytes_wrote = 0; /* # of bytes written   */

                if (block_index > sizeof(size_t))
                    bytes_in = sizeof(size_t);
                else
                    bytes_in = (size_t)block_index;

                bytes_wrote = fwrite(mem, 1, bytes_in, stream);

                if (bytes_wrote != bytes_in || (0 == bytes_wrote && ferror(stream)))
                    H5TOOLS_THROW((-1), "fwrite failed");

                block_index -= (hsize_t)bytes_wrote;
                mem = mem + bytes_wrote;
            }
            break;
        case H5T_STRING: {
            unsigned int  i;
            H5T_str_t     pad;
            char         *s = NULL;
            unsigned char tempuchar;

            H5TOOLS_DEBUG("H5T_STRING");
            pad = H5Tget_strpad(tid);

            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char *)_mem) + block_index * size;

                if (H5Tis_variable_str(tid)) {
                    s = *(char **)((void *)mem);
                    if (s != NULL)
                        size = strlen(s);
                    else
                        H5TOOLS_THROW((-1), "NULL string");
                }
                else {
                    s = (char *)mem;
                }
                for (i = 0; i < size && (s[i] || pad != H5T_STR_NULLTERM); i++) {
                    memcpy(&tempuchar, &s[i], sizeof(unsigned char));
                    if (1 != fwrite(&tempuchar, sizeof(unsigned char), 1, stream))
                        H5TOOLS_THROW((-1), "fwrite failed");
                } /* i */
            }     /* for (block_index = 0; block_index < block_nelmts; block_index++) */
        } break;
        case H5T_COMPOUND: {
            int      snmembs;
            unsigned nmembs;

            H5TOOLS_DEBUG("H5T_COMPOUND");
            if ((snmembs = H5Tget_nmembers(tid)) < 0)
                H5TOOLS_THROW((-1), "H5Tget_nmembers of compound failed");
            nmembs = (unsigned)snmembs;

            for (block_index = 0; block_index < block_nelmts; block_index++) {
                unsigned j;

                mem = ((unsigned char *)_mem) + block_index * size;
                for (j = 0; j < nmembs; j++) {
                    hid_t  memb = H5I_INVALID_HID;
                    size_t offset;

                    offset = H5Tget_member_offset(tid, j);
                    memb   = H5Tget_member_type(tid, j);

                    if (render_bin_output(stream, container, memb, mem + offset, 1) < 0) {
                        H5Tclose(memb);
                        H5TOOLS_THROW((-1), "render_bin_output of compound member failed");
                    }

                    H5Tclose(memb);
                }
            }
        } break;
        case H5T_ARRAY: {
            int     k, ndims;
            hsize_t dims[H5S_MAX_RANK], temp_nelmts, nelmts = 0;
            hid_t   memb = H5I_INVALID_HID;

            H5TOOLS_DEBUG("H5T_ARRAY");
            /* get the array's base datatype for each element */
            memb  = H5Tget_super(tid);
            ndims = H5Tget_array_ndims(tid);
            H5Tget_array_dims2(tid, dims);
            if (ndims >= 1 && ndims <= H5S_MAX_RANK) {
                /* calculate the number of array elements */
                for (k = 0, nelmts = 1; k < ndims; k++) {
                    temp_nelmts = nelmts;
                    temp_nelmts *= dims[k];
                    nelmts = (size_t)temp_nelmts;
                }
            }
            else {
                H5Tclose(memb);
                H5TOOLS_THROW((-1), "calculate the number of array elements failed");
            }

            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char *)_mem) + block_index * size;
                /* dump the array element */
                if (render_bin_output(stream, container, memb, mem, nelmts) < 0) {
                    H5Tclose(memb);
                    H5TOOLS_THROW((-1), "render_bin_output failed");
                }
            }
            H5Tclose(memb);
        } break;
        case H5T_VLEN: {
            hsize_t nelmts;
            hid_t   memb = H5I_INVALID_HID;

            H5TOOLS_DEBUG("H5T_VLEN");
            /* get the VL sequences's base datatype for each element */
            memb = H5Tget_super(tid);

            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char *)_mem) + block_index * size;
                /* Get the number of sequence elements */
                nelmts = ((hvl_t *)((void *)mem))->len;

                /* dump the array element */
                if (render_bin_output(stream, container, memb, ((char *)(((hvl_t *)((void *)mem))->p)),
                                      nelmts) < 0) {
                    H5Tclose(memb);
                    H5TOOLS_THROW((-1), "render_bin_output failed");
                }
            }
            H5Tclose(memb);
        } break;
        case H5T_REFERENCE: {
            H5TOOLS_DEBUG("reference class type");
            if (H5Tequal(tid, H5T_STD_REF)) {
                H5TOOLS_DEBUG("H5T_STD_REF");
                if (region_output) {
                    /* region data */
                    hid_t        region_id    = H5I_INVALID_HID;
                    hid_t        region_space = H5I_INVALID_HID;
                    H5S_sel_type region_type;
                    H5R_ref_t    tref;

                    if (size > sizeof(tref))
                        H5TOOLS_THROW((-1), "unexpectedly large ref");

                    memset(&tref, 0, sizeof(tref));

                    for (block_index = 0; block_index < block_nelmts; block_index++) {
                        mem = ((unsigned char *)_mem) + block_index * size;
                        memcpy(&tref, mem, size);
                        if ((region_id = H5Ropen_object(&tref, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                            H5TOOLS_INFO("H5Ropen_object H5T_STD_REF failed");
                        else {
                            if ((region_space = H5Ropen_region(&tref, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                if (!h5tools_is_zero(&tref, H5Tget_size(H5T_STD_REF))) {

                                    region_type = H5Sget_select_type(region_space);
                                    if (region_type == H5S_SEL_POINTS)
                                        render_bin_output_region_points(region_space, region_id, stream,
                                                                        container);
                                    else
                                        render_bin_output_region_blocks(region_space, region_id, stream,
                                                                        container);
                                }
                                else {
                                    H5TOOLS_INFO("H5Ropen_object H5T_STD_REF NULL");
                                }
                                H5Sclose(region_space);
                            } /* end if (region_space >= 0) */
                            H5Dclose(region_id);
                        }
                    }
                } /* end if (region_output... */
            }
            else if (H5Tequal(tid, H5T_STD_REF_DSETREG)) {
                /* if (size == H5R_DSET_REG_REF_BUF_SIZE) */
                H5TOOLS_DEBUG("H5T_STD_REF_DSETREG");
            }
            else if (H5Tequal(tid, H5T_STD_REF_OBJ)) {
                /* if (size == H5R_OBJ_REF_BUF_SIZE) */
                H5TOOLS_DEBUG("H5T_STD_REF_OBJ");
            }
        } break;

        case H5T_TIME:
        case H5T_OPAQUE:
            H5TOOLS_DEBUG("H5T_OPAQUE");
            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char *)_mem) + block_index * size;
                if (size != fwrite(mem, sizeof(char), size, stream))
                    H5TOOLS_THROW((-1), "fwrite failed");
            } /* end for */
            break;

        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            /* Badness */
            H5TOOLS_THROW((-1), "bad type class");
            break;
    } /* end switch */

    CATCH
    H5TOOLS_ENDDEBUG(" ");
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: render_bin_output_region_data_blocks
 *
 * Purpose:  Print the data values from a dataset referenced by region blocks.
 *           This is a special case subfunction to print the data in a region reference of type blocks.
 *
 * Return:   FAIL if there was an error
 *           SUCCEED otherwise
 *
 *-------------------------------------------------------------------------
 */
int
render_bin_output_region_data_blocks(hid_t region_id, FILE *stream, hid_t container, unsigned ndims,
                                     hid_t type_id, hsize_t nblocks, const hsize_t *ptdata)
{
    hsize_t *dims1 = NULL;
    hsize_t *start = NULL;
    hsize_t *count = NULL;
    hsize_t  numelem;
    hsize_t  total_size[H5S_MAX_RANK];
    unsigned jndx;
    size_t   type_size;
    hid_t    mem_space  = H5I_INVALID_HID;
    void    *region_buf = NULL;
    bool     past_catch = false;
    hsize_t  blkndx;
    hid_t    sid1      = H5I_INVALID_HID;
    int      ret_value = -1;

    H5TOOLS_START_DEBUG(" ");
    /* Get the dataspace of the dataset */
    if ((sid1 = H5Dget_space(region_id)) < 0)
        H5TOOLS_THROW((-1), "H5Dget_space failed");

    /* Allocate space for the dimension array */
    if ((dims1 = (hsize_t *)malloc(sizeof(hsize_t) * ndims)) == NULL)
        H5TOOLS_THROW((-1), "Could not allocate buffer for dims");

    /* find the dimensions of each data space from the block coordinates */
    numelem = 1;
    for (jndx = 0; jndx < ndims; jndx++) {
        dims1[jndx] = ptdata[jndx + ndims] - ptdata[jndx] + 1;
        numelem     = dims1[jndx] * numelem;
    }

    /* Create dataspace for reading buffer */
    if ((mem_space = H5Screate_simple((int)ndims, dims1, NULL)) < 0)
        H5TOOLS_THROW((-1), "H5Screate_simple failed");

    if ((type_size = H5Tget_size(type_id)) == 0)
        H5TOOLS_THROW((-1), "H5Tget_size failed");

    if ((region_buf = malloc(type_size * (size_t)numelem)) == NULL)
        H5TOOLS_THROW((-1), "Could not allocate region buffer");

    /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
    /*          1   2        n      1   2        n                                       */
    if ((start = (hsize_t *)malloc(sizeof(hsize_t) * ndims)) == NULL)
        H5TOOLS_THROW((-1), "Could not allocate buffer for start");

    if ((count = (hsize_t *)malloc(sizeof(hsize_t) * ndims)) == NULL)
        H5TOOLS_THROW((-1), "Could not allocate buffer for count");

    for (blkndx = 0; blkndx < nblocks; blkndx++) {
        for (jndx = 0; jndx < ndims; jndx++) {
            start[jndx] = ptdata[jndx + blkndx * ndims * 2];
            count[jndx] = dims1[jndx];
        }

        if (H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Sselect_hyperslab failed");

        if (H5Dread(region_id, type_id, mem_space, sid1, H5P_DEFAULT, region_buf) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Dread failed");

        if (H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_dims failed");

        if (render_bin_output(stream, container, type_id, (char *)region_buf, numelem) < 0)
            H5TOOLS_GOTO_ERROR((-1), "render_bin_output of data region failed");
        /* Render the region data element end */
done:;
    } /* end for (blkndx = 0; blkndx < nblocks; blkndx++) */

    CATCH
    free(start);
    free(count);
    free(region_buf);
    free(dims1);

    if (H5Sclose(mem_space) < 0)
        H5TOOLS_ERROR((-1), "H5Sclose failed");
    if (H5Sclose(sid1) < 0)
        H5TOOLS_ERROR((-1), "H5Sclose failed");

    H5TOOLS_ENDDEBUG(" ");
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: render_bin_output_region_blocks
 *
 * Purpose:  Print some values from a dataset referenced by region blocks.
 *           This is a special case subfunction to dump a region reference using blocks.
 *
 * Return:   False if ERROR
 *           True otherwise
 *-------------------------------------------------------------------------
 */
bool
render_bin_output_region_blocks(hid_t region_space, hid_t region_id, FILE *stream, hid_t container)
{
    hssize_t snblocks;
    hsize_t  nblocks;
    hsize_t  alloc_size;
    hsize_t *ptdata = NULL;
    int      sndims;
    unsigned ndims;
    hid_t    dtype      = H5I_INVALID_HID;
    hid_t    type_id    = H5I_INVALID_HID;
    bool     past_catch = false;
    bool     ret_value  = true;

    H5TOOLS_START_DEBUG(" ");
    if ((snblocks = H5Sget_select_hyper_nblocks(region_space)) <= 0)
        H5TOOLS_THROW(false, "H5Sget_select_hyper_nblocks failed");
    nblocks = (hsize_t)snblocks;

    /* Print block information */
    if ((sndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5TOOLS_THROW(false, "H5Sget_simple_extent_ndims failed");
    ndims = (unsigned)sndims;

    alloc_size = nblocks * ndims * 2 * sizeof(ptdata[0]);
    if ((ptdata = (hsize_t *)malloc((size_t)alloc_size)) == NULL)
        H5TOOLS_GOTO_ERROR(false, "Could not allocate buffer for ptdata");

    if (H5Sget_select_hyper_blocklist(region_space, (hsize_t)0, nblocks, ptdata) < 0)
        H5TOOLS_GOTO_ERROR(false, "H5Rget_select_hyper_blocklist failed");

    if ((dtype = H5Dget_type(region_id)) < 0)
        H5TOOLS_GOTO_ERROR(false, "H5Dget_type failed");
    if ((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        H5TOOLS_GOTO_ERROR(false, "H5Tget_native_type failed");

    render_bin_output_region_data_blocks(region_id, stream, container, ndims, type_id, nblocks, ptdata);

done:
    free(ptdata);

    if (type_id > 0 && H5Tclose(type_id) < 0)
        H5TOOLS_ERROR(false, "H5Tclose failed");

    if (dtype > 0 && H5Tclose(dtype) < 0)
        H5TOOLS_ERROR(false, "H5Tclose failed");

    H5_LEAVE(true);

    CATCH
    H5TOOLS_ENDDEBUG(" ");
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type points.
 *
 * Return:
 *      The function returns FAIL on error, otherwise SUCCEED
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      size_t ncols
 *      int ndims is the number of dimensions of the region element
 *      hssize_t npoints is the number of points in the region
 *-------------------------------------------------------------------------
 */
int
render_bin_output_region_data_points(hid_t region_space, hid_t region_id, FILE *stream, hid_t container,
                                     unsigned ndims, hid_t type_id, hsize_t npoints)
{
    hsize_t *dims1 = NULL;
    size_t   type_size;
    hid_t    mem_space  = H5I_INVALID_HID;
    void    *region_buf = NULL;
    int      ret_value  = 0;

    H5TOOLS_START_DEBUG(" ");
    if ((type_size = H5Tget_size(type_id)) == 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Tget_size failed");

    if ((region_buf = malloc(type_size * (size_t)npoints)) == NULL)
        H5TOOLS_GOTO_ERROR((-1), "Could not allocate buffer for region");

    /* Allocate space for the dimension array */
    if ((dims1 = (hsize_t *)malloc(sizeof(hsize_t) * ndims)) == NULL)
        H5TOOLS_GOTO_ERROR((-1), "Could not allocate buffer for dims");

    dims1[0] = npoints;
    if ((mem_space = H5Screate_simple(1, dims1, NULL)) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Screate_simple failed");

    if (H5Dread(region_id, type_id, mem_space, region_space, H5P_DEFAULT, region_buf) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Dread failed");
    if (H5Sget_simple_extent_dims(region_space, dims1, NULL) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_dims failed");

    if (render_bin_output(stream, container, type_id, (char *)region_buf, npoints) < 0)
        H5TOOLS_GOTO_ERROR((-1), "render_bin_output of data points failed");

done:
    free(region_buf);
    free(dims1);

    if (H5Sclose(mem_space) < 0)
        H5TOOLS_ERROR((-1), "H5Sclose failed");

    H5TOOLS_ENDDEBUG(" ");
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: render_bin_output_region_points
 *
 * Purpose:  Print some values from a dataset referenced by region points.
 *           This is a special case function to dump a region reference using points.
 *
 * Return:   False if the last dimension has been reached
 *           True otherwise
 *-------------------------------------------------------------------------
 */
bool
render_bin_output_region_points(hid_t region_space, hid_t region_id, FILE *stream, hid_t container)
{
    hssize_t snpoints;
    hsize_t  npoints;
    int      sndims;
    unsigned ndims;
    hid_t    dtype      = H5I_INVALID_HID;
    hid_t    type_id    = H5I_INVALID_HID;
    bool     past_catch = false;
    bool     ret_value  = true;

    H5TOOLS_START_DEBUG(" ");
    if ((snpoints = H5Sget_select_elem_npoints(region_space)) <= 0)
        H5TOOLS_THROW(false, "H5Sget_select_elem_npoints failed");
    npoints = (hsize_t)snpoints;

    /* Allocate space for the dimension array */
    if ((sndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5TOOLS_THROW(false, "H5Sget_simple_extent_ndims failed");
    ndims = (unsigned)sndims;

    if ((dtype = H5Dget_type(region_id)) < 0)
        H5TOOLS_GOTO_ERROR(false, "H5Dget_type failed");

    if ((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        H5TOOLS_GOTO_ERROR(false, "H5Tget_native_type failed");

    render_bin_output_region_data_points(region_space, region_id, stream, container, ndims, type_id, npoints);

done:
    if (type_id > 0 && H5Tclose(type_id) < 0)
        H5TOOLS_ERROR(false, "H5Tclose failed");

    if (dtype > 0 && H5Tclose(dtype) < 0)
        H5TOOLS_ERROR(false, "H5Tclose failed");

    H5_LEAVE(ret_value);
    CATCH
    H5TOOLS_ENDDEBUG(" ");
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_is_zero
 *
 * Purpose:  Determines if memory is initialized to all zero bytes.
 *
 * Return:   true if all bytes are zero
 *           false otherwise
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE bool
h5tools_is_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char *)_mem;

    while (size-- > 0)
        if (mem[size])
            return false;

    return true;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_is_obj_same
 *
 * Purpose:  Check if two given object IDs or link names point to the same object.
 *
 * Parameters:
 *           hid_t loc_id1: location of the first object
 *           char *name1:   link name of the first object.
 *                          Use "." or NULL if loc_id1 is the object to be compared.
 *           hid_t loc_id2: location of the second object
 *           char *name1:   link name of the first object.
 *                          Use "." or NULL if loc_id2 is the object to be compared.
 *
 * Return:   true if it is the same object
 *           false otherwise.
 *-------------------------------------------------------------------------
 */
bool
h5tools_is_obj_same(hid_t loc_id1, const char *name1, hid_t loc_id2, const char *name2)
{
    H5O_info2_t oinfo1, oinfo2;
    bool        ret_val = false;

    if (name1 && strcmp(name1, ".") != 0)
        H5Oget_info_by_name3(loc_id1, name1, &oinfo1, H5O_INFO_BASIC, H5P_DEFAULT);
    else
        H5Oget_info3(loc_id1, &oinfo1, H5O_INFO_BASIC);

    if (name2 && strcmp(name2, ".") != 0)
        H5Oget_info_by_name3(loc_id2, name2, &oinfo2, H5O_INFO_BASIC, H5P_DEFAULT);
    else
        H5Oget_info3(loc_id2, &oinfo2, H5O_INFO_BASIC);

    if (oinfo1.fileno == oinfo2.fileno) {
        int token_cmp_val;

        H5Otoken_cmp(loc_id1, &oinfo1.token, &oinfo2.token, &token_cmp_val);

        if (!token_cmp_val)
            ret_val = true;
    }

    return ret_val;
}
