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

#include "h5tools.h"
#include "h5tools_dump.h"
#include "H5LDprivate.h"

/*
 * Note: This tool used private routine
 */
#define PROGRAMNAME   "h5watch" /* Name of tool */
#define FIELD_SEP     ","       /* nested field separator */
#define DEFAULT_RETRY 50        /* number of times to try opening the file */

/*
 * Note:(see comments in hl/src/H5LDprivate.h)
 *  This tool uses private routines H5LD_construct_vector()and H5LD_clean_vector()
 *    This tool uses H5LD_memb_t data structure declared in H5LDprivate.h
 */

static const char   *progname         = "h5watch"; /* tool name */
static char         *g_list_of_fields = NULL;      /* command line input for "list_of_fields" */
static char         *g_dup_fields     = NULL;      /* copy of "list_of_fields" */
static H5LD_memb_t **g_listv          = NULL;      /* vector info for "list_of_fields" */

static bool     g_monitor_size_only = false; /* monitor changes in dataset dimension sizes */
static unsigned g_polling_interval  = 1;     /* polling interval to check appended data */

static bool     g_label         = false;  /* label compound values */
static int      g_display_width = 80;     /* output width in characters */
static bool     g_simple_output = false;  /* make output more machine-readable */
static unsigned g_retry = DEFAULT_RETRY;  /* # of times to try opening the file if somehow file is unstable */
static bool     g_display_hex    = false; /* display data in hexadecimal format : LATER */
static bool     g_user_interrupt = false; /* Flag to indicate that user interrupted execution */

static herr_t doprint(hid_t did, const hsize_t *start, const hsize_t *block, int rank);
static herr_t slicendump(hid_t did, hsize_t *prev_dims, hsize_t *cur_dims, hsize_t *start, hsize_t *block,
                         int rank, int subrank);
static herr_t monitor_dataset(hid_t fid, char *dsetname);
static herr_t process_cmpd_fields(hid_t fid, char *dsetname);
static herr_t check_dataset(hid_t fid, char *dsetname);
static void   leave(int ret);
static void   usage(const char *prog);
static void   parse_command_line(int argc, const char *const *argv);

/*
 * Command-line options: The user can only specify long-named parameters.
 * The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
static const char            *s_opts   = "?";
static struct h5_long_options l_opts[] = {{"help", no_arg, 'h'},         {"hel", no_arg, 'h'},
                                          {"dim", no_arg, 'd'},          {"di", no_arg, 'd'},
                                          {"label", no_arg, 'l'},        {"labe", no_arg, 'l'},
                                          {"lab", no_arg, 'l'},          {"la", no_arg, 'l'},
                                          {"simple", no_arg, 'S'},       {"simpl", no_arg, 'S'},
                                          {"simp", no_arg, 'S'},         {"sim", no_arg, 'S'},
                                          {"si", no_arg, 'S'},           {"hexdump", no_arg, 'x'},
                                          {"hexdum", no_arg, 'x'},       {"hexdu", no_arg, 'x'},
                                          {"hexd", no_arg, 'x'},         {"hex", no_arg, 'x'},
                                          {"width", require_arg, 'w'},   {"widt", require_arg, 'w'},
                                          {"wid", require_arg, 'w'},     {"wi", require_arg, 'w'},
                                          {"polling", require_arg, 'p'}, {"pollin", require_arg, 'p'},
                                          {"polli", require_arg, 'p'},   {"poll", require_arg, 'p'},
                                          {"pol", require_arg, 'p'},     {"po", require_arg, 'p'},
                                          {"fields", require_arg, 'f'},  {"field", require_arg, 'f'},
                                          {"fiel", require_arg, 'f'},    {"fie", require_arg, 'f'},
                                          {"fi", require_arg, 'f'},      {"version", no_arg, 'V'},
                                          {"versio", no_arg, 'V'},       {"versi", no_arg, 'V'},
                                          {"vers", no_arg, 'V'},         {"ver", no_arg, 'V'},
                                          {"ve", no_arg, 'V'},           {NULL, 0, '\0'}};

/*-------------------------------------------------------------------------
 * Function: doprint()
 *
 * Purpose: Prepare to print the dataset's appended data.
 *          Call the tools library routine h5tools_dump_dset() to do the printing.
 *          (This routine is mostly copied from dump_dataset_values() in tools/h5ls/h5ls.c
 *          and modified accordingly).
 *
 * Return: 0 on success; negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
doprint(hid_t did, const hsize_t *start, const hsize_t *block, int rank)
{
    h5tools_context_t ctx;                           /* print context  */
    h5tool_format_t   info;                          /* Format info for the tools library */
    static char       fmt_double[16], fmt_float[16]; /* Format info */
    struct subset_t   subset;                        /* Subsetting info */
    hsize_t           ss_start[H5S_MAX_RANK];        /* Info for hyperslab */
    hsize_t           ss_stride[H5S_MAX_RANK];       /* Info for hyperslab */
    hsize_t           ss_block[H5S_MAX_RANK];        /* Info for hyperslab */
    hsize_t           ss_count[H5S_MAX_RANK];        /* Info for hyperslab */
    int               i;                             /* Local index variable */
    herr_t            ret_value = SUCCEED;           /* Return value */

    /* Subsetting information for the tools library printing routines */
    subset.start.data  = ss_start;
    subset.stride.data = ss_stride;
    subset.block.data  = ss_block;
    subset.count.data  = ss_count;

    /* Initialize subsetting information */
    for (i = 0; i < rank; i++) {
        subset.stride.data[i] = 1;
        subset.count.data[i]  = 1;
        subset.start.data[i]  = start[i];
        subset.block.data[i]  = block[i];
    } /* end for */

    memset(&ctx, 0, sizeof(ctx));
    ctx.sset = &subset;

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);

    if (g_simple_output) {
        info.idx_fmt        = "";
        info.line_ncols     = 65535; /*something big*/
        info.line_per_line  = 1;
        info.line_multi_new = 0;
        info.line_pre       = "        ";
        info.line_cont      = "         ";

        info.arr_pre = "";
        info.arr_suf = "";
        info.arr_sep = " ";

        info.cmpd_pre = "";
        info.cmpd_suf = "";
        info.cmpd_sep = " ";

        /* The "fields" selected by the user */
        info.cmpd_listv = (const struct H5LD_memb_t *const *)g_listv;

        if (g_label)
            info.cmpd_name = "%s=";

        info.elmt_suf1  = " ";
        info.str_locale = ESCAPE_HTML;
    }
    else {
        info.idx_fmt = "(%s)";
        if (!g_display_width) {
            info.line_ncols    = 65535;
            info.line_per_line = 1;
        } /* end if */
        else
            info.line_ncols = (unsigned)g_display_width;

        info.line_multi_new = 1;

        /* The "fields" selected by the user */
        info.cmpd_listv = (const struct H5LD_memb_t *const *)g_listv;
        if (g_label)
            info.cmpd_name = "%s=";
        info.line_pre   = "        %s ";
        info.line_cont  = "        %s  ";
        info.str_repeat = 8;
    } /* end else */

    /* Floating point types should display full precision */
    snprintf(fmt_float, sizeof(fmt_float), "%%1.%dg", FLT_DIG);
    info.fmt_float = fmt_float;
    snprintf(fmt_double, sizeof(fmt_double), "%%1.%dg", DBL_DIG);
    info.fmt_double = fmt_double;

    info.dset_format     = "DSET-%s ";
    info.dset_hidefileno = 0;

    info.obj_format     = "-%lu:%" PRIuHADDR;
    info.obj_hidefileno = 0;

    info.dset_blockformat_pre = "%sBlk%lu: ";
    info.dset_ptformat_pre    = "%sPt%lu: ";

    info.line_indent = "";

    if (g_display_hex) {
        /* Print all data in hexadecimal format if the `-x' or `--hexdump'
         * command line switch was given. */
        info.raw = true;
    } /* end if */

    /* Print the values. */
    if ((ret_value = h5tools_dump_dset(stdout, &info, &ctx, did)) < 0)
        error_msg("unable to print data\n");

    fprintf(stdout, "\n");

    return ret_value;

} /* end doprint() */

/*-------------------------------------------------------------------------
 * Function: slicendump
 *
 * Purpose:  To dump the slice for each dimension
 *           For example: prev_dims[2] = {5, 4}; cur_dims[2] = {7, 8}
 *              This routine will dump data as follows:
 *                  {0, 3} to {0, 7} (1x4 elements)
 *                  {1, 3} to {0, 7} (1x4 elements)
 *                  {2, 3} to {0, 7} (1x4 elements)
 *                  {3, 3} to {0, 7} (1x4 elements)
 *                  {4, 3} to {0, 7} (1x4 elements)
 *                  {5, 0} to {6, 7} (2x8 elements)
 *
 * Return:      Non-negative on success
 *              Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
slicendump(hid_t did, hsize_t *prev_dims, hsize_t *cur_dims, hsize_t *start, hsize_t *block, int rank,
           int subrank)
{
    int    i;                   /* Local index variable */
    int    ind;                 /* Index for the current rank */
    herr_t ret_value = SUCCEED; /* Return value */

    ind = rank - subrank;

    if ((subrank - 1) > 0) {
        /* continue onto the next dimension */
        for (i = 0; i < (hssize_t)MIN(prev_dims[ind], cur_dims[ind]); i++) {
            start[ind] = (hsize_t)i;
            if ((ret_value = slicendump(did, prev_dims, cur_dims, start, block, rank, subrank - 1)) < 0)
                goto done;
        } /* end for */
    }     /* end if */

    /* this dimension remains the same or shrinking */
    if (cur_dims[ind] <= prev_dims[ind])
        goto done;

    /* select first the slice for the faster changing dimension */
    /* select later the whole slice for the slower changing dimension */
    start[ind] = prev_dims[ind];
    block[ind] = cur_dims[ind] - prev_dims[ind];

    for (i = ind + 1; i < rank; i++) {
        start[i] = 0;
        block[i] = cur_dims[i];
    } /* end for */

    /* Print the appended data */
    ret_value = doprint(did, start, block, rank);

done:
    return ret_value;
} /* end slicendump() */

/*-------------------------------------------------------------------------
 * Function:    monitor_dataset
 *
 * Purpose:     To poll a dataset periodically for changes in dimension sizes.
 *              For dataset with unchanged and/or decreased dimension sizes:
 *                  it just prints the dimension size changes
 *              For dataset with increase in at least one of its dimension sizes:
 *                  it will print the new appended data to the dataset
 *
 * Return:      Non-negative on success: dataset can be monitored
 *              Negative on failure: dataset cannot be monitored
 *
 *-------------------------------------------------------------------------
 */
static herr_t
monitor_dataset(hid_t fid, char *dsetname)
{
    hid_t   did;                     /* dataset id */
    hid_t   sid;                     /* dataspace id */
    int     ndims;                   /* # of dimensions in the dataspace */
    int     i, u;                    /* local index variable */
    hsize_t prev_dims[H5S_MAX_RANK]; /* current dataspace dimensions */
    hsize_t cur_dims[H5S_MAX_RANK];  /* previous dataspace dimensions */
    herr_t  ret_value = SUCCEED;     /* return value */

    fprintf(stdout, "Monitoring dataset %s...\n", dsetname);

    /* Open the dataset for minitoring */
    if ((did = H5Dopen2(fid, dsetname, H5P_DEFAULT)) < 0) {
        error_msg("error in opening dataset \"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }
    if ((sid = H5Dget_space(did)) < 0) {
        error_msg("error in getting dataspace id for dataset \"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }

    /* Get the dataset's dimension sizes */
    if ((ndims = H5Sget_simple_extent_dims(sid, prev_dims, NULL)) < 0) {
        error_msg("unable to get dimensions sizes for \"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }
    fflush(stdout);

    /* Loop until an error occurs or the user interrupts execution */
    while (!g_user_interrupt) {

        /* Refreshes the dataset */
        if (H5Drefresh(did) < 0) {
            ret_value = FAIL;
            goto done;
        }

        /* Get the dataset's current dimension sizes */
        if (H5LDget_dset_dims(did, cur_dims) < 0) {
            error_msg("unable to get dimension sizes for \"%s\"\n", dsetname);
            ret_value = FAIL;
            goto done;
        }

        /* Check the dimension sizes */
        for (i = 0; i < ndims; i++)
            if (cur_dims[i] != prev_dims[i])
                break;

        /* at least one dimension has changed */
        if (i != ndims) {
            /* Printing changes in dimension sizes */
            for (u = 0; u < ndims; u++) {
                fprintf(stdout, "dimension %d: %" PRIuHSIZE "->%" PRIuHSIZE "", u, prev_dims[u], cur_dims[u]);
                if (cur_dims[u] > prev_dims[u])
                    fprintf(stdout, " (increases)\n");
                else if (cur_dims[u] < prev_dims[u])
                    fprintf(stdout, " (decreases)\n");
                else
                    fprintf(stdout, " (unchanged)\n");
            }

            /* Printing elements appended to the dataset if there is */
            if (!g_monitor_size_only) {

                /* See if at least one dimension size has increased */
                for (u = 0; u < ndims; u++) {
                    int     j;
                    hsize_t start[H5S_MAX_RANK];
                    hsize_t block[H5S_MAX_RANK];

                    /* Print the new appended data to the dataset */
                    if (cur_dims[u] > prev_dims[u]) {
                        fprintf(stdout, "    Data:\n");

                        for (j = 0; j < ndims; j++) {
                            start[j] = 0;
                            block[j] = 1;
                        }

                        if ((ret_value = slicendump(did, prev_dims, cur_dims, start, block, ndims, ndims)) <
                            0)
                            goto done;
                        break;
                    }
                } /* end for */
            }
            fflush(stdout);
        }

        /* Save the current dimension sizes */
        memcpy(prev_dims, cur_dims, (size_t)ndims * sizeof(hsize_t));

        /* Sleep before next monitor */
        HDsleep(g_polling_interval);
    } /* end while */

    fflush(stdout);

done:
    /* Closing */
    H5E_BEGIN_TRY
    H5Dclose(did);
    H5E_END_TRY

    return (ret_value);
} /* monitor_dataset() */

/*-------------------------------------------------------------------------
 * Function:  process_cmpd_fields
 *
 * Purpose: To check whether the fields selected in "g_list_of_fields"
 *          are valid fields associated with the dataset.
 *
 * Return: 0 on success; negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
process_cmpd_fields(hid_t fid, char *dsetname)
{
    hid_t  did  = -1;           /* dataset id */
    hid_t  dtid = -1, tid = -1; /* dataset's data type id */
    size_t len;                 /* number of comma-separated fields in "g_list_of_fields" */
    herr_t ret_value = SUCCEED; /* Return value */

    assert(g_list_of_fields && *g_list_of_fields);

    /* Open the dataset */
    if ((did = H5Dopen2(fid, dsetname, H5P_DEFAULT)) < 0) {
        error_msg("error in opening dataset \"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }

    /* Get the dataset's datatype  */
    if (((dtid = H5Dget_type(did)) < 0) || (tid = H5Tget_native_type(dtid, H5T_DIR_DEFAULT)) < 0) {
        error_msg("error in getting dataset's datatype\n");
        ret_value = FAIL;
        goto done;
    }

    /* Check to make sure that the dataset's datatype is compound type */
    if (H5Tget_class(dtid) != H5T_COMPOUND) {
        error_msg("dataset should be compound type for <list_of_fields>\n");
        ret_value = FAIL;
        goto done;
    }

    /* Make a copy of "g_list_of_fields" */
    if ((g_dup_fields = strdup(g_list_of_fields)) == NULL) {
        error_msg("error in duplicating g_list_of_fields\n");
        ret_value = FAIL;
        goto done;
    }

    /* Estimate the number of comma-separated fields in "g_list of_fields" */
    len = strlen(g_list_of_fields) / 2 + 2;

    /* Allocate memory for a list vector of H5LD_memb_t structures to store "g_list_of_fields" info */
    if ((g_listv = (H5LD_memb_t **)calloc(len, sizeof(H5LD_memb_t *))) == NULL) {
        error_msg("error in allocating memory for H5LD_memb_t\n");
        ret_value = FAIL;
        goto done;
    }

    /* Process and store info for "g_listv" */
    if (H5LD_construct_vector(g_dup_fields, g_listv, tid) < 0) {
        error_msg("error in processing <list_of_fields>\n");
        ret_value = FAIL;
        goto done;
    }

    /* Will free memory for g_listv and g_dup_fields when exiting from h5watch */
done:
    /* Closing */
    H5E_BEGIN_TRY
    H5Tclose(dtid);
    H5Tclose(tid);
    H5Dclose(did);
    H5E_END_TRY
    return (ret_value);
} /* process_cmpd_fields() */

/*-------------------------------------------------------------------------
 * Function:    check_dataset
 *
 * Purpose:     To check whether a dataset can be monitored:
 *              A chunked dataset with unlimited or max. dimension setting
 *
 * Return:      Non-negative on success: dataset can be monitored
 *              Negative on failure: dataset cannot be monitored
 *
 *-------------------------------------------------------------------------
 */
static herr_t
check_dataset(hid_t fid, char *dsetname)
{
    hid_t        did = -1;               /* Dataset id */
    hid_t        dcp = -1;               /* Dataset creation property */
    hid_t        sid = -1;               /* Dataset's dataspace id */
    int          ndims;                  /* # of dimensions in the dataspace */
    unsigned     u;                      /* Local index variable */
    hsize_t      cur_dims[H5S_MAX_RANK]; /* size of dataspace dimensions */
    hsize_t      max_dims[H5S_MAX_RANK]; /* maximum size of dataspace dimensions */
    bool         unlim_max_dims = false; /* whether dataset has unlimited or max. dimension setting */
    void        *edata;
    H5E_auto2_t  func;
    H5D_layout_t layout;
    herr_t       ret_value = SUCCEED; /* Return value */

    /* Disable error reporting */
    H5Eget_auto2(H5E_DEFAULT, &func, &edata);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* Open the dataset */
    if ((did = H5Dopen2(fid, dsetname, H5P_DEFAULT)) < 0) {
        error_msg("unable to open dataset \"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }

    /* Get dataset's creation property list */
    if ((dcp = H5Dget_create_plist(did)) < 0) {
        error_msg("unable to get dataset's creation property list \"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }

    /* Query dataset's layout; the layout should be chunked or virtual */
    if ((layout = H5Pget_layout(dcp)) < 0) {
        error_msg("unable to get dataset layout \"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }
    if (layout != H5D_CHUNKED && layout != H5D_VIRTUAL) {
        error_msg("\"%s\" should be a chunked or virtual dataset\n", dsetname);
        ret_value = FAIL;
        goto done;
    }

    memset(cur_dims, 0, sizeof cur_dims);
    memset(max_dims, 0, sizeof max_dims);

    /* Get dataset's dataspace */
    if ((sid = H5Dget_space(did)) < 0) {
        error_msg("can't get dataset's dataspace\"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }

    /* Get dimension size of dataset's dataspace */
    if ((ndims = H5Sget_simple_extent_dims(sid, cur_dims, max_dims)) < 0) {
        error_msg("can't get dataspace dimensions for dataset \"%s\"\n", dsetname);
        ret_value = FAIL;
        goto done;
    }

    /* Check whether dataset has unlimited dimension or max. dimension setting */
    for (u = 0; u < (unsigned)ndims; u++)
        if (max_dims[u] == H5S_UNLIMITED || cur_dims[u] != max_dims[u]) {
            unlim_max_dims = true;
            break;
        }

    if (!unlim_max_dims) {
        error_msg("\"%s\" should have unlimited or max. dimension setting\n", dsetname);
        ret_value = FAIL;
    }

done:
    H5Eset_auto2(H5E_DEFAULT, func, edata);

    /* Closing */
    H5E_BEGIN_TRY
    H5Sclose(sid);
    H5Pclose(dcp);
    H5Dclose(did);
    H5E_END_TRY

    return (ret_value);
} /* check_dataset() */

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Close the H5 Tools library and exit
 *
 * Return:      Does not return
 *
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    h5tools_close();

    exit(ret);
}

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message about h5watch (only long options)
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    fflush(stdout);
    fprintf(stdout, "Usage: %s [OPTIONS] [OBJECT]\n", prog);
    fprintf(stdout, "\n");
    fprintf(stdout, "     OPTIONS\n");
    fprintf(stdout, "        --help            Print a usage message and exit.\n");
    fprintf(stdout, "        --version         Print version number and exit.\n");
    fprintf(stdout, "        --label           Label members of compound typed dataset.\n");
    fprintf(stdout, "        --simple          Use a machine-readable output format.\n");
    fprintf(stdout, "        --dim             Monitor changes in size of dataset dimensions only.\n");
    fprintf(stdout, "        --width=N         Set the number of columns to N for output.\n");
    fprintf(stdout, "                              A value of 0 sets the number of columns to the\n");
    fprintf(stdout, "                              maximum (65535). The default width is 80 columns.\n");
    fprintf(stdout, "        --polling=N       Set the polling interval to N (in seconds) when the\n");
    fprintf(stdout,
            "                              dataset will be checked for appended data.  The default\n");
    fprintf(stdout, "                              polling interval is 1.\n");
    fprintf(stdout, "        --fields=<list_of_fields>\n");
    fprintf(stdout,
            "                              Display data for the fields specified in <list_of_fields>\n");
    fprintf(stdout, "                              for a compound data type.  <list_of_fields> can be\n");
    fprintf(stdout, "                              specified as follows:\n");
    fprintf(stdout, "                                   1) A comma-separated list of field names in a\n");
    fprintf(stdout, "                                   compound data type.  \",\" is the separator\n");
    fprintf(stdout, "                                   for field names while \".\" is the separator\n");
    fprintf(stdout, "                                   for a nested field.\n");
    fprintf(stdout, "                                   2) A single field name in a compound data type.\n");
    fprintf(stdout, "                                   Can use this option multiple times.\n");
    fprintf(stdout, "                              Note that backslash is the escape character to avoid\n");
    fprintf(stdout,
            "                              characters in field names that conflict with the tool's\n");
    fprintf(stdout, "                              separators.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "     OBJECT is specified as [<filename>/<path_to_dataset>/<dsetname>]\n");
    fprintf(stdout, "        <filename>            Name of the HDF5 file.  It may be preceded by path\n");
    fprintf(stdout, "                              separated by slashes to the specified HDF5 file.\n");
    fprintf(stdout, "        <path_to_dataset>     Path separated by slashes to the specified dataset\n");
    fprintf(stdout, "        <dsetname>            Name of the dataset\n");
    fprintf(stdout, "\n");
    fprintf(stdout,
            "     User can end the h5watch process by ctrl-C (SIGINT) or kill the process (SIGTERM).\n");

} /* usage() */

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse the command line for h5watch (take only long options)
 *
 * Return:      Success:    Set the corresponding command flags and return void
 *              Failure:    Exits program with EXIT_FAILURE value.
 *
 *-------------------------------------------------------------------------
 */
static void
parse_command_line(int argc, const char *const *argv)
{
    int opt; /* Command line option */
    int tmp;

    /* no arguments */
    if (argc == 1) {
        usage(h5tools_getprogname());
        leave(EXIT_FAILURE);
    }

    /* parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case '?':
            case 'h': /* --help */
                usage(h5tools_getprogname());
                leave(EXIT_SUCCESS);
                break;

            case 'V': /* --version */
                print_version(progname);
                leave(EXIT_SUCCESS);
                break;

            case 'w': /* --width=N */
                g_display_width = (int)strtol(H5_optarg, NULL, 0);
                if (g_display_width < 0) {
                    usage(h5tools_getprogname());
                    leave(EXIT_FAILURE);
                }
                break;

            case 'd': /* --dim */
                g_monitor_size_only = true;
                break;

            case 'S': /* --simple */
                g_simple_output = true;
                break;

            case 'l': /* --label */
                g_label = true;
                break;

            case 'p': /* --polling=N */
                /* g_polling_interval = strtod(H5_optarg, NULL); */
                if ((tmp = (int)strtol(H5_optarg, NULL, 10)) <= 0) {
                    usage(h5tools_getprogname());
                    leave(EXIT_FAILURE);
                }
                g_polling_interval = (unsigned)tmp;
                break;

            case 'f': /* --fields=<list_of_fields> */
                if (g_list_of_fields == NULL) {
                    if ((g_list_of_fields = strdup(H5_optarg)) == NULL) {
                        error_msg("memory allocation failed (file %s:line %d)\n", __FILE__, __LINE__);
                        leave(EXIT_FAILURE);
                    }
                }
                else {
                    char *str;

                    if ((str = strdup(H5_optarg)) == NULL) {
                        error_msg("memory allocation failed (file %s:line %d)\n", __FILE__, __LINE__);
                        leave(EXIT_FAILURE);
                    }
                    if ((g_list_of_fields = (char *)realloc(g_list_of_fields, strlen(g_list_of_fields) +
                                                                                  strlen(str) + 2)) == NULL) {
                        error_msg("memory allocation failed (file %s:line %d)\n", __FILE__, __LINE__);
                        leave(EXIT_FAILURE);
                    }
                    strcat(g_list_of_fields, FIELD_SEP);
                    strcat(g_list_of_fields, str);
                }

                break;

            default:
                usage(h5tools_getprogname());
                leave(EXIT_FAILURE);
        }
    }

    /* check for object to be processed */
    if (argc <= H5_optind) {
        error_msg("missing dataset name\n");
        usage(h5tools_getprogname());
        leave(EXIT_FAILURE);
    }
} /* parse_command_line() */

/*-------------------------------------------------------------------------
 * Function:    catch_signal
 *
 * Purpose:     The signal handler to catch the signals:
 *              SIGTERM and SIGINT and set flag to get out of the main loop
 *
 * Return:      No return
 *
 *-------------------------------------------------------------------------
 */
static void
catch_signal(int H5_ATTR_UNUSED signo)
{
    /* Set the flag to get out of the main loop */
    g_user_interrupt = true;
} /* catch_signal() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     h5watch
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    char  drivername[50]; /* VFD name */
    char *fname = NULL;   /* File name */
    char *dname = NULL;   /* Dataset name */
    char *x;              /* Temporary string pointer */
    hid_t fid  = -1;      /* File ID */
    hid_t fapl = -1;      /* File access property list */

    /* Set up tool name and exit status */
    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /* To exit from h5watch for SIGTERM signal */
    if (HDsignal(SIGTERM, catch_signal) == SIG_ERR) {
        error_msg("An error occurred while setting a signal handler.\n");
        leave(EXIT_FAILURE);
    }

    /* To exit from h5watch for SIGINT signal */
    if (HDsignal(SIGINT, catch_signal) == SIG_ERR) {
        error_msg("An error occurred while setting a signal handler.\n");
        leave(EXIT_FAILURE);
    }

    /* parse command line options */
    parse_command_line(argc, (const char *const *)argv);

    if (argc <= H5_optind) {
        error_msg("missing dataset name\n");
        usage(h5tools_getprogname());
        leave(EXIT_FAILURE);
    }

    /* enable error reporting if command line option */
    h5tools_error_report();

    /* Mostly copied from tools/h5ls coding & modified accordingly */
    /*
     * [OBJECT] is specified as
     *        [<filename>/<path_to_dataset>/<dsetname>]
     *
     * Example: ../dir1/foo/bar/dset
     *          \_________/\______/
     *             file       obj
     *
     * The dichotomy is determined by calling H5Fopen() repeatedly until it
     * succeeds. The first call uses the entire name and each subsequent call
     * chops off the last component. If we reach the beginning of the name
     * then there must have been something wrong with the file (perhaps it
     * doesn't exist).
     */
    if ((fname = strdup(argv[H5_optind])) == NULL) {
        error_msg("memory allocation failed (file %s:line %d)\n", __FILE__, __LINE__);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Create a copy of file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    do {
        while (fname && *fname) {
            fid = h5tools_fopen(fname, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl, false, drivername,
                                sizeof drivername);

            if (fid >= 0) {
                fprintf(stdout, "Opened \"%s\" with %s driver.\n", fname, drivername);
                break; /*success*/
            }          /* end if */

            /* Shorten the file name; lengthen the object name */
            x     = dname;
            dname = strrchr(fname, '/');
            if (x)
                *x = '/';
            if (!dname)
                break;
            *dname = '\0';
        } /* end while */
        /* Try opening the file again if somehow unstable */
    } while (g_retry-- > 0 && fid == FAIL);

    if (fid < 0) {
        error_msg("unable to open file \"%s\"\n", fname);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    if (!dname) {
        error_msg("no dataset specified\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }
    else {
        *dname = '/';
        x      = dname;
        if ((dname = strdup(dname)) == NULL) {
            error_msg("memory allocation failed (file %s:line %d)\n", __FILE__, __LINE__);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else {
            *x = '\0';
            /* Validate dataset */
            if (check_dataset(fid, dname) < 0) {
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            }
            /* Validate input "fields" */
            else if (g_list_of_fields && *g_list_of_fields) {
                if (process_cmpd_fields(fid, dname) < 0) {
                    h5tools_setstatus(EXIT_FAILURE);
                    goto done;
                }
            }
        }
    }

    /* If everything is fine, start monitoring the dataset */
    if (h5tools_getstatus() != EXIT_FAILURE)
        if (monitor_dataset(fid, dname) < 0)
            h5tools_setstatus(EXIT_FAILURE);

done:
    /* Free spaces */
    if (fname)
        free(fname);
    if (dname)
        free(dname);
    if (g_list_of_fields)
        free(g_list_of_fields);
    if (g_listv) {
        H5LD_clean_vector(g_listv);
        free(g_listv);
    }
    if (g_dup_fields)
        free(g_dup_fields);

    /* Close the file access property list */
    if (fapl >= 0 && H5Pclose(fapl) < 0) {
        error_msg("unable to close file access property list\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    /* Close the file */
    if (fid >= 0 && H5Fclose(fid) < 0) {
        error_msg("unable to close file\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    /* exit */
    leave(h5tools_getstatus());
} /* main() */
