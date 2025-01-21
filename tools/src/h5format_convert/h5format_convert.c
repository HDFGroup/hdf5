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
 * We include the private header file so we can get to the uniform
 * programming environment it declares.
 * HDF5 API functions (except for H5G_basename())
 */
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5trav.h"

/* Name of tool */
#define PROGRAMNAME "h5format_convert"

static char *fname_g   = NULL;
static char *dname_g   = NULL;
static int   dset_g    = false;
static int   noop_g    = false;
static int   verbose_g = 0;

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char            *s_opts   = "hVvd:n";
static struct h5_long_options l_opts[] = {{"help", no_arg, 'h'},    {"version", no_arg, 'V'},
                                          {"verbose", no_arg, 'v'}, {"dname", require_arg, 'd'},
                                          {"noop", no_arg, 'n'},    {"enable-error-stack", no_arg, 'E'},
                                          {NULL, 0, '\0'}};

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print usage
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    fprintf(stdout, "usage: %s [OPTIONS] file_name\n", prog);
    fprintf(stdout, "  OPTIONS\n");
    fprintf(stdout, "   -h, --help                Print a usage message and exit\n");
    fprintf(stdout, "   -V, --version             Print version number and exit\n");
    fprintf(stdout, "   -v, --verbose             Turn on verbose mode\n");
    fprintf(stdout, "   -d dname, --dname=dataset_name    Pathname for the dataset\n");
    fprintf(stdout, "   -n, --noop                Perform all the steps except the actual conversion\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "Examples of use:\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "h5format_convert -d /group/dataset file_name\n");
    fprintf(stdout, "  Convert the dataset </group/dataset> in the HDF5 file <file_name>:\n");
    fprintf(stdout, "    a. chunked dataset: convert the chunk indexing type to version 1 B-tree\n");
    fprintf(stdout, "    b. compact/contiguous dataset: downgrade the layout version to 3\n");
    fprintf(stdout, "    c. virtual dataset: no action\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "h5format_convert file_name\n");
    fprintf(stdout, "  Convert all datasets in the HDF5 file <file_name>:\n");
    fprintf(stdout, "    a. chunked dataset: convert the chunk indexing type to version 1 B-tree\n");
    fprintf(stdout, "    b. compact/contiguous dataset: downgrade the layout version to 3\n");
    fprintf(stdout, "    c. virtual dataset: no action\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "h5format_convert -n -d /group/dataset file_name\n");
    fprintf(stdout, "  Go through all the steps except the actual conversion when \n");
    fprintf(stdout, "  converting the dataset </group/dataset> in the HDF5 file <file_name>.\n");
} /* usage() */

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *
 * Return: Success: 0
 *         Failure: 1
 *
 *-------------------------------------------------------------------------
 */
static int
parse_command_line(int argc, const char *const *argv)
{
    int opt;

    /* no arguments */
    if (argc == 1) {
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }

    /* parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case 'h':
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                goto error;

            case 'V':
                print_version(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                goto error;

            case 'v':
                verbose_g = true;
                break;

            case 'd': /* -d dname */
                if (H5_optarg != NULL && *H5_optarg)
                    dname_g = strdup(H5_optarg);
                if (dname_g == NULL) {
                    h5tools_setstatus(EXIT_FAILURE);
                    error_msg("No dataset name `%s`\n", H5_optarg);
                    usage(h5tools_getprogname());
                    goto error;
                }
                dset_g = true;
                break;

            case 'n': /* -n */
                noop_g = true;
                break;

            case 'E':
                enable_error_stack = 1;
                break;

            default:
                h5tools_setstatus(EXIT_FAILURE);
                usage(h5tools_getprogname());
                goto error;
                break;
        } /* switch */
    }     /* while */

    if (argc <= H5_optind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }

    fname_g = strdup(argv[H5_optind]);

    return 0;

error:
    return -1;
} /* parse_command_line() */

/*-------------------------------------------------------------------------
 * Function: leave
 *
 * Purpose: Close HDF5
 *
 * Return: Does not return
 *
 *-------------------------------------------------------------------------
 */
static void
leave(int ret)
{
    h5tools_close();

    exit(ret);
} /* leave() */

/*-------------------------------------------------------------------------
 * Function: convert()
 *
 * Purpose: To downgrade a dataset's indexing type or layout version:
 *        For chunked:
 *          Downgrade the chunk indexing type to version 1 B-tree
 *              If type is already version 1 B-tree, no further action
 *        For compact/contiguous:
 *          Downgrade the layout version from 4 to 3
 *          If version is already <= 3, no further action
 *        For virtual:
 *          No further action
 *
 * Return: Success: 0
 *         Failure: 1
 *
 *-------------------------------------------------------------------------
 */
static int
convert(hid_t fid, const char *dname)
{
    hid_t             dcpl = H5I_INVALID_HID;
    hid_t             did  = H5I_INVALID_HID;
    H5D_layout_t      layout_type;
    H5D_chunk_index_t idx_type;

    /* Open the dataset */
    if ((did = H5Dopen2(fid, dname, H5P_DEFAULT)) < 0) {
        error_msg("unable to open dataset \"%s\"\n", dname);
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }
    else if (verbose_g)
        fprintf(stdout, "Open the dataset\n");

    /* Get the dataset's creation property list */
    if ((dcpl = H5Dget_create_plist(did)) < 0) {
        error_msg("unable to get the dataset creation property list\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }

    /* Get the dataset's layout */
    if ((layout_type = H5Pget_layout(dcpl)) < 0) {
        error_msg("unable to get the dataset layout type\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }
    else if (verbose_g)
        fprintf(stdout, "Retrieve the dataset's layout\n");

    switch (layout_type) {
        case H5D_CHUNKED:
            if (verbose_g)
                fprintf(stdout, "Dataset is a chunked dataset\n");

            /* Get the dataset's chunk indexing type */
            if (H5Dget_chunk_index_type(did, &idx_type) < 0) {
                error_msg("unable to get the chunk indexing type for \"%s\"\n", dname);
                h5tools_setstatus(EXIT_FAILURE);
                goto error;
            }
            else if (verbose_g)
                fprintf(stdout, "Retrieve the dataset's chunk indexing type\n");

            if (idx_type == H5D_CHUNK_IDX_BTREE) {
                if (verbose_g)
                    fprintf(stdout,
                            "Dataset's chunk indexing type is already version 1 B-tree: no further action\n");
                h5tools_setstatus(EXIT_SUCCESS);
                goto done;
            }
            else if (verbose_g)
                fprintf(stdout, "Dataset's chunk indexing type is not version 1 B-tree\n");

            break;

        case H5D_CONTIGUOUS:
            if (verbose_g)
                fprintf(stdout, "Dataset is a contiguous dataset: downgrade layout version as needed\n");
            break;

        case H5D_COMPACT:
            if (verbose_g)
                fprintf(stdout, "Dataset is a compact dataset: downgrade layout version as needed\n");
            break;

        case H5D_VIRTUAL:
            if (verbose_g)
                fprintf(stdout, "No further action for virtual dataset\n");
            goto done;

        case H5D_NLAYOUTS:
        case H5D_LAYOUT_ERROR:
        default:
            error_msg("unknown layout type for \"%s\"\n", dname);
            h5tools_setstatus(EXIT_FAILURE);
            goto error;

    } /* end switch */

    /* No further action if it is a noop */
    if (noop_g) {
        if (verbose_g)
            fprintf(stdout, "Not converting the dataset\n");
        h5tools_setstatus(EXIT_SUCCESS);
        goto done;
    }

    if (verbose_g)
        fprintf(stdout, "Converting the dataset...\n");

    /* Downgrade the dataset */
    if (H5Dformat_convert(did) < 0) {
        error_msg("unable to downgrade dataset \"%s\"\n", dname);
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }
    else if (verbose_g)
        fprintf(stdout, "Done\n");

done:
    /* Close the dataset */
    if (H5Dclose(did) < 0) {
        error_msg("unable to close dataset \"%s\"\n", dname);
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }
    else if (verbose_g)
        fprintf(stdout, "Close the dataset\n");

    /* Close the dataset creation property list */
    if (H5Pclose(dcpl) < 0) {
        error_msg("unable to close dataset creation property list\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto error;
    }
    else if (verbose_g)
        printf("Close the dataset creation property list\n");

    return 0;

error:
    if (verbose_g)
        fprintf(stdout, "Error encountered\n");

    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Dclose(did);
    }
    H5E_END_TRY

    return -1;
} /* convert() */

/*-------------------------------------------------------------------------
 * Function:    convert_dsets_cb()
 *
 * Purpose:     The callback routine from the traversal to convert the
 *              chunk indexing type of the dataset object.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
convert_dsets_cb(const char *path, const H5O_info2_t *oi, const char *already_visited, void *_fid)
{
    hid_t fid = *(hid_t *)_fid;

    /* If the object has already been seen then just return */
    if (NULL == already_visited) {
        if (oi->type == H5O_TYPE_DATASET) {
            if (verbose_g)
                fprintf(stdout, "Going to process dataset:%s...\n", path);
            if (convert(fid, path) < 0)
                goto error;
        } /* end if */
    }     /* end if */

    return 0;

error:
    return -1;
} /* end convert_dsets_cb() */

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: To convert the chunk indexing type of a dataset in a file to
 *        version 1 B-tree.
 *
 * Return: Success: 0
 *         Failure: 1
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t fid = H5I_INVALID_HID;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Parse command line options */
    if (parse_command_line(argc, (const char *const *)argv) < 0)
        goto done;
    else if (verbose_g)
        fprintf(stdout, "Process command line options\n");

    if (noop_g && verbose_g)
        fprintf(stdout, "It is noop...\n");

    /* enable error reporting if command line option */
    h5tools_error_report();

    /* Open the HDF5 file */
    if ((fid = h5tools_fopen(fname_g, H5F_ACC_RDWR, H5P_DEFAULT, false, NULL, 0)) < 0) {
        error_msg("unable to open file \"%s\"\n", fname_g);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }
    else if (verbose_g)
        fprintf(stdout, "Open the file %s\n", fname_g);

    if (dset_g) { /* Convert a specified dataset in the file */
        if (verbose_g)
            fprintf(stdout, "Going to process dataset: %s...\n", dname_g);
        if (convert(fid, dname_g) < 0)
            goto done;
    }
    else { /* Convert all datasets in the file */
        if (verbose_g)
            fprintf(stdout, "Processing all datasets in the file...\n");
        if (h5trav_visit(fid, "/", true, true, convert_dsets_cb, NULL, &fid, H5O_INFO_BASIC) < 0)
            goto done;
    } /* end else */

    if (verbose_g) {
        if (noop_g) {
            fprintf(stdout, "Not processing the file's superblock...\n");
            h5tools_setstatus(EXIT_SUCCESS);
            goto done;
        } /* end if */
        fprintf(stdout, "Processing the file's superblock...\n");
    } /* end if */

    /* Process superblock */
    if (H5Fformat_convert(fid) < 0) {
        error_msg("unable to convert file's superblock\"%s\"\n", fname_g);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    } /* end if */

done:
    /* Close the file */
    if (fid >= 0) {
        if (H5Fclose(fid) < 0) {
            error_msg("unable to close file \"%s\"\n", fname_g);
            h5tools_setstatus(EXIT_FAILURE);
        }
        else if (verbose_g) {
            fprintf(stdout, "Close the file\n");
        }
    } /* end if */

    if (fname_g)
        free(fname_g);
    if (dname_g)
        free(dname_g);

    leave(h5tools_getstatus());

} /* end main() */
