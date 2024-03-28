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

#include "H5private.h"
#include "h5diff.h"
#include "h5diff_common.h"
#include "h5tools.h"
#include "h5tools_utils.h"

static int check_n_input(const char *);
static int check_p_input(const char *);
static int check_d_input(const char *);

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char            *s_opts   = "cd:ehln:p:qrv*xA:CE:NS*V";
static struct h5_long_options l_opts[] = {{"compare", no_arg, 'c'},
                                          {"delta", require_arg, 'd'},
                                          {"use-system-epsilon", no_arg, 'e'},
                                          {"help", no_arg, 'h'},
                                          {"follow-symlinks", no_arg, 'l'},
                                          {"count", require_arg, 'n'},
                                          {"relative", require_arg, 'p'},
                                          {"quiet", no_arg, 'q'},
                                          {"report", no_arg, 'r'},
                                          {"verbose", optional_arg, 'v'},
                                          {"no-dangling-links", no_arg, 'x'},
                                          {"exclude-attribute", require_arg, 'A'},
                                          {"no-compact-subset", no_arg, 'C'},
                                          {"exclude-path", require_arg, 'E'},
                                          {"nan", no_arg, 'N'},
                                          {"enable-error-stack", optional_arg, 'S'},
                                          {"version", no_arg, 'V'},
                                          {"vol-value-1", require_arg, '1'},
                                          {"vol-name-1", require_arg, '2'},
                                          {"vol-info-1", require_arg, '3'},
                                          {"vol-value-2", require_arg, '4'},
                                          {"vol-name-2", require_arg, '5'},
                                          {"vol-info-2", require_arg, '6'},
                                          {"vfd-value-1", require_arg, '7'},
                                          {"vfd-name-1", require_arg, '8'},
                                          {"vfd-info-1", require_arg, '9'},
                                          {"vfd-value-2", require_arg, '0'},
                                          {"vfd-name-2", require_arg, 'Y'},
                                          {"vfd-info-2", require_arg, 'Z'},
                                          {NULL, 0, '\0'}};

static H5FD_onion_fapl_info_t onion_fa_g_1 = {
    H5FD_ONION_FAPL_INFO_VERSION_CURR,
    H5P_DEFAULT,                   /* backing_fapl_id                */
    32,                            /* page_size                      */
    H5FD_ONION_STORE_TARGET_ONION, /* store_target                   */
    H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
    0,                  /* force_write_open               */
    0,                  /* creation_flags                 */
    "first input file", /* comment                        */
};

static H5FD_onion_fapl_info_t onion_fa_g_2 = {
    H5FD_ONION_FAPL_INFO_VERSION_CURR,
    H5P_DEFAULT,                   /* backing_fapl_id                */
    32,                            /* page_size                      */
    H5FD_ONION_STORE_TARGET_ONION, /* store_target                   */
    H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
    0,                   /* force_write_open               */
    0,                   /* creation_flags                 */
    "second input file", /* comment                        */
};

/*-------------------------------------------------------------------------
 * Function: check_options
 *
 * Purpose: parse command line input
 *-------------------------------------------------------------------------
 */
static void
check_options(diff_opt_t *opts)
{
    /*--------------------------------------------------------------
     * check for mutually exclusive options
     *--------------------------------------------------------------*/

    /* check between -d , -p, --use-system-epsilon.
     * These options are mutually exclusive.
     */
    if ((opts->delta_bool + opts->percent_bool + opts->use_system_epsilon) > 1) {
        printf("%s error: -d, -p and --use-system-epsilon options are mutually-exclusive;\n", PROGRAMNAME);
        printf("use no more than one.\n");
        printf("Try '-h' or '--help' option for more information or see the %s entry in the 'HDF5 "
               "Reference Manual'.\n",
               PROGRAMNAME);
        h5diff_exit(EXIT_FAILURE);
    }
}

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *-------------------------------------------------------------------------
 */

void
parse_command_line(int argc, const char *const *argv, const char **fname1, const char **fname2,
                   const char **objname1, const char **objname2, diff_opt_t *opts)
{
    int                       i;
    int                       opt;
    struct exclude_path_list *exclude_head, *exclude_prev, *exclude_node;
    struct exclude_path_list *exclude_attr_head, *exclude_attr_prev, *exclude_attr_node;

    H5TOOLS_START_DEBUG(" ");
    /* process the command-line */
    memset(opts, 0, sizeof(diff_opt_t));

    /* assume equal contents initially */
    opts->contents = 1;

    /* NaNs are handled by default */
    opts->do_nans = 1;

    /* not Listing objects that are not comparable */
    opts->mode_list_not_cmp = 0;

    /* initially no not-comparable. */
    /**this is bad in mixing option with results**/
    opts->not_cmp = 0;

    /* init for exclude-path option */
    exclude_head = NULL;

    /* init for exclude-attribute option */
    exclude_attr_head = NULL;

    /* parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            default:
                usage();
                h5diff_exit(EXIT_FAILURE);
                break;

            case 'h':
                usage();
                h5diff_exit(EXIT_SUCCESS);
                break;

            case 'V':
                print_version(h5tools_getprogname());
                h5diff_exit(EXIT_SUCCESS);
                break;

            case 'v':
                opts->mode_verbose = 1;
                for (i = 1; i < argc; i++) {
                    /*
                     * special check for short opt
                     */
                    if (!strcmp(argv[i], "-v")) {
                        if (H5_optarg != NULL)
                            H5_optind--;
                        opts->mode_verbose_level = 0;
                        break;
                    }
                    else if (!strncmp(argv[i], "-v", (size_t)2)) {
                        if (H5_optarg != NULL)
                            H5_optind--;
                        opts->mode_verbose_level = atoi(&argv[i][2]);
                        break;
                    }
                    else {
                        if (H5_optarg != NULL)
                            opts->mode_verbose_level = atoi(H5_optarg);
                        else
                            opts->mode_verbose_level = 0;
                    }
                }
                break;

            case 'q':
                /* use quiet mode; suppress the message "0 differences found" */
                opts->mode_quiet = 1;
                break;

            case 'r':
                opts->mode_report = 1;
                break;

            case 'l':
                opts->follow_links = true;
                break;

            case 'x':
                opts->no_dangle_links = 1;
                break;

            case 'S':
                if (H5_optarg != NULL)
                    enable_error_stack = atoi(H5_optarg);
                else
                    enable_error_stack = 1;
                break;

            case 'E':
                opts->exclude_path = 1;

                /* create linked list of excluding objects */
                if ((exclude_node = (struct exclude_path_list *)malloc(sizeof(struct exclude_path_list))) ==
                    NULL) {
                    printf("Error: lack of memory!\n");
                    h5diff_exit(EXIT_FAILURE);
                }

                /* init */
                exclude_node->obj_path = H5_optarg;
                exclude_node->obj_type = H5TRAV_TYPE_UNKNOWN;
                exclude_prev           = exclude_head;

                if (NULL == exclude_head) {
                    exclude_head       = exclude_node;
                    exclude_head->next = NULL;
                }
                else {
                    while (NULL != exclude_prev->next)
                        exclude_prev = exclude_prev->next;

                    exclude_node->next = NULL;
                    exclude_prev->next = exclude_node;
                }
                break;

            case 'C':
                opts->disable_compact_subset = true;
                break;

            case 'A':
                opts->exclude_attr_path = 1;

                /* create linked list of excluding objects */
                if ((exclude_attr_node =
                         (struct exclude_path_list *)malloc(sizeof(struct exclude_path_list))) == NULL) {
                    printf("Error: lack of memory!\n");
                    h5diff_exit(EXIT_FAILURE);
                }

                /* init */
                exclude_attr_node->obj_path = H5_optarg;
                exclude_attr_node->obj_type = H5TRAV_TYPE_UNKNOWN;
                exclude_attr_prev           = exclude_attr_head;

                if (NULL == exclude_attr_head) {
                    exclude_attr_head       = exclude_attr_node;
                    exclude_attr_head->next = NULL;
                }
                else {
                    while (NULL != exclude_attr_prev->next)
                        exclude_attr_prev = exclude_attr_prev->next;

                    exclude_attr_node->next = NULL;
                    exclude_attr_prev->next = exclude_attr_node;
                }
                break;

            case 'd':
                opts->delta_bool = 1;

                if (check_d_input(H5_optarg) == -1) {
                    printf("<-d %s> is not a valid option\n", H5_optarg);
                    usage();
                    h5diff_exit(EXIT_FAILURE);
                }
                opts->delta = atof(H5_optarg);
                /* do not check against default, the DBL_EPSILON is being replaced by user */
                break;

            case 'p':
                opts->percent_bool = 1;
                if (check_p_input(H5_optarg) == -1) {
                    printf("<-p %s> is not a valid option\n", H5_optarg);
                    usage();
                    h5diff_exit(EXIT_FAILURE);
                }
                opts->percent = atof(H5_optarg);

                /* -p 0 is the same as default */
                if (H5_DBL_ABS_EQUAL(opts->percent, 0.0))
                    opts->percent_bool = 0;
                break;

            case 'n':
                opts->count_bool = 1;
                if (check_n_input(H5_optarg) == -1) {
                    printf("<-n %s> is not a valid option\n", H5_optarg);
                    usage();
                    h5diff_exit(EXIT_FAILURE);
                }
                opts->count = strtoull(H5_optarg, NULL, 0);
                break;

            case 'N':
                opts->do_nans = 0;
                break;

            case 'c':
                opts->mode_list_not_cmp = 1;
                break;

            case 'e':
                opts->use_system_epsilon = 1;
                break;

            case '1':
                opts->vol_info[0].type    = VOL_BY_VALUE;
                opts->vol_info[0].u.value = (H5VL_class_value_t)atoi(H5_optarg);
                opts->custom_vol[0]       = true;
                break;

            case '2':
                opts->vol_info[0].type   = VOL_BY_NAME;
                opts->vol_info[0].u.name = H5_optarg;
                opts->custom_vol[0]      = true;
                break;

            case '3':
                opts->vol_info[0].info_string = H5_optarg;
                break;

            case '4':
                opts->vol_info[1].type    = VOL_BY_VALUE;
                opts->vol_info[1].u.value = (H5VL_class_value_t)atoi(H5_optarg);
                opts->custom_vol[1]       = true;
                break;

            case '5':
                opts->vol_info[1].type   = VOL_BY_NAME;
                opts->vol_info[1].u.name = H5_optarg;
                opts->custom_vol[1]      = true;
                break;

            case '6':
                opts->vol_info[1].info_string = H5_optarg;
                break;

            case '7':
                opts->vfd_info[0].type    = VFD_BY_VALUE;
                opts->vfd_info[0].u.value = (H5FD_class_value_t)atoi(H5_optarg);
                opts->custom_vfd[0]       = true;
                break;

            case '8':
                opts->vfd_info[0].type   = VFD_BY_NAME;
                opts->vfd_info[0].u.name = H5_optarg;
                opts->custom_vfd[0]      = true;
                break;

            case '9':
                opts->vfd_info[0].info = (const void *)H5_optarg;
                break;

            case '0':
                opts->vfd_info[1].type    = VFD_BY_VALUE;
                opts->vfd_info[1].u.value = (H5FD_class_value_t)atoi(H5_optarg);
                opts->custom_vfd[1]       = true;
                break;

            case 'Y':
                opts->vfd_info[1].type   = VFD_BY_NAME;
                opts->vfd_info[1].u.name = H5_optarg;
                opts->custom_vfd[1]      = true;
                break;

            case 'Z':
                opts->vfd_info[1].info = (const void *)H5_optarg;
                break;
        }
    }

    /* If file 1 uses the onion VFD, get the revision number */
    if (opts->vfd_info[0].u.name && !strcmp(opts->vfd_info[0].u.name, "onion")) {
        if (opts->vfd_info[0].info) {
            errno                     = 0;
            onion_fa_g_1.revision_num = strtoull(opts->vfd_info[0].info, NULL, 10);
            if (errno == ERANGE) {
                printf("Invalid onion revision specified for file 1\n");
                usage();
                h5diff_exit(EXIT_FAILURE);
            }
        }
        else
            onion_fa_g_1.revision_num = 0;

        opts->vfd_info[0].info = &onion_fa_g_1;
    }

    /* If file 2 uses the onion VFD, get the revision number */
    if (opts->vfd_info[1].u.name && !strcmp(opts->vfd_info[1].u.name, "onion")) {
        if (opts->vfd_info[1].info) {
            errno                     = 0;
            onion_fa_g_2.revision_num = strtoull(opts->vfd_info[1].info, NULL, 10);
            if (errno == ERANGE) {
                printf("Invalid onion revision specified for file 2\n");
                usage();
                h5diff_exit(EXIT_FAILURE);
            }
        }
        else
            onion_fa_g_2.revision_num = 0;

        opts->vfd_info[1].info = &onion_fa_g_2;
    }

    /* check options */
    check_options(opts);

    /* if exclude-path option is used, keep the exclude path list */
    if (opts->exclude_path)
        opts->exclude = exclude_head;

    /* if exclude-attribute option is used, keep the exclude attr list */
    if (opts->exclude_attr_path)
        opts->exclude_attr = exclude_attr_head;

    /* check for file names to be processed */
    if (argc <= H5_optind || argv[H5_optind + 1] == NULL) {
        error_msg("missing file names\n");
        usage();
        h5diff_exit(EXIT_FAILURE);
    }

    *fname1   = argv[H5_optind];
    *fname2   = argv[H5_optind + 1];
    *objname1 = argv[H5_optind + 2];
    H5TOOLS_DEBUG("file1 = %s", *fname1);
    H5TOOLS_DEBUG("file2 = %s", *fname2);

    if (*objname1 == NULL) {
        *objname2 = NULL;
        H5TOOLS_ENDDEBUG("No obj names");
        return;
    }
    H5TOOLS_DEBUG("objname1 = %s", *objname1);

    if (argv[H5_optind + 3] != NULL) {
        *objname2 = argv[H5_optind + 3];
    }
    else {
        *objname2 = *objname1;
    }
    H5TOOLS_DEBUG("objname2 = %s", *objname2);

    if (!opts->disable_compact_subset) {
        opts->sset[0] = parse_subset_params(*objname1);
        opts->sset[1] = parse_subset_params(*objname2);
    }

    H5TOOLS_ENDDEBUG(" ");
}

/*-------------------------------------------------------------------------
 * Function: print_info
 *
 * Purpose: print several information messages after h5diff call
 *-------------------------------------------------------------------------
 */
void
print_info(diff_opt_t *opts)
{
    if (opts->mode_quiet || opts->err_stat)
        return;

    if (opts->cmn_objs == 0) {
        printf("No common objects found. Files are not comparable.\n");
        if (!opts->mode_verbose)
            printf("Use -v for a list of objects.\n");
    }

    if (opts->not_cmp == 1) {
        if (opts->mode_list_not_cmp == 0) {
            printf("--------------------------------\n");
            printf("Some objects are not comparable\n");
            printf("--------------------------------\n");
            if (opts->mode_verbose)
                printf("Use -c for a list of objects without details of differences.\n");
            else
                printf("Use -c for a list of objects.\n");
        }
    }
}

/*-------------------------------------------------------------------------
 * Function: check_n_input
 *
 * Purpose: check for valid input
 *
 * Return: 1 for ok, -1 for fail
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE static int
check_n_input(const char *str)
{
    unsigned i;
    char     c;

    for (i = 0; i < strlen(str); i++) {
        c = str[i];
        if (i == 0) {
            if (c < 49 || c > 57) /* ascii values between 1 and 9 */
                return -1;
        }
        else if (c < 48 || c > 57) /* 0 also */
            return -1;
    }
    return 1;
}

/*-------------------------------------------------------------------------
 * Function: check_p_input
 *
 * Purpose: check for a valid p option input
 *
 * Return: 1 for ok, -1 for fail
 *-------------------------------------------------------------------------
 */
static int
check_p_input(const char *str)
{
    double x;

    /*
     * the atof return value on a hexadecimal input is different
     * on some systems; we do a character check for this
     */
    if (strlen(str) > 2 && str[0] == '0' && str[1] == 'x')
        return -1;

    x = atof(str);
    if (x < 0)
        return -1;

    return 1;
}

/*-------------------------------------------------------------------------
 * Function: check_d_input
 *
 * Purpose: check for a valid d option input
 *
 * Return: 1 for ok, -1 for fail
 *-------------------------------------------------------------------------
 */
static int
check_d_input(const char *str)
{
    double x;

    /*
     * the atof return value on a hexadecimal input is different
     * on some systems; we do a character check for this
     */
    if (strlen(str) > 2 && str[0] == '0' && str[1] == 'x')
        return -1;

    x = atof(str);
    if (x < 0)
        return -1;

    return 1;
}

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print a usage message
 *
 * Return: void
 *-------------------------------------------------------------------------
 */

void
usage(void)
{
    PRINTVALSTREAM(rawoutstream, "usage: h5diff [OPTIONS] file1 file2 [obj1[ obj2]]\n");
    PRINTVALSTREAM(rawoutstream, "  file1             File name of the first HDF5 file\n");
    PRINTVALSTREAM(rawoutstream, "  file2             File name of the second HDF5 file\n");
    PRINTVALSTREAM(rawoutstream, "  [obj1]            Name of an HDF5 object, in absolute path\n");
    PRINTVALSTREAM(rawoutstream, "  [obj2]            Name of an HDF5 object, in absolute path\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  ERROR\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --enable-error-stack Prints messages from the HDF5 error stack as they occur.\n");
    PRINTVALSTREAM(rawoutstream, "                        Optional value 2 also prints file open errors.\n");
    PRINTVALSTREAM(rawoutstream, "  OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "   -h, --help\n");
    PRINTVALSTREAM(rawoutstream, "         Print a usage message and exit.\n");
    PRINTVALSTREAM(rawoutstream, "   -V, --version\n");
    PRINTVALSTREAM(rawoutstream, "         Print version number and exit.\n");
    PRINTVALSTREAM(rawoutstream, "   -r, --report\n");
    PRINTVALSTREAM(rawoutstream, "         Report mode. Print differences.\n");
    PRINTVALSTREAM(rawoutstream, "   -v --verbose\n");
    PRINTVALSTREAM(rawoutstream,
                   "         Verbose mode. Print differences information and list of objects.\n");
    PRINTVALSTREAM(rawoutstream, "   -vN --verbose=N\n");
    PRINTVALSTREAM(rawoutstream,
                   "         Verbose mode with level. Print differences and list of objects.\n");
    PRINTVALSTREAM(rawoutstream, "         Level of detail depends on value of N:\n");
    PRINTVALSTREAM(rawoutstream, "          0 : Identical to '-v' or '--verbose'.\n");
    PRINTVALSTREAM(rawoutstream,
                   "          1 : All level 0 information plus one-line attribute status summary.\n");
    PRINTVALSTREAM(rawoutstream,
                   "          2 : All level 1 information plus extended attribute status report.\n");
    PRINTVALSTREAM(rawoutstream, "          3 : All level 2 information plus file names.\n");
    PRINTVALSTREAM(rawoutstream, "   -q, --quiet\n");
    PRINTVALSTREAM(rawoutstream, "         Quiet mode. Do not produce output.\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vol-value-1           Value (ID) of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                           first HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vol-name-1            Name of the VOL connector to use for opening the first\n");
    PRINTVALSTREAM(rawoutstream, "                           HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vol-info-1            VOL-specific info to pass to the VOL connector used for\n");
    PRINTVALSTREAM(rawoutstream, "                           opening the first HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vol-value-2           Value (ID) of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                           second HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vol-name-2            Name of the VOL connector to use for opening the second\n");
    PRINTVALSTREAM(rawoutstream, "                           HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vol-info-2            VOL-specific info to pass to the VOL connector used for\n");
    PRINTVALSTREAM(rawoutstream, "                           opening the second HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "                           If none of the above options are used to "
                                 "specify a VOL for a file, then\n");
    PRINTVALSTREAM(
        rawoutstream,
        "                           the VOL named by HDF5_VOL_CONNECTOR (or the native VOL connector,\n");
    PRINTVALSTREAM(rawoutstream,
                   "                           if that environment variable is unset) will be used\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vfd-value-1           Value (ID) of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                           first HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vfd-name-1            Name of the VFL driver to use for opening the first\n");
    PRINTVALSTREAM(rawoutstream, "                           HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vfd-info-1            VFD-specific info to pass to the VFL driver used for\n");
    PRINTVALSTREAM(rawoutstream, "                           opening the first HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vfd-value-2           Value (ID) of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                           second HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vfd-name-2            Name of the VFL driver to use for opening the second\n");
    PRINTVALSTREAM(rawoutstream, "                           HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vfd-info-2            VFD-specific info to pass to the VFL driver used for\n");
    PRINTVALSTREAM(rawoutstream, "                           opening the second HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "   --follow-symlinks\n");
    PRINTVALSTREAM(rawoutstream,
                   "         Follow symbolic links (soft links and external links and compare the)\n");
    PRINTVALSTREAM(rawoutstream, "         links' target objects.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         If symbolic link(s) with the same name exist in the files being\n");
    PRINTVALSTREAM(rawoutstream,
                   "         compared, then determine whether the target of each link is an existing\n");
    PRINTVALSTREAM(rawoutstream,
                   "         object (dataset, group, or named datatype) or the link is a dangling\n");
    PRINTVALSTREAM(rawoutstream,
                   "         link (a soft or external link pointing to a target object that does\n");
    PRINTVALSTREAM(rawoutstream, "         not yet exist).\n");
    PRINTVALSTREAM(rawoutstream,
                   "         - If both symbolic links are dangling links, they are treated as being\n");
    PRINTVALSTREAM(rawoutstream, "           the same; by default, h5diff returns an exit code of 0.\n");
    PRINTVALSTREAM(rawoutstream,
                   "           If, however, --no-dangling-links is used with --follow-symlinks,\n");
    PRINTVALSTREAM(rawoutstream, "           this situation is treated as an error and h5diff returns an\n");
    PRINTVALSTREAM(rawoutstream, "           exit code of 2.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         - If only one of the two links is a dangling link,they are treated as\n");
    PRINTVALSTREAM(rawoutstream, "           being different and h5diff returns an exit code of 1.\n");
    PRINTVALSTREAM(rawoutstream,
                   "           If, however, --no-dangling-links is used with --follow-symlinks,\n");
    PRINTVALSTREAM(rawoutstream, "           this situation is treated as an error and h5diff returns an\n");
    PRINTVALSTREAM(rawoutstream, "           exit code of 2.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         - If both symbolic links point to existing objects, h5diff compares the\n");
    PRINTVALSTREAM(rawoutstream, "           two objects.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         If any symbolic link specified in the call to h5diff does not exist,\n");
    PRINTVALSTREAM(rawoutstream, "         h5diff treats it as an error and returns an exit code of 2.\n");
    PRINTVALSTREAM(rawoutstream, "   --no-dangling-links\n");
    PRINTVALSTREAM(rawoutstream,
                   "         Must be used with --follow-symlinks option; otherwise, h5diff shows\n");
    PRINTVALSTREAM(rawoutstream, "         error message and returns an exit code of 2.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         Check for any symbolic links (soft links or external links) that do not\n");
    PRINTVALSTREAM(rawoutstream,
                   "         resolve to an existing object (dataset, group, or named datatype).\n");
    PRINTVALSTREAM(rawoutstream,
                   "         If any dangling link is found, this situation is treated as an error\n");
    PRINTVALSTREAM(rawoutstream, "         and h5diff returns an exit code of 2.\n");
    PRINTVALSTREAM(rawoutstream, "   -c, --compare\n");
    PRINTVALSTREAM(rawoutstream, "         List objects that are not comparable\n");
    PRINTVALSTREAM(rawoutstream, "   -N, --nan\n");
    PRINTVALSTREAM(rawoutstream, "         Avoid NaNs detection\n");
    PRINTVALSTREAM(rawoutstream, "   -n C, --count=C\n");
    PRINTVALSTREAM(rawoutstream, "         Print differences up to C. C must be a positive integer.\n");
    PRINTVALSTREAM(rawoutstream, "   -d D, --delta=D\n");
    PRINTVALSTREAM(rawoutstream,
                   "         Print difference if (|a-b| > D). D must be a positive number. Where a\n");
    PRINTVALSTREAM(rawoutstream,
                   "         is the data point value in file1 and b is the data point value in file2.\n");
    PRINTVALSTREAM(rawoutstream, "         Can not use with '-p' or '--use-system-epsilon'.\n");
    PRINTVALSTREAM(rawoutstream, "   -p R, --relative=R\n");
    PRINTVALSTREAM(rawoutstream,
                   "         Print difference if (|(a-b)/b| > R). R must be a positive number. Where a\n");
    PRINTVALSTREAM(rawoutstream,
                   "         is the data point value in file1 and b is the data point value in file2.\n");
    PRINTVALSTREAM(rawoutstream, "         Can not use with '-d' or '--use-system-epsilon'.\n");
    PRINTVALSTREAM(rawoutstream, "   --use-system-epsilon\n");
    PRINTVALSTREAM(
        rawoutstream,
        "         Print difference if (|a-b| > EPSILON), EPSILON is system defined value. Where a\n");
    PRINTVALSTREAM(rawoutstream,
                   "         is the data point value in file1 and b is the data point value in file2.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         If the system epsilon is not defined,one of the following predefined\n");
    PRINTVALSTREAM(rawoutstream, "         values will be used:\n");
    PRINTVALSTREAM(rawoutstream, "           FLT_EPSILON = 1.19209E-07 for floating-point type\n");
    PRINTVALSTREAM(rawoutstream, "           DBL_EPSILON = 2.22045E-16 for double precision type\n");
    PRINTVALSTREAM(rawoutstream, "         Can not use with '-p' or '-d'.\n");
    PRINTVALSTREAM(rawoutstream, "   --exclude-path \"path\"\n");
    PRINTVALSTREAM(rawoutstream,
                   "         Exclude the specified path to an object when comparing files or groups.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         If a group is excluded, all member objects will also be excluded.\n");
    PRINTVALSTREAM(rawoutstream, "         The specified path is excluded wherever it occurs.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         This flexibility enables the same option to exclude either objects that\n");
    PRINTVALSTREAM(rawoutstream,
                   "         exist only in one file or common objects that are known to differ.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "         When comparing files, \"path\" is the absolute path to the excluded;\n");
    PRINTVALSTREAM(rawoutstream,
                   "         object; when comparing groups, \"path\" is similar to the relative\n");
    PRINTVALSTREAM(rawoutstream,
                   "         path from the group to the excluded object. This \"path\" can be\n");
    PRINTVALSTREAM(rawoutstream,
                   "         taken from the first section of the output of the --verbose option.\n");
    PRINTVALSTREAM(rawoutstream,
                   "         For example, if you are comparing the group /groupA in two files and\n");
    PRINTVALSTREAM(rawoutstream,
                   "         you want to exclude /groupA/groupB/groupC in both files, the exclude\n");
    PRINTVALSTREAM(rawoutstream, "         option would read as follows:\n");
    PRINTVALSTREAM(rawoutstream, "           --exclude-path \"/groupB/groupC\"\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "         If there are multiple paths to an object, only the specified path(s)\n");
    PRINTVALSTREAM(rawoutstream,
                   "         will be excluded; the comparison will include any path not explicitly\n");
    PRINTVALSTREAM(rawoutstream, "         excluded.\n");
    PRINTVALSTREAM(rawoutstream, "         This option can be used repeatedly to exclude multiple paths.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   --exclude-attribute \"path/to/object/with/attribute\"\n");
    PRINTVALSTREAM(
        rawoutstream,
        "         Exclude attributes on the specified path to an object when comparing files or groups.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "         If there are multiple paths to an object, only the specified path(s)\n");
    PRINTVALSTREAM(rawoutstream,
                   "         will be excluded; the comparison will include any path not explicitly\n");
    PRINTVALSTREAM(rawoutstream, "         excluded.\n");
    PRINTVALSTREAM(rawoutstream, "         This option can be used repeatedly to exclude multiple paths.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, " Modes of output:\n");
    PRINTVALSTREAM(rawoutstream,
                   "  Default mode: print the number of differences found and where they occurred\n");
    PRINTVALSTREAM(rawoutstream, "  -r Report mode: print the above plus the differences\n");
    PRINTVALSTREAM(rawoutstream, "  -v Verbose mode: print the above plus a list of objects and warnings\n");
    PRINTVALSTREAM(rawoutstream, "  -q Quiet mode: do not print output\n");

    PRINTVALSTREAM(rawoutstream, "\n");

    PRINTVALSTREAM(rawoutstream, " File comparison:\n");
    PRINTVALSTREAM(rawoutstream,
                   "  If no objects [obj1[ obj2]] are specified, the h5diff comparison proceeds as\n");
    PRINTVALSTREAM(rawoutstream,
                   "  a comparison of the two files' root groups.  That is, h5diff first compares\n");
    PRINTVALSTREAM(rawoutstream,
                   "  the names of root group members, generates a report of root group objects\n");
    PRINTVALSTREAM(rawoutstream,
                   "  that appear in only one file or in both files, and recursively compares\n");
    PRINTVALSTREAM(rawoutstream, "  common objects.\n");
    PRINTVALSTREAM(rawoutstream, "\n");

    PRINTVALSTREAM(rawoutstream, " Object comparison:\n");
    PRINTVALSTREAM(rawoutstream, "  1) Groups\n");
    PRINTVALSTREAM(rawoutstream,
                   "      First compares the names of member objects (relative path, from the\n");
    PRINTVALSTREAM(rawoutstream,
                   "      specified group) and generates a report of objects that appear in only\n");
    PRINTVALSTREAM(rawoutstream,
                   "      one group or in both groups. Common objects are then compared recursively.\n");
    PRINTVALSTREAM(rawoutstream, "  2) Attributes and Datasets\n");
    PRINTVALSTREAM(rawoutstream,
                   "      Array rank and dimensions, datatypes, and data values are compared.\n");
    PRINTVALSTREAM(rawoutstream, "  3) Datatypes\n");
    PRINTVALSTREAM(rawoutstream, "      The comparison is based on the return value of H5Tequal.\n");
    PRINTVALSTREAM(rawoutstream, "  4) Symbolic links\n");
    PRINTVALSTREAM(rawoutstream, "      The paths to the target objects are compared.\n");
    PRINTVALSTREAM(rawoutstream, "      (The option --follow-symlinks overrides the default behavior when\n");
    PRINTVALSTREAM(rawoutstream, "       symbolic links are compared.).\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    /*
     * TRILABS_227 is complete except for an issue with printing indices
     * the following will be needed for subsetting
     */
    PRINTVALSTREAM(rawoutstream, " Subsetting options:\n");
    PRINTVALSTREAM(rawoutstream,
                   "  --no-compact-subset  Disable compact form of subsetting and allow the use\n");
    PRINTVALSTREAM(rawoutstream, "                          of \"[\" in dataset names.\n");
    PRINTVALSTREAM(rawoutstream,
                   "  Subsetting is available by using the fcompact form of subsetting, as follows:\n");
    PRINTVALSTREAM(rawoutstream, "    obj1 /foo/mydataset[START;STRIDE;COUNT;BLOCK]\n");
    PRINTVALSTREAM(rawoutstream,
                   "  It is not required to use all parameters, but until the last parameter value used,\n");
    PRINTVALSTREAM(
        rawoutstream,
        "  all of the semicolons (;) are required, even when a parameter value is not specified. Example:\n");
    PRINTVALSTREAM(rawoutstream, "    obj1 /foo/mydataset[START;;COUNT;BLOCK]\n");
    PRINTVALSTREAM(rawoutstream, "    obj1 /foo/mydataset[START]\n");
    PRINTVALSTREAM(rawoutstream,
                   "  The STRIDE, COUNT, and BLOCK parameters are optional and will default to 1 in\n");
    PRINTVALSTREAM(rawoutstream,
                   "  each dimension. START is optional and will default to 0 in each dimension.\n");
    PRINTVALSTREAM(
        rawoutstream,
        "  Each of START, STRIDE, COUNT, and BLOCK must be a comma-separated list of integers with\n");
    PRINTVALSTREAM(rawoutstream, "  one integer for each dimension of the dataset.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, " Exit code:\n");
    PRINTVALSTREAM(rawoutstream, "  0 if no differences, 1 if differences found, 2 if error\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, " Examples of use:\n");
    PRINTVALSTREAM(rawoutstream, " 1) h5diff file1 file2 /g1/dset1 /g1/dset2\n");
    PRINTVALSTREAM(rawoutstream, "    Compares object '/g1/dset1' in file1 with '/g1/dset2' in file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, " 2) h5diff file1 file2 /g1/dset1\n");
    PRINTVALSTREAM(rawoutstream, "    Compares object '/g1/dset1' in both files\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, " 3) h5diff file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "    Compares all objects in both files\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, " Notes:\n");
    PRINTVALSTREAM(rawoutstream, "  file1 and file2 can be the same file.\n");
    PRINTVALSTREAM(rawoutstream, "  Use h5diff file1 file1 /g1/dset1 /g1/dset2 to compare\n");
    PRINTVALSTREAM(rawoutstream, "  '/g1/dset1' and '/g1/dset2' in the same file\n");
    PRINTVALSTREAM(rawoutstream, "\n");
}
