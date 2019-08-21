/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"
#include "h5diff.h"
#include "h5diff_common.h"
#include "h5tools.h"
#include "h5tools_utils.h"

static int check_n_input(const char*);
static int check_p_input(const char*);
static int check_d_input(const char*);

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char *s_opts = "hVrv:qn:d:p:NcelxE:S";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "version", no_arg, 'V' },
    { "report", no_arg, 'r' },
    { "verbose", optional_arg, 'v' },
    { "quiet", no_arg, 'q' },
    { "count", require_arg, 'n' },
    { "delta", require_arg, 'd' },
    { "relative", require_arg, 'p' },
    { "nan", no_arg, 'N' },
    { "compare", no_arg, 'c' },
    { "use-system-epsilon", no_arg, 'e' },
    { "follow-symlinks", no_arg, 'l' },
    { "no-dangling-links", no_arg, 'x' },
    { "exclude-path", require_arg, 'E' },
    { "enable-error-stack", no_arg, 'S' },
    { NULL, 0, '\0' }
};

/*-------------------------------------------------------------------------
 * Function: check_options
 *
 * Purpose: parse command line input
 *
 *-------------------------------------------------------------------------
 */
static void check_options(diff_opt_t* opts)
{
    /*--------------------------------------------------------------
     * check for mutually exclusive options
     *--------------------------------------------------------------*/

    /* check between -d , -p, --use-system-epsilon.
     * These options are mutually exclusive.
     */
    if ((opts->d + opts->p + opts->use_system_epsilon) > 1) {
        HDprintf("%s error: -d, -p and --use-system-epsilon options are mutually-exclusive;\n", PROGRAMNAME);
        HDprintf("use no more than one.\n");
        HDprintf("Try '-h' or '--help' option for more information or see the %s entry in the 'HDF5 Reference Manual'.\n", PROGRAMNAME);
        h5diff_exit(EXIT_FAILURE);
    }
}


/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *
 *-------------------------------------------------------------------------
 */

void parse_command_line(int argc,
                        const char* argv[],
                        const char** fname1,
                        const char** fname2,
                        const char** objname1,
                        const char** objname2,
                        diff_opt_t* opts)
{
    int i;
    int opt;
    struct exclude_path_list *exclude_head, *exclude_prev, *exclude_node;

    /* process the command-line */
    memset(opts, 0, sizeof (diff_opt_t));

    /* assume equal contents initially */
    opts->contents = 1;

    /* NaNs are handled by default */
    opts->do_nans = 1;

    /* not Listing objects that are not comparable */
    opts->m_list_not_cmp = 0;

    /* initially no not-comparable. */
    /**this is bad in mixing option with results**/
    opts->not_cmp=0;

    /* init for exclude-path option */
    exclude_head = NULL;

    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
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
            opts->m_verbose = 1;
            /* This for loop is for handling style like
             * -v, -v1, --verbose, --verbose=1.
             */
            for (i = 1; i < argc; i++) {
                /*
                 * short opt
                 */
                if (!strcmp (argv[i], "-v")) {  /* no arg */
                    opt_ind--;
                    opts->m_verbose_level = 0;
                    break;
                }
                else if (!strncmp (argv[i], "-v", (size_t)2)) {
                    opts->m_verbose_level = atoi(&argv[i][2]);
                    break;
                }

                /*
                 * long opt
                 */
                if (!strcmp (argv[i], "--verbose")) {  /* no arg */
                    opts->m_verbose_level = 0;
                    break;
                }
                else if ( !strncmp (argv[i], "--verbose", (size_t)9) && argv[i][9]=='=') {
                    opts->m_verbose_level = atoi(&argv[i][10]);
                    break;
                }
            }
            break;

        case 'q':
            /* use quiet mode; supress the message "0 differences found" */
            opts->m_quiet = 1;
            break;

        case 'r':
            opts->m_report = 1;
            break;

        case 'l':
            opts->follow_links = TRUE;
            break;

        case 'x':
            opts->no_dangle_links = 1;
            break;

        case 'S':
            enable_error_stack = 1;
            break;

        case 'E':
            opts->exclude_path = 1;

            /* create linked list of excluding objects */
            if( (exclude_node = (struct exclude_path_list*) HDmalloc(sizeof(struct exclude_path_list))) == NULL) {
                HDprintf("Error: lack of memory!\n");
                h5diff_exit(EXIT_FAILURE);
            }

            /* init */
            exclude_node->obj_path = (char*)opt_arg;
            exclude_node->obj_type = H5TRAV_TYPE_UNKNOWN;
            exclude_prev = exclude_head;

            if (NULL == exclude_head) {
                exclude_head = exclude_node;
                exclude_head->next = NULL;
            }
            else {
                while(NULL != exclude_prev->next)
                    exclude_prev=exclude_prev->next;

                exclude_node->next = NULL;
                exclude_prev->next = exclude_node;
            }
            break;

        case 'd':
            opts->d=1;

            if (check_d_input(opt_arg) == - 1) {
                HDprintf("<-d %s> is not a valid option\n", opt_arg);
                usage();
                h5diff_exit(EXIT_FAILURE);
            }
            opts->delta = atof(opt_arg);

            /* -d 0 is the same as default */
            if (H5_DBL_ABS_EQUAL(opts->delta, (double)0.0F))
                opts->d=0;
            break;

        case 'p':
            opts->p=1;
            if (check_p_input(opt_arg) == -1) {
                HDprintf("<-p %s> is not a valid option\n", opt_arg);
                usage();
                h5diff_exit(EXIT_FAILURE);
            }
            opts->percent = atof(opt_arg);

            /* -p 0 is the same as default */
            if (H5_DBL_ABS_EQUAL(opts->percent, (double)0.0F))
                opts->p = 0;
            break;

        case 'n':
            opts->n=1;
            if ( check_n_input(opt_arg) == -1) {
                HDprintf("<-n %s> is not a valid option\n", opt_arg);
                usage();
                h5diff_exit(EXIT_FAILURE);
            }
            opts->count = HDstrtoull(opt_arg, NULL, 0);
            break;

        case 'N':
            opts->do_nans = 0;
            break;

        case 'c':
            opts->m_list_not_cmp = 1;
            break;

        case 'e':
            opts->use_system_epsilon = 1;
            break;
        }
    }

    /* check options */
    check_options(opts);

    /* if exclude-path option is used, keep the exclude path list */
    if (opts->exclude_path)
        opts->exclude = exclude_head;

    /* check for file names to be processed */
    if (argc <= opt_ind || argv[ opt_ind + 1 ] == NULL) {
        error_msg("missing file names\n");
        usage();
        h5diff_exit(EXIT_FAILURE);
    }

    *fname1 = argv[ opt_ind ];
    *fname2 = argv[ opt_ind + 1 ];
    *objname1 = argv[ opt_ind + 2 ];

    if (*objname1 == NULL) {
        *objname2 = NULL;
        return;
    }

    if (argv[ opt_ind + 3 ] != NULL) {
        *objname2 = argv[ opt_ind + 3 ];
    }
    else {
        *objname2 = *objname1;
    }


}


/*-------------------------------------------------------------------------
 * Function: print_info
 *
 * Purpose: print several information messages after h5diff call
 *
 *-------------------------------------------------------------------------
 */

 void  print_info(diff_opt_t* opts)
 {
     if (opts->m_quiet || opts->err_stat)
         return;

     if (opts->cmn_objs == 0) {
         HDprintf("No common objects found. Files are not comparable.\n");
         if (!opts->m_verbose)
             HDprintf("Use -v for a list of objects.\n");
     }

     if (opts->not_cmp == 1) {
         if (opts->m_list_not_cmp == 0) {
             HDprintf("--------------------------------\n");
             HDprintf("Some objects are not comparable\n");
             HDprintf("--------------------------------\n");
             if (opts->m_verbose)
                 HDprintf("Use -c for a list of objects without details of differences.\n");
             else
                 HDprintf("Use -c for a list of objects.\n");
         }
     }
 }

/*-------------------------------------------------------------------------
 * Function: check_n_input
 *
 * Purpose: check for valid input
 *
 * Return: 1 for ok, -1 for fail
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE static int
check_n_input( const char *str )
{
    unsigned i;
    char c;

    for (i = 0; i < HDstrlen(str); i++) {
        c = str[i];
        if (i == 0) {
            if (c < 49 || c > 57) /* ascii values between 1 and 9 */
                return -1;
        }
        else
            if (c < 48 || c > 57) /* 0 also */
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
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
check_p_input( const char *str )
{
    double x;

    /*
    the atof return value on a hexadecimal input is different
    on some systems; we do a character check for this
    */
    if (HDstrlen(str) > 2 && str[0] == '0' && str[1] == 'x')
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
 *
 * Date: November 11, 2007
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
check_d_input( const char *str )
{
    double x;

    /*
    the atof return value on a hexadecimal input is different
    on some systems; we do a character check for this
    */
    if (HDstrlen(str) > 2 && str[0] == '0' && str[1] == 'x')
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
 *
 *-------------------------------------------------------------------------
 */

void usage(void)
{
 PRINTVALSTREAM(rawoutstream, "usage: h5diff [OPTIONS] file1 file2 [obj1[ obj2]]\n");
 PRINTVALSTREAM(rawoutstream, "  file1             File name of the first HDF5 file\n");
 PRINTVALSTREAM(rawoutstream, "  file2             File name of the second HDF5 file\n");
 PRINTVALSTREAM(rawoutstream, "  [obj1]            Name of an HDF5 object, in absolute path\n");
 PRINTVALSTREAM(rawoutstream, "  [obj2]            Name of an HDF5 object, in absolute path\n");
 PRINTVALSTREAM(rawoutstream, "\n");
 PRINTVALSTREAM(rawoutstream, "  OPTIONS\n");
 PRINTVALSTREAM(rawoutstream, "   -h, --help\n");
 PRINTVALSTREAM(rawoutstream, "         Print a usage message and exit.\n");
 PRINTVALSTREAM(rawoutstream, "   -V, --version\n");
 PRINTVALSTREAM(rawoutstream, "         Print version number and exit.\n");
 PRINTVALSTREAM(rawoutstream, "   -r, --report\n");
 PRINTVALSTREAM(rawoutstream, "         Report mode. Print differences.\n");
 PRINTVALSTREAM(rawoutstream, "   -v --verbose\n");
 PRINTVALSTREAM(rawoutstream, "         Verbose mode. Print differences information and list of objects.\n");
 PRINTVALSTREAM(rawoutstream, "   -vN --verbose=N\n");
 PRINTVALSTREAM(rawoutstream, "         Verbose mode with level. Print differences and list of objects.\n");
 PRINTVALSTREAM(rawoutstream, "         Level of detail depends on value of N:\n");
 PRINTVALSTREAM(rawoutstream, "          0 : Identical to '-v' or '--verbose'.\n");
 PRINTVALSTREAM(rawoutstream, "          1 : All level 0 information plus one-line attribute\n");
 PRINTVALSTREAM(rawoutstream, "              status summary.\n");
 PRINTVALSTREAM(rawoutstream, "          2 : All level 1 information plus extended attribute\n");
 PRINTVALSTREAM(rawoutstream, "              status report.\n");
 PRINTVALSTREAM(rawoutstream, "   -q, --quiet\n");
 PRINTVALSTREAM(rawoutstream, "         Quiet mode. Do not produce output.\n");
 PRINTVALSTREAM(rawoutstream, "   --enable-error-stack\n");
 PRINTVALSTREAM(rawoutstream, "                   Prints messages from the HDF5 error stack as they occur.\n");
 PRINTVALSTREAM(rawoutstream, "   --follow-symlinks\n");
 PRINTVALSTREAM(rawoutstream, "         Follow symbolic links (soft links and external links and compare the)\n");
 PRINTVALSTREAM(rawoutstream, "         links' target objects.\n");
 PRINTVALSTREAM(rawoutstream, "         If symbolic link(s) with the same name exist in the files being\n");
 PRINTVALSTREAM(rawoutstream, "         compared, then determine whether the target of each link is an existing\n");
 PRINTVALSTREAM(rawoutstream, "         object (dataset, group, or named datatype) or the link is a dangling\n");
 PRINTVALSTREAM(rawoutstream, "         link (a soft or external link pointing to a target object that does\n");
 PRINTVALSTREAM(rawoutstream, "         not yet exist).\n");
 PRINTVALSTREAM(rawoutstream, "         - If both symbolic links are dangling links, they are treated as being\n");
 PRINTVALSTREAM(rawoutstream, "           the same; by default, h5diff returns an exit code of 0.\n");
 PRINTVALSTREAM(rawoutstream, "           If, however, --no-dangling-links is used with --follow-symlinks,\n");
 PRINTVALSTREAM(rawoutstream, "           this situation is treated as an error and h5diff returns an\n");
 PRINTVALSTREAM(rawoutstream, "           exit code of 2.\n");
 PRINTVALSTREAM(rawoutstream, "         - If only one of the two links is a dangling link,they are treated as\n");
 PRINTVALSTREAM(rawoutstream, "           being different and h5diff returns an exit code of 1.\n");
 PRINTVALSTREAM(rawoutstream, "           If, however, --no-dangling-links is used with --follow-symlinks,\n");
 PRINTVALSTREAM(rawoutstream, "           this situation is treated as an error and h5diff returns an\n");
 PRINTVALSTREAM(rawoutstream, "           exit code of 2.\n");
 PRINTVALSTREAM(rawoutstream, "         - If both symbolic links point to existing objects, h5diff compares the\n");
 PRINTVALSTREAM(rawoutstream, "           two objects.\n");
 PRINTVALSTREAM(rawoutstream, "         If any symbolic link specified in the call to h5diff does not exist,\n");
 PRINTVALSTREAM(rawoutstream, "         h5diff treats it as an error and returns an exit code of 2.\n");
 PRINTVALSTREAM(rawoutstream, "   --no-dangling-links\n");
 PRINTVALSTREAM(rawoutstream, "         Must be used with --follow-symlinks option; otherwise, h5diff shows\n");
 PRINTVALSTREAM(rawoutstream, "         error message and returns an exit code of 2.\n");
 PRINTVALSTREAM(rawoutstream, "         Check for any symbolic links (soft links or external links) that do not\n");
 PRINTVALSTREAM(rawoutstream, "         resolve to an existing object (dataset, group, or named datatype).\n");
 PRINTVALSTREAM(rawoutstream, "         If any dangling link is found, this situation is treated as an error\n");
 PRINTVALSTREAM(rawoutstream, "         and h5diff returns an exit code of 2.\n");
 PRINTVALSTREAM(rawoutstream, "   -c, --compare\n");
 PRINTVALSTREAM(rawoutstream, "         List objects that are not comparable\n");
 PRINTVALSTREAM(rawoutstream, "   -N, --nan\n");
 PRINTVALSTREAM(rawoutstream, "         Avoid NaNs detection\n");
 PRINTVALSTREAM(rawoutstream, "   -n C, --count=C\n");
 PRINTVALSTREAM(rawoutstream, "         Print differences up to C. C must be a positive integer.\n");
 PRINTVALSTREAM(rawoutstream, "   -d D, --delta=D\n");
 PRINTVALSTREAM(rawoutstream, "         Print difference if (|a-b| > D). D must be a positive number. Where a\n");
 PRINTVALSTREAM(rawoutstream, "         is the data point value in file1 and b is the data point value in file2.\n");
 PRINTVALSTREAM(rawoutstream, "         Can not use with '-p' or '--use-system-epsilon'.\n");
 PRINTVALSTREAM(rawoutstream, "   -p R, --relative=R\n");
 PRINTVALSTREAM(rawoutstream, "         Print difference if (|(a-b)/b| > R). R must be a positive number. Where a\n");
 PRINTVALSTREAM(rawoutstream, "         is the data point value in file1 and b is the data point value in file2.\n");
 PRINTVALSTREAM(rawoutstream, "         Can not use with '-d' or '--use-system-epsilon'.\n");
 PRINTVALSTREAM(rawoutstream, "   --use-system-epsilon\n");
 PRINTVALSTREAM(rawoutstream, "         Print difference if (|a-b| > EPSILON), EPSILON is system defined value. Where a\n");
 PRINTVALSTREAM(rawoutstream, "         is the data point value in file1 and b is the data point value in file2.\n");
 PRINTVALSTREAM(rawoutstream, "         If the system epsilon is not defined,one of the following predefined\n");
 PRINTVALSTREAM(rawoutstream, "         values will be used:\n");
 PRINTVALSTREAM(rawoutstream, "           FLT_EPSILON = 1.19209E-07 for floating-point type\n");
 PRINTVALSTREAM(rawoutstream, "           DBL_EPSILON = 2.22045E-16 for double precision type\n");
 PRINTVALSTREAM(rawoutstream, "         Can not use with '-p' or '-d'.\n");
 PRINTVALSTREAM(rawoutstream, "   --exclude-path \"path\"\n");
 PRINTVALSTREAM(rawoutstream, "         Exclude the specified path to an object when comparing files or groups.\n");
 PRINTVALSTREAM(rawoutstream, "         If a group is excluded, all member objects will also be excluded.\n");
 PRINTVALSTREAM(rawoutstream, "         The specified path is excluded wherever it occurs.\n");
 PRINTVALSTREAM(rawoutstream, "         This flexibility enables the same option to exclude either objects that\n");
 PRINTVALSTREAM(rawoutstream, "         exist only in one file or common objects that are known to differ.\n");
 PRINTVALSTREAM(rawoutstream, "\n");
 PRINTVALSTREAM(rawoutstream, "         When comparing files, \"path\" is the absolute path to the excluded;\n");
 PRINTVALSTREAM(rawoutstream, "         object; when comparing groups, \"path\" is similar to the relative\n");
 PRINTVALSTREAM(rawoutstream, "         path from the group to the excluded object. This \"path\" can be\n");
 PRINTVALSTREAM(rawoutstream, "         taken from the first section of the output of the --verbose option.\n");
 PRINTVALSTREAM(rawoutstream, "         For example, if you are comparing the group /groupA in two files and\n");
 PRINTVALSTREAM(rawoutstream, "         you want to exclude /groupA/groupB/groupC in both files, the exclude\n");
 PRINTVALSTREAM(rawoutstream, "         option would read as follows:\n");
 PRINTVALSTREAM(rawoutstream, "           --exclude-path \"/groupB/groupC\"\n");
 PRINTVALSTREAM(rawoutstream, "\n");
 PRINTVALSTREAM(rawoutstream, "         If there are multiple paths to an object, only the specified path(s)\n");
 PRINTVALSTREAM(rawoutstream, "         will be excluded; the comparison will include any path not explicitly\n");
 PRINTVALSTREAM(rawoutstream, "         excluded.\n");
 PRINTVALSTREAM(rawoutstream, "         This option can be used repeatedly to exclude multiple paths.\n");
 PRINTVALSTREAM(rawoutstream, "\n");

 PRINTVALSTREAM(rawoutstream, " Modes of output:\n");
 PRINTVALSTREAM(rawoutstream, "  Default mode: print the number of differences found and where they occured\n");
 PRINTVALSTREAM(rawoutstream, "  -r Report mode: print the above plus the differences\n");
 PRINTVALSTREAM(rawoutstream, "  -v Verbose mode: print the above plus a list of objects and warnings\n");
 PRINTVALSTREAM(rawoutstream, "  -q Quiet mode: do not print output\n");

 PRINTVALSTREAM(rawoutstream, "\n");

 PRINTVALSTREAM(rawoutstream, " File comparison:\n");
 PRINTVALSTREAM(rawoutstream, "  If no objects [obj1[ obj2]] are specified, the h5diff comparison proceeds as\n");
 PRINTVALSTREAM(rawoutstream, "  a comparison of the two files' root groups.  That is, h5diff first compares\n");
 PRINTVALSTREAM(rawoutstream, "  the names of root group members, generates a report of root group objects\n");
 PRINTVALSTREAM(rawoutstream, "  that appear in only one file or in both files, and recursively compares\n");
 PRINTVALSTREAM(rawoutstream, "  common objects.\n");
 PRINTVALSTREAM(rawoutstream, "\n");

 PRINTVALSTREAM(rawoutstream, " Object comparison:\n");
 PRINTVALSTREAM(rawoutstream, "  1) Groups\n");
 PRINTVALSTREAM(rawoutstream, "      First compares the names of member objects (relative path, from the\n");
 PRINTVALSTREAM(rawoutstream, "      specified group) and generates a report of objects that appear in only\n");
 PRINTVALSTREAM(rawoutstream, "      one group or in both groups. Common objects are then compared recursively.\n");
 PRINTVALSTREAM(rawoutstream, "  2) Datasets\n");
 PRINTVALSTREAM(rawoutstream, "      Array rank and dimensions, datatypes, and data values are compared.\n");
 PRINTVALSTREAM(rawoutstream, "  3) Datatypes\n");
 PRINTVALSTREAM(rawoutstream, "      The comparison is based on the return value of H5Tequal.\n");
 PRINTVALSTREAM(rawoutstream, "  4) Symbolic links\n");
 PRINTVALSTREAM(rawoutstream, "      The paths to the target objects are compared.\n");
 PRINTVALSTREAM(rawoutstream, "      (The option --follow-symlinks overrides the default behavior when\n");
 PRINTVALSTREAM(rawoutstream, "       symbolic links are compared.).\n");
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
