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

#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5repack.h"

/* Name of tool */
#define PROGRAMNAME "h5repack"

static int parse_command_line(int argc, const char **argv, pack_opt_t* options);
static void leave(int ret) H5_ATTR_NORETURN;


/* module-scoped variables */
static int has_i = 0;
static int has_o = 0;
const char *infile = NULL;
const char *outfile = NULL;

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char *s_opts = "hVvf:l:m:e:nLj:k:c:d:s:u:b:M:t:a:i:o:S:P:T:G:q:z:E";
static struct long_options l_opts[] = {
    { "help",                no_arg,      'h' },
    { "version",             no_arg,      'V' },
    { "verbose",             no_arg,      'v' },
    { "filter",              require_arg, 'f' },
    { "layout",              require_arg, 'l' },
    { "minimum",             require_arg, 'm' },
    { "file",                require_arg, 'e' },
    { "native",              no_arg,      'n' },
    { "latest",              no_arg,      'L' },
    { "low",                 require_arg, 'j' },
    { "high",                require_arg, 'k' },
    { "compact",             require_arg, 'c' },
    { "indexed",             require_arg, 'd' },
    { "ssize",               require_arg, 's' },
    { "ublock",              require_arg, 'u' },
    { "block",               require_arg, 'b' },
    { "metadata_block_size", require_arg, 'M' },
    { "threshold",           require_arg, 't' },
    { "alignment",           require_arg, 'a' },
    { "infile",              require_arg, 'i' }, /* for backward compability */
    { "outfile",             require_arg, 'o' }, /* for backward compability */
    { "fs_strategy",         require_arg, 'S' },
    { "fs_persist",          require_arg, 'P' },
    { "fs_threshold",        require_arg, 'T' },
    { "fs_pagesize",         require_arg, 'G' },
    { "sort_by",             require_arg, 'q' },
    { "sort_order",          require_arg, 'z' },
    { "enable-error-stack",  no_arg,      'E' },
    { NULL, 0, '\0' }
};

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print usage
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
static void usage(const char *prog) {
    FLUSHSTREAM(rawoutstream);
    PRINTSTREAM(rawoutstream, "usage: %s [OPTIONS] file1 file2\n", prog);
    PRINTVALSTREAM(rawoutstream, "  file1                    Input HDF5 File\n");
    PRINTVALSTREAM(rawoutstream, "  file2                    Output HDF5 File\n");
    PRINTVALSTREAM(rawoutstream, "  OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "   -h, --help              Print a usage message and exit\n");
    PRINTVALSTREAM(rawoutstream, "   -v, --verbose           Verbose mode, print object information\n");
    PRINTVALSTREAM(rawoutstream, "   -V, --version           Print version number and exit\n");
    PRINTVALSTREAM(rawoutstream, "   -n, --native            Use a native HDF5 type when repacking\n");
    PRINTVALSTREAM(rawoutstream, "   --enable-error-stack    Prints messages from the HDF5 error stack as they\n");
    PRINTVALSTREAM(rawoutstream, "                           occur\n");
    PRINTVALSTREAM(rawoutstream, "   -L, --latest            Use latest version of file format\n");
    PRINTVALSTREAM(rawoutstream, "                           This option will take precedence over the options\n");
    PRINTVALSTREAM(rawoutstream, "                           --low and --high\n");
    PRINTVALSTREAM(rawoutstream, "   --low=BOUND             The low bound for library release versions to use\n");
    PRINTVALSTREAM(rawoutstream, "                           when creating objects in the file\n");
    PRINTVALSTREAM(rawoutstream, "                           (default is H5F_LIBVER_EARLIEST)\n");
    PRINTVALSTREAM(rawoutstream, "   --high=BOUND            The high bound for library release versions to use\n");
    PRINTVALSTREAM(rawoutstream, "                           when creating objects in the file\n");
    PRINTVALSTREAM(rawoutstream, "                           (default is H5F_LIBVER_LATEST)\n");
    PRINTVALSTREAM(rawoutstream, "   -c L1, --compact=L1     Maximum number of links in header messages\n");
    PRINTVALSTREAM(rawoutstream, "   -d L2, --indexed=L2     Minimum number of links in the indexed format\n");
    PRINTVALSTREAM(rawoutstream, "   -s S[:F], --ssize=S[:F] Shared object header message minimum size\n");
    PRINTVALSTREAM(rawoutstream, "   -m M, --minimum=M       Do not apply the filter to datasets smaller than M\n");
    PRINTVALSTREAM(rawoutstream, "   -e E, --file=E          Name of file E with the -f and -l options\n");
    PRINTVALSTREAM(rawoutstream, "   -u U, --ublock=U        Name of file U with user block data to be added\n");
    PRINTVALSTREAM(rawoutstream, "   -b B, --block=B         Size of user block to be added\n");
    PRINTVALSTREAM(rawoutstream, "   -M A, --metadata_block_size=A  Metadata block size for H5Pset_meta_block_size\n");
    PRINTVALSTREAM(rawoutstream, "   -t T, --threshold=T     Threshold value for H5Pset_alignment\n");
    PRINTVALSTREAM(rawoutstream, "   -a A, --alignment=A     Alignment value for H5Pset_alignment\n");
    PRINTVALSTREAM(rawoutstream, "   -q Q, --sort_by=Q       Sort groups and attributes by index Q\n");
    PRINTVALSTREAM(rawoutstream, "   -z Z, --sort_order=Z    Sort groups and attributes by order Z\n");
    PRINTVALSTREAM(rawoutstream, "   -f FILT, --filter=FILT  Filter type\n");
    PRINTVALSTREAM(rawoutstream, "   -l LAYT, --layout=LAYT  Layout type\n");
    PRINTVALSTREAM(rawoutstream, "   -S FS_STRATEGY, --fs_strategy=FS_STRATEGY  File space management strategy for\n");
    PRINTVALSTREAM(rawoutstream, "                           H5Pset_file_space_strategy\n");
    PRINTVALSTREAM(rawoutstream, "   -P FS_PERSIST, --fs_persist=FS_PERSIST  Persisting or not persisting free-\n");
    PRINTVALSTREAM(rawoutstream, "                           space for H5Pset_file_space_strategy\n");
    PRINTVALSTREAM(rawoutstream, "   -T FS_THRESHOLD, --fs_threshold=FS_THRESHOLD   Free-space section threshold\n");
    PRINTVALSTREAM(rawoutstream, "                           for H5Pset_file_space_strategy\n");
    PRINTVALSTREAM(rawoutstream, "   -G FS_PAGESIZE, --fs_pagesize=FS_PAGESIZE   File space page size for\n");
    PRINTVALSTREAM(rawoutstream, "                           H5Pset_file_space_page_size\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    M - is an integer greater than 1, size of dataset in bytes (default is 0)\n");
    PRINTVALSTREAM(rawoutstream, "    E - is a filename.\n");
    PRINTVALSTREAM(rawoutstream, "    S - is an integer\n");
    PRINTVALSTREAM(rawoutstream, "    U - is a filename.\n");
    PRINTVALSTREAM(rawoutstream, "    T - is an integer\n");
    PRINTVALSTREAM(rawoutstream, "    A - is an integer greater than zero\n");
    PRINTVALSTREAM(rawoutstream, "    Q - is the sort index type for the input file. It can be \"name\" or\n");
    PRINTVALSTREAM(rawoutstream, "        \"creation_order\" (default)\n");
    PRINTVALSTREAM(rawoutstream, "    Z - is the sort order type for the input file. It can be \"descending\" or\n");
    PRINTVALSTREAM(rawoutstream, "        \"ascending\" (default)\n");
    PRINTVALSTREAM(rawoutstream, "    B - is the user block size, any value that is 512 or greater and is\n");
    PRINTVALSTREAM(rawoutstream, "        a power of 2 (1024 default)\n");
    PRINTVALSTREAM(rawoutstream, "    F - is the shared object header message type, any of <dspace|dtype|fill|\n");
    PRINTVALSTREAM(rawoutstream, "        pline|attr>. If F is not specified, S applies to all messages\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    BOUND is an integer indicating the library release versions to use when\n");
    PRINTVALSTREAM(rawoutstream, "          creating objects in the file (see H5Pset_libver_bounds()):\n");
    PRINTVALSTREAM(rawoutstream, "        0: This is H5F_LIBVER_EARLIEST in H5F_libver_t struct\n");
    PRINTVALSTREAM(rawoutstream, "        1: This is H5F_LIBVER_V18 in H5F_libver_t struct\n");
    PRINTVALSTREAM(rawoutstream, "        2: This is H5F_LIBVER_V110 in H5F_libver_t struct\n");
    PRINTVALSTREAM(rawoutstream, "           (H5F_LIBVER_LATEST is aliased to H5F_LIBVER_V110 for this release\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    FS_STRATEGY is a string indicating the file space strategy used:\n");
    PRINTVALSTREAM(rawoutstream, "        FSM_AGGR:\n");
    PRINTVALSTREAM(rawoutstream, "               The mechanisms used in managing file space are free-space\n");
    PRINTVALSTREAM(rawoutstream, "               managers, aggregators and virtual file driver.\n");
    PRINTVALSTREAM(rawoutstream, "        PAGE:\n");
    PRINTVALSTREAM(rawoutstream, "               The mechanisms used in managing file space are free-space\n");
    PRINTVALSTREAM(rawoutstream, "               managers with embedded paged aggregation and virtual file driver.\n");
    PRINTVALSTREAM(rawoutstream, "        AGGR:\n");
    PRINTVALSTREAM(rawoutstream, "               The mechanisms used in managing file space are aggregators and\n");
    PRINTVALSTREAM(rawoutstream, "               virtual file driver.\n");
    PRINTVALSTREAM(rawoutstream, "        NONE:\n");
    PRINTVALSTREAM(rawoutstream, "               The mechanisms used in managing file space are virtual file\n");
    PRINTVALSTREAM(rawoutstream, "               driver.\n");
    PRINTVALSTREAM(rawoutstream, "        The default strategy when not set is FSM_AGGR without persisting free-\n");
    PRINTVALSTREAM(rawoutstream, "        space.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    FS_PERSIST is 1 to persisting free-space or 0 to not persisting free-space.\n");
    PRINTVALSTREAM(rawoutstream, "      The default when not set is not persisting free-space.\n");
    PRINTVALSTREAM(rawoutstream, "      The value is ignored for AGGR and NONE strategies.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    FS_THRESHOLD is the minimum size (in bytes) of free-space sections to be\n");
    PRINTVALSTREAM(rawoutstream, "        tracked by the library.\n");
    PRINTVALSTREAM(rawoutstream, "      The default when not set is 1.\n");
    PRINTVALSTREAM(rawoutstream, "      The value is ignored for AGGR and NONE strategies.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    FS_PAGESIZE is the size (in bytes) >=512 that is used by the library when\n");
    PRINTVALSTREAM(rawoutstream, "        the file space strategy PAGE is used.\n");
    PRINTVALSTREAM(rawoutstream, "      The default when not set is 4096.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    FILT - is a string with the format:\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects>:<name of filter>=<filter parameters>\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects> is a comma separated list of object names, meaning apply\n");
    PRINTVALSTREAM(rawoutstream, "        compression only to those objects. If no names are specified, the filter\n");
    PRINTVALSTREAM(rawoutstream, "        is applied to all objects\n");
    PRINTVALSTREAM(rawoutstream, "      <name of filter> can be:\n");
    PRINTVALSTREAM(rawoutstream, "        GZIP, to apply the HDF5 GZIP filter (GZIP compression)\n");
    PRINTVALSTREAM(rawoutstream, "        SZIP, to apply the HDF5 SZIP filter (SZIP compression)\n");
    PRINTVALSTREAM(rawoutstream, "        SHUF, to apply the HDF5 shuffle filter\n");
    PRINTVALSTREAM(rawoutstream, "        FLET, to apply the HDF5 checksum filter\n");
    PRINTVALSTREAM(rawoutstream, "        NBIT, to apply the HDF5 NBIT filter (NBIT compression)\n");
    PRINTVALSTREAM(rawoutstream, "        SOFF, to apply the HDF5 Scale/Offset filter\n");
    PRINTVALSTREAM(rawoutstream, "        UD,   to apply a user defined filter\n");
    PRINTVALSTREAM(rawoutstream, "        NONE, to remove all filters\n");
    PRINTVALSTREAM(rawoutstream, "      <filter parameters> is optional filter parameter information\n");
    PRINTVALSTREAM(rawoutstream, "        GZIP=<deflation level> from 1-9\n");
    PRINTVALSTREAM(rawoutstream, "        SZIP=<pixels per block,coding> pixels per block is a even number in\n");
    PRINTVALSTREAM(rawoutstream, "            2-32 and coding method is either EC or NN\n");
    PRINTVALSTREAM(rawoutstream, "        SHUF (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        FLET (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        NBIT (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        SOFF=<scale_factor,scale_type> scale_factor is an integer and scale_type\n");
    PRINTVALSTREAM(rawoutstream, "            is either IN or DS\n");
    PRINTVALSTREAM(rawoutstream, "        UD=<filter_number,filter_flag,cd_value_count,value1[,value2,...,valueN]>\n");
    PRINTVALSTREAM(rawoutstream, "            Required values: filter_number, filter_flag, cd_value_count, value1\n");
    PRINTVALSTREAM(rawoutstream, "            Optional values: value2 to valueN\n");
    PRINTVALSTREAM(rawoutstream, "        NONE (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    LAYT - is a string with the format:\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects>:<layout type>=<layout parameters>\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects> is a comma separated list of object names, meaning that\n");
    PRINTVALSTREAM(rawoutstream, "        layout information is supplied for those objects. If no names are\n");
    PRINTVALSTREAM(rawoutstream, "        specified, the layout type is applied to all objects\n");
    PRINTVALSTREAM(rawoutstream, "      <layout type> can be:\n");
    PRINTVALSTREAM(rawoutstream, "        CHUNK, to apply chunking layout\n");
    PRINTVALSTREAM(rawoutstream, "        COMPA, to apply compact layout\n");
    PRINTVALSTREAM(rawoutstream, "        CONTI, to apply contiguous layout\n");
    PRINTVALSTREAM(rawoutstream, "      <layout parameters> is optional layout information\n");
    PRINTVALSTREAM(rawoutstream, "        CHUNK=DIM[xDIM...xDIM], the chunk size of each dimension\n");
    PRINTVALSTREAM(rawoutstream, "        COMPA (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        CONTI (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "Examples of use:\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "1) h5repack -v -f GZIP=1 file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   GZIP compression with level 1 to all objects\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "2) h5repack -v -f dset1:SZIP=8,NN file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   SZIP compression with 8 pixels per block and NN coding method to object dset1\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "3) h5repack -v -l dset1,dset2:CHUNK=20x10 -f dset3,dset4,dset5:NONE file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Chunked layout, with a layout size of 20x10, to objects dset1 and dset2\n");
    PRINTVALSTREAM(rawoutstream, "   and remove filters to objects dset3, dset4, dset5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "4) h5repack -L -c 10 -s 20:dtype file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Using latest file format with maximum compact group size of 10 and\n");
    PRINTVALSTREAM(rawoutstream, "   minimum shared datatype size of 20\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "5) h5repack -f SHUF -f GZIP=1 file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Add both filters SHUF and GZIP in this order to all datasets\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "6) h5repack -f UD=307,0,1,9 file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Add bzip2 filter to all datasets\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "7) h5repack --low=0 --high=1 file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   Set low=H5F_LIBVER_EARLIEST and high=H5F_LIBVER_V18 via\n");
    PRINTVALSTREAM(rawoutstream, "   H5Pset_libver_bounds() when creating the repacked file, file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
}

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown MPI & HDF5 and call exit()
 *
 * Return:      Does not return
 *-------------------------------------------------------------------------
 */
static void leave(int ret)
{
    h5tools_close();
    HDexit(ret);
}

/*-------------------------------------------------------------------------
 * Function: read_info
 *
 * Purpose: read comp and chunk options from a file
 *
 * Return: void, exit on error
 *-------------------------------------------------------------------------
 */
static
int read_info(const char *filename, pack_opt_t *options)
{
    char stype[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    char comp_info[1024];
    FILE *fp = NULL;
    char c;
    int i, rc = 1;
    int ret_value = EXIT_SUCCESS;

    if (NULL == (fp = HDfopen(filename, "r"))) {
        error_msg("cannot open options file %s\n", filename);
        h5tools_setstatus(EXIT_FAILURE);
        ret_value = EXIT_FAILURE;
        goto done;
    }

    /* cycle until end of file reached */
    while (1) {

        /* Info indicator must be for layout or filter */
        if (HDstrcmp(stype,"-l") && HDstrcmp(stype, "-f")) {
            error_msg("bad file format for %s", filename);
            h5tools_setstatus(EXIT_FAILURE);
            ret_value = EXIT_FAILURE;
            goto done;
        }

        /* find begining of info */
        i = 0;
        c = '0';
        while (c != ' ') {
            if (fscanf(fp, "%c", &c) < 0 && HDferror(fp)) {
                error_msg("fscanf error\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret_value = EXIT_FAILURE;
                goto done;
            }
            if (HDfeof(fp))
                break;
        }
        c = '0';
        /* go until end */
        while (c != ' ') {
            if (fscanf(fp, "%c", &c) < 0 && HDferror(fp)) {
                error_msg("fscanf error\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret_value = EXIT_FAILURE;
                goto done;
            }
            comp_info[i++] = c;
            if (HDfeof(fp))
                break;
            if (c == 10 /*eol*/)
                break;
        }
        comp_info[i - 1] = '\0'; /*cut the last " */

        if (!HDstrcmp(stype, "-l")) {
            if (h5repack_addlayout(comp_info, options) == -1) {
                error_msg("could not add chunck option\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret_value = EXIT_FAILURE;
                goto done;
            }
        }
        else {
            if (h5repack_addfilter(comp_info, options) == -1) {
                error_msg("could not add compression option\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret_value = EXIT_FAILURE;
                goto done;
            }
        }
    } /* end while info-read cycling */

done:
    if (fp)
        HDfclose(fp);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    set_sort_by
 *
 * Purpose: set the "by" form of sorting by translating from a string input
 *          parameter to a H5_index_t return value
 *          current sort values are [creation_order | name]
 *
 * Return: H5_index_t form of sort or H5_INDEX_UNKNOWN if none found
 *-------------------------------------------------------------------------
 */
static H5_index_t
set_sort_by(const char *form)
{
    H5_index_t idx_type = H5_INDEX_UNKNOWN;

    if (!HDstrcmp(form, "name"))
        idx_type = H5_INDEX_NAME;
    else if (!HDstrcmp(form, "creation_order"))
        idx_type = H5_INDEX_CRT_ORDER;

    return idx_type;
}

/*-------------------------------------------------------------------------
 * Function:    set_sort_order
 *
 * Purpose: set the order of sorting by translating from a string input
 *          parameter to a H5_iter_order_t return value
 *          current order values are [ascending | descending ]
 *
 * Return: H5_iter_order_t form of order or H5_ITER_UNKNOWN if none found
 *-------------------------------------------------------------------------
 */
static H5_iter_order_t
set_sort_order(const char *form)
{
    H5_iter_order_t iter_order = H5_ITER_UNKNOWN;

    if (!HDstrcmp(form, "ascending"))
        iter_order = H5_ITER_INC;
    else if (!HDstrcmp(form, "descending"))
        iter_order = H5_ITER_DEC;

    return iter_order;
}

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *-------------------------------------------------------------------------
 */
static
int parse_command_line(int argc, const char **argv, pack_opt_t* options)
{
    int opt;
    int ret_value = 0;

    /* parse command line options */
    while (EOF != (opt = get_option(argc, argv, s_opts, l_opts))) {
        switch ((char) opt) {

            /* -i for backward compatibility */
            case 'i':
                infile = opt_arg;
                has_i++;
                break;

            /* -o for backward compatibility */
            case 'o':
                outfile = opt_arg;
                has_o++;
                break;

            case 'h':
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                ret_value = -1;
                goto done;

            case 'V':
                print_version(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                ret_value = -1;
                goto done;

            case 'v':
                options->verbose = 1;
                break;

            case 'f':
                /* parse the -f filter option */
                if (h5repack_addfilter(opt_arg, options) < 0) {
                    error_msg("in parsing filter\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'l':
                /* parse the -l layout option */
                if (h5repack_addlayout(opt_arg, options) < 0) {
                    error_msg("in parsing layout\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'm':
                options->min_comp = HDstrtoull(opt_arg , NULL, 0);
                if ((int) options->min_comp <= 0) {
                    error_msg("invalid minimum compress size <%s>\n", opt_arg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'e':
                ret_value = read_info(opt_arg, options);
                if (ret_value < 0)
                    goto done;
                break;

            case 'n':
                options->use_native = 1;
                break;

            case 'L':
                options->latest = TRUE;
                break;

            case 'j':
                options->low_bound = (H5F_libver_t)HDatoi(opt_arg);
                if (options->low_bound < H5F_LIBVER_EARLIEST || options->low_bound > H5F_LIBVER_LATEST) {
                    error_msg("in parsing low bound\n");
                    goto done;
                }
                break;

            case 'k':
                options->high_bound = (H5F_libver_t)HDatoi(opt_arg);
                if (options->high_bound < H5F_LIBVER_EARLIEST || options->high_bound > H5F_LIBVER_LATEST) {
                    error_msg("in parsing high bound\n");
                    goto done;
                }
                break;

            case 'c':
                options->grp_compact = HDatoi( opt_arg );
                if (options->grp_compact > 0)
                    options->latest = TRUE; /* must use latest format */
                break;

            case 'd':
                options->grp_indexed = HDatoi( opt_arg );
                if (options->grp_indexed > 0)
                    options->latest = TRUE; /* must use latest format */
                break;

            case 's':
                {
                    int idx = 0;
                    int ssize = 0;
                    char *msgPtr = HDstrchr(opt_arg, ':');
                    options->latest = TRUE; /* must use latest format */
                    if (msgPtr == NULL) {
                        ssize = HDatoi(opt_arg);
                        for (idx = 0; idx < 5; idx++)
                            options->msg_size[idx] = ssize;
                    }
                    else {
                        char msgType[10];

                        HDstrcpy(msgType, msgPtr + 1);
                        msgPtr[0] = '\0';
                        ssize = HDatoi( opt_arg );
                        if (!HDstrncmp(msgType, "dspace", 6))
                            options->msg_size[0] = ssize;
                        else if (!HDstrncmp(msgType, "dtype", 5))
                            options->msg_size[1] = ssize;
                        else if (!HDstrncmp(msgType, "fill", 4))
                            options->msg_size[2] = ssize;
                        else if (!HDstrncmp(msgType, "pline", 5))
                            options->msg_size[3] = ssize;
                        else if (!HDstrncmp(msgType, "attr", 4))
                            options->msg_size[4] = ssize;
                    }
                }
                break;

            case 'u':
                options->ublock_filename = opt_arg;
                break;

            case 'b':
                options->ublock_size = (hsize_t) HDatol( opt_arg );
                break;

            case 'M':
                options->meta_block_size = (hsize_t) HDatol( opt_arg );
                break;

            case 't':
                options->threshold = (hsize_t) HDatol( opt_arg );
                break;

            case 'a':
                options->alignment = HDstrtoull(opt_arg , NULL, 0);
                if (options->alignment < 1) {
                    error_msg("invalid alignment size\n", opt_arg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'S':
                {
                    char strategy[MAX_NC_NAME];

                    HDstrcpy(strategy, opt_arg);
                    if (!HDstrcmp(strategy, "FSM_AGGR"))
                        options->fs_strategy = H5F_FSPACE_STRATEGY_FSM_AGGR;
                    else if (!HDstrcmp(strategy, "PAGE"))
                        options->fs_strategy = H5F_FSPACE_STRATEGY_PAGE;
                    else if (!HDstrcmp(strategy, "AGGR"))
                        options->fs_strategy = H5F_FSPACE_STRATEGY_AGGR;
                    else if (!HDstrcmp(strategy, "NONE"))
                        options->fs_strategy = H5F_FSPACE_STRATEGY_NONE;
                    else {
                        error_msg("invalid file space management strategy\n", opt_arg);
                        h5tools_setstatus(EXIT_FAILURE);
                        ret_value = -1;
                        goto done;
                    }
                    if (options->fs_strategy == (H5F_fspace_strategy_t)0)
                        /* To distinguish the "specified" zero value */
                        options->fs_strategy = (H5F_fspace_strategy_t)-1;
                }
                break;

            case 'P':
                options->fs_persist = HDatoi(opt_arg);
                if (options->fs_persist == 0)
                    /* To distinguish the "specified" zero value */
                    options->fs_persist = -1;
                break;

            case 'T':
                options->fs_threshold = HDatol(opt_arg);
                if (options->fs_threshold == 0)
                    /* To distinguish the "specified" zero value */
                    options->fs_threshold = -1;
                break;

            case 'G':
                options->fs_pagesize = HDstrtoll(opt_arg, NULL, 0);
                if (options->fs_pagesize == 0)
                    /* To distinguish the "specified" zero value */
                   options->fs_pagesize = -1;
                break;

            case 'q':
                if (H5_INDEX_UNKNOWN == set_sort_by(opt_arg)) {
                    error_msg(" failed to set sort by form <%s>\n", opt_arg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'z':
                if (set_sort_order(opt_arg) == H5_ITER_UNKNOWN) {
                    error_msg(" failed to set sort order form <%s>\n", opt_arg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'E':
                enable_error_stack = 1;
                break;

            default:
                break;
        } /* end switch */
    } /* end while there are more options to parse */

    /* If neither -i nor -o given, get in and out files positionally */
    if (0 == (has_i + has_o)) {
         if (argv[opt_ind] != NULL && argv[opt_ind + 1] != NULL) {
             infile = argv[opt_ind];
             outfile = argv[opt_ind + 1];

             if (!HDstrcmp(infile, outfile)) {
                 error_msg("file names cannot be the same\n");
                 usage(h5tools_getprogname());
                 h5tools_setstatus(EXIT_FAILURE);
                 ret_value = -1;
             }
         }
         else {
             error_msg("file names missing\n");
             usage(h5tools_getprogname());
             h5tools_setstatus(EXIT_FAILURE);
             ret_value = -1;
         }
     }
     else if (has_i != 1 || has_o != 1) {
         error_msg("filenames must be either both -i -o or both positional\n");
         usage(h5tools_getprogname());
         h5tools_setstatus(EXIT_FAILURE);
         ret_value = -1;
     }

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h5repack main program
 *
 * Return: Success: EXIT_SUCCESS(0)
 *
 * Failure: EXIT_FAILURE(1)
 *-------------------------------------------------------------------------
 */
int main(int argc, const char **argv)
{
    pack_opt_t options; /*the global options */
    H5E_auto2_t         func;
    H5E_auto2_t         tools_func;
    void               *edata;
    void               *tools_edata;

    HDmemset(&options, 0, sizeof(pack_opt_t));

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Disable error reporting */
    H5Eget_auto2(H5E_DEFAULT, &func, &edata);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Disable tools error reporting */
    H5Eget_auto2(H5tools_ERR_STACK_g, &tools_func, &tools_edata);
    H5Eset_auto2(H5tools_ERR_STACK_g, NULL, NULL);

    /* update hyperslab buffer size from H5TOOLS_BUFSIZE env if exist */
    if (h5tools_getenv_update_hyperslab_bufsize() < 0) {
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* initialize options  */
    if (h5repack_init(&options, 0, FALSE) < 0) {
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }
    /* Initialize default indexing options */
    sort_by = H5_INDEX_CRT_ORDER;

    if (parse_command_line(argc, argv, &options) < 0)
        goto done;

    if (enable_error_stack > 0) {
        H5Eset_auto2(H5E_DEFAULT, func, edata);
        H5Eset_auto2(H5tools_ERR_STACK_g, tools_func, tools_edata);
    }

    /* pack it */
    h5tools_setstatus(h5repack(infile, outfile, &options));

done:
    /* free tables */
    h5repack_end(&options);

    leave(h5tools_getstatus());
}

