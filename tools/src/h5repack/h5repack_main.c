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
#include "h5tools_utils.h"
#include "h5repack.h"

/* Name of tool */
#define PROGRAMNAME "h5repack"

static int  parse_command_line(int argc, const char *const *argv, pack_opt_t *options);
static void leave(int ret) H5_ATTR_NORETURN;

/* module-scoped variables */
static int         has_i   = 0;
static int         has_o   = 0;
static const char *infile  = NULL;
static const char *outfile = NULL;

/*
 * Command-line options: The user can specify short or long-named
 * parameters.
 */
static const char *s_opts = "a:b:c:d:e:f:hi:j:k:l:m:no:q:s:t:u:v*z:E*G:LM:P:S:T:VXWY:Z:1:2:3:4:5:6:7:8:9:0:";
static struct h5_long_options l_opts[] = {{"alignment", require_arg, 'a'},
                                          {"block", require_arg, 'b'},
                                          {"compact", require_arg, 'c'},
                                          {"indexed", require_arg, 'd'},
                                          {"file", require_arg, 'e'},
                                          {"filter", require_arg, 'f'},
                                          {"help", no_arg, 'h'},
                                          {"infile", require_arg, 'i'}, /* for backward compatibility */
                                          {"low", require_arg, 'j'},
                                          {"high", require_arg, 'k'},
                                          {"layout", require_arg, 'l'},
                                          {"minimum", require_arg, 'm'},
                                          {"native", no_arg, 'n'},
                                          {"outfile", require_arg, 'o'}, /* for backward compatibility */
                                          {"sort_by", require_arg, 'q'},
                                          {"ssize", require_arg, 's'},
                                          {"threshold", require_arg, 't'},
                                          {"ublock", require_arg, 'u'},
                                          {"verbose", optional_arg, 'v'},
                                          {"sort_order", require_arg, 'z'},
                                          {"enable-error-stack", optional_arg, 'E'},
                                          {"fs_pagesize", require_arg, 'G'},
                                          {"latest", no_arg, 'L'},
                                          {"metadata_block_size", require_arg, 'M'},
                                          {"fs_persist", require_arg, 'P'},
                                          {"fs_strategy", require_arg, 'S'},
                                          {"fs_threshold", require_arg, 'T'},
                                          {"version", no_arg, 'V'},
                                          {"merge", no_arg, 'X'},
                                          {"prune", no_arg, 'W'},
                                          {"src-vol-value", require_arg, '1'},
                                          {"src-vol-name", require_arg, '2'},
                                          {"src-vol-info", require_arg, '3'},
                                          {"dst-vol-value", require_arg, '4'},
                                          {"dst-vol-name", require_arg, '5'},
                                          {"dst-vol-info", require_arg, '6'},
                                          {"src-vfd-value", require_arg, '7'},
                                          {"src-vfd-name", require_arg, '8'},
                                          {"src-vfd-info", require_arg, '9'},
                                          {"dst-vfd-value", require_arg, '0'},
                                          {"dst-vfd-name", require_arg, 'Y'},
                                          {"dst-vfd-info", require_arg, 'Z'},
                                          {NULL, 0, '\0'}};

static H5FD_onion_fapl_info_t onion_fa_in_g = {
    H5FD_ONION_FAPL_INFO_VERSION_CURR,
    H5P_DEFAULT,                   /* backing_fapl_id                */
    32,                            /* page_size                      */
    H5FD_ONION_STORE_TARGET_ONION, /* store_target                   */
    H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
    0,            /* force_write_open               */
    0,            /* creation_flags                 */
    "input file", /* comment                        */
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
static void
usage(const char *prog)
{
    FLUSHSTREAM(rawoutstream);
    PRINTSTREAM(rawoutstream, "usage: %s [OPTIONS] file1 file2\n", prog);
    PRINTVALSTREAM(rawoutstream, "  file1                    Input HDF5 File\n");
    PRINTVALSTREAM(rawoutstream, "  file2                    Output HDF5 File\n");
    PRINTVALSTREAM(rawoutstream, "  ERROR\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --enable-error-stack    Prints messages from the HDF5 error stack as they occur.\n");
    PRINTVALSTREAM(rawoutstream,
                   "                           Optional value 2 also prints file open errors.\n");
    PRINTVALSTREAM(rawoutstream, "  OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "   -h, --help              Print a usage message and exit\n");
    PRINTVALSTREAM(rawoutstream, "   -v N, --verbose=N       Verbose mode, print object information.\n");
    PRINTVALSTREAM(rawoutstream, "      N - is an integer greater than 1, 2 displays read/write timing\n");
    PRINTVALSTREAM(rawoutstream, "   -V, --version           Print version number and exit\n");
    PRINTVALSTREAM(rawoutstream, "   -n, --native            Use a native HDF5 type when repacking\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --src-vol-value         Value (ID) of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                           input HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --src-vol-name          Name of the VOL connector to use for opening the input\n");
    PRINTVALSTREAM(rawoutstream, "                           HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --src-vol-info          VOL-specific info to pass to the VOL connector used for\n");
    PRINTVALSTREAM(rawoutstream, "                           opening the input HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --dst-vol-value         Value (ID) of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                           output HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --dst-vol-name          Name of the VOL connector to use for opening the output\n");
    PRINTVALSTREAM(rawoutstream, "                           HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --dst-vol-info          VOL-specific info to pass to the VOL connector used for\n");
    PRINTVALSTREAM(rawoutstream, "                           opening the output HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --src-vfd-value         Value (ID) of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                           input HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --src-vfd-name          Name of the VFL driver to use for opening the input\n");
    PRINTVALSTREAM(rawoutstream, "                           HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --src-vfd-info          VFD-specific info to pass to the VFL driver used for\n");
    PRINTVALSTREAM(rawoutstream, "                           opening the input HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --dst-vfd-value         Value (ID) of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                           output HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --dst-vfd-name          Name of the VFL driver to use for opening the output\n");
    PRINTVALSTREAM(rawoutstream, "                           HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --dst-vfd-info          VFD-specific info to pass to the VFL driver used for\n");
    PRINTVALSTREAM(rawoutstream, "                           opening the output HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "   -L, --latest            Use latest version of file format\n");
    PRINTVALSTREAM(rawoutstream,
                   "                           This option will take precedence over the options\n");
    PRINTVALSTREAM(rawoutstream, "                           --low and --high\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --low=BOUND             The low bound for library release versions to use\n");
    PRINTVALSTREAM(rawoutstream, "                           when creating objects in the file\n");
    PRINTVALSTREAM(rawoutstream, "                           (default is H5F_LIBVER_EARLIEST)\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --high=BOUND            The high bound for library release versions to use\n");
    PRINTVALSTREAM(rawoutstream, "                           when creating objects in the file\n");
    PRINTVALSTREAM(rawoutstream, "                           (default is H5F_LIBVER_LATEST)\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --merge                 Follow external soft link recursively and merge data\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --prune                 Do not follow external soft links and remove link\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --merge --prune         Follow external link, merge data and remove dangling link\n");
    PRINTVALSTREAM(rawoutstream, "   -c L1, --compact=L1     Maximum number of links in header messages\n");
    PRINTVALSTREAM(rawoutstream,
                   "   -d L2, --indexed=L2     Minimum number of links in the indexed format\n");
    PRINTVALSTREAM(rawoutstream, "   -s S[:F], --ssize=S[:F] Shared object header message minimum size\n");
    PRINTVALSTREAM(rawoutstream,
                   "   -m M, --minimum=M       Do not apply the filter to datasets smaller than M\n");
    PRINTVALSTREAM(rawoutstream, "   -e E, --file=E          Name of file E with the -f and -l options\n");
    PRINTVALSTREAM(rawoutstream,
                   "   -u U, --ublock=U        Name of file U with user block data to be added\n");
    PRINTVALSTREAM(rawoutstream, "   -b B, --block=B         Size of user block to be added\n");
    PRINTVALSTREAM(rawoutstream,
                   "   -M A, --metadata_block_size=A  Metadata block size for H5Pset_meta_block_size\n");
    PRINTVALSTREAM(rawoutstream, "   -t T, --threshold=T     Threshold value for H5Pset_alignment\n");
    PRINTVALSTREAM(rawoutstream, "   -a A, --alignment=A     Alignment value for H5Pset_alignment\n");
    PRINTVALSTREAM(rawoutstream, "   -q Q, --sort_by=Q       Sort groups and attributes by index Q\n");
    PRINTVALSTREAM(rawoutstream, "   -z Z, --sort_order=Z    Sort groups and attributes by order Z\n");
    PRINTVALSTREAM(rawoutstream, "   -f FILT, --filter=FILT  Filter type\n");
    PRINTVALSTREAM(rawoutstream, "   -l LAYT, --layout=LAYT  Layout type\n");
    PRINTVALSTREAM(rawoutstream,
                   "   -S FS_STRATEGY, --fs_strategy=FS_STRATEGY  File space management strategy for\n");
    PRINTVALSTREAM(rawoutstream, "                           H5Pset_file_space_strategy\n");
    PRINTVALSTREAM(rawoutstream,
                   "   -P FS_PERSIST, --fs_persist=FS_PERSIST  Persisting or not persisting free-\n");
    PRINTVALSTREAM(rawoutstream, "                           space for H5Pset_file_space_strategy\n");
    PRINTVALSTREAM(rawoutstream,
                   "   -T FS_THRESHOLD, --fs_threshold=FS_THRESHOLD   Free-space section threshold\n");
    PRINTVALSTREAM(rawoutstream, "                           for H5Pset_file_space_strategy\n");
    PRINTVALSTREAM(rawoutstream, "   -G FS_PAGESIZE, --fs_pagesize=FS_PAGESIZE   File space page size for\n");
    PRINTVALSTREAM(rawoutstream, "                           H5Pset_file_space_page_size\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "    M - is an integer greater than 1, size of dataset in bytes (default is 0)\n");
    PRINTVALSTREAM(rawoutstream, "    E - is a filename.\n");
    PRINTVALSTREAM(rawoutstream, "    S - is an integer\n");
    PRINTVALSTREAM(rawoutstream, "    U - is a filename.\n");
    PRINTVALSTREAM(rawoutstream, "    T - is an integer\n");
    PRINTVALSTREAM(rawoutstream, "    A - is an integer greater than zero\n");
    PRINTVALSTREAM(rawoutstream,
                   "    Q - is the sort index type for the input file. It can be \"name\" or\n");
    PRINTVALSTREAM(rawoutstream, "        \"creation_order\" (default)\n");
    PRINTVALSTREAM(rawoutstream,
                   "    Z - is the sort order type for the input file. It can be \"descending\" or\n");
    PRINTVALSTREAM(rawoutstream, "        \"ascending\" (default)\n");
    PRINTVALSTREAM(rawoutstream, "    B - is the user block size, any value that is 512 or greater and is\n");
    PRINTVALSTREAM(rawoutstream, "        a power of 2 (1024 default)\n");
    PRINTVALSTREAM(rawoutstream,
                   "    F - is the shared object header message type, any of <dspace|dtype|fill|\n");
    PRINTVALSTREAM(rawoutstream, "        pline|attr>. If F is not specified, S applies to all messages\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "    BOUND is an integer indicating the library release versions to use when\n");
    PRINTVALSTREAM(rawoutstream, "          creating objects in the file (see H5Pset_libver_bounds()):\n");
    PRINTVALSTREAM(rawoutstream, "        0: This is H5F_LIBVER_EARLIEST in H5F_libver_t struct\n");
    PRINTVALSTREAM(rawoutstream, "        1: This is H5F_LIBVER_V18 in H5F_libver_t struct\n");
    PRINTVALSTREAM(rawoutstream, "        2: This is H5F_LIBVER_V110 in H5F_libver_t struct\n");
    PRINTVALSTREAM(rawoutstream, "        3: This is H5F_LIBVER_V112 in H5F_libver_t struct\n");
    PRINTVALSTREAM(rawoutstream, "        4: This is H5F_LIBVER_V114 in H5F_libver_t struct\n");
    PRINTVALSTREAM(rawoutstream,
                   "           (H5F_LIBVER_LATEST is aliased to H5F_LIBVER_V114 for this release\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    FS_STRATEGY is a string indicating the file space strategy used:\n");
    PRINTVALSTREAM(rawoutstream, "        FSM_AGGR:\n");
    PRINTVALSTREAM(rawoutstream,
                   "               The mechanisms used in managing file space are free-space\n");
    PRINTVALSTREAM(rawoutstream, "               managers, aggregators and virtual file driver.\n");
    PRINTVALSTREAM(rawoutstream, "        PAGE:\n");
    PRINTVALSTREAM(rawoutstream,
                   "               The mechanisms used in managing file space are free-space\n");
    PRINTVALSTREAM(rawoutstream,
                   "               managers with embedded paged aggregation and virtual file driver.\n");
    PRINTVALSTREAM(rawoutstream, "        AGGR:\n");
    PRINTVALSTREAM(rawoutstream,
                   "               The mechanisms used in managing file space are aggregators and\n");
    PRINTVALSTREAM(rawoutstream, "               virtual file driver.\n");
    PRINTVALSTREAM(rawoutstream, "        NONE:\n");
    PRINTVALSTREAM(rawoutstream,
                   "               The mechanisms used in managing file space are virtual file\n");
    PRINTVALSTREAM(rawoutstream, "               driver.\n");
    PRINTVALSTREAM(rawoutstream,
                   "        The default strategy when not set is FSM_AGGR without persisting free-\n");
    PRINTVALSTREAM(rawoutstream, "        space.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "    FS_PERSIST is 1 to persisting free-space or 0 to not persisting free-space.\n");
    PRINTVALSTREAM(rawoutstream, "      The default when not set is not persisting free-space.\n");
    PRINTVALSTREAM(rawoutstream, "      The value is ignored for AGGR and NONE strategies.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "    FS_THRESHOLD is the minimum size (in bytes) of free-space sections to be\n");
    PRINTVALSTREAM(rawoutstream, "        tracked by the library.\n");
    PRINTVALSTREAM(rawoutstream, "      The default when not set is 1.\n");
    PRINTVALSTREAM(rawoutstream, "      The value is ignored for AGGR and NONE strategies.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "    FS_PAGESIZE is the size (in bytes) >=512 that is used by the library when\n");
    PRINTVALSTREAM(rawoutstream, "        the file space strategy PAGE is used.\n");
    PRINTVALSTREAM(rawoutstream, "      The default when not set is 4096.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    FILT - is a string with the format:\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects>:<name of filter>=<filter parameters>\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "      <list of objects> is a comma separated list of object names, meaning apply\n");
    PRINTVALSTREAM(rawoutstream,
                   "        compression only to those objects. If no names are specified, the filter\n");
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
    PRINTVALSTREAM(rawoutstream,
                   "        SZIP=<pixels per block,coding> pixels per block is a even number in\n");
    PRINTVALSTREAM(rawoutstream, "            2-32 and coding method is either EC or NN\n");
    PRINTVALSTREAM(rawoutstream, "        SHUF (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        FLET (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "        NBIT (no parameter)\n");
    PRINTVALSTREAM(rawoutstream,
                   "        SOFF=<scale_factor,scale_type> scale_factor is an integer and scale_type\n");
    PRINTVALSTREAM(rawoutstream, "            is either IN or DS\n");
    PRINTVALSTREAM(rawoutstream,
                   "        UD=<filter_number,filter_flag,cd_value_count,value1[,value2,...,valueN]>\n");
    PRINTVALSTREAM(rawoutstream,
                   "            Required values: filter_number, filter_flag, cd_value_count, value1\n");
    PRINTVALSTREAM(rawoutstream, "            Optional values: value2 to valueN\n");
    PRINTVALSTREAM(rawoutstream, "            filter_flag: 1 is OPTIONAL or 0 is MANDATORY\n");
    PRINTVALSTREAM(rawoutstream, "        NONE (no parameter)\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "    LAYT - is a string with the format:\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      <list of objects>:<layout type>=<layout parameters>\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "      <list of objects> is a comma separated list of object names, meaning that\n");
    PRINTVALSTREAM(rawoutstream,
                   "        layout information is supplied for those objects. If no names are\n");
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
    PRINTVALSTREAM(rawoutstream,
                   "   SZIP compression with 8 pixels per block and NN coding method to object dset1\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "3) h5repack -v -l dset1,dset2:CHUNK=20x10 -f dset3,dset4,dset5:NONE file1 file2\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "   Chunked layout, with a layout size of 20x10, to objects dset1 and dset2\n");
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
static void
leave(int ret)
{
    h5tools_close();
    exit(ret);
}

/*-------------------------------------------------------------------------
 * Function: read_info
 *
 * Purpose: read comp and chunk options from a file
 *
 * Return: void, exit on error
 *-------------------------------------------------------------------------
 */
static int
read_info(const char *filename, pack_opt_t *options)
{
    char  stype[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    char  comp_info[1024];
    FILE *fp = NULL;
    char  c;
    int   i;
    int   ret_value = EXIT_SUCCESS;

    if (NULL == (fp = fopen(filename, "r"))) {
        error_msg("cannot open options file %s\n", filename);
        h5tools_setstatus(EXIT_FAILURE);
        ret_value = EXIT_FAILURE;
        goto done;
    }

    /* cycle until end of file reached */
    while (1) {
        if (EOF == fscanf(fp, "%9s", stype))
            break;

        /* Info indicator must be for layout or filter */
        if (strcmp(stype, "-l") != 0 && strcmp(stype, "-f") != 0) {
            error_msg("bad file format for %s", filename);
            h5tools_setstatus(EXIT_FAILURE);
            ret_value = EXIT_FAILURE;
            goto done;
        }

        /* find beginning of info */
        i = 0;
        c = '0';
        while (c != ' ') {
            if (fscanf(fp, "%c", &c) < 0 && ferror(fp)) {
                error_msg("fscanf error\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret_value = EXIT_FAILURE;
                goto done;
            }
            if (feof(fp))
                break;
        }
        c = '0';
        /* go until end */
        while (c != ' ') {
            if (fscanf(fp, "%c", &c) < 0 && ferror(fp)) {
                error_msg("fscanf error\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret_value = EXIT_FAILURE;
                goto done;
            }
            comp_info[i++] = c;
            if (feof(fp))
                break;
            if (c == 10 /*eol*/)
                break;
        }
        comp_info[i - 1] = '\0'; /*cut the last " */

        if (!strcmp(stype, "-l")) {
            if (h5repack_addlayout(comp_info, options) == -1) {
                error_msg("could not add chunk option\n");
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
        fclose(fp);

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

    if (!strcmp(form, "name"))
        idx_type = H5_INDEX_NAME;
    else if (!strcmp(form, "creation_order"))
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

    if (!strcmp(form, "ascending"))
        iter_order = H5_ITER_INC;
    else if (!strcmp(form, "descending"))
        iter_order = H5_ITER_DEC;

    return iter_order;
}

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: parse command line input
 *-------------------------------------------------------------------------
 */
static int
parse_command_line(int argc, const char *const *argv, pack_opt_t *options)
{
    h5tools_vol_info_t in_vol_info;
    h5tools_vol_info_t out_vol_info;
    h5tools_vfd_info_t in_vfd_info;
    h5tools_vfd_info_t out_vfd_info;
    bool               custom_in_vol  = false;
    bool               custom_in_vfd  = false;
    bool               custom_out_vol = false;
    bool               custom_out_vfd = false;
    hid_t              tmp_fapl       = H5I_INVALID_HID;
    int                bound, opt;
    int                ret_value = 0;

    /* Initialize fapl info structs */
    memset(&in_vol_info, 0, sizeof(h5tools_vol_info_t));
    memset(&out_vol_info, 0, sizeof(h5tools_vol_info_t));
    memset(&in_vfd_info, 0, sizeof(h5tools_vfd_info_t));
    memset(&out_vfd_info, 0, sizeof(h5tools_vfd_info_t));

    /* parse command line options */
    while (EOF != (opt = H5_get_option(argc, argv, s_opts, l_opts))) {
        switch ((char)opt) {

            /* -i for backward compatibility */
            case 'i':
                infile = H5_optarg;
                has_i++;
                break;

            /* -o for backward compatibility */
            case 'o':
                outfile = H5_optarg;
                has_o++;
                break;

            case 'h':
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                ret_value = 1;
                goto done;

            case 'V':
                print_version(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                ret_value = 1;
                goto done;

            case 'v':
                if (H5_optarg != NULL) {
                    if (2 == atoi(H5_optarg))
                        options->verbose = 2;
                }
                else
                    options->verbose = 1;
                break;

            case 'f':
                /* parse the -f filter option */
                if (h5repack_addfilter(H5_optarg, options) < 0) {
                    error_msg("in parsing filter\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'l':
                /* parse the -l layout option */
                if (h5repack_addlayout(H5_optarg, options) < 0) {
                    error_msg("in parsing layout\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'm':
                options->min_comp = strtoull(H5_optarg, NULL, 0);
                if ((int)options->min_comp <= 0) {
                    error_msg("invalid minimum compress size <%s>\n", H5_optarg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'e':
                if ((ret_value = read_info(H5_optarg, options)) < 0) {
                    error_msg("failed to read from repack options file <%s>\n", H5_optarg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'n':
                options->use_native = 1;
                break;

            case 'L':
                options->latest = true;
                break;

            case 'j':
                bound = atoi(H5_optarg);
                if (bound < H5F_LIBVER_EARLIEST || bound > H5F_LIBVER_LATEST) {
                    error_msg("in parsing low bound\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                options->low_bound = bound;
                break;

            case 'k':
                bound = atoi(H5_optarg);
                if (bound < H5F_LIBVER_EARLIEST || bound > H5F_LIBVER_LATEST) {
                    error_msg("in parsing high bound\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                options->high_bound = bound;
                break;

            case 'X':
                options->merge = true;
                break;

            case 'W':
                options->prune = true;
                break;

            case 'c':
                options->grp_compact = atoi(H5_optarg);
                if (options->grp_compact > 0)
                    options->latest = true; /* must use latest format */
                break;

            case 'd':
                options->grp_indexed = atoi(H5_optarg);
                if (options->grp_indexed > 0)
                    options->latest = true; /* must use latest format */
                break;

            case 's': {
                int   idx       = 0;
                int   ssize     = 0;
                char *msgPtr    = strchr(H5_optarg, ':');
                options->latest = true; /* must use latest format */
                if (msgPtr == NULL) {
                    ssize = atoi(H5_optarg);
                    for (idx = 0; idx < 5; idx++)
                        options->msg_size[idx] = ssize;
                }
                else {
                    char msgType[10];

                    strcpy(msgType, msgPtr + 1);
                    msgPtr[0] = '\0';
                    ssize     = atoi(H5_optarg);
                    if (!strncmp(msgType, "dspace", 6))
                        options->msg_size[0] = ssize;
                    else if (!strncmp(msgType, "dtype", 5))
                        options->msg_size[1] = ssize;
                    else if (!strncmp(msgType, "fill", 4))
                        options->msg_size[2] = ssize;
                    else if (!strncmp(msgType, "pline", 5))
                        options->msg_size[3] = ssize;
                    else if (!strncmp(msgType, "attr", 4))
                        options->msg_size[4] = ssize;
                }
            } break;

            case 'u':
                options->ublock_filename = H5_optarg;
                break;

            case 'b':
                options->ublock_size = (hsize_t)atol(H5_optarg);
                break;

            case 'M':
                options->meta_block_size = (hsize_t)atol(H5_optarg);
                break;

            case 't':
                options->threshold = (hsize_t)atol(H5_optarg);
                break;

            case 'a':
                options->alignment = strtoull(H5_optarg, NULL, 0);
                if (options->alignment < 1) {
                    error_msg("invalid alignment size `%s`\n", H5_optarg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'S': {
                char strategy[MAX_NC_NAME];

                strcpy(strategy, H5_optarg);
                if (!strcmp(strategy, "FSM_AGGR"))
                    options->fs_strategy = H5F_FSPACE_STRATEGY_FSM_AGGR;
                else if (!strcmp(strategy, "PAGE"))
                    options->fs_strategy = H5F_FSPACE_STRATEGY_PAGE;
                else if (!strcmp(strategy, "AGGR"))
                    options->fs_strategy = H5F_FSPACE_STRATEGY_AGGR;
                else if (!strcmp(strategy, "NONE"))
                    options->fs_strategy = H5F_FSPACE_STRATEGY_NONE;
                else {
                    error_msg("invalid file space management strategy `%s`\n", H5_optarg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                if (options->fs_strategy == (H5F_fspace_strategy_t)0)
                    /* To distinguish the "specified" zero value */
                    options->fs_strategy = (H5F_fspace_strategy_t)-1;
            } break;

            case 'P':
                options->fs_persist = atoi(H5_optarg);
                if (options->fs_persist == 0)
                    /* To distinguish the "specified" zero value */
                    options->fs_persist = -1;
                break;

            case 'T':
                options->fs_threshold = atol(H5_optarg);
                if (options->fs_threshold == 0)
                    /* To distinguish the "specified" zero value */
                    options->fs_threshold = -1;
                break;

            case 'G':
                options->fs_pagesize = strtoll(H5_optarg, NULL, 0);
                if (options->fs_pagesize == 0)
                    /* To distinguish the "specified" zero value */
                    options->fs_pagesize = -1;
                break;

            case 'q':
                if (H5_INDEX_UNKNOWN == (sort_by = set_sort_by(H5_optarg))) {
                    error_msg("failed to set sort by form <%s>\n", H5_optarg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'z':
                if (H5_ITER_UNKNOWN == (sort_order = set_sort_order(H5_optarg))) {
                    error_msg("failed to set sort order form <%s>\n", H5_optarg);
                    h5tools_setstatus(EXIT_FAILURE);
                    ret_value = -1;
                    goto done;
                }
                break;

            case 'E':
                if (H5_optarg != NULL)
                    enable_error_stack = atoi(H5_optarg);
                else
                    enable_error_stack = 1;
                break;

            case '1':
                in_vol_info.type    = VOL_BY_VALUE;
                in_vol_info.u.value = (H5VL_class_value_t)atoi(H5_optarg);
                custom_in_vol       = true;
                break;

            case '2':
                in_vol_info.type   = VOL_BY_NAME;
                in_vol_info.u.name = H5_optarg;
                custom_in_vol      = true;
                break;

            case '3':
                in_vol_info.info_string = H5_optarg;
                break;

            case '4':
                out_vol_info.type    = VOL_BY_VALUE;
                out_vol_info.u.value = (H5VL_class_value_t)atoi(H5_optarg);
                custom_out_vol       = true;
                break;

            case '5':
                out_vol_info.type   = VOL_BY_NAME;
                out_vol_info.u.name = H5_optarg;
                custom_out_vol      = true;
                break;

            case '6':
                out_vol_info.info_string = H5_optarg;
                break;

            case '7':
                in_vfd_info.type    = VFD_BY_VALUE;
                in_vfd_info.u.value = (H5FD_class_value_t)atoi(H5_optarg);
                custom_in_vfd       = true;
                break;

            case '8':
                in_vfd_info.type   = VFD_BY_NAME;
                in_vfd_info.u.name = H5_optarg;
                custom_in_vfd      = true;
                break;

            case '9':
                in_vfd_info.info = (const void *)H5_optarg;
                break;

            case '0':
                out_vfd_info.type    = VFD_BY_VALUE;
                out_vfd_info.u.value = (H5FD_class_value_t)atoi(H5_optarg);
                custom_out_vfd       = true;
                break;

            case 'Y':
                out_vfd_info.type   = VFD_BY_NAME;
                out_vfd_info.u.name = H5_optarg;
                custom_out_vfd      = true;
                break;

            case 'Z':
                out_vfd_info.info = (const void *)H5_optarg;
                break;

            default:
                break;
        } /* end switch */
    }     /* end while there are more options to parse */

    /* If neither -i nor -o given, get in and out files positionally */
    if (0 == (has_i + has_o)) {
        if (argv[H5_optind] != NULL && argv[H5_optind + 1] != NULL) {
            infile  = argv[H5_optind];
            outfile = argv[H5_optind + 1];

            if (!strcmp(infile, outfile)) {
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

    /* If the input file uses the onion VFD, get the revision number */
    if (in_vfd_info.u.name && !strcmp(in_vfd_info.u.name, "onion")) {
        if (in_vfd_info.info) {
            errno                      = 0;
            onion_fa_in_g.revision_num = strtoull(in_vfd_info.info, NULL, 10);
            if (errno == ERANGE) {
                printf("Invalid onion revision specified for the input file\n");
                usage(h5tools_getprogname());
                exit(EXIT_FAILURE);
            }
        }
        else
            onion_fa_in_g.revision_num = 0;

        in_vfd_info.info = &onion_fa_in_g;
    }

    /* Setup FAPL for input and output file accesses */
    if (custom_in_vol || custom_in_vfd) {
        if ((tmp_fapl = h5tools_get_fapl(options->fin_fapl, custom_in_vol ? &in_vol_info : NULL,
                                         custom_in_vfd ? &in_vfd_info : NULL)) < 0) {
            error_msg("failed to setup FAPL for input file\n");
            h5tools_setstatus(EXIT_FAILURE);
            ret_value = -1;
            goto done;
        }

        /* Close old FAPL */
        if (options->fin_fapl != H5P_DEFAULT)
            if (H5Pclose(options->fin_fapl) < 0) {
                error_msg("failed to close FAPL\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret_value = -1;
                goto done;
            }

        options->fin_fapl = tmp_fapl;
    }

    if (custom_out_vol || custom_out_vfd) {
        if ((tmp_fapl = h5tools_get_fapl(options->fout_fapl, custom_out_vol ? &out_vol_info : NULL,
                                         custom_out_vfd ? &out_vfd_info : NULL)) < 0) {
            error_msg("failed to setup FAPL for output file\n");
            h5tools_setstatus(EXIT_FAILURE);
            ret_value = -1;
            goto done;
        }

        /* Close old FAPL */
        if (options->fout_fapl != H5P_DEFAULT)
            if (H5Pclose(options->fout_fapl) < 0) {
                error_msg("failed to close FAPL\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret_value = -1;
                goto done;
            }

        options->fout_fapl = tmp_fapl;
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
int
main(int argc, char **argv)
{
    pack_opt_t options; /*the global options */
    int        parse_ret;

    memset(&options, 0, sizeof(pack_opt_t));

    /* Initialize h5tools lib */
    h5tools_init();

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* update hyperslab buffer size from H5TOOLS_BUFSIZE env if exist */
    if (h5tools_getenv_update_hyperslab_bufsize() < 0) {
        printf("Error occurred while retrieving H5TOOLS_BUFSIZE value\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* initialize options  */
    if (h5repack_init(&options, 0, false) < 0) {
        printf("Error occurred while initializing repack options\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Initialize default indexing options */
    sort_by = H5_INDEX_CRT_ORDER;

    parse_ret = parse_command_line(argc, (const char *const *)argv, &options);
    if (parse_ret < 0) {
        printf("Error occurred while parsing command-line options\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }
    else if (parse_ret > 0) {
        /* Short-circuit success */
        h5tools_setstatus(EXIT_SUCCESS);
        goto done;
    }

    /* enable error reporting if command line option */
    h5tools_error_report();

    /* pack it */
    if (h5repack(infile, outfile, &options) < 0) {
        printf("Error occurred while repacking\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    h5tools_setstatus(EXIT_SUCCESS);

done:
    if (options.fin_fapl >= 0 && options.fin_fapl != H5P_DEFAULT)
        H5Pclose(options.fin_fapl);
    if (options.fout_fapl >= 0 && options.fout_fapl != H5P_DEFAULT)
        H5Pclose(options.fout_fapl);

    /* free tables */
    h5repack_end(&options);

    leave(h5tools_getstatus());
}
