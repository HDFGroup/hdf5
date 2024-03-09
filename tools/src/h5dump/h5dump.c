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

#include "h5dump.h"
#include "h5dump_ddl.h"
#include "h5dump_xml.h"

/* Name of tool */
#define PROGRAMNAME "h5dump"

const char        *outfname_g    = NULL;
static bool        doxml_g       = false;
static bool        useschema_g   = true;
static const char *xml_dtd_uri_g = NULL;

static bool use_custom_vol_g = false;
static bool use_custom_vfd_g = false;

static h5tools_vol_info_t vol_info_g = {0};
static h5tools_vfd_info_t vfd_info_g = {0};

static bool get_onion_revision_count = false;

#ifdef H5_HAVE_ROS3_VFD
/* Default "anonymous" S3 configuration */
static H5FD_ros3_fapl_ext_t ros3_fa_g = {
    {
        1,     /* Structure Version */
        false, /* Authenticate?     */
        "",    /* AWS Region        */
        "",    /* Access Key ID     */
        "",    /* Secret Access Key */
    },
    "", /* Session/security token */
};
#endif /* H5_HAVE_ROS3_VFD */

#ifdef H5_HAVE_LIBHDFS
/* "Default" HDFS configuration */
static H5FD_hdfs_fapl_t hdfs_fa_g = {
    1,           /* Structure Version     */
    "localhost", /* Namenode Name         */
    0,           /* Namenode Port         */
    "",          /* Kerberos ticket cache */
    "",          /* User name             */
    2048,        /* Stream buffer size    */
};
#endif /* H5_HAVE_LIBHDFS */

static H5FD_onion_fapl_info_t onion_fa_g = {
    H5FD_ONION_FAPL_INFO_VERSION_CURR,
    H5P_DEFAULT,                   /* backing_fapl_id                */
    32,                            /* page_size                      */
    H5FD_ONION_STORE_TARGET_ONION, /* store_target                   */
    H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
    0,            /* force_write_open               */
    0,            /* creation_flags                 */
    "input file", /* comment                        */
};

/* module-scoped variables for XML option */
#define DEFAULT_XSD "http://www.hdfgroup.org/HDF5/XML/schema/HDF5-File.xsd"
#define DEFAULT_DTD "http://www.hdfgroup.org/HDF5/XML/DTD/HDF5-File.dtd"

/* Standard DDL output */
static const dump_functions ddl_function_table = {
    dump_group, dump_named_datatype, dump_dataset, dump_dataspace, dump_datatype, dump_attr_cb, dump_data};

/* XML output */
static const dump_functions xml_function_table = {
    xml_dump_group,    xml_dump_named_datatype, xml_dump_dataset, xml_dump_dataspace,
    xml_dump_datatype, xml_dump_attr,           xml_dump_data};

/* internal functions */
static void init_prefix(char **prfx, size_t prfx_len);

/* a structure for handling the order command-line parameters come in */
struct handler_t {
    void (*func)(hid_t, const char *, void *, int, const char *);
    char            *obj;
    struct subset_t *subset_info;
};

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
/* The following initialization makes use of C language concatenating */
/* "xxx" "yyy" into "xxxyyy". */
static const char            *s_opts   = "a:b*c:d:ef:g:hik:l:m:n*o*pq:rs:t:uvw:xyz:A*BCD:E*F:G:HM:N:O*RS:VX:";
static struct h5_long_options l_opts[] = {{"attribute", require_arg, 'a'},
                                          {"binary", optional_arg, 'b'},
                                          {"count", require_arg, 'c'},
                                          {"dataset", require_arg, 'd'},
                                          {"escape", no_arg, 'e'},
                                          {"filedriver", require_arg, 'f'},
                                          {"group", require_arg, 'g'},
                                          {"help", no_arg, 'h'},
                                          {"object-ids", no_arg, 'i'},
                                          {"block", require_arg, 'k'},
                                          {"soft-link", require_arg, 'l'},
                                          {"format", require_arg, 'm'},
                                          {"contents", optional_arg, 'n'},
                                          {"output", optional_arg, 'o'},
                                          {"properties", no_arg, 'p'},
                                          {"sort_by", require_arg, 'q'},
                                          {"string", no_arg, 'r'},
                                          {"start", require_arg, 's'},
                                          {"datatype", require_arg, 't'},
                                          {"use-dtd", no_arg, 'u'},
                                          {"vds-view-first-missing", no_arg, 'v'},
                                          {"width", require_arg, 'w'},
                                          {"xml", no_arg, 'x'},
                                          {"noindex", no_arg, 'y'},
                                          {"sort_order", require_arg, 'z'},
                                          {"onlyattr", optional_arg, 'A'},
                                          {"superblock", no_arg, 'B'},
                                          {"boot-block", no_arg, 'B'},
                                          {"no-compact-subset", no_arg, 'C'},
                                          {"xml-dtd", require_arg, 'D'},
                                          {"enable-error-stack", optional_arg, 'E'},
                                          {"form", require_arg, 'F'},
                                          {"vds-gap-size", require_arg, 'G'},
                                          {"header", no_arg, 'H'},
                                          {"packed-bits", require_arg, 'M'},
                                          {"any_path", require_arg, 'N'},
                                          {"ddl", optional_arg, 'O'},
                                          {"region", no_arg, 'R'},
                                          {"stride", require_arg, 'S'},
                                          {"version", no_arg, 'V'},
                                          {"xml-ns", require_arg, 'X'},
                                          {"s3-cred", require_arg, '$'},
                                          {"hdfs-attrs", require_arg, '#'},
                                          {"vol-value", require_arg, '1'},
                                          {"vol-name", require_arg, '2'},
                                          {"vol-info", require_arg, '3'},
                                          {"vfd-value", require_arg, '4'},
                                          {"vfd-name", require_arg, '5'},
                                          {"vfd-info", require_arg, '6'},
                                          {NULL, 0, '\0'}};

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
 * Function:    usage
 *
 * Purpose:     Print the usage message about dumper
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    FLUSHSTREAM(rawoutstream);
    PRINTSTREAM(rawoutstream, "usage: %s [OPTIONS] files\n", prog);
    PRINTVALSTREAM(rawoutstream, "  OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "     -h,   --help         Print a usage message and exit\n");
    PRINTVALSTREAM(rawoutstream, "     -V,   --version      Print version number and exit\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Error Options ---------------\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --enable-error-stack Prints messages from the HDF5 error stack as they occur.\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          Optional value 2 also prints file open errors.\n");
    PRINTVALSTREAM(rawoutstream, "                          Default setting disables any error reporting.\n");
    PRINTVALSTREAM(rawoutstream, "--------------- File Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -n,   --contents     Print a list of the file contents and exit\n");
    PRINTVALSTREAM(rawoutstream, "                          Optional value 1 also prints attributes.\n");
    PRINTVALSTREAM(rawoutstream, "     -B,   --superblock   Print the content of the super block\n");
    PRINTVALSTREAM(rawoutstream, "     -H,   --header       Print the header only; no data is displayed\n");
    PRINTVALSTREAM(rawoutstream, "     -f D, --filedriver=D Specify which driver to open the file with\n");
    PRINTVALSTREAM(rawoutstream, "     -o F, --output=F     Output raw data into file F\n");
    PRINTVALSTREAM(rawoutstream, "     -b B, --binary=B     Binary file output, of form B\n");
    PRINTVALSTREAM(rawoutstream, "     -O F, --ddl=F        Output ddl text into file F\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          Use blank(empty) filename F to suppress ddl display\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --s3-cred=<cred>     Supply S3 authentication information to \"ros3\" vfd.\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          <cred> :: \"(<aws-region>,<access-id>,<access-key>)\"\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          If absent or <cred> -> \"(,,)\", no authentication.\n");
    PRINTVALSTREAM(rawoutstream, "                          Has no effect if filedriver is not \"ros3\".\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --hdfs-attrs=<attrs> Supply configuration information for HDFS file access.\n");
    PRINTVALSTREAM(rawoutstream, "                          For use with \"--filedriver=hdfs\"\n");
    PRINTVALSTREAM(rawoutstream, "                          <attrs> :: (<namenode name>,<namenode port>,\n");
    PRINTVALSTREAM(rawoutstream, "                                      <kerberos cache path>,<username>,\n");
    PRINTVALSTREAM(rawoutstream, "                                      <buffer size>)\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          Any absent attribute will use a default value.\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --vol-value          Value (ID) of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                          HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --vol-name           Name of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                          HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --vol-info           VOL-specific info to pass to the VOL connector used for\n");
    PRINTVALSTREAM(rawoutstream, "                          opening the HDF5 file specified\n");
    PRINTVALSTREAM(
        rawoutstream,
        "                          If none of the above options are used to specify a VOL, then\n");
    PRINTVALSTREAM(
        rawoutstream,
        "                          the VOL named by HDF5_VOL_CONNECTOR (or the native VOL connector,\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          if that environment variable is unset) will be used\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --vfd-value          Value (ID) of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                          HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "     --vfd-name           Name of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                          HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --vfd-info           VFD-specific info to pass to the VFL driver used for\n");
    PRINTVALSTREAM(rawoutstream, "                          opening the HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Object Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -a P, --attribute=P  Print the specified attribute\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          If an attribute name contains a slash (/), escape the\n");
    PRINTVALSTREAM(rawoutstream, "                          slash with a preceding backslash (\\).\n");
    PRINTVALSTREAM(rawoutstream, "                          (See example section below.)\n");
    PRINTVALSTREAM(rawoutstream, "     -d P, --dataset=P    Print the specified dataset\n");
    PRINTVALSTREAM(rawoutstream, "     -g P, --group=P      Print the specified group and all members\n");
    PRINTVALSTREAM(rawoutstream, "     -l P, --soft-link=P  Print the value(s) of the specified soft link\n");
    PRINTVALSTREAM(rawoutstream, "     -t P, --datatype=P   Print the specified named datatype\n");
    PRINTVALSTREAM(
        rawoutstream,
        "     -N P, --any_path=P   Print any attribute, dataset, group, datatype, or link that matches P\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          P can be the absolute path or just a relative path.\n");
    PRINTVALSTREAM(rawoutstream, "     -A,   --onlyattr     Print the header and value of attributes\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          Optional value 0 suppresses printing attributes.\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --vds-view-first-missing Set the VDS bounds to first missing mapped elements.\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --vds-gap-size=N     Set the missing file gap size, N=non-negative integers\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Object Property Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -i,   --object-ids   Print the object ids\n");
    PRINTVALSTREAM(rawoutstream,
                   "     -p,   --properties   Print dataset filters, storage layout and fill value\n");
    PRINTVALSTREAM(rawoutstream,
                   "     -M L, --packedbits=L Print packed bits as unsigned integers, using mask\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          format L for an integer dataset specified with\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          option -d. L is a list of offset,length values,\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          separated by commas. Offset is the beginning bit in\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          the data value and length is the number of bits of\n");
    PRINTVALSTREAM(rawoutstream, "                          the mask.\n");
    PRINTVALSTREAM(rawoutstream, "     -R,   --region       Print dataset pointed by region references\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Formatting Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -e,   --escape       Escape non printing characters\n");
    PRINTVALSTREAM(rawoutstream, "     -r,   --string       Print 1-byte integer datasets as ASCII\n");
    PRINTVALSTREAM(rawoutstream, "     -y,   --noindex      Do not print array indices with the data\n");
    PRINTVALSTREAM(rawoutstream, "     -m T, --format=T     Set the floating point output format\n");
    PRINTVALSTREAM(rawoutstream, "     -q Q, --sort_by=Q    Sort groups and attributes by index Q\n");
    PRINTVALSTREAM(rawoutstream, "     -z Z, --sort_order=Z Sort groups and attributes by order Z\n");
    PRINTVALSTREAM(rawoutstream,
                   "     --no-compact-subset  Disable compact form of subsetting and allow the use\n");
    PRINTVALSTREAM(rawoutstream, "                          of \"[\" in dataset names.\n");
    PRINTVALSTREAM(rawoutstream,
                   "     -w N, --width=N      Set the number of columns of output. A value of 0 (zero)\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          sets the number of columns to the maximum (65535).\n");
    PRINTVALSTREAM(rawoutstream, "                          Default width is 80 columns.\n");
    PRINTVALSTREAM(rawoutstream, "--------------- XML Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -x,   --xml          Output in XML using Schema\n");
    PRINTVALSTREAM(rawoutstream, "     -u,   --use-dtd      Output in XML using DTD\n");
    PRINTVALSTREAM(rawoutstream, "     -D U, --xml-dtd=U    Use the DTD or schema at U\n");
    PRINTVALSTREAM(rawoutstream, "     -X S, --xml-ns=S     (XML Schema) Use qualified names n the XML\n");
    PRINTVALSTREAM(rawoutstream, "                          \":\": no namespace, default: \"hdf5:\"\n");
    PRINTVALSTREAM(rawoutstream,
                   "                          E.g., to dump a file called \"-f\", use h5dump -- -f\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Subsetting Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, " Subsetting is available by using the following options with a dataset\n");
    PRINTVALSTREAM(rawoutstream, " option. Subsetting is done by selecting a hyperslab from the data.\n");
    PRINTVALSTREAM(rawoutstream, " Thus, the options mirror those for performing a hyperslab selection.\n");
    PRINTVALSTREAM(
        rawoutstream,
        " One of the START, COUNT, STRIDE, or BLOCK parameters are mandatory if you do subsetting.\n");
    PRINTVALSTREAM(rawoutstream,
                   " The STRIDE, COUNT, and BLOCK parameters are optional and will default to 1 in\n");
    PRINTVALSTREAM(rawoutstream,
                   " each dimension. START is optional and will default to 0 in each dimension.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "      -s START,  --start=START    Offset of start of subsetting selection\n");
    PRINTVALSTREAM(rawoutstream, "      -S STRIDE, --stride=STRIDE  Hyperslab stride\n");
    PRINTVALSTREAM(rawoutstream,
                   "      -c COUNT,  --count=COUNT    Number of blocks to include in selection\n");
    PRINTVALSTREAM(rawoutstream, "      -k BLOCK,  --block=BLOCK    Size of block in hyperslab\n");
    PRINTVALSTREAM(
        rawoutstream,
        "  START, COUNT, STRIDE, and BLOCK - is a list of integers the number of which are equal to the\n");
    PRINTVALSTREAM(rawoutstream, "      number of dimensions in the dataspace being queried\n");
    PRINTVALSTREAM(rawoutstream,
                   "      (Alternate compact form of subsetting is described in the Reference Manual)\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Option Argument Conventions ---------------\n");
    PRINTVALSTREAM(
        rawoutstream,
        "  D - is the file driver to use in opening the file. Acceptable values are available from\n");
    PRINTVALSTREAM(
        rawoutstream,
        "      "
        "https://portal.hdfgroup.org/documentation/hdf5-docs/registered_virtual_file_drivers_vfds.html.\n");
    PRINTVALSTREAM(rawoutstream,
                   "      Without the file driver flag, the file will be opened with each driver in\n");
    PRINTVALSTREAM(rawoutstream, "      turn and in the order specified above until one driver succeeds\n");
    PRINTVALSTREAM(rawoutstream, "      in opening the file.\n");
    PRINTVALSTREAM(rawoutstream,
                   "      See examples below for family, split, and multi driver special file name usage.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  F - is a filename.\n");
    PRINTVALSTREAM(rawoutstream, "  P - is the full path from the root group to the object.\n");
    PRINTVALSTREAM(rawoutstream, "  N - is an integer greater than 1.\n");
    PRINTVALSTREAM(rawoutstream, "  T - is a string containing the floating point format, e.g '%%.3f'\n");
    PRINTVALSTREAM(rawoutstream, "  U - is a URI reference (as defined in [IETF RFC 2396],\n");
    PRINTVALSTREAM(rawoutstream, "        updated by [IETF RFC 2732])\n");
    PRINTVALSTREAM(rawoutstream,
                   "  B - is the form of binary output: NATIVE for a memory type, FILE for the\n");
    PRINTVALSTREAM(rawoutstream,
                   "        file type, LE or BE for pre-existing little or big endian types.\n");
    PRINTVALSTREAM(rawoutstream, "        Must be used with -o (output file) and it is recommended that\n");
    PRINTVALSTREAM(rawoutstream,
                   "        -d (dataset) is used. B is an optional argument, defaults to NATIVE\n");
    PRINTVALSTREAM(rawoutstream,
                   "  Q - is the sort index type. It can be \"creation_order\" or \"name\" (default)\n");
    PRINTVALSTREAM(rawoutstream,
                   "  Z - is the sort order type. It can be \"descending\" or \"ascending\" (default)\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Examples ---------------\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  1) Attribute foo of the group /bar_none in file quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -a /bar_none/foo quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "     Attribute \"high/low\" of the group /bar_none in the file quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -a \"/bar_none/high\\/low\" quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  2) Selecting a subset from dataset /foo in file quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "      h5dump -d /foo -s \"0,1\" -S \"1,1\" -c \"2,3\" -k \"2,2\" quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  3) Saving dataset 'dset' in file quux.h5 to binary file 'out.bin'\n");
    PRINTVALSTREAM(rawoutstream, "        using a little-endian type\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /dset -b LE -o out.bin quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "  4) Display two packed bits (bits 0-1 and bits 4-6) in the dataset /dset\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /dset -M 0,1,4,3 quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  5) Dataset foo in files file1.h5 file2.h5 file3.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /foo file1.h5 file2.h5 file3.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  6) Dataset foo in split files splitfile-m.h5 splitfile-r.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /foo -f split splitfile\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(
        rawoutstream,
        "  7) Dataset foo in multi files mf-s.h5, mf-b.h5, mf-r.h5, mf-g.h5, mf-l.h5 and mf-o.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /foo -f multi mf\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream,
                   "  8) Dataset foo in family files fam00000.h5 fam00001.h5 and fam00002.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /foo -f family fam%%05d.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
}

/*-------------------------------------------------------------------------
 * Function:    table_list_add
 *
 * Purpose:     Add a new set of tables
 *
 * Return:      index of added table on success, -1 on failure
 *-------------------------------------------------------------------------
 */
ssize_t
table_list_add(hid_t oid, unsigned long file_no)
{
    size_t      idx; /* Index of table to use */
    find_objs_t info;

    /* Allocate space if necessary */
    if (table_list.nused == table_list.nalloc) {
        h5dump_table_items_t *tmp_ptr;

        table_list.nalloc = MAX(1, table_list.nalloc * 2);
        if (NULL == (tmp_ptr = (h5dump_table_items_t *)realloc(
                         table_list.tables, table_list.nalloc * sizeof(table_list.tables[0]))))
            return -1;
        table_list.tables = tmp_ptr;
    } /* end if */

    /* Append it */
    idx                           = table_list.nused++;
    table_list.tables[idx].fileno = file_no;
    table_list.tables[idx].oid    = oid;
    if (H5Iinc_ref(oid) < 0) {
        table_list.nused--;
        return -1;
    }
    if (init_objs(oid, &info, &table_list.tables[idx].group_table, &table_list.tables[idx].dset_table,
                  &table_list.tables[idx].type_table) < 0) {
        H5Idec_ref(oid);
        table_list.nused--;
        return -1;
    }

#ifdef H5DUMP_DEBUG
    dump_tables(&info);
#endif /* H5DUMP_DEBUG */

    return ((ssize_t)idx);
} /* end table_list_add() */

/*-------------------------------------------------------------------------
 * Function:    table_list_visited
 *
 * Purpose:     Check if a table already exists for the specified fileno
 *
 * Return:      The index of the matching table, or -1 if no matches found
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE ssize_t
table_list_visited(unsigned long file_no)
{
    size_t u; /* Local index variable */

    /* Look for table */
    for (u = 0; u < table_list.nused; u++)
        /* Check for fileno value already in array */
        if (table_list.tables[u].fileno == file_no)
            return ((ssize_t)u);

    /* Didn't find table */
    return (-1);
} /* end table_list_visited() */

/*-------------------------------------------------------------------------
 * Function:    table_list_free
 *
 * Purpose:     Frees the table list
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
table_list_free(void)
{
    size_t u; /* Local index variable */

    /* Iterate over tables */
    for (u = 0; u < table_list.nused; u++) {
        /* Release object id */
        if (H5Idec_ref(table_list.tables[u].oid) < 0)
            h5tools_setstatus(EXIT_FAILURE);

        /* Free each table */
        free_table(table_list.tables[u].group_table);
        free_table(table_list.tables[u].dset_table);
        free_table(table_list.tables[u].type_table);
    }

    /* Free the table list */
    free(table_list.tables);
    table_list.tables = NULL;
    table_list.nalloc = table_list.nused = 0;
} /* end table_list_free() */

/*-------------------------------------------------------------------------
 * Function:    set_binary_form
 *
 * Purpose:     set the binary form of output by translating from a string input
 *              parameter to a integer return value
 *
 * Return:      integer form of binary output or -1 if none found
 *-------------------------------------------------------------------------
 */
static int
set_binary_form(const char *form)
{
    int bform = -1;

    if (strcmp(form, "NATIVE") == 0 || strcmp(form, "MEMORY") == 0) {
        /* native form */
        bform = 0;
    }
    else if (strcmp(form, "FILE") == 0) /* file type form */
        bform = 1;
    else if (strcmp(form, "LE") == 0) /* convert to little endian */
        bform = 2;
    else if (strcmp(form, "BE") == 0) /* convert to big endian */
        bform = 3;

    return bform;
}

/*-------------------------------------------------------------------------
 * Function:    set_sort_by
 *
 * Purpose:     set the "by" form of sorting by translating from a string input
 *              parameter to a H5_index_t return value
 *              current sort values are [creation_order | name]
 *
 * Return:      H5_index_t form of sort or H5_INDEX_UNKNOWN if none found
 *-------------------------------------------------------------------------
 */
static H5_index_t
set_sort_by(const char *form)
{
    H5_index_t idx_type = H5_INDEX_UNKNOWN;

    if (strcmp(form, "name") == 0) /* H5_INDEX_NAME */
        idx_type = H5_INDEX_NAME;
    else if (strcmp(form, "creation_order") == 0) /* H5_INDEX_CRT_ORDER */
        idx_type = H5_INDEX_CRT_ORDER;

    return idx_type;
}

/*-------------------------------------------------------------------------
 * Function:    set_sort_order
 *
 * Purpose:     set the order of sorting by translating from a string input
 *              parameter to a H5_iter_order_t return value
 *              current order values are [ascending | descending ]
 *
 * Return:      H5_iter_order_t form of order or H5_ITER_UNKNOWN if none found
 *-------------------------------------------------------------------------
 */
static H5_iter_order_t
set_sort_order(const char *form)
{
    H5_iter_order_t iter_order = H5_ITER_UNKNOWN;

    if (strcmp(form, "ascending") == 0) /* H5_ITER_INC */
        iter_order = H5_ITER_INC;
    else if (strcmp(form, "descending") == 0) /* H5_ITER_DEC */
        iter_order = H5_ITER_DEC;

    return iter_order;
}

/*-------------------------------------------------------------------------
 * Function:    parse_mask_list
 *
 * Purpose:     Parse a list of comma or space separated integers and fill
 *              the packed_bits list and counter. The string being passed into this function
 *              should be at the start of the list you want to parse.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *-------------------------------------------------------------------------
 */
static int
parse_mask_list(const char *h_list)
{
    int                soffset_value;
    unsigned           offset_value;
    int                slength_value;
    unsigned           length_value;
    unsigned long long temp_mask;
    const char        *ptr = NULL;

    /* sanity check */
    if (h_list) {
        memset(packed_mask, 0, sizeof(packed_mask));

        packed_bits_num = 0;
        /* scan in pair of offset,length separated by commas. */
        ptr = h_list;
        while (*ptr) {
            /* scan for an offset which is an unsigned int */
            if (!isdigit(*ptr)) {
                error_msg("Bad mask list(%s)\n", h_list);
                return FAIL;
            }
            soffset_value = atoi(ptr);
            offset_value  = (unsigned)soffset_value;
            if (soffset_value < 0 || offset_value >= PACKED_BITS_SIZE_MAX) {
                error_msg("Packed Bit offset value(%d) must be between 0 and %u\n", soffset_value,
                          (unsigned)(PACKED_BITS_SIZE_MAX - 1));
                return FAIL;
            }

            /* skip to end of integer */
            while (isdigit(*++ptr))
                ;
            /* Look for the common separator */
            if (*ptr++ != ',') {
                error_msg("Bad mask list(%s), missing expected comma separator.\n", h_list);
                return FAIL;
            }

            /* scan for a length which is a positive int */
            if (!isdigit(*ptr)) {
                error_msg("Bad mask list(%s)\n", h_list);
                return FAIL;
            }
            slength_value = atoi(ptr);
            if (slength_value <= 0) {
                error_msg("Packed Bit length value(%d) must be positive.\n", slength_value);
                return FAIL;
            }
            length_value = (unsigned)slength_value;
            if ((offset_value + length_value) > PACKED_BITS_SIZE_MAX) {
                error_msg("Packed Bit offset+length value(%u) too large. Max is %u\n",
                          offset_value + length_value, (unsigned)PACKED_BITS_SIZE_MAX);
                return FAIL;
            }

            /* skip to end of int */
            while (isdigit(*++ptr))
                ;

            /* store the offset,length pair */
            if (packed_bits_num >= PACKED_BITS_MAX) {
                /* too many requests */
                error_msg("Too many masks requested (max. %d). Mask list(%s)\n", PACKED_BITS_MAX, h_list);
                return FAIL;
            }
            packed_offset[packed_bits_num] = offset_value;
            packed_length[packed_bits_num] = length_value;
            /* create the bit mask by left shift 1's by length, then negate it. */
            /* After packed_mask is calculated, packed_length is not needed but  */
            /* keep it for debug purpose. */
            temp_mask = ~0ULL;
            if (length_value < (int)(8 * sizeof(unsigned long long))) {
                temp_mask                    = temp_mask << length_value;
                packed_mask[packed_bits_num] = ~temp_mask;
            }
            else
                packed_mask[packed_bits_num] = temp_mask;
            packed_bits_num++;

            /* skip a possible comma separator */
            if (*ptr == ',') {
                if (!(*++ptr)) {
                    /* unexpected end of string */
                    error_msg("Bad mask list(%s), unexpected end of string.\n", h_list);
                    return FAIL;
                }
            }
        }
        if (packed_bits_num > PACKED_BITS_MAX) {
            error_msg("Maximum number of packed bits exceeded\n");
            return FAIL;
        }
        if (packed_bits_num == 0) {
            /* got no masks! */
            error_msg("Bad mask list(%s)\n", h_list);
            return FAIL;
        }
        return SUCCEED;
    }
    else {
        error_msg("Bad mask list argument\n");
        return FAIL;
    }
}

/*-------------------------------------------------------------------------
 * Function:    free_handler
 *
 * Purpose:     Convenience function to free the handler_t structures. Needs a
 *              length variable (LEN) to know how many in the array it needs
 *              to free
 *
 * Return:      Nothing
 *-------------------------------------------------------------------------
 */
static void
free_handler(struct handler_t *hand, int len)
{
    int i;

    if (hand) {
        for (i = 0; i < len; i++) {
            if (hand[i].obj) {
                free(hand[i].obj);
                hand[i].obj = NULL;
            }

            if (hand[i].subset_info) {
                if (hand[i].subset_info->start.data)
                    free(hand[i].subset_info->start.data);
                if (hand[i].subset_info->stride.data)
                    free(hand[i].subset_info->stride.data);
                if (hand[i].subset_info->count.data)
                    free(hand[i].subset_info->count.data);
                if (hand[i].subset_info->block.data)
                    free(hand[i].subset_info->block.data);

                free(hand[i].subset_info);
                hand[i].subset_info = NULL;
            }
        }

        free(hand);
    }
}

/*-------------------------------------------------------------------------
 * Function:    parse_command_line
 *
 * Purpose:     Parse the command line for the h5dumper.
 *
 * Return:      Success:    A pointer to an array of handler_t structures.
 *                          These contain all the information needed to dump
 *                          the necessary object.
 *              Failure:    Exits program with EXIT_FAILURE value.
 *-------------------------------------------------------------------------
 */
static struct handler_t *
parse_command_line(int argc, const char *const *argv)
{
    struct handler_t *hand      = NULL;
    struct handler_t *last_dset = NULL;
    int               i;
    int               opt;
    int               last_was_dset = false;

    /* no arguments */
    if (argc == 1) {
        usage(h5tools_getprogname());
        goto error;
    }

    /* this will be plenty big enough to hold the info */
    if ((hand = (struct handler_t *)calloc((size_t)argc, sizeof(struct handler_t))) == NULL) {
        goto error;
    }

    /* parse command line options */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
parse_start:
        switch ((char)opt) {
            case 'R':
                dump_opts.display_region = true;
                region_output            = true;
                break;
            case 'B':
                dump_opts.display_bb = true;
                last_was_dset        = false;
                break;
            case 'n':
                dump_opts.display_fi = true;
                last_was_dset        = false;
                if (H5_optarg != NULL)
                    h5trav_set_verbose(atoi(H5_optarg));
                break;
            case 'p':
                dump_opts.display_dcpl = true;
                break;
            case 'y':
                dump_opts.display_ai = false;
                break;
            case 'e':
                dump_opts.display_escape = true;
                break;
            case 'H':
                dump_opts.display_data      = false;
                dump_opts.display_attr_data = false;
                last_was_dset               = false;
                break;
            case 'A':
                if (H5_optarg != NULL) {
                    if (0 == atoi(H5_optarg))
                        dump_opts.include_attrs = false;
                }
                else {
                    dump_opts.display_data      = false;
                    dump_opts.display_attr_data = true;
                    last_was_dset               = false;
                }
                break;
            case 'i':
                dump_opts.display_oid = true;
                last_was_dset         = false;
                break;
            case 'r':
                dump_opts.display_char = true;
                break;
            case 'V':
                print_version(h5tools_getprogname());
                free_handler(hand, argc);
                hand = NULL;
                h5tools_setstatus(EXIT_SUCCESS);
                goto done;
                break;
            case 'w': {
                int sh5tools_nCols = atoi(H5_optarg);

                if (sh5tools_nCols <= 0)
                    h5tools_nCols = 65535;
                else
                    h5tools_nCols = (unsigned)sh5tools_nCols;
                last_was_dset = false;
            } break;
            case 'N':
                dump_opts.display_all = 0;

                for (i = 0; i < argc; i++)
                    if (!hand[i].func) {
                        hand[i].func = handle_paths;
                        hand[i].obj  = strdup(H5_optarg);
                        break;
                    }

                last_was_dset = false;
                break;
            case 'a':
                dump_opts.display_all = 0;

                for (i = 0; i < argc; i++)
                    if (!hand[i].func) {
                        hand[i].func = handle_attributes;
                        hand[i].obj  = strdup(H5_optarg);
                        break;
                    }

                last_was_dset = false;
                break;
            case 'd':
                dump_opts.display_all = 0;

                for (i = 0; i < argc; i++)
                    if (!hand[i].func) {
                        hand[i].func = handle_datasets;
                        hand[i].obj  = strdup(H5_optarg);
                        if (!dump_opts.disable_compact_subset)
                            hand[i].subset_info = parse_subset_params(hand[i].obj);
                        last_dset = &hand[i];
                        break;
                    }

                last_was_dset = true;
                break;
            case 'f':
                vfd_info_g.type   = VFD_BY_NAME;
                vfd_info_g.u.name = H5_optarg;
                use_custom_vfd_g  = true;

#ifdef H5_HAVE_ROS3_VFD
                if (0 == strcmp(vfd_info_g.u.name, drivernames[ROS3_VFD_IDX]))
                    if (!vfd_info_g.info)
                        vfd_info_g.info = &ros3_fa_g;
#endif
#ifdef H5_HAVE_LIBHDFS
                if (0 == strcmp(vfd_info_g.u.name, drivernames[HDFS_VFD_IDX]))
                    if (!vfd_info_g.info)
                        vfd_info_g.info = &hdfs_fa_g;
#endif

                break;
            case 'g':
                dump_opts.display_all = 0;

                for (i = 0; i < argc; i++)
                    if (!hand[i].func) {
                        hand[i].func = handle_groups;
                        hand[i].obj  = strdup(H5_optarg);
                        break;
                    }

                last_was_dset = false;
                break;
            case 'l':
                dump_opts.display_all = 0;

                for (i = 0; i < argc; i++)
                    if (!hand[i].func) {
                        hand[i].func = handle_links;
                        hand[i].obj  = strdup(H5_optarg);
                        break;
                    }

                last_was_dset = false;
                break;
            case 't':
                dump_opts.display_all = 0;

                for (i = 0; i < argc; i++)
                    if (!hand[i].func) {
                        hand[i].func = handle_datatypes;
                        hand[i].obj  = strdup(H5_optarg);
                        break;
                    }

                last_was_dset = false;
                break;

            case 'O':
                if (h5tools_set_output_file(H5_optarg, 0) < 0) {
                    usage(h5tools_getprogname());
                    goto error;
                }
                break;

            case 'o':
                if (bin_output) {
                    if (h5tools_set_data_output_file(H5_optarg, 1) < 0) {
                        usage(h5tools_getprogname());
                        goto error;
                    }
                }
                else {
                    if (dump_opts.display_attr_data && !dump_opts.display_data) {
                        if (h5tools_set_attr_output_file(H5_optarg, 0) < 0) {
                            usage(h5tools_getprogname());
                            goto error;
                        }
                    }
                    if (dump_opts.display_data || dump_opts.display_all) {
                        if (h5tools_set_data_output_file(H5_optarg, 0) < 0) {
                            usage(h5tools_getprogname());
                            goto error;
                        }
                    }
                }

                dump_opts.usingdasho = true;
                last_was_dset        = false;
                outfname_g           = H5_optarg;
                break;

            case 'b':
                if (H5_optarg != NULL) {
                    if ((bin_form = set_binary_form(H5_optarg)) < 0) {
                        /* failed to set binary form */
                        usage(h5tools_getprogname());
                        goto error;
                    }
                }
                bin_output = true;
                if (outfname_g != NULL) {
                    if (h5tools_set_data_output_file(outfname_g, 1) < 0) {
                        /* failed to set output file */
                        usage(h5tools_getprogname());
                        goto error;
                    }

                    last_was_dset = false;
                }
                break;

            case 'q':
                if ((sort_by = set_sort_by(H5_optarg)) < 0) {
                    /* failed to set "sort by" form */
                    usage(h5tools_getprogname());
                    goto error;
                }
                break;

            case 'z':
                if ((sort_order = set_sort_order(H5_optarg)) < 0) {
                    /* failed to set "sort order" form */
                    usage(h5tools_getprogname());
                    goto error;
                }
                break;

            case 'M':
                if (!last_was_dset) {
                    error_msg("option \"-%c\" can only be used after --dataset option\n", opt);
                    goto error;
                }
                if (parse_mask_list(H5_optarg) != SUCCEED) {
                    usage(h5tools_getprogname());
                    goto error;
                }
                dump_opts.display_packed_bits = true;
                break;
            case 'v':
                dump_opts.display_vds_first = true;
                break;
            case 'G':
                dump_opts.vds_gap_size = atoi(H5_optarg);
                if (dump_opts.vds_gap_size < 0) {
                    usage(h5tools_getprogname());
                    goto error;
                }
                break;

            /** begin XML parameters **/
            case 'x':
                /* select XML output */
                doxml_g                    = true;
                useschema_g                = true;
                h5tools_dump_header_format = NULL;
                dump_function_table        = &xml_function_table;
                h5tools_nCols              = 0;
                break;
            case 'u':
                doxml_g                    = true;
                useschema_g                = false;
                xmlnsprefix                = "";
                h5tools_dump_header_format = NULL;
                dump_function_table        = &xml_function_table;
                h5tools_nCols              = 0;
                break;
            case 'D':
                /* specify alternative XML DTD or schema */
                /* To Do: check format of this value?  */
                xml_dtd_uri_g = H5_optarg;
                h5tools_nCols = 0;
                break;

            case 'm':
                /* specify alternative floating point printing format */
                fp_format     = H5_optarg;
                h5tools_nCols = 0;
                break;

            case 'X':
                /* specify XML namespace (default="hdf5:"), or none */
                /* To Do: check format of this value?  */
                if (!useschema_g) {
                    usage(h5tools_getprogname());
                    goto error;
                }
                if (strcmp(H5_optarg, ":") == 0)
                    xmlnsprefix = "";
                else
                    xmlnsprefix = H5_optarg;
                h5tools_nCols = 0;
                break;
            /** end XML parameters **/

            /** begin subsetting parameters **/
            case 's':
            case 'S':
            case 'c':
            case 'k': {
                struct subset_t *s;

                if (!last_was_dset) {
                    error_msg("option \"-%c\" can only be used after --dataset option\n", opt);
                    goto error;
                }

                if (last_dset->subset_info) {
                    /*
                     * This overrides the "terse" syntax if they actually mixed
                     * the two.
                     */
                    s = last_dset->subset_info;
                }
                else {
                    last_dset->subset_info = s = (struct subset_t *)calloc(1, sizeof(struct subset_t));
                }

                /*
                 * slightly convoluted, but...we are only interested in options
                 * for subsetting: "--start", "--stride", "--count", and "--block"
                 * which can come in any order. If we run out of parameters (EOF)
                 * or run into one which isn't a subsetting parameter (NOT s, S,
                 * c, or K), then we exit the do-while loop, set the subset_info
                 * to the structure we've been filling. If we've reached the end
                 * of the options, we exit the parsing (goto parse_end) otherwise,
                 * since we've "read" the next option, we need to parse it. So we
                 * jump to the beginning of the switch statement (goto parse_start).
                 */
                do {
                    switch ((char)opt) {
                        case 's':
                            if (s->start.data) {
                                free(s->start.data);
                                s->start.data = NULL;
                            }
                            parse_hsize_list(H5_optarg, &s->start);
                            break;
                        case 'S':
                            if (s->stride.data) {
                                free(s->stride.data);
                                s->stride.data = NULL;
                            }
                            parse_hsize_list(H5_optarg, &s->stride);
                            break;
                        case 'c':
                            if (s->count.data) {
                                free(s->count.data);
                                s->count.data = NULL;
                            }
                            parse_hsize_list(H5_optarg, &s->count);
                            break;
                        case 'k':
                            if (s->block.data) {
                                free(s->block.data);
                                s->block.data = NULL;
                            }
                            parse_hsize_list(H5_optarg, &s->block);
                            break;
                        default:
                            goto end_collect;
                    }
                } while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF);

end_collect:
                last_was_dset = false;

                if (opt != EOF)
                    goto parse_start;
                else
                    goto parse_end;
            }
                /** end subsetting parameters **/

            case 'E':
                if (H5_optarg != NULL)
                    enable_error_stack = atoi(H5_optarg);
                else
                    enable_error_stack = 1;
                break;
            case 'C':
                dump_opts.disable_compact_subset = true;
                break;
            case 'h':
                usage(h5tools_getprogname());
                free_handler(hand, argc);
                hand = NULL;
                h5tools_setstatus(EXIT_SUCCESS);
                goto done;

            case '$':
#ifdef H5_HAVE_ROS3_VFD
                if (h5tools_parse_ros3_fapl_tuple(H5_optarg, ',', &ros3_fa_g) < 0) {
                    error_msg("failed to parse S3 VFD credential info\n");
                    usage(h5tools_getprogname());
                    free_handler(hand, argc);
                    hand = NULL;
                    h5tools_setstatus(EXIT_FAILURE);
                    goto done;
                }

                vfd_info_g.info = &ros3_fa_g;
#else
                error_msg(
                    "Read-Only S3 VFD is not available unless enabled when HDF5 is configured and built.\n");
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
#endif
                break;

            case '#':
#ifdef H5_HAVE_LIBHDFS
                if (h5tools_parse_hdfs_fapl_tuple(H5_optarg, ',', &hdfs_fa_g) < 0) {
                    error_msg("failed to parse HDFS VFD configuration info\n");
                    usage(h5tools_getprogname());
                    free_handler(hand, argc);
                    hand = NULL;
                    h5tools_setstatus(EXIT_FAILURE);
                    goto done;
                }

                vfd_info_g.info = &hdfs_fa_g;
#else
                error_msg("HDFS VFD is not available unless enabled when HDF5 is configured and built.\n");
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
#endif
                break;

            case '1':
                vol_info_g.type    = VOL_BY_VALUE;
                vol_info_g.u.value = (H5VL_class_value_t)atoi(H5_optarg);
                use_custom_vol_g   = true;
                break;

            case '2':
                vol_info_g.type   = VOL_BY_NAME;
                vol_info_g.u.name = H5_optarg;
                use_custom_vol_g  = true;
                break;

            case '3':
                vol_info_g.info_string = H5_optarg;
                break;

            case '4':
                vfd_info_g.type    = VFD_BY_VALUE;
                vfd_info_g.u.value = (H5FD_class_value_t)atoi(H5_optarg);
                use_custom_vfd_g   = true;
                break;

            case '5':
                vfd_info_g.type   = VFD_BY_NAME;
                vfd_info_g.u.name = H5_optarg;
                use_custom_vfd_g  = true;
                break;

            case '6':
                vfd_info_g.info = (const void *)H5_optarg;
                break;

            case '?':
            default:
                usage(h5tools_getprogname());
                goto error;
        }
    }

    /* If the file uses the onion VFD, get the revision number */
    if (vfd_info_g.type == VFD_BY_NAME && vfd_info_g.u.name && !strcmp(vfd_info_g.u.name, "onion")) {

        if (vfd_info_g.info) {
            if (!strcmp(vfd_info_g.info, "revision_count"))
                get_onion_revision_count = true;
            else {
                errno                   = 0;
                onion_fa_g.revision_num = strtoull(vfd_info_g.info, NULL, 10);
                if (errno == ERANGE) {
                    printf("Invalid onion revision specified\n");
                    goto error;
                }

                printf("Using revision %" PRIu64 "\n", onion_fa_g.revision_num);
            }
        }
        else
            onion_fa_g.revision_num = 0;

        vfd_info_g.info = &onion_fa_g;
    }

parse_end:
    /* check for file name to be processed */
    if (argc <= H5_optind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        goto error;
    }
done:
    return hand;

error:
    if (hand) {
        free_handler(hand, argc);
        hand = NULL;
    }
    h5tools_setstatus(EXIT_FAILURE);

    return hand;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 dumper
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t             fid     = H5I_INVALID_HID;
    hid_t             gid     = H5I_INVALID_HID;
    hid_t             fapl_id = H5P_DEFAULT;
    H5O_info2_t       oi;
    struct handler_t *hand = NULL;
    int               i;
    unsigned          u;
    char             *fname = NULL;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);
    h5tools_dump_header_format = &h5tools_standardformat;
    dump_function_table        = &ddl_function_table;
    dump_indent                = 0;

    /* Initialize h5tools lib */
    h5tools_init();

    if ((hand = parse_command_line(argc, (const char *const *)argv)) == NULL) {
        goto done;
    }

    if (bin_output && outfname_g == NULL) {
        error_msg("binary output requires a file name, use -o <filename>\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Check for conflicting options */
    if (doxml_g) {
        if (!dump_opts.display_all) {
            error_msg("option \"%s\" not available for XML\n", "to display selected objects");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (dump_opts.display_bb) {
            error_msg("option \"%s\" not available for XML\n", "--boot-block");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (dump_opts.display_oid == 1) {
            error_msg("option \"%s\" not available for XML\n", "--object-ids");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (dump_opts.display_char == true) {
            error_msg("option \"%s\" not available for XML\n", "--string");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (dump_opts.usingdasho) {
            error_msg("option \"%s\" not available for XML\n", "--output");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }
    else {
        if (xml_dtd_uri_g) {
            warn_msg("option \"%s\" only applies with XML: %s\n", "--xml-dtd", xml_dtd_uri_g);
        }
    }

    if (argc <= H5_optind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Enable error reporting if --enable-error-stack command line option is specified */
    h5tools_error_report();

    /* Initialize indexing options */
    h5trav_set_index(sort_by, sort_order);

    if (use_custom_vol_g || use_custom_vfd_g) {
        if ((fapl_id = h5tools_get_fapl(H5P_DEFAULT, use_custom_vol_g ? &vol_info_g : NULL,
                                        use_custom_vfd_g ? &vfd_info_g : NULL)) < 0) {
            error_msg("unable to create FAPL for file access\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }

    while (H5_optind < argc) {
        fname = strdup(argv[H5_optind++]);

        /* A short cut to get the revision count of an onion file without opening the file */
        if (get_onion_revision_count && H5FD_ONION == H5Pget_driver(fapl_id)) {
            uint64_t revision_count = 0;

            if (H5FDonion_get_revision_count(fname, fapl_id, &revision_count) < 0) {
                error_msg("unable to create FAPL for file access\n");
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            }

            printf("The number of revisions for the onion file is %" PRIu64 "\n", revision_count);
            goto done;
        }
        else
            fid = h5tools_fopen(fname, H5F_ACC_RDONLY, fapl_id, (fapl_id != H5P_DEFAULT), NULL, 0);

        if (fid < 0) {
            error_msg("unable to open file \"%s\"\n", fname);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        /* allocate and initialize internal data structure */
        init_prefix(&prefix, prefix_len);

        /* Prepare to find objects that might be targets of a reference */
        fill_ref_path_table(fid);

        if (doxml_g) {
            /* initialize XML */
            /* reset prefix! */
            strcpy(prefix, "");

            /* make sure the URI is initialized to something */
            if (xml_dtd_uri_g == NULL) {
                if (useschema_g) {
                    xml_dtd_uri_g = DEFAULT_XSD;
                }
                else {
                    xml_dtd_uri_g = DEFAULT_DTD;
                    xmlnsprefix   = "";
                }
            }
            else {
                if (useschema_g && strcmp(xmlnsprefix, "") != 0) {
                    error_msg(
                        "Cannot set Schema URL for a qualified namespace--use -X or -U option with -D \n");
                    h5tools_setstatus(EXIT_FAILURE);
                    goto done;
                }
            }
        }

        /* Get object info for root group */
        if (H5Oget_info_by_name3(fid, "/", &oi, H5O_INFO_BASIC, H5P_DEFAULT) < 0) {
            error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        /* Initialize object tables */
        if (table_list_add(fid, oi.fileno) < 0) {
            error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        group_table = table_list.tables[0].group_table;
        dset_table  = table_list.tables[0].dset_table;
        type_table  = table_list.tables[0].type_table;

        /* does there exist unnamed committed datatype */
        for (u = 0; u < type_table->nobjs; u++)
            if (!type_table->objs[u].recorded) {
                unamedtype = 1;
                break;
            } /* end if */

        /* start to dump - display file header information */
        if (!doxml_g) {
            begin_obj(h5tools_dump_header_format->filebegin, fname,
                      h5tools_dump_header_format->fileblockbegin);
        }
        else {
            PRINTVALSTREAM(rawoutstream, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

            /* alternative first element, depending on schema or DTD. */
            if (useschema_g) {
                if (strcmp(xmlnsprefix, "") == 0) {
                    PRINTSTREAM(rawoutstream,
                                "<HDF5-File xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
                                "xsi:noNamespaceSchemaLocation=\"%s\">\n",
                                xml_dtd_uri_g);
                }
                else {
                    /*  TODO: make -url option work in this case (may need new option) */
                    char *ns;
                    char *indx;

                    ns   = strdup(xmlnsprefix);
                    indx = strrchr(ns, (int)':');
                    if (indx)
                        *indx = '\0';

                    PRINTSTREAM(rawoutstream,
                                "<%sHDF5-File xmlns:%s=\"http://hdfgroup.org/HDF5/XML/schema/HDF5-File.xsd\" "
                                "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
                                "xsi:schemaLocation=\"http://hdfgroup.org/HDF5/XML/schema/HDF5-File "
                                "http://www.hdfgroup.org/HDF5/XML/schema/HDF5-File.xsd\">\n",
                                xmlnsprefix, ns);
                    free(ns);
                }
            }
            else {
                PRINTSTREAM(rawoutstream, "<!DOCTYPE HDF5-File PUBLIC \"HDF5-File.dtd\" \"%s\">\n",
                            xml_dtd_uri_g);
                PRINTVALSTREAM(rawoutstream, "<HDF5-File>\n");
            }
        }

        if (!doxml_g) {
            if (dump_opts.display_fi) {
                PRINTVALSTREAM(rawoutstream, "\n");
                dump_fcontents(fid);
                end_obj(h5tools_dump_header_format->fileend, h5tools_dump_header_format->fileblockend);
                PRINTVALSTREAM(rawoutstream, "\n");
                goto done;
            }

            if (dump_opts.display_bb)
                dump_fcpl(fid);
        }

        if (dump_opts.display_all) {
            if ((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) {
                error_msg("unable to open root group\n");
                h5tools_setstatus(EXIT_FAILURE);
            }
            else {
                if (!doxml_g)
                    dump_indent += COL;
                dump_function_table->dump_group_function(gid, "/");
                if (!doxml_g)
                    dump_indent -= COL;
                PRINTVALSTREAM(rawoutstream, "\n");
            }

            if (H5Gclose(gid) < 0) {
                error_msg("unable to close root group\n");
                h5tools_setstatus(EXIT_FAILURE);
            }
        }
        else {
            /* Note: this option is not supported for XML */
            if (doxml_g) {
                error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            } /* end if */

            for (i = 0; i < argc; i++) {
                if (hand[i].func) {
                    hand[i].func(fid, hand[i].obj, hand[i].subset_info, 1, NULL);
                }
            }
            PRINTVALSTREAM(rawoutstream, "\n");
        }

        if (!doxml_g) {
            end_obj(h5tools_dump_header_format->fileend, h5tools_dump_header_format->fileblockend);
            PRINTVALSTREAM(rawoutstream, "\n");
        }
        else {
            PRINTSTREAM(rawoutstream, "</%sHDF5-File>\n", xmlnsprefix);
        }
        /* Free tables for objects */
        table_list_free();

        if (fid >= 0)
            if (H5Fclose(fid) < 0)
                h5tools_setstatus(EXIT_FAILURE);

        if (prefix) {
            free(prefix);
            prefix = NULL;
        }
        if (fname) {
            free(fname);
            fname = NULL;
        }
    } /* end while */

    if (hand)
        free_handler(hand, argc);

    /* To Do:  clean up XML table */

    leave(h5tools_getstatus());

done:
    /* Free tables for objects */
    table_list_free();

    if (fapl_id != H5P_DEFAULT && 0 < H5Pclose(fapl_id)) {
        error_msg("Can't close fapl entry\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    if (fid >= 0)
        if (H5Fclose(fid) < 0)
            h5tools_setstatus(EXIT_FAILURE);

    if (prefix) {
        free(prefix);
        prefix = NULL;
    }
    if (fname) {
        free(fname);
        fname = NULL;
    }

    if (hand)
        free_handler(hand, argc);

    /* To Do:  clean up XML table */

    leave(h5tools_getstatus());
} /* main */

/*-------------------------------------------------------------------------
 * Function:    init_prefix
 *
 * Purpose:     allocate and initialize prefix
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
init_prefix(char **prfx, size_t prfx_len)
{
    if (prfx_len > 0)
        *prfx = (char *)calloc(prfx_len, 1);
    else
        error_msg("unable to allocate prefix buffer\n");
}

/*-------------------------------------------------------------------------
 * Function:    add_prefix
 *
 * Purpose:     Add object to prefix
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
add_prefix(char **prfx, size_t *prfx_len, const char *name)
{
    size_t new_len = strlen(*prfx) + strlen(name) + 2;

    /* Check if we need more space */
    if (*prfx_len <= new_len) {
        *prfx_len = new_len + 1;
        *prfx     = (char *)realloc(*prfx, *prfx_len);
    }

    /* Append object name to prefix */
    strcat(strcat(*prfx, "/"), name);
} /* end add_prefix */
