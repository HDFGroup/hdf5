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

#include "h5dump.h"
#include "h5dump_ddl.h"
#include "h5dump_xml.h"

/* Name of tool */
#define PROGRAMNAME "h5dump"

static const char   *driver = NULL;      /* The driver to open the file with. */
const char          *outfname=NULL;
static int           doxml = 0;
static int           useschema = 1;
static const char   *xml_dtd_uri = NULL;

#ifdef H5_HAVE_ROS3_VFD
static H5FD_ros3_fapl_t ros3_fa = {
    1,     /* version           */
    false, /* authenticate      */
    "",    /* aws region        */
    "",    /* access key id     */
    "",    /* secret access key */
};
#endif /* H5_HAVE_ROS3_VFD */

#ifdef H5_HAVE_LIBHDFS
static H5FD_hdfs_fapl_t hdfs_fa = {
    1,           /* fapl version          */
    "localhost", /* namenode name         */
    0,           /* namenode port         */
    "",          /* kerberos ticket cache */
    "",          /* user name             */
    2048,        /* stream buffer size    */
};
#endif /* H5_HAVE_LIBHDFS */

/* module-scoped variables for XML option */
#define DEFAULT_XSD     "http://www.hdfgroup.org/HDF5/XML/schema/HDF5-File.xsd"
#define DEFAULT_DTD     "http://www.hdfgroup.org/HDF5/XML/DTD/HDF5-File.dtd"

/* Standard DDL output */
static const dump_functions ddl_function_table = {
    dump_group,
    dump_named_datatype,
    dump_dataset,
    dump_dataspace,
    dump_datatype,
    dump_attr_cb,
    dump_data
};

/* XML output */
static const dump_functions xml_function_table = {
    xml_dump_group,
    xml_dump_named_datatype,
    xml_dump_dataset,
    xml_dump_dataspace,
    xml_dump_datatype,
    xml_dump_attr,
    xml_dump_data
};

/* internal functions */
static void     init_prefix(char **prfx, size_t prfx_len);

/* a structure for handling the order command-line parameters come in */
struct handler_t {
    void (*func)(hid_t, const char *, void *, int, const char *);
    char *obj;
    struct subset_t *subset_info;
};

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
/* The following initialization makes use of C language concatenating */
/* "xxx" "yyy" into "xxxyyy". */
static const char *s_opts = "hn*peyBHirVa:c:d:f:g:k:l:t:w:xD:uX:o*b*F:s:S:A*q:z:m:RE*CM:O*N:vG:";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "hel", no_arg, 'h' },
    { "contents", optional_arg, 'n' },
    { "properties", no_arg, 'p' },
    { "superblock", no_arg, 'B' },
    { "boot-block", no_arg, 'B' },
    { "boot-bloc", no_arg, 'B' },
    { "boot-blo", no_arg, 'B' },
    { "boot-bl", no_arg, 'B' },
    { "boot-b", no_arg, 'B' },
    { "boot", no_arg, 'B' },
    { "boo", no_arg, 'B' },
    { "bo", no_arg, 'B' },
    { "header", no_arg, 'H' },
    { "heade", no_arg, 'H' },
    { "head", no_arg, 'H' },
    { "hea", no_arg, 'H' },
    { "object-ids", no_arg, 'i' },
    { "object-id", no_arg, 'i' },
    { "object-i", no_arg, 'i' },
    { "object", no_arg, 'i' },
    { "objec", no_arg, 'i' },
    { "obje", no_arg, 'i' },
    { "obj", no_arg, 'i' },
    { "ob", no_arg, 'i' },
    { "version", no_arg, 'V' },
    { "versio", no_arg, 'V' },
    { "versi", no_arg, 'V' },
    { "vers", no_arg, 'V' },
    { "ver", no_arg, 'V' },
    { "ve", no_arg, 'V' },
    { "attribute", require_arg, 'a' },
    { "attribut", require_arg, 'a' },
    { "attribu", require_arg, 'a' },
    { "attrib", require_arg, 'a' },
    { "attri", require_arg, 'a' },
    { "attr", require_arg, 'a' },
    { "att", require_arg, 'a' },
    { "at", require_arg, 'a' },
    { "block", require_arg, 'k' },
    { "bloc", require_arg, 'k' },
    { "blo", require_arg, 'k' },
    { "bl", require_arg, 'k' },
    { "count", require_arg, 'c' },
    { "coun", require_arg, 'c' },
    { "cou", require_arg, 'c' },
    { "co", require_arg, 'c' },
    { "dataset", require_arg, 'd' },
    { "datase", require_arg, 'd' },
    { "datas", require_arg, 'd' },
    { "datatype", require_arg, 't' },
    { "datatyp", require_arg, 't' },
    { "dataty", require_arg, 't' },
    { "datat", require_arg, 't' },
    { "filedriver", require_arg, 'f' },
    { "filedrive", require_arg, 'f' },
    { "filedriv", require_arg, 'f' },
    { "filedri", require_arg, 'f' },
    { "filedr", require_arg, 'f' },
    { "filed", require_arg, 'f' },
    { "file", require_arg, 'f' },
    { "fil", require_arg, 'f' },
    { "fi", require_arg, 'f' },
    { "group", require_arg, 'g' },
    { "grou", require_arg, 'g' },
    { "gro", require_arg, 'g' },
    { "gr", require_arg, 'g' },
    { "output", optional_arg, 'o' },
    { "outpu", optional_arg, 'o' },
    { "outp", optional_arg, 'o' },
    { "out", optional_arg, 'o' },
    { "ou", optional_arg, 'o' },
    { "soft-link", require_arg, 'l' },
    { "soft-lin", require_arg, 'l' },
    { "soft-li", require_arg, 'l' },
    { "soft-l", require_arg, 'l' },
    { "soft", require_arg, 'l' },
    { "sof", require_arg, 'l' },
    { "start", require_arg, 's' },
    { "star", require_arg, 's' },
    { "sta", require_arg, 's' },
    { "stride", require_arg, 'S' },
    { "strid", require_arg, 'S' },
    { "string", no_arg, 'r' },
    { "strin", no_arg, 'r' },
    { "use-dtd", no_arg, 'u' },
    { "use-dt", no_arg, 'u' },
    { "use-d", no_arg, 'u' },
    { "use-", no_arg, 'u' },
    { "use", no_arg, 'u' },
    { "us", no_arg, 'u' },
    { "u", no_arg, 'u' },
    { "width", require_arg, 'w' },
    { "widt", require_arg, 'w' },
    { "wid", require_arg, 'w' },
    { "wi", require_arg, 'w' },
    { "xml-dtd", require_arg, 'D' },
    { "xml-dt", require_arg, 'D' },
    { "xml-d", require_arg, 'D' },
    { "xml-ns", require_arg, 'X' },
    { "xml-n", require_arg, 'X' },
    { "xml", no_arg, 'x' },
    { "xm", no_arg, 'x' },
    { "onlyattr", optional_arg, 'A' },
    { "escape", no_arg, 'e' },
    { "noindex", no_arg, 'y' },
    { "binary", optional_arg, 'b' },
    { "form", require_arg, 'F' },
    { "sort_by", require_arg, 'q' },
    { "sort_order", require_arg, 'z' },
    { "format", require_arg, 'm' },
    { "region", no_arg, 'R' },
    { "enable-error-stack", optional_arg, 'E' },
    { "packed-bits", require_arg, 'M' },
    { "no-compact-subset", no_arg, 'C' },
    { "ddl", optional_arg, 'O' },
    { "any_path", require_arg, 'N' },
    { "vds-view-first-missing", no_arg, 'v' },
    { "vds-gap-size", require_arg, 'G' },
    { "s3-cred", require_arg, '$' },
    { "hdfs-attrs", require_arg, '#' },
    { NULL, 0, '\0' }
};


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

    HDexit(ret);
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
    PRINTVALSTREAM(rawoutstream, "--------------- File Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -n,   --contents     Print a list of the file contents and exit\n");
    PRINTVALSTREAM(rawoutstream, "                          Optional value 1 also prints attributes.\n");
    PRINTVALSTREAM(rawoutstream, "     -B,   --superblock   Print the content of the super block\n");
    PRINTVALSTREAM(rawoutstream, "     -H,   --header       Print the header only; no data is displayed\n");
    PRINTVALSTREAM(rawoutstream, "     -f D, --filedriver=D Specify which driver to open the file with\n");
    PRINTVALSTREAM(rawoutstream, "     -o F, --output=F     Output raw data into file F\n");
    PRINTVALSTREAM(rawoutstream, "     -b B, --binary=B     Binary file output, of form B\n");
    PRINTVALSTREAM(rawoutstream, "     -O F, --ddl=F        Output ddl text into file F\n");
    PRINTVALSTREAM(rawoutstream, "                          Use blank(empty) filename F to suppress ddl display\n");
    PRINTVALSTREAM(rawoutstream, "     --s3-cred=<cred>     Supply S3 authentication information to \"ros3\" vfd.\n");
    PRINTVALSTREAM(rawoutstream, "                          <cred> :: \"(<aws-region>,<access-id>,<access-key>)\"\n");
    PRINTVALSTREAM(rawoutstream, "                          If absent or <cred> -> \"(,,)\", no authentication.\n");
    PRINTVALSTREAM(rawoutstream, "                          Has no effect is filedriver is not `ros3'.\n");
    PRINTVALSTREAM(rawoutstream, "     --hdfs-attrs=<attrs> Supply configuration information for HDFS file access.\n");
    PRINTVALSTREAM(rawoutstream, "                          For use with \"--filedriver=hdfs\"\n");
    PRINTVALSTREAM(rawoutstream, "                          <attrs> :: (<namenode name>,<namenode port>,\n");
    PRINTVALSTREAM(rawoutstream, "                                      <kerberos cache path>,<username>,\n");
    PRINTVALSTREAM(rawoutstream, "                                      <buffer size>)\n");
    PRINTVALSTREAM(rawoutstream, "                          Any absent attribute will use a default value.\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Object Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -a P, --attribute=P  Print the specified attribute\n");
    PRINTVALSTREAM(rawoutstream, "                          If an attribute name contains a slash (/), escape the\n");
    PRINTVALSTREAM(rawoutstream, "                          slash with a preceding backslash (\\).\n");
    PRINTVALSTREAM(rawoutstream, "                          (See example section below.)\n");
    PRINTVALSTREAM(rawoutstream, "     -d P, --dataset=P    Print the specified dataset\n");
    PRINTVALSTREAM(rawoutstream, "     -g P, --group=P      Print the specified group and all members\n");
    PRINTVALSTREAM(rawoutstream, "     -l P, --soft-link=P  Print the value(s) of the specified soft link\n");
    PRINTVALSTREAM(rawoutstream, "     -t P, --datatype=P   Print the specified named datatype\n");
    PRINTVALSTREAM(rawoutstream, "     -N P, --any_path=P   Print any attribute, dataset, group, datatype, or link that matches P\n");
    PRINTVALSTREAM(rawoutstream, "                          P can be the absolute path or just a relative path.\n");
    PRINTVALSTREAM(rawoutstream, "     -A,   --onlyattr     Print the header and value of attributes\n");
    PRINTVALSTREAM(rawoutstream, "                          Optional value 0 suppresses printing attributes.\n");
    PRINTVALSTREAM(rawoutstream, "     --vds-view-first-missing Set the VDS bounds to first missing mapped elements.\n");
    PRINTVALSTREAM(rawoutstream, "     --vds-gap-size=N     Set the missing file gap size, N=non-negative integers\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Object Property Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -i,   --object-ids   Print the object ids\n");
    PRINTVALSTREAM(rawoutstream, "     -p,   --properties   Print dataset filters, storage layout and fill value\n");
    PRINTVALSTREAM(rawoutstream, "     -M L, --packedbits=L Print packed bits as unsigned integers, using mask\n");
    PRINTVALSTREAM(rawoutstream, "                          format L for an integer dataset specified with\n");
    PRINTVALSTREAM(rawoutstream, "                          option -d. L is a list of offset,length values,\n");
    PRINTVALSTREAM(rawoutstream, "                          separated by commas. Offset is the beginning bit in\n");
    PRINTVALSTREAM(rawoutstream, "                          the data value and length is the number of bits of\n");
    PRINTVALSTREAM(rawoutstream, "                          the mask.\n");
    PRINTVALSTREAM(rawoutstream, "     -R,   --region       Print dataset pointed by region references\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Formatting Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -e,   --escape       Escape non printing characters\n");
    PRINTVALSTREAM(rawoutstream, "     -r,   --string       Print 1-byte integer datasets as ASCII\n");
    PRINTVALSTREAM(rawoutstream, "     -y,   --noindex      Do not print array indices with the data\n");
    PRINTVALSTREAM(rawoutstream, "     -m T, --format=T     Set the floating point output format\n");
    PRINTVALSTREAM(rawoutstream, "     -q Q, --sort_by=Q    Sort groups and attributes by index Q\n");
    PRINTVALSTREAM(rawoutstream, "     -z Z, --sort_order=Z Sort groups and attributes by order Z\n");
    PRINTVALSTREAM(rawoutstream, "     --enable-error-stack Prints messages from the HDF5 error stack as they occur.\n");
    PRINTVALSTREAM(rawoutstream, "                          Optional value 2 also prints file open errors.\n");
    PRINTVALSTREAM(rawoutstream, "     --no-compact-subset  Disable compact form of subsetting and allow the use\n");
    PRINTVALSTREAM(rawoutstream, "                          of \"[\" in dataset names.\n");
    PRINTVALSTREAM(rawoutstream, "     -w N, --width=N      Set the number of columns of output. A value of 0 (zero)\n");
    PRINTVALSTREAM(rawoutstream, "                          sets the number of columns to the maximum (65535).\n");
    PRINTVALSTREAM(rawoutstream, "                          Default width is 80 columns.\n");
    PRINTVALSTREAM(rawoutstream, "--------------- XML Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, "     -x,   --xml          Output in XML using Schema\n");
    PRINTVALSTREAM(rawoutstream, "     -u,   --use-dtd      Output in XML using DTD\n");
    PRINTVALSTREAM(rawoutstream, "     -D U, --xml-dtd=U    Use the DTD or schema at U\n");
    PRINTVALSTREAM(rawoutstream, "     -X S, --xml-ns=S     (XML Schema) Use qualified names n the XML\n");
    PRINTVALSTREAM(rawoutstream, "                          \":\": no namespace, default: \"hdf5:\"\n");
    PRINTVALSTREAM(rawoutstream, "                          E.g., to dump a file called `-f', use h5dump -- -f\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Subsetting Options ---------------\n");
    PRINTVALSTREAM(rawoutstream, " Subsetting is available by using the following options with a dataset\n");
    PRINTVALSTREAM(rawoutstream, " option. Subsetting is done by selecting a hyperslab from the data.\n");
    PRINTVALSTREAM(rawoutstream, " Thus, the options mirror those for performing a hyperslab selection.\n");
    PRINTVALSTREAM(rawoutstream, " One of the START, COUNT, STRIDE, or BLOCK parameters are mandatory if you do subsetting.\n");
    PRINTVALSTREAM(rawoutstream, " The STRIDE, COUNT, and BLOCK parameters are optional and will default to 1 in\n");
    PRINTVALSTREAM(rawoutstream, " each dimension. START is optional and will default to 0 in each dimension.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      -s START,  --start=START    Offset of start of subsetting selection\n");
    PRINTVALSTREAM(rawoutstream, "      -S STRIDE, --stride=STRIDE  Hyperslab stride\n");
    PRINTVALSTREAM(rawoutstream, "      -c COUNT,  --count=COUNT    Number of blocks to include in selection\n");
    PRINTVALSTREAM(rawoutstream, "      -k BLOCK,  --block=BLOCK    Size of block in hyperslab\n");
    PRINTVALSTREAM(rawoutstream, "  START, COUNT, STRIDE, and BLOCK - is a list of integers the number of which are equal to the\n");
    PRINTVALSTREAM(rawoutstream, "      number of dimensions in the dataspace being queried\n");
    PRINTVALSTREAM(rawoutstream, "      (Alternate compact form of subsetting is described in the Reference Manual)\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "--------------- Option Argument Conventions ---------------\n");
    PRINTVALSTREAM(rawoutstream, "  D - is the file driver to use in opening the file. Acceptable values\n");
    PRINTVALSTREAM(rawoutstream, "      are \"sec2\", \"family\", \"split\", \"multi\", \"direct\", and \"stream\". Without\n");
    PRINTVALSTREAM(rawoutstream, "      the file driver flag, the file will be opened with each driver in\n");
    PRINTVALSTREAM(rawoutstream, "      turn and in the order specified above until one driver succeeds\n");
    PRINTVALSTREAM(rawoutstream, "      in opening the file.\n");
    PRINTVALSTREAM(rawoutstream, "      See examples below for family, split, and multi driver special file name usage.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  F - is a filename.\n");
    PRINTVALSTREAM(rawoutstream, "  P - is the full path from the root group to the object.\n");
    PRINTVALSTREAM(rawoutstream, "  N - is an integer greater than 1.\n");
    PRINTVALSTREAM(rawoutstream, "  T - is a string containing the floating point format, e.g '%%.3f'\n");
    PRINTVALSTREAM(rawoutstream, "  U - is a URI reference (as defined in [IETF RFC 2396],\n");
    PRINTVALSTREAM(rawoutstream, "        updated by [IETF RFC 2732])\n");
    PRINTVALSTREAM(rawoutstream, "  B - is the form of binary output: NATIVE for a memory type, FILE for the\n");
    PRINTVALSTREAM(rawoutstream, "        file type, LE or BE for pre-existing little or big endian types.\n");
    PRINTVALSTREAM(rawoutstream, "        Must be used with -o (output file) and it is recommended that\n");
    PRINTVALSTREAM(rawoutstream, "        -d (dataset) is used. B is an optional argument, defaults to NATIVE\n");
    PRINTVALSTREAM(rawoutstream, "  Q - is the sort index type. It can be \"creation_order\" or \"name\" (default)\n");
    PRINTVALSTREAM(rawoutstream, "  Z - is the sort order type. It can be \"descending\" or \"ascending\" (default)\n");
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
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /foo -s \"0,1\" -S \"1,1\" -c \"2,3\" -k \"2,2\" quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  3) Saving dataset 'dset' in file quux.h5 to binary file 'out.bin'\n");
    PRINTVALSTREAM(rawoutstream, "        using a little-endian type\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /dset -b LE -o out.bin quux.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  4) Display two packed bits (bits 0-1 and bits 4-6) in the dataset /dset\n");
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
    PRINTVALSTREAM(rawoutstream, "  7) Dataset foo in multi files mf-s.h5, mf-b.h5, mf-r.h5, mf-g.h5, mf-l.h5 and mf-o.h5\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "      h5dump -d /foo -f multi mf\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  8) Dataset foo in family files fam00000.h5 fam00001.h5 and fam00002.h5\n");
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
    size_t      idx;         /* Index of table to use */
    find_objs_t info;

    /* Allocate space if necessary */
    if(table_list.nused == table_list.nalloc) {
        h5dump_table_items_t    *tmp_ptr;

        table_list.nalloc = MAX(1, table_list.nalloc * 2);
        if(NULL == (tmp_ptr = (h5dump_table_items_t *)HDrealloc(table_list.tables, table_list.nalloc * sizeof(table_list.tables[0]))))
            return -1;
        table_list.tables = tmp_ptr;
    } /* end if */

    /* Append it */
    idx = table_list.nused++;
    table_list.tables[idx].fileno = file_no;
    table_list.tables[idx].oid = oid;
    if(H5Iinc_ref(oid) < 0) {
        table_list.nused--;
        return -1;
    }
    if(init_objs(oid, &info, &table_list.tables[idx].group_table, &table_list.tables[idx].dset_table, &table_list.tables[idx].type_table) < 0) {
        H5Idec_ref(oid);
        table_list.nused--;
        return -1;
    }

#ifdef H5DUMP_DEBUG
    dump_tables(&info);
#endif /* H5DUMP_DEBUG */

    return((ssize_t) idx);
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
    size_t u;           /* Local index variable */

    /* Look for table */
    for(u = 0; u < table_list.nused; u++)
        /* Check for fileno value already in array */
        if(table_list.tables[u].fileno == file_no)
            return((ssize_t) u);

    /* Didn't find table */
    return(-1);
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
    size_t u;           /* Local index variable */

    /* Iterate over tables */
    for(u = 0; u < table_list.nused; u++) {
        /* Release object id */
        if(H5Idec_ref(table_list.tables[u].oid) < 0)
            h5tools_setstatus(EXIT_FAILURE);

        /* Free each table */
        free_table(table_list.tables[u].group_table);
        HDfree(table_list.tables[u].group_table);
        free_table(table_list.tables[u].dset_table);
        HDfree(table_list.tables[u].dset_table);
        free_table(table_list.tables[u].type_table);
        HDfree(table_list.tables[u].type_table);
    }

    /* Free the table list */
    HDfree(table_list.tables);
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

    if (HDstrcmp(form,"NATIVE") == 0 || HDstrcmp(form,"MEMORY") == 0) {
        /* native form */
        bform = 0;
    }
    else if (HDstrcmp(form,"FILE") == 0) /* file type form */
        bform = 1;
    else if (HDstrcmp(form,"LE") == 0) /* convert to little endian */
        bform = 2;
    else if (HDstrcmp(form,"BE") == 0) /* convert to big endian */
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

    if (HDstrcmp(form,"name")==0) /* H5_INDEX_NAME */
        idx_type = H5_INDEX_NAME;
    else if (HDstrcmp(form,"creation_order")==0) /* H5_INDEX_CRT_ORDER */
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

    if (HDstrcmp(form,"ascending")==0) /* H5_ITER_INC */
        iter_order = H5_ITER_INC;
    else if (HDstrcmp(form,"descending")==0) /* H5_ITER_DEC */
        iter_order = H5_ITER_DEC;

    return iter_order;
}

/*-------------------------------------------------------------------------
 * Function:    parse_hsize_list
 *
 * Purpose:     Parse a list of comma or space separated integers and return
 *              them in a list. The string being passed into this function
 *              should be at the start of the list you want to parse. You are
 *              responsible for freeing the array returned from here.
 *
 *              Lists in the so-called "terse" syntax are separated by
 *              semicolons (;). The lists themselves can be separated by
 *              either commas (,) or white spaces.
 *
 * Return:      <none>
 *-------------------------------------------------------------------------
 */
static void
parse_hsize_list(const char *h_list, subset_d *d)
{
    hsize_t        *p_list;
    const char     *ptr;
    unsigned int    size_count = 0;
    unsigned int    i = 0;
    unsigned int    last_digit = 0;

    if (!h_list || !*h_list || *h_list == ';')
        return;

    /* count how many integers do we have */
    for (ptr = h_list; ptr && *ptr && *ptr != ';' && *ptr != ']'; ptr++)
        if (HDisdigit(*ptr)) {
            if (!last_digit)
                /* the last read character wasn't a digit */
                size_count++;

            last_digit = 1;
        }
        else
            last_digit = 0;

    if (size_count == 0)
        /* there aren't any integers to read */
        return;

    /* allocate an array for the integers in the list */
    p_list = (hsize_t *)HDcalloc(size_count, sizeof(hsize_t));

    for (ptr = h_list; i < size_count && ptr && *ptr && *ptr != ';' && *ptr != ']'; ptr++)
        if(HDisdigit(*ptr)) {
            /* we should have an integer now */
            p_list[i++] = (hsize_t)HDstrtoull(ptr, NULL, 0);

            while (HDisdigit(*ptr))
                /* scroll to end of integer */
                ptr++;
        }
    d->data = p_list;
    d->len = size_count;
}

/*-------------------------------------------------------------------------
 * Function:    parse_subset_params
 *
 * Purpose:     Parse the so-called "terse" syntax for specifying subsetting
 *              parameters.
 *
 * Return:      Success:    struct subset_t object
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static struct subset_t *
parse_subset_params(char *dset)
{
    struct subset_t *s = NULL;
    char   *brace;

    if (!disable_compact_subset && ((brace = HDstrrchr(dset, '[')) != NULL)) {
        *brace++ = '\0';

        s = (struct subset_t *)HDcalloc(1, sizeof(struct subset_t));
        parse_hsize_list(brace, &s->start);

        while (*brace && *brace != ';')
            brace++;

        if (*brace)
            brace++;

        parse_hsize_list(brace, &s->stride);

        while (*brace && *brace != ';')
            brace++;

        if (*brace)
            brace++;

        parse_hsize_list(brace, &s->count);

        while (*brace && *brace != ';')
            brace++;

        if (*brace)
            brace++;

        parse_hsize_list(brace, &s->block);
    }

    return s;
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
    if(h_list) {
        HDmemset(packed_mask,0,sizeof(packed_mask));

        packed_bits_num = 0;
        /* scan in pair of offset,length separated by commas. */
        ptr = h_list;
        while (*ptr) {
            /* scan for an offset which is an unsigned int */
            if (!HDisdigit(*ptr)) {
                error_msg("Bad mask list(%s)\n", h_list);
                return FAIL;
            }
            soffset_value = HDatoi(ptr);
            offset_value = (unsigned)soffset_value;
            if (soffset_value < 0 || offset_value >= PACKED_BITS_SIZE_MAX) {
                error_msg("Packed Bit offset value(%d) must be between 0 and %u\n",
                        soffset_value, (unsigned)(PACKED_BITS_SIZE_MAX - 1));
                return FAIL;
            }

            /* skip to end of integer */
            while (HDisdigit(*++ptr))
                ;
            /* Look for the common separator */
            if (*ptr++ != ',') {
                error_msg("Bad mask list(%s), missing expected comma separator.\n", h_list);
                return FAIL;
            }

            /* scan for a length which is a positive int */
            if (!HDisdigit(*ptr)) {
                error_msg("Bad mask list(%s)\n", h_list);
                return FAIL;
            }
            slength_value = HDatoi(ptr);
            if (slength_value <= 0) {
                error_msg("Packed Bit length value(%d) must be positive.\n", slength_value);
                return FAIL;
            }
            length_value = (unsigned)slength_value;
            if ((offset_value + length_value) > PACKED_BITS_SIZE_MAX) {
                error_msg("Packed Bit offset+length value(%u) too large. Max is %u\n",
                        offset_value+length_value, (unsigned)PACKED_BITS_SIZE_MAX);
                return FAIL;
            }

            /* skip to end of int */
            while (HDisdigit(*++ptr))
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
            if(length_value < (int)(8 *sizeof(unsigned long long))) {
                temp_mask = temp_mask << length_value;
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
        if(packed_bits_num > PACKED_BITS_MAX) {
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
    else  {
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

    if(hand) {
        for (i = 0; i < len; i++) {
            if(hand[i].obj) {
                HDfree(hand[i].obj);
                hand[i].obj=NULL;
            }

            if (hand[i].subset_info) {
                if(hand[i].subset_info->start.data)
                    HDfree(hand[i].subset_info->start.data);
                if(hand[i].subset_info->stride.data)
                    HDfree(hand[i].subset_info->stride.data);
                if(hand[i].subset_info->count.data)
                    HDfree(hand[i].subset_info->count.data);
                if(hand[i].subset_info->block.data)
                    HDfree(hand[i].subset_info->block.data);

                HDfree(hand[i].subset_info);
                hand[i].subset_info=NULL;
            }
        }

        HDfree(hand);
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
parse_command_line(int argc, const char *argv[])
{
    struct handler_t   *hand = NULL;
    struct handler_t   *last_dset = NULL;
    int                 i;
    int                 opt;
    int                 last_was_dset = FALSE;

     /* no arguments */
    if (argc == 1) {
        usage(h5tools_getprogname());
        goto error;
    }

    /* this will be plenty big enough to hold the info */
    if((hand = (struct handler_t *)HDcalloc((size_t)argc, sizeof(struct handler_t)))==NULL) {
        goto error;
    }

    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
parse_start:
        switch ((char)opt) {
        case 'R':
            display_region = TRUE;
            region_output = TRUE;
            break;
        case 'B':
            display_bb = TRUE;
            last_was_dset = FALSE;
            break;
        case 'n':
            display_fi = TRUE;
            last_was_dset = FALSE;
            if (opt_arg != NULL)
                h5trav_set_verbose(HDatoi(opt_arg));
            break;
        case 'p':
            display_dcpl = TRUE;
            break;
        case 'y':
            display_ai = FALSE;
            break;
        case 'e':
            display_escape = TRUE;
            break;
        case 'H':
            display_data = FALSE;
            display_attr_data = FALSE;
            last_was_dset = FALSE;
            break;
        case 'A':
            if (opt_arg != NULL) {
                if(0 == HDatoi(opt_arg))
                    include_attrs = FALSE;
            }
            else {
                display_data = FALSE;
                display_attr_data = TRUE;
                last_was_dset = FALSE;
            }
            break;
        case 'i':
            display_oid = TRUE;
            last_was_dset = FALSE;
            break;
        case 'r':
            display_char = TRUE;
            break;
        case 'V':
            print_version(h5tools_getprogname());
            free_handler(hand, argc);
            hand = NULL;
            h5tools_setstatus(EXIT_SUCCESS);
            goto done;
            break;
        case 'w':
            {
                int sh5tools_nCols = HDatoi(opt_arg);

                if (sh5tools_nCols <= 0)
                    h5tools_nCols = 65535;
                else
                    h5tools_nCols = (unsigned)sh5tools_nCols;
                last_was_dset = FALSE;
            }
            break;
        case 'N':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_paths;
                    hand[i].obj = HDstrdup(opt_arg);
                    break;
                }

            last_was_dset = FALSE;
            break;
        case 'a':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_attributes;
                    hand[i].obj = HDstrdup(opt_arg);
                    break;
                }

            last_was_dset = FALSE;
            break;
        case 'd':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_datasets;
                    hand[i].obj = HDstrdup(opt_arg);
                    hand[i].subset_info = parse_subset_params(hand[i].obj);
                    last_dset = &hand[i];
                    break;
                }

            last_was_dset = TRUE;
            break;
        case 'f':
            driver = opt_arg;
            break;
        case 'g':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_groups;
                    hand[i].obj = HDstrdup(opt_arg);
                    break;
                }

            last_was_dset = FALSE;
            break;
        case 'l':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_links;
                    hand[i].obj = HDstrdup(opt_arg);
                    break;
                }

            last_was_dset = FALSE;
            break;
        case 't':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_datatypes;
                    hand[i].obj = HDstrdup(opt_arg);
                    break;
                }

            last_was_dset = FALSE;
            break;

        case 'O':
            if (h5tools_set_output_file(opt_arg, 0) < 0) {
                usage(h5tools_getprogname());
                goto error;
            }
            break;

        case 'o':
            if (bin_output) {
                if (h5tools_set_data_output_file(opt_arg, 1) < 0) {
                    usage(h5tools_getprogname());
                    goto error;
                }
            }
            else {
                if(display_attr_data && !display_data) {
                    if (h5tools_set_attr_output_file(opt_arg, 0) < 0) {
                        usage(h5tools_getprogname());
                        goto error;
                    }
                }
                if(display_data || display_all) {
                    if (h5tools_set_data_output_file(opt_arg, 0) < 0) {
                        usage(h5tools_getprogname());
                        goto error;
                    }
                }
            }

            usingdasho = TRUE;
            last_was_dset = FALSE;
            outfname = opt_arg;
            break;

        case 'b':
            if (opt_arg != NULL) {
                if ((bin_form = set_binary_form(opt_arg)) < 0) {
                    /* failed to set binary form */
                    usage(h5tools_getprogname());
                    goto error;
                }
            }
            bin_output = TRUE;
            if (outfname!=NULL) {
                if (h5tools_set_data_output_file(outfname, 1) < 0)  {
                    /* failed to set output file */
                    usage(h5tools_getprogname());
                    goto error;
                }

                last_was_dset = FALSE;
            }
            break;

        case 'q':
            if ((sort_by = set_sort_by(opt_arg)) < 0) {
                /* failed to set "sort by" form */
                usage(h5tools_getprogname());
                goto error;
            }
            break;

        case 'z':
            if ((sort_order = set_sort_order(opt_arg)) < 0) {
                /* failed to set "sort order" form */
                usage(h5tools_getprogname());
                goto error;
            }
            break;

        case 'M':
            if (!last_was_dset) {
                error_msg("option `-%c' can only be used after --dataset option\n", opt);
                goto error;
            }
            if (parse_mask_list(opt_arg) != SUCCEED){
                usage(h5tools_getprogname());
                goto error;
            }
            display_packed_bits = TRUE;
            break;
        case 'v':
            display_vds_first = TRUE;
            break;
        case 'G':
            vds_gap_size = HDatoi(opt_arg);
            if (vds_gap_size < 0) {
                usage(h5tools_getprogname());
                goto error;
            }
            break;

        /** begin XML parameters **/
        case 'x':
            /* select XML output */
            doxml = TRUE;
            useschema = TRUE;
            h5tools_dump_header_format = NULL;
            dump_function_table = &xml_function_table;
            h5tools_nCols = 0;
            break;
        case 'u':
            doxml = TRUE;
            useschema = FALSE;
            xmlnsprefix = "";
            h5tools_dump_header_format = NULL;
            dump_function_table = &xml_function_table;
            h5tools_nCols = 0;
            break;
        case 'D':
            /* specify alternative XML DTD or schema */
            /* To Do: check format of this value?  */
            xml_dtd_uri = opt_arg;
            h5tools_nCols = 0;
            break;

        case 'm':
            /* specify alternative floating point printing format */
            fp_format = opt_arg;
            h5tools_nCols = 0;
            break;

        case 'X':
            /* specify XML namespace (default="hdf5:"), or none */
            /* To Do: check format of this value?  */
            if (!useschema) {
                usage(h5tools_getprogname());
                goto error;
            }
            if (HDstrcmp(opt_arg,":") == 0)
                xmlnsprefix = "";
            else
                xmlnsprefix = opt_arg;
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
                error_msg("option `-%c' can only be used after --dataset option\n", opt);
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
                last_dset->subset_info = s = (struct subset_t *)HDcalloc(1, sizeof(struct subset_t));
            }

            /*
             * slightly convoluted, but...we are only interested in options
             * for subsetting: "--start", "--stride", "--count", and "--block"
             * which can come in any order. If we run out of parameters (EOF)
             * or run into one which isn't a subsetting parameter (NOT s, S,
             * c, or K), then we exit the do-while look, set the subset_info
             * to the structure we've been filling. If we've reached the end
             * of the options, we exit the parsing (goto parse_end) otherwise,
             * since we've "read" the next option, we need to parse it. So we
             * jump to the beginning of the switch statement (goto parse_start).
             */
            do {
                switch ((char)opt) {
                case 's':
                    if (s->start.data) {
                        HDfree(s->start.data);
                        s->start.data = NULL;
                    }
                    parse_hsize_list(opt_arg, &s->start);
                    break;
                case 'S':
                    if (s->stride.data) {
                        HDfree(s->stride.data);
                        s->stride.data = NULL;
                    }
                    parse_hsize_list(opt_arg, &s->stride);
                    break;
                case 'c':
                    if (s->count.data) {
                        HDfree(s->count.data);
                        s->count.data = NULL;
                    }
                    parse_hsize_list(opt_arg, &s->count);
                    break;
                case 'k':
                    if (s->block.data) {
                        HDfree(s->block.data);
                        s->block.data = NULL;
                    }
                    parse_hsize_list(opt_arg, &s->block);
                    break;
                default:
                    goto end_collect;
                }
            } while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF);

end_collect:
            last_was_dset = FALSE;

            if (opt != EOF)
                goto parse_start;
            else
                goto parse_end;
        }
        /** end subsetting parameters **/

        case 'E':
            if (opt_arg != NULL)
                enable_error_stack = HDatoi(opt_arg);
            else
                enable_error_stack = 1;
            break;
        case 'C':
            disable_compact_subset = TRUE;
            break;
        case 'h':
            usage(h5tools_getprogname());
            free_handler(hand, argc);
            hand = NULL;
            h5tools_setstatus(EXIT_SUCCESS);
            goto done;

        case '$':
#ifndef H5_HAVE_ROS3_VFD
            error_msg("Read-Only S3 VFD not enabled.\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
#else
            /* s3 credential */
            {
                char       **s3_cred = NULL;
                char        *s3_cred_string = NULL;
                const char  *ccred[3];
                unsigned     nelems = 0;
                if (FAIL == parse_tuple(opt_arg, ',', &s3_cred_string, &nelems, &s3_cred)) {
                    error_msg("unable to parse malformed s3 credentials\n");
                    usage(h5tools_getprogname());
                    free_handler(hand, argc);
                    hand = NULL;
                    h5tools_setstatus(EXIT_FAILURE);
                    goto done;
                }
                if (nelems != 3) {
                    error_msg("s3 credentials expects 3 elements\n");
                    usage(h5tools_getprogname());
                    free_handler(hand, argc);
                    hand = NULL;
                    h5tools_setstatus(EXIT_FAILURE);
                    goto done;
                }
                ccred[0] = (const char *)s3_cred[0];
                ccred[1] = (const char *)s3_cred[1];
                ccred[2] = (const char *)s3_cred[2];
                if (0 == h5tools_populate_ros3_fapl(&ros3_fa, ccred)) {
                    error_msg("Invalid S3 credentials\n");
                    usage(h5tools_getprogname());
                    free_handler(hand, argc);
                    hand = NULL;
                    h5tools_setstatus(EXIT_FAILURE);
                    goto done;
                }
                HDfree(s3_cred);
                HDfree(s3_cred_string);
            } /* s3 credential block */
            break;
#endif /* H5_HAVE_ROS3_VFD */

        case '#':
#ifndef H5_HAVE_LIBHDFS
            error_msg("HDFS VFD is not enabled.\n");
            goto error;
#else
            {
                /* read hdfs properties tuple and store values in `hdfs_fa` */
                unsigned         nelems    = 0;
                char            *props_src = NULL;
                char           **props     = NULL;
                unsigned long    k         = 0;
                if (FAIL == parse_tuple((const char *)opt_arg, ',', &props_src, &nelems, &props)) {
                    error_msg("unable to parse hdfs properties tuple\n");
                    goto error;
                }
                /* sanity-check tuple count
                 */
                if (nelems != 5) {
                    h5tools_setstatus(EXIT_FAILURE);
                    goto error;
                }
                /* Populate fapl configuration structure with given
                 * properties.
                 * WARNING: No error-checking is done on length of input
                 *          strings... Silent overflow is possible, albeit
                 *          unlikely.
                 */
                if (strncmp(props[0], "", 1))
                    HDstrncpy(hdfs_fa.namenode_name, (const char *)props[0], HDstrlen(props[0]));

                if (strncmp(props[1], "", 1)) {
                    k = strtoul((const char *)props[1], NULL, 0);
                    if (errno == ERANGE) {
                        h5tools_setstatus(EXIT_FAILURE);
                        goto error;
                    }
                    hdfs_fa.namenode_port = (int32_t)k;
                }
                if (strncmp(props[2], "", 1))
                    HDstrncpy(hdfs_fa.kerberos_ticket_cache, (const char *)props[2], HDstrlen(props[2]));

                if (strncmp(props[3], "", 1))
                    HDstrncpy(hdfs_fa.user_name, (const char *)props[3], HDstrlen(props[3]));

                if (strncmp(props[4], "", 1)) {
                    k = strtoul((const char *)props[4], NULL, 0);
                    if (errno == ERANGE) {
                        h5tools_setstatus(EXIT_FAILURE);
                        goto error;
                    }
                    hdfs_fa.stream_buffer_size = (int32_t)k;
                }
                HDfree(props);
                HDfree(props_src);
            }
#endif /* H5_HAVE_LIBHDFS */
            break;

        case '?':
        default:
            usage(h5tools_getprogname());
            goto error;
        }
    }

parse_end:
    /* check for file name to be processed */
    if (argc <= opt_ind) {
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
main(int argc, const char *argv[])
{
    hid_t               fid = -1;
    hid_t               gid = -1;
    hid_t               fapl_id = H5P_DEFAULT;
    H5E_auto2_t         func;
    H5E_auto2_t         tools_func;
    H5O_info_t          oi;
    struct handler_t   *hand = NULL;
    int                 i;
    unsigned            u;
    void               *edata;
    void               *tools_edata;
    char               *fname = NULL;

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);
    h5tools_dump_header_format = &h5tools_standardformat;
    dump_function_table = &ddl_function_table;
    dump_indent = 0;

    /* Disable error reporting */
    H5Eget_auto2(H5E_DEFAULT, &func, &edata);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Disable tools error reporting */
    H5Eget_auto2(H5tools_ERR_STACK_g, &tools_func, &tools_edata);
    H5Eset_auto2(H5tools_ERR_STACK_g, NULL, NULL);

    if((hand = parse_command_line(argc, argv))==NULL) {
        goto done;
    }

    if (bin_output && outfname == NULL) {
        error_msg("binary output requires a file name, use -o <filename>\n");
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    if (enable_error_stack > 0) {
        H5Eset_auto2(H5E_DEFAULT, func, edata);
        H5Eset_auto2(H5tools_ERR_STACK_g, tools_func, tools_edata);
    }

    /* Check for conflicting options */
    if (doxml) {
        if (!display_all) {
            error_msg("option \"%s\" not available for XML\n", "to display selected objects");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (display_bb) {
            error_msg("option \"%s\" not available for XML\n", "--boot-block");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (display_oid == 1) {
            error_msg("option \"%s\" not available for XML\n", "--object-ids");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (display_char == TRUE) {
            error_msg("option \"%s\" not available for XML\n", "--string");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (usingdasho) {
            error_msg("option \"%s\" not available for XML\n", "--output");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }
    else {
        if (xml_dtd_uri) {
            warn_msg("option \"%s\" only applies with XML: %s\n", "--xml-dtd", xml_dtd_uri);
        }
    }

    if (argc <= opt_ind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }
    /* Initialize indexing options */
    h5trav_set_index(sort_by, sort_order);

    if (driver != NULL) {
        void *conf_fa = NULL;

        if (!strcmp(driver, "ros3")) {
#ifndef H5_HAVE_ROS3_VFD
            error_msg("Read-Only S3 VFD not enabled.\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
#else
            conf_fa = (void *)&ros3_fa;
#endif /* H5_HAVE_ROS3_VFD */
        }
        else if (!HDstrcmp(driver, "hdfs")) {
#ifndef H5_HAVE_LIBHDFS
            error_msg("HDFS VFD is not enabled.\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
#else
            conf_fa = (void *)&hdfs_fa;
#endif /* H5_HAVE_LIBHDFS */
        }

        if (conf_fa != NULL) {
            fapl_id = H5Pcreate(H5P_FILE_ACCESS);
            if (fapl_id < 0) {
                error_msg("unable to create fapl entry\n");
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            }
            /* driver guaranteed "ros3" or "hdfs" */
            /* conf_fa appropriate to driver */
            if (0 == h5tools_set_configured_fapl(fapl_id, driver, conf_fa)) {
                error_msg("unable to set fapl\n");
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            }
        }
    } /* driver defined */

    while(opt_ind < argc) {
        fname = HDstrdup(argv[opt_ind++]);

        if (fapl_id != H5P_DEFAULT) {
            fid = H5Fopen(fname, H5F_ACC_RDONLY, fapl_id);
        }
        else {
            fid = h5tools_fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT, driver, NULL, 0);
        }

        if (fid < 0) {
            error_msg("unable to open file \"%s\"\n", fname);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        /* allocate and initialize internal data structure */
        init_prefix(&prefix, prefix_len);

        /* Prepare to find objects that might be targets of a reference */
        fill_ref_path_table(fid);

        if(doxml) {
            /* initialize XML */
            /* reset prefix! */
            HDstrcpy(prefix, "");

            /* make sure the URI is initialized to something */
            if (xml_dtd_uri == NULL) {
                if (useschema) {
                    xml_dtd_uri = DEFAULT_XSD;
                }
                else {
                    xml_dtd_uri = DEFAULT_DTD;
                    xmlnsprefix = "";
                }
            }
            else {
                if (useschema && HDstrcmp(xmlnsprefix,"")) {
                    error_msg("Cannot set Schema URL for a qualified namespace--use -X or -U option with -D \n");
                    h5tools_setstatus(EXIT_FAILURE);
                    goto done;
                }
            }
        }

        /* Get object info for root group */
        if(H5Oget_info_by_name2(fid, "/", &oi, H5O_INFO_BASIC, H5P_DEFAULT) < 0) {
            error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        /* Initialize object tables */
        if(table_list_add(fid, oi.fileno) < 0) {
            error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        group_table = table_list.tables[0].group_table;
        dset_table = table_list.tables[0].dset_table;
        type_table = table_list.tables[0].type_table;

        /* does there exist unamed committed datatype */
        for (u = 0; u < type_table->nobjs; u++)
            if (!type_table->objs[u].recorded) {
                unamedtype = 1;
                break;
            } /* end if */

        /* start to dump - display file header information */
        if (!doxml) {
            begin_obj(h5tools_dump_header_format->filebegin, fname, h5tools_dump_header_format->fileblockbegin);
        }
        else {
            PRINTVALSTREAM(rawoutstream, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

            /* alternative first element, depending on schema or DTD. */
            if (useschema) {
                if (HDstrcmp(xmlnsprefix,"") == 0) {
                    PRINTSTREAM(rawoutstream, "<HDF5-File xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"%s\">\n", xml_dtd_uri);
                }
                else {
                    /*  TO DO: make -url option work in this case (may need new option) */
                    char *ns;
                    char *indx;

                    ns = HDstrdup(xmlnsprefix);
                    indx = HDstrrchr(ns,(int)':');
                    if (indx)
                        *indx = '\0';

                    PRINTSTREAM(rawoutstream, "<%sHDF5-File xmlns:%s=\"http://hdfgroup.org/HDF5/XML/schema/HDF5-File.xsd\" "
                            "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
                            "xsi:schemaLocation=\"http://hdfgroup.org/HDF5/XML/schema/HDF5-File "
                            "http://www.hdfgroup.org/HDF5/XML/schema/HDF5-File.xsd\">\n",xmlnsprefix,ns);
                    HDfree(ns);
                }
            }
            else {
                PRINTSTREAM(rawoutstream, "<!DOCTYPE HDF5-File PUBLIC \"HDF5-File.dtd\" \"%s\">\n", xml_dtd_uri);
                PRINTVALSTREAM(rawoutstream, "<HDF5-File>\n");
            }
        }

        if (!doxml) {
            if (display_fi) {
                PRINTVALSTREAM(rawoutstream, "\n");
                dump_fcontents(fid);
                end_obj(h5tools_dump_header_format->fileend,h5tools_dump_header_format->fileblockend);
                PRINTVALSTREAM(rawoutstream, "\n");
                goto done;
            }

            if (display_bb)
                dump_fcpl(fid);
        }

        if(display_all) {
            if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) {
                error_msg("unable to open root group\n");
                h5tools_setstatus(EXIT_FAILURE);
            }
            else {
                if (!doxml)
                    dump_indent += COL;
                dump_function_table->dump_group_function(gid, "/" );
                if (!doxml)
                    dump_indent -= COL;
                PRINTVALSTREAM(rawoutstream, "\n");
            }

            if(H5Gclose(gid) < 0) {
                error_msg("unable to close root group\n");
                h5tools_setstatus(EXIT_FAILURE);
            }

        }
        else {
            /* Note: this option is not supported for XML */
            if(doxml) {
                error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            } /* end if */

            for(i = 0; i < argc; i++) {
                if(hand[i].func) {
                    hand[i].func(fid, hand[i].obj, hand[i].subset_info, 1, NULL);
                }
            }
            PRINTVALSTREAM(rawoutstream, "\n");
        }

        if (!doxml) {
            end_obj(h5tools_dump_header_format->fileend, h5tools_dump_header_format->fileblockend);
            PRINTVALSTREAM(rawoutstream, "\n");
        }
        else {
            PRINTSTREAM(rawoutstream, "</%sHDF5-File>\n", xmlnsprefix);
        }
        /* Free tables for objects */
        table_list_free();

        if(fid >=0)
            if (H5Fclose(fid) < 0)
                h5tools_setstatus(EXIT_FAILURE);

        if(prefix) {
            HDfree(prefix);
            prefix = NULL;
        }
        if(fname) {
            HDfree(fname);
            fname = NULL;
        }
    } /* end while */

    if(hand)
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

    if(fid >=0)
        if (H5Fclose(fid) < 0)
            h5tools_setstatus(EXIT_FAILURE);

    if(prefix) {
        HDfree(prefix);
        prefix = NULL;
    }
    if(fname) {
        HDfree(fname);
        fname = NULL;
    }

    if(hand)
        free_handler(hand, argc);

    /* To Do:  clean up XML table */

    H5Eset_auto2(H5E_DEFAULT, func, edata);

    leave(h5tools_getstatus());
} /* main */


/*-------------------------------------------------------------------------
 * Function:    init_prefix
 *
 * Purpose:     allocate and initialize prefix
 *
 * Return:      void
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
init_prefix(char **prfx, size_t prfx_len)
{
    if(prfx_len > 0)
        *prfx = (char *)HDcalloc(prfx_len, 1);
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
    size_t new_len = HDstrlen(*prfx) + HDstrlen(name) + 2;

    /* Check if we need more space */
    if(*prfx_len <= new_len) {
        *prfx_len = new_len + 1;
        *prfx = (char *)HDrealloc(*prfx, *prfx_len);
    }

    /* Append object name to prefix */
    HDstrcat(HDstrcat(*prfx, "/"), name);
} /* end add_prefix */

