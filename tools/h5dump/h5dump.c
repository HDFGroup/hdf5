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
#include <stdio.h>
#include <stdlib.h>

#include "h5dump.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5tools_ref.h"
#include "h5trav.h"


/* module-scoped variables */
const char  *progname = "h5dump";

/* Macros for displaying objects */
#define begin_obj(obj,name,begin)                               \
    if (name)                                                   \
        printf("%s \"%s\" %s\n", (obj), (name), (begin));       \
    else                                                        \
        printf("%s %s\n", (obj), (begin));

#define end_obj(obj,end)                                        \
    if(HDstrlen(end)) {                                         \
        printf("%s", end);                                      \
        if(HDstrlen(obj))                                       \
            printf(" ");                                        \
    }                                                           \
    if(HDstrlen(obj))                                           \
        printf("%s", obj);                                      \
    printf("\n");

/* 3 private values: can't be set, but can be read.
   Note: these are defined in H5Zprivate, they are
   duplicated here.
 */
#define H5_SZIP_LSB_OPTION_MASK         8
#define H5_SZIP_MSB_OPTION_MASK         16
#define H5_SZIP_RAW_OPTION_MASK         128

int                 d_status = EXIT_SUCCESS;
static int          unamedtype = 0;     /* shared datatype with no name */
static table_t      *group_table = NULL, *dset_table = NULL, *type_table = NULL;
static size_t       prefix_len = 1024;
static char         *prefix;
static const char   *driver = NULL;      /* The driver to open the file with. */
static const h5dump_header_t *dump_header_format;
static const char   *fp_format = NULL;
static const char   *outfname=NULL;


/* things to display or which are set via command line parameters */
static int          display_all       = TRUE;
static int          display_oid       = FALSE;
static int          display_data      = TRUE;
static int          display_attr_data = TRUE;
static int          display_char      = FALSE; /*print 1-byte numbers as ASCII */
static int          usingdasho        = FALSE;
static int          display_bb        = FALSE; /*superblock */
static int          display_dcpl      = FALSE; /*dcpl */
static int          display_fi        = FALSE; /*file index */
static int          display_ai        = TRUE;  /*array index */
static int          display_escape    = FALSE; /*escape non printable characters */

/**
 **  Added for XML  **
 **/

/* module-scoped variables for XML option */
#define DEFAULT_XSD     "http://www.hdfgroup.org/DTDs/HDF5-File.xsd"
#define DEFAULT_DTD     "http://www.hdfgroup.org/DTDs/HDF5-File.dtd"

static int              doxml = 0;
static int              useschema = 1;
static const char       *xml_dtd_uri = NULL;
static const char       *xmlnsprefix="hdf5:";
static int              indent;              /*how far in to indent the line         */

/** end XML **/

/* internal functions */
static hid_t    h5_fileaccess(void);
static void     dump_oid(hid_t oid);
static void     print_enum(hid_t type);
static herr_t   dump_all(hid_t group, const char *name, void *op_data);
static int      xml_name_to_XID(const char *, char *, int , int );
static void     init_prefix(char **prfx, size_t prfx_len);
static void     add_prefix(char **prfx, size_t *prfx_len, const char *name);

static h5tool_format_t         dataformat = {
    0,              /*raw */

    "",             /*fmt_raw */
    "%d",           /*fmt_int */
    "%u",           /*fmt_uint */
    "%d",           /*fmt_schar */
    "%u",           /*fmt_uchar */
    "%d",           /*fmt_short */
    "%u",           /*fmt_ushort */
    "%ld",          /*fmt_long */
    "%lu",          /*fmt_ulong */
    NULL,           /*fmt_llong */
    NULL,           /*fmt_ullong */
    "%g",           /*fmt_double */
    "%g",           /*fmt_float */

    0,              /*ascii */
    0,              /*str_locale */
    0,              /*str_repeat */

    "[ ",           /*arr_pre */
    ",",            /*arr_sep */
    " ]",           /*arr_suf */
    1,              /*arr_linebreak */

    "",             /*cmpd_name */
    ",\n",          /*cmpd_sep */
    "{\n",          /*cmpd_pre */
    "}",            /*cmpd_suf */
    "\n",           /*cmpd_end */

    ", ",           /*vlen_sep */
    "(",            /*vlen_pre */
    ")",            /*vlen_suf */
    "",             /*vlen_end */

    "%s",           /*elmt_fmt */
    ",",            /*elmt_suf1 */
    " ",            /*elmt_suf2 */

    "",             /*idx_n_fmt */
    "",             /*idx_sep */
    "",             /*idx_fmt */

    80,             /*line_ncols *//*standard default columns */
    0,              /*line_per_line */
    "",             /*line_pre */
    "%s",           /*line_1st */
    "%s",           /*line_cont */
    "",             /*line_suf */
    "",             /*line_sep */
    1,              /*line_multi_new */
    "   ",          /*line_indent */

    1,              /*skip_first */

    1,              /*obj_hidefileno */
    " %lu:%lu",         /*obj_format */

    1,              /*dset_hidefileno */
    "DATASET %lu:%lu ",     /*dset_format */
    "%s",           /*dset_blockformat_pre */
    "%s",           /*dset_ptformat_pre */
    "%s",           /*dset_ptformat */
    1,                          /*array indices */
    1                           /*escape non printable characters */
};

/**
 **  Added for XML  **
 **/
/*
 *  Alternative formating for data dumped to XML
 *  In general, the numbers are the same, but separators
 *  except spaces are not used.
 *
 *  Some of these are not used, as some kinds of data are
 *  dumped in completely new subroutines.
 *
 *  Some of this formatting may yet need to change.
 *
 *  This table only affects XML output.
 */
static h5tool_format_t         xml_dataformat = {
    0,              /*raw */

    "",             /*fmt_raw */
    "%d",           /*fmt_int */
    "%u",           /*fmt_uint */
    "%d",           /*fmt_schar */
    "%u",           /*fmt_uchar */
    "%d",           /*fmt_short */
    "%u",           /*fmt_ushort */
    "%ld",          /*fmt_long */
    "%lu",          /*fmt_ulong */
    NULL,           /*fmt_llong */
    NULL,           /*fmt_ullong */
    "%g",           /*fmt_double */
    "%g",           /*fmt_float */

    0,              /*ascii */
    0,              /*str_locale */
    0,              /*str_repeat */

    " ",            /*arr_pre */
    "",             /*arr_sep */
    "",             /*arr_suf */
    1,              /*arr_linebreak */

    "",             /*cmpd_name */
    " ",            /*cmpd_sep */
    "",             /*cmpd_pre */
    "",             /*cmpd_suf */
    "",             /*cmpd_end */

    " ",            /*vlen_sep */
    " ",            /*vlen_pre */
    "",             /*vlen_suf */
    "",             /*vlen_end */

    "%s",           /*elmt_fmt */
    "",             /*elmt_suf1 */
    " ",            /*elmt_suf2 */

    "",             /*idx_n_fmt */
    "",             /*idx_sep */
    "",             /*idx_fmt */

    80,             /*line_ncols *//*standard default columns */
    0,              /*line_per_line */
    "",             /*line_pre */
    "%s",           /*line_1st */
    "%s",           /*line_cont */
    "",             /*line_suf */
    "",             /*line_sep */
    1,              /*line_multi_new */
    "   ",          /*line_indent */

    1,              /*skip_first */

    1,              /*obj_hidefileno */
    " %lu:%lu",         /*obj_format */

    1,              /*dset_hidefileno */
    "DATASET %lu:%lu ",     /*dset_format */
    "%s",           /*dset_blockformat_pre */
    "%s",           /*dset_ptformat_pre */
    "%s",           /*dset_ptformat */
     0,                         /*array indices */
     0                          /*escape non printable characters */
};

/** XML **/

static const h5dump_header_t standardformat = {
    "standardformat",       /*name */
    "HDF5",         /*fileebgin */
    "",             /*fileend */
    SUPER_BLOCK,            /*bootblockbegin */
    "",             /*bootblockend */
    H5_TOOLS_GROUP,          /*groupbegin */
    "",             /*groupend */
    H5_TOOLS_DATASET,            /*datasetbegin */
    "",             /*datasetend */
    ATTRIBUTE,          /*attributebegin */
    "",             /*attributeend */
    H5_TOOLS_DATATYPE,           /*datatypebegin */
    "",             /*datatypeend */
    DATASPACE,          /*dataspacebegin */
    "",             /*dataspaceend */
    DATA,           /*databegin */
    "",             /*dataend */
    SOFTLINK,           /*softlinkbegin */
    "",             /*softlinkend */
    SUBSET,         /*subsettingbegin */
    "",             /*subsettingend */
    START,          /*startbegin */
    "",             /*startend */
    STRIDE,         /*stridebegin */
    "",             /*strideend */
    COUNT,          /*countbegin */
    "",             /*countend */
    BLOCK,          /*blockbegin */
    "",             /*blockend */

    "{",            /*fileblockbegin */
    "}",            /*fileblockend */
    "{",            /*bootblockblockbegin */
    "}",            /*bootblockblockend */
    "{",            /*groupblockbegin */
    "}",            /*groupblockend */
    "{",            /*datasetblockbegin */
    "}",            /*datasetblockend */
    "{",            /*attributeblockbegin */
    "}",            /*attributeblockend */
    "",             /*datatypeblockbegin */
    "",             /*datatypeblockend */
    "",             /*dataspaceblockbegin */
    "",             /*dataspaceblockend */
    "{",            /*datablockbegin */
    "}",            /*datablockend */
    "{",            /*softlinkblockbegin */
    "}",            /*softlinkblockend */
    "{",            /*strblockbegin */
    "}",            /*strblockend */
    "{",            /*enumblockbegin */
    "}",            /*enumblockend */
    "{",            /*structblockbegin */
    "}",            /*structblockend */
    "{",            /*vlenblockbegin */
    "}",            /*vlenblockend */
    "{",                        /*subsettingblockbegin */
    "}",                        /*subsettingblockend */
    "(",                        /*startblockbegin */
    ");",                       /*startblockend */
    "(",                        /*strideblockbegin */
    ");",                       /*strideblockend */
    "(",                        /*countblockbegin */
    ");",                       /*countblockend */
    "(",                        /*blockblockbegin */
    ");",                       /*blockblockend */

    "",             /*dataspacedescriptionbegin */
    "",             /*dataspacedescriptionend */
    "(",            /*dataspacedimbegin */
    ")",            /*dataspacedimend */
};

/**
 ** Added for XML **
 **/
/* internal functions used by XML option */
static void             xml_print_datatype(hid_t, unsigned);
static void             xml_print_enum(hid_t);
static int              xml_print_refs(hid_t, int);
static int              xml_print_strs(hid_t, int);
static char            *xml_escape_the_string(const char *, int);
static char            *xml_escape_the_name(const char *);

/* a structure for handling the order command-line parameters come in */
struct handler_t {
    void (*func)(hid_t, char *, void *);
    char *obj;
    struct subset_t *subset_info;
};

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
static const char *s_opts = "hnpeyBHirVa:c:d:f:g:k:l:t:w:xD:uX:o:b*:F:s:S:Am:";
static struct long_options l_opts[] = {
    { "help", no_arg, 'h' },
    { "hel", no_arg, 'h' },
    { "contents", no_arg, 'n' },
    { "properties", no_arg, 'p' },
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
    { "output", require_arg, 'o' },
    { "outpu", require_arg, 'o' },
    { "outp", require_arg, 'o' },
    { "out", require_arg, 'o' },
    { "ou", require_arg, 'o' },
    { "soft-link", require_arg, 'l' },
    { "soft-lin", require_arg, 'l' },
    { "soft-li", require_arg, 'l' },
    { "soft-l", require_arg, 'l' },
    { "soft", require_arg, 'l' },
    { "sof", require_arg, 'l' },
    { "so", require_arg, 'l' },
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
    { "onlyattr", no_arg, 'A' },
    { "escape", no_arg, 'e' },
    { "noindex", no_arg, 'y' },
    { "binary", optional_arg, 'b' },
    { "form", require_arg, 'F' },
    { "format", require_arg, 'm' },
    { NULL, 0, '\0' }
};

/**
 **  Change for XML  **
 **
 **  The 'dump_xxx' functions have two versions, standard and XML.
 **
 **    They are called indirectly through the 'dump_function_table'.
 **    e.g., dump_group(...) becomes dump_functions->dump_group(...);
 **
 **    The standard functions are unchanged except for the way
 **    they are called
 **/

/* The dump functions of the dump_function_table */

/* standard format:  no change */
static void             dump_group(hid_t, const char *);
static void             dump_named_datatype(hid_t, const char *);
static void             dump_dataset(hid_t, const char *, struct subset_t *);
static void             dump_dataspace(hid_t space);
static void             dump_datatype(hid_t type);
static herr_t           dump_attr(hid_t, const char *, void *);
static void             dump_data(hid_t, int, struct subset_t *, int);
static void             dump_dcpl(hid_t dcpl, hid_t type_id, hid_t obj_id);
static void             dump_comment(hid_t obj_id);
static void             dump_fcpl(hid_t fid);
static void             dump_fcontents(hid_t fid);

/* XML format:   same interface, alternative output */

static void             xml_dump_group(hid_t, const char *);
static void             xml_dump_named_datatype(hid_t, const char *);
static void             xml_dump_dataset(hid_t, const char *, struct subset_t *);
static void             xml_dump_dataspace(hid_t space);
static void             xml_dump_datatype(hid_t type);
static herr_t           xml_dump_attr(hid_t, const char *, void *);
static void             xml_dump_data(hid_t, int, struct subset_t *, int);

/**
 ** Added for XML **
 **
 **  This is the global dispatch table for the dump functions.
 **/
/* the table of dump functions */
typedef struct dump_functions_t {
    void                (*dump_group_function) (hid_t, const char *);
    void                (*dump_named_datatype_function) (hid_t, const char *);
    void                (*dump_dataset_function) (hid_t, const char *, struct subset_t *);
    void                (*dump_dataspace_function) (hid_t);
    void                (*dump_datatype_function) (hid_t);
    herr_t              (*dump_attribute_function) (hid_t, const char *, void *);
    void                (*dump_data_function) (hid_t, int, struct subset_t *, int);
} dump_functions;

/* Standard DDL output */
static const dump_functions ddl_function_table = {
    dump_group,
    dump_named_datatype,
    dump_dataset,
    dump_dataspace,
    dump_datatype,
    dump_attr,
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

/*
 * The global table is set to either ddl_function_table or
 * xml_function_table in the initialization.
 */
static const dump_functions *dump_function_table;


/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Shutdown MPI & HDF5 and call exit()
 *
 * Return:      Does not return
 *
 * Programmer:  Quincey Koziol
 *              Saturday, 31. January 2004
 *
 * Modifications:
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
 * Purpose:     Print the usage message about dumper
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage(const char *prog)
{
    fflush(stdout);
    fprintf(stdout, "usage: %s [OPTIONS] file\n", prog);
    fprintf(stdout, "  OPTIONS\n");
    fprintf(stdout, "     -h, --help           Print a usage message and exit\n");
    fprintf(stdout, "     -n, --contents       Print a list of the file contents and exit\n");
    fprintf(stdout, "     -B, --superblock     Print the content of the super block\n");
    fprintf(stdout, "     -H, --header         Print the header only; no data is displayed\n");
    fprintf(stdout, "     -A, --onlyattr       Print the header and value of attributes\n");
    fprintf(stdout, "     -i, --object-ids     Print the object ids\n");
    fprintf(stdout, "     -r, --string         Print 1-byte integer datasets as ASCII\n");
    fprintf(stdout, "     -e, --escape         Escape non printing characters\n");
    fprintf(stdout, "     -V, --version        Print version number and exit\n");
    fprintf(stdout, "     -a P, --attribute=P  Print the specified attribute\n");
    fprintf(stdout, "     -d P, --dataset=P    Print the specified dataset\n");
    fprintf(stdout, "     -y, --noindex        Do not print array indices with the data\n");
    fprintf(stdout, "     -p,   --properties   Print dataset filters, storage layout and fill value\n");
    fprintf(stdout, "     -f D, --filedriver=D Specify which driver to open the file with\n");
    fprintf(stdout, "     -g P, --group=P      Print the specified group and all members\n");
    fprintf(stdout, "     -l P, --soft-link=P  Print the value(s) of the specified soft link\n");
    fprintf(stdout, "     -o F, --output=F     Output raw data into file F\n");
    fprintf(stdout, "     -b B, --binary=B     Binary file output, of form B\n");
    fprintf(stdout, "     -t P, --datatype=P   Print the specified named data type\n");
    fprintf(stdout, "     -w N, --width=N      Set the number of columns of output. A value of 0 (zero)\n");
    fprintf(stdout, "                          sets the number of columns to the maximum (65535).\n");
    fprintf(stdout, "                          Default width is 80 columns.\n");
    fprintf(stdout, "     -m T, --format=T     Set the floating point output format\n");
    fprintf(stdout, "     -x, --xml            Output in XML using Schema\n");
    fprintf(stdout, "     -u, --use-dtd        Output in XML using DTD\n");
    fprintf(stdout, "     -D U, --xml-dtd=U    Use the DTD or schema at U\n");
    fprintf(stdout, "     -X S, --xml-ns=S      (XML Schema) Use qualified names n the XML\n");
    fprintf(stdout, "                          \":\": no namespace, default: \"hdf5:\"\n");
    fprintf(stdout, "                          E.g., to dump a file called `-f', use h5dump -- -f\n");
    fprintf(stdout, "\n");
    fprintf(stdout, " Subsetting is available by using the following options with a dataset\n");
    fprintf(stdout, " attribute. Subsetting is done by selecting a hyperslab from the data.\n");
    fprintf(stdout, " Thus, the options mirror those for performing a hyperslab selection.\n");
    fprintf(stdout, " The START and COUNT parameters are mandatory if you do subsetting.\n");
    fprintf(stdout, " The STRIDE and BLOCK parameters are optional and will default to 1 in\n");
    fprintf(stdout, " each dimension.\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "      -s L, --start=L     Offset of start of subsetting selection\n");
    fprintf(stdout, "      -S L, --stride=L    Hyperslab stride\n");
    fprintf(stdout, "      -c L, --count=L     Number of blocks to include in selection\n");
    fprintf(stdout, "      -k L, --block=L     Size of block in hyperslab\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  D - is the file driver to use in opening the file. Acceptable values\n");
    fprintf(stdout, "        are \"sec2\", \"family\", \"split\", and \"multi\". Without\n");
    fprintf(stdout, "        the file driver flag, the file will be opened with each driver in\n");
    fprintf(stdout, "        turn and in the order specified above until one driver succeeds\n");
    fprintf(stdout, "        in opening the file.\n");
    fprintf(stdout, "  F - is a filename.\n");
    fprintf(stdout, "  P - is the full path from the root group to the object.\n");
    fprintf(stdout, "  N - is an integer greater than 1.\n");
    fprintf(stdout, "  T - is a string containing the floating point format, e.g '%%.3f'\n");
    fprintf(stdout, "  L - is a list of integers the number of which are equal to the\n");
    fprintf(stdout, "        number of dimensions in the dataspace being queried\n");
    fprintf(stdout, "  U - is a URI reference (as defined in [IETF RFC 2396],\n");
    fprintf(stdout, "        updated by [IETF RFC 2732])\n");
    fprintf(stdout, "  B - is the form of binary output: MEMORY for a memory type, FILE for the\n");
    fprintf(stdout, "        file type, LE or BE for pre-existing little or big endian types.\n");
    fprintf(stdout, "        Must be used with -o (output file) and it is recommended that\n");
    fprintf(stdout, "        -d (dataset) is used\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  Examples:\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  1) Attribute foo of the group /bar_none in file quux.h5\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "     	h5dump -a /bar_none/foo quux.h5\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  2) Selecting a subset from dataset /foo in file quux.h5\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "      h5dump -d /foo -s \"0,1\" -S \"1,1\" -c \"2,3\" -k \"2,2\" quux.h5\n");
    fprintf(stdout, "\n");
    fprintf(stdout, "  3) Saving dataset 'dset' in file quux.h5 to binary file 'out.bin' using a little-endian type \n");
    fprintf(stdout, "\n");
    fprintf(stdout, "      h5dump -d /dset -b LE -o out.bin quux.h5\n");
    fprintf(stdout, "\n");
}

/*-------------------------------------------------------------------------
 * Function:    print_datatype
 *
 * Purpose:     print the datatype.
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: pvn, March 28, 2006
 *  print information about type when a native match is not possible
 *
 *-------------------------------------------------------------------------
 */

static void
print_datatype(hid_t type,unsigned in_group)
{
    char       *mname;
    hid_t       mtype, str_type;
    unsigned    nmembers;
    unsigned    ndims;
    unsigned    i;
    size_t      size=0;
    hsize_t     dims[H5DUMP_MAX_RANK];
    H5T_str_t   str_pad;
    H5T_cset_t  cset;
    H5T_order_t order;
    hid_t       super;
    hid_t       tmp_type;
    htri_t      is_vlstr=FALSE;
    const char  *order_s=NULL;  /* byte order string */
    H5T_sign_t  sign;           /* sign scheme value */
    const char  *sign_s=NULL;   /* sign scheme string */


    if (!in_group && H5Tcommitted(type) > 0) {
        obj_t  *obj;    /* Found object */
        H5G_stat_t  statbuf;

        H5Gget_objinfo(type, ".", TRUE, &statbuf);
        obj = search_obj(type_table, statbuf.objno);

        if (obj) {
            if (!obj->recorded) {
                unsigned long   objno[2];   /*object number         */

                objno[0] = (unsigned long)(obj->objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
                objno[1] = (unsigned long)(obj->objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                HDfprintf(stdout,"\"/#%lu:%lu\"", objno[0], objno[1]);
            }
            else
                printf("\"%s\"", obj->objname);
        } else {
            error_msg(progname, "unknown committed type.\n");
            d_status = EXIT_FAILURE;
        }
    } else {
        switch (H5Tget_class(type)) {
        case H5T_INTEGER:
                if(H5Tequal(type, H5T_STD_I8BE) == TRUE) {
                    printf("H5T_STD_I8BE");
                } else if(H5Tequal(type, H5T_STD_I8LE) == TRUE) {
                    printf("H5T_STD_I8LE");
                } else if(H5Tequal(type, H5T_STD_I16BE) == TRUE) {
                    printf("H5T_STD_I16BE");
                } else if(H5Tequal(type, H5T_STD_I16LE) == TRUE) {
                    printf("H5T_STD_I16LE");
                } else if(H5Tequal(type, H5T_STD_I32BE) == TRUE) {
                    printf("H5T_STD_I32BE");
                } else if(H5Tequal(type, H5T_STD_I32LE) == TRUE) {
                    printf("H5T_STD_I32LE");
                } else if(H5Tequal(type, H5T_STD_I64BE) == TRUE) {
                    printf("H5T_STD_I64BE");
                } else if(H5Tequal(type, H5T_STD_I64LE) == TRUE) {
                    printf("H5T_STD_I64LE");
                } else if(H5Tequal(type, H5T_STD_U8BE) == TRUE) {
                    printf("H5T_STD_U8BE");
                } else if(H5Tequal(type, H5T_STD_U8LE) == TRUE) {
                    printf("H5T_STD_U8LE");
                } else if(H5Tequal(type, H5T_STD_U16BE) == TRUE) {
                    printf("H5T_STD_U16BE");
                } else if(H5Tequal(type, H5T_STD_U16LE) == TRUE) {
                    printf("H5T_STD_U16LE");
                } else if(H5Tequal(type, H5T_STD_U32BE) == TRUE) {
                    printf("H5T_STD_U32BE");
                } else if(H5Tequal(type, H5T_STD_U32LE) == TRUE) {
                    printf("H5T_STD_U32LE");
                } else if(H5Tequal(type, H5T_STD_U64BE) == TRUE) {
                    printf("H5T_STD_U64BE");
                } else if(H5Tequal(type, H5T_STD_U64LE) == TRUE) {
                    printf("H5T_STD_U64LE");
                } else if(H5Tequal(type, H5T_NATIVE_SCHAR) == TRUE) {
                    printf("H5T_NATIVE_SCHAR");
                } else if(H5Tequal(type, H5T_NATIVE_UCHAR) == TRUE) {
                    printf("H5T_NATIVE_UCHAR");
                } else if(H5Tequal(type, H5T_NATIVE_SHORT) == TRUE) {
                    printf("H5T_NATIVE_SHORT");
                } else if(H5Tequal(type, H5T_NATIVE_USHORT) == TRUE) {
                    printf("H5T_NATIVE_USHORT");
                } else if(H5Tequal(type, H5T_NATIVE_INT) == TRUE) {
                    printf("H5T_NATIVE_INT");
                } else if(H5Tequal(type, H5T_NATIVE_UINT) == TRUE) {
                    printf("H5T_NATIVE_UINT");
                } else if(H5Tequal(type, H5T_NATIVE_LONG) == TRUE) {
                    printf("H5T_NATIVE_LONG");
                } else if(H5Tequal(type, H5T_NATIVE_ULONG) == TRUE) {
                    printf("H5T_NATIVE_ULONG");
                } else if(H5Tequal(type, H5T_NATIVE_LLONG) == TRUE) {
                    printf("H5T_NATIVE_LLONG");
                } else if(H5Tequal(type, H5T_NATIVE_ULLONG) == TRUE) {
                    printf("H5T_NATIVE_ULLONG");
                } else {

                 /* byte order */
                 if(H5Tget_size(type)>1) {
                  order = H5Tget_order(type);
                  if (H5T_ORDER_LE == order) {
                   order_s = " little-endian";
                  } else if (H5T_ORDER_BE == order) {
                   order_s = " big-endian";
                  } else if (H5T_ORDER_VAX == order) {
                   order_s = " mixed-endian";
                  } else {
                   order_s = " unknown-byte-order";
                  }
                 } else {
                  order_s = "";
                 }

                 /* sign */
                 if ((sign=H5Tget_sign(type))>=0)
                 {
                  if (H5T_SGN_NONE == sign) {
                   sign_s = " unsigned";
                  } else if (H5T_SGN_2 == sign) {
                   sign_s = "";
                  } else {
                   sign_s = " unknown-sign";
                  }
                 } else {
                  sign_s = " unknown-sign";
                 }

                 /* print size, order, and sign  */
                 printf("%lu-bit%s%s integer",
                  (unsigned long)(8*H5Tget_size(type)), order_s, sign_s);
                }
                break;

        case H5T_FLOAT:
                if(H5Tequal(type, H5T_IEEE_F32BE) == TRUE) {
                    printf("H5T_IEEE_F32BE");
                } else if(H5Tequal(type, H5T_IEEE_F32LE) == TRUE) {
                    printf("H5T_IEEE_F32LE");
                } else if(H5Tequal(type, H5T_IEEE_F64BE) == TRUE) {
                    printf("H5T_IEEE_F64BE");
                } else if(H5Tequal(type, H5T_IEEE_F64LE) == TRUE) {
                    printf("H5T_IEEE_F64LE");
                } else if(H5Tequal(type, H5T_NATIVE_FLOAT) == TRUE) {
                    printf("H5T_NATIVE_FLOAT");
                } else if(H5Tequal(type, H5T_NATIVE_DOUBLE) == TRUE) {
                    printf("H5T_NATIVE_DOUBLE");
#if H5_SIZEOF_LONG_DOUBLE !=0
                } else if(H5Tequal(type, H5T_NATIVE_LDOUBLE) == TRUE) {
                    printf("H5T_NATIVE_LDOUBLE");
#endif
                } else {

                 /* byte order */
                 if(H5Tget_size(type)>1) {
                  order = H5Tget_order(type);
                  if (H5T_ORDER_LE == order) {
                   order_s = " little-endian";
                  } else if (H5T_ORDER_BE == order) {
                   order_s = " big-endian";
                  } else if (H5T_ORDER_VAX == order) {
                   order_s = " mixed-endian";
                  } else {
                   order_s = " unknown-byte-order";
                  }
                 } else {
                  order_s = "";
                 }

                 /* print size and byte order */
                 printf("%lu-bit%s floating-point",
                  (unsigned long)(8*H5Tget_size(type)), order_s);

                }
                break;

        case H5T_TIME:
            printf("H5T_TIME: not yet implemented");
            break;

        case H5T_STRING:
            /* Make a copy of type in memory in case when TYPE is on disk, the size
             * will be bigger than in memory.  This makes it easier to compare
             * types in memory. */
            tmp_type = H5Tcopy(type);
            size = H5Tget_size(tmp_type);
            str_pad = H5Tget_strpad(tmp_type);
            cset = H5Tget_cset(tmp_type);
            is_vlstr = H5Tis_variable_str(tmp_type);

            printf("H5T_STRING %s\n", dump_header_format->strblockbegin);
            indent += COL;

            indentation(indent + COL);
            if(is_vlstr)
                printf("%s H5T_VARIABLE;\n", STRSIZE);
            else
                printf("%s %d;\n", STRSIZE, (int) size);

            indentation(indent + COL);
            printf("%s ", STRPAD);
            if (str_pad == H5T_STR_NULLTERM)
                printf("H5T_STR_NULLTERM;\n");
            else if (str_pad == H5T_STR_NULLPAD)
                printf("H5T_STR_NULLPAD;\n");
            else if (str_pad == H5T_STR_SPACEPAD)
                printf("H5T_STR_SPACEPAD;\n");
            else
                printf("H5T_STR_ERROR;\n");

            indentation(indent + COL);
            printf("%s ", CSET);

            if (cset == H5T_CSET_ASCII)
                printf("H5T_CSET_ASCII;\n");
            else
                printf("unknown_cset;\n");

            str_type = H5Tcopy(H5T_C_S1);
            if(is_vlstr)
                H5Tset_size(str_type, H5T_VARIABLE);
            else
                H5Tset_size(str_type, size);
            H5Tset_cset(str_type, cset);
            H5Tset_strpad(str_type, str_pad);

            indentation(indent + COL);
            printf("%s ", CTYPE);

            /* Check C variable-length string first. Are the two types equal? */
            if (H5Tequal(tmp_type, str_type)) {
                printf("H5T_C_S1;\n");
                goto done;
            }

            /* Change the endianness and see if they're equal. */
            order = H5Tget_order(tmp_type);
            if(order==H5T_ORDER_LE)
                H5Tset_order(str_type, H5T_ORDER_LE);
            else if(order==H5T_ORDER_BE)
                H5Tset_order(str_type, H5T_ORDER_BE);

            if (H5Tequal(tmp_type, str_type)) {
                printf("H5T_C_S1;\n");
                goto done;
            }

            /* If not equal to C variable-length string, check Fortran type. */
            H5Tclose(str_type);
            str_type = H5Tcopy(H5T_FORTRAN_S1);
            H5Tset_cset(str_type, cset);
            H5Tset_size(str_type, size);
            H5Tset_strpad(str_type, str_pad);

            /* Are the two types equal? */
            if (H5Tequal(tmp_type, str_type)) {
                printf("H5T_FORTRAN_S1;\n");
                goto done;
            }

            /* Change the endianness and see if they're equal. */
            order = H5Tget_order(tmp_type);
            if(order==H5T_ORDER_LE)
                H5Tset_order(str_type, H5T_ORDER_LE);
            else if(order==H5T_ORDER_BE)
                H5Tset_order(str_type, H5T_ORDER_BE);

            if (H5Tequal(tmp_type, str_type)) {
                printf("H5T_FORTRAN_S1;\n");
                goto done;
            }

            /* Type doesn't match any of above. */
            printf("unknown_one_character_type;\n ");
            d_status = EXIT_FAILURE;

    done:
            H5Tclose(str_type);
            H5Tclose(tmp_type);

            indent -= COL;
            indentation(indent + COL);
            printf("%s", dump_header_format->strblockend);
            break;

        case H5T_BITFIELD:
            if (H5Tequal(type, H5T_STD_B8BE)==TRUE) {
                printf("H5T_STD_B8BE");
            } else if (H5Tequal(type, H5T_STD_B8LE)==TRUE) {
                printf("H5T_STD_B8LE");
            } else if (H5Tequal(type, H5T_STD_B16BE)==TRUE) {
                printf("H5T_STD_B16BE");
            } else if (H5Tequal(type, H5T_STD_B16LE)==TRUE) {
                printf("H5T_STD_B16LE");
            } else if (H5Tequal(type, H5T_STD_B32BE)==TRUE) {
                printf("H5T_STD_B32BE");
            } else if (H5Tequal(type, H5T_STD_B32LE)==TRUE) {
                printf("H5T_STD_B32LE");
            } else if (H5Tequal(type, H5T_STD_B64BE)==TRUE) {
                printf("H5T_STD_B64BE");
            } else if (H5Tequal(type, H5T_STD_B64LE)==TRUE) {
                printf("H5T_STD_B64LE");
            } else {
                printf("undefined bitfield");
                d_status = EXIT_FAILURE;
            }
            break;

        case H5T_OPAQUE:
            printf("\n");
            indentation(indent + COL);
            printf("H5T_OPAQUE;\n");
            indentation(indent + COL);
            printf("OPAQUE_TAG \"%s\";\n", H5Tget_tag(type));
            indentation(indent);
            break;

        case H5T_COMPOUND:
            nmembers = H5Tget_nmembers(type);
            printf("H5T_COMPOUND %s\n", dump_header_format->structblockbegin);

            for (i = 0; i < nmembers; i++) {
                mname = H5Tget_member_name(type, i);
                mtype = H5Tget_member_type(type, i);
                indentation(indent + COL);

                if (H5Tget_class(mtype) == H5T_COMPOUND)
                    indent += COL;

                print_datatype(mtype,0);

                if (H5Tget_class(mtype) == H5T_COMPOUND)
                    indent -= COL;

                printf(" \"%s\";\n", mname);
                free(mname);
            }

            indentation(indent);
            printf("%s", dump_header_format->structblockend);
            break;

        case H5T_REFERENCE:
            printf("H5T_REFERENCE");
            break;

        case H5T_ENUM:
            printf("H5T_ENUM %s\n", dump_header_format->enumblockbegin);
            indent += COL;
            indentation(indent + COL);
            super = H5Tget_super(type);
            print_datatype(super,0);
            printf(";\n");
            print_enum(type);
            indent -= COL;
            indentation(indent + COL);
            printf("%s", dump_header_format->enumblockend);
            break;

        case H5T_VLEN:
            printf("H5T_VLEN %s ", dump_header_format->vlenblockbegin);
            super = H5Tget_super(type);
            print_datatype(super,0);
            H5Tclose(super);

            /* Print closing */
            printf("%s", dump_header_format->vlenblockend);
            break;

        case H5T_ARRAY:
            /* Get array base type */
            super = H5Tget_super(type);

            /* Print lead-in */
            printf("H5T_ARRAY { ");

            /* Get array information */
            ndims = H5Tget_array_ndims(type);
            H5Tget_array_dims(type, dims, NULL);

            /* Print array dimensions */
            for (i = 0; i < ndims; i++)
                printf("[%d]", (int) dims[i]);

            printf(" ");

            /* Print base type */
            print_datatype(super,0);

            /* Close array base type */
            H5Tclose(super);

            /* Print closing */
            printf(" }");

            break;

        default:
            printf("unknown data type");
            d_status = EXIT_FAILURE;
            break;
        }
    } /* end else */
}


/*-------------------------------------------------------------------------
 * Function:    dump_datatype
 *
 * Purpose:     Dump the data type. Data type can be HDF5 predefined
 *              atomic data type or committed/transient data type.
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_datatype(hid_t type)
{
    indent += COL;

    indentation(indent);
    printf("%s %s ", dump_header_format->datatypebegin,
           dump_header_format->datatypeblockbegin);

    print_datatype(type,0);

    end_obj(dump_header_format->datatypeend,
        dump_header_format->datatypeblockend);
    indent -= COL;
}

/*-------------------------------------------------------------------------
 * Function:    dump_dataspace
 *
 * Purpose:     Dump the data space. Data space can be named data space,
 *              array, or others.
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_dataspace(hid_t space)
{
    hsize_t   size[H5DUMP_MAX_RANK];
    hsize_t   maxsize[H5DUMP_MAX_RANK];
    int       ndims = H5Sget_simple_extent_dims(space, size, maxsize);
    H5S_class_t space_type = H5Sget_simple_extent_type(space);
    int       i;

    indentation(indent + COL);
    printf("%s ", dump_header_format->dataspacebegin);

    switch (space_type) {
        case H5S_SCALAR:
        /* scalar dataspace */
        HDfprintf(stdout, "%s %s",
              dump_header_format->dataspacedescriptionbegin, S_SCALAR);
            break;
        case H5S_SIMPLE:
        /* simple dataspace */
        HDfprintf(stdout, "%s %s { %s %Hu",
              dump_header_format->dataspacedescriptionbegin, S_SIMPLE,
              dump_header_format->dataspacedimbegin, size[0]);

        for (i = 1; i < ndims; i++)
        HDfprintf(stdout, ", %Hu", size[i]);

        printf(" %s / ", dump_header_format->dataspacedimend);

        if (maxsize[0] == H5S_UNLIMITED)
        HDfprintf(stdout, "%s %s",
              dump_header_format->dataspacedimbegin,
              "H5S_UNLIMITED");
        else
        HDfprintf(stdout, "%s %Hu",
                          dump_header_format->dataspacedimbegin, maxsize[0]);

        for (i = 1; i < ndims; i++)
        if (maxsize[i] == H5S_UNLIMITED)
            HDfprintf(stdout, ", %s", "H5S_UNLIMITED");
        else
            HDfprintf(stdout, ", %Hu", maxsize[i]);

        printf(" %s }", dump_header_format->dataspacedimend);

        break;
        case H5S_COMPLEX:
        printf("%s not yet implemented %s\n", BEGIN, END);
            break;
        case H5S_NO_CLASS:
        default:
        printf("%s unknown dataspace %s\n", BEGIN, END);
    }

    end_obj(dump_header_format->dataspaceend,
        dump_header_format->dataspaceblockend);
}

/*-------------------------------------------------------------------------
 * Function:    dump_attr
 *
 * Purpose:     dump the attribute
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dump_attr(hid_t attr, const char *attr_name, void UNUSED * op_data)
{
    hid_t       attr_id, type, space;
    herr_t      ret = SUCCEED;

    indentation(indent);
    begin_obj(dump_header_format->attributebegin, attr_name,
          dump_header_format->attributeblockbegin);

    if ((attr_id = H5Aopen_name(attr, attr_name)) < 0) {
    indentation(indent + COL);
        error_msg(progname, "unable to open attribute \"%s\"\n", attr_name);
    indentation(indent);
    end_obj(dump_header_format->attributeend,
        dump_header_format->attributeblockend);
    d_status = EXIT_FAILURE;
    ret = FAIL;
    } else {
    type = H5Aget_type(attr_id);
    space = H5Aget_space(attr_id);
    dump_datatype(type);
    dump_dataspace(space);

    if (display_oid)
        dump_oid(attr_id);

    if (display_data || display_attr_data)
        dump_data(attr_id, ATTRIBUTE_DATA, NULL, display_ai);

    H5Tclose(type);
    H5Sclose(space);
    H5Aclose(attr_id);
    indentation(indent);
    end_obj(dump_header_format->attributeend,
        dump_header_format->attributeblockend);
    }

    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    dump_selected_attr
 *
 * Purpose:     dump the selected attribute
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dump_selected_attr(hid_t loc_id, const char *name)
{
    int j;
    char *obj_name;
    const char *attr_name;
    hid_t  oid, attr_id, type, space;
    H5G_stat_t statbuf;

    j = (int)strlen(name) - 1;
    obj_name = malloc((size_t)j + 2);

    /* find the last / */
    while (name[j] != '/' && j >= 0)
    j--;

    /* object name */
    if (j == -1) {
    strcpy(obj_name, "/");
    } else {
        strncpy(obj_name, name, (size_t)j + 1);
        obj_name[j + 1] = '\0';
    }

    attr_name = name + j + 1;
    begin_obj(dump_header_format->attributebegin, name,
          dump_header_format->attributeblockbegin);
    H5Gget_objinfo(loc_id, obj_name, FALSE, &statbuf);

    switch (statbuf.type) {
    case H5G_GROUP:
    if ((oid = H5Gopen(loc_id, obj_name)) < 0) {
        indentation(COL);
            error_msg(progname, "unable to open group \"%s\"\n", obj_name);
        end_obj(dump_header_format->attributeend,
            dump_header_format->attributeblockend);
        d_status = EXIT_FAILURE;
        return FAIL;
    }
    break;

    case H5G_DATASET:
    if ((oid = H5Dopen(loc_id, obj_name)) < 0) {
        indentation(COL);
            error_msg(progname, "unable to open dataset \"%s\"\n", obj_name);
        end_obj(dump_header_format->attributeend,
            dump_header_format->attributeblockend);
        d_status = EXIT_FAILURE;
        return FAIL;
    }
    break;

    case H5G_TYPE:
    if ((oid = H5Topen(loc_id, obj_name)) < 0) {
        indentation(COL);
            error_msg(progname, "unable to open datatype \"%s\"\n", obj_name);
        end_obj(dump_header_format->attributeend,
            dump_header_format->attributeblockend);
        d_status = EXIT_FAILURE;
        return FAIL;
    }
    break;

    default:
    indentation(COL);
        error_msg(progname, "unable to open unknown \"%s\"\n", obj_name);
    end_obj(dump_header_format->attributeend,
        dump_header_format->attributeblockend);
    d_status = EXIT_FAILURE;
    return FAIL;
    }

    if ((attr_id = H5Aopen_name(oid, attr_name)) >= 0) {
    type = H5Aget_type(attr_id);
    space = H5Aget_space(attr_id);
    dump_datatype(type);
    dump_dataspace(space);

    if (display_oid)
        dump_oid(attr_id);

    if (display_data || display_attr_data)
        dump_data(attr_id, ATTRIBUTE_DATA, NULL, display_ai);

    H5Tclose(type);
    H5Sclose(space);
    H5Aclose(attr_id);
    end_obj(dump_header_format->attributeend,
        dump_header_format->attributeblockend);
    } else {
    indentation(COL);
        error_msg(progname, "unable to open attribute \"%s\"\n", obj_name);
    end_obj(dump_header_format->attributeend,
        dump_header_format->attributeblockend);
    d_status = EXIT_FAILURE;
    }

    switch (statbuf.type) {
    case H5G_GROUP:
    if (H5Gclose(oid) < 0) {
        d_status = EXIT_FAILURE;
        return FAIL;
    }
    break;

    case H5G_DATASET:
    if (H5Dclose(oid) < 0) {
        d_status = EXIT_FAILURE;
        return FAIL;
    }
    break;

    case H5G_TYPE:
    if (H5Tclose(oid) < 0) {
        d_status = EXIT_FAILURE;
        return FAIL;
    }
    break;
    default:
    d_status = EXIT_FAILURE;
    return FAIL;
    }

    free(obj_name);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    dump_all
 *
 * Purpose:     Dump everything in the specified object
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *          RMcG, November 2000
 *          Added XML support. Also, optionally checks the op_data
 *          argument.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dump_all(hid_t group, const char *name, void * op_data)
{
    hid_t       obj;
    char       *obj_path = NULL;        /* Full path of object */
    H5G_stat_t  statbuf;
    herr_t      ret = SUCCEED;

    /* Stat the object */
    if(H5Gget_objinfo(group, name, FALSE, &statbuf) < 0) {
        error_msg(progname, "unable to get object information for \"%s\"\n", name);
        d_status = EXIT_FAILURE;
        ret = FAIL;
        goto done;
    } /* end if */

    if (*(int *)op_data != H5G_UNKNOWN && statbuf.type != *(int *) op_data)
        goto done;

    /* Build the object's path name */
    obj_path = HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    HDassert(obj_path);
    HDstrcpy(obj_path, prefix);
    HDstrcat(obj_path, "/");
    HDstrcat(obj_path, name);

    switch (statbuf.type) {
        case H5G_LINK:
        {
            char       *targbuf;

            indentation(indent);
            targbuf = HDmalloc(statbuf.linklen);
            HDassert(targbuf);

            if (!doxml) {
                begin_obj(dump_header_format->softlinkbegin, name,
                          dump_header_format->softlinkblockbegin);
                indentation(indent + COL);
            }

            if (H5Gget_linkval(group, name, statbuf.linklen, targbuf) < 0) {
                error_msg(progname, "unable to get link value\n");
                d_status = EXIT_FAILURE;
                ret = FAIL;
            } else {
                /* print the value of a soft link */
                if (!doxml) {
                    /* Standard DDL: no modification */
                    printf("LINKTARGET \"%s\"\n", targbuf);
                } else {
                    /* XML */
                    char linkxid[100];
                    char parentxid[100];
                    char targetxid[100];
                    char *t_prefix = xml_escape_the_name(HDstrcmp(prefix,"") ? prefix : "/");
                    char *t_name = xml_escape_the_name(name);
                    char *t_targbuf = xml_escape_the_name(targbuf);
                    char *t_obj_path = xml_escape_the_name(obj_path);
                    char *t_link_path;
                    int res;

                    t_link_path = HDmalloc(HDstrlen(prefix) + statbuf.linklen + 1);
                    if(targbuf[0] == '/')
                        HDstrcpy(t_link_path, targbuf);
                    else {
                        HDstrcpy(t_link_path, prefix);
                        HDstrcat(HDstrcat(t_link_path, "/"), targbuf);
                    } /* end else */

                    /* Create OBJ-XIDs for the parent and object */
                    xml_name_to_XID(t_obj_path, linkxid, sizeof(linkxid), 1);
                    xml_name_to_XID(prefix, parentxid, sizeof(parentxid), 1);

                    /* Try to create an OBJ-XID for the object pointed to */
                    res = xml_name_to_XID(t_link_path, targetxid, sizeof(targetxid), 0);
                    if (res == 0) {
                        /* target obj found */
                        printf("<%sSoftLink LinkName=\"%s\" "
                               "OBJ-XID=\"%s\" "
                               "H5SourcePath=\"%s\" "
                               "TargetPath=\"%s\" TargetObj=\"%s\" "
                               "Parents=\"%s\" H5ParentPaths=\"%s\" />\n",
                                xmlnsprefix,
                                t_name,         /* LinkName */
                                linkxid,        /* OBJ-XID */
                                t_obj_path,     /* H5SourcePath */
                                t_targbuf,      /* TargetPath */
                                targetxid,      /* TargetObj */
                                parentxid,      /* Parents */
                                t_prefix);      /* H5ParentPaths */
                    } else {
                        /* dangling link -- omit from xml attributes */
                        printf("<%sSoftLink LinkName=\"%s\" "
                               "OBJ-XID=\"%s\" "
                               "H5SourcePath=\"%s\" "
                               "TargetPath=\"%s\"  "
                               "Parents=\"%s\" H5ParentPaths=\"%s\" />\n",
                                xmlnsprefix,
                                t_name,         /* LinkName */
                                linkxid,        /* OBJ-XID */
                                t_obj_path,     /* H5SourcePath */
                                t_targbuf,      /* TargetPath */
                                parentxid,      /* Parents */
                                t_prefix);      /* H5ParentPaths */
                    }

                    HDfree(t_prefix);
                    HDfree(t_name);
                    HDfree(t_targbuf);
                    HDfree(t_obj_path);
                    HDfree(t_link_path);
                }
            }

            if (!doxml) {
                indentation(indent);
                end_obj(dump_header_format->softlinkend,
                        dump_header_format->softlinkblockend);
            }

            HDfree(targbuf);
            break;
        }

        case H5G_GROUP:
            if ((obj = H5Gopen(group, name)) < 0) {
                error_msg(progname, "unable to dump group \"%s\"\n", name);
                d_status = EXIT_FAILURE;
                ret = FAIL;
            } else {
                char *old_prefix;                /* Pointer to previous prefix */

                /* Keep copy of prefix before iterating into group */
                old_prefix = HDstrdup(prefix);
                HDassert(old_prefix);

                /* Append group name to prefix */
                add_prefix(&prefix, &prefix_len, name);

                /* Iterate into group */
                dump_function_table->dump_group_function(obj, name);

                /* Restore old prefix name */
                HDstrcpy(prefix, old_prefix);
                HDfree(old_prefix);

                /* Close group */
                H5Gclose(obj);
            }

            break;

        case H5G_DATASET:
            if ((obj = H5Dopen(group, name)) >= 0) {
                /* hard link */
                H5Gget_objinfo(obj, ".", TRUE, &statbuf);

                if (statbuf.nlink > 1) {
                    obj_t  *found_obj;    /* Found object */

                    found_obj = search_obj(dset_table, statbuf.objno);

                    if (found_obj == NULL) {
                        indentation(indent);
                        begin_obj(dump_header_format->datasetbegin, name,
                                  dump_header_format->datasetblockbegin);
                        indentation(indent + COL);
                        error_msg(progname,
                                  "internal error (file %s:line %d)\n",
                                  __FILE__, __LINE__);
                        indentation(indent);
                        end_obj(dump_header_format->datasetend,
                                dump_header_format->datasetblockend);
                        d_status = EXIT_FAILURE;
                        ret = FAIL;
                        H5Dclose(obj);
                        goto done;
                    } else if (found_obj->displayed) {
                        indentation(indent);

                        if (!doxml) {
                            begin_obj(dump_header_format->datasetbegin, name,
                                      dump_header_format->datasetblockbegin);
                            indentation(indent + COL);
                            printf("%s \"%s\"\n", HARDLINK, found_obj->objname);
                            indentation(indent);
                            end_obj(dump_header_format->datasetend,
                                    dump_header_format->datasetblockend);
                        } else {
                            /* the XML version */
                            char *t_obj_path = xml_escape_the_name(obj_path);
                            char *t_prefix = xml_escape_the_name(HDstrcmp(prefix,"") ? prefix : "/");
                            char *t_name = xml_escape_the_name(name);
                            char dsetxid[100];
                            char parentxid[100];
                            char pointerxid[100];

                            /* Create OBJ-XIDs for the parent and object */
                            xml_name_to_XID(obj_path, dsetxid, sizeof(dsetxid), 1);
                            xml_name_to_XID(prefix, parentxid, sizeof(parentxid), 1);

                            printf("<%sDataset Name=\"%s\" OBJ-XID=\"%s-%d\" "
                                   "H5Path=\"%s\" Parents=\"%s\" "
                                   "H5ParentPaths=\"%s\">\n",
                                    xmlnsprefix,
                                    t_name,                     /* Dataset Name */
                                    dsetxid, get_next_xid(),    /* OBJ-XID */
                                    t_obj_path,                 /* H5Path */
                                    parentxid,                  /* Parents */
                                    t_prefix);                  /* H5ParentPaths */

                            indentation(indent + COL);
                            xml_name_to_XID(found_obj->objname, pointerxid, sizeof(pointerxid), 1);
                            printf("<%sDatasetPtr OBJ-XID=\"%s\" H5Path=\"%s\"/>\n",
                                    xmlnsprefix,
                                    pointerxid,t_obj_path);
                            indentation(indent);
                            printf("</%sDataset>\n", xmlnsprefix);

                            HDfree(t_name);
                            HDfree(t_obj_path);
                            HDfree(t_prefix);
                        }

                        H5Dclose(obj);
                        goto done;
                    } else {
                        found_obj->displayed = TRUE;
                    }
                } /* end if */

                dump_function_table->dump_dataset_function(obj, name, NULL);
                H5Dclose(obj);
            } else {
                error_msg(progname, "unable to dump dataset \"%s\"\n", name);
                d_status = EXIT_FAILURE;
                ret = FAIL;
            }
            break;

        case H5G_TYPE:
            if ((obj = H5Topen(group, name)) < 0) {
                error_msg(progname, "unable to dump data type \"%s\"\n", name);
                d_status = EXIT_FAILURE;
                ret = FAIL;
            } else {
                dump_function_table->dump_named_datatype_function(obj, name);
                H5Tclose(obj);
            }
            break;

        default:
            error_msg(progname, "unknown object \"%s\"\n", name);
            d_status = EXIT_FAILURE;
            ret = FAIL;
    }

done:
    if(obj_path)
        HDfree(obj_path);
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    dump_named_datatype
 *
 * Purpose:     Dump named data type
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: pvn March 27, 2006
 *  add printing of attributes
 *
 *-------------------------------------------------------------------------
 */
static void
dump_named_datatype(hid_t type, const char *name)
{
 indentation(indent);
 printf("%s \"%s\" %s", dump_header_format->datatypebegin, name,
  dump_header_format->datatypeblockbegin);

 if (H5Tget_class(type) == H5T_COMPOUND) {
  print_datatype(type,1);
 } else {
  indentation(indent + COL);
  print_datatype(type,1);
  printf(";\n");
 }

 /* print attributes */
 indent += COL;
 H5Aiterate(type, NULL, dump_attr, NULL);
 indent -= COL;

 end_obj(dump_header_format->datatypeend,
  dump_header_format->datatypeblockend);
}

/*-------------------------------------------------------------------------
 * Function:    dump_group
 *
 * Purpose:     Dump everything within the specified group
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *      Call to dump_all -- add parameter to select everything.
 *
 *-------------------------------------------------------------------------
 */
static void
dump_group(hid_t gid, const char *name)
{
    H5G_stat_t  statbuf;
    hid_t       dset, type;
    char        type_name[1024], *tmp;
    int         xtype = H5G_UNKNOWN; /* dump all */

    tmp = HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    HDstrcpy(tmp, prefix);
    indentation(indent);
    begin_obj(dump_header_format->groupbegin, name,
          dump_header_format->groupblockbegin);
    indent += COL;

    if (display_oid)
        dump_oid(gid);

    dump_comment(gid);

    if (!HDstrcmp(name, "/") && unamedtype) {
        unsigned u;             /* Local index variable */

    /* dump unamed type in root group */
    for (u = 0; u < type_table->nobjs; u++)
        if (!type_table->objs[u].recorded) {
                unsigned long   objno[2];   /*object number         */

        dset = H5Dopen(gid, type_table->objs[u].objname);
        type = H5Dget_type(dset);

                objno[0] = (unsigned long)(type_table->objs[u].objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
                objno[1] = (unsigned long)(type_table->objs[u].objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
        sprintf(type_name, "#%lu:%lu", objno[0], objno[1]);
                dump_function_table->dump_named_datatype_function(type, type_name);

        H5Tclose(type);
        H5Dclose(dset);
        }
    } /* end if */

    H5Gget_objinfo(gid, ".", TRUE, &statbuf);

    if (statbuf.nlink > 1) {
        obj_t  *found_obj;    /* Found object */

        found_obj = search_obj(group_table, statbuf.objno);

    if (found_obj == NULL) {
        indentation(indent);
            error_msg(progname, "internal error (file %s:line %d)\n",
                      __FILE__, __LINE__);
        d_status = EXIT_FAILURE;
    } else if (found_obj->displayed) {
        indentation(indent);
        printf("%s \"%s\"\n", HARDLINK, found_obj->objname);
    } else {
        found_obj->displayed = TRUE;
        H5Aiterate(gid, NULL, dump_attr, NULL);
        H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
    }
    } else {
    H5Aiterate(gid, NULL, dump_attr, NULL);
    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
    }

    indent -= COL;
    indentation(indent);
    end_obj(dump_header_format->groupend, dump_header_format->groupblockend);
    free(tmp);
}

/*-------------------------------------------------------------------------
 * Function:    dump_dataset
 *
 * Purpose:     Dump the specified data set
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: pvn, 2004, added dcpl dump
 *
 *-------------------------------------------------------------------------
 */
static void
dump_dataset(hid_t did, const char *name, struct subset_t *sset)
{
    hid_t   type, space, dcpl_id;

    indentation(indent);
    begin_obj(dump_header_format->datasetbegin, name,
          dump_header_format->datasetblockbegin);
    type = H5Dget_type(did);
    space = H5Dget_space(did);
    dcpl_id = H5Dget_create_plist(did);

    dump_comment(did);
    dump_datatype(type);
    dump_dataspace(space);

    if (display_oid)
        dump_oid(did);

    if (display_dcpl)
        dump_dcpl(dcpl_id, type, did);

    if (display_data)
    switch (H5Tget_class(type)) {
            case H5T_TIME:
                indentation(indent + COL);
                printf("DATA{ not yet implemented.}\n");
                break;

            case H5T_INTEGER:
            case H5T_FLOAT:
            case H5T_STRING:
            case H5T_BITFIELD:
            case H5T_OPAQUE:
            case H5T_COMPOUND:
            case H5T_REFERENCE:
            case H5T_ENUM:
            case H5T_VLEN:
            case H5T_ARRAY:
                dump_data(did, DATASET_DATA, sset, display_ai);
                break;

            default:
                break;
    }

    if ( !bin_output )
    {
        
        indent += COL;
        H5Aiterate(did, NULL, dump_attr, NULL);
        indent -= COL;
        
    }
    H5Tclose(type);
    H5Sclose(space);
    H5Pclose(dcpl_id);
    indentation(indent);
    end_obj(dump_header_format->datasetend,
        dump_header_format->datasetblockend);
}

/*-------------------------------------------------------------------------
 * Function:    dump_dims
 *
 * Purpose:     Dump the dimensions handed to it in a comma separated list
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 27. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_dims(hsize_t *s, int dims)
{
    int i;

    for (i = 0; i < dims; i++) {
        printf(HSIZE_T_FORMAT, s[i]);

        if (i + 1 != dims)
            printf(", ");
    }
}


/*-------------------------------------------------------------------------
 * Function:    dump_subsetting_header
 *
 * Purpose:     Dump the subsetting header like specified in the DDL.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 27. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_subsetting_header(struct subset_t *sset, int dims)
{
    indentation(indent);
    printf("%s %s\n", dump_header_format->subsettingbegin,
           dump_header_format->subsettingblockbegin);

    indent += COL;
    indentation(indent);
    printf("%s %s ", dump_header_format->startbegin,
           dump_header_format->startblockbegin);
    dump_dims((hsize_t *)sset->start, dims);
    printf("%s %s\n", dump_header_format->startend,
           dump_header_format->startblockend);

    indentation(indent);
    printf("%s %s ", dump_header_format->stridebegin,
           dump_header_format->strideblockbegin);
    dump_dims(sset->stride, dims);
    printf("%s %s\n", dump_header_format->strideend,
           dump_header_format->strideblockend);

    indentation(indent);
    printf("%s %s ", dump_header_format->countbegin,
           dump_header_format->countblockbegin);

    if (sset->count)
        dump_dims(sset->count, dims);
    else
        printf("DEFAULT");

    printf("%s %s\n", dump_header_format->countend,
           dump_header_format->countblockend);

    indentation(indent);
    printf("%s %s ", dump_header_format->blockbegin,
           dump_header_format->blockblockbegin);

    if (sset->block)
        dump_dims(sset->block, dims);
    else
        printf("DEFAULT");

    printf("%s %s\n", dump_header_format->blockend,
           dump_header_format->blockblockend);
}

/*-------------------------------------------------------------------------
 * Function:    dump_data
 *
 * Purpose:     Dump attribute or dataset data
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: pvn, print the matrix indices
 *  Albert Cheng, 2004/11/18
 *  Add --string printing for attributes too.
 *
 *-------------------------------------------------------------------------
 */
static void
dump_data(hid_t obj_id, int obj_data, struct subset_t *sset, int display_index)
{
    h5tool_format_t   *outputformat = &dataformat;
    int         status = -1;
    void       *buf;
    hid_t       space, type, p_type;
    H5S_class_t space_type;
    int         ndims, i;
    hsize_t     size[64], nelmts = 1, alloc_size;
    int         depth;
    int         stdindent = COL;    /* should be 3 */

    if (fp_format)
    {
        outputformat->fmt_double = fp_format;
        outputformat->fmt_float = fp_format;
    }

    if (nCols==0) {
        outputformat->line_ncols = 65535;
        outputformat->line_per_line = 1;
    }
    else
        outputformat->line_ncols = nCols;
    outputformat->do_escape=display_escape;
    /* print the matrix indices */
    outputformat->pindex=display_index;

    /* do not print indices for regions */
    if(obj_data == DATASET_DATA) 
    {
        hid_t f_type = H5Dget_type(obj_id);
        
        if (H5Tequal(f_type, H5T_STD_REF_DSETREG)) 
        {
            outputformat->pindex = 0;
        }
        H5Tclose(f_type);
    }

    if (outputformat->pindex) {
        outputformat->idx_fmt   = "(%s): ";
        outputformat->idx_n_fmt = HSIZE_T_FORMAT;
        outputformat->idx_sep   = ",";
        outputformat->line_pre  = "%s";
    }

    indent += COL;

    /*
     * the depth will tell us how far we need to indent extra.  we use to just
     * use indent but with the merging of the tools lib we have to do
     * something different for the lib funtions... the normal indentation is 6
     * so when we don't need any extra indentation, depth will be 0.
     */
    depth = indent / stdindent + 1;

    if (sset && obj_data == DATASET_DATA) {
        hid_t f_space = H5Dget_space(obj_id);

        dump_subsetting_header(sset, H5Sget_simple_extent_ndims(f_space));
        H5Sclose(f_space);

        /* recalculate the depth of the data */
        depth = indent / stdindent + 1;
    }

    indentation(indent);
    begin_obj(dump_header_format->databegin, (const char *)NULL,
          dump_header_format->datablockbegin);

    /* Print all the values. */
    if (obj_data == DATASET_DATA) {
        hid_t       f_type = H5Dget_type(obj_id);
        char        string_prefix[64];
        h5tool_format_t    string_dataformat;

        if (display_char && H5Tget_size(f_type) == 1 && H5Tget_class(f_type) == H5T_INTEGER) {
            /*
             * Print 1-byte integer data as an ASCII character string
             * instead of integers if the `-r' or `--string' command-line
             * option was given.
             *
             * We don't want to modify the global dataformat, so make a
             * copy of it instead.
             */
            string_dataformat = *outputformat;
            string_dataformat.idx_fmt = " ";
            string_dataformat.line_multi_new = 1;
            string_dataformat.line_1st = "        %s\"";
            string_dataformat.line_pre = "        %s";
            string_dataformat.line_cont = "        %s";
            string_dataformat.str_repeat = 8;
            string_dataformat.ascii = TRUE;
            string_dataformat.elmt_suf1 = "";
            string_dataformat.elmt_suf2 = "";
            string_dataformat.line_indent = "";
            strcpy(string_prefix, string_dataformat.line_pre);
            strcat(string_prefix, "\"");
            string_dataformat.line_pre = string_prefix;
            string_dataformat.line_suf = "\"";
            outputformat = &string_dataformat;
        }

        status = h5tools_dump_dset(stdout, outputformat, obj_id, -1, sset, depth);

        H5Tclose(f_type);
    } else {
        /* need to call h5tools_dump_mem for the attribute data */
        space = H5Aget_space(obj_id);
        space_type = H5Sget_simple_extent_type(space);
        if(space_type == H5S_NO_CLASS || space_type == H5S_COMPLEX) {
            status = SUCCEED;
        } else {
            char        string_prefix[64];
            h5tool_format_t    string_dataformat;

            type = H5Aget_type(obj_id);
            p_type = h5tools_get_native_type(type);

            ndims = H5Sget_simple_extent_dims(space, size, NULL);

            for (i = 0; i < ndims; i++)
                nelmts *= size[i];

            alloc_size = nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type));
            assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/

            buf = malloc((size_t)alloc_size);
            assert(buf);

            if (H5Aread(obj_id, p_type, buf) >= 0)
                if (display_char && H5Tget_size(type) == 1 && H5Tget_class(type) == H5T_INTEGER) {
                    /*
                     * Print 1-byte integer data as an ASCII character string
                     * instead of integers if the `-r' or `--string' command-line
                     * option was given.
                     *
                     * We don't want to modify the global dataformat, so make a
                     * copy of it instead.
                     */
                    string_dataformat = *outputformat;
                    string_dataformat.idx_fmt = " ";
                    string_dataformat.line_multi_new = 1;
                    string_dataformat.line_1st = "        %s\"";
                    string_dataformat.line_pre = "        %s";
                    string_dataformat.line_cont = "        %s";
                    string_dataformat.str_repeat = 8;
                    string_dataformat.ascii = TRUE;
                    string_dataformat.elmt_suf1 = "";
                    string_dataformat.elmt_suf2 = "";
                    string_dataformat.line_indent = "";
                    strcpy(string_prefix, string_dataformat.line_pre);
                    strcat(string_prefix, "\"");
                    string_dataformat.line_pre = string_prefix;
                    string_dataformat.line_suf = "\"";
                    outputformat = &string_dataformat;
                }

            status = h5tools_dump_mem(stdout, outputformat, obj_id, p_type,
                                    space, buf, depth);

            free(buf);
            H5Tclose(p_type);
            H5Tclose(type);
        }
        H5Sclose(space);
    }

    if (status == FAIL) {
        indentation(indent + COL);
        error_msg(progname, "unable to print data\n");
        d_status = EXIT_FAILURE;
    }

    indentation(indent);
    end_obj(dump_header_format->dataend, dump_header_format->datablockend);
    indent -= COL;

    if (sset && obj_data == DATASET_DATA) {
        indentation(indent);
        end_obj(dump_header_format->subsettingend,
                dump_header_format->subsettingblockend);
        indent -= COL;
    }
}

/*-------------------------------------------------------------------------
 * Function:    dump_oid
 *
 * Purpose:     Prints the object ids
 *
 * Return:      void
 *
 * Programmer:  Patrick Lu
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_oid(hid_t oid)
{
    indentation(indent + COL);
    printf("%s %s %d %s\n", OBJID, BEGIN, oid, END);
}

/*-------------------------------------------------------------------------
 * Function:    dump_comment
 *
 * Purpose:     prints the comment for the the object name
 *
 * Return:      void
 *
 * Programmer:  pvn
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void dump_comment(hid_t obj_id)
{
    char comment[50];

    comment[0] = '\0';
    H5Gget_comment(obj_id, ".", sizeof(comment), comment);

    if (comment[0]) {
        indentation(indent);
        printf("COMMENT \"%s\"\n", comment);
    }
}


/*-------------------------------------------------------------------------
 * Function:    dump_fill_value
 *
 * Purpose:     prints the fill value
 *
 * Return:      void
 *
 * Programmer:  pvn
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void dump_fill_value(hid_t dcpl,hid_t type_id, hid_t obj_id)
{
    h5tools_context_t   ctx;            /*print context     */
    size_t            size;
    void              *buf=NULL;
    hsize_t           nelmts=1;
    h5tool_format_t          *outputformat = &dataformat;
    hid_t             n_type;

    memset(&ctx, 0, sizeof(ctx));
    ctx.indent_level=2;

    n_type = h5tools_get_native_type(type_id);

    size = H5Tget_size(n_type);
    buf = malloc(size);

    H5Pget_fill_value(dcpl, n_type, buf);

    h5tools_dump_simple_data(stdout, outputformat, obj_id, &ctx, START_OF_DATA | END_OF_DATA, nelmts, n_type, buf);

    H5Tclose(n_type);

    if (buf)
        free (buf);

}


/*-------------------------------------------------------------------------
 * Function:    dump_dcpl
 *
 * Purpose:     prints several dataset create property list properties
 *
 * Return:      void
 *
 * Programmer:  pvn
 *
 * Modifications: pvn, March 28, 2008
 *   Add a COMPRESSION ratio information for cases when filters are present
 *
 *-------------------------------------------------------------------------
 */
static void
dump_dcpl(hid_t dcpl_id,hid_t type_id, hid_t obj_id)
{
    int              nfilters;       /* number of filters */
    unsigned         filt_flags;     /* filter flags */
    H5Z_filter_t     filtn;          /* filter identification number */
    unsigned         cd_values[20];  /* filter client data values */
    size_t           cd_nelmts;      /* filter client number of values */
    char             f_name[256];    /* filter name */
    unsigned         szip_options_mask;
    unsigned         szip_pixels_per_block;
    hsize_t          chsize[64];     /* chunk size in elements */
    int              rank;           /* rank */
    char                name[256];          /* external file name       */
    off_t            offset;         /* offset of external file     */
    hsize_t          size;           /* size of external file   */
    H5D_fill_value_t fvstatus;
    H5D_alloc_time_t at;
    H5D_fill_time_t  ft;
    hsize_t          storage_size;
    haddr_t          ioffset;
    int              i, next;
    unsigned         j;

    storage_size=H5Dget_storage_size(obj_id);
    nfilters = H5Pget_nfilters(dcpl_id);
    ioffset=H5Dget_offset(obj_id);
    next=H5Pget_external_count(dcpl_id);
    strcpy(f_name,"\0");

    /*-------------------------------------------------------------------------
    * STORAGE_LAYOUT
    *-------------------------------------------------------------------------
    */
    indentation(indent + COL);
    printf("%s %s\n", STORAGE_LAYOUT, BEGIN);

    if (H5D_CHUNKED == H5Pget_layout(dcpl_id)) {
        /*start indent */
        indent += COL;
        indentation(indent + COL);
        printf("%s ", CHUNKED);

        rank = H5Pget_chunk(dcpl_id,NELMTS(chsize),chsize);
        HDfprintf(stdout,"%s %Hu", dump_header_format->dataspacedimbegin, chsize[0]);
        for ( i=1; i<rank; i++)
            HDfprintf(stdout, ", %Hu", chsize[i]);
        printf(" %s\n", dump_header_format->dataspacedimend);
        indentation(indent + COL);

        /* if there are filters, print a compression ratio */
        if ( nfilters )
        {

            hid_t sid = H5Dget_space( obj_id );
            hid_t tid = H5Dget_type( obj_id );
            size_t datum_size = H5Tget_size( tid );
            hsize_t dims[H5S_MAX_RANK];
            int ndims = H5Sget_simple_extent_dims( sid, dims, NULL);  
            hsize_t nelmts = 1;
            hsize_t size;
            double ratio = 0;
            hssize_t a, b;
            int ok = 0;

            /* only print the compression ratio for these filters */
            for ( i = 0; i < nfilters; i++) 
            {
                cd_nelmts = NELMTS(cd_values);
                filtn = H5Pget_filter(dcpl_id, (unsigned)i, &filt_flags, &cd_nelmts,
                    cd_values, sizeof(f_name), f_name);
                
                switch (filtn) 
                {
                case H5Z_FILTER_DEFLATE:
                case H5Z_FILTER_SZIP:
                    ok = 1;
                    break;
                }
            }
            
            if (ndims && ok )
            {
                
                for (i = 0; i < ndims; i++)
                {
                    nelmts *= dims[i];
                }
                size = nelmts * datum_size;

                a = size; b = storage_size;

                /* compression ratio = uncompressed size /  compressed size */

                if (b!=0)
                    ratio = (double) a / (double) b;
              
                HDfprintf(stdout, "SIZE %Hu (%.3f:1 COMPRESSION)\n ", storage_size, ratio);
                
            }
            else
                HDfprintf(stdout, "SIZE %Hu\n ", storage_size);


            H5Sclose(sid);
            H5Tclose(tid);
            
        }
        else
        {
            HDfprintf(stdout, "SIZE %Hu\n ", storage_size);
        }


        /*end indent */
        indent -= COL;
        indentation(indent + COL);
        printf("%s\n",END);
    }
    else if (H5D_COMPACT == H5Pget_layout(dcpl_id)) {
        /*start indent */
        indent += COL;
        indentation(indent + COL);
        printf("%s\n", COMPACT);

        indentation(indent + COL);
        HDfprintf(stdout, "SIZE %Hu\n", storage_size);

        /*end indent */
        indent -= COL;
        indentation(indent + COL);
        printf("%s\n",END);
    }
    else if (H5D_CONTIGUOUS == H5Pget_layout(dcpl_id)) {
        /*-------------------------------------------------------------------------
        * EXTERNAL_FILE
        *-------------------------------------------------------------------------
        */
        if (next) {
            /*start indent */
            indent += COL;
            indentation(indent + COL);
            printf("%s\n", CONTIGUOUS);

            indentation(indent + COL);
            printf("%s %s\n", EXTERNAL, BEGIN);

            /*start indent */
            indent += COL;
            for ( j=0; j<(unsigned)next; j++) {
                H5Pget_external(dcpl_id,j,sizeof(name),name,&offset,&size);
                indentation(indent + COL);
                HDfprintf(stdout,"FILENAME %s SIZE %Hu OFFSET %ld\n",name,size,offset);
            }
            /*end indent */
            indent -= COL;
            indentation(indent + COL);
            printf("%s\n",END);

            /*end indent */
            indent -= COL;
            indentation(indent + COL);
            printf("%s\n",END);
        }
        else {
            /*start indent */
            indent += COL;
            indentation(indent + COL);
            printf("%s\n", CONTIGUOUS);

            indentation(indent + COL);
            HDfprintf(stdout,"SIZE %Hu\n", storage_size);
            indentation(indent + COL);
            HDfprintf(stdout,"OFFSET %Hu\n", ioffset);

            /*end indent */
            indent -= COL;
            indentation(indent + COL);
            printf("%s\n",END);
        }
    }
   /*-------------------------------------------------------------------------
    * FILTERS
    *-------------------------------------------------------------------------
    */    

    indentation(indent + COL);
    printf("%s %s\n", FILTERS, BEGIN);
    indent += COL;

    if (nfilters) {
        for (i=0; i<nfilters; i++) {
            cd_nelmts = NELMTS(cd_values);
            filtn = H5Pget_filter(dcpl_id, (unsigned)i, &filt_flags, &cd_nelmts,
                cd_values, sizeof(f_name), f_name);

            switch (filtn) {
                case H5Z_FILTER_DEFLATE:
                    indentation(indent + COL);
                    printf("%s %s %s %d %s\n", DEFLATE, BEGIN, DEFLATE_LEVEL, cd_values[0], END);
                    break;
                case H5Z_FILTER_SHUFFLE:
                    indentation(indent + COL);
                    printf("%s\n", SHUFFLE);
                    break;
                case H5Z_FILTER_FLETCHER32:
                    indentation(indent + COL);
                    printf("%s\n", FLETCHER32);
                    break;
                case H5Z_FILTER_SZIP:
                    {
                        szip_options_mask=cd_values[0];;
                        szip_pixels_per_block=cd_values[1];

                        indentation(indent + COL);
                        printf("%s %s\n",SZIP, BEGIN);

                        /*start indent */
                        indent += COL;
                        indentation(indent + COL);
                        printf("PIXELS_PER_BLOCK %d\n", szip_pixels_per_block);

                        indentation(indent + COL);
                        if (szip_options_mask & H5_SZIP_CHIP_OPTION_MASK)
                            printf("MODE %s\n", "HARDWARE");
                        else if (szip_options_mask & H5_SZIP_ALLOW_K13_OPTION_MASK)
                            printf("MODE %s\n", "K13");

                        indentation(indent + COL);
                        if (szip_options_mask & H5_SZIP_EC_OPTION_MASK)
                            printf("CODING %s\n", "ENTROPY");
                        else if (szip_options_mask & H5_SZIP_NN_OPTION_MASK)
                            printf("CODING %s\n", "NEAREST NEIGHBOUR");

                        indentation(indent + COL);
                        if (szip_options_mask & H5_SZIP_LSB_OPTION_MASK)
                            printf("BYTE_ORDER %s\n", "LSB");
                        else if (szip_options_mask & H5_SZIP_MSB_OPTION_MASK)
                            printf("BYTE_ORDER %s\n", "MSB");

                        indentation(indent + COL);
                        if (szip_options_mask & H5_SZIP_RAW_OPTION_MASK)
                            printf("HEADER %s\n", "RAW");

                        /*end indent */
                        indent -= COL;
                        indentation(indent + COL);
                        printf("%s\n",END);
                    }
                    break;
                default:
                    indentation(indent + COL);
                    if (H5Zfilter_avail(filtn))
                        printf("%s %s\n", "USER_REGISTERED_FILTER", BEGIN);
                    else
                        printf("%s %s\n", "UNKNOWN_FILTER", BEGIN);
                    /*start indent */
                    indent += COL;
                    indentation(indent + COL);
                    printf("FILTER_ID %d\n", filtn);
                    if (f_name[0]!='\0') {
                        indentation(indent + COL);
                        printf("COMMENT %s\n", f_name);
                    }
                    if (cd_nelmts) {
                        indentation(indent + COL);
                        printf("%s %s ","PARAMS", BEGIN);
                        for (j=0; j<cd_nelmts; j++)
                            printf("%d ", cd_values[j]);
                        printf("%s\n", END);
                    }
                    break;
            }/*switch*/
        } /*i*/
    }/*nfilters*/
    else {
        indentation(indent + COL);
        printf("NONE\n");
    }
    indent -= COL;
    indentation(indent + COL);
    printf("%s\n",END);

    /*-------------------------------------------------------------------------
    * FILLVALUE
    *-------------------------------------------------------------------------
    */
    indentation(indent + COL);
    printf("%s %s\n", FILLVALUE, BEGIN);
    /*start indent */
    indent += COL;
    indentation(indent + COL);
    printf("FILL_TIME ");
    H5Pget_fill_time(dcpl_id, &ft);
    switch ( ft ) {
        case H5D_FILL_TIME_ALLOC:
            printf("%s", "H5D_FILL_TIME_ALLOC\n");
            break;
        case H5D_FILL_TIME_NEVER:
            printf("%s", "H5D_FILL_TIME_NEVER\n");
            break;
        case H5D_FILL_TIME_IFSET:
            printf("%s", "H5D_FILL_TIME_IFSET\n");
            break;
        default:
            assert(0);
            break;
    }
    indentation(indent + COL);
    printf("%s ", "VALUE ");
    H5Pfill_value_defined(dcpl_id, &fvstatus);
    if (fvstatus == H5D_FILL_VALUE_UNDEFINED)
        printf("%s\n", "H5D_FILL_VALUE_UNDEFINED");
    else
        dump_fill_value(dcpl_id,type_id,obj_id);
    /* end indent */
    indent -= COL;
    indentation(indent + COL);
    printf("\n");
    indentation(indent + COL);
    printf("%s\n",END);

    /*-------------------------------------------------------------------------
    * ALLOCATION_TIME
    *-------------------------------------------------------------------------
    */
    indentation(indent + COL);
    printf("ALLOCATION_TIME %s\n",BEGIN);
    /*start indent */
    indent += COL;
    indentation(indent + COL);
    H5Pget_alloc_time(dcpl_id, &at);
    switch (at) {
        case H5D_ALLOC_TIME_EARLY:
            printf("%s", "H5D_ALLOC_TIME_EARLY\n");
            break;
        case H5D_ALLOC_TIME_INCR:
            printf("%s", "H5D_ALLOC_TIME_INCR\n");
            break;
        case H5D_ALLOC_TIME_LATE:
            printf("%s", "H5D_ALLOC_TIME_LATE\n");
            break;
        default:
            assert(0);
            break;
    }
    /* end indent */
    indent -= COL;
    indentation(indent + COL);
    printf("%s\n",END);
}

/*-------------------------------------------------------------------------
 * Function:    dump_fcpl
 *
 * Purpose:     prints file creation property list information
 *
 * Return:      void
 *
 * Programmer:  pvn
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_fcpl(hid_t fid)
{
    hid_t    fcpl;      /* file creation property list ID */
    hid_t         fapl;      /* file access property list ID */
    hsize_t  userblock; /* userblock size retrieved from FCPL */
    size_t   off_size;  /* size of offsets in the file */
    size_t   len_size;  /* size of lengths in the file */
    unsigned super;     /* superblock version # */
    unsigned freelist;  /* free list version # */
    unsigned stab;      /* symbol table entry version # */
    unsigned shhdr;     /* shared object header version # */
    hid_t    fdriver;    /* file driver */
    char     dname[15]; /* buffer to store driver name */
#ifdef H5_WANT_H5_V1_4_COMPAT
     int      sym_lk;
     int      sym_ik;    /* symbol table B-tree internal 'K' value */
#else
    unsigned sym_lk;    /* symbol table B-tree leaf 'K' value */
    unsigned sym_ik;    /* symbol table B-tree internal 'K' value */
#endif
    unsigned istore_ik; /* indexed storage B-tree internal 'K' value */

    fcpl=H5Fget_create_plist(fid);
    H5Pget_version(fcpl, &super, &freelist, &stab, &shhdr);
    H5Pget_userblock(fcpl,&userblock);
    H5Pget_sizes(fcpl,&off_size,&len_size);
    H5Pget_sym_k(fcpl,&sym_ik,&sym_lk);
    H5Pget_istore_k(fcpl,&istore_ik);
    H5Pclose(fcpl);
    fapl=h5_fileaccess();
    fdriver=H5Pget_driver(fapl);
    H5Pclose(fapl);

    /*-------------------------------------------------------------------------
    * SUPER_BLOCK
    *-------------------------------------------------------------------------
    */
    printf("%s %s\n",SUPER_BLOCK, BEGIN);
    indentation(indent + COL);
    printf("%s %u\n","SUPERBLOCK_VERSION", super);
    indentation(indent + COL);
    printf("%s %u\n","FREELIST_VERSION", freelist);
    indentation(indent + COL);
    printf("%s %u\n","SYMBOLTABLE_VERSION", stab);
    indentation(indent + COL);
    printf("%s %u\n","OBJECTHEADER_VERSION", shhdr);
    indentation(indent + COL);
    HDfprintf(stdout,"%s %Hd\n","OFFSET_SIZE", (long_long)off_size);
    indentation(indent + COL);
    HDfprintf(stdout,"%s %Hd\n","LENGTH_SIZE", (long_long)len_size);
    indentation(indent + COL);
    printf("%s %u\n","BTREE_RANK", sym_ik);
    indentation(indent + COL);
    printf("%s %d\n","BTREE_LEAF", sym_lk);

    if (H5FD_CORE==fdriver)
        HDstrcpy(dname,"H5FD_CORE");
    else if (H5FD_FAMILY==fdriver)
        HDstrcpy(dname,"H5FD_FAMILY");
     else if (H5FD_LOG==fdriver)
        HDstrcpy(dname,"H5FD_LOG");
    else if (H5FD_MPIO==fdriver)
        HDstrcpy(dname,"H5FD_MPIO");
    else if (H5FD_MULTI==fdriver)
        HDstrcpy(dname,"H5FD_MULTI");
    else if (H5FD_SEC2==fdriver)
        HDstrcpy(dname,"H5FD_SEC2");
    else if (H5FD_STDIO==fdriver)
        HDstrcpy(dname,"H5FD_STDIO");

    indentation(indent + COL);
    printf("%s %s\n","FILE_DRIVER", dname);
    indentation(indent + COL);
    printf("%s %u\n","ISTORE_K", istore_ik);
    printf("%s\n",END);

    /*-------------------------------------------------------------------------
    * USER_BLOCK
    *-------------------------------------------------------------------------
    */
    printf("USER_BLOCK %s\n",BEGIN);
    indentation(indent + COL);
    HDfprintf(stdout,"%s %Hu\n","USERBLOCK_SIZE", userblock);
    printf("%s\n",END);
}

/*-------------------------------------------------------------------------
 * Function:    dump_fcontents
 *
 * Purpose:     prints all objects
 *
 * Return:      void
 *
 * Programmer:  pvn
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void dump_fcontents(hid_t fid)
{
    printf("%s %s\n",FILE_CONTENTS, BEGIN);

    /* special case of unamed types in root group */
    if (unamedtype) {
        unsigned u;

        for (u = 0; u < type_table->nobjs; u++) {
            if (!type_table->objs[u].recorded) {
                unsigned long   objno[2];   /*object number         */

                objno[0] = (unsigned long)(type_table->objs[u].objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
                objno[1] = (unsigned long)(type_table->objs[u].objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                printf(" %-10s /#%lu:%lu\n", "datatype", objno[0], objno[1]);
            }
        }
    }

    /* print objects in the files */
    h5trav_getinfo(fid, NULL, 1);

    printf(" %s\n",END);
}


/*-------------------------------------------------------------------------
 * Function:    set_output_file
 *
 * Purpose:     Open fname as the output file for dataset raw data.
 *      Set rawdatastream as its file stream.
 *
 * Return:      0 -- succeeded
 *      negative -- failed
 *
 * Programmer:  Albert Cheng, 2000/09/30
 *
 * Modifications:
 *  pvn June, 1, 2006. Add a switch for binary output
 *
 *-------------------------------------------------------------------------
 */
static int
set_output_file(const char *fname, int is_bin)
{
 FILE    *f;    /* temporary holding place for the stream pointer
* so that rawdatastream is changed only when succeeded */

 if (rawdatastream && rawdatastream != stdout) {
  if (fclose(rawdatastream))
   perror("closing rawdatastream");
  else
   rawdatastream = NULL;
 }

 /* binary output */
 if (is_bin)
 {
  if ((f = fopen(fname, "wb")) != NULL) {
   rawdatastream = f;
   return 0;
  }
 }
 else
 {
  if ((f = fopen(fname, "w")) != NULL) {
   rawdatastream = f;
   return 0;
  }
 }

 return -1;
}

/*-------------------------------------------------------------------------
 * Function:    set_binary_form
 *
 * Purpose: set the binary form of output by translating from a string input
 *          parameter to a integer return value
 *
 * Return: integer form of binary output or -1 if none found
 *
 * Programmer:  Pedro Vicente Nunes
 *             June 28, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
set_binary_form(const char *form)
{
 int bform=-1;

 if (strcmp(form,"NATIVE")==0 ||
     strcmp(form,"MEMORY")==0) 
 {/* native form */
  bform = 0;
 }
 else if (strcmp(form,"FILE")==0) /* file type form */
  bform = 1;
 else if (strcmp(form,"LE")==0) /* convert to little endian */
  bform = 2;
 else if (strcmp(form,"BE")==0) /* convert to big endian */
  bform = 3;

 return bform;
}

/*-------------------------------------------------------------------------
 * Function:    handle_attributes
 *
 * Purpose:     Handle the attributes from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
handle_attributes(hid_t fid, char *attr, void UNUSED * data)
{
    dump_selected_attr(fid, attr);
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
 * Return:      Success:    hsize_t array. NULL is a valid return type if
 *                          there aren't any elements in the array.
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 6. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t *
parse_hsize_list(const char *h_list)
{
    hsize_t        *p_list;
    const char     *ptr;
    unsigned int    size_count = 0, i = 0, last_digit = 0;

    if (!h_list || !*h_list || *h_list == ';')
        return NULL;

    /* count how many integers do we have */
    for (ptr = h_list; ptr && *ptr && *ptr != ';' && *ptr != ']'; ptr++)
        if (isdigit(*ptr)) {
            if (!last_digit)
                /* the last read character wasn't a digit */
                size_count++;

            last_digit = 1;
        } else {
            last_digit = 0;
        }

    if (size_count == 0)
        /* there aren't any integers to read */
        return NULL;

    /* allocate an array for the integers in the list */
    p_list = calloc(size_count, sizeof(hsize_t));

    for (ptr = h_list; i < size_count && ptr && *ptr && *ptr != ';' && *ptr != ']'; ptr++)
        if (isdigit(*ptr)) {
            /* we should have an integer now */
            p_list[i++] = (hsize_t)atof(ptr);

            while (isdigit(*ptr))
                /* scroll to end of integer */
                ptr++;
        }

    return p_list;
}

/*-------------------------------------------------------------------------
 * Function:    parse_subset_params
 *
 * Purpose:     Parse the so-called "terse" syntax for specifying subsetting
 *              parameters.
 *
 * Return:      Success:    struct subset_t object
 *              Failure:    NULL
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 6. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static struct subset_t *
parse_subset_params(char *dset)
{
    struct subset_t *s = NULL;
    register char   *brace;

    if ((brace = strrchr(dset, '[')) != NULL) {
        char *slash = strrchr(dset, '/');

        /* sanity check to make sure the [ isn't part of the dataset name */
        if (brace > slash) {
            *brace++ = '\0';

            s = calloc(1, sizeof(struct subset_t));
            s->start = parse_hsize_list(brace);

            while (*brace && *brace != ';')
                brace++;

            if (*brace)
                brace++;

            s->stride = parse_hsize_list(brace);

            while (*brace && *brace != ';')
                brace++;

            if (*brace)
                brace++;

            s->count = parse_hsize_list(brace);

            while (*brace && *brace != ';')
                brace++;

            if (*brace)
                brace++;

            s->block = parse_hsize_list(brace);
        }
    }

    return s;
}

/*-------------------------------------------------------------------------
 * Function:    handle_datasets
 *
 * Purpose:     Handle the datasets from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *  Pedro Vicente, Friday, March 7, 2008
 *  check for block overlap
 *
 *-------------------------------------------------------------------------
 */
static void
handle_datasets(hid_t fid, char *dset, void *data)
{
    H5G_stat_t       statbuf;
    hid_t            dsetid;
    struct subset_t *sset = (struct subset_t *)data;

    if ((dsetid = H5Dopen(fid, dset)) < 0) {
        begin_obj(dump_header_format->datasetbegin, dset,
                  dump_header_format->datasetblockbegin);
        indentation(COL);
        error_msg(progname, "unable to open dataset \"%s\"\n", dset);
        end_obj(dump_header_format->datasetend,
                dump_header_format->datasetblockend);
        d_status = EXIT_FAILURE;
        return;
    }

    if (sset) {
        if (!sset->start || !sset->stride || !sset->count || !sset->block) {
            /* they didn't specify a ``stride'' or ``block''. default to 1 in all
             * dimensions */
            hid_t sid = H5Dget_space(dsetid);
            unsigned int ndims = H5Sget_simple_extent_ndims(sid);

            if (!sset->start)
                /* default to (0, 0, ...) for the start coord */
                sset->start = calloc(ndims, sizeof(hsize_t));

            if (!sset->stride) {
                unsigned int i;

                sset->stride = calloc(ndims, sizeof(hsize_t));

                for (i = 0; i < ndims; i++)
                    sset->stride[i] = 1;
            }

            if (!sset->count) {
                hsize_t dims[H5S_MAX_RANK];
                herr_t status = H5Sget_simple_extent_dims(sid, dims, NULL);
                unsigned int i;

                if (status == FAIL) {
                    error_msg(progname, "unable to get dataset dimensions\n");
                    d_status = EXIT_FAILURE;
                    H5Sclose(sid);
                    return;
                }

                sset->count = calloc(ndims, sizeof(hsize_t));

                for (i = 0; i < ndims; i++)
                    sset->count[i] = dims[i] - sset->start[i];
            }

            if (!sset->block) {
                unsigned int i;

                sset->block = calloc(ndims, sizeof(hsize_t));

                for (i = 0; i < ndims; i++)
                    sset->block[i] = 1;
            }

            H5Sclose(sid);
        }
    }

   /*-------------------------------------------------------------------------
    * check for block overlap
    *-------------------------------------------------------------------------
    */

    if(sset) 
    {
        hid_t sid = H5Dget_space(dsetid);
        unsigned int ndims = H5Sget_simple_extent_ndims(sid);
        unsigned int i;
        
        for ( i = 0; i < ndims; i++)
        {
            if ( sset->count[i] > 1 )
            {
                
                if ( sset->stride[i] < sset->block[i] )
                {
                    error_msg(progname, "wrong subset selection; blocks overlap\n");
                    d_status = EXIT_FAILURE;
                    return;
                    
                }                                
                
            }
            
        } 
        H5Sclose(sid);
        
    }

    H5Gget_objinfo(dsetid, ".", TRUE, &statbuf);

    if (statbuf.nlink > 1) {
        obj_t  *found_obj;    /* Found object */

        found_obj = search_obj(dset_table, statbuf.objno);

        if (found_obj) {
            if (found_obj->displayed) {
                begin_obj(dump_header_format->datasetbegin, dset,
                          dump_header_format->datasetblockbegin);
                indentation(indent + COL);
                printf("%s \"%s\"\n", HARDLINK, found_obj->objname);
                indentation(indent);
                end_obj(dump_header_format->datasetend,
                        dump_header_format->datasetblockend);
            } else {
                found_obj->displayed = TRUE;
                dump_dataset(dsetid, dset, sset);
            }
        } else {
            d_status = EXIT_FAILURE;
        }
    } else {
        dump_dataset(dsetid, dset, sset);
    }

    if (H5Dclose(dsetid) < 0)
        d_status = EXIT_FAILURE;
}

/*-------------------------------------------------------------------------
 * Function:    handle_groups
 *
 * Purpose:     Handle the groups from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
handle_groups(hid_t fid, char *group, void UNUSED * data)
{
    H5G_stat_t  statbuf;
    hid_t       gid;

    if ((gid = H5Gopen(fid, group)) < 0) {
        begin_obj(dump_header_format->groupbegin, group,
                  dump_header_format->groupblockbegin);
        indentation(COL);
        error_msg(progname, "unable to open group \"%s\"\n", group);
        end_obj(dump_header_format->groupend,
                dump_header_format->groupblockend);
        d_status = EXIT_FAILURE;
    } else {
        size_t new_len = HDstrlen(group) + 1;

        if (prefix_len <= new_len) {
            prefix_len = new_len;
            prefix = HDrealloc(prefix, prefix_len);
        }

        H5Gget_objinfo(gid, ".", TRUE, &statbuf);
        HDstrcpy(prefix, group);
        dump_group(gid, group);

        if (H5Gclose(gid) < 0)
            d_status = EXIT_FAILURE;
    }
}

/*-------------------------------------------------------------------------
 * Function:    handle_links
 *
 * Purpose:     Handle the links from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
handle_links(hid_t fid, char *links, void UNUSED * data)
{
    H5G_stat_t  statbuf;

    if (H5Gget_objinfo(fid, links, FALSE, &statbuf) < 0) {
        error_msg(progname, "unable to get obj info from \"%s\"\n", links);
        d_status = EXIT_FAILURE;
    } else if (statbuf.type == H5G_LINK) {
        char *buf = HDmalloc(statbuf.linklen);

        begin_obj(dump_header_format->softlinkbegin, links,
                  dump_header_format->softlinkblockbegin);
        indentation(COL);

        if (H5Gget_linkval(fid, links, statbuf.linklen, buf) >= 0) {
            printf("LINKTARGET \"%s\"\n", buf);
        } else {
            error_msg(progname, "h5dump error: unable to get link value for \"%s\"\n",
                      links);
            d_status = EXIT_FAILURE;
        }

        end_obj(dump_header_format->softlinkend,
                dump_header_format->softlinkblockend);
        HDfree(buf);
    } else {
        error_msg(progname, "\"%s\" is not a link\n", links);
        d_status = EXIT_FAILURE;
    }
}

/*-------------------------------------------------------------------------
 * Function:    handle_datatypes
 *
 * Purpose:     Handle the datatypes from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
handle_datatypes(hid_t fid, char *type, void UNUSED * data)
{
    hid_t       type_id;

    if ((type_id = H5Topen(fid, type)) < 0) {
        /* check if type is unamed data type */
        unsigned idx = 0;

        while (idx < type_table->nobjs ) {
            char name[128];

            if (!type_table->objs[idx].recorded) {
                unsigned long   objno[2];   /*object number         */

                /* unamed data type */
                objno[0] = (unsigned long)(type_table->objs[idx].objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
                objno[1] = (unsigned long)(type_table->objs[idx].objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                sprintf(name, "/#%lu:%lu", objno[0], objno[1]);

                if (!HDstrcmp(name, type))
                    break;
            }

            idx++;
        }

        if (idx ==  type_table->nobjs) {
            /* unknown type */
            begin_obj(dump_header_format->datatypebegin, type,
                      dump_header_format->datatypeblockbegin);
            indentation(COL);
            error_msg(progname, "unable to open datatype \"%s\"\n", type);
            end_obj(dump_header_format->datatypeend,
                    dump_header_format->datatypeblockend);
            d_status = EXIT_FAILURE;
        } else {
            hid_t dsetid = H5Dopen(fid, type_table->objs[idx].objname);
            type_id = H5Dget_type(dsetid);
            dump_named_datatype(type_id, type);
            H5Tclose(type_id);
            H5Dclose(dsetid);
        }
    } else {
        dump_named_datatype(type_id, type);

        if (H5Tclose(type_id) < 0)
            d_status = EXIT_FAILURE;
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
 *
 *              Failure:    Exits program with EXIT_FAILURE value.
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 20. February 2001
 *
 * Modifications:
 *  pvn June, 1, 2006. Add a switch for binary output
 *
 *-------------------------------------------------------------------------
 */
static struct handler_t *
parse_command_line(int argc, const char *argv[])
{
    struct handler_t   *hand, *last_dset = NULL;
    int                 i, opt, last_was_dset = FALSE;

    /* no arguments */
    if (argc == 1) {
        usage(progname);
        leave(EXIT_FAILURE);
    }

    /* this will be plenty big enough to hold the info */
    hand = calloc((size_t)argc, sizeof(struct handler_t));

    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
parse_start:
        switch ((char)opt) {
        case 'B':
            display_bb = TRUE;
            last_was_dset = FALSE;
            break;
        case 'n':
            display_fi = TRUE;
            last_was_dset = FALSE;
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
            display_data = FALSE;
            display_attr_data = TRUE;
            last_was_dset = FALSE;
            break;
        case 'i':
            display_oid = TRUE;
            last_was_dset = FALSE;
            break;
        case 'r':
            display_char = TRUE;
            break;
        case 'V':
            print_version(progname);
            leave(EXIT_SUCCESS);
            break;
        case 'w':
            nCols = atoi(opt_arg);
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
                    last_dset = hand;
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

        case 'o':
         
         if ( bin_output )
         {
          if (set_output_file(opt_arg, 1) < 0){
           usage(progname);
           leave(EXIT_FAILURE);
          }
         }
         else
         {
          if (set_output_file(opt_arg, 0) < 0){
           usage(progname);
           leave(EXIT_FAILURE);
          }
         }

         usingdasho = TRUE;
         last_was_dset = FALSE;
         outfname = opt_arg;
         break;

       case 'b':
            
           if ( opt_arg != NULL)
           {
               if ( ( bin_form = set_binary_form(opt_arg)) < 0)
               {
                   /* failed to set binary form */
                   usage(progname);
                   leave(EXIT_FAILURE);
               }
           }
           bin_output = TRUE;
           if (outfname!=NULL) 
           {
               if (set_output_file(outfname, 1) < 0)
               {
                   /* failed to set output file */
                   usage(progname);
                   leave(EXIT_FAILURE);
               }
               
               last_was_dset = FALSE;
           }
           
           break;

        /** begin XML parameters **/
        case 'x':
            /* select XML output */
            doxml = TRUE;
            useschema = TRUE;
            dump_header_format = NULL;
            dump_function_table = &xml_function_table;
            break;
        case 'u':
            doxml = TRUE;
            useschema = FALSE;
            xmlnsprefix = "";
            dump_header_format = NULL;
            dump_function_table = &xml_function_table;
            break;
        case 'D':
            /* specify alternative XML DTD or schema */
            /* To Do: check format of this value?  */
            xml_dtd_uri = opt_arg;
            break;

            
        case 'm':
            /* specify alternative floating point printing format */
            fp_format = opt_arg;
            break;

        case 'X':
            /* specify XML namespace (default="hdf5:"), or none */
            /* To Do: check format of this value?  */
        if (!useschema) {
                usage(progname);
                leave(EXIT_FAILURE);
        }
        if (strcmp(opt_arg,":") == 0) {
                xmlnsprefix = "";
        } else {
                xmlnsprefix = opt_arg;
        }
            break;
        /** end XML parameters **/

        /** begin subsetting parameters **/
        case 's':
        case 'S':
        case 'c':
        case 'k': {
            struct subset_t *s;

            if (!last_was_dset) {
                error_msg(progname,
                          "option `-%c' can only be used after --dataset option\n",
                          opt);
                leave(EXIT_FAILURE);
            }

            if (last_dset->subset_info) {
                /*
                 * This overrides the "terse" syntax if they actually mixed
                 * the two.
                 */
                s = last_dset->subset_info;
            } else {
                last_dset->subset_info = s = calloc(1, sizeof(struct subset_t));
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
                case 's': free(s->start); s->start = parse_hsize_list(opt_arg); break;
                case 'S': free(s->stride); s->stride = parse_hsize_list(opt_arg); break;
                case 'c': free(s->count); s->count = parse_hsize_list(opt_arg); break;
                case 'k': free(s->block); s->block = parse_hsize_list(opt_arg); break;
                default: goto end_collect;
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

        case 'h':
            usage(progname);
            leave(EXIT_SUCCESS);
        case '?':
        default:
            usage(progname);
            leave(EXIT_FAILURE);
        }
    }

parse_end:
    /* check for file name to be processed */
    if (argc <= opt_ind) {
        error_msg(progname, "missing file name\n");
        usage(progname);
        leave(EXIT_FAILURE);
    }
    return hand;
}


/*-------------------------------------------------------------------------
 * Function:    free_handler
 *
 * Purpose:     Convenience function to free the handler_t structures. Needs a
 *              length variable (LEN) to know how many in the array it needs
 *              to free
 *
 * Return:      Nothing
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 20. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
free_handler(struct handler_t *hand, int len)
{
    register int i;

    for (i = 0; i < len; i++) {
        free(hand[i].obj);

        if (hand[i].subset_info) {
            free(hand[i].subset_info->start);
            free(hand[i].subset_info->stride);
            free(hand[i].subset_info->count);
            free(hand[i].subset_info->block);
            free(hand[i].subset_info);
        }
    }

    free(hand);
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 dumper
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *        Albert Cheng
 *        30. September 2000
 *        Add the -o option--output file for datasets raw data
 *
 *        REMcG
 *        November 2000
 *        Changes to support XML.
 *
 *        Bill Wendling
 *        Wednesday, 10. January 2001
 *        Modified the way command line parameters are interpreted. They go
 *        through one function call now (get_option).
 *
 *        Bill Wendling
 *        Tuesday, 20. February 2001
 *        Moved command line parsing to separate function. Made various
 *        "display_*" flags global.
 *
 *        REMcG
 *        August 2003
 *        Major upgrade to XML support.
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, const char *argv[])
{
    hid_t               fid, gid;
    const char         *fname = NULL;
    void               *edata;
    H5E_auto_t          func;
    find_objs_t         info;
    struct handler_t   *hand;
    int                 i;
    unsigned            u;

    dump_header_format = &standardformat;
    dump_function_table = &ddl_function_table;

    /* Disable error reporting */
    H5Eget_auto(&func, &edata);
    H5Eset_auto(NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();
    hand = parse_command_line(argc, argv);

    if ( bin_output && outfname == NULL )
    {
        error_msg(progname, "binary output requires a file name, use -o <filename>\n");
        leave(EXIT_FAILURE);
    }

    /* Check for conflicting options */
    if (doxml) {
    if (!display_all) {
            error_msg(progname, "option \"%s\" not available for XML\n",
              "to display selected objects");
        leave(EXIT_FAILURE);
    } else if (display_bb) {
            error_msg(progname, "option \"%s\" not available for XML\n",
              "--boot-block");
        leave(EXIT_FAILURE);
    } else if (display_oid == 1) {
            error_msg(progname, "option \"%s\" not available for XML\n",
              "--object-ids");
        leave(EXIT_FAILURE);
        } else if (display_char == TRUE) {
            error_msg(progname, "option \"%s\" not available for XML\n",
              "--string");
        leave(EXIT_FAILURE);
    } else if (usingdasho) {
            error_msg(progname, "option \"%s\" not available for XML\n",
              "--output");
        leave(EXIT_FAILURE);
    }
    } else {
        if (xml_dtd_uri) {
            warn_msg(progname, "option \"%s\" only applies with XML: %s\n",
                     "--xml-dtd", xml_dtd_uri);
        }
    }

    if (argc <= opt_ind) {
        error_msg(progname, "missing file name\n");
        usage(progname);
        leave(EXIT_FAILURE);
    }
    fname = argv[opt_ind];

    fid = h5tools_fopen(fname, driver, NULL, 0);

    if (fid < 0) {
        error_msg(progname, "unable to open file \"%s\"\n", fname);
        leave(EXIT_FAILURE);
    }

    /* allocate and initialize internal data structure */
    init_prefix(&prefix, prefix_len);

    /* find all objects that might be targets of a refernce */
    init_ref_path_table(fid);      /* Insert the root group into the ref. path table */
    H5Giterate(fid, (const char *)"/", NULL, fill_ref_path_table, (void *)"");

    if (doxml) {
    /* initialize XML */

    /* reset prefix! */
    HDstrcpy(prefix, "");

    /* make sure the URI is initialized to something */
    if (xml_dtd_uri == NULL) {
        if (useschema) {
            xml_dtd_uri = DEFAULT_XSD;
        } else {
            xml_dtd_uri = DEFAULT_DTD;
            xmlnsprefix = "";
        }
    } else {
        if (useschema && strcmp(xmlnsprefix,"")) {
        error_msg(progname, "Cannot set Schema URL for a qualified namespace--use -X or -U option with -D \n");
            leave(EXIT_FAILURE);
        }
    }
    }

    /* find all shared objects */
    if(init_objs(fid, &info, &group_table, &dset_table, &type_table) < 0) {
        error_msg(progname, "internal error (file %s:line %d)\n",
                  __FILE__, __LINE__);
        d_status = EXIT_FAILURE;
        goto done;
    }

    /* does there exist unamed committed data type */
    for (u = 0; u < type_table->nobjs; u++)
        if (!type_table->objs[u].recorded) {
            unamedtype = 1;
            break;
        } /* end if */

#ifdef H5DUMP_DEBUG
    dump_tables(&info);
#endif /* H5DUMP_DEBUG */

    /* start to dump - display file header information */
    if (!doxml) {
    begin_obj(dump_header_format->filebegin, fname,
          dump_header_format->fileblockbegin);
    } else {
    printf("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    /* alternative first element, depending on schema or DTD. */
        if (useschema) {
            if (strcmp(xmlnsprefix,"") == 0) {
                printf("<HDF5-File xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"%s\">\n",
                   xml_dtd_uri);
            } else {
/*  TO DO: make -url option work in this case (may need new option) */
                char * ns;
                char *indx;

                ns = strdup(xmlnsprefix);
                indx = strrchr(ns,(int)':');
                if (indx) *indx = '\0';

                printf("<%sHDF5-File xmlns:%s=\"http://hdfgroup.org/DTDs/HDF5-File\" "
                    "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
                    "xsi:schemaLocation=\"http://hdfgroup.org/DTDs/HDF5-File "
                    "http://www.hdfgroup.org/DTDs/HDF5-File.xsd\">\n",xmlnsprefix,ns);
            }
    } else {
            printf("<!DOCTYPE HDF5-File PUBLIC \"HDF5-File.dtd\" \"%s\">\n",
                   xml_dtd_uri);
            printf("<HDF5-File>\n");
    }
    }

    if (!doxml) {
        if (display_fi) {
            dump_fcontents(fid);
            end_obj(dump_header_format->fileend,dump_header_format->fileblockend);
            goto done;
        }

        if (display_bb)
            dump_fcpl(fid);
    }


    if (display_all) {
        if ((gid = H5Gopen(fid, "/")) < 0) {
            error_msg(progname, "unable to open root group\n");
            d_status = EXIT_FAILURE;
        } else {
        dump_function_table->dump_group_function(gid, "/");
        }

        if (H5Gclose(gid) < 0) {
            error_msg(progname, "unable to close root group\n");
            d_status = EXIT_FAILURE;
        }
    } else {
    if (doxml) {
        /* Note: this option is not supported for XML */
            error_msg(progname, "internal error (file %s:line %d)\n",
                      __FILE__, __LINE__);
            d_status = EXIT_FAILURE;
        goto done;
    }

        for (i = 0; i < argc; i++)
            if (hand[i].func)
                hand[i].func(fid, hand[i].obj, hand[i].subset_info);
    }

    if (!doxml) {
    end_obj(dump_header_format->fileend,
        dump_header_format->fileblockend);
    } else {
    printf("</%sHDF5-File>\n", xmlnsprefix);
    }

done:
    if (H5Fclose(fid) < 0)
    d_status = EXIT_FAILURE;

    free_handler(hand, argc);

    /* Free tables for objects */
    free_table(group_table);
    free_table(dset_table);
    free_table(type_table);

    HDfree(prefix);

    /* To Do:  clean up XML table */

    H5Eset_auto(func, edata);

    leave(d_status);
}

/*-------------------------------------------------------------------------
 * Function:    print_enum
 *
 * Purpose:     prints the enum data
 *
 * Return:      void
 *
 * Programmer:  Patrick Lu
 *
 * Modifications:
 *
 * NOTE: this function was taken from h5ls. should be moved into the toolslib
 *
 *-----------------------------------------------------------------------*/
static void
print_enum(hid_t type)
{
    char           **name = NULL;   /*member names                   */
    unsigned char   *value = NULL;  /*value array                    */
    unsigned char   *copy = NULL;   /*a pointer to value array       */
    unsigned         nmembs;        /*number of members              */
    int              nchars;        /*number of output characters    */
    hid_t            super;         /*enum base integer type         */
    hid_t            native = -1;   /*native integer data type       */
    size_t           dst_size;      /*destination value type size    */
    unsigned         i;

    nmembs = H5Tget_nmembers(type);
    assert(nmembs>0);
    super = H5Tget_super(type);

    /*
     * Determine what data type to use for the native values.  To simplify
     * things we entertain three possibilities:
     *  1. long_long -- the largest native signed integer
     *    2. unsigned long_long -- the largest native unsigned integer
     *    3. raw format
     */
    if (H5Tget_size(type) <= sizeof(long_long)) {
    dst_size = sizeof(long_long);

    if (H5T_SGN_NONE == H5Tget_sign(type)) {
        native = H5T_NATIVE_ULLONG;
    } else {
        native = H5T_NATIVE_LLONG;
    }
    } else {
    dst_size = H5Tget_size(type);
    }

    /* Get the names and raw values of all members */
    name = calloc(nmembs, sizeof(char *));
    value = calloc(nmembs, MAX(H5Tget_size(type), dst_size));

    for (i = 0; i < nmembs; i++) {
    name[i] = H5Tget_member_name(type, i);
    H5Tget_member_value(type, i, value + i * H5Tget_size(type));
    }

    /* Convert values to native data type */
    if (native > 0)
        H5Tconvert(super, native, nmembs, value, NULL, H5P_DEFAULT);

    /*
     * Sort members by increasing value
     *    ***not implemented yet***
     */

    /* Print members */
    for (i = 0; i < nmembs; i++) {
    indentation(indent + COL);
    nchars = printf("\"%s\"", name[i]);
    printf("%*s   ", MAX(0, 16 - nchars), "");

    if (native < 0) {
            size_t j;

        printf("0x");

        for (j = 0; j < dst_size; j++)
        printf("%02x", value[i * dst_size + j]);
    } else if (H5T_SGN_NONE == H5Tget_sign(native)) {
        /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
         *strangely, unless use another pointer "copy".*/
        copy = value+i*dst_size;
        HDfprintf(stdout,"%" H5_PRINTF_LL_WIDTH "u", *((unsigned long_long *)
                          ((void *)copy)));
    } else {
        /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
         *strangely, unless use another pointer "copy".*/
        copy = value+i*dst_size;
        HDfprintf(stdout,"%" H5_PRINTF_LL_WIDTH "d",
           *((long_long *) ((void *)copy)));
    }

    printf(";\n");
    }

    /* Release resources */
    for (i = 0; i < nmembs; i++)
    free(name[i]);

    free(name);
    free(value);
    H5Tclose(super);

    if (0 == nmembs)
    printf("\n%*s <empty>", indent + 4, "");
}


/*
 * create a string suitable for and XML NCNAME.  Uses the
 * object reference to create the string.
 *
 *  'gen'; 0 - return null if not found
 *         1 - generate a fake entry and return fake id.
 */

int
xml_name_to_XID(const char *str , char *outstr, int outlen, int gen)
{
    haddr_t objno;      /* Object ID for object at path */
    unsigned long   _objno[2];  /*object number         */

    if (outlen < 22) return 1;

    objno = ref_path_table_lookup(str);
    if (objno == HADDR_UNDEF) {
        if (HDstrlen(str) == 0) {
            objno = ref_path_table_lookup("/");
            if (objno == HADDR_UNDEF) {
                if (gen) {
                    objno = ref_path_table_gen_fake(str);
                    _objno[0] = (unsigned long)(objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
                    _objno[1] = (unsigned long)(objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                    _objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
            sprintf(outstr,"xid_%lu-%lu", _objno[0], _objno[1]);
                    return 0;
                } else {
                    return 1;
                }
            }
        } else {
            if (gen) {
                objno = ref_path_table_gen_fake(str);
                _objno[0] = (unsigned long)(objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
                _objno[1] = (unsigned long)(objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                _objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                sprintf(outstr,"xid_%lu-%lu", _objno[0], _objno[1]);
                return 0;
            } else {
                return 1;
            }
        }
    }

    _objno[0] = (unsigned long)(objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
    _objno[1] = (unsigned long)(objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
    _objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
    sprintf(outstr,"xid_%lu-%lu", _objno[0], _objno[1]);

    return(0);
}

static const char      *quote = "&quot;";
static const char      *amp = "&amp;";
static const char      *lt = "&lt;";
static const char      *gt = "&gt;";
static const char      *apos = "&apos;";

/*-------------------------------------------------------------------------
 * Function:    xml_escape_the_name
 *
 * Purpose:     Escape XML reserved chars in a name, so HDF5 strings
 *              and paths can be correctly read back in XML element.
 *
 * Return:      The revised string.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char                   *
xml_escape_the_name(const char *str)
{
    size_t                     extra;
    size_t                     len;
    size_t                     i;
    const char             *cp;
    char                   *ncp;
    char                   *rcp;

    if (!str)
    return NULL;

    cp = str;
    len = strlen(str);
    extra = 0;

    for (i = 0; i < len; i++) {
    if (*cp == '\"') {
        extra += (strlen(quote) - 1);
    } else if (*cp == '\'') {
        extra += (strlen(apos) - 1);
    } else if (*cp == '<') {
        extra += (strlen(lt) - 1);
    } else if (*cp == '>') {
        extra += (strlen(gt) - 1);
    } else if (*cp == '&') {
        extra += (strlen(amp) - 1);
    }

    cp++;
    }

    if (extra == 0)
    return HDstrdup(str);

    cp = str;
    rcp = ncp = HDmalloc(len + extra + 1);

    if (!ncp)
        return NULL;    /* ?? */

    for (i = 0; i < len; i++) {
        if (*cp == '\'') {
            strncpy(ncp, apos, strlen(apos));
            ncp += strlen(apos);
            cp++;
        } else if (*cp == '<') {
            strncpy(ncp, lt, strlen(lt));
            ncp += strlen(lt);
            cp++;
        } else if (*cp == '>') {
            strncpy(ncp, gt, strlen(gt));
            ncp += strlen(gt);
            cp++;
        } else if (*cp == '\"') {
            strncpy(ncp, quote, strlen(quote));
            ncp += strlen(quote);
            cp++;
        } else if (*cp == '&') {
            strncpy(ncp, amp, strlen(amp));
            ncp += strlen(amp);
            cp++;
        } else {
            *ncp++ = *cp++;
        }
    }

    *ncp = '\0';
    return rcp;
}

/*-------------------------------------------------------------------------
 * Function:    xml_escape_the_string
 *
 * Purpose:     Escape XML reserved chars in a string, so HDF5 strings
 *              and paths can be correctly read back in XML CDATA.
 *
 * Return:      The revised string.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char                   *
xml_escape_the_string(const char *str, int slen)
{
    size_t                     extra;
    size_t                     len;
    size_t                     i;
    const char             *cp;
    char                   *ncp;
    char                   *rcp;

    if (!str)
    return NULL;

    cp = str;

    if (slen < 0)
    len = strlen(str);
    else
    len = slen;

    extra = 0;

    for (i = 0; i < len; i++) {
    if (*cp == '\\') {
        extra++;
    } else if (*cp == '\"') {
        extra++;
    } else if (*cp == '\'') {
        extra += (strlen(apos) - 1);
    } else if (*cp == '<') {
        extra += (strlen(lt) - 1);
    } else if (*cp == '>') {
        extra += (strlen(gt) - 1);
    } else if (*cp == '&') {
        extra += (strlen(amp) - 1);
    }
    cp++;
    }

    cp = str;
    rcp = ncp = calloc((len + extra + 1), sizeof(char));

    if (ncp == NULL)
    return NULL;        /* ?? */

    for (i = 0; i < len; i++) {
    if (*cp == '\\') {
        *ncp++ = '\\';
        *ncp++ = *cp++;
    } else if (*cp == '\"') {
        *ncp++ = '\\';
        *ncp++ = *cp++;
    } else if (*cp == '\'') {
        strncpy(ncp, apos, strlen(apos));
        ncp += strlen(apos);
        cp++;
    } else if (*cp == '<') {
        strncpy(ncp, lt, strlen(lt));
        ncp += strlen(lt);
        cp++;
    } else if (*cp == '>') {
        strncpy(ncp, gt, strlen(gt));
        ncp += strlen(gt);
        cp++;
    } else if (*cp == '&') {
        strncpy(ncp, amp, strlen(amp));
        ncp += strlen(amp);
        cp++;
    } else {
        *ncp++ = *cp++;
    }
    }

    *ncp = '\0';
    return rcp;
}

/**
 **  XML print functions--these replace some functions in the
 **  h5tools.c suite.
 **/

/*-------------------------------------------------------------------------
 * Function:    xml_print_datatype
 *
 * Purpose:     Print description of a datatype in XML.
 *              Note:  this is called inside a <DataType> element.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_print_datatype(hid_t type, unsigned in_group)
{
    char                   *fname;
    hid_t                   mtype;
    unsigned                nmembers;
    int                     perm[H5DUMP_MAX_RANK];
    unsigned                ndims;
    unsigned                i;
    size_t                  size;
    hsize_t                 dims[H5DUMP_MAX_RANK];
    H5T_str_t               str_pad;
    H5T_cset_t              cset;
    H5G_stat_t              statbuf;
    hid_t                   super;
    H5T_order_t             ord;
    H5T_sign_t              sgn;
    size_t                  sz;
    size_t                  spos;
    size_t                  epos;
    size_t                  esize;
    size_t                  mpos;
    size_t                  msize;
    int                     nmembs;
    htri_t                  is_vlstr=FALSE;

    if (!in_group && H5Tcommitted(type) > 0) {
        obj_t  *found_obj;    /* Found object */

        /* detect a shared datatype, output only once */
        H5Gget_objinfo(type, ".", TRUE, &statbuf);
        found_obj = search_obj(type_table, statbuf.objno);

        if (found_obj) {
            /* This should be defined somewhere else */
            /* These 2 cases are handled the same right now, but
               probably will have something different eventually */
            char * dtxid = malloc(100);
            xml_name_to_XID(found_obj->objname,dtxid,100,1);
            if (!found_obj->recorded) {
                /* 'anonymous' NDT.  Use it's object num.
                   as it's name.  */
                printf("<%sNamedDataTypePtr OBJ-XID=\"/%s\"/>\n",
                    xmlnsprefix, dtxid);
            } else {
                /* point to the NDT by name */
                char *t_objname = xml_escape_the_name(found_obj->objname);
                printf("<%sNamedDataTypePtr OBJ-XID=\"%s\" H5Path=\"%s\"/>\n",
                    xmlnsprefix, dtxid,t_objname);
                free(t_objname);
            }
            free(dtxid);
        } else {
            printf("<!-- h5dump error: unknown committed type. -->\n");
            d_status = EXIT_FAILURE;
        }

    } else {

        switch (H5Tget_class(type)) {
        case H5T_INTEGER:
            indentation(indent);
            printf("<%sAtomicType>\n",xmlnsprefix);
            indent += COL;
            /* <hdf5:IntegerType ByteOrder="bo" Sign="torf" Size="bytes"/> */
            ord = H5Tget_order(type);
            sgn = H5Tget_sign(type);
            indentation(indent);
            printf("<%sIntegerType ByteOrder=\"",xmlnsprefix);
            switch (ord) {
            case H5T_ORDER_LE:
                printf("LE");
                break;
            case H5T_ORDER_BE:
                printf("BE");
                break;
            case H5T_ORDER_VAX:
            default:
                printf("ERROR_UNKNOWN");
            }
            printf("\" Sign=\"");
            switch (sgn) {
            case H5T_SGN_NONE:
                printf("false");
                break;
            case H5T_SGN_2:
                printf("true");
                break;
            default:
                printf("ERROR_UNKNOWN");
            }
            printf("\" Size=\"");
            sz = H5Tget_size(type);
            printf("%lu", (unsigned long)sz);
            printf("\" />\n");
            indent -= COL;
            indentation(indent);
            printf("</%sAtomicType>\n",xmlnsprefix);
            break;

        case H5T_FLOAT:
            /* <hdf5:FloatType ByteOrder="bo" Size="bytes"
               SignBitLocation="bytes"
               ExponentBits="eb" ExponentLocation="el"
               MantissaBits="mb" MantissaLocation="ml" /> */
            ord = H5Tget_order(type);
            indentation(indent);
            printf("<%sAtomicType>\n",xmlnsprefix);
            indent += COL;
            indentation(indent);
            printf("<%sFloatType ByteOrder=\"",xmlnsprefix);
            switch (ord) {
            case H5T_ORDER_LE:
                printf("LE");
                break;
            case H5T_ORDER_BE:
                printf("BE");
                break;
            case H5T_ORDER_VAX:
            default:
                printf("ERROR_UNKNOWN");
            }
            printf("\" Size=\"");
            sz = H5Tget_size(type);
            printf("%lu", (unsigned long)sz);
            H5Tget_fields(type, &spos, &epos, &esize, &mpos, &msize);
            printf("\" SignBitLocation=\"%lu\" ", (unsigned long)spos);
            printf("ExponentBits=\"%lu\" ExponentLocation=\"%lu\" ", (unsigned long)esize, (unsigned long)epos);
            printf("MantissaBits=\"%lu\" MantissaLocation=\"%lu\" />\n",
                   (unsigned long)msize, (unsigned long)mpos);
            indent -= COL;
            indentation(indent);
            printf("</%sAtomicType>\n",xmlnsprefix);
            break;

        case H5T_TIME:
            indentation(indent);
            printf("<%sAtomicType>\n",xmlnsprefix);
            indent += COL;
            indentation(indent);
            printf("<%sTimeType />\n",xmlnsprefix);
            printf("<!-- H5T_TIME: not yet implemented -->");
            indent -= COL;
            indentation(indent);
            printf("</%sAtomicType>\n",xmlnsprefix);
            break;

        case H5T_STRING:
            /* <hdf5:StringType Cset="cs" StrSize="chars" StrPad="pad" /> */
            size = H5Tget_size(type);
            str_pad = H5Tget_strpad(type);
            cset = H5Tget_cset(type);
            is_vlstr = H5Tis_variable_str(type);

            indentation(indent);
            printf("<%sAtomicType>\n",xmlnsprefix);
            indent += COL;
            indentation(indent);
            printf("<%sStringType Cset=\"",xmlnsprefix);
            if (cset == H5T_CSET_ASCII) {
                printf("H5T_CSET_ASCII\" ");
            } else {
                printf("unknown_cset\" ");
            }
            if(is_vlstr)
                printf("StrSize=\"H5T_VARIABLE\" StrPad=\"");
            else
                printf("StrSize=\"%d\" StrPad=\"", (int) size);
            if (str_pad == H5T_STR_NULLTERM) {
                printf("H5T_STR_NULLTERM\"/>\n");
            } else if (str_pad == H5T_STR_NULLPAD) {
                printf("H5T_STR_NULLPAD\"/>\n");
            } else if (str_pad == H5T_STR_SPACEPAD) {
                printf("H5T_STR_SPACEPAD\"/>\n");
            } else {
                printf("H5T_STR_ERROR\"/>\n");
            }
            indent -= COL;
            indentation(indent);
            printf("</%sAtomicType>\n",xmlnsprefix);
            break;

        case H5T_BITFIELD:
            /* <hdf5:BitfieldType ByteOrder="bo" Size="bytes"/> */
            ord = H5Tget_order(type);
            indentation(indent);
            printf("<%sAtomicType>\n",xmlnsprefix);
            indent += COL;
            indentation(indent);
            printf("<%sBitfieldType ByteOrder=\"",xmlnsprefix);
            switch (ord) {
            case H5T_ORDER_LE:
                printf("LE");
                break;
            case H5T_ORDER_BE:
                printf("BE");
                break;
            case H5T_ORDER_VAX:
            default:
                printf("ERROR_UNKNOWN");
            }
            size = H5Tget_size(type);
            printf("\" Size=\"%lu\"/>\n", (unsigned long)size);
            indent -= COL;
            indentation(indent);
            printf("</%sAtomicType>\n",xmlnsprefix);
            break;

        case H5T_OPAQUE:
            /* <hdf5:OpaqueType Tag="tag" Size="bytes" /> */
            indentation(indent);
            printf("<%sAtomicType>\n",xmlnsprefix);
            indent += COL;
            indentation(indent);
            printf("<%sOpaqueType Tag=\"%s\" ",xmlnsprefix, H5Tget_tag(type));
            size = H5Tget_size(type);
            printf("Size=\"%lu\"/>\n", (unsigned long)size);
            indent -= COL;
            indentation(indent);
            printf("</%sAtomicType>\n",xmlnsprefix);
            break;

        case H5T_COMPOUND:
            /* recursively describe the components of a compound datatype */

            /* type of a dataset */
            nmembers = H5Tget_nmembers(type);

            indentation(indent);
            printf("<%sCompoundType>\n",xmlnsprefix);

            /* List each member Field of the type */
            /*   <hdf5:Field FieldName="name" > */
            /*   <hdf5:DataType > */
            indent += COL;
            for (i = 0; i < nmembers; i++) {
                char *t_fname;

                fname = H5Tget_member_name(type, i);
                mtype = H5Tget_member_type(type, i);
                indentation(indent);
                t_fname = xml_escape_the_name(fname);
                printf("<%sField FieldName=\"%s\">\n",xmlnsprefix, t_fname);

                free(fname);
                free(t_fname);
                indent += COL;
                indentation(indent);
                printf("<%sDataType>\n",xmlnsprefix);
                indent += COL;
                xml_print_datatype(mtype,0);
                indent -= COL;
                indentation(indent);
                printf("</%sDataType>\n",xmlnsprefix);
                indent -= COL;

                indentation(indent);
                printf("</%sField>\n",xmlnsprefix);
            }
            indent -= COL;
            indentation(indent);
            printf("</%sCompoundType>\n",xmlnsprefix);
            break;

        case H5T_REFERENCE:
            indentation(indent);
            printf("<%sAtomicType>\n",xmlnsprefix);
            indent += COL;
            indentation(indent);
            /*  Only Object references supported at this time */
            printf("<%sReferenceType>\n",xmlnsprefix);
            indentation(indent + COL);
            printf("<%sObjectReferenceType />\n",xmlnsprefix);
            indentation(indent);
            printf("</%sReferenceType>\n",xmlnsprefix);
            indent -= COL;
            indentation(indent);
            printf("</%sAtomicType>\n",xmlnsprefix);
            break;

        case H5T_ENUM:
            /*  <hdf5:EnumType Nelems="ne" >
               list Name, values of enum
             */
            nmembs = H5Tget_nmembers(type);
            indentation(indent);
            printf("<%sAtomicType>\n",xmlnsprefix);
            indent += COL;
            indentation(indent);
            printf("<%sEnumType Nelems=\"%d\">\n",xmlnsprefix, nmembs);
            xml_print_enum(type);
            indentation(indent);
            printf("</%sEnumType>\n",xmlnsprefix);
            indent -= COL;
            indentation(indent);
            printf("</%sAtomicType>\n",xmlnsprefix);
            break;

        case H5T_VLEN:
            indentation(indent);
            printf("<%sVLType>\n",xmlnsprefix);
            super = H5Tget_super(type);
            indent += COL;
            indentation(indent);
            printf("<%sDataType>\n",xmlnsprefix);
            indent += COL;
            xml_print_datatype(super,0);
            indent -= COL;
            indentation(indent);
            printf("</%sDataType>\n",xmlnsprefix);
            indent -= COL;
            indentation(indent);
            printf("</%sVLType>\n",xmlnsprefix);
            H5Tclose(super);

            break;

        case H5T_ARRAY:
            /* Get array base type */
            super = H5Tget_super(type);

            /* Print lead-in */
            indentation(indent);
            printf("<%sArrayType Ndims=\"",xmlnsprefix);
            ndims = H5Tget_array_ndims(type);
            printf("%u\">\n", ndims);

            /* Get array information */
            H5Tget_array_dims(type, dims, perm);

            /* list of dimensions */
            indent += COL;
            if (perm != NULL) {
                /* for each dimension, list */
                for (i = 0; i < ndims; i++) {
                    indentation(indent);
                    printf("<%sArrayDimension DimSize=\"%u\" DimPerm=\"%u\"/>\n",
                           xmlnsprefix,(int) dims[i], (int) perm[i]);
                }
            } else {
                for (i = 0; i < ndims; i++) {
                    indentation(indent);
                    printf("<%sArrayDimension DimSize=\"%u\" DimPerm=\"0\"/>\n",
                           xmlnsprefix,
                           (int) dims[i]);
                }
            }
            indent -= COL;

            indent += COL;
            indentation(indent);
            printf("<%sDataType>\n",xmlnsprefix);
            indent += COL;
            xml_print_datatype(super,0);
            indent -= COL;
            indentation(indent);
            printf("</%sDataType>\n",xmlnsprefix);
            indent -= COL;
            indentation(indent);
            printf("</%sArrayType>\n",xmlnsprefix);
            /* Close array base type */
            H5Tclose(super);
            break;

        default:
            printf("<!-- unknown data type -->");
            d_status = EXIT_FAILURE;
            break;
        }
    } /* end else */
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_datatype
 *
 * Purpose:     Dump description of a datatype in XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_dump_datatype(hid_t type)
{
    H5G_stat_t              statbuf;

    indent += COL;
    indentation(indent);

    if (H5Tcommitted(type) > 0) {
        obj_t  *found_obj;    /* Found object */

    /* Data type is a shared or named data type */
    H5Gget_objinfo(type, ".", TRUE, &statbuf);
    found_obj = search_obj(type_table, statbuf.objno);

    if (found_obj) {
        /* Shared data type, must be entered as an object  */
        /* These 2 cases are the same now, but may change */
        char * dtxid = malloc(100);
        xml_name_to_XID(found_obj->objname,dtxid,100,1);
        if (!found_obj->recorded) {
        /* anonymous stored data type:
           following the dumper's current
           practice:
           use it's object ref as its name
         */
        printf("<%sNamedDataTypePtr OBJ-XID=\"%s\"/>\n",
            xmlnsprefix, dtxid);

        } else {
        /* pointer to a named data type already in XML */
                char *t_objname = xml_escape_the_name(found_obj->objname);
        printf("<%sNamedDataTypePtr OBJ-XID=\"%s\" H5Path=\"%s\" />\n",
            xmlnsprefix, dtxid,t_objname);
        free(t_objname);
        }
        free(dtxid);
    } else {
        printf("<!-- h5dump error: unknown committed type. -->\n");
    }
    indent -= COL;
    return;
    }
    printf("<%sDataType>\n",xmlnsprefix);
    indent += COL;
    xml_print_datatype(type,0);
    indent -= COL;
    indentation(indent);
    printf("</%sDataType>\n",xmlnsprefix);
    indent -= COL;
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_dataspace
 *
 * Purpose:     Dump description of a dataspace in XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_dump_dataspace(hid_t space)
{
    hsize_t                 size[H5DUMP_MAX_RANK];
    hsize_t                 maxsize[H5DUMP_MAX_RANK];
    int                     ndims =
    H5Sget_simple_extent_dims(space, size, maxsize);
    H5S_class_t             space_type = H5Sget_simple_extent_type(space);
    int                     i;

    indentation(indent + COL);
    printf("<%sDataspace>\n", xmlnsprefix);
    indentation(indent + COL + COL);

    switch (space_type) {
        case H5S_SCALAR:
        /* scalar dataspace (just a tag, no XML attrs. defined */
        printf("<%sScalarDataspace />\n",xmlnsprefix);

            break;
        case H5S_SIMPLE:
        /* simple dataspace */
        /* <hdf5:SimpleDataspace Ndims="nd"> */
        printf("<%sSimpleDataspace Ndims=\"%d\">\n",xmlnsprefix, ndims);

        /* print the <hdf5:Dimension> elements */
        for (i = 0; i < ndims; i++) {
        indentation(indent + COL + COL + COL);
        if (maxsize[i] == H5S_UNLIMITED) {
            HDfprintf(stdout,
                  "<%sDimension  DimSize=\"%Hu\" MaxDimSize=\"UNLIMITED\"/>\n",
                  xmlnsprefix,size[i]);
        } else if (maxsize[i] == (hsize_t) 0) {
            HDfprintf(stdout,
                  "<%sDimension  DimSize=\"%Hu\" MaxDimSize=\"%Hu\"/>\n",
                  xmlnsprefix,size[i], size[i]);
        } else {
            HDfprintf(stdout,
                  "<%sDimension  DimSize=\"%Hu\" MaxDimSize=\"%Hu\"/>\n",
                  xmlnsprefix, size[i], maxsize[i]);
        }
        }
        indentation(indent + COL + COL);
        printf("</%sSimpleDataspace>\n", xmlnsprefix );

            break;
        case H5S_COMPLEX:
        printf("<!-- not yet implemented -->\n");

            break;
        case H5S_NO_CLASS:
        default:
        printf("<!-- unknown dataspace -->\n");
    }

    indentation(indent + COL);
    printf("</%sDataspace>\n", xmlnsprefix);
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_data
 *
 * Purpose:     Dump description of data in XML.
 *              Note that this calls the h5dump_xxx calls in
 *              the h5tools library.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_dump_data(hid_t obj_id, int obj_data, struct subset_t UNUSED * sset, int UNUSED pindex)
{
    h5tool_format_t               *outputformat = &xml_dataformat;
    int                     status = -1;
    void                   *buf;
    hid_t                   space, type, p_type;
    int                     ndims, i;
    hsize_t                 size[64], nelmts = 1;
    int                     depth;
    int                     stdindent = COL;    /* should be 3 */

    if (nCols==0) {
        outputformat->line_ncols = 65535;
        outputformat->line_per_line = 1;
    }
    else
        outputformat->line_ncols = nCols;
    indent += COL;

    /*
     * the depth will tell us how far we need to indent extra.  we use to just
     * use indent but with the merging of the tools lib we have to do
     * something different for the lib funtions... the normal indentation is 6
     * so when we don't need any extra indentation, depth will be 0.
     */
    depth = indent / stdindent + 1;

    /* Print all the values. */
    indentation(indent);
    printf("<%sData>\n", xmlnsprefix);
    indentation(indent + COL);
    printf("<%sDataFromFile>\n",xmlnsprefix);
    if (obj_data == DATASET_DATA) {
    type = H5Dget_type(obj_id);
    if (H5Tget_class(type) == H5T_REFERENCE) {
        status = xml_print_refs(obj_id, DATASET_DATA);
    } else if (H5Tget_class(type) == H5T_STRING) {
        status = xml_print_strs(obj_id, DATASET_DATA);
    } else {
        status = h5tools_dump_dset(stdout, outputformat, obj_id, -1, NULL, depth);
    }
    } else {
    /* Attribute data */
    type = H5Aget_type(obj_id);

    if (H5Tget_class(type) == H5T_REFERENCE) {
        /* references are done differently than
           the standard output:
           XML dumps a path to the object
           referenced.
         */
        status = xml_print_refs(obj_id, ATTRIBUTE_DATA);
        H5Tclose(type);
    } else if (H5Tget_class(type) == H5T_STRING) {
        status = xml_print_strs(obj_id, ATTRIBUTE_DATA);
    } else {
        /* all other data */
            p_type = h5tools_get_native_type(type);

        H5Tclose(type);

        space = H5Aget_space(obj_id);

        ndims = H5Sget_simple_extent_dims(space, size, NULL);

        for (i = 0; i < ndims; i++)
        nelmts *= size[i];

        buf =
        malloc((size_t)(nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type))));
        assert(buf);

        if (H5Aread(obj_id, p_type, buf) >= 0)
                status = h5tools_dump_mem(stdout, outputformat, obj_id,
                                          p_type, space, buf, depth);

        free(buf);
        H5Tclose(p_type);
        H5Sclose(space);
        H5Tclose(type);
    }
    }

    if (status == FAIL) {
    indentation(indent + COL);
    printf("Unable to print data.\n");
    status = 1;
    }

    indentation(indent + COL);
    printf("</%sDataFromFile>\n",xmlnsprefix);
    indentation(indent);
    printf("</%sData>\n", xmlnsprefix);
    indent -= COL;
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_attr
 *
 * Purpose:     Dump a description of an HDF5 attribute in XML.
 *
 * Return:      herr_t
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
xml_dump_attr(hid_t attr, const char *attr_name, void UNUSED * op_data)
{
    hid_t   attr_id, type, space;
    char   *t_aname = xml_escape_the_name(attr_name);

    indentation(indent);
    printf("<%sAttribute Name=\"%s\">\n",xmlnsprefix, t_aname);
    free(t_aname);

    if ((attr_id = H5Aopen_name(attr, attr_name)) >= 0) {
    type = H5Aget_type(attr_id);
    space = H5Aget_space(attr_id);

    dump_function_table->dump_dataspace_function(space);
    dump_function_table->dump_datatype_function(type);

    if (display_attr_data) {
        switch (H5Tget_class(type)) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_STRING:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_ARRAY:
        dump_function_table->dump_data_function(attr_id, ATTRIBUTE_DATA, NULL, 0);
        break;

        case H5T_TIME:
        indent += COL;
        indentation(indent);
        printf("<%sData>\n",xmlnsprefix);
        indentation(indent);
        printf("<!-- Time data not yet implemented. -->\n");
        indentation(indent);
        printf("<%sNoData/>\n",xmlnsprefix);
        indentation(indent);
        printf("<hdf5:Data>\n");
        printf("</%sData>\n",xmlnsprefix);
        indent -= COL;
        break;

        case H5T_COMPOUND:
        indentation(indent);
        printf("<!-- Note: format of compound data not specified -->\n");
        dump_function_table->dump_data_function(attr_id, ATTRIBUTE_DATA, NULL, 0);
        break;

        case H5T_REFERENCE:
        indentation(indent);
        printf("<%sData>\n",xmlnsprefix);
        indentation(indent);
                if (!H5Tequal(type, H5T_STD_REF_OBJ)) {
                   printf("<!-- Note: Region references not supported -->\n");
                   indentation(indent);
                   printf("<%sNoData />\n",xmlnsprefix);
                } else {
            printf("<%sDataFromFile>\n",xmlnsprefix);
            xml_print_refs(attr_id, ATTRIBUTE_DATA);
            indentation(indent);
            printf("</%sDataFromFile>\n",xmlnsprefix);
                }
        indentation(indent);
        printf("</%sData>\n",xmlnsprefix);
        break;

        case H5T_VLEN:
        printf("<!-- Note: format of VL data not specified -->\n");
        dump_function_table->dump_data_function(attr_id, ATTRIBUTE_DATA, NULL, 0);
        break;
        default:
        indentation(indent);
        printf("<%sData>\n",xmlnsprefix);
        indentation(indent);
        printf("<!-- Unknown datatype: %d -->\n", H5Tget_class(type));
        indentation(indent);
        printf("<%sNoData/>\n",xmlnsprefix);
        indentation(indent);
        printf("</%sData>\n",xmlnsprefix);
        break;
        }
    } else {
        /* The case of an attribute never yet written ?? */
        indentation(indent + COL);
        printf("<%sData>\n",xmlnsprefix);
        indentation(indent + COL + COL);
        printf("<%sNoData/>\n",xmlnsprefix);
        indentation(indent + COL);
        printf("</%sData>\n",xmlnsprefix);
    }

    H5Tclose(type);
    H5Sclose(space);
    H5Aclose(attr_id);
    indentation(indent);
    printf("</%sAttribute>\n", xmlnsprefix );
    return SUCCEED;

    } else {
    /* ?? failed */
    indentation(indent + COL);
    printf("<!-- h5dump error: unable to open attribute. -->\n");
    indentation(indent);
    printf("</%sAttribute>\n", xmlnsprefix );
    d_status = EXIT_FAILURE;
    return FAIL;
    }
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_named_datatype
 *
 * Purpose:     Dump a description of an HDF5 NDT in XML.
 *
 * Return:      herr_t
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_dump_named_datatype(hid_t type, const char *name)
{
    char                   *tmp;
    char * dtxid;
    char * parentxid;
    char *t_tmp;
    char *t_prefix;
    char   *t_name;

    tmp = HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    HDstrcpy(tmp, prefix);
    HDstrcat(tmp, "/");
    HDstrcat(tmp, name);

    indentation(indent);
    /*char **/ dtxid = HDmalloc(100);
    /*char **/ parentxid = HDmalloc(100);
    /*char **/t_tmp = xml_escape_the_name(tmp);
    /*char **/t_prefix = xml_escape_the_name(prefix);
    /*char   **/t_name = xml_escape_the_name(name);
    xml_name_to_XID(tmp,dtxid,100,1);
    xml_name_to_XID(prefix,parentxid,100,1);
    if (strncmp(name, "#", 1) == 0) {
    /*  Special:  this is an 'anonymous' NDT, deleted but
       still in use.
       We follow the dumper's undocumented practice, and
       use its object id as its name.
       Exactly the same as normal, but a separate case
       in the event we want to do something else in
       the future.
     */
    printf("<%sNamedDataType Name=\"%s\" OBJ-XID=\"%s\" "
        "Parents=\"%s\" H5ParentPaths=\"%s\">\n",
        xmlnsprefix,
        name, dtxid,
        parentxid,(HDstrcmp(prefix, "") ? t_prefix : "/"));
    } else {
    printf("<%sNamedDataType Name=\"%s\" OBJ-XID=\"%s\" "
        "H5Path=\"%s\" Parents=\"%s\" H5ParentPaths=\"%s\">\n",
        xmlnsprefix,
        t_name, dtxid,
        t_tmp, parentxid, (HDstrcmp(prefix, "") ? t_prefix : "/"));
    }
    HDfree(dtxid);
    HDfree(parentxid);
    HDfree(t_tmp);
    HDfree(t_prefix);
    HDfree(t_name);
    HDfree(tmp);

    indent += COL;
    indentation(indent);
    printf("<%sDataType>\n",xmlnsprefix);

    indent += COL;
    xml_print_datatype(type,1);

    indent -= COL;
    indentation(indent);
    printf("</%sDataType>\n",xmlnsprefix);

    indent -= COL;
    indentation(indent);
    printf("</%sNamedDataType>\n",xmlnsprefix);
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_group
 *
 * Purpose:     Dump a description of an HDF5 Group (and its members) in XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_dump_group(hid_t gid, const char *name)
{
    H5G_stat_t              statbuf;
    char                   *cp;
    hid_t                   dset, type;
    char                    type_name[1024], *tmp = NULL;
    char                   *par = NULL;
    int                     isRoot = 0;
    int                     xtype;
    char *ptrstr;
    char *t_objname;
    char *par_name;

    if (HDstrcmp(name, "/") == 0) {
    isRoot = 1;
    tmp = HDmalloc(2);
    HDstrcpy(tmp, "/");
    } else {
    tmp = HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    HDstrcpy(tmp, prefix);
    par = HDstrdup(tmp);
    cp = HDstrrchr(par, '/');
    if (cp != NULL) {
        if ((cp == par) && HDstrlen(par) > 1) {
        *(cp + 1) = '\0';
        } else {
        *cp = '\0';
        }
    }
    }

    indentation(indent);

    indent += COL;
    H5Gget_objinfo(gid, ".", TRUE, &statbuf);

    if (statbuf.nlink > 1) {
        obj_t  *found_obj;    /* Found object */

    /* Group with more than one link to it... */
    found_obj = search_obj(group_table, statbuf.objno);

    if (found_obj == NULL) {
        indentation(indent);
            error_msg(progname, "internal error (file %s:line %d)\n",
                      __FILE__, __LINE__);
        d_status = EXIT_FAILURE;
    } else {
            char *t_name = xml_escape_the_name(name);
            char *grpxid = malloc(100);
            char *parentxid = malloc(100);

            if (found_obj->displayed) {
                /* already seen: enter a groupptr */
                if (isRoot) {
                    /* probably can't happen! */
                    xml_name_to_XID("/",grpxid,100,1);
                    printf("<%sRootGroup OBJ-XID=\"%s\" H5Path=\"%s\">\n",
                            xmlnsprefix,grpxid,"/");
                } else {
                    t_objname = xml_escape_the_name(found_obj->objname);
                    par_name = xml_escape_the_name(par);
                    xml_name_to_XID(tmp,grpxid,100,1);
                    xml_name_to_XID(par,parentxid,100,1);
                    printf("<%sGroup Name=\"%s\" OBJ-XID=\"%s-%d\" H5Path=\"%s\" "
                            "Parents=\"%s\" H5ParentPaths=\"%s\">\n",
                            xmlnsprefix,t_name, grpxid, get_next_xid(),
                            t_objname, parentxid, par_name);
                    free(t_objname);
                    free(par_name);
                }

                indentation(indent + COL);
                ptrstr = malloc(100);
                t_objname = xml_escape_the_name(found_obj->objname);
                par_name = xml_escape_the_name(par);
                xml_name_to_XID(par,parentxid,100,1);
                xml_name_to_XID(found_obj->objname,ptrstr,100,1);
                printf("<%sGroupPtr OBJ-XID=\"%s\" H5Path=\"%s\" "
                            "Parents=\"%s\" H5ParentPaths=\"%s\" />\n",
                            xmlnsprefix,
                            ptrstr, t_objname, parentxid, par_name);
                free(t_objname);
                free(par_name);

            } else {

                /* first time this group has been seen -- describe it  */
                if (isRoot) {
                    xml_name_to_XID("/",grpxid,100,1);
                    printf("<%sRootGroup OBJ-XID=\"%s\" H5Path=\"%s\">\n",
                            xmlnsprefix,grpxid,"/");
                } else {
                    char *t_tmp = xml_escape_the_name(tmp);
                    par_name = xml_escape_the_name(par);
                    xml_name_to_XID(tmp,grpxid,100,1);
                    xml_name_to_XID(par,parentxid,100,1);
                    printf("<%sGroup Name=\"%s\" OBJ-XID=\"%s\" H5Path=\"%s\" "
                            "Parents=\"%s\" H5ParentPaths=\"%s\" >\n",
                           xmlnsprefix,t_name, grpxid, t_tmp, parentxid, par_name);
                    free(t_tmp);
                    free(par_name);
                }
                found_obj->displayed = TRUE;

                /* 1.  do all the attributes of the group */
                H5Aiterate(gid, NULL,
                           dump_function_table->dump_attribute_function, NULL);

                if (!strcmp(name, "/") && unamedtype) {
                    unsigned u;

                    /* Very special case: dump unamed type in root group */
                    for (u = 0; u < type_table->nobjs; u++) {
                        if (!type_table->objs[u].recorded) {
                            unsigned long   objno[2];   /*object number         */

                            dset = H5Dopen(gid, type_table->objs[u].objname);
                            type = H5Dget_type(dset);

                            objno[0] = (unsigned long)(type_table->objs[u].objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
                            objno[1] = (unsigned long)(type_table->objs[u].objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                            objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                            sprintf(type_name, "#%lu:%lu", objno[0], objno[1]);
                            dump_function_table->dump_named_datatype_function(type, type_name);

                            H5Tclose(type);
                            H5Dclose(dset);
                        }
                    }
                }

                /* iterate through all the members */
                xtype = H5G_TYPE;
                H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
                xtype = H5G_DATASET;
                H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
                xtype = H5G_GROUP;
                H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
                xtype = H5G_LINK;
                H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);

            }
            free(t_name);
            free(grpxid);
            free(parentxid);
        }
    } else {

    /* only link -- must be first time! */
        char *t_name = xml_escape_the_name(name);
    char *grpxid = malloc(100);
    char *parentxid = malloc(100);
    if (isRoot) {
        xml_name_to_XID("/",grpxid,100,1);
        printf("<%sRootGroup OBJ-XID=\"%s\" H5Path=\"%s\">\n",
            xmlnsprefix,grpxid,"/");
    } else {
        char *t_tmp = xml_escape_the_name(tmp);
        par_name = xml_escape_the_name(par);
        xml_name_to_XID(tmp,grpxid,100,1);
        xml_name_to_XID(par,parentxid,100,1);
        printf("<%sGroup Name=\"%s\" OBJ-XID=\"%s\" H5Path=\"%s\" "
            "Parents=\"%s\" H5ParentPaths=\"%s\" >\n",
                xmlnsprefix,t_name, grpxid, t_tmp,
            parentxid, par_name);
        free(t_tmp);
        free(par_name);
    }
        free(t_name);
    free(grpxid);
    free(parentxid);
    /* 1.  do all the attributes of the group */
    H5Aiterate(gid, NULL, dump_function_table->dump_attribute_function, NULL);

    if (!strcmp(name, "/") && unamedtype) {
            unsigned u;

        /* Very special case: dump unamed type in root group */
        for (u = 0; u < type_table->nobjs; u++) {
        if (!type_table->objs[u].recorded) {
                    unsigned long   objno[2];   /*object number         */

            dset = H5Dopen(gid, type_table->objs[u].objname);
            type = H5Dget_type(dset);

                    objno[0] = (unsigned long)(type_table->objs[u].objno);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
                    objno[1] = (unsigned long)(type_table->objs[u].objno >> 8*sizeof(long));
#else /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
                    objno[1] = 0;
#endif /* H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG */
            sprintf(type_name, "#%lu:%lu", objno[0], objno[1]);
            dump_function_table->dump_named_datatype_function(type, type_name);

            H5Tclose(type);
            H5Dclose(dset);
        }
        }
    }

    /* iterate through all the members */
    xtype = H5G_TYPE;
    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
    xtype = H5G_DATASET;
    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
    xtype = H5G_GROUP;
    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
    xtype = H5G_LINK;
    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);

    }

    indent -= COL;
    indentation(indent);
    if (isRoot) {
    printf("</%sRootGroup>\n",xmlnsprefix);
    } else {
    printf("</%sGroup>\n" ,xmlnsprefix);
    }
    free(tmp);
}

/*-------------------------------------------------------------------------
 * Function:    xml_print_refs
 *
 * Purpose:     Print a path to the objects referenced by HDF5 Referneces.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
xml_print_refs(hid_t did, int source)
{
    herr_t                  e;
    hid_t                   type, space;
    char                   *buf;
    hobj_ref_t             *refbuf;
    hsize_t                 ssiz;
    hsize_t                 i;

    if (source == DATASET_DATA) {
    type = H5Dget_type(did);
    } else if (source == ATTRIBUTE_DATA) {
    type = H5Aget_type(did);
    } else {
    /* return an error */
    return FAIL;
    }
    if (H5Tget_class(type) != H5T_REFERENCE) {
    /* return an error */
    return FAIL;
    }
    if (!H5Tequal(type, H5T_STD_REF_OBJ)) {
    /* region ref not supported yet... */
    /* return an error */
    return FAIL;
    }
    if (source == DATASET_DATA) {
    space = H5Dget_space(did);
    ssiz = H5Sget_simple_extent_npoints(space);
    ssiz *= H5Tget_size(type);

    buf = calloc((size_t)ssiz, sizeof(char));
    if (buf == NULL) {
        return FAIL;
    }
    e = H5Dread(did, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    /* need to check result here */
    if (e < 0) {
        free(buf);
        return FAIL;
    }

    } else if (source == ATTRIBUTE_DATA) {
    space = H5Aget_space(did);
    ssiz = H5Sget_simple_extent_npoints(space);
    ssiz *= H5Tget_size(type);

    buf = calloc((size_t)ssiz, sizeof(char));
    if (buf == NULL) {
        free(buf);
        return FAIL;
    }
    e = H5Aread(did, H5T_STD_REF_OBJ, buf);
    /* need to check the result here */
    } else {
    /* error */
    return FAIL;
    }

    refbuf = (hobj_ref_t *) buf;
    ssiz = H5Sget_simple_extent_npoints(space);

    for (i = 0; i < ssiz; i++) {
        const char *path;

    path = lookup_ref_path(*refbuf);
    indentation(indent + COL);

    if (!path) {
        printf("\"%s\"\n", "NULL");
    } else {
            char *t_path = xml_escape_the_string(path, -1);

        printf("\"%s\"\n", t_path);
            free(t_path);
    }

    refbuf++;
    }

    free(buf);

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    xml_print_strs
 *
 * Purpose:     Print strings.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
xml_print_strs(hid_t did, int source)
{
    herr_t                  e;
    hid_t                   type, space;
    void                   *buf;
    char                   *bp;
    char                   *onestring=NULL;
    hsize_t                 ssiz;
    size_t                  tsiz, str_size=0;
    size_t                  i;
    htri_t                  is_vlstr;

    if (source == DATASET_DATA) {
    type = H5Dget_type(did);
    } else if (source == ATTRIBUTE_DATA) {
    type = H5Aget_type(did);
    } else {
    /* return an error */
    return FAIL;
    }
    if (H5Tget_class(type) != H5T_STRING) {
    /* return an error */
    return FAIL;
    }
    is_vlstr = H5Tis_variable_str(type);

    if (source == DATASET_DATA) {
    space = H5Dget_space(did);
    ssiz = H5Sget_simple_extent_npoints(space);
    ssiz *= H5Tget_size(type);

    buf = malloc((size_t)ssiz);

    if (buf == NULL) {
        return FAIL;
    }

    e = H5Dread(did, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    if (e < 0) {
        free(buf);
        return FAIL;
    }
    } else if (source == ATTRIBUTE_DATA) {
    space = H5Aget_space(did);
    ssiz = H5Sget_simple_extent_npoints(space);
    ssiz *= H5Tget_size(type);

    buf = malloc((size_t)ssiz);
    if (buf == NULL) {
        return FAIL;
    }

    e = H5Aread(did, type, buf);
    if (e < 0) {
        free(buf);
        return FAIL;
    }
    } else {
    /* error */
    return FAIL;
    }

    /* pull out each string... */
    ssiz = H5Sget_simple_extent_npoints(space);

    tsiz = H5Tget_size(type);
    bp = (char*)buf;
    if(!is_vlstr)
        onestring = (char *) calloc(tsiz, sizeof(char));

    for (i = 0; i < ssiz; i++) {
        if(is_vlstr) {
            onestring = *(char **)bp;
            if(onestring)
                str_size = (size_t)HDstrlen(onestring);
        } else {
            HDstrncpy(onestring, bp, tsiz);
            str_size = tsiz;
        }
    indentation(indent + COL);

    if (!onestring) {
        printf("NULL\n");
    } else {
            char *t_onestring = xml_escape_the_string(onestring, (int)str_size);

        printf("\"%s\"\n", t_onestring);
            free(t_onestring);
    }

       bp += tsiz;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    check_filters
 *
 * Purpose:     private function to check for the filters and
 *              put tags in the XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
check_filters(hid_t dcpl)
{
    int                     nfilt;
    int                     i;
    H5Z_filter_t            filter;
    char                    namebuf[120];
    size_t                  cd_nelmts = 20;
    unsigned int            cd_values[20];
    unsigned int            flags;

    nfilt = H5Pget_nfilters(dcpl);
    if (nfilt <= 0)
    return;
    for (i = 0; i < nfilt; i++) {
    filter = H5Pget_filter(dcpl, (unsigned)i, &flags,
                   (size_t *) &cd_nelmts,
                   cd_values, 120, namebuf);
    if (filter == H5Z_FILTER_DEFLATE) {
        indentation(indent + COL);
        printf("<%sDeflate Level=\"",xmlnsprefix);
            if (cd_nelmts < 1) {
        /* not sure what this means? */
            printf("6");
            } else {
            printf("%d",cd_values[0]);
            }
        printf("\"/>\n");
    } else if (filter == H5Z_FILTER_FLETCHER32) {
        indentation(indent + COL);
        printf("<%sFletcher32 />",xmlnsprefix);
    } else if (filter == H5Z_FILTER_SHUFFLE) {
        indentation(indent + COL);
        printf("<%sShuffle />",xmlnsprefix);
    } else if (filter == H5Z_FILTER_SZIP) {

        indentation(indent + COL);
        printf("<%sSZIP ",xmlnsprefix);
            if (cd_nelmts < 2) {
        /* no pixels ? */
            printf("Pixels_per_block=\"-1\" ");
            } else {
            printf("Pixels_per_block=\"%d\" ",cd_values[1]);
            }
        /* analyse the options mask */
        if (cd_values[0] & H5_SZIP_CHIP_OPTION_MASK) {
                    printf("Mode =\"Hardware\" ");
                } else if (cd_values[0] & H5_SZIP_ALLOW_K13_OPTION_MASK) {
                    printf("Mode =\"K13\" ");
                }
                printf("Coding=\"");
        if (cd_values[0] & H5_SZIP_EC_OPTION_MASK) {
                    printf("Entropy");
                } else if (cd_values[0] & H5_SZIP_NN_OPTION_MASK) {
                    printf("NN");
                }
                printf("\" ");

                printf("ByteOrder=\"");
        if (cd_values[0] & H5_SZIP_LSB_OPTION_MASK) {
                    printf("LSB");
                } else if (cd_values[0] & H5_SZIP_MSB_OPTION_MASK) {
                    printf("MSB");
                }
                printf("\" ");

        if (cd_values[0] & H5_SZIP_RAW_OPTION_MASK) {
                    printf("Header=\"Raw\"");
                }
            printf("/>\n");
    } else {
        /* unknown option */
    }
    }
}

static void
xml_dump_fill_value(hid_t dcpl, hid_t type)
{
    size_t sz;
    size_t i;
    hsize_t space;
    void * buf;
    char * name;

    indent += COL;
    indentation(indent);
    printf("<%sData>\n",xmlnsprefix);
    indent += COL;

    space = H5Tget_size(type);
    buf = malloc((size_t)space);

    H5Pget_fill_value(dcpl, type, buf);

    if (H5Tget_class(type) == H5T_REFERENCE) {
        const char * path;

        path = lookup_ref_path(*(hobj_ref_t *)buf);

        indentation(indent);
        printf("<%sDataFromFile>\n",xmlnsprefix);
        if (!path) {
            printf("\"%s\"\n", "NULL");
        } else {
            char *t_path = xml_escape_the_string(path, -1);

            printf("\"%s\"\n", t_path);
            free(t_path);
        }
        indentation(indent);
        printf("</%sDataFromFile>\n",xmlnsprefix);
    } else if (H5Tget_class(type) == H5T_STRING) {
            /* ????? */
            indentation(indent);
            printf("<!-- String fill values not yet implemented. -->\n");
            indentation(indent);
            printf("<%sNoData />\n",xmlnsprefix);
    } else {
        /* all other data */
        switch (H5Tget_class(type)) {
            case H5T_INTEGER:
                    indentation(indent);
                    printf("<%sDataFromFile>\n",xmlnsprefix);
                    indentation(indent);
                    printf("\"%d\"\n",*(int *)buf);
                    indentation(indent);
                    printf("</%sDataFromFile>\n",xmlnsprefix);
                    break;
            case H5T_FLOAT:
                    indentation(indent);
                    printf("<%sDataFromFile>\n",xmlnsprefix);
                    indentation(indent);
                    printf("\"%f\"\n",*(float *)buf);
                    indentation(indent);
                    printf("</%sDataFromFile>\n",xmlnsprefix);
                    break;
            case H5T_BITFIELD:
            case H5T_OPAQUE:
                    indentation(indent);
                    printf("<%sDataFromFile>\n",xmlnsprefix);
                    sz = H5Tget_size(type);
                    indentation(indent);
                    printf("\"");
                    for (i = 0; i < sz; i++) {
                            printf("%x ",*(unsigned int *)buf);
                            buf = (char *)buf + sizeof(unsigned int);
                    }
                    printf("\"\n");
                    indentation(indent);
                    printf("</%sDataFromFile>\n",xmlnsprefix);
                    break;
            case H5T_ENUM:
                    indentation(indent);
                    printf("<%sDataFromFile>\n",xmlnsprefix);
                    name = H5Tget_member_name(type, *(unsigned *)buf);
                    indentation(indent);
                    printf("\"%s\"\n",name);
                    indentation(indent);
                    printf("</%sDataFromFile>\n",xmlnsprefix);
                    break;
            case H5T_ARRAY:
                    indentation(indent);
                printf("<!-- Array fill values not yet implemented. -->\n");
                indentation(indent);
                printf("<%sNoData />\n",xmlnsprefix);
                break;
            case H5T_TIME:
                    indentation(indent);
                printf("<!-- Time fill not yet implemented. -->\n");
                indentation(indent);
                printf("<%sNoData />\n",xmlnsprefix);
                break;
            case H5T_COMPOUND:
                    indentation(indent);
                printf("<!-- Compound fill not yet implemented. -->\n");
                indentation(indent);
                printf("<%sNoData />\n",xmlnsprefix);
                break;
    case H5T_VLEN:
            indentation(indent);
                printf("<!-- VL fill not yet implemented. -->\n");
                indentation(indent);
                printf("<%sNoData />\n",xmlnsprefix);
                break;
    default:
            indentation(indent);
        printf("<!-- Unknown fill datatype: %d -->\n", H5Tget_class(type));
        indentation(indent);
        printf("<%sNoData/>\n",xmlnsprefix);
        break;
    }
    }
    free(buf);
    indent -= COL;
    indentation(indent);
    printf("</%sData>\n",xmlnsprefix);
    indent -= COL;
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_group
 *
 * Purpose:     Dump a description of an HDF5 Group (and its members) in XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_dump_dataset(hid_t did, const char *name, struct subset_t UNUSED * sset)
{
    hid_t                   type, space;
    hid_t                   dcpl;
    H5D_fill_value_t        fvstatus;
    int                     maxdims;
    hsize_t                *chsize;
    int                     ndims;
    int                     i;
    H5D_alloc_time_t        at;
    H5D_fill_time_t        ft;
    hsize_t                 tempi;
    char                   *tmp;
    char                   *t_name, *t_tmp, *t_prefix;
    char *rstr = HDmalloc(100);
    char *pstr = HDmalloc(100);

    tmp = HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    HDstrcpy(tmp, prefix);
    HDstrcat(tmp, "/");
    HDstrcat(tmp, name);
    indentation(indent);

    t_name = xml_escape_the_name(name);
    t_tmp = xml_escape_the_name(tmp);
    t_prefix = xml_escape_the_name(prefix);

    xml_name_to_XID(tmp, rstr, 100, 1);
    xml_name_to_XID(prefix, pstr, 100, 1);
    printf("<%sDataset Name=\"%s\" OBJ-XID=\"%s\" H5Path= \"%s\" Parents=\"%s\" H5ParentPaths=\"%s\">\n",xmlnsprefix,
       t_name, rstr, t_tmp, pstr, strcmp(prefix, "") ? t_prefix : "/");

    HDfree(t_name);
    HDfree(t_tmp);
    HDfree(t_prefix);
    HDfree(rstr);
    HDfree(pstr);
    HDfree(tmp);

    dcpl = H5Dget_create_plist(did);
    type = H5Dget_type(did);
    space = H5Dget_space(did);

    /* Print information about storage layout */
    if (H5D_CHUNKED == H5Pget_layout(dcpl)) {
    maxdims = H5Sget_simple_extent_ndims(space);
    chsize = (hsize_t *) malloc(maxdims * sizeof(hsize_t));
    indent += COL;
    indentation(indent);
    printf("<%sStorageLayout>\n",xmlnsprefix);
    indent += COL;
    indentation(indent);
    printf("<%sChunkedLayout ",xmlnsprefix);
    ndims = H5Pget_chunk(dcpl, maxdims, chsize);
    printf("Ndims=\"%d\">\n", ndims);

    indent += COL;

    for (i = 0; i < ndims; i++) {
        indentation(indent);
        HDfprintf(stdout, "<%sChunkDimension DimSize=\"%Hu\" />\n",
               xmlnsprefix, chsize[i]);
    }

    indentation(indent);
    printf("<%sRequiredFilter>\n",xmlnsprefix);
    indent += COL;
    check_filters(dcpl);
    indent -= COL;
    indentation(indent);
    printf("</%sRequiredFilter>\n",xmlnsprefix);

    indent -= COL;

    indentation(indent);
    printf("</%sChunkedLayout>\n",xmlnsprefix);
    indent -= COL;
    indentation(indent);
    printf("</%sStorageLayout>\n",xmlnsprefix);
    indent -= COL;
    free(chsize);
    } else if (H5D_CONTIGUOUS == H5Pget_layout(dcpl)) {
    indent += COL;
    indentation(indent);
    printf("<%sStorageLayout>\n",xmlnsprefix);
    indent += COL;
    indentation(indent);
    printf("<%sContiguousLayout/>\n",xmlnsprefix);
    indent -= COL;
    indentation(indent);
    printf("</%sStorageLayout>\n",xmlnsprefix);
    indent -= COL;
    indentation(indent);
    } else if (H5D_COMPACT == H5Pget_layout(dcpl)) {
    indent += COL;
    indentation(indent);
    printf("<%sStorageLayout>\n",xmlnsprefix);
    indent += COL;
    indentation(indent);
    printf("<%sCompactLayout/>\n",xmlnsprefix);
    indent -= COL;
    indentation(indent);
    printf("</%sStorageLayout>\n",xmlnsprefix);
    indent -= COL;
    indentation(indent);
    }
    /* and check for external.... ?? */

    /* fill value */

    indent += COL;
    indentation(indent);
    printf("<%sFillValueInfo ",xmlnsprefix);
    H5Pget_fill_time(dcpl, &ft);
    printf("FillTime=\"");
    switch ( ft ) {
    case H5D_FILL_TIME_ALLOC:
                printf("FillOnAlloc");
        break;
    case H5D_FILL_TIME_NEVER:
                printf("FillNever");
        break;
    case H5D_FILL_TIME_IFSET:
                printf("FillIfSet");
        break;
    default:
                printf("?");
        break;
    }
    printf("\" ");
    H5Pget_alloc_time(dcpl, &at);
    printf("AllocationTime=\"");
    switch ( at ) {
    case H5D_ALLOC_TIME_EARLY:
                printf("Early");
        break;
    case H5D_ALLOC_TIME_INCR:
                printf("Incremental");
        break;
    case H5D_ALLOC_TIME_LATE:
                printf("Late");
        break;
    case H5D_ALLOC_TIME_DEFAULT:
    default:
                printf("?");
        break;
    }
    printf("\"");
    printf(">\n");

    indent += COL;
    indentation(indent);
    printf("<%sFillValue>\n",xmlnsprefix);
    indent += COL;
    H5Pfill_value_defined(dcpl, &fvstatus);
    if (fvstatus == H5D_FILL_VALUE_UNDEFINED ||
            (fvstatus==H5D_FILL_VALUE_DEFAULT && ft==H5D_FILL_TIME_IFSET)) {
        indentation(indent + COL);
        printf("<%sNoFill/>\n",xmlnsprefix);
    } else {
        xml_dump_fill_value(dcpl,type);
    }

    indent -= COL;
    indentation(indent);
    printf("</%sFillValue>\n",xmlnsprefix);

    indent -= COL;
    indentation(indent);
    printf("</%sFillValueInfo>\n",xmlnsprefix);
    indent -= COL;

    dump_function_table->dump_dataspace_function(space);
    dump_function_table->dump_datatype_function(type);

    indent += COL;
    H5Aiterate(did, NULL, dump_function_table->dump_attribute_function, NULL);
    indent -= COL;
    tempi = H5Dget_storage_size(did);

    if (display_data && (tempi > 0)) {
    switch (H5Tget_class(type)) {
    case H5T_INTEGER:
    case H5T_FLOAT:
    case H5T_STRING:
    case H5T_BITFIELD:
    case H5T_OPAQUE:
    case H5T_ENUM:
    case H5T_ARRAY:
        dump_function_table->dump_data_function(did, DATASET_DATA, NULL, 0);
        break;

    case H5T_TIME:
        indent += COL;
        indentation(indent);
        printf("<%sData>\n",xmlnsprefix);
        indentation(indent);
        printf("<!-- Time data not yet implemented. -->\n");
        indentation(indent);
        printf("<%sNoData />\n",xmlnsprefix);
        indentation(indent);
        printf("<%sData>\n",xmlnsprefix);
        indent -= COL;
        break;

    case H5T_COMPOUND:
        indentation(indent);
        printf("<!-- Note: format of compound data not specified -->\n");
        dump_function_table->dump_data_function(did, DATASET_DATA, NULL, 0);
        break;

    case H5T_REFERENCE:
        indentation(indent);
        printf("<%sData>\n",xmlnsprefix);
        indentation(indent);
            if (!H5Tequal(type, H5T_STD_REF_OBJ)) {
                printf("<!-- Note: Region references not supported -->\n");
                indentation(indent);
                printf("<%sNoData />\n",xmlnsprefix);
            } else {
            printf("<%sDataFromFile>\n",xmlnsprefix);
            xml_print_refs(did, DATASET_DATA);
            indentation(indent);
            printf("</%sDataFromFile>\n",xmlnsprefix);
            }
        indentation(indent);
        printf("</%sData>\n",xmlnsprefix);
        break;

    case H5T_VLEN:
        printf("<!-- Note: format of VL data not specified -->\n");
        dump_function_table->dump_data_function(did, DATASET_DATA, NULL, 0);
        break;
    default:
        indentation(indent);
        printf("<%sData>\n",xmlnsprefix);
        indentation(indent);
        printf("<!-- Unknown datatype: %d -->\n", H5Tget_class(type));
        indentation(indent);
        printf("<%sNoData/>\n",xmlnsprefix);
        indentation(indent);
        printf("</%sData>\n",xmlnsprefix);
        break;
    }
    } else {
    /* no data written */
    indentation(indent + COL);
    printf("<%sData>\n",xmlnsprefix);
    indentation(indent + COL + COL);
    printf("<%sNoData/>\n",xmlnsprefix);
    indentation(indent + COL);
    printf("</%sData>\n",xmlnsprefix);
    }

    H5Tclose(type);
    H5Sclose(space);
    H5Pclose(dcpl);
    indentation(indent);
    printf("</%sDataset>\n", xmlnsprefix);
}

/*-------------------------------------------------------------------------
 * Function:    xml_print_enum
 *
 * Purpose:     Print the values of an HDF5 ENUM in XML.
 *              Very similar to regular DDL output.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_print_enum(hid_t type)
{
    char                  **name = NULL;    /*member names                    */
    unsigned char          *value = NULL;   /*value array                    */
    unsigned                nmembs; /*number of members                */
    hid_t                   super;  /*enum base integer type        */
    hid_t                   native = -1;    /*native integer data type        */
    size_t                  dst_size;   /*destination value type size    */
    unsigned                i;  /*miscellaneous counters        */
    size_t                  j;

    nmembs = H5Tget_nmembers(type);
    super = H5Tget_super(type);

    indentation(indent);
    printf("<%sDataType>\n",xmlnsprefix);
    xml_print_datatype(super,0);
    indentation(indent);
    printf("</%sDataType>\n",xmlnsprefix);

    /*
     * Determine what data type to use for the native values.  To simplify
     * things we entertain three possibilities:
     *  1. long_long -- the largest native signed integer
     *    2. unsigned long_long -- the largest native unsigned integer
     *    3. raw format
     */
    if (H5Tget_size(type) <= sizeof(long_long)) {
    dst_size = sizeof(long_long);

    if (H5T_SGN_NONE == H5Tget_sign(type)) {
        native = H5T_NATIVE_ULLONG;
    } else {
        native = H5T_NATIVE_LLONG;
    }
    } else {
    dst_size = H5Tget_size(type);
    }

    /* Get the names and raw values of all members */
    name = calloc(nmembs, sizeof(char *));
    value = calloc(nmembs, MAX(H5Tget_size(type), dst_size));

    for (i = 0; i < nmembs; i++) {
    name[i] = H5Tget_member_name(type, i);
    H5Tget_member_value(type, i, value + i * H5Tget_size(type));
    }

    /* Convert values to native data type */
    if (native > 0)
    H5Tconvert(super, native, nmembs, value, NULL, H5P_DEFAULT);

    /* Sort members by increasing value */
    /*not implemented yet */

    /* Print members */
    indent += COL;
    for (i = 0; i < nmembs; i++) {
        char *t_name = xml_escape_the_name(name[i]);

    indentation(indent);
    printf("<%sEnumElement>\n",xmlnsprefix);
    indentation(indent + COL);
    printf("%s\n", t_name);
        free(t_name);
    indentation(indent);
    printf("</%sEnumElement>\n",xmlnsprefix);
    indentation(indent);
    printf("<%sEnumValue>\n",xmlnsprefix);
    indentation(indent + COL);
    if (native < 0) {
        printf("0x");

        for (j = 0; j < dst_size; j++)
        printf("%02x", value[i * dst_size + j]);
    } else if (H5T_SGN_NONE == H5Tget_sign(native)) {
        HDfprintf(stdout,"%" H5_PRINTF_LL_WIDTH "u", *((unsigned long_long *)
                          ((void *) (value + i * dst_size))));
    } else {
        HDfprintf(stdout,"%" H5_PRINTF_LL_WIDTH "d",
           *((long_long *) ((void *) (value + i * dst_size))));
    }
    printf("\n");
    indentation(indent);
    printf("</%sEnumValue>\n",xmlnsprefix);

    }
    indent -= COL;

    /* Release resources */
    for (i = 0; i < nmembs; i++)
    free(name[i]);

    free(name);
    free(value);
    H5Tclose(super);
}


/*-------------------------------------------------------------------------
 * Function:    h5_fileaccess
 *
 * Purpose: Returns a file access template which is the default template
 *      but with a file driver set according to the constant or
 *      environment variable HDF5_DRIVER
 *
 * Return:  Success:    A file access property list
 *
 *      Failure:    -1
 *
 * Programmer:  Robb Matzke
 *              Thursday, November 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
h5_fileaccess(void)
{
    static const char *multi_letters = "msbrglo";
    const char  *val = NULL;
    const char  *name;
    char s[1024];
    hid_t fapl = -1;

    /* First use the environment variable, then the constant */
    val = HDgetenv("HDF5_DRIVER");
#ifdef HDF5_DRIVER
    if (!val) val = HDF5_DRIVER;
#endif

    if ((fapl=H5Pcreate(H5P_FILE_ACCESS))<0) return -1;
    if (!val || !*val) return fapl; /*use default*/

    HDstrncpy(s, val, sizeof s);
    s[sizeof(s)-1] = '\0';
    if (NULL==(name=HDstrtok(s, " \t\n\r"))) return fapl;

    if (!HDstrcmp(name, "sec2")) {
    /* Unix read() and write() system calls */
    if (H5Pset_fapl_sec2(fapl)<0) return -1;
    } else if (!HDstrcmp(name, "stdio")) {
    /* Standard C fread() and fwrite() system calls */
    if (H5Pset_fapl_stdio(fapl)<0) return -1;
    } else if (!HDstrcmp(name, "core")) {
    /* In-core temporary file with 1MB increment */
    if (H5Pset_fapl_core(fapl, 1024*1024, FALSE)<0) return -1;
    } else if (!HDstrcmp(name, "split")) {
    /* Split meta data and raw data each using default driver */
    if (H5Pset_fapl_split(fapl,
                  "-m.h5", H5P_DEFAULT,
                  "-r.h5", H5P_DEFAULT)<0)
        return -1;
    } else if (!HDstrcmp(name, "multi")) {
    /* Multi-file driver, general case of the split driver */
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    const char *memb_name[H5FD_MEM_NTYPES];
    char sv[H5FD_MEM_NTYPES][1024];
    haddr_t memb_addr[H5FD_MEM_NTYPES];
        H5FD_mem_t  mt;

    HDmemset(memb_map, 0, sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);

    assert(HDstrlen(multi_letters)==H5FD_MEM_NTYPES);
    for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt)) {
        memb_fapl[mt] = H5P_DEFAULT;
        sprintf(sv[mt], "%%s-%c.h5", multi_letters[mt]);
        memb_name[mt] = sv[mt];
        memb_addr[mt] = MAX(mt-1,0)*(HADDR_MAX/10);
    }

    if (H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name,
                  memb_addr, FALSE)<0) {
        return -1;
    }
    } else if (!HDstrcmp(name, "family")) {
        hsize_t fam_size = 100*1024*1024; /*100 MB*/

    /* Family of files, each 1MB and using the default driver */
    if ((val=HDstrtok(NULL, " \t\n\r")))
        fam_size = (hsize_t)(HDstrtod(val, NULL) * 1024*1024);
    if (H5Pset_fapl_family(fapl, fam_size, H5P_DEFAULT)<0)
            return -1;
    } else if (!HDstrcmp(name, "log")) {
#ifdef H5_WANT_H5_V1_4_COMPAT
        long verbosity = 1;
#else /* H5_WANT_H5_V1_4_COMPAT */
        long log_flags = H5FD_LOG_LOC_IO;
#endif /* H5_WANT_H5_V1_4_COMPAT */

#ifdef H5_WANT_H5_V1_4_COMPAT
        /* Log file access */
        if ((val = strtok(NULL, " \t\n\r")))
            verbosity = strtol(val, NULL, 0);

        if (H5Pset_fapl_log(fapl, NULL, (int)verbosity) < 0)
        return -1;
#else /* H5_WANT_H5_V1_4_COMPAT */
        /* Log file access */
        if ((val = HDstrtok(NULL, " \t\n\r")))
            log_flags = HDstrtol(val, NULL, 0);

        if (H5Pset_fapl_log(fapl, NULL, (unsigned)log_flags, 0) < 0)
        return -1;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    } else {
    /* Unknown driver */
    return -1;
    }

    return fapl;
}

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
    assert(prfx_len > 0);
    *prfx = HDcalloc(prfx_len, 1);
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
static void
add_prefix(char **prfx, size_t *prfx_len, const char *name)
{
    size_t new_len = HDstrlen(*prfx) + HDstrlen(name) + 2;

    /* Check if we need more space */
    if(*prfx_len <= new_len) {
        *prfx_len = new_len + 1;
        *prfx = HDrealloc(*prfx, *prfx_len);
    }

    /* Append object name to prefix */
    HDstrcat(HDstrcat(*prfx, "/"), name);
} /* end add_prefix */

