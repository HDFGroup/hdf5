/*-------------------------------------------------------------------------
 *
 * Copyright (C) 1998, 1999, 2000, 2001
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 *
 *-------------------------------------------------------------------------
 */
#include <stdio.h>
#include <stdlib.h>

#include "h5dump.h"
#include "H5private.h"
#include "h5tools.h"

static herr_t           dump_all(hid_t group, const char *name,
				     void *op_data);
static int              display_oid = 0;
static int              display_data = 1;
static int              d_status = 0;
static int              unamedtype = 0;	/* shared data type with no name */

static int              prefix_len = 1024;
static table_t         *group_table = NULL, *dset_table = NULL, *type_table =
    NULL;
static char            *prefix;
static const dump_header *dump_header_format;

/**
 **  Added for XML  **
 **/

/* global variables for XML option */

static int              doxml = 0;
#define DEFAULT_DTD "http://hdf.ncsa.uiuc.edu/DTDs/HDF5-File.dtd"
static char            *xml_dtd_uri = NULL;
/* fill_ref_path_table is called to inialize the object
reference paths. */
hid_t                   thefile = -1;
static herr_t           fill_ref_path_table(hid_t, const char *,
					    void UNUSED *);
struct ref_path_table_entry_t * ref_path_table_lookup(char *);
char * lookup_ref_path(hobj_ref_t *);
void check_compression(hid_t );
/** end XML **/

/* internal functions used by standard format */

static void             dump_oid(hid_t oid);
static void             print_enum(hid_t type);

/* external functions */
extern void             indentation(int);
extern int              print_data(hid_t, hid_t, int);

static h5dump_t         dataformat = {
    0,				/*raw */

    "",				/*fmt_raw */
    "%d",			/*fmt_int */
    "%u",			/*fmt_uint */
    "%d",			/*fmt_schar */
    "%u",			/*fmt_uchar */
    "%d",			/*fmt_short */
    "%u",			/*fmt_ushort */
    "%ld",			/*fmt_long */
    "%lu",			/*fmt_ulong */
    NULL,			/*fmt_llong */
    NULL,			/*fmt_ullong */
    "%g",			/*fmt_double */
    "%g",			/*fmt_float */

    0,				/*ascii */
    0,				/*str_locale */
    0,				/*str_repeat */

    "[ ",			/*arr_pre */
    ", ",			/*arr_sep */
    " ]",			/*arr_suf */
    1,				/*arr_linebreak */

    "",				/*cmpd_name */
    ",\n",			/*cmpd_sep */
    "{\n",			/*cmpd_pre */
    "}",			/*cmpd_suf */
    "\n",			/*cmpd_end */

    ",",			/*vlen_sep */
    "(",			/*vlen_pre */
    ")",			/*vlen_suf */
    "",				/*vlen_end */

    "%s",			/*elmt_fmt */
    ",",			/*elmt_suf1 */
    " ",			/*elmt_suf2 */

    "",				/*idx_n_fmt */
    "",				/*idx_sep */
    "",				/*idx_fmt */

    80,				/*line_ncols *//*standard default columns */
    0,				/*line_per_line */
    "",				/*line_pre */
    "%s",			/*line_1st */
    "%s",			/*line_cont */
    "",				/*line_suf */
    "",				/*line_sep */
    1,				/*line_multi_new */
    "   ",			/*line_indent */

    1,				/*skip_first */

    1,				/*obj_hidefileno */
    " %lu:%lu",			/*obj_format */

    1,				/*dset_hidefileno */
    "DATASET %lu:%lu ",		/*dset_format */
    "%s",			/*dset_blockformat_pre */
    "%s",			/*dset_ptformat_pre */
    "%s",			/*dset_ptformat */
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
static h5dump_t         xml_dataformat = {
    0,				/*raw */

    "",				/*fmt_raw */
    "%d",			/*fmt_int */
    "%u",			/*fmt_uint */
    "%d",			/*fmt_schar */
    "%u",			/*fmt_uchar */
    "%d",			/*fmt_short */
    "%u",			/*fmt_ushort */
    "%ld",			/*fmt_long */
    "%lu",			/*fmt_ulong */
    NULL,			/*fmt_llong */
    NULL,			/*fmt_ullong */
    "%g",			/*fmt_double */
    "%g",			/*fmt_float */

    0,				/*ascii */
    0,				/*str_locale */
    0,				/*str_repeat */

    " ",			/*arr_pre */
    " ",			/*arr_sep */
    " ",			/*arr_suf */
    1,				/*arr_linebreak */

    "",				/*cmpd_name */
    " ",			/*cmpd_sep */
    "",				/*cmpd_pre */
    "",				/*cmpd_suf */
    "",				/*cmpd_end */

    " ",			/*vlen_sep */
    " ",			/*vlen_pre */
    " ",			/*vlen_suf */
    "",				/*vlen_end */

    "%s",			/*elmt_fmt */
    " ",			/*elmt_suf1 */
    "",				/*elmt_suf2 */

    "",				/*idx_n_fmt */
    "",				/*idx_sep */
    "",				/*idx_fmt */

    80,				/*line_ncols *//*standard default columns */
    0,				/*line_per_line */
    "",				/*line_pre */
    "%s",			/*line_1st */
    "%s",			/*line_cont */
    "",				/*line_suf */
    "",				/*line_sep */
    1,				/*line_multi_new */
    "   ",			/*line_indent */

    1,				/*skip_first */

    1,				/*obj_hidefileno */
    " %lu:%lu",			/*obj_format */

    1,				/*dset_hidefileno */
    "DATASET %lu:%lu ",		/*dset_format */
    "%s",			/*dset_blockformat_pre */
    "%s",			/*dset_ptformat_pre */
    "%s",			/*dset_ptformat */
};

/** XML **/

static const dump_header standardformat = {
    "standardformat",		/*name */
    "HDF5",			/*fileebgin */
    "",				/*fileend */
    BOOT_BLOCK,			/*bootblockbegin */
    "",				/*bootblockend */
    GROUPNAME,			/*groupbegin */
    "",				/*groupend */
    DATASET,			/*datasetbegin */
    "",				/*datasetend */
    ATTRIBUTE,			/*attributebegin */
    "",				/*attributeend */
    DATATYPE,			/*datatypebegin */
    "",				/*datatypeend */
    DATASPACE,			/*dataspacebegin */
    "",				/*dataspaceend */
    DATA,			/*databegin */
    "",				/*dataend */
    SOFTLINK,			/*softlinkbegin */
    "",				/*softlinkend */

    "{",			/*fileblockbegin */
    "}",			/*fileblockend */
    "{",			/*bootblockblockbegin */
    "}",			/*bootblockblockend */
    "{",			/*groupblockbegin */
    "}",			/*groupblockend */
    "{",			/*datasetblockbegin */
    "}",			/*datasetblockend */
    "{",			/*attributeblockbegin */
    "}",			/*attributeblockend */
    "",				/*datatypeblockbegin */
    "",				/*datatypeblockend */
    "",				/*dataspaceblockbegin */
    "",				/*dataspaceblockend */
    "{",			/*datablockbegin */
    "}",			/*datablockend */
    "{",			/*softlinkblockbegin */
    "}",			/*softlinkblockend */
    "{",			/*strblockbegin */
    "}",			/*strblockend */
    "{",			/*enumblockbegin */
    "}",			/*enumblockend */
    "{",			/*structblockbegin */
    "}",			/*structblockend */
    "{",			/*vlenblockbegin */
    "}",			/*vlenblockend */

    "",				/*dataspacedescriptionbegin */
    "",				/*dataspacedescriptionend */
    "(",			/*dataspacedimbegin */
    ")",			/*dataspacedimend */
};

/** 
 ** Added for XML **
 **/
/* The 'header' formats for XML -- mostly null
 *
 * XML output has values embedded in the 'headers', so
 * all the XML headers are done on a case by case basis.
 */
static const dump_header xmlformat = {
    "xml",			/*name */
    "",				/*filebegin */
    "</HDF5-File>",		/*fileend */
    "",				/*bootblockbegin */
    "",				/*bootblockend */
    "",				/*groupbegin */
    "</Group>",			/*groupend */
    "",				/*datasetbegin */
    "</Dataset>",		/*datasetend */
    "",				/*attributebegin */
    "</Attribute>",		/*attributeend */
    "<DataType>",		/*datatypeend */
    "</DataType>",		/*datatypeend */
    "<Dataspace>",		/*dataspacebegin */
    "</Dataspace>",		/*dataspaceend */
    "<Data>",			/*databegin */
    "</Data>",			/*dataend */
    "",				/*softlinkbegin */
    "",				/*softlinkend */

    "",				/*fileblockbegin */
    "",				/*fileblockend */
    "",				/*bootblockblockbegin */
    "",				/*bootblockblockend */
    "",				/*groupblockbegin */
    "",				/*groupblockend */
    "",				/*datasetblockbegin */
    "",				/*datasetblockend */
    "",				/*attributeblockbegin */
    "",				/*attributeblockend */
    "",				/*datatypeblockbegin */
    "",				/*datatypeblockend */
    "",				/*dataspaceblockbegin */
    "",				/*dataspaceblockend */
    "",				/*datablockbegin */
    "",				/*datablockend */
    "",				/*softlinkblockbegin */
    "",				/*softlinkblockend */
    "",				/*strblockbegin */
    "",				/*strblockend */
    "",				/*enumblockbegin */
    "",				/*enumblockend */
    "",				/*structblockbegin */
    "",				/*structblockend */
    "",				/*vlenblockbegin */
    "",				/*vlenblockend */

    "",				/*dataspacedescriptionbegin */
    "",				/*dataspacedescriptionend */
    "",				/*dataspacedimbegin */
    "",				/*dataspacedimend */
};

/** XML **/

/** 
 ** Added for XML **
 **/
/* internal functions used by XML option */
static void             xml_print_datatype(hid_t);
static void             xml_print_enum(hid_t);
static int              xml_print_refs(hid_t, int);
static int              xml_print_strs(hid_t, int);
hobj_ref_t             *ref_path_table_put(hid_t, char *);
char                   *xml_escape_the_string(char *, int);
char                   *xml_escape_the_name(char *);

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
static void             dump_dataset(hid_t, const char *);
static void             dump_data(hid_t, int);
static void             dump_named_datatype(hid_t, const char *);
static void             dump_dataspace(hid_t space);
static void             dump_datatype(hid_t type);
static herr_t           dump_attr(hid_t attr, const char *attr_name,
				  void UNUSED * op_data);

/* XML format:   same interface, alternaive output */

static void             xml_dump_group(hid_t, const char *);
static void             xml_dump_dataset(hid_t ds, const char *dsname);
static void             xml_dump_data(hid_t, int);
static void             xml_dump_named_datatype(hid_t, const char *);
static void             xml_dump_dataspace(hid_t space);
static void             xml_dump_datatype(hid_t type);
static herr_t           xml_dump_attr(hid_t attr, const char *attr_name,
				      void UNUSED * op_data);

/** 
 ** Added for XML **
 **
 **  This is the global dispatch table for the dump functions.
 **/
/* the table of dump functions */
typedef struct
{
    void                    (*dump_group_function) (hid_t, const char *);
    void                    (*dump_named_datatype_function) (hid_t,
							     const char *);
    void                    (*dump_dataset_function) (hid_t, const char *);
    void                    (*dump_dataspace_function) (hid_t);
    void                    (*dump_datatype_function) (hid_t);
    herr_t                  (*dump_attribute_function) (hid_t, const char *,
							void *);
    void                    (*dump_data_function) (hid_t, int);
}
dump_functions;

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

/* The global table is set to either ddl_function_table or 
   xml_function_table in the initialization.
 */
static const dump_functions *dump_function_table;


/* a structure for handling the order command-line parameters come in */
struct handler_t {
    void (*func)(hid_t, const char *);
    const char *obj;
};

/*
 * Command-line options: The user can specify short or long-named
 * parameters. The long-named ones can be partially spelled. When
 * adding more, make sure that they don't clash with each other.
 */
static const char *s_opts = "hBHvVa:d:g:l:t:w:xD:o:";
static struct long_options l_opts[] = {
	{ "help", no_arg, 'h' },
	{ "hel", no_arg, 'h' },
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
	{ "he", no_arg, 'H' },
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
	{ "dataset", require_arg, 'd' },
	{ "datase", require_arg, 'd' },
	{ "datas", require_arg, 'd' },
	{ "group", require_arg, 'g' },
	{ "grou", require_arg, 'g' },
	{ "gro", require_arg, 'g' },
	{ "gr", require_arg, 'g' },
	{ "soft-link", require_arg, 'l' },
	{ "soft-lin", require_arg, 'l' },
	{ "soft-li", require_arg, 'l' },
	{ "soft-l", require_arg, 'l' },
	{ "soft", require_arg, 'l' },
	{ "sof", require_arg, 'l' },
	{ "so", require_arg, 'l' },
	{ "datatype", require_arg, 't' },
	{ "datatyp", require_arg, 't' },
	{ "dataty", require_arg, 't' },
	{ "datat", require_arg, 't' },
	{ "width", require_arg, 'w' },
	{ "widt", require_arg, 'w' },
	{ "wid", require_arg, 'w' },
	{ "wi", require_arg, 'w' },
	{ "xml", no_arg, 'x' },
	{ "xm", no_arg, 'x' },
	{ "xml-dtd", require_arg, 'D' },
	{ "xml-dt", require_arg, 'D' },
	{ "xml-d", require_arg, 'D' },
	{ "output", require_arg, 'o' },
	{ "outpu", require_arg, 'o' },
	{ "outp", require_arg, 'o' },
	{ "out", require_arg, 'o' },
	{ "ou", require_arg, 'o' },
	{ NULL, 0, '\0' }
};

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
usage(const char *progname)
{
    fprintf(stderr, "\
usage: %s [OPTIONS] file\n\
   OPTIONS\n\
      -h, --help          Print a usage message and exit\n\
      -B, --bootblock     Print the content of the boot block\n\
      -H, --header        Print the header only; no data is displayed\n\
      -i, --object-ids    Print the object ids\n\
      -V, --version       Print version number and exit\n\
      -a P, --attribute=P Print the specified attribute\n\
      -d P, --dataset=P   Print the specified dataset\n\
      -g P, --group=P     Print the specified group and all members\n\
      -l P, --soft-link=P Print the value(s) of the specified soft link\n\
      -o F, --output=F    Output raw data into file F\n\
      -t T, --datatype=T  Print the specified named data type\n\
      -w #, --width=#     Set the number of columns of output\n\
      -x, --xml         Output XML\n\
      -D URI, --xml-dtd=URL Use the DTD at URI\n\
  P - is the full path from the root group to the object.\n\
  T - is the name of the data type.\n\
  F - is a filename.\n\
  # - is an integer greater than 1.\n\
  URI - is a URI a URI reference (as defined in [IETF RFC 2396],\n\
        updated by [IETF RFC 2732])\n\
  Example:\n\
\n\
     Attribute foo of the group /bar_none in file quux.h5\n\
\n\
     	h5dump -a /bar_none/foo quux.h5\n\
\n", progname);
}

/*-------------------------------------------------------------------------
 * Function:    print_datatype
 *
 * Purpose:     print the data type.
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
print_datatype(hid_t type)
{
    char                   *fname;
    hid_t                   nmembers, mtype, str_type;
    int                     i, j, ndims, perm[H5DUMP_MAX_RANK];
    size_t                  size;
    hsize_t                 dims[H5DUMP_MAX_RANK];
    H5T_str_t               str_pad;
    H5T_cset_t              cset;
    H5G_stat_t              statbuf;
    hid_t                   super;

    switch (H5Tget_class(type)) {
    case H5T_INTEGER:
	if (H5Tequal(type, H5T_STD_I8BE)) {
	    printf("H5T_STD_I8BE");
	} else if (H5Tequal(type, H5T_STD_I8LE)) {
	    printf("H5T_STD_I8LE");
	} else if (H5Tequal(type, H5T_STD_I16BE)) {
	    printf("H5T_STD_I16BE");
	} else if (H5Tequal(type, H5T_STD_I16LE)) {
	    printf("H5T_STD_I16LE");
	} else if (H5Tequal(type, H5T_STD_I32BE)) {
	    printf("H5T_STD_I32BE");
	} else if (H5Tequal(type, H5T_STD_I32LE)) {
	    printf("H5T_STD_I32LE");
	} else if (H5Tequal(type, H5T_STD_I64BE)) {
	    printf("H5T_STD_I64BE");
	} else if (H5Tequal(type, H5T_STD_I64LE)) {
	    printf("H5T_STD_I64LE");
	} else if (H5Tequal(type, H5T_STD_U8BE)) {
	    printf("H5T_STD_U8BE");
	} else if (H5Tequal(type, H5T_STD_U8LE)) {
	    printf("H5T_STD_U8LE");
	} else if (H5Tequal(type, H5T_STD_U16BE)) {
	    printf("H5T_STD_U16BE");
	} else if (H5Tequal(type, H5T_STD_U16LE)) {
	    printf("H5T_STD_U16LE");
	} else if (H5Tequal(type, H5T_STD_U32BE)) {
	    printf("H5T_STD_U32BE");
	} else if (H5Tequal(type, H5T_STD_U32LE)) {
	    printf("H5T_STD_U32LE");
	} else if (H5Tequal(type, H5T_STD_U64BE)) {
	    printf("H5T_STD_U64BE");
	} else if (H5Tequal(type, H5T_STD_U64LE)) {
	    printf("H5T_STD_U64LE");
	} else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
	    printf("H5T_NATIVE_SCHAR");
	} else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
	    printf("H5T_NATIVE_UCHAR");
	} else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
	    printf("H5T_NATIVE_SHORT");
	} else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
	    printf("H5T_NATIVE_USHORT");
	} else if (H5Tequal(type, H5T_NATIVE_INT)) {
	    printf("H5T_NATIVE_INT");
	} else if (H5Tequal(type, H5T_NATIVE_UINT)) {
	    printf("H5T_NATIVE_UINT");
	} else if (H5Tequal(type, H5T_NATIVE_LONG)) {
	    printf("H5T_NATIVE_LONG");
	} else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
	    printf("H5T_NATIVE_ULONG");
	} else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
	    printf("H5T_NATIVE_LLONG");
	} else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
	    printf("H5T_NATIVE_ULLONG");
	} else {
	    printf("undefined integer");
	    d_status = 1;
	}
	break;

    case H5T_FLOAT:
	if (H5Tequal(type, H5T_IEEE_F32BE)) {
	    printf("H5T_IEEE_F32BE");
	} else if (H5Tequal(type, H5T_IEEE_F32LE)) {
	    printf("H5T_IEEE_F32LE");
	} else if (H5Tequal(type, H5T_IEEE_F64BE)) {
	    printf("H5T_IEEE_F64BE");
	} else if (H5Tequal(type, H5T_IEEE_F64LE)) {
	    printf("H5T_IEEE_F64LE");
	} else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
	    printf("H5T_NATIVE_FLOAT");
	} else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
	    printf("H5T_NATIVE_DOUBLE");
	} else if (H5Tequal(type, H5T_NATIVE_LDOUBLE)) {
	    printf("H5T_NATIVE_LDOUBLE");
	} else {
	    printf("undefined float");
	    d_status = 1;
	}
	break;

    case H5T_TIME:
	printf("H5T_TIME: not yet implemented");
	break;

    case H5T_STRING:
	size = H5Tget_size(type);
	str_pad = H5Tget_strpad(type);
	cset = H5Tget_cset(type);

	printf("H5T_STRING %s\n", dump_header_format->strblockbegin);
	indent += COL;

	indentation(indent + COL);
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
	H5Tset_cset(str_type, cset);
	H5Tset_size(str_type, size);
	H5Tset_strpad(str_type, str_pad);

	indentation(indent + COL);
	printf("%s ", CTYPE);

	if (H5Tequal(type, str_type)) {
	    printf("H5T_C_S1;\n");
	    H5Tclose(str_type);
	} else {
	    H5Tclose(str_type);
	    str_type = H5Tcopy(H5T_FORTRAN_S1);
	    H5Tset_cset(str_type, cset);
	    H5Tset_size(str_type, size);
	    H5Tset_strpad(str_type, str_pad);

	    if (H5Tequal(type, str_type)) {
		printf("H5T_FORTRAN_S1;\n");
	    } else {
		printf("unknown_one_character_type;\n ");
		d_status = 1;
	    }

	    H5Tclose(str_type);
	}

	indent -= COL;
	indentation(indent + COL);
	printf("%s", dump_header_format->strblockend);
	break;

    case H5T_BITFIELD:
	if (H5Tequal(type, H5T_STD_B8BE)) {
	    printf("H5T_STD_B8BE");
	} else if (H5Tequal(type, H5T_STD_B8LE)) {
	    printf("H5T_STD_B8LE");
	} else if (H5Tequal(type, H5T_STD_B16BE)) {
	    printf("H5T_STD_B16BE");
	} else if (H5Tequal(type, H5T_STD_B16LE)) {
	    printf("H5T_STD_B16LE");
	} else if (H5Tequal(type, H5T_STD_B32BE)) {
	    printf("H5T_STD_B32BE");
	} else if (H5Tequal(type, H5T_STD_B32LE)) {
	    printf("H5T_STD_B32LE");
	} else if (H5Tequal(type, H5T_STD_B64BE)) {
	    printf("H5T_STD_B64BE");
	} else if (H5Tequal(type, H5T_STD_B64LE)) {
	    printf("H5T_STD_B64LE");
	} else {
	    printf("undefined bitfield");
	    d_status = 1;
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
	if (H5Tcommitted(type) > 0) {
	    H5Gget_objinfo(type, ".", TRUE, &statbuf);
	    i = search_obj(type_table, statbuf.objno);

	    if (i >= 0) {
		if (!type_table->objs[i].recorded)
		    printf("\"/#%lu:%lu\"\n", type_table->objs[i].objno[0],
			   type_table->objs[i].objno[1]);
		else
		    printf("\"%s\"", type_table->objs[i].objname);
	    } else {
		printf("h5dump error: unknown committed type.\n");
		d_status = 1;
	    }
	} else {
	    nmembers = H5Tget_nmembers(type);

	    printf("H5T_COMPOUND %s\n", dump_header_format->structblockbegin);
	    for (i = 0; i < nmembers; i++) {
		fname = H5Tget_member_name(type, i);
		mtype = H5Tget_member_type(type, i);

		indentation(indent + COL);

		if (H5Tget_class(mtype) == H5T_COMPOUND) {
		    indent += COL;
		}

		print_datatype(mtype);

		if (H5Tget_class(mtype) == H5T_COMPOUND) {
		    indent -= COL;
		}

		printf(" \"%s\";\n", fname);

		free(fname);
	    }
	    indentation(indent);
	    printf("%s", dump_header_format->structblockend);
	}
	break;

    case H5T_REFERENCE:
	printf("H5T_REFERENCE");
	break;

    case H5T_ENUM:
	printf("H5T_ENUM %s\n", dump_header_format->enumblockbegin);
	indent += COL;
	indentation(indent + COL);
	super = H5Tget_super(type);
	print_datatype(super);
	printf(";\n");
	print_enum(type);
	indent -= COL;
	indentation(indent + COL);
	printf("%s", dump_header_format->enumblockend);
	break;

    case H5T_VLEN:
	printf("H5T_VLEN %s ", dump_header_format->vlenblockbegin);

	super = H5Tget_super(type);
	print_datatype(super);
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
	H5Tget_array_dims(type, dims, perm);

	/* Print array dimensions */
	for (j = 0; j < ndims; j++)
	    printf("[%d]", (int) dims[j]);

	printf(" ");

	/* Print base type */
	print_datatype(super);

	/* Close array base type */
	H5Tclose(super);

	/* Print closing */
	printf(" }");

	break;

    default:
	printf("unknown data type");
	d_status = 1;
	break;
    }
}

/*-------------------------------------------------------------------------
 * Function:    dump_bb
 *
 * Purpose:     Dump the boot block
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_bb(void)
{
    printf("%s %s boot block not yet implemented %s\n",
	   BOOT_BLOCK, BEGIN, END);
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
 *-----------------------------------------------------------------------*/
static void
dump_datatype(hid_t type)
{
    indent += COL;

    indentation(indent);
    printf("%s %s ", dump_header_format->datatypebegin,
	   dump_header_format->datatypeblockbegin);

    print_datatype(type);

    if (H5Tget_class(type) == H5T_COMPOUND
	|| H5Tget_class(type) == H5T_STRING) indentation(indent);
    printf(" %s %s\n", dump_header_format->datatypeblockend,
	   dump_header_format->datatypeend);

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
 *-----------------------------------------------------------------------*/
static void
dump_dataspace(hid_t space)
{
    hsize_t                 size[H5DUMP_MAX_RANK];
    hsize_t                 maxsize[H5DUMP_MAX_RANK];
    int                     ndims =
	H5Sget_simple_extent_dims(space, size, maxsize);
    int                     i;

    indentation(indent + COL);
    printf("%s ", dump_header_format->dataspacebegin);

    if (H5Sis_simple(space)) {
	if (ndims == 0) {
	    /* scalar dataspace */
	    HDfprintf(stdout, "%s %s ",
		      dump_header_format->dataspacedescriptionbegin, SCALAR);
	} else {
	    /* simple dataspace */
	    HDfprintf(stdout, "%s %s { %s %Hu",
		      dump_header_format->dataspacedescriptionbegin, SIMPLE,
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
	}
    } else {
	printf("%s not yet implemented %s\n", BEGIN, END);
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
 *-----------------------------------------------------------------------*/
static herr_t
dump_attr(hid_t attr, const char *attr_name, void UNUSED * op_data)
{
    hid_t                   attr_id, type, space;
    herr_t                  ret = SUCCEED;

    indentation(indent);
    begin_obj(dump_header_format->attributebegin, attr_name,
	      dump_header_format->attributeblockbegin);

    if ((attr_id = H5Aopen_name(attr, attr_name)) >= 0) {
	type = H5Aget_type(attr_id);
	space = H5Aget_space(attr_id);
	dump_datatype(type);
	dump_dataspace(space);

	if (display_oid)
	    dump_oid(attr_id);

	if (display_data)
	    dump_data(attr_id, ATTRIBUTE_DATA);

	H5Tclose(type);
	H5Sclose(space);
	H5Aclose(attr_id);
	indentation(indent);
	end_obj(dump_header_format->attributeend,
		dump_header_format->attributeblockend);
    } else {
	indentation(indent + COL);
	printf("h5dump error: unable to open attribute.\n");
	indentation(indent);
	end_obj(dump_header_format->attributeend,
		dump_header_format->attributeblockend);
	d_status = 1;
	ret = FAIL;
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
 *-----------------------------------------------------------------------*/
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
        strncpy(obj_name, name, (size_t)j+1);
        obj_name[j+1] = '\0';
    }

    attr_name = name + j + 1;
    begin_obj(dump_header_format->attributebegin, name,
	      dump_header_format->attributeblockbegin);
    H5Gget_objinfo(loc_id, obj_name, FALSE, &statbuf);

    switch (statbuf.type) {
    case H5G_GROUP:
	if ((oid = H5Gopen(loc_id, obj_name)) < 0) {
	    indentation(COL);
	    fprintf(stdout, "h5dump error: unable to open %s\n", obj_name);
	    end_obj(dump_header_format->attributeend,
		    dump_header_format->attributeblockend);
	    d_status = 1;
	    return FAIL;
	}
	break;

    case H5G_DATASET:
	if ((oid = H5Dopen(loc_id, obj_name)) < 0) {
	    indentation(COL);
	    fprintf(stdout, "h5dump error: unable to open %s\n", obj_name);
	    end_obj(dump_header_format->attributeend,
		    dump_header_format->attributeblockend);
	    d_status = 1;
	    return FAIL;
	}
	break;

    case H5G_TYPE:
	if ((oid = H5Topen(loc_id, obj_name)) < 0) {
	    indentation(COL);
	    fprintf(stdout, "h5dump error: unable to open %s\n", obj_name);
	    end_obj(dump_header_format->attributeend,
		    dump_header_format->attributeblockend);
	    d_status = 1;
	    return FAIL;
	}
	break;

    default:
	indentation(COL);
	fprintf(stdout, "h5dump error: unable to open %s\n", obj_name);
	end_obj(dump_header_format->attributeend,
		dump_header_format->attributeblockend);
	d_status = 1;
	return FAIL;
    }

    if ((attr_id = H5Aopen_name(oid, attr_name)) >= 0) {
	type = H5Aget_type(attr_id);
	space = H5Aget_space(attr_id);
	dump_datatype(type);
	dump_dataspace(space);

	if (display_oid)
	    dump_oid(attr_id);

	if (display_data)
	    dump_data(attr_id, ATTRIBUTE_DATA);

	H5Tclose(type);
	H5Sclose(space);
	H5Aclose(attr_id);
	end_obj(dump_header_format->attributeend,
		dump_header_format->attributeblockend);
    } else {
	indentation(COL);
	printf("h5dump error: unable to open attribute.\n");
	end_obj(dump_header_format->attributeend,
		dump_header_format->attributeblockend);
	d_status = 1;
    }

    switch (statbuf.type) {
    case H5G_GROUP:
	if (H5Gclose(oid) < 0) {
	    d_status = 1;
	    return FAIL;
	}
	break;

    case H5G_DATASET:
	if (H5Dclose(oid) < 0) {
	    d_status = 1;
	    return FAIL;
	}
	break;

    case H5G_TYPE:
	if (H5Tclose(oid) < 0) {
	    d_status = 1;
	    return FAIL;
	}
	break;
    default:
	d_status = 1;
	return FAIL;
    }

    free(obj_name);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    dump all
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
 *   11/00  Added XML support.  Also, optionally checks the op_data
 *          argument.
 *
 *-----------------------------------------------------------------------*/
static herr_t
dump_all(hid_t group, const char *name, void * op_data)
{
    hid_t                   obj;
    char                   *buf, *tmp, *tmp2;
    H5G_stat_t              statbuf;
    int                     i;

    H5Gget_objinfo(group, name, FALSE, &statbuf);
    if ((*(int *)op_data != H5G_UNKNOWN) &&
    (statbuf.type != *(int *) op_data))
	return SUCCEED;
    tmp = (char *) malloc(strlen(prefix) + strlen(name) + 2);
    strcpy(tmp, prefix);

    switch (statbuf.type) {
    case H5G_LINK:
	indentation(indent);
	buf = malloc(statbuf.linklen);

	if (!doxml) {
	    begin_obj(dump_header_format->softlinkbegin, name,
		      dump_header_format->softlinkblockbegin);
	    indentation(indent + COL);
	}

	if (H5Gget_linkval(group, name, statbuf.linklen, buf) >= 0) {
	    /* print the value of a soft link */
	    if (!doxml) {
		/* Standard DDL:  no modification */
		printf("LINKTARGET \"%s\"\n", buf);
	    } else {
		/* XML */
                tmp2 = (char *) malloc(strlen(prefix) + statbuf.linklen + 2);
                strcpy(tmp2, prefix);
		if (buf && buf[0] == '/') {
			strcat(tmp2, buf);
		} else {
			strcat(strcat(tmp2, "/"), buf);
		}
		strcat(strcat(tmp, "/"), name);
		printf
		    ("<SoftLink LinkName=\"%s\" Target=\"%s\" TargetObj=\"%s\" OBJ-XID=\"%s\" Source=\"%s\"/>\n",
		     xml_escape_the_name(name), 
                     xml_escape_the_name(buf),
		     xml_escape_the_name(tmp2), 
                     xml_escape_the_name(tmp),
		     (strcmp(prefix,
			     "") ? xml_escape_the_name((char *)prefix) : "root"));
               free(tmp2);
	    }
	} else {
	    printf("h5dump error: unable to get link value.\n");
	    d_status = 1;
	}

	if (!doxml) {
	    indentation(indent);
	    end_obj(dump_header_format->softlinkend,
		    dump_header_format->softlinkblockend);
	}

	free(buf);
	break;

    case H5G_GROUP:
	if ((obj = H5Gopen(group, name)) >= 0) {
	    strcat(strcat(prefix, "/"), name);
	    dump_function_table->dump_group_function(obj, name);
	    strcpy(prefix, tmp);
	    H5Gclose(obj);
	} else {
	    printf("h5dump error: unable to dump group %s\n", name);
	    d_status = 1;
	}

	break;

    case H5G_DATASET:
	if ((obj = H5Dopen(group, name)) >= 0) {
	    /* hard link */
	    H5Gget_objinfo(obj, ".", TRUE, &statbuf);

	    if (statbuf.nlink > 1) {
		i = search_obj(dset_table, statbuf.objno);

		if (i < 0) {
		    indentation(indent);
		    begin_obj(dump_header_format->datasetbegin, name,
			      dump_header_format->datasetblockbegin);
		    indentation(indent + COL);
		    printf("h5dump error: internal error\n");
		    indentation(indent);
		    end_obj(dump_header_format->datasetend,
			    dump_header_format->datasetblockend);
		    d_status = 1;
		    H5Dclose(obj);
		    goto done;
		} else if (dset_table->objs[i].displayed) {
		    indentation(indent);
		    if (!doxml) {
			begin_obj(dump_header_format->datasetbegin, name,
				  dump_header_format->datasetblockbegin);
			indentation(indent + COL);
			printf("%s \"%s\"\n", HARDLINK,
			       dset_table->objs[i].objname);
			indentation(indent);
			end_obj(dump_header_format->datasetend,
				dump_header_format->datasetblockend);
		    } else {
			/* the XML version */
			strcat(strcat(tmp, "/"), name);
			printf
			    ("<Dataset Name=\"%s\" OBJ-XID=\"%s\" Parents=\"%s\">\n",
			     xml_escape_the_name((char *)name),
			     xml_escape_the_name((char *)tmp),
			     (strcmp(prefix, "") ? xml_escape_the_name((char *)prefix)
			      : "root"));

			indentation(indent + COL);
			printf("<DatasetPtr OBJ-XID=\"%s\"/>\n",
			       xml_escape_the_name(dset_table->
						   objs[i].objname));
			indentation(indent);
			printf("%s\n", dump_header_format->datasetend);
		    }
		    H5Dclose(obj);
		    goto done;
		} else {
		    dset_table->objs[i].displayed = 1;
		    strcat(tmp, "/");
		    strcat(tmp, name);
		    strcpy(dset_table->objs[i].objname, tmp);
		}
	    }

	    dump_function_table->dump_dataset_function(obj, name);
	    H5Dclose(obj);
	} else {
	    printf("h5dump error: unable to dump dataset %s\n", name);
	    d_status = 1;
	}
	break;

    case H5G_TYPE:
	if ((obj = H5Topen(group, name)) >= 0) {
	    dump_function_table->dump_named_datatype_function(obj, name);
	    H5Tclose(obj);
	} else {
	    printf("h5dump error: unable to dump data type %s\n", name);
	    d_status = 1;
	}

	break;

    default:
	printf("h5dump error: unknown object %s\n", name);
	d_status = 1;
	return FAIL;
    }

  done:
    free(tmp);
    return SUCCEED;
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
 * Modifications: Comments: not yet implemented.
 *
 *-----------------------------------------------------------------------*/
static void
dump_named_datatype(hid_t type, const char *name)
{
    indentation(indent);
    printf("%s \"%s\" %s", dump_header_format->datatypebegin, name,
	   dump_header_format->datatypeblockbegin);

    if (H5Tget_class(type) == H5T_COMPOUND) {
	hid_t                   temp_type = H5Tcopy(type);

	print_datatype(temp_type);
	H5Tclose(temp_type);
    } else {
	indentation(indent + COL);
	print_datatype(type);
	printf(";\n");
    }

    indentation(indent);
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
 *
 *-----------------------------------------------------------------------*/
static void
dump_group(hid_t gid, const char *name)
{
    H5G_stat_t              statbuf;
    hid_t                   dset, type;
    char                    typename[1024], *tmp;
    int                     i;
    int                     xtype = H5G_UNKNOWN; /* dump all */

    tmp = malloc(strlen(prefix) + strlen(name) + 2);
    strcpy(tmp, prefix);
    indentation(indent);
    begin_obj(dump_header_format->groupbegin, name,
	      dump_header_format->groupblockbegin);
    indent += COL;

    if (display_oid)
	dump_oid(gid);

    if (!strcmp(name, "/") && unamedtype)
	/* dump unamed type in root group */
	for (i = 0; i < type_table->nobjs; i++)
	    if (!type_table->objs[i].recorded) {
		dset = H5Dopen(gid, type_table->objs[i].objname);
		type = H5Dget_type(dset);
		sprintf(typename, "#%lu:%lu",
			type_table->objs[i].objno[0],
			type_table->objs[i].objno[1]);
		dump_named_datatype(type, typename);
		H5Tclose(type);
		H5Dclose(dset);
	    }

    H5Gget_objinfo(gid, ".", TRUE, &statbuf);

    if (statbuf.nlink > 1) { 
        i = search_obj(group_table, statbuf.objno);

	if (i < 0) {
	    indentation(indent);
	    printf("h5dump error: internal error\n");
	    d_status = 1;
	} else if (group_table->objs[i].displayed) {
	    indentation(indent);
	    printf("%s \"%s\"\n", HARDLINK, group_table->objs[i].objname);
	} else {
	    strcpy(group_table->objs[i].objname, prefix);
	    group_table->objs[i].displayed = 1;
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
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_dataset(hid_t did, const char *name)
{
    hid_t                   type, space;

    indentation(indent);
    begin_obj(dump_header_format->datasetbegin, name,
	      dump_header_format->datasetblockbegin);
    type = H5Dget_type(did);
    space = H5Dget_space(did);
    dump_datatype(type);
    dump_dataspace(space);

    if (display_oid)
	dump_oid(did);

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
	    dump_data(did, DATASET_DATA);
	    break;

	default:
	    break;
	}

    indent += COL;
    H5Aiterate(did, NULL, dump_attr, NULL);
    indent -= COL;
    H5Tclose(type);
    H5Sclose(space);
    indentation(indent);
    end_obj(dump_header_format->datasetend,
	    dump_header_format->datasetblockend);
}

#if H5DUMP_DEBUG
/*-------------------------------------------------------------------------
 * Function:    dump_tables
 *
 * Purpose:     display the contents of tables for debugging purposes
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_tables(void)
{
    int                     i;

    printf("group_table: # of entries = %d\n", group_table->nobjs);

    for (i = 0; i < group_table->nobjs; i++)
	printf("%lu %lu %s %d %d\n", group_table->objs[i].objno[0],
	       group_table->objs[i].objno[1],
	       group_table->objs[i].objname,
	       group_table->objs[i].displayed, group_table->objs[i].recorded);

    printf("\ndset_table: # of entries = %d\n", dset_table->nobjs);

    for (i = 0; i < dset_table->nobjs; i++)
	printf("%lu %lu %s %d %d\n", dset_table->objs[i].objno[0],
	       dset_table->objs[i].objno[1],
	       dset_table->objs[i].objname,
	       dset_table->objs[i].displayed, dset_table->objs[i].recorded);

    printf("\ntype_table: # of entries = %d\n", type_table->nobjs);

    for (i = 0; i < type_table->nobjs; i++)
	printf("%lu %lu %s %d %d\n", type_table->objs[i].objno[0],
	       type_table->objs[i].objno[1],
	       type_table->objs[i].objname,
	       type_table->objs[i].displayed, type_table->objs[i].recorded);
}
#endif /* H5DUMP_DEBUG */

/*-------------------------------------------------------------------------
 * Function:    dump_data
 *
 * Purpose:     Dump attribute or dataset data
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------
 */
static void
dump_data(hid_t obj_id, int obj_data)
{
    h5dump_t               *outputformat = &dataformat;
    int                     d_status = -1;
    void                   *buf;
    hid_t                   space, type, p_type;
    int                     ndims, i;
    hsize_t                 size[64], nelmts = 1, alloc_size;
    int                     depth;
    int                     stdindent = COL;	/* should be 3 */

    outputformat->line_ncols = nCols;
    indent += COL;

    /*
     * the depth will tell us how far we need to indent extra.  we use to just
     * use indent but with the merging of the tools lib we have to do
     * something different for the lib funtions... the normal indentation is 6
     * so when we don't need any extra indentation, depth will be 0.
     */
    depth = indent / stdindent + 1;
    indentation(indent);
    begin_obj(dump_header_format->databegin, (const char *) NULL,
	      dump_header_format->datablockbegin);

    /* Print all the values. */
    if (obj_data == DATASET_DATA) {
	d_status = h5dump_dset(stdout, outputformat, obj_id, -1, depth);
    } else {
        /* need to call h5dump_mem for the attribute data */    
        type = H5Aget_type(obj_id);
        p_type = h5dump_fixtype(type);
        space = H5Aget_space(obj_id);
        ndims = H5Sget_simple_extent_dims(space, size, NULL);

        for (i = 0; i < ndims; i++)
            nelmts *= size[i];

        alloc_size= nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type));
        assert(alloc_size==(hsize_t)((size_t)alloc_size)); /*check for overflow*/
        buf = malloc((size_t)alloc_size);
        assert(buf);

        if (H5Aread(obj_id, p_type, buf) >= 0)
            d_status = h5dump_mem(stdout, outputformat, obj_id, p_type, space, buf, depth);

        free(buf);
        H5Tclose(p_type); 
        H5Sclose(space);
        H5Tclose(type);
    }
    if (d_status < 0) {
	indentation(indent + COL);
	printf("Unable to print data.\n");
	d_status = 1;
    }

    indentation(indent);
    end_obj(dump_header_format->dataend, dump_header_format->datablockend);
    indent -= COL;
}

/*-------------------------------------------------------------------------
 * Function:    set_output_file
 *
 * Purpose:     Open fname as the output file for dataset raw data.
 *		Set rawdatastream as its file stream.
 *
 * Return:      0 -- succeeded
 *		negative -- failed
 *
 * Programmer:  Albert Cheng, 2000/09/30
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
set_output_file(const char *fname)
{
    FILE                   *f;	/* temporary holding place for the stream pointer */
    /* so that rawdatastream is changed only when succeeded */

    if ((f = fopen(fname, "w")) != NULL) {
	rawdatastream = f;
	return 0;
    }

    return -1;
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
handle_attributes(hid_t fid, const char *attr)
{
    dump_selected_attr(fid, attr);
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
 *
 *-------------------------------------------------------------------------
 */
static void
handle_datasets(hid_t fid, const char *dset)
{
    H5G_stat_t statbuf;
    hid_t dsetid;

    if ((dsetid = H5Dopen(fid, dset)) < 0) {
        begin_obj(dump_header_format->datasetbegin, dset,
                  dump_header_format->datasetblockbegin); 
        indentation(COL);
        fprintf(stdout, "h5dump error: unable to open %s\n", dset);
        end_obj(dump_header_format->datasetend,
                dump_header_format->datasetblockend);
        d_status = 1;
    } else {
        H5Gget_objinfo(dsetid, ".", TRUE, &statbuf);

        if (statbuf.nlink > 1) {
            int index = search_obj(dset_table, statbuf.objno);

            if (index >= 0) {
                if (dset_table->objs[index].displayed) {
                    begin_obj(dump_header_format->datasetbegin, dset,
                              dump_header_format->datasetblockbegin);
                    indentation(indent + COL);
                    printf("%s \"%s\"\n", HARDLINK,
                           dset_table->objs[index].objname);
                    indentation(indent);
                    end_obj(dump_header_format->datasetend,
                            dump_header_format->datasetblockend);
                } else {
                    strcpy(dset_table->objs[index].objname, dset);
                    dset_table->objs[index].displayed = 1;
                    dump_dataset(dsetid, dset);
                }
            } else {
                d_status = 1;
            }
        } else {
            dump_dataset(dsetid, dset);
        }

        if (H5Dclose(dsetid) < 1)
            d_status = 1;
    }
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
handle_groups(hid_t fid, const char *group)
{
    H5G_stat_t statbuf;
    hid_t gid;

    if ((gid = H5Gopen(fid, group)) < 0) {
        begin_obj(dump_header_format->groupbegin, group,
                  dump_header_format->groupblockbegin); 
        indentation(COL);
        fprintf(stdout, "h5dump error: unable to open %s\n", group);
        end_obj(dump_header_format->groupend,
                dump_header_format->groupblockend);
        d_status = 1;
    } else {
        H5Gget_objinfo(gid, ".", TRUE, &statbuf);
        strcpy(prefix, group);
        dump_group(gid, group);

        if (H5Gclose(gid) < 0)
            d_status = 1;
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
handle_links(hid_t fid, const char *link)
{
    H5G_stat_t statbuf;

    if (H5Gget_objinfo(fid, link, FALSE, &statbuf) < 0) {
        begin_obj(dump_header_format->softlinkbegin, link,
                  dump_header_format->softlinkblockbegin);
        indentation(COL);
        fprintf(stdout, "h5dump error: unable to get obj info from %s\n", link);
        end_obj(dump_header_format->softlinkend,
                dump_header_format->softlinkblockend);
        d_status = 1;
    } else if (statbuf.type == H5G_LINK) {
        char *buf = malloc(statbuf.linklen*sizeof(char));

        begin_obj(dump_header_format->softlinkbegin, link,
                  dump_header_format->softlinkblockbegin);
        indentation(COL);

        if (H5Gget_linkval(fid, link, statbuf.linklen, buf) >= 0) {
            printf("LINKTARGET \"%s\"\n", buf);
        } else {
            fprintf(stdout, "h5dump error: unable to get link value\n");
            d_status = 1;
        }

        end_obj(dump_header_format->softlinkend,
                dump_header_format->softlinkblockend);
        free(buf);
    } else {
        begin_obj(dump_header_format->softlinkbegin, link,
                  dump_header_format->softlinkblockbegin);
        indentation(COL);
        fprintf(stdout, "h5dump error: %s is not a link\n", link);
        end_obj(dump_header_format->softlinkend,
                dump_header_format->softlinkblockend);
        d_status = 1;
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
handle_datatypes(hid_t fid, const char *type)
{
    hid_t typeid;

    if ((typeid = H5Topen(fid, type)) < 0) {
        /* check if type is unamed data type */
        int index = 0;

        while (index < type_table->nobjs ) {
            char name[128], name1[128];

            if (!type_table->objs[index].recorded) {
                /* unamed data type */
                sprintf(name, "#%lu:%lu\n",
                        type_table->objs[index].objno[0], 
                        type_table->objs[index].objno[1]);
                sprintf(name1, "/#%lu:%lu\n",
                        type_table->objs[index].objno[0], 
                        type_table->objs[index].objno[1]);

            if (!strncmp(name, type, strlen(type)) || 
                !strncmp(name1, type, strlen(type)))
                break;
            } 

            index++;
        }

        if (index ==  type_table->nobjs) {
            /* unknown type */
            begin_obj(dump_header_format->datatypebegin, type,
                      dump_header_format->datatypeblockbegin); 
            indentation(COL);
            fprintf(stdout, "h5dump error: unable to open %s\n", type);
            end_obj(dump_header_format->datatypeend,
                    dump_header_format->datatypeblockend);
            d_status = 1;
        } else {
            hid_t dsetid = H5Dopen(fid, type_table->objs[index].objname);
            typeid = H5Dget_type(dsetid);
            dump_named_datatype(typeid, type);
            H5Tclose(typeid);
            H5Dclose(dsetid);
        }
    } else {
        dump_named_datatype(typeid, type);

        if (H5Tclose(typeid) < 0)
            d_status = 1;
    }
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
 *        Albert Cheng, 2000/09/30
 *        Add the -o option--output file for datasets raw data
 *        Changes to support XML Nov. 2000, REMcG
 *
 *        Bill Wendling
 *        Wednesday, 10. January 2001
 *        Modified the way command line parameters are interpreted. They go
 *        through one function call now (get_option). Also, removed the
 *        `strcpy(prefix, "")' code which is bad since prefix isn't
 *        initialized at this point...
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, const char *argv[])
{
    hid_t               fid, gid;
    const char         *progname = "h5dump";
    const char         *fname = NULL;
    int                 i, display_bb = 0, display_all = 1, newwidth = 0;
    void               *edata;
    hid_t               (*func)(void*);
    find_objs_t         info;
    int                 opt;
    struct handler_t   *hand;
    int                 usingdasho = 0;;

    if (argc < 2) {
        usage(progname);
        exit(EXIT_FAILURE);
    }

    dump_header_format = &standardformat;
    dump_function_table = &ddl_function_table;

    /* Disable error reporting */
    H5Eget_auto(&func, &edata);
    H5Eset_auto(NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();

    /* this will be plenty big enough for holding the info */
    hand = calloc((size_t)argc, sizeof(struct handler_t));

    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
        case 'B':
            display_bb = TRUE;
            break;
        case 'H':
            display_data = FALSE;
            break;
        case 'v':
            display_oid = TRUE;
            break;
        case 'V':
            print_version(progname);
            exit(EXIT_SUCCESS);
            break;
        case 'w':
            nCols = atoi(opt_arg);
            break;
        case 'a':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_attributes;
                    hand[i].obj = opt_arg;
                    break;
                }

            break;
        case 'd':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_datasets;
                    hand[i].obj = opt_arg;
                    break;
                }

            break;
        case 'g':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_groups;
                    hand[i].obj = opt_arg;
                    break;
                }

            break;
        case 'l':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_links;
                    hand[i].obj = opt_arg;
                    break;
                }

            break;
        case 't':
            display_all = 0;

            for (i = 0; i < argc; i++)
                if (!hand[i].func) {
                    hand[i].func = handle_datatypes;
                    hand[i].obj = opt_arg;
                    break;
                }

            break;
        case 'o':
            if (set_output_file(opt_arg) < 0){
                /* failed to set output file */
                usage(progname);
                exit(EXIT_FAILURE);
            }
            usingdasho = 1;
            break;
        case 'x':
            /* select XML output */
            doxml = TRUE;
            dump_header_format = &xmlformat;
            dump_function_table = &xml_function_table;
            break;
        case 'D':
            /* specify alternative XML DTD */
            xml_dtd_uri = strdup(opt_arg);
            break;
        case 'h':
            usage(progname);
            exit(EXIT_SUCCESS);
        case '?':
        default:
            usage(progname);
            exit(EXIT_FAILURE);
        }
    }

    /*  check for conflicting options */
    if (doxml) {
	if (!display_all) {
	    fprintf(stderr, "h5dump error: option %s not available for XML\n",
		    "to display selected objects");
	    exit(1);
	} else if (display_bb) {
	    fprintf(stderr, "h5dump error: option %s not available for XML\n",
		    "-bb");
	    exit(1);
	} else if (!display_data) {
	    fprintf(stderr, "h5dump error: option %s not available for XML\n",
		    "-header");
	    exit(1);
	} else if (display_oid == 1) {
	    fprintf(stderr, "h5dump error: option %s not available for XML\n",
		    "-v");
	    exit(1);
	} else if (usingdasho) {
	    fprintf(stderr, "h5dump error: option %s not available for XML\n",
		    "-o");
	    exit(1);
	}
    } else {
	if (xml_dtd_uri != NULL) {
	    fprintf(stderr,
		    "h5dump warning: option %s only applies with XML: %s\n",
		    "-dtd ", xml_dtd_uri);
	}
    }
    if (argv[argc - 1][0] == '\\')
	fname = &argv[argc - 1][1];
    else
	fname = argv[argc - 1];

    fid = h5dump_fopen(fname, NULL, 0);

    if (fid < 0) {
        fprintf(stderr, "h5dump error: unable to open file %s\n", fname);
        exit(EXIT_FAILURE);
    }

    /* allocate and initialize internal data structure */
    init_table(&group_table);
    init_table(&type_table);
    init_table(&dset_table);
    init_prefix(&prefix, prefix_len);

    /* init the find_objs_t */
    info.threshold = 0;
    info.prefix_len = prefix_len;
    info.prefix = calloc((size_t)info.prefix_len, 1);
    info.group_table = group_table;
    info.type_table = type_table;
    info.dset_table = dset_table;
    info.status = d_status;

    if (doxml) {
	/* initialize XML */
	thefile = fid;

	/* find all objects that might be targets of a refernce */
	if ((gid = H5Gopen(fid, "/")) >= 0) {
	    ref_path_table_put(gid, (char *)"/");
	    H5Giterate(fid, "/", NULL, fill_ref_path_table, NULL);
	}
	H5Gclose(gid);
	/* reset prefix! */
/* not needed yet
        info.prefix = calloc((size_t)info.prefix_len, 1);
*/
	strcpy(prefix,"");

	/* make sure the URI is initialized to something */
	if (xml_dtd_uri == NULL)
	    xml_dtd_uri = strdup(DEFAULT_DTD);
    }

    /* find all shared objects */
    H5Giterate(fid, "/", NULL, find_objs, (void *)&info);

    /* does there exist unamed committed data type */
    for (i = 0; i < type_table->nobjs; i++)
        if (type_table->objs[i].recorded == 0)
            unamedtype = 1;

#ifdef H5DUMP_DEBUG
    dump_tables();
#endif

    if (info.status) {
        printf("internal error! \n");
        goto done;
    }

    /* start to dump */
    if (!doxml) {
	begin_obj(dump_header_format->filebegin, fname,
		  dump_header_format->fileblockbegin);
    } else {
	printf("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
	printf("<!DOCTYPE HDF5-File PUBLIC \"HDF5-File.dtd\" \"%s\">\n",
	       xml_dtd_uri);
	printf("<HDF5-File>\n");
    }

    if (!doxml && display_bb)
	dump_bb();

    if (newwidth)
	sscanf(argv[newwidth + 1], "%d", &nCols);

    if (display_all) {
        if ((gid = H5Gopen (fid, "/")) < 0 ) {
            fprintf(stdout, "h5dump error: unable to open root group\n");
            d_status = 1;
        } else {
	    dump_function_table->dump_group_function(gid, "/");
        }

        if (H5Gclose (gid) < 0) {
            fprintf(stdout, "h5dump error: unable to close root group\n");
            d_status = 1;
        }
    } else {
	if (doxml) {
	    /*  Note:  this option is not supported for XML  */
	    printf("internal error! \n");
	    goto done;
	}
        for (i = 0; i < argc; i++)
            if (hand[i].func)
                hand[i].func(fid, hand[i].obj);
    }

    if (!doxml) {
	end_obj(dump_header_format->fileend,
		dump_header_format->fileblockend);
    } else {
	printf("%s\n", dump_header_format->fileend);
    }

  done:
    if (H5Fclose(fid) < 0)
	d_status = 1;


    free(hand);

    free(group_table->objs);
    free(dset_table->objs);
    free(type_table->objs);
    free(prefix);
    free(info.prefix);
    /* To Do:  clean up XML table */

    h5tools_close();
    H5Eset_auto(func, edata);
    return d_status;
}

/*-------------------------------------------------------------------------
 * Function:    print_enum
 *
 * Purpose:     prints the enum data - 
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
    char                  **name = NULL;	/*member names                   */
    unsigned char          *value = NULL;	/*value array                    */
    int                     nmembs;	/*number of members              */
    int                     nchars;	/*number of output characters    */
    hid_t                   super;	/*enum base integer type         */
    hid_t                   native = -1;	/*native integer data type       */
    size_t                  dst_size;	/*destination value type size    */
    int                     i;	/*miscellaneous counters         */
    size_t                  j;

    nmembs = H5Tget_nmembers(type);
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
    assert(nmembs>0);
    name = calloc((size_t)nmembs, sizeof(char *));
    value = calloc((size_t)nmembs, MAX(H5Tget_size(type), dst_size));

    for (i = 0; i < nmembs; i++) {
	name[i] = H5Tget_member_name(type, i);
	H5Tget_member_value(type, i, value + i * H5Tget_size(type));
    }

    /* Convert values to native data type */
    if (native > 0)
        H5Tconvert(super, native, (hsize_t)nmembs, value, NULL, H5P_DEFAULT);

    /* Sort members by increasing value */
    /*not implemented yet */

    /* Print members */
    for (i = 0; i < nmembs; i++) {
	indentation(indent + COL);
	nchars = printf("\"%s\"", name[i]);	/*display_string(stdout, name[i], TRUE); */
	printf("%*s   ", MAX(0, 16 - nchars), "");

	if (native < 0) {
	    printf("0x");

	    for (j = 0; j < dst_size; j++)
		printf("%02x", value[i * dst_size + j]);
	} else if (H5T_SGN_NONE == H5Tget_sign(native)) {
	    printf("%" PRINTF_LL_WIDTH "u", *((unsigned long_long *)
					      ((void *) (value + i * dst_size))));
	} else {
	    printf("%" PRINTF_LL_WIDTH "d",
		   *((long_long *) ((void *) (value + i * dst_size))));
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
 *-----------------------------------------------------------------------*/
static void
dump_oid(hid_t oid)
{
    indentation(indent + COL);
    printf("%s %s ", OBJID, BEGIN);
    printf("%d", oid);
    printf(" %s\n", END);
}

/*
 *   XML support
 */

/*
 *  XML needs a table to look up a path name for an object
 *  reference.
 *
 *  This table stores mappings of reference -> path
 *  for all objects in the file that may be the target of
 *  an object reference.
 *
 *  The 'path' is an absolute path by which the object
 *  can be accessed.  When an object has > 1 such path,
 *  only one will be used in the table, with no particular
 *  method of selecting which one.
 */

struct ref_path_table_entry_t
{
    hsize_t                 obj;
    hobj_ref_t             *obj_ref;
    char                   *apath;
    struct ref_path_table_entry_t *next;
};

struct ref_path_table_entry_t *ref_path_table = NULL;	/* the table */
int                     npte = 0;	/* number of entries in the table */

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_lookup
 *
 * Purpose:     Looks up a table entry given a path name.
 *              Used during construction of the table.
 *
 * Return:      The table entre (pte) or NULL if not in the
 *              table.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
struct ref_path_table_entry_t *
ref_path_table_lookup(char *thepath)
{
    int                     i;
    hobj_ref_t             *ref;
    herr_t                  status;
    struct ref_path_table_entry_t *pte = ref_path_table;
    if (ref_path_table == NULL)
	return NULL;
    ref = (hobj_ref_t *) malloc(sizeof(hobj_ref_t));
    if (ref == NULL) {
	/*  fatal error ? */
	return NULL;
    }
    status = H5Rcreate(ref, thefile, thepath, H5R_OBJECT, -1);
    if (status < 0) {
	/*  fatal error ? */
	return NULL;
    }
    for (i = 0; i < npte; i++) {
	if (memcmp(ref, pte->obj_ref, sizeof(hobj_ref_t)) == 0) {
	    return pte;
	}
	pte = pte->next;
    }
    return NULL;
}

/*-------------------------------------------------------------------------
 * Function:    ref_path_table_put
 *
 * Purpose:     Enter the 'obj' with 'path' in the table if
 *              not already there.
 *              Create an object reference, pte, and store them
 *              in the table.
 *
 * Return:      The object reference for the object.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/

hobj_ref_t             *
ref_path_table_put(hid_t obj, char *path)
{
    hobj_ref_t             *ref;
    herr_t                  status;
    struct ref_path_table_entry_t *pte;

    /* look up 'obj'.  If already in table, return */
    pte = ref_path_table_lookup(path);
    if (pte != NULL)
	return pte->obj_ref;

    /* if not found, then make new entry */

    pte = (struct ref_path_table_entry_t *)
	malloc(sizeof(struct ref_path_table_entry_t));
    if (pte == NULL) {
	/* fatal error? */
	return NULL;
    }

    pte->obj = obj;
    ref = (hobj_ref_t *) malloc(sizeof(hobj_ref_t));
    if (ref == NULL) {
	/* fatal error? */
	free(pte);
	return NULL;
    }

    status = H5Rcreate(ref, thefile, path, H5R_OBJECT, -1);
    if (status < 0) {
	/* fatal error? */
	free(ref);
	free(pte);
	return NULL;
    }

    pte->obj_ref = ref;

    pte->apath = strdup(path);

    pte->next = ref_path_table;
    ref_path_table = pte;

    npte++;

    return ref;
}

/*-------------------------------------------------------------------------
 * Function:    lookup_ref_path
 *
 * Purpose:     Lookup the path to the object with refernce 'ref'.
 *
 * Return:      Return a path to the object, or NULL if not found.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
char                   *
lookup_ref_path(hobj_ref_t * ref)
{
    int                     i;
    struct ref_path_table_entry_t *pte = NULL;

    if (ref_path_table == NULL)
	return NULL;

    pte = ref_path_table;
    if (pte == NULL) {
	/* fatal -- not initialized? */
	return NULL;
    }
    for (i = 0; i < npte; i++) {
	if (memcmp(ref, pte->obj_ref, sizeof(hobj_ref_t)) == 0) {
	    return pte->apath;
	}
	pte = pte->next;
    }
    return NULL;
}

/*-------------------------------------------------------------------------
 * Function:    fill_ref_path_table
 *
 * Purpose:     Called by interator to create references for
 *              all objects and enter them in the table.
 *
 * Return:      Error status.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/

static herr_t
fill_ref_path_table(hid_t group, const char *name, void UNUSED * op_data)
{
    hid_t                   obj;
    char                   *tmp;
    H5G_stat_t              statbuf;
    struct ref_path_table_entry_t *pte;
    char                   *thepath;

    H5Gget_objinfo(group, name, FALSE, &statbuf);
    tmp = (char *) malloc(strlen(prefix) + strlen(name) + 2);
    if (tmp == NULL)
	return FAIL;
    thepath = (char *) malloc(strlen(prefix) + strlen(name) + 2);
    if (thepath == NULL) {
	free(tmp);
	return FAIL;
    }
    strcpy(tmp, prefix);

    strcpy(thepath, prefix);
    strcat(thepath, "/");
    strcat(thepath, name);

    switch (statbuf.type) {
    case H5G_DATASET:
	if ((obj = H5Dopen(group, name)) >= 0) {
	    pte = ref_path_table_lookup(thepath);
	    if (pte == NULL) {
		ref_path_table_put(obj, thepath);
	    }
	    H5Dclose(obj);
	} else {
	    fprintf(stderr, "h5dump error: unable to get dataset %s\n", name);
	    d_status = 1;
	}
	break;
    case H5G_GROUP:
	if ((obj = H5Gopen(group, name)) >= 0) {
	    strcat(strcat(prefix, "/"), name);
	    pte = ref_path_table_lookup(thepath);
	    if (pte == NULL) {
		ref_path_table_put(obj, thepath);
		H5Giterate(obj, ".", NULL, fill_ref_path_table, NULL);
		strcpy(prefix, tmp);
	    }
	    H5Gclose(obj);
	} else {
	    fprintf(stderr, "h5dump error: unable to dump group %s\n", name);
	    d_status = 1;
	}
	break;
    case H5G_TYPE:
	if ((obj = H5Topen(group, name)) >= 0) {
	    pte = ref_path_table_lookup(thepath);
	    if (pte == NULL) {
		ref_path_table_put(obj, thepath);
	    }
	    H5Tclose(obj);
	} else {
	    fprintf(stderr, "h5dump error: unable to get dataset %s\n", name);
	    d_status = 1;
	}
	break;
    default:;
    }
    free(tmp);
    free(thepath);
    return 0;
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
 *-----------------------------------------------------------------------*/

char                   *
xml_escape_the_name(char *str)
{
    int                     extra;
    int                     len;
    int                     i;
    char                   *cp;
    char                   *ncp;
    char                   *rcp;

    if (str == NULL)
	return (char *) str;
    cp = (char *) str;
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

    if (extra == 0) {
	return (char *) str;
    } else {
	cp = (char *) str;
	rcp = ncp = calloc((size_t)(len + extra + 1), sizeof(char));
	if (ncp == NULL)
	    return NULL;	/* ?? */
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
 *-----------------------------------------------------------------------*/

char                   *
xml_escape_the_string(char *str, int slen)
{
    int                     extra;
    int                     len;
    int                     i;
    char                   *cp;
    char                   *ncp;
    char                   *rcp;

    if (str == NULL)
	return (char *) str;
    cp = (char *) str;
    if (slen < 0) {
	len = strlen(str);
    } else {
	len = slen;
    }
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

    cp = (char *) str;
    rcp = ncp = calloc((size_t)(len + extra + 1), sizeof(char));
    if (ncp == NULL)
	return NULL;		/* ?? */
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
 *-----------------------------------------------------------------------*/
static void
xml_print_datatype(hid_t type)
{
    char                   *fname;
    hid_t                   nmembers, mtype;
    int                     i, j, ndims, perm[H5DUMP_MAX_RANK];
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

    switch (H5Tget_class(type)) {
    case H5T_INTEGER:
	indentation(indent);
	printf("<AtomicType>\n");
	indent += COL;
	/* <IntegerType ByteOrder="bo" Sign="torf" Size="bytes"/> */
	ord = H5Tget_order(type);
	sgn = H5Tget_sign(type);
	indentation(indent);
	printf("<IntegerType ByteOrder=\"");
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
	printf("%d", sz);
	printf("\" />\n");
	indent -= COL;
	indentation(indent);
	printf("</AtomicType>\n");
	break;

    case H5T_FLOAT:
	/* <FloatType ByteOrder="bo" Size="bytes" 
	   SignBitLocation="bytes"
	   ExponentBits="eb" ExponentLocation="el" 
	   MatissaBits="mb" MatissaLocation="ml" /> */
	ord = H5Tget_order(type);
	indentation(indent);
	printf("<AtomicType>\n");
	indent += COL;
	indentation(indent);
	printf("<FloatType ByteOrder=\"");
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
	printf("%d", sz);
	H5Tget_fields(type, &spos, &epos, &esize, &mpos, &msize);
	printf("\" SignBitLocation=\"%d\" ", spos);
	printf("ExponentBits=\"%d\" ExponentLocation=\"%d\" ", esize, epos);
	printf("MantissaBits=\"%d\" MantissaLocation=\"%d\" />\n",
	       msize, mpos);
	indent -= COL;
	indentation(indent);
	printf("</AtomicType>\n");
	break;

    case H5T_TIME:
	indentation(indent);
	printf("<AtomicType>\n");
	indent += COL;
	indentation(indent);
	printf("<TimeType />\n");
	printf("<!-- H5T_TIME: not yet implemented -->");
	indent -= COL;
	indentation(indent);
	printf("</AtomicType>\n");
	break;

    case H5T_STRING:
	/* <StringType Cset="cs" StrSize="chars" StrPad="pad" /> */
	size = H5Tget_size(type);
	str_pad = H5Tget_strpad(type);
	cset = H5Tget_cset(type);

	indentation(indent);
	printf("<AtomicType>\n");
	indent += COL;
	indentation(indent);
	printf("<StringType Cset=\"");
	if (cset == H5T_CSET_ASCII) {
	    printf("H5T_CSET_ASCII\" ");
	} else {
	    printf("unknown_cset\" ");
	}
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
	printf("</AtomicType>\n");
	break;

    case H5T_BITFIELD:
	/* <BitfieldType ByteOrder="bo" Size="bytes"/> */
	ord = H5Tget_order(type);
	indentation(indent);
	printf("<AtomicType>\n");
	indent += COL;
	indentation(indent);
	printf("<BitfieldType ByteOrder=\"");
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
	printf("\" Size=\"%d\"/>\n", size);
	indent -= COL;
	indentation(indent);
	printf("</AtomicType>\n");
	break;

    case H5T_OPAQUE:
	/* <OpaqueType Tag="tag" Size="bytes" /> */

	indentation(indent);
	printf("<AtomicType>\n");
	indent += COL;
	indentation(indent);
	printf("<OpaqueType Tag=\"%s\" ", H5Tget_tag(type));
	size = H5Tget_size(type);
	printf("Size=\"%d\"/>\n", size);
	indent -= COL;
	indentation(indent);
	printf("</AtomicType>\n");
	break;

    case H5T_COMPOUND:
	/* recursively describe the components of a compound datatype */
	if (H5Tcommitted(type) > 0) {
	    /* detect a shared datatype, output only once */
	    H5Gget_objinfo(type, ".", TRUE, &statbuf);
	    i = search_obj(type_table, statbuf.objno);

	    if (i >= 0) {
		/* This should be defined somewhere else */
		if (!type_table->objs[i].recorded) {
		    /* 'anonymous' NDT.  Use it's object num.
		       as it's name.  */
		    printf("<NamedDataTypePtr OBJ-XID=\"");
		    printf("/#%lu:%lu",
			   type_table->objs[i].objno[0],
			   type_table->objs[i].objno[1]);
		    printf("\"/>\n");
		} else {
		    /* point to the NDT by name */
		    printf("<NamedDataTypePtr OBJ-XID=\"");
		    printf("%s",
			   xml_escape_the_name(type_table->objs[i].objname));
		    printf("\"/>\n");
		}
	    } else {
		printf("<!-- h5dump error: unknown committed type. -->\n");
		d_status = 1;
	    }

	} else {
	    /* type of a dataset */
	    nmembers = H5Tget_nmembers(type);

	    indentation(indent);
	    printf("<CompoundType>\n");

	    /* List each member Field of the type */
	    /*   <Field FieldName="name" > */
	    /*   <DataType > */
	    indent += COL;
	    for (i = 0; i < nmembers; i++) {
		fname = H5Tget_member_name(type, i);
		mtype = H5Tget_member_type(type, i);
		indentation(indent);
		printf("<Field FieldName=\"%s\">\n",
		       xml_escape_the_name(fname));

		free(fname);
		indent += COL;
		indentation(indent);
		printf("<DataType>\n");
		indent += COL;
		xml_print_datatype(mtype);
		indent -= COL;
		indentation(indent);
		printf("%s\n", dump_header_format->datatypeend);
		indent -= COL;

		indentation(indent);
		printf("</Field>\n");
	    }
	    indent -= COL;
	    indentation(indent);
	    printf("</CompoundType>\n");
	}
	break;

    case H5T_REFERENCE:
	indentation(indent);
	printf("<AtomicType>\n");
	indent += COL;
	indentation(indent);
	/*  Only Object references supported at this time */
	printf("<ReferenceType>\n");
	indentation(indent + COL);
	printf("<ObjectReferenceType />\n");
	indentation(indent);
	printf("</ReferenceType>\n");
	indent -= COL;
	indentation(indent);
	printf("</AtomicType>\n");
	break;

    case H5T_ENUM:
	/*  <EnumType Nelems="ne" >
	   list Name, values of enum
	 */
	nmembs = H5Tget_nmembers(type);
	indentation(indent);
	printf("<AtomicType>\n");
	indent += COL;
	indentation(indent);
	printf("<EnumType Nelems=\"%d\">\n", nmembs);
	xml_print_enum(type);
	indentation(indent);
	printf("</EnumType>\n");
	indent -= COL;
	indentation(indent);
	printf("</AtomicType>\n");
	break;

    case H5T_VLEN:
	indentation(indent);
	printf("<VLType>\n");
	super = H5Tget_super(type);
	indent += COL;
	indentation(indent);
	printf("<DataType>\n");
	indent += COL;
	xml_print_datatype(super);
	indent -= COL;
	indentation(indent);
	printf("%s\n", dump_header_format->datatypeend);
	indent -= COL;
	indentation(indent);
	printf("</VLType>\n");
	H5Tclose(super);

	break;

    case H5T_ARRAY:
	/* Get array base type */
	super = H5Tget_super(type);

	/* Print lead-in */
	indentation(indent);
	printf("<ArrayType Ndims=\"");
	ndims = H5Tget_array_ndims(type);
	printf("%d\">\n", ndims);

	/* Get array information */
	H5Tget_array_dims(type, dims, perm);

	/* list of dimensions */
	indent += COL;
	if (perm != NULL) {
	    /* for each dimension, list */
	    for (j = 0; j < ndims; j++) {
		indentation(indent);
		printf("<ArrayDimension DimSize=\"%u\" DimPerm=\"%u\"/>\n",
		       (int) dims[j], (int) perm[j]);
	    }
	} else {
	    for (j = 0; j < ndims; j++) {
		indentation(indent);
		printf("<ArrayDimension DimSize=\"%u\" DimPerm=\"0\"/>\n",
		       (int) dims[j]);
	    }
	}
	indent -= COL;

	indent += COL;
	indentation(indent);
	printf("<DataType>\n");
	indent += COL;
	xml_print_datatype(super);
	indent -= COL;
	indentation(indent);
	printf("%s\n", dump_header_format->datatypeend);
	indent -= COL;
	indentation(indent);
	printf("</ArrayType>\n");
	/* Close array base type */
	H5Tclose(super);
	break;

    default:
	printf("<!-- unknown data type -->");
	d_status = 1;
	break;
    }
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
 *-----------------------------------------------------------------------*/
static void
xml_dump_datatype(hid_t type)
{
    int                     i;
    H5G_stat_t              statbuf;

    indent += COL;
    indentation(indent);

    if (H5Tcommitted(type) > 0) {
	/* Data type is a shared or named data type */
	H5Gget_objinfo(type, ".", TRUE, &statbuf);
	i = search_obj(type_table, statbuf.objno);

	if (i >= 0) {
	    /* Shared data type, must be entered as an object  */
	    if (!type_table->objs[i].recorded) {
		/* anonymous stored data type:
		   following the dumper's current
		   practice:
		   use it's object ref as its name
		 */
		printf("<NamedDataTypePtr OBJ-XID=\"");
		printf("/#%lu:%lu",
		       type_table->objs[i].objno[0],
		       type_table->objs[i].objno[1]);
		printf("\"/>\n");
	    } else {
		/* pointer to a named data type already in XML */
		printf("<NamedDataTypePtr OBJ-XID=\"");
		printf("%s",
		       xml_escape_the_name(type_table->objs[i].objname));
		printf("\"/>\n");
	    }
	} else {
	    printf("<!-- h5dump error: unknown committed type. -->\n");
	}
	indent -= COL;
	return;
    }
    printf("%s %s\n", dump_header_format->datatypebegin,
	   dump_header_format->datatypeblockbegin);
    indent += COL;
    xml_print_datatype(type);
    indent -= COL;
    indentation(indent);
    printf("%s\n", dump_header_format->datatypeend);
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
 *-----------------------------------------------------------------------*/
static void
xml_dump_dataspace(hid_t space)
{
    hsize_t                 size[H5DUMP_MAX_RANK];
    hsize_t                 maxsize[H5DUMP_MAX_RANK];
    int                     ndims =
	H5Sget_simple_extent_dims(space, size, maxsize);
    int                     i;

    indentation(indent + COL);
    printf("%s\n", dump_header_format->dataspacebegin);
    if (H5Sis_simple(space)) {
	indentation(indent + COL + COL);

	if (ndims == 0) {
	    /* scalar dataspace (just a tag, no XML attrs. defined */
	    printf("<ScalarDataspace />\n");
	} else {
	    /* simple dataspace */
	    /* <SimpleDataspace Ndims="nd"> */
	    printf("<SimpleDataspace Ndims=\"%d\">\n", ndims);

	    /* print the <Dimension> elements */
	    for (i = 0; i < ndims; i++) {
		indentation(indent + COL + COL + COL);
		if (maxsize[i] == H5S_UNLIMITED) {
		    HDfprintf(stdout,
			      "<Dimension  DimSize=\"%Hu\" MaxDimSize=\"UNLIMITED\"/>\n",
			      size[i]);
		} else if (maxsize[i] == (hsize_t) 0) {
		    HDfprintf(stdout,
			      "<Dimension  DimSize=\"%Hu\" MaxDimSize=\"%Hu\"/>\n",
			      size[i], size[i]);
		} else {
		    HDfprintf(stdout,
			      "<Dimension  DimSize=\"%Hu\" MaxDimSize=\"%Hu\"/>\n",
			      size[i], maxsize[i]);
		}
	    }
	    indentation(indent + COL + COL);
	    printf("</SimpleDataspace>\n");
	}
    } else {
	printf("<!-- not yet implemented -->\n");
    }

    indentation(indent + COL);
    printf("%s\n", dump_header_format->dataspaceend);

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
 *-----------------------------------------------------------------------*/
static void
xml_dump_data(hid_t obj_id, int obj_data)
{
    h5dump_t               *outputformat = &xml_dataformat;
    int                     status = -1;
    void                   *buf;
    hid_t                   space, type, p_type;
    int                     ndims, i;
    hsize_t                 size[64], nelmts = 1;
    int                     depth;
    int                     stdindent = COL;	/* should be 3 */

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
    printf("%s\n", dump_header_format->databegin);
    indentation(indent + COL);
    printf("<DataFromFile>\n");
    if (obj_data == DATASET_DATA) {
	type = H5Dget_type(obj_id);
	if (H5Tget_class(type) == H5T_REFERENCE) {
	    status = xml_print_refs(obj_id, DATASET_DATA);
	} else if (H5Tget_class(type) == H5T_STRING) {
	    status = xml_print_strs(obj_id, DATASET_DATA);
	} else {
	    status = h5dump_dset(stdout, outputformat, obj_id, -1, depth);
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
	    p_type = h5dump_fixtype(type);
	    H5Tclose(type);

	    space = H5Aget_space(obj_id);

	    ndims = H5Sget_simple_extent_dims(space, size, NULL);

	    for (i = 0; i < ndims; i++)
		nelmts *= size[i];

	    buf =
		malloc((size_t)(nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type))));
	    assert(buf);

	    if (H5Aread(obj_id, p_type, buf) >= 0) {
		status =
		    h5dump_mem(stdout, outputformat, obj_id, p_type, space,
			       buf, depth);
	    }
	    free(buf);
	    H5Tclose(p_type);
	    H5Sclose(space);
	    H5Tclose(type);
	}
    }

    if (status < 0) {
	indentation(indent + COL);
	printf("Unable to print data.\n");
	status = 1;
    }

    indentation(indent + COL);
    printf("</DataFromFile>\n");
    indentation(indent);
    printf("%s\n", dump_header_format->dataend);
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
 *-----------------------------------------------------------------------*/
static herr_t
xml_dump_attr(hid_t attr, const char *attr_name, void UNUSED * op_data)
{
    hid_t                   attr_id, type, space;

    indentation(indent);
    printf("<Attribute Name=\"%s\">\n", xml_escape_the_name((char *)attr_name));

    if ((attr_id = H5Aopen_name(attr, attr_name)) >= 0) {
	type = H5Aget_type(attr_id);
	space = H5Aget_space(attr_id);

	dump_function_table->dump_dataspace_function(space);
	dump_function_table->dump_datatype_function(type);

	if (display_data) {
	    switch (H5Tget_class(type)) {
	    case H5T_INTEGER:
	    case H5T_FLOAT:
	    case H5T_STRING:
	    case H5T_BITFIELD:
	    case H5T_OPAQUE:
	    case H5T_ENUM:
	    case H5T_ARRAY:
		dump_function_table->dump_data_function(attr_id,
							ATTRIBUTE_DATA);
		break;

	    case H5T_TIME:
		indent += COL;
		indentation(indent);
		printf("<Data>\n");
		indentation(indent);
		printf("<!-- Time data not yet implemented. -->\n");
		indentation(indent);
		printf("<NoData />\n");
		indentation(indent);
		printf("<Data>\n");
		indent -= COL;
		break;

	    case H5T_COMPOUND:
		indentation(indent);
		printf
		    ("<!-- Note: format of compound data not specified -->\n");
		dump_function_table->dump_data_function(attr_id,
							ATTRIBUTE_DATA);
		break;

	    case H5T_REFERENCE:
		indentation(indent);
		printf("<Data>\n");
		indentation(indent);
		printf("<DataFromFile>\n");
		xml_print_refs(attr_id, ATTRIBUTE_DATA);
		indentation(indent);
		printf("</DataFromFile>\n");
		indentation(indent);
		printf("</Data>\n");
		break;

	    case H5T_VLEN:
		printf("<!-- Note: format of VL data not specified -->\n");
		dump_function_table->dump_data_function(attr_id,
							ATTRIBUTE_DATA);
		break;
	    default:
		indentation(indent);
		printf("<Data>\n");
		indentation(indent);
		printf("<!-- Unknown datatype: %d -->\n", H5Tget_class(type));
		indentation(indent);
		printf("<NoData/>\n");
		indentation(indent);
		printf("</Data>\n");
		break;
	    }
	} else {
	    /* The case of an attribute never yet written (??) */
	    indentation(indent);
	    printf("<Data>\n");
	    indentation(indent + COL);
	    printf("<NoData/>\n");
	    indentation(indent);
	    printf("</Data>\n");
	}

	H5Tclose(type);
	H5Sclose(space);
	H5Aclose(attr_id);
	indentation(indent);
	printf("%s\n", dump_header_format->attributeend);
	return SUCCEED;

    } else {
	/* ?? failed */
	indentation(indent + COL);
	printf("<!-- h5dump error: unable to open attribute. -->\n");
	indentation(indent);
	printf("%s\n", dump_header_format->attributeend);
	d_status = 1;
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
 *-----------------------------------------------------------------------*/
static void
xml_dump_named_datatype(hid_t type, const char *name)
{
    int                     nmembers = 1, x;
    hid_t                   mtype;
    char                   *fname;
    char                   *tmp;

    tmp = (char *) malloc(strlen(prefix) + strlen(name) + 2);
    strcpy(tmp, prefix);
    strcat(tmp, "/");
    strcat(tmp, name);

    indentation(indent);
    if (strncmp(name, "#", 1) == 0) {
	/*  Special:  this is an 'anonymous' NDT, deleted but 
	   still in use.
	   We follow the dumper's undocumented practice, and
	   use its object id as its name.
	   Exactly the same as normal, but a separate case
	   in the event we want to do something else in
	   the future.
	 */
	printf("<NamedDataType Name=\"%s\" OBJ-XID=\"%s\" Parents=\"%s\">\n",
	       name, 
	       xml_escape_the_name(tmp),
	       (strcmp(prefix, "") ? xml_escape_the_name(prefix) : "root"));
    } else {
	printf("<NamedDataType Name=\"%s\" OBJ-XID=\"%s\" Parents=\"%s\">\n",
	       xml_escape_the_name((char *)name),
	       xml_escape_the_name((char *)tmp),
	       (strcmp(prefix, "") ? xml_escape_the_name((char *)prefix) : "root"));
    }

    indent += COL;
    if (H5Tget_class(type) == H5T_COMPOUND) {
	/* Dump this here for sure.  */
	nmembers = H5Tget_nmembers(type);

	indentation(indent);
	printf("<CompoundType>\n");

	indent += COL;
	for (x = 0; x < nmembers; x++) {
	    fname = H5Tget_member_name(type, x);
	    mtype = H5Tget_member_type(type, x);
	    indentation(indent);
	    printf("<Field FieldName=\"%s\">\n", xml_escape_the_name(fname));
	    free(fname);

	    if ((H5Tget_class(mtype) == H5T_COMPOUND)
		|| (H5Tget_class(mtype) == H5T_VLEN)
		|| (H5Tget_class(mtype) == H5T_ARRAY)) {
		indent += COL;

		/*  Nested compound type:  recur */
		indentation(indent);
		printf("%s %s\n", dump_header_format->datatypebegin,
		       dump_header_format->datatypeblockbegin);
		indent += COL;
		xml_print_datatype(mtype);
		indent -= COL;
		indentation(indent);
		printf("%s\n", dump_header_format->datatypeend);
		indent -= COL;
	    } else {
		indent += COL;
		indentation(indent);
		printf("%s %s\n", dump_header_format->datatypebegin,
		       dump_header_format->datatypeblockbegin);
		indent += COL;
		xml_print_datatype(mtype);
		indent -= COL;
		indentation(indent);
		printf("%s\n", dump_header_format->datatypeend);
		indent -= COL;
	    }

	    indentation(indent);
	    printf("</Field>\n");
	}

	indent -= COL;
	indentation(indent);
	printf("</CompoundType>\n");
    } else {
	/* Other data types: call print_datatype */
	indent += COL;
	xml_print_datatype(type);
	indent -= COL;
    }

    indent -= COL;
    indentation(indent);
    printf("</NamedDataType>\n");
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
 *-----------------------------------------------------------------------*/
static void
xml_dump_group(hid_t gid, const char *name)
{
    H5G_stat_t              statbuf;
    char                   *cp;
    hid_t                   dset, type;
    char                    typename[1024], *tmp;
    char                   *par;
    int                     i;
    int                     isRoot = 0;
    int                     xtype;

    if (strcmp(name, "/") == 0) {
	isRoot = 1;
    } else {
	tmp = malloc(strlen(prefix) + strlen(name) + 2);
	strcpy(tmp, prefix);
	par = strdup(tmp);
	cp = strrchr(par, '/');
	if (cp != NULL) {
	    if ((cp == par) && strlen(par) > 1) {
		*(cp + 1) = '\0';
	    } else {
		*cp = '\0';
	    }
	}
    }
    indentation(indent);
    if (isRoot) {
	printf("<RootGroup OBJ-XID=\"root\">\n");
    } else {
	printf("<Group Name=\"%s\" OBJ-XID=\"%s\" Parents=\"%s\" >\n",
	       xml_escape_the_name((char *)name),
	       xml_escape_the_name(tmp),
	       (strcmp(prefix, "") ? xml_escape_the_name(par) : "root"));
    }
    indent += COL;
    H5Gget_objinfo(gid, ".", TRUE, &statbuf);

    if (statbuf.nlink > 1) {
	/* Group with more than one link to it... */
	i = search_obj(group_table, statbuf.objno);

	if (i < 0) {
	    indentation(indent);
	    printf("h5dump error: internal error\n");
	    d_status = 1;
	} else if (group_table->objs[i].displayed) {
	    /* already seen: enter a groupptr */
	    indentation(indent + COL);
	    printf("<GroupPtr OBJ-XID=\"%s\"/>\n",
		   xml_escape_the_name(group_table->objs[i].objname));
	} else {
	    /* first time this group has been seen -- describe it  */
	    strcpy(group_table->objs[i].objname, prefix);
	    group_table->objs[i].displayed = 1;

	    /* 1.  do all the attributes of the group */
	    H5Aiterate(gid, NULL,
		       dump_function_table->dump_attribute_function, NULL);

	    /* iterate through all the members */
	    xtype = H5G_GROUP;
	    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
	    xtype = H5G_DATASET;
	    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
	    if (!strcmp(name, "/") && unamedtype) {
		/* Very special case: dump unamed type in root group */
		for (i = 0; i < type_table->nobjs; i++) {
		    if (!type_table->objs[i].recorded) {
			dset = H5Dopen(gid, type_table->objs[i].objname);
			type = H5Dget_type(dset);
			sprintf(typename, "#%lu:%lu",
				type_table->objs[i].objno[0],
				type_table->objs[i].objno[1]);
			dump_function_table->dump_named_datatype_function
			    (type, typename);
			H5Tclose(type);
			H5Dclose(dset);
		    }
		}
	    }
	    xtype = H5G_TYPE;
	    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
	    xtype = H5G_LINK;
	    H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
	}
    } else {
	/* 1.  do all the attributes of the group */
	H5Aiterate(gid, NULL, dump_function_table->dump_attribute_function,
		   NULL);
	/* iterate through all the members */
	xtype = H5G_GROUP;
	H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
	xtype = H5G_DATASET;
	H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
	if (!strcmp(name, "/") && unamedtype) {
	    /* Very special case: dump unamed type in root group */
	    for (i = 0; i < type_table->nobjs; i++) {
		if (!type_table->objs[i].recorded) {
		    dset = H5Dopen(gid, type_table->objs[i].objname);
		    type = H5Dget_type(dset);
		    sprintf(typename, "#%lu:%lu",
			    type_table->objs[i].objno[0],
			    type_table->objs[i].objno[1]);
		    dump_function_table->dump_named_datatype_function(type,
								      typename);
		    H5Tclose(type);
		    H5Dclose(dset);
		}
	    }
	}
	xtype = H5G_TYPE;
	H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
	xtype = H5G_LINK;
	H5Giterate(gid, ".", NULL, dump_all, (void *) &xtype);
    }

    indent -= COL;
    indentation(indent);
    if (isRoot) {
	printf("</RootGroup>\n");
    } else {
	printf("%s\n", dump_header_format->groupend);
    }
/*  don't free this!!!
    free(tmp);
*/
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
 *-----------------------------------------------------------------------*/
static int
xml_print_refs(hid_t did, int source)
{
    herr_t                  e;
    hid_t                   type, space;
    char                   *buf;
    hobj_ref_t             *refbuf;
    char                   *path;
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
	path = lookup_ref_path(refbuf);
	indentation(indent + COL);
	if (path == NULL) {
	    printf("\"%s\"\n", "NULL");
	} else {
	    printf("\"%s\"\n", xml_escape_the_string(path, -1));
	}
	refbuf++;
    }
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
 *-----------------------------------------------------------------------*/
static int
xml_print_strs(hid_t did, int source)
{
    herr_t                  e;
    hid_t                   type, space;
    char                   *buf;
    char                   *bp;
    char                   *onestring;
    hsize_t                 ssiz;
    size_t                  tsiz;
    size_t                  i;
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
    if (source == DATASET_DATA) {
	space = H5Dget_space(did);
	ssiz = H5Sget_simple_extent_npoints(space);
	ssiz *= H5Tget_size(type);

	buf = calloc((size_t)ssiz, sizeof(char));
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

	buf = calloc((size_t)ssiz, sizeof(char));
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

/*  pull out each string... */
    ssiz = H5Sget_simple_extent_npoints(space);

    tsiz = H5Tget_size(type);
    onestring = (char *) calloc((size_t)tsiz, sizeof(char));
    bp = buf;

    for (i = 0; i < ssiz; i++) {
	strncpy(onestring, bp, tsiz);
	indentation(indent + COL);
	if (onestring == NULL) {
	    printf("\"%s\"\n", "NULL");
	} else {
	    printf("\"%s\"\n", xml_escape_the_string(onestring, (int)tsiz));
	}
	bp += tsiz;
    }
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    check_compression
 *
 * Purpose:     private function to check for compression and
 *              put a comment in the XML.  (Not fully implemented.)
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void
check_compression(hid_t dcpl)
{
    int                     nfilt;
    int                     i;
    H5Z_filter_t            filter;
    char                    namebuf[20];
    size_t                  cd_nelmts = 1;
    unsigned int            cd_values;
    unsigned int            flags;
/*  not used yet:  will need to do somehting more elaborate to handle future
 * compression methods.
    char                   *t1 = "H5Z_FILTER_DEFLATE";
*/

    nfilt = H5Pget_nfilters(dcpl);
    if (nfilt <= 0)
	return;
    for (i = 0; i < nfilt; i++) {
	filter = H5Pget_filter(dcpl, i, &flags,
			       (size_t *) &cd_nelmts,
			       &cd_values, 20, namebuf);
	if (filter == H5Z_FILTER_DEFLATE) {
	    indentation(indent + COL);
	    printf("<Compression />\n");
	    indentation(indent + COL);
	    printf("<!-- Compression parameter %d -->\n", cd_values);
	}
    }
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
 *-----------------------------------------------------------------------*/
static void
xml_dump_dataset(hid_t did, const char *name)
{
    hid_t                   type, space;
    hid_t                   dcpl;
    int                     maxdims;
    hsize_t                *chsize;
    int                     ndims;
    int                     i;
    char                   *tmp;

    tmp = (char *) malloc(strlen(prefix) + strlen(name) + 2);
    strcpy(tmp, prefix);
    strcat(tmp, "/");
    strcat(tmp, name);
    indentation(indent);
    printf("<Dataset Name=\"%s\" OBJ-XID=\"%s\" Parents=\"%s\">\n",
	   xml_escape_the_name((char *)name),
	   xml_escape_the_name((char *)tmp),
	   (strcmp(prefix, "") ? xml_escape_the_name((char *)prefix) : "root"));

    dcpl = H5Dget_create_plist(did);
    type = H5Dget_type(did);
    space = H5Dget_space(did);

    /* Print information about chunked storage */
    if (H5D_CHUNKED == H5Pget_layout(dcpl)) {
	maxdims = H5Sget_simple_extent_ndims(space);
	chsize = (hsize_t *) malloc(maxdims * sizeof(hsize_t));
	indent += COL;
	indentation(indent);
	printf("<StorageLayout>\n");
	indent += COL;
	indentation(indent);
	printf("<ChunkedLayout ");
	ndims = H5Pget_chunk(dcpl, maxdims, chsize);
	printf("Ndims=\"%d\">\n", ndims);
	/* check for compression and tell about it... */

	check_compression(dcpl);

	indent += COL;
	for (i = 0; i < ndims; i++) {
	    indentation(indent);
	    HDfprintf(stdout,"<ChunkDimension DimSize=\"%Hu\" />\n", chsize[i]);
	}
	indent -= COL;

	indentation(indent);
	printf("</ChunkedLayout>\n");
	indent -= COL;
	indentation(indent);
	printf("</StorageLayout>\n");
	indent -= COL;
	free(chsize);
    }
    /* and check for external.... */

    dump_function_table->dump_dataspace_function(space);
    dump_function_table->dump_datatype_function(type);

    indent += COL;
    H5Aiterate(did, NULL, dump_function_table->dump_attribute_function, NULL);
    indent -= COL;
    i = H5Dget_storage_size(did);
    if (display_data && (i > 0)) {
	switch (H5Tget_class(type)) {
	case H5T_INTEGER:
	case H5T_FLOAT:
	case H5T_STRING:
	case H5T_BITFIELD:
	case H5T_OPAQUE:
	case H5T_ENUM:
	case H5T_ARRAY:
	    dump_function_table->dump_data_function(did, DATASET_DATA);
	    break;

	case H5T_TIME:
	    indent += COL;
	    indentation(indent);
	    printf("<Data>\n");
	    indentation(indent);
	    printf("<!-- Time data not yet implemented. -->\n");
	    indentation(indent);
	    printf("<NoData />\n");
	    indentation(indent);
	    printf("<Data>\n");
	    indent -= COL;
	    break;

	case H5T_COMPOUND:
	    indentation(indent);
	    printf("<!-- Note: format of compound data not specified -->\n");
	    dump_function_table->dump_data_function(did, DATASET_DATA);
	    break;

	case H5T_REFERENCE:
	    indentation(indent);
	    printf("<Data>\n");
	    indentation(indent);
	    printf("<DataFromFile>\n");
	    xml_print_refs(did, DATASET_DATA);
	    indentation(indent);
	    printf("</DataFromFile>\n");
	    indentation(indent);
	    printf("</Data>\n");
	    break;

	case H5T_VLEN:
	    printf("<!-- Note: format of VL data not specified -->\n");
	    dump_function_table->dump_data_function(did, DATASET_DATA);
	    break;
	default:
	    indentation(indent);
	    printf("<Data>\n");
	    indentation(indent);
	    printf("<!-- Unknown datatype: %d -->\n", H5Tget_class(type));
	    indentation(indent);
	    printf("<NoData/>\n");
	    indentation(indent);
	    printf("</Data>\n");
	    break;
	}
    } else {
	/* no data written */
	indentation(indent);
	printf("<Data>\n");
	indentation(indent);
	printf("<NoData/>\n");
	indentation(indent);
	printf("</Data>\n");
    }

/*
    free(tmp);
*/
    H5Tclose(type);
    H5Sclose(space);
    indentation(indent);
    printf("%s\n", dump_header_format->datasetend);
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
 *-----------------------------------------------------------------------*/
static void
xml_print_enum(hid_t type)
{
    char                  **name = NULL;	/*member names                    */
    unsigned char          *value = NULL;	/*value array                    */
    int                     nmembs;	/*number of members                */
    hid_t                   super;	/*enum base integer type        */
    hid_t                   native = -1;	/*native integer data type        */
    size_t                  dst_size;	/*destination value type size    */
    int                     i;	/*miscellaneous counters        */
    size_t                  j;

    nmembs = H5Tget_nmembers(type);
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
    name = calloc((size_t)nmembs, sizeof(char *));
    value = calloc((size_t)nmembs, MAX(H5Tget_size(type), dst_size));

    for (i = 0; i < nmembs; i++) {
	name[i] = H5Tget_member_name(type, i);
	H5Tget_member_value(type, i, value + i * H5Tget_size(type));
    }

    /* Convert values to native data type */
    if (native > 0)
	H5Tconvert(super, native, (hsize_t)nmembs, value, NULL, H5P_DEFAULT);

    /* Sort members by increasing value */
    /*not implemented yet */

    /* Print members */
    indent += COL;
    for (i = 0; i < nmembs; i++) {
	indentation(indent);
	printf("<EnumElement>\n");
	indentation(indent + COL);
	printf("%s\n", xml_escape_the_name(name[i]));
	indentation(indent);
	printf("</EnumElement>\n");
	indentation(indent);
	printf("<EnumValue>\n");
	indentation(indent + COL);
	if (native < 0) {
	    printf("0x");

	    for (j = 0; j < dst_size; j++)
		printf("%02x", value[i * dst_size + j]);
	} else if (H5T_SGN_NONE == H5Tget_sign(native)) {
	    printf("%" PRINTF_LL_WIDTH "u", *((unsigned long_long *)
					      ((void *) (value + i * dst_size))));
	} else {
	    printf("%" PRINTF_LL_WIDTH "d",
		   *((long_long *) ((void *) (value + i * dst_size))));
	}
	printf("\n");
	indentation(indent);
	printf("</EnumValue>\n");

    }
    indent -= COL;

    /* Release resources */
    for (i = 0; i < nmembs; i++)
	free(name[i]);

    free(name);
    free(value);
    H5Tclose(super);
}
