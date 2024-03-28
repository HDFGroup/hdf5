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
 * programming environment it declares.  Other than that, h5ls only calls
 * HDF5 API functions (except for H5G_basename())
 */
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5tools_dump.h"
#include "h5trav.h"

/* Name of tool */
#define PROGRAMNAME "h5ls"

#define NAME_BUF_SIZE 2048
/*
 *  Alternative formatting for data dumped by H5LS
 *
 *  This table only affects H5LS output.
 */
static h5tool_format_t ls_dataformat = {
    0, /*raw */

    "",     /*fmt_raw */
    "%d",   /*fmt_int */
    "%u",   /*fmt_uint */
    "%hhd", /*fmt_schar */
    "%u",   /*fmt_uchar */
    "%d",   /*fmt_short */
    "%u",   /*fmt_ushort */
    "%ld",  /*fmt_long */
    "%lu",  /*fmt_ulong */
    NULL,   /*fmt_llong */
    NULL,   /*fmt_ullong */
    "%g",   /*fmt_double */
    "%g",   /*fmt_float */

    0, /*ascii */
    0, /*str_locale */
    0, /*str_repeat */

    "[", /*arr_pre */
    ",", /*arr_sep */
    "]", /*arr_suf */
    1,   /*arr_linebreak */

    "",   /*cmpd_name */
    ",",  /*cmpd_sep */
    "{",  /*cmpd_pre */
    "}",  /*cmpd_suf */
    "",   /*cmpd_end */
    NULL, /* cmpd_listv */

    ",", /*vlen_sep */
    "(", /*vlen_pre */
    ")", /*vlen_suf */
    "",  /*vlen_end */

    "%s", /*elmt_fmt */
    ",",  /*elmt_suf1 */
    " ",  /*elmt_suf2 */

    "%" PRIuHSIZE, /*idx_n_fmt */
    ",",           /*idx_sep */
    "(%s)",        /*idx_fmt */

    65535,
    /*line_ncols */ /*standard default columns */
    0,              /*line_per_line */
    "",             /*line_pre */
    "%s",           /*line_1st */
    "%s",           /*line_cont */
    "",             /*line_suf */
    "",             /*line_sep */
    1,              /*line_multi_new */
    "   ",          /*line_indent */

    0, /*skip_first */

    0,                  /*obj_hidefileno */
    "-%lu:%" PRIuHADDR, /*obj_format */

    0,            /*dset_hidefileno */
    "DSET-%s ",   /*dset_format */
    "%sBlk%lu: ", /*dset_blockformat_pre */
    "%sPt%lu: ",  /*dset_ptformat_pre */
    "%s",         /*dset_ptformat */
    1,            /*array indices */
    1             /*escape non printable characters */
};

/* Struct to pass through to visitors */
typedef struct {
    const char     *fname;          /* Filename */
    hid_t           fid;            /* File ID */
    hid_t           gid;            /* Group ID */
    bool            symlink_target; /* Whether this is the target of an symbolic link */
    symlink_trav_t *symlink_list;   /* List of visited symbolic links */
    size_t          base_len;       /* Length of base path name, if not root */
    size_t          name_start;     /* # of leading characters to strip off path names on output */
} iter_t;

/* Command-line switches */
static int  verbose_g          = 0;     /* lots of extra output */
static int  width_g            = 80;    /* output width in characters */
static bool address_g          = false; /* print raw data addresses */
static bool data_g             = false; /* display dataset values? */
static bool label_g            = false; /* label compound values? */
static bool string_g           = false; /* print 1-byte numbers as ASCII? */
static bool fullname_g         = false; /* print full path names */
static bool recursive_g        = false; /* recursive descent listing */
static bool follow_symlink_g   = false; /* follow symbolic links */
static bool no_dangling_link_g = false; /* treat dangling link is error */
static bool follow_elink_g     = false; /* follow external links */
static bool grp_literal_g      = false; /* list group, not contents */
static bool hexdump_g          = false; /* show data as raw hexadecimal */
static bool simple_output_g    = false; /* make output more machine-readable */
static bool show_file_name_g   = false; /* show file name for full names */
static bool no_line_wrap_g     = false; /* show data content without line wrap */
static bool display_root_g     = false; /* show root group in output? */

/* Information about how to display each type of object */
static struct dispatch_t {
    const char *name;
    hid_t (*open)(hid_t loc, const char *name, hid_t apl_id);
    herr_t (*close)(hid_t obj);
    herr_t (*list1)(hid_t obj);
    herr_t (*list2)(hid_t obj, const char *name);
} dispatch_g[H5O_TYPE_NTYPES];

#define DISPATCH(TYPE, NAME, LIST1, LIST2)                                                                   \
    do {                                                                                                     \
        dispatch_g[TYPE].name  = (NAME);                                                                     \
        dispatch_g[TYPE].list1 = (LIST1);                                                                    \
        dispatch_g[TYPE].list2 = (LIST2);                                                                    \
    } while (0)

static void   print_type(h5tools_str_t *buffer, hid_t type, int ind);
static bool   print_int_type(h5tools_str_t *buffer, hid_t type, int ind);
static bool   print_float_type(h5tools_str_t *buffer, hid_t type, int ind);
static herr_t visit_obj(hid_t file, const char *oname, iter_t *iter);

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: Prints a usage message on stderr and then returns.
 *
 * Return: void
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    FLUSHSTREAM(rawoutstream);
    PRINTVALSTREAM(rawoutstream, "usage: h5ls [OPTIONS] file[/OBJECT] [file[/[OBJECT]...]\n");
    PRINTVALSTREAM(rawoutstream, "  OPTIONS\n");
    PRINTVALSTREAM(rawoutstream, "   -h, -?, --help  Print a usage message and exit\n");
    PRINTVALSTREAM(rawoutstream,
                   "   -a, --address   Print raw data address.  If dataset is contiguous, address\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   is offset in file of beginning of raw data. If chunked,\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   returned list of addresses indicates offset of each chunk.\n");
    PRINTVALSTREAM(rawoutstream, "                   Must be used with -v, --verbose option.\n");
    PRINTVALSTREAM(rawoutstream, "                   Provides no information for non-dataset objects.\n");
    PRINTVALSTREAM(rawoutstream, "   -d, --data      Print the values of datasets\n");
    PRINTVALSTREAM(rawoutstream, "   --enable-error-stack\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   Prints messages from the HDF5 error stack as they occur.\n");
    PRINTVALSTREAM(rawoutstream, "   --follow-symlinks\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   Follow symbolic links (soft links and external links)\n");
    PRINTVALSTREAM(rawoutstream, "                   to display target object information.\n");
    PRINTVALSTREAM(rawoutstream, "                   Without this option, h5ls identifies a symbolic link\n");
    PRINTVALSTREAM(rawoutstream, "                   as a soft link or external link and prints the value\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   assigned to the symbolic link; it does not provide any\n");
    PRINTVALSTREAM(rawoutstream, "                   information regarding the target object or determine\n");
    PRINTVALSTREAM(rawoutstream, "                   whether the link is a dangling link.\n");
    PRINTVALSTREAM(rawoutstream, "   --no-dangling-links\n");
    PRINTVALSTREAM(rawoutstream, "                   Must be used with --follow-symlinks option;\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   otherwise, h5ls shows error message and returns an exit\n");
    PRINTVALSTREAM(rawoutstream, "                   code of 1.\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   Check for any symbolic links (soft links or external links)\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   that do not resolve to an existing object (dataset, group,\n");
    PRINTVALSTREAM(rawoutstream, "                   or named datatype).\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   If any dangling link is found, this situation is treated\n");
    PRINTVALSTREAM(rawoutstream, "                   as an error and h5ls returns an exit code of 1.\n");
    PRINTVALSTREAM(rawoutstream, "   -f, --full      Print full path names instead of base names\n");
    PRINTVALSTREAM(rawoutstream, "   -g, --group     Show information about a group, not its contents\n");
    PRINTVALSTREAM(rawoutstream, "   -l, --label     Label members of compound datasets\n");
    PRINTVALSTREAM(rawoutstream, "   -r, --recursive List all groups recursively, avoiding cycles\n");
    PRINTVALSTREAM(rawoutstream, "   -s, --string    Print 1-byte integer datasets as ASCII\n");
    PRINTVALSTREAM(rawoutstream, "   -S, --simple    Use a machine-readable output format\n");
    PRINTVALSTREAM(rawoutstream, "   -wN, --width=N  Set the number of columns of output\n");
    PRINTVALSTREAM(rawoutstream, "   -v, --verbose   Generate more verbose output\n");
    PRINTVALSTREAM(rawoutstream, "   -V, --version   Print version number and exit\n");
    PRINTVALSTREAM(rawoutstream, "   --vfd=DRIVER    Use the specified virtual file driver\n");
    PRINTVALSTREAM(rawoutstream, "   -x, --hexdump   Show raw data in hexadecimal format\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --s3-cred=C     Supply S3 authentication information to \"ros3\" vfd.\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   Accepts tuple of \"(<aws-region>,<access-id>,<access-key>)\".\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   If absent or C->\"(,,)\", defaults to no-authentication.\n");
    PRINTVALSTREAM(rawoutstream, "                   Has no effect if vfd flag not set to \"ros3\".\n");
    PRINTVALSTREAM(rawoutstream, "   --hdfs-attrs=A  Supply configuration information to Hadoop VFD.\n");
    PRINTVALSTREAM(rawoutstream, "                   Accepts tuple of (<namenode name>,<namenode port>,\n");
    PRINTVALSTREAM(rawoutstream, "                   ...<kerberos cache path>,<username>,<buffer size>)\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   If absent or A == '(,,,,)', all default values are used.\n");
    PRINTVALSTREAM(rawoutstream, "                   Has no effect if vfd flag is not 'hdfs'.\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vol-value     Value (ID) of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                   HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "   --vol-name      Name of the VOL connector to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                   HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "   --vol-info      VOL-specific info to pass to the VOL connector used for\n");
    PRINTVALSTREAM(rawoutstream, "                   opening the HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   If none of the above options are used to specify a VOL, then\n");
    PRINTVALSTREAM(rawoutstream,
                   "                   the VOL named by HDF5_VOL_CONNECTOR (or the native VOL connector,\n");
    PRINTVALSTREAM(rawoutstream, "                   if that environment variable is unset) will be used\n");
    PRINTVALSTREAM(rawoutstream, "   --vfd-value     Value (ID) of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                   HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "   --vfd-name      Name of the VFL driver to use for opening the\n");
    PRINTVALSTREAM(rawoutstream, "                   HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "   --vfd-info      VFD-specific info to pass to the VFL driver used for\n");
    PRINTVALSTREAM(rawoutstream, "                   opening the HDF5 file specified\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  file/OBJECT\n");
    PRINTVALSTREAM(rawoutstream, "    Each object consists of an HDF5 file name optionally followed by a\n");
    PRINTVALSTREAM(rawoutstream, "    slash and an object name within the file (if no object is specified\n");
    PRINTVALSTREAM(rawoutstream, "    within the file then the contents of the root group are displayed).\n");
    PRINTVALSTREAM(rawoutstream, "    The file name may include a printf(3C) integer format such as\n");
    PRINTVALSTREAM(rawoutstream, "    \"%%05d\" to open a file family.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "  Deprecated Options\n");
    PRINTVALSTREAM(rawoutstream,
                   "    The following options have been removed in HDF5 1.12. Use the indicated\n");
    PRINTVALSTREAM(rawoutstream, "    replacement option in all work.\n");
    PRINTVALSTREAM(rawoutstream, "\n");
    PRINTVALSTREAM(rawoutstream, "   -E or --external   Follow external links.\n");
    PRINTVALSTREAM(rawoutstream, "                      Replaced by --follow-symlinks.\n");
    PRINTVALSTREAM(rawoutstream, "   -e, --errors       Show all HDF5 error reporting\n");
    PRINTVALSTREAM(rawoutstream, "                      Replaced by --enable-error-stack.\n");
}

/*-------------------------------------------------------------------------
 * Function:    print_string
 *
 * Purpose:     Print a string value by escaping unusual characters. If
 *              STREAM is null then we only count how large the output would be.
 *
 * Return:      Number of characters printed.
 *-------------------------------------------------------------------------
 */
static int
print_string(h5tools_str_t *buffer, const char *s, bool escape_spaces)
{
    int nprint = 0;

    for (/*void*/; s && *s; s++) {
        switch (*s) {
            case '"':
                if (buffer)
                    h5tools_str_append(buffer, "\\\"");
                nprint += 2;
                break;
            case '\\':
                if (buffer)
                    h5tools_str_append(buffer, "\\\\");
                nprint += 2;
                break;
            case '\b':
                if (buffer)
                    h5tools_str_append(buffer, "\\b");
                nprint += 2;
                break;
            case '\f':
                if (buffer)
                    h5tools_str_append(buffer, "\\f");
                nprint += 2;
                break;
            case '\n':
                if (buffer)
                    h5tools_str_append(buffer, "\\n");
                nprint += 2;
                break;
            case '\r':
                if (buffer)
                    h5tools_str_append(buffer, "\\r");
                nprint += 2;
                break;
            case '\t':
                if (buffer)
                    h5tools_str_append(buffer, "\\t");
                nprint += 2;
                break;
            case ' ':
                if (escape_spaces) {
                    if (buffer)
                        h5tools_str_append(buffer, "\\ ");
                    nprint += 2;
                }
                else {
                    if (buffer)
                        h5tools_str_append(buffer, " ");
                    nprint++;
                }
                break;
            default:
                if (isprint((int)*s)) {
                    if (buffer)
                        h5tools_str_append(buffer, "%c", *s);
                    nprint++;
                }
                else {
                    if (buffer)
                        h5tools_str_append(buffer, "\\%03o", *((const unsigned char *)s));
                    nprint += 4;
                }
                break;
        }
    }
    return nprint;
}

/*-------------------------------------------------------------------------
 * Function:    print_obj_name
 *
 * Purpose:     Print an object name and another string.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static int
print_obj_name(h5tools_str_t *buffer, const iter_t *iter, const char *oname, const char *s)
{
    static char fullname[NAME_BUF_SIZE]; /* Buffer for file and/or object name */
    const char *name = fullname;         /* Pointer to buffer for printing */
    int         n;

    if (show_file_name_g)
        snprintf(fullname, sizeof(fullname), "%s/%s", iter->fname, oname + iter->name_start);
    else
        name = oname + iter->name_start;

    /* Print the object name, either full name or base name */
    if (fullname_g)
        n = print_string(buffer, name, true);
    else {
        const char *last_sep; /* The location of the last group separator */

        /* Find the last component of the path name */
        if (NULL == (last_sep = strrchr(name, '/')))
            last_sep = name;
        else {
            last_sep++;
        } /* end else */
        n = print_string(buffer, last_sep, true);
    } /* end else */
    h5tools_str_append(buffer, "%*s ", MAX(0, (24 - n)), s);

    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_native_type
 *
 * Purpose:     Prints the name of a native C data type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed.
 *-------------------------------------------------------------------------
 */
static bool
print_native_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    if (!simple_output_g) {
        if (H5Tequal(type, H5T_NATIVE_SCHAR) == true) {
            h5tools_str_append(buffer, "native signed char");
        }
        else if (H5Tequal(type, H5T_NATIVE_UCHAR) == true) {
            h5tools_str_append(buffer, "native unsigned char");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT) == true) {
            h5tools_str_append(buffer, "native int");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT) == true) {
            h5tools_str_append(buffer, "native unsigned int");
        }
        else if (H5Tequal(type, H5T_NATIVE_SHORT) == true) {
            h5tools_str_append(buffer, "native short");
        }
        else if (H5Tequal(type, H5T_NATIVE_USHORT) == true) {
            h5tools_str_append(buffer, "native unsigned short");
        }
        else if (H5Tequal(type, H5T_NATIVE_LONG) == true) {
            h5tools_str_append(buffer, "native long");
        }
        else if (H5Tequal(type, H5T_NATIVE_ULONG) == true) {
            h5tools_str_append(buffer, "native unsigned long");
        }
        else if (H5Tequal(type, H5T_NATIVE_LLONG) == true) {
            h5tools_str_append(buffer, "native long long");
        }
        else if (H5Tequal(type, H5T_NATIVE_ULLONG) == true) {
            h5tools_str_append(buffer, "native unsigned long long");
        }
#ifdef H5_HAVE__FLOAT16
        else if (H5Tequal(type, H5T_NATIVE_FLOAT16) == true) {
            h5tools_str_append(buffer, "native _Float16");
        }
#endif
        else if (H5Tequal(type, H5T_NATIVE_FLOAT) == true) {
            h5tools_str_append(buffer, "native float");
        }
        else if (H5Tequal(type, H5T_NATIVE_DOUBLE) == true) {
            h5tools_str_append(buffer, "native double");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT8) == true) {
            h5tools_str_append(buffer, "native int8_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT8) == true) {
            h5tools_str_append(buffer, "native uint8_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT16) == true) {
            h5tools_str_append(buffer, "native int16_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT16) == true) {
            h5tools_str_append(buffer, "native uint16_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT32) == true) {
            h5tools_str_append(buffer, "native int32_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT32) == true) {
            h5tools_str_append(buffer, "native uint32_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT64) == true) {
            h5tools_str_append(buffer, "native int64_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT64) == true) {
            h5tools_str_append(buffer, "native uint64_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT_LEAST8) == true) {
            h5tools_str_append(buffer, "native int_least8_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT_LEAST8) == true) {
            h5tools_str_append(buffer, "native uint_least8_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT_LEAST16) == true) {
            h5tools_str_append(buffer, "native int_least16_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT_LEAST16) == true) {
            h5tools_str_append(buffer, "native uint_least16_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT_LEAST32) == true) {
            h5tools_str_append(buffer, "native int_least32_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT_LEAST32) == true) {
            h5tools_str_append(buffer, "native uint_least32_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT_LEAST64) == true) {
            h5tools_str_append(buffer, "native int_least64_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT_LEAST64) == true) {
            h5tools_str_append(buffer, "native uint_least64_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT_FAST8) == true) {
            h5tools_str_append(buffer, "native int_fast8_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT_FAST8) == true) {
            h5tools_str_append(buffer, "native uint_fast8_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT_FAST16) == true) {
            h5tools_str_append(buffer, "native int_fast16_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT_FAST16) == true) {
            h5tools_str_append(buffer, "native uint_fast16_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT_FAST32) == true) {
            h5tools_str_append(buffer, "native int_fast32_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT_FAST32) == true) {
            h5tools_str_append(buffer, "native uint_fast32_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT_FAST64) == true) {
            h5tools_str_append(buffer, "native int_fast64_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT_FAST64) == true) {
            h5tools_str_append(buffer, "native uint_fast64_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_B8) == true) {
            h5tools_str_append(buffer, "native 8-bit field");
        }
        else if (H5Tequal(type, H5T_NATIVE_B16) == true) {
            h5tools_str_append(buffer, "native 16-bit field");
        }
        else if (H5Tequal(type, H5T_NATIVE_B32) == true) {
            h5tools_str_append(buffer, "native 32-bit field");
        }
        else if (H5Tequal(type, H5T_NATIVE_B64) == true) {
            h5tools_str_append(buffer, "native 64-bit field");
        }
        else if (H5Tequal(type, H5T_NATIVE_HSIZE) == true) {
            h5tools_str_append(buffer, "native hsize_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_HSSIZE) == true) {
            h5tools_str_append(buffer, "native hssize_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_HERR) == true) {
            h5tools_str_append(buffer, "native herr_t");
        }
        else if (H5Tequal(type, H5T_NATIVE_HBOOL) == true) {
            h5tools_str_append(buffer, "native bool");
        }
        else {
            return print_int_type(buffer, type, ind);
        }
    }
    else {
        return print_int_type(buffer, type, ind);
    }
    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_ieee_type
 *
 * Purpose:     Print the name of an IEEE floating-point data type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_ieee_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    if (H5Tequal(type, H5T_IEEE_F16BE) == true) {
        h5tools_str_append(buffer, "IEEE 16-bit big-endian float");
    }
    else if (H5Tequal(type, H5T_IEEE_F16LE) == true) {
        h5tools_str_append(buffer, "IEEE 16-bit little-endian float");
    }
    else if (H5Tequal(type, H5T_IEEE_F32BE) == true) {
        h5tools_str_append(buffer, "IEEE 32-bit big-endian float");
    }
    else if (H5Tequal(type, H5T_IEEE_F32LE) == true) {
        h5tools_str_append(buffer, "IEEE 32-bit little-endian float");
    }
    else if (H5Tequal(type, H5T_IEEE_F64BE) == true) {
        h5tools_str_append(buffer, "IEEE 64-bit big-endian float");
    }
    else if (H5Tequal(type, H5T_IEEE_F64LE) == true) {
        h5tools_str_append(buffer, "IEEE 64-bit little-endian float");
    }
    else {
        return print_float_type(buffer, type, ind);
    }
    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_precision
 *
 * Purpose:     Prints information on the next line about precision and
 *              padding if the precision is less than the total data type
 *              size.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
print_precision(h5tools_str_t *buffer, hid_t type, int ind)
{
    size_t      prec;          /* precision */
    H5T_pad_t   plsb, pmsb;    /* lsb and msb padding */
    const char *plsb_s = NULL; /* lsb padding string */
    const char *pmsb_s = NULL; /* msb padding string */
    size_t      nbits;         /* number of bits */

    /* If the precision is less than the total size then show the precision
     * and offset on the following line.  Also display the padding
     * information. */
    if (8 * H5Tget_size(type) != (prec = H5Tget_precision(type))) {
        h5tools_str_append(buffer, "\n%*s(%lu bit%s of precision beginning at bit %lu)", ind, "",
                           (unsigned long)prec, 1 == prec ? "" : "s", (unsigned long)H5Tget_offset(type));

        H5Tget_pad(type, &plsb, &pmsb);
        if (H5Tget_offset(type) > 0) {
            switch (plsb) {
                case H5T_PAD_ZERO:
                    plsb_s = "zero";
                    break;
                case H5T_PAD_ONE:
                    plsb_s = "one";
                    break;
                case H5T_PAD_BACKGROUND:
                    plsb_s = "bkg";
                    break;
                case H5T_PAD_ERROR:
                case H5T_NPAD:
                    plsb_s = "unknown";
                    break;
                default:
                    break;
            }
        }
        if ((unsigned)H5Tget_offset(type) + prec < 8 * H5Tget_size(type)) {
            switch (pmsb) {
                case H5T_PAD_ZERO:
                    pmsb_s = "zero";
                    break;
                case H5T_PAD_ONE:
                    pmsb_s = "one";
                    break;
                case H5T_PAD_BACKGROUND:
                    pmsb_s = "bkg";
                    break;
                case H5T_PAD_ERROR:
                case H5T_NPAD:
                    pmsb_s = "unknown";
                    break;
                default:
                    break;
            }
        }
        if (plsb_s || pmsb_s) {
            h5tools_str_append(buffer, "\n%*s(", ind, "");
            if (plsb_s) {
                nbits = (unsigned)H5Tget_offset(type);
                h5tools_str_append(buffer, "%lu %s bit%s at bit 0", (unsigned long)nbits, plsb_s,
                                   1 == nbits ? "" : "s");
            }
            if (plsb_s && pmsb_s)
                h5tools_str_append(buffer, ", ");
            if (pmsb_s) {
                nbits = (8 * H5Tget_size(type)) - ((unsigned)H5Tget_offset(type) + prec);
                h5tools_str_append(buffer, "%lu %s bit%s at bit %lu", (unsigned long)nbits, pmsb_s,
                                   1 == nbits ? "" : "s", (unsigned long)(8 * H5Tget_size(type) - nbits));
            }
            h5tools_str_append(buffer, ")");
        }
    }
}

/*-------------------------------------------------------------------------
 * Function:    print_int_type
 *
 * Purpose:     Print the name of an integer data type.  Common information
 *              like number of bits, byte order, and sign scheme appear on
 *              the first line. Additional information might appear in
 *              parentheses on the following lines.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_int_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    H5T_order_t order;          /* byte order value */
    const char *order_s = NULL; /* byte order string */
    H5T_sign_t  sign;           /* sign scheme value */
    const char *sign_s = NULL;  /* sign scheme string */

    if (H5T_INTEGER != H5Tget_class(type))
        return false;

    /* Byte order */
    if (H5Tget_size(type) > 1) {
        order = H5Tget_order(type);
        if (H5T_ORDER_LE == order) {
            order_s = " little-endian";
        }
        else if (H5T_ORDER_BE == order) {
            order_s = " big-endian";
        }
        else if (H5T_ORDER_VAX == order) {
            order_s = " mixed-endian";
        }
        else {
            order_s = " unknown-byte-order";
        }
    }
    else {
        order_s = "";
    }

    /* Sign */
    if ((sign = H5Tget_sign(type)) >= 0) {
        if (H5T_SGN_NONE == sign) {
            sign_s = " unsigned";
        }
        else if (H5T_SGN_2 == sign) {
            sign_s = "";
        }
        else {
            sign_s = " unknown-sign";
        }
    }
    else {
        sign_s = " unknown-sign";
    }

    /* Print size, order, and sign on first line, precision and padding
     * information on the subsequent lines */
    h5tools_str_append(buffer, "%lu-bit%s%s integer", (unsigned long)(8 * H5Tget_size(type)), order_s,
                       sign_s);
    print_precision(buffer, type, ind);
    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_float_type
 *
 * Purpose:     Print info about a floating point data type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_float_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    H5T_order_t order;          /* byte order value */
    const char *order_s = NULL; /* byte order string */
    size_t      spos;           /* sign bit position */
    size_t      esize, epos;    /* exponent size and position */
    size_t      msize, mpos;    /* significand size and position */
    size_t      ebias;          /* exponent bias */
    H5T_norm_t  norm;           /* significand normalization */
    const char *norm_s = NULL;  /* normalization string */
    H5T_pad_t   pad;            /* internal padding value */
    const char *pad_s = NULL;   /* internal padding string */

    if (H5T_FLOAT != H5Tget_class(type))
        return false;

    /* Byte order */
    if (H5Tget_size(type) > 1) {
        order = H5Tget_order(type);
        if (H5T_ORDER_LE == order) {
            order_s = " little-endian";
        }
        else if (H5T_ORDER_BE == order) {
            order_s = " big-endian";
        }
        else if (H5T_ORDER_VAX == order) {
            order_s = " mixed-endian";
        }
        else {
            order_s = " unknown-byte-order";
        }
    }
    else {
        order_s = "";
    }

    /* Print size and byte order on first line, precision and padding on
     * subsequent lines. */
    h5tools_str_append(buffer, "%lu-bit%s floating-point", (unsigned long)(8 * H5Tget_size(type)), order_s);
    print_precision(buffer, type, ind);

    /* Print sizes, locations, and other information about each field */
    H5Tget_fields(type, &spos, &epos, &esize, &mpos, &msize);
    ebias = H5Tget_ebias(type);
    norm  = H5Tget_norm(type);
    switch (norm) {
        case H5T_NORM_IMPLIED:
            norm_s = ", msb implied";
            break;
        case H5T_NORM_MSBSET:
            norm_s = ", msb always set";
            break;
        case H5T_NORM_NONE:
            norm_s = ", no normalization";
            break;
        case H5T_NORM_ERROR:
            norm_s = ", unknown normalization";
            break;
        default:;
            break;
    }
    h5tools_str_append(buffer, "\n%*s(significant for %lu bit%s at bit %lu%s)", ind, "", (unsigned long)msize,
                       1 == msize ? "" : "s", (unsigned long)mpos, norm_s);
    h5tools_str_append(buffer, "\n%*s(exponent for %lu bit%s at bit %lu, bias is 0x%lx)", ind, "",
                       (unsigned long)esize, 1 == esize ? "" : "s", (unsigned long)epos,
                       (unsigned long)ebias);
    h5tools_str_append(buffer, "\n%*s(sign bit at %lu)", ind, "", (unsigned long)spos);

    /* Display internal padding */
    if ((1 + esize + msize) < H5Tget_precision(type)) {
        pad = H5Tget_inpad(type);
        switch (pad) {
            case H5T_PAD_ZERO:
                pad_s = "zero";
                break;
            case H5T_PAD_ONE:
                pad_s = "one";
                break;
            case H5T_PAD_BACKGROUND:
                pad_s = "bkg";
                break;
            case H5T_PAD_ERROR:
            case H5T_NPAD:
                pad_s = "unknown";
                break;
            default:;
                break;
        }
        h5tools_str_append(buffer, "\n%*s(internal padding bits are %s)", ind, "", pad_s);
    }
    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_cmpd_type
 *
 * Purpose:     Print info about a compound data type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_cmpd_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    char    *name = NULL; /* member name */
    size_t   size;        /* total size of type in bytes */
    hid_t    subtype;     /* member data type */
    int      nmembs;      /* number of members */
    int      n;           /* miscellaneous counters */
    unsigned i;           /* miscellaneous counters */

    if (H5T_COMPOUND != H5Tget_class(type))
        return false;
    if ((nmembs = H5Tget_nmembers(type)) < 0)
        return false;

    h5tools_str_append(buffer, "struct {");
    for (i = 0; i < (unsigned)nmembs; i++) {
        /* Name and offset */
        name = H5Tget_member_name(type, i);
        h5tools_str_append(buffer, "\n%*s\"", ind + 4, "");
        n = print_string(buffer, name, false);
        h5tools_str_append(buffer, "\"%*s +%-4lu ", MAX(0, 16 - n), "",
                           (unsigned long)H5Tget_member_offset(type, i));
        H5free_memory(name);

        /* Member's type */
        subtype = H5Tget_member_type(type, i);
        print_type(buffer, subtype, ind + 4);
        H5Tclose(subtype);
    }
    size = H5Tget_size(type);
    h5tools_str_append(buffer, "\n%*s} %lu byte%s", ind, "", (unsigned long)size, 1 == size ? "" : "s");

    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_enum_type
 *
 * Purpose:     Print info about an enumeration data type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_enum_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    int   nmembs; /* number of members */
    hid_t super;  /* enum base integer type */

    if (H5T_ENUM != H5Tget_class(type))
        return false;
    if ((nmembs = H5Tget_nmembers(type)) < 0)
        return false;

    super = H5Tget_super(type);
    h5tools_str_append(buffer, "enum ");
    print_type(buffer, super, ind + 4);
    h5tools_str_append(buffer, " {");

    if (nmembs > 0) {
        char         **name;                     /* member names */
        unsigned char *value;                    /* value array */
        hid_t          native = H5I_INVALID_HID; /* native integer data type */
        size_t         dst_size;                 /* destination value type size */
        unsigned       i;                        /* miscellaneous counters */

        /* Determine what data type to use for the native values.  To simplify
         * things we entertain three possibilities:
         *  1. long long -- the largest native signed integer
         *  2. unsigned long long -- the largest native unsigned integer
         *  3. raw format */
        if (H5Tget_size(type) <= sizeof(long long)) {
            dst_size = sizeof(long long);
            if (H5T_SGN_NONE == H5Tget_sign(type))
                native = H5T_NATIVE_ULLONG;
            else
                native = H5T_NATIVE_LLONG;
        } /* end if */
        else
            dst_size = H5Tget_size(type);

        /* Get the names and raw values of all members */
        name  = (char **)calloc((size_t)nmembs, sizeof(char *));
        value = (unsigned char *)calloc((size_t)nmembs, MAX(H5Tget_size(type), dst_size));
        for (i = 0; i < (unsigned)nmembs; i++) {
            name[i] = H5Tget_member_name(type, i);
            H5Tget_member_value(type, i, value + i * H5Tget_size(type));
        }

        /* Convert values to native data type */
        if (native > 0)
            if (H5Tconvert(super, native, (size_t)nmembs, value, NULL, H5P_DEFAULT) < 0) {
                /* Release resources */
                for (i = 0; i < (unsigned)nmembs; i++)
                    H5free_memory(name[i]);
                free(name);
                free(value);
                H5Tclose(super);

                return false;
            }

        /* Sort members by increasing value */
        /*not implemented yet*/

        /* Print members */
        for (i = 0; i < (unsigned)nmembs; i++) {
            int nchars; /* number of output characters */

            h5tools_str_append(buffer, "\n%*s", ind + 4, "");
            nchars = print_string(buffer, name[i], true);
            h5tools_str_append(buffer, "%*s = ", MAX(0, 16 - nchars), "");

            if (native < 0) {
                size_t j;

                h5tools_str_append(buffer, "0x");
                for (j = 0; j < dst_size; j++)
                    h5tools_str_append(buffer, "%02x", value[i * dst_size + j]);
            }
            else if (H5T_SGN_NONE == H5Tget_sign(native)) {
                unsigned long long copy;

                memcpy(&copy, value + i * dst_size, sizeof(copy));
                h5tools_str_append(buffer, "%llu", copy);
            }
            else {
                long long copy;

                memcpy(&copy, value + i * dst_size, sizeof(copy));
                h5tools_str_append(buffer, "%lld", copy);
            }
        }

        /* Release resources */
        for (i = 0; i < (unsigned)nmembs; i++)
            H5free_memory(name[i]);
        free(name);
        free(value);
    }
    else
        h5tools_str_append(buffer, "\n%*s <empty>", ind + 4, "");

    h5tools_str_append(buffer, "\n%*s}", ind, "");

    H5Tclose(super);

    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_string_type
 *
 * Purpose:     Print information about a string data type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_string_type(h5tools_str_t *buffer, hid_t type, int H5_ATTR_UNUSED ind)
{
    H5T_str_t   pad;
    const char *pad_s = NULL;
    H5T_cset_t  cset;
    const char *cset_s = NULL;

    if (H5T_STRING != H5Tget_class(type))
        return false;

    /* Padding */
    pad = H5Tget_strpad(type);
    switch (pad) {
        case H5T_STR_NULLTERM:
            pad_s = "null-terminated";
            break;
        case H5T_STR_NULLPAD:
            pad_s = "null-padded";
            break;
        case H5T_STR_SPACEPAD:
            pad_s = "space-padded";
            break;
        case H5T_STR_RESERVED_3:
        case H5T_STR_RESERVED_4:
        case H5T_STR_RESERVED_5:
        case H5T_STR_RESERVED_6:
        case H5T_STR_RESERVED_7:
        case H5T_STR_RESERVED_8:
        case H5T_STR_RESERVED_9:
        case H5T_STR_RESERVED_10:
        case H5T_STR_RESERVED_11:
        case H5T_STR_RESERVED_12:
        case H5T_STR_RESERVED_13:
        case H5T_STR_RESERVED_14:
        case H5T_STR_RESERVED_15:
        case H5T_STR_ERROR:
            pad_s = "unknown-format";
            break;
        default:;
            break;
    }

    /* Character set */
    cset = H5Tget_cset(type);
    switch (cset) {
        case H5T_CSET_ASCII:
            cset_s = "ASCII";
            break;
        case H5T_CSET_UTF8:
            cset_s = "UTF-8";
            break;
        case H5T_CSET_RESERVED_2:
        case H5T_CSET_RESERVED_3:
        case H5T_CSET_RESERVED_4:
        case H5T_CSET_RESERVED_5:
        case H5T_CSET_RESERVED_6:
        case H5T_CSET_RESERVED_7:
        case H5T_CSET_RESERVED_8:
        case H5T_CSET_RESERVED_9:
        case H5T_CSET_RESERVED_10:
        case H5T_CSET_RESERVED_11:
        case H5T_CSET_RESERVED_12:
        case H5T_CSET_RESERVED_13:
        case H5T_CSET_RESERVED_14:
        case H5T_CSET_RESERVED_15:
        case H5T_CSET_ERROR:
            cset_s = "unknown-character-set";
            break;
        default:;
            break;
    }

    if (H5Tis_variable_str(type)) {
        h5tools_str_append(buffer, "variable-length");
    }
    else {
        h5tools_str_append(buffer, "%lu-byte", (unsigned long)H5Tget_size(type));
    }
    h5tools_str_append(buffer, " %s %s string", pad_s, cset_s);
    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_reference_type
 *
 * Purpose:     Prints information about a reference data type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_reference_type(h5tools_str_t *buffer, hid_t type, int H5_ATTR_UNUSED ind)
{
    if (H5T_REFERENCE != H5Tget_class(type))
        return false;

    if (H5Tequal(type, H5T_STD_REF) == true) {
        h5tools_str_append(buffer, "standard reference");
    }
    else if (H5Tequal(type, H5T_STD_REF_OBJ) == true) {
        h5tools_str_append(buffer, "object reference");
    }
    else if (H5Tequal(type, H5T_STD_REF_DSETREG) == true) {
        h5tools_str_append(buffer, "dataset region reference");
    }
    else {
        h5tools_str_append(buffer, "%lu-byte unknown reference", (unsigned long)H5Tget_size(type));
    }

    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_opaque_type
 *
 * Purpose:     Prints information about an opaque data type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_opaque_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    char  *tag;
    size_t size;

    if (H5T_OPAQUE != H5Tget_class(type))
        return false;

    size = H5Tget_size(type);
    h5tools_str_append(buffer, "%lu-byte opaque type", (unsigned long)size);
    if ((tag = H5Tget_tag(type))) {
        h5tools_str_append(buffer, "\n%*s(tag = \"", ind, "");
        print_string(buffer, tag, false);
        h5tools_str_append(buffer, "\")");
        H5free_memory(tag);
    }
    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_vlen_type
 *
 * Purpose:     Print information about a variable-length type
 *
 * Return:      Success:        true
 *              Failure:        false
 *-------------------------------------------------------------------------
 */
static bool
print_vlen_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    hid_t super;

    if (H5T_VLEN != H5Tget_class(type))
        return false;

    h5tools_str_append(buffer, "variable length of\n%*s", ind + 4, "");
    super = H5Tget_super(type);
    print_type(buffer, super, ind + 4);
    H5Tclose(super);
    return true;
}

/*---------------------------------------------------------------------------
 * Purpose:     Print information about an array type
 *
 * Return:      Success:        true
 *              Failure:        false
 *---------------------------------------------------------------------------
 */
static bool
print_array_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    hid_t    super;
    int      ndims, i;
    hsize_t *dims = NULL;

    if (H5T_ARRAY != H5Tget_class(type))
        return false;
    ndims = H5Tget_array_ndims(type);
    if (ndims) {
        dims = (hsize_t *)malloc((unsigned)ndims * sizeof(dims[0]));
        H5Tget_array_dims2(type, dims);

        /* Print dimensions */
        for (i = 0; i < ndims; i++)
            h5tools_str_append(buffer, "%s%" PRIuHSIZE, i ? "," : "[", dims[i]);
        h5tools_str_append(buffer, "]");

        free(dims);
    }
    else
        h5tools_str_append(buffer, " [SCALAR]\n");

    /* Print parent type */
    h5tools_str_append(buffer, " ");
    super = H5Tget_super(type);
    print_type(buffer, super, ind + 4);
    H5Tclose(super);
    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_bitfield_type
 *
 * Purpose:     Print information about a bitfield type.
 *
 * Return:      Success: true
 *              Failure: false, nothing printed
 *-------------------------------------------------------------------------
 */
static bool
print_bitfield_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    H5T_order_t order;          /* byte order value */
    const char *order_s = NULL; /* byte order string */

    if (H5T_BITFIELD != H5Tget_class(type))
        return false;
    if (H5Tget_size(type) > 1) {
        order = H5Tget_order(type);
        if (H5T_ORDER_LE == order) {
            order_s = " little-endian";
        }
        else if (H5T_ORDER_BE == order) {
            order_s = " big-endian";
        }
        else if (H5T_ORDER_VAX == order) {
            order_s = " mixed-endian";
        }
        else {
            order_s = "unknown-byte-order";
        }
    }
    else {
        order_s = "";
    }

    h5tools_str_append(buffer, "%lu-bit%s bitfield", (unsigned long)(8 * H5Tget_size(type)), order_s);
    print_precision(buffer, type, ind);
    return true;
}

/*-------------------------------------------------------------------------
 * Function:    print_type
 *
 * Purpose:     Prints a data type definition.  The definition is printed
 *              without any leading space or trailing line-feed (although
 *              there might be line-feeds inside the type definition).  The
 *              first line is assumed to have IND characters before it on
 *              the same line (printed by the caller).
 *              Prints the OID of shared data types.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
print_type(h5tools_str_t *buffer, hid_t type, int ind)
{
    H5T_class_t data_class = H5Tget_class(type);

    /* Bad data type */
    if (type < 0) {
        h5tools_str_append(buffer, "<ERROR>");
        return;
    }

    /* Shared? If so then print the type's OID */
    if (H5Tcommitted(type)) {
        H5O_info2_t oi;

        if (H5Oget_info3(type, &oi, H5O_INFO_BASIC) >= 0) {
            char *type_string = NULL;

            H5Otoken_to_str(type, &oi.token, &type_string);

            h5tools_str_append(buffer, "shared-%lu:%s", oi.fileno, type_string);

            H5free_memory(type_string);
        } /* end if */
        else
            h5tools_str_append(buffer, "shared ");
    } /* end if */

    /* Print the type */
    if (print_native_type(buffer, type, ind) || print_ieee_type(buffer, type, ind) ||
        print_cmpd_type(buffer, type, ind) || print_enum_type(buffer, type, ind) ||
        print_string_type(buffer, type, ind) || print_reference_type(buffer, type, ind) ||
        print_vlen_type(buffer, type, ind) || print_array_type(buffer, type, ind) ||
        print_opaque_type(buffer, type, ind) || print_bitfield_type(buffer, type, ind))
        return;

    /* Unknown type */
    h5tools_str_append(buffer, "%lu-byte class-%u unknown", (unsigned long)H5Tget_size(type),
                       (unsigned)data_class);
}

/*-------------------------------------------------------------------------
 * Function:    dump_dataset_values
 *
 * Purpose:     Prints all values of a dataset.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
dump_dataset_values(hid_t dset)
{
    hid_t             f_type = H5I_INVALID_HID;
    hid_t             space  = H5I_INVALID_HID;
    hsize_t           total_size[H5S_MAX_RANK];
    int               ndims;
    char              string_prefix[64];
    static char       fmt_double[16];
    static char       fmt_float[16];
    hsize_t           curr_pos = 0; /* total data element position   */
    h5tools_str_t     buffer;       /* string into which to render   */
    h5tools_context_t ctx;          /* print context  */
    h5tool_format_t   outputformat;
    h5tool_format_t  *info    = &ls_dataformat;
    H5R_ref_t        *ref_buf = NULL;

    H5TOOLS_START_DEBUG(" ");

    f_type = H5Dget_type(dset);
    space  = H5Dget_space(dset);

    memset(&ctx, 0, sizeof(ctx));
    memset(&buffer, 0, sizeof(h5tools_str_t));

    outputformat          = *info;
    outputformat.line_1st = NULL;
    outputformat.idx_fmt  = "";
    if (simple_output_g) {
        outputformat.idx_fmt        = "";
        outputformat.line_per_line  = 1;
        outputformat.line_multi_new = 0;
        outputformat.line_pre       = "        ";
        outputformat.line_cont      = "         ";

        outputformat.arr_pre = "";
        outputformat.arr_suf = "";
        outputformat.arr_sep = " ";

        if (!label_g) {
            outputformat.cmpd_pre = "";
            outputformat.cmpd_suf = "";
        }
        outputformat.cmpd_sep = " ";

        if (label_g)
            outputformat.cmpd_name = "%s=";

        outputformat.elmt_suf1  = " ";
        outputformat.str_locale = ESCAPE_HTML;
    }
    else {
        if (no_line_wrap_g) {
            outputformat.line_per_line = 1;
        }
        else {
            outputformat.line_ncols = (unsigned)width_g;
        }
        if (label_g)
            outputformat.cmpd_name = "%s=";
        outputformat.line_pre   = "        %s ";
        outputformat.line_cont  = "        %s ";
        outputformat.str_repeat = 8;

        outputformat.arr_pre = NULL;
        outputformat.arr_suf = NULL;
        outputformat.arr_sep = NULL;

        outputformat.cmpd_pre = NULL;
        outputformat.cmpd_suf = NULL;
        outputformat.cmpd_sep = NULL;

        outputformat.vlen_sep = NULL;
        outputformat.vlen_pre = NULL;
        outputformat.vlen_suf = NULL;
        outputformat.vlen_end = NULL;
    }
    outputformat.arr_linebreak = 0;
    /* Floating point types should display full precision */
    snprintf(fmt_float, sizeof(fmt_float), "%%1.%dg", FLT_DIG);
    outputformat.fmt_float = fmt_float;
    snprintf(fmt_double, sizeof(fmt_double), "%%1.%dg", DBL_DIG);
    outputformat.fmt_double = fmt_double;

    if (hexdump_g) {
        /* Print all data in hexadecimal format if the `-x' or `--hexdump'
         * command line switch was given. */
        outputformat.raw = true;
    }
    else if (string_g && H5Tget_size(f_type) == 1 && (H5Tget_class(f_type) == H5T_INTEGER)) {
        /* Print 1-byte integer data as an ASCI character string instead of
         * integers if the `-s' or `--string' command-line option was given. */
        outputformat.ascii     = true;
        outputformat.elmt_suf1 = "";
        outputformat.elmt_suf2 = "";
        snprintf(string_prefix, sizeof(string_prefix), "%s\"", outputformat.line_pre);
        outputformat.line_pre = string_prefix;
        outputformat.line_suf = "\"";
    }
    info = &outputformat;

    ctx.indent_level = 1;
    ctx.cur_column   = (size_t)curr_pos;
    /* Print all the values. */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "    Data:\n");

    h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols, (hsize_t)0,
                           (hsize_t)0);

    ctx.need_prefix = true;
    ctx.cur_column  = (size_t)curr_pos;
    if (H5Tget_class(f_type) == H5T_REFERENCE) {
        H5TOOLS_DEBUG("reference class type");
        if (!H5Tequal(f_type, H5T_STD_REF) && !H5Tequal(f_type, H5T_STD_REF_DSETREG) &&
            !H5Tequal(f_type, H5T_STD_REF_OBJ)) {
            H5TOOLS_GOTO_DONE_NO_RET();
        }

        ndims = (int)H5Sget_simple_extent_npoints(space);
        H5TOOLS_DEBUG("ndims=%d - ctx.ndims=%d", ndims, ctx.ndims);

        /* Assume entire data space to be printed */
        H5Sget_simple_extent_dims(space, total_size, NULL);
        init_acc_pos(ctx.ndims, total_size, ctx.acc, ctx.pos, ctx.p_min_idx);

        ctx.need_prefix = true;

        if (NULL !=
            (ref_buf = (H5R_ref_t *)calloc(MAX(sizeof(unsigned), sizeof(H5R_ref_t)), (size_t)ndims))) {
            H5TOOLS_DEBUG("H5Dread reference read");
            if (H5Dread(dset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_buf) < 0) {
                free(ref_buf);
                H5TOOLS_INFO("H5Dread reference failed");
                H5TOOLS_GOTO_DONE_NO_RET();
            }
            h5tools_dump_reference(rawoutstream, info, &ctx, dset, ref_buf, ndims);

            PRINTVALSTREAM(rawoutstream, "\n");
            free(ref_buf);
        }
    }
    else {
        if (h5tools_dump_dset(rawoutstream, info, &ctx, dset) < 0) {
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "        Unable to print data.");
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);
        }
    }
done:
    H5Sclose(space);
    H5Tclose(f_type);

    h5tools_str_close(&buffer);

    PRINTVALSTREAM(rawoutstream, "\n");

    H5TOOLS_ENDDEBUG(" ");
}

/*-------------------------------------------------------------------------
 * Function:    dump_attribute_values
 *
 * Purpose:     Prints all values of a attribute.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
dump_attribute_values(hid_t attr)
{
    hid_t             f_type = H5I_INVALID_HID;
    hid_t             space  = H5I_INVALID_HID;
    hsize_t           total_size[H5S_MAX_RANK];
    int               ndims;
    char              string_prefix[64];
    static char       fmt_double[16];
    static char       fmt_float[16];
    hsize_t           curr_pos = 0; /* total data element position   */
    h5tools_str_t     buffer;       /* string into which to render   */
    h5tools_context_t ctx;          /* print context  */
    h5tool_format_t   outputformat;
    h5tool_format_t  *info    = &ls_dataformat;
    H5R_ref_t        *ref_buf = NULL;

    H5TOOLS_START_DEBUG(" ");

    f_type = H5Aget_type(attr);
    space  = H5Aget_space(attr);

    memset(&ctx, 0, sizeof(ctx));
    memset(&buffer, 0, sizeof(h5tools_str_t));

    outputformat          = *info;
    outputformat.line_1st = NULL;
    outputformat.idx_fmt  = "";
    if (simple_output_g) {
        outputformat.idx_fmt        = "";
        outputformat.line_per_line  = 1;
        outputformat.line_multi_new = 0;
        outputformat.line_pre       = "        ";
        outputformat.line_cont      = "        ";

        outputformat.arr_pre = "";
        outputformat.arr_suf = "";
        outputformat.arr_sep = " ";

        if (!label_g) {
            outputformat.cmpd_pre = "";
            outputformat.cmpd_suf = "";
        }
        outputformat.cmpd_sep = " ";

        if (label_g)
            outputformat.cmpd_name = "%s=";

        outputformat.elmt_suf1  = " ";
        outputformat.str_locale = ESCAPE_HTML;
    }
    else {
        if (no_line_wrap_g) {
            outputformat.line_per_line = 1;
        }
        else {
            outputformat.line_ncols = (unsigned)width_g;
        }
        if (label_g)
            outputformat.cmpd_name = "%s=";
        outputformat.line_pre   = "        %s ";
        outputformat.line_cont  = "        %s  ";
        outputformat.str_repeat = 8;

        outputformat.arr_pre = NULL;
        outputformat.arr_suf = NULL;
        outputformat.arr_sep = NULL;

        outputformat.cmpd_pre = NULL;
        outputformat.cmpd_suf = NULL;
        outputformat.cmpd_sep = NULL;

        outputformat.vlen_sep = NULL;
        outputformat.vlen_pre = NULL;
        outputformat.vlen_suf = NULL;
        outputformat.vlen_end = NULL;
    }
    outputformat.arr_linebreak = 0;
    /* Floating point types should display full precision */
    snprintf(fmt_float, sizeof(fmt_float), "%%1.%dg", FLT_DIG);
    outputformat.fmt_float = fmt_float;
    snprintf(fmt_double, sizeof(fmt_double), "%%1.%dg", DBL_DIG);
    outputformat.fmt_double = fmt_double;

    if (hexdump_g) {
        /* Print all data in hexadecimal format if the `-x' or `--hexdump'
         * command line switch was given. */
        outputformat.raw = true;
    }
    else if (string_g && H5Tget_size(f_type) == 1 && (H5Tget_class(f_type) == H5T_INTEGER)) {
        /* Print 1-byte integer data as an ASCI character string instead of
         * integers if the `-s' or `--string' command-line option was given. */
        outputformat.ascii     = true;
        outputformat.elmt_suf1 = "";
        outputformat.elmt_suf2 = "";
        snprintf(string_prefix, sizeof(string_prefix), "%s\"", outputformat.line_pre);
        outputformat.line_pre = string_prefix;
        outputformat.line_suf = "\"";
    }
    info = &outputformat;

    ctx.indent_level = 2;
    ctx.cur_column   = (size_t)curr_pos;

    /* Print all the values. */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "        Data:\n");

    h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols, (hsize_t)0,
                           (hsize_t)0);

    ctx.need_prefix = true;
    ctx.cur_column  = (size_t)curr_pos;
    if (H5Tget_class(f_type) == H5T_REFERENCE) {
        H5TOOLS_DEBUG("reference class type");
        if (!H5Tequal(f_type, H5T_STD_REF) && !H5Tequal(f_type, H5T_STD_REF_DSETREG) &&
            !H5Tequal(f_type, H5T_STD_REF_OBJ)) {
            H5TOOLS_GOTO_DONE_NO_RET();
        }

        ndims = (int)H5Sget_simple_extent_npoints(space);
        H5TOOLS_DEBUG("ndims=%d - ctx.ndims=%d", ndims, ctx.ndims);

        /* Assume entire data space to be printed */
        H5Sget_simple_extent_dims(space, total_size, NULL);
        init_acc_pos(ctx.ndims, total_size, ctx.acc, ctx.pos, ctx.p_min_idx);

        ctx.need_prefix = true;

        if (NULL !=
            (ref_buf = (H5R_ref_t *)calloc(MAX(sizeof(unsigned), sizeof(H5R_ref_t)), (size_t)ndims))) {
            H5TOOLS_DEBUG("H5Aread reference read");
            if (H5Aread(attr, H5T_STD_REF, ref_buf) < 0) {
                free(ref_buf);
                H5TOOLS_INFO("H5Aread reference failed");
                H5TOOLS_GOTO_DONE_NO_RET();
            }
            ctx.indent_level++;
            h5tools_dump_reference(rawoutstream, info, &ctx, attr, ref_buf, ndims);

            PRINTVALSTREAM(rawoutstream, "\n");
            ctx.indent_level--;
            free(ref_buf);
        }
    }
    else {
        H5TOOLS_DEBUG("Attribute data read");
        ctx.indent_level++;
        if (h5tools_dump_mem(rawoutstream, info, &ctx, attr) < 0) {
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "        Unable to print data.");
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);
        }
        ctx.indent_level--;
        H5TOOLS_DEBUG("Attribute data read complete");
    }
done:
    H5Sclose(space);
    H5Tclose(f_type);

    h5tools_str_close(&buffer);

    PRINTVALSTREAM(rawoutstream, "\n");

    H5TOOLS_ENDDEBUG(" ");
}

/*-------------------------------------------------------------------------
 * Function:    list_attr
 *
 * Purpose:     Prints information about attributes.
 *
 * Return:      Success: 0
 *              Failure: -1
 *-------------------------------------------------------------------------
 */
static herr_t
list_attr(hid_t obj, const char *attr_name, const H5A_info_t H5_ATTR_UNUSED *ainfo,
          void H5_ATTR_UNUSED *op_data)
{
    hid_t             attr  = H5I_INVALID_HID;
    hid_t             space = H5I_INVALID_HID;
    hid_t             type  = H5I_INVALID_HID;
    hsize_t           size[H5S_MAX_RANK];
    int               ndims;
    int               i;
    H5S_class_t       space_type;
    hsize_t           curr_pos = 0; /* total data element position   */
    h5tools_str_t     buffer;       /* string into which to render   */
    h5tools_context_t ctx;          /* print context  */
    h5tool_format_t  *info = &ls_dataformat;

    H5TOOLS_START_DEBUG(" ");

    memset(&ctx, 0, sizeof(ctx));
    memset(&buffer, 0, sizeof(h5tools_str_t));

    ctx.indent_level = 2;
    ctx.cur_column   = (size_t)curr_pos;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "    Attribute: ");

    print_string(&buffer, attr_name, true);

    H5TOOLS_DEBUG("Attribute name:%s", attr_name);
    if ((attr = H5Aopen(obj, attr_name, H5P_DEFAULT)) >= 0) {
        space = H5Aget_space(attr);
        type  = H5Aget_type(attr);

        /* Data space */
        ndims      = H5Sget_simple_extent_dims(space, size, NULL);
        space_type = H5Sget_simple_extent_type(space);
        H5TOOLS_DEBUG("Attribute ndims:%d", ndims);
        switch (space_type) {
            case H5S_SCALAR:
                /* scalar dataspace */
                h5tools_str_append(&buffer, " scalar\n");
                h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                       (hsize_t)0, (hsize_t)0);
                break;

            case H5S_SIMPLE:
                /* simple dataspace */
                h5tools_str_append(&buffer, " {");
                for (i = 0; i < ndims; i++)
                    h5tools_str_append(&buffer, "%s%" PRIuHSIZE, i ? ", " : "", size[i]);
                h5tools_str_append(&buffer, "}\n");
                h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                       (hsize_t)0, (hsize_t)0);
                break;

            case H5S_NULL:
                /* null dataspace */
                h5tools_str_append(&buffer, " null\n");
                h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                       (hsize_t)0, (hsize_t)0);
                break;

            case H5S_NO_CLASS:
            default:
                /* Unknown dataspace type */
                h5tools_str_append(&buffer, " unknown\n");
                h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                       (hsize_t)0, (hsize_t)0);
                break;
        } /* end switch */

        /* Data type */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "        %-10s ", "Type:");
        print_type(&buffer, type, 15);
        h5tools_str_append(&buffer, "\n");
        h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                               (hsize_t)0, (hsize_t)0);

        H5Sclose(space);
        H5Tclose(type);

        h5tools_str_close(&buffer);

        if (data_g)
            dump_attribute_values(attr);
        H5Aclose(attr);
    }
    else {
        H5TOOLS_DEBUG("Attribute open failed");
        h5tools_str_close(&buffer);
    }
    H5TOOLS_ENDDEBUG(" ");

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    dataset_list1
 *
 * Purpose:     List information about a dataset which should appear on the
 *              same line as the dataset name.  This information will precede
 *              information which is applicable to all objects which will be
 *              printed by the caller.
 *
 * Return:      Success: 0
 *              Failure: -1
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_list1(hid_t dset)
{
    hsize_t           cur_size[H5S_MAX_RANK]; /* current dataset dimensions */
    hsize_t           max_size[H5S_MAX_RANK]; /* maximum dataset dimensions */
    hid_t             space;                  /* data space                 */
    int               ndims;                  /* dimensionality             */
    H5S_class_t       space_type;             /* type of dataspace          */
    int               i;
    hsize_t           curr_pos = 0; /* total data element position   */
    h5tools_str_t     buffer;       /* string into which to render   */
    h5tools_context_t ctx;          /* print context  */
    h5tool_format_t  *info = &ls_dataformat;

    memset(&ctx, 0, sizeof(ctx));
    memset(&buffer, 0, sizeof(h5tools_str_t));

    h5tools_str_reset(&buffer);

    /* Information that goes on the same row as the name.  The name has
     * already been printed. */
    space      = H5Dget_space(dset);
    space_type = H5Sget_simple_extent_type(space);
    ndims      = H5Sget_simple_extent_dims(space, cur_size, max_size);
    h5tools_str_append(&buffer, " {");
    for (i = 0; i < ndims; i++) {
        h5tools_str_append(&buffer, "%s%" PRIuHSIZE, i ? ", " : "", cur_size[i]);
        if (max_size[i] == H5S_UNLIMITED) {
            h5tools_str_append(&buffer, "/%s", "Inf");
        }
        else if (max_size[i] != cur_size[i] || verbose_g > 0) {
            h5tools_str_append(&buffer, "/%" PRIuHSIZE, max_size[i]);
        }
    }
    if (space_type == H5S_SCALAR)
        h5tools_str_append(&buffer, "SCALAR");
    else if (space_type == H5S_NULL)
        h5tools_str_append(&buffer, "NULL");
    h5tools_str_append(&buffer, "}");
    h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols, (hsize_t)0,
                           (hsize_t)0);
    H5Sclose(space);

    h5tools_str_close(&buffer);

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    dataset_list2
 *
 * Purpose:     List information about a dataset which should appear after
 *              information which is general to all objects.
 *
 * Return:      Success: 0
 *              Failure: -1
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_list2(hid_t dset, const char H5_ATTR_UNUSED *name)
{
    hid_t             dcpl;          /* dataset creation property list */
    hid_t             type;          /* data type of dataset */
    hid_t             space;         /* data space of dataset */
    int               nf;            /* number of filters */
    unsigned          filt_flags;    /* filter flags */
    H5Z_filter_t      filt_id;       /* filter identification number */
    unsigned          cd_values[20]; /* filter client data values */
    size_t            cd_nelmts;     /* filter client number of values */
    size_t            cd_num;        /* filter client data counter */
    char              f_name[256];   /* filter/file name */
    char              s[64];         /* temporary string buffer */
    off_t             f_offset;      /* offset in external file */
    hsize_t           f_size;        /* bytes used in external file */
    hsize_t           total, used;   /* total size or offset */
    int               ndims;         /* dimensionality */
    int               n, max_len;    /* max extern file name length */
    double            utilization;   /* percent utilization of storage */
    H5T_class_t       tclass;        /* datatype class identifier */
    int               i;
    H5D_layout_t      stl;
    hsize_t           curr_pos = 0; /* total data element position   */
    h5tools_str_t     buffer;       /* string into which to render   */
    h5tools_context_t ctx;          /* print context  */
    h5tool_format_t  *info = &ls_dataformat;

    memset(&ctx, 0, sizeof(ctx));
    memset(&buffer, 0, sizeof(h5tools_str_t));

    h5tools_str_reset(&buffer);

    if (verbose_g > 0) {
        dcpl  = H5Dget_create_plist(dset);
        space = H5Dget_space(dset);
        type  = H5Dget_type(dset);

        stl = H5Pget_layout(dcpl);
        switch (stl) {
            case H5D_CHUNKED: {
                hsize_t chsize[64]; /* chunk size in elements */

                ndims = H5Pget_chunk(dcpl, (int)NELMTS(chsize), chsize /*out*/);
                h5tools_str_append(&buffer, "    %-10s {", "Chunks:");
                total = H5Tget_size(type);
                for (i = 0; i < ndims; i++) {
                    h5tools_str_append(&buffer, "%s%" PRIuHSIZE, i ? ", " : "", chsize[i]);
                    total *= chsize[i];
                }
                h5tools_str_append(&buffer, "} %" PRIuHSIZE " bytes\n", total);
            } break;
            case H5D_COMPACT:
                break;
            case H5D_CONTIGUOUS:
                /* Print information about external storage */
                if ((nf = H5Pget_external_count(dcpl)) > 0) {
                    for (i = 0, max_len = 0; i < nf; i++) {
                        if (H5Pget_external(dcpl, (unsigned)i, sizeof(f_name), f_name, NULL, NULL) < 0)
                            continue;
                        n       = print_string(NULL, f_name, true);
                        max_len = MAX(max_len, n);
                    } /* end for */
                    h5tools_str_append(&buffer, "    %-10s %d external file%s\n", "Extern:", nf,
                                       1 == nf ? "" : "s");
                    h5tools_str_append(&buffer, "        %4s %10s %10s %10s %s\n", "ID", "DSet-Addr",
                                       "File-Addr", "Bytes", "File");
                    h5tools_str_append(&buffer, "        %4s %10s %10s %10s ", "----", "----------",
                                       "----------", "----------");
                    for (i = 0; i < max_len; i++)
                        h5tools_str_append(&buffer, "-");
                    h5tools_str_append(&buffer, "\n");
                    for (i = 0, total = 0; i < nf; i++) {
                        if (H5Pget_external(dcpl, (unsigned)i, sizeof(f_name), f_name, &f_offset, &f_size) <
                            0) {
                            h5tools_str_append(
                                &buffer, "        #%03d %10" PRIuHSIZE " %10s %10s ***ERROR*** %s\n", i,
                                total, "", "", i + 1 < nf ? "Following addresses are incorrect" : "");
                        }
                        else if (H5S_UNLIMITED == f_size) {
                            h5tools_str_append(&buffer,
                                               "        #%03d %10" PRIuHSIZE " %10" PRIuHSIZE " %10s ", i,
                                               total, (hsize_t)f_offset, "INF");
                            print_string(&buffer, f_name, true);
                        }
                        else {
                            h5tools_str_append(
                                &buffer, "        #%03d %10" PRIuHSIZE " %10" PRIuHSIZE " %10" PRIuHSIZE " ",
                                i, total, (hsize_t)f_offset, f_size);
                            print_string(&buffer, f_name, true);
                        }
                        h5tools_str_append(&buffer, "\n");
                        total += f_size;
                    }
                    h5tools_str_append(&buffer, "        %4s %10s %10s %10s ", "----", "----------",
                                       "----------", "----------");
                    for (i = 0; i < max_len; i++)
                        h5tools_str_append(&buffer, "-");
                    h5tools_str_append(&buffer, "\n");
                } /* end if */
                break;

            case H5D_VIRTUAL: {
                char   dset_name[256]; /* Dataset name */
                size_t vmaps;

                H5Pget_virtual_count(dcpl, &vmaps);

                if (vmaps) {
                    size_t next;

                    h5tools_str_append(&buffer, "    %-10s {%zu} Source {\n", "Maps:", vmaps);
                    for (next = 0; next < (unsigned)vmaps; next++) {
                        H5Pget_virtual_filename(dcpl, next, f_name, sizeof(f_name));
                        H5Pget_virtual_dsetname(dcpl, next, dset_name, sizeof(dset_name));
                        h5tools_str_append(&buffer, "    %-10s        ", " ");
                        print_string(&buffer, f_name, true);
                        h5tools_str_append(&buffer, "   ");
                        print_string(&buffer, dset_name, true);
                        h5tools_str_append(&buffer, "\n");
                    }
                    h5tools_str_append(&buffer, "     %-10s}\n", " ");
                }
            } break;

            case H5D_LAYOUT_ERROR:
            case H5D_NLAYOUTS:
            default:
                h5tools_str_append(&buffer, "layout information not available");
                break;
        }
        /* Print total raw storage size */
        total  = (hsize_t)H5Sget_simple_extent_npoints(space) * H5Tget_size(type);
        used   = H5Dget_storage_size(dset);
        tclass = H5Tget_class(type);
        h5tools_str_append(&buffer, "    %-10s ", "Storage:");
        switch (tclass) {
            case H5T_VLEN:
                h5tools_str_append(&buffer, "information not available");
                break;

            case H5T_REFERENCE:
                if (H5Tequal(dset, H5T_STD_REF)) {
                    h5tools_str_append(&buffer, "reference information not available");
                }
                else if (H5Tequal(dset, H5T_STD_REF_DSETREG)) {
                    h5tools_str_append(&buffer, "information not available");
                }
                break;

            case H5T_NO_CLASS:
            case H5T_INTEGER:
            case H5T_FLOAT:
            case H5T_TIME:
            case H5T_STRING:
            case H5T_BITFIELD:
            case H5T_OPAQUE:
            case H5T_COMPOUND:
            case H5T_ENUM:
            case H5T_ARRAY:
            case H5T_NCLASSES:
            default:
                h5tools_str_append(&buffer, "%" PRIuHSIZE " logical byte%s, %" PRIuHSIZE " allocated byte%s",
                                   total, 1 == total ? "" : "s", used, 1 == used ? "" : "s");
                if (used > 0) {
                    utilization = ((double)total * 100.0) / (double)used;
                    h5tools_str_append(&buffer, ", %1.2f%% utilization", utilization);
                }
        }

        h5tools_str_append(&buffer, "\n");

        /* Print information about raw data filters */
        if ((nf = H5Pget_nfilters(dcpl)) > 0) {
            for (i = 0; i < nf; i++) {
                cd_nelmts = NELMTS(cd_values);
                filt_id   = H5Pget_filter2(dcpl, (unsigned)i, &filt_flags, &cd_nelmts, cd_values,
                                           sizeof(f_name), f_name, NULL);
                f_name[sizeof(f_name) - 1] = '\0';
                snprintf(s, sizeof(s), "Filter-%d:", i);
                h5tools_str_append(&buffer, "    %-10s %s-%u %s {", s, (f_name[0] ? f_name : "method"),
                                   (unsigned)filt_id, ((filt_flags & H5Z_FLAG_OPTIONAL) ? "OPT" : ""));
                for (cd_num = 0; cd_num < cd_nelmts; cd_num++)
                    h5tools_str_append(&buffer, "%s%u", (cd_num ? ", " : ""), cd_values[cd_num]);
                h5tools_str_append(&buffer, "}\n");
            } /* end for */
        }     /* end if */
        h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                               (hsize_t)0, (hsize_t)0);

        /* Print data type */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "    %-10s ", "Type:");
        print_type(&buffer, type, 15);
        h5tools_str_append(&buffer, "\n");
        h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                               (hsize_t)0, (hsize_t)0);

        /* Print address information */
        if (address_g)
            H5Ddebug(dset);

        /* Close stuff */
        H5Tclose(type);
        H5Sclose(space);
        H5Pclose(dcpl);
    } /* end if */

    h5tools_str_close(&buffer);

    if (data_g)
        dump_dataset_values(dset);

    return 0;
} /* end dataset_list2() */

/*-------------------------------------------------------------------------
 * Function:    datatype_list2
 *
 * Purpose:     List information about a datatype which should appear after
 *              information which is general to all objects.
 *
 * Return:      Success: 0
 *              Failure: -1
 *-------------------------------------------------------------------------
 */
static herr_t
datatype_list2(hid_t type, const char H5_ATTR_UNUSED *name)
{
    if (verbose_g > 0) {
        hsize_t           curr_pos = 0; /* total data element position   */
        h5tools_str_t     buffer;       /* string into which to render   */
        h5tools_context_t ctx;          /* print context  */
        h5tool_format_t  *info = &ls_dataformat;

        memset(&ctx, 0, sizeof(ctx));
        memset(&buffer, 0, sizeof(h5tools_str_t));

        h5tools_str_reset(&buffer);

        h5tools_str_append(&buffer, "    %-10s ", "Type:");
        print_type(&buffer, type, 15);
        h5tools_str_append(&buffer, "\n");
        h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                               (hsize_t)0, (hsize_t)0);

        h5tools_str_close(&buffer);
    }
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    list_obj
 *
 * Purpose:     Prints information about an object
 *
 * Return:      Success: 0
 *              Failure: -1
 *-------------------------------------------------------------------------
 */
static herr_t
list_obj(const char *name, const H5O_info2_t *oinfo, const char *first_seen, void *_iter)
{
    H5O_type_t        obj_type = oinfo->type; /* Type of the object */
    iter_t           *iter     = (iter_t *)_iter;
    hsize_t           curr_pos = 0; /* total data element position   */
    h5tools_str_t     buffer;       /* string into which to render   */
    h5tools_context_t ctx;          /* print context  */
    h5tool_format_t  *info = &ls_dataformat;

    H5TOOLS_START_DEBUG(" ");

    memset(&ctx, 0, sizeof(ctx));
    memset(&buffer, 0, sizeof(h5tools_str_t));

    h5tools_str_reset(&buffer);

    H5TOOLS_DEBUG("Object name:%s", name);
    /* Print the link's name, either full name or base name */
    if (!iter->symlink_target)
        print_obj_name(&buffer, iter, name, "");

    /* Check object information */
    if (oinfo->type < 0 || oinfo->type >= H5O_TYPE_NTYPES) {
        h5tools_str_append(&buffer, "Unknown type(%d)", (int)oinfo->type);
        obj_type = H5O_TYPE_UNKNOWN;
    }
    if (iter->symlink_target)
        h5tools_str_append(&buffer, "{");
    if (obj_type >= 0 && dispatch_g[obj_type].name)
        h5tools_str_append(&buffer, "%s", dispatch_g[obj_type].name);
    h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols, (hsize_t)0,
                           (hsize_t)0);

    /* Check if we've seen this object before */
    if (first_seen) {
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, ", same as ");
        print_string(&buffer, first_seen, true);
        if (!iter->symlink_target) {
            h5tools_str_append(&buffer, "\n");
        }
        h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                               (hsize_t)0, (hsize_t)0);
    } /* end if */
    else {
        hid_t obj_id = H5I_INVALID_HID; /* ID of object opened */

        /* Open the object.  Not all objects can be opened.  If this is the case
         * then return right away.
         */
        H5TOOLS_DEBUG("Open object name=%s", name);
        if (obj_type >= 0 && (obj_id = H5Oopen(iter->fid, name, H5P_DEFAULT)) < 0) {
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, " *ERROR*\n");
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);
            goto done;
        } /* end if */

        /* List the first line of information for the object. */
        H5TOOLS_DEBUG("Object type:%d", obj_type);
        if (obj_type >= 0 && dispatch_g[obj_type].list1)
            (dispatch_g[obj_type].list1)(obj_id);
        if (!iter->symlink_target || (verbose_g > 0)) {
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\n");
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);
        }

        /* Show detailed information about the object, beginning with information
         * which is common to all objects. */
        if (verbose_g > 0) {
            size_t   buf_size    = 0;
            char    *comment     = NULL;
            char    *obj_tok_str = NULL;
            ssize_t  cmt_bufsize = -1;
            uint64_t supported   = 0;

            /* Display attributes */
            H5TOOLS_DEBUG("Display attributes");
            if (obj_type >= 0)
                H5Aiterate2(obj_id, H5_INDEX_NAME, H5_ITER_INC, NULL, list_attr, NULL);

            /* Object location & reference count */
            H5Otoken_to_str(obj_id, &oinfo->token, &obj_tok_str);

            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "    %-10s %lu:%s\n", "Location:", oinfo->fileno, obj_tok_str);
            h5tools_str_append(&buffer, "    %-10s %u\n", "Links:", (unsigned)oinfo->rc);
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);

            H5free_memory(obj_tok_str);

            /* Modification time */
            if (oinfo->mtime > 0) {
                char       buf[256];
                struct tm *tm;

                if (simple_output_g)
                    tm = HDgmtime(&(oinfo->mtime));
                else
                    tm = HDlocaltime(&(oinfo->mtime));
                if (tm) {
                    strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "    %-10s %s\n", "Modified:", buf);
                    h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos,
                                           (size_t)info->line_ncols, (hsize_t)0, (hsize_t)0);
                } /* end if */
            }     /* end if */

            /* Only emit comments if the VOL connector supports that */
            H5VLquery_optional(obj_id, H5VL_SUBCLS_OBJECT, H5VL_NATIVE_OBJECT_GET_COMMENT, &supported);

            if (supported & H5VL_OPT_QUERY_SUPPORTED) {

                /* Object comment */
                cmt_bufsize = H5Oget_comment(obj_id, comment, buf_size);

                /* if the actual length of the comment is longer than cmt_bufsize, then call
                 * H5Oget_comment again with the correct value.
                 */
                if (cmt_bufsize > 0) {
                    comment =
                        (char *)malloc((size_t)cmt_bufsize + 1); /* new_size including null terminator */
                    if (comment) {
                        cmt_bufsize = H5Oget_comment(obj_id, comment, (size_t)cmt_bufsize);
                        if (cmt_bufsize > 0) {
                            comment[cmt_bufsize] = 0;
                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, "    %-10s \"", "Comment:");
                            print_string(&buffer, comment, false);
                            h5tools_str_append(&buffer, "\"\n");
                            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos,
                                                   (size_t)info->line_ncols, (hsize_t)0, (hsize_t)0);
                        } /* end if */
                        free(comment);
                    }
                }
            }
        } /* end if */

        /* Detailed list for object */
        if (obj_type >= 0 && dispatch_g[obj_type].list2)
            (dispatch_g[obj_type].list2)(obj_id, name);

        /* Close the object. */
        if (obj_type >= 0)
            H5Oclose(obj_id);
    } /* end else */

done:
    if (iter->symlink_target) {
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "}\n");
        h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                               (hsize_t)0, (hsize_t)0);
        iter->symlink_target = false;
    }
    h5tools_str_close(&buffer);

    H5TOOLS_ENDDEBUG(" ");

    return 0;
} /* end list_obj() */

/*-------------------------------------------------------------------------
 * Function: list_lnk
 *
 * Purpose: Prints information about a link
 *
 * Return: Success: 0
 *         Failure: -1
 *-------------------------------------------------------------------------
 */
static herr_t
list_lnk(const char *name, const H5L_info2_t *linfo, void *_iter)
{
    char              *buf  = NULL;
    iter_t            *iter = (iter_t *)_iter;
    int                ret;
    hsize_t            curr_pos = 0; /* total data element position   */
    h5tool_link_info_t lnk_info;
    h5tools_str_t      buffer; /* string into which to render   */
    h5tools_context_t  ctx;    /* print context  */
    h5tool_format_t   *info = &ls_dataformat;

    memset(&ctx, 0, sizeof(ctx));
    memset(&buffer, 0, sizeof(h5tools_str_t));

    h5tools_str_reset(&buffer);

    /* init linkinfo struct */
    memset(&lnk_info, 0, sizeof(h5tool_link_info_t));

    /* if verbose, make H5tools_get_symlink_info() display more */
    if (verbose_g)
        lnk_info.opt.msg_mode = 1;

    /* Print the link's name, either full name or base name */
    print_obj_name(&buffer, iter, name, "");

    switch (linfo->type) {
        case H5L_TYPE_SOFT:
            ret = H5tools_get_symlink_info(iter->fid, name, &lnk_info, follow_symlink_g);
            /* lnk_info.trg_path is malloced in H5tools_get_symlink_info()
             * so it will be freed via buf later */
            buf = (char *)lnk_info.trg_path;
            /* error */
            if (ret < 0)
                goto done;
            /* no dangling link option given and detect dangling link */
            else if (no_dangling_link_g && ret == 0)
                iter->symlink_list->dangle_link = true;

            h5tools_str_append(&buffer, "Soft Link {");
            h5tools_str_append(&buffer, "%s", buf);
            h5tools_str_append(&buffer, "}");
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);
            if (follow_symlink_g) {
                bool orig_grp_literal = grp_literal_g;
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, " ");

                /* Check if we have already seen this softlink */
                if (symlink_is_visited(iter->symlink_list, linfo->type, NULL, buf)) {
                    h5tools_str_append(&buffer, "{Already Visited}\n");
                    h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos,
                                           (size_t)info->line_ncols, (hsize_t)0, (hsize_t)0);
                    goto done;
                }
                h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                       (hsize_t)0, (hsize_t)0);

                /* Add this link to the list of seen softlinks */
                if (symlink_visit_add(iter->symlink_list, linfo->type, NULL, buf) < 0)
                    goto done;

                /* Adjust user data to specify that we are operating on the
                 * target of an soft link */
                iter->symlink_target = true;

                /* Prevent recursive listing of soft link target if
                 * recursive_g is off */
                if (!recursive_g)
                    grp_literal_g = true;
                /* Recurse through the soft link */
                if (visit_obj(iter->fid, name, iter) < 0) {
                    grp_literal_g = orig_grp_literal;
                    goto done;
                }

                grp_literal_g = orig_grp_literal;
            }
            else {
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "\n");
                h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                       (hsize_t)0, (hsize_t)0);
            }

            break;

        case H5L_TYPE_EXTERNAL: {
            const char *filename;
            const char *path;
            bool        follow_link = follow_symlink_g || follow_elink_g;

            ret = H5tools_get_symlink_info(iter->fid, name, &lnk_info, follow_link);
            /* lnk_info.trg_path is malloced in H5tools_get_symlink_info()
             * so it will be freed via buf later */
            buf = (char *)lnk_info.trg_path;
            /* error */
            if (ret < 0)
                goto done;
            /* no dangling link option given and detect dangling link */
            else if (no_dangling_link_g && ret == 0)
                iter->symlink_list->dangle_link = true;

            if (H5Lunpack_elink_val(buf, linfo->u.val_size, NULL, &filename, &path) < 0)
                goto done;

            h5tools_str_append(&buffer, "External Link {");
            h5tools_str_append(&buffer, "%s", filename);
            h5tools_str_append(&buffer, "/");
            if (*path != '/')
                h5tools_str_append(&buffer, "/");
            h5tools_str_append(&buffer, "%s", path);
            h5tools_str_append(&buffer, "}");
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);

            /* Recurse through the external link */
            /* keep the follow_elink_g for backward compatibility with -E */
            if (follow_link) {
                bool orig_grp_literal = grp_literal_g;
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, " ");

                /* Check if we have already seen this elink */
                if (symlink_is_visited(iter->symlink_list, linfo->type, filename, path)) {
                    h5tools_str_append(&buffer, "{Already Visited}\n");
                    h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos,
                                           (size_t)info->line_ncols, (hsize_t)0, (hsize_t)0);
                    goto done;
                }
                h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                       (hsize_t)0, (hsize_t)0);

                /* Add this link to the list of seen elinks */
                if (symlink_visit_add(iter->symlink_list, linfo->type, filename, path) < 0) {
                    goto done;
                }

                /* Adjust user data to specify that we are operating on the
                 * target of an external link */
                iter->symlink_target = true;

                /* Prevent recursive listing of external link target if
                 * recursive_g is off */
                if (!recursive_g)
                    grp_literal_g = true;

                /* Recurse through the external link */
                if (visit_obj(iter->fid, name, iter) < 0) {
                    grp_literal_g = orig_grp_literal;
                    goto done;
                }

                grp_literal_g = orig_grp_literal;
            }
            else
                PRINTVALSTREAM(rawoutstream, "\n");
        } break;

        case H5L_TYPE_ERROR:
        case H5L_TYPE_HARD:
        case H5L_TYPE_MAX:
        default:
            h5tools_str_append(&buffer, "UD Link {cannot follow UD links}\n");
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);
            break;
    } /* end switch */

done:
    h5tools_str_close(&buffer);

    if (buf)
        free(buf);
    return 0;
} /* end list_lnk() */

/*-------------------------------------------------------------------------
 * Function:    visit_obj
 *
 * Purpose:     Begins iteration on an object
 *
 * Return:      Success: 0
 *              Failure: -1
 *-------------------------------------------------------------------------
 */
static herr_t
visit_obj(hid_t file, const char *oname, iter_t *iter)
{
    int               retval = 0;
    H5O_info2_t       oi;           /* Information for object */
    hsize_t           curr_pos = 0; /* total data element position   */
    h5tools_str_t     buffer;       /* string into which to render   */
    h5tools_context_t ctx;          /* print context  */
    h5tool_format_t  *info = &ls_dataformat;

    memset(&ctx, 0, sizeof(ctx));
    memset(&buffer, 0, sizeof(h5tools_str_t));

    h5tools_str_reset(&buffer);

    /* Retrieve info for object to list */
    if (H5Oget_info_by_name3(file, oname, &oi, H5O_INFO_BASIC | H5O_INFO_TIME, H5P_DEFAULT) < 0) {
        if (iter->symlink_target) {
            h5tools_str_append(&buffer, "{**NOT FOUND**}\n");
            iter->symlink_target = false;
        }
        else
            print_obj_name(&buffer, iter, oname, "**NOT FOUND**");
        h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                               (hsize_t)0, (hsize_t)0);
        retval = -1;
        goto done;
    } /* end if */

    /* Check for group iteration */
    if (H5O_TYPE_GROUP == oi.type && !grp_literal_g) {
        /* Get ID for group */
        if (!iter->symlink_target && (iter->gid = H5Gopen2(file, oname, H5P_DEFAULT)) < 0) {
            h5tools_str_append(&buffer, "%s: unable to open '%s' as group\n", iter->fname, oname);
            h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                   (hsize_t)0, (hsize_t)0);
            goto done; /* Previously "continue", when this code was in main().
                        * We don't "continue" here in order to close the file
                        * and free the file name properly. */
        }              /* end if */

        /* Delay specifying the name start point so the original object name is
         * displayed if it is a link or non-group object */
        iter->name_start = iter->base_len;

        /* Specified name is a group. List the complete contents of the group. */
        h5trav_visit(file, oname, (bool)(display_root_g || iter->symlink_target), recursive_g, list_obj,
                     list_lnk, iter, H5O_INFO_BASIC | H5O_INFO_TIME);

        /* Close group */
        if (!iter->symlink_target)
            H5Gclose(iter->gid);
    } /* end if */
    else {
        /* Use file ID for root group ID */
        iter->gid = file;

        /* Specified name is a non-group object -- list that object */
        list_obj(oname, &oi, NULL, iter);
    } /* end else */

done:
    h5tools_str_close(&buffer);

    return retval;
}

/*-------------------------------------------------------------------------
 * Function:    get_width
 *
 * Purpose:     Figure out how wide the screen is.  This is highly
 *              unportable, but the user can always override the width we
 *              detect by giving a command-line option. These code snippets
 *              were borrowed from the GNU less(1).
 *
 * Return:      Success: Number of columns.
 *              Failure: Some default number of columns.
 *-------------------------------------------------------------------------
 */
static int
get_width(void)
{
    int   width = 80; /*the default   */
    char *s;

    /* Try to get it from the COLUMNS environment variable first since it's
     * value is sometimes wrong. */
    if ((s = getenv("COLUMNS")) && *s && isdigit((int)*s))
        width = (int)strtol(s, NULL, 0);

#if defined(H5_HAVE_STRUCT_VIDEOCONFIG) && defined(H5_HAVE__GETVIDEOCONFIG)
    {
        /* Microsoft C */
        struct videoconfig w;
        _getvideoconfig(&w);
        width = w.numtextcols;
    }
#elif defined(H5_HAVE_STRUCT_TEXT_INFO) && defined(H5_HAVE_GETTEXTINFO)
    {
        /* Borland C or DJGPPC */
        struct text_info w;
        gettextinfo(&w);
        width = w.screenwidth;
    }
#elif defined(H5_HAVE_GETCONSOLESCREENBUFFERINFO)
    {
        /* Win32 C */
        CONSOLE_SCREEN_BUFFER_INFO scr;
        GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &scr);
        width = scr.srWindow.Right - scr.srWindow.Left + 1;
    }
#elif defined(H5_HAVE__SCRSIZE)
    {
        /* OS/2 */
        int w[2];
        _scrsize(w);
        width = w[0];
    }
#elif defined(H5_HAVE_TIOCGWINSZ) && defined(H5_HAVE_IOCTL)
    {
        /* Unix with ioctl(TIOCGWINSZ) */
        struct winsize w;
        if (ioctl(2, (int)TIOCGWINSZ, &w) >= 0 && w.ws_col > 0)
            width = w.ws_col;
    }
#elif defined(H5_HAVE_TIOCGETD) && defined(H5_HAVE_IOCTL)
    {
        /* Unix with ioctl(TIOCGETD) */
        struct uwdata w;
        if (ioctl(2, WIOCGETD, &w) >= 0 && w.uw_width > 0)
            width = w.uw_width / w.uw_hs;
    }
#endif

    /* Set to at least 1 */
    if (width < 1)
        width = 1;
    return width;
}

/*-------------------------------------------------------------------------
 * Function:    is_valid_args
 *
 * Purpose:     check if command line arguments are valid
 *
 * Return:      Success: true (1)
 *              Failure: false (0)
 *-------------------------------------------------------------------------*/
static bool
is_valid_args(void)
{
    bool ret = true;

    if (recursive_g && grp_literal_g) {
        fprintf(rawerrorstream, "Error: 'recursive' option not compatible with 'group info' option!\n\n");
        ret = false;
        goto out;
    }

    if (no_dangling_link_g && !follow_symlink_g) {
        fprintf(rawerrorstream,
                "Error: --no-dangling-links must be used along with --follow-symlinks option!\n\n");
        ret = false;
        goto out;
    }

out:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    leave
 *
 * Purpose:     Close HDF5 and MPI and call exit()
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
 * Function:    main
 *
 * Purpose:     Opens a file and lists the specified group
 *
 * Return:      Success: 0
 *              Failure: 1
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t              file_id = H5I_INVALID_HID;
    char              *fname = NULL, *oname = NULL, *x = NULL;
    const char        *s = NULL;
    char              *rest;
    int                argno;
    static char        root_name[] = "/";
    char               drivername[50];
    int                err_exit        = 0;
    hid_t              fapl_id         = H5P_DEFAULT;
    bool               custom_vol_fapl = false;
    bool               custom_vfd_fapl = false;
    h5tools_vol_info_t vol_info        = {0};
    h5tools_vfd_info_t vfd_info        = {0};

#ifdef H5_HAVE_ROS3_VFD
    /* Default "anonymous" S3 configuration */
    H5FD_ros3_fapl_ext_t ros3_fa = {
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
    H5FD_hdfs_fapl_t hdfs_fa = {
        1,           /* Structure Version     */
        "localhost", /* Namenode Name         */
        0,           /* Namenode Port         */
        "",          /* Kerberos ticket cache */
        "",          /* User name             */
        2048,        /* Stream buffer size    */
    };
#endif /* H5_HAVE_LIBHDFS */

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /* Initialize fapl info structs */
    memset(&vol_info, 0, sizeof(h5tools_vol_info_t));
    memset(&vfd_info, 0, sizeof(h5tools_vfd_info_t));

    /* Build object display table */
    DISPATCH(H5O_TYPE_GROUP, "Group", NULL, NULL);
    DISPATCH(H5O_TYPE_DATASET, "Dataset", dataset_list1, dataset_list2);
    DISPATCH(H5O_TYPE_NAMED_DATATYPE, "Type", NULL, datatype_list2);

    /* Default output width */
    width_g = get_width();

    /* Switches come before non-switch arguments */
    for (argno = 1; argno < argc && '-' == argv[argno][0]; argno++) {
        if (!strcmp(argv[argno], "--")) {
            /* Last switch */
            argno++;
            break;
        }
        else if (!strcmp(argv[argno], "--help")) {
            usage();
            leave(EXIT_SUCCESS);
        }
        else if (!strcmp(argv[argno], "--address")) {
            address_g = true;
        }
        else if (!strcmp(argv[argno], "--data")) {
            data_g = true;
        }
        else if (!strcmp(argv[argno], "--enable-error-stack")) {
            enable_error_stack = 1;
        }
        else if (!strcmp(argv[argno], "--errors")) {
            /* deprecated --errors */
            enable_error_stack = 1;
        }
        else if (!strcmp(argv[argno], "--follow-symlinks")) {
            follow_symlink_g = true;
        }
        else if (!strcmp(argv[argno], "--no-dangling-links")) {
            no_dangling_link_g = true;
        }
        else if (!strcmp(argv[argno], "--external")) {
            follow_elink_g = true;
        }
        else if (!strcmp(argv[argno], "--full")) {
            fullname_g = true;
        }
        else if (!strcmp(argv[argno], "--group")) {
            grp_literal_g = true;
        }
        else if (!strcmp(argv[argno], "--label")) {
            label_g = true;
        }
        else if (!strcmp(argv[argno], "--recursive")) {
            recursive_g = true;
            fullname_g  = true;
        }
        else if (!strcmp(argv[argno], "--simple")) {
            simple_output_g = true;
        }
        else if (!strcmp(argv[argno], "--string")) {
            string_g = true;
        }
        else if (!strncmp(argv[argno], "--vol-value=", (size_t)12)) {
            vol_info.type    = VOL_BY_VALUE;
            vol_info.u.value = (H5VL_class_value_t)atoi(argv[argno] + 12);
            custom_vol_fapl  = true;
        }
        else if (!strncmp(argv[argno], "--vol-name=", (size_t)11)) {
            vol_info.type   = VOL_BY_NAME;
            vol_info.u.name = argv[argno] + 11;
            custom_vol_fapl = true;
        }
        else if (!strncmp(argv[argno], "--vol-info=", (size_t)11)) {
            vol_info.info_string = argv[argno] + 11;
        }
        else if (!strncmp(argv[argno], "--vfd=", (size_t)6)) {
            vfd_info.type   = VFD_BY_NAME;
            vfd_info.u.name = argv[argno] + 6;
            custom_vfd_fapl = true;
        }
        else if (!strncmp(argv[argno], "--vfd-value=", (size_t)12)) {
            vfd_info.type    = VFD_BY_VALUE;
            vfd_info.u.value = (H5FD_class_value_t)atoi(argv[argno] + 12);
            custom_vfd_fapl  = true;
        }
        else if (!strncmp(argv[argno], "--vfd-name=", (size_t)11)) {
            vfd_info.type   = VFD_BY_NAME;
            vfd_info.u.name = argv[argno] + 11;
            custom_vfd_fapl = true;
        }
        else if (!strncmp(argv[argno], "--vfd-info=", (size_t)11)) {
            vfd_info.info = (const void *)(argv[argno] + 11);
        }
        else if (!strncmp(argv[argno], "--width=", (size_t)8)) {
            width_g = (int)strtol(argv[argno] + 8, &rest, 0);

            if (0 == width_g)
                no_line_wrap_g = true;
            else if (width_g < 0 || *rest) {
                usage();
                leave(EXIT_FAILURE);
            }
        }
        else if (!strcmp(argv[argno], "--width")) {
            if ((argno + 1) >= argc) {
                usage();
                leave(EXIT_FAILURE);
            }
            else {
                s = argv[++argno];
            }
            width_g = (int)strtol(s, &rest, 0);
            if (width_g <= 0 || *rest) {
                usage();
                leave(EXIT_FAILURE);
            }
        }
        else if (!strcmp(argv[argno], "--verbose")) {
            verbose_g++;
        }
        else if (!strcmp(argv[argno], "--version")) {
            print_version(h5tools_getprogname());
            leave(EXIT_SUCCESS);
        }
        else if (!strcmp(argv[argno], "--hexdump")) {
            hexdump_g = true;
        }
        else if (!strncmp(argv[argno], "-w", (size_t)2)) {
            if (argv[argno][2]) {
                s = argv[argno] + 2;
            }
            else if ((argno + 1) >= argc) {
                usage();
                leave(EXIT_FAILURE);
            }
            else {
                s = argv[++argno];
            }
            width_g = (int)strtol(s, &rest, 0);

            if (0 == width_g) {
                no_line_wrap_g = true;
            }
            else if (width_g < 0 || *rest) {
                usage();
                leave(EXIT_FAILURE);
            }
        }
        else if (!strncmp(argv[argno], "--s3-cred=", (size_t)10)) {
#ifdef H5_HAVE_ROS3_VFD
            char const *start = NULL;

            start = strchr(argv[argno], '=');
            if (start == NULL) {
                fprintf(rawerrorstream,
                        "Error: Unable to parse null credentials tuple\n"
                        "    For anonymous access, omit \"--s3-cred\" and use only \"--vfd=ros3\"\n\n");
                usage();
                leave(EXIT_FAILURE);
            }
            start++;

            if (h5tools_parse_ros3_fapl_tuple(start, ',', &ros3_fa) < 0) {
                fprintf(rawerrorstream, "Error: failed to parse S3 VFD credential info\n\n");
                usage();
                leave(EXIT_FAILURE);
            }

            vfd_info.info = &ros3_fa;
#else
            fprintf(rawerrorstream, "Error: Read-Only S3 VFD is not available unless enabled when HDF5 is "
                                    "configured and built.\n\n");
            usage();
            leave(EXIT_FAILURE);
#endif
        }
        else if (!strncmp(argv[argno], "--hdfs-attrs=", (size_t)13)) {
#ifdef H5_HAVE_LIBHDFS
            char const *start = NULL;

            start = argv[argno] + 13; /* should never segfault: worst case of */
            if (*start != '(') {      /* null-termintor after '='.            */
                usage();
                leave(EXIT_FAILURE);
            }

            if (h5tools_parse_hdfs_fapl_tuple(start, ',', &hdfs_fa) < 0) {
                fprintf(rawerrorstream, "Error: failed to parse HDFS VFD configuration info\n\n");
                usage();
                leave(EXIT_FAILURE);
            }

            vfd_info.info = &hdfs_fa;
#else
            fprintf(
                rawerrorstream,
                "Error: The HDFS VFD is not available unless enabled when HDF5 is configured and built.\n\n");
            usage();
            leave(EXIT_FAILURE);
#endif
        }
        else if ('-' != argv[argno][1]) {
            /* Single-letter switches */
            for (s = argv[argno] + 1; *s; s++) {
                switch (*s) {
                    case '?':
                    case 'h': /* --help */
                        usage();
                        leave(EXIT_SUCCESS);
                        break;

                    case 'a': /* --address */
                        address_g = true;
                        break;

                    case 'd': /* --data */
                        data_g = true;
                        break;

                    /* deprecated -e */
                    case 'e': /* --errors */
                        enable_error_stack = 1;
                        break;

                    case 'E': /* --external */
                        follow_elink_g = true;
                        break;

                    case 'f': /* --full */
                        fullname_g = true;
                        break;

                    case 'g': /* --group */
                        grp_literal_g = true;
                        break;

                    case 'l': /* --label */
                        label_g = true;
                        break;

                    case 'r': /* --recursive */
                        recursive_g = true;
                        fullname_g  = true;
                        break;

                    case 'S': /* --simple */
                        simple_output_g = true;
                        break;

                    case 's': /* --string */
                        string_g = true;
                        break;

                    case 'v': /* --verbose */
                        verbose_g++;
                        break;

                    case 'V': /* --version */
                        print_version(h5tools_getprogname());
                        leave(EXIT_SUCCESS);
                        break;

                    case 'x': /* --hexdump */
                        hexdump_g = true;
                        break;

                    default:
                        usage();
                        leave(EXIT_FAILURE);
                } /* end switch */
            }     /* end for */
        }
        else {
            fprintf(stderr, "Unknown argument: %s\n", argv[argno]);
            usage();
            leave(EXIT_FAILURE);
        }
    } /* end for */

    /* enable error reporting if command line option */
    h5tools_error_report();

    /* If no arguments remain then print a usage message (instead of doing
     * absolutely nothing ;-) */
    if (argno >= argc) {
        usage();
        leave(EXIT_FAILURE);
    } /* end if */

    /* Check for conflicting arguments */
    if (!is_valid_args()) {
        usage();
        leave(EXIT_FAILURE);
    }

    /* Setup a custom fapl for file accesses */
    if (custom_vol_fapl || custom_vfd_fapl) {
#ifdef H5_HAVE_ROS3_VFD
        if (custom_vfd_fapl && (0 == strcmp(vfd_info.u.name, drivernames[ROS3_VFD_IDX]))) {
            if (!vfd_info.info)
                vfd_info.info = &ros3_fa;
        }
#endif
#ifdef H5_HAVE_LIBHDFS
        if (custom_vfd_fapl && (0 == strcmp(vfd_info.u.name, drivernames[HDFS_VFD_IDX]))) {
            if (!vfd_info.info)
                vfd_info.info = &hdfs_fa;
        }
#endif

        if ((fapl_id = h5tools_get_fapl(H5P_DEFAULT, custom_vol_fapl ? &vol_info : NULL,
                                        custom_vfd_fapl ? &vfd_info : NULL)) < 0) {
            error_msg("failed to setup file access property list (fapl) for file\n");
            leave(EXIT_FAILURE);
        }
    }

    /* Each remaining argument is an hdf5 file followed by an optional slash
     * and object name.
     *
     * Example: ../dir1/foo/bar/baz
     *          \_________/\______/
     *             file       obj
     *
     * The dichotomy is determined by calling H5Fopen() repeatedly until it
     * succeeds. The first call uses the entire name and each subsequent call
     * chops off the last component. If we reach the beginning of the name
     * then there must have been something wrong with the file (perhaps it
     * doesn't exist). */
    show_file_name_g = (argc - argno > 1); /*show file names if more than one*/
    while (argno < argc) {
        H5L_info2_t    li;
        iter_t         iter;
        symlink_trav_t symlink_list;
        size_t         u;

        fname   = strdup(argv[argno++]);
        oname   = NULL;
        file_id = H5I_INVALID_HID;

        while (fname && *fname) {
            file_id = h5tools_fopen(fname, H5F_ACC_RDONLY, fapl_id, (fapl_id != H5P_DEFAULT), drivername,
                                    sizeof drivername);

            if (file_id >= 0) {
                if (verbose_g)
                    PRINTSTREAM(rawoutstream, "Opened \"%s\" with %s driver.\n", fname, drivername);
                break; /*success*/
            }          /* end if */

            /* Shorten the file name; lengthen the object name */
            x     = oname;
            oname = strrchr(fname, '/');
            if (x)
                *x = '/';
            if (!oname)
                break;
            *oname = '\0';
        } /* end while */

        if (file_id < 0) {
            fprintf(rawerrorstream, "%s: unable to open file\n", argv[argno - 1]);
            free(fname);
            err_exit = 1;
            continue;
        } /* end if */
        if (oname) {
            /* Always use absolute paths to avoid confusion, keep track of where
             * to begin path name output */
            *oname        = '/';
            iter.base_len = strlen(oname);
            iter.base_len -= oname[iter.base_len - 1] == '/';
            x = oname;
            if (NULL == (oname = strdup(oname))) {
                fprintf(rawerrorstream, "memory allocation failed\n");
                leave(EXIT_FAILURE);
            }
            *x = '\0';
            /* Delay specifying the name start point so the original object name
             * is displayed if it is a link or non-group object */
            iter.name_start = 1;
        }
        if (!oname || !*oname) {
            oname = root_name;
            if (recursive_g)
                display_root_g = true;
            iter.base_len   = 0;
            iter.name_start = 0;
            /* Use x to remember if we have allocated space in oname */
            x = NULL;
        } /* end if */

        /* Remember the file information for later */
        iter.fname                     = fname;
        iter.fid                       = file_id;
        iter.gid                       = H5I_INVALID_HID;
        iter.symlink_target            = false;
        iter.symlink_list              = &symlink_list;
        iter.symlink_list->dangle_link = false;

        /* Initialize list of visited symbolic links */
        symlink_list.nused = symlink_list.nalloc = 0;
        symlink_list.objs                        = NULL;

        /* Check for root group as object name */
        if (strcmp(oname, root_name) != 0) {
            /* Check the type of link given */
            if (H5Lget_info2(file_id, oname, &li, H5P_DEFAULT) < 0) {
                hsize_t           curr_pos = 0; /* total data element position   */
                h5tools_str_t     buffer;       /* string into which to render   */
                h5tools_context_t ctx;          /* print context  */
                h5tool_format_t  *info = &ls_dataformat;

                memset(&ctx, 0, sizeof(ctx));
                memset(&buffer, 0, sizeof(h5tools_str_t));

                h5tools_str_reset(&buffer);
                print_obj_name(&buffer, &iter, oname, "**NOT FOUND**");
                h5tools_render_element(rawoutstream, info, &ctx, &buffer, &curr_pos, (size_t)info->line_ncols,
                                       (hsize_t)0, (hsize_t)0);
                leave(EXIT_FAILURE);
            } /* end if */
        }     /* end if */
        else
            li.type = H5L_TYPE_HARD;

        /* Open the object and display it's information */
        if (li.type == H5L_TYPE_HARD) {
            if (visit_obj(file_id, oname, &iter) < 0) {
                leave(EXIT_FAILURE);
            }
        } /* end if(li.type == H5L_TYPE_HARD) */
        else {
            /* Specified name is not for object -- list that link */
            /* Use file_id ID for root group ID */
            iter.gid = file_id;
            list_lnk(oname, &li, &iter);
        }
        H5Fclose(file_id);
        free(fname);
        if (x)
            free(oname);

        for (u = 0; u < symlink_list.nused; u++) {
            if (symlink_list.objs[u].type == H5L_TYPE_EXTERNAL)
                free(symlink_list.objs[u].file);

            free(symlink_list.objs[u].path);
        }
        free(symlink_list.objs);

        /* if no-dangling-links option specified and dangling link found */
        if (no_dangling_link_g && iter.symlink_list->dangle_link)
            err_exit = 1;
    } /* end while */

    if (fapl_id != H5P_DEFAULT) {
        if (0 < H5Pclose(fapl_id)) {
            fprintf(rawerrorstream, "Error: Unable to set close fapl entry\n\n");
            leave(EXIT_FAILURE);
        }
    }

    if (err_exit)
        leave(EXIT_FAILURE);
    else
        leave(EXIT_SUCCESS);
} /* end main() */
