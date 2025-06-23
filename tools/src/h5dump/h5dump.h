/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#ifndef H5DUMP_H
#define H5DUMP_H

/** \page H5TOOL_DP_UG The HDF5 h5dump Tool
 *
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 * <hr>
 *
 * \section sec_cltools_h5dump h5dump
 *
 * \subsection subsec_cltools_h5dump_intro Introduction
 *  With h5dump, you can display objects from an HDF5 file.
 *
 * \subsection subsec_cltools_h5dump_usage Usage
 * <h4>h5dump [OPTIONS] [files</h4>
 *
 * \subsection subsec_cltools_h5dump_error Error Report Option
 * \li <strong>--enable-error-stack</strong> Prints messages from the HDF5 error stack as they occur.
 *             Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5dump_options Options
 * \li <strong>--help</strong>    Print a usage message and exit
 * \li <strong>--version</strong> Print the library version number and exit
 *
 * \subsection subsec_cltools_h5dump_options_file File Options
 * \li <strong>--contents</strong>   Print a list of the file contents, group and dataset,
 *              names and values, then exit. Optional value 1 also prints attributes, --contents=1.
 * \li <strong>--superblock</strong> Print the content of the super block
 * \li <strong>--header</strong>     Print the header only; no data is displayed
 * \li <strong>--filedriver=D</strong> Specify which driver to open the file with
 * \li <strong>--output=F</strong>   Output raw data into file F
 * \li <strong>--binary=B</strong>   Binary file output, of form B
 * \li <strong>--ddl=F</strong>      Output ddl text into file F
 *                                   Use blank(empty) filename F to suppress ddl display
 * \li <strong>--page-buffer-size=N</strong> Set the page buffer cache size, N=non-negative integers
 * \li <strong>--endpoint-url=P</strong> Supply S3 endpoint url information to "ros3" vfd.
 *                                       P is the AWS service endpoint.
 *                                       Has no effect if filedriver is not "ros3".
 * \li <strong>--s3-cred=\<cred\></strong>   Supply S3 authentication information to "ros3" vfd.
 *                          \code <cred> :: "(<aws-region>,<access-id>,<access-key>)" \endcode
 *                          \code <cred> :: "(<aws-region>,<access-id>,<access-key>,<session-token>)" \endcode
 *                          If absent or \code \<cred\> -> "(,,)" \endcode or \code \<cred\> -> "(,,,)"
 * \endcode, no authentication. Has no effect if filedriver is not "ros3". \li
 * <strong>--hdfs-attrs=\<attrs\></strong> Supply configuration information for HDFS file access. For use with
 * <strong>--filedriver=hdfs</strong> \code <attrs> :: (<namenode name>,<namenode port>, <kerberos cache
 * path>,<username>, <buffer size>) \endcode Any absent attribute will use a default value. \li
 * <strong>--vol-value</strong> Value (ID) of the VOL connector to use for opening the HDF5 file specified \li
 * <strong>--vol-name</strong>  Name of the VOL connector to use for opening the HDF5 file specified \li
 * <strong>--vol-info</strong>  VOL-specific info to pass to the VOL connector used for opening the HDF5 file
 * specified.<br /> If none of the above options are used to specify a VOL, then the VOL named by \b
 * HDF5_VOL_CONNECTOR (or the native VOL connector, if that environment variable is unset) will be used
 * \li<strong>--vfd-value</strong> Value (ID) of the VFL driver to use for opening the HDF5 file specified
 * \li <strong>--vfd-name</strong> Name of the VFL driver to use for opening the HDF5 file specified
 * \li <strong>--vfd-info</strong> VFD-specific info to pass to the VFL driver used for
 *                                    opening the HDF5 file specified
 *
 * \subsection subsec_cltools_h5dump_options_obj Object Options
 * \li <strong>--attribute=P</strong>  Print the specified attribute
 *                          If an attribute name contains a slash (/), escape the
 *                          slash with a preceding backslash (\).
 *                          (See example section below.)
 * \li <strong>--dataset=P</strong>    Print the specified dataset
 * \li <strong>--group=P</strong>      Print the specified group and all members
 * \li <strong>--soft-link=P</strong>  Print the value(s) of the specified soft link
 * \li <strong>--datatype=P</strong>   Print the specified named datatype
 * \li <strong>--any_path=P</strong>   Print any attribute, dataset, group, datatype, or link that matches P
 *                                       P can be the absolute path or just a relative path.
 * \li <strong>--onlyattr</strong>     Print the header and value of attributes
 *                                       Optional value 0 suppresses printing attributes.
 * \li <strong>--vds-view-first-missing</strong> Set the VDS bounds to first missing mapped elements.
 * \li <strong>--vds-gap-size=N</strong>  Set the missing file gap size, N=non-negative integers
 *
 * \subsection subsec_cltools_h5dump_options_prop Object Property Options
 * \li <strong>--object-ids</strong>   Print the object ids
 * \li <strong>--properties</strong>   Print dataset filters, storage layout and fill value
 * \li <strong>--packedbits=L</strong> Print packed bits as unsigned integers, using mask
 *                          format L for an integer dataset specified with
 *                          option -d. L is a list of offset,length values,
 *                          separated by commas. Offset is the beginning bit in
 *                          the data value and length is the number of bits of
 *                          the mask.
 * \li <strong>--region</strong>       Print dataset pointed by region references
 *
 * \subsection subsec_cltools_h5dump_options_fmt Formatting Options
 * \li <strong>--escape</strong>       Escape non printing characters
 * \li <strong>--string</strong>       Print 1-byte integer datasets as ASCII
 * \li <strong>--noindex</strong>      Do not print array indices with the data
 * \li <strong>--format=T</strong>     Set the floating point output format
 * \li <strong>--lformat=T</strong>    Set the floating point long double output format
 * \li <strong>--sort_by=Q</strong>    Sort groups and attributes by index Q
 * \li <strong>--sort_order=Z</strong> Sort groups and attributes by order Z
 * \li <strong>--no-compact-subset</strong>  Disable compact form of subsetting and allow the use
 *                                       of "[" in dataset names.
 * \li <strong>--width=N</strong>      Set the number of columns of output. A value of 0 (zero)
 *                                       sets the number of columns to the maximum (65535).
 *                                       Default width is 80 columns.
 *
 * \subsection subsec_cltools_h5dump_options_xml XML Options
 * \li <strong>--xml</strong>          Output in XML using Schema
 * \li <strong>--use-dtd</strong>      Output in XML using DTD
 * \li <strong>--xml-dtd=U</strong>    Use the DTD or schema at U
 * \li <strong>--xml-ns=S</strong>     (XML Schema) Use qualified names n the XML
 *                                       ":": no namespace, default: "hdf5:"
 *                                       E.g., to dump a file called "-f", use h5dump -- -f
 *
 * \subsection subsec_cltools_h5dump_options_subset Subsetting Options
 * Subsetting is available by using the following options with a dataset
 * option. Subsetting is done by selecting a hyperslab from the data.
 * Thus, the options mirror those for performing a hyperslab selection.<br />
 * One of the \b START, \b COUNT, \b STRIDE, or \b BLOCK parameters are mandatory if you do subsetting.
 * The \b STRIDE, \b COUNT, and \b BLOCK parameters are optional and will default to 1 in
 * each dimension. \b START is optional and will default to 0 in each dimension.
 *
 * \li <strong>--start=START</strong>    Offset of start of subsetting selection
 *  \b START - is a list of integers, the number of which are equal to the
 *      number of dimensions in the dataspace being queried.<br />
 * \li <strong>--stride=STRIDE</strong>  Hyperslab stride
 *  \b COUNT - is a list of integers, the number of which are equal to the
 *      number of dimensions in the dataspace being queried.<br />
 * \li <strong>--count=COUNT</strong>    Number of blocks to include in selection
 *  \b STRIDE - is a list of integers, the number of which are equal to the
 *      number of dimensions in the dataspace being queried.<br />
 * \li <strong>--block=BLOCK</strong>    Size of block in hyperslab
 *  \b BLOCK - is a list of integers, the number of which are equal to the
 *      number of dimensions in the dataspace being queried.<br />
 *      (Alternate compact form of subsetting is described in the Reference Manual)
 *
 * \subsubsection subsubsec_cltools_h5dump_options_args Option Argument Conventions
 * \li <strong>D</strong> - is the file driver to use in opening the file. Acceptable values are available
 * from https://support.hdfgroup.org/releases/hdf5/documentation/registered_virtual_file_drivers_vfds.md.
 * Without the file driver flag, the file will be opened with each driver in turn and in the order specified
 * above until one driver succeeds in opening the file. See examples below for family, split, and multi driver
 * special file name usage.
 *
 * \li <strong>F</strong> - is a filename.
 * \li <strong>P</strong> - is the full path from the root group to the object.
 * \li <strong>N</strong> - is an integer greater than 1.
 * \li <strong>T</strong> - is a string containing the floating point format, e.g '%.3g'
 * \li <strong>T</strong> - is a string containing the floating point long double format, e.g '%.3Lg'
 * \li <strong>U</strong> - is a URI reference (as defined in [IETF RFC 2396],
 *        updated by [IETF RFC 2732])
 * \li <strong>B</strong> - is the form of binary output: NATIVE for a memory type, FILE for the
 *        file type, LE or BE for pre-existing little or big endian types.
 *        Must be used with -o (output file) and it is recommended that
 *        -d (dataset) is used. B is an optional argument, defaults to NATIVE
 * \li <strong>Q</strong> - is the sort index type. It can be "creation_order" or "name" (default)
 * \li <strong>Z</strong> - is the sort order type. It can be "descending" or "ascending" (default)
 *
 * \subsection subsec_cltools_h5dump_examples Usage Examples
 *
 * \li 1) Attribute foo of the group /bar_none in file quux.h5
 *
 *      h5dump --attribute=/bar_none/foo quux.h5
 *
 * \li 2) Attribute "high/low" of the group /bar_none in the file quux.h5
 *
 *      h5dump --attribute="/bar_none/high\/low" quux.h5
 *
 * \li 3) Selecting a subset from dataset /foo in file quux.h5
 *
 *      h5dump --dataset=/foo --start="0,1" --stride="1,1" --count="2,3" --block="2,2" quux.h5
 *
 * \li 4) Saving dataset 'dset' in file quux.h5 to binary file 'out.bin' using a little-endian type
 *
 *      h5dump --dataset=/dset --binary=LE --output=out.bin quux.h5
 *
 * \li 5) Display two packed bits (bits 0-1 and bits 4-6) in the dataset /dset
 *
 *      h5dump -d-dataset=/dset --packedbits=0,1,4,3 quux.h5
 *
 * \li 6) Dataset foo in files file1.h5 file2.h5 file3.h5
 *
 *      h5dump --dataset=/foo file1.h5 file2.h5 file3.h5
 *
 * \li 7) Dataset foo in split files splitfile-m.h5 splitfile-r.h5
 *
 *      h5dump --dataset=/foo --filedriver=split splitfile
 *
 * \li 8) Dataset foo in multi files mf-s.h5, mf-b.h5, mf-r.h5, mf-g.h5, mf-l.h5 and mf-o.h5
 *
 *      h5dump --dataset=/foo --filedriver=multi mf
 *
 * \li 9) Dataset foo in family files fam00000.h5 fam00001.h5 and fam00002.h5
 *
 *      h5dump --dataset=/foo --filedriver=family fam%05d.h5
 *
 * Previous Chapter \ref sec_cltools_h5diff - Next Chapter \ref sec_cltools_h5format_convert
 *
 * <hr>
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 *
 * \subsubsection subsubsec_cltools_h5dump_xml XML Deprecated
 * The XML option for h5dump has been deprecated. The methods for displaying XML output
 * has not been updated since 1.10.0 was released. Many new features introduced into the
 * library have never been added to the XML generation functions. Also the dtd and xsd
 * files for XML have been archived, and copies of the files, HDF5-File.dtd and HDF5-File.xsd,
 * have been saved in the "source"/tools/test/h5dump/testfiles/xml folder.
 *
 */

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5tools_ref.h"
#include "h5trav.h"
#include "h5dump_defines.h"

/**
 **  This is the global dispatch table for the dump functions.
 **/
/* the table of dump functions */
typedef struct dump_functions_t {
    void (*dump_group_function)(hid_t, const char *);
    void (*dump_named_datatype_function)(hid_t, const char *);
    void (*dump_dataset_function)(hid_t, const char *, struct subset_t *);
    void (*dump_dataspace_function)(hid_t);
    void (*dump_datatype_function)(hid_t);
    herr_t (*dump_attribute_function)(hid_t, const char *, const H5A_info_t *, void *);
    void (*dump_data_function)(hid_t, int, struct subset_t *, int);
} dump_functions;

/* List of table structures.  There is one table structure for each file */
typedef struct h5dump_table_items_t {
    unsigned long fileno;      /* File number that these tables refer to */
    hid_t         oid;         /* ID of an object in this file, held open so fileno is consistent */
    table_t      *group_table; /* Table of groups */
    table_t      *dset_table;  /* Table of datasets */
    table_t      *type_table;  /* Table of datatypes */
} h5dump_table_items_t;

typedef struct h5dump_table_list_t {
    size_t                nalloc;
    size_t                nused;
    h5dump_table_items_t *tables;
} h5dump_table_list_t;

h5dump_table_list_t table_list  = {0, 0, NULL};
table_t            *group_table = NULL, *dset_table = NULL, *type_table = NULL;

unsigned    dump_indent    = 0;     /* how far in to indent the line */
int         unamedtype     = 0;     /* shared datatype with no name */
bool        hit_elink      = false; /* whether we have traversed an external link */
size_t      prefix_len     = 1024;
char       *prefix         = NULL;
const char *fp_format      = NULL;
const char *fp_lformat     = NULL;
const char *complex_format = NULL; /* format for printing complex numbers */

/* things to display or which are set via command line parameters */
typedef struct {
    int display_all;
    int display_oid;
    int display_data;
    int display_attr_data;
    int display_char; /* print 1-byte numbers as ASCII */
    int usingdasho;
    int display_bb;             /* superblock */
    int display_dcpl;           /* dcpl */
    int display_fi;             /* file index */
    int display_ai;             /* array index */
    int display_escape;         /* escape non printable characters */
    int display_region;         /* print region reference data */
    int disable_compact_subset; /* disable compact form of subset notation */
    int display_packed_bits;    /* print 1-8 byte numbers as packed bits */
    int include_attrs;          /* Display attributes */
    int display_vds_first;      /* vds display to all by default */
    int vds_gap_size;           /* vds skip missing files default is none */
} dump_opt_t;
dump_opt_t dump_opts = {true, false, true,  true,  false, false, false, false, false,
                        true, false, false, false, false, true,  false, 0};

#define PACKED_BITS_MAX      8                       /* Maximum number of packed-bits to display */
#define PACKED_BITS_SIZE_MAX (8 * sizeof(long long)) /* Maximum bits size of integer types of packed-bits */
/* mask list for packed bits */
unsigned long long packed_mask[PACKED_BITS_MAX]; /* packed bits are restricted to 8*sizeof(llong) bytes */

/* packed bits display parameters */
unsigned packed_offset[PACKED_BITS_MAX];
unsigned packed_length[PACKED_BITS_MAX];

/*
 * The global table is set to either ddl_function_table or
 * xml_function_table in the initialization.
 */
const dump_functions *dump_function_table;

#ifdef __cplusplus
"C"
{
#endif

    void    add_prefix(char **prfx, size_t *prfx_len, const char *name);
    hid_t   h5_fileaccess(void);
    ssize_t table_list_add(hid_t oid, unsigned long file_no);
    ssize_t table_list_visited(unsigned long file_no);

#ifdef __cplusplus
}
#endif

#endif /* H5DUMP_H */
