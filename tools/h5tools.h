/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 23, 1998
 *
 * Purpose:	Support functions for the various tools.
 */
#ifndef _H5TOOLS_H
#define _H5TOOLS_H

#include <hdf5.h>
#include <stdio.h>

/*
 * Information about how to format output.
 */
typedef struct h5dump_t {
    /*
     * Fields associated with formatting numeric data.  If a datatype matches
     * multiple formats based on its size, then the first applicable format
     * from this list is used. However, if `raw' is non-zero then dump all
     * data in hexadecimal format without translating from what appears on
     * disk.
     *
     *	 raw:        If set then print all data as hexadecimal without
     *		     performing any conversion from disk.
     *
     *	 fmt_raw:    The printf() format for each byte of raw data. The
     *		     default is `%02x'.
     *
     *	 fmt_int:    The printf() format to use when rendering data which is
     *		     typed `int'. The default is `%d'.
     *
     *	 fmt_uint:   The printf() format to use when rendering data which is
     *		     typed `unsigned'. The default is `%u'.
     *
     *	 fmt_schar:  The printf() format to use when rendering data which is
     *		     typed `signed char'. The default is `%d'. This format is
     *		     used ony if the `ascii' field is zero.
     *
     *	 fmt_uchar:  The printf() format to use when rendering data which is
     *		     typed `unsigned char'. The default is `%u'. This format
     *		     is used only if the `ascii' field is zero.
     *	 
     *	 fmt_short:  The printf() format to use when rendering data which is
     *		     typed `short'. The default is `%d'.
     *
     *	 fmt_ushort: The printf() format to use when rendering data which is
     *		     typed `unsigned short'. The default is `%u'.
     *
     *	 fmt_long:   The printf() format to use when rendering data which is
     *		     typed `long'. The default is `%ld'.
     *
     *	 fmt_ulong:  The printf() format to use when rendering data which is
     *		     typed `unsigned long'. The default is `%lu'.
     *
     *	 fmt_llong:  The printf() format to use when rendering data which is
     *		     typed `long long'. The default depends on what printf()
     *		     format is available to print this datatype.
     *
     *	 fmt_ullong: The printf() format to use when rendering data which is
     *		     typed `unsigned long long'. The default depends on what
     *		     printf() format is available to print this datatype.
     *
     *	 fmt_double: The printf() format to use when rendering data which is
     *		     typed `double'. The default is `%g'.
     * 
     *	 fmt_float:  The printf() format to use when rendering data which is
     *		     typed `float'. The default is `%g'.
     *
     *	 ascii:      If set then print 1-byte integer values as an ASCII
     *		     character (no quotes).  If the character is one of the
     *		     standard C escapes then print the escaped version.  If
     *		     the character is unprintable then print a 3-digit octal
     *		     escape.  If `ascii' is zero then then 1-byte integers are
     *		     printed as numeric values.  The default is zero.
     *
     * Numeric data is also subject to the formats for individual elements.
     */
    hbool_t	raw;
    const char	*fmt_raw;
    const char	*fmt_int;
    const char	*fmt_uint;
    const char	*fmt_schar;
    const char	*fmt_uchar;
    const char	*fmt_short;
    const char	*fmt_ushort;
    const char	*fmt_long;
    const char	*fmt_ulong;
    const char	*fmt_llong;
    const char	*fmt_ullong;
    const char	*fmt_double;
    const char	*fmt_float;
    int		ascii;

    /*
     * Fields associated with compound array members.
     *
     *	 pre:	    A string to print at the beginning of each array. The
     *		    default value is the left square bracket `['.
     *
     *	 sep:	    A string to print between array values.  The default
     *		    value is a ",\001" ("\001" indicates an optional line
     *		    break).
     *
     *	 suf:	    A string to print at the end of each array.  The default
     *		    value is a right square bracket `]'.
     */
    const char	*arr_pre;
    const char	*arr_sep;
    const char	*arr_suf;
    
    /*
     * Fields associated with compound data types.
     *
     *	 name:      How the name of the struct member is printed in the
     *		    values. By default the name is not printed, but a
     *		    reasonable setting might be "%s=" which prints the name
     *		    followed by an equal sign and then the value.
     *
     *	 sep:	    A string that separates one member from another.  The
     *		    default is ", \001" (the \001 indicates an optional
     *		    line break to allow structs to span multiple lines of
     *		    output).
     *
     *	 pre:	    A string to print at the beginning of a compound type.
     *		    The default is a left curly brace.
     *
     *	 suf:       A string to print at the end of each compound type.  The
     *		    default is a right curly brace.
     */
    const char	*cmpd_name;
    const char	*cmpd_sep;
    const char	*cmpd_pre;
    const char	*cmpd_suf;

    /*
     * Fields associated with the individual elements.
     *
     *	 fmt:       A printf(3c) format to use to print the value string
     *		    after it has been rendered.  The default is "%s".
     *
     *	 suf1:	    This string is appended to elements which are followed by
     *		    another element whether the following element is on the
     *		    same line or the next line.  The default is a comma.
     *
     *	 suf2:	    This string is appended (after `suf1') to elements which
     *		    are followed on the same line by another element.  The
     *		    default is a single space.
     */
    const char	*elmt_fmt;
    const char	*elmt_suf1;
    const char	*elmt_suf2;
    
    /*
     * Fields associated with the index values printed at the left edge of
     * each line of output.
     *
     *	 n_fmt:	    Each index value is printed according to this printf(3c)
     *		    format string which should include a format for a long
     *		    integer.  The default is "%lu".
     *
     *	 sep:	    Each integer in the index list will be separated from the
     *		    others by this string, which defaults to a comma.
     *
     *	 fmt:	    After the index values are formated individually and
     *		    separated from one another by some string, the entire
     *		    resulting string will be formated according to this
     *		    printf(3c) format which should include a format for a
     *		    character string.  The default is "%s".
     */
    const char	*idx_n_fmt;		/*index number format		*/
    const char	*idx_sep;		/*separator between numbers	*/
    const char	*idx_fmt;		/*entire index format		*/
    
    /*
     * Fields associated with entire lines.
     *
     *   ncols:	    Number of columns per line defaults to 80.
     *
     *	 pre:       Each line of output contains an optional prefix area
     *		    before the data. This area can contain the index for the
     *		    first datum (represented by `%s') as well as other
     *		    constant text.  The default value is `%s'.
     *
     *	 1st:	    This is the format to print at the beginning of the first
     *		    line of output. The default value is the current value of
     *		    `pre' described above.
     *
     *	 cont:	    This is the format to print at the beginning of each line
     *		    which was continued because the line was split onto
     *		    multiple lines. This often happens with compound
     *		    data which is longer than one line of output. The default
     *		    value is the current value of the `pre' field
     *		    described above.
     *
     *	 suf:	    This character string will be appended to each line of
     *		    output.  It should not contain line feeds.  The default
     *		    is the empty string.
     *	 
     *	 sep:	    A character string to be printed after every line feed
     *		    defaulting to the empty string.  It should end with a
     *		    line feed.
     *
     *	 multi_new: Indicates the algorithm to use when data elements tend to
     *		    occupy more than one line of output. The possible values
     *		    are (zero is the default):
     *
     *		    0:  No consideration. Each new element is printed
     *			beginning where the previous element ended.
     *
     *		    1:  Print the current element beginning where the
     *			previous element left off. But if that would result
     *			in the element occupying more than one line and it
     *			would only occupy one line if it started at the
     *			beginning of a line, then it is printed at the
     *			beginning of the next line.
     *
     *	 multi_new: If an element is continued onto additional lines then
     *		    should the following element begin on the next line? The
     *		    default is to start the next element on the same line
     *		    unless it wouldn't fit.
     */
    int		line_ncols;		/*columns of output		*/
    const char	*line_pre;		/*prefix at front of each line	*/
    const char	*line_1st;		/*alternate pre. on first line	*/
    const char	*line_cont;		/*alternate pre. on continuation*/
    const char	*line_suf;		/*string to append to each line	*/
    const char	*line_sep;		/*separates lines		*/
    int		line_multi_new;		/*split multi-line outputs?	*/
} h5dump_t;


hid_t h5dump_fixtype(hid_t f_type);
int h5dump_dset(FILE *stream, const h5dump_t *info, hid_t dset, hid_t p_type);
int h5dump_mem(FILE *stream, const h5dump_t *info, hid_t type, hid_t space,
	       void *mem);
int copy_atomic_char(char* output, char* input, int numchar, int freespace);


/*if we get a new program that needs to use the library add its name here*/
typedef enum {
    UNKNOWN,
    H5LS,
    H5DUMP
} ProgType;


/*
	taken from h5dump.h
  */

#define ATTRIBUTE_DATA 0
#define DATASET_DATA	1
#define ENUM_DATA 2


#define COL 3
extern int indent;
extern void indentation(int);
extern int nCols;
/* 
	used to determine what action to take in certain cases
	this variable should be set at the beginning of all programs
	that use the lib
 */
extern ProgType programtype; 



/* taken from h5dump.h*/
#define BOOT_BLOCK	"BOOT_BLOCK"
#define GROUPNAME	"GROUP"
#define DATASET		"DATASET"
#define ATTRIBUTE	"ATTRIBUTE"
#define	DATATYPE	"DATATYPE"
#define DATASPACE	"DATASPACE"
#define DATA		"DATA"
#define SCALAR		"SCALAR"
#define SIMPLE		"SIMPLE"
#define COMPLEX		"COMPLEX"
#define STORAGELAYOUT	"STORAGELAYOUT"
#define COMPRESSION	"COMPRESSION"
#define EXTERNAL	"EXTERNAL"
#define SOFTLINK	"SOFTLINK"
#define HARDLINK	"HARDLINK"
#define NLINK		"NLINK"
#define FILENO		"FILENO"
#define OBJNO		"OBJNO"
#define STRSIZE		"STRSIZE"
#define STRPAD		"STRPAD"
#define CSET		"CSET"
#define CTYPE		"CTYPE"
#define CONCATENATOR "//"
#define DATASET "DATASET"
#define BEGIN		"{"
#define END		"}"
#endif
