#ifndef _H5DUMP_H
#define _H5DUMP_H

#include <hdf5.h>

#define BOOT_BLOCK	"BOOT_BLOCK"
#define GROUPNAME	"GROUP"
#define DATASET		"DATASET"
#define ATTRIBUTE	"ATTRIBUTE"
#define	DATATYPE	"DATATYPE"
#define DATASPACE	"DATASPACE"
#define DATA		"DATA"
#define ARRAY		"ARRAY"
#define OTHER		"OTHER"
#define STORAGELAYOUT	"STORAGELAYOUT"
#define COMPRESSION	"COMPRESSION"
#define EXTERNAL	"EXTERNAL"
#define SOFTLINK	"SOFTLINK"
#define HARDLINK	"HARDLINK"
#define NLINK		"NLINK"
#define FILENO		"FILENO"
#define OBJNO		"OBJNO"

#define BEGIN		"{"
#define END		"}"

#define ATTRIBUTE_DATA	0
#define DATASET_DATA	1

#define H5DUMP_MAX_NDIMS	64

#define begin_obj(obj,name)	printf("%s \"%s\" %s\n", obj, name, BEGIN)
#define end_obj()		printf("%s\n", END);

#define col 3

#endif


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


/*
 * Information about how to format output.
 */
typedef struct h5dump_t {
    /*
     * Fields associated with compound array members.
     *
     *	 pre:	    A string to print at the beginning of each array. The
     *		    default value is the left square bracket `['.
     *
     *	 sep:	    A string to print between array values.  The default
     *		    value is a comma.
     *
     *	 suf:	    A strint to print at the end of each array.  The default
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
     *		    default is a comma.
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
     *	 suf:	    This character string will be appended to each line of
     *		    output.  It should not contain line feeds.  The default
     *		    is the empty string.
     *	 
     *	 sep:	    A character string to be printed after every line feed
     *		    defaulting to the empty string.  It should end with a
     *		    line feed.
     */
    int		line_ncols;		/*columns of output		*/
    const char	*line_suf;		/*string to append to each line	*/
    const char	*line_sep;		/*separates lines		*/
    
} h5dump_t;


int h5dump1(FILE *stream, const h5dump_t *info, hid_t dset, hid_t p_type, int);

#endif
