/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 23, 1998
 *
 * Purpose:	A library for displaying the values of a dataset in a human
 *		readable format.
 */
#include <assert.h>
#include <ctype.h>
#include <h5tools.h>
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <H5private.h>
#include <h5dump.h>

/*
	taken from h5dumputil.c
*/

int indent = 0;
int compound_data=0;
int nCols = 80;
ProgType programtype = UNKNOWN;
static void display_numeric_data(hsize_t hs_nelmts, hid_t p_type,
				 unsigned char *sm_buf, size_t p_type_nbytes,
				 hsize_t p_nelmts, hsize_t dim_n_size,
				 hsize_t elmtno, hid_t container);
static void display_string(hsize_t hs_nelmts, hid_t p_type,
			   unsigned char *sm_buf, size_t p_type_nbytes,
			   hsize_t p_nelmts, hsize_t dim_n_size,
			   hsize_t elmtno);
static void display_compound_data(hsize_t hs_nelmts, hid_t p_type,
				  unsigned char *sm_buf, size_t p_type_nbytes,
				  hsize_t p_nelmts, hsize_t elmtno);

int h5dump_attr(hid_t oid, hid_t ptype);				  

int print_data(hid_t oid, hid_t _p_type, int obj_data);

/*
 * If REPEAT_VERBOSE is defined then character strings will be printed so
 * that repeated character sequences like "AAAAAAAAAA" are displayed as
 *
 * 	'A' repeates 9 times
 *
 * Otherwise the format is more Perl-like
 *
 * 	'A'*10
 * 
 */
#define REPEAT_VERBOSE

/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed.  This constant sets the limit on the
 * size of that temporary buffer in bytes.  For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */
#if 1
#define H5DUMP_BUFSIZE	(1024*1024)
#else
#define H5DUMP_BUFSIZE	(1024)
#endif

#define OPT(X,S)	((X)?(X):(S))
#define ALIGN(A,Z)	((((A)+(Z)-1)/(Z))*(Z))
#define START_OF_DATA	0x0001
#define END_OF_DATA	0x0002

/* Special strings embedded in the output */
#define OPTIONAL_LINE_BREAK	"\001"

/* Variable length string datatype */
#define STR_INIT_LEN	4096		/*initial length		*/
typedef struct h5dump_str_t {
    char		*s;		/*allocate string		*/
    size_t		len;		/*length of actual value	*/
    size_t		nalloc;		/*allocated size of string	*/
} h5dump_str_t;

/* Output variables */
typedef struct h5dump_context_t {
    size_t		cur_column;	/*current column for output	*/
    size_t		cur_elmt;	/*current element/output line	*/
    int			need_prefix;	/*is line prefix needed?	*/
    int			ndims;		/*dimensionality		*/
    hsize_t		p_min_idx[H5S_MAX_RANK]; /*min selected index	*/
    hsize_t		p_max_idx[H5S_MAX_RANK]; /*max selected index	*/
    int			prev_multiline;	/*was prev datum multiline?	*/
    size_t		prev_prefix_len;/*length of previous prefix	*/
    int			continuation;	/*continuation of previous data?*/
} h5dump_context_t;

typedef herr_t (*H5G_operator_t)(hid_t, const char*, void*);



extern void init_prefix(char **temp, int length);
extern void init_table(table_t **table);
extern void free_table(table_t **table);
extern void dump_table(char *name, table_t* table);
extern herr_t find_objs(hid_t group, const char *name, void *op_data);
extern int search_obj (table_t *temp, unsigned long *);
extern int get_table_idx(table_t *table, unsigned long *);
extern int get_tableflag(table_t*, int);
extern int set_tableflag(table_t*, int);
extern char* get_objectname(table_t*, int);
   

/*-------------------------------------------------------------------------
 * Function:	h5dump_str_close
 *
 * Purpose:	Closes a string by releasing it's memory and setting the size
 *		information to zero.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
h5dump_str_close(h5dump_str_t *str)
{
    if (str && str->nalloc) {
	free(str->s);
	memset(str, 0, sizeof(h5dump_str_t));
    }
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_str_len
 *
 * Purpose:	Returns the length of the string, not counting the null
 *		terminator.
 *
 * Return:	Success:	Length of string
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
h5dump_str_len(h5dump_str_t *str)
{
    return str->len;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_str_append
 *
 * Purpose:	Formats variable arguments according to printf() format
 *		string and appends the result to variable length string STR.
 *
 * Return:	Success:	Pointer to buffer containing result.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
h5dump_str_append(h5dump_str_t *str/*in,out*/, const char *fmt, ...)
{
    va_list	ap;

    va_start(ap, fmt);

    /* Make sure we have some memory into which to print */
    if (!str->s || str->nalloc<=0) {
	str->nalloc = STR_INIT_LEN;
	str->s = malloc(str->nalloc);
	assert(str->s);
	str->s[0] = '\0';
	str->len = 0;
    }

    while (1) {
	size_t avail = str->nalloc - str->len;
	size_t nchars = HDvsnprintf(str->s+str->len, avail, fmt, ap);
	if (nchars<avail) {
	    /* success */
	    str->len += nchars;
	    break;
	}
	
	/* Try again with twice as much space */
	str->nalloc *= 2;
	str->s = realloc(str->s, str->nalloc);
	assert(str->s);
    }

    va_end(ap);
    return str->s;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_str_reset
 *
 * Purpose:	Reset the string to the empty value. If no memory is
 *		allocated yet then initialize the h5dump_str_t struct.
 *
 * Return:	Success:	Ptr to the buffer which contains a null
 *				character as the first element.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
h5dump_str_reset(h5dump_str_t *str/*in,out*/)
{
    if (!str->s || str->nalloc<=0) {
	str->nalloc = STR_INIT_LEN;
	str->s = malloc(str->nalloc);
	assert(str->s);
    }

    str->s[0] = '\0';
    str->len = 0;
    return str->s;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_str_trunc
 *
 * Purpose:	Truncate a string to be at most SIZE characters.
 *
 * Return:	Success:	Pointer to the string
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
h5dump_str_trunc(h5dump_str_t *str/*in,out*/, size_t size)
{
    if (size<str->len) {
	str->len = size;
	str->s[size] = '\0';
    }
    return str->s;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_str_fmt
 *
 * Purpose:	Reformat a string contents beginning at character START
 *		according to printf format FMT. FMT should contain no format
 *		specifiers except possibly the `%s' variety. For example, if
 *		the input string is `hello' and the format is "<<%s>>" then
 *		the output value will be "<<hello>>".
 *
 * Return:	Success:	A pointer to the resulting string.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
h5dump_str_fmt(h5dump_str_t *str/*in,out*/, size_t start, const char *fmt)
{
    char	_temp[1024], *temp=_temp;

    /* If the format string is simply "%s" then don't bother doing anything */
    if (!strcmp(fmt, "%s")) return str->s;

    /*
     * Save the input value if there is a `%' anywhere in FMT.  Otherwise
     * don't bother because we don't need a temporary copy.
     */
    if (strchr(fmt, '%')) {
	if ((str->len-start)+1>sizeof _temp) {
	    temp = malloc((str->len-start)+1);
	    assert(temp);
	}
	strcpy(temp, str->s+start);
    }

    /* Reset the output string and append a formatted version */
    h5dump_str_trunc(str, start);
    h5dump_str_append(str, fmt, temp);

    /* Free the temp buffer if we allocated one */
    if (temp != _temp) free(temp);
    return str->s;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_prefix
 *
 * Purpose:	Renders the line prefix value into string STR.
 *
 * Return:	Success:	Pointer to the prefix.
 *
 * 		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
h5dump_prefix(h5dump_str_t *str/*in,out*/, const h5dump_t *info,
	      hsize_t elmtno, int ndims, hsize_t min_idx[], hsize_t max_idx[])
{
    hsize_t	p_prod[H5S_MAX_RANK], p_idx[H5S_MAX_RANK];
    hsize_t	n, i=0;

    h5dump_str_reset(str);
    if (ndims>0) {
	/*
	 * Calculate the number of elements represented by a unit change in a
	 * certain index position.
	 */
	for (i=ndims-1, p_prod[ndims-1]=1; i>0; --i) {
	    p_prod[i-1] = (max_idx[i]-min_idx[i]) * p_prod[i];
	}

	/*
	 * Calculate the index values from the element number.
	 */
	for (i=0, n=elmtno; i<(hsize_t)ndims; i++) {
	    p_idx[i] = n / p_prod[i] + min_idx[i];
	    n %= p_prod[i];
	}

	/*
	 * Print the index values.
	 */
	for (i=0; i<(hsize_t)ndims; i++) {
	    if (i) h5dump_str_append(str, "%s", OPT(info->idx_sep, ","));
	    h5dump_str_append(str, OPT(info->idx_n_fmt, "%lu"),
			      (unsigned long)p_idx[i]);
	}
    } else {
	/* Scalar */
	h5dump_str_append(str, OPT(info->idx_n_fmt, "%lu"), (unsigned long)0);
    }

    /*
     * Add prefix and suffix to the index.
     */
    return h5dump_str_fmt(str, 0, OPT(info->idx_fmt, "%s: "));
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_escape
 *
 * Purpose:	Changes all "funny" characters in S into standard C escape
 *		sequences. If ESCAPE_SPACES is non-zero then spaces are
 *		escaped by prepending a backslash.
 *
 * Return:	Success:	S
 *
 *		Failure:	NULL if the buffer would overflow. The
 *				buffer has as many left-to-right escapes as
 *				possible before overflow would have happened.
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
h5dump_escape(char *s/*in,out*/, size_t size, int escape_spaces)
{
    size_t	n = strlen(s);
    size_t	i;
    const char	*escape;
    char	octal[8];
    
    for (i=0; i<n; i++) {
	switch (s[i]) {
	case '"':
	    escape = "\\\"";
	    break;
	case '\\':
	    escape = "\\\\";
	    break;
	case '\b':
	    escape = "\\b";
	    break;
	case '\f':
	    escape = "\\f";
	    break;
	case '\n':
	    escape = "\\n";
	    break;
	case '\r':
	    escape = "\\r";
	    break;
	case '\t':
	    escape = "\\t";
	    break;
	case ' ':
	    escape = escape_spaces ? "\\ " : NULL;
	    break;
	default:
	    if (!isprint((int)*s)) {
		sprintf(octal, "\\%03o", (unsigned char)(s[i]));
		escape = octal;
	    } else {
		escape = NULL;
	    }
	    break;
	}

	if (escape) {
	    size_t esc_size = strlen(escape);
	    if (n+esc_size+1>size) return NULL; /*would overflow*/
	    memmove(s+i+esc_size, s+i, (n-i)+1); /*make room*/
	    memcpy(s+i, escape, esc_size); /*insert*/
	    n += esc_size;
	    i += esc_size - 1;
	}
    }
    return s;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_is_zero
 *
 * Purpose:	Determines if memory is initialized to all zero bytes.
 *
 * Return:	TRUE if all bytes are zero; FALSE otherwise
 *
 * Programmer:	Robb Matzke
 *              Monday, June  7, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
h5dump_is_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char*)_mem;
    while (size-- > 0) {
	if (mem[size]) return FALSE;
    }
    return TRUE;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_region
 *
 * Purpose:	Prints information about a dataspace region by appending
 *		the information to the specified string.
 *
 * Return:	Success:	0
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, June  7, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
h5dump_region(hid_t region, h5dump_str_t *str/*in,out*/)
{
    hssize_t	nblocks, npoints, i;
    hsize_t	*ptdata;
    int		j;
    int		ndims = H5Sget_simple_extent_ndims(region);

    /*
     * These two functions fail if the region does not have blocks or points,
     * respectively. They do not currently know how to translate from one to
     * the other.
     */
    H5E_BEGIN_TRY {
	nblocks = H5Sget_select_hyper_nblocks(region);
	npoints = H5Sget_select_elem_npoints(region);
    } H5E_END_TRY;
    h5dump_str_append(str, "{");

    /* Print block information */
    if (nblocks>0) {
	ptdata = malloc(nblocks*ndims*2*sizeof(ptdata[0]));
	H5Sget_select_hyper_blocklist(region, 0, nblocks, ptdata);
	for (i=0; i<nblocks; i++) {
		if (programtype == H5DUMP) {
	    h5dump_str_append(str, "%s",
			      i?","OPTIONAL_LINE_BREAK" ":"",
			      (unsigned long)i);
		}
		else {
	    h5dump_str_append(str, "%sBlk%lu: ",
			      i?","OPTIONAL_LINE_BREAK" ":"",
			      (unsigned long)i);
		}
	    /* Start coordinates and opposite corner */
	    for (j=0; j<ndims; j++) {
			h5dump_str_append(str, "%s%lu", j?",":"(",
				  (unsigned long)(ptdata[i*2*ndims+j]));
	    }
	    for (j=0; j<ndims; j++) {
		h5dump_str_append(str, "%s%lu", j?",":")-(",
				  (unsigned long)(ptdata[i*2*ndims+j+ndims]));
	    }
	    h5dump_str_append(str, ")");
	}
	free(ptdata);
    }

    /* Print point information */
    if (npoints>0) {
	ptdata = malloc(npoints*ndims*sizeof(ptdata[0]));
	H5Sget_select_elem_pointlist(region, 0, npoints, ptdata);
	for (i=0; i<npoints; i++) {
		if (programtype == H5DUMP){
	    h5dump_str_append(str, "%s",
			      i?","OPTIONAL_LINE_BREAK" ":"",
			      (unsigned long)i);		
		}
		else {
	    h5dump_str_append(str, "%sPt%lu: ",
			      i?","OPTIONAL_LINE_BREAK" ":"",
			      (unsigned long)i);
		}
	    for (j=0; j<ndims; j++) {
		h5dump_str_append(str, "%s%lu", j?",":"(",
				  (unsigned long)(ptdata[i*ndims+j]));
	    }
	    h5dump_str_append(str, ")");
	}
	free(ptdata);
    }
    
    h5dump_str_append(str, "}");
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_sprint
 *
 * Purpose:	Renders the value pointed to by VP of type TYPE into variable
 *		length string STR.
 *
 * Return:	A pointer to memory containing the result or NULL on error.
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-04-26
 *		Made this function safe from overflow problems by allowing it
 *		to reallocate the output string.
 *
 * 		Robb Matzke, 1999-06-04
 *		Added support for object references. The new `container'
 *		argument is the dataset where the reference came from.
 *
 * 		Robb Matzke, 1999-06-07
 *		Added support for printing raw data. If info->raw is non-zero
 *		then data is printed in hexadecimal format.
 *
 *-------------------------------------------------------------------------
 */
static char *
h5dump_sprint(h5dump_str_t *str/*in,out*/, const h5dump_t *info,
	      hid_t container, hid_t type, void *vp)
{
    size_t	i, n, offset, size, dims[H5S_MAX_RANK], nelmts, start;
    char	*name, quote='\0';
    hid_t	memb, obj, region;
    int		nmembs, j, k, ndims, otype;
    static char	fmt_llong[8], fmt_ullong[8];
    H5T_str_t 	pad;
    H5G_stat_t	sb;
    
    /* Build default formats for long long types */
    if (!fmt_llong[0]) {
	sprintf(fmt_llong, "%%%sd", PRINTF_LL_WIDTH);
	sprintf(fmt_ullong, "%%%su", PRINTF_LL_WIDTH);
    }

	
    /* Append value depending on data type */
    start = h5dump_str_len(str);
    if (info->raw) {
	h5dump_str_append(str, "0x");
	n = H5Tget_size(type);
	for (i=0; i<n; i++) {
	    h5dump_str_append(str, OPT(info->fmt_raw, "%02x"),
			      ((unsigned char*)vp)[i]);
	}
	
    } else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
	h5dump_str_append(str, OPT(info->fmt_double, "%g"), *((double*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
	h5dump_str_append(str, OPT(info->fmt_double, "%g"), *((float*)vp));
		
    } else if (info->ascii &&
	       (H5Tequal(type, H5T_NATIVE_SCHAR) ||
		H5Tequal(type, H5T_NATIVE_UCHAR))) {
	if (ESCAPE_HTML==info->str_locale) {
	    if (*((char*)vp)<=' ' || *((char*)vp)>'~') {
		h5dump_str_append(str, "%%%02X", *((unsigned char*)vp));
	    } else {
		h5dump_str_append(str, "%c", *((char*)vp));
	    }
	} else {
	    switch (*((char*)vp)) {
	    case '"':
		h5dump_str_append(str, "\\\"");
		break;
	    case '\\':
		h5dump_str_append(str, "\\\\");
		break;
	    case '\b':
		h5dump_str_append(str, "\\b");
		break;
	    case '\f':
		h5dump_str_append(str, "\\f");
		break;
	    case '\n':
		h5dump_str_append(str, "\\n");
		break;
	    case '\r':
		h5dump_str_append(str, "\\r");
		break;
	    case '\t':
		h5dump_str_append(str, "\\t");
		break;
	    default:
		if (isprint(*((char*)vp))) {
		    h5dump_str_append(str, "%c", *((char*)vp));
		} else {
		    h5dump_str_append(str, "\\%03o", *((unsigned char*)vp));
		}
		break;
	    }
	}

    } else if (H5T_STRING==H5Tget_class(type)) {
	size = H5Tget_size(type);
	quote = '\0';
	pad = H5Tget_strpad(type);
	
	for (i=0;
	     i<size && ((pad == H5T_STR_NULLPAD)?1:(((char*)vp)[i] != '\0'));
	     i++) {
			
	    /*
	     * Count how many times the next character repeats. If the
	     * threshold is zero then that means it can repeat any number
	     * of times.
	     */
	    j=1;
	    if (info->str_repeat>0) {
		while (i+j<size && ((char*)vp)[i]==((char*)vp)[i+j]) j++;
	    }
	    
	    /*
	     * Print the opening quote.  If the repeat count is high enough to
	     * warrant printing the number of repeats instead of enumerating
	     * the characters, then make sure the character to be repeated is
	     * in it's own quote.
	     */
	    if (info->str_repeat>0 && j>info->str_repeat) {
		if (quote) h5dump_str_append(str, "%c", quote);
		quote = '\'';
		h5dump_str_append(str, "%s%c", i?" ":"", quote);
	    } else if (!quote) {
		quote = '"';
		h5dump_str_append(str, "%s%c", i?" ":"", quote);
	    }
			
	    /* Print the character */
	    if (ESCAPE_HTML==info->str_locale) {
		if (((char*)vp)[i]<=' ' || ((char*)vp)[i]>'~') {
		    h5dump_str_append(str, "%%%02X", ((unsigned char*)vp)[i]);
		} else {
		    h5dump_str_append(str, "%c", ((char*)vp)[i]);
		}
	    } else {
		switch (((char*)vp)[i]) {
		case '"':
		    h5dump_str_append(str, "\\\"");
		    break;
		case '\\':
		    h5dump_str_append(str, "\\\\");
		    break;
		case '\b':
		    h5dump_str_append(str, "\\b");
		    break;
		case '\f':
		    h5dump_str_append(str, "\\f");
		    break;
		case '\n':
		    h5dump_str_append(str, "\\n");
		    break;
		case '\r':
		    h5dump_str_append(str, "\\r");
		    break;
		case '\t':
		    h5dump_str_append(str, "\\t");
		    break;
		default:
		    if (isprint(((char*)vp)[i])) {
			h5dump_str_append(str, "%c", ((char*)vp)[i]);
		    } else {
			h5dump_str_append(str, "\\%03o",
					  ((unsigned char*)vp)[i]);
		    }
		    break;
		}
	    }
	    
	    /* Print the repeat count */
	    if (info->str_repeat && j>info->str_repeat) {
#ifdef REPEAT_VERBOSE
		h5dump_str_append(str, "%c repeats %d times", quote, j-1);
#else
		h5dump_str_append(str, "%c*%d", quote, j-1);
#endif
		quote = '\0';
		i += j-1;
	    }
	}
	if (quote) h5dump_str_append(str, "%c", quote);

	if (0==i) {
	    h5dump_str_append(str, "\"\"");	/*empty string*/
	}
	
		
    } else if (H5Tequal(type, H5T_NATIVE_INT)) {
	h5dump_str_append(str, OPT(info->fmt_int, "%d"),
			  *((int*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
	h5dump_str_append(str, OPT(info->fmt_uint, "%u"),
			  *((unsigned*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
	h5dump_str_append(str, OPT(info->fmt_schar, "%d"),
			  *((signed char*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
	h5dump_str_append(str, OPT(info->fmt_uchar, "%u"),
			  *((unsigned char*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
	h5dump_str_append(str, OPT(info->fmt_short, "%d"),
			  *((short*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
	h5dump_str_append(str, OPT(info->fmt_ushort, "%u"),
			  *((unsigned short*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
	h5dump_str_append(str, OPT(info->fmt_long, "%ld"),
			  *((long*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
	h5dump_str_append(str, OPT(info->fmt_ulong, "%lu"),
			  *((unsigned long*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
	h5dump_str_append(str, OPT(info->fmt_llong, fmt_llong),
			  *((long_long*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
	h5dump_str_append(str, OPT(info->fmt_ullong, fmt_ullong),
			  *((unsigned long_long*)vp));
		
    } else if (H5Tequal(type, H5T_NATIVE_HSSIZE)) {
	if (sizeof(hssize_t)==sizeof(int)) {
	    h5dump_str_append(str, OPT(info->fmt_int, "%d"),
			      *((int*)vp));
	} else if (sizeof(hssize_t)==sizeof(long)) {
	    h5dump_str_append(str, OPT(info->fmt_long, "%ld"),
			      *((long*)vp));
	} else {
	    h5dump_str_append(str, OPT(info->fmt_llong, fmt_llong),
			      *((int64_t*)vp));
	}
		
    } else if (H5Tequal(type, H5T_NATIVE_HSIZE)) {
	if (sizeof(hsize_t)==sizeof(int)) {
	    h5dump_str_append(str, OPT(info->fmt_uint, "%u"),
			      *((unsigned*)vp));
	} else if (sizeof(hsize_t)==sizeof(long)) {
	    h5dump_str_append(str, OPT(info->fmt_ulong, "%lu"),
			      *((unsigned long*)vp));
	} else {
	    h5dump_str_append(str, OPT(info->fmt_ullong, fmt_ullong),
			      *((uint64_t*)vp));
	}
		
    } else if (H5T_COMPOUND==H5Tget_class(type)) {
	nmembs = H5Tget_nmembers(type);
	h5dump_str_append(str, "%s", OPT(info->cmpd_pre, "{"));
	for (j=0; j<nmembs; j++) {
	    if (j) h5dump_str_append(str, "%s",
				     OPT(info->cmpd_sep,
					 ", " OPTIONAL_LINE_BREAK));
			
	    /* The name */
	    name = H5Tget_member_name(type, j);
	    h5dump_str_append(str, OPT(info->cmpd_name, ""), name);
	    free(name);
			
	    /* The value */
	    offset = H5Tget_member_offset(type, j);
	    memb = H5Tget_member_type(type, j);
	    size = H5Tget_size(memb);
	    ndims = H5Tget_member_dims(type, j, dims, NULL);
	    assert(ndims>=0 && ndims<=H5S_MAX_RANK);
	    for (k=0, nelmts=1; k<ndims; k++) nelmts *= dims[k];
			
	    if (nelmts>1) {
		h5dump_str_append(str, "%s", OPT(info->arr_pre, "["));
	    }
	    for (i=0; i<nelmts; i++) {
		if (i) {
		    h5dump_str_append(str, "%s",
				      OPT(info->arr_sep,
					  "," OPTIONAL_LINE_BREAK));
		}
		h5dump_sprint(str, info, container, memb,
			      (char*)vp+offset+i*size);
	    }
	    if (nelmts>1) {
		h5dump_str_append(str, "%s", OPT(info->arr_suf, "]"));
	    }
	    H5Tclose(memb);
	}
	h5dump_str_append(str, "%s", OPT(info->cmpd_suf, "}"));
		
    } else if (H5T_ENUM==H5Tget_class(type)) {
	char enum_name[1024];
	if (H5Tenum_nameof(type, vp, enum_name, sizeof enum_name)>=0) {
	    h5dump_str_append(str, h5dump_escape(enum_name, sizeof enum_name, TRUE));
	} else {
	    h5dump_str_append(str, "0x");
	    n = H5Tget_size(type);
	    for (i=0; i<n; i++) {
		h5dump_str_append(str, "%02x", ((unsigned char*)vp)[i]);
	    }
	}
		
    } else if (H5Tequal(type, H5T_STD_REF_DSETREG)) {
	/*
	 * Dataset region reference -- show the type and OID of the referenced
	 * object, but we are unable to show the region yet because there
	 * isn't enough support in the data space layer.  - rpm 19990604
	 */
	if (h5dump_is_zero(vp, H5Tget_size(type))) {
	    h5dump_str_append(str, "NULL");
	} else {
	    obj = H5Rdereference(container, H5R_DATASET_REGION, vp);
	    region = H5Rget_region(container, H5R_DATASET_REGION, vp);
	    H5Gget_objinfo(obj, ".", FALSE, &sb);
		if (programtype == H5DUMP) {
	    h5dump_str_append(str, "%s %lu:%lu ",DATASET,sb.objno[1], sb.objno[0]);
		}
		else {
	    h5dump_str_append(str, "DSET-%lu:%lu:%lu:%lu-",
			      sb.fileno[1], sb.fileno[0],
			      sb.objno[1], sb.objno[0]);
		}
	    h5dump_region(region, str);
	    H5Sclose(region);
	    H5Dclose(obj);
	}
	
    } else if (H5Tequal(type, H5T_STD_REF_OBJ)) {
	/*
	 * Object references -- show the type and OID of the referenced
	 * object.
	 */
	if (h5dump_is_zero(vp, H5Tget_size(type))) {
	    h5dump_str_append(str, "NULL");
	} else {
	    otype = H5Rget_object_type(container, vp);
	    obj = H5Rdereference(container, H5R_OBJECT, vp);
	    H5Gget_objinfo(obj, ".", FALSE, &sb);

	    /* Print object type and close object */
	    switch (otype) {
	    case H5G_GROUP:
		h5dump_str_append(str, GROUPNAME);
		H5Gclose(obj);
		break;
	    case H5G_DATASET:
		h5dump_str_append(str, DATASET);
		H5Dclose(obj);
		break;
	    case H5G_TYPE:
		h5dump_str_append(str, DATATYPE);
		H5Tclose(obj);
		break;
	    default:
			if (programtype == H5LS) {
				h5dump_str_append(str, "%u-", otype);
			}
			else if (programtype == H5DUMP) {
				h5dump_str_append(str, "unknown object reference type");
			}
			/* unable to close `obj' since we don't know the type */
			break;
	    }

	    /* Print OID */
		if (programtype == H5DUMP) {
		h5dump_str_append(str, " %lu:%lu",
			      sb.objno[1], sb.objno[0]);
		}
		else {
	    h5dump_str_append(str, "-%lu:%lu:%lu:%lu",
			      sb.fileno[1], sb.fileno[0],
			      sb.objno[1], sb.objno[0]);
		}
	}
	
    } else {
	/* All other types get printed as hexadecimal */
	h5dump_str_append(str, "0x");
	n = H5Tget_size(type);
	for (i=0; i<n; i++) {
	    h5dump_str_append(str, "%02x", ((unsigned char*)vp)[i]);
	}
    }
	
    return h5dump_str_fmt(str, start, OPT(info->elmt_fmt, "%s"));
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_ncols
 *
 * Purpose:	Count the number of columns in a string. This is the number
 *		of characters in the string not counting line-control
 *		characters.
 *
 * Return:	Success:	Width of string.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 27, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
h5dump_ncols(const char *s)
{
    size_t	i;
    
    for (i=0; *s; s++) if (*s>=' ') i++;
    return i;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_simple_prefix
 *
 * Purpose:	If ctx->need_prefix is set then terminate the current line
 *		(if applicable), calculate the prefix string, and display it
 * 		at the start of a line.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 *		Robb Matzke, 1999-09-29
 *		If a new prefix is printed then the current element number is
 *		set back to zero.
 *-------------------------------------------------------------------------
 */
static void
h5dump_simple_prefix(FILE *stream, const h5dump_t *info,
		     h5dump_context_t *ctx, hsize_t elmtno, int secnum)
{
    h5dump_str_t	prefix;

    memset(&prefix, 0, sizeof(h5dump_str_t));
    if (!ctx->need_prefix) return;
    
    /* Terminate previous line, if any */
    if (ctx->cur_column) {
	fputs(OPT(info->line_suf, ""), stream);
	putc('\n', stream);
	fputs(OPT(info->line_sep, ""), stream);
    }

    /* Calculate new prefix */
    h5dump_prefix(&prefix, info, elmtno, ctx->ndims,
		  ctx->p_min_idx, ctx->p_max_idx);

    /* Write new prefix to output */
    if (0==elmtno && 0==secnum && info->line_1st) {
	fputs(h5dump_str_fmt(&prefix, 0, info->line_1st),
	      stream);
    } else if (secnum && info->line_cont) {
	fputs(h5dump_str_fmt(&prefix, 0, info->line_cont),
	      stream);
    } else {
	fputs(h5dump_str_fmt(&prefix, 0, info->line_pre),
	      stream);
    }
    ctx->cur_column = ctx->prev_prefix_len = h5dump_str_len(&prefix);
    ctx->cur_elmt = 0;
    ctx->need_prefix = 0;

    /* Free string */
    h5dump_str_close(&prefix);
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_simple_data
 *
 * Purpose:	Prints some (NELMTS) data elements to output STREAM. The
 *		elements are stored in _MEM as type TYPE and are printed
 *		according to the format described in INFO. The CTX struct
 *		contains context information shared between calls to this
 *		function.  The FLAGS is a bit field that indicates whether
 *		the data supplied in this call falls at the beginning or end
 *		of the total data to be printed (START_OF_DATA and
 *		END_OF_DATA).
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Monday, April 26, 1999
 *
 * Modifications:
 * 		Robb Matzke, 1999-06-04
 *		The `container' argument is the optional dataset for
 *		reference types.
 *
 * 		Robb Matzke, 1999-09-29
 *		Understands the `per_line' property which indicates that
 *		every Nth element should begin a new line.
 *
 *-------------------------------------------------------------------------
 */
static void
h5dump_simple_data(FILE *stream, const h5dump_t *info, hid_t container,
		   h5dump_context_t *ctx/*in,out*/, unsigned flags,
		   hsize_t nelmts, hid_t type, void *_mem)
{
    unsigned char	*mem = (unsigned char*)_mem;
    hsize_t		i;		/*element counter		*/
    char		*s, *section;	/*a section of output		*/
    int			secnum;		/*section sequence number	*/
    size_t		size;		/*size of each datum		*/
    size_t		ncols=80;	/*available output width	*/
    h5dump_str_t	buffer;		/*string into which to render	*/
    int			multiline;	/*datum was multiline		*/

    /* Setup */
    memset(&buffer, 0, sizeof(h5dump_str_t));
    size = H5Tget_size(type);
    if (info->line_ncols>0) ncols = info->line_ncols;
    h5dump_simple_prefix(stream, info, ctx, 0, 0);
    
    for (i=0; i<nelmts; i++, ctx->cur_elmt++) {
	
	/* Render the element */
	h5dump_str_reset(&buffer);
	h5dump_sprint(&buffer, info, container, type, mem+i*size);
	if (i+1<nelmts || 0==(flags & END_OF_DATA)) {
	    h5dump_str_append(&buffer, "%s", OPT(info->elmt_suf1, ","));
	}
	s = h5dump_str_fmt(&buffer, 0, "%s");

	/*
	 * If the element would split on multiple lines if printed at our
	 * current location...
	 */
	if (1==info->line_multi_new &&
	    (ctx->cur_column + h5dump_ncols(s) +
	     strlen(OPT(info->elmt_suf2, " ")) +
	     strlen(OPT(info->line_suf, ""))) > ncols) {
	    if (ctx->prev_multiline) {
		/*
		 * ... and the previous element also occupied more than one
		 * line, then start this element at the beginning of a line.
		 */
		ctx->need_prefix = TRUE;
	    } else if ((ctx->prev_prefix_len + h5dump_ncols(s) +
			strlen(OPT(info->elmt_suf2, " ")) +
			strlen(OPT(info->line_suf, ""))) <= ncols) {
		/* 
		 * ...but *could* fit on one line otherwise, then we
		 * should end the current line and start this element on its
		 * own line.
		 */
		ctx->need_prefix = TRUE;
	    }
	}

	/*
	 * If the previous element occupied multiple lines and this element
	 * is too long to fit on a line then start this element at the
	 * beginning of the line.
	 */
	if (1==info->line_multi_new &&
	    ctx->prev_multiline &&
	    (ctx->cur_column + h5dump_ncols(s) +
	     strlen(OPT(info->elmt_suf2, " ")) +
	     strlen(OPT(info->line_suf, ""))) > ncols) {
	    ctx->need_prefix = TRUE;
	}

	/*
	 * If too many elements have already been printed then we need to
	 * start a new line.
	 */
	if (info->line_per_line>0 && ctx->cur_elmt>=info->line_per_line) {
	    ctx->need_prefix = TRUE;
	}
	
	/*
	 * Each OPTIONAL_LINE_BREAK embedded in the rendered string can cause
	 * the data to split across multiple lines.  We display the sections
	 * one-at a time.
	 */
	for (secnum=0, multiline=0;
	     (section=strtok(secnum?NULL:s, OPTIONAL_LINE_BREAK));
	     secnum++) {
	    /*
	     * If the current section plus possible suffix and end-of-line
	     * information would cause the output to wrap then we need to
	     * start a new line.
	     */
	    if ((ctx->cur_column + strlen(section) +
		 strlen(OPT(info->elmt_suf2, " ")) +
		 strlen(OPT(info->line_suf, ""))) > ncols) {
		ctx->need_prefix = 1;
	    }

	    /*
	     * Print the prefix or separate the beginning of this element
	     * from the previous element.
	     */
	    if (ctx->need_prefix) {
		if (secnum) multiline++;
		h5dump_simple_prefix(stream, info, ctx, i, secnum);
	    } else if ((i || ctx->continuation) && 0==secnum) {
		fputs(OPT(info->elmt_suf2, " "), stream);
		ctx->cur_column += strlen(OPT(info->elmt_suf2, " "));
	    }
	    
	    /* Print the section */
	    fputs(section, stream);
	    ctx->cur_column += strlen(section);
	}
	ctx->prev_multiline = multiline;
    }
    h5dump_str_close(&buffer);
}

	

/*-------------------------------------------------------------------------
 * Function:	h5dump_simple_dset
 *
 * Purpose:	Print some values from a dataset with a simple data space.
 *		This is a special case of h5dump_dset(). This function only
 *		intended for dumping datasets -- it does strip mining and
 *		some other things which are unnecessary for smaller objects
 *		such as attributes (to print small objects like attributes
 *		simply read the attribute and call h5dump_simple_mem()).
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */ 
static int
h5dump_simple_dset(FILE *stream, const h5dump_t *info, hid_t dset,
		   hid_t p_type)
{
    hid_t		f_space;		/*file data space	*/
    hsize_t		elmtno, i;		/*counters		*/
    int			carry;			/*counter carry value	*/
    hssize_t		zero[8];		/*vector of zeros	*/
    unsigned		flags;			/*buffer extent flags	*/
    hsize_t		total_size[H5S_MAX_RANK];/*total size of dataset*/

    /* Print info */
    h5dump_context_t	ctx;			/*print context		*/
    size_t		p_type_nbytes;		/*size of memory type	*/
    hsize_t		p_nelmts;		/*total selected elmts	*/

    /* Stripmine info */
    hsize_t		sm_size[H5S_MAX_RANK];	/*stripmine size	*/
    hsize_t		sm_nbytes;		/*bytes per stripmine	*/
    hsize_t		sm_nelmts;		/*elements per stripmine*/
    unsigned char	*sm_buf=NULL;		/*buffer for raw data	*/
    hid_t		sm_space;		/*stripmine data space	*/

    /* Hyperslab info */
    hssize_t		hs_offset[H5S_MAX_RANK];/*starting offset	*/
    hsize_t		hs_size[H5S_MAX_RANK];	/*size this pass	*/
    hsize_t		hs_nelmts;		/*elements in request	*/

    hsize_t		dim_n_size;

    /*
     * Check that everything looks okay.  The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof ctx);
    ctx.need_prefix = 1;
    f_space = H5Dget_space(dset);
    ctx.ndims = H5Sget_simple_extent_ndims(f_space);
    if ((size_t)(ctx.ndims)>NELMTS(sm_size)) return -1;

    /* Assume entire data space to be printed */
    for (i=0; i<(hsize_t)(ctx.ndims); i++) ctx.p_min_idx[i] = 0;
    H5Sget_simple_extent_dims(f_space, total_size, NULL);
    for (i=0, p_nelmts=1; i<(hsize_t)(ctx.ndims); i++) {
	p_nelmts *= total_size[i];
    }
    if (0==p_nelmts) return 0; /*nothing to print*/

    /*
     * Determine the strip mine size and allocate a buffer.  The strip mine is
     * a hyperslab whose size is manageable.
     */
    p_type_nbytes = H5Tget_size(p_type);
    for (i=ctx.ndims, sm_nbytes=p_type_nbytes; i>0; --i) {
	sm_size[i-1] = MIN (total_size[i-1], H5DUMP_BUFSIZE/sm_nbytes);
	sm_nbytes *= sm_size[i-1];
	assert(sm_nbytes>0);
    }
    sm_buf = malloc(sm_nbytes);
    sm_nelmts = sm_nbytes/p_type_nbytes;
    sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

    /* The stripmine loop */
    memset(hs_offset, 0, sizeof hs_offset);
    memset(zero, 0, sizeof zero);
    for (elmtno=0; elmtno<p_nelmts; elmtno+=hs_nelmts) {

	/* Calculate the hyperslab size */
	if (ctx.ndims>0) {
	    for (i=0, hs_nelmts=1; i<(hsize_t)(ctx.ndims); i++) {
		hs_size[i] = MIN(total_size[i]-hs_offset[i], sm_size[i]);
		ctx.p_max_idx[i] = ctx.p_min_idx[i] + hs_size[i];
		hs_nelmts *= hs_size[i];
	    }
	    H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL,
				hs_size, NULL);
	    H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL,
				&hs_nelmts, NULL);
	    dim_n_size = total_size[ctx.ndims-1];
	} else {
	    H5Sselect_all(f_space);
	    H5Sselect_all(sm_space);
	    hs_nelmts = 1;
	    dim_n_size = 1;
	}
	
	/* Read the data */
	if (H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf)<0) {
	    return -1;	
	}

	/* Print the data */
	flags = ((0==elmtno ? START_OF_DATA : 0) |
		 (elmtno+hs_nelmts>=p_nelmts ? END_OF_DATA : 0));
	if (programtype == UNKNOWN){
		return FAIL;
	} else if (programtype == H5LS){
	    h5dump_simple_data(stream, info, dset, &ctx, flags, hs_nelmts,
			       p_type, sm_buf);
	} else if (programtype == H5DUMP){
	    switch (H5Tget_class(p_type)) {
	    case H5T_INTEGER:
		display_numeric_data (hs_nelmts, p_type, sm_buf, p_type_nbytes,
				      p_nelmts, dim_n_size, elmtno, dset);
		break;
			
	    case H5T_FLOAT:
		display_numeric_data (hs_nelmts, p_type, sm_buf, p_type_nbytes,
				      p_nelmts, dim_n_size, elmtno, dset);
		break;
			
	    case H5T_TIME:
		break;
			
	    case H5T_STRING:
		display_string (hs_nelmts, p_type, sm_buf, p_type_nbytes, 
				p_nelmts, dim_n_size, elmtno);
		break;
			
	    case H5T_BITFIELD:
		break;
			
	    case H5T_OPAQUE:
		break;
			
	    case H5T_COMPOUND:
		compound_data = 1;
		display_compound_data (hs_nelmts, p_type, sm_buf,
				       p_type_nbytes, p_nelmts, elmtno);
		compound_data = 0;
		break;
			
	    case H5T_REFERENCE:
		display_numeric_data(hs_nelmts, p_type, sm_buf, p_type_nbytes,
				     p_nelmts, dim_n_size, elmtno, dset);
		break;
#if 0
		display_reference_data(hs_nelmts, p_type, sm_buf,
				       p_type_nbytes, p_nelmts, dim_n_size,
				       elmtno, dset);
#endif
	    case H5T_ENUM:
		    display_numeric_data(hs_nelmts, p_type, sm_buf,
					 p_type_nbytes, p_nelmts, dim_n_size,
					 elmtno, dset);
		    break;	

	    default:
		break;
	    }
		
	}
	
	/* Calculate the next hyperslab offset */
	for (i=ctx.ndims, carry=1; i>0 && carry; --i) {
	    ctx.p_min_idx[i-1] = ctx.p_max_idx[i-1];
	    hs_offset[i-1] += hs_size[i-1];
	    if (hs_offset[i-1]==(hssize_t)(total_size[i-1])) {
		hs_offset[i-1] = 0;
	    } else {
		carry = 0;
	    }
	}
	ctx.continuation++;
    }

    /* Terminate the output */
    if (ctx.cur_column) {
	fputs(OPT(info->line_suf, ""), stream);
	putc('\n', stream);
	fputs(OPT(info->line_sep, ""), stream);
    }
    H5Sclose(sm_space);
    H5Sclose(f_space);
    if (sm_buf) free(sm_buf);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_simple_mem
 *
 * Purpose:	Print some values from memory with a simple data space.
 *		This is a special case of h5dump_mem().
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
h5dump_simple_mem(FILE *stream, const h5dump_t *info, hid_t type,
		  hid_t space, void *mem)
{
    hsize_t		i;			/*counters		*/
    hsize_t		nelmts;			/*total selected elmts	*/
    h5dump_context_t	ctx;			/*printing context	*/

    /*
     * Check that everything looks okay.  The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof ctx);
    ctx.need_prefix = 1;
    ctx.ndims = H5Sget_simple_extent_ndims(space);
    if ((size_t)(ctx.ndims)>NELMTS(ctx.p_min_idx)) return -1;

    /* Assume entire data space to be printed */
    for (i=0; i<(hsize_t)(ctx.ndims); i++) ctx.p_min_idx[i] = 0;
    H5Sget_simple_extent_dims(space, ctx.p_max_idx, NULL);
    for (i=0, nelmts=1; i<(hsize_t)(ctx.ndims); i++) {
	nelmts *= ctx.p_max_idx[i] - ctx.p_min_idx[i];
    }
    if (0==nelmts) return 0; /*nothing to print*/

    /* Print it */
    h5dump_simple_data(stream, info, -1/*no dataset*/, &ctx,
		       START_OF_DATA|END_OF_DATA, nelmts, type, mem);

    /* Terminate the output */
    if (ctx.cur_column) {
	fputs(OPT(info->line_suf, ""), stream);
	putc('\n', stream);
	fputs(OPT(info->line_sep, ""), stream);
    }
    
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_fixtype
 *
 * Purpose:	Given a file data type choose a memory data type which is
 *		appropriate for printing the data.
 *
 * Return:	Success:	Memory data type
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-06-04
 *		Added support for references.
 *
 *-------------------------------------------------------------------------
 */

hid_t
h5dump_fixtype(hid_t f_type)
{
    hid_t	m_type = FAIL, f_memb;
    hid_t	*memb = NULL;
    char	**name = NULL;
    int		nmembs = 0, i, j, *ndims = NULL;
    size_t	size, offset, *dims = NULL, nelmts;
    H5T_str_t strpad;

    size = H5Tget_size(f_type);
    switch (H5Tget_class(f_type)) {

    case H5T_INTEGER:
	/*
	 * Use the smallest native integer type of the same sign as the file
	 * such that the memory type is at least as large as the file type.
	 * If there is no memory type large enough then use the largest
	 * memory type available.
	 */
	if (size<=sizeof(char)) {
	    m_type = H5Tcopy(H5T_NATIVE_SCHAR);
	} else if (size<=sizeof(short)) {
	    m_type = H5Tcopy(H5T_NATIVE_SHORT);
	} else if (size<=sizeof(int)) {
	    m_type = H5Tcopy(H5T_NATIVE_INT);
	} else if (size<=sizeof(long)) {
	    m_type = H5Tcopy(H5T_NATIVE_LONG);
	} else {
	    m_type = H5Tcopy(H5T_NATIVE_LLONG);
	}
	H5Tset_sign(m_type, H5Tget_sign(f_type));
	break;
	
    case H5T_FLOAT:
	/*
	 * Use the smallest native floating point type available such that
	 * its size is at least as large as the file type.  If there is not
	 * native type large enough then use the largest native type.
	 */
	if (size<=sizeof(float)) {
	    m_type = H5Tcopy(H5T_NATIVE_FLOAT);
	} else if (size<=sizeof(double)) {
	    m_type = H5Tcopy(H5T_NATIVE_DOUBLE);
	} else {
	    m_type = H5Tcopy(H5T_NATIVE_LDOUBLE);
	}
	break;
	
    case H5T_STRING:
	/*
	 * This is needed because the function in dumputil.c is the case where
	 * strDUAction == TRUE. if it is false we will do the original action
	 * here.
	 */
	if (programtype == H5DUMP) {
	    m_type = H5Tcopy(H5T_C_S1);
	    H5Tset_size(m_type, size);
	    strpad = H5Tget_strpad(f_type) ;
	    H5Tset_strpad(m_type, strpad);
		
	    if (H5Tequal(m_type,f_type) < 0) {
		H5Tclose(m_type);
		m_type = H5Tcopy(H5T_FORTRAN_S1);
		H5Tset_size(m_type, size);
		H5Tset_strpad(m_type, strpad);
		if (H5Tequal(m_type,f_type) < 0) 
		    m_type = -1;
	    } 
	} else if (programtype == H5LS) {
	    m_type = H5Tcopy(f_type);
	    H5Tset_cset(m_type, H5T_CSET_ASCII);
	} else if (programtype == UNKNOWN){
	    return FAIL;
	}
	break;

    case H5T_COMPOUND:
	/*
	 * We have to do this in two steps.  The first step scans the file
	 * type and converts the members to native types and remembers all
	 * their names and sizes, computing the size of the memory compound
	 * type at the same time.  Then we create the memory compound type
	 * and add the members.
	 */
	nmembs = H5Tget_nmembers(f_type);
	memb = calloc(nmembs, sizeof(hid_t));
	name = calloc(nmembs, sizeof(char*));
	ndims = calloc(nmembs, sizeof(int));
	dims = calloc(nmembs*4, sizeof(size_t));
	
	for (i=0, size=0; i<nmembs; i++) {

	    /* Get the member type and fix it */
	    f_memb = H5Tget_member_type(f_type, i);
	    memb[i] = h5dump_fixtype(f_memb);
	    H5Tclose(f_memb);
	    if (memb[i]<0) goto done;

	    /* Get the member dimensions */
	    ndims[i] = H5Tget_member_dims(f_type, i, dims+i*4, NULL);
	    assert(ndims[i]>=0 && ndims[i]<=4);
	    for (j=0, nelmts=1; j<ndims[i]; j++) nelmts *= dims[i*4+j];

	    /* Get the member name */
	    name[i] = H5Tget_member_name(f_type, i);
	    if (NULL==name[i]) goto done;

	    /*
	     * Compute the new offset so each member is aligned on a byte
	     * boundary which is the same as the member size.
	     */
	    size = ALIGN(size, H5Tget_size(memb[i])) +
		     nelmts * H5Tget_size(memb[i]);
	}

	m_type = H5Tcreate(H5T_COMPOUND, size);
	for (i=0, offset=0; i<nmembs; i++) {
	    H5Tinsert_array(m_type, name[i], offset, ndims[i], dims+i*4,
			    NULL, memb[i]);
	    for (j=0, nelmts=1; j<ndims[i]; j++) nelmts *= dims[i*4+j];
	    offset = ALIGN(offset, H5Tget_size(memb[i])) +
		     nelmts * H5Tget_size(memb[i]);
	}
	break;

    case H5T_ENUM:
    case H5T_REFERENCE:
    case H5T_OPAQUE:
	/* Same as file type */
	m_type = H5Tcopy(f_type);
	break;

    case H5T_BITFIELD:
	/*
	 * Same as the file except the offset is set to zero and the byte
	 * order is set to little endian.
	 */
	m_type = H5Tcopy(f_type);
	H5Tset_offset(m_type, 0);
	H5Tset_order(m_type, H5T_ORDER_LE);
	break;

    case H5T_TIME:
	/*
	 * These type classes are not implemented yet.
	 */
	break;

    default:
	/* What the heck? */
	break;
    }

 done:
    /* Clean up temp buffers */
    if (memb && name && ndims && dims) {
	for (i=0; i<nmembs; i++) {
	    if (memb[i]>=0) H5Tclose(memb[i]);
	    if (name[i]) free(name[i]);
	}
	free(memb);
	free(name);
	free(ndims);
	free(dims);
    }
    
    return m_type;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_dset
 *
 * Purpose:	Print some values from a dataset DSET to the file STREAM
 *		after converting all types to P_TYPE (which should be a
 *		native type).  If P_TYPE is a negative value then it will be
 *		computed from the dataset type using only native types.
 *
 * Note:	This function is intended only for datasets since it does
 *		some things like strip mining which are unnecessary for
 *		smaller objects such as attributes. The easiest way to print
 *		small objects is to read the object into memory and call
 *		h5dump_mem().
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-06-07
 *		If info->raw is set then the memory datatype will be the same
 *		as the file datatype.
 *
 *-------------------------------------------------------------------------
 */
int
h5dump_dset(FILE *stream, const h5dump_t *info, hid_t dset, hid_t _p_type)
{
    hid_t	f_space;
    hid_t	p_type = _p_type;
    hid_t	f_type;
    int		status;
    h5dump_t	info_dflt;

    /* Use default values */
    if (!stream) stream = stdout;
    if (!info) {
	memset(&info_dflt, 0, sizeof info_dflt);
	info = &info_dflt;
    }
    if (p_type<0) {
	f_type = H5Dget_type(dset);
	if (info->raw) {
	    p_type = H5Tcopy(f_type);
	} else {
	    p_type = h5dump_fixtype(f_type);
	}
	H5Tclose(f_type);
	if (p_type<0) return -1;
    }

    /* Check the data space */
    f_space = H5Dget_space(dset);
    if (H5Sis_simple(f_space)<=0) return -1;
    H5Sclose(f_space);

    /* Print the data */
    status = h5dump_simple_dset(stream, info, dset, p_type);
    if (p_type!=_p_type) H5Tclose(p_type);
    return status;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_mem
 *
 * Purpose:	Displays the data contained in MEM. MEM must have the
 *		specified data TYPE and SPACE.  Currently only simple data
 *		spaces are allowed and only the `all' selection.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
h5dump_mem(FILE *stream, const h5dump_t *info, hid_t type, hid_t space,
	   void *mem)
{
    h5dump_t	info_dflt;
    
    /* Use default values */
    if (!stream) stream = stdout;
    if (!info) {
	memset(&info_dflt, 0, sizeof info_dflt);
	info = &info_dflt;
    }

    /* Check the data space */
    if (H5Sis_simple(space)<=0) return -1;
    return h5dump_simple_mem(stream, info, type, space, mem);
}





/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

/*from h5dumputil.c*/

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	display_numeric_data
 *
 * Purpose:	Display numeric data in ddl format.
 *
 * Return:	void
 *
 * Comment:     hs_nelmts     number of elements to be printed
 *              p_type        memory data type
 *              sm_buf        data buffer
 *              p_type_nbytes size of p_type
 *              p_nelmts      total number of elements
 *              dim_n_size    size of dimemsion n
 *              elmtno        element index
 *
 *-------------------------------------------------------------------------
 */
static void display_numeric_data 
(hsize_t hs_nelmts, hid_t p_type, unsigned char *sm_buf, size_t p_type_nbytes, 
 hsize_t p_nelmts, hsize_t dim_n_size, hsize_t elmtno, hid_t container) {

hsize_t i;
/*char p_buf[256];		*/
char* out_buf = malloc(sizeof(char) * nCols);
struct h5dump_str_t tempstr;
hsize_t x;
hbool_t isref = FALSE;
hsize_t totalspace;
hbool_t done;
int temp;


/******************************************************************************************/
    h5dump_t		info;

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);
    info.idx_fmt = "(%s)";
    info.line_ncols = nCols;
    info.line_multi_new = 1;
 
    /*
     * If a compound datatype is split across multiple lines then add an
     * ellipsis to the beginning of the continuation line.
     */
    info.line_pre  = "        %s ";
    info.line_cont = "        %s  ";
/*********************************************************************************************/


	/* i added this too*/
	if (H5Tequal(p_type, H5T_STD_REF_DSETREG)) {
		isref = TRUE;
	}

	
    out_buf[0] = '\0';
    if ((indent+COL) > nCols) indent = 0;
	memset(&tempstr, 0, sizeof(h5dump_str_t));

    for (i=0; i<hs_nelmts && (elmtno+i) < p_nelmts; i++) {
		h5dump_str_reset(&tempstr);  
		h5dump_sprint(&tempstr, &info, container, p_type, sm_buf+i*p_type_nbytes);
		if (isref) {
			for (x = 0; x <tempstr.len; x++){
			/* removes the strange characters */
				if (tempstr.s[x] == 1){
					memmove(tempstr.s+x, tempstr.s+(x+1), strlen(tempstr.s+x));
					tempstr.len --;
				}

			}
		}
		totalspace = nCols - indent - COL;

         if ((int)(strlen(out_buf)+tempstr.len+1) > (nCols-indent-COL)) {
			 if (isref){
			/* i added this */
				 temp = (int)strlen(out_buf);
				 if ((strlen(out_buf) + 7) < (totalspace)){ /* 7 for the word dataset */
					 memcpy(out_buf+strlen(out_buf), tempstr.s, totalspace - strlen(out_buf));
					 out_buf[totalspace] = '\0';
					 memmove(tempstr.s, tempstr.s+(totalspace - temp), tempstr.len - (totalspace - temp));
					 tempstr.s[tempstr.len - totalspace + temp] = '\0';
					 tempstr.len = strlen(tempstr.s);
				 }
			 }
             /* first row of member */
             if (compound_data && (elmtno+i+1) == dim_n_size)
                 printf("%s\n", out_buf);
             else {
                 indentation(indent+COL);
                 printf("%s\n", out_buf);
             }
             strcpy(out_buf, tempstr.s);

			/* i added this too*/
			 if (isref) {
				 done = FALSE;
				 while (!done) {
					 if (tempstr.len > totalspace) {
						 /* keep printing until we can fit in the totalspace */
						 memmove(out_buf,tempstr.s, totalspace);
						 out_buf[totalspace] = '\0';
						 memmove(tempstr.s,tempstr.s+totalspace, strlen(tempstr.s + totalspace));
						 tempstr.s[tempstr.len - totalspace] = '\0';
						 tempstr.len = strlen(tempstr.s);
						 indentation(indent+COL);
						 printf(out_buf);
						 
					 } else {
						 strcpy(out_buf, tempstr.s);
						 done = TRUE;
					 }
				 }
			 }

             if ((elmtno+i+1) % dim_n_size) 
                 strcat(out_buf, ", ");
             else { /* end of a row, flush out_buf */
                 indentation(indent+COL);
                 printf("%s", out_buf);
                 if ((elmtno+i+1) != p_nelmts) /* not last element */
                     printf(",\n");
                 else if (compound_data) { /* last element of member data*/
                     if ((nCols-strlen(out_buf)-indent-COL) < 2) { 
                          /* 2 for space and ] */
                         printf("\n");
                         indentation(indent+COL-3);
                     }
                 } else
                     printf("\n"); /* last row */
                 *out_buf = '\0';
             }
        } else {
             strcat(out_buf, tempstr.s);
             if ((elmtno+i+1) % dim_n_size) {
                  if ((nCols-strlen(out_buf)-indent-COL-1) > 0)
                      strcat(out_buf, ", ");
                  else 
                      strcat(out_buf, ",");
             } else { /* end of a row */
                 /* 1st row of member data */
                 if (compound_data && (elmtno+i+1) == dim_n_size) 
                     printf("%s", out_buf);
                 else {
                     indentation(indent+COL);
                     printf("%s", out_buf);
                 }

                 /* if it's the last element */
                 if ((elmtno+i+1) != p_nelmts)
                     printf(",\n");
                 else if (compound_data) { /* last row of member data*/
                     /* 2 for space and ] */
                     if ((nCols-strlen(out_buf)-indent-COL) < 2) { 
                         printf("\n");
                         indentation(indent+COL-3);
                     }
                 } else
                     printf("\n"); /* last row */
                 *out_buf = '\0';
             }
        }
    }
#if !defined (WIN32) && !defined (_DEBUG)
	free(out_buf);
#endif
}


/*-------------------------------------------------------------------------
 * Function:	display_string
 *
 * Purpose:	Display string in ddl format
 *
 * Return:	void
 *
 * Comment:     concatenator operator : '//'
 *              separator between elements: ','
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void display_string
(hsize_t hs_nelmts, hid_t p_type, unsigned char *sm_buf, size_t p_type_nbytes, 
 hsize_t p_nelmts, hsize_t dim_n_size, hsize_t elmtno) {
	hsize_t i, row_size=0;
	int j, m, x, y, first_row=1;
	int free_space, long_string = 0;
	char* out_buf =  malloc(sizeof(char) * nCols);
	struct h5dump_str_t tempstr;
	int temp;

/******************************************************************************************/
    h5dump_t		info;

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);
    info.idx_fmt = "(%s)";
    info.line_ncols = nCols;
    info.line_multi_new = 1;
 
    /*
     * If a compound datatype is split across multiple lines then add an
     * ellipsis to the beginning of the continuation line.
     */
    info.line_pre  = "        %s ";
    info.line_cont = "        %s  ";
/*********************************************************************************************/
    out_buf[0] = '\0';
	
	memset(&tempstr, 0, sizeof(h5dump_str_t));
	
	
	h5dump_str_reset(&tempstr); 
    for (i=0; i<hs_nelmts && (elmtno+i) < p_nelmts; i++) {
		row_size++;
		
		h5dump_str_reset(&tempstr);		
		h5dump_sprint(&tempstr, &info, -1/*no container*/, p_type,
			      sm_buf+i*p_type_nbytes);

		memmove(tempstr.s, tempstr.s + 1, tempstr.len -1);
		tempstr.s[tempstr.len - 2] = '\0';
		tempstr.len = tempstr.len - 2;

		free_space = (int)(nCols - indent - COL - strlen(out_buf));

         if ((elmtno+i+1) == p_nelmts) { /* last element */
            /* 2 for double quotes */
            if (((int)tempstr.len + 2) > free_space) long_string = 1;
         } else 
            /* 3 for double quotes and one comma */
            if (((int)tempstr.len + 3) > free_space) long_string = 1;

         if (long_string) {

             if (free_space < 5) {  /* 5 for double quotes, one space and two '/'s */
                 /* flush out_buf */
                 if (compound_data && first_row) {
                     printf("%s\n", out_buf);
                     first_row = 0;
                 } else {
                     indentation(indent+COL); 
                     printf("%s\n", out_buf);
                 }
                 out_buf[0] = '\0';
                 x = 0 ;
             } else {
                 x = free_space - 5;
                 if (compound_data && first_row) {
                     printf("%s\"", out_buf);
                     strncpy(out_buf, tempstr.s, x);
                     out_buf[x] = '\0';
                     printf("%s\" %s\n", out_buf,CONCATENATOR);
                     first_row = 0;
                     out_buf[0] = '\0';
                 } else {
                     indentation(indent+COL);     
					 printf("%s\"", out_buf);
					 memset(out_buf, '\0', nCols); 
					 temp = copy_atomic_char(out_buf,tempstr.s,(int)tempstr.len,x);
                     out_buf[x] = '\0';
					 printf("%s\" %s\n",out_buf,CONCATENATOR);
					 x = temp;
                 }
             }

             y = nCols - indent -COL - 5;

             m = (int)((tempstr.len - x)/y);

             for (j = 0; j < m - 1 ; j++) {
                  indentation(indent+COL);
                  strncpy(out_buf, tempstr.s+x+j*y, y);
                  out_buf[y] = '\0';
                  printf("\"%s\" %s\n", out_buf,CONCATENATOR);
             }

             if ((elmtno+i+1) == p_nelmts) { /* last element */
                  if ((int)strlen(tempstr.s+x+j*y) > (nCols - indent - COL -2)) { /* 2 for double quotes */
                     indentation(indent+COL);
                     strncpy(out_buf, tempstr.s+x+j*y, y);
                     out_buf[y] = '\0';
                     printf("\"%s\" %s\n", out_buf, CONCATENATOR);
                     indentation(indent+COL);
                     printf("\"%s\"", tempstr.s+x+m*y);
                     if (compound_data) {
                         if ((nCols-strlen(out_buf)-indent-COL) < 2) {
                              printf("\n");
                              indentation(indent+COL-3);
                         }
                     } else 
                         printf("\n");

                  } else {
                     indentation(indent+COL);
                     printf("\"%s\"", tempstr.s+x+j*y);
                     if (compound_data) {
                         if ((nCols-strlen(out_buf)-indent-COL) < 2) {
                              printf("\n");
                              indentation(indent+COL-3);
                         }
  
                     } else
                         printf("\n");
                  }
                  out_buf[0] = '\0';
             } else if ( row_size == dim_n_size) {
                  if ((int)strlen(tempstr.s+x+j*y) > (nCols - indent - COL -3)) { /* 3 for 2 "'s and 1 , */
                     indentation(indent+COL);
                     strncpy(out_buf, tempstr.s+x+j*y, y);
                     out_buf[y] = '\0';
                     printf("\"%s\" %s\n", out_buf, CONCATENATOR);
                     indentation(indent+COL);
                     printf("\"%s\",\n", tempstr.s+x+m*y);
                  } else {
                     indentation(indent+COL);
                     printf("\"%s\",\n", tempstr.s+x+j*y);

                  }
                  out_buf[0] = '\0';
                  row_size = 0;

             } else {
                  if ((int)strlen(tempstr.s+x+j*y) > (nCols - indent - COL -3)) { /* 3 for 2 "'s and 1 , */
                     indentation(indent+COL);
                     strncpy(out_buf, tempstr.s+x+j*y, y);
                     out_buf[y] = '\0';
                     printf("\"%s\" %s\n", out_buf, CONCATENATOR);
                     strcpy(out_buf, "\"");
                     strcat(out_buf, tempstr.s+x+m*y);
                     strcat(out_buf, "\",");
                     if ((int)strlen(out_buf) < (nCols-indent-COL)) strcat(out_buf, " "); 
                  } else {
					 strcpy(out_buf, "\"");
                     strcat (out_buf, tempstr.s+x+j*y);
                     strcat(out_buf, "\",");
                     if ((int)strlen(out_buf) < (nCols-indent-COL)) strcat(out_buf, " "); 
                  }
             }
             long_string = 0;

         } else {

            /* flush out_buf if it's end of a row */
            if (row_size == dim_n_size) {
                if (compound_data && (elmtno+i+1) == dim_n_size) { /* 1st row */

                    printf("%s\"%s\"", out_buf, tempstr.s);
                    first_row = 0;
                } else {
                    indentation(indent+COL); 

					printf("%s\"%s\"", out_buf, tempstr.s);
                }

               if ((elmtno+i+1) != p_nelmts) 
                   printf(",\n");
               else if (compound_data) {
                       if ((nCols-strlen(out_buf)-tempstr.len-indent-COL) < 2) {
                           /* 2 for space and ] */
                           printf("\n");
                           indentation(indent+COL-3);
                       }
               } else
                   printf("\n");
   
               out_buf[0] = '\0';
               row_size = 0;
            } else {
                 strcat(out_buf, "\""); 
                 strcat(out_buf, tempstr.s);
                 strcat(out_buf, "\",");
                 if ((int)strlen(out_buf) < (nCols-indent-COL)) strcat(out_buf, " ");
            }

         }
    }
	free(out_buf);
}


/*-------------------------------------------------------------------------
 * Function:	display_compound_data
 *
 * Purpose:	Display compound data in ddl format
 *
 * Return:	void
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
 static void display_compound_data
    (hsize_t hs_nelmts, hid_t p_type, unsigned char *sm_buf, size_t p_type_nbytes, 
	 hsize_t p_nelmts, hsize_t elmtno) {


	 size_t  offset, size, dims[4]; 
	 hsize_t nelmts, dim_n_size=0;
	 hid_t   memb;
	 int     nmembs, i, j, k, ndims, perm[4];
	 
	 if ((indent+COL) > nCols) indent = 0;
	 
	 for (i=0; i<(int)hs_nelmts && (elmtno+i) < p_nelmts; i++) {
		 
		 nmembs = H5Tget_nmembers(p_type);
		 
		 indentation(indent+COL); 
		 printf("{\n");
		 
		 indent+= COL;
		 for (j=0; j<nmembs; j++) {
			 
			 offset = H5Tget_member_offset(p_type, j);
			 memb = H5Tget_member_type(p_type, j);
			 size = H5Tget_size(memb);
			 ndims = H5Tget_member_dims(p_type, j, dims, perm);
			 if (ndims > 0) dim_n_size = dims[ndims-1];
			 else dim_n_size = 1;
			 for (k=0, nelmts=1; k<ndims; k++) nelmts *= dims[k];
			 

			 

			 switch (H5Tget_class(memb)) {
			 case H5T_INTEGER:
				 indentation(indent+COL);
				 indent += 2;
				 if (nelmts > 1) {
					 printf("[ ");
				 }
                 display_numeric_data
					 (nelmts, memb, sm_buf+offset+i*p_type_nbytes, size, nelmts, dim_n_size, 0, -1) ;
				 if (nelmts > 1) {
					 printf(" ]");
				 }
				 indent -= 2;
                 break;
				 
			 case H5T_FLOAT:
				 indentation(indent+COL);
				 indent += 2;
				 if (nelmts > 1) {
					 printf("[ ");
				 }                
				 display_numeric_data
					 (nelmts, memb, sm_buf+offset+i*p_type_nbytes, size, nelmts, dim_n_size, 0, -1) ;
				 if (nelmts > 1) {
					 printf(" ]");
				 }
				 indent -= 2;
                 break;
				 
			 case H5T_TIME:

                 break;
				 
			 case H5T_STRING:
				 indentation(indent+COL);
				 indent += 2;				 
				 if (nelmts > 1) {
					 printf("[ ");
				 }                               
				 display_string
					 (nelmts, memb, sm_buf+offset+i*p_type_nbytes, size, nelmts, dim_n_size, 0 ) ;
				 if (nelmts > 1) {
					 printf(" ]");
				 }	
				 indent -= 2;                 
				 break;
				 
			 case H5T_BITFIELD:

                 break;
				 
			 case H5T_OPAQUE:
                 
				 break;
			 case H5T_COMPOUND:
				 display_compound_data (nelmts, memb, sm_buf+offset+i*p_type_nbytes, size, nelmts, 0);
				 indentation(indent);
				 break;
			 default: break;
				 
			 }

			 if ( j == nmembs-1) printf("\n");
			 else printf(",\n");

			 H5Tclose(memb);
		 }
		 indent-= COL;
		 
		 indentation(indent+COL);
		 if ((elmtno+i+1) == p_nelmts) printf("}\n");
		 else printf("},\n");
	 }
	 
 }
/*-------------------------------------------------------------------------
 * Function:	print_data	
 *
 * Purpose:	Print some values from a dataset or an attribute to the 
 *              file STREAM after converting all types to P_TYPE (which
 *              should be a native type).  If P_TYPE is a negative value 
 *              then it will be computed from the dataset/attribute type 
 *              using only native types.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
print_data(hid_t oid, hid_t _p_type, int obj_data)
{
    hid_t	f_space;
    hid_t	p_type = _p_type;
    hid_t	f_type;
    int		status = -1;

    if (p_type < 0) {

        if (obj_data == DATASET_DATA) 
            f_type = H5Dget_type(oid);
        else
            f_type = H5Aget_type(oid);

        if (f_type < 0) return status;

	p_type = h5dump_fixtype(f_type);

	H5Tclose(f_type);

	if (p_type < 0) return status;
    }

    /* Check the data space */
    if (obj_data == DATASET_DATA) 
        f_space = H5Dget_space(oid);
    else 
        f_space = H5Aget_space(oid);

    if (f_space < 0) return status;
 
    if (H5Sis_simple(f_space) >= 0) {
		if (obj_data == DATASET_DATA) {
			status = h5dump_simple_dset(NULL,NULL, oid,p_type);
		}
		else { /*attribute data*/
			status = h5dump_attr(oid,p_type);
		}
	}
    H5Sclose(f_space);

    if (p_type != _p_type) H5Tclose(p_type);

    return status;
}

/*-------------------------------------------------------------------------
 * Function:    indentation
 *
 * Purpose:     Print spaces for indentation
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void indentation(int x) {

	if(x < nCols) {
		while (x>0) { printf(" "); x--; }
	}
	else {
		printf("The indentation exceeds the number of cols. Exiting....\n");
		exit(1);
	}
}



/*-------------------------------------------------------------------------
 * Function:    copy_atomic_char
 *
 * Purpose:     copies the atomic characters from 1 string to another
 *              assumes there will be enough room in output for the input string
 *
 * Return:      returns the number of actual characters copied
 *
 * Programmer:  Patrick Lu
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
int copy_atomic_char(char* output, char* input, int numchar, int freespace){

	int x = 0;

	while (freespace || (x == numchar)){
		if (input[x] == '\\'){
			if (freespace == 1){
				break;				
			}
			else {
				if ((input[x+1] == '"') || (input[x+1] == '\\') || 
					(input[x+1] == 'b') || (input[x+1] == 'f') || 
					(input[x+1] == 'n') || (input[x+1] == 'r') || 
					(input[x+1] == 't')){ /*escape characters*/
					strncat(output,input,2);
					x += 2;
					freespace = freespace - 2;
				}
				else { /* octal number */
					if (freespace < 4){
						break;
					}
					else {
						strncat(output,input,4);
						x += 4;
						freespace = freespace - 4;
					}
				}
			}
		}
		else {
			strncat(output,input+x,1);
			freespace = freespace - 1;
			x++;
		}
	}

	if (x == 0) x = FAIL;
	return(x);
}

/*-------------------------------------------------------------------------
 * Function:    h5dump_attr
 *
 * Purpose:    dumps an attribute
 *
 * Return:      0 = succeed or -1 for fail
 *
 * Programmer:  Patrick Lu
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/

int h5dump_attr(hid_t oid, hid_t p_type){
	
	hid_t f_space;
	void *sm_buf;
	hid_t type;
	hsize_t size[64], nelmts = 1, dim_n_size;
	size_t p_type_nbytes, need;
	int ndims, i;
	int status = -1;
	
	f_space = H5Aget_space(oid);

	/* get the size of the attribute and allocate enough mem*/
	type = H5Aget_type(oid);
	ndims = H5Sget_simple_extent_dims(f_space, size, NULL);
	if (ndims){
		for (i = 0; i < ndims; i++){
			nelmts *= size[i];	
		}
		dim_n_size = size[ndims - 1];
	}
	else {
	   dim_n_size = 1;
	}
	need = nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type));
	sm_buf = malloc(need);
	p_type_nbytes = H5Tget_size(p_type);
	
	/*read the attr*/
	if (H5Aread(oid, p_type, sm_buf) < 0){
		return (status);
	}
	status = 0;
	/*print it*/
	switch (H5Tget_class(p_type)) {
	case H5T_INTEGER:
		display_numeric_data (nelmts, p_type, sm_buf, p_type_nbytes, 
			nelmts, dim_n_size, 0, oid);
		break;
		
	case H5T_FLOAT:
		display_numeric_data (nelmts, p_type, sm_buf, p_type_nbytes, 
			nelmts, dim_n_size, 0, oid);
		break;
		
	case H5T_TIME:
		break;
		
	case H5T_STRING:
		display_string (nelmts, p_type, sm_buf, p_type_nbytes, 
			nelmts, dim_n_size, 0);
		break;
		
	case H5T_BITFIELD:
		break;
		
	case H5T_OPAQUE:
		break;
		
	case H5T_COMPOUND:
		compound_data = 1;
		display_compound_data (nelmts, p_type, sm_buf, p_type_nbytes, nelmts, 0);
		compound_data = 0;
		break;
	case H5T_REFERENCE:
		display_numeric_data(nelmts, p_type, sm_buf, p_type_nbytes, 
			nelmts, dim_n_size, 0, oid);
	case H5T_ENUM:
		display_numeric_data(nelmts, p_type, sm_buf, p_type_nbytes, 
			nelmts, dim_n_size, 0, oid);
/*		display_reference_data(nelmts, p_type, sm_buf, p_type_nbytes, 
			nelmts, dim_n_size, 0, oid);*/
		break;
	default: break;
	}
	free(sm_buf);
	return (status);
}

/* Print the program name and the version information which is */
/* defined the same as the HDF5 library version. */
void print_version(char *program_name)
{
    printf("%s: Version %u.%u.%u%s\n",
       program_name, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE,
       H5_VERS_SUBRELEASE);
}






/*

THE FUNCTIONS BELOW ARE FROM THE H5FINSHD.C FILE

*/

/*-------------------------------------------------------------------------
 * Function:    init_table
 *
 * Purpose:     allocate and initialize tables for shared groups, datasets, 
 *              and committed types
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void init_table (table_t** temp){

	int i;
	table_t *table = malloc(sizeof(table_t));

    table->size = 20;
    table->nobjs = 0;

    table->objs = (obj_t*) malloc(table->size*sizeof(obj_t));
  

    for (i = 0; i < table->size; i++) {
         table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
         table->objs[i].displayed = 0;
         table->objs[i].recorded = 0;
		 table->objs[i].objflag = 0;
    }
	*temp = table;

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
 *-----------------------------------------------------------------------*/
void init_prefix(char **prefix, int prefix_len){
	char *temp;
    temp = (char *) malloc(prefix_len * sizeof (char));
    *temp = '\0';
	*prefix = temp;
}



/*-------------------------------------------------------------------------
 * Function:    free_table
 *
 * Purpose:     free tables for shared groups, datasets, 
 *              and committed types
 *
 * Return:      void
 *
 * Programmer:  Paul Harten
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void
free_table (table_t **table){

	table_t *temp = *table;
        if (temp->objs != NULL) {
		HDfree(temp->objs);
        }
	*table = temp;
}


/*-------------------------------------------------------------------------
 * Function:    search_obj
 *
 * Purpose:     search the object specified by objno in the table
 *
 * Return:      an integer, the location of the object
 *              -1   if object is not found
 *
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
int 
search_obj (table_t *table, unsigned long *objno) {
int i=0, found=0;

    while (i < table->nobjs && !found) 
           if (table->objs[i].objno[0] == *(objno) &&
               table->objs[i].objno[1] == *(objno+1) )  found = 1;
           else     i++; 
  
    if (!found) return -1;
    else return i;

}


/*-------------------------------------------------------------------------
 * Function:    add_obj
 *
 * Purpose:     add a shared object to the table
 *              realloc the table if necessary
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void
add_obj (table_t *table, unsigned long *objno, char *objname) {
int i;

    if (table->nobjs == table->size) {
        table->size *= 2;
        table->objs = realloc (table->objs, table->size*sizeof(obj_t));
        for (i = table->nobjs; i < table->size; i++) {
             table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
             table->objs[i].displayed = 0;
             table->objs[i].recorded = 0;
			 table->objs[i].objflag = 0;
        }
    }

    i = table->nobjs++;
    table->objs[i].objno[0] = *objno;
    table->objs[i].objno[1] = *(objno+1);
    strcpy (table->objs[i].objname, objname);

}
/*-------------------------------------------------------------------------
 * Function:   Find_objs 
 *
 * Purpose:    Find objects, committed types and store them in tables
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
herr_t find_objs(hid_t group, const char *name, void *op_data)
{
	hid_t obj, type;
	H5G_stat_t statbuf;
	char *tmp;
	int i;
	find_objs_t *info = (find_objs_t*)op_data;

	if (info->threshold > 1) { /*will get an infinite loop if greater than 1*/
		return(FAIL);
	}

    H5Gget_objinfo(group, name, TRUE, &statbuf);

    tmp = (char *) malloc ((strlen(info->prefix)+strlen(name)+2) * sizeof(char));

    strcpy(tmp, info->prefix); 

    switch (statbuf.type) {

    case H5G_GROUP:
        if ((obj=H5Gopen (group, name))>=0) {

            if (info->prefix_len < (int)(strlen(info->prefix) + strlen(name) + 2)) {
                info->prefix_len *= 2;
                info->prefix = realloc (info->prefix, info->prefix_len * sizeof(char));
            } 
            strcat(strcat(info->prefix,"/"), name);

            if (statbuf.nlink > info->threshold) {
                if (search_obj (info->group_table,  statbuf.objno) < 0) {
                    add_obj (info->group_table, statbuf.objno, info->prefix); 
                    H5Giterate (obj, ".", NULL, find_objs, (void*)info);
                }
            } else 
                H5Giterate (obj, ".", NULL, find_objs, (void*)info);

            strcpy(info->prefix, tmp);
            H5Gclose (obj);

        } else 
            info->status = 1;

        break;

    case H5G_DATASET:

        strcat(tmp,"/");
        strcat(tmp,name); /* absolute name of the data set */
        if (statbuf.nlink > info->threshold  && 
            search_obj (info->dset_table, statbuf.objno) < 0)
            add_obj (info->dset_table, statbuf.objno, tmp);

        if ((obj=H5Dopen (group, name))>=0) {              
             type = H5Dget_type (obj);
             if (H5Tcommitted(type) > 0 ) {
                 H5Gget_objinfo(type, ".", TRUE, &statbuf);
                 if (search_obj (info->type_table, statbuf.objno) < 0) {
                     add_obj (info->type_table, statbuf.objno, tmp) ;
					 info->type_table->objs[info->type_table->nobjs - 1].objflag = 0;
				 }
             }
             H5Tclose(type);
             H5Dclose (obj);
        } else
             info->status = 1;
            
        break;

    case H5G_TYPE:
         strcat(tmp,"/");
         strcat(tmp,name); /* absolute name of the type */
         i = search_obj (info->type_table, statbuf.objno);
         if (i < 0) {
             add_obj (info->type_table, statbuf.objno, tmp) ;
             info->type_table->objs[info->type_table->nobjs-1].recorded = 1; /* named data type */
			 info->type_table->objs[info->type_table->nobjs-1].objflag = 1; /* named data type */
         } else {
             strcpy (info->type_table->objs[i].objname, tmp);
             info->type_table->objs[i].recorded = 1; 
			 info->type_table->objs[info->type_table->nobjs-1].objflag = 1; /* named data type */  
		 }
         break;

    default:
        break;
    }

    free (tmp);

    return SUCCEED;
}


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
void 
dump_table(char* tablename, table_t *table) {
int i;

    printf("%s: # of entries = %d\n", tablename,table->nobjs);
    for ( i = 0; i < table->nobjs; i++)
        printf ("%lu %lu %s %d\n", table->objs[i].objno[0],
                                   table->objs[i].objno[1],
                                   table->objs[i].objname,
                                   table->objs[i].objflag);

}



/*-------------------------------------------------------------------------
 * Function:   get_table_idx
 *
 * Purpose:    Determine if objects are in a link loop
 *
 * Return:      Success:        table index of object detected to be in loop
 *
 *              Failure:        FAIL
 *
 * Programmer:  Paul Harten
 *
 *-----------------------------------------------------------------------*/
int
get_table_idx(table_t *table, unsigned long *objno)
{
int idx = -1;
/*
    switch (type) {

    case H5G_GROUP:

	idx = search_obj(&group_table, objno);
        break;

    case H5G_DATASET:

	idx = search_obj(&dset_table, objno);
        break;

    case H5G_TYPE:

	idx = search_obj(&type_table, objno);
	break;

    default:

	idx = -1;

    }
*/
   idx = search_obj(table, objno);
    return idx;

}
/*-------------------------------------------------------------------------
 * Function:   Get table flag setting
 *
 * Purpose:    Keep the structures and variables used private to
 *             this file.
 *
 * Return:      Success:        Boolean setting of the i'th element of the
 *                              object table flag
 *
 *              Failure:        FAIL
 *
 * Programmer:  Paul Harten
 *
 *-----------------------------------------------------------------------*/
int
get_tableflag(table_t *table, int idx)
{
  /*
    switch (type) {

    case H5G_GROUP:

	return group_table.objs[idx].objflag;

    case H5G_DATASET:

	return dset_table.objs[idx].objflag;

    case H5G_TYPE:

	return type_table.objs[idx].objflag;

    default:

	return -1;
}
  */
  return(table->objs[idx].objflag);
  

}
/*-------------------------------------------------------------------------
 * Function:   Set table flag setting
 *
 * Purpose:    Keep the structures and variables used private to
 *             this file.
 *
 * Return:      Success:        Boolean setting of the i'th element of the
 *                              object table flag
 *
 *              Failure:        FAIL
 *
 * Programmer:  Paul Harten
 *
 *-----------------------------------------------------------------------*/
int
set_tableflag(table_t *table, int idx)
{
/*
    switch (type) {

    case H5G_GROUP:

	group_table.objs[idx].objflag = TRUE;
	return SUCCEED;

    case H5G_DATASET:

	dset_table.objs[idx].objflag = TRUE;
	return SUCCEED;

    case H5G_TYPE:

	type_table.objs[idx].objflag = TRUE;
	return SUCCEED;

    default:

	return FAIL;
}
*/
    table->objs[idx].objflag = TRUE;
    return(SUCCEED);
    

}
/*-------------------------------------------------------------------------
 * Function:   Get name of i'th object in table
 *
 * Purpose:    
 *
 * Return:      Success:       strdup() of object name character string
 *
 *              Failure:       NULL
 *
 * Programmer:  Paul Harten
 *
 *-----------------------------------------------------------------------*/
char *
get_objectname(table_t* table, int idx)
{
  /*
    switch (type) {

    case H5G_GROUP:

	return strdup(group_table.objs[idx].objname);

    case H5G_DATASET:

	return strdup(dset_table.objs[idx].objname);

    case H5G_TYPE:

	return strdup(type_table.objs[idx].objname);

    default:

	return NULL;

    }
  */

  return(strdup(table->objs[idx].objname));
}








