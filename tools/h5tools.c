/*
 * Copyright © 1998, 2001 National Center for Supercomputing Applications
 *                        All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 23, 1998
 *
 * Purpose:	A library for displaying the values of a dataset in a human
 *		readable format.
 */

/*
 * Portions of this work are derived from _Obfuscated C and Other Mysteries_,
 * by Don Libes, copyright (c) 1993 by John Wiley & Sons, Inc.
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "h5tools.h"
#include "hdf5.h"
#include "H5private.h"
#include "h5dump.h"

/* taken from h5dumputil.c */

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
#define H5DUMP_BUFSIZE          (1024 * 1024)
#else
#define H5DUMP_BUFSIZE          (1024)
#endif

#define OPT(X,S)		((X) ? (X) : (S))
#define ALIGN(A,Z)		((((A) + (Z) - 1) / (Z)) * (Z))
#define START_OF_DATA		0x0001
#define END_OF_DATA		0x0002

/* Special strings embedded in the output */
#define OPTIONAL_LINE_BREAK     "\001"

/* Variable length string datatype */
#define STR_INIT_LEN            4096    /*initial length               */

/* module-scoped variables */
static int h5tools_init_g;      /* if h5tools lib has been initialized */

int   indent;
int   compound_data;
int   nCols = 80;
FILE *rawdatastream;	/* should initialize to stdout but gcc moans about it */

/* ``get_option'' variables */
int         opt_err = 1;    /*get_option prints errors if this is on */
int         opt_ind = 1;    /*token pointer                          */
const char *opt_arg;        /*flag argument (or value)               */

typedef struct h5dump_str_t {
    char	*s;		/*allocate string		*/
    size_t	len;		/*length of actual value	*/
    size_t	nalloc;		/*allocated size of string	*/
} h5dump_str_t;

/* Output variables */
typedef struct h5dump_context_t {
    size_t	cur_column;	/*current column for output	*/
    size_t	cur_elmt;	/*current element/output line	*/
    int		need_prefix;	/*is line prefix needed?	*/
    int		ndims;		/*dimensionality		*/
    hsize_t	p_min_idx[H5S_MAX_RANK]; /*min selected index	*/
    hsize_t	p_max_idx[H5S_MAX_RANK]; /*max selected index	*/
    int		prev_multiline;	/*was prev datum multiline?	*/
    size_t	prev_prefix_len;/*length of previous prefix	*/
    int		continuation;	/*continuation of previous data?*/
    int		size_last_dim;  /*the size of the last dimension,
                                 *needed so we can break after each
                                 *row */
    int		indent_level;   /*the number of times we need some
                                 *extra indentation */
    int		default_indent_level; /*this is used when the indent
                                       *level gets changed */
} h5dump_context_t;

typedef herr_t (*H5G_operator_t)(hid_t, const char*, void*);

extern void free_table(table_t **table);
extern void dump_table(char *name, table_t* table);
extern int get_table_idx(table_t *table, unsigned long *);
extern int get_tableflag(table_t*, int);
extern int set_tableflag(table_t*, int);
extern char *get_objectname(table_t*, int);


/*-------------------------------------------------------------------------
 * Function:	get_option
 *
 * Purpose:	Determine the command-line options a user specified. We can
 *		accept both short and long type command-lines.
 *
 * Return:	Success:	The short valued "name" of the command line
 * 				parameter or EOF if there are no more
 * 				parameters to process.
 *
 *		Failure:	A question mark.
 *
 * Programmer:	Bill Wendling
 *              Friday, 5. January 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
get_option(int argc, const char **argv, const char *opts, const struct long_options *l_opts)
{
    static int sp = 1;    /* character index in current token */
    int opt_opt='?';      /* option character passed back to user */

    if (sp == 1) {
        /* check for more flag-like tokens */
        if (opt_ind >= argc || argv[opt_ind][0] != '-' || argv[opt_ind][1] == '\0') {
            return EOF;
        } else if (strcmp(argv[opt_ind], "--") == 0) {
            opt_ind++;
            return EOF;
        }
    }

    if (sp == 1 && argv[opt_ind][0] == '-' && argv[opt_ind][1] == '-') {
        /* long command line option */
        const char *arg = &argv[opt_ind][2];
        register int i;

        for (i = 0; l_opts && l_opts[i].name; i++) {
            size_t len = strlen(l_opts[i].name);

            if (strncmp(arg, l_opts[i].name, len) == 0) {
                /* we've found a matching long command line flag */
                opt_opt = l_opts[i].shortval;

                if (l_opts[i].has_arg != no_arg) {
                    if (arg[len] == '=') {
                        opt_arg = &arg[len + 1];
                    } else if (opt_ind < (argc - 1) && argv[opt_ind + 1][0] != '-') {
                        opt_arg = argv[++opt_ind];
                    } else if (l_opts[i].has_arg == require_arg) {
                        if (opt_err)
                            fprintf(stderr,
                                    "%s: option required for \"--%s\" flag\n",
                                    argv[0], arg);

                        opt_opt = '?';
                    }
                } else {
                    if (arg[len] == '=') {
                        if (opt_err)
                            fprintf(stderr,
                                    "%s: no option required for \"%s\" flag\n",
                                    argv[0], arg);

                        opt_opt = '?';
                    }

                    opt_arg = NULL;
                }

                break;
            }
        }

        if (l_opts[i].name == NULL) {
            /* exhausted all of the l_opts we have and still didn't match */
            if (opt_err)
                fprintf(stderr, "%s: unknown option \"%s\"\n", argv[0], arg);

            opt_opt = '?';
        }

        opt_ind++;
        sp = 1;
    } else {
        register char *cp;    /* pointer into current token */

        /* short command line option */
        opt_opt = argv[opt_ind][sp];

        if (opt_opt == ':' || (cp = strchr(opts, opt_opt)) == 0) {
            if (opt_err)
                fprintf(stderr, "%s: unknown option \"%c\"\n",
                        argv[0], opt_opt);

            /* if no chars left in this token, move to next token */
            if (argv[opt_ind][++sp] == '\0') {
                opt_ind++;
                sp = 1;
            }

            return '?';
        }

        if (*++cp == ':') {
            /* if a value is expected, get it */
            if (argv[opt_ind][sp + 1] != '\0') {
                /* flag value is rest of current token */
                opt_arg = &argv[opt_ind++][sp + 1];
            } else if (++opt_ind >= argc) {
                if (opt_err)
                    fprintf(stderr,
                            "%s: value expected for option \"%c\"\n",
                            argv[0], opt_opt);

                opt_opt = '?';
            } else {
                /* flag value is next token */
                opt_arg = argv[opt_ind++];
            }

            sp = 1;
        } else {
            /* set up to look at next char in token, next time */
            if (argv[opt_ind][++sp] == '\0') {
                /* no more in current token, so setup next token */
                opt_ind++;
                sp = 1;
            }

            opt_arg = NULL;
        }
    }

    /* return the current flag character found */
    return opt_opt;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_init
 *
 * Purpose:	Initialize the H5 Tools library.
 *		This should be called before any other h5tools function is
 *		called.  Effect of any h5tools function called before this
 *		has been called is undetermined.
 *
 * Return:	None
 *
 * Programmer:	Albert Cheng, 2000-10-31
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
void
h5tools_init(void)
{
    if (!h5tools_init_g) {
	if (!rawdatastream)
	    rawdatastream = stdout;

	h5tools_init_g++;
    }
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_close
 *
 * Purpose:	Close the H5 Tools library by closing or releasing resources
 *		such as files opened by the library.
 *		This should be called after all other h5tools functions have
 *		been called.  Effect of any h5tools function called after this
 *		has been called is undetermined.
 *
 * Return:	None
 *
 * Programmer:	Albert Cheng, 2000-10-31
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
void
h5tools_close(void)
{
    if (h5tools_init_g) {
	if (rawdatastream && rawdatastream != stdout) {
	    if (fclose(rawdatastream))
		perror("closing rawdatastream");
	    else
		rawdatastream = NULL;
	}

	h5tools_init_g = 0;
    }
}

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
    if (!str->s || str->nalloc <= 0) {
	str->nalloc = STR_INIT_LEN;
	str->s = malloc(str->nalloc);
	assert(str->s);
	str->s[0] = '\0';
	str->len = 0;
    }

    while (1) {
	size_t avail = str->nalloc - str->len;
	size_t nchars = (size_t)HDvsnprintf(str->s+str->len, avail, fmt, ap);

	if (nchars < avail) {
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
    if (!str->s || str->nalloc <= 0) {
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
    if (size < str->len) {
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
    char _temp[1024], *temp = _temp;

    /* If the format string is simply "%s" then don't bother doing anything */
    if (!strcmp(fmt, "%s"))
        return str->s;

    /*
     * Save the input value if there is a `%' anywhere in FMT.  Otherwise
     * don't bother because we don't need a temporary copy.
     */
    if (strchr(fmt, '%')) {
        if (str->len - start + 1 > sizeof(_temp)) {
            temp = malloc(str->len-start + 1);
            assert(temp);
        }

        strcpy(temp, str->s + start);
    }

    /* Reset the output string and append a formatted version */
    h5dump_str_trunc(str, start);
    h5dump_str_append(str, fmt, temp);

    /* Free the temp buffer if we allocated one */
    if (temp != _temp)
        free(temp);

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
    hsize_t p_prod[H5S_MAX_RANK], p_idx[H5S_MAX_RANK];
    hsize_t n, i = 0;

    h5dump_str_reset(str);

    if (ndims > 0) {
        /*
         * Calculate the number of elements represented by a unit change in a
         * certain index position.
         */
        for (i = ndims - 1, p_prod[ndims - 1] = 1; i > 0; --i) {
            p_prod[i - 1] = (max_idx[i] - min_idx[i]) * p_prod[i];
        }

        /*
         * Calculate the index values from the element number.
         */
        for (i = 0, n = elmtno; i < (hsize_t)ndims; i++) {
            p_idx[i] = n / p_prod[i] + min_idx[i];
            n %= p_prod[i];
        }

        /*
         * Print the index values.
         */
        for (i = 0; i < (hsize_t)ndims; i++) {
            if (i)
                h5dump_str_append(str, "%s", OPT(info->idx_sep, ","));

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
    size_t n = strlen(s);
    size_t i;
    const char *escape;
    char octal[8];
    
    for (i = 0; i < n; i++) {
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
		sprintf(octal, "\\%03o", (unsigned char)s[i]);
		escape = octal;
	    } else {
		escape = NULL;
	    }

	    break;
	}

	if (escape) {
	    size_t esc_size = strlen(escape);

	    if (n + esc_size + 1 > size)
		/*would overflow*/
		return NULL;

	    memmove(s + i + esc_size, s + i, (n - i) + 1); /*make room*/
	    memcpy(s + i, escape, esc_size); /*insert*/
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
    const unsigned char *mem = (const unsigned char *)_mem;

    while (size-- > 0)
	if (mem[size])
	    return FALSE;

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
h5dump_region(hid_t region, h5dump_str_t *str/*in,out*/, const h5dump_t *info)
{
    hssize_t	nblocks, npoints;
    hsize_t alloc_size;
    hsize_t	*ptdata;
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
    if (nblocks > 0) {
        int i;

        alloc_size=nblocks * ndims * 2 * sizeof(ptdata[0]);
        assert(alloc_size==(hsize_t)((size_t)alloc_size)); /*check for overflow*/
        ptdata = malloc((size_t)alloc_size);
        H5_CHECK_OVERFLOW(nblocks,hssize_t,hsize_t);
        H5Sget_select_hyper_blocklist(region, (hsize_t)0, (hsize_t)nblocks, ptdata);

        for (i = 0; i < nblocks; i++) {
            int j;

            h5dump_str_append(str, info->dset_blockformat_pre,
                      i ? "," OPTIONAL_LINE_BREAK " " : "",
                      (unsigned long)i);
            
            /* Start coordinates and opposite corner */
            for (j = 0; j < ndims; j++)
                h5dump_str_append(str, "%s%lu", j ? "," : "(",
                      (unsigned long)ptdata[i * 2 * ndims + j]);

            for (j = 0; j < ndims; j++)
                h5dump_str_append(str, "%s%lu", j ? "," : ")-(",
                      (unsigned long)ptdata[i * 2 * ndims + j + ndims]);

            h5dump_str_append(str, ")");
        }
        free(ptdata);
    }

    /* Print point information */
    if (npoints > 0) {
        int i;

        alloc_size=npoints * ndims * sizeof(ptdata[0]);
        assert(alloc_size==(hsize_t)((size_t)alloc_size)); /*check for overflow*/
        ptdata = malloc((size_t)alloc_size);
        H5_CHECK_OVERFLOW(npoints,hssize_t,hsize_t);
        H5Sget_select_elem_pointlist(region, (hsize_t)0, (hsize_t)npoints, ptdata);

        for (i = 0; i < npoints; i++) {
            int j;

            h5dump_str_append(str, info->dset_ptformat_pre ,
                      i ? "," OPTIONAL_LINE_BREAK " " : "",
                      (unsigned long)i);
            
            for (j = 0; j < ndims; j++)
                h5dump_str_append(str, "%s%lu", j ? "," : "(",
                      (unsigned long)(ptdata[i * ndims + j]));

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
	      hid_t container, hid_t type, void *vp, h5dump_context_t *ctx)
{
    size_t	n, offset, size, nelmts, start;
    char	*name, quote='\0';
    unsigned char *ucp_vp = (unsigned char *)vp;
    char 	*cp_vp = (char *)vp;
    hid_t	memb, obj, region;
    int		nmembs, otype;
    static char	fmt_llong[8], fmt_ullong[8];
    H5T_str_t 	pad;
    H5G_stat_t	sb;

    /*some tempvars to store the value before we append it to the string
      to get rid of the memory alignment problem*/
    float tempfloat;
    double tempdouble;
    int tempint;
    unsigned int tempuint;
    short tempshort;
    unsigned short tempushort;
    long templong;
    unsigned long tempulong;
    unsigned long_long tempullong;
    long_long templlong;
 
    /* Build default formats for long long types */
    if (!fmt_llong[0]) {
        sprintf(fmt_llong, "%%%sd", PRINTF_LL_WIDTH);
        sprintf(fmt_ullong, "%%%su", PRINTF_LL_WIDTH);
    }

    /* Append value depending on data type */
    start = h5dump_str_len(str);

    if (info->raw) {
        unsigned int i;

        h5dump_str_append(str, "0x");
        n = H5Tget_size(type);

        for (i = 0; i < n; i++)
            h5dump_str_append(str, OPT(info->fmt_raw, "%02x"), ucp_vp[i]);
    } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
        memcpy(&tempfloat, vp, sizeof(float));	
        h5dump_str_append(str, OPT(info->fmt_float, "%g"), tempfloat);
    } else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
        memcpy(&tempdouble, vp, sizeof(double)); 
        h5dump_str_append(str, OPT(info->fmt_double, "%g"), tempdouble);
    } else if (info->ascii && (H5Tequal(type, H5T_NATIVE_SCHAR) ||
            H5Tequal(type, H5T_NATIVE_UCHAR))) {
        if (ESCAPE_HTML == info->str_locale) {
            if (*cp_vp <= ' ' || *cp_vp > '~')
                h5dump_str_append(str, "%%%02X", *ucp_vp);
            else
                h5dump_str_append(str, "%c", *cp_vp);
        } else {
            switch (*cp_vp) {
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
                    if (isprint(*cp_vp))
                        h5dump_str_append(str, "%c", *cp_vp);
                    else 
                        h5dump_str_append(str, "\\%03o", *ucp_vp);
                    break;
            }
        }
    } else if (H5T_STRING == H5Tget_class(type)) {
        unsigned int i;

        size = H5Tget_size(type);
        quote = '\0';
        pad = H5Tget_strpad(type);

        for (i=0;
                 i < size && (cp_vp[i] != '\0' || pad != H5T_STR_NULLTERM);
                 i++) {
            int j = 1;

            /*
             * Count how many times the next character repeats. If the
             * threshold is zero then that means it can repeat any number
             * of times.
             */
            if (info->str_repeat > 0)
                while (i + j < size && cp_vp[i] == cp_vp[i + j])
                    j++;
            
            /*
             * Print the opening quote.  If the repeat count is high enough to
             * warrant printing the number of repeats instead of enumerating
             * the characters, then make sure the character to be repeated is
             * in it's own quote.
             */
            if (info->str_repeat > 0 && j > info->str_repeat) {
                if (quote)
                    h5dump_str_append(str, "%c", quote);

                quote = '\'';
                h5dump_str_append(str, "%s%c", i ? " " : "", quote);
            } else if (!quote) {
                quote = '"';
                h5dump_str_append(str, "%s%c", i ? " " : "", quote);
            }
                
            /* Print the character */
            if (ESCAPE_HTML == info->str_locale) {
                if (cp_vp[i] <= ' ' || cp_vp[i] > '~') {
                    h5dump_str_append(str, "%%%02X", ucp_vp[i]);
                } else {
                    h5dump_str_append(str, "%c", cp_vp[i]);
                }
            } else {
                switch (cp_vp[i]) {
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
                        if (isprint(cp_vp[i]))
                            h5dump_str_append(str, "%c", cp_vp[i]);
                        else
                            h5dump_str_append(str, "\\%03o", ucp_vp[i]);
                        break;
                }
            }
            
            /* Print the repeat count */
            if (info->str_repeat && j > info->str_repeat) {
#ifdef REPEAT_VERBOSE
                h5dump_str_append(str, "%c repeats %d times", quote, j - 1);
#else
                h5dump_str_append(str, "%c*%d", quote, j - 1);
#endif
                quote = '\0';
                i += j - 1;
            }

        }

        if (quote)
            h5dump_str_append(str, "%c", quote);

        if (0 == i)
            h5dump_str_append(str, "\"\"");	/*empty string*/
    } else if (H5Tequal(type, H5T_NATIVE_INT)) {
        memcpy(&tempint, vp, sizeof(int));
        h5dump_str_append(str, OPT(info->fmt_int, "%d"), tempint);
    } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
        memcpy(&tempuint, vp, sizeof(unsigned int));
        h5dump_str_append(str, OPT(info->fmt_uint, "%u"), tempuint);
    } else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
        h5dump_str_append(str, OPT(info->fmt_schar, "%d"), *((signed char*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
        h5dump_str_append(str, OPT(info->fmt_uchar, "%u"), *((unsigned char*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
        memcpy(&tempshort, vp, sizeof(short));
        h5dump_str_append(str, OPT(info->fmt_short, "%d"), tempshort);
    } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
        memcpy(&tempushort, vp, sizeof(unsigned short));
        h5dump_str_append(str, OPT(info->fmt_ushort, "%u"), tempushort);
    } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
        memcpy(&templong, vp, sizeof(long));
        h5dump_str_append(str, OPT(info->fmt_long, "%ld"), templong);
    } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
        memcpy(&tempulong, vp, sizeof(unsigned long));
        h5dump_str_append(str, OPT(info->fmt_ulong, "%lu"), tempulong);
    } else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
        memcpy(&templlong, vp, sizeof(long_long));
        h5dump_str_append(str, OPT(info->fmt_llong, fmt_llong), templlong);
    } else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
        memcpy(&tempullong, vp, sizeof(unsigned long_long));
        h5dump_str_append(str, OPT(info->fmt_ullong, fmt_ullong), tempullong);
    } else if (H5Tequal(type, H5T_NATIVE_HSSIZE)) {
        if (sizeof(hssize_t) == sizeof(int)) {
            memcpy(&tempint, vp, sizeof(int));	  
            h5dump_str_append(str, OPT(info->fmt_int, "%d"), tempint);
        } else if (sizeof(hssize_t) == sizeof(long)) {
            memcpy(&templong, vp, sizeof(long));
            h5dump_str_append(str, OPT(info->fmt_long, "%ld"), templong);
        } else {
            memcpy(&templlong, vp, sizeof(long_long));
            h5dump_str_append(str, OPT(info->fmt_llong, fmt_llong), templlong);
        }
    } else if (H5Tequal(type, H5T_NATIVE_HSIZE)) {
        if (sizeof(hsize_t) == sizeof(int)) {
            memcpy(&tempuint, vp, sizeof(unsigned int));
            h5dump_str_append(str, OPT(info->fmt_uint, "%u"), tempuint);
        } else if (sizeof(hsize_t) == sizeof(long)) {
            memcpy(&tempulong, vp, sizeof(long));
            h5dump_str_append(str, OPT(info->fmt_ulong, "%lu"), tempulong);
        } else {
            memcpy(&tempullong, vp, sizeof(unsigned long_long));
            h5dump_str_append(str, OPT(info->fmt_ullong, fmt_ullong), tempullong);
        }
    } else if (H5T_COMPOUND==H5Tget_class(type)) {
        int j, x;

        nmembs = H5Tget_nmembers(type);
        h5dump_str_append(str, "%s", OPT(info->cmpd_pre, "{"));

        for (j = 0; j < nmembs; j++) {
            if (j)
                h5dump_str_append(str, "%s", OPT(info->cmpd_sep, ", " OPTIONAL_LINE_BREAK));

            /* RPM 2000-10-31
             * If the previous character is a line-feed (which is true when
             * h5dump is running) then insert some white space for
             * indentation. Be warned that column number calculations will be
             * incorrect and that object indices at the beginning of the line
             * will be missing (h5dump doesn't display them anyway).  */
            if (ctx->indent_level >= 0 && str->len &&
                    str->s[str->len - 1] == '\n') {
                h5dump_str_append(str, OPT(info->line_pre, ""), "");

                for (x = 0; x < ctx->indent_level + 1; x++)
                    h5dump_str_append(str, "%s", OPT(info->line_indent, ""));
            }

            /* The name */
            name = H5Tget_member_name(type, j);
            h5dump_str_append(str, OPT(info->cmpd_name, ""), name);
            free(name);

            /* The value */
            offset = H5Tget_member_offset(type, j);
            memb = H5Tget_member_type(type, j);

            ctx->indent_level++;
            h5dump_sprint(str, info, container, memb, cp_vp + offset , ctx);
            ctx->indent_level--;

            H5Tclose(memb);
        }

        /* RPM 2000-10-31
         * If the previous character is a line feed (which is true when
         * h5dump is running) then insert some white space for indentation.
         * Be warned that column number calculations will be incorrect and
         * that object indices at the beginning of the line will be missing
         * (h5dump doesn't display them anyway). */
        h5dump_str_append(str, "%s", OPT(info->cmpd_end, ""));

        if (ctx->indent_level >= 0 && str->len &&
            str->s[str->len - 1] == '\n') {
            h5dump_str_append(str, OPT(info->line_pre, ""), "");

            for (x = 0; x < ctx->indent_level; x++)
                h5dump_str_append(str, "%s", OPT(info->line_indent, ""));
        }

        h5dump_str_append(str, "%s", OPT(info->cmpd_suf, "}"));
    } else if (H5T_ENUM==H5Tget_class(type)) {
        char enum_name[1024];

        if (H5Tenum_nameof(type, vp, enum_name, sizeof enum_name) >= 0) {
            h5dump_str_append(str, h5dump_escape(enum_name, sizeof(enum_name), TRUE));
        } else {
            unsigned int i;

            h5dump_str_append(str, "0x");
            n = H5Tget_size(type);

            for (i = 0; i < n; i++)
                h5dump_str_append(str, "%02x", ucp_vp[i]);
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

            if (info->dset_hidefileno)
                h5dump_str_append(str, info->dset_format, sb.objno[1], sb.objno[0]);
            else
                h5dump_str_append(str, info->dset_format,
                      sb.fileno[1], sb.fileno[0], sb.objno[1], sb.objno[0]);

            h5dump_region(region, str, info);
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
                    h5dump_str_append(str, "%u-", otype);
                    break;
            }

            /* Print OID */
            if (info->obj_hidefileno) {
                h5dump_str_append(str, info->obj_format, sb.objno[1], sb.objno[0]);
            } else {
                h5dump_str_append(str, info->obj_format,
                          sb.fileno[1], sb.fileno[0], sb.objno[1], sb.objno[0]);
            }
        }
    } else if (H5T_ARRAY==H5Tget_class(type)) {
        int x;
	    unsigned int i;
	    int k;
        hsize_t	dims[H5S_MAX_RANK];
        int		ndims;

        /* Get the array's base datatype for each element */
        memb=H5Tget_super(type);

	    size = H5Tget_size(memb);
        ndims = H5Tget_array_ndims(type);
        H5Tget_array_dims(type, dims, NULL);
	    assert(ndims>=1 && ndims<=H5S_MAX_RANK);

        /* Calculate the number of array elements */
	    for (k = 0, nelmts = 1; k < ndims; k++)
            nelmts *= dims[k];
			
        /* Print the opening bracket */
        h5dump_str_append(str, "%s", OPT(info->arr_pre, "["));

	    for (i = 0; i < nelmts; i++) {
            if (i)
                h5dump_str_append(str, "%s",
                          OPT(info->arr_sep, "," OPTIONAL_LINE_BREAK));

            if (info->arr_linebreak && i && 0 == i % dims[ndims - 1]) {
                h5dump_str_append(str, "%s", "\n");

                /*need to indent some more here*/
                if (ctx->indent_level >= 0)
                    h5dump_str_append(str, "%s", OPT(info->line_pre, ""));

                for (x = 0; x < ctx->indent_level + 1; x++)
                    h5dump_str_append(str,"%s",OPT(info->line_indent,""));
            } /* end if */

            ctx->indent_level++;

            /* Dump the array element */
            h5dump_sprint(str, info, container, memb, cp_vp + i * size, ctx);

            ctx->indent_level--;
	    } /* end for */

        /* Print the closing bracket */
 		h5dump_str_append(str, "%s", OPT(info->arr_suf, "]"));

	    H5Tclose(memb);
    } else if (H5T_VLEN==H5Tget_class(type)) {
	    unsigned int i;

        /* Get the VL sequences's base datatype for each element */
        memb=H5Tget_super(type);
	    size = H5Tget_size(memb);

        /* Print the opening bracket */
        h5dump_str_append(str, "%s", OPT(info->vlen_pre, "("));

        /* Get the number of sequence elements */
        nelmts = ((hvl_t *)cp_vp)->len;

	    for (i = 0; i < nelmts; i++) {
            if (i)
                h5dump_str_append(str, "%s",
                          OPT(info->arr_sep, "," OPTIONAL_LINE_BREAK));

#ifdef LATER
/* Need to fix so VL data breaks at correct location on end of line -QAK */
            if (info->arr_linebreak && h5dump_str_len(str)>=info->line_ncols) {
                int x;

                h5dump_str_append(str, "%s", "\n");

                /* need to indent some more here */
                if (ctx->indent_level >= 0)
                    h5dump_str_append(str, "%s", OPT(info->line_pre, ""));

                for (x = 0; x < ctx->indent_level + 1; x++)
                    h5dump_str_append(str,"%s",OPT(info->line_indent,""));
            } /* end if */
#endif /* LATER */

            ctx->indent_level++;

            /* Dump the array element */
            h5dump_sprint(str, info, container, memb, ((char *)(((hvl_t *)cp_vp)->p)) + i * size, ctx);

            ctx->indent_level--;
	    } /* end for */

 		h5dump_str_append(str, "%s", OPT(info->vlen_suf, ")"));

	    H5Tclose(memb);

    } else {
        /* All other types get printed as hexadecimal */
        unsigned int i;

        h5dump_str_append(str, "0x");
        n = H5Tget_size(type);

        for (i = 0; i < n; i++)
            h5dump_str_append(str, "%02x", ((unsigned char *)vp)[i]);
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
    size_t i;
    
    for (i = 0; *s; s++)
	if (*s >= ' ')
	    i++;

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
    h5dump_str_t prefix;
    size_t templength = 0;
    int i, indentlevel = 0;
	
    memset(&prefix, 0, sizeof(h5dump_str_t));

    if (!ctx->need_prefix)
	return;
    
    /* Terminate previous line, if any */
    if (ctx->cur_column) {
	fputs(OPT(info->line_suf, ""), stream);
#if 0 /*why?*/
        if (info->line_ncols != ctx->cur_column) {
            putc('\n', stream);
        }
#endif
        putc('\n',stream);
	fputs(OPT(info->line_sep, ""), stream);
    }

    /* Calculate new prefix */
    h5dump_prefix(&prefix, info, elmtno, ctx->ndims,
		  ctx->p_min_idx, ctx->p_max_idx);

    /* Write new prefix to output */
    if (ctx->indent_level >= 0) {
        indentlevel = ctx->indent_level;
    } else {
        /* this is because sometimes we dont print out all the header
         * info for the data(like the tattr-2.ddl example. if that happens
         * the ctx->indent_level a negative so we need to skip the above
         * and just print out the default indent levels. */      
	indentlevel = ctx->default_indent_level;
    }

    if (elmtno == 0 && secnum == 0 && info->line_1st)
        fputs(h5dump_str_fmt(&prefix, 0, info->line_1st), stream);
    else if (secnum && info->line_cont)
        fputs(h5dump_str_fmt(&prefix, 0, info->line_cont), stream);
    else
        fputs(h5dump_str_fmt(&prefix, 0, info->line_pre), stream);

    templength = h5dump_str_len(&prefix);      

    for (i = 0; i < indentlevel; i++){
        fputs(h5dump_str_fmt(&prefix, 0, info->line_indent), stream);
        templength += h5dump_str_len(&prefix);
    }	

    ctx->cur_column = ctx->prev_prefix_len = templength;
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
    int                 elmt_counter=0; /*counts the # elements printed.
                                         * I (ptl?) needed something that
                                         * isnt going to get reset when a new
                                         * line is formed. I'm going to use
                                         * this var to count elements and
                                         * break after we see a number equal
                                         * to the ctx->size_last_dim. */
    
    /* Setup */
    memset(&buffer, 0, sizeof(h5dump_str_t));
    size = H5Tget_size(type);

    if (info->line_ncols > 0)
	ncols = info->line_ncols;

    h5dump_simple_prefix(stream, info, ctx, (hsize_t)0, 0);
    
    for (i = 0; i < nelmts; i++, ctx->cur_elmt++, elmt_counter++) {
        /* Render the element */
        h5dump_str_reset(&buffer);
        h5dump_sprint(&buffer, info, container, type, mem + i * size, ctx);

        if (i + 1 < nelmts || 0 == (flags & END_OF_DATA))
            h5dump_str_append(&buffer, "%s", OPT(info->elmt_suf1, ","));

        s = h5dump_str_fmt(&buffer, 0, "%s");

        /*
         * If the element would split on multiple lines if printed at our
         * current location...
         */
        if (info->line_multi_new == 1 &&
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
         * We need to break after each row of a dimension---> we should
         * break at the end of the each last dimension well that is the
         * way the dumper did it before
         */
        if (info->arr_linebreak && ctx->cur_elmt) {
            if (ctx->size_last_dim && (ctx->cur_elmt % ctx->size_last_dim) == 0)
                ctx->need_prefix = TRUE;

            if (elmt_counter == ctx->size_last_dim) {
                ctx->need_prefix = TRUE;
                elmt_counter = 0;
            }
        }

        /*
         * If the previous element occupied multiple lines and this element
         * is too long to fit on a line then start this element at the
         * beginning of the line.
         */
        if (info->line_multi_new == 1 && ctx->prev_multiline &&
                (ctx->cur_column + h5dump_ncols(s) +
                 strlen(OPT(info->elmt_suf2, " ")) +
                 strlen(OPT(info->line_suf, ""))) > ncols)
            ctx->need_prefix = TRUE;

        /*
         * If too many elements have already been printed then we need to
         * start a new line.
         */
        if (info->line_per_line > 0 && ctx->cur_elmt >= info->line_per_line)
            ctx->need_prefix = TRUE;
        
        /*
         * Each OPTIONAL_LINE_BREAK embedded in the rendered string can cause
         * the data to split across multiple lines.  We display the sections
         * one-at a time.
         */
        for (secnum = 0, multiline = 0;
                 (section = strtok(secnum ? NULL : s, OPTIONAL_LINE_BREAK));
                 secnum++) {
            /*
             * If the current section plus possible suffix and end-of-line
             * information would cause the output to wrap then we need to
             * start a new line.
             */

            /*
             * Added the info->skip_first because the dumper does not want
             * this check to happen for the first line
             */
            if ((!info->skip_first || i) &&
                    (ctx->cur_column + strlen(section) +
                     strlen(OPT(info->elmt_suf2, " ")) +
                     strlen(OPT(info->line_suf, ""))) > ncols)
                ctx->need_prefix = 1;

            /*
             * Print the prefix or separate the beginning of this element
             * from the previous element.
             */
            if (ctx->need_prefix) {
                if (secnum)
                    multiline++;

                h5dump_simple_prefix(stream, info, ctx, i, secnum);
            } else if ((i || ctx->continuation) && secnum == 0) {
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
		   hid_t p_type, int indentlevel)
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

    /* VL data special information */
    unsigned vl_data=0;     /* Whether the dataset contains VL datatypes */

#if 0
    hsize_t		dim_n_size;
#endif

    /*
     * Check that everything looks okay. The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof ctx);
    ctx.indent_level = indentlevel;
    ctx.need_prefix = 1;
    f_space = H5Dget_space(dset);
    ctx.ndims = H5Sget_simple_extent_ndims(f_space);

    if ((size_t)ctx.ndims > NELMTS(sm_size))
        return -1;

    /* Assume entire data space to be printed */
    if(ctx.ndims>0)
        for (i = 0; i < (hsize_t)ctx.ndims; i++)
            ctx.p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(f_space, total_size, NULL);

    if(ctx.ndims>0)
        for (i = 0, p_nelmts = 1; i < (hsize_t)ctx.ndims; i++)
            p_nelmts *= total_size[i];
    else
        p_nelmts = 1;
 
    if (p_nelmts == 0) {
        H5Sclose(f_space);
        return 0; /*nothing to print*/
    }

    ctx.size_last_dim = total_size[ctx.ndims - 1];

    /* Check if we have VL data in the dataset's datatype */
    if(H5Tdetect_class(p_type,H5T_VLEN)==TRUE)
        vl_data=TRUE;

    /*
     * Determine the strip mine size and allocate a buffer. The strip mine is
     * a hyperslab whose size is manageable.
     */
    p_type_nbytes = H5Tget_size(p_type);

    if(ctx.ndims>0)
        for (i = ctx.ndims, sm_nbytes = p_type_nbytes; i > 0; --i) {
            sm_size[i - 1] = MIN(total_size[i - 1], H5DUMP_BUFSIZE / sm_nbytes);
            sm_nbytes *= sm_size[i - 1];
            assert(sm_nbytes > 0);
        }
    else
        sm_nbytes = p_type_nbytes;

    assert(sm_nbytes==(hsize_t)((size_t)sm_nbytes)); /*check for overflow*/
    sm_buf = malloc((size_t)sm_nbytes);
    sm_nelmts = sm_nbytes / p_type_nbytes;
    sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

    /* The stripmine loop */
    memset(hs_offset, 0, sizeof hs_offset);
    memset(zero, 0, sizeof zero);

    for (elmtno = 0; elmtno < p_nelmts; elmtno += hs_nelmts) {
        /* Calculate the hyperslab size */
        if (ctx.ndims > 0) {
            for (i = 0, hs_nelmts = 1; i < (hsize_t)ctx.ndims; i++) {
                hs_size[i] = MIN(total_size[i] - hs_offset[i], sm_size[i]);
                ctx.p_max_idx[i] = ctx.p_min_idx[i] + hs_size[i];
                hs_nelmts *= hs_size[i];
            }

            H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL,
                    hs_size, NULL);
            H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL,
                    &hs_nelmts, NULL);
        } else {
            H5Sselect_all(f_space);
            H5Sselect_all(sm_space);
            hs_nelmts = 1;
        }

        /* Read the data */
        if (H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) < 0) {
            H5Sclose(f_space);
            H5Sclose(sm_space);
            free(sm_buf);
            return -1;
        }

        /* Print the data */
        flags = (elmtno == 0) ? START_OF_DATA : 0;
        flags |= ((elmtno + hs_nelmts) >= p_nelmts) ? END_OF_DATA : 0;
        h5dump_simple_data(stream, info, dset, &ctx, flags, hs_nelmts,
                               p_type, sm_buf);

        /* Reclaim any VL memory, if necessary */
        if(vl_data)
            H5Dvlen_reclaim(p_type,sm_space,H5P_DEFAULT,sm_buf);

        /* Calculate the next hyperslab offset */
        for (i = ctx.ndims, carry = 1; i > 0 && carry; --i) {
            ctx.p_min_idx[i - 1] = ctx.p_max_idx[i - 1];
            hs_offset[i - 1] += hs_size[i - 1];

            if (hs_offset[i - 1] == (hssize_t)total_size[i - 1])
                hs_offset[i-1] = 0;
            else
                carry = 0;
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
    free(sm_buf);
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
h5dump_simple_mem(FILE *stream, const h5dump_t *info, hid_t obj_id, hid_t type,
		  hid_t space, void *mem, int indentlevel)
{
    hsize_t		i;			/*counters		*/
    hsize_t		nelmts;			/*total selected elmts	*/
    h5dump_context_t	ctx;			/*printing context	*/

    /*
     * Check that everything looks okay.  The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof(ctx));
    ctx.ndims = H5Sget_simple_extent_ndims(space);

    if ((size_t)ctx.ndims > NELMTS(ctx.p_min_idx))
        return -1;

    ctx.indent_level = indentlevel;
    ctx.need_prefix = 1;

    /* Assume entire data space to be printed */
    for (i = 0; i < (hsize_t)ctx.ndims; i++)
        ctx.p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(space, ctx.p_max_idx, NULL);

    for (i = 0, nelmts = 1; ctx.ndims != 0 && i < (hsize_t)ctx.ndims; i++)
        nelmts *= ctx.p_max_idx[i] - ctx.p_min_idx[i];
 
    if (nelmts == 0)
        return 0; /*nothing to print*/

    ctx.size_last_dim = ctx.p_max_idx[ctx.ndims - 1];

    /* Print it */
    h5dump_simple_data(stream, info, obj_id, &ctx,
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
    int		nmembs = 0, i;
    int     ndims;
    hsize_t  dim[H5S_MAX_RANK];
    size_t	size, offset;
    hid_t   array_base;
    /* H5T_str_t strpad; */

    size = H5Tget_size(f_type);

    switch (H5Tget_class(f_type)) {
    case H5T_INTEGER:
	/*
	 * Use the smallest native integer type of the same sign as the file
	 * such that the memory type is at least as large as the file type.
	 * If there is no memory type large enough then use the largest
	 * memory type available.
	 */
	if (size <= sizeof(char)) {
	    m_type = H5Tcopy(H5T_NATIVE_SCHAR);
	} else if (size <= sizeof(short)) {
	    m_type = H5Tcopy(H5T_NATIVE_SHORT);
	} else if (size <= sizeof(int)) {
	    m_type = H5Tcopy(H5T_NATIVE_INT);
	} else if (size <= sizeof(long)) {
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
	if (size <= sizeof(float)) {
	    m_type = H5Tcopy(H5T_NATIVE_FLOAT);
	} else if (size <= sizeof(double)) {
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
	m_type = H5Tcopy(f_type);
	H5Tset_cset(m_type, H5T_CSET_ASCII);
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
    assert(nmembs>0);
	memb = calloc((size_t)nmembs, sizeof(hid_t));
	name = calloc((size_t)nmembs, sizeof(char *));
	
	for (i = 0, size = 0; i < nmembs; i++) {

	    /* Get the member type and fix it */
	    f_memb = H5Tget_member_type(f_type, i);
	    memb[i] = h5dump_fixtype(f_memb);
	    H5Tclose(f_memb);

	    if (memb[i] < 0)
            goto done;

	    /* Get the member name */
	    name[i] = H5Tget_member_name(f_type, i);

	    if (name[i] == NULL)
            goto done;

	    /*
	     * Compute the new offset so each member is aligned on a byte
	     * boundary which is the same as the member size.
	     */
	    size = ALIGN(size, H5Tget_size(memb[i])) + H5Tget_size(memb[i]);
	}

	m_type = H5Tcreate(H5T_COMPOUND, size);

	for (i = 0, offset = 0; i < nmembs; i++) {
            if (offset)
                offset = ALIGN(offset, H5Tget_size(memb[i]));

	    H5Tinsert(m_type, name[i], offset, memb[i]);
	    offset += H5Tget_size(memb[i]);
	}

	break;

    case H5T_ARRAY:
        /* Get the array information */
        ndims = H5Tget_array_ndims(f_type);
        H5Tget_array_dims(f_type, dim, NULL);

        /* Get the array's base type and convert it to the printable version */
        f_memb = H5Tget_super(f_type);
        array_base = h5dump_fixtype(f_memb);

        /* Copy the array */
        m_type = H5Tarray_create(array_base, ndims, dim, NULL);

        /* Close the temporary datatypes */
        H5Tclose(array_base);
        H5Tclose(f_memb);
        break;

    case H5T_VLEN:
        /* Get the VL sequence's base type and convert it to the printable version */
        f_memb = H5Tget_super(f_type);
        array_base = h5dump_fixtype(f_memb);

        /* Copy the VL type */
        m_type = H5Tvlen_create(array_base);

        /* Close the temporary datatypes */
        H5Tclose(array_base);
        H5Tclose(f_memb);
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
    if (memb && name) {
        int j;

        for (j = 0; j < nmembs; j++) {
            if (memb[j] >= 0)
                H5Tclose(memb[j]);

            if (name[j])
                free(name[j]);
        }

        free(memb);
        free(name);
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
h5dump_dset(FILE *stream, const h5dump_t *info, hid_t dset, hid_t _p_type,
            int indentlevel)
{
    hid_t	f_space;
    hid_t	p_type = _p_type;
    hid_t	f_type;
    int		status = -1;
    h5dump_t	info_dflt;

    /* Use default values */
    if (!stream)
        stream = stdout;

    if (!info) {
        memset(&info_dflt, 0, sizeof info_dflt);
        info = &info_dflt;
    }

    if (p_type < 0) {
        f_type = H5Dget_type(dset);

        if (info->raw)
            p_type = H5Tcopy(f_type);
        else
            p_type = h5dump_fixtype(f_type);

        H5Tclose(f_type);

        if (p_type < 0)
            goto done;
    }

    /* Check the data space */
    f_space = H5Dget_space(dset);

    /* Print the data */
    if (H5Sis_simple(f_space) > 0) {
        status = h5dump_simple_dset(rawdatastream, info, dset, p_type,
                            indentlevel);
    }

    /* Close the dataspace */
    H5Sclose(f_space);

done:
    if (p_type != _p_type)
        H5Tclose(p_type);

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
h5dump_mem(FILE *stream, const h5dump_t *info, hid_t obj_id, hid_t type, hid_t space,
	   void *mem,int indentlevel)
{
    h5dump_t	info_dflt;
    
    /* Use default values */
    if (!stream)
	stream = stdout;

    if (!info) {
	memset(&info_dflt, 0, sizeof info_dflt);
	info = &info_dflt;
    }

    /* Check the data space */
    if (H5Sis_simple(space) <= 0)
	return -1;

    return h5dump_simple_mem(stream, info, obj_id, type, space, mem, indentlevel);
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
void indentation(int x)
{
    if (x < nCols) {
        while (x-- > 0)
	    printf(" ");
    } else {
        printf("The indentation exceeds the number of cols. Exiting....\n");
        exit(1);
    }
}

/*-------------------------------------------------------------------------
 * Function:    print_version
 *
 * Purpose:     Print the program name and the version information which is
 *		defined the same as the HDF5 library version.
 *
 * Return:      void
 *
 * Programmer:  unknown
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
void
print_version(const char *program_name)
{
    printf("%s: Version %u.%u.%u%s%s\n",
           program_name, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE,
           H5_VERS_SUBRELEASE[0] ? "-" : "", H5_VERS_SUBRELEASE);
}

/*
 *
 * THE FUNCTIONS BELOW ARE FROM THE H5FINSHD.C FILE
 *
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
void
init_table(table_t** temp)
{
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
void
init_prefix(char **prefix, int prefix_len)
{
    assert(prefix_len > 0);
    *prefix = calloc((size_t)prefix_len, 1);
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
free_table(table_t **table)
{
    HDfree((*table)->objs);
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
search_obj(table_t *table, unsigned long *objno)
{
    int i;

    for (i = 0; i < table->nobjs; i++)
        if (table->objs[i].objno[0] == *objno && table->objs[i].objno[1] == *(objno + 1))
	    return i;
  
    return -1;
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
static void
add_obj (table_t *table, unsigned long *objno, char *objname)
{
    int i;

    if (table->nobjs == table->size) {
        table->size *= 2;
        table->objs = realloc(table->objs, table->size*sizeof(obj_t));

        for (i = table->nobjs; i < table->size; i++) {
            table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
            table->objs[i].displayed = 0;
            table->objs[i].recorded = 0;
            table->objs[i].objflag = 0;
        }
    }

    i = table->nobjs++;
    table->objs[i].objno[0] = *objno;
    table->objs[i].objno[1] = *(objno + 1);
    strcpy(table->objs[i].objname, objname);
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
herr_t
find_objs(hid_t group, const char *name, void *op_data)
{
    hid_t obj, type;
    H5G_stat_t statbuf;
    char *tmp;
    int i;
    find_objs_t *info = (find_objs_t*)op_data;

    if (info->threshold > 1)
        /*will get an infinite loop if greater than 1*/
        return FAIL;

    H5Gget_objinfo(group, name, TRUE, &statbuf);

    tmp = malloc(strlen(info->prefix) + strlen(name) + 2);
    strcpy(tmp, info->prefix); 

    switch (statbuf.type) {
    case H5G_GROUP:
        if ((obj = H5Gopen(group, name)) >= 0) {
            if (info->prefix_len < (int)(strlen(info->prefix) + strlen(name) + 2)) {
                info->prefix_len *= 2;
                info->prefix = realloc(info->prefix,
                                       info->prefix_len * sizeof(char));
            }

            strcat(strcat(info->prefix,"/"), name);

            if (statbuf.nlink > info->threshold) {
                if (search_obj(info->group_table,  statbuf.objno) < 0) {
                    add_obj(info->group_table, statbuf.objno, info->prefix); 
                    H5Giterate(obj, ".", NULL, find_objs, (void *)info);
                }
            } else {
                H5Giterate (obj, ".", NULL, find_objs, (void *)info);
	    }

            strcpy(info->prefix, tmp);
            H5Gclose (obj);
        } else {
            info->status = 1;
	}

        break;

    case H5G_DATASET:
        strcat(tmp,"/");
        strcat(tmp,name); /* absolute name of the data set */

        if (statbuf.nlink > info->threshold  &&
			search_obj(info->dset_table, statbuf.objno) < 0)
            add_obj(info->dset_table, statbuf.objno, tmp);

        if ((obj = H5Dopen (group, name)) >= 0) {              
            type = H5Dget_type(obj);

            if (H5Tcommitted(type) > 0) {
                H5Gget_objinfo(type, ".", TRUE, &statbuf);

                if (search_obj (info->type_table, statbuf.objno) < 0) {
                    add_obj(info->type_table, statbuf.objno, tmp);
                    info->type_table->objs[info->type_table->nobjs - 1].objflag = 0;
                }
            }

            H5Tclose(type);
            H5Dclose (obj);
        } else {
            info->status = 1;
	}
            
        break;

    case H5G_TYPE:
        strcat(tmp,"/");
        strcat(tmp,name); /* absolute name of the type */
        i = search_obj(info->type_table, statbuf.objno);

        if (i < 0) {
            add_obj(info->type_table, statbuf.objno, tmp) ;

            /* named data type */
            info->type_table->objs[info->type_table->nobjs-1].recorded = 1;

            /* named data type */
            info->type_table->objs[info->type_table->nobjs-1].objflag = 1;
        } else {
            strcpy (info->type_table->objs[i].objname, tmp);
            info->type_table->objs[i].recorded = 1; 

            /* named data type */  
            info->type_table->objs[info->type_table->nobjs-1].objflag = 1;
        }

        break;

    default:
        break;
    }

    free(tmp);
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
dump_table(char* tablename, table_t *table)
{
    int i;

    printf("%s: # of entries = %d\n", tablename,table->nobjs);

    for ( i = 0; i < table->nobjs; i++)
        printf("%lu %lu %s %d\n", table->objs[i].objno[0],
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
    return search_obj(table, objno);

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
    return table->objs[idx].objflag;
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
    table->objs[idx].objflag = TRUE;
    return SUCCEED;
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
    return strdup(table->objs[idx].objname);
}

/*-------------------------------------------------------------------------
 * Function:    opens a file using the list of drivers
 *
 * Purpose:     Attempts to open a file with various VFL drivers.
 *
 * Return:      Success:        a file id for the opened file. If
 *                              DRIVERNAME is non-null then the first
 *                              DRIVERNAME_SIZE-1 characters of the driver
 *                              name are copied into the DRIVERNAME array
 *                              and null terminated.
 *
 *              Failure:        -1. If DRIVERNAME is non-null then the
 *                              first byte is set to the null terminator.
 *
 * Modifications:
 *              Robb Matzke, 2000-06-23
 *              We only have to initialize driver[] on the first call,
 *              thereby preventing memory leaks from repeated calls to
 *              H5Pcreate().
 *
 *              Robb Matzke, 2000-06-23
 *              Added DRIVERNAME_SIZE arg to prevent overflows when
 *              writing to DRIVERNAME.
 *
 *              Robb Matzke, 2000-06-23
 *              Added test to prevent coredump when the file could not be
 *              opened by any driver.
 *
 *              Robb Matzke, 2000-06-23
 *              Changed name from H5ToolsFopen() so it jives better with
 *              the names we already have at the top of this source file.
 *
 *              Thomas Radke, 2000-09-12
 *              Added Stream VFD to the driver[] array.
 *
 *              Bill Wendling, 2001-01-10
 *              Changed macro behavior so that if we have a version other
 *              than 1.2.x (i.e., > 1.2), then we do the drivers check.
 *-----------------------------------------------------------------------*/
hid_t
h5dump_fopen(const char *fname, char *drivername, size_t drivername_size)
{
    static struct {
        const char	*name;
        hid_t		fapl;
    } driver[16];
    static int          ndrivers = 0;
    hid_t               fid=(-1);
#ifndef VERSION12
    hid_t               fapl = H5P_DEFAULT;
#endif
    int                 drivernum;

    if (!ndrivers) {
        /* Build a list of file access property lists which we should try
         * when opening the file.  Eventually we'd like some way for the
         * user to augment/replace this list interactively. */
        driver[ndrivers].name = "sec2";
        driver[ndrivers].fapl = H5P_DEFAULT;
        ndrivers++;
        
#ifndef VERSION12
        driver[ndrivers].name = "family";
        driver[ndrivers].fapl = fapl = H5Pcreate(H5P_FILE_ACCESS);
        H5Pset_fapl_family(fapl, (hsize_t)0, H5P_DEFAULT);
        ndrivers++;

        driver[ndrivers].name = "split";
        driver[ndrivers].fapl = fapl = H5Pcreate(H5P_FILE_ACCESS);
        H5Pset_fapl_split(fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT);
        ndrivers++;

        driver[ndrivers].name = "multi";
        driver[ndrivers].fapl = fapl = H5Pcreate(H5P_FILE_ACCESS);
        H5Pset_fapl_multi(fapl, NULL, NULL, NULL, NULL, TRUE);
        ndrivers++;

#ifdef H5_HAVE_STREAM
        driver[ndrivers].name = "stream";
        driver[ndrivers].fapl = fapl = H5Pcreate(H5P_FILE_ACCESS);
        H5Pset_fapl_stream(fapl, NULL);
        ndrivers++;
#endif	/* H5_HAVE_STREAM */
#endif	/* !VERSION12 */
    }

    /* Try to open the file using each of the drivers */
    for (drivernum = 0; drivernum < ndrivers; drivernum++) {
        H5E_BEGIN_TRY {
            fid = H5Fopen(fname, H5F_ACC_RDONLY, driver[drivernum].fapl);
        } H5E_END_TRY;

        if (fid >= 0)
	    break;
    }

    /* Save the driver name */
    if (drivername && drivername_size){
        if (fid >= 0) {
            strncpy(drivername, driver[drivernum].name, drivername_size);
            drivername[drivername_size - 1] = '\0';
        } else {
            drivername[0] = '\0'; /*no file opened*/
        }
    }

    return fid;
}
