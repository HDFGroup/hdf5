/*
 * Copyright (c) 2001 National Center for Supercomputing Applications
 *                    All rights reserved.
 *
 * Programmer:  Bill Wendling <wendling@ncsa.uiuc.edu>
 *              Monday, 19. February 2001
 *
 * Purpose: These are string functions for us to use and abuse.
 */
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "H5private.h"
#include "h5tools.h"            /*for h5dump_t structure    */
#include "h5tools_str.h"        /*function prototypes       */

/* Variable length string datatype */
#define STR_INIT_LEN    4096    /*initial length            */

/*-------------------------------------------------------------------------
 * Function:	h5tools_str_close
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
void
h5tools_str_close(h5tools_str_t *str)
{
    if (str && str->nalloc) {
        free(str->s);
        memset(str, 0, sizeof(h5tools_str_t));
    }
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_str_len
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
size_t
h5tools_str_len(h5tools_str_t *str)
{
    return str->len;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_str_append
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
char *
h5tools_str_append(h5tools_str_t *str/*in,out*/, const char *fmt, ...)
{
    va_list ap;

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
	size_t nchars = (size_t)HDvsnprintf(str->s + str->len, avail, fmt, ap);

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
 * Function:	h5tools_str_reset
 *
 * Purpose:	Reset the string to the empty value. If no memory is
 *		allocated yet then initialize the h5tools_str_t struct.
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
char *
h5tools_str_reset(h5tools_str_t *str/*in,out*/)
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
 * Function:	h5tools_str_trunc
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
char *
h5tools_str_trunc(h5tools_str_t *str/*in,out*/, size_t size)
{
    if (size < str->len) {
	str->len = size;
	str->s[size] = '\0';
    }

    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_str_fmt
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
char *
h5tools_str_fmt(h5tools_str_t *str/*in,out*/, size_t start, const char *fmt)
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
    h5tools_str_trunc(str, start);
    h5tools_str_append(str, fmt, temp);

    /* Free the temp buffer if we allocated one */
    if (temp != _temp)
        free(temp);

    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_prefix
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
char *
h5tools_str_prefix(h5tools_str_t *str/*in,out*/, const h5dump_t *info,
                   hsize_t elmtno, int ndims, hsize_t min_idx[],
                   hsize_t max_idx[])
{
    hsize_t p_prod[H5S_MAX_RANK], p_idx[H5S_MAX_RANK];
    hsize_t n, i = 0;

    h5tools_str_reset(str);

    if (ndims > 0) {
        /*
         * Calculate the number of elements represented by a unit change in a
         * certain index position.
         */
        for (i = ndims - 1, p_prod[ndims - 1] = 1; i > 0; --i)
            p_prod[i - 1] = (max_idx[i] - min_idx[i]) * p_prod[i];

        /* Calculate the index values from the element number. */
        for (i = 0, n = elmtno; i < (hsize_t)ndims; i++) {
            p_idx[i] = n / p_prod[i] + min_idx[i];
            n %= p_prod[i];
        }

        /* Print the index values */
        for (i = 0; i < (hsize_t)ndims; i++) {
            if (i)
                h5tools_str_append(str, "%s", OPT(info->idx_sep, ","));

            h5tools_str_append(str, OPT(info->idx_n_fmt, "%lu"),
                               (unsigned long)p_idx[i]);
        }
    } else {
        /* Scalar */
        h5tools_str_append(str, OPT(info->idx_n_fmt, "%lu"), (unsigned long)0);
    }

    /* Add prefix and suffix to the index */
    return h5tools_str_fmt(str, 0, OPT(info->idx_fmt, "%s: "));
}
