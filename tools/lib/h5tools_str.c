/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Purpose: These are string functions for us to use and abuse.
 */
#include "H5private.h"
#include "h5tools.h" /* for h5tool_format_t structure */
#include "h5tools_ref.h"
#include "h5tools_str.h" /* function prototypes */

/* Copied from hl/src/H5LDprivate.h */
/* Info about the list of comma-separated compound fields */
typedef struct H5LD_memb_t {
    size_t tot_offset;
    size_t last_tsize;
    hid_t  last_tid;
    char **names;
} H5LD_memb_t;

/*
 * If REPEAT_VERBOSE is defined then character strings will be printed so
 * that repeated character sequences like "AAAAAAAAAA" are displayed as
 *
 *  'A' repeats 9 times
 *
 * Otherwise the format is more Perl-like
 *
 *  'A'*10
 *
 */
#define REPEAT_VERBOSE

/* Variable length string datatype */
#define STR_INIT_LEN 4096 /*initial length            */

static char *  h5tools_escape(char *s, size_t size);
static hbool_t h5tools_str_is_zero(const void *_mem, size_t size);
static void    h5tools_print_char(h5tools_str_t *str, const h5tool_format_t *info, char ch);
void           h5tools_str_indent(h5tools_str_t *str, const h5tool_format_t *info, h5tools_context_t *ctx);

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_close
 *
 * Purpose: Closes a string by releasing it's memory and setting the size
 *          information to zero.
 *
 * Return:  void
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *-------------------------------------------------------------------------
 */
void
h5tools_str_close(h5tools_str_t *str)
{
    if (str && str->nalloc) {
        HDfree(str->s);
        HDmemset(str, 0, sizeof(h5tools_str_t));
    }
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_len
 *
 * Purpose: Returns the length of the string, not counting the null
 *          terminator.
 *
 * Return:  Success:    Length of string
 *
 *          Failure:    0
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE size_t
h5tools_str_len(h5tools_str_t *str)
{
    return str->len;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_append
 *
 * Purpose: Formats variable arguments according to printf() format
 *          string and appends the result to variable length string STR.
 *
 * Return:  Success:    Pointer to buffer containing result.
 *
 *          Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_append(h5tools_str_t *str /*in,out*/, const char *fmt, ...)
{
    va_list ap;

    /* Make sure we have some memory into which to print */
    if (!str->s || str->nalloc <= 0)
        h5tools_str_reset(str);

    if (HDstrlen(fmt) == 0)
        /* nothing to print */
        return str->s;

    /* Format the arguments and append to the value already in `str' */
    while (1) {
        /* How many bytes available for new value, counting the new NUL */
        int    nchars = -1;
        size_t avail  = str->nalloc - str->len;

        HDva_start(ap, fmt);
        nchars = HDvsnprintf(str->s + str->len, avail, fmt, ap);
        HDva_end(ap);

        /* Note: HDvsnprintf() behaves differently on Windows as Unix, when
         * buffer is smaller than source string. On Unix, this function
         * returns length of the source string and copy string upto the
         * buffer size with NULL at the end of the buffer. However on
         * Windows with the same condition, this function returns -1 and
         * doesn't add NULL at the end of the buffer.
         * Because of this different return results, the strlen of the new string
         * is used to handle when HDvsnprintf() returns -1 on Windows due
         * to lack of buffer size, so try one more time after realloc more
         * buffer size before return NULL.
         */
        if (nchars < 0)
            /* failure, such as bad format */
            return NULL;

        if ((size_t)nchars >= avail || (0 == nchars && (HDstrcmp(fmt, "%s") != 0))) {
            /* Truncation return value as documented by C99, or zero return value with either of the
             * following conditions, each of which indicates that the proper C99 return value probably
             *  should have been positive when the format string is
             *  something other than "%s"
             * Alocate at least twice as much space and try again.
             */
            size_t newsize = MAX(str->len + (size_t)nchars + 1, 2 * str->nalloc);
            HDassert(newsize > str->nalloc); /*overflow*/
            str->s = (char *)HDrealloc(str->s, newsize);
            HDassert(str->s);
            str->nalloc = newsize;
        }
        else {
            /* Success */
            str->len += (size_t)nchars;
            break;
        }
    }
    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_reset
 *
 * Purpose: Reset the string to the empty value. If no memory is
 *          allocated yet then initialize the h5tools_str_t struct.
 *
 * Return:  Success:    Ptr to the buffer which contains a null
 *                      character as the first element.
 *
 *          Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_reset(h5tools_str_t *str /*in,out*/)
{
    if (!str->s || str->nalloc <= 0) {
        str->nalloc = STR_INIT_LEN;
        str->s      = (char *)HDmalloc(str->nalloc);
        HDassert(str->s);
    }

    str->s[0] = '\0';
    str->len  = 0;
    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_trunc
 *
 * Purpose: Truncate a string to be at most SIZE characters.
 *
 * Return:  Success:    Pointer to the string
 *
 *          Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_trunc(h5tools_str_t *str /*in,out*/, size_t size)
{
    if (size < str->len) {
        str->len     = size;
        str->s[size] = '\0';
    }

    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_fmt
 *
 * Purpose: Reformat a string contents beginning at character START
 *          according to printf format FMT. FMT should contain no format
 *          specifiers except possibly the `%s' variety. For example, if
 *          the input string is `hello' and the format is "<<%s>>" then
 *          the output value will be "<<hello>>".
 *
 * Return:  Success:    A pointer to the resulting string.
 *
 *          Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_fmt(h5tools_str_t *str /*in,out*/, size_t start, const char *fmt)
{
    char _temp[1024], *temp = _temp;

    HDassert(str);
    HDassert(fmt);

    /* If the format string is simply "%s" then don't bother doing anything */
    if (!HDstrcmp(fmt, "%s"))
        return str->s;

    /*
     * Save the input value if there is a `%' anywhere in FMT.  Otherwise
     * don't bother because we don't need a temporary copy.
     */
    if (HDstrchr(fmt, '%')) {
        size_t n = sizeof(_temp);
        if (str->len - start + 1 > n) {
            n    = str->len - start + 1;
            temp = (char *)HDmalloc(n);
            HDassert(temp);
        }

        HDstrncpy(temp, str->s + start, n);
    }

    /* Reset the output string and append a formatted version */
    h5tools_str_trunc(str, start);
    h5tools_str_append(str, fmt, temp);

    /* Free the temp buffer if we allocated one */
    if (temp != _temp)
        HDfree(temp);

    return str->s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_prefix
 *
 * Purpose: Renders the line prefix value into string STR.
 *
 * Return:  Success:    Pointer to the prefix.
 *          Failure:    NULL
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_prefix(h5tools_str_t *str /*in,out*/, const h5tool_format_t *info, hsize_t elmtno,
                   h5tools_context_t *ctx)
{
    size_t i = 0;

    H5TOOLS_START_DEBUG("");

    H5TOOLS_DEBUG("elmtno=%ld, ctx->ndims=%d", elmtno, ctx->ndims);
    h5tools_str_reset(str);

    calc_acc_pos(ctx->ndims, elmtno, ctx->acc, ctx->pos);
    if (ctx->ndims > 0) {
        /* Print the index values */
        for (i = 0; i < (size_t)ctx->ndims; i++) {
            if (i)
                h5tools_str_append(str, "%s", OPT(info->idx_sep, ","));

            h5tools_str_append(str, OPT(info->idx_n_fmt, HSIZE_T_FORMAT), (hsize_t)ctx->pos[i]);
        }
    }
    else /* Scalar */
        h5tools_str_append(str, OPT(info->idx_n_fmt, HSIZE_T_FORMAT), (hsize_t)elmtno);
    H5TOOLS_DEBUG("str=%s", str->s);

    H5TOOLS_ENDDEBUG("");

    /* Add prefix and suffix to the index */
    return h5tools_str_fmt(str, (size_t)0, OPT(info->idx_fmt, "%s: "));
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_region_prefix
 *
 * Purpose: Renders the line prefix value into string STR. Region reference specific.
 *
 * Return:  Success:    Pointer to the prefix.
 *          Failure:    NULL
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_region_prefix(h5tools_str_t *str /*in,out*/, const h5tool_format_t *info, hsize_t elmtno,
                          const hsize_t *ptdata, h5tools_context_t *ctx)
{
    size_t i = 0;

    H5TOOLS_START_DEBUG("");

    H5TOOLS_DEBUG("elmtno=%ld, ctx->ndims=%d", elmtno, ctx->ndims);
    h5tools_str_reset(str);

    calc_acc_pos(ctx->ndims, elmtno, ctx->acc, ctx->pos);
    if (ctx->ndims > 0) {
        /* Print the index values */
        for (i = 0; i < (size_t)ctx->ndims; i++) {
            ctx->pos[i] += (unsigned long)ptdata[ctx->sm_pos + i];
            if (i)
                h5tools_str_append(str, "%s", OPT(info->idx_sep, ","));

            h5tools_str_append(str, OPT(info->idx_n_fmt, HSIZE_T_FORMAT), (hsize_t)ctx->pos[i]);
        }
    }
    else /* Scalar */
        h5tools_str_append(str, OPT(info->idx_n_fmt, HSIZE_T_FORMAT), (hsize_t)0);
    H5TOOLS_DEBUG("str=%s", str->s);

    H5TOOLS_ENDDEBUG("");

    /* Add prefix and suffix to the index */
    return h5tools_str_fmt(str, (size_t)0, OPT(info->idx_fmt, "%s: "));
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_dump_space_slabs
 *
 * Purpose: Prints information about a dataspace selection by appending
 *          the information to the specified string.
 *
 * Return:  none
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      h5tools_str_t     *str
 *-------------------------------------------------------------------------
 */
void
h5tools_str_dump_space_slabs(h5tools_str_t *str, hid_t rspace, const h5tool_format_t *info,
                             h5tools_context_t *ctx)
{
    hsize_t start[H5S_MAX_RANK];
    hsize_t stride[H5S_MAX_RANK];
    hsize_t count[H5S_MAX_RANK];
    hsize_t block[H5S_MAX_RANK];
    int     j;
    int     ndims = H5Sget_simple_extent_ndims(rspace);

    H5Sget_regular_hyperslab(rspace, start, stride, count, block);

    /* Print hyperslab information */

    /* Start coordinates */
    h5tools_str_append(str, "%s%s ", info->line_indent, START);
    for (j = 0; j < ndims; j++)
        h5tools_str_append(str, "%s" HSIZE_T_FORMAT, j ? "," : "(", start[j]);
    h5tools_str_append(str, ")");
    h5tools_str_append(str, "%s", "\n");
    h5tools_str_indent(str, info, ctx);

    /* Stride coordinates */
    h5tools_str_append(str, "%s ", STRIDE);
    for (j = 0; j < ndims; j++)
        h5tools_str_append(str, "%s" HSIZE_T_FORMAT, j ? "," : "(", stride[j]);
    h5tools_str_append(str, ")");
    h5tools_str_append(str, "%s", "\n");
    h5tools_str_indent(str, info, ctx);

    /* Count coordinates */
    h5tools_str_append(str, "%s ", COUNT);
    for (j = 0; j < ndims; j++) {
        if (count[j] == H5S_UNLIMITED)
            h5tools_str_append(str, "%s%s", j ? "," : "(", "H5S_UNLIMITED");
        else
            h5tools_str_append(str, "%s" HSIZE_T_FORMAT, j ? "," : "(", count[j]);
    }
    h5tools_str_append(str, ")");
    h5tools_str_append(str, "%s", "\n");
    h5tools_str_indent(str, info, ctx);

    /* Block coordinates */
    h5tools_str_append(str, "%s ", BLOCK);
    for (j = 0; j < ndims; j++) {
        if (block[j] == H5S_UNLIMITED)
            h5tools_str_append(str, "%s%s", j ? "," : "(", "H5S_UNLIMITED");
        else
            h5tools_str_append(str, "%s" HSIZE_T_FORMAT, j ? "," : "(", block[j]);
    }
    h5tools_str_append(str, ")");
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_dump_space_blocks
 *
 * Purpose: Prints information about a dataspace selection by appending
 *          the information to the specified string.
 *
 * Return:  none
 *
 * In/Out:  h5tools_str_t     *str
 *-------------------------------------------------------------------------
 */
void
h5tools_str_dump_space_blocks(h5tools_str_t *str, hid_t rspace, const h5tool_format_t *info)
{
    hssize_t snblocks;

    /*
     * This function fails if the rspace does not have blocks.
     */
    H5E_BEGIN_TRY
    {
        snblocks = H5Sget_select_hyper_nblocks(rspace);
    }
    H5E_END_TRY;

    /* Print block information */
    if (snblocks > 0) {
        hsize_t  alloc_size;
        hsize_t  nblocks;
        hsize_t *ptdata;
        hsize_t  u;
        unsigned ndims = (unsigned)H5Sget_simple_extent_ndims(rspace);

        nblocks    = (hsize_t)snblocks;
        alloc_size = nblocks * ndims * 2 * sizeof(ptdata[0]);
        HDassert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/
        ptdata = (hsize_t *)HDmalloc((size_t)alloc_size);
        H5Sget_select_hyper_blocklist(rspace, (hsize_t)0, nblocks, ptdata);

        for (u = 0; u < nblocks; u++) {
            unsigned v;

            h5tools_str_append(str, info->dset_blockformat_pre, u ? "," OPTIONAL_LINE_BREAK " " : "",
                               (unsigned long)u);

            /* Start coordinates and opposite corner */
            for (v = 0; v < ndims; v++)
                h5tools_str_append(str, "%s" HSIZE_T_FORMAT, v ? "," : "(", ptdata[u * 2 * ndims + v]);

            for (v = 0; v < ndims; v++)
                h5tools_str_append(str, "%s" HSIZE_T_FORMAT, v ? "," : ")-(",
                                   ptdata[u * 2 * ndims + v + ndims]);

            h5tools_str_append(str, ")");
        }

        HDfree(ptdata);
    } /* end if (nblocks > 0) */
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_dump_space_points
 *
 * Purpose: Prints information about a dataspace selection by appending
 *          the information to the specified string.
 *
 * Return:  none
 *
 * In/Out:  h5tools_str_t     *str
 *-------------------------------------------------------------------------
 */
void
h5tools_str_dump_space_points(h5tools_str_t *str, hid_t rspace, const h5tool_format_t *info)
{
    hssize_t snpoints;

    /*
     * This function fails if the rspace does not have points.
     */
    H5E_BEGIN_TRY
    {
        snpoints = H5Sget_select_elem_npoints(rspace);
    }
    H5E_END_TRY;

    /* Print point information */
    if (snpoints > 0) {
        hsize_t  alloc_size;
        hsize_t  npoints;
        hsize_t *ptdata;
        hsize_t  u;
        unsigned ndims = (unsigned)H5Sget_simple_extent_ndims(rspace);

        npoints    = (hsize_t)snpoints;
        alloc_size = npoints * ndims * sizeof(ptdata[0]);
        HDassert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/
        ptdata = (hsize_t *)HDmalloc((size_t)alloc_size);
        H5Sget_select_elem_pointlist(rspace, (hsize_t)0, npoints, ptdata);

        for (u = 0; u < npoints; u++) {
            unsigned v;

            h5tools_str_append(str, info->dset_ptformat_pre, u ? "," OPTIONAL_LINE_BREAK " " : "",
                               (unsigned long)u);

            for (v = 0; v < ndims; v++)
                h5tools_str_append(str, "%s" HSIZE_T_FORMAT, v ? "," : "(", (ptdata[u * ndims + v]));

            h5tools_str_append(str, ")");
        }

        HDfree(ptdata);
    } /* end if (npoints > 0) */
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_print_char
 *
 * Purpose: Shove a character into the STR.
 *
 * Return:  Nothing
 *
 *-------------------------------------------------------------------------
 */
static void
h5tools_print_char(h5tools_str_t *str, const h5tool_format_t *info, char ch)
{
    if (info->str_locale == ESCAPE_HTML) {
        if (ch <= ' ' || ch > '~')
            h5tools_str_append(str, "%%%02x", ch);
        else
            h5tools_str_append(str, "%c", ch);
    }
    else {
        switch (ch) {
            case '"':
                if (!info->do_escape)
                    h5tools_str_append(str, "\"");
                else
                    h5tools_str_append(str, "\\\"");
                break;
            case '\\':
                if (!info->do_escape)
                    h5tools_str_append(str, "\\");
                else
                    h5tools_str_append(str, "\\\\");
                break;
            case '\b':
                if (!info->do_escape)
                    h5tools_str_append(str, "\b");
                else
                    h5tools_str_append(str, "\\b");
                break;
            case '\f':
                if (!info->do_escape)
                    h5tools_str_append(str, "\f");
                else
                    h5tools_str_append(str, "\\f");
                break;
            case '\n':
                if (!info->do_escape) {
                    h5tools_str_append(str, "\n");
                    h5tools_str_append(str, "           ");
                }
                else
                    h5tools_str_append(str, "\\n");
                break;
            case '\r':
                if (!info->do_escape) {
                    h5tools_str_append(str, "\r");
                    h5tools_str_append(str, "           ");
                }
                else
                    h5tools_str_append(str, "\\r");
                break;
            case '\t':
                if (!info->do_escape)
                    h5tools_str_append(str, "\t");
                else
                    h5tools_str_append(str, "\\t");
                break;
            default:
                if (isprint(ch))
                    h5tools_str_append(str, "%c", ch);
                else
                    h5tools_str_append(str, "\\%03o", ch);

                break;
        }
    }
}
void
h5tools_str_indent(h5tools_str_t *str, const h5tool_format_t *info, h5tools_context_t *ctx)
{
    unsigned u, indentlevel = 0;

    /* Write new prefix */
    if (ctx->indent_level > 0)
        indentlevel = ctx->indent_level;
    else
        /*
         * This is because sometimes we don't print out all the header
         * info for the data (like the tattr-2.ddl example). If that happens
         * the ctx->indent_level is negative so we need to skip the above and
         * just print out the default indent levels.
         */
        indentlevel = ctx->default_indent_level;

    for (u = 0; u < indentlevel; u++)
        h5tools_str_append(str, "%s", OPT(info->line_indent, ""));
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_sprint
 *
 * Purpose:     Renders the value pointed to by VP of type TYPE into variable
 *              length string STR.
 *
 * Return:      A pointer to memory containing the result or NULL on error.
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_sprint(h5tools_str_t *str, const h5tool_format_t *info, hid_t container, hid_t type, void *vp,
                   h5tools_context_t *ctx)
{
    size_t         nsize, offset, size = 0, nelmts, start;
    H5T_sign_t     nsign;
    char *         name   = NULL;
    unsigned char *ucp_vp = (unsigned char *)vp;
    char *         cp_vp  = (char *)vp;
    hid_t          memb   = H5I_INVALID_HID;
    hid_t          obj    = H5I_INVALID_HID;
    static char    fmt_llong[8], fmt_ullong[8];
    H5T_str_t      pad;
    H5T_class_t    type_class;
    char *         ret_value = NULL;

    H5TOOLS_START_DEBUG("");
    /* Build default formats for long long types */
    if (!fmt_llong[0]) {
        HDsnprintf(fmt_llong, sizeof(fmt_llong), "%%%sd", H5_PRINTF_LL_WIDTH);
        HDsnprintf(fmt_ullong, sizeof(fmt_ullong), "%%%su", H5_PRINTF_LL_WIDTH);
    }

    /* Append value depending on data type */
    start = h5tools_str_len(str);

    nsize = H5Tget_size(type);
    nsign = H5Tget_sign(type);
    if (info->raw) {
        size_t i;

        H5TOOLS_DEBUG("info->raw");
        if (1 == nsize)
            h5tools_str_append(str, OPT(info->fmt_raw, "0x%02x"), ucp_vp[0]);
        else
            for (i = 0; i < nsize; i++) {
                if (i)
                    h5tools_str_append(str, ":");
                h5tools_str_append(str, OPT(info->fmt_raw, "%02x"), ucp_vp[i]);
            }
    }
    else {
        H5TOOLS_DEBUG("H5Tget_class(type)");
        if ((type_class = H5Tget_class(type)) < 0) {
            H5TOOLS_ENDDEBUG(" with %s", "NULL");
            return NULL;
        }
        switch (type_class) {
            case H5T_FLOAT:
                H5TOOLS_DEBUG("H5T_FLOAT");
                if (sizeof(float) == nsize) {
                    /* if (H5Tequal(type, H5T_NATIVE_FLOAT)) */
                    float tempfloat;

                    HDmemcpy(&tempfloat, vp, sizeof(float));
                    h5tools_str_append(str, OPT(info->fmt_float, "%g"), (double)tempfloat);
                }
                else if (sizeof(double) == nsize) {
                    /* if (H5Tequal(type, H5T_NATIVE_DOUBLE)) */
                    double tempdouble;

                    HDmemcpy(&tempdouble, vp, sizeof(double));
                    h5tools_str_append(str, OPT(info->fmt_double, "%g"), tempdouble);
#if H5_SIZEOF_LONG_DOUBLE != 0
                }
                else if (sizeof(long double) == nsize) {
                    /* if (H5Tequal(type, H5T_NATIVE_LDOUBLE)) */
                    long double templdouble;

                    HDmemcpy(&templdouble, vp, sizeof(long double));
                    h5tools_str_append(str, "%Lg", templdouble);
#endif
                }
                else {
                    size_t i;

                    for (i = 0; i < nsize; i++) {
                        if (i)
                            h5tools_str_append(str, ":");
                        h5tools_str_append(str, OPT(info->fmt_raw, "%02x"), ucp_vp[i]);
                    }
                }
                break;

            case H5T_STRING: {
                unsigned int i;
                char         quote = '\0';
                char *       s;

                H5TOOLS_DEBUG("H5T_STRING");
                quote = '\0';
                if (H5Tis_variable_str(type)) {
                    /* cp_vp is the pointer into the struct where a `char*' is stored. So we have
                     * to dereference the pointer to get the `char*' to pass to HDstrlen(). */
                    s = *(char **)((void *)cp_vp);
                    if (s != NULL)
                        size = HDstrlen(s);
                }
                else {
                    s    = cp_vp;
                    size = H5Tget_size(type);
                }
                pad = H5Tget_strpad(type);

                /* Check for NULL pointer for string */
                if (s == NULL)
                    h5tools_str_append(str, "NULL");
                else {
                    for (i = 0; i < size && (s[i] || pad != H5T_STR_NULLTERM); i++) {
                        unsigned j = 1;

                        /*
                         * Count how many times the next character repeats. If the
                         * threshold is zero then that means it can repeat any number
                         * of times.
                         */
                        if (info->str_repeat > 0)
                            while (i + j < size && s[i] == s[i + j])
                                j++;

                        /*
                         * Print the opening quote.  If the repeat count is high enough to
                         * warrant printing the number of repeats instead of enumerating
                         * the characters, then make sure the character to be repeated is
                         * in it's own quote.
                         */
                        if (info->str_repeat > 0 && j > info->str_repeat) {
                            if (quote)
                                h5tools_str_append(str, "%c", quote);

                            quote = '\'';
                            h5tools_str_append(str, "%s%c", i ? " " : "", quote);
                        }
                        else if (!quote) {
                            quote = '"';
                            h5tools_str_append(str, "%s%c", i ? " " : "", quote);
                        }

                        /* Print the character */
                        h5tools_print_char(str, info, s[i]);

                        /* Print the repeat count */
                        if (info->str_repeat && j > info->str_repeat) {
#ifdef REPEAT_VERBOSE
                            h5tools_str_append(str, "%c repeats %d times", quote, j - 1);
#else
                            h5tools_str_append(str, "%c*%d", quote, j - 1);
#endif /* REPEAT_VERBOSE */
                            quote = '\0';
                            i += j - 1;
                        }
                    }

                    if (quote)
                        h5tools_str_append(str, "%c", quote);

                    if (i == 0)
                        /*empty string*/
                        h5tools_str_append(str, "\"\"");
                } /* end else */
            } break;

            case H5T_INTEGER:
                H5TOOLS_DEBUG("H5T_INTEGER");
                if (sizeof(char) == nsize) {
                    if (info->ascii)
                        h5tools_print_char(str, info, (char)(*ucp_vp));
                    else if (H5T_SGN_NONE == nsign) {
                        unsigned char tempuchar;

                        HDmemcpy(&tempuchar, ucp_vp, sizeof(unsigned char));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(unsigned char))
                                tempuchar = 0;
                            else
                                tempuchar =
                                    (unsigned char)((unsigned long long)(tempuchar >> packed_data_offset) &
                                                    packed_data_mask);
                        }
                        h5tools_str_append(str, OPT(info->fmt_uchar, "%hhu"), tempuchar);
                    }
                    else {
                        signed char tempchar;

                        HDmemcpy(&tempchar, cp_vp, sizeof(char));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(char))
                                tempchar = 0;
                            else
                                tempchar =
                                    (signed char)((unsigned long long)(tempchar >> packed_data_offset) &
                                                  packed_data_mask);
                        }
                        h5tools_str_append(str, OPT(info->fmt_schar, "%hhd"), tempchar);
                    }
                } /* end if (sizeof(char) == nsize) */
                else if (sizeof(int) == nsize) {
                    if (H5T_SGN_NONE == nsign) {
                        unsigned int tempuint;

                        HDmemcpy(&tempuint, vp, sizeof(unsigned int));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(unsigned int))
                                tempuint = 0;
                            else
                                tempuint = (unsigned)((tempuint >> packed_data_offset) & packed_data_mask);
                        }
                        h5tools_str_append(str, OPT(info->fmt_uint, "%u"), tempuint);
                    }
                    else {
                        int tempint;

                        HDmemcpy(&tempint, vp, sizeof(int));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(int))
                                tempint = 0;
                            else
                                tempint = (int)((unsigned long long)(tempint >> packed_data_offset) &
                                                packed_data_mask);
                        }
                        h5tools_str_append(str, OPT(info->fmt_int, "%d"), tempint);
                    }
                } /* end if (sizeof(int) == nsize) */
                else if (sizeof(short) == nsize) {
                    if (H5T_SGN_NONE == nsign) {
                        unsigned short tempushort;

                        HDmemcpy(&tempushort, vp, sizeof(unsigned short));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(unsigned short))
                                tempushort = 0;
                            else
                                tempushort =
                                    (unsigned short)((unsigned long long)(tempushort >> packed_data_offset) &
                                                     packed_data_mask);
                        }
                        h5tools_str_append(str, OPT(info->fmt_ushort, "%hu"), tempushort);
                    }
                    else {
                        short tempshort;

                        HDmemcpy(&tempshort, vp, sizeof(short));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(short))
                                tempshort = 0;
                            else
                                tempshort = (short)((unsigned long long)(tempshort >> packed_data_offset) &
                                                    packed_data_mask);
                        }
                        h5tools_str_append(str, OPT(info->fmt_short, "%hd"), tempshort);
                    }
                } /* end if (sizeof(short) == nsize) */
                else if (sizeof(long) == nsize) {
                    if (H5T_SGN_NONE == nsign) {
                        unsigned long tempulong;

                        HDmemcpy(&tempulong, vp, sizeof(unsigned long));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(unsigned long))
                                tempulong = 0;
                            else
                                tempulong = (tempulong >> packed_data_offset) & packed_data_mask;
                        }
                        h5tools_str_append(str, OPT(info->fmt_ulong, "%lu"), tempulong);
                    }
                    else {
                        long templong;

                        HDmemcpy(&templong, vp, sizeof(long));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(long))
                                templong = 0;
                            else
                                templong = (long)((unsigned long long)(templong >> packed_data_offset) &
                                                  packed_data_mask);
                        }
                        h5tools_str_append(str, OPT(info->fmt_long, "%ld"), templong);
                    }
                } /* end if (sizeof(long) == nsize) */
#if H5_SIZEOF_LONG != H5_SIZEOF_LONG_LONG
                else if (sizeof(long long) == nsize) {
                    if (H5T_SGN_NONE == nsign) {
                        unsigned long long tempullong;

                        HDmemcpy(&tempullong, vp, sizeof(unsigned long long));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(unsigned long long))
                                tempullong = 0;
                            else
                                tempullong = (tempullong >> packed_data_offset) & packed_data_mask;
                        }
                        h5tools_str_append(str, OPT(info->fmt_ullong, fmt_ullong), tempullong);
                    }
                    else {
                        long long templlong;

                        HDmemcpy(&templlong, vp, sizeof(long long));
                        if (packed_bits_num) {
                            if (packed_data_offset >= 8 * sizeof(long long))
                                templlong = 0;
                            else
                                templlong = (templlong >> packed_data_offset) & packed_data_mask;
                        }
                        h5tools_str_append(str, OPT(info->fmt_llong, fmt_llong), templlong);
                    }
                } /* end if (sizeof(long long) == nsize) */
#endif            /* H5_SIZEOF_LONG != H5_SIZEOF_LONG_LONG */
                break;

            case H5T_COMPOUND:
                H5TOOLS_DEBUG("H5T_COMPOUND");
                if (ctx->cmpd_listv) {                           /* there is <list_of_fields> */
                    unsigned                  save_indent_level; /* The indentation level */
                    size_t                    curr_field;        /* Current field to display */
                    int                       i = 0;             /* Local index variable */
                    unsigned                  x = 0;             /* Local index variable */
                    const H5LD_memb_t *const *listv; /* Vector of information for <list_of_fields> */

                    listv           = ctx->cmpd_listv;
                    ctx->cmpd_listv = NULL;

                    h5tools_str_append(str, "%s", OPT(info->cmpd_pre, "{"));

                    /*
                     * Go through the vector containing info about the comma-separated list of
                     * compound fields and then members in each field:
                     *       put in "{", "}", ",", member name and value accordingly.
                     */
                    save_indent_level = ctx->indent_level;
                    for (curr_field = 0; listv[curr_field] != NULL; curr_field++) {
                        if (curr_field)
                            h5tools_str_append(str, "%s", OPT(info->cmpd_sep, ", " OPTIONAL_LINE_BREAK));
                        else
                            h5tools_str_append(str, "%s", OPT(info->cmpd_end, ""));

                        if (info->arr_linebreak)
                            h5tools_str_indent(str, info, ctx);

                        /* Process members of each field */
                        for (i = 0; listv[curr_field]->names[i] != NULL; i++) {
                            h5tools_str_append(str, OPT(info->cmpd_name, ""), listv[curr_field]->names[i]);
                            if (i) {
                                ctx->indent_level++;
                                h5tools_str_append(str, "%s", OPT(info->cmpd_pre, "{"));
                            }
                        }
                        h5tools_str_sprint(str, info, container, listv[curr_field]->last_tid,
                                           cp_vp + listv[curr_field]->tot_offset, ctx);
                        if (ctx->indent_level > 0)
                            for (x = ctx->indent_level; x > 0; x--)
                                h5tools_str_append(str, "%s", OPT(info->cmpd_suf, "}"));
                        ctx->indent_level = save_indent_level;
                    }

                    if (info->arr_linebreak) {
                        h5tools_str_append(str, "%s", OPT(info->cmpd_end, ""));
                        h5tools_str_indent(str, info, ctx);
                    }
                    h5tools_str_append(str, "%s", OPT(info->cmpd_suf, "}"));

                    ctx->cmpd_listv = info->cmpd_listv;
                }
                else {
                    int retvalue;

                    retvalue = H5Tget_nmembers(type);
                    if (retvalue >= 0) {
                        unsigned j;
                        unsigned nmembs = (unsigned)retvalue;

                        h5tools_str_append(str, "%s", OPT(info->cmpd_pre, "{"));

                        ctx->indent_level++;

                        for (j = 0; j < nmembs; j++) {
                            if (j)
                                h5tools_str_append(str, "%s", OPT(info->cmpd_sep, ", " OPTIONAL_LINE_BREAK));
                            else
                                h5tools_str_append(str, "%s", OPT(info->cmpd_end, ""));

                            if (info->arr_linebreak)
                                h5tools_str_indent(str, info, ctx);

                            /* The name */
                            name = H5Tget_member_name(type, j);
                            h5tools_str_append(str, OPT(info->cmpd_name, ""), name);
                            H5free_memory(name);

                            /* The value */
                            offset = H5Tget_member_offset(type, j);
                            memb   = H5Tget_member_type(type, j);

                            h5tools_str_sprint(str, info, container, memb, cp_vp + offset, ctx);

                            H5Tclose(memb);
                        }
                        ctx->indent_level--;

                        if (info->arr_linebreak) {
                            h5tools_str_append(str, "%s", OPT(info->cmpd_end, ""));
                            h5tools_str_indent(str, info, ctx);
                        }
                        h5tools_str_append(str, "%s", OPT(info->cmpd_suf, "}"));
                    }
                }
                break;

            case H5T_ENUM: {
                char enum_name[1024];

                H5TOOLS_DEBUG("H5T_ENUM");
                if (H5Tenum_nameof(type, vp, enum_name, sizeof enum_name) >= 0)
                    h5tools_str_append(str, h5tools_escape(enum_name, sizeof(enum_name)));
                else {
                    size_t i;
                    if (1 == nsize)
                        h5tools_str_append(str, "0x%02x", ucp_vp[0]);
                    else
                        for (i = 0; i < nsize; i++)
                            h5tools_str_append(str, "%s%02x", i ? ":" : "", ucp_vp[i]);
                }
            } break;

            case H5T_REFERENCE:
                H5TOOLS_DEBUG("H5T_REFERENCE");
                if (h5tools_str_is_zero(vp, nsize))
                    h5tools_str_append(str, "NULL");
                else {
                    if (H5Tequal(type, H5T_STD_REF)) {
                        H5O_type_t obj_type = -1; /* Object type */
                        H5R_type_t ref_type;      /* Reference type */
                        H5R_ref_t *ref_vp = (H5R_ref_t *)vp;

                        H5TOOLS_DEBUG("H5T_REFERENCE:H5T_STD_REF");
                        ref_type = H5Rget_type(ref_vp);
                        H5Rget_obj_type3(ref_vp, H5P_DEFAULT, &obj_type);
                        switch (ref_type) {
                            case H5R_OBJECT1: {
                                /* Object references -- show the type and OID of the referenced object. */
                                H5O_info2_t oi;
                                char *      obj_tok_str = NULL;

                                H5TOOLS_DEBUG("ref_type is H5R_OBJECT1");
                                if ((obj = H5Ropen_object(ref_vp, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                    H5Oget_info3(obj, &oi, H5O_INFO_BASIC);
                                    H5Otoken_to_str(obj, &oi.token, &obj_tok_str);
                                }
                                else
                                    H5TOOLS_ERROR(NULL, "H5Ropen_object H5R_OBJECT1 failed");

                                /* Print object type and close object */
                                switch (obj_type) {
                                    case H5O_TYPE_GROUP:
                                        h5tools_str_append(str, "%u-%s", (unsigned)oi.type, H5_TOOLS_GROUP);
                                        break;

                                    case H5O_TYPE_DATASET:
                                        h5tools_str_append(str, "%u-%s", (unsigned)oi.type, H5_TOOLS_DATASET);
                                        break;

                                    case H5O_TYPE_NAMED_DATATYPE:
                                        h5tools_str_append(str, "%u-%s", (unsigned)oi.type,
                                                           H5_TOOLS_DATATYPE);
                                        break;

                                    case H5O_TYPE_MAP:
                                    case H5O_TYPE_UNKNOWN:
                                    case H5O_TYPE_NTYPES:
                                    default:
                                        h5tools_str_append(str, "%u-%s", (unsigned)oi.type, H5_TOOLS_UNKNOWN);
                                        break;
                                } /* end switch */

                                h5tools_str_sprint_reference(str, ref_vp);

                                /* Print OID */
                                if (info->obj_hidefileno)
                                    h5tools_str_append(str, info->obj_format, obj_tok_str);
                                else
                                    h5tools_str_append(str, info->obj_format, oi.fileno, obj_tok_str);

                                if (obj_tok_str) {
                                    H5free_memory(obj_tok_str);
                                    obj_tok_str = NULL;
                                }

                                if (obj >= 0)
                                    if (H5Oclose(obj) < 0)
                                        H5TOOLS_ERROR(NULL, "H5Oclose H5R_OBJECT1 failed");
                            }

                            break;
                            case H5R_DATASET_REGION1:
                                H5TOOLS_DEBUG("ref_type is H5R_DATASET_REGION1");
                                h5tools_str_append(str, H5_TOOLS_DATASET);
                                h5tools_str_sprint_reference(str, ref_vp);
                                break;
                            case H5R_OBJECT2:
                                H5TOOLS_DEBUG("ref_type is H5R_OBJECT2");
                                switch (obj_type) {
                                    case H5O_TYPE_GROUP:
                                        h5tools_str_append(str, H5_TOOLS_GROUP);
                                        break;

                                    case H5O_TYPE_DATASET:
                                        h5tools_str_append(str, H5_TOOLS_DATASET);
                                        break;

                                    case H5O_TYPE_NAMED_DATATYPE:
                                        h5tools_str_append(str, H5_TOOLS_DATATYPE);
                                        break;

                                    case H5O_TYPE_MAP:
                                    case H5O_TYPE_UNKNOWN:
                                    case H5O_TYPE_NTYPES:
                                    default:
                                        h5tools_str_append(str, H5_TOOLS_UNKNOWN);
                                        break;
                                } /* end switch */
                                h5tools_str_sprint_reference(str, ref_vp);
                                break;
                            case H5R_DATASET_REGION2:
                                H5TOOLS_DEBUG("ref_type is H5R_DATASET_REGION2");
                                h5tools_str_append(str, H5_TOOLS_DATASET);
                                h5tools_str_sprint_reference(str, ref_vp);
                                break;
                            case H5R_ATTR:
                                H5TOOLS_DEBUG("ref_type is H5R_ATTR");
                                h5tools_str_append(str, H5_TOOLS_ATTRIBUTE);
                                h5tools_str_sprint_reference(str, ref_vp);
                                break;
                            case H5R_BADTYPE:
                            case H5R_MAXTYPE:
                            default:
                                break;
                        } /* end switch */
                        H5TOOLS_DEBUG("H5T_REFERENCE:H5T_STD_REF end");
                    }
                    else if (H5Tequal(type, H5T_STD_REF_DSETREG)) {
                        /* if(nsize == H5R_DSET_REG_REF_BUF_SIZE) */
                        H5TOOLS_DEBUG("H5T_REFERENCE:H5T_STD_REF_DSETREG");
                    }
                    else if (H5Tequal(type, H5T_STD_REF_OBJ)) {
                        /* if (nsize == H5R_OBJ_REF_BUF_SIZE) */
                        H5TOOLS_DEBUG("H5T_REFERENCE:H5T_STD_REF_OBJ");
                    } /* end else if (H5Tequal(type, H5T_STD_REF_OBJ)) */
                }
                break;

            case H5T_ARRAY: {
                int        k, ndims;
                hsize_t    i, dims[H5S_MAX_RANK], temp_nelmts;
                static int is_next_arry_elmt = 0;

                H5TOOLS_DEBUG("H5T_ARRAY");
                /* Get the array's base datatype for each element */
                memb  = H5Tget_super(type);
                size  = H5Tget_size(memb);
                ndims = H5Tget_array_ndims(type);
                H5Tget_array_dims2(type, dims);
                HDassert(ndims >= 1 && ndims <= H5S_MAX_RANK);

                /* Calculate the number of array elements */
                for (k = 0, nelmts = 1; k < ndims; k++) {
                    temp_nelmts = nelmts;
                    temp_nelmts *= dims[k];
                    HDassert(temp_nelmts == (hsize_t)((size_t)temp_nelmts));
                    nelmts = (size_t)temp_nelmts;
                }
                /* Print the opening bracket */
                h5tools_str_append(str, "%s", OPT(info->arr_pre, "["));

                ctx->indent_level++;

                for (i = 0; i < nelmts; i++) {
                    if (i)
                        h5tools_str_append(str, "%s", OPT(info->arr_sep, "," OPTIONAL_LINE_BREAK));

                    if (info->arr_linebreak && i && i % dims[ndims - 1] == 0) {
                        h5tools_str_append(str, "%s", "\n");
                        h5tools_str_indent(str, info, ctx);

                    } /* end if */
                    else if (i && info->arr_sep) {
                        /* if next element begin, add next line with indent */
                        if (is_next_arry_elmt) {
                            is_next_arry_elmt = 0;

                            h5tools_str_append(str, "%s", "\n ");
                            h5tools_str_indent(str, info, ctx);
                        }
                        /* otherwise just add space */
                        else
                            h5tools_str_append(str, " ");

                    } /* end else if */

                    /* Dump values in an array element */
                    is_next_arry_elmt = 0; /* dump all values in the array element, so turn it off */
                    h5tools_str_sprint(str, info, container, memb, cp_vp + i * size, ctx);
                } /* end for */

                ctx->indent_level--;

                /* Print the closing bracket */
                h5tools_str_append(str, "%s", OPT(info->arr_suf, "]"));
                is_next_arry_elmt = 1; /* set for begining of next array element */
                H5Tclose(memb);
            } break;

            case H5T_VLEN: {
                unsigned int i;

                H5TOOLS_DEBUG("H5T_VLEN");
                /* Get the VL sequences's base datatype for each element */
                memb = H5Tget_super(type);
                size = H5Tget_size(memb);

                /* Print the opening bracket */
                h5tools_str_append(str, "%s", OPT(info->vlen_pre, "("));

                /* Get the number of sequence elements */
                nelmts = ((hvl_t *)((void *)cp_vp))->len;

                for (i = 0; i < nelmts; i++) {
                    if (i)
                        h5tools_str_append(str, "%s", OPT(info->vlen_sep, "," OPTIONAL_LINE_BREAK));

#ifdef LATER
                    /* Need to fix so VL data breaks at correct location on end of line -QAK */
                    if (info->arr_linebreak && h5tools_str_len(str) >= info->line_ncols) {
                        int x;

                        h5tools_str_append(str, "%s", "\n");

                        /* need to indent some more here */
                        if (ctx->indent_level >= 0)
                            h5tools_str_append(str, "%s", OPT(info->line_pre, ""));

                        for (x = 0; x < ctx->indent_level + 1; x++)
                            h5tools_str_append(str, "%s", OPT(info->line_indent, ""));
                    } /* end if */
#endif                /* LATER */

                    ctx->indent_level++;

                    /* Dump the array element */
                    h5tools_str_sprint(str, info, container, memb,
                                       ((char *)(((hvl_t *)((void *)cp_vp))->p)) + i * size, ctx);

                    ctx->indent_level--;
                } /* end for */

                h5tools_str_append(str, "%s", OPT(info->vlen_suf, ")"));
                H5Tclose(memb);
            } break;

            case H5T_TIME:
            case H5T_BITFIELD:
            case H5T_OPAQUE: {
                /* All other types get printed as hexadecimal */
                size_t i;

                H5TOOLS_DEBUG("OTHER");
                if (1 == nsize)
                    h5tools_str_append(str, "0x%02x", ucp_vp[0]);
                else
                    for (i = 0; i < nsize; i++)
                        h5tools_str_append(str, "%s%02x", i ? ":" : "", ucp_vp[i]);
            } break;

            case H5T_NO_CLASS:
            case H5T_NCLASSES:
            default:
                h5tools_str_append(str, "invalid datatype");
                break;
        } /* end switch */
    }

    ret_value = h5tools_str_fmt(str, start, OPT(info->elmt_fmt, "%s"));

    H5TOOLS_ENDDEBUG(" with %s", ret_value);
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_sprint_reference
 *
 * Purpose: Object reference -- show the name of the referenced object.
 *
 * Return:  Nothing
 *-------------------------------------------------------------------------
 */
void
h5tools_str_sprint_reference(h5tools_str_t *str, H5R_ref_t *ref_vp)
{
    ssize_t buf_size;

    H5TOOLS_START_DEBUG("");

    h5tools_str_append(str, " \"");
    buf_size = H5Rget_file_name(ref_vp, NULL, 0);
    H5TOOLS_DEBUG("buf_size=%ld", buf_size);
    if (buf_size) {
        char *file_name = (char *)HDmalloc(sizeof(char) * (size_t)buf_size + 1);
        if (H5Rget_file_name(ref_vp, file_name, (size_t)buf_size + 1) >= 0) {
            file_name[buf_size] = '\0';
            H5TOOLS_DEBUG("name=%s", file_name);
            h5tools_str_append(str, "%s", file_name);
        }
        HDfree(file_name);
    }

    buf_size = H5Rget_obj_name(ref_vp, H5P_DEFAULT, NULL, 0);
    H5TOOLS_DEBUG("buf_size=%ld", buf_size);
    if (buf_size) {
        char *obj_name = (char *)HDmalloc(sizeof(char) * (size_t)buf_size + 1);
        if (H5Rget_obj_name(ref_vp, H5P_DEFAULT, obj_name, (size_t)buf_size + 1) >= 0) {
            obj_name[buf_size] = '\0';
            H5TOOLS_DEBUG("name=%s", obj_name);
            h5tools_str_append(str, "%s", obj_name);
        }
        HDfree(obj_name);
    }

    if (H5Rget_type(ref_vp) == H5R_ATTR) {
        buf_size = H5Rget_attr_name(ref_vp, NULL, 0);
        H5TOOLS_DEBUG("buf_size=%ld", buf_size);
        if (buf_size) {
            char *attr_name = (char *)HDmalloc(sizeof(char) * (size_t)buf_size + 1);
            if (H5Rget_attr_name(ref_vp, attr_name, (size_t)buf_size + 1) >= 0) {
                attr_name[buf_size] = '\0';
                H5TOOLS_DEBUG("name=%s", attr_name);
                h5tools_str_append(str, "/%s", attr_name);
            }
            HDfree(attr_name);
        }
    }
    h5tools_str_append(str, "\"");

    H5TOOLS_ENDDEBUG("");
}

/*-------------------------------------------------------------------------
 * Function: h5tools_escape
 *
 * Purpose:  Changes all "funny" characters in S into standard C escape
 *           sequences.
 *
 * Return:   Success:    S
 *
 *           Failure:    NULL if the buffer would overflow. The
 *              buffer has as many left-to-right escapes as
 *              possible before overflow would have happened.
 *
 * Programmer:  Robb Matzke
 *              Monday, April 26, 1999
 *-------------------------------------------------------------------------
 */
static char *
h5tools_escape(char *s /*in,out*/, size_t size)
{
    register size_t i;
    const char *    escape;
    char            octal[8];
    size_t          n = HDstrlen(s);

    for (i = 0; i < n; i++) {
        switch (s[i]) {
            case '\'':
                escape = "\\\'";
                break;
            case '\"':
                escape = "\\\"";
                break;
            case '\\':
                escape = "\\\\";
                break;
            case '\?':
                escape = "\\\?";
                break;
            case '\a':
                escape = "\\a";
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
            case '\v':
                escape = "\\v";
                break;
            default:
                if (!isprint(s[i])) {
                    HDsnprintf(octal, sizeof(octal), "\\%03o", (unsigned char)s[i]);
                    escape = octal;
                }
                else
                    escape = NULL;

                break;
        }

        if (escape) {
            size_t esc_size = HDstrlen(escape);

            if (n + esc_size + 1 > size)
                /*would overflow*/
                return NULL;

            HDmemmove(s + i + esc_size, s + i + 1, n - i); /*make room*/
            HDmemcpy(s + i, escape, esc_size);             /*insert*/
            n += esc_size - 1;                             /* adjust total string size */
            i += esc_size;                                 /* adjust string position */
        }
    }

    return s;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_is_zero
 *
 * Purpose:     Determines if memory is initialized to all zero bytes.
 *
 * Return:      TRUE if all bytes are zero; FALSE otherwise
 *
 * Programmer:  Robb Matzke
 *              Monday, June  7, 1999
 *-------------------------------------------------------------------------
 */
static hbool_t
h5tools_str_is_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char *)_mem;

    while (size-- > 0)
        if (mem[size])
            return FALSE;

    return TRUE;
}

/*-------------------------------------------------------------------------
 * Function:    h5tools_str_replace
 *
 * Purpose:     replace all occurrences of substring.
 *
 * Return:      char *
 *
 * Programmer:  Peter Cao
 *              March 8, 2012
 *
 * Notes:
 *   Applications need to call free() to free the memoery allocated for
 *   the return string
 *
 *-------------------------------------------------------------------------
 */
char *
h5tools_str_replace(const char *string, const char *substr, const char *replacement)
{
    char *tok    = NULL;
    char *newstr = NULL;
    char *head   = NULL;

    if (substr == NULL || replacement == NULL)
        return HDstrdup(string);
    newstr = HDstrdup(string);
    head   = newstr;
    while ((tok = HDstrstr(head, substr))) {
        char *oldstr;

        oldstr = newstr;
        newstr = (char *)HDmalloc(HDstrlen(oldstr) - HDstrlen(substr) + HDstrlen(replacement) + 1);

        if (newstr == NULL) {
            HDfree(oldstr);
            return NULL;
        }
        HDmemcpy(newstr, oldstr, (size_t)(tok - oldstr));
        HDmemcpy(newstr + (tok - oldstr), replacement, HDstrlen(replacement));
        HDmemcpy(newstr + (tok - oldstr) + HDstrlen(replacement), tok + HDstrlen(substr),
                 HDstrlen(oldstr) - HDstrlen(substr) - (size_t)(tok - oldstr));
        HDmemset(newstr + HDstrlen(oldstr) - HDstrlen(substr) + HDstrlen(replacement), 0, 1);
        /* move back head right after the last replacement */
        head = newstr + (tok - oldstr) + HDstrlen(replacement);
        HDfree(oldstr);
    }

    return newstr;
}
