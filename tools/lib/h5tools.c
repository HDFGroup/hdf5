/*
 * Copyright (c) 1998-2001 National Center for Supercomputing Applications
 *                         All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 23, 1998
 *
 * Purpose:	A library for displaying the values of a dataset in a human
 *		readable format.
 */

#include <stdio.h>
#include <stdlib.h>

#include "h5tools.h"
#include "h5tools_str.h"
#include "h5tools_utils.h"
#include "hdf5.h"
#include "H5private.h"

/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed. This constant sets the limit on the
 * size of that temporary buffer in bytes. For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */
#if 1
#define H5TOOLS_BUFSIZE         (1024 * 1024)
#else
#define H5TOOLS_BUFSIZE         (1024)
#endif

#define ALIGN(A,Z)		((((A) + (Z) - 1) / (Z)) * (Z))

#define START_OF_DATA		0x0001
#define END_OF_DATA		0x0002

/* global variables */
int         indent;
int         compound_data;
FILE       *rawdatastream;	/* should initialize to stdout but gcc moans about it */

/* module-scoped variables */
static int  h5tools_init_g;     /* if h5tools lib has been initialized */

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
 * Function:    h5tools_fopen
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
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_fopen(const char *fname, char *drivername, size_t drivername_size)
{
    static struct {
        const char     *name;
        hid_t		fapl;
    } driver[16];
    static int          ndrivers = 0;
    hid_t               fid = -1;
#ifndef VERSION12
    hid_t               fapl = H5P_DEFAULT;
#endif  /* !VERSION12 */
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

/*-------------------------------------------------------------------------
 * Function:	h5tools_ncols
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
h5tools_ncols(const char *s)
{
    register size_t i;
    
    for (i = 0; *s; s++)
        if (*s >= ' ')
            i++;

    return i;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_simple_prefix
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
h5tools_simple_prefix(FILE *stream, const h5dump_t *info,
                      h5tools_context_t *ctx, hsize_t elmtno, int secnum)
{
    h5tools_str_t prefix;
    size_t templength = 0;
    int i, indentlevel = 0;
	
    if (!ctx->need_prefix)
	return;
 
    memset(&prefix, 0, sizeof(h5tools_str_t));

    /* Terminate previous line, if any */
    if (ctx->cur_column) {
	fputs(OPT(info->line_suf, ""), stream);
        putc('\n', stream);
	fputs(OPT(info->line_sep, ""), stream);
    }

    /* Calculate new prefix */
    h5tools_str_prefix(&prefix, info, elmtno, ctx->ndims, ctx->p_min_idx,
                       ctx->p_max_idx);

    /* Write new prefix to output */
    if (ctx->indent_level >= 0) {
        indentlevel = ctx->indent_level;
    } else {
        /*
         * this is because sometimes we dont print out all the header
         * info for the data(like the tattr-2.ddl example. if that happens
         * the ctx->indent_level a negative so we need to skip the above
         * and just print out the default indent levels.
         */
	indentlevel = ctx->default_indent_level;
    }

    if (elmtno == 0 && secnum == 0 && info->line_1st)
        fputs(h5tools_str_fmt(&prefix, 0, info->line_1st), stream);
    else if (secnum && info->line_cont)
        fputs(h5tools_str_fmt(&prefix, 0, info->line_cont), stream);
    else
        fputs(h5tools_str_fmt(&prefix, 0, info->line_pre), stream);

    templength = h5tools_str_len(&prefix);

    for (i = 0; i < indentlevel; i++){
        fputs(h5tools_str_fmt(&prefix, 0, info->line_indent), stream);
        templength += h5tools_str_len(&prefix);
    }

    ctx->cur_column = ctx->prev_prefix_len = templength;
    ctx->cur_elmt = 0;
    ctx->need_prefix = 0;

    /* Free string */
    h5tools_str_close(&prefix);
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_dump_simple_data
 *
 * Purpose:	Prints some (NELMTS) data elements to output STREAM. The
 *		elements are stored in _MEM as type TYPE and are printed
 *		according to the format described in INFO. The CTX struct
 *		contains context information shared between calls to this
 *		function. The FLAGS is a bit field that indicates whether
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
h5tools_dump_simple_data(FILE *stream, const h5dump_t *info, hid_t container,
                         h5tools_context_t *ctx/*in,out*/, unsigned flags,
                         hsize_t nelmts, hid_t type, void *_mem)
{
    unsigned char	*mem = (unsigned char*)_mem;
    hsize_t		i;		/*element counter		*/
    char		*s, *section;	/*a section of output		*/
    int			secnum;		/*section sequence number	*/
    size_t		size;		/*size of each datum		*/
    size_t		ncols = 80;	/*available output width	*/
    h5tools_str_t	buffer;		/*string into which to render	*/
    int			multiline;	/*datum was multiline		*/
    int                 elmt_counter = 0;/*counts the # elements printed.
                                          *I (ptl?) needed something that
                                          *isn't going to get reset when a new
                                          *line is formed. I'm going to use
                                          *this var to count elements and
                                          *break after we see a number equal
                                          *to the ctx->size_last_dim.   */

    /* Setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));
    size = H5Tget_size(type);

    if (info->line_ncols > 0)
	ncols = info->line_ncols;

    h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

    for (i = 0; i < nelmts; i++, ctx->cur_elmt++, elmt_counter++) {
        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_sprint(&buffer, info, container, type, mem + i * size, ctx);

        if (i + 1 < nelmts || 0 == (flags & END_OF_DATA))
            h5tools_str_append(&buffer, "%s", OPT(info->elmt_suf1, ","));

        s = h5tools_str_fmt(&buffer, 0, "%s");

        /*
         * If the element would split on multiple lines if printed at our
         * current location...
         */
        if (info->line_multi_new == 1 &&
                (ctx->cur_column + h5tools_ncols(s) +
                 strlen(OPT(info->elmt_suf2, " ")) +
                 strlen(OPT(info->line_suf, ""))) > ncols) {
            if (ctx->prev_multiline) {
                /*
                 * ... and the previous element also occupied more than one
                 * line, then start this element at the beginning of a line.
                 */
                ctx->need_prefix = TRUE;
            } else if ((ctx->prev_prefix_len + h5tools_ncols(s) +
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
                (ctx->cur_column + h5tools_ncols(s) +
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

                h5tools_simple_prefix(stream, info, ctx, i, secnum);
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

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_dump_simple_subset
 *
 * Purpose:
 *
 * Return:	Success:    SUCCEED
 *
 *		Failure:    FAIL
 *
 * Programmer:	Bill Wendling
 *              Wednesday, 07. March 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */ 
static herr_t
h5tools_dump_simple_subset(FILE *stream, const h5dump_t *info, hid_t dset,
                           hid_t p_type, struct subset_t *sset,
                           int indentlevel)
{
    herr_t              ret;                    /*the value to return   */
    hid_t		f_space;		/*file data space	*/
    hsize_t		elmtno, i;		/*counters		*/
    int			carry;			/*counter carry value	*/
    hssize_t		zero[8];		/*vector of zeros	*/
    unsigned int	flags;			/*buffer extent flags	*/
    hsize_t		total_size[H5S_MAX_RANK];/*total size of dataset*/

    /* Print info */
    h5tools_context_t	ctx;			/*print context		*/
    size_t		p_type_nbytes;		/*size of memory type	*/
    hsize_t		p_nelmts;		/*total selected elmts	*/

    /* Stripmine info */
    hsize_t		sm_size[H5S_MAX_RANK];	/*stripmine size	*/
    hsize_t		sm_nbytes;		/*bytes per stripmine	*/
    hsize_t		sm_nelmts;		/*elements per stripmine*/
    unsigned char      *sm_buf = NULL;		/*buffer for raw data	*/
    hid_t		sm_space;		/*stripmine data space	*/

    /* Hyperslab info */
    hssize_t		hs_offset[H5S_MAX_RANK];/*starting offset	*/
    hsize_t		hs_size[H5S_MAX_RANK];	/*size this pass	*/
    hsize_t		hs_nelmts;		/*elements in request	*/

    ret = FAIL;     /* be pessimistic */
    f_space = H5Dget_space(dset);

    if (f_space == FAIL)
        goto done;

    /*
     * check that everything looks okay. the dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = indentlevel;
    ctx.need_prefix = 1;
    ctx.ndims = H5Sget_simple_extent_ndims(f_space);

    if ((size_t)ctx.ndims > NELMTS(sm_size))
        goto done_close;

    /* assume entire data space to be printed */
    if (ctx.ndims > 0)
        for (i = 0; i < (hsize_t)ctx.ndims; i++)
            ctx.p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(f_space, total_size, NULL);
    ctx.size_last_dim = total_size[ctx.ndims - 1];

    /* calculate the potential number of elements we're going to print */
    p_nelmts = 1;

    if (ctx.ndims > 0)
        for (i = 0; i < (hsize_t)ctx.ndims; i++)
            p_nelmts *= total_size[i];
 
    if (p_nelmts == 0) {
        /* nothing to print */
        ret = SUCCEED;
        goto done_close;
    }

    /*
     * determine the strip mine size and allocate a buffer. the strip mine is
     * a hyperslab whose size is manageable.
     */
    sm_nbytes = p_type_nbytes = H5Tget_size(p_type);

    if (ctx.ndims > 0)
        for (i = ctx.ndims; i > 0; --i) {
            sm_size[i - 1] = MIN(total_size[i - 1], H5TOOLS_BUFSIZE / sm_nbytes);
            sm_nbytes *= sm_size[i - 1];
            assert(sm_nbytes > 0);
        }

    assert(sm_nbytes == (hsize_t)((size_t)sm_nbytes)); /*check for overflow*/
    sm_buf = malloc((size_t)sm_nbytes);
    sm_nelmts = sm_nbytes / p_type_nbytes;
    sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

    /* the stripmine loop */
    memset(hs_offset, 0, sizeof hs_offset);
    memset(zero, 0, sizeof zero);

    ret = SUCCEED;

done_close:
    H5Sclose(f_space);
done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_dump_simple_dset
 *
 * Purpose:	Print some values from a dataset with a simple data space.
 *		This is a special case of h5tools_dump_dset(). This function
 *		only intended for dumping datasets -- it does strip mining and
 *		some other things which are unnecessary for smaller objects
 *		such as attributes (to print small objects like attributes
 *		simply read the attribute and call h5tools_dump_simple_mem()).
 *
 * Return:	Success:    SUCCEED
 *
 *		Failure:    FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */ 
static int
h5tools_dump_simple_dset(FILE *stream, const h5dump_t *info, hid_t dset,
                         hid_t p_type, int indentlevel)
{
    hid_t		f_space;		/*file data space	*/
    hsize_t		elmtno, i;		/*counters		*/
    int			carry;			/*counter carry value	*/
    hssize_t		zero[8];		/*vector of zeros	*/
    unsigned int	flags;			/*buffer extent flags	*/
    hsize_t		total_size[H5S_MAX_RANK];/*total size of dataset*/

    /* Print info */
    h5tools_context_t	ctx;			/*print context		*/
    size_t		p_type_nbytes;		/*size of memory type	*/
    hsize_t		p_nelmts;		/*total selected elmts	*/

    /* Stripmine info */
    hsize_t		sm_size[H5S_MAX_RANK];	/*stripmine size	*/
    hsize_t		sm_nbytes;		/*bytes per stripmine	*/
    hsize_t		sm_nelmts;		/*elements per stripmine*/
    unsigned char      *sm_buf = NULL;		/*buffer for raw data	*/
    hid_t		sm_space;		/*stripmine data space	*/

    /* Hyperslab info */
    hssize_t		hs_offset[H5S_MAX_RANK];/*starting offset	*/
    hsize_t		hs_size[H5S_MAX_RANK];	/*size this pass	*/
    hsize_t		hs_nelmts;		/*elements in request	*/

    /* VL data special information */
    unsigned int        vl_data = 0;            /*contains VL datatypes */

    f_space = H5Dget_space(dset);

    if (f_space == FAIL)
        return FAIL;

    /*
     * Check that everything looks okay. The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = indentlevel;
    ctx.need_prefix = 1;
    ctx.ndims = H5Sget_simple_extent_ndims(f_space);

    if ((size_t)ctx.ndims > NELMTS(sm_size)) {
        H5Sclose(f_space);
        return FAIL;
    }

    /* Assume entire data space to be printed */
    if (ctx.ndims > 0)
        for (i = 0; i < (hsize_t)ctx.ndims; i++)
            ctx.p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(f_space, total_size, NULL);
    ctx.size_last_dim = total_size[ctx.ndims - 1];

    /* calculate the number of elements we're going to print */
    p_nelmts = 1;

    if (ctx.ndims > 0)
        for (i = 0, p_nelmts = 1; i < (hsize_t)ctx.ndims; i++)
            p_nelmts *= total_size[i];
 
    if (p_nelmts == 0) {
        /* nothing to print */
        H5Sclose(f_space);
        return SUCCEED;
    }

    /* Check if we have VL data in the dataset's datatype */
    if (H5Tdetect_class(p_type, H5T_VLEN) == TRUE)
        vl_data = TRUE;

    /*
     * Determine the strip mine size and allocate a buffer. The strip mine is
     * a hyperslab whose size is manageable.
     */
    sm_nbytes = p_type_nbytes = H5Tget_size(p_type);

    if (ctx.ndims > 0) {
        for (i = ctx.ndims; i > 0; --i) {
            sm_size[i - 1] = MIN(total_size[i - 1], H5TOOLS_BUFSIZE / sm_nbytes);
            sm_nbytes *= sm_size[i - 1];
            assert(sm_nbytes > 0);
        }
    }

    assert(sm_nbytes == (hsize_t)((size_t)sm_nbytes)); /*check for overflow*/
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
            return FAIL;
        }

        /* Print the data */
        flags = (elmtno == 0) ? START_OF_DATA : 0;
        flags |= ((elmtno + hs_nelmts) >= p_nelmts) ? END_OF_DATA : 0;
        h5tools_dump_simple_data(stream, info, dset, &ctx, flags, hs_nelmts,
                                 p_type, sm_buf);

        /* Reclaim any VL memory, if necessary */
        if (vl_data)
            H5Dvlen_reclaim(p_type, sm_space, H5P_DEFAULT, sm_buf);

        /* Calculate the next hyperslab offset */
        for (i = ctx.ndims, carry = 1; i > 0 && carry; --i) {
            ctx.p_min_idx[i - 1] = ctx.p_max_idx[i - 1];
            hs_offset[i - 1] += hs_size[i - 1];

            if (hs_offset[i - 1] == (hssize_t)total_size[i - 1])
                hs_offset[i - 1] = 0;
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
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_dump_simple_mem
 *
 * Purpose:	Print some values from memory with a simple data space.
 *		This is a special case of h5tools_dump_mem().
 *
 * Return:	Success:    SUCCEED
 *
 *		Failure:    FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
h5tools_dump_simple_mem(FILE *stream, const h5dump_t *info, hid_t obj_id,
                        hid_t type, hid_t space, void *mem, int indentlevel)
{
    hsize_t		i;			/*counters		*/
    hsize_t		nelmts;			/*total selected elmts	*/
    h5tools_context_t	ctx;			/*printing context	*/

    /*
     * Check that everything looks okay.  The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    memset(&ctx, 0, sizeof(ctx));
    ctx.ndims = H5Sget_simple_extent_ndims(space);

    if ((size_t)ctx.ndims > NELMTS(ctx.p_min_idx))
        return FAIL;

    ctx.indent_level = indentlevel;
    ctx.need_prefix = 1;

    /* Assume entire data space to be printed */
    for (i = 0; i < (hsize_t)ctx.ndims; i++)
        ctx.p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(space, ctx.p_max_idx, NULL);

    for (i = 0, nelmts = 1; ctx.ndims != 0 && i < (hsize_t)ctx.ndims; i++)
        nelmts *= ctx.p_max_idx[i] - ctx.p_min_idx[i];
 
    if (nelmts == 0)
        return SUCCEED; /*nothing to print*/

    ctx.size_last_dim = ctx.p_max_idx[ctx.ndims - 1];

    /* Print it */
    h5tools_dump_simple_data(stream, info, obj_id, &ctx,
                             START_OF_DATA | END_OF_DATA, nelmts, type, mem);

    /* Terminate the output */
    if (ctx.cur_column) {
        fputs(OPT(info->line_suf, ""), stream);
        putc('\n', stream);
        fputs(OPT(info->line_sep, ""), stream);
    }
 
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_fixtype
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
h5tools_fixtype(hid_t f_type)
{
    hid_t   m_type = FAIL, f_memb;
    hid_t  *memb = NULL;
    char  **name = NULL;
    int     nmembs = 0, i;
    int     ndims;
    hsize_t dim[H5S_MAX_RANK];
    size_t  size, offset;
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
        assert(nmembs > 0);
	memb = calloc((size_t)nmembs, sizeof(hid_t));
	name = calloc((size_t)nmembs, sizeof(char *));
	
	for (i = 0, size = 0; i < nmembs; i++) {
	    /* Get the member type and fix it */
	    f_memb = H5Tget_member_type(f_type, i);
	    memb[i] = h5tools_fixtype(f_memb);
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
        array_base = h5tools_fixtype(f_memb);

        /* Copy the array */
        m_type = H5Tarray_create(array_base, ndims, dim, NULL);

        /* Close the temporary datatypes */
        H5Tclose(array_base);
        H5Tclose(f_memb);
        break;

    case H5T_VLEN:
        /* Get the VL sequence's base type and convert it to the printable version */
        f_memb = H5Tget_super(f_type);
        array_base = h5tools_fixtype(f_memb);

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
        register int j;

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
 * Function:	h5tools_dump_dset
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
 *		h5tools_dump_mem().
 *
 * Return:	Success:    SUCCEED
 *
 *		Failure:    FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-06-07
 *		If info->raw is set then the memory datatype will be the same
 *		as the file datatype.
 *
 *		Bill Wendling, 2001-02-27
 *		Renamed to ``h5tools_dump_dset'' and added the subsetting
 *		parameter.
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_dset(FILE *stream, const h5dump_t *info, hid_t dset, hid_t _p_type,
                  struct subset_t *sset, int indentlevel)
{
    hid_t     f_space;
    hid_t     p_type = _p_type;
    hid_t     f_type;
    int       status = FAIL;
    h5dump_t  info_dflt;

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
            p_type = h5tools_fixtype(f_type);

        H5Tclose(f_type);

        if (p_type < 0)
            goto done;
    }

    /* Check the data space */
    f_space = H5Dget_space(dset);

    /* Print the data */
    if (H5Sis_simple(f_space) > 0) {
        if (!sset)
            status = h5tools_dump_simple_dset(rawdatastream, info, dset, p_type,
                                              indentlevel);
        else
            status = h5tools_dump_simple_subset(rawdatastream, info, dset, p_type,
                                                sset, indentlevel);
    }

    /* Close the dataspace */
    H5Sclose(f_space);

done:
    if (p_type != _p_type)
        H5Tclose(p_type);

    return status;
}

/*-------------------------------------------------------------------------
 * Function:	h5tools_dump_mem
 *
 * Purpose:	Displays the data contained in MEM. MEM must have the
 *		specified data TYPE and SPACE.  Currently only simple data
 *		spaces are allowed and only the `all' selection.
 *
 * Return:	Success:    SUCCEED
 *
 *		Failure:    FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_mem(FILE *stream, const h5dump_t *info, hid_t obj_id, hid_t type,
                 hid_t space, void *mem, int indentlevel)
{
    h5dump_t    info_dflt;
    
    /* Use default values */
    if (!stream)
	stream = stdout;

    if (!info) {
	memset(&info_dflt, 0, sizeof(info_dflt));
	info = &info_dflt;
    }

    /* Check the data space */
    if (H5Sis_simple(space) <= 0)
	return -1;

    return h5tools_dump_simple_mem(stream, info, obj_id, type, space, mem,
                                   indentlevel);
}
