/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 23, 1998
 *
 * Purpose: A library for displaying the values of a dataset in a human
 *  readable format.
 */

#include <stdio.h>
#include <stdlib.h>

#include "h5tools.h"
#include "h5tools_dump.h"
#include "h5tools_ref.h"
#include "h5tools_utils.h"
#include "H5private.h"

h5tool_format_t h5tools_dataformat = {
0, /*raw */

"", /*fmt_raw */
"%d", /*fmt_int */
"%u", /*fmt_uint */
"%hhd", /*fmt_schar */
"%u", /*fmt_uchar */
"%d", /*fmt_short */
"%u", /*fmt_ushort */
"%ld", /*fmt_long */
"%lu", /*fmt_ulong */
NULL, /*fmt_llong */
NULL, /*fmt_ullong */
"%g", /*fmt_double */
"%g", /*fmt_float */

0, /*ascii */
0, /*str_locale */
0, /*str_repeat */

"[ ", /*arr_pre */
",", /*arr_sep */
" ]", /*arr_suf */
1, /*arr_linebreak */

"", /*cmpd_name */
",\n", /*cmpd_sep */
"{", /*cmpd_pre */
"}", /*cmpd_suf */
"\n", /*cmpd_end */

", ", /*vlen_sep */
"(", /*vlen_pre */
")", /*vlen_suf */
"", /*vlen_end */

"%s", /*elmt_fmt */
",", /*elmt_suf1 */
" ", /*elmt_suf2 */

"", /*idx_n_fmt */
"", /*idx_sep */
"", /*idx_fmt */

80, /*line_ncols *//*standard default columns */
0, /*line_per_line */
"", /*line_pre */
"%s", /*line_1st */
"%s", /*line_cont */
"", /*line_suf */
"", /*line_sep */
1, /*line_multi_new */
"   ", /*line_indent */

1, /*skip_first */

1, /*obj_hidefileno */
" "H5_PRINTF_HADDR_FMT, /*obj_format */

1, /*dset_hidefileno */
"DATASET %s ", /*dset_format */
"%s", /*dset_blockformat_pre */
"%s", /*dset_ptformat_pre */
"%s", /*dset_ptformat */
1, /*array indices */
1 /*escape non printable characters */
};

const h5tools_dump_header_t h5tools_standardformat = {
"standardformat", /*name */
"HDF5", /*fileebgin */
"", /*fileend */
SUPER_BLOCK, /*bootblockbegin */
"", /*bootblockend */
H5_TOOLS_GROUP, /*groupbegin */
"", /*groupend */
H5_TOOLS_DATASET, /*datasetbegin */
"", /*datasetend */
ATTRIBUTE, /*attributebegin */
"", /*attributeend */
H5_TOOLS_DATATYPE, /*datatypebegin */
"", /*datatypeend */
DATASPACE, /*dataspacebegin */
"", /*dataspaceend */
DATA, /*databegin */
"", /*dataend */
SOFTLINK, /*softlinkbegin */
"", /*softlinkend */
EXTLINK, /*extlinkbegin */
"", /*extlinkend */
UDLINK, /*udlinkbegin */
"", /*udlinkend */
SUBSET, /*subsettingbegin */
"", /*subsettingend */
START, /*startbegin */
"", /*startend */
STRIDE, /*stridebegin */
"", /*strideend */
COUNT, /*countbegin */
"", /*countend */
BLOCK, /*blockbegin */
"", /*blockend */

"{", /*fileblockbegin */
"}", /*fileblockend */
"{", /*bootblockblockbegin */
"}", /*bootblockblockend */
"{", /*groupblockbegin */
"}", /*groupblockend */
"{", /*datasetblockbegin */
"}", /*datasetblockend */
"{", /*attributeblockbegin */
"}", /*attributeblockend */
"", /*datatypeblockbegin */
"", /*datatypeblockend */
"", /*dataspaceblockbegin */
"", /*dataspaceblockend */
"{", /*datablockbegin */
"}", /*datablockend */
"{", /*softlinkblockbegin */
"}", /*softlinkblockend */
"{", /*extlinkblockbegin */
"}", /*extlinkblockend */
"{", /*udlinkblockbegin */
"}", /*udlinkblockend */
"{", /*strblockbegin */
"}", /*strblockend */
"{", /*enumblockbegin */
"}", /*enumblockend */
"{", /*structblockbegin */
"}", /*structblockend */
"{", /*vlenblockbegin */
"}", /*vlenblockend */
"{", /*subsettingblockbegin */
"}", /*subsettingblockend */
"(", /*startblockbegin */
");", /*startblockend */
"(", /*strideblockbegin */
");", /*strideblockend */
"(", /*countblockbegin */
");", /*countblockend */
"(", /*blockblockbegin */
");", /*blockblockend */

"", /*dataspacedescriptionbegin */
"", /*dataspacedescriptionend */
"(", /*dataspacedimbegin */
")", /*dataspacedimend */
};

const h5tools_dump_header_t* h5tools_dump_header_format;
table_t *h5dump_type_table = NULL;  /*type table reference for datatype dump  */

/* local prototypes */
static int h5tools_print_region_data_blocks(hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t *cur_ctx,
        h5tools_str_t *buffer/*string into which to render */, size_t ncols,
        unsigned ndims, hid_t type_id, hsize_t nblocks, hsize_t *ptdata);

hbool_t h5tools_dump_region_data_points(hid_t region_space, hid_t region_id,
                FILE *stream, const h5tool_format_t *info,
                h5tools_context_t *ctx/*in,out*/,
                h5tools_str_t *buffer/*string into which to render */,
                hsize_t *curr_pos/*total data element position*/,
                size_t ncols, hsize_t region_elmt_counter/*element counter*/,
                hsize_t elmt_counter);

int h5tools_print_region_data_points(hid_t region_space, hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t *cur_ctx,
        h5tools_str_t *buffer, size_t ncols,
        int ndims, hid_t type_id, hssize_t npoints, hsize_t *ptdata);

hbool_t h5tools_dump_region_data_blocks(hid_t region_space, hid_t region_id,
                FILE *stream, const h5tool_format_t *info,
                h5tools_context_t *ctx/*in,out*/,
                h5tools_str_t *buffer/*string into which to render */,
                hsize_t *curr_pos/*total data element position*/,
                size_t ncols, hsize_t region_elmt_counter/*element counter*/,
                hsize_t elmt_counter);

void h5tools_print_dims(h5tools_str_t *buffer, hsize_t *s, int dims);

void h5tools_dump_subsetting_header(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, struct subset_t *sset, int dims);

void
h5tools_dump_init(void)
{
    h5tools_dump_header_format = &h5tools_standardformat;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Prints NELMTS data elements to output STREAM.
 * Description:
 *      Prints some (NELMTS) data elements to output STREAM. The elements are
 *      stored in _MEM as type TYPE and are printed according to the format
 *      described in INFO. The CTX struct contains context information shared
 *      between calls to this function. The FLAGS is a bit field that
 *      indicates whether the data supplied in this call falls at the
 *      beginning or end of the total data to be printed (START_OF_DATA and
 *      END_OF_DATA).
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 * Programmer:
 *      Robb Matzke, Monday, April 26, 1999
 * Modifications:
 *  Robb Matzke, 1999-06-04
 * The `container' argument is the optional dataset for reference types.
 *
 *  Robb Matzke, 1999-09-29
 * Understands the `per_line' property which indicates that every Nth
 * element should begin a new line.
 *
 *      Robb Matzke, LLNL, 2003-06-05
 *      Do not dereference the memory for a variable-length string here.
 *      Deref in h5tools_str_sprint() instead so recursive types are
 *      handled correctly.
 *
 *      Pedro Vicente Nunes, The HDF Group, 2005-10-19
 *        pass to the prefix in h5tools_simple_prefix the total position
 *        instead of the current stripmine position i; this is necessary
 *        to print the array indices
 *        new field sm_pos in h5tools_context_t, the current stripmine element position
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_simple_data(FILE *stream, const h5tool_format_t *info, hid_t container,
                         h5tools_context_t *ctx/*in,out*/, unsigned flags,
                         hsize_t nelmts, hid_t type, void *_mem)
{
    int            ret_value = 0; /*no need to LEAVE() on ERROR: HERR_INIT(int, SUCCEED) */
    unsigned char *mem = (unsigned char*) _mem;
    hsize_t        i;         /*element counter  */
    size_t         size;      /*size of each datum  */
    hid_t          region_space = -1;
    hid_t          region_id = -1;
    hbool_t        dimension_break = TRUE;
    H5S_sel_type   region_type;
    size_t         ncols = 80; /*available output width */
    h5tools_str_t  buffer;    /*string into which to render */
    hsize_t        curr_pos = 0;  /* total data element position   */
    hsize_t        elmt_counter = 0;/*counts the # elements printed.
                                     *I (ptl?) needed something that
                                     *isn't going to get reset when a new
                                     *line is formed. I'm going to use
                                     *this var to count elements and
                                     *break after we see a number equal
                                     *to the ctx->size_last_dim.   */

    /* binary dump */
    if (bin_output && (rawdatastream != NULL)) {
        if (render_bin_output(rawdatastream, container, type, _mem, nelmts) < 0) {
            PRINTVALSTREAM(rawoutstream, "\nError in writing binary stream\n");
        }
    } /* end if */
    else {
        /* setup */
        HDmemset(&buffer, 0, sizeof(h5tools_str_t));
        size = H5Tget_size(type);

        if (info->line_ncols > 0)
            ncols = info->line_ncols;

        /* pass to the prefix in h5tools_simple_prefix the total position
         * instead of the current stripmine position i; this is necessary
         * to print the array indices
         */
        curr_pos = ctx->sm_pos;

        if (region_output && (size == H5R_DSET_REG_REF_BUF_SIZE)) {
            for (i = 0; i < nelmts; i++, ctx->cur_elmt++, elmt_counter++) {
                void* memref = mem + i * size;
                char ref_name[1024];

                /* region data */
                region_id = H5Rdereference(container, H5R_DATASET_REGION, memref);
                if (region_id >= 0) {
                    region_space = H5Rget_region(container, H5R_DATASET_REGION, memref);
                    if (region_space >= 0) {
                        if (h5tools_is_zero(memref, H5Tget_size(type))) {
                            ctx->need_prefix = TRUE;
                            h5tools_simple_prefix(rawoutstream, info, ctx, curr_pos, 0);

                            /* Render the region element begin */
                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, "NULL");

                            dimension_break = h5tools_render_element(rawoutstream, info,
                                       ctx, &buffer, &curr_pos, ncols, i, elmt_counter);
                        }
                        else {
                            if(H5Rget_name(region_id, H5R_DATASET_REGION, memref, (char*) ref_name, (size_t)1024)<0)
                                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Rget_name failed");

                            ctx->need_prefix = TRUE;
                            h5tools_simple_prefix(rawoutstream, info, ctx, curr_pos+i, 0);

                            /* Render the region element begin */
                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, info->dset_format, ref_name);

                            dimension_break = h5tools_render_element(rawoutstream, info,
                                       ctx, &buffer, &curr_pos, ncols, i, elmt_counter);

                            region_type = H5Sget_select_type(region_space);
                            if(region_type == H5S_SEL_POINTS)
                                /* Print point information */
                                dimension_break = h5tools_dump_region_data_points(
                                                       region_space, region_id, rawoutstream, info, ctx,
                                                       &buffer, &curr_pos, ncols, i, elmt_counter);
                            else if(region_type == H5S_SEL_HYPERSLABS)
                                /* Print block information */
                                dimension_break = h5tools_dump_region_data_blocks(
                                                       region_space, region_id, rawoutstream, info, ctx,
                                                       &buffer, &curr_pos, ncols, i, elmt_counter);
                            else
                                HERROR(H5E_tools_g, H5E_tools_min_id_g, "invalid region type");
                            /* Render the region element end */

                        } /* end else to if (h5tools_is_zero(... */
                        if(H5Sclose(region_space) < 0)
                            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
                    } /* end if (region_space >= 0) */
                    else
                        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Rget_region failed");
                    if(H5Dclose(region_id) < 0)
                        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Dclose failed");

                } /* if (region_id >= 0) */
                else {
                    /* if (region_id < 0) - could mean that no reference was written do not throw failure */
                    H5Epush2(H5tools_ERR_STACK_g, __FILE__, FUNC, __LINE__, H5tools_ERR_CLS_g, H5E_tools_g, H5E_tools_min_id_g, "H5Rdereference failed");
                }

                ctx->need_prefix = TRUE;

                if(FALSE == dimension_break)
                    elmt_counter = 0;
            } /* end for (i = 0; i < nelmts... */
        } /* end if (region_output... */
        else {
            for (i = 0; i < nelmts; i++, ctx->cur_elmt++, elmt_counter++) {
                void* memref = mem + i * size;
                /* Render the data element begin*/
                h5tools_str_reset(&buffer);
                h5tools_str_sprint(&buffer, info, container, type, memref, ctx);

                if (i + 1 < nelmts || (flags & END_OF_DATA) == 0)
                    h5tools_str_append(&buffer, "%s", OPT(info->elmt_suf1, ","));

                dimension_break = h5tools_render_element(stream, info, ctx, &buffer,
                                                           &curr_pos, ncols, i, elmt_counter);
                /* Render the data element end*/
                if(FALSE == dimension_break)
                    elmt_counter = 0;
            } /* end for (i = 0; i < nelmts... */
        }

        h5tools_str_close(&buffer);
    }/* else bin */

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type blocks.
 *
 * Return:
 *      The function returns FAIL if there was an error, otherwise SUCEED
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      size_t ncols
 *      int ndims is the number of dimensions of the region element
 *      hssize_t nblocks is the number of blocks in the region
 *-------------------------------------------------------------------------
 */
static int
h5tools_print_region_data_blocks(hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t *cur_ctx,
        h5tools_str_t *buffer/*string into which to render */, size_t ncols,
        unsigned ndims, hid_t type_id, hsize_t nblocks, hsize_t *ptdata)
{
    hbool_t      dimension_break = TRUE;
    hsize_t     *dims1 = NULL;
    hsize_t     *start = NULL;
    hsize_t     *count = NULL;
    hsize_t      blkndx;
    hsize_t      total_size[H5S_MAX_RANK];
    hsize_t      elmtno; /* elemnt index  */
    hsize_t      curr_pos = 0;
    unsigned int region_flags; /* buffer extent flags */
    hsize_t      numelem;
    hsize_t      numindex;
    unsigned     indx;
    unsigned     jndx;
    size_t       type_size;
    int          ret_value = SUCCEED;
    hid_t        mem_space = -1;
    hid_t        sid1 = -1;
    h5tools_context_t ctx;
    void        *region_buf = NULL;

    HDassert(info);
    HDassert(cur_ctx);
    HDassert(buffer);
    HDassert(ptdata);

    HDmemset(&ctx, 0, sizeof(ctx));

    /* Get the dataspace of the dataset */
    if((sid1 = H5Dget_space(region_id)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");

    /* Allocate space for the dimension array */
    if((dims1 = (hsize_t *) HDmalloc((size_t)(sizeof(hsize_t) * ndims))) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for dims");

    /* find the dimensions of each data space from the block coordinates */
    numelem = 1;
    for (jndx = 0; jndx < ndims; jndx++) {
        dims1[jndx] = ptdata[jndx + ndims] - ptdata[jndx] + 1;
        numelem = dims1[jndx] * numelem;
    }

    /* Create dataspace for reading buffer */
    if((mem_space = H5Screate_simple((int)ndims, dims1, NULL)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

    if((type_size = H5Tget_size(type_id)) == 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((region_buf = HDmalloc(type_size * (size_t)numelem)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate region buffer");

    /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
    /*          1   2        n      1   2        n                                       */
    if((start = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for start");

    if((count = (hsize_t *) HDmalloc(sizeof(hsize_t) * ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for count");

    curr_pos = 0;
    ctx.indent_level = cur_ctx->indent_level;
    ctx.cur_column = cur_ctx->cur_column;
    ctx.prev_multiline = cur_ctx->prev_multiline;
    ctx.ndims = ndims;
    for (blkndx = 0; blkndx < nblocks; blkndx++) {
        ctx.need_prefix = TRUE;
        ctx.cur_elmt = 0;
        for (indx = 0; indx < ndims; indx++) {
            start[indx] = ptdata[indx + blkndx * ndims * 2];
            count[indx] = dims1[indx];
        }

        if(H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");

        if(H5Dread(region_id, type_id, mem_space, sid1, H5P_DEFAULT, region_buf) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Dread failed");

        ctx.indent_level++;
        if(H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

        /* assume entire data space to be printed */
        for (indx = 0; indx < (unsigned)ctx.ndims; indx++)
            ctx.p_min_idx[indx] = start[indx];
        init_acc_pos(&ctx, total_size);

        /* print the data */
        region_flags = START_OF_DATA;
        if (blkndx == nblocks - 1)
            region_flags |= END_OF_DATA;

        for (indx = 0; indx < (unsigned)ctx.ndims; indx++)
            ctx.p_max_idx[indx] = dims1[indx];

        curr_pos = 0;
        ctx.sm_pos = blkndx * 2 * ndims;
        ctx.size_last_dim = dims1[ndims-1];

        h5tools_region_simple_prefix(stream, info, &ctx, curr_pos, ptdata, 0);

        elmtno = 0;
        for (numindex = 0; numindex < numelem; numindex++, elmtno++, ctx.cur_elmt++) {
            /* Render the region data element begin */
            h5tools_str_reset(buffer);

            h5tools_str_append(buffer, "%s", numindex ? OPTIONAL_LINE_BREAK "" : "");
            h5tools_str_sprint(buffer, info, region_id, type_id,
                                ((char*)region_buf + numindex * type_size), &ctx);

            if (numindex + 1 < numelem || (region_flags & END_OF_DATA) == 0)
                h5tools_str_append(buffer, "%s", OPT(info->elmt_suf1, ","));

            dimension_break = h5tools_render_region_element(stream, info, &ctx, buffer, &curr_pos,
                                                                    ncols, ptdata, numindex, elmtno);
            /* Render the region data element end */

            if(FALSE == dimension_break)
                elmtno = 0;
        } /* end for (numindex = 0; numindex < numelem; numindex++, elmtno++, ctx.cur_elmt++) */

        ctx.indent_level--;
    } /* end for (blkndx = 0; blkndx < nblocks; blkndx++) */

 done:
    HDfree(start);
    HDfree(count);
    HDfree(region_buf);
    HDfree(dims1);

    if(H5Sclose(mem_space) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
    if(H5Sclose(sid1) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using blocks.
 *
 * Return:
 *      The function returns False if the last dimension has been reached, otherwise True
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      hsize_t *curr_pos
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      hsize_t curr_pos is the total data element position
 *      size_t ncols
 *      hsize_t region_elmt_counter is the region element loop counter
 *      hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_dump_region_data_blocks(hid_t region_space, hid_t region_id,
        FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx/*in,out*/,
        h5tools_str_t *buffer/*string into which to render */,
        hsize_t *curr_pos/*total data element position*/,
        size_t ncols, hsize_t region_elmt_counter/*element counter*/,
        hsize_t elmt_counter)
{
    HERR_INIT(hbool_t, TRUE)
    hbool_t      dimension_break = TRUE;
    hssize_t     snblocks;
    hsize_t      nblocks;
    hsize_t      alloc_size;
    hsize_t     *ptdata = NULL;
    int          sndims;
    unsigned     ndims;
    hid_t        dtype = -1;
    hid_t        type_id = -1;
    hsize_t      u;

    HDassert(info);
    HDassert(ctx);
    HDassert(buffer);

    if((snblocks = H5Sget_select_hyper_nblocks(region_space)) <= 0)
        H5E_THROW(dimension_break, H5E_tools_min_id_g, "H5Sget_select_hyper_nblocks failed");
    nblocks = (hsize_t)snblocks;

    /* Print block information */
    if((sndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5E_THROW(dimension_break, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");
    ndims = (unsigned)sndims;

    /* Render the region { element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, "{");
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region { element end */

    ctx->indent_level++;
    ctx->need_prefix = TRUE;

    /* Render the region datatype info and indices element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "REGION_TYPE BLOCK  ");

    alloc_size = nblocks * ndims * 2 * sizeof(ptdata[0]);
    HDassert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
    if((ptdata = (hsize_t*) HDmalloc((size_t) alloc_size)) == NULL)
{
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "Could not allocate buffer for ptdata");
        HGOTO_DONE(dimension_break);
}

    if(H5Sget_select_hyper_blocklist(region_space, (hsize_t)0, nblocks, ptdata) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Rget_select_hyper_blocklist failed");

    for(u = 0; u < nblocks; u++) {
        unsigned v;

        h5tools_str_append(buffer, info->dset_blockformat_pre,
                            u ? "," OPTIONAL_LINE_BREAK " " : "", (unsigned long)u);

        /* Start coordinates and opposite corner */
        for (v = 0; v < ndims; v++)
            h5tools_str_append(buffer, "%s" HSIZE_T_FORMAT, v ? "," : "(",
                                ptdata[u * 2 * ndims + v]);

        for (v = 0; v < ndims; v++)
            h5tools_str_append(buffer, "%s" HSIZE_T_FORMAT, v ? "," : ")-(",
                                ptdata[u * 2 * ndims + v + ndims]);

        h5tools_str_append(buffer, ")");
    } /* end for (u = 0; u < nblocks; u++) */

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region datatype info and indices element end */

    ctx->need_prefix = TRUE;

    if((dtype = H5Dget_type(region_id)) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Dget_type failed");
    if((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Tget_native_type failed");

    /* Render the datatype element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->datatypebegin,
                        h5tools_dump_header_format->datatypeblockbegin);

    ctx->indent_level++;
    h5tools_print_datatype(stream, buffer, info, ctx, dtype, TRUE);
    ctx->indent_level--;

    if (HDstrlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (HDstrlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeend);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the datatype element end */

    ctx->need_prefix = TRUE;

    /* Render the dataspace element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s ", h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(buffer, region_space);

    if (HDstrlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (HDstrlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->dataspaceend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the dataspace element end */

    ctx->need_prefix = TRUE;

    /* Render the databegin element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->databegin,
                        h5tools_dump_header_format->datablockbegin);
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the databegin element end */

    ctx->need_prefix = TRUE;

    h5tools_print_region_data_blocks(region_id, rawdatastream, info, ctx,
        buffer, ncols, ndims, type_id, nblocks, ptdata);

 done:
    HDfree(ptdata);

    if(H5Tclose(type_id) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    if(H5Tclose(dtype) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    ctx->need_prefix = TRUE;

    /* Render the dataend element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->dataend,
                        h5tools_dump_header_format->datablockend);
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos,
                                                ncols, region_elmt_counter, elmt_counter);
    /* Render the dataend element end */

    ctx->indent_level--;
    ctx->need_prefix = TRUE;

    /* Render the region } element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "}");
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos,
                                                ncols, region_elmt_counter, elmt_counter);
    /* Render the region } element end */

    H5_LEAVE(dimension_break)

 CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print the data values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type points.
 *
 * Return:
 *      The function returns FAIL on error, otherwise SUCCEED
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      size_t ncols
 *      int ndims is the number of dimensions of the region element
 *      hssize_t npoints is the number of points in the region
 *-------------------------------------------------------------------------
 */
int
h5tools_print_region_data_points(hid_t region_space, hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t *cur_ctx,
        h5tools_str_t *buffer, size_t ncols,
        int ndims, hid_t type_id, hssize_t npoints, hsize_t *ptdata)
{
    hbool_t  dimension_break = TRUE;
    hsize_t *dims1 = NULL;
    hsize_t  elmtno; /* elemnt index  */
    hsize_t  curr_pos = 0;
    hsize_t  total_size[H5S_MAX_RANK];
    size_t   jndx;
    unsigned indx;
    int      type_size;
    int      ret_value = SUCCEED;
    unsigned int region_flags; /* buffer extent flags */
    hid_t    mem_space = -1;
    void    *region_buf = NULL;
    h5tools_context_t ctx;

    HDassert(info);
    HDassert(cur_ctx);
    HDassert(buffer);
    HDassert(ptdata);
    HDassert(ndims > 0);

    HDmemset(&ctx, 0, sizeof(ctx));
    /* Allocate space for the dimension array */
    if((dims1 = (hsize_t *) HDmalloc(sizeof(hsize_t) * (size_t)ndims)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for dims");

    dims1[0] = npoints;

    /* Create dataspace for reading buffer */
    if((mem_space = H5Screate_simple(1, dims1, NULL)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

    if((type_size = H5Tget_size(type_id)) == 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

    if((region_buf = HDmalloc(type_size * (size_t)npoints)) == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for region");

    curr_pos = 0;
    ctx.indent_level = cur_ctx->indent_level;
    ctx.cur_column = cur_ctx->cur_column;
    ctx.prev_multiline = cur_ctx->prev_multiline;
    ctx.ndims = ndims;

    if(H5Dread(region_id, type_id, mem_space, region_space, H5P_DEFAULT, region_buf) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dread failed");

    elmtno = 0;
    for (jndx = 0; jndx < npoints; jndx++, elmtno++) {
        ctx.need_prefix = TRUE;
        ctx.cur_elmt = 0;    /* points are always 0 */

        ctx.indent_level++;
        if(H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

        /* assume entire data space to be printed */
        for (indx = 0; indx < (size_t) ctx.ndims; indx++)
            ctx.p_min_idx[indx] = 0;
        init_acc_pos(&ctx, total_size);

        /* print the data */
        region_flags = START_OF_DATA;
        if (jndx == npoints - 1)
            region_flags |= END_OF_DATA;

        for (indx = 0; indx < (size_t)ctx.ndims; indx++)
            ctx.p_max_idx[indx] = cur_ctx->p_max_idx[indx];

        ctx.sm_pos = jndx * ndims;
        if (ctx.ndims > 0) {
            ctx.size_last_dim = (int) (ctx.p_max_idx[ctx.ndims - 1]);
        }
        else
            ctx.size_last_dim = 0;

        curr_pos = 0;    /* points requires constant 0 */
        h5tools_region_simple_prefix(stream, info, &ctx, curr_pos, ptdata, 0);

        /* Render the point element begin */
        h5tools_str_reset(buffer);

        h5tools_str_append(buffer, "%s", jndx ? OPTIONAL_LINE_BREAK "" : "");
        h5tools_str_sprint(buffer, info, region_id, type_id,
                               ((char*)region_buf + jndx * type_size), &ctx);

        if (jndx + 1 < npoints || (region_flags & END_OF_DATA) == 0)
            h5tools_str_append(buffer, "%s", OPT(info->elmt_suf1, ","));

        dimension_break = h5tools_render_region_element(stream, info, &ctx, buffer, &curr_pos,
                                                                ncols, ptdata, (hsize_t)0, elmtno);
        /* Render the point element end */
        if(FALSE == dimension_break)
            elmtno = 0;

        ctx.indent_level--;
    } /* end for (jndx = 0; jndx < npoints; jndx++, elmtno++) */

 done:
    HDfree(region_buf);
    HDfree(dims1);

    if(H5Sclose(mem_space) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using points.
 *
 * Return:
 *      The function returns False if the last dimension has been reached, otherwise True
 *
 * In/Out:
 *      h5tools_context_t *ctx
 *      hsize_t *curr_pos
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      hsize_t curr_pos is the total data element position
 *      size_t ncols
 *      hsize_t region_elmt_counter is the region element loop counter
 *      hsize_t elmt_count is the data element loop counter
 *-------------------------------------------------------------------------
 */
hbool_t
h5tools_dump_region_data_points(hid_t region_space, hid_t region_id,
        FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
        h5tools_str_t *buffer, hsize_t *curr_pos, size_t ncols, hsize_t region_elmt_counter,
        hsize_t elmt_counter) {
    HERR_INIT(hbool_t, TRUE)
    hbool_t  dimension_break = TRUE;
    hssize_t npoints;
    hsize_t  alloc_size;
    hsize_t *ptdata;
    int      ndims;
    hssize_t indx;
    hid_t    dtype = -1;
    hid_t    type_id = -1;

    HDassert(info);
    HDassert(ctx);
    HDassert(buffer);

    if((npoints = H5Sget_select_elem_npoints(region_space)) <= 0)
        H5E_THROW(dimension_break, H5E_tools_min_id_g, "H5Sget_select_elem_npoints failed");

    /* Allocate space for the dimension array */
    if((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5E_THROW(dimension_break, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    /* Render the region { element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, "{");
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region { element end */

    /* Render the region datatype info and indices element begin */
    h5tools_str_reset(buffer);

    ctx->indent_level++;
    ctx->need_prefix = TRUE;
    h5tools_str_append(buffer, "REGION_TYPE POINT  ");

    alloc_size = npoints * ndims * sizeof(ptdata[0]);
    HDassert(alloc_size == (hsize_t) ((size_t) alloc_size)); /*check for overflow*/
    if(NULL == (ptdata = (hsize_t *)HDmalloc((size_t) alloc_size)))
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "Could not allocate buffer for ptdata");

    H5_CHECK_OVERFLOW(npoints, hssize_t, hsize_t);
    if(H5Sget_select_elem_pointlist(region_space, (hsize_t) 0, (hsize_t) npoints, ptdata) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Sget_select_elem_pointlist failed");

    for (indx = 0; indx < npoints; indx++) {
        int loop_indx;

        h5tools_str_append(buffer, info->dset_ptformat_pre,
                            indx ? "," OPTIONAL_LINE_BREAK " " : "", (unsigned long) indx);

        for (loop_indx = 0; loop_indx < ndims; loop_indx++)
            h5tools_str_append(buffer, "%s" HSIZE_T_FORMAT, loop_indx ? "," : "(",
                                ptdata[indx * ndims + loop_indx]);

        h5tools_str_append(buffer, ")");
    } /* end for (indx = 0; indx < npoints; indx++) */

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region datatype info and indices element end */

    ctx->need_prefix = TRUE;

    if((dtype = H5Dget_type(region_id)) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Dget_type failed");

    if((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(dimension_break, H5E_tools_min_id_g, "H5Tget_native_type failed");

    /* Render the datatype element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->datatypebegin,
                        h5tools_dump_header_format->datatypeblockbegin);

    ctx->indent_level++;
    h5tools_print_datatype(stream, buffer, info, ctx, dtype, TRUE);
    ctx->indent_level--;

    if (HDstrlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (HDstrlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeend);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the datatype element end */

    ctx->need_prefix = TRUE;

    /* Render the dataspace element begin */
    h5tools_str_reset(buffer);

    ctx->need_prefix = TRUE;
    h5tools_str_append(buffer, "%s ", h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(buffer, region_space);

    if (HDstrlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (HDstrlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->dataspaceend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the dataspace element end */

    ctx->need_prefix = TRUE;

    /* Render the databegin element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->databegin,
                        h5tools_dump_header_format->datablockbegin);

    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);

    ctx->need_prefix = TRUE;

    h5tools_print_region_data_points(region_space, region_id,
            rawdatastream, info, ctx, buffer, ncols, ndims, type_id, npoints, ptdata);

 done:
    HDfree(ptdata);

    if(type_id > 0 && H5Tclose(type_id) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    if(dtype > 0 && H5Tclose(dtype) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

    ctx->need_prefix = TRUE;

    /* Render the dataend element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ",
                        h5tools_dump_header_format->dataend,
                        h5tools_dump_header_format->datablockend);
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos,
                                              ncols, region_elmt_counter, elmt_counter);
    /* Render the dataend element end*/

    ctx->indent_level--;
    ctx->need_prefix = TRUE;

    /* Render the region } element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "}");
    dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos,
                                                ncols, region_elmt_counter, elmt_counter);
    /* Render the region } element end */

    H5_LEAVE(dimension_break)
CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     print out the data for a subset of a dataset.
 * Description:
 *
 *  Select a hyperslab from the dataset DSET using the parameters
 *   specified in SSET. Dump this out to STREAM.
 *
 *  Hyperslabs select "count" blocks of size "block", spaced "stride" elements
 *   from each other, starting at coordinate "start".
 *
 * Return:
 *      On success, return SUCCEED. Otherwise, the function returns FAIL.
 *
 * Algorithm
 *
 * The parameters from SSET are translated into temporary
 * variables so that 1 row is printed at a time (getting the coordinate indices
 * at each row).
 * We define the stride, count and block to be 1 in the row dimension to achieve
 * this and advance until all points are printed.
 *
 * The element position is obtained from the matrix according to:
 *       Given an index I(z,y,x) its position from the beginning of an array
 *       of sizes A(size_z, size_y,size_x) is given by
 *       Position of I(z,y,x) = index_z * size_y * size_x
 *                             + index_y * size_x
 *                             + index_x
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5tools_print_simple_subset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                           hid_t dset, hid_t p_type, struct subset_t *sset,
                           hid_t f_space, hsize_t hyperslab_count,
                           hsize_t *temp_start,/* start inside offset count loop */
                           hsize_t *temp_count,/* count inside offset count loop  */
                           hsize_t *temp_block,/* block size used in loop  */
                           hsize_t *temp_stride,/* stride size used in loop  */
                           hsize_t *total_size,/* total size of dataset */
                           unsigned int row_dim/* index of row_counter dimension */)
{
    HERR_INIT(herr_t, SUCCEED)
    size_t            i;                       /* counters  */
    size_t            j;                       /* counters  */
    hsize_t           zero[1] = {0};           /* vector of zeros */
    unsigned int      flags;                   /* buffer extent flags */
    hsize_t           elmtno;                  /* elemnt index  */
    hsize_t           low[H5S_MAX_RANK];       /* low bound of hyperslab */
    hsize_t           high[H5S_MAX_RANK];      /* higher bound of hyperslab */
    size_t            p_type_nbytes;           /* size of memory type */
    hsize_t           sm_size[H5S_MAX_RANK];   /* stripmine size */
    hsize_t           sm_nbytes;               /* bytes per stripmine */
    hssize_t          ssm_nelmts;              /* elements per stripmine*/
    hsize_t           sm_nelmts;               /* elements per stripmine*/
    unsigned char    *sm_buf = NULL;           /* buffer for raw data */
    hid_t             sm_space = -1;           /* stripmine data space */
    hsize_t           size_row_block;          /* size for blocks along rows */
    hsize_t           row_counter = 0;

    /* VL data special information */
    unsigned int        vl_data = 0; /* contains VL datatypes */

    if ((size_t) ctx->ndims > NELMTS(sm_size))
        H5E_THROW(FAIL, H5E_tools_min_id_g, "ndims and sm_size comparision failed");

    if (ctx->ndims > 0)
        init_acc_pos(ctx, total_size);

    size_row_block = sset->block.data[row_dim];

    /* Check if we have VL data in the dataset's datatype */
    if (h5tools_detect_vlen(p_type) == TRUE)
        vl_data = TRUE;

    /* display loop */
    for (; hyperslab_count > 0; temp_start[row_dim] += temp_stride[row_dim], hyperslab_count--) {
        /* jump rows if size of block exceeded
         cases where block > 1 only and stride > block */
        if (size_row_block > 1
                && row_counter == size_row_block
                && sset->stride.data[row_dim] > sset->block.data[row_dim]) {

            hsize_t increase_rows = sset->stride.data[row_dim] - sset->block.data[row_dim];
            temp_start[row_dim] += increase_rows;
            row_counter = 0;
        }

        row_counter++;

        /* calculate the potential number of elements we're going to print */
        if(H5Sselect_hyperslab(f_space, H5S_SELECT_SET, temp_start, temp_stride, temp_count, temp_block) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");

        if((ssm_nelmts = H5Sget_select_npoints(f_space)) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_select_npoints failed");
        sm_nelmts = (hsize_t)ssm_nelmts;

        if (sm_nelmts > 0) {
            /*
             * determine the strip mine size and allocate a buffer. the strip mine is
             * a hyperslab whose size is manageable.
             */
            if((sm_nbytes = p_type_nbytes = H5Tget_size(p_type)) == 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

            if (ctx->ndims > 0)
                for (i = ctx->ndims; i > 0; --i) {
                    hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
                    if (size == 0) /* datum size > H5TOOLS_BUFSIZE */
                        size = 1;
                    sm_size[i - 1] = MIN(total_size[i - 1], size);
                    sm_nbytes *= sm_size[i - 1];
                    HDassert(sm_nbytes > 0);
                }

            HDassert(sm_nbytes == (hsize_t) ((size_t) sm_nbytes)); /*check for overflow*/
            if(NULL == (sm_buf = (unsigned char *)HDmalloc((size_t) sm_nelmts * p_type_nbytes)))
                H5E_THROW(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for strip-mine");

            if((sm_space = H5Screate_simple(1, &sm_nelmts, NULL)) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Screate_simple failed");

            if(H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &sm_nelmts, NULL) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");

            /* read the data */
            if(H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Dread failed");

            /* print the data */
            flags = START_OF_DATA;

            if (hyperslab_count == 1)
                flags |= END_OF_DATA;

            for (i = 0; i < ctx->ndims; i++)
                ctx->p_max_idx[i] = ctx->p_min_idx[i] + MIN(total_size[i], sm_size[i]);

            /* print array indices. get the lower bound of the hyperslab and calulate
             the element position at the start of hyperslab */
            if(H5Sget_select_bounds(f_space, low, high) < 0)
                H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_select_bounds failed");

            elmtno = 0;
            for (i = 0; i < (size_t) ctx->ndims - 1; i++) {
                hsize_t offset = 1; /* accumulation of the previous dimensions */
                for (j = i + 1; j < (size_t) ctx->ndims; j++)
                    offset *= total_size[j];
                elmtno += low[i] * offset;
            }
            elmtno += low[ctx->ndims - 1];

            /* initialize the current stripmine position; this is necessary to print the array
             indices */
            ctx->sm_pos = elmtno;

            ctx->need_prefix = TRUE;

            if(h5tools_dump_simple_data(stream, info, dset, ctx, flags, sm_nelmts, p_type, sm_buf) < 0)
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "h5tools_dump_simple_data failed");

            /* Reclaim any VL memory, if necessary */
            if (vl_data)
                H5Dvlen_reclaim(p_type, sm_space, H5P_DEFAULT, sm_buf);

            if(H5Sclose(sm_space) < 0)
                H5E_THROW(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
            if(sm_buf)
                HDfree(sm_buf);
            sm_buf = NULL;
        }
        else
            H5E_THROW(SUCCEED, H5E_tools_min_id_g, "nothing to print");

        ctx->continuation++;

    } /* hyperslab_count loop */

CATCH
    if(sm_buf)
        HDfree(sm_buf);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     print out the data for a subset of a dataset.
 * Description:
 *
 *  Select a hyperslab from the dataset DSET using the parameters
 *   specified in SSET. Dump this out to STREAM.
 *
 *  Hyperslabs select "count" blocks of size "block", spaced "stride" elements
 *   from each other, starting at coordinate "start".
 *
 * Return:
 *      On success, return SUCCEED. Otherwise, the function returns FAIL.
 *
 * Algorithm
 *
 * The parameters from SSET are translated into temporary
 * variables so that 1 row is printed at a time (getting the coordinate indices
 * at each row).
 * We define the stride, count and block to be 1 in the row dimension to achieve
 * this and advance until all points are printed.
 *
 * The element position is obtained from the matrix according to:
 *       Given an index I(z,y,x) its position from the beginning of an array
 *       of sizes A(size_z, size_y,size_x) is given by
 *       Position of I(z,y,x) = index_z * size_y * size_x
 *                             + index_y * size_x
 *                             + index_x
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5tools_display_simple_subset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                           hid_t dset, hid_t p_type, struct subset_t *sset,
                           hid_t f_space, hsize_t *total_size)
{
    size_t            i;                       /* counters  */
    hsize_t           n;                       /* counters  */
    hsize_t           count;                   /* hyperslab count */
    hsize_t           outer_count;             /* offset count */
    unsigned int      row_dim;                 /* index of row_counter dimension */
    int               current_outer_dim;       /* dimension for start */
    hsize_t           temp_start[H5S_MAX_RANK];/* temporary start inside offset count loop */
    hsize_t           max_start[H5S_MAX_RANK]; /* maximum start inside offset count loop */
    hsize_t           temp_count[H5S_MAX_RANK];/* temporary count inside offset count loop  */
    hsize_t           temp_block[H5S_MAX_RANK];/* temporary block size used in loop  */
    hsize_t           temp_stride[H5S_MAX_RANK];/* temporary stride size used in loop  */
    int               reset_dim;
    herr_t            ret_value = SUCCEED;

    if (ctx->ndims == 1)
        row_dim = 0;
    else
        row_dim = ctx->ndims - 2;

    /* get the offset count */
    outer_count = 1;
    if (ctx->ndims > 2)
        for (i = 0; i < (size_t) ctx->ndims - 2; i++) {
            /* block size is handled by containing h5tools_print_simple_subset call */
            outer_count = outer_count * sset->count.data[i];
        }

    /* initialize temporary start, count and maximum start */
    for (i = 0; i < (size_t) ctx->ndims; i++) {
        temp_start[i] = sset->start.data[i];
        temp_count[i] = sset->count.data[i];
        temp_block[i] = sset->block.data[i];
        temp_stride[i] = sset->stride.data[i];
        max_start[i] = 0;
    }

    if (ctx->ndims > 2) {
        for (i = 0; i < (size_t) ctx->ndims - 2; i++) {
            max_start[i] = temp_start[i] + sset->count.data[i] * sset->stride.data[i];
            temp_count[i] = 1;
        }
    }

    /* offset loop */
    for (n = 0; n < outer_count; n++) {
        /* number of read iterations in inner loop, read by rows, to match 2D display */
        if (ctx->ndims > 1) {
            /* count is the number of iterations to display all the rows,
             the block size count times */
            count = sset->count.data[row_dim] * sset->block.data[row_dim];

            /* always 1 row_counter at a time, that is a block of size 1, 1 time */
            temp_count[row_dim] = 1;
            temp_block[row_dim] = 1;

            /* advance 1 row_counter at a time  */
            if (sset->block.data[row_dim] > 1)
                temp_stride[row_dim] = 1;
        }
        /* for the 1D case */
        else {
            count = 1;
        }

        h5tools_print_simple_subset(stream, info, ctx, dset, p_type, sset,
                                   f_space, count, temp_start, temp_count,
                                   temp_block, temp_stride, total_size, row_dim);

        if (ctx->ndims > 2) {
            /* dimension for start */
            current_outer_dim = (ctx->ndims - 2) - 1;

            /* set start to original from current_outer_dim up */
            for (i = current_outer_dim + 1; i < ctx->ndims; i++) {
                temp_start[i] = sset->start.data[i];
            }

            /* increment start dimension */
            do {
                reset_dim = 0;
                temp_start[current_outer_dim] += sset->stride.data[current_outer_dim];
                if (temp_start[current_outer_dim] >= max_start[current_outer_dim]) {
                    temp_start[current_outer_dim] = sset->start.data[current_outer_dim];

                    current_outer_dim--;
                    reset_dim = 1;
                }
            } while (current_outer_dim >= 0 && reset_dim);

        } /* ctx.ndims > 1 */

    } /* outer_count */

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose:     Dump out a subset of a dataset.
 * Description:
 *
 *  Select a hyperslab from the dataset DSET using the parameters
 *   specified in SSET. Dump this out to STREAM.
 *
 *  Hyperslabs select "count" blocks of size "block", spaced "stride" elements
 *   from each other, starting at coordinate "start".
 *
 * Return:
 *      On success, return SUCCEED. Otherwise, the function returns FAIL.
 *
 * Original programmer:
 *      Bill Wendling, Wednesday, March 07, 2001
 *
 * Rewritten with modified algorithm by:
 *      Pedro Vicente, Wednesday, January 16, 2008, contributions from Quincey Koziol
 *
 * Algorithm
 *
 * In a inner loop, the parameters from SSET are translated into temporary
 * variables so that 1 row is printed at a time (getting the coordinate indices
 * at each row).
 * We define the stride, count and block to be 1 in the row dimension to achieve
 * this and advance until all points are printed.
 * An outer loop for cases where dimensionality is greater than 2D is made.
 * In each iteration, the 2D block is displayed in the inner loop. The remaining
 * slower dimensions above the first 2 are incremented one at a time in the outer loop
 *
 * The element position is obtained from the matrix according to:
 *       Given an index I(z,y,x) its position from the beginning of an array
 *       of sizes A(size_z, size_y,size_x) is given by
 *       Position of I(z,y,x) = index_z * size_y * size_x
 *                             + index_y * size_x
 *                             + index_x
 *
 *-------------------------------------------------------------------------
 */
static herr_t
h5tools_dump_simple_subset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t dset,
                           hid_t p_type, struct subset_t *sset)
{
    HERR_INIT(herr_t, SUCCEED)
    int               sndims;
    hid_t             f_space = -1;            /* file data space */
    size_t            i;                       /* counters  */
    hsize_t           total_size[H5S_MAX_RANK];/* total size of dataset*/

    if((f_space = H5Dget_space(dset)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");

    if((sndims = H5Sget_simple_extent_ndims(f_space)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");
    ctx->ndims = (unsigned)sndims;

    /* assume entire data space to be printed */
    if (ctx->ndims > 0)
        for (i = 0; i < (size_t) ctx->ndims; i++)
            ctx->p_min_idx[i] = 0;

    if(H5Sget_simple_extent_dims(f_space, total_size, NULL) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");
    ctx->size_last_dim = total_size[ctx->ndims - 1];

    h5tools_display_simple_subset(stream, info, ctx, dset, p_type, sset, f_space, total_size);

CATCH
    if(f_space >= 0 && H5Sclose(f_space) < 0)
        H5E_THROW(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from a dataset with a simple data space.
 * Description:
 *      This is a special case of h5tools_dump_dset(). This function only
 *      intended for dumping datasets -- it does strip mining and some other
 *      things which are unnecessary for smaller objects such as attributes
 *      (to print small objects like attributes simply read the attribute and
 *      call h5tools_dump_simple_mem()).
 * Return:
 *      On success, the function returns SUCCEED. Otherwise, the function
 *      returns FAIL.
 *-------------------------------------------------------------------------
 */
static int
h5tools_dump_simple_dset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                         hid_t dset, hid_t p_type)
{
    HERR_INIT(herr_t, SUCCEED)
    hid_t               f_space = -1;                  /* file data space */
    hsize_t             elmtno;                   /* counter  */
    size_t              i;                        /* counter  */
    int                 carry;                    /* counter carry value */
    hsize_t             zero[8];                  /* vector of zeros */
    unsigned int        flags;                    /* buffer extent flags */
    hsize_t             total_size[H5S_MAX_RANK]; /* total size of dataset*/

    /* Print info */
    size_t              p_type_nbytes;            /* size of memory type */
    hsize_t             p_nelmts;                 /* total selected elmts */

    /* Stripmine info */
    hsize_t             sm_size[H5S_MAX_RANK];    /* stripmine size */
    hsize_t             sm_nbytes;                /* bytes per stripmine */
    hsize_t             sm_nelmts;                /* elements per stripmine*/
    unsigned char      *sm_buf = NULL;            /* buffer for raw data */
    hid_t               sm_space = -1;                 /* stripmine data space */

    /* Hyperslab info */
    hsize_t             hs_offset[H5S_MAX_RANK];  /* starting offset */
    hsize_t             hs_size[H5S_MAX_RANK];    /* size this pass */
    hsize_t             hs_nelmts;                /* elements in request */

    /* VL data special information */
    unsigned int        vl_data = 0; /* contains VL datatypes */

    f_space = H5Dget_space(dset);

    if (f_space == FAIL)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");

    ctx->ndims = H5Sget_simple_extent_ndims(f_space);

    if ((size_t)ctx->ndims > NELMTS(sm_size)) {
        H5E_THROW(FAIL, H5E_tools_min_id_g, "ctx->ndims > NELMTS(sm_size) failed");
    }

    /* Assume entire data space to be printed */
    if (ctx->ndims > 0)
        for (i = 0; i < (size_t)ctx->ndims; i++)
            ctx->p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(f_space, total_size, NULL);

    /* calculate the number of elements we're going to print */
    p_nelmts = 1;

    if (ctx->ndims > 0) {
        for (i = 0; i < ctx->ndims; i++)
            p_nelmts *= total_size[i];
        ctx->size_last_dim = (total_size[ctx->ndims - 1]);
    } /* end if */
    else
        ctx->size_last_dim = 0;

    if (p_nelmts == 0) {
        H5_LEAVE(SUCCEED); /* nothing to print */
    }

    /* Check if we have VL data in the dataset's datatype */
    if (h5tools_detect_vlen(p_type) == TRUE)
        vl_data = TRUE;

    /*
     * Determine the strip mine size and allocate a buffer. The strip mine is
     * a hyperslab whose size is manageable.
     */
    sm_nbytes = p_type_nbytes = H5Tget_size(p_type);

    if (ctx->ndims > 0) {
        for (i = ctx->ndims; i > 0; --i) {
            hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
            if ( size == 0) /* datum size > H5TOOLS_BUFSIZE */
                size = 1;
            sm_size[i - 1] = MIN(total_size[i - 1], size);
            sm_nbytes *= sm_size[i - 1];
            HDassert(sm_nbytes > 0);
        }
    }

    if(!sm_nbytes)
        goto done;

    HDassert(sm_nbytes == (hsize_t)((size_t)sm_nbytes)); /*check for overflow*/
    sm_buf = (unsigned char *)HDmalloc((size_t)sm_nbytes);

    sm_nelmts = sm_nbytes / p_type_nbytes;
    sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

    if (ctx->ndims > 0)
        init_acc_pos(ctx, total_size);

    /* The stripmine loop */
    HDmemset(hs_offset, 0, sizeof hs_offset);
    HDmemset(zero, 0, sizeof zero);

    for (elmtno = 0; elmtno < p_nelmts; elmtno += hs_nelmts) {
        /* Calculate the hyperslab size */
        if (ctx->ndims > 0) {
            for (i = 0, hs_nelmts = 1; i < ctx->ndims; i++) {
                hs_size[i] = MIN(total_size[i] - hs_offset[i], sm_size[i]);
                ctx->p_max_idx[i] = ctx->p_min_idx[i] + hs_size[i];
                hs_nelmts *= hs_size[i];
            }

            H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL);
            H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &hs_nelmts, NULL);
        }
        else {
            H5Sselect_all(f_space);
            H5Sselect_all(sm_space);
            hs_nelmts = 1;
        }

        /* Read the data */
        if (H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) < 0) {
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Dread failed");
        }

        /* Print the data */
        flags = (elmtno == 0) ? START_OF_DATA : 0;
        flags |= ((elmtno + hs_nelmts) >= p_nelmts) ? END_OF_DATA : 0;

        /* initialize the current stripmine position; this is necessary to print the array
         indices */
        ctx->sm_pos = elmtno;

        if(h5tools_dump_simple_data(stream, info, dset, ctx, flags, hs_nelmts, p_type, sm_buf) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "h5tools_dump_simple_data failed");

        /* Reclaim any VL memory, if necessary */
        if (vl_data)
            H5Dvlen_reclaim(p_type, sm_space, H5P_DEFAULT, sm_buf);

        /* Calculate the next hyperslab offset */
        for (i = ctx->ndims, carry = 1; i > 0 && carry; --i) {
            ctx->p_min_idx[i - 1] = ctx->p_max_idx[i - 1];
            hs_offset[i - 1] += hs_size[i - 1];

            if (hs_offset[i - 1] == total_size[i - 1])
                hs_offset[i - 1] = 0;
            else
                carry = 0;
        }

        ctx->continuation++;
    }

CATCH
    if(sm_buf)
        HDfree(sm_buf);

done:
    if(sm_space >= 0 && H5Sclose(sm_space) < 0)
        H5E_THROW(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");
    if(f_space >= 0 && H5Sclose(f_space) < 0)
        H5E_THROW(H5E_tools_g, H5E_tools_min_id_g, "H5Sclose failed");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_dump_simple_mem
 *
 * Purpose: Print some values from memory with a simple data space.
 *  This is a special case of h5tools_dump_mem().
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
static int
h5tools_dump_simple_mem(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t obj_id,
                        hid_t type, hid_t space, void *mem)
{
    HERR_INIT(herr_t, SUCCEED)
    unsigned       i;      /*counters  */
    hsize_t        nelmts; /*total selected elmts */

    ctx->ndims = H5Sget_simple_extent_ndims(space);

    if ((size_t) ctx->ndims > NELMTS(ctx->p_min_idx))
        H5E_THROW(FAIL, H5E_tools_min_id_g, "ctx->ndims > NELMTS(ctx->p_min_idx) failed");

    /* Assume entire data space to be printed */
    for (i = 0; i < ctx->ndims; i++)
        ctx->p_min_idx[i] = 0;

    H5Sget_simple_extent_dims(space, ctx->p_max_idx, NULL);

    for (i = 0, nelmts = 1; ctx->ndims != 0 && i < ctx->ndims; i++)
        nelmts *= ctx->p_max_idx[i] - ctx->p_min_idx[i];

    if (nelmts == 0)
        H5_LEAVE(SUCCEED); /* nothing to print */
    if (ctx->ndims > 0) {
        HDassert(ctx->p_max_idx[ctx->ndims - 1] == (hsize_t) ((int) ctx->p_max_idx[ctx->ndims - 1]));
        ctx->size_last_dim = (int) (ctx->p_max_idx[ctx->ndims - 1]);
    } /* end if */
    else
        ctx->size_last_dim = 0;

    if (ctx->ndims > 0)
        init_acc_pos(ctx, ctx->p_max_idx);

    if(h5tools_dump_simple_data(stream, info, obj_id, ctx, START_OF_DATA | END_OF_DATA, nelmts, type, mem) < 0)
        HERROR(H5E_tools_g, H5E_tools_min_id_g, "h5tools_dump_simple_data failed");

CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_dump_dset
 *
 * Purpose: Print some values from a dataset DSET to the file STREAM
 *  after converting all types to P_TYPE (which should be a
 *  native type).  If P_TYPE is a negative value then it will be
 *  computed from the dataset type using only native types.
 *
 * Note: This function is intended only for datasets since it does
 *  some things like strip mining which are unnecessary for
 *  smaller objects such as attributes. The easiest way to print
 *  small objects is to read the object into memory and call
 *  h5tools_dump_mem().
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *
 * Modifications:
 *   Robb Matzke, 1999-06-07
 *      If info->raw is set then the memory datatype will be the same
 *      as the file datatype.
 *
 *  Bill Wendling, 2001-02-27
 *      Renamed to ``h5tools_dump_dset'' and added the subsetting
 *      parameter.
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_dset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
        hid_t dset, struct subset_t *sset)
{
    hid_t     f_space = -1;
    hid_t     p_type = -1;
    hid_t     f_type = -1;
    H5S_class_t space_type;
    int       status = FAIL;
    h5tool_format_t info_dflt;
    /* Use default values */
    if (!stream)
        stream = rawoutstream;

    if (!info) {
        HDmemset(&info_dflt, 0, sizeof info_dflt);
        info = &info_dflt;
    }

    f_type = H5Dget_type(dset);
    if (f_type < 0)
        goto done;

    if (info->raw || bin_form == 1)
        p_type = H5Tcopy(f_type);
    else if (bin_form == 2)
        p_type = h5tools_get_little_endian_type(f_type);
    else if (bin_form == 3)
        p_type = h5tools_get_big_endian_type(f_type);
    else
        p_type = h5tools_get_native_type(f_type);

    if (p_type < 0)
        goto done;

    /* Check the data space */
    f_space = H5Dget_space(dset);
    if (f_space < 0)
        goto done;

    space_type = H5Sget_simple_extent_type(f_space);

    /* Print the data */
    if (space_type == H5S_SIMPLE || space_type == H5S_SCALAR) {
        if(!sset)
            status = h5tools_dump_simple_dset(rawdatastream, info, ctx, dset, p_type);
        else
            status = h5tools_dump_simple_subset(rawdatastream, info, ctx, dset, p_type, sset);
    }
    else
        /* space is H5S_NULL */
        status = SUCCEED;

done:
    if (f_type > 0)
        H5Tclose(f_type);
    if (p_type > 0)
        H5Tclose(p_type);
    if (f_space > 0)
        H5Sclose(f_space);

    return status;
}

/*-------------------------------------------------------------------------
 * Function: h5tools_dump_mem
 *
 * Purpose: Displays the data contained in MEM. MEM must have the
 *  specified data TYPE and SPACE.  Currently only simple data
 *  spaces are allowed and only the `all' selection.
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_mem(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                hid_t obj_id, hid_t type, hid_t space, void *mem)
{
    HERR_INIT(int, SUCCEED)
    h5tool_format_t    info_dflt;

    /* Use default values */
    if (!stream)
        stream = rawoutstream;

    if (!info) {
        HDmemset(&info_dflt, 0, sizeof(info_dflt));
        info = &info_dflt;
    }

    /* Check the data space */
    if (H5Sis_simple(space) <= 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sis_simple failed")

     H5_LEAVE(h5tools_dump_simple_mem(rawattrstream, info, ctx, obj_id, type, space, mem))

CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    print_datatype
 *
 * Purpose:     print the datatype - do not prefix.
 *
 * Return:      void
 *
 * In/Out:      h5tools_str_t *buffer
 *              h5tools_context_t *ctx
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_print_datatype(FILE *stream, h5tools_str_t *buffer, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t type, int object_search)
{
    HERR_INIT(int, SUCCEED)
    char        *mname;
    hid_t        mtype = -1;
    hid_t        str_type = -1;
    hid_t        super = -1;
    hid_t        tmp_type = -1;
    int          snmembers;
    int          sndims;
    unsigned     nmembers;
    unsigned     i;
    size_t       size = 0;
    size_t       ncols = 80; /*available output width */
    hsize_t      dims[H5TOOLS_DUMP_MAX_RANK];
    hsize_t      curr_pos = 0;        /* total data element position   */
    H5T_str_t    str_pad;
    H5T_cset_t   cset;
    H5T_order_t  order;
    H5T_class_t  type_class;
    H5T_sign_t   sign;           /* sign scheme value */
    htri_t       is_vlstr = FALSE;
    const char  *sign_s = NULL;  /* sign scheme string */
    const char  *order_s = NULL; /* byte order string */

    if((type_class = H5Tget_class(type)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_class failed");
    if (object_search && H5Tcommitted(type) > 0) {
        H5O_info_t  oinfo;
        obj_t      *obj = NULL;    /* Found object */

        H5Oget_info(type, &oinfo);
        obj = search_obj(h5dump_type_table, oinfo.addr);

        if(obj) {
            if(!obj->recorded) {
                h5tools_str_append(buffer,"\"/#"H5_PRINTF_HADDR_FMT"\"", obj->objno);
            }
            else
                h5tools_str_append(buffer, "\"%s\"", obj->objname);
        }
        else {
            error_msg("unknown committed type.\n");
            h5tools_setstatus(EXIT_FAILURE);
        }

        return ret_value;
    }

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    switch (type_class) {
    case H5T_INTEGER:
        if (H5Tequal(type, H5T_STD_I8BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I8BE");
        }
        else if (H5Tequal(type, H5T_STD_I8LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I8LE");
        }
        else if (H5Tequal(type, H5T_STD_I16BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I16BE");
        }
        else if (H5Tequal(type, H5T_STD_I16LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I16LE");
        }
        else if (H5Tequal(type, H5T_STD_I32BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I32BE");
        }
        else if (H5Tequal(type, H5T_STD_I32LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I32LE");
        }
        else if (H5Tequal(type, H5T_STD_I64BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I64BE");
        }
        else if (H5Tequal(type, H5T_STD_I64LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_I64LE");
        }
        else if (H5Tequal(type, H5T_STD_U8BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U8BE");
        }
        else if (H5Tequal(type, H5T_STD_U8LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U8LE");
        }
        else if (H5Tequal(type, H5T_STD_U16BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U16BE");
        }
        else if (H5Tequal(type, H5T_STD_U16LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U16LE");
        }
        else if (H5Tequal(type, H5T_STD_U32BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U32BE");
        }
        else if (H5Tequal(type, H5T_STD_U32LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U32LE");
        }
        else if (H5Tequal(type, H5T_STD_U64BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U64BE");
        }
        else if (H5Tequal(type, H5T_STD_U64LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_U64LE");
        }
        else if (H5Tequal(type, H5T_NATIVE_SCHAR) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_SCHAR");
        }
        else if (H5Tequal(type, H5T_NATIVE_UCHAR) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_UCHAR");
        }
        else if (H5Tequal(type, H5T_NATIVE_SHORT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_SHORT");
        }
        else if (H5Tequal(type, H5T_NATIVE_USHORT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_USHORT");
        }
        else if (H5Tequal(type, H5T_NATIVE_INT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_INT");
        }
        else if (H5Tequal(type, H5T_NATIVE_UINT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_UINT");
        }
        else if (H5Tequal(type, H5T_NATIVE_LONG) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_LONG");
        }
        else if (H5Tequal(type, H5T_NATIVE_ULONG) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_ULONG");
        }
        else if (H5Tequal(type, H5T_NATIVE_LLONG) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_LLONG");
        }
        else if (H5Tequal(type, H5T_NATIVE_ULLONG) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_ULLONG");
        }
        else {

            /* byte order */
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

            /* sign */
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

            /* print size, order, and sign  */
            h5tools_str_append(buffer, "%lu-bit%s%s integer",
                                (unsigned long) (8 * H5Tget_size(type)), order_s, sign_s);
        }
        break;

    case H5T_FLOAT:
        if (H5Tequal(type, H5T_IEEE_F32BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_IEEE_F32BE");
        }
        else if (H5Tequal(type, H5T_IEEE_F32LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_IEEE_F32LE");
        }
        else if (H5Tequal(type, H5T_IEEE_F64BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_IEEE_F64BE");
        }
        else if (H5Tequal(type, H5T_IEEE_F64LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_IEEE_F64LE");
        }
        else if (H5Tequal(type, H5T_VAX_F32) == TRUE) {
            h5tools_str_append(buffer, "H5T_VAX_F32");
        }
        else if (H5Tequal(type, H5T_VAX_F64) == TRUE) {
            h5tools_str_append(buffer, "H5T_VAX_F64");
        }
        else if (H5Tequal(type, H5T_NATIVE_FLOAT) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_FLOAT");
        }
        else if (H5Tequal(type, H5T_NATIVE_DOUBLE) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_DOUBLE");
#if H5_SIZEOF_LONG_DOUBLE !=0
        }
        else if (H5Tequal(type, H5T_NATIVE_LDOUBLE) == TRUE) {
            h5tools_str_append(buffer, "H5T_NATIVE_LDOUBLE");
#endif
        }
        else {

            /* byte order */
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

            /* print size and byte order */
            h5tools_str_append(buffer, "%lu-bit%s floating-point",
                                (unsigned long) (8 * H5Tget_size(type)), order_s);

        }
        break;

    case H5T_TIME:
        h5tools_str_append(buffer, "H5T_TIME: not yet implemented");
        break;

    case H5T_STRING:
        /* Make a copy of type in memory in case when TYPE is on disk, the size
         * will be bigger than in memory.  This makes it easier to compare
         * types in memory. */
        tmp_type = H5Tcopy(type);
        size = H5Tget_size(tmp_type);
        str_pad = H5Tget_strpad(tmp_type);
        cset = H5Tget_cset(tmp_type);
        is_vlstr = H5Tis_variable_str(tmp_type);

        curr_pos = ctx->cur_column;
        h5tools_str_append(buffer, "H5T_STRING %s", h5tools_dump_header_format->strblockbegin);
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        ctx->indent_level++;

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);

        if (is_vlstr)
            h5tools_str_append(buffer, "%s H5T_VARIABLE;", STRSIZE);
        else
            h5tools_str_append(buffer, "%s %d;", STRSIZE, (int) size);
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);

        h5tools_str_append(buffer, "%s ", STRPAD);
        switch (str_pad) {
            case H5T_STR_NULLTERM:
                h5tools_str_append(buffer, "H5T_STR_NULLTERM;");
                break;
            case H5T_STR_NULLPAD:
                h5tools_str_append(buffer, "H5T_STR_NULLPAD;");
                break;
            case H5T_STR_SPACEPAD:
                h5tools_str_append(buffer, "H5T_STR_SPACEPAD;");
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
                h5tools_str_append(buffer, "H5T_STR_UNKNOWN;");
                break;
            case H5T_STR_ERROR:
                h5tools_str_append(buffer, "H5T_STR_ERROR;");
                break;
            default:
                h5tools_str_append(buffer, "ERROR;");
            break;
        }
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);

        h5tools_str_append(buffer, "%s ", CSET);

        switch (cset) {
            case H5T_CSET_ASCII:
                h5tools_str_append(buffer, "H5T_CSET_ASCII;");
                break;
            case H5T_CSET_UTF8:
                h5tools_str_append(buffer, "H5T_CSET_UTF8;");
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
                h5tools_str_append(buffer, "H5T_CSET_UNKNOWN;");
                break;
            case H5T_CSET_ERROR:
                h5tools_str_append(buffer, "H5T_CSET_ERROR;");
                break;
            default:
                h5tools_str_append(buffer, "ERROR;");
            break;
        }
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);

        str_type = H5Tcopy(H5T_C_S1);
        if (is_vlstr)
            H5Tset_size(str_type, H5T_VARIABLE);
        else
            H5Tset_size(str_type, size);
        H5Tset_cset(str_type, cset);
        H5Tset_strpad(str_type, str_pad);

        h5tools_str_append(buffer, "%s ", CTYPE);

        /* Check C variable-length string first. Are the two types equal? */
        if (H5Tequal(tmp_type, str_type)) {
            h5tools_str_append(buffer, "H5T_C_S1;");
            goto found_string_type;
        }

        /* Change the endianness and see if they're equal. */
        order = H5Tget_order(tmp_type);
        if(order == H5T_ORDER_LE) {
            if(H5Tset_order(str_type, H5T_ORDER_LE) < 0)
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tset_order failed");
        } /* end if */
        else if(order == H5T_ORDER_BE) {
            if(H5Tset_order(str_type, H5T_ORDER_BE) < 0)
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tset_order failed");
        } /* end if */

        if(H5Tequal(tmp_type, str_type)) {
            h5tools_str_append(buffer, "H5T_C_S1;");
            goto found_string_type;
        }

        /* If not equal to C variable-length string, check Fortran type. */
        if(H5Tclose(str_type) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");
        str_type = H5Tcopy(H5T_FORTRAN_S1);

        H5Tset_cset(str_type, cset);
        H5Tset_size(str_type, size);
        H5Tset_strpad(str_type, str_pad);

        /* Are the two types equal? */
        if (H5Tequal(tmp_type, str_type)) {
            h5tools_str_append(buffer, "H5T_FORTRAN_S1;");
            goto found_string_type;
        }

        /* Change the endianness and see if they're equal. */
        order = H5Tget_order(tmp_type);
        if(order == H5T_ORDER_LE) {
            if(H5Tset_order(str_type, H5T_ORDER_LE) < 0)
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tset_order failed");
        } /* end if */
        else if(order == H5T_ORDER_BE) {
            if(H5Tset_order(str_type, H5T_ORDER_BE) < 0)
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tset_order failed");
        } /* end if */

        if(H5Tequal(tmp_type, str_type)) {
            h5tools_str_append(buffer, "H5T_FORTRAN_S1;");
            goto found_string_type;
        }

        /* Type doesn't match any of above. */
        h5tools_str_append(buffer, "unknown_one_character_type;");

  found_string_type:
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
        ctx->indent_level--;

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);
        if(H5Tclose(str_type) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");
        if(H5Tclose(tmp_type) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->strblockend);
        break;

    case H5T_BITFIELD:
        if (H5Tequal(type, H5T_STD_B8BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B8BE");
        }
        else if (H5Tequal(type, H5T_STD_B8LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B8LE");
        }
        else if (H5Tequal(type, H5T_STD_B16BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B16BE");
        }
        else if (H5Tequal(type, H5T_STD_B16LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B16LE");
        }
        else if (H5Tequal(type, H5T_STD_B32BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B32BE");
        }
        else if (H5Tequal(type, H5T_STD_B32LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B32LE");
        }
        else if (H5Tequal(type, H5T_STD_B64BE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B64BE");
        }
        else if (H5Tequal(type, H5T_STD_B64LE) == TRUE) {
            h5tools_str_append(buffer, "H5T_STD_B64LE");
        }
        else {
            h5tools_str_append(buffer, "undefined bitfield");
        }
        break;

    case H5T_OPAQUE:
        h5tools_str_append(buffer, "H5T_OPAQUE %s", h5tools_dump_header_format->structblockbegin);
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
        ctx->indent_level++;
        {
           char *ttag;

           if(NULL == (ttag = H5Tget_tag(type)))
              H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_tag failed");

           ctx->need_prefix = TRUE;
           h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

           h5tools_str_reset(buffer);
           h5tools_str_append(buffer, "OPAQUE_TAG \"%s\";", ttag);
           h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

           H5free_memory(ttag);
        }
        ctx->indent_level--;

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->structblockend);
        break;

    case H5T_COMPOUND:
        if((snmembers = H5Tget_nmembers(type)) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_nmembers failed");
        nmembers = (unsigned)snmembers;

        h5tools_str_append(buffer, "H5T_COMPOUND %s", h5tools_dump_header_format->structblockbegin);
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        ctx->indent_level++;
        for (i = 0; i < nmembers; i++) {
            mname = H5Tget_member_name(type, i);
            if((mtype = H5Tget_member_type(type, i)) >= 0) {
                ctx->need_prefix = TRUE;
                h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

                h5tools_str_reset(buffer);
                h5tools_print_datatype(stream, buffer, info, ctx, mtype, TRUE);

                h5tools_str_append(buffer, " \"%s\";", mname);
                h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                if(H5Tclose(mtype) < 0)
                    HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");
            }
            else
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tget_member_type failed");
            H5free_memory(mname);
        }
        ctx->indent_level--;

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->structblockend);
        break;

    case H5T_REFERENCE:
        h5tools_str_append(buffer, "H5T_REFERENCE");
        if (H5Tequal(type, H5T_STD_REF_DSETREG) == TRUE) {
            h5tools_str_append(buffer, " { H5T_STD_REF_DSETREG }");
        }
        else {
            h5tools_str_append(buffer, " { H5T_STD_REF_OBJECT }");
        }
        break;

    case H5T_ENUM:
        if((super = H5Tget_super(type)) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_super failed");

        h5tools_str_append(buffer, "H5T_ENUM %s", h5tools_dump_header_format->enumblockbegin);
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
        ctx->indent_level++;

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);
        h5tools_print_datatype(stream, buffer, info, ctx, super, TRUE);

        if(H5Tclose(super) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

        h5tools_str_append(buffer, ";");
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        h5tools_print_enum(stream, buffer, info, ctx, type);

        ctx->indent_level--;

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->enumblockend);

        break;

    case H5T_VLEN:
        if((super = H5Tget_super(type)) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_super failed");

        h5tools_str_append(buffer, "H5T_VLEN %s ", h5tools_dump_header_format->vlenblockbegin);

        h5tools_print_datatype(stream, buffer, info, ctx, super, TRUE);

        if(H5Tclose(super) < 0)
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");

        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->vlenblockend);

        break;

    case H5T_ARRAY:
        h5tools_str_append(buffer, "H5T_ARRAY { ");

        /* Get array information */
        if((sndims = H5Tget_array_ndims(type)) >= 0) {
            unsigned     ndims = (unsigned)sndims;

            if(H5Tget_array_dims2(type, dims) >= 0) {
                /* Print array dimensions */
                for (i = 0; i < ndims; i++)
                    h5tools_str_append(buffer, "[" HSIZE_T_FORMAT "]", dims[i]);

                h5tools_str_append(buffer, " ");
            }
            else
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tget_array_dims2 failed");
        }
        else
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tget_array_ndims failed");

        /* Get array base type */
        if((super = H5Tget_super(type)) >= 0) {
            /* Print base type */
            h5tools_print_datatype(stream, buffer, info, ctx, super, TRUE);
            /* Close array base type */
            if(H5Tclose(super) < 0)
                HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tclose failed");
        }
        else
            HERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Tget_super failed");

        h5tools_str_append(buffer, " }");

        break;

    case H5T_NO_CLASS:
    case H5T_NCLASSES:
    default:
        h5tools_str_append(buffer, "unknown datatype");
        break;
    }

CATCH
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    print_dataspace
 *
 * Purpose:     print the dataspace.
 *
 * Return:      void
 *
 * In/Out:      h5tools_str_t *buffer
 *              h5tools_context_t *ctx
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_print_dataspace(h5tools_str_t *buffer, hid_t space)
{
    HERR_INIT(int, SUCCEED)
    hsize_t     size[H5TOOLS_DUMP_MAX_RANK];
    hsize_t     maxsize[H5TOOLS_DUMP_MAX_RANK];
    int         ndims = -1;
    H5S_class_t space_type = -1;
    int         i;

    if((ndims = H5Sget_simple_extent_dims(space, size, maxsize)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

    if((space_type = H5Sget_simple_extent_type(space)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_type failed");

    switch(space_type) {
    case H5S_SCALAR:
        /* scalar dataspace */
        h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->dataspacedescriptionbegin, S_SCALAR);
        break;

    case H5S_SIMPLE:
        /* simple dataspace */
        h5tools_str_append(buffer, "%s %s { %s " HSIZE_T_FORMAT,
                            h5tools_dump_header_format->dataspacedescriptionbegin, S_SIMPLE,
                            h5tools_dump_header_format->dataspacedimbegin, size[0]);

        for(i = 1; i < ndims; i++)
            h5tools_str_append(buffer, ", " HSIZE_T_FORMAT, size[i]);

        h5tools_str_append(buffer, " %s / ", h5tools_dump_header_format->dataspacedimend);

        if(maxsize[0] == H5S_UNLIMITED)
            h5tools_str_append(buffer, "%s %s",
                                h5tools_dump_header_format->dataspacedimbegin, "H5S_UNLIMITED");
        else
            h5tools_str_append(buffer, "%s " HSIZE_T_FORMAT,
                                h5tools_dump_header_format->dataspacedimbegin, maxsize[0]);

        for(i = 1; i < ndims; i++)
            if(maxsize[i] == H5S_UNLIMITED)
                h5tools_str_append(buffer, ", %s", "H5S_UNLIMITED");
            else
                h5tools_str_append(buffer, ", " HSIZE_T_FORMAT, maxsize[i]);

        h5tools_str_append(buffer, " %s }", h5tools_dump_header_format->dataspacedimend);
        break;

    case H5S_NULL:
        /* null dataspace */
        h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->dataspacedescriptionbegin, S_NULL);
        break;

    case H5S_NO_CLASS:
    default:
        h5tools_str_append(buffer, "%s unknown dataspace %s\n", BEGIN, END);
        break;
    } /* end switch */

CATCH
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:    print_enum
 *
 * Purpose:     prints the enum data
 *
 * Return:      void
 *
 * In/Out:      h5tools_str_t *buffer
 *              h5tools_context_t *ctx
 *
 *-----------------------------------------------------------------------*/
int
h5tools_print_enum(FILE *stream, h5tools_str_t *buffer, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t type)
{
    HERR_INIT(int, SUCCEED)
    char         **name = NULL;  /*member names                   */
    unsigned char *value = NULL; /*value array                    */
    unsigned char *copy = NULL;  /*a pointer to value array       */
    unsigned       i;
    unsigned       nmembs = 0;   /*number of members              */
    int            snmembs;
    int            nchars;       /*number of output characters    */
    hid_t          super = -1;   /*enum base integer type         */
    hid_t          native = -1;  /*native integer datatype        */
    H5T_sign_t     sign_type;    /*sign of value type             */
    size_t         type_size;    /*value type size                */
    size_t         dst_size;     /*destination value type size    */
    size_t         ncols = 80; /*available output width */
    hsize_t        curr_pos = 0;        /* total data element position   */

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    if((snmembs = H5Tget_nmembers(type)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_nmembers failed");
    nmembs = (unsigned)snmembs;
    HDassert(nmembs > 0);

    if((super = H5Tget_super(type)) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_super failed");

    if((type_size = H5Tget_size(type)) <= 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_size(type) failed");

    /*
     * Determine what datatype to use for the native values.  To simplify
     * things we entertain three possibilities:
     *  1. long long -- the largest native signed integer
     *  2. unsigned long long -- the largest native unsigned integer
     *  3. raw format
     */
    if(type_size <= sizeof(long long)) {
        dst_size = sizeof(long long);

        if((sign_type = H5Tget_sign(type))<0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_sign failed");
        if(H5T_SGN_NONE == sign_type)
            native = H5T_NATIVE_ULLONG;
        else
            native = H5T_NATIVE_LLONG;
    } /* end if */
    else
        dst_size = type_size;

    /* Get the names and raw values of all members */
    if(NULL == (name = (char **)HDcalloc((size_t)nmembs, sizeof(char *))))
        H5E_THROW(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for member name");
    if(NULL == (value = (unsigned char *)HDcalloc((size_t)nmembs, MAX(type_size, dst_size))))
        H5E_THROW(FAIL, H5E_tools_min_id_g, "Could not allocate buffer for member value");

    for (i = 0; i < nmembs; i++) {
        name[i] = H5Tget_member_name(type, i);
        if(H5Tget_member_value(type, i, value + i * type_size) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tget_member_value failed");
    }

    /* Convert values to native datatype */
    if (native > 0)
        if(H5Tconvert(super, native, (size_t)nmembs, value, NULL, H5P_DEFAULT) < 0)
            H5E_THROW(FAIL, H5E_tools_min_id_g, "H5Tconvert failed");

    /*
     * Sort members by increasing value
     *    ***not implemented yet***
     */

    /* Print members */
    for (i = 0; i < nmembs; i++) {
        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);
        h5tools_str_append(buffer, "\"%s\"", name[i]);
        nchars = HDstrlen(name[i]);
        h5tools_str_append(buffer, "%*s ", MAX(0, 16 - nchars), "");

        if (native < 0) {
            size_t j;

            h5tools_str_append(buffer, "0x");

            for (j = 0; j < dst_size; j++)
                h5tools_str_append(buffer, "%02x", value[i * dst_size + j]);
        }
        else if (H5T_SGN_NONE == H5Tget_sign(native)) {
            /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
             *strangely, unless use another pointer "copy".*/
            copy = value + i * dst_size;
            h5tools_str_append(buffer, HSIZE_T_FORMAT, *((unsigned long long *) ((void *) copy)));
        }
        else {
            /*On SGI Altix(cobalt), wrong values were printed out with "value+i*dst_size"
             *strangely, unless use another pointer "copy".*/
            copy = value + i * dst_size;
            h5tools_str_append(buffer, "%" H5_PRINTF_LL_WIDTH "d", *((long long *) ((void *) copy)));
        }

        h5tools_str_append(buffer, ";");
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
    }

CATCH
    if(name) {
        /* Release resources */
        for(i = 0; i < nmembs; i++)
            if(name[i])
                H5free_memory(name[i]);
        HDfree(name);
    } /* end if */

    if(value)
        HDfree(value);

    if(super >= 0 && H5Tclose(super) < 0)
        H5E_THROW(FAIL, H5E_tools_min_id_g, "Could not close datatype's super class");

    if(0 == nmembs)
        h5tools_str_append(buffer, "\n<empty>");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    dump_datatype
 *
 * Purpose:     Dump the datatype. Datatype can be HDF5 predefined
 *              atomic datatype or committed/transient datatype.
 *
 * Return:      void
 *
 * In/Out:      h5tools_context_t *ctx
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_datatype(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t type)
{
    h5tools_str_t buffer;          /* string into which to render   */
    size_t        ncols = 80;      /* available output width        */
    hsize_t       curr_pos = ctx->sm_pos;   /* total data element position   */
                                            /* pass to the prefix in h5tools_simple_prefix the total position
                                             * instead of the current stripmine position i; this is necessary
                                             * to print the array indices
                                             */

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = TRUE;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ",
                        h5tools_dump_header_format->datatypebegin,
                        h5tools_dump_header_format->datatypeblockbegin);
    h5tools_print_datatype(stream, &buffer, info, ctx, type, TRUE);
    if (HDstrlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (HDstrlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(&buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datatypeend);

    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_dataspace
 *
 * Purpose:     Dump the dataspace.
 *
 * Return:      void
 *
 * In/Out:      h5tools_context_t *ctx
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_dataspace(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t type)
{
    h5tools_str_t buffer;          /* string into which to render   */
    size_t        ncols = 80;      /* available output width        */
    hsize_t       curr_pos = ctx->sm_pos;   /* total data element position   */
                                            /* pass to the prefix in h5tools_simple_prefix the total position
                                             * instead of the current stripmine position i; this is necessary
                                             * to print the array indices
                                             */

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = TRUE;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s ",
                        h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(&buffer, type);

    if (HDstrlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (HDstrlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(&buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->dataspaceend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->dataspaceend);

    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_oid
 *
 * Purpose:     Dump the oid.
 *
 * Return:      void
 *
 * In/Out:      h5tools_context_t *ctx
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_oid(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t oid)
{
    h5tools_str_t buffer;          /* string into which to render   */
    size_t        ncols = 80;      /* available output width        */
    hsize_t       curr_pos = ctx->sm_pos;   /* total data element position   */
                                            /* pass to the prefix in h5tools_simple_prefix the total position
                                             * instead of the current stripmine position i; this is necessary
                                             * to print the array indices
                                             */

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s %d %s", OBJID, BEGIN, oid, END);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}


/*-------------------------------------------------------------------------
 * Function:    dump_fill_value
 *
 * Purpose:     prints the fill value
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
h5tools_print_fill_value(h5tools_str_t *buffer/*in,out*/, const h5tool_format_t *info, h5tools_context_t *ctx/*in,out*/, hid_t dcpl, hid_t type_id, hid_t obj_id)
{
    size_t            size;
    hid_t             n_type;
    void             *buf = NULL;

    n_type = h5tools_get_native_type(type_id);

    size = H5Tget_size(n_type);
    buf = HDmalloc(size);

    H5Pget_fill_value(dcpl, n_type, buf);

    h5tools_str_sprint(buffer, info, obj_id, n_type, buf, ctx);

    H5Tclose(n_type);

    if (buf)
        HDfree (buf);
}

/*-------------------------------------------------------------------------
 * Function:    dump_dcpl
 *
 * Purpose:     prints several dataset create property list properties
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_dcpl(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t dcpl_id, hid_t type_id, hid_t obj_id)
{
    int              nfilters;       /* number of filters */
    int              rank;           /* rank */
    int              i;
    unsigned         j;
    unsigned         filt_flags;     /* filter flags */
    unsigned         cd_values[20];  /* filter client data values */
    unsigned         szip_options_mask;
    unsigned         szip_pixels_per_block;
    H5Z_filter_t     filtn;          /* filter identification number */
    H5D_fill_value_t fvstatus;
    H5D_alloc_time_t at;
    H5D_fill_time_t  ft;
    H5D_layout_t     stl;
    size_t           ncols = 80;      /* available output width        */
    size_t           cd_nelmts;      /* filter client number of values */
    off_t            offset;         /* offset of external file     */
    char             f_name[256];    /* filter name */
    char             name[256];      /* external file name       */
    hsize_t          chsize[64];     /* chunk size in elements */
    hsize_t          size;           /* size of external file   */
    hsize_t          storage_size;
    hsize_t          curr_pos = 0;        /* total data element position   */
    h5tools_str_t    buffer;          /* string into which to render   */

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));
    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    storage_size = H5Dget_storage_size(obj_id);
    nfilters = H5Pget_nfilters(dcpl_id);
    HDstrcpy(f_name,"\0");

    /*-------------------------------------------------------------------------
    * STORAGE_LAYOUT
    *-------------------------------------------------------------------------
    */
    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", STORAGE_LAYOUT, BEGIN);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    stl = H5Pget_layout(dcpl_id);
    switch (stl) {
        case H5D_CHUNKED:
            ctx->indent_level++;
            ctx->need_prefix = TRUE;
            h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s ", CHUNKED);

            rank = H5Pget_chunk(dcpl_id, (int) NELMTS(chsize), chsize);
            h5tools_str_append(&buffer, "%s " HSIZE_T_FORMAT, h5tools_dump_header_format->dataspacedimbegin, chsize[0]);
            for(i = 1; i < rank; i++)
                h5tools_str_append(&buffer, ", " HSIZE_T_FORMAT, chsize[i]);
            h5tools_str_append(&buffer, " %s", h5tools_dump_header_format->dataspacedimend);
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

            ctx->need_prefix = TRUE;
            h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

            h5tools_str_reset(&buffer);

            /* if there are filters, print a compression ratio */
            if (nfilters) {
                hsize_t dims[H5S_MAX_RANK];
                hsize_t nelmts = 1;
                double ratio = 0;
                int ok = 0;

                hid_t tid = H5Dget_type(obj_id);
                hid_t sid = H5Dget_space(obj_id);
                size_t datum_size = H5Tget_size(tid);
                int ndims = H5Sget_simple_extent_dims(sid, dims, NULL);

                /* only print the compression ratio for these filters */
                for (i = 0; i < nfilters && !ok; i++) {
                    cd_nelmts = NELMTS(cd_values);
                    filtn = H5Pget_filter2(dcpl_id, (unsigned) i, &filt_flags, &cd_nelmts, cd_values, sizeof(f_name),
                            f_name, NULL);
                    ok = (filtn >= 0);
                }

                if(ndims && ok) {
                    hsize_t uncomp_size;

                    for(i = 0; i < ndims; i++) {
                        nelmts *= dims[i];
                    }
                    uncomp_size = nelmts * datum_size;

                    /* compression ratio = uncompressed size /  compressed size */

                    if(storage_size != 0)
                        ratio = (double) uncomp_size / (double) storage_size;

                    h5tools_str_append(&buffer, "SIZE " HSIZE_T_FORMAT" (%.3f:1 COMPRESSION)", storage_size, ratio);

                }
                else
                    h5tools_str_append(&buffer, "SIZE " HSIZE_T_FORMAT, storage_size);

                H5Sclose(sid);
                H5Tclose(tid);

            }
            else {
                h5tools_str_append(&buffer, "SIZE " HSIZE_T_FORMAT, storage_size);
            }
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

            ctx->indent_level--;
            break;
        case H5D_COMPACT:
            ctx->indent_level++;
            ctx->need_prefix = TRUE;
            h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s", COMPACT);
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

            ctx->need_prefix = TRUE;
            h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "SIZE " HSIZE_T_FORMAT, storage_size);
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

            ctx->indent_level--;
            break;
        case H5D_CONTIGUOUS:
            {
                int next;

                next = H5Pget_external_count(dcpl_id);

                /*-------------------------------------------------------------------------
                 * EXTERNAL_FILE
                 *-------------------------------------------------------------------------
                 */
                ctx->indent_level++;
                if (next) {
                    ctx->need_prefix = TRUE;
                    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s", CONTIGUOUS);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                    ctx->need_prefix = TRUE;
                    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s %s", EXTERNAL, BEGIN);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                    ctx->indent_level++;
                    for (j = 0; j < (unsigned) next; j++) {
                        H5Pget_external(dcpl_id, j, sizeof(name), name, &offset, &size);

                        ctx->need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "FILENAME %s SIZE " HSIZE_T_FORMAT, name, size);
                        h5tools_str_append(&buffer, " OFFSET %ld", offset);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    }
                    ctx->indent_level--;

                    ctx->need_prefix = TRUE;
                    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s", END);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t) ncols, (hsize_t) 0, (hsize_t) 0);
                }
                else {
                    haddr_t ioffset;

                    ctx->need_prefix = TRUE;
                    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s", CONTIGUOUS);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                    ctx->need_prefix = TRUE;
                    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer,"SIZE " HSIZE_T_FORMAT, storage_size);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                    ctx->need_prefix = TRUE;
                    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                    h5tools_str_reset(&buffer);
                    ioffset = H5Dget_offset(obj_id);
                    h5tools_str_append(&buffer, "OFFSET "H5_PRINTF_HADDR_FMT, ioffset);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t) ncols, (hsize_t) 0, (hsize_t) 0);
                }
                ctx->indent_level--;
            }
            break;

        case H5D_LAYOUT_ERROR:
        case H5D_NLAYOUTS:
        default:
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s", "Unknown layout");
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t) ncols, (hsize_t) 0, (hsize_t) 0);
    }/*switch*/

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s", END);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    /*-------------------------------------------------------------------------
    * FILTERS
    *-------------------------------------------------------------------------
    */

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", FILTERS, BEGIN);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level++;

    if(nfilters) {
        for(i = 0; i < nfilters; i++) {
            cd_nelmts = NELMTS(cd_values);
            filtn = H5Pget_filter2(dcpl_id, (unsigned)i, &filt_flags, &cd_nelmts,
                cd_values, sizeof(f_name), f_name, NULL);

            if (filtn<0)
                continue; /* nothing to print for invalid filter */

            ctx->need_prefix = TRUE;
            h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

            h5tools_str_reset(&buffer);
            switch(filtn) {
                case H5Z_FILTER_DEFLATE:
                    h5tools_str_append(&buffer, "%s %s %s %d %s", DEFLATE, BEGIN, DEFLATE_LEVEL, cd_values[0], END);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    break;
                case H5Z_FILTER_SHUFFLE:
                    h5tools_str_append(&buffer, "%s", SHUFFLE);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    break;
                case H5Z_FILTER_FLETCHER32:
                    h5tools_str_append(&buffer, "%s", FLETCHER32);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    break;
                case H5Z_FILTER_SZIP:
                    {
                        szip_options_mask = cd_values[0];;
                        szip_pixels_per_block = cd_values[1];

                        h5tools_str_append(&buffer, "%s %s",SZIP, BEGIN);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                        ctx->indent_level++;

                        ctx->need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "PIXELS_PER_BLOCK %d", szip_pixels_per_block);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                        ctx->need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                        h5tools_str_reset(&buffer);
                        if(szip_options_mask & H5_SZIP_CHIP_OPTION_MASK)
                            h5tools_str_append(&buffer, "MODE %s", "HARDWARE");
                        else if(szip_options_mask & H5_SZIP_ALLOW_K13_OPTION_MASK)
                            h5tools_str_append(&buffer, "MODE %s", "K13");
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                        ctx->need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                        h5tools_str_reset(&buffer);
                        if(szip_options_mask & H5_SZIP_EC_OPTION_MASK)
                            h5tools_str_append(&buffer, "CODING %s", "ENTROPY");
                        else if(szip_options_mask & H5_SZIP_NN_OPTION_MASK)
                            h5tools_str_append(&buffer, "CODING %s", "NEAREST NEIGHBOUR");
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                        ctx->need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                        h5tools_str_reset(&buffer);
                        if(szip_options_mask & H5_SZIP_LSB_OPTION_MASK)
                            h5tools_str_append(&buffer, "BYTE_ORDER %s", "LSB");
                        else if(szip_options_mask & H5_SZIP_MSB_OPTION_MASK)
                            h5tools_str_append(&buffer, "BYTE_ORDER %s", "MSB");
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                        if(szip_options_mask & H5_SZIP_RAW_OPTION_MASK) {
                            ctx->need_prefix = TRUE;
                            h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, "HEADER %s", "RAW");
                            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                        }

                        ctx->indent_level--;

                        ctx->need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "%s", END);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    }
                    break;
                case H5Z_FILTER_NBIT:
                    h5tools_str_append(&buffer, "%s", NBIT);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    break;
                case H5Z_FILTER_SCALEOFFSET:
                    h5tools_str_append(&buffer, "%s %s %s %d %s", SCALEOFFSET, BEGIN, SCALEOFFSET_MINBIT, cd_values[0], END);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    break;
                default:
                    h5tools_str_append(&buffer, "%s %s", "USER_DEFINED_FILTER", BEGIN);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                    ctx->indent_level++;

                    ctx->need_prefix = TRUE;
                    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "FILTER_ID %d", filtn);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                    if(f_name[0] != '\0') {
                        ctx->need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "COMMENT %s", f_name);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    }
                    if (cd_nelmts) {
                        ctx->need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "%s %s ","PARAMS", BEGIN);
                        for (j=0; j<cd_nelmts; j++)
                            h5tools_str_append(&buffer, "%d ", cd_values[j]);
                        h5tools_str_append(&buffer, "%s", END);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    }
                    ctx->indent_level--;

                    ctx->need_prefix = TRUE;
                    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s",END);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    break;
            }/*switch*/
        } /*i*/
    }/*nfilters*/
    else {

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "NONE");
        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
    }
    ctx->indent_level--;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s",END);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    /*-------------------------------------------------------------------------
    * FILLVALUE
    *-------------------------------------------------------------------------
    */
    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", FILLVALUE, BEGIN);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level++;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "FILL_TIME ");

    H5Pget_fill_time(dcpl_id, &ft);
    switch(ft) {
        case H5D_FILL_TIME_ALLOC:
            h5tools_str_append(&buffer, "%s", "H5D_FILL_TIME_ALLOC");
            break;
        case H5D_FILL_TIME_NEVER:
            h5tools_str_append(&buffer, "%s", "H5D_FILL_TIME_NEVER");
            break;
        case H5D_FILL_TIME_IFSET:
            h5tools_str_append(&buffer, "%s", "H5D_FILL_TIME_IFSET");
            break;
        case H5D_FILL_TIME_ERROR:
        default:
            HDassert(0);
            break;
    }
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s ", "VALUE ");
    H5Pfill_value_defined(dcpl_id, &fvstatus);
    switch (fvstatus) {
        case H5D_FILL_VALUE_UNDEFINED:
            h5tools_str_append(&buffer, "%s", "H5D_FILL_VALUE_UNDEFINED");
            break;
        case H5D_FILL_VALUE_DEFAULT:
            h5tools_str_append(&buffer, "%s", "H5D_FILL_VALUE_DEFAULT");
            break;
        case H5D_FILL_VALUE_USER_DEFINED:
            ctx->indent_level--;
            h5tools_print_fill_value(&buffer, info, ctx, dcpl_id, type_id, obj_id);
            ctx->indent_level++;
            break;
        case H5D_FILL_VALUE_ERROR:
        default:
            HDassert(0);
            break;
    }
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
    ctx->indent_level--;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s", END);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    /*-------------------------------------------------------------------------
    * ALLOCATION_TIME
    *-------------------------------------------------------------------------
    */
    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "ALLOCATION_TIME %s", BEGIN);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level++;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    H5Pget_alloc_time(dcpl_id, &at);
    switch(at) {
        case H5D_ALLOC_TIME_EARLY:
            h5tools_str_append(&buffer, "%s", "H5D_ALLOC_TIME_EARLY");
            break;
        case H5D_ALLOC_TIME_INCR:
            h5tools_str_append(&buffer, "%s", "H5D_ALLOC_TIME_INCR");
            break;
        case H5D_ALLOC_TIME_LATE:
            h5tools_str_append(&buffer, "%s", "H5D_ALLOC_TIME_LATE");
            break;
        default:
            HDassert(0);
            break;
    }
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level--;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s", END);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_comment
 *
 * Purpose:     prints the comment for the the object name
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_comment(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t obj_id)
{
    char         *comment = NULL;
    ssize_t       cmt_bufsize = -1;
    size_t        buf_size = 0;
    size_t        ncols = 80;      /* available output width        */
    h5tools_str_t buffer;          /* string into which to render   */
    hsize_t       curr_pos = ctx->sm_pos;   /* total data element position   */
                                            /* pass to the prefix in h5tools_simple_prefix the total position
                                             * instead of the current stripmine position i; this is necessary
                                             * to print the array indices
                                             */

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    cmt_bufsize = H5Oget_comment(obj_id, comment, buf_size);

    /* call H5Oget_comment again with the correct value.
     * If the call to H5Oget_comment returned an error, skip this block */
    if (cmt_bufsize > 0) {
        comment = (char *)HDmalloc((size_t)(cmt_bufsize+1)); /* new_size including null terminator */
        if(comment) {
            cmt_bufsize = H5Oget_comment(obj_id, comment, cmt_bufsize);
            if(cmt_bufsize > 0) {
                comment[cmt_bufsize] = '\0'; /* necessary because null char is not returned */

                ctx->need_prefix = TRUE;

                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "COMMENT \"%s\"", comment);
                h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

                h5tools_str_close(&buffer);
            } /* end if */
            HDfree(comment);
        }
    }
} /* end dump_comment() */

/*-------------------------------------------------------------------------
 * Function:    dump_attribute
 *
 * Purpose:     Dump the attribute.
 *
 * Return:      void
 *
 * In/Out:      h5tools_context_t *ctx
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_attribute(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, const char *attr_name, hid_t attr_id,
        int display_index, int display_char)
{
    h5tools_str_t buffer;          /* string into which to render   */
    size_t        ncols = 80;      /* available output width        */
    hsize_t       curr_pos = ctx->sm_pos;   /* total data element position   */
                                            /* pass to the prefix in h5tools_simple_prefix the total position
                                             * instead of the current stripmine position i; this is necessary
                                             * to print the array indices
                                             */

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, curr_pos, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s \"%s\" %s",
            h5tools_dump_header_format->attributebegin, attr_name,
            h5tools_dump_header_format->attributeblockbegin);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    if(attr_id < 0) {
        error_msg("unable to open attribute \"%s\"\n", attr_name);
    }
    else {
        hid_t type, space;

        ctx->indent_level++;

        type = H5Aget_type(attr_id);
        h5tools_dump_datatype(stream, info, ctx, type);

        space = H5Aget_space(attr_id);
        h5tools_dump_dataspace(stream, info, ctx, space);

        if(oid_output)
            h5tools_dump_oid(stream, info, ctx, attr_id);

        if(data_output || attr_data_output)
            h5tools_dump_data(stream, info, ctx, attr_id, FALSE, NULL, display_index, display_char);

        ctx->indent_level--;

        H5Tclose(type);
        H5Sclose(space);
        H5Aclose(attr_id);
    }

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

    h5tools_str_reset(&buffer);

    if (HDstrlen(h5tools_dump_header_format->attributeblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->attributeblockend);
        if (HDstrlen(h5tools_dump_header_format->attributeend))
            h5tools_str_append(&buffer, " ");
    }
    if (HDstrlen(h5tools_dump_header_format->attributeend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->attributeend);

    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_dims
 *
 * Purpose:     Dump the dimensions handed to it in a comma separated list
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 27. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5tools_print_dims(h5tools_str_t *buffer, hsize_t *s, int dims)
{
    int i;

    for (i = 0; i < dims; i++) {
        h5tools_str_append(buffer, HSIZE_T_FORMAT, s[i]);

        if (i + 1 != dims)
            h5tools_str_append(buffer, ", ");
    }
}

/*-------------------------------------------------------------------------
 * Function:    print_packed_bits
 *
 * Purpose:     Prints the packed bits offset and length
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
h5tools_print_packed_bits(h5tools_str_t *buffer, hid_t type)
{
    int     packed_bits_size = 0;

    hid_t n_type = h5tools_get_native_type(type);
    if(H5Tget_class(n_type)==H5T_INTEGER) {
        if(H5Tequal(n_type, H5T_NATIVE_SCHAR) == TRUE) {
            packed_bits_size = 8 * sizeof(char);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_UCHAR) == TRUE) {
            packed_bits_size = 8 * sizeof(unsigned char);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_SHORT) == TRUE) {
            packed_bits_size = 8 * sizeof(short);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_USHORT) == TRUE) {
            packed_bits_size = 8 * sizeof(unsigned short);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_INT) == TRUE) {
            packed_bits_size = 8 * sizeof(int);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_UINT) == TRUE) {
            packed_bits_size = 8 * sizeof(unsigned int);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_LONG) == TRUE) {
            packed_bits_size = 8 * sizeof(long);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_ULONG) == TRUE) {
            packed_bits_size = 8 * sizeof(unsigned long);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_LLONG) == TRUE) {
            packed_bits_size = 8 * sizeof(long long);
        }
        else if(H5Tequal(n_type, H5T_NATIVE_ULLONG) == TRUE) {
            packed_bits_size = 8 * sizeof(unsigned long long);
        }
        else
            error_msg("Packed Bit not valid for this datatype");
    }

    if ((packed_bits_size>0) && (packed_data_offset + packed_data_length) > packed_bits_size) {
        error_msg("Packed Bit offset+length value(%d) too large. Max is %d\n", packed_data_offset+packed_data_length, packed_bits_size);
        packed_data_mask = 0;
    };
    h5tools_str_append(buffer, "%s %s=%d %s=%d", PACKED_BITS, PACKED_OFFSET, packed_data_offset, PACKED_LENGTH, packed_data_length);
}

/*-------------------------------------------------------------------------
 * Function:    dump_subsetting_header
 *
 * Purpose:     Dump the subsetting header like specified in the DDL.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 27. February 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_subsetting_header(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, struct subset_t *sset, int dims)
{
    h5tools_str_t buffer;          /* string into which to render   */
    hsize_t       curr_pos = 0;        /* total data element position   */
    size_t        ncols = 80;      /* available output width        */

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));
    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->subsettingbegin, h5tools_dump_header_format->subsettingblockbegin);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level++;

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->startbegin, h5tools_dump_header_format->startblockbegin);
    h5tools_print_dims(&buffer, sset->start.data, dims);
    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->startend, h5tools_dump_header_format->startblockend);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->stridebegin, h5tools_dump_header_format->strideblockbegin);
    h5tools_print_dims(&buffer, sset->stride.data, dims);
    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->strideend, h5tools_dump_header_format->strideblockend);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->countbegin, h5tools_dump_header_format->countblockbegin);

    if(sset->count.data)
        h5tools_print_dims(&buffer, sset->count.data, dims);
    else
        h5tools_str_append(&buffer, "DEFAULT");

    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->countend, h5tools_dump_header_format->countblockend);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->blockbegin, h5tools_dump_header_format->blockblockbegin);

    if(sset->block.data)
        h5tools_print_dims(&buffer, sset->block.data, dims);
    else
        h5tools_str_append(&buffer, "DEFAULT");

    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->blockend, h5tools_dump_header_format->blockblockend);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level--;

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_data
 *
 * Purpose:     Dump attribute or dataset data
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_data(FILE *stream, const h5tool_format_t *info,
        h5tools_context_t *ctx, hid_t obj_id, int obj_data, struct subset_t *sset,
        int display_index, int display_char)
{
    H5S_class_t space_type;
    int         ndims;
    int         i;
    hid_t       space;
    hid_t       type;
    hid_t       p_type;
    hsize_t     size[64];
    hsize_t     alloc_size;
    hsize_t     nelmts = 1;
    int         status = -1;
    void       *buf = NULL;
    h5tools_str_t buffer;          /* string into which to render   */
    hsize_t       curr_pos = 0;        /* total data element position   */
    size_t        ncols = 80;      /* available output width        */
    h5tool_format_t     string_dataformat;
    h5tool_format_t     outputformat;

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));
    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    outputformat = *info;
    string_dataformat = *info;
    /* print the matrix indices */
    string_dataformat.pindex = display_index;

    /* do not print indices for regions */
    if(obj_data) {
        hid_t f_type = H5Dget_type(obj_id);

        if (H5Tequal(f_type, H5T_STD_REF_DSETREG)) {
            /* For the region option, correct the display of indices */
            if (region_output) {
                if (!string_dataformat.pindex) {
                    string_dataformat.idx_fmt   = "";
                    string_dataformat.idx_n_fmt = "";
                    string_dataformat.idx_sep   = "";
                    string_dataformat.line_pre  = "";
                }
            }
            else
                string_dataformat.pindex = 0;
        }
        H5Tclose(f_type);
    }

    if (string_dataformat.pindex) {
        string_dataformat.idx_fmt   = "(%s): ";
        string_dataformat.idx_n_fmt = HSIZE_T_FORMAT;
        string_dataformat.idx_sep   = ",";
        string_dataformat.line_pre  = "%s";
    }
    info = &string_dataformat;

    if (sset && obj_data) {
        hid_t f_space = H5Dget_space(obj_id);

        h5tools_dump_subsetting_header(stream, &outputformat, ctx, sset, H5Sget_simple_extent_ndims(f_space));
        H5Sclose(f_space);

        ctx->indent_level++;
    }

    ctx->need_prefix = TRUE;
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->databegin, h5tools_dump_header_format->datablockbegin);
    h5tools_render_element(stream, &outputformat, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    /* Print all the values. */
    if(obj_data) {
        h5tools_context_t datactx = *ctx;            /* print context  */
        hid_t               f_type = H5Dget_type(obj_id);

        if((display_char && H5Tget_size(f_type) == 1) && (H5Tget_class(f_type) == H5T_INTEGER)) {
            /*
             * Print 1-byte integer data as an ASCII character string
             * instead of integers if the `-r' or `--string' command-line
             * option was given.
             *
             * We don't want to modify the global dataformat, so make a
             * copy of it instead.
             */
            string_dataformat = *info;
            string_dataformat.idx_fmt = "\"";
            info = &string_dataformat;
            datactx.indent_level++;
            datactx.need_prefix = TRUE;
            h5tools_simple_prefix(stream, info, &datactx, (hsize_t)0, 0);

            string_dataformat = *info;
            string_dataformat.idx_fmt = "\"";
            string_dataformat.line_multi_new = 1;
            string_dataformat.str_repeat = 8;
            string_dataformat.ascii = TRUE;
            string_dataformat.elmt_suf1 = "";
            string_dataformat.elmt_suf2 = "";
            string_dataformat.line_suf = "\"";
            info = &string_dataformat;
        }
        else
            datactx.need_prefix = TRUE;
        status = h5tools_dump_dset(stream, info, &datactx, obj_id, sset);
        if((display_char && H5Tget_size(f_type) == 1) && (H5Tget_class(f_type) == H5T_INTEGER)) {
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"");
            h5tools_render_element(stream, &outputformat, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
        }
        H5Tclose(f_type);
    }
    else {
        h5tools_context_t datactx = *ctx;            /* print context  */
        /* need to call h5tools_dump_mem for the attribute data */
        space = H5Aget_space(obj_id);
        space_type = H5Sget_simple_extent_type(space);
        if(space_type == H5S_NULL || space_type == H5S_NO_CLASS) {
            status = SUCCEED;
        }
        else {
            /* VL data special information */
            unsigned int        vl_data = 0; /* contains VL datatypes */

            type = H5Aget_type(obj_id);
            p_type = h5tools_get_native_type(type);

            ndims = H5Sget_simple_extent_dims(space, size, NULL);

            /* Check if we have VL data in the dataset's datatype */
            if (h5tools_detect_vlen(p_type) == TRUE)
                vl_data = TRUE;

            for (i = 0; i < ndims; i++)
                nelmts *= size[i];

            alloc_size = nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type));
            HDassert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/

            if(alloc_size) {
                buf = HDmalloc((size_t)alloc_size);
                HDassert(buf);

                if (H5Aread(obj_id, p_type, buf) >= 0) {
                    if (display_char && H5Tget_size(type) == 1 && H5Tget_class(type) == H5T_INTEGER) {
                        /*
                         * Print 1-byte integer data as an ASCII character string
                         * instead of integers if the `-r' or `--string' command-line
                         * option was given.
                         *
                         * We don't want to modify the global dataformat, so make a
                         * copy of it instead.
                         */
                        string_dataformat = *info;
                        string_dataformat.idx_fmt = "\"";
                        info = &string_dataformat;
                        datactx.indent_level++;
                        datactx.need_prefix = TRUE;
                        h5tools_simple_prefix(stream, info, &datactx, (hsize_t)0, 0);

                        string_dataformat = *info;
                        string_dataformat.idx_fmt = "\"";
                        string_dataformat.line_multi_new = 1;
                        string_dataformat.str_repeat = 8;
                        string_dataformat.ascii = TRUE;
                        string_dataformat.elmt_suf1 = "";
                        string_dataformat.elmt_suf2 = "";
                        string_dataformat.line_suf = "\"";
                        info = &string_dataformat;
                    }
                    else
                        datactx.need_prefix = TRUE;
                }

                status = h5tools_dump_mem(stream, info, &datactx, obj_id, p_type, space, buf);
                if (display_char && H5Tget_size(type) == 1 && H5Tget_class(type) == H5T_INTEGER) {
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "\"");
                    h5tools_render_element(stream, &outputformat, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
                }

                /* Reclaim any VL memory, if necessary */
                if (vl_data)
                    H5Dvlen_reclaim(p_type, space, H5P_DEFAULT, buf);

                HDfree(buf);
            }
            else
                status = SUCCEED;

            H5Tclose(p_type);
            H5Tclose(type);
        }
        H5Sclose(space);
    }

    if (status == FAIL) {
        error_msg("unable to print data\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    ctx->need_prefix = TRUE;
    h5tools_simple_prefix(stream, &outputformat, ctx, (hsize_t)0, 0);

    h5tools_str_reset(&buffer);
    if(HDstrlen(h5tools_dump_header_format->datablockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datablockend);
        if(HDstrlen(h5tools_dump_header_format->dataend))
            h5tools_str_append(&buffer, " ");
    }
    if(HDstrlen(h5tools_dump_header_format->dataend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->dataend);
    h5tools_render_element(stream, &outputformat, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    if (sset && obj_data) {
        ctx->indent_level--;

        ctx->need_prefix = TRUE;
        h5tools_simple_prefix(stream, &outputformat, ctx, (hsize_t)0, 0);

        h5tools_str_reset(&buffer);
        if(HDstrlen(h5tools_dump_header_format->subsettingblockend)) {
            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->subsettingblockend);
            if(HDstrlen(h5tools_dump_header_format->subsettingend))
                h5tools_str_append(&buffer, " ");
        }
        if(HDstrlen(h5tools_dump_header_format->subsettingend))
            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->subsettingend);
        h5tools_render_element(stream, &outputformat, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
    }

    h5tools_str_close(&buffer);
}

