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
 * Purpose: A library for displaying the values of a dataset in a human
 *  readable format.
 */

#include "h5tools.h"
#include "h5tools_dump.h"
#include "h5tools_ref.h"
#include "h5tools_utils.h"
#include "H5private.h"

h5tool_format_t h5tools_dataformat = {
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

    "[ ", /*arr_pre */
    ",",  /*arr_sep */
    " ]", /*arr_suf */
    1,    /*arr_linebreak */

    "",    /*cmpd_name */
    ",\n", /*cmpd_sep */
    "{",   /*cmpd_pre */
    "}",   /*cmpd_suf */
    "\n",  /*cmpd_end */
    NULL,  /* cmpd_listv */

    ", ", /*vlen_sep */
    "(",  /*vlen_pre */
    ")",  /*vlen_suf */
    "",   /*vlen_end */

    "%s", /*elmt_fmt */
    ",",  /*elmt_suf1 */
    " ",  /*elmt_suf2 */

    "", /*idx_n_fmt */
    "", /*idx_sep */
    "", /*idx_fmt */

    80,
    /*line_ncols */ /*standard default columns */
    0,              /*line_per_line */
    "",             /*line_pre */
    "%s",           /*line_1st */
    "%s",           /*line_cont */
    "",             /*line_suf */
    "",             /*line_sep */
    1,              /*line_multi_new */
    "   ",          /*line_indent */

    1, /*skip_first */

    1,              /*obj_hidefileno */
    " %" PRIuHADDR, /*obj_format */

    1,             /*dset_hidefileno */
    "DATASET %s ", /*dset_format */
    "%s",          /*dset_blockformat_pre */
    "%s",          /*dset_ptformat_pre */
    "%s",          /*dset_ptformat */
    1,             /*array indices */
    1              /*escape non printable characters */
};

const h5tools_dump_header_t h5tools_standardformat = {
    "standardformat",  /*name */
    "HDF5",            /*filebegin */
    "",                /*fileend */
    SUPER_BLOCK,       /*bootblockbegin */
    "",                /*bootblockend */
    H5_TOOLS_GROUP,    /*groupbegin */
    "",                /*groupend */
    H5_TOOLS_DATASET,  /*datasetbegin */
    "",                /*datasetend */
    ATTRIBUTE,         /*attributebegin */
    "",                /*attributeend */
    H5_TOOLS_DATATYPE, /*datatypebegin */
    "",                /*datatypeend */
    DATASPACE,         /*dataspacebegin */
    "",                /*dataspaceend */
    DATA,              /*databegin */
    "",                /*dataend */
    SOFTLINK,          /*softlinkbegin */
    "",                /*softlinkend */
    EXTLINK,           /*extlinkbegin */
    "",                /*extlinkend */
    UDLINK,            /*udlinkbegin */
    "",                /*udlinkend */
    SUBSET,            /*subsettingbegin */
    "",                /*subsettingend */
    START,             /*startbegin */
    "",                /*startend */
    STRIDE,            /*stridebegin */
    "",                /*strideend */
    COUNT,             /*countbegin */
    "",                /*countend */
    BLOCK,             /*blockbegin */
    "",                /*blockend */

    "{",                /*fileblockbegin */
    "}",                /*fileblockend */
    "{",                /*bootblockblockbegin */
    "}",                /*bootblockblockend */
    "{",                /*groupblockbegin */
    "}",                /*groupblockend */
    "{",                /*datasetblockbegin */
    "}",                /*datasetblockend */
    "{",                /*attributeblockbegin */
    "}",                /*attributeblockend */
    "",                 /*datatypeblockbegin */
    "",                 /*datatypeblockend */
    "",                 /*dataspaceblockbegin */
    "",                 /*dataspaceblockend */
    "{",                /*datablockbegin */
    "}",                /*datablockend */
    "{",                /*softlinkblockbegin */
    "}",                /*softlinkblockend */
    "{",                /*extlinkblockbegin */
    "}",                /*extlinkblockend */
    "{",                /*udlinkblockbegin */
    "}",                /*udlinkblockend */
    "H5T_ARRAY { ",     /*arrblockbegin */
    " }",               /*arrblockend */
    "H5T_COMPOUND {",   /*cmpdblockbegin */
    "}",                /*cmpdblockend */
    "H5T_ENUM {",       /*enumblockbegin */
    "}",                /*enumblockend */
    "H5T_OPAQUE {",     /*opaqblockbegin */
    "}",                /*opaqblockend */
    "H5T_REFERENCE { ", /*refblockbegin */
    " }",               /*refblockend */
    "H5T_STRING {",     /*strblockbegin */
    "}",                /*strblockend */
    "H5T_VLEN { ",      /*vlenblockbegin */
    " }",               /*vlenblockend */
    "{",                /*structblockbegin */
    "}",                /*structblockend */
    "{",                /*subsettingblockbegin */
    "}",                /*subsettingblockend */
    "(",                /*startblockbegin */
    ");",               /*startblockend */
    "(",                /*strideblockbegin */
    ");",               /*strideblockend */
    "(",                /*countblockbegin */
    ");",               /*countblockend */
    "(",                /*blockblockbegin */
    ");",               /*blockblockend */

    "",  /*dataspacedescriptionbegin */
    "",  /*dataspacedescriptionend */
    "(", /*dataspacedimbegin */
    ")", /*dataspacedimend */

    "",   /*virtualselectionbegin */
    "",   /*virtualselectionend */
    "{",  /*virtualselectionblockbegin */
    "}",  /*virtualselectionblockend */
    "\"", /*virtualfilenamebeginbegin */
    "\"", /*virtualfilenamebeginend */
    "\"", /*virtualdatasetnamebegin */
    "\"", /*virtualdtatasetnameend */
};

const h5tools_dump_header_t *h5tools_dump_header_format;
table_t                     *h5dump_type_table = NULL; /* type table reference for datatype dump  */

/* local prototypes */

static int h5tools_print_region_data_blocks(hid_t region_id, FILE *stream, const h5tool_format_t *info,
                                            h5tools_context_t *cur_ctx,
                                            h5tools_str_t     *buffer, /* string into which to render */
                                            size_t ncols, unsigned ndims, hid_t type_id, hsize_t nblocks,
                                            hsize_t *ptdata);

static int h5tools_print_region_data_points(hid_t region_space, hid_t region_id, FILE *stream,
                                            const h5tool_format_t *info, h5tools_context_t *cur_ctx,
                                            h5tools_str_t *buffer, size_t ncols, unsigned ndims,
                                            hid_t type_id, hsize_t npoints, hsize_t *ptdata);

void h5tools_print_dims(h5tools_str_t *buffer, hsize_t *s, int dims);

void h5tools_dump_subsetting_header(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                                    int dims);

static void h5tools_print_virtual_selection(hid_t vspace, FILE *stream, const h5tool_format_t *info,
                                            h5tools_context_t *ctx,      /* in,out */
                                            h5tools_str_t     *buffer,   /* string into which to render */
                                            hsize_t           *curr_pos, /* total data element position */
                                            size_t             ncols);

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
 *
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_simple_data(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, /* in,out */
                         hid_t container, unsigned flags, hsize_t nelmts, hid_t type, void *_mem)
{
    unsigned char *mem = (unsigned char *)_mem;
    hsize_t        i;    /* element counter  */
    size_t         size; /* size of each datum  */
    bool           dimension_break = true;
    size_t         ncols           = 80; /* available output width */
    h5tools_str_t  buffer;               /* string into which to render */
    hsize_t        curr_pos     = 0;     /* total data element position   */
    hsize_t        elmt_counter = 0;     /* counts the # elements printed.
                                          * I (ptl?) needed something that
                                          * isn't going to get reset when a new
                                          * line is formed. I'm going to use
                                          * this var to count elements and
                                          * break after we see a number equal
                                          * to the ctx->size_last_dim.   */
    int ret_value = 0;

    H5TOOLS_START_DEBUG(" file=%p", (void *)stream);
    H5TOOLS_DEBUG("rawdata file=%p", (void *)rawdatastream);
    /* binary dump */
    if (bin_output && (rawdatastream != NULL)) {
        H5TOOLS_DEBUG("render_bin_output");
        if (render_bin_output(rawdatastream, container, type, _mem, nelmts) < 0) {
            PRINTVALSTREAM(rawoutstream, "\nError in writing binary stream\n");
        }
    } /* end if */
    else {
        /* setup */
        memset(&buffer, 0, sizeof(h5tools_str_t));
        size = H5Tget_size(type);
        H5TOOLS_DEBUG("type size is %ld", size);

        if (info->line_ncols > 0)
            ncols = info->line_ncols;

        /* pass to the prefix in h5tools_simple_prefix the total position
         * instead of the current stripmine position i; this is necessary
         * to print the array indices
         */
        curr_pos = ctx->sm_pos;

        H5TOOLS_DEBUG("data render start:%ld", nelmts);
        for (i = 0; i < nelmts; i++, ctx->cur_elmt++, elmt_counter++) {
            void *memref = mem + i * size;

            /* Render the data element begin*/
            h5tools_str_reset(&buffer);
            h5tools_str_sprint(&buffer, info, container, type, memref, ctx);

            if (i + 1 < nelmts || (flags & END_OF_DATA) == 0)
                h5tools_str_append(&buffer, "%s", OPT(info->elmt_suf1, ","));

            dimension_break =
                h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, ncols, i, elmt_counter);
            /* Render the data element end*/
            if (false == dimension_break)
                elmt_counter = 0;
        } /* end for (i = 0; i < nelmts... */
        H5TOOLS_DEBUG("data render finish");

        h5tools_str_close(&buffer);
    } /* else bin */

    H5TOOLS_ENDDEBUG("exit");
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Audience:    Public
 * Chapter:     H5Tools Library
 * Purpose: Print some values from an attribute referenced by object reference.
 *
 * Description:
 *      This is a special case subfunction to dump an attribute reference.
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
bool
h5tools_dump_region_attribute(hid_t region_id, FILE *stream, const h5tool_format_t *info,
                              h5tools_context_t *ctx,                    /* in,out */
                              h5tools_str_t     *buffer,                 /* string into which to render */
                              hsize_t           *curr_pos,               /* total data element position */
                              size_t ncols, hsize_t region_elmt_counter, /* element counter */
                              hsize_t elmt_counter)
{
    bool            dimension_break = true;
    hid_t           atype           = H5I_INVALID_HID;
    hid_t           type_id         = H5I_INVALID_HID;
    hid_t           region_space    = H5I_INVALID_HID;
    h5tool_format_t outputformat; /* Use to disable prefix for DATA attribute display */
    bool            past_catch = false;
    bool            ret_value  = true;

    assert(info);
    assert(ctx);
    assert(buffer);

    outputformat           = *info;
    outputformat.idx_fmt   = "";
    outputformat.idx_n_fmt = "";
    outputformat.idx_sep   = "";
    outputformat.line_pre  = "";

    H5TOOLS_DEBUG("enter file=%p", (void *)stream);
    H5TOOLS_DEBUG("rawdata file=%p", (void *)rawdatastream);

    /* Render the region { element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, " {");
    dimension_break = h5tools_render_element(stream, &outputformat, ctx, buffer, curr_pos, ncols,
                                             region_elmt_counter, elmt_counter);
    /* Render the region { element end */

    if ((region_space = H5Aget_space(region_id)) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Aget_space failed");
    if ((atype = H5Aget_type(region_id)) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Aget_type failed");
    if ((type_id = H5Tget_native_type(atype, H5T_DIR_DEFAULT)) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Tget_native_type failed");

    ctx->indent_level++;
    ctx->need_prefix = true;

    /* Render the datatype element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ", h5tools_dump_header_format->datatypebegin,
                       h5tools_dump_header_format->datatypeblockbegin);

    ctx->need_prefix = true;
    ctx->indent_level++;
    h5tools_print_datatype(stream, buffer, info, ctx, atype, true);
    ctx->indent_level--;

    if (strlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (strlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeend);

    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the datatype element end */

    ctx->need_prefix = true;

    /* Render the dataspace element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s ", h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(buffer, region_space);

    if (strlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (strlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->dataspaceend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);

    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the dataspace element end */

    if (region_output) {
        ctx->need_prefix = true;

        h5tools_dump_data(stream, &outputformat, ctx, region_id, false);
    }

done:
    if (H5Tclose(type_id) < 0)
        H5TOOLS_ERROR(dimension_break, "H5Tclose failed");

    if (H5Tclose(atype) < 0)
        H5TOOLS_ERROR(dimension_break, "H5Tclose failed");

    if (H5Sclose(region_space) < 0)
        H5TOOLS_ERROR(dimension_break, "H5Sclose failed");

    ctx->indent_level--;
    ctx->need_prefix = true;

    /* Render the region } element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "}");
    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region } element end */

    H5_LEAVE(dimension_break);

    CATCH

    H5TOOLS_ENDDEBUG(" ");
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
 *      The function returns FAIL if there was an error, otherwise SUCCEED
 *
 * Parameters Description:
 *      h5tools_str_t *buffer is the string into which to render
 *      size_t ncols
 *      int ndims is the number of dimensions of the region element
 *      hssize_t nblocks is the number of blocks in the region
 *-------------------------------------------------------------------------
 */
static int
h5tools_print_region_data_blocks(hid_t region_id, FILE *stream, const h5tool_format_t *info,
                                 h5tools_context_t *cur_ctx,
                                 h5tools_str_t     *buffer, /* string into which to render */
                                 size_t ncols, unsigned ndims, hid_t type_id, hsize_t nblocks,
                                 hsize_t *ptdata)
{
    bool              dimension_break = true;
    hsize_t          *dims1           = NULL;
    hsize_t          *start           = NULL;
    hsize_t          *count           = NULL;
    hsize_t           blkndx;
    hsize_t           total_size[H5S_MAX_RANK];
    hsize_t           elmtno; /* element index  */
    hsize_t           curr_pos = 0;
    unsigned int      region_flags; /* buffer extent flags */
    hsize_t           numelem;
    hsize_t           numindex;
    unsigned          indx;
    unsigned          jndx;
    bool              past_catch = false;
    size_t            type_size;
    hid_t             mem_space = H5I_INVALID_HID;
    hid_t             sid1      = H5I_INVALID_HID;
    h5tools_context_t ctx;
    void             *region_buf = NULL;
    int               ret_value  = 0;

    assert(info);
    assert(cur_ctx);
    assert(buffer);
    assert(ptdata);

    memset(&ctx, 0, sizeof(ctx));

    H5TOOLS_START_DEBUG(" ");

    if ((type_size = H5Tget_size(type_id)) == 0)
        H5TOOLS_THROW(FAIL, "H5Tget_size failed");

    /* Get the dataspace of the dataset */
    if ((sid1 = H5Dget_space(region_id)) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Dget_space failed");

    /* Allocate space for the dimension array */
    if ((dims1 = (hsize_t *)malloc((size_t)(sizeof(hsize_t) * ndims))) == NULL)
        H5TOOLS_GOTO_ERROR(FAIL, "Could not allocate buffer for dims");

    /* find the dimensions of each data space from the block coordinates */
    numelem = 1;
    for (jndx = 0; jndx < ndims; jndx++) {
        dims1[jndx] = ptdata[jndx + ndims] - ptdata[jndx] + 1;
        numelem     = dims1[jndx] * numelem;
    }

    /* Create dataspace for reading buffer */
    if ((mem_space = H5Screate_simple((int)ndims, dims1, NULL)) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Screate_simple failed");

    if ((region_buf = malloc(type_size * (size_t)numelem)) == NULL)
        H5TOOLS_GOTO_ERROR(FAIL, "Could not allocate region buffer");

    /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
    /*          1   2        n      1   2        n                                       */
    if ((start = (hsize_t *)malloc(sizeof(hsize_t) * ndims)) == NULL)
        H5TOOLS_GOTO_ERROR(FAIL, "Could not allocate buffer for start");

    if ((count = (hsize_t *)malloc(sizeof(hsize_t) * ndims)) == NULL)
        H5TOOLS_GOTO_ERROR(FAIL, "Could not allocate buffer for count");

    curr_pos           = 0;
    ctx.indent_level   = cur_ctx->indent_level;
    ctx.cur_column     = cur_ctx->cur_column;
    ctx.prev_multiline = cur_ctx->prev_multiline;
    ctx.ndims          = ndims;
    for (blkndx = 0; blkndx < nblocks; blkndx++) {
        ctx.need_prefix = true;
        ctx.cur_elmt    = 0;
        for (indx = 0; indx < ndims; indx++) {
            start[indx] = ptdata[indx + blkndx * ndims * 2];
            count[indx] = dims1[indx];
        }

        if (H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL) >= 0) {
            if (H5Dread(region_id, type_id, mem_space, sid1, H5P_DEFAULT, region_buf) >= 0) {
                ctx.indent_level++;
                if (H5Sget_simple_extent_dims(mem_space, total_size, NULL) >= 0) {
                    /* assume entire data space to be printed */
                    init_acc_pos(ctx.ndims, total_size, ctx.acc, ctx.pos, ctx.p_min_idx);

                    /* reset data space to be printed */
                    for (indx = 0; indx < (unsigned)ctx.ndims; indx++)
                        ctx.p_min_idx[indx] = start[indx];

                    /* print the data */
                    region_flags = START_OF_DATA;
                    if (blkndx == nblocks - 1)
                        region_flags |= END_OF_DATA;

                    for (indx = 0; indx < (unsigned)ctx.ndims; indx++)
                        ctx.p_max_idx[indx] = dims1[indx];

                    curr_pos          = 0;
                    ctx.sm_pos        = blkndx * 2 * ndims;
                    ctx.size_last_dim = dims1[ndims - 1];

                    h5tools_region_simple_prefix(stream, info, &ctx, curr_pos, ptdata, 0);

                    H5TOOLS_DEBUG("data render start:%ld", numelem);
                    elmtno = 0;
                    for (numindex = 0; numindex < numelem; numindex++, elmtno++, ctx.cur_elmt++) {
                        /* Render the region data element begin */
                        h5tools_str_reset(buffer);

                        h5tools_str_append(buffer, "%s", numindex ? OPTIONAL_LINE_BREAK "" : "");
                        h5tools_str_sprint(buffer, info, region_id, type_id,
                                           ((char *)region_buf + numindex * type_size), &ctx);

                        if (numindex + 1 < numelem || (region_flags & END_OF_DATA) == 0)
                            h5tools_str_append(buffer, "%s", OPT(info->elmt_suf1, ","));

                        dimension_break = h5tools_render_region_element(stream, info, &ctx, buffer, &curr_pos,
                                                                        ncols, ptdata, numindex, elmtno);
                        /* Render the region data element end */

                        if (false == dimension_break)
                            elmtno = 0;
                    } /* end for (numindex = 0; numindex < numelem; numindex++, elmtno++, ctx.cur_elmt++) */
                }
                else {
                    H5TOOLS_ERROR(FAIL, "H5Sget_simple_extent_dims failed");
                }
                ctx.indent_level--;
            }
            else {
                H5TOOLS_ERROR(FAIL, "H5Dread failed");
            }
        }
        else {
            H5TOOLS_ERROR(FAIL, "H5Sselect_hyperslab failed");
        }

    } /* end for (blkndx = 0; blkndx < nblocks; blkndx++) */

done:
    free(start);
    free(count);
    free(region_buf);
    free(dims1);

    if (H5Sclose(mem_space) < 0)
        H5TOOLS_ERROR(FAIL, "H5Sclose failed");
    if (H5Sclose(sid1) < 0)
        H5TOOLS_ERROR(FAIL, "H5Sclose failed");
    CATCH
    H5TOOLS_ENDDEBUG(" ");

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
bool
h5tools_dump_region_data_blocks(hid_t region_space, hid_t region_id, FILE *stream,
                                const h5tool_format_t *info, h5tools_context_t *ctx, /* in,out */
                                h5tools_str_t *buffer,                     /* string into which to render */
                                hsize_t       *curr_pos,                   /* total data element position */
                                size_t ncols, hsize_t region_elmt_counter, /* element counter */
                                hsize_t elmt_counter)
{
    bool            dimension_break = true;
    hssize_t        snblocks;
    hsize_t         nblocks;
    hsize_t         alloc_size;
    hsize_t        *ptdata = NULL;
    int             sndims;
    unsigned        ndims;
    hsize_t         indx;
    hid_t           dtype   = H5I_INVALID_HID;
    hid_t           type_id = H5I_INVALID_HID;
    h5tool_format_t outputformat; /* Use to disable prefix for DATA attribute display */
    bool            past_catch = false;
    bool            ret_value  = true;

    assert(info);
    assert(ctx);
    assert(buffer);

    H5TOOLS_START_DEBUG(" ");
    outputformat           = *info;
    outputformat.idx_fmt   = "";
    outputformat.idx_n_fmt = "";
    outputformat.idx_sep   = "";
    outputformat.line_pre  = "";

    if ((snblocks = H5Sget_select_hyper_nblocks(region_space)) <= 0)
        H5TOOLS_THROW(dimension_break, "H5Sget_select_hyper_nblocks failed");
    nblocks = (hsize_t)snblocks;

    /* Print block information */
    if ((sndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5TOOLS_THROW(dimension_break, "H5Sget_simple_extent_ndims failed");
    ndims = (unsigned)sndims;

    H5TOOLS_DEBUG("enter ndims=%d", ndims);

    /* Render the region { element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, " {");
    dimension_break = h5tools_render_element(stream, &outputformat, ctx, buffer, curr_pos, ncols,
                                             region_elmt_counter, elmt_counter);
    /* Render the region { element end */

    ctx->indent_level++;
    ctx->need_prefix = true;

    /* Render the region datatype info and indices element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "REGION_TYPE BLOCK  ");

    alloc_size = nblocks * ndims * 2 * sizeof(ptdata[0]);
    assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/
    if ((ptdata = (hsize_t *)malloc((size_t)alloc_size)) == NULL)
        H5TOOLS_GOTO_ERROR(dimension_break, "Could not allocate buffer for ptdata");

    if (H5Sget_select_hyper_blocklist(region_space, (hsize_t)0, nblocks, ptdata) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Rget_select_hyper_blocklist failed");

    for (indx = 0; indx < nblocks; indx++) {
        unsigned loop_indx;

        H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
        h5tools_str_append(buffer, outputformat.dset_blockformat_pre, indx ? "," OPTIONAL_LINE_BREAK " " : "",
                           (unsigned long)indx);
        H5_GCC_CLANG_DIAG_ON("format-nonliteral")

        /* Start coordinates and opposite corner */
        for (loop_indx = 0; loop_indx < ndims; loop_indx++)
            h5tools_str_append(buffer, "%s%" PRIuHSIZE, loop_indx ? "," : "(",
                               ptdata[indx * 2 * ndims + loop_indx]);

        for (loop_indx = 0; loop_indx < ndims; loop_indx++)
            h5tools_str_append(buffer, "%s%" PRIuHSIZE, loop_indx ? "," : ")-(",
                               ptdata[indx * 2 * ndims + loop_indx + ndims]);

        h5tools_str_append(buffer, ")");
    } /* end for (indx = 0; indx < nblocks; indx++) */

    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region datatype info and indices element end */

    ctx->need_prefix = true;

    if ((dtype = H5Dget_type(region_id)) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Dget_type failed");
    if ((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Tget_native_type failed");

    /* Render the datatype element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ", h5tools_dump_header_format->datatypebegin,
                       h5tools_dump_header_format->datatypeblockbegin);

    ctx->indent_level++;
    h5tools_print_datatype(stream, buffer, info, ctx, dtype, true);
    ctx->indent_level--;

    if (strlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (strlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeend);

    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the datatype element end */

    ctx->need_prefix = true;

    /* Render the dataspace element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s ", h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(buffer, region_space);

    if (strlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (strlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->dataspaceend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);

    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the dataspace element end */

    if (region_output) {
        ctx->need_prefix = true;

        /* Render the databegin element begin */
        h5tools_str_reset(buffer);
        h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->databegin,
                           h5tools_dump_header_format->datablockbegin);
        dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols,
                                                 region_elmt_counter, elmt_counter);
        /* Render the databegin element end */

        ctx->need_prefix = true;

        h5tools_print_region_data_blocks(region_id, rawdatastream, info, ctx, buffer, ncols, ndims, type_id,
                                         nblocks, ptdata);
    }

done:
    free(ptdata);

    if (type_id > 0 && H5Tclose(type_id) < 0)
        H5TOOLS_ERROR(dimension_break, "H5Tclose failed");

    if (dtype > 0 && H5Tclose(dtype) < 0)
        H5TOOLS_ERROR(dimension_break, "H5Tclose failed");

    if (region_output) {
        ctx->need_prefix = true;

        /* Render the dataend element begin */
        h5tools_str_reset(buffer);
        if (strlen(h5tools_dump_header_format->datablockend)) {
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datablockend);
            if (strlen(h5tools_dump_header_format->dataend))
                h5tools_str_append(buffer, " ");
        }

        if (strlen(h5tools_dump_header_format->dataend))
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataend);
        dimension_break = h5tools_render_element(stream, &outputformat, ctx, buffer, curr_pos, ncols,
                                                 region_elmt_counter, elmt_counter);
        /* Render the dataend element end */
    }

    ctx->indent_level--;
    ctx->need_prefix = true;

    /* Render the region } element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "}");
    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region } element end */

    H5_LEAVE(dimension_break);

    CATCH

    H5TOOLS_ENDDEBUG(" ");

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
static int
h5tools_print_region_data_points(hid_t region_space, hid_t region_id, FILE *stream,
                                 const h5tool_format_t *info, h5tools_context_t *cur_ctx,
                                 h5tools_str_t *buffer, size_t ncols, unsigned ndims, hid_t type_id,
                                 hsize_t npoints, hsize_t *ptdata)
{
    bool              dimension_break = true;
    hsize_t          *dims1           = NULL;
    hsize_t           elmtno; /* element index  */
    hsize_t           curr_pos = 0;
    hsize_t           total_size[H5S_MAX_RANK];
    hsize_t           jndx;
    unsigned          indx;
    size_t            type_size;
    unsigned int      region_flags; /* buffer extent flags */
    hid_t             mem_space  = H5I_INVALID_HID;
    void             *region_buf = NULL;
    h5tools_context_t ctx;
    bool              past_catch = false;
    int               ret_value  = 0;

    assert(info);
    assert(cur_ctx);
    assert(buffer);
    assert(ptdata);
    assert(ndims > 0);

    H5TOOLS_START_DEBUG(" ");

    memset(&ctx, 0, sizeof(ctx));
    /* Allocate space for the dimension array */
    if ((dims1 = (hsize_t *)malloc(sizeof(hsize_t) * ndims)) == NULL)
        H5TOOLS_THROW((-1), "Could not allocate buffer for dims");

    dims1[0] = npoints;

    /* Create dataspace for reading buffer */
    if ((mem_space = H5Screate_simple(1, dims1, NULL)) < 0)
        H5TOOLS_THROW((-1), "H5Screate_simple failed");

    if ((type_size = H5Tget_size(type_id)) == 0)
        H5TOOLS_THROW((-1), "H5Tget_size failed");

    if ((region_buf = malloc(type_size * (size_t)npoints)) == NULL)
        H5TOOLS_THROW((-1), "Could not allocate buffer for region");

    curr_pos           = 0;
    ctx.indent_level   = cur_ctx->indent_level;
    ctx.cur_column     = cur_ctx->cur_column;
    ctx.prev_multiline = cur_ctx->prev_multiline;
    ctx.ndims          = ndims;

    if (H5Dread(region_id, type_id, mem_space, region_space, H5P_DEFAULT, region_buf) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Dread failed");

    H5TOOLS_DEBUG("data render start:%ld", npoints);
    elmtno = 0;
    for (jndx = 0; jndx < npoints; jndx++, elmtno++) {
        ctx.need_prefix = true;
        ctx.cur_elmt    = 0; /* points are always 0 */
        ctx.indent_level++;

        if (H5Sget_simple_extent_dims(mem_space, total_size, NULL) >= 0) {
            /* assume entire data space to be printed */
            init_acc_pos(ctx.ndims, total_size, ctx.acc, ctx.pos, ctx.p_min_idx);

            /* print the data */
            region_flags = START_OF_DATA;
            if (jndx == npoints - 1)
                region_flags |= END_OF_DATA;

            for (indx = 0; indx < ctx.ndims; indx++)
                ctx.p_max_idx[indx] = cur_ctx->p_max_idx[indx];

            ctx.sm_pos = jndx * ndims;
            if (ctx.ndims > 0)
                ctx.size_last_dim = ctx.p_max_idx[ctx.ndims - 1];
            else
                ctx.size_last_dim = 0;

            curr_pos = 0; /* points requires constant 0 */
            h5tools_region_simple_prefix(stream, info, &ctx, curr_pos, ptdata, 0);

            /* Render the point element begin */
            h5tools_str_reset(buffer);

            h5tools_str_append(buffer, "%s", jndx ? OPTIONAL_LINE_BREAK "" : "");
            h5tools_str_sprint(buffer, info, region_id, type_id, ((char *)region_buf + jndx * type_size),
                               &ctx);

            if (jndx + 1 < npoints || (region_flags & END_OF_DATA) == 0)
                h5tools_str_append(buffer, "%s", OPT(info->elmt_suf1, ","));

            dimension_break = h5tools_render_region_element(stream, info, &ctx, buffer, &curr_pos, ncols,
                                                            ptdata, (hsize_t)0, elmtno);
            /* Render the point element end */
            if (false == dimension_break)
                elmtno = 0;
        }
        else {
            H5TOOLS_ERROR((-1), "H5Sget_simple_extent_dims failed");
        }
        ctx.indent_level--;
    } /* end for (jndx = 0; jndx < npoints; jndx++, elmtno++) */

done:
    free(region_buf);
    CATCH
    free(dims1);

    if (H5Sclose(mem_space) < 0)
        H5TOOLS_ERROR((-1), "H5Sclose failed");

    H5TOOLS_ENDDEBUG(" ");

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
bool
h5tools_dump_region_data_points(hid_t region_space, hid_t region_id, FILE *stream,
                                const h5tool_format_t *info, h5tools_context_t *ctx, h5tools_str_t *buffer,
                                hsize_t *curr_pos, size_t ncols, hsize_t region_elmt_counter,
                                hsize_t elmt_counter)
{
    bool            dimension_break = true;
    hssize_t        snpoints;
    hsize_t         npoints;
    hsize_t         alloc_size;
    hsize_t        *ptdata;
    int             sndims;
    unsigned        ndims;
    hsize_t         indx;
    hid_t           dtype   = H5I_INVALID_HID;
    hid_t           type_id = H5I_INVALID_HID;
    h5tool_format_t outputformat; /* Use to disable prefix for DATA attribute display */
    bool            past_catch = false;
    bool            ret_value  = true;

    assert(info);
    assert(ctx);
    assert(buffer);

    H5TOOLS_START_DEBUG(" ");
    outputformat           = *info;
    outputformat.idx_fmt   = "";
    outputformat.idx_n_fmt = "";
    outputformat.idx_sep   = "";
    outputformat.line_pre  = "";

    if ((snpoints = H5Sget_select_elem_npoints(region_space)) <= 0)
        H5TOOLS_THROW(dimension_break, "H5Sget_select_elem_npoints failed");
    npoints = (hsize_t)snpoints;

    /* Allocate space for the dimension array */
    if ((sndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5TOOLS_THROW(dimension_break, "H5Sget_simple_extent_ndims failed");
    ndims = (unsigned)sndims;

    H5TOOLS_DEBUG("enter ndims=%d", ndims);

    /* Render the region { element begin */
    h5tools_str_reset(buffer);

    h5tools_str_append(buffer, "{");
    dimension_break = h5tools_render_element(stream, &outputformat, ctx, buffer, curr_pos, ncols,
                                             region_elmt_counter, elmt_counter);
    /* Render the region { element end */

    ctx->indent_level++;
    ctx->need_prefix = true;

    /* Render the region datatype info and indices element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "REGION_TYPE POINT  ");

    alloc_size = npoints * ndims * sizeof(ptdata[0]);
    assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/
    if (NULL == (ptdata = (hsize_t *)malloc((size_t)alloc_size)))
        H5TOOLS_GOTO_ERROR(dimension_break, "Could not allocate buffer for ptdata");

    if (H5Sget_select_elem_pointlist(region_space, (hsize_t)0, npoints, ptdata) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Sget_select_elem_pointlist failed");

    for (indx = 0; indx < npoints; indx++) {
        unsigned loop_indx;

        H5_GCC_CLANG_DIAG_OFF("format-nonliteral")
        h5tools_str_append(buffer, outputformat.dset_ptformat_pre, indx ? "," OPTIONAL_LINE_BREAK " " : "",
                           (unsigned long)indx);
        H5_GCC_CLANG_DIAG_ON("format-nonliteral")

        for (loop_indx = 0; loop_indx < ndims; loop_indx++)
            h5tools_str_append(buffer, "%s%" PRIuHSIZE, loop_indx ? "," : "(",
                               ptdata[indx * ndims + loop_indx]);

        h5tools_str_append(buffer, ")");
    } /* end for (indx = 0; indx < npoints; indx++) */

    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region datatype info and indices element end */

    ctx->need_prefix = true;

    if ((dtype = H5Dget_type(region_id)) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Dget_type failed");
    if ((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
        H5TOOLS_GOTO_ERROR(dimension_break, "H5Tget_native_type failed");

    /* Render the datatype element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s %s ", h5tools_dump_header_format->datatypebegin,
                       h5tools_dump_header_format->datatypeblockbegin);

    ctx->indent_level++;
    h5tools_print_datatype(stream, buffer, info, ctx, dtype, true);
    ctx->indent_level--;

    if (strlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (strlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datatypeend);

    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the datatype element end */

    ctx->need_prefix = true;

    /* Render the dataspace element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "%s ", h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(buffer, region_space);

    if (strlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (strlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->dataspaceend))
        h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataspaceblockend);

    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the dataspace element end */

    if (region_output) {
        ctx->need_prefix = true;

        /* Render the databegin element begin */
        h5tools_str_reset(buffer);

        h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->databegin,
                           h5tools_dump_header_format->datablockbegin);

        dimension_break = h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols,
                                                 region_elmt_counter, elmt_counter);

        ctx->need_prefix = true;

        h5tools_print_region_data_points(region_space, region_id, rawdatastream, info, ctx, buffer, ncols,
                                         ndims, type_id, npoints, ptdata);
    }

done:
    free(ptdata);

    if (type_id > 0 && H5Tclose(type_id) < 0)
        H5TOOLS_ERROR(dimension_break, "H5Tclose failed");

    if (dtype > 0 && H5Tclose(dtype) < 0)
        H5TOOLS_ERROR(dimension_break, "H5Tclose failed");

    if (region_output) {
        ctx->need_prefix = true;

        /* Render the dataend element begin */
        h5tools_str_reset(buffer);
        if (strlen(h5tools_dump_header_format->datablockend)) {
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->datablockend);
            if (strlen(h5tools_dump_header_format->dataend))
                h5tools_str_append(buffer, " ");
        }

        if (strlen(h5tools_dump_header_format->dataend))
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->dataend);
        dimension_break = h5tools_render_element(stream, &outputformat, ctx, buffer, curr_pos, ncols,
                                                 region_elmt_counter, elmt_counter);
        /* Render the dataend element end*/
    }

    ctx->indent_level--;
    ctx->need_prefix = true;

    /* Render the region } element begin */
    h5tools_str_reset(buffer);
    h5tools_str_append(buffer, "}");
    dimension_break =
        h5tools_render_element(stream, info, ctx, buffer, curr_pos, ncols, region_elmt_counter, elmt_counter);
    /* Render the region } element end */

    H5_LEAVE(dimension_break);
    CATCH

    H5TOOLS_ENDDEBUG(" ");

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
h5tools_print_simple_subset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t dset,
                            hid_t p_type, hid_t f_space, hsize_t hyperslab_count,
                            hsize_t       *temp_start,  /* start inside offset count loop */
                            hsize_t       *temp_count,  /* count inside offset count loop  */
                            hsize_t       *temp_block,  /* block size used in loop  */
                            hsize_t       *temp_stride, /* stride size used in loop  */
                            const hsize_t *total_size,  /* total size of dataset */
                            unsigned int   row_dim)       /* index of row_counter dimension */
{
    size_t         i;                          /* counters  */
    size_t         j;                          /* counters  */
    hsize_t        zero[1] = {0};              /* vector of zeros */
    unsigned int   flags;                      /* buffer extent flags */
    hsize_t        low[H5S_MAX_RANK];          /* low bound of hyperslab */
    hsize_t        high[H5S_MAX_RANK];         /* higher bound of hyperslab */
    size_t         p_type_nbytes;              /* size of memory type */
    hsize_t        sm_size[H5S_MAX_RANK];      /* stripmine size */
    hsize_t        sm_nbytes;                  /* bytes per stripmine */
    hssize_t       ssm_nelmts;                 /* elements per stripmine*/
    hsize_t        sm_nelmts;                  /* elements per stripmine*/
    unsigned char *sm_buf   = NULL;            /* buffer for raw data */
    hid_t          sm_space = H5I_INVALID_HID; /* stripmine data space */
    hsize_t        size_row_block;             /* size for blocks along rows */
    hsize_t        row_counter = 0;
    bool           past_catch  = false;
    /* VL data special information */
    unsigned int vl_data   = 0; /* contains VL datatypes */
    herr_t       ret_value = SUCCEED;

    H5TOOLS_START_DEBUG(" ");
    if ((size_t)ctx->ndims > NELMTS(sm_size))
        H5TOOLS_THROW(FAIL, "ndims and sm_size comparison failed");

    size_row_block = ctx->sset->block.data[row_dim];

    /* Check if we have VL data in the dataset's datatype */
    if (h5tools_detect_vlen(p_type) == true)
        vl_data = true;

    /* display loop */
    for (; hyperslab_count > 0; temp_start[row_dim] += temp_stride[row_dim], hyperslab_count--) {
        /* jump rows if size of block exceeded
         cases where block > 1 only and stride > block */
        if (size_row_block > 1 && row_counter == size_row_block &&
            ctx->sset->stride.data[row_dim] > ctx->sset->block.data[row_dim]) {

            hsize_t increase_rows = ctx->sset->stride.data[row_dim] - ctx->sset->block.data[row_dim];
            temp_start[row_dim] += increase_rows;
            row_counter = 0;
        }

        row_counter++;

        /* calculate the potential number of elements we're going to print */
        if (H5Sselect_hyperslab(f_space, H5S_SELECT_SET, temp_start, temp_stride, temp_count, temp_block) < 0)
            H5TOOLS_THROW(FAIL, "H5Sselect_hyperslab failed");

        if ((ssm_nelmts = H5Sget_select_npoints(f_space)) < 0)
            H5TOOLS_THROW(FAIL, "H5Sget_select_npoints failed");
        sm_nelmts = (hsize_t)ssm_nelmts;

        if (sm_nelmts > 0) {
            /*
             * determine the strip mine size and allocate a buffer. the strip mine is
             * a hyperslab whose size is manageable.
             */
            if ((sm_nbytes = p_type_nbytes = H5Tget_size(p_type)) == 0)
                H5TOOLS_THROW(FAIL, "H5Tget_size failed");

            if (ctx->ndims > 0)
                for (i = ctx->ndims; i > 0; --i) {
                    hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
                    if (size == 0) /* datum size > H5TOOLS_BUFSIZE */
                        size = 1;
                    sm_size[i - 1] = MIN(total_size[i - 1], size);
                    sm_nbytes *= sm_size[i - 1];
                    assert(sm_nbytes > 0);
                }

            assert(sm_nbytes == (hsize_t)((size_t)sm_nbytes)); /*check for overflow*/
            if (NULL == (sm_buf = (unsigned char *)malloc((size_t)sm_nelmts * p_type_nbytes)))
                H5TOOLS_THROW(FAIL, "Could not allocate buffer for strip-mine");

            if ((sm_space = H5Screate_simple(1, &sm_nelmts, NULL)) < 0)
                H5TOOLS_THROW(FAIL, "H5Screate_simple failed");

            if (H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &sm_nelmts, NULL) < 0)
                H5TOOLS_THROW(FAIL, "H5Sselect_hyperslab failed");

            /* read the data */
            if (H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) < 0)
                H5TOOLS_THROW(FAIL, "H5Dread failed");

            /* print the data */
            flags = START_OF_DATA;

            if (hyperslab_count == 1)
                flags |= END_OF_DATA;

            for (i = 0; i < ctx->ndims; i++)
                ctx->p_max_idx[i] = ctx->p_min_idx[i] + MIN(total_size[i], sm_size[i]);

            /* print array indices. get the lower bound of the hyperslab and calculate
               the element position at the start of hyperslab */
            if (H5Sget_select_bounds(f_space, low, high) < 0)
                H5TOOLS_THROW(FAIL, "H5Sget_select_bounds failed");

            /* initialize the current stripmine position; this is necessary to print the array indices */
            ctx->sm_pos = 0;
            for (i = 0; i < (size_t)ctx->ndims - 1; i++) {
                hsize_t offset = 1; /* accumulation of the previous dimensions */
                for (j = i + 1; j < (size_t)ctx->ndims; j++)
                    offset *= total_size[j];
                ctx->sm_pos += low[i] * offset;
            }
            ctx->sm_pos += low[ctx->ndims - 1];

            ctx->need_prefix = true;

            if (h5tools_dump_simple_data(stream, info, ctx, dset, flags, sm_nelmts, p_type, sm_buf) < 0)
                H5TOOLS_THROW(FAIL, "h5tools_dump_simple_data failed");

            /* Reclaim any VL memory, if necessary */
            if (vl_data)
                H5Treclaim(p_type, sm_space, H5P_DEFAULT, sm_buf);

            if (H5Sclose(sm_space) < 0)
                H5TOOLS_THROW(FAIL, "H5Sclose failed");
            if (sm_buf)
                free(sm_buf);
            sm_buf = NULL;
        }
        else
            H5TOOLS_THROW(SUCCEED, "nothing to print");

        ctx->continuation++;

    } /* hyperslab_count loop */

    CATCH
    if (sm_buf)
        free(sm_buf);

    H5TOOLS_ENDDEBUG(" ");
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
h5tools_display_simple_subset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t dset,
                              hid_t p_type, hid_t f_space, hsize_t *total_size)
{
    size_t       i;                         /* counters  */
    hsize_t      n;                         /* counters  */
    hsize_t      count;                     /* hyperslab count */
    hsize_t      outer_count;               /* offset count */
    unsigned int row_dim;                   /* index of row_counter dimension */
    int          current_outer_dim;         /* dimension for start */
    hsize_t      temp_start[H5S_MAX_RANK];  /* temporary start inside offset count loop */
    hsize_t      max_start[H5S_MAX_RANK];   /* maximum start inside offset count loop */
    hsize_t      temp_count[H5S_MAX_RANK];  /* temporary count inside offset count loop  */
    hsize_t      temp_block[H5S_MAX_RANK];  /* temporary block size used in loop  */
    hsize_t      temp_stride[H5S_MAX_RANK]; /* temporary stride size used in loop  */
    int          reset_dim;
    herr_t       ret_value = SUCCEED;

    if (ctx->ndims == 1)
        row_dim = 0;
    else
        row_dim = ctx->ndims - 2;

    /* get the offset count */
    outer_count = 1;
    if (ctx->ndims > 2)
        for (i = 0; i < (size_t)ctx->ndims - 2; i++) {
            /* block size is handled by containing h5tools_print_simple_subset call */
            outer_count = outer_count * ctx->sset->count.data[i];
        }

    /* initialize temporary start, count and maximum start */
    for (i = 0; i < ctx->ndims; i++) {
        temp_start[i]  = ctx->sset->start.data[i];
        temp_count[i]  = ctx->sset->count.data[i];
        temp_block[i]  = ctx->sset->block.data[i];
        temp_stride[i] = ctx->sset->stride.data[i];
        max_start[i]   = 0;
    }

    if (ctx->ndims > 2) {
        for (i = 0; i < (size_t)ctx->ndims - 2; i++) {
            max_start[i]  = temp_start[i] + ctx->sset->count.data[i] * ctx->sset->stride.data[i];
            temp_count[i] = 1;
        }
    }

    /* offset loop */
    for (n = 0; n < outer_count; n++) {
        /* number of read iterations in inner loop, read by rows, to match 2D display */
        if (ctx->ndims > 1) {
            /* count is the number of iterations to display all the rows,
             the block size count times */
            count = ctx->sset->count.data[row_dim] * ctx->sset->block.data[row_dim];

            /* always 1 row_counter at a time, that is a block of size 1, 1 time */
            temp_count[row_dim] = 1;
            temp_block[row_dim] = 1;

            /* advance 1 row_counter at a time  */
            if (ctx->sset->block.data[row_dim] > 1)
                temp_stride[row_dim] = 1;
        }
        /* for the 1D case */
        else {
            count = 1;
        }

        h5tools_print_simple_subset(stream, info, ctx, dset, p_type, f_space, count, temp_start, temp_count,
                                    temp_block, temp_stride, total_size, row_dim);

        if (ctx->ndims > 2) {
            /* dimension for start */
            current_outer_dim = (int)(ctx->ndims - 2) - 1;

            /* set start to original from current_outer_dim up */
            for (i = (size_t)(current_outer_dim + 1); i < ctx->ndims; i++)
                temp_start[i] = ctx->sset->start.data[i];

            /* increment start dimension */
            do {
                reset_dim = 0;
                temp_start[current_outer_dim] += ctx->sset->stride.data[current_outer_dim];
                if (temp_start[current_outer_dim] >= max_start[current_outer_dim]) {
                    temp_start[current_outer_dim] = ctx->sset->start.data[current_outer_dim];

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
                           hid_t p_type)
{
    int     sndims;
    hid_t   f_space = H5I_INVALID_HID; /* file data space */
    hsize_t total_size[H5S_MAX_RANK];  /* total size of dataset*/
    bool    past_catch = false;
    herr_t  ret_value  = SUCCEED;

    H5TOOLS_START_DEBUG(" ");
    if ((f_space = H5Dget_space(dset)) < 0)
        H5TOOLS_THROW(FAIL, "H5Dget_space failed");

    if ((sndims = H5Sget_simple_extent_ndims(f_space)) < 0)
        H5TOOLS_THROW(FAIL, "H5Sget_simple_extent_ndims failed");
    ctx->ndims = (unsigned)sndims;

    /* assume entire data space to be printed */
    if (H5Sget_simple_extent_dims(f_space, total_size, NULL) < 0)
        H5TOOLS_THROW(FAIL, "H5Sget_simple_extent_dims failed");
    init_acc_pos(ctx->ndims, total_size, ctx->acc, ctx->pos, ctx->p_min_idx);

    ctx->size_last_dim = total_size[ctx->ndims - 1];

    /* Set the compound datatype field list for display */
    ctx->cmpd_listv = info->cmpd_listv;

    h5tools_display_simple_subset(stream, info, ctx, dset, p_type, f_space, total_size);

    CATCH
    if (f_space >= 0 && H5Sclose(f_space) < 0)
        H5TOOLS_THROW(FAIL, "H5Sclose failed");

    H5TOOLS_ENDDEBUG(" ");
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
h5tools_dump_simple_dset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t dset,
                         hid_t p_type)
{
    hid_t        f_space = H5I_INVALID_HID; /* file data space */
    hsize_t      elmtno;                    /* counter  */
    size_t       i = 0;                     /* counter  */
    int          sndims;                    /* rank of dataspace */
    int          carry;                     /* counter carry value */
    hsize_t      zero[8];                   /* vector of zeros */
    unsigned int flags;                     /* buffer extent flags */
    hsize_t      total_size[H5S_MAX_RANK];  /* total size of dataset*/
    bool         past_catch = false;

    /* Print info */
    size_t  p_type_nbytes; /* size of memory type */
    hsize_t p_nelmts;      /* total selected elmts */

    /* Stripmine info */
    hsize_t        sm_size[H5S_MAX_RANK];      /* stripmine size */
    hsize_t        sm_nbytes;                  /* bytes per stripmine */
    hsize_t        sm_nelmts;                  /* elements per stripmine*/
    unsigned char *sm_buf   = NULL;            /* buffer for raw data */
    hid_t          sm_space = H5I_INVALID_HID; /* stripmine data space */

    /* Hyperslab info */
    hsize_t hs_offset[H5S_MAX_RANK]; /* starting offset */
    hsize_t hs_size[H5S_MAX_RANK];   /* size this pass */
    hsize_t hs_nelmts;               /* elements in request */

    /* VL data special information */
    unsigned int vl_data   = 0; /* contains VL datatypes */
    int          ret_value = 0;

    H5TOOLS_START_DEBUG(" ");
    if (H5I_INVALID_HID == (f_space = H5Dget_space(dset)))
        H5TOOLS_GOTO_ERROR((-1), "H5Dget_space failed");

    sndims = H5Sget_simple_extent_ndims(f_space);
    if (sndims < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Dget_simple_extent_ndims failed");
    ctx->ndims = (unsigned)sndims;
    H5TOOLS_DEBUG("sndims:%d", sndims);

    if ((size_t)ctx->ndims > NELMTS(sm_size))
        H5TOOLS_GOTO_ERROR((-1), "ctx->ndims > NELMTS(sm_size) failed");

    /* Assume entire data space to be printed */
    H5Sget_simple_extent_dims(f_space, total_size, NULL);
    init_acc_pos(ctx->ndims, total_size, ctx->acc, ctx->pos, ctx->p_min_idx);

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
    if (h5tools_detect_vlen(p_type) == true)
        vl_data = true;

    /*
     * Determine the strip mine size and allocate a buffer. The strip mine is
     * a hyperslab whose size is manageable.
     */
    sm_nbytes = p_type_nbytes = H5Tget_size(p_type);
    if (ctx->ndims > 0) {
        for (i = ctx->ndims; i > 0; --i) {
            hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
            if (size == 0) /* datum size > H5TOOLS_BUFSIZE */
                size = 1;
            sm_size[i - 1] = MIN(total_size[i - 1], size);
            sm_nbytes *= sm_size[i - 1];
        }
    }
    if (!sm_nbytes)
        goto done;

    assert(sm_nbytes == (hsize_t)((size_t)sm_nbytes)); /*check for overflow*/
    if (NULL != (sm_buf = (unsigned char *)malloc((size_t)sm_nbytes))) {
        H5TOOLS_DEBUG("stripmine size:%ld", sm_nbytes);

        sm_nelmts = sm_nbytes / p_type_nbytes;
        sm_space  = H5Screate_simple(1, &sm_nelmts, NULL);
        H5TOOLS_DEBUG("sm_nelmts size:%ld", sm_nelmts);

        H5TOOLS_DEBUG("ctx->ndims:%d", ctx->ndims);

        /* The stripmine loop */
        memset(hs_offset, 0, sizeof hs_offset);
        memset(zero, 0, sizeof zero);

        for (elmtno = 0; elmtno < p_nelmts; elmtno += hs_nelmts) {
            H5TOOLS_DEBUG("stripmine read loop:%d", i);
            /* Calculate the hyperslab size */
            if (ctx->ndims > 0) {
                for (i = 0, hs_nelmts = 1; i < ctx->ndims; i++) {
                    hs_size[i]        = MIN(total_size[i] - hs_offset[i], sm_size[i]);
                    ctx->p_max_idx[i] = ctx->p_min_idx[i] + hs_size[i];
                    hs_nelmts *= hs_size[i];
                }

                if (H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0)
                    H5TOOLS_ERROR((-1), "H5Sselect_hyperslab hs_offset failed");
                if (H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &hs_nelmts, NULL) < 0)
                    H5TOOLS_ERROR((-1), "H5Sselect_hyperslab zero failed");
            }
            else {
                if (H5Sselect_all(f_space) < 0)
                    H5TOOLS_ERROR((-1), "H5Sselect_all f_space failed");
                if (H5Sselect_all(sm_space) < 0)
                    H5TOOLS_ERROR((-1), "H5Sselect_all sm_space failed");
                hs_nelmts = 1;
            }

            H5TOOLS_DEBUG("Read the data");
            /* Read the data */
            if (H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) >= 0) {
                /* Print the data */
                flags = (elmtno == 0) ? START_OF_DATA : 0;
                flags |= ((elmtno + hs_nelmts) >= p_nelmts) ? END_OF_DATA : 0;

                /* initialize the current stripmine position; this is necessary to print the array
                indices */
                ctx->sm_pos = elmtno;

                if (h5tools_dump_simple_data(stream, info, ctx, dset, flags, hs_nelmts, p_type, sm_buf) < 0)
                    H5TOOLS_ERROR((-1), "h5tools_dump_simple_data failed");

                /* Reclaim any VL memory, if necessary */
                if (vl_data)
                    H5Treclaim(p_type, sm_space, H5P_DEFAULT, sm_buf);

                H5TOOLS_DEBUG("Calculate the next hyperslab offset");
                /* Calculate the next hyperslab offset */
                for (i = ctx->ndims, carry = 1; i > 0 && carry; --i) {
                    ctx->p_min_idx[i - 1] = ctx->p_max_idx[i - 1];
                    hs_offset[i - 1] += hs_size[i - 1];

                    if (hs_offset[i - 1] == total_size[i - 1])
                        hs_offset[i - 1] = 0;
                    else
                        carry = 0;
                }
            }
            else
                H5TOOLS_ERROR((-1), "H5Dread failed");

            ctx->continuation++;
            H5TOOLS_DEBUG("stripmine read loop:%d complete", i);
        }
        free(sm_buf);
    } /* if (NULL != (sm_buf...)) */

done:
    if (sm_space >= 0 && H5Sclose(sm_space) < 0)
        H5TOOLS_ERROR((-1), "H5Sclose failed");
    if (f_space >= 0 && H5Sclose(f_space) < 0)
        H5TOOLS_ERROR((-1), "H5Sclose failed");
    CATCH
    H5TOOLS_ENDDEBUG(" ");
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
 *-------------------------------------------------------------------------
 */
static int
h5tools_dump_simple_mem(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t attr_id,
                        hid_t p_type)
{
    hid_t          f_space = H5I_INVALID_HID; /* file data space */
    hsize_t        alloc_size;
    int            sndims;                   /* rank of dataspace */
    unsigned       i;                        /* counters  */
    hsize_t        total_size[H5S_MAX_RANK]; /* total size of dataset*/
    hsize_t        p_nelmts;                 /* total selected elmts */
    bool           past_catch = false;
    unsigned char *buf        = NULL; /* buffer for raw data */
    int            ret_value  = 0;

    /* VL data special information */
    unsigned int vl_data = 0; /* contains VL datatypes */

    H5TOOLS_START_DEBUG(" ");
    if (H5I_INVALID_HID == (f_space = H5Aget_space(attr_id)))
        H5TOOLS_GOTO_ERROR((-1), "H5Dget_space failed");

    sndims = H5Sget_simple_extent_ndims(f_space);
    if (sndims < 0)
        H5TOOLS_THROW((-1), "H5Dget_simple_extent_ndims failed");
    ctx->ndims = (unsigned)sndims;
    H5TOOLS_DEBUG("sndims:%d", sndims);

    if ((size_t)ctx->ndims > NELMTS(ctx->p_min_idx))
        H5TOOLS_THROW((-1), "ctx->ndims > NELMTS(ctx->p_min_idx) failed");

    /* Assume entire data space to be printed */
    H5Sget_simple_extent_dims(f_space, total_size, NULL);
    init_acc_pos(ctx->ndims, total_size, ctx->acc, ctx->pos, ctx->p_min_idx);

    /* calculate the number of elements we're going to print */
    p_nelmts = 1;

    if (ctx->ndims > 0) {
        for (i = 0; i < ctx->ndims; i++)
            p_nelmts *= total_size[i];
        ctx->size_last_dim = (total_size[ctx->ndims - 1]);
    } /* end if */
    else
        ctx->size_last_dim = 0;

    if (p_nelmts == 0)
        H5_LEAVE(SUCCEED); /* nothing to print */

    /* Check if we have VL data in the dataset's datatype */
    if (h5tools_detect_vlen(p_type) == true)
        vl_data = true;

    alloc_size = p_nelmts * H5Tget_size(p_type);
    assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/
    if (NULL != (buf = (unsigned char *)malloc((size_t)alloc_size))) {
        H5TOOLS_DEBUG("ctx->ndims:%d", ctx->ndims);

        H5TOOLS_DEBUG("Read the data");
        /* Read the data */
        if (H5Aread(attr_id, p_type, buf) >= 0) {
            if (h5tools_dump_simple_data(stream, info, ctx, attr_id, START_OF_DATA | END_OF_DATA, p_nelmts,
                                         p_type, buf) < 0)
                H5TOOLS_ERROR((-1), "h5tools_dump_simple_data failed");

            /* Reclaim any VL memory, if necessary */
            if (vl_data)
                H5Treclaim(p_type, f_space, H5P_DEFAULT, buf);
        }
        else
            H5TOOLS_ERROR((-1), "H5Aread failed");
        free(buf);
    } /* if (NULL != (buf...)) */
done:
    if (f_space >= 0 && H5Sclose(f_space) < 0)
        H5TOOLS_ERROR((-1), "H5Sclose failed");
    CATCH
    H5TOOLS_ENDDEBUG(" ");
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
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_dset(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t dset)
{
    hid_t           f_space = H5I_INVALID_HID;
    hid_t           p_type  = H5I_INVALID_HID;
    hid_t           f_type  = H5I_INVALID_HID;
    H5S_class_t     space_type;
    h5tool_format_t info_dflt;
    int             ret_value = 0;

    H5TOOLS_START_DEBUG(" ");
    /* Use default values */
    if (!stream)
        stream = rawoutstream;

    if (!info) {
        memset(&info_dflt, 0, sizeof info_dflt);
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
        p_type = H5Tget_native_type(f_type, H5T_DIR_DEFAULT);

    if (p_type < 0)
        goto done;

    /* Check the data space */
    f_space = H5Dget_space(dset);
    if (f_space < 0)
        goto done;

    space_type = H5Sget_simple_extent_type(f_space);

    /* Print the data */
    if (space_type == H5S_SIMPLE || space_type == H5S_SCALAR) {
        if (!ctx->sset)
            ret_value = h5tools_dump_simple_dset(rawdatastream, info, ctx, dset, p_type);
        else
            ret_value = h5tools_dump_simple_subset(rawdatastream, info, ctx, dset, p_type);
    }
    else {
        /* space is H5S_NULL */
        ret_value = SUCCEED;
    }
done:
    if (f_type > 0)
        H5Tclose(f_type);
    if (p_type > 0)
        H5Tclose(p_type);
    if (f_space > 0)
        H5Sclose(f_space);

    H5TOOLS_ENDDEBUG(" ");
    return ret_value;
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
 *-------------------------------------------------------------------------
 */
int
h5tools_dump_mem(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t attr_id)
{
    hid_t           f_space = H5I_INVALID_HID;
    hid_t           p_type  = H5I_INVALID_HID;
    hid_t           f_type  = H5I_INVALID_HID;
    h5tool_format_t info_dflt;
    int             ret_value = 0;

    H5TOOLS_START_DEBUG(" ");
    /* Use default values */
    if (!stream)
        stream = rawoutstream;

    if (!info) {
        memset(&info_dflt, 0, sizeof(info_dflt));
        info = &info_dflt;
    }

    f_type = H5Aget_type(attr_id);
    if (f_type < 0)
        goto done;

    if (info->raw || bin_form == 1)
        p_type = H5Tcopy(f_type);
    else if (bin_form == 2)
        p_type = h5tools_get_little_endian_type(f_type);
    else if (bin_form == 3)
        p_type = h5tools_get_big_endian_type(f_type);
    else
        p_type = H5Tget_native_type(f_type, H5T_DIR_DEFAULT);

    if (p_type < 0)
        goto done;

    /* Check the data space */
    f_space = H5Aget_space(attr_id);
    if (f_space < 0)
        goto done;

    /* Check the data space */
    if (H5Sis_simple(f_space) <= 0) {
        H5TOOLS_ERROR((-1), "H5Sis_simple failed");
    }
    else {
        ret_value = h5tools_dump_simple_mem(rawattrstream, info, ctx, attr_id, p_type);
    }
done:
    if (f_type > 0)
        H5Tclose(f_type);
    if (p_type > 0)
        H5Tclose(p_type);
    if (f_space > 0)
        H5Sclose(f_space);

    H5TOOLS_ENDDEBUG(" ");
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
 *-------------------------------------------------------------------------
 */
int
h5tools_print_datatype(FILE *stream, h5tools_str_t *buffer, const h5tool_format_t *info,
                       h5tools_context_t *ctx, hid_t type, int object_search)
{
    char       *mname;
    hid_t       mtype    = H5I_INVALID_HID;
    hid_t       str_type = H5I_INVALID_HID;
    hid_t       super    = H5I_INVALID_HID;
    hid_t       tmp_type = H5I_INVALID_HID;
    int         snmembers;
    int         sndims;
    unsigned    nmembers;
    unsigned    i;
    size_t      size  = 0;
    size_t      ncols = 80; /*available output width */
    hsize_t     dims[H5TOOLS_DUMP_MAX_RANK];
    hsize_t     curr_pos = 0; /* total data element position   */
    H5T_str_t   str_pad;
    H5T_cset_t  cset;
    H5T_order_t order;
    H5T_class_t type_class;
    H5T_sign_t  sign; /* sign scheme value */
    htri_t      is_vlstr   = false;
    bool        past_catch = false;
    const char *sign_s     = NULL; /* sign scheme string */
    const char *order_s    = NULL; /* byte order string */
    int         ret_value  = 0;

    H5TOOLS_START_DEBUG(" ");
    if ((type_class = H5Tget_class(type)) < 0)
        H5TOOLS_THROW((-1), "H5Tget_class failed");
    if (object_search && H5Tcommitted(type) > 0) {
        H5O_info2_t oinfo;
        obj_t      *obj = NULL; /* Found object */

        H5Oget_info3(type, &oinfo, H5O_INFO_BASIC);
        obj = search_obj(h5dump_type_table, &oinfo.token);

        if (obj) {
            if (!obj->recorded) {
                char *obj_tok_str = NULL;

                H5Otoken_to_str(type, &oinfo.token, &obj_tok_str);
                h5tools_str_append(buffer, "\"/#%s\"", obj_tok_str);
                H5free_memory(obj_tok_str);
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
            if (H5Tequal(type, H5T_STD_I8BE) == true)
                h5tools_str_append(buffer, "H5T_STD_I8BE");
            else if (H5Tequal(type, H5T_STD_I8LE) == true)
                h5tools_str_append(buffer, "H5T_STD_I8LE");
            else if (H5Tequal(type, H5T_STD_I16BE) == true)
                h5tools_str_append(buffer, "H5T_STD_I16BE");
            else if (H5Tequal(type, H5T_STD_I16LE) == true)
                h5tools_str_append(buffer, "H5T_STD_I16LE");
            else if (H5Tequal(type, H5T_STD_I32BE) == true)
                h5tools_str_append(buffer, "H5T_STD_I32BE");
            else if (H5Tequal(type, H5T_STD_I32LE) == true)
                h5tools_str_append(buffer, "H5T_STD_I32LE");
            else if (H5Tequal(type, H5T_STD_I64BE) == true)
                h5tools_str_append(buffer, "H5T_STD_I64BE");
            else if (H5Tequal(type, H5T_STD_I64LE) == true)
                h5tools_str_append(buffer, "H5T_STD_I64LE");
            else if (H5Tequal(type, H5T_STD_U8BE) == true)
                h5tools_str_append(buffer, "H5T_STD_U8BE");
            else if (H5Tequal(type, H5T_STD_U8LE) == true)
                h5tools_str_append(buffer, "H5T_STD_U8LE");
            else if (H5Tequal(type, H5T_STD_U16BE) == true)
                h5tools_str_append(buffer, "H5T_STD_U16BE");
            else if (H5Tequal(type, H5T_STD_U16LE) == true)
                h5tools_str_append(buffer, "H5T_STD_U16LE");
            else if (H5Tequal(type, H5T_STD_U32BE) == true)
                h5tools_str_append(buffer, "H5T_STD_U32BE");
            else if (H5Tequal(type, H5T_STD_U32LE) == true)
                h5tools_str_append(buffer, "H5T_STD_U32LE");
            else if (H5Tequal(type, H5T_STD_U64BE) == true)
                h5tools_str_append(buffer, "H5T_STD_U64BE");
            else if (H5Tequal(type, H5T_STD_U64LE) == true)
                h5tools_str_append(buffer, "H5T_STD_U64LE");
            else if (H5Tequal(type, H5T_NATIVE_SCHAR) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_SCHAR");
            else if (H5Tequal(type, H5T_NATIVE_UCHAR) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_UCHAR");
            else if (H5Tequal(type, H5T_NATIVE_SHORT) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_SHORT");
            else if (H5Tequal(type, H5T_NATIVE_USHORT) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_USHORT");
            else if (H5Tequal(type, H5T_NATIVE_INT) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_INT");
            else if (H5Tequal(type, H5T_NATIVE_UINT) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_UINT");
            else if (H5Tequal(type, H5T_NATIVE_LONG) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_LONG");
            else if (H5Tequal(type, H5T_NATIVE_ULONG) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_ULONG");
            else if (H5Tequal(type, H5T_NATIVE_LLONG) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_LLONG");
            else if (H5Tequal(type, H5T_NATIVE_ULLONG) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_ULLONG");
            else {

                /* byte order */
                if (H5Tget_size(type) > 1) {
                    order = H5Tget_order(type);
                    if (H5T_ORDER_LE == order)
                        order_s = " little-endian";
                    else if (H5T_ORDER_BE == order)
                        order_s = " big-endian";
                    else if (H5T_ORDER_VAX == order)
                        order_s = " mixed-endian";
                    else
                        order_s = " unknown-byte-order";
                }
                else
                    order_s = "";

                /* sign */
                if ((sign = H5Tget_sign(type)) >= 0) {
                    if (H5T_SGN_NONE == sign)
                        sign_s = " unsigned";
                    else if (H5T_SGN_2 == sign)
                        sign_s = "";
                    else
                        sign_s = " unknown-sign";
                }
                else
                    sign_s = " unknown-sign";

                /* print size, order, sign, and precision */
                h5tools_str_append(buffer, "%zu-bit%s%s integer %zu-bit precision", 8 * H5Tget_size(type),
                                   order_s, sign_s, H5Tget_precision(type));
            }
            break;

        case H5T_FLOAT:
            if (H5Tequal(type, H5T_IEEE_F16BE) == true)
                h5tools_str_append(buffer, "H5T_IEEE_F16BE");
            else if (H5Tequal(type, H5T_IEEE_F16LE) == true)
                h5tools_str_append(buffer, "H5T_IEEE_F16LE");
            else if (H5Tequal(type, H5T_IEEE_F32BE) == true)
                h5tools_str_append(buffer, "H5T_IEEE_F32BE");
            else if (H5Tequal(type, H5T_IEEE_F32LE) == true)
                h5tools_str_append(buffer, "H5T_IEEE_F32LE");
            else if (H5Tequal(type, H5T_IEEE_F64BE) == true)
                h5tools_str_append(buffer, "H5T_IEEE_F64BE");
            else if (H5Tequal(type, H5T_IEEE_F64LE) == true)
                h5tools_str_append(buffer, "H5T_IEEE_F64LE");
            else if (H5Tequal(type, H5T_VAX_F32) == true)
                h5tools_str_append(buffer, "H5T_VAX_F32");
            else if (H5Tequal(type, H5T_VAX_F64) == true)
                h5tools_str_append(buffer, "H5T_VAX_F64");
#ifdef H5_HAVE__FLOAT16
            else if (H5Tequal(type, H5T_NATIVE_FLOAT16) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_FLOAT16");
#endif
            else if (H5Tequal(type, H5T_NATIVE_FLOAT) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_FLOAT");
            else if (H5Tequal(type, H5T_NATIVE_DOUBLE) == true)
                h5tools_str_append(buffer, "H5T_NATIVE_DOUBLE");
            else {
                /* print what the library knows */
                /* byte order */
                if (H5Tget_size(type) > 1) {
                    order = H5Tget_order(type);
                    if (H5T_ORDER_LE == order)
                        order_s = " little-endian";
                    else if (H5T_ORDER_BE == order)
                        order_s = " big-endian";
                    else if (H5T_ORDER_VAX == order)
                        order_s = " mixed-endian";
                    else
                        order_s = " unknown-byte-order";
                }
                else
                    order_s = "";

                /* print size. byte order, and precision */
                h5tools_str_append(buffer, "%zu-bit%s floating-point %zu-bit precision",
                                   8 * H5Tget_size(type), order_s, H5Tget_precision(type));
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
            size     = H5Tget_size(tmp_type);
            str_pad  = H5Tget_strpad(tmp_type);
            cset     = H5Tget_cset(tmp_type);
            is_vlstr = H5Tis_variable_str(tmp_type);

            curr_pos = ctx->cur_column;
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->strblockbegin);
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);

            ctx->indent_level++;

            ctx->need_prefix = true;

            h5tools_str_reset(buffer);

            if (is_vlstr)
                h5tools_str_append(buffer, "%s H5T_VARIABLE;", STRSIZE);
            else
                h5tools_str_append(buffer, "%s %d;", STRSIZE, (int)size);
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);

            ctx->need_prefix = true;

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
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);

            ctx->need_prefix = true;

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
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);

            ctx->need_prefix = true;

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
            if (order == H5T_ORDER_LE) {
                if (H5Tset_order(str_type, H5T_ORDER_LE) < 0)
                    H5TOOLS_ERROR((-1), "H5Tset_order failed");
            } /* end if */
            else if (order == H5T_ORDER_BE) {
                if (H5Tset_order(str_type, H5T_ORDER_BE) < 0)
                    H5TOOLS_ERROR((-1), "H5Tset_order failed");
            } /* end if */

            if (H5Tequal(tmp_type, str_type)) {
                h5tools_str_append(buffer, "H5T_C_S1;");
                goto found_string_type;
            }

            /* If not equal to C variable-length string, check Fortran type. */
            if (H5Tclose(str_type) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose failed");
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
            if (order == H5T_ORDER_LE) {
                if (H5Tset_order(str_type, H5T_ORDER_LE) < 0)
                    H5TOOLS_ERROR((-1), "H5Tset_order failed");
            } /* end if */
            else if (order == H5T_ORDER_BE) {
                if (H5Tset_order(str_type, H5T_ORDER_BE) < 0)
                    H5TOOLS_ERROR((-1), "H5Tset_order failed");
            } /* end if */

            if (H5Tequal(tmp_type, str_type)) {
                h5tools_str_append(buffer, "H5T_FORTRAN_S1;");
                goto found_string_type;
            }

            /* Type doesn't match any of above. */
            h5tools_str_append(buffer, "unknown_one_character_type;");

found_string_type:
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);
            ctx->indent_level--;

            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            if (H5Tclose(str_type) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose failed");
            if (H5Tclose(tmp_type) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose failed");

            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->strblockend);
            break;

        case H5T_BITFIELD:
            if (H5Tequal(type, H5T_STD_B8BE) == true)
                h5tools_str_append(buffer, "H5T_STD_B8BE");
            else if (H5Tequal(type, H5T_STD_B8LE) == true)
                h5tools_str_append(buffer, "H5T_STD_B8LE");
            else if (H5Tequal(type, H5T_STD_B16BE) == true)
                h5tools_str_append(buffer, "H5T_STD_B16BE");
            else if (H5Tequal(type, H5T_STD_B16LE) == true)
                h5tools_str_append(buffer, "H5T_STD_B16LE");
            else if (H5Tequal(type, H5T_STD_B32BE) == true)
                h5tools_str_append(buffer, "H5T_STD_B32BE");
            else if (H5Tequal(type, H5T_STD_B32LE) == true)
                h5tools_str_append(buffer, "H5T_STD_B32LE");
            else if (H5Tequal(type, H5T_STD_B64BE) == true)
                h5tools_str_append(buffer, "H5T_STD_B64BE");
            else if (H5Tequal(type, H5T_STD_B64LE) == true)
                h5tools_str_append(buffer, "H5T_STD_B64LE");
            else
                h5tools_str_append(buffer, "undefined bitfield");
            break;

        case H5T_OPAQUE:
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->opaqblockbegin);
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);
            ctx->indent_level++;
            {
                char *ttag;

                if (NULL == (ttag = H5Tget_tag(type)))
                    H5TOOLS_THROW((-1), "H5Tget_tag failed");

                ctx->need_prefix = true;

                h5tools_str_reset(buffer);
                h5tools_str_append(buffer, "OPAQUE_TAG \"%s\";", ttag);
                h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);

                H5free_memory(ttag);

                if ((size = H5Tget_size(type)) <= 0) {
                    ctx->need_prefix = true;

                    h5tools_str_reset(buffer);
                    h5tools_str_append(buffer, "OPAQUE_SIZE \"%zu\";", size);
                    h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);
                }
            }
            ctx->indent_level--;

            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->opaqblockend);
            break;

        case H5T_COMPOUND:
            if ((snmembers = H5Tget_nmembers(type)) < 0)
                H5TOOLS_THROW((-1), "H5Tget_nmembers failed");
            nmembers = (unsigned)snmembers;

            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->cmpdblockbegin);
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);

            ctx->indent_level++;
            for (i = 0; i < nmembers; i++) {
                mname = H5Tget_member_name(type, i);
                if ((mtype = H5Tget_member_type(type, i)) >= 0) {
                    ctx->need_prefix = true;

                    h5tools_str_reset(buffer);
                    h5tools_print_datatype(stream, buffer, info, ctx, mtype, true);

                    h5tools_str_append(buffer, " \"%s\";", mname);
                    h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);
                    if (H5Tclose(mtype) < 0)
                        H5TOOLS_ERROR((-1), "H5Tclose failed");
                }
                else
                    H5TOOLS_ERROR((-1), "H5Tget_member_type failed");
                H5free_memory(mname);
            }
            ctx->indent_level--;

            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->cmpdblockend);
            break;

        case H5T_REFERENCE:
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->refblockbegin);
            if (H5Tequal(type, H5T_STD_REF_DSETREG) == true) {
                h5tools_str_append(buffer, "H5T_STD_REF_DSETREG");
            }
            else if (H5Tequal(type, H5T_STD_REF_OBJ) == true) {
                h5tools_str_append(buffer, "H5T_STD_REF_OBJECT");
            }
            else if (H5Tequal(type, H5T_STD_REF) == true) {
                h5tools_str_append(buffer, "H5T_STD_REF");
            }
            else {
                h5tools_str_append(buffer, "UNDEFINED");
            }
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->refblockend);
            break;

        case H5T_ENUM:
            if ((super = H5Tget_super(type)) < 0)
                H5TOOLS_THROW((-1), "H5Tget_super failed");

            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->enumblockbegin);
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);
            ctx->indent_level++;

            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            h5tools_print_datatype(stream, buffer, info, ctx, super, true);

            if (H5Tclose(super) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose failed");

            h5tools_str_append(buffer, ";");
            h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);

            h5tools_print_enum(stream, buffer, info, ctx, type);

            ctx->indent_level--;

            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->enumblockend);

            break;

        case H5T_VLEN:
            if ((super = H5Tget_super(type)) < 0)
                H5TOOLS_THROW((-1), "H5Tget_super failed");

            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->vlenblockbegin);

            h5tools_print_datatype(stream, buffer, info, ctx, super, true);

            if (H5Tclose(super) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose failed");

            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->vlenblockend);

            break;

        case H5T_ARRAY:
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->arrblockbegin);

            /* Get array information */
            if ((sndims = H5Tget_array_ndims(type)) >= 0) {
                unsigned ndims = (unsigned)sndims;

                if (H5Tget_array_dims2(type, dims) >= 0) {
                    /* Print array dimensions */
                    for (i = 0; i < ndims; i++)
                        h5tools_str_append(buffer, "[%" PRIuHSIZE "]", dims[i]);

                    h5tools_str_append(buffer, " ");
                }
                else
                    H5TOOLS_ERROR((-1), "H5Tget_array_dims2 failed");
            }
            else
                H5TOOLS_ERROR((-1), "H5Tget_array_ndims failed");

            /* Get array base type */
            if ((super = H5Tget_super(type)) >= 0) {
                /* Print base type */
                h5tools_print_datatype(stream, buffer, info, ctx, super, true);
                /* Close array base type */
                if (H5Tclose(super) < 0)
                    H5TOOLS_ERROR((-1), "H5Tclose failed");
            }
            else
                H5TOOLS_ERROR((-1), "H5Tget_super failed");

            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->arrblockend);

            break;

        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            h5tools_str_append(buffer, "unknown datatype");
            break;
    }

    CATCH
    H5TOOLS_ENDDEBUG(" ");
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
 *-------------------------------------------------------------------------
 */
int
h5tools_print_dataspace(h5tools_str_t *buffer, hid_t space)
{
    hsize_t     size[H5TOOLS_DUMP_MAX_RANK];
    hsize_t     maxsize[H5TOOLS_DUMP_MAX_RANK];
    int         ndims      = -1;
    H5S_class_t space_type = -1;
    bool        past_catch = false;
    int         i;
    int         ret_value = 0;

    H5TOOLS_START_DEBUG(" ");
    if ((ndims = H5Sget_simple_extent_dims(space, size, maxsize)) < 0)
        H5TOOLS_THROW((-1), "H5Sget_simple_extent_dims failed");

    if ((space_type = H5Sget_simple_extent_type(space)) < 0)
        H5TOOLS_THROW((-1), "H5Sget_simple_extent_type failed");

    switch (space_type) {
        case H5S_SCALAR:
            /* scalar dataspace */
            h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->dataspacedescriptionbegin,
                               S_SCALAR);
            break;

        case H5S_SIMPLE:
            /* simple dataspace */
            h5tools_str_append(buffer, "%s %s { %s %" PRIuHSIZE,
                               h5tools_dump_header_format->dataspacedescriptionbegin, S_SIMPLE,
                               h5tools_dump_header_format->dataspacedimbegin, size[0]);

            for (i = 1; i < ndims; i++)
                h5tools_str_append(buffer, ", %" PRIuHSIZE, size[i]);

            h5tools_str_append(buffer, " %s / ", h5tools_dump_header_format->dataspacedimend);

            if (maxsize[0] == H5S_UNLIMITED)
                h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->dataspacedimbegin,
                                   "H5S_UNLIMITED");
            else
                h5tools_str_append(buffer, "%s %" PRIuHSIZE, h5tools_dump_header_format->dataspacedimbegin,
                                   maxsize[0]);

            for (i = 1; i < ndims; i++)
                if (maxsize[i] == H5S_UNLIMITED)
                    h5tools_str_append(buffer, ", %s", "H5S_UNLIMITED");
                else
                    h5tools_str_append(buffer, ", %" PRIuHSIZE, maxsize[i]);

            h5tools_str_append(buffer, " %s }", h5tools_dump_header_format->dataspacedimend);
            break;

        case H5S_NULL:
            /* null dataspace */
            h5tools_str_append(buffer, "%s %s", h5tools_dump_header_format->dataspacedescriptionbegin,
                               S_NULL);
            break;

        case H5S_NO_CLASS:
        default:
            h5tools_str_append(buffer, "%s unknown dataspace %s\n", BEGIN, END);
            break;
    } /* end switch */

    CATCH
    H5TOOLS_ENDDEBUG(" ");
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
h5tools_print_enum(FILE *stream, h5tools_str_t *buffer, const h5tool_format_t *info, h5tools_context_t *ctx,
                   hid_t type)
{
    char         **name  = NULL; /*member names                   */
    unsigned char *value = NULL; /*value array                    */
    unsigned       i;
    unsigned       nmembs = 0; /*number of members              */
    int            snmembs;
    hid_t          super  = H5I_INVALID_HID; /*enum base integer type         */
    hid_t          native = H5I_INVALID_HID; /*native integer datatype        */
    H5T_sign_t     sign_type;                /*sign of value type             */
    size_t         type_size;                /*value type size                */
    size_t         dst_size;                 /*destination value type size    */
    size_t         ncols      = 80;          /*available output width */
    hsize_t        curr_pos   = 0;           /* total data element position   */
    bool           past_catch = false;
    int            ret_value  = 0;

    H5TOOLS_START_DEBUG(" ");
    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    if ((snmembs = H5Tget_nmembers(type)) < 0)
        H5TOOLS_THROW((-1), "H5Tget_nmembers failed");
    nmembs = (unsigned)snmembs;
    assert(nmembs > 0);

    if ((super = H5Tget_super(type)) < 0)
        H5TOOLS_THROW((-1), "H5Tget_super failed");

    if ((type_size = H5Tget_size(type)) <= 0)
        H5TOOLS_THROW((-1), "H5Tget_size(type) failed");

    /*
     * Determine what datatype to use for the native values.  To simplify
     * things we entertain three possibilities:
     *  1. long long -- the largest native signed integer
     *  2. unsigned long long -- the largest native unsigned integer
     *  3. raw format
     */
    if (type_size <= sizeof(long long)) {
        dst_size = sizeof(long long);

        if ((sign_type = H5Tget_sign(type)) < 0)
            H5TOOLS_THROW((-1), "H5Tget_sign failed");
        if (H5T_SGN_NONE == sign_type)
            native = H5T_NATIVE_ULLONG;
        else
            native = H5T_NATIVE_LLONG;
    } /* end if */
    else
        dst_size = type_size;

    /* Get the names and raw values of all members */
    if (NULL == (name = (char **)calloc((size_t)nmembs, sizeof(char *))))
        H5TOOLS_THROW((-1), "Could not allocate buffer for member name");
    if (NULL == (value = (unsigned char *)calloc((size_t)nmembs, MAX(type_size, dst_size))))
        H5TOOLS_THROW((-1), "Could not allocate buffer for member value");

    for (i = 0; i < nmembs; i++) {
        name[i] = H5Tget_member_name(type, i);
        if (H5Tget_member_value(type, i, value + i * type_size) < 0)
            H5TOOLS_THROW((-1), "H5Tget_member_value failed");
    }

    /* Convert values to native datatype */
    if (native > 0)
        if (H5Tconvert(super, native, (size_t)nmembs, value, NULL, H5P_DEFAULT) < 0)
            H5TOOLS_THROW((-1), "H5Tconvert failed");

    /*
     * Sort members by increasing value
     *    ***not implemented yet***
     */

    /* Print members */
    for (i = 0; i < nmembs; i++) {
        int nchars; /*number of output characters    */

        ctx->need_prefix = true;
        h5tools_simple_prefix(stream, info, ctx, (hsize_t)0, 0);

        h5tools_str_reset(buffer);
        h5tools_str_append(buffer, "\"%s\"", name[i]);
        nchars = (int)strlen(name[i]);
        h5tools_str_append(buffer, "%*s ", MAX(0, 16 - nchars), "");

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

        h5tools_str_append(buffer, ";");
        h5tools_render_element(stream, info, ctx, buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
    }

    CATCH
    if (name) {
        /* Release resources */
        for (i = 0; i < nmembs; i++)
            if (name[i])
                H5free_memory(name[i]);
        free(name);
    } /* end if */

    if (value)
        free(value);

    if (super >= 0 && H5Tclose(super) < 0)
        H5TOOLS_THROW((-1), "Could not close datatype's super class");

    if (0 == nmembs)
        h5tools_str_append(buffer, "\n<empty>");

    H5TOOLS_ENDDEBUG(" ");
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
h5tools_dump_datatype(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t type)
{
    h5tools_str_t buffer;                 /* string into which to render   */
    size_t        ncols    = 80;          /* available output width        */
    hsize_t       curr_pos = ctx->sm_pos; /* total data element position   */
                                          /* pass to the prefix in h5tools_simple_prefix the total position
                                           * instead of the current stripmine position i; this is necessary
                                           * to print the array indices
                                           */

    /* setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->datatypebegin,
                       h5tools_dump_header_format->datatypeblockbegin);
    h5tools_print_datatype(stream, &buffer, info, ctx, type, true);
    if (strlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if (strlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(&buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->datatypeend))
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
h5tools_dump_dataspace(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t type)
{
    h5tools_str_t buffer;                 /* string into which to render   */
    size_t        ncols    = 80;          /* available output width        */
    hsize_t       curr_pos = ctx->sm_pos; /* total data element position   */
                                          /* pass to the prefix in h5tools_simple_prefix the total position
                                           * instead of the current stripmine position i; this is necessary
                                           * to print the array indices
                                           */

    /* setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s ", h5tools_dump_header_format->dataspacebegin);

    h5tools_print_dataspace(&buffer, type);

    if (strlen(h5tools_dump_header_format->dataspaceblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->dataspaceblockend);
        if (strlen(h5tools_dump_header_format->dataspaceend))
            h5tools_str_append(&buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->dataspaceend))
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
h5tools_dump_oid(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t oid)
{
    h5tools_str_t buffer;                 /* string into which to render   */
    size_t        ncols    = 80;          /* available output width        */
    hsize_t       curr_pos = ctx->sm_pos; /* total data element position   */
                                          /* pass to the prefix in h5tools_simple_prefix the total position
                                           * instead of the current stripmine position i; this is necessary
                                           * to print the array indices
                                           */

    /* setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s %" PRId64 " %s", OBJID, BEGIN, oid, END);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    print_virtual_selection
 *
 * Purpose:     Print the virtual dataset selection.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
static void
h5tools_print_virtual_selection(hid_t vspace, FILE *stream, const h5tool_format_t *info,
                                h5tools_context_t *ctx,      /* in,out*/
                                h5tools_str_t     *buffer,   /* string into which to render */
                                hsize_t           *curr_pos, /* total data element position */
                                size_t             ncols)
{
    switch (H5Sget_select_type(vspace)) {
        case H5S_SEL_NONE: /* Nothing selected         */
            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            h5tools_str_append(buffer, "%s", VDS_NONE);
            break;
        case H5S_SEL_POINTS: /* Sequence of points selected  */
            h5tools_str_reset(buffer);
            h5tools_str_append(buffer, "%s %s ", VDS_POINT,
                               h5tools_dump_header_format->virtualselectionblockbegin);
            h5tools_str_dump_space_points(buffer, vspace, info);
            h5tools_str_append(buffer, " %s", h5tools_dump_header_format->virtualselectionblockend);
            break;
        case H5S_SEL_HYPERSLABS: /* "New-style" hyperslab selection defined  */
            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            if (H5Sis_regular_hyperslab(vspace)) {
                h5tools_str_append(buffer, "%s %s ", VDS_REG_HYPERSLAB,
                                   h5tools_dump_header_format->virtualselectionblockbegin);
                h5tools_render_element(stream, info, ctx, buffer, curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);

                h5tools_str_reset(buffer);
                h5tools_str_dump_space_slabs(buffer, vspace, info, ctx);
            }
            else {
                h5tools_str_append(buffer, "%s %s ", VDS_IRR_HYPERSLAB,
                                   h5tools_dump_header_format->virtualselectionblockbegin);
                h5tools_render_element(stream, info, ctx, buffer, curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);
                ctx->indent_level++;
                ctx->need_prefix = true;
                h5tools_simple_prefix(stream, info, ctx, *curr_pos, 0);

                h5tools_str_reset(buffer);
                h5tools_str_dump_space_blocks(buffer, vspace, info);
                ctx->indent_level--;
            }
            h5tools_render_element(stream, info, ctx, buffer, curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);
            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            h5tools_str_append(buffer, "%s", h5tools_dump_header_format->virtualselectionblockend);
            break;
        case H5S_SEL_ALL: /* Entire extent selected   */
            ctx->need_prefix = true;

            h5tools_str_reset(buffer);
            h5tools_str_append(buffer, "%s", VDS_ALL);
            break;
        case H5S_SEL_ERROR:
        case H5S_SEL_N:
        default:
            h5tools_str_append(buffer, "Unknown Selection");
    }
    h5tools_render_element(stream, info, ctx, buffer, curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
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
h5tools_print_fill_value(h5tools_str_t *buffer /*in,out*/, const h5tool_format_t *info,
                         h5tools_context_t *ctx /*in,out*/, hid_t dcpl, hid_t type_id, hid_t obj_id)
{
    size_t size;
    hid_t  n_type  = H5I_INVALID_HID;
    void  *buf     = NULL;
    bool   vl_data = false;

    n_type = H5Tget_native_type(type_id, H5T_DIR_DEFAULT);

    if (h5tools_detect_vlen(type_id) == true)
        vl_data = true;

    size = H5Tget_size(n_type);
    buf  = malloc(size);

    H5Pget_fill_value(dcpl, n_type, buf);

    h5tools_str_sprint(buffer, info, obj_id, n_type, buf, ctx);

    H5Tclose(n_type);

    if (vl_data) {
        hsize_t dims[1]  = {1};
        hid_t   space_id = H5I_INVALID_HID;

        space_id = H5Screate_simple(1, dims, NULL);

        H5Treclaim(type_id, space_id, H5P_DEFAULT, buf);

        H5Sclose(space_id);
    }

    if (buf)
        free(buf);
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
h5tools_dump_dcpl(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t dcpl_id,
                  hid_t type_id, hid_t dset_id)
{
    int              nfilters = -1; /* number of filters */
    int              rank;          /* rank */
    int              i;
    unsigned         j;
    unsigned         filt_flags;    /* filter flags */
    unsigned         cd_values[20]; /* filter client data values */
    unsigned         szip_options_mask;
    unsigned         szip_pixels_per_block;
    H5Z_filter_t     filtn; /* filter identification number */
    H5D_fill_value_t fvstatus = H5D_FILL_VALUE_ERROR;
    H5D_alloc_time_t at       = H5D_ALLOC_TIME_ERROR;
    H5D_fill_time_t  ft       = H5D_FILL_TIME_ERROR;
    H5D_layout_t     stl      = H5D_LAYOUT_ERROR;
    size_t           ncols    = 80; /* available output width        */
    size_t           cd_nelmts;     /* filter client number of values */
    off_t            offset;        /* offset of external file     */
    char             f_name[256];   /* filter name */
    char             name[256];     /* external or virtual file name       */
    hsize_t          chsize[64];    /* chunk size in elements */
    hsize_t          size;          /* size of external file   */
    hsize_t          storage_size;
    hsize_t          curr_pos = 0; /* total data element position   */
    h5tools_str_t    buffer;       /* string into which to render   */

    /* setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));
    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    storage_size = H5Dget_storage_size(dset_id);
    if (dcpl_id >= 0)
        nfilters = H5Pget_nfilters(dcpl_id);

    strcpy(f_name, "\0");

    /*-------------------------------------------------------------------------
     * STORAGE_LAYOUT
     *-------------------------------------------------------------------------
     */
    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", STORAGE_LAYOUT, BEGIN);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    if (dcpl_id >= 0)
        stl = H5Pget_layout(dcpl_id);

    switch (stl) {
        case H5D_CHUNKED:
            ctx->indent_level++;
            ctx->need_prefix = true;

            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s ", CHUNKED);

            rank = H5Pget_chunk(dcpl_id, (int)NELMTS(chsize), chsize);
            h5tools_str_append(&buffer, "%s %" PRIuHSIZE, h5tools_dump_header_format->dataspacedimbegin,
                               chsize[0]);
            for (i = 1; i < rank; i++)
                h5tools_str_append(&buffer, ", %" PRIuHSIZE, chsize[i]);
            h5tools_str_append(&buffer, " %s", h5tools_dump_header_format->dataspacedimend);
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);

            ctx->need_prefix = true;

            h5tools_str_reset(&buffer);

            /* if there are filters, print a compression ratio */
            if (nfilters) {
                hsize_t dims[H5S_MAX_RANK];
                hsize_t nelmts = 1;
                double  ratio  = 0;
                int     ok     = 0;

                hid_t  tid        = H5Dget_type(dset_id);
                hid_t  sid        = H5Dget_space(dset_id);
                size_t datum_size = H5Tget_size(tid);
                int    ndims      = H5Sget_simple_extent_dims(sid, dims, NULL);

                /* only print the compression ratio for these filters */
                for (i = 0; i < nfilters && !ok; i++) {
                    cd_nelmts = NELMTS(cd_values);
                    filtn     = H5Pget_filter2(dcpl_id, (unsigned)i, &filt_flags, &cd_nelmts, cd_values,
                                               sizeof(f_name), f_name, NULL);
                    ok        = (filtn >= 0);
                }

                if (ndims && ok) {
                    hsize_t uncomp_size;

                    for (i = 0; i < ndims; i++) {
                        nelmts *= dims[i];
                    }
                    uncomp_size = nelmts * datum_size;

                    /* compression ratio = uncompressed size /  compressed size */

                    if (storage_size != 0)
                        ratio = (double)uncomp_size / (double)storage_size;

                    h5tools_str_append(&buffer, "SIZE %" PRIuHSIZE " (%.3f:1 COMPRESSION)", storage_size,
                                       ratio);
                }
                else
                    h5tools_str_append(&buffer, "SIZE %" PRIuHSIZE, storage_size);

                H5Sclose(sid);
                H5Tclose(tid);
            }
            else {
                h5tools_str_append(&buffer, "SIZE %" PRIuHSIZE, storage_size);
            }
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);
            ctx->indent_level--;
            break;
        case H5D_COMPACT:
            ctx->indent_level++;
            ctx->need_prefix = true;

            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s", COMPACT);
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);

            ctx->need_prefix = true;

            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "SIZE %" PRIuHSIZE, storage_size);
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);
            ctx->indent_level--;
            break;
        case H5D_CONTIGUOUS: {
            int n_external;

            n_external = H5Pget_external_count(dcpl_id);

            ctx->indent_level++;
            if (n_external) {

                /* EXTERNAL FILE */

                ctx->need_prefix = true;

                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "%s", CONTIGUOUS);
                h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);

                ctx->need_prefix = true;

                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "%s %s", EXTERNAL, BEGIN);
                h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);

                ctx->indent_level++;
                for (j = 0; j < (unsigned)n_external; j++) {
                    H5Pget_external(dcpl_id, j, sizeof(name), name, &offset, &size);

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "FILENAME %s SIZE %" PRIuHSIZE, name, size);
                    /* Using %lld with a cast to (long long) is probably the only portable
                     * way to print off_t values. There's no real standard for off_t other
                     * than it must be signed, according to POSIX.
                     */
                    h5tools_str_append(&buffer, " OFFSET %lld", (long long)offset);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);
                }
                ctx->indent_level--;

                ctx->need_prefix = true;

                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "%s", END);
                h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);
            }
            else {
                haddr_t  ioffset;
                uint64_t supported = 0;

                /* NORMAL FILE */

                ctx->need_prefix = true;

                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "%s", CONTIGUOUS);
                h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);

                ctx->need_prefix = true;

                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "SIZE %" PRIuHSIZE, storage_size);
                h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);

                /* Only dump the offset if the VOL connector implements
                 * the functionality.
                 */
                H5VLquery_optional(dset_id, H5VL_SUBCLS_DATASET, H5VL_NATIVE_DATASET_GET_OFFSET, &supported);

                if (supported & H5VL_OPT_QUERY_SUPPORTED) {

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    ioffset = H5Dget_offset(dset_id);
                    if (HADDR_UNDEF == ioffset)
                        h5tools_str_append(&buffer, "OFFSET HADDR_UNDEF");
                    else
                        h5tools_str_append(&buffer, "OFFSET %" PRIuHADDR, ioffset);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);
                }
            }
            ctx->indent_level--;
        } break;

        case H5D_VIRTUAL: {
            char   dsetname[256]; /* virtual dataset name       */
            size_t n_vmaps;

            H5Pget_virtual_count(dcpl_id, &n_vmaps);

            if (n_vmaps) {
                size_t                        curr_vmap;
                ssize_t H5_ATTR_NDEBUG_UNUSED ssize_out;

                ctx->indent_level++;
                for (curr_vmap = 0; curr_vmap < n_vmaps; curr_vmap++) {
                    hid_t virtual_vspace   = H5Pget_virtual_vspace(dcpl_id, curr_vmap);
                    hid_t virtual_srcspace = H5Pget_virtual_srcspace(dcpl_id, curr_vmap);

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s %zu %s ", VDS_MAPPING, curr_vmap, BEGIN);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);

                    ctx->indent_level++;

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s %s", VDS_VIRTUAL, BEGIN);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);

                    ctx->indent_level++;

                    h5tools_print_virtual_selection(virtual_vspace, stream, info, ctx, &buffer, &curr_pos,
                                                    (size_t)ncols);

                    ctx->indent_level--;

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s", END);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s %s", VDS_SOURCE, BEGIN);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);

                    ctx->indent_level++;

                    ssize_out = H5Pget_virtual_filename(dcpl_id, curr_vmap, NULL, 0);
                    assert(ssize_out > 0);
                    assert((size_t)ssize_out < sizeof(name));
                    H5Pget_virtual_filename(dcpl_id, curr_vmap, name, sizeof(name));
                    ssize_out = H5Pget_virtual_dsetname(dcpl_id, curr_vmap, NULL, 0);
                    assert(ssize_out > 0);
                    assert((size_t)ssize_out < sizeof(name));
                    H5Pget_virtual_dsetname(dcpl_id, curr_vmap, dsetname, sizeof(dsetname));

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s %s", VDS_SRC_FILE,
                                       h5tools_dump_header_format->virtualfilenamebegin);
                    h5tools_str_append(&buffer, "%s", name);
                    h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->virtualfilenameend);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s %s", VDS_SRC_DATASET,
                                       h5tools_dump_header_format->virtualdatasetnamebegin);
                    h5tools_str_append(&buffer, "%s", dsetname);
                    h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->virtualdatasetnameend);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);

                    h5tools_print_virtual_selection(virtual_srcspace, stream, info, ctx, &buffer, &curr_pos,
                                                    (size_t)ncols);

                    ctx->indent_level--;

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s", END);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);

                    ctx->indent_level--;

                    ctx->need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "%s", END);
                    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                           (hsize_t)0);
                }
                ctx->indent_level--;
            }
        } break;

        case H5D_LAYOUT_ERROR:
        case H5D_NLAYOUTS:
        default:
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s", "Unknown layout");
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);
    } /*switch*/

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s", END);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    /*-------------------------------------------------------------------------
     * FILTERS
     *-------------------------------------------------------------------------
     */
    if (H5D_VIRTUAL != stl) {
        ctx->need_prefix = true;

        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "%s %s", FILTERS, BEGIN);
        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        ctx->indent_level++;

        if (nfilters) {
            for (i = 0; i < nfilters; i++) {
                cd_nelmts = NELMTS(cd_values);
                filtn     = H5Pget_filter2(dcpl_id, (unsigned)i, &filt_flags, &cd_nelmts, cd_values,
                                           sizeof(f_name), f_name, NULL);

                if (filtn < 0)
                    continue; /* nothing to print for invalid filter */

                ctx->need_prefix = true;

                h5tools_str_reset(&buffer);
                switch (filtn) {
                    case H5Z_FILTER_DEFLATE:
                        h5tools_str_append(&buffer, "%s %s %s %d %s", DEFLATE, BEGIN, DEFLATE_LEVEL,
                                           cd_values[0], END);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);
                        break;
                    case H5Z_FILTER_SHUFFLE:
                        h5tools_str_append(&buffer, "%s", SHUFFLE);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);
                        break;
                    case H5Z_FILTER_FLETCHER32:
                        h5tools_str_append(&buffer, "%s", FLETCHER32);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);
                        break;
                    case H5Z_FILTER_SZIP:
                        szip_options_mask     = cd_values[0];
                        szip_pixels_per_block = cd_values[1];

                        h5tools_str_append(&buffer, "%s %s", SZIP, BEGIN);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);

                        ctx->indent_level++;

                        ctx->need_prefix = true;

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "PIXELS_PER_BLOCK %d", szip_pixels_per_block);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);

                        ctx->need_prefix = true;

                        h5tools_str_reset(&buffer);
                        if (szip_options_mask & H5_SZIP_CHIP_OPTION_MASK)
                            h5tools_str_append(&buffer, "MODE %s", "HARDWARE");
                        else if (szip_options_mask & H5_SZIP_ALLOW_K13_OPTION_MASK)
                            h5tools_str_append(&buffer, "MODE %s", "K13");
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);

                        ctx->need_prefix = true;

                        h5tools_str_reset(&buffer);
                        if (szip_options_mask & H5_SZIP_EC_OPTION_MASK)
                            h5tools_str_append(&buffer, "CODING %s", "ENTROPY");
                        else if (szip_options_mask & H5_SZIP_NN_OPTION_MASK)
                            h5tools_str_append(&buffer, "CODING %s", "NEAREST NEIGHBOUR");
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);

                        ctx->need_prefix = true;

                        h5tools_str_reset(&buffer);
                        if (szip_options_mask & H5_SZIP_LSB_OPTION_MASK)
                            h5tools_str_append(&buffer, "BYTE_ORDER %s", "LSB");
                        else if (szip_options_mask & H5_SZIP_MSB_OPTION_MASK)
                            h5tools_str_append(&buffer, "BYTE_ORDER %s", "MSB");
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);

                        if (szip_options_mask & H5_SZIP_RAW_OPTION_MASK) {
                            ctx->need_prefix = true;

                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, "HEADER %s", "RAW");
                            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                                   (hsize_t)0, (hsize_t)0);
                        }

                        ctx->indent_level--;

                        ctx->need_prefix = true;

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "%s", END);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);
                        break;
                    case H5Z_FILTER_NBIT:
                        h5tools_str_append(&buffer, "%s", NBIT);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);
                        break;
                    case H5Z_FILTER_SCALEOFFSET:
                        h5tools_str_append(&buffer, "%s %s %s %d %s", SCALEOFFSET, BEGIN, SCALEOFFSET_MINBIT,
                                           cd_values[0], END);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);
                        break;
                    default:
                        h5tools_str_append(&buffer, "%s %s", "USER_DEFINED_FILTER", BEGIN);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);

                        ctx->indent_level++;

                        ctx->need_prefix = true;

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "FILTER_ID %d", filtn);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);

                        if (f_name[0] != '\0') {
                            ctx->need_prefix = true;

                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, "COMMENT %s", f_name);
                            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                                   (hsize_t)0, (hsize_t)0);
                        }
                        if (cd_nelmts) {
                            ctx->need_prefix = true;

                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, "%s %s ", "PARAMS", BEGIN);
                            for (j = 0; j < cd_nelmts; j++)
                                h5tools_str_append(&buffer, "%d ", cd_values[j]);
                            h5tools_str_append(&buffer, "%s", END);
                            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                                   (hsize_t)0, (hsize_t)0);
                        }
                        ctx->indent_level--;

                        ctx->need_prefix = true;

                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "%s", END);
                        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols,
                                               (hsize_t)0, (hsize_t)0);
                        break;
                } /*switch*/
            }     /*i*/
        }         /*nfilters*/
        else {
            ctx->need_prefix = true;

            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "NONE");
            h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                   (hsize_t)0);
        }
        ctx->indent_level--;

        ctx->need_prefix = true;

        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "%s", END);
        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
    } /* end if (H5D_VIRTUAL != stl) */

    /*-------------------------------------------------------------------------
     * FILLVALUE
     *-------------------------------------------------------------------------
     */
    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", FILLVALUE, BEGIN);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level++;

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "FILL_TIME ");

    if (dcpl_id >= 0)
        H5Pget_fill_time(dcpl_id, &ft);

    switch (ft) {
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
            h5tools_str_append(&buffer, "%s", "INVALID");
            break;
    }
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s ", "VALUE ");

    if (dcpl_id >= 0)
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
            h5tools_print_fill_value(&buffer, info, ctx, dcpl_id, type_id, dset_id);
            ctx->indent_level++;
            break;
        case H5D_FILL_VALUE_ERROR:
        default:
            h5tools_str_append(&buffer, "%s", "INVALID");
            break;
    }
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
    ctx->indent_level--;

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s", END);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    /*-------------------------------------------------------------------------
     * ALLOCATION_TIME
     *-------------------------------------------------------------------------
     */
    if (H5D_VIRTUAL != stl) {
        ctx->need_prefix = true;

        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "ALLOCATION_TIME %s", BEGIN);
        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        ctx->indent_level++;

        ctx->need_prefix = true;

        h5tools_str_reset(&buffer);

        if (dcpl_id >= 0)
            H5Pget_alloc_time(dcpl_id, &at);

        switch (at) {
            case H5D_ALLOC_TIME_EARLY:
                h5tools_str_append(&buffer, "%s", "H5D_ALLOC_TIME_EARLY");
                break;
            case H5D_ALLOC_TIME_INCR:
                h5tools_str_append(&buffer, "%s", "H5D_ALLOC_TIME_INCR");
                break;
            case H5D_ALLOC_TIME_LATE:
                h5tools_str_append(&buffer, "%s", "H5D_ALLOC_TIME_LATE");
                break;
            case H5D_ALLOC_TIME_ERROR:
            case H5D_ALLOC_TIME_DEFAULT:
            default:
                h5tools_str_append(&buffer, "%s", "INVALID");
                break;
        }
        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

        ctx->indent_level--;

        ctx->need_prefix = true;

        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "%s", END);
        h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);
    } /* end if (H5D_VIRTUAL != stl) */

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_comment
 *
 * Purpose:     prints the comment for the object name
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_comment(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t obj_id)
{
    char         *comment     = NULL;
    ssize_t       cmt_bufsize = -1;
    size_t        buf_size    = 0;
    size_t        ncols       = 80;       /* available output width        */
    h5tools_str_t buffer;                 /* string into which to render   */
    hsize_t       curr_pos = ctx->sm_pos; /* total data element position   */
                                          /* pass to the prefix in h5tools_simple_prefix the total position
                                           * instead of the current stripmine position i; this is necessary
                                           * to print the array indices
                                           */
    uint64_t supported = 0;

    /* Check if comments are supported and return if not */
    H5VLquery_optional(obj_id, H5VL_SUBCLS_OBJECT, H5VL_NATIVE_OBJECT_GET_COMMENT, &supported);

    if (!(supported & H5VL_OPT_QUERY_SUPPORTED))
        return;

    /* setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    cmt_bufsize = H5Oget_comment(obj_id, comment, buf_size);

    /* call H5Oget_comment again with the correct value */
    if (cmt_bufsize > 0) {
        comment = (char *)malloc((size_t)(cmt_bufsize + 1)); /* new_size including null terminator */
        if (comment) {
            cmt_bufsize = H5Oget_comment(obj_id, comment, (size_t)cmt_bufsize);
            if (cmt_bufsize > 0) {
                comment[cmt_bufsize] = '\0'; /* necessary because null char is not returned */

                ctx->need_prefix = true;

                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "COMMENT \"%s\"", comment);
                h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                                       (hsize_t)0);

                h5tools_str_close(&buffer);
            }
            free(comment);
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
h5tools_dump_attribute(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx,
                       const char *attr_name, hid_t attr_id)
{
    h5tools_str_t buffer;                 /* string into which to render   */
    size_t        ncols    = 80;          /* available output width        */
    hsize_t       curr_pos = ctx->sm_pos; /* total data element position   */
                                          /* pass to the prefix in h5tools_simple_prefix the total position
                                           * instead of the current stripmine position i; this is necessary
                                           * to print the array indices
                                           */

    /* setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));

    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s \"%s\" %s", h5tools_dump_header_format->attributebegin, attr_name,
                       h5tools_dump_header_format->attributeblockbegin);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    if (attr_id < 0) {
        error_msg("unable to open attribute \"%s\"\n", attr_name);
    }
    else {
        hid_t type  = H5I_INVALID_HID;
        hid_t space = H5I_INVALID_HID;

        ctx->indent_level++;

        type = H5Aget_type(attr_id);
        h5tools_dump_datatype(stream, info, ctx, type);

        space = H5Aget_space(attr_id);
        h5tools_dump_dataspace(stream, info, ctx, space);

        if (oid_output)
            h5tools_dump_oid(stream, info, ctx, attr_id);

        if (data_output || attr_data_output)
            h5tools_dump_data(stream, info, ctx, attr_id, false);

        ctx->indent_level--;

        H5Tclose(type);
        H5Sclose(space);
        H5Aclose(attr_id);
    }

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);

    if (strlen(h5tools_dump_header_format->attributeblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->attributeblockend);
        if (strlen(h5tools_dump_header_format->attributeend))
            h5tools_str_append(&buffer, " ");
    }
    if (strlen(h5tools_dump_header_format->attributeend))
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
 *-------------------------------------------------------------------------
 */
void
h5tools_print_dims(h5tools_str_t *buffer, hsize_t *s, int dims)
{
    int i;

    for (i = 0; i < dims; i++) {
        h5tools_str_append(buffer, "%" PRIuHSIZE, s[i]);

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
 *-------------------------------------------------------------------------
 */
void
h5tools_print_packed_bits(h5tools_str_t *buffer, hid_t type)
{
    unsigned packed_bits_size = 0;
    hid_t    n_type           = H5Tget_native_type(type, H5T_DIR_DEFAULT);

    if (H5Tget_class(n_type) == H5T_INTEGER) {
        if (H5Tequal(n_type, H5T_NATIVE_SCHAR) == true)
            packed_bits_size = 8 * sizeof(char);
        else if (H5Tequal(n_type, H5T_NATIVE_UCHAR) == true)
            packed_bits_size = 8 * sizeof(unsigned char);
        else if (H5Tequal(n_type, H5T_NATIVE_SHORT) == true)
            packed_bits_size = 8 * sizeof(short);
        else if (H5Tequal(n_type, H5T_NATIVE_USHORT) == true)
            packed_bits_size = 8 * sizeof(unsigned short);
        else if (H5Tequal(n_type, H5T_NATIVE_INT) == true)
            packed_bits_size = 8 * sizeof(int);
        else if (H5Tequal(n_type, H5T_NATIVE_UINT) == true)
            packed_bits_size = 8 * sizeof(unsigned int);
        else if (H5Tequal(n_type, H5T_NATIVE_LONG) == true)
            packed_bits_size = 8 * sizeof(long);
        else if (H5Tequal(n_type, H5T_NATIVE_ULONG) == true)
            packed_bits_size = 8 * sizeof(unsigned long);
        else if (H5Tequal(n_type, H5T_NATIVE_LLONG) == true)
            packed_bits_size = 8 * sizeof(long long);
        else if (H5Tequal(n_type, H5T_NATIVE_ULLONG) == true)
            packed_bits_size = 8 * sizeof(unsigned long long);
        else
            error_msg("Packed Bit not valid for this datatype");
    }

    if ((packed_bits_size > 0) && (packed_data_offset + packed_data_length) > packed_bits_size) {
        error_msg("Packed Bit offset+length value(%u) too large. Max is %d\n",
                  packed_data_offset + packed_data_length, packed_bits_size);
        packed_data_mask = 0;
    }
    h5tools_str_append(buffer, "%s %s=%u %s=%u", PACKED_BITS, PACKED_OFFSET, packed_data_offset,
                       PACKED_LENGTH, packed_data_length);
}

/*-------------------------------------------------------------------------
 * Function:    dump_subsetting_header
 *
 * Purpose:     Dump the subsetting header like specified in the DDL.
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_subsetting_header(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, int dims)
{
    h5tools_str_t buffer;        /* string into which to render   */
    hsize_t       curr_pos = 0;  /* total data element position   */
    size_t        ncols    = 80; /* available output width        */

    /* setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));
    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->subsettingbegin,
                       h5tools_dump_header_format->subsettingblockbegin);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level++;

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->startbegin,
                       h5tools_dump_header_format->startblockbegin);
    h5tools_print_dims(&buffer, ctx->sset->start.data, dims);
    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->startend,
                       h5tools_dump_header_format->startblockend);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->stridebegin,
                       h5tools_dump_header_format->strideblockbegin);
    h5tools_print_dims(&buffer, ctx->sset->stride.data, dims);
    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->strideend,
                       h5tools_dump_header_format->strideblockend);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->countbegin,
                       h5tools_dump_header_format->countblockbegin);

    if (ctx->sset->count.data)
        h5tools_print_dims(&buffer, ctx->sset->count.data, dims);
    else
        h5tools_str_append(&buffer, "DEFAULT");

    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->countend,
                       h5tools_dump_header_format->countblockend);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->need_prefix = true;

    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s ", h5tools_dump_header_format->blockbegin,
                       h5tools_dump_header_format->blockblockbegin);

    if (ctx->sset->block.data)
        h5tools_print_dims(&buffer, ctx->sset->block.data, dims);
    else
        h5tools_str_append(&buffer, "DEFAULT");

    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->blockend,
                       h5tools_dump_header_format->blockblockend);
    h5tools_render_element(stream, info, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0, (hsize_t)0);

    ctx->indent_level--;

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_reference
 *
 * Purpose:     Dump reference data
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_reference(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t container,
                       H5R_ref_t *ref_buf, int ndims)
{
    hid_t             new_obj_id   = H5I_INVALID_HID;
    hid_t             new_obj_sid  = H5I_INVALID_HID;
    hsize_t           elmt_counter = 0;  /*counts the # elements printed. */
    size_t            ncols        = 80; /* available output width        */
    int               i;
    hsize_t           curr_pos = 0; /* total data element position   */
    h5tools_str_t     buffer;       /* string into which to render   */
    h5tools_context_t datactx;      /* print context  */

    H5TOOLS_START_DEBUG(" ");

    datactx = *ctx; /* print context  */
    /* Assume entire data space to be printed */
    datactx.need_prefix = true;

    memset(&buffer, 0, sizeof(h5tools_str_t));
    for (i = 0; i < ndims; i++, datactx.cur_elmt++, elmt_counter++) {
        H5O_type_t obj_type = -1; /* Object type */
        H5R_type_t ref_type;      /* Reference type */

        H5TOOLS_DEBUG("reference loop:%d with curr_pos=%ld", i, curr_pos);

        datactx.need_prefix = true;
        h5tools_str_reset(&buffer);
        H5TOOLS_DEBUG("reference loop - h5tools_str_sprint with H5T_STD_REF:%d", i);
        h5tools_str_sprint(&buffer, info, container, H5T_STD_REF, &ref_buf[i], &datactx);
        h5tools_render_element(stream, info, &datactx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)i,
                               (hsize_t)ndims);

        ref_type = H5Rget_type((const H5R_ref_t *)&ref_buf[i]);
        switch (ref_type) {
            case H5R_OBJECT1:
                H5TOOLS_DEBUG("ref_type is H5R_OBJECT1");
                if (H5Rget_obj_type3(&ref_buf[i], H5P_DEFAULT, &obj_type) >= 0) {
                    switch (obj_type) {
                        case H5O_TYPE_DATASET:
                            if ((new_obj_id = H5Ropen_object(&ref_buf[i], H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                datactx.indent_level++;
                                h5tools_dump_data(stream, info, &datactx, new_obj_id, true);
                                // h5tools_dump_dset(stream, info, &datactx, new_obj_id);
                                datactx.indent_level--;
                                if (H5Dclose(new_obj_id) < 0)
                                    H5TOOLS_INFO("H5Dclose H5R_OBJECT1:H5O_TYPE_DATASET failed");
                            }
                            else
                                H5TOOLS_INFO("H5Ropen_object H5R_OBJECT1:H5O_TYPE_DATASET failed");
                            break;

                        case H5O_TYPE_GROUP:
                        case H5O_TYPE_NAMED_DATATYPE:
                        case H5O_TYPE_MAP:
                        case H5O_TYPE_UNKNOWN:
                        case H5O_TYPE_NTYPES:
                        default:
                            break;
                    } /* end switch */
                }
                else
                    H5TOOLS_INFO("H5Rget_obj_type3 H5R_OBJECT1 failed");
                break;
            case H5R_DATASET_REGION1:
                H5TOOLS_DEBUG("ref_type is H5R_DATASET_REGION1");
                if ((new_obj_id = H5Ropen_object(&ref_buf[i], H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                    datactx.indent_level++;
                    h5tools_dump_data(stream, info, &datactx, new_obj_id, true);
                    // h5tools_dump_dset(stream, info, &datactx, new_obj_id);
                    datactx.indent_level--;
                    if (H5Dclose(new_obj_id) < 0)
                        H5TOOLS_INFO("H5Dclose H5R_DATASET_REGION1 failed");
                }
                else
                    H5TOOLS_INFO("H5Ropen_object H5R_DATASET_REGION1 failed");
                break;
            case H5R_OBJECT2:
                H5TOOLS_DEBUG("ref_type is H5R_OBJECT2");
                if (H5Rget_obj_type3(&ref_buf[i], H5P_DEFAULT, &obj_type) >= 0) {
                    switch (obj_type) {
                        case H5O_TYPE_GROUP:
                            break;

                        case H5O_TYPE_DATASET:
                            if ((new_obj_id = H5Ropen_object(&ref_buf[i], H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                datactx.indent_level++;
                                h5tools_dump_data(stream, info, &datactx, new_obj_id, true);
                                // h5tools_dump_dset(stream, info, &datactx, new_obj_id);
                                datactx.indent_level--;
                                if (H5Oclose(new_obj_id) < 0)
                                    H5TOOLS_INFO("H5Oclose H5R_OBJECT2 failed");
                            }
                            else
                                H5TOOLS_INFO("H5Ropen_object H5R_OBJECT2 failed");
                            break;

                        case H5O_TYPE_NAMED_DATATYPE:
                            break;

                        case H5O_TYPE_MAP:
                        case H5O_TYPE_UNKNOWN:
                        case H5O_TYPE_NTYPES:
                        default:
                            break;
                    } /* end switch */
                }
                else
                    H5TOOLS_INFO("H5Rget_obj_type3 H5R_OBJECT2 failed");
                break;
            case H5R_DATASET_REGION2:
                H5TOOLS_DEBUG("ref_type is H5R_DATASET_REGION2");

                if (info->line_ncols > 0)
                    ncols = info->line_ncols;

                /* if (new_obj_id < 0) - could mean that no reference was written do not throw failure */
                if ((new_obj_id = H5Ropen_object(&ref_buf[i], H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    H5TOOLS_INFO("H5Ropen_object H5R_DATASET_REGION2 failed");
                else {
                    if ((new_obj_sid = H5Ropen_region(&ref_buf[i], H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                        if (h5tools_is_zero(&ref_buf[i], H5Tget_size(H5T_STD_REF))) {
                            H5TOOLS_DEBUG("NULL H5R_DATASET_REGION2");

                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, " {");
                            h5tools_render_element(stream, info, &datactx, &buffer, &curr_pos, (size_t)ncols,
                                                   (hsize_t)0, (hsize_t)0);

                            datactx.need_prefix = true;
                            datactx.indent_level++;
                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, "NULL");
                            h5tools_render_element(stream, info, &datactx, &buffer, &curr_pos, (size_t)ncols,
                                                   (hsize_t)0, (hsize_t)0);
                            datactx.indent_level--;
                            datactx.need_prefix = true;

                            h5tools_str_reset(&buffer);
                            h5tools_str_append(&buffer, "}");
                            h5tools_render_element(stream, info, &datactx, &buffer, &curr_pos, (size_t)ncols,
                                                   (hsize_t)0, (hsize_t)0);
                        }
                        else {
                            H5S_sel_type region_type;

                            region_type = H5Sget_select_type(new_obj_sid);
                            if (region_type == H5S_SEL_POINTS) {
                                /* Print point information */
                                H5TOOLS_DEBUG("H5S_SEL_POINTS H5R_DATASET_REGION2");
                                h5tools_dump_region_data_points(new_obj_sid, new_obj_id, stream, info,
                                                                &datactx, &buffer, &curr_pos, ncols,
                                                                (hsize_t)i, elmt_counter);
                            }
                            else if (region_type == H5S_SEL_HYPERSLABS) {
                                /* Print block information */
                                H5TOOLS_DEBUG("H5S_SEL_HYPERSLABS H5R_DATASET_REGION2");
                                h5tools_dump_region_data_blocks(new_obj_sid, new_obj_id, stream, info,
                                                                &datactx, &buffer, &curr_pos, ncols,
                                                                (hsize_t)i, elmt_counter);
                            }
                            else
                                H5TOOLS_INFO("invalid region type");
                        } /* end else to if (h5tools_is_zero(... */
                        if (H5Sclose(new_obj_sid) < 0)
                            H5TOOLS_INFO("H5Sclose H5R_DATASET_REGION2 failed");
                    }
                    else
                        H5TOOLS_INFO("H5Ropen_region H5R_DATASET_REGION2 failed");
                    if (H5Dclose(new_obj_id) < 0)
                        H5TOOLS_INFO("H5Dclose H5R_DATASET_REGION2 failed");
                }
                break;
            case H5R_ATTR:
                H5TOOLS_DEBUG("ref_type is H5R_ATTR");
                if ((new_obj_id = H5Ropen_attr(&ref_buf[i], H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                    h5tools_dump_region_attribute(new_obj_id, stream, info, &datactx, &buffer, &curr_pos,
                                                  (size_t)ncols, (hsize_t)0, (hsize_t)0);
                    if (H5Aclose(new_obj_id) < 0)
                        H5TOOLS_INFO("H5Aclose H5R_ATTR failed");
                }
                else {
                    H5TOOLS_DEBUG("NULL H5R_ATTR");

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, " {");
                    h5tools_render_element(stream, info, &datactx, &buffer, &curr_pos, (size_t)ncols,
                                           (hsize_t)0, (hsize_t)0);

                    datactx.need_prefix = true;
                    datactx.indent_level++;
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "NULL");
                    h5tools_render_element(stream, info, &datactx, &buffer, &curr_pos, (size_t)ncols,
                                           (hsize_t)0, (hsize_t)0);
                    datactx.indent_level--;
                    datactx.need_prefix = true;

                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "}");
                    h5tools_render_element(stream, info, &datactx, &buffer, &curr_pos, (size_t)ncols,
                                           (hsize_t)0, (hsize_t)0);

                    H5TOOLS_INFO("H5Ropen_attr H5R_ATTR failed");
                }
                break;
            case H5R_BADTYPE:
            case H5R_MAXTYPE:
            default:
                break;
        } /* end switch */

        if (H5Rdestroy(&ref_buf[i]) < 0)
            H5TOOLS_INFO("H5Rdestroy failed");

        H5TOOLS_DEBUG("finished reference loop:%d", i);
    } /* end for(i = 0; i < ndims; i++, ctx->cur_elmt++, elmt_counter++) */

    h5tools_str_close(&buffer);

    H5TOOLS_ENDDEBUG(" ");
}

/*-------------------------------------------------------------------------
 * Function:    dump_data
 *
 * Purpose:     Dump attribute, obj_data is false, or dataset data, obj_data is true
 *
 * Return:      void
 *-------------------------------------------------------------------------
 */
void
h5tools_dump_data(FILE *stream, const h5tool_format_t *info, h5tools_context_t *ctx, hid_t obj_id,
                  int obj_data)
{
    H5S_class_t       space_type;
    int               ndims;
    hid_t             space  = H5I_INVALID_HID;
    hid_t             f_type = H5I_INVALID_HID;
    hsize_t           total_size[H5S_MAX_RANK];
    int               status = -1;
    h5tools_context_t datactx;       /* print context  */
    h5tools_str_t     buffer;        /* string into which to render   */
    hsize_t           curr_pos = 0;  /* total data element position   */
    size_t            ncols    = 80; /* available output width        */
    h5tool_format_t   string_dataformat;
    h5tool_format_t   outputformat;
    H5R_ref_t        *ref_buf = NULL;

    H5TOOLS_START_DEBUG(" file=%p", (void *)stream);
    H5TOOLS_DEBUG("rawdata file=%p", (void *)rawdatastream);
    /* setup */
    memset(&buffer, 0, sizeof(h5tools_str_t));
    if (info->line_ncols > 0)
        ncols = info->line_ncols;

    outputformat      = *info;
    string_dataformat = *info;
    /* print the matrix indices */
    string_dataformat.pindex = ctx->display_index;

    if (obj_data) {
        H5TOOLS_DEBUG("dataset");
        f_type = H5Dget_type(obj_id);
        space  = H5Dget_space(obj_id);
    }
    else {
        H5TOOLS_DEBUG("attribute");
        f_type = H5Aget_type(obj_id);
        space  = H5Aget_space(obj_id);
    }

    if (string_dataformat.pindex) {
        string_dataformat.idx_fmt   = "(%s): ";
        string_dataformat.idx_n_fmt = "%" PRIuHSIZE;
        string_dataformat.idx_sep   = ",";
        string_dataformat.line_pre  = "%s";
    }
    info = &string_dataformat;

    if (ctx->sset && obj_data) {

        h5tools_dump_subsetting_header(stream, &outputformat, ctx, H5Sget_simple_extent_ndims(space));

        ctx->indent_level++;
    }

    ctx->need_prefix = true;
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s %s", h5tools_dump_header_format->databegin,
                       h5tools_dump_header_format->datablockbegin);
    h5tools_render_element(stream, &outputformat, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                           (hsize_t)0);

    if (H5Tget_class(f_type) == H5T_REFERENCE) {
        ctx->indent_level++;
        datactx = *ctx;
        H5TOOLS_DEBUG("reference class type");
        if (!H5Tequal(f_type, H5T_STD_REF) && !H5Tequal(f_type, H5T_STD_REF_DSETREG) &&
            !H5Tequal(f_type, H5T_STD_REF_OBJ)) {
            H5TOOLS_GOTO_DONE_NO_RET();
        }

        ndims = (int)H5Sget_simple_extent_npoints(space);
        H5TOOLS_DEBUG("ndims=%d - datactx.ndims=%d", ndims, datactx.ndims);

        /* Assume entire data space to be printed */
        H5Sget_simple_extent_dims(space, total_size, NULL);
        init_acc_pos(datactx.ndims, total_size, datactx.acc, datactx.pos, datactx.p_min_idx);

        datactx.need_prefix = true;

        if (NULL !=
            (ref_buf = (H5R_ref_t *)calloc(MAX(sizeof(unsigned), sizeof(H5R_ref_t)), (size_t)ndims))) {
            if (obj_data) {
                if (H5Dread(obj_id, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_buf) < 0) {
                    free(ref_buf);
                    H5TOOLS_INFO("H5Dread reference failed");
                    H5TOOLS_GOTO_DONE_NO_RET();
                }
            }
            else {
                if (H5Aread(obj_id, H5T_STD_REF, ref_buf) < 0) {
                    free(ref_buf);
                    H5TOOLS_INFO("H5Aread reference failed");
                    H5TOOLS_GOTO_DONE_NO_RET();
                }
            }
            h5tools_dump_reference(stream, &outputformat, &datactx, obj_id, ref_buf, ndims);
            free(ref_buf);
        }
        ctx->indent_level--;
    }
    else {
        H5TOOLS_DEBUG("Print all the values");
        datactx           = *ctx;
        string_dataformat = *info;
        if ((datactx.display_char && H5Tget_size(f_type) == 1) && (H5Tget_class(f_type) == H5T_INTEGER)) {
            H5TOOLS_DEBUG("Print 1-byte integer data as an ASCII character string");
            /*
             * Print 1-byte integer data as an ASCII character string
             * instead of integers if the `-r' or `--string' command-line
             * option was given.
             *
             * We don't want to modify the global dataformat, so make a
             * copy of it instead.
             */
            string_dataformat.idx_fmt = "\"";
            datactx.indent_level++;
            datactx.need_prefix = true;
            h5tools_simple_prefix(stream, &string_dataformat, &datactx, (hsize_t)0, 0);

            string_dataformat.line_multi_new = 1;
            string_dataformat.str_repeat     = 8;
            string_dataformat.ascii          = true;
            string_dataformat.elmt_suf1      = "";
            string_dataformat.elmt_suf2      = "";
            string_dataformat.line_suf       = "\"";
        }
        else {
            datactx.need_prefix = true;
        }

        /* Print all the values. */
        if (obj_data) {
            H5TOOLS_DEBUG("h5tools_dump_dset");
            status = h5tools_dump_dset(stream, &string_dataformat, &datactx, obj_id);
        }
        else {
            /* need to call h5tools_dump_mem for the attribute data */
            space_type = H5Sget_simple_extent_type(space);
            if (space_type == H5S_NULL || space_type == H5S_NO_CLASS) {
                status = SUCCEED;
            }
            else {
                H5TOOLS_DEBUG("call h5tools_dump_mem");
                status = h5tools_dump_mem(stream, &string_dataformat, &datactx, obj_id);
            }
        }
        if (datactx.display_char && H5Tget_size(f_type) == 1 && H5Tget_class(f_type) == H5T_INTEGER) {
            H5TOOLS_DEBUG("Print 1-byte integer data as an ASCII character string eol=%s",
                          string_dataformat.line_suf);
            datactx.need_prefix              = false;
            string_dataformat.arr_linebreak  = 0;
            string_dataformat.idx_fmt        = "";
            string_dataformat.line_multi_new = 0;
            string_dataformat.line_suf       = "";
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"");
            h5tools_render_element(stream, &string_dataformat, &datactx, &buffer, &curr_pos, (size_t)ncols,
                                   (hsize_t)0, (hsize_t)0);
        }
        H5TOOLS_DEBUG("Print all the values Complete");

        if (status == FAIL) {
            error_msg("unable to print data\n");
            h5tools_setstatus(EXIT_FAILURE);
        }
    }
done:
    H5Sclose(space);
    H5Tclose(f_type);

    ctx->need_prefix = true;
    h5tools_simple_prefix(stream, &outputformat, ctx, (hsize_t)0, 0);

    h5tools_str_reset(&buffer);
    if (strlen(h5tools_dump_header_format->datablockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datablockend);
        if (strlen(h5tools_dump_header_format->dataend))
            h5tools_str_append(&buffer, " ");
    }

    if (strlen(h5tools_dump_header_format->dataend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->dataend);
    h5tools_render_element(stream, &outputformat, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                           (hsize_t)0);

    if (ctx->sset && obj_data) {
        ctx->indent_level--;

        ctx->need_prefix = true;
        h5tools_simple_prefix(stream, &outputformat, ctx, (hsize_t)0, 0);

        h5tools_str_reset(&buffer);
        if (strlen(h5tools_dump_header_format->subsettingblockend)) {
            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->subsettingblockend);
            if (strlen(h5tools_dump_header_format->subsettingend))
                h5tools_str_append(&buffer, " ");
        }
        if (strlen(h5tools_dump_header_format->subsettingend))
            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->subsettingend);
        h5tools_render_element(stream, &outputformat, ctx, &buffer, &curr_pos, (size_t)ncols, (hsize_t)0,
                               (hsize_t)0);
    }

    h5tools_str_close(&buffer);
    H5TOOLS_ENDDEBUG(" ");
}
