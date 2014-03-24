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
#include <stdio.h>
#include <stdlib.h>

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_dump.h"
#include "h5tools_utils.h"
#include "h5tools_ref.h"
#include "h5trav.h"
#include "h5dump_extern.h"
#include "h5dump_ddl.h"

typedef struct {
    hid_t fid;                      /* File ID being traversed */
    char *op_name;					/* Object name wanted */
} trav_handle_udata_t;

typedef struct {
	char *path;                     /* Path of object being searched */
    char *op_name;					/* Object name wanted */
} trav_attr_udata_t;

/* callback function used by H5Literate() */
static herr_t   dump_all_cb(hid_t group, const char *name, const H5L_info_t *linfo, void *op_data);
static int      dump_extlink(hid_t group, const char *linkname, const char *objname);

/*-------------------------------------------------------------------------
 * Function:    dump_datatype
 *
 * Purpose:     Dump the datatype. Datatype can be HDF5 predefined
 *              atomic datatype or committed/transient datatype.
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
dump_datatype(hid_t type)
{
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;

    h5dump_type_table = type_table;
    h5tools_dump_datatype(rawoutstream, outputformat, &ctx, type);
    h5dump_type_table = NULL;
}

/*-------------------------------------------------------------------------
 * Function:    dump_dataspace
 *
 * Purpose:     Dump the dataspace. Dataspace can be named dataspace,
 *              array, or others.
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
dump_dataspace(hid_t space)
{
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;
   
    h5tools_dump_dataspace(rawoutstream, outputformat, &ctx, space);
}


/*-------------------------------------------------------------------------
 * Function:    dump_attr_cb
 *
 * Purpose:     attribute function callback called by H5Aiterate2, displays the attribute
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: Pedro Vicente, October 4, 2007
 *  Added H5A_info_t parameter to conform with H5Aiterate2
 *
 *-------------------------------------------------------------------------
 */
herr_t
dump_attr_cb(hid_t oid, const char *attr_name, const H5A_info_t UNUSED *info, void UNUSED *_op_data)
{
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;
    h5tool_format_t   string_dataformat;
    
    hid_t       attr_id;
    herr_t      ret = SUCCEED;

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;
    
    attr_id = H5Aopen(oid, attr_name, H5P_DEFAULT);
    oid_output = display_oid;
    data_output = display_data;
    attr_data_output = display_attr_data;
    
    string_dataformat = *outputformat;

    if (fp_format) {
        string_dataformat.fmt_double = fp_format;
        string_dataformat.fmt_float = fp_format;
    }

    if (h5tools_nCols==0) {
        string_dataformat.line_ncols = 65535;
        string_dataformat.line_per_line = 1;
    }
    else
        string_dataformat.line_ncols = h5tools_nCols;

    string_dataformat.do_escape = display_escape;
    outputformat = &string_dataformat;

    h5dump_type_table = type_table;
    h5tools_dump_attribute(rawoutstream, outputformat, &ctx, attr_name, attr_id, display_ai, display_char);
    h5dump_type_table = NULL;

    if(attr_id < 0) {
        h5tools_setstatus(EXIT_FAILURE);
        ret = FAIL;
    }
    
    return ret;
}


/*-------------------------------------------------------------------------
 * Function:    dump_all_cb
 *
 * Purpose:     function callback called by H5Literate,
 *                displays everything in the specified object
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *  RMcG, November 2000
 *   Added XML support. Also, optionally checks the op_data argument
 *
 * PVN, May 2008
 *   Dump external links
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dump_all_cb(hid_t group, const char *name, const H5L_info_t *linfo, void UNUSED *op_data)
{
    hid_t       obj;
    herr_t      ret = SUCCEED;
    char       *obj_path = NULL;    /* Full path of object */
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;
    h5tool_format_t   string_dataformat;
    hsize_t     curr_pos = 0;        /* total data element position   */

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;
    
    string_dataformat = *outputformat;

    if (fp_format) {
        string_dataformat.fmt_double = fp_format;
        string_dataformat.fmt_float = fp_format;
    }

    if (h5tools_nCols==0) {
        string_dataformat.line_ncols = 65535;
        string_dataformat.line_per_line = 1;
    }
    else
        string_dataformat.line_ncols = h5tools_nCols;

    string_dataformat.do_escape = display_escape;
    outputformat = &string_dataformat;

    /* Build the object's path name */
    obj_path = (char *)HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    if(!obj_path) {
        ret = FAIL;
        goto done;
    } 
    
    HDstrcpy(obj_path, prefix);
    HDstrcat(obj_path, "/");
    HDstrcat(obj_path, name);

    if(linfo->type == H5L_TYPE_HARD) {
        H5O_info_t  oinfo;

        /* Stat the object */
        if(H5Oget_info_by_name(group, name, &oinfo, H5P_DEFAULT) < 0) {
            error_msg("unable to get object information for \"%s\"\n", name);
            h5tools_setstatus(EXIT_FAILURE);
            ret = FAIL;
            goto done;
        } /* end if */

        switch(oinfo.type) {
        case H5O_TYPE_GROUP:
            if((obj = H5Gopen2(group, name, H5P_DEFAULT)) < 0)  {
                error_msg("unable to dump group \"%s\"\n", name);
                h5tools_setstatus(EXIT_FAILURE);
                ret = FAIL;
            }
            else {
                char *old_prefix; /* Pointer to previous prefix */

                /* Keep copy of prefix before iterating into group */
                old_prefix = HDstrdup(prefix);
                HDassert(old_prefix);

                /* Append group name to prefix */
                add_prefix(&prefix, &prefix_len, name);

                /* Iterate into group */
                dump_function_table->dump_group_function(obj, name);

                /* Restore old prefix name */
                HDstrcpy(prefix, old_prefix);
                HDfree(old_prefix);

                /* Close group */
                H5Gclose(obj);
            }
            break;

        case H5O_TYPE_DATASET:
            if((obj = H5Dopen2(group, name, H5P_DEFAULT)) >= 0) {
                if(oinfo.rc > 1 || hit_elink) {
                    obj_t  *found_obj;    /* Found object */

                    found_obj = search_obj(dset_table, oinfo.addr);

                    if(found_obj == NULL) {
                        ctx.indent_level++;

                        ctx.need_prefix = TRUE;
                        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                        /* Render the element */
                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "%s \"%s\" %s",
                                h5tools_dump_header_format->datasetbegin, name,
                                h5tools_dump_header_format->datasetblockbegin);
                        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                        error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);

                        ctx.need_prefix = TRUE;
                        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                        /* Render the element */
                        h5tools_str_reset(&buffer);
                        if(HDstrlen(h5tools_dump_header_format->datasetblockend)) {
                            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datasetblockend);
                            if(HDstrlen(h5tools_dump_header_format->datasetend))
                                h5tools_str_append(&buffer, " ");
                        }
                        if(HDstrlen(h5tools_dump_header_format->datasetend))
                            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datasetend);
                        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                        ctx.indent_level--;

                        h5tools_setstatus(EXIT_FAILURE);
                        ret = FAIL;
                        H5Dclose(obj);
                        goto done;
                    } 
                    else if(found_obj->displayed) {
                        ctx.need_prefix = TRUE;
                        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                        /* Render the element */
                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "%s \"%s\" %s",
                                h5tools_dump_header_format->datasetbegin, name,
                                h5tools_dump_header_format->datasetblockbegin);
                        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                        ctx.indent_level++;

                        ctx.need_prefix = TRUE;

                        /* Render the element */
                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "%s \"%s\"", HARDLINK, found_obj->objname);
                        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                        ctx.indent_level--;

                        ctx.need_prefix = TRUE;
                        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                        /* Render the element */
                        h5tools_str_reset(&buffer);
                        if(HDstrlen(h5tools_dump_header_format->datasetblockend)) {
                            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datasetblockend);
                            if(HDstrlen(h5tools_dump_header_format->datasetend))
                                h5tools_str_append(&buffer, " ");
                        }
                        if(HDstrlen(h5tools_dump_header_format->datasetend))
                            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datasetend);
                        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                        H5Dclose(obj);
                        goto done;
                    } 
                    else {
                        found_obj->displayed = TRUE;
                    }
                } /* end if */

                dump_function_table->dump_dataset_function(obj, name, NULL);
                H5Dclose(obj);
            } 
            else {
                error_msg("unable to dump dataset \"%s\"\n", name);
                h5tools_setstatus(EXIT_FAILURE);
                ret = FAIL;
            }
            break;

        case H5O_TYPE_NAMED_DATATYPE:
            if((obj = H5Topen2(group, name, H5P_DEFAULT)) < 0) {
                error_msg("unable to dump datatype \"%s\"\n", name);
                h5tools_setstatus(EXIT_FAILURE);
                ret = FAIL;
            } 
            else {
                dump_function_table->dump_named_datatype_function(obj, name);
                H5Tclose(obj);
            }
            break;

        default:
            error_msg("unknown object \"%s\"\n", name);
            h5tools_setstatus(EXIT_FAILURE);
            ret = FAIL;
        }
    } /* end if */
    else {
        char       *targbuf;

        switch(linfo->type) {
        case H5L_TYPE_SOFT:
            targbuf = (char *)HDmalloc(linfo->u.val_size);
            HDassert(targbuf);

            ctx.need_prefix = TRUE;

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s \"%s\" %s",
                    h5tools_dump_header_format->softlinkbegin, name,
                    h5tools_dump_header_format->softlinkblockbegin);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.indent_level++;

            if(H5Lget_val(group, name, targbuf, linfo->u.val_size, H5P_DEFAULT) < 0) {
                error_msg("unable to get link value\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret = FAIL;
            } 
            else {
                /* print the value of a soft link */
                /* Standard DDL: no modification */
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "LINKTARGET \"%s\"", targbuf);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            }

            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            if(HDstrlen(h5tools_dump_header_format->softlinkblockend)) {
                h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->softlinkblockend);
                if(HDstrlen(h5tools_dump_header_format->softlinkend))
                    h5tools_str_append(&buffer, " ");
            }
            if(HDstrlen(h5tools_dump_header_format->softlinkend))
                h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->softlinkend);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            HDfree(targbuf);
            break;

        case H5L_TYPE_EXTERNAL:
            targbuf = (char *)HDmalloc(linfo->u.val_size);
            HDassert(targbuf);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s \"%s\" %s",
                    h5tools_dump_header_format->extlinkbegin, name,
                    h5tools_dump_header_format->extlinkblockbegin);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            if(H5Lget_val(group, name, targbuf, linfo->u.val_size, H5P_DEFAULT) < 0) {
                indentation(dump_indent);
                error_msg("unable to get external link value\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret = FAIL;
            } /* end if */
            else {
                const char *filename;
                const char *targname;

                if(H5Lunpack_elink_val(targbuf, linfo->u.val_size, NULL, &filename, &targname) < 0) {
                    indentation(dump_indent);
                    error_msg("unable to unpack external link value\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret = FAIL;
                } /* end if */
                else {
                    ctx.indent_level++;

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "TARGETFILE \"%s\"", filename);
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "TARGETPATH \"%s\"", targname);
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                    /* dump the external link */
                    dump_extlink(group, name, targname);
                    ctx.indent_level--;
                } /* end else */
            } /* end else */
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            if(HDstrlen(h5tools_dump_header_format->extlinkblockend)) {
                h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->extlinkblockend);
                if(HDstrlen(h5tools_dump_header_format->extlinkend))
                    h5tools_str_append(&buffer, " ");
            }
            if(HDstrlen(h5tools_dump_header_format->extlinkend))
                h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->extlinkend);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            HDfree(targbuf);
            break;

        default:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s \"%s\" %s",
                    h5tools_dump_header_format->udlinkbegin, name,
                    h5tools_dump_header_format->udlinkblockbegin);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "LINKCLASS %d", linfo->type);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
            /* Render the element */
            h5tools_str_reset(&buffer);
            if(HDstrlen(h5tools_dump_header_format->udlinkblockend)) {
                h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->udlinkblockend);
                if(HDstrlen(h5tools_dump_header_format->udlinkend))
                    h5tools_str_append(&buffer, " ");
            }
            if(HDstrlen(h5tools_dump_header_format->udlinkend))
                h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->udlinkend);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            break;
        } /* end switch */
    } /* end else */

done:

    h5tools_str_close(&buffer);
    
    if(obj_path)
        HDfree(obj_path);
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    attr_iteration
 *
 * Purpose:     Iterate and display attributes within the specified group
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
attr_iteration(hid_t gid, unsigned attr_crt_order_flags)
{
    /* attribute iteration: if there is a request to do H5_INDEX_CRT_ORDER and tracking order is set
       in the group for attributes, then, sort by creation order, otherwise by name */
    if(include_attrs) {
        if((sort_by == H5_INDEX_CRT_ORDER) && (attr_crt_order_flags & H5P_CRT_ORDER_TRACKED)) {
            if(H5Aiterate2(gid, sort_by, sort_order, NULL, dump_attr_cb, NULL) < 0) {
                error_msg("error getting attribute information\n");
                h5tools_setstatus(EXIT_FAILURE);
            } /* end if */
        } /* end if */
        else {
            if(H5Aiterate2(gid, H5_INDEX_NAME, sort_order, NULL, dump_attr_cb, NULL) < 0) {
                error_msg("error getting attribute information\n");
                h5tools_setstatus(EXIT_FAILURE);
            } /* end if */
        } /* end else */
    }
}

/*-------------------------------------------------------------------------
 * Function:    link_iteration
 *
 * Purpose:     Iterate and display links within the specified group
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
link_iteration(hid_t gid, unsigned crt_order_flags)
{

    /* if there is a request to do H5_INDEX_CRT_ORDER and tracking order is set
       in the group, then, sort by creation order, otherwise by name */
    if((sort_by == H5_INDEX_CRT_ORDER) && (crt_order_flags & H5P_CRT_ORDER_TRACKED))
        H5Literate(gid, sort_by, sort_order, NULL, dump_all_cb, NULL);
    else
        H5Literate(gid, H5_INDEX_NAME, sort_order, NULL, dump_all_cb, NULL);
}

/*-------------------------------------------------------------------------
 * Function:    dump_named_datatype
 *
 * Purpose:     Dump named datatype
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *  Pedro Vicente, March 27, 2006
 *   added display of attributes
 *  Pedro Vicente, October 4, 2007, added parameters to H5Aiterate2() to allow for
 *   other iteration orders
 *
 *-------------------------------------------------------------------------
 */
void
dump_named_datatype(hid_t tid, const char *name)
{
    H5O_info_t        oinfo;
    unsigned          attr_crt_order_flags;
    hid_t             tcpl_id = -1;  /* datatype creation property list ID */
    hsize_t           curr_pos = 0;        /* total data element position   */
    h5tools_str_t     buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;
    h5tool_format_t   string_dataformat;

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;
    
    string_dataformat = *outputformat;

    if (fp_format) {
        string_dataformat.fmt_double = fp_format;
        string_dataformat.fmt_float = fp_format;
    }

    if (h5tools_nCols==0) {
        string_dataformat.line_ncols = 65535;
        string_dataformat.line_per_line = 1;
    }
    else
        string_dataformat.line_ncols = h5tools_nCols;

    string_dataformat.do_escape = display_escape;
    outputformat = &string_dataformat;

    if ((tcpl_id = H5Tget_create_plist(tid)) < 0) {
        error_msg("error in getting creation property list ID\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    /* query the creation properties for attributes */
    if (H5Pget_attr_creation_order(tcpl_id, &attr_crt_order_flags) < 0) {
        error_msg("error in getting creation properties\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    if(H5Pclose(tcpl_id) < 0) {
        error_msg("error in closing creation property list ID\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    ctx.need_prefix = TRUE;
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s \"%s\" %s",
                        h5tools_dump_header_format->datatypebegin, name,
                        h5tools_dump_header_format->datatypeblockbegin);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    H5Oget_info(tid, &oinfo);

    /* Must check for uniqueness of all objects if we've traversed an elink,
     * otherwise only check if the reference count > 1.
     */
    if(oinfo.rc > 1 || hit_elink) {
        obj_t  *found_obj;    /* Found object */

        found_obj = search_obj(type_table, oinfo.addr);

        if (found_obj == NULL) {
            error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        else if (found_obj->displayed) {
            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s \"%s\"", HARDLINK, found_obj->objname);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            goto done;
        }
        else
            found_obj->displayed = TRUE;
    } /* end if */
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_print_datatype(rawoutstream, &buffer, outputformat, &ctx, tid, FALSE);

    if(H5Tget_class(tid) != H5T_COMPOUND) {
        h5tools_str_append(&buffer, ";");
    }
    
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    /* print attributes */
    dump_indent += COL;

    attr_iteration(tid, attr_crt_order_flags);

    dump_indent -= COL;

done:
    /* Render the element */
    h5tools_str_reset(&buffer);
    if(HDstrlen(h5tools_dump_header_format->datatypeblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datatypeblockend);
        if(HDstrlen(h5tools_dump_header_format->datatypeend))
            h5tools_str_append(&buffer, " ");
    }
    if(HDstrlen(h5tools_dump_header_format->datatypeend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datatypeend);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_group
 *
 * Purpose:     Dump everything within the specified group
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 * Call to dump_all_cb -- add parameter to select everything.
 *
 * Pedro Vicente, October 1, 2007
 *  handle several iteration orders for attributes and groups
 *
 *-------------------------------------------------------------------------
 */
void
dump_group(hid_t gid, const char *name)
{
    H5O_info_t  oinfo;
    hid_t       dset;
    hid_t       type;
    hid_t       gcpl_id;
    unsigned    crt_order_flags;
    unsigned    attr_crt_order_flags;
    char        type_name[1024];
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;
    h5tool_format_t   string_dataformat;
    hsize_t     curr_pos = 0;        /* total data element position   */

    if ((gcpl_id = H5Gget_create_plist(gid)) < 0) {
        error_msg("error in getting group creation property list ID\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    /* query the group creation properties for attributes */
    if (H5Pget_attr_creation_order(gcpl_id, &attr_crt_order_flags) < 0) {
        error_msg("error in getting group creation properties\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    /* query the group creation properties */
    if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) {
        error_msg("error in getting group creation properties\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    if(H5Pclose(gcpl_id) < 0) {
        error_msg("error in closing group creation property list ID\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;
    
    string_dataformat = *outputformat;

    if (fp_format) {
        string_dataformat.fmt_double = fp_format;
        string_dataformat.fmt_float = fp_format;
    }

    if (h5tools_nCols==0) {
        string_dataformat.line_ncols = 65535;
        string_dataformat.line_per_line = 1;
    }
    else
        string_dataformat.line_ncols = h5tools_nCols;

    string_dataformat.do_escape = display_escape;
    outputformat = &string_dataformat;

    ctx.need_prefix = TRUE;

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s \"%s\" %s",
                        h5tools_dump_header_format->groupbegin, name,
                        h5tools_dump_header_format->groupblockbegin);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    
    ctx.indent_level++;
    dump_indent += COL;

    if(!HDstrcmp(name, "/") && unamedtype) {
        unsigned    u;  /* Local index variable */

        /* dump unamed type in root group */
        for(u = 0; u < type_table->nobjs; u++)
            if(!type_table->objs[u].recorded) {
                dset = H5Dopen2(gid, type_table->objs[u].objname, H5P_DEFAULT);
                type = H5Dget_type(dset);
                sprintf(type_name, "#"H5_PRINTF_HADDR_FMT, type_table->objs[u].objno);
                dump_function_table->dump_named_datatype_function(type, type_name);
                H5Tclose(type);
                H5Dclose(dset);
            }
    } /* end if */

    if(display_oid) {
        h5tools_dump_oid(rawoutstream, outputformat, &ctx, gid);
    }

    h5tools_dump_comment(rawoutstream, outputformat, &ctx, gid);

    H5Oget_info(gid, &oinfo);

    /* Must check for uniqueness of all objects if we've traversed an elink,
     * otherwise only check if the reference count > 1.
     */
    if(oinfo.rc > 1 || hit_elink) {
        obj_t  *found_obj;    /* Found object */

        found_obj = search_obj(group_table, oinfo.addr);

        if (found_obj == NULL) {
            error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
            h5tools_setstatus(EXIT_FAILURE);
        }
        else if (found_obj->displayed) {
            ctx.need_prefix = TRUE;

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "%s \"%s\"", HARDLINK, found_obj->objname);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        }
        else {
            found_obj->displayed = TRUE;
            attr_iteration(gid, attr_crt_order_flags);
            link_iteration(gid, crt_order_flags);
        }
    }
    else {
        attr_iteration(gid, attr_crt_order_flags);
        link_iteration(gid, crt_order_flags);
    }

    dump_indent -= COL;
    ctx.indent_level--;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    if(HDstrlen(h5tools_dump_header_format->groupblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->groupblockend);
        if(HDstrlen(h5tools_dump_header_format->groupend))
            h5tools_str_append(&buffer, " ");
    }
    if(HDstrlen(h5tools_dump_header_format->groupend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->groupend);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_dataset
 *
 * Purpose:     Dump the specified data set
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *  Pedro Vicente, 2004, added dataset creation property list display
 *  Pedro Vicente, October 4, 2007, added parameters to H5Aiterate2() to allow for
 *   other  iteration orders
 *
 *-------------------------------------------------------------------------
 */
void
dump_dataset(hid_t did, const char *name, struct subset_t *sset)
{
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;
    h5tool_format_t   string_dataformat;
    hid_t       type, space;
    unsigned    attr_crt_order_flags;
    hid_t       dcpl_id;  /* dataset creation property list ID */
    h5tools_str_t buffer;          /* string into which to render   */
    hsize_t       curr_pos = 0;        /* total data element position   */

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;
    
    string_dataformat = *outputformat;

    if (fp_format) {
        string_dataformat.fmt_double = fp_format;
        string_dataformat.fmt_float = fp_format;
    }

    if (h5tools_nCols==0) {
        string_dataformat.line_ncols = 65535;
        string_dataformat.line_per_line = 1;
    }
    else
        string_dataformat.line_ncols = h5tools_nCols;

    string_dataformat.do_escape = display_escape;
    outputformat = &string_dataformat;

    if ((dcpl_id = H5Dget_create_plist(did)) < 0) {
        error_msg("error in getting creation property list ID\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    /* query the creation properties for attributes */
    if (H5Pget_attr_creation_order(dcpl_id, &attr_crt_order_flags) < 0) {
        error_msg("error in getting creation properties\n");
        h5tools_setstatus(EXIT_FAILURE);
    }

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
     /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "%s \"%s\" %s",
                        h5tools_dump_header_format->datasetbegin, name,
                        h5tools_dump_header_format->datasetblockbegin);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    h5tools_dump_comment(rawoutstream, outputformat, &ctx, did);

    dump_indent += COL;
    ctx.indent_level++;
    
    type = H5Dget_type(did);
    h5dump_type_table = type_table;
    h5tools_dump_datatype(rawoutstream, outputformat, &ctx, type);
    h5dump_type_table = NULL;

    space = H5Dget_space(did);
    h5tools_dump_dataspace(rawoutstream, outputformat, &ctx, space);
    H5Sclose(space);

    if(display_oid) {
        h5tools_dump_oid(rawoutstream, outputformat, &ctx, did);
    }

    if(display_dcpl) {
        h5dump_type_table = type_table;
        h5tools_dump_dcpl(rawoutstream, outputformat, &ctx, dcpl_id, type, did);
        h5dump_type_table = NULL;
    }
    H5Pclose(dcpl_id);

    if(display_data) {
        int  data_loop = 1;
        int  i;
        if(display_packed_bits)
            data_loop = packed_bits_num;
        for(i=0; i<data_loop; i++) {
            if(display_packed_bits) {
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                /* Render the element */
                h5tools_str_reset(&buffer);
                packed_data_mask = packed_mask[i];
                packed_data_offset = packed_offset[i];
                packed_data_length = packed_length[i];
                h5tools_print_packed_bits(&buffer, type);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            }
            switch(H5Tget_class(type)) {
            case H5T_TIME:
                ctx.indent_level++;

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "DATA{ not yet implemented.}");
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                
                ctx.indent_level--;
                break;

            case H5T_INTEGER:
            case H5T_FLOAT:
            case H5T_STRING:
            case H5T_BITFIELD:
            case H5T_OPAQUE:
            case H5T_COMPOUND:
            case H5T_REFERENCE:
            case H5T_ENUM:
            case H5T_VLEN:
            case H5T_ARRAY:
                {
                    h5tools_dump_data(rawoutstream, outputformat, &ctx, did, TRUE, sset, display_ai, display_char);
                }
                break;

            default:
                break;
            } /* end switch */
        } /* for(i=0;i<data_loop;i++) */
    }
    H5Tclose(type);

    if (!bin_output) {
        attr_iteration(did, attr_crt_order_flags);
    }
    ctx.indent_level--;
    dump_indent -= COL;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    if(HDstrlen(h5tools_dump_header_format->datasetblockend)) {
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datasetblockend);
        if(HDstrlen(h5tools_dump_header_format->datasetend))
            h5tools_str_append(&buffer, " ");
    }
    if(HDstrlen(h5tools_dump_header_format->datasetend))
        h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->datasetend);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    dump_data
 *
 * Purpose:     Dump attribute or dataset data
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: pvn, print the matrix indices
 *  Albert Cheng, 2004/11/18
 *  Add --string printing for attributes too.
 *
 *-------------------------------------------------------------------------
 */
void
dump_data(hid_t obj_id, int obj_data, struct subset_t *sset, int display_index)
{
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;
    h5tool_format_t   string_dataformat;
    int               print_dataset = FALSE;

    string_dataformat = *outputformat;

    if (fp_format) {
        string_dataformat.fmt_double = fp_format;
        string_dataformat.fmt_float = fp_format;
    }

    if (h5tools_nCols==0) {
        string_dataformat.line_ncols = 65535;
        string_dataformat.line_per_line = 1;
    }
    else
        string_dataformat.line_ncols = h5tools_nCols;

    string_dataformat.do_escape = display_escape;
    outputformat = &string_dataformat;

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;

    if(obj_data == DATASET_DATA)
        print_dataset = TRUE;
    h5tools_dump_data(rawoutstream, outputformat, &ctx, obj_id, print_dataset, sset, display_index, display_char);
}


/*-------------------------------------------------------------------------
 * Function:    dump_fcpl
 *
 * Purpose:     prints file creation property list information
 *
 * Return:      void
 *
 * Programmer:  pvn
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
dump_fcpl(hid_t fid)
{
    hid_t    fcpl;      /* file creation property list ID */
    hsize_t  userblock; /* userblock size retrieved from FCPL */
    size_t   off_size;  /* size of offsets in the file */
    size_t   len_size;  /* size of lengths in the file */
    unsigned super;     /* superblock version # */
    unsigned freelist;  /* free list version # */
    unsigned stab;      /* symbol table entry version # */
    unsigned shhdr;     /* shared object header version # */
#ifdef SHOW_FILE_DRIVER
    hid_t    fapl;      /* file access property list ID */
    hid_t    fdriver;   /* file driver */
    char     dname[32]; /* buffer to store driver name */
#endif
    unsigned sym_lk;    /* symbol table B-tree leaf 'K' value */
    unsigned sym_ik;    /* symbol table B-tree internal 'K' value */
    unsigned istore_ik; /* indexed storage B-tree internal 'K' value */

    fcpl=H5Fget_create_plist(fid);
    H5Pget_version(fcpl, &super, &freelist, &stab, &shhdr);
    H5Pget_userblock(fcpl,&userblock);
    H5Pget_sizes(fcpl,&off_size,&len_size);
    H5Pget_sym_k(fcpl,&sym_ik,&sym_lk);
    H5Pget_istore_k(fcpl,&istore_ik);
    H5Pclose(fcpl);
#ifdef SHOW_FILE_DRIVER
    fapl=h5_fileaccess();
    fdriver=H5Pget_driver(fapl);
    H5Pclose(fapl);
#endif
    
   /*-------------------------------------------------------------------------
    * SUPER_BLOCK
    *-------------------------------------------------------------------------
    */
    PRINTSTREAM(rawoutstream, "\n%s %s\n",SUPER_BLOCK, BEGIN);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s %u\n","SUPERBLOCK_VERSION", super);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s %u\n","FREELIST_VERSION", freelist);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s %u\n","SYMBOLTABLE_VERSION", stab);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s %u\n","OBJECTHEADER_VERSION", shhdr);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream,"%s %zu\n","OFFSET_SIZE", off_size);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream,"%s %zu\n","LENGTH_SIZE", len_size);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s %u\n","BTREE_RANK", sym_ik);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s %d\n","BTREE_LEAF", sym_lk);

#ifdef SHOW_FILE_DRIVER
    if(H5FD_CORE==fdriver)
        HDstrcpy(dname,"H5FD_CORE");
#ifdef H5_HAVE_DIRECT
    else if(H5FD_DIRECT==fdriver)
        HDstrcpy(dname,"H5FD_DIRECT");
#endif
    else if(H5FD_FAMILY==fdriver)
        HDstrcpy(dname,"H5FD_FAMILY");
    else if(H5FD_LOG==fdriver)
        HDstrcpy(dname,"H5FD_LOG");
    else if(H5FD_MPIO==fdriver)
        HDstrcpy(dname,"H5FD_MPIO");
    else if(H5FD_MULTI==fdriver)
        HDstrcpy(dname,"H5FD_MULTI");
    else if(H5FD_SEC2==fdriver)
        HDstrcpy(dname,"H5FD_SEC2");
    else if(H5FD_STDIO==fdriver)
        HDstrcpy(dname,"H5FD_STDIO");
    else
        HDstrcpy(dname,"Unknown driver");

    /* Take out this because the driver used can be different from the
     * standard output. */
    /*indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s %s\n","FILE_DRIVER", dname);*/
#endif
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s %u\n","ISTORE_K", istore_ik);

    /*-------------------------------------------------------------------------
    * USER_BLOCK
    *-------------------------------------------------------------------------
    */
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "USER_BLOCK %s\n",BEGIN);
    indentation(dump_indent + COL + COL);
    PRINTSTREAM(rawoutstream,"%s %Hu\n","USERBLOCK_SIZE", userblock);
    indentation(dump_indent + COL);
    PRINTSTREAM(rawoutstream, "%s\n",END);

    PRINTSTREAM(rawoutstream, "%s",END);
}

/*-------------------------------------------------------------------------
 * Function:    dump_fcontents
 *
 * Purpose:     prints all objects
 *
 * Return:      void
 *
 * Programmer:  pvn
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
dump_fcontents(hid_t fid)
{
    PRINTSTREAM(rawoutstream, "%s %s\n",FILE_CONTENTS, BEGIN);

    /* special case of unamed types in root group */
    if (unamedtype) {
        unsigned u;

        for (u = 0; u < type_table->nobjs; u++) {
            if (!type_table->objs[u].recorded)
                PRINTSTREAM(rawoutstream, " %-10s /#"H5_PRINTF_HADDR_FMT"\n", "datatype", type_table->objs[u].objno);
        }
    }

    /* print objects in the files */
    h5trav_print(fid);

    PRINTSTREAM(rawoutstream, " %s\n",END);
}

static herr_t
attr_search(hid_t oid, const char *attr_name, const H5A_info_t UNUSED *ainfo, void *_op_data)
{
    herr_t              ret = SUCCEED;
    int 				i;
    int 				j;
    int 				k;
    char			   *obj_op_name;
    char               *obj_name;
	trav_attr_udata_t  *attr_data = (trav_attr_udata_t*)_op_data;
    char               *buf = attr_data->path;
    char			   *op_name = attr_data->op_name;

    j = (int)HDstrlen(op_name) - 1;
    /* find the last / */
    while(j >= 0) {
    	if (op_name[j] == '/' && (j==0 || (j>0 && op_name[j-1]!='\\')))
    		break;
    	j--;
    }

    obj_op_name = h5tools_str_replace(op_name + j + 1, "\\/", "/");

    if(obj_op_name == NULL) {
    	h5tools_setstatus(EXIT_FAILURE);
    	ret = FAIL;
    }
    else {
    	if(HDstrcmp(attr_name, obj_op_name)==0) {
    	    /* object name */
    	    i = (int)HDstrlen(buf);
    	    j = (int)HDstrlen(op_name);
    	    k = (size_t)i + 1 + (size_t)j + 1 + 2;
    	    obj_name = (char *)HDmalloc((size_t)k);
    	    if(obj_name == NULL) {
    	    	h5tools_setstatus(EXIT_FAILURE);
    	    	ret = FAIL;
    	    }
    	    else {
    	    	HDmemset(obj_name, '\0', (size_t)k);
    	        if(op_name[0] != '/') {
    	        	HDstrncat(obj_name, buf, (size_t)i + 1);
    	        	if(buf[i-1] != '/')
    	            	HDstrncat(obj_name, "/", (size_t)2);
    	        }
    	        HDstrncat(obj_name, op_name, (size_t)j + 1);

    	    	handle_attributes(oid, obj_name, NULL, 0, NULL);
    	    	HDfree(obj_name);
        	}
    	}
    	HDfree(obj_op_name);
    }
    return ret;
} /* end attr_search() */

static herr_t
obj_search(const char *path, const H5O_info_t *oi, const char UNUSED *already_visited, void *_op_data)
{
	trav_handle_udata_t  *handle_data = (trav_handle_udata_t*)_op_data;
    char *op_name = (char*)handle_data->op_name;

    trav_attr_udata_t  attr_data;
    attr_data.path = (char*)path;
    attr_data.op_name = op_name;
    H5Aiterate_by_name(handle_data->fid, path, H5_INDEX_NAME, H5_ITER_INC, NULL, attr_search, (void*)&attr_data, H5P_DEFAULT);

    if(HDstrcmp(path, op_name)==0) {
    	switch(oi->type) {
    	case H5O_TYPE_GROUP:
    		handle_groups(handle_data->fid, path, NULL, 0, NULL);
    		break;
    	case H5O_TYPE_DATASET:
    		handle_datasets(handle_data->fid, path, NULL, 0, NULL);
    		break;
    	case H5O_TYPE_NAMED_DATATYPE:
    		handle_datatypes(handle_data->fid, path, NULL, 0, NULL);
    		break;
    	case H5O_TYPE_UNKNOWN:
    	case H5O_TYPE_NTYPES:
    	default:
    		error_msg("unknown object type value\n");
    		h5tools_setstatus(EXIT_FAILURE);
    	} /* end switch */
    }

    return 0;
} /* end obj_search() */

static herr_t
lnk_search(const char *path, const H5L_info_t *li, void *_op_data)
{
    int         search_len;
    int			k;
    char       *search_name;
	trav_handle_udata_t  *handle_data = (trav_handle_udata_t*)_op_data;
    char *op_name = (char*)handle_data->op_name;

    search_len = HDstrlen(op_name);
    if(search_len > 0 && op_name[0] != '/') {
    	k = 2;
    }
    else
        k = 1;
   	search_name = (char *)HDmalloc((size_t)(search_len + k));
    if(search_name == NULL) {
		error_msg("creating temporary link\n");
    	h5tools_setstatus(EXIT_FAILURE);
    }
    else {
		if (k == 2) {
			HDstrcpy(search_name, "/");
			HDstrncat(search_name, op_name, (size_t)search_len + 1);
		}
		else
			HDstrncpy(search_name, op_name, (size_t)search_len + 1);
		search_name[search_len + k - 1] = '\0';

		if(HDstrcmp(path, search_name) == 0) {
			switch(li->type) {
			case H5L_TYPE_SOFT:
			case H5L_TYPE_EXTERNAL:
				handle_links(handle_data->fid, op_name, NULL, 0, NULL);
				break;

			case H5L_TYPE_HARD:
			case H5L_TYPE_MAX:
			case H5L_TYPE_ERROR:
			default:
				error_msg("unknown link type value\n");
				h5tools_setstatus(EXIT_FAILURE);
				break;
			} /* end switch() */
		}
		HDfree(search_name);
    }
    return 0;
} /* end lnk_search() */

/*-------------------------------------------------------------------------
 * Function:    handle_paths
 *
 * Purpose:     Handle objects from the command.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
handle_paths(hid_t fid, const char *path_name, void UNUSED * data, int UNUSED pe, const char UNUSED *display_name)
{
    hid_t  gid = -1;

    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) {
        error_msg("unable to open root group\n");
        h5tools_setstatus(EXIT_FAILURE);
    }
    else {
        hid_t       gcpl_id;
        unsigned    crt_order_flags;
        unsigned    attr_crt_order_flags;
		trav_handle_udata_t handle_udata;     /* User data for traversal */

        if ((gcpl_id = H5Gget_create_plist(gid)) < 0) {
            error_msg("error in getting group creation property list ID\n");
            h5tools_setstatus(EXIT_FAILURE);
        }

        /* query the group creation properties for attributes */
        if (H5Pget_attr_creation_order(gcpl_id, &attr_crt_order_flags) < 0) {
            error_msg("error in getting group creation properties\n");
            h5tools_setstatus(EXIT_FAILURE);
        }

        /* query the group creation properties */
        if(H5Pget_link_creation_order(gcpl_id, &crt_order_flags) < 0) {
            error_msg("error in getting group creation properties\n");
            h5tools_setstatus(EXIT_FAILURE);
        }

        if(H5Pclose(gcpl_id) < 0) {
            error_msg("error in closing group creation property list ID\n");
            h5tools_setstatus(EXIT_FAILURE);
        }

		handle_udata.fid = fid;
		handle_udata.op_name = (char*)path_name;
		if(h5trav_visit(fid, "/", TRUE, TRUE, obj_search, lnk_search, &handle_udata) < 0) {
			error_msg("error traversing information\n");
			h5tools_setstatus(EXIT_FAILURE);
		}
    }
}

/*-------------------------------------------------------------------------
 * Function:    handle_attributes
 *
 * Purpose:     Handle the attributes from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *
 * PVN, May 2008
 *   add an extra parameter PE, to allow printing/not printing of error messages
 *
 *-------------------------------------------------------------------------
 */
void
handle_attributes(hid_t fid, const char *attr, void UNUSED * data, int UNUSED pe, const char UNUSED *display_name)
{
    hid_t  oid = -1;
    hid_t  attr_id = -1;
    char *obj_name;
    char *attr_name;
    int j;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &h5tools_dataformat;
    h5tool_format_t   string_dataformat;
    hsize_t     curr_pos = 0;        /* total data element position   */

    j = (int)HDstrlen(attr) - 1;
    obj_name = (char *)HDmalloc((size_t)j + 2);
    if(obj_name == NULL)
        goto error;

    /* find the last / */
    while(j >= 0) {
        if (attr[j] == '/' && (j==0 || (j>0 && attr[j-1]!='\\'))) 
            break;
        j--;
    }

    /* object name */
    if(j == -1)
        HDstrcpy(obj_name, "/");
    else {
        HDstrncpy(obj_name, attr, (size_t)j + 1);
        obj_name[j + 1] = '\0';
    } /* end else */

    dump_indent += COL;
    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;

    string_dataformat = *outputformat;

    if (fp_format) {
        string_dataformat.fmt_double = fp_format;
        string_dataformat.fmt_float = fp_format;
    }

    if (h5tools_nCols==0) {
        string_dataformat.line_ncols = 65535;
        string_dataformat.line_per_line = 1;
    }
    else
        string_dataformat.line_ncols = h5tools_nCols;

    string_dataformat.do_escape = display_escape;
    outputformat = &string_dataformat;

	attr_name = h5tools_str_replace(attr + j + 1, "\\/", "/");

    /* handle error case: cannot open the object with the attribute */
    if((oid = H5Oopen(fid, obj_name, H5P_DEFAULT)) < 0) {
        /* setup */
        HDmemset(&buffer, 0, sizeof(h5tools_str_t));

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "%s \"%s\" %s",
                h5tools_dump_header_format->attributebegin, attr,
                h5tools_dump_header_format->attributeblockbegin);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        error_msg("unable to open object \"%s\"\n", obj_name);

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
        /* Render the element */
        h5tools_str_reset(&buffer);
        if(HDstrlen(h5tools_dump_header_format->attributeblockend)) {
            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->attributeblockend);
            if(HDstrlen(h5tools_dump_header_format->attributeend))
                h5tools_str_append(&buffer, " ");
        }
        if(HDstrlen(h5tools_dump_header_format->attributeend))
            h5tools_str_append(&buffer, "%s", h5tools_dump_header_format->attributeend);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        h5tools_str_close(&buffer);

        goto error;
    } /* end if */

    attr_id = H5Aopen(oid, attr_name, H5P_DEFAULT);
    oid_output = display_oid;
    data_output = display_data;
    attr_data_output = display_attr_data;

    h5dump_type_table = type_table;
    h5tools_dump_attribute(rawoutstream, outputformat, &ctx, attr_name, attr_id, display_ai, display_char);
    h5dump_type_table = NULL;

    if(attr_id < 0) {
        goto error;
    }

    /* Close object */
    if(H5Oclose(oid) < 0) {
        goto error;
    } /* end if */

    HDfree(obj_name);
	HDfree(attr_name);
    dump_indent -= COL;
    return;

error:
    h5tools_setstatus(EXIT_FAILURE);
    if(obj_name)
        HDfree(obj_name);
		
	if (attr_name)
		HDfree(attr_name);

    H5E_BEGIN_TRY {
        H5Oclose(oid);
        H5Aclose(attr_id);
    } H5E_END_TRY;
    dump_indent -= COL;
}

/*-------------------------------------------------------------------------
 * Function:    handle_datasets
 *
 * Purpose:     Handle the datasets from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *  Pedro Vicente, Tuesday, January 15, 2008
 *  check for block overlap\
 *
 *  Pedro Vicente, May 8, 2008
 *   added a flag PE that prints/not prints error messages
 *   added for cases of external links not found, to avoid printing of
 *    objects not found, since external links are dumped on a trial error basis
 *
 *-------------------------------------------------------------------------
 */
void
handle_datasets(hid_t fid, const char *dset, void *data, int pe, const char *display_name)
{
    H5O_info_t       oinfo;
    hid_t            dsetid;
    struct subset_t *sset = (struct subset_t *)data;
    const char      *real_name = display_name ? display_name : dset;

    if((dsetid = H5Dopen2(fid, dset, H5P_DEFAULT)) < 0) {
        if (pe) {
            handle_links(fid, dset, data, pe, display_name);
        }
        return;
    } /* end if */

    if(sset) {
        unsigned int i;
        unsigned int ndims;
        hid_t sid = H5Dget_space(dsetid);
        int ndims_res = H5Sget_simple_extent_ndims(sid);

        H5Sclose(sid);
        if(ndims_res < 0) {
            error_msg("H5Sget_simple_extent_ndims failed\n");
            h5tools_setstatus(EXIT_FAILURE);
            return;
        }
        else
            ndims = ndims_res;

        if(!sset->start.data || !sset->stride.data || !sset->count.data || !sset->block.data) {
            /* they didn't specify a ``stride'' or ``block''. default to 1 in all
             * dimensions */
            if(!sset->start.data) {
                /* default to (0, 0, ...) for the start coord */
                sset->start.data = (hsize_t *)HDcalloc((size_t)ndims, sizeof(hsize_t));
                sset->start.len = ndims;
            }

            if(!sset->stride.data) {
                sset->stride.data = (hsize_t *)HDcalloc((size_t)ndims, sizeof(hsize_t));
                sset->stride.len = ndims;
                for (i = 0; i < ndims; i++)
                    sset->stride.data[i] = 1;
            }

            if(!sset->count.data) {
                sset->count.data = (hsize_t *)HDcalloc((size_t)ndims, sizeof(hsize_t));
                sset->count.len = ndims;
                for (i = 0; i < ndims; i++)
                    sset->count.data[i] = 1;
            }

            if(!sset->block.data) {
                sset->block.data = (hsize_t *)HDcalloc((size_t)ndims, sizeof(hsize_t));
                sset->block.len = ndims;
                for (i = 0; i < ndims; i++)
                    sset->block.data[i] = 1;
            }
        }

        /*-------------------------------------------------------------------------
         * check for dimension overflow
         *-------------------------------------------------------------------------
         */
        if(sset->start.len > ndims) {
            error_msg("number of start dims (%u) exceed dataset dims (%u)\n", sset->start.len, ndims);
            h5tools_setstatus(EXIT_FAILURE);
            return;
        }
        if(sset->stride.len > ndims) {
            error_msg("number of stride dims (%u) exceed dataset dims (%u)\n", sset->stride.len, ndims);
            h5tools_setstatus(EXIT_FAILURE);
            return;
        }
        if(sset->count.len > ndims) {
            error_msg("number of count dims (%u) exceed dataset dims (%u)\n", sset->count.len, ndims);
            h5tools_setstatus(EXIT_FAILURE);
            return;
        }
        if(sset->block.len > ndims) {
            error_msg("number of block dims (%u) exceed dataset dims (%u)\n", sset->block.len, ndims);
            h5tools_setstatus(EXIT_FAILURE);
            return;
        }
        
        /*-------------------------------------------------------------------------
         * check for block overlap
         *-------------------------------------------------------------------------
         */
        for(i = 0; i < ndims; i++) {
            if(sset->count.data[i] > 1) {
                if(sset->stride.data[i] < sset->block.data[i]) {
                    error_msg("wrong subset selection; blocks overlap\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    return;
                } /* end if */
            } /* end if */
        } /* end for */
    } /* end if */


    H5Oget_info(dsetid, &oinfo);
    if(oinfo.rc > 1 || hit_elink) {
        obj_t  *found_obj;    /* Found object */

        found_obj = search_obj(dset_table, oinfo.addr);

        if(found_obj) {
            if (found_obj->displayed) {
                PRINTVALSTREAM(rawoutstream, "\n");
                indentation(dump_indent);
                begin_obj(h5tools_dump_header_format->datasetbegin, real_name, h5tools_dump_header_format->datasetblockbegin);
                PRINTVALSTREAM(rawoutstream, "\n");
                indentation(dump_indent + COL);
                PRINTSTREAM(rawoutstream, "%s \"%s\"\n", HARDLINK, found_obj->objname);
                indentation(dump_indent);
                end_obj(h5tools_dump_header_format->datasetend, h5tools_dump_header_format->datasetblockend);
            } 
            else {
                found_obj->displayed = TRUE;
                dump_indent += COL;
                dump_dataset(dsetid, real_name, sset);
                dump_indent -= COL;
            }
        }
        else
            h5tools_setstatus(EXIT_FAILURE);
    }
    else {
        dump_indent += COL;
        dump_dataset(dsetid, real_name, sset);
        dump_indent -= COL;
    }

    if(H5Dclose(dsetid) < 0)
        h5tools_setstatus(EXIT_FAILURE);
}

/*-------------------------------------------------------------------------
 * Function:    handle_groups
 *
 * Purpose:     Handle the groups from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications: Pedro Vicente, September 26, 2007
 *  handle creation order
 *
 * Pedro Vicente, May 8, 2008
 *   added a flag PE that prints/not prints error messages
 *   added for cases of external links not found, to avoid printing of
 *    objects not found, since external links are dumped on a trial error basis
 *
 *-------------------------------------------------------------------------
 */
void
handle_groups(hid_t fid, const char *group, void UNUSED *data, int pe, const char *display_name)
{
    hid_t       gid;
    const char  *real_name = display_name ? display_name : group;

    if((gid = H5Gopen2(fid, group, H5P_DEFAULT)) < 0) {
        if (pe) {
            PRINTVALSTREAM(rawoutstream, "\n");
            begin_obj(h5tools_dump_header_format->groupbegin, real_name, h5tools_dump_header_format->groupblockbegin);
            PRINTVALSTREAM(rawoutstream, "\n");
            indentation(COL);
            error_msg("unable to open group \"%s\"\n", real_name);
            end_obj(h5tools_dump_header_format->groupend, h5tools_dump_header_format->groupblockend);
            h5tools_setstatus(EXIT_FAILURE);
        }
    }
    else {
        size_t new_len = HDstrlen(group) + 1;

        if(prefix_len <= new_len) {
            prefix_len = new_len;
            prefix = (char *)HDrealloc(prefix, prefix_len);
        } /* end if */

        HDstrcpy(prefix, group);

        dump_indent += COL;
        dump_group(gid, real_name);
        dump_indent -= COL;

        if(H5Gclose(gid) < 0)
            h5tools_setstatus(EXIT_FAILURE);
    } /* end else */
} /* end handle_groups() */

/*-------------------------------------------------------------------------
 * Function:    handle_links
 *
 * Purpose:     Handle soft or UD links from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
handle_links(hid_t fid, const char *links, void UNUSED * data, int UNUSED pe, const char UNUSED *display_name)
{
    H5L_info_t linfo;

    if(H5Lget_info(fid, links, &linfo, H5P_DEFAULT) < 0) {
        error_msg("unable to get link info from \"%s\"\n", links);
        h5tools_setstatus(EXIT_FAILURE);
    } 
    else if(linfo.type == H5L_TYPE_HARD) {
        error_msg("\"%s\" is a hard link\n", links);
        h5tools_setstatus(EXIT_FAILURE);
    } 
    else {
        char *buf = (char *)HDmalloc(linfo.u.val_size);
        PRINTVALSTREAM(rawoutstream, "\n");

        switch(linfo.type) {
        case H5L_TYPE_SOFT:    /* Soft link */
            begin_obj(h5tools_dump_header_format->softlinkbegin, links, h5tools_dump_header_format->softlinkblockbegin);
            PRINTVALSTREAM(rawoutstream, "\n");
            indentation(COL);
            if(H5Lget_val(fid, links, buf, linfo.u.val_size, H5P_DEFAULT) >= 0)
                PRINTSTREAM(rawoutstream, "LINKTARGET \"%s\"\n", buf);
            else {
                error_msg("h5dump error: unable to get link value for \"%s\"\n", links);
                h5tools_setstatus(EXIT_FAILURE);
            }
            end_obj(h5tools_dump_header_format->softlinkend, h5tools_dump_header_format->softlinkblockend);
            break;

        case H5L_TYPE_EXTERNAL:
            begin_obj(h5tools_dump_header_format->udlinkbegin, links, h5tools_dump_header_format->udlinkblockbegin);
            PRINTVALSTREAM(rawoutstream, "\n");
            indentation(COL);
            begin_obj(h5tools_dump_header_format->extlinkbegin, links, h5tools_dump_header_format->extlinkblockbegin);
            PRINTVALSTREAM(rawoutstream, "\n");
            if(H5Lget_val(fid, links, buf, linfo.u.val_size, H5P_DEFAULT) >= 0) {
                const char *elink_file;
                const char *elink_path;

                if(H5Lunpack_elink_val(buf, linfo.u.val_size, NULL, &elink_file, &elink_path)>=0) {
                    indentation(COL);
                    PRINTSTREAM(rawoutstream, "LINKCLASS %d\n", linfo.type);
                    indentation(COL);
                    PRINTSTREAM(rawoutstream, "TARGETFILE \"%s\"\n", elink_file);
                    indentation(COL);
                    PRINTSTREAM(rawoutstream, "TARGETPATH \"%s\"\n", elink_path);
                } 
                else {
                    error_msg("h5dump error: unable to unpack external link value for \"%s\"\n", links);
                    h5tools_setstatus(EXIT_FAILURE);
                }
            } 
            else {
                error_msg("h5dump error: unable to get external link value for \"%s\"\n", links);
                h5tools_setstatus(EXIT_FAILURE);
            }
            end_obj(h5tools_dump_header_format->extlinkend, h5tools_dump_header_format->extlinkblockend);
            break;

        default:
            begin_obj(h5tools_dump_header_format->udlinkbegin, links, h5tools_dump_header_format->udlinkblockbegin);
            PRINTVALSTREAM(rawoutstream, "\n");
            indentation(COL);
            begin_obj(h5tools_dump_header_format->udlinkbegin, links, h5tools_dump_header_format->udlinkblockbegin);
            PRINTVALSTREAM(rawoutstream, "\n");
            indentation(COL);
            PRINTSTREAM(rawoutstream, "LINKCLASS %d\n", linfo.type);
            end_obj(h5tools_dump_header_format->udlinkend, h5tools_dump_header_format->udlinkblockend);
            break;
        } /* end switch */
        HDfree(buf);
    } /* end else */
}

/*-------------------------------------------------------------------------
 * Function:    handle_datatypes
 *
 * Purpose:     Handle the datatypes from the command.
 *
 * Return:      void
 *
 * Programmer:  Bill Wendling
 *              Tuesday, 9. January 2001
 *
 * Modifications:
 *
 *  Pedro Vicente, May 8, 2008
 *   added a flag PE that prints/not prints error messages
 *   added for cases of external links not found, to avoid printing of
 *    objects not found, since external links are dumped on a trial error basis
 *
 *-------------------------------------------------------------------------
 */
void
handle_datatypes(hid_t fid, const char *type, void UNUSED * data, int pe, const char *display_name)
{
    hid_t       type_id;
    const char  *real_name = display_name ? display_name : type;

    if((type_id = H5Topen2(fid, type, H5P_DEFAULT)) < 0) {
        /* check if type is unamed datatype */
        unsigned idx = 0;

        while(idx < type_table->nobjs ) {
            char name[128];

            if(!type_table->objs[idx].recorded) {
                /* unamed datatype */
                sprintf(name, "/#"H5_PRINTF_HADDR_FMT, type_table->objs[idx].objno);

                if(!HDstrcmp(name, real_name))
                    break;
            } /* end if */

            idx++;
        } /* end while */

        if(idx == type_table->nobjs) {
            if (pe) {
                /* unknown type */
                PRINTVALSTREAM(rawoutstream, "\n");
                begin_obj(h5tools_dump_header_format->datatypebegin, real_name, h5tools_dump_header_format->datatypeblockbegin);
                PRINTVALSTREAM(rawoutstream, "\n");
                indentation(COL);
                error_msg("unable to open datatype \"%s\"\n", real_name);
                end_obj(h5tools_dump_header_format->datatypeend, h5tools_dump_header_format->datatypeblockend);
                h5tools_setstatus(EXIT_FAILURE);
            }
        }
        else {
            hid_t dsetid = H5Dopen2(fid, type_table->objs[idx].objname, H5P_DEFAULT);
            type_id = H5Dget_type(dsetid);

            dump_indent += COL;
            dump_named_datatype(type_id, real_name);
            dump_indent -= COL;

            H5Tclose(type_id);
            H5Dclose(dsetid);
        }
    }
    else {
        dump_indent += COL;
        dump_named_datatype(type_id, real_name);
        dump_indent -= COL;

        if(H5Tclose(type_id) < 0)
            h5tools_setstatus(EXIT_FAILURE);
    }
}


/*-------------------------------------------------------------------------
 * Function:    dump_extlink
 *
 * made by: PVN
 *
 * Purpose:     Dump an external link
 *  Since external links are soft links, they are dumped on a trial error
 *   basis, attempting to dump as a dataset, as a group and as a named datatype
 *   Error messages are supressed
 *
 * Modifications:
 *      Neil Fortner
 *      13 October 2008
 *      Function basically rewritten.  No longer directly opens the target file,
 *      now initializes a new set of tables for the external file.  No longer
 *      dumps on a trial and error basis, but errors are still suppressed.
 *
 *-------------------------------------------------------------------------
 */
static int
dump_extlink(hid_t group, const char *linkname, const char *objname)
{
    hid_t       oid;
    H5O_info_t  oi;
    table_t     *old_group_table = group_table;
    table_t     *old_dset_table = dset_table;
    table_t     *old_type_table = type_table;
    hbool_t     old_hit_elink;
    ssize_t     idx;

    /* Open target object */
    if ((oid = H5Oopen(group, linkname, H5P_DEFAULT)) < 0)
        goto fail;

    /* Get object info */
    if (H5Oget_info(oid, &oi) < 0) {
        H5Oclose(oid);
        goto fail;
    }

    /* Check if we have visited this file already */
    if ((idx = table_list_visited(oi.fileno)) < 0) {
        /* We have not visited this file, build object tables */
        if ((idx = table_list_add(oid, oi.fileno)) < 0) {
            H5Oclose(oid);
            goto fail;
        }
    }

    /* Do not recurse through an external link into the original file (idx=0) */
    if (idx) {
        /* Update table pointers */
        group_table = table_list.tables[idx].group_table;
        dset_table = table_list.tables[idx].dset_table;
        type_table = table_list.tables[idx].type_table;

        /* We will now traverse the external link, set this global to indicate this */
        old_hit_elink = hit_elink;
        hit_elink = TRUE;

        /* add some indentation to distinguish that these objects are external */
        dump_indent += COL;

        /* Recurse into the external file */
        switch (oi.type) {
            case H5O_TYPE_GROUP:
                handle_groups(group, linkname, NULL, 0, objname);
                break;
            case H5O_TYPE_DATASET:
                handle_datasets(group, linkname, NULL, 0, objname);
                break;
            case H5O_TYPE_NAMED_DATATYPE:
                handle_datatypes(group, linkname, NULL, 0, objname);
                break;
            default:
                h5tools_setstatus(EXIT_FAILURE);
        }

        dump_indent -= COL;

        /* Reset table pointers */
        group_table = old_group_table;
        dset_table = old_dset_table;
        type_table = old_type_table;

        /* Reset hit_elink */
        hit_elink = old_hit_elink;
    } /* end if */

    if (H5Idec_ref(oid) < 0)
        h5tools_setstatus(EXIT_FAILURE);

    return SUCCEED;

fail:
    return FAIL;
}

