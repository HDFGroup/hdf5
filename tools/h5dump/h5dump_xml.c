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
#include "h5dump_xml.h"

const char       *xmlnsprefix="hdf5:";

/*
 *  Alternative formating for data dumped to XML
 *  In general, the numbers are the same, but separators
 *  except spaces are not used.
 *
 *  Some of these are not used, as some kinds of data are
 *  dumped in completely new subroutines.
 *
 *  Some of this formatting may yet need to change.
 *
 *  This table only affects XML output.
 */
static h5tool_format_t         xml_dataformat = {
    0,              /*raw */

    "",             /*fmt_raw */
    "%d",           /*fmt_int */
    "%u",           /*fmt_uint */
    "%hhd",           /*fmt_schar */
    "%u",           /*fmt_uchar */
    "%d",           /*fmt_short */
    "%u",           /*fmt_ushort */
    "%ld",          /*fmt_long */
    "%lu",          /*fmt_ulong */
    NULL,           /*fmt_llong */
    NULL,           /*fmt_ullong */
    "%g",           /*fmt_double */
    "%g",           /*fmt_float */

    0,              /*ascii */
    0,              /*str_locale */
    0,              /*str_repeat */

    "",            /*arr_pre */
    "",             /*arr_sep */
    "",             /*arr_suf */
    1,              /*arr_linebreak */

    "",             /*cmpd_name */
    "",             /*cmpd_sep */
    "",             /*cmpd_pre */
    "",             /*cmpd_suf */
    "",             /*cmpd_end */

    " ",            /*vlen_sep */
    " ",            /*vlen_pre */
    "",             /*vlen_suf */
    "",             /*vlen_end */

    "%s",           /*elmt_fmt */
    "",             /*elmt_suf1 */
    " ",            /*elmt_suf2 */

    "",             /*idx_n_fmt */
    "",             /*idx_sep */
    "",             /*idx_fmt */

    80,             /*line_ncols *//*standard default columns */
    0,              /*line_per_line */
    "",             /*line_pre */
    "%s",           /*line_1st */
    "%s",           /*line_cont */
    "",             /*line_suf */
    "",             /*line_sep */
    1,              /*line_multi_new */
    "   ",          /*line_indent */

    1,              /*skip_first */

    1,              /*obj_hidefileno */
    " "H5_PRINTF_HADDR_FMT, /*obj_format */

    1,              /*dset_hidefileno */
    "DATASET %s ",  /*dset_format */
    "%s",           /*dset_blockformat_pre */
    "%s",           /*dset_ptformat_pre */
    "%s",           /*dset_ptformat */
     0,             /*array indices */
     0              /*escape non printable characters */
};


/* internal functions */
static int      xml_name_to_XID(const char *, char *, int , int );

/* internal functions used by XML option */
static void             xml_print_datatype(hid_t, unsigned);
static void             xml_print_enum(hid_t);
static int              xml_print_refs(hid_t, int);
static int              xml_print_strs(hid_t, int);
static char            *xml_escape_the_string(const char *, int);
static char            *xml_escape_the_name(const char *);

/*-------------------------------------------------------------------------
 * Function:    xml_dump_all_cb
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
xml_dump_all_cb(hid_t group, const char *name, const H5L_info_t *linfo, void H5_ATTR_UNUSED *op_data)
{
    hid_t       obj;
    herr_t      ret = SUCCEED;
    char       *obj_path = NULL;    /* Full path of object */
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
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
                        /* the XML version */
                        char *t_obj_path = xml_escape_the_name(obj_path);
                        char *t_prefix = xml_escape_the_name(HDstrcmp(prefix,"") ? prefix : "/");
                        char *t_name = xml_escape_the_name(name);
                        char *t_objname = xml_escape_the_name(found_obj->objname);
                        char dsetxid[100];
                        char parentxid[100];
                        char pointerxid[100];

                        /* Create OBJ-XIDs for the parent and object */
                        xml_name_to_XID(obj_path, dsetxid, (int)sizeof(dsetxid), 1);
                        xml_name_to_XID(prefix, parentxid, (int)sizeof(parentxid), 1);

                        ctx.need_prefix = TRUE;
                        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                        /* Render the element */
                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "<%sDataset Name=\"%s\" OBJ-XID=\"%s-%d\" "
                                "H5Path=\"%s\" Parents=\"%s\" "
                                "H5ParentPaths=\"%s\">",
                                xmlnsprefix,
                                t_name,                     /* Dataset Name */
                                dsetxid, get_next_xid(),    /* OBJ-XID */
                                t_obj_path,                 /* H5Path */
                                parentxid,                  /* Parents */
                                t_prefix);                  /* H5ParentPaths */
                        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                        xml_name_to_XID(found_obj->objname, pointerxid, (int)sizeof(pointerxid), 1);

                        ctx.indent_level++;

                        ctx.need_prefix = TRUE;
                        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                        /* Render the element */
                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "<%sDatasetPtr OBJ-XID=\"%s\" H5Path=\"%s\"/>",
                                xmlnsprefix,
                                pointerxid,t_objname);
                        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                        ctx.indent_level--;

                        ctx.need_prefix = TRUE;
                        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                        /* Render the element */
                        h5tools_str_reset(&buffer);
                        h5tools_str_append(&buffer, "</%sDataset>", xmlnsprefix);
                        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                        HDfree(t_name);
                        HDfree(t_obj_path);
                        HDfree(t_prefix);
                        HDfree(t_objname);

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

        case H5O_TYPE_UNKNOWN:
        case H5O_TYPE_NTYPES:
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

            if(H5Lget_val(group, name, targbuf, linfo->u.val_size, H5P_DEFAULT) < 0) {
                error_msg("unable to get link value\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret = FAIL;
            } 
            else {
                /* print the value of a soft link */
                /* XML */
                char linkxid[100];
                char parentxid[100];
                char targetxid[100];
                char *t_prefix = xml_escape_the_name(HDstrcmp(prefix,"") ? prefix : "/");
                char *t_name = xml_escape_the_name(name);
                char *t_targbuf = xml_escape_the_name(targbuf);
                char *t_obj_path = xml_escape_the_name(obj_path);
                char *t_link_path;
                int res;

                t_link_path = (char *)HDmalloc(HDstrlen(prefix) + linfo->u.val_size + 1);
                if(targbuf[0] == '/')
                    HDstrcpy(t_link_path, targbuf);
                else {
                    HDstrcpy(t_link_path, prefix);
                    HDstrcat(HDstrcat(t_link_path, "/"), targbuf);
                } /* end else */

                /* Create OBJ-XIDs for the parent and object */
                xml_name_to_XID(t_obj_path, linkxid, (int)sizeof(linkxid), 1);
                xml_name_to_XID(prefix, parentxid, (int)sizeof(parentxid), 1);

                /* Try to create an OBJ-XID for the object pointed to */
                res = xml_name_to_XID(t_link_path, targetxid, (int)sizeof(targetxid), 0);
                if (res == 0) {
                    /* target obj found */
                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sSoftLink LinkName=\"%s\" "
                            "OBJ-XID=\"%s\" "
                            "H5SourcePath=\"%s\" "
                            "TargetPath=\"%s\" TargetObj=\"%s\" "
                            "Parents=\"%s\" H5ParentPaths=\"%s\" />",
                            xmlnsprefix,
                            t_name,         /* LinkName */
                            linkxid,        /* OBJ-XID */
                            t_obj_path,     /* H5SourcePath */
                            t_targbuf,      /* TargetPath */
                            targetxid,      /* TargetObj */
                            parentxid,      /* Parents */
                            t_prefix);      /* H5ParentPaths */
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                } 
                else {
                    /* dangling link -- omit from xml attributes */
                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sSoftLink LinkName=\"%s\" "
                            "OBJ-XID=\"%s\" "
                            "H5SourcePath=\"%s\" "
                            "TargetPath=\"%s\"  "
                            "Parents=\"%s\" H5ParentPaths=\"%s\" />",
                            xmlnsprefix,
                            t_name,         /* LinkName */
                            linkxid,        /* OBJ-XID */
                            t_obj_path,     /* H5SourcePath */
                            t_targbuf,      /* TargetPath */
                            parentxid,      /* Parents */
                            t_prefix);      /* H5ParentPaths */
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                }

                HDfree(t_prefix);
                HDfree(t_name);
                HDfree(t_targbuf);
                HDfree(t_obj_path);
                HDfree(t_link_path);
            }

            HDfree(targbuf);
            break;

        case H5L_TYPE_EXTERNAL:
            targbuf = (char *)HDmalloc(linfo->u.val_size);
            HDassert(targbuf);

            if(H5Lget_val(group, name, targbuf, linfo->u.val_size, H5P_DEFAULT) < 0) {
                error_msg("unable to get external link value\n");
                h5tools_setstatus(EXIT_FAILURE);
                ret = FAIL;
            } /* end if */
            else {
                const char *filename;
                const char *targname;

                if(H5Lunpack_elink_val(targbuf, linfo->u.val_size, NULL, &filename, &targname) < 0) {
                    error_msg("unable to unpack external link value\n");
                    h5tools_setstatus(EXIT_FAILURE);
                    ret = FAIL;
                } /* end if */
                else {
                    char linkxid[100];
                    char parentxid[100];
                    char *t_name = xml_escape_the_name(name);
                    char *t_prefix = xml_escape_the_name(HDstrcmp(prefix,"") ? prefix : "/");
                    char *t_obj_path = xml_escape_the_name(obj_path);
                    char *t_filename = xml_escape_the_name(filename);
                    char *t_targname = xml_escape_the_name(targname);

                    /* Create OBJ-XIDs for the parent and object */
                    xml_name_to_XID(t_obj_path, linkxid, (int)sizeof(linkxid), 1);
                    xml_name_to_XID(prefix, parentxid, (int)sizeof(parentxid), 1);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sExternalLink LinkName=\"%s\" "
                            "OBJ-XID=\"%s\" "
                            "H5SourcePath=\"%s\" "
                            "TargetFilename=\"%s\"  "
                            "TargetPath=\"%s\"  "
                            "Parents=\"%s\" H5ParentPaths=\"%s\" />",
                            xmlnsprefix,
                            t_name,         /* LinkName */
                            linkxid,        /* OBJ-XID */
                            t_obj_path,     /* H5SourcePath */
                            filename,       /* TargetFilename */
                            targname,       /* TargetPath*/
                            parentxid,      /* Parents */
                            t_prefix);      /* H5ParentPaths */
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                    
                    HDfree(t_prefix);
                    HDfree(t_name);
                    HDfree(t_filename);
                    HDfree(t_targname);
                    HDfree(t_obj_path);
                } /* end else */
            } /* end else */
            HDfree(targbuf);
            break;

        case H5L_TYPE_ERROR:
        case H5L_TYPE_MAX:
            HDassert(0);
            /* fall through */
        case H5L_TYPE_HARD:
        default:
        {
            char linkxid[100];
            char parentxid[100];
            char *t_name = xml_escape_the_name(name);
            char *t_prefix = xml_escape_the_name(HDstrcmp(prefix,"") ? prefix : "/");
            char *t_obj_path = xml_escape_the_name(obj_path);

            /* Create OBJ-XIDs for the parent and object */
            xml_name_to_XID(t_obj_path, linkxid, (int)sizeof(linkxid), 1);
            xml_name_to_XID(prefix, parentxid, (int)sizeof(parentxid), 1);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sUserDefined LinkName=\"%s\" "
                    "OBJ-XID=\"%s\" "
                    "H5SourcePath=\"%s\" "
                    "LinkClass=\"%d\"  "
                    "Parents=\"%s\" H5ParentPaths=\"%s\" />",
                    xmlnsprefix,
                    t_name,             /* LinkName */
                    linkxid,            /* OBJ-XID */
                    t_obj_path,         /* H5SourcePath */
                    linfo->type,        /* LinkClass */
                    parentxid,          /* Parents */
                    t_prefix);          /* H5ParentPaths */
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            
            HDfree(t_prefix);
            HDfree(t_name);
            HDfree(t_obj_path);
        }
        break;
        } /* end switch */
    } /* end else */

done:

    h5tools_str_close(&buffer);

    if(obj_path)
        HDfree(obj_path);
    return ret;
}

/*
 * create a string suitable for and XML NCNAME.  Uses the
 * object reference to create the string.
 *
 *  'gen'; 0 - return null if not found
 *         1 - generate a fake entry and return fake id.
 */
int
xml_name_to_XID(const char *str , char *outstr, int outlen, int gen)
{
    haddr_t objno;      /* Object ID for object at path */

    if (outlen < 22) return 1;

    objno = ref_path_table_lookup(str);
    if (objno == HADDR_UNDEF) {
        if (HDstrlen(str) == 0) {
            objno = ref_path_table_lookup("/");
            if (objno == HADDR_UNDEF) {
                if (gen) {
                    objno = ref_path_table_gen_fake(str);
                    sprintf(outstr, "xid_"H5_PRINTF_HADDR_FMT, objno);
                    return 0;
                } 
                else {
                    return 1;
                }
            }
        } 
        else {
            if (gen) {
                objno = ref_path_table_gen_fake(str);
                sprintf(outstr, "xid_"H5_PRINTF_HADDR_FMT, objno);
                return 0;
            } 
            else {
                return 1;
            }
        }
    }

    sprintf(outstr, "xid_"H5_PRINTF_HADDR_FMT, objno);

    return(0);
}

static const char      *quote = "&quot;";
static const char      *amp = "&amp;";
static const char      *lt = "&lt;";
static const char      *gt = "&gt;";
static const char      *apos = "&apos;";

/*-------------------------------------------------------------------------
 * Function:    xml_escape_the_name
 *
 * Purpose:     Escape XML reserved chars in a name, so HDF5 strings
 *              and paths can be correctly read back in XML element.
 *
 * Return:      The revised string.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char                   *
xml_escape_the_name(const char *str)
{
    size_t      extra;
    size_t      len;
    size_t      i;
    const char *cp;
    char       *ncp;
    char       *rcp;
    size_t      ncp_len;

    if (!str)
        return NULL;

    cp = str;
    len = HDstrlen(str);
    extra = 0;

    for (i = 0; i < len; i++) {
        if (*cp == '\"')
            extra += (HDstrlen(quote) - 1);
        else if (*cp == '\'')
            extra += (HDstrlen(apos) - 1);
        else if (*cp == '<')
            extra += (HDstrlen(lt) - 1);
        else if (*cp == '>')
            extra += (HDstrlen(gt) - 1);
        else if (*cp == '&')
            extra += (HDstrlen(amp) - 1);

        cp++;
    }

    if (extra == 0)
        return HDstrdup(str);

    cp = str;
    ncp_len = len + extra + 1;
    rcp = ncp = (char *)HDmalloc(ncp_len);

    if (!ncp)
        return NULL;    /* ?? */

    for (i = 0; i < len; i++) {
        size_t esc_len;

        HDassert(ncp_len);
        if (*cp == '\'') {
            HDstrncpy(ncp, apos, ncp_len);
            esc_len = HDstrlen(apos);
        } 
        else if (*cp == '<') {
            HDstrncpy(ncp, lt, ncp_len);
            esc_len = HDstrlen(lt);
        } 
        else if (*cp == '>') {
            HDstrncpy(ncp, gt, ncp_len);
            esc_len = HDstrlen(gt);
        } 
        else if (*cp == '\"') {
            HDstrncpy(ncp, quote, ncp_len);
            esc_len = HDstrlen(quote);
        } 
        else if (*cp == '&') {
            HDstrncpy(ncp, amp, ncp_len);
            esc_len = HDstrlen(amp);
        } 
        else {
            *ncp = *cp;
            esc_len = 1;
        }
        ncp += esc_len;
        ncp_len -= esc_len;
        cp++;
    }

    *ncp = '\0';
    return rcp;
}

/*-------------------------------------------------------------------------
 * Function:    xml_escape_the_string
 *
 * Purpose:     Escape XML reserved chars in a string, so HDF5 strings
 *              and paths can be correctly read back in XML CDATA.
 *
 * Return:      The revised string.
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char                   *
xml_escape_the_string(const char *str, int slen)
{
    size_t      extra;
    size_t      len;
    size_t      i;
    const char *cp;
    char       *ncp;
    char       *rcp;
    size_t      ncp_len;

    if (!str)
        return NULL;

    cp = str;

    if (slen < 0)
        len = HDstrlen(str);
    else
        len = slen;

    extra = 0;

    for (i = 0; i < len; i++) {
        if (*cp == '\\')
            extra++;
        else if (*cp == '\"')
            extra++;
        else if (*cp == '\'')
            extra += (HDstrlen(apos) - 1);
        else if (*cp == '<')
            extra += (HDstrlen(lt) - 1);
        else if (*cp == '>')
            extra += (HDstrlen(gt) - 1);
        else if (*cp == '&')
            extra += (HDstrlen(amp) - 1);
        cp++;
    }

    cp = str;
    ncp_len = len + extra + 1;
    rcp = ncp = (char *) HDcalloc(ncp_len, sizeof(char));

    if (ncp == NULL)
        return NULL; /* ?? */

    for (i = 0; i < len; i++) {
        size_t esc_len;

        HDassert(ncp_len);
        if (*cp == '\\') {
            *ncp++ = '\\';
            *ncp = *cp;
            esc_len = 1;
        }
        else if (*cp == '\"') {
            *ncp++ = '\\';
            *ncp = *cp;
            esc_len = 1;
        }
        else if (*cp == '\'') {
            HDstrncpy(ncp, apos, ncp_len);
            esc_len = HDstrlen(apos);
        }
        else if (*cp == '<') {
            HDstrncpy(ncp, lt, ncp_len);
            esc_len = HDstrlen(lt);
        }
        else if (*cp == '>') {
            HDstrncpy(ncp, gt, ncp_len);
            esc_len = HDstrlen(gt);
        }
        else if (*cp == '&') {
            HDstrncpy(ncp, amp, ncp_len);
            esc_len = HDstrlen(amp);
        }
        else {
            *ncp = *cp;
            esc_len = 1;
        }
        ncp += esc_len;
        ncp_len -= esc_len;
        cp++;
    }

    *ncp = '\0';
    return rcp;
}

/**
 **  XML print functions--these replace some functions in the
 **  h5tools.c suite.
 **/

/*-------------------------------------------------------------------------
 * Function:    xml_print_datatype
 *
 * Purpose:     Print description of a datatype in XML.
 *              Note:  this is called inside a <DataType> element.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_print_datatype(hid_t type, unsigned in_group)
{
    char                   *mname;
    hid_t                   mtype;
    unsigned                nmembers;
    unsigned                ndims;
    unsigned                i;
    size_t                  size;
    hsize_t                 dims[H5DUMP_MAX_RANK];
    H5T_str_t               str_pad;
    H5T_cset_t              cset;
    hid_t                   super;
    H5T_order_t             ord;
    H5T_sign_t              sgn;
    size_t                  sz;
    size_t                  spos;
    size_t                  epos;
    size_t                  esize;
    size_t                  mpos;
    size_t                  msize;
    int                     nmembs;
    htri_t                  is_vlstr=FALSE;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
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

    if(!in_group && H5Tcommitted(type) > 0) {
        H5O_info_t oinfo;
        obj_t  *found_obj;    /* Found object */

        /* detect a shared datatype, output only once */
        H5Oget_info(type, &oinfo);
        found_obj = search_obj(type_table, oinfo.addr);

        if(found_obj) {
            /* This should be defined somewhere else */
            /* These 2 cases are handled the same right now, but
               probably will have something different eventually */
            char * dtxid = (char *)HDmalloc((size_t)100);

            xml_name_to_XID(found_obj->objname, dtxid, 100, 1);
            if (!found_obj->recorded) {
                /* 'anonymous' NDT.  Use it's object num.
                   as it's name.  */

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sNamedDataTypePtr OBJ-XID=\"/%s\"/>",
                        xmlnsprefix, dtxid);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            } 
            else {
                /* point to the NDT by name */
                char *t_objname = xml_escape_the_name(found_obj->objname);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sNamedDataTypePtr OBJ-XID=\"%s\" H5Path=\"%s\"/>",
                        xmlnsprefix, dtxid, t_objname);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                HDfree(t_objname);
            }
            HDfree(dtxid);
        } 
        else {
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- h5dump error: unknown committed type. -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            h5tools_setstatus(EXIT_FAILURE);
        }
    } 
    else {

        switch (H5Tget_class(type)) {
        case H5T_INTEGER:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level++;

            /* <hdf5:IntegerType ByteOrder="bo" Sign="torf" Size="bytes"/> */
            ord = H5Tget_order(type);
            sgn = H5Tget_sign(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sIntegerType ByteOrder=\"",xmlnsprefix);
            switch (ord) {
            case H5T_ORDER_LE:
                h5tools_str_append(&buffer, "LE");
                break;
            case H5T_ORDER_BE:
                h5tools_str_append(&buffer, "BE");
                break;
            case H5T_ORDER_VAX:
                case H5T_ORDER_MIXED:
                case H5T_ORDER_NONE:
                case H5T_ORDER_ERROR:
            default:
                h5tools_str_append(&buffer, "ERROR_UNKNOWN");
                    break;
            } /* end switch */
            
            h5tools_str_append(&buffer, "\" Sign=\"");
            
            switch (sgn) {
            case H5T_SGN_NONE:
                h5tools_str_append(&buffer, "false");
                break;
            case H5T_SGN_2:
                h5tools_str_append(&buffer, "true");
                break;
                case H5T_SGN_ERROR:
                case H5T_NSGN:
            default:
                h5tools_str_append(&buffer, "ERROR_UNKNOWN");
                    break;
            } /* end switch */
            
            h5tools_str_append(&buffer, "\" Size=\"");
            sz = H5Tget_size(type);
            h5tools_str_append(&buffer, "%lu", (unsigned long)sz);
            h5tools_str_append(&buffer, "\" />");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_FLOAT:
            /* <hdf5:FloatType ByteOrder="bo" Size="bytes"
               SignBitLocation="bytes"
               ExponentBits="eb" ExponentLocation="el"
               MantissaBits="mb" MantissaLocation="ml" /> */
            ord = H5Tget_order(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sFloatType ByteOrder=\"",xmlnsprefix);
            
            switch (ord) {
            case H5T_ORDER_LE:
                h5tools_str_append(&buffer, "LE");
                break;
            case H5T_ORDER_BE:
                h5tools_str_append(&buffer, "BE");
                break;
            case H5T_ORDER_VAX:
                h5tools_str_append(&buffer, "VAX");
                break;
                case H5T_ORDER_MIXED:
                case H5T_ORDER_NONE:
                case H5T_ORDER_ERROR:
            default:
                h5tools_str_append(&buffer, "ERROR_UNKNOWN");
            } /* end switch */
            
            h5tools_str_append(&buffer, "\" Size=\"");
            sz = H5Tget_size(type);
            h5tools_str_append(&buffer, "%lu", (unsigned long)sz);
            H5Tget_fields(type, &spos, &epos, &esize, &mpos, &msize);
            h5tools_str_append(&buffer, "\" SignBitLocation=\"%lu\" ", (unsigned long)spos);
            h5tools_str_append(&buffer, "ExponentBits=\"%lu\" ExponentLocation=\"%lu\" ", (unsigned long)esize, (unsigned long)epos);
            h5tools_str_append(&buffer, "MantissaBits=\"%lu\" MantissaLocation=\"%lu\" />", (unsigned long)msize, (unsigned long)mpos);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_TIME:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sTimeType />",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            h5tools_str_append(&buffer, "<!-- H5T_TIME: not yet implemented -->");
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_STRING:
            /* <hdf5:StringType Cset="cs" StrSize="chars" StrPad="pad" /> */
            size = H5Tget_size(type);
            str_pad = H5Tget_strpad(type);
            cset = H5Tget_cset(type);
            is_vlstr = H5Tis_variable_str(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sStringType Cset=\"",xmlnsprefix);
            if (cset == H5T_CSET_ASCII) {
                h5tools_str_append(&buffer, "H5T_CSET_ASCII\" ");
            } 
            else {
                h5tools_str_append(&buffer, "unknown_cset\" ");
            }
            if(is_vlstr)
                h5tools_str_append(&buffer, "StrSize=\"H5T_VARIABLE\" StrPad=\"");
            else
                h5tools_str_append(&buffer, "StrSize=\"%d\" StrPad=\"", (int) size);
            if (str_pad == H5T_STR_NULLTERM) {
                h5tools_str_append(&buffer, "H5T_STR_NULLTERM\"/>");
            } 
            else if (str_pad == H5T_STR_NULLPAD) {
                h5tools_str_append(&buffer, "H5T_STR_NULLPAD\"/>");
            } 
            else if (str_pad == H5T_STR_SPACEPAD) {
                h5tools_str_append(&buffer, "H5T_STR_SPACEPAD\"/>");
            } 
            else {
                h5tools_str_append(&buffer, "H5T_STR_ERROR\"/>");
            }
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_BITFIELD:
            /* <hdf5:BitfieldType ByteOrder="bo" Size="bytes"/> */
            ord = H5Tget_order(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sBitfieldType ByteOrder=\"",xmlnsprefix);
            
            switch (ord) {
            case H5T_ORDER_LE:
                h5tools_str_append(&buffer, "LE");
                break;
            case H5T_ORDER_BE:
                h5tools_str_append(&buffer, "BE");
                break;
            case H5T_ORDER_VAX:
                case H5T_ORDER_MIXED:
                case H5T_ORDER_NONE:
                case H5T_ORDER_ERROR:
            default:
                h5tools_str_append(&buffer, "ERROR_UNKNOWN");
            } /* end switch */
            
            size = H5Tget_size(type);
            h5tools_str_append(&buffer, "\" Size=\"%lu\"/>", (unsigned long)size);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_OPAQUE:
            /* <hdf5:OpaqueType Tag="tag" Size="bytes" /> */

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level++;
            mname = H5Tget_tag(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sOpaqueType Tag=\"%s\" ",xmlnsprefix, mname);
            H5free_memory(mname);
            size = H5Tget_size(type);
            h5tools_str_append(&buffer, "Size=\"%lu\"/>", (unsigned long)size);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_COMPOUND:
            /* recursively describe the components of a compound datatype */

            /* type of a dataset */
            nmembers = H5Tget_nmembers(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sCompoundType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            /* List each member Field of the type */
            /*   <hdf5:Field FieldName="name" > */
            /*   <hdf5:DataType > */
            ctx.indent_level++;
            dump_indent += COL;
            for (i = 0; i < nmembers; i++) {
                char *t_fname;

                mname = H5Tget_member_name(type, i);
                mtype = H5Tget_member_type(type, i);
                t_fname = xml_escape_the_name(mname);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sField FieldName=\"%s\">",xmlnsprefix, t_fname);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                H5free_memory(mname);
                HDfree(t_fname);
                dump_indent += COL;
                ctx.indent_level++;

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sDataType>",xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                ctx.indent_level++;
                dump_indent += COL;
                xml_print_datatype(mtype,0);
                dump_indent -= COL;
                ctx.indent_level--;

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "</%sDataType>",xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                dump_indent -= COL;
                ctx.indent_level--;

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "</%sField>",xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            }
            dump_indent -= COL;
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sCompoundType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_REFERENCE:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level++;
            /*  Only Object references supported at this time */

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sReferenceType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sObjectReferenceType />",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sReferenceType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_ENUM:
            /*  <hdf5:EnumType Nelems="ne" > list Name, values of enum */
            nmembs = H5Tget_nmembers(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            dump_indent += COL;
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sEnumType Nelems=\"%d\">",xmlnsprefix, nmembs);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            xml_print_enum(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sEnumType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            dump_indent -= COL;
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sAtomicType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_VLEN:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sVLType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            super = H5Tget_super(type);
            dump_indent += COL;
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sDataType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            dump_indent += COL;
            ctx.indent_level++;
            xml_print_datatype(super,0);
            dump_indent -= COL;
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sDataType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            dump_indent -= COL;
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sVLType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            H5Tclose(super);

            break;

        case H5T_ARRAY:
            /* Get array base type */
            super = H5Tget_super(type);

            /* Print lead-in */
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sArrayType Ndims=\"",xmlnsprefix);
            ndims = H5Tget_array_ndims(type);
            h5tools_str_append(&buffer, "%u\">", ndims);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            /* Get array information */
            H5Tget_array_dims2(type, dims);

            /* list of dimensions */
            ctx.indent_level++;
            for (i = 0; i < ndims; i++) {
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sArrayDimension DimSize=\"%u\"/>", xmlnsprefix, (int) dims[i]);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            }
            ctx.indent_level--;

            dump_indent += COL;
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sDataType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            dump_indent += COL;
            ctx.indent_level++;
            xml_print_datatype(super,0);
            dump_indent -= COL;
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sDataType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            dump_indent -= COL;
            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sArrayType>",xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            /* Close array base type */
            H5Tclose(super);
            break;

        case H5T_NO_CLASS:
        case H5T_NCLASSES:
            HDassert(0);
            /* fall through */
        default:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- unknown datatype -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            h5tools_setstatus(EXIT_FAILURE);
            break;
        }
    } /* end else */

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_datatype
 *
 * Purpose:     Dump description of a datatype in XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
xml_dump_datatype(hid_t type)
{
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
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

    ctx.indent_level++;
    dump_indent += COL;

    if(H5Tcommitted(type) > 0) {
        H5O_info_t oinfo;
        obj_t  *found_obj;    /* Found object */

        /* Datatype is a shared or named datatype */
        H5Oget_info(type, &oinfo);
        found_obj = search_obj(type_table, oinfo.addr);

        if(found_obj) {
            /* Shared datatype, must be entered as an object  */
            /* These 2 cases are the same now, but may change */
            char *dtxid = (char *)HDmalloc((size_t)100);

            xml_name_to_XID(found_obj->objname, dtxid, 100, 1);
            if (!found_obj->recorded) {
                /* anonymous stored datatype:
                   following the dumper's current
                   practice:
                   use it's object ref as its name
                 */

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sNamedDataTypePtr OBJ-XID=\"%s\"/>",
                        xmlnsprefix, dtxid);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            } 
            else {
                /* pointer to a named datatype already in XML */
                char *t_objname = xml_escape_the_name(found_obj->objname);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sNamedDataTypePtr OBJ-XID=\"%s\" H5Path=\"%s\" />",
                        xmlnsprefix, dtxid, t_objname);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                HDfree(t_objname);
            }
            HDfree(dtxid);
        } 
        else {
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- h5dump error: unknown committed type. -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        }
    }
    else {
        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sDataType>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level++;
        dump_indent += COL;
        xml_print_datatype(type, 0);
        ctx.indent_level--;
        dump_indent -= COL;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sDataType>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    }
    ctx.indent_level--;
    dump_indent -= COL;

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_dataspace
 *
 * Purpose:     Dump description of a dataspace in XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
xml_dump_dataspace(hid_t space)
{
    hsize_t         size[H5DUMP_MAX_RANK];
    hsize_t         maxsize[H5DUMP_MAX_RANK];
    int             i;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
    h5tool_format_t   string_dataformat;
    hsize_t     curr_pos = 0;        /* total data element position   */
    
    int             ndims = H5Sget_simple_extent_dims(space, size, maxsize);
    H5S_class_t     space_type = H5Sget_simple_extent_type(space);

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

    ctx.indent_level++;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sDataspace>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    ctx.indent_level++;

    switch (space_type) {
    case H5S_SCALAR:
        /* scalar dataspace (just a tag, no XML attrs. defined */

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sScalarDataspace />",xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        break;

    case H5S_SIMPLE:
        /* simple dataspace */
        /* <hdf5:SimpleDataspace Ndims="nd"> */

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sSimpleDataspace Ndims=\"%d\">",xmlnsprefix, ndims);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        /* print the <hdf5:Dimension> elements */
        ctx.indent_level++;
        for (i = 0; i < ndims; i++) {
            if (maxsize[i] == H5S_UNLIMITED) {
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sDimension  DimSize=\"%" H5_PRINTF_LL_WIDTH "u\" MaxDimSize=\"UNLIMITED\"/>",
                        xmlnsprefix,size[i]);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            } 
            else if (maxsize[i] == (hsize_t) 0) {
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sDimension  DimSize=\"%" H5_PRINTF_LL_WIDTH "u\" MaxDimSize=\"%" H5_PRINTF_LL_WIDTH "u\"/>",
                        xmlnsprefix,size[i], size[i]);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            } 
            else {
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sDimension  DimSize=\"%" H5_PRINTF_LL_WIDTH "u\" MaxDimSize=\"%" H5_PRINTF_LL_WIDTH "u\"/>",
                        xmlnsprefix, size[i], maxsize[i]);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            }
        }
        ctx.indent_level--;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sSimpleDataspace>", xmlnsprefix );
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        break;

#ifdef TMP
        /* Commented out: wait until the schema is updated first */
    case H5S_NULL:
        /* null dataspace (just a tag, no XML attrs. defined */

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sNullDataspace />",xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
       break;
#endif /* TMP */

    case H5S_NULL:
    case H5S_NO_CLASS:
    default:
        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<!-- unknown dataspace -->");
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    }

    ctx.indent_level--;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sDataspace>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    ctx.indent_level--;

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_data
 *
 * Purpose:     Dump description of data in XML.
 *              Note that this calls the h5dump_xxx calls in
 *              the h5tools library.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
xml_dump_data(hid_t obj_id, int obj_data, struct subset_t H5_ATTR_UNUSED * sset, int H5_ATTR_UNUSED pindex)
{
    hid_t               space = -1;
    hid_t               type = -1;
    hid_t               p_type = -1;
    hsize_t             size[64];
    hsize_t             nelmts = 1;
    int                 ndims;
    int                 i;
    int                 status = -1;
    void               *buf = NULL;
    hsize_t             curr_pos = 0;        /* total data element position   */
    h5tools_str_t       buffer;          /* string into which to render   */
    h5tools_context_t   ctx;             /* print context  */
    h5tool_format_t    *outputformat = &xml_dataformat;
    h5tool_format_t     string_dataformat;

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;
    
    /* Print all the values. */
    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));
    
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
    string_dataformat.cmpd_sep = " ";
    string_dataformat.cmpd_pre = "";
    string_dataformat.cmpd_suf = "";
    string_dataformat.cmpd_end = "";
    string_dataformat.arr_linebreak = 0;
    string_dataformat.arr_pre = "";
    outputformat = &string_dataformat;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    ctx.indent_level++;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sDataFromFile>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    ctx.indent_level--;
    
    dump_indent += COL;
    ctx.indent_level++;

    if (obj_data == DATASET_DATA) {
        type = H5Dget_type(obj_id);
        if (H5Tget_class(type) == H5T_REFERENCE) {
            status = xml_print_refs(obj_id, DATASET_DATA);
        } 
        else if (H5Tget_class(type) == H5T_STRING) {
            status = xml_print_strs(obj_id, DATASET_DATA);
        } 
        else {
            h5tools_context_t datactx;
            HDmemset(&datactx, 0, sizeof(datactx));
            datactx.need_prefix = TRUE;
            datactx.indent_level = ctx.indent_level;
            datactx.cur_column = ctx.cur_column;
            status = h5tools_dump_dset(rawoutstream, outputformat, &datactx, obj_id, NULL);
        }
    } 
    else {
        /* Attribute data */
        type = H5Aget_type(obj_id);

        if (H5Tget_class(type) == H5T_REFERENCE) {
            /* references are done differently than
               the standard output:
               XML dumps a path to the object
               referenced.
             */
            status = xml_print_refs(obj_id, ATTRIBUTE_DATA);
            H5Tclose(type);
        } 
        else if (H5Tget_class(type) == H5T_STRING) {
            status = xml_print_strs(obj_id, ATTRIBUTE_DATA);
        } 
        else {  /* all other data */
            /* VL data special information */
            unsigned int vl_data = 0; /* contains VL datatypes */
            
            p_type = h5tools_get_native_type(type);

            /* Check if we have VL data in the dataset's datatype */
            if (h5tools_detect_vlen(p_type) == TRUE)
                vl_data = TRUE;

            H5Tclose(type);

            space = H5Aget_space(obj_id);

            ndims = H5Sget_simple_extent_dims(space, size, NULL);

            for (i = 0; i < ndims; i++)
                nelmts *= size[i];

            buf = HDmalloc((size_t)(nelmts * MAX(H5Tget_size(type), H5Tget_size(p_type))));
            HDassert(buf);
            
            if (H5Aread(obj_id, p_type, buf) >= 0) {
                h5tools_context_t datactx;
                HDmemset(&datactx, 0, sizeof(datactx));
                datactx.need_prefix = TRUE;
                datactx.indent_level = ctx.indent_level;
                datactx.cur_column = ctx.cur_column;
                status = h5tools_dump_mem(rawoutstream, outputformat, &datactx, obj_id, p_type, space, buf);
            }
            /* Reclaim any VL memory, if necessary */
            if (vl_data)
                H5Dvlen_reclaim(p_type, space, H5P_DEFAULT, buf);

            HDfree(buf);
            H5Tclose(p_type);
            H5Sclose(space);
            H5Tclose(type);
        }
    }

    if (status == FAIL) {
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
        
        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "Unable to print data.");
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        ctx.indent_level--;

        status = 1;
    }
    ctx.indent_level--;
    dump_indent -= COL;

    ctx.indent_level++;

    ctx.need_prefix = TRUE;
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sDataFromFile>",xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    ctx.indent_level--;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_attr
 *
 * Purpose:     Dump a description of an HDF5 attribute in XML.
 *
 * Return:      herr_t
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
xml_dump_attr(hid_t attr, const char *attr_name, const H5A_info_t H5_ATTR_UNUSED *info,
    void H5_ATTR_UNUSED * op_data)
{
    hid_t             attr_id = -1;
    hid_t             type = -1;
    hid_t             space = -1;
    H5S_class_t       space_type;
    hsize_t           curr_pos = 0;        /* total data element position   */
    h5tools_str_t     buffer;          /* string into which to render   */
    h5tools_context_t ctx;           /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
    h5tool_format_t   string_dataformat;
    
    char *t_aname = xml_escape_the_name(attr_name);

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
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sAttribute Name=\"%s\">", xmlnsprefix, t_aname);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    HDfree(t_aname);

    if ((attr_id = H5Aopen(attr, attr_name, H5P_DEFAULT)) >= 0) {
        type = H5Aget_type(attr_id);
        space = H5Aget_space(attr_id);
        space_type = H5Sget_simple_extent_type(space);

        dump_function_table->dump_dataspace_function(space);
        dump_function_table->dump_datatype_function(type);

        ctx.indent_level++;
        dump_indent += COL;

        if (display_attr_data && space_type != H5S_NULL) {
            switch (H5Tget_class(type)) {
            case H5T_INTEGER:
            case H5T_FLOAT:
            case H5T_STRING:
            case H5T_BITFIELD:
            case H5T_OPAQUE:
            case H5T_ENUM:
            case H5T_ARRAY:
                dump_function_table->dump_data_function(attr_id, ATTRIBUTE_DATA, NULL, 0);
                break;

            case H5T_TIME:
                ctx.indent_level++;
                dump_indent += COL;

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<!-- Time data not yet implemented. -->");
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sNoData/>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<hdf5:Data>");
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.indent_level--;
                dump_indent -= COL;
                break;

            case H5T_COMPOUND:
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<!-- Note: format of compound data not specified -->");
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                dump_function_table->dump_data_function(attr_id, ATTRIBUTE_DATA, NULL, 0);
                break;

            case H5T_REFERENCE:
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                if (!H5Tequal(type, H5T_STD_REF_OBJ)) {
                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                    
                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<!-- Note: Region references not supported -->");
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                    
                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sNoData />", xmlnsprefix);
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                }
                else {
                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                    
                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sDataFromFile>", xmlnsprefix);
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                    xml_print_refs(attr_id, ATTRIBUTE_DATA);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                    
                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "</%sDataFromFile>", xmlnsprefix);
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                }

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                break;

            case H5T_VLEN:
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<!-- Note: format of VL data not specified -->");
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                dump_function_table->dump_data_function(attr_id, ATTRIBUTE_DATA, NULL, 0);
                break;
            case H5T_NO_CLASS:
            case H5T_NCLASSES:
                HDassert(0);
                /* fall through */
            default:
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<!-- Unknown datatype: %d -->", H5Tget_class(type));
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sNoData/>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                break;
            }
        }
        else {
            /* The case of an attribute never yet written ??
             * Or dataspace is H5S_NULL. */
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
            
            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
            
            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sNoData/>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.indent_level--;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
            
            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        }
        ctx.indent_level--;
        dump_indent -= COL;

        H5Tclose(type);
        H5Sclose(space);
        H5Aclose(attr_id);

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
        
        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sAttribute>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        h5tools_str_close(&buffer);
        return SUCCEED;
    }
    else {
        /* ?? failed */
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
        
        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<!-- h5dump error: unable to open attribute. -->");
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        ctx.indent_level--;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
        
        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sAttribute>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        h5tools_str_close(&buffer);
        
        h5tools_setstatus(EXIT_FAILURE);
        return FAIL;
    }
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_named_datatype
 *
 * Purpose:     Dump a description of an HDF5 NDT in XML.
 *
 * Return:      herr_t
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
xml_dump_named_datatype(hid_t type, const char *name)
{
    hsize_t           curr_pos = 0;        /* total data element position   */
    h5tools_str_t     buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
    h5tool_format_t   string_dataformat;
    char             *tmp;
    char             *dtxid;
    char             *parentxid;
    char             *t_tmp;
    char             *t_prefix;
    char             *t_name;

    tmp = (char *)HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    HDstrcpy(tmp, prefix);
    HDstrcat(tmp, "/");
    HDstrcat(tmp, name);

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

    dtxid = (char *)HDmalloc((size_t)100);
    parentxid = (char *)HDmalloc((size_t)100);
    t_tmp = xml_escape_the_name(tmp);
    t_prefix = xml_escape_the_name(prefix);
    t_name = xml_escape_the_name(name);

    xml_name_to_XID(tmp, dtxid, 100, 1);
    xml_name_to_XID(prefix, parentxid, 100, 1);
    if(HDstrncmp(name, "#", (size_t)1) == 0) {
        /*  Special:  this is an 'anonymous' NDT, deleted but
           still in use.
           We follow the dumper's undocumented practice, and
           use its object id as its name.
           Exactly the same as normal, but a separate case
           in the event we want to do something else in
           the future.
         */

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
        
        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sNamedDataType Name=\"%s\" OBJ-XID=\"%s\" "
                "Parents=\"%s\" H5ParentPaths=\"%s\">",
                xmlnsprefix,
                name, dtxid,
                parentxid, HDstrcmp(prefix,"") ? t_prefix : "/");
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    } 
    else {
        H5O_info_t  oinfo;          /* Object info */

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
        
        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sNamedDataType Name=\"%s\" OBJ-XID=\"%s\" "
                "H5Path=\"%s\" Parents=\"%s\" H5ParentPaths=\"%s\">",
                xmlnsprefix,
                t_name, dtxid,
                t_tmp, parentxid, (HDstrcmp(prefix, "") ? t_prefix : "/"));
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        /* Check uniqueness of named datatype */
        H5Oget_info(type, &oinfo);
        if(oinfo.rc > 1) {
            obj_t       *found_obj;     /* Found object */

            /* Group with more than one link to it... */
            found_obj = search_obj(type_table, oinfo.addr);

            if (found_obj == NULL) {
                indentation(dump_indent);
                error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            } 
            else if(found_obj->displayed) {
                /* We have already printed this named datatype, print it as a
                 * NamedDatatypePtr
                 */
                char pointerxid[100];
                char *t_objname = xml_escape_the_name(found_obj->objname);

                ctx.indent_level++;
                
                xml_name_to_XID(found_obj->objname, pointerxid, (int)sizeof(pointerxid), 1);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sNamedDatatypePtr OBJ-XID=\"%s\" H5Path=\"%s\"/>", xmlnsprefix, pointerxid, t_objname);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                
                ctx.indent_level--;

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
                
                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "</%sNamedDataType>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                HDfree(t_objname);
                goto done;
            } 
            else
                found_obj->displayed = TRUE;
        }
    }

    ctx.indent_level++;
    dump_indent += COL;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sDataType>",xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    ctx.indent_level++;
    dump_indent += COL;
    xml_print_datatype(type,1);
    ctx.indent_level--;
    dump_indent -= COL;
    
    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sDataType>",xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    ctx.indent_level--;
    dump_indent -= COL;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);
    
    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sNamedDataType>",xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

done:

    h5tools_str_close(&buffer);

    HDfree(dtxid);
    HDfree(parentxid);
    HDfree(t_tmp);
    HDfree(t_prefix);
    HDfree(t_name);
    HDfree(tmp);
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_group
 *
 * Purpose:     Dump a description of an HDF5 Group (and its members) in XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *  Pedro Vicente, October 9, 2007
 *   added parameters to H5A(L)iterate to allow for other iteration orders
 *
 *-------------------------------------------------------------------------
 */
void
xml_dump_group(hid_t gid, const char *name)
{
    H5O_info_t              oinfo;
    hid_t                   gcpl_id;
    hid_t                   dset, type;
    unsigned                crt_order_flags;
    unsigned                attr_crt_order_flags;
    int                     isRoot = 0;
    char                    type_name[1024];
    char                   *t_objname = NULL;
    char                   *par_name = NULL;
    char                   *cp = NULL;
    char                   *tmp = NULL;
    char                   *par = NULL;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
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

    if(HDstrcmp(name, "/") == 0) {
        isRoot = 1;
        tmp = HDstrdup("/");
    }
    else {
        tmp = (char *)HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
        HDstrcpy(tmp, prefix);
        par = HDstrdup(tmp);
        cp = HDstrrchr(par, '/');
        if(cp) {
            if((cp == par) && HDstrlen(par) > 1)
                *(cp + 1) = '\0';
            else
                *cp = '\0';
        }
    }

    H5Oget_info(gid, &oinfo);

    if(oinfo.rc > 1) {
        obj_t *found_obj;    /* Found object */

        /* Group with more than one link to it... */
        found_obj = search_obj(group_table, oinfo.addr);

        if (found_obj == NULL) {
            indentation(dump_indent);
            error_msg("internal error (file %s:line %d)\n", __FILE__, __LINE__);
            h5tools_setstatus(EXIT_FAILURE);
        }
        else {
            char *t_name = xml_escape_the_name(name);
            char *grpxid = (char *)HDmalloc((size_t)100);
            char *parentxid = (char *)HDmalloc((size_t)100);

            if(found_obj->displayed) {
                char *ptrstr = (char *)HDmalloc((size_t)100);

                /* already seen: enter a groupptr */
                if(isRoot) {
                    /* probably can't happen! */
                    xml_name_to_XID("/", grpxid, 100, 1);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sRootGroup OBJ-XID=\"%s\" H5Path=\"%s\">",
                            xmlnsprefix, grpxid, "/");
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                }
                else {
                    t_objname = xml_escape_the_name(found_obj->objname);
                    par_name = xml_escape_the_name(par);
                    xml_name_to_XID(tmp, grpxid, 100, 1);
                    xml_name_to_XID(par, parentxid, 100, 1);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sGroup Name=\"%s\" OBJ-XID=\"%s-%d\" H5Path=\"%s\" "
                            "Parents=\"%s\" H5ParentPaths=\"%s\">",
                            xmlnsprefix,t_name, grpxid, get_next_xid(),
                            t_objname, parentxid, par_name);
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                    HDfree(t_objname);
                    HDfree(par_name);
                    
                    ctx.indent_level++;

                    t_objname = xml_escape_the_name(found_obj->objname);/* point to the NDT by name */
                    par_name = xml_escape_the_name(par);
                    xml_name_to_XID(found_obj->objname, ptrstr, 100, 1);
                    xml_name_to_XID(par, parentxid, 100, 1);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sGroupPtr OBJ-XID=\"%s\" H5Path=\"%s\" "
                                "Parents=\"%s\" H5ParentPaths=\"%s\" />",
                                xmlnsprefix,
                                ptrstr, t_objname, parentxid, par_name);
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                    
                    ctx.indent_level--;

                    HDfree(t_objname);
                    HDfree(par_name);
                }
                HDfree(ptrstr);
            }
            else {

                /* first time this group has been seen -- describe it  */
                if(isRoot) {
                    xml_name_to_XID("/", grpxid, 100, 1);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sRootGroup OBJ-XID=\"%s\" H5Path=\"%s\">",
                            xmlnsprefix, grpxid, "/");
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                }
                else {
                    char *t_tmp = xml_escape_the_name(tmp);

                    par_name = xml_escape_the_name(par);
                    xml_name_to_XID(tmp, grpxid, 100, 1);
                    xml_name_to_XID(par, parentxid, 100, 1);

                    ctx.need_prefix = TRUE;
                    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                    /* Render the element */
                    h5tools_str_reset(&buffer);
                    h5tools_str_append(&buffer, "<%sGroup Name=\"%s\" OBJ-XID=\"%s\" H5Path=\"%s\" "
                            "Parents=\"%s\" H5ParentPaths=\"%s\" >",
                            xmlnsprefix,t_name, grpxid, t_tmp, parentxid, par_name);
                    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                    HDfree(t_tmp);
                    HDfree(par_name);
                }
                found_obj->displayed = TRUE;

                /* 1.  do all the attributes of the group */
                
                ctx.indent_level++;
                dump_indent += COL;

                if((sort_by == H5_INDEX_CRT_ORDER) && (attr_crt_order_flags & H5P_CRT_ORDER_TRACKED)) {
                    if(H5Aiterate2(gid, sort_by, sort_order, NULL, dump_function_table->dump_attribute_function, NULL) < 0) {
                        error_msg("error getting attribute information\n");
                        h5tools_setstatus(EXIT_FAILURE);
                    } /* end if */
                } /* end if */
                else {
                    if(H5Aiterate2(gid, H5_INDEX_NAME, sort_order, NULL, dump_function_table->dump_attribute_function, NULL) < 0) {
                        error_msg("error getting attribute information\n");
                        h5tools_setstatus(EXIT_FAILURE);
                    } /* end if */
                } /* end else */

                if(isRoot && unamedtype) {
                    unsigned u;

                    /* Very special case: dump unamed type in root group */
                    for(u = 0; u < type_table->nobjs; u++) {
                        if(!type_table->objs[u].recorded) {
                            dset = H5Dopen2(gid, type_table->objs[u].objname, H5P_DEFAULT);
                            type = H5Dget_type(dset);
                            sprintf(type_name, "#"H5_PRINTF_HADDR_FMT, type_table->objs[u].objno);
                            dump_function_table->dump_named_datatype_function(type, type_name);
                            H5Tclose(type);
                            H5Dclose(dset);
                        }
                    }
                }

                /* iterate through all the links */

                if((sort_by == H5_INDEX_CRT_ORDER) && (crt_order_flags & H5P_CRT_ORDER_TRACKED))
                    H5Literate(gid, sort_by, sort_order, NULL, xml_dump_all_cb, NULL);
                else
                    H5Literate(gid, H5_INDEX_NAME, sort_order, NULL, xml_dump_all_cb, NULL);

                dump_indent -= COL;
                ctx.indent_level--;
            }
            HDfree(t_name);
            HDfree(grpxid);
            HDfree(parentxid);
        }
    }
    else {
        /* only link -- must be first time! */
        char *t_name = xml_escape_the_name(name);
        char *grpxid = (char *)HDmalloc((size_t)100);
        char *parentxid = (char *)HDmalloc((size_t)100);

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);

        if(isRoot) {
            xml_name_to_XID("/", grpxid, 100, 1);
            h5tools_str_append(&buffer, "<%sRootGroup OBJ-XID=\"%s\" H5Path=\"%s\">", xmlnsprefix, grpxid, "/");
        }
        else {
            char *t_tmp = xml_escape_the_name(tmp);

            par_name = xml_escape_the_name(par);
            xml_name_to_XID(tmp, grpxid, 100, 1);
            xml_name_to_XID(par, parentxid, 100, 1);
            h5tools_str_append(&buffer, "<%sGroup Name=\"%s\" OBJ-XID=\"%s\" H5Path=\"%s\" "
                    "Parents=\"%s\" H5ParentPaths=\"%s\" >",
                    xmlnsprefix, t_name, grpxid, t_tmp, parentxid, par_name);
            HDfree(t_tmp);
            HDfree(par_name);
        }
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        HDfree(t_name);
        HDfree(grpxid);
        HDfree(parentxid);

        /* 1.  do all the attributes of the group */
        
        ctx.indent_level++;
        dump_indent += COL;

        if((sort_by == H5_INDEX_CRT_ORDER) && (attr_crt_order_flags & H5P_CRT_ORDER_TRACKED)) {
            if(H5Aiterate2(gid, sort_by, sort_order, NULL, dump_function_table->dump_attribute_function, NULL) < 0) {
                error_msg("error getting attribute information\n");
                h5tools_setstatus(EXIT_FAILURE);
            } /* end if */
        } /* end if */
        else {
            if(H5Aiterate2(gid, H5_INDEX_NAME, sort_order, NULL, dump_function_table->dump_attribute_function, NULL) < 0) {
                error_msg("error getting attribute information\n");
                h5tools_setstatus(EXIT_FAILURE);
            } /* end if */
        } /* end else */

        if(isRoot && unamedtype) {
            unsigned u;

            /* Very special case: dump unamed type in root group */
            for(u = 0; u < type_table->nobjs; u++) {
                if(!type_table->objs[u].recorded) {
                    dset = H5Dopen2(gid, type_table->objs[u].objname, H5P_DEFAULT);
                    type = H5Dget_type(dset);
                    sprintf(type_name, "#"H5_PRINTF_HADDR_FMT, type_table->objs[u].objno);
                    dump_function_table->dump_named_datatype_function(type, type_name);
                    H5Tclose(type);
                    H5Dclose(dset);
                }
            }
        }

        /* iterate through all the links */

        if((sort_by == H5_INDEX_CRT_ORDER) && (crt_order_flags & H5P_CRT_ORDER_TRACKED))
            H5Literate(gid, sort_by, sort_order, NULL, xml_dump_all_cb, NULL);
        else
            H5Literate(gid, H5_INDEX_NAME, sort_order, NULL, xml_dump_all_cb, NULL);

        dump_indent -= COL;
        ctx.indent_level--;
    }

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    if(isRoot)
        h5tools_str_append(&buffer, "</%sRootGroup>", xmlnsprefix);
    else
        h5tools_str_append(&buffer, "</%sGroup>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
    
    if(par)
        HDfree(par);
    if(tmp)
        HDfree(tmp);
}

/*-------------------------------------------------------------------------
 * Function:    xml_print_refs
 *
 * Purpose:     Print a path to the objects referenced by HDF5 Referneces.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
xml_print_refs(hid_t did, int source)
{
    herr_t      e;
    hid_t       type    = -1;
    hid_t       space   = -1;
    hssize_t    ssiz    = -1;
    hsize_t     i;
    size_t      tsiz;
    hobj_ref_t *refbuf = NULL;
    char       *buf = NULL;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
    h5tool_format_t   string_dataformat;
    hsize_t     curr_pos = 0;        /* total data element position   */

    if (source == DATASET_DATA) {
        type = H5Dget_type(did);
    }
    else if (source == ATTRIBUTE_DATA) {
        type = H5Aget_type(did);
    }
    else {
        /* return an error */
        return FAIL;
    }
    if (H5Tget_class(type) != H5T_REFERENCE) {
        /* return an error */
        goto error;
    }
    if (!H5Tequal(type, H5T_STD_REF_OBJ)) {
        /* region ref not supported yet... */
        /* return an error */
        goto error;
    }
    if (source == DATASET_DATA) {
        space = H5Dget_space(did);
        if ((ssiz = H5Sget_simple_extent_npoints(space)) < 0)
            goto error;
        if ((tsiz = H5Tget_size(type)) == 0)
            goto error;

        buf = (char *) HDcalloc((size_t)(ssiz * tsiz), sizeof(char));
        if (buf == NULL)
            goto error;
        e = H5Dread(did, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
        /* need to check result here */
        if (e < 0) {
            goto error;
        }
    }
    else if (source == ATTRIBUTE_DATA) {
        space = H5Aget_space(did);
        if ((ssiz = H5Sget_simple_extent_npoints(space)) < 0)
            goto error;
        if ((tsiz = H5Tget_size(type)) == 0)
            goto error;

        buf = (char *) HDcalloc((size_t)(ssiz * tsiz), sizeof(char));
        if (buf == NULL) {
            goto error;
        }
        e = H5Aread(did, H5T_STD_REF_OBJ, buf);
        /* need to check the result here */
        if (e < 0) {
            goto error;
        }
    }

    refbuf = (hobj_ref_t *) buf;

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

    for (i = 0; i < (hsize_t)ssiz; i++) {
        const char *path = lookup_ref_path(*refbuf);
        ctx.indent_level++;

        if (!path) {
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"%s\"", "NULL");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        }
        else {
            char *t_path = xml_escape_the_string(path, -1);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"%s\"", t_path);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            HDfree(t_path);
        }
        ctx.indent_level--;

        refbuf++;
    }

    h5tools_str_close(&buffer);

    HDfree(buf);
    H5Tclose(type);
    H5Sclose(space);
    return SUCCEED;
    
error:
    if(buf)
        HDfree(buf);

    H5E_BEGIN_TRY {
        H5Tclose(type);
        H5Sclose(space);
    } H5E_END_TRY;
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    xml_print_strs
 *
 * Purpose:     Print strings.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
xml_print_strs(hid_t did, int source)
{
    herr_t      e;
    hid_t       type    = -1;
    hid_t       space   = -1;
    hssize_t    ssiz    = -1;
    htri_t      is_vlstr = FALSE;
    size_t      tsiz    = 0;
    size_t      i;
    size_t      str_size = 0;
    char       *bp = NULL;
    char       *onestring = NULL;
    void       *buf = NULL;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
    h5tool_format_t   string_dataformat;
    hsize_t     curr_pos = 0;        /* total data element position   */

    if (source == DATASET_DATA) {
        type = H5Dget_type(did);
    }
    else if (source == ATTRIBUTE_DATA) {
        type = H5Aget_type(did);
    }
    else {
        /* return an error */
        return FAIL;
    }
    if (H5Tget_class(type) != H5T_STRING) {
        /* return an error */
        goto error;
    }
    /* Check if we have VL data in the dataset's datatype */
    is_vlstr = H5Tis_variable_str(type);

    if (source == DATASET_DATA) {
        space = H5Dget_space(did);
        if((ssiz = H5Sget_simple_extent_npoints(space)) < 0)
            goto error;
        if((tsiz = H5Tget_size(type)) == 0)
            goto error;

        buf = HDmalloc((size_t)(ssiz * tsiz));
        if (buf == NULL)
            goto error;

        e = H5Dread(did, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
        if (e < 0) {
            goto error;
        }
    }
    else if (source == ATTRIBUTE_DATA) {
        space = H5Aget_space(did);
        if((ssiz = H5Sget_simple_extent_npoints(space)) < 0)
            goto error;
        if((tsiz = H5Tget_size(type)) == 0)
            goto error;

        buf = HDmalloc((size_t)(ssiz * tsiz));
        if (buf == NULL)
            goto error;

        e = H5Aread(did, type, buf);
        if (e < 0) {
            goto error;
        }
    }

    bp = (char*) buf;
    if (!is_vlstr)
        onestring = (char *) HDcalloc(tsiz, sizeof(char));

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

    for (i = 0; i < ssiz; i++) {
        if (is_vlstr) {
            onestring = *(char **) bp;
            if (onestring)
                str_size = (size_t) HDstrlen(onestring);
        }
        else {
            HDstrncpy(onestring, bp, tsiz);
            str_size = tsiz;
        }

        if (!onestring) {
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "NULL");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        }
        else {
            char *t_onestring = xml_escape_the_string(onestring, (int) str_size);
            if (t_onestring) {
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "\"%s\"", t_onestring);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
                HDfree(t_onestring);
            }
        }
        bp += tsiz;
    }

    h5tools_str_close(&buffer);

    /* Reclaim any VL memory, if necessary */
    if (!is_vlstr)
        if (onestring)
            HDfree(onestring);
    if (buf) {
        if (is_vlstr)
            H5Dvlen_reclaim(type, space, H5P_DEFAULT, buf);
        HDfree(buf);
    }
    H5Tclose(type);
    H5Sclose(space);
    return SUCCEED;
    
error:
    if(buf)
        HDfree(buf);

    H5E_BEGIN_TRY {
        H5Tclose(type);
        H5Sclose(space);
    } H5E_END_TRY;
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    check_filters
 *
 * Purpose:     private function to check for the filters and
 *              put tags in the XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
check_filters(hid_t dcpl)
{
    int             nfilt;
    int             i;
    H5Z_filter_t    filter;
    char            namebuf[120];
    size_t          cd_nelmts = 20;
    unsigned int    cd_values[20];
    unsigned int    flags;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
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

    nfilt = H5Pget_nfilters(dcpl);
    if (nfilt <= 0)
        return;
    for (i = 0; i < nfilt; i++) {
        filter = H5Pget_filter2(dcpl, (unsigned) i, &flags, (size_t *) &cd_nelmts, cd_values, (size_t)120, namebuf, NULL);
        if (filter == H5Z_FILTER_DEFLATE) {
            ctx.indent_level++;
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sDeflate Level=\"", xmlnsprefix);
            if (cd_nelmts < 1) {
                /* not sure what this means? */
                h5tools_str_append(&buffer, "6");
            }
            else {
                h5tools_str_append(&buffer, "%d", cd_values[0]);
            }
            h5tools_str_append(&buffer, "\"/>");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;
        }
        else if (filter == H5Z_FILTER_FLETCHER32) {
            ctx.indent_level++;
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sFletcher32 />", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;
        }
        else if (filter == H5Z_FILTER_SHUFFLE) {
            ctx.indent_level++;
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sShuffle />", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;
        }
        else if (filter == H5Z_FILTER_SZIP) {
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sSZIP ", xmlnsprefix);
            if (cd_nelmts < 2) {
                /* no pixels ? */
                h5tools_str_append(&buffer, "Pixels_per_block=\"-1\" ");
            }
            else {
                h5tools_str_append(&buffer, "Pixels_per_block=\"%d\" ", cd_values[1]);
            }
            /* analyse the options mask */
            if (cd_values[0] & H5_SZIP_CHIP_OPTION_MASK) {
                h5tools_str_append(&buffer, "Mode =\"Hardware\" ");
            }
            else if (cd_values[0] & H5_SZIP_ALLOW_K13_OPTION_MASK) {
                h5tools_str_append(&buffer, "Mode =\"K13\" ");
            }
            h5tools_str_append(&buffer, "Coding=\"");
            if (cd_values[0] & H5_SZIP_EC_OPTION_MASK) {
                h5tools_str_append(&buffer, "Entropy");
            }
            else if (cd_values[0] & H5_SZIP_NN_OPTION_MASK) {
                h5tools_str_append(&buffer, "NN");
            }
            h5tools_str_append(&buffer, "\" ");

            h5tools_str_append(&buffer, "ByteOrder=\"");
            if (cd_values[0] & H5_SZIP_LSB_OPTION_MASK) {
                h5tools_str_append(&buffer, "LSB");
            }
            else if (cd_values[0] & H5_SZIP_MSB_OPTION_MASK) {
                h5tools_str_append(&buffer, "MSB");
            }
            h5tools_str_append(&buffer, "\" ");

            if (cd_values[0] & H5_SZIP_RAW_OPTION_MASK) {
                h5tools_str_append(&buffer, "Header=\"Raw\"");
            }
            h5tools_str_append(&buffer, "/>");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level--;
        }
        else {
            /* unknown option */
        }
    }

    h5tools_str_close(&buffer);
}

static void
xml_dump_fill_value(hid_t dcpl, hid_t type)
{
    size_t      sz;
    size_t      i;
    hsize_t     space;
    void       *buf;
    char       *name;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
    h5tool_format_t   string_dataformat;
    hsize_t           curr_pos = 0;        /* total data element position   */

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

    ctx.indent_level++;
    dump_indent += COL;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    ctx.indent_level++;
    dump_indent += COL;

    space = H5Tget_size(type);
    buf = HDmalloc((size_t) space);

    H5Pget_fill_value(dcpl, type, buf);

    if (H5Tget_class(type) == H5T_REFERENCE) {
        const char * path = lookup_ref_path(*(hobj_ref_t *) buf);

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sDataFromFile>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        if (!path) {
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"%s\"", "NULL");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        }
        else {
            char *t_path = xml_escape_the_string(path, -1);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"%s\"", t_path);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            HDfree(t_path);
        }

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sDataFromFile>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    }
    else if (H5Tget_class(type) == H5T_STRING) {
        /* ????? */
        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<!-- String fill values not yet implemented. -->");
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sNoData />", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    }
    else {
        /* all other data */
        switch (H5Tget_class(type)) {
        case H5T_INTEGER:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sDataFromFile>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"%d\"", *(int *) buf);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sDataFromFile>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        case H5T_FLOAT:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sDataFromFile>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"%f\"", (double)*(float *)buf);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sDataFromFile>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        case H5T_BITFIELD:
        case H5T_OPAQUE:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sDataFromFile>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            sz = H5Tget_size(type);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"");
            for (i = 0; i < sz; i++) {
                h5tools_str_append(&buffer, "%x ", *(unsigned int *) buf);
                buf = (char *) buf + sizeof(unsigned int);
            }
            h5tools_str_append(&buffer, "\"");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sDataFromFile>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        case H5T_ENUM:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sDataFromFile>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            name = H5Tget_member_name(type, *(unsigned *) buf);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "\"%s\"", name);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            if(name)
                H5free_memory(name);
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sDataFromFile>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        case H5T_ARRAY:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- Array fill values not yet implemented. -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sNoData />", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        case H5T_TIME:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- Time fill not yet implemented. -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sNoData />", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        case H5T_COMPOUND:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- Compound fill not yet implemented. -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sNoData />", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        case H5T_VLEN:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- VL fill not yet implemented. -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sNoData />", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
            HDassert(0);
            /* fall through */
        case H5T_STRING:
        case H5T_REFERENCE:
        default:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- Unknown fill datatype: %d -->", H5Tget_class(type));
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sNoData/>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        }
    }
    HDfree(buf);
    ctx.indent_level--;
    dump_indent -= COL;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    ctx.indent_level--;
    dump_indent -= COL;

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    xml_dump_dataset
 *
 * Purpose:     Dump a description of an HDF5 dataset in XML.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *  Pedro Vicente, October 9, 2007
 *   added parameters to H5Aiterate2 to allow for other iteration orders
 *
 *-------------------------------------------------------------------------
 */
void
xml_dump_dataset(hid_t did, const char *name, struct subset_t H5_ATTR_UNUSED * sset)
{
    hid_t               type;
    hid_t               space;
    hid_t               dcpl;
    H5D_fill_value_t    fvstatus;
    int                 maxdims;
    hsize_t            *chsize;
    int                 ndims;
    int                 i;
    H5D_alloc_time_t    at;
    H5D_fill_time_t     ft;
    hsize_t             tempi;
    char               *tmp;
    char               *t_name;
    char               *t_tmp;
    char               *t_prefix;
    unsigned            attr_crt_order_flags;
    h5tools_str_t       buffer;          /* string into which to render   */
    h5tools_context_t   ctx;            /* print context  */
    h5tool_format_t    *outputformat = &xml_dataformat;
    h5tool_format_t     string_dataformat;
    hsize_t             curr_pos = 0;        /* total data element position   */
    
    char *rstr = (char*) HDmalloc((size_t)100);
    char *pstr = (char*) HDmalloc((size_t)100);

    tmp = (char*) HDmalloc(HDstrlen(prefix) + HDstrlen(name) + 2);
    HDstrcpy(tmp, prefix);
    HDstrcat(tmp, "/");
    HDstrcat(tmp, name);

    /* setup */
    HDmemset(&buffer, 0, sizeof(h5tools_str_t));

    HDmemset(&ctx, 0, sizeof(ctx));
    ctx.indent_level = dump_indent/COL;
    ctx.cur_column = dump_indent;

    t_name = xml_escape_the_name(name);
    t_tmp = xml_escape_the_name(tmp);
    t_prefix = xml_escape_the_name(prefix);
    
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

    xml_name_to_XID(tmp, rstr, 100, 1);
    xml_name_to_XID(prefix, pstr, 100, 1);

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sDataset Name=\"%s\" OBJ-XID=\"%s\" H5Path= \"%s\" Parents=\"%s\" H5ParentPaths=\"%s\">",
            xmlnsprefix, t_name, rstr, t_tmp, pstr,
            strcmp(prefix, "") ? t_prefix : "/");
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    HDfree(t_name);
    HDfree(t_tmp);
    HDfree(t_prefix);
    HDfree(rstr);
    HDfree(pstr);
    HDfree(tmp);

    dcpl = H5Dget_create_plist(did);
    type = H5Dget_type(did);
    space = H5Dget_space(did);

    /* query the creation properties for attributes */
    H5Pget_attr_creation_order(dcpl, &attr_crt_order_flags);

    /* Print information about storage layout */
    if (H5D_CHUNKED == H5Pget_layout(dcpl)) {
        maxdims = H5Sget_simple_extent_ndims(space);
        HDassert(maxdims >= 0);
        chsize = (hsize_t *)HDmalloc((size_t)maxdims * sizeof(hsize_t));
        ctx.indent_level++;
        dump_indent += COL;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sStorageLayout>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level++;
        dump_indent += COL;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sChunkedLayout ", xmlnsprefix);
        ndims = H5Pget_chunk(dcpl, maxdims, chsize);
        h5tools_str_append(&buffer, "Ndims=\"%d\">", ndims);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        ctx.indent_level++;
        dump_indent += COL;

        for (i = 0; i < ndims; i++) {

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sChunkDimension DimSize=\"%" H5_PRINTF_LL_WIDTH "u\" />", xmlnsprefix, chsize[i]);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        }

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sRequiredFilter>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        ctx.indent_level++;
        dump_indent += COL;
        check_filters(dcpl);
        ctx.indent_level--;
        dump_indent -= COL;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sRequiredFilter>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        ctx.indent_level--;
        dump_indent -= COL;
        
        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sChunkedLayout>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;
        dump_indent -= COL;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sStorageLayout>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;
        dump_indent -= COL;
        HDfree(chsize);
    }
    else if (H5D_CONTIGUOUS == H5Pget_layout(dcpl)) {
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sStorageLayout>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sContiguousLayout/>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sStorageLayout>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;
    }
    else if (H5D_COMPACT == H5Pget_layout(dcpl)) {
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sStorageLayout>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sCompactLayout/>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sStorageLayout>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;
    }
    /* and check for external.... ?? */

    /* fill value */

    ctx.indent_level++;
    dump_indent += COL;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sFillValueInfo ", xmlnsprefix);
    H5Pget_fill_time(dcpl, &ft);
    h5tools_str_append(&buffer, "FillTime=\"");
    switch (ft) {
    case H5D_FILL_TIME_ALLOC:
        h5tools_str_append(&buffer, "FillOnAlloc");
        break;
    case H5D_FILL_TIME_NEVER:
        h5tools_str_append(&buffer, "FillNever");
        break;
    case H5D_FILL_TIME_IFSET:
        h5tools_str_append(&buffer, "FillIfSet");
        break;
        case H5D_FILL_TIME_ERROR:
            HDassert(0);
            /* fall through */
    default:
        h5tools_str_append(&buffer, "?");
        break;
    } /* end switch */
    h5tools_str_append(&buffer, "\" ");
    H5Pget_alloc_time(dcpl, &at);
    h5tools_str_append(&buffer, "AllocationTime=\"");
    switch (at) {
    case H5D_ALLOC_TIME_EARLY:
        h5tools_str_append(&buffer, "Early");
        break;
    case H5D_ALLOC_TIME_INCR:
        h5tools_str_append(&buffer, "Incremental");
        break;
    case H5D_ALLOC_TIME_LATE:
        h5tools_str_append(&buffer, "Late");
        break;
    case H5D_ALLOC_TIME_DEFAULT:
        case H5D_ALLOC_TIME_ERROR:
            HDassert(0);
            /* fall through */
    default:
        h5tools_str_append(&buffer, "?");
        break;
    } /* end switch */
    h5tools_str_append(&buffer, "\"");
    h5tools_str_append(&buffer, ">");
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    ctx.indent_level++;
    dump_indent += COL;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sFillValue>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    H5Pfill_value_defined(dcpl, &fvstatus);
    if (fvstatus == H5D_FILL_VALUE_UNDEFINED || (fvstatus == H5D_FILL_VALUE_DEFAULT && ft == H5D_FILL_TIME_IFSET)) {
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sNoFill/>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;
    }
    else {
        xml_dump_fill_value(dcpl, type);
    }

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sFillValue>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    ctx.indent_level--;
    dump_indent -= COL;

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sFillValueInfo>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    ctx.indent_level--;
    dump_indent -= COL;

    dump_function_table->dump_dataspace_function(space);
    dump_function_table->dump_datatype_function(type);

    ctx.indent_level++;
    dump_indent += COL;

    if ((sort_by == H5_INDEX_CRT_ORDER) && (attr_crt_order_flags & H5P_CRT_ORDER_TRACKED)) {
        if (H5Aiterate2(did, sort_by, sort_order, NULL, dump_function_table->dump_attribute_function, NULL) < 0) {
            error_msg("error getting attribute information\n");
            h5tools_setstatus(EXIT_FAILURE);
        } /* end if */
    } /* end if */
    else {
        if (H5Aiterate2(did, H5_INDEX_NAME, sort_order, NULL, dump_function_table->dump_attribute_function, NULL) < 0) {
            error_msg("error getting attribute information\n");
            h5tools_setstatus(EXIT_FAILURE);
        } /* end if */
    } /* end else */

    ctx.indent_level--;
    dump_indent -= COL;
    tempi = H5Dget_storage_size(did);

    if (display_data && (tempi > 0)) {
        switch (H5Tget_class(type)) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_STRING:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_ARRAY:
            ctx.indent_level++;
            dump_indent += COL;
            dump_function_table->dump_data_function(did, DATASET_DATA, NULL, 0);
            ctx.indent_level--;
            dump_indent -= COL;
            break;

        case H5T_TIME:
            ctx.indent_level++;

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- Time data not yet implemented. -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sNoData />", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.indent_level--;
            break;

        case H5T_COMPOUND:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- Note: format of compound data not specified -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.indent_level++;
            dump_indent += COL;
            dump_function_table->dump_data_function(did, DATASET_DATA, NULL, 0);
            ctx.indent_level--;
            dump_indent -= COL;
            break;

        case H5T_REFERENCE:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            if (!H5Tequal(type, H5T_STD_REF_OBJ)) {
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<!-- Note: Region references not supported -->");
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sNoData />", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            }
            else {
                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "<%sDataFromFile>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

                xml_print_refs(did, DATASET_DATA);

                ctx.need_prefix = TRUE;
                h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

                /* Render the element */
                h5tools_str_reset(&buffer);
                h5tools_str_append(&buffer, "</%sDataFromFile>", xmlnsprefix);
                h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            }

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;

        case H5T_VLEN:
            ctx.indent_level--;
            dump_indent -= COL;
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- Note: format of VL data not specified -->");
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            ctx.indent_level++;
            dump_indent += COL;

            ctx.indent_level++;
            dump_indent += COL;
            dump_function_table->dump_data_function(did, DATASET_DATA, NULL, 0);
            ctx.indent_level--;
            dump_indent -= COL;
            break;
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
            HDassert(0);
            /* fall through */

        default:
            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<!-- Unknown datatype: %d -->", H5Tget_class(type));
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "<%sNoData/>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

            ctx.need_prefix = TRUE;
            h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

            /* Render the element */
            h5tools_str_reset(&buffer);
            h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
            h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
            break;
        }
    }
    else {
        /* no data written */
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sData>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level++;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sNoData/>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sData>", xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;
    }

    H5Tclose(type);
    H5Sclose(space);
    H5Pclose(dcpl);

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sDataset>", xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    h5tools_str_close(&buffer);
}

/*-------------------------------------------------------------------------
 * Function:    xml_print_enum
 *
 * Purpose:     Print the values of an HDF5 ENUM in XML.
 *              Very similar to regular DDL output.
 *
 * Return:      void
 *
 * Programmer:  REMcG
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
xml_print_enum(hid_t type)
{
    char              **name = NULL;    /*member names                   */
    unsigned char      *value = NULL;   /*value array                    */
    unsigned            nmembs;         /*number of members              */
    hid_t               super;          /*enum base integer type         */
    hid_t               native = -1;    /*native integer datatype        */
    size_t              dst_size;       /*destination value type size    */
    unsigned            i;              /*miscellaneous counters         */
    size_t              j;
    h5tools_str_t buffer;          /* string into which to render   */
    h5tools_context_t ctx;            /* print context  */
    h5tool_format_t  *outputformat = &xml_dataformat;
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

    nmembs = (unsigned)H5Tget_nmembers(type);
    super = H5Tget_super(type);

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "<%sDataType>",xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    xml_print_datatype(super,0);

    ctx.need_prefix = TRUE;
    h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

    /* Render the element */
    h5tools_str_reset(&buffer);
    h5tools_str_append(&buffer, "</%sDataType>",xmlnsprefix);
    h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

    /*
     * Determine what datatype to use for the native values.  To simplify
     * things we entertain three possibilities:
     *  1. long long -- the largest native signed integer
     *    2. unsigned long long -- the largest native unsigned integer
     *    3. raw format
     */
    if (H5Tget_size(type) <= sizeof(long long)) {
        dst_size = sizeof(long long);

        if (H5T_SGN_NONE == H5Tget_sign(type)) {
            native = H5T_NATIVE_ULLONG;
        } 
        else {
            native = H5T_NATIVE_LLONG;
        }
    } 
    else {
        dst_size = H5Tget_size(type);
    }

    /* Get the names and raw values of all members */
    name = (char **)HDcalloc((size_t)nmembs, sizeof(char *));
    value = (unsigned char *)HDcalloc((size_t)nmembs, MAX(H5Tget_size(type), dst_size));

    for (i = 0; i < nmembs; i++) {
        name[i] = H5Tget_member_name(type, i);
        H5Tget_member_value(type, i, value + i * H5Tget_size(type));
    }

    /* Convert values to native datatype */
    if (native > 0)
        H5Tconvert(super, native, (size_t)nmembs, value, NULL, H5P_DEFAULT);

    /* Sort members by increasing value */
    /*not implemented yet */

    /* Print members */
    ctx.indent_level++;
    dump_indent += COL;
    for (i = 0; i < nmembs; i++) {
        char *t_name = xml_escape_the_name(name[i]);

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sEnumElement>",xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level++;
        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "%s", t_name);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        HDfree(t_name);
        ctx.indent_level--;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sEnumElement>",xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "<%sEnumValue>",xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level++;
        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        if (native < 0) {
            h5tools_str_append(&buffer, "0x");

            for (j = 0; j < dst_size; j++)
                h5tools_str_append(&buffer, "%02x", value[i * dst_size + j]);
        } 
        else if (H5T_SGN_NONE == H5Tget_sign(native)) {
            h5tools_str_append(&buffer,"%" H5_PRINTF_LL_WIDTH "u", *((unsigned long long *)
                       ((void *) (value + i * dst_size))));
        } 
        else {
            h5tools_str_append(&buffer,"%" H5_PRINTF_LL_WIDTH "d",
                      *((long long *) ((void *) (value + i * dst_size))));
        }
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
        ctx.indent_level--;

        ctx.need_prefix = TRUE;
        h5tools_simple_prefix(rawoutstream, outputformat, &ctx, (hsize_t)0, 0);

        /* Render the element */
        h5tools_str_reset(&buffer);
        h5tools_str_append(&buffer, "</%sEnumValue>",xmlnsprefix);
        h5tools_render_element(rawoutstream, outputformat, &ctx, &buffer, &curr_pos, (size_t)outputformat->line_ncols, (hsize_t)0, (hsize_t)0);
    }
    ctx.indent_level--;
    dump_indent -= COL;

    h5tools_str_close(&buffer);

    /* Release resources */
    for (i = 0; i < nmembs; i++)
        H5free_memory(name[i]);

    HDfree(name);
    HDfree(value);
    H5Tclose(super);
}

