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
#ifndef H5DUMP_XML_H__
#define H5DUMP_XML_H__

extern const char       *xmlnsprefix;

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

#ifdef __cplusplus
extern "C" {
#endif

/* The dump functions of the dump_function_table */
/* XML format:   same interface, alternative output */

void      xml_dump_group(hid_t, const char *);
void      xml_dump_named_datatype(hid_t, const char *);
void      xml_dump_dataset(hid_t, const char *, struct subset_t *);
void      xml_dump_dataspace(hid_t space);
void      xml_dump_datatype(hid_t type);
herr_t    xml_dump_attr(hid_t, const char *, const H5A_info_t *, void *);
void      xml_dump_data(hid_t, int, struct subset_t *, int);

#ifdef __cplusplus
}
#endif

#endif  /* !H5DUMP_XML_H__ */
