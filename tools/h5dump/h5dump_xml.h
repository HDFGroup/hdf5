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
