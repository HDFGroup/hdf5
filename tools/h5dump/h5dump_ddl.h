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

#ifndef H5DUMP_DDL_H__
#define H5DUMP_DDL_H__

#ifdef __cplusplus
extern "C" {
#endif

/* The dump functions of the dump_function_table */
/* standard format:  no change */
void      dump_group(hid_t, const char *);
void      dump_named_datatype(hid_t, const char *);
void      dump_dataset(hid_t, const char *, struct subset_t *);
void      dump_dataspace(hid_t space);
void      dump_datatype(hid_t type);
void      dump_data(hid_t, int, struct subset_t *, int);
void      dump_fcpl(hid_t fid);
void      dump_fcontents(hid_t fid);

/* callback function used by H5Aiterate2() */
herr_t    dump_attr_cb(hid_t loc_id, const char *attr_name, const H5A_info_t *info, void *_op_data);

void handle_paths(hid_t fid, const char *path_name, void *data, int pe, const char *display_name);
void handle_datasets(hid_t fid, const char *dset, void *data, int pe, const char *display_name);
void handle_attributes(hid_t fid, const char *attr, void UNUSED * data, int UNUSED pe, const char UNUSED *display_name);
void handle_groups(hid_t fid, const char *group, void UNUSED *data, int pe, const char *display_name);
void handle_links(hid_t fid, const char *links, void UNUSED * data, int UNUSED pe, const char UNUSED *display_name);
void handle_datatypes(hid_t fid, const char *type, void UNUSED * data, int pe, const char *display_name);

#ifdef __cplusplus
}
#endif

#endif  /* !H5DUMP_DDL_H__ */
