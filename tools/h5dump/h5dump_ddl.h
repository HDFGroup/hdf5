/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
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

/* other iteration functions */
void link_iteration(hid_t gid, unsigned crt_order_flags);
void attr_iteration(hid_t gid, unsigned attr_crt_order_flags);

void handle_paths(hid_t fid, const char *path_name, void *data, int pe, const char *display_name);
void handle_datasets(hid_t fid, const char *dset, void *data, int pe, const char *display_name);
void handle_attributes(hid_t fid, const char *attr, void H5_ATTR_UNUSED * data, int H5_ATTR_UNUSED pe, const char H5_ATTR_UNUSED *display_name);
void handle_groups(hid_t fid, const char *group, void H5_ATTR_UNUSED *data, int pe, const char *display_name);
void handle_links(hid_t fid, const char *links, void H5_ATTR_UNUSED * data, int H5_ATTR_UNUSED pe, const char H5_ATTR_UNUSED *display_name);
void handle_datatypes(hid_t fid, const char *type, void H5_ATTR_UNUSED * data, int pe, const char *display_name);

#ifdef __cplusplus
}
#endif

#endif  /* !H5DUMP_DDL_H__ */
