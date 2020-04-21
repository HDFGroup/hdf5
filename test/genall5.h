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

/* 
 * This file contains declarations of all functions defined in genall5.c
 */

typedef struct _zoo_config {
    int proc_num;
    bool continue_on_failure;
    bool skip_compact;
    bool skip_varlen;
} zoo_config_t;

bool create_zoo(hid_t, const char *, zoo_config_t);
bool validate_zoo(hid_t, const char *, zoo_config_t);
bool delete_zoo(hid_t, const char *, zoo_config_t);
bool validate_deleted_zoo(hid_t, const char *, zoo_config_t);

bool ns_grp_0(hid_t fid, const char *group_name);
bool vrfy_ns_grp_0(hid_t fid, const char *group_name);

bool ns_grp_c(hid_t fid, const char *group_name, unsigned nlinks);
bool vrfy_ns_grp_c(hid_t fid, const char *group_name, unsigned nlinks);

bool ns_grp_d(hid_t fid, const char *group_name, unsigned nlinks);
bool vrfy_ns_grp_d(hid_t fid, const char *group_name, unsigned nlinks);

bool os_grp_0(hid_t fid, const char *group_name);
bool vrfy_os_grp_0(hid_t fid, const char *group_name);

bool os_grp_n(hid_t fid, const char *group_name, int proc_num, unsigned nlinks);
bool vrfy_os_grp_n(hid_t fid, const char *group_name, int proc_num, 
    unsigned nlinks);

bool ds_ctg_i(hid_t fid, const char *dset_name, hbool_t write_data);
bool vrfy_ds_ctg_i(hid_t fid, const char *dset_name, hbool_t write_data);

bool ds_chk_i(hid_t fid, const char *dset_name, hbool_t write_data);
bool vrfy_ds_chk_i(hid_t fid, const char *dset_name, hbool_t write_data);

bool ds_cpt_i(hid_t fid, const char *dset_name, hbool_t write_data);
bool vrfy_ds_cpt_i(hid_t fid, const char *dset_name, hbool_t write_data);

bool ds_ctg_v(hid_t fid, const char *dset_name, hbool_t write_data);
bool vrfy_ds_ctg_v(hid_t fid, const char *dset_name, hbool_t write_data);

/* Individual tests can override zoo_create_hook(), which is called
 * after each step of create_zoo().  The `hid_t` argument identifies
 * the file where the step was performed.  The test library provides a
 * default implementation of zoo_create_hook() that does nothing.
 */
void zoo_create_hook(hid_t);
