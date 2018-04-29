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

/* Programmer:  John Mainzer
 *              9/4/15
 *
 *              This file contains declarations of all functions defined
 *		in genall5.c
 */

void create_zoo(hid_t fid, const char *base_path, int proc_num);
void validate_zoo(hid_t fid, const char *base_path, int proc_num);

void ns_grp_0(hid_t fid, const char *group_name);
void vrfy_ns_grp_0(hid_t fid, const char *group_name);

void ns_grp_c(hid_t fid, const char *group_name, unsigned nlinks);
void vrfy_ns_grp_c(hid_t fid, const char *group_name, unsigned nlinks);

void ns_grp_d(hid_t fid, const char *group_name, unsigned nlinks);
void vrfy_ns_grp_d(hid_t fid, const char *group_name, unsigned nlinks);

void os_grp_0(hid_t fid, const char *group_name);
void vrfy_os_grp_0(hid_t fid, const char *group_name);

void os_grp_n(hid_t fid, const char *group_name, int proc_num, unsigned nlinks);
void vrfy_os_grp_n(hid_t fid, const char *group_name, int proc_num, 
    unsigned nlinks);

void ds_ctg_i(hid_t fid, const char *dset_name, hbool_t write_data);
void vrfy_ds_ctg_i(hid_t fid, const char *dset_name, hbool_t write_data);

void ds_chk_i(hid_t fid, const char *dset_name, hbool_t write_data);
void vrfy_ds_chk_i(hid_t fid, const char *dset_name, hbool_t write_data);

void ds_cpt_i(hid_t fid, const char *dset_name, hbool_t write_data);
void vrfy_ds_cpt_i(hid_t fid, const char *dset_name, hbool_t write_data);

void ds_ctg_v(hid_t fid, const char *dset_name, hbool_t write_data);
void vrfy_ds_ctg_v(hid_t fid, const char *dset_name, hbool_t write_data);

