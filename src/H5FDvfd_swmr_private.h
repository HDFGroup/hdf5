/*
 * Copyright (c) 2019 The HDF Group.  All rights reserved.
 *
 * This file is part of HDF5.  The full HDF5 copyright notice, including
 * terms governing use, modification, and redistribution, is contained in
 * the COPYING file, which can be found at the root of the source code
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
 * If you do not have access to either file, you may request a copy from
 * help@hdfgroup.org.
 */

#ifndef _H5FDvfd_swmr_private_H
#define _H5FDvfd_swmr_private_H

/* Temporary globals for VFD SWMR */
extern hbool_t vfd_swmr_g;
extern unsigned int vfd_swmr_api_entries_g;
extern hbool_t vfd_swmr_writer_g;
extern uint64_t tick_num_g;
extern struct timespec end_of_tick_g;

H5_DLL herr_t H5F_vfd_swmr_writer_end_of_tick(void);
H5_DLL herr_t H5F_vfd_swmr_reader_end_of_tick(void);

#endif /* _H5FDvfd_swmr_private_H */
