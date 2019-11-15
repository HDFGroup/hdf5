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
extern struct H5F_t *vfd_swmr_file_g;
extern hbool_t vfd_swmr_g;
extern unsigned int vfd_swmr_api_entries_g;
extern hbool_t vfd_swmr_writer_g;
extern uint64_t tick_num_g;
extern struct timespec end_of_tick_g;

/* Forward declaration */
struct H5F_t;
struct H5FD_vfd_swmr_idx_entry_t;

/***************************************/
/* Library-private Function Prototypes */
/***************************************/

H5_DLL herr_t H5F_vfd_swmr_init(struct H5F_t *f, hbool_t file_create);
H5_DLL herr_t H5F_vfd_swmr_close_or_flush(struct H5F_t *f, hbool_t closing);
H5_DLL herr_t H5F_update_vfd_swmr_metadata_file(struct H5F_t *f, uint32_t index_len, 
    struct H5FD_vfd_swmr_idx_entry_t *index);
H5_DLL herr_t H5F_vfd_swmr_writer__delay_write(struct H5F_t *f, uint64_t page,
    uint64_t *delay_write_until_ptr);
H5_DLL herr_t H5F_vfd_swmr_writer__prep_for_flush_or_close(struct H5F_t *f);
H5_DLL herr_t H5F_vfd_swmr_writer_end_of_tick(void);
H5_DLL herr_t H5F_vfd_swmr_reader_end_of_tick(void);

#endif /* _H5FDvfd_swmr_private_H */
