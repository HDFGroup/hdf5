/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Onion Virtual File Driver (VFD)
 *
 * Purpose:    The public header file for the Onion VFD.
 */
#ifndef H5FDonion_H
#define H5FDonion_H

#define H5FD_ONION       (H5FD_onion_init())

#define H5FD_ONION_ENABLE_INDEX_STATS 0

#define H5FD_ONION_FAPL_INFO_MAGIC 0x10101010 /* TODO */
#define H5FD_ONION_FAPL_INFO_VERSION_CURR 1
#define H5FD_ONION_FAPL_INFO_FLAG_FORCE_OPEN 1
#define H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_DIVERGENT_HISTORY 1
#define H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT 2
#define H5FD_ONION_FAPL_INFO_COMMENT_MAX_LEN 255
#define H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST (uint64_t)(-1)

enum H5FD_onion_target_file_constant {
    H5FD_ONION_STORE_TARGET_H5, /* onion history as part of H5 file */
    H5FD_ONION_STORE_TARGET_ONION, /* separate, single "onion" file */
   /* TODO: other storage location/scheme? */
};

/*-----------------------------------------------------------------------------
 * Structure    H5FD_onion_fapl_info_t
 *
 * Purpose:     Encapsulate info for the Onion driver FAPL entry.
 *
 * magic:       "Magic number" identifying struct.
 *              Must equal H5FD_ONION_FAPL_MAGIC to be considered valid.
 *
 * version:     Future-proofing identifier. Informs struct membership.
 *              Must equal H5FD_ONION_FAPL_VERSION_CURR to be considered valid.
 *
 * backing_fapl_id:
 *              Backing or 'child' FAPL ID to handle I/O with the
 *              underlying backing store. If the onion data is stored as a
 *              separate file, it must use the same backing driver as the
 *              original file.
 *
 * page_size:   Size of the amended data pages. If opening an existing file,
 *              must equal the existing page size or zero (0). If creating a
 *              new file or an initial revision of an existing file, must be a
 *              power of 2.
 *
 * store_target:
 *              Enumerated/defined value identifying where the history data is
 *              stored, either in the same file (appended to HDF5 data) or a
 *              separate file. Other options may be added in later versions.
 *
 *              + H5FD_ONION_FAPL_STORE_MODE_SEPARATE_SINGLE (1)
 *                      Onion history is stored in a single, separate "onion
 *                      file". Shares filename and path as hdf5 file (if any),
 *                      with only a different filename extension.
 *
 * revision_id: Which revision to open. Must be 0 (the original file) or the
 *              revision number of an existing revision.
 *              Revision ID -1 is reserved to open the most recently-created
 *              revision in history.
 *
 * force_write_open:
 *              Flag to ignore the write-lock flag in the onion data
 *              and attempt to open the file write-only anyway.
 *              This may be relevant if, for example, the library crashed
 *              while the file was open in write mode and the write-lock
 *              flag was not cleared.
 *              Must equal H5FD_ONION_FAPL_FLAG_FORCE_OPEN to enable.
 *
 * creation_flags:
 *              Flag used only when instantiating an Onion file.
 *              If the relevant bit is set to a nonzero value, its feature
 *              will be enabled.
 *
 *              + H5FD_ONION_FAPL_CREATE_FLAG_ENABLE_DIVERGENT_HISTORY
 *                (1, bit 1)
 *                      User will be allowed to open arbitrary revisions
 *                      in write mode.
 *                      If disabled (0), only the most recent revision may be
 *                      opened for amendment.
 *
 *              + H5FD_ONION_FAPL_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT (2, bit 2)
 *                      Onion history metadata will align to page_size.
 *                      Partial pages of unused space will occur in the file,
 *                      but may improve read performance from the backing store
 *                      on some systems.
 *                      If disabled (0), padding will not be inserted to align
 *                      to page boundaries.
 *
 *              + <Remaining bits reserved>
 *
 * comment:     User-supplied NULL-terminated comment for a revision to be
 *              written.
 *              Cannot be longer than H5FD_ONION_FAPL_COMMENT_MAX_LEN.
 *              Ignored if part of a FAPL used to open in read mode.
 *
 *              The comment for a revision may be modified prior to committing
 *              to the revision (closing the file and writing the record)
 *              with a call to H5FDfctl().
 *              This H5FDfctl overwrite may be used to exceed constraints of
 *              maximum string length and the NULL-terminator requirement.
 *
 *-----------------------------------------------------------------------------
 */
typedef struct H5FD_onion_fapl_info_t {
    uint32_t magic;
    uint8_t  version;
    hid_t    backing_fapl_id;
    uint32_t page_size;
    enum H5FD_onion_target_file_constant store_target;
    uint64_t revision_id;
    uint8_t  force_write_open;
    uint8_t  creation_flags;
    char     comment[H5FD_ONION_FAPL_INFO_COMMENT_MAX_LEN + 1];
} H5FD_onion_fapl_info_t;

#ifdef __cplusplus
extern "C" {
#endif

/*
 * PUBLIC PROTOTYPES
 */

H5_DLL hid_t H5FD_onion_init(void);
H5_DLL herr_t H5Pget_fapl_onion(hid_t fapl_id, H5FD_onion_fapl_info_t *fa_out);
H5_DLL herr_t H5Pset_fapl_onion(hid_t fapl_id,
        const H5FD_onion_fapl_info_t *fa);

#ifdef __cplusplus
}
#endif

#endif /* H5FDonion_H */

