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

#define H5FD_ONION       (H5FDperform_init(H5FD_onion_init))
#define H5FD_ONION_VALUE H5_VFD_ONION

/* Current version of the fapl info struct */
#define H5FD_ONION_FAPL_INFO_VERSION_CURR 1

/* Flag to open a file that has a locked header (after crashes, for example) */
#define H5FD_ONION_FAPL_INFO_FLAG_FORCE_OPEN 1

/* Flag to enable opening older revisions in write mode, creating a tree */
#define H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_DIVERGENT_HISTORY 0x1

/* Flag to require page alignment of onion revision data */
#define H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT 0x2

/* Max length of a comment
 * The buffer is defined to be this size + 1 to handle the NUL
 */
#define H5FD_ONION_FAPL_INFO_COMMENT_MAX_LEN 255

/* Indicates that you want the latest revision */
#define H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST UINT64_MAX

typedef enum H5FD_onion_target_file_constant_t {
    H5FD_ONION_STORE_TARGET_H5,    /* Onion history as part of HDF5 file */
    H5FD_ONION_STORE_TARGET_ONION, /* Separate, single "onion" file */
} H5FD_onion_target_file_constant_t;

/*-----------------------------------------------------------------------------
 * Structure    H5FD_onion_fapl_info_t
 *
 * Purpose:     Encapsulate info for the Onion driver FAPL entry.
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
 *              must equal the existing page size or zero. If creating a new
 *              file or an initial revision of an existing file, must be a
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
 * revision_num: Which revision to open. Must be 0 (the original file) or the
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
    uint8_t                           version;
    hid_t                             backing_fapl_id;
    uint32_t                          page_size;
    H5FD_onion_target_file_constant_t store_target;
    uint64_t                          revision_num;
    uint8_t                           force_write_open;
    uint8_t                           creation_flags;
    char                              comment[H5FD_ONION_FAPL_INFO_COMMENT_MAX_LEN + 1];
} H5FD_onion_fapl_info_t;

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5FD_onion_init(void);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5P
 *
 * \brief get the onion info from the file access property list
 *
 * \param[in] fapl_id The ID of the file access property list
 * \param[out] fa_out The pointer to the structure H5FD_onion_fapl_info_t
 *
 * \return \herr_t
 *
 * \details H5Pget_fapl_onion() retrieves the structure H5FD_onion_fapl_info_t
 *          from the file access property list that is set for the onion VFD
 *          driver.
 */
H5_DLL herr_t H5Pget_fapl_onion(hid_t fapl_id, H5FD_onion_fapl_info_t *fa_out);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5P
 *
 * \brief set the onion info for the file access property list
 *
 * \param[in] fapl_id The ID of the file access property list
 * \param[in] fa The pointer to the structure H5FD_onion_fapl_info_t
 *
 * \return \herr_t
 *
 * \details H5Pset_fapl_onion() sets the structure H5FD_onion_fapl_info_t
 *          for the file access property list that is set for the onion VFD
 *          driver.
 */
H5_DLL herr_t H5Pset_fapl_onion(hid_t fapl_id, const H5FD_onion_fapl_info_t *fa);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5FD
 *
 * \brief get the number of revisions
 *
 * \param[in] filename The name of the onion file
 * \param[in] fapl_id The ID of the file access property list
 * \param[out] revision_count The number of revisions
 *
 * \return \herr_t
 *
 * \details H5FDonion_get_revision_count() returns the number of revisions
 *          for an onion file. It takes the file name and file access property
 *          list that is set for the onion VFD driver.
 *
 */
H5_DLL herr_t H5FDonion_get_revision_count(const char *filename, hid_t fapl_id, uint64_t *revision_count);

#ifdef __cplusplus
}
#endif

#endif /* H5FDonion_H */
