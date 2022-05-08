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

/* Flags for the fapl's creation_flags field */ 
#define H5FD_ONION_ENABLE_PAGE_ALIGNMENT 0x1 /**< Require page alignment of onion file data */

/* Max length of a comment */
#define H5FD_ONION_FAPL_INFO_COMMENT_MAX_LEN 255

/* Indicates that you want the latest revision */
#define H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST UINT64_MAX

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
 * revision_num: Which revision to open. Must be 0 (the original file) or the
 *              revision number of an existing revision.
 *              Revision H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST is reserved
 *              to open the most recently-created revision in history.
 *
 * force_write_open:
 *              TODO: NOT IMPLEMENTED
 *              Flag to ignore the write-lock flag in the onion data
 *              and attempt to open the file write-only anyway.
 *              This may be relevant if, for example, the library crashed
 *              while the file was open in write mode and the write-lock
 *              flag was not cleared.
 *
 * creation_flags:
 *              Flags used only when instantiating an onion file.
 *              If the relevant bit is set to 1, its feature will be enabled.
 *
 *              + H5FD_ONION_FAPL_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT (1, bit 1)
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
    unsigned                          version;
    hid_t                             backing_fapl_id;
    uint32_t                          page_size;
    uint64_t                          revision_num;
    hbool_t                           force_write_open;
    uint64_t                          creation_flags;
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
H5_DLL herr_t H5FDonion_get_revision_count(const char *filename, hid_t fapl_id, size_t *revision_count);

#ifdef __cplusplus
}
#endif

#endif /* H5FDonion_H */
