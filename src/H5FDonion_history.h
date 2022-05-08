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
 * Purpose:     Interface for the onion file's history
 */

#ifndef H5FDonion_history_H
#define H5FDonion_history_H

/* Number of bytes to encode fixed-size components */
#define H5FD__ONION_ENCODED_SIZE_HEADER        40
#define H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY 20

/* Flags must align exactly one per bit, up to 24 bits */
#define H5FD__ONION_HEADER_FLAG_WRITE_LOCK        0x1
#define H5FD__ONION_HEADER_FLAG_DIVERGENT_HISTORY 0x2
#define H5FD__ONION_HEADER_FLAG_PAGE_ALIGNMENT    0x4
#define H5FD__ONION_HEADER_SIGNATURE              "OHDH"
#define H5FD__ONION_HEADER_VERSION_CURR           1

#define H5FD__ONION_WHOLE_HISTORY_SIGNATURE    "OWHS"
#define H5FD__ONION_WHOLE_HISTORY_VERSION_CURR 1

/* In-memory representation of the on-store onion history file header.
 */
typedef struct H5FD_onion_history_header_t {
    uint8_t  version;
    uint32_t flags; /* At most three bytes used! */
    uint32_t page_size;
    uint64_t origin_eof; /* Size of the 'original' canonical file */
    uint64_t history_addr;
    uint64_t history_size;
    uint32_t checksum;
} H5FD_onion_history_header_t;

/* In-memory representation of the on-store revision record.
 * Used in the history.
 */
typedef struct H5FD_onion_record_loc_t {
    haddr_t  phys_addr;
    size_t   record_size;
    uint32_t checksum;
} H5FD_onion_record_loc_t;

/* In-memory representation of the on-store history record/summary.
 */
typedef struct H5FD_onion_history_t {
    uint8_t                  version;
    uint64_t                 n_revisions;
    H5FD_onion_record_loc_t *record_locs;
    uint32_t                 checksum;
} H5FD_onion_history_t;

#ifdef __cplusplus
extern "C" {
#endif
H5_DLL herr_t H5FD__onion_ingest_history_header(H5FD_onion_history_header_t *hdr_out, H5FD_t *raw_file,
                                                haddr_t addr);
H5_DLL herr_t H5FD__onion_ingest_history(H5FD_onion_history_t *history_out, H5FD_t *raw_file, haddr_t addr,
                                         haddr_t size);

H5_DLL herr_t   H5FD__onion_write_header(H5FD_onion_history_header_t *header, H5FD_t *backing_file);
H5_DLL uint64_t H5FD__onion_write_history(H5FD_onion_history_t *history, H5FD_t *backing_file,
                                          haddr_t off_start, haddr_t filesize_curr);

H5_DLL uint64_t H5FD__onion_history_header_decode(unsigned char *, H5FD_onion_history_header_t *);
H5_DLL uint64_t H5FD__onion_history_header_encode(H5FD_onion_history_header_t *, unsigned char *, uint32_t *);

H5_DLL uint64_t H5FD__onion_history_decode(unsigned char *, H5FD_onion_history_t *);
H5_DLL uint64_t H5FD__onion_history_encode(H5FD_onion_history_t *, unsigned char *, uint32_t *);

#ifdef __cplusplus
}
#endif

#endif /* H5FDonion_history_H */
