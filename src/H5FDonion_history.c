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
 * Purpose:     Code for the onion file's history
 */

/* This source code file is part of the H5FD driver module */
#include "H5FDdrvr_module.h"

#include "H5private.h"      /* Generic Functions           */
#include "H5Eprivate.h"     /* Error handling              */
#include "H5FDprivate.h"    /* File drivers                */
#include "H5FDonion.h"      /* Onion file driver           */
#include "H5FDonion_priv.h" /* Onion file driver internals */

/*-----------------------------------------------------------------------------
 * Function:    H5FD_onion_history_header_decode
 *
 * Purpose:     Attempt to read a buffer and store it as a history-header
 *              structure.
 *
 *              Implementation must correspond with
 *              H5FD_onion_history_header_encode().
 *
 * Return:      Success:    Number of bytes read from buffer
 *              Failure:    0
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_history_header_decode(unsigned char *buf, H5FD_onion_history_header_t *header)
{
    uint32_t       ui32      = 0;
    uint32_t       sum       = 0;
    uint64_t       ui64      = 0;
    uint8_t *      ui8p      = NULL;
    unsigned char *ptr       = NULL;
    uint64_t       ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT;

    HDassert(buf != NULL);
    HDassert(header != NULL);
    HDassert(H5FD__ONION_HEADER_VERSION_CURR == header->version);

    if (HDstrncmp((const char *)buf, H5FD__ONION_HEADER_SIGNATURE, 4))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid header signature")

    if (buf[4] != H5FD__ONION_HEADER_VERSION_CURR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid header version")

    ptr  = buf + 5;
    ui32 = 0;
    HDmemcpy(&ui32, ptr, 3);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, header->flags);
    ptr += 3;

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, header->page_size);
    ptr += 4;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT32DECODE(ui8p, header->origin_eof);
    ptr += 8;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT32DECODE(ui8p, header->history_addr);
    ptr += 8;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT32DECODE(ui8p, header->history_size);
    ptr += 8;

    sum = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, header->checksum);
    ptr += 4;

    if (sum != header->checksum)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "checksum mismatch")

    ret_value = (uint64_t)(ptr - buf);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_history_header_decode() */

/*-----------------------------------------------------------------------------
 * Function:    H5FD_onion_history_header_encode
 *
 * Purpose:     Write history-header structure to the given buffer.
 *              All multi-byte elements are stored in little-endian word order.
 *
 *              Implementation must correspond with
 *              H5FD_onion_history_header_decode().
 *
 *              The destination buffer must be sufficiently large to hold the
 *              encoded contents (H5FD__ONION_ENCODED_SIZE_HEADER).
 *
 * Return:      Number of bytes written to buffer.
 *              The checksum of the generated buffer contents (excluding the
 *              checksum itself) is stored in the pointer `sum_out`).
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_history_header_encode(H5FD_onion_history_header_t *header, unsigned char *buf, uint32_t *sum_out)
{
    unsigned char *ptr       = buf;
    uint64_t       ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(buf != NULL);
    HDassert(sum_out != NULL);
    HDassert(header != NULL);
    HDassert(H5FD__ONION_HEADER_VERSION_CURR == header->version);
    HDassert(0 == (header->flags & 0xFF000000)); /* max three bits long */

    HDmemcpy(ptr, H5FD__ONION_HEADER_SIGNATURE, 4);
    ptr += 4;
    HDmemcpy(ptr, (unsigned char *)&header->version, 1);
    ptr += 1;
    UINT32ENCODE(ptr, header->flags);
    ptr -= 1; /* truncate to three bytes */
    UINT32ENCODE(ptr, header->page_size);
    UINT64ENCODE(ptr, header->origin_eof);
    UINT64ENCODE(ptr, header->history_addr);
    UINT64ENCODE(ptr, header->history_size);
    *sum_out = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));
    UINT32ENCODE(ptr, *sum_out);
    ret_value = (uint64_t)(ptr - buf);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_history_header_encode() */

/*-----------------------------------------------------------------------------
 * Function:    H5FD_onion_history_decode
 *
 * Purpose:     Attempt to read a buffer and store it as a history
 *              structure.
 *
 *              Implementation must correspond with
 *              H5FD_onion_history_encode().
 *
 *              MUST BE CALLED TWICE:
 *              On the first call, n_records in the destination structure must
 *              be zero, and record_pointer_list be NULL.
 *
 *              If the buffer is well-formed, the destination structure is
 *              tentatively populated with fixed-size values, and the number of
 *              bytes read are returned.
 *
 *              Prior to the second call, the user must allocate space for
 *              record_pointer_list to hold n_records record-pointer structs.
 *
 *              Then the decode operation is called a second time, and all
 *              components will be populated (and again number of bytes read is
 *              returned).
 *
 * Return:      Success:    Number of bytes read from buffer
 *              Failure:    0
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_history_decode(unsigned char *buf, H5FD_onion_history_t *history)
{
    uint32_t       ui32        = 0;
    uint32_t       sum         = 0;
    uint64_t       ui64        = 0;
    uint64_t       n_revisions = 0;
    uint8_t *      ui8p        = NULL;
    unsigned char *ptr         = NULL;
    uint64_t       ret_value   = 0;

    FUNC_ENTER_NOAPI_NOINIT;

    HDassert(buf != NULL);
    HDassert(history != NULL);
    HDassert(H5FD__ONION_WHOLE_HISTORY_VERSION_CURR == history->version);

    if (HDstrncmp((const char *)buf, "OWHS", 4))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid signature")

    if (H5FD__ONION_WHOLE_HISTORY_VERSION_CURR != buf[4])
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid version")

    ptr = buf + 8;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT64DECODE(ui8p, n_revisions);
    ptr += 8;

    if (0 == history->n_revisions) {
        history->n_revisions = n_revisions;
        ptr += H5FD__ONION_ENCODED_SIZE_RECORD_POINTER * n_revisions;
    }
    else {
        uint64_t i = 0;

        if (history->n_revisions != n_revisions)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0,
                        "history argument suggests different revision count than encoded buffer")
        if (NULL == history->record_pointer_list)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "list is NULL -- cannot populate")

        for (i = 0; i < n_revisions; i++) {
            H5FD_onion_record_pointer_t *rpp = &history->record_pointer_list[i];

            HDmemcpy(&ui64, ptr, 8);
            ui8p = (uint8_t *)&ui64;
            UINT64DECODE(ui8p, rpp->phys_addr);
            ptr += 8;

            HDmemcpy(&ui64, ptr, 8);
            ui8p = (uint8_t *)&ui64;
            UINT64DECODE(ui8p, rpp->record_size);
            ptr += 8;

            HDmemcpy(&ui32, ptr, 4);
            ui8p = (uint8_t *)&ui32;
            UINT64DECODE(ui8p, rpp->checksum);
            ptr += 4;
        }
    }

    sum = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, history->checksum);
    ptr += 4;

    if (sum != history->checksum)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "checksum mismatch")

    ret_value = (uint64_t)(ptr - buf);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_history_decode() */

/*-----------------------------------------------------------------------------
 * Function:    H5FD_onion_history_encode
 *
 * Purpose:     Write history structure to the given buffer.
 *              All multi-byte elements are stored in little-endian word order.
 *
 *              Implementation must correspond with
 *              H5FD_onion_history_decode().
 *
 *              The destination buffer must be sufficiently large to hold the
 *              encoded contents.
 *              (Hint: `sizeof(history struct) +
 *              sizeof(record-pointer-struct) * n_records)` guarantees
 *              ample/excess space.)
 *
 * Return:      Number of bytes written to buffer.
 *              The checksum of the generated buffer contents (excluding the
 *              checksum itself) is stored in the pointer `sum_out`).
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_history_encode(H5FD_onion_history_t *history, unsigned char *buf, uint32_t *sum_out)
{
    unsigned char *ptr      = buf;
    uint32_t       vers_u32 = (uint32_t)history->version; /* pad out unused bytes */

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(history != NULL);
    HDassert(H5FD__ONION_WHOLE_HISTORY_VERSION_CURR == history->version);
    HDassert(buf != NULL);
    HDassert(sum_out != NULL);

    HDmemcpy(ptr, H5FD__ONION_WHOLE_HISTORY_SIGNATURE, 4);
    ptr += 4;
    UINT32ENCODE(ptr, vers_u32);
    UINT64ENCODE(ptr, history->n_revisions);
    if (history->n_revisions > 0) {
        uint64_t i = 0;

        HDassert(history->record_pointer_list != NULL); /* TODO: error? */
        for (i = 0; i < history->n_revisions; i++) {
            UINT64ENCODE(ptr, history->record_pointer_list[i].phys_addr);
            UINT64ENCODE(ptr, history->record_pointer_list[i].record_size);
            UINT32ENCODE(ptr, history->record_pointer_list[i].checksum);
        }
    }
    *sum_out = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));
    UINT32ENCODE(ptr, *sum_out);

    FUNC_LEAVE_NOAPI((uint64_t)(ptr - buf));
} /* end H5FD_onion_history_encode() */


