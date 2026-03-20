/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5PLsig_H
#define H5PLsig_H

/*
 * Appended Signature Format
 * =========================
 *
 * Plugin files use an appended signature format:
 *
 *   [ Binary Data (ELF/DLL/Mach-O) ] [ RSA Signature ] [ Footer ]
 *
 * The footer contains metadata about the signature and a magic number
 * to identify signed plugins. The binary loader ignores trailing data,
 * so the plugin loads normally.
 *
 * This approach:
 *   - Works on all platforms (Linux, Windows, macOS)
 *   - No ELF parsing required
 *   - No external tools needed (objcopy, etc.)
 *   - Simple append operation for signing
 *   - Simple read-from-end for verification
 */

/* Magic number to identify HDF5 signed plugins */
#define H5PL_SIG_MAGIC 0x48444635 /* "HDF5" in hex */

/* Current signature format version */
#define H5PL_SIG_FORMAT_VERSION_CURRENT 1

/* Hash Algorithm Identifiers */
#define H5PL_SIG_ALGO_SHA256     0x01 /* SHA-256 with RSA-PKCS1 */
#define H5PL_SIG_ALGO_SHA384     0x02 /* SHA-384 with RSA-PKCS1 */
#define H5PL_SIG_ALGO_SHA512     0x03 /* SHA-512 with RSA-PKCS1 (default) */
#define H5PL_SIG_ALGO_SHA256_PSS 0x11 /* SHA-256 with RSA-PSS */
#define H5PL_SIG_ALGO_SHA384_PSS 0x12 /* SHA-384 with RSA-PSS */
#define H5PL_SIG_ALGO_SHA512_PSS 0x13 /* SHA-512 with RSA-PSS */
#define H5PL_SIG_ALGO_SHA3_256   0x20 /* SHA3-256 (future) */
#define H5PL_SIG_ALGO_BLAKE3     0x30 /* BLAKE3 (future) */

/* Signature footer on-disk size (12 bytes) */
#define H5PL_SIG_FOOTER_SIZE 12

/* True when algo id selects an RSA-PSS padding variant */
#define H5PL_SIG_ALGO_IS_PSS(id) ((id) >= H5PL_SIG_ALGO_SHA256_PSS && (id) <= H5PL_SIG_ALGO_SHA512_PSS)

/* Maximum RSA signature size in bytes.
 * A 4096-bit RSA key produces a 512-byte signature; 1024 bytes allows
 * headroom for 8192-bit keys.  Used by both the signer and verifier. */
#define H5PL_MAX_SIGNATURE_SIZE 1024

/* Maximum plugin file size (1GB).  Shared between the library verifier
 * and the h5sign tool to keep the limit in sync. */
#define H5PL_MAX_PLUGIN_SIZE (1024LL * 1024LL * 1024LL)

/* Signature footer structure
 *
 * On-disk layout (12 bytes, little-endian):
 *   [magic: 4][sig_len: 4][algo_id: 1][format_ver: 1][reserved: 2]
 *
 * Note: Magic is encoded first so it can be verified before interpreting
 *       remaining fields.  Always decode from byte buffer using
 *       little-endian byte order.  Never read directly into this struct
 *       due to endianness portability (the on-disk format is always
 *       little-endian, but host byte order varies).
 */
typedef struct H5PL_sig_footer_t {
    uint32_t magic;            /* Magic number H5PL_SIG_MAGIC */
    uint32_t signature_length; /* Length of RSA signature in bytes */
    uint8_t  algorithm_id;     /* Hash algorithm identifier */
    uint8_t  format_version;   /* Footer format version */
    uint16_t reserved;         /* Reserved for future use */
} H5PL_sig_footer_t;

/*-------------------------------------------------------------------------
 * Function:    H5PL_sig_encode_footer
 *
 * Purpose:     Encode a signature footer struct into a 12-byte little-endian
 *              buffer suitable for appending to a signed plugin file.
 *
 * Note:        Requires H5encode.h for UINT32ENCODE / UINT16ENCODE.
 *-------------------------------------------------------------------------
 */
static H5_ATTR_UNUSED void
H5PL_sig_encode_footer(uint8_t buf[H5PL_SIG_FOOTER_SIZE], const H5PL_sig_footer_t *footer)
{
    uint8_t *p = buf;

    UINT32ENCODE(p, footer->magic);
    UINT32ENCODE(p, footer->signature_length);
    *p++ = footer->algorithm_id;
    *p++ = footer->format_version;
    UINT16ENCODE(p, footer->reserved);
} /* end H5PL_sig_encode_footer() */

/*-------------------------------------------------------------------------
 * Function:    H5PL_sig_decode_footer
 *
 * Purpose:     Decode a 12-byte little-endian buffer into a footer struct.
 *              Decodes magic first so it can be verified before interpreting
 *              remaining fields.
 *
 * Note:        Requires H5encode.h for UINT32DECODE / UINT16DECODE.
 *-------------------------------------------------------------------------
 */
static H5_ATTR_UNUSED void
H5PL_sig_decode_footer(const uint8_t buf[H5PL_SIG_FOOTER_SIZE], H5PL_sig_footer_t *footer)
{
    const uint8_t *p = buf;

    UINT32DECODE(p, footer->magic);
    UINT32DECODE(p, footer->signature_length);
    footer->algorithm_id   = *p++;
    footer->format_version = *p++;
    UINT16DECODE(p, footer->reserved);
} /* end H5PL_sig_decode_footer() */

#endif /* H5PLsig_H */
