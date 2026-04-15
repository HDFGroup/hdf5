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

/* 8-byte magic to identify HDF5 signed plugins.
 * Modelled on the HDF5 file signature ("\211HDF\r\n\032\n") but distinct.
 * Contains non-ASCII bytes to detect transport corruption and reduce
 * the chance of a false positive in arbitrary binary data. */
#define H5PL_SIG_MAGIC_LEN 8
static const uint8_t H5PL_SIG_MAGIC[H5PL_SIG_MAGIC_LEN] = {0x89, 'H', 'P', 'S', '\r', '\n', 0x1A, '\n'};

/* Current signature format version.
 * If future versions change the footer layout, the decoder should be
 * updated to accept older versions so that already-signed plugins
 * remain loadable without re-signing. */
#define H5PL_SIG_FORMAT_VERSION_CURRENT 1

/* Hash Algorithm Identifiers (on-disk values, stored as uint8_t) */
typedef enum {
    H5PL_SIG_ALGO_SHA256     = 0x01, /* SHA-256 with RSA-PKCS1 */
    H5PL_SIG_ALGO_SHA384     = 0x02, /* SHA-384 with RSA-PKCS1 */
    H5PL_SIG_ALGO_SHA512     = 0x03, /* SHA-512 with RSA-PKCS1 (default) */
    H5PL_SIG_ALGO_SHA256_PSS = 0x11, /* SHA-256 with RSA-PSS */
    H5PL_SIG_ALGO_SHA384_PSS = 0x12, /* SHA-384 with RSA-PSS */
    H5PL_SIG_ALGO_SHA512_PSS = 0x13, /* SHA-512 with RSA-PSS */
    H5PL_SIG_ALGO_SHA3_256   = 0x20, /* SHA3-256 (future) */
    H5PL_SIG_ALGO_BLAKE3     = 0x30  /* BLAKE3 (future) */
} H5PL_sig_algo_t;

/* Signature footer on-disk size (14 bytes) */
#define H5PL_SIG_FOOTER_SIZE 14

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
 * On-disk layout (14 bytes, little-endian where applicable):
 *   [algo_id: 1][sig_len: 4][magic: 8][format_ver: 1]
 *    byte 0      bytes 1-4   bytes 5-12  byte 13
 *
 * Magic (8 bytes) and version (1 byte) are placed at the end so they
 * reside at a fixed offset from EOF regardless of any future footer
 * growth.  This lets any library version locate the magic, check the
 * version, and give a meaningful error (e.g. "unsupported version")
 * rather than "not signed".
 *
 * During decoding, magic is still verified *first* — before any other
 * field is interpreted — to avoid parsing untrusted data from an
 * unsigned file.
 *
 * Always decode from byte buffer using little-endian byte order.
 * Never read directly into this struct due to endianness portability
 * (the on-disk format is always little-endian, but host byte order
 * varies).
 */
typedef struct H5PL_sig_footer_t {
    uint32_t        signature_length; /* Length of RSA signature in bytes */
    H5PL_sig_algo_t algorithm_id;     /* Hash algorithm identifier */
    uint8_t         format_version;   /* Footer format version */
} H5PL_sig_footer_t;

/*-------------------------------------------------------------------------
 * Function:    H5PL_sig_encode_footer
 *
 * Purpose:     Encode a signature footer struct into a little-endian buffer
 *              suitable for appending to a signed plugin file.
 *
 * Note:        Requires H5encode.h for UINT32ENCODE.
 *              buf_size must be >= H5PL_SIG_FOOTER_SIZE (14).
 *-------------------------------------------------------------------------
 */
static inline void
H5PL_sig_encode_footer(uint8_t *buf, size_t buf_size, const H5PL_sig_footer_t *footer)
{
    uint8_t *p = buf;

    assert(buf_size >= H5PL_SIG_FOOTER_SIZE);
    (void)buf_size; /* used only by assert */

    *p++ = (uint8_t)footer->algorithm_id;          /* byte  0      */
    UINT32ENCODE(p, footer->signature_length);     /* bytes 1-4    */
    memcpy(p, H5PL_SIG_MAGIC, H5PL_SIG_MAGIC_LEN); /* bytes 5-12 */
    p += H5PL_SIG_MAGIC_LEN;
    *p++ = footer->format_version; /* byte  13     */
} /* end H5PL_sig_encode_footer() */

/*-------------------------------------------------------------------------
 * Function:    H5PL_sig_decode_footer
 *
 * Purpose:     Decode a little-endian buffer into a footer struct and
 *              perform minimal validation (magic and format version).
 *
 * Return:      true  — footer decoded and valid
 *              false — magic mismatch or unsupported format version
 *
 * Note:        Requires H5encode.h for UINT32DECODE.
 *              buf_size must be >= H5PL_SIG_FOOTER_SIZE (14).
 *
 *              On-disk order is [algo_id][sig_len][magic][version], but
 *              magic is decoded and verified first (at offset 5) to avoid
 *              interpreting untrusted fields from an unsigned file.
 *-------------------------------------------------------------------------
 */
static inline bool
H5PL_sig_decode_footer(const uint8_t *buf, size_t buf_size, H5PL_sig_footer_t *footer)
{
    const uint8_t *p;

    if (buf_size < H5PL_SIG_FOOTER_SIZE)
        return false;

    /* Decode and verify magic first (at offset 5) */
    if (memcmp(buf + 5, H5PL_SIG_MAGIC, H5PL_SIG_MAGIC_LEN) != 0)
        return false;

    /* Magic valid — now decode remaining fields from the beginning */
    p                    = buf;
    footer->algorithm_id = (H5PL_sig_algo_t)*p++; /* byte  0      */
    UINT32DECODE(p, footer->signature_length);    /* bytes 1-4    */
    /* skip magic (already verified above) */
    p += H5PL_SIG_MAGIC_LEN;       /* bytes 5-12   */
    footer->format_version = *p++; /* byte  13     */

    /* Verify format version.
     * Currently only version 1 exists.  When a new version is introduced,
     * add backward-compatible decoding here (e.g. accept versions 1..N)
     * so that plugins signed with an older format remain loadable. */
    if (footer->format_version < 1 || footer->format_version > H5PL_SIG_FORMAT_VERSION_CURRENT)
        return false;

    return true;
} /* end H5PL_sig_decode_footer() */

#endif /* H5PLsig_H */
