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

/* Maximum RSA signature size in bytes.
 * A 4096-bit RSA key produces a 512-byte signature; 1024 bytes allows
 * headroom for 8192-bit keys.  Used by both the signer and verifier. */
#define H5PL_MAX_SIGNATURE_SIZE 1024

/* Signature footer structure
 *
 * On-disk layout (12 bytes, little-endian):
 *   [sig_len: 4][algo_id: 1][format_ver: 1][reserved: 2][magic: 4]
 *
 * Note: Always decode from byte buffer using little-endian byte order.
 *       Never read directly into this struct due to endianness portability
 *       (the on-disk format is always little-endian, but host byte order varies).
 */
typedef struct H5PL_sig_footer_t {
    uint32_t signature_length; /* Length of RSA signature in bytes */
    uint8_t  algorithm_id;     /* Hash algorithm identifier */
    uint8_t  format_version;   /* Footer format version */
    uint16_t reserved;         /* Reserved for future use */
    uint32_t magic;            /* Magic number H5PL_SIG_MAGIC */
} H5PL_sig_footer_t;

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

/*
 * KeyStore Configuration
 *
 * Key loading priority:
 *   1. Environment variable: HDF5_PLUGIN_KEYSTORE
 *   2. CMake-configured directory: HDF5_PLUGIN_KEYSTORE_DIR
 */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */

#endif /* H5PLsig_H */
