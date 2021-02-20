/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: Test the Mirror VFD functionality.
 */

/* WARNING: The use of realpath() is probably system-dependent, as are
 * other things here such as the socket calls.
 * Notable to realpath() in particular is the use of "PATH_MAX", which
 * apparently has some major potential issues if paths are abused.
 * http://insanecoding.blogspot.com/2007/11/pathmax-simply-isnt.html
 * so BE CAREFUL about the paths we throw around?
 */

#include "h5test.h"
#include "cache_common.h"
#include "genall5.h"

#ifdef H5_HAVE_MIRROR_VFD

#include "H5FDmirror_priv.h" /* Private header for the mirror VFD */

/* For future consideration, IP address and port number might be
 * environment variables?
 */
#define SERVER_IP_ADDRESS "127.0.0.1"

/* Primary listening port on server. */
#define SERVER_HANDSHAKE_PORT 3000

#define DATABUFFER_SIZE 128
#define DSET_NAME_LEN   16

/* Parameters for the "large chunked dataset" writing */
#define MAX_DSET_COUNT 255
#define DSET_DIM       32
#define CHUNK_DIM      8

#define CONCURRENT_COUNT 3 /* Number of files in concurrent test */

/* Macro: LOGPRINT()
 * Prints logging and debugging messages to the output stream based
 * on the level of verbosity.
 *     0 : no logging
 *     1 : errors only
 *     2 : details
 *     3 : all
 */
#define DEFAULT_VERBOSITY 1
static unsigned int g_verbosity = DEFAULT_VERBOSITY;

/* Macro for selective debug printing / logging */
#define LOGPRINT(lvl, ...)                                                                                   \
    do {                                                                                                     \
        if ((lvl) <= g_verbosity) {                                                                          \
            HDfprintf(g_log_stream, __VA_ARGS__);                                                            \
            HDfflush(g_log_stream);                                                                          \
        }                                                                                                    \
    } while (0)

#define MIRROR_RW_DIR "mirror_rw/"
#define MIRROR_WO_DIR "mirror_wo/"

/* String buffer for error messages */
#define MIRR_MESG_SIZE 128
static char mesg[MIRR_MESG_SIZE + 1];

/* Convenience structure for passing file names via helper functions.
 */
struct mirrortest_filenames {
    char rw[H5FD_SPLITTER_PATH_MAX + 1];
    char wo[H5FD_SPLITTER_PATH_MAX + 1];
    char log[H5FD_SPLITTER_PATH_MAX + 1];
};

static FILE *g_log_stream = NULL; /* initialized at runtime */

static herr_t _verify_datasets(unsigned min_dset, unsigned max_dset, hid_t *filespace_id, hid_t *dataset_id,
                               hid_t memspace_id);

static herr_t _create_chunking_ids(hid_t file_id, unsigned min_dset, unsigned max_dset, hsize_t *chunk_dims,
                                   hsize_t *dset_dims, hid_t *dataspace_ids, hid_t *filespace_ids,
                                   hid_t *dataset_ids, hid_t *memspace_id);

static herr_t _close_chunking_ids(unsigned min_dset, unsigned max_dset, hid_t *dataspace_ids,
                                  hid_t *filespace_ids, hid_t *dataset_ids, hid_t *memspace_id);

static herr_t _populate_filepath(const char *dirname, const char *_basename, hid_t fapl_id, char *path_out,
                                 hbool_t h5suffix);

static hid_t create_mirroring_split_fapl(const char *_basename, struct mirrortest_filenames *names);

static void mybzero(void *dest, size_t size);

/* ----------------------------------------------------------------------------
 * Function:   mybzero
 *
 * Purpose:    Have bzero simplicity and abstraction in (possible) absence of
 *             it being available.
 *
 * Programmer: Jacob Smith
 *             2020-03-30
 * ----------------------------------------------------------------------------
 */
static void
mybzero(void *dest, size_t size)
{
    size_t i = 0;
    char * s = NULL;
    HDassert(dest != NULL);
    s = (char *)dest;
    for (i = 0; i < size; i++) {
        *(s + i) = 0;
    }
} /* end mybzero() */

/* ----------------------------------------------------------------------------
 * Function:   _populate_filepath
 *
 * Purpose:    Given a directory name and a base name, concatenate the two and
 *             run h5fixname() to get the "actual" path to the intented target.
 *             `h5suffix' should be FALSE to keep the base name unaltered;
 *             TRUE will append the '.h5' h5suffix to the basename...
 *             FALSE -> h5fixname_no_suffix(), TRUE -> h5fixname()
 *             <h5fixname_prefix> / <dirname> / <_basename> <h5prefix?>
 *
 * Programmer: Jacob Smith
 *             2019-08-16
 * ----------------------------------------------------------------------------
 */
static herr_t
_populate_filepath(const char *dirname, const char *_basename, hid_t fapl_id, char *path_out,
                   hbool_t h5suffix)
{
    char _path[H5FD_SPLITTER_PATH_MAX];

    if ((_basename == NULL) || (*_basename == 0) || (dirname == NULL) || (*dirname == 0) ||
        (path_out == NULL)) {
        TEST_ERROR;
    }

    if (HDsnprintf(_path, H5FD_SPLITTER_PATH_MAX, "%s%s%s", dirname,
                   (dirname[HDstrlen(dirname)] == '/') ? "" : "/", /* slash iff needed */
                   _basename) > H5FD_SPLITTER_PATH_MAX) {
        TEST_ERROR;
    }

    if (h5suffix == TRUE) {
        if (h5_fixname(_path, fapl_id, path_out, H5FD_SPLITTER_PATH_MAX) == NULL) {
            TEST_ERROR;
        }
    }
    else {
        if (h5_fixname_no_suffix(_path, fapl_id, path_out, H5FD_SPLITTER_PATH_MAX) == NULL) {
            TEST_ERROR;
        }
    }

    return SUCCEED;

error:
    return FAIL;
} /* end _populate_filepath() */

/* ---------------------------------------------------------------------------
 * Function:    build_paths
 *
 * Purpose:     Convenience function to create the three file paths used in
 *              most mirror tests.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2019-08-16
 * ---------------------------------------------------------------------------
 */
static herr_t
build_paths(const char *_basename, H5FD_splitter_vfd_config_t *splitter_config,
            struct mirrortest_filenames *names)
{
    char baselogname[H5FD_SPLITTER_PATH_MAX];

    if (_populate_filepath(MIRROR_RW_DIR, _basename, splitter_config->rw_fapl_id, names->rw, TRUE) == FAIL) {
        TEST_ERROR;
    }

    if (_populate_filepath(MIRROR_WO_DIR, _basename, splitter_config->wo_fapl_id, names->wo, TRUE) == FAIL) {
        TEST_ERROR;
    }

    if (_basename == NULL || *_basename == 0)
        return FAIL;
    if (HDsnprintf(baselogname, H5FD_SPLITTER_PATH_MAX, "%s_err.log", _basename) > H5FD_SPLITTER_PATH_MAX) {
        TEST_ERROR;
    }

    if (_populate_filepath(MIRROR_WO_DIR, baselogname, splitter_config->wo_fapl_id, names->log, FALSE) ==
        FAIL) {
        TEST_ERROR;
    }

    return SUCCEED;

error:
    return FAIL;
} /* end build_paths() */

/* ---------------------------------------------------------------------------
 * Function:    test_fapl_configuration
 *
 * Purpose:     Test FAPL configuration and examination.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Jacob Smith
 *              2019-03-12
 * ---------------------------------------------------------------------------
 */
static int
test_fapl_configuration(void)
{
    hid_t              fapl_id;
    H5FD_mirror_fapl_t mirror_conf = {
        H5FD_MIRROR_FAPL_MAGIC,          /* magic */
        H5FD_MIRROR_CURR_FAPL_T_VERSION, /* version */
        SERVER_HANDSHAKE_PORT,           /* handhake_port */
        SERVER_IP_ADDRESS,               /* remote_ip "IP address" */
    };
    H5FD_mirror_fapl_t fa_out = {0, 0, 0, ""};

    TESTING("Mirror fapl configuration (set/get)");

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id) {
        TEST_ERROR;
    }

    if (H5Pset_fapl_mirror(fapl_id, &mirror_conf) == FAIL) {
        TEST_ERROR;
    }

    if (H5Pget_fapl_mirror(fapl_id, &fa_out) == FAIL) {
        TEST_ERROR;
    }
    if (H5FD_MIRROR_FAPL_MAGIC != fa_out.magic) {
        TEST_ERROR;
    }
    if (H5FD_MIRROR_CURR_FAPL_T_VERSION != fa_out.version) {
        TEST_ERROR;
    }
    if (SERVER_HANDSHAKE_PORT != fa_out.handshake_port) {
        TEST_ERROR;
    }
    if (HDstrncmp(SERVER_IP_ADDRESS, (const char *)fa_out.remote_ip, H5FD_MIRROR_MAX_IP_LEN)) {
        TEST_ERROR;
    }

    if (H5Pclose(fapl_id) == FAIL) {
        TEST_ERROR;
    }

    PASSED();
    return 0;

error:
    if (H5I_INVALID_HID != fapl_id) {
        (void)H5Pclose(fapl_id);
    }
    return -1;
} /* end test_fapl_configuration() */

#define PRINT_BUFFER_DIFF(act, exp, len)                                                                     \
    do {                                                                                                     \
        size_t _x = 0;                                                                                       \
        while ((act)[_x] == (exp)[_x]) {                                                                     \
            _x++;                                                                                            \
        }                                                                                                    \
        if (_x != (len)) {                                                                                   \
            size_t _y = 0;                                                                                   \
            HDprintf("First bytes differ at %zu\n", _x);                                                     \
            HDprintf("exp  ");                                                                               \
            for (_y = _x; _y < (len); _y++) {                                                                \
                HDprintf("%02X", (unsigned char)(exp)[_y]);                                                  \
            }                                                                                                \
            HDprintf("\nact  ");                                                                             \
            for (_y = _x; _y < (len); _y++) {                                                                \
                HDprintf("%02X", (unsigned char)(act)[_y]);                                                  \
            }                                                                                                \
            HDprintf("\n");                                                                                  \
        }                                                                                                    \
    } while (0); /* end PRINT_BUFFER_DIFF */

/* ---------------------------------------------------------------------------
 * Function:    test_xmit_encode_decode
 *
 * Purpose:     Test byte-encoding operations for network transport.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Jacob Smith
 *              2020-02-02
 * ---------------------------------------------------------------------------
 */
static int
test_xmit_encode_decode(void)
{
    H5FD_mirror_xmit_t xmit_mock; /* re-used header in various xmit tests */

    TESTING("Mirror encode/decode of xmit elements");

    /* Set bogus values matching expected; encoding doesn't care
     * Use sequential values to easily generate the expected buffer with a
     * for loop.
     */
    xmit_mock.magic         = 0x00010203;
    xmit_mock.version       = 0x04;
    xmit_mock.session_token = 0x05060708;
    xmit_mock.xmit_count    = 0x090A0B0C;
    xmit_mock.op            = 0x0D;

    /* Test uint8_t encode/decode
     */
    do {
        unsigned char buf[8];
        unsigned char expected[8];
        const uint8_t v   = 200;
        unsigned char out = 0;

        /* Start of buffer uint8_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[0] = 200;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint8(buf, v) != 1) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint8(&out, buf) != 1) {
            TEST_ERROR;
        }
        if (v != out) {
            TEST_ERROR;
        }

        /* Middle of buffer uint8_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[3] = v;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint8((buf + 3), v) != 1) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint8(&out, (buf + 3)) != 1) {
            TEST_ERROR;
        }
        if (v != out) {
            TEST_ERROR;
        }

        /* End of buffer uint8_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[7] = v;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint8((buf + 7), v) != 1) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint8(&out, (buf + 7)) != 1) {
            TEST_ERROR;
        }
        if (v != out) {
            TEST_ERROR;
        }

    } while (0); /* end uint8_t en/decode */

    /* Test uint16_t encode/decode
     */
    do {
        unsigned char  buf[8];
        unsigned char  expected[8];
        const uint16_t v   = 0x8F02;
        uint16_t       out = 0;

        /* Start of buffer uint16_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[0] = 0x8F;
        expected[1] = 0x02;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint16(buf, v) != 2) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint16(&out, buf) != 2) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }

        /* Middle of buffer uint16_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[3] = 0x8F;
        expected[4] = 0x02;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint16((buf + 3), v) != 2) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint16(&out, (buf + 3)) != 2) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }
        /* slice */
        if (H5FD__mirror_xmit_decode_uint16(&out, (buf + 4)) != 2) {
            TEST_ERROR;
        }
        if (out != 0x0200) {
            TEST_ERROR;
        }

        /* End of buffer uint16_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[6] = 0x8F;
        expected[7] = 0x02;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint16((buf + 6), v) != 2) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint16(&out, (buf + 6)) != 2) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }

    } while (0); /* end uint16_t en/decode */

    /* Test uint32_t encode/decode
     */
    do {
        unsigned char  buf[8];
        unsigned char  expected[8];
        const uint32_t v   = 0x8F020048;
        uint32_t       out = 0;

        /* Start of buffer uint32_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[0] = 0x8F;
        expected[1] = 0x02;
        expected[2] = 0x00;
        expected[3] = 0x48;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint32(buf, v) != 4) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint32(&out, buf) != 4) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }

        /* Middle of buffer uint32_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[3] = 0x8F;
        expected[4] = 0x02;
        expected[5] = 0x00;
        expected[6] = 0x48;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint32((buf + 3), v) != 4) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint32(&out, (buf + 3)) != 4) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }
        /* slice */
        if (H5FD__mirror_xmit_decode_uint32(&out, (buf + 4)) != 4) {
            TEST_ERROR;
        }
        if (out != 0x02004800) {
            TEST_ERROR;
        }

        /* End of buffer uint32_t
         */
        mybzero(buf, 8);
        mybzero(expected, 8);
        expected[4] = 0x8F;
        expected[5] = 0x02;
        expected[6] = 0x00;
        expected[7] = 0x48;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint32((buf + 4), v) != 4) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 8);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint32(&out, (buf + 4)) != 4) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }

    } while (0); /* end uint32_t en/decode */

    /* Test uint64_t encode/decode
     */
    do {
        unsigned char  buf[16];
        unsigned char  expected[16];
        const uint64_t v   = 0x90DCBE17939CE4BB;
        uint64_t       out = 0;

        /* Start of buffer uint64_t
         */
        mybzero(buf, 16);
        mybzero(expected, 16);
        expected[0] = 0x90;
        expected[1] = 0xDC;
        expected[2] = 0xBE;
        expected[3] = 0x17;
        expected[4] = 0x93;
        expected[5] = 0x9C;
        expected[6] = 0xE4;
        expected[7] = 0xBB;
        out         = 0;
        if (H5FD__mirror_xmit_encode_uint64(buf, v) != 8) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 16) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 16);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint64(&out, buf) != 8) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }

        /* Middle of buffer uint64_t
         */
        mybzero(buf, 16);
        mybzero(expected, 16);
        expected[3]  = 0x90;
        expected[4]  = 0xDC;
        expected[5]  = 0xBE;
        expected[6]  = 0x17;
        expected[7]  = 0x93;
        expected[8]  = 0x9C;
        expected[9]  = 0xE4;
        expected[10] = 0xBB;
        out          = 0;
        if (H5FD__mirror_xmit_encode_uint64((buf + 3), v) != 8) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 16) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 16);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint64(&out, (buf + 3)) != 8) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }
        /* slice */
        if (H5FD__mirror_xmit_decode_uint64(&out, (buf + 6)) != 8) {
            TEST_ERROR;
        }
        if (out != 0x17939CE4BB000000) {
            TEST_ERROR;
        }

        /* End of buffer uint64_t
         */
        mybzero(buf, 16);
        mybzero(expected, 16);
        expected[8]  = 0x90;
        expected[9]  = 0xDC;
        expected[10] = 0xBE;
        expected[11] = 0x17;
        expected[12] = 0x93;
        expected[13] = 0x9C;
        expected[14] = 0xE4;
        expected[15] = 0xBB;
        out          = 0;
        if (H5FD__mirror_xmit_encode_uint64((buf + 8), v) != 8) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, 16) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, 16);
            TEST_ERROR;
        }
        if (H5FD__mirror_xmit_decode_uint64(&out, (buf + 8)) != 8) {
            TEST_ERROR;
        }
        if (out != v) {
            TEST_ERROR;
        }

    } while (0); /* end uint64_t en/decode */

    /* Test xmit header structure encode/decode
     * Write bogus but easily verifiable data to inside a buffer, and compare.
     * Then decode the buffer and compare the structure contents.
     * Then repeat from a different offset in the buffer and compare.
     */
    do {
        unsigned char      buf[H5FD_MIRROR_XMIT_HEADER_SIZE + 8];
        unsigned char      expected[H5FD_MIRROR_XMIT_HEADER_SIZE + 8];
        H5FD_mirror_xmit_t xmit_out;
        size_t             i = 0;

        /* sanity check */
        if (14 != H5FD_MIRROR_XMIT_HEADER_SIZE) {
            FAIL_PUTS_ERROR("Header size definition does not match test\n");
        }

        /* Populate the expected buffer; expect end padding of 0xFF
         */
        HDmemset(expected, 0xFF, H5FD_MIRROR_XMIT_HEADER_SIZE + 8);
        for (i = 0; i < H5FD_MIRROR_XMIT_HEADER_SIZE; i++) {
            expected[i + 2] = (unsigned char)i;
        }

        /* Encode, and compare buffer contents
         * Initial buffer is filled with 0xFF to match expected padding
         */
        HDmemset(buf, 0xFF, H5FD_MIRROR_XMIT_HEADER_SIZE + 8);
        if (H5FD_mirror_xmit_encode_header((buf + 2), &xmit_mock) != H5FD_MIRROR_XMIT_HEADER_SIZE) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, H5FD_MIRROR_XMIT_HEADER_SIZE + 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, H5FD_MIRROR_XMIT_HEADER_SIZE + 8);
            TEST_ERROR;
        }

        /* Decode from buffer
         */
        if (H5FD_mirror_xmit_decode_header(&xmit_out, (buf + 2)) != H5FD_MIRROR_XMIT_HEADER_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.magic != xmit_mock.magic)
            TEST_ERROR;
        if (xmit_out.version != xmit_mock.version)
            TEST_ERROR;
        if (xmit_out.session_token != xmit_mock.session_token)
            TEST_ERROR;
        if (xmit_out.xmit_count != xmit_mock.xmit_count)
            TEST_ERROR;
        if (xmit_out.op != xmit_mock.op)
            TEST_ERROR;

        /* Decode from different offset in buffer
         * Observe changes when ingesting the padding
         */
        if (H5FD_mirror_xmit_decode_header(&xmit_out, (buf)) != H5FD_MIRROR_XMIT_HEADER_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.magic != 0xFFFF0001)
            TEST_ERROR;
        if (xmit_out.version != 0x02)
            TEST_ERROR;
        if (xmit_out.session_token != 0x03040506)
            TEST_ERROR;
        if (xmit_out.xmit_count != 0x0708090A)
            TEST_ERROR;
        if (xmit_out.op != 0x0B)
            TEST_ERROR;

    } while (0); /* end xmit header en/decode */

    /* Test xmit set-eoa structure encode/decode
     * Write bogus but easily verifiable data to inside a buffer, and compare.
     * Then decode the buffer and compare the structure contents.
     * Then repeat from a different offset in the buffer and compare.
     */
    do {
        unsigned char          buf[H5FD_MIRROR_XMIT_EOA_SIZE + 8];
        unsigned char          expected[H5FD_MIRROR_XMIT_EOA_SIZE + 8];
        H5FD_mirror_xmit_eoa_t xmit_in;
        H5FD_mirror_xmit_eoa_t xmit_out;
        size_t                 i = 0;

        /* sanity check */
        if ((14 + 9) != H5FD_MIRROR_XMIT_EOA_SIZE) {
            FAIL_PUTS_ERROR("Header size definition does not match test\n");
        }
        if (xmit_mock.op != 0x0D) {
            FAIL_PUTS_ERROR("shared header structure is not in expected state");
        }

        /* Populate the expected buffer; expect end padding of 0xFF
         */
        HDmemset(expected, 0xFF, H5FD_MIRROR_XMIT_EOA_SIZE + 8);
        for (i = 0; i < H5FD_MIRROR_XMIT_EOA_SIZE; i++) {
            expected[i + 2] = (unsigned char)i;
        }

        /* Set xmit_in
         */
        xmit_in.pub      = xmit_mock; /* shared/common */
        xmit_in.type     = 0x0E;
        xmit_in.eoa_addr = 0x0F10111213141516;

        /* Encode, and compare buffer contents
         * Initial buffer is filled with 0xFF to match expected padding
         */
        HDmemset(buf, 0xFF, H5FD_MIRROR_XMIT_EOA_SIZE + 8);
        if (H5FD_mirror_xmit_encode_set_eoa((buf + 2), &xmit_in) != H5FD_MIRROR_XMIT_EOA_SIZE) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, H5FD_MIRROR_XMIT_EOA_SIZE + 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, H5FD_MIRROR_XMIT_EOA_SIZE + 8);
            TEST_ERROR;
        }

        /* Decode from buffer
         */
        if (H5FD_mirror_xmit_decode_set_eoa(&xmit_out, (buf + 2)) != H5FD_MIRROR_XMIT_EOA_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != xmit_mock.magic)
            TEST_ERROR;
        if (xmit_out.pub.version != xmit_mock.version)
            TEST_ERROR;
        if (xmit_out.pub.session_token != xmit_mock.session_token)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != xmit_mock.xmit_count)
            TEST_ERROR;
        if (xmit_out.pub.op != xmit_mock.op)
            TEST_ERROR;
        if (xmit_out.type != 0x0E)
            TEST_ERROR;
        if (xmit_out.eoa_addr != 0x0F10111213141516)
            TEST_ERROR;

        /* Decode from different offset in buffer
         * Observe changes when ingesting the padding
         */
        if (H5FD_mirror_xmit_decode_set_eoa(&xmit_out, (buf)) != H5FD_MIRROR_XMIT_EOA_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != 0xFFFF0001)
            TEST_ERROR;
        if (xmit_out.pub.version != 0x02)
            TEST_ERROR;
        if (xmit_out.pub.session_token != 0x03040506)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != 0x0708090A)
            TEST_ERROR;
        if (xmit_out.pub.op != 0x0B)
            TEST_ERROR;
        if (xmit_out.type != 0x0C)
            TEST_ERROR;
        if (xmit_out.eoa_addr != 0x0D0E0F1011121314)
            TEST_ERROR;

    } while (0); /* end xmit set-eoa en/decode */

    /* Test xmit lock structure encode/decode
     * Write bogus but easily verifiable data to inside a buffer, and compare.
     * Then decode the buffer and compare the structure contents.
     * Then repeat from a different offset in the buffer and compare.
     */
    do {
        unsigned char           buf[H5FD_MIRROR_XMIT_LOCK_SIZE + 8];
        unsigned char           expected[H5FD_MIRROR_XMIT_LOCK_SIZE + 8];
        H5FD_mirror_xmit_lock_t xmit_in;
        H5FD_mirror_xmit_lock_t xmit_out;
        size_t                  i = 0;

        /* sanity check */
        if ((14 + 8) != H5FD_MIRROR_XMIT_LOCK_SIZE) {
            FAIL_PUTS_ERROR("Header size definition does not match test\n");
        }
        if (xmit_mock.op != 0x0D) {
            FAIL_PUTS_ERROR("shared header structure is not in expected state");
        }

        /* Populate the expected buffer; expect end padding of 0xFF
         */
        HDmemset(expected, 0xFF, H5FD_MIRROR_XMIT_LOCK_SIZE + 8);
        for (i = 0; i < H5FD_MIRROR_XMIT_LOCK_SIZE; i++) {
            expected[i + 2] = (unsigned char)i;
        }

        /* Set xmit_in
         */
        xmit_in.pub = xmit_mock; /* shared/common */
        xmit_in.rw  = 0x0E0F101112131415;

        /* Encode, and compare buffer contents
         * Initial buffer is filled with 0xFF to match expected padding
         */
        HDmemset(buf, 0xFF, H5FD_MIRROR_XMIT_LOCK_SIZE + 8);
        if (H5FD_mirror_xmit_encode_lock((buf + 2), &xmit_in) != H5FD_MIRROR_XMIT_LOCK_SIZE) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, H5FD_MIRROR_XMIT_LOCK_SIZE + 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, H5FD_MIRROR_XMIT_LOCK_SIZE + 8);
            TEST_ERROR;
        }

        /* Decode from buffer
         */
        if (H5FD_mirror_xmit_decode_lock(&xmit_out, (buf + 2)) != H5FD_MIRROR_XMIT_LOCK_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != xmit_mock.magic)
            TEST_ERROR;
        if (xmit_out.pub.version != xmit_mock.version)
            TEST_ERROR;
        if (xmit_out.pub.session_token != xmit_mock.session_token)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != xmit_mock.xmit_count)
            TEST_ERROR;
        if (xmit_out.pub.op != xmit_mock.op)
            TEST_ERROR;
        if (xmit_out.rw != 0x0E0F101112131415)
            TEST_ERROR;

        /* Decode from different offset in buffer
         * Observe changes when ingesting the padding
         */
        if (H5FD_mirror_xmit_decode_lock(&xmit_out, (buf)) != H5FD_MIRROR_XMIT_LOCK_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != 0xFFFF0001)
            TEST_ERROR;
        if (xmit_out.pub.version != 0x02)
            TEST_ERROR;
        if (xmit_out.pub.session_token != 0x03040506)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != 0x0708090A)
            TEST_ERROR;
        if (xmit_out.pub.op != 0x0B)
            TEST_ERROR;
        if (xmit_out.rw != 0x0C0D0E0F10111213)
            TEST_ERROR;

    } while (0); /* end xmit lock en/decode */

    /* Test xmit open structure encode/decode
     * Write bogus but easily verifiable data to inside a buffer, and compare.
     * Then decode the buffer and compare the structure contents.
     * Then repeat from a different offset in the buffer and compare.
     *
     * Verifies that the first zero character in the filepath will end the
     * string, with all following bytes in the encoded buffer being zeroed.
     */
    do {
        unsigned char           buf[H5FD_MIRROR_XMIT_OPEN_SIZE + 8];
        unsigned char           expected[H5FD_MIRROR_XMIT_OPEN_SIZE + 8];
        H5FD_mirror_xmit_open_t xmit_in;
        H5FD_mirror_xmit_open_t xmit_out;
        size_t                  i = 0;

        /* sanity check */
        if ((14 + 20 + 4097) != H5FD_MIRROR_XMIT_OPEN_SIZE) {
            FAIL_PUTS_ERROR("Header size definition does not match test\n");
        }
        if (xmit_mock.op != 0x0D) {
            FAIL_PUTS_ERROR("shared header structure is not in expected state");
        }

        /* Populate the expected buffer; expect end padding of 0xFF
         */
        HDmemset(expected, 0xFF, H5FD_MIRROR_XMIT_OPEN_SIZE + 8);
        for (i = 0; i < H5FD_MIRROR_XMIT_OPEN_SIZE; i++) {
            /* 0x100 is "zero" in a byte, so encode will treat it as a NULL-
             * terminator in the filepath string. Expect all zeroes following.
             */
            expected[i + 2] = (i > 0xFF) ? 0 : (unsigned char)i;
        }

        /* Set xmit_in
         */
        xmit_in.pub         = xmit_mock; /* shared/common */
        xmit_in.flags       = 0x0E0F1011;
        xmit_in.maxaddr     = 0x1213141516171819;
        xmit_in.size_t_blob = 0x1A1B1C1D1E1F2021;
        for (i = 0x22; i < H5FD_MIRROR_XMIT_FILEPATH_MAX + 0x22; i++) {
            /* nonzero values repeat after 0x100, but will not be encoded */
            xmit_in.filename[i - 0x22] = (char)(i % 0x100);
        }
        xmit_in.filename[H5FD_MIRROR_XMIT_FILEPATH_MAX - 1] = 0;

        /* Encode, and compare buffer contents
         * Initial buffer is filled with 0xFF to match expected padding
         */
        HDmemset(buf, 0xFF, H5FD_MIRROR_XMIT_OPEN_SIZE + 8);
        if (H5FD_mirror_xmit_encode_open((buf + 2), &xmit_in) != H5FD_MIRROR_XMIT_OPEN_SIZE) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, H5FD_MIRROR_XMIT_OPEN_SIZE + 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, H5FD_MIRROR_XMIT_OPEN_SIZE + 8);
            TEST_ERROR;
        }

        /* Decode from buffer
         */
        if (H5FD_mirror_xmit_decode_open(&xmit_out, (buf + 2)) != H5FD_MIRROR_XMIT_OPEN_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != xmit_mock.magic)
            TEST_ERROR;
        if (xmit_out.pub.version != xmit_mock.version)
            TEST_ERROR;
        if (xmit_out.pub.session_token != xmit_mock.session_token)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != xmit_mock.xmit_count)
            TEST_ERROR;
        if (xmit_out.pub.op != xmit_mock.op)
            TEST_ERROR;
        if (xmit_out.flags != xmit_in.flags)
            TEST_ERROR;
        if (xmit_out.maxaddr != xmit_in.maxaddr)
            TEST_ERROR;
        if (xmit_out.size_t_blob != xmit_in.size_t_blob)
            TEST_ERROR;
        if (HDstrncmp(xmit_out.filename, xmit_in.filename, H5FD_MIRROR_XMIT_FILEPATH_MAX) != 0) {
            PRINT_BUFFER_DIFF(xmit_out.filename, xmit_in.filename, H5FD_MIRROR_XMIT_FILEPATH_MAX);
            TEST_ERROR;
        }

        /* Decode from different offset in buffer
         * Observe changes when ingesting the padding
         */
        if (H5FD_mirror_xmit_decode_open(&xmit_out, (buf)) != H5FD_MIRROR_XMIT_OPEN_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != 0xFFFF0001)
            TEST_ERROR;
        if (xmit_out.pub.version != 0x02)
            TEST_ERROR;
        if (xmit_out.pub.session_token != 0x03040506)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != 0x0708090A)
            TEST_ERROR;
        if (xmit_out.pub.op != 0x0B)
            TEST_ERROR;
        if (xmit_out.flags != 0x0C0D0E0F)
            TEST_ERROR;
        if (xmit_out.maxaddr != 0x1011121314151617)
            TEST_ERROR;
        if (xmit_out.size_t_blob != 0x18191A1B1C1D1E1F)
            TEST_ERROR;
        /* update expected "filepath" in structure */
        for (i = 0x20; i < H5FD_MIRROR_XMIT_FILEPATH_MAX + 0x20; i++) {
            xmit_in.filename[i - 0x20] = (i > 0xFF) ? 0 : (char)i;
        }
        if (HDstrncmp(xmit_out.filename, xmit_in.filename, H5FD_MIRROR_XMIT_FILEPATH_MAX) != 0) {
            PRINT_BUFFER_DIFF(xmit_out.filename, xmit_in.filename, H5FD_MIRROR_XMIT_FILEPATH_MAX);
            TEST_ERROR;
        }

    } while (0); /* end xmit open en/decode */

    /* Test xmit reply structure encode/decode
     * Write bogus but easily verifiable data to inside a buffer, and compare.
     * Then decode the buffer and compare the structure contents.
     * Then repeat from a different offset in the buffer and compare.
     *
     * Verifies that the first zero character in the filepath will end the
     * string, with all following bytes in the encoded buffer being zeroed.
     */
    do {
        unsigned char            buf[H5FD_MIRROR_XMIT_REPLY_SIZE + 8];
        unsigned char            expected[H5FD_MIRROR_XMIT_REPLY_SIZE + 8];
        H5FD_mirror_xmit_reply_t xmit_in;
        H5FD_mirror_xmit_reply_t xmit_out;
        size_t                   i = 0;

        /* sanity check */
        if ((14 + 4 + 256) != H5FD_MIRROR_XMIT_REPLY_SIZE) {
            FAIL_PUTS_ERROR("Header size definition does not match test\n");
        }
        if (xmit_mock.op != 0x0D) {
            FAIL_PUTS_ERROR("shared header structure is not in expected state");
        }

        /* Populate the expected buffer; expect end padding of 0xFF
         */
        HDmemset(expected, 0xFF, H5FD_MIRROR_XMIT_REPLY_SIZE + 8);
        for (i = 0; i < H5FD_MIRROR_XMIT_REPLY_SIZE; i++) {
            /* 0x100 is "zero" in a byte, so encode will treat it as a NULL-
             * terminator in the filepath string. Expect all zeroes following.
             */
            expected[i + 2] = (i > 0xFF) ? 0 : (unsigned char)i;
        }

        /* Set xmit_in
         */
        xmit_in.pub    = xmit_mock; /* shared/common */
        xmit_in.status = 0x0E0F1011;
        for (i = 0x12; i < H5FD_MIRROR_STATUS_MESSAGE_MAX + 0x12; i++) {
            /* nonzero values repeat after 0x100, but will not be encoded */
            xmit_in.message[i - 0x12] = (char)(i % 0x100);
        }
        xmit_in.message[H5FD_MIRROR_STATUS_MESSAGE_MAX - 1] = 0;

        /* Encode, and compare buffer contents
         * Initial buffer is filled with 0xFF to match expected padding
         */
        HDmemset(buf, 0xFF, H5FD_MIRROR_XMIT_REPLY_SIZE + 8);
        if (H5FD_mirror_xmit_encode_reply((buf + 2), &xmit_in) != H5FD_MIRROR_XMIT_REPLY_SIZE) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, H5FD_MIRROR_XMIT_REPLY_SIZE + 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, H5FD_MIRROR_XMIT_REPLY_SIZE + 8);
            TEST_ERROR;
        }

        /* Decode from buffer
         */
        if (H5FD_mirror_xmit_decode_reply(&xmit_out, (buf + 2)) != H5FD_MIRROR_XMIT_REPLY_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != xmit_mock.magic)
            TEST_ERROR;
        if (xmit_out.pub.version != xmit_mock.version)
            TEST_ERROR;
        if (xmit_out.pub.session_token != xmit_mock.session_token)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != xmit_mock.xmit_count)
            TEST_ERROR;
        if (xmit_out.pub.op != xmit_mock.op)
            TEST_ERROR;
        if (xmit_out.status != xmit_in.status)
            TEST_ERROR;
        if (HDstrncmp(xmit_out.message, xmit_in.message, H5FD_MIRROR_STATUS_MESSAGE_MAX) != 0) {
            PRINT_BUFFER_DIFF(xmit_out.message, xmit_in.message, H5FD_MIRROR_STATUS_MESSAGE_MAX);
            TEST_ERROR;
        }

        /* Decode from different offset in buffer
         * Observe changes when ingesting the padding
         */
        if (H5FD_mirror_xmit_decode_reply(&xmit_out, (buf)) != H5FD_MIRROR_XMIT_REPLY_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != 0xFFFF0001)
            TEST_ERROR;
        if (xmit_out.pub.version != 0x02)
            TEST_ERROR;
        if (xmit_out.pub.session_token != 0x03040506)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != 0x0708090A)
            TEST_ERROR;
        if (xmit_out.pub.op != 0x0B)
            TEST_ERROR;
        if (xmit_out.status != 0x0C0D0E0F)
            TEST_ERROR;
        /* update expected "message" in structure */
        for (i = 0x10; i < H5FD_MIRROR_STATUS_MESSAGE_MAX + 0x10; i++) {
            xmit_in.message[i - 0x10] = (i > 0xFF) ? 0 : (char)i;
        }
        if (HDstrncmp(xmit_out.message, xmit_in.message, H5FD_MIRROR_STATUS_MESSAGE_MAX) != 0) {
            PRINT_BUFFER_DIFF(xmit_out.message, xmit_in.message, H5FD_MIRROR_STATUS_MESSAGE_MAX);
            TEST_ERROR;
        }

    } while (0); /* end xmit reply en/decode */

    /* Test xmit write structure encode/decode
     * Write bogus but easily verifiable data to inside a buffer, and compare.
     * Then decode the buffer and compare the structure contents.
     * Then repeat from a different offset in the buffer and compare.
     */
    do {
        unsigned char            buf[H5FD_MIRROR_XMIT_WRITE_SIZE + 8];
        unsigned char            expected[H5FD_MIRROR_XMIT_WRITE_SIZE + 8];
        H5FD_mirror_xmit_write_t xmit_in;
        H5FD_mirror_xmit_write_t xmit_out;
        size_t                   i = 0;

        /* sanity check */
        if ((14 + 17) != H5FD_MIRROR_XMIT_WRITE_SIZE) {
            FAIL_PUTS_ERROR("Header size definition does not match test\n");
        }
        if (xmit_mock.op != 0x0D) {
            FAIL_PUTS_ERROR("shared header structure is not in expected state");
        }

        /* Populate the expected buffer; expect end padding of 0xFF
         */
        HDmemset(expected, 0xFF, H5FD_MIRROR_XMIT_WRITE_SIZE + 8);
        for (i = 0; i < H5FD_MIRROR_XMIT_WRITE_SIZE; i++) {
            expected[i + 2] = (unsigned char)i;
        }

        /* Set xmit_in
         */
        xmit_in.pub    = xmit_mock; /* shared/common */
        xmit_in.type   = 0x0E;
        xmit_in.offset = 0x0F10111213141516;
        xmit_in.size   = 0x1718191A1B1C1D1E;

        /* Encode, and compare buffer contents
         * Initial buffer is filled with 0xFF to match expected padding
         */
        HDmemset(buf, 0xFF, H5FD_MIRROR_XMIT_WRITE_SIZE + 8);
        if (H5FD_mirror_xmit_encode_write((buf + 2), &xmit_in) != H5FD_MIRROR_XMIT_WRITE_SIZE) {
            TEST_ERROR;
        }
        if (HDmemcmp(buf, expected, H5FD_MIRROR_XMIT_WRITE_SIZE + 8) != 0) {
            PRINT_BUFFER_DIFF(buf, expected, H5FD_MIRROR_XMIT_WRITE_SIZE + 8);
            TEST_ERROR;
        }

        /* Decode from buffer
         */
        if (H5FD_mirror_xmit_decode_write(&xmit_out, (buf + 2)) != H5FD_MIRROR_XMIT_WRITE_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != xmit_mock.magic)
            TEST_ERROR;
        if (xmit_out.pub.version != xmit_mock.version)
            TEST_ERROR;
        if (xmit_out.pub.session_token != xmit_mock.session_token)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != xmit_mock.xmit_count)
            TEST_ERROR;
        if (xmit_out.pub.op != xmit_mock.op)
            TEST_ERROR;
        if (xmit_out.type != 0x0E)
            TEST_ERROR;
        if (xmit_out.offset != 0x0F10111213141516)
            TEST_ERROR;
        if (xmit_out.size != 0x1718191A1B1C1D1E)
            TEST_ERROR;

        /* Decode from different offset in buffer
         * Observe changes when ingesting the padding
         */
        if (H5FD_mirror_xmit_decode_write(&xmit_out, (buf)) != H5FD_MIRROR_XMIT_WRITE_SIZE) {
            TEST_ERROR;
        }
        if (xmit_out.pub.magic != 0xFFFF0001)
            TEST_ERROR;
        if (xmit_out.pub.version != 0x02)
            TEST_ERROR;
        if (xmit_out.pub.session_token != 0x03040506)
            TEST_ERROR;
        if (xmit_out.pub.xmit_count != 0x0708090A)
            TEST_ERROR;
        if (xmit_out.pub.op != 0x0B)
            TEST_ERROR;
        if (xmit_out.type != 0x0C)
            TEST_ERROR;
        if (xmit_out.offset != 0x0D0E0F1011121314)
            TEST_ERROR;
        if (xmit_out.size != 0x15161718191A1B1C)
            TEST_ERROR;

    } while (0); /* end xmit write en/decode */

    PASSED();
    return 0;

error:
    return -1;
} /* end test_xmit_encode_decode */

/* ---------------------------------------------------------------------------
 * Function:    create_mirroring_split_fapl
 *
 * Purpose:     Create and populate a mirroring FAPL ID.
 *              Creates target files with the given base name -- ideally the
 *              test name -- and creates mirroring/split FAPL set to use the
 *              global mirroring info and a sec2 R/W channel driver.
 *
 *              TODO: receive target IP from caller?
 *
 * Return:      Success: HID of the top-level (splitter) FAPL, a non-negative
 *                       value.
 *              Failure: H5I_INVALID_HID, a negative value.
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
static hid_t
create_mirroring_split_fapl(const char *_basename, struct mirrortest_filenames *names)
{
    H5FD_splitter_vfd_config_t splitter_config;
    H5FD_mirror_fapl_t         mirror_conf;
    hid_t                      ret_value = H5I_INVALID_HID;

    if (_basename == NULL || *_basename == '\0') {
        TEST_ERROR;
    }

    splitter_config.magic          = H5FD_SPLITTER_MAGIC;
    splitter_config.version        = H5FD_CURR_SPLITTER_VFD_CONFIG_VERSION;
    splitter_config.ignore_wo_errs = FALSE;

    /* Create Splitter R/W channel driver (sec2)
     */
    splitter_config.rw_fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == splitter_config.rw_fapl_id) {
        TEST_ERROR;
    }
    if (H5Pset_fapl_sec2(splitter_config.rw_fapl_id) == FAIL) {
        TEST_ERROR;
    }

    /* Create Splitter W/O channel driver (mirror)
     */
    mirror_conf.magic          = H5FD_MIRROR_FAPL_MAGIC;
    mirror_conf.version        = H5FD_MIRROR_CURR_FAPL_T_VERSION;
    mirror_conf.handshake_port = SERVER_HANDSHAKE_PORT;
    if (HDstrncpy(mirror_conf.remote_ip, SERVER_IP_ADDRESS, H5FD_MIRROR_MAX_IP_LEN) == NULL) {
        TEST_ERROR;
    }
    splitter_config.wo_fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == splitter_config.wo_fapl_id) {
        TEST_ERROR;
    }
    if (H5Pset_fapl_mirror(splitter_config.wo_fapl_id, &mirror_conf) == FAIL) {
        TEST_ERROR;
    }

    /* Build r/w, w/o, and log file paths
     */
    if (build_paths(_basename, &splitter_config, names) < 0) {
        TEST_ERROR;
    }

    /* Set file paths for w/o and logfile
     */
    if (HDstrncpy(splitter_config.wo_path, (const char *)names->wo, H5FD_SPLITTER_PATH_MAX) == NULL) {
        TEST_ERROR;
    }
    if (HDstrncpy(splitter_config.log_file_path, (const char *)names->log, H5FD_SPLITTER_PATH_MAX) == NULL) {
        TEST_ERROR;
    }

    /* Create Splitter FAPL
     */
    ret_value = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == ret_value) {
        TEST_ERROR;
    }
    if (H5Pset_fapl_splitter(ret_value, &splitter_config) == FAIL) {
        TEST_ERROR;
    }

    /* Close FAPLs created for child channels
     */
    if (H5Pclose(splitter_config.rw_fapl_id) < 0) {
        TEST_ERROR;
    }
    splitter_config.rw_fapl_id = H5I_INVALID_HID;
    if (H5Pclose(splitter_config.wo_fapl_id) < 0) {
        TEST_ERROR;
    }
    splitter_config.wo_fapl_id = H5I_INVALID_HID;

    return ret_value;

error:
    if (splitter_config.wo_fapl_id >= 0) {
        (void)H5Pclose(splitter_config.wo_fapl_id);
    }
    if (splitter_config.rw_fapl_id >= 0) {
        (void)H5Pclose(splitter_config.rw_fapl_id);
    }
    if (ret_value >= 0) {
        (void)H5Pclose(ret_value);
    }
    return H5I_INVALID_HID;
} /* end create_mirroring_split_fapl() */

/* ---------------------------------------------------------------------------
 * Function:    test_create_and_close
 *
 * Purpose:     Test/demonstrate a do-nothing file open and close.
 *
 *              Verifying file existence and contents is part of other tests.
 *
 *              TODO: receive target IP from caller?
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Jacob Smith
 *              2019-12-17
 * ---------------------------------------------------------------------------
 */
static int
test_create_and_close(void)
{
    struct mirrortest_filenames names;
    hid_t                       file_id = H5I_INVALID_HID;
    hid_t                       fapl_id = H5P_DEFAULT;

    TESTING("File creation and immediate close");

    /* Create FAPL for Splitter[sec2|mirror]
     */
    fapl_id = create_mirroring_split_fapl("basic_create", &names);
    if (H5I_INVALID_HID == fapl_id) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* TEST: Create and Close */

    file_id = H5Fcreate(names.rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* Standard cleanup */

    if (H5Fclose(file_id) == FAIL) {
        TEST_ERROR;
    }
    if (fapl_id != H5P_DEFAULT && fapl_id >= 0) {
        if (H5Pclose(fapl_id) == FAIL) {
            TEST_ERROR;
        }
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Fclose(file_id);
        (void)H5Pclose(fapl_id);
    }
    H5E_END_TRY;
    return -1;
} /* end test_create_and_close() */

/* ----------------------------------------------------------------------------
 * Function:    create_datasets
 *
 * Purpose:     Given a file ID and least and greateset dataset indices, create
 *              populated chunked datasets in the target file from min_dset to
 *              (and including) max_dset.
 *              Uses #defined constants to determine chunk and dataset sizes
 *              and values.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2019-08-14
 * ----------------------------------------------------------------------------
 */
static herr_t
create_datasets(hid_t file_id, unsigned min_dset, unsigned max_dset)
{
    hid_t        dataspace_ids[MAX_DSET_COUNT + 1];
    hid_t        dataset_ids[MAX_DSET_COUNT + 1];
    hid_t        filespace_ids[MAX_DSET_COUNT + 1];
    int          data_chunk[CHUNK_DIM][CHUNK_DIM];
    unsigned int i, j, k, l, m;
    hsize_t      offset[2];
    hid_t        memspace_id   = H5I_INVALID_HID;
    hsize_t      a_size[2]     = {CHUNK_DIM, CHUNK_DIM};
    hsize_t      chunk_dims[2] = {CHUNK_DIM, CHUNK_DIM};
    hsize_t      dset_dims[2]  = {DSET_DIM, DSET_DIM};

    HDassert(file_id >= 0);
    HDassert(min_dset <= max_dset);
    HDassert(max_dset <= MAX_DSET_COUNT);

    LOGPRINT(2, "create_dataset()\n");

    /* ---------------------------------
     * "Clear" ID arrays
     */

    for (i = 0; i < MAX_DSET_COUNT; i++) {
        LOGPRINT(3, "clearing IDs [%d]\n", i);
        dataspace_ids[i] = H5I_INVALID_HID;
        dataset_ids[i]   = H5I_INVALID_HID;
        filespace_ids[i] = H5I_INVALID_HID;
    }

    /* ---------------------------------
     * Generate dataspace, dataset, and 'filespace' IDs
     */

    if (_create_chunking_ids(file_id, min_dset, max_dset, chunk_dims, dset_dims, dataspace_ids, filespace_ids,
                             dataset_ids, &memspace_id) == FAIL) {
        TEST_ERROR;
    }

    /* ---------------------------------
     * Initialize (write) all datasets in a "round robin"...
     * for a given chunk 'location', write chunk data to each dataset.
     */

    for (i = 0; i < DSET_DIM; i += CHUNK_DIM) {
        LOGPRINT(3, "i: %d\n", i);
        for (j = 0; j < DSET_DIM; j += CHUNK_DIM) {
            LOGPRINT(3, "  j: %d\n", j);
            for (m = min_dset; m <= max_dset; m++) {
                LOGPRINT(3, "    m: %d\n", m);
                for (k = 0; k < CHUNK_DIM; k++) {
                    for (l = 0; l < CHUNK_DIM; l++) {
                        data_chunk[k][l] = (int)((DSET_DIM * DSET_DIM * m) + (DSET_DIM * (i + k)) + j + l);
                        LOGPRINT(3, "      data_chunk[%d][%d]: %d\n", k, l, data_chunk[k][l]);
                    }
                }

                /* select on disk hyperslab */
                offset[0] = (hsize_t)i;
                offset[1] = (hsize_t)j;
                LOGPRINT(3, "    H5Sselect_hyperslab()\n");
                if (H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET, offset, NULL, a_size, NULL) < 0) {
                    TEST_ERROR;
                }

                LOGPRINT(3, "    H5Dwrite()\n");
                if (H5Dwrite(dataset_ids[m], H5T_NATIVE_INT, memspace_id, filespace_ids[m], H5P_DEFAULT,
                             data_chunk) < 0) {
                    TEST_ERROR;
                }
            }
        }
    }

    /* ---------------------------------
     * Read and verify data from datasets
     */

    if (_verify_datasets(min_dset, max_dset, filespace_ids, dataset_ids, memspace_id) == FAIL) {
        TEST_ERROR;
    }

    /* ---------------------------------
     * Cleanup
     */

    if (_close_chunking_ids(min_dset, max_dset, dataspace_ids, filespace_ids, dataset_ids, &memspace_id) ==
        FAIL) {
        TEST_ERROR;
    }

    return SUCCEED;

error:
    (void)_close_chunking_ids(min_dset, max_dset, dataspace_ids, filespace_ids, dataset_ids, &memspace_id);
    LOGPRINT(1, "create_datasets() FAILED\n");
    return FAIL;
} /* end create_datasets() */

/* ----------------------------------------------------------------------------
 * Function:   _create_chunking_ids
 *
 * Purpose:    Create new IDs to be used with the associated file.
 *
 * Return:     SUCCEED/FAIL
 *
 * Programer:  Jacob Smith
 *             2019
 * ----------------------------------------------------------------------------
 */
static herr_t
_create_chunking_ids(hid_t file_id, unsigned min_dset, unsigned max_dset, hsize_t *chunk_dims,
                     hsize_t *dset_dims, hid_t *dataspace_ids, hid_t *filespace_ids, hid_t *dataset_ids,
                     hid_t *memspace_id)
{
    char     dset_name[DSET_NAME_LEN + 1];
    unsigned m       = 0;
    hid_t    dcpl_id = H5I_INVALID_HID;

    LOGPRINT(2, "_create_chunking_ids()\n");

    /* --------------------
     * Create chunking DCPL
     */

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_id < 0) {
        TEST_ERROR;
    }
    if (H5Pset_chunk(dcpl_id, 2, chunk_dims) == FAIL) {
        TEST_ERROR;
    }

    /* --------------------
     * Create dataspace IDs
     */

    for (m = min_dset; m <= max_dset; m++) {
        dataspace_ids[m] = H5Screate_simple(2, dset_dims, NULL);
        if (dataspace_ids[m] < 0) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to create dataspace ID %d\n", m);
            FAIL_PUTS_ERROR(mesg);
        }
    }

    /* --------------------
     * Create dataset IDs
     */

    for (m = min_dset; m <= max_dset; m++) {
        if (HDsnprintf(dset_name, DSET_NAME_LEN, "/dset%03d", m) > DSET_NAME_LEN) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to compose dset name %d\n", m);
            FAIL_PUTS_ERROR(mesg);
        }

        dataset_ids[m] =
            H5Dcreate(file_id, dset_name, H5T_STD_I32BE, dataspace_ids[m], H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
        if (dataset_ids[m] < 0) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to create dset ID %d\n", m);
            FAIL_PUTS_ERROR(mesg);
        }
    }

    /* --------------------
     * Get file space IDs
     */

    for (m = min_dset; m <= max_dset; m++) {
        filespace_ids[m] = H5Dget_space(dataset_ids[m]);
        if (filespace_ids[m] < 0) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to create filespace ID %d\n", m);
            FAIL_PUTS_ERROR(mesg);
        }
    }

    /* --------------------
     * Create mem space to be used to read and write chunks
     */

    *memspace_id = H5Screate_simple(2, chunk_dims, NULL);
    if (*memspace_id < 0) {
        TEST_ERROR;
    }

    /* --------------------
     * Clean up the DCPL, even if there were errors before
     */

    if (dcpl_id != H5P_DEFAULT && dcpl_id != H5I_INVALID_HID) {
        if (H5Pclose(dcpl_id) == FAIL) {
            TEST_ERROR;
        }
    }

    return SUCCEED;

error:
    if (dcpl_id != H5P_DEFAULT && dcpl_id != H5I_INVALID_HID) {
        (void)H5Pclose(dcpl_id);
    }
    LOGPRINT(1, "_create_chunking_ids() FAILED\n");
    return FAIL;
} /* end _create_chunking_ids() */

/* ----------------------------------------------------------------------------
 * Function:    _open_chunking_ids
 *
 * Purpose:     Open/access IDs from the given file.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2019
 * ----------------------------------------------------------------------------
 */
static herr_t
_open_chunking_ids(hid_t file_id, unsigned min_dset, unsigned max_dset, hsize_t *chunk_dims,
                   hid_t *filespace_ids, hid_t *dataset_ids, hid_t *memspace_id)
{
    char     dset_name[DSET_NAME_LEN + 1];
    unsigned m = 0;

    LOGPRINT(2, "_open_chunking_ids()\n");

    /* --------------------
     * Open dataset IDs
     */

    for (m = min_dset; m <= max_dset; m++) {
        if (HDsnprintf(dset_name, DSET_NAME_LEN, "/dset%03d", m) > DSET_NAME_LEN) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to compose dset name %d\n", m);
            FAIL_PUTS_ERROR(mesg);
        }

        dataset_ids[m] = H5Dopen2(file_id, dset_name, H5P_DEFAULT);
        if (dataset_ids[m] < 0) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to open dset ID %d\n", m);
            FAIL_PUTS_ERROR(mesg);
        }
    }

    /* --------------------
     * Open filespace IDs
     */

    for (m = min_dset; m <= max_dset; m++) {
        filespace_ids[m] = H5Dget_space(dataset_ids[m]);
        if (filespace_ids[m] < 0) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to get filespace ID %d\n", m);
            FAIL_PUTS_ERROR(mesg);
        }
    }

    /* --------------------
     * Create mem space to be used to read and write chunks
     */

    *memspace_id = H5Screate_simple(2, chunk_dims, NULL);
    if (*memspace_id < 0) {
        TEST_ERROR;
    }

    return SUCCEED;

error:
    LOGPRINT(1, "_open_chunking_ids() FAILED\n");
    return FAIL;
} /* end _open_chunking_ids() */

/* ---------------------------------------------------------------------------
 * Function:    _close_chunking_ids
 *
 * Purpose:     Close IDs that were created or opened.
 *              Pass NULL into `dataspace_ids` when closing items opened with
 *              _open_chunking_ids(). (as opposed to created IDs)
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
static herr_t
_close_chunking_ids(unsigned min_dset, unsigned max_dset, hid_t *dataspace_ids, hid_t *filespace_ids,
                    hid_t *dataset_ids, hid_t *memspace_id)
{
    unsigned m;

    LOGPRINT(2, "_close_chunking_ids()\n");

    for (m = min_dset; m <= max_dset; m++) {
        LOGPRINT(3, "closing ids[%d]\n", m);
        if (dataspace_ids) {
            if (H5Sclose(dataspace_ids[m]) < 0) {
                HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to close dataspace_id[%d]\n", m);
                FAIL_PUTS_ERROR(mesg);
            }
        }
        if (H5Dclose(dataset_ids[m]) < 0) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to close dataset_id[%d]\n", m);
            FAIL_PUTS_ERROR(mesg);
        }
        if (H5Sclose(filespace_ids[m]) < 0) {
            HDsnprintf(mesg, MIRR_MESG_SIZE, "unable to close filespace_id[%d]\n", m);
            FAIL_PUTS_ERROR(mesg);
        }
    }

    if ((*memspace_id != H5I_INVALID_HID) && (H5Sclose(*memspace_id) < 0)) {
        TEST_ERROR;
    }

    return SUCCEED;

error:
    LOGPRINT(1, "_close_chunking_ids() FAILED\n");
    return FAIL;
} /* end _close_chunking_ids() */

/* ---------------------------------------------------------------------------
 * Function:    _verify_datasets
 *
 * Purpose:     Check that each chunk's contents are as expected, as pertaining
 *              to create_datasets().
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
static herr_t
_verify_datasets(unsigned min_dset, unsigned max_dset, hid_t *filespace_ids, hid_t *dataset_ids,
                 hid_t memspace_id)
{
    unsigned i, j, k, l, m;
    int      data_chunk[CHUNK_DIM][CHUNK_DIM];
    hsize_t  offset[2];
    hsize_t  a_size[2] = {CHUNK_DIM, CHUNK_DIM};

    LOGPRINT(2, "_verify_datasets()\n");

    for (i = 0; i < DSET_DIM; i += CHUNK_DIM) {
        LOGPRINT(3, "i: %d\n", i);
        for (j = 0; j < DSET_DIM; j += CHUNK_DIM) {
            LOGPRINT(3, "  j: %d\n", j);
            for (m = min_dset; m <= max_dset; m++) {
                LOGPRINT(3, "    m: %d\n", m);

                /* select on disk hyperslab */
                offset[0] = (hsize_t)i;
                offset[1] = (hsize_t)j;
                if (H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET, offset, NULL, a_size, NULL) < 0) {
                    TEST_ERROR;
                }

                if (H5Dread(dataset_ids[m], H5T_NATIVE_INT, memspace_id, filespace_ids[m], H5P_DEFAULT,
                            data_chunk) < 0) {
                    HDsnprintf(mesg, MIRR_MESG_SIZE, "      H5Dread() [%d][%d][%d]\n", i, j, m);
                    FAIL_PUTS_ERROR(mesg);
                }

                for (k = 0; k < CHUNK_DIM; k++) {
                    for (l = 0; l < CHUNK_DIM; l++) {
                        if ((unsigned)data_chunk[k][l] !=
                            ((DSET_DIM * DSET_DIM * m) + (DSET_DIM * (i + k)) + j + l)) {
                            HDsnprintf(mesg, MIRR_MESG_SIZE, "      MISMATCH [%d][%d][%d][%d][%d]\n", i, j, m,
                                       k, l);
                            FAIL_PUTS_ERROR(mesg);
                        }
                    }
                }
            }
        }
    }

    return SUCCEED;

error:
    LOGPRINT(1, "_verify_datasets() FAILED\n");
    return FAIL;
} /* end _verify_datasets() */

/* ---------------------------------------------------------------------------
 * Function:    verify_datasets
 *
 * Purpose:     Inspect the datasets in the file created by create_datasets().
 *              Wrapper for _verify_datasets() -- this function sets up and
 *              tears down accessor information.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
static herr_t
verify_datasets(hid_t file_id, unsigned min_dset, unsigned max_dset)
{
    hid_t    dataset_ids[MAX_DSET_COUNT + 1];
    hid_t    filespace_ids[MAX_DSET_COUNT + 1];
    unsigned i;
    hid_t    memspace_id   = H5I_INVALID_HID;
    hsize_t  chunk_dims[2] = {CHUNK_DIM, CHUNK_DIM};

    HDassert(file_id >= 0);
    HDassert(min_dset <= max_dset);
    HDassert(max_dset <= MAX_DSET_COUNT);

    LOGPRINT(2, "verify_datasets()\n");

    /* ---------------------------------
     * "Clear" ID arrays
     */

    for (i = 0; i < MAX_DSET_COUNT; i++) {
        LOGPRINT(3, "clearing IDs [%d]\n", i);
        dataset_ids[i]   = H5I_INVALID_HID;
        filespace_ids[i] = H5I_INVALID_HID;
    }

    /* ---------------------------------
     * Generate dataspace, dataset, and 'filespace' IDs
     */

    if (_open_chunking_ids(file_id, min_dset, max_dset, chunk_dims, filespace_ids, dataset_ids,
                           &memspace_id) == FAIL) {
        TEST_ERROR;
    }

    /* ---------------------------------
     * Read and verify data from datasets
     */

    if (_verify_datasets(min_dset, max_dset, filespace_ids, dataset_ids, memspace_id) == FAIL) {
        TEST_ERROR;
    }

    /* ---------------------------------
     * Cleanup
     */

    if (_close_chunking_ids(min_dset, max_dset, NULL, filespace_ids, dataset_ids, &memspace_id) == FAIL) {
        TEST_ERROR;
    }

    return SUCCEED;

error:
    LOGPRINT(1, "verify_datasets() FAILED\n");
    (void)_close_chunking_ids(min_dset, max_dset, NULL, filespace_ids, dataset_ids, &memspace_id);
    return FAIL;

} /* end verify_datasets() */

/* ---------------------------------------------------------------------------
 * Function:    test_basic_dataset_write
 *
 * Purpose:     Create and close files; repoen files and write a dataset,
 *              close; compare files.
 *
 *              TODO: receive target IP from caller?
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
static int
test_basic_dataset_write(void)
{
    struct mirrortest_filenames names;
    hid_t                       file_id   = H5I_INVALID_HID;
    hid_t                       fapl_id   = H5P_DEFAULT;
    hid_t                       dset_id   = H5I_INVALID_HID;
    hid_t                       dspace_id = H5I_INVALID_HID;
    hid_t                       dtype_id  = H5T_NATIVE_INT;
    hsize_t                     dims[2]   = {DATABUFFER_SIZE, DATABUFFER_SIZE};
    int *                       buf       = NULL;
    int                         i         = 0;
    int                         j         = 0;

    TESTING("Mirror open and dataset writing");

    /* Create FAPL for Splitter[sec2|mirror]
     */
    fapl_id = create_mirroring_split_fapl("basic_write", &names);
    if (H5I_INVALID_HID == fapl_id) {
        TEST_ERROR;
    }

    /* Prepare data to be written
     */
    buf = (int *)HDmalloc(DATABUFFER_SIZE * DATABUFFER_SIZE * sizeof(int));
    if (NULL == buf) {
        TEST_ERROR;
    }
    for (i = 0; i < DATABUFFER_SIZE; i++) {
        for (j = 0; j < DATABUFFER_SIZE; j++) {
            int k  = i * DATABUFFER_SIZE + j;
            buf[k] = k;
        }
    }

    /* -------------------- */
    /* TEST: Create and Close */

    file_id = H5Fcreate(names.rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }
    if (H5Fclose(file_id) == FAIL) {
        TEST_ERROR;
    }
    file_id = H5I_INVALID_HID;

    /* -------------------- */
    /* TEST: Repoen and Write */

    file_id = H5Fopen(names.rw, H5F_ACC_RDWR, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }

    dspace_id = H5Screate_simple(2, dims, NULL);
    if (H5I_INVALID_HID == dspace_id) {
        TEST_ERROR;
    }
    dset_id = H5Dcreate2(file_id, "dataset", dtype_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (H5I_INVALID_HID == dset_id) {
        TEST_ERROR;
    }

    if (H5Dwrite(dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) == FAIL) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* Standard cleanup */

    HDfree(buf);
    buf = NULL;
    if (H5Dclose(dset_id) == FAIL) {
        TEST_ERROR;
    }
    if (H5Sclose(dspace_id) == FAIL) {
        TEST_ERROR;
    }
    if (H5Fclose(file_id) == FAIL) {
        TEST_ERROR;
    }
    if (fapl_id != H5P_DEFAULT && fapl_id > 0) {
        if (H5Pclose(fapl_id) == FAIL) {
            TEST_ERROR;
        }
    }

    /* -------------------- */
    /* TEST: Verify that the R/W and W/O files are identical */

    if (h5_compare_file_bytes(names.rw, names.wo) < 0) {
        TEST_ERROR;
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Fclose(file_id);
        if (buf) {
            HDfree(buf);
        }
        (void)H5Dclose(dset_id);
        (void)H5Sclose(dspace_id);
        if (fapl_id != H5P_DEFAULT && fapl_id > 0) {
            (void)H5Pclose(fapl_id);
        }
    }
    H5E_END_TRY;
    return -1;
} /* end test_basic_dataset_write() */

/* ---------------------------------------------------------------------------
 * Function:    test_chunked_dataset_write
 *
 * Purpose:     Create and close files; repoen files and write a dataset,
 *              close; compare files.
 *
 *              TODO: receive target IP from caller?
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
static int
test_chunked_dataset_write(void)
{
    struct mirrortest_filenames names;
    hid_t                       file_id = H5I_INVALID_HID;
    hid_t                       fapl_id = H5P_DEFAULT;

    TESTING("Mirror open and dataset writing (chunked)");

    /* Create FAPL for Splitter[sec2|mirror]
     */
    fapl_id = create_mirroring_split_fapl("chunked_write", &names);
    if (H5I_INVALID_HID == fapl_id) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* TEST: Create and Close */

    file_id = H5Fcreate(names.rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }
    if (H5Fclose(file_id) == FAIL) {
        TEST_ERROR;
    }
    file_id = H5I_INVALID_HID;

    /* -------------------- */
    /* TEST: Reopen and Write */

    file_id = H5Fopen(names.rw, H5F_ACC_RDWR, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }

    /* Write datasets to file
     */
    if (create_datasets(file_id, 0, MAX_DSET_COUNT) == FAIL) {
        TEST_ERROR;
    }

    /* Close to 'flush to disk', and reopen file
     */
    if (H5Fclose(file_id) == FAIL) {
        TEST_ERROR;
    }
    file_id = H5I_INVALID_HID;

    /* Reopen file
     */
    file_id = H5Fopen(names.rw, H5F_ACC_RDWR, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }

    /* Verify written data integrity
     */
    if (verify_datasets(file_id, 0, MAX_DSET_COUNT) == FAIL) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* Standard cleanup */

    if (H5Fclose(file_id) == FAIL) {
        TEST_ERROR;
    }
    file_id = H5I_INVALID_HID;
    if (fapl_id != H5P_DEFAULT && fapl_id > 0) {
        if (H5Pclose(fapl_id) == FAIL) {
            TEST_ERROR;
        }
        fapl_id = H5I_INVALID_HID;
    }

    /* -------------------- */
    /* TEST: Verify that the R/W and W/O files are identical */

    if (h5_compare_file_bytes(names.rw, names.wo) < 0) {
        TEST_ERROR;
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Fclose(file_id);
        if (fapl_id != H5P_DEFAULT && fapl_id > 0) {
            (void)H5Pclose(fapl_id);
        }
    }
    H5E_END_TRY;
    return -1;
} /* end test_chunked_dataset_write() */

/* ---------------------------------------------------------------------------
 * Function:    test_on_disk_zoo
 *
 * Purpose:     Verify that the mirror can handle the passing of all the
 *              various on-disk data structures over the wire, as implemented
 *              in genall5.c:create_zoo().
 *
 *              TODO: receive target IP from caller?
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
static int
test_on_disk_zoo(void)
{
    const char                  grp_name[] = "/only";
    struct mirrortest_filenames names;
    hid_t                       file_id = H5I_INVALID_HID;
    hid_t                       grp_id  = H5I_INVALID_HID;
    hid_t                       fapl_id = H5P_DEFAULT;

    TESTING("'Zoo' of on-disk structures");

    /* Create FAPL for Splitter[sec2|mirror]
     */
    fapl_id = create_mirroring_split_fapl("zoo", &names);
    if (H5I_INVALID_HID == fapl_id) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* TEST: Create file    */
    file_id = H5Fcreate(names.rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }

    grp_id = H5Gcreate2(file_id, grp_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp_id == H5I_INVALID_HID) {
        TEST_ERROR;
    }

    /* Create datasets in file, close (flush) and reopen, validate.
     * Use of ( pass ) a conceit required for using create_ and validate_zoo()
     * from cache_common and/or genall5.
     */

    if (pass) {
        create_zoo(file_id, grp_name, 0);
    }
    if (pass) {
        if (H5Fclose(file_id) == FAIL) {
            TEST_ERROR;
        }
        file_id = H5Fopen(names.rw, H5F_ACC_RDWR, fapl_id);
        if (H5I_INVALID_HID == file_id) {
            TEST_ERROR;
        }
    }
    if (pass) {
        validate_zoo(file_id, grp_name, 0); /* sanity-check */
    }
    if (!pass) {
        HDprintf(failure_mssg);
        TEST_ERROR;
    }

    /* -------------------- */
    /* Standard cleanup */

    if (fapl_id != H5P_DEFAULT && fapl_id >= 0) {
        if (H5Pclose(fapl_id) == FAIL) {
            TEST_ERROR;
        }
    }
    if (H5Gclose(grp_id) == FAIL) {
        TEST_ERROR;
    }
    if (H5Fclose(file_id) == FAIL) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* TEST: Verify that the R/W and W/O files are identical */

    if (h5_compare_file_bytes(names.rw, names.wo) < 0) {
        TEST_ERROR;
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Fclose(file_id);
        (void)H5Gclose(grp_id);
        if (fapl_id != H5P_DEFAULT && fapl_id > 0) {
            (void)H5Pclose(fapl_id);
        }
    }
    H5E_END_TRY;
    return -1;
} /* end test_on_disk_zoo() */

/* ---------------------------------------------------------------------------
 * Function:    test_vanishing_datasets
 *
 * Purpose:     Verify behavior when writing to a file where data is deleted.
 *
 *              Each dataset is populated with the value of its suffix
 *              (dset5 is all fives).
 *
 *              Opens 0..15 create one new dataset each, '/dset[i]'.
 *              Opens 3..18 delete '/dset[1-3]'
 *
 *              Should end with no data in file.
 *
 * Return:      Success:  0
 *              Failure: -1
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
static int
test_vanishing_datasets(void)
{
    struct mirrortest_filenames names;
    hid_t                       file_id        = H5I_INVALID_HID;
    hid_t                       fapl_id        = H5I_INVALID_HID;
    hid_t                       dset_id        = H5I_INVALID_HID;
    hid_t                       dspace_id      = H5I_INVALID_HID;
    hid_t                       mirror_fapl_id = H5I_INVALID_HID;
    hsize_t                     dims[2]        = {DATABUFFER_SIZE, DATABUFFER_SIZE};
    uint32_t                    buf[DATABUFFER_SIZE][DATABUFFER_SIZE]; /* consider malloc? */
    H5G_info_t                  group_info;
    unsigned int                i, j, k;
    const unsigned int          max_loops       = 20;
    const unsigned int          max_at_one_time = 3;

    TESTING("Vanishing Datasets");

    /* -------------------- */
    /* Set up recurrent data (FAPL, dataspace) */

    /* Create FAPL for Splitter[sec2|mirror]
     */
    fapl_id = create_mirroring_split_fapl("vanishing", &names);
    if (H5I_INVALID_HID == fapl_id) {
        TEST_ERROR;
    }

    dspace_id = H5Screate_simple(2, dims, NULL);
    if (dspace_id < 0) {
        TEST_ERROR;
    }

    /* create file */
    file_id = H5Fcreate(names.rw, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }

    for (i = 0; i < max_loops; i++) {
        char namebuf[DSET_NAME_LEN + 1];

        /* deleting datasets */
        if (i >= max_at_one_time) {
            if (HDsnprintf(namebuf, DSET_NAME_LEN, "/dset%02d", (i - max_at_one_time)) > DSET_NAME_LEN) {
                TEST_ERROR;
            }
            if (H5Ldelete(file_id, namebuf, H5P_DEFAULT) < 0) {
                TEST_ERROR;
            }
        } /* end if deleting a dataset */

        /* writing datasets */
        if (i < (max_loops - max_at_one_time)) {
            if (HDsnprintf(namebuf, DSET_NAME_LEN, "/dset%02d", i) > DSET_NAME_LEN) {
                TEST_ERROR;
            }
            dset_id =
                H5Dcreate2(file_id, namebuf, H5T_STD_U32LE, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            if (H5I_INVALID_HID == dset_id) {
                TEST_ERROR;
            }

            for (j = 0; j < DATABUFFER_SIZE; j++) {
                for (k = 0; k < DATABUFFER_SIZE; k++) {
                    buf[j][k] = (uint32_t)i;
                }
            }

            if (H5Dwrite(dset_id, H5T_STD_U32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) {
                TEST_ERROR;
            }

            if (H5Dclose(dset_id) < 0) {
                TEST_ERROR;
            }
            dset_id = H5I_INVALID_HID;
        } /* end if writing a dataset */

    } /* end for dataset create-destroy cycles */

    if (H5Fclose(file_id) < 0) {
        TEST_ERROR;
    }
    file_id = H5I_INVALID_HID;

    /* verify there are no datasets in file */
    file_id = H5Fopen(names.rw, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file_id < 0) {
        TEST_ERROR;
    }
    if (H5Gget_info(file_id, &group_info) < 0) {
        TEST_ERROR;
    }
    if (group_info.nlinks > 0) {
        HDfprintf(stderr, "links in rw file: %" PRIuHSIZE "\n", group_info.nlinks);
        HDfflush(stderr);
        TEST_ERROR;
    }
    if (H5Fclose(file_id) < 0) {
        TEST_ERROR;
    }
    file_id = H5Fopen(names.wo, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file_id < 0) {
        TEST_ERROR;
    }
    if (H5Gget_info(file_id, &group_info) < 0) {
        TEST_ERROR;
    }
    if (group_info.nlinks > 0) {
        HDfprintf(stderr, "links in wo file: %" PRIuHSIZE "\n", group_info.nlinks);
        HDfflush(stderr);
        TEST_ERROR;
    }
    if (H5Fclose(file_id) < 0) {
        TEST_ERROR;
    }
    file_id = H5I_INVALID_HID;

    if (h5_compare_file_bytes(names.rw, names.wo) < 0)
        TEST_ERROR;

    /* -------------------- */
    /* Teardown */

    if (H5Sclose(dspace_id) < 0) {
        TEST_ERROR;
    }
    if (H5Pclose(fapl_id) < 0) {
        TEST_ERROR;
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (mirror_fapl_id != H5I_INVALID_HID) {
            H5Pclose(mirror_fapl_id);
        }
        if (fapl_id != H5I_INVALID_HID) {
            H5Pclose(fapl_id);
        }
        if (file_id != H5I_INVALID_HID) {
            H5Fclose(file_id);
        }
        if (dset_id != H5I_INVALID_HID) {
            H5Dclose(dset_id);
        }
        if (dspace_id != H5I_INVALID_HID) {
            H5Sclose(dspace_id);
        }
    }
    H5E_END_TRY;
    return -1;
} /* test_vanishing_datasets() */

/* ---------------------------------------------------------------------------
 * Function:    test_concurrent_access
 *
 * Purpose:     Verify that more than one file may be opened at a time.
 *
 *              TODO: receive target IP from caller?
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Jacob Smith
 *              2020-03-09
 * ---------------------------------------------------------------------------
 */
static int
test_concurrent_access(void)
{
    struct file_bundle {
        struct mirrortest_filenames names;
        hid_t                       dset_id;
        hid_t                       fapl_id;
        hid_t                       file_id;
    } bundle[CONCURRENT_COUNT];
    hid_t   dspace_id = H5I_INVALID_HID;
    hid_t   dtype_id  = H5T_NATIVE_INT;
    hsize_t dims[2]   = {DATABUFFER_SIZE, DATABUFFER_SIZE};
    int *   buf       = NULL;
    int     i         = 0;
    int     j         = 0;

    TESTING("Concurrent opened mirrored files");

    /* blank bundle */
    for (i = 0; i < CONCURRENT_COUNT; i++) {
        bundle[i].dset_id    = H5I_INVALID_HID;
        bundle[i].fapl_id    = H5I_INVALID_HID;
        bundle[i].file_id    = H5I_INVALID_HID;
        *bundle[i].names.rw  = '\0';
        *bundle[i].names.wo  = '\0';
        *bundle[i].names.log = '\0';
    }

    /* Create FAPL for Splitter[sec2|mirror]
     */
    for (i = 0; i < CONCURRENT_COUNT; i++) {
        char  _name[16] = "";
        hid_t _fapl_id  = H5I_INVALID_HID;
        HDsnprintf(_name, 15, "concurrent%d", i);
        _fapl_id = create_mirroring_split_fapl(_name, &bundle[i].names);
        if (H5I_INVALID_HID == _fapl_id) {
            TEST_ERROR;
        }
        bundle[i].fapl_id = _fapl_id;
    }

    /* Prepare data to be written
     */
    buf = (int *)HDmalloc(DATABUFFER_SIZE * DATABUFFER_SIZE * sizeof(int));
    if (NULL == buf) {
        TEST_ERROR;
    }
    for (i = 0; i < DATABUFFER_SIZE; i++) {
        for (j = 0; j < DATABUFFER_SIZE; j++) {
            int k  = i * DATABUFFER_SIZE + j;
            buf[k] = k;
        }
    }

    /* Prepare generic dataspace
     */
    dspace_id = H5Screate_simple(2, dims, NULL);
    if (H5I_INVALID_HID == dspace_id) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* TEST: Create file and open elements */

    for (i = 0; i < CONCURRENT_COUNT; i++) {
        hid_t _file_id = H5I_INVALID_HID;
        hid_t _dset_id = H5I_INVALID_HID;

        _file_id = H5Fcreate(bundle[i].names.rw, H5F_ACC_TRUNC, H5P_DEFAULT, bundle[i].fapl_id);
        if (H5I_INVALID_HID == _file_id) {
            TEST_ERROR;
        }

        bundle[i].file_id = _file_id;

        _dset_id =
            H5Dcreate2(_file_id, "dataset", dtype_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if (H5I_INVALID_HID == _dset_id) {
            TEST_ERROR;
        }
        bundle[i].dset_id = _dset_id;
    }

    /* -------------------- */
    /* TEST: Write to files */

    for (i = 0; i < CONCURRENT_COUNT; i++) {
        if (H5Dwrite(bundle[i].dset_id, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) == FAIL) {
            TEST_ERROR;
        }
    }

    /* -------------------- */
    /* TEST: Close elements  */

    for (i = 0; i < CONCURRENT_COUNT; i++) {
        if (H5Dclose(bundle[i].dset_id) == FAIL) {
            TEST_ERROR;
        }
        if (H5Fclose(bundle[i].file_id) == FAIL) {
            TEST_ERROR;
        }
        if (H5Pclose(bundle[i].fapl_id) == FAIL) {
            TEST_ERROR;
        }
    }

    /* -------------------- */
    /* Standard cleanup */

    HDfree(buf);
    buf = NULL;
    if (H5Sclose(dspace_id) == FAIL) {
        TEST_ERROR;
    }

    /* -------------------- */
    /* TEST: Verify that the R/W and W/O files are identical */

    for (i = 0; i < CONCURRENT_COUNT; i++) {
        if (h5_compare_file_bytes(bundle[i].names.rw, bundle[i].names.wo) < 0) {
            TEST_ERROR;
        }
    }

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (buf) {
            HDfree(buf);
        }
        (void)H5Sclose(dspace_id);
        for (i = 0; i < CONCURRENT_COUNT; i++) {
            (void)H5Dclose(bundle[i].dset_id);
            (void)H5Fclose(bundle[i].file_id);
            (void)H5Pclose(bundle[i].fapl_id);
        }
    }
    H5E_END_TRY;
    return -1;
} /* end test_concurrent_access() */

/* ---------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run tests.
 *
 * Return:      Success: 0
 *              Failure: 1
 *
 * Programmer:  Jacob Smith
 *              2019
 * ---------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    h5_reset();

    g_log_stream = stdout; /* default debug/logging output stream */

    HDprintf("Testing Mirror VFD functionality.\n");

    /* -------------------- */
    /* SETUP */

    /* Create directories for test-generated .h5 files
     */
    if (nerrors == 0) {
        if ((HDmkdir(MIRROR_RW_DIR, (mode_t)0755) < 0) && (errno != EEXIST)) {
            nerrors++;
        }
    }
    if (nerrors == 0) {
        if ((HDmkdir(MIRROR_WO_DIR, (mode_t)0755) < 0) && (errno != EEXIST)) {
            nerrors++;
        }
    }

    /* -------------------- */
    /* TESTS */
    /* Tests return negative values; `-=' increments nerrors count */

    if (nerrors == 0) {
        nerrors -= test_fapl_configuration();
        nerrors -= test_xmit_encode_decode();
        nerrors -= test_create_and_close();
        nerrors -= test_basic_dataset_write();
        nerrors -= test_chunked_dataset_write();
        nerrors -= test_on_disk_zoo();
        nerrors -= test_vanishing_datasets();
        nerrors -= test_concurrent_access();
    }

    if (nerrors) {
        HDprintf("***** %d Mirror VFD TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    HDprintf("All Mirror Virtual File Driver tests passed.\n");
    return EXIT_SUCCESS;
} /* end main() */

#else /* H5_HAVE_MIRROR_VFD */

int
main(void)
{
    h5_reset();
    HDprintf("Testing Mirror VFD functionality.\n");
    HDprintf("SKIPPED - Mirror VFD not built.\n");
    return EXIT_SUCCESS;
}

#endif /* H5_HAVE_MIRROR_VFD */
