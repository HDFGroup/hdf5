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
 * Read-Only S3 Virtual File Driver (VFD)
 *
 * Purpose:    Unit tests for the S3 Communications (s3comms) module.
 */

#include "h5test.h"
#include "H5FDs3comms.h"
#include "H5MMprivate.h" /* memory management */

#ifdef H5_HAVE_ROS3_VFD

#define S3_TEST_PROFILE_NAME "ros3_vfd_test"

#define S3_TEST_RESOURCE_TEXT_RESTRICTED "t8.shakespeare.txt"
#define S3_TEST_RESOURCE_TEXT_PUBLIC     "Poe_Raven.txt"
#define S3_TEST_RESOURCE_MISSING         "missing.csv"

/* URL max size */
#define S3_TEST_MAX_URL_SIZE 256

/* Read buffer max size */
#define S3COMMS_READ_BUFFER_SIZE 256

/* Global variables for AWS test profile.
 *
 * An attempt is made to read ~/.aws/credentials and ~/.aws/config upon test
 * startup -- if unable to open either file or cannot load region, id, and key,
 * tests connecting with S3 will not be run.
 */
static int  s3_test_credentials_loaded               = 0;
static char s3_test_aws_region[16]                   = "";
static char s3_test_aws_access_key_id[64]            = "";
static char s3_test_aws_secret_access_key[128]       = "";
static char s3_test_aws_security_token[1024]         = "";
static char s3_test_bucket_url[S3_TEST_MAX_URL_SIZE] = "";
static bool s3_test_bucket_defined                   = false;

/*---------------------------------------------------------------------------
 * Function:    test_macro_format_credential
 *
 * Purpose:     Demonstrate that the macro `S3COMMS_FORMAT_CREDENTIAL`
 *              performs as expected.
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *----------------------------------------------------------------------------
 */
static int
test_macro_format_credential(void)
{
    char       dest[256];
    const char access[]   = "AKIAIOSFODNN7EXAMPLE";
    const char date[]     = "20130524";
    const char region[]   = "us-east-1";
    const char service[]  = "s3";
    const char expected[] = "AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request";

    TESTING("test_macro_format_credential");

    if (S3COMMS_MAX_CREDENTIAL_SIZE < S3COMMS_FORMAT_CREDENTIAL(dest, access, date, region, service))
        FAIL_PUTS_ERROR("bad s3comms credential size");

    if (strncmp(expected, dest, 256))
        FAIL_PUTS_ERROR("incorrect credential string");

    PASSED();
    return 0;

error:
    return 1;
} /* end test_macro_format_credential() */

/*---------------------------------------------------------------------------
 * Function:    test_aws_canonical_request
 *
 * Purpose:     Demonstrate the construction of a Canonical Request (and
 *              Signed Headers)
 *
 *     Elided / not yet implemented:
 *         Query strings
 *         request "body"
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_aws_canonical_request(void)
{
    struct header {
        const char *name;
        const char *value;
    };

    struct testcase {
        const char   *exp_request;
        const char   *exp_headers;
        const char   *verb;
        const char   *resource;
        int           listsize;
        struct header list[5];
    };

    struct testcase cases[] = {
        {
            "GET\n/some/"
            "path.file\n\nhost:somebucket.someserver.somedomain\nrange:bytes=150-244\n\nhost;"
            "range\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
            "host;range",
            "GET",
            "/some/path.file",
            2,
            {
                {"Range", "bytes=150-244"},
                {"Host", "somebucket.someserver.somedomain"},
            },
        },
        {"HEAD\n/bucketpath/"
         "myfile.dat\n\nhost:place.domain\nx-amz-content-sha256:"
         "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\nx-amz-date:"
         "19411207T150803Z\n\nhost;x-amz-content-sha256;x-amz-"
         "date\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
         "host;x-amz-content-sha256;x-amz-date",
         "HEAD",
         "/bucketpath/myfile.dat",
         3,
         {
             {"x-amz-content-sha256", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"},
             {"host", "place.domain"},
             {"x-amz-date", "19411207T150803Z"},
         }},
        {
            "PUT\n/\n\n\n\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
            "",
            "PUT",
            "/",
            0,
            {
                {"", ""},
            }, /* unused; satisfies compiler */
        },
    }; /* struct testcase cases[] */

    struct testcase *C = NULL;
    char             cr_dest[512];  /* Canonical request */
    hrb_t           *hrb    = NULL; /* http request buffer object */
    hrb_node_t      *node   = NULL; /* http headers list pointer */
    const int        NCASES = 3;
    char             sh_dest[64]; /* Signed headers */
    herr_t           ret;

    TESTING("test_aws_canonical_request");

    for (int i = 0; i < NCASES; i++) {
        C = &cases[i];

        for (int j = 0; j < 256; j++) {
            cr_dest[j] = 0;
        }

        for (int j = 0; j < 64; j++) {
            sh_dest[j] = 0;
        }

        /* Create HTTP request object with given verb, resource/path */
        hrb = H5FD_s3comms_hrb_init_request(C->verb, C->resource, "HTTP/1.1");
        assert(hrb->body == NULL);

        /* Create headers list from test case input */
        for (int j = 0; j < C->listsize; j++) {
            if (H5FD_s3comms_hrb_node_set(&node, C->list[j].name, C->list[j].value) < 0)
                TEST_ERROR;
        }

        hrb->first_header = node;

        /* Test */
        if (H5FD_s3comms_aws_canonical_request(cr_dest, 512, sh_dest, 64, hrb) < 0)
            TEST_ERROR;
        if (strncmp(C->exp_headers, sh_dest, 512))
            FAIL_PUTS_ERROR("header string mismatch");
        if (strncmp(C->exp_request, cr_dest, 512))
            FAIL_PUTS_ERROR("request string mismatch");

        /* Tear-down */
        while (node != NULL) {
            if (H5FD_s3comms_hrb_node_set(&node, node->name, NULL) < 0)
                TEST_ERROR;
        }
        if (H5FD_s3comms_hrb_destroy(&hrb) < 0)
            TEST_ERROR;
    }

    /* ERROR CASES - Malformed hrb and/or node-list */
    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_aws_canonical_request(cr_dest, 20, sh_dest, 20, NULL);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("http request object cannot be null");

    hrb = H5FD_s3comms_hrb_init_request("GET", "/", "HTTP/1.1");
    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_aws_canonical_request(NULL, 20, sh_dest, 20, hrb);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("canonical request destination cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_aws_canonical_request(cr_dest, 20, NULL, 20, hrb);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("signed headers destination cannot be null");

    if (H5FD_s3comms_hrb_destroy(&hrb) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:

    if (node != NULL) {
        while (node != NULL)
            H5FD_s3comms_hrb_node_set(&node, node->name, NULL);
    }
    if (hrb != NULL) {
        H5FD_s3comms_hrb_destroy(&hrb);
    }

    return 1;

} /* end test_aws_canonical_request() */

/*---------------------------------------------------------------------------
 * Function:    test_bytes_to_hex
 *
 * Purpose:     Define and verify behavior of `H5FD_s3comms_bytes_to_hex()`.
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_bytes_to_hex(void)
{
    struct testcase {
        const char          exp[17]; /* in size * 2 + 1 for null terminator */
        const unsigned char in[8];
        size_t              size;
        bool                lower;
    };

    struct testcase cases[] = {
        {
            "52F3000C9A",
            {82, 243, 0, 12, 154},
            5,
            false,
        },
        {
            "009a0cf3005200", /* lowercase alphas */
            {0, 154, 12, 243, 0, 82, 0},
            7,
            true,
        },
        {
            "", {17, 63, 26, 56}, 0, false, /* irrelevant */
        },
    };
    const int NCASES = 3;
    char      out[17];
    herr_t    ret;

    TESTING("bytes-to-hex");

    for (int i = 0; i < NCASES; i++) {
        for (int out_off = 0; out_off < 17; out_off++) {
            out[out_off] = 0;
        }

        if (H5FD_s3comms_bytes_to_hex(out, cases[i].in, cases[i].size, cases[i].lower) < 0)
            TEST_ERROR;

        if (strncmp(cases[i].exp, out, 17))
            FAIL_PUTS_ERROR("incorrect bytes to hex conversion");
    }

    /* dest cannot be null */
    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_bytes_to_hex(NULL, (const unsigned char *)"nada", 5, false);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("dest parameter cannot be null");

    PASSED();
    return 0;

error:
    return 1;
} /* end test_bytes_to_hex() */

/*---------------------------------------------------------------------------
 * Function:    test_hrb_init_request
 *
 * Purpose:     Define and verify behavior of `H5FD_s3comms_hrb_init_request()`
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_hrb_init_request(void)
{
    struct testcase {
        const char  msg[64];
        const char *verb;
        const char *resource;
        const char *exp_res;
        const char *version;
        bool        ret_null;
    };

    struct testcase cases[] = {
        {
            "get HTTP request just as we provided",
            "GET",
            "/path/to/some/file",
            "/path/to/some/file",
            "HTTP/1.1",
            false,
        },
        {
            "null verb substitutes to GET",
            NULL,
            "/MYPATH/MYFILE.tiff",
            "/MYPATH/MYFILE.tiff",
            "HTTP/1.1",
            false,
        },
        {
            "demonstrate non-GET verb",
            "HEAD",
            "/MYPATH/MYFILE.tiff",
            "/MYPATH/MYFILE.tiff",
            "HTTP/1.1",
            false,
        },
        {
            "slash prepended to resource path, if necessary",
            NULL,
            "MYPATH/MYFILE.tiff",
            "/MYPATH/MYFILE.tiff",
            NULL,
            false,
        },
        {
            "null resource path causes problem",
            "GET",
            NULL,
            NULL,
            NULL,
            true,
        },
    };
    const int NCASES = 5;
    hrb_t    *req    = NULL;

    TESTING("hrb_init_request");

    for (int i = 0; i < NCASES; i++) {
        struct testcase *C = &cases[i];

        req = H5FD_s3comms_hrb_init_request(C->verb, C->resource, C->version);

        if (cases[i].ret_null == true) {
            if (req != NULL)
                TEST_ERROR;
        }
        else {
            if (req == NULL)
                TEST_ERROR;
            if (C->verb == NULL) {
                if (strcmp("GET", req->verb))
                    TEST_ERROR;
            }
            else {
                if (strcmp(req->verb, C->verb))
                    TEST_ERROR;
            }
            if (strcmp("HTTP/1.1", req->version))
                TEST_ERROR;
            if (strcmp(C->exp_res, req->resource))
                TEST_ERROR;
            if (req->first_header != NULL)
                TEST_ERROR;
            if (req->body != NULL)
                TEST_ERROR;
            if (0 != req->body_len)
                TEST_ERROR;
            if (H5FD_s3comms_hrb_destroy(&req) < 0)
                FAIL_PUTS_ERROR("unable to destroy hrb_t");
            /* Should annull pointer as well as free */
            if (NULL != req)
                TEST_ERROR;
        }
    }

    PASSED();
    return 0;

error:
    H5FD_s3comms_hrb_destroy(&req);
    return 1;
} /* end test_hrb_init_request() */

/*---------------------------------------------------------------------------
 * Function:    test_hrb_node_set
 *
 * Purpose:     Test operations on hrb_node_t structure
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_hrb_node_set(void)
{
    /* Bundle of name/value representing an hrb_node_t */
    typedef struct node_mock_t {
        const char *name;
        const char *value;
    } node_mock_t;

    /* bundle for a testcase
     *
     * `message`
     *     purpose of the testcase
     *
     * `delta`
     *     container for name and value strings to pass into node-set function
     *     to to modify the list.
     *
     * `returned`
     *     expected return value of node-set function
     *
     * `given`
     * `expected`
     *     string arrays representing the state of the list before and after
     *     modification. The number of strings must be even, with each name
     *     paired to a value. `NULL` terminates the list, with `{NULL}`
     *     representing the empty list.
     */
    typedef struct testcase {
        const char *message;
        node_mock_t delta;
        herr_t      returned;
        const char *given[11]; /* name/value pairs in array; NULL sentinel */
        const char *expected[11];
    } testcase;

    testcase cases[] = {
        {
            "cannot remove node from null list",
            {"Host", NULL},
            FAIL,
            {NULL},
            {NULL},
        },
        {
            "cannot create list with NULL field name",
            {NULL, "somevalue"},
            FAIL,
            {NULL},
            {NULL},
        },
        {
            "create a new list",
            {"Host", "somevalue"},
            SUCCEED,
            {NULL},
            {
                "Host",
                "somevalue",
                NULL,
            },
        },
        {
            "insert new node at head list",
            {"Host", "somevalue"},
            SUCCEED,
            {
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Host",
                "somevalue",
                "Range",
                "bytes=20-40",
                NULL,
            },
        },
        {
            "append new node at list end",
            {"x-amz-date", "somevalue"},
            SUCCEED,
            {
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Range",
                "bytes=20-40",
                "x-amz-date",
                "somevalue",
                NULL,
            },
        },
        {
            "insert new node inside list",
            {"Intermediary", "somevalue"},
            SUCCEED,
            {
                "Host",
                "somehost",
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Host",
                "somehost",
                "Intermediary",
                "somevalue",
                "Range",
                "bytes=20-40",
                NULL,
            },
        },
        {
            "modify node",
            {"Range", "bytes=40-80"},
            SUCCEED,
            {
                "Host",
                "somehost",
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Host",
                "somehost",
                "Range",
                "bytes=40-80",
                NULL,
            },
        },
        {
            "modify node with new case",
            {"RANGE", "bytes=40-80"},
            SUCCEED,
            {
                "Host",
                "somehost",
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Host",
                "somehost",
                "RANGE",
                "bytes=40-80",
                NULL,
            },
        },
        {
            "cannot add node with no name",
            {NULL, "bytes=40-80"},
            FAIL,
            {
                "Host",
                "somehost",
                NULL,
            },
            {
                "Host",
                "somehost",
                NULL,
            },
        },
        {
            "add node with 'empty' name",
            {"", "bytes=40-80"},
            SUCCEED,
            {
                "Host",
                "somehost",
                NULL,
            },
            {
                "",
                "bytes=40-80",
                "Host",
                "somehost",
                NULL,
            },
        },
        {
            "remove node from end of list",
            {"Host", NULL},
            SUCCEED,
            {
                "Date",
                "Thr, 25 Jan 2018",
                "Host",
                "somehost",
                NULL,
            },
            {
                "Date",
                "Thr, 25 Jan 2018",
                NULL,
            },
        },
        {
            "remove node from middle of list",
            {"Host", NULL},
            SUCCEED,
            {
                "Date",
                "Thr, 25 Jan 2018",
                "Host",
                "somehost",
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Date",
                "Thr, 25 Jan 2018",
                "Range",
                "bytes=20-40",
                NULL,
            },
        },
        {
            "remove node from start of list",
            {"Date", NULL},
            SUCCEED,
            {
                "Date",
                "Thr, 25 Jan 2018",
                "Host",
                "somehost",
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Host",
                "somehost",
                "Range",
                "bytes=20-40",
                NULL,
            },
        },
        {
            "remove only node in list",
            {"Date", NULL},
            SUCCEED,
            {
                "Date",
                "Thr, 25 Jan 2018",
                NULL,
            },
            {
                NULL,
            },
        },
        {
            "attempt to remove absent node fails",
            {"Host", NULL},
            FAIL,
            {
                "Date",
                "Thr, 25 Jan 2018",
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Date",
                "Thr, 25 Jan 2018",
                "Range",
                "bytes=20-40",
                NULL,
            },
        },
        {
            "removal is case-insensitive",
            {"hOsT", NULL},
            SUCCEED,
            {
                "Date",
                "Thr, 25 Jan 2018",
                "Host",
                "somehost",
                "Range",
                "bytes=20-40",
                NULL,
            },
            {
                "Date",
                "Thr, 25 Jan 2018",
                "Range",
                "bytes=20-40",
                NULL,
            },
        },
    };
    const int   NCASES = 16;
    hrb_node_t *list   = NULL;

    TESTING("hrb_node_t (test_hrb_node_set)");

    for (int i = 0; i < NCASES; i++) {
        const hrb_node_t *node   = NULL;
        const testcase   *test   = &(cases[i]);
        unsigned          mock_i = 0;

        /* SETUP */

        for (mock_i = 0; test->given[mock_i] != NULL; mock_i += 2) {
            const char *name  = test->given[mock_i];
            const char *value = test->given[mock_i + 1];

            if (H5FD_s3comms_hrb_node_set(&list, name, value) < 0)
                TEST_ERROR;
        }

        /* TEST */

        /* Modify list */
        if (test->returned != H5FD_s3comms_hrb_node_set(&list, test->delta.name, test->delta.value))
            FAIL_PUTS_ERROR(test->message);

        /* Verify resulting list */
        node   = list;
        mock_i = 0;
        while (test->expected[mock_i] != NULL && node != NULL) {
            const char *name  = test->expected[mock_i];
            const char *value = test->expected[mock_i + 1];

            if (strcmp(name, node->name))
                TEST_ERROR;
            if (strcmp(value, node->value))
                TEST_ERROR;

            mock_i += 2;
            node = node->next;
        }
        if (test->expected[mock_i] != NULL)
            TEST_ERROR;
        if (node != NULL)
            TEST_ERROR;

        /* TEARDOWN */

        while (list != NULL) {
            if (H5FD_s3comms_hrb_node_set(&list, list->name, NULL) < 0)
                TEST_ERROR;
        }
    }

    PASSED();
    return 0;

error:
    while (list != NULL) {
        H5FD_s3comms_hrb_node_set(&list, list->name, NULL);
    }

    return 1;

} /* end test_hrb_node_set() */

/*---------------------------------------------------------------------------
 * Function:    test_HMAC_SHA256
 *
 * Purpose:     Define and verify behavior of `H5FD_s3comms_HMAC_SHA256()`
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_HMAC_SHA256(void)
{
    struct testcase {
        herr_t              ret; /* SUCCEED/FAIL expected from call */
        const unsigned char key[SHA256_DIGEST_LENGTH];
        size_t              key_len;
        const char         *msg;
        size_t              msg_len;
        const char         *exp;       /* not used if ret == FAIL */
        size_t              dest_size; /* if 0, `dest` is not malloc'd */
    };

    struct testcase cases[] = {
        {
            SUCCEED,
            {
                0xdb, 0xb8, 0x93, 0xac, 0xc0, 0x10, 0x96, 0x49, 0x18, 0xf1, 0xfd,
                0x43, 0x3a, 0xdd, 0x87, 0xc7, 0x0e, 0x8b, 0x0d, 0xb6, 0xbe, 0x30,
                0xc1, 0xfb, 0xea, 0xfe, 0xfa, 0x5e, 0xc6, 0xba, 0x83, 0x78,
            },
            SHA256_DIGEST_LENGTH,
            "AWS4-HMAC-SHA256\n20130524T000000Z\n20130524/us-east-1/s3/"
            "aws4_request\n7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972",
            strlen("AWS4-HMAC-SHA256\n20130524T000000Z\n20130524/us-east-1/s3/"
                   "aws4_request\n7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972"),
            "f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41",
            SHA256_DIGEST_LENGTH * 2 + 1, /* +1 for null terminator */
        },
        {
            SUCCEED,
            {'J', 'e', 'f', 'e'},
            4,
            "what do ya want for nothing?",
            28,
            "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843",
            SHA256_DIGEST_LENGTH * 2 + 1,
        },
        {
            FAIL, "DOESN'T MATTER", 14, "ALSO IRRELEVANT", 15, NULL,
            0, /* dest -> null, resulting in immediate error */
        },
    };
    char     *dest   = NULL;
    const int NCASES = 3;

    TESTING("HMAC_SHA256");

    for (int i = 0; i < NCASES; i++) {

        if (cases[i].dest_size == 0) {
            dest = NULL;
        }
        else {
            if (NULL == (dest = (char *)malloc(sizeof(char) * cases[i].dest_size)))
                TEST_ERROR;
        }

        if (cases[i].ret !=
            H5FD_s3comms_HMAC_SHA256(cases[i].key, cases[i].key_len, cases[i].msg, cases[i].msg_len, dest))
            TEST_ERROR;

        if (cases[i].ret == SUCCEED) {
            if (strncmp(cases[i].exp, dest, strlen(cases[i].exp)))
                TEST_ERROR;
        }
        free(dest);
    }

    PASSED();
    return 0;

error:
    free(dest);
    return 1;

} /* end test_HMAC_SHA256() */

/*---------------------------------------------------------------------------
 * Function: test_parse_url
 *
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_parse_url(void)
{
    typedef struct {
        const char *scheme;
        const char *host;
        const char *port;
        const char *path;
        const char *query;
    } const_purl_t;

    struct testcase {
        const char  *url;
        herr_t       exp_ret;  /* expected return */
        const_purl_t expected; /* unused if exp_ret is FAIL */
        const char  *msg;
    };

    parsed_url_t   *purl    = NULL;
    const int       NCASES  = 15;
    struct testcase cases[] = {
        {
            NULL,
            FAIL,
            {NULL, NULL, NULL, NULL, NULL},
            "null url",
        },
        {
            "",
            FAIL,
            {NULL, NULL, NULL, NULL, NULL},
            "empty url",
        },
        {
            "ftp://[1000:4000:0002:2010]",
            SUCCEED,
            {
                "ftp",
                "[1000:4000:0002:2010]",
                NULL,
                NULL,
                NULL,
            },
            "IPv6 ftp and empty path (root)",
        },
        {
            "ftp://[1000:4000:0002:2010]:2040",
            SUCCEED,
            {
                "ftp",
                "[1000:4000:0002:2010]",
                "2040",
                NULL,
                NULL,
            },
            "root IPv6 ftp with port",
        },
        {
            "http://some.domain.org:9000/path/to/resource.txt",
            SUCCEED,
            {
                "http",
                "some.domain.org",
                "9000",
                "path/to/resource.txt",
                NULL,
            },
            "without query",
        },
        {
            "https://domain.me:00/file.txt?some_params unchecked",
            SUCCEED,
            {
                "https",
                "domain.me",
                "00",
                "file.txt",
                "some_params unchecked",
            },
            "with query",
        },
        {
            "ftp://domain.com/",
            SUCCEED,
            {
                "ftp",
                "domain.com",
                NULL,
                NULL,
                NULL,
            },
            "explicit root w/out port",
        },
        {
            "ftp://domain.com:1234/",
            SUCCEED,
            {
                "ftp",
                "domain.com",
                "1234",
                NULL,
                NULL,
            },
            "explicit root with port",
        },
        {
            "ftp://domain.com:1234/file?",
            FAIL,
            {
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
            },
            "empty query is invalid",
        },
        {
            "ftp://:1234/file",
            FAIL,
            {
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
            },
            "no host",
        },
        {
            "h&r block",
            FAIL,
            {
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
            },
            "no scheme (bad URL)",
        },
        {
            "http://domain.com?a=b&d=b",
            SUCCEED,
            {
                "http",
                "domain.com",
                NULL,
                NULL,
                "a=b&d=b",
            },
            "QUERY with implicit PATH",
        },
        {
            "http://[5]/path?a=b&d=b",
            SUCCEED,
            {
                "http",
                "[5]",
                NULL,
                "path",
                "a=b&d=b",
            },
            "IPv6 extraction is really dumb",
        },
        {
            "http://[1234:5678:0910:1112]:port/path",
            FAIL,
            {
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
            },
            "non-decimal PORT (port)",
        },
        {
            "http://mydomain.com:01a3/path",
            FAIL,
            {
                NULL,
                NULL,
                NULL,
                NULL,
                NULL,
            },
            "non-decimal PORT (01a3)",
        },
    };

    TESTING("url-parsing functionality");

    /*********
     * TESTS *
     *********/

    for (int i = 0; i < NCASES; i++) {

        if (cases[i].exp_ret != H5FD_s3comms_parse_url(cases[i].url, &purl))
            TEST_ERROR;

        if (cases[i].exp_ret == FAIL) {
            /* On FAIL, `purl` should be untouched--remains NULL */
            if (purl != NULL)
                TEST_ERROR;
        }
        else {
            /* On SUCCEED, `purl` should be set */
            if (purl == NULL)
                TEST_ERROR;

            if (cases[i].expected.scheme != NULL) {
                if (NULL == purl->scheme)
                    TEST_ERROR;
                if (strcmp(cases[i].expected.scheme, purl->scheme))
                    TEST_ERROR;
            }
            else {
                if (NULL != purl->scheme)
                    TEST_ERROR;
            }

            if (cases[i].expected.host != NULL) {
                if (NULL == purl->host)
                    TEST_ERROR;
                if (strcmp(cases[i].expected.host, purl->host))
                    TEST_ERROR;
            }
            else {
                if (NULL != purl->host)
                    TEST_ERROR;
            }

            if (cases[i].expected.port != NULL) {
                if (NULL == purl->port)
                    TEST_ERROR;
                if (strcmp(cases[i].expected.port, purl->port))
                    TEST_ERROR;
            }
            else {
                if (NULL != purl->port)
                    TEST_ERROR;
            }

            if (cases[i].expected.path != NULL) {
                if (NULL == purl->path)
                    TEST_ERROR;
                if (strcmp(cases[i].expected.path, purl->path))
                    TEST_ERROR;
            }
            else {
                if (NULL != purl->path)
                    TEST_ERROR;
            }

            if (cases[i].expected.query != NULL) {
                if (NULL == purl->query)
                    TEST_ERROR;
                if (strcmp(cases[i].expected.query, purl->query))
                    TEST_ERROR;
            }
            else {
                if (NULL != purl->query)
                    TEST_ERROR;
            }
        }

        if (H5FD_s3comms_free_purl(purl) < 0)
            TEST_ERROR;

        purl = NULL;
    }

    PASSED();
    return 0;

error:
    H5FD_s3comms_free_purl(purl);

    return 1;

} /* end test_parse_url() */

/*---------------------------------------------------------------------------
 * Function:    test_signing_key
 *
 * Purpose:     Verify behavior of `H5FD_s3comms_signing_key()`
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_signing_key(void)
{
    struct testcase {
        const char   *region;
        const char   *secret_key;
        const char   *when;
        unsigned char exp[SHA256_DIGEST_LENGTH];
    };

    struct testcase cases[] = {
        {
            "us-east-1",
            "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
            "20130524T000000Z",
            {
                0xdb, 0xb8, 0x93, 0xac, 0xc0, 0x10, 0x96, 0x49, 0x18, 0xf1, 0xfd,
                0x43, 0x3a, 0xdd, 0x87, 0xc7, 0x0e, 0x8b, 0x0d, 0xb6, 0xbe, 0x30,
                0xc1, 0xfb, 0xea, 0xfe, 0xfa, 0x5e, 0xc6, 0xba, 0x83, 0x78,
            },
        },
    };

    unsigned char *key    = NULL;
    const int      NCASES = 1;
    herr_t         ret;

    TESTING("signing_key");

    for (int i = 0; i < NCASES; i++) {
        if (NULL == (key = (unsigned char *)malloc(sizeof(unsigned char) * SHA256_DIGEST_LENGTH)))
            TEST_ERROR;

        if (H5FD_s3comms_signing_key(key, cases[i].secret_key, cases[i].region, cases[i].when) < 0)
            TEST_ERROR;

        if (strncmp((const char *)cases[i].exp, (const char *)key, SHA256_DIGEST_LENGTH))
            TEST_ERROR;

        free(key);
        key = NULL;
    }

    /* ERROR CASES */

    if (NULL == (key = (unsigned char *)malloc(sizeof(unsigned char) * SHA256_DIGEST_LENGTH)))
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_signing_key(NULL, cases[0].secret_key, cases[0].region, cases[0].when);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("destination cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_signing_key(key, NULL, cases[0].region, cases[0].when);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("secret key cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_signing_key(key, cases[0].secret_key, NULL, cases[0].when);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("aws region cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_signing_key(key, cases[0].secret_key, cases[0].region, NULL);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("time string cannot be NULL");

    free(key);

    PASSED();
    return 0;

error:
    free(key);
    return 1;
} /* end test_signing_key() */

/*---------------------------------------------------------------------------
 * Function:    test_tostringtosign()
 *
 * Purpose:     Verify that we can get the "string to sign" from a Canonical
 *              Request and related information.
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_tostringtosign(void)
{
    const char canonreq[]   = "GET\n/"
                              "test.txt\n\nhost:examplebucket.s3.amazonaws.com\nrange:bytes=0-9\nx-amz-content-"
                              "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\nx-amz-"
                              "date:20130524T000000Z\n\nhost;range;x-amz-content-sha256;x-amz-"
                              "date\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
    const char iso8601now[] = "20130524T000000Z";
    const char region[]     = "us-east-1";
    char       s2s[512];
    herr_t     ret;

    TESTING("s3comms tostringtosign");

    if (H5FD_s3comms_tostringtosign(s2s, canonreq, iso8601now, region) < 0)
        FAIL_PUTS_ERROR("unable to create string to sign");

    if (strncmp("AWS4-HMAC-SHA256\n20130524T000000Z\n20130524/us-east-1/s3/"
                "aws4_request\n7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972",
                s2s, 512))
        TEST_ERROR;

    /* ERROR CASES */

    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_tostringtosign(s2s, NULL, iso8601now, region);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("canonical request string cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_tostringtosign(s2s, canonreq, NULL, region);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("time string cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_tostringtosign(s2s, canonreq, iso8601now, NULL);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("aws region cannot be NULL");

    PASSED();
    return 0;

error:
    return 1;

} /* end test_tostringtosign() */

/*---------------------------------------------------------------------------
 * Function:    test_s3r_get_filesize
 *
 * Purpose:     Test H5FD_s3comms_s3r_get_filesize()
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_s3r_get_filesize(void)
{
    char   url_raven[S3_TEST_MAX_URL_SIZE];
    s3r_t *handle = NULL;

    TESTING("s3r_get_filesize");

    /* Setup -- compose url to target resource */
    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_raven, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_TEXT_PUBLIC))
        TEST_ERROR;

    if (0 != H5FD_s3comms_s3r_get_filesize(NULL))
        FAIL_PUTS_ERROR("filesize of the null handle should be 0");

    if (NULL == (handle = H5FD_s3comms_s3r_open(url_raven, NULL, NULL, NULL, NULL)))
        TEST_ERROR;

    if (6464 != H5FD_s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("incorrect file size - fragile, make sure the file size didn't change");

    if (H5FD_s3comms_s3r_close(handle) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (handle != NULL)
        H5FD_s3comms_s3r_close(handle);
    return 1;
} /* end test_s3r_get_filesize() */

/*---------------------------------------------------------------------------
 * Function:    test_s3r_open
 *
 * Purpose:     Test H5FD_s3comms_s3r_open()
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_s3r_open(void)
{
    char          url_missing[S3_TEST_MAX_URL_SIZE];
    char          url_raven[S3_TEST_MAX_URL_SIZE];
    char          url_shakespeare[S3_TEST_MAX_URL_SIZE];
    unsigned char signing_key[SHA256_DIGEST_LENGTH];
    struct tm    *now = NULL;
    char          iso8601now[ISO8601_SIZE];
    s3r_t        *handle = NULL;
    parsed_url_t *purl   = NULL;

    TESTING("s3r_open");

    if (s3_test_credentials_loaded == 0) {
        SKIPPED();
        puts("    s3 credentials are not loaded");
        fflush(stdout);
        return 0;
    }
    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    /******************
     * PRE-TEST SETUP *
     ******************/

    if (S3_TEST_MAX_URL_SIZE < snprintf(url_shakespeare, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url,
                                        S3_TEST_RESOURCE_TEXT_RESTRICTED))
        TEST_ERROR;

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_missing, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_MISSING))
        TEST_ERROR;

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_raven, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_TEXT_PUBLIC))
        TEST_ERROR;

    /* Set given bucket url with invalid/inactive port number for badport.
     * Note, this sort of micro-management of parsed_url_t is not advised
     */
    if (H5FD_s3comms_parse_url(s3_test_bucket_url, &purl) < 0)
        TEST_ERROR;

    if (purl->port == NULL) {
        if (NULL == (purl->port = (char *)H5MM_malloc(sizeof(char) * 5)))
            TEST_ERROR;
        if (5 < snprintf(purl->port, 5, "9000"))
            TEST_ERROR;
    }
    else if (strcmp(purl->port, "9000") != 0) {
        if (5 < snprintf(purl->port, 5, "9000"))
            TEST_ERROR;
    }
    else {
        if (5 < snprintf(purl->port, 5, "1234"))
            TEST_ERROR;
    }

    if (NULL == (now = gmnow()))
        TEST_ERROR;
    if (ISO8601NOW(iso8601now, now) != (ISO8601_SIZE - 1))
        TEST_ERROR;

    /* It is desired to have means available to verify that signing_key
     * was set successfully and to an expected value.
     */
    if (H5FD_s3comms_signing_key(signing_key, (const char *)s3_test_aws_secret_access_key,
                                 (const char *)s3_test_aws_region, (const char *)iso8601now) < 0)
        TEST_ERROR;

    /*************************
     * OPEN NONEXISTENT FILE *
     *************************/

    /* Attempt anonymously */
    H5E_BEGIN_TRY
    {
        handle = H5FD_s3comms_s3r_open(url_missing, NULL, NULL, NULL, NULL);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /* Attempt with authentication */
    H5E_BEGIN_TRY
    {
        handle = H5FD_s3comms_s3r_open(
            url_missing, (const char *)s3_test_aws_region, (const char *)s3_test_aws_access_key_id,
            (const unsigned char *)signing_key, (const char *)s3_test_aws_security_token);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /*******************************
     * INVALID AUTHENTICATION INFO *
     *******************************/

    /* Anonymous access on restricted file */
    H5E_BEGIN_TRY
    {
        handle = H5FD_s3comms_s3r_open(url_shakespeare, NULL, NULL, NULL, NULL);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /* Pass in a bad ID */
    H5E_BEGIN_TRY
    {
        handle = H5FD_s3comms_s3r_open(url_shakespeare, (const char *)s3_test_aws_region, "I_MADE_UP_MY_ID",
                                       (const unsigned char *)signing_key,
                                       (const char *)s3_test_aws_security_token);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /* Using an invalid signing key */
    H5E_BEGIN_TRY
    {
        handle = H5FD_s3comms_s3r_open(
            url_shakespeare, (const char *)s3_test_aws_region, (const char *)s3_test_aws_access_key_id,
            (const unsigned char *)EMPTY_SHA256, (const char *)s3_test_aws_security_token);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /*******************************
     * SUCCESSFUL OPEN (AND CLOSE) *
     *******************************/

    /* Anonymous */
    handle = H5FD_s3comms_s3r_open(url_raven, NULL, NULL, NULL, NULL);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD_s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD_s3comms_s3r_close(handle) < 0)
        TEST_ERROR;
    handle = NULL;

    /* Using authentication on anonymously-accessible file? */
    handle = H5FD_s3comms_s3r_open(
        url_raven, (const char *)s3_test_aws_region, (const char *)s3_test_aws_access_key_id,
        (const unsigned char *)signing_key, (const char *)s3_test_aws_security_token);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD_s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD_s3comms_s3r_close(handle))
        TEST_ERROR;
    handle = NULL;

    /* Authenticating */
    handle = H5FD_s3comms_s3r_open(
        url_shakespeare, (const char *)s3_test_aws_region, (const char *)s3_test_aws_access_key_id,
        (const unsigned char *)signing_key, (const char *)s3_test_aws_security_token);
    if (handle == NULL)
        TEST_ERROR;
    if (5458199 != H5FD_s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD_s3comms_s3r_close(handle) < 0)
        TEST_ERROR;
    handle = NULL;

    if (H5FD_s3comms_free_purl(purl) < 0)
        TEST_ERROR;

    PASSED();
    return 0;
error:
    if (handle != NULL)
        H5FD_s3comms_s3r_close(handle);
    if (purl != NULL)
        H5FD_s3comms_free_purl(purl);

    return 1;
} /* end test_s3r_open() */

/*---------------------------------------------------------------------------
 * Function:    test_s3r_read
 *
 * Purpose:     Specify and demonstrate the use and life cycle of an S3
 *              request handle `s3r_t`, through its related functions.
 *
 *     H5FD_s3comms_s3r_open
 *     H5FD_s3comms_s3r_getsize << called by open() _only_
 *     H5FD_s3comms_s3r_read    << called by getsize(), multiple times working
 *     H5FD_s3comms_s3r_close
 *
 *     Shows most basic curl iteration
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_s3r_read(void)
{
    char   url_raven[S3_TEST_MAX_URL_SIZE];
    char   buffer[S3COMMS_READ_BUFFER_SIZE];
    s3r_t *handle = NULL;
    herr_t ret;

    TESTING("test_s3r_read");

    /* Initial setup */
    if (false == s3_test_bucket_defined) {
        SKIPPED();
        puts("    environment variable HDF5_ROS3_TEST_BUCKET_URL not defined");
        fflush(stdout);
        return 0;
    }

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_raven, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_TEXT_PUBLIC))
        TEST_ERROR;

    /* Open file */
    handle = H5FD_s3comms_s3r_open(url_raven, NULL, NULL, NULL, NULL);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD_s3comms_s3r_get_filesize(handle))
        TEST_ERROR;

    /*****************************
     * Tests that should succeed *
     *****************************/

    /* Read from start of file */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD_s3comms_s3r_read(handle, (haddr_t)0, (size_t)118, buffer) < 0)
        TEST_ERROR;
    if (strcmp("Once upon a midnight dreary, while I pondered, weak and weary,\n"
               "Over many a quaint and curious volume of forgotten lore",
               buffer))
        TEST_ERROR;

    /* Read arbitrary range */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD_s3comms_s3r_read(handle, (haddr_t)2540, (size_t)54, buffer) < 0)
        TEST_ERROR;
    if (strcmp("the grave and stern decorum of the countenance it wore", buffer))
        TEST_ERROR;

    /* Read one character */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD_s3comms_s3r_read(handle, (haddr_t)2540, (size_t)1, buffer) < 0)
        TEST_ERROR;
    if (strcmp("t", buffer))
        TEST_ERROR;

    /* Read to EOF */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD_s3comms_s3r_read(handle, (haddr_t)6370, (size_t)0, buffer) < 0)
        TEST_ERROR;
    if (strncmp(
            buffer,
            "And my soul from out that shadow that lies floating on the floor\nShall be lifted—nevermore!\n",
            94))
        TEST_ERROR;

    /**************************
     * Tests that should fail *
     **************************/

    /* Read past eof */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_s3r_read(handle, (haddr_t)6400, (size_t)100, /* 6400+100 > 6464 */ buffer);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        TEST_ERROR;
    if (strcmp("", buffer))
        TEST_ERROR;

    /* Read starts past eof */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_s3r_read(handle, (haddr_t)1200699, /* 1200699 > 6464 */ (size_t)100, buffer);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        TEST_ERROR;
    if (strcmp("", buffer))
        TEST_ERROR;

    /* Read starts on eof */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    H5E_BEGIN_TRY
    {
        ret = H5FD_s3comms_s3r_read(handle, (haddr_t)6464, (size_t)0, buffer);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        TEST_ERROR;
    if (strcmp("", buffer))
        TEST_ERROR;

    /*************
     * TEAR DOWN *
     *************/

    if (H5FD_s3comms_s3r_close(handle) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (handle != NULL)
        H5FD_s3comms_s3r_close(handle);
    return 1;
} /* end test_s3r_read() */

#endif /* H5_HAVE_ROS3_VFD */

/*-------------------------------------------------------------------------
 * Function:    main()
 *
 * Purpose:     Run unit tests for S3 communications (s3comms)
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
#ifdef H5_HAVE_ROS3_VFD
    int         nerrors        = 0;
    const char *bucket_url_env = NULL;

    h5_test_init();

#endif /* H5_HAVE_ROS3_VFD */

    printf("Testing S3 communications functionality\n");

#ifdef H5_HAVE_ROS3_VFD

    /* "clear" profile data strings */
    s3_test_aws_access_key_id[0]     = '\0';
    s3_test_aws_secret_access_key[0] = '\0';
    s3_test_aws_region[0]            = '\0';
    s3_test_bucket_url[0]            = '\0';

    /* TODO: unit/regression test for H5FD_s3comms_load_aws_profile()
     * requires a few test files and/or manipulation of default path
     */

    /* attempt to load test credentials
     * if unable, certain tests will be skipped
     */
    if (SUCCEED == H5FD_s3comms_load_aws_profile(S3_TEST_PROFILE_NAME, s3_test_aws_access_key_id,
                                                 s3_test_aws_secret_access_key, s3_test_aws_region)) {
        s3_test_credentials_loaded = 1;
    }

    bucket_url_env = getenv("HDF5_ROS3_TEST_BUCKET_URL");
    if (bucket_url_env == NULL || bucket_url_env[0] == '\0') {
        printf("WARNING: S3 bucket url is not defined in environment "
               "variable 'HDF5_ROS3_TEST_BUCKET_URL'!\n");
    }
    else {
        strncpy(s3_test_bucket_url, bucket_url_env, S3_TEST_MAX_URL_SIZE);
        s3_test_bucket_defined = true;
    }

    curl_global_init(CURL_GLOBAL_DEFAULT);

    nerrors += test_macro_format_credential();
    nerrors += test_aws_canonical_request();
    nerrors += test_bytes_to_hex();
    nerrors += test_hrb_init_request();
    nerrors += test_hrb_node_set();
    nerrors += test_HMAC_SHA256();
    nerrors += test_parse_url();
    nerrors += test_signing_key();
    nerrors += test_tostringtosign();

    nerrors += test_s3r_get_filesize();
    nerrors += test_s3r_open();
    nerrors += test_s3r_read();

    curl_global_cleanup();

    if (nerrors) {
        printf("***** %d s3comms TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    printf("All s3comms tests passed.\n");
    return EXIT_SUCCESS;

#else

    printf("SKIPPED - read-only S3 VFD not built\n");
    return EXIT_SUCCESS;

#endif /* H5_HAVE_ROS3_VFD */

} /* end main() */
