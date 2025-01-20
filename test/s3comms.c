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

/*
 * Read-Only S3 Virtual File Driver (VFD)
 *
 * Purpose:    Unit tests for the S3 Communications (s3comms) module.
 */

#include "h5test.h"

#define H5FD_S3COMMS_TESTING
#include "H5FDs3comms.h"

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
 * Function:    test_make_aws_canonical_request
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
test_make_aws_canonical_request(void)
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

    TESTING("make AWS canonical request");

    for (int i = 0; i < NCASES; i++) {
        C = &cases[i];

        for (int j = 0; j < 256; j++) {
            cr_dest[j] = 0;
        }

        for (int j = 0; j < 64; j++) {
            sh_dest[j] = 0;
        }

        /* Create HTTP request object with given verb, resource/path */
        hrb = H5FD__s3comms_hrb_init_request(C->verb, C->resource, "HTTP/1.1");
        assert(hrb->body == NULL);

        /* Create headers list from test case input */
        for (int j = 0; j < C->listsize; j++) {
            if (H5FD__s3comms_hrb_node_set(&node, C->list[j].name, C->list[j].value) < 0)
                TEST_ERROR;
        }

        hrb->first_header = node;

        /* Test */
        if (H5FD__s3comms_make_aws_canonical_request(cr_dest, 512, sh_dest, 64, hrb) < 0)
            TEST_ERROR;
        if (strncmp(C->exp_headers, sh_dest, 512))
            FAIL_PUTS_ERROR("header string mismatch");
        if (strncmp(C->exp_request, cr_dest, 512))
            FAIL_PUTS_ERROR("request string mismatch");

        /* Tear-down */
        while (node != NULL) {
            if (H5FD__s3comms_hrb_node_set(&node, node->name, NULL) < 0)
                TEST_ERROR;
        }
        if (H5FD__s3comms_hrb_destroy(hrb) < 0)
            TEST_ERROR;
    }

    /* ERROR CASES - Malformed hrb and/or node-list */
    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_make_aws_canonical_request(cr_dest, 20, sh_dest, 20, NULL);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("http request object cannot be null");

    hrb = H5FD__s3comms_hrb_init_request("GET", "/", "HTTP/1.1");
    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_make_aws_canonical_request(NULL, 20, sh_dest, 20, hrb);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("canonical request destination cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_make_aws_canonical_request(cr_dest, 20, NULL, 20, hrb);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("signed headers destination cannot be null");

    if (H5FD__s3comms_hrb_destroy(hrb) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:

    if (node != NULL) {
        while (node != NULL)
            H5FD__s3comms_hrb_node_set(&node, node->name, NULL);
    }
    H5FD__s3comms_hrb_destroy(hrb);

    return 1;

} /* end test_make_aws_canonical_request() */

/*---------------------------------------------------------------------------
 * Function:    test_hrb_init_request
 *
 * Purpose:     Define and verify behavior of `H5FD__s3comms_hrb_init_request()`
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

        req = H5FD__s3comms_hrb_init_request(C->verb, C->resource, C->version);

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
            if (H5FD__s3comms_hrb_destroy(req) < 0)
                FAIL_PUTS_ERROR("unable to destroy hrb_t");
        }
    }

    PASSED();
    return 0;

error:
    H5FD__s3comms_hrb_destroy(req);
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

            if (H5FD__s3comms_hrb_node_set(&list, name, value) < 0)
                TEST_ERROR;
        }

        /* TEST */

        /* Modify list */
        if (test->returned != H5FD__s3comms_hrb_node_set(&list, test->delta.name, test->delta.value))
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
            if (H5FD__s3comms_hrb_node_set(&list, list->name, NULL) < 0)
                TEST_ERROR;
        }
    }

    PASSED();
    return 0;

error:
    while (list != NULL) {
        H5FD__s3comms_hrb_node_set(&list, list->name, NULL);
    }

    return 1;

} /* end test_hrb_node_set() */

/* This is difficult to test since we took away the time parameter */
/*---------------------------------------------------------------------------
 * Function:    test_make_aws_signing_key
 *
 * Purpose:     Verify behavior of `H5FD__s3comms_make_aws_signing_key()`
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_make_aws_signing_key(void)
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

    TESTING("make AWS signing key");

    for (int i = 0; i < NCASES; i++) {
        if (NULL == (key = (unsigned char *)malloc(sizeof(unsigned char) * SHA256_DIGEST_LENGTH)))
            TEST_ERROR;

        if (H5FD__s3comms_make_aws_signing_key(key, cases[i].secret_key, cases[i].region, cases[i].when) < 0)
            TEST_ERROR;

        if (strncmp((const char *)cases[i].exp, (const char *)key, SHA256_DIGEST_LENGTH))
            TEST_ERROR;

        free(key);
        key = NULL;
    }

    PASSED();
    return 0;

error:
    free(key);
    return 1;
} /* end test_make_aws_signing_key() */

/*---------------------------------------------------------------------------
 * Function:    test_make_aws_stringtosign()
 *
 * Purpose:     Verify that we can get the "string to sign" from a Canonical
 *              Request and related information.
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_make_aws_stringtosign(void)
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

    TESTING("make AWS stringtosign");

    if (H5FD__s3comms_make_aws_stringtosign(s2s, canonreq, iso8601now, region) < 0)
        FAIL_PUTS_ERROR("unable to create string to sign");

    if (strncmp("AWS4-HMAC-SHA256\n20130524T000000Z\n20130524/us-east-1/s3/"
                "aws4_request\n7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972",
                s2s, 512))
        TEST_ERROR;

    /* ERROR CASES */

    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_make_aws_stringtosign(s2s, NULL, iso8601now, region);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("canonical request string cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_make_aws_stringtosign(s2s, canonreq, NULL, region);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("time string cannot be NULL");

    H5E_BEGIN_TRY
    {
        ret = H5FD__s3comms_make_aws_stringtosign(s2s, canonreq, iso8601now, NULL);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        FAIL_PUTS_ERROR("aws region cannot be NULL");

    PASSED();
    return 0;

error:
    return 1;

} /* end test_make_aws_stringtosign() */

/*---------------------------------------------------------------------------
 * Function:    test_s3r_get_filesize
 *
 * Purpose:     Test H5FD__s3comms_s3r_get_filesize()
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

    if (0 != H5FD__s3comms_s3r_get_filesize(NULL))
        FAIL_PUTS_ERROR("filesize of the null handle should be 0");

    if (NULL == (handle = H5FD__s3comms_s3r_open(url_raven, NULL, NULL)))
        TEST_ERROR;

    if (6464 != H5FD__s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("incorrect file size - fragile, make sure the file size didn't change");

    if (H5FD__s3comms_s3r_close(handle) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (handle != NULL)
        H5FD__s3comms_s3r_close(handle);
    return 1;
} /* end test_s3r_get_filesize() */

/*---------------------------------------------------------------------------
 * Function:    test_s3r_open
 *
 * Purpose:     Test H5FD__s3comms_s3r_open()
 *
 * Return:      PASS : 0
 *              FAIL : 1
 *---------------------------------------------------------------------------
 */
static int
test_s3r_open(void)
{
    char              url_missing[S3_TEST_MAX_URL_SIZE];
    char              url_raven[S3_TEST_MAX_URL_SIZE];
    char              url_shakespeare[S3_TEST_MAX_URL_SIZE];
    H5FD_ros3_fapl_t *fa     = NULL;
    s3r_t            *handle = NULL;

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

    /* Create and fill a common fapl
     *
     * Specific fields will be set (and reset) as needed by tests below
     */
    if (NULL == (fa = (H5FD_ros3_fapl_t *)calloc(1, sizeof(H5FD_ros3_fapl_t))))
        TEST_ERROR;
    fa->version      = H5FD_CURR_ROS3_FAPL_T_VERSION;
    fa->authenticate = true;
    strcpy(fa->aws_region, s3_test_aws_region);
    strcpy(fa->secret_id, s3_test_aws_access_key_id);
    strcpy(fa->secret_key, s3_test_aws_secret_access_key);

    if (S3_TEST_MAX_URL_SIZE < snprintf(url_shakespeare, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url,
                                        S3_TEST_RESOURCE_TEXT_RESTRICTED))
        TEST_ERROR;

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_missing, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_MISSING))
        TEST_ERROR;

    if (S3_TEST_MAX_URL_SIZE <
        snprintf(url_raven, S3_TEST_MAX_URL_SIZE, "%s/%s", s3_test_bucket_url, S3_TEST_RESOURCE_TEXT_PUBLIC))
        TEST_ERROR;

    /*************************
     * OPEN NONEXISTENT FILE *
     *************************/

    /* Attempt anonymously */
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_missing, NULL, NULL);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /* Attempt with authentication */
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_missing, fa, (const char *)s3_test_aws_security_token);
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
        handle = H5FD__s3comms_s3r_open(url_shakespeare, NULL, NULL);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;

    /* Pass in a bad ID */
    strcpy(fa->secret_id, "I_MADE_UP_MY_ID");
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_shakespeare, fa, (const char *)s3_test_aws_security_token);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;
    strcpy(fa->secret_id, s3_test_aws_access_key_id);

    /* Using an invalid signing key */
    strcpy(fa->secret_key, "I_AM_A_FAKE_KEY");
    H5E_BEGIN_TRY
    {
        handle = H5FD__s3comms_s3r_open(url_shakespeare, fa, (const char *)s3_test_aws_security_token);
    }
    H5E_END_TRY
    if (handle != NULL)
        TEST_ERROR;
    strcpy(fa->secret_key, s3_test_aws_secret_access_key);

    /*******************************
     * SUCCESSFUL OPEN (AND CLOSE) *
     *******************************/

    /* Anonymous */
    handle = H5FD__s3comms_s3r_open(url_raven, NULL, NULL);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD__s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD__s3comms_s3r_close(handle) < 0)
        TEST_ERROR;
    handle = NULL;

    /* Using authentication on anonymously-accessible file? */
    handle = H5FD__s3comms_s3r_open(url_raven, fa, (const char *)s3_test_aws_security_token);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD__s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD__s3comms_s3r_close(handle))
        TEST_ERROR;
    handle = NULL;

    /* Authenticating */
    handle = H5FD__s3comms_s3r_open(url_shakespeare, fa, (const char *)s3_test_aws_security_token);
    if (handle == NULL)
        TEST_ERROR;
    if (5458199 != H5FD__s3comms_s3r_get_filesize(handle))
        FAIL_PUTS_ERROR("did not get expected filesize");
    if (H5FD__s3comms_s3r_close(handle) < 0)
        TEST_ERROR;
    handle = NULL;

    free(fa);

    PASSED();
    return 0;
error:
    if (handle != NULL)
        H5FD__s3comms_s3r_close(handle);
    free(fa);

    return 1;
} /* end test_s3r_open() */

/*---------------------------------------------------------------------------
 * Function:    test_s3r_read
 *
 * Purpose:     Specify and demonstrate the use and life cycle of an S3
 *              request handle `s3r_t`, through its related functions.
 *
 *     H5FD__s3comms_s3r_open
 *     H5FD_s3comms_s3r_getsize << called by open() _only_
 *     H5FD__s3comms_s3r_read    << called by getsize(), multiple times working
 *     H5FD__s3comms_s3r_close
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

    TESTING("s3r_read");

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
    handle = H5FD__s3comms_s3r_open(url_raven, NULL, NULL);
    if (handle == NULL)
        TEST_ERROR;
    if (6464 != H5FD__s3comms_s3r_get_filesize(handle))
        TEST_ERROR;

    /*****************************
     * Tests that should succeed *
     *****************************/

    /* Read from start of file */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD__s3comms_s3r_read(handle, (haddr_t)0, (size_t)118, buffer) < 0)
        TEST_ERROR;
    if (strcmp("Once upon a midnight dreary, while I pondered, weak and weary,\n"
               "Over many a quaint and curious volume of forgotten lore",
               buffer))
        TEST_ERROR;

    /* Read arbitrary range */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD__s3comms_s3r_read(handle, (haddr_t)2540, (size_t)54, buffer) < 0)
        TEST_ERROR;
    if (strcmp("the grave and stern decorum of the countenance it wore", buffer))
        TEST_ERROR;

    /* Read one character */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD__s3comms_s3r_read(handle, (haddr_t)2540, (size_t)1, buffer) < 0)
        TEST_ERROR;
    if (strcmp("t", buffer))
        TEST_ERROR;

    /* Read to EOF */
    memset(buffer, 0, S3COMMS_READ_BUFFER_SIZE);
    if (H5FD__s3comms_s3r_read(handle, (haddr_t)6370, (size_t)0, buffer) < 0)
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
        ret = H5FD__s3comms_s3r_read(handle, (haddr_t)6400, (size_t)100, /* 6400+100 > 6464 */ buffer);
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
        ret = H5FD__s3comms_s3r_read(handle, (haddr_t)1200699, /* 1200699 > 6464 */ (size_t)100, buffer);
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
        ret = H5FD__s3comms_s3r_read(handle, (haddr_t)6464, (size_t)0, buffer);
    }
    H5E_END_TRY
    if (ret == SUCCEED)
        TEST_ERROR;
    if (strcmp("", buffer))
        TEST_ERROR;

    /*************
     * TEAR DOWN *
     *************/

    if (H5FD__s3comms_s3r_close(handle) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (handle != NULL)
        H5FD__s3comms_s3r_close(handle);
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

    /* TODO: unit/regression test for H5FD__s3comms_load_aws_profile()
     * requires a few test files and/or manipulation of default path
     */

    /* attempt to load test credentials
     * if unable, certain tests will be skipped
     */
    if (SUCCEED == H5FD__s3comms_load_aws_profile(S3_TEST_PROFILE_NAME, s3_test_aws_access_key_id,
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
        s3_test_bucket_url[S3_TEST_MAX_URL_SIZE - 1] = '\0';
        s3_test_bucket_defined                       = true;
    }

    curl_global_init(CURL_GLOBAL_DEFAULT);

    nerrors += test_macro_format_credential();
    nerrors += test_make_aws_canonical_request();
    nerrors += test_hrb_init_request();
    nerrors += test_hrb_node_set();
    nerrors += test_make_aws_signing_key();
    nerrors += test_make_aws_stringtosign();

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
