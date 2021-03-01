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
 * Purpose: unit-test functionality of the routines in `tools/lib/h5tools_utils`
 *
 * Jacob Smith 2017-11-10
 */

#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5test.h"

/* Selector for which test cases to run */
typedef enum vfd_tests_e { VFD_TESTS_COMMON, VFD_TESTS_ROS3, VFD_TESTS_HDFS } vfd_tests_e;

/* Whether we pass in H5I_INVALID_HID, H5P_DEFAULT, or a created fapl */
typedef enum fapl_choice_e { FAPL_CHOICE_INVALID, FAPL_CHOICE_DEFAULT, FAPL_CHOICE_CREATE } fapl_choice_e;

/* Test case data */
typedef struct fapl_testcase_t {
    const char    error_message[88];
    herr_t        expected_result;
    fapl_choice_e fapl_choice;
    const char    vfd_name[12];
    void *        fa;
} fapl_testcase_t;

/* Generic "incorrect" VFD struct */
typedef struct other_fa_t {
    int a;
    int b;
    int c;
} other_fa_t;

other_fa_t wrong_fa_g = {0x432, 0xf82, 0x9093};

/******************************/
/* Common test configurations */
/******************************/

fapl_testcase_t common_cases_g[] = {
    {
        "(common) H5I_INVALID_HID + NULL VFD struct (UNEXPECTED SUCCESS)",
        FAIL,
        FAPL_CHOICE_INVALID,
        "",
        NULL,
    },
    {
        "(common) H5I_INVALID_HID + inappropriate VFD struct (UNEXPECTED SUCCESS)",
        FAIL,
        FAPL_CHOICE_INVALID,
        "",
        &wrong_fa_g,
    },
    {
        "(common) H5P_DEFAULT + NULL VFD struct (FAILED)",
        SUCCEED,
        FAPL_CHOICE_DEFAULT,
        "",
        NULL,
    },
    {
        "(common) H5P_DEFAULT + ignored VFD struct (FAILED)",
        SUCCEED,
        FAPL_CHOICE_DEFAULT,
        "",
        &wrong_fa_g,
    },
    {
        "(common) H5Pcreate() + NULL VFD struct (FAILED)",
        SUCCEED,
        FAPL_CHOICE_CREATE,
        "",
        NULL,
    },
    {
        "(common) H5Pcreate() + ignored VFD struct (FAILED)",
        SUCCEED,
        FAPL_CHOICE_CREATE,
        "",
        &wrong_fa_g,
    },
    {
        "(common) H5P_DEFAULT + non-VFD name + NULL VFD struct (UNEXPECTED SUCCESS)",
        FAIL,
        FAPL_CHOICE_DEFAULT,
        "unknown",
        NULL,
    }};

#ifdef H5_HAVE_ROS3_VFD

/****************************/
/* ROS3 test configurations */
/****************************/

H5FD_ros3_fapl_t ros3_anon_fa_g = {1, FALSE, "", "", ""};

fapl_testcase_t ros3_cases_g[] = {{
                                      "(ROS3) H5I_INVALID_HID + NULL VFD struct (UNEXPECTED SUCCESS)",
                                      FAIL,
                                      FAPL_CHOICE_INVALID,
                                      "ros3",
                                      NULL,
                                  },
                                  {
                                      "(ROS3) H5I_INVALID_HID + valid VFD struct (UNEXPECTED SUCCESS)",
                                      FAIL,
                                      FAPL_CHOICE_INVALID,
                                      "ros3",
                                      &ros3_anon_fa_g,
                                  },
                                  {
                                      "(ROS3) H5Pcreate() + NULL VFD struct (UNEXPECTED SUCCESS)",
                                      FAIL,
                                      FAPL_CHOICE_CREATE,
                                      "ros3",
                                      NULL,
                                  },
                                  {
                                      "(ROS3) H5Pcreate() + valid VFD struct (FAILED)",
                                      SUCCEED,
                                      FAPL_CHOICE_CREATE,
                                      "ros3",
                                      &ros3_anon_fa_g,
                                  },
                                  {
                                      "(ROS3) H5P_DEFAULT + valid VFD struct (FAILED)",
                                      SUCCEED,
                                      FAPL_CHOICE_DEFAULT,
                                      "ros3",
                                      &ros3_anon_fa_g,
                                  }};
#endif /* H5_HAVE_ROS3_VFD */

#ifdef H5_HAVE_LIBHDFS

/****************************/
/* HDFS test configurations */
/****************************/

H5FD_hdfs_fapl_t hdfs_fa_g = {
    1,    /* fapl version          */
    "",   /* namenode name         */
    0,    /* namenode port         */
    "",   /* kerberos ticket cache */
    "",   /* user name             */
    2048, /* stream buffer size    */
};

fapl_testcase_t hdfs_cases_g[] = {{
                                      "(HDFS) H5I_INVALID_HID + NULL VFD struct (UNEXPECTED SUCCESS)",
                                      FAIL,
                                      FAPL_CHOICE_INVALID,
                                      "hdfs",
                                      NULL,
                                  },
                                  {
                                      "(HDFS) H5I_INVALID_HID + valid VFD struct (UNEXPECTED SUCCESS)",
                                      FAIL,
                                      FAPL_CHOICE_INVALID,
                                      "hdfs",
                                      &hdfs_fa_g,
                                  },
                                  {
                                      "(HDFS) H5Pcreate() + NULL VFD struct (UNEXPECTED SUCCESS)",
                                      FAIL,
                                      FAPL_CHOICE_CREATE,
                                      "hdfs",
                                      NULL,
                                  },
                                  {
                                      "(HDFS) H5Pcreate() + valid VFD struct (FAILED)",
                                      SUCCEED,
                                      FAPL_CHOICE_CREATE,
                                      "hdfs",
                                      &hdfs_fa_g,
                                  },
                                  {
                                      "(HDFS) H5P_DEFAULT + valid VFD struct (FAILED)",
                                      SUCCEED,
                                      FAPL_CHOICE_DEFAULT,
                                      "hdfs",
                                      &hdfs_fa_g,
                                  }};
#endif /* H5_HAVE_LIBHDFS */

typedef struct tuple_testcase_t {
    const char *test_msg;        /* info about test case */
    const char *in_str;          /* input string */
    int         sep;             /* separator "character" */
    herr_t      expected_result; /* expected SUCCEED / FAIL */
    unsigned    exp_nelems;      /* expected number of elements */
                                 /* (no more than 7!)           */
    const char *exp_elems[7];    /* list of elements (no more than 7!) */
} tuple_testcase_t;

tuple_testcase_t tuple_cases_g[] = {
    {
        "bad start",
        "words(before)",
        ';',
        FAIL,
        0,
        {NULL},
    },
    {
        "tuple not closed",
        "(not ok",
        ',',
        FAIL,
        0,
        {NULL},
    },
    {
        "empty tuple",
        "()",
        '-',
        SUCCEED,
        1,
        {""},
    },
    {
        "no separator",
        "(stuff keeps on going)",
        ',',
        SUCCEED,
        1,
        {"stuff keeps on going"},
    },
    {
        "4-ple, escaped seperator",
        "(elem0,elem1,el\\,em2,elem3)", /* "el\,em" */
        ',',
        SUCCEED,
        4,
        {"elem0", "elem1", "el,em2", "elem3"},
    },
    {
        "5-ple, escaped escaped separator",
        "(elem0,elem1,el\\\\,em2,elem3)",
        ',',
        SUCCEED,
        5,
        {"elem0", "elem1", "el\\", "em2", "elem3"},
    },
    {
        "escaped non-comma separator",
        "(5-2-7-2\\-6-2)",
        '-',
        SUCCEED,
        5,
        {"5", "2", "7", "2-6", "2"},
    },
    {
        "embedded close-paren",
        "(be;fo)re)",
        ';',
        SUCCEED,
        2,
        {"be", "fo)re"},
    },
    {
        "embedded non-escaping backslash",
        "(be;fo\\re)",
        ';',
        SUCCEED,
        2,
        {"be", "fo\\re"},
    },
    {
        "double close-paren at end",
        "(be;fore))",
        ';',
        SUCCEED,
        2,
        {"be", "fore)"},
    },
    {
        "empty elements",
        "(;a1;;a4;)",
        ';',
        SUCCEED,
        5,
        {"", "a1", "", "a4", ""},
    },
    {
        "nested tuples with different separators",
        "((4,e,a);(6,2,a))",
        ';',
        SUCCEED,
        2,
        {"(4,e,a)", "(6,2,a)"},
    },
    {
        "nested tuples with same separators",
        "((4,e,a),(6,2,a))",
        ',',
        SUCCEED,
        6,
        {"(4", "e", "a)", "(6", "2", "a)"},
    },
    {
        "real-world use case",
        "(us-east-2,AKIAIMC3D3XLYXLN5COA,ugs5aVVnLFCErO/8uW14iWE3K5AgXMpsMlWneO/+)",
        ',',
        SUCCEED,
        3,
        {"us-east-2", "AKIAIMC3D3XLYXLN5COA", "ugs5aVVnLFCErO/8uW14iWE3K5AgXMpsMlWneO/+"},
    }};

/*----------------------------------------------------------------------------
 *
 * Function:    test_parse_tuple()
 *
 * Purpose:     Provide unit tests and specification for the `parse_tuple()`
 *              function.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2017-11-11
 *
 *----------------------------------------------------------------------------
 */
static herr_t
test_parse_tuple(void)
{
    tuple_testcase_t tc;
    unsigned         n_tests = 14;
    unsigned         u       = 0;
    unsigned         count   = 0;
    unsigned         elem_u  = 0;
    char **          parsed  = NULL;
    char *           cpy     = NULL;
    herr_t           ret;

    TESTING("arbitrary-count tuple parsing");

    for (u = 0; u < n_tests; u++) {

        tc = tuple_cases_g[u];

        ret = parse_tuple(tc.in_str, tc.sep, &cpy, &count, &parsed);

        if (tc.expected_result != ret)
            FAIL_PUTS_ERROR("unexpected result from parse_tuple()")
        if (tc.exp_nelems != count)
            FAIL_PUTS_ERROR("incorrect number of elements returned from parse_tupble()")

        if (ret == SUCCEED) {
            /* Successful return case checks */
            if (NULL == parsed)
                FAIL_PUTS_ERROR("parsed pointer should not be NULL on success");
            for (elem_u = 0; elem_u < count; elem_u++) {
                if (HDstrcmp(tc.exp_elems[elem_u], parsed[elem_u]))
                    FAIL_PUTS_ERROR("Bad elements detected")
            }
        }
        else {
            /* Failed return case checks */
            if (parsed != NULL)
                FAIL_PUTS_ERROR("should not have returned elements on failure")
        }

        HDfree(cpy);
        HDfree(parsed);

    } /* for each testcase */

    PASSED();
    return SUCCEED;

error:
    HDfree(parsed);
    HDfree(cpy);

    return FAIL;

} /* test_parse_tuple */

/*----------------------------------------------------------------------------
 *
 * Function:    test_populate_ros3_fa()
 *
 * Purpose:     Verify behavior of `populate_ros3_fa()`
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2017-11-13
 *
 *----------------------------------------------------------------------------
 */
#ifdef H5_HAVE_ROS3_VFD
static herr_t
test_populate_ros3_fa(void)
{
    int bad_version = 0xf87a; /* arbitrarily wrong version number */

    TESTING("ros3 fapl population");

    HDassert(bad_version != H5FD_CURR_ROS3_FAPL_T_VERSION);

    /* NULL fapl config pointer fails */
    {
        const char *values[] = {"x", "y", "z"};

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(NULL, values))
            TEST_ERROR
    }

    /* NULL values pointer yields default fapl */
    {
        H5FD_ros3_fapl_t fa = {bad_version, TRUE, "u", "v", "w"};

        /* Should PASS */
        if (FAIL == h5tools_populate_ros3_fapl(&fa, NULL))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* all-empty values yields default fapl */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, TRUE, "u", "v", "w"};
        const char *     values[] = {"", "", ""};

        /* Should PASS */
        if (FAIL == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* successfully set fapl with values
     * excess value is ignored
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"x", "y", "z", "a"};

        /* Should PASS */
        if (FAIL == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != TRUE)
            TEST_ERROR
        if (HDstrcmp("x", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("y", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("z", fa.secret_key))
            TEST_ERROR
    }

    /* NULL region
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {NULL, "y", "z", NULL};

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* empty region
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"", "y", "z", NULL};

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* region overflow
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"somewhere over the rainbow not too high "
                                "there is another rainbow bounding some darkened sky",
                                "y", "z"};

        HDassert(HDstrlen(values[0]) > H5FD_ROS3_MAX_REGION_LEN);

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* NULL id
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"x", NULL, "z", NULL};

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* empty id (non-empty region, key)
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"x", "", "z", NULL};

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* id overflow
     * partial set: region
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"x",
                                "Why is it necessary to solve the problem? "
                                "What benefits will you receive by solving the problem? "
                                "What is the unknown? "
                                "What is it you don't yet understand? "
                                "What is the information you have? "
                                "What isn't the problem? "
                                "Is the information insufficient, redundant, or contradictory? "
                                "Should you draw a diagram or figure of the problem? "
                                "What are the boundaries of the problem? "
                                "Can you separate the various parts of the problem?",
                                "z"};

        HDassert(HDstrlen(values[1]) > H5FD_ROS3_MAX_SECRET_ID_LEN);

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("x", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* NULL key
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"x", "y", NULL, NULL};

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* empty key (non-empty region, id)
     * yields authenticating fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"x", "y", "", NULL};

        /* Should PASS */
        if (FAIL == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != TRUE)
            TEST_ERROR
        if (HDstrcmp("x", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("y", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* empty key, region (non-empty id)
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"", "y", "", NULL};

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* empty key, id (non-empty region)
     * yields default fapl
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"x", "", "", NULL};

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* key overflow
     * partial set: region, id
     */
    {
        H5FD_ros3_fapl_t fa       = {bad_version, FALSE, "a", "b", "c"};
        const char *     values[] = {"x", "y",
                                "Why is it necessary to solve the problem? "
                                "What benefits will you receive by solving the problem? "
                                "What is the unknown? "
                                "What is it you don't yet understand? "
                                "What is the information you have? "
                                "What isn't the problem? "
                                "Is the information insufficient, redundant, or contradictory? "
                                "Should you draw a diagram or figure of the problem? "
                                "What are the boundaries of the problem? "
                                "Can you separate the various parts of the problem?"};

        HDassert(HDstrlen(values[2]) > H5FD_ROS3_MAX_SECRET_KEY_LEN);

        /* Should FAIL */
        if (SUCCEED == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != H5FD_CURR_ROS3_FAPL_T_VERSION)
            TEST_ERROR
        if (fa.authenticate != FALSE)
            TEST_ERROR
        if (HDstrcmp("x", fa.aws_region))
            TEST_ERROR
        if (HDstrcmp("y", fa.secret_id))
            TEST_ERROR
        if (HDstrcmp("", fa.secret_key))
            TEST_ERROR
    }

    /* use case
     */
    {
        H5FD_ros3_fapl_t fa       = {0, 0, "", "", ""};
        const char *     values[] = {"us-east-2", "AKIAIMC3D3XLYXLN5COA",
                                "ugs5aVVnLFCErO/8uW14iWE3K5AgXMpsMlWneO/+"};

        /* Should PASS */
        if (FAIL == h5tools_populate_ros3_fapl(&fa, values))
            TEST_ERROR

        if (fa.version != 1)
            TEST_ERROR
        if (fa.authenticate != TRUE)
            TEST_ERROR
    }

    PASSED();
    return SUCCEED;

error:

    return FAIL;

} /* test_populate_ros3_fa */
#endif /* H5_HAVE_ROS3_VFD */

/*----------------------------------------------------------------------------
 *
 * Function:    test_set_configured_fapl()
 *
 * Purpose:     Verify `h5tools_get_fapl()` with ROS3 and HDFS VFDs
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Jacob Smith
 *              2018-07-12
 *
 *----------------------------------------------------------------------------
 */
static herr_t
test_set_configured_fapl(vfd_tests_e test_type, fapl_testcase_t cases[], unsigned n_cases)
{
    hid_t    in_fapl_id  = H5I_INVALID_HID;
    hid_t    out_fapl_id = H5I_INVALID_HID;
    unsigned u;

    if (VFD_TESTS_COMMON == test_type)
        TESTING("set fapl vfd (common)")
    else if (VFD_TESTS_ROS3 == test_type)
        TESTING("set fapl vfd (ros3)")
    else if (VFD_TESTS_HDFS == test_type)
        TESTING("set fapl vfd (hdfs)")

    /* Loop over all test cases */
    for (u = 0; u < n_cases; u++) {
        h5tools_vfd_info_t vfd_info;
        fapl_testcase_t    tc = cases[u];

        /* Setup */
        if (tc.fapl_choice == FAPL_CHOICE_DEFAULT) {
            in_fapl_id = H5P_DEFAULT;
        }
        else if (tc.fapl_choice == FAPL_CHOICE_CREATE) {
            if (H5I_INVALID_HID == (in_fapl_id = H5Pcreate(H5P_FILE_ACCESS)))
                TEST_ERROR
        }
        else
            in_fapl_id = H5I_INVALID_HID;

        /* Test */
        if (!HDstrcmp("", tc.vfd_name))
            out_fapl_id = h5tools_get_fapl(in_fapl_id, NULL);
        else {
            vfd_info.info = tc.fa;
            vfd_info.name = tc.vfd_name;
            out_fapl_id   = h5tools_get_fapl(in_fapl_id, &vfd_info);
        }

        /* Check */
        if ((tc.expected_result == FAIL && H5I_INVALID_HID != out_fapl_id) ||
            (tc.expected_result == SUCCEED && H5I_INVALID_HID == out_fapl_id))
            FAIL_PUTS_ERROR(tc.error_message)

        /* Close */
        if (tc.fapl_choice == FAPL_CHOICE_CREATE && H5Pclose(in_fapl_id) < 0)
            TEST_ERROR
        if (out_fapl_id != H5I_INVALID_HID && H5Pclose(out_fapl_id) < 0)
            TEST_ERROR
    }

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(in_fapl_id);
        H5Pclose(out_fapl_id);
    }
    H5E_END_TRY;

    return FAIL;
} /* test_set_configured_fapl */

/*----------------------------------------------------------------------------
 *
 * Function:    main()
 *
 * Purpose:     Run all test functions
 *
 * Return:      EXIT_FAILURE/EXIT_SUCCESS
 *
 * Programmer:  Jacob Smith
 *              2017-11-10
 *
 *----------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    h5_reset();

    HDprintf("Testing tools VFD functionality.\n");

    nerrors += test_parse_tuple() < 0 ? 1 : 0;
    nerrors += test_set_configured_fapl(VFD_TESTS_COMMON, common_cases_g, 7) < 0 ? 1 : 0;
#ifdef H5_HAVE_ROS3_VFD
    nerrors += test_populate_ros3_fa() < 0 ? 1 : 0;
    nerrors += test_set_configured_fapl(VFD_TESTS_ROS3, ros3_cases_g, 5) < 0 ? 1 : 0;
#endif
#ifdef H5_HAVE_LIBHDFS
    nerrors += test_set_configured_fapl(VFD_TESTS_HDFS, hdfs_cases_g, 5) < 0 ? 1 : 0;
#endif

    if (nerrors) {
        HDprintf("***** %d tools VFD TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    HDprintf("All tools VFD tests passed.\n");

    return EXIT_SUCCESS;

} /* end main() */
