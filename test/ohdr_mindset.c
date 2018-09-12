/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Tests to verify behavior of minimized object headers.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "h5test.h"

/******************
 * TESTING MACROS *
 ******************/

#define DEBUG_OH_SIZE 0 /* toggle some debug printing (0 off, 1 on)*/

#ifndef JSMITH_TESTING

/*****************************************************************************
 *
 * FILE-LOCAL TESTING MACROS
 *
 * Purpose:
 *
 *     1) Upon test failure, goto-jump to single-location teardown in test
 *        function. E.g., `error:` (consistency with HDF corpus) or
 *        `failed:` (reflects purpose).
 *            >>> using "error", in part because `H5E_BEGIN_TRY` expects it.
 *     2) Increase clarity and reduce overhead found with `TEST_ERROR`.
 *        e.g., "if(somefunction(arg, arg2) < 0) TEST_ERROR:"
 *        requires reading of entire line to know whether this if/call is
 *        part of the test setup, test operation, or a test unto itself.
 *     3) Provide testing macros with optional user-supplied failure message;
 *        if not supplied (NULL), generate comparison output in the spirit of
 *        test-driven development. E.g., "expected 5 but was -3"
 *        User messages clarify test's purpose in code, encouraging description
 *        without relying on comments.
 *     4) Configurable expected-actual order in generated comparison strings.
 *        Some prefer `VERIFY(expected, actual)`, others
 *        `VERIFY(actual, expected)`. Provide preprocessor ifdef switch
 *        to satifsy both parties, assuming one paradigm per test file.
 *        (One could #undef and redefine the flag through the file as desired,
 *         but _why_.)
 *
 *     Provided as courtesy, per consideration for inclusion in the library
 *     proper.
 *
 *     Macros:
 *
 *         JSVERIFY_EXP_ACT - ifdef flag, configures comparison order
 *         FAIL_IF()        - check condition
 *         FAIL_UNLESS()    - check _not_ condition
 *         JSVERIFY()       - long-int equality check; prints reason/comparison
 *         JSVERIFY_NOT()   - long-int inequality check; prints
 *         JSVERIFY_STR()   - string equality check; prints
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *****************************************************************************/


/*----------------------------------------------------------------------------
 *
 * ifdef flag: JSVERIFY_EXP_ACT
 *
 * JSVERIFY macros accept arguments as (EXPECTED, ACTUAL[, reason])
 *   default, if this is undefined, is (ACTUAL, EXPECTED[, reason])
 *
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_EXP_ACT 1L


/*----------------------------------------------------------------------------
 *
 * Macro: JSFAILED_AT()
 *
 * Purpose:
 *
 *     Preface a test failure by printing "*FAILED*" and location to stdout
 *     Similar to `H5_FAILED(); AT();` from h5test.h
 *
 *     *FAILED* at somefile.c:12 in function_name()...
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSFAILED_AT() {                                                 \
    HDprintf("*FAILED* at %s:%d in %s()...\n", __FILE__, __LINE__, FUNC); \
}


/*----------------------------------------------------------------------------
 *
 * Macro: FAIL_IF()
 *
 * Purpose:
 *
 *     Make tests more accessible and less cluttered than
 *         `if (thing == otherthing()) TEST_ERROR`
 *         paradigm.
 *
 *     The following lines are roughly equivalent:
 *
 *         `if (myfunc() < 0) TEST_ERROR;` (as seen elsewhere in HDF tests)
 *         `FAIL_IF(myfunc() < 0)`
 *
 *     Prints a generic "FAILED AT" line to stdout and jumps to `error`,
 *     similar to `TEST_ERROR` in h5test.h
 *
 * Programmer: Jacob Smith
 *             2017-10-23
 *
 *----------------------------------------------------------------------------
 */
#define FAIL_IF(condition) \
if (condition) {           \
    JSFAILED_AT()          \
    goto error;           \
}


/*----------------------------------------------------------------------------
 *
 * Macro: FAIL_UNLESS()
 *
 * Purpose:
 *
 *     TEST_ERROR wrapper to reduce cognitive overhead from "negative tests",
 *     e.g., "a != b".
 *
 *     Opposite of FAIL_IF; fails if the given condition is _not_ true.
 *
 *     `FAIL_IF( 5 != my_op() )`
 *     is equivalent to
 *     `FAIL_UNLESS( 5 == my_op() )`
 *     However, `JSVERIFY(5, my_op(), "bad return")` may be even clearer.
 *         (see JSVERIFY)
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#if 0 /* UNUSED */
#define FAIL_UNLESS(condition) \
if (!(condition)) {            \
    JSFAILED_AT()              \
    goto error;                \
}
#endif /* UNUSED */


/*----------------------------------------------------------------------------
 *
 * Macro: JSERR_LONG()
 *
 * Purpose:
 *
 *     Print an failure message for long-int arguments.
 *     ERROR-AT printed first.
 *     If `reason` is given, it is printed on own line and newlined after
 *     else, prints "expected/actual" aligned on own lines.
 *
 *     *FAILED* at myfile.c:488 in somefunc()...
 *     forest must be made of trees.
 *
 *     or
 *
 *     *FAILED* at myfile.c:488 in somefunc()...
 *       ! Expected 425
 *       ! Actual   3
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSERR_LONG(expected, actual, reason) {           \
    JSFAILED_AT()                                        \
    if (reason!= NULL) {                                 \
        HDprintf("%s\n", (reason));                        \
    } else {                                             \
        HDprintf("  ! Expected %ld\n  ! Actual   %ld\n",   \
                  (long)(expected), (long)(actual));     \
    }                                                    \
}


/*----------------------------------------------------------------------------
 *
 * Macro: JSERR_STR()
 *
 * Purpose:
 *
 *     Print an failure message for string arguments.
 *     ERROR-AT printed first.
 *     If `reason` is given, it is printed on own line and newlined after
 *     else, prints "expected/actual" aligned on own lines.
 *
 *     *FAILED*  at myfile.c:421 in myfunc()...
 *     Blue and Red strings don't match!
 *
 *     or
 *
 *     *FAILED*  at myfile.c:421 in myfunc()...
 *     !!! Expected:
 *     this is my expected
 *     string
 *     !!! Actual:
 *     not what I expected at all
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSERR_STR(expected, actual, reason) {           \
    JSFAILED_AT()                                       \
    if ((reason) != NULL) {                             \
        HDprintf("%s\n", (reason));                       \
    } else {                                            \
        HDprintf("!!! Expected:\n%s\n!!!Actual:\n%s\n",   \
                 (expected), (actual));                 \
    }                                                   \
}

#ifdef JSVERIFY_EXP_ACT


/*----------------------------------------------------------------------------
 *
 * Macro: JSVERIFY()
 *
 * Purpose:
 *
 *     Verify that two long integers are equal.
 *     If unequal, print failure message
 *     (with `reason`, if not NULL; expected/actual if NULL)
 *     and jump to `error` at end of function
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSVERIFY(expected, actual, reason)     \
if ((long)(actual) != (long)(expected)) {      \
    JSERR_LONG((expected), (actual), (reason)) \
    goto error;                                \
} /* JSVERIFY */


/*----------------------------------------------------------------------------
 *
 * Macro: JSVERIFY_NOT()
 *
 * Purpose:
 *
 *     Verify that two long integers are _not_ equal.
 *     If equal, print failure message
 *     (with `reason`, if not NULL; expected/actual if NULL)
 *     and jump to `error` at end of function
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_NOT(expected, actual, reason) \
if ((long)(actual) == (long)(expected)) {      \
    JSERR_LONG((expected), (actual), (reason)) \
    goto error;                                \
} /* JSVERIFY_NOT */


/*----------------------------------------------------------------------------
 *
 * Macro: JSVERIFY_STR()
 *
 * Purpose:
 *
 *     Verify that two strings are equal.
 *     If unequal, print failure message
 *     (with `reason`, if not NULL; expected/actual if NULL)
 *     and jump to `error` at end of function
 *
 * Programmer: Jacob Smith
 *             2017-10-24
 *
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_STR(expected, actual, reason) \
if (strcmp((actual), (expected)) != 0) {       \
    JSERR_STR((expected), (actual), (reason)); \
    goto error;                                \
} /* JSVERIFY_STR */

#else /* JSVERIFY_EXP_ACT */
      /* Repeats macros above, but with actual/expected parameters reversed. */


/*----------------------------------------------------------------------------
 * Macro: JSVERIFY()
 * See: JSVERIFY documentation above.
 * Programmer: Jacob Smith
 *             2017-10-14
 *----------------------------------------------------------------------------
 */
#define JSVERIFY(actual, expected, reason)      \
if ((long)(actual) != (long)(expected)) {       \
    JSERR_LONG((expected), (actual), (reason)); \
    goto error;                                 \
} /* JSVERIFY */


/*----------------------------------------------------------------------------
 * Macro: JSVERIFY_NOT()
 * See: JSVERIFY_NOT documentation above.
 * Programmer: Jacob Smith
 *             2017-10-14
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_NOT(actual, expected, reason) \
if ((long)(actual) == (long)(expected)) {      \
    JSERR_LONG((expected), (actual), (reason)) \
    goto error;                                \
} /* JSVERIFY_NOT */


/*----------------------------------------------------------------------------
 * Macro: JSVERIFY_STR()
 * See: JSVERIFY_STR documentation above.
 * Programmer: Jacob Smith
 *             2017-10-14
 *----------------------------------------------------------------------------
 */
#define JSVERIFY_STR(actual, expected, reason) \
if (strcmp((actual), (expected)) != 0) {       \
    JSERR_STR((expected), (actual), (reason)); \
    goto error;                                \
} /* JSVERIFY_STR */

#endif /* JSVERIFY_EXP_ACT */

#endif /* JSMITH_TESTING */

/* basenames of test files created in this test suite */
#define OHMIN_FILENAME_A "ohdr_min_a"
#define OHMIN_FILENAME_B "ohdr_min_b"

/* used for object header size comparison */
#define EQ 1
#define LT 2
#define GT 3

/* pseudo-enumeration of symbols to select H5*close() function in macro */
#define CLOSE_ATTRIBUTE 1
#define CLOSE_DATASET 2
#define CLOSE_DATASPACE 3
#define CLOSE_DATATYPE 4
#define CLOSE_FILE 5
#define CLOSE_PLIST 6


/* ---------------------------------------------------------------------------
 * Macro: MUST_CLOSE(...)
 *
 * Trigger an error if calling close on the id fails (e.g., H5Fclose(fid).
 * Uses #defined values to indicate expected id kind (plist vs file, &c.).
 * Prints message on error.
 * Please use only at "top level" of test function (because JSVERIFY).
 * ---------------------------------------------------------------------------
 */
#define MUST_CLOSE(id, kind)                                       \
{   switch (kind) {                                                \
        case CLOSE_ATTRIBUTE :                                     \
            JSVERIFY(SUCCEED, H5Aclose((id)), "closing attribute") \
            break;                                                 \
        case CLOSE_DATASET :                                       \
            JSVERIFY(SUCCEED, H5Dclose((id)), "closing dataset")   \
            break;                                                 \
        case CLOSE_DATASPACE :                                     \
            JSVERIFY(SUCCEED, H5Sclose((id)), "closing dataspace") \
            break;                                                 \
        case CLOSE_DATATYPE :                                      \
            JSVERIFY(SUCCEED, H5Tclose((id)), "closing datatype")  \
            break;                                                 \
        case CLOSE_FILE :                                          \
            JSVERIFY(SUCCEED, H5Fclose((id)), "closing file")      \
            break;                                                 \
        case CLOSE_PLIST :                                         \
            JSVERIFY(SUCCEED, H5Pclose((id)), "closing plist")     \
            break;                                                 \
        default:                                                   \
            JSVERIFY(0, 1, "Unidentified MUST_CLOSE constant")     \
            break;                                                 \
    }                                                              \
    (id) = -1;                                                     \
}


/* ---------------------------------------------------------------------------
 * Macro: PRINT_DSET_OH_COMPARISON(...)
 *
 * Pretty-print metadata information about two dataset object headers.
 * ---------------------------------------------------------------------------
 */
#define PRINT_DSET_OH_COMPARISON(did1, did2)                         \
{   H5O_info_t info1;                                                \
    H5O_info_t info2;                                                \
                                                                     \
    FAIL_IF( SUCCEED != H5Oget_info2((did1), &info1, H5O_INFO_HDR) ) \
    FAIL_IF( SUCCEED != H5Oget_info2((did2), &info2, H5O_INFO_HDR) ) \
                                                                     \
    HDprintf("\n==HEADERS==  UNMINIMIZED  MINIMIZED\n");               \
    HDprintf("    version: %11u  %9u\n",                               \
            info1.hdr.version,                                       \
            info2.hdr.version);                                      \
    HDprintf(" # messages: %11u  %9u\n",                               \
            info1.hdr.nmesgs,                                        \
            info2.hdr.nmesgs);                                       \
    HDprintf("       meta: %11llu  %9llu\n",                           \
            info1.hdr.space.meta,                                    \
            info2.hdr.space.meta);                                   \
    HDprintf("       free: %11llu  %9llu\n",                           \
            info1.hdr.space.free,                                    \
            info2.hdr.space.free);                                   \
    HDprintf("      total: %11llu  %9llu\n",                           \
            info1.hdr.space.total,                                   \
            info2.hdr.space.total);                                  \
}

/*********************
 * UTILITY FUNCTIONS *
 *********************/


/* ---------------------------------------------------------------------------
 * Function:  make_file()
 *
 * Purpose: Create a file with the name, and record its hid in out parameter.
 *
 * Return: 0 (success) or -1 (failure)
 *
 * ---------------------------------------------------------------------------
 */
static herr_t
make_file(                    \
        const char *filename, \
        hid_t      *fid)
{
    hid_t id = -1;
    id = H5Fcreate(
            filename,
            H5F_ACC_TRUNC,
            H5P_DEFAULT,
            H5P_DEFAULT);
    if (id < 0)
        return FAIL;
    *fid = id;

    return SUCCEED;
} /* make_file */


/* ---------------------------------------------------------------------------
 * Function:  make_dataset()
 *
 * Purpose: Create a dataset and record its hid in out parameter `dset_id`.
 *
 * Return: 0 (success) or -1 (failure)
 *
 * ---------------------------------------------------------------------------
 */
static herr_t
make_dataset(                     \
        hid_t       loc_id,       \
        const char *name,         \
        hid_t       datatype_id,  \
        hid_t       dataspace_id, \
        hid_t       dcpl_id,      \
        hid_t      *dset_id)
{
    hid_t id = -1;

    id = H5Dcreate(
            loc_id,
            name,
            datatype_id,
            dataspace_id,
            H5P_DEFAULT,  /* LCPL id */
            dcpl_id,
            H5P_DEFAULT); /* DAPL id */
    if (id < 0)
        return FAIL;
    *dset_id = id;

    return SUCCEED;
} /* make_dataset */


/* ---------------------------------------------------------------------------
 * Function:  put_attribute()
 *
 * Purpose:   Set an attribute with the given information.
 *
 *     If the out parameter `attr_id` is negative, a new attribute will be
 *     created with the given information. Else, it will attempt to update the
 *     attribute with the new value.
 *
 * Return: 0 (success) or -1 (failure)
 *
 * ---------------------------------------------------------------------------
 */
static herr_t
put_attribute(                      \
        hid_t       loc_id,         \
        const char *attrname,       \
        const void *attrvalue,      \
        hid_t       datatype_id,    \
        hid_t       dataspace_id,  /* ignored if attribute_id >= 0 */ \
        hid_t      *attribute_id)
{
    if ((*attribute_id) < 0) {
        hid_t id = -1;
        id = H5Acreate(
                loc_id,
                attrname,
                datatype_id,
                dataspace_id,
                H5P_DEFAULT,  /* acpl */
                H5P_DEFAULT); /* aapl */
        if (id < 0)
            return FAIL;
        *attribute_id = id;
    }
    return H5Awrite(*attribute_id, datatype_id, attrvalue);
} /* put_attribute */


/* ---------------------------------------------------------------------------
 * Function:  count_attributes()
 *
 * Purpose: Count the number of attributes attached to an object.
 *
 *          TODO: If the location id is that of a file, tries to count all the
 *                attributes present in the file.
 *
 * Return: -1 if an error occurred, else the number of attributes.
 *
 * ---------------------------------------------------------------------------
 */
static int
count_attributes(hid_t dset_id)
{
    H5O_info_t info;

    if (0 > H5Oget_info(dset_id, &info, H5O_INFO_ALL))
        return -1;
    else 
        return (int)info.num_attrs; /* should never exceed int bounds */
} /* count_attributes */


/* ---------------------------------------------------------------------------
 * Function:  _oh_getsize()
 *
 * Purpose: Get the total space used by the object header
 *
 *
 * Return: SUCCEED/FAIL. On success, stores size in `size_out` pointer.
 *
 * ---------------------------------------------------------------------------
 */
static herr_t
_oh_getsize(hid_t did, hsize_t *size_out)
{
    H5O_info_t info;
    if (FAIL == H5Oget_info2(did, &info, H5O_INFO_HDR))
        return FAIL;
    *size_out = info.hdr.space.total;
    return SUCCEED;
} /* _oh_getsize */


/* ---------------------------------------------------------------------------
 * Function:  oh_compare()
 *
 * Purpose: Compare the TOTAL space used by datasets' object headers.
 *
 *
 * Return: -1 if an error occurred, else positive #defined indicator value.
 *
 * ---------------------------------------------------------------------------
 */
static int
oh_compare(         \
        hid_t did1, \
        hid_t did2)
{
    hsize_t space1 = 0;
    hsize_t space2 = 0;

    if (FAIL == _oh_getsize(did1, &space1))
        return -1;

    if (FAIL == _oh_getsize(did2, &space2))
        return -2;

    if (space1 < space2)
        return LT;
    else if (space1 > space2)
        return GT;
    else
        return EQ;
} 

/******************
 * TEST FUNCTIONS *
 ******************/


/* ---------------------------------------------------------------------------
 * Function:  test_attribute_addition()
 *
 * Purpose: Demonstrate attribute addition to datasets.
 *
 * Return: 0 (pass) or 1 (failure)
 *
 * ---------------------------------------------------------------------------
 */
static int
test_attribute_addition(void)
{
    hid_t   int_type_id   = -1;
    hid_t   char_type_id  = -1;
    hid_t   dcpl_id       = -1;
    hid_t   dspace_id     = -1;
    hid_t   dspace_scalar_id     = -1;
    hid_t   dset_id       = -1;
    hid_t   mindset_id    = -1;
    hid_t   attr_1_id       = -1;
    hid_t   attr_1a_id     = -1;
    hid_t   attr_2_id       = -1;
    hid_t   attr_2a_id     = -1;
    hid_t   attr_3_id       = -1;
    hid_t   attr_3a_id     = -1;
    hid_t   file_id       = -1;
    hsize_t array_10[1]   = {10}; /* dataspace extent */
    char    buffer[10]    = "";   /* to inspect string attribute */
    int     a_out         = 0;
    char    filename[512] = "";

    TESTING("attribute additions to [un]minimized dataset")

    /*********
     * SETUP *
     *********/

    FAIL_IF( NULL == h5_fixname(
            OHMIN_FILENAME_A,
            H5P_DEFAULT,
            filename,
            sizeof(filename)) )

    dspace_id = H5Screate_simple(
            1,        /* rank */
            array_10, /* current dimensions */
            NULL);    /* maximum dimensions */
    FAIL_IF( 0 > dspace_id )

    dspace_scalar_id = H5Screate(H5S_SCALAR);
    FAIL_IF( 0 > dspace_scalar_id )

    char_type_id = H5Tcopy(H5T_NATIVE_CHAR);
    FAIL_IF( 0 > char_type_id )

    int_type_id = H5Tcopy(H5T_NATIVE_INT);
    FAIL_IF( 0 > int_type_id )

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > dcpl_id )
    JSVERIFY( SUCCEED,                                  \
              H5Pset_dset_no_attrs_hint(dcpl_id, TRUE), \
             "can't set DCPL to minimize object header")

    JSVERIFY( SUCCEED,           \
              make_file(         \
                    filename,    \
                    &file_id),   \
             "unable to create file")

    H5E_BEGIN_TRY {
        JSVERIFY( -1,                                        \
                  count_attributes(dset_id),                 \
                 "shouldn't be able to count missing dataset")
    } H5E_END_TRY;

    JSVERIFY( SUCCEED,                 \
              make_dataset(            \
                    file_id,          /* shorthand for root group? */ \
                    "dataset",         \
                    int_type_id,       \
                    dspace_id,         \
                    H5P_DEFAULT,      /* default DCPL */ \
                    &dset_id),         \
             "unable to create dataset")

    JSVERIFY( SUCCEED,                 \
              make_dataset(            \
                    file_id,           \
                    "mindataset",      \
                    int_type_id,       \
                    dspace_id,         \
                    dcpl_id,           \
                    &mindset_id),      \
             "unable to create minimizing dataset")

    /********************
     * TEST/DEMONSTRATE *
     ********************/

    /* -------------------
     * no attributes added
     */

    JSVERIFY( 0,                         \
              count_attributes(dset_id), \
              NULL)
    JSVERIFY( 0,                         \
              count_attributes(mindset_id), \
              NULL)

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /* -----------------
     * add one attribute
     */

    JSVERIFY( SUCCEED,            \
              put_attribute(      \
                    dset_id,      \
                    "PURPOSE",    \
                    "DEMO",       \
                    char_type_id, \
                    dspace_id,    \
                    &attr_1_id),  \
             "unable to set attribute 'PURPOSE:DEMO'")
    JSVERIFY( SUCCEED,            \
              put_attribute(      \
                    mindset_id,   \
                    "PURPOSE",    \
                    "DEMO",       \
                    char_type_id, \
                    dspace_id,    \
                    &attr_1a_id), \
             "unable to set attribute 'PURPOSE:DEMO'")

    JSVERIFY( 1,                         \
              count_attributes(dset_id), \
              NULL)
    JSVERIFY( 1,                            \
              count_attributes(mindset_id), \
              NULL)

    JSVERIFY( SUCCEED,
              H5Aread(attr_1_id, char_type_id, buffer),
             "can't read attribute 'PURPOSE'")
    JSVERIFY_STR( "DEMO", buffer, NULL )
    JSVERIFY( SUCCEED,
              H5Aread(attr_1a_id, char_type_id, buffer),
             "can't read attribute 'PURPOSE'")
    JSVERIFY_STR( "DEMO", buffer, NULL )

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /* -----------------
     * modify one attribute
     */

    JSVERIFY( SUCCEED,            \
              put_attribute(      \
                    dset_id,      \
                    "PURPOSE",    \
                    "REWRITE",    \
                    char_type_id, \
                    -1,           \
                    &attr_1_id),  \
             "unable to rewrite attribute 'PURPOSE:REWRITE'")
    JSVERIFY( SUCCEED,            \
              put_attribute(      \
                    mindset_id,   \
                    "PURPOSE",    \
                    "REWRITE",    \
                    char_type_id, \
                    -1,           \
                    &attr_1a_id), \
             "unable to rewrite attribute 'PURPOSE:REWRITE'")

    JSVERIFY( 1,                         \
              count_attributes(dset_id), \
              NULL)
    JSVERIFY( 1,                            \
              count_attributes(mindset_id), \
              NULL)

    JSVERIFY( SUCCEED,
              H5Aread(attr_1_id, char_type_id, buffer),
             "can't read attribute 'PURPOSE'")
    JSVERIFY_STR( "REWRITE", buffer, NULL )
    JSVERIFY( SUCCEED,
              H5Aread(attr_1a_id, char_type_id, buffer),
             "can't read attribute 'PURPOSE'")
    JSVERIFY_STR( "REWRITE", buffer, NULL )

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /* -----------------
     * add second attribute
     */

    a_out = 5;
    JSVERIFY( SUCCEED,                \
              put_attribute(          \
                    dset_id,          \
                    "RANK",           \
                    &a_out,           \
                    int_type_id,      \
                    dspace_scalar_id, \
                    &attr_2_id),      \
             "unable to set attribute 'RANK:5'")
    a_out = 3;
    JSVERIFY( SUCCEED,                \
              put_attribute(          \
                    mindset_id,       \
                    "RANK",           \
                    &a_out,           \
                    int_type_id,      \
                    dspace_scalar_id, \
                    &attr_2a_id),     \
             "unable to set attribute (minimized) 'RANK:3'")

    JSVERIFY( 2,                         \
              count_attributes(dset_id), \
              NULL)
    JSVERIFY( 2,                            \
              count_attributes(mindset_id), \
              NULL)

    JSVERIFY( SUCCEED,
              H5Aread(attr_2_id, int_type_id, &a_out),
             "can't read attribute 'RANK'")
    JSVERIFY( 5, a_out, NULL )
    JSVERIFY( SUCCEED,
              H5Aread(attr_2a_id, int_type_id, &a_out),
             "can't read attribute (minimized) 'RANK'")
    JSVERIFY( 3, a_out, NULL )

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /* -----------------
     * add third attribute
     */

    a_out = -86;
    JSVERIFY( SUCCEED,                \
              put_attribute(          \
                    dset_id,          \
                    "FLAVOR",         \
                    &a_out,           \
                    int_type_id,      \
                    dspace_scalar_id, \
                    &attr_3_id),      \
             "unable to set attribute 'FLAVOR:-86'")
    a_out = 2185;
    JSVERIFY( SUCCEED,                \
              put_attribute(          \
                    mindset_id,       \
                    "FLAVOR",         \
                    &a_out,           \
                    int_type_id,      \
                    dspace_scalar_id, \
                    &attr_3a_id),     \
             "unable to set attribute (minimized) 'FLAVOR:2185'")

    JSVERIFY( 3,                         \
              count_attributes(dset_id), \
              NULL)
    JSVERIFY( 3,                            \
              count_attributes(mindset_id), \
              NULL)

    JSVERIFY( SUCCEED,
              H5Aread(attr_3_id, int_type_id, &a_out),
             "can't read attribute 'RANK'")
    JSVERIFY( -86, a_out, NULL )
    JSVERIFY( SUCCEED,
              H5Aread(attr_3a_id, int_type_id, &a_out),
             "can't read attribute (minimized) 'RANK'")
    JSVERIFY( 2185, a_out, NULL )

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_id, mindset_id)

    /************
     * TEARDOWN *
     ************/

    MUST_CLOSE(int_type_id,  CLOSE_DATATYPE)
    MUST_CLOSE(char_type_id, CLOSE_DATATYPE)
    MUST_CLOSE(dcpl_id,      CLOSE_PLIST)
    MUST_CLOSE(dspace_id,    CLOSE_DATASPACE)
    MUST_CLOSE(dset_id,      CLOSE_DATASET)
    MUST_CLOSE(mindset_id,   CLOSE_DATASET)
    MUST_CLOSE(attr_1_id,    CLOSE_ATTRIBUTE)
    MUST_CLOSE(attr_1a_id,   CLOSE_ATTRIBUTE)
    MUST_CLOSE(attr_2_id,    CLOSE_ATTRIBUTE)
    MUST_CLOSE(attr_2a_id,   CLOSE_ATTRIBUTE)
    MUST_CLOSE(attr_3_id,    CLOSE_ATTRIBUTE)
    MUST_CLOSE(attr_3a_id,   CLOSE_ATTRIBUTE)
    MUST_CLOSE(file_id,      CLOSE_FILE)

    PASSED()
    return 0;

error :
    H5E_BEGIN_TRY {
        (void)H5Tclose(int_type_id);
        (void)H5Tclose(char_type_id);
        (void)H5Pclose(dcpl_id);
        (void)H5Sclose(dspace_id);
        (void)H5Dclose(dset_id);
        (void)H5Dclose(mindset_id);
        (void)H5Aclose(attr_1_id);
        (void)H5Aclose(attr_1a_id);
        (void)H5Aclose(attr_2_id);
        (void)H5Aclose(attr_2a_id);
        (void)H5Aclose(attr_3_id);
        (void)H5Aclose(attr_3a_id);
        (void)H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* test_attribute_addition */


/* ---------------------------------------------------------------------------
 * Function:  test_size_comparisons()
 *
 * Purpose: Examine when headers have been minimized.
 *
 * Return: 0 (pass) or 1 (failure)
 *
 * ---------------------------------------------------------------------------
 */
static int
test_size_comparisons(void)
{
    hsize_t array_10[1] = {10}; /* dataspace extents */

    /* IDs that are file-agnostic */
    hid_t dspace_id     = -1;
    hid_t int_type_id   = -1;
    hid_t dcpl_minimize = -1;
    hid_t dcpl_dontmin  = -1;

    /* IDs for non-minimzed file open */
    hid_t file_f_id   = -1; /* lower 'f' for standard file setting */
    hid_t dset_f_x_id = -1; /* 'x' for default */
    hid_t dset_f_N_id = -1; /* 'N' for explcit non-minimized dset */
    hid_t dset_f_Y_id = -1; /* 'Y' for minimzed dset */

    /* IDs for minimzed file open */
    hid_t file_F_id   = -1; /* upper 'F' for minimzed file setting */
    hid_t dset_F_x_id = -1; /* 'x' for default */
    hid_t dset_F_N_id = -1; /* 'N' for explcit non-minimized dset */
    hid_t dset_F_Y_id = -1; /* 'Y' for minimzed dset */

    char filename_a[512] = "";
    char filename_b[512] = "";

    TESTING("default size comparisons");

    /*********
     * SETUP *
     *********/

    FAIL_IF( NULL == h5_fixname(
            OHMIN_FILENAME_A,
            H5P_DEFAULT,
            filename_a,
            sizeof(filename_a)) )

    FAIL_IF( NULL == h5_fixname(
            OHMIN_FILENAME_B,
            H5P_DEFAULT,
            filename_b,
            sizeof(filename_b)) )

    dcpl_minimize = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > dcpl_minimize )
    JSVERIFY( SUCCEED,
              H5Pset_dset_no_attrs_hint(dcpl_minimize, TRUE),
              NULL )

    dcpl_dontmin = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > dcpl_dontmin )
    JSVERIFY( SUCCEED,
              H5Pset_dset_no_attrs_hint(dcpl_dontmin, FALSE),
              NULL )

    dspace_id = H5Screate_simple(
            1,        /* rank */
            array_10, /* current dimensions */
            NULL);    /* maximum dimensions */
    FAIL_IF( 0 > dspace_id )

    int_type_id = H5Tcopy(H5T_NATIVE_INT);
    FAIL_IF( 0 > int_type_id )

    JSVERIFY( SUCCEED,                  \
              make_file(                \
                    filename_a,         \
                    &file_f_id),        \
             "unable to create file 'ohdr_min_a.h5'")

    JSVERIFY( SUCCEED,              \
              make_dataset(         \
                    file_f_id,      \
                    "default",      \
                    int_type_id,    \
                    dspace_id,      \
                    H5P_DEFAULT,    \
                    &dset_f_x_id),  \
             "unable to create dataset fx")

    JSVERIFY( SUCCEED,              \
              make_dataset(         \
                    file_f_id,      \
                    "dsetNOT",      \
                    int_type_id,    \
                    dspace_id,      \
                    dcpl_dontmin,   \
                    &dset_f_N_id),  \
             "unable to create dataset fN")

    JSVERIFY( SUCCEED,              \
              make_dataset(         \
                    file_f_id,      \
                    "dsetMIN",      \
                    int_type_id,    \
                    dspace_id,      \
                    dcpl_minimize,  \
                    &dset_f_Y_id),  \
             "unable to create dataset fY")

    JSVERIFY( SUCCEED,              \
              make_file(            \
                    filename_b,     \
                    &file_F_id),    \
             "unable to create file 'ohdr_min_b.h5'")
    FAIL_IF( 0 > H5Fset_dset_no_attrs_hint(file_F_id, TRUE) )

    JSVERIFY( SUCCEED,              \
              make_dataset(         \
                    file_F_id,      \
                    "default",      \
                    int_type_id,    \
                    dspace_id,      \
                    H5P_DEFAULT,    \
                    &dset_F_x_id),  \
             "unable to create dataset Fx")

    JSVERIFY( SUCCEED,              \
              make_dataset(         \
                    file_F_id,      \
                    "dsetNOT",      \
                    int_type_id,    \
                    dspace_id,      \
                    dcpl_dontmin,   \
                    &dset_F_N_id),  \
             "unable to create dataset FN")

    JSVERIFY( SUCCEED,              \
              make_dataset(         \
                    file_F_id,      \
                    "dsetMIN",      \
                    int_type_id,    \
                    dspace_id,      \
                    dcpl_minimize,  \
                    &dset_F_Y_id),  \
             "unable to create dataset FY")

    /*********
     * TESTS *
     *********/

    JSVERIFY( EQ, oh_compare(dset_f_x_id, dset_f_x_id), NULL ) /* identity */

    JSVERIFY( EQ, oh_compare(dset_f_x_id, dset_f_N_id), NULL )
    JSVERIFY( GT, oh_compare(dset_f_x_id, dset_f_Y_id), NULL )
    JSVERIFY( GT, oh_compare(dset_f_N_id, dset_f_Y_id), NULL )

    JSVERIFY( EQ, oh_compare(dset_F_x_id, dset_F_N_id), NULL )
    JSVERIFY( EQ, oh_compare(dset_F_x_id, dset_F_Y_id), NULL )
    JSVERIFY( EQ, oh_compare(dset_F_N_id, dset_F_Y_id), NULL )

    JSVERIFY( EQ, oh_compare(dset_F_x_id, dset_f_Y_id), NULL )
    JSVERIFY( LT, oh_compare(dset_F_x_id, dset_f_x_id), NULL )

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_f_x_id, dset_F_x_id)

    /************
     * TEARDOWN *
     ************/

    MUST_CLOSE(dspace_id,     CLOSE_DATASPACE)
    MUST_CLOSE(int_type_id,   CLOSE_DATATYPE)
    MUST_CLOSE(dcpl_minimize, CLOSE_PLIST)
    MUST_CLOSE(dcpl_dontmin,  CLOSE_PLIST)

    MUST_CLOSE(file_f_id,     CLOSE_FILE)
    MUST_CLOSE(dset_f_x_id,   CLOSE_DATASET)
    MUST_CLOSE(dset_f_N_id,   CLOSE_DATASET)
    MUST_CLOSE(dset_f_Y_id,   CLOSE_DATASET)

    MUST_CLOSE(file_F_id,     CLOSE_FILE)
    MUST_CLOSE(dset_F_x_id,   CLOSE_DATASET)
    MUST_CLOSE(dset_F_N_id,   CLOSE_DATASET)
    MUST_CLOSE(dset_F_Y_id,   CLOSE_DATASET)

    PASSED()
    return 0;

error :
    H5E_BEGIN_TRY {
        (void)H5Pclose(dcpl_minimize);
        (void)H5Pclose(dcpl_dontmin);
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(int_type_id);

        (void)H5Fclose(file_f_id);
        (void)H5Dclose(dset_f_x_id);
        (void)H5Dclose(dset_f_N_id);
        (void)H5Dclose(dset_f_Y_id);

        (void)H5Fclose(file_F_id);
        (void)H5Dclose(dset_F_x_id);
        (void)H5Dclose(dset_F_N_id);
        (void)H5Dclose(dset_F_Y_id);
    } H5E_END_TRY;
    return 1;
} /* test_size_comparisons */


/* ---------------------------------------------------------------------------
 * Test minimized dataset header with filter/pipeline message
 * ---------------------------------------------------------------------------
 */
static int
test_minimized_with_filter(void)
{
    hid_t   dspace_id   = -1;
    hid_t   dtype_id    = -1;
    hid_t   fdcpl_id    = -1;
    hid_t   mindcpl_id  = -1;
    hid_t   minfdcpl_id = -1;
    hid_t   dset_id     = -1;
    hid_t   fdset_id    = -1;
    hid_t   mindset_id  = -1;
    hid_t   minfdset_id = -1;
    hid_t   file_id     = -1;

    char           filename[512]   = "";
    const hsize_t  extents[1]      = {10}; /* extents of dataspace */
    const unsigned filter_values[] = {0};  /* TBD */
    const hsize_t  chunk_dim[]     = {2};  /* needed for filter */
    const int      ndims           = 1;

/*           | default | minimize
 * ----------+---------+---------
 * no filter |    dset |  mindset
 * ----------+---------+---------
 * filter    |   fdset | minfdset
 */

    TESTING("with filter message");

    /*********
     * SETUP *
     *********/

    FAIL_IF( NULL == h5_fixname(
            OHMIN_FILENAME_A,
            H5P_DEFAULT,
            filename,
            sizeof(filename)) )

    mindcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > mindcpl_id )
    JSVERIFY( SUCCEED,
              H5Pset_dset_no_attrs_hint(mindcpl_id, TRUE),
              NULL )

    fdcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > fdcpl_id )
    JSVERIFY( SUCCEED,
              H5Pset_chunk(fdcpl_id, ndims, chunk_dim),
             "unable to chunk dataset")
    JSVERIFY( SUCCEED, 
              H5Pset_filter(
                      fdcpl_id,
                      H5Z_FILTER_DEFLATE,
                      H5Z_FLAG_OPTIONAL,
                      0,
                      filter_values),
             "unable to set compression")

    minfdcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > minfdcpl_id )
    JSVERIFY( SUCCEED,
              H5Pset_dset_no_attrs_hint(minfdcpl_id, TRUE),
             "unable to minimize to-be-filtered dataset header")
    JSVERIFY( SUCCEED,
              H5Pset_chunk(minfdcpl_id, ndims, chunk_dim),
             "unable to chunk minimized dataset")
    JSVERIFY( SUCCEED, 
              H5Pset_filter(
                      minfdcpl_id,
                      H5Z_FILTER_DEFLATE,
                      H5Z_FLAG_OPTIONAL,
                      0,
                      filter_values),
             "unable to set compression (minimized)")

    dspace_id = H5Screate_simple(1, extents, extents);
    FAIL_IF( 0 > dspace_id )

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    FAIL_IF( 0 > dtype_id )

    JSVERIFY( SUCCEED,           \
              make_file(         \
                    filename,    \
                    &file_id),   \
             "unable to create file 'ohdr_min_a.h5'")

    JSVERIFY( SUCCEED,           \
              make_dataset(      \
                    file_id,     \
                    "xx",      \
                    dtype_id,    \
                    dspace_id,   \
                    H5P_DEFAULT, \
                    &dset_id),   \
             "unable to create dataset fx")

    JSVERIFY( SUCCEED,            \
              make_dataset(       \
                    file_id,      \
                    "Mx",    \
                    dtype_id,     \
                    dspace_id,    \
                    mindcpl_id,   \
                    &mindset_id), \
             "unable to create dataset (minoh)")

    JSVERIFY( SUCCEED,            \
              make_dataset(       \
                    file_id,      \
                    "xZ",   \
                    dtype_id,     \
                    dspace_id,    \
                    fdcpl_id,     \
                    &fdset_id),   \
             "unable to create dataset (gzip)")

    JSVERIFY( SUCCEED,             \
              make_dataset(        \
                    file_id,       \
                    "MZ", \
                    dtype_id,      \
                    dspace_id,     \
                    minfdcpl_id,      \
                    &minfdset_id), \
             "unable to create dataset (minimized, gzip)")

    /*********
     * TESTS *
     *********/

    JSVERIFY( LT, oh_compare(mindset_id, dset_id),  NULL )
    JSVERIFY( LT, oh_compare(mindset_id, fdset_id), NULL )
    JSVERIFY( GT, oh_compare(minfdset_id, mindset_id), NULL )
    JSVERIFY( LT, oh_compare(minfdset_id, fdset_id),   NULL )

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(fdset_id, minfdset_id)

    /************
     * TEARDOWN *
     ************/

    MUST_CLOSE(dspace_id,   CLOSE_DATASPACE)
    MUST_CLOSE(dtype_id,    CLOSE_DATATYPE)
    MUST_CLOSE(fdcpl_id,    CLOSE_PLIST)
    MUST_CLOSE(mindcpl_id,  CLOSE_PLIST)
    MUST_CLOSE(minfdcpl_id, CLOSE_PLIST)
    MUST_CLOSE(dset_id,     CLOSE_DATASET)
    MUST_CLOSE(fdset_id,    CLOSE_DATASET)
    MUST_CLOSE(mindset_id,  CLOSE_DATASET)
    MUST_CLOSE(minfdset_id, CLOSE_DATASET)
    MUST_CLOSE(file_id,     CLOSE_FILE)

    PASSED()
    return 0;

error:
    H5E_BEGIN_TRY {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(fdcpl_id);
        (void)H5Pclose(mindcpl_id);
        (void)H5Pclose(minfdcpl_id);
        (void)H5Dclose(dset_id);
        (void)H5Dclose(fdset_id);
        (void)H5Dclose(mindset_id);
        (void)H5Dclose(minfdset_id);
        (void)H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* test_minimized_with_filter */


/* ---------------------------------------------------------------------------
 * Test minimized and recording modification times.
 * ---------------------------------------------------------------------------
 */
static int
test_modification_times(void)
{
    hid_t   dspace_id   = -1;
    hid_t   dtype_id    = -1;
    hid_t   dcpl_xM_id  = -1; /* Modtime */
    hid_t   dcpl_mx_id  = -1; /* minimized */
    hid_t   dcpl_mM_id  = -1; /* minimized, Modtime */
    hid_t   dset_xx_id  = -1;
    hid_t   dset_xM_id  = -1;
    hid_t   dset_mx_id  = -1;
    hid_t   dset_mM_id  = -1;
    hid_t   file_id     = -1;

    char          filename[512] = "";
    const hsize_t extents[1]    = {128}; /* extents of dataspace */

    TESTING("with modification times");

    /*********
     * SETUP *
     *********/

    FAIL_IF( NULL == h5_fixname(
            OHMIN_FILENAME_A,
            H5P_DEFAULT,
            filename,
            sizeof(filename)) )

    dcpl_mx_id = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > dcpl_mx_id )
    JSVERIFY( SUCCEED,
              H5Pset_dset_no_attrs_hint(dcpl_mx_id, TRUE),
              NULL )

    dcpl_xM_id = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > dcpl_xM_id )
    /* TODO: set OH VERSION to > 1 and to store modtimes? */

    dcpl_mM_id = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > dcpl_mM_id )
    JSVERIFY( SUCCEED,
              H5Pset_dset_no_attrs_hint(dcpl_mM_id, TRUE),
             "unable to minimize to-be-filtered dataset header")
    /* TODO: set OH VERSION to > 1 and to store modtimes? */

    dspace_id = H5Screate_simple(1, extents, extents);
    FAIL_IF( 0 > dspace_id )

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    FAIL_IF( 0 > dtype_id )

    JSVERIFY( SUCCEED,           \
              make_file(         \
                    filename,    \
                    &file_id),   \
             "unable to create file 'ohdr_min_a.h5'")

    JSVERIFY( SUCCEED,            \
              make_dataset(       \
                    file_id,      \
                    "xx",         \
                    dtype_id,     \
                    dspace_id,    \
                    H5P_DEFAULT,  \
                    &dset_xx_id), \
             "unable to create dataset fx")

    JSVERIFY( SUCCEED,            \
              make_dataset(       \
                    file_id,      \
                    "mx",         \
                    dtype_id,     \
                    dspace_id,    \
                    dcpl_mx_id,   \
                    &dset_mx_id), \
             "unable to create dataset (minoh)")

    JSVERIFY( SUCCEED,            \
              make_dataset(       \
                    file_id,      \
                    "xM",         \
                    dtype_id,     \
                    dspace_id,    \
                    dcpl_xM_id,   \
                    &dset_xM_id), \
             "unable to create dataset (gzip)")

    JSVERIFY( SUCCEED,             \
              make_dataset(        \
                    file_id,       \
                    "mM",          \
                    dtype_id,      \
                    dspace_id,     \
                    dcpl_mM_id,    \
                    &dset_mM_id),  \
             "unable to create dataset (minimized, gzip)")

    /*********
     * TESTS *
     *********/

    /* sanity check */
    FAIL_IF ( LT != oh_compare(dset_mx_id, dset_xx_id) )
    FAIL_IF ( LT != oh_compare(dset_mx_id, dset_xM_id) )

#define TODO_MINOH_MODTIME 1
#if TODO_MINOH_MODTIME
    SKIPPED();
    puts("    modtime details not yet implemented");
#else
    JSVERIFY( GT, oh_compare(dset_mM_id, dset_mx_id), NULL )
    JSVERIFY( LT, oh_compare(dset_mM_id, dset_xM_id), NULL )

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_xM_id, dset_mM_id)
#endif

    /************
     * TEARDOWN *
     ************/

    MUST_CLOSE(dspace_id,  CLOSE_DATASPACE)
    MUST_CLOSE(dtype_id,   CLOSE_DATATYPE)
    MUST_CLOSE(dcpl_xM_id, CLOSE_PLIST)
    MUST_CLOSE(dcpl_mx_id, CLOSE_PLIST)
    MUST_CLOSE(dcpl_mM_id, CLOSE_PLIST)
    MUST_CLOSE(dset_xx_id, CLOSE_DATASET)
    MUST_CLOSE(dset_xM_id, CLOSE_DATASET)
    MUST_CLOSE(dset_mx_id, CLOSE_DATASET)
    MUST_CLOSE(dset_mM_id, CLOSE_DATASET)
    MUST_CLOSE(file_id,    CLOSE_FILE)

#if TODO_MINOH_MODTIME
#else
    PASSED()
#endif
    return 0;

error:
    H5E_BEGIN_TRY {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(dcpl_xM_id);
        (void)H5Pclose(dcpl_mx_id);
        (void)H5Pclose(dcpl_mM_id);
        (void)H5Dclose(dset_xx_id);
        (void)H5Dclose(dset_xM_id);
        (void)H5Dclose(dset_mx_id);
        (void)H5Dclose(dset_mM_id);
        (void)H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* test_modification_times */


/* ---------------------------------------------------------------------------
 * Test minimized dataset header with a fill value set.
 * ---------------------------------------------------------------------------
 */
static int
test_fillvalue_backwards_compatability(void)
{
    hid_t file_id   = -1;
    hid_t dtype_id  = -1;
    hid_t dspace_id = -1;
    hid_t dcpl_id   = -1;
    hid_t fapl_id   = -1;
    hid_t dset_0_id = -1;
    hid_t dset_1_id = -1;

    char          filename[512] = "";
    const hsize_t extents[1]    = {64}; /* extents of dataspace */
    const int     fill[1]       = {343}; /* fill value of dataset */

    hsize_t size0 = 0;
    hsize_t size1 = 0;

    /*********
     * SETUP *
     *********/

    TESTING("with fill values and different libver support");

    FAIL_IF( NULL == h5_fixname(
            OHMIN_FILENAME_A,
            H5P_DEFAULT,
            filename,
            sizeof(filename)) )

    dspace_id = H5Screate_simple(1, extents, extents);
    FAIL_IF( 0 > dspace_id )

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    FAIL_IF( 0 > dtype_id )

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    FAIL_IF( 0 > dcpl_id )
    FAIL_IF( FAIL == H5Pset_dset_no_attrs_hint(dcpl_id, TRUE) )
    FAIL_IF( FAIL == H5Pset_fill_value(dcpl_id, dtype_id, fill) )

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    FAIL_IF( 0 > fapl_id )
    FAIL_IF( FAIL == H5Pset_libver_bounds(
            fapl_id,
            H5F_LIBVER_EARLIEST,
            H5F_LIBVER_LATEST) )

    file_id = H5Fcreate(
            filename,
            H5F_ACC_TRUNC,
            H5P_DEFAULT,
            fapl_id);
    FAIL_IF( 0 > file_id )

    JSVERIFY( SUCCEED,            \
              make_dataset(       \
                    file_id,      \
                    "fullrange",  \
                    dtype_id,     \
                    dspace_id,    \
                    dcpl_id,      \
                    &dset_0_id), \
             "unable to create dataset supporting full library range")

#if 0
    JSVERIFY( SUCCEED, _oh_getsize(dset_0_id, &size0), "can't get dset0 size" )
#endif

    /* Close file and re-open with different libver bounds
     */
    H5Fclose(file_id);
    file_id = -1;

    FAIL_IF( FAIL == H5Pset_libver_bounds(
            fapl_id,
            H5F_LIBVER_V18,
            H5F_LIBVER_LATEST) )

    file_id = H5Fopen(
            filename,
            H5F_ACC_RDWR,
            fapl_id);
    FAIL_IF( 0 > file_id )

    JSVERIFY( SUCCEED,            \
              make_dataset(       \
                    file_id,      \
                    "upperrange", \
                    dtype_id,     \
                    dspace_id,    \
                    dcpl_id,      \
                    &dset_1_id),  \
             "unable to create dataset supporting upper library range")

#if 0
    JSVERIFY( SUCCEED, _oh_getsize(dset_1_id, &size1), "can't get dset0 size" )
#endif

    /*********
     * TESTS *
     *********/

#if 0
    printf("0::%llu 1::%llu\n", size0, size1); fflush(stdout);
#endif

    if (DEBUG_OH_SIZE)
        PRINT_DSET_OH_COMPARISON(dset_1_id, dset_0_id)

    JSVERIFY( LT, oh_compare(dset_1_id, dset_0_id),
             "dset not supporting pre-1.08 should be smaller?")

    /************
     * TEARDOWN *
     ************/

    MUST_CLOSE(dspace_id, CLOSE_DATASPACE)
    MUST_CLOSE(dtype_id,  CLOSE_DATATYPE)
    MUST_CLOSE(dcpl_id,   CLOSE_PLIST)
    MUST_CLOSE(fapl_id,   CLOSE_PLIST)
    MUST_CLOSE(dset_0_id, CLOSE_DATASET)
    MUST_CLOSE(dset_1_id, CLOSE_DATASET)
    MUST_CLOSE(file_id,   CLOSE_FILE)

    PASSED()
    return 0;

error:
    H5E_BEGIN_TRY {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(dcpl_id);
        (void)H5Pclose(fapl_id);
        (void)H5Dclose(dset_0_id);
        (void)H5Dclose(dset_1_id);
        (void)H5Fclose(file_id);
    } H5E_END_TRY;
    return 1;
} /* test_fillvalue_backwards_compatability */

/********
 * MAIN *
 ********/


/* ---------------------------------------------------------------------------
 * Main function is main. Runs tests.
 *
 * Returns number of failed tests.
 * ---------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    HDprintf("Testing minimized dataset object headers.\n");

    nerrors += test_attribute_addition();
    nerrors += test_size_comparisons();
    nerrors += test_minimized_with_filter();
    nerrors += test_modification_times(); /* is this valid for datasets? */
    nerrors += test_fillvalue_backwards_compatability();
    /* nerrors += test_external_file_links() */ /* TOOD */

    if (nerrors > 0) {
        HDprintf("***** %d MINIMIZED DATASET OHDR TEST%s FAILED! *****\n",
                nerrors,
                nerrors > 1 ? "S" : "");
    } else {
        HDprintf("All minimized dataset object header tests passed.\n");
    }

    return nerrors;
} /* main */


