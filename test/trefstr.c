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
   FILE
       refstr.c
   Test HDF reference counted string routines.

   REMARKS

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES
 */

#include "testhdf5.h"
#include "H5FLprivate.h"
#include "H5RSprivate.h"

/* Declare extern the PQ free list for the wrapped strings */
H5FL_BLK_EXTERN(str_buf);

/****************************************************************
**
**  test_refstr_init(): Test basic H5RS (ref-counted strings) code.
**      Initialize data for RS testing
**
****************************************************************/
static void
test_refstr_init(void)
{
} /* end test_refstr_init() */

/****************************************************************
**
**  test_refstr_create(): Test basic H5RS (ref-counted strings) code.
**      Tests creating and closing ref-counted strings.
**
****************************************************************/
static void
test_refstr_create(void)
{
    H5RS_str_t *rs;    /* Ref-counted string created */
    unsigned    count; /* Reference count on string */
    herr_t      ret;   /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Creating & Closing Ref-Counted Strings\n"));

    /* Try creating a ref-counted string */
    rs = H5RS_create("foo");
    CHECK_PTR(rs, "H5RS_create");

    /* Get the reference count on the string */
    count = H5RS_get_count(rs);
    VERIFY(count, 1, "H5RS_get_count");

    /* Try closing a real ref-counted string */
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_create() */

/****************************************************************
**
**  test_refstr_count(): Test basic H5RS (ref-counted strings) code.
**      Tests reference counting on ref-counted strings.
**
****************************************************************/
static void
test_refstr_count(void)
{
    H5RS_str_t *rs;    /* Ref-counted string created */
    unsigned    count; /* Reference count on string */
    herr_t      ret;   /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Incrementing & Decrementing Ref-Counted Strings\n"));

    /* Try creating a ref-counted string */
    rs = H5RS_create("foo");
    CHECK_PTR(rs, "H5RS_create");

    /* Get the reference count on the string */
    count = H5RS_get_count(rs);
    VERIFY(count, 1, "H5RS_get_count");

    /* Increment reference count */
    ret = H5RS_incr(rs);
    CHECK(ret, FAIL, "H5RS_incr");

    /* Get the reference count on the string */
    count = H5RS_get_count(rs);
    VERIFY(count, 2, "H5RS_get_count");

    /* Decrement reference count for string */
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

    /* Get the reference count on the string */
    count = H5RS_get_count(rs);
    VERIFY(count, 1, "H5RS_get_count");

    /* Decrement reference count for string */
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_count() */

/****************************************************************
**
**  test_refstr_dup(): Test basic H5RS (ref-counted strings) code.
**      Tests duplicating ref-counted strings.
**
****************************************************************/
static void
test_refstr_dup(void)
{
    H5RS_str_t *rs1;   /* Ref-counted string created */
    H5RS_str_t *rs2;   /* Ref-counted string created */
    unsigned    count; /* Reference count on string */
    herr_t      ret;   /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Duplicating Ref-Counted Strings\n"));

    /* Try creating a ref-counted string */
    rs1 = H5RS_create("foo");
    CHECK_PTR(rs1, "H5RS_create");

    /* Get the reference count on the string */
    count = H5RS_get_count(rs1);
    VERIFY(count, 1, "H5RS_get_count");

    /* Duplicate r-string */
    rs2 = H5RS_dup(rs1);
    CHECK_PTR(rs2, "H5RS_dup");

    /* Get the reference count on the strings */
    count = H5RS_get_count(rs1);
    VERIFY(count, 2, "H5RS_get_count");
    count = H5RS_get_count(rs2);
    VERIFY(count, 2, "H5RS_get_count");

    /* Decrement reference count for string */
    ret = H5RS_decr(rs2);
    CHECK(ret, FAIL, "H5RS_decr");

    /* Get the reference count on the string */
    count = H5RS_get_count(rs1);
    VERIFY(count, 1, "H5RS_get_count");

    /* Decrement reference count for string */
    ret = H5RS_decr(rs1);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_dup() */

/****************************************************************
**
**  test_refstr_cmp(): Test basic H5RS (ref-counted strings) code.
**      Tests comparing ref-counted strings.
**
****************************************************************/
static void
test_refstr_cmp(void)
{
    H5RS_str_t *rs1; /* Ref-counted string created */
    H5RS_str_t *rs2; /* Ref-counted string created */
    int         cmp; /* Comparison value */
    size_t      len; /* Length of string */
    herr_t      ret; /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Comparing Ref-Counted Strings\n"));

    /* Create first reference counted string */
    rs1 = H5RS_create("foo");
    CHECK_PTR(rs1, "H5RS_create");

    /* Create second reference counted string */
    rs2 = H5RS_create("foo2");
    CHECK_PTR(rs2, "H5RS_create");

    /* Compare the strings in various ways */
    cmp = H5RS_cmp(rs1, rs1);
    VERIFY(cmp, 0, "H5RS_cmp");
    cmp = H5RS_cmp(rs2, rs2);
    VERIFY(cmp, 0, "H5RS_cmp");
    cmp = H5RS_cmp(rs1, rs2);
    if (cmp >= 0)
        TestErrPrintf("%d: string comparison incorrect!\n", __LINE__);

    /* Check the lengths of the strings also */
    len = H5RS_len(rs1);
    VERIFY(len, 3, "H5RS_len");
    len = H5RS_len(rs2);
    VERIFY(len, 4, "H5RS_len");

    /* Decrement reference count for strings */
    ret = H5RS_decr(rs2);
    CHECK(ret, FAIL, "H5RS_decr");
    ret = H5RS_decr(rs1);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_cmp() */

/****************************************************************
**
**  test_refstr_wrap(): Test basic H5RS (ref-counted strings) code.
**      Tests wrapping ref-counted strings around existing strings.
**
****************************************************************/
static void
test_refstr_wrap(void)
{
    H5RS_str_t *rs;      /* Ref-counted string created */
    const char *s;       /* Pointer to raw string in ref-counted string */
    char        buf[16]; /* Buffer to wrap */
    int         cmp;     /* Comparison value */
    herr_t      ret;     /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Wrapping Ref-Counted Strings\n"));

    /* Initialize buffer */
    strcpy(buf, "foo");

    /* Wrap ref-counted string around existing buffer */
    rs = H5RS_wrap(buf);
    CHECK_PTR(rs, "H5RS_wrap");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    CHECK_PTR_EQ(s, buf, "wrapping");
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Increment reference count (should duplicate string) */
    ret = H5RS_incr(rs);
    CHECK(ret, FAIL, "H5RS_incr");

    /* Change the buffer initially wrapped */
    buf[0] = 'F';

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    if (s == buf)
        TestErrPrintf("%d: Should not have gotten the same pointer from reference-counted string!\n",
                      __LINE__);
    cmp = strcmp(s, buf);
    if (cmp <= 0)
        TestErrPrintf("%d: string comparison incorrect!\n", __LINE__);

    /* Decrement reference count for string */
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_wrap() */

/****************************************************************
**
**  test_refstr_asprintf_cat(): Test basic H5RS (ref-counted strings) code.
**      Tests appending printf-formatted output to ref-counted strings.
**
****************************************************************/
static void
test_refstr_asprintf_cat(void)
{
    H5RS_str_t *rs;       /* Ref-counted string created */
    const char *s;        /* Pointer to raw string in ref-counted string */
    char        buf[256]; /* Buffer to compare against */
    int         cmp;      /* Comparison value */
    herr_t      ret;      /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Printf-formatted Output to Ref-Counted Strings\n"));

    /* Wrap ref-counted string around existing buffer */
    rs = H5RS_create(NULL);
    CHECK_PTR(rs, "H5RS_create");

    /* Print initial output to ref-counted string */
    ret = H5RS_asprintf_cat(rs, "%d-%s", (int)10, "foo");
    CHECK(ret, FAIL, "H5RS_asprintf_cat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    snprintf(buf, sizeof(buf), "%d-%s", (int)10, "foo");
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Append more output to ref-counted string */
    ret = H5RS_asprintf_cat(rs, "-%f", (double)20.0);
    CHECK(ret, FAIL, "H5RS_asprintf_cat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    snprintf(buf, sizeof(buf), "%d-%s-%f", (int)10, "foo", (double)20.0);
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Decrement reference count for string */
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

} /* end test_refstr_asprintf_cat() */

/****************************************************************
**
**  test_refstr_acat(): Test basic H5RS (ref-counted strings) code.
**      Tests appending strings to ref-counted strings.
**
****************************************************************/
static void
test_refstr_acat(void)
{
    H5RS_str_t *rs;                     /* Ref-counted string created */
    const char *s;                      /* Pointer to raw string in ref-counted string */
    char        buf[256];               /* Buffer to compare against */
    char       *large_str, *large_str2; /* Large strings to append */
    int         cmp;                    /* Comparison value */
    herr_t      ret;                    /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Appending Strings to Ref-Counted Strings\n"));

    /* Wrap ref-counted string around existing buffer */
    rs = H5RS_create(NULL);
    CHECK_PTR(rs, "H5RS_create");

    /* Append first string to ref-counted string */
    ret = H5RS_acat(rs, "foo");
    CHECK(ret, FAIL, "H5RS_acat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    snprintf(buf, sizeof(buf), "%s", "foo");
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Append another string to ref-counted string */
    ret = H5RS_acat(rs, "bar");
    CHECK(ret, FAIL, "H5RS_acat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    snprintf(buf, sizeof(buf), "%s", "foobar");
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Append a large string to ref-counted string */
    large_str = malloc(1024);
    CHECK_PTR(large_str, "malloc");
    memset(large_str, 'a', 1024);
    large_str[1023] = '\0';
    ret             = H5RS_acat(rs, large_str);
    CHECK(ret, FAIL, "H5RS_acat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    snprintf(buf, sizeof(buf), "%s", "foobar");
    large_str2 = malloc(1024 + 6);
    CHECK_PTR(large_str2, "malloc");
    strcpy(large_str2, "foobar");
    memset(&large_str2[6], 'a', 1024);
    large_str2[1029] = '\0';
    cmp              = strcmp(s, large_str2);
    VERIFY(cmp, 0, "strcmp");

    /* Decrement reference count for string */
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");

    /* Free large strings */
    free(large_str);
    free(large_str2);
} /* end test_refstr_acat() */

/****************************************************************
**
**  test_refstr_ancat(): Test basic H5RS (ref-counted strings) code.
**      Tests appending length-limited strings to ref-counted strings.
**
****************************************************************/
static void
test_refstr_ancat(void)
{
    H5RS_str_t *rs;       /* Ref-counted string created */
    const char *s;        /* Pointer to raw string in ref-counted string */
    char        buf[256]; /* Buffer to compare against */
    int         cmp;      /* Comparison value */
    herr_t      ret;      /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Appending Strings to Ref-Counted Strings\n"));

    /* Wrap ref-counted string around existing buffer */
    rs = H5RS_create(NULL);
    CHECK_PTR(rs, "H5RS_create");

    /* Append first string to ref-counted string */
    ret = H5RS_ancat(rs, "foo", 2);
    CHECK(ret, FAIL, "H5RS_ancat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    strcpy(buf, "fo");
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Append another string to ref-counted string */
    ret = H5RS_ancat(rs, "bar", 2);
    CHECK(ret, FAIL, "H5RS_ancat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    strcpy(buf, "foba");
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Decrement reference count for string */
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");
} /* end test_refstr_ancat() */

/****************************************************************
**
**  test_refstr_aputc(): Test basic H5RS (ref-counted strings) code.
**      Tests appending characters to ref-counted strings.
**
****************************************************************/
static void
test_refstr_aputc(void)
{
    H5RS_str_t *rs;       /* Ref-counted string created */
    const char *s;        /* Pointer to raw string in ref-counted string */
    char        buf[256]; /* Buffer to compare against */
    int         cmp;      /* Comparison value */
    herr_t      ret;      /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Appending Strings to Ref-Counted Strings\n"));

    /* Wrap ref-counted string around existing buffer */
    rs = H5RS_create(NULL);
    CHECK_PTR(rs, "H5RS_create");

    /* Append first character to ref-counted string */
    ret = H5RS_aputc(rs, 'f');
    CHECK(ret, FAIL, "H5RS_ancat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    strcpy(buf, "f");
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Append another character to ref-counted string */
    ret = H5RS_aputc(rs, 'o');
    CHECK(ret, FAIL, "H5RS_ancat");

    /* Get pointer to raw string in ref-counted string */
    s = H5RS_get_str(rs);
    CHECK_PTR(s, "H5RS_get_str");
    strcpy(buf, "fo");
    cmp = strcmp(s, buf);
    VERIFY(cmp, 0, "strcmp");

    /* Decrement reference count for string */
    ret = H5RS_decr(rs);
    CHECK(ret, FAIL, "H5RS_decr");
} /* end test_refstr_aputc() */

/****************************************************************
**
**  test_refstr_finalize(): Test basic H5RS (ref-counted strings) code.
**      Wrap up data for ref-counted string testing
**
****************************************************************/
static void
test_refstr_finalize(void)
{
} /* end test_refstr_finalize() */

/****************************************************************
**
**  test_refstr(): Main H5RS testing routine.
**
****************************************************************/
void
test_refstr(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Reference Counted Strings\n"));

    /* Initialize ref-counted strings testing data */
    test_refstr_init();

    /* Actual ref-counted strings tests */
    test_refstr_create();       /* Test ref-counted string creation */
    test_refstr_count();        /* Test ref-counted string counting */
    test_refstr_dup();          /* Test ref-counted string duplication */
    test_refstr_cmp();          /* Test ref-counted string comparison */
    test_refstr_wrap();         /* Test ref-counted string wrapping */
    test_refstr_asprintf_cat(); /* Test ref-counted string printf-formatted output */
    test_refstr_acat();         /* Test ref-counted string appends */
    test_refstr_ancat();        /* Test ref-counted length-limited string appends */
    test_refstr_aputc();        /* Test ref-counted character appends */

    /* Finalize ref-counted strings testing data */
    test_refstr_finalize();
} /* end test_refstr() */
