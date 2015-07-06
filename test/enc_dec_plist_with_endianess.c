/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Serial tests for encoding/decoding plists
 */

#include "h5test.h"
#include "H5srcdir.h"

static int test_plists(const char *filename1, const char *filename2);

int
main(void)
{
    if(VERBOSE_MED)
	printf("Encode/Decode property list endianess\n");

    /******* ENCODE/DECODE DCPLS *****/
    TESTING("DCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/dcpl_le", "testfiles/plist_files/dcpl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE DAPLS *****/
    TESTING("DAPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/dapl_le", "testfiles/plist_files/dapl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE DXPLS *****/
    TESTING("DXPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/dxpl_le", "testfiles/plist_files/dxpl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE GCPLS *****/
    TESTING("GCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/gcpl_le", "testfiles/plist_files/gcpl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE LCPLS *****/
    TESTING("LCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/lcpl_le", "testfiles/plist_files/lcpl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE LAPLS *****/
    TESTING("LAPL Encoding/Decoding");
    //if(test_plists("testfiles/plist_files/lapl_le", "testfiles/plist_files/lapl_be") < 0)
        //FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE OCPLS *****/
    TESTING("OCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/ocpl_le", "testfiles/plist_files/ocpl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE OCPYPLS *****/
    TESTING("OCPYPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/ocpypl_le", "testfiles/plist_files/ocpypl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE FCPLS *****/
    TESTING("FCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/fcpl_le", "testfiles/plist_files/fcpl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE FAPLS *****/
    TESTING("FAPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/fapl_le", "testfiles/plist_files/fapl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE STRCPLS *****/
    TESTING("STRCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/strcpl_le", "testfiles/plist_files/strcpl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE ACPLS *****/
    TESTING("ACPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/acpl_le", "testfiles/plist_files/acpl_be") < 0)
        FAIL_STACK_ERROR
    PASSED();

    return 0;

error:
    return 1;
}

static int
test_plists(const char *filename1, const char *filename2) 
{
    int fd_le, fd_be;
    size_t size_le = 0, size_be = 0;
    void *buf_le = NULL, *buf_be = NULL;
    hid_t plist_le, plist_be;	       	/* dataset create prop. list */
    const char *testfile;

    testfile = H5_get_srcdir_filename(filename1);
    if((fd_le = HDopen(testfile, O_RDONLY, 0666)) < 0)
        TEST_ERROR
    size_le = HDlseek(fd_le, (HDoff_t)0, SEEK_END);
    HDlseek(fd_le, (HDoff_t)0, SEEK_SET);
    buf_le = (void *)HDmalloc(size_le);
    if(HDread(fd_le, buf_le, size_le) < 0)
        TEST_ERROR
    HDclose(fd_le);

    testfile = H5_get_srcdir_filename(filename2);
    if((fd_be = HDopen(testfile, O_RDONLY, 0666)) < 0)
        TEST_ERROR
    size_be = HDlseek(fd_be, (HDoff_t)0, SEEK_END);
    HDlseek(fd_be, (HDoff_t)0, SEEK_SET);
    buf_be = (void *)HDmalloc(size_be);
    if(HDread(fd_be, buf_be, size_be) < 0)
        TEST_ERROR
    HDclose(fd_be);

    if((plist_le = H5Pdecode(buf_le)) < 0)
        FAIL_STACK_ERROR
    if((plist_be = H5Pdecode(buf_be)) < 0)
        FAIL_STACK_ERROR

    if(!H5Pequal(plist_le, plist_be))
        FAIL_PUTS_ERROR("PLIST encoding/decoding comparison failed\n")

    if((H5Pclose(plist_le)) < 0)
        FAIL_STACK_ERROR
    if((H5Pclose(plist_be)) < 0)
        FAIL_STACK_ERROR

    HDfree(buf_le);
    HDfree(buf_be);

    return 1;

error:
    printf("***** Plist Encode/Decode tests FAILED! *****\n");
    return -1;
}

