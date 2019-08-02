/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Serial tests for encoding/decoding plists
 */

#include "h5test.h"
#include "H5srcdir.h"

#define CONFIG_LE   0x01
#define CONFIG_64   0x02
#define NCONFIG     0x04

static int test_plists(const char *filename_prefix);

int
main(void)
{
    if(VERBOSE_MED)
        HDprintf("Encode/Decode property list endianess\n");

    /******* ENCODE/DECODE DCPLS *****/
    TESTING("Default DCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_dcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("DCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/dcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE DAPLS *****/
    TESTING("Default DAPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_dapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("DAPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/dapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE DXPLS *****/
    TESTING("Default DXPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_dxpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("DXPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/dxpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE GCPLS *****/
    TESTING("Default GCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_gcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("GCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/gcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE LCPLS *****/
    TESTING("Default LCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_lcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("LCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/lcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE LAPLS *****/
    TESTING("Default LAPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_lapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("LAPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/lapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE OCPLS *****/
    TESTING("Default OCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_ocpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("OCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/ocpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE OCPYPLS *****/
    TESTING("Default OCPYPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_ocpypl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("OCPYPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/ocpypl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE FCPLS *****/
    TESTING("Default FCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_fcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("FCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/fcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE FAPLS *****/
    TESTING("Default FAPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_fapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("FAPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/fapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE STRCPLS *****/
    TESTING("Default STRCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_strcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("STRCPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/strcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE ACPLS *****/
    TESTING("Default ACPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/def_acpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("ACPL Encoding/Decoding");
    if(test_plists("testfiles/plist_files/acpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    return 0;

error:
    return 1;
}

static int
test_plists(const char *filename_prefix) 
{
    unsigned config_1, config_2;
    int fd_1, fd_2;
    size_t size_1 = 0, size_2 = 0;
    void *buf_1 = NULL, *buf_2 = NULL;
    hid_t plist_1, plist_2;
    char filename[1024];
    const char *testfile;

    /* Iterate over all combinations of configurations */
    for(config_1 = 0; config_1 < (NCONFIG - 1); config_1++)
        for(config_2 = config_1 + 1; config_2 < NCONFIG; config_2++) {
            /* Generate filename for file 1 */
            if(HDsnprintf(filename, sizeof(filename), "%s%s%s", filename_prefix,
                    config_1 & CONFIG_64 ? "64" : "32",
                    config_1 & CONFIG_LE ? "le" : "be") < 0)
                TEST_ERROR

            /* Read file 1 */
            testfile = H5_get_srcdir_filename(filename);
            if((fd_1 = HDopen(testfile, O_RDONLY)) < 0)
                TEST_ERROR
            size_1 = (size_t)HDlseek(fd_1, (HDoff_t)0, SEEK_END);
            HDlseek(fd_1, (HDoff_t)0, SEEK_SET);
            buf_1 = (void *)HDmalloc(size_1);
            if(HDread(fd_1, buf_1, size_1) < 0)
                TEST_ERROR
            HDclose(fd_1);

            /* Generate filename for file 2 */
            if(HDsnprintf(filename, sizeof(filename), "%s%s%s", filename_prefix,
                    config_2 & CONFIG_64 ? "64" : "32",
                    config_2 & CONFIG_LE ? "le" : "be") < 0)
                TEST_ERROR

            /* Read file 1 */
            testfile = H5_get_srcdir_filename(filename);
            if((fd_2 = HDopen(testfile, O_RDONLY)) < 0)
                TEST_ERROR
            size_2 = (size_t)HDlseek(fd_2, (HDoff_t)0, SEEK_END);
            HDlseek(fd_2, (HDoff_t)0, SEEK_SET);
            buf_2 = (void *)HDmalloc(size_2);
            if(HDread(fd_2, buf_2, size_2) < 0)
                TEST_ERROR
            HDclose(fd_2);

            /* Decode property lists */
            if((plist_1 = H5Pdecode(buf_1)) < 0)
                FAIL_STACK_ERROR
            if((plist_2 = H5Pdecode(buf_2)) < 0)
                FAIL_STACK_ERROR

            /* Compare decoded property lists */
            if(!H5Pequal(plist_1, plist_2))
                FAIL_PUTS_ERROR("PLIST encoding/decoding comparison failed\n")

            /* Close */
            if((H5Pclose(plist_1)) < 0)
                FAIL_STACK_ERROR
            if((H5Pclose(plist_2)) < 0)
                FAIL_STACK_ERROR

            HDfree(buf_1);
            HDfree(buf_2);
        } /* end for */

    return 1;

error:
    HDprintf("***** Plist Encode/Decode tests FAILED! *****\n");
    return -1;
}

