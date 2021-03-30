/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Serial tests for encoding/decoding plists
 */

#include "h5test.h"
#include "H5srcdir.h"

#define CONFIG_LE 0x01
#define CONFIG_64 0x02
#define NCONFIG   0x04

static int test_plists(const char *filename_prefix);

int
main(void)
{
    if (VERBOSE_MED)
        HDprintf("Encode/Decode property list endianess\n");

    /******* ENCODE/DECODE DCPLS *****/
    TESTING("Default DCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_dcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("DCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/dcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE DAPLS *****/
    TESTING("Default DAPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_dapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("DAPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/dapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE DXPLS *****/
    TESTING("Default DXPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_dxpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("DXPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/dxpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE GCPLS *****/
    TESTING("Default GCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_gcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("GCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/gcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE LCPLS *****/
    TESTING("Default LCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_lcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("LCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/lcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE LAPLS *****/
    TESTING("Default LAPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_lapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("LAPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/lapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE OCPLS *****/
    TESTING("Default OCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_ocpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("OCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/ocpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE OCPYPLS *****/
    TESTING("Default OCPYPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_ocpypl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("OCPYPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/ocpypl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE FCPLS *****/
    TESTING("Default FCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_fcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("FCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/fcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE FAPLS *****/
    TESTING("Default FAPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_fapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("FAPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/fapl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE STRCPLS *****/
    TESTING("Default STRCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_strcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("STRCPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/strcpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    /******* ENCODE/DECODE ACPLS *****/
    TESTING("Default ACPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/def_acpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();
    TESTING("ACPL Encoding/Decoding");
    if (test_plists("testfiles/plist_files/acpl_") < 0)
        FAIL_STACK_ERROR
    PASSED();

    return 0;

error:
    return 1;
}

static hid_t
read_and_decode_plist_file(const char *filename_prefix, unsigned config, char *filename, size_t filename_len)
{
    int         fd;
    size_t      size;
    const char *testfile;
    void *      buf   = NULL;
    hid_t       plist = H5I_INVALID_HID;

    /* Generate filename from prefix and configuration word */
    if (HDsnprintf(filename, filename_len, "%s%s%s", filename_prefix, config & CONFIG_64 ? "64" : "32",
                   config & CONFIG_LE ? "le" : "be") < 0)
        TEST_ERROR

    /* Read file 1 */
    testfile = H5_get_srcdir_filename(filename);
    if ((fd = HDopen(testfile, O_RDONLY)) < 0)
        TEST_ERROR
    size = (size_t)HDlseek(fd, 0, SEEK_END);
    HDlseek(fd, 0, SEEK_SET);
    buf = HDmalloc(size);
    if (HDread(fd, buf, size) < 0)
        TEST_ERROR
    HDclose(fd);

    /* Decode property lists */
    if ((plist = H5Pdecode(buf)) < 0)
        FAIL_STACK_ERROR
error:
    if (buf != NULL)
        HDfree(buf);
    return plist;
}

static int
test_plists(const char *filename_prefix)
{
    int      i;
    unsigned config[2];
    hid_t    plist[2];
    char     filename[2][1024];

    /* Iterate over all combinations of configurations */
    for (config[0] = 0; config[0] < (NCONFIG - 1); config[0]++) {
        for (config[1] = config[0] + 1; config[1] < NCONFIG; config[1]++) {
            for (i = 0; i < 2; i++) {
                plist[i] =
                    read_and_decode_plist_file(filename_prefix, config[i], filename[i], sizeof(filename[i]));
                if (plist[i] == H5I_INVALID_HID)
                    goto error;
            }

            /* Compare decoded property lists */
            if (!H5Pequal(plist[0], plist[1]))
                FAIL_PRINTF_ERROR("PLIST encoding/decoding comparison failed, "
                                  "%s != %s\n",
                                  filename[0], filename[1])

            for (i = 0; i < 2; i++) {
                if ((H5Pclose(plist[i])) < 0)
                    FAIL_STACK_ERROR
            }
        }
    }

    return 1;

error:
    HDprintf("***** Plist Encode/Decode tests FAILED! *****\n");
    return -1;
}
