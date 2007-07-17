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

/* Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, November 24, 1998
 */
#include "h5test.h"
#include "H5Iprivate.h"
/*
 * This file needs to access private datatypes from the H5O package.
 * This file also needs to access the object header testing code.
 */
#define H5O_PACKAGE
#define H5O_TESTING
#include "H5Opkg.h"

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include "H5Gpkg.h"

const char *FILENAME[] = {
    "ohdr",
    NULL
};

/* The tbogus.h5 is generated from gen_bogus.c in HDF5 'test' directory.
 * To get this data file, define H5O_ENABLE_BOGUS in src/H5Oprivate, rebuild
 * the library and simply compile gen_bogus.c with that HDF5 library and run it.
 */
#define FILE_BOGUS "tbogus.h5"


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl=-1, file=-1;
    hid_t	dset=-1;
    H5F_t	*f=NULL;
    char	filename[1024];
    H5O_info_t  oinfo;                  /* Object info */
    H5O_loc_t	oh_loc;
    time_t	time_new, ro;
    int		i;
    hbool_t     b;                      /* Index for "new format" loop */
    const char  *envval = NULL;
    herr_t      ret;                    /* Generic return value */
 
    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Loop over old & new formats */
    for(b = FALSE; b <= TRUE; b++) {
        /* Display info about testing */
        if(b)
            HDputs("Using new file format:");
        else
            HDputs("Using default file format:");

        /* Set the format to use for the file */
        if (H5Pset_latest_format(fapl, b) < 0) FAIL_STACK_ERROR

        /* Create the file to operate on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
        if(NULL == (f = H5I_object(file))) FAIL_STACK_ERROR


        /*
         * Test object header creation
         * (using default group creation property list only because it's convenient)
         */
        TESTING("object header creation");
        HDmemset(&oh_loc, 0, sizeof(oh_loc));
        if(H5O_create(f, H5P_DATASET_XFER_DEFAULT, (size_t)64, H5P_GROUP_CREATE_DEFAULT, &oh_loc/*out*/) < 0)
            FAIL_STACK_ERROR
        PASSED();


        /* create a new message */
        TESTING("message creation");
        time_new = 11111111;
        if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT, TRUE) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR
        PASSED();


        /*
         * Test modification of an existing message.
         */
        TESTING("message modification");
        time_new = 33333333;
        if(H5O_msg_write(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT, TRUE) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR

        /* Make certain that chunk #0 in the object header can be encoded with a 1-byte size */
        if(H5O_get_info(&oh_loc, H5P_DATASET_XFER_DEFAULT, FALSE, &oinfo) < 0)
            FAIL_STACK_ERROR
        if(oinfo.hdr.space.total >=256)
            TEST_ERROR

        PASSED();

        /*
         * Test creation of a bunch of messages one after another to see
         * what happens when the object header overflows in core.
         * (Use 'old' MTIME message here, because it is large enough to be
         *  replaced with a continuation message (the new one is too small)
         *  and the library doesn't understand how to migrate more than one
         *  message from an object header currently - QAK - 10/8/03)
         */
        TESTING("object header overflow in memory");
        for(i = 0; i < 40; i++) {
            time_new = (i + 1) * 1000 + 1;
            if(H5O_msg_create(&oh_loc, H5O_MTIME_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
                FAIL_STACK_ERROR
        } /* end for */
        if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT, TRUE) < 0)
            FAIL_STACK_ERROR

        /* Make certain that chunk #0 in the object header will be encoded with a 2-byte size */
        if(H5O_get_info(&oh_loc, H5P_DATASET_XFER_DEFAULT, FALSE, &oinfo) < 0)
            FAIL_STACK_ERROR
        if(oinfo.hdr.space.total < 256)
            TEST_ERROR

        PASSED();

        /* Close & re-open file & object header */
        /* (makes certain that an object header in the new format that transitions
         *  between 1-byte chunk #0 size encoding and 2-byte chunk #0 size encoding
         *  works correctly - QAK)
         */
        TESTING("close & re-open object header");
        envval = HDgetenv("HDF5_DRIVER");
        if(envval == NULL) 
            envval = "nomatch";
        if(HDstrcmp(envval, "multi") && HDstrcmp(envval, "split") && HDstrcmp(envval, "family")) {
            if(H5O_close(&oh_loc) < 0)
                FAIL_STACK_ERROR
            if(H5Fclose(file) < 0)
                FAIL_STACK_ERROR
            if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR
            if(NULL == (f = H5I_object(file)))
                FAIL_STACK_ERROR
            oh_loc.file = f;
            if(H5O_open(&oh_loc) < 0)
                FAIL_STACK_ERROR
            PASSED();
        } /* end if */
        else {
            SKIPPED();
            puts("   Test not compatible with current Virtual File Driver");
        } /* end else */

        /*
         * Test creation of a bunch of messages one after another to see
         * what happens when the object header overflows on disk.
         */
        TESTING("object header overflow on disk");
        for(i = 0; i < 10; i++) {
            time_new = (i + 1) * 1000 + 10;
            if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
                FAIL_STACK_ERROR
            if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT, TRUE) < 0)
                FAIL_STACK_ERROR
        } /* end for */
        PASSED();

        /*
         * Delete all time messages.
         */
        TESTING("message deletion");
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, TRUE, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_ID, H5O_ALL, TRUE, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(H5O_msg_read(&oh_loc, H5O_MTIME_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        PASSED();


        /*
         * Constant message handling.
         * (can't write to them, but should be able to remove them)
         */
        TESTING("constant message handling");
        time_new = 22222222;
        if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, H5O_MSG_FLAG_CONSTANT, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT, TRUE) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR
        time_new = 33333333;
        H5E_BEGIN_TRY {
            ret = H5O_msg_write(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0)
            TEST_ERROR
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, TRUE, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        PASSED();


        /* release resources */
        TESTING("object header closing");
        if(H5O_close(&oh_loc) < 0)
            FAIL_STACK_ERROR
        PASSED();


        /* Test reading datasets with undefined object header messages */
        HDputs("Accessing objects with unknown header messages:");
        envval = HDgetenv("HDF5_DRIVER");
        if(envval == NULL) 
            envval = "nomatch";
        if(HDstrcmp(envval, "multi") && HDstrcmp(envval, "split") && HDstrcmp(envval, "family")) {
            hid_t file2;                    /* File ID for 'bogus' object file */
            char testpath[512] = "";
            char testfile[512] = "";
            char *srcdir = HDgetenv("srcdir");

            /* Build path to all test files */
            if(srcdir && ((HDstrlen(srcdir) + 2) < sizeof(testpath))) {
                HDstrcpy(testpath, srcdir);
                HDstrcat(testpath, "/");
            } /* end if */

            /* Build path to test file */
            if(srcdir && ((HDstrlen(testpath) + HDstrlen(FILE_BOGUS) + 1) < sizeof(testfile)))
                HDstrcpy(testfile, testpath);
            HDstrcat(testfile, FILE_BOGUS);

            TESTING("object with unknown header message and no flags set");

            /* Open the file with objects that have unknown header messages (generated with gen_bogus.c) */
            if((file2 = H5Fopen(testfile, H5F_ACC_RDONLY, fapl)) < 0)
                TEST_ERROR

            /* Open the dataset with the unknown header message, but no extra flags */
            if((dset = H5Dopen(file2, "/Dataset1")) < 0)
                TEST_ERROR
            if(H5Dclose(dset) < 0)
                TEST_ERROR

            PASSED();

            TESTING("object with unknown header message & 'fail if unknown' flag set");

            /* Attempt to open the dataset with the unknown header message, and "fail if unknown" flag */
            H5E_BEGIN_TRY {
                dset = H5Dopen(file2, "/Dataset2");
            } H5E_END_TRY;
            if(dset >= 0) {
                H5Dclose(dset);
                TEST_ERROR
            } /* end if */

            PASSED();

            TESTING("object with unknown header message & 'mark if unknown' flag set");

            /* Copy object with "mark if unknown" flag on message into file that can be modified */
            if(H5Ocopy(file2, "/Dataset3", file, "/Dataset3", H5P_DEFAULT, H5P_DEFAULT) < 0)
                TEST_ERROR

            /* Close the file we created (to flush changes to file) */
            if(H5Fclose(file) < 0)
                TEST_ERROR

            /* Re-open the file created, with read-only permissions */
            if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
                TEST_ERROR

            /* Open the dataset with the "mark if unknown" message */
            if((dset = H5Dopen(file, "/Dataset3")) < 0)
                TEST_ERROR

            /* Check that the "unknown" message was _NOT_ marked */
            if(H5O_check_msg_marked_test(dset, FALSE) < 0)
                FAIL_STACK_ERROR

            /* Close the dataset */
            if(H5Dclose(dset) < 0)
                TEST_ERROR

            /* Close the file we created (to flush change to object header) */
            if(H5Fclose(file) < 0)
                TEST_ERROR

            /* Re-open the file created */
            if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                TEST_ERROR

            /* Open the dataset with the "mark if unknown" message */
            if((dset = H5Dopen(file, "/Dataset3")) < 0)
                TEST_ERROR
            if(H5Dclose(dset) < 0)
                TEST_ERROR

            /* Close the file we created (to flush change to object header) */
            if(H5Fclose(file) < 0)
                TEST_ERROR

            /* Re-open the file created */
            if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
                TEST_ERROR

            /* Re-open the dataset with the "mark if unknown" message */
            if((dset = H5Dopen(file, "/Dataset3")) < 0)
                TEST_ERROR

            /* Check that the "unknown" message was marked */
            if(H5O_check_msg_marked_test(dset, TRUE) < 0)
                FAIL_STACK_ERROR

            /* Close the dataset */
            if(H5Dclose(dset) < 0)
                TEST_ERROR


            /* Close the file with the bogus objects */
            if(H5Fclose(file2) < 0)
                TEST_ERROR

            PASSED();
        } /* end if */
        else {
            SKIPPED();
            puts("   Test not compatible with current Virtual File Driver");
        } /* end else */

        /* Close the file we created */
        if(H5Fclose(file) < 0)
            TEST_ERROR
    } /* end for */

    puts("All object header tests passed.");
    h5_cleanup(FILENAME, fapl);
    return(0);

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* end main() */

