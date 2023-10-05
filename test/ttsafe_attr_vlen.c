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

/********************************************************************
 *
 * Testing for thread safety in H5A library operations.
 * ------------------------------------------------------------------
 *
 * Purpose: Verify that the segmentation fault described in HDFFV-11080
 *          is fixed.
 *
 *          This test simulates what the user did to trigger the error:
 *          --Create an HDF5 file
 *          --Create an attribute with variable length string datatype
 *          --Attach the attribute to a group
 *          --Write data to the attribute
 *          --Close the file
 *          --Create NUM_THREADS threads
 *          --For each thread:
 *              --Open the test file
 *              --Open and read the attribute for each opened file
 *
 *          The cause of the problem in this jira issue is due to the file pointer
 *          that is set in the variable length string datatype for the attribute.
 *          That file pointer is already closed and therefore needs to be set to
 *          the current opened file pointer when the attribute is accessed.
 *          Similar patch up was done before when reading dataset in H5D__read()
 *          in src/H5Aint.c.
 *          Hopefully this kind of patch can go away when we resolve the
 *          shared file pointer issue.
 *
 ********************************************************************/

#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE

#define FILENAME    "ttsafe_attr_vlen.h5"
#define ATTR_NAME   "root_attr"
#define NUM_THREADS 32

void *tts_attr_vlen_thread(void *);

void
tts_attr_vlen(void)
{
    H5TS_thread_t threads[NUM_THREADS] = {0};             /* Thread declaration */
    hid_t         fid                  = H5I_INVALID_HID; /* File ID */
    hid_t         gid                  = H5I_INVALID_HID; /* Group ID */
    hid_t         atid                 = H5I_INVALID_HID; /* Datatype ID for attribute */
    hid_t         asid                 = H5I_INVALID_HID; /* Dataspace ID for attribute */
    hid_t         aid                  = H5I_INVALID_HID; /* The attribute ID */
    const char   *string_attr          = "2.0";           /* The attribute data */
    int           ret;                                    /* Return value */
    int           i;                                      /* Local index variable */

    /* Create the HDF5 test file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Create variable length string type for attribute */
    atid = H5Tcopy(H5T_C_S1);
    CHECK(atid, H5I_INVALID_HID, "H5Tcopy");
    H5Tset_size(atid, H5T_VARIABLE);

    /* Create dataspace for attribute */
    asid = H5Screate(H5S_SCALAR);
    CHECK(asid, H5I_INVALID_HID, "H5Screate");

    /* Open the root group */
    gid = H5Gopen2(fid, "/", H5P_DEFAULT);
    CHECK(gid, H5I_INVALID_HID, "H5Gopen2");

    /* Attach the attribute to the root group */
    aid = H5Acreate2(gid, ATTR_NAME, atid, asid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(aid, H5I_INVALID_HID, "H5Acreate2");

    /* Write data to the attribute */
    ret = H5Awrite(aid, atid, &string_attr);
    CHECK(ret, H5I_INVALID_HID, "H5Awrite");

    /* Close IDs */
    ret = H5Sclose(asid);
    CHECK(ret, H5I_INVALID_HID, "H5Sclose");

    ret = H5Aclose(aid);
    CHECK(ret, H5I_INVALID_HID, "H5Aclose");

    ret = H5Gclose(gid);
    CHECK(ret, H5I_INVALID_HID, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, H5I_INVALID_HID, "H5Fclose");

    ret = H5Tclose(atid);
    CHECK(ret, H5I_INVALID_HID, "H5Tclose");

    /* Start multiple threads and execute tts_attr_vlen_thread() for each thread */
    for (i = 0; i < NUM_THREADS; i++) {
        threads[i] = H5TS_create_thread(tts_attr_vlen_thread, NULL, NULL);
    }

    /* Wait for the threads to end */
    for (i = 0; i < NUM_THREADS; i++)
        H5TS_wait_for_thread(threads[i]);

} /* end tts_attr_vlen() */

/* Start execution for each thread */
void *
tts_attr_vlen_thread(void H5_ATTR_UNUSED *client_data)
{
    hid_t       fid  = H5I_INVALID_HID; /* File ID */
    hid_t       gid  = H5I_INVALID_HID; /* Group ID */
    hid_t       aid  = H5I_INVALID_HID; /* Attribute ID */
    hid_t       atid = H5I_INVALID_HID; /* Datatype ID for the attribute */
    char       *string_attr_check;      /* The attribute data being read */
    const char *string_attr = "2.0";    /* The expected attribute data */
    herr_t      ret;                    /* Return value */

    /* Open the test file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, H5I_INVALID_HID, "H5Fopen");

    /* Open the group */
    gid = H5Gopen2(fid, "/", H5P_DEFAULT);
    CHECK(gid, H5I_INVALID_HID, "H5Gopen");

    /* Open the attribute */
    aid = H5Aopen(gid, "root_attr", H5P_DEFAULT);
    CHECK(aid, H5I_INVALID_HID, "H5Aopen");

    /* Get the attribute datatype */
    atid = H5Aget_type(aid);
    CHECK(atid, H5I_INVALID_HID, "H5Aget_type");

    /* Read the attribute */
    ret = H5Aread(aid, atid, &string_attr_check);
    CHECK(ret, FAIL, "H5Aclose");

    /* Verify the attribute data is as expected */
    VERIFY_STR(string_attr_check, string_attr, "H5Aread");

    /* Close IDs */
    ret = H5Aclose(aid);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Tclose(atid);
    CHECK(ret, FAIL, "H5Aclose");

    return NULL;
} /* end tts_attr_vlen_thread() */

void
cleanup_attr_vlen(void)
{
    HDunlink(FILENAME);
}

#endif /*H5_HAVE_THREADSAFE*/
