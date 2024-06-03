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
#include "h5test.h"

#define H5F_FRIEND  /*suppress error about including H5Fpkg	  */
#define H5FD_FRIEND /*suppress error about including H5FDpkg	  */
#define H5FD_TESTING

#include "H5Fpkg.h"
#include "H5FDpkg.h"

#include "H5CXprivate.h" /* API Contexts                         */
#include "H5Iprivate.h"
#include "H5VLprivate.h" /* Virtual Object Layer                     */

/* Filename: this is the same as the define in accum.c used by test_swmr_write_big() */
static const char *FILENAME[] = {"accum", "accum_swmr_big", NULL};

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     This is the reader forked/execved by "test_swmr_write_big()"
 *		        test in accum.c.  The reader reads at address 1024 from the file
 *		        and verifies that the metadata in the accumulator at address
 * 		        1024 does get written to disk.
 *
 * Return:      Success: EXIT_SUCCESS
 *              Failure: EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    const char *driver_name = NULL;            /* VFD string (from env variable) */
    hid_t       fid         = H5I_INVALID_HID; /* File ID */
    hid_t       fapl        = H5I_INVALID_HID; /* file access property list ID */
    H5F_t      *f           = NULL;            /* File pointer */
    char        filename[1024];
    unsigned    u;                      /* Local index variable */
    uint8_t     rbuf[1024];             /* Buffer for reading */
    uint8_t     buf[1024];              /* Buffer for holding the expected data */
    bool        api_ctx_pushed = false; /* Whether API context pushed */

    /* Testing setup */
    h5_reset();

    /* Skip this test if SWMR I/O is not supported for the VFD specified
     * by the environment variable.
     */
    driver_name = h5_get_test_driver_name();
    if (!H5FD__supports_swmr_test(driver_name))
        return EXIT_SUCCESS;

    /* Initialize buffers */
    for (u = 0; u < 1024; u++) {
        rbuf[u] = 0; /* The buffer for reading */
        buf[u]  = 1; /* The expected data should be all 1s */
    }

    if ((fapl = h5_fileaccess()) < 0)
        FAIL_STACK_ERROR;
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    /* Open the file with SWMR_READ */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Push API context */
    if (H5CX_push() < 0)
        FAIL_STACK_ERROR;
    api_ctx_pushed = true;

    /* Get H5F_t * to internal file structure */
    if (NULL == (f = (H5F_t *)H5VL_object(fid)))
        FAIL_STACK_ERROR;

    /* Should read in [1024, 2024] with buf data */
    if (H5F_block_read(f, H5FD_MEM_DEFAULT, (haddr_t)1024, (size_t)1024, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify the data read is correct */
    if (memcmp(buf, rbuf, (size_t)1024) != 0)
        TEST_ERROR;

    /* CLose the file */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Pop API context */
    if (api_ctx_pushed && H5CX_pop(false) < 0)
        FAIL_STACK_ERROR;
    api_ctx_pushed = false;

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (api_ctx_pushed)
        H5CX_pop(false);

    return EXIT_FAILURE;
} /* end main() */
