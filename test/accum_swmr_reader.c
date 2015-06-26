/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
#include "h5test.h"

#define H5F_PACKAGE
#include "H5Fpkg.h"
#include "H5FDprivate.h"
#include "H5Iprivate.h"

/* Filename: this is the same as the define in accum.c used by test_swmr_write_big() */
#define SWMR_FILENAME "accum_swmr_big.h5"


/*-------------------------------------------------------------------------
 * Function:    main
 * 
 * Purpose:     This is the reader forked/execved by "test_swmr_write_big()" 
 *		test in accum.c.  The reader reads at address 1024 from the file
 *		and verifies that the metadata in the accumulator at address
 * 		1024 does get written to disk.
 * 
 * Return:      Success: 0
 *              Failure: 1
 * 
 * Programmer:  Vailin Choi; June 2013
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fid = -1;	/* File ID */
    H5F_t *f = NULL;	/* File pointer */
    unsigned u;		/* Local index variable */
    uint8_t rbuf[1024];	/* Buffer for reading */
    uint8_t buf[1024];	/* Buffer for holding the expected data */

    /* Initialize buffers */
    for(u = 0; u < 1024; u++) {
        rbuf[u] = 0;	/* The buffer for reading */
        buf[u] = 1;	/* The expected data should be all 1s */
    }

    /* Open the file with SWMR_READ */
    if((fid = H5Fopen(SWMR_FILENAME, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Get H5F_t * to internal file structure */
    if(NULL == (f = (H5F_t *)H5I_object(fid))) 
	FAIL_STACK_ERROR

    /* Should read in [1024, 2024] with buf data */
    if(H5F_block_read(f, H5FD_MEM_DEFAULT, (haddr_t)1024, (size_t)1024, H5P_DATASET_XFER_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;

    /* Verify the data read is correct */
    if(HDmemcmp(buf, rbuf, (size_t)1024) != 0) 
	TEST_ERROR;

    /* CLose the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    return 0;

error: 
    H5Fclose(fid);
    return 1;
} /* end main() */
