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
 * Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Wednesday, March 17, 2010
 *
 * Purpose:     srcdir querying support.
 */
#ifndef _EXTERNAL_H
#define _EXTERNAL_H

/* Include test header files */
#include "h5test.h"

const char *FILENAME[] = {
    "extern_1",
    "extern_2",
    "extern_3",
    "extern_4",
    "extern_dir/file_1",
    "extern_5",
    NULL
};

/* A similar collection of files is used for the tests that
 * perform file I/O.
 */
#define N_EXT_FILES         4
#define PART_SIZE           25
#define TOTAL_SIZE          100
#define GARBAGE_PER_FILE    10


/*-------------------------------------------------------------------------
 * Function:    reset_raw_data_files
 *
 * Purpose:     Resets the data in the raw data files for tests that
 *              perform dataset I/O on a set of files.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              February 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
reset_raw_data_files(void)
{
    int		    fd = 0;             /* external file descriptor             */
    size_t	    i, j;               /* iterators                            */
    hssize_t	n;                  /* bytes of I/O                         */
    char	    filename[1024];     /* file name                            */
    int		    data[PART_SIZE];    /* raw data buffer                      */
    uint8_t     *garbage = NULL;    /* buffer of garbage data               */
    size_t      garbage_count;      /* size of garbage buffer               */
    size_t      garbage_bytes;      /* # of garbage bytes written to file   */

    /* Set up garbage buffer */
    garbage_count = N_EXT_FILES * GARBAGE_PER_FILE;
    if(NULL == (garbage = (uint8_t *)HDcalloc(garbage_count, sizeof(uint8_t))))
        goto error;
    for(i = 0; i < garbage_count; i++)
        garbage[i] = 0xFF;

    /* The *r files are pre-filled with data and are used to
     * verify that read operations work correctly.
     */
    for(i = 0; i < N_EXT_FILES; i++) {

        /* Open file */
        HDsprintf(filename, "extern_%lur.raw", (unsigned long)i + 1);
        if((fd = HDopen(filename, O_RDWR|O_CREAT|O_TRUNC, H5_POSIX_CREATE_MODE_RW)) < 0)
            goto error;
        
        /* Write garbage data to the file. This allows us to test the
         * the ability to set an offset in the raw data file.
         */
        garbage_bytes = i * 10;
        n = HDwrite(fd, garbage, garbage_bytes);
        if(n < 0 || (size_t)n != garbage_bytes)
            goto error;

        /* Fill array with data */
        for(j = 0; j < PART_SIZE; j++) {
            data[j] = (int)(i * 25 + j);
        } /* end for */

        /* Write raw data to the file. */
        n = HDwrite(fd, data, sizeof(data));
        if(n != sizeof(data))
            goto error;

        /* Close this file */
        HDclose(fd);

    } /* end for */

    /* The *w files are only pre-filled with the garbage data and are
     * used to verify that write operations work correctly. The individual
     * tests fill in the actual data.
     */
    for(i = 0; i < N_EXT_FILES; i++) {

        /* Open file */
        HDsprintf(filename, "extern_%luw.raw", (unsigned long)i + 1);
        if((fd = HDopen(filename, O_RDWR|O_CREAT|O_TRUNC, H5_POSIX_CREATE_MODE_RW)) < 0)
            goto error;
        
        /* Write garbage data to the file. This allows us to test the
         * the ability to set an offset in the raw data file.
         */
        garbage_bytes = i * 10;
        n = HDwrite(fd, garbage, garbage_bytes);
        if(n < 0 || (size_t)n != garbage_bytes)
            goto error;

        /* Close this file */
        HDclose(fd);

    } /* end for */
    HDfree(garbage);
    return SUCCEED;

error:
    if(fd)
        HDclose(fd);
    if(garbage)
        HDfree(garbage);
    return FAIL;
} /* end reset_raw_data_files() */
#endif /* _EXTERNAL_H */

