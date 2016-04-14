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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, March  3, 1998
 *
 * Purpose:	Tests datasets stored in external raw files.
 */
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
 * Function:    files_have_same_contents
 *
 * Purpose:     Determines whether two files contain the same data.
 *
 * Return:      Success:    nonzero if same, zero if different.
 *		        Failure:    zero
 *
 * Programmer:  Robb Matzke
 *              Wednesday, March  4, 1998
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
files_have_same_contents(const char *name1, const char *name2)
{
    int		fd1 = 0, fd2 = 0;
    ssize_t	n1, n2;
    char	buf1[1024], buf2[1024];
    hbool_t ret = FALSE;            /* not equal until proven otherwise */

    if((fd1 = HDopen(name1, O_RDONLY, 0666)) < 0)
        goto out;
    if((fd2 = HDopen(name2, O_RDONLY, 0666)) < 0)
        goto out;

    /* Loop until files are empty or we encounter a problem */
    while(1) {
        HDmemset(buf1, 0, sizeof(buf1));
        HDmemset(buf2, 0, sizeof(buf2));

        n1 = HDread(fd1, buf1, sizeof(buf1));
        if(n1 < 0 || (size_t)n1 > sizeof(buf1))
            break;
        n2 = HDread(fd2, buf2, sizeof(buf2));
        if(n2 < 0 || (size_t)n2 > sizeof(buf2))
            break;

        if(n1 != n2)
            break;

        if(n1 == 0 && n2 == 0) {
            ret = TRUE;
            break;
        }

        if(HDmemcmp(buf1, buf2, (size_t)n1))
            break;

    } /* end while */

out:
    if(fd1)
        HDclose(fd1);
    if(fd2)
        HDclose(fd2);
    return ret;
} /* end files_have_same_contents() */


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
        if((fd = HDopen(filename, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0)
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
        if((fd = HDopen(filename, O_RDWR|O_CREAT|O_TRUNC, 0666)) < 0)
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


/*-------------------------------------------------------------------------
 * Function:    test_non_extendible
 *
 * Purpose:     Tests a non-extendible dataset with a single external file.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Robb Matzke
 *              Monday, November 23, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_non_extendible(hid_t file)
{
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dset = -1;          /* dataset                              */
    hsize_t	cur_size[1];        /* data space current size              */
    hsize_t	max_size[1];        /* data space maximum size              */
    int		n;                  /* number of external files             */
    char	name[256];          /* external file name                   */
    off_t	file_offset;        /* external file offset                 */
    hsize_t	file_size;          /* sizeof external file segment         */
    haddr_t dset_addr;          /* address of dataset                   */

    TESTING("fixed-size data space, exact storage");

    /* Create the dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    cur_size[0] = max_size[0] = 100;
    if(H5Pset_external(dcpl, "ext1.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int))) < 0)
        FAIL_STACK_ERROR
    if((space = H5Screate_simple(1, cur_size, max_size)) < 0)
        FAIL_STACK_ERROR
    if((dset = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(dset) < 0)
        FAIL_STACK_ERROR
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR

    /* Read dataset creation information */
    if((dset = H5Dopen2(file, "dset1", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Test dataset address.  Should be undefined. */
    H5E_BEGIN_TRY {
        dset_addr = H5Dget_offset(dset);
    } H5E_END_TRY;
    if(dset_addr != HADDR_UNDEF)
        FAIL_STACK_ERROR

    /* Check external count */
    if((dcpl = H5Dget_create_plist(dset)) < 0)
        FAIL_STACK_ERROR
    if((n = H5Pget_external_count(dcpl)) < 0)
        FAIL_STACK_ERROR
    if(1 != n) {
        H5_FAILED();
        HDputs("    Returned external count is wrong.");
        printf("   got: %d\n    ans: 1\n", n);
        goto error;
    } /* end if */

    HDstrcpy(name + sizeof(name) - 4, "...");
    if(H5Pget_external(dcpl, 0, sizeof(name) - 4, name, &file_offset, &file_size) < 0)
        FAIL_STACK_ERROR

    /* Check file offset */
    if(file_offset != 0) {
        H5_FAILED();
        HDputs("    Wrong file offset.");
        printf("    got: %lu\n    ans: 0\n", (unsigned long)file_offset);
        goto error;
    } /* end if */

    /* Check file size */
    if(file_size != (max_size[0] * sizeof(int))) {
        H5_FAILED();
        HDputs("    Wrong file size.");
        printf("    got: %lu\n    ans: %lu\n", (unsigned long)file_size, (unsigned long)max_size[0]*sizeof(int));
        goto error;
    } /* end if */

    /* Done (dataspace was previously closed) */
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(space);
        H5Dclose(dset);
    } H5E_END_TRY;
    return 1;
} /* end test_non_extendible() */


/*-------------------------------------------------------------------------
 * Function:    test_too_small
 *
 * Purpose:     Test a single external file which is too small to represent
 *              all the data.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:	Robb Matzke
 *              Monday, November 23, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_too_small(hid_t file)
{
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dset = -1;          /* dataset                              */
    hsize_t	cur_size[1];        /* current data space size              */
    hsize_t	max_size[1];        /* maximum data space size              */

    TESTING("external storage is too small");

    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    cur_size[0] = max_size[0] = 100;
    if(H5Pset_external(dcpl, "ext1.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) - 1)) < 0)
        FAIL_STACK_ERROR
    if((space = H5Screate_simple(1, cur_size, max_size)) < 0)
        FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        dset = H5Dcreate2(file, "dset2", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dset >= 0)
        FAIL_PUTS_ERROR("    Small external file succeeded instead of failing.");
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Dclose(dset);
    } H5E_END_TRY;
    return 1;
} /* end test_too_small() */


/*-------------------------------------------------------------------------
 * Function:    test_large_enough_current_eventual
 *
 * Purpose:     Test a single external file which is large enough to
 *              represent the current data and large enough to represent the
 *              eventual size of the data.
 *
 * Return:  Success:    0
 *          Failure:    1
 *
 * Programmer:	Robb Matzke
 *              Monday, November 23, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_large_enough_current_eventual(hid_t file)
{
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dset = -1;          /* dataset                              */
    hsize_t	cur_size[1];        /* current data space size              */
    hsize_t	max_size[1];        /* maximum data space size              */

    TESTING("extendible dataspace, exact external size");

    if((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    cur_size[0] = 100;
    max_size[0] = 200;
    if(H5Pset_external(dcpl, "ext1.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int))) < 0)
        FAIL_STACK_ERROR
    if((space = H5Screate_simple(1, cur_size, max_size)) < 0)
        FAIL_STACK_ERROR
    if((dset = H5Dcreate2(file, "dset3", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
    } H5E_END_TRY;
    return 1;
} /* end test_large_enough_current_eventual() */


/*-------------------------------------------------------------------------
 * Function:    test_large_enough_current_not_eventual
 *
 * Purpose:     Test a single external file which is large enough for the
 *              current data size but not large enough for the eventual size.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Robb Matzke
 *              Monday, November 23, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_large_enough_current_not_eventual(hid_t file)
{
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dset = -1;          /* dataset                              */
    hsize_t	cur_size[1];        /* current data space size              */
    hsize_t	max_size[1];        /* maximum data space size              */

    TESTING("extendible dataspace, external storage is too small");

    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    cur_size[0] = 100;
    max_size[0] = 200;
    if(H5Pset_external(dcpl, "ext1.data", (off_t)0, (hsize_t)(max_size[0] * sizeof(int) - 1)) < 0)
        FAIL_STACK_ERROR
    if((space = H5Screate_simple(1, cur_size, max_size)) < 0)
        FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        dset = H5Dcreate2(file, "dset4", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dset >= 0)
        FAIL_PUTS_ERROR("    Small external file succeeded instead of failing.");

    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
    } H5E_END_TRY;
    return 1;
} /* end test_large_enough_current_not_eventual() */


/*-------------------------------------------------------------------------
 * Function:    test_unlimited
 *
 * Purpose:     Test a single external file of unlimited size and an
 *              unlimited data space.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Robb Matzke
 *              Monday, November 23, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlimited(hid_t file)
{
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dset = -1;          /* dataset                              */
    hsize_t	cur_size[1];        /* data space current size              */
    hsize_t	max_size[1];        /* data space maximum size              */
    int		n;                  /* number of external files             */
    char	name[256];          /* external file name                   */
    off_t	file_offset;        /* external file offset                 */
    hsize_t	file_size;          /* sizeof external file segment         */

    TESTING("unlimited dataspace, unlimited external storage");

    /* Create dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_external(dcpl, "ext1.data", (off_t)0, H5F_UNLIMITED) < 0)
        FAIL_STACK_ERROR
    cur_size[0] = 100;
    max_size[0] = H5S_UNLIMITED;
    if((space = H5Screate_simple(1, cur_size, max_size)) < 0)
        FAIL_STACK_ERROR
    if((dset = H5Dcreate2(file, "dset5", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR

    /* Read dataset creation information */
    if((dset = H5Dopen2(file, "dset5", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((dcpl = H5Dget_create_plist(dset)) < 0)
        FAIL_STACK_ERROR
    if((n = H5Pget_external_count(dcpl)) < 0)
        FAIL_STACK_ERROR
    if(1 != n) {
        H5_FAILED();
        HDputs("    Returned external count is wrong.");
        printf("    got: %d\n    ans: 1\n", n);
        goto error;
    } /* end if */

    HDstrcpy(name + sizeof(name) - 4, "...");
    if(H5Pget_external(dcpl, 0, sizeof(name) - 4, name, &file_offset, &file_size) < 0)
        FAIL_STACK_ERROR
    if(file_offset != 0) {
        H5_FAILED();
        HDputs("    Wrong file offset.");
        printf("    got: %lu\n    ans: 0\n", (unsigned long)file_offset);
        goto error;
    } /* end if */

    if(H5F_UNLIMITED != file_size) {
        H5_FAILED();
        HDputs("    Wrong file size.");
        printf("    got: %lu\n    ans: INF\n", (unsigned long)file_size);
        goto error;
    } /* end if */

    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
    } H5E_END_TRY;
    return 1;
} /* end test_unlimited() */


/*-------------------------------------------------------------------------
 * Function:    test_multiple_files
 *
 * Purpose:     Test multiple external files for a dataset.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Robb Matzke
 *              Monday, November 23, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_multiple_files(hid_t file)
{
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* dataspace                            */
    hid_t	dset = -1;          /* dataset                              */
    hsize_t	cur_size[1];        /* data space current size              */
    hsize_t	max_size[1];        /* data space maximum size              */

    TESTING("multiple external files");

    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR

    cur_size[0] = max_size[0] = 100;

    if(H5Pset_external(dcpl, "ext1.data", (off_t)0, (hsize_t)(max_size[0]*sizeof(int)/4)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_external(dcpl, "ext2.data", (off_t)0, (hsize_t)(max_size[0]*sizeof(int)/4)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_external(dcpl, "ext3.data", (off_t)0, (hsize_t)(max_size[0]*sizeof(int)/4)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_external(dcpl, "ext4.data", (off_t)0, (hsize_t)(max_size[0]*sizeof(int)/4)) < 0)
        FAIL_STACK_ERROR
    if((space = H5Screate_simple(1, cur_size, max_size)) < 0)
        FAIL_STACK_ERROR
    if((dset = H5Dcreate2(file, "dset6", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
    } H5E_END_TRY;
    return 1;
} /* end test_multiple_files() */


/*-------------------------------------------------------------------------
 * Function:    test_add_to_unlimited
 *
 * Purpose:     It should be impossible to define an unlimited external file
 *              and then follow it with another external file.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Robb Matzke
 *              Monday, November 23, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_add_to_unlimited(void)
{
    hid_t	dcpl = -1;          /* dataset creation properties          */
    herr_t	status;             /* function return status               */
    int		n;                  /* number of external files             */

    TESTING("external file following unlimited file");

    if((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_external(dcpl, "ext1.data", (off_t)0, H5F_UNLIMITED) < 0)
        FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        status = H5Pset_external(dcpl, "ext2.data", (off_t)0, (hsize_t)100);
    } H5E_END_TRY;
    if(status >= 0)
        FAIL_PUTS_ERROR("    H5Pset_external() succeeded when it should have failed.");

    if((n = H5Pget_external_count(dcpl)) < 0)
        FAIL_STACK_ERROR
    if(1 != n)
        FAIL_PUTS_ERROR("    Wrong external file count returned.");

    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
    } H5E_END_TRY;
    return 1;
} /* end test_add_to_unlimited() */


/*-------------------------------------------------------------------------
 * Function:    test_overflow
 *
 * Purpose:     It should be impossible to create a set of external files
 *              whose total size overflows a size_t integer.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Robb Matzke
 *              Monday, November 23, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_overflow(void)
{
    hid_t	dcpl = -1;          /* dataset creation properties          */
    herr_t	status;             /* return status                        */

    TESTING("address overflow in external files");

    if((dcpl=H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_external(dcpl, "ext1.data", (off_t)0, H5F_UNLIMITED-1) < 0)
        FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        status = H5Pset_external(dcpl, "ext2.data", (off_t)0, (hsize_t)100);
    } H5E_END_TRY;
    if(status >= 0)
        FAIL_PUTS_ERROR("    H5Pset_external() succeeded when it should have failed.");

    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
    } H5E_END_TRY;
    return 1;
} /* end test_overflow() */


/*-------------------------------------------------------------------------
 * Function:    test_read_file_set
 *
 * Purpose:     Tests reading from an external file set.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Robb Matzke
 *              Wednesday, March  4, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_read_file_set(hid_t fapl)
{
    hid_t	file = -1;          /* file to write to                     */
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dset = -1;          /* dataset                              */
    hid_t	grp = -1;           /* group to emit diagnostics            */
    size_t	i;                  /* miscellaneous counter                */
    char	filename[1024];     /* file names                           */
    int	    part[PART_SIZE];    /* raw data buffer (partial)            */
    int     whole[TOTAL_SIZE];  /* raw data buffer (total)              */
    hsize_t	cur_size;           /* current data space size              */
    hid_t	hs_space = -1;      /* hyperslab data space                 */
    hsize_t	hs_start = 30;      /* hyperslab starting offset            */
    hsize_t	hs_count = 25;      /* hyperslab size                       */

    TESTING("read external dataset");

    if(HDsetenv("HDF5_EXTFILE_PREFIX", "", 1) < 0)
        TEST_ERROR

    /* Reset the raw data files */
    if(reset_raw_data_files() < 0)
        TEST_ERROR

    /* Create the file and an initial group.  This causes messages about
     * debugging to be emitted before we start playing games with what the
     * output looks like.
     */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR
    if((grp = H5Gcreate2(file, "emit-diagnostics", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if(H5Gclose(grp) < 0) FAIL_STACK_ERROR

    /* Create the dcpl */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < N_EXT_FILES; i++) {
        HDsnprintf(filename, sizeof(filename), "extern_%dr.raw", (int) i + 1);
        if(H5Pset_external(dcpl, filename, (off_t)(i * GARBAGE_PER_FILE), (hsize_t)sizeof(part)) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Create the dataspace */
    cur_size = TOTAL_SIZE;
    if((space = H5Screate_simple(1, &cur_size, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Create the dataset */
    if((dset = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Read the entire dataset */
    HDmemset(whole, 0, sizeof(whole));
    if(H5Dread(dset, H5T_NATIVE_INT, space, space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR

    /* Compare data */
    for(i = 0; i < TOTAL_SIZE; i++)
        if(whole[i] != (signed)i)
            FAIL_PUTS_ERROR("Incorrect value(s) read.");

    /* Read via a hypserslab in the middle of the dataset */

    /* Set up dataspace */
    if((hs_space = H5Scopy(space)) < 0)
        FAIL_STACK_ERROR
    if(H5Sselect_hyperslab(hs_space, H5S_SELECT_SET, &hs_start, NULL, &hs_count, NULL) < 0)
        FAIL_STACK_ERROR

    /* Read */
    HDmemset(whole, 0, sizeof(whole));
    if(H5Dread(dset, H5T_NATIVE_INT, hs_space, hs_space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR

    /* Verify data */
    for(i = (size_t)hs_start; i < (size_t)(hs_start + hs_count); i++) {
        if(whole[i] != (signed)i)
            FAIL_PUTS_ERROR("Incorrect value(s) read (hyperslab).");
    } /* end for */

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Sclose(hs_space) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Gclose(grp);
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
        H5Sclose(hs_space);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_read_file_set() */


/*-------------------------------------------------------------------------
 * Function:    test_write_file_set
 *
 * Purpose:     Tests writing to an external file set.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Robb Matzke
 *              Wednesday, March  4, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_write_file_set(hid_t fapl)
{
    hid_t	file = -1;          /* file to which to write               */
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	mem_space = -1;     /* memory data space                    */
    hid_t	file_space = -1;    /* file data space                      */
    hid_t	dset = -1;          /* dataset                              */
    unsigned i;                 /* miscellaneous counter                */
    int	    part[PART_SIZE];    /* raw data buffer (partial)            */
    int     whole[TOTAL_SIZE];  /* raw data buffer (total)              */
    hsize_t	cur_size = 100;     /* current data space size              */
    hsize_t	max_size = 200;     /* maximum data space size              */
    hsize_t	hs_start = 100;     /* hyperslab starting offset            */
    hsize_t	hs_count = 100;     /* hyperslab size                       */
    char	filename[1024];     /* file name                            */

    TESTING("write external dataset");

    if(HDsetenv("HDF5_EXTFILE_PREFIX", "", 1) < 0)
        TEST_ERROR

    /* Create another file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof(filename));
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Create the dcpl and external file list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < N_EXT_FILES; i++) {
        hsize_t size;

        HDsnprintf(filename, sizeof(filename), "extern_%dw.raw", (int) i + 1);

        if(i != N_EXT_FILES -1)
            size = (hsize_t)sizeof(part);
        else
            size = H5F_UNLIMITED;

        if(H5Pset_external(dcpl, filename, (off_t)(i * GARBAGE_PER_FILE), size) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* Reset the raw data files */
    if(reset_raw_data_files() < 0)
        TEST_ERROR

    /* Create the dataset */
    if((mem_space = H5Screate_simple(1, &cur_size, &max_size)) < 0)
        FAIL_STACK_ERROR
    if((file_space = H5Scopy(mem_space)) < 0)
        FAIL_STACK_ERROR
    if((dset = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, file_space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write the entire dataset and compare with the original */
    for(i = 0; i < cur_size; i++)
        whole[i] = (int)i;
    if(H5Dwrite(dset, H5T_NATIVE_INT, mem_space, file_space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < N_EXT_FILES; i++) {
        char name1[64], name2[64];

        HDsprintf(name1, "extern_%dr.raw", i + 1);
        HDsprintf(name2, "extern_%dw.raw", i + 1);
        if(!files_have_same_contents(name1, name2))
            FAIL_PUTS_ERROR("   Output differs from expected value.")
    } /* end for */

    /* Extend the dataset by another 100 elements */
    if(H5Dset_extent(dset, &max_size) < 0)
        FAIL_STACK_ERROR
    if(H5Sclose(file_space) < 0)
        FAIL_STACK_ERROR
    if((file_space = H5Dget_space(dset)) < 0)
        FAIL_STACK_ERROR

    /* Write second half of dataset */
    for(i = 0; i < hs_count; i++)
        whole[i] = 100 + (int)i;
    if(H5Sselect_hyperslab(file_space, H5S_SELECT_SET, &hs_start, NULL, &hs_count, NULL) < 0)
        FAIL_STACK_ERROR
    if(H5Dwrite(dset, H5T_NATIVE_INT, mem_space, file_space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR


    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Sclose(mem_space) < 0) FAIL_STACK_ERROR
    if(H5Sclose(file_space) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(mem_space);
        H5Sclose(file_space);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_write_file_set() */

 
 /*-------------------------------------------------------------------------
 * Function:    test_path_absolute
 *
 * Purpose:     Test absolute filenames for external files.
 *              This will create an HDF5 file in a subdirectory which will
 *              refer to /full/path/extern_*a.raw on unix and to
 *              c:\full\path\extern_*a.raw and \full\path\extern_*a.raw on
 *              windows.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Steffen Kiess
 *              March 10, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
test_path_absolute(hid_t fapl)
{
    hid_t	file = -1;          /* file to write to                     */
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dset = -1;          /* dataset                              */
    size_t	i;                  /* miscellaneous counter                */
    char	cwdpath[1024];      /* working directory                    */
    char	filename[1024];     /* file name                            */
    int	    part[PART_SIZE];    /* raw data buffer (partial)            */
    int     whole[TOTAL_SIZE];  /* raw data buffer (total)              */
    hsize_t	cur_size;           /* current data space size              */

    TESTING("absolute filenames for external file");

    h5_fixname(FILENAME[3], fapl, filename, sizeof(filename));
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Reset the raw data files */
    if(reset_raw_data_files() < 0)
        TEST_ERROR

    /* Create the dcpl */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(NULL == HDgetcwd(cwdpath, sizeof(cwdpath)))
        TEST_ERROR
    for(i = 0; i < N_EXT_FILES; i++) {
        HDsnprintf(filename, sizeof(filename), "%s%sextern_%dr.raw", cwdpath, H5_DIR_SEPS, (int) i + 1);
#if defined(H5_HAVE_WINDOW_PATH)
        /* For windows, test path-absolute case (\dir\file.raw) for the second file */
        if(i == 1)
            HDsnprintf(filename, sizeof(filename), "%s%sextern_%dr.raw", cwdpath + 2, H5_DIR_SEPS, i + 1);
#endif
        if(H5Pset_external(dcpl, filename, (off_t)(i * GARBAGE_PER_FILE), (hsize_t)sizeof(part)) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    /* create the dataspace */
    cur_size = TOTAL_SIZE;
    if((space = H5Screate_simple(1, &cur_size, NULL)) < 0)
        FAIL_STACK_ERROR

    /* create the dataset */
    if((dset = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Read the entire dataset and compare with the original */
    HDmemset(whole, 0, sizeof(whole));
    if(H5Dread(dset, H5T_NATIVE_INT, space, space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < TOTAL_SIZE; i++)
        if(whole[i] != (signed)i)
            FAIL_PUTS_ERROR("Incorrect value(s) read.");

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_path_absolute() */


/*-------------------------------------------------------------------------
 * Function:    test_path_relative
 *
 * Purpose:     Test external files with filename relative to current directory.
 *              This will create an HDF5 file in a subdirectory which will
 *              refer to extern_*a.raw
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Steffen Kiess
 *              March 10, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
test_path_relative(hid_t fapl)
{
    hid_t	file = -1;          /* file to write to                     */
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dset = -1;          /* dataset                              */
    size_t	i;                  /* miscellaneous counters               */
    char	cwdpath[1024];      /* working directory                    */
    char	filename[1024];     /* file name                            */
    int	    part[PART_SIZE];    /* raw data buffer (partial)            */
    int     whole[TOTAL_SIZE];  /* raw data buffer (total)              */
    hsize_t	cur_size;           /* current data space size              */

    TESTING("filenames relative to current directory for external file");

    if(HDsetenv("HDF5_EXTFILE_PREFIX", "", 1) < 0)
        TEST_ERROR

    if (HDmkdir("extern_dir", (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR;

    h5_fixname(FILENAME[4], fapl, filename, sizeof(filename));
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Reset the raw data files */
    if(reset_raw_data_files() < 0)
        TEST_ERROR

    /* Create the dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(NULL == HDgetcwd(cwdpath, sizeof(cwdpath)))
        TEST_ERROR
    for (i = 0; i < N_EXT_FILES; i++) {
        HDsnprintf(filename, sizeof(filename), "extern_%dr.raw", (int)i + 1);
        if(H5Pset_external(dcpl, filename, (off_t)(i * GARBAGE_PER_FILE), (hsize_t)sizeof(part)) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    cur_size = TOTAL_SIZE;
    if((space = H5Screate_simple(1, &cur_size, NULL)) < 0)
        FAIL_STACK_ERROR
    if((dset = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Read the entire dataset and compare with the original */
    HDmemset(whole, 0, sizeof(whole));
    if(H5Dread(dset, H5T_NATIVE_INT, space, space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < TOTAL_SIZE; i++)
        if(whole[i] != (signed)i)
            FAIL_PUTS_ERROR("Incorrect value(s) read.");

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_path_relative() */


/*-------------------------------------------------------------------------
 * Function:    test_path_relative_cwd
 *
 * Purpose:     Test external files with filename relative to current directory.
 *              This will create an HDF5 file in a subdirectory which will
 *              refer to ../extern_*a.raw
 *              The files are then accessed by setting the efile_prefix dataset
 *              access property to "${ORIGIN}".
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Steffen Kiess
 *              March 10, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
test_path_relative_cwd(hid_t fapl)
{
    hid_t	file = -1;          /* file to write to                     */
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dapl = -1;          /* dataset access property list         */
    hid_t	dapl2 = -1;         /* copy of dapl                         */
    hid_t	dset = -1;          /* dataset                              */
    hid_t	dset2 = -1;         /* dataset, opened a second time        */
    hid_t	dset3 = -1;         /* dataset, opened with different prefix    */
    size_t	i;                  /* miscellaneous counters               */
    char	cwdpath[1024];      /* working directory                    */
    char	filename[1024];     /* file name                            */
    int	    part[PART_SIZE];    /* raw data buffer (partial)            */
    int     whole[TOTAL_SIZE];  /* raw data buffer (total)              */
    hsize_t	cur_size;           /* current data space size              */
    char	buffer[1024];       /* buffer to read efile_prefix          */

    TESTING("filenames relative to HDF5 file for external file");

    if(HDsetenv("HDF5_EXTFILE_PREFIX", "", 1) < 0)
        TEST_ERROR

    if(HDmkdir("extern_dir", (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR;

    h5_fixname(FILENAME[4], fapl, filename, sizeof(filename));
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Reset the raw data files */
    if(reset_raw_data_files() < 0)
        TEST_ERROR

    /* Create the dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(NULL == HDgetcwd(cwdpath, sizeof(cwdpath)))
        TEST_ERROR
    for(i = 0; i < N_EXT_FILES; i++) {
        HDsnprintf(filename, sizeof(filename), "..%sextern_%dr.raw", H5_DIR_SEPS, (int)i + 1);
        if(H5Pset_external(dcpl, filename, (off_t)(i * GARBAGE_PER_FILE), (hsize_t)sizeof(part)) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    cur_size = TOTAL_SIZE;
    if((space = H5Screate_simple(1, &cur_size, NULL)) < 0)
        FAIL_STACK_ERROR
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_efile_prefix(dapl, "${ORIGIN}") < 0)
        FAIL_STACK_ERROR
    if(H5Pget_efile_prefix(dapl, buffer, sizeof(buffer)) < 0)
        FAIL_STACK_ERROR
    if(HDstrcmp(buffer, "${ORIGIN}") != 0)
        FAIL_PUTS_ERROR("efile prefix not set correctly");
    if((dapl2 = H5Pcopy(dapl)) < 0)
        FAIL_STACK_ERROR

    /* Create dataset */
    if((dset = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, dapl2)) < 0)
        FAIL_STACK_ERROR

    /* Reopen dataset with same efile_prefix property */
    if((dset2 = H5Dopen2(file, "dset1", dapl2)) < 0)
        FAIL_STACK_ERROR

    /* Reopen dataset with different efile_prefix property */
    if(H5Pset_efile_prefix(dapl, "//") < 0)
        FAIL_STACK_ERROR
    H5E_BEGIN_TRY {
        dset3 = H5Dopen2(file, "dset1", dapl);
    } H5E_END_TRY;
    if(dset3 >= 0)
        FAIL_PUTS_ERROR("reopening the dataset with a different efile_prefix succeded");

    /* Read the entire dataset and compare with the original */
    HDmemset(whole, 0, sizeof(whole));
    if(H5Dread(dset, H5T_NATIVE_INT, space, space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < TOTAL_SIZE; i++)
        if(whole[i] != (signed)i)
            FAIL_PUTS_ERROR("Incorrect value(s) read.");

    /* Close dataset */
    if(H5Dclose(dset2) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR

    /* Open dataset (use a differend prefix than for create.
     * This works because the dataset was closed.
     */
    if(H5Pset_efile_prefix(dapl2, "${ORIGIN}/.") < 0)
        FAIL_STACK_ERROR
    if((dset = H5Dopen2(file, "dset1", dapl2)) < 0)
        FAIL_STACK_ERROR

    /* Reopen dataset with same efile_prefix property */
    if((dset2 = H5Dopen2(file, "dset1", dapl2)) < 0)
        FAIL_STACK_ERROR

    /* Reopen dataset with different efile_prefix property */
    if(H5Pset_efile_prefix(dapl, NULL) < 0)
        FAIL_STACK_ERROR
    H5E_BEGIN_TRY {
        dset3 = H5Dopen2(file, "dset1", dapl);
    } H5E_END_TRY;
    if(dset3 >= 0)
        FAIL_PUTS_ERROR("reopening the dataset with a different efile_prefix succeded");

    /* Read the entire dataset and compare with the original */
    HDmemset(whole, 0, sizeof(whole));
    if(H5Dread(dset, H5T_NATIVE_INT, space, space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < TOTAL_SIZE; i++)
        if(whole[i] != (signed)i)
            FAIL_PUTS_ERROR("Incorrect value(s) read.");

    if(H5Dclose(dset2) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dapl2) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dapl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dapl2);
        H5Pclose(dapl);
        H5Dclose(dset3);
        H5Dclose(dset2);
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_path_relative_cwd() */


/*-------------------------------------------------------------------------
 * Function:    test_path_env
 *
 * Purpose:     Test whether the value of HDF5_EXTFILE_PREFIX will overwrite
 *              the efile_prefix dataset access property.
 *              This will create an HDF5 file in a subdirectory which will
 *              refer to ../extern_*a.raw
 *              The files are then accessed by setting the HDF5_EXTFILE_PREFIX
 *              environment variable to "${ORIGIN}".
 *              The efile_prefix dataset access property is set to "someprefix",
 *              which will cause an error if the value is not overwritten by
 *              the environment variable.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Steffen Kiess
 *              March 10, 2015
 *
 *-------------------------------------------------------------------------
 */
static int
test_path_env(hid_t fapl)
{
    hid_t	file = -1;          /* file to write to                     */
    hid_t	dcpl = -1;          /* dataset creation properties          */
    hid_t	space = -1;         /* data space                           */
    hid_t	dapl = -1;          /* dataset access property list         */
    hid_t	dset = -1;          /* dataset                              */
    size_t	i;                  /* miscellaneous counters               */
    char	cwdpath[1024];		/* working directory                    */
    char	filename[1024];		/* file name                            */
    int	    part[PART_SIZE];    /* raw data buffer (partial)            */
    int     whole[TOTAL_SIZE];  /* raw data buffer (total)              */
    hsize_t	cur_size;           /* current data space size              */
    char	buffer[1024];       /* buffer to read efile_prefix          */

    TESTING("prefix in HDF5_EXTFILE_PREFIX");

    if(HDsetenv("HDF5_EXTFILE_PREFIX", "${ORIGIN}", 1))
        TEST_ERROR

    if(HDmkdir("extern_dir", (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR;

    h5_fixname(FILENAME[4], fapl, filename, sizeof(filename));
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Reset the raw data files */
    if(reset_raw_data_files() < 0)
        TEST_ERROR

    /* Create the dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(NULL == HDgetcwd(cwdpath, sizeof(cwdpath)))
        TEST_ERROR
    for(i = 0; i < N_EXT_FILES; i++) {
        HDsnprintf(filename, sizeof(filename), "..%sextern_%dr.raw", H5_DIR_SEPS, (int) i + 1);
        if(H5Pset_external(dcpl, filename, (off_t)(i * GARBAGE_PER_FILE), (hsize_t)sizeof(part)) < 0)
            FAIL_STACK_ERROR
    } /* end for */

    cur_size = TOTAL_SIZE;
    if((space = H5Screate_simple(1, &cur_size, NULL)) < 0)
        FAIL_STACK_ERROR
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR

    /* Set prefix to a nonexistent directory, will be overwritten by environment variable */
    if(H5Pset_efile_prefix(dapl, "someprefix") < 0)
        FAIL_STACK_ERROR
    if(H5Pget_efile_prefix(dapl, buffer, sizeof(buffer)) < 0)
        FAIL_STACK_ERROR
    if(HDstrcmp(buffer, "someprefix") != 0)
        FAIL_PUTS_ERROR("efile prefix not set correctly");

    /* Create dataset */
    if((dset = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, dapl)) < 0)
        FAIL_STACK_ERROR

    /* Read the entire dataset and compare with the original */
    HDmemset(whole, 0, sizeof(whole));
    if(H5Dread(dset, H5T_NATIVE_INT, space, space, H5P_DEFAULT, whole) < 0)
        FAIL_STACK_ERROR
    for(i = 0; i < TOTAL_SIZE; i++)
        if(whole[i] != (signed)i)
            FAIL_PUTS_ERROR("Incorrect value(s) read.");

    if(H5Dclose(dset) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dapl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Sclose(space) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dapl);
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(space);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
} /* end test_path_env() */


/*-------------------------------------------------------------------------
 * Function:    test_h5d_get_access_plist
 *
 * Purpose:     Ensure that H5Dget_access_plist returns correct values.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Dana Robinson
 *              March 2016
 *
 *-------------------------------------------------------------------------
 */
static int
test_h5d_get_access_plist(hid_t fapl_id)
{
    hid_t	fid = -1;           /* file to write to                     */
    hid_t	dcpl_id = -1;       /* dataset creation properties          */
    hid_t	dapl_id = -1;       /* dataset access properties            */
    hid_t	sid = -1;           /* data space                           */
    hid_t	did = -1;           /* dataset                              */
    hsize_t dims = 0;           /* dataset size                         */
    char    *buffer = NULL;     /* saved prefix name from dapl          */
    char	filename[1024];     /* file names                           */

    TESTING("H5Dget_access_plist() returns correct prefix");

    if(HDsetenv("HDF5_EXTFILE_PREFIX", "", 1) < 0)
        TEST_ERROR

    /* Reset the raw data files */
    if(reset_raw_data_files() < 0)
        TEST_ERROR

    /* Create the file */
    h5_fixname(FILENAME[5], fapl_id, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        FAIL_STACK_ERROR

    /* Create the dcpl and set external storage */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_external(dcpl_id, "extern_1r.raw", (off_t)0, (hsize_t)0) < 0)
        FAIL_STACK_ERROR

    /* Create the dapl and set the prefix */
    if((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_efile_prefix(dapl_id, "someprefix") < 0)
        FAIL_STACK_ERROR

    /* Create the dataset */
    if((sid = H5Screate_simple(1, &dims, NULL)) < 0)
        FAIL_STACK_ERROR
    if((did = H5Dcreate2(fid, "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, dapl_id)) < 0)
        FAIL_STACK_ERROR

    /* Close the dapl */
    if(H5Pclose(dapl_id) < 0)
        FAIL_STACK_ERROR
    dapl_id = -1;

    /* Get a data access property list from the dataset */
    if((dapl_id = H5Dget_access_plist(did)) < 0)
        FAIL_STACK_ERROR

    /* Check the value for the external prefix */
    if((buffer = (char *)HDcalloc((size_t)64, sizeof(char))) == NULL)
        TEST_ERROR
    if(H5Pget_efile_prefix(dapl_id, buffer, (size_t)64) < 0)
        FAIL_STACK_ERROR
    if(HDstrcmp(buffer, "someprefix") != 0)
        FAIL_PUTS_ERROR("external file prefix from dapl incorrect");

    /* Close everything */
    HDfree(buffer);
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(did) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl_id) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dapl_id) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

 error:
    if(buffer)
        HDfree(buffer);
    H5E_BEGIN_TRY {
        H5Dclose(did);
        H5Pclose(dcpl_id);
        H5Pclose(dapl_id);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end test_h5d_get_access_plist() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Runs external dataset tests.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl_id_old = -1;   /* file access properties (old format)  */
    hid_t	fapl_id_new = -1;   /* file access properties (new format)  */
    hid_t	fid = -1;           /* file for test_1* functions           */
    hid_t	gid = -1;           /* group to emit diagnostics            */
    char	filename[1024];     /* file name for test_1* funcs          */
    unsigned latest_format;     /* default or latest file format        */
    int		nerrors = 0;        /* number of errors                     */

    h5_reset();

    /* Get a fapl for the old (default) file format */
    fapl_id_old = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id_old, filename, sizeof(filename));

    /* Copy and set up a fapl for the latest file format */
    if((fapl_id_new = H5Pcopy(fapl_id_old)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_libver_bounds(fapl_id_new, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR

    /* The file format doesn't matter for this test */
    nerrors += test_h5d_get_access_plist(fapl_id_new);
    HDputs("");

    /* Test with old & new format groups */
    for(latest_format = FALSE; latest_format <= TRUE; latest_format++) {
        hid_t current_fapl_id = -1;

        /* Set the fapl for different file formats */
        if(latest_format) {
            HDputs("\nTesting with the latest file format:");
            current_fapl_id = fapl_id_new;
        } /* end if */
        else {
            HDputs("Testing with the default file format:");
            current_fapl_id = fapl_id_old;
        } /* end else */

        /* Create the common file used by some of the tests */
        if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, current_fapl_id)) < 0)
            FAIL_STACK_ERROR

        /* Create a group that will be used in the file set read test */
        if((gid = H5Gcreate2(fid, "emit-diagnostics", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        if(H5Gclose(gid) < 0) FAIL_STACK_ERROR

        /* These tests use a common file */
        nerrors += test_non_extendible(fid);
        nerrors += test_too_small(fid);
        nerrors += test_large_enough_current_eventual(fid);
        nerrors += test_large_enough_current_not_eventual(fid);
        nerrors += test_unlimited(fid);
        nerrors += test_multiple_files(fid);

        /* These tests use no file */
        nerrors += test_add_to_unlimited();
        nerrors += test_overflow();

        /* These file set tests use the VFD-aware fapl */
        nerrors += test_read_file_set(current_fapl_id);
        nerrors += test_write_file_set(current_fapl_id);
        nerrors += test_path_absolute(current_fapl_id);
        nerrors += test_path_relative(current_fapl_id);
        nerrors += test_path_relative_cwd(current_fapl_id);
        nerrors += test_path_env(current_fapl_id);
 
        /* Verify symbol table messages are cached */
        nerrors += (h5_verify_cached_stabs(FILENAME, current_fapl_id) < 0 ? 1 : 0);

        /* Close the common file */
        if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    } /* end for */

    if(nerrors > 0) goto error;

    /* Close the new ff fapl. h5_cleanup will take care of the old ff fapl */
    if(H5Pclose(fapl_id_new) < 0) FAIL_STACK_ERROR

    HDputs("All external storage tests passed.");

    /* Clean up files used by file set tests */
    if(h5_cleanup(FILENAME, fapl_id_old)) {
        HDremove("extern_1r.raw");
        HDremove("extern_2r.raw");
        HDremove("extern_3r.raw");
        HDremove("extern_4r.raw");

        HDremove("extern_1w.raw");
        HDremove("extern_2w.raw");
        HDremove("extern_3w.raw");
        HDremove("extern_4w.raw");

        HDrmdir("extern_dir");
    } /* end if */

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
        H5Pclose(fapl_id_old);
        H5Pclose(fapl_id_new);
        H5Gclose(gid);
    } H5E_END_TRY;
    nerrors = MAX(1, nerrors);
    printf("%d TEST%s FAILED.\n", nerrors, 1 == nerrors ? "" : "s");
    return EXIT_FAILURE;
} /* end main() */

