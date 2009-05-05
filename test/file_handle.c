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
 * Programmer:  Raymond Lu<slu@ncsa.uiuc.edu>
 *              Tuesday, Sept 24, 2002
 *
 * Purpose:     Tests the file handle interface
 */

#include "h5test.h"

#define KB              1024U
#define FAMILY_NUMBER   4
#define FAMILY_SIZE     (1*KB)
#define FAMILY_SIZE2    (5*KB)
#define MULTI_SIZE      128
#define CORE_INCREMENT  (4*KB)
#define FILE_COPY_BUF_SIZE  4096

const char *FILENAME[] = {
    "sec2_file",
    "core_file",
    "family_file",
    "new_family_v18_",
    "multi_file",
    NULL
};

#define COMPAT_BASENAME "family_v1.8_"


/*-------------------------------------------------------------------------
 * Function:    test_sec2
 *
 * Purpose:     Tests the file handle interface for SEC2 driver
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, June 23, 2004
 *              Added test for H5Fget_filesize.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sec2(void)
{
    hid_t       file=(-1), fapl, access_fapl = -1;
    char        filename[1024];
    int         *fhandle=NULL;
    hsize_t     file_size;

    TESTING("SEC2 file driver");

    /* Set property list and file name for SEC2 driver. */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_sec2(fapl)<0)
        goto error;
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        goto error;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        goto error;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        goto error;

    /* Check file handle API */
    if(H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle)<0)
        goto error;
    if(*fhandle<0)
        goto error;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        goto error;

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if(file_size<1*KB || file_size>4*KB)
        goto error;

    if(H5Fclose(file)<0)
        goto error;
    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose (fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_core
 *
 * Purpose:     Tests the file handle interface for CORE driver
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, June 23, 2004
 *              Added test for H5Fget_filesize.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_core(void)
{
    hid_t       file=(-1), fapl, access_fapl = -1;
    char        filename[1024];
    void        *fhandle=NULL;
    hsize_t     file_size;

    TESTING("CORE file driver");

    /* Set property list and file name for CORE driver */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_core(fapl, (size_t)CORE_INCREMENT, TRUE)<0)
        goto error;
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        goto error;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        goto error;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        goto error;

    if(H5Fget_vfd_handle(file, H5P_DEFAULT, &fhandle)<0)
        goto error;
    if(fhandle==NULL)
    {
        printf("fhandle==NULL\n");
               goto error;
    }

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        goto error;

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  Why is this 4KB?
     */
    if(file_size<2*KB || file_size>6*KB)
        goto error;

    if(H5Fclose(file)<0)
        goto error;
    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose (fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_family
 *
 * Purpose:     Tests the file handle interface for FAMILY driver
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, June 23, 2004
 *              Added test for H5Fget_filesize.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family(void)
{
    hid_t       file=(-1), fapl, fapl2=(-1), space=(-1), dset=(-1);
    hid_t       access_fapl = -1;
    char        filename[1024];
    char        dname[]="dataset";
    unsigned int i, j;
    int         *fhandle=NULL, *fhandle2=NULL;
    int         buf[FAMILY_NUMBER][FAMILY_SIZE];
    hsize_t     dims[2]={FAMILY_NUMBER, FAMILY_SIZE};
    hsize_t     file_size;

    TESTING("FAMILY file driver");

    /* Set property list and file name for FAMILY driver */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT)<0)
        goto error;
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        goto error;

    if(H5Fclose(file)<0)
        goto error;

    /* Tries to reopen the file with member file size smaller than
     * actual 1st member file size(976 bytes).  The library is
     * supposed to adjust the member size to 976 bytes. */
    if(H5Pset_fapl_family(fapl, (hsize_t)512, H5P_DEFAULT)<0)
        goto error;

    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl))<0)
        goto error;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        goto error;

    /* The file size is supposed to be 976 bytes right now. */
    if(file_size<KB/2 || file_size>4*KB)
        goto error;

    /* Create and write dataset */
    if((space=H5Screate_simple(2, dims, NULL))<0)
        goto error;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        goto error;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        goto error;

    if((dset=H5Dcreate(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT))<0)
        goto error;

    for(i=0; i<FAMILY_NUMBER; i++)
        for(j=0; j<FAMILY_SIZE; j++)
            buf[i][j] = i*10000+j;
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
        goto error;

    /* check file handle API */
    if((fapl2=H5Pcreate(H5P_FILE_ACCESS))<0)
        goto error;
    if(H5Pset_family_offset(fapl2, (hsize_t)0)<0)
        goto error;

    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle)<0)
        goto error;
    if(*fhandle<0)
        goto error;

    if(H5Pset_family_offset(fapl2, (hsize_t)(FAMILY_SIZE*2))<0)
        goto error;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle2)<0)
        goto error;
    if(*fhandle2<0)
        goto error;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        goto error;

    /* Some data has been written.  The file size should be bigger(18KB+976 bytes if int size is 4 bytes) now. */
    if(sizeof(int)<=4) {
        if(file_size<18*KB || file_size>20*KB)
            goto error;
    } else if(sizeof(int)>=8) {
        if(file_size<32*KB || file_size>40*KB)
            goto error;
    }

    if(H5Sclose(space)<0)
        goto error;
    if(H5Dclose(dset)<0)
        goto error;
    if(H5Pclose(fapl2)<0)
        goto error;
    if(H5Fclose(file)<0)
        goto error;
    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose (fapl2);
        H5Pclose (fapl2);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_family_compat
 *
 * Purpose:     Tests the forward compatibility for FAMILY driver.
 *              See if we can open files created with v1.8 library.
 *              The source file was created by the test/vfd.c
 *              of the v1.8 library.  Then tools/misc/h5repart.c was
 *              used to concantenated.  The command was "h5repart -m 5k
 *              family_file%05d.h5 family_v1.8_%05d.h5".
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              June 3, 2005
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static herr_t
test_family_compat(void)
{
    hid_t       file=(-1), fapl;
    hid_t       dset;
    char        dname[]="dataset";
    char        filename[1024];
    char        pathname[1024], pathname_individual[1024];
    char        newname[1024], newname_individual[1024];
    char       *srcdir = getenv("srcdir"); /*where the src code is located*/
    FILE        *tmp_fp, *old_fp;       /* Pointers to temp & old files */
    void        *copy_buf;              /* Pointer to buffer for copying data */
    size_t      written;                /* Amount of data written to new file */
    size_t      read_in;                /* Amount of data read in from old file */
    int         counter = 0;

    TESTING("FAMILY file driver forward compatibility");

    /* Set property list and file name for FAMILY driver */
    fapl = h5_fileaccess();

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE2, H5P_DEFAULT)<0)
        goto error;

    h5_fixname(COMPAT_BASENAME, fapl, filename, sizeof filename);
    h5_fixname(FILENAME[3], fapl, newname, sizeof newname);

    pathname[0] = '\0';

    /* Generate correct name for test file by prepending the source path */
    if(srcdir && ((strlen(srcdir) + strlen(filename) + 1) < sizeof(pathname))) {
        strcpy(pathname, srcdir);
        strcat(pathname, "/");
    }
    strcat(pathname, filename);

    /* The following code makes the copies of the family files in the source directory.
     * Since we're going to open the files with write mode, this protects the original
     * files.
     */
    if(NULL == (copy_buf = HDmalloc((size_t)FILE_COPY_BUF_SIZE))) TEST_ERROR

    sprintf(newname_individual, newname, counter);
    sprintf(pathname_individual, pathname, counter);

    /* Open the original files until no more left.  Copy the content into the new files. */
    while((old_fp = HDfopen(pathname_individual,"rb"))) {
        /* Open the new file */
        if(NULL == (tmp_fp = fopen(newname_individual,"wb"))) TEST_ERROR

        /* Copy data from the old file to the new file */
        while((read_in = HDfread(copy_buf, (size_t)1, (size_t)FILE_COPY_BUF_SIZE, old_fp)) > 0)
            /* Write the data to the new file */
            if(read_in != (written = HDfwrite(copy_buf, (size_t)1, read_in, tmp_fp))) TEST_ERROR

        /* Close the old file */
        if(HDfclose(old_fp)) TEST_ERROR

        /* Close the new file */
        if(HDfclose(tmp_fp)) TEST_ERROR

        counter++;
        sprintf(newname_individual, newname, counter);
        sprintf(pathname_individual, pathname, counter);
    }

    /* Free the copy buffer */
    free(copy_buf);

    /* Make sure we can open the file.  Use the read and write mode. */
    if((file=H5Fopen(newname, H5F_ACC_RDWR, fapl))<0)
        goto error;

    if((dset = H5Dopen(file, dname)) < 0)
        TEST_ERROR;

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Fclose(file)<0)
        goto error;

    /* Open the file again to make sure it isn't corrupted. */
    if((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if((dset = H5Dopen(file, dname)) < 0)
        TEST_ERROR;

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_multi
 *
 * Purpose:     Tests the file handle interface for MUTLI driver
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, June 23, 2004
 *              Added test for H5Fget_filesize.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multi(void)
{
    hid_t       file=(-1), fapl, fapl2=(-1), dset=(-1), space=(-1);
    hid_t       access_fapl = -1;
    char        filename[1024];
    int         *fhandle2=NULL, *fhandle=NULL;
    hsize_t     file_size;
    H5FD_mem_t  mt, memb_map[H5FD_MEM_NTYPES];
    hid_t       memb_fapl[H5FD_MEM_NTYPES];
    haddr_t     memb_addr[H5FD_MEM_NTYPES];
    const char  *memb_name[H5FD_MEM_NTYPES];
    char        sv[H5FD_MEM_NTYPES][32];
    hsize_t     dims[2]={MULTI_SIZE, MULTI_SIZE};
    char        dname[]="dataset";
    int         i, j;
    int         buf[MULTI_SIZE][MULTI_SIZE];

    TESTING("MULTI file driver");
    /* Set file access property list for MULTI driver */
    fapl = h5_fileaccess();

    HDmemset(memb_map, 0,  sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);
    HDmemset(sv, 0, sizeof sv);

    for(mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt))
        memb_map[mt] = H5FD_MEM_SUPER;
    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;

    memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
    sprintf(sv[H5FD_MEM_SUPER], "%%s-%c.h5", 's');
    memb_name[H5FD_MEM_SUPER] = sv[H5FD_MEM_SUPER];
    memb_addr[H5FD_MEM_SUPER] = 0;

    memb_fapl[H5FD_MEM_DRAW] = H5P_DEFAULT;
    sprintf(sv[H5FD_MEM_DRAW], "%%s-%c.h5", 'r');
    memb_name[H5FD_MEM_DRAW] = sv[H5FD_MEM_DRAW];
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

    if(H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE)<0)
        goto error;
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0)
        goto error;

    /* Create and write data set */
    if((space=H5Screate_simple(2, dims, NULL))<0)
        goto error;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        goto error;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        goto error;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        goto error;

    /* Before any data is written, the raw data file is empty.  So
     * the file size is only the size of metadata file.  It's supposed
     * to be 2KB.
     */
    if(file_size<1*KB || file_size>4*KB)
        goto error;

    if((dset=H5Dcreate(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT))<0)
        goto error;

    for(i=0; i<MULTI_SIZE; i++)
        for(j=0; j<MULTI_SIZE; j++)
            buf[i][j] = i*10000+j;
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
        goto error;

    if((fapl2=H5Pcreate(H5P_FILE_ACCESS))<0)
        goto error;
    if(H5Pset_multi_type(fapl2, H5FD_MEM_SUPER)<0)
        goto error;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle)<0)
        goto error;
    if(*fhandle<0)
        goto error;

    if(H5Pset_multi_type(fapl2, H5FD_MEM_DRAW)<0)
        goto error;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle2)<0)
        goto error;
    if(*fhandle2<0)
        goto error;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        goto error;

    /* After the data is written, the file size is huge because the
     * beginning of raw data file is set at HADDR_MAX/2.  It's supposed
     * to be (HADDR_MAX/2 + 128*128*4)
     */
    if(file_size < HADDR_MAX/2 || file_size > HADDR_MAX)
        goto error;

    if(H5Sclose(space)<0)
        goto error;
    if(H5Dclose(dset)<0)
        goto error;
    if(H5Pclose(fapl2)<0)
        goto error;
    if(H5Fclose(file)<0)
        goto error;

    h5_cleanup(FILENAME, fapl);
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose(fapl);
        H5Pclose(fapl2);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the file handle interface(H5Fget_vfd_handle and
 *              H5FDget_vfd_handle)
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int                 nerrors=0;

    h5_reset();

    nerrors += test_sec2()<0      ?1:0;
    nerrors += test_core()<0      ?1:0;
    nerrors += test_family()<0    ?1:0;
    nerrors += test_family_compat()<0    ?1:0;
    nerrors += test_multi()<0     ?1:0;

    if (nerrors) goto error;
       printf("All file handle tests passed.\n");
    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d FILE HANDLE TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
}
