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
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, May 13, 2003
 *
 * Purpose:	Test dangling IDs
 */
#include "h5test.h"
#include "H5private.h"

const char *FILENAME[] = {
    "dangle",
    NULL
};

#define DSETNAME        "Dataset"
#define GROUPNAME       "Group"
#define TYPENAME        "Type"


/*-------------------------------------------------------------------------
 * Function:	get_file_size
 *
 * Purpose:	Get the current size of a file (in bytes)
 *
 * Return:	Success:	Size of file in bytes (could be 0)
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 22, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static off_t
get_file_size(const char *filename)
{
    h5_stat_t	sb;

    /* Get the file's statistics */
    if (HDstat(filename, &sb)>=0)
        return(sb.st_size);

    return(0);
} /* end get_file_size() */


/*-------------------------------------------------------------------------
 * Function:	test_dangle_dataset
 *
 * Purpose:	Check for dangling dataset IDs causing problems on library
 *              shutdown
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_dataset(void)
{
    char	filename[1024];
    off_t file_size;    /* Size of file */
    hid_t fid;  /* File ID */
    hid_t dsid; /* Dataset ID */
    hid_t sid;  /* Dataspace ID */

    TESTING("dangling dataset IDs");

    if(H5open()<0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0)
        TEST_ERROR;

    if((sid = H5Screate (H5S_SCALAR))<0)
        TEST_ERROR;

    if((dsid = H5Dcreate (fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT))<0)
        TEST_ERROR;

    if(H5Dclose(dsid)<0)
        TEST_ERROR;

    if((dsid = H5Dopen (fid, DSETNAME))<0)
        TEST_ERROR;

    if((dsid = H5Dopen (fid, DSETNAME))<0)
        TEST_ERROR;

    if(H5Sclose(sid)<0)
        TEST_ERROR;

    if(H5Fclose(fid)<0)
        TEST_ERROR;

    if(H5close()<0)
        TEST_ERROR;

    if((file_size=get_file_size(filename))==0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();                                                 
    return 0;                                                 
                                                                      
error:                                                       
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_dangle_group
 *
 * Purpose:	Check for dangling group IDs causing problems on library
 *              shutdown
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_group(void)
{
    char	filename[1024];
    off_t file_size;    /* Size of file */
    hid_t fid;  /* File ID */
    hid_t gid;  /* Group ID */

    TESTING("dangling group IDs");

    if(H5open()<0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0)
        TEST_ERROR;

    if((gid = H5Gcreate (fid, GROUPNAME, 0))<0)
        TEST_ERROR;

    if(H5Gclose(gid)<0)
        TEST_ERROR;

    if((gid = H5Gopen (fid, GROUPNAME))<0)
        TEST_ERROR;

    if((gid = H5Gopen (fid, GROUPNAME))<0)
        TEST_ERROR;

    if(H5Fclose(fid)<0)
        TEST_ERROR;

    if(H5close()<0)
        TEST_ERROR;

    if((file_size=get_file_size(filename))==0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();                                                 
    return 0;                                                 
                                                                      
error:                                                       
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_dangle_datatype
 *
 * Purpose:	Check for dangling datatype IDs causing problems on library
 *              shutdown
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_datatype(void)
{
    char	filename[1024];
    off_t file_size;    /* Size of file */
    hid_t fid;  /* File ID */
    hid_t tid;  /* Datatype ID */

    TESTING("dangling datatype IDs");

    if(H5open()<0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0)
        TEST_ERROR;

    if((tid = H5Tcopy (H5T_NATIVE_INT))<0)
        TEST_ERROR;

    if(H5Tcommit(fid,TYPENAME,tid)<0)
        TEST_ERROR;

    if(H5Tclose(tid)<0)
        TEST_ERROR;

    if((tid = H5Topen (fid, TYPENAME))<0)
        TEST_ERROR;

    if((tid = H5Topen (fid, TYPENAME))<0)
        TEST_ERROR;

    if(H5Fclose(fid)<0)
        TEST_ERROR;

    if(H5close()<0)
        TEST_ERROR;

    if((file_size=get_file_size(filename))==0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();                                                 
    return 0;                                                 
                                                                      
error:                                                       
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Executes dangling ID tests
 *
 * Return:	Success:	zero
 *		Failure:	non-zero
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int		nerrors=0;

    /* Run tests */
    nerrors += test_dangle_dataset();
    nerrors += test_dangle_group();
    nerrors += test_dangle_datatype();

    /* Check for errors */
    if (nerrors)
        goto error;
    puts("All dangling ID tests passed.");

    return 0;

error:
    puts("***** DANGLING ID TESTS FAILED *****");
    return 1;
}

