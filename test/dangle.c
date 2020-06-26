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
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, May 13, 2003
 *
 * Purpose:    Test dangling IDs
 */
#include "h5test.h"

const char *FILENAME[] = {
    "dangle",
    NULL
};

#define MAX_DANGLE      1000

#define DSETNAME        "Dataset"
#define GROUPNAME       "Group"
#define TYPENAME        "Type"
#define ATTRNAME        "Attribute"


/*-------------------------------------------------------------------------
 * Function:    test_dangle_dataset
 *
 * Purpose:    Check for dangling dataset IDs causing problems on library
 *              shutdown
 *
 * Return:    Success:    zero
 *        Failure:    non-zero
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_dataset(H5F_close_degree_t degree)
{
    char    filename[1024];
    hid_t fid;  /* File ID */
    hid_t fapl; /* File access property list */
    hid_t dsid; /* Dataset ID */
    hid_t sid;  /* Dataspace ID */
    unsigned u; /* Local index variable */

    TESTING("    dangling dataset IDs");

    if(H5open() < 0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl, degree) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if((sid = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR;

    if((dsid = H5Dcreate2(fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dsid) < 0)
        TEST_ERROR;

    /* Try creating duplicate dataset */
    H5E_BEGIN_TRY {
        if((dsid = H5Dcreate2(fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0)
            TEST_ERROR;
    } H5E_END_TRY;

    if(H5Sclose(sid) < 0)
        TEST_ERROR;

    /* Leave open a _lot_ of objects */
    for(u = 0; u < MAX_DANGLE; u++)
        if((dsid = H5Dopen2(fid, DSETNAME, H5P_DEFAULT)) < 0)
            TEST_ERROR;

    if(degree == H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid) >= 0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid) < 0)
            TEST_ERROR;

    if(h5_get_file_size(filename, fapl) < 0)
        TEST_ERROR;

    if(H5Pclose(fapl) < 0)
        TEST_ERROR;

    if(H5close() < 0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_dangle_group
 *
 * Purpose:    Check for dangling group IDs causing problems on library
 *              shutdown
 *
 * Return:    Success:    zero
 *        Failure:    non-zero
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_group(H5F_close_degree_t degree)
{
    char    filename[1024];
    hid_t fid;  /* File ID */
    hid_t fapl; /* File access property list */
    hid_t gid;  /* Group ID */
    unsigned u; /* Local index variable */

    TESTING("    dangling group IDs");

    if(H5open() < 0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl=H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl,degree) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if((gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    if(H5Gclose(gid) < 0)
        TEST_ERROR;

    /* Try creating duplicate group */
    H5E_BEGIN_TRY {
        gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if(gid >= 0) TEST_ERROR

    /* Leave open a _lot_ of objects */
    for(u = 0; u < MAX_DANGLE; u++)
        if((gid = H5Gopen2(fid, GROUPNAME, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR

    if((gid = H5Gopen2(fid, GROUPNAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(degree==H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid)>=0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid) < 0)
            TEST_ERROR;

    if(h5_get_file_size(filename, fapl) < 0)
        TEST_ERROR;

    if(H5Pclose(fapl) < 0)
        TEST_ERROR;

    if(H5close() < 0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_dangle_datatype1
 *
 * Purpose:    Check for dangling datatype IDs causing problems on library
 *              shutdown
 *
 * Return:    Success:    zero
 *        Failure:    non-zero
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_datatype1(H5F_close_degree_t degree)
{
    char    filename[1024];
    hid_t       fid;                    /* File ID */
    hid_t       fapl;                   /* File access property list */
    hid_t       tid;                    /* Datatype ID */
    unsigned    u;                      /* Local index variable */

    TESTING("    dangling named datatype IDs");

    if(H5open() < 0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl, degree) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;

    if(H5Tcommit2(fid, TYPENAME, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if(H5Tclose(tid) < 0)
        TEST_ERROR;

    /* Try creating duplicate named datatype */
    if((tid = H5Tcopy (H5T_NATIVE_INT)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY {
        if(H5Tcommit2(fid, TYPENAME, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) >= 0)
            TEST_ERROR;
    } H5E_END_TRY;
    if(H5Tclose(tid) < 0)
        TEST_ERROR;

    /* Leave open a _lot_ of objects */
    for(u = 0; u < MAX_DANGLE; u++)
        if((tid = H5Topen2(fid, TYPENAME, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR

    if(degree == H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid) >= 0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid) < 0)
            TEST_ERROR;

    if(h5_get_file_size(filename, fapl) < 0)
        TEST_ERROR;

    if(H5Pclose(fapl) < 0)
        TEST_ERROR;

    if(H5close() < 0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_dangle_datatype2
 *
 * Purpose:    Check for dangling datatype IDs causing problems on library
 *              shutdown
 *
 * Return:    Success:    zero
 *        Failure:    non-zero
 *
 * Programmer:    Quincey Koziol
 *              Thursday, August 25, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_datatype2(H5F_close_degree_t degree)
{
    char    filename[1024];
    hid_t       fid;                    /* File ID */
    hid_t       fapl;                   /* File access property list */
    hid_t       did;                    /* Dataset ID */
    hid_t       sid;                    /* Dataspace ID */
    hid_t       tid;                    /* Datatype ID */

    TESTING("    dangling named datatype ID used by dataset");

    if(H5open() < 0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl, degree) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;

    if(H5Tcommit2(fid, TYPENAME, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Create a dataset that uses the named datatype & leave it open */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR;
    if((did = H5Dcreate2(fid, DSETNAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if(H5Sclose(sid) < 0)
        TEST_ERROR;

    if(degree == H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid) >= 0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid) < 0)
            TEST_ERROR;

    if(h5_get_file_size(filename, fapl) < 0)
        TEST_ERROR;

    if(H5Pclose(fapl) < 0)
        TEST_ERROR;

    if(H5close() < 0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_dangle_attribute
 *
 * Purpose:    Check for dangling attribute IDs causing problems on library
 *              shutdown
 *
 * Return:    Success:    zero
 *        Failure:    non-zero
 *
 * Programmer:    Quincey Koziol
 *              Wednesday, June 18, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_attribute(H5F_close_degree_t degree)
{
    char    filename[1024];
    hid_t fid;  /* File ID */
    hid_t fapl; /* File access property list */
    hid_t dsid; /* Dataset ID */
    hid_t sid;  /* Dataspace ID */
    hid_t aid;  /* Attribute ID */
    unsigned u; /* Local index variable */

    TESTING("    dangling attribute IDs");

    if(H5open() < 0)
        TEST_ERROR;

    /* Create file access property list */
    if((fapl=H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set file close degree */
    if(H5Pset_fclose_degree(fapl,degree) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if((sid = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR;

    if((dsid = H5Dcreate2(fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create an attribute on the dataset */
    if((aid = H5Acreate2(dsid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Don't worry about writing the attribute - it will have a fill value */

    /* Close the attribute on the dataset */
    if(H5Aclose(aid) < 0)
        TEST_ERROR;

    /* Try creating duplicate attribute */
    H5E_BEGIN_TRY {
        if((aid = H5Acreate2(dsid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) >= 0)
            TEST_ERROR;
    } H5E_END_TRY;

    if(H5Sclose(sid) < 0)
        TEST_ERROR;

    /* Leave open a _lot_ of objects */
    for(u = 0; u < MAX_DANGLE; u++)
        if((aid = H5Aopen(dsid, ATTRNAME, H5P_DEFAULT)) < 0)
            TEST_ERROR

    if(H5Dclose(dsid) < 0)
        TEST_ERROR

    if(degree == H5F_CLOSE_SEMI) {
        H5E_BEGIN_TRY {
            if(H5Fclose(fid) >= 0)
                TEST_ERROR;
        } H5E_END_TRY;
    } /* end if */
    else
        if(H5Fclose(fid) < 0)
            TEST_ERROR;

    if(h5_get_file_size(filename, fapl) < 0)
        TEST_ERROR;

    if(H5Pclose(fapl) < 0)
        TEST_ERROR;

    if(H5close() < 0)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    PASSED();
    return 0;

error:
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    test_dangle_force
 *
 * Purpose:    Shut down all danging IDs with generic file & ID routines,
 *              instead of letting library shut then down.
 *
 * Return:    Success:    zero
 *        Failure:    non-zero
 *
 * Programmer:    Quincey Koziol
 *              Friday, October 29, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
test_dangle_force(void)
{
    char    filename[1024];
    hid_t fid;  /* File ID */
    hid_t gid, gid2;  /* Group IDs */
    hid_t dsid, dsid2; /* Dataset IDs */
    hid_t sid;  /* Dataspace ID */
    hid_t aid, aid2;  /* Attribute IDs */
    hid_t tid, tid2;  /* Named datatype IDs */
    ssize_t count;  /* Count of open objects */
    hid_t *objs = NULL;    /* Pointer to list of open objects */
    size_t u;   /* Local index variable */

    TESTING("force dangling IDs to close, from API routines");

    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create a dataspace for the dataset & attribute to use */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        FAIL_STACK_ERROR

    /* Create a dataset */
    if((dsid = H5Dcreate2(fid, DSETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Re-open the dataset */
    if((dsid2 = H5Dopen2(fid, DSETNAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create an attribute on the dataset */
    if((aid = H5Acreate2(dsid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Re-open the attribute */
    if((aid2 = H5Aopen(dsid, ATTRNAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Close the dataspace ID */
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR

    /* Open a group ID */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Open group again */
    if((gid2 = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create a named datatype */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        FAIL_STACK_ERROR
    if(H5Tcommit2(fid, TYPENAME, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Re-open the named datatype */
    if((tid2 = H5Topen2(fid, TYPENAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Increment the ref count on all the "second" objects */
    if(H5Iinc_ref(dsid2) < 0)
        FAIL_STACK_ERROR
    if(H5Iinc_ref(aid2) < 0)
        FAIL_STACK_ERROR
    if(H5Iinc_ref(gid2) < 0)
        FAIL_STACK_ERROR
    if(H5Iinc_ref(aid2) < 0)
        FAIL_STACK_ERROR

    /* Get the number of open objects */
    if((count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_ALL)) < 0)
        FAIL_STACK_ERROR
    if(0 == count)
        TEST_ERROR;

    /* Allocate the array of object IDs */
    if(NULL == (objs = (hid_t *)HDcalloc((size_t)count, sizeof(hid_t))))
        TEST_ERROR;

    /* Get the list of open IDs */
    if(H5Fget_obj_ids((hid_t)H5F_OBJ_ALL, H5F_OBJ_ALL, (size_t)count, objs) < 0)
        FAIL_STACK_ERROR

    /* Close all open IDs */
    for(u = 0; u < (size_t)count; u++)
        while(H5Iget_type(objs[u]) != H5I_BADID && H5Iget_ref(objs[u]) > 0)
            H5Idec_ref(objs[u]);

    /* Get the number of open objects */
    if((count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_ALL)) < 0)
        FAIL_STACK_ERROR
    if(0 != count)
        TEST_ERROR;

    /* Clean up temporary file */
    HDremove(filename);

    /* Release object ID array */
    HDfree(objs);

    PASSED();
    return 0;

error:
    if(objs)
        HDfree(objs);
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    Executes dangling ID tests
 *
 * Return:    Success:    zero
 *        Failure:    non-zero
 *
 * Programmer:    Quincey Koziol
 *              Tuesday, May 13, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int        nerrors=0;

    /* Run tests w/weak file close */
    puts("Testing dangling objects with weak file close:");
    nerrors += test_dangle_dataset(H5F_CLOSE_WEAK);
    nerrors += test_dangle_group(H5F_CLOSE_WEAK);
    nerrors += test_dangle_datatype1(H5F_CLOSE_WEAK);
    nerrors += test_dangle_datatype2(H5F_CLOSE_WEAK);
    nerrors += test_dangle_attribute(H5F_CLOSE_WEAK);

    /* Run tests w/semi file close */
    puts("Testing dangling objects with semi file close:");
    nerrors += test_dangle_dataset(H5F_CLOSE_SEMI);
    nerrors += test_dangle_group(H5F_CLOSE_SEMI);
    nerrors += test_dangle_datatype1(H5F_CLOSE_SEMI);
    nerrors += test_dangle_datatype2(H5F_CLOSE_SEMI);
    nerrors += test_dangle_attribute(H5F_CLOSE_SEMI);

    /* Run tests w/strong file close */
    puts("Testing dangling objects with strong file close:");
    nerrors += test_dangle_dataset(H5F_CLOSE_STRONG);
    nerrors += test_dangle_group(H5F_CLOSE_STRONG);
    nerrors += test_dangle_datatype1(H5F_CLOSE_STRONG);
    nerrors += test_dangle_datatype2(H5F_CLOSE_STRONG);
    nerrors += test_dangle_attribute(H5F_CLOSE_STRONG);

    /* Close open IDs "the hard way" */
    nerrors += test_dangle_force();

    /* Check for errors */
    if (nerrors)
        goto error;
    puts("All dangling ID tests passed.");

    return 0;

error:
    puts("***** DANGLING ID TESTS FAILED *****");
    return 1;
}

