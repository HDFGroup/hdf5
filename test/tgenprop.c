/****************************************************************************
 * NCSA HDF								                                    *
 * Software Development Group						                        *
 * National Center for Supercomputing Applications			                *
 * University of Illinois at Urbana-Champaign				                *
 * 605 E. Springfield, Champaign IL 61820				                    *
 *									                                        *
 * For conditions of distribution and use, see the accompanying		        *
 * hdf/COPYING file.							                            *
 *									                                        *
 ****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "$Revision$";
#endif

/* $Id$ */

/***********************************************************
*
* Test program:	 tgenprop
*
* Test the Generic Property functionality
*
*************************************************************/

#include <testhdf5.h>

#include <hdf5.h>

#define FILENAME   "tgenprop.h5"

#define CLASS1_NAME     "Class 1"
#define CLASS1_HASHSIZE 25

/****************************************************************
**
**  test_genprop_basic_class(): Test basic generic property list code.
**      Tests creating new generic classes.
** 
****************************************************************/
static void 
test_genprop_basic_class(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		cid2;		/* Generic Property class ID */
    hid_t		cid3;		/* Generic Property class ID */
    char       *name;       /* Name of class */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Creation Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check class name */
    name = H5Pget_class_name(cid1);
    CHECK_PTR(name, "H5Pget_class_name");
    if(HDstrcmp(name,CLASS1_NAME)!=0) {
        num_errs++;
        printf("Class names don't match!, name=%s, CLASS1_NAME=%s\n",name,CLASS1_NAME);
    } /* end if */
    free(name);

    /* Check class parent */
    cid2 = H5Pget_class_parent(cid1);
    CHECK_I(cid2, "H5Pget_class_parent");

    /* Verify class parent correct */
    ret = H5Pequal(cid2,H5P_NO_CLASS_NEW);
    VERIFY(ret, 1, "H5Pequal");

    /* Make certain false postives aren't being returned */
    ret = H5Pequal(cid2,H5P_FILE_CREATE_NEW);
    VERIFY(ret, 0, "H5Pequal");

    /* Close parent class */
    ret = H5Pclose_class(cid2);
    CHECK_I(ret, "H5Pclose_class");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

    /* Create a new generic class, derived from file creation class */
    cid1 = H5Pcreate_class(H5P_FILE_CREATE_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check class name */
    name = H5Pget_class_name(cid1);
    CHECK_PTR(name, "H5Pget_class_name");
    if(HDstrcmp(name,CLASS1_NAME)!=0) {
        num_errs++;
        printf("Class names don't match!, name=%s, CLASS1_NAME=%s\n",name,CLASS1_NAME);
    } /* end if */
    free(name);

    /* Check class parent */
    cid2 = H5Pget_class_parent(cid1);
    CHECK_I(cid2, "H5Pget_class_parent");

    /* Verify class parent correct */
    ret = H5Pequal(cid2,H5P_FILE_CREATE_NEW);
    VERIFY(ret, 1, "H5Pequal");

    /* Check class parent's parent */
    cid3 = H5Pget_class_parent(cid2);
    CHECK_I(cid3, "H5Pget_class_parent");

    /* Verify class parent's parent correct */
    ret = H5Pequal(cid3,H5P_NO_CLASS_NEW);
    VERIFY(ret, 1, "H5Pequal");

    /* Close parent class's parent */
    ret = H5Pclose_class(cid3);
    CHECK_I(ret, "H5Pclose_class");

    /* Close parent class */
    ret = H5Pclose_class(cid2);
    CHECK_I(ret, "H5Pclose_class");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_basic_class() */

/****************************************************************
**
**  test_genprop(): Main generic property testing routine.
** 
****************************************************************/
void 
test_genprop(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Generic Properties\n"));

    /* These tests use the same file... */
    test_genprop_basic_class(); /* Test basic code for creating a generic class */

}   /* test_genprop() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_genprop
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              June 8, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_genprop(void)
{
    remove(FILENAME);
}

