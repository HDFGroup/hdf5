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

/* Property definitions */
#define CLASS1_NAME     "Class 1"
#define CLASS1_HASHSIZE 25

#define CLASS2_NAME     "Class 2"
#define CLASS2_HASHSIZE 25

/* Property definitions */
#define PROP1_NAME     "Property 1"
int         prop1_def=10;   /* Property 1 default value */
#define PROP1_SIZE      sizeof(prop1_def)
#define PROP1_DEF_VALUE (&prop1_def)

#define PROP2_NAME     "Property 2"
float         prop2_def=3.14;   /* Property 2 default value */
#define PROP2_SIZE      sizeof(prop2_def)
#define PROP2_DEF_VALUE (&prop2_def)

#define PROP3_NAME     "Property 3"
char          prop3_def[10]="Ten chars";   /* Property 3 default value */
#define PROP3_SIZE      sizeof(prop3_def)
#define PROP3_DEF_VALUE (&prop3_def)

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

    /* Create another new generic class, derived from file creation class */
    cid1 = H5Pcreate_class(H5P_FILE_CREATE_NEW,CLASS2_NAME,CLASS2_HASHSIZE,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check class name */
    name = H5Pget_class_name(cid1);
    CHECK_PTR(name, "H5Pget_class_name");
    if(HDstrcmp(name,CLASS2_NAME)!=0) {
        num_errs++;
        printf("Class names don't match!, name=%s, CLASS2_NAME=%s\n",name,CLASS2_NAME);
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
**  test_genprop_basic_class_prop(): Test basic generic property list code.
**      Tests adding properties to generic classes.
** 
****************************************************************/
static void 
test_genprop_basic_class_prop(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    size_t		size;		/* Size of property */
    size_t		nprops;		/* Number of properties in class */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Class Properties Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 0, "H5Pget_nprops");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Try to insert the first property again (should fail) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL);
    VERIFY(ret, FAIL, "H5Pregister");

    /* Check the size of the first property */
    ret = H5Pget_size(cid1,PROP1_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP1_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 1, "H5Pget_nprops");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Try to insert the second property again (should fail) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL);
    VERIFY(ret, FAIL, "H5Pregister");

    /* Check the size of the second property */
    ret = H5Pget_size(cid1,PROP2_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP2_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP3_NAME,PROP3_SIZE,PROP3_DEF_VALUE,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Check the size of the third property */
    ret = H5Pget_size(cid1,PROP3_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP3_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Unregister first property */
    ret = H5Punregister(cid1,PROP1_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Try to check the size of the first property (should fail) */
    ret = H5Pget_size(cid1,PROP1_NAME,&size);
    VERIFY(ret, FAIL, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Unregister second property */
    ret = H5Punregister(cid1,PROP2_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 1, "H5Pget_nprops");

    /* Unregister third property */
    ret = H5Punregister(cid1,PROP3_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 0, "H5Pget_nprops");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_basic_class_prop() */

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
    test_genprop_basic_class_prop(); /* Test basic code for adding properties to a generic class */

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

