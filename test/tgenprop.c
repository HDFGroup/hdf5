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

/* $Id$ */

/***********************************************************
*
* Test program:	 tgenprop
*
* Test the Generic Property functionality
*
*************************************************************/

#include "testhdf5.h"
#include "hdf5.h"

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

#define PROP4_NAME     "Property 4"
double          prop4_def=1.41;   /* Property 4 default value */
#define PROP4_SIZE      sizeof(prop4_def)
#define PROP4_DEF_VALUE (&prop4_def)

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
    MESSAGE(5, ("Testing Basic Generic Property List Class Creation Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL,NULL,NULL);
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
    cid1 = H5Pcreate_class(H5P_FILE_CREATE_NEW,CLASS2_NAME,CLASS2_HASHSIZE,NULL,NULL,NULL,NULL,NULL,NULL);
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
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 0, "H5Pget_nprops");

    /* Check the existance of the first property (should fail) */
    ret = H5Pexist(cid1,PROP1_NAME);
    VERIFY(ret, 0, "H5Pexist");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Try to insert the first property again (should fail) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    VERIFY(ret, FAIL, "H5Pregister");

    /* Check the existance of the first property */
    ret = H5Pexist(cid1,PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the size of the first property */
    ret = H5Pget_size(cid1,PROP1_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP1_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 1, "H5Pget_nprops");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Try to insert the second property again (should fail) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    VERIFY(ret, FAIL, "H5Pregister");

    /* Check the existance of the second property */
    ret = H5Pexist(cid1,PROP2_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the size of the second property */
    ret = H5Pget_size(cid1,PROP2_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP2_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP3_NAME,PROP3_SIZE,PROP3_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Check the existance of the third property */
    ret = H5Pexist(cid1,PROP3_NAME);
    VERIFY(ret, 1, "H5Pexist");

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
**  test_genprop_iter1(): Property iterator for test_genprop_class_iter
** 
****************************************************************/
static int 
test_genprop_iter1(hid_t id, const char *name, void *iter_data)
{
    struct {                /* Struct for iterations */
        int iter_count;
        const char **names;
    } *iter_struct=iter_data;

    /* Shut compiler up */
    id=id;

    return(HDstrcmp(name,iter_struct->names[iter_struct->iter_count++]));
}

/****************************************************************
**
**  test_genprop_class_iter(): Test basic generic property list code.
**      Tests iterating over properties in a generic class.
** 
****************************************************************/
static void 
test_genprop_class_iter(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    size_t		nprops;		/* Number of properties in class */
    int         idx;        /* Index to start iteration at */
    struct {                /* Struct for iterations */
        int iter_count;
        const char **names;
    } iter_struct;
    const char *pnames[4]={ /* Names of properties for iterator */
        PROP1_NAME,
        PROP2_NAME,
        PROP3_NAME,
        PROP4_NAME};
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Class Property Iteration Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP3_NAME,PROP3_SIZE,PROP3_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP4_NAME,PROP4_SIZE,PROP4_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Iterate over all properties in class */
    iter_struct.iter_count=0;
    iter_struct.names=pnames;
    ret = H5Piterate(cid1,NULL,test_genprop_iter1,&iter_struct);
    VERIFY(ret, 0, "H5Piterate");

    /* Iterate over last three properties in class */
    idx=iter_struct.iter_count=1;
    ret = H5Piterate(cid1,&idx,test_genprop_iter1,&iter_struct);
    VERIFY(ret, 0, "H5Piterate");
    VERIFY(idx, (int)nprops, "H5Piterate");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_class_iter() */

/****************************************************************
**
**  test_genprop_cls_cb1(): Property List callback for test_genprop_class_callback
** 
****************************************************************/
static herr_t 
test_genprop_cls_cb1(hid_t list_id, void *create_data)
{
    struct {                /* Struct for iterations */
        int count;
        hid_t id;
    } *count_struct=create_data;

    count_struct->count++;
    count_struct->id=list_id;

    return(SUCCEED);
}

static herr_t 
test_genprop_cls_cb2(hid_t new_list_id, hid_t old_list_id, void *create_data)
{
    struct {                /* Struct for iterations */
        int count;
        hid_t id;
    } *count_struct=create_data;

    count_struct->count++;
    count_struct->id=new_list_id;

    return(SUCCEED);
}
/****************************************************************
**
**  test_genprop_class_callback(): Test basic generic property list code.
**      Tests callbacks for property lists in a generic class.
** 
****************************************************************/
static void 
test_genprop_class_callback(void)
{
    hid_t	cid1;		/* Generic Property class ID */
    hid_t	lid1;		/* Generic Property list ID */
    hid_t	lid2;		/* Generic Property list ID */
    size_t	nprops;		/* Number of properties in class */
    struct {                    /* Struct for callbacks */
        int count;
        hid_t id;
    } crt_cb_struct, cls_cb_struct;
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Class Callback Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,test_genprop_cls_cb1,&crt_cb_struct,NULL,NULL,test_genprop_cls_cb1,&cls_cb_struct);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP3_NAME,PROP3_SIZE,PROP3_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert fourth property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP4_NAME,PROP4_SIZE,PROP4_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Initialize class callback structs */
    crt_cb_struct.count=0;
    crt_cb_struct.id=(-1);
    cls_cb_struct.count=0;
    cls_cb_struct.id=(-1);

    /* Create a property list from the class */
    lid1 = H5Pcreate_list(cid1);
    CHECK_I(lid1, "H5Pcreate_list");

    /* Verify that the creation callback occurred */
    VERIFY(crt_cb_struct.count, 1, "H5Pcreate_list");
    VERIFY(crt_cb_struct.id, lid1, "H5Pcreate_list");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Create another property list from the class */
    lid2 = H5Pcreate_list(cid1);
    CHECK_I(lid2, "H5Pcreate_list");

    /* Verify that the creation callback occurred */
    VERIFY(crt_cb_struct.count, 2, "H5Pcreate_list");
    VERIFY(crt_cb_struct.id, lid2, "H5Pcreate_list");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid2,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Close first list */
    ret = H5Pclose_list(lid1);
    CHECK_I(ret, "H5Pclose_list");

    /* Verify that the close callback occurred */
    VERIFY(cls_cb_struct.count, 1, "H5Pclose_list");
    VERIFY(cls_cb_struct.id, lid1, "H5Pclose_list");

    /* Close second list */
    ret = H5Pclose_list(lid2);
    CHECK_I(ret, "H5Pclose_list");

    /* Verify that the close callback occurred */
    VERIFY(cls_cb_struct.count, 2, "H5Pclose_list");
    VERIFY(cls_cb_struct.id, lid2, "H5Pclose_list");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_class_callback() */

/****************************************************************
**
**  test_genprop_basic_list(): Test basic generic property list code.
**      Tests creating new generic property lists.
** 
****************************************************************/
static void 
test_genprop_basic_list(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		cid2;		/* Generic Property class ID */
    hid_t		lid1;		/* Generic Property list ID */
    size_t		nprops;		/* Number of properties */
    size_t		size;		/* Size of property */
    int                 prop1_value;    /* Value for property #1 */
    float               prop2_value;    /* Value for property #2 */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Creation Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Add several properties (w/default values) */

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Create a property list from the class */
    lid1 = H5Pcreate_list(cid1);
    CHECK_I(lid1, "H5Pcreate_list");

    /* Get the list's class */
    cid2 = H5Pget_class_new(lid1);
    CHECK_I(cid2, "H5Pget_class_new");

    /* Check that the list's class is correct */
    ret = H5Pequal(cid1,cid2);
    VERIFY(ret, 1, "H5Pequal");

    /* Check correct "is a" class/list relationship */
    ret = H5Pisa_class(lid1,cid1);
    VERIFY(ret, 1, "H5Pisa_class");

    /* Check "is a" class/list relationship another way */
    ret = H5Pisa_class(lid1,cid2);
    VERIFY(ret, 1, "H5Pisa_class");

    /* Close class */
    ret = H5Pclose_class(cid2);
    CHECK_I(ret, "H5Pclose_class");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Check existence of properties */
    ret = H5Pexist(lid1,PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1,PROP2_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the sizes of the properties */
    ret = H5Pget_size(lid1,PROP1_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP1_SIZE, "H5Pget_size");
    ret = H5Pget_size(lid1,PROP2_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP2_SIZE, "H5Pget_size");

    /* Check values of properties (set with default values) */
    ret = H5Pget(lid1,PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");
    ret = H5Pget(lid1,PROP2_NAME,&prop2_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop2_value, *PROP2_DEF_VALUE, "H5Pget");

    /* Close list */
    ret = H5Pclose_list(lid1);
    CHECK_I(ret, "H5Pclose_list");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

} /* end test_genprop_basic_list() */

/****************************************************************
**
**  test_genprop_basic_list_prop(): Test basic generic property list code.
**      Tests creating new generic property lists and adding and
**      removing properties from them.
** 
****************************************************************/
static void 
test_genprop_basic_list_prop(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		lid1;		/* Generic Property list ID */
    size_t		nprops;		/* Number of properties */
    int                 prop1_value;    /* Value for property #1 */
    float               prop2_value;    /* Value for property #2 */
    char                prop3_value[10];/* Property #3 value */
    double              prop4_value;    /* Property #4 value */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Property Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Add several properties (several w/default values) */

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Create a property list from the class */
    lid1 = H5Pcreate_list(cid1);
    CHECK_I(lid1, "H5Pcreate_list");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Add temporary properties */

    /* Insert first temporary property into class (with no callbacks) */
    ret = H5Pinsert(lid1,PROP3_NAME,PROP3_SIZE,PROP3_DEF_VALUE,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pinsert");

    /* Insert second temporary property into class (with no callbacks) */
    ret = H5Pinsert(lid1,PROP4_NAME,PROP4_SIZE,PROP4_DEF_VALUE,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pinsert");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Check existence of all properties */
    ret = H5Pexist(lid1,PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1,PROP2_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1,PROP3_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1,PROP4_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of permanent properties (set with default values) */
    ret = H5Pget(lid1,PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");
    ret = H5Pget(lid1,PROP2_NAME,&prop2_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop2_value, *PROP2_DEF_VALUE, "H5Pget");

    /* Check values of temporary properties (set with regular values) */
    ret = H5Pget(lid1,PROP3_NAME,&prop3_value);
    CHECK_I(ret, "H5Pget");
    if(memcmp(&prop3_value,PROP3_DEF_VALUE,PROP3_SIZE)!=0) {
        num_errs++;
        printf("Property #3 doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    ret = H5Pget(lid1,PROP4_NAME,&prop4_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop4_value, *PROP4_DEF_VALUE, "H5Pget");

    /* Delete permanent property */
    ret = H5Premove(lid1,PROP2_NAME);
    CHECK_I(ret, "H5Premove");

    /* Check number of properties */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Delete temporary property */
    ret = H5Premove(lid1,PROP3_NAME);
    CHECK_I(ret, "H5Premove");

    /* Check number of properties */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Check existence of remaining properties */
    ret = H5Pexist(lid1,PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1,PROP4_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of permanent properties (set with default values) */
    ret = H5Pget(lid1,PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");

    /* Check values of temporary properties (set with regular values) */
    ret = H5Pget(lid1,PROP4_NAME,&prop4_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop4_value, *PROP4_DEF_VALUE, "H5Pget");

    /* Close list */
    ret = H5Pclose_list(lid1);
    CHECK_I(ret, "H5Pclose_list");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

} /* end test_genprop_basic_list_prop() */

/****************************************************************
**
**  test_genprop_iter2(): Property iterator for test_genprop_list_iter
** 
****************************************************************/
static int 
test_genprop_iter2(hid_t id, const char *name, void *iter_data)
{
    struct {                /* Struct for iterations */
        int iter_count;
        const char **names;
    } *iter_struct=iter_data;

    /* Shut compiler up */
    id=id;

    return(HDstrcmp(name,iter_struct->names[iter_struct->iter_count++]));
}

/****************************************************************
**
**  test_genprop_list_iter(): Test basic generic property list code.
**      Tests iterating over generic property list properties.
** 
****************************************************************/
static void 
test_genprop_list_iter(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		lid1;		/* Generic Property list ID */
    size_t		nprops;		/* Number of properties */
    int         idx;        /* Index to start iteration at */
    struct {                /* Struct for iterations */
        int iter_count;
        const char **names;
    } iter_struct;
    const char *pnames[4]={ /* Names of properties for iterator */
        PROP1_NAME,
        PROP2_NAME,
        PROP3_NAME,
        PROP4_NAME};
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Generic Property List Iteration Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Add several properties (several w/default values) */

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Create a property list from the class */
    lid1 = H5Pcreate_list(cid1);
    CHECK_I(lid1, "H5Pcreate_list");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Add temporary properties */

    /* Insert first temporary property into class (with no callbacks) */
    ret = H5Pinsert(lid1,PROP3_NAME,PROP3_SIZE,PROP3_DEF_VALUE,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pinsert");

    /* Insert second temporary property into class (with no callbacks) */
    ret = H5Pinsert(lid1,PROP4_NAME,PROP4_SIZE,PROP4_DEF_VALUE,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pinsert");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Iterate over all properties in list */
    iter_struct.iter_count=0;
    iter_struct.names=pnames;
    ret = H5Piterate(lid1,NULL,test_genprop_iter2,&iter_struct);
    VERIFY(ret, 0, "H5Piterate");

    /* Iterate over last three properties in list */
    idx=iter_struct.iter_count=1;
    ret = H5Piterate(lid1,&idx,test_genprop_iter2,&iter_struct);
    VERIFY(ret, 0, "H5Piterate");
    VERIFY(idx, (int)nprops, "H5Piterate");

    /* Close list */
    ret = H5Pclose_list(lid1);
    CHECK_I(ret, "H5Pclose_list");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

} /* end test_genprop_list_iter() */

typedef struct {
    /* Creation information */
    int crt_count;
    char *crt_name;
    void *crt_value;

    /* Set information */
    int set_count;
    hid_t set_plist_id;
    char *set_name;
    void *set_value;

    /* Get information */
    int get_count;
    hid_t get_plist_id;
    char *get_name;
    void *get_value;

    /* Delete information */
    int del_count;
    hid_t del_plist_id;
    char *del_name;
    void *del_value;

    /* Copy information */
    int cop_count;
    char *cop_name;
    void *cop_value;

    /* Close information */
    int cls_count;
    char *cls_name;
    void *cls_value;
} prop_cb_info;

/* Global variables for Callback information */
prop_cb_info prop1_cb_info;     /* Callback statistics for property #1 */
prop_cb_info prop2_cb_info;     /* Callback statistics for property #2 */

/****************************************************************
**
**  test_genprop_prop_crt_cb1(): Property creation callback for test_genprop_list_callback
** 
****************************************************************/
static herr_t 
test_genprop_prop_crt_cb1(const char *name, size_t size, void *def_value)
{
    /* Set the information from the creation call */
    prop1_cb_info.crt_count++;
    prop1_cb_info.crt_name=HDstrdup(name);
    prop1_cb_info.crt_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.crt_value,def_value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_set_cb1(): Property set callback for test_genprop_list_callback
** 
****************************************************************/
static herr_t 
test_genprop_prop_set_cb1(hid_t plist_id, const char *name, size_t size, void *value)
{
    /* Set the information from the set call */
    prop1_cb_info.set_count++;
    prop1_cb_info.set_plist_id=plist_id;
    if(prop1_cb_info.set_name==NULL)
        prop1_cb_info.set_name=HDstrdup(name);
    if(prop1_cb_info.set_value==NULL)
        prop1_cb_info.set_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.set_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_get_cb1(): Property get callback for test_genprop_list_callback
** 
****************************************************************/
static herr_t 
test_genprop_prop_get_cb1(hid_t plist_id, const char *name, size_t size, void *value)
{
    /* Set the information from the get call */
    prop1_cb_info.get_count++;
    prop1_cb_info.get_plist_id=plist_id;
    if(prop1_cb_info.get_name==NULL)
        prop1_cb_info.get_name=HDstrdup(name);
    if(prop1_cb_info.get_value==NULL)
        prop1_cb_info.get_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.get_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_cop_cb1(): Property copy callback for test_genprop_list_callback
** 
****************************************************************/
static herr_t 
test_genprop_prop_cop_cb1(const char *name, size_t size, void *value)
{
    /* Set the information from the get call */
    prop1_cb_info.cop_count++;
    if(prop1_cb_info.cop_name==NULL)
        prop1_cb_info.cop_name=HDstrdup(name);
    if(prop1_cb_info.cop_value==NULL)
        prop1_cb_info.cop_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.cop_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_cls_cb1(): Property close callback for test_genprop_list_callback
** 
****************************************************************/
static herr_t 
test_genprop_prop_cls_cb1(const char *name, size_t size, void *value)
{
    /* Set the information from the close call */
    prop1_cb_info.cls_count++;
    prop1_cb_info.cls_name=HDstrdup(name);
    prop1_cb_info.cls_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.cls_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_del_cb2(): Property delete callback for test_genprop_list_callback
** 
****************************************************************/
static herr_t 
test_genprop_prop_del_cb2(hid_t plist_id, const char *name, size_t size, void *value)
{
    /* Set the information from the delete call */
    prop2_cb_info.del_count++;
    prop2_cb_info.del_plist_id=plist_id;
    prop2_cb_info.del_name=HDstrdup(name);
    prop2_cb_info.del_value=HDmalloc(size);
    HDmemcpy(prop2_cb_info.del_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_list_callback(): Test basic generic property list code.
**      Tests callbacks for properties in a generic property list.
** 
****************************************************************/
static void 
test_genprop_list_callback(void)
{
    hid_t	cid1;		/* Generic Property class ID */
    hid_t	lid1;		/* Generic Property list ID */
    hid_t	lid2;		/* 2nd Generic Property list ID */
    size_t	nprops;		/* Number of properties in class */
    int         prop1_value;    /* Value for property #1 */
    int         prop1_new_value=20;   /* Property #1 new value */
    float       prop2_value;    /* Value for property #2 */
    char        prop3_value[10];/* Property #3 value */
    double      prop4_value;    /* Property #4 value */
    struct {                    /* Struct for callbacks */
        int count;
        hid_t id;
    } cop_cb_struct;
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Property Callback Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_NO_CLASS_NEW,CLASS1_NAME,CLASS1_HASHSIZE,NULL,NULL,test_genprop_cls_cb2,&cop_cb_struct,NULL,NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with callbacks) */
    ret = H5Pregister(cid1,PROP1_NAME,PROP1_SIZE,PROP1_DEF_VALUE,test_genprop_prop_crt_cb1,test_genprop_prop_set_cb1,test_genprop_prop_get_cb1,NULL,test_genprop_prop_cop_cb1,test_genprop_prop_cls_cb1);
    CHECK_I(ret, "H5Pregister");

    /* Insert second property into class (with only delete callback) */
    ret = H5Pregister(cid1,PROP2_NAME,PROP2_SIZE,PROP2_DEF_VALUE,NULL,NULL,NULL,test_genprop_prop_del_cb2,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP3_NAME,PROP3_SIZE,PROP3_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Insert fourth property into class (with no callbacks) */
    ret = H5Pregister(cid1,PROP4_NAME,PROP4_SIZE,PROP4_DEF_VALUE,NULL,NULL,NULL,NULL,NULL,NULL);
    CHECK_I(ret, "H5Pregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Initialize class callback structs */
    cop_cb_struct.count=0;
    cop_cb_struct.id=(-1);

    /* Initialize callback information for properties tracked */
    HDmemset(&prop1_cb_info,0,sizeof(prop_cb_info));
    HDmemset(&prop2_cb_info,0,sizeof(prop_cb_info));

    /* Create a property list from the class */
    lid1 = H5Pcreate_list(cid1);
    CHECK_I(lid1, "H5Pcreate_list");

    /* Verify creation callback information for properties tracked */
    VERIFY(prop1_cb_info.crt_count, 1, "H5Pcreate_list");
    if(HDstrcmp(prop1_cb_info.crt_name,PROP1_NAME)!=0) {
        num_errs++;
        printf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    if(HDmemcmp(prop1_cb_info.crt_value,PROP1_DEF_VALUE,PROP1_SIZE)!=0) {
        num_errs++;
        printf("Property #1 value doesn't match!, line=%d\n",__LINE__);
    } /* end if */

    /* Check values of permanent properties (set with default values) */
    ret = H5Pget(lid1,PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");
    ret = H5Pget(lid1,PROP2_NAME,&prop2_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop2_value, *PROP2_DEF_VALUE, "H5Pget");

    /* Check values of temporary properties (set with regular values) */
    ret = H5Pget(lid1,PROP3_NAME,&prop3_value);
    CHECK_I(ret, "H5Pget");
    if(memcmp(&prop3_value,PROP3_DEF_VALUE,PROP3_SIZE)!=0) {
        num_errs++;
        printf("Property #3 doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    ret = H5Pget(lid1,PROP4_NAME,&prop4_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop4_value, *PROP4_DEF_VALUE, "H5Pget");

    /* Verify get callback information for properties tracked */
    VERIFY(prop1_cb_info.get_count, 1, "H5Pget");
    VERIFY(prop1_cb_info.get_plist_id, lid1, "H5Pget");
    if(HDstrcmp(prop1_cb_info.get_name,PROP1_NAME)!=0) {
        num_errs++;
        printf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    if(HDmemcmp(prop1_cb_info.get_value,PROP1_DEF_VALUE,PROP1_SIZE)!=0) {
        num_errs++;
        printf("Property #1 value doesn't match!, line=%d\n",__LINE__);
    } /* end if */

    /* Set value of property #1 to different value */
    ret = H5Pset(lid1,PROP1_NAME,&prop1_new_value);
    CHECK_I(ret, "H5Pset");

    /* Verify set callback information for properties tracked */
    VERIFY(prop1_cb_info.set_count, 1, "H5Pset");
    VERIFY(prop1_cb_info.set_plist_id, lid1, "H5Pset");
    if(HDstrcmp(prop1_cb_info.set_name,PROP1_NAME)!=0) {
        num_errs++;
        printf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    if(HDmemcmp(prop1_cb_info.set_value,&prop1_new_value,PROP1_SIZE)!=0) {
        num_errs++;
        printf("Property #1 value doesn't match!, line=%d\n",__LINE__);
    } /* end if */

    /* Check new value of tracked properties */
    ret = H5Pget(lid1,PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, prop1_new_value, "H5Pget");

    /* Verify get callback information again for properties tracked */
    VERIFY(prop1_cb_info.get_count, 2, "H5Pget");
    VERIFY(prop1_cb_info.get_plist_id, lid1, "H5Pget");
    if(HDstrcmp(prop1_cb_info.get_name,PROP1_NAME)!=0) {
        num_errs++;
        printf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    if(HDmemcmp(prop1_cb_info.get_value,&prop1_new_value,PROP1_SIZE)!=0) {
        num_errs++;
        printf("Property #1 value doesn't match!, line=%d\n",__LINE__);
    } /* end if */

    /* Delete property #2 */
    ret = H5Premove(lid1,PROP2_NAME);
    CHECK_I(ret, "H5Premove");

    /* Verify delete callback information for properties tracked */
    VERIFY(prop2_cb_info.del_count, 1, "H5Premove");
    VERIFY(prop2_cb_info.del_plist_id, lid1, "H5Premove");
    if(HDstrcmp(prop2_cb_info.del_name,PROP2_NAME)!=0) {
        num_errs++;
        printf("Property #2 name doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    if(HDmemcmp(prop2_cb_info.del_value,PROP2_DEF_VALUE,PROP2_SIZE)!=0) {
        num_errs++;
        printf("Property #2 value doesn't match!, line=%d\n",__LINE__);
    } /* end if */

    /* Copy first list */
    lid2 = H5Pcopy(lid1);
    CHECK_I(lid2, "H5Pcopy");

    /* Verify copy callback information for properties tracked */
    VERIFY(prop1_cb_info.cop_count, 1, "H5Pcopy");
    if(HDstrcmp(prop1_cb_info.cop_name,PROP1_NAME)!=0) {
        num_errs++;
        printf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    if(HDmemcmp(prop1_cb_info.cop_value,&prop1_new_value,PROP1_SIZE)!=0) {
        num_errs++;
        printf("Property #1 value doesn't match!, line=%d\n",__LINE__);
    } /* end if */

    /* Verify that the class creation callback occurred */
    VERIFY(cop_cb_struct.count, 1, "H5Pcopy");
    VERIFY(cop_cb_struct.id, lid2, "H5Pcopy");

    /* Close first list */
    ret = H5Pclose_list(lid1);
    CHECK_I(ret, "H5Pclose_list");

    /* Verify close callback information for properties tracked */
    VERIFY(prop1_cb_info.cls_count, 1, "H5Pclose");
    if(HDstrcmp(prop1_cb_info.cls_name,PROP1_NAME)!=0) {
        num_errs++;
        printf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    } /* end if */
    if(HDmemcmp(prop1_cb_info.cls_value,&prop1_new_value,PROP1_SIZE)!=0) {
        num_errs++;
        printf("Property #1 value doesn't match!, line=%d\n",__LINE__);
    } /* end if */

    /* Free memory allocated for tracking properties */
    HDfree(prop1_cb_info.crt_name);
    HDfree(prop1_cb_info.crt_value);
    HDfree(prop1_cb_info.get_name);
    HDfree(prop1_cb_info.get_value);
    HDfree(prop1_cb_info.set_name);
    HDfree(prop1_cb_info.set_value);
    HDfree(prop1_cb_info.cop_name);
    HDfree(prop1_cb_info.cop_value);
    HDfree(prop1_cb_info.cls_name);
    HDfree(prop1_cb_info.cls_value);
    HDfree(prop2_cb_info.del_name);
    HDfree(prop2_cb_info.del_value);

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_list_callback() */

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
    test_genprop_class_iter();  /* Test code for iterating over properties in a generic class */
    test_genprop_class_callback();  /* Test code for property class callbacks */

    test_genprop_basic_list();  /* Test basic code for creating a generic property list */
    test_genprop_basic_list_prop(); /* Test basic code for adding properties to a generic property list */
    test_genprop_list_iter();  /* Test basic code for iterating over properties in a generic property list */
    test_genprop_list_callback();  /* Test code for property list callbacks */

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

