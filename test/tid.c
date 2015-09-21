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

/* Test user-created identifiers (hid_t's) and identifier types. */

#include "testhdf5.h"
#include "hdf5.h"

	/* Include H5Ipkg.h to calculate max number of groups */
#define H5I_FRIEND		/*suppress error about including H5Ipkg	  */
#include "H5Ipkg.h"

	/* Test basic functionality of registering and deleting types and IDs */
static int basic_id_test(void)
{
	H5I_type_t myType = H5I_BADID;
	hid_t arrayID = H5I_INVALID_HID;
	void* testObj = NULL;
	void* testPtr = NULL;
	char nameString[10];
	hid_t testID;
	ssize_t testSize = -1;
	herr_t err;
	int num_ref;
        hsize_t num_members;


		/* Try to register an ID with ficticious types */
	H5E_BEGIN_TRY
		arrayID = H5Iregister((H5I_type_t) 420, testObj);
	H5E_END_TRY

	VERIFY(arrayID, H5I_INVALID_HID, "H5Iregister");
	if(arrayID != H5I_INVALID_HID)
		goto out;

	H5E_BEGIN_TRY
		arrayID = H5Iregister((H5I_type_t) -1, testObj);
	H5E_END_TRY

	VERIFY(arrayID, H5I_INVALID_HID, "H5Iregister");
	if(arrayID != H5I_INVALID_HID)
		goto out;

                /* Try to access IDs with ficticious types */
	H5E_BEGIN_TRY
            testPtr = H5Iobject_verify((hid_t)100, (H5I_type_t) 0);
	H5E_END_TRY

	VERIFY(testPtr, NULL, "H5Iobject_verify");
	if(testPtr != NULL)
		goto out;

	H5E_BEGIN_TRY
            testPtr = H5Iobject_verify((hid_t)700, (H5I_type_t) 700);
	H5E_END_TRY

	VERIFY(testPtr, NULL, "H5Iobject_verify");
	if(testPtr != NULL)
		goto out;

		/* Register a type */
	myType = H5Iregister_type((size_t)64, 0, (H5I_free_t) free );

	CHECK(myType, H5I_BADID, "H5Iregister_type");
	if(myType == H5I_BADID)
		goto out;

	/* Register an ID and retrieve the object it points to.
	 * Once the ID has been registered, testObj will be freed when
         * its ID type is destroyed. */
	testObj = HDmalloc(7 * sizeof(int));
	arrayID = H5Iregister(myType, testObj);

	CHECK(arrayID, H5I_INVALID_HID, "H5Iregister");
	if(arrayID == H5I_INVALID_HID)
        {
		HDfree(testObj);
		goto out;
        }

	testPtr = (int *) H5Iobject_verify(arrayID, myType);

	VERIFY(testPtr, testObj, "H5Iobject_verify");
	if(testPtr != testObj)
		goto out;

	/* Ensure that H5Iget_file_id and H5Iget_name() fail, since this
         * is an hid_t for the wrong kind of object */
	H5E_BEGIN_TRY
		testID = H5Iget_file_id(arrayID);
	H5E_END_TRY

	VERIFY(testID, H5I_INVALID_HID, "H5Iget_file_id");
	if(testID != H5I_INVALID_HID)
		goto out;

	H5E_BEGIN_TRY
		testSize = H5Iget_name(arrayID, nameString, (size_t)9);
	H5E_END_TRY

	VERIFY(testSize, -1, "H5Iget_name");
	if(testSize != -1)
		goto out;

	/* Make sure H5Iremove_verify catches objects of the wrong type */
	H5E_BEGIN_TRY
		testPtr = (int*) H5Iremove_verify(arrayID, (H5I_type_t) 0);
	H5E_END_TRY

	VERIFY(testPtr, NULL, "H5Iremove_verify");
	if(testPtr != NULL)
		goto out;

	H5E_BEGIN_TRY
		testPtr = (int*) H5Iremove_verify(arrayID, (H5I_type_t) ((int) myType-1));
	H5E_END_TRY

	VERIFY(testPtr, NULL, "H5Iremove_verify");
	if(testPtr != NULL)
		goto out;

		/* Remove an ID and make sure we can't access it */
	testPtr = (int*) H5Iremove_verify(arrayID, myType);

	CHECK(testPtr, NULL, "H5Iremove_verify");
	if(testPtr == NULL)
		goto out;

	H5E_BEGIN_TRY
		testPtr = (int*) H5Iobject_verify(arrayID, myType);
	H5E_END_TRY

	VERIFY(testPtr, NULL, "H5Iobject_verify");
	if(testPtr != NULL)
		goto out;

	/* Delete the type and make sure we can't access objects within it */
	arrayID = H5Iregister(myType, testObj);

	err = H5Idestroy_type(myType);
	VERIFY(err, 0, "H5Idestroy_type");
	if( err != 0)
		goto out;
        VERIFY(H5Itype_exists(myType), 0, "H5Itype_exists");
        if(H5Itype_exists(myType) != 0)
            goto out;

        H5E_BEGIN_TRY
	VERIFY(H5Inmembers(myType, NULL), -1, "H5Inmembers");
	if(H5Inmembers(myType, NULL) != -1)
		goto out;
        H5E_END_TRY

	/* Register another type and another object in that type */
	myType = H5Iregister_type((size_t)64, 0, (H5I_free_t) free );

	CHECK(myType, H5I_BADID, "H5Iregister_type");
	if(myType == H5I_BADID)
		goto out;

	/* The memory that testObj pointed to should already have been
	 * freed when the previous type was destroyed.  Allocate new
	 * memory for it.
         */
	testObj = HDmalloc(7 * sizeof(int));
	arrayID = H5Iregister(myType, testObj);

	CHECK(arrayID, H5I_INVALID_HID, "H5Iregister");
	if(arrayID == H5I_INVALID_HID)
	{
		HDfree(testObj);
		goto out;
	}

	err = H5Inmembers(myType, &num_members);
        CHECK(err, -1, "H5Inmembers");
        if (err < 0)
            goto out;
	VERIFY(num_members, 1, "H5Inmembers");
	if(num_members != 1)
		goto out;

	/* Increment references to type and ensure that dec_type_ref
		doesn't destroy the type */
	num_ref = H5Iinc_type_ref(myType);
	VERIFY(num_ref, 2, "H5Iinc_type_ref");
	if( num_ref != 2)
		goto out;
	num_ref = H5Idec_type_ref(myType);
	VERIFY(num_ref, 1, "H5Idec_type_ref");
	if(num_ref != 1)
		goto out;
        err = H5Inmembers(myType, &num_members);
        CHECK(err, -1, "H5Inmembers");
        if (err < 0)
            goto out;
	VERIFY(num_members, 1, "H5Inmembers");
	if(num_members != 1)
		goto out;

	/* This call to dec_type_ref should destroy the type */
	num_ref = H5Idec_type_ref(myType);
	VERIFY(num_ref, 0, "H5Idec_type_ref");
	if(num_ref != 0)
		goto out;
        VERIFY(H5Itype_exists(myType), 0, "H5Itype_exists");
        if (H5Itype_exists(myType) != 0)
            goto out;

        H5E_BEGIN_TRY
        err = H5Inmembers(myType, &num_members);
	if(err >= 0)
		goto out;
        H5E_END_TRY

	return 0;

out:
	/* Clean up type if it has been allocated and free memory used
         * by testObj */
	if(myType >= 0)
		H5Idestroy_type(myType);

	return -1;
}


	/* A dummy search function for the next test */
static int test_search_func(void H5_ATTR_UNUSED * ptr1, void H5_ATTR_UNUSED * ptr2) { return 0; }

	/* Ensure that public functions cannot access "predefined" ID types */
static int id_predefined_test(void )
{
	void * testObj;
	hid_t testID;
	hid_t typeID = H5I_INVALID_HID;
	void * testPtr;
	herr_t testErr;

	testObj = HDmalloc(sizeof(int));

	/* Try to perform illegal functions on various predefined types */
	H5E_BEGIN_TRY
		testID = H5Iregister(H5I_FILE, testObj);
	H5E_END_TRY

	VERIFY(testID, H5I_INVALID_HID, "H5Iregister");
	if(testID != H5I_INVALID_HID)
		goto out;

	H5E_BEGIN_TRY
		testPtr = H5Isearch(H5I_GENPROP_LST, (H5I_search_func_t) test_search_func, testObj);
	H5E_END_TRY

	VERIFY(testPtr, NULL, "H5Isearch");
	if(testPtr != NULL)
		goto out;

	H5E_BEGIN_TRY
		testErr = H5Inmembers(H5I_ERROR_STACK, NULL);
	H5E_END_TRY

	VERIFY(testErr, -1, "H5Inmembers");
	if(testErr != -1)
		goto out;

	H5E_BEGIN_TRY
		testErr = H5Iclear_type(H5I_FILE, 0);
	H5E_END_TRY

	VERIFY((testErr >= 0), 0, "H5Iclear_type");
	if(testErr >= 0)
		goto out;

	H5E_BEGIN_TRY
		testErr = H5Idestroy_type(H5I_DATASET);
	H5E_END_TRY

	VERIFY((testErr >= 0), 0, "H5Idestroy_type");
	if(testErr >= 0)
		goto out;

	/* Create a datatype ID and try to perform illegal functions on it */
	typeID = H5Tcreate(H5T_OPAQUE, (size_t)42);
	CHECK(typeID, H5I_INVALID_HID, "H5Tcreate");
	if(typeID == H5I_INVALID_HID)
		goto out;

	H5E_BEGIN_TRY
		testPtr = H5Iremove_verify(typeID, H5I_DATATYPE);
	H5E_END_TRY

	VERIFY(testPtr, NULL, "H5Iremove_verify");
	if(testPtr != NULL)
		goto out;

	H5E_BEGIN_TRY
		testPtr = H5Iobject_verify(typeID, H5I_DATATYPE);
	H5E_END_TRY

	VERIFY(testPtr, NULL, "H5Iobject_verify");
	if(testPtr != NULL)
		goto out;

	H5Tclose(typeID);

	/* testObj was never registered as an atom, so it will not be
         * automatically freed. */
	HDfree(testObj);
	return 0;

out:
	if(typeID != H5I_INVALID_HID)
		H5Tclose(typeID);
        if(testObj != NULL)
		HDfree(testObj);

	return -1;
}


/* Test the H5Iis_valid function */
static int test_is_valid(void)
{
    hid_t   dtype;      /* datatype id */
    int64_t nmembs1;    /* number of type memnbers */
    int64_t nmembs2;
    htri_t  tri_ret;    /* htri_t return value */
    herr_t  ret;        /* return value */

    /* Create a datatype id */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    if (dtype < 0)
        goto out;

    /* Check that the ID is valid */
    tri_ret = H5Iis_valid(dtype);
    VERIFY(tri_ret, TRUE, "H5Iis_valid");
    if (tri_ret != TRUE)
        goto out;

    /* Artificially manipulate the reference counts so app_count is 0, and dtype
     * appears to be an internal id.  This takes advantage of the fact that
     * H5Ipkg is included.
     */
    ret = H5I_inc_ref(dtype, FALSE);
    CHECK(ret, FAIL, "H5I_inc_ref");
    if (ret < 0)
        goto out;
    ret = H5I_dec_app_ref(dtype);
    CHECK(ret, FAIL, "H5I_dec_ref");
    if (ret < 0)
        goto out;

    /* Check that dtype is invalid */
    tri_ret = H5Iis_valid(dtype);
    VERIFY(tri_ret, FALSE, "H5Iis_valid");
    if (tri_ret != FALSE)
        goto out;

    /* Close dtype and verify that it has been closed */
    nmembs1 = H5I_nmembers(H5I_DATATYPE);
    CHECK(nmembs1, FAIL, "H5I_nmembers");
    if (nmembs1 < 0)
        goto out;
    ret = H5I_dec_ref(dtype);
    CHECK(ret, FAIL, "H5I_dec_ref");
    if (ret < 0)
        goto out;
    nmembs2 = H5I_nmembers(H5I_DATATYPE);
    VERIFY(nmembs2, nmembs1 - 1, "H5I_nmembers");
    if (nmembs2 != nmembs1 - 1)
        goto out;

    /* Check that dtype is invalid */
    tri_ret = H5Iis_valid(dtype);
    VERIFY(tri_ret, FALSE, "H5Iis_valid");
    if (tri_ret != FALSE)
        goto out;

    /* Check that an id of -1 is invalid */
    tri_ret = H5Iis_valid((hid_t)-1);
    VERIFY(tri_ret, FALSE, "H4Iis_valid");
    if (tri_ret != FALSE)
        goto out;

    return 0;

out:
    /* Don't attempt to close dtype as we don't know the exact state of the
     * reference counts.  Every state in this function will be automatically
     * closed at library exit anyways, as internal count is never > 1.
     */
    return -1;
}

/* Test the H5Iget_type function */
static int test_get_type(void)
{
    hid_t   dtype;           /* datatype id */
    H5I_type_t  type_ret;    /* return value */

    /* Create a datatype id */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    if (dtype < 0)
        goto out;

    /* Check that the ID is correct */
    type_ret = H5Iget_type(dtype);
    VERIFY(type_ret, H5I_DATATYPE, "H5Iget_type");
    if (type_ret == H5I_BADID)
        goto out;

    /* Check that the ID is correct */
    type_ret = H5Iget_type((hid_t)H5T_STRING);
    VERIFY(type_ret, H5I_BADID, "H5Iget_type");
    if (type_ret != H5I_BADID)
        goto out;

    /* Check that the ID is correct */
    type_ret = H5Iget_type((hid_t)-1);
    VERIFY(type_ret, H5I_BADID, "H5Iget_type");
    if (type_ret != H5I_BADID)
        goto out;

    H5Tclose(dtype);

    return 0;

out:
    if(dtype != H5I_INVALID_HID)
	H5Tclose(dtype);

    return -1;
}

	/* Test boundary cases with lots of types */

/* Type IDs range from H5I_NTYPES to H5I_MAX_NUM_TYPES.  The system will assign */
/* IDs in sequential order until H5I_MAX_NUM_TYPES IDs have been given out, at which */
/* point it will search for type IDs that were allocated but have since been */
/* deleted. */
/* This test will allocate IDs up to H5I_MAX_NUM_TYPES, ensure that IDs wrap around */
/* to low values successfully, ensure that an error is thrown when all possible */
/* type IDs are taken, then ensure that deleting types frees up their IDs. */
/* Note that this test depends on the implementation of IDs, so may break */
/*		if the implementation changes. */
/* Also note that if someone else registered a user-defined type and forgot to */
/* destroy it, this test will mysteriously fail (because it will expect there to */
/* be one more "free" type ID than there is). */
/* H5I_NTYPES is defined in h5public.h, H5I_MAX_NUM_TYPES is defined in h5pkg.h */
static int test_id_type_list(void)
{
	H5I_type_t startType;	/* The first type ID we were assigned in this test */
	H5I_type_t currentType;
	H5I_type_t testType;
	int i;	/* Just a counter variable */

	startType = H5Iregister_type((size_t)8, 0, (H5I_free_t) free );
	CHECK(startType, H5I_BADID, "H5Iregister_type");
	if(startType == H5I_BADID)
		goto out;

	/* Sanity check */
	if(startType >= H5I_MAX_NUM_TYPES || startType < H5I_NTYPES)
	{
		/* Error condition, throw an error */
		CHECK(1, 1, "H5Iregister_type");
		goto out;
	}
	/* Create types up to H5I_MAX_NUM_TYPES */
	for(i = startType + 1; i < H5I_MAX_NUM_TYPES; i++)
	{
		currentType = H5Iregister_type((size_t)8, 0, (H5I_free_t) free );
		CHECK(currentType, H5I_BADID, "H5Iregister_type");
		if(currentType == H5I_BADID)
			goto out;
	}

	/* Wrap around to low type ID numbers */
	for(i = H5I_NTYPES; i < startType; i++)
	{
		currentType = H5Iregister_type((size_t)8, 0, (H5I_free_t) free );
		CHECK(currentType, H5I_BADID, "H5Iregister_type");
		if(currentType == H5I_BADID)
			goto out;
	}

	/* There should be no room at the inn for a new ID type*/
	H5E_BEGIN_TRY
		testType = H5Iregister_type((size_t)8, 0, (H5I_free_t) free );
	H5E_END_TRY

	VERIFY(testType, H5I_BADID, "H5Iregister_type");
	if(testType != H5I_BADID)
		goto out;

	/* Now delete a type and try to insert again */
	H5Idestroy_type(H5I_NTYPES);
	testType = H5Iregister_type((size_t)8, 0, (H5I_free_t) free );

	VERIFY(testType, H5I_NTYPES, "H5Iregister_type");
	if(testType != H5I_NTYPES)
		goto out;

	/* Cleanup.  Destroy all types. */
	for(i = H5I_NTYPES; i < H5I_MAX_NUM_TYPES; i++)
		H5Idestroy_type((H5I_type_t) i);

	return 0;

out:
    /* Cleanup.  For simplicity, just destroy all types and ignore errors. */
	H5E_BEGIN_TRY
		for(i = H5I_NTYPES; i < H5I_MAX_NUM_TYPES; i++)
			H5Idestroy_type((H5I_type_t) i);
	H5E_END_TRY
	return -1;
}

    /* Test removing ids in callback for H5Iclear_type */

/* There was a rare bug where, if an id free callback being called by
 * H5I_clear_type() removed another id in that type, a segfault could occur.
 * This test tests for that error (and freeing ids "out of order" within
 * H5Iclear_type() in general). */
/* Macro definitions */
#define TEST_RCT_MAX_NOBJS 25
#define TEST_RCT_MIN_NOBJS 5
#define TEST_RCT_NITER 50

/* Structure to hold the list of objects */
typedef struct {
    struct test_rct_obj_t *list; /* List of objects */
    long nobjs;                 /* Number of objects in list */
    long nobjs_rem;             /* Number of objects in list that have not been freed */
} test_rct_list_t;

/* Structure for an object */
typedef struct test_rct_obj_t {
    hid_t id;                   /* ID for this object */
    int nfrees;                 /* Number of times this object has been freed */
    hbool_t freeing;            /* Whether we are currently freeing this object directly (through H5Idec_ref()) */
    test_rct_list_t *obj_list;  /* List of all objects */
} test_rct_obj_t;

/* Free callback */
static herr_t test_rct_free(void *_obj) {
    test_rct_obj_t *obj = (test_rct_obj_t *)_obj;
    long rem_idx, i;
    herr_t  ret;        /* return value */

    /* Mark this object as freed */
    obj->nfrees++;
    obj->obj_list->nobjs_rem--;

    /* Check freeing and nobjs_rem */
    if(!obj->freeing && (obj->obj_list->nobjs_rem > 0)) {
        /* Remove a random object from the list */
        rem_idx = HDrandom() % obj->obj_list->nobjs_rem;

        /* Scan the list, finding the rem_idx'th object that has not been
         * freed */
        for(i = 0; i < obj->obj_list->nobjs; i++)
            if(obj->obj_list->list[i].nfrees == 0) {
                if(rem_idx == 0)
                    break;
                else
                    rem_idx--;
            } /* end if */
        if(i == obj->obj_list->nobjs) {
            ERROR("invalid obj_list");
            goto out;
        } /* end if */
        else {
            /* Remove the object.  Mark as "freeing" so its own callback does
             * not free another object. */
            obj->obj_list->list[i].freeing = TRUE;
            ret = H5Idec_ref(obj->obj_list->list[i].id);
            CHECK(ret, FAIL, "H5Idec_ref");
            if(ret == FAIL)
                goto out;
            obj->obj_list->list[i].freeing = FALSE;
        } /* end else */
    } /* end if */

    /* Verify nobjs_rem is non-negative */
    if(obj->obj_list->nobjs_rem < 0) {
        ERROR("invalid nobjs_rem");
        goto out;
    } /* end if */

    return 0;

out:
    return -1;
} /* end test_rct_free() */

/* Test function */
static int test_remove_clear_type(void)
{
    H5I_type_t obj_type;
    test_rct_list_t obj_list;
    test_rct_obj_t list[TEST_RCT_MAX_NOBJS];
    long i, j;
    long nobjs_found;
    hsize_t nmembers;
    herr_t  ret;        /* return value */

    /* Register type */
    obj_type = H5Iregister_type((size_t)8, 0, test_rct_free);
    CHECK(obj_type, H5I_BADID, "H5Iregister_type");
    if(obj_type == H5I_BADID)
        goto out;

    /* Init obj_list.list */
    obj_list.list = list;

    for(i = 0; i < TEST_RCT_NITER; i++) {
        /* Build object list */
        obj_list.nobjs = obj_list.nobjs_rem = TEST_RCT_MIN_NOBJS + (HDrandom() % (long)(TEST_RCT_MAX_NOBJS - TEST_RCT_MIN_NOBJS + 1));
        for(j = 0; j < obj_list.nobjs; j++) {
            list[j].nfrees = 0;
            list[j].freeing = FALSE;
            list[j].obj_list = &obj_list;
            list[j].id = H5Iregister(obj_type, &list[j]);
            CHECK(list[j].id, FAIL, "H5Iregister");
            if(list[j].id == FAIL)
                goto out;
            if(HDrandom() % 2) {
                ret = H5Iinc_ref(list[j].id);
                CHECK(ret, FAIL, "H5Iinc_ref");
                if(ret == FAIL)
                    goto out;
            } /* end if */
        } /* end for */

        /* Clear the type */
        ret = H5Iclear_type(obj_type, FALSE);
        CHECK(ret, FAIL, "H5Iclear_type");
        if(ret == FAIL)
            goto out;

        /* Verify list */
        nobjs_found = 0;
        for(j = 0; j < obj_list.nobjs; j++) {
            if(list[j].nfrees == 0)
                nobjs_found++;
            else {
                VERIFY(list[j].nfrees, (long)1, "list[j].nfrees");
                if(list[j].nfrees != (long)1)
                    goto out;
            } /* end else */
            VERIFY(list[j].freeing, FALSE, "list[j].freeing");
            if(list[j].freeing != FALSE)
                goto out;
        } /* end for */

        /* Verify number of objects */
        VERIFY(obj_list.nobjs_rem, nobjs_found, "obj_list.nobjs_rem");
        if(obj_list.nobjs_rem != nobjs_found)
            goto out;
        ret = H5Inmembers(obj_type, &nmembers);
        CHECK(ret, FAIL, "H5Inmembers");
        if(ret == FAIL)
            goto out;
        VERIFY(nmembers, (size_t)nobjs_found, "H5Inmembers");
        if(nmembers != (size_t)nobjs_found)
            goto out;

        /* Clear the type with force set to TRUE */
        ret = H5Iclear_type(obj_type, TRUE);
        CHECK(ret, FAIL, "H5Iclear_type");
        if(ret == FAIL)
            goto out;

        /* Verify list */
        for(j = 0; j < obj_list.nobjs; j++) {
            VERIFY(list[j].nfrees, (long)1, "list[j].nfrees");
            if(list[j].nfrees != (long)1)
                goto out;
            VERIFY(list[j].freeing, FALSE, "list[j].freeing");
            if(list[j].freeing != FALSE)
                goto out;
        } /* end for */

        /* Verify number of objects is 0 */
        VERIFY(obj_list.nobjs_rem, (long)0, "obj_list.nobjs_rem");
        if(obj_list.nobjs_rem != (long)0)
            goto out;
        ret = H5Inmembers(obj_type, &nmembers);
        CHECK(ret, FAIL, "H5Inmembers");
        if(ret == FAIL)
            goto out;
        VERIFY(nmembers, (size_t)0, "H5Inmembers");
        if(nmembers != (size_t)0)
            goto out;
    } /* end for */

    /* Destroy type */
    ret = H5Idestroy_type(obj_type);
    CHECK(ret, FAIL, "H5Idestroy_type");
    if(ret == FAIL)
        goto out;

    return 0;

out:
    /* Cleanup.  For simplicity, just destroy the types and ignore errors. */
    H5E_BEGIN_TRY
        H5Idestroy_type(obj_type);
    H5E_END_TRY
    return -1;
} /* end test_remove_clear_type() */

void test_ids(void)
{
    /* Set the random # seed */
    HDsrandom((unsigned)HDtime(NULL));

	if (basic_id_test() < 0) TestErrPrintf("Basic ID test failed\n");
	if (id_predefined_test() < 0) TestErrPrintf("Predefined ID type test failed\n");
	if (test_is_valid() < 0) TestErrPrintf("H5Iis_valid test failed\n");
	if (test_get_type() < 0) TestErrPrintf("H5Iget_type test failed\n");
	if (test_id_type_list() < 0) TestErrPrintf("ID type list test failed\n");
	if (test_remove_clear_type() < 0) TestErrPrintf("ID remove during H5Iclear_type test failed\n");

}
