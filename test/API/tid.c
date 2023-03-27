/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Test user-created identifiers (hid_t's) and identifier types. */

#include "testhdf5.h"

#if 0
/* Include H5Ipkg.h to calculate max number of groups */
#define H5I_FRIEND /*suppress error about including H5Ipkg      */
#include "H5Ipkg.h"
#endif

/*
 * Number of bits to use for ID Type in each ID. Increase if more types
 * are needed (though this will decrease the number of available IDs per
 * type). This is the only number that must be changed since all other bit
 * field sizes and masks are calculated from TYPE_BITS.
 */
#define TYPE_BITS 7
#define TYPE_MASK (((hid_t)1 << TYPE_BITS) - 1)

#define H5I_MAX_NUM_TYPES TYPE_MASK

static herr_t
free_wrapper(void *p, void H5_ATTR_UNUSED **_ctx)
{
    HDfree(p);
    return SUCCEED;
}

/* Test basic functionality of registering and deleting types and IDs */
static int
basic_id_test(void)
{
    H5I_type_t myType  = H5I_BADID;
    hid_t      arrayID = H5I_INVALID_HID;
    void      *testObj = NULL;
    void      *testPtr = NULL;
    char       nameString[10];
    hid_t      testID;
    ssize_t    testSize = -1;
    herr_t     err;
    int        num_ref;
    hsize_t    num_members;

    /* Try to register an ID with fictitious types */
    H5E_BEGIN_TRY
    arrayID = H5Iregister((H5I_type_t)420, testObj);
    H5E_END_TRY

    VERIFY(arrayID, H5I_INVALID_HID, "H5Iregister");
    if (arrayID != H5I_INVALID_HID)
        goto out;

    H5E_BEGIN_TRY
    arrayID = H5Iregister((H5I_type_t)-1, testObj);
    H5E_END_TRY

    VERIFY(arrayID, H5I_INVALID_HID, "H5Iregister");
    if (arrayID != H5I_INVALID_HID)
        goto out;

    /* Try to access IDs with fictitious types */
    H5E_BEGIN_TRY
    testPtr = H5Iobject_verify((hid_t)100, (H5I_type_t)0);
    H5E_END_TRY

    CHECK_PTR_NULL(testPtr, "H5Iobject_verify");
    if (testPtr != NULL)
        goto out;

    H5E_BEGIN_TRY
    testPtr = H5Iobject_verify((hid_t)700, (H5I_type_t)700);
    H5E_END_TRY

    CHECK_PTR_NULL(testPtr, "H5Iobject_verify");
    if (testPtr != NULL)
        goto out;

    /* Register a type */
    myType = H5Iregister_type((size_t)64, 0, free_wrapper);

    CHECK(myType, H5I_BADID, "H5Iregister_type");
    if (myType == H5I_BADID)
        goto out;

    /* Register an ID and retrieve the object it points to.
     * Once the ID has been registered, testObj will be freed when
     * its ID type is destroyed.
     */
    testObj = HDmalloc(7 * sizeof(int));
    arrayID = H5Iregister(myType, testObj);

    CHECK(arrayID, H5I_INVALID_HID, "H5Iregister");
    if (arrayID == H5I_INVALID_HID) {
        HDfree(testObj);
        goto out;
    }

    testPtr = (int *)H5Iobject_verify(arrayID, myType);

    CHECK_PTR_EQ(testPtr, testObj, "H5Iobject_verify");
    if (testPtr != testObj)
        goto out;

    /* Ensure that H5Iget_file_id and H5Iget_name() fail, since this
     * is an hid_t for the wrong kind of object
     */
    H5E_BEGIN_TRY
    testID = H5Iget_file_id(arrayID);
    H5E_END_TRY

    VERIFY(testID, H5I_INVALID_HID, "H5Iget_file_id");
    if (testID != H5I_INVALID_HID)
        goto out;

    H5E_BEGIN_TRY
    testSize = H5Iget_name(arrayID, nameString, (size_t)9);
    H5E_END_TRY

    VERIFY(testSize, -1, "H5Iget_name");
    if (testSize != -1)
        goto out;

    /* Make sure H5Iremove_verify catches objects of the wrong type */
    H5E_BEGIN_TRY
    testPtr = (int *)H5Iremove_verify(arrayID, (H5I_type_t)0);
    H5E_END_TRY

    CHECK_PTR_NULL(testPtr, "H5Iremove_verify");
    if (testPtr != NULL)
        goto out;

    H5E_BEGIN_TRY
    testPtr = (int *)H5Iremove_verify(arrayID, (H5I_type_t)((int)myType - 1));
    H5E_END_TRY

    CHECK_PTR_NULL(testPtr, "H5Iremove_verify");
    if (testPtr != NULL)
        goto out;

    /* Remove an ID and make sure we can't access it */
    testPtr = (int *)H5Iremove_verify(arrayID, myType);

    CHECK_PTR(testPtr, "H5Iremove_verify");
    if (testPtr == NULL)
        goto out;

    H5E_BEGIN_TRY
    testPtr = (int *)H5Iobject_verify(arrayID, myType);
    H5E_END_TRY

    CHECK_PTR_NULL(testPtr, "H5Iobject_verify");
    if (testPtr != NULL)
        goto out;

    /* Delete the type and make sure we can't access objects within it */
    arrayID = H5Iregister(myType, testObj);

    err = H5Idestroy_type(myType);
    VERIFY(err, 0, "H5Idestroy_type");
    if (err != 0)
        goto out;
    VERIFY(H5Itype_exists(myType), 0, "H5Itype_exists");
    if (H5Itype_exists(myType) != 0)
        goto out;

    H5E_BEGIN_TRY
    VERIFY(H5Inmembers(myType, NULL), -1, "H5Inmembers");
    if (H5Inmembers(myType, NULL) != -1)
        goto out;
    H5E_END_TRY

    /* Register another type and another object in that type */
    myType = H5Iregister_type((size_t)64, 0, free_wrapper);

    CHECK(myType, H5I_BADID, "H5Iregister_type");
    if (myType == H5I_BADID)
        goto out;

    /* The memory that testObj pointed to should already have been
     * freed when the previous type was destroyed.  Allocate new
     * memory for it.
     */
    testObj = HDmalloc(7 * sizeof(int));
    arrayID = H5Iregister(myType, testObj);

    CHECK(arrayID, H5I_INVALID_HID, "H5Iregister");
    if (arrayID == H5I_INVALID_HID) {
        HDfree(testObj);
        goto out;
    }

    err = H5Inmembers(myType, &num_members);
    CHECK(err, -1, "H5Inmembers");
    if (err < 0)
        goto out;
    VERIFY(num_members, 1, "H5Inmembers");
    if (num_members != 1)
        goto out;

    /* Increment references to type and ensure that dec_type_ref
     * doesn't destroy the type
     */
    num_ref = H5Iinc_type_ref(myType);
    VERIFY(num_ref, 2, "H5Iinc_type_ref");
    if (num_ref != 2)
        goto out;
    num_ref = H5Idec_type_ref(myType);
    VERIFY(num_ref, 1, "H5Idec_type_ref");
    if (num_ref != 1)
        goto out;
    err = H5Inmembers(myType, &num_members);
    CHECK(err, -1, "H5Inmembers");
    if (err < 0)
        goto out;
    VERIFY(num_members, 1, "H5Inmembers");
    if (num_members != 1)
        goto out;

    /* This call to dec_type_ref should destroy the type */
    num_ref = H5Idec_type_ref(myType);
    VERIFY(num_ref, 0, "H5Idec_type_ref");
    if (num_ref != 0)
        goto out;
    VERIFY(H5Itype_exists(myType), 0, "H5Itype_exists");
    if (H5Itype_exists(myType) != 0)
        goto out;

    H5E_BEGIN_TRY
    err = H5Inmembers(myType, &num_members);
    if (err >= 0)
        goto out;
    H5E_END_TRY

    return 0;

out:
    /* Clean up type if it has been allocated and free memory used
     * by testObj
     */
    if (myType >= 0)
        H5Idestroy_type(myType);

    return -1;
}

/* A dummy search function for the next test */
static int
test_search_func(void H5_ATTR_UNUSED *ptr1, hid_t H5_ATTR_UNUSED id, void H5_ATTR_UNUSED *ptr2)
{
    return 0;
}

/* Ensure that public functions cannot access "predefined" ID types */
static int
id_predefined_test(void)
{
    void  *testObj;
    hid_t  testID;
    hid_t  typeID = H5I_INVALID_HID;
    void  *testPtr;
    herr_t testErr;

    testObj = HDmalloc(sizeof(int));

    /*
     * Attempt to perform public functions on various library types
     */

    H5E_BEGIN_TRY
    testID = H5Iregister(H5I_FILE, testObj);
    H5E_END_TRY

    VERIFY(testID, H5I_INVALID_HID, "H5Iregister");
    if (testID != H5I_INVALID_HID)
        goto out;

    H5E_BEGIN_TRY
    testPtr = H5Isearch(H5I_GENPROP_LST, test_search_func, testObj);
    H5E_END_TRY

    CHECK_PTR_NULL(testPtr, "H5Isearch");
    if (testPtr != NULL)
        goto out;

    H5E_BEGIN_TRY
    testErr = H5Inmembers(H5I_ERROR_STACK, NULL);
    H5E_END_TRY

    VERIFY(testErr, -1, "H5Inmembers");
    if (testErr != -1)
        goto out;

    H5E_BEGIN_TRY
    testErr = H5Iclear_type(H5I_FILE, 0);
    H5E_END_TRY

    VERIFY((testErr >= 0), 0, "H5Iclear_type");
    if (testErr >= 0)
        goto out;

    H5E_BEGIN_TRY
    testErr = H5Idestroy_type(H5I_DATASET);
    H5E_END_TRY

    VERIFY((testErr >= 0), 0, "H5Idestroy_type");
    if (testErr >= 0)
        goto out;

    H5E_BEGIN_TRY
    testErr = H5Itype_exists(H5I_GROUP);
    H5E_END_TRY

    VERIFY(testErr, -1, "H5Itype_exists");
    if (testErr != -1)
        goto out;

    H5E_BEGIN_TRY
    testErr = H5Itype_exists(H5I_ATTR);
    H5E_END_TRY

    VERIFY(testErr, -1, "H5Itype_exists");
    if (testErr != -1)
        goto out;

    /*
     * Create a datatype ID and try to perform illegal functions on it
     */

    typeID = H5Tcreate(H5T_OPAQUE, (size_t)42);
    CHECK(typeID, H5I_INVALID_HID, "H5Tcreate");
    if (typeID == H5I_INVALID_HID)
        goto out;

    H5E_BEGIN_TRY
    testPtr = H5Iremove_verify(typeID, H5I_DATATYPE);
    H5E_END_TRY

    CHECK_PTR_NULL(testPtr, "H5Iremove_verify");
    if (testPtr != NULL)
        goto out;

    H5E_BEGIN_TRY
    testPtr = H5Iobject_verify(typeID, H5I_DATATYPE);
    H5E_END_TRY

    CHECK_PTR_NULL(testPtr, "H5Iobject_verify");
    if (testPtr != NULL)
        goto out;

    H5Tclose(typeID);

    /* testObj was never registered as an atom, so it will not be
     * automatically freed. */
    HDfree(testObj);
    return 0;

out:
    if (typeID != H5I_INVALID_HID)
        H5Tclose(typeID);
    if (testObj != NULL)
        HDfree(testObj);

    return -1;
}

/* Test the H5Iis_valid function */
static int
test_is_valid(void)
{
    hid_t dtype; /* datatype id */
#if 0
    int64_t nmembs1; /* number of type memnbers */
    int64_t nmembs2;
#endif
    htri_t tri_ret; /* htri_t return value */
#if 0
    herr_t  ret;     /* return value */
#endif

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
#if 0 /* Cannot call internal APIs and cannot call public H5Inmembers on library types */
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
#endif
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
static int
test_get_type(void)
{
    hid_t      dtype;    /* datatype id */
    H5I_type_t type_ret; /* return value */

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
    if (dtype != H5I_INVALID_HID)
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
/*        if the implementation changes. */
/* Also note that if someone else registered a user-defined type and forgot to */
/* destroy it, this test will mysteriously fail (because it will expect there to */
/* be one more "free" type ID than there is). */
/* H5I_NTYPES is defined in h5public.h, H5I_MAX_NUM_TYPES is defined in h5pkg.h */
static int
test_id_type_list(void)
{
    H5I_type_t startType; /* The first type ID we were assigned in this test */
    H5I_type_t currentType;
    H5I_type_t testType;
    int        i; /* Just a counter variable */

    startType = H5Iregister_type((size_t)8, 0, free_wrapper);
    CHECK(startType, H5I_BADID, "H5Iregister_type");
    if (startType == H5I_BADID)
        goto out;

    /* Sanity check */
    if ((int)startType >= H5I_MAX_NUM_TYPES || startType < H5I_NTYPES) {
        /* Error condition, throw an error */
        ERROR("H5Iregister_type");
        goto out;
    }
    /* Create types up to H5I_MAX_NUM_TYPES */
    for (i = startType + 1; i < H5I_MAX_NUM_TYPES; i++) {
        currentType = H5Iregister_type((size_t)8, 0, free_wrapper);
        CHECK(currentType, H5I_BADID, "H5Iregister_type");
        if (currentType == H5I_BADID)
            goto out;
    }

    /* Wrap around to low type ID numbers */
    for (i = H5I_NTYPES; i < startType; i++) {
        currentType = H5Iregister_type((size_t)8, 0, free_wrapper);
        CHECK(currentType, H5I_BADID, "H5Iregister_type");
        if (currentType == H5I_BADID)
            goto out;
    }

    /* There should be no room at the inn for a new ID type*/
    H5E_BEGIN_TRY
    testType = H5Iregister_type((size_t)8, 0, free_wrapper);
    H5E_END_TRY

    VERIFY(testType, H5I_BADID, "H5Iregister_type");
    if (testType != H5I_BADID)
        goto out;

    /* Now delete a type and try to insert again */
    H5Idestroy_type(H5I_NTYPES);
    testType = H5Iregister_type((size_t)8, 0, free_wrapper);

    VERIFY(testType, H5I_NTYPES, "H5Iregister_type");
    if (testType != H5I_NTYPES)
        goto out;

    /* Cleanup.  Destroy all types. */
    for (i = H5I_NTYPES; i < H5I_MAX_NUM_TYPES; i++)
        H5Idestroy_type((H5I_type_t)i);

    return 0;

out:
    /* Cleanup.  For simplicity, just destroy all types and ignore errors. */
    H5E_BEGIN_TRY
    for (i = H5I_NTYPES; i < H5I_MAX_NUM_TYPES; i++)
        H5Idestroy_type((H5I_type_t)i);
    H5E_END_TRY
    return -1;
}

/* Test removing ids in callback for H5Iclear_type */

/* There was a rare bug where, if an id free callback being called by
 * H5I_clear_type() removed another id in that type, a segfault could occur.
 * This test tests for that error (and freeing ids "out of order" within
 * H5Iclear_type() in general).
 *
 * NB: RCT = "remove clear type"
 */

/* Macro definitions */
#define RCT_MAX_NOBJS 25 /* Maximum number of objects in the list */
#define RCT_MIN_NOBJS 5
#define RCT_NITER     50 /* Number of times we cycle through object creation and deletion */

/* Structure to hold the master list of objects */
typedef struct rct_obj_list_t {

    /* Pointer to the objects */
    struct rct_obj_t *objects;

    /* The number of objects in the list */
    long count;

    /* The number of objects in the list that have not been freed */
    long remaining;
} rct_obj_list_t;

/* Structure for an object */
typedef struct rct_obj_t {
    /* The ID for this object */
    hid_t id;

    /* The number of times this object has been freed */
    int nfrees;

    /* Whether we are currently freeing this object directly
     * through H5Idec_ref().
     */
    hbool_t freeing;

    /* Pointer to the master list of all objects */
    rct_obj_list_t *list;
} rct_obj_t;

/* Free callback passed to H5Iclear_type()
 *
 * When invoked on a closing object, frees a random unfreed ID in the
 * master list of objects.
 */
static herr_t
rct_free_cb(void *_obj, void H5_ATTR_UNUSED **_ctx)
{
    rct_obj_t *obj = (rct_obj_t *)_obj;
    long       remove_nth;
    long       i;
    herr_t     ret;

    /* Mark this object as freed */
    obj->nfrees++;

    /* Decrement the number of objects in the list that have not been freed */
    obj->list->remaining--;

    /* If this object isn't already being freed by a callback free call and
     * the master object list still contains objects to free, pick another
     * object and free it.
     */
    if (!obj->freeing && (obj->list->remaining > 0)) {

        /* Pick a random object from the list. This is done by picking a
         * random number between 0 and the # of remaining unfreed objects
         * and then scanning through the list to find that nth unfreed
         * object.
         */
        remove_nth = HDrandom() % obj->list->remaining;
        for (i = 0; i < obj->list->count; i++)
            if (obj->list->objects[i].nfrees == 0) {
                if (remove_nth == 0)
                    break;
                else
                    remove_nth--;
            }

        /* Badness if we scanned through the list and didn't manage to
         * select one to delete (the list stats were probably updated
         * incorrectly).
         */
        if (i == obj->list->count) {
            ERROR("invalid obj_list");
            goto error;
        }

        /* Mark the object we're about to free so its own callback does
         * not free another object. We don't want to recursively free the
         * entire list when we free the first ID.
         */
        obj->list->objects[i].freeing = TRUE;

        /* Decrement the reference count on the object */
        ret = H5Idec_ref(obj->list->objects[i].id);
        CHECK(ret, FAIL, "H5Idec_ref");
        if (ret == FAIL)
            goto error;

        /* Unset the "freeing" flag */
        obj->list->objects[i].freeing = FALSE;
    }

    /* Verify the number of objects remaining in the master list is non-negative */
    if (obj->list->remaining < 0) {
        ERROR("invalid number of objects remaining");
        goto error;
    }

    return 0;

error:
    return -1;
} /* end rct_free_cb() */

/* Test function */
static int
test_remove_clear_type(void)
{
    H5I_type_t     obj_type;
    rct_obj_list_t obj_list;
    rct_obj_t     *objects = NULL; /* Convenience pointer to objects stored in master list */
    size_t         list_size;
    long           i, j;
    herr_t         ret; /* return value */

    /* Register a user-defined type with our custom ID-deleting callback */
    obj_type = H5Iregister_type((size_t)8, 0, rct_free_cb);
    CHECK(obj_type, H5I_BADID, "H5Iregister_type");
    if (obj_type == H5I_BADID)
        goto error;

    /* Create an array to hold the objects in the master list */
    list_size        = RCT_MAX_NOBJS * sizeof(rct_obj_t);
    obj_list.objects = HDmalloc(list_size);
    CHECK_PTR(obj_list.objects, "HDcalloc");
    if (NULL == obj_list.objects)
        goto error;

    /* Set a convenience pointer to the object array */
    objects = obj_list.objects;

    for (i = 0; i < RCT_NITER; i++) {

        /* The number of members in the type, according to the HDF5 library */
        hsize_t nmembers = 1234567; /* (init to fake number) */

        /* The number of objects found while scanning through the object list */
        int found;

        /*********************
         * Build object list *
         *********************/

        HDmemset(obj_list.objects, 0, list_size);

        /* The number of objects used is a random number between the min and max */
        obj_list.count = obj_list.remaining =
            RCT_MIN_NOBJS + (HDrandom() % (long)(RCT_MAX_NOBJS - RCT_MIN_NOBJS + 1));

        /* Create the actual objects */
        for (j = 0; j < obj_list.count; j++) {

            /* Object setup */
            objects[j].nfrees  = 0;
            objects[j].freeing = FALSE;
            objects[j].list    = &obj_list;

            /* Register an ID for it */
            objects[j].id = H5Iregister(obj_type, &objects[j]);
            CHECK(objects[j].id, FAIL, "H5Iregister");
            if (objects[j].id == FAIL)
                goto error;

            /* Bump the reference count by 1 (to 2) 50% of the time */
            if (HDrandom() % 2) {
                ret = H5Iinc_ref(objects[j].id);
                CHECK(ret, FAIL, "H5Iinc_ref");
                if (ret == FAIL)
                    goto error;
            }
        }

        /******************************************
         * Clear the type with force set to FALSE *
         ******************************************/

        /* Clear the type. Since force is FALSE, only
         * IDs with a reference count of 1 will be cleared.
         */
        ret = H5Iclear_type(obj_type, FALSE);
        CHECK(ret, FAIL, "H5Iclear_type");
        if (ret == FAIL)
            goto error;

        /* Verify that the object struct fields are sane and count the
         * number of unfreed objects
         */
        found = 0;
        for (j = 0; j < obj_list.count; j++) {

            if (objects[j].nfrees == 0) {
                /* Count unfreed objects */
                found++;
            }
            else {
                /* Every freed object should have been freed exactly once */
                VERIFY(objects[j].nfrees, 1, "object freed more than once");
                if (objects[j].nfrees != 1)
                    goto error;
            }

            /* No object should still be marked as "freeing" */
            VERIFY(objects[j].freeing, FALSE, "object marked as freeing");
            if (objects[j].freeing != FALSE)
                goto error;
        }

        /* Verify the number of unfreed objects we found during our scan
         * matches the number stored in the list
         */
        VERIFY(obj_list.remaining, found, "incorrect number of objects remaining");
        if (obj_list.remaining != found)
            goto error;

        /* Make sure the HDF5 library confirms our count */
        ret = H5Inmembers(obj_type, &nmembers);
        CHECK(ret, FAIL, "H5Inmembers");
        if (ret == FAIL)
            goto error;
        VERIFY(nmembers, found, "The number of members remaining in the type did not match our count");
        if (nmembers != (hsize_t)found)
            goto error;

        /*****************************************
         * Clear the type with force set to TRUE *
         *****************************************/

        /* Clear the type. Since force is TRUE, all IDs will be cleared. */
        ret = H5Iclear_type(obj_type, TRUE);
        CHECK(ret, FAIL, "H5Iclear_type");
        if (ret == FAIL)
            goto error;

        /* Verify that the object struct fields are sane */
        for (j = 0; j < obj_list.count; j++) {

            /* Every object should have been freed exactly once */
            VERIFY(objects[j].nfrees, 1, "object freed more than once");
            if (objects[j].nfrees != 1)
                goto error;

            /* No object should still be marked as "freeing" */
            VERIFY(objects[j].freeing, FALSE, "object marked as freeing");
            if (objects[j].freeing != FALSE)
                goto error;
        }

        /* Verify the number of objects is 0 */
        VERIFY(obj_list.remaining, 0, "objects remaining was not zero");
        if (obj_list.remaining != 0)
            goto error;

        /* Make sure the HDF5 library confirms zero members in the type */
        ret = H5Inmembers(obj_type, &nmembers);
        CHECK(ret, FAIL, "H5Inmembers");
        if (ret == FAIL)
            goto error;
        VERIFY(nmembers, 0, "The number of members remaining in the type was not zero");
        if (nmembers != 0)
            goto error;
    }

    /* Destroy the type */
    ret = H5Idestroy_type(obj_type);
    CHECK(ret, FAIL, "H5Idestroy_type");
    if (ret == FAIL)
        goto error;

    /* Free the object array */
    HDfree(obj_list.objects);

    return 0;

error:
    /* Cleanup. For simplicity, just destroy the types and ignore errors. */
    H5E_BEGIN_TRY
    {
        H5Idestroy_type(obj_type);
    }
    H5E_END_TRY

    HDfree(obj_list.objects);

    return -1;
} /* end test_remove_clear_type() */

#if defined(H5VL_VERSION) && H5VL_VERSION >= 2
/* Typedef for future objects */
typedef struct {
    H5I_type_t obj_type; /* ID type for actual object */
} future_obj_t;

/* Global (static) future ID object type */
H5I_type_t future_obj_type_g = H5I_BADID;

/* Callback to free the actual object for future object test */
static herr_t
free_actual_object(void *_p, void H5_ATTR_UNUSED **_ctx)
{
    int *p = (int *)_p;

    if (7 != *p)
        return FAIL;

    HDfree(p);

    return SUCCEED;
}

/* Callback to realize a future object */
static herr_t
realize_future_cb(void *_future_obj, hid_t *actual_id)
{
    future_obj_t *future_obj = (future_obj_t *)_future_obj; /* Future object */
    int          *actual_obj;                               /* Pointer to the actual object */

    /* Check for bad future object */
    if (NULL == future_obj)
        return FAIL;

    /* Determine type of object to realize */
    if (H5I_DATASPACE == future_obj->obj_type) {
        hsize_t dims = 13;

        if ((*actual_id = H5Screate_simple(1, &dims, NULL)) < 0)
            return FAIL;
    }
    else if (H5I_DATATYPE == future_obj->obj_type) {
        if ((*actual_id = H5Tcopy(H5T_NATIVE_INT)) < 0)
            return FAIL;
    }
    else if (H5I_GENPROP_LST == future_obj->obj_type) {
        if ((*actual_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
            return FAIL;
    }
    else {
        /* Create a new object (the 'actual object') of the correct type */
        if (NULL == (actual_obj = HDmalloc(sizeof(int))))
            return FAIL;
        *actual_obj = 7;

        /* Register actual object of the user-defined type */
        *actual_id = H5Iregister(future_obj->obj_type, actual_obj);
        CHECK(*actual_id, FAIL, "H5Iregister");
        if (*actual_id == FAIL)
            return FAIL;
    }

    return SUCCEED;
}

/* Callback to discard a future object */
static herr_t
discard_future_cb(void *future_obj)
{
    if (NULL == future_obj)
        return FAIL;

    HDfree(future_obj);

    return SUCCEED;
}

/* Callback to realize a future object when future objects are NULL*/
static herr_t
realize_future_generate_cb(void *_future_obj, hid_t *actual_id)
{
    future_obj_t *future_obj = (future_obj_t *)_future_obj; /* Future object */
    int          *actual_obj;                               /* Pointer to the actual object */

    if (NULL != future_obj)
        return FAIL;
    /* Create a new object (the 'actual object') of the correct type */
    if (NULL == (actual_obj = HDmalloc(sizeof(int))))
        return FAIL;
    *actual_obj = 7;

    /* Register actual object without using future object info */
    *actual_id = H5Iregister(future_obj_type_g, actual_obj);
    CHECK(*actual_id, FAIL, "H5Iregister");
    if (*actual_id == FAIL)
        return FAIL;

    return SUCCEED;
}

/* Callback to discard a future object when future objects are NULL */
static herr_t
discard_future_generate_cb(void *future_obj)
{
    if (NULL != future_obj)
        return FAIL;

    return SUCCEED;
}

/* Test function */
static int
test_future_ids(void)
{
    H5I_type_t    obj_type;        /* New user-defined ID type */
    hid_t         future_id;       /* ID for future object */
    int           fake_future_obj; /* "Fake" future object for tests */
    future_obj_t *future_obj;      /* Future object */
    int          *actual_obj;      /* Actual object */
    int          *actual_obj2;     /* Another actual object */
    H5I_type_t    id_type;         /* Type of ID */
    H5T_class_t   type_class;      /* Datatype class */
    herr_t        ret;             /* Return value */

    /* Register a user-defined type with our custom ID-deleting callback */
    obj_type = H5Iregister_type((size_t)15, 0, free_actual_object);
    CHECK(obj_type, H5I_BADID, "H5Iregister_type");
    if (H5I_BADID == obj_type)
        goto error;

    /* Test basic error conditions */
    fake_future_obj = 0;
    H5E_BEGIN_TRY
    {
        future_id = H5Iregister_future(obj_type, &fake_future_obj, NULL, NULL);
    }
    H5E_END_TRY
    VERIFY(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID != future_id)
        goto error;

    H5E_BEGIN_TRY
    {
        future_id = H5Iregister_future(obj_type, &fake_future_obj, realize_future_cb, NULL);
    }
    H5E_END_TRY
    VERIFY(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID != future_id)
        goto error;

    H5E_BEGIN_TRY
    {
        future_id = H5Iregister_future(obj_type, &fake_future_obj, NULL, discard_future_cb);
    }
    H5E_END_TRY
    VERIFY(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID != future_id)
        goto error;

    H5E_BEGIN_TRY
    {
        future_id = H5Iregister_future(H5I_BADID, &fake_future_obj, realize_future_cb, discard_future_cb);
    }
    H5E_END_TRY
    VERIFY(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID != future_id)
        goto error;

    /* Test base use-case: create a future object and destroy type without
     *  realizing the future object.
     */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = obj_type;
    future_id            = H5Iregister_future(obj_type, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* Destroy the type */
    ret = H5Idestroy_type(obj_type);
    CHECK(ret, FAIL, "H5Idestroy_type");
    if (FAIL == ret)
        goto error;

    /* Re-register a user-defined type with our custom ID-deleting callback */
    obj_type = H5Iregister_type((size_t)15, 0, free_actual_object);
    CHECK(obj_type, H5I_BADID, "H5Iregister_type");
    if (H5I_BADID == obj_type)
        goto error;

    /* Test base use-case: create a future object and realize the actual object.  */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = obj_type;
    future_id            = H5Iregister_future(obj_type, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    actual_obj = H5Iobject_verify(future_id, obj_type);
    CHECK_PTR(actual_obj, "H5Iobject_verify");
    if (NULL == actual_obj)
        goto error;
    VERIFY(*actual_obj, 7, "H5Iobject_verify");
    if (7 != *actual_obj)
        goto error;

    /* Retrieve the object again and verify that it's the same actual object */
    actual_obj2 = H5Iobject_verify(future_id, obj_type);
    CHECK_PTR(actual_obj2, "H5Iobject_verify");
    if (NULL == actual_obj2)
        goto error;
    VERIFY(*actual_obj2, 7, "H5Iobject_verify");
    if (7 != *actual_obj2)
        goto error;
    CHECK_PTR_EQ(actual_obj, actual_obj2, "H5Iobject_verify");
    if (actual_obj != actual_obj2)
        goto error;

    /* Destroy the type */
    ret = H5Idestroy_type(obj_type);
    CHECK(ret, FAIL, "H5Idestroy_type");
    if (FAIL == ret)
        goto error;

    /* Re-register a user-defined type with our custom ID-deleting callback */
    obj_type = H5Iregister_type((size_t)15, 0, free_actual_object);
    CHECK(obj_type, H5I_BADID, "H5Iregister_type");
    if (H5I_BADID == obj_type)
        goto error;

    /* Set the global future object type */
    future_obj_type_g = obj_type;

    /* Test "actual object generator" use-case: create a future object with
     *  NULL object pointer, to create new object of predefined type when
     *  future object is realized.
     */
    future_id = H5Iregister_future(obj_type, NULL, realize_future_generate_cb, discard_future_generate_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* Realize the actual object, with will be dynamically allocated within
     *  the 'realize' callback.
     */
    actual_obj = H5Iobject_verify(future_id, obj_type);
    CHECK_PTR(actual_obj, "H5Iobject_verify");
    if (NULL == actual_obj)
        goto error;
    VERIFY(*actual_obj, 7, "H5Iobject_verify");
    if (7 != *actual_obj)
        goto error;

    /* Reset the global future object type */
    future_obj_type_g = H5I_BADID;

    /* Retrieve the object again and verify that it's the same actual object */
    /* (Will fail if global future object type used) */
    actual_obj2 = H5Iobject_verify(future_id, obj_type);
    CHECK_PTR(actual_obj2, "H5Iobject_verify");
    if (NULL == actual_obj2)
        goto error;
    VERIFY(*actual_obj2, 7, "H5Iobject_verify");
    if (7 != *actual_obj2)
        goto error;
    CHECK_PTR_EQ(actual_obj, actual_obj2, "H5Iobject_verify");
    if (actual_obj != actual_obj2)
        goto error;

    /* Destroy the type */
    ret = H5Idestroy_type(obj_type);
    CHECK(ret, FAIL, "H5Idestroy_type");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    /* (DATASPACE) */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_DATASPACE;
    future_id = H5Iregister_future(H5I_DATASPACE, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* (Can't verify the type of the future ID, because the library's current
     *  implementation realizes the object during sanity checks on the ID)
     */

    /* Close future object for pre-defined type without realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_DATASPACE;
    future_id = H5Iregister_future(H5I_DATASPACE, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* Verify that the application believes the future ID is a dataspace */
    /* (Currently realizes the object "implicitly" during a sanity check) */
    id_type = H5Iget_type(future_id);
    CHECK(id_type, H5I_BADID, "H5Iget_type");
    if (H5I_BADID == id_type)
        goto error;
    if (H5I_DATASPACE != id_type)
        goto error;

    /* Close future object for pre-defined type without realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_DATASPACE;
    future_id = H5Iregister_future(H5I_DATASPACE, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* Realize future dataspace by requesting its rank */
    ret = H5Sget_simple_extent_ndims(future_id);
    CHECK(ret, FAIL, "H5Sget_simple_extent_ndims");
    if (FAIL == ret)
        goto error;
    if (1 != ret)
        goto error;

    /* Verify that the application believes the ID is still a dataspace */
    id_type = H5Iget_type(future_id);
    CHECK(id_type, H5I_BADID, "H5Iget_type");
    if (H5I_BADID == id_type)
        goto error;
    if (H5I_DATASPACE != id_type)
        goto error;

    /* Close future object for pre-defined type after realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    /* (DATATYPE) */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_DATATYPE;
    future_id            = H5Iregister_future(H5I_DATATYPE, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* (Can't verify the type of the future ID, because the library's current
     *  implementation realizes the object during sanity checks on the ID)
     */

    /* Close future object for pre-defined type without realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_DATATYPE;
    future_id            = H5Iregister_future(H5I_DATATYPE, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* Verify that the application believes the future ID is a datatype */
    /* (Currently realizes the object "implicitly" during a sanity check) */
    id_type = H5Iget_type(future_id);
    CHECK(id_type, H5I_BADID, "H5Iget_type");
    if (H5I_BADID == id_type)
        goto error;
    if (H5I_DATATYPE != id_type)
        goto error;

    /* Close future object for pre-defined type without realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_DATATYPE;
    future_id            = H5Iregister_future(H5I_DATATYPE, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* Realize future datatype by requesting its class */
    type_class = H5Tget_class(future_id);
    CHECK(ret, FAIL, "H5Tget_class");
    if (FAIL == ret)
        goto error;
    if (H5T_INTEGER != type_class)
        goto error;

    /* Verify that the application believes the ID is still a datatype */
    id_type = H5Iget_type(future_id);
    CHECK(id_type, H5I_BADID, "H5Iget_type");
    if (H5I_BADID == id_type)
        goto error;
    if (H5I_DATATYPE != id_type)
        goto error;

    /* Close future object for pre-defined type after realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    /* (PROPERTY LIST) */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_GENPROP_LST;
    future_id = H5Iregister_future(H5I_GENPROP_LST, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* (Can't verify the type of the future ID, because the library's current
     *  implementation realizes the object during sanity checks on the ID)
     */

    /* Close future object for pre-defined type without realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_GENPROP_LST;
    future_id = H5Iregister_future(H5I_GENPROP_LST, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* Verify that the application believes the future ID is a property list */
    /* (Currently realizes the object "implicitly" during a sanity check) */
    id_type = H5Iget_type(future_id);
    CHECK(id_type, H5I_BADID, "H5Iget_type");
    if (H5I_BADID == id_type)
        goto error;
    if (H5I_GENPROP_LST != id_type)
        goto error;

    /* Close future object for pre-defined type without realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    /* Test base use-case: create a future object for a pre-defined type */
    future_obj           = HDmalloc(sizeof(future_obj_t));
    future_obj->obj_type = H5I_GENPROP_LST;
    future_id = H5Iregister_future(H5I_GENPROP_LST, future_obj, realize_future_cb, discard_future_cb);
    CHECK(future_id, H5I_INVALID_HID, "H5Iregister_future");
    if (H5I_INVALID_HID == future_id)
        goto error;

    /* Realize future property list by verifying its class */
    ret = H5Pisa_class(future_id, H5P_DATASET_XFER);
    CHECK(ret, FAIL, "H5Pisa_class");
    if (FAIL == ret)
        goto error;
    if (TRUE != ret)
        goto error;

    /* Verify that the application believes the ID is still a property list */
    id_type = H5Iget_type(future_id);
    CHECK(id_type, H5I_BADID, "H5Iget_type");
    if (H5I_BADID == id_type)
        goto error;
    if (H5I_GENPROP_LST != id_type)
        goto error;

    /* Close future object for pre-defined type after realizing it */
    ret = H5Idec_ref(future_id);
    CHECK(ret, FAIL, "H5Idec_ref");
    if (FAIL == ret)
        goto error;

    return 0;

error:
    /* Cleanup. For simplicity, just destroy the types and ignore errors. */
    H5E_BEGIN_TRY
    {
        H5Idestroy_type(obj_type);
    }
    H5E_END_TRY

    return -1;
} /* end test_future_ids() */
#endif

void
test_ids(void)
{
    /* Set the random # seed */
    HDsrandom((unsigned)HDtime(NULL));

    if (basic_id_test() < 0)
        TestErrPrintf("Basic ID test failed\n");
    if (id_predefined_test() < 0)
        TestErrPrintf("Predefined ID type test failed\n");
    if (test_is_valid() < 0)
        TestErrPrintf("H5Iis_valid test failed\n");
    if (test_get_type() < 0)
        TestErrPrintf("H5Iget_type test failed\n");
    if (test_id_type_list() < 0)
        TestErrPrintf("ID type list test failed\n");
    if (test_remove_clear_type() < 0)
        TestErrPrintf("ID remove during H5Iclear_type test failed\n");
#if defined(H5VL_VERSION) && H5VL_VERSION >= 2
    if (test_future_ids() < 0)
        TestErrPrintf("Future ID test failed\n");
#endif
}
