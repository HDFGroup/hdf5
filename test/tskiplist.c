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
   Test HDF Skip List routines.

   REMARKS

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
       Quincey Koziol

   MODIFICATION HISTORY
       11/15/04 - Started coding
 */

#include "testhdf5.h"
#include "H5SLprivate.h"

/* The number of elements in testing arrays */
#define NUM_ELEMS       1000

/* Random numbers */
static int *rand_num;
static int *sort_rand_num;
static int *rev_sort_rand_num;

static int tst_sort(const void *i1, const void *i2)
{
    return(*(const int *)i1-*(const int *)i2);
}

static int tst_rev_sort(const void *i1, const void *i2)
{
    return(*(const int *)i2-*(const int *)i1);
}

/****************************************************************
**
**  test_skiplist_init(): Test H5SL (skiplist) code.
**      Initialize data for skip list testing
**
****************************************************************/
static void
test_skiplist_init(void)
{
    time_t curr_time;   /* Current time, for seeding random number generator */
    int new_val;        /* New value to insert */
    unsigned found;     /* Flag to indicate value was inserted already */
    size_t u,v;         /* Local index variables */

    /* Allocate arrays */
    rand_num = (int *)HDmalloc(sizeof(int) * NUM_ELEMS);
    CHECK_PTR(rand_num, "HDmalloc");
    sort_rand_num = (int *)HDmalloc(sizeof(int) * NUM_ELEMS);
    CHECK_PTR(sort_rand_num, "HDmalloc");
    rev_sort_rand_num = (int *)HDmalloc(sizeof(int) * NUM_ELEMS);
    CHECK_PTR(rev_sort_rand_num, "HDmalloc");

    /* Initialize random number seed */
    curr_time = HDtime(NULL);
    HDsrandom((unsigned)curr_time);

    /* Create randomized set of numbers */
    for(u = 0; u < NUM_ELEMS; u++) {
        do {
            /* Reset flag */
            found = 0;

            /* Generate random numbers from -5000 to 5000 */
            new_val = (int)(HDrandom() % 10001) - 5001;

            /* Check if the value is already in the array */
            for(v = 0; v < u; v++)
                if(rand_num[v] == new_val)
                    found = 1;
        } while(found);

        /* Set unique value in array */
        rand_num[u]=new_val;
    } /* end for */

    /* Copy random values to sorted array */
    HDmemcpy(sort_rand_num, rand_num, sizeof(int) * NUM_ELEMS);

    /* Sort random numbers */
    HDqsort(sort_rand_num, (size_t)NUM_ELEMS, sizeof(int), tst_sort);

    /* Copy random values to reverse sorted array */
    HDmemcpy(rev_sort_rand_num, rand_num, sizeof(int) * NUM_ELEMS);

    /* Sort random numbers */
    HDqsort(rev_sort_rand_num, (size_t)NUM_ELEMS, sizeof(int), tst_rev_sort);
} /* end test_skiplist_init() */

/****************************************************************
**
**  test_skiplist_create(): Test basic H5SL (skiplist) code.
**      Tests creating and closing skip lists.
**
****************************************************************/
static void
test_skiplist_create(void)
{
    H5SL_t *slist;      /* Skip list created */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(6, ("Testing Creating & Closing Skip Lists\n"));

    /* Try creating a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Try closing the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_create() */

/****************************************************************
**
**  test_skiplist_insert(): Test H5SL (skip list) code.
**      Tests inserting single object into skip list.
**
****************************************************************/
static void
test_skiplist_insert(void)
{
    H5SL_t *slist;      /* Skip list created */
    int key,            /* Key of item to insert */
        item;           /* Item to insert */
    int search_key;     /* Key of item to search for in skip list */
    int *found_item;    /* Item found in skip list */
    size_t num;         /* Number of elements in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Insertion Into Skip List\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Try searching for item in empty skip list */
    key=37;
    found_item=(int *)H5SL_search(slist,&key);
    CHECK_PTR_NULL(found_item, "H5SL_search");

    /* Insert an object into the skip list */
    key=2; item=10;
    ret=H5SL_insert(slist,&item,&key);
    CHECK(ret, FAIL, "H5SL_insert");

    /* Check that the skip list has one element */
    num=H5SL_count(slist);
    VERIFY(num, 1, "H5SL_count");

    /* Search for the item just inserted */
    found_item=(int *)H5SL_search(slist,&key);
    CHECK_PTR(found_item, "H5SL_search");
    VERIFY(*found_item, item, "H5SL_search");

    /* Search for an item not in list */
    search_key=37;
    found_item=(int *)H5SL_search(slist,&search_key);
    CHECK_PTR_NULL(found_item, "H5SL_search");

    /* Attempt to insert duplicate key (should fail) */
    search_key=2;
    ret=H5SL_insert(slist,&search_key,&search_key);
    VERIFY(ret, FAIL, "H5SL_insert");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_insert() */

/****************************************************************
**
**  test_skiplist_insert_many(): Test H5SL (skip list) code.
**      Tests inserting many objects into skip list.
**
****************************************************************/
static void
test_skiplist_insert_many(void)
{
    H5SL_t *slist;      /* Skip list created */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    int *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Insertion of Many Items Into Skip List\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert many objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Search for all objects in the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        found_item=(int *)H5SL_search(slist,&rand_num[u]);
        CHECK_PTR(found_item, "H5SL_search");
        VERIFY(*found_item,rand_num[u],"H5SL_search");
    } /* end for */

    /* Release all the items from the list */
    ret=H5SL_release(slist);
    CHECK(ret, FAIL, "H5SL_release");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert many objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Release all the items from the list */
    ret=H5SL_release(slist);
    CHECK(ret, FAIL, "H5SL_release");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_insert_many() */

/****************************************************************
**
**  test_skiplist_remove(): Test H5SL (skip list) code.
**      Tests basic object removal from skip list.
**
****************************************************************/
static void
test_skiplist_remove(void)
{
    H5SL_t *slist;      /* Skip list created */
    int key1,           /* Key of 1st item to insert */
        key2,           /* Key of 2nd item to insert */
        key3;           /* Key of 3rd item to insert */
    int search_key;     /* Key of item to search for in skip list */
    int *found_item;    /* Item found in skip list */
    size_t num;         /* Number of elements in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Removal From Skip List\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Try removing an item in empty skip list */
    search_key=37;
    found_item=(int *)H5SL_remove(slist,&search_key);
    CHECK_PTR_NULL(found_item, "H5SL_remove");

    /* Insert three objects into the skip list */
    key1=15;
    ret=H5SL_insert(slist,&key1,&key1);
    CHECK(ret, FAIL, "H5SL_insert");

    key2=10;
    ret=H5SL_insert(slist,&key2,&key2);
    CHECK(ret, FAIL, "H5SL_insert");

    key3=20;
    ret=H5SL_insert(slist,&key3,&key3);
    CHECK(ret, FAIL, "H5SL_insert");

    /* Check that the skip list has three elements */
    num=H5SL_count(slist);
    VERIFY(num, 3, "H5SL_count");

    /* Try removing items from skip list */
    search_key=key1;
    found_item=(int *)H5SL_remove(slist,&search_key);
    CHECK_PTR(found_item, "H5SL_remove");
    CHECK_PTR_EQ(found_item, &key1, "H5SL_remove");

    search_key=key2;
    found_item=(int *)H5SL_remove(slist,&search_key);
    CHECK_PTR(found_item, "H5SL_remove");
    CHECK_PTR_EQ(found_item, &key2, "H5SL_remove");

    search_key=key3;
    found_item=(int *)H5SL_remove(slist,&search_key);
    CHECK_PTR(found_item, "H5SL_remove");
    CHECK_PTR_EQ(found_item, &key3, "H5SL_remove");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Try removing items from empty skip list (after its been worked on) */
    search_key=key1;
    found_item=(int *)H5SL_remove(slist,&search_key);
    CHECK_PTR_NULL(found_item, "H5SL_remove");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_remove() */

/****************************************************************
**
**  test_skiplist_remove_many(): Test H5SL (skip list) code.
**      Tests removing many objects from skip list.
**
****************************************************************/
static void
test_skiplist_remove_many(void)
{
    H5SL_t *slist;      /* Skip list created */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    int *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Removal of Many Items From Skip List\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert many objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Remove all objects from the skip list (in random order) */
    for(u=0; u<NUM_ELEMS; u++) {
        found_item=(int *)H5SL_remove(slist,&rand_num[u]);
        CHECK_PTR(found_item, "H5SL_remove");
        VERIFY(*found_item,rand_num[u],"H5SL_remove");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

/* Insert & remove again (in sorted order), to check that completely empty skip lists can be added again */

    /* Insert many objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Remove all objects from the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        found_item=(int *)H5SL_remove(slist,&sort_rand_num[u]);
        CHECK_PTR(found_item, "H5SL_remove");
        VERIFY(*found_item,sort_rand_num[u],"H5SL_remove");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

/* Insert & remove again (in reverse sorted order), to check that completely empty skip lists can be added again */

    /* Insert many objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Remove all objects from the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        found_item=(int *)H5SL_remove(slist,&rev_sort_rand_num[u]);
        CHECK_PTR(found_item, "H5SL_remove");
        VERIFY(*found_item,rev_sort_rand_num[u],"H5SL_remove");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_remove_many() */

/****************************************************************
**
**  test_skiplist_firstnext(): Test H5SL (skip list) code.
**      Tests iterating over nodes in skip list with first/next calls.
**
****************************************************************/
static void
test_skiplist_firstnext(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    int *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Iterating Over Skip List With First/Next\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);

    VERIFY(num, 0, "H5SL_count");
    /* Check that the list appears empty */
    node=H5SL_first(slist);
    CHECK_PTR_NULL(node, "H5SL_first");

    /* Insert many objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Iterate over all the nodes in the skip list */
    node=H5SL_first(slist);
    CHECK_PTR(node, "H5SL_first");
    u=0;
    while(node!=NULL) {
        found_item=(int *)H5SL_item(node);
        VERIFY(*found_item,sort_rand_num[u],"H5SL_next");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Release all the items from the list */
    ret=H5SL_release(slist);
    CHECK(ret, FAIL, "H5SL_release");

    /* Check that the list appears empty again */
    node=H5SL_first(slist);
    CHECK_PTR_NULL(node, "H5SL_first");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_firstnext() */

/****************************************************************
**
**  test_skiplist_string(): Test H5SL (skip list) code.
**      Tests using strings for keys in skip lists.
**
****************************************************************/
static void
test_skiplist_string(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    typedef struct string_node {
        int i;
        const char *s;
    } string_node;
    string_node data[10]={
        {10,"10"},
        {20,"20"},
        {15,"15"},
        { 5,"05"},
        {50,"50"},
        {30,"30"},
        {31,"31"},
        {32,"32"},
        {80,"80"},
        {90,"90"}};
    string_node hashed_data[10]={
        { 5,"05"},
        {10,"10"},
        {15,"15"},
        {20,"20"},
        {30,"30"},
        {31,"31"},
        {32,"32"},
        {50,"50"},
        {80,"80"},
        {90,"90"}
        };
    string_node *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List With String Keys\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_STR, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert objects into the skip list */
    for(u=0; u<10; u++) {
        ret=H5SL_insert(slist,&data[u],data[u].s);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 10, "H5SL_count");

    /* Iterate over all the nodes in the skip list */
    node=H5SL_first(slist);
    u=0;
    while(node!=NULL) {
        found_item=(string_node *)H5SL_item(node);
        VERIFY(found_item->i, hashed_data[u].i, "H5SL_next");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_string() */

static herr_t
test_skiplist_iter(void *item, void H5_ATTR_UNUSED *key, void *op_data)
{
    size_t *up=(size_t *)op_data;

    VERIFY(*(int *)item,sort_rand_num[*up],"H5SL_iterate");
    (*up)++;

    return(0);
}

/****************************************************************
**
**  test_skiplist_iterate(): Test H5SL (skip list) code.
**      Tests iterating over nodes in skip list with callbacks.
**
****************************************************************/
static void
test_skiplist_iterate(void)
{
    H5SL_t *slist;      /* Skip list created */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Iterating Over Skip List\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert many objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Iterate over all the nodes in the skip list */
    u=0;
    ret=H5SL_iterate(slist,test_skiplist_iter,&u);
    CHECK(ret, FAIL, "H5SL_iterate");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_iterate() */

/****************************************************************
**
**  test_skiplist_hsize(): Test H5SL (skip list) code.
**      Tests using hsize_t for keys in skip lists.
**
****************************************************************/
static void
test_skiplist_hsize(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    hsize_t data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    hsize_t sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90};
    hsize_t *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List With hsize_t Keys\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_HSIZE, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert objects into the skip list */
    for(u=0; u<10; u++) {
        ret=H5SL_insert(slist,&data[u],&data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 10, "H5SL_count");

    /* Iterate over all the nodes in the skip list */
    node=H5SL_first(slist);
    u=0;
    while(node!=NULL) {
        found_item=(hsize_t *)H5SL_item(node);
        VERIFY(*found_item,sorted_data[u],"H5SL_next");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_hsize() */

/****************************************************************
**
**  test_skiplist_unsigned(): Test H5SL (skip list) code.
**      Tests using unsigned for keys in skip lists.
**
****************************************************************/
static void
test_skiplist_unsigned(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    unsigned data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    unsigned sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90};
    unsigned *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List With unsigned Keys\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_UNSIGNED, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert objects into the skip list */
    for(u=0; u<10; u++) {
        ret=H5SL_insert(slist,&data[u],&data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 10, "H5SL_count");

    /* Iterate over all the nodes in the skip list */
    node=H5SL_first(slist);
    u=0;
    while(node!=NULL) {
        found_item=(unsigned *)H5SL_item(node);
        VERIFY(*found_item,sorted_data[u],"H5SL_next");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_unsigned() */

/****************************************************************
**
**  test_skiplist_obj(): Test H5SL (skip list) code.
**      Tests using H5_obj_t's for keys in skip lists.
**
****************************************************************/
static void
test_skiplist_obj(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    H5_obj_t data[10]={ {10, 12}, {20, 12}, {10, 32}, {10, 11}, {50, 1}, {8, 12}, {31, 12}, {20, 11}, {31, 11}, {8, 32} };
    H5_obj_t sorted_data[10]={ {8, 12}, {8, 32}, {10, 11}, {10, 12}, {10, 32}, {20, 11}, {20, 12}, {31, 11}, {31, 12}, {50, 1} };
    H5_obj_t *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List With H5_obj_t Keys\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_OBJ, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert objects into the skip list */
    for(u=0; u<10; u++) {
        ret=H5SL_insert(slist,&data[u],&data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 10, "H5SL_count");

    /* Iterate over all the nodes in the skip list */
    node=H5SL_first(slist);
    u=0;
    while(node!=NULL) {
        found_item=(H5_obj_t *)H5SL_item(node);
        VERIFY(found_item->fileno,sorted_data[u].fileno,"H5SL_next");
        VERIFY(found_item->addr,sorted_data[u].addr,"H5SL_next");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_obj() */

/****************************************************************
**
**  test_skiplist_generic(): Test H5SL (skip list) code.
**      Tests using generic keys in skip lists.  Define
**      structure for keys, and define comparison using
**      key.a-key.b.
**
****************************************************************/
typedef struct generic_t {
    unsigned a;
    unsigned b;
} generic_t;

static int
test_skiplist_generic_cmp(const void *_key1, const void *_key2)
{
    const generic_t     *key1 = (const generic_t *)_key1;
    const generic_t     *key2 = (const generic_t *)_key2;
    long long           result = (long long)(key1->a) - (long long)(key1->b) - (long long)(key2->a) + (long long)(key2->b);

    if(result < 0)
        return -1;
    else if(result > 0)
        return 1;
    else
        return 0;
} /* end test_skiplist_generic_cmp */

static void
test_skiplist_generic(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    generic_t data[10]={ {10, 1}, {20, 13}, {15, 32}, {5, 2}, {50, 37}, {30, 100}, {31, 38}, {32, 34}, {80, 32}, {90, 0} };
    generic_t sorted_data[10]={ {30, 100}, {15, 32}, {31, 38}, {32, 34}, {5, 2}, {20, 13}, {10, 1}, {50, 37}, {80, 32}, {90, 0} };
    generic_t *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List With generic Keys\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_GENERIC, test_skiplist_generic_cmp);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert objects into the skip list */
    for(u=0; u<10; u++) {
        ret=H5SL_insert(slist,&data[u],&data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 10, "H5SL_count");

    /* Iterate over all the nodes in the skip list */
    node=H5SL_first(slist);
    u=0;
    while(node!=NULL) {
        found_item=(generic_t *)H5SL_item(node);
        VERIFY(found_item->a,sorted_data[u].a,"H5SL_next");
        VERIFY(found_item->b,sorted_data[u].b,"H5SL_next");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_generic() */

/****************************************************************
**
**  test_skiplist_lastprev(): Test H5SL (skip list) code.
**      Tests iterating over nodes in skip list with last/prev calls.
**
****************************************************************/
static void
test_skiplist_lastprev(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    int *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Iterating Over Skip List With Last/Prev\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Check that the list appears empty */
    node=H5SL_last(slist);
    CHECK_PTR_NULL(node, "H5SL_last");

    /* Insert many objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Iterate over all the nodes in the skip list */
    node=H5SL_last(slist);
    CHECK_PTR(node, "H5SL_last");
    u=NUM_ELEMS-1;
    while(node!=NULL) {
        found_item=(int *)H5SL_item(node);
        VERIFY(*found_item,sort_rand_num[u],"H5SL_prev");
        u--;
        node=H5SL_prev(node);
    } /* end while */

    /* Release all the items from the list */
    ret=H5SL_release(slist);
    CHECK(ret, FAIL, "H5SL_release");

    /* Check that the list appears empty again */
    node=H5SL_last(slist);
    CHECK_PTR_NULL(node, "H5SL_last");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_lastprev() */

/****************************************************************
**
**  test_skiplist_find(): Test H5SL (skip list) code.
**      Tests 'find' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_find(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t u;           /* Local index variable */
    unsigned data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    unsigned sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90};
    unsigned *found_item;    /* Item found in skip list */
    unsigned find_item; /* Item to find in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Find' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_UNSIGNED, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u=0; u<10; u++) {
        ret=H5SL_insert(slist,&data[u],&data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Find the element with key==30 in the skip list */
    find_item=30;
    node=H5SL_find(slist,&find_item);
    CHECK_PTR(node, "H5SL_find");

    /* Iterate over the rest of the nodes in the skip list */
    u=4;
    while(node!=NULL) {
        found_item=(unsigned *)H5SL_item(node);
        VERIFY(*found_item,sorted_data[u],"H5SL_next");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Check for trying to locate non-existent item */
    find_item=81;
    node=H5SL_find(slist,&find_item);
    CHECK_PTR_NULL(node, "H5SL_find");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_find() */

/****************************************************************
**
**  test_skiplist_add(): Test H5SL (skip list) code.
**      Tests 'add' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_add(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t u;           /* Local index variable */
    unsigned data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    unsigned sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90};
    unsigned *found_item;    /* Item found in skip list */
    unsigned new_item;  /* Item to add to skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Add' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_UNSIGNED, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u=0; u<10; u++) {
        ret=H5SL_insert(slist,&data[u],&data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Add the element with key==12 in the skip list */
    new_item=12;
    node=H5SL_add(slist,&new_item,&new_item);
    CHECK_PTR(node, "H5SL_add");

    /* Advance to next node in list */
    node=H5SL_next(node);
    CHECK_PTR(node, "H5SL_next");

    /* Iterate over the rest of the nodes in the skip list */
    u=2;
    while(node!=NULL) {
        found_item=(unsigned *)H5SL_item(node);
        VERIFY(*found_item,sorted_data[u],"H5SL_item");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_add() */

static herr_t
test_skiplist_destroy_free(void *item, void H5_ATTR_UNUSED *key, void *op_data)
{
    unsigned *free_count=(unsigned *)op_data;

    VERIFY(*(int *)item,sort_rand_num[*free_count],"H5SL_destroy");
    (*free_count)++;

    return(0);
}

/****************************************************************
**
**  test_skiplist_destroy(): Test H5SL (skip list) code.
**      Tests 'destroy' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_destroy(void)
{
    H5SL_t *slist;      /* Skip list created */
    size_t u;           /* Local index variable */
    unsigned free_count; /* Number of items freed */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Destroy' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Destroy the skip list */
    free_count=0;
    ret=H5SL_destroy(slist,test_skiplist_destroy_free,&free_count);
    CHECK(ret, FAIL, "H5SL_destroy");
    VERIFY(free_count, NUM_ELEMS, "H5SL_destroy");

} /* end test_skiplist_destroy() */

/****************************************************************
**
**  test_skiplist_free(): Test H5SL (skip list) code.
**      Tests 'free' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_free(void)
{
    H5SL_t *slist;      /* Skip list created */
    size_t num;         /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    unsigned free_count; /* Number of items freed */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Free' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Destroy the skip list */
    free_count=0;
    ret=H5SL_free(slist,test_skiplist_destroy_free,&free_count);
    CHECK(ret, FAIL, "H5SL_destroy");
    VERIFY(free_count, NUM_ELEMS, "H5SL_free");

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Insert objects into the skip list again */
    for(u=0; u<NUM_ELEMS; u++) {
        ret=H5SL_insert(slist,&rand_num[u],&rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, NUM_ELEMS, "H5SL_count");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_free() */

/****************************************************************
**
**  test_skiplist_try_free_safe(): Test H5SL (skip list) code.
**      Tests 'try_free_safe' operation in skip lists.
**
****************************************************************/
/* Macro definitions */
#define TEST_TFS_MAX_NOBJS 100
#define TEST_TFS_MIN_NOBJS 5
#define TEST_TFS_NITER 50

/* Structure to hold the list of objects */
typedef struct {
    H5SL_t *slist;              /* Skiplist holding the objects */
    struct test_tfs_obj_t *list; /* Linear list of objects */
    int nobjs;                  /* Number of objects in list */
    int nobjs_rem;              /* Number of objects in list that have not been freed */
} test_tfs_list_t;

/* Structure for an object */
typedef struct test_tfs_obj_t {
    int idx;                    /* Index (key) for this object */
    int nfrees;                 /* Number of times this object has been freed */
} test_tfs_obj_t;

/* op_data struct for H5SL_iterate() */
typedef struct test_tfs_it_ud_t {
    test_tfs_list_t *obj_list;  /* List of objects */
    int last_idx;               /* Index of last object visited in iteration */
    int ncalls;                 /* Number of times this callback was called */
} test_tfs_it_ud_t;

/* iterate callback */
static herr_t test_tfs_iter(void *_obj, void *key, void *_udata) {
    test_tfs_obj_t *obj = (test_tfs_obj_t *)_obj;
    test_tfs_it_ud_t *udata = (test_tfs_it_ud_t *)_udata;

    /* Check consistency */
    CHECK_PTR_EQ((void *)&obj->idx, key, "obj->idx");
    CHECK_PTR_EQ(obj, &udata->obj_list->list[obj->idx], "obj_list->list[obj->idx]");

    /* Increment number of calls */
    udata->ncalls++;

    /* Verify we were given the correct object */
    do {
        udata->last_idx++;
    } while(udata->obj_list->list[udata->last_idx].nfrees != 0);
    VERIFY(udata->last_idx, obj->idx, "H5SL_iterate");

    return 0;
} /* end test_tfs_iter() */

/* try_free_safe callback */
static htri_t test_tfs_free(void *_obj, void *key, void *_obj_list) {
    test_tfs_obj_t *obj = (test_tfs_obj_t *)_obj;
    test_tfs_list_t *obj_list = (test_tfs_list_t *)_obj_list;
    test_tfs_it_ud_t iter_ud;
    int nrems, rem_idx, i, j;
    test_tfs_obj_t *obj_ret;
    herr_t  ret;        /* return value */
    htri_t ret_value;

    /* Check consistency */
    CHECK_PTR_EQ((void *)&obj->idx, key, "obj->idx");
    CHECK_PTR_EQ(obj, &obj_list->list[obj->idx], "obj_list->list[obj->idx]");

    /* Mark this object as freed (to make sure it isn't recursively freed, that
     * is not something we support, we will undo this if we decide later not to
     * free the object) */
    obj->nfrees++;
    obj_list->nobjs_rem--;

    /* Decide how many objects to remove */
    nrems = (int)(HDrandom() % (long)3);

    /* Remove objects */
    for(i = 0; i < nrems; i++)
        /* Check nobjs_rem */
        if(obj_list->nobjs_rem > 0) {
            /* Remove a random object from the list */
            rem_idx = (int)(HDrandom() % (long)obj_list->nobjs_rem);

            /* Scan the list, finding the rem_idx'th object that has not been
             * freed */
            for(j = 0; j < obj_list->nobjs; j++)
                if(obj_list->list[j].nfrees == 0) {
                    if(rem_idx == 0)
                        break;
                    else
                        rem_idx--;
                } /* end if */
            if(j == obj_list->nobjs)
                ERROR("invalid obj_list");
            else {
                /* Remove the object */
                obj_ret = (test_tfs_obj_t *)H5SL_remove(obj_list->slist, &j);
                CHECK_PTR(obj_ret, "H5SL_remove");
                obj_ret->nfrees++;
                obj_list->nobjs_rem--;
            } /* end else */
        } /* end if */

    /* Mark this object as not freed so we know to expect it in the iterate call
     */
    obj->nfrees--;
    obj_list->nobjs_rem++;

    /* Iterate over skip list (maybe) */
    if(HDrandom() % (long)5) {
        iter_ud.obj_list = obj_list;
        iter_ud.last_idx = -1;
        iter_ud.ncalls = 0;
        ret = H5SL_iterate(obj_list->slist, test_tfs_iter, &iter_ud);
        CHECK(ret, FAIL, "H5SL_iterate");
        VERIFY(iter_ud.ncalls, obj_list->nobjs_rem, "H5SL_iterate");
    } /* end if */

    /* Verify nobjs_rem is non-negative */
    if(obj_list->nobjs_rem < 0)
        ERROR("invalid nobjs_rem");

    /* Decide whether this object should be freed */
    if(HDrandom() % (long)2) {
        /* Free the object */
        ret_value = TRUE;
        obj->nfrees++;
        obj_list->nobjs_rem--;
    } /* end if */
    else
        /* Do not free the object */
        ret_value = FALSE;

    return ret_value;
} /* end test_tfs_free() */

/* Test function */
static void
test_skiplist_try_free_safe(void)
{
    test_tfs_list_t obj_list;
    test_tfs_obj_t list[TEST_TFS_MAX_NOBJS];
    int i, j;
    int nobjs_found;
    hsize_t count;
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Try Free Safe' Operation\n"));

    /* Create a skip list */
    obj_list.slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(obj_list.slist, "H5SL_create");

    /* Init obj_list.list */
    obj_list.list = list;
    for(j = 0; j < TEST_TFS_MAX_NOBJS; j++)
        list[j].idx = j;

    for(i = 0; i < TEST_TFS_NITER; i++) {
        /* Build object list */
        obj_list.nobjs = obj_list.nobjs_rem = (int)(TEST_TFS_MIN_NOBJS + (HDrandom() % (long)(TEST_TFS_MAX_NOBJS - TEST_TFS_MIN_NOBJS + 1)));
        for(j = 0; j < obj_list.nobjs; j++) {
            list[j].nfrees = 0;
            ret = H5SL_insert(obj_list.slist, &list[j], &list[j].idx);
            CHECK(ret, FAIL, "H5SL_insert");
        } /* end for */

        /* Call H5S_try_free_safe() - free most of the items in the skip list in
         * a safe manner */
        ret = H5SL_try_free_safe(obj_list.slist, test_tfs_free, &obj_list);
        CHECK(ret, FAIL, "H5SL_try_free_safe");

        /* Verify list */
        nobjs_found = 0;
        for(j = 0; j < obj_list.nobjs; j++)
            if(list[j].nfrees == 0)
                nobjs_found++;
            else
                VERIFY(list[j].nfrees, (long)1, "list[j].nfrees");

        /* Verify number of objects */
        VERIFY(obj_list.nobjs_rem, nobjs_found, "obj_list.nobjs_rem");
        count = H5SL_count(obj_list.slist);
        VERIFY(count, (size_t)nobjs_found, "H5SL_count");

        /* Release the skip list, forcibly freeing all nodes (will not make
         * callbacks) */
        ret = H5SL_release(obj_list.slist);
        CHECK(ret, FAIL, "H5SL_release");

        /* Verify number of objects is 0 */
        count = H5SL_count(obj_list.slist);
        VERIFY(count, (size_t)0, "H5SL_count");
    } /* end for */

    /* Close the skip list */
    ret = H5SL_close(obj_list.slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_try_free_safe() */

/****************************************************************
**
**  test_skiplist_less(): Test H5SL (skip list) code.
**      Tests 'less' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_less(void)
{
    H5SL_t *slist;      /* Skip list created */
    size_t u;           /* Local index variable */
    unsigned data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    /* unsigned sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90}; */
    unsigned *found_item;    /* Item found in skip list */
    unsigned find_item;  /* Item to add to skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Less' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_UNSIGNED, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u=0; u<10; u++) {
        ret=H5SL_insert(slist,&data[u],&data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check for exact match of items in various positions */
    find_item=20;
    found_item=(unsigned *)H5SL_less(slist,&find_item);
    VERIFY(*found_item,find_item,"H5SL_less");
    find_item=90;
    found_item=(unsigned *)H5SL_less(slist,&find_item);
    VERIFY(*found_item,find_item,"H5SL_less");
    find_item=5;
    found_item=(unsigned *)H5SL_less(slist,&find_item);
    VERIFY(*found_item,find_item,"H5SL_less");

    /* Find item less than a missing key, in various positions */
    find_item=19;
    found_item=(unsigned *)H5SL_less(slist,&find_item);
    VERIFY(*found_item,15,"H5SL_less");
    find_item=89;
    found_item=(unsigned *)H5SL_less(slist,&find_item);
    VERIFY(*found_item,80,"H5SL_less");
    find_item=100;
    found_item=(unsigned *)H5SL_less(slist,&find_item);
    VERIFY(*found_item,90,"H5SL_less");
    find_item=9;
    found_item=(unsigned *)H5SL_less(slist,&find_item);
    VERIFY(*found_item,5,"H5SL_less");
    find_item=4;
    found_item=(unsigned *)H5SL_less(slist,&find_item);
    CHECK_PTR_NULL(found_item, "H5SL_less");

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_less() */

/****************************************************************
**
**  test_skiplist_greater(): Test H5SL (skip list) code.
**      Tests 'greater' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_greater(void)
{
    H5SL_t *slist;      /* Skip list created */
    size_t u;           /* Local index variable */
    unsigned data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    /* unsigned sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90}; */
    unsigned *found_item;    /* Item found in skip list */
    unsigned find_item;  /* Item to add to skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Greater' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_UNSIGNED, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u = 0; u < 10; u++) {
        ret = H5SL_insert(slist, &data[u], &data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check for exact match of items in various positions */
    find_item = 20;
    found_item = (unsigned *)H5SL_greater(slist, &find_item);
    VERIFY(*found_item, find_item, "H5SL_greater");
    find_item = 90;
    found_item = (unsigned *)H5SL_greater(slist, &find_item);
    VERIFY(*found_item, find_item, "H5SL_greater");
    find_item = 5;
    found_item = (unsigned *)H5SL_greater(slist, &find_item);
    VERIFY(*found_item, find_item, "H5SL_greater");

    /* Find item greater than a missing key, in various positions */
    find_item = 19;
    found_item = (unsigned *)H5SL_greater(slist,&find_item);
    VERIFY(*found_item, 20, "H5SL_greater");
    find_item = 89;
    found_item = (unsigned *)H5SL_greater(slist, &find_item);
    VERIFY(*found_item, 90, "H5SL_greater");
    find_item = 100;
    found_item = (unsigned *)H5SL_greater(slist, &find_item);
    CHECK_PTR_NULL(found_item, "H5SL_greater");
    find_item = 6;
    found_item = (unsigned *)H5SL_greater(slist, &find_item);
    VERIFY(*found_item, 10, "H5SL_greater");
    find_item = 4;
    found_item = (unsigned *)H5SL_greater(slist, &find_item);
    VERIFY(*found_item, 5, "H5SL_greater");

    /* Close the skip list */
    ret = H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_greater() */

/****************************************************************
**
**  test_skiplist_below(): Test H5SL (skip list) code.
**      Tests 'below' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_below(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t u;           /* Local index variable */
    unsigned data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    /* unsigned sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90}; */
    unsigned *found_item;    /* Item found in skip list */
    unsigned find_item;  /* Item to add to skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Below' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_UNSIGNED, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u = 0; u < 10; u++) {
        ret = H5SL_insert(slist, &data[u], &data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check for exact match of items in various positions */
    find_item = 20;
    node = H5SL_below(slist, &find_item);
    CHECK_PTR(node, "H5SL_below");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, find_item, "H5SL_below");
    find_item = 90;
    node = H5SL_below(slist, &find_item);
    CHECK_PTR(node, "H5SL_below");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, find_item, "H5SL_below");
    find_item = 5;
    node = H5SL_below(slist, &find_item);
    CHECK_PTR(node, "H5SL_below");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, find_item, "H5SL_below");

    /* Find item less than a missing key, in various positions */
    find_item = 19;
    node = H5SL_below(slist, &find_item);
    CHECK_PTR(node, "H5SL_below");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, 15, "H5SL_below");
    find_item = 89;
    node = H5SL_below(slist, &find_item);
    CHECK_PTR(node, "H5SL_below");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, 80, "H5SL_below");
    find_item = 100;
    node = H5SL_below(slist, &find_item);
    CHECK_PTR(node, "H5SL_below");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, 90, "H5SL_below");
    find_item = 9;
    node = H5SL_below(slist, &find_item);
    CHECK_PTR(node, "H5SL_below");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, 5, "H5SL_below");
    find_item = 4;
    node = (H5SL_node_t *)H5SL_less(slist, &find_item);
    CHECK_PTR_NULL(node, "H5SL_below");

    /* Close the skip list */
    ret = H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_below() */

/****************************************************************
**
**  test_skiplist_above(): Test H5SL (skip list) code.
**      Tests 'above' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_above(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    size_t u;           /* Local index variable */
    unsigned data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    /* unsigned sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90}; */
    unsigned *found_item;    /* Item found in skip list */
    unsigned find_item;  /* Item to add to skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Above' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_UNSIGNED, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u = 0; u < 10; u++) {
        ret = H5SL_insert(slist, &data[u], &data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Check for exact match of items in various positions */
    find_item = 20;
    node = H5SL_above(slist, &find_item);
    CHECK_PTR(node, "H5SL_above");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, find_item, "H5SL_above");
    find_item = 90;
    node = H5SL_above(slist, &find_item);
    CHECK_PTR(node, "H5SL_above");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, find_item, "H5SL_above");
    find_item = 5;
    node = H5SL_above(slist, &find_item);
    CHECK_PTR(node, "H5SL_above");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, find_item, "H5SL_above");

    /* Find item greater than a missing key, in various positions */
    find_item = 19;
    node = H5SL_above(slist, &find_item);
    CHECK_PTR(node, "H5SL_above");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, 20, "H5SL_above");
    find_item = 89;
    node = H5SL_above(slist, &find_item);
    CHECK_PTR(node, "H5SL_above");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, 90, "H5SL_above");
    find_item = 100;
    node = H5SL_above(slist, &find_item);
    CHECK_PTR_NULL(node, "H5SL_above");
    find_item = 6;
    node = H5SL_above(slist, &find_item);
    CHECK_PTR(node, "H5SL_above");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, 10, "H5SL_above");
    find_item = 4;
    node = H5SL_above(slist, &find_item);
    CHECK_PTR(node, "H5SL_above");
    found_item = (unsigned *)H5SL_item(node);
    VERIFY(*found_item, 5, "H5SL_above");

    /* Close the skip list */
    ret = H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_above() */

/****************************************************************
**
**  test_skiplist_remote_first(): Test H5SL (skip list) code.
**      Tests 'remove first' operation in skip lists.
**
****************************************************************/
static void
test_skiplist_remove_first(void)
{
    H5SL_t *slist;      /* Skip list created */
    size_t u;           /* Local index variable */
    unsigned data[10]={ 10, 20, 15, 5, 50, 30, 31, 32, 80, 90};
    unsigned sorted_data[10]={ 5, 10, 15, 20, 30, 31, 32, 50, 80, 90};
    unsigned *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Remove First' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_UNSIGNED, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u = 0; u < 10; u++) {
        ret = H5SL_insert(slist, &data[u], &data[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Remove objects from the skip list */
    for(u = 0; u < 10; u++) {
        found_item = (unsigned *)H5SL_remove_first(slist);
        VERIFY(*found_item, sorted_data[u], "H5SL_remove_first");
    } /* end for */

    /* Check for removing object from empty list */
    found_item = (unsigned *)H5SL_remove_first(slist);
    CHECK_PTR_NULL(found_item, "H5SL_remove_first");

    /* Close the skip list */
    ret = H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_remove_first() */

/****************************************************************
**
**  test_skiplist_remote_first_many(): Test H5SL (skip list) code.
**      Tests 'remove first' operation in large skip lists.
**
****************************************************************/
static void
test_skiplist_remove_first_many(void)
{
    H5SL_t  *slist;         /* Skip list created */
    size_t  u;              /* Local index variable */
    int     *found_item;    /* Item found in skip list */
    int     prev_item = INT_MIN; /* Previously found item in skip list */
    herr_t  ret;            /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Skip List 'Remove First' Operation\n"));

    /* Create a skip list */
    slist = H5SL_create(H5SL_TYPE_INT, NULL);
    CHECK_PTR(slist, "H5SL_create");

    /* Insert objects into the skip list */
    for(u = 0; u < NUM_ELEMS; u++) {
        ret = H5SL_insert(slist, &rand_num[u], &rand_num[u]);
        CHECK(ret, FAIL, "H5SL_insert");
    } /* end for */

    /* Remove objects from the skip list */
    for(u = 0; u < NUM_ELEMS; u++) {
        found_item = (int *)H5SL_remove_first(slist);
        VERIFY(*found_item > prev_item, TRUE, "H5SL_remove_first");
        prev_item = *found_item;
    } /* end for */

    /* Check for removing object from empty list */
    found_item = (int *)H5SL_remove_first(slist);
    CHECK_PTR_NULL(found_item, "H5SL_remove_first");

    /* Close the skip list */
    ret = H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_remove_first() */

/****************************************************************
**
**  test_skiplist_term(): Test H5SL (skiplist) code.
**      Release data for skip list testing
**
****************************************************************/
static void
test_skiplist_term(void)
{
    /* Release arrays */
    if(rand_num)
        HDfree(rand_num);
    if(sort_rand_num)
        HDfree(sort_rand_num);
    if(rev_sort_rand_num)
        HDfree(rev_sort_rand_num);
} /* end test_skiplist_term() */

/****************************************************************
**
**  test_skiplist(): Main H5SL testing routine.
**
****************************************************************/
void
test_skiplist(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Skip Lists\n"));

    /* Initialize skip list testing data */
    test_skiplist_init();

    /* Actual skip list tests */
    test_skiplist_create();     /* Test skip list creation */
    test_skiplist_insert();     /* Test basic skip list insertion */
    test_skiplist_insert_many(); /* Test insertion of many items into skip list */
    test_skiplist_remove();     /* Test basic skip list removal */
    test_skiplist_remove_many(); /* Test removal of many items from skip list */
    test_skiplist_firstnext();  /* Test iteration over skip list nodes with first/next */
    test_skiplist_string();     /* Test skip list string keys */
    test_skiplist_iterate();    /* Test iteration over skip list nodes with callback */
    test_skiplist_hsize();      /* Test skip list hsize_t keys */
    test_skiplist_unsigned();   /* Test skip list unsigned keys */
    test_skiplist_obj();        /* Test skip list H5_obj_t keys */
    test_skiplist_generic();    /* Test skip list generic keys */
    test_skiplist_lastprev();   /* Test iteration over skip list nodes with last/prev */
    test_skiplist_find();       /* Test 'find' operation */
    test_skiplist_add();        /* Test 'add' operation */
    test_skiplist_destroy();    /* Test 'destroy' operation */
    test_skiplist_free();       /* Test 'free' operation */
    test_skiplist_try_free_safe(); /* Test 'try_free_safe' operation */
    test_skiplist_less();       /* Test 'less' operation */
    test_skiplist_greater();    /* Test 'greater' operation */
    test_skiplist_below();      /* Test 'below' operation */
    test_skiplist_above();      /* Test 'above' operation */
    test_skiplist_remove_first();   /* Test 'remove first' operation */
    test_skiplist_remove_first_many();  /* Test 'remove first' operation on large skip lists */

    /* Release skip list testing data */
    test_skiplist_term();

}   /* end test_skiplist() */

