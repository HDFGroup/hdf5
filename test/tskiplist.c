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

#include <time.h>
#include <stdlib.h>

#include "testhdf5.h"
#include "H5SLprivate.h"

/* The number of elements in testing arrays */
#define NUM_ELEMS       1000

/* Random numbers */
static int rand_num[NUM_ELEMS];
static int sort_rand_num[NUM_ELEMS];
static int rev_sort_rand_num[NUM_ELEMS];

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

    /* Initialize random number seed */
    curr_time=time(NULL);
    HDsrandom((unsigned long)curr_time);

    /* Create randomized set of numbers */
    for(u=0; u<NUM_ELEMS; u++) {
        do {
            /* Reset flag */
            found=0;

            /* Generate random numbers from -5000 to 5000 */
            new_val=(int)(HDrandom()%10001)-5001;

            /* Check if the value is already in the array */
            for(v=0; v<u; v++)
                if(rand_num[v]==new_val)
                    found=1;
        } while(found);

        /* Set unique value in array */
        rand_num[u]=new_val;
    } /* end for */

    /* Copy random values to sorted array */
    HDmemcpy(sort_rand_num,rand_num,sizeof(int)*NUM_ELEMS);

    /* Sort random numbers */
    HDqsort(sort_rand_num,NUM_ELEMS,sizeof(int),tst_sort);

    /* Copy random values to reverse sorted array */
    HDmemcpy(rev_sort_rand_num,rand_num,sizeof(int)*NUM_ELEMS);

    /* Sort random numbers */
    HDqsort(rev_sort_rand_num,NUM_ELEMS,sizeof(int),tst_rev_sort);
} /* end test_tst_init() */

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
    slist=H5SL_create(H5SL_TYPE_INT, 0.5,16);
    CHECK(slist, NULL, "H5SL_create");

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
    ssize_t num;        /* Number of elements in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Insertion Into Skip List\n"));

    /* Create a Heap */
    slist=H5SL_create(H5SL_TYPE_INT, 0.5, 16);
    CHECK(slist, NULL, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Try searching for item in empty skip list */
    key=37;
    found_item=H5SL_search(slist,&key);
    VERIFY(found_item, NULL, "H5SL_search");

    /* Insert an object into the skip list */
    key=2; item=10;
    ret=H5SL_insert(slist,&item,&key);
    CHECK(ret, FAIL, "H5SL_insert");

    /* Check that the skip list has one element */
    num=H5SL_count(slist);
    VERIFY(num, 1, "H5SL_count");

    /* Search for the item just inserted */
    found_item=H5SL_search(slist,&key);
    CHECK(found_item, NULL, "H5SL_search");
    VERIFY(*found_item,item,"H5SL_search");

    /* Search for an item not in list */
    search_key=37;
    found_item=H5SL_search(slist,&search_key);
    VERIFY(found_item, NULL, "H5SL_search");

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
    ssize_t num;        /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    int *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Insertion of Many Items Into Skip List\n"));

    /* Create a Heap */
    slist=H5SL_create(H5SL_TYPE_INT, 0.5, 16);
    CHECK(slist, NULL, "H5SL_create");

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
        found_item=H5SL_search(slist,&rand_num[u]);
        CHECK(found_item, NULL, "H5SL_search");
        VERIFY(*found_item,rand_num[u],"H5SL_search");
    } /* end for */

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
    ssize_t num;        /* Number of elements in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Removal From Skip List\n"));

    /* Create a Heap */
    slist=H5SL_create(H5SL_TYPE_INT, 0.5, 16);
    CHECK(slist, NULL, "H5SL_create");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Try removing an item in empty skip list */
    search_key=37;
    found_item=H5SL_remove(slist,&search_key);
    VERIFY(found_item, NULL, "H5SL_remove");

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
    found_item=H5SL_remove(slist,&search_key);
    CHECK(found_item, NULL, "H5SL_remove");
    VERIFY(found_item, &key1, "H5SL_remove");

    search_key=key2;
    found_item=H5SL_remove(slist,&search_key);
    CHECK(found_item, NULL, "H5SL_remove");
    VERIFY(found_item, &key2, "H5SL_remove");

    search_key=key3;
    found_item=H5SL_remove(slist,&search_key);
    CHECK(found_item, NULL, "H5SL_remove");
    VERIFY(found_item, &key3, "H5SL_remove");

    /* Check that the skip list has no elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

    /* Try removing items from empty skip list (after its been worked on) */
    search_key=key1;
    found_item=H5SL_remove(slist,&search_key);
    VERIFY(found_item, NULL, "H5SL_remove");

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
    ssize_t num;        /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    int *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Removal of Many Items From Skip List\n"));

    /* Create a Heap */
    slist=H5SL_create(H5SL_TYPE_INT, 0.5, 16);
    CHECK(slist, NULL, "H5SL_create");

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
        found_item=H5SL_remove(slist,&rand_num[u]);
        CHECK(found_item, NULL, "H5SL_remove");
        VERIFY(*found_item,rand_num[u],"H5SL_remove");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

/* Insert & remove again (in sorted order), to check that completely empty heaps can be added again */

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
        found_item=H5SL_remove(slist,&sort_rand_num[u]);
        CHECK(found_item, NULL, "H5SL_remove");
        VERIFY(*found_item,sort_rand_num[u],"H5SL_remove");
    } /* end for */

    /* Check that the skip list has correct # of elements */
    num=H5SL_count(slist);
    VERIFY(num, 0, "H5SL_count");

/* Insert & remove again (in reverse sorted order), to check that completely empty heaps can be added again */

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
        found_item=H5SL_remove(slist,&rev_sort_rand_num[u]);
        CHECK(found_item, NULL, "H5SL_remove");
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
**  test_skiplist_iterate(): Test H5SL (skip list) code.
**      Tests iterating over nodes in skip list.
** 
****************************************************************/
static void 
test_skiplist_iterate(void)
{
    H5SL_t *slist;      /* Skip list created */
    H5SL_node_t *node;  /* Skip list node */
    ssize_t num;        /* Number of elements in skip list */
    size_t u;           /* Local index variable */
    int *found_item;    /* Item found in skip list */
    herr_t ret;         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(7, ("Testing Iterating Over Skip List\n"));

    /* Create a Heap */
    slist=H5SL_create(H5SL_TYPE_INT, 0.5, 16);
    CHECK(slist, NULL, "H5SL_create");

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
    node=H5SL_first(slist);
    u=0;
    while(node!=NULL) {
        found_item=H5SL_item(node);
        VERIFY(*found_item,sort_rand_num[u],"H5SL_next");
        u++;
        node=H5SL_next(node);
    } /* end while */

    /* Close the skip list */
    ret=H5SL_close(slist);
    CHECK(ret, FAIL, "H5SL_close");

} /* end test_skiplist_iterate() */

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
    test_skiplist_iterate();    /* Test iteration over skip list nodes */

}   /* end test_skiplist() */


