/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*
   FILE
       tbbt.c
   Test HDF Threaded-Balanced-Binary Tree (tbbt) routines.

   REMARKS

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
       Quincey Koziol

   MODIFICATION HISTORY
       4/22/00 - Converted from HDF4 test routine.
 */

#include <time.h>
#include "testhdf5.h"
#include "H5TBprivate.h"

#define MAX_TEST_SIZE 31    /* maximum number of elements to insert */
#define NUM_TEST_RUNS 100   /* number of times to insert & remove each size */

#define RandInt(a,b) ((rand()%(((b)-(a))+1))+(a))

intn tcompare (void * k1, void * k2, intn cmparg);

static void swap_arr(int *arr, intn a, intn b)
{
    int       t;

    if (a != b)
      {
          t = arr[a];
          arr[a] = arr[b];
          arr[b] = t;
      }     /* end if */
}   /* end swap_arr() */

intn
tcompare(void * k1, void * k2, intn cmparg)
{
    /* shut compiler up */
    cmparg=cmparg;
    return ((intn) ((*(int *) k1) - (*(int *) k2)));
}

void
test_tbbt(void)
{
    intn        test_size;
    intn        i, j;
    int       ins_arr[MAX_TEST_SIZE];
    int       rem_arr[MAX_TEST_SIZE];
    intn        t;
    TBBT_TREE  *tree;
    void *      *r;

    t = (intn)time(NULL);
    srand((uintn)t);

    for (test_size = 3; test_size <= MAX_TEST_SIZE; test_size++)
      {
          MESSAGE(7, ("\nTesting trees with %d elements\n", test_size));
          MESSAGE(8, ("Testing tree #:"));
          for (j = 0; j < NUM_TEST_RUNS; j++)
            {
                MESSAGE(8, (" %d", j));
                for (i = 0; i < test_size; i++)
                  {     /* initialize the arrays */
                      ins_arr[i] = i;
                      rem_arr[i] = i;
                  }     /* end for */
                for (i = 0; i < test_size; i++)
                  {     /* shuffle the arrays */
                      t = RandInt(i, test_size - 1);
                      swap_arr(ins_arr, i, t);
                      t = RandInt(i, test_size - 1);
                      swap_arr(rem_arr, i, t);
                  }     /* end for */

                if (Verbosity > 9)
                  {
                      printf("ins_arr: \n");
                      for (i = 0; i < test_size; i++)   /* print the arrays */
                          printf("%d \n", (int) ins_arr[i]);
                      printf("\nrem_arr: \n");
                      for (i = 0; i < test_size; i++)   /* print the arrays */
                          printf("%d \n", (int) rem_arr[i]);
                      printf("\n");
                  }     /* end if */

                tree = H5TB_dmake(tcompare, sizeof(int),0);
                for (i = 0; i < test_size; i++)
                  {
                      MESSAGE(9, ("inserting %d\n", (int) ins_arr[i]));
                      H5TB_dins(tree, (void *) &ins_arr[i], NULL);
#ifdef H5TB_DEBUG
                      if(Verbosity>9)
                          H5TB_dump(tree, -1);
#endif /* H5TB_DEBUG */
                  }
#ifdef H5TB_DEBUG
                if(Verbosity>9)
                    H5TB_dump(tree, -1);
#endif /* H5TB_DEBUG */
                for (i = 0; i < test_size; i++)
                  {
                      int       key;

                      key = rem_arr[i];
                      r = (void * *) H5TB_dfind(tree, (void *) &key, NULL);
                      MESSAGE(9, ("removing %d\n", (int) key));
                      H5TB_rem((TBBT_NODE **) tree, (TBBT_NODE *) r, NULL);
#ifdef H5TB_DEBUG
                      if(Verbosity>9)
                          H5TB_dump(tree, -1);
#endif /* H5TB_DEBUG */
                  } /* end for */
                H5TB_dfree(tree, NULL, NULL);
            }   /* end for */
      }     /* end for */
}   /* end test_tbbt() */

