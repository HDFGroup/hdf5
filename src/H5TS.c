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
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/* private headers */
#include <H5private.h>          /*library                 		*/
#include <H5Eprivate.h>         /*error handling          		*/

#ifdef H5_HAVE_THREADSAFE

/*--------------------------------------------------------------------------
 * NAME
 *    H5_first_thread_init
 * USAGE
 *    H5_first_thread_init()
 * 
 * RETURNS
 *
 * DESCRIPTION
 *   Initialization of global API lock, keys for per-thread error stacks
 *   and cancallability information. Called by the first thread that enters
 *   the library.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 * MODIFICATIONS:
 *
 *--------------------------------------------------------------------------
 */
void H5_first_thread_init() {
  /* initialize global API mutex lock                      */
  H5_g.H5_libinit_g = FALSE;
  H5_g.init_lock.owner_thread = NULL;
  pthread_mutex_init(&H5_g.init_lock.atomic_lock, NULL);
  pthread_cond_init(&H5_g.init_lock.cond_var, NULL);
  H5_g.init_lock.lock_count = 0;

  /* initialize key for thread-specific error stacks       */
  pthread_key_create(&H5_errstk_key_g, NULL);

  /* initialize key for thread cancellability mechanism    */
  pthread_key_create(&H5_cancel_key_g, NULL);
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5_mutex_init
 * USAGE
 *    H5_mutex_init(&mutex_var)
 * 
 * RETURNS
 *
 * DESCRIPTION
 *  Recursive lock semantics for HDF5 (lock initialization) -
 *  Multiple acquisition of a lock by a thread is permitted with a
 *  corresponding unlock operation required.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 * MODIFICATIONS:
 *
 *--------------------------------------------------------------------------
 */
void H5_mutex_init(H5_mutex_t *H5_mutex) {
  (*H5_mutex).owner_thread = NULL;
  pthread_mutex_init(&(*H5_mutex).atomic_lock, NULL);
  pthread_cond_init(&(*H5_mutex).cond_var, NULL);
  (*H5_mutex).lock_count = 0;
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5_mutex_lock
 * USAGE
 *    H5_mutex_lock(&mutex_var)
 * 
 * RETURNS
 *
 * DESCRIPTION
 *  Recursive lock semantics for HDF5 (locking) -
 *  Multiple acquisition of a lock by a thread is permitted with a
 *  corresponding unlock operation required.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 * MODIFICATIONS:
 *
 *--------------------------------------------------------------------------
 */
void H5_mutex_lock(H5_mutex_t *H5_mutex) {
  pthread_mutex_lock(&(*H5_mutex).atomic_lock);
  if (pthread_equal(pthread_self(), (*H5_mutex).owner_thread)) {
    /* already owned by self - increment count */
    (*H5_mutex).lock_count++;
  } else {
    if ((*H5_mutex).owner_thread == NULL) {
      /* no one else has locked it - set owner and grab lock */
      (*H5_mutex).owner_thread = pthread_self();
      (*H5_mutex).lock_count = 1;
    } else {
      /* if already locked by someone else */
      while (1) {
	pthread_cond_wait(&(*H5_mutex).cond_var, &(*H5_mutex).atomic_lock);
	if ((*H5_mutex).owner_thread == NULL) {
	  (*H5_mutex).owner_thread = pthread_self();
	  (*H5_mutex).lock_count = 1;
	  break;
	} /* else do nothing and loop back to wait on condition*/
      }
    }
  }
  pthread_mutex_unlock(&(*H5_mutex).atomic_lock);
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5_mutex_unlock
 * USAGE
 *    H5_mutex_unlock(&mutex_var)
 * 
 * RETURNS
 *
 * DESCRIPTION
 *  Recursive lock semantics for HDF5 (unlocking) -
 *  Multiple acquisition of a lock by a thread is permitted with a
 *  corresponding unlock operation required.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 * MODIFICATIONS:
 *
 *--------------------------------------------------------------------------
 */
void H5_mutex_unlock(H5_mutex_t *H5_mutex) {
  pthread_mutex_lock(&(*H5_mutex).atomic_lock);
  (*H5_mutex).lock_count--;
  if ((*H5_mutex).lock_count == 0) {
    (*H5_mutex).owner_thread = NULL;
    pthread_cond_signal(&(*H5_mutex).cond_var);
  }
  pthread_mutex_unlock(&(*H5_mutex).atomic_lock);
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5_cancel_count_inc
 * USAGE
 *    H5_cancel_count_inc()
 * 
 * RETURNS
 *
 * DESCRIPTION
 *    Creates a cancelation counter for a thread if it is the first time
 *    the thread is entering the library.
 *
 *    if counter value is zero, then set cancelability type of the thread
 *    to PTHREAD_CANCEL_DISABLE as thread is entering the library and store
 *    the previous cancelability type into cancelation counter.
 *    Increase the counter value by 1.
 *
 * PROGRAMMER: Chee Wai LEE
 *            May 2, 2000
 *
 * MODIFICATIONS:
 *
 *--------------------------------------------------------------------------
 */
void
H5_cancel_count_inc(void)
{
  H5_cancel_t *cancel_counter;

  if (cancel_counter = pthread_getspecific(H5_cancel_key_g)) {
    /* do nothing here */
  } else {
    /* first time thread calls library - create new counter and associate
       with key
    */
    cancel_counter = (H5_cancel_t *)malloc(sizeof(H5_cancel_t));
    cancel_counter->cancel_count = 0;
    pthread_setspecific(H5_cancel_key_g, (void *)cancel_counter);
  }

  if (cancel_counter->cancel_count == 0) {
    /* thread entering library */
    pthread_setcancelstate(PTHREAD_CANCEL_DISABLE,
			   &(cancel_counter->previous_state));
  }
  cancel_counter->cancel_count++;
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5_cancel_count_dec
 * USAGE
 *    H5_cancel_count_dec()
 *   
 * RETURNS
 *
 * DESCRIPTION
 *    if counter value is one, then set cancelability type of the thread
 *    to the previous cancelability type stored in the cancelation counter.
 *    (the thread is leaving the library).
 *
 *    Decrement the counter value by 1.
 *
 * PROGRAMMER: Chee Wai LEE
 *             May 2, 2000
 *
 * MODIFICATIONS:
 *
 *--------------------------------------------------------------------------
 */
void
H5_cancel_count_dec(void)
{
  H5_cancel_t *cancel_counter = pthread_getspecific(H5_cancel_key_g);

  if (cancel_counter->cancel_count == 1) {
    pthread_setcancelstate(cancel_counter->previous_state, NULL);
  }
  cancel_counter->cancel_count--;
}

#endif
