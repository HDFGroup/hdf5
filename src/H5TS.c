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

/* Module specific data structures */

/* cancelability structure */
typedef struct H5TS_cancel_struct {
  int previous_state;
  unsigned int cancel_count;
} H5TS_cancel_t;

/* Global variable definitions */
pthread_once_t H5TS_first_init_g = PTHREAD_ONCE_INIT;
pthread_key_t H5TS_errstk_key_g;
pthread_key_t H5TS_cancel_key_g;
hbool_t H5TS_allow_concurrent_g = FALSE; /* concurrent APIs override this */

/* Local function definitions */
#ifdef NOT_USED
static void H5TS_mutex_init(H5TS_mutex_t *mutex);
#endif /* NOT_USED */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_first_thread_init
 * USAGE
 *    H5TS_first_thread_init()
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
void
H5TS_first_thread_init(void)
{
    /* initialize global API mutex lock                      */
    H5_g.H5_libinit_g = FALSE;
    H5_g.init_lock.owner_thread = NULL;
    pthread_mutex_init(&H5_g.init_lock.atomic_lock, NULL);
    pthread_cond_init(&H5_g.init_lock.cond_var, NULL);
    H5_g.init_lock.lock_count = 0;

    /* initialize key for thread-specific error stacks       */
    pthread_key_create(&H5TS_errstk_key_g, NULL);

    /* initialize key for thread cancellability mechanism    */
    pthread_key_create(&H5TS_cancel_key_g, NULL);
}

#ifdef NOT_USED
/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_mutex_init
 * USAGE
 *    H5TS_mutex_init(&mutex_var)
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
static void
H5TS_mutex_init(H5TS_mutex_t *mutex)
{
    (*mutex).owner_thread = NULL;
    pthread_mutex_init(&(*mutex).atomic_lock, NULL);
    pthread_cond_init(&(*mutex).cond_var, NULL);
    (*mutex).lock_count = 0;
}
#endif /* NOT_USED */

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_mutex_lock
 * USAGE
 *    H5TS_mutex_lock(&mutex_var)
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
void
H5TS_mutex_lock(H5TS_mutex_t *mutex)
{
    pthread_mutex_lock(&(*mutex).atomic_lock);
    if (pthread_equal(pthread_self(), (*mutex).owner_thread)) {
        /* already owned by self - increment count */
        (*mutex).lock_count++;
    } else {
        if ((*mutex).owner_thread == NULL) {
            /* no one else has locked it - set owner and grab lock */
            (*mutex).owner_thread = pthread_self();
            (*mutex).lock_count = 1;
        } else {
            /* if already locked by someone else */
            while (1) {
                pthread_cond_wait(&(*mutex).cond_var, &(*mutex).atomic_lock);
                if ((*mutex).owner_thread == NULL) {
                    (*mutex).owner_thread = pthread_self();
                    (*mutex).lock_count = 1;
                    break;
                } /* else do nothing and loop back to wait on condition*/
            }
        }
    }
    pthread_mutex_unlock(&(*mutex).atomic_lock);
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_mutex_unlock
 * USAGE
 *    H5TS_mutex_unlock(&mutex_var)
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
void
H5TS_mutex_unlock(H5TS_mutex_t *mutex)
{
    pthread_mutex_lock(&(*mutex).atomic_lock);
    (*mutex).lock_count--;
    if ((*mutex).lock_count == 0) {
        (*mutex).owner_thread = NULL;
        pthread_cond_signal(&(*mutex).cond_var);
    }
    pthread_mutex_unlock(&(*mutex).atomic_lock);
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_cancel_count_inc
 * USAGE
 *    H5TS_cancel_count_inc()
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
H5TS_cancel_count_inc(void)
{
    H5TS_cancel_t *cancel_counter;

    if ((cancel_counter = pthread_getspecific(H5TS_cancel_key_g))!=NULL) {
        /* do nothing here */
    } else {
        /* first time thread calls library - create new counter and associate
         *  with key
         */
        cancel_counter = (H5TS_cancel_t *)malloc(sizeof(H5TS_cancel_t));
        cancel_counter->cancel_count = 0;
        pthread_setspecific(H5TS_cancel_key_g, (void *)cancel_counter);
    }

    if (cancel_counter->cancel_count == 0) {
        /* thread entering library */
        pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &(cancel_counter->previous_state));
    }
    cancel_counter->cancel_count++;
}

/*--------------------------------------------------------------------------
 * NAME
 *    H5TS_cancel_count_dec
 * USAGE
 *    H5TS_cancel_count_dec()
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
H5TS_cancel_count_dec(void)
{
    H5TS_cancel_t *cancel_counter = pthread_getspecific(H5TS_cancel_key_g);

    if (cancel_counter->cancel_count == 1) {
        pthread_setcancelstate(cancel_counter->previous_state, NULL);
    }
    cancel_counter->cancel_count--;
}

#endif
