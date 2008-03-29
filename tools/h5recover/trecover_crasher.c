/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

/*  
 * crasher HDF5 API module of the trecover test program.
 *
 * Creator: Albert Cheng, Jan 28, 2008.
 */
 
#include "trecover.h"

/* Crasher
 * Two crash modes:
 *    SyncCrash: will now at once;
 *    AsyncCrash: schedule a crash to occur in the future in the asynchronous
 *       fashion.
 * Crash_param: used by AsyncCrash mode and ignored by SyncCrash mode.
 */
void
crasher(int crash_mode, CrasherParam_t *crash_param)
{
    float	fraction, integral;
    struct	itimerval old, new;
    pid_t	mypid;

    switch (crash_mode) {
	case SyncCrash:
	    /* need to use SIGTERM since _exit(0) may hang in Red Storm. */
	    mypid = getpid();
	    kill(mypid, SIGTERM);	/* Terminate myself */
	    break;
	case AsyncCrash:
	    /* Setup a wakeup call in the future */
	    signal(SIGALRM, wakeup);
	    fraction = modff(crash_param->tinterval, &integral);
	    new.it_interval.tv_usec = 0;
	    new.it_interval.tv_sec = 0;
	    new.it_value.tv_usec = fraction * 1.0e6;
	    new.it_value.tv_sec = integral;
	    setitimer(ITIMER_REAL, &new, &old);
	    break;
	default:
	    fprintf(stderr, "Unknown Crash Mode (%d)\n", crash_mode);
	    break;
    }
}     


/* Red Storm may hang if the signal handlin routine does I/O, even just printf() calls.
 * Be aware if you want to add printf calls.
 */
void wakeup(int signum) 
{
    /* call crasher with sync mode */
    crasher(SyncCrash, 0);
}
