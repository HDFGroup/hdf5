/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* changes:
 * rename pio_timer.c as io_timer.c;
 * Removed pio_perf.h so that it is not dependant on it;
 * Removed set_timer_type() and get_timer_type() since no one calls them;
 * Merged sio_timer.c into io_timer.c;
 */

/*
 * Purpose:
 *
 * This is a module of useful timing functions for performance testing.
 */

#include "H5private.h"
#include "hdf5.h"

#include "io_timer.h"

/*
 * The number to divide the tv_usec field with to get a nice decimal to add to
 * the number of seconds.
 */
#define MICROSECOND     1000000.0F

/* global variables */
io_time_t   *timer_g;            /* timer: global for stub functions     */

/*
 * Function:  sub_time
 * Purpose:   Struct two time values, and return the difference, in microseconds
 *
 *         Note that the function assumes that a > b
 * Programmer: Leon Arber, 1/27/06
 */
static double sub_time(struct timeval* a, struct timeval* b)
{
    return (((double)a->tv_sec +
     ((double)a->tv_usec) / (double)MICROSECOND) -
  ((double)b->tv_sec +
   ((double)b->tv_usec) / (double)MICROSECOND));
}


/*
 * Function:    io_time_new
 * Purpose:     Build us a brand, spankin', new performance time object.
 *              The object is a black box to the user. They just tell us
 *              what type of timer they want (MPI_CLOCK for MPI_Wtime or
 *              SYS_CLOCK for system time).
 * Return:      Pointer to io_time object
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
io_time_t *
io_time_new(clock_type type)
{
    io_time_t *pt = (io_time_t *)HDcalloc(1, sizeof(struct io_time_t));

    /* set global timer variable */
    timer_g = pt;

    pt->type = type;
    return pt;
}

/*
 * Function:    io_time_destroy
 * Purpose:     Remove the memory allocated for the io_time object. Only
 *              need to call on a pointer allocated with the ``io_time_new''
 *              function.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
void
io_time_destroy(io_time_t *pt)
{
    HDfree(pt);
    /* reset the global timer pointer too. */
    timer_g = NULL;
}

#if 0
/* no one is calling set_timer_type or get_timer_type ???*/
/*
 * Function:    set_timer_type
 * Purpose:     Set the type of the timer to either MPI_CLOCK or SYS_CLOCK.
 *              This really only needs to be called if you didn't construct a
 *              timer with the pio_timer_new function (shame!).
 * Return:      Nothing
 * Programmer:  Bill Wendling, 04. October 2001
 * Modifications:
 */
void
set_timer_type(io_time_t *pt, clock_type type)
{
    pt->type = type;
}

/*
 * Function:    get_timer_type
 * Purpose:     Get the type of the timer.
 * Return:      MPI_CLOCK or SYS_CLOCK.
 * Programmer:  Bill Wendling, 04. October 2001
 * Modifications:
 */
clock_type
get_timer_type(io_time_t *pt)
{
    return pt->type;
}
#endif

/*
 * Function:    set_time
 * Purpose:     Set the time in a ``io_time_t'' object.
 * Return:      Pointer to the passed in ``io_time_t'' object if SUCCEED; Null otherwise.
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
io_time_t *
set_time(io_time_t *pt, timer_type t, int start_stop)
{
    /* sanity check */
    HDassert(pt);

    switch(pt->type){
#ifdef H5_HAVE_PARALLEL
    case MPI_CLOCK:
	if (start_stop == TSTART) {
	    pt->mpi_timer[t] = MPI_Wtime();

	    /* When we start the timer for HDF5_FINE_WRITE_FIXED_DIMS or HDF5_FINE_READ_FIXED_DIMS
	     * we compute the time it took to only open the file */
	    if(t == HDF5_FINE_WRITE_FIXED_DIMS)
		pt->total_time[HDF5_FILE_WRITE_OPEN] += pt->mpi_timer[t] - pt->mpi_timer[HDF5_GROSS_WRITE_FIXED_DIMS];
	    else if(t == HDF5_FINE_READ_FIXED_DIMS)
		pt->total_time[HDF5_FILE_READ_OPEN] += pt->mpi_timer[t] - pt->mpi_timer[HDF5_GROSS_READ_FIXED_DIMS];

	} else {
	    pt->total_time[t] += MPI_Wtime() - pt->mpi_timer[t];
	    pt->mpi_timer[t] = MPI_Wtime();

	    /* When we stop the timer for HDF5_GROSS_WRITE_FIXED_DIMS or HDF5_GROSS_READ_FIXED_DIMS
	     * we compute the time it took to close the file after the last read/write finished */
	    if(t == HDF5_GROSS_WRITE_FIXED_DIMS)
		pt->total_time[HDF5_FILE_WRITE_CLOSE] += pt->mpi_timer[t] - pt->mpi_timer[HDF5_FINE_WRITE_FIXED_DIMS];
	    else if(t == HDF5_GROSS_READ_FIXED_DIMS)
		pt->total_time[HDF5_FILE_READ_CLOSE] += pt->mpi_timer[t] - pt->mpi_timer[HDF5_FINE_READ_FIXED_DIMS];
	}
	break;
#else
    case MPI_CLOCK:
	    HDfprintf(stderr, "MPI clock set in serial library\n");
	    return NULL;
#endif /* H5_HAVE_PARALLEL */
    case SYS_CLOCK:
            if (start_stop == TSTART) {
                HDgettimeofday(&pt->sys_timer[t], NULL);

		/* When we start the timer for HDF5_FINE_WRITE_FIXED_DIMS or HDF5_FINE_READ_FIXED_DIMS
		 * we compute the time it took to only open the file */
		if(t == HDF5_FINE_WRITE_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_WRITE_OPEN] += sub_time(&(pt->sys_timer[t]), &(pt->sys_timer[HDF5_GROSS_WRITE_FIXED_DIMS]));
		else if(t == HDF5_FINE_READ_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_READ_OPEN] += sub_time(&(pt->sys_timer[t]), &(pt->sys_timer[HDF5_GROSS_READ_FIXED_DIMS]));


            } else {
                struct timeval sys_t;

                HDgettimeofday(&sys_t, NULL);
                pt->total_time[t] += sub_time(&sys_t, &(pt->sys_timer[t]));

		/* When we stop the timer for HDF5_GROSS_WRITE_FIXED_DIMS or HDF5_GROSS_READ_FIXED_DIMS
		 * we compute the time it took to close the file after the last read/write finished */
		if(t == HDF5_GROSS_WRITE_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_WRITE_CLOSE] += sub_time(&(pt->sys_timer[t]), &(pt->sys_timer[HDF5_FINE_WRITE_FIXED_DIMS]));
		else if(t == HDF5_GROSS_READ_FIXED_DIMS)
		    pt->total_time[HDF5_FILE_READ_CLOSE] += sub_time(&(pt->sys_timer[t]), &(pt->sys_timer[HDF5_FINE_READ_FIXED_DIMS]));

            }
	break;

    default:
	    HDfprintf(stderr, "Unknown time clock type (%d)\n", pt->type);
	    return NULL;
    } /* end switch */

#if 0
    /* this does not belong here. Need fix in h5perf code when set_time() is called. -AKC- */
    debug_start_stop_time(pt, t, start_stop);
#endif

    return pt;
}

/*
 * Function:    get_time
 * Purpose:     Get the time from a ``io_time_t'' object.
 * Return:      The number of seconds as a DOUBLE.
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
H5_ATTR_PURE double
get_time(io_time_t *pt, timer_type t)
{
    /* sanity check */
    HDassert(pt);

    return pt->total_time[t];
}

#if 0
/* standalone is not working yet. Need fix later. -AKC- */
#ifdef STANDALONE
#include "pio_standalone.c"
#endif
#endif
