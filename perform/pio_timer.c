/*
 * Copyright (C) 2001
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 */

/*
 * Purpose:
 *
 * This is a module of useful timing functions for performance testing.
 */

#include <stdio.h>
#include <stdlib.h>

#include "pio_timer.h"

#ifdef H5_HAVE_PARALLEL

#include <mpi.h>

/*
 * The number to divide the tv_usec field with to get a nice decimal to add to
 * the number of seconds.
 */
#define MILLISECOND     1000000.0

/*
 * Function:    pio_time_new
 * Purpose:     Build us a brand, spankin', new performance time object.
 *              The object is a black box to the user. They just tell us
 *              what type of timer they want (MPI_TIMER for MPI_Wtime or
 *              SYS_TIMER for system time).
 * Return:      Pointer to pio_time object
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
pio_time *
pio_time_new(unsigned int type)
{
    pio_time *pt = (pio_time *)calloc(1, sizeof(struct pio_time_));

    pt->type = type;
    return pt;
}

/*
 * Function:    pio_time_destroy
 * Purpose:     Remove the memory allocated for the pio_time object. Only
 *              need to call on a pointer allocated with the ``pio_time_new''
 *              function.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
void
pio_time_destroy(pio_time *pt)
{
    free(pt);
}

/*
 * Function:    set_timer_type
 * Purpose:     Set the type of the timer to either MPI_TIMER or SYS_TIMER.
 *              This really only needs to be called if you didn't construct a
 *              timer with the pio_timer_new function (shame!).
 * Return:      Nothing
 * Programmer:  Bill Wendling, 04. October 2001
 * Modifications:
 */
void
set_timer_type(pio_time *pt, timer_type type)
{
    pt->type = type;
}

/*
 * Function:    get_timer_type
 * Purpose:     Get the type of the timer.
 * Return:      MPI_TIMER or SYS_TIMER.
 * Programmer:  Bill Wendling, 04. October 2001
 * Modifications:
 */
timer_type
get_timer_type(pio_time *pt)
{
    return pt->type;
}

/*
 * Function:    set_time
 * Purpose:     Set the time in a ``pio_time'' object.
 * Return:      Pointer to the passed in ``pio_time'' object.
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
pio_time *
set_time(pio_time *pt, timer_type t, int start_stop)
{
    if (pt) {
        if (pt->type == MPI_TIMER) {
            if (start_stop == START) {
                pt->mpi_timer[t] = MPI_Wtime();
            } else {
                pt->total_time[t] += MPI_Wtime() - pt->mpi_timer[t];
            }
        } else {
            if (start_stop == START) {
                gettimeofday(&pt->sys_timer[t], NULL);
            } else {
                struct timeval sys_t;

                gettimeofday(&sys_t, NULL);
                pt->total_time[t] += (double)pt->sys_timer[t].tv_sec +
                            ((double)pt->sys_timer[t].tv_usec) / MILLISECOND;
            }
        }
    }

    return pt;
}

/*
 * Function:    get_time
 * Purpose:     Get the time from a ``pio_time'' object.
 * Return:      The number of seconds as a DOUBLE.
 * Programmer:  Bill Wendling, 01. October 2001
 * Modifications:
 */
double
get_time(pio_time *pt, timer_type t)
{
    return pt->total_time[t];
}

#endif /* H5_HAVE_PARALLEL */
