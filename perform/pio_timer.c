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

#include "pio_perf.h"

/*
 * The number to divide the tv_usec field with to get a nice decimal to add to
 * the number of seconds.
 */
#define MICROSECOND     1000000.0

/* global variables */
pio_time   *timer_g;            /* timer: global for stub functions     */

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
    register int i;

    /* set global timer variable */
    timer_g = pt;
    for (i = 0; i < NUM_TIMERS; ++i)
        pt->total_time[i] = 0.0;

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
    /* reset the global timer pointer too. */
    timer_g = NULL;
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
                pt->total_time[t] +=
                    ((double)sys_t.tv_sec +
                                ((double)sys_t.tv_usec) / MICROSECOND) -
                    ((double)pt->sys_timer[t].tv_sec +
                            ((double)pt->sys_timer[t].tv_usec) / MICROSECOND);
            }
        }
    }

    if (pio_debug_level >= 4) {
        char *msg;
        int myrank;

        MPI_Comm_rank(pio_comm_g, &myrank);

        switch (t) {
        case HDF5_FILE_OPENCLOSE:
            msg = "File Open/Close";
            break;
        case HDF5_DATASET_CREATE:
            msg = "Dataset Create";
            break;
        case HDF5_MPI_WRITE:
            msg = "MPI Write";
            break;
        case HDF5_MPI_READ:
            msg = "MPI Read";
            break;
        case HDF5_FINE_WRITE_FIXED_DIMS:
            msg = "Fine Write";
            break;
        case HDF5_FINE_READ_FIXED_DIMS:
            msg = "Fine Read";
            break;
        case HDF5_GROSS_WRITE_FIXED_DIMS:
            msg = "Gross Write";
            break;
        case HDF5_GROSS_READ_FIXED_DIMS:
            msg = "Gross Read";
            break;
        default:
            msg = "Unknown Timer";
            break;
        }

        fprintf(output, "    Proc %d: %s %s: %.2f\n", myrank, msg,
                (start_stop == START ? "Start" : "Stop"),
                pt->total_time[t]);
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
