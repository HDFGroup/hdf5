/*
 * Copyright (C) 2001
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 *
 */
#ifndef PIO_TIMER__
#define PIO_TIMER__

#include "hdf5.h"

#if defined(H5_TIME_WITH_SYS_TIME)
#   include <sys/time.h>
#   include <time.h>
#elif defined(H5_HAVE_SYS_TIME_H)
#   include <sys/time.h>
#else
#   include <time.h>
#endif

/* The different types of timers we can have */
typedef enum timer_type_ {
    HDF5_FILE_OPENCLOSE,
    HDF5_DATASET_CREATE,
    HDF5_FINE_WRITE_FIXED_DIMS,
    HDF5_FINE_READ_FIXED_DIMS,
    HDF5_GROSS_WRITE_FIXED_DIMS,
    HDF5_GROSS_READ_FIXED_DIMS,
    NUM_TIMERS
} timer_type;

/* Miscellaneous identifiers */
enum {
    MPI_TIMER = 0,  /* Use MPI timer to measure time        */
    SYS_TIMER = 1,  /* Use system clock to measure time     */

    START,          /* Start a specified timer              */
    STOP            /* Stop a specified timer               */
};

/* The performance time structure */
typedef struct pio_time_ {
    unsigned int type : 1;
    double total_time[NUM_TIMERS];
    double mpi_timer[NUM_TIMERS];
    struct timeval sys_timer[NUM_TIMERS];
} pio_time;

/* External function declarations */
#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
extern pio_time    *pio_time_new(unsigned int);
extern void         pio_time_destroy(pio_time *pt);
extern void         set_timer_type(pio_time *pt, timer_type type);
extern timer_type   get_timer_type(pio_time *pt);
extern pio_time    *set_time(pio_time *pt, timer_type t, int start_stop);
extern double       get_time(pio_time *pt, timer_type t);
#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* PIO_TIMER__ */
