/*
 * Copyright (C) 2001
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 *
 */
#ifndef PERF_TIMER__
#define PERF_TIMER__

#if 0

#if defined(H5_TIME_WITH_SYS_TIME)
#   include <sys/time.h>
#   include <time.h>
#elif defined(H5_HAVE_SYS_TIME_H)
#   include <sys/time.h>
#else
#   include <time.h>
#endif

#else

#include <sys/time.h>
#include <time.h>

#endif  /* 0 */

/* The different types of timers we can have */
typedef enum timer_type_ {
    HDF5_MPI_OVERHEAD,
    HDF5_FILE_OPENCLOSE,
    HDF5_GROUP_CREATE,
    HDF5_DATASET_CREATE,
    HDF5_WRITE_FIXED_DIMS,
    HDF5_READ_FIXED_DIMS,
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
typedef struct perf_time_ {
    unsigned int type : 1;
    double total_time[NUM_TIMERS];
    double mpi_timer[NUM_TIMERS];
    struct timeval sys_timer[NUM_TIMERS];
} perf_time;

/* External function declarations */
#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
extern perf_time   *perf_time_new(unsigned int);
extern void         perf_time_destroy(perf_time *pt);
extern void         set_timer_type(perf_time *pt, timer_type type);
extern timer_type   get_timer_type(perf_time *pt);
extern perf_time   *set_time(perf_time *pt, timer_type t, int start_stop);
extern double       get_time(perf_time *pt, timer_type t);
#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* PERF_TIMER__ */
