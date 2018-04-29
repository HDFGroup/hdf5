/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef IO_TIMER__
#define IO_TIMER__

#include "hdf5.h"

#if defined(H5_TIME_WITH_SYS_TIME)
#   include <sys/time.h>
#   include <time.h>
#elif defined(H5_HAVE_SYS_TIME_H)
#   include <sys/time.h>
#else
#   include <time.h>
#endif

#ifdef H5_HAVE_WINSOCK2_H
#  include <winsock2.h>
#endif /* H5_HAVE_WINSOCK2_H */
/* The different types of timers we can have */
typedef enum timer_type_ {
    HDF5_FILE_OPENCLOSE,
    HDF5_DATASET_CREATE,
    HDF5_MPI_WRITE,
    HDF5_MPI_READ,
    HDF5_FILE_READ_OPEN,
    HDF5_FILE_READ_CLOSE,
    HDF5_FILE_WRITE_OPEN,
    HDF5_FILE_WRITE_CLOSE,
    HDF5_FINE_WRITE_FIXED_DIMS,
    HDF5_FINE_READ_FIXED_DIMS,
    HDF5_GROSS_WRITE_FIXED_DIMS,
    HDF5_GROSS_READ_FIXED_DIMS,
    HDF5_RAW_WRITE_FIXED_DIMS,
    HDF5_RAW_READ_FIXED_DIMS,
    NUM_TIMERS
} timer_type;

typedef enum clock_type_ {
    SYS_CLOCK = 0,   /* Use system clock to measure time     */
    MPI_CLOCK = 1    /* Use MPI clock to measure time        */
} clock_type;

/* Miscellaneous identifiers */
enum {
    TSTART,          /* Start a specified timer              */
    TSTOP            /* Stop a specified timer               */
};

/* The performance time structure */
typedef struct io_time_t {
    clock_type type;
    double total_time[NUM_TIMERS];
    double mpi_timer[NUM_TIMERS];
    struct timeval sys_timer[NUM_TIMERS];
} io_time_t;

/* External function declarations */
#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
H5TOOLS_DLL io_time_t   *io_time_new(clock_type t);
H5TOOLS_DLL void         io_time_destroy(io_time_t *pt);
H5TOOLS_DLL io_time_t   *set_time(io_time_t *pt, timer_type t, int start_stop);
H5TOOLS_DLL double       get_time(io_time_t *pt, timer_type t);
#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* IO_TIMER__ */
