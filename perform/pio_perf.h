/*
 * Copyright (C) 2001
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 *
 */
#ifndef PIO_PERF_H__
#define PIO_PERF_H__

#include "pio_timer.h"

typedef enum iotype_ {
    RAW,
    MPIO,
    PHDF5
    /*NUM_TYPES*/
} iotype;

typedef struct parameters_ {
    unsigned int max_num_procs;     /* Maximum number of processes to use   */
    iotype io_type;                 /* The type of IO test to perform       */
    unsigned int num_files;         /* Number of files to create            */
    unsigned long num_dsets;        /* Number of datasets to create         */
    unsigned long num_elmts;        /* Number of native ints in each dset   */
    unsigned int num_iters;         /* Number of times to loop doing the IO */
} parameters;

typedef struct results_ {
    herr_t      ret_code;
    pio_time   *timers;
} results;

#ifndef SUCCESS
#define SUCCESS     0
#endif  /* !SUCCESS */

#ifndef FAIL
#define FAIL        -1
#endif  /* !FAIL */

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
extern results do_pio(parameters param);
#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* PIO_PERF_H__ */
