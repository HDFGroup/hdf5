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
    iotype	io_type;        /* The type of IO test to perform       */
    int		num_procs;      /* Maximum number of processes to use   */
    int		num_files;      /* Number of files to create            */
    long	num_dsets;      /* Number of datasets to create         */
    long	num_elmts;      /* Number of native ints in each dset   */
    int		num_iters;      /* Number of times to loop doing the IO */
    long 	buf_size;       /* Buffer size                          */
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

extern MPI_Comm pio_comm_g;         /* Communicator to run the PIO          */
extern int      pio_mpi_rank_g;     /* MPI rank of pio_comm_g               */
extern int	    pio_mpi_nprocs_g;   /* number of processes of pio_comm_g    */

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
extern results do_pio(parameters param);
#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* PIO_PERF_H__ */
