/* $Id$ */

#ifndef PHDF5TEST_H
#define PHDF5TEST_H

#include <assert.h>
#include <hdf5.h>
#include <H5private.h>
#include <mpi.h>
#include <mpio.h>

/* Define some handy debugging shorthands, routines, ... */
/* debugging tools */

#define MESG(x)								      \
	if (verbose) printf("%s\n", x);					      \

#define VRFY(val, mesg) do {                                                  \
    if (val) {                                                                \
	if (*mesg != '\0'){						      \
	    MESG(mesg);							      \
	}								      \
    }								      	      \
    else{								      \
        printf("*** Assertion failed (%s) at line %4d in %s\n",               \
	    mesg, (int)__LINE__, __FILE__);     			      \
        nerrors++;                                                            \
        H5Eprint (stdout);                                                    \
	if (!verbose) exit(nerrors);					      \
    }                                                                         \
    H5Eclear();                                                               \
} while(0)

#define MPI_BANNER(mesg)\
    {printf("--------------------------------\n");\
    printf("Proc %d: ", mpi_rank); \
    printf("*** %s\n", mesg);\
    printf("--------------------------------\n");}

#define MAINPROCESS	(!mpi_rank)	/* define process 0 as main process */

#define SYNC(comm)\
    {MPI_BANNER("doing a SYNC"); MPI_Barrier(comm); MPI_BANNER("SYNC DONE");}
/* End of Define some handy debugging shorthands, routines, ... */

/* Constants definitions */
/* DIM1 and DIM2 must be multiples of mpi-sizes to be used. */
/* E.g. 24, a multiple of 2, 3, 4, 6, 8, 12, would be a good choice. */
#define DIM1		24
#define DIM2		24
#define RANK		2
#define DATASETNAME1	"Data1"
#define DATASETNAME2	"Data2"
#define DATASETNAME3	"Data3"
/* hyperslab layout styles */
#define BYROW		1	/* divide into slabs of rows */
#define BYCOL		2	/* divide into blocks of columns */


/* dataset data type.  Int's can be easily octo dumped. */
typedef int DATATYPE;

/* shared global variables */
extern int nerrors;				/* errors count */
extern int verbose;				/* verbose, default as no. */
extern herr_t (*old_func)(void*);		/* previous error handler */
extern void *old_client_data;			/* previous error handler arg.*/

#endif /* PHDF5TEST_H */
