/* $Id$ */

#ifndef PHDF5TEST_H
#define PHDF5TEST_H

#include <assert.h>
#include <stdlib.h>
#include <hdf5.h>
#include <h5test.h>

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
	printf("Proc %d: ", mpi_rank); \
        printf("*** PHDF5 Assertion failed (%s) at line %4d in %s\n",         \
	    mesg, (int)__LINE__, __FILE__);     			      \
        nerrors++;                                                            \
	fflush(stdout);							      \
	if (!verbose){							      \
	    printf("aborting MPI process\n"); \
	    MPI_Finalize(); exit(nerrors); \
	}								      \
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
#define DIM0		600 	/* Default dataset sizes. */
#define DIM1		800	/* Values are from a monitor pixel sizes */
#define RANK		2
#define DATASETNAME1	"Data1"
#define DATASETNAME2	"Data2"
#define DATASETNAME3	"Data3"
/* hyperslab layout styles */
#define BYROW		1	/* divide into slabs of rows */
#define BYCOL		2	/* divide into blocks of columns */
#define ZROW		3	/* same as BYCOL except process 0 gets 0 rows */
#define ZCOL		4	/* same as BYCOL except process 0 gets 0 columns */


/* dataset data type.  Int's can be easily octo dumped. */
typedef int DATATYPE;

/* shared global variables */
extern int dim0, dim1;				/*Dataset dimensions */
extern int chunkdim0, chunkdim1;		/*Chunk dimensions */
extern int nerrors;				/*errors count */
extern int verbose;				/*verbose, default as no. */
extern herr_t (*old_func)(void*);		/*previous error handler */
extern void *old_client_data;			/*previous error handler arg.*/

#endif /* PHDF5TEST_H */
