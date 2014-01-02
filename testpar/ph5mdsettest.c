/*
 * Example of using the parallel HDF5 library to access datasets.
 *
 * This program contains two parts.  
 * In the first part, the mpi processes
 * collectively create a new parallel HDF5 file and create two fixed
 * dimension datasets in it.  Then each process writes a hyperslab into
 * each dataset in an independent mode.  All processes collectively
 * close the datasets and the file.
 *
 * In the second part, the processes collectively open the created file
 * and the two datasets in it.  Then each process reads a hyperslab from
 * each dataset in an independent mode and prints them out.
 * All processes collectively close the datasets and the file.
 *
 * The need of requirement of parallel file prefix is that in general
 * the current working directory in which compiling is done, is not suitable
 * for parallel I/O and there is no standard pathname for parallel file
 * systems.  In some cases, the parallel file name may even needs some
 * parallel file type prefix such as: "pfs:/GF/...".  Therefore, this
 * example requires an explicite parallel file prefix.  See the usage
 * for more detail.
 */

#include <assert.h>
#include "hdf5.h"
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>


/* Swtich Test type between 'Feature test = 1' or 'Performance test = 0' */
const int TEST_TYPE=1;


/*
 * Test Options
 * - comment or uncomment 
 */
/* Test multi-dset Write/Read in serial mode (without MPI) */
/* note: single-dset is testing on koala or ostrich */
/* #define TEST_NO_MPI */


/* Test No slection on a last dset from a 2nd process 
 * This is not related space selection. relate to dset selection.
 * This applies to all feature tests */
#define TEST_MDSET_NO_LAST_DSET_2ND_PROC

/* Test multiple multi-dset Write/Read before H5Dclose 
 * If comment cout, only read/write singe time */
#define TEST_DOUBLE_WR_BEFORE_CLOSE
#define TEST_DOUBLE_RD_BEFORE_CLOSE

/*
 * Control number of datasets
 * This is more of debugging purpose, when want to test with single dataset.
 */
/* Only 1 or 2 CONTIG dset allowed */
#define NDSET_CONTIG 2
#define TEST_TWO_CONTIG 
/* Only 1 or 2 CHUNK dset allowed */
#define NDSET_CHUNK 2
#define TEST_TWO_CHUNK 

/* number of all dsets. 2 CHUNKED 2 CONTIG */
#define NDSET   4

/* performance test rank */
#define RANK_PERF 1 


/* initial value to fill read/write buffer */
#define INIT_VAL 9

/* 
 * These are parameter values for test functions.
 * Can be combined for various tests
 */
typedef enum phdf5_mode_t {
    PHDF5_SERIAL = 0,  /* NO MPI */
    PHDF5_PARALLEL     /* MPI */
} phdf5_mode_t;

/* Type of collective I/O in parallel mode */
typedef enum mpio_collective_mode_t {
    MPIO_COLLECTIVE_IO = 0,
    MPIO_INDIVIDUAL_IO 
} mpio_collective_mode_t;

typedef enum sel_mode_t {
    SEL_HYPER_1BLOCK = 0,    /* select single block in a piece */
    SEL_HYPER_BLOCKS,        /* select multiple blocks in a piece */
    SEL_NONE,                /* select none in a piece */
    SEL_POINTS,              /* select multiple points in a piece */
    SEL_1POINT               /* select single point in a piece */
} sel_mode_t;

typedef enum multi_mode_t {
    MULTI_DSET = 0, 		
    SINGLE_DSET
} multi_mode_t;



/* Temporary source code */
#define FAIL -1
/* temporary code end */

/* Define some handy debugging shorthands, routines, ... */
/* debugging tools */
#define MESG(x)\
	if (verbose) printf("%s\n", x);\

#define MPI_BANNER(mesg)\
    {printf("--------------------------------\n");\
    printf("Proc %d: ", mpi_rank); \
    printf("*** %s\n", mesg);\
    printf("--------------------------------\n");}
/* End of Define some handy debugging shorthands, routines, ... */


/* Constants definitions */
/* Use even number which can be divided by 2 processes */
#define SPACE1_DIM1 6 
#define SPACE1_DIM2 4

#define SPACE1_RANK	2

#define NPOINT 2  /* two points selection */

#define DATASETNAME1	"d1"
#define DATASETNAME2	"d2"
#define DATASETNAME3	"d3"
#define DATASETNAME4	"d4"

//
/* hyperslab layout styles */
#define BYROW		1	/* divide into slabs of rows */
#define BYCOL		2	/* divide into blocks of columns */
#define BYROW2		3	/* divide into slabs of rows (1 row small each side) */
#define BYCOL2		4	/* divide into blocks of columns (1 col small each side) */
#define BYROW_M     6   /* multiple partial selections in a chunk (piece) */
#define BYCOL_M     7   /* multiple partial selections in a chunk (piece) */


#define PARAPREFIX	"HDF5_PARAPREFIX"	/* file prefix environment variable name */


/* dataset data type.  Int's can be easily octo dumped. */
typedef int DTYPE_INT;

/* global variables */
int nerrors = 0;				/* errors count */
#ifndef PATH_MAX
#define PATH_MAX    512
#endif  /* !PATH_MAX */
#define TEST_MAX 30
char    testfiles[TEST_MAX][PATH_MAX];


int mpi_size, mpi_rank;				/* mpi variables */

/* option flags */
int doread=0;				/* read test */
int dowrite=0;				/* write test */
int docleanup=0;			/* cleanup */
int doCOLL=0;               /* parallel collective IO */
int doIND=0;                /* parallel independent IO */
int doSERIAL=0;             /* serial IO */
int verbose = 0;			/* verbose, default as no. */

multi_mode_t multi_mode_g = MULTI_DSET;  /* MULTI_DSET or SINGLE_DSET */
hsize_t dim0_g=0;
size_t ndset_g=0;
char fname_g[32]={0,};
hsize_t chunks_g=0;

/* Prototypes */
//void slab_set(hsize_t start[], hsize_t count[], hsize_t stride[], int mode);
void dataset_fill_2d(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT * dataset, int add);
void dataset_print(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT * dataset);
int dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT *dataset, DTYPE_INT *original, int dnum);
void test_split_comm_access(char filenames[][PATH_MAX]);
int parse_options(int argc, char **argv);
void usage(void);
int mkfilenames(char *prefix);
void cleanup(void);


void put_timeval (struct timeval *tv)
{
  long milliseconds;

  /* Compute milliseconds from microseconds.  */
  milliseconds = (*tv).tv_usec / 1000;
  /* Print the formatted time, in seconds, followed by a decimal point
     and the milliseconds.  */
  printf ("%ld.%03ld\n", (*tv).tv_sec, milliseconds);
}

static float timeval2float (struct timeval *tv)
{
    float ret_val= 0;
    ret_val = ((float)(*tv).tv_sec)*1000000 + (float)((*tv).tv_usec);
    ret_val = ret_val / 1000000;
    return ret_val;
}

static void float2timeval (float value, struct timeval *tv)
{
    int status=0;
    int sec = (int)value;
    (*tv).tv_sec = sec;
    (*tv).tv_usec = (value - sec)*1000000;
}

/*
 * Setup the dimensions of the hyperslab.
 * Two modes--by rows or by columns.
 * Assume dimension rank is 2.
 */
void
slab_set(hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[], int dmode)
{
    int l_mpi_rank = mpi_rank;

    switch (dmode){
    /* select ALL as one slab */
    case BYROW:
	/* Each process select all as one slab of rows. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1/mpi_size;
	count[1] = SPACE1_DIM2;
	start[0] = l_mpi_rank*count[0];
	start[1] = 0;
    block[0] = 1;
    block[1] = 1;
	break;
    case BYCOL:
	/* Each process select all as one slab of columns. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1;
	count[1] = SPACE1_DIM2/mpi_size;
	start[0] = 0;
	start[1] = l_mpi_rank*count[1];
    block[0] = 1;
    block[1] = 1;
	break;
    case BYROW2:
    /* Each process select all as one slab except first and last rows */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1/mpi_size;
	count[1] = SPACE1_DIM2 - 2;  // 2 rows (right/left) shrink
	start[0] = l_mpi_rank*count[0];
	start[1] = 0 + 1; // 1 row shift right
    block[0] = 1;
    block[1] = 1;
	break;
    case BYCOL2:
    /* Each process select all as one slab except first and last columns */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1 - 2;  // 2 cols (top/bottom) shrink
	count[1] = SPACE1_DIM2/mpi_size;
	start[0] = 0 + 1; // 1 col shift down
	start[1] = l_mpi_rank*count[1];
    block[0] = 1;
    block[1] = 1;
	break;
    case BYROW_M:  
	/* Each process takes multiple slabs each row. */
	start[0] = (SPACE1_DIM1/mpi_size) * l_mpi_rank;
	start[1] = 0;
    block[0] = 1;
    block[1] = SPACE1_DIM2/2 - 1;
	stride[0] = block[0] + 1;
	stride[1] = block[1] + 1;
	count[0] = (SPACE1_DIM1/mpi_size)/stride[0];
	count[1] = SPACE1_DIM2/stride[1];
    block[0] = 1;
    block[1] = 2;
	break;
    case BYCOL_M:  
	/* Each process takes multiple slabs each column. */
	start[0] = 0;
	start[1] = (SPACE1_DIM2/mpi_size) * l_mpi_rank;
    block[0] = SPACE1_DIM1/2 - 1;
    block[1] = 1;
	stride[0] = block[0] + 1;
	stride[1] = block[1] + 1;
	count[0] = SPACE1_DIM1/stride[0];
	count[1] = (SPACE1_DIM2/mpi_size)/stride[1];
    block[0] = 2;
    block[1] = 1;
	break;
    default:
	/* Unknown dmode.  Set it to cover the whole dataset. */
	printf("unknown slab_set dmode (%d)\n", dmode);
	start[0] = 0;
	start[1] = 0;
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1;
	count[1] = SPACE1_DIM2;
    block[0] = 1;
    block[1] = 1;
	break;
    }
}

void
slab_set_1d(hsize_t dims[], hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[], int dmode)

{
    int l_mpi_rank = mpi_rank;

    switch (dmode){
    case BYROW:
    /* Each process select all as one slab of rows. */
	stride[0] = 1;
	count[0] = dims[0]/mpi_size;
	start[0] = l_mpi_rank*count[0];
	break;

    case BYROW_M:  
    /* Each process takes multiple slabs each row. */
	start[0] = (dims[0]/mpi_size) * l_mpi_rank;
    block[0] = 2;
	stride[0] = block[0] + 1;
	count[0] = (dims[0]/mpi_size)/stride[0];
	break;
    default:
	/* Unknown dmode.  Set it to cover the whole dataset. */
	printf("unknown slab_set dmode (%d)\n", dmode);
	stride[0] = 1;
	count[0] = dims[0];
	start[0] = 0;
	break;
    }
}



/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2 and data is stored contiguous.
 */
void
dataset_fill_2d(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT * dataset, int add)
{
    DTYPE_INT *dataptr = dataset;
    hsize_t i, j, v=1;

#if 0 // JK_DBG
    printf("%s P%d > start[]=(%lu,%lu), count[]=(%lu,%lu), stride[]=(%lu,%lu)\n",
        __FUNCTION__, mpi_rank, 
	    (unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)stride[0],(unsigned long)stride[1]);
    fflush(stdout);
#endif

    /* put some trivial data in the data_array */
    for (i=0; i < count[0]; i++){
	    for (j=0; j < count[1]; j++){
	        *dataptr++ = mpi_rank+1 + add;
	    }
    }
}
void
dataset_fill_1d(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT * dataset, int add)
{
    DTYPE_INT *dataptr = dataset;
    hsize_t i, j, v=1;

#if 0 // JK_DBG
    printf("%s P%d > start[]=(%lu), count[]=(%lu), stride[]=(%lu)\n",
        __FUNCTION__, mpi_rank, 
	    (unsigned long)start[0], (unsigned long)count[0], (unsigned long)stride[0]);
    fflush(stdout);
#endif

    /* put some trivial data in the data_array */
    for (i=0; i < count[0]; i++){
	    //*dataptr++ = (i*stride[0]+start[0])*SPACE1_DIM2 + (j*stride[1]+start[1]+1);
	    *dataptr++ = mpi_rank + add;
    }
}

void
dataset_fill2_1d(hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[], DTYPE_INT * dataset, int add)
{
    DTYPE_INT *dataptr = NULL;
    hsize_t i;
    hsize_t y;

    dataptr = dataset;

#if 0 // JK_DBG
    printf("%s P%d > start[]=(%lu), stride[]=(%lu), count[]=(%lu), block[]=(%lu)\n",
        __FUNCTION__, mpi_rank, 
	    (unsigned long)start[0],
        (unsigned long)stride[0],
        (unsigned long)count[0],
        (unsigned long)block[0]);
    fflush(stdout);
#endif

    /* put some trivial data in the data_array */
    for (y=0; y< count[0]; y++)
      {
         hsize_t begin;
         begin = start[0] + stride[0] * y;
         for (i=0; i < block[0]; i++){
	        dataptr = dataset + (begin+i); //*SPACE1_DIM2;
            *dataptr = mpi_rank+1 + add;
         }
      }
}

void
dataset_fill2_2d(hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[], DTYPE_INT * dataset, int add)
{
    DTYPE_INT *dataptr = NULL;
    hsize_t i, j;
    hsize_t y,z;

    dataptr = dataset;

#if 0 // JK_DBG
    printf("%s P%d > start[]=(%lu,%lu), stride[]=(%lu,%lu), count[]=(%lu,%lu), block[]=(%lu,%lu)\n",
        __FUNCTION__, mpi_rank, 
	    (unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)stride[0],(unsigned long)stride[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)block[0],(unsigned long)block[1]);
    fflush(stdout);
#endif

    /* put some trivial data in the data_array */
    for (y=0; y< count[0]; y++)
      for (z=0; z < count[1]; z++)
      {
         hsize_t begin[2];
         begin[0] = start[0] + stride[0] * y;
         begin[1] = start[1] + stride[1] * z;
         for (i=0; i < block[0]; i++){
	       for (j=0; j < block[1]; j++){
	        dataptr = dataset + (begin[0]+i)*SPACE1_DIM2 + (begin[1]+j);
            *dataptr = mpi_rank+1 + add;
	       }
         }
      }
}
void
dataset_fill3_2d(hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[], DTYPE_INT * dataset, int add, int mode)
{
    DTYPE_INT *dataptr = NULL;
    hsize_t i, j;
    hsize_t y,z;

    dataptr = dataset;

#if 0 // JK_DBG
    printf("%s P%d > start[]=(%lu,%lu), stride[]=(%lu,%lu), count[]=(%lu,%lu), block[]=(%lu,%lu)\n",
        __FUNCTION__, mpi_rank, 
	    (unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)stride[0],(unsigned long)stride[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)block[0],(unsigned long)block[1]);
    fflush(stdout);
#endif

    /* put some trivial data in the data_array */

    for (y=0; y< count[0]; y++)
      for (z=0; z < count[1]; z++)
      {
         hsize_t begin[2];
         begin[0] = start[0] + stride[0] * y;
         begin[1] = start[1] + stride[1] * z;
         for (i=0; i < block[0]; i++){
	       for (j=0; j < block[1]; j++){
            if (mode==BYROW_M)
	            dataptr = dataset + (begin[0]+i) + (begin[1]+j)*SPACE1_DIM1;
            else if(mode==BYCOL_M)
	            dataptr = dataset + (begin[0]+i)*SPACE1_DIM2 + (begin[1]+j);
            *dataptr = mpi_rank+1 + add;
	       }
         }
      }
}


/*
 * Print the content of the dataset.
 */
void dataset_print(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT * dataset)
{
    DTYPE_INT *dataptr = dataset;
    hsize_t i, j;

    /* print the slab read */
    for (i=0; i < count[0]; i++){
	    printf("Col %lu: ", (unsigned long)(i*stride[0]+start[0]));
    	for (j=0; j < count[1]; j++){
	        printf("%d ", (int) *dataptr);
            dataptr++;
    	}
	    printf("\n");
    }
}

void dataset_print_1d(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT * dataset)
{
    DTYPE_INT *dataptr = dataset;
    hsize_t i;

    /* print the slab read */
    for (i=0; i < count[0]; i++){
	    printf("%d ", (int) *dataptr);
        dataptr++;
    }
	printf("\n");
}
/*
 * Print the content of selected slab by this process.
 */
void dset_select_display(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT * dataset, int dnum)
{
    DTYPE_INT *dataptr = dataset;
    hsize_t i, j;

#if 0 // JK_DBG
    printf("JKDBG P%d(%d) select display> dset%d \n",mpi_rank, getpid(), dnum);
#endif

    /* print the slab read */
    for (i=0; i < count[0]; i++){
    	for (j=0; j < count[1]; j++){
	        printf("%d ", (int) *dataptr);
            dataptr++;
    	}
	    printf("\n");
        fflush(stdout);
    }
    fflush(stdout);
}

/*
 * Print all dset.
 * Assume 2 dims
 */
void dset_display(hsize_t dim1, hsize_t dim2, DTYPE_INT * dataset, int dnum)
{
    DTYPE_INT *dataptr = dataset;
    hsize_t i, j;

#if 0 // JK_DBG
    printf("JKDBG P%d dset_display> dset %d \n",mpi_rank, dnum);
#endif

    /* print the slab read */
    for (i=0; i < dim1; i++){
	    //printf("Row %lu: ", (unsigned long)(i*stride[0]+start[0]));
    	for (j=0; j < dim2; j++){
	        printf("%d ",(int) *dataptr);
            dataptr++;
    	}
	    printf("\n");
        fflush(stdout);
    }
    fflush(stdout);
}

/*
 * Init data_array. 1 dim
 */
void data_array1d_init(hsize_t dim0, int * dataset, int val)
{
    int *dataptr = dataset;
    hsize_t i, j;

#if 0 // JK_DBG
    printf("JKDBG P%d data_array1d_init> val %d \n",mpi_rank, val);
#endif

    // print buffer 
    for (i=0; i < dim0; i++){
        *dataptr = val;
#if 0 // JK_DBG
	    printf("%d ", *dataptr);
#endif
        dataptr++;
    }
    printf("\n");
    fflush(stdout);
    
}

/*
 * Init data_array.
 * Assume 2 dims
 */
void data_array2d_init(hsize_t dim1, hsize_t dim2, DTYPE_INT * dataset, int val)
{
    DTYPE_INT *dataptr = dataset;
    hsize_t i, j;

#if 0 // JK_DBG
    printf("JKDBG P%d data_array2d_init> val %d \n",mpi_rank, val);
#endif

    // print buffer 
    for (i=0; i < dim1; i++){
    	for (j=0; j < dim2; j++){
            *dataptr = val;
#if 0 // JK_DBG
	        printf("%d ", *dataptr);
#endif
            dataptr++;
    	}
#if 0 // JK_DBG
	    printf("\n");
        fflush(stdout);
#endif
    }
    printf("\n");
    fflush(stdout);
    
}

void print_dsets_2d(hsize_t dim_col, hsize_t dim_row,DTYPE_INT *wbuf, DTYPE_INT *rbuf)
{
    int i=0,j=0;
    int nerr=0;
    DTYPE_INT *dataptr1;
    DTYPE_INT *dataptr2;

    for (i=0; i < dim_col; i++) {
        for (j=0; j< dim_row; j++) {
            dataptr1 = wbuf + i*dim_row + j;
            dataptr2 = rbuf + i*dim_row + j;
            printf("%d=%d ", *dataptr1, *dataptr2);
        }
        printf("\n");
    }
}


/*
 * Print the content of the dataset.
 */
int dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], DTYPE_INT *dataset, DTYPE_INT *original,int dnum)
{
#define MAX_ERR_REPORT	10		/* Maximum number of errors reported */

    hsize_t i, j;
    int nerr;

    /* print it if verbose */
    if (verbose)
	    dataset_print(start, count, stride, dataset);
#if 0 // JK_DBG
    printf("JKDBG P%d dataset_vrfy> c0:%lu c1:%lu\n", mpi_rank, (unsigned long) count[0], (unsigned long)count[1]);
	dataset_print(start, count, stride, dataset);
#endif

    nerr = 0;
    for (i=0; i < count[0]; i++) {
	    for (j=0; j < count[1]; j++) {
#if 0 // JK_DBG
            printf("<o:%d v:%d> ", (int) *original, (int) *dataset);
#endif
	        if ((int)*dataset != (int)*original) {
    		    nerr++;
	    	    if (nerr <= MAX_ERR_REPORT) {
		            printf("Dataset %d Verify failed : expect %d, got %d\n",
                    dnum, *original, *dataset);
    		    }
                dataset++;
                original++;
	        }
	    }
    }
#if 0 // JK_DBG
            printf("\n");
            fflush(stdout);
#endif
    if (nerr > MAX_ERR_REPORT)
	    printf("[more errors ...]\n");
    if (nerr)
	    printf("%d errors found in dataset_vrfy\n", nerr);
    return(nerr);
}

int dataset_vrfy2(hsize_t start[], hsize_t stride[], hsize_t count[], hsize_t block[], DTYPE_INT *original, DTYPE_INT *dataset,int dnum)
{
#define MAX_ERR_REPORT	10		/* Maximum number of errors reported */

    hsize_t i, j;
    hsize_t y,z;
    DTYPE_INT *dataptr1 = NULL;
    DTYPE_INT *dataptr2 = NULL;
    int nerr;

    dataptr1 = original; 
    dataptr2 = dataset;

    /* print it if verbose */
    if (verbose)
        print_dsets_2d(SPACE1_DIM1, SPACE1_DIM2, original, dataset);

#if 0 // JK_DBG
    printf("%s P%d > start[]=(%lu,%lu), stride[]=(%lu,%lu), count[]=(%lu,%lu), block[]=(%lu,%lu)\n",
        __FUNCTION__, mpi_rank, 
	    (unsigned long)start[0], (unsigned long)start[1],
        (unsigned long)stride[0],(unsigned long)stride[1],
        (unsigned long)count[0], (unsigned long)count[1],
        (unsigned long)block[0],(unsigned long)block[1]);
    fflush(stdout);
#endif

#if 0 // JK_DBG
    printf("JKDBG P%d dataset_vrfy> c0:%lu c1:%lu\n", mpi_rank, (unsigned long) count[0], (unsigned long)count[1]);
	dataset_print(start, count, stride, dataset);
#endif

    nerr = 0;
    for (y=0; y < count[0]; y++) {
	    for (z=0; z < count[1]; z++) {
            hsize_t begin[2];
            begin[0] = start[0] + stride[0] * y;
            begin[1] = start[1] + stride[1] * z;
            for (i=0; i < block[0]; i++) {
                for (j=0; j < block[1]; j++) {
                    dataptr1 = original + (begin[0]+i)*SPACE1_DIM2 + (begin[1]+j);
                    dataptr2 = dataset + (begin[0]+i)*SPACE1_DIM2 + (begin[1]+j);
#if 0 // JK_DBG
                    printf("<o:%d v:%d> ", (int) *dataptr1, (int) *dataptr2);
#endif
	                if ((int)*dataptr1 != (int)*dataptr2) {
    		            nerr++;
	    	            if (nerr <= MAX_ERR_REPORT) {
		                    printf("Dataset %d Verify failed : expect %d, got %d\n", dnum, *dataptr1, *dataptr2);
    		            }
	                }
                } // j loop
            } // i loop
	    } // z loop
    } // y loop

#if 0 // JK_DBG
    printf("\n");
    fflush(stdout);
#endif

    if (nerr > MAX_ERR_REPORT)
	    printf("[more errors ...]\n");
    if (nerr)
	    printf("%d errors found in dataset_vrfy\n", nerr);
    return(nerr);
}

int diff_datasets(hsize_t dim1, hsize_t dim2, DTYPE_INT *original, DTYPE_INT *dataset,int dnum)
{
    DTYPE_INT *dataptr1 = original;
    DTYPE_INT *dataptr2 = dataset;
    hsize_t i, j;
    int nerr=0;

#if 0  // JK_DBG
    printf("JKDBG P%d diff_datasets> DSET%d: dim1:%d, dim2:%d \n",mpi_rank, dnum, dim1,dim2);
#endif

#if 0 // JK_DBG
    print_dsets_2d(SPACE1_DIM1, SPACE1_DIM2, original, dataset);
#endif

    
    // print buffer 
    for (i=0; i < dim1; i++){
    	for (j=0; j < dim2; j++){
#if 0 // JK_DBG
	        printf("%d=%d ", *dataptr1, *dataptr2);
#endif
            if (*dataptr1 != *dataptr2) {
                nerr++;
	            printf("DIFF! <o:%d, n:%d> \n", *dataptr1, *dataptr2);
            }
            dataptr1++;
            dataptr2++;
    	}
#if 0 // JK_DBG
	    printf("\n");
        fflush(stdout);
#endif
    }
    printf("\n");
    fflush(stdout);

    return nerr;
}

int diff_dset_points(int pnt_num, hsize_t pcoords[][2], DTYPE_INT *wbuf, DTYPE_INT *rbuf)
{
    int i=0,j=0;
    int nerr=0;
    DTYPE_INT *dataptr1;
    DTYPE_INT *dataptr2;

    for (i=0; i< pnt_num; i++) {
#if 0 // JK_DBG
        printf("p%d_col: %llu, p%d_row: %llu \n", i, pcoords[i][0], i, pcoords[i][1]);
#endif
        dataptr1 = wbuf + i*pcoords[i][0] + pcoords[i][1];
        dataptr2 = rbuf + i*pcoords[i][0] + pcoords[i][1];
#if 0 // JK_DBG
        printf("JKDBG P%d> %d=%d \n", mpi_rank, *dataptr1, *dataptr2);
#endif
        if(*dataptr1 != *dataptr2) {
            nerr++;
	        printf("DIFF! <o:%d, n:%d> \n", *dataptr1, *dataptr2);
        }
    }

#if 0 // JK_DBG
    print_dsets_2d(SPACE1_DIM1, SPACE1_DIM2,wbuf, rbuf);
#endif
    return nerr;
}


/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
 * dataset. [Note: not so yet.  Datasets are of sizes DIM1xDIM2 and
 * each process controls a hyperslab within.]
 */


/* This test with two CONTIG and two CHUNKED dsets 
 * Perform Write
 */
void
phdf5Write_mdset_All(char *filename, phdf5_mode_t pmode, mpio_collective_mode_t mpio_opt, sel_mode_t sel_mode, multi_mode_t multi_mode)
{
    int i;
    int Count;
    hid_t fid;			/* HDF5 file IDs */
    hid_t sid;   		/* Dataspace ID */
    hsize_t dims[SPACE1_RANK] = {SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    H5D_rw_multi_t dset_info[NDSET];
    hid_t crp_plist1, crp_plist2;
    hid_t file_dataspace1, file_dataspace2;	/* File dataspace ID */
    hid_t file_dataspace3, file_dataspace4;	/* File dataspace ID */
    hid_t mem_dataspace1=-1, mem_dataspace2=-1;	/* memory dataspace ID */
    hid_t mem_dataspace3=-1, mem_dataspace4=-1;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hid_t dataset3, dataset4;	/* Dataset ID */
    hsize_t chunk_dims[SPACE1_RANK];	/* dataspace dim sizes */

    DTYPE_INT data_array1[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array2[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array3[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array4[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    hsize_t start1[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count1[SPACE1_RANK];  
    hsize_t stride1[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block1[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start2[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count2[SPACE1_RANK];  
    hsize_t stride2[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block2[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start3[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count3[SPACE1_RANK];  
    hsize_t stride3[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block3[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start4[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count4[SPACE1_RANK];  
    hsize_t stride4[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block4[SPACE1_RANK];	/* for hyperslab setting */

    /* point selection */
    hsize_t pcoord[NPOINT][SPACE1_RANK];

    herr_t ret;         	/* Generic return value */

#ifndef TEST_NO_MPI
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
#endif

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    // init dset_info[]
    for (i=0; i< NDSET ; i++) {
        memset(&dset_info[i], 0, sizeof(H5D_rw_multi_t));
    }

    // Init data_array
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array1[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array2[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array3[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array4[0][0],INIT_VAL);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl != FAIL);
    MESG("H5Pcreate access succeed");
#ifndef TEST_NO_MPI
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");
#endif

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    assert(fid != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    assert(ret != FAIL);


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality dsets */
    sid = H5Screate_simple (SPACE1_RANK, dims, NULL);
    assert (sid != FAIL);
    MESG("H5Screate_simple succeed");


    /* ==========================================
     * set up for chunked Dset1 */
    crp_plist1 = H5Pcreate(H5P_DATASET_CREATE);
    assert (crp_plist1 != FAIL);
    chunk_dims[0] = SPACE1_DIM1 / 2; // divede col by half
    chunk_dims[1] = SPACE1_DIM2;
    ret = H5Pset_chunk(crp_plist1, 2, chunk_dims);
    assert (ret != FAIL);


    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist1, H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dcreate2 succeed");

    // can close chunk prop
    ret = H5Pclose(crp_plist1);
    assert (ret != FAIL);

    /* ----------------------------------------
     * set up for chunked Dset2 */
    crp_plist2 = H5Pcreate(H5P_DATASET_CREATE);
    assert (crp_plist2 != FAIL);
    chunk_dims[0] = SPACE1_DIM1; 
    chunk_dims[1] = SPACE1_DIM2 / 2; // divede row by half
    ret = H5Pset_chunk(crp_plist2, 2, chunk_dims);
    assert (ret != FAIL);

    /* create another dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist2, H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dcreate2 2 succeed");

    // can close chunk prop
    ret = H5Pclose(crp_plist2);
    assert (ret != FAIL);

    /* ==========================================
     * set up for chunked Dset3 */
    /* create a dataset collectively */
    dataset3 = H5Dcreate2(fid, DATASETNAME3, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset3 != FAIL);
    MESG("H5Dcreate2 succeed");

    /* ==========================================
     * set up for chunked Dset4 */
    /* create a dataset collectively */
    dataset4 = H5Dcreate2(fid, DATASETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset4 != FAIL);
    MESG("H5Dcreate2 succeed");


    /*
     * Set up dimensions of the slab this process accesses.
     */

#if 1 // JK_DBG
	printf("== Write DSET 1 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        slab_set(start1, stride1, count1, block1, BYROW);
        //slab_set(start1, stride1, count1, block1, BYROW2);
        //slab_set(start1, stride1, count1, block1, BYCOL);
        //slab_set(start1, stride1, count1, block1, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start1, stride1, count1, block1, BYROW_M);
        slab_set(start1, stride1, count1, block1, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start1[]=(%lu,%lu), stride1[]=(%lu,%lu), count1[]=(%lu,%lu), block1[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start1[0], (unsigned long)start1[1],
       (unsigned long)stride1[0], (unsigned long)stride1[1],
       (unsigned long)count1[0], (unsigned long)count1[1],
       (unsigned long)block1[0], (unsigned long)block1[1]);
#endif
    
    /* create a file dataspace independently */
    file_dataspace1 = H5Dget_space (dataset1);
    assert(file_dataspace1 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple (SPACE1_RANK, count1, NULL);
        ret=H5Sselect_hyperslab(file_dataspace1, H5S_SELECT_SET, start1, stride1, count1, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace1 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace1, H5S_SELECT_SET, start1, stride1, count1, block1);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");

    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple (SPACE1_RANK, count1, NULL);
        ret=H5Sselect_none(file_dataspace1);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace1);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start1[0] = pcoord[0][0]; start1[1] = pcoord[0][1];
        stride1[0] = 1; stride1[1] = 1;
        count1[0] = 1; count1[1] = 2;
        block1[0] = 1; block1[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace1, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace1, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start1[0] = pcoord[0][0]; start1[1] = pcoord[0][1];
        stride1[0] = 1; stride1[1] = 1;
        count1[0] = 1; count1[1] = 1;
        block1[0] = 1; block1[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif
        ret = H5Sselect_elements (file_dataspace1, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace1, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start1, count1, stride1, &data_array1[0][0],0);
    else
        dataset_fill2_2d(start1, stride1, count1, block1, &data_array1[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start1, count1, stride1, &data_array1[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset1 Write Data ---\n");
    //dset_select_display(start1, count1, stride1, &data_array1[0][0],1);
    dataset_print(start1, count1, stride1, &data_array1[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array1[0][0], 1);
#endif


    /* write data collectively */
    //ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace1, file_dataspace1,
	//    xfer_plist, data_array1);
    //assert(ret != FAIL);
    //MESG("H5Dwrite succeed");



#if 1 // JK_DBG
	printf("== Write DSET 2 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start2, stride2, count2, block2, BYROW);
        //slab_set(start2, stride2, count2, block2, BYROW2);
        slab_set(start2, stride2, count2, block2, BYCOL);
        //slab_set(start2, stride2, count2, block2, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start2, stride2, count2, block2, BYROW_M);
        slab_set(start2, stride2, count2, block2, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start2[]=(%lu,%lu), stride2[]=(%lu,%lu), count2[]=(%lu,%lu), block2[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start2[0], (unsigned long)start2[1],
       (unsigned long)stride2[0], (unsigned long)stride2[1],
       (unsigned long)count2[0], (unsigned long)count2[1],
       (unsigned long)block2[0], (unsigned long)block2[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace2 = H5Dget_space (dataset2);
    assert(file_dataspace2 != FAIL);
    MESG("H5Dget_space succeed");

    if(sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple (SPACE1_RANK, count2, NULL);
        ret=H5Sselect_hyperslab(file_dataspace2, H5S_SELECT_SET, start2, stride2, count2, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if(sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace2 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace2, H5S_SELECT_SET, start2, stride2, count2, block2);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple (SPACE1_RANK, count2, NULL);
        ret=H5Sselect_none(file_dataspace2);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace2);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start2[0] = pcoord[0][0]; start2[1] = pcoord[0][1];
        stride2[0] = 1; stride2[1] = 1;
        count2[0] = 1; count2[1] = 2;
        block2[0] = 1; block2[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif
        ret = H5Sselect_elements (file_dataspace2, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace2, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start2[0] = pcoord[0][0]; start2[1] = pcoord[0][1];
        stride2[0] = 1; stride2[1] = 1;
        count2[0] = 1; count2[1] = 1;
        block2[0] = 1; block2[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif
        ret = H5Sselect_elements (file_dataspace2, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace2, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start2, count2, stride2, &data_array2[0][0],0);
    else
        dataset_fill2_2d(start2, stride2, count2, block2, &data_array2[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start2, count2, stride2, &data_array2[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset2 Write Data ---\n");
    //dset_select_display(start2, count2, stride2, &data_array2[0][0],2);
    dataset_print(start2, count2, stride2, &data_array2[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array2[0][0], 2);
#endif


#if 1 // JK_DBG
	printf("== Write DSET 3 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start3, stride3, count3, block3, BYROW);
        slab_set(start3, stride3, count3, block3, BYROW2);
        //slab_set(start3, stride3, count3, block3, BYCOL);
        //slab_set(start3, stride3, count3, block3, BYCOL2);
    }
    else
    {
        /* each process takes a block */
        //slab_set(start3, stride3, count3, block3, BYROW_M);
        slab_set(start3, stride3, count3, block3, BYCOL_M);
    }


#if 0 // JK_DBG
    printf("%s P%d > start3[]=(%lu,%lu), stride3[]=(%lu,%lu), count3[]=(%lu,%lu), block3[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start3[0], (unsigned long)start3[1],
       (unsigned long)stride3[0], (unsigned long)stride3[1],
       (unsigned long)count3[0], (unsigned long)count3[1],
       (unsigned long)block3[0], (unsigned long)block3[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace3 = H5Dget_space (dataset3);
    assert(file_dataspace3 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple (SPACE1_RANK, count3, NULL);
        ret=H5Sselect_hyperslab(file_dataspace3, H5S_SELECT_SET, start3, stride3, count3, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace3 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace3, H5S_SELECT_SET, start3, stride3, count3, block3);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple (SPACE1_RANK, count3, NULL);
        ret=H5Sselect_none(file_dataspace3);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace3);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start3[0] = pcoord[0][0]; start3[1] = pcoord[0][1];
        stride3[0] = 1; stride3[1] = 1;
        count3[0] = 1; count3[1] = 2;
        block3[0] = 1; block3[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif
        ret = H5Sselect_elements (file_dataspace3, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace3, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start3[0] = pcoord[0][0]; start3[1] = pcoord[0][1];
        stride3[0] = 1; stride3[1] = 1;
        count3[0] = 1; count3[1] = 1;
        block3[0] = 1; block3[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif
        ret = H5Sselect_elements (file_dataspace3, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace3, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start3, count3, stride3, &data_array3[0][0],0);
    else
        dataset_fill2_2d(start3, stride3, count3, block3, &data_array3[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start3, count3, stride3, &data_array3[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset3 Write Data ---\n");
    //dset_select_display(start3, count3, stride3, &data_array3[0][0],0);
   	dataset_print(start3, count3, stride3, &data_array3[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array3[0][0], 0);
#endif

#if 1 // JK_DBG
	printf("== Write DSET 4 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start4, stride4, count4, block4, BYROW);
        //slab_set(start4, stride4, count4, block4, BYROW2);
        //slab_set(start4, stride4, count4, block4, BYCOL);
        slab_set(start4, stride4, count4, block4, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start4, stride4, count4, block4, BYROW_M);
        slab_set(start4, stride4, count4, block4, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start4[]=(%lu,%lu), stride4[]=(%lu,%lu), count4[]=(%lu,%lu), block4[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start4[0], (unsigned long)start4[1],
       (unsigned long)stride4[0], (unsigned long)stride4[1],
       (unsigned long)count4[0], (unsigned long)count4[1],
       (unsigned long)block4[0], (unsigned long)block4[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace4 = H5Dget_space (dataset4);
    assert(file_dataspace4 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple (SPACE1_RANK, count4, NULL);
        ret=H5Sselect_hyperslab(file_dataspace4, H5S_SELECT_SET, start4, stride4, count4, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if(sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace4 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace4, H5S_SELECT_SET, start4, stride4, count4, block4);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple (SPACE1_RANK, count4, NULL);
        ret=H5Sselect_none(file_dataspace4);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace4);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start4[0] = pcoord[0][0]; start4[1] = pcoord[0][1];
        stride4[0] = 1; stride4[1] = 1;
        count4[0] = 1; count4[1] = 2;
        block4[0] = 1; block4[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif
        ret = H5Sselect_elements (file_dataspace4, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace4, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start4[0] = pcoord[0][0]; start4[1] = pcoord[0][1];
        stride4[0] = 1; stride4[1] = 1;
        count4[0] = 1; count4[1] = 1;
        block4[0] = 1; block4[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif
        ret = H5Sselect_elements (file_dataspace4, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace4, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start4, count4, stride4, &data_array4[0][0],0);
    else
        dataset_fill2_2d(start4, stride4, count4, block4, &data_array4[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start4, count4, stride4, &data_array4[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset4 Write Data ---\n");
    //dset_select_display(start4, count4, stride4, &data_array4[0][0],0);
    dataset_print(start4, count4, stride4, &data_array4[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array4[0][0], 0);
#endif



    /*==================================================
     * set up the collective transfer properties list 
     */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    MESG("H5Pcreate xfer succeed");

#ifndef TEST_NO_MPI
    if (pmode == PHDF5_PARALLEL)
    {
        ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        assert(ret != FAIL);

        /* In parallel mode, do collective IO (MPI_File_write_at_all) */
        if (mpio_opt == MPIO_COLLECTIVE_IO) {
            /* Note: this is default, so don't need to set explicitly */
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_COLLECTIVE_IO);
            assert(ret >= 0);

            /* Note: this is default, so don't need to set explicitly 
             * Just leave this here for a reference if add more options later */
            /*
            ret = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
            assert(ret >= 0);
            */
        }
        /* In parallel mode, do independent IO (MPI_File_write_at) */
        else if (mpio_opt == MPIO_INDIVIDUAL_IO) {
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
            assert(ret >= 0);
        }
    }
#endif

    /*
     * Set up dset info structure 
     * HDF5 lib will handle by order of dset_info[X] X
     */
#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC //------------------------I
    if (mpi_rank == 0) {
#if 1 // USE_DSET1
    // init H5D_rw_multi_t for write DSET1 (CHUNKED)
    dset_info[0].dset_id = dataset1;
    dset_info[0].mem_type_id = H5T_NATIVE_INT;
    dset_info[0].mem_space_id = mem_dataspace1;
    dset_info[0].dset_space_id = file_dataspace1;
    dset_info[0].u.wbuf = &data_array1[0][0];
#else
    /* free before rewrite DSET1 */
    if(mem_dataspace1)  { ret = H5Sclose(mem_dataspace1); assert(ret != FAIL); }
    if(file_dataspace1) { ret = H5Sclose(file_dataspace1); assert(ret != FAIL); }
    if(dataset1) { ret = H5Dclose(dataset1); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET2
    // init H5D_rw_multi_t for write DSET2 (CHUNKED)
    dset_info[1].dset_id = dataset2;
    dset_info[1].mem_type_id = H5T_NATIVE_INT;
    dset_info[1].mem_space_id = mem_dataspace2;
    dset_info[1].dset_space_id = file_dataspace2;
    dset_info[1].u.wbuf = &data_array2[0][0];
#else
    /* free before rewrite DSET2 */
    if(mem_dataspace2)  { ret = H5Sclose(mem_dataspace2); assert(ret != FAIL); }
    if(file_dataspace2) { ret = H5Sclose(file_dataspace2); assert(ret != FAIL); }
    if(dataset2) { ret = H5Dclose(dataset2); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET3
    // init H5D_rw_multi_t for write DSET3 (CONTIG)
    dset_info[2].dset_id = dataset3;
    dset_info[2].mem_type_id = H5T_NATIVE_INT;
    dset_info[2].mem_space_id = mem_dataspace3;
    dset_info[2].dset_space_id = file_dataspace3;
    dset_info[2].u.wbuf = &data_array3[0][0];
#else
    /* free before rewrite DSET3 */
    if(mem_dataspace3)  { ret = H5Sclose(mem_dataspace3); assert(ret != FAIL); }
    if(file_dataspace3) { ret = H5Sclose(file_dataspace3); assert(ret != FAIL); }
    if(dataset3) { ret = H5Dclose(dataset3); assert(ret != FAIL); }
#endif
    
#if 1 // USE_DSET4
    // init H5D_rw_multi_t for write DSET4 (CONTIG)
    dset_info[3].dset_id = dataset4;
    dset_info[3].mem_type_id = H5T_NATIVE_INT;
    dset_info[3].mem_space_id = mem_dataspace4;
    dset_info[3].dset_space_id = file_dataspace4;
    dset_info[3].u.wbuf = &data_array4[0][0];
#else
    /* free before rewrite DSET4 */
    if(mem_dataspace4)  { ret = H5Sclose(mem_dataspace4); assert(ret != FAIL); }
    if(file_dataspace4) { ret = H5Sclose(file_dataspace4); assert(ret != FAIL); }
    if(dataset4) { ret = H5Dclose(dataset4); assert(ret != FAIL); }
#endif
    }

    if (mpi_rank == 1) {
#if 1 // USE_DSET1
    // init H5D_rw_multi_t for write DSET1 (CHUNKED)
    dset_info[0].dset_id = dataset1;
    dset_info[0].mem_type_id = H5T_NATIVE_INT;
    dset_info[0].mem_space_id = mem_dataspace1;
    dset_info[0].dset_space_id = file_dataspace1;
    dset_info[0].u.wbuf = &data_array1[0][0];
#else
    /* free before rewrite DSET1 */
    if(mem_dataspace1)  { ret = H5Sclose(mem_dataspace1); assert(ret != FAIL); }
    if(file_dataspace1) { ret = H5Sclose(file_dataspace1); assert(ret != FAIL); }
    if(dataset1) { ret = H5Dclose(dataset1); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET2
    // init H5D_rw_multi_t for write DSET2 (CHUNKED)
    dset_info[1].dset_id = dataset2;
    dset_info[1].mem_type_id = H5T_NATIVE_INT;
    dset_info[1].mem_space_id = mem_dataspace2;
    dset_info[1].dset_space_id = file_dataspace2;
    dset_info[1].u.wbuf = &data_array2[0][0];
#else
    /* free before rewrite DSET2 */
    if(mem_dataspace2)  { ret = H5Sclose(mem_dataspace2); assert(ret != FAIL); }
    if(file_dataspace2) { ret = H5Sclose(file_dataspace2); assert(ret != FAIL); }
    if(dataset2) { ret = H5Dclose(dataset2); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET3
    // init H5D_rw_multi_t for write DSET3 (CONTIG)
    dset_info[2].dset_id = dataset3;
    dset_info[2].mem_type_id = H5T_NATIVE_INT;
    dset_info[2].mem_space_id = mem_dataspace3;
    dset_info[2].dset_space_id = file_dataspace3;
    dset_info[2].u.wbuf = &data_array3[0][0];
#else
    /* free before rewrite DSET3 */
    if(mem_dataspace3)  { ret = H5Sclose(mem_dataspace3); assert(ret != FAIL); }
    if(file_dataspace3) { ret = H5Sclose(file_dataspace3); assert(ret != FAIL); }
    if(dataset3) { ret = H5Dclose(dataset3); assert(ret != FAIL); }
#endif
    
#if 0 // USE_DSET4
    // init H5D_rw_multi_t for write DSET4 (CONTIG)
    dset_info[3].dset_id = dataset4;
    dset_info[3].mem_type_id = H5T_NATIVE_INT;
    dset_info[3].mem_space_id = mem_dataspace4;
    dset_info[3].dset_space_id = file_dataspace4;
    dset_info[3].u.wbuf = &data_array4[0][0];
#else
    /* free before rewrite DSET4 */
    if(mem_dataspace4)  { ret = H5Sclose(mem_dataspace4); assert(ret != FAIL); }
    if(file_dataspace4) { ret = H5Sclose(file_dataspace4); assert(ret != FAIL); }
    if(dataset4) { ret = H5Dclose(dataset4); assert(ret != FAIL); }
#endif
    }

#else  // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------------------
    // init H5D_rw_multi_t for write DSET1 (CHUNKED)
    dset_info[0].dset_id = dataset1;
    dset_info[0].mem_type_id = H5T_NATIVE_INT;
    dset_info[0].mem_space_id = mem_dataspace1;
    dset_info[0].dset_space_id = file_dataspace1;
    dset_info[0].u.wbuf = &data_array1[0][0];

    // init H5D_rw_multi_t for write DSET2 (CHUNKED)
    dset_info[1].dset_id = dataset2;
    dset_info[1].mem_type_id = H5T_NATIVE_INT;
    dset_info[1].mem_space_id = mem_dataspace2;
    dset_info[1].dset_space_id = file_dataspace2;
    dset_info[1].u.wbuf = &data_array2[0][0];

    // init H5D_rw_multi_t for write DSET3 (CONTIG)
    dset_info[2].dset_id = dataset3;
    dset_info[2].mem_type_id = H5T_NATIVE_INT;
    dset_info[2].mem_space_id = mem_dataspace3;
    dset_info[2].dset_space_id = file_dataspace3;
    dset_info[2].u.wbuf = &data_array3[0][0];

    // init H5D_rw_multi_t for write DSET4 (CONTIG)
    dset_info[3].dset_id = dataset4;
    dset_info[3].mem_type_id = H5T_NATIVE_INT;
    dset_info[3].mem_space_id = mem_dataspace4;
    dset_info[3].dset_space_id = file_dataspace4;
    dset_info[3].u.wbuf = &data_array4[0][0];
#endif // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------------------O

#if 0 // JK_DBG
    for (i=0; i< NDSET ; i++)
        printf("%s P%d > dset_info[%d].u.wbuf addr: %x\n", __FUNCTION__, mpi_rank, i, dset_info[i].u.wbuf);
#endif

#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC
    if (mpi_rank == 0)      Count = 4;
    if (mpi_rank == 1)      Count = 3;
#else
    Count = NDSET;
#endif

    if(multi_mode==MULTI_DSET) {
        ret = H5Dwrite_multi(fid, xfer_plist, Count, dset_info);
        assert(ret != FAIL);
#ifdef TEST_DOUBLE_WR_BEFORE_CLOSE
        /* test multiple write before close */
sync();
        ret = H5Dwrite_multi(fid, xfer_plist, Count, dset_info);
        assert(ret != FAIL);
#endif
    }
    else if(multi_mode==SINGLE_DSET) {
        for (i=0; i<Count; i++)
            ret = H5Dwrite(dset_info[i].dset_id, dset_info[i].mem_type_id, dset_info[i].mem_space_id, dset_info[i].dset_space_id, xfer_plist, dset_info[i].u.wbuf);
    }

    MESG("H5Dwrite succeed");

    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    /* release all temporary handles. */
    for (i=0;i< Count;i++)
    {
        if(dset_info[i].mem_space_id > 0) {
            ret = H5Sclose(dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(dset_info[i].dset_space_id > 0) {
            ret = H5Sclose(dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(dset_info[i].dset_id > 0) {
            ret = H5Dclose(dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        MESG("H5Dclose succeed");
    }

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    H5Fclose(fid);
}

/* 
 * This test with two CONTIG and two CHUNKED dsets 
 * Perform Write, Read and verify
 */
void
phdf5Read_mdset_All(char *filename, phdf5_mode_t pmode, mpio_collective_mode_t mpio_opt, sel_mode_t sel_mode, multi_mode_t multi_mode)
{
    int i;
    int Count;
    hid_t fid;			/* HDF5 file IDs */
    hid_t sid;   		/* Dataspace ID */
    hsize_t dims[SPACE1_RANK] = {SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    H5D_rw_multi_t w_dset_info[NDSET];
    H5D_rw_multi_t r_dset_info[NDSET];
    hid_t crp_plist1, crp_plist2;
    hid_t file_dataspace1, file_dataspace2;	/* File dataspace ID */
    hid_t file_dataspace3, file_dataspace4;	/* File dataspace ID */
    hid_t mem_dataspace1=-1, mem_dataspace2=-1;	/* memory dataspace ID */
    hid_t mem_dataspace3=-1, mem_dataspace4=-1;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hid_t dataset3, dataset4;	/* Dataset ID */
    hsize_t chunk_dims[SPACE1_RANK];	/* dataspace dim sizes */

    /* Write Buffer */
    DTYPE_INT data_array1_w[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array2_w[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array3_w[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array4_w[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    /* Read Buffer */
    DTYPE_INT data_array1_r[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array2_r[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array3_r[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array4_r[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    hsize_t start1[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count1[SPACE1_RANK];  
    hsize_t stride1[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block1[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start2[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count2[SPACE1_RANK];  
    hsize_t stride2[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block2[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start3[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count3[SPACE1_RANK];  
    hsize_t stride3[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block3[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start4[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count4[SPACE1_RANK];  
    hsize_t stride4[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block4[SPACE1_RANK];	/* for hyperslab setting */

    /* point selection */
    hsize_t pcoord[NPOINT][SPACE1_RANK];

    herr_t ret;         	/* Generic return value */

#ifndef TEST_NO_MPI
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
#endif

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    // init [w/r]_dset_info[]
    for (i=0; i< NDSET ; i++) {
        memset(&w_dset_info[i], 0, sizeof(H5D_rw_multi_t));
        memset(&r_dset_info[i], 0, sizeof(H5D_rw_multi_t));
    }

    // Init data_array
    // Wbuf (init all 9)
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array1_w[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array2_w[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array3_w[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array4_w[0][0],INIT_VAL);
    
    // Rbuf (init all 9)
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array1_r[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array2_r[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array3_r[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array4_r[0][0],INIT_VAL);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl != FAIL);
    MESG("H5Pcreate access succeed");
#ifndef TEST_NO_MPI
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");
#endif

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    assert(fid != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    assert(ret != FAIL);


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality dsets */
    sid = H5Screate_simple (SPACE1_RANK, dims, NULL);
    assert (sid != FAIL);
    MESG("H5Screate_simple succeed");


    /* ==========================================
     * set up for chunked Dset1 */
    crp_plist1 = H5Pcreate(H5P_DATASET_CREATE);
    assert (crp_plist1 != FAIL);
    chunk_dims[0] = SPACE1_DIM1 / 2; // divede col by half
    chunk_dims[1] = SPACE1_DIM2;
    ret = H5Pset_chunk(crp_plist1, 2, chunk_dims);
    assert (ret != FAIL);


    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist1, H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dcreate2 succeed");

    // can close chunk prop
    ret = H5Pclose(crp_plist1);
    assert (ret != FAIL);

    /* ----------------------------------------
     * set up for chunked Dset2 */
    crp_plist2 = H5Pcreate(H5P_DATASET_CREATE);
    assert (crp_plist2 != FAIL);
    chunk_dims[0] = SPACE1_DIM1; 
    chunk_dims[1] = SPACE1_DIM2 / 2; // divede row by half
    ret = H5Pset_chunk(crp_plist2, 2, chunk_dims);
    assert (ret != FAIL);

    /* create another dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist2, H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dcreate2 2 succeed");

    // can close chunk prop
    ret = H5Pclose(crp_plist2);
    assert (ret != FAIL);

    /* ==========================================
     * set up for chunked Dset3 */
    /* create a dataset collectively */
    dataset3 = H5Dcreate2(fid, DATASETNAME3, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset3 != FAIL);
    MESG("H5Dcreate2 succeed");

    /* ==========================================
     * set up for chunked Dset4 */
    /* create a dataset collectively */
    dataset4 = H5Dcreate2(fid, DATASETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset4 != FAIL);
    MESG("H5Dcreate2 succeed");


    /*
     * Set up dimensions of the slab this process accesses.
     */

#if 1 // JK_DBG
	printf("== Write DSET 1 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start1, stride1, count1, block1, BYROW);
        slab_set(start1, stride1, count1, block1, BYROW2);
        //slab_set(start1, stride1, count1, block1, BYCOL);
        //slab_set(start1, stride1, count1, block1, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start1, stride1, count1, block1, BYROW_M);
        slab_set(start1, stride1, count1, block1, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start1[]=(%lu,%lu), stride1[]=(%lu,%lu), count1[]=(%lu,%lu), block1[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start1[0], (unsigned long)start1[1],
       (unsigned long)stride1[0], (unsigned long)stride1[1],
       (unsigned long)count1[0], (unsigned long)count1[1],
       (unsigned long)block1[0], (unsigned long)block1[1]);
#endif
    
    /* create a file dataspace independently */
    file_dataspace1 = H5Dget_space (dataset1);
    assert(file_dataspace1 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple (SPACE1_RANK, count1, NULL);

        ret=H5Sselect_hyperslab(file_dataspace1, H5S_SELECT_SET, start1, stride1, count1, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace1 = H5S_ALL;

        ret=H5Sselect_hyperslab(file_dataspace1, H5S_SELECT_SET, start1, stride1, count1, block1);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple (SPACE1_RANK, count1, NULL);

        ret=H5Sselect_none(file_dataspace1);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace1);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start1[0] = pcoord[0][0]; start1[1] = pcoord[0][1];
        stride1[0] = 1; stride1[1] = 1;
        count1[0] = 1; count1[1] = 2;
        block1[0] = 1; block1[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace1, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace1, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start1[0] = pcoord[0][0]; start1[1] = pcoord[0][1];
        stride1[0] = 1; stride1[1] = 1;
        count1[0] = 1; count1[1] = 1;
        block1[0] = 1; block1[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif
        ret = H5Sselect_elements (file_dataspace1, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace1, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start1, count1, stride1, &data_array1_w[0][0],0);
    else
        dataset_fill2_2d(start1, stride1, count1, block1, &data_array1_w[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start1, count1, stride1, &data_array1_w[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset1 Write Data ---\n");
    //dset_select_display(start1, count1, stride1, &data_array1_w[0][0],1);
    dataset_print(start1, count1, stride1, &data_array1_w[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array1_w[0][0], 1);
#endif


    /* write data collectively */
    //ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace1, file_dataspace1,
	//    xfer_plist, data_array1_w);
    //assert(ret != FAIL);
    //MESG("H5Dwrite succeed");



#if 1 // JK_DBG
	printf("== Write DSET 2 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start2, stride2, count2, block2, BYROW);
        //slab_set(start2, stride2, count2, block2, BYROW2);
        //slab_set(start2, stride2, count2, block2, BYCOL);
        slab_set(start2, stride2, count2, block2, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start2, stride2, count2, block2, BYROW_M);
        slab_set(start2, stride2, count2, block2, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start2[]=(%lu,%lu), stride2[]=(%lu,%lu), count2[]=(%lu,%lu), block2[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start2[0], (unsigned long)start2[1],
       (unsigned long)stride2[0], (unsigned long)stride2[1],
       (unsigned long)count2[0], (unsigned long)count2[1],
       (unsigned long)block2[0], (unsigned long)block2[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace2 = H5Dget_space (dataset2);
    assert(file_dataspace2 != FAIL);
    MESG("H5Dget_space succeed");

    if(sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple (SPACE1_RANK, count2, NULL);

        ret=H5Sselect_hyperslab(file_dataspace2, H5S_SELECT_SET, start2, stride2,
	        count2, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if(sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace2 = H5S_ALL;

        ret=H5Sselect_hyperslab(file_dataspace2, H5S_SELECT_SET, start2, stride2, count2, block2);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple (SPACE1_RANK, count2, NULL);

        ret=H5Sselect_none(file_dataspace2);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace2);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start2[0] = pcoord[0][0]; start2[1] = pcoord[0][1];
        stride2[0] = 1; stride2[1] = 1;
        count2[0] = 1; count2[1] = 2;
        block2[0] = 1; block2[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif
        ret = H5Sselect_elements (file_dataspace2, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace2, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start2[0] = pcoord[0][0]; start2[1] = pcoord[0][1];
        stride2[0] = 1; stride2[1] = 1;
        count2[0] = 1; count2[1] = 1;
        block2[0] = 1; block2[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif
        ret = H5Sselect_elements (file_dataspace2, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace2, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start2, count2, stride2, &data_array2_w[0][0],0);
    else
        dataset_fill2_2d(start2, stride2, count2, block2, &data_array2_w[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start2, count2, stride2, &data_array2_w[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset2 Write Data ---\n");
    //dset_select_display(start2, count2, stride2, &data_array2_w[0][0],2);
    dataset_print(start2, count2, stride2, &data_array2_w[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array2_w[0][0], 2);
#endif

    /* write data independently */
    //ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace2, file_dataspace2,
	//    xfer_plist, data_array2_w);
    //assert(ret != FAIL);
    //MESG("H5Dwrite succeed");

#if 1 // JK_DBG
	printf("== Write DSET 3 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start3, stride3, count3, block3, BYROW);
        slab_set(start3, stride3, count3, block3, BYROW2);
        //slab_set(start3, stride3, count3, block3, BYCOL);
        //slab_set(start3, stride3, count3, block3, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start3, stride3, count3, block3, BYROW_M);
        slab_set(start3, stride3, count3, block3, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start3[]=(%lu,%lu), stride3[]=(%lu,%lu), count3[]=(%lu,%lu), block3[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start3[0], (unsigned long)start3[1],
       (unsigned long)stride3[0], (unsigned long)stride3[1],
       (unsigned long)count3[0], (unsigned long)count3[1],
       (unsigned long)block3[0], (unsigned long)block3[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace3 = H5Dget_space (dataset3);
    assert(file_dataspace3 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple (SPACE1_RANK, count3, NULL);
        
        ret=H5Sselect_hyperslab(file_dataspace3, H5S_SELECT_SET, start3, stride3, count3, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace3 = H5S_ALL;

        ret=H5Sselect_hyperslab(file_dataspace3, H5S_SELECT_SET, start3, stride3, count3, block3);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple (SPACE1_RANK, count3, NULL);
        
        ret=H5Sselect_none(file_dataspace3);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace3);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start3[0] = pcoord[0][0]; start3[1] = pcoord[0][1];
        stride3[0] = 1; stride3[1] = 1;
        count3[0] = 1; count3[1] = 2;
        block3[0] = 1; block3[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif
        ret = H5Sselect_elements (file_dataspace3, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace3, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start3[0] = pcoord[0][0]; start3[1] = pcoord[0][1];
        stride3[0] = 1; stride3[1] = 1;
        count3[0] = 1; count3[1] = 1;
        block3[0] = 1; block3[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif
        ret = H5Sselect_elements (file_dataspace3, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace3, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start3, count3, stride3, &data_array3_w[0][0],0);
    else
        dataset_fill2_2d(start3, stride3, count3, block3, &data_array3_w[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start3, count3, stride3, &data_array3_w[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset3 Write Data ---\n");
    //dset_select_display(start3, count3, stride3, &data_array3_w[0][0],0);
   	dataset_print(start3, count3, stride3, &data_array3_w[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array3_w[0][0], 0);
#endif

#if 1 // JK_DBG
	printf("== Write DSET 4 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start4, stride4, count4, block4, BYROW);
        //slab_set(start4, stride4, count4, block4, BYROW2);
        //slab_set(start4, stride4, count4, block4, BYCOL);
        slab_set(start4, stride4, count4, block4, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start4, stride4, count4, block4, BYROW_M);
        slab_set(start4, stride4, count4, block4, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start4[]=(%lu,%lu), stride4[]=(%lu,%lu), count4[]=(%lu,%lu), block4[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start4[0], (unsigned long)start4[1],
       (unsigned long)stride4[0], (unsigned long)stride4[1],
       (unsigned long)count4[0], (unsigned long)count4[1],
       (unsigned long)block4[0], (unsigned long)block4[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace4 = H5Dget_space (dataset4);
    assert(file_dataspace4 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple (SPACE1_RANK, count4, NULL);

        ret=H5Sselect_hyperslab(file_dataspace4, H5S_SELECT_SET, start4, stride4, count4, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if(sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace4 = H5S_ALL;

        ret=H5Sselect_hyperslab(file_dataspace4, H5S_SELECT_SET, start4, stride4, count4, block4);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple (SPACE1_RANK, count4, NULL);

        ret=H5Sselect_none(file_dataspace4);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace4);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start4[0] = pcoord[0][0]; start4[1] = pcoord[0][1];
        stride4[0] = 1; stride4[1] = 1;
        count4[0] = 1; count4[1] = 2;
        block4[0] = 1; block4[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif
        ret = H5Sselect_elements (file_dataspace4, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace4, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start4[0] = pcoord[0][0]; start4[1] = pcoord[0][1];
        stride4[0] = 1; stride4[1] = 1;
        count4[0] = 1; count4[1] = 1;
        block4[0] = 1; block4[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif
        ret = H5Sselect_elements (file_dataspace4, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace4, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start4, count4, stride4, &data_array4_w[0][0],0);
    else
        dataset_fill2_2d(start4, stride4, count4, block4, &data_array4_w[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start4, count4, stride4, &data_array4_w[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset4 Write Data ---\n");
    //dset_select_display(start4, count4, stride4, &data_array4_w[0][0],0);
    dataset_print(start4, count4, stride4, &data_array4_w[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array4_w[0][0], 0);
#endif



    /*==================================================
     * set up the collective transfer properties list 
     */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    MESG("H5Pcreate xfer succeed");

#ifndef TEST_NO_MPI
    if (pmode == PHDF5_PARALLEL)
    {
        ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        assert(ret != FAIL);

        /* In parallel mode, do collective IO (MPI_File_write_at_all) */
        if (mpio_opt == MPIO_COLLECTIVE_IO) {
            /* Note: this is default, so don't need to set explicitly */
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_COLLECTIVE_IO);
            assert(ret >= 0);

            /* Note: this is default, so don't need to set explicitly 
             * Just leave this here for a reference if add more options later */
            /*
            ret = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
            assert(ret >= 0);
            */
        }
        /* In parallel mode, do independent IO (MPI_File_write_at) */
        else if (mpio_opt == MPIO_INDIVIDUAL_IO) {
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
            assert(ret >= 0);
        }
    }
#endif

    /*
     * Set up dset info structure 
     * HDF5 lib will handle by order of [w/r]_dset_info[X] X
     */
#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC //------------------------I
    if (mpi_rank == 0) {
#if 1 // USE_DSET1
        // init H5D_rw_multi_t for write DSET1 (CHUNKED)
        w_dset_info[0].dset_id = dataset1;
        w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[0].mem_space_id = mem_dataspace1;
        w_dset_info[0].dset_space_id = file_dataspace1;
        w_dset_info[0].u.wbuf = &data_array1_w[0][0];

        r_dset_info[0].dset_id = dataset1;
        r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[0].mem_space_id = mem_dataspace1;
        r_dset_info[0].dset_space_id = file_dataspace1;
        r_dset_info[0].u.rbuf = &data_array1_r[0][0];
#else
        /* free before rewrite DSET1 */
        if(mem_dataspace1)  { ret = H5Sclose(mem_dataspace1); assert(ret != FAIL); }
        if(file_dataspace1) { ret = H5Sclose(file_dataspace1); assert(ret != FAIL); }
        if(dataset1) { ret = H5Dclose(dataset1); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET2
        // init H5D_rw_multi_t for write DSET2 (CHUNKED)
        w_dset_info[1].dset_id = dataset2;
        w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[1].mem_space_id = mem_dataspace2;
        w_dset_info[1].dset_space_id = file_dataspace2;
        w_dset_info[1].u.wbuf = &data_array2_w[0][0];

        r_dset_info[1].dset_id = dataset2;
        r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[1].mem_space_id = mem_dataspace2;
        r_dset_info[1].dset_space_id = file_dataspace2;
        r_dset_info[1].u.rbuf = &data_array2_r[0][0];
#else
        /* free before rewrite DSET2 */
        if(mem_dataspace2)  { ret = H5Sclose(mem_dataspace2); assert(ret != FAIL); }
        if(file_dataspace2) { ret = H5Sclose(file_dataspace2); assert(ret != FAIL); }
        if(dataset2) { ret = H5Dclose(dataset2); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET3
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        w_dset_info[2].dset_id = dataset3;
        w_dset_info[2].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[2].mem_space_id = mem_dataspace3;
        w_dset_info[2].dset_space_id = file_dataspace3;
        w_dset_info[2].u.wbuf = &data_array3_w[0][0];

        r_dset_info[2].dset_id = dataset3;
        r_dset_info[2].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[2].mem_space_id = mem_dataspace3;
        r_dset_info[2].dset_space_id = file_dataspace3;
        r_dset_info[2].u.rbuf = &data_array3_r[0][0];
#else
        /* free before rewrite DSET3 */
        if(mem_dataspace3)  { ret = H5Sclose(mem_dataspace3); assert(ret != FAIL); }
        if(file_dataspace3) { ret = H5Sclose(file_dataspace3); assert(ret != FAIL); }
        if(dataset3) { ret = H5Dclose(dataset3); assert(ret != FAIL); }
#endif
        
#if 1 // USE_DSET4
        // init H5D_rw_multi_t for write DSET4 (CONTIG)
        w_dset_info[3].dset_id = dataset4;
        w_dset_info[3].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[3].mem_space_id = mem_dataspace4;
        w_dset_info[3].dset_space_id = file_dataspace4;
        w_dset_info[3].u.wbuf = &data_array4_w[0][0];

        r_dset_info[3].dset_id = dataset4;
        r_dset_info[3].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[3].mem_space_id = mem_dataspace4;
        r_dset_info[3].dset_space_id = file_dataspace4;
        r_dset_info[3].u.rbuf = &data_array4_r[0][0];
#else
        /* free before rewrite DSET4 */
        if(mem_dataspace4)  { ret = H5Sclose(mem_dataspace4); assert(ret != FAIL); }
        if(file_dataspace4) { ret = H5Sclose(file_dataspace4); assert(ret != FAIL); }
        if(dataset4) { ret = H5Dclose(dataset4); assert(ret != FAIL); }
#endif
    }

    if (mpi_rank == 1) {
#if 1 // USE_DSET1
        // init H5D_rw_multi_t for write DSET1 (CHUNKED)
        w_dset_info[0].dset_id = dataset1;
        w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[0].mem_space_id = mem_dataspace1;
        w_dset_info[0].dset_space_id = file_dataspace1;
        w_dset_info[0].u.wbuf = &data_array1_w[0][0];

        r_dset_info[0].dset_id = dataset1;
        r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[0].mem_space_id = mem_dataspace1;
        r_dset_info[0].dset_space_id = file_dataspace1;
        r_dset_info[0].u.rbuf = &data_array1_r[0][0];
#else
        /* free before rewrite DSET1 */
        if(mem_dataspace1)  { ret = H5Sclose(mem_dataspace1); assert(ret != FAIL); }
        if(file_dataspace1) { ret = H5Sclose(file_dataspace1); assert(ret != FAIL); }
        if(dataset1) { ret = H5Dclose(dataset1); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET2
        // init H5D_rw_multi_t for write DSET2 (CHUNKED)
        w_dset_info[1].dset_id = dataset2;
        w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[1].mem_space_id = mem_dataspace2;
        w_dset_info[1].dset_space_id = file_dataspace2;
        w_dset_info[1].u.wbuf = &data_array2_w[0][0];

        r_dset_info[1].dset_id = dataset2;
        r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[1].mem_space_id = mem_dataspace2;
        r_dset_info[1].dset_space_id = file_dataspace2;
        r_dset_info[1].u.rbuf = &data_array2_r[0][0];
#else
        /* free before rewrite DSET2 */
        if(mem_dataspace2)  { ret = H5Sclose(mem_dataspace2); assert(ret != FAIL); }
        if(file_dataspace2) { ret = H5Sclose(file_dataspace2); assert(ret != FAIL); }
        if(dataset2) { ret = H5Dclose(dataset2); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET3
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        w_dset_info[2].dset_id = dataset3;
        w_dset_info[2].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[2].mem_space_id = mem_dataspace3;
        w_dset_info[2].dset_space_id = file_dataspace3;
        w_dset_info[2].u.wbuf = &data_array3_w[0][0];

        r_dset_info[2].dset_id = dataset3;
        r_dset_info[2].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[2].mem_space_id = mem_dataspace3;
        r_dset_info[2].dset_space_id = file_dataspace3;
        r_dset_info[2].u.rbuf = &data_array3_r[0][0];
#else
        /* free before rewrite DSET3 */
        if(mem_dataspace3)  { ret = H5Sclose(mem_dataspace3); assert(ret != FAIL); }
        if(file_dataspace3) { ret = H5Sclose(file_dataspace3); assert(ret != FAIL); }
        if(dataset3) { ret = H5Dclose(dataset3); assert(ret != FAIL); }
#endif
        
#if 0 // USE_DSET4
        // init H5D_rw_multi_t for write DSET4 (CONTIG)
        w_dset_info[3].dset_id = dataset4;
        w_dset_info[3].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[3].mem_space_id = mem_dataspace4;
        w_dset_info[3].dset_space_id = file_dataspace4;
        w_dset_info[3].u.wbuf = &data_array4_w[0][0];

        r_dset_info[3].dset_id = dataset4;
        r_dset_info[3].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[3].mem_space_id = mem_dataspace4;
        r_dset_info[3].dset_space_id = file_dataspace4;
        r_dset_info[3].u.rbuf = &data_array4_r[0][0];
#else
        /* free before rewrite DSET4 */
        if(mem_dataspace4)  { ret = H5Sclose(mem_dataspace4); assert(ret != FAIL); }
        if(file_dataspace4) { ret = H5Sclose(file_dataspace4); assert(ret != FAIL); }
        if(dataset4) { ret = H5Dclose(dataset4); assert(ret != FAIL); }
#endif
    }

#else  // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------------------
    // init H5D_rw_multi_t for write DSET1 (CHUNKED)
    w_dset_info[0].dset_id = dataset1;
    w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[0].mem_space_id = mem_dataspace1;
    w_dset_info[0].dset_space_id = file_dataspace1;
    w_dset_info[0].u.wbuf = &data_array1_w[0][0];

    r_dset_info[0].dset_id = dataset1;
    r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[0].mem_space_id = mem_dataspace1;
    r_dset_info[0].dset_space_id = file_dataspace1;
    r_dset_info[0].u.rbuf = &data_array1_r[0][0];

    // init H5D_rw_multi_t for write DSET2 (CHUNKED)
    w_dset_info[1].dset_id = dataset2;
    w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[1].mem_space_id = mem_dataspace2;
    w_dset_info[1].dset_space_id = file_dataspace2;
    w_dset_info[1].u.wbuf = &data_array2_w[0][0];

    r_dset_info[1].dset_id = dataset2;
    r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[1].mem_space_id = mem_dataspace2;
    r_dset_info[1].dset_space_id = file_dataspace2;
    r_dset_info[1].u.rbuf = &data_array2_r[0][0];

    // init H5D_rw_multi_t for write DSET3 (CONTIG)
    w_dset_info[2].dset_id = dataset3;
    w_dset_info[2].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[2].mem_space_id = mem_dataspace3;
    w_dset_info[2].dset_space_id = file_dataspace3;
    w_dset_info[2].u.wbuf = &data_array3_w[0][0];

    r_dset_info[2].dset_id = dataset3;
    r_dset_info[2].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[2].mem_space_id = mem_dataspace3;
    r_dset_info[2].dset_space_id = file_dataspace3;
    r_dset_info[2].u.rbuf = &data_array3_r[0][0];

    // init H5D_rw_multi_t for write DSET4 (CONTIG)
    w_dset_info[3].dset_id = dataset4;
    w_dset_info[3].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[3].mem_space_id = mem_dataspace4;
    w_dset_info[3].dset_space_id = file_dataspace4;
    w_dset_info[3].u.wbuf = &data_array4_w[0][0];

    r_dset_info[3].dset_id = dataset4;
    r_dset_info[3].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[3].mem_space_id = mem_dataspace4;
    r_dset_info[3].dset_space_id = file_dataspace4;
    r_dset_info[3].u.rbuf = &data_array4_r[0][0];
#endif // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------------------O

#if 0 // JK_DBG
    for (i=0; i< NDSET ; i++)
        printf("%s P%d > w_dset_info[%d].u.wbuf addr: %p\n", __FUNCTION__, mpi_rank, i, w_dset_info[i].u.wbuf);
#endif

#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC
    if (mpi_rank == 0)      Count = 4;
    if (mpi_rank == 1)      Count = 3;
#else
    Count = NDSET;
#endif

    if(multi_mode==MULTI_DSET) {
        ret = H5Dwrite_multi(fid, xfer_plist, Count, w_dset_info);
        assert(ret != FAIL);
#ifdef TEST_DOUBLE_WR_BEFORE_CLOSE
        /* test multiple write before close */
sync();
        ret = H5Dwrite_multi(fid, xfer_plist, Count, w_dset_info);
        assert(ret != FAIL);
#endif
        
        // Read
        ret = H5Dread_multi(fid, xfer_plist, Count, r_dset_info);
        assert(ret != FAIL);

#ifdef TEST_DOUBLE_RD_BEFORE_CLOSE
        /* test multiple read before close */
sync();
        ret = H5Dread_multi(fid, xfer_plist, Count, r_dset_info);
        assert(ret != FAIL);
#endif
    }
    else if(multi_mode==SINGLE_DSET) {
        // Write
        for (i=0; i<Count; i++)
            ret = H5Dwrite(w_dset_info[i].dset_id, w_dset_info[i].mem_type_id, w_dset_info[i].mem_space_id, w_dset_info[i].dset_space_id, xfer_plist, w_dset_info[i].u.wbuf);

        // Read
        for (i=0; i<Count; i++)
            ret = H5Dread(r_dset_info[i].dset_id, r_dset_info[i].mem_type_id, r_dset_info[i].mem_space_id, r_dset_info[i].dset_space_id, xfer_plist, r_dset_info[i].u.rbuf);
    }

    // Verify Read buffer with Write buffer
    if(sel_mode == SEL_HYPER_1BLOCK || sel_mode == SEL_POINTS || sel_mode == SEL_1POINT) {
        if(Count >=1)  // relate to TEST_MDSET_NO_LAST_DSET_2ND_PROC
            ret = dataset_vrfy2(start1, stride1, count1, block1, (DTYPE_INT *)w_dset_info[0].u.wbuf, (DTYPE_INT *)r_dset_info[0].u.rbuf,1);
        if(Count >=2)   // relate to TEST_MDSET_NO_LAST_DSET_2ND_PROC 
            ret = dataset_vrfy2(start2, stride2, count2, block2, (DTYPE_INT *)w_dset_info[1].u.wbuf, (DTYPE_INT *)r_dset_info[1].u.rbuf,2);
        if(Count >=3)  // relate to TEST_MDSET_NO_LAST_DSET_2ND_PROC
            ret = dataset_vrfy2(start3, stride3, count3, block3, (DTYPE_INT *)w_dset_info[2].u.wbuf, (DTYPE_INT *)r_dset_info[2].u.rbuf,3);
        if(Count >=4)   // relate to TEST_MDSET_NO_LAST_DSET_2ND_PROC 
            ret = dataset_vrfy2(start4, stride4, count4, block4, (DTYPE_INT *)w_dset_info[3].u.wbuf, (DTYPE_INT *)r_dset_info[3].u.rbuf,4);
    }
    else if (sel_mode == SEL_NONE) {
        // init the wbuf, as rbuf shouldn't read any 
        data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array1_w[0][0],INIT_VAL);
        data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array2_w[0][0],INIT_VAL);
        data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array3_w[0][0],INIT_VAL);
        data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array4_w[0][0],INIT_VAL);
        if(Count >= 1)
            ret = diff_datasets(SPACE1_DIM1,SPACE1_DIM2, (DTYPE_INT *)w_dset_info[0].u.wbuf, (DTYPE_INT *)r_dset_info[0].u.rbuf,1);
        if(Count >= 2) 
            ret = diff_datasets(SPACE1_DIM1,SPACE1_DIM2, (DTYPE_INT *)w_dset_info[1].u.wbuf, (DTYPE_INT *)r_dset_info[1].u.rbuf,2);
        if(Count >= 3)
            ret = diff_datasets(SPACE1_DIM1,SPACE1_DIM2, (DTYPE_INT *)w_dset_info[2].u.wbuf, (DTYPE_INT *)r_dset_info[2].u.rbuf,3);
        if(Count > 4) 
            ret = diff_datasets(SPACE1_DIM1,SPACE1_DIM2, (DTYPE_INT *)w_dset_info[3].u.wbuf, (DTYPE_INT *)r_dset_info[3].u.rbuf,4);         
    }

    MESG("H5Dwrite succeed");

    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    /* release all temporary handles. */
    for (i=0;i< Count;i++)
    {
        if(w_dset_info[i].mem_space_id > 0) {
            ret = H5Sclose(w_dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].mem_space_id > 0) {
            ret = H5Sclose(r_dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(w_dset_info[i].dset_space_id > 0) {
            ret = H5Sclose(w_dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].dset_space_id > 0) {
            ret = H5Sclose(r_dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(w_dset_info[i].dset_id > 0) {
            ret = H5Dclose(w_dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].dset_id > 0) {
            ret = H5Dclose(r_dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        MESG("H5Dclose succeed");
    }

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    H5Fclose(fid);
}


/* This test with one or two CHUNKED dset */
void
phdf5Write_mdset_Chunk(char *filename, phdf5_mode_t pmode, mpio_collective_mode_t mpio_opt, sel_mode_t sel_mode, multi_mode_t multi_mode)
{
    int i;
    int Count=0;
    hid_t fid;			/* HDF5 file IDs */
    hid_t sid;   		/* Dataspace ID */
    hsize_t dims[SPACE1_RANK] = {SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    H5D_rw_multi_t dset_info[NDSET_CHUNK];
    hid_t crp_plist1, crp_plist2;
    hid_t file_dataspace1, file_dataspace2;	/* File dataspace ID */
    hid_t mem_dataspace1=-1, mem_dataspace2=-1;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hsize_t chunk_dims[SPACE1_RANK];	/* dataspace dim sizes */

    DTYPE_INT data_array1[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array2[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    hsize_t start1[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count1[SPACE1_RANK];  
    hsize_t stride1[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block1[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start2[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count2[SPACE1_RANK];  
    hsize_t stride2[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block2[SPACE1_RANK];	/* for hyperslab setting */

    /* point selection */
    hsize_t pcoord[NPOINT][SPACE1_RANK];

    herr_t ret;         	/* Generic return value */

#ifndef TEST_NO_MPI
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
#endif

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    // init dset_info[]
    for (i=0; i< NDSET_CHUNK ; i++) {
        memset(&dset_info[i], 0, sizeof(H5D_rw_multi_t));
    }

    // Init data_array
    // Wbuf (init all 9)
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array1[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array2[0][0],INIT_VAL);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl != FAIL);
    MESG("H5Pcreate access succeed");
#ifndef TEST_NO_MPI
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");
#endif

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    assert(fid != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    assert(ret != FAIL);


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality dsets */
    sid = H5Screate_simple (SPACE1_RANK, dims, NULL);
    assert (sid != FAIL);
    MESG("H5Screate_simple succeed");


    /* ==========================================
     * set up for chunked Dset1 */
    crp_plist1 = H5Pcreate(H5P_DATASET_CREATE);
    assert (crp_plist1 != FAIL);
    chunk_dims[0] = SPACE1_DIM1;
    chunk_dims[1] = SPACE1_DIM2 / 2;  // divede row by half
    ret = H5Pset_chunk(crp_plist1, 2, chunk_dims);
    assert (ret != FAIL);


    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist1, H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dcreate2 succeed");

    // can close chunk prop
    ret = H5Pclose(crp_plist1);
    assert (ret != FAIL);

#ifdef TEST_TWO_CHUNK
    /* ----------------------------------------
     * set up for chunked Dset2 */
    crp_plist2 = H5Pcreate(H5P_DATASET_CREATE);
    assert (crp_plist2 != FAIL);
    chunk_dims[0] = SPACE1_DIM1 / 2; // divede col by half
    chunk_dims[1] = SPACE1_DIM2;
    ret = H5Pset_chunk(crp_plist2, 2, chunk_dims);
    assert (ret != FAIL);

    /* create another dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist2, H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dcreate2 2 succeed");

    // can close chunk prop
    ret = H5Pclose(crp_plist2);
    assert (ret != FAIL);
#endif 

    /*
     * Set up dimensions of the slab this process accesses.
     */
#if 1 // JK_DBG
	printf("== Write DSET 1 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        slab_set(start1, stride1, count1, block1, BYROW);
        //slab_set(start1, stride1, count1, block1, BYROW2);
        //slab_set(start1, stride1, count1, block1, BYCOL);
        //slab_set(start1, stride1, count1, block1, BYCOL2);
    }
    else {
        /* each process takes partial */
        //slab_set(start1, stride1, count1, block1, BYROW_M);
        slab_set(start1, stride1, count1, block1, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start1[]=(%lu,%lu), stride1[]=(%lu,%lu), count1[]=(%lu,%lu), block1[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start1[0], (unsigned long)start1[1],
       (unsigned long)stride1[0], (unsigned long)stride1[1],
       (unsigned long)count1[0], (unsigned long)count1[1],
       (unsigned long)block1[0], (unsigned long)block1[1]);
#endif

    
    /* create a file dataspace independently */
    file_dataspace1 = H5Dget_space (dataset1);
    assert(file_dataspace1 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple (SPACE1_RANK, count1, NULL);
        ret=H5Sselect_hyperslab(file_dataspace1, H5S_SELECT_SET, start1, stride1, count1, NULL);

        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace1 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace1, H5S_SELECT_SET, start1, stride1, count1, block1);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple (SPACE1_RANK, count1, NULL);

        ret=H5Sselect_none(file_dataspace1);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace1);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start1[0] = pcoord[0][0]; start1[1] = pcoord[0][1];
        stride1[0] = 1; stride1[1] = 1;
        count1[0] = 1; count1[1] = 2;
        block1[0] = 1; block1[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace1, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace1, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start1[0] = pcoord[0][0]; start1[1] = pcoord[0][1];
        stride1[0] = 1; stride1[1] = 1;
        count1[0] = 1; count1[1] = 1;
        block1[0] = 1; block1[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif

        ret = H5Sselect_elements (file_dataspace1, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace1, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start1, count1, stride1, &data_array1[0][0],0);
    else
        dataset_fill2_2d(start1, stride1, count1, block1, &data_array1[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start1, count1, stride1, &data_array1[0][0]);
    }

#if 0 // JK_DBG
	printf("--- dset1 Write Data ---\n");
    //dset_select_display(start1, count1, stride1, &data_array1[0][0],1);
    dataset_print(start1, count1, stride1, &data_array1[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array1[0][0], 1);
#endif

#ifdef TEST_TWO_CHUNK
#if 1 // JK_DBG
	printf("== Write DSET 2 =============================\n");
#endif
    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start2, stride2, count2, block2, BYROW);
        //slab_set(start2, stride2, count2, block2, BYROW2);
        //slab_set(start2, stride2, count2, block2, BYCOL);
        slab_set(start2, stride2, count2, block2, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start2, stride2, count2, block2, BYROW_M);
        slab_set(start2, stride2, count2, block2, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start2[]=(%lu,%lu), stride2[]=(%lu,%lu), count2[]=(%lu,%lu), block2[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start2[0], (unsigned long)start2[1],
       (unsigned long)stride2[0], (unsigned long)stride2[1],
       (unsigned long)count2[0], (unsigned long)count2[1],
       (unsigned long)block2[0], (unsigned long)block2[1]);
#endif


    /* create a file dataspace independently */
    file_dataspace2 = H5Dget_space (dataset2);
    assert(file_dataspace2 != FAIL);
    MESG("H5Dget_space succeed");

    if(sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple (SPACE1_RANK, count2, NULL);
        ret=H5Sselect_hyperslab(file_dataspace2, H5S_SELECT_SET, start2, stride2, count2, NULL);

        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if(sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace2 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace2, H5S_SELECT_SET, start2, stride2, count2, block2);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple (SPACE1_RANK, count2, NULL);
        
        ret=H5Sselect_none(file_dataspace2);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace2);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start2[0] = pcoord[1][0]; start2[1] = pcoord[1][1] - 1;
        stride2[0] = 1; stride2[1] = 1;
        count2[0] = 1; count2[1] = 2;
        block2[0] = 1; block2[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace2, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace2, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start2[0] = pcoord[0][0]; start2[1] = pcoord[0][1];
        stride2[0] = 1; stride2[1] = 1;
        count2[0] = 1; count2[1] = 1;
        block2[0] = 1; block2[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif

        ret = H5Sselect_elements (file_dataspace2, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace2, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start2, count2, stride2, &data_array2[0][0],0);
    else
        dataset_fill2_2d(start2, stride2, count2, block2, &data_array2[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start2, count2, stride2, &data_array2[0][0]);
    }

#if 0 // JK_DBG
	printf("--- dset2 Write Data ---\n");
    //dset_select_display(start2, count2, stride2, &data_array2[0][0],2);
    dataset_print(start2, count2, stride2, &data_array2[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array2[0][0], 2);
#endif

#endif // TEST_TWO_CHUNK


    /*==================================================
     * set up the collective transfer properties list 
     */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    MESG("H5Pcreate xfer succeed");

#ifndef TEST_NO_MPI
    if (pmode == PHDF5_PARALLEL)
    {
        ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        assert(ret != FAIL);

        /* In parallel mode, do collective IO (MPI_File_write_at_all) */
        if (mpio_opt == MPIO_COLLECTIVE_IO) {
            /* Note: this is default, so don't need to set explicitly */
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_COLLECTIVE_IO);
            assert(ret >= 0);

            /* Note: this is default, so don't need to set explicitly 
             * Just leave this here for a reference if add more options later */
            /*
            ret = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
            assert(ret >= 0);
            */
        }
        /* In parallel mode, do independent IO (MPI_File_write_at) */
        else if (mpio_opt == MPIO_INDIVIDUAL_IO) {
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
            assert(ret >= 0);
        }
    }
#endif

    /*
     * Set up dset info structure 
     * HDF5 lib will handle by order of dset_info[X] X
     */
#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC //------------------------I
   if (mpi_rank == 0) {
#if 1 // USE_DSET1
        // init H5D_rw_multi_t for write DSET1 (CHUNKED)
        dset_info[0].dset_id = dataset1;
        dset_info[0].mem_type_id = H5T_NATIVE_INT;
        dset_info[0].mem_space_id = mem_dataspace1;
        dset_info[0].dset_space_id = file_dataspace1;
        dset_info[0].u.wbuf = &data_array1[0][0];
#else
        /* free before rewrite DSET1 */
        if(mem_dataspace1)  { ret = H5Sclose(mem_dataspace1); assert(ret != FAIL); }
        if(file_dataspace1) { ret = H5Sclose(file_dataspace1); assert(ret != FAIL); }
        if(dataset1) { ret = H5Dclose(dataset1); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET2
        // init H5D_rw_multi_t for write DSET2 (CHUNKED)
        dset_info[1].dset_id = dataset2;
        dset_info[1].mem_type_id = H5T_NATIVE_INT;
        dset_info[1].mem_space_id = mem_dataspace2;
        dset_info[1].dset_space_id = file_dataspace2;
        dset_info[1].u.wbuf = &data_array2[0][0];
#else
        /* free before rewrite DSET2 */
        if(mem_dataspace2)  { ret = H5Sclose(mem_dataspace2); assert(ret != FAIL); }
        if(file_dataspace2) { ret = H5Sclose(file_dataspace2); assert(ret != FAIL); }
        if(dataset2) { ret = H5Dclose(dataset2); assert(ret != FAIL); }
#endif
   } // mpi_rank == 0

   if (mpi_rank == 1) {
#if 1 // USE_DSET1
        // init H5D_rw_multi_t for write DSET1 (CHUNKED)
        dset_info[0].dset_id = dataset1;
        dset_info[0].mem_type_id = H5T_NATIVE_INT;
        dset_info[0].mem_space_id = mem_dataspace1;
        dset_info[0].dset_space_id = file_dataspace1;
        dset_info[0].u.wbuf = &data_array1[0][0];
#else
        /* free before rewrite DSET1 */
        if(mem_dataspace1)  { ret = H5Sclose(mem_dataspace1); assert(ret != FAIL); }
        if(file_dataspace1) { ret = H5Sclose(file_dataspace1); assert(ret != FAIL); }
        if(dataset1) { ret = H5Dclose(dataset1); assert(ret != FAIL); }
#endif

#if 0 // USE_DSET2
        // init H5D_rw_multi_t for write DSET2 (CHUNKED)
        dset_info[1].dset_id = dataset2;
        dset_info[1].mem_type_id = H5T_NATIVE_INT;
        dset_info[1].mem_space_id = mem_dataspace2;
        dset_info[1].dset_space_id = file_dataspace2;
        dset_info[1].u.wbuf = &data_array2[0][0];
#else
        /* free before rewrite DSET2 */
        if(mem_dataspace2)  { ret = H5Sclose(mem_dataspace2); assert(ret != FAIL); }
        if(file_dataspace2) { ret = H5Sclose(file_dataspace2); assert(ret != FAIL); }
        if(dataset2) { ret = H5Dclose(dataset2); assert(ret != FAIL); }
#endif
   } // mpi_rank == 1

#else // TEST_MDSET_NO_LAST_DSET_2ND_PROC ---------
    // init H5D_rw_multi_t for write DSET1 (CHUNKED)
    dset_info[0].dset_id = dataset1;
    dset_info[0].mem_type_id = H5T_NATIVE_INT;
    dset_info[0].mem_space_id = mem_dataspace1;
    dset_info[0].dset_space_id = file_dataspace1;
    dset_info[0].u.wbuf = &data_array1[0][0];

#ifdef TEST_TWO_CHUNK
    // init H5D_rw_multi_t for write DSET2 (CHUNKED)
    dset_info[1].dset_id = dataset2;
    dset_info[1].mem_type_id = H5T_NATIVE_INT;
    dset_info[1].mem_space_id = mem_dataspace2;
    dset_info[1].dset_space_id = file_dataspace2;
    dset_info[1].u.wbuf = &data_array2[0][0];
#endif
#endif //TEST_MDSET_NO_LAST_DSET_2ND_PROC --------------O

#if 0 // JK_DBG
    for (i=0; i< NDSET_CHUNK ; i++)
        printf("%s P%d > dset_info[%d].u.wbuf addr: %x\n", __FUNCTION__, mpi_rank, i, dset_info[i].u.wbuf);
#endif

#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC
    if (mpi_rank == 0)      Count = 2;
    if (mpi_rank == 1)      Count = 1;
#else
    Count = NDSET_CHUNK;
#endif

    if(multi_mode==MULTI_DSET) {
        ret = H5Dwrite_multi(fid, xfer_plist, Count, dset_info);
        assert(ret != FAIL);
#ifdef TEST_DOUBLE_WR_BEFORE_CLOSE
        /* test multiple write before close */
sync();
        ret = H5Dwrite_multi(fid, xfer_plist, Count, dset_info);
        assert(ret != FAIL);
#endif
    }
    else if(multi_mode==SINGLE_DSET) {
        for (i=0; i<Count; i++)
            ret = H5Dwrite(dset_info[i].dset_id, dset_info[i].mem_type_id, dset_info[i].mem_space_id, dset_info[i].dset_space_id, xfer_plist, dset_info[i].u.wbuf );
    }

    MESG("H5Dwrite succeed");

    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    /* release all temporary handles. */
    for (i=0;i< Count;i++)
    {
        if(dset_info[i].mem_space_id > 0)
        {
            ret = H5Sclose(dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(dset_info[i].dset_space_id > 0)
        {
            ret = H5Sclose(dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(dset_info[i].dset_id > 0)
        {
            ret = H5Dclose(dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        MESG("H5Dclose succeed");
    }

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    H5Fclose(fid);
}

/* This test with one or two CHUNKED dset */
void
phdf5Read_mdset_Chunk(char *filename, phdf5_mode_t pmode, mpio_collective_mode_t mpio_opt, sel_mode_t sel_mode, multi_mode_t multi_mode)
{
    int i;
    int Count=0;
    hid_t fid;			/* HDF5 file IDs */
    hid_t sid;   		/* Dataspace ID */
    hsize_t dims[SPACE1_RANK] = {SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    H5D_rw_multi_t w_dset_info[NDSET_CHUNK];
    H5D_rw_multi_t r_dset_info[NDSET_CHUNK];
    hid_t crp_plist1, crp_plist2;
    hid_t file_dataspace1, file_dataspace2;	/* File dataspace ID */
    hid_t mem_dataspace1=-1, mem_dataspace2=-1;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hsize_t chunk_dims[SPACE1_RANK];	/* dataspace dim sizes */

    /* Write Buffer */
    DTYPE_INT data_array1_w[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array2_w[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    /* Read Buffer */
    DTYPE_INT data_array1_r[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array2_r[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    hsize_t start1[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t count1[SPACE1_RANK];  
    hsize_t stride1[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block1[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start2[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t count2[SPACE1_RANK];  
    hsize_t stride2[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block2[SPACE1_RANK];	/* for hyperslab setting */

    /* point selection */
    hsize_t pcoord[NPOINT][SPACE1_RANK];

    herr_t ret;         	/* Generic return value */

#ifndef TEST_NO_MPI
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
#endif

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    // init [w/r]_dset_info[]
    for (i=0; i< NDSET_CHUNK ; i++) {
        memset(&w_dset_info[i], 0, sizeof(H5D_rw_multi_t));
        memset(&r_dset_info[i], 0, sizeof(H5D_rw_multi_t));
    }

    // Init data_array
    // Wbuf (init all 9)
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array1_w[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array2_w[0][0],INIT_VAL);

    // Rbuf (init all 9)
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array1_r[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array2_r[0][0],INIT_VAL);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl != FAIL);
    MESG("H5Pcreate access succeed");
#ifndef TEST_NO_MPI
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");
#endif

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    assert(fid != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    assert(ret != FAIL);


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality dsets */
    sid = H5Screate_simple (SPACE1_RANK, dims, NULL);
    assert (sid != FAIL);
    MESG("H5Screate_simple succeed");


    /* ==========================================
     * set up for chunked Dset1 */
    crp_plist1 = H5Pcreate(H5P_DATASET_CREATE);
    assert (crp_plist1 != FAIL);
    chunk_dims[0] = SPACE1_DIM1;
    chunk_dims[1] = SPACE1_DIM2 / 2;  // divede row by half
    ret = H5Pset_chunk(crp_plist1, 2, chunk_dims);
    assert (ret != FAIL);


    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist1, H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dcreate2 succeed");

    // can close chunk prop
    ret = H5Pclose(crp_plist1);
    assert (ret != FAIL);

#ifdef TEST_TWO_CHUNK
    /* ----------------------------------------
     * set up for chunked Dset2 */
    crp_plist2 = H5Pcreate(H5P_DATASET_CREATE);
    assert (crp_plist2 != FAIL);
    chunk_dims[0] = SPACE1_DIM1 / 2; // divede col by half
    chunk_dims[1] = SPACE1_DIM2;
    ret = H5Pset_chunk(crp_plist2, 2, chunk_dims);
    assert (ret != FAIL);

    /* create another dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist2, H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dcreate2 2 succeed");

    // can close chunk prop
    ret = H5Pclose(crp_plist2);
    assert (ret != FAIL);
#endif 

    /*
     * Set up dimensions of the slab this process accesses.
     */
#if 1 // JK_DBG
	printf("== Write DSET 1 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block. */
        slab_set(start1, stride1, count1, block1, BYROW);
        //slab_set(start1, stride1, count1, block1, BYROW2);
        //slab_set(start1, stride1, count1, block1, BYCOL);
        //slab_set(start1, stride1, count1, block1, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start1, stride1, count1, block1, BYROW_M);
        slab_set(start1, stride1, count1, block1, BYCOL_M);
    }


#if 0 // JK_DBG
    printf("%s P%d > start1[]=(%lu,%lu), stride1[]=(%lu,%lu), count1[]=(%lu,%lu), block1[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start1[0], (unsigned long)start1[1],
       (unsigned long)stride1[0], (unsigned long)stride1[1],
       (unsigned long)count1[0], (unsigned long)count1[1],
       (unsigned long)block1[0], (unsigned long)block1[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace1 = H5Dget_space (dataset1);
    assert(file_dataspace1 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple (SPACE1_RANK, count1, NULL);
        ret=H5Sselect_hyperslab(file_dataspace1, H5S_SELECT_SET, start1, stride1, count1, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace1 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace1, H5S_SELECT_SET, start1, stride1, count1, block1);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple (SPACE1_RANK, count1, NULL);
        ret=H5Sselect_none(file_dataspace1);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace1);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start1[0] = pcoord[0][0]; start1[1] = pcoord[0][1];
        stride1[0] = 1; stride1[1] = 1;
        count1[0] = 1; count1[1] = 2;
        block1[0] = 1; block1[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace1, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace1, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start1[0] = pcoord[0][0]; start1[1] = pcoord[0][1];
        stride1[0] = 1; stride1[1] = 1;
        count1[0] = 1; count1[1] = 1;
        block1[0] = 1; block1[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif

        ret = H5Sselect_elements (file_dataspace1, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace1 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace1, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start1, count1, stride1, &data_array1_w[0][0],0);
    else
        dataset_fill2_2d(start1, stride1, count1, block1, &data_array1_w[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start1, count1, stride1, &data_array1_w[0][0]);
    }

#if 0 // JK_DBG
	printf("--- dset1 Write Data ---\n");
    //dset_select_display(start1, count1, stride1, &data_array1_w[0][0],1);
    dataset_print(start1, count1, stride1, &data_array1_w[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array1_w[0][0], 1);
#endif

#ifdef TEST_TWO_CHUNK
#if 1 // JK_DBG
	printf("== Write DSET 2 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start2, stride2, count2, block2, BYROW);
        //slab_set(start2, stride2, count2, block2, BYROW2);
        //slab_set(start2, stride2, count2, block2, BYCOL);
        slab_set(start2, stride2, count2, block2, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start2, stride2, count2, block2, BYROW_M);
        slab_set(start2, stride2, count2, block2, BYCOL_M);
    }


#if 0 // JK_DBG
    printf("%s P%d > start2[]=(%lu,%lu), stride2[]=(%lu,%lu), count2[]=(%lu,%lu), block2[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start2[0], (unsigned long)start2[1],
       (unsigned long)stride2[0], (unsigned long)stride2[1],
       (unsigned long)count2[0], (unsigned long)count2[1],
       (unsigned long)block2[0], (unsigned long)block2[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace2 = H5Dget_space (dataset2);
    assert(file_dataspace2 != FAIL);
    MESG("H5Dget_space succeed");

    if(sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple (SPACE1_RANK, count2, NULL);

        ret=H5Sselect_hyperslab(file_dataspace2, H5S_SELECT_SET, start2, stride2, count2, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if(sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace2 = H5S_ALL;

        ret=H5Sselect_hyperslab(file_dataspace2, H5S_SELECT_SET, start2, stride2, count2, block2);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple (SPACE1_RANK, count2, NULL);

        ret=H5Sselect_none(file_dataspace2);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace2);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start2[0] = pcoord[1][0]; start2[1] = pcoord[1][1] - 1;
        stride2[0] = 1; stride2[1] = 1;
        count2[0] = 1; count2[1] = 2;
        block2[0] = 1; block2[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace2, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace2, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start2[0] = pcoord[0][0]; start2[1] = pcoord[0][1];
        stride2[0] = 1; stride2[1] = 1;
        count2[0] = 1; count2[1] = 1;
        block2[0] = 1; block2[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif

        ret = H5Sselect_elements (file_dataspace2, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace2 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace2, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start2, count2, stride2, &data_array2_w[0][0],0);
    else
        dataset_fill2_2d(start2, stride2, count2, block2, &data_array2_w[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start2, count2, stride2, &data_array2_w[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset2 Write Data ---\n");
    //dset_select_display(start2, count2, stride2, &data_array2_w[0][0],2);
    dataset_print(start2, count2, stride2, &data_array2_w[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array2_w[0][0], 2);
#endif

#endif // TEST_TWO_CHUNK


    /*==================================================
     * set up the collective transfer properties list 
     */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    MESG("H5Pcreate xfer succeed");

#ifndef TEST_NO_MPI
    if (pmode == PHDF5_PARALLEL)
    {
        ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        assert(ret != FAIL);

        /* In parallel mode, do collective IO (MPI_File_write_at_all) */
        if (mpio_opt == MPIO_COLLECTIVE_IO) {
            /* Note: this is default, so don't need to set explicitly */
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_COLLECTIVE_IO);
            assert(ret >= 0);

            /* Note: this is default, so don't need to set explicitly 
             * Just leave this here for a reference if add more options later */
            /*
            ret = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
            assert(ret >= 0);
            */
        }
        /* In parallel mode, do independent IO (MPI_File_write_at) */
        else if (mpio_opt == MPIO_INDIVIDUAL_IO) {
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
            assert(ret >= 0);
        }
    }
#endif

    /*
     * Set up dset info structure 
     * HDF5 lib will handle by order of [w/r]_dset_info[X] X
     */
#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC //------------------------I
   if (mpi_rank == 0) {
#if 1 // USE_DSET1
        // init H5D_rw_multi_t for write DSET1 (CHUNKED)
        w_dset_info[0].dset_id = dataset1;
        w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[0].mem_space_id = mem_dataspace1;
        w_dset_info[0].dset_space_id = file_dataspace1;
        w_dset_info[0].u.wbuf = &data_array1_w[0][0];

        r_dset_info[0].dset_id = dataset1;
        r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[0].mem_space_id = mem_dataspace1;
        r_dset_info[0].dset_space_id = file_dataspace1;
        r_dset_info[0].u.rbuf = &data_array1_r[0][0];
#else
        /* free before rewrite DSET1 */
        if(mem_dataspace1)  { ret = H5Sclose(mem_dataspace1); assert(ret != FAIL); }
        if(file_dataspace1) { ret = H5Sclose(file_dataspace1); assert(ret != FAIL); }
        if(dataset1) { ret = H5Dclose(dataset1); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET2
        // init H5D_rw_multi_t for write DSET2 (CHUNKED)
        w_dset_info[1].dset_id = dataset2;
        w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[1].mem_space_id = mem_dataspace2;
        w_dset_info[1].dset_space_id = file_dataspace2;
        w_dset_info[1].u.wbuf = &data_array2_w[0][0];

        r_dset_info[1].dset_id = dataset2;
        r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[1].mem_space_id = mem_dataspace2;
        r_dset_info[1].dset_space_id = file_dataspace2;
        r_dset_info[1].u.rbuf = &data_array2_r[0][0];
#else
        /* free before rewrite DSET2 */
        if(mem_dataspace2)  { ret = H5Sclose(mem_dataspace2); assert(ret != FAIL); }
        if(file_dataspace2) { ret = H5Sclose(file_dataspace2); assert(ret != FAIL); }
        if(dataset2) { ret = H5Dclose(dataset2); assert(ret != FAIL); }
#endif
   } // mpi_rank == 0

   if (mpi_rank == 1) {
#if 1 // USE_DSET1
        // init H5D_rw_multi_t for write DSET1 (CHUNKED)
        w_dset_info[0].dset_id = dataset1;
        w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[0].mem_space_id = mem_dataspace1;
        w_dset_info[0].dset_space_id = file_dataspace1;
        w_dset_info[0].u.wbuf = &data_array1_w[0][0];

        r_dset_info[0].dset_id = dataset1;
        r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[0].mem_space_id = mem_dataspace1;
        r_dset_info[0].dset_space_id = file_dataspace1;
        r_dset_info[0].u.rbuf = &data_array1_r[0][0];
#else
        /* free before rewrite DSET1 */
        if(mem_dataspace1)  { ret = H5Sclose(mem_dataspace1); assert(ret != FAIL); }
        if(file_dataspace1) { ret = H5Sclose(file_dataspace1); assert(ret != FAIL); }
        if(dataset1) { ret = H5Dclose(dataset1); assert(ret != FAIL); }
#endif

#if 0 // USE_DSET2
        // init H5D_rw_multi_t for write DSET2 (CHUNKED)
        w_dset_info[1].dset_id = dataset2;
        w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[1].mem_space_id = mem_dataspace2;
        w_dset_info[1].dset_space_id = file_dataspace2;
        w_dset_info[1].u.wbuf = &data_array2_w[0][0];

        r_dset_info[1].dset_id = dataset2;
        r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[1].mem_space_id = mem_dataspace2;
        r_dset_info[1].dset_space_id = file_dataspace2;
        r_dset_info[1].u.rbuf = &data_array2_r[0][0];
#else
        /* free before rewrite DSET2 */
        if(mem_dataspace2)  { ret = H5Sclose(mem_dataspace2); assert(ret != FAIL); }
        if(file_dataspace2) { ret = H5Sclose(file_dataspace2); assert(ret != FAIL); }
        if(dataset2) { ret = H5Dclose(dataset2); assert(ret != FAIL); }
#endif
   } // mpi_rank == 1

#else // TEST_MDSET_NO_LAST_DSET_2ND_PROC ---------
    // init H5D_rw_multi_t for write DSET1 (CHUNKED)
    w_dset_info[0].dset_id = dataset1;
    w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[0].mem_space_id = mem_dataspace1;
    w_dset_info[0].dset_space_id = file_dataspace1;
    w_dset_info[0].u.wbuf = &data_array1_w[0][0];

    r_dset_info[0].dset_id = dataset1;
    r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[0].mem_space_id = mem_dataspace1;
    r_dset_info[0].dset_space_id = file_dataspace1;
    r_dset_info[0].u.rbuf = &data_array1_r[0][0];

#ifdef TEST_TWO_CHUNK
    // init H5D_rw_multi_t for write DSET2 (CHUNKED)
    w_dset_info[1].dset_id = dataset2;
    w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[1].mem_space_id = mem_dataspace2;
    w_dset_info[1].dset_space_id = file_dataspace2;
    w_dset_info[1].u.wbuf = &data_array2_w[0][0];

    r_dset_info[1].dset_id = dataset2;
    r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[1].mem_space_id = mem_dataspace2;
    r_dset_info[1].dset_space_id = file_dataspace2;
    r_dset_info[1].u.rbuf = &data_array2_r[0][0];
#endif
#endif //TEST_MDSET_NO_LAST_DSET_2ND_PROC --------------O

#if 0 // JK_DBG
    for (i=0; i< NDSET_CHUNK ; i++)
        printf("%s P%d > w_dset_info[%d].u.wbuf addr: %x\n", __FUNCTION__, mpi_rank, i, w_dset_info[i].u.wbuf);
#endif

#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC
    if (mpi_rank == 0)      Count = 2;
    if (mpi_rank == 1)      Count = 1;
#else
    Count = NDSET_CHUNK;
#endif

    if(multi_mode==MULTI_DSET) {
        // Write
        ret = H5Dwrite_multi(fid, xfer_plist, Count, w_dset_info);
        assert(ret != FAIL);
#ifdef TEST_DOUBLE_WR_BEFORE_CLOSE
        /* test multiple write before close */
sync();
        ret = H5Dwrite_multi(fid, xfer_plist, Count, w_dset_info);
        assert(ret != FAIL);
#endif

        // Read
        ret = H5Dread_multi(fid, xfer_plist, Count, r_dset_info);
        assert(ret != FAIL);

#ifdef TEST_DOUBLE_RD_BEFORE_CLOSE
        /* test multiple read before close */
sync();
        ret = H5Dread_multi(fid, xfer_plist, Count, r_dset_info);
        assert(ret != FAIL);
#endif
    }
    else if(multi_mode==SINGLE_DSET) {
        // Write
        for (i=0; i<Count; i++) {
            ret = H5Dwrite(w_dset_info[i].dset_id, w_dset_info[i].mem_type_id, w_dset_info[i].mem_space_id, w_dset_info[i].dset_space_id, xfer_plist, w_dset_info[i].u.wbuf );
            assert(ret != FAIL);
        }

        // Read
        for (i=0; i<Count; i++) {
            ret = H5Dread(r_dset_info[i].dset_id, r_dset_info[i].mem_type_id, r_dset_info[i].mem_space_id, r_dset_info[i].dset_space_id, xfer_plist, r_dset_info[i].u.rbuf );
            assert(ret != FAIL);
        }
    }

    // Verify Read buffer with Write buffer 
    if(sel_mode == SEL_HYPER_1BLOCK || sel_mode == SEL_POINTS || sel_mode == SEL_1POINT) {
        block1[0] = 1; block1[1] = 1;
        block2[0] = 1; block2[1] = 1;
        if(Count >=1)  // relate to TEST_MDSET_NO_LAST_DSET_2ND_PROC
            ret = dataset_vrfy2(start1, stride1, count1, block1, (DTYPE_INT *)w_dset_info[0].u.wbuf, (DTYPE_INT *)r_dset_info[0].u.rbuf,1);
        if(Count > 1)   // relate to TEST_MDSET_NO_LAST_DSET_2ND_PROC 
            ret = dataset_vrfy2(start2, stride2, count2, block2, (DTYPE_INT *)w_dset_info[1].u.wbuf, (DTYPE_INT *)r_dset_info[1].u.rbuf,2);
    }
    else if (sel_mode == SEL_NONE) {
        // init the wbuf, as rbuf shouldn't read any 
        data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array1_w[0][0],INIT_VAL);
        data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array2_w[0][0],INIT_VAL);
        if(Count >= 1)
            ret = diff_datasets(SPACE1_DIM1,SPACE1_DIM2, (DTYPE_INT *)w_dset_info[0].u.wbuf, (DTYPE_INT *)r_dset_info[0].u.rbuf,1);
        if(Count > 1) 
            ret = diff_datasets(SPACE1_DIM1,SPACE1_DIM2, (DTYPE_INT *)w_dset_info[1].u.wbuf, (DTYPE_INT *)r_dset_info[1].u.rbuf,2);
    }

    MESG("H5Dwrite succeed");

    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    /* release all temporary handles. */
    for (i=0;i< Count;i++)
    {
        if(w_dset_info[i].mem_space_id > 0)
        {
            ret = H5Sclose(w_dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].mem_space_id > 0)
        {
            ret = H5Sclose(r_dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(w_dset_info[i].dset_space_id > 0)
        {
            ret = H5Sclose(w_dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].dset_space_id > 0)
        {
            ret = H5Sclose(r_dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(w_dset_info[i].dset_id > 0)
        {
            ret = H5Dclose(w_dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].dset_id > 0)
        {
            ret = H5Dclose(r_dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        MESG("H5Dclose succeed");
    }

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    H5Fclose(fid);
}


/* This test with one or two CONTIG dset */
void
phdf5Write_mdset_Contig(char *filename, phdf5_mode_t pmode, mpio_collective_mode_t mpio_opt, sel_mode_t sel_mode, multi_mode_t multi_mode)
{
    int i;
    int Count;
    hid_t fid;			/* HDF5 file IDs */
    hid_t sid;   		/* Dataspace ID */
    hsize_t dims[SPACE1_RANK] = {SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    H5D_rw_multi_t dset_info[NDSET_CONTIG];
    hid_t file_dataspace3, file_dataspace4;	/* File dataspace ID */
    hid_t mem_dataspace3=-1, mem_dataspace4=-1;	/* memory dataspace ID */
    hid_t dataset3, dataset4;	/* Dataset ID */
    hsize_t chunk_dims[SPACE1_RANK];	/* dataspace dim sizes */

    DTYPE_INT data_array3[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array4[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    hsize_t start3[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count3[SPACE1_RANK];  
    hsize_t stride3[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block3[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start4[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count4[SPACE1_RANK];  
    hsize_t stride4[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block4[SPACE1_RANK];	/* for hyperslab setting */

    /* point selection */
    hsize_t pcoord[NPOINT][SPACE1_RANK];

    herr_t ret;         	/* Generic return value */

#ifndef TEST_NO_MPI
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
#endif

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    // init dset_info[]
    for (i=0; i< NDSET_CONTIG ; i++)
        memset(&dset_info[i], 0, sizeof(H5D_rw_multi_t));

    // Init data_array
    // Wbuf (init all 9)
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array3[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array4[0][0],INIT_VAL);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl != FAIL);
    MESG("H5Pcreate access succeed");
#ifndef TEST_NO_MPI
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");
#endif

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    assert(fid != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    assert(ret != FAIL);


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality dsets */
    sid = H5Screate_simple (SPACE1_RANK, dims, NULL);
    assert (sid != FAIL);
    MESG("H5Screate_simple succeed");


    /* ==========================================
     * set up for contig Dset3 */
    /* create a dataset collectively */
    dataset3 = H5Dcreate2(fid, DATASETNAME3, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset3 != FAIL);
    MESG("H5Dcreate2 succeed");

#ifdef TEST_TWO_CONTIG
    /* ==========================================
     * set up for contig Dset4 */
    /* create a dataset collectively */
    dataset4 = H5Dcreate2(fid, DATASETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset4 != FAIL);
    MESG("H5Dcreate2 succeed");
#endif // TEST_TWO_CONTIG


    /*
     * Set up dimensions of the slab this process accesses.
     */
#if 1 // JK_DBG
	printf("== Write DSET 3 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        slab_set(start3, stride3, count3, block3, BYROW);
        //slab_set(start3, stride3, count3, block3, BYROW2);
        //slab_set(start3, stride3, count3, block3, BYCOL);
        //slab_set(start3, stride3, count3, block3, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start3, stride3, count3, block3, BYROW_M);
        slab_set(start3, stride3, count3, block3, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start3[]=(%lu,%lu), stride3[]=(%lu,%lu), count3[]=(%lu,%lu), block3[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start3[0], (unsigned long)start3[1],
       (unsigned long)stride3[0], (unsigned long)stride3[1],
       (unsigned long)count3[0], (unsigned long)count3[1],
       (unsigned long)block3[0], (unsigned long)block3[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace3 = H5Dget_space (dataset3);
    assert(file_dataspace3 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple (SPACE1_RANK, count3, NULL);
        ret=H5Sselect_hyperslab(file_dataspace3, H5S_SELECT_SET, start3, stride3, count3, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace3 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace3, H5S_SELECT_SET, start3, stride3, count3, block3);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple (SPACE1_RANK, count3, NULL);

        ret=H5Sselect_none(file_dataspace3);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace3);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start3[0] = pcoord[0][0]; start3[1] = pcoord[0][1];
        stride3[0] = 1; stride3[1] = 1;
        count3[0] = 1; count3[1] = 2;
        block3[0] = 1; block3[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace3, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace3, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank; 
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start3[0] = pcoord[0][0]; start3[1] = pcoord[0][1];
        stride3[0] = 1; stride3[1] = 1;
        count3[0] = 1; count3[1] = 1;
        block3[0] = 1; block3[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif

        ret = H5Sselect_elements (file_dataspace3, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace3, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start3, count3, stride3, &data_array3[0][0],0);
    else
        dataset_fill2_2d(start3, stride3, count3, block3, &data_array3[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start3, count3, stride3, &data_array3[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset3 Write Data ---\n");
    //dset_select_display(start3, count3, stride3, &data_array3[0][0],0);
    dataset_print(start3, count3, stride3, &data_array3[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array3[0][0], 0);
#endif

#ifdef TEST_TWO_CONTIG
#if 1 // JK_DBG
	printf("== Write DSET 4 =============================\n");
#endif
    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start4, stride4, count4, block4, BYROW);
        //slab_set(start4, stride4, count4, block4, BYROW2);
        //slab_set(start4, stride4, count4, block4, BYCOL);
        slab_set(start4, stride4, count4, block4, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start4, stride4, count4, block4, BYROW_M);
        slab_set(start4, stride4, count4, block4, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start4[]=(%lu,%lu), stride4[]=(%lu,%lu), count4[]=(%lu,%lu), block4[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start4[0], (unsigned long)start4[1],
       (unsigned long)stride4[0], (unsigned long)stride4[1],
       (unsigned long)count4[0], (unsigned long)count4[1],
       (unsigned long)block4[0], (unsigned long)block4[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace4 = H5Dget_space (dataset4);
    assert(file_dataspace4 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple (SPACE1_RANK, count4, NULL);
        ret=H5Sselect_hyperslab(file_dataspace4, H5S_SELECT_SET, start4, stride4, count4, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if(sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace4 = H5S_ALL;
        ret=H5Sselect_hyperslab(file_dataspace4, H5S_SELECT_SET, start4, stride4, count4, block4);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple (SPACE1_RANK, count4, NULL);
        ret=H5Sselect_none(file_dataspace4);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace4);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start4[0] = pcoord[1][0]; start4[1] = pcoord[1][1] - 1;
        stride4[0] = 1; stride4[1] = 1;
        count4[0] = 1; count4[1] = 2;
        block4[0] = 1; block4[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace4, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace4, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank;
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start4[0] = pcoord[0][0]; start4[1] = pcoord[0][1];
        stride4[0] = 1; stride4[1] = 1;
        count4[0] = 1; count4[1] = 1;
        block4[0] = 1; block4[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif

        ret = H5Sselect_elements (file_dataspace4, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace4, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start4, count4, stride4, &data_array4[0][0],0);
    else
        dataset_fill2_2d(start4, stride4, count4, block4, &data_array4[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start4, count4, stride4, &data_array4[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset4 Write Data ---\n");
    //dset_select_display(start4, count4, stride4, &data_array4[0][0],0);
    dataset_print(start4, count4, stride4, &data_array4[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array4[0][0], 0);
#endif

#endif // TEST_TWO_CONTIG


    /*==================================================
     * set up the collective transfer properties list 
     */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    MESG("H5Pcreate xfer succeed");

#ifndef TEST_NO_MPI
    if (pmode == PHDF5_PARALLEL)
    {
        ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        assert(ret != FAIL);

        /* In parallel mode, do collective IO (MPI_File_write_at_all) */
        if (mpio_opt == MPIO_COLLECTIVE_IO) {
            /* Note: this is default, so don't need to set explicitly */
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_COLLECTIVE_IO);
            assert(ret >= 0);

            /* Note: this is default, so don't need to set explicitly 
             * Just leave this here for a reference if add more options later */
            /*
            ret = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
            assert(ret >= 0);
            */
        }
        /* In parallel mode, do independent IO (MPI_File_write_at) */
        else if (mpio_opt == MPIO_INDIVIDUAL_IO) {
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
            assert(ret >= 0);
        }
    }
#endif

    /*
     * Set up dset info structure 
     * HDF5 lib will handle by order of dset_info[X] X
     */
#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC //------------------------I
   if (mpi_rank == 0) {
#if 1 // USE_DSET3
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        dset_info[0].dset_id = dataset3;
        dset_info[0].mem_type_id = H5T_NATIVE_INT;
        dset_info[0].mem_space_id = mem_dataspace3;
        dset_info[0].dset_space_id = file_dataspace3;
        dset_info[0].u.wbuf = &data_array3[0][0];
#else
        /* free before rewrite DSET3 */
        if(mem_dataspace3)  { ret = H5Sclose(mem_dataspace3); assert(ret != FAIL); }
        if(file_dataspace3) { ret = H5Sclose(file_dataspace3); assert(ret != FAIL); }
        if(dataset3) { ret = H5Dclose(dataset3); assert(ret != FAIL); }
#endif
        
#if 1 // USE_DSET4
        // init H5D_rw_multi_t for write DSET4 (CONTIG)
        dset_info[1].dset_id = dataset4;
        dset_info[1].mem_type_id = H5T_NATIVE_INT;
        dset_info[1].mem_space_id = mem_dataspace4;
        dset_info[1].dset_space_id = file_dataspace4;
        dset_info[1].u.wbuf = &data_array4[0][0];
#else
        /* free before rewrite DSET4 */
        if(mem_dataspace4)  { ret = H5Sclose(mem_dataspace4); assert(ret != FAIL); }
        if(file_dataspace4) { ret = H5Sclose(file_dataspace4); assert(ret != FAIL); }
        if(dataset4) { ret = H5Dclose(dataset4); assert(ret != FAIL); }
#endif
   } // mpi_rank == 0

   if (mpi_rank == 1) {
#if 1 // USE_DSET3
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        dset_info[0].dset_id = dataset3;
        dset_info[0].mem_type_id = H5T_NATIVE_INT;
        dset_info[0].mem_space_id = mem_dataspace3;
        dset_info[0].dset_space_id = file_dataspace3;
        dset_info[0].u.wbuf = &data_array3[0][0];
#else
        /* free before rewrite DSET3 */
        if(mem_dataspace3)  { ret = H5Sclose(mem_dataspace3); assert(ret != FAIL); }
        if(file_dataspace3) { ret = H5Sclose(file_dataspace3); assert(ret != FAIL); }
        if(dataset3) { ret = H5Dclose(dataset3); assert(ret != FAIL); }
#endif
        
#if 0 // USE_DSET4
        // init H5D_rw_multi_t for write DSET4 (CONTIG)
        dset_info[1].dset_id = dataset4;
        dset_info[1].mem_type_id = H5T_NATIVE_INT;
        dset_info[1].mem_space_id = mem_dataspace4;
        dset_info[1].dset_space_id = file_dataspace4;
        dset_info[1].u.wbuf = &data_array4[0][0];
#else
        /* free before rewrite DSET4 */
        if(mem_dataspace4)  { ret = H5Sclose(mem_dataspace4); assert(ret != FAIL); }
        if(file_dataspace4) { ret = H5Sclose(file_dataspace4); assert(ret != FAIL); }
        if(dataset4) { ret = H5Dclose(dataset4); assert(ret != FAIL); }
#endif
   } // mpi_rank == 1

#else // TEST_MDSET_NO_LAST_DSET_2ND_PROC ---------
    // init H5D_rw_multi_t for write DSET3 (CONTIG)
    dset_info[0].dset_id = dataset3;
    dset_info[0].mem_type_id = H5T_NATIVE_INT;
    dset_info[0].mem_space_id = mem_dataspace3;
    dset_info[0].dset_space_id = file_dataspace3;
    dset_info[0].u.wbuf = &data_array3[0][0];


#ifdef TEST_TWO_CONTIG
    // init H5D_rw_multi_t for write DSET4 (CONTIG)
    dset_info[1].dset_id = dataset4;
    dset_info[1].mem_type_id = H5T_NATIVE_INT;
    dset_info[1].mem_space_id = mem_dataspace4;
    dset_info[1].dset_space_id = file_dataspace4;
    dset_info[1].u.wbuf = &data_array4[0][0];
#endif
#endif // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------O

#if 0 // JK_DBG
    for (i=0; i< NDSET_CONTIG ; i++)
        printf("%s P%d > dset_info[%d].u.wbuf addr: %x\n", __FUNCTION__, mpi_rank, i, dset_info[i].u.wbuf);
#endif

#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC
  if (mpi_rank == 0)      Count = 2;
  if (mpi_rank == 1)      Count = 1;
#else
  Count = NDSET_CONTIG;
#endif

    if(multi_mode==MULTI_DSET) {
        ret = H5Dwrite_multi(fid, xfer_plist, Count, dset_info);
        assert(ret != FAIL);
#ifdef TEST_DOUBLE_WR_BEFORE_CLOSE
        /* test multiple write before close */
sync();
        ret = H5Dwrite_multi(fid, xfer_plist, Count, dset_info);
        assert(ret != FAIL);
#endif
    }
    else if(multi_mode==SINGLE_DSET) {
        for (i=0; i<Count; i++)
            ret = H5Dwrite(dset_info[i].dset_id, dset_info[i].mem_type_id, dset_info[i].mem_space_id, dset_info[i].dset_space_id, xfer_plist, dset_info[i].u.wbuf );
    }

    MESG("H5Dwrite succeed");

    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    /* release all temporary handles. */
    for (i=0;i< Count;i++)
    {
        if(dset_info[i].mem_space_id > 0)
        {
            ret = H5Sclose(dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(dset_info[i].dset_space_id > 0)
        {
            ret = H5Sclose(dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(dset_info[i].dset_id > 0)
        {
            ret = H5Dclose(dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        MESG("H5Dclose succeed");
    }

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    H5Fclose(fid);
}

/* This test with one or two CONTIG dset */
void
phdf5Read_mdset_Contig(char *filename, phdf5_mode_t pmode, mpio_collective_mode_t mpio_opt, sel_mode_t sel_mode, multi_mode_t multi_mode)
{
    int i;
    int Count;
    hid_t fid;			/* HDF5 file IDs */
    hid_t sid;   		/* Dataspace ID */
    hsize_t dims[SPACE1_RANK] = {SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    H5D_rw_multi_t w_dset_info[NDSET_CONTIG];
    H5D_rw_multi_t r_dset_info[NDSET_CONTIG];
    hid_t file_dataspace3, file_dataspace4;	/* File dataspace ID */
    hid_t mem_dataspace3=-1, mem_dataspace4=-1;	/* memory dataspace ID */
    hid_t dataset3, dataset4;	/* Dataset ID */
    hsize_t chunk_dims[SPACE1_RANK];	/* dataspace dim sizes */

    /* Write Buffer */
    DTYPE_INT data_array3_w[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array4_w[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    /* Read Buffer */
    DTYPE_INT data_array3_r[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */
    DTYPE_INT data_array4_r[SPACE1_DIM1][SPACE1_DIM2]={{0,},{0,}};	/* data buffer */

    hsize_t start3[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count3[SPACE1_RANK];  
    hsize_t stride3[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block3[SPACE1_RANK];	/* for hyperslab setting */

    hsize_t start4[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count4[SPACE1_RANK];  
    hsize_t stride4[SPACE1_RANK];	/* for hyperslab setting */
    hsize_t block4[SPACE1_RANK];	/* for hyperslab setting */

    /* point selection */
    hsize_t pcoord[NPOINT][SPACE1_RANK];

    herr_t ret;         	/* Generic return value */

#ifndef TEST_NO_MPI
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
#endif

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    // init [w/r]_dset_info[]
    for (i=0; i< NDSET_CONTIG ; i++) {
        memset(&w_dset_info[i], 0, sizeof(H5D_rw_multi_t));
        memset(&r_dset_info[i], 0, sizeof(H5D_rw_multi_t));
    }

    // Init data_array
    // Wbuf (init all 9)
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array3_w[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array4_w[0][0],INIT_VAL);

    // Rbuf (init all 9)
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array3_r[0][0],INIT_VAL);
    data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array4_r[0][0],INIT_VAL);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl != FAIL);
    MESG("H5Pcreate access succeed");
#ifndef TEST_NO_MPI
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");
#endif

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    assert(fid != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    assert(ret != FAIL);


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality dsets */
    sid = H5Screate_simple (SPACE1_RANK, dims, NULL);
    assert (sid != FAIL);
    MESG("H5Screate_simple succeed");


    /* ==========================================
     * set up for contig Dset3 */
    /* create a dataset collectively */
    dataset3 = H5Dcreate2(fid, DATASETNAME3, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset3 != FAIL);
    MESG("H5Dcreate2 succeed");

#ifdef TEST_TWO_CONTIG
    /* ==========================================
     * set up for contig Dset4 */
    /* create a dataset collectively */
    dataset4 = H5Dcreate2(fid, DATASETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset4 != FAIL);
    MESG("H5Dcreate2 succeed");
#endif // TEST_TWO_CONTIG


    /*
     * Set up dimensions of the slab this process accesses.
     */
#if 1 // JK_DBG
	printf("== Write DSET 3 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        slab_set(start3, stride3, count3, block3, BYROW);
        //slab_set(start3, stride3, count3, block3, BYROW2);
        //slab_set(start3, stride3, count3, block3, BYCOL);
        //slab_set(start3, stride3, count3, block3, BYCOL2);
    }
    else {
        /* each process takes a block. */
        //slab_set(start3, stride3, count3, block3, BYROW_M);
        slab_set(start3, stride3, count3, block3, BYCOL_M);
    }
     
#if 0 // JK_DBG
    printf("%s P%d > start3[]=(%lu,%lu), stride3[]=(%lu,%lu), count3[]=(%lu,%lu), block3[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start3[0], (unsigned long)start3[1],
       (unsigned long)stride3[0], (unsigned long)stride3[1],
       (unsigned long)count3[0], (unsigned long)count3[1],
       (unsigned long)block3[0], (unsigned long)block3[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace3 = H5Dget_space (dataset3);
    assert(file_dataspace3 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple (SPACE1_RANK, count3, NULL);

        ret=H5Sselect_hyperslab(file_dataspace3, H5S_SELECT_SET, start3, stride3, count3, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace3 = H5S_ALL;

        ret=H5Sselect_hyperslab(file_dataspace3, H5S_SELECT_SET, start3, stride3, count3, block3);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple (SPACE1_RANK, count3, NULL);

        ret=H5Sselect_none(file_dataspace3);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace3);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start3[0] = pcoord[0][0]; start3[1] = pcoord[0][1];
        stride3[0] = 1; stride3[1] = 1;
        count3[0] = 1; count3[1] = 2;
        block3[0] = 1; block3[1] = 1;
       
#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace3, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace3, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank;
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start3[0] = pcoord[0][0]; start3[1] = pcoord[0][1];
        stride3[0] = 1; stride3[1] = 1;
        count3[0] = 1; count3[1] = 1;
        block3[0] = 1; block3[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif

        ret = H5Sselect_elements (file_dataspace3, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace3 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace3, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start3, count3, stride3, &data_array3_w[0][0],0);
    else
        dataset_fill2_2d(start3, stride3, count3, block3, &data_array3_w[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start3, count3, stride3, &data_array3_w[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset3 Write Data ---\n");
    //dset_select_display(start3, count3, stride3, &data_array3_w[0][0],0);
    dataset_print(start3, count3, stride3, &data_array3_w[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array3_w[0][0], 0);
#endif

#ifdef TEST_TWO_CONTIG
#if 1 // JK_DBG
	printf("== Write DSET 4 =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        //slab_set(start4, stride4, count4, block4, BYROW);
        //slab_set(start4, stride4, count4, block4, BYROW2);
        //slab_set(start4, stride4, count4, block4, BYCOL);
        slab_set(start4, stride4, count4, block4, BYCOL2);
    }
    else {
        /* each process takes a block */
        //slab_set(start4, stride4, count4, block4, BYROW_M);
        slab_set(start4, stride4, count4, block4, BYCOL_M);
    }

#if 0 // JK_DBG
    printf("%s P%d > start4[]=(%lu,%lu), stride4[]=(%lu,%lu), count4[]=(%lu,%lu), block4[]=(%lu,%lu),\n",
       __FUNCTION__, mpi_rank, 
	   (unsigned long)start4[0], (unsigned long)start4[1],
       (unsigned long)stride4[0], (unsigned long)stride4[1],
       (unsigned long)count4[0], (unsigned long)count4[1],
       (unsigned long)block4[0], (unsigned long)block4[1]);
#endif

    /* create a file dataspace independently */
    file_dataspace4 = H5Dget_space (dataset4);
    assert(file_dataspace4 != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple (SPACE1_RANK, count4, NULL);

        ret=H5Sselect_hyperslab(file_dataspace4, H5S_SELECT_SET, start4, stride4, count4, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if(sel_mode == SEL_HYPER_BLOCKS)
    {
        mem_dataspace4 = H5S_ALL;

        ret=H5Sselect_hyperslab(file_dataspace4, H5S_SELECT_SET, start4, stride4, count4, block4);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_NONE)
    {
        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple (SPACE1_RANK, count4, NULL);

        ret=H5Sselect_none(file_dataspace4);
        assert(ret != FAIL);
        ret=H5Sselect_none(mem_dataspace4);
        assert(ret != FAIL);
    }
    else if (sel_mode == SEL_POINTS)
    {
        pcoord[0][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[0][1] = (SPACE1_DIM2/mpi_size)*mpi_rank;
        pcoord[1][0] = (SPACE1_DIM1/mpi_size)*mpi_rank; pcoord[1][1] = (SPACE1_DIM2/mpi_size)*mpi_rank + 1;

        // match hyperslab same as 2 POINTs space selection
        start4[0] = pcoord[1][0]; start4[1] = pcoord[1][1] - 1;
        stride4[0] = 1; stride4[1] = 1;
        count4[0] = 1; count4[1] = 2;
        block4[0] = 1; block4[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]  p2[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1], pcoord[1][0], pcoord[1][1] );
#endif

        ret = H5Sselect_elements (file_dataspace4, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace4, H5S_SELECT_SET, NPOINT, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }
    else if (sel_mode == SEL_1POINT)
    {
        pcoord[0][0] = mpi_rank;
        pcoord[0][1] = mpi_rank;

        // match hyperslab same as 1 POINT space selection
        start4[0] = pcoord[0][0]; start4[1] = pcoord[0][1];
        stride4[0] = 1; stride4[1] = 1;
        count4[0] = 1; count4[1] = 1;
        block4[0] = 1; block4[1] = 1;

#if 0 // JK_DBG
        printf("JKDBG P%d> p1[%llu,%llu]\n",mpi_rank, pcoord[0][0], pcoord[0][1]);
#endif

        ret = H5Sselect_elements (file_dataspace4, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements fspace succeed");

        /* create a memory dataspace independently */
        mem_dataspace4 = H5Screate_simple(SPACE1_RANK, dims, NULL);

        ret = H5Sselect_elements (mem_dataspace4, H5S_SELECT_SET, 1, (const hsize_t *)pcoord);
        assert(ret != FAIL);
        MESG("H5Sselect_elements mspace succeed");
    }

    /* fill the local slab with data indicate process rank */
    if (sel_mode == SEL_HYPER_1BLOCK)
        dataset_fill_2d(start4, count4, stride4, &data_array4_w[0][0],0);
    else
        dataset_fill2_2d(start4, stride4, count4, block4, &data_array4_w[0][0],0);

    MESG("data_array initialized");
    if (verbose){
	    MESG("data_array created");
    	dataset_print(start4, count4, stride4, &data_array4_w[0][0]);
    }
#if 0 // JK_DBG
	printf("--- dset4 Write Data ---\n");
    //dset_select_display(start4, count4, stride4, &data_array4_w[0][0],0);
    dataset_print(start4, count4, stride4, &data_array4_w[0][0]);
    //dset_display(SPACE1_DIM1, SPACE1_DIM2, &data_array4_w[0][0], 0);
#endif

#endif // TEST_TWO_CONTIG


    /*==================================================
     * set up the collective transfer properties list 
     */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    MESG("H5Pcreate xfer succeed");

#ifndef TEST_NO_MPI
    if (pmode == PHDF5_PARALLEL)
    {
        ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        assert(ret != FAIL);

        /* In parallel mode, do collective IO (MPI_File_write_at_all) */
        if (mpio_opt == MPIO_COLLECTIVE_IO) {
            /* Note: this is default, so don't need to set explicitly */
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_COLLECTIVE_IO);
            assert(ret >= 0);

            /* Note: this is default, so don't need to set explicitly 
             * Just leave this here for a reference if add more options later */
            /*
            ret = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
            assert(ret >= 0);
            */
        }
        /* In parallel mode, do independent IO (MPI_File_write_at) */
        else if (mpio_opt == MPIO_INDIVIDUAL_IO) {
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
            assert(ret >= 0);
        }
    }
#endif

    /*
     * Set up dset info structure 
     * HDF5 lib will handle by order of [w/r]_dset_info[X] X
     */
#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC //------------------------I
   if (mpi_rank == 0) {
#if 1 // USE_DSET3
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        w_dset_info[0].dset_id = dataset3;
        w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[0].mem_space_id = mem_dataspace3;
        w_dset_info[0].dset_space_id = file_dataspace3;
        w_dset_info[0].u.wbuf = &data_array3_w[0][0];

        r_dset_info[0].dset_id = dataset3;
        r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[0].mem_space_id = mem_dataspace3;
        r_dset_info[0].dset_space_id = file_dataspace3;
        r_dset_info[0].u.rbuf = &data_array3_r[0][0];
#else
        /* free before rewrite DSET3 */
        if(mem_dataspace3)  { ret = H5Sclose(mem_dataspace3); assert(ret != FAIL); }
        if(file_dataspace3) { ret = H5Sclose(file_dataspace3); assert(ret != FAIL); }
        if(dataset3) { ret = H5Dclose(dataset3); assert(ret != FAIL); }
#endif

#if 1 // USE_DSET4
        // init H5D_rw_multi_t for write DSET4 (CONTIG)
        w_dset_info[1].dset_id = dataset4;
        w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[1].mem_space_id = mem_dataspace4;
        w_dset_info[1].dset_space_id = file_dataspace4;
        w_dset_info[1].u.wbuf = &data_array4_w[0][0];

        r_dset_info[1].dset_id = dataset4;
        r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[1].mem_space_id = mem_dataspace4;
        r_dset_info[1].dset_space_id = file_dataspace4;
        r_dset_info[1].u.rbuf = &data_array4_r[0][0];
#else
        /* free before rewrite DSET4 */
        if(mem_dataspace4)  { ret = H5Sclose(mem_dataspace4); assert(ret != FAIL); }
        if(file_dataspace4) { ret = H5Sclose(file_dataspace4); assert(ret != FAIL); }
        if(dataset4) { ret = H5Dclose(dataset4); assert(ret != FAIL); }
#endif
   } // mpi_rank == 0

   if (mpi_rank == 1) {
#if 1 // USE_DSET3
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        w_dset_info[0].dset_id = dataset3;
        w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[0].mem_space_id = mem_dataspace3;
        w_dset_info[0].dset_space_id = file_dataspace3;
        w_dset_info[0].u.wbuf = &data_array3_w[0][0];

        r_dset_info[0].dset_id = dataset3;
        r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[0].mem_space_id = mem_dataspace3;
        r_dset_info[0].dset_space_id = file_dataspace3;
        r_dset_info[0].u.rbuf = &data_array3_r[0][0];
#else
        /* free before rewrite DSET3 */
        if(mem_dataspace3)  { ret = H5Sclose(mem_dataspace3); assert(ret != FAIL); }
        if(file_dataspace3) { ret = H5Sclose(file_dataspace3); assert(ret != FAIL); }
        if(dataset3) { ret = H5Dclose(dataset3); assert(ret != FAIL); }
#endif

#if 0 // USE_DSET4
        // init H5D_rw_multi_t for write DSET4 (CONTIG)
        w_dset_info[1].dset_id = dataset4;
        w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[1].mem_space_id = mem_dataspace4;
        w_dset_info[1].dset_space_id = file_dataspace4;
        w_dset_info[1].u.wbuf = &data_array4_w[0][0];

        r_dset_info[1].dset_id = dataset4;
        r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[1].mem_space_id = mem_dataspace4;
        r_dset_info[1].dset_space_id = file_dataspace4;
        r_dset_info[1].u.rbuf = &data_array4_r[0][0];
#else
        /* free before rewrite DSET4 */
        if(mem_dataspace4)  { ret = H5Sclose(mem_dataspace4); assert(ret != FAIL); }
        if(file_dataspace4) { ret = H5Sclose(file_dataspace4); assert(ret != FAIL); }
        if(dataset4) { ret = H5Dclose(dataset4); assert(ret != FAIL); }
#endif
    } // mpi_rank == 1

#else // TEST_MDSET_NO_LAST_DSET_2ND_PROC ---------
    // init H5D_rw_multi_t for write DSET3 (CONTIG)
    w_dset_info[0].dset_id = dataset3;
    w_dset_info[0].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[0].mem_space_id = mem_dataspace3;
    w_dset_info[0].dset_space_id = file_dataspace3;
    w_dset_info[0].u.wbuf = &data_array3_w[0][0];

    r_dset_info[0].dset_id = dataset3;
    r_dset_info[0].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[0].mem_space_id = mem_dataspace3;
    r_dset_info[0].dset_space_id = file_dataspace3;
    r_dset_info[0].u.rbuf = &data_array3_r[0][0];

#ifdef TEST_TWO_CONTIG
    // init H5D_rw_multi_t for write DSET4 (CONTIG)
    w_dset_info[1].dset_id = dataset4;
    w_dset_info[1].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[1].mem_space_id = mem_dataspace4;
    w_dset_info[1].dset_space_id = file_dataspace4;
    w_dset_info[1].u.wbuf = &data_array4_w[0][0];

    r_dset_info[1].dset_id = dataset4;
    r_dset_info[1].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[1].mem_space_id = mem_dataspace4;
    r_dset_info[1].dset_space_id = file_dataspace4;
    r_dset_info[1].u.rbuf = &data_array4_r[0][0];
#endif
#endif // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------O

#if 0 // JK_DBG
    for (i=0; i< NDSET_CONTIG ; i++) {
        printf("%s P%d > w_dset_info[%d].u.wbuf addr: %x\n", __FUNCTION__, mpi_rank, i, w_dset_info[i].u.wbuf);
        printf("%s P%d > r_dset_info[%d].u.rbuf addr: %x\n", __FUNCTION__, mpi_rank, i, r_dset_info[i].u.rbuf);
    }
#endif

#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC
    if (mpi_rank == 0)      Count = 2;
    if (mpi_rank == 1)      Count = 1;
#else
    Count = NDSET_CONTIG;
#endif

    if(multi_mode==MULTI_DSET) {
        ret = H5Dwrite_multi(fid, xfer_plist, Count, w_dset_info);
        assert(ret != FAIL);
#ifdef TEST_DOUBLE_WR_BEFORE_CLOSE
        /* test multiple write before close */
sync();
        ret = H5Dwrite_multi(fid, xfer_plist, Count, w_dset_info);
        assert(ret != FAIL);
#endif
        
        // Read
        ret = H5Dread_multi(fid, xfer_plist, Count, r_dset_info);
        assert(ret != FAIL);

#ifdef TEST_DOUBLE_RD_BEFORE_CLOSE
        /* test multiple read before close */
sync();
        ret = H5Dread_multi(fid, xfer_plist, Count, r_dset_info);
        assert(ret != FAIL);
#endif
    }
    else if(multi_mode==SINGLE_DSET) {
        // Write
        for (i=0; i<Count; i++) {
            ret = H5Dwrite(w_dset_info[i].dset_id, w_dset_info[i].mem_type_id, w_dset_info[i].mem_space_id, w_dset_info[i].dset_space_id, xfer_plist, w_dset_info[i].u.wbuf);
            assert(ret != FAIL);
        }

        // Read
        for (i=0; i<Count; i++) {
            ret = H5Dread(r_dset_info[i].dset_id, r_dset_info[i].mem_type_id, r_dset_info[i].mem_space_id, r_dset_info[i].dset_space_id, xfer_plist, r_dset_info[i].u.rbuf);
            assert(ret != FAIL);
        }
    }

    // Verify Read buffer with Write buffer 
    if(sel_mode == SEL_HYPER_1BLOCK || sel_mode == SEL_POINTS || sel_mode == SEL_1POINT) {
        if(Count >=1)  // relate to TEST_MDSET_NO_LAST_DSET_2ND_PROC
            ret = dataset_vrfy2(start3, stride3, count3, block3, (DTYPE_INT *)w_dset_info[0].u.wbuf, (DTYPE_INT *)r_dset_info[0].u.rbuf,1);
        if(Count > 1)   // relate to TEST_MDSET_NO_LAST_DSET_2ND_PROC 
            ret = dataset_vrfy2(start4, stride4, count4, block4, (DTYPE_INT *)w_dset_info[1].u.wbuf, (DTYPE_INT *)r_dset_info[1].u.rbuf,2);
    }
    else if (sel_mode == SEL_NONE) {
        // init the wbuf, as rbuf shouldn't read any 
        data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array3_w[0][0],INIT_VAL);
        data_array2d_init(SPACE1_DIM1,SPACE1_DIM2, &data_array4_w[0][0],INIT_VAL);
        if(Count >= 1)
            ret = diff_datasets(SPACE1_DIM1,SPACE1_DIM2, (DTYPE_INT *)w_dset_info[0].u.wbuf, (DTYPE_INT *)r_dset_info[0].u.rbuf,1);
        if(Count > 1) 
            ret = diff_datasets(SPACE1_DIM1,SPACE1_DIM2, (DTYPE_INT *)w_dset_info[1].u.wbuf, (DTYPE_INT *)r_dset_info[1].u.rbuf,2);
    }

    MESG("H5Dwrite succeed");

    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    /* release all temporary handles. */
    for (i=0;i< Count;i++)
    {
        if(w_dset_info[i].mem_space_id > 0)
        {
            ret = H5Sclose(w_dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].mem_space_id > 0)
        {
            ret = H5Sclose(r_dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(w_dset_info[i].dset_space_id > 0)
        {
            ret = H5Sclose(w_dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].dset_space_id > 0)
        {
            ret = H5Sclose(r_dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(w_dset_info[i].dset_id > 0)
        {
            ret = H5Dclose(w_dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].dset_id > 0)
        {
            ret = H5Dclose(r_dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        MESG("H5Dclose succeed");
    }

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    H5Fclose(fid);
}

/*---------------------------------------------------
 *  Scailable & performance test for Write I/O.
 *  All processes write its own portion to all dsets.
 *  Generate multiple dsets (CONTIG or CHUNKED) either with
 *  H5Dread or H5Dread_multi.
 */
void phdf5Write_mdset_many(char *filename, size_t ndsets, hsize_t dim0, hsize_t chunks, phdf5_mode_t pmode, multi_mode_t multi_mode, mpio_collective_mode_t mpio_opt, sel_mode_t sel_mode )
{
    int i,j;
    int Count;
    hid_t fid;			/* HDF5 file IDs */
    hid_t sid;   		/* Dataspace ID */
    hsize_t dims[RANK_PERF] = {dim0};	/* dataspace dim sizes */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    H5D_rw_multi_t *dset_info=NULL;
    hid_t crp_plist1;
    hid_t f_sid;	/* File dataspace ID */
    hid_t m_sid=-1;	/* memory dataspace ID */
    hid_t did;	/* Dataset ID */
    hsize_t chunk_dims[RANK_PERF];

    int *data_array=NULL;	/* data buffer */

    hsize_t start[RANK_PERF];			/* for hyperslab setting */
    hsize_t count[RANK_PERF];  
    hsize_t stride[RANK_PERF];	/* for hyperslab setting */
    hsize_t block[RANK_PERF];	/* for hyperslab setting */

    char        Dname[32]=""; // dset name

    struct timeval tv1, tv2, tv_sub, tv_add, tv_max, tv_min;
    float time_f=0, timemax_f=0, timemin_f=0;

    herr_t ret;         	/* Generic return value */

#ifndef TEST_NO_MPI
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
#endif

    timerclear(&tv_sub);
    timerclear(&tv_add);

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    if (verbose)
     if(mpi_rank==0) {
         printf("INFO - P%d > fname: %s, #dset: %zu, dim-size:%llu\n",
                mpi_rank, filename,  ndsets, (unsigned long long)dim0);
         if(multi_mode==MULTI_DSET)
             printf("> multi mode: H5Dwrite_multi Test\n");
         else if(multi_mode==SINGLE_DSET)
             printf("> multi mode: H5Dwrite Test\n");
#ifdef TEST_NO_MPI
             printf("> NO MPI!\n");
#endif
         fflush(stdout);
     }
    

    if(NULL == (dset_info = (H5D_rw_multi_t*) calloc( ndsets , sizeof(H5D_rw_multi_t)))) {
        printf("Error: failed malloc");
        assert(0);
    }

    data_array = (int *)calloc(dims[0],sizeof(int));

    // Init data_array
    data_array1d_init(dims[0], &data_array[0],INIT_VAL);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl != FAIL);
    MESG("H5Pcreate access succeed");
#ifndef TEST_NO_MPI
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");
#endif

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    assert(fid != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    assert(ret != FAIL);

    /*==================================================
     * set up the collective transfer properties list 
     */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    MESG("H5Pcreate xfer succeed");

#ifndef TEST_NO_MPI
    if (pmode == PHDF5_PARALLEL)
    {
        ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        assert(ret != FAIL);

        /* In parallel mode, do collective IO (MPI_File_write_at_all) */
        if (mpio_opt == MPIO_COLLECTIVE_IO) {
            /* Note: this is default, so don't need to set explicitly */
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_COLLECTIVE_IO);
            assert(ret >= 0);

            /* Note: this is default, so don't need to set explicitly 
             * Just leave this here for a reference if add more options later */
            /*
            ret = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
            assert(ret >= 0);
            */
        }
        /* In parallel mode, do independent IO (MPI_File_write_at) */
        else if (mpio_opt == MPIO_INDIVIDUAL_IO) {
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
            assert(ret >= 0);
        }
    }
#endif


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality dsets */
    sid = H5Screate_simple (RANK_PERF, dims, NULL);
    assert (sid != FAIL);
    MESG("H5Screate_simple succeed");

    if (chunks > 0) // CHUNKED
    {
        if (chunks > dims[0])
            chunks = dims[0];
        else
        {
            /* ==========================================
             * set up for chunked Dset1 */
            crp_plist1 = H5Pcreate(H5P_DATASET_CREATE);
            assert (crp_plist1 != FAIL);
            chunk_dims[0] = chunks; 
            ret = H5Pset_chunk(crp_plist1, 1, chunk_dims);
            assert (ret != FAIL);
        }
        
        if(verbose && mpi_rank==0) {
            printf("INFO - %d: chunk-size: %llu\n",
                mpi_rank, (unsigned long long)chunk_dims[0]);
            if(multi_mode==MULTI_DSET)
                 printf("> multi mode: H5Dwrite_multi Test\n");
            else if(multi_mode==SINGLE_DSET)
                 printf("> multi mode: H5Dwrite Test\n");
#ifdef TEST_NO_MPI
                printf("> NO MPI!\n");
#endif
            fflush(stdout);
        }

    }
    else // CONTIG
        crp_plist1= H5P_DEFAULT;



   for (j=0; j<ndsets; j++)
       {
        /* Dset name */
        sprintf(Dname, "%05d_dset", j+1);

        /* ==========================================
         * set up for chunked Dset3 */
        /* create a dataset collectively */
        did = H5Dcreate2(fid, Dname, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist1, H5P_DEFAULT);
        assert(did != FAIL);
        MESG("H5Dcreate2 succeed");

        /*
         * Set up dimensions of the slab this process accesses.
         */
#if 0 // JK_DBG
            printf("== Write DSET  =============================\n");
#endif

        if (sel_mode == SEL_HYPER_1BLOCK) {
            /* each process takes a block */
            slab_set_1d(dims, start, stride, count, block, BYROW);
#if 1 // JK_DBG
            printf("%s P%d > start[]=%lu, count[]=%lu, stride[]=%lu, total datapoints=%lu\n", __FUNCTION__, mpi_rank, 
                (unsigned long)start[0], 
                (unsigned long)count[0], 
                (unsigned long)stride[0],
                (unsigned long)count[0]);
#endif
        }
        else {
            /* each process takes a block */
            slab_set_1d(dims, start, stride, count, block, BYROW_M); // for 1D
#if 0 // JK_DBG
            printf("%s P%d > start[]=%lu, stride[]=%lu, count[]=%lu, block[]=%lu,\n", __FUNCTION__, mpi_rank, 
                (unsigned long)start[0],
                (unsigned long)stride[0],
                (unsigned long)count[0], 
                (unsigned long)block[0]);
#endif
        }

        if (count[0] <= 0)
            assert(0 && "-n Number should be bigger than process#. Multiple to process# is ideal.");

        /* create a file dataspace independently */
        f_sid = H5Dget_space (did);
        assert(f_sid != FAIL);
        MESG("H5Dget_space succeed");

        if (sel_mode == SEL_HYPER_1BLOCK)
        {
            /* create a memory dataspace independently */
            m_sid = H5Screate_simple (RANK_PERF, count, NULL);

            ret=H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, stride, count, NULL);
            assert(ret != FAIL);
            MESG("H5Sset_hyperslab succeed");
        }
        else if (sel_mode == SEL_HYPER_BLOCKS)
        {
            m_sid = H5S_ALL;

            ret=H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, stride, count, block);
            assert(ret != FAIL);
            MESG("H5Sset_hyperslab succeed");
        }

        /* fill the local slab with some trivial data */
        if (sel_mode == SEL_HYPER_1BLOCK)
            dataset_fill_1d(start, count, stride, &data_array[0],1);
        else {
            dataset_fill2_1d(start, stride, count, block, &data_array[0],0);
            //dataset_fill3_2d(start, stride, count, block, &data_array[0][0],0,BYROW_M);
        }

        MESG("data_array initialized");
        if (verbose){
                MESG("data_array created");
            dataset_print_1d(start, count, stride, &data_array[0]);
        }
#if 0 // JK_DBG
            printf("--- dset3 Write Data ---\n");
        //dset_select_display(start, count, stride, &data_array[0][0],0);
        dataset_print_1d(start, count, stride, &data_array[0]);
#endif


        /*
         * Set up dset info structure 
         * HDF5 lib will handle by order of dset_info[X] X
         */
#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC //------------------------I
        if (mpi_rank == 0) {
#if 1 // USE_DSET3
            // init H5D_rw_multi_t for write DSET3 (CONTIG)
            dset_info[j].dset_id = did;
            dset_info[j].mem_type_id = H5T_NATIVE_INT;
            dset_info[j].mem_space_id = m_sid;
            dset_info[j].dset_space_id = f_sid;
            dset_info[j].u.wbuf = &data_array[0];
#else
            /* free before rewrite DSET3 */
            if(m_sid)  { ret = H5Sclose(m_sid); assert(ret != FAIL); }
            if(f_sid) { ret = H5Sclose(f_sid); assert(ret != FAIL); }
            if(did) { ret = H5Dclose(did); assert(ret != FAIL); }
#endif
        }

        if (mpi_rank == 1) {
#if 1 // USE_DSET3
            // init H5D_rw_multi_t for write DSET3 (CONTIG)
            dset_info[j].dset_id = did;
            dset_info[j].mem_type_id = H5T_NATIVE_INT;
            dset_info[j].mem_space_id = m_sid;
            dset_info[j].dset_space_id = f_sid;
            dset_info[j].u.wbuf = &data_array[0];
#else
            /* free before rewrite DSET3 */
            if(m_sid)  { ret = H5Sclose(m_sid); assert(ret != FAIL); }
            if(f_sid) { ret = H5Sclose(f_sid); assert(ret != FAIL); }
            if(did) { ret = H5Dclose(did); assert(ret != FAIL); }
#endif
        }

#else  // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------------------
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        dset_info[j].dset_id = did;
        dset_info[j].mem_type_id = H5T_NATIVE_INT;
        dset_info[j].mem_space_id = m_sid;
        dset_info[j].dset_space_id = f_sid;
        dset_info[j].u.wbuf = &data_array[0];
#endif // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------------------O
    } // end of for loop #dsets

  // can close chunk prop
  ret = H5Pclose(crp_plist1);
  assert (ret != FAIL);

#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC
    if (mpi_rank == 0)      Count = ndsets;
    if (mpi_rank == 1)      Count = ndsets;
#else
    Count = ndsets;
#endif

#if 0 // JK_DBG
    for (i=0; i< Count ; i++)
        printf("%s:%d P%d > dset_info[%d].u.wbuf addr: %x\n", __FUNCTION__,__LINE__, mpi_rank, i, dset_info[i].u.wbuf);
#endif

    if(multi_mode == SINGLE_DSET) {
        for(j=0; j< Count; j++) {
            gettimeofday (&tv1, NULL); 
            ret = H5Dwrite(dset_info[j].dset_id, dset_info[j].mem_type_id, dset_info[j].mem_space_id, dset_info[j].dset_space_id, xfer_plist, dset_info[j].u.wbuf);
            assert(ret != FAIL);
sync();
            gettimeofday (&tv2, NULL);
            timersub(&tv2, &tv1, &tv_sub);
            timeradd(&tv_sub, &tv_add, &tv_add);
        }

        /* Display raw data write time */
        if(mpi_rank == (mpi_size-1)) {
            printf("%s:%d p%d> H5Dwrite DONE. Elapsed time: ", __FUNCTION__, __LINE__, mpi_rank);
            put_timeval(&tv_add);
        fflush(stdout);
        }
    }
    else if (multi_mode == MULTI_DSET) { 
        gettimeofday (&tv1, NULL);
        ret = H5Dwrite_multi(fid, xfer_plist, Count, dset_info);
        assert(ret != FAIL);
sync();
        gettimeofday (&tv2, NULL);
        timersub(&tv2, &tv1, &tv_sub);
        time_f = timeval2float(&tv_sub);
#ifndef TEST_NO_MPI
        if (MPI_SUCCESS != MPI_Allreduce (&time_f, &timemax_f, 1, MPI_FLOAT, MPI_MAX, MPI_COMM_WORLD))
            printf("Error: MPI_Allreduce MAX\n");

        if (MPI_SUCCESS != MPI_Allreduce (&time_f, &timemin_f, 1, MPI_FLOAT, MPI_MIN, MPI_COMM_WORLD))
            printf("Error: MPI_Allreduce MIN\n");
#endif

        /* Display raw data write time */
        if(mpi_rank == (mpi_size-1))
            printf("%s:%d p%d> H5Dwrite_multi DONE Elapsed time: %fsec (MIN) - %fsec (MAX)\n", __FUNCTION__, __LINE__, mpi_rank, timemin_f, timemax_f);
        fflush(stdout);
    }

    MESG("H5Dwrite succeed");

    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    /* release all temporary handles. */
    for (i=0;i< Count;i++)
    {
        if(dset_info[i].mem_space_id > 0) {
            ret = H5Sclose(dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(dset_info[i].dset_space_id > 0) {
            ret = H5Sclose(dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(dset_info[i].dset_id > 0) {
            ret = H5Dclose(dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        MESG("H5Dclose succeed");
    }

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    gettimeofday (&tv1, NULL);
    H5Fclose(fid);
    gettimeofday (&tv2, NULL);
    timersub(&tv2, &tv1, &tv_sub);
    time_f = timeval2float(&tv_sub);
#ifndef TEST_NO_MPI
    if (MPI_SUCCESS != MPI_Allreduce (&time_f, &timemax_f, 1, MPI_FLOAT, MPI_MAX, MPI_COMM_WORLD))
        printf("Error: MPI_Allreduce MAX\n");

    if (MPI_SUCCESS != MPI_Allreduce (&time_f, &timemin_f, 1, MPI_FLOAT, MPI_MIN, MPI_COMM_WORLD))
        printf("Error: MPI_Allreduce MIN\n");
#endif

#if 1 // JK_DBG
    if(mpi_rank == (mpi_size-1)) {
        printf("%s:%d p%d> H5Fclose DONE Elapsed time: %f (MIN) - %f (MAX)\n", __FUNCTION__, __LINE__, mpi_rank, timemin_f, timemax_f);
    }
    fflush(stdout);
#endif

    free(data_array);
    if(dset_info)  
        free(dset_info);
}


/*-------------------------------------------------------
 *  Scailable & performance test for Read I/O.
 *  All processes read its own portion from all dsets.
 *  First generate multiple dsets (CONTIG or CHUNKED) and read them either with
 *  H5Dread or H5Dread_multi.
 */
void phdf5Read_mdset_many(char *filename, size_t ndsets, hsize_t dim0, hsize_t chunks, phdf5_mode_t pmode, multi_mode_t multi_mode, mpio_collective_mode_t mpio_opt, sel_mode_t sel_mode )
{
    int i,j;
    int Count;
    hid_t fid;			/* HDF5 file IDs */
    hid_t sid;   		/* Dataspace ID */
    hsize_t dims[RANK_PERF] = {dim0};	/* dataspace dim sizes */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    H5D_rw_multi_t *w_dset_info=NULL;
    H5D_rw_multi_t *r_dset_info=NULL;
    hid_t crp_plist1;
    hid_t f_sid;	/* File dataspace ID */
    hid_t m_sid=-1;	/* memory dataspace ID */
    hid_t did;	/* Dataset ID */
    hsize_t chunk_dims[RANK_PERF];

    int *data_array_w=NULL;	/* data buffer for write */
    int *data_array_r=NULL;	/* data buffer for read */

    hsize_t start[RANK_PERF];			/* for hyperslab setting */
    hsize_t count[RANK_PERF];  
    hsize_t stride[RANK_PERF];	/* for hyperslab setting */
    hsize_t block[RANK_PERF];	/* for hyperslab setting */

    char        Dname[32]=""; // dset name

    struct timeval tv1, tv2, tv_sub, tv_add, tv_max, tv_min;
    float time_f=0, timemax_f=0, timemin_f=0;

    herr_t ret;         	/* Generic return value */

#ifndef TEST_NO_MPI
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
#endif

    timerclear(&tv_sub);
    timerclear(&tv_add);

    if (verbose)
	printf("Collective read test on file %s\n", filename);

     if(verbose && mpi_rank==0) {
         printf("INFO - P%d > fname: %s, #dset: %zu, dim-size:%llu\n",
                mpi_rank, filename,  ndsets, (unsigned long long)dim0);
         if(multi_mode==MULTI_DSET)
             printf("> multi mode: H5Dread_multi Test\n");
         else if(multi_mode==SINGLE_DSET)
             printf("> multi mode: H5Dread Test\n");
#ifdef TEST_NO_MPI
             printf("> NO MPI!\n");
#endif
         fflush(stdout);
     }
    

    if(NULL == (w_dset_info = (H5D_rw_multi_t*) calloc( ndsets , sizeof(H5D_rw_multi_t)))) {
        printf("Error: failed malloc");
        assert(0);
    }
    if(NULL == (r_dset_info = (H5D_rw_multi_t*) calloc( ndsets , sizeof(H5D_rw_multi_t)))) {
        printf("Error: failed malloc");
        assert(0);
    }

    data_array_w = (int *)calloc(dims[0],sizeof(int));
    data_array_r = (int *)calloc(dims[0],sizeof(int));

    // Init data buffers
    data_array1d_init(dims[0], &data_array_w[0],INIT_VAL);
    data_array1d_init(dims[0], &data_array_r[0],INIT_VAL);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl != FAIL);
    MESG("H5Pcreate access succeed");
#ifndef TEST_NO_MPI
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    assert(ret != FAIL);
    MESG("H5Pset_fapl_mpio succeed");
#endif

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    assert(fid != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    assert(ret != FAIL);

    /*==================================================
     * set up the collective transfer properties list 
     */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    assert(xfer_plist != FAIL);
    MESG("H5Pcreate xfer succeed");

#ifndef TEST_NO_MPI
    if (pmode == PHDF5_PARALLEL)
    {
        ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        assert(ret != FAIL);

        /* In parallel mode, do collective IO (MPI_File_write_at_all) */
        if (mpio_opt == MPIO_COLLECTIVE_IO) {
            /* Note: this is default, so don't need to set explicitly */
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_COLLECTIVE_IO);
            assert(ret >= 0);

            /* Note: this is default, so don't need to set explicitly 
             * Just leave this here for a reference if add more options later */
            /*
            ret = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
            assert(ret >= 0);
            */
        }
        /* In parallel mode, do independent IO (MPI_File_write_at) */
        else if (mpio_opt == MPIO_INDIVIDUAL_IO) {
            ret=H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
            assert(ret >= 0);
        }
    }
#endif


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality dsets */
    sid = H5Screate_simple (RANK_PERF, dims, NULL);
    assert (sid != FAIL);
    MESG("H5Screate_simple succeed");

    if (chunks > 0) // CHUNKED
    {
        if (chunks > dims[0])
            chunks = dims[0];
        else
        {
            /* ==========================================
             * set up for chunked Dset1 */
            crp_plist1 = H5Pcreate(H5P_DATASET_CREATE);
            assert (crp_plist1 != FAIL);
            chunk_dims[0] = chunks; 
            ret = H5Pset_chunk(crp_plist1, 1, chunk_dims);
            assert (ret != FAIL);
        }

        if(verbose && mpi_rank==0) {
            printf("INFO - %d: chunk-size: %llu\n",
                mpi_rank, (unsigned long long)chunk_dims[0]);
            if(multi_mode==MULTI_DSET)
                 printf("> multi mode: H5Dread_multi Test\n");
            else if(multi_mode==SINGLE_DSET)
                 printf("> multi mode: H5Dread Test\n");
#ifdef TEST_NO_MPI
                printf("> NO MPI!\n");
#endif
            fflush(stdout);
        }

    }
    else // CONTIG
        crp_plist1= H5P_DEFAULT;



   for (j=0; j<ndsets; j++)
   {
    /* Dset name */
    sprintf(Dname, "%05d_dset", j+1);

    /* ==========================================
     * set up for chunked Dset3 */
    /* create a dataset collectively */
    did = H5Dcreate2(fid, Dname, H5T_NATIVE_INT, sid, H5P_DEFAULT, crp_plist1, H5P_DEFAULT);
    assert(did != FAIL);
    MESG("H5Dcreate2 succeed");

    /*
     * Set up dimensions of the slab this process accesses.
     */
#if 0 // JK_DBG
	printf("== Write DSET  =============================\n");
#endif

    if (sel_mode == SEL_HYPER_1BLOCK) {
        /* each process takes a block */
        slab_set_1d(dims, start, stride, count, block, BYROW);
#if 0 // JK_DBG
        printf("%s P%d > start[]=%lu, count[]=%lu, stride[]=%lu, total datapoints=%lu\n", __FUNCTION__, mpi_rank, 
    	    (unsigned long)start[0], 
            (unsigned long)count[0], 
            (unsigned long)stride[0],
            (unsigned long)count[0]);
#endif
    }
    else {
        /* each process takes a block */
        slab_set_1d(dims, start, stride, count, block, BYROW_M); // for 1D
#if 0 // JK_DBG
        printf("%s P%d > start[]=%lu, stride[]=%lu, count[]=%lu, block[]=%lu,\n", __FUNCTION__, mpi_rank, 
            (unsigned long)start[0],
            (unsigned long)stride[0],
            (unsigned long)count[0], 
            (unsigned long)block[0]);
#endif
    }

    if (count[0] <= 0)
        assert(0 && "-n Number should be bigger than process#. Multiple to process# is ideal.");

    /* create a file dataspace independently */
    f_sid = H5Dget_space (did);
    assert(f_sid != FAIL);
    MESG("H5Dget_space succeed");

    if (sel_mode == SEL_HYPER_1BLOCK)
    {
        /* create a memory dataspace independently */
        m_sid = H5Screate_simple (RANK_PERF, count, NULL);

        ret=H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, stride, count, NULL);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }
    else if (sel_mode == SEL_HYPER_BLOCKS)
    {
        m_sid = H5S_ALL;

        ret=H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, stride, count, block);
        assert(ret != FAIL);
        MESG("H5Sset_hyperslab succeed");
    }

    /* fill the local slab with some trivial data */
    if (sel_mode == SEL_HYPER_1BLOCK) {
        dataset_fill_1d(start, count, stride, &data_array_w[0],1);
    }
    else {
        dataset_fill2_1d(start, stride, count, block, &data_array_w[0],0);
        //dataset_fill3_2d(start, stride, count, block, &data_array_w[0][0],0,BYROW_M);
    }

    MESG("data_array_w initialized");
    if (verbose){
	    MESG("data_array_w created");
    	dataset_print_1d(start, count, stride, &data_array_w[0]);
    }
#if 0 // JK_DBG
	printf("--- dset3 Write Data ---\n");
    //dset_select_display(start, count, stride, &data_array_w[0][0],0);
    dataset_print_1d(start, count, stride, &data_array_w[0]);
#endif


    /*
     * Set up dset info structure 
     * HDF5 lib will handle by order of [w/r]_dset_info[X] X
     */
#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC //------------------------I
    if (mpi_rank == 0) {
#if 1 // USE_DSET3
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        w_dset_info[j].dset_id = did;
        w_dset_info[j].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[j].mem_space_id = m_sid;
        w_dset_info[j].dset_space_id = f_sid;
        w_dset_info[j].u.wbuf = &data_array_w[0];

        r_dset_info[j].dset_id = did;
        r_dset_info[j].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[j].mem_space_id = m_sid;
        r_dset_info[j].dset_space_id = f_sid;
        r_dset_info[j].u.rbuf = &data_array_r[0];
#else
        /* free before rewrite DSET3 */
        if(m_sid)  { ret = H5Sclose(m_sid); assert(ret != FAIL); }
        if(f_sid) { ret = H5Sclose(f_sid); assert(ret != FAIL); }
        if(did) { ret = H5Dclose(did); assert(ret != FAIL); }
#endif
    }

    if (mpi_rank == 1) {
#if 1 // USE_DSET3
        // init H5D_rw_multi_t for write DSET3 (CONTIG)
        w_dset_info[j].dset_id = did;
        w_dset_info[j].mem_type_id = H5T_NATIVE_INT;
        w_dset_info[j].mem_space_id = m_sid;
        w_dset_info[j].dset_space_id = f_sid;
        w_dset_info[j].u.wbuf = &data_array_w[0];

        r_dset_info[j].dset_id = did;
        r_dset_info[j].mem_type_id = H5T_NATIVE_INT;
        r_dset_info[j].mem_space_id = m_sid;
        r_dset_info[j].dset_space_id = f_sid;
        r_dset_info[j].u.rbuf = &data_array_r[0];
#else
        /* free before rewrite DSET3 */
        if(m_sid)  { ret = H5Sclose(m_sid); assert(ret != FAIL); }
        if(f_sid) { ret = H5Sclose(f_sid); assert(ret != FAIL); }
        if(did) { ret = H5Dclose(did); assert(ret != FAIL); }
#endif
    }

#else  // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------------------
    // init H5D_rw_multi_t for write DSET3 (CONTIG)
    w_dset_info[j].dset_id = did;
    w_dset_info[j].mem_type_id = H5T_NATIVE_INT;
    w_dset_info[j].mem_space_id = m_sid;
    w_dset_info[j].dset_space_id = f_sid;
    w_dset_info[j].u.wbuf = &data_array_w[0];

    r_dset_info[j].dset_id = did;
    r_dset_info[j].mem_type_id = H5T_NATIVE_INT;
    r_dset_info[j].mem_space_id = m_sid;
    r_dset_info[j].dset_space_id = f_sid;
    r_dset_info[j].u.rbuf = &data_array_r[0];
#endif // TEST_MDSET_NO_LAST_DSET_2ND_PROC ------------------------O

  } // end of for loop #dsets

  // can close chunk prop
  ret = H5Pclose(crp_plist1);
  assert (ret != FAIL);

#ifdef TEST_MDSET_NO_LAST_DSET_2ND_PROC
  if (mpi_rank == 0)      Count = ndsets;
  if (mpi_rank == 1)      Count = ndsets;
#else
  Count = ndsets;
#endif

#if 0 // JK_DBG
    for (i=0; i< Count ; i++)
        printf("%s:%d P%d > w_dset_info[%d].u.wbuf addr: %x\n", __FUNCTION__,__LINE__, mpi_rank, i, w_dset_info[i].u.wbuf);
#endif

    if(multi_mode == SINGLE_DSET) {
        for(j=0; j< Count; j++) {
            ret = H5Dwrite(w_dset_info[j].dset_id, w_dset_info[j].mem_type_id, w_dset_info[j].mem_space_id, w_dset_info[j].dset_space_id, xfer_plist, w_dset_info[j].u.wbuf);
            assert(ret != FAIL);

            gettimeofday (&tv1, NULL); 
            /* Read Now */
            ret = H5Dread(r_dset_info[j].dset_id, r_dset_info[j].mem_type_id, r_dset_info[j].mem_space_id, r_dset_info[j].dset_space_id, xfer_plist, r_dset_info[j].u.rbuf);
            assert(ret != FAIL);
sync();
            gettimeofday (&tv2, NULL);
            timersub(&tv2, &tv1, &tv_sub);
            timeradd(&tv_sub, &tv_add, &tv_add);
        }

        /* Display raw data read time */
        if(mpi_rank == (mpi_size-1)) {
            printf("%s:%d p%d> H5Dread DONE. Elapsed time: ", __FUNCTION__, __LINE__, mpi_rank);
            put_timeval(&tv_add);
        fflush(stdout);
        }
    }
    else if (multi_mode == MULTI_DSET) { 
        ret = H5Dwrite_multi(fid, xfer_plist, Count, w_dset_info);
        assert(ret != FAIL);

        gettimeofday (&tv1, NULL);
        /* Read Now */
        ret = H5Dread_multi(fid, xfer_plist, Count, r_dset_info);
        assert(ret != FAIL);
sync();
        gettimeofday (&tv2, NULL);
        timersub(&tv2, &tv1, &tv_sub);
        time_f = timeval2float(&tv_sub);
#ifndef TEST_NO_MPI
        if (MPI_SUCCESS != MPI_Allreduce (&time_f, &timemax_f, 1, MPI_FLOAT, MPI_MAX, MPI_COMM_WORLD))
            printf("Error: MPI_Allreduce MAX\n");
        if (MPI_SUCCESS != MPI_Allreduce (&time_f, &timemin_f, 1, MPI_FLOAT, MPI_MIN, MPI_COMM_WORLD))
            printf("Error: MPI_Allreduce MIN\n");
#endif

        /* Display raw data read time */
        if(mpi_rank == (mpi_size-1)) {
            printf("JKDBG %s:%d p%d> H5Dread_multi DONE Elapsed time: %fsec (MIN) - %fsec (MAX)\n", __FUNCTION__, __LINE__, mpi_rank, timemin_f, timemax_f);
        }
        fflush(stdout);
    }

    // Verify Read buffers with Write buffers
    for(i=0; i< Count; i++) {
        diff_datasets(dim0,1,(DTYPE_INT *)w_dset_info[i].u.wbuf, (DTYPE_INT *)r_dset_info[i].u.rbuf, i);
    }


    MESG("H5Dread succeed");

    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    /* release all temporary handles. */
    for (i=0;i< Count;i++)
    {
        if(w_dset_info[i].mem_space_id > 0) {
            ret = H5Sclose(w_dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].mem_space_id > 0) {
            ret = H5Sclose(r_dset_info[i].mem_space_id);
            assert(ret != FAIL);
        }
        if(w_dset_info[i].dset_space_id > 0) {
            ret = H5Sclose(w_dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].dset_space_id > 0) {
            ret = H5Sclose(r_dset_info[i].dset_space_id);
            assert(ret != FAIL);
        }
        if(w_dset_info[i].dset_id > 0) {
            ret = H5Dclose(w_dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        if(r_dset_info[i].dset_id > 0) {
            ret = H5Dclose(r_dset_info[i].dset_id);
            assert(ret != FAIL);
        }
        MESG("H5Dclose succeed");
    }

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    gettimeofday (&tv1, NULL);
    H5Fclose(fid);
    gettimeofday (&tv2, NULL);
    timersub(&tv2, &tv1, &tv_sub);
    time_f = timeval2float(&tv_sub);
#ifndef TEST_NO_MPI
    if (MPI_SUCCESS != MPI_Allreduce (&time_f, &timemax_f, 1, MPI_FLOAT, MPI_MAX, MPI_COMM_WORLD))
        printf("Error: MPI_Allreduce MAX\n");
    if (MPI_SUCCESS != MPI_Allreduce (&time_f, &timemin_f, 1, MPI_FLOAT, MPI_MIN, MPI_COMM_WORLD))
        printf("Error: MPI_Allreduce MIN\n");
#endif

#if 1 // JK_DBG
    if(mpi_rank == (mpi_size-1))
        printf("JKDBG %s:%d p%d> H5Fclose DONE Elapsed time: %f (MIN) - %f (MAX)\n", __FUNCTION__, __LINE__, mpi_rank, timemin_f, timemax_f);
    fflush(stdout);
#endif

    /* free buffers */
    if(data_array_w)
        free(data_array_w);
    if(data_array_r)
        free(data_array_r);

    if(w_dset_info)  
        free(w_dset_info);
    if(r_dset_info)  
        free(r_dset_info);
}


/*
 * Show command usage
 */
void
usage(void)
{
    if(TEST_TYPE) {
        printf("Feature test mode usage:\n");
    }
    else {
        printf("Performance test usage:\n");
    }
    printf(" COMMON OPTIONS:\n");
    printf("\t-h\t Show help\n");
    printf("\t-c\t MPI Collective-IO\n");
    printf("\t-i\t MPI Independent-IO\n");
    printf("\t-s\t Non-MPI Independent\n");
    printf("\t-w\t Write I/O\n");
    printf("\t-r\t Read I/O\n");
    if(!TEST_TYPE) {
        printf("\t-f\t output HDF5 filename.\n");
        printf("\t-d N\t make N dsets\n");
        printf("\t-n N\t make N dim in a dset\n");
        printf("\t-k N\t make N chunk-size CHUNKED dsets\n");
        printf("\t-u\t Test H5Dwrite() instead. Single-dset path test.\n");
        printf("\t-v\t verbose on\n");
    }
    printf("\n");
    
    if(TEST_TYPE) {
        printf(" EXAMPLES for feature test:\n");
        printf("   H5Dwrite_multi for Parallel Collective-IO:\n");
        printf("    ./a.out -c -w\n");
        printf("   H5Dwrite_multi for Parallel Independent-IO:\n");
        printf("    ./a.out -i -w\n");
        printf("   H5Dwrite_multi for Serial IO:\n");
        printf("    ./a.out -s -w\n");
        printf("\n");
        printf("   H5Dread_multi for Parallel Collective-IO:\n");
        printf("    ./a.out -c -r\n");
        printf("   H5Dread_multi for Parallel Independent-IO:\n");
        printf("    ./a.out -i -r\n");
        printf("   H5Dread_multi for Serial IO:\n");
        printf("    ./a.out -s -r\n");
        printf("\n");
    }
    else {
        printf(" EXAMPLES for Performance test:\n");
        printf("   H5Dwirte_multi for 5 CONTIG dsets with 10 dim:\n");
        printf("    ./a.out -f test.h5 -c -w -d 5 -n 10 \n");
        printf("   H5Dwirte for 5 CONTIG dsets with 10 dim: \n");
        printf("    ./a.out -f test.h5 -c -w -d 5 -n 10 -u \n");
        printf("   H5Dwirte_multi for 5 CHUNKED dsets with 5 dim chunk-size :\n");
        printf("    ./a.out -f test.h5 -c -w -d 5 -n 10 -k 5 \n");
        printf("   H5Dwirte for 5 CHUNKED dsets with 5 dim chunk-size :\n");
        printf("    ./a.out -f test.h5 -c -w -d 5 -n 10 -k 5 -u \n");
        printf("   Note: for read test, just replace -w with -r. \n");
        printf("\n");
    }
}


/*
 * compose the test filename with the prefix supplied.
 * return code: 0 if no error
 *              1 otherwise.
 */
int
mkfilenames(char *prefix)
{
    int i, n;
    size_t strsize;

    /* filename will be prefix/ParaEgN.h5 where N is 0 to 9. */
    /* So, string must be big enough to hold the prefix, / and 10 more chars */
    /* and the terminating null. */
    strsize = strlen(prefix) + 12;
    if (strsize > PATH_MAX){
	printf("File prefix too long;  Use a short path name.\n");
	return(1);
    }
    n = sizeof(testfiles)/sizeof(testfiles[0]);
    if (n > TEST_MAX){
	printf("Warning: Too many entries in testfiles. "
	    "Need to adjust the code to accommodate the large size.\n");
    } 

    for (i=0; i<n; i++){
	sprintf(testfiles[i], "%s/ParaEg%d.h5", prefix, i);
    }
    return(0);
}


/*
 * parse the command line options
 */
int
parse_options(int argc, char **argv)
{
    int i, n;

    /* initialize testfiles to nulls */
    n = sizeof(testfiles)/sizeof(testfiles[0]);
    for (i=0; i<n; i++){
	testfiles[i][0] = '\0';
    }

    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
		case 'f':   ++argv;
			    if (--argc < 1){
				usage();
				nerrors++;
				return(1);
			    }
                sprintf(fname_g,"%s",*argv);
			    break;
        case 'n': ++argv; 
                if (--argc < 1){
				usage();
				nerrors++;
				return(1);
			    }
                dim0_g = atoi(*argv);
                break;
        case 'd': ++argv; 
                if (--argc < 1){
				usage();
				nerrors++;
				return(1);
			    }
                ndset_g = atoi(*argv);
                break;
        case 'k': ++argv; 
                if (--argc < 1){
				usage();
				nerrors++;
				return(1);
			    }
                chunks_g = atoi(*argv);
                break;
		case 'x':   docleanup = 1;	/* no cleanup */
			    break;
		case 'r':   doread = 1;
			    break;
		case 'w':   dowrite = 1;
			    break;
		case 'c':   doCOLL = 1;
			    break;
		case 'i':   doIND = 1;
			    break;
		case 's':   doSERIAL = 1;
			    break;
		case 'u':   multi_mode_g = SINGLE_DSET;
			    break;
		case 'v':   verbose = 1;
			    break;
        case 'h': usage();
                return(1);
                break;
		default:    usage();
			    nerrors++;
			    return(1);
	    }
	}
    }

    /* check the file prefix */
    if (testfiles[0][0] == '\0'){
	/* try get it from environment variable HDF5_PARAPREFIX */
	char *env;
	char *env_default = ".";	/* default to current directory */
	if ((env=getenv(PARAPREFIX))==NULL){
	    env = env_default;
	}
    if(fname_g[0]==0)
        strcpy(fname_g, "mdset_perf.h5");
    	mkfilenames(env);
    }
    return(0);
}


/*
 * cleanup test files created
 */
void
cleanup(void)
{
    int i, n;

    n = sizeof(testfiles)/sizeof(testfiles[0]);
#ifndef TEST_NO_MPI
    for (i=0; i<n; i++)
	    MPI_File_delete(testfiles[i], MPI_INFO_NULL);
#endif
}


/* Main Program */
int
main(int argc, char **argv)
{
    int i, n;
#ifndef TEST_NO_MPI
    int mpi_namelen;
    char mpi_name[MPI_MAX_PROCESSOR_NAME];

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    MPI_Get_processor_name(mpi_name,&mpi_namelen);
#else
    mpi_size=1; // serial mode , just single process
    mpi_rank=0;
#endif


    if (parse_options(argc, argv) != 0)
	    goto finish;

    if (dim0_g == 0) {
    /* Make sure datasets can be divided into equal chunks by the processes */
    if ((SPACE1_DIM1 % mpi_size) || (SPACE1_DIM2 % mpi_size)){
	    printf("DIM1(%d) and DIM2(%d) must be multiples of processes (%d)\n",
	            SPACE1_DIM1, SPACE1_DIM2, mpi_size);
    	nerrors++;
    	goto finish;
    }
    }

    /* show test file names */
    if (mpi_rank == 0){
	    n = sizeof(testfiles)/sizeof(testfiles[0]);
    	printf("Parallel test files are:\n");
	    for (i=0; i<n; i++){
    	    printf("   %s\n", testfiles[i]);
    	}
    }
#if 0 // JK_DBG
    printf("%s P%d> pid=%d -- START -- \n", __FUNCTION__, mpi_rank, getpid());
    printf("%s P%d> multi_mode_g: %d\n", __FUNCTION__, mpi_rank, multi_mode_g);
    fflush(stdout);
#endif

    if (doread) {
        if(doCOLL) {
	        MPI_BANNER("testing PHDF5 dataset CollectiveIO read Eg0");

            if(TEST_TYPE) {
            /*
             * This section is for multi-dset feature tests
             */
            phdf5Read_mdset_All(testfiles[0], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            phdf5Read_mdset_All(testfiles[1], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            phdf5Read_mdset_All(testfiles[2], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_NONE, MULTI_DSET); 
            phdf5Read_mdset_All(testfiles[3], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_POINTS, MULTI_DSET); 
            phdf5Read_mdset_All(testfiles[4], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_1POINT, MULTI_DSET); 

            //phdf5Read_mdset_Chunk(testfiles[5], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Read_mdset_Chunk(testfiles[6], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Read_mdset_Chunk(testfiles[7], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_NONE, MULTI_DSET);
            //phdf5Read_mdset_Chunk(testfiles[8], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_POINTS, MULTI_DSET);
            //phdf5Read_mdset_Chunk(testfiles[9], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_1POINT, MULTI_DSET);
            
            //phdf5Read_mdset_Contig(testfiles[10], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Read_mdset_Contig(testfiles[11], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Read_mdset_Contig(testfiles[12], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_NONE, MULTI_DSET); 
            //phdf5Read_mdset_Contig(testfiles[13], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_POINTS, MULTI_DSET); 
            //phdf5Read_mdset_Contig(testfiles[14], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_1POINT, MULTI_DSET); 
            }
            else {
            /* 
             * This is for performance test 
             * Generating many dsets with many processes 
             */
            phdf5Read_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_PARALLEL, multi_mode_g, MPIO_COLLECTIVE_IO, SEL_HYPER_1BLOCK);
            //phdf5Read_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_PARALLEL, multi_mode_g, MPIO_COLLECTIVE_IO, SEL_HYPER_BLOCKS);
            }

        }
        if(doIND) {
	        MPI_BANNER("testing PHDF5 dataset IndependentIO read Eg0");

            if(TEST_TYPE) {
            /*
             * This section is for multi-dset feature tests
             */
            phdf5Read_mdset_All(testfiles[0], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            phdf5Read_mdset_All(testfiles[1], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            phdf5Read_mdset_All(testfiles[2], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_NONE, MULTI_DSET);
            phdf5Read_mdset_All(testfiles[3], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_POINTS, MULTI_DSET);
            //phdf5Read_mdset_All(testfiles[4], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_1POINT, MULTI_DSET);
            
            //phdf5Read_mdset_Chunk(testfiles[5], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Read_mdset_Chunk(testfiles[6], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Read_mdset_Chunk(testfiles[7], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_NONE, MULTI_DSET);
            //phdf5Read_mdset_Chunk(testfiles[8], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_POINTS, MULTI_DSET);
            //phdf5Read_mdset_Chunk(testfiles[9], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_1POINT, MULTI_DSET);

            //phdf5Read_mdset_Contig(testfiles[10], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Read_mdset_Contig(testfiles[11], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Read_mdset_Contig(testfiles[12], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_NONE, MULTI_DSET);
            //phdf5Read_mdset_Contig(testfiles[13], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_POINTS, MULTI_DSET);
            //phdf5Read_mdset_Contig(testfiles[14], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_1POINT, MULTI_DSET);
            }
            else {
            /* 
             * This is for performance test 
             * Generating many dsets with many processes 
             */
            phdf5Read_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_PARALLEL, multi_mode_g, MPIO_INDIVIDUAL_IO, SEL_HYPER_1BLOCK);
            //phdf5Read_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_PARALLEL, multi_mode_g, MPIO_INDIVIDUAL_IO, SEL_HYPER_BLOCKS);
            }

        }
        if(doSERIAL) {
	        MPI_BANNER("testing PHDF5 dataset Serial read Eg0");

            if(TEST_TYPE) {
            /*
             * This section is for multi-dset feature tests
             */
            phdf5Read_mdset_All(testfiles[0], PHDF5_SERIAL, 0, SEL_HYPER_1BLOCK, SINGLE_DSET /*MULTI_DSET*/);
            phdf5Read_mdset_All(testfiles[1], PHDF5_SERIAL, 0, SEL_HYPER_BLOCKS, SINGLE_DSET /*MULTI_DSET*/);
            phdf5Read_mdset_All(testfiles[2], PHDF5_SERIAL, 0, SEL_NONE, SINGLE_DSET /*MULTI_DSET*/);
            phdf5Read_mdset_All(testfiles[3], PHDF5_SERIAL, 0, SEL_POINTS, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_All(testfiles[4], PHDF5_SERIAL, 0, SEL_1POINT, SINGLE_DSET /*MULTI_DSET*/);
            
            //phdf5Read_mdset_Chunk(testfiles[5], PHDF5_SERIAL, 0, SEL_HYPER_1BLOCK, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_Chunk(testfiles[6], PHDF5_SERIAL, 0, SEL_HYPER_BLOCKS, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_Chunk(testfiles[7], PHDF5_SERIAL, 0, SEL_NONE, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_Chunk(testfiles[8], PHDF5_SERIAL, 0, SEL_POINTS, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_Chunk(testfiles[9], PHDF5_SERIAL, 0, SEL_1POINT, SINGLE_DSET /*MULTI_DSET*/);
            
            //phdf5Read_mdset_Contig(testfiles[10], PHDF5_SERIAL, 0, SEL_HYPER_1BLOCK, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_Contig(testfiles[11], PHDF5_SERIAL, 0, SEL_HYPER_BLOCKS, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_Contig(testfiles[12], PHDF5_SERIAL, 0, SEL_NONE, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_Contig(testfiles[13], PHDF5_SERIAL, 0, SEL_POINTS, SINGLE_DSET /*MULTI_DSET*/);
            //phdf5Read_mdset_Contig(testfiles[14], PHDF5_SERIAL, 0, SEL_1POINT, SINGLE_DSET /*MULTI_DSET*/);
            }
            else {
            /* 
             * This is for performance test 
             * Generating many dsets with many processes 
             */
            phdf5Read_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_SERIAL, multi_mode_g, 0, SEL_HYPER_1BLOCK);
            //phdf5Read_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_SERIAL, multi_mode_g, 0, SEL_HYPER_BLOCKS);
            }
        }
    }

    if (dowrite){
        if(doCOLL) {
	        MPI_BANNER("testing PHDF5 dataset CollectiveIO write Eg0");

            if(TEST_TYPE) {
            /*
             * This section is for multi-dset feature tests
             */
            phdf5Write_mdset_All(testfiles[0], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[1], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[2], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_NONE, MULTI_DSET); 
            phdf5Write_mdset_All(testfiles[3], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_POINTS, MULTI_DSET); 
            phdf5Write_mdset_All(testfiles[4], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_1POINT, MULTI_DSET); 
            
            //phdf5Write_mdset_Chunk(testfiles[5], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[6], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[7], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_NONE, MULTI_DSET); 
            //phdf5Write_mdset_Chunk(testfiles[8], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_POINTS, MULTI_DSET); 
            //phdf5Write_mdset_Chunk(testfiles[9], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_1POINT, MULTI_DSET);

            //phdf5Write_mdset_Contig(testfiles[10], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[11], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[12], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_NONE, MULTI_DSET); 
            //phdf5Write_mdset_Contig(testfiles[13], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_POINTS, MULTI_DSET); 
            //phdf5Write_mdset_Contig(testfiles[14], PHDF5_PARALLEL, MPIO_COLLECTIVE_IO, SEL_1POINT, MULTI_DSET); 
            }
            else {
            /* 
             * This is for performance test 
             * Generating many dsets with many processes 
             */
            phdf5Write_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_PARALLEL, multi_mode_g, MPIO_COLLECTIVE_IO, SEL_HYPER_1BLOCK);
            //phdf5Write_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_PARALLEL, multi_mode_g, MPIO_COLLECTIVE_IO, SEL_HYPER_BLOCKS);
            }
        }

        if(doIND) {
	        MPI_BANNER("testing PHDF5 dataset IndependentIO write Eg0");

            if(TEST_TYPE) {
            /*
             * This section is for multi-dset feature tests
             */
            phdf5Write_mdset_All(testfiles[0], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[1], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[2], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_NONE, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[3], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_POINTS, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[4], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_1POINT, MULTI_DSET);
            
            //phdf5Write_mdset_Chunk(testfiles[5], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[6], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[7], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_NONE, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[8], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_POINTS, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[9], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_1POINT, MULTI_DSET);

            //phdf5Write_mdset_Contig(testfiles[10], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[11], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[12], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_NONE, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[13], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_POINTS, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[14], PHDF5_PARALLEL, MPIO_INDIVIDUAL_IO, SEL_1POINT, MULTI_DSET);
            }
            else {
            /* 
             * This is for performance test 
             * Generating many dsets with many processes 
             */
            phdf5Write_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_PARALLEL, multi_mode_g, MPIO_INDIVIDUAL_IO, SEL_HYPER_1BLOCK);
            //phdf5Write_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_PARALLEL, multi_mode_g, MPIO_INDIVIDUAL_IO, SEL_HYPER_BLOCKS);
            }
        }

        if(doSERIAL) {
	        MPI_BANNER("testing PHDF5 dataset Serial write Eg0");

            if(TEST_TYPE) {
            /*
             * This section is for multi-dset feature tests
             */
            phdf5Write_mdset_All(testfiles[0], PHDF5_SERIAL, 0, SEL_HYPER_1BLOCK, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[1], PHDF5_SERIAL, 0, SEL_HYPER_BLOCKS, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[2], PHDF5_SERIAL, 0, SEL_NONE, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[3], PHDF5_SERIAL, 0, SEL_POINTS, MULTI_DSET);
            phdf5Write_mdset_All(testfiles[4], PHDF5_SERIAL, 0, SEL_1POINT, MULTI_DSET);
            
            //phdf5Write_mdset_Chunk(testfiles[5], PHDF5_SERIAL, 0, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[6], PHDF5_SERIAL, 0, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[7], PHDF5_SERIAL, 0, SEL_NONE, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[8], PHDF5_SERIAL, 0, SEL_POINTS, MULTI_DSET);
            //phdf5Write_mdset_Chunk(testfiles[9], PHDF5_SERIAL, 0, SEL_1POINT, MULTI_DSET);
            
            //phdf5Write_mdset_Contig(testfiles[10], PHDF5_SERIAL, 0, SEL_HYPER_1BLOCK, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[11], PHDF5_SERIAL, 0, SEL_HYPER_BLOCKS, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[12], PHDF5_SERIAL, 0, SEL_NONE, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[13], PHDF5_SERIAL, 0, SEL_POINTS, MULTI_DSET);
            //phdf5Write_mdset_Contig(testfiles[14], PHDF5_SERIAL, 0, SEL_1POINT, MULTI_DSET);
            }
            else {

            /* 
             * This is for performance test 
             * Generating many dsets with many processes 
             */
            phdf5Write_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_SERIAL, multi_mode_g, 0, SEL_HYPER_1BLOCK);
            //phdf5Write_mdset_many(fname_g, ndset_g, dim0_g, chunks_g, PHDF5_SERIAL, multi_mode_g, 0, SEL_HYPER_BLOCKS);
            }
        }
    }
#if 0 // JK_DBG
    printf("%s P%d> pid=%d -- END -- \n", __FUNCTION__, mpi_rank, getpid());
    fflush(stdout);
#endif

    if (!(dowrite || doread)){
	    usage();
	    nerrors++;
    }

finish:
    if (mpi_rank == 0){		/* only process 0 reports */
	    if (nerrors)
    	    printf("***PHDF5 multi-dset tests detected %d errors***\n", nerrors);
    	else{
    	    printf("==============================================\n");
    	    printf("PHDF5 multi-dset tests finished with no errors\n");
    	    printf("==============================================\n");
    	}
    }

    
    if (docleanup)
	    cleanup();

#ifndef TEST_NO_MPI
    MPI_Finalize();
#endif

    return(nerrors);
}

