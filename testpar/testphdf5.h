/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef PHDF5TEST_H
#define PHDF5TEST_H

#include "h5test.h"

#ifndef TRUE
#define TRUE    1
#endif  /* !TRUE */

#ifndef FALSE
#define FALSE   (!TRUE)
#endif  /* !FALSE */

/* Define some handy debugging shorthands, routines, ... */
/* debugging tools */

#define MESG(x)                                                         \
	if (VERBOSE_MED) printf("%s\n", x);                                 \

#define VRFY(val, mesg) do {                                            \
    if (val) {                                                          \
        if (*mesg != '\0') {                                            \
            MESG(mesg);                                                 \
        }                                                               \
    } else {                                                            \
        printf("Proc %d: ", mpi_rank);                                  \
        printf("*** PHDF5 ERROR ***\n");                                \
        printf("        Assertion (%s) failed at line %4d in %s\n",     \
               mesg, (int)__LINE__, __FILE__);                          \
        ++nerrors;                                                      \
        fflush(stdout);                                                 \
        if (!VERBOSE_MED) {                                                 \
            printf("aborting MPI process\n");                           \
            MPI_Finalize();                                             \
            exit(nerrors);                                              \
        }                                                               \
    }                                                                   \
} while(0)

/*
 * Checking for information purpose.
 * If val is false, print mesg; else nothing.
 * Either case, no error setting.
 */
#define INFO(val, mesg) do {                                            \
    if (val) {                                                          \
        if (*mesg != '\0') {                                            \
            MESG(mesg);                                                 \
        }                                                               \
    } else {                                                            \
        printf("Proc %d: ", mpi_rank);                                  \
        printf("*** PHDF5 REMARK (not an error) ***\n");                \
        printf("        Condition (%s) failed at line %4d in %s\n",     \
               mesg, (int)__LINE__, __FILE__);                          \
        fflush(stdout);                                                 \
    }                                                                   \
} while(0)

#define MPI_BANNER(mesg) do {                                           \
    if (VERBOSE_MED || MAINPROCESS){                                    \
	printf("--------------------------------\n");                   \
	printf("Proc %d: ", mpi_rank);                                  \
	printf("*** %s\n", mesg);                                       \
	printf("--------------------------------\n");                   \
    }                                                                   \
} while(0)

#define MAINPROCESS     (!mpi_rank) /* define process 0 as main process */

#define SYNC(comm) do {                                                 \
    MPI_BANNER("doing a SYNC");                                         \
    MPI_Barrier(comm);                                                  \
    MPI_BANNER("SYNC DONE");                                            \
} while(0)

/* End of Define some handy debugging shorthands, routines, ... */

/* Constants definitions */
#define DIM0		600 	/* Default dataset sizes. */
#define DIM1		1200	/* Values are from a monitor pixel sizes */
#define ROW_FACTOR      8       /* Nominal row factor for dataset size */
#define COL_FACTOR      16      /* Nominal column factor for dataset size */
#define RANK		2
#define DATASETNAME1	"Data1"
#define DATASETNAME2	"Data2"
#define DATASETNAME3	"Data3"
#define DATASETNAME4	"Data4"

/* Hyperslab layout styles */
#define BYROW           1       /* divide into slabs of rows */
#define BYCOL           2       /* divide into blocks of columns */
#define ZROW            3       /* same as BYCOL except process 0 gets 0 rows */
#define ZCOL            4       /* same as BYCOL except process 0 gets 0 columns */
#define MAX_ERR_REPORT  10      /* Maximum number of errors reported */

/* File_Access_type bits */
#define FACC_DEFAULT    0x0     /* default */
#define FACC_MPIO       0x1     /* MPIO */
#define FACC_SPLIT      0x2     /* Split File */
#define FACC_MULTI      0x4     /* Multi File */
#define FACC_MPIPOSIX   0x8     /* MPIPOSIX */

/*Constants for collective chunk definitions */
#define SPACE_DIM1 24 
#define SPACE_DIM2 4 
#define BYROW_CONT 1
#define BYROW_DISCONT 2
#define DSET_COLLECTIVE_CHUNK_NAME "coll_chunk_name"


/*Constants for MPI derived data type generated from span tree */

#define MSPACE1_RANK     1          /* Rank of the first dataset in memory */
#define MSPACE1_DIM      27000      /* Dataset size in memory */
#define FSPACE_RANK      2        /* Dataset rank as it is stored in the file */
#define FSPACE_DIM1      9       /* Dimension sizes of the dataset as it is                                                                  stored in the file */
#define FSPACE_DIM2      3600                                                                                         /* We will read dataset back from the file                                         to the dataset in memory with these                                                                      dataspace parameters. */
#define MSPACE_RANK      2
#define MSPACE_DIM1      9
#define MSPACE_DIM2      3600
#define FHCOUNT0           1         /* Count of the first dimension of the first hyperslab selection*/
#define FHCOUNT1           768       /* Count of the second dimension of the first hyperslab selection*/
#define FHSTRIDE0          4        /* Stride of the first dimension of the first hyperslab selection*/
#define FHSTRIDE1          3        /* Stride of the second dimension of the first hyperslab selection*/
#define FHBLOCK0           3        /* Block of the first dimension of the first hyperslab selection*/
#define FHBLOCK1           2        /* Block of the second dimension of the first hyperslab selection*/
#define FHSTART0           0        /* start of the first dimension of the first hyperslab selection*/
#define FHSTART1           1        /* start of the second dimension of the first hyperslab selection*/

#define SHCOUNT0           1         /* Count of the first dimension of the first hyperslab selection*/
#define SHCOUNT1           1       /* Count of the second dimension of the first hyperslab selection*/
#define SHSTRIDE0          1        /* Stride of the first dimension of the first hyperslab selection*/
#define SHSTRIDE1          1        /* Stride of the second dimension of the first hyperslab selection*/
#define SHBLOCK0           3        /* Block of the first dimension of the first hyperslab selection*/
#define SHBLOCK1           768        /* Block of the second dimension of the first hyperslab selection*/
#define SHSTART0           4        /* start of the first dimension of the first hyperslab selection*/
#define SHSTART1           0        /* start of the second dimension of the first hyperslab selection*/

#define MHCOUNT0           6912         /* Count of the first dimension of the first hyperslab selection*/
#define MHSTRIDE0          1        /* Stride of the first dimension of the first hyperslab selection*/
#define MHBLOCK0           1        /* Block of the first dimension of the first hyperslab selection*/
#define MHSTART0           1        /* start of the first dimension of the first hyperslab selection*/



#define RFFHCOUNT0           3       /* Count of the first dimension of the first hyperslab selection*/
#define RFFHCOUNT1           768       /* Count of the second dimension of the first hyperslab selection*/
#define RFFHSTRIDE0          1        /* Stride of the first dimension of the first hyperslab selection*/
#define RFFHSTRIDE1          1        /* Stride of the second dimension of the first hyperslab selection*/
#define RFFHBLOCK0           1        /* Block of the first dimension of the first hyperslab selection*/
#define RFFHBLOCK1           1        /* Block of the second dimension of the first hyperslab selection*/
#define RFFHSTART0           1        /* start of the first dimension of the first hyperslab selection*/
#define RFFHSTART1           2        /* start of the second dimension of the first hyperslab selection*/


#define RFSHCOUNT0           3       /* Count of the first dimension of the first hyperslab selection*/
#define RFSHCOUNT1           1536    /* Count of the second dimension of the first hyperslab selection*/
#define RFSHSTRIDE0          1        /* Stride of the first dimension of the first hyperslab selection*/
#define RFSHSTRIDE1          1        /* Stride of the second dimension of the first hyperslab selection*/
#define RFSHBLOCK0           1        /* Block of the first dimension of the first hyperslab selection*/
#define RFSHBLOCK1           1        /* Block of the second dimension of the first hyperslab selection*/
#define RFSHSTART0           2        /* start of the first dimension of the first hyperslab selection*/
#define RFSHSTART1           4        /* start of the second dimension of the first hyperslab selection*/


#define RMFHCOUNT0           3       /* Count of the first dimension of the first hyperslab selection*/
#define RMFHCOUNT1           768       /* Count of the second dimension of the first hyperslab selection*/
#define RMFHSTRIDE0          1        /* Stride of the first dimension of the first hyperslab selection*/
#define RMFHSTRIDE1          1        /* Stride of the second dimension of the first hyperslab selection*/
#define RMFHBLOCK0           1        /* Block of the first dimension of the first hyperslab selection*/
#define RMFHBLOCK1           1        /* Block of the second dimension of the first hyperslab selection*/
#define RMFHSTART0           0        /* start of the first dimension of the first hyperslab selection*/
#define RMFHSTART1           0        /* start of the second dimension of the first hyperslab selection*/

#define RMSHCOUNT0           3       /* Count of the first dimension of the first hyperslab selection*/
#define RMSHCOUNT1           1536    /* Count of the second dimension of the first hyperslab selection*/
#define RMSHSTRIDE0          1        /* Stride of the first dimension of the first hyperslab selection*/
#define RMSHSTRIDE1          1        /* Stride of the second dimension of the first hyperslab selection*/
#define RMSHBLOCK0           1        /* Block of the first dimension of the first hyperslab selection*/
#define RMSHBLOCK1           1        /* Block of the second dimension of the first hyperslab selection*/
#define RMSHSTART0           1        /* start of the first dimension of the first hyperslab selection*/
#define RMSHSTART1           2        /* start of the second dimension of the first hyperslab selection*/


#define NPOINTS          4          /* Number of points that will be selected
                                                                and overwritten */

/* Don't erase these lines, they are put here for debugging purposes */
/*
#define MSPACE1_RANK     1
#define MSPACE1_DIM      50
#define MSPACE2_RANK     1
#define MSPACE2_DIM      4
#define FSPACE_RANK      2
#define FSPACE_DIM1      8
#define FSPACE_DIM2      12
#define MSPACE_RANK      2
#define MSPACE_DIM1      8
#define MSPACE_DIM2      9
#define NPOINTS          4


*/ /* end of debugging macro */
/* type definitions */
typedef struct H5Ptest_param_t  /* holds extra test parameters */
{
    char	*name;
    int		count;
} H5Ptest_param_t;

/* Dataset data type.  Int's can be easily octo dumped. */
typedef int DATATYPE;

/* Shared global variables */
extern int dim0, dim1;				/*Dataset dimensions */
extern int chunkdim0, chunkdim1;		/*Chunk dimensions */
extern int nerrors;				/*errors count */
extern H5E_auto_t old_func;			/* previous error handler */
extern void *old_client_data;			/*previous error handler arg.*/
extern int facc_type;				/*Test file access type */

/* Test program prototypes */
void multiple_dset_write(void);
void multiple_group_write(void);
void multiple_group_read(void);
void collective_group_write(void);
void independent_group_read(void);
void test_fapl_mpio_dup(void);
void test_fapl_mpiposix_dup(void);
void test_split_comm_access(void);
void dataset_writeInd(void);
void dataset_writeAll(void);
void extend_writeInd(void);
void extend_writeInd2(void);
void extend_writeAll(void);
void dataset_readInd(void);
void dataset_readAll(void);
void extend_readInd(void);
void extend_readAll(void);
void none_selection_chunk(void);
void test_chunk_alloc(void);
void test_filter_read(void);
void compact_dataset(void);
void big_dataset(void);
void dataset_fillvalue(void);
void coll_chunk1(void);
void coll_chunk2(void);
void coll_chunk3(void);
void coll_chunk4(void);
void coll_irregular_cont_read(void);
void coll_irregular_cont_write(void);
void coll_irregular_simple_chunk_read(void);
void coll_irregular_simple_chunk_write(void);
void coll_irregular_complex_chunk_read(void);
void coll_irregular_complex_chunk_write(void);
void io_mode_confusion(void);
#ifdef H5_HAVE_FILTER_DEFLATE
void compress_readAll(void);
#endif /* H5_HAVE_FILTER_DEFLATE */

/* commonly used prototypes */
hid_t create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type, hbool_t use_gpfs);
int dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[],
                 hsize_t block[], DATATYPE *dataset, DATATYPE *original);

#endif /* PHDF5TEST_H */
