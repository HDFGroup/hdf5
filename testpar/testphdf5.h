/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* $Id$ */

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
extern H5E_auto_t old_func;		        /* previous error handler */
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
void extend_writeAll(void);
void dataset_readInd(void);
void dataset_readAll(void);
void extend_readInd(void);
void extend_readAll(void);
void compact_dataset(void);
void big_dataset(void);
void dataset_fillvalue(void);

/* commonly used prototypes */
hid_t create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type, hbool_t use_gpfs);
MPI_Offset h5_mpi_get_file_size(const char *filename, MPI_Comm comm, MPI_Info info);
int dataset_vrfy(hssize_t start[], hsize_t count[], hsize_t stride[],
                 hsize_t block[], DATATYPE *dataset, DATATYPE *original);

#endif /* PHDF5TEST_H */
