/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Parallel tests for datasets
 */

/*
 * Example of using the parallel HDF5 library to access datasets.
 *
 * This program contains three major parts.  Part 1 tests fixed dimension
 * datasets, for both independent and collective transfer modes.
 * Part 2 tests extendible datasets, for independent transfer mode
 * only.
 * Part 3 tests extendible datasets, for collective transfer mode
 * only.
 */

#include <stdio.h>
#include "hdf5.h"
#include "testpar.h"

/* Include testing framework functionality */
#include "testframe.h"

#include "mpi.h"

/* For this test, we don't want to inherit the RANK definition
 * from testphdf5.h.  We'll define MAX_RANK to accommodate 3D arrays
 * and use that definition rather than RANK.
 */
#ifndef MAX_RANK
#define MAX_RANK 2
#endif

/* As with RANK vs MAX_RANK, we use BIG_X_FACTOR vs ROW_FACTOR
 * and BIG_Y_FACTOR vs COL_FACTOR.   We introduce BIG_Z_FACTOR
 * for the 3rd dimension.
 */

#ifndef BIG_X_FACTOR
#define BIG_X_FACTOR 1048576
#endif
#ifndef BIG_Y_FACTOR
#define BIG_Y_FACTOR 32
#endif
#ifndef BIG_Z_FACTOR
#define BIG_Z_FACTOR 2048
#endif

#define DATASETNAME1 "Data1"
#define DATASETNAME2 "Data2"
#define DATASETNAME3 "Data3"
#define DATASETNAME4 "Data4"
#define DATASETNAME7 "Data7"
#define DATASETNAME8 "Data8"
#define DATASETNAME9 "Data9"

#ifndef PATH_MAX
#define PATH_MAX 512
#endif /* !PATH_MAX */

/* Dataset data type.  Int's can be easily octo dumped. */
typedef int DATATYPE;

/* global variables */
int dim0;
int dim1;
int dim2;
int chunkdim0;
int chunkdim1;
int nerrors   = 0;               /* errors count */
int ndatasets = 300;             /* number of datasets to create*/
int ngroups   = 512;             /* number of groups to create in root
                                  * group. */
int facc_type       = FACC_MPIO; /*Test file access type */
int dxfer_coll_type = DXFER_COLLECTIVE_IO;

H5E_auto2_t old_func;        /* previous error handler */
void       *old_client_data; /* previous error handler arg.*/

#define NFILENAME    3
#define PARATESTFILE filenames[0]
const char *FILENAME[NFILENAME] = {"ParaTest", "Hugefile", NULL};
char       *filenames[NFILENAME];
hid_t       fapl; /* file access property list */
MPI_Comm    test_comm = MPI_COMM_WORLD;

/* Structure for passing test parameters around */
typedef struct test_params_t {
    char *filename;
} test_params_t;

// static int enable_error_stack = 0;   /* enable error stack; disable=0 enable=1 */
// static const char *TestProgName = NULL;
// static void (*TestPrivateUsage)(void) = NULL;
// static int (*TestPrivateParser)(int ac, char *av[]) = NULL;

/*
 * The following are various utility routines used by the tests.
 */

/*
 * Show command usage
 */
static void
usage(FILE *stream)
{
    fprintf(stream, "    [-r] [-w] [-m<n_datasets>] [-n<n_groups>] "
                    "[-o] [-f <prefix>] [-d <dim0> <dim1>]\n");
    fprintf(stream, "\t-m<n_datasets>"
                    "\tset number of datasets for the multiple dataset test\n");
    fprintf(stream, "\t-n<n_groups>"
                    "\tset number of groups for the multiple group test\n");
    fprintf(stream, "\t-f <prefix>\tfilename prefix\n");
    fprintf(stream, "\t-2\t\tuse Split-file together with MPIO\n");
    fprintf(stream, "\t-d <factor0> <factor1>\tdataset dimensions factors. Defaults (%d,%d)\n", BIG_X_FACTOR,
            BIG_Y_FACTOR);
    fprintf(stream, "\t-c <dim0> <dim1>\tdataset chunk dimensions. Defaults (dim0/10,dim1/10)\n");
    fprintf(stream, "\n");
}

/*
 * parse the command line options
 */
static int
parse_options(int argc, char **argv)
{
    int mpi_size, mpi_rank; /* mpi variables */

    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* setup default chunk-size. Make sure sizes are > 0 */

    chunkdim0 = (dim0 + 9) / 10;
    chunkdim1 = (dim1 + 9) / 10;

    while (--argc) {
        if (**(++argv) != '-') {
            break;
        }
        else {
            switch (*(*argv + 1)) {
                case 'm':
                    ndatasets = atoi((*argv + 1) + 1);
                    if (ndatasets < 0) {
                        nerrors++;
                        return (1);
                    }
                    break;
                case 'n':
                    ngroups = atoi((*argv + 1) + 1);
                    if (ngroups < 0) {
                        nerrors++;
                        return (1);
                    }
                    break;
                case 'f':
                    if (--argc < 1) {
                        nerrors++;
                        return (1);
                    }
                    if (**(++argv) == '-') {
                        nerrors++;
                        return (1);
                    }
                    paraprefix = *argv;
                    break;
                case 'i': /* Collective MPI-IO access with independent IO  */
                    dxfer_coll_type = DXFER_INDEPENDENT_IO;
                    break;
                case '2': /* Use the split-file driver with MPIO access */
                    /* Can use $HDF5_METAPREFIX to define the */
                    /* meta-file-prefix. */
                    facc_type = FACC_MPIO | FACC_SPLIT;
                    break;
                case 'd': /* dimensizes */
                    if (--argc < 2) {
                        nerrors++;
                        return (1);
                    }
                    dim0 = atoi(*(++argv)) * mpi_size;
                    argc--;
                    dim1 = atoi(*(++argv)) * mpi_size;
                    /* set default chunkdim sizes too */
                    chunkdim0 = (dim0 + 9) / 10;
                    chunkdim1 = (dim1 + 9) / 10;
                    break;
                case 'c': /* chunk dimensions */
                    if (--argc < 2) {
                        nerrors++;
                        return (1);
                    }
                    chunkdim0 = atoi(*(++argv));
                    argc--;
                    chunkdim1 = atoi(*(++argv));
                    break;
                case 'h': /* print help message--return with nerrors set */
                    return (1);
                default:
                    printf("Illegal option(%s)\n", *argv);
                    nerrors++;
                    return (1);
            }
        }
    } /*while*/

    /* check validity of dimension and chunk sizes */
    if (dim0 <= 0 || dim1 <= 0) {
        printf("Illegal dim sizes (%d, %d)\n", dim0, dim1);
        nerrors++;
        return (1);
    }
    if (chunkdim0 <= 0 || chunkdim1 <= 0) {
        printf("Illegal chunkdim sizes (%d, %d)\n", chunkdim0, chunkdim1);
        nerrors++;
        return (1);
    }

    /* Make sure datasets can be divided into equal portions by the processes */
    if ((dim0 % mpi_size) || (dim1 % mpi_size)) {
        if (MAINPROCESS)
            printf("dim0(%d) and dim1(%d) must be multiples of processes(%d)\n", dim0, dim1, mpi_size);
        nerrors++;
        return (1);
    }

    /* compose the test filenames */
    {
        int i, n;

        n = sizeof(FILENAME) / sizeof(FILENAME[0]) - 1; /* exclude the NULL */

        for (i = 0; i < n; i++)
            if (h5_fixname(FILENAME[i], fapl, filenames[i], PATH_MAX) == NULL) {
                printf("h5_fixname failed\n");
                nerrors++;
                return (1);
            }

        if (MAINPROCESS) {
            printf("Test filenames are:\n");
            for (i = 0; i < n; i++)
                printf("    %s\n", filenames[i]);
        }
    }

    return (0);
}

/*
 * Setup the dimensions of the hyperslab.
 * Two modes--by rows or by columns.
 * Assume dimension rank is 2.
 * BYROW    divide into slabs of rows
 * BYCOL    divide into blocks of columns
 * ZROW        same as BYROW except process 0 gets 0 rows
 * ZCOL        same as BYCOL except process 0 gets 0 columns
 */
static void
slab_set(int mpi_rank, int mpi_size, hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[],
         int mode)
{
    switch (mode) {
        case BYROW:
            /* Each process takes a slabs of rows. */
            block[0]  = (hsize_t)dim0 / (hsize_t)mpi_size;
            block[1]  = (hsize_t)dim1;
            stride[0] = block[0];
            stride[1] = block[1];
            count[0]  = 1;
            count[1]  = 1;
            start[0]  = (hsize_t)mpi_rank * block[0];
            start[1]  = 0;
            if (VERBOSE_MED)
                printf("slab_set BYROW\n");
            break;
        case BYCOL:
            /* Each process takes a block of columns. */
            block[0]  = (hsize_t)dim0;
            block[1]  = (hsize_t)dim1 / (hsize_t)mpi_size;
            stride[0] = block[0];
            stride[1] = block[1];
            count[0]  = 1;
            count[1]  = 1;
            start[0]  = 0;
            start[1]  = (hsize_t)mpi_rank * block[1];
            if (VERBOSE_MED)
                printf("slab_set BYCOL\n");
            break;
        case ZROW:
            /* Similar to BYROW except process 0 gets 0 row */
            block[0]  = (hsize_t)(mpi_rank ? dim0 / mpi_size : 0);
            block[1]  = (hsize_t)dim1;
            stride[0] = (mpi_rank ? block[0] : 1); /* avoid setting stride to 0 */
            stride[1] = block[1];
            count[0]  = 1;
            count[1]  = 1;
            start[0]  = (hsize_t)(mpi_rank ? (hsize_t)mpi_rank * block[0] : 0);
            start[1]  = 0;
            if (VERBOSE_MED)
                printf("slab_set ZROW\n");
            break;
        case ZCOL:
            /* Similar to BYCOL except process 0 gets 0 column */
            block[0]  = (hsize_t)dim0;
            block[1]  = (hsize_t)(mpi_rank ? dim1 / mpi_size : 0);
            stride[0] = block[0];
            stride[1] = (mpi_rank ? block[1] : 1); /* avoid setting stride to 0 */
            count[0]  = 1;
            count[1]  = 1;
            start[0]  = 0;
            start[1]  = (hsize_t)(mpi_rank ? (hsize_t)mpi_rank * block[1] : 0);
            if (VERBOSE_MED)
                printf("slab_set ZCOL\n");
            break;
        default:
            /* Unknown mode.  Set it to cover the whole dataset. */
            printf("unknown slab_set mode (%d)\n", mode);
            block[0]  = (hsize_t)dim0;
            block[1]  = (hsize_t)dim1;
            stride[0] = block[0];
            stride[1] = block[1];
            count[0]  = 1;
            count[1]  = 1;
            start[0]  = 0;
            start[1]  = 0;
            if (VERBOSE_MED)
                printf("slab_set wholeset\n");
            break;
    }
    if (VERBOSE_MED) {
        printf("start[]=(%lu,%lu), count[]=(%lu,%lu), stride[]=(%lu,%lu), block[]=(%lu,%lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));
    }
}

/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2 and data is stored contiguous.
 */
static void
dataset_fill(hsize_t start[], hsize_t block[], DATATYPE *dataset)
{
    DATATYPE *dataptr = dataset;
    hsize_t   i, j;

    /* put some trivial data in the data_array */
    for (i = 0; i < block[0]; i++) {
        for (j = 0; j < block[1]; j++) {
            *dataptr = (DATATYPE)((i + start[0]) * 100 + (j + start[1] + 1));
            dataptr++;
        }
    }
}

/*
 * Print the content of the dataset.
 */
static void
dataset_print(hsize_t start[], hsize_t block[], DATATYPE *dataset)
{
    DATATYPE *dataptr = dataset;
    hsize_t   i, j;

    /* print the column heading */
    printf("%-8s", "Cols:");
    for (j = 0; j < block[1]; j++) {
        printf("%3lu ", (unsigned long)(start[1] + j));
    }
    printf("\n");

    /* print the slab data */
    for (i = 0; i < block[0]; i++) {
        printf("Row %2lu: ", (unsigned long)(i + start[0]));
        for (j = 0; j < block[1]; j++) {
            printf("%03d ", *dataptr++);
        }
        printf("\n");
    }
}

/*
 * Print the content of the dataset.
 */
static int
dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], DATATYPE *dataset,
             DATATYPE *original)
{
    hsize_t i, j;
    int     vrfyerrs;

    /* print it if VERBOSE_MED */
    if (VERBOSE_MED) {
        printf("dataset_vrfy dumping:::\n");
        printf("start(%lu, %lu), count(%lu, %lu), stride(%lu, %lu), block(%lu, %lu)\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1]);
        printf("original values:\n");
        dataset_print(start, block, original);
        printf("compared values:\n");
        dataset_print(start, block, dataset);
    }

    vrfyerrs = 0;
    for (i = 0; i < block[0]; i++) {
        for (j = 0; j < block[1]; j++) {
            if (*dataset != *original) {
                if (vrfyerrs++ < MAX_ERR_REPORT || VERBOSE_MED) {
                    printf("Dataset Verify failed at [%lu][%lu](row %lu, col %lu): expect %d, got %d\n",
                           (unsigned long)i, (unsigned long)j, (unsigned long)(i + start[0]),
                           (unsigned long)(j + start[1]), *(original), *(dataset));
                }
                dataset++;
                original++;
            }
        }
    }
    if (vrfyerrs > MAX_ERR_REPORT && !VERBOSE_MED)
        printf("[more errors ...]\n");
    if (vrfyerrs)
        printf("%d errors found in dataset_vrfy\n", vrfyerrs);
    return (vrfyerrs);
}

/* NOTE:  This is a memory intensive test and is only run
 *        with 2 MPI ranks and with a testing express level
 *        of 0, i.e. Exhaustive test run is allowed.  Otherwise
 *        the test is skipped.
 *
 * Thanks to l.ferraro@cineca.it for the following test::
 *
 * This is a simple test case to reproduce a problem
 * occurring on LUSTRE filesystem with the creation
 * of a 4GB dataset using chunking with parallel HDF5.
 * The test works correctly if disabling chunking or
 * when the bytes assigned to each process is less
 * that 4GB. if equal or more, either hangs or results
 * in a PMPI_Waitall error.
 *
 * $> mpirun -genv I_MPI_EXTRA_FILESYSTEM on
 *           -genv I_MPI_EXTRA_FILESYSTEM_LIST gpfs
 *           -n 1 ./h5_mpi_big_dataset.x 1024 1024 1024
 */

#define H5FILE_NAME "hugefile.h5"
#define DATASETNAME "dataset"

static int
MpioTest2G(MPI_Comm comm)
{
    /*
     * HDF5 APIs definitions
     */
    herr_t status;
    hid_t  file_id, dset_id; /* file and dataset identifiers */
    hid_t  plist_id;         /* property list identifier */
    hid_t  filespace;        /* file and memory dataspace identifiers */
    int   *data;             /* pointer to data buffer to write */
    size_t tot_size_bytes;
    hid_t  dcpl_id;
    hid_t  memorydataspace;
    hid_t  filedataspace;
    size_t slice_per_process;
    size_t data_size;
    size_t data_size_bytes;

    hsize_t chunk[3];
    hsize_t h5_counts[3];
    hsize_t h5_offsets[3];
    hsize_t shape[3] = {1024, 1024, 1152};

    /*
     * MPI variables
     */
    int      mpi_size, mpi_rank;
    MPI_Info info = MPI_INFO_NULL;

    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    if (mpi_rank == 0) {
        printf("Using %d process on dataset shape "
               "[%" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE "]\n",
               mpi_size, shape[0], shape[1], shape[2]);
    }

    /*
     * Set up file access property list with parallel I/O access
     */
    plist_id = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((plist_id >= 0), "H5Pcreate file_access succeeded");
    status = H5Pset_fapl_mpio(plist_id, comm, info);
    VRFY((status >= 0), "H5Pset_dxpl_mpio succeeded");

    /*
     * Create a new file collectively and release property list identifier.
     */
    file_id = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    H5Pclose(plist_id);

    /*
     * Create the dataspace for the dataset.
     */
    tot_size_bytes = sizeof(int);
    for (int i = 0; i < 3; i++) {
        tot_size_bytes *= shape[i];
    }
    if (mpi_rank == 0) {
        printf("Dataset of %zu bytes\n", tot_size_bytes);
    }
    filespace = H5Screate_simple(3, shape, NULL);
    VRFY((filespace >= 0), "H5Screate_simple succeeded");

    /*
     * Select chunking
     */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl_id >= 0), "H5P_DATASET_CREATE");
    chunk[0] = 4;
    chunk[1] = shape[1];
    chunk[2] = shape[2];
    status   = H5Pset_chunk(dcpl_id, 3, chunk);
    VRFY((status >= 0), "H5Pset_chunk succeeded");

    /*
     * Create the dataset with default properties and close filespace.
     */
    dset_id = H5Dcreate2(file_id, DATASETNAME, H5T_NATIVE_INT, filespace, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dcreate2 succeeded");
    H5Sclose(filespace);

    /*
     * Create property list for collective dataset write.
     */
    plist_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((plist_id >= 0), "H5P_DATASET_XFER");
    status = H5Pset_dxpl_mpio(plist_id, H5FD_MPIO_COLLECTIVE);
    VRFY((status >= 0), "");

    H5_CHECKED_ASSIGN(slice_per_process, size_t, (shape[0] + (hsize_t)mpi_size - 1) / (hsize_t)mpi_size,
                      hsize_t);
    data_size       = slice_per_process * shape[1] * shape[2];
    data_size_bytes = sizeof(int) * data_size;
    data            = malloc(data_size_bytes);
    VRFY((data != NULL), "data malloc succeeded");

    for (size_t i = 0; i < data_size; i++) {
        data[i] = mpi_rank;
    }

    h5_counts[0]  = slice_per_process;
    h5_counts[1]  = shape[1];
    h5_counts[2]  = shape[2];
    h5_offsets[0] = (size_t)mpi_rank * slice_per_process;
    h5_offsets[1] = 0;
    h5_offsets[2] = 0;
    filedataspace = H5Screate_simple(3, shape, NULL);
    VRFY((filedataspace >= 0), "H5Screate_simple succeeded");

    // fix reminder along first dimension multiple of chunk[0]
    if (h5_offsets[0] + h5_counts[0] > shape[0]) {
        h5_counts[0] = shape[0] - h5_offsets[0];
    }

    status = H5Sselect_hyperslab(filedataspace, H5S_SELECT_SET, h5_offsets, NULL, h5_counts, NULL);
    VRFY((status >= 0), "H5Sselect_hyperslab succeeded");

    memorydataspace = H5Screate_simple(3, h5_counts, NULL);
    VRFY((memorydataspace >= 0), "H5Screate_simple succeeded");

    status = H5Dwrite(dset_id, H5T_NATIVE_INT, memorydataspace, filedataspace, plist_id, data);
    VRFY((status >= 0), "H5Dwrite succeeded");
    H5Pclose(plist_id);

    /*
     * Close/release resources.
     */
    H5Sclose(filedataspace);
    H5Sclose(memorydataspace);
    H5Dclose(dset_id);
    H5Fclose(file_id);

    free(data);
    printf("Proc %d - MpioTest2G test succeeded\n", mpi_rank);

    if (mpi_rank == 0)
        HDremove(FILENAME[1]);
    return 0;
}

/*
 * Part 1.a--Independent read/write for fixed dimension datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 files with parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset.
 */

static void
dataset_writeInd(void *params)
{
    hid_t   fid;                /* HDF5 file ID */
    hid_t   acc_tpl;            /* File access templates */
    hid_t   sid;                /* Dataspace ID */
    hid_t   file_dataspace;     /* File dataspace ID */
    hid_t   mem_dataspace;      /* memory dataspace ID */
    hid_t   dataset1, dataset2; /* Dataset ID */
    hsize_t dims[MAX_RANK] = {
        1,
    }; /* dataset dim sizes */
    hsize_t     data_size;
    DATATYPE   *data_array1 = NULL; /* data buffer */
    const char *filename;

    hsize_t start[MAX_RANK]; /* for hyperslab setting */
    hsize_t count[MAX_RANK];
    hsize_t stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];  /* for hyperslab setting */

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = ((const test_params_t *)params)->filename;
    if (VERBOSE_MED)
        printf("Independent write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* allocate memory for data buffer */
    data_size = sizeof(DATATYPE);
    data_size *= (hsize_t)dim0 * (hsize_t)dim1;
    data_array1 = (DATATYPE *)malloc(data_size);
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* ----------------------------------------
     * CREATE AN HDF5 FILE WITH PARALLEL ACCESS
     * ---------------------------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type);
    VRFY((acc_tpl >= 0), "");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* ---------------------------------------------
     * Define the dimensions of the overall datasets
     * and the slabs local to the MPI process.
     * ------------------------------------------- */
    /* setup dimensionality object */
    dims[0] = (hsize_t)dim0;
    dims[1] = (hsize_t)dim1;
    sid     = H5Screate_simple(MAX_RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate2 succeeded");

    /* create another dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate2 succeeded");

    /*
     * To test the independent orders of writes between processes, all
     * even number processes write to dataset1 first, then dataset2.
     * All odd number processes write to dataset2 first, then dataset1.
     */

    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");
    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset2 succeeded");

    /* setup dimensions again to write with zero rows for process 0 */
    if (VERBOSE_MED)
        printf("writeInd by some with zero row\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZROW);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if (MAINPROCESS) {
        ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("writeInd by some with zero row");
    if ((mpi_rank / 2) * 2 != mpi_rank) {
        ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
        VRFY((ret >= 0), "H5Dwrite dataset1 by ZROW succeeded");
    }
#ifdef BARRIER_CHECKS
    MPI_Barrier(test_comm);
#endif /* BARRIER_CHECKS */

    /* release dataspace ID */
    H5Sclose(file_dataspace);

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (data_array1)
        free(data_array1);
}

/* Example of using the parallel HDF5 library to read a dataset */
static void
dataset_readInd(void *params)
{
    hid_t       fid;                 /* HDF5 file ID */
    hid_t       acc_tpl;             /* File access templates */
    hid_t       file_dataspace;      /* File dataspace ID */
    hid_t       mem_dataspace;       /* memory dataspace ID */
    hid_t       dataset1, dataset2;  /* Dataset ID */
    DATATYPE   *data_array1  = NULL; /* data buffer */
    DATATYPE   *data_origin1 = NULL; /* expected data buffer */
    const char *filename;

    hsize_t start[MAX_RANK];                   /* for hyperslab setting */
    hsize_t count[MAX_RANK], stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];                   /* for hyperslab setting */

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = ((const test_params_t *)params)->filename;
    if (VERBOSE_MED)
        printf("Independent read test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");
    data_origin1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");

    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type);
    VRFY((acc_tpl >= 0), "");

    /* open the file collectively */
    fid = H5Fopen(filename, H5F_ACC_RDONLY, acc_tpl);
    VRFY((fid >= 0), "");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "");

    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (ret)
        nerrors++;

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (ret)
        nerrors++;

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "");

    /* release all IDs created */
    H5Sclose(file_dataspace);

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (data_array1)
        free(data_array1);
    if (data_origin1)
        free(data_origin1);
}

/*
 * Part 1.b--Collective read/write for fixed dimension datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset. [Note: not so yet.  Datasets are of sizes dim0xdim1 and
 * each process controls a hyperslab within.]
 */

static void
dataset_writeAll(void *params)
{
    hid_t   fid;                                    /* HDF5 file ID */
    hid_t   acc_tpl;                                /* File access templates */
    hid_t   xfer_plist;                             /* Dataset transfer properties list */
    hid_t   sid;                                    /* Dataspace ID */
    hid_t   file_dataspace;                         /* File dataspace ID */
    hid_t   mem_dataspace;                          /* memory dataspace ID */
    hid_t   dataset1, dataset2, dataset3, dataset4; /* Dataset ID */
    hid_t   dataset5, dataset6, dataset7;           /* Dataset ID */
    hid_t   datatype;                               /* Datatype ID */
    hsize_t dims[MAX_RANK] = {
        1,
    };                              /* dataset dim sizes */
    DATATYPE   *data_array1 = NULL; /* data buffer */
    const char *filename;

    hsize_t start[MAX_RANK]; /* for hyperslab setting */
    hsize_t count[MAX_RANK];
    hsize_t stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];  /* for hyperslab setting */

    size_t   num_points;    /* for point selection */
    hsize_t *coords = NULL; /* for point selection */
    hsize_t  current_dims;  /* for point selection */

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = ((const test_params_t *)params)->filename;
    if (VERBOSE_MED)
        printf("Collective write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* set up the coords array selection */
    num_points = (size_t)dim1;
    coords     = (hsize_t *)malloc((size_t)dim1 * (size_t)MAX_RANK * sizeof(hsize_t));
    VRFY((coords != NULL), "coords malloc succeeded");

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type);
    VRFY((acc_tpl >= 0), "");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup 2-D dimensionality object */
    dims[0] = (hsize_t)dim0;
    dims[1] = (hsize_t)dim1;
    sid     = H5Screate_simple(MAX_RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate2 succeeded");

    /* create another dataset collectively */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    ret      = H5Tset_order(datatype, H5T_ORDER_LE);
    VRFY((ret >= 0), "H5Tset_order succeeded");

    dataset2 = H5Dcreate2(fid, DATASETNAME2, datatype, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate2 2 succeeded");

    /* create a third dataset collectively */
    dataset3 = H5Dcreate2(fid, DATASETNAME3, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset3 >= 0), "H5Dcreate2 succeeded");

    dataset5 = H5Dcreate2(fid, DATASETNAME7, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset5 >= 0), "H5Dcreate2 succeeded");
    dataset6 = H5Dcreate2(fid, DATASETNAME8, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset6 >= 0), "H5Dcreate2 succeeded");
    dataset7 = H5Dcreate2(fid, DATASETNAME9, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset7 >= 0), "H5Dcreate2 succeeded");

    /* release 2-D space ID created */
    H5Sclose(sid);

    /* setup scalar dimensionality object */
    sid = H5Screate(H5S_SCALAR);
    VRFY((sid >= 0), "H5Screate succeeded");

    /* create a fourth dataset collectively */
    dataset4 = H5Dcreate2(fid, DATASETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset4 >= 0), "H5Dcreate2 succeeded");

    /* release scalar space ID created */
    H5Sclose(sid);

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of rows. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill the local slab with some trivial data */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* write data collectively */
    MESG("writeAll by Row");
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");

    /* setup dimensions again to writeAll with zero rows for process 0 */
    if (VERBOSE_MED)
        printf("writeAll by some with zero row\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZROW);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if (MAINPROCESS) {
        ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("writeAll by some with zero row");
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 by ZROW succeeded");

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of columns. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill the local slab with some trivial data */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset2 succeeded");

    /* setup dimensions again to writeAll with zero columns for process 0 */
    if (VERBOSE_MED)
        printf("writeAll by some with zero col\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZCOL);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if (MAINPROCESS) {
        ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("writeAll by some with zero col");
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 by ZCOL succeeded");

    /* release all temporary handles. */
    /* Could have used them for dataset3 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset3: each process takes a block of rows, except process zero uses "none" selection. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset3);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    if (MAINPROCESS) {
        ret = H5Sselect_none(file_dataspace);
        VRFY((ret >= 0), "H5Sselect_none file_dataspace succeeded");
    } /* end if */
    else {
        ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sselect_hyperslab succeeded");
    } /* end else */

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");
    if (MAINPROCESS) {
        ret = H5Sselect_none(mem_dataspace);
        VRFY((ret >= 0), "H5Sselect_none mem_dataspace succeeded");
    } /* end if */

    /* fill the local slab with some trivial data */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    } /* end if */

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* write data collectively */
    MESG("writeAll with none");
    ret = H5Dwrite(dataset3, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset3 succeeded");

    /* write data collectively (with datatype conversion) */
    MESG("writeAll with none");
    ret = H5Dwrite(dataset3, H5T_NATIVE_UCHAR, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset3 succeeded");

    /* release all temporary handles. */
    /* Could have used them for dataset4 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset4: each process writes no data, except process zero uses "all" selection. */
    /* Additionally, these are in a scalar dataspace */

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset4);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    if (MAINPROCESS) {
        ret = H5Sselect_none(file_dataspace);
        VRFY((ret >= 0), "H5Sselect_all file_dataspace succeeded");
    } /* end if */
    else {
        ret = H5Sselect_all(file_dataspace);
        VRFY((ret >= 0), "H5Sselect_none succeeded");
    } /* end else */

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate(H5S_SCALAR);
    VRFY((mem_dataspace >= 0), "");
    if (MAINPROCESS) {
        ret = H5Sselect_none(mem_dataspace);
        VRFY((ret >= 0), "H5Sselect_all mem_dataspace succeeded");
    } /* end if */
    else {
        ret = H5Sselect_all(mem_dataspace);
        VRFY((ret >= 0), "H5Sselect_none succeeded");
    } /* end else */

    /* fill the local slab with some trivial data */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    } /* end if */

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* write data collectively */
    MESG("writeAll with scalar dataspace");
    ret = H5Dwrite(dataset4, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset4 succeeded");

    /* write data collectively (with datatype conversion) */
    MESG("writeAll with scalar dataspace");
    ret = H5Dwrite(dataset4, H5T_NATIVE_UCHAR, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset4 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    if (data_array1)
        free(data_array1);
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    block[0]  = 1;
    block[1]  = (hsize_t)dim1;
    stride[0] = 1;
    stride[1] = (hsize_t)dim1;
    count[0]  = 1;
    count[1]  = 1;
    start[0]  = (hsize_t)dim0 / (hsize_t)mpi_size * (hsize_t)mpi_rank;
    start[1]  = 0;

    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* Dataset5: point selection in File - Hyperslab selection in Memory*/
    /* create a file dataspace independently */
    point_set(start, count, stride, block, num_points, coords, OUT_OF_ORDER);
    if (VERBOSE_MED) {
        hsize_t k = 0;

        printf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));

        for (size_t i = 0; i < num_points; i++) {
            printf("(%d, %d)\n", (int)coords[k], (int)coords[k + 1]);
            k += MAX_RANK;
        }
    }

    file_dataspace = H5Dget_space(dataset5);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    start[0]      = 0;
    start[1]      = 0;
    mem_dataspace = H5Dget_space(dataset5);
    VRFY((mem_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* write data collectively */
    ret = H5Dwrite(dataset5, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset5 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset6: point selection in File - Point selection in Memory*/
    /* create a file dataspace independently */
    start[0] = (hsize_t)dim0 / (hsize_t)mpi_size * (hsize_t)mpi_rank;
    start[1] = 0;
    point_set(start, count, stride, block, num_points, coords, OUT_OF_ORDER);
    if (VERBOSE_MED) {
        hsize_t k = 0;

        printf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));

        for (size_t i = 0; i < num_points; i++) {
            printf("(%d, %d)\n", (int)coords[k], (int)coords[k + 1]);
            k += MAX_RANK;
        }
    }

    file_dataspace = H5Dget_space(dataset6);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    start[0] = 0;
    start[1] = 0;
    point_set(start, count, stride, block, num_points, coords, IN_ORDER);
    if (VERBOSE_MED) {
        hsize_t k = 0;

        printf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));

        for (size_t i = 0; i < num_points; i++) {
            printf("(%d, %d)\n", (int)coords[k], (int)coords[k + 1]);
            k += MAX_RANK;
        }
    }

    mem_dataspace = H5Dget_space(dataset6);
    VRFY((mem_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(mem_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* write data collectively */
    ret = H5Dwrite(dataset6, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset6 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset7: point selection in File - All selection in Memory*/
    /* create a file dataspace independently */
    start[0] = (hsize_t)dim0 / (hsize_t)mpi_size * (hsize_t)mpi_rank;
    start[1] = 0;
    point_set(start, count, stride, block, num_points, coords, IN_ORDER);
    if (VERBOSE_MED) {
        hsize_t k = 0;

        printf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));

        for (size_t i = 0; i < num_points; i++) {
            printf("(%d, %d)\n", (int)coords[k], (int)coords[k + 1]);
            k += MAX_RANK;
        }
    }

    file_dataspace = H5Dget_space(dataset7);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    current_dims  = num_points;
    mem_dataspace = H5Screate_simple(1, &current_dims, NULL);
    VRFY((mem_dataspace >= 0), "mem_dataspace create succeeded");

    ret = H5Sselect_all(mem_dataspace);
    VRFY((ret >= 0), "H5Sselect_all succeeded");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* write data collectively */
    ret = H5Dwrite(dataset7, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset7 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");
    ret = H5Dclose(dataset3);
    VRFY((ret >= 0), "H5Dclose3 succeeded");
    ret = H5Dclose(dataset4);
    VRFY((ret >= 0), "H5Dclose4 succeeded");
    ret = H5Dclose(dataset5);
    VRFY((ret >= 0), "H5Dclose5 succeeded");
    ret = H5Dclose(dataset6);
    VRFY((ret >= 0), "H5Dclose6 succeeded");
    ret = H5Dclose(dataset7);
    VRFY((ret >= 0), "H5Dclose7 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (coords)
        free(coords);
    if (data_array1)
        free(data_array1);
}

/*
 * Example of using the parallel HDF5 library to read two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset. [Note: not so yet.  Datasets are of sizes dim0xdim1 and
 * each process controls a hyperslab within.]
 */

static void
dataset_readAll(void *params)
{
    hid_t       fid;                                              /* HDF5 file ID */
    hid_t       acc_tpl;                                          /* File access templates */
    hid_t       xfer_plist;                                       /* Dataset transfer properties list */
    hid_t       file_dataspace;                                   /* File dataspace ID */
    hid_t       mem_dataspace;                                    /* memory dataspace ID */
    hid_t       dataset1, dataset2, dataset5, dataset6, dataset7; /* Dataset ID */
    DATATYPE   *data_array1  = NULL;                              /* data buffer */
    DATATYPE   *data_origin1 = NULL;                              /* expected data buffer */
    const char *filename;

    hsize_t start[MAX_RANK];                   /* for hyperslab setting */
    hsize_t count[MAX_RANK], stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];                   /* for hyperslab setting */

    size_t   num_points;    /* for point selection */
    hsize_t *coords = NULL; /* for point selection */
    int      i, j, k;

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = ((const test_params_t *)params)->filename;
    if (VERBOSE_MED)
        printf("Collective read test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* set up the coords array selection */
    num_points = (size_t)dim1;
    coords     = (hsize_t *)malloc((size_t)dim0 * (size_t)dim1 * MAX_RANK * sizeof(hsize_t));
    VRFY((coords != NULL), "coords malloc succeeded");

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");
    data_origin1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");

    /* -------------------
     * OPEN AN HDF5 FILE
     * -------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type);
    VRFY((acc_tpl >= 0), "");

    /* open the file collectively */
    fid = H5Fopen(filename, H5F_ACC_RDONLY, acc_tpl);
    VRFY((fid >= 0), "H5Fopen succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* --------------------------
     * Open the datasets in it
     * ------------------------- */
    /* open the dataset1 collectively */
    dataset1 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dopen2 succeeded");

    /* open another dataset collectively */
    dataset2 = H5Dopen2(fid, DATASETNAME2, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dopen2 2 succeeded");

    /* open another dataset collectively */
    dataset5 = H5Dopen2(fid, DATASETNAME7, H5P_DEFAULT);
    VRFY((dataset5 >= 0), "H5Dopen2 5 succeeded");
    dataset6 = H5Dopen2(fid, DATASETNAME8, H5P_DEFAULT);
    VRFY((dataset6 >= 0), "H5Dopen2 6 succeeded");
    dataset7 = H5Dopen2(fid, DATASETNAME9, H5P_DEFAULT);
    VRFY((dataset7 >= 0), "H5Dopen2 7 succeeded");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of columns. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_origin1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset1 succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (ret)
        nerrors++;

    /* setup dimensions again to readAll with zero columns for process 0 */
    if (VERBOSE_MED)
        printf("readAll by some with zero col\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZCOL);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if (MAINPROCESS) {
        ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("readAll by some with zero col");
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset1 by ZCOL succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (ret)
        nerrors++;

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of rows. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_origin1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset2 succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (ret)
        nerrors++;

    /* setup dimensions again to readAll with zero rows for process 0 */
    if (VERBOSE_MED)
        printf("readAll by some with zero row\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZROW);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if (MAINPROCESS) {
        ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("readAll by some with zero row");
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset1 by ZROW succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (ret)
        nerrors++;

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    if (data_array1)
        free(data_array1);
    if (data_origin1)
        free(data_origin1);
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");
    data_origin1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");

    block[0]  = 1;
    block[1]  = (hsize_t)dim1;
    stride[0] = 1;
    stride[1] = (hsize_t)dim1;
    count[0]  = 1;
    count[1]  = 1;
    start[0]  = (hsize_t)dim0 / (hsize_t)mpi_size * (hsize_t)mpi_rank;
    start[1]  = 0;

    dataset_fill(start, block, data_origin1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_origin1);
    }

    /* Dataset5: point selection in memory - Hyperslab selection in file*/
    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset5);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    start[0] = 0;
    start[1] = 0;
    point_set(start, count, stride, block, num_points, coords, OUT_OF_ORDER);
    if (VERBOSE_MED) {
        hsize_t idx = 0;

        printf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));

        for (size_t point = 0; point < num_points; point++) {
            printf("(%d, %d)\n", (int)coords[idx], (int)coords[idx + 1]);
            idx += MAX_RANK;
        }
    }

    mem_dataspace = H5Dget_space(dataset5);
    VRFY((mem_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(mem_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset5, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset5 succeeded");

    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (ret)
        nerrors++;

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    if (data_array1)
        free(data_array1);
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* Dataset6: point selection in File - Point selection in Memory*/
    /* create a file dataspace independently */
    start[0] = (hsize_t)dim0 / (hsize_t)mpi_size * (hsize_t)mpi_rank;
    start[1] = 0;
    point_set(start, count, stride, block, num_points, coords, IN_ORDER);
    if (VERBOSE_MED) {
        hsize_t idx = 0;

        printf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));

        for (size_t point = 0; point < num_points; point++) {
            printf("(%d, %d)\n", (int)coords[idx], (int)coords[idx + 1]);
            idx += MAX_RANK;
        }
    }

    file_dataspace = H5Dget_space(dataset6);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    start[0] = 0;
    start[1] = 0;
    point_set(start, count, stride, block, num_points, coords, OUT_OF_ORDER);
    if (VERBOSE_MED) {
        hsize_t idx = 0;

        printf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));

        for (size_t point = 0; point < num_points; point++) {
            printf("(%d, %d)\n", (int)coords[idx], (int)coords[idx + 1]);
            idx += MAX_RANK;
        }
    }

    mem_dataspace = H5Dget_space(dataset6);
    VRFY((mem_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(mem_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset6, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset6 succeeded");

    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if (ret)
        nerrors++;

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    if (data_array1)
        free(data_array1);
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* Dataset7: point selection in memory - All selection in file*/
    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset7);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_all(file_dataspace);
    VRFY((ret >= 0), "H5Sselect_all succeeded");

    num_points = (size_t)dim0 * (size_t)dim1;
    k          = 0;
    for (i = 0; i < dim0; i++) {
        for (j = 0; j < dim1; j++) {
            coords[k++] = (hsize_t)i;
            coords[k++] = (hsize_t)j;
        }
    }
    mem_dataspace = H5Dget_space(dataset7);
    VRFY((mem_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(mem_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset7, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset7 succeeded");

    start[0] = (hsize_t)dim0 / (hsize_t)mpi_size * (hsize_t)mpi_rank;
    start[1] = 0;
    ret      = dataset_vrfy(start, count, stride, block, data_array1 + (dim0 / mpi_size * dim1 * mpi_rank),
                            data_origin1);
    if (ret)
        nerrors++;

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /*
     * All reads completed.  Close datasets collectively
     */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");
    ret = H5Dclose(dataset5);
    VRFY((ret >= 0), "H5Dclose5 succeeded");
    ret = H5Dclose(dataset6);
    VRFY((ret >= 0), "H5Dclose6 succeeded");
    ret = H5Dclose(dataset7);
    VRFY((ret >= 0), "H5Dclose7 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (coords)
        free(coords);
    if (data_array1)
        free(data_array1);
    if (data_origin1)
        free(data_origin1);
}

/*
 * Example of using the parallel HDF5 library to create an extendable dataset
 * and perform I/O on it in a way that verifies that the chunk cache is
 * bypassed for parallel I/O.
 */

static void
extend_writeInd2(void *params)
{
    const char *filename;
    hid_t       fid;             /* HDF5 file ID */
    hid_t       fapl_id;         /* File access templates */
    hid_t       fs;              /* File dataspace ID */
    hid_t       ms;              /* Memory dataspace ID */
    hid_t       dataset;         /* Dataset ID */
    hsize_t     orig_size  = 10; /* Original dataset dim size */
    hsize_t     new_size   = 20; /* Extended dataset dim size */
    hsize_t     one        = 1;
    hsize_t     max_size   = H5S_UNLIMITED; /* dataset maximum dim size */
    hsize_t     chunk_size = 16384;         /* chunk size */
    hid_t       dcpl;                       /* dataset create prop. list */
    int         written[10],                /* Data to write */
        retrieved[10];                      /* Data read in */
    int    mpi_size, mpi_rank;              /* MPI settings */
    int    i;                               /* Local index variable */
    herr_t ret;                             /* Generic return value */

    filename = ((const test_params_t *)params)->filename;
    if (VERBOSE_MED)
        printf("Extend independent write test #2 on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template */
    fapl_id = create_faccess_plist(test_comm, MPI_INFO_NULL, facc_type);
    VRFY((fapl_id >= 0), "create_faccess_plist succeeded");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(fapl_id);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /* --------------------------------------------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------------------------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl >= 0), "H5Pcreate succeeded");
    ret = H5Pset_chunk(dcpl, 1, &chunk_size);
    VRFY((ret >= 0), "H5Pset_chunk succeeded");

    /* setup dimensionality object */
    fs = H5Screate_simple(1, &orig_size, &max_size);
    VRFY((fs >= 0), "H5Screate_simple succeeded");

    /* create an extendible dataset collectively */
    dataset = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, fs, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreat2e succeeded");

    /* release resource */
    ret = H5Pclose(dcpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /* -------------------------
     * Test writing to dataset
     * -------------------------*/
    /* create a memory dataspace independently */
    ms = H5Screate_simple(1, &orig_size, &max_size);
    VRFY((ms >= 0), "H5Screate_simple succeeded");

    /* put some trivial data in the data_array */
    for (i = 0; i < (int)orig_size; i++)
        written[i] = i;
    MESG("data array initialized");
    if (VERBOSE_MED) {
        MESG("writing at offset zero: ");
        for (i = 0; i < (int)orig_size; i++)
            printf("%s%d", i ? ", " : "", written[i]);
        printf("\n");
    }
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, written);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* -------------------------
     * Read initial data from dataset.
     * -------------------------*/
    ret = H5Dread(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, retrieved);
    VRFY((ret >= 0), "H5Dread succeeded");
    for (i = 0; i < (int)orig_size; i++)
        if (written[i] != retrieved[i]) {
            printf("Line #%d: written!=retrieved: written[%d]=%d, retrieved[%d]=%d\n", __LINE__, i,
                   written[i], i, retrieved[i]);
            nerrors++;
        }
    if (VERBOSE_MED) {
        MESG("read at offset zero: ");
        for (i = 0; i < (int)orig_size; i++)
            printf("%s%d", i ? ", " : "", retrieved[i]);
        printf("\n");
    }

    /* -------------------------
     * Extend the dataset & retrieve new dataspace
     * -------------------------*/
    ret = H5Dset_extent(dataset, &new_size);
    VRFY((ret >= 0), "H5Dset_extent succeeded");
    ret = H5Sclose(fs);
    VRFY((ret >= 0), "H5Sclose succeeded");
    fs = H5Dget_space(dataset);
    VRFY((fs >= 0), "H5Dget_space succeeded");

    /* -------------------------
     * Write to the second half of the dataset
     * -------------------------*/
    for (i = 0; i < (int)orig_size; i++)
        H5_CHECKED_ASSIGN(written[i], int, orig_size + (hsize_t)i, hsize_t);
    MESG("data array re-initialized");
    if (VERBOSE_MED) {
        MESG("writing at offset 10: ");
        for (i = 0; i < (int)orig_size; i++)
            printf("%s%d", i ? ", " : "", written[i]);
        printf("\n");
    }
    ret = H5Sselect_hyperslab(fs, H5S_SELECT_SET, &orig_size, NULL, &one, &orig_size);
    VRFY((ret >= 0), "H5Sselect_hyperslab succeeded");
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, written);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* -------------------------
     * Read the new data
     * -------------------------*/
    ret = H5Dread(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, retrieved);
    VRFY((ret >= 0), "H5Dread succeeded");
    for (i = 0; i < (int)orig_size; i++)
        if (written[i] != retrieved[i]) {
            printf("Line #%d: written!=retrieved: written[%d]=%d, retrieved[%d]=%d\n", __LINE__, i,
                   written[i], i, retrieved[i]);
            nerrors++;
        }
    if (VERBOSE_MED) {
        MESG("read at offset 10: ");
        for (i = 0; i < (int)orig_size; i++)
            printf("%s%d", i ? ", " : "", retrieved[i]);
        printf("\n");
    }

    /* Close dataset collectively */
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded");

    /* Close the file collectively */
    ret = H5Fclose(fid);
    VRFY((ret >= 0), "H5Fclose succeeded");
}

/*
 * Example of using the parallel HDF5 library to read a compressed
 * dataset in an HDF5 file with collective parallel access support.
 */
#ifdef H5_HAVE_FILTER_DEFLATE
static void
compress_readAll(void *params)
{
    hid_t       fid;                           /* HDF5 file ID */
    hid_t       acc_tpl;                       /* File access templates */
    hid_t       dcpl;                          /* Dataset creation property list */
    hid_t       xfer_plist;                    /* Dataset transfer properties list */
    hid_t       dataspace;                     /* Dataspace ID */
    hid_t       dataset;                       /* Dataset ID */
    int         rank = 1;                      /* Dataspace rank */
    hsize_t     dim  = (hsize_t)dim0;          /* Dataspace dimensions */
    unsigned    u;                             /* Local index variable */
    unsigned    chunk_opts;                    /* Chunk options */
    unsigned    disable_partial_chunk_filters; /* Whether filters are disabled on partial chunks */
    DATATYPE   *data_read = NULL;              /* data buffer */
    DATATYPE   *data_orig = NULL;              /* expected data buffer */
    const char *filename;
    MPI_Comm    comm = test_comm;
    MPI_Info    info = MPI_INFO_NULL;
    int         mpi_size, mpi_rank;
    herr_t      ret; /* Generic return value */

    filename = ((const test_params_t *)params)->filename;
    if (VERBOSE_MED)
        printf("Collective chunked dataset read test on file %s\n", filename);

    /* Retrieve MPI parameters */
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    /* Allocate data buffer */
    data_orig = (DATATYPE *)malloc((size_t)dim * sizeof(DATATYPE));
    VRFY((data_orig != NULL), "data_origin1 malloc succeeded");
    data_read = (DATATYPE *)malloc((size_t)dim * sizeof(DATATYPE));
    VRFY((data_read != NULL), "data_array1 malloc succeeded");

    /* Initialize data buffers */
    for (u = 0; u < dim; u++)
        data_orig[u] = (DATATYPE)u;

    /* Run test both with and without filters disabled on partial chunks */
    for (disable_partial_chunk_filters = 0; disable_partial_chunk_filters <= 1;
         disable_partial_chunk_filters++) {
        /* Process zero creates the file with a compressed, chunked dataset */
        if (mpi_rank == 0) {
            hsize_t chunk_dim; /* Chunk dimensions */

            /* Create the file */
            fid = H5Fcreate(h5_rmprefix(filename), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
            VRFY((fid > 0), "H5Fcreate succeeded");

            /* Create property list for chunking and compression */
            dcpl = H5Pcreate(H5P_DATASET_CREATE);
            VRFY((dcpl > 0), "H5Pcreate succeeded");

            ret = H5Pset_layout(dcpl, H5D_CHUNKED);
            VRFY((ret >= 0), "H5Pset_layout succeeded");

            /* Use eight chunks */
            chunk_dim = dim / 8;
            ret       = H5Pset_chunk(dcpl, rank, &chunk_dim);
            VRFY((ret >= 0), "H5Pset_chunk succeeded");

            /* Set chunk options appropriately */
            if (disable_partial_chunk_filters) {
                ret = H5Pget_chunk_opts(dcpl, &chunk_opts);
                VRFY((ret >= 0), "H5Pget_chunk_opts succeeded");

                chunk_opts |= H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS;

                ret = H5Pset_chunk_opts(dcpl, chunk_opts);
                VRFY((ret >= 0), "H5Pset_chunk_opts succeeded");
            } /* end if */

            ret = H5Pset_deflate(dcpl, 9);
            VRFY((ret >= 0), "H5Pset_deflate succeeded");

            /* Create dataspace */
            dataspace = H5Screate_simple(rank, &dim, NULL);
            VRFY((dataspace > 0), "H5Screate_simple succeeded");

            /* Create dataset */
            dataset =
                H5Dcreate2(fid, "compressed_data", H5T_NATIVE_INT, dataspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
            VRFY((dataset > 0), "H5Dcreate2 succeeded");

            /* Write compressed data */
            ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_orig);
            VRFY((ret >= 0), "H5Dwrite succeeded");

            /* Close objects */
            ret = H5Pclose(dcpl);
            VRFY((ret >= 0), "H5Pclose succeeded");
            ret = H5Sclose(dataspace);
            VRFY((ret >= 0), "H5Sclose succeeded");
            ret = H5Dclose(dataset);
            VRFY((ret >= 0), "H5Dclose succeeded");
            ret = H5Fclose(fid);
            VRFY((ret >= 0), "H5Fclose succeeded");
        }

        /* Wait for file to be created */
        MPI_Barrier(comm);

        /* -------------------
         * OPEN AN HDF5 FILE
         * -------------------*/

        /* setup file access template */
        acc_tpl = create_faccess_plist(comm, info, facc_type);
        VRFY((acc_tpl >= 0), "");

        /* open the file collectively */
        fid = H5Fopen(filename, H5F_ACC_RDWR, acc_tpl);
        VRFY((fid > 0), "H5Fopen succeeded");

        /* Release file-access template */
        ret = H5Pclose(acc_tpl);
        VRFY((ret >= 0), "H5Pclose succeeded");

        /* Open dataset with compressed chunks */
        dataset = H5Dopen2(fid, "compressed_data", H5P_DEFAULT);
        VRFY((dataset > 0), "H5Dopen2 succeeded");

        /* Try reading & writing data */
        if (dataset > 0) {
            /* Create dataset transfer property list */
            xfer_plist = H5Pcreate(H5P_DATASET_XFER);
            VRFY((xfer_plist > 0), "H5Pcreate succeeded");

            ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
            if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
                ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
                VRFY((ret >= 0), "set independent IO collectively succeeded");
            }

            /* Try reading the data */
            ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer_plist, data_read);
            VRFY((ret >= 0), "H5Dread succeeded");

            /* Verify data read */
            for (u = 0; u < dim; u++)
                if (data_orig[u] != data_read[u]) {
                    printf("Line #%d: written!=retrieved: data_orig[%u]=%d, data_read[%u]=%d\n", __LINE__,
                           (unsigned)u, data_orig[u], (unsigned)u, data_read[u]);
                    nerrors++;
                }

#ifdef H5_HAVE_PARALLEL_FILTERED_WRITES
            ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer_plist, data_read);
            VRFY((ret >= 0), "H5Dwrite succeeded");
#endif

            ret = H5Pclose(xfer_plist);
            VRFY((ret >= 0), "H5Pclose succeeded");
            ret = H5Dclose(dataset);
            VRFY((ret >= 0), "H5Dclose succeeded");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        VRFY((ret >= 0), "H5Fclose succeeded");
    } /* end for */

    /* release data buffers */
    if (data_read)
        free(data_read);
    if (data_orig)
        free(data_orig);
}
#endif /* H5_HAVE_FILTER_DEFLATE */

/*
 * Part 4--Non-selection for chunked dataset
 */

/*
 * Example of using the parallel HDF5 library to create chunked
 * dataset in one HDF5 file with collective and independent parallel
 * MPIO access support.  The Datasets are of sizes dim0 x dim1.
 * Each process controls only a slab of size dim0 x dim1 within the
 * dataset with the exception that one processor selects no element.
 */

static void
none_selection_chunk(void *params)
{
    hid_t       fid;                /* HDF5 file ID */
    hid_t       acc_tpl;            /* File access templates */
    hid_t       xfer_plist;         /* Dataset transfer properties list */
    hid_t       sid;                /* Dataspace ID */
    hid_t       file_dataspace;     /* File dataspace ID */
    hid_t       mem_dataspace;      /* memory dataspace ID */
    hid_t       dataset1, dataset2; /* Dataset ID */
    const char *filename;
    hsize_t     dims[MAX_RANK];       /* dataset dim sizes */
    DATATYPE   *data_origin = NULL;   /* data buffer */
    DATATYPE   *data_array  = NULL;   /* data buffer */
    hsize_t     chunk_dims[MAX_RANK]; /* chunk sizes */
    hid_t       dataset_pl;           /* dataset create prop. list */

    hsize_t start[MAX_RANK];  /* for hyperslab setting */
    hsize_t count[MAX_RANK];  /* for hyperslab setting */
    hsize_t stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];  /* for hyperslab setting */
    hsize_t mstart[MAX_RANK]; /* for data buffer in memory */

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = ((const test_params_t *)params)->filename;
    if (VERBOSE_MED)
        printf("Extend independent write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* setup chunk-size. Make sure sizes are > 0 */
    chunk_dims[0] = (hsize_t)chunkdim0;
    chunk_dims[1] = (hsize_t)chunkdim1;

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type);
    VRFY((acc_tpl >= 0), "");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* --------------------------------------------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------------------------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    if (VERBOSE_MED)
        printf("chunks[]=%lu,%lu\n", (unsigned long)chunk_dims[0], (unsigned long)chunk_dims[1]);
    dataset_pl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dataset_pl >= 0), "H5Pcreate succeeded");
    ret = H5Pset_chunk(dataset_pl, MAX_RANK, chunk_dims);
    VRFY((ret >= 0), "H5Pset_chunk succeeded");

    /* setup dimensionality object */
    dims[0] = (hsize_t)dim0;
    dims[1] = (hsize_t)dim1;
    sid     = H5Screate_simple(MAX_RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    /* create an extendible dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dataset_pl, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate2 succeeded");

    /* create another extendible dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dataset_pl, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate2 succeeded");

    /* release resource */
    H5Sclose(sid);
    H5Pclose(dataset_pl);

    /* -------------------------
     * Test collective writing to dataset1
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* allocate memory for data buffer. Only allocate enough buffer for
     * each processor's data. */
    if (mpi_rank) {
        data_origin = (DATATYPE *)malloc(block[0] * block[1] * sizeof(DATATYPE));
        VRFY((data_origin != NULL), "data_origin malloc succeeded");

        data_array = (DATATYPE *)malloc(block[0] * block[1] * sizeof(DATATYPE));
        VRFY((data_array != NULL), "data_array malloc succeeded");

        /* put some trivial data in the data_array */
        mstart[0] = mstart[1] = 0;
        dataset_fill(mstart, block, data_origin);
        MESG("data_array initialized");
        if (VERBOSE_MED) {
            MESG("data_array created");
            dataset_print(mstart, block, data_origin);
        }
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Process 0 has no selection */
    if (!mpi_rank) {
        ret = H5Sselect_none(mem_dataspace);
        VRFY((ret >= 0), "H5Sselect_none succeeded");
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* Process 0 has no selection */
    if (!mpi_rank) {
        ret = H5Sselect_none(file_dataspace);
        VRFY((ret >= 0), "H5Sselect_none succeeded");
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    /* write data collectively */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_origin);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    if (mpi_rank) {
        ret = dataset_vrfy(mstart, count, stride, block, data_array, data_origin);
        if (ret)
            nerrors++;
    }

    /* -------------------------
     * Test independent writing to dataset2
     * -------------------------*/
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_INDEPENDENT);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    /* write data collectively */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_origin);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    if (mpi_rank) {
        ret = dataset_vrfy(mstart, count, stride, block, data_array, data_origin);
        if (ret)
            nerrors++;
    }

    /* release resource */
    ret = H5Sclose(file_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Sclose(mem_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Pclose(xfer_plist);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (data_origin)
        free(data_origin);
    if (data_array)
        free(data_array);
}

int
main(int argc, char **argv)
{
    test_params_t test_params;
    int           express_test;
    int           mpi_size, mpi_rank; /* mpi variables */
    hsize_t       oldsize, newsize = 1048576;

#ifndef H5_HAVE_WIN32_API
    /* Un-buffer the stdout and stderr */
    setbuf(stderr, NULL);
    setbuf(stdout, NULL);
#endif

    MPI_Init(&argc, &argv);
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    memset(filenames, 0, sizeof(filenames));

    dim0 = BIG_X_FACTOR;
    dim1 = BIG_Y_FACTOR;
    dim2 = BIG_Z_FACTOR;

    if (MAINPROCESS) {
        printf("===================================\n");
        printf("2 GByte IO TESTS START\n");
        printf("2 MPI ranks will run the tests...\n");
        printf("===================================\n");
    }

    h5_show_hostname();

    if (H5dont_atexit() < 0) {
        printf("Failed to turn off atexit processing. Continue.\n");
    };
    H5open();

    memset(filenames, 0, sizeof(filenames));
    for (int i = 0; i < NFILENAME; i++) {
        if (NULL == (filenames[i] = malloc(PATH_MAX))) {
            printf("couldn't allocate filename array\n");
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }

    /* Set the internal transition size to allow use of derived datatypes
     * without having to actually read or write large datasets (>2GB).
     */
    oldsize = H5_mpi_set_bigio_count(newsize);

    if (mpi_size > 2) {
        int rank_color = 0;
        if (mpi_rank >= 2)
            rank_color = 1;
        if (MPI_Comm_split(test_comm, rank_color, mpi_rank, &test_comm) != MPI_SUCCESS) {
            printf("MPI returned an error. Exiting\n");
        }
    }

    /* Initialize testing framework */
    if (mpi_rank < 2) {
        if (TestInit(argv[0], usage, parse_options, NULL, NULL, mpi_rank) < 0) {
            fprintf(stderr, "couldn't initialize testing framework\n");
            MPI_Abort(MPI_COMM_WORLD, -1);
        }

        test_params.filename = PARATESTFILE;

        AddTest("idsetw", dataset_writeInd, NULL, NULL, &test_params, sizeof(test_params),
                "dataset independent write");

        AddTest("idsetr", dataset_readInd, NULL, NULL, &test_params, sizeof(test_params),
                "dataset independent read");

        AddTest("cdsetw", dataset_writeAll, NULL, NULL, &test_params, sizeof(test_params),
                "dataset collective write");

        AddTest("cdsetr", dataset_readAll, NULL, NULL, &test_params, sizeof(test_params),
                "dataset collective read");

        AddTest("eidsetw2", extend_writeInd2, NULL, NULL, &test_params, sizeof(test_params),
                "extendible dataset independent write #2");

        AddTest("selnone", none_selection_chunk, NULL, NULL, &test_params, sizeof(test_params),
                "chunked dataset with none-selection");

#ifdef H5_HAVE_FILTER_DEFLATE
        AddTest("cmpdsetr", compress_readAll, NULL, NULL, &test_params, sizeof(test_params),
                "compressed dataset collective read");
#endif /* H5_HAVE_FILTER_DEFLATE */

        /* Display testing information */
        TestInfo(stdout);

        /* Parse command line arguments */
        if (TestParseCmdLine(argc, argv) < 0) {
            fprintf(stderr, "couldn't parse command-line arguments\n");
            TestShutdown();
            MPI_Abort(MPI_COMM_WORLD, -1);
        }

        /* setup file access property list */
        fapl = H5Pcreate(H5P_FILE_ACCESS);
        H5Pset_fapl_mpio(fapl, test_comm, MPI_INFO_NULL);

        /* Perform requested testing */
        if (PerformTests() < 0) {
            fprintf(stderr, "couldn't run tests\n");
            TestShutdown();
            MPI_Abort(MPI_COMM_WORLD, -1);
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* Restore the default bigio setting */
    H5_mpi_set_bigio_count(oldsize);

    express_test = GetTestExpress();
    if ((express_test == H5_TEST_EXPRESS_EXHAUSTIVE) && (mpi_rank < 2)) {
        MpioTest2G(test_comm);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (mpi_rank == 0)
        HDremove(FILENAME[0]);

    for (int i = 0; i < NFILENAME; i++) {
        free(filenames[i]);
        filenames[i] = NULL;
    }

    if (TestShutdown() < 0) {
        if (MAINPROCESS)
            fprintf(stderr, "couldn't shut down testing framework\n");
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    H5close();
    if (test_comm != MPI_COMM_WORLD) {
        MPI_Comm_free(&test_comm);
    }
    MPI_Finalize();
    return 0;
}
