/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
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
#include "testphdf5.h"

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

#ifndef PATH_MAX
#define PATH_MAX 512
#endif /* !PATH_MAX */

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
usage(void)
{
    printf("    [-r] [-w] [-m<n_datasets>] [-n<n_groups>] "
           "[-o] [-f <prefix>] [-d <dim0> <dim1>]\n");
    printf("\t-m<n_datasets>"
           "\tset number of datasets for the multiple dataset test\n");
    printf("\t-n<n_groups>"
           "\tset number of groups for the multiple group test\n");
    printf("\t-f <prefix>\tfilename prefix\n");
    printf("\t-2\t\tuse Split-file together with MPIO\n");
    printf("\t-d <factor0> <factor1>\tdataset dimensions factors. Defaults (%d,%d)\n", BIG_X_FACTOR,
           BIG_Y_FACTOR);
    printf("\t-c <dim0> <dim1>\tdataset chunk dimensions. Defaults (dim0/10,dim1/10)\n");
    printf("\n");
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
 * Create the appropriate File access property list
 */
hid_t
create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type)
{
    hid_t  ret_pl = H5I_INVALID_HID;
    herr_t ret;      /* generic return value */
    int    mpi_rank; /* mpi variables */

    /* need the rank for error checking macros */
    MPI_Comm_rank(test_comm, &mpi_rank);

    ret_pl = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((ret_pl >= 0), "H5P_FILE_ACCESS");

    if (l_facc_type == FACC_DEFAULT)
        return (ret_pl);

    if (l_facc_type == FACC_MPIO) {
        /* set Parallel access with communicator */
        ret = H5Pset_fapl_mpio(ret_pl, comm, info);
        VRFY((ret >= 0), "");
        ret = H5Pset_all_coll_metadata_ops(ret_pl, true);
        VRFY((ret >= 0), "");
        ret = H5Pset_coll_metadata_write(ret_pl, true);
        VRFY((ret >= 0), "");
        return (ret_pl);
    }

    if (l_facc_type == (FACC_MPIO | FACC_SPLIT)) {
        hid_t mpio_pl;

        mpio_pl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((mpio_pl >= 0), "");
        /* set Parallel access with communicator */
        ret = H5Pset_fapl_mpio(mpio_pl, comm, info);
        VRFY((ret >= 0), "");

        /* setup file access template */
        ret_pl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((ret_pl >= 0), "");
        /* set Parallel access with communicator */
        ret = H5Pset_fapl_split(ret_pl, ".meta", mpio_pl, ".raw", mpio_pl);
        VRFY((ret >= 0), "H5Pset_fapl_split succeeded");
        H5Pclose(mpio_pl);
        return (ret_pl);
    }

    /* unknown file access types */
    return (ret_pl);
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
 * Setup the coordinates for point selection.
 */
void
point_set(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], size_t num_points,
          hsize_t coords[], int order)
{
    hsize_t i, j, k = 0, m, n, s1, s2;

    // HDcompile_assert(MAX_RANK == 3);
    HDcompile_assert(MAX_RANK == 2);

    if (OUT_OF_ORDER == order)
        k = (num_points * MAX_RANK) - 1;
    else if (IN_ORDER == order)
        k = 0;

    s1 = start[0];
    s2 = start[1];

    for (i = 0; i < count[0]; i++)
        for (j = 0; j < count[1]; j++)
            for (m = 0; m < block[0]; m++)
                for (n = 0; n < block[1]; n++)
                    if (OUT_OF_ORDER == order) {
                        coords[k--] = s2 + (stride[1] * j) + n;
                        coords[k--] = s1 + (stride[0] * i) + m;
                    }
                    else if (IN_ORDER == order) {
                        coords[k++] = s1 + stride[0] * i + m;
                        coords[k++] = s2 + stride[1] * j + n;
                    }

    if (VERBOSE_MED) {
        printf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total "
               "datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0],
               (unsigned long)count[1], (unsigned long)stride[0], (unsigned long)stride[1],
               (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));
        k = 0;
        for (i = 0; i < num_points; i++) {
            printf("(%d, %d)\n", (int)coords[k], (int)coords[k + 1]);
            k += 2;
        }
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
int
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

void
dataset_writeInd(void)
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

    filename = GetTestParameters();
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
void
dataset_readInd(void)
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

    filename = GetTestParameters();
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

void
dataset_writeAll(void)
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

    filename = GetTestParameters();
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
    file_dataspace = H5Dget_space(dataset6);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    start[0] = 0;
    start[1] = 0;
    point_set(start, count, stride, block, num_points, coords, IN_ORDER);
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

void
dataset_readAll(void)
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

    filename = GetTestParameters();
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
    file_dataspace = H5Dget_space(dataset6);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    start[0] = 0;
    start[1] = 0;
    point_set(start, count, stride, block, num_points, coords, OUT_OF_ORDER);
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
 * Part 2--Independent read/write for extendible datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two extendible
 * datasets in one HDF5 file with independent parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset.
 */

void
extend_writeInd(void)
{
    hid_t       fid;                /* HDF5 file ID */
    hid_t       acc_tpl;            /* File access templates */
    hid_t       sid;                /* Dataspace ID */
    hid_t       file_dataspace;     /* File dataspace ID */
    hid_t       mem_dataspace;      /* memory dataspace ID */
    hid_t       dataset1, dataset2; /* Dataset ID */
    const char *filename;
    hsize_t     dims[MAX_RANK];                                      /* dataset dim sizes */
    hsize_t     max_dims[MAX_RANK] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dataset maximum dim sizes */
    DATATYPE   *data_array1        = NULL;                           /* data buffer */
    hsize_t     chunk_dims[MAX_RANK];                                /* chunk sizes */
    hid_t       dataset_pl;                                          /* dataset create prop. list */

    hsize_t start[MAX_RANK];  /* for hyperslab setting */
    hsize_t count[MAX_RANK];  /* for hyperslab setting */
    hsize_t stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];  /* for hyperslab setting */

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if (VERBOSE_MED)
        printf("Extend independent write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* setup chunk-size. Make sure sizes are > 0 */
    chunk_dims[0] = (hsize_t)chunkdim0;
    chunk_dims[1] = (hsize_t)chunkdim1;

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type);
    VRFY((acc_tpl >= 0), "");

    /* Reduce the number of metadata cache slots, so that there are cache
     * collisions during the raw data I/O on the chunked dataset.  This stresses
     * the metadata cache and tests for cache bugs. -QAK
     */
    {
        int    mdc_nelmts;
        size_t rdcc_nelmts;
        size_t rdcc_nbytes;
        double rdcc_w0;

        ret = H5Pget_cache(acc_tpl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0);
        VRFY((ret >= 0), "H5Pget_cache succeeded");
        mdc_nelmts = 4;
        ret        = H5Pset_cache(acc_tpl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0);
        VRFY((ret >= 0), "H5Pset_cache succeeded");
    }

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
    /* start out with no rows, extend it later. */
    dims[0] = dims[1] = 0;
    sid               = H5Screate_simple(MAX_RANK, dims, max_dims);
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
     * Test writing to dataset1
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Extend its current dim sizes before writing */
    dims[0] = (hsize_t)dim0;
    dims[1] = (hsize_t)dim1;
    ret     = H5Dset_extent(dataset1, dims);
    VRFY((ret >= 0), "H5Dset_extent succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* release resource */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);

    /* -------------------------
     * Test writing to dataset2
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Try write to dataset2 beyond its current dim sizes.  Should fail. */
    /* Temporary turn off auto error reporting */
    H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset2);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently.  Should fail. */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret < 0), "H5Dwrite failed as expected");

    /* restore auto error reporting */
    H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data);
    H5Sclose(file_dataspace);

    /* Extend dataset2 and try again.  Should succeed. */
    dims[0] = (hsize_t)dim0;
    dims[1] = (hsize_t)dim1;
    ret     = H5Dset_extent(dataset2, dims);
    VRFY((ret >= 0), "H5Dset_extent succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset2);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* release resource */
    ret = H5Sclose(file_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Sclose(mem_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (data_array1)
        free(data_array1);
}

/*
 * Example of using the parallel HDF5 library to create an extendable dataset
 * and perform I/O on it in a way that verifies that the chunk cache is
 * bypassed for parallel I/O.
 */

void
extend_writeInd2(void)
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

    filename = GetTestParameters();
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

/* Example of using the parallel HDF5 library to read an extendible dataset */
void
extend_readInd(void)
{
    hid_t       fid;                 /* HDF5 file ID */
    hid_t       acc_tpl;             /* File access templates */
    hid_t       file_dataspace;      /* File dataspace ID */
    hid_t       mem_dataspace;       /* memory dataspace ID */
    hid_t       dataset1, dataset2;  /* Dataset ID */
    hsize_t     dims[MAX_RANK];      /* dataset dim sizes */
    DATATYPE   *data_array1  = NULL; /* data buffer */
    DATATYPE   *data_array2  = NULL; /* data buffer */
    DATATYPE   *data_origin1 = NULL; /* expected data buffer */
    const char *filename;

    hsize_t start[MAX_RANK];                   /* for hyperslab setting */
    hsize_t count[MAX_RANK], stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];                   /* for hyperslab setting */

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if (VERBOSE_MED)
        printf("Extend independent read test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");
    data_array2 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array2 != NULL), "data_array2 malloc succeeded");
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

    /* Try extend dataset1 which is open RDONLY.  Should fail. */
    /* first turn off auto error reporting */
    H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sget_simple_extent_dims(file_dataspace, dims, NULL);
    VRFY((ret > 0), "H5Sget_simple_extent_dims succeeded");
    dims[0]++;
    ret = H5Dset_extent(dataset1, dims);
    VRFY((ret < 0), "H5Dset_extent failed as expected");

    /* restore auto error reporting */
    H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data);
    H5Sclose(file_dataspace);

    /* Read dataset1 using BYROW pattern */
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
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    VRFY((ret == 0), "dataset1 read verified correct");
    if (ret)
        nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);

    /* Read dataset2 using BYCOL pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset2);
    VRFY((file_dataspace >= 0), "");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    VRFY((ret == 0), "dataset2 read verified correct");
    if (ret)
        nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (data_array1)
        free(data_array1);
    if (data_array2)
        free(data_array2);
    if (data_origin1)
        free(data_origin1);
}

/*
 * Part 3--Collective read/write for extendible datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two extendible
 * datasets in one HDF5 file with collective parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset.
 */

void
extend_writeAll(void)
{
    hid_t       fid;                /* HDF5 file ID */
    hid_t       acc_tpl;            /* File access templates */
    hid_t       xfer_plist;         /* Dataset transfer properties list */
    hid_t       sid;                /* Dataspace ID */
    hid_t       file_dataspace;     /* File dataspace ID */
    hid_t       mem_dataspace;      /* memory dataspace ID */
    hid_t       dataset1, dataset2; /* Dataset ID */
    const char *filename;
    hsize_t     dims[MAX_RANK];                                      /* dataset dim sizes */
    hsize_t     max_dims[MAX_RANK] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dataset maximum dim sizes */
    DATATYPE   *data_array1        = NULL;                           /* data buffer */
    hsize_t     chunk_dims[MAX_RANK];                                /* chunk sizes */
    hid_t       dataset_pl;                                          /* dataset create prop. list */

    hsize_t start[MAX_RANK];  /* for hyperslab setting */
    hsize_t count[MAX_RANK];  /* for hyperslab setting */
    hsize_t stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];  /* for hyperslab setting */

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if (VERBOSE_MED)
        printf("Extend independent write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* setup chunk-size. Make sure sizes are > 0 */
    chunk_dims[0] = (hsize_t)chunkdim0;
    chunk_dims[1] = (hsize_t)chunkdim1;

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type);
    VRFY((acc_tpl >= 0), "");

    /* Reduce the number of metadata cache slots, so that there are cache
     * collisions during the raw data I/O on the chunked dataset.  This stresses
     * the metadata cache and tests for cache bugs. -QAK
     */
    {
        int    mdc_nelmts;
        size_t rdcc_nelmts;
        size_t rdcc_nbytes;
        double rdcc_w0;

        ret = H5Pget_cache(acc_tpl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0);
        VRFY((ret >= 0), "H5Pget_cache succeeded");
        mdc_nelmts = 4;
        ret        = H5Pset_cache(acc_tpl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0);
        VRFY((ret >= 0), "H5Pset_cache succeeded");
    }

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
    /* start out with no rows, extend it later. */
    dims[0] = dims[1] = 0;
    sid               = H5Screate_simple(MAX_RANK, dims, max_dims);
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
     * Test writing to dataset1
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Extend its current dim sizes before writing */
    dims[0] = (hsize_t)dim0;
    dims[1] = (hsize_t)dim1;
    ret     = H5Dset_extent(dataset1, dims);
    VRFY((ret >= 0), "H5Dset_extent succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

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
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* release resource */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* -------------------------
     * Test writing to dataset2
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if (VERBOSE_MED) {
        MESG("data_array created");
        dataset_print(start, block, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if (dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret >= 0), "set independent IO collectively succeeded");
    }

    /* Try write to dataset2 beyond its current dim sizes.  Should fail. */
    /* Temporary turn off auto error reporting */
    H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset2);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently.  Should fail. */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret < 0), "H5Dwrite failed as expected");

    /* restore auto error reporting */
    H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data);
    H5Sclose(file_dataspace);

    /* Extend dataset2 and try again.  Should succeed. */
    dims[0] = (hsize_t)dim0;
    dims[1] = (hsize_t)dim1;
    ret     = H5Dset_extent(dataset2, dims);
    VRFY((ret >= 0), "H5Dset_extent succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset2);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite succeeded");

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
    if (data_array1)
        free(data_array1);
}

/* Example of using the parallel HDF5 library to read an extendible dataset */
void
extend_readAll(void)
{
    hid_t       fid;                /* HDF5 file ID */
    hid_t       acc_tpl;            /* File access templates */
    hid_t       xfer_plist;         /* Dataset transfer properties list */
    hid_t       file_dataspace;     /* File dataspace ID */
    hid_t       mem_dataspace;      /* memory dataspace ID */
    hid_t       dataset1, dataset2; /* Dataset ID */
    const char *filename;
    hsize_t     dims[MAX_RANK];      /* dataset dim sizes */
    DATATYPE   *data_array1  = NULL; /* data buffer */
    DATATYPE   *data_array2  = NULL; /* data buffer */
    DATATYPE   *data_origin1 = NULL; /* expected data buffer */

    hsize_t start[MAX_RANK];                   /* for hyperslab setting */
    hsize_t count[MAX_RANK], stride[MAX_RANK]; /* for hyperslab setting */
    hsize_t block[MAX_RANK];                   /* for hyperslab setting */

    herr_t ret; /* Generic return value */
    int    mpi_size, mpi_rank;

    MPI_Comm comm = test_comm;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if (VERBOSE_MED)
        printf("Extend independent read test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");
    data_array2 = (DATATYPE *)malloc((size_t)dim0 * (size_t)dim1 * sizeof(DATATYPE));
    VRFY((data_array2 != NULL), "data_array2 malloc succeeded");
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

    /* Try extend dataset1 which is open RDONLY.  Should fail. */
    /* first turn off auto error reporting */
    H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    file_dataspace = H5Dget_space(dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sget_simple_extent_dims(file_dataspace, dims, NULL);
    VRFY((ret > 0), "H5Sget_simple_extent_dims succeeded");
    dims[0]++;
    ret = H5Dset_extent(dataset1, dims);
    VRFY((ret < 0), "H5Dset_extent failed as expected");

    /* restore auto error reporting */
    H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data);
    H5Sclose(file_dataspace);

    /* Read dataset1 using BYROW pattern */
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

    /* read data collectively */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    VRFY((ret == 0), "dataset1 read verified correct");
    if (ret)
        nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);
    H5Pclose(xfer_plist);

    /* Read dataset2 using BYCOL pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset2);
    VRFY((file_dataspace >= 0), "");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(MAX_RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
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

    /* read data collectively */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace, xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    VRFY((ret == 0), "dataset2 read verified correct");
    if (ret)
        nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);
    H5Pclose(xfer_plist);

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (data_array1)
        free(data_array1);
    if (data_array2)
        free(data_array2);
    if (data_origin1)
        free(data_origin1);
}

/*
 * Example of using the parallel HDF5 library to read a compressed
 * dataset in an HDF5 file with collective parallel access support.
 */
#ifdef H5_HAVE_FILTER_DEFLATE
void
compress_readAll(void)
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

    filename = GetTestParameters();
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

void
none_selection_chunk(void)
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

    filename = GetTestParameters();
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

/* Function: test_actual_io_mode
 *
 * Purpose: tests one specific case of collective I/O and checks that the
 *          actual_chunk_opt_mode property and the actual_io_mode
 *          properties in the DXPL have the correct values.
 *
 * Input:   selection_mode: changes the way processes select data from the space, as well
 *          as some dxpl flags to get collective I/O to break in different ways.
 *
 *          The relevant I/O function and expected response for each mode:
 *              TEST_ACTUAL_IO_MULTI_CHUNK_IND:
 *                  H5D_mpi_chunk_collective_io, each process reports independent I/O
 *
 *              TEST_ACTUAL_IO_MULTI_CHUNK_COL:
 *                  H5D_mpi_chunk_collective_io, each process reports collective I/O
 *
 *              TEST_ACTUAL_IO_MULTI_CHUNK_MIX:
 *                  H5D_mpi_chunk_collective_io, each process reports mixed I/O
 *
 *              TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE:
 *                  H5D_mpi_chunk_collective_io, processes disagree. The root reports
 *                  collective, the rest report independent I/O
 *
 *              TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_IND:
 *                  Same test TEST_ACTUAL_IO_MULTI_CHUNK_IND.
 *                  Set directly go to multi-chunk-io without num threshold calc.
 *              TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_COL:
 *                  Same test TEST_ACTUAL_IO_MULTI_CHUNK_COL.
 *                  Set directly go to multi-chunk-io without num threshold calc.
 *
 *              TEST_ACTUAL_IO_LINK_CHUNK:
 *                  H5D_link_chunk_collective_io, processes report linked chunk I/O
 *
 *              TEST_ACTUAL_IO_CONTIGUOUS:
 *                  H5D__contig_collective_write or H5D__contig_collective_read
 *                  each process reports contiguous collective I/O
 *
 *              TEST_ACTUAL_IO_NO_COLLECTIVE:
 *                  Simple independent I/O. This tests that the defaults are properly set.
 *
 *              TEST_ACTUAL_IO_RESET:
 *                  Performs collective and then independent I/O with the same dxpl to
 *                  make sure the property is correctly reset to the default on each use.
 *                  Specifically, this test runs TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_MIX_DISAGREE
 *                  (The most complex case that works on all builds) and then performs
 *                  an independent read and write with the same dxpls.
 *
 *          Note: DIRECT_MULTI_CHUNK_MIX and DIRECT_MULTI_CHUNK_MIX_DISAGREE
 *          is not needed as they are covered by DIRECT_CHUNK_MIX and
 *          MULTI_CHUNK_MIX_DISAGREE cases. _DIRECT_ cases are only for testing
 *          path way to multi-chunk-io by H5FD_MPIO_CHUNK_MULTI_IO instead of num-threshold.
 */
static void
test_actual_io_mode(int selection_mode)
{
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode_write    = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode_read     = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode_expected = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
    H5D_mpio_actual_io_mode_t        actual_io_mode_write           = H5D_MPIO_NO_COLLECTIVE;
    H5D_mpio_actual_io_mode_t        actual_io_mode_read            = H5D_MPIO_NO_COLLECTIVE;
    H5D_mpio_actual_io_mode_t        actual_io_mode_expected        = H5D_MPIO_NO_COLLECTIVE;
    const char                      *filename;
    const char                      *test_name;
    bool                             direct_multi_chunk_io;
    bool                             multi_chunk_io;
    bool                             is_chunked;
    bool                             is_collective;
    int                              mpi_size = -1;
    int                              mpi_rank = -1;
    int                              length;
    int                             *buffer;
    int                              i;
    MPI_Comm                         mpi_comm   = MPI_COMM_NULL;
    MPI_Info                         mpi_info   = MPI_INFO_NULL;
    hid_t                            fid        = H5I_INVALID_HID;
    hid_t                            sid        = H5I_INVALID_HID;
    hid_t                            dataset    = H5I_INVALID_HID;
    hid_t                            data_type  = H5T_NATIVE_INT;
    hid_t                            fapl_id    = H5I_INVALID_HID;
    hid_t                            mem_space  = H5I_INVALID_HID;
    hid_t                            file_space = H5I_INVALID_HID;
    hid_t                            dcpl       = H5I_INVALID_HID;
    hid_t                            dxpl_write = H5I_INVALID_HID;
    hid_t                            dxpl_read  = H5I_INVALID_HID;
    hsize_t                          dims[MAX_RANK];
    hsize_t                          chunk_dims[MAX_RANK];
    hsize_t                          start[MAX_RANK];
    hsize_t                          stride[MAX_RANK];
    hsize_t                          count[MAX_RANK];
    hsize_t                          block[MAX_RANK];
    char                             message[256];
    herr_t                           ret;

    /* Set up some flags to make some future if statements slightly more readable */
    direct_multi_chunk_io = (selection_mode == TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_IND ||
                             selection_mode == TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_COL);

    /* Note: RESET performs the same tests as MULTI_CHUNK_MIX_DISAGREE and then
     * tests independent I/O
     */
    multi_chunk_io =
        (selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_IND ||
         selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_COL ||
         selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_MIX ||
         selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE || selection_mode == TEST_ACTUAL_IO_RESET);

    is_chunked =
        (selection_mode != TEST_ACTUAL_IO_CONTIGUOUS && selection_mode != TEST_ACTUAL_IO_NO_COLLECTIVE);

    is_collective = selection_mode != TEST_ACTUAL_IO_NO_COLLECTIVE;

    /* Set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    MPI_Barrier(test_comm);

    assert(mpi_size >= 1);

    mpi_comm = test_comm;
    mpi_info = MPI_INFO_NULL;

    filename = (const char *)GetTestParameters();
    assert(filename != NULL);

    /* Setup the file access template */
    fapl_id = create_faccess_plist(mpi_comm, mpi_info, facc_type);
    VRFY((fapl_id >= 0), "create_faccess_plist() succeeded");

    /* Create the file */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Create the basic Space */
    dims[0] = (hsize_t)dim0;
    dims[1] = (hsize_t)dim1;
    sid     = H5Screate_simple(MAX_RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    /* Create the dataset creation plist */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl >= 0), "dataset creation plist created successfully");

    /* If we are not testing contiguous datasets */
    if (is_chunked) {
        /* Set up chunk information.  */
        chunk_dims[0] = dims[0] / (hsize_t)mpi_size;
        chunk_dims[1] = dims[1];
        ret           = H5Pset_chunk(dcpl, 2, chunk_dims);
        VRFY((ret >= 0), "chunk creation property list succeeded");
    }

    /* Create the dataset */
    dataset = H5Dcreate2(fid, "actual_io", data_type, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2() dataset succeeded");

    /* Create the file dataspace */
    file_space = H5Dget_space(dataset);
    VRFY((file_space >= 0), "H5Dget_space succeeded");

    /* Choose a selection method based on the type of I/O we want to occur,
     * and also set up some selection-dependeent test info. */
    switch (selection_mode) {

        /* Independent I/O with optimization */
        case TEST_ACTUAL_IO_MULTI_CHUNK_IND:
        case TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_IND:
            /* Since the dataset is chunked by row and each process selects a row,
             * each process  writes to a different chunk. This forces all I/O to be
             * independent.
             */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

            test_name                      = "Multi Chunk - Independent";
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK;
            actual_io_mode_expected        = H5D_MPIO_CHUNK_INDEPENDENT;
            break;

        /* Collective I/O with optimization */
        case TEST_ACTUAL_IO_MULTI_CHUNK_COL:
        case TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_COL:
            /* The dataset is chunked by rows, so each process takes a column which
             * spans all chunks. Since the processes write non-overlapping regular
             * selections to each chunk, the operation is purely collective.
             */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

            test_name                      = "Multi Chunk - Collective";
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK;
            if (mpi_size > 1)
                actual_io_mode_expected = H5D_MPIO_CHUNK_COLLECTIVE;
            else
                actual_io_mode_expected = H5D_MPIO_CHUNK_INDEPENDENT;
            break;

        /* Mixed I/O with optimization */
        case TEST_ACTUAL_IO_MULTI_CHUNK_MIX:
            /* A chunk will be assigned collective I/O only if it is selected by each
             * process. To get mixed I/O, have the root select all chunks and each
             * subsequent process select the first and nth chunk. The first chunk,
             * accessed by all, will be assigned collective I/O while each other chunk
             * will be accessed only by the root and the nth process and will be
             * assigned independent I/O. Each process will access one chunk collectively
             * and at least one chunk independently, reporting mixed I/O.
             */

            if (mpi_rank == 0) {
                /* Select the first column */
                slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);
            }
            else {
                /* Select the first and the nth chunk in the nth column */
                block[0]  = (hsize_t)(dim0 / mpi_size);
                block[1]  = (hsize_t)(dim1 / mpi_size);
                count[0]  = 2;
                count[1]  = 1;
                stride[0] = (hsize_t)mpi_rank * block[0];
                stride[1] = 1;
                start[0]  = 0;
                start[1]  = (hsize_t)mpi_rank * block[1];
            }

            test_name                      = "Multi Chunk - Mixed";
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK;
            actual_io_mode_expected        = H5D_MPIO_CHUNK_MIXED;
            break;

        /* RESET tests that the properties are properly reset to defaults each time I/O is
         * performed. To achieve this, we have RESET perform collective I/O (which would change
         * the values from the defaults) followed by independent I/O (which should report the
         * default values). RESET doesn't need to have a unique selection, so we reuse
         * MULTI_CHUMK_MIX_DISAGREE, which was chosen because it is a complex case that works
         * on all builds. The independent section of RESET can be found at the end of this function.
         */
        case TEST_ACTUAL_IO_RESET:

        /* Mixed I/O with optimization and internal disagreement */
        case TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE:
            /* A chunk will be assigned collective I/O only if it is selected by each
             * process. To get mixed I/O with disagreement, assign process n to the
             * first chunk and the nth chunk. The first chunk, selected by all, is
             * assgigned collective I/O, while each other process gets independent I/O.
             * Since the root process with only access the first chunk, it will report
             * collective I/O. The subsequent processes will access the first chunk
             * collectively, and their other chunk independently, reporting mixed I/O.
             */

            if (mpi_rank == 0) {
                /* Select the first chunk in the first column */
                slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);
                block[0] = block[0] / (hsize_t)mpi_size;
            }
            else {
                /* Select the first and the nth chunk in the nth column */
                block[0]  = (hsize_t)(dim0 / mpi_size);
                block[1]  = (hsize_t)(dim1 / mpi_size);
                count[0]  = 2;
                count[1]  = 1;
                stride[0] = (hsize_t)mpi_rank * block[0];
                stride[1] = 1;
                start[0]  = 0;
                start[1]  = (hsize_t)mpi_rank * block[1];
            }

            /* If the testname was not already set by the RESET case */
            if (selection_mode == TEST_ACTUAL_IO_RESET)
                test_name = "RESET";
            else
                test_name = "Multi Chunk - Mixed (Disagreement)";

            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK;
            if (mpi_size > 1) {
                if (mpi_rank == 0)
                    actual_io_mode_expected = H5D_MPIO_CHUNK_COLLECTIVE;
                else
                    actual_io_mode_expected = H5D_MPIO_CHUNK_MIXED;
            }
            else
                actual_io_mode_expected = H5D_MPIO_CHUNK_INDEPENDENT;

            break;

        /* Linked Chunk I/O */
        case TEST_ACTUAL_IO_LINK_CHUNK:
            /* Nothing special; link chunk I/O is forced in the dxpl settings. */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

            test_name                      = "Link Chunk";
            actual_chunk_opt_mode_expected = H5D_MPIO_LINK_CHUNK;
            actual_io_mode_expected        = H5D_MPIO_CHUNK_COLLECTIVE;
            break;

        /* Contiguous Dataset */
        case TEST_ACTUAL_IO_CONTIGUOUS:
            /* A non overlapping, regular selection in a contiguous dataset leads to
             * collective I/O */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

            test_name                      = "Contiguous";
            actual_chunk_opt_mode_expected = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
            actual_io_mode_expected        = H5D_MPIO_CONTIGUOUS_COLLECTIVE;
            break;

        case TEST_ACTUAL_IO_NO_COLLECTIVE:
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

            test_name                      = "Independent";
            actual_chunk_opt_mode_expected = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
            actual_io_mode_expected        = H5D_MPIO_NO_COLLECTIVE;
            break;

        default:
            test_name                      = "Undefined Selection Mode";
            actual_chunk_opt_mode_expected = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
            actual_io_mode_expected        = H5D_MPIO_NO_COLLECTIVE;
            break;
    }

    ret = H5Sselect_hyperslab(file_space, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* Create a memory dataspace mirroring the dataset and select the same hyperslab
     * as in the file space.
     */
    mem_space = H5Screate_simple(MAX_RANK, dims, NULL);
    VRFY((mem_space >= 0), "mem_space created");

    ret = H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* Get the number of elements in the selection */
    length = dim0 * dim1;

    /* Allocate and initialize the buffer */
    buffer = (int *)malloc(sizeof(int) * (size_t)length);
    VRFY((buffer != NULL), "malloc of buffer succeeded");
    for (i = 0; i < length; i++)
        buffer[i] = i;

    /* Set up the dxpl for the write */
    dxpl_write = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_write >= 0), "H5Pcreate(H5P_DATASET_XFER) succeeded");

    /* Set collective I/O properties in the dxpl. */
    if (is_collective) {
        /* Request collective I/O */
        ret = H5Pset_dxpl_mpio(dxpl_write, H5FD_MPIO_COLLECTIVE);
        VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

        /* Set the threshold number of processes per chunk to twice mpi_size.
         * This will prevent the threshold from ever being met, thus forcing
         * multi chunk io instead of link chunk io.
         * This is via default.
         */
        if (multi_chunk_io) {
            /* force multi-chunk-io by threshold */
            ret = H5Pset_dxpl_mpio_chunk_opt_num(dxpl_write, (unsigned)mpi_size * 2);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio_chunk_opt_num succeeded");

            /* set this to manipulate testing scenario about allocating processes
             * to chunks */
            ret = H5Pset_dxpl_mpio_chunk_opt_ratio(dxpl_write, (unsigned)99);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio_chunk_opt_ratio succeeded");
        }

        /* Set directly go to multi-chunk-io without threshold calc. */
        if (direct_multi_chunk_io) {
            /* set for multi chunk io by property*/
            ret = H5Pset_dxpl_mpio_chunk_opt(dxpl_write, H5FD_MPIO_CHUNK_MULTI_IO);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
        }
    }

    /* Make a copy of the dxpl to test the read operation */
    dxpl_read = H5Pcopy(dxpl_write);
    VRFY((dxpl_read >= 0), "H5Pcopy succeeded");

    /* Write */
    ret = H5Dwrite(dataset, data_type, mem_space, file_space, dxpl_write, buffer);
    if (ret < 0)
        H5Eprint2(H5E_DEFAULT, stdout);
    VRFY((ret >= 0), "H5Dwrite() dataset multichunk write succeeded");

    /* Retrieve Actual io values */
    ret = H5Pget_mpio_actual_io_mode(dxpl_write, &actual_io_mode_write);
    VRFY((ret >= 0), "retrieving actual io mode succeeded");

    ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_write, &actual_chunk_opt_mode_write);
    VRFY((ret >= 0), "retrieving actual chunk opt mode succeeded");

    /* Read */
    ret = H5Dread(dataset, data_type, mem_space, file_space, dxpl_read, buffer);
    if (ret < 0)
        H5Eprint2(H5E_DEFAULT, stdout);
    VRFY((ret >= 0), "H5Dread() dataset multichunk read succeeded");

    /* Retrieve Actual io values */
    ret = H5Pget_mpio_actual_io_mode(dxpl_read, &actual_io_mode_read);
    VRFY((ret >= 0), "retrieving actual io mode succeeded");

    ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_read, &actual_chunk_opt_mode_read);
    VRFY((ret >= 0), "retrieving actual chunk opt mode succeeded");

    /* Check write vs read */
    VRFY((actual_io_mode_read == actual_io_mode_write),
         "reading and writing are the same for actual_io_mode");
    VRFY((actual_chunk_opt_mode_read == actual_chunk_opt_mode_write),
         "reading and writing are the same for actual_chunk_opt_mode");

    /* Test values */
    if (actual_chunk_opt_mode_expected != (H5D_mpio_actual_chunk_opt_mode_t)-1 &&
        actual_io_mode_expected != (H5D_mpio_actual_io_mode_t)-1) {
        snprintf(message, sizeof(message), "Actual Chunk Opt Mode has the correct value for %s.\n",
                 test_name);
        VRFY((actual_chunk_opt_mode_write == actual_chunk_opt_mode_expected), message);
        snprintf(message, sizeof(message), "Actual IO Mode has the correct value for %s.\n", test_name);
        VRFY((actual_io_mode_write == actual_io_mode_expected), message);
    }
    else {
        fprintf(stderr, "%s %d -> (%d,%d)\n", test_name, mpi_rank, actual_chunk_opt_mode_write,
                actual_io_mode_write);
    }

    /* To test that the property is successfully reset to the default, we perform some
     * independent I/O after the collective I/O
     */
    if (selection_mode == TEST_ACTUAL_IO_RESET) {
        if (mpi_rank == 0) {
            /* Switch to independent io */
            ret = H5Pset_dxpl_mpio(dxpl_write, H5FD_MPIO_INDEPENDENT);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
            ret = H5Pset_dxpl_mpio(dxpl_read, H5FD_MPIO_INDEPENDENT);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

            /* Write */
            ret = H5Dwrite(dataset, data_type, H5S_ALL, H5S_ALL, dxpl_write, buffer);
            VRFY((ret >= 0), "H5Dwrite() dataset multichunk write succeeded");

            /* Check Properties */
            ret = H5Pget_mpio_actual_io_mode(dxpl_write, &actual_io_mode_write);
            VRFY((ret >= 0), "retrieving actual io mode succeeded");
            ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_write, &actual_chunk_opt_mode_write);
            VRFY((ret >= 0), "retrieving actual chunk opt mode succeeded");

            VRFY(actual_chunk_opt_mode_write == H5D_MPIO_NO_CHUNK_OPTIMIZATION,
                 "actual_chunk_opt_mode has correct value for reset write (independent)");
            VRFY(actual_io_mode_write == H5D_MPIO_NO_COLLECTIVE,
                 "actual_io_mode has correct value for reset write (independent)");

            /* Read */
            ret = H5Dread(dataset, data_type, H5S_ALL, H5S_ALL, dxpl_read, buffer);
            VRFY((ret >= 0), "H5Dwrite() dataset multichunk write succeeded");

            /* Check Properties */
            ret = H5Pget_mpio_actual_io_mode(dxpl_read, &actual_io_mode_read);
            VRFY((ret >= 0), "retrieving actual io mode succeeded");
            ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_read, &actual_chunk_opt_mode_read);
            VRFY((ret >= 0), "retrieving actual chunk opt mode succeeded");

            VRFY(actual_chunk_opt_mode_read == H5D_MPIO_NO_CHUNK_OPTIMIZATION,
                 "actual_chunk_opt_mode has correct value for reset read (independent)");
            VRFY(actual_io_mode_read == H5D_MPIO_NO_COLLECTIVE,
                 "actual_io_mode has correct value for reset read (independent)");
        }
    }

    /* Release some resources */
    ret = H5Sclose(sid);
    ret = H5Pclose(fapl_id);
    ret = H5Pclose(dcpl);
    ret = H5Pclose(dxpl_write);
    ret = H5Pclose(dxpl_read);
    ret = H5Dclose(dataset);
    ret = H5Sclose(mem_space);
    ret = H5Sclose(file_space);
    ret = H5Fclose(fid);
    free(buffer);
    return;
}

/* Function: actual_io_mode_tests
 *
 * Purpose: Tests all possible cases of the actual_io_mode property.
 */
void
actual_io_mode_tests(void)
{
    int mpi_size = -1;
    int mpi_rank = -1;
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_size(test_comm, &mpi_rank);

    test_actual_io_mode(TEST_ACTUAL_IO_NO_COLLECTIVE);

    /*
     * Test multi-chunk-io via proc_num threshold
     */
    test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_IND);
    test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_COL);

    /* The Multi Chunk Mixed test requires at least three processes. */
    if (mpi_size > 2)
        test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_MIX);
    else
        fprintf(stdout, "Multi Chunk Mixed test requires 3 processes minimum\n");

    test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE);

    /*
     * Test multi-chunk-io via setting direct property
     */
    test_actual_io_mode(TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_IND);
    test_actual_io_mode(TEST_ACTUAL_IO_DIRECT_MULTI_CHUNK_COL);

    test_actual_io_mode(TEST_ACTUAL_IO_LINK_CHUNK);
    test_actual_io_mode(TEST_ACTUAL_IO_CONTIGUOUS);

    test_actual_io_mode(TEST_ACTUAL_IO_RESET);
    return;
}

/*
 * Function: test_no_collective_cause_mode
 *
 * Purpose:
 *    tests cases for broken collective I/O and checks that the
 *    H5Pget_mpio_no_collective_cause properties in the DXPL have the correct values.
 *
 * Input:
 *    selection_mode: various mode to cause broken collective I/O
 *    Note: Originally, each TEST case is supposed to be used alone.
 *          After some discussion, this is updated to take multiple TEST cases
 *          with '|'.  However there is no error check for any of combined
 *          test cases, so a tester is responsible to understand and feed
 *          proper combination of TESTs if needed.
 *
 *
 *       TEST_COLLECTIVE:
 *         Test for regular collective I/O without cause of breaking.
 *         Just to test normal behavior.
 *
 *       TEST_SET_INDEPENDENT:
 *         Test for Independent I/O as the cause of breaking collective I/O.
 *
 *       TEST_DATATYPE_CONVERSION:
 *         Test for Data Type Conversion as the cause of breaking collective I/O.
 *
 *       TEST_DATA_TRANSFORMS:
 *         Test for Data Transform feature as the cause of breaking collective I/O.
 *
 *       TEST_NOT_SIMPLE_OR_SCALAR_DATASPACES:
 *         Test for NULL dataspace as the cause of breaking collective I/O.
 *
 *       TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_COMPACT:
 *         Test for Compact layout as the cause of breaking collective I/O.
 *
 *       TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL:
 *         Test for Externl-File storage as the cause of breaking collective I/O.
 */
#define FILE_EXTERNAL "nocolcause_extern.data"
static void
test_no_collective_cause_mode(int selection_mode)
{
    uint32_t no_collective_cause_local_write     = 0;
    uint32_t no_collective_cause_local_read      = 0;
    uint32_t no_collective_cause_local_expected  = 0;
    uint32_t no_collective_cause_global_write    = 0;
    uint32_t no_collective_cause_global_read     = 0;
    uint32_t no_collective_cause_global_expected = 0;
    // hsize_t coord[NELM][MAX_RANK];

    uint32_t no_selection_io_cause_write    = 0;
    uint32_t no_selection_io_cause_read     = 0;
    uint32_t no_selection_io_cause_expected = 0;

    const char *filename;
    const char *test_name;
    bool        is_chunked     = 1;
    bool        is_independent = 0;
    int         mpi_size       = -1;
    int         mpi_rank       = -1;
    int         length;
    int        *buffer;
    int         i;
    MPI_Comm    mpi_comm;
    MPI_Info    mpi_info;
    hid_t       fid        = H5I_INVALID_HID;
    hid_t       sid        = H5I_INVALID_HID;
    hid_t       dataset    = H5I_INVALID_HID;
    hid_t       data_type  = H5T_NATIVE_INT;
    hid_t       fapl_id    = H5I_INVALID_HID;
    hid_t       dcpl       = H5I_INVALID_HID;
    hid_t       dxpl_write = H5I_INVALID_HID;
    hid_t       dxpl_read  = H5I_INVALID_HID;
    hsize_t     dims[MAX_RANK];
    hid_t       mem_space  = H5I_INVALID_HID;
    hid_t       file_space = H5I_INVALID_HID;
    hsize_t     chunk_dims[MAX_RANK];
    herr_t      ret;
    /* set to global value as default */
    int  l_facc_type = facc_type;
    char message[256];

    /* Set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    MPI_Barrier(test_comm);

    assert(mpi_size >= 1);

    mpi_comm = test_comm;
    mpi_info = MPI_INFO_NULL;

    /* Create the dataset creation plist */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl >= 0), "dataset creation plist created successfully");

    if (selection_mode & TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_COMPACT) {
        ret = H5Pset_layout(dcpl, H5D_COMPACT);
        VRFY((ret >= 0), "set COMPACT layout succeeded");
        is_chunked = 0;
    }

    if (selection_mode & TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL) {
        ret = H5Pset_external(dcpl, FILE_EXTERNAL, 0, H5F_UNLIMITED);
        VRFY((ret >= 0), "set EXTERNAL file layout succeeded");
        is_chunked = 0;
    }

    if (selection_mode & TEST_NOT_SIMPLE_OR_SCALAR_DATASPACES) {
        sid = H5Screate(H5S_NULL);
        VRFY((sid >= 0), "H5Screate_simple succeeded");
        is_chunked = 0;
    }
    else {
        /* Create the basic Space */
        /* if this is a compact dataset, create a small dataspace that does not exceed 64K */
        if (selection_mode & TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_COMPACT) {
            dims[0] = BIG_X_FACTOR * 6;
            dims[1] = BIG_Y_FACTOR * 6;
        }
        else {
            dims[0] = (hsize_t)dim0;
            dims[1] = (hsize_t)dim1;
        }
        sid = H5Screate_simple(MAX_RANK, dims, NULL);
        VRFY((sid >= 0), "H5Screate_simple succeeded");
    }

    filename = (const char *)GetTestParameters();
    assert(filename != NULL);

    /* Setup the file access template */
    fapl_id = create_faccess_plist(mpi_comm, mpi_info, l_facc_type);
    VRFY((fapl_id >= 0), "create_faccess_plist() succeeded");

    /* Create the file */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* If we are not testing contiguous datasets */
    if (is_chunked) {
        /* Set up chunk information.  */
        chunk_dims[0] = dims[0] / (hsize_t)mpi_size;
        chunk_dims[1] = dims[1];
        ret           = H5Pset_chunk(dcpl, 2, chunk_dims);
        VRFY((ret >= 0), "chunk creation property list succeeded");
    }

    /* Create the dataset */
    dataset = H5Dcreate2(fid, "nocolcause", data_type, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2() dataset succeeded");

    /* Set up the dxpl for the write */
    dxpl_write = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_write >= 0), "H5Pcreate(H5P_DATASET_XFER) succeeded");

    /*
     * Set expected causes and some tweaks based on the type of test
     */
    if (selection_mode & TEST_DATATYPE_CONVERSION) {
        test_name = "Broken Collective I/O - Datatype Conversion";

        /* set different sign to trigger type conversion */
        data_type = H5T_NATIVE_UINT;

        /* Disable selection I/O since datatype conversion is supported in collective with selection I/O */
        ret = H5Pset_selection_io(dxpl_write, H5D_SELECTION_IO_MODE_OFF);
        VRFY((ret >= 0), "H5Pset_selection_io succeeded");

        no_collective_cause_local_expected |= H5D_MPIO_DATATYPE_CONVERSION | H5D_MPIO_NO_SELECTION_IO;
        no_collective_cause_global_expected |= H5D_MPIO_DATATYPE_CONVERSION | H5D_MPIO_NO_SELECTION_IO;
        no_selection_io_cause_expected |= H5D_SEL_IO_DISABLE_BY_API;
    }

    if (selection_mode & TEST_DATA_TRANSFORMS) {
        test_name = "Broken Collective I/O - DATA Transforms";

        /* Set transform */
        ret = H5Pset_data_transform(dxpl_write, "x+1");
        VRFY((ret >= 0), "H5Pset_data_transform succeeded");

        /* Disable selection I/O since data transforms are supported in collective with selection I/O */
        ret = H5Pset_selection_io(dxpl_write, H5D_SELECTION_IO_MODE_OFF);
        VRFY((ret >= 0), "H5Pset_selection_io succeeded");

        no_collective_cause_local_expected |= H5D_MPIO_DATA_TRANSFORMS | H5D_MPIO_NO_SELECTION_IO;
        no_collective_cause_global_expected |= H5D_MPIO_DATA_TRANSFORMS | H5D_MPIO_NO_SELECTION_IO;
        no_selection_io_cause_expected |= H5D_SEL_IO_DISABLE_BY_API;
    }

    if (selection_mode & TEST_NOT_SIMPLE_OR_SCALAR_DATASPACES) {
        test_name = "Broken Collective I/O - No Simple or Scalar DataSpace";
        no_collective_cause_local_expected |= H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES;
        no_collective_cause_global_expected |= H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES;
        no_collective_cause_local_expected &= ~(unsigned)H5D_MPIO_NO_SELECTION_IO;
        no_collective_cause_global_expected &= ~(unsigned)H5D_MPIO_NO_SELECTION_IO;
    }

    if (selection_mode & TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_COMPACT ||
        selection_mode & TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL) {
        test_name = "Broken Collective I/O - No CONTI or CHUNKED Dataset";
        no_collective_cause_local_expected |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;
        no_collective_cause_global_expected |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;
        no_collective_cause_local_expected &= ~(unsigned)H5D_MPIO_NO_SELECTION_IO;
        no_collective_cause_global_expected &= ~(unsigned)H5D_MPIO_NO_SELECTION_IO;
    }

    if (selection_mode & TEST_COLLECTIVE) {
        test_name                           = "Broken Collective I/O - Not Broken";
        no_collective_cause_local_expected  = H5D_MPIO_COLLECTIVE;
        no_collective_cause_global_expected = H5D_MPIO_COLLECTIVE;
    }

    if (selection_mode & TEST_SET_INDEPENDENT) {
        test_name                           = "Broken Collective I/O - Independent";
        no_collective_cause_local_expected  = H5D_MPIO_SET_INDEPENDENT;
        no_collective_cause_global_expected = H5D_MPIO_SET_INDEPENDENT;
        no_collective_cause_local_expected &= ~(unsigned)H5D_MPIO_NO_SELECTION_IO;
        no_collective_cause_global_expected &= ~(unsigned)H5D_MPIO_NO_SELECTION_IO;
        /* switch to independent io */
        is_independent = 1;
    }

    /* use all spaces for certain tests */
    if (selection_mode & TEST_NOT_SIMPLE_OR_SCALAR_DATASPACES ||
        selection_mode & TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL) {
        file_space = H5S_ALL;
        mem_space  = H5S_ALL;
    }
    else {
        /* Get the file dataspace */
        file_space = H5Dget_space(dataset);
        VRFY((file_space >= 0), "H5Dget_space succeeded");

        /* Create the memory dataspace */
        mem_space = H5Screate_simple(MAX_RANK, dims, NULL);
        VRFY((mem_space >= 0), "mem_space created");
    }

    /* Get the number of elements in the selection */
    H5_CHECKED_ASSIGN(length, int, dims[0] * dims[1], hsize_t);

    /* Allocate and initialize the buffer */
    buffer = (int *)malloc(sizeof(int) * (size_t)length);
    VRFY((buffer != NULL), "malloc of buffer succeeded");
    for (i = 0; i < length; i++)
        buffer[i] = i;

    if (is_independent) {
        /* Set Independent I/O */
        ret = H5Pset_dxpl_mpio(dxpl_write, H5FD_MPIO_INDEPENDENT);
        VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    }
    else {
        /* Set Collective I/O */
        ret = H5Pset_dxpl_mpio(dxpl_write, H5FD_MPIO_COLLECTIVE);
        VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    }

    /*---------------------
     * Test Write access
     *---------------------*/

    /* Write */
    ret = H5Dwrite(dataset, data_type, mem_space, file_space, dxpl_write, buffer);
    if (ret < 0)
        H5Eprint2(H5E_DEFAULT, stdout);
    VRFY((ret >= 0), "H5Dwrite() dataset multichunk write succeeded");

    /* Get the cause of broken collective I/O */
    ret = H5Pget_mpio_no_collective_cause(dxpl_write, &no_collective_cause_local_write,
                                          &no_collective_cause_global_write);
    VRFY((ret >= 0), "retrieving no collective cause succeeded");

    ret = H5Pget_no_selection_io_cause(dxpl_write, &no_selection_io_cause_write);
    VRFY((ret >= 0), "retrieving no selection io cause succeeded");

    if (no_collective_cause_local_write & H5D_MPIO_NO_SELECTION_IO) {
        VRFY((no_selection_io_cause_write == no_selection_io_cause_expected),
             "H5D_MPIO_NO_SELECTION_IO for write is as expected");
    }

    if (no_collective_cause_global_write & H5D_MPIO_NO_SELECTION_IO) {

        VRFY((no_selection_io_cause_write == no_selection_io_cause_expected),
             "H5D_MPIO_NO_SELECTION_IO for write is as expected");
    }

    /*---------------------
     * Test Read access
     *---------------------*/

    /* Make a copy of the dxpl to test the read operation */
    dxpl_read = H5Pcopy(dxpl_write);
    VRFY((dxpl_read >= 0), "H5Pcopy succeeded");

    /* Read */
    ret = H5Dread(dataset, data_type, mem_space, file_space, dxpl_read, buffer);

    if (ret < 0)
        H5Eprint2(H5E_DEFAULT, stdout);
    VRFY((ret >= 0), "H5Dread() dataset multichunk read succeeded");

    /* Get the cause of broken collective I/O */
    ret = H5Pget_mpio_no_collective_cause(dxpl_read, &no_collective_cause_local_read,
                                          &no_collective_cause_global_read);
    VRFY((ret >= 0), "retrieving no collective cause succeeded");

    ret = H5Pget_no_selection_io_cause(dxpl_read, &no_selection_io_cause_read);
    VRFY((ret >= 0), "retrieving no selection io cause succeeded");

    if (no_collective_cause_local_read & H5D_MPIO_NO_SELECTION_IO) {

        VRFY((no_selection_io_cause_read == no_selection_io_cause_expected),
             "H5D_MPIO_NO_SELECTION_IO for read is as expected");
    }

    if (no_collective_cause_global_read & H5D_MPIO_NO_SELECTION_IO) {

        VRFY((no_selection_io_cause_read == no_selection_io_cause_expected),
             "H5D_MPIO_NO_SELECTION_IO for read is as expected");
    }

    /* Check write vs read */
    VRFY((no_collective_cause_local_read == no_collective_cause_local_write),
         "reading and writing are the same for local cause of Broken Collective I/O");
    VRFY((no_collective_cause_global_read == no_collective_cause_global_write),
         "reading and writing are the same for global cause of Broken Collective I/O");

    /* Test values */
    memset(message, 0, sizeof(message));
    snprintf(message, sizeof(message), "Local cause of Broken Collective I/O has the correct value for %s.\n",
             test_name);
    VRFY((no_collective_cause_local_write == no_collective_cause_local_expected), message);
    memset(message, 0, sizeof(message));
    snprintf(message, sizeof(message),
             "Global cause of Broken Collective I/O has the correct value for %s.\n", test_name);
    VRFY((no_collective_cause_global_write == no_collective_cause_global_expected), message);

    /* Release some resources */
    if (sid)
        H5Sclose(sid);
    if (fapl_id)
        H5Pclose(fapl_id);
    if (dcpl)
        H5Pclose(dcpl);
    if (dxpl_write)
        H5Pclose(dxpl_write);
    if (dxpl_read)
        H5Pclose(dxpl_read);
    if (dataset)
        H5Dclose(dataset);
    if (mem_space)
        H5Sclose(mem_space);
    if (file_space)
        H5Sclose(file_space);
    if (fid)
        H5Fclose(fid);
    free(buffer);

    /* clean up external file */
    if (selection_mode & TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL)
        HDremove(FILE_EXTERNAL);

    return;
}

/* Function: no_collective_cause_tests
 *
 * Purpose: Tests cases for broken collective IO.
 */
void
no_collective_cause_tests(void)
{
    /*
     * Test individual cause
     */
    test_no_collective_cause_mode(TEST_COLLECTIVE);
    test_no_collective_cause_mode(TEST_SET_INDEPENDENT);
    test_no_collective_cause_mode(TEST_DATATYPE_CONVERSION);
    test_no_collective_cause_mode(TEST_DATA_TRANSFORMS);
    test_no_collective_cause_mode(TEST_NOT_SIMPLE_OR_SCALAR_DATASPACES);
    test_no_collective_cause_mode(TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_COMPACT);
    test_no_collective_cause_mode(TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL);

    /*
     * Test combined causes
     */
    test_no_collective_cause_mode(TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL | TEST_DATATYPE_CONVERSION);
    test_no_collective_cause_mode(TEST_DATATYPE_CONVERSION | TEST_DATA_TRANSFORMS);
    test_no_collective_cause_mode(TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET_EXTERNAL | TEST_DATATYPE_CONVERSION |
                                  TEST_DATA_TRANSFORMS);

    return;
}

/* Function: dense_attr_test
 *
 * Purpose: Test cases for writing dense attributes in parallel
 */
void
test_dense_attr(void)
{
    int         mpi_size, mpi_rank;
    hid_t       fpid, fid;
    hid_t       gid, gpid;
    hid_t       atFileSpace, atid;
    hsize_t     atDims[1] = {10000};
    herr_t      status;
    const char *filename;

    /* get filename */
    filename = (const char *)GetTestParameters();
    assert(filename != NULL);

    /* set up MPI parameters */
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    fpid = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((fpid > 0), "H5Pcreate succeeded");
    status = H5Pset_libver_bounds(fpid, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    VRFY((status >= 0), "H5Pset_libver_bounds succeeded");
    status = H5Pset_fapl_mpio(fpid, test_comm, MPI_INFO_NULL);
    VRFY((status >= 0), "H5Pset_fapl_mpio succeeded");
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fpid);
    VRFY((fid > 0), "H5Fcreate succeeded");
    status = H5Pclose(fpid);
    VRFY((status >= 0), "H5Pclose succeeded");

    gpid = H5Pcreate(H5P_GROUP_CREATE);
    VRFY((gpid > 0), "H5Pcreate succeeded");
    status = H5Pset_attr_phase_change(gpid, 0, 0);
    VRFY((status >= 0), "H5Pset_attr_phase_change succeeded");
    gid = H5Gcreate2(fid, "foo", H5P_DEFAULT, gpid, H5P_DEFAULT);
    VRFY((gid > 0), "H5Gcreate2 succeeded");
    status = H5Pclose(gpid);
    VRFY((status >= 0), "H5Pclose succeeded");

    atFileSpace = H5Screate_simple(1, atDims, NULL);
    VRFY((atFileSpace > 0), "H5Screate_simple succeeded");
    atid = H5Acreate2(gid, "bar", H5T_STD_U64LE, atFileSpace, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((atid > 0), "H5Acreate succeeded");
    status = H5Sclose(atFileSpace);
    VRFY((status >= 0), "H5Sclose succeeded");

    status = H5Aclose(atid);
    VRFY((status >= 0), "H5Aclose succeeded");

    status = H5Gclose(gid);
    VRFY((status >= 0), "H5Gclose succeeded");
    status = H5Fclose(fid);
    VRFY((status >= 0), "H5Fclose succeeded");

    return;
}

int
main(int argc, char **argv)
{
    int     express_test;
    int     mpi_size, mpi_rank; /* mpi variables */
    hsize_t oldsize, newsize = 1048576;

#ifndef H5_HAVE_WIN32_API
    /* Un-buffer the stdout and stderr */
    HDsetbuf(stderr, NULL);
    HDsetbuf(stdout, NULL);
#endif

    MPI_Init(&argc, &argv);
    MPI_Comm_size(test_comm, &mpi_size);
    MPI_Comm_rank(test_comm, &mpi_rank);

    mpi_rank_framework_g = mpi_rank;

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
        TestInit(argv[0], usage, parse_options);

        /* Parse command line arguments */
        TestParseCmdLine(argc, argv);

        AddTest("idsetw", dataset_writeInd, NULL, "dataset independent write", PARATESTFILE);

        AddTest("idsetr", dataset_readInd, NULL, "dataset independent read", PARATESTFILE);

        AddTest("cdsetw", dataset_writeAll, NULL, "dataset collective write", PARATESTFILE);

        AddTest("cdsetr", dataset_readAll, NULL, "dataset collective read", PARATESTFILE);

        AddTest("eidsetw2", extend_writeInd2, NULL, "extendible dataset independent write #2", PARATESTFILE);

        AddTest("selnone", none_selection_chunk, NULL, "chunked dataset with none-selection", PARATESTFILE);

#ifdef H5_HAVE_FILTER_DEFLATE
        AddTest("cmpdsetr", compress_readAll, NULL, "compressed dataset collective read", PARATESTFILE);
#endif /* H5_HAVE_FILTER_DEFLATE */

        /* Display testing information */
        TestInfo(argv[0]);

        /* setup file access property list */
        fapl = H5Pcreate(H5P_FILE_ACCESS);
        H5Pset_fapl_mpio(fapl, test_comm, MPI_INFO_NULL);

        /* Perform requested testing */
        PerformTests();
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* Restore the default bigio setting */
    H5_mpi_set_bigio_count(oldsize);

    express_test = GetTestExpress();
    if ((express_test == 0) && (mpi_rank < 2)) {
        MpioTest2G(test_comm);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if (mpi_rank == 0)
        HDremove(FILENAME[0]);

    for (int i = 0; i < NFILENAME; i++) {
        free(filenames[i]);
        filenames[i] = NULL;
    }

    H5close();
    if (test_comm != MPI_COMM_WORLD) {
        MPI_Comm_free(&test_comm);
    }
    MPI_Finalize();
    return 0;
}
