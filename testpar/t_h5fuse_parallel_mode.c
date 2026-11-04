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
 * Helper for the "h5fuse -p" (parallel mode) test.
 *
 * "write" (run with mpiexec): writes H5FUSE_TEST_FILE with the subfiling VFD,
 * using several subfiles, and leaves the subfiles and the configuration file
 * in place so that h5fuse can combine them.
 *
 * "verify" (run on one rank): opens the combined file with the default file
 * driver and checks every element of the dataset.
 */

#include "testpar.h"

#ifdef H5_HAVE_SUBFILING_VFD

#include "H5FDsubfiling.h"

#define H5FUSE_TEST_DIR      "h5fuse_parallel_mode"
#define H5FUSE_TEST_FILE     H5FUSE_TEST_DIR "/h5fuse_parallel_mode.h5"
#define H5FUSE_TEST_DSET     "data"
#define H5FUSE_TEST_STRIPE   4096
#define H5FUSE_TEST_SUBFILES 4
#define H5FUSE_TEST_PER_RANK 16384 /* elements written by each rank */

static int
write_file(int mpi_rank, int mpi_size)
{
    H5FD_subfiling_config_t cfg;
    hid_t                   fapl   = H5I_INVALID_HID;
    hid_t                   file   = H5I_INVALID_HID;
    hid_t                   fspace = H5I_INVALID_HID;
    hid_t                   mspace = H5I_INVALID_HID;
    hid_t                   dset   = H5I_INVALID_HID;
    hsize_t                 dims[1];
    hsize_t                 start[1];
    hsize_t                 count[1] = {H5FUSE_TEST_PER_RANK};
    int                    *buf      = NULL;
    int                     ret      = 1;

    if (mpi_rank == 0)
        (void)HDmkdir(H5FUSE_TEST_DIR, (mode_t)0755);
    MPI_Barrier(MPI_COMM_WORLD);

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto done;
    if (H5Pset_mpi_params(fapl, MPI_COMM_WORLD, MPI_INFO_NULL) < 0)
        goto done;
    if (H5Pget_fapl_subfiling(fapl, &cfg) < 0)
        goto done;
    cfg.shared_cfg.stripe_size  = H5FUSE_TEST_STRIPE;
    cfg.shared_cfg.stripe_count = H5FUSE_TEST_SUBFILES;
    if (H5Pset_fapl_subfiling(fapl, &cfg) < 0)
        goto done;

    if ((file = H5Fcreate(H5FUSE_TEST_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto done;

    dims[0]  = (hsize_t)mpi_size * H5FUSE_TEST_PER_RANK;
    start[0] = (hsize_t)mpi_rank * H5FUSE_TEST_PER_RANK;
    if ((fspace = H5Screate_simple(1, dims, NULL)) < 0)
        goto done;
    if ((mspace = H5Screate_simple(1, count, NULL)) < 0)
        goto done;
    if ((dset = H5Dcreate2(file, H5FUSE_TEST_DSET, H5T_NATIVE_INT, fspace, H5P_DEFAULT, H5P_DEFAULT,
                           H5P_DEFAULT)) < 0)
        goto done;
    if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        goto done;

    if (NULL == (buf = malloc(H5FUSE_TEST_PER_RANK * sizeof(int))))
        goto done;
    for (int i = 0; i < H5FUSE_TEST_PER_RANK; i++)
        buf[i] = mpi_rank * H5FUSE_TEST_PER_RANK + i;
    if (H5Dwrite(dset, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, buf) < 0)
        goto done;

    ret = 0;

done:
    free(buf);
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(mspace);
        H5Sclose(fspace);
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY
    return ret;
}

static int
verify_file(void)
{
    hid_t    file  = H5I_INVALID_HID;
    hid_t    dset  = H5I_INVALID_HID;
    hid_t    space = H5I_INVALID_HID;
    hssize_t npoints;
    int     *buf = NULL;
    int      ret = 1;

    /* The combined file is a regular HDF5 file */
    if ((file = H5Fopen(H5FUSE_TEST_FILE, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        goto done;
    if ((dset = H5Dopen2(file, H5FUSE_TEST_DSET, H5P_DEFAULT)) < 0)
        goto done;
    if ((space = H5Dget_space(dset)) < 0)
        goto done;
    if ((npoints = H5Sget_simple_extent_npoints(space)) <= 0)
        goto done;
    if (NULL == (buf = malloc((size_t)npoints * sizeof(int))))
        goto done;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto done;

    for (hssize_t i = 0; i < npoints; i++)
        if (buf[i] != (int)i) {
            printf("h5fuse -p: element %lld is %d, expected %lld\n", (long long)i, buf[i], (long long)i);
            goto done;
        }

    printf("h5fuse -p: all %lld elements of the combined file are correct\n", (long long)npoints);
    ret = 0;

done:
    free(buf);
    H5E_BEGIN_TRY
    {
        H5Sclose(space);
        H5Dclose(dset);
        H5Fclose(file);
    }
    H5E_END_TRY
    return ret;
}

int
main(int argc, char **argv)
{
    int mpi_rank;
    int mpi_size;
    int required = MPI_THREAD_MULTIPLE;
    int provided = 0;
    int ret      = 1;

    if (MPI_SUCCESS != MPI_Init_thread(&argc, &argv, required, &provided)) {
        printf("MPI_Init_thread failed\n");
        return 1;
    }
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    if (argc != 2 || (strcmp(argv[1], "write") != 0 && strcmp(argv[1], "verify") != 0)) {
        if (mpi_rank == 0)
            printf("usage: %s write|verify\n", argv[0]);
    }
    else if (strcmp(argv[1], "write") == 0) {
        /* The subfiling VFD requires MPI_THREAD_MULTIPLE. As in t_subfiling_vfd,
         * that's a failure rather than a skip. */
        if (provided != required) {
            if (mpi_rank == 0)
                printf("MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE\n");
        }
        else
            ret = write_file(mpi_rank, mpi_size);
    }
    else /* verify reads the combined file with the default file driver */
        ret = (mpi_rank == 0) ? verify_file() : 0;

    /* Fail on every rank if any rank failed */
    MPI_Allreduce(MPI_IN_PLACE, &ret, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);

    H5close();
    MPI_Finalize();
    return ret;
}

#else /* H5_HAVE_SUBFILING_VFD */

int
main(void)
{
    printf("Testing Subfiling VFD - SKIPPED\n");
    return 0;
}

#endif /* H5_HAVE_SUBFILING_VFD */
