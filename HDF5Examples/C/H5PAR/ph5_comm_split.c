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
 * Demonstrates how to use parallel HDF5 with a sub-communicator created via
 * MPI_Comm_split rather than MPI_COMM_WORLD.
 *
 * Applications commonly divide MPI_COMM_WORLD into groups assigned to different
 * ranks, with only a subset accessing the HDF5 file. This example
 * splits MPI_COMM_WORLD by rank parity: even ranks form a "writer"
 * group that collectively creates a dataset, assigning one row per writer rank,
 * then reopens the file in read-only mode to verify the written data. The odd
 * ranks form a group that performs unrelated tasks and does not enter the
 * HDF5 code path. A single MPI_Barrier on MPI_COMM_WORLD at the
 * end synchronizes the two groups.
 *
 * Two key rules govern this pattern:
 *
 *   1. MPI_Comm_split is a collective operation on the *parent* communicator, so
 *      every rank in MPI_COMM_WORLD must call it, including those
 *      that will not open the file. Any rank that skips the split will hang
 *      all ranks that called it.
 *
 *   2. After opening a file with a FAPL that uses a sub-communicator
 *      (H5Pset_fapl_mpio(fapl_id, sub_comm, info)), every collective HDF5
 *      call on that file is collective only within that sub-communicator.
 *      Every rank in the group must participate in those calls, in the same
 *      order. No rank outside the group may participate. Violating
 *      either rule can deadlock the job within the MPI collectives that HDF5
 *      issues internally (not in the application's own MPI calls), so the
 *      stack trace of a hung rank usually points to H5Fopen or H5Dwrite
 *      rather than the mismatched call itself.
 *
 * This example works with any number of MPI ranks.
 *
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"

#ifdef H5_HAVE_PARALLEL

#define EXAMPLE_FILE      "ph5_comm_split.h5"
#define EXAMPLE_DSET_NAME "DSET"

/* Number of elements each rank of the writer group contributes */
#define EXAMPLE_DSET_ROW_LEN 8

#ifndef PATH_MAX
#define PATH_MAX 512
#endif
/*
 * Abort the entire job if an HDF5 call fails.
 *
 * Local recovery from a failure inside a collective call is unsafe:
 * other ranks in the group are already waiting in the corresponding MPI
 * collective. If one rank simply returns, it will leave others stranded. Aborting
 * over MPI_COMM_WORLD is the only way to ensure all ranks, including those
 * outside the writer group, are released.
 */
static void
check(bool succeeded, const char *what)
{
    if (!succeeded) {
        fprintf(stderr, "%s failed\n", what);
        fflush(stderr);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
}

/*
 * Create the dataset, and have each rank in the writer group write its assigned
 * row. Each rank fills its row with its MPI_COMM_WORLD rank, allowing
 * verification to confirm that each row was written to the correct location.
 */
static void
write_dataset(hid_t file_id, int world_rank, int group_rank, int group_size)
{
    hid_t   dset_id    = H5I_INVALID_HID;
    hid_t   file_space = H5I_INVALID_HID;
    hid_t   mem_space  = H5I_INVALID_HID;
    hid_t   dxpl_id    = H5I_INVALID_HID;
    hsize_t dims[2]    = {(hsize_t)group_size, EXAMPLE_DSET_ROW_LEN};
    hsize_t start[2]   = {(hsize_t)group_rank, 0};
    hsize_t count[2]   = {1, EXAMPLE_DSET_ROW_LEN};
    int     data[EXAMPLE_DSET_ROW_LEN];

    for (int i = 0; i < EXAMPLE_DSET_ROW_LEN; i++)
        data[i] = world_rank;

    /*
     * The dataset gets one row per rank of the writer group, so its shape
     * comes from the size of the sub-communicator, not from the size of
     * MPI_COMM_WORLD. Every rank of the group describes the same full
     * dataset here; the per-rank selection is made further down.
     */
    file_space = H5Screate_simple(2, dims, NULL);
    check(file_space >= 0, "H5Screate_simple (file dataspace)");

    /*
     * H5Dcreate2 is collective over the file's communicator, which is the
     * sub-communicator the FAPL was built with.
     */
    dset_id = H5Dcreate2(file_id, EXAMPLE_DSET_NAME, H5T_NATIVE_INT, file_space, H5P_DEFAULT, H5P_DEFAULT,
                         H5P_DEFAULT);
    check(dset_id >= 0, "H5Dcreate2");

    /* Select the single row this rank owns */
    check(H5Sselect_hyperslab(file_space, H5S_SELECT_SET, start, NULL, count, NULL) >= 0,
          "H5Sselect_hyperslab (write)");

    /* The in-memory buffer holds exactly that one row */
    mem_space = H5Screate_simple(2, count, NULL);
    check(mem_space >= 0, "H5Screate_simple (memory dataspace)");

    /*
     * Ask for collective I/O. The MPI collectives this triggers run over
     * the sub-communicator, so only the writer group participates.
     */
    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    check(dxpl_id >= 0, "H5Pcreate (H5P_DATASET_XFER)");
    check(H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) >= 0, "H5Pset_dxpl_mpio");

    check(H5Dwrite(dset_id, H5T_NATIVE_INT, mem_space, file_space, dxpl_id, data) >= 0, "H5Dwrite");

    check(H5Pclose(dxpl_id) >= 0, "H5Pclose (dxpl)");
    check(H5Sclose(mem_space) >= 0, "H5Sclose (memory dataspace)");
    check(H5Sclose(file_space) >= 0, "H5Sclose (file dataspace)");
    check(H5Dclose(dset_id) >= 0, "H5Dclose (write)");
}

/*
 * Read back the row this rank wrote and confirm its contents. The file has
 * been reopened read-only on the same sub-communicator, so these calls are
 * collective over the writer group exactly as the write calls were.
 */
static void
verify_dataset(hid_t file_id, int world_rank, int group_rank)
{
    hid_t   dset_id    = H5I_INVALID_HID;
    hid_t   file_space = H5I_INVALID_HID;
    hid_t   mem_space  = H5I_INVALID_HID;
    hid_t   dxpl_id    = H5I_INVALID_HID;
    hsize_t start[2]   = {(hsize_t)group_rank, 0};
    hsize_t count[2]   = {1, EXAMPLE_DSET_ROW_LEN};
    int     data[EXAMPLE_DSET_ROW_LEN];

    dset_id = H5Dopen2(file_id, EXAMPLE_DSET_NAME, H5P_DEFAULT);
    check(dset_id >= 0, "H5Dopen2");

    /* Ask the file for the dataset's shape and select this rank's row in it */
    file_space = H5Dget_space(dset_id);
    check(file_space >= 0, "H5Dget_space");
    check(H5Sselect_hyperslab(file_space, H5S_SELECT_SET, start, NULL, count, NULL) >= 0,
          "H5Sselect_hyperslab (read)");

    mem_space = H5Screate_simple(2, count, NULL);
    check(mem_space >= 0, "H5Screate_simple (read memory dataspace)");

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    check(dxpl_id >= 0, "H5Pcreate (H5P_DATASET_XFER)");
    check(H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) >= 0, "H5Pset_dxpl_mpio");

    check(H5Dread(dset_id, H5T_NATIVE_INT, mem_space, file_space, dxpl_id, data) >= 0, "H5Dread");

    for (int i = 0; i < EXAMPLE_DSET_ROW_LEN; i++) {
        if (data[i] != world_rank) {
            fprintf(stderr, "rank %d read %d at element %d of its row, expected %d\n", world_rank, data[i], i,
                    world_rank);
            fflush(stderr);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
    }

    check(H5Pclose(dxpl_id) >= 0, "H5Pclose (dxpl)");
    check(H5Sclose(mem_space) >= 0, "H5Sclose (read memory dataspace)");
    check(H5Sclose(file_space) >= 0, "H5Sclose (read file dataspace)");
    check(H5Dclose(dset_id) >= 0, "H5Dclose (read)");
}

/*
 * Build a FAPL bound to the given communicator, with the collective
 * metadata settings that are generally recommended for parallel access.
 */
static hid_t
make_fapl(MPI_Comm comm)
{
    hid_t fapl_id = H5Pcreate(H5P_FILE_ACCESS);

    check(fapl_id >= 0, "H5Pcreate (H5P_FILE_ACCESS)");

    /*
     * This is the call that ties the file to the sub-communicator. HDF5
     * duplicates the communicator internally, so the application keeps
     * ownership of its own handle and is free to release it once the FAPL
     * has been set.
     */
    check(H5Pset_fapl_mpio(fapl_id, comm, MPI_INFO_NULL) >= 0, "H5Pset_fapl_mpio");

    /*
     * OPTIONAL: It is generally recommended to set collective
     *           metadata reads on FAPL to perform metadata reads
     *           collectively, which usually allows datasets
     *           to perform better at scale, although it is not
     *           strictly necessary.
     */
    check(H5Pset_all_coll_metadata_ops(fapl_id, true) >= 0, "H5Pset_all_coll_metadata_ops");

    /*
     * OPTIONAL: It is generally recommended to set collective
     *           metadata writes on FAPL to perform metadata writes
     *           collectively, which usually allows datasets
     *           to perform better at scale, although it is not
     *           strictly necessary.
     */
    check(H5Pset_coll_metadata_write(fapl_id, true) >= 0, "H5Pset_coll_metadata_write");

    return fapl_id;
}

int
main(int argc, char **argv)
{
    MPI_Comm sub_comm   = MPI_COMM_NULL;
    hid_t    fapl_id    = H5I_INVALID_HID;
    hid_t    file_id    = H5I_INVALID_HID;
    char    *par_prefix = NULL;
    char     filename[PATH_MAX];
    int      world_rank, world_size;
    int      group_rank, group_size;
    int      color;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    /* Parse any parallel prefix and create filename */
    par_prefix = getenv("HDF5_PARAPREFIX");
    snprintf(filename, PATH_MAX, "%s%s%s", par_prefix ? par_prefix : "", par_prefix ? "/" : "", EXAMPLE_FILE);

    /*
     * -----------------------------------------
     * Split MPI_COMM_WORLD into the two groups
     * -----------------------------------------
     *
     * Color 0 is the writer group, which will do all of the HDF5 work.
     * Color 1 stands in for a group with other responsibilities and never
     * opens the file.
     *
     * Both groups call MPI_Comm_split, because the split is collective over
     * MPI_COMM_WORLD. Only the ranks of color 0 go on to use the resulting
     * communicator for HDF5.
     */
    color = world_rank % 2;
    MPI_Comm_split(MPI_COMM_WORLD, color, world_rank, &sub_comm);

    MPI_Comm_rank(sub_comm, &group_rank);
    MPI_Comm_size(sub_comm, &group_size);

    if (color == 0) {
        /*
         * -------------------------------------------------
         * Writer group: create the file on the sub-comm and
         * write one row per rank of the group
         * -------------------------------------------------
         *
         * Every call from here to the H5Fclose below is collective over
         * sub_comm. The odd ranks are not in sub_comm and make none of
         * these calls.
         */
        if (group_rank == 0)
            printf("%d of %d ranks form the writer group\n", group_size, world_size);

        fapl_id = make_fapl(sub_comm);

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        check(file_id >= 0, "H5Fcreate");

        write_dataset(file_id, world_rank, group_rank, group_size);

        check(H5Fclose(file_id) >= 0, "H5Fclose (write)");
        check(H5Pclose(fapl_id) >= 0, "H5Pclose (write fapl)");

        /*
         * ------------------------------------------------
         * Writer group: reopen read-only on the same
         * sub-comm and verify what was written
         * ------------------------------------------------
         *
         * Reopening needs a fresh FAPL carrying the same communicator. A
         * FAPL does not survive the file it was used to open, and the
         * communicator must match the set of ranks that will make the
         * collective calls on the reopened file.
         */
        fapl_id = make_fapl(sub_comm);

        file_id = H5Fopen(filename, H5F_ACC_RDONLY, fapl_id);
        check(file_id >= 0, "H5Fopen");

        verify_dataset(file_id, world_rank, group_rank);

        check(H5Fclose(file_id) >= 0, "H5Fclose (read)");
        check(H5Pclose(fapl_id) >= 0, "H5Pclose (read fapl)");
    }
    else {
        /*
         * ----------------------------------------------------
         * Other group: no HDF5 calls whatsoever
         * ----------------------------------------------------
         *
         * A single HDF5 call here against the file above would be a call
         * that the writer group never makes, and it would block forever in
         * the MPI collective it issues internally.
         */
        if (group_rank == 0)
            printf("%d of %d ranks are doing other work and stay out of HDF5\n", group_size, world_size);
    }

    /*
     * Rejoin the two groups. This barrier is over MPI_COMM_WORLD, so it is
     * the first collective since the split that both groups take part in.
     */
    MPI_Barrier(MPI_COMM_WORLD);

    /*
     * Cleanup created file. MPI_File_delete is not collective, so exactly
     * one rank makes the call, and it does so after the barrier above has
     * confirmed that every writer rank has closed the file.
     */
    if (world_rank == 0 && !getenv(HDF5_NOCLEANUP))
        MPI_File_delete(filename, MPI_INFO_NULL);

    if (world_rank == 0)
        printf("PHDF5 example finished with no errors\n");

    /* MPI_Comm_free is collective over sub_comm, so both groups free theirs */
    MPI_Comm_free(&sub_comm);

    MPI_Finalize();

    return 0;
}

#else

int
main(void)
{
    printf("HDF5 not configured with parallel support!\n");
    return 0;
}

#endif
