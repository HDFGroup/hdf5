/*
 * Example of how to use Flexible Parallel HDF5.
 *
 * Author:
 *      Bill Wendling (wendling@ncsa.uiuc.edu)
 *      20. February 2003
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mpi.h"

#ifdef H5_HAVE_FPHDF5

#include "hdf5.h"
#include "H5public.h"
#include "H5FPpublic.h"

MPI_Comm SAP_Comm;
MPI_Comm SAP_Barrier_Comm;

void err(const char *func, int mrc);
void create_file(int);

void create_file(int sap_rank)
{
    int my_rank, mrc;
    hid_t fid, acc_tpl, sid, dataset;

    if ((mrc = MPI_Comm_rank(SAP_Comm, &my_rank)) != MPI_SUCCESS) {
        err("H5FPinit", mrc);
        return;
    }

    fprintf(stderr, "%d: Creating file foo.h5\n", my_rank);

    if ((acc_tpl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        fprintf(stderr, "%d: Failed to create access property list\n", my_rank);
        goto done;
    }

    fprintf(stderr, "%d: Created access property list\n", my_rank);

    if (H5Pset_fapl_fphdf5(acc_tpl, SAP_Comm, SAP_Barrier_Comm,
                           MPI_INFO_NULL, (unsigned)sap_rank) < 0) {
        fprintf(stderr, "%d: Failed to set fapl\n", my_rank);
        goto done;
    }

    fprintf(stderr, "%d: Set access property list\n", my_rank);

    if ((fid = H5Fcreate("/tmp/foo.h5", H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl)) < 0) {
        fprintf(stderr, "%d: Failed to create file foo.h5\n", my_rank);
        goto done;
    }

    fprintf(stderr, "%d: Created file foo.h5\n", my_rank);

    fprintf(stderr, "%d: SAP_Barrier_Comm==%d\n", my_rank, SAP_Barrier_Comm);
    MPI_Barrier(SAP_Barrier_Comm);

    fflush(NULL);
    fflush(NULL);
    fflush(NULL);
    sleep(3);

goto done;

    if (my_rank == 0) {
        /* Create a dataset in the file */
        hsize_t dims[2];

        dims[0] = 3;
        dims[1] = 5;

        if ((sid = H5Screate_simple(2, dims, NULL)) < 0) {
            fprintf(stderr, "%d: Failed to create simple dataspace\n", my_rank);
            goto done;
        }

        if ((dataset = H5Dcreate(fid, "/dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) {
            fprintf(stderr, "%d: Failed to create simple dataset\n", my_rank);
            goto done;
        }

        fprintf(stderr, "%d: Created dataset ``/dataset''\n", my_rank);

        H5Sclose(sid);
        H5Dclose(dataset);
    }

    fprintf(stderr, "%d: SAP_Barrier_Comm==%d\n", my_rank, SAP_Barrier_Comm);
    MPI_Barrier(SAP_Barrier_Comm);

    if (my_rank == 2) {
        /* See if dataset is there */
        if ((dataset = H5Dopen(fid, "/dataset")) < 0) {
            fprintf(stderr, "%d: Failed to open dataset\n", my_rank);
            goto done;
        }

        fprintf(stderr, "%d: Opened dataset ``/dataset''\n", my_rank);

        H5Dclose(dataset);
    }

    fprintf(stderr, "%d: SAP_Barrier_Comm==%d\n", my_rank, SAP_Barrier_Comm);
    MPI_Barrier(SAP_Barrier_Comm);

done:
    fprintf(stderr, "----------------------------\n");
    fflush(stderr);
    if (H5Pclose(acc_tpl) < 0)
        fprintf(stderr, "%d: Failed to close access property list\n", my_rank);
    else
        fprintf(stderr, "%d: Closed access property list\n", my_rank);

    if (H5Fclose(fid) < 0)
        fprintf(stderr, "%d: Failed to close file\n", my_rank);
    else
        fprintf(stderr, "%d: Closed file\n", my_rank);

    fprintf(stderr, "%d: leaving create_file\n", my_rank);
    MPI_Barrier(SAP_Barrier_Comm);
    fflush(NULL);
    fflush(NULL);
    fflush(NULL);
    fflush(NULL);
    fflush(NULL);
    sleep(5);
}

int main(int argc, char *argv[])
{
    int ret = EXIT_SUCCESS, mrc;
    int my_rank;
    int sap_rank = 1;

    MPI_Init(&argc, &argv);
    mrc = H5FPinit(MPI_COMM_WORLD, sap_rank, &SAP_Comm, &SAP_Barrier_Comm);

    if (mrc < 0) {
        err("H5FPinit", mrc);
        ret = EXIT_FAILURE;
        goto fail;
    }

    if ((mrc = MPI_Comm_rank(SAP_Comm, &my_rank)) != MPI_SUCCESS) {
        err("H5FPinit", mrc);
        ret = EXIT_FAILURE;
        goto fail;
    }

    fprintf(stderr, "%d: Initialized FPHDF5\n", my_rank);

    if (my_rank != sap_rank) {
        create_file(sap_rank);
        MPI_Barrier(SAP_Barrier_Comm);
    }

fail:
    H5FPfinalize();
    fprintf(stderr, "%d: H5FP finalized\n", my_rank);

    H5close();
    fprintf(stderr, "%d: HDF5 Closed\n", my_rank);

    MPI_Finalize();
    fprintf(stderr, "%d: MPI finalized\n", my_rank);
    return ret;
}

void err(const char *func, int mrc)
{
    fprintf(stderr, "error: %s: ", func);

    switch (mrc) {
    case MPI_ERR_COMM:
        fprintf(stderr, "invalid communicator\n");
        break;
    case MPI_ERR_COUNT:
        fprintf(stderr, "invalid count argument\n");
        break;
    case MPI_ERR_TYPE:
        fprintf(stderr, "invalid datatype argument\n");
        break;
    case MPI_ERR_TAG:
        fprintf(stderr, "invalid tag argument\n");
        break;
    case MPI_ERR_RANK:
        fprintf(stderr, "invalid source or destination rank\n");
        break;
    case MPI_ERR_INTERN:
        fprintf(stderr, "internal MPI-IO error\n");
        break;
    case MPI_ERR_REQUEST:
        fprintf(stderr, "invalid MPI_Request\n");
        break;
    case MPI_ERR_ARG:
        fprintf(stderr, "invalid argument\n");
        break;
    default:
        fprintf(stderr, "unknown MPI-IO error\n");
        break;
    }
}

#else

/* dummy program since H5_HAVE_PARALLE is not configured in */
int
main(int argc, char *argv[])
{
    int my_rank;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    if (my_rank==0){
	printf("No t_fphdf5 Test because FPHDF5 is not configured in\n");
    }
    MPI_Finalize();
    return(0);
}
#endif
