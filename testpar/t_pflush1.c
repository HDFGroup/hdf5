/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Leon Arber  <larber@uiuc.edu>
 *              Sept. 28, 2006.
 *
 * Purpose:     This is the first half of a two-part test that makes sure
 *              that a file can be read after a parallel application crashes
 *              as long as the file was flushed first.  We simulate a crash by
 *              calling _exit() since this doesn't flush HDF5 caches but
 *              still exits with success.
 */
#include "h5test.h"

const char *FILENAME[] = {
    "flush",
    "noflush",
    NULL
};

static int  data_g[100][100];

#define N_GROUPS    100


/*-------------------------------------------------------------------------
 * Function:    create_test_file
 *
 * Purpose:     Creates the file used in part 1 of the test
 *
 * Return:      Success:    A valid file ID
 *              Failure:    H5I_INVALID_HID
 *
 * Programmer:  Leon Arber
 *              Sept. 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_test_file(char *name, hid_t fapl_id)
{
    hid_t       fid             = H5I_INVALID_HID;
    hid_t       dcpl_id         = H5I_INVALID_HID;
    hid_t       sid             = H5I_INVALID_HID;
    hid_t       did             = H5I_INVALID_HID;
    hid_t       top_level_gid   = H5I_INVALID_HID;
    hid_t       gid             = H5I_INVALID_HID;
    hid_t       dxpl_id         = H5I_INVALID_HID;
    hsize_t     dims[2]         = {100, 100};
    hsize_t     chunk_dims[2]   = {5, 5};
    hsize_t     i, j;

    if((fid = H5Fcreate(name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        goto error;

    /* Create a chunked dataset */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_chunk(dcpl_id, 2, chunk_dims) < 0)
        goto error;
    if((sid = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;
    if((did = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    if((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if(H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;

    /* Write some data */
    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++)
            data_g[i][j] = (int)(i + (i * j) + j);

    if(H5Dwrite(did, H5T_NATIVE_INT, sid, sid, dxpl_id, data_g) < 0)
        goto error;

    /* Create some groups */
    if((top_level_gid = H5Gcreate2(fid, "some_groups", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    for(i = 0; i < N_GROUPS; i++) {
        HDsprintf(name, "grp%02u", (unsigned)i);
        if((gid = H5Gcreate2(top_level_gid, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Gclose(gid) < 0)
            goto error;
    }

    return fid;

error:
    return H5I_INVALID_HID;
} /* end create_test_file() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Part 1 of a two-part parallel H5Fflush() test.
 *
 * Return:      EXIT_FAILURE (always)
 *
 * Programmer:  Robb Matzke
 *              Friday, October 23, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char* argv[])
{
    hid_t       fid1        = H5I_INVALID_HID;
    hid_t       fid2        = H5I_INVALID_HID;
    hid_t       fapl_id     = H5I_INVALID_HID;
    MPI_File    *mpifh_p    = NULL;
    char        name[1024];
    const char  *envval     = NULL;
    int         mpi_size;
    int         mpi_rank;
    MPI_Comm    comm        = MPI_COMM_WORLD;
    MPI_Info    info        = MPI_INFO_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    if(mpi_rank == 0)
        TESTING("H5Fflush (part1)");

    /* Don't run using the split VFD */
    envval = HDgetenv("HDF5_DRIVER");
    if(envval == NULL)
        envval = "nomatch";

    if(!HDstrcmp(envval, "split")) {
        if(mpi_rank == 0) {
            SKIPPED();
            HDputs("    Test not compatible with current Virtual File Driver");
        }
        MPI_Finalize();
        HDexit(EXIT_FAILURE);
    }

    if((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if(H5Pset_fapl_mpio(fapl_id, comm, info) < 0)
        goto error;

    /* Create the file */
    h5_fixname(FILENAME[0], fapl_id, name, sizeof(name));
    if((fid1 = create_test_file(name, fapl_id)) < 0)
        goto error;
    /* Flush and exit without closing the library */
    if(H5Fflush(fid1, H5F_SCOPE_GLOBAL) < 0)
        goto error;

    /* Create the other file which will not be flushed */
    h5_fixname(FILENAME[1], fapl_id, name, sizeof(name));
    if((fid2 = create_test_file(name, fapl_id)) < 0)
        goto error;

    if(mpi_rank == 0)
        PASSED();

    HDfflush(stdout);
    HDfflush(stderr);

    /* Some systems like AIX do not like files not being closed when MPI_Finalize
     * is called.  So, we need to get the MPI file handles, close them by hand.
     * Then the _exit is still needed to stop at_exit from happening in some systems.
     * Note that MPIO VFD returns the address of the file-handle in the VFD struct
     * because MPI_File_close wants to modify the file-handle variable.
     */

    /* Close file 1 */
    if(H5Fget_vfd_handle(fid1, fapl_id, (void **)&mpifh_p) < 0)
        goto error;
    if(MPI_File_close(mpifh_p) != MPI_SUCCESS)
        goto error;

    /* Close file 2 */
    if(H5Fget_vfd_handle(fid2, fapl_id, (void **)&mpifh_p) < 0)
        goto error;
    if(MPI_File_close(mpifh_p) != MPI_SUCCESS)
        goto error;

    HDfflush(stdout);
    HDfflush(stderr);

    /* Always exit with a failure code!
     *
     * In accordance with the standard, not having all processes
     * call MPI_Finalize() can be considered an error, so mpiexec
     * et al. may indicate failure on return. It's much easier to
     * always ignore the failure condition than to handle some
     * platforms returning success and others failure.
     */
    HD_exit(EXIT_FAILURE);

error:
    HDfflush(stdout);
    HDfflush(stderr);
    HDprintf("*** ERROR ***\n");
    HDprintf("THERE WAS A REAL ERROR IN t_pflush1.\n");
    HD_exit(EXIT_FAILURE);
} /* end main() */

