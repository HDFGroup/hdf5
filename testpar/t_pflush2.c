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
 * Purpose:     This is the second half of a two-part test that makes sure
 *              that a file can be read after a parallel application crashes as long
 *              as the file was flushed first.  We simulate a crash by
 *              calling _exit(0) since this doesn't flush HDF5 caches but
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
 * Function:    check_test_file
 *
 * Purpose:     Part 2 of a two-part H5Fflush() test.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Leon Arber
 *              Sept. 26, 2006.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
check_test_file(char* name, hid_t fapl_id)
{
    hid_t       fid             = H5I_INVALID_HID;
    hid_t       sid             = H5I_INVALID_HID;
    hid_t       did             = H5I_INVALID_HID;
    hid_t       top_level_gid   = H5I_INVALID_HID;
    hid_t       gid             = H5I_INVALID_HID;
    hid_t       dxpl_id         = H5I_INVALID_HID;
    hsize_t     dims[2];
    int         val;
    hsize_t     i, j;

    if((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if(H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) < 0)
        goto error;
    if((fid = H5Fopen(name, H5F_ACC_RDONLY, fapl_id)) < 0)
        goto error;

    /* Open the dataset */
    if((did = H5Dopen2(fid, "dset", H5P_DEFAULT)) < 0)
        goto error;
    if((sid = H5Dget_space(did)) < 0)
        goto error;
    if(H5Sget_simple_extent_dims(sid, dims, NULL) < 0)
        goto error;
    HDassert(100 == dims[0] && 100 == dims[1]);

    /* Read some data */
    if(H5Dread(did, H5T_NATIVE_INT, sid, sid, dxpl_id, data_g) < 0)
        goto error;
    for(i = 0; i < dims[0]; i++) {
        for(j = 0; j < dims[1]; j++) {
            val = (int)(i + (i * j) + j);
            if(data_g[i][j] != val) {
                H5_FAILED();
                HDprintf("    data_g[%lu][%lu] = %d\n", (unsigned long)i, (unsigned long)j, data_g[i][j]);
                HDprintf("    should be %d\n", val);
            }
        }
    }

    /* Open some groups */
    if((top_level_gid = H5Gopen2(fid, "some_groups", H5P_DEFAULT)) < 0)
        goto error;
    for(i = 0; i < N_GROUPS; i++) {
        HDsprintf(name, "grp%02u", (unsigned)i);
        if((gid = H5Gopen2(top_level_gid, name, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Gclose(gid) < 0)
            goto error;
    }

    if(H5Gclose(top_level_gid) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;
    if(H5Pclose(dxpl_id) < 0)
        goto error;
    if(H5Sclose(sid) < 0)
        goto error;

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dxpl_id);
        H5Gclose(top_level_gid);
        H5Dclose(did);
        H5Fclose(fid);
        H5Sclose(sid);
        H5Gclose(gid);
    } H5E_END_TRY;
    return FAIL;
} /* end check_test_file() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Part 2 of a two-part H5Fflush() test.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, October 23, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t       fapl_id1    = H5I_INVALID_HID;
    hid_t       fapl_id2    = H5I_INVALID_HID;
    H5E_auto2_t func;
    char        name[1024];
    const char *envval = NULL;

    int mpi_size;
    int mpi_rank;
    MPI_Comm comm  = MPI_COMM_WORLD;
    MPI_Info info  = MPI_INFO_NULL;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(comm, &mpi_size);
    MPI_Comm_rank(comm, &mpi_rank);

    if(mpi_rank == 0)
        TESTING("H5Fflush (part2 with flush)");

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

    if((fapl_id1 = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if(H5Pset_fapl_mpio(fapl_id1, comm, info) < 0)
        goto error;

    if((fapl_id2 = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if(H5Pset_fapl_mpio(fapl_id2, comm, info) < 0)
        goto error;

    /* Check the case where the file was flushed */
    h5_fixname(FILENAME[0], fapl_id1, name, sizeof(name));
    if(check_test_file(name, fapl_id1)) {
        H5_FAILED()
        goto error;
    }
    else if(mpi_rank == 0) {
        PASSED()
    }

    /* Check the case where the file was not flushed.  This should give an error
     * so we turn off the error stack temporarily.
     */
    if(mpi_rank == 0)
        TESTING("H5Fflush (part2 without flush)");
    H5Eget_auto2(H5E_DEFAULT,&func, NULL);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    h5_fixname(FILENAME[1], fapl_id2, name, sizeof(name));
    if(check_test_file(name, fapl_id2)) {
        if(mpi_rank == 0)
            PASSED()
    }
    else {
        H5_FAILED()
        goto error;
    }

    H5Eset_auto2(H5E_DEFAULT, func, NULL);

    h5_clean_files(&FILENAME[0], fapl_id1);
    h5_clean_files(&FILENAME[1], fapl_id2);

    MPI_Finalize();

    HDexit(EXIT_SUCCESS);

error:
    HDexit(EXIT_FAILURE);
} /* end main() */

