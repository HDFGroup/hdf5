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

/*
 * Parallel tests for file operations
 */

#include "testphdf5.h"

#define H5F_PACKAGE
#include "hdf5.h"
#include "H5Iprivate.h"
#include "H5FDprivate.h"
#include "H5Fpkg.h"

/*
 * test file access by communicator besides COMM_WORLD.
 * Split COMM_WORLD into two, one (even_comm) contains the original
 * processes of even ranks.  The other (odd_comm) contains the original
 * processes of odd ranks.  Processes in even_comm creates a file, then
 * cloose it, using even_comm.  Processes in old_comm just do a barrier
 * using odd_comm.  Then they all do a barrier using COMM_WORLD.
 * If the file creation and cloose does not do correct collective action
 * according to the communicator argument, the processes will freeze up
 * sooner or later due to barrier mixed up.
 */
void
test_split_comm_access(void)
{
    int mpi_size, mpi_rank;
    MPI_Comm comm;
    MPI_Info info = MPI_INFO_NULL;
    int is_old, mrc;
    int newrank, newprocs;
    hid_t fid;			/* file IDs */
    hid_t acc_tpl;		/* File access properties */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    herr_t ret;			/* generic return value */
    const char *filename;

    filename = (const char *)GetTestParameters();
    if (VERBOSE_MED)
	printf("Split Communicator access test on file %s\n",
	    filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    is_old = mpi_rank%2;
    mrc = MPI_Comm_split(MPI_COMM_WORLD, is_old, mpi_rank, &comm);
    VRFY((mrc==MPI_SUCCESS), "");
    MPI_Comm_size(comm,&newprocs);
    MPI_Comm_rank(comm,&newrank);

    if (is_old){
	/* odd-rank processes */
	mrc = MPI_Barrier(comm);
	VRFY((mrc==MPI_SUCCESS), "");
    }else{
	/* even-rank processes */
	int sub_mpi_rank;	/* rank in the sub-comm */
	MPI_Comm_rank(comm,&sub_mpi_rank);

	/* setup file access template */
	acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
	VRFY((acc_tpl >= 0), "");

	/* create the file collectively */
	fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
	VRFY((fid >= 0), "H5Fcreate succeeded");

	/* Release file-access template */
	ret=H5Pclose(acc_tpl);
	VRFY((ret >= 0), "");

	/* close the file */
	ret=H5Fclose(fid);
	VRFY((ret >= 0), "");

	/* delete the test file */
	if (sub_mpi_rank == 0){
	    mrc = MPI_File_delete((char *)filename, info);
	    /*VRFY((mrc==MPI_SUCCESS), ""); */
	}
    }
    mrc = MPI_Comm_free(&comm);
    VRFY((mrc==MPI_SUCCESS), "MPI_Comm_free succeeded");
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "final MPI_Barrier succeeded");
}


/*-------------------------------------------------------------------------
 *
 * Function:    test_avoid_truncation
 *
 * Purpose:     Tests the library's method of stored the EOA in the superblock
 *              in order to detect file truncation rather than truncating the 
 *              file to match the EOA on file close, whilst in parallel.
 *
 *              This test verifies that the 'avoid truncate' feature can
 *              be enabled via setting the latest format flag. It also verifies
 *              that file truncation is detected correctly when a file is
 *              closed with its EOA value differs from its EOF.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              March 17, 2011
 *
 *-------------------------------------------------------------------------
 */
void
test_avoid_truncation(void)
{
    int mpi_size, mpi_rank;
    H5F_t * f = NULL;               /* Internal File Pointer */
    int mrc;
    hid_t sid,did,fid;			/* file IDs */
    haddr_t eoa,eof = HADDR_UNDEF; /* End of File/Allocation values */
    hid_t fcpl,acc_tpl;		/* File access properties */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    herr_t ret;			/* generic return value */
    hid_t status;
    hbool_t avoid_truncate;
    int filesize;
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
    const char *filename;

    filename = GetTestParameters();

    if (VERBOSE_MED)
	printf("Avoid Truncate test on file %s\n",
	    filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    int sub_mpi_rank;	/* rank in the sub-comm */
    MPI_Comm_rank(comm,&sub_mpi_rank);

    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* Create a file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    VRFY((fcpl >= 0), "");

    /* Enable 'avoid truncate' feature */
    status = H5Pset_avoid_truncate(fcpl, TRUE);
    VRFY((status >= 0), "");

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,fcpl,acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Get the internal file pointer */
    f = (H5F_t *)H5I_object(fid);
    VRFY((fid != NULL), "");
    
    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    VRFY((sid >= 0), "H5Screate succeeded");

    /* Create dataset */
    did = H5Dcreate2(fid, "Dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT);
    VRFY((did >= 0), "H5Dcreate succeeded");

    /* Determine EOA and EOF values */
    eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER);
    VRFY((eoa != HADDR_UNDEF), "");

    eof = H5FD_get_eof(f->shared->lf);
    VRFY((eof != HADDR_UNDEF), "");

    /* Make sure EOA/EOF are not the same */
    VRFY((eoa != eof),"");

    /* Close the property list, dataset, dataspace, and file */
    status = H5Dclose(did);
    VRFY((status >= 0), "");
    status = H5Sclose(sid);
    VRFY((status >= 0), "");
    status = H5Pclose(fcpl);
    VRFY((status >= 0), "");
    status = H5Fclose(fid);
    VRFY((status >= 0), "");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "");

    /* Get this file's creation properties */
    fcpl = H5Fget_create_plist(fid);
    VRFY((fcpl >= 0), "");

    /* Verify that 'avoid truncate' feature is detected as enabled */
    H5Pget_avoid_truncate(fcpl, &avoid_truncate);
    VRFY((avoid_truncate == 1), "");

    /* Close the fcpl */
    status = H5Pclose(fcpl);
    VRFY((status >= 0), "");

    /* Determine EOA and EOF values */
    eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER);
    VRFY((eoa != HADDR_UNDEF), "");

    eof = H5FD_get_eof(f->shared->lf);
    VRFY((eof != HADDR_UNDEF), "");

    /* Make sure values are not equal */
    VRFY((eoa != eof), "");

    /* close the file */
    status=H5Fclose(fid);
    VRFY((status >= 0), "");

    /* Manually truncate the file, rendering it unreadable by HDF5 */
    status = truncate(filename, (off_t)eof-1);
    VRFY((status == 0),"");    

    /* Try to re-open file: this should fail, as the file has been truncated */
    H5E_BEGIN_TRY {
        fid = H5Fopen(filename, H5F_ACC_RDWR, acc_tpl);
    } H5E_END_TRY;
    VRFY((fid < 0),"");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* Cleanup */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "pre-file removal MPI_Barrier succeeded");
    /* delete the test file */
    if (sub_mpi_rank == 0){
        mrc = MPI_File_delete((char *)filename, info);
    }
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "final MPI_Barrier succeeded");

} /* test_avoid_truncation */

