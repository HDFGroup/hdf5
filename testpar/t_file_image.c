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
 * Parallel tests for file image operations
 */

#include "testphdf5.h"

/* file_image_daisy_chain_test
 *
 * Process zero:
 *
 *	1) Creates a core file with an integer vector data set of 
 * 	   length n (= mpi_size), 
 *
 *	2) Initializes the vector to zero in * location 0, and to -1 
 *         everywhere else.  
 *
 *	3) Flushes the core file, and gets an image of it.  Closes
 *	   the core file.
 *
 *	4) Sends the image to process 1.
 *
 *	5) Awaits receipt on a file image from process n-1.
 *
 *	6) opens the image received from process n-1, verifies that
 *	   it contains a vector of length equal to mpi_size, and 
 *	   that the vector contains (0, 1, 2, ... n-1)
 *
 *	7) closes the core file and exits.
 *
 * Process i (0 < i < n)
 *
 *	1) Await receipt of file image from process (i - 1).
 *
 *	2) Open the image with the core file driver, verify that i
 *	   contains a vector v of length, and that v[j] = j for 
 *	   0 <= j < i, and that v[j] == -1 for i <= j < n
 *
 *	3) Set v[i] = i in the core file.
 *
 *	4) Flush the core file and send it to process (i + 1) % n.
 *
 *	5) close the core file and exit.
 *
 * Test fails on a hang (if an image is not received), or on invalid data.
 *
 *                                               JRM -- 11/28/11
 */
void
file_image_daisy_chain_test(void)
{
    char file_name[1024] = "\0";
    int mpi_size, mpi_rank;
    int mpi_result;
    int i;
    int space_ndims;
    MPI_Status rcvstat;
    int * vector_ptr = NULL;
    hid_t fapl_id = -1;
    hid_t file_id;			/* file IDs */
    hid_t dset_id = -1;
    hid_t dset_type_id = -1;
    hid_t space_id = -1;
    herr_t err;
    hsize_t dims[1];
    void * image_ptr = NULL;
    ssize_t bytes_read;
    ssize_t image_len;
    hbool_t vector_ok = TRUE;
    htri_t tri_result;


    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* setup file name */
    HDsnprintf(file_name, 1024, "file_image_daisy_chain_test_%05d.h5", 
               (int)mpi_rank);

    if(mpi_rank == 0) {
 
	/* 1) Creates a core file with an integer vector data set 
         *    of length mpi_size, 
         */
	fapl_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((fapl_id >= 0), "creating fapl");

	err = H5Pset_fapl_core(fapl_id, (size_t)(64 *1024), FALSE);
        VRFY((err >= 0), "setting core file driver in fapl.");

        file_id = H5Fcreate(file_name, 0, H5P_DEFAULT, fapl_id);
        VRFY((file_id >= 0), "created core file");

        dims[0] = (hsize_t)mpi_size;
	space_id = H5Screate_simple(1, dims, dims);
        VRFY((space_id >= 0), "created data space");

        dset_id = H5Dcreate2(file_id, "v", H5T_NATIVE_INT, space_id,
                             H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((dset_id >= 0), "created data set");
 

	/* 2) Initialize the vector to zero in location 0, and 
         *    to -1 everywhere else.  
         */

	vector_ptr = (int *)HDmalloc((size_t)(mpi_size) * sizeof(int));
        VRFY((vector_ptr != NULL), "allocated in memory representation of vector");

        vector_ptr[0] = 0;
        for(i = 1; i < mpi_size; i++)
            vector_ptr[i] = -1;

	err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                       H5P_DEFAULT, (void *)vector_ptr);
        VRFY((err >= 0), "wrote initial data to vector.");

        HDfree(vector_ptr);
        vector_ptr = NULL;

 
        /* 3) Flush the core file, and get an image of it.  Close
         *    the core file.
         */
	err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
        VRFY((err >= 0), "flushed core file.");

        image_len = H5Fget_file_image(file_id, NULL, (size_t)0);
        VRFY((image_len > 0), "got image file size");

        image_ptr = (void *)HDmalloc((size_t)image_len);
        VRFY(image_ptr != NULL, "allocated file image buffer.");

        bytes_read = H5Fget_file_image(file_id, image_ptr, (size_t)image_len);
        VRFY(bytes_read == image_len, "wrote file into image buffer");

        err = H5Sclose(space_id);
	VRFY((err >= 0), "closed data space.");

	err = H5Dclose(dset_id);
	VRFY((err >= 0), "closed data set.");

	err = H5Fclose(file_id);
	VRFY((err >= 0), "closed core file(1).");

	err = H5Pclose(fapl_id);
	VRFY((err >= 0), "closed fapl(1).");

 
        /* 4) Send the image to process 1. */

        mpi_result = MPI_Ssend((void *)(&image_len), (int)sizeof(ssize_t), 
			       MPI_BYTE, 1, 0, MPI_COMM_WORLD);
	VRFY((mpi_result == MPI_SUCCESS), "sent image size to process 1");

        mpi_result = MPI_Ssend((void *)image_ptr, (int)image_len, 
			       MPI_BYTE, 1, 0, MPI_COMM_WORLD);
	VRFY((mpi_result == MPI_SUCCESS), "sent image to process 1");

        HDfree(image_ptr);
        image_ptr = NULL;
        image_len = 0;


	/* 5) Await receipt on a file image from process n-1. */

	mpi_result = MPI_Recv((void *)(&image_len), (int)sizeof(ssize_t),
                              MPI_BYTE, mpi_size - 1, 0, MPI_COMM_WORLD,
                              &rcvstat);
	VRFY((mpi_result == MPI_SUCCESS), "received image len from process n-1");

        image_ptr = (void *)HDmalloc((size_t)image_len);
        VRFY(image_ptr != NULL, "allocated file image receive buffer.");

	mpi_result = MPI_Recv((void *)image_ptr, (int)image_len,
                              MPI_BYTE, mpi_size - 1, 0, MPI_COMM_WORLD,
                              &rcvstat);
	VRFY((mpi_result == MPI_SUCCESS), \
             "received file image from process n-1");
 
	/* 6) open the image received from process n-1, verify that
         *    it contains a vector of length equal to mpi_size, and 
	 *    that the vector contains (0, 1, 2, ... n-1).
         */
	fapl_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((fapl_id >= 0), "creating fapl");

	err = H5Pset_fapl_core(fapl_id, (size_t)(64 *1024), FALSE);
        VRFY((err >= 0), "setting core file driver in fapl.");

	err = H5Pset_file_image(fapl_id, image_ptr, (size_t)image_len);
        VRFY((err >= 0), "set file image in fapl.");

        file_id = H5Fopen(file_name, H5F_ACC_RDWR, fapl_id);
        VRFY((file_id >= 0), "opened received file image file");

	dset_id = H5Dopen2(file_id, "v", H5P_DEFAULT);
        VRFY((dset_id >= 0), "opened data set");

	dset_type_id = H5Dget_type(dset_id);
        VRFY((dset_type_id >= 0), "obtained data set type");

	tri_result = H5Tequal(dset_type_id, H5T_NATIVE_INT);
        VRFY((tri_result == TRUE), "verified data set type");

	space_id = H5Dget_space(dset_id);
        VRFY((space_id >= 0), "opened data space");

	space_ndims = H5Sget_simple_extent_ndims(space_id);
	VRFY((space_ndims == 1), "verified data space num dims(1)");

	space_ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);
	VRFY((space_ndims == 1), "verified data space num dims(2)");
	VRFY((dims[0] == (hsize_t)mpi_size), "verified data space dims");

	vector_ptr = (int *)HDmalloc((size_t)(mpi_size) * sizeof(int));
        VRFY((vector_ptr != NULL), "allocated in memory rep of vector");

	err = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, 
                      H5P_DEFAULT, (void *)vector_ptr);
        VRFY((err >= 0), "read received vector.");

	vector_ok = TRUE;
	for(i = 0; i < mpi_size; i++)
            if(vector_ptr[i] != i)
                vector_ok = FALSE;
        VRFY((vector_ok), "verified received vector.");
 
        HDfree(vector_ptr);
        vector_ptr = NULL;

	/* 7) closes the core file and exit. */

        err = H5Sclose(space_id);
	VRFY((err >= 0), "closed data space.");

	err = H5Dclose(dset_id);
	VRFY((err >= 0), "closed data set.");

	err = H5Fclose(file_id);
	VRFY((err >= 0), "closed core file(1).");

	err = H5Pclose(fapl_id);
	VRFY((err >= 0), "closed fapl(1).");

        HDfree(image_ptr);
        image_ptr = NULL;
        image_len = 0;
    } else {
        /* 1) Await receipt of file image from process (i - 1). */

	mpi_result = MPI_Recv((void *)(&image_len), (int)sizeof(ssize_t),
                              MPI_BYTE, mpi_rank - 1, 0, MPI_COMM_WORLD,
                              &rcvstat);
	VRFY((mpi_result == MPI_SUCCESS), \
             "received image size from process mpi_rank-1");

        image_ptr = (void *)HDmalloc((size_t)image_len);
        VRFY(image_ptr != NULL, "allocated file image receive buffer.");

	mpi_result = MPI_Recv((void *)image_ptr, (int)image_len,
                              MPI_BYTE, mpi_rank - 1, 0, MPI_COMM_WORLD,
                              &rcvstat);
	VRFY((mpi_result == MPI_SUCCESS), \
             "received file image from process mpi_rank-1");
 
	/* 2) Open the image with the core file driver, verify that it
	 *    contains a vector v of length, and that v[j] = j for 
	 *    0 <= j < i, and that v[j] == -1 for i <= j < n
	 */
	fapl_id = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((fapl_id >= 0), "creating fapl");

	err = H5Pset_fapl_core(fapl_id, (size_t)(64 * 1024), FALSE);
        VRFY((err >= 0), "setting core file driver in fapl.");

	err = H5Pset_file_image(fapl_id, image_ptr, (size_t)image_len);
        VRFY((err >= 0), "set file image in fapl.");

        file_id = H5Fopen(file_name, H5F_ACC_RDWR, fapl_id);
	H5Eprint2(H5P_DEFAULT, stderr);
        VRFY((file_id >= 0), "opened received file image file");

	dset_id = H5Dopen2(file_id, "v", H5P_DEFAULT);
        VRFY((dset_id >= 0), "opened data set");

	dset_type_id = H5Dget_type(dset_id);
        VRFY((dset_type_id >= 0), "obtained data set type");

	tri_result = H5Tequal(dset_type_id, H5T_NATIVE_INT);
        VRFY((tri_result == TRUE), "verified data set type");

	space_id = H5Dget_space(dset_id);
        VRFY((space_id >= 0), "opened data space");

	space_ndims = H5Sget_simple_extent_ndims(space_id);
	VRFY((space_ndims == 1), "verified data space num dims(1)");

	space_ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);
	VRFY((space_ndims == 1), "verified data space num dims(2)");
	VRFY((dims[0] == (hsize_t)mpi_size), "verified data space dims");

	vector_ptr = (int *)HDmalloc((size_t)(mpi_size) * sizeof(int));
        VRFY((vector_ptr != NULL), "allocated in memory rep of vector");

	err = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, 
                       H5P_DEFAULT, (void *)vector_ptr);
        VRFY((err >= 0), "read received vector.");

	vector_ok = TRUE;
	for(i = 0; i < mpi_size; i++){
            if(i < mpi_rank) {
                if(vector_ptr[i] != i)
                    vector_ok = FALSE;
	    } else {
                if(vector_ptr[i] != -1)
                    vector_ok = FALSE;
	    }
        }
        VRFY((vector_ok), "verified received vector.");
 

	/* 3) Set v[i] = i in the core file. */

	vector_ptr[mpi_rank] = mpi_rank;

	err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                       H5P_DEFAULT, (void *)vector_ptr);
        VRFY((err >= 0), "wrote modified data to vector.");

        HDfree(vector_ptr);
        vector_ptr = NULL;

 
	/* 4) Flush the core file and send it to process (mpi_rank + 1) % n. */

	err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
        VRFY((err >= 0), "flushed core file.");

        image_len = H5Fget_file_image(file_id, NULL, (size_t)0);
        VRFY((image_len > 0), "got (possibly modified) image file len");

        image_ptr = (void *)HDrealloc((void *)image_ptr, (size_t)image_len);
        VRFY(image_ptr != NULL, "re-allocated file image buffer.");

        bytes_read = H5Fget_file_image(file_id, image_ptr, (size_t)image_len);
        VRFY(bytes_read == image_len, "wrote file into image buffer");

        mpi_result = MPI_Ssend((void *)(&image_len), (int)sizeof(ssize_t), 
			       MPI_BYTE, (mpi_rank + 1) % mpi_size, 0, 
                               MPI_COMM_WORLD);
	VRFY((mpi_result == MPI_SUCCESS), \
             "sent image size to process (mpi_rank + 1) % mpi_size");

        mpi_result = MPI_Ssend((void *)image_ptr, (int)image_len, 
			       MPI_BYTE, (mpi_rank + 1) % mpi_size, 0, 
                               MPI_COMM_WORLD);
	VRFY((mpi_result == MPI_SUCCESS), \
              "sent image to process (mpi_rank + 1) % mpi_size");

        HDfree(image_ptr);
        image_ptr = NULL;
        image_len = 0;
 
	/* 5) close the core file and exit. */

        err = H5Sclose(space_id);
	VRFY((err >= 0), "closed data space.");

	err = H5Dclose(dset_id);
	VRFY((err >= 0), "closed data set.");

	err = H5Fclose(file_id);
	VRFY((err >= 0), "closed core file(1).");

	err = H5Pclose(fapl_id);
	VRFY((err >= 0), "closed fapl(1).");
    }

    return;

} /* file_image_daisy_chain_test() */

