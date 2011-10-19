/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
   This program will test independant and collective reads and writes between
   selections of different rank that non-the-less are deemed as having the 
   same shape by H5Sselect_shape_same().
 */

#define H5S_PACKAGE             /*suppress error about including H5Spkg   */

/* Define this macro to indicate that the testing APIs should be available */
#define H5S_TESTING


#include "hdf5.h"
#include "H5private.h"
#include "testphdf5.h"
#include "H5Spkg.h"             /* Dataspaces                           */


/* On Lustre (and perhaps other parallel file systems?), we have severe
 * slow downs if two or more processes attempt to access the same file system
 * block.  To minimize this problem, we set alignment in the shape same tests
 * to the default Lustre block size -- which greatly reduces contention in 
 * the chunked dataset case.
 */

#define SHAPE_SAME_TEST_ALIGNMENT	((hsize_t)(4 * 1024 * 1024))


#define PAR_SS_DR_MAX_RANK	5 	/* must update code if this changes */

struct hs_dr_pio_test_vars_t
{
    int		mpi_size;
    int         mpi_rank;
    MPI_Comm    mpi_comm;
    MPI_Info	mpi_info;
    int         test_num;
    int         edge_size;
    int		checker_edge_size;
    int         chunk_edge_size;
    int         small_rank;
    int         large_rank;
    hid_t       dset_type;
    uint32_t  * small_ds_buf_0;
    uint32_t  * small_ds_buf_1;
    uint32_t  * small_ds_buf_2;
    uint32_t  * small_ds_slice_buf;
    uint32_t  * large_ds_buf_0;
    uint32_t  * large_ds_buf_1;
    uint32_t  * large_ds_buf_2;
    uint32_t  * large_ds_slice_buf;
    int         small_ds_offset;
    int         large_ds_offset;
    hid_t       fid;               /* HDF5 file ID */
    hid_t	xfer_plist;
    hid_t       full_mem_small_ds_sid;
    hid_t       full_file_small_ds_sid;
    hid_t       mem_small_ds_sid;
    hid_t       file_small_ds_sid_0;
    hid_t       file_small_ds_sid_1;
    hid_t	small_ds_slice_sid;
    hid_t       full_mem_large_ds_sid;
    hid_t       full_file_large_ds_sid;
    hid_t       mem_large_ds_sid;
    hid_t       file_large_ds_sid_0;
    hid_t       file_large_ds_sid_1;
    hid_t       file_large_ds_process_slice_sid;
    hid_t       mem_large_ds_process_slice_sid;
    hid_t	large_ds_slice_sid;
    hid_t       small_dataset;     /* Dataset ID */
    hid_t       large_dataset;     /* Dataset ID */
    size_t      small_ds_size;
    size_t      small_ds_slice_size;
    size_t      large_ds_size;
    size_t      large_ds_slice_size;
    hsize_t     dims[PAR_SS_DR_MAX_RANK];
    hsize_t     chunk_dims[PAR_SS_DR_MAX_RANK];
    hsize_t     start[PAR_SS_DR_MAX_RANK];
    hsize_t     stride[PAR_SS_DR_MAX_RANK];
    hsize_t     count[PAR_SS_DR_MAX_RANK];
    hsize_t     block[PAR_SS_DR_MAX_RANK];
    hsize_t   * start_ptr;
    hsize_t   * stride_ptr;
    hsize_t   * count_ptr;
    hsize_t   * block_ptr;
    int         skips;
    int	        max_skips;
    int64_t	total_tests;
    int64_t	tests_run;
    int64_t	tests_skipped;
};

/*-------------------------------------------------------------------------
 * Function:	hs_dr_pio_test__setup()
 *
 * Purpose:	Do setup for tests of I/O to/from hyperslab selections of 
 * 		different rank in the parallel case.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 8/9/11
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define CONTIG_HS_DR_PIO_TEST__SETUP__DEBUG 0

static void
hs_dr_pio_test__setup(const int test_num,
                      const int edge_size,
                      const int checker_edge_size,
                      const int chunk_edge_size,
                      const int small_rank,
                      const int large_rank,
                      const hbool_t use_collective_io,
                      const hid_t dset_type,
                      const int express_test,
                      struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CONTIG_HS_DR_PIO_TEST__SETUP__DEBUG 
    const char *fcnName = "hs_dr_pio_test__setup()";
#endif /* CONTIG_HS_DR_PIO_TEST__SETUP__DEBUG */
    const char *filename;
    hbool_t	use_gpfs = FALSE;   /* Use GPFS hints */
    hbool_t	mis_match = FALSE;
    int		i;
    int         mrc;
    int		mpi_rank; /* needed by the VRFY macro */
    uint32_t	expected_value;
    uint32_t  * ptr_0;
    uint32_t  * ptr_1;
    hid_t	acc_tpl;		/* File access templates */
    hid_t       small_ds_dcpl_id = H5P_DEFAULT;
    hid_t       large_ds_dcpl_id = H5P_DEFAULT;
    herr_t	ret;		/* Generic return value */

    HDassert( edge_size >= 6 );
    HDassert( edge_size >= chunk_edge_size );
    HDassert( ( chunk_edge_size == 0 ) || ( chunk_edge_size >= 3 ) );
    HDassert( 1 < small_rank );
    HDassert( small_rank < large_rank );
    HDassert( large_rank <= PAR_SS_DR_MAX_RANK );

    tv_ptr->test_num = test_num;
    tv_ptr->edge_size = edge_size;
    tv_ptr->checker_edge_size = checker_edge_size;
    tv_ptr->chunk_edge_size = chunk_edge_size;
    tv_ptr->small_rank = small_rank;
    tv_ptr->large_rank = large_rank;
    tv_ptr->dset_type = dset_type;

    MPI_Comm_size(MPI_COMM_WORLD, &(tv_ptr->mpi_size));
    MPI_Comm_rank(MPI_COMM_WORLD, &(tv_ptr->mpi_rank));
    /* the VRFY() macro needs the local variable mpi_rank -- set it up now */
    mpi_rank = tv_ptr->mpi_rank;

    HDassert( tv_ptr->mpi_size >= 1 );

    tv_ptr->mpi_comm = MPI_COMM_WORLD;
    tv_ptr->mpi_info = MPI_INFO_NULL;

    for ( i = 0; i < tv_ptr->small_rank - 1; i++ )
    {
        tv_ptr->small_ds_size *= (size_t)(tv_ptr->edge_size);
        tv_ptr->small_ds_slice_size *= (size_t)(tv_ptr->edge_size);
    }
    tv_ptr->small_ds_size *= (size_t)(tv_ptr->mpi_size + 1);

    /* used by checker board tests only */
    tv_ptr->small_ds_offset = PAR_SS_DR_MAX_RANK - tv_ptr->small_rank;

    HDassert( 0 < tv_ptr->small_ds_offset );
    HDassert( tv_ptr->small_ds_offset < PAR_SS_DR_MAX_RANK );

    for ( i = 0; i < tv_ptr->large_rank - 1; i++ ) {

        tv_ptr->large_ds_size *= (size_t)(tv_ptr->edge_size);
        tv_ptr->large_ds_slice_size *= (size_t)(tv_ptr->edge_size);
    }
    tv_ptr->large_ds_size *= (size_t)(tv_ptr->mpi_size + 1);

    /* used by checker board tests only */
    tv_ptr->large_ds_offset = PAR_SS_DR_MAX_RANK - tv_ptr->large_rank;

    HDassert( 0 <= tv_ptr->large_ds_offset );
    HDassert( tv_ptr->large_ds_offset < PAR_SS_DR_MAX_RANK );


    /* set up the start, stride, count, and block pointers */
    /* used by contiguous tests only */
    tv_ptr->start_ptr  = &(tv_ptr->start[PAR_SS_DR_MAX_RANK - tv_ptr->large_rank]);
    tv_ptr->stride_ptr = &(tv_ptr->stride[PAR_SS_DR_MAX_RANK - tv_ptr->large_rank]);
    tv_ptr->count_ptr  = &(tv_ptr->count[PAR_SS_DR_MAX_RANK - tv_ptr->large_rank]);
    tv_ptr->block_ptr  = &(tv_ptr->block[PAR_SS_DR_MAX_RANK - tv_ptr->large_rank]);


    /* Allocate buffers */
    tv_ptr->small_ds_buf_0 = (uint32_t *)HDmalloc(sizeof(uint32_t) * tv_ptr->small_ds_size);
    VRFY((tv_ptr->small_ds_buf_0 != NULL), "malloc of small_ds_buf_0 succeeded");

    tv_ptr->small_ds_buf_1 = (uint32_t *)HDmalloc(sizeof(uint32_t) * tv_ptr->small_ds_size);
    VRFY((tv_ptr->small_ds_buf_1 != NULL), "malloc of small_ds_buf_1 succeeded");

    tv_ptr->small_ds_buf_2 = (uint32_t *)HDmalloc(sizeof(uint32_t) * tv_ptr->small_ds_size);
    VRFY((tv_ptr->small_ds_buf_2 != NULL), "malloc of small_ds_buf_2 succeeded");

    tv_ptr->small_ds_slice_buf = 
        (uint32_t *)HDmalloc(sizeof(uint32_t) * tv_ptr->small_ds_slice_size);
    VRFY((tv_ptr->small_ds_slice_buf != NULL), "malloc of small_ds_slice_buf succeeded");

    tv_ptr->large_ds_buf_0 = (uint32_t *)HDmalloc(sizeof(uint32_t) * tv_ptr->large_ds_size);
    VRFY((tv_ptr->large_ds_buf_0 != NULL), "malloc of large_ds_buf_0 succeeded");

    tv_ptr->large_ds_buf_1 = (uint32_t *)HDmalloc(sizeof(uint32_t) * tv_ptr->large_ds_size);
    VRFY((tv_ptr->large_ds_buf_1 != NULL), "malloc of large_ds_buf_1 succeeded");

    tv_ptr->large_ds_buf_2 = (uint32_t *)HDmalloc(sizeof(uint32_t) * tv_ptr->large_ds_size);
    VRFY((tv_ptr->large_ds_buf_2 != NULL), "malloc of large_ds_buf_2 succeeded");

    tv_ptr->large_ds_slice_buf = 
        (uint32_t *)HDmalloc(sizeof(uint32_t) * tv_ptr->large_ds_slice_size);
    VRFY((tv_ptr->large_ds_slice_buf != NULL), "malloc of large_ds_slice_buf succeeded");

    /* initialize the buffers */

    ptr_0 = tv_ptr->small_ds_buf_0;
    for(i = 0; i < (int)(tv_ptr->small_ds_size); i++)
        *ptr_0++ = (uint32_t)i;
    HDmemset(tv_ptr->small_ds_buf_1, 0, sizeof(uint32_t) * tv_ptr->small_ds_size);
    HDmemset(tv_ptr->small_ds_buf_2, 0, sizeof(uint32_t) * tv_ptr->small_ds_size);

    HDmemset(tv_ptr->small_ds_slice_buf, 0, sizeof(uint32_t) * tv_ptr->small_ds_slice_size);

    ptr_0 = tv_ptr->large_ds_buf_0;
    for(i = 0; i < (int)(tv_ptr->large_ds_size); i++)
        *ptr_0++ = (uint32_t)i;
    HDmemset(tv_ptr->large_ds_buf_1, 0, sizeof(uint32_t) * tv_ptr->large_ds_size);
    HDmemset(tv_ptr->large_ds_buf_2, 0, sizeof(uint32_t) * tv_ptr->large_ds_size);

    HDmemset(tv_ptr->large_ds_slice_buf, 0, sizeof(uint32_t) * tv_ptr->large_ds_slice_size);

    filename = (const char *)GetTestParameters();
    HDassert( filename != NULL );
#if CONTIG_HS_DR_PIO_TEST__SETUP__DEBUG 
    if ( MAINPROCESS ) {

        HDfprintf(stdout, "%d: test num = %d.\n", tv_ptr->mpi_rank, tv_ptr->test_num);
        HDfprintf(stdout, "%d: mpi_size = %d.\n", tv_ptr->mpi_rank, tv_ptr->mpi_size);
        HDfprintf(stdout, 
                  "%d: small/large rank = %d/%d, use_collective_io = %d.\n",
                  tv_ptr->mpi_rank, tv_ptr->small_rank, tv_ptr->large_rank, 
                  (int)use_collective_io);
        HDfprintf(stdout, "%d: edge_size = %d, chunk_edge_size = %d.\n",
                  tv_ptr->mpi_rank, tv_ptr->edge_size, tv_ptr->chunk_edge_size);
        HDfprintf(stdout, "%d: checker_edge_size = %d.\n",
                  tv_ptr->mpi_rank, tv_ptr->checker_edge_size);
        HDfprintf(stdout, "%d: small_ds_size = %d, large_ds_size = %d.\n",
                  tv_ptr->mpi_rank, (int)(tv_ptr->small_ds_size), 
                  (int)(tv_ptr->large_ds_size));
        HDfprintf(stdout, "%d: filename = %s.\n", tv_ptr->mpi_rank, filename);
    }
#endif /* CONTIG_HS_DR_PIO_TEST__SETUP__DEBUG */
    /* ----------------------------------------
     * CREATE AN HDF5 FILE WITH PARALLEL ACCESS
     * ---------------------------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(tv_ptr->mpi_comm, tv_ptr->mpi_info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "create_faccess_plist() succeeded");

    /* set the alignment -- need it large so that we aren't always hitting the
     * the same file system block.  Do this only if express_test is greater
     * than zero.
     */
    if ( express_test > 0 ) {

        ret = H5Pset_alignment(acc_tpl, (hsize_t)0, SHAPE_SAME_TEST_ALIGNMENT);
        VRFY((ret != FAIL), "H5Pset_alignment() succeeded");
    }

    /* create the file collectively */
    tv_ptr->fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((tv_ptr->fid >= 0), "H5Fcreate succeeded");

    MESG("File opened.");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "H5Pclose(acc_tpl) succeeded");


    /* setup dims: */
    tv_ptr->dims[0] = (hsize_t)(tv_ptr->mpi_size + 1);
    tv_ptr->dims[1] = tv_ptr->dims[2] = 
        tv_ptr->dims[3] = tv_ptr->dims[4] = (hsize_t)(tv_ptr->edge_size);


    /* Create small ds dataspaces */
    tv_ptr->full_mem_small_ds_sid = 
        H5Screate_simple(tv_ptr->small_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->full_mem_small_ds_sid != 0), 
         "H5Screate_simple() full_mem_small_ds_sid succeeded");

    tv_ptr->full_file_small_ds_sid = 
        H5Screate_simple(tv_ptr->small_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->full_file_small_ds_sid != 0), 
         "H5Screate_simple() full_file_small_ds_sid succeeded");

    tv_ptr->mem_small_ds_sid = H5Screate_simple(tv_ptr->small_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->mem_small_ds_sid != 0), 
         "H5Screate_simple() mem_small_ds_sid succeeded");

    tv_ptr->file_small_ds_sid_0 = H5Screate_simple(tv_ptr->small_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->file_small_ds_sid_0 != 0), 
         "H5Screate_simple() file_small_ds_sid_0 succeeded");

    /* used by checker board tests only */
    tv_ptr->file_small_ds_sid_1 = H5Screate_simple(tv_ptr->small_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->file_small_ds_sid_1 != 0), 
         "H5Screate_simple() file_small_ds_sid_1 succeeded");

    tv_ptr->small_ds_slice_sid = 
        H5Screate_simple(tv_ptr->small_rank - 1, &(tv_ptr->dims[1]), NULL);
    VRFY((tv_ptr->small_ds_slice_sid != 0), 
         "H5Screate_simple() small_ds_slice_sid succeeded");


    /* Create large ds dataspaces */
    tv_ptr->full_mem_large_ds_sid = 
        H5Screate_simple(tv_ptr->large_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->full_mem_large_ds_sid != 0), 
         "H5Screate_simple() full_mem_large_ds_sid succeeded");

    tv_ptr->full_file_large_ds_sid = 
        H5Screate_simple(tv_ptr->large_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->full_file_large_ds_sid != FAIL), 
         "H5Screate_simple() full_file_large_ds_sid succeeded");

    tv_ptr->mem_large_ds_sid = H5Screate_simple(tv_ptr->large_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->mem_large_ds_sid != FAIL), 
         "H5Screate_simple() mem_large_ds_sid succeeded");

    tv_ptr->file_large_ds_sid_0 = H5Screate_simple(tv_ptr->large_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->file_large_ds_sid_0 != FAIL), 
         "H5Screate_simple() file_large_ds_sid_0 succeeded");

    /* used by checker board tests only */
    tv_ptr->file_large_ds_sid_1 = H5Screate_simple(tv_ptr->large_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->file_large_ds_sid_1 != FAIL), 
         "H5Screate_simple() file_large_ds_sid_1 succeeded");

    tv_ptr->mem_large_ds_process_slice_sid = 
        H5Screate_simple(tv_ptr->large_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->mem_large_ds_process_slice_sid != FAIL), 
         "H5Screate_simple() mem_large_ds_process_slice_sid succeeded");

    tv_ptr->file_large_ds_process_slice_sid = 
        H5Screate_simple(tv_ptr->large_rank, tv_ptr->dims, NULL);
    VRFY((tv_ptr->file_large_ds_process_slice_sid != FAIL), 
         "H5Screate_simple() file_large_ds_process_slice_sid succeeded");


    tv_ptr->large_ds_slice_sid = 
        H5Screate_simple(tv_ptr->large_rank - 1, &(tv_ptr->dims[1]), NULL);
    VRFY((tv_ptr->large_ds_slice_sid != 0), 
         "H5Screate_simple() large_ds_slice_sid succeeded");


    /* if chunk edge size is greater than zero, set up the small and
     * large data set creation property lists to specify chunked
     * datasets.
     */
    if ( tv_ptr->chunk_edge_size > 0 ) {

        /* Under Lustre (and perhaps other parallel file systems?) we get 
         * locking delays when two or more processes attempt to access the 
         * same file system block.
         *
         * To minimize this problem, I have changed chunk_dims[0] 
         * from (mpi_size + 1) to just when any sort of express test is
         * selected.  Given the structure of the test, and assuming we 
         * set the alignment large enough, this avoids the contention 
         * issue by seeing to it that each chunk is only accessed by one 
         * process.
         *
         * One can argue as to whether this is a good thing to do in our 
         * tests, but for now it is necessary if we want the test to complete
         * in a reasonable amount of time.
         *
         *                                         JRM -- 9/16/10
         */
        if ( express_test == 0 ) {

            tv_ptr->chunk_dims[0] = 1;

        } else {

            tv_ptr->chunk_dims[0] = 1;
        }
        tv_ptr->chunk_dims[1] = tv_ptr->chunk_dims[2] = 
                                tv_ptr->chunk_dims[3] = 
                                tv_ptr->chunk_dims[4] = (hsize_t)(tv_ptr->chunk_edge_size);

        small_ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((ret != FAIL), "H5Pcreate() small_ds_dcpl_id succeeded");

        ret = H5Pset_layout(small_ds_dcpl_id, H5D_CHUNKED);
        VRFY((ret != FAIL), "H5Pset_layout() small_ds_dcpl_id succeeded");

        ret = H5Pset_chunk(small_ds_dcpl_id, tv_ptr->small_rank, tv_ptr->chunk_dims);
        VRFY((ret != FAIL), "H5Pset_chunk() small_ds_dcpl_id succeeded");


        large_ds_dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((ret != FAIL), "H5Pcreate() large_ds_dcpl_id succeeded");

        ret = H5Pset_layout(large_ds_dcpl_id, H5D_CHUNKED);
        VRFY((ret != FAIL), "H5Pset_layout() large_ds_dcpl_id succeeded");

        ret = H5Pset_chunk(large_ds_dcpl_id, tv_ptr->large_rank, tv_ptr->chunk_dims);
        VRFY((ret != FAIL), "H5Pset_chunk() large_ds_dcpl_id succeeded");
    }

    /* create the small dataset */
    tv_ptr->small_dataset = H5Dcreate2(tv_ptr->fid, "small_dataset", tv_ptr->dset_type,
                                       tv_ptr->file_small_ds_sid_0, H5P_DEFAULT,
                                       small_ds_dcpl_id, H5P_DEFAULT);
    VRFY((ret != FAIL), "H5Dcreate2() small_dataset succeeded");

    /* create the large dataset */
    tv_ptr->large_dataset = H5Dcreate2(tv_ptr->fid, "large_dataset", tv_ptr->dset_type,
                                       tv_ptr->file_large_ds_sid_0, H5P_DEFAULT,
                                       large_ds_dcpl_id, H5P_DEFAULT);
    VRFY((ret != FAIL), "H5Dcreate2() large_dataset succeeded");


    /* setup xfer property list */
    tv_ptr->xfer_plist = H5Pcreate(H5P_DATASET_XFER);
    VRFY((tv_ptr->xfer_plist >= 0), "H5Pcreate(H5P_DATASET_XFER) succeeded");

    if(use_collective_io) {
        ret = H5Pset_dxpl_mpio(tv_ptr->xfer_plist, H5FD_MPIO_COLLECTIVE);
        VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    }

    /* setup selection to write initial data to the small and large data sets */
    tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_rank);
    tv_ptr->stride[0] = (hsize_t)(2 * (tv_ptr->mpi_size + 1));
    tv_ptr->count[0] = 1;
    tv_ptr->block[0] = 1;

    for ( i = 1; i < tv_ptr->large_rank; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
    }

    /* setup selections for writing initial data to the small data set */
    ret = H5Sselect_hyperslab(tv_ptr->mem_small_ds_sid,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(tv_ptr->file_small_ds_sid_0,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid_0, set) suceeded");

    if ( MAINPROCESS ) { /* add an additional slice to the selections */

        tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_size);

        ret = H5Sselect_hyperslab(tv_ptr->mem_small_ds_sid,
                                  H5S_SELECT_OR,
                                  tv_ptr->start,
                                  tv_ptr->stride,
                                  tv_ptr->count,
                                  tv_ptr->block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(mem_small_ds_sid, or) suceeded");

        ret = H5Sselect_hyperslab(tv_ptr->file_small_ds_sid_0,
                                  H5S_SELECT_OR,
                                  tv_ptr->start,
                                  tv_ptr->stride,
                                  tv_ptr->count,
                                  tv_ptr->block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(file_small_ds_sid_0, or) suceeded");
    }


    /* write the initial value of the small data set to file */
    ret = H5Dwrite(tv_ptr->small_dataset, tv_ptr->dset_type, tv_ptr->mem_small_ds_sid, 
                   tv_ptr->file_small_ds_sid_0, tv_ptr->xfer_plist, tv_ptr->small_ds_buf_0);

    VRFY((ret >= 0), "H5Dwrite() small_dataset initial write succeeded");


    /* sync with the other processes before checking data */
    if ( ! use_collective_io ) {

        mrc = MPI_Barrier(MPI_COMM_WORLD);
        VRFY((mrc==MPI_SUCCESS), "Sync after small dataset writes");
    }

    /* read the small data set back to verify that it contains the 
     * expected data.  Note that each process reads in the entire 
     * data set and verifies it.
     */
    ret = H5Dread(tv_ptr->small_dataset,
                  H5T_NATIVE_UINT32,
                  tv_ptr->full_mem_small_ds_sid,
                  tv_ptr->full_file_small_ds_sid,
                  tv_ptr->xfer_plist,
                  tv_ptr->small_ds_buf_1);
    VRFY((ret >= 0), "H5Dread() small_dataset initial read succeeded");


    /* verify that the correct data was written to the small data set */
    expected_value = 0;
    mis_match = FALSE;
    ptr_1 = tv_ptr->small_ds_buf_1;

    i = 0;
    for ( i = 0; i < (int)(tv_ptr->small_ds_size); i++ ) {

        if ( *ptr_1 != expected_value ) {

            mis_match = TRUE;
        }
        ptr_1++;
        expected_value++;
    }
    VRFY( (mis_match == FALSE), "small ds init data good.");


    /* setup selections for writing initial data to the large data set */

    tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_rank);

    ret = H5Sselect_hyperslab(tv_ptr->mem_large_ds_sid,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_large_ds_sid, set) suceeded");

    ret = H5Sselect_hyperslab(tv_ptr->file_large_ds_sid_0,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_large_ds_sid_0, set) suceeded");
 
    /* In passing, setup the process slice data spaces as well */

    ret = H5Sselect_hyperslab(tv_ptr->mem_large_ds_process_slice_sid,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), 
         "H5Sselect_hyperslab(mem_large_ds_process_slice_sid, set) suceeded");

    ret = H5Sselect_hyperslab(tv_ptr->file_large_ds_process_slice_sid,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), 
         "H5Sselect_hyperslab(file_large_ds_process_slice_sid, set) suceeded");

    if ( MAINPROCESS ) { /* add an additional slice to the selections */

        tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_size);

        ret = H5Sselect_hyperslab(tv_ptr->mem_large_ds_sid,
                                  H5S_SELECT_OR,
                                  tv_ptr->start,
                                  tv_ptr->stride,
                                  tv_ptr->count,
                                  tv_ptr->block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(mem_large_ds_sid, or) suceeded");

        ret = H5Sselect_hyperslab(tv_ptr->file_large_ds_sid_0,
                                  H5S_SELECT_OR,
                                  tv_ptr->start,
                                  tv_ptr->stride,
                                  tv_ptr->count,
                                  tv_ptr->block);
        VRFY((ret>= 0), "H5Sselect_hyperslab(file_large_ds_sid_0, or) suceeded");
    }


    /* write the initial value of the large data set to file */
    ret = H5Dwrite(tv_ptr->large_dataset, tv_ptr->dset_type, 
                   tv_ptr->mem_large_ds_sid, tv_ptr->file_large_ds_sid_0, 
                   tv_ptr->xfer_plist, tv_ptr->large_ds_buf_0);
    if ( ret < 0 ) H5Eprint2(H5E_DEFAULT, stderr);
    VRFY((ret >= 0), "H5Dwrite() large_dataset initial write succeeded");


    /* sync with the other processes before checking data */
    if ( ! use_collective_io ) {

        mrc = MPI_Barrier(MPI_COMM_WORLD);
        VRFY((mrc==MPI_SUCCESS), "Sync after large dataset writes");
    }


    /* read the large data set back to verify that it contains the 
     * expected data.  Note that each process reads in the entire 
     * data set.
     */
    ret = H5Dread(tv_ptr->large_dataset,
                  H5T_NATIVE_UINT32,
                  tv_ptr->full_mem_large_ds_sid,
                  tv_ptr->full_file_large_ds_sid,
                  tv_ptr->xfer_plist,
                  tv_ptr->large_ds_buf_1);
    VRFY((ret >= 0), "H5Dread() large_dataset initial read succeeded");


    /* verify that the correct data was written to the large data set */
    expected_value = 0;
    mis_match = FALSE;
    ptr_1 = tv_ptr->large_ds_buf_1;

    i = 0;
    for ( i = 0; i < (int)(tv_ptr->large_ds_size); i++ ) {

        if ( *ptr_1 != expected_value ) {

            mis_match = TRUE;
        }
        ptr_1++;
        expected_value++;
    }
    VRFY( (mis_match == FALSE), "large ds init data good.");


    /* sync with the other processes before changing data */

    if ( ! use_collective_io ) {

        mrc = MPI_Barrier(MPI_COMM_WORLD);
        VRFY((mrc==MPI_SUCCESS), "Sync initial values check");
    }

    return;

} /* hs_dr_pio_test__setup() */


/*-------------------------------------------------------------------------
 * Function:	hs_dr_pio_test__takedown()
 *
 * Purpose:	Do takedown after tests of I/O to/from hyperslab selections 
 *		of different rank in the parallel case.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/18/09
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define HS_DR_PIO_TEST__TAKEDOWN__DEBUG 0

static void
hs_dr_pio_test__takedown( struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if HS_DR_PIO_TEST__TAKEDOWN__DEBUG 
    const char *fcnName = "hs_dr_pio_test__takedown()";
#endif /* HS_DR_PIO_TEST__TAKEDOWN__DEBUG */
    int		mpi_rank;       /* needed by the VRFY macro */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;

    /* Close property lists */
    if ( tv_ptr->xfer_plist != H5P_DEFAULT ) {
        ret = H5Pclose(tv_ptr->xfer_plist);
        VRFY((ret != FAIL), "H5Pclose(xfer_plist) succeeded");
    }

    /* Close dataspaces */
    ret = H5Sclose(tv_ptr->full_mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_mem_small_ds_sid) succeeded");

    ret = H5Sclose(tv_ptr->full_file_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_file_small_ds_sid) succeeded");

    ret = H5Sclose(tv_ptr->mem_small_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_small_ds_sid) succeeded");

    ret = H5Sclose(tv_ptr->file_small_ds_sid_0);
    VRFY((ret != FAIL), "H5Sclose(file_small_ds_sid_0) succeeded");

    ret = H5Sclose(tv_ptr->file_small_ds_sid_1);
    VRFY((ret != FAIL), "H5Sclose(file_small_ds_sid_1) succeeded");

    ret = H5Sclose(tv_ptr->small_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(small_ds_slice_sid) succeeded");

    ret = H5Sclose(tv_ptr->full_mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_mem_large_ds_sid) succeeded");

    ret = H5Sclose(tv_ptr->full_file_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(full_file_large_ds_sid) succeeded");

    ret = H5Sclose(tv_ptr->mem_large_ds_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_sid) succeeded");

    ret = H5Sclose(tv_ptr->file_large_ds_sid_0);
    VRFY((ret != FAIL), "H5Sclose(file_large_ds_sid_0) succeeded");

    ret = H5Sclose(tv_ptr->file_large_ds_sid_1);
    VRFY((ret != FAIL), "H5Sclose(file_large_ds_sid_1) succeeded");

    ret = H5Sclose(tv_ptr->mem_large_ds_process_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(mem_large_ds_process_slice_sid) succeeded");

    ret = H5Sclose(tv_ptr->file_large_ds_process_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(file_large_ds_process_slice_sid) succeeded");

    ret = H5Sclose(tv_ptr->large_ds_slice_sid);
    VRFY((ret != FAIL), "H5Sclose(large_ds_slice_sid) succeeded");

    /* Close Datasets */
    ret = H5Dclose(tv_ptr->small_dataset);
    VRFY((ret != FAIL), "H5Dclose(small_dataset) succeeded");

    ret = H5Dclose(tv_ptr->large_dataset);
    VRFY((ret != FAIL), "H5Dclose(large_dataset) succeeded");

    /* close the file collectively */
    MESG("about to close file.");
    ret = H5Fclose(tv_ptr->fid);
    VRFY((ret != FAIL), "file close succeeded");

    /* Free memory buffers */

    if ( tv_ptr->small_ds_buf_0 != NULL ) HDfree(tv_ptr->small_ds_buf_0);
    if ( tv_ptr->small_ds_buf_1 != NULL ) HDfree(tv_ptr->small_ds_buf_1);
    if ( tv_ptr->small_ds_buf_2 != NULL ) HDfree(tv_ptr->small_ds_buf_2);
    if ( tv_ptr->small_ds_slice_buf != NULL ) HDfree(tv_ptr->small_ds_slice_buf);

    if ( tv_ptr->large_ds_buf_0 != NULL ) HDfree(tv_ptr->large_ds_buf_0);
    if ( tv_ptr->large_ds_buf_1 != NULL ) HDfree(tv_ptr->large_ds_buf_1);
    if ( tv_ptr->large_ds_buf_2 != NULL ) HDfree(tv_ptr->large_ds_buf_2);
    if ( tv_ptr->large_ds_slice_buf != NULL ) HDfree(tv_ptr->large_ds_slice_buf);

    return;

} /* hs_dr_pio_test__takedown() */


/*-------------------------------------------------------------------------
 * Function:	contig_hs_dr_pio_test__d2m_l2s()
 *
 * Purpose:	Part one of a series of tests of I/O to/from hyperslab 
 *		selections of different rank in the parallel.
 *
 *		Verify that we can read from disk correctly using 
 *		selections of different rank that H5S_select_shape_same() 
 *		views as being of the same shape.
 *
 *              In this function, we test this by reading small_rank - 1 
 *		slices from the on disk large cube, and verifying that the 
 *		data read is correct.  Verify that H5S_select_shape_same() 
 *		returns true on the memory and file selections.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/10/11
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define CONTIG_HS_DR_PIO_TEST__D2M_L2S__DEBUG 0

static void
contig_hs_dr_pio_test__d2m_l2s(struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CONTIG_HS_DR_PIO_TEST__D2M_L2S__DEBUG 
    const char *fcnName = "contig_hs_dr_pio_test__run_test()";
#endif /* CONTIG_HS_DR_PIO_TEST__D2M_L2S__DEBUG */
    hbool_t	mis_match = FALSE;
    int		i, j, k, l;
    size_t	n;
    int		mpi_rank; /* needed by the VRFY macro */
    uint32_t	expected_value;
    uint32_t  * ptr_1;
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;


    /* We have already done a H5Sselect_all() on the data space 
     * small_ds_slice_sid in the initialization phase, so no need to 
     * call H5Sselect_all() again.
     */

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (tv_ptr->small_rank - 1) ) {

            tv_ptr->block[i] = 1;

        } else {

            tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
        }
    }

    /* zero out the buffer we will be reading into */
    HDmemset(tv_ptr->small_ds_slice_buf, 0, sizeof(uint32_t) * tv_ptr->small_ds_slice_size);

#if CONTIG_HS_DR_PIO_TEST__D2M_L2S__DEBUG 
    HDfprintf(stdout, 
              "%s reading slices from big cube on disk into small cube slice.\n",
              fcnName);
#endif /* CONTIG_HS_DR_PIO_TEST__D2M_L2S__DEBUG */

    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set.  However, in the parallel version, each 
     * process only works with that slice of the large cube indicated
     * by its rank -- hence we set the most slowly changing index to 
     * mpi_rank, and don't itterate over it.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 0 ) {

        i = tv_ptr->mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 1 ) {

        j = tv_ptr->mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 2 ) {

            k = tv_ptr->mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                if ( (tv_ptr->skips)++ < tv_ptr->max_skips ) { /* skip the test */

 		    (tv_ptr->tests_skipped)++;

                } else { /* run the test */

                    tv_ptr->skips = 0; /* reset the skips counter */

                    /* we know that small_rank - 1 >= 1 and that 
                     * large_rank > small_rank by the assertions at the head 
                     * of this function.  Thus no need for another inner loop.
                     */
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;

                    ret = H5Sselect_hyperslab(tv_ptr->file_large_ds_sid_0,
                                              H5S_SELECT_SET,
                                              tv_ptr->start_ptr,
                                              tv_ptr->stride_ptr,
                                              tv_ptr->count_ptr,
                                              tv_ptr->block_ptr);
                    VRFY((ret != FAIL), 
                         "H5Sselect_hyperslab(file_large_cube_sid) succeeded");


                    /* verify that H5S_select_shape_same() reports the two
                     * selections as having the same shape.
                     */
                    check = H5S_select_shape_same_test(tv_ptr->small_ds_slice_sid,
                                                       tv_ptr->file_large_ds_sid_0);
                    VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                    /* Read selection from disk */
#if CONTIG_HS_DR_PIO_TEST__D2M_L2S__DEBUG 
                    HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                              fcnName, (int)(tv_ptr->mpi_rank), 
                              (int)(tv_ptr->start[0]), (int)(tv_ptr->start[1]), 
                              (int)(tv_ptr->start[2]), (int)(tv_ptr->start[3]), 
                              (int)(tv_ptr->start[4]));
                    HDfprintf(stdout, "%s slice/file extent dims = %d/%d.\n",
                              fcnName,
                              H5Sget_simple_extent_ndims(tv_ptr->small_ds_slice_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_large_ds_sid_0));
#endif /* CONTIG_HS_DR_PIO_TEST__D2M_L2S__DEBUG */
                    ret = H5Dread(tv_ptr->large_dataset,
                                  H5T_NATIVE_UINT32,
                                  tv_ptr->small_ds_slice_sid,
                                  tv_ptr->file_large_ds_sid_0,
                                  tv_ptr->xfer_plist,
                                  tv_ptr->small_ds_slice_buf);
                    VRFY((ret >= 0), "H5Dread() slice from large ds succeeded.");


                    /* verify that expected data is retrieved */

                    mis_match = FALSE;
                    ptr_1 = tv_ptr->small_ds_slice_buf;
                    expected_value = (uint32_t)(
                        (i * tv_ptr->edge_size * tv_ptr->edge_size * 
                             tv_ptr->edge_size * tv_ptr->edge_size) +
                        (j * tv_ptr->edge_size * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (k * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (l * tv_ptr->edge_size));

                    for ( n = 0; n < tv_ptr->small_ds_slice_size; n++ ) {

                        if ( *ptr_1 != expected_value ) {

                            mis_match = TRUE;
                        }

                        *ptr_1 = 0; /* zero data for next use */

                        ptr_1++;
                        expected_value++;
                    }

                    VRFY((mis_match == FALSE), 
                         "small slice read from large ds data good.");

 		    (tv_ptr->tests_run)++;
                }

                l++;

                (tv_ptr->total_tests)++;

            } while ( ( tv_ptr->large_rank > 2 ) &&
                      ( (tv_ptr->small_rank - 1) <= 1 ) &&
                      ( l < tv_ptr->edge_size ) );
            k++;
        } while ( ( tv_ptr->large_rank > 3 ) &&
                  ( (tv_ptr->small_rank - 1) <= 2 ) &&
                  ( k < tv_ptr->edge_size ) );
        j++;
    } while ( ( tv_ptr->large_rank > 4 ) &&
              ( (tv_ptr->small_rank - 1) <= 3 ) &&
              ( j < tv_ptr->edge_size ) );

    return;

} /* contig_hs_dr_pio_test__d2m_l2s() */


/*-------------------------------------------------------------------------
 * Function:	contig_hs_dr_pio_test__d2m_s2l()
 *
 * Purpose:	Part two of a series of tests of I/O to/from hyperslab 
 *		selections of different rank in the parallel.
 *
 *		Verify that we can read from disk correctly using 
 *		selections of different rank that H5S_select_shape_same() 
 *		views as being of the same shape.
 *
 *		In this function, we test this by reading slices of the 
 *		on disk small data set into slices through the in memory 
 *		large data set, and verify that the correct data (and 
 *		only the correct data) is read.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 8/10/11
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define CONTIG_HS_DR_PIO_TEST__D2M_S2L__DEBUG 0

static void
contig_hs_dr_pio_test__d2m_s2l(struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CONTIG_HS_DR_PIO_TEST__D2M_S2L__DEBUG 
    const char *fcnName = "contig_hs_dr_pio_test__d2m_s2l()";
#endif /* CONTIG_HS_DR_PIO_TEST__D2M_S2L__DEBUG */
    hbool_t	mis_match = FALSE;
    int		i, j, k, l;
    size_t	n;
    int		mpi_rank; /* needed by the VRFY macro */
    size_t      start_index;
    size_t      stop_index;
    uint32_t	expected_value;
    uint32_t  * ptr_1;
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;

    /* Read slices of the on disk small data set into slices 
     * through the in memory large data set, and verify that the correct 
     * data (and only the correct data) is read.
     */

    tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_rank);
    tv_ptr->stride[0] = (hsize_t)(2 * (tv_ptr->mpi_size + 1));
    tv_ptr->count[0] = 1;
    tv_ptr->block[0] = 1;

    for ( i = 1; i < tv_ptr->large_rank; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
    }

    ret = H5Sselect_hyperslab(tv_ptr->file_small_ds_sid_0,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid_0, set) suceeded");


#if CONTIG_HS_DR_PIO_TEST__D2M_S2L__DEBUG 
    HDfprintf(stdout, 
      "%s reading slices of on disk small data set into slices of big data set.\n",
              fcnName);
#endif /* CONTIG_HS_DR_PIO_TEST__D2M_S2L__DEBUG */

    /* zero out the in memory large ds */
    HDmemset(tv_ptr->large_ds_buf_1, 0, sizeof(uint32_t) * tv_ptr->large_ds_size);

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (tv_ptr->small_rank - 1) ) {

            tv_ptr->block[i] = 1;

        } else {

            tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
        }
    }


    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set that don't appear in the small data set.  
     *
     * However, in the parallel version, each process only works with that 
     * slice of the large (and small) data set indicated by its rank -- hence 
     * we set the most slowly changing index to mpi_rank, and don't itterate 
     * over it.
     */


    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 0 ) {

        i = tv_ptr->mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 1 ) {

        j = tv_ptr->mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 2 ) {

            k = tv_ptr->mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                if ( (tv_ptr->skips)++ < tv_ptr->max_skips ) { /* skip the test */

                    (tv_ptr->tests_skipped)++;

                } else { /* run the test */

                    tv_ptr->skips = 0; /* reset the skips counter */

                    /* we know that small_rank >= 1 and that large_rank > small_rank
                     * by the assertions at the head of this function.  Thus no
                     * need for another inner loop.
                     */
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;

                    ret = H5Sselect_hyperslab(tv_ptr->mem_large_ds_sid,
                                              H5S_SELECT_SET,
                                              tv_ptr->start_ptr,
                                              tv_ptr->stride_ptr,
                                              tv_ptr->count_ptr,
                                              tv_ptr->block_ptr);
                    VRFY((ret != FAIL), 
                         "H5Sselect_hyperslab(mem_large_ds_sid) succeeded");


                    /* verify that H5S_select_shape_same() reports the two
                     * selections as having the same shape.
                     */
                    check = H5S_select_shape_same_test(tv_ptr->file_small_ds_sid_0,
                                                       tv_ptr->mem_large_ds_sid);
                    VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                    /* Read selection from disk */
#if CONTIG_HS_DR_PIO_TEST__D2M_S2L__DEBUG 
                    HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                              fcnName, (int)(tv_ptr->mpi_rank), 
                              (int)(tv_ptr->start[0]), (int)(tv_ptr->start[1]), 
                              (int)(tv_ptr->start[2]), (int)(tv_ptr->start[3]), 
                              (int)(tv_ptr->start[4]));
                    HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                              fcnName, tv_ptr->mpi_rank,
                              H5Sget_simple_extent_ndims(tv_ptr->mem_large_ds_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_small_ds_sid_0));
#endif /* CONTIG_HS_DR_PIO_TEST__D2M_S2L__DEBUG */
                    ret = H5Dread(tv_ptr->small_dataset,
                                  H5T_NATIVE_UINT32,
                                  tv_ptr->mem_large_ds_sid,
                                  tv_ptr->file_small_ds_sid_0,
                                  tv_ptr->xfer_plist,
                                  tv_ptr->large_ds_buf_1);
                    VRFY((ret >= 0), "H5Dread() slice from small ds succeeded.");

                    /* verify that the expected data and only the
                     * expected data was read.
                     */
                    ptr_1 = tv_ptr->large_ds_buf_1;
                    expected_value = (uint32_t)
                        ((size_t)(tv_ptr->mpi_rank) * tv_ptr->small_ds_slice_size);
                    start_index = (size_t)(
                        (i * tv_ptr->edge_size * tv_ptr->edge_size * 
                             tv_ptr->edge_size * tv_ptr->edge_size) +
                        (j * tv_ptr->edge_size * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (k * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (l * tv_ptr->edge_size));
                    stop_index = start_index + tv_ptr->small_ds_slice_size - 1;

                    HDassert( start_index < stop_index );
                    HDassert( stop_index <= tv_ptr->large_ds_size );

                    for ( n = 0; n < tv_ptr->large_ds_size; n++ ) {

                        if ( ( n >= start_index ) && ( n <= stop_index ) ) {

                            if ( *ptr_1 != expected_value ) {

                                mis_match = TRUE;
                            }
                            expected_value++;

                        } else {

                            if ( *ptr_1 != 0 ) {

                                mis_match = TRUE;
                            }
                        }
                        /* zero out the value for the next pass */
                        *ptr_1 = 0;

                        ptr_1++;
                    }

                    VRFY((mis_match == FALSE), 
                         "small slice read from large ds data good.");

                    (tv_ptr->tests_run)++;
                }

                l++;

                (tv_ptr->total_tests)++;

            } while ( ( tv_ptr->large_rank > 2 ) &&
                      ( (tv_ptr->small_rank - 1) <= 1 ) &&
                      ( l < tv_ptr->edge_size ) );
            k++;
        } while ( ( tv_ptr->large_rank > 3 ) &&
                  ( (tv_ptr->small_rank - 1) <= 2 ) &&
                  ( k < tv_ptr->edge_size ) );
        j++;
    } while ( ( tv_ptr->large_rank > 4 ) &&
              ( (tv_ptr->small_rank - 1) <= 3 ) &&
              ( j < tv_ptr->edge_size ) );

    return;

} /* contig_hs_dr_pio_test__d2m_s2l() */


/*-------------------------------------------------------------------------
 * Function:	contig_hs_dr_pio_test__m2d_l2s()
 *
 * Purpose:	Part three of a series of tests of I/O to/from hyperslab 
 *		selections of different rank in the parallel.
 *
 *		Verify that we can write from memory to file using 
 *		selections of different rank that H5S_select_shape_same() 
 *		views as being of the same shape.
 *
 *		Do this by writing small_rank - 1 dimensional slices from 
 *		the in memory large data set to the on disk small cube 
 *		dataset.  After each write, read the slice of the small 
 *		dataset back from disk, and verify that it contains 
 *              the expected data. Verify that H5S_select_shape_same() 
 *		returns true on the memory and file selections.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 8/10/11
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define CONTIG_HS_DR_PIO_TEST__M2D_L2S__DEBUG 0

static void
contig_hs_dr_pio_test__m2d_l2s(struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CONTIG_HS_DR_PIO_TEST__M2D_L2S__DEBUG 
    const char *fcnName = "contig_hs_dr_pio_test__m2d_l2s()";
#endif /* CONTIG_HS_DR_PIO_TEST__M2D_L2S__DEBUG */
    hbool_t	mis_match = FALSE;
    int		i, j, k, l;
    size_t	n;
    int		mpi_rank; /* needed by the VRFY macro */
    size_t      start_index;
    size_t      stop_index;
    uint32_t	expected_value;
    uint32_t  * ptr_1;
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;


    /* now we go in the opposite direction, verifying that we can write
     * from memory to file using selections of different rank that
     * H5S_select_shape_same() views as being of the same shape.
     *
     * Start by writing small_rank - 1 dimensional slices from the in memory large 
     * data set to the on disk small cube dataset.  After each write, read the 
     * slice of the small dataset back from disk, and verify that it contains 
     * the expected data. Verify that H5S_select_shape_same() returns true on 
     * the memory and file selections.
     */

    tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_rank);
    tv_ptr->stride[0] = (hsize_t)(2 * (tv_ptr->mpi_size + 1));
    tv_ptr->count[0] = 1;
    tv_ptr->block[0] = 1;

    for ( i = 1; i < tv_ptr->large_rank; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
    }

    ret = H5Sselect_hyperslab(tv_ptr->file_small_ds_sid_0,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid_0, set) suceeded");

    ret = H5Sselect_hyperslab(tv_ptr->mem_small_ds_sid,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");


    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (tv_ptr->small_rank - 1) ) {

            tv_ptr->block[i] = 1;

        } else {

            tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
        }
    }

    /* zero out the in memory small ds */
    HDmemset(tv_ptr->small_ds_buf_1, 0, sizeof(uint32_t) * tv_ptr->small_ds_size);


#if CONTIG_HS_DR_PIO_TEST__M2D_L2S__DEBUG 
    HDfprintf(stdout, 
              "%s writing slices from big ds to slices of small ds on disk.\n",
              fcnName);
#endif /* CONTIG_HS_DR_PIO_TEST__M2D_L2S__DEBUG */

    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set that don't appear in the small data set.  
     *
     * However, in the parallel version, each process only works with that 
     * slice of the large (and small) data set indicated by its rank -- hence 
     * we set the most slowly changing index to mpi_rank, and don't itterate 
     * over it.
     */


    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 0 ) {

        i = tv_ptr->mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 1 ) {

        j = tv_ptr->mpi_rank;

    } else {

        j = 0;
    }

    j = 0;
    do {
        if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 2 ) {

            k = tv_ptr->mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                if ( (tv_ptr->skips)++ < tv_ptr->max_skips ) { /* skip the test */

                    (tv_ptr->tests_skipped)++;

                } else { /* run the test */

                    tv_ptr->skips = 0; /* reset the skips counter */

                    /* we know that small_rank >= 1 and that large_rank > small_rank
                     * by the assertions at the head of this function.  Thus no
                     * need for another inner loop.
                     */

                    /* zero out this rank's slice of the on disk small data set */
                    ret = H5Dwrite(tv_ptr->small_dataset,
                                   H5T_NATIVE_UINT32,
                                   tv_ptr->mem_small_ds_sid,
                                   tv_ptr->file_small_ds_sid_0,
                                   tv_ptr->xfer_plist,
                                   tv_ptr->small_ds_buf_2);
                    VRFY((ret >= 0), "H5Dwrite() zero slice to small ds succeeded.");

                    /* select the portion of the in memory large cube from which we
                     * are going to write data.
                     */
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;

                    ret = H5Sselect_hyperslab(tv_ptr->mem_large_ds_sid,
                                              H5S_SELECT_SET,
                                              tv_ptr->start_ptr,
                                              tv_ptr->stride_ptr,
                                              tv_ptr->count_ptr,
                                              tv_ptr->block_ptr);
                    VRFY((ret >= 0), 
                         "H5Sselect_hyperslab() mem_large_ds_sid succeeded.");


                    /* verify that H5S_select_shape_same() reports the in
                     * memory slice through the cube selection and the
                     * on disk full square selections as having the same shape.
                     */
                    check = H5S_select_shape_same_test(tv_ptr->file_small_ds_sid_0,
                                                       tv_ptr->mem_large_ds_sid);
                    VRFY((check == TRUE), "H5S_select_shape_same_test passed.");


                    /* write the slice from the in memory large data set to the 
                     * slice of the on disk small dataset. */
#if CONTIG_HS_DR_PIO_TEST__M2D_L2S__DEBUG 
                    HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                              fcnName, (int)(tv_ptr->mpi_rank),
                              (int)(tv_ptr->start[0]), (int)(tv_ptr->start[1]), 
                              (int)(tv_ptr->start[2]), (int)(tv_ptr->start[3]), 
                              (int)(tv_ptr->start[4]));
                    HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                              fcnName, tv_ptr->mpi_rank,
                              H5Sget_simple_extent_ndims(tv_ptr->mem_large_ds_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_small_ds_sid_0));
#endif /* CONTIG_HS_DR_PIO_TEST__M2D_L2S__DEBUG */
                    ret = H5Dwrite(tv_ptr->small_dataset,
                                   H5T_NATIVE_UINT32,
                                   tv_ptr->mem_large_ds_sid,
                                   tv_ptr->file_small_ds_sid_0,
                                   tv_ptr->xfer_plist,
                                   tv_ptr->large_ds_buf_0);
                    VRFY((ret >= 0), "H5Dwrite() slice to large ds succeeded.");


                    /* read the on disk square into memory */
                    ret = H5Dread(tv_ptr->small_dataset,
                                  H5T_NATIVE_UINT32,
                                  tv_ptr->mem_small_ds_sid,
                                  tv_ptr->file_small_ds_sid_0,
                                  tv_ptr->xfer_plist,
                                  tv_ptr->small_ds_buf_1);
                    VRFY((ret >= 0), "H5Dread() slice from small ds succeeded.");


                    /* verify that expected data is retrieved */

                    mis_match = FALSE;
                    ptr_1 = tv_ptr->small_ds_buf_1;

                    expected_value = (uint32_t)(
                        (i * tv_ptr->edge_size * tv_ptr->edge_size * 
                             tv_ptr->edge_size * tv_ptr->edge_size) +
                        (j * tv_ptr->edge_size * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (k * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (l * tv_ptr->edge_size));

                    start_index = (size_t)(tv_ptr->mpi_rank) * tv_ptr->small_ds_slice_size;
                    stop_index = start_index + tv_ptr->small_ds_slice_size - 1;

                    HDassert( start_index < stop_index );
                    HDassert( stop_index <= tv_ptr->small_ds_size );

                    for ( n = 0; n < tv_ptr->small_ds_size; n++ ) {

                        if ( ( n >= start_index ) && ( n <= stop_index ) ) {

                            if ( *ptr_1 != expected_value ) {

                                mis_match = TRUE;
                            }
                            expected_value++;

                        } else {

                            if ( *ptr_1 != 0 ) {

                                mis_match = TRUE;
                            }
                        }
                        /* zero out the value for the next pass */
                        *ptr_1 = 0;

                        ptr_1++;
                    }

                    VRFY((mis_match == FALSE), 
                         "small slice write from large ds data good.");

                    (tv_ptr->tests_run)++;
                }

                l++;

               (tv_ptr->total_tests)++;

            } while ( ( tv_ptr->large_rank > 2 ) &&
                      ( (tv_ptr->small_rank - 1) <= 1 ) &&
                      ( l < tv_ptr->edge_size ) );
            k++;
        } while ( ( tv_ptr->large_rank > 3 ) &&
                  ( (tv_ptr->small_rank - 1) <= 2 ) &&
                  ( k < tv_ptr->edge_size ) );
        j++;
    } while ( ( tv_ptr->large_rank > 4 ) &&
              ( (tv_ptr->small_rank - 1) <= 3 ) &&
              ( j < tv_ptr->edge_size ) );

    return;

} /* contig_hs_dr_pio_test__m2d_l2s() */


/*-------------------------------------------------------------------------
 * Function:	contig_hs_dr_pio_test__m2d_s2l()
 *
 * Purpose:	Part four of a series of tests of I/O to/from hyperslab 
 *		selections of different rank in the parallel.
 *
 *		Verify that we can write from memory to file using 
 *		selections of different rank that H5S_select_shape_same() 
 *		views as being of the same shape.
 *
 *		Do this by writing the contents of the process's slice of 
 *		the in memory small data set to slices of the on disk 
 *		large data set.  After each write, read the process's 
 *		slice of the large data set back into memory, and verify 
 *		that it contains the expected data. 
 *
 *		Verify that H5S_select_shape_same() returns true on the 
 *		memory and file selections.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 8/10/11
 *
 * Modifications:
 *
 *		None
 *
 *-------------------------------------------------------------------------
 */

#define CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG 0

static void
contig_hs_dr_pio_test__m2d_s2l(struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG 
    const char *fcnName = "contig_hs_dr_pio_test__m2d_s2l()";
#endif /* CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG */
    hbool_t	mis_match = FALSE;
    int		i, j, k, l;
    size_t	n;
    int		mpi_rank; /* needed by the VRFY macro */
    size_t      start_index;
    size_t      stop_index;
    uint32_t	expected_value;
    uint32_t  * ptr_1;
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;

    /* Now write the contents of the process's slice of the in memory 
     * small data set to slices of the on disk large data set.  After 
     * each write, read the process's slice of the large data set back
     * into memory, and verify that it contains the expected data. 
     * Verify that H5S_select_shape_same() returns true on the memory 
     * and file selections.
     */

    /* select the slice of the in memory small data set associated with 
     * the process's mpi rank.
     */
    tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_rank);
    tv_ptr->stride[0] = (hsize_t)(2 * (tv_ptr->mpi_size + 1));
    tv_ptr->count[0] = 1;
    tv_ptr->block[0] = 1;

    for ( i = 1; i < tv_ptr->large_rank; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
    }

    ret = H5Sselect_hyperslab(tv_ptr->mem_small_ds_sid,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");


    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to write slices of the small data set to
     * slices of the large data set.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (tv_ptr->small_rank - 1) ) {

            tv_ptr->block[i] = 1;

        } else {

            tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
        }
    }

    /* zero out the in memory large ds */
    HDmemset(tv_ptr->large_ds_buf_1, 0, sizeof(uint32_t) * tv_ptr->large_ds_size);

#if CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG 
    HDfprintf(stdout, 
         "%s writing process slices of small ds to slices of large ds on disk.\n",
         fcnName);
#endif /* CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 0 ) {

        i = tv_ptr->mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 1 ) {

        j = tv_ptr->mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 2 ) {

            k = tv_ptr->mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                if ( (tv_ptr->skips)++ < tv_ptr->max_skips ) { /* skip the test */

                    (tv_ptr->tests_skipped)++;

#if CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG 
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;

                    HDfprintf(stdout, 
                              "%s:%d: skipping test with start = %d %d %d %d %d.\n", 
                              fcnName, (int)(tv_ptr->mpi_rank),
                              (int)(tv_ptr->start[0]), (int)(tv_ptr->start[1]), 
                              (int)(tv_ptr->start[2]), (int)(tv_ptr->start[3]), 
                              (int)(tv_ptr->start[4]));
                    HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                              fcnName, tv_ptr->mpi_rank,
                              H5Sget_simple_extent_ndims(tv_ptr->mem_small_ds_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_large_ds_sid_0));
#endif /* CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG */
                } else { /* run the test */

                    tv_ptr->skips = 0; /* reset the skips counter */

                    /* we know that small_rank >= 1 and that large_rank > small_rank
                     * by the assertions at the head of this function.  Thus no
                     * need for another inner loop.
                     */

                    /* Zero out this processes slice of the on disk large data set.
                     * Note that this will leave one slice with its original data
                     * as there is one more slice than processes.
                     */
                    ret = H5Dwrite(tv_ptr->large_dataset,
                                   H5T_NATIVE_UINT32,
                                   tv_ptr->large_ds_slice_sid,
                                   tv_ptr->file_large_ds_process_slice_sid,
                                   tv_ptr->xfer_plist,
                                   tv_ptr->large_ds_buf_2);
                    VRFY((ret != FAIL), "H5Dwrite() to zero large ds suceeded");


                    /* select the portion of the in memory large cube to which we
                     * are going to write data.
                     */
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;

                    ret = H5Sselect_hyperslab(tv_ptr->file_large_ds_sid_0,
                                              H5S_SELECT_SET,
                                              tv_ptr->start_ptr,
                                              tv_ptr->stride_ptr,
                                              tv_ptr->count_ptr,
                                              tv_ptr->block_ptr);
                    VRFY((ret != FAIL), 
                         "H5Sselect_hyperslab() target large ds slice succeeded");


                    /* verify that H5S_select_shape_same() reports the in
                     * memory small data set slice selection and the
                     * on disk slice through the large data set selection
                     * as having the same shape.
                     */
                    check = H5S_select_shape_same_test(tv_ptr->mem_small_ds_sid,
                                                       tv_ptr->file_large_ds_sid_0);
                    VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                    /* write the small data set slice from memory to the 
                     * target slice of the disk data set 
                     */
#if CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG 
                    HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                              fcnName, (int)(tv_ptr->mpi_rank),
                              (int)(tv_ptr->start[0]), (int)(tv_ptr->start[1]), 
                              (int)(tv_ptr->start[2]), (int)(tv_ptr->start[3]), 
                              (int)(tv_ptr->start[4]));
                    HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                              fcnName, tv_ptr->mpi_rank,
                              H5Sget_simple_extent_ndims(tv_ptr->mem_small_ds_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_large_ds_sid_0));
#endif /* CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG */
                    ret = H5Dwrite(tv_ptr->large_dataset,
                                   H5T_NATIVE_UINT32,
                                   tv_ptr->mem_small_ds_sid,
                                   tv_ptr->file_large_ds_sid_0,
                                   tv_ptr->xfer_plist,
                                   tv_ptr->small_ds_buf_0);
                    VRFY((ret != FAIL), 
                          "H5Dwrite of small ds slice to large ds succeeded");


                    /* read this processes slice on the on disk large 
                     * data set into memory.
                     */

                    ret = H5Dread(tv_ptr->large_dataset,
                                  H5T_NATIVE_UINT32,
                                  tv_ptr->mem_large_ds_process_slice_sid,
                                  tv_ptr->file_large_ds_process_slice_sid,
                                  tv_ptr->xfer_plist,
                                  tv_ptr->large_ds_buf_1);
                    VRFY((ret != FAIL), 
                         "H5Dread() of process slice of large ds succeeded");


                    /* verify that the expected data and only the
                     * expected data was read.
                     */
                    ptr_1 = tv_ptr->large_ds_buf_1;
                    expected_value = (uint32_t)
                	    ((size_t)(tv_ptr->mpi_rank) * tv_ptr->small_ds_slice_size);

                    start_index = (size_t)
                                  ((i * tv_ptr->edge_size * tv_ptr->edge_size * 
                                        tv_ptr->edge_size * tv_ptr->edge_size) +
                                   (j * tv_ptr->edge_size * tv_ptr->edge_size * 
                                        tv_ptr->edge_size) +
                                   (k * tv_ptr->edge_size * tv_ptr->edge_size) +
                                   (l * tv_ptr->edge_size));
                    stop_index = start_index + tv_ptr->small_ds_slice_size - 1;

                    HDassert( start_index < stop_index );
                    HDassert( stop_index < tv_ptr->large_ds_size );

                    for ( n = 0; n < tv_ptr->large_ds_size; n++ ) {

                        if ( ( n >= start_index ) && ( n <= stop_index ) ) {

                            if ( *ptr_1 != expected_value ) {

                                mis_match = TRUE;
                            }

                            expected_value++;

                        } else {

                            if ( *ptr_1 != 0 ) {

                                mis_match = TRUE;
                            }
                        }
                        /* zero out buffer for next test */
                        *ptr_1 = 0;
                        ptr_1++;
                    }

                    VRFY((mis_match == FALSE), 
                         "small ds slice write to large ds slice data good.");

                    (tv_ptr->tests_run)++;
                }

                l++;

                (tv_ptr->total_tests)++;

            } while ( ( tv_ptr->large_rank > 2 ) &&
                      ( (tv_ptr->small_rank - 1) <= 1 ) &&
                      ( l < tv_ptr->edge_size ) );
            k++;
        } while ( ( tv_ptr->large_rank > 3 ) &&
                  ( (tv_ptr->small_rank - 1) <= 2 ) &&
                  ( k < tv_ptr->edge_size ) );
        j++;
    } while ( ( tv_ptr->large_rank > 4 ) &&
              ( (tv_ptr->small_rank - 1) <= 3 ) &&
              ( j < tv_ptr->edge_size ) );

    return;

} /* contig_hs_dr_pio_test__m2d_s2l() */


/*-------------------------------------------------------------------------
 * Function:	contig_hs_dr_pio_test__run_test()
 *
 * Purpose:	Test I/O to/from hyperslab selections of different rank in
 *		the parallel.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/18/09
 *
 * Modifications:
 *
 *		JRM -- 9/16/10
 *		Added express_test parameter.  Use it to control whether 
 *		we set up the chunks so that no chunk is shared between 
 *		processes, and also whether we set an alignment when we 
 *		create the test file.
 *
 *		JRM -- 8/11/11
 *		Refactored function heavily & broke it into six functions.
 *		Added the skips_ptr, max_skips, total_tests_ptr, 
 *		tests_run_ptr, and tests_skiped_ptr parameters to support 
 *		skipping portions of the test according to the express 
 *		test value.
 *
 *-------------------------------------------------------------------------
 */

#define CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 0

static void
contig_hs_dr_pio_test__run_test(const int test_num,
                                const int edge_size,
                                const int chunk_edge_size,
                                const int small_rank,
                                const int large_rank,
                                const hbool_t use_collective_io,
                                const hid_t dset_type,
                                int express_test,
                                int * skips_ptr,
                                int max_skips,
                                int64_t * total_tests_ptr,
                                int64_t * tests_run_ptr,
                                int64_t * tests_skipped_ptr)
{
#if CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 
    const char *fcnName = "contig_hs_dr_pio_test__run_test()";
#endif /* CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG */
    int		mpi_rank;
    struct hs_dr_pio_test_vars_t test_vars = 
    {
        /* int	       mpi_size                        = */ -1,
        /* int         mpi_rank                        = */ -1,
        /* MPI_Comm    mpi_comm                        = */ MPI_COMM_NULL,
        /* MPI_Inf     mpi_info                        = */ MPI_INFO_NULL,
        /* int         test_num                        = */ -1,
        /* int         edge_size                       = */ -1,
        /* int         checker_edge_size               = */ -1,
        /* int         chunk_edge_size                 = */ -1,
        /* int         small_rank                      = */ -1,
        /* int         large_rank                      = */ -1,
        /* hid_t       dset_type                       = */ -1,
        /* uint32_t  * small_ds_buf_0                  = */ NULL,
        /* uint32_t  * small_ds_buf_1                  = */ NULL,
        /* uint32_t  * small_ds_buf_2                  = */ NULL,
        /* uint32_t  * small_ds_slice_buf              = */ NULL,
        /* uint32_t  * large_ds_buf_0                  = */ NULL,
        /* uint32_t  * large_ds_buf_1                  = */ NULL, 
        /* uint32_t  * large_ds_buf_2                  = */ NULL,
        /* uint32_t  * large_ds_slice_buf              = */ NULL,
        /* int         small_ds_offset                 = */ -1,
        /* int         large_ds_offset                 = */ -1,
        /* hid_t       fid                             = */ -1,  /* HDF5 file ID */
        /* hid_t       xfer_plist                      = */ H5P_DEFAULT,
        /* hid_t       full_mem_small_ds_sid           = */ -1,
        /* hid_t       full_file_small_ds_sid          = */ -1,
        /* hid_t       mem_small_ds_sid                = */ -1,
        /* hid_t       file_small_ds_sid_0             = */ -1,
        /* hid_t       file_small_ds_sid_1             = */ -1,
        /* hid_t       small_ds_slice_sid              = */ -1,
        /* hid_t       full_mem_large_ds_sid           = */ -1,
        /* hid_t       full_file_large_ds_sid          = */ -1,
        /* hid_t       mem_large_ds_sid                = */ -1,
        /* hid_t       file_large_ds_sid_0             = */ -1,
        /* hid_t       file_large_ds_sid_1             = */ -1,
        /* hid_t       file_large_ds_process_slice_sid = */ -1,
        /* hid_t       mem_large_ds_process_slice_sid  = */ -1,
        /* hid_t       large_ds_slice_sid              = */ -1,
        /* hid_t       small_dataset                   = */ -1,     /* Dataset ID */
        /* hid_t       large_dataset                   = */ -1,     /* Dataset ID */
        /* size_t      small_ds_size                   = */ 1,
        /* size_t      small_ds_slice_size             = */ 1,
        /* size_t      large_ds_size                   = */ 1,
        /* size_t      large_ds_slice_size             = */ 1,
        /* hsize_t     dims[PAR_SS_DR_MAX_RANK]        = */ {0,0,0,0,0},
        /* hsize_t     chunk_dims[PAR_SS_DR_MAX_RANK]  = */ {0,0,0,0,0},
        /* hsize_t     start[PAR_SS_DR_MAX_RANK]       = */ {0,0,0,0,0},
        /* hsize_t     stride[PAR_SS_DR_MAX_RANK]      = */ {0,0,0,0,0},
        /* hsize_t     count[PAR_SS_DR_MAX_RANK]       = */ {0,0,0,0,0},
        /* hsize_t     block[PAR_SS_DR_MAX_RANK]       = */ {0,0,0,0,0},
        /* hsize_t   * start_ptr                       = */ NULL,
        /* hsize_t   * stride_ptr                      = */ NULL,
        /* hsize_t   * count_ptr                       = */ NULL,
        /* hsize_t   * block_ptr                       = */ NULL,
        /* int 	       skips                           = */ 0,
        /* int 	       max_skips                       = */ 0,
        /* int64_t     total_tests                     = */ 0,
        /* int64_t     tests_run                       = */ 0,
        /* int64_t     tests_skipped                   = */ 0
    };
    struct hs_dr_pio_test_vars_t * tv_ptr = &test_vars;

    hs_dr_pio_test__setup(test_num, edge_size, -1, chunk_edge_size,
                          small_rank, large_rank, use_collective_io,
                          dset_type, express_test, tv_ptr);

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;

    /* initialize skips & max_skips */
    tv_ptr->skips = *skips_ptr;
    tv_ptr->max_skips = max_skips;

#if CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 
    if ( MAINPROCESS ) {
        HDfprintf(stdout, "test %d: small rank = %d, large rank = %d.\n",
                  test_num, small_rank, large_rank);
        HDfprintf(stdout, "test %d: Initialization complete.\n", test_num);
    }
#endif /* CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG */

    /* first, verify that we can read from disk correctly using selections
     * of different rank that H5S_select_shape_same() views as being of the
     * same shape.
     *
     * Start by reading small_rank - 1 dimensional slice from the on disk 
     * large cube, and verifying that the data read is correct.  Verify that 
     * H5S_select_shape_same() returns true on the memory and file selections.
     */

#if CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 
    if ( MAINPROCESS ) {
        HDfprintf(stdout, "test %d: running contig_hs_dr_pio_test__d2m_l2s.\n", test_num);
    }
#endif /* CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG */
    contig_hs_dr_pio_test__d2m_l2s(tv_ptr);


    /* Second, read slices of the on disk small data set into slices 
     * through the in memory large data set, and verify that the correct 
     * data (and only the correct data) is read.
     */

#if CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 
    if ( MAINPROCESS ) {
        HDfprintf(stdout, "test %d: running contig_hs_dr_pio_test__d2m_s2l.\n", test_num);
    }
#endif /* CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG */
    contig_hs_dr_pio_test__d2m_s2l(tv_ptr);


    /* now we go in the opposite direction, verifying that we can write
     * from memory to file using selections of different rank that
     * H5S_select_shape_same() views as being of the same shape.
     *
     * Start by writing small_rank - 1 D slices from the in memory large data
     * set to the on disk small cube dataset.  After each write, read the 
     * slice of the small dataset back from disk, and verify that it contains 
     * the expected data. Verify that H5S_select_shape_same() returns true on 
     * the memory and file selections.
     */

#if CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 
    if ( MAINPROCESS ) {
        HDfprintf(stdout, "test %d: running contig_hs_dr_pio_test__m2d_l2s.\n", test_num);
    }
#endif /* CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG */
    contig_hs_dr_pio_test__m2d_l2s(tv_ptr);


    /* Now write the contents of the process's slice of the in memory 
     * small data set to slices of the on disk large data set.  After 
     * each write, read the process's slice of the large data set back
     * into memory, and verify that it contains the expected data. 
     * Verify that H5S_select_shape_same() returns true on the memory 
     * and file selections.
     */

#if CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 
    if ( MAINPROCESS ) {
        HDfprintf(stdout, "test %d: running contig_hs_dr_pio_test__m2d_s2l.\n", test_num);
    }
#endif /* CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG */
    contig_hs_dr_pio_test__m2d_s2l(tv_ptr);

#if CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 
    if ( MAINPROCESS ) {
        HDfprintf(stdout, 
            "test %d: Subtests complete -- tests run/skipped/total = %lld/%lld/%lld.\n", 
             test_num, (long long)(tv_ptr->tests_run), (long long)(tv_ptr->tests_skipped),
             (long long)(tv_ptr->total_tests));
    }
#endif /* CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG */

    hs_dr_pio_test__takedown(tv_ptr);

#if CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG 
    if ( MAINPROCESS ) {
        HDfprintf(stdout, "test %d: Takedown complete.\n", test_num);
    }
#endif /* CONTIG_HS_DR_PIO_TEST__RUN_TEST__DEBUG */

    *skips_ptr = tv_ptr->skips;
    *total_tests_ptr += tv_ptr->total_tests;
    *tests_run_ptr += tv_ptr->tests_run;
    *tests_skipped_ptr += tv_ptr->tests_skipped;

    return;

} /* contig_hs_dr_pio_test__run_test() */


/*-------------------------------------------------------------------------
 * Function:	contig_hs_dr_pio_test(ShapeSameTestMethods sstest_type)
 *
 * Purpose:	Test I/O to/from hyperslab selections of different rank in
 *		the parallel case.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/18/09
 *
 * Modifications:
 *
 *  		Modified function to take a sample of the run times
 *		of the different tests, and skip some of them if 
 *		run times are too long.  
 *
 *		We need to do this because Lustre runns very slowly
 *		if two or more processes are banging on the same 
 *		block of memory.
 *						JRM -- 9/10/10
 *              Break this one big test into 4 smaller tests according
 *              to {independent,collective}x{contigous,chunked} datasets.
 *		AKC -- 2010/01/14
 *
 *-------------------------------------------------------------------------
 */

#define CONTIG_HS_DR_PIO_TEST__DEBUG 0

void
contig_hs_dr_pio_test(ShapeSameTestMethods sstest_type)
{
    int         express_test;
    int         local_express_test;
    int         mpi_rank = -1;
    int	        test_num = 0;
    int		edge_size = 10;
    int		chunk_edge_size = 0;
    int	        small_rank;
    int	        large_rank;
    int		mpi_result;
    int		skips = 0;
    int		max_skips = 0;
    /* The following table list the number of sub-tests skipped between 
     * each test that is actually executed as a function of the express 
     * test level.  Note that any value in excess of 4880 will cause all
     * sub tests to be skipped.
     */
    int         max_skips_tbl[4] = {0, 4, 64, 1024};
    hid_t	dset_type = H5T_NATIVE_UINT;
    int64_t	total_tests = 0;
    int64_t	tests_run = 0;
    int64_t     tests_skipped = 0;

    HDcompile_assert(sizeof(uint32_t) == sizeof(unsigned));

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    local_express_test = GetTestExpress();

    mpi_result = MPI_Allreduce((void *)&local_express_test,
                               (void *)&express_test,
                               1,
                               MPI_INT,
                               MPI_MAX,
                               MPI_COMM_WORLD);

    VRFY((mpi_result == MPI_SUCCESS ), "MPI_Allreduce(0) succeeded");

    if ( local_express_test < 0 ) {
        max_skips = max_skips_tbl[0];
    } else if ( local_express_test > 3 ) {
        max_skips = max_skips_tbl[3];
    } else {
        max_skips = max_skips_tbl[local_express_test];
    }

    for ( large_rank = 3; large_rank <= PAR_SS_DR_MAX_RANK; large_rank++ ) {

        for ( small_rank = 2; small_rank < large_rank; small_rank++ ) {

            switch(sstest_type){
                case IND_CONTIG:
                    /* contiguous data set, independent I/O */
                    chunk_edge_size = 0;

                    contig_hs_dr_pio_test__run_test(test_num,
                                                   edge_size,
                                                   chunk_edge_size,
                                                   small_rank,
                                                   large_rank,
                                                   FALSE,  
                                                   dset_type,
                                                   express_test,
                                                   &skips,
                                                   max_skips,
                                                   &total_tests,
                                                   &tests_run,
                                                   &tests_skipped);
                    test_num++;
                    break;
                    /* end of case IND_CONTIG */

                case COL_CONTIG:
                    /* contiguous data set, collective I/O */
                    chunk_edge_size = 0;

                    contig_hs_dr_pio_test__run_test(test_num,
                                                   edge_size,
                                                   chunk_edge_size,
                                                   small_rank,
                                                   large_rank,
                                                   TRUE,  
                                                   dset_type,
                                                   express_test,
                                                   &skips,
                                                   max_skips,
                                                   &total_tests,
                                                   &tests_run,
                                                   &tests_skipped);
                    test_num++;
                    break;
                    /* end of case COL_CONTIG */

                case IND_CHUNKED:
                    /* chunked data set, independent I/O */
                    chunk_edge_size = 5;

                    contig_hs_dr_pio_test__run_test(test_num,
                                                   edge_size,
                                                   chunk_edge_size,
                                                   small_rank,
                                                   large_rank,
                                                   FALSE,  
                                                   dset_type,
                                                   express_test,
                                                   &skips,
                                                   max_skips,
                                                   &total_tests,
                                                   &tests_run,
                                                   &tests_skipped);
                    test_num++;
                    break;
                    /* end of case IND_CHUNKED */

                case COL_CHUNKED:
                    /* chunked data set, collective I/O */
                    chunk_edge_size = 5;

                    contig_hs_dr_pio_test__run_test(test_num,
                                                   edge_size,
                                                   chunk_edge_size,
                                                   small_rank,
                                                   large_rank,
                                                   TRUE,  
                                                   dset_type,
                                                   express_test,
                                                   &skips,
                                                   max_skips,
                                                   &total_tests,
                                                   &tests_run,
                                                   &tests_skipped);
                    test_num++;
                    break;
                    /* end of case COL_CHUNKED */

                default:
                    VRFY((FALSE), "unknown test type");
                    break;

            } /* end of switch(sstest_type) */
#if CONTIG_HS_DR_PIO_TEST__DEBUG
            if ( ( MAINPROCESS ) && ( tests_skipped > 0 ) ) {
                HDfprintf(stdout, "	run/skipped/total = %lld/%lld/%lld.\n",
                          tests_run, tests_skipped, total_tests);
            }
#endif /* CONTIG_HS_DR_PIO_TEST__DEBUG */
        }
    }

    if ( ( MAINPROCESS ) && ( tests_skipped > 0 ) ) {
        HDfprintf(stdout, "	%lld of %lld subtests skipped to expedite testing.\n",
                  tests_skipped, total_tests);
    }

    return;

} /* contig_hs_dr_pio_test() */


/****************************************************************
**
**  ckrbrd_hs_dr_pio_test__slct_ckrbrd():  
**	Given a data space of tgt_rank, and dimensions:
**
**		(mpi_size + 1), edge_size, ... , edge_size
**
**	edge_size, and a checker_edge_size, select a checker
**	board selection of a sel_rank (sel_rank < tgt_rank) 
**	dimensional slice through the data space parallel to the 
**      sel_rank fastest changing indicies, with origin (in the
**	higher indicies) as indicated by the start array.
**
**	Note that this function, like all its relatives, is
**	hard coded to presume a maximum data space rank of 5.
**	While this maximum is declared as a constant, increasing
**	it will require extensive coding in addition to changing
**      the value of the constant.
**
**					JRM -- 10/8/09
**
****************************************************************/

#define CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 0

static void
ckrbrd_hs_dr_pio_test__slct_ckrbrd(const int mpi_rank,
                                   const hid_t tgt_sid,
                                   const int tgt_rank,
                                   const int edge_size,
                                   const int checker_edge_size,
                                   const int sel_rank,
                                   hsize_t sel_start[])
{
#if CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
    const char *	fcnName = "ckrbrd_hs_dr_pio_test__slct_ckrbrd():";
#endif 
    hbool_t		first_selection = TRUE;
    int                 i, j, k, l, m;
    int			n_cube_offset;
    int			sel_offset;
    const int		test_max_rank = PAR_SS_DR_MAX_RANK;  /* must update code if */
                                                             /* this changes        */
    hsize_t		base_count;
    hsize_t             offset_count;
    hsize_t     	start[PAR_SS_DR_MAX_RANK];
    hsize_t     	stride[PAR_SS_DR_MAX_RANK];
    hsize_t     	count[PAR_SS_DR_MAX_RANK];
    hsize_t     	block[PAR_SS_DR_MAX_RANK];
    herr_t      	ret;            /* Generic return value */

    HDassert( edge_size >= 6 );
    HDassert( 0 < checker_edge_size );
    HDassert( checker_edge_size <= edge_size );
    HDassert( 0 < sel_rank );
    HDassert( sel_rank <= tgt_rank );
    HDassert( tgt_rank <= test_max_rank );
    HDassert( test_max_rank <= PAR_SS_DR_MAX_RANK );

    sel_offset = test_max_rank - sel_rank;
    HDassert( sel_offset >= 0 );

    n_cube_offset = test_max_rank - tgt_rank;
    HDassert( n_cube_offset >= 0 );
    HDassert( n_cube_offset <= sel_offset );

#if CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
    HDfprintf(stdout, "%s:%d: edge_size/checker_edge_size = %d/%d\n",
              fcnName, mpi_rank, edge_size, checker_edge_size);
    HDfprintf(stdout, "%s:%d: sel_rank/sel_offset = %d/%d.\n", 
              fcnName, mpi_rank, sel_rank, sel_offset);
    HDfprintf(stdout, "%s:%d: tgt_rank/n_cube_offset = %d/%d.\n", 
              fcnName, mpi_rank, tgt_rank, n_cube_offset);
#endif /* CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG */ 

    /* First, compute the base count (which assumes start == 0
     * for the associated offset) and offset_count (which
     * assumes start == checker_edge_size for the associated
     * offset).
     *
     * Note that the following computation depends on the C99
     * requirement that integer division discard any fraction
     * (truncation towards zero) to function correctly. As we
     * now require C99, this shouldn't be a problem, but noting
     * it may save us some pain if we are ever obliged to support
     * pre-C99 compilers again.
     */

    base_count = (hsize_t)(edge_size / (checker_edge_size * 2));

    if ( (edge_size % (checker_edge_size * 2)) > 0 ) {

        base_count++;
    }

    offset_count = (hsize_t)((edge_size - checker_edge_size) / (checker_edge_size * 2));

    if ( ((edge_size - checker_edge_size) % (checker_edge_size * 2)) > 0 ) {

        offset_count++;
    }

    /* Now set up the stride and block arrays, and portions of the start
     * and count arrays that will not be altered during the selection of 
     * the checker board.
     */
    i = 0;
    while ( i < n_cube_offset ) {

        /* these values should never be used */
        start[i] = 0;
        stride[i] = 0;
        count[i] = 0;
        block[i] = 0;

        i++;
    }

    while ( i < sel_offset ) {

        start[i] = sel_start[i];
        stride[i] = (hsize_t)(2 * edge_size);
        count[i] = 1;
        block[i] = 1;

        i++;
    }

    while ( i < test_max_rank ) {

        stride[i] = (hsize_t)(2 * checker_edge_size);
        block[i] = (hsize_t)checker_edge_size;

        i++;
    }
   
    i = 0;
    do {
        if ( 0 >= sel_offset ) {

            if ( i == 0 ) {

                start[0] = 0;
                count[0] = base_count;

            } else {

                start[0] = (hsize_t)checker_edge_size;
                count[0] = offset_count;

            }
        }

        j = 0;
        do { 
            if ( 1 >= sel_offset ) {

                if ( j == 0 ) {

                    start[1] = 0;
                    count[1] = base_count;

                } else {

                    start[1] = (hsize_t)checker_edge_size;
                    count[1] = offset_count;

                }
            }

            k = 0;
            do {
                if ( 2 >= sel_offset ) {

                    if ( k == 0 ) {

                        start[2] = 0;
                        count[2] = base_count;

                    } else {

                        start[2] = (hsize_t)checker_edge_size;
                        count[2] = offset_count;

                    }
                }

                l = 0;
                do {
                    if ( 3 >= sel_offset ) {

                        if ( l == 0 ) {

                            start[3] = 0;
                            count[3] = base_count;

                        } else {

                            start[3] = (hsize_t)checker_edge_size;
                            count[3] = offset_count;

                        }
                    }

                    m = 0;
                    do {
                        if ( 4 >= sel_offset ) {

                            if ( m == 0 ) {

                                start[4] = 0;
                                count[4] = base_count;

                            } else {

                                start[4] = (hsize_t)checker_edge_size;
                                count[4] = offset_count;

                            }
                        }

                        if ( ((i + j + k + l + m) % 2) == 0 ) {

#if CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
                            HDfprintf(stdout, "%s%d: *** first_selection = %d ***\n", 
                                      fcnName, mpi_rank, (int)first_selection);
                            HDfprintf(stdout, "%s:%d: i/j/k/l/m = %d/%d/%d/%d/%d\n",
                                      fcnName, mpi_rank, i, j, k, l, m);
                            HDfprintf(stdout, 
                                      "%s:%d: start = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, (int)start[0], (int)start[1], 
                                      (int)start[2], (int)start[3], (int)start[4]);
                            HDfprintf(stdout, 
                                      "%s:%d: stride = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, (int)stride[0], (int)stride[1], 
                                      (int)stride[2], (int)stride[3], (int)stride[4]);
                            HDfprintf(stdout, 
                                      "%s:%d: count = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, (int)count[0], (int)count[1], 
                                      (int)count[2], (int)count[3], (int)count[4]);
                            HDfprintf(stdout, 
                                      "%s:%d: block = %d %d %d %d %d.\n", 
                                      fcnName, mpi_rank, (int)block[0], (int)block[1], 
                                      (int)block[2], (int)block[3], (int)block[4]);
                            HDfprintf(stdout, "%s:%d: n-cube extent dims = %d.\n", 
                                      fcnName, mpi_rank,
                                      H5Sget_simple_extent_ndims(tgt_sid));
                            HDfprintf(stdout, "%s:%d: selection rank = %d.\n", 
                                      fcnName, mpi_rank, sel_rank);
#endif

                            if ( first_selection ) {

                                first_selection = FALSE; 

                                ret = H5Sselect_hyperslab
                                      (
                                        tgt_sid, 
                                        H5S_SELECT_SET,
                                        &(start[n_cube_offset]), 
                                        &(stride[n_cube_offset]), 
                                        &(count[n_cube_offset]), 
                                        &(block[n_cube_offset])
                                      );
    
                                VRFY((ret != FAIL), "H5Sselect_hyperslab(SET) succeeded");

                            } else {

                                ret = H5Sselect_hyperslab
                                      (
                                        tgt_sid, 
                                        H5S_SELECT_OR,
                                        &(start[n_cube_offset]), 
                                        &(stride[n_cube_offset]), 
                                        &(count[n_cube_offset]), 
                                        &(block[n_cube_offset])
                                      );
    
                                VRFY((ret != FAIL), "H5Sselect_hyperslab(OR) succeeded");

                            }
                        }

                        m++;

                    } while ( ( m <= 1 ) &&
                              ( 4 >= sel_offset ) );

                    l++;

                } while ( ( l <= 1 ) &&
                          ( 3 >= sel_offset ) );

                k++;

            } while ( ( k <= 1 ) &&
                      ( 2 >= sel_offset ) );

            j++;

        } while ( ( j <= 1 ) &&
                  ( 1 >= sel_offset ) );


        i++;

    } while ( ( i <= 1 ) &&
              ( 0 >= sel_offset ) );

#if CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
    HDfprintf(stdout, "%s%d: H5Sget_select_npoints(tgt_sid) = %d.\n",
              fcnName, mpi_rank, (int)H5Sget_select_npoints(tgt_sid));
#endif /* CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG */

    /* Clip the selection back to the data space proper. */

    for ( i = 0; i < test_max_rank; i++ ) {

        start[i]  = 0;
        stride[i] = (hsize_t)edge_size;
        count[i]  = 1;
        block[i]  = (hsize_t)edge_size;
    }

    ret = H5Sselect_hyperslab(tgt_sid, H5S_SELECT_AND,
                              start, stride, count, block);

    VRFY((ret != FAIL), "H5Sselect_hyperslab(AND) succeeded");

#if CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG 
    HDfprintf(stdout, "%s%d: H5Sget_select_npoints(tgt_sid) = %d.\n",
              fcnName, mpi_rank, (int)H5Sget_select_npoints(tgt_sid));
    HDfprintf(stdout, "%s%d: done.\n", fcnName, mpi_rank);
#endif /* CKRBRD_HS_DR_PIO_TEST__SELECT_CHECKER_BOARD__DEBUG */

    return;

} /* ckrbrd_hs_dr_pio_test__slct_ckrbrd() */


/****************************************************************
**
**  ckrbrd_hs_dr_pio_test__verify_data(): 
**
**	Examine the supplied buffer to see if it contains the 
**	expected data.  Return TRUE if it does, and FALSE 
**      otherwise.
**
**	The supplied buffer is presumed to this process's slice 
**	of the target data set.  Each such slice will be an
**	n-cube of rank (rank -1) and the supplied edge_size with
**	origin (mpi_rank, 0, ... , 0) in the target data set.
**
**	Further, the buffer is presumed to be the result of reading
**	or writing a checker board selection of an m (1 <= m < 
**      rank) dimensional slice through this processes slice
**	of the target data set.  Also, this slice must be parallel
**	to the fastest changing indicies.  
**
**	It is further presumed that the buffer was zeroed before
**	the read/write, and that the full target data set (i.e.
**	the buffer/data set for all processes) was initialized
**      with the natural numbers listed in order from the origin 
**	along the fastest changing axis.
**
**      Thus for a 20x10x10 dataset, the value stored in location
**	(x, y, z) (assuming that z is the fastest changing index
**	and x the slowest) is assumed to be:
**
**		(10 * 10 * x) + (10 * y) + z
**
**	Further, supposing that this is process 10, this process's 
**	slice of the dataset would be a 10 x 10 2-cube with origin
**	(10, 0, 0) in the data set, and would be initialize (prior
**	to the checkerboard selection) as follows:
**
**		1000, 1001, 1002, ... 1008, 1009
**		1010, 1011, 1012, ... 1018, 1019
**		  .     .     .         .     .
**		  .     .     .         .     .
**		  .     .     .         .     .
**		1090, 1091, 1092, ... 1098, 1099
**
**	In the case of a read from the processors slice of another
**	data set of different rank, the values expected will have
**	to be adjusted accordingly.  This is done via the 
**	first_expected_val parameter.
**
**	Finally, the function presumes that the first element 
**	of the buffer resides either at the origin of either
**	a selected or an unselected checker.  (Translation:
**	if partial checkers appear in the buffer, they will
**	intersect the edges of the n-cube oposite the origin.)
**
****************************************************************/

#define CKRBRD_HS_DR_PIO_TEST__VERIFY_DATA__DEBUG 0

static hbool_t
ckrbrd_hs_dr_pio_test__verify_data(uint32_t * buf_ptr,
                                   const int rank,
                                   const int edge_size,
                                   const int checker_edge_size,
                                   uint32_t first_expected_val,
                                   hbool_t buf_starts_in_checker)
{
#if CKRBRD_HS_DR_PIO_TEST__VERIFY_DATA__DEBUG
    const char *	fcnName = "ckrbrd_hs_dr_pio_test__verify_data():";
#endif
    hbool_t good_data = TRUE;
    hbool_t in_checker;
    hbool_t start_in_checker[5];
    uint32_t expected_value;
    uint32_t * val_ptr;
    int i, j, k, l, m;  /* to track position in n-cube */
    int v, w, x, y, z;  /* to track position in checker */
    const int test_max_rank = 5; /* code changes needed if this is increased */

    HDassert( buf_ptr != NULL );
    HDassert( 0 < rank );
    HDassert( rank <= test_max_rank );
    HDassert( edge_size >= 6 );
    HDassert( 0 < checker_edge_size );
    HDassert( checker_edge_size <= edge_size );
    HDassert( test_max_rank <= PAR_SS_DR_MAX_RANK );

#if CKRBRD_HS_DR_PIO_TEST__VERIFY_DATA__DEBUG 

    int		mpi_rank;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    HDfprintf(stdout, "%s mpi_rank = %d.\n", fcnName, mpi_rank);
    HDfprintf(stdout, "%s rank = %d.\n", fcnName, rank);
    HDfprintf(stdout, "%s edge_size = %d.\n", fcnName, edge_size);
    HDfprintf(stdout, "%s checker_edge_size = %d.\n", fcnName, checker_edge_size);
    HDfprintf(stdout, "%s first_expected_val = %d.\n", fcnName, (int)first_expected_val);
    HDfprintf(stdout, "%s starts_in_checker = %d.\n", fcnName, (int)buf_starts_in_checker);
}
#endif

    val_ptr = buf_ptr;
    expected_value = first_expected_val;

    i = 0;
    v = 0;
    start_in_checker[0] = buf_starts_in_checker;
    do
    {
        if ( v >= checker_edge_size ) {

            start_in_checker[0] = ! start_in_checker[0];
            v = 0;
        }

        j = 0;
        w = 0;
        start_in_checker[1] = start_in_checker[0];
        do
        {
            if ( w >= checker_edge_size ) {

                start_in_checker[1] = ! start_in_checker[1];
                w = 0;
            }

            k = 0;
            x = 0;
            start_in_checker[2] = start_in_checker[1];
            do
            {
                if ( x >= checker_edge_size ) {

                    start_in_checker[2] = ! start_in_checker[2];
                    x = 0;
                }

                l = 0;
                y = 0;
                start_in_checker[3] = start_in_checker[2];
                do
                { 
                    if ( y >= checker_edge_size ) {

                        start_in_checker[3] = ! start_in_checker[3];
                        y = 0;
                    }

                    m = 0;
                    z = 0;
#if CKRBRD_HS_DR_PIO_TEST__VERIFY_DATA__DEBUG 
                    HDfprintf(stdout, "%d, %d, %d, %d, %d:", i, j, k, l, m);
#endif
                    in_checker = start_in_checker[3];
                    do
                    {
#if CKRBRD_HS_DR_PIO_TEST__VERIFY_DATA__DEBUG 
                        HDfprintf(stdout, " %d", (int)(*val_ptr));
#endif
                        if ( z >= checker_edge_size ) {

                            in_checker = ! in_checker;
                            z = 0;
                        }
         
                        if ( in_checker ) {
                   
                            if ( *val_ptr != expected_value ) {

                                good_data = FALSE;
                            }
 
                            /* zero out buffer for re-use */
                            *val_ptr = 0;

                        } else if ( *val_ptr != 0 ) {

                            good_data = FALSE;
 
                            /* zero out buffer for re-use */
                            *val_ptr = 0;

                        }

                        val_ptr++;
                        expected_value++;
                        m++;
                        z++;
 
                    } while ( ( rank >= (test_max_rank - 4) ) &&
                              ( m < edge_size ) );
#if CKRBRD_HS_DR_PIO_TEST__VERIFY_DATA__DEBUG 
                    HDfprintf(stdout, "\n");
#endif
                    l++;
                    y++;
                } while ( ( rank >= (test_max_rank - 3) ) &&
                          ( l < edge_size ) );
                k++;
                x++;
            } while ( ( rank >= (test_max_rank - 2) ) &&
                      ( k < edge_size ) );
            j++;
            w++;
        } while ( ( rank >= (test_max_rank - 1) ) &&
                  ( j < edge_size ) );
        i++;
        v++;
    } while ( ( rank >= test_max_rank ) &&
              ( i < edge_size ) );

    return(good_data);

} /* ckrbrd_hs_dr_pio_test__verify_data() */


/*-------------------------------------------------------------------------
 * Function:	ckrbrd_hs_dr_pio_test__d2m_l2s()
 *
 * Purpose:	Part one of a series of tests of I/O to/from hyperslab 
 *		selections of different rank in the parallel.
 *
 *		Verify that we can read from disk correctly using checker
 *		board selections of different rank that 
 *              H5S_select_shape_same() views as being of the same shape.
 *
 *              In this function, we test this by reading small_rank - 1 
 *		checker board slices from the on disk large cube, and 
 *		verifying that the data read is correct.  Verify that 
 *		H5S_select_shape_same() returns true on the memory and 
 *		file selections.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/15/11
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG 0

static void
ckrbrd_hs_dr_pio_test__d2m_l2s(struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG 
    const char *fcnName = "ckrbrd_hs_dr_pio_test__d2m_l2s()";
    uint32_t  * ptr_0;
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG */
    hbool_t	data_ok = FALSE;
    int		i, j, k, l;
    uint32_t	expected_value;
    int		mpi_rank; /* needed by VRFY */
    hsize_t     sel_start[PAR_SS_DR_MAX_RANK];
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;


    /* first, verify that we can read from disk correctly using selections
     * of different rank that H5S_select_shape_same() views as being of the
     * same shape.
     *
     * Start by reading a (small_rank - 1)-D checker board slice from this 
     * processes slice of the on disk large data set, and verifying that the 
     * data read is correct.  Verify that H5S_select_shape_same() returns 
     * true on the memory and file selections.
     *
     * The first step is to set up the needed checker board selection in the
     * in memory small small cube
     */

    sel_start[0] = sel_start[1] = sel_start[2] = sel_start[3] = sel_start[4] = 0;
    sel_start[tv_ptr->small_ds_offset] = (hsize_t)(tv_ptr->mpi_rank);

    ckrbrd_hs_dr_pio_test__slct_ckrbrd(tv_ptr->mpi_rank,
                                       tv_ptr->small_ds_slice_sid,
                                       tv_ptr->small_rank - 1,
                                       tv_ptr->edge_size,
                                       tv_ptr->checker_edge_size,
                                       tv_ptr->small_rank - 1,
                                       sel_start);

    /* zero out the buffer we will be reading into */
    HDmemset(tv_ptr->small_ds_slice_buf, 0, sizeof(uint32_t) * tv_ptr->small_ds_slice_size);

#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG 
    HDfprintf(stdout, "%s:%d: initial small_ds_slice_buf = ",
              fcnName, tv_ptr->mpi_rank);
    ptr_0 = tv_ptr->small_ds_slice_buf;
    for ( i = 0; i < (int)(tv_ptr->small_ds_slice_size); i++ ) {
        HDfprintf(stdout, "%d ", (int)(*ptr_0));
        ptr_0++;
    }
    HDfprintf(stdout, "\n");
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG */ 

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (tv_ptr->small_rank - 1) ) {

            tv_ptr->block[i] = 1;

        } else {

            tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
        }
    }

#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG 
    HDfprintf(stdout, 
              "%s:%d: reading slice from big ds on disk into small ds slice.\n",
              fcnName, tv_ptr->mpi_rank);
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG */ 
    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set.  However, in the parallel version, each 
     * process only works with that slice of the large cube indicated
     * by its rank -- hence we set the most slowly changing index to 
     * mpi_rank, and don't itterate over it.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 0 ) {

        i = tv_ptr->mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 1 ) {

        j = tv_ptr->mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 2 ) {

            k = tv_ptr->mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                if ( (tv_ptr->skips)++ < tv_ptr->max_skips ) { /* skip the test */

                    (tv_ptr->tests_skipped)++;

                } else { /* run the test */

                    tv_ptr->skips = 0; /* reset the skips counter */

                    /* we know that small_rank - 1 >= 1 and that 
                     * large_rank > small_rank by the assertions at the head 
                     * of this function.  Thus no need for another inner loop.
                     */
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;

                    HDassert((tv_ptr->start[0] == 0)||(0 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[1] == 0)||(1 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[2] == 0)||(2 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[3] == 0)||(3 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[4] == 0)||(4 < tv_ptr->small_ds_offset + 1));

                    ckrbrd_hs_dr_pio_test__slct_ckrbrd
                    (
                      tv_ptr->mpi_rank,
                      tv_ptr->file_large_ds_sid_0,
                      tv_ptr->large_rank,
                      tv_ptr->edge_size,
                      tv_ptr->checker_edge_size,
                      tv_ptr->small_rank - 1,
                      tv_ptr->start
                    );

                    /* verify that H5S_select_shape_same() reports the two
                     * selections as having the same shape.
                     */
                    check = H5S_select_shape_same_test(tv_ptr->small_ds_slice_sid,
                                                       tv_ptr->file_large_ds_sid_0);
                    VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                    /* Read selection from disk */
#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG 
                    HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", fcnName,
                              tv_ptr->mpi_rank, tv_ptr->start[0], tv_ptr->start[1], 
                              tv_ptr->start[2], tv_ptr->start[3], tv_ptr->start[4]);
                    HDfprintf(stdout, "%s slice/file extent dims = %d/%d.\n",
                              fcnName,
                              H5Sget_simple_extent_ndims(tv_ptr->small_ds_slice_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_large_ds_sid_0));
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG */ 

                    ret = H5Dread(tv_ptr->large_dataset,
                                  H5T_NATIVE_UINT32,
                                  tv_ptr->small_ds_slice_sid,
                                  tv_ptr->file_large_ds_sid_0,
                                  tv_ptr->xfer_plist,
                                  tv_ptr->small_ds_slice_buf);
                    VRFY((ret >= 0), "H5Dread() slice from large ds succeeded.");

#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG 
                    HDfprintf(stdout, "%s:%d: H5Dread() returns.\n", 
                              fcnName, tv_ptr->mpi_rank);
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_L2S__DEBUG */

                    /* verify that expected data is retrieved */

                    expected_value = (uint32_t)
                        ((i * tv_ptr->edge_size * tv_ptr->edge_size * 
                              tv_ptr->edge_size * tv_ptr->edge_size) +
                         (j * tv_ptr->edge_size * tv_ptr->edge_size * tv_ptr->edge_size) +
                         (k * tv_ptr->edge_size * tv_ptr->edge_size) +
                         (l * tv_ptr->edge_size));

                    data_ok = ckrbrd_hs_dr_pio_test__verify_data
                              (
                                tv_ptr->small_ds_slice_buf,
                                tv_ptr->small_rank - 1,
                                tv_ptr->edge_size,
                                tv_ptr->checker_edge_size,
                                expected_value,
                                (hbool_t)TRUE
                              );

                    VRFY((data_ok == TRUE), 
                         "small slice read from large ds data good.");

                    (tv_ptr->tests_run)++;
                }

                l++;

                (tv_ptr->total_tests)++;

            } while ( ( tv_ptr->large_rank > 2 ) &&
                      ( (tv_ptr->small_rank - 1) <= 1 ) &&
                      ( l < tv_ptr->edge_size ) );
            k++;
        } while ( ( tv_ptr->large_rank > 3 ) &&
                  ( (tv_ptr->small_rank - 1) <= 2 ) &&
                  ( k < tv_ptr->edge_size ) );
        j++;
    } while ( ( tv_ptr->large_rank > 4 ) &&
              ( (tv_ptr->small_rank - 1) <= 3 ) &&
              ( j < tv_ptr->edge_size ) );

    return;

} /* ckrbrd_hs_dr_pio_test__d2m_l2s() */


/*-------------------------------------------------------------------------
 * Function:	ckrbrd_hs_dr_pio_test__d2m_s2l()
 *
 * Purpose:	Part two of a series of tests of I/O to/from hyperslab 
 *		selections of different rank in the parallel.
 *
 *		Verify that we can read from disk correctly using 
 *		selections of different rank that H5S_select_shape_same() 
 *		views as being of the same shape.
 *
 *		In this function, we test this by reading checker board
 *		slices of the on disk small data set into slices through 
 *		the in memory large data set, and verify that the correct 
 *		data (and only the correct data) is read.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 8/15/11
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG 0

static void
ckrbrd_hs_dr_pio_test__d2m_s2l(struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG 
    const char *fcnName = "ckrbrd_hs_dr_pio_test__d2m_s2l()";
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG */
    hbool_t	data_ok = FALSE;
    int		i, j, k, l;
    size_t      u;
    size_t      start_index;
    size_t      stop_index;
    uint32_t	expected_value;
    uint32_t  * ptr_1;
    int		mpi_rank; /* needed by VRFY */
    hsize_t     sel_start[PAR_SS_DR_MAX_RANK];
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;


    /* similarly, read slices of the on disk small data set into slices 
     * through the in memory large data set, and verify that the correct 
     * data (and only the correct data) is read.
     */

    sel_start[0] = sel_start[1] = sel_start[2] = sel_start[3] = sel_start[4] = 0;
    sel_start[tv_ptr->small_ds_offset] = (hsize_t)(tv_ptr->mpi_rank);

    ckrbrd_hs_dr_pio_test__slct_ckrbrd(tv_ptr->mpi_rank,
                                       tv_ptr->file_small_ds_sid_0,
                                       tv_ptr->small_rank,
                                       tv_ptr->edge_size,
                                       tv_ptr->checker_edge_size,
                                       tv_ptr->small_rank - 1,
                                       sel_start);

#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG 
    HDfprintf(stdout, 
      "%s reading slices of on disk small data set into slices of big data set.\n",
              fcnName);
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG */

    /* zero out the buffer we will be reading into */
    HDmemset(tv_ptr->large_ds_buf_1, 0, sizeof(uint32_t) * tv_ptr->large_ds_size);

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read the slice of the small data set
     * into different slices of the process slice of the large data 
     * set.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (tv_ptr->small_rank - 1) ) {

            tv_ptr->block[i] = 1;

        } else {

            tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
        }
    }

    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set that don't appear in the small data set.  
     *
     * However, in the parallel version, each process only works with that 
     * slice of the large (and small) data set indicated by its rank -- hence 
     * we set the most slowly changing index to mpi_rank, and don't itterate 
     * over it.
     */


    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 0 ) {

        i = tv_ptr->mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 1 ) {

        j = tv_ptr->mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 2 ) {

            k = tv_ptr->mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                if ( (tv_ptr->skips)++ < tv_ptr->max_skips ) { /* skip the test */

                    (tv_ptr->tests_skipped)++;

                } else { /* run the test */

                    tv_ptr->skips = 0; /* reset the skips counter */

                    /* we know that small_rank >= 1 and that large_rank > small_rank
                     * by the assertions at the head of this function.  Thus no
                     * need for another inner loop.
                     */
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;

                    HDassert((tv_ptr->start[0] == 0)||(0 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[1] == 0)||(1 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[2] == 0)||(2 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[3] == 0)||(3 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[4] == 0)||(4 < tv_ptr->small_ds_offset + 1));

                    ckrbrd_hs_dr_pio_test__slct_ckrbrd
                    (
                      tv_ptr->mpi_rank,
                      tv_ptr->mem_large_ds_sid,
                      tv_ptr->large_rank,
                      tv_ptr->edge_size,
                      tv_ptr->checker_edge_size,
                      tv_ptr->small_rank - 1,
                      tv_ptr->start
                    );


                    /* verify that H5S_select_shape_same() reports the two
                     * selections as having the same shape.
                     */
                    check = H5S_select_shape_same_test(tv_ptr->file_small_ds_sid_0,
                                                       tv_ptr->mem_large_ds_sid);
                    VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                    /* Read selection from disk */
#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG 
                    HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                              fcnName, tv_ptr->mpi_rank, 
                              tv_ptr->start[0], tv_ptr->start[1], tv_ptr->start[2], 
                              tv_ptr->start[3], tv_ptr->start[4]); 
                    HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                              fcnName, tv_ptr->mpi_rank,
                              H5Sget_simple_extent_ndims(tv_ptr->large_ds_slice_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_small_ds_sid_0));
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG */
                    ret = H5Dread(tv_ptr->small_dataset,
                                  H5T_NATIVE_UINT32,
                                  tv_ptr->mem_large_ds_sid,
                                  tv_ptr->file_small_ds_sid_0,
                                  tv_ptr->xfer_plist,
                                  tv_ptr->large_ds_buf_1);
                    VRFY((ret >= 0), "H5Dread() slice from small ds succeeded.");

                    /* verify that the expected data and only the
                     * expected data was read.
                     */
                    data_ok = TRUE;
                    ptr_1 = tv_ptr->large_ds_buf_1;
                    expected_value = 
                       (uint32_t)((size_t)(tv_ptr->mpi_rank) * tv_ptr->small_ds_slice_size);
                    start_index = (size_t)(
                        (i * tv_ptr->edge_size * tv_ptr->edge_size * 
                             tv_ptr->edge_size * tv_ptr->edge_size) +
                        (j * tv_ptr->edge_size * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (k * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (l * tv_ptr->edge_size));
                    stop_index = start_index + tv_ptr->small_ds_slice_size - 1;

#if CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG 
                    {
                        int m, n;

                        HDfprintf(stdout, "%s:%d: expected_value = %d.\n", 
                                  fcnName, tv_ptr->mpi_rank, expected_value);
                        HDfprintf(stdout, "%s:%d: start/stop index = %d/%d.\n",
                                  fcnName, tv_ptr->mpi_rank, start_index, stop_index);
                        n = 0;
                        for ( m = 0; (unsigned)m < tv_ptr->large_ds_size; m ++ ) {
                            HDfprintf(stdout, "%d ", (int)(*ptr_1));
                            ptr_1++;
                            n++;
                            if ( n >= tv_ptr->edge_size ) {
                                HDfprintf(stdout, "\n");
                                n = 0;
                            }
                        }
                        HDfprintf(stdout, "\n");
                        ptr_1 = tv_ptr->large_ds_buf_1;
                    }
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__D2M_S2L__DEBUG */

                    HDassert( start_index < stop_index );
                    HDassert( stop_index <= tv_ptr->large_ds_size );

                    for ( u = 0; u < start_index; u++ ) {

                        if ( *ptr_1 != 0 ) {

                            data_ok = FALSE;
                        }

                        /* zero out the value for the next pass */
                        *ptr_1 = 0;

                        ptr_1++;
                    }

                    VRFY((data_ok == TRUE), 
                         "slice read from small to large ds data good(1).");

                    data_ok = ckrbrd_hs_dr_pio_test__verify_data
                              (
                                ptr_1,
                                tv_ptr->small_rank - 1,
                                tv_ptr->edge_size,
                                tv_ptr->checker_edge_size,
                                expected_value,
                                (hbool_t)TRUE
                              );

                    VRFY((data_ok == TRUE), 
                         "slice read from small to large ds data good(2).");


                    ptr_1 = tv_ptr->large_ds_buf_1 + stop_index + 1;

                    for ( u = stop_index + 1; u < tv_ptr->large_ds_size; u++ ) {

                        if ( *ptr_1 != 0 ) {

                            data_ok = FALSE;
                        }

                        /* zero out the value for the next pass */
                        *ptr_1 = 0;

                        ptr_1++;
                    }

                    VRFY((data_ok == TRUE), 
                         "slice read from small to large ds data good(3).");

                    (tv_ptr->tests_run)++;
                }

                l++;

                (tv_ptr->total_tests)++;

            } while ( ( tv_ptr->large_rank > 2 ) &&
                      ( (tv_ptr->small_rank - 1) <= 1 ) &&
                      ( l < tv_ptr->edge_size ) );
            k++;
        } while ( ( tv_ptr->large_rank > 3 ) &&
                  ( (tv_ptr->small_rank - 1) <= 2 ) &&
                  ( k < tv_ptr->edge_size ) );
        j++;
    } while ( ( tv_ptr->large_rank > 4 ) &&
              ( (tv_ptr->small_rank - 1) <= 3 ) &&
              ( j < tv_ptr->edge_size ) );

    return;

} /* ckrbrd_hs_dr_pio_test__d2m_s2l() */


/*-------------------------------------------------------------------------
 * Function:	ckrbrd_hs_dr_pio_test__m2d_l2s()
 *
 * Purpose:	Part three of a series of tests of I/O to/from checker
 *		board hyperslab selections of different rank in the 
 *		parallel.
 *
 *		Verify that we can write from memory to file using checker
 *		board selections of different rank that 
 *		H5S_select_shape_same() views as being of the same shape.
 *
 *		Do this by writing small_rank - 1 dimensional checker 
 *		board slices from the in memory large data set to the on 
 *		disk small cube dataset.  After each write, read the 
 *		slice of the small dataset back from disk, and verify 
 *		that it contains the expected data. Verify that 
 *		H5S_select_shape_same() returns true on the memory and 
 *		file selections.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 8/15/11
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define CHECKER_BOARD_HS_DR_PIO_TEST__M2D_L2S__DEBUG 0

static void
ckrbrd_hs_dr_pio_test__m2d_l2s(struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CHECKER_BOARD_HS_DR_PIO_TEST__M2D_L2S__DEBUG 
    const char *fcnName = "ckrbrd_hs_dr_pio_test__m2d_l2s()";
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__M2D_L2S__DEBUG */
    hbool_t	data_ok = FALSE;
    hbool_t	mis_match = FALSE;
    int		i, j, k, l;
    size_t      u;
    size_t      start_index;
    size_t      stop_index;
    uint32_t	expected_value;
    uint32_t  * ptr_1;
    int		mpi_rank; /* needed by VRFY */
    hsize_t     sel_start[PAR_SS_DR_MAX_RANK];
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;


    /* now we go in the opposite direction, verifying that we can write
     * from memory to file using selections of different rank that
     * H5S_select_shape_same() views as being of the same shape.
     *
     * Start by writing small_rank - 1 D slices from the in memory large data
     * set to the on disk small dataset.  After each write, read the slice of 
     * the small dataset back from disk, and verify that it contains the 
     * expected data. Verify that H5S_select_shape_same() returns true on 
     * the memory and file selections.
     */

    tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_rank);
    tv_ptr->stride[0] = (hsize_t)(2 * (tv_ptr->mpi_size + 1));
    tv_ptr->count[0] = 1;
    tv_ptr->block[0] = 1;

    for ( i = 1; i < tv_ptr->large_rank; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
    }

    ret = H5Sselect_hyperslab(tv_ptr->file_small_ds_sid_0,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_small_ds_sid_0, set) suceeded");

    ret = H5Sselect_hyperslab(tv_ptr->mem_small_ds_sid,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(mem_small_ds_sid, set) suceeded");


    sel_start[0] = sel_start[1] = sel_start[2] = sel_start[3] = sel_start[4] = 0;
    sel_start[tv_ptr->small_ds_offset] = (hsize_t)(tv_ptr->mpi_rank);

    ckrbrd_hs_dr_pio_test__slct_ckrbrd(tv_ptr->mpi_rank,
                                       tv_ptr->file_small_ds_sid_1,
                                       tv_ptr->small_rank,
                                       tv_ptr->edge_size,
                                       tv_ptr->checker_edge_size,
                                       tv_ptr->small_rank - 1,
                                       sel_start);


    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to read slices of the large cube.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (tv_ptr->small_rank - 1) ) {

            tv_ptr->block[i] = 1;

        } else {

            tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
        }
    }

    /* zero out the in memory small ds */
    HDmemset(tv_ptr->small_ds_buf_1, 0, sizeof(uint32_t) * tv_ptr->small_ds_size);


#if CHECKER_BOARD_HS_DR_PIO_TEST__M2D_L2S__DEBUG 
    HDfprintf(stdout, 
    "%s writing checker boards selections of slices from big ds to slices of small ds on disk.\n",
    fcnName);
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__M2D_L2S__DEBUG */

    /* in serial versions of this test, we loop through all the dimensions
     * of the large data set that don't appear in the small data set.  
     *
     * However, in the parallel version, each process only works with that 
     * slice of the large (and small) data set indicated by its rank -- hence 
     * we set the most slowly changing index to mpi_rank, and don't itterate 
     * over it.
     */


    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 0 ) {

        i = tv_ptr->mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 1 ) {

        j = tv_ptr->mpi_rank;

    } else {

        j = 0;
    }

    j = 0;
    do {
        if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 2 ) {

            k = tv_ptr->mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                if ( (tv_ptr->skips)++ < tv_ptr->max_skips ) { /* skip the test */

                    (tv_ptr->tests_skipped)++;

                } else { /* run the test */

                    tv_ptr->skips = 0; /* reset the skips counter */

                    /* we know that small_rank >= 1 and that large_rank > small_rank
                     * by the assertions at the head of this function.  Thus no
                     * need for another inner loop.
                     */
    
                    /* zero out this rank's slice of the on disk small data set */
                    ret = H5Dwrite(tv_ptr->small_dataset,
                                   H5T_NATIVE_UINT32,
                                   tv_ptr->mem_small_ds_sid,
                                   tv_ptr->file_small_ds_sid_0,
                                   tv_ptr->xfer_plist,
                                   tv_ptr->small_ds_buf_2);
                    VRFY((ret >= 0), "H5Dwrite() zero slice to small ds succeeded.");
    
                    /* select the portion of the in memory large cube from which we
                     * are going to write data.
                     */
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;
    
                    HDassert((tv_ptr->start[0] == 0)||(0 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[1] == 0)||(1 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[2] == 0)||(2 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[3] == 0)||(3 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[4] == 0)||(4 < tv_ptr->small_ds_offset + 1));
    
                    ckrbrd_hs_dr_pio_test__slct_ckrbrd
                    (
                      tv_ptr->mpi_rank,
                      tv_ptr->mem_large_ds_sid,
                      tv_ptr->large_rank,
                      tv_ptr->edge_size,
                      tv_ptr->checker_edge_size,
                      tv_ptr->small_rank - 1,
                      tv_ptr->start
                    );
    
    
                    /* verify that H5S_select_shape_same() reports the in
                     * memory checkerboard selection of the slice through the 
                     * large dataset and the checkerboard selection of the process
                     * slice of the small data set as having the same shape.
                     */
                    check = H5S_select_shape_same_test(tv_ptr->file_small_ds_sid_1,
                                                       tv_ptr->mem_large_ds_sid);
                    VRFY((check == TRUE), "H5S_select_shape_same_test passed.");
    
    
                    /* write the checker board selection of the slice from the in 
                     * memory large data set to the slice of the on disk small 
                     * dataset. 
                     */
#if CHECKER_BOARD_HS_DR_PIO_TEST__M2D_L2S__DEBUG 
                    HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                              fcnName, tv_ptr->mpi_rank,
                              tv_ptr->start[0], tv_ptr->start[1], tv_ptr->start[2], 
                              tv_ptr->start[3], tv_ptr->start[4]);
                    HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                              fcnName, tv_ptr->mpi_rank,
                              H5Sget_simple_extent_ndims(tv_ptr->mem_large_ds_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_small_ds_sid_1));
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__M2D_L2S__DEBUG */
                    ret = H5Dwrite(tv_ptr->small_dataset,
                                   H5T_NATIVE_UINT32,
                                   tv_ptr->mem_large_ds_sid,
                                   tv_ptr->file_small_ds_sid_1,
                                   tv_ptr->xfer_plist,
                                   tv_ptr->large_ds_buf_0);
                    VRFY((ret >= 0), "H5Dwrite() slice to large ds succeeded.");
    
    
                    /* read the on disk process slice of the small dataset into memory */
                    ret = H5Dread(tv_ptr->small_dataset,
                                  H5T_NATIVE_UINT32,
                                  tv_ptr->mem_small_ds_sid,
                                  tv_ptr->file_small_ds_sid_0,
                                  tv_ptr->xfer_plist,
                                  tv_ptr->small_ds_buf_1);
                    VRFY((ret >= 0), "H5Dread() slice from small ds succeeded.");
    
    
                    /* verify that expected data is retrieved */
    
                    mis_match = FALSE;
    
                    expected_value = (uint32_t)(
    			(i * tv_ptr->edge_size * tv_ptr->edge_size * 
                                 tv_ptr->edge_size * tv_ptr->edge_size) +
                        (j * tv_ptr->edge_size * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (k * tv_ptr->edge_size * tv_ptr->edge_size) +
                        (l * tv_ptr->edge_size));
    
                    start_index = (size_t)(tv_ptr->mpi_rank) * tv_ptr->small_ds_slice_size;
                    stop_index = start_index + tv_ptr->small_ds_slice_size - 1;
    
                    HDassert( start_index < stop_index );
                    HDassert( stop_index <= tv_ptr->small_ds_size );
    
                    data_ok = TRUE;
    
                    ptr_1 = tv_ptr->small_ds_buf_1;
                    for ( u = 0; u < start_index; u++, ptr_1++ ) {
    
                        if ( *ptr_1 != 0 ) {

                            data_ok = FALSE;
                            *ptr_1 = 0;
                        }
                    }

                    data_ok &= ckrbrd_hs_dr_pio_test__verify_data
                               (
                                 tv_ptr->small_ds_buf_1 + start_index,
                                 tv_ptr->small_rank - 1,
                                 tv_ptr->edge_size,
                                 tv_ptr->checker_edge_size,
                                 expected_value,
                                 (hbool_t)TRUE
                               );


                    ptr_1 = tv_ptr->small_ds_buf_1;
                    for ( u = stop_index; u < tv_ptr->small_ds_size; u++, ptr_1++ ) {

                        if ( *ptr_1 != 0 ) {

                            data_ok = FALSE;
                            *ptr_1 = 0;
                        }
                    }

                    VRFY((data_ok == TRUE), 
                         "large slice write slice to small slice data good.");

                    (tv_ptr->tests_run)++;
                }

                l++;

                (tv_ptr->total_tests)++;

            } while ( ( tv_ptr->large_rank > 2 ) &&
                      ( (tv_ptr->small_rank - 1) <= 1 ) &&
                      ( l < tv_ptr->edge_size ) );
            k++;
        } while ( ( tv_ptr->large_rank > 3 ) &&
                  ( (tv_ptr->small_rank - 1) <= 2 ) &&
                  ( k < tv_ptr->edge_size ) );
        j++;
    } while ( ( tv_ptr->large_rank > 4 ) &&
              ( (tv_ptr->small_rank - 1) <= 3 ) &&
              ( j < tv_ptr->edge_size ) );

    return;

} /* ckrbrd_hs_dr_pio_test__m2d_l2s() */


/*-------------------------------------------------------------------------
 * Function:	ckrbrd_hs_dr_pio_test__m2d_s2l()
 *
 * Purpose:	Part four of a series of tests of I/O to/from checker
 *		board hyperslab selections of different rank in the parallel.
 *
 *		Verify that we can write from memory to file using 
 *		selections of different rank that H5S_select_shape_same() 
 *		views as being of the same shape.
 *
 *		Do this by writing checker board selections of the contents 
 *		of the process's slice of the in memory small data set to 
 *		slices of the on disk large data set.  After each write, 
 *		read the process's slice of the large data set back into 
 *		memory, and verify that it contains the expected data. 
 *
 *		Verify that H5S_select_shape_same() returns true on the 
 *		memory and file selections.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 8/15/11
 *
 * Modifications:
 *
 *		None
 *
 *-------------------------------------------------------------------------
 */

#define CHECKER_BOARD_HS_DR_PIO_TEST__M2D_S2L__DEBUG 0

static void
ckrbrd_hs_dr_pio_test__m2d_s2l(struct hs_dr_pio_test_vars_t * tv_ptr)
{
#if CHECKER_BOARD_HS_DR_PIO_TEST__M2D_S2L__DEBUG 
    const char *fcnName = "ckrbrd_hs_dr_pio_test__m2d_s2l()";
#endif /* CONTIG_HS_DR_PIO_TEST__M2D_S2L__DEBUG */
    hbool_t	data_ok = FALSE;
    hbool_t	mis_match = FALSE;
    int		i, j, k, l;
    size_t      u;
    size_t      start_index;
    size_t      stop_index;
    uint32_t	expected_value;
    uint32_t  * ptr_1;
    int		mpi_rank; /* needed by VRFY */
    hsize_t     sel_start[PAR_SS_DR_MAX_RANK];
    htri_t      check;          /* Shape comparison return value */
    herr_t	ret;		/* Generic return value */

    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;


    /* Now write the contents of the process's slice of the in memory 
     * small data set to slices of the on disk large data set.  After 
     * each write, read the process's slice of the large data set back
     * into memory, and verify that it contains the expected data. 
     * Verify that H5S_select_shape_same() returns true on the memory 
     * and file selections.
     */

    tv_ptr->start[0] = (hsize_t)(tv_ptr->mpi_rank);
    tv_ptr->stride[0] = (hsize_t)(2 * (tv_ptr->mpi_size + 1));
    tv_ptr->count[0] = 1;
    tv_ptr->block[0] = 1;

    for ( i = 1; i < tv_ptr->large_rank; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
    }

    ret = H5Sselect_hyperslab(tv_ptr->file_large_ds_sid_0,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(file_large_ds_sid_0, set) suceeded");

    ret = H5Sselect_hyperslab(tv_ptr->mem_large_ds_sid,
                              H5S_SELECT_SET,
                              tv_ptr->start,
                              tv_ptr->stride,
                              tv_ptr->count,
                              tv_ptr->block);
    VRFY((ret >= 0), "H5Sselect_hyperslab(tv_ptr->mem_large_ds_sid, set) suceeded");

    /* setup a checkerboard selection of the slice of the in memory small 
     * data set associated with the process's mpi rank.
     */

    sel_start[0] = sel_start[1] = sel_start[2] = sel_start[3] = sel_start[4] = 0;
    sel_start[tv_ptr->small_ds_offset] = (hsize_t)(tv_ptr->mpi_rank);

    ckrbrd_hs_dr_pio_test__slct_ckrbrd(tv_ptr->mpi_rank,
                                       tv_ptr->mem_small_ds_sid,
                                       tv_ptr->small_rank,
                                       tv_ptr->edge_size,
                                       tv_ptr->checker_edge_size,
                                       tv_ptr->small_rank - 1,
                                       sel_start);

    /* set up start, stride, count, and block -- note that we will
     * change start[] so as to write checkerboard selections of slices 
     * of the small data set to slices of the large data set.
     */
    for ( i = 0; i < PAR_SS_DR_MAX_RANK; i++ ) {

        tv_ptr->start[i] = 0;
        tv_ptr->stride[i] = (hsize_t)(2 * tv_ptr->edge_size);
        tv_ptr->count[i] = 1;
        if ( (PAR_SS_DR_MAX_RANK - i) > (tv_ptr->small_rank - 1) ) {

            tv_ptr->block[i] = 1;

        } else {

            tv_ptr->block[i] = (hsize_t)(tv_ptr->edge_size);
        }
    }

    /* zero out the in memory large ds */
    HDmemset(tv_ptr->large_ds_buf_1, 0, sizeof(uint32_t) * tv_ptr->large_ds_size);

#if CHECKER_BOARD_HS_DR_PIO_TEST__M2D_S2L__DEBUG
    HDfprintf(stdout, 
         "%s writing process checkerboard selections of slices of small ds to process slices of large ds on disk.\n",
         fcnName);
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__M2D_S2L__DEBUG */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 0 ) {

        i = tv_ptr->mpi_rank;

    } else {

        i = 0;
    }

    /* since large_rank is at most PAR_SS_DR_MAX_RANK, no need to 
     * loop over it -- either we are setting i to mpi_rank, or
     * we are setting it to zero.  It will not change during the 
     * test.
     */

    if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 1 ) {

        j = tv_ptr->mpi_rank;

    } else {

        j = 0;
    }

    do {
        if ( PAR_SS_DR_MAX_RANK - tv_ptr->large_rank == 2 ) {

            k = tv_ptr->mpi_rank;

        } else {

            k = 0;
        }

        do {
            /* since small rank >= 2 and large_rank > small_rank, we 
             * have large_rank >= 3.  Since PAR_SS_DR_MAX_RANK == 5
             * (baring major re-orgaization), this gives us:
             *
             *     (PAR_SS_DR_MAX_RANK - large_rank) <= 2
             *
             * so no need to repeat the test in the outer loops --
             * just set l = 0.
             */

            l = 0;
            do {
                if ( (tv_ptr->skips)++ < tv_ptr->max_skips ) { /* skip the test */

                    (tv_ptr->tests_skipped)++;

                } else { /* run the test */

                    tv_ptr->skips = 0; /* reset the skips counter */

                    /* we know that small_rank >= 1 and that large_rank > small_rank
                     * by the assertions at the head of this function.  Thus no
                     * need for another inner loop.
                     */

                    /* Zero out this processes slice of the on disk large data set.
                     * Note that this will leave one slice with its original data
                     * as there is one more slice than processes.
                     */
                    ret = H5Dwrite(tv_ptr->large_dataset,
                                   H5T_NATIVE_UINT32,
                                   tv_ptr->mem_large_ds_sid,
                                   tv_ptr->file_large_ds_sid_0,
                                   tv_ptr->xfer_plist,
                                   tv_ptr->large_ds_buf_2);
                    VRFY((ret != FAIL), "H5Dwrite() to zero large ds suceeded");


                    /* select the portion of the in memory large cube to which we
                     * are going to write data.
                     */
                    tv_ptr->start[0] = (hsize_t)i;
                    tv_ptr->start[1] = (hsize_t)j;
                    tv_ptr->start[2] = (hsize_t)k;
                    tv_ptr->start[3] = (hsize_t)l;
                    tv_ptr->start[4] = 0;

                    HDassert((tv_ptr->start[0] == 0)||(0 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[1] == 0)||(1 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[2] == 0)||(2 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[3] == 0)||(3 < tv_ptr->small_ds_offset + 1));
                    HDassert((tv_ptr->start[4] == 0)||(4 < tv_ptr->small_ds_offset + 1));

                    ckrbrd_hs_dr_pio_test__slct_ckrbrd
                    (
                      tv_ptr->mpi_rank,
                      tv_ptr->file_large_ds_sid_1,
                      tv_ptr->large_rank,
                      tv_ptr->edge_size,
                      tv_ptr->checker_edge_size,
                      tv_ptr->small_rank - 1,
                      tv_ptr->start
                    );


                    /* verify that H5S_select_shape_same() reports the in
                     * memory small data set slice selection and the
                     * on disk slice through the large data set selection
                     * as having the same shape.
                     */
                    check = H5S_select_shape_same_test(tv_ptr->mem_small_ds_sid,
                                                       tv_ptr->file_large_ds_sid_1);
                    VRFY((check == TRUE), "H5S_select_shape_same_test passed");


                    /* write the small data set slice from memory to the 
                     * target slice of the disk data set 
                     */
#if CHECKER_BOARD_HS_DR_PIO_TEST__M2D_S2L__DEBUG 
                    HDfprintf(stdout, "%s:%d: start = %d %d %d %d %d.\n", 
                              fcnName, tv_ptr->mpi_rank,
                              tv_ptr->start[0], tv_ptr->start[1], tv_ptr->start[2], 
                              tv_ptr->start[3], tv_ptr->start[4]);
                    HDfprintf(stdout, "%s:%d: mem/file extent dims = %d/%d.\n",
                              fcnName, tv_ptr->mpi_rank,
                              H5Sget_simple_extent_ndims(tv_ptr->mem_small_ds_sid),
                              H5Sget_simple_extent_ndims(tv_ptr->file_large_ds_sid_1));
#endif /* CHECKER_BOARD_HS_DR_PIO_TEST__M2D_S2L__DEBUG */
                    ret = H5Dwrite(tv_ptr->large_dataset,
                                   H5T_NATIVE_UINT32,
                                   tv_ptr->mem_small_ds_sid,
                                   tv_ptr->file_large_ds_sid_1,
                                   tv_ptr->xfer_plist,
                                   tv_ptr->small_ds_buf_0);
                    VRFY((ret != FAIL), 
                         "H5Dwrite of small ds slice to large ds succeeded");


                    /* read this processes slice on the on disk large 
                     * data set into memory.
                     */

                    ret = H5Dread(tv_ptr->large_dataset,
                                  H5T_NATIVE_UINT32,
                                  tv_ptr->mem_large_ds_sid,
                                  tv_ptr->file_large_ds_sid_0,
                                  tv_ptr->xfer_plist,
                                  tv_ptr->large_ds_buf_1);
                    VRFY((ret != FAIL), 
                         "H5Dread() of process slice of large ds succeeded");


                    /* verify that the expected data and only the
                     * expected data was read.
                     */
                    expected_value = 
                       (uint32_t)((size_t)(tv_ptr->mpi_rank) * tv_ptr->small_ds_slice_size);

                    start_index = (size_t)
                        ((i * tv_ptr->edge_size * tv_ptr->edge_size * 
                              tv_ptr->edge_size * tv_ptr->edge_size) +
                         (j * tv_ptr->edge_size * tv_ptr->edge_size * tv_ptr->edge_size) +
                         (k * tv_ptr->edge_size * tv_ptr->edge_size) +
                         (l * tv_ptr->edge_size));
                    stop_index = start_index + tv_ptr->small_ds_slice_size - 1;

                    HDassert( start_index < stop_index );
                    HDassert( stop_index < tv_ptr->large_ds_size );


                    mis_match = FALSE;

                    data_ok = TRUE;

                    ptr_1 = tv_ptr->large_ds_buf_1;
                    for ( u = 0; u < start_index; u++, ptr_1++ ) {

                        if ( *ptr_1 != 0 ) {

                            data_ok = FALSE;
                            *ptr_1 = 0;
                        }
                    }

                    data_ok &= ckrbrd_hs_dr_pio_test__verify_data
                               (
                                 tv_ptr->large_ds_buf_1 + start_index,
                                 tv_ptr->small_rank - 1,
                                 tv_ptr->edge_size,
                                 tv_ptr->checker_edge_size,
                                 expected_value,
                                 (hbool_t)TRUE
                               );


                    ptr_1 = tv_ptr->large_ds_buf_1;
                    for ( u = stop_index; u < tv_ptr->small_ds_size; u++, ptr_1++ ) {

                        if ( *ptr_1 != 0 ) {

                            data_ok = FALSE;
                            *ptr_1 = 0;
                        }
                    }

                    VRFY((data_ok == TRUE), 
                         "small ds cb slice write to large ds slice data good.");

                    (tv_ptr->tests_run)++;
                }

                l++;

                (tv_ptr->total_tests)++;

            } while ( ( tv_ptr->large_rank > 2 ) &&
                      ( (tv_ptr->small_rank - 1) <= 1 ) &&
                      ( l < tv_ptr->edge_size ) );
            k++;
        } while ( ( tv_ptr->large_rank > 3 ) &&
                  ( (tv_ptr->small_rank - 1) <= 2 ) &&
                  ( k < tv_ptr->edge_size ) );
        j++;
    } while ( ( tv_ptr->large_rank > 4 ) &&
              ( (tv_ptr->small_rank - 1) <= 3 ) &&
              ( j < tv_ptr->edge_size ) );

    return;

} /* ckrbrd_hs_dr_pio_test__m2d_s2l() */


/*-------------------------------------------------------------------------
 * Function:	ckrbrd_hs_dr_pio_test__run_test()
 *
 * Purpose:	Test I/O to/from checkerboard selections of hyperslabs of 
 *		different rank in the parallel.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 10/10/09
 *
 * Modifications:
 *
 *		JRM -- 9/16/10
 *		Added the express_test parameter.  Use it to control 
 *		whether we set an alignment, and whether we allocate
 *		chunks such that no two processes will normally touch
 *		the same chunk.
 *
 *-------------------------------------------------------------------------
 */

#define CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG 0

static void
ckrbrd_hs_dr_pio_test__run_test(const int test_num,
                                const int edge_size,
                                const int checker_edge_size,
                                const int chunk_edge_size,
                                const int small_rank,
                                const int large_rank,
                                const hbool_t use_collective_io,
                                const hid_t dset_type,
                                const int express_test,
                                int * skips_ptr,
                                int max_skips,
                                int64_t * total_tests_ptr,
                                int64_t * tests_run_ptr,
                                int64_t * tests_skipped_ptr)

{
#if CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG
    const char *fcnName = "ckrbrd_hs_dr_pio_test__run_test()";
#endif /* CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG */
    int		mpi_rank; /* needed by VRFY */
    struct hs_dr_pio_test_vars_t test_vars = 
    {
        /* int	       mpi_size                        = */ -1,
        /* int         mpi_rank                        = */ -1,
        /* MPI_Comm    mpi_comm                        = */ MPI_COMM_NULL,
        /* MPI_Inf     mpi_info                        = */ MPI_INFO_NULL,
        /* int         test_num                        = */ -1,
        /* int         edge_size                       = */ -1,
        /* int         checker_edge_size               = */ -1,
        /* int         chunk_edge_size                 = */ -1,
        /* int         small_rank                      = */ -1,
        /* int         large_rank                      = */ -1,
        /* hid_t       dset_type                       = */ -1,
        /* uint32_t  * small_ds_buf_0                  = */ NULL,
        /* uint32_t  * small_ds_buf_1                  = */ NULL,
        /* uint32_t  * small_ds_buf_2                  = */ NULL,
        /* uint32_t  * small_ds_slice_buf              = */ NULL,
        /* uint32_t  * large_ds_buf_0                  = */ NULL,
        /* uint32_t  * large_ds_buf_1                  = */ NULL, 
        /* uint32_t  * large_ds_buf_2                  = */ NULL,
        /* uint32_t  * large_ds_slice_buf              = */ NULL,
        /* int         small_ds_offset                 = */ -1,
        /* int         large_ds_offset                 = */ -1,
        /* hid_t       fid                             = */ -1,  /* HDF5 file ID */
        /* hid_t       xfer_plist                      = */ H5P_DEFAULT,
        /* hid_t       full_mem_small_ds_sid           = */ -1,
        /* hid_t       full_file_small_ds_sid          = */ -1,
        /* hid_t       mem_small_ds_sid                = */ -1,
        /* hid_t       file_small_ds_sid_0             = */ -1,
        /* hid_t       file_small_ds_sid_1             = */ -1,
        /* hid_t       small_ds_slice_sid              = */ -1,
        /* hid_t       full_mem_large_ds_sid           = */ -1,
        /* hid_t       full_file_large_ds_sid          = */ -1,
        /* hid_t       mem_large_ds_sid                = */ -1,
        /* hid_t       file_large_ds_sid_0             = */ -1,
        /* hid_t       file_large_ds_sid_1             = */ -1,
        /* hid_t       file_large_ds_process_slice_sid = */ -1,
        /* hid_t       mem_large_ds_process_slice_sid  = */ -1,
        /* hid_t       large_ds_slice_sid              = */ -1,
        /* hid_t       small_dataset                   = */ -1,     /* Dataset ID */
        /* hid_t       large_dataset                   = */ -1,     /* Dataset ID */
        /* size_t      small_ds_size                   = */ 1,
        /* size_t      small_ds_slice_size             = */ 1,
        /* size_t      large_ds_size                   = */ 1,
        /* size_t      large_ds_slice_size             = */ 1,
        /* hsize_t     dims[PAR_SS_DR_MAX_RANK]        = */ {0,0,0,0,0},
        /* hsize_t     chunk_dims[PAR_SS_DR_MAX_RANK]  = */ {0,0,0,0,0},
        /* hsize_t     start[PAR_SS_DR_MAX_RANK]       = */ {0,0,0,0,0},
        /* hsize_t     stride[PAR_SS_DR_MAX_RANK]      = */ {0,0,0,0,0},
        /* hsize_t     count[PAR_SS_DR_MAX_RANK]       = */ {0,0,0,0,0},
        /* hsize_t     block[PAR_SS_DR_MAX_RANK]       = */ {0,0,0,0,0},
        /* hsize_t   * start_ptr                       = */ NULL,
        /* hsize_t   * stride_ptr                      = */ NULL,
        /* hsize_t   * count_ptr                       = */ NULL,
        /* hsize_t   * block_ptr                       = */ NULL,
        /* int 	       skips                           = */ 0,
        /* int 	       max_skips                       = */ 0,
        /* int64_t     total_tests                     = */ 0,
        /* int64_t     tests_run                       = */ 0,
        /* int64_t     tests_skipped                   = */ 0
    };
    struct hs_dr_pio_test_vars_t * tv_ptr = &test_vars;

    hs_dr_pio_test__setup(test_num, edge_size, checker_edge_size, 
                          chunk_edge_size, small_rank, large_rank, 
                          use_collective_io, dset_type, express_test, 
                          tv_ptr);


    /* initialize the local copy of mpi_rank */
    mpi_rank = tv_ptr->mpi_rank;


    /* initialize skips & max_skips */
    tv_ptr->skips = *skips_ptr;
    tv_ptr->max_skips = max_skips;


#if CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG
    if ( MAINPROCESS ) {
        HDfprintf(stdout, "test %d: small rank = %d, large rank = %d.\n",
                  test_num, small_rank, large_rank);
        HDfprintf(stdout, "test %d: Initialization complete.\n", test_num);
    }
#endif /* CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG */


    /* first, verify that we can read from disk correctly using selections
     * of different rank that H5S_select_shape_same() views as being of the
     * same shape.
     *
     * Start by reading a (small_rank - 1)-D slice from this processes slice 
     * of the on disk large data set, and verifying that the data read is 
     * correct.  Verify that H5S_select_shape_same() returns true on the 
     * memory and file selections.
     *
     * The first step is to set up the needed checker board selection in the
     * in memory small small cube
     */

    ckrbrd_hs_dr_pio_test__d2m_l2s(tv_ptr);


    /* similarly, read slices of the on disk small data set into slices 
     * through the in memory large data set, and verify that the correct 
     * data (and only the correct data) is read.
     */

    ckrbrd_hs_dr_pio_test__d2m_s2l(tv_ptr);


    /* now we go in the opposite direction, verifying that we can write
     * from memory to file using selections of different rank that
     * H5S_select_shape_same() views as being of the same shape.
     *
     * Start by writing small_rank - 1 D slices from the in memory large data
     * set to the on disk small dataset.  After each write, read the slice of 
     * the small dataset back from disk, and verify that it contains the 
     * expected data. Verify that H5S_select_shape_same() returns true on 
     * the memory and file selections.
     */

    ckrbrd_hs_dr_pio_test__m2d_l2s(tv_ptr);


    /* Now write the contents of the process's slice of the in memory 
     * small data set to slices of the on disk large data set.  After 
     * each write, read the process's slice of the large data set back
     * into memory, and verify that it contains the expected data. 
     * Verify that H5S_select_shape_same() returns true on the memory 
     * and file selections.
     */

    ckrbrd_hs_dr_pio_test__m2d_s2l(tv_ptr);


#if CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG
    if ( MAINPROCESS ) {
        HDfprintf(stdout, 
            "test %d: Subtests complete -- tests run/skipped/total = %lld/%lld/%lld.\n", 
             test_num, (long long)(tv_ptr->tests_run), (long long)(tv_ptr->tests_skipped),
             (long long)(tv_ptr->total_tests));
    }
#endif /* CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG */

    hs_dr_pio_test__takedown(tv_ptr);

#if CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG
    if ( MAINPROCESS ) {
        HDfprintf(stdout, "test %d: Takedown complete.\n", test_num);
    }
#endif /* CKRBRD_HS_DR_PIO_TEST__RUN_TEST__DEBUG */

    *skips_ptr = tv_ptr->skips;
    *total_tests_ptr += tv_ptr->total_tests;
    *tests_run_ptr += tv_ptr->tests_run;
    *tests_skipped_ptr += tv_ptr->tests_skipped;

    return;

} /* ckrbrd_hs_dr_pio_test__run_test() */


/*-------------------------------------------------------------------------
 * Function:	ckrbrd_hs_dr_pio_test()
 *
 * Purpose:	Test I/O to/from hyperslab selections of different rank in
 *		the parallel case.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 9/18/09
 *
 * Modifications:
 *
 *  		Modified function to take a sample of the run times
 *		of the different tests, and skip some of them if 
 *		run times are too long.  
 *
 *		We need to do this because Lustre runns very slowly
 *		if two or more processes are banging on the same 
 *		block of memory.
 *						JRM -- 9/10/10
 *      	Break this one big test into 4 smaller tests according
 *      	to {independent,collective}x{contigous,chunked} datasets.
 *		AKC -- 2010/01/17
 *
 *-------------------------------------------------------------------------
 */

static void
ckrbrd_hs_dr_pio_test(ShapeSameTestMethods sstest_type)
{
    int         express_test;
    int         local_express_test;
    int	        mpi_size = -1;
    int         mpi_rank = -1;
    int	        test_num = 0;
    int		edge_size = 10;
    int         checker_edge_size = 3;
    int		chunk_edge_size = 0;
    int	        small_rank = 3;
    int	        large_rank = 4;
    int		mpi_result;
    hid_t	dset_type = H5T_NATIVE_UINT;
    int         skips = 0;
    int         max_skips = 0;
    /* The following table list the number of sub-tests skipped between
     * each test that is actually executed as a function of the express
     * test level.  Note that any value in excess of 4880 will cause all
     * sub tests to be skipped.
     */
    int         max_skips_tbl[4] = {0, 4, 64, 1024};
    int64_t     total_tests = 0;
    int64_t     tests_run = 0;
    int64_t     tests_skipped = 0;

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    local_express_test = GetTestExpress();

    HDcompile_assert(sizeof(uint32_t) == sizeof(unsigned));

    mpi_result = MPI_Allreduce((void *)&local_express_test,
                               (void *)&express_test,
                               1,
                               MPI_INT,
                               MPI_MAX,
                               MPI_COMM_WORLD);

    VRFY((mpi_result == MPI_SUCCESS ), "MPI_Allreduce(0) succeeded");

    if ( local_express_test < 0 ) {
        max_skips = max_skips_tbl[0];
    } else if ( local_express_test > 3 ) {
        max_skips = max_skips_tbl[3];
    } else {
        max_skips = max_skips_tbl[local_express_test];
    }

#if 0 
    {
        int DebugWait = 1;
 
        while (DebugWait) ;
    }
#endif 

    for ( large_rank = 3; large_rank <= PAR_SS_DR_MAX_RANK; large_rank++ ) {

        for ( small_rank = 2; small_rank < large_rank; small_rank++ ) {
            switch(sstest_type){
                case IND_CONTIG:
                    /* contiguous data set, independent I/O */
                    chunk_edge_size = 0;
                    ckrbrd_hs_dr_pio_test__run_test(test_num,
                                                    edge_size,
                                                    checker_edge_size,
                                                    chunk_edge_size,
                                                    small_rank,
                                                    large_rank,
                                                    FALSE,
                                                    dset_type,
                                                    express_test,
                                                    &skips,
                                                    max_skips,
                                                    &total_tests,
                                                    &tests_run,
                                                    &tests_skipped);
                    test_num++;
                    break;
                    /* end of case IND_CONTIG */

                 case COL_CONTIG:
                    /* contiguous data set, collective I/O */
                    chunk_edge_size = 0;
                    ckrbrd_hs_dr_pio_test__run_test(test_num,
                                                    edge_size,
                                                    checker_edge_size,
                                                    chunk_edge_size,
                                                    small_rank,
                                                    large_rank,
                                                    TRUE,
                                                    dset_type,
                                                    express_test,
                                                    &skips,
                                                    max_skips,
                                                    &total_tests,
                                                    &tests_run,
                                                    &tests_skipped);
                    test_num++;
                    break;
                    /* end of case COL_CONTIG */

                case IND_CHUNKED:
                    /* chunked data set, independent I/O */
                    chunk_edge_size = 5;
                    ckrbrd_hs_dr_pio_test__run_test(test_num,
                                                    edge_size,
                                                    checker_edge_size,
                                                    chunk_edge_size,
                                                    small_rank,
                                                    large_rank,
                                                    FALSE,
                                                    dset_type,
                                                    express_test,
                                                    &skips,
                                                    max_skips,
                                                    &total_tests,
                                                    &tests_run,
                                                    &tests_skipped);
                    test_num++;
                    break;
                    /* end of case IND_CHUNKED */

                case COL_CHUNKED:
                    /* chunked data set, collective I/O */
                    chunk_edge_size = 5;
                    ckrbrd_hs_dr_pio_test__run_test(test_num,
                                                    edge_size,
                                                    checker_edge_size,
                                                    chunk_edge_size,
                                                    small_rank,
                                                    large_rank,
                                                    TRUE,
                                                    dset_type,
                                                    express_test,
                                                    &skips,
                                                    max_skips,
                                                    &total_tests,
                                                    &tests_run,
                                                    &tests_skipped);
                    test_num++;
                    break;
                    /* end of case COL_CHUNKED */

                default:
                    VRFY((FALSE), "unknown test type");
                    break;

            } /* end of switch(sstest_type) */
#if CONTIG_HS_DR_PIO_TEST__DEBUG
            if ( ( MAINPROCESS ) && ( tests_skipped > 0 ) ) {
                HDfprintf(stdout, "     run/skipped/total = %lld/%lld/%lld.\n",
                          tests_run, tests_skipped, total_tests);
            }
#endif /* CONTIG_HS_DR_PIO_TEST__DEBUG */
        }
    }

    if ( ( MAINPROCESS ) && ( tests_skipped > 0 ) ) {
        HDfprintf(stdout, "     %lld of %lld subtests skipped to expedite testing.\n",
                  tests_skipped, total_tests);
    }

    return;

} /* ckrbrd_hs_dr_pio_test() */

/* Main Body. Here for now, may have to move them to a separated file later. */

/*
 * Main driver of the Parallel HDF5 tests
 */

#include "testphdf5.h"

#ifndef PATH_MAX
#define PATH_MAX    512
#endif  /* !PATH_MAX */

/* global variables */
int dim0;
int dim1;
int chunkdim0;
int chunkdim1;
int nerrors = 0;			/* errors count */
int ndatasets = 300;			/* number of datasets to create*/
int ngroups = 512;                      /* number of groups to create in root
                                         * group. */
int facc_type = FACC_MPIO;		/*Test file access type */
int dxfer_coll_type = DXFER_COLLECTIVE_IO;

H5E_auto2_t old_func;		        /* previous error handler */
void *old_client_data;			/* previous error handler arg.*/

/* other option flags */

/* FILENAME and filenames must have the same number of names.
 * Use PARATESTFILE in general and use a separated filename only if the file
 * created in one test is accessed by a different test.
 * filenames[0] is reserved as the file name for PARATESTFILE.
 */
#define NFILENAME 2
#define PARATESTFILE filenames[0]
const char *FILENAME[NFILENAME]={
	    "ShapeSameTest",
	    NULL};
char	filenames[NFILENAME][PATH_MAX];
hid_t	fapl;				/* file access property list */

#ifdef USE_PAUSE
/* pause the process for a moment to allow debugger to attach if desired. */
/* Will pause more if greenlight file is not persent but will eventually */
/* continue. */
#include <sys/types.h>
#include <sys/stat.h>

void pause_proc(void)
{

    int pid;
    h5_stat_t	statbuf;
    char greenlight[] = "go";
    int maxloop = 10;
    int loops = 0;
    int time_int = 10;

    /* mpi variables */
    int  mpi_size, mpi_rank;
    int  mpi_namelen;
    char mpi_name[MPI_MAX_PROCESSOR_NAME];

    pid = getpid();
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Get_processor_name(mpi_name, &mpi_namelen);

    if (MAINPROCESS)
	while ((HDstat(greenlight, &statbuf) == -1) && loops < maxloop){
	    if (!loops++){
		printf("Proc %d (%*s, %d): to debug, attach %d\n",
		    mpi_rank, mpi_namelen, mpi_name, pid, pid);
	    }
	    printf("waiting(%ds) for file %s ...\n", time_int, greenlight);
	    fflush(stdout);
	    sleep(time_int);
	}
    MPI_Barrier(MPI_COMM_WORLD);
}

/* Use the Profile feature of MPI to call the pause_proc() */
int MPI_Init(int *argc, char ***argv)
{
    int ret_code;
    ret_code=PMPI_Init(argc, argv);
    pause_proc();
    return (ret_code);
}
#endif	/* USE_PAUSE */


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
    printf("\t-p\t\tuse combo MPI-POSIX driver\n");
    printf("\t-d <factor0> <factor1>\tdataset dimensions factors. Defaults (%d,%d)\n",
	ROW_FACTOR, COL_FACTOR);
    printf("\t-c <dim0> <dim1>\tdataset chunk dimensions. Defaults (dim0/10,dim1/10)\n");
    printf("\n");
}


/*
 * parse the command line options
 */
static int
parse_options(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* setup default chunk-size. Make sure sizes are > 0 */

    chunkdim0 = (dim0+9)/10;
    chunkdim1 = (dim1+9)/10;

    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
		case 'm':   ndatasets = atoi((*argv+1)+1);
			    if (ndatasets < 0){
				nerrors++;
				return(1);
			    }
			    break;
	        case 'n':   ngroups = atoi((*argv+1)+1);
		            if (ngroups < 0){
                                nerrors++;
                                return(1);
			    }
                            break;
		case 'f':   if (--argc < 1) {
				nerrors++;
				return(1);
			    }
			    if (**(++argv) == '-') {
				nerrors++;
				return(1);
			    }
			    paraprefix = *argv;
			    break;
		case 'p':   /* Use the MPI-POSIX driver access */
			    facc_type = FACC_MPIPOSIX;
			    break;
		case 'i':   /* Collective MPI-IO access with independent IO  */
			    dxfer_coll_type = DXFER_INDEPENDENT_IO;
			    break;
		case '2':   /* Use the split-file driver with MPIO access */
			    /* Can use $HDF5_METAPREFIX to define the */
			    /* meta-file-prefix. */
			    facc_type = FACC_MPIO | FACC_SPLIT;
			    break;
		case 'd':   /* dimensizes */
			    if (--argc < 2){
				nerrors++;
				return(1);
			    }
			    dim0 = atoi(*(++argv))*mpi_size;
			    argc--;
			    dim1 = atoi(*(++argv))*mpi_size;
			    /* set default chunkdim sizes too */
			    chunkdim0 = (dim0+9)/10;
			    chunkdim1 = (dim1+9)/10;
			    break;
		case 'c':   /* chunk dimensions */
			    if (--argc < 2){
				nerrors++;
				return(1);
			    }
			    chunkdim0 = atoi(*(++argv));
			    argc--;
			    chunkdim1 = atoi(*(++argv));
			    break;
		case 'h':   /* print help message--return with nerrors set */
			    return(1);
		default:    printf("Illegal option(%s)\n", *argv);
			    nerrors++;
			    return(1);
	    }
	}
    } /*while*/

    /* check validity of dimension and chunk sizes */
    if (dim0 <= 0 || dim1 <= 0){
	printf("Illegal dim sizes (%d, %d)\n", dim0, dim1);
	nerrors++;
	return(1);
    }
    if (chunkdim0 <= 0 || chunkdim1 <= 0){
	printf("Illegal chunkdim sizes (%d, %d)\n", chunkdim0, chunkdim1);
	nerrors++;
	return(1);
    }

    /* Make sure datasets can be divided into equal portions by the processes */
    if ((dim0 % mpi_size) || (dim1 % mpi_size)){
	if (MAINPROCESS)
	    printf("dim0(%d) and dim1(%d) must be multiples of processes(%d)\n",
		    dim0, dim1, mpi_size);
	nerrors++;
	return(1);
    }

    /* compose the test filenames */
    {
	int i, n;

	n = sizeof(FILENAME)/sizeof(FILENAME[0]) - 1;	/* exclude the NULL */

	for (i=0; i < n; i++)
	    if (h5_fixname(FILENAME[i],fapl,filenames[i],sizeof(filenames[i]))
		== NULL){
		printf("h5_fixname failed\n");
		nerrors++;
		return(1);
	    }
	printf("Test filenames are:\n");
	for (i=0; i < n; i++)
	    printf("    %s\n", filenames[i]);
    }

    return(0);
}


/*
 * Create the appropriate File access property list
 */
hid_t
create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type,
                     hbool_t use_gpfs)
{
    hid_t ret_pl = -1;
    herr_t ret;                 /* generic return value */
    int mpi_rank;		/* mpi variables */

    /* need the rank for error checking macros */
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    ret_pl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((ret_pl >= 0), "H5P_FILE_ACCESS");

    if (l_facc_type == FACC_DEFAULT)
	return (ret_pl);

    if (l_facc_type == FACC_MPIO){
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(ret_pl, comm, info);
	VRFY((ret >= 0), "");
	return(ret_pl);
    }

    if (l_facc_type == (FACC_MPIO | FACC_SPLIT)){
	hid_t mpio_pl;

	mpio_pl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((mpio_pl >= 0), "");
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(mpio_pl, comm, info);
	VRFY((ret >= 0), "");

	/* setup file access template */
	ret_pl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((ret_pl >= 0), "");
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_split(ret_pl, ".meta", mpio_pl, ".raw", mpio_pl);
	VRFY((ret >= 0), "H5Pset_fapl_split succeeded");
	H5Pclose(mpio_pl);
	return(ret_pl);
    }

    if (l_facc_type == FACC_MPIPOSIX) {
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpiposix(ret_pl, comm, use_gpfs);
	VRFY((ret >= 0), "H5Pset_fapl_mpiposix succeeded");
	return(ret_pl);
    }

    /* unknown file access types */
    return (ret_pl);
}


/* Shape Same test using contigous hyperslab using independent IO on contigous datasets */
static void
sscontig1(void)
{
    contig_hs_dr_pio_test(IND_CONTIG);
}

/* Shape Same test using contigous hyperslab using collective IO on contigous datasets */
static void
sscontig2(void)
{
    contig_hs_dr_pio_test(COL_CONTIG);
}

/* Shape Same test using contigous hyperslab using independent IO on chunked datasets */
static void
sscontig3(void)
{
    contig_hs_dr_pio_test(IND_CHUNKED);
}

/* Shape Same test using contigous hyperslab using collective IO on chunked datasets */
static void
sscontig4(void)
{
    contig_hs_dr_pio_test(COL_CHUNKED);
}


/* Shape Same test using checker hyperslab using independent IO on contigous datasets */
static void
sschecker1(void)
{
    ckrbrd_hs_dr_pio_test(IND_CONTIG);
}

/* Shape Same test using checker hyperslab using collective IO on contigous datasets */
static void
sschecker2(void)
{
    ckrbrd_hs_dr_pio_test(COL_CONTIG);
}

/* Shape Same test using checker hyperslab using independent IO on chunked datasets */
static void
sschecker3(void)
{
    ckrbrd_hs_dr_pio_test(IND_CHUNKED);
}

/* Shape Same test using checker hyperslab using collective IO on chunked datasets */
static void
sschecker4(void)
{
    ckrbrd_hs_dr_pio_test(COL_CHUNKED);
}


int main(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */

    /* Un-buffer the stdout and stderr */
    setbuf(stderr, NULL);
    setbuf(stdout, NULL);

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    dim0 = ROW_FACTOR*mpi_size;
    dim1 = COL_FACTOR*mpi_size;

    if (MAINPROCESS){
	printf("===================================\n");
	printf("Shape Same Tests Start\n");
        printf("	express_test = %d.\n", GetTestExpress());
	printf("===================================\n");
    }

    /* Attempt to turn off atexit post processing so that in case errors
     * happen during the test and the process is aborted, it will not get
     * hang in the atexit post processing in which it may try to make MPI
     * calls.  By then, MPI calls may not work.
     */
    if (H5dont_atexit() < 0){
	printf("%d: Failed to turn off atexit processing. Continue.\n", mpi_rank);
    };
    H5open();
    h5_show_hostname();

    /* Initialize testing framework */
    TestInit(argv[0], usage, parse_options);

    /* Shape Same tests using contigous hyperslab */
#if 1
    AddTest("sscontig1", sscontig1, NULL,
	"Shape Same, contigous hyperslab, ind IO, contig datasets", PARATESTFILE);
    AddTest("sscontig2", sscontig2, NULL,
	"Shape Same, contigous hyperslab, col IO, contig datasets", PARATESTFILE);
    AddTest("sscontig3", sscontig3, NULL,
	"Shape Same, contigous hyperslab, ind IO, chunked datasets", PARATESTFILE);
    AddTest("sscontig4", sscontig4, NULL,
	"Shape Same, contigous hyperslab, col IO, chunked datasets", PARATESTFILE);
#endif

    /* Shape Same tests using checker board hyperslab */
    AddTest("sschecker1", sschecker1, NULL,
	"Shape Same, checker hyperslab, ind IO, contig datasets", PARATESTFILE);
    AddTest("sschecker2", sschecker2, NULL,
	"Shape Same, checker hyperslab, col IO, contig datasets", PARATESTFILE);
    AddTest("sschecker3", sschecker3, NULL,
	"Shape Same, checker hyperslab, ind IO, chunked datasets", PARATESTFILE);
    AddTest("sschecker4", sschecker4, NULL,
	"Shape Same, checker hyperslab, col IO, chunked datasets", PARATESTFILE);

    /* Display testing information */
    TestInfo(argv[0]);

    /* setup file access property list */
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* Parse command line arguments */
    TestParseCmdLine(argc, argv);

    if (facc_type == FACC_MPIPOSIX && MAINPROCESS){
	printf("===================================\n"
	       "   Using MPIPOSIX driver\n"
	       "===================================\n");
    }

    if (dxfer_coll_type == DXFER_INDEPENDENT_IO && MAINPROCESS){
	printf("===================================\n"
	       "   Using Independent I/O with file set view to replace collective I/O \n"
	       "===================================\n");
    }


    /* Perform requested testing */
    PerformTests();

    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Display test summary, if requested */
    if (MAINPROCESS && GetTestSummary())
        TestSummary();

    /* Clean up test files */
    h5_cleanup(FILENAME, fapl);

    nerrors += GetTestNumErrs();

    /* Gather errors from all processes */
    {
        int temp;
        MPI_Allreduce(&nerrors, &temp, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
	nerrors=temp;
    }

    if (MAINPROCESS){		/* only process 0 reports */
	printf("===================================\n");
	if (nerrors)
	    printf("***Shape Same tests detected %d errors***\n", nerrors);
	else
	    printf("Shape Same tests finished with no errors\n");
	printf("===================================\n");
    }
    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrors) because exit code is limited to 1byte */
    return(nerrors!=0);
}
