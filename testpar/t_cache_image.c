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

/* Programmer:  John Mainzer
 *              7/13/15
 *
 *              This file contains tests specific to the cache image
 *        feature implemented in H5C.c
 */
#include "testphdf5.h"

#include "cache_common.h"
#include "genall5.h"

#define TEST_FILES_TO_CONSTRUCT 2
#define CHUNK_SIZE              10
#define DSET_SIZE               (40 * CHUNK_SIZE)
#define MAX_NUM_DSETS           256
#define PAR_NUM_DSETS           32
#define PAGE_SIZE        (4 * 1024)
#define PB_SIZE                 (64 * PAGE_SIZE)

/* global variable declarations: */


const char *FILENAMES[] = {
        "t_cache_image_00",
        "t_cache_image_01",
        "t_cache_image_02",
        NULL
};

/* local utility function declarations */

static void create_data_sets(hid_t file_id, int min_dset, int max_dset);
#if 0 /* keep pending full parallel cache image */
static void delete_data_sets(hid_t file_id, int min_dset, int max_dset);
#endif

static void open_hdf5_file(const hbool_t create_file,
    const hbool_t mdci_sbem_expected,
    const hbool_t read_only,
    const hbool_t set_mdci_fapl,
    const hbool_t config_fsm,
    const hbool_t enable_page_buffer,
    const char * hdf_file_name,
    const unsigned cache_image_flags,
    hid_t * file_id_ptr,
    H5F_t ** file_ptr_ptr,
    H5C_t ** cache_ptr_ptr,
    MPI_Comm comm,
    MPI_Info info,
   int l_facc_type,
   const hbool_t all_coll_metadata_ops,
   const hbool_t coll_metadata_write,
   const int md_write_strat);

static void verify_data_sets(hid_t file_id, int min_dset, int max_dset);

/* local test function declarations */

static hbool_t parse_flags(int argc, char * argv[], hbool_t * setup_ptr,
    hbool_t * ici_ptr, int * file_idx_ptr, int * mpi_size_ptr, hbool_t display);
static void usage(void);
static unsigned construct_test_file(int test_file_index);
static void par_create_dataset(int dset_num, hid_t file_id, int mpi_rank,
    int mpi_size);
static void par_delete_dataset(int dset_num, hid_t file_id, int mpi_rank);
static void par_verify_dataset(int dset_num, hid_t file_id, int mpi_rank);

static hbool_t serial_insert_cache_image(int file_name_idx, int mpi_size);
static void serial_verify_dataset(int dset_num, hid_t file_id, int mpi_size);

/* top level test function declarations */
static unsigned verify_cache_image_RO(int file_name_id,
    int md_write_strat, int mpi_rank);
static unsigned verify_cache_image_RW(int file_name_id,
    int md_write_strat, int mpi_rank);

static hbool_t smoke_check_1(MPI_Comm mpi_comm, MPI_Info mpi_info,
    int mpi_rank, int mpi_size);


/****************************************************************************/
/***************************** Utility Functions ****************************/
/****************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    construct_test_file()
 *
 * Purpose:     This function attempts to mimic the typical "poor man's
 *        parallel use case in which the file is passed between
 *        processes, each of which open the file, write some data,
 *        close the file, and then pass control on to the next
 *        process.
 *
 *        In this case, we create one group for each process, and
 *        populate it with a "zoo" of HDF5 objects selected to
 *        (ideally) exercise all HDF5 on disk data structures.
 *
 *        The end result is a test file used verify that PHDF5
 *        can open a file with a cache image.
 *
 *        Cycle of operation
 *
 *        1) Create a HDF5 file with the cache image FAPL entry.
 *
 *           Verify that the cache is informed of the cache image
 *           FAPL entry.
 *
 *           Set all cache image flags, forcing full functionality.
 *
 *        2) Create a data set in the file.
 *
 *        3) Close the file.
 *
 *        4) Open the file.
 *
 *           Verify that the metadata cache is instructed to load
 *                 the metadata cache image.
 *
 *        5) Create a data set in the file.
 *
 *        6) Close the file.  If enough datasets have been created
 *                 goto 7.  Otherwise return to 4.
 *
 *        7) Open the file R/O.
 *
 *                 Verify that the file contains a metadata cache image
 *                 superblock extension message.
 *
 *        8) Verify all data sets.
 *
 *           Verify that the cache image has been loaded.
 *
 *            9) close the file.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              1/25/17
 *
 * Modifications:
 *
 *        None.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
construct_test_file(int test_file_index)
{
    const char * fcn_name = "construct_test_file()";
    char filename[512];
    hbool_t show_progress = FALSE;
    hid_t file_id = -1;
    H5F_t *file_ptr = NULL;
    H5C_t *cache_ptr = NULL;
    int cp = 0;
    int min_dset = 0;
    int max_dset = 0;
    MPI_Comm dummy_comm = MPI_COMM_WORLD;
    MPI_Info dummy_info = MPI_INFO_NULL;

    pass = TRUE;

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* setup the file name */
    if ( pass ) {

        HDassert(FILENAMES[test_file_index]);

        if ( h5_fixname(FILENAMES[test_file_index], H5P_DEFAULT,
                                  filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 1) Create a HDF5 file with the cache image FAPL entry.
     *
     *      Verify that the cache is informed of the cache image FAPL entry.
     *
     *    Set flags forcing full function of the cache image feature.
     */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ TRUE,
                       /* mdci_sbem_expected    */ FALSE,
                       /* read_only             */ FALSE,
                       /* set_mdci_fapl         */ TRUE,
            /* config_fsm            */ TRUE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ dummy_comm,
                       /* info                  */ dummy_info,
                       /* l_facc_type           */ 0,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ FALSE,
                       /* md_write_strat        */ 0);
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 2) Create a data set in the file. */

    if ( pass ) {

        create_data_sets(file_id, min_dset++, max_dset++);
    }

#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( cache_ptr->images_loaded != 0 ) {

            pass = FALSE;
            failure_mssg = "metadata cache image block loaded(1).";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 3) Close the file. */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    while ( ( pass ) && ( max_dset < MAX_NUM_DSETS ) )
    {

        /* 4) Open the file.
         *
         *    Verify that the metadata cache is instructed to load the
         *    metadata cache image.
         */

        if ( pass ) {

            open_hdf5_file(/* create_file           */ FALSE,
                           /* mdci_sbem_expected    */ TRUE,
                           /* read_only             */ FALSE,
                           /* set_mdci_fapl         */ TRUE,
                /* config_fsm            */ FALSE,
                           /* enable_page_buffer    */ FALSE,
                           /* hdf_file_name         */ filename,
                           /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                           /* file_id_ptr           */ &file_id,
                           /* file_ptr_ptr          */ &file_ptr,
                           /* cache_ptr_ptr         */ &cache_ptr,
                           /* comm                  */ dummy_comm,
                           /* info                  */ dummy_info,
                           /* l_facc_type           */ 0,
                           /* all_coll_metadata_ops */ FALSE,
                           /* coll_metadata_write   */ FALSE,
                           /* md_write_strat        */ 0);
        }

        if ( show_progress )
            HDfprintf(stdout, "%s:L1 cp = %d, max_dset = %d, pass = %d.\n",
                      fcn_name, cp, max_dset, pass);


        /* 5) Create a data set in the file. */

        if ( pass ) {

            create_data_sets(file_id, min_dset++, max_dset++);
        }

#if H5C_COLLECT_CACHE_STATS
        if ( pass ) {

            if ( cache_ptr->images_loaded == 0 ) {

                pass = FALSE;
                failure_mssg = "metadata cache image block not loaded(1).";
            }
        }
#endif /* H5C_COLLECT_CACHE_STATS */

        if ( show_progress )
            HDfprintf(stdout, "%s:L2 cp = %d, max_dset = %d, pass = %d.\n",
                      fcn_name, cp + 1, max_dset, pass);


        /* 6) Close the file. */

        if ( pass ) {

            if ( H5Fclose(file_id) < 0  ) {

                pass = FALSE;
                failure_mssg = "H5Fclose() failed.\n";

            }
        }

        if ( show_progress )
            HDfprintf(stdout, "%s:L3 cp = %d, max_dset = %d, pass = %d.\n",
                      fcn_name, cp + 2, max_dset, pass);
    } /* end while */
    cp += 3;


    /* 7) Open the file R/O.
     *
     *    Verify that the file contains a metadata cache image
     *    superblock extension message.
     */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ FALSE,
                       /* mdci_sbem_expected    */ TRUE,
                       /* read_only             */ TRUE,
                       /* set_mdci_fapl         */ FALSE,
                /* config_fsm            */ FALSE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ dummy_comm,
                       /* info                  */ dummy_info,
                       /* l_facc_type           */ 0,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ FALSE,
                       /* md_write_strat        */ 0);
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 8) Open and close all data sets.
     *
     *    Verify that the cache image has been loaded.
     */

    if ( pass ) {

       verify_data_sets(file_id, 0, max_dset - 1);
    }

#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( cache_ptr->images_loaded == 0 ) {

            pass = FALSE;
            failure_mssg = "metadata cache image block not loaded(2).";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* 9) Close the file. */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";
        }
    }

    return !pass;

} /* construct_test_file() */


/*-------------------------------------------------------------------------
 * Function:    create_data_sets()
 *
 * Purpose:     If pass is TRUE on entry, create the specified data sets
 *        in the indicated file.
 *
 *        Data sets and their contents must be well know, as we
 *        will verify that they contain the expected data later.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              7/15/15
 *
 * Modifications:
 *
 *              Added min_dset and max_dset parameters and supporting
 *        code.  This allows the caller to specify a range of
 *        datasets to create.
 *                        JRM -- 8/20/15
 *
 *-------------------------------------------------------------------------
 */

static void
create_data_sets(hid_t file_id, int min_dset, int max_dset)
{
    const char * fcn_name = "create_data_sets()";
    char dset_name[64];
    hbool_t show_progress = FALSE;
    hbool_t valid_chunk;
    hbool_t verbose = FALSE;
    int cp = 0;
    int i, j, k, l, m;
    int data_chunk[CHUNK_SIZE][CHUNK_SIZE];
    herr_t status;
    hid_t dataspace_id = -1;
    hid_t filespace_ids[MAX_NUM_DSETS];
    hid_t memspace_id = -1;
    hid_t dataset_ids[MAX_NUM_DSETS];
    hid_t properties = -1;
    hsize_t dims[2];
    hsize_t a_size[2];
    hsize_t offset[2];
    hsize_t chunk_size[2];

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    HDassert(0 <= min_dset);
    HDassert(min_dset <= max_dset);
    HDassert(max_dset < MAX_NUM_DSETS);

    /* create the datasets */

    if ( pass ) {

        i = min_dset;

        while ( ( pass ) && ( i <= max_dset ) )
        {
            /* create a dataspace for the chunked dataset */
            dims[0] = DSET_SIZE;
            dims[1] = DSET_SIZE;
            dataspace_id = H5Screate_simple(2, dims, NULL);

            if ( dataspace_id < 0 ) {

                pass = FALSE;
                failure_mssg = "H5Screate_simple() failed.";
            }

            /* set the dataset creation plist to specify that the raw data is
             * to be partioned into 10X10 element chunks.
             */

            if ( pass ) {

                chunk_size[0] = CHUNK_SIZE;
                chunk_size[1] = CHUNK_SIZE;
                properties = H5Pcreate(H5P_DATASET_CREATE);

                if ( properties < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Pcreate() failed.";
                }
            }

            if ( pass ) {

                if ( H5Pset_chunk(properties, 2, chunk_size) < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Pset_chunk() failed.";
                }
            }

            /* create the dataset */
            if ( pass ) {

                HDsprintf(dset_name, "/dset%03d", i);
                dataset_ids[i] = H5Dcreate2(file_id, dset_name, H5T_STD_I32BE,
                                            dataspace_id, H5P_DEFAULT,
                                            properties, H5P_DEFAULT);

                if ( dataset_ids[i] < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Dcreate() failed.";
                }
            }

            /* get the file space ID */
            if ( pass ) {

                filespace_ids[i] = H5Dget_space(dataset_ids[i]);

                if ( filespace_ids[i] < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Dget_space() failed.";
                }
            }

            i++;
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create the mem space to be used to read and write chunks */
    if ( pass ) {

        dims[0] = CHUNK_SIZE;
        dims[1] = CHUNK_SIZE;
        memspace_id = H5Screate_simple(2, dims, NULL);

        if ( memspace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Screate_simple() failed.";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* select in memory hyperslab */
    if ( pass ) {

        offset[0] = 0;  /*offset of hyperslab in memory*/
        offset[1] = 0;
        a_size[0] = CHUNK_SIZE;  /*size of hyperslab*/
        a_size[1] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, NULL,
                                     a_size, NULL);

        if ( status < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sselect_hyperslab() failed.";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* initialize all datasets on a round robin basis */
    i = 0;
    while ( ( pass ) && ( i < DSET_SIZE ) )
    {
        j = 0;
        while ( ( pass ) && ( j < DSET_SIZE ) )
        {
            m = min_dset;
            while ( ( pass ) && ( m <= max_dset ) )
            {
                /* initialize the slab */
                for ( k = 0; k < CHUNK_SIZE; k++ )
                {
                    for ( l = 0; l < CHUNK_SIZE; l++ )
                    {
                        data_chunk[k][l] = (DSET_SIZE * DSET_SIZE * m) +
                                           (DSET_SIZE * (i + k)) + j + l;
                    }
                }

                /* select on disk hyperslab */
                offset[0] = (hsize_t)i; /*offset of hyperslab in file*/
                offset[1] = (hsize_t)j;
                a_size[0] = CHUNK_SIZE;   /*size of hyperslab*/
                a_size[1] = CHUNK_SIZE;
                status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                             offset, NULL, a_size, NULL);

                if ( status < 0 ) {

                    pass = FALSE;
                    failure_mssg = "disk H5Sselect_hyperslab() failed.";
                }

                /* write the chunk to file */
                status = H5Dwrite(dataset_ids[m], H5T_NATIVE_INT, memspace_id,
                                  filespace_ids[m], H5P_DEFAULT, data_chunk);

                if ( status < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Dwrite() failed.";
                }
                m++;
            }
            j += CHUNK_SIZE;
        }

        i += CHUNK_SIZE;
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* read data from data sets and validate it */
    i = 0;
    while ( ( pass ) && ( i < DSET_SIZE ) )
    {
        j = 0;
        while ( ( pass ) && ( j < DSET_SIZE ) )
        {
            m = min_dset;
            while ( ( pass ) && ( m <= max_dset ) )
            {

                /* select on disk hyperslab */
                offset[0] = (hsize_t)i; /* offset of hyperslab in file */
                offset[1] = (hsize_t)j;
                a_size[0] = CHUNK_SIZE; /* size of hyperslab */
                a_size[1] = CHUNK_SIZE;
                status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                             offset, NULL, a_size, NULL);

                if ( status < 0 ) {

                   pass = FALSE;
                   failure_mssg = "disk hyperslab create failed.";
                }

                /* read the chunk from file */
                if ( pass ) {

                    status = H5Dread(dataset_ids[m], H5T_NATIVE_INT,
                                     memspace_id, filespace_ids[m],
                                     H5P_DEFAULT, data_chunk);

                    if ( status < 0 ) {

                       pass = FALSE;
                       failure_mssg = "disk hyperslab create failed.";
                    }
                }

                /* validate the slab */
                if ( pass ) {

                    valid_chunk = TRUE;
                    for ( k = 0; k < CHUNK_SIZE; k++ )
                    {
                        for ( l = 0; l < CHUNK_SIZE; l++ )
                        {
                            if ( data_chunk[k][l]
                                 !=
                                 ((DSET_SIZE * DSET_SIZE * m) +
                                  (DSET_SIZE * (i + k)) + j + l) ) {

                                valid_chunk = FALSE;

                if ( verbose ) {

                                    HDfprintf(stdout,
                                    "data_chunk[%0d][%0d] = %0d, expect %0d.\n",
                                    k, l, data_chunk[k][l],
                                    ((DSET_SIZE * DSET_SIZE * m) +
                                     (DSET_SIZE * (i + k)) + j + l));
                                    HDfprintf(stdout,
                                    "m = %d, i = %d, j = %d, k = %d, l = %d\n",
                                    m, i, j, k, l);
                }
                            }
                        }
                    }

                    if ( ! valid_chunk ) {

                        pass = FALSE;
                        failure_mssg = "slab validation failed.";

            if ( verbose ) {

                HDfprintf(stdout,
                                  "Chunk (%0d, %0d) in /dset%03d is invalid.\n",
                                  i, j, m);
            }
                    }
                }
                m++;
            }
            j += CHUNK_SIZE;
        }
        i += CHUNK_SIZE;
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* close the file spaces */
    i = min_dset;
    while ( ( pass ) && ( i <= max_dset ) )
    {
        if ( H5Sclose(filespace_ids[i]) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sclose() failed.";
        }
        i++;
    }


    /* close the datasets */
    i = min_dset;
    while ( ( pass ) && ( i <= max_dset ) )
    {
        if ( H5Dclose(dataset_ids[i]) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dclose() failed.";
        }
        i++;
    }

    /* close the mem space */
    if ( pass ) {

        if ( H5Sclose(memspace_id) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sclose(memspace_id) failed.";
        }
    }

    return;

} /* create_data_sets() */


/*-------------------------------------------------------------------------
 * Function:    delete_data_sets()
 *
 * Purpose:     If pass is TRUE on entry, verify and then delete the
 *        dataset(s) indicated by min_dset and max_dset in the
 *        indicated file.
 *
 *        Data sets and their contents must be well know, as we
 *        will verify that they contain the expected data later.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              10/31/16
 *
 * Modifications:
 *
 *              None.
 *                        JRM -- 8/20/15
 *
 *-------------------------------------------------------------------------
 */
#if 0
/* this code will be needed to test full support of cache image
 * in parallel -- keep it around against that day.
 *
 *                                      -- JRM
 */
static void
delete_data_sets(hid_t file_id, int min_dset, int max_dset)
{
    const char * fcn_name = "delete_data_sets()";
    char dset_name[64];
    hbool_t show_progress = FALSE;
    int cp = 0;
    int i;

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    HDassert(0 <= min_dset);
    HDassert(min_dset <= max_dset);
    HDassert(max_dset < MAX_NUM_DSETS);

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* first, verify the contents of the target dataset(s) */
    verify_data_sets(file_id, min_dset, max_dset);

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* now delete the target datasets */
    if ( pass ) {

        i = min_dset;

        while ( ( pass ) && ( i <= max_dset ) )
        {
            HDsprintf(dset_name, "/dset%03d", i);

        if ( H5Ldelete(file_id, dset_name, H5P_DEFAULT) < 0) {

                pass = FALSE;
                failure_mssg = "H5Ldelete() failed.";
        }

            i++;
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    return;

} /* delete_data_sets() */
#endif


/*-------------------------------------------------------------------------
 * Function:    open_hdf5_file()
 *
 * Purpose:     If pass is true on entry, create or open the specified HDF5
 *        and test to see if it has a metadata cache image superblock
 *        extension message.
 *
 *        Set pass to FALSE and issue a suitable failure
 *        message if either the file contains a metadata cache image
 *        superblock extension and mdci_sbem_expected is TRUE, or
 *        vise versa.
 *
 *        If mdci_sbem_expected is TRUE, also verify that the metadata
 *        cache has been advised of this.
 *
 *        If read_only is TRUE, open the file read only.  Otherwise
 *        open the file read/write.
 *
 *        If set_mdci_fapl is TRUE, set the metadata cache image
 *        FAPL entry when opening the file, and verify that the
 *        metadata cache is notified.
 *
 *        If config_fsm is TRUE, setup the persistant free space
 *        manager.  Note that this flag may only be set if
 *        create_file is also TRUE.
 *
 *              Return pointers to the cache data structure and file data
 *              structures.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              7/14/15
 *
 * Modifications:
 *
 *              Modified function to handle parallel file creates / opens.
 *
 *                                                 JRM -- 2/1/17
 *
 *              Modified function to handle
 *
 *-------------------------------------------------------------------------
 */

static void
open_hdf5_file(const hbool_t create_file,
               const hbool_t mdci_sbem_expected,
        const hbool_t read_only,
        const hbool_t set_mdci_fapl,
        const hbool_t config_fsm,
               const hbool_t enable_page_buffer,
        const char * hdf_file_name,
               const unsigned cache_image_flags,
               hid_t * file_id_ptr,
               H5F_t ** file_ptr_ptr,
               H5C_t ** cache_ptr_ptr,
               MPI_Comm comm,
               MPI_Info info,
               int l_facc_type,
               const hbool_t all_coll_metadata_ops,
               const hbool_t coll_metadata_write,
               const int md_write_strat)
{
    const char * fcn_name = "open_hdf5_file()";
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    hid_t fapl_id = -1;
    hid_t fcpl_id = -1;
    hid_t file_id = -1;
    herr_t result;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    H5C_cache_image_ctl_t image_ctl;
    H5AC_cache_image_config_t cache_image_config = {
        H5AC__CURR_CACHE_IMAGE_CONFIG_VERSION,
        TRUE,
        FALSE,
        H5AC__CACHE_IMAGE__ENTRY_AGEOUT__NONE};

    HDassert(!create_file || config_fsm);

    if ( pass )
    {
    /* opening the file both read only and with a cache image
         * requested is a contradiction.  We resolve it by ignoring
         * the cache image request silently.
         */
        if ( ( create_file && mdci_sbem_expected ) ||
             ( create_file && read_only ) ||
             ( config_fsm && !create_file ) ||
             ( create_file && enable_page_buffer && ! config_fsm ) ||
             ( hdf_file_name == NULL ) ||
             ( ( set_mdci_fapl ) && ( cache_image_flags == 0 ) ) ||
             ( ( set_mdci_fapl ) &&
               ( (cache_image_flags & ~H5C_CI__ALL_FLAGS) != 0 ) ) ||
             ( file_id_ptr == NULL ) ||
             ( file_ptr_ptr == NULL ) ||
             ( cache_ptr_ptr == NULL ) ||
             ( l_facc_type != (l_facc_type & (FACC_MPIO)) ) ) {

            failure_mssg =
               "Bad param(s) on entry to open_hdf5_file().\n";

            pass = FALSE;
        } else  if ( verbose ) {

            HDfprintf(stdout, "%s: HDF file name = \"%s\".\n",
                      fcn_name, hdf_file_name);
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* create a file access propertly list. */
    if ( pass ) {

        fapl_id = H5Pcreate(H5P_FILE_ACCESS);

        if ( fapl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pcreate() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* call H5Pset_libver_bounds() on the fapl_id */
    if ( pass ) {

        if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST)
                < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_libver_bounds() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* get metadata cache image config -- verify that it is the default */
    if ( pass ) {

        result = H5Pget_mdc_image_config(fapl_id, &cache_image_config);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_image_config() failed.\n";
        }

        if ( ( cache_image_config.version !=
               H5AC__CURR_CACHE_IMAGE_CONFIG_VERSION ) ||
             ( cache_image_config.generate_image != FALSE ) ||
             ( cache_image_config.save_resize_status != FALSE ) ||
             ( cache_image_config.entry_ageout !=
               H5AC__CACHE_IMAGE__ENTRY_AGEOUT__NONE ) ) {

            pass = FALSE;
            failure_mssg = "Unexpected default cache image config.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* set metadata cache image fapl entry if indicated */
    if ( ( pass ) && ( set_mdci_fapl ) ) {

        /* set cache image config fields to taste */
        cache_image_config.generate_image = TRUE;
        cache_image_config.save_resize_status = FALSE;
        cache_image_config.entry_ageout = H5AC__CACHE_IMAGE__ENTRY_AGEOUT__NONE;

        result = H5Pset_mdc_image_config(fapl_id, &cache_image_config);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_mdc_image_config() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the persistant free space manager if indicated */
    if ( ( pass ) && ( config_fsm ) ) {

    fcpl_id = H5Pcreate(H5P_FILE_CREATE);

    if ( fcpl_id <= 0 ) {

        pass = FALSE;
        failure_mssg = "H5Pcreate(H5P_FILE_CREATE) failed.";
    }
    }

    if ( ( pass ) && ( config_fsm ) ) {

        if ( H5Pset_file_space_strategy(fcpl_id, H5F_FSPACE_STRATEGY_PAGE,
                                        TRUE, (hsize_t)1) == FAIL ) {
            pass = FALSE;
            failure_mssg = "H5Pset_file_space_strategy() failed.\n";
        }
    }

    if ( ( pass ) && ( config_fsm ) ) {

        if ( H5Pset_file_space_page_size(fcpl_id, PAGE_SIZE) == FAIL ) {

            pass = FALSE;
            failure_mssg = "H5Pset_file_space_page_size() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the page buffer if indicated */
    if ( ( pass ) && ( enable_page_buffer ) ) {

        if ( H5Pset_page_buffer_size(fapl_id, PB_SIZE, 0, 0) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_page_buffer_size() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    if ( ( pass ) && ( l_facc_type == FACC_MPIO ) ) {

        /* set Parallel access with communicator */
        if ( H5Pset_fapl_mpio(fapl_id, comm, info) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_fapl_mpio() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( ( pass ) && ( l_facc_type == FACC_MPIO ) ) {

        if (H5Pset_all_coll_metadata_ops(fapl_id, all_coll_metadata_ops) < 0) {

            pass = FALSE;
            failure_mssg = "H5Pset_all_coll_metadata_ops() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( ( pass ) && ( l_facc_type == FACC_MPIO ) ) {

        if ( H5Pset_coll_metadata_write(fapl_id, coll_metadata_write) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_coll_metadata_write() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( ( pass ) && ( l_facc_type == FACC_MPIO ) ) {

        /* set the desired parallel metadata write strategy */
        H5AC_cache_config_t mdc_config;

        mdc_config.version = H5C__CURR_AUTO_SIZE_CTL_VER;

        if ( H5Pget_mdc_config(fapl_id, &mdc_config) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pget_mdc_config() failed.\n";
        }

        mdc_config.metadata_write_strategy = md_write_strat;

        if ( H5Pset_mdc_config(fapl_id, &mdc_config) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_mdc_config() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* open the file */
    if ( pass ) {

        if ( create_file ) {

        if ( fcpl_id != -1 )

                file_id = H5Fcreate(hdf_file_name, H5F_ACC_TRUNC,
                    fcpl_id, fapl_id);
        else

        file_id = H5Fcreate(hdf_file_name, H5F_ACC_TRUNC,
                                    H5P_DEFAULT, fapl_id);

        } else {

            if ( read_only )

                file_id = H5Fopen(hdf_file_name, H5F_ACC_RDONLY, fapl_id);

            else

                file_id = H5Fopen(hdf_file_name, H5F_ACC_RDWR, fapl_id);
        }

        if ( file_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fcreate() or H5Fopen() failed.\n";

        } else {

            file_ptr = (struct H5F_t *)H5I_object_verify(file_id, H5I_FILE);

            if ( file_ptr == NULL ) {

                pass = FALSE;
                failure_mssg = "Can't get file_ptr.";

                if ( verbose ) {
                    HDfprintf(stdout, "%s: Can't get file_ptr.\n", fcn_name);
                }
            }
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* get a pointer to the files internal data structure and then
     * to the cache structure
     */
    if ( pass ) {

        if ( file_ptr->shared->cache == NULL ) {

            pass = FALSE;
            failure_mssg = "can't get cache pointer(1).\n";

        } else {

            cache_ptr = file_ptr->shared->cache;
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* verify expected page buffer status.  At present, page buffering
     * must be disabled in parallel -- hopefully this will change in the
     * future.
     */
    if ( pass ) {

        if ( ( file_ptr->shared->page_buf ) &&
             ( ( ! enable_page_buffer ) || ( l_facc_type == FACC_MPIO ) ) ) {

            pass = FALSE;
            failure_mssg = "page buffer unexepectedly enabled.";

        } else if ( ( file_ptr->shared->page_buf != NULL ) &&
             ( ( enable_page_buffer ) || ( l_facc_type != FACC_MPIO ) ) ) {

            pass = FALSE;
            failure_mssg = "page buffer unexepectedly disabled.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* verify expected metadata cache status */

    /* get the cache image control structure from the cache, and verify
     * that it contains the expected values.
     *
     * Then set the flags in this structure to the specified value.
     */
    if ( pass ) {

        if ( H5C_get_cache_image_config(cache_ptr, &image_ctl) < 0 ) {

            pass = FALSE;
            failure_mssg = "error returned by H5C_get_cache_image_config().";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    if ( pass ) {

        if ( set_mdci_fapl ) {

        if ( read_only ) {

                if ( ( image_ctl.version !=
                       H5AC__CURR_CACHE_IMAGE_CONFIG_VERSION ) ||
                     ( image_ctl.generate_image != FALSE ) ||
                     ( image_ctl.save_resize_status != FALSE ) ||
                     ( image_ctl.entry_ageout !=
                       H5AC__CACHE_IMAGE__ENTRY_AGEOUT__NONE ) ||
                     ( image_ctl.flags != H5C_CI__ALL_FLAGS ) ) {

                    pass = FALSE;
                    failure_mssg = "Unexpected image_ctl values(1).\n";
                }
        } else {

                if ( ( image_ctl.version !=
                       H5AC__CURR_CACHE_IMAGE_CONFIG_VERSION ) ||
                     ( image_ctl.generate_image != TRUE ) ||
                     ( image_ctl.save_resize_status != FALSE ) ||
                     ( image_ctl.entry_ageout !=
                       H5AC__CACHE_IMAGE__ENTRY_AGEOUT__NONE ) ||
                     ( image_ctl.flags != H5C_CI__ALL_FLAGS ) ) {

                    pass = FALSE;
                    failure_mssg = "Unexpected image_ctl values(2).\n";
                }
            }
        } else {

            if ( ( image_ctl.version !=
                   H5AC__CURR_CACHE_IMAGE_CONFIG_VERSION ) ||
                 ( image_ctl.generate_image != FALSE ) ||
                 ( image_ctl.save_resize_status != FALSE ) ||
                 ( image_ctl.entry_ageout !=
                   H5AC__CACHE_IMAGE__ENTRY_AGEOUT__NONE ) ||
                 ( image_ctl.flags != H5C_CI__ALL_FLAGS ) ) {

                pass = FALSE;
                failure_mssg = "Unexpected image_ctl values(3).\n";
            }
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( ( pass ) && ( set_mdci_fapl ) ) {

        image_ctl.flags = cache_image_flags;

        if ( H5C_set_cache_image_config(file_ptr, cache_ptr, &image_ctl) < 0 ) {

            pass = FALSE;
            failure_mssg = "error returned by H5C_set_cache_image_config().";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( pass ) {

        if ( cache_ptr->close_warning_received == TRUE ) {

            pass = FALSE;
            failure_mssg = "Unexpected value of close_warning_received.\n";
        }

        if ( mdci_sbem_expected ) {

            if ( read_only ) {

                if ( ( cache_ptr->load_image != TRUE ) ||
                     ( cache_ptr->delete_image != FALSE ) ) {

                    pass = FALSE;
                    failure_mssg = "mdci sb extension message not present?\n";
                }
            } else {

                if ( ( cache_ptr->load_image != TRUE ) ||
                     ( cache_ptr->delete_image != TRUE ) ) {

                    pass = FALSE;
                    failure_mssg = "mdci sb extension message not present?\n";
                }
        }
        } else {

        if ( ( cache_ptr->load_image == TRUE ) ||
                 ( cache_ptr->delete_image == TRUE ) ) {

                pass = FALSE;
                failure_mssg = "mdci sb extension message present?\n";
        }
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( pass ) {

        *file_id_ptr = file_id;
        *file_ptr_ptr = file_ptr;
        *cache_ptr_ptr = cache_ptr;
    }

    if ( show_progress ) {
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- exiting.\n",
                  fcn_name, cp++, pass);

        if ( ! pass )
            HDfprintf(stdout, "%s: failure_mssg = %s\n",
                      fcn_name, failure_mssg);
    }

    return;

} /* open_hdf5_file() */


/*-------------------------------------------------------------------------
 * Function:    par_create_dataset()
 *
 * Purpose:     Collectively create a chunked dataset, and fill it with
 *              known values.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/4/17
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static void
par_create_dataset(int dset_num,
                   hid_t file_id,
                   int mpi_rank,
                   int mpi_size)
{
    const char * fcn_name = "par_create_dataset()";
    char dset_name[256];
    hbool_t show_progress = FALSE;
    hbool_t valid_chunk;
    hbool_t verbose = FALSE;
    int cp = 0;
    int i, j, k, l;
    int data_chunk[1][CHUNK_SIZE][CHUNK_SIZE];
    hsize_t dims[3];
    hsize_t a_size[3];
    hsize_t offset[3];
    hsize_t chunk_size[3];
    hid_t status;
    hid_t dataspace_id = -1;
    hid_t memspace_id = -1;
    hid_t dset_id = -1;
    hid_t filespace_id = -1;
    hid_t dcpl_id = -1;
    hid_t dxpl_id = -1;

    show_progress = (show_progress && (mpi_rank == 0));
    verbose       = (verbose && (mpi_rank == 0));

    HDsprintf(dset_name, "/dset%03d", dset_num);

    if ( show_progress ) {
        HDfprintf(stdout, "%s: dset name = \"%s\".\n", fcn_name, dset_name);
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);
    }

    if ( pass ) {

        /* create a dataspace for the chunked dataset */
        dims[0] = (hsize_t)mpi_size;
        dims[1] = DSET_SIZE;
        dims[2] = DSET_SIZE;
        dataspace_id = H5Screate_simple(3, dims, NULL);

        if ( dataspace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Screate_simple() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* set the dataset creation plist to specify that the raw data is
     * to be partioned into 1X10X10 element chunks.
     */

    if ( pass ) {

        dcpl_id = H5Pcreate(H5P_DATASET_CREATE);

        if ( dcpl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_DATASET_CREATE) failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( pass ) {

        chunk_size[0] = 1;
        chunk_size[1] = CHUNK_SIZE;
        chunk_size[2] = CHUNK_SIZE;

        if ( H5Pset_chunk(dcpl_id, 3, chunk_size) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_chunk() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* create the dataset */
    if ( pass ) {

        dset_id = H5Dcreate2(file_id, dset_name, H5T_STD_I32BE,
                             dataspace_id, H5P_DEFAULT,
                             dcpl_id, H5P_DEFAULT);

        if ( dset_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dcreate() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* get the file space ID */
    if ( pass ) {

        filespace_id = H5Dget_space(dset_id);

        if ( filespace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dget_space() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* create the mem space to be used to read and write chunks */
    if ( pass ) {

        dims[0] = 1;
        dims[1] = CHUNK_SIZE;
        dims[2] = CHUNK_SIZE;
        memspace_id = H5Screate_simple(3, dims, NULL);

        if ( memspace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Screate_simple() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* select in memory hyperslab */
    if ( pass ) {

        offset[0] = 0;                  /* offset of hyperslab in memory */
        offset[1] = 0;
        offset[2] = 0;
        a_size[0] = 1;                  /* size of hyperslab */
        a_size[1] = CHUNK_SIZE;
        a_size[2] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, NULL,
                                     a_size, NULL);

        if ( status < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sselect_hyperslab() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the DXPL for collective I/O */
    if ( pass ) {

        dxpl_id = H5Pcreate(H5P_DATASET_XFER);

        if ( dxpl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_DATASET_XFER) failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( pass ) {

        if ( H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_dxpl_mpio() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* initialize the dataset with collective writes */
    i = 0;
    while ( ( pass ) && ( i < DSET_SIZE ) )
    {
        j = 0;
        while ( ( pass ) && ( j < DSET_SIZE ) )
        {

            if ( show_progress )
                HDfprintf(stdout, "%s: cp = %d.0, pass = %d.\n",
                          fcn_name, cp, pass);

            /* initialize the slab */
            for ( k = 0; k < CHUNK_SIZE; k++ )
            {
                for ( l = 0; l < CHUNK_SIZE; l++ )
                {
                    data_chunk[0][k][l] = (DSET_SIZE * DSET_SIZE * mpi_rank) +
                                          (DSET_SIZE * (i + k)) + j + l +
                                          dset_num;
                }
            }

            if ( show_progress )
                HDfprintf(stdout, "%s: cp = %d.1, pass = %d.\n",
                          fcn_name, cp, pass);

            /* select on disk hyperslab */
            offset[0] = (hsize_t)mpi_rank; /* offset of hyperslab in file */
            offset[1] = (hsize_t)i;
            offset[2] = (hsize_t)j;
            a_size[0] = (hsize_t)1;        /* size of hyperslab */
            a_size[1] = CHUNK_SIZE;
            a_size[2] = CHUNK_SIZE;
            status = H5Sselect_hyperslab(filespace_id, H5S_SELECT_SET,
                                         offset, NULL, a_size, NULL);

            if ( status < 0 ) {

                pass = FALSE;
                failure_mssg = "disk H5Sselect_hyperslab() failed.";
            }

            if ( show_progress )
                HDfprintf(stdout, "%s: cp = %d.2, pass = %d.\n",
                          fcn_name, cp, pass);

            /* write the chunk to file */
            status = H5Dwrite(dset_id, H5T_NATIVE_INT, memspace_id,
                              filespace_id, dxpl_id, data_chunk);

            if ( status < 0 ) {

                pass = FALSE;
                failure_mssg = "H5Dwrite() failed.";
            }

            if ( show_progress )
                HDfprintf(stdout, "%s: cp = %d.3, pass = %d.\n",
                          fcn_name, cp, pass);

            j += CHUNK_SIZE;
        }

        i += CHUNK_SIZE;
    }

    cp++;
    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* read data from data sets and validate it */
    i = 0;
    while ( ( pass ) && ( i < DSET_SIZE ) )
    {
        j = 0;
        while ( ( pass ) && ( j < DSET_SIZE ) )
        {
            /* select on disk hyperslab */
            offset[0] = (hsize_t)mpi_rank;
            offset[1] = (hsize_t)i; /* offset of hyperslab in file */
            offset[2] = (hsize_t)j;
            a_size[0] = (hsize_t)1;
            a_size[1] = CHUNK_SIZE; /* size of hyperslab */
            a_size[2] = CHUNK_SIZE;

            status = H5Sselect_hyperslab(filespace_id, H5S_SELECT_SET,
                                         offset, NULL, a_size, NULL);

            if ( status < 0 ) {

               pass = FALSE;
               failure_mssg = "disk hyperslab create failed.";
            }

            /* read the chunk from file */
            if ( pass ) {

                status = H5Dread(dset_id, H5T_NATIVE_INT,
                                 memspace_id, filespace_id,
                                 dxpl_id, data_chunk);

                if ( status < 0 ) {

                   pass = FALSE;
                   failure_mssg = "chunk read failed.";
                }
            }

            /* validate the slab */
            if ( pass ) {

                valid_chunk = TRUE;
                for ( k = 0; k < CHUNK_SIZE; k++ )
                {
                    for ( l = 0; l < CHUNK_SIZE; l++ )
                    {
                        if ( data_chunk[0][k][l]
                             !=
                             ((DSET_SIZE * DSET_SIZE * mpi_rank) +
                              (DSET_SIZE * (i + k)) + j + l + dset_num) ) {

                            valid_chunk = FALSE;

                            if ( verbose ) {

                                HDfprintf(stdout,
                                    "data_chunk[%0d][%0d] = %0d, expect %0d.\n",
                                    k, l, data_chunk[0][k][l],
                                    ((DSET_SIZE * DSET_SIZE * mpi_rank) +
                                     (DSET_SIZE * (i + k)) + j + l + dset_num));
                                    HDfprintf(stdout,
                                    "dset_num = %d, i = %d, j = %d, k = %d, l = %d\n",
                                    dset_num, i, j, k, l);
                            }
                        }
                    }
                }

                if ( ! valid_chunk ) {

                    pass = FALSE;
                    failure_mssg = "slab validation failed.";

                    if ( verbose ) {

                        HDfprintf(stdout,
                                "Chunk (%0d, %0d) in /dset%03d is invalid.\n",
                                i, j, dset_num);
                    }
                }
            }
            j += CHUNK_SIZE;
        }
        i += CHUNK_SIZE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* close the data space */
    if ( ( pass ) && ( H5Sclose(dataspace_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Sclose(dataspace_id) failed.";
    }

    /* close the file space */
    if ( ( pass ) && ( H5Sclose(filespace_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Sclose(filespace_id) failed.";
    }

    /* close the dataset */
    if ( ( pass ) && ( H5Dclose(dset_id) < 0 ) )  {

        pass = FALSE;
        failure_mssg = "H5Dclose(dset_id) failed.";
    }

    /* close the mem space */
    if ( ( pass ) && ( H5Sclose(memspace_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Sclose(memspace_id) failed.";
    }

    /* close the dataset creation property list */
    if ( ( pass ) && ( H5Pclose(dcpl_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Pclose(dcpl) failed.";
    }

    /* close the data access property list */
    if ( ( pass ) && ( H5Pclose(dxpl_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Pclose(dxpl) failed.";
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    return;

} /* par_create_dataset() */


/*-------------------------------------------------------------------------
 * Function:    par_delete_dataset()
 *
 * Purpose:     Collectively delete the specified dataset.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/6/17
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static void
par_delete_dataset(int dset_num,
                   hid_t file_id,
                   int mpi_rank)
{
    const char * fcn_name = "par_delete_dataset()";
    char dset_name[256];
    hbool_t show_progress = FALSE;
    int cp = 0;

    show_progress = (show_progress && (mpi_rank == 0));

    HDsprintf(dset_name, "/dset%03d", dset_num);

    if ( show_progress ) {
        HDfprintf(stdout, "%s: dset name = \"%s\".\n", fcn_name, dset_name);
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);
    }

    /* verify the target dataset */
    if ( pass ) {

        par_verify_dataset(dset_num, file_id, mpi_rank);
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* delete the target dataset */
    if ( pass ) {

        if ( H5Ldelete(file_id, dset_name, H5P_DEFAULT) < 0) {

            pass = FALSE;
            failure_mssg = "H5Ldelete() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    return;

} /* par_delete_dataset() */


/* This test uses many POSIX things that are not available on
 * Windows. We're using a check for fork(2) here as a proxy for
 * all POSIX/Unix/Linux things until this test can be made
 * more platform-independent.
 */
#ifdef H5_HAVE_FORK


/*-------------------------------------------------------------------------
 * Function:    par_insert_cache_image()
 *
 * Purpose:     Insert a cache image in the supplied file.
 *
 *              At present, cache image is not enabled in the parallel
 *              so we have to insert the cache image with a serial
 *              process.  Do this via a fork and an execv from process 0.
 *              All processes wait until the child process completes, and
 *              then return.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/8/17
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static void
par_insert_cache_image(int file_name_idx, int mpi_rank, int mpi_size )
{
    hbool_t show_progress = FALSE;

    if ( pass ) {

        if ( mpi_rank == 0 ) { /* insert cache image in supplied test file */

            char file_name_idx_str[32];
            char mpi_size_str[32];
            int child_status;
            pid_t child_pid;

            HDsprintf(file_name_idx_str, "%d", file_name_idx);
            HDsprintf(mpi_size_str, "%d", mpi_size);

            child_pid = fork();

            if ( child_pid == 0 ) { /* this is the child process */

                /* fun and games to shutup the compiler */
                char param0[32] = "t_cache_image";
                char param1[32] = "ici";
                char * child_argv[] = {param0,
                                       param1,
                                       file_name_idx_str,
                                       mpi_size_str,
                                       NULL};

                /* we may need to play with the path here */
                if ( execv("t_cache_image", child_argv) == -1 ) {

                    HDfprintf(stdout,
                             "execl() of ici process failed. errno = %d(%s)\n",
                             errno, strerror(errno));
                    HDexit(1);
                }

            } else if ( child_pid != -1 ) {
                /* this is the parent process -- wait until child is done */
                if ( -1 == waitpid(child_pid, &child_status, WUNTRACED)) {

                    HDfprintf(stdout, "can't wait on ici process.\n");
                    pass = FALSE;

                } else if ( ! WIFEXITED(child_status) ) {

                    HDfprintf(stdout, "ici process hasn't exitied.\n");
                    pass = FALSE;

                } else if ( WEXITSTATUS(child_status) != 0 ) {

                    HDfprintf(stdout, "ici process reports failure.\n");
                    pass = FALSE;

                } else if ( show_progress ) {

                    HDfprintf(stdout, "cache image insertion complete.\n");
                }
            } else { /* fork failed */

                HDfprintf(stdout,
                          "can't create process to insert cache image.\n");
                pass = FALSE;
            }
        }
    }

    if ( pass ) {

        /* make sure insertion of the cache image is complete
         * before proceeding
         */
        MPI_Barrier(MPI_COMM_WORLD);
    }

    return;

} /* par_insert_cache_image() */
#else /* H5_HAVE_FORK */

static void
par_insert_cache_image(int file_name_idx, int mpi_rank, int mpi_size )
{
    return;
} /* par_insert_cache_image() */

#endif /* H5_HAVE_FORK */


/*-------------------------------------------------------------------------
 * Function:    par_verify_dataset()
 *
 * Purpose:     Collectively verify the contents of a chunked dataset.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/6/17
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static void
par_verify_dataset(int dset_num,
                   hid_t file_id,
                   int mpi_rank)
{
    const char * fcn_name = "par_verify_dataset()";
    char dset_name[256];
    hbool_t show_progress = FALSE;
    hbool_t valid_chunk;
    hbool_t verbose = FALSE;
    int cp = 0;
    int i, j, k, l;
    int data_chunk[1][CHUNK_SIZE][CHUNK_SIZE];
    hsize_t dims[3];
    hsize_t a_size[3];
    hsize_t offset[3];
    hid_t status;
    hid_t memspace_id = -1;
    hid_t dset_id = -1;
    hid_t filespace_id = -1;
    hid_t dxpl_id = -1;

    show_progress = (show_progress && (mpi_rank == 0));
    verbose       = (verbose && (mpi_rank == 0));

    HDsprintf(dset_name, "/dset%03d", dset_num);

    if ( show_progress ) {
        HDfprintf(stdout, "%s: dset name = \"%s\".\n", fcn_name, dset_name);
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);
    }

    if ( pass ) {

        /* open the dataset */

        dset_id = H5Dopen2(file_id, dset_name, H5P_DEFAULT);

        if ( dset_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dopen2() failed.";
        }
    }

    /* get the file space ID */
    if ( pass ) {

        filespace_id = H5Dget_space(dset_id);

        if ( filespace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dget_space() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* create the mem space to be used to read */
    if ( pass ) {

        dims[0] = 1;
        dims[1] = CHUNK_SIZE;
        dims[2] = CHUNK_SIZE;
        memspace_id = H5Screate_simple(3, dims, NULL);

        if ( memspace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Screate_simple() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* select in memory hyperslab */
    if ( pass ) {

        offset[0] = 0;                  /* offset of hyperslab in memory */
        offset[1] = 0;
        offset[2] = 0;
        a_size[0] = 1;                  /* size of hyperslab */
        a_size[1] = CHUNK_SIZE;
        a_size[2] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, NULL,
                                     a_size, NULL);

        if ( status < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sselect_hyperslab() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the DXPL for collective I/O */
    if ( pass ) {

        dxpl_id = H5Pcreate(H5P_DATASET_XFER);

        if ( dxpl_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pcreate(H5P_DATASET_XFER) failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    if ( pass ) {

        if ( H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_dxpl_mpio() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* read data from data sets and validate it */
    i = 0;
    while ( ( pass ) && ( i < DSET_SIZE ) )
    {
        j = 0;
        while ( ( pass ) && ( j < DSET_SIZE ) )
        {
            /* select on disk hyperslab */
            offset[0] = (hsize_t)mpi_rank;
            offset[1] = (hsize_t)i; /* offset of hyperslab in file */
            offset[2] = (hsize_t)j;
            a_size[0] = (hsize_t)1;
            a_size[1] = CHUNK_SIZE; /* size of hyperslab */
            a_size[2] = CHUNK_SIZE;

            status = H5Sselect_hyperslab(filespace_id, H5S_SELECT_SET,
                                         offset, NULL, a_size, NULL);

            if ( status < 0 ) {

               pass = FALSE;
               failure_mssg = "disk hyperslab create failed.";
            }

            /* read the chunk from file */
            if ( pass ) {

                status = H5Dread(dset_id, H5T_NATIVE_INT,
                                 memspace_id, filespace_id,
                                 dxpl_id, data_chunk);

                if ( status < 0 ) {

                   pass = FALSE;
                   failure_mssg = "chunk read failed.";
                }
            }

            /* validate the slab */
            if ( pass ) {

                valid_chunk = TRUE;
                for ( k = 0; k < CHUNK_SIZE; k++ )
                {
                    for ( l = 0; l < CHUNK_SIZE; l++ )
                    {
                        if ( data_chunk[0][k][l]
                             !=
                             ((DSET_SIZE * DSET_SIZE * mpi_rank) +
                              (DSET_SIZE * (i + k)) + j + l + dset_num) ) {

                            valid_chunk = FALSE;

                            if ( verbose ) {

                                HDfprintf(stdout,
                                    "data_chunk[%0d][%0d] = %0d, expect %0d.\n",
                                    k, l, data_chunk[0][k][l],
                                    ((DSET_SIZE * DSET_SIZE * mpi_rank) +
                                     (DSET_SIZE * (i + k)) + j + l + dset_num));
                                    HDfprintf(stdout,
                                    "dset_num = %d, i = %d, j = %d, k = %d, l = %d\n",
                                    dset_num, i, j, k, l);
                            }
                        }
                    }
                }

                if ( ! valid_chunk ) {

                    pass = FALSE;
                    failure_mssg = "slab validation failed.";

                    if ( verbose ) {

                        HDfprintf(stdout,
                                "Chunk (%0d, %0d) in /dset%03d is invalid.\n",
                                i, j, dset_num);
                    }
                }
            }
            j += CHUNK_SIZE;
        }
        i += CHUNK_SIZE;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* close the file space */
    if ( ( pass ) && ( H5Sclose(filespace_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Sclose(filespace_id) failed.";
    }

    /* close the dataset */
    if ( ( pass ) && ( H5Dclose(dset_id) < 0 ) )  {

        pass = FALSE;
        failure_mssg = "H5Dclose(dset_id) failed.";
    }

    /* close the mem space */
    if ( ( pass ) && ( H5Sclose(memspace_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Sclose(memspace_id) failed.";
    }

    /* close the data access property list */
    if ( ( pass ) && ( H5Pclose(dxpl_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Pclose(dxpl) failed.";
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    return;

} /* par_verify_dataset() */


/*-------------------------------------------------------------------------
 * Function:    serial_insert_cache_image()
 *
 * Purpose:     Insert a cache image in the supplied file.
 *
 *         To populate the cache image, validate the contents
 *         of the file before closing.
 *
 *              On failure, print an appropriate error message and
 *              return FALSE.
 *
 * Return:      TRUE if succussful, FALSE otherwise.
 *
 * Programmer:  John Mainzer
 *              3/8/17
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static hbool_t
serial_insert_cache_image(int file_name_idx, int mpi_size )
{
    const char * fcn_name = "serial_insert_cache_image()";
    char filename[512];
    hbool_t show_progress = FALSE;
    int cp = 0;
    int i;
    int num_dsets = PAR_NUM_DSETS;
    hid_t file_id = -1;
    H5F_t *file_ptr = NULL;
    H5C_t *cache_ptr = NULL;
    MPI_Comm dummy_comm = MPI_COMM_WORLD;
    MPI_Info dummy_info = MPI_INFO_NULL;

    pass = TRUE;

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 1) setup the file name */
    if ( pass ) {

        HDassert(FILENAMES[file_name_idx]);

        if ( h5_fixname(FILENAMES[file_name_idx], H5P_DEFAULT,
                                  filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            HDfprintf(stdout, "h5_fixname() failed.\n");
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 2) Open the PHDF5 file with the cache image FAPL entry.
     */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ FALSE,
                       /* mdci_sbem_expected    */ FALSE,
                       /* read_only             */ FALSE,
                       /* set_mdci_fapl         */ TRUE,
                       /* config_fsm            */ FALSE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ dummy_comm,
                       /* info                  */ dummy_info,
                       /* l_facc_type           */ 0,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ FALSE,
                       /* md_write_strat        */ 1);
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 3) Validate contents of the file */

    i = 0;
    while ( ( pass ) && ( i < num_dsets ) ) {

        serial_verify_dataset(i, file_id, mpi_size);
        i++;
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 4) Close the file */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    return pass;

} /* serial_insert_cache_image() */


/*-------------------------------------------------------------------------
 * Function:    serial_verify_dataset()
 *
 * Purpose:     Verify the contents of a chunked dataset.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/6/17
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

static void
serial_verify_dataset(int dset_num,
                      hid_t file_id,
                      int mpi_size)
{
    const char * fcn_name = "serial_verify_dataset()";
    char dset_name[256];
    hbool_t show_progress = FALSE;
    hbool_t valid_chunk;
    hbool_t verbose = FALSE;
    int cp = 0;
    int i, j, k, l, m;
    int data_chunk[1][CHUNK_SIZE][CHUNK_SIZE];
    hsize_t dims[3];
    hsize_t a_size[3];
    hsize_t offset[3];
    hid_t status;
    hid_t memspace_id = -1;
    hid_t dset_id = -1;
    hid_t filespace_id = -1;

    HDsprintf(dset_name, "/dset%03d", dset_num);

    if ( show_progress ) {
        HDfprintf(stdout, "%s: dset name = \"%s\".\n", fcn_name, dset_name);
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);
    }

    if ( pass ) {

        /* open the dataset */

        dset_id = H5Dopen2(file_id, dset_name, H5P_DEFAULT);

        if ( dset_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dopen2() failed.";
        }
    }

    /* get the file space ID */
    if ( pass ) {

        filespace_id = H5Dget_space(dset_id);

        if ( filespace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dget_space() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* create the mem space to be used to read */
    if ( pass ) {

        dims[0] = 1;
        dims[1] = CHUNK_SIZE;
        dims[2] = CHUNK_SIZE;
        memspace_id = H5Screate_simple(3, dims, NULL);

        if ( memspace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Screate_simple() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* select in memory hyperslab */
    if ( pass ) {

        offset[0] = 0;                  /* offset of hyperslab in memory */
        offset[1] = 0;
        offset[2] = 0;
        a_size[0] = 1;                  /* size of hyperslab */
        a_size[1] = CHUNK_SIZE;
        a_size[2] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, NULL,
                                     a_size, NULL);

        if ( status < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sselect_hyperslab() failed.";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* read data from data sets and validate it */
    i = 0;
    while ( ( pass ) && ( i < mpi_size ) )
    {
        j = 0;
        while ( ( pass ) && ( j < DSET_SIZE ) )
        {
            k = 0;
            while ( ( pass ) && ( k < DSET_SIZE ) )
            {
                /* select on disk hyperslab */
                offset[0] = (hsize_t)i; /* offset of hyperslab in file */
                offset[1] = (hsize_t)j; /* offset of hyperslab in file */
                offset[2] = (hsize_t)k;
                a_size[0] = (hsize_t)1;
                a_size[1] = CHUNK_SIZE; /* size of hyperslab */
                a_size[2] = CHUNK_SIZE;

                status = H5Sselect_hyperslab(filespace_id, H5S_SELECT_SET,
                                             offset, NULL, a_size, NULL);

                if ( status < 0 ) {

                   pass = FALSE;
                   failure_mssg = "disk hyperslab create failed.";
                }

                /* read the chunk from file */
                if ( pass ) {

                    status = H5Dread(dset_id, H5T_NATIVE_INT,
                                     memspace_id, filespace_id,
                                     H5P_DEFAULT, data_chunk);

                    if ( status < 0 ) {

                       pass = FALSE;
                       failure_mssg = "chunk read failed.";
                    }
                }

                /* validate the slab */
                if ( pass ) {

                    valid_chunk = TRUE;

                    for ( l = 0; l < CHUNK_SIZE; l++ )
                    {
                        for ( m = 0; m < CHUNK_SIZE; m++ )
                        {
                            if ( data_chunk[0][l][m]
                                 !=
                                 ((DSET_SIZE * DSET_SIZE * i) +
                                  (DSET_SIZE * (j + l)) + k + m + dset_num) ) {

                                valid_chunk = FALSE;

                                if ( verbose ) {

                                    HDfprintf(stdout,
                                    "data_chunk[%0d][%0d] = %0d, expect %0d.\n",
                                    j, k, data_chunk[0][j][k],
                                    ((DSET_SIZE * DSET_SIZE * i) +
                                     (DSET_SIZE * (j + l)) + k + m + dset_num));
                                    HDfprintf(stdout,
                                    "dset_num = %d, i = %d, j = %d, k = %d, l = %d, m = %d\n",
                                    dset_num, i, j, k, l, m);
                                }
                            }
                        }
                    }

                    if ( ! valid_chunk ) {

                        pass = FALSE;
                        failure_mssg = "slab validation failed.";

                        if ( verbose ) {

                            HDfprintf(stdout,
                                  "Chunk (%0d, %0d) in /dset%03d is invalid.\n",
                                   j, k, dset_num);
                        }
                    }
                }
                k += CHUNK_SIZE;
            }
            j += CHUNK_SIZE;
        }
        i++;
    }


    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* close the file space */
    if ( ( pass ) && ( H5Sclose(filespace_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Sclose(filespace_id) failed.";
    }

    /* close the dataset */
    if ( ( pass ) && ( H5Dclose(dset_id) < 0 ) )  {

        pass = FALSE;
        failure_mssg = "H5Dclose(dset_id) failed.";
    }

    /* close the mem space */
    if ( ( pass ) && ( H5Sclose(memspace_id) < 0 ) ) {

        pass = FALSE;
        failure_mssg = "H5Sclose(memspace_id) failed.";
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    return;

} /* serial_verify_dataset() */


/*-------------------------------------------------------------------------
 * Function:    parse_flags
 *
 * Purpose:     Parse the flags passed to this program, and load the
 *              values into the supplied field.
 *
 * Return:      Success:        1
 *              Failure:        0
 *
 * Programmer:  J Mainzer
 *              4/28/11
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
parse_flags(int argc, char * argv[], hbool_t * setup_ptr,
    hbool_t * ici_ptr, int * file_idx_ptr, int * mpi_size_ptr, hbool_t display)
{
    const char * fcn_name = "parse_flags()";
    const char * (ops[]) = {"setup", "ici"};
    int success = TRUE;

    HDassert(setup_ptr);
    HDassert(*setup_ptr == FALSE);
    HDassert(ici_ptr);
    HDassert(*ici_ptr == FALSE);
    HDassert(file_idx_ptr);
    HDassert(mpi_size_ptr);

    if ( setup_ptr == NULL ) {

        success = FALSE;
        HDfprintf(stdout, "%s: bad arg(s) on entry.\n", fcn_name);
    }


    if ( ( success ) &&
         ( ( argc != 1 ) && ( argc != 2 ) && ( argc != 4 ) ) ) {

        success = FALSE;
        usage();
    }


    if ( ( success ) && ( argc >= 2 ) ) {

        if ( strcmp(argv[1], ops[0]) == 0 ) {

            if ( argc != 2 ) {

                success = FALSE;
                usage();

            } else {

                *setup_ptr = TRUE;

            }
        } else if ( strcmp(argv[1], ops[1]) == 0 ) {

            if ( argc != 4 ) {

                success = FALSE;
                usage();

            } else {

                *ici_ptr = TRUE;
                *file_idx_ptr = atoi(argv[2]);
                *mpi_size_ptr = atoi(argv[3]);

            }
        }
    }

    if ( ( success ) && ( display ) ) {

        if ( *setup_ptr )

            HDfprintf(stdout, "t_cache_image setup\n");

        else if ( *ici_ptr )

            HDfprintf(stdout, "t_cache_image ici %d %d\n",
                      *file_idx_ptr, *mpi_size_ptr);

        else

            HDfprintf(stdout, "t_cache_image\n");
    }

    return(success);

} /* parse_flags() */


/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Display a brief message describing the purpose and use
 *              of the program.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              4/28/11
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
usage(void)
{
    const char * s[] =
    {
        "\n",
        "t_cache_image:\n",
        "\n",
        "Run the parallel cache image tests.  \n"
        "\n"
        "In general, this program is run via MPI.  However, at present, files\n"
        "with cache images can only be constructed by serial processes.\n",
        "\n",
        "To square this circle, one process in the parallel computation \n"
        "forks a serial version of the test program to handle this detail.\n",
        "The \"setup\" parameter indicates that t_cache_image is being \n",
        "invokde for this purpose.\n",
        "\n",
        "Similarly, only a serial process can add a cache image to an\n",
        "existing file.\n",
        "\n",
        "Here again, one process forks a serial version of the test program\n",
        "with the \"ici\" parameter.\n"
        "\n",
        "usage: t_cache_image [setup|ici m n]\n",
        "\n",
        "where:\n",
        "\n",
        "       setup parameter forces creation of test file\n",
        "\n",
        "       ici parameter forces insertion of a cache image into the \n",
        "       m   th test file, created by a parallel computation with .\n",
        "       n   processes\n",
        "\n",
        "Returns 0 on success, 1 on failure.\n",
        "\n",
        NULL,
    };
    int i = 0;

    while(s[i] != NULL) {
        HDfprintf(stdout, "%s", s[i]);
        i++;
    }

    return;
} /* usage() */


/*-------------------------------------------------------------------------
 * Function:    verify_data_sets()
 *
 * Purpose:     If pass is TRUE on entry, verify that the data sets in the
 *        file exist and contain the expected data.
 *
 *        Note that these data sets were created by
 *        create_data_sets() above.  Thus any changes in that
 *        function must be reflected in this function, and
 *        vise-versa.
 *
 *              On failure, set pass to FALSE, and set failure_mssg
 *              to point to an appropriate failure message.
 *
 *              Do nothing if pass is FALSE on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              7/15/15
 *
 * Modifications:
 *
 *              Added min_dset and max_dset parameters and supporting
 *        code.  This allows the caller to specify a range of
 *        datasets to verify.
 *                        JRM -- 8/20/15
 *
 *-------------------------------------------------------------------------
 */

static void
verify_data_sets(hid_t file_id, int min_dset, int max_dset)
{
    const char * fcn_name = "verify_data_sets()";
    char dset_name[64];
    hbool_t show_progress = FALSE;
    hbool_t valid_chunk;
    hbool_t verbose = FALSE;
    int cp = 0;
    int i, j, k, l, m;
    int data_chunk[CHUNK_SIZE][CHUNK_SIZE];
    herr_t status;
    hid_t filespace_ids[MAX_NUM_DSETS];
    hid_t memspace_id = -1;
    hid_t dataset_ids[MAX_NUM_DSETS];
    hsize_t dims[2];
    hsize_t a_size[2];
    hsize_t offset[2];

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    HDassert(0 <= min_dset);
    HDassert(min_dset <= max_dset);
    HDassert(max_dset < MAX_NUM_DSETS);

    /* open the datasets */

    if ( pass ) {

        i = min_dset;

        while ( ( pass ) && ( i <= max_dset ) )
        {
            /* open the dataset */
            if ( pass ) {

                HDsprintf(dset_name, "/dset%03d", i);
                dataset_ids[i] = H5Dopen2(file_id, dset_name, H5P_DEFAULT);

                if ( dataset_ids[i] < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Dopen2() failed.";
                }
            }

            /* get the file space ID */
            if ( pass ) {

                filespace_ids[i] = H5Dget_space(dataset_ids[i]);

                if ( filespace_ids[i] < 0 ) {

                    pass = FALSE;
                    failure_mssg = "H5Dget_space() failed.";
                }
            }

            i++;
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* create the mem space to be used to read and write chunks */
    if ( pass ) {

        dims[0] = CHUNK_SIZE;
        dims[1] = CHUNK_SIZE;
        memspace_id = H5Screate_simple(2, dims, NULL);

        if ( memspace_id < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Screate_simple() failed.";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* select in memory hyperslab */
    if ( pass ) {

        offset[0] = 0;  /*offset of hyperslab in memory*/
        offset[1] = 0;
        a_size[0] = CHUNK_SIZE;  /*size of hyperslab*/
        a_size[1] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, NULL,
                                     a_size, NULL);

        if ( status < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sselect_hyperslab() failed.";
        }
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);


    /* read data from data sets and validate it */
    i = 0;
    while ( ( pass ) && ( i < DSET_SIZE ) )
    {
        j = 0;
        while ( ( pass ) && ( j < DSET_SIZE ) )
        {
            m = min_dset;
            while ( ( pass ) && ( m <= max_dset ) )
            {

                /* select on disk hyperslab */
                offset[0] = (hsize_t)i; /* offset of hyperslab in file */
                offset[1] = (hsize_t)j;
                a_size[0] = CHUNK_SIZE; /* size of hyperslab */
                a_size[1] = CHUNK_SIZE;
                status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                             offset, NULL, a_size, NULL);

                if ( status < 0 ) {

                   pass = FALSE;
                   failure_mssg = "disk hyperslab create failed.";
                }

                /* read the chunk from file */
                if ( pass ) {

                    status = H5Dread(dataset_ids[m], H5T_NATIVE_INT,
                                     memspace_id, filespace_ids[m],
                                     H5P_DEFAULT, data_chunk);

                    if ( status < 0 ) {

                       pass = FALSE;
                       failure_mssg = "disk hyperslab create failed.";
                    }
                }

                /* validate the slab */
                if ( pass ) {

                    valid_chunk = TRUE;
                    for ( k = 0; k < CHUNK_SIZE; k++ )
                    {
                        for ( l = 0; l < CHUNK_SIZE; l++ )
                        {
                            if ( data_chunk[k][l]
                                 !=
                                 ((DSET_SIZE * DSET_SIZE * m) +
                                  (DSET_SIZE * (i + k)) + j + l) ) {

                                valid_chunk = FALSE;

                if ( verbose ) {

                                    HDfprintf(stdout,
                                    "data_chunk[%0d][%0d] = %0d, expect %0d.\n",
                                    k, l, data_chunk[k][l],
                                    ((DSET_SIZE * DSET_SIZE * m) +
                                     (DSET_SIZE * (i + k)) + j + l));
                                    HDfprintf(stdout,
                                     "m = %d, i = %d, j = %d, k = %d, l = %d\n",
                                     m, i, j, k, l);
                }
                            }
                        }
                    }

                    if ( ! valid_chunk ) {

                        pass = FALSE;
                        failure_mssg = "slab validation failed.";

            if ( verbose ) {

                HDfprintf(stdout,
                                  "Chunk (%0d, %0d) in /dset%03d is invalid.\n",
                                  i, j, m);
            }
                    }
                }
                m++;
            }
            j += CHUNK_SIZE;
        }
        i += CHUNK_SIZE;
    }

    if ( show_progress ) HDfprintf(stdout, "%s: cp = %d.\n", fcn_name, cp++);

    /* close the file spaces */
    i = min_dset;
    while ( ( pass ) && ( i <= max_dset ) )
    {
        if ( H5Sclose(filespace_ids[i]) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sclose() failed.";
        }
        i++;
    }


    /* close the datasets */
    i = min_dset;
    while ( ( pass ) && ( i <= max_dset ) )
    {
        if ( H5Dclose(dataset_ids[i]) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Dclose() failed.";
        }
        i++;
    }

    /* close the mem space */
    if ( pass ) {

        if ( H5Sclose(memspace_id) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Sclose(memspace_id) failed.";
        }
    }

    return;

} /* verify_data_sets() */


/****************************************************************************/
/******************************* Test Functions *****************************/
/****************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    verify_cache_image_RO()
 *
 * Purpose:     Verify that a HDF5 file containing a cache image is
 *              opened R/O and read correctly by PHDF5 with the specified
 *              metadata write strategy.
 *
 *              Basic cycle of operation is as follows:
 *
 *        1) Open the test file created at the beginning of this
 *           test read only.
 *
 *           Verify that the file contains a cache image.
 *
 *           Verify that only process 0 reads the cache image.
 *
 *           Verify that all other processes receive the cache
 *           image block from process 0.
 *
 *              2) Verify that the file contains the expected data.
 *
 *              3) Close the file.
 *
 *              4) Open the file R/O, and verify that it still contains
 *                 a cache image.
 *
 *              5) Verify that the file contains the expected data.
 *
 *              6) Close the file.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              3/11/17
 *
 * Modifications:
 *
 *        None.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
verify_cache_image_RO(int file_name_id, int md_write_strat, int mpi_rank)
{
    const char * fcn_name = "verify_cache_image_RO()";
    char filename[512];
    hbool_t show_progress = FALSE;
    hid_t file_id = -1;
    H5F_t *file_ptr = NULL;
    H5C_t *cache_ptr = NULL;
    int cp = 0;

    pass = TRUE;

    if ( mpi_rank == 0 ) {

        switch(md_write_strat) {

            case H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY:
                TESTING("parallel CI load test -- proc0 md write -- R/O");
                break;

            case H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED:
                TESTING("parallel CI load test -- dist md write -- R/O");
                break;

            default:
                TESTING("parallel CI load test -- unknown md write -- R/o");
                pass = FALSE;
                break;
        }
    }

    show_progress = ( ( show_progress ) && ( mpi_rank == 0 ) );

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[file_name_id], H5P_DEFAULT,
                        filename, sizeof(filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 1) Open the test file created at the beginning of this test.
     *
     *    Verify that the file contains a cache image.
     */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ FALSE,
                       /* mdci_sbem_expected    */ TRUE,
                       /* read_only             */ TRUE,
                       /* set_mdci_fapl         */ FALSE,
            /* config_fsm            */ FALSE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ MPI_COMM_WORLD,
                       /* info                  */ MPI_INFO_NULL,
                       /* l_facc_type           */ FACC_MPIO,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ FALSE,
                       /* md_write_strat        */ md_write_strat);
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 2) Verify that the file contains the expected data.
     *
     *    Verify that only process 0 reads the cache image.
     *
     *    Verify that all other processes receive the cache
     *    image block from process 0.
     */

    if ( pass ) {

       verify_data_sets(file_id, 0, MAX_NUM_DSETS - 1);
    }

    /* Verify that only process 0 reads the cache image. */
#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( ( ( mpi_rank == 0 ) && ( cache_ptr->images_read != 1 ) ) ||
             ( ( mpi_rank > 0 )  && ( cache_ptr->images_read != 0 ) ) ) {

            pass = FALSE;
            failure_mssg = "unexpected images_read.";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Verify that all other processes receive the cache image block
     * from process 0.
     *
     * Since we have alread verified that only process 0 has read the
     * image, it is sufficient to verify that the image was loaded on
     * all processes.
     */
#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( cache_ptr->images_loaded != 1 ) {

            pass = FALSE;
            failure_mssg = "Image not loaded?.";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 3) Close the file. */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 4) Open the file, and verify that it doesn't contain a cache image. */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ FALSE,
                       /* mdci_sbem_expected    */ TRUE,
                       /* read_only             */ TRUE,
                       /* set_mdci_fapl         */ FALSE,
            /* config_fsm            */ FALSE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ MPI_COMM_WORLD,
                       /* info                  */ MPI_INFO_NULL,
                       /* l_facc_type           */ FACC_MPIO,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ FALSE,
                       /* md_write_strat        */ md_write_strat);
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 5) Verify that the file contains the expected data. */

    if ( pass ) {

       verify_data_sets(file_id, 0, MAX_NUM_DSETS - 1);
    }

#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( cache_ptr->images_loaded != 1 ) {

            pass = FALSE;
            failure_mssg = "metadata cache image block not loaded(2).";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */


    /* 6) Close the file. */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* report results */
    if ( mpi_rank == 0 ) {

        if ( pass ) {

            PASSED();

        } else {

            H5_FAILED();

            if ( show_progress )
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", failure_mssg);
        }
    }


    return !pass;

} /* verify_cache_image_RO() */


/*-------------------------------------------------------------------------
 * Function:    verify_cache_image_RW()
 *
 * Purpose:     Verify that a HDF5 file containing a cache image is
 *              opened and read correctly by PHDF5 with the specified
 *              metadata write strategy.
 *
 *              Basic cycle of operation is as follows:
 *
 *        1) Open the test file created at the beginning of this
 *           test.
 *
 *           Verify that the file contains a cache image.
 *
 *              2) Verify that the file contains the expected data.
 *
 *           Verify that only process 0 reads the cache image.
 *
 *           Verify that all other processes receive the cache
 *           image block from process 0.
 *
 *
 *              3) Close the file.
 *
 *              4) Open the file, and verify that it doesn't contain
 *                 a cache image.
 *
 *              5) Verify that the file contains the expected data.
 *
 *              6) Close the file.
 *
 *              7) Delete the file.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              1/25/17
 *
 * Modifications:
 *
 *        None.
 *
 *-------------------------------------------------------------------------
 */

static unsigned
verify_cache_image_RW(int file_name_id, int md_write_strat, int mpi_rank)
{
    const char * fcn_name = "verify_cache_imageRW()";
    char filename[512];
    hbool_t show_progress = FALSE;
    hid_t file_id = -1;
    H5F_t *file_ptr = NULL;
    H5C_t *cache_ptr = NULL;
    int cp = 0;

    pass = TRUE;

    if ( mpi_rank == 0 ) {

        switch(md_write_strat) {

            case H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY:
                TESTING("parallel CI load test -- proc0 md write -- R/W");
                break;

            case H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED:
                TESTING("parallel CI load test -- dist md write -- R/W");
                break;

            default:
                TESTING("parallel CI load test -- unknown md write -- R/W");
                pass = FALSE;
                break;
        }
    }

    show_progress = ( ( show_progress ) && ( mpi_rank == 0 ) );

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* setup the file name */
    if ( pass ) {

        if ( h5_fixname(FILENAMES[file_name_id], H5P_DEFAULT,
                        filename, sizeof(filename)) == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 1) Open the test file created at the beginning of this test.
     *
     *    Verify that the file contains a cache image.
     *
     *    Verify that only process 0 reads the cache image.
     *
     *    Verify that all other processes receive the cache
     *    image block from process 0.
     */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ FALSE,
                       /* mdci_sbem_expected    */ TRUE,
                       /* read_only             */ FALSE,
                       /* set_mdci_fapl         */ FALSE,
            /* config_fsm            */ FALSE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ MPI_COMM_WORLD,
                       /* info                  */ MPI_INFO_NULL,
                       /* l_facc_type           */ FACC_MPIO,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ FALSE,
                       /* md_write_strat        */ md_write_strat);
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 2) Verify that the file contains the expected data.
     *
     *    Verify that only process 0 reads the cache image.
     *
     *    Verify that all other processes receive the cache
     *    image block from process 0.
     */
    if ( pass ) {

       verify_data_sets(file_id, 0, MAX_NUM_DSETS - 1);
    }

    /* Verify that only process 0 reads the cache image. */
#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( ( ( mpi_rank == 0 ) && ( cache_ptr->images_read != 1 ) ) ||
             ( ( mpi_rank > 0 )  && ( cache_ptr->images_read != 0 ) ) ) {

            pass = FALSE;
            failure_mssg = "unexpected images_read.";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Verify that all other processes receive the cache image block
     * from process 0.
     *
     * Since we have alread verified that only process 0 has read the
     * image, it is sufficient to verify that the image was loaded on
     * all processes.
     */
#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( cache_ptr->images_loaded != 1 ) {

            pass = FALSE;
            failure_mssg = "Image not loaded?.";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 3) Close the file. */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 4) Open the file, and verify that it doesn't contain a cache image. */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ FALSE,
                       /* mdci_sbem_expected    */ FALSE,
                       /* read_only             */ FALSE,
                       /* set_mdci_fapl         */ FALSE,
            /* config_fsm            */ FALSE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ MPI_COMM_WORLD,
                       /* info                  */ MPI_INFO_NULL,
                       /* l_facc_type           */ FACC_MPIO,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ FALSE,
                       /* md_write_strat        */ md_write_strat);
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 5) Verify that the file contains the expected data. */

    if ( pass ) {

       verify_data_sets(file_id, 0, MAX_NUM_DSETS - 1);
    }

#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( cache_ptr->images_loaded != 0 ) {

            pass = FALSE;
            failure_mssg = "metadata cache image block loaded(1).";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */


    /* 6) Close the file. */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        }
    }

    if ( show_progress )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 7) Delete the file. */

    if ( pass ) {

        /* wait for everyone to close the file */
        MPI_Barrier(MPI_COMM_WORLD);

        if ( ( mpi_rank == 0 ) && ( HDremove(filename) < 0 ) ) {

            pass = FALSE;
            failure_mssg = "HDremove() failed.\n";
        }
    }


    /* report results */
    if ( mpi_rank == 0 ) {

        if ( pass ) {

            PASSED();

        } else {

            H5_FAILED();

            if ( show_progress )
                HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n", failure_mssg);
        }
    }


    return !pass;

} /* verify_cache_imageRW() */


/*****************************************************************************
 *
 * Function:    smoke_check_1()
 *
 * Purpose:     Initial smoke check to verify correct behaviour of cache
 *              image in combination with parallel.
 *
 *              As cache image is currently disabled in the parallel case,
 *              we construct a test file in parallel, verify it in serial
 *              and generate a cache image in passing, and then verify
 *              it again in parallel.
 *
 *              In passing, also verify that page buffering is silently
 *              disabled in the parallel case.  Needless to say, this part
 *              of the test will have to be re-worked when and if page
 *              buffering is supported in parallel.
 *
 * Return:      Success:        TRUE
 *
 *              Failure:        FALSE
 *
 * Programmer:  JRM -- 3/6/17
 *
 *****************************************************************************/
static hbool_t
smoke_check_1(MPI_Comm mpi_comm, MPI_Info mpi_info, int mpi_rank, int mpi_size)
{
    const char * fcn_name = "smoke_check_1()";
    char filename[512];
    hbool_t show_progress = FALSE;
    hid_t file_id = -1;
    H5F_t *file_ptr = NULL;
    H5C_t *cache_ptr = NULL;
    int cp = 0;
    int i;
    int num_dsets = PAR_NUM_DSETS;
    int test_file_index = 2;
    h5_stat_size_t file_size;

    pass = TRUE;

    if ( mpi_rank == 0 ) {

        TESTING("parallel cache image smoke check 1");
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* setup the file name */
    if ( pass ) {

        HDassert(FILENAMES[test_file_index]);

        if ( h5_fixname(FILENAMES[test_file_index], H5P_DEFAULT,
                                  filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 1) Create a PHDF5 file without the cache image FAPL entry.
     *
     *    Verify that a cache image is not requested
     */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ TRUE,
                       /* mdci_sbem_expected    */ FALSE,
                       /* read_only             */ FALSE,
                       /* set_mdci_fapl         */ FALSE,
                       /* config_fsm            */ TRUE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ mpi_comm,
                       /* info                  */ mpi_info,
                       /* l_facc_type           */ FACC_MPIO,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ TRUE,
                       /* md_write_strat        */ 1);
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 2) Create datasets in the file */

    i = 0;
    while ( ( pass ) && ( i < num_dsets ) ) {

        par_create_dataset(i, file_id, mpi_rank, mpi_size);
        i++;
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 3) Verify the datasets in the file */

    i = 0;
    while ( ( pass ) && ( i < num_dsets ) ) {

        par_verify_dataset(i, file_id, mpi_rank);
        i++;
    }


    /* 4) Close the file */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.\n";

        }
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 5 Insert a cache image into the file */

    if ( pass ) {

        par_insert_cache_image(test_file_index, mpi_rank,  mpi_size);
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 6) Open the file R/O */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ FALSE,
                       /* mdci_sbem_expected    */ TRUE,
                       /* read_only             */ TRUE,
                       /* set_mdci_fapl         */ FALSE,
                       /* config_fsm            */ FALSE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ mpi_comm,
                       /* info                  */ mpi_info,
                       /* l_facc_type           */ FACC_MPIO,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ TRUE,
                       /* md_write_strat        */ 1);
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 7) Verify the datasets in the file backwards
     *
     *    Verify that only process 0 reads the cache image.
     *
     *    Verify that all other processes receive the cache
     *    image block from process 0.
     */

    i = num_dsets - 1;
    while ( ( pass ) && ( i >= 0 ) ) {

        par_verify_dataset(i, file_id, mpi_rank);
        i--;
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Verify that only process 0 reads the cache image. */
#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( ( ( mpi_rank == 0 ) && ( cache_ptr->images_read != 1 ) ) ||
             ( ( mpi_rank > 0 )  && ( cache_ptr->images_read != 0 ) ) ) {

            pass = FALSE;
            failure_mssg = "unexpected images_read.";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Verify that all other processes receive the cache image block
     * from process 0.
     *
     * Since we have alread verified that only process 0 has read the
     * image, it is sufficient to verify that the image was loaded on
     * all processes.
     */
#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( cache_ptr->images_loaded != 1 ) {

            pass = FALSE;
            failure_mssg = "Image not loaded?.";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 8) Close the file */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.";

        }
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 9) Open the file */

    if ( pass ) {

        open_hdf5_file(/* create_file           */ FALSE,
                       /* mdci_sbem_expected    */ TRUE,
                       /* read_only             */ FALSE,
                       /* set_mdci_fapl         */ FALSE,
                       /* config_fsm            */ FALSE,
                       /* enable_page_buffer    */ FALSE,
                       /* hdf_file_name         */ filename,
                       /* cache_image_flags     */ H5C_CI__ALL_FLAGS,
                       /* file_id_ptr           */ &file_id,
                       /* file_ptr_ptr          */ &file_ptr,
                       /* cache_ptr_ptr         */ &cache_ptr,
                       /* comm                  */ mpi_comm,
                       /* info                  */ mpi_info,
                       /* l_facc_type           */ FACC_MPIO,
                       /* all_coll_metadata_ops */ FALSE,
                       /* coll_metadata_write   */ TRUE,
                       /* md_write_strat        */ 1);
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 10) Verify the datasets in the file
     *
     *     Verify that only process 0 reads the cache image.
     *
     *     Verify that all other processes receive the cache
     *     image block from process 0.
     */

    i = 0;
    while ( ( pass ) && ( i < num_dsets ) ) {

        par_verify_dataset(i, file_id, mpi_rank);
        i++;
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Verify that only process 0 reads the cache image. */
#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( ( ( mpi_rank == 0 ) && ( cache_ptr->images_read != 1 ) ) ||
             ( ( mpi_rank > 0 )  && ( cache_ptr->images_read != 0 ) ) ) {

            pass = FALSE;
            failure_mssg = "unexpected images_read.";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);

    /* Verify that all other processes receive the cache image block
     * from process 0.
     *
     * Since we have alread verified that only process 0 has read the
     * image, it is sufficient to verify that the image was loaded on
     * all processes.
     */
#if H5C_COLLECT_CACHE_STATS
    if ( pass ) {

        if ( cache_ptr->images_loaded != 1 ) {

            pass = FALSE;
            failure_mssg = "Image not loaded?.";
        }
    }
#endif /* H5C_COLLECT_CACHE_STATS */

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 11) Delete the datasets in the file */

    i = 0;
    while ( ( pass ) && ( i < num_dsets ) ) {

        par_delete_dataset(i, file_id, mpi_rank);
        i++;
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 12) Close the file */

    if ( pass ) {

        if ( H5Fclose(file_id) < 0  ) {

            pass = FALSE;
            failure_mssg = "H5Fclose() failed.";

        }
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 13) Get the size of the file.  Verify that it is less
     *     than 20 KB.  Without deletions and persistant free
     *     space managers, size size is about 30 MB, so this
     *     is sufficient to verify that the persistant free
     *     space managers are more or less doing their job.
     *
     *     Note that this test will have to change if we use
     *     a larger page size.
     */
    if ( pass ) {

        if ( ( file_size = h5_get_file_size(filename, H5P_DEFAULT) ) < 0 ) {

            pass = FALSE;
            failure_mssg = "h5_get_file_size() failed.";

        } else if ( file_size > 20 * 1024 ) {

            pass = FALSE;
            failure_mssg = "unexpectedly large file size.";
        }
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* 14) Delete the file */

    if ( pass ) {

        /* wait for everyone to close the file */
        MPI_Barrier(MPI_COMM_WORLD);

        if ( ( mpi_rank == 0 ) && ( HDremove(filename) < 0 ) ) {

            pass = FALSE;
            failure_mssg = "HDremove() failed.\n";
        }
    }

    if ( ( mpi_rank == 0 ) && ( show_progress ) )
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, pass);


    /* report results */
    if ( mpi_rank == 0 ) {

        if ( pass ) {

            PASSED();

        } else {

            H5_FAILED();

            HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n",
                      fcn_name, failure_mssg);
        }
    }

    return !pass;

} /* smoke_check_1() */


/* This test uses many POSIX things that are not available on
 * Windows. We're using a check for fork(2) here as a proxy for
 * all POSIX/Unix/Linux things until this test can be made
 * more platform-independent.
 */
#ifdef H5_HAVE_FORK

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run parallel tests on the cache image feature.
 *
 *              At present, cache image is disabled in parallel, and
 *              thus these tests are restructed to verifying that a
 *              file with a cache image can be opened in the parallel
 *              case, and verifying that instructions to create a
 *              cache image are ignored in the parallel case.
 *
 *              WARNING: This test uses fork() and execve(), and
 *                       therefore will not run on Windows.
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 * Programmer:  John Mainzer
 *              1/25/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int
main(int argc, char **argv)
{
    hbool_t setup = FALSE;
    hbool_t ici = FALSE;
    unsigned nerrs = 0;
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
    int file_idx;
    int i;
    int mpi_size;
    int mpi_rank;

    if ( ! parse_flags(argc, argv, &setup, &ici, &file_idx, &mpi_size, FALSE) )
        exit(1);  /* exit now if unable to parse flags */

    if ( setup ) { /* construct test files and exit */

        H5open();
        HDfprintf(stdout, "Constructing test files: \n");
        HDfflush(stdout);

        i = 0;
        while ( ( FILENAMES[i] != NULL ) && ( i < TEST_FILES_TO_CONSTRUCT ) ) {

            HDfprintf(stdout, "   writing %s ... ", FILENAMES[i]);
            HDfflush(stdout);
            construct_test_file(i);

            if ( pass ) {

                HDprintf("done.\n");
                HDfflush(stdout);

            } else {

                HDprintf("failed.\n");
                HDexit(1);
            }
            i++;
        }

        HDfprintf(stdout, "Test file construction complete.\n");
        HDexit(0);

    } else if ( ici ) {

        if ( serial_insert_cache_image(file_idx, mpi_size) ) {

            HDexit(0);

        } else {

            HDfprintf(stderr, "\n\nCache image insertion failed.\n");
            HDfprintf(stderr, "  failure mssg = \"%s\"\n", failure_mssg);
            HDexit(1);
        }
    }

    HDassert(!setup);
    HDassert(!ici);

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* Attempt to turn off atexit post processing so that in case errors
     * happen during the test and the process is aborted, it will not get
     * hang in the atexit post processing in which it may try to make MPI
     * calls.  By then, MPI calls may not work.
     */
    if (H5dont_atexit() < 0){
        HDprintf("%d:Failed to turn off atexit processing. Continue.\n",
               mpi_rank);
    };

    H5open();

    if ( mpi_rank == 0 ) {
        HDprintf("===================================\n");
        HDprintf("Parallel metadata cache image tests\n");
        HDprintf("        mpi_size     = %d\n", mpi_size);
        HDprintf("===================================\n");
    }

    if ( mpi_size < 2 ) {

        if ( mpi_rank == 0 ) {

            HDprintf("    Need at least 2 processes.  Exiting.\n");
        }
        goto finish;
    }

    if ( mpi_rank == 0 ) { /* create test files */

        int child_status;
        pid_t child_pid;

        child_pid = fork();

        if ( child_pid == 0 ) { /* this is the child process */

            /* fun and games to shutup the compiler */
            char param0[32] = "t_cache_image";
            char param1[32] = "setup";
            char * child_argv[] = {param0, param1, NULL};

            /* we may need to play with the path here */
            if ( execv("t_cache_image", child_argv) == -1 ) {

                HDfprintf(stdout,
                          "execl() of setup process failed. errno = %d(%s)\n",
                          errno, strerror(errno));
                HDexit(1);
            }

        } else if ( child_pid != -1 ) {
            /* this is the parent process -- wait until child is done */
            if ( -1 == waitpid(child_pid, &child_status, WUNTRACED)) {

                HDfprintf(stdout, "can't wait on setup process.\n");

            } else if ( ! WIFEXITED(child_status) ) {

                HDfprintf(stdout, "setup process hasn't exitied.\n");

            } else if ( WEXITSTATUS(child_status) != 0 ) {

                HDfprintf(stdout, "setup process reports failure.\n");

            } else {

                HDfprintf(stdout,
                 "testfile construction complete -- proceeding with tests.\n");
            }
        } else { /* fork failed */

            HDfprintf(stdout, "can't create process to construct test file.\n");
        }
    }

    /* can't start test until test files exist */
    MPI_Barrier(MPI_COMM_WORLD);


    nerrs += verify_cache_image_RO(0,
                     H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY, mpi_rank);
#if 1
    nerrs += verify_cache_image_RO(1,
                     H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED, mpi_rank);
    nerrs += verify_cache_image_RW(0,
                     H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY, mpi_rank);
    nerrs += verify_cache_image_RW(1,
                     H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED, mpi_rank);
    nerrs += smoke_check_1(comm, info, mpi_rank, mpi_size);
#endif
finish:

    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);

    if ( mpi_rank == 0 ) {           /* only process 0 reports */
        HDsleep(10);
        HDprintf("===================================\n");
        if ( nerrs > 0 ) {
            HDprintf("***metadata cache image tests detected %d failures***\n",
                   nerrs);
        }
        else {
            HDprintf("metadata cache image tests finished with no failures\n");
        }
        HDprintf("===================================\n");
    }

    /* takedown_derived_types(); */

    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrs) because exit code is limited to 1byte */
    return(nerrs > 0);

} /* main() */
#else /* H5_HAVE_FORK */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_FORK */

