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
 * This program illustrates the usage of metadata journaling.
 */

#include <stdlib.h>
#include "hdf5.h"

#define HDF5_FILE_NAME 		"jnl_demo.h5"
#define JOURNAL_FILE_NAME	"jnl_demo.jnl"



/*-------------------------------------------------------------------------
 * Function:	mdj_api_example
 *
 * Purpose:	This example demonstrates enabling metadata journaling at file
 * 		creation time and enabling journaling on an open file.  It
 * 		also demonstrates disabling metadata journaling both manually
 * 		during a computation and automatically at file close.  Finally,
 * 		it demonstrates the use of H5Fflush() to keep the journal
 * 		file from becoming too large.
 *
 * 		We begin by creating an HDF5 file with journaling enabled.
 * 		The paths to the HDF5 and journal files are passed in, so
 * 		the only point to consider is whether the journal file
 * 		already exists, as HDF5 will refuse to overwrite it if
 * 		it does.  In this example, we just remove any pre-existing
 * 		journal file unconditionally, but you will probably wish
 * 		to do otherwise in your application.
 *
 *              With these preliminaries dealt with, we allocate a
 *              file access property list (FAPL).  Journaling uses some
 *              recent extensions to the superblock, so the first step
 *              is to call H5Pset_libver_bounds() to specify using the
 *              latest version of the HDF5 file format.
 *
 *              Next, we must set up the journaling property.  We could
 *              do this in several ways, but in this example we will use
 *              H5Pget_jnl_config() to get the default journaling
 *              configuration, modify that configuration as needed, and then
 *		use H5Pset_jnl_config() to replace the default journaling
 *              configuration with our own.  See the comments in the code
 *		for the particulars -- note that we must set the version
 *		field of the H5AC2_jnl_config_t struct before calling
 *              H5Pget_jnl_config().
 *
 *		After setting up the FAPL, we create the file as usual.
 *		Since the FAPL calls for metadata journaling, journaling
 *              will be enabled on this file.
 *
 *		With the file created and journaling running, we then go off
 *		and do what we want -- in this example we set up a selection
 *		of chunked datasets.  Note that these datasets (and our
 *		access pattern) are chosen to maximize the amount of dirty
 *		metadata generated.  This is done deliberately to exercise
 *              the matadata journaling features.  Your application will
 *		presumably be structured quite differently.
 *
 *		After the datasets are created, we then shut down journaling
 *		and re-enable it via H5Fget_jnl_config() and H5Fset_jnl_config()
 *		calls.  Note that when we re-enable journaling via the
 *		H5Fset_jnl_config() call, we don't need to set all the fields
 *		in the H5AC2_jnl_config_t struct again; we are simply re-using
 *		the configuration obtained via the H5Fget_jnl_config() call.
 *		(If we had originally opened the file without journaling and
 *		then wanted to enable journaling, we would have to set up the
 *		fields of the H5AC2_jnl_config_t struct in much the same way
 *		we did earlier in this example.  We would also have had to
 *		use H5Pset_libver_bounds() to set the FAPL to create the file
 *              initially with the latest HDF5 file format format.)
 *
 *		Having re-enabled journaling, we then proceed to write to
 *		our datasets.  Again, please note that our write strategy
 *		(round robin and small chunks) is designed to maximize
 *		dirty metadata generation and to load on the metadata cache.
 *		In your application, you should try to do just the opposite
 *		if possible.
 *
 *		However, since we are maximizing dirty metadata generation,
 *		the journal file will grow quickly.  This can be a problem,
 *		so from time to time we force truncation of the journal file
 *		via a call to H5Fflush().  This call flushes the HDF5 file,
 *		and then truncates the journal file, as the content of the
 *		journal becomes irrelevant after the file is flushed.
 *
 * 		After writing data to our datasets, we then do a number of
 * 		reads.  We could turn off journaling here, as we are not
 * 		modifying the file.  But since we are not generating any
 * 		dirty metadata, we aren't generating any journal entries
 * 		either, so it really doesn't matter.
 *
 * 		Finally, we close the HDF5 file.  Since journaling is enabled,
 * 		the call to H5Fclose() will flush the journal, flush the
 * 		metadata cache, truncate the journal, mark the file as not
 * 		having journaling in progress, and then delete the journal
 * 		file as part of the close.
 *
 *              This example may run for several minutes.  If you are
 *              interested in observing the example's behavior (or if you're
 *              just bored), run 'ls -l' every few seconds in the directory
 *              containing the journal file; you will be able to watch the
 *              journal file grow, see when it is truncated as the file is
 *              flushed, and watch it grow again.  This cycle is repeated many
 *              times through the course of the example.
 *
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              12/14/08
 *
 * Modifications:
 *
 * 		None.
 *
 *------------------------------------------------------------------------- */



#define CHUNK_SIZE              10
#define DSET_SIZE               (40 * CHUNK_SIZE)
#define NUM_DSETS               6
#define NUM_RANDOM_ACCESSES     200000

void
mdj_api_example(char * hdf5_file_name,
		char * jnl_file_name)
{
    const char * fcn_name = "mdj_api_example_test()";
    hbool_t valid_chunk;
    hid_t fapl_id = -1;
    hid_t file_id = -1;
    hid_t dataspace_id = -1;
    hid_t filespace_ids[NUM_DSETS];
    hid_t memspace_id = -1;
    hid_t dataset_ids[NUM_DSETS];
    hid_t properties;
    char dset_name[64];
    int i, j, k, l, m, n;
    int progress_counter;
    herr_t status;
    hsize_t dims[2];
    hsize_t a_size[2];
    hsize_t offset[2];
    hsize_t chunk_size[2];
    int data_chunk[CHUNK_SIZE][CHUNK_SIZE];
    H5AC2_jnl_config_t jnl_config_0;
    H5AC2_jnl_config_t jnl_config_1;

    fprintf(stdout, "running journaling example -- this may take a while:\n");
    fflush(stdout);

    if ( hdf5_file_name == NULL )
    {
        fprintf(stderr, "bad hdf5 file name.  Exiting...\n");
	exit(1);
    }

    if ( ( jnl_file_name == NULL ) ||
         ( strlen(jnl_file_name) > H5AC2__MAX_JOURNAL_FILE_NAME_LEN ) )
    {
        fprintf(stderr, "bad journal file name.  Exiting...\n");
	exit(1);
    }

    /* The HDF5 Library will refuse to overwrite an existing journal
     * file -- thus we delete it unconditionally here.  You probably
     * want to to be more cautious in your application.
     */
    remove(jnl_file_name);

    fprintf(stdout, "setting up FAPL...");
    fflush(stdout);

    /* Create a file access propertly list (FAPL). */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);

    if ( fapl_id < 0 ) {

        fprintf(stderr, "can't allocate FAPL.  Exiting...\n");
	exit(1);
    }


    /* Metadata journaling requires the latest version of the file format.  */
    if ( H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST,
			      H5F_LIBVER_LATEST) < 0 ) {

	fprintf(stderr,
		"can't set latest version of file format.  Exiting...\n");
	exit(1);
    }


    /* Get the current FAPL journaling configuration; this will normally be
     * the default. We could just write a predefined journal configuration
     * structure to the FAPL directly, but creating it in the manner
     * illustrated here shows off the H5Pget_jnl_config() call and is less
     * susceptible to API definition changes.
     */
    jnl_config_0.version = H5AC2__CURR_JNL_CONFIG_VER;

    status = H5Pget_jnl_config(fapl_id, &jnl_config_0);

    if ( status < 0 ) {

	fprintf(stdout, "Can't get FAPL jnl config.  Exiting...\n");
	exit(1);
    }


    /* Modify the current FAPL journaling configuration to enable
     * journaling as desired, then write the revised configuration
     * back to the FAPL.
     */
    jnl_config_0.enable_journaling = 1; /* TRUE */

    strcpy(jnl_config_0.journal_file_path, jnl_file_name);

    /* jnl_config_0.journal_recovered should always be FALSE unless
     * you are writing a new journal recovery tool and need to
     * tell the library that you have recovered the journal and
     * that the file is now readable.  As this field is set to
     * FALSE by default, we don't touch it here.
     */

    /* The journal buffer size should be some multiple of the block
     * size of the underlying file system.
     */
    jnl_config_0.jbrb_buf_size = (8 * 1024);

    /* The number of journal buffers should be either 1 or 2 when
     * synchronous I/O is used for journal writes.  If asynchronous I/O (AIO)
     * is used, the number should be large enough that the write of a buffer
     * will usually be complete by the time that buffer is needed again.
     */
    jnl_config_0.jbrb_num_bufs = 2;

    /* At present, HDF5 does not support AIO for journal writes, so this
     * field will be FALSE.
     */
    jnl_config_0.jbrb_use_aio = 0; /* FALSE */

    /* At present, only human-readable journal files are supported,
     * so this field will be TRUE for now.  Once machine readable journal
     * files become available, journaling should be faster and journal files
     * will be smaller.
     */
    jnl_config_0.jbrb_human_readable = 1; /* TRUE */

    status = H5Pset_jnl_config(fapl_id, &jnl_config_0);

    if ( status < 0 ) {

	fprintf(stderr, "can't set FAPL jnl config.  Exiting...\n");
	exit(1);
    }

    fprintf(stdout, "done.\n");
    fprintf(stdout, "creating the hdf5 file...");
    fflush(stdout);

    /* Now open the file using the FAPL we have created. */
    file_id = H5Fcreate(hdf5_file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

    if ( file_id < 0 ) {

	fprintf(stderr, "H5Fcreate() failed.  Exiting...\n");
	exit(1);
    }

    fprintf(stdout, "done.\n");
    fprintf(stdout, "creating the datasets...");
    fflush(stdout);

    /* Create the datasets. */
    i = 0;

    while ( i < NUM_DSETS )
    {
        /* Create a dataspace for the chunked dataset. */
        dims[0] = DSET_SIZE;
        dims[1] = DSET_SIZE;
        dataspace_id = H5Screate_simple(2, dims, NULL);

        if ( dataspace_id < 0 ) {

	    fprintf(stderr, "H5Screate_simple(%d) faile.  Exiting...\n", i);
	    exit(1);
	}

        /* Set the dataset creation property list to specify that the raw data
         * is to be partitioned into 10X10 element chunks.
         */

        chunk_size[0] = CHUNK_SIZE;
        chunk_size[1] = CHUNK_SIZE;
        properties = H5Pcreate(H5P_DATASET_CREATE);

        if ( properties < 0 ) {

            fprintf(stderr, "H5Pcreate(%d) failed.  Exiting...\n", i);
	    exit(1);
	}

        if ( H5Pset_chunk(properties, 2, chunk_size) < 0 ) {

            fprintf(stderr, "H5Pset_chunk(%d) failed.  Exiting...\n", i);
	    exit(1);
	}

        /* Create the dataset. */
        sprintf(dset_name, "/dset%03d", i);
        dataset_ids[i] = H5Dcreate2(file_id, dset_name, H5T_STD_I32BE,
			            dataspace_id, H5P_DEFAULT,
				    properties, H5P_DEFAULT);

        if ( dataset_ids[i] < 0 ) {

	    fprintf(stderr, "H5Dcreate(%d) failed.  Exiting...\n", i);
	    exit(1);
	}


        /* Get the file dataspace ID. */
        filespace_ids[i] = H5Dget_space(dataset_ids[i]);

        if ( filespace_ids[i] < 0 ) {

	    fprintf(stderr, "H5Dget_space(%d) failed.  Exiting...\n", i);
	    exit(1);
        }

        i++;
    }

    fprintf(stdout, "done.\n");
    fprintf(stdout, "disabling and re-enabling journaling...");
    fflush(stdout);

    /* Just for purposes of demonstration, turn off journaling and
     * turn it back on again.  Note that this will force a
     * flush of the file, including all metadata.  Turning off
     * journaling will also cause HDF5 to close and discard the
     * journal file after all metadata is on disk.
     */
    jnl_config_1.version = H5AC2__CURR_JNL_CONFIG_VER;

    status = H5Fget_jnl_config(file_id, &jnl_config_1);

    if ( status < 0 ) {

        fprintf(stderr, "H5Fget_mdc_config() failed.  Exiting...\n");
	exit(1);
    }

    jnl_config_1.enable_journaling = 0; /* FALSE */

    status = H5Fset_jnl_config(file_id, &jnl_config_1);

    if ( status < 0 ) {

        fprintf(stderr, "H5Fset_mdc_config(1) failed.  Exiting...\n");
	exit(1);
    }


    /* Note that here we simply set jnl_config_1.enable_journaling to
     * TRUE and pass it back to the HDF5 library via the
     * H5Fset_jnl_config() call.
     *
     * We can do this because jnl_config_1 reflected the current
     * journaling configuration when we got it from the library
     * via the H5Fget_jnl_config() call, and H5Fset_mdc_config()
     * doesn't change the values of any fields.
     */
    jnl_config_1.enable_journaling = 1; /* TRUE */

    status = H5Fset_jnl_config(file_id, &jnl_config_1);

    if ( status < 0 ) {

        fprintf(stderr, "H5Fset_mdc_config(2) failed.  Exiting...\n");
	exit(1);
    }

    fprintf(stdout, "done.\n");
    fprintf(stdout, "setting up to write the datasets...");
    fflush(stdout);

    /* Create the memory dataspace to be used to read and write chunks. */
    dims[0] = CHUNK_SIZE;
    dims[1] = CHUNK_SIZE;
    memspace_id = H5Screate_simple(2, dims, NULL);

    if ( memspace_id < 0 ) {

        fprintf(stderr, "H5Screate_simple() failed.  Exiting...\n");
	exit(1);
    }

    /* Select in-memory hyperslab. */
    offset[0] = 0;  /*offset of hyperslab in memory*/
    offset[1] = 0;
    a_size[0] = CHUNK_SIZE;  /*size of hyperslab*/
    a_size[1] = CHUNK_SIZE;
    status = H5Sselect_hyperslab(memspace_id, H5S_SELECT_SET, offset, NULL,
                                 a_size, NULL);

    if ( status < 0 ) {

        fprintf(stderr, "H5Sselect_hyperslab() failed.  Exiting...\n");
	exit(1);
    }

    fprintf(stdout, "done.\n");
    fprintf(stdout, "writing the datasets...");
    fflush(stdout);

    /* Initialize all datasets on a round robin basis. */
    i = 0;
    progress_counter = 0;

    while ( i < DSET_SIZE )
    {
        j = 0;
        while ( j < DSET_SIZE )
        {
            m = 0;
            while ( m < NUM_DSETS )
            {
                /* Initialize the hyperslab. */
                for ( k = 0; k < CHUNK_SIZE; k++ )
                {
                    for ( l = 0; l < CHUNK_SIZE; l++ )
                    {
                        data_chunk[k][l] = (DSET_SIZE * DSET_SIZE * m) +
                                           (DSET_SIZE * (i + k)) + j + l;
                    }
                }

                /* Select on-disk hyperslab. */
                offset[0] = i; /*offset of hyperslab in file*/
                offset[1] = j;
                a_size[0] = CHUNK_SIZE;   /*size of hyperslab*/
                a_size[1] = CHUNK_SIZE;
                status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                         offset, NULL, a_size, NULL);

                if ( status < 0 ) {

		    fprintf(stderr,
		        "disk H5Sselect_hyperslab() failed.  Exiting...\n");
		    exit(1);
                }

                /* Write the chunk to the file. */
                status = H5Dwrite(dataset_ids[m], H5T_NATIVE_INT, memspace_id,
                                  filespace_ids[m], H5P_DEFAULT, data_chunk);

                if ( status < 0 ) {

		    fprintf(stderr, "H5Dwrite() failed.  Exiting...\n");
		    exit(1);
                }
                m++;
            }
            j += CHUNK_SIZE;
        }

        i += CHUNK_SIZE;

	/* We are generating a lot of dirty metadata here, all of which
	 * will wind up in the journal file.  To keep the journal file
	 * from getting too big (and to make sure the raw data is on
	 * disk), we should do an occasional flush of the HDF5 file.
	 *
	 * This will force all metadata to disk and cause the journal
	 * file to be truncated.
	 *
	 * On the other hand, it will impose a significant file I/O
	 * overhead, and slow us down. (Try it both ways.)
	 */
#if 1
	status = H5Fflush(file_id, H5F_SCOPE_GLOBAL);

        if ( status < 0 ) {

	    fprintf(stderr, "H5Fflush() failed.  Exiting...\n");
	    exit(1);
        }
#endif
	fprintf(stdout, ".");
        fflush(stdout);
    }

    /* Do random reads on all datasets. */

    fprintf(stdout, "done.\n");
    fprintf(stdout, "doing random reads across all datasets...");
    fflush(stdout);

    n = 0;
    progress_counter = 0;
    while ( n < NUM_RANDOM_ACCESSES )
    {
        m = rand() % NUM_DSETS;
        i = (rand() % (DSET_SIZE / CHUNK_SIZE)) * CHUNK_SIZE;
        j = (rand() % (DSET_SIZE / CHUNK_SIZE)) * CHUNK_SIZE;

        /* Select on-disk hyperslab. */
        offset[0] = i; /*offset of hyperslab in file*/
        offset[1] = j;
        a_size[0] = CHUNK_SIZE;   /*size of hyperslab*/
        a_size[1] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                     offset, NULL, a_size, NULL);

        if ( status < 0 ) {

            fprintf(stderr, "disk hyperslab create failed.  Exiting...\n");
	    exit(1);
        }

        /* Read the chunk from the file. */
        status = H5Dread(dataset_ids[m], H5T_NATIVE_INT, memspace_id,
                         filespace_ids[m], H5P_DEFAULT, data_chunk);

        if ( status < 0 ) {

            fprintf(stderr, "H5Dread() failed.  Exiting...\n");
            exit(1);
        }

        /* Validate the hyperslab. */
        valid_chunk = 1; /* TRUE */
        for ( k = 0; k < CHUNK_SIZE; k++ )
        {
            for ( l = 0; l < CHUNK_SIZE; l++ )
            {
                 if ( data_chunk[k][l]
                      !=
                      ((DSET_SIZE * DSET_SIZE * m) +
                       (DSET_SIZE * (i + k)) + j + l) ) {

                     valid_chunk = 0; /* FALSE */
                }
            }
        }

        if ( ! valid_chunk ) {

            fprintf(stderr, "slab validation failed.  Exiting...\n");
            exit(1);
        }

        n++;
    }

    fprintf(stdout, "done.\n");
    fprintf(stdout, "closing datasets 1 and up...");
    fflush(stdout);

    /* Close the file dataspaces we are done with. */
    i = 1;
    while ( i < NUM_DSETS )
    {
        if ( H5Sclose(filespace_ids[i]) < 0 ) {

            fprintf(stderr, "H5Sclose(%d) failed.  Exiting...\n", i);
	    exit(1);
        }
        i++;
    }


    /* Close the datasets we are done with. */
    i = 1;
    while ( i < NUM_DSETS )
    {
        if ( H5Dclose(dataset_ids[i]) < 0 ) {

            fprintf(stderr, "H5Dclose(%d) failed.  Exiting...\n", i);
	    exit(1);
        }
        i++;
    }

    fprintf(stdout, "done.\n");
    fprintf(stdout, "doing random reads on dataset 0 only...");
    fflush(stdout);

    /* Do random reads on dataset 0 only. */
    m = 0;
    n = 0;
    progress_counter = 0;
    while ( n < NUM_RANDOM_ACCESSES )
    {
        i = (rand() % (DSET_SIZE / CHUNK_SIZE)) * CHUNK_SIZE;
        j = (rand() % (DSET_SIZE / CHUNK_SIZE)) * CHUNK_SIZE;

        /* Select on-disk hyperslab. */
        offset[0] = i; /*offset of hyperslab in file*/
        offset[1] = j;
        a_size[0] = CHUNK_SIZE;   /*size of hyperslab*/
        a_size[1] = CHUNK_SIZE;
        status = H5Sselect_hyperslab(filespace_ids[m], H5S_SELECT_SET,
                                     offset, NULL, a_size, NULL);

        if ( status < 0 ) {

            fprintf(stderr, "disk hyperslab create failed.  Exiting...\n");
	    exit(1);
        }

        status = H5Dread(dataset_ids[m], H5T_NATIVE_INT, memspace_id,
                         filespace_ids[m], H5P_DEFAULT, data_chunk);

        if ( status < 0 ) {

            fprintf(stderr, "H5Dread() failed.  Exiting...\n");
	    exit(1);
        }

        /* Validate the hyperslab. */
        valid_chunk = 1; /* TRUE */
        for ( k = 0; k < CHUNK_SIZE; k++ )
        {
           for ( l = 0; l < CHUNK_SIZE; l++ )
           {
                if ( data_chunk[k][l]
                     !=
                     ((DSET_SIZE * DSET_SIZE * m) +
                      (DSET_SIZE * (i + k)) + j + l) ) {

                    valid_chunk = 0; /* FALSE */
                }
            }
        }

        if ( ! valid_chunk ) {

            fprintf(stderr, "slab validation failed.  Exiting...\n");
	    exit(1);
        }

        n++;
    }

    fprintf(stdout, "done.\n");
    fprintf(stdout, "closing down...");
    fflush(stdout);

    /* Close file dataspace 0. */
    if ( H5Sclose(filespace_ids[0]) < 0 ) {

        fprintf(stderr, "H5Sclose(filespace_ids[0]) failed.  Exiting...\n");
        exit(1);
    }

    /* Close the dataspace. */
    if ( H5Sclose(dataspace_id) < 0 ) {

        fprintf(stderr, "H5Sclose(dataspace_id) failed.  Exiting...\n");
        exit(1);
    }

    /* Close the memmory dataspace */
    if ( H5Sclose(memspace_id) < 0 ) {

        fprintf(stderr, "H5Sclose(memspace_id) failed.  Exiting...\n");
        exit(1);
    }

    /* Close dataset 0. */
    if ( H5Dclose(dataset_ids[0]) < 0 ) {

        fprintf(stderr, "H5Dclose(dataset_ids[0]) failed.  Exiting...\n");
        exit(1);
    }

    /* Close the file and delete it. */
    if ( H5Fclose(file_id) < 0  ) {

        fprintf(stderr, "H5Fclose() failed.  Exiting...\n");
        exit(1);
    }
    else if ( remove(hdf5_file_name) < 0 ) {

        fprintf(stderr, "remove(hdf5_file_name) failed.  Exiting...\n");
        exit(1);
    }

    fprintf(stdout, "done.\n");
    fflush(stdout);

    return;

} /* mdj_api_example() */

int
main(void)
{
    mdj_api_example(HDF5_FILE_NAME, JOURNAL_FILE_NAME);
    return(0);
}
