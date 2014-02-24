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

/*-------------------------------------------------------------------------
 *
 * Created:     swmr_start_write.c
 *
 * Purpose:     This program enables SWMR writing mode via H5Fstart_swmr_write().
 *		It writes data to a randomly selected subset of the datasets
 *              in the SWMR test file; and it is intended to run concurrently
 *              with the swmr_reader program.
 *
 * NOTE: The routines in this program are basically copied and modified from
 *	 swmr*.c.
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include <assert.h>
#include <sys/time.h>

#include "swmr_common.h"

/********************/
/* Local Prototypes */
/********************/

static hid_t create_file(const char *filename, unsigned verbose,
    const char *index_type, unsigned random_seed);
static int create_datasets(hid_t fid, int comp_level, unsigned verbose);
static int create_close_datasets(hid_t fid, int comp_level, unsigned verbose);
static hid_t open_datasets(hid_t fid, unsigned verbose);
static hid_t open_file(const char *filename, unsigned verbose);

static int add_records(hid_t fid, unsigned verbose, unsigned long nrecords,
    unsigned long flush_count);
static void usage(void);

#define CHUNK_SIZE      50      /* Chunk size for created datasets */


/*-------------------------------------------------------------------------
 * Function:    create_file
 *
 * Purpose:     Creates the HDF5 file (without SWMR access) which
 *              which will be used for testing H5Fstart_swmr_write().
 *
 * Parameters:  
 *		filename: The SWMR test file's name.
 *              verbose: whether verbose console output is desired.
 *              index_type: The chunk index type (b1 | b2 | ea | fa)
 *		random_seed: The random seed to store in the file.  
 *			     The sparse tests use this value.
 *	
 * Return:      Success:    the file ID
 *              Failure:     -1
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_file(const char *filename, unsigned verbose,
    const char *index_type, unsigned random_seed)
{
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t fcpl;         /* File creation property list */
    hid_t fapl;         /* File access property list */
    hid_t sid;          /* Dataspace ID */
    hid_t aid;          /* Attribute ID */
    hsize_t max_dims[2] = {1, H5S_UNLIMITED}; /* Dataset maximum dimensions */
#ifdef FILLVAL_WORKS
    symbol_t fillval;   /* Dataset fill value */
#endif /* FILLVAL_WORKS */

    assert(filename);
    assert(index_type);

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        return -1;

    /* We ALWAYS select the latest file format for SWMR */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        return -1;

    /* There are two chunk indexes tested here.
     * With one unlimited dimension, we get the extensible array index
     * type, with two unlimited dimensions, we get a v-2 B-tree.
     */
    if(!strcmp(index_type, "b2"))
        max_dims[0] = H5S_UNLIMITED;

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Creating file without SWMR access\n");

    /* Create the file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        return -1;

    /* Close file creation property list */
    if(H5Pclose(fcpl) < 0)
        return -1;

    /* Close file access property list */
    if(H5Pclose(fapl) < 0)
        return -1;

    /* Create attribute with (shared) random number seed - for sparse test */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        return -1;
    if((aid = H5Acreate2(fid, "seed", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return -1;
    if(H5Awrite(aid, H5T_NATIVE_UINT, &random_seed) < 0)
        return -1;
    if(H5Sclose(sid) < 0)
        return -1;
    if(H5Aclose(aid) < 0)
        return -1;

    return fid;
} /* end create_file() */


/*-------------------------------------------------------------------------
 * Function:    create_datasets
 *
 * Purpose:     Create datasets (and keep them opened) which will be used for testing 
 *		H5Fstart_swmr_write().
 *
 * Parameters:  
 *		fid: file ID for the SWMR test file
 *              comp_level: the compresssion level
 *              verbose: whether verbose console output is desired.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static int
create_datasets(hid_t fid, int comp_level, unsigned verbose)
{
    hid_t dcpl;         /* Dataset creation property list */
    hid_t tid;          /* Datatype for dataset elements */
    hid_t sid;          /* Dataspace ID */
    hsize_t dims[2] = {1, 0}; /* Dataset starting dimensions */
    hsize_t max_dims[2] = {1, H5S_UNLIMITED}; /* Dataset maximum dimensions */
    hsize_t chunk_dims[2] = {1, CHUNK_SIZE}; /* Chunk dimensions */
    unsigned u, v;      /* Local index variable */

    /* Create datatype for creating datasets */
    if((tid = create_symbol_datatype()) < 0)
        return -1;

    /* Create dataspace for creating datasets */
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0)
        return -1;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return -1;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        return -1;
    if(comp_level >= 0) {
        if(H5Pset_deflate(dcpl, (unsigned)comp_level) < 0)
            return -1;
    } /* end if */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Creating datasets\n");

    /* Create the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++) {

            if((symbol_info[u][v].dsid = H5Dcreate2(fid, symbol_info[u][v].name, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                return -1;
            symbol_info[u][v].nrecords = 0;

        } /* end for */

    return 0;
} /* create_datasets() */


/*-------------------------------------------------------------------------
 * Function:    create_close_datasets
 *
 * Purpose:     Create and close datasets which will be used for testing 
 *		H5Fstart_swmr_write().
 *
 * Parameters:  
 *		fid: file ID for the SWMR test file
 *              comp_level: the compresssion level
 *              verbose: whether verbose console output is desired.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static int
create_close_datasets(hid_t fid, int comp_level, unsigned verbose)
{
    hid_t dcpl;         /* Dataset creation property list */
    hid_t tid;          /* Datatype for dataset elements */
    hid_t sid;          /* Dataspace ID */
    hsize_t dims[2] = {1, 0}; /* Dataset starting dimensions */
    hsize_t max_dims[2] = {1, H5S_UNLIMITED}; /* Dataset maximum dimensions */
    hsize_t chunk_dims[2] = {1, CHUNK_SIZE}; /* Chunk dimensions */
    unsigned u, v;      /* Local index variable */

    /* Create datatype for creating datasets */
    if((tid = create_symbol_datatype()) < 0)
        return -1;

    /* Create dataspace for creating datasets */
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0)
        return -1;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return -1;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        return -1;
    if(comp_level >= 0) {
        if(H5Pset_deflate(dcpl, (unsigned)comp_level) < 0)
            return -1;
    } /* end if */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Creating datasets\n");

    /* Create the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++) {
            hid_t dsid;         /* Dataset ID */
            char name_buf[64];

            generate_name(name_buf, u, v);
            if((dsid = H5Dcreate2(fid, name_buf, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                return -1;

            if(H5Dclose(dsid) < 0)
                return -1;
        } /* end for */

    /* Closing */
    if(H5Pclose(dcpl) < 0)
        return -1;
    if(H5Sclose(sid) < 0)
        return -1;
    if(H5Tclose(tid) < 0)
        return -1;

    return 0;
} /* create_close_datasets() */


/*-------------------------------------------------------------------------
 * Function:    open_file
 *
 * Purpose:     Opens the HDF5 test file without SWMR access.
 *
 * Parameters:  
 *		filename: The filename of the HDF5 file to open
 *              verbose: whether or not to emit verbose console messages
 *
 * Return:      Success: The file ID of the opened SWMR file
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static hid_t
open_file(const char *filename, unsigned verbose)
{
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t fapl;         /* File access property list */

    assert(filename);

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        return -1;

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Opening the file without SWMR access: %s\n", filename);

    /* Open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        return -1;

    /* Close file access property list */
    if(H5Pclose(fapl) < 0)
        return -1;

    return fid;
} /* Open file() */



/*-------------------------------------------------------------------------
 * Function:    open_datasets
 *
 * Purpose:     Opens the datasets.
 *
 * Parameters:  
*		filename: the filename of the SWMR HDF5 file to open
 *              verbose: whether or not to emit verbose console messages
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static int
open_datasets(hid_t fid, unsigned verbose)
{
    unsigned u, v;      /* Local index variable */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Opening datasets\n");

    /* Open the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++) {
            if((symbol_info[u][v].dsid = H5Dopen2(fid, symbol_info[u][v].name, H5P_DEFAULT)) < 0)
                return -1;
            symbol_info[u][v].nrecords = 0;
        } /* end for */

    return 0;
} /* open_datasets() */


/*-------------------------------------------------------------------------
 * Function:    add_records
 *
 * Purpose:     Writes a specified number of records to random datasets in
 *              the SWMR test file.
 *
 * Parameters:  
*		fid: The file ID of the SWMR HDF5 file
 *              verbose: Whether or not to emit verbose console messages
 *              nrecords: # of records to write to the datasets
 *              flush_count: # of records to write before flushing the file to disk
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static int
add_records(hid_t fid, unsigned verbose, unsigned long nrecords, unsigned long flush_count)
{
    hid_t tid;                              /* Datatype ID for records */
    hid_t mem_sid;                          /* Memory dataspace ID */
    hsize_t start[2] = {0, 0}, count[2] = {1, 1}; /* Hyperslab selection values */
    hsize_t dim[2] = {1, 0};                /* Dataspace dimensions */
    symbol_t record;                        /* The record to add to the dataset */
    H5AC_cache_config_t mdc_config_orig;    /* Original metadata cache configuration */
    H5AC_cache_config_t mdc_config_cork;    /* Corked metadata cache configuration */
    unsigned long rec_to_flush;             /* # of records left to write before flush */
    unsigned long u, v;                     /* Local index variables */

    assert(fid >= 0);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    memset(&record, 0, sizeof(record));

    /* Create a dataspace for the record to add */
    if((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        return -1;

    /* Create datatype for appending records */
    if((tid = create_symbol_datatype()) < 0)
        return -1;

    /* Get the current metadata cache configuration, and set up the corked
     * configuration */
    mdc_config_orig.version = H5AC__CURR_CACHE_CONFIG_VERSION;
    if(H5Fget_mdc_config(fid, &mdc_config_orig) < 0)
        return -1;
    memcpy(&mdc_config_cork, &mdc_config_orig, sizeof(mdc_config_cork));
    mdc_config_cork.evictions_enabled = FALSE;
    mdc_config_cork.incr_mode = H5C_incr__off;
    mdc_config_cork.flash_incr_mode = H5C_flash_incr__off;
    mdc_config_cork.decr_mode = H5C_decr__off;

    /* Add records to random datasets, according to frequency distribution */
    rec_to_flush = flush_count;
    for(u = 0; u < nrecords; u++) {
        symbol_info_t *symbol;  /* Symbol to write record to */
        hid_t file_sid;         /* Dataset's space ID */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset();

        /* Set the record's ID (equal to its position) */
        record.rec_id = symbol->nrecords;

        /* Get the coordinate to write */
        start[1] = symbol->nrecords;

        /* Extend the dataset's dataspace to hold the new record */
        symbol->nrecords++;
        dim[1] = symbol->nrecords;
        if(H5Dset_extent(symbol->dsid, dim) < 0)
            return -1;

        /* Get the dataset's dataspace */
        if((file_sid = H5Dget_space(symbol->dsid)) < 0)
            return -1;

        /* Choose the last record in the dataset */
        if(H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            return -1;

        /* Write record to the dataset */
        if(H5Dwrite(symbol->dsid, tid, mem_sid, file_sid, H5P_DEFAULT, &record) < 0)
            return -1;

        /* Close the dataset's dataspace */
        if(H5Sclose(file_sid) < 0)
            return -1;

        /* Check for flushing file */
        if(flush_count > 0) {
            /* Decrement count of records to write before flushing */
            rec_to_flush--;

            /* Check for counter being reached */
            if(0 == rec_to_flush) {
                /* Flush contents of file */
                if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
                    return -1;

                /* Reset flush counter */
                rec_to_flush = flush_count;
            } /* end if */
        } /* end if */
    } /* end for */

    /* Close the memory dataspace */
    if(H5Sclose(mem_sid) < 0)
        return -1;

    /* Close the datatype */
    if(H5Tclose(tid) < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Closing datasets\n");

    /* Close the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++)
            if(H5Dclose(symbol_info[u][v].dsid) < 0)
                return -1;

    return 0;
} /* add_records() */

static void
usage(void)
{
    printf("\n");
    printf("Usage error!\n");
    printf("\n");
    printf("Usage: swmr_start_write [-f <# of records to write between flushing file contents>]\n");
    printf("    [-i <index type>] [-c <deflate compression level>]\n");
    printf("    [-r <random seed>] [-q] <# of records>\n");
    printf("\n");
    printf("<# of records to write between flushing file contents> should be 0\n");
    printf("(for no flushing) or between 1 and (<# of records> - 1).\n");
    printf("\n");
    printf("<index type> should be b2 or ea\n");
    printf("\n");
    printf("<deflate compression level> should be -1 (for no compression) or 0-9\n");
    printf("\n");
    printf("<# of records> must be specified.\n");
    printf("\n");
    printf("Defaults to flushing every 10000 records ('-f 10000'),\n");
    printf("v1 b-tree indexing (-i b1), compression ('-c -1'),\n");
    printf("will generate a random seed (no -r given), and verbose (no '-q' given)\n");
    printf("\n");
    exit(1);
} /* usage() */

/*
 * Can test with different scenarios as below:
 *	1) create_file(), create_datasets(), H5Fstart_swmr_write(), add_records(), H5Fclose().
 *	2) create_file(), create_close_datasets(), open_datasets(), H5Fstart_swmr_write(), add_records(), H5Fclose().
 *	3) create_file(), create_close_datasets(), H5Fclose(),
 *	   open_file(), open_dataset(), H5Fstart_swmr_write(), add_records(), H5Fclose().
 */
int main(int argc, const char *argv[])
{
    hid_t fid;                  	/* File ID for file opened */
    long nrecords = 0;          	/* # of records to append */
    long flush_count = 10000;   	/* # of records to write between flushing file */
    unsigned verbose = 1;       	/* Whether to emit some informational messages */
    unsigned use_seed = 0;      	/* Set to 1 if a seed was set on the command line */
    unsigned random_seed = 0;   	/* Random # seed */
    int comp_level = -1;		/* Compression level (-1 is no compression) */
    const char *index_type = "b1";	/* Chunk index type */
    unsigned u;                 	/* Local index variable */
    int temp;				/* Temporary variable */

    /* Parse command line options */
    if(argc < 2)
        usage();
    if(argc > 1) {
        u = 1;
        while(u < (unsigned)argc) {
            if(argv[u][0] == '-') {
                switch(argv[u][1]) {
		    /* Compress dataset chunks */
                    case 'c':
                        comp_level = atoi(argv[u + 1]);
                        if(comp_level < -1 || comp_level > 9)
                            usage();
                        u += 2;
                        break;

                    /* Chunk index type */
                    case 'i':
                        index_type = argv[u + 1];
                        if(strcmp(index_type, "ea")
                                && strcmp(index_type, "b2"))
                            usage();
                        u += 2;
                        break;

                    /* # of records to write between flushing file */
                    case 'f':
                        flush_count = atol(argv[u + 1]);
                        if(flush_count < 0)
                            usage();
                        u += 2;
                        break;

                    /* Be quiet */
                    case 'q':
                        verbose = 0;
                        u++;
                        break;
                    
                    /* Random # seed */
                    case 'r':
                        use_seed = 1;
                        temp = atoi(argv[u + 1]);
                        if(temp < 0)
                            usage();
                        else
                            random_seed = (unsigned)temp;
                        u += 2;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            } /* end if */
            else {
                /* Get the number of records to append */
                nrecords = atol(argv[u]);
                if(nrecords <= 0)
                    usage();

                u++;
            } /* end else */
        } /* end while */
    } /* end if */

    if(nrecords <= 0)
        usage();
    if(flush_count >= nrecords)
        usage();

    /* Emit informational message */
    if(verbose) {
        fprintf(stderr, "Parameters:\n");
        fprintf(stderr, "\tindex type = %s\n", index_type);
        fprintf(stderr, "\tcompression level = %d\n", comp_level);
        fprintf(stderr, "\t# of records between flushes = %ld\n", flush_count);
        fprintf(stderr, "\t# of records to write = %ld\n", nrecords);
    } /* end if */

    /* Set the random seed */
    if(0 == use_seed) {
        struct timeval t;
        gettimeofday(&t, NULL);
        random_seed = (unsigned)((t.tv_sec * 1000) + t.tv_usec);
    } /* end if */
    srandom(random_seed);
    /* ALWAYS emit the random seed for possible debugging */
    fprintf(stderr, "Using writer random seed: %u\n", random_seed);

    /* Create the test file */
    if((fid = create_file(FILENAME, verbose, index_type, random_seed)) < 0) {
        fprintf(stderr, "Error creating the file...\n");
        exit(1);
    }

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Generating symbol names\n");

    /* Generate dataset names */
    if(generate_symbols() < 0)
        return -1;

    /* Create the datasets in the file */
    if(create_datasets(fid, comp_level, verbose) < 0) {
        fprintf(stderr, "Error creating datasets...\n");
        exit(1);
    }

    /* Enable SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0) {
        fprintf(stderr, "Error starting SWMR writing mode...\n");
        exit(1);
    }

#ifdef OUT
    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Creating and closing datasets: %s\n", FILENAME);

    /* Create and close the datasets in the file */
    if(create_close_datasets(fid, comp_level, verbose) < 0) {
        fprintf(stderr, "Error creating datasets...\n");
        exit(1);
    }

    /* Close the file */
    if(H5Fclose(fid) < 0) {
        fprintf(stderr, "Error closing file!\n");
        exit(1);
    } /* end if */

    /* Open the file */
    if((fid = open_file(FILENAME, verbose)) < 0) {
        fprintf(stderr, "Error opening the file...\n");
        exit(1);
    }


    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Generating symbol names\n");

    /* Generate dataset names */
    if(generate_symbols() < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Opening datasets: %s\n", FILENAME);

    /* Open the file's datasets */
    if(open_datasets(fid, verbose) < 0) {
        fprintf(stderr, "Error opening datasets...\n");
        exit(1);
    } /* end if */


    /* Enable SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0) {
        fprintf(stderr, "Error starting SWMR writing mode...\n");
        exit(1);
    }
#endif

    /* Send a message to indicate "H5Fopen" is complete--releasing the file lock */
    h5_send_message(WRITER_MESSAGE);

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Adding records\n");

    /* Append records to datasets */
    if(add_records(fid, verbose, (unsigned long)nrecords, (unsigned long)flush_count) < 0) {
        fprintf(stderr, "Error appending records to datasets!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Releasing symbols\n");

    /* Clean up the symbols */
    if(shutdown_symbols() < 0) {
        fprintf(stderr, "Error releasing symbols!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Closing the file\n");

    /* Close objects opened */
    if(H5Fclose(fid) < 0) {
        fprintf(stderr, "Error closing file!\n");
        exit(1);
    } /* end if */

    return 0;
} /* main() */
