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

/*-------------------------------------------------------------------------
 *
 * Created:     swmr_start_write.c
 *
 * Purpose:     This program enables SWMR writing mode via H5Fstart_swmr_write().
 *              It writes data to a randomly selected subset of the datasets
 *              in the SWMR test file; and it is intended to run concurrently
 *              with the swmr_reader program.
 *
 * NOTE: The routines in this program are basically copied and modified from
 *          swmr*.c.
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include "h5test.h"
#include "swmr_common.h"

/********************/
/* Local Prototypes */
/********************/

static hid_t create_file(const char *filename, hbool_t verbose,
    FILE *verbose_file, unsigned random_seed);
static int create_datasets(hid_t fid, int comp_level, hbool_t verbose,
    FILE *verbose_file, const char *index_type);
static int add_records(hid_t fid, hbool_t verbose, FILE *verbose_file,
    unsigned long nrecords, unsigned long flush_count);
static void usage(void);

#define CHUNK_SIZE      50      /* Chunk size for created datasets */


/*-------------------------------------------------------------------------
 * Function:    create_file
 *
 * Purpose:     Creates the HDF5 file (without SWMR access) which
 *              which will be used for testing H5Fstart_swmr_write().
 *
 * Parameters:  
 *              filename: The SWMR test file's name.
 *              verbose: whether verbose console output is desired.
 *              verbose_file: file pointer for verbose output
 *              random_seed: The random seed to store in the file.  
 *              The sparse tests use this value.
 *
 * Return:      Success:    the file ID
 *              Failure:     -1
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_file(const char *filename, hbool_t verbose, FILE *verbose_file,
    unsigned random_seed)
{
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t fcpl;         /* File creation property list */
    hid_t fapl;         /* File access property list */
    hid_t sid;          /* Dataspace ID */
    hid_t aid;          /* Attribute ID */

    HDassert(filename);

    /* Create file access property list */
    if((fapl = h5_fileaccess()) < 0)
        return -1;

    /* We ALWAYS select the latest file format for SWMR */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        return -1;

#ifdef QAK
    if(verbose) {
        char verbose_name[1024];

        HDsnprintf(verbose_name, sizeof(verbose_name), "swmr_start_write.log.%u", random_seed);

        H5Pset_fapl_log(fapl, verbose_name, H5FD_LOG_ALL, (size_t)(512 * 1024 * 1024));
    } /* end if */
#endif /* QAK */

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        HDfprintf(verbose_file, "Creating file without SWMR access\n");

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
 *              H5Fstart_swmr_write().
 *
 * Parameters:  
 *              fid: file ID for the SWMR test file
 *              comp_level: the compresssion level
 *              index_type: The chunk index type (b1 | b2 | ea | fa)
 *              verbose: whether verbose console output is desired.
 *              verbose_file: file pointer for verbose output
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static int
create_datasets(hid_t fid, int comp_level, hbool_t verbose, FILE *verbose_file,
    const char *index_type)
{
    hid_t dcpl;         /* Dataset creation property list */
    hid_t tid;          /* Datatype for dataset elements */
    hid_t sid;          /* Dataspace ID */
    hsize_t dims[2] = {1, 0}; /* Dataset starting dimensions */
    hsize_t max_dims[2] = {1, H5S_UNLIMITED}; /* Dataset maximum dimensions */
    hsize_t chunk_dims[2] = {1, CHUNK_SIZE}; /* Chunk dimensions */
    unsigned u, v;      /* Local index variable */

    HDassert(index_type);

    /* Create datatype for creating datasets */
    if((tid = create_symbol_datatype()) < 0)
        return -1;

    /* There are two chunk indexes tested here.
     * With one unlimited dimension, we get the extensible array index
     * type, with two unlimited dimensions, we get a v-2 B-tree.
     */
    if(!HDstrcmp(index_type, "b2"))
        max_dims[0] = H5S_UNLIMITED;

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
        HDfprintf(verbose_file, "Creating datasets\n");

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
 * Function:    add_records
 *
 * Purpose:     Writes a specified number of records to random datasets in
 *              the SWMR test file.
 *
 * Parameters:  
 *              fid: The file ID of the SWMR HDF5 file
 *              verbose: Whether or not to emit verbose console messages
 *              verbose_file: file pointer for verbose output
 *              nrecords: # of records to write to the datasets
 *              flush_count: # of records to write before flushing the file to disk
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static int
add_records(hid_t fid, hbool_t verbose, FILE *verbose_file,
    unsigned long nrecords, unsigned long flush_count)
{
    hid_t tid;                              /* Datatype ID for records */
    hid_t mem_sid;                          /* Memory dataspace ID */
    hsize_t start[2] = {0, 0}, count[2] = {1, 1}; /* Hyperslab selection values */
    hsize_t dim[2] = {1, 0};                /* Dataspace dimensions */
    symbol_t record;                        /* The record to add to the dataset */
    unsigned long rec_to_flush;             /* # of records left to write before flush */
    unsigned long u, v;                     /* Local index variables */

    HDassert(fid >= 0);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    HDmemset(&record, 0, sizeof(record));

    /* Create a dataspace for the record to add */
    if((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        return -1;

    /* Create datatype for appending records */
    if((tid = create_symbol_datatype()) < 0)
        return -1;

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

        /* Cork the metadata cache, to prevent the object header from being
         * flushed before the data has been written */
        if(H5Odisable_mdc_flushes(symbol->dsid) < 0)
            return -1;

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

        /* Uncork the metadata cache */
        if(H5Oenable_mdc_flushes(symbol->dsid) < 0)
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
        HDfprintf(verbose_file, "Closing datasets\n");

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
    HDprintf("\n");
    HDprintf("Usage error!\n");
    HDprintf("\n");
    HDprintf("Usage: swmr_start_write [-f <# of records to write between flushing file contents>]\n");
    HDprintf("    [-i <index type>] [-c <deflate compression level>]\n");
    HDprintf("    [-r <random seed>] [-q] <# of records>\n");
    HDprintf("\n");
    HDprintf("<# of records to write between flushing file contents> should be 0\n");
    HDprintf("(for no flushing) or between 1 and (<# of records> - 1).\n");
    HDprintf("\n");
    HDprintf("<index type> should be b2 or ea\n");
    HDprintf("\n");
    HDprintf("<deflate compression level> should be -1 (for no compression) or 0-9\n");
    HDprintf("\n");
    HDprintf("<# of records> must be specified.\n");
    HDprintf("\n");
    HDprintf("Defaults to flushing every 10000 records ('-f 10000'),\n");
    HDprintf("v1 b-tree indexing (-i b1), compression ('-c -1'),\n");
    HDprintf("will generate a random seed (no -r given), and verbose (no '-q' given)\n");
    HDprintf("\n");
    HDexit(EXIT_FAILURE);
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
    hid_t fid;                      /* File ID for file opened */
    long nrecords = 0;              /* # of records to append */
    long flush_count = 10000;       /* # of records to write between flushing file */
    hbool_t verbose = TRUE;         /* Whether to emit some informational messages */
    FILE *verbose_file = NULL;      /* File handle for verbose output */
    hbool_t use_seed = FALSE;       /* Set to TRUE if a seed was set on the command line */
    unsigned random_seed = 0;       /* Random # seed */
    int comp_level = -1;            /* Compression level (-1 is no compression) */
    const char *index_type = "b1";  /* Chunk index type */
    unsigned u;                     /* Local index variable */
    int temp;                       /* Temporary variable */

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
                        comp_level = HDatoi(argv[u + 1]);
                        if(comp_level < -1 || comp_level > 9)
                            usage();
                        u += 2;
                        break;

                    /* Chunk index type */
                    case 'i':
                        index_type = argv[u + 1];
                        if(HDstrcmp(index_type, "ea")
                                && HDstrcmp(index_type, "b2"))
                            usage();
                        u += 2;
                        break;

                    /* # of records to write between flushing file */
                    case 'f':
                        flush_count = HDatol(argv[u + 1]);
                        if(flush_count < 0)
                            usage();
                        u += 2;
                        break;

                    /* Be quiet */
                    case 'q':
                        verbose = FALSE;
                        u++;
                        break;
                    
                    /* Random # seed */
                    case 'r':
                        use_seed = TRUE;
                        temp = HDatoi(argv[u + 1]);
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
                nrecords = HDatol(argv[u]);
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

    /* Set the random seed */
    if(!use_seed) {
        struct timeval t;

        HDgettimeofday(&t, NULL);
        random_seed = (unsigned)(t.tv_usec);
    } /* end if */
    HDsrandom(random_seed);

    /* Open output file */
    if(verbose) {
        char verbose_name[1024];

        HDsnprintf(verbose_name, sizeof(verbose_name), "swmr_writer.out.%u", random_seed);
        if(NULL == (verbose_file = HDfopen(verbose_name, "w"))) {
            HDfprintf(stderr, "Can't open verbose output file!\n");
            HDexit(EXIT_FAILURE);
        }
    } /* end if */

    /* Emit informational message */
    if(verbose) {
        HDfprintf(verbose_file, "Parameters:\n");
        HDfprintf(verbose_file, "\tindex type = %s\n", index_type);
        HDfprintf(verbose_file, "\tcompression level = %d\n", comp_level);
        HDfprintf(verbose_file, "\t# of records between flushes = %ld\n", flush_count);
        HDfprintf(verbose_file, "\t# of records to write = %ld\n", nrecords);
    } /* end if */

    /* ALWAYS emit the random seed for possible debugging */
    HDfprintf(stdout, "Using writer random seed: %u\n", random_seed);

    /* Create the test file */
    if((fid = create_file(FILENAME, verbose, verbose_file, random_seed)) < 0) {
        HDfprintf(stderr, "Error creating the file...\n");
        HDexit(EXIT_FAILURE);
    }

    /* Emit informational message */
    if(verbose)
        HDfprintf(verbose_file, "Generating symbol names\n");

    /* Generate dataset names */
    if(generate_symbols() < 0)
        return -1;

    /* Create the datasets in the file */
    if(create_datasets(fid, comp_level, verbose, verbose_file, index_type) < 0) {
        HDfprintf(stderr, "Error creating datasets...\n");
        HDexit(EXIT_FAILURE);
    }

    /* Enable SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0) {
        HDfprintf(stderr, "Error starting SWMR writing mode...\n");
        HDexit(EXIT_FAILURE);
    }

    /* Send a message to indicate "H5Fopen" is complete--releasing the file lock */
    h5_send_message(WRITER_MESSAGE, NULL, NULL);

    /* Emit informational message */
    if(verbose)
        HDfprintf(verbose_file, "Adding records\n");

    /* Append records to datasets */
    if(add_records(fid, verbose, verbose_file, (unsigned long)nrecords, (unsigned long)flush_count) < 0) {
        HDfprintf(stderr, "Error appending records to datasets!\n");
        HDexit(EXIT_FAILURE);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        HDfprintf(verbose_file, "Releasing symbols\n");

    /* Clean up the symbols */
    if(shutdown_symbols() < 0) {
        HDfprintf(stderr, "Error releasing symbols!\n");
        HDexit(EXIT_FAILURE);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        HDfprintf(verbose_file, "Closing the file\n");

    /* Close objects opened */
    if(H5Fclose(fid) < 0) {
        HDfprintf(stderr, "Error closing file!\n");
        HDexit(EXIT_FAILURE);
    } /* end if */

    return 0;
} /* main() */

