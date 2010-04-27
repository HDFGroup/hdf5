#include "swmr_common.h"
#include <unistd.h>

static hid_t symbol_tid = (-1);

static int
check_dataset(hid_t fid, unsigned verbose, const char *sym_name, symbol_t *record,
    hid_t rec_sid)
{
    hid_t dsid;                 /* Dataset ID */
    hid_t file_sid;             /* Dataset's space ID */
    hssize_t snpoints;          /* Number of elements in dataset */
    hsize_t start, count = 1;   /* Hyperslab selection values */

    /* Open dataset for symbol */
    if((dsid = H5Dopen2(fid, sym_name, H5P_DEFAULT)) < 0)
        return(-1);

    /* Get the dataset's dataspace */
    if((file_sid = H5Dget_space(dsid)) < 0)
        return(-1);

    /* Get the number of elements (= records, for 1-D datasets) */
    if((snpoints = H5Sget_simple_extent_npoints(file_sid)) < 0)
        return(-1);

    /* Emit informational message */
    if(verbose)
        printf("Symbol = '%s', # of records = %lld\n", sym_name, (long long)snpoints);

    /* Check if there are records for symbol */
    if(snpoints > 0) {
        /* Choose the last record in the dataset */
        start = (hsize_t)(snpoints - 1);
        if(H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, &start, NULL, &count, NULL) < 0)
            return(-1);

        /* Read record from dataset */
        record->rec_id = (uint64_t)ULLONG_MAX;
        if(H5Dread(dsid, symbol_tid, rec_sid, file_sid, H5P_DEFAULT, record) < 0)
            return(-1);

        /* Verify record value */
        if(record->rec_id != start) {
            printf("Incorrect record value!\n");
            printf("Symbol = '%s', # of records = %lld, record->rec_id = %llu\n", sym_name, (long long)snpoints, (unsigned long long)record->rec_id);
            return(-1);
        } /* end if */
    } /* end if */

    /* Close the dataset's dataspace */
    if(H5Sclose(file_sid) < 0)
        return(-1);

    /* Close dataset for symbol */
    if(H5Dclose(dsid) < 0)
        return(-1);

    return(0);
} /* end check_dataset() */

static int
read_records(const char *filename, unsigned verbose, unsigned long nseconds,
    unsigned poll_time, unsigned ncommon, unsigned nrandom)
{
    time_t start_time;          /* Starting time */
    time_t curr_time;           /* Current time */
    symbol_info_t **sym_com = NULL, **sym_rand = NULL;        /* Pointers to arrays of common & random dataset IDs */
    hid_t mem_sid;              /* Memory dataspace ID */
    symbol_t record;            /* The record to add to the dataset */
    unsigned v;                 /* Local index variable */

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    memset(&record, 0, sizeof(record));

    /* Emit informational message */
    if(verbose)
        printf("Choosing datasets\n");

    /* Allocate space for 'common' datasets, if any */
    if(ncommon > 0) {
        /* Allocate array to hold pointers to symbols for common datasets */
        if(NULL == (sym_com = malloc(sizeof(symbol_info_t *) * ncommon)))
            return(-1);

        /* Open the common datasets */
        for(v = 0; v < ncommon; v++) {
            unsigned offset;                /* Offset of symbol to use */

            /* Determine the offset of the symbol, within level 0 symbols */
            /* (level 0 symbols are the most common symbols) */
            offset = random() % symbol_count[0];
            sym_com[v] = &symbol_info[0][offset];

            /* Emit informational message */
            if(verbose)
                printf("Common symbol #%u = '%s'\n", v, symbol_info[0][offset].name);
        } /* end for */
    } /* end if */

    /* Allocate space for 'random' datasets, if any */
    if(nrandom > 0) {
        /* Allocate array to hold pointers to symbols for random datasets */
        if(NULL == (sym_rand = malloc(sizeof(symbol_info_t *) * nrandom)))
            return(-1);

        /* Determine the random datasets */
        for(v = 0; v < nrandom; v++) {
            symbol_info_t *sym;         /* Symbol to use */

            /* Determine the symbol, within all symbols */
            if(NULL == (sym = choose_dataset()))
                return(-1);
            sym_rand[v] = sym;

            /* Emit informational message */
            if(verbose)
                printf("Random symbol #%u = '%s'\n", v, sym->name);
        } /* end for */
    } /* end if */

    /* Create a dataspace for the record to read */
    if((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        return(-1);

    /* Emit informational message */
    if(verbose)
        printf("Reading records\n");

    /* Get the starting time */
    start_time = time(NULL);
    curr_time = start_time;

    /* Loop over reading records until [at least] the correct # of seconds have passed */
    while(curr_time < (time_t)(start_time + nseconds)) {
        hid_t fid;              /* File ID */

        /* Emit informational message */
        if(verbose)
            printf("Opening file: %s\n", filename);

        /* Open the file */
        if((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, H5P_DEFAULT)) < 0)
            return(-1);

        /* Check 'common' datasets, if any */
        if(ncommon > 0) {
            /* Emit informational message */
            if(verbose)
                printf("Checking common symbols\n");

            /* Iterate over common datasets */
            for(v = 0; v < ncommon; v++) {
                /* Check common dataset */
                if(check_dataset(fid, verbose, sym_com[v]->name, &record, mem_sid) < 0)
                    return(-1);
            } /* end for */
        } /* end if */

        /* Check 'random' datasets, if any */
        if(nrandom > 0) {
            /* Emit informational message */
            if(verbose)
                printf("Checking random symbols\n");

            /* Iterate over random datasets */
            for(v = 0; v < nrandom; v++) {
                /* Check random dataset */
                if(check_dataset(fid, verbose, sym_rand[v]->name, &record, mem_sid) < 0)
                    return(-1);
            } /* end for */
        } /* end if */

        /* Emit informational message */
        if(verbose)
            printf("Closing file\n");

        /* Close the file */
        if(H5Fclose(fid) < 0)
            return(-1);

        /* Sleep for the appropriate # of seconds */
        sleep(poll_time);

        /* Retrieve the current time */
        curr_time = time(NULL);
    } /* end while */

    /* Close the memory dataspace */
    if(H5Sclose(mem_sid) < 0)
        return(-1);

    /* Emit informational message */
    if(verbose)
        printf("Closing datasets\n");

    /* Close 'random' datasets, if any */
    if(nrandom > 0) {
        /* Release array holding dataset ID's for random datasets */
        free(sym_rand);
    } /* end if */

    /* Close 'common' datasets, if any */
    if(ncommon > 0) {
        /* Release array holding dataset ID's for common datasets */
        free(sym_com);
    } /* end if */

    return(0);
} /* end read_records() */

static void
usage(void)
{
    printf("Usage error!\n");
    printf("Usage: swmr_reader [-q] [-s <# of seconds to sleep between polling>] [-h <# of common symbols to poll>] [-l <# of random symbols to poll>] [-r <random # seed>] <# of seconds to test>\n");
    printf("Defaults to verbose (no '-q' given), 1 second between polling ('-s 1'), 5 common symbols to poll ('-h 5') and 10 random symbols to poll ('-l 10')\n");
    exit(1);
} 

int main(int argc, const char *argv[])
{
    long nseconds = 0;  /* # of seconds to test */
    int poll_time = 1;  /* # of seconds between polling */
    int ncommon = 5;    /* # of common symbols to poll */
    int nrandom = 10;   /* # of random symbols to poll */
    unsigned verbose = 1;       /* Whether to emit some informational messages */
    int random_seed = 0; /* Random # seed */
    unsigned u;         /* Local index variables */

    /* Parse command line options */
    if(argc < 2)
        usage();
    if(argc > 1) {
        u = 1;
        while(u < (unsigned)argc) {
            if(argv[u][0] == '-') {
                switch(argv[u][1]) {
                    /* # of common symbols to poll */
                    case 'h':
                        ncommon = atoi(argv[u + 1]);
                        if(ncommon < 0)
                            usage();
                        u += 2;
                        break;

                    /* # of random symbols to poll */
                    case 'l':
                        nrandom = atoi(argv[u + 1]);
                        if(nrandom < 0)
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
                        random_seed = atoi(argv[u + 1]);
                        if(random_seed < 0)
                            usage();
                        u += 2;
                        break;

                    /* # of seconds between polling */
                    case 's':
                        poll_time = atoi(argv[u + 1]);
                        if(poll_time < 0)
                            usage();
                        u += 2;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            } /* end if */
            else {
                /* Get the number of records to append */
                nseconds = atol(argv[u]);
                if(nseconds <= 0)
                    usage();

                u++;
            } /* end else */
        } /* end while */
    } /* end if */
    if(nseconds <= 0)
        usage();
    if(poll_time >= nseconds)
        usage();

    /* Emit informational message */
    if(verbose) {
        printf("Parameters:\n");
        printf("\t# of seconds between polling = %d\n", poll_time);
        printf("\t# of common symbols to poll = %d\n", ncommon);
        printf("\t# of random symbols to poll = %d\n", nrandom);
        printf("\t# of seconds to test = %ld\n", nseconds);
    } /* end if */

    /* Create randomized set of numbers */
    random_seed += (int)time(NULL);
    srandom((unsigned)random_seed);

    /* Emit informational message */
    if(verbose)
        printf("Generating symbol names\n");

    /* Generate dataset names */
    if(generate_symbols() < 0) {
        printf("Error generating symbol names!\n");
        exit(1);
    } /* end if */

    /* Create datatype for creating datasets */
    if((symbol_tid = create_symbol_datatype()) < 0)
        return(-1);

    /* Reading records from datasets */
    if(read_records(FILENAME, verbose, (unsigned long)nseconds, (unsigned)poll_time, (unsigned)ncommon, (unsigned)nrandom) < 0) {
        printf("Error reading records from datasets!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        printf("Releasing symbols\n");

    /* Clean up the symbols */
    if(shutdown_symbols() < 0) {
        printf("Error releasing symbols!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        printf("Closing objects\n");

    /* Close objects created */
    if(H5Tclose(symbol_tid) < 0) {
        printf("Error closing symbol datatype!\n");
        exit(1);
    } /* end if */

    return(0);
}

