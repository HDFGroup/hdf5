/*
 * Copyright © 1998 Spizella Software
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <robb@arborea.spizella.com>
 *              Tuesday, August 25, 1998
 */
#include <assert.h>
#include <hdf5.h>
#include <signal.h>
#include <stdlib.h>

#include <H5private.h>	/*for performance monitoring*/


#define NOTIFY_INTERVAL	2 /*seconds*/
#define TIME_LIMIT	60 /*seconds*/
#define CH_SIZE		8192*8 /*approx chunk size in bytes*/
#define MAX_NELMTS	3000000

#define C_MTYPE		unsigned int	/*type in memory		*/
#define H_MTYPE		H5T_NATIVE_UINT	/*type in memory		*/
#define H_FTYPE		H5T_NATIVE_UINT	/*type in file			*/

typedef struct {
    double		percent;
    size_t		lo, hi;
    size_t		nhits;
} quant_t;

#if 1
/* Typical VBT sizes */
static quant_t quant_g[] = {
    {10.00, 	   1, 	    5},
    {89.00, 	   6, 	   20},
    { 0.90, 	  21, 	  100},
    { 0.09, 	 101, 	 1000},
    { 0.01,	1001, 	10000},
};
#elif 0
/* Sizes for testing */
static quant_t	quant_g[] = {
    {10.0, 	   1,	    5},
    {80.0, 	   6,	   15},
    {10.0,	  16,	   20},
};
#elif 0
/* Larger I/O */
static quant_t	quant_g[] = {
    {10.0,         1, 	 1000},
    {80.0, 	1001,	 5000},
    {10.0, 	5001,	10000},
};
#else
/* All same size */
static quant_t	quant_g[] = {
    {100.0, 	1000, 1000}
};
#endif

static volatile sig_atomic_t alarm_g = 0;
static volatile sig_atomic_t timeout_g = 0;


/*-------------------------------------------------------------------------
 * Function:	catch_alarm
 *
 * Purpose:	Increments the global `alarm_g' and resets the alarm for
 *		another few seconds.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
catch_alarm(int __unused__ signum)
{
    static int	ncalls=0;

    ncalls++;
    if (0==ncalls % NOTIFY_INTERVAL) {
	alarm_g++;
    }
    if (timeout_g>0) --timeout_g;
    alarm(1);
}


/*-------------------------------------------------------------------------
 * Function:	display_error_cb
 *
 * Purpose:	Displays the error stack after printing "*FAILED*".
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
display_error_cb (void __unused__ *client_data)
{
    putchar('\n');
    H5Eprint (stdout);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	rand_nelmts
 *
 * Purpose:	Returns a the length of a 1-d array according to the
 *		probabilities described above.
 *
 * Return:	Success:	Number of elements
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 20, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
rand_nelmts(int reset_counters)
{
    double		p = (rand() % 1000000)/1000000.0;
    double		total = 0.0;
    size_t		size=0, i;
    static size_t	ncalls=0;

    if (reset_counters) {
	printf("    %9s      %8s %8s\n", "Length", "Requsted", "Actual");
	printf("   --------------- -------- --------\n");
	for (i=0; i<NELMTS(quant_g); i++) {
	    printf("   [%6lu,%6lu] %7.3f%% %7.3f%%\n",
		   (unsigned long)(quant_g[i].lo),
		   (unsigned long)(quant_g[i].hi),
		   quant_g[i].percent,
		   100.0*(double)(quant_g[i].nhits)/(double)ncalls);
	    quant_g[i].nhits = 0;
	}
	printf("   --------------- -------- --------\n");
	ncalls = 0;
	size = 0;
    } else {
	for (i=0; i<NELMTS(quant_g); i++) {
	    total += quant_g[i].percent/100.0;
	    if (p<total) {
		size = rand()%(1+(quant_g[i].hi-quant_g[i].lo)) +
		       quant_g[i].lo;
		quant_g[i].nhits++;
		break;
	    }
	}
	assert(i<NELMTS(quant_g));
	ncalls++;
    }
    
    return size;
}


/*-------------------------------------------------------------------------
 * Function:	ragged_write_all
 *
 * Purpose:	Writes rows to the ragged array RA.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
ragged_write_all(hid_t ra, hsize_t rows_at_once)
{
    int			*dd, total_nelmts=0;
    hssize_t		row;			/*current row number	*/
    hsize_t		i;			/*counter		*/
    hsize_t		max_width = quant_g[NELMTS(quant_g)-1].hi;
    hsize_t		interval_nelmts;	/*elmts/interval timer	*/
    hsize_t		*size=NULL;		/*size of each row	*/
    void		**buf=NULL;		/*buffer for each row	*/
    H5_timer_t		timer, timer_total;	/*performance timers	*/
    char		s[64];			/*tempory string buffer	*/
    char		testname[80];

    sprintf(testname, "Testing write all, units of %lu",
	    (unsigned long)rows_at_once);
    printf("%s...\n", testname);
    fflush(stdout);
    timeout_g = TIME_LIMIT;

    /* Create the ragged array row in memory */
    if (NULL==(dd = malloc(max_width*sizeof(C_MTYPE))) ||
	NULL==(size = malloc(rows_at_once*sizeof(*size))) ||
	NULL==(buf = malloc(rows_at_once*sizeof(*buf)))) {
	puts("Memory allocation failed");
	goto error;
    }
    for (i=0; i<max_width; i++) dd[i] = i+1;

    /*
     * Describe a few rows then add them to the ragged array.  Print a status
     * report every once in a while too.
     */
    printf("   %8s %8s %8s %10s\n",
	   "Row", "Nelmts", "Complete", "Bandwidth");
    printf("   -------- -------- -------- ----------\n");
    H5_timer_reset(&timer_total);
    H5_timer_begin(&timer);
    interval_nelmts = 0;
    for (row=0; total_nelmts<MAX_NELMTS && timeout_g>0; row+=i) {
	for (i=0; i<rows_at_once && total_nelmts<MAX_NELMTS; i++) {
	    size[i] = rand_nelmts(0);
	    total_nelmts += size[i];
	    buf[i] = dd;
	    interval_nelmts += size[i];
	}
	if (H5RAwrite(ra, row, i, H_MTYPE, size, buf)<0) goto error;
	if (0==row || alarm_g || 0==timeout_g) {
	    alarm_g = 0;
	    H5_timer_end(&timer_total, &timer);
	    H5_bandwidth(s, (double)interval_nelmts*sizeof(C_MTYPE),
			 timer.etime);
	    printf("   %8lu %8lu %7.3f%% %10s%s\n",
		   (unsigned long)(row+i), (unsigned long)total_nelmts,
		   100.0*total_nelmts/MAX_NELMTS, s,
		   0==timeout_g?" (aborting)":"");
	    interval_nelmts = 0;
	    H5_timer_begin(&timer);
	}
    }

    /* Conclusions */
    if (timeout_g) { /*a minor race condition, but who really cares?*/
	H5_timer_end(&timer_total, &timer);
	H5_bandwidth(s, (double)interval_nelmts*sizeof(C_MTYPE), timer.etime);
	printf("   %8lu %8lu %7.3f%% %10s\n",
	       (unsigned long)row, (unsigned long)total_nelmts,
	       100.0*total_nelmts/MAX_NELMTS, s);
    }
    printf("   -------- -------- -------- ----------\n");
    H5_bandwidth(s, (double)total_nelmts*sizeof(C_MTYPE), timer_total.etime);
    printf("   %27s%10s\n", "", s);

    /* Cleanup */
    free(dd);
    free(size);
    free(buf);
    printf("%-70s PASSED\n\n", testname);
    return 0;

 error:
    printf("%-70s*FAILED*\n\n", testname);
    return -1;
}
    

/*-------------------------------------------------------------------------
 * Function:	ragged_read_all
 *
 * Purpose:	Reads all rows of a ragged array in row order a few rows at a
 *		time.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
ragged_read_all(hid_t ra, hsize_t rows_at_once)
{
    int			total_nelmts=0;
    hsize_t		i, j;			/*counters		*/
    hssize_t		row;			/*current row number	*/
    hsize_t		interval_nelmts;	/*elmts/interval timer	*/
    hsize_t		*size=NULL;		/*size of each row	*/
    C_MTYPE		**buf=NULL;		/*buffer for each row	*/
    H5_timer_t		timer, timer_total;	/*performance timers	*/
    char		s[64];			/*tempory string buffer	*/
    char		testname[80];

    sprintf(testname, "Testing read all, units of %lu",
	    (unsigned long)rows_at_once);
    printf("%s...\n", testname);
    fflush(stdout);
    timeout_g = TIME_LIMIT;

    /* Create the ragged array row in memory */
    if (NULL==(size = malloc(rows_at_once*sizeof(*size))) ||
	NULL==(buf = malloc(rows_at_once*sizeof(*buf)))) {
	puts("Memory allocation failed");
	goto error;
    }

    /*
     * Read a few rows at a time from the ragged array.  Print a status report
     * every once in a while too.
     */
    printf("   %8s %8s %8s %10s\n",
	   "Row", "Nelmts", "Complete", "Bandwidth");
    printf("   -------- -------- -------- ----------\n");
    H5_timer_reset(&timer_total);
    H5_timer_begin(&timer);
    interval_nelmts = 0;
    for (row=0; total_nelmts<MAX_NELMTS && timeout_g>0; row+=i) {

	/* Clear data then read */
	HDmemset(size, 0, rows_at_once*sizeof(*size));
	HDmemset(buf, 0, rows_at_once*sizeof(*buf));
	if (H5RAread(ra, row, rows_at_once, H_MTYPE, size,
		    (void**)buf)<0) {
	    goto error;
	}

	/* Check values read */
	for (i=0; i<rows_at_once && size[i]; i++) {
	    interval_nelmts += size[i];
	    total_nelmts += size[i];
	    for (j=0; j<size[i]; j++) {
		if (buf[i][j]!=j+1) {
		    printf("Wrong value(s) read for row %ld.\n",
			   (long)(row+i));
		    for (j=0; j<size[i]; j++) {
			printf("%s%d", j?",":"", buf[i][j]);
		    }
		    putchar('\n');
		    goto error;
		}
	    }
	    free(buf[i]);
	    buf[i] = NULL;
	}

	/* Print statistics? */
	if (0==row || alarm_g || 0==timeout_g) {
	    alarm_g = 0;
	    H5_timer_end(&timer_total, &timer);
	    H5_bandwidth(s, (double)interval_nelmts*sizeof(C_MTYPE),
			 timer.etime);
	    printf("   %8lu %8lu %7.3f%% %10s%s\n",
		   (unsigned long)(row+i), (unsigned long)total_nelmts,
		   100.0*total_nelmts/MAX_NELMTS, s,
		   0==timeout_g?" (aborting)":"");
	    interval_nelmts = 0;
	    H5_timer_begin(&timer);
	}
	if (0==size[rows_at_once-1]) {
	    /* Reached the end of the array */
	    if (total_nelmts<MAX_NELMTS) {
		puts("   * Short read, previous write probably aborted");
	    }
	    row += i;
	    break;
	}
    }

    /* Conclusions */
    if (timeout_g) { /*a minor race condition, but who really cares?*/
	H5_timer_end(&timer_total, &timer);
	H5_bandwidth(s, (double)interval_nelmts*sizeof(C_MTYPE), timer.etime);
	printf("   %8lu %8lu %7.3f%% %10s\n",
	       (unsigned long)row, (unsigned long)total_nelmts,
	       100.0*total_nelmts/MAX_NELMTS, s);
    }
    printf("   -------- -------- -------- ----------\n");
    H5_bandwidth(s, (double)total_nelmts*sizeof(C_MTYPE), timer_total.etime);
    printf("   %27s%10s\n", "", s);

    /* Cleanup */
    free(size);
    free(buf);
    printf("%-70s PASSED\n\n", testname);
    return 0;

 error:
    printf("%-70s*FAILED*\n\n", testname);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	ragged_read_short
 *
 * Purpose:	Reads all the data but only the part that is in the `raw'
 *		dataset.  We should see a nice speed increase because we
 *		don't have to perform the little reads into the overflow
 *		array.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, August 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
ragged_read_short(hid_t ra, hsize_t rows_at_once, hsize_t width)
{
    int			total_nelmts=0;
    hsize_t		i, j;
    hssize_t		row;			/*current row number	*/
    hsize_t		interval_nelmts;	/*elmts/interval timer	*/
    hsize_t		read_nelmts=0;		/*total elements read	*/
    hsize_t		*size=NULL;		/*size of each row	*/
    C_MTYPE		**buf=NULL;		/*buffer for each row	*/
    H5_timer_t		timer, timer_total;	/*performance timers	*/
    char		s[64];			/*tempory string buffer	*/
    char		testname[80];

    sprintf(testname, "Testing read short, units of %lu",
	    (unsigned long)rows_at_once);
    printf("%s...\n", testname);
    fflush(stdout);
    timeout_g = TIME_LIMIT;

    /* Create the ragged array row in memory */
    if (NULL==(size = malloc(rows_at_once*sizeof(*size))) ||
	NULL==(buf = malloc(rows_at_once*sizeof(*buf)))) {
	puts("Memory allocation failed");
	goto error;
    }
    for (i=0; i<rows_at_once; i++) {
	if (NULL==(buf[i] = malloc(width*sizeof(C_MTYPE)))) {
	    puts("Memory allocation failed");
	    goto error;
	}
    }

    /*
     * Read a few rows at a time from the ragged array.  Print a status report
     * every once in a while too.
     */
    printf("   %8s %8s %8s %10s\n",
	   "Row", "Nelmts", "Complete", "Bandwidth");
    printf("   -------- -------- -------- ----------\n");
    H5_timer_reset(&timer_total);
    H5_timer_begin(&timer);
    interval_nelmts = 0;
    for (row=0; total_nelmts<MAX_NELMTS && timeout_g>0; row+=i) {

	/* Read data */
	for (i=0; i<rows_at_once; i++) size[i] = width;
	if (H5RAread(ra, row, rows_at_once, H_MTYPE, size,
		    (void**)buf)<0) {
	    goto error;
	}

	/* Check values read */
	for (i=0; i<rows_at_once && size[i]; i++) {

	    /*
	     * Number of useful elements actually read in this timing
	     * interval.  This is used to calculate bandwidth.
	     */
	    interval_nelmts += MIN(width, size[i]);

	    /*
	     * Total number of elements actually read for rows so far.
	     */
	    read_nelmts += MIN(width, size[i]);

	    /*
	     * Total number of elements attributed to the rows read so far.
	     * This is used to calculate the percent done.
	     */
	    total_nelmts += size[i];

	    /* Check the values */
	    for (j=0; j<MIN(width, size[i]); j++) {
		if (buf[i][j]!=j+1) {
		    printf("Wrong value(s) read for row %ld.\n",
			   (long)(row+i));
		    for (j=0; j<MIN(width, size[i]); j++) {
			printf("%s%d", j?",":"", buf[i][j]);
		    }
		    putchar('\n');
		    goto error;
		}
	    }
	}

	/* Print statistics? */
	if (0==row || alarm_g || 0==timeout_g) {
	    alarm_g = 0;
	    H5_timer_end(&timer_total, &timer);
	    H5_bandwidth(s, (double)interval_nelmts*sizeof(C_MTYPE),
			 timer.etime);
	    printf("   %8lu %8lu %7.3f%% %10s%s\n",
		   (unsigned long)(row+i), (unsigned long)read_nelmts,
		   100.0*total_nelmts/MAX_NELMTS, s,
		   0==timeout_g?" (aborting)":"");
	    interval_nelmts = 0;
	    H5_timer_begin(&timer);
	}
	if (0==size[rows_at_once-1]) {
	    /* Reached the end of the array */
	    if (total_nelmts<MAX_NELMTS) {
		puts("   * Short read, previous write probably aborted");
	    }
	    row += i;
	    break;
	}
    }

    /* Conclusions */
    if (timeout_g) { /*a minor race condition, but who really cares?*/
	H5_timer_end(&timer_total, &timer);
	H5_bandwidth(s, (double)interval_nelmts*sizeof(C_MTYPE), timer.etime);
	printf("   %8lu %8lu %7.3f%% %10s\n",
	       (unsigned long)row, (unsigned long)read_nelmts,
	       100.0*total_nelmts/MAX_NELMTS, s);
    }
    printf("   -------- -------- -------- ----------\n");
    H5_bandwidth(s, (double)read_nelmts*sizeof(C_MTYPE), timer_total.etime);
    printf("   %27s%10s\n", "", s);

    /* Cleanup */
    for (i=0; i<rows_at_once; i++) free(buf[i]);
    free(size);
    free(buf);
    printf("%-70s PASSED\n\n", testname);
    return 0;

 error:
    printf("%-70s*FAILED*\n\n", testname);
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Friday, August 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t		file, dcpl, ra;
    hsize_t		ch_size[2];		/*chunk size		*/
    hsize_t		rows_at_once=100;	/*row aggregation	*/
    int			argno=1;

    /* Parse command line options */
    if (argno<argc) {
	rows_at_once = strtol(argv[argno++], NULL, 0);
    }
    
    /* Display HDF5 API errors in a special way */
    H5Eset_auto(display_error_cb, NULL);

    /* Get a SIGALRM every few seconds */
#ifdef HAVE_SIGACTION
    {
	struct sigaction act;
	act.sa_handler = catch_alarm;
	sigemptyset(&(act.sa_mask));
	act.sa_flags = 0;
	sigaction(SIGALRM, &act, NULL);
	alarm(1);
    }
#else
    puts("No sigaction().  This test may run for a *long* time.");
#endif

    /* Create the file and ragged array */
    if ((file=H5Fcreate("ragged.h5", H5F_ACC_TRUNC, H5P_DEFAULT,
			H5P_DEFAULT))<0) goto error;
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    ch_size[1] = 20;
    ch_size[0] = MAX(1, CH_SIZE/(ch_size[1]*sizeof(C_MTYPE))); /*length*/
    printf("Chunk size is %lu by %lu\n",
	   (unsigned long)(ch_size[0]), (unsigned long)(ch_size[1]));
    if (H5Pset_chunk(dcpl, 2, ch_size)<0) goto error;
    if ((ra=H5RAcreate(file, "ra", H_FTYPE, dcpl))<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;

    /* The tests */
    if (ragged_write_all(ra, rows_at_once)<0) goto error;
    if (ragged_read_all(ra, rows_at_once)<0) goto error;
    if (ragged_read_short(ra, rows_at_once, ch_size[1])<0) goto error;
    
    /* The tests again */
    if (ragged_write_all(ra, rows_at_once)<0) goto error;
    if (ragged_read_all(ra, rows_at_once)<0) goto error;
    if (ragged_read_short(ra, rows_at_once, ch_size[1])<0) goto error;
    
    /* Conclusions */
    printf("\n\nDistribution of row lengths:\n");
    rand_nelmts(1);
    
    /* Cleanup */
    if (H5RAclose(ra)<0) goto error;
    if (H5Fclose(file)<0) goto error;

    puts("All ragged array tests passed.");
    return 0;

 error:
    puts("*** RAGGED ARRAY TEST(S) FAILED ***");
    return -1;
}
