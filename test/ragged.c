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
static volatile sig_atomic_t abort_g = 0;


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
catch_alarm(int signum)
{
    static int	ncalls=0;

    ncalls++;
    if (0==ncalls % NOTIFY_INTERVAL) {
	alarm_g++;
    }
    if (ncalls>=TIME_LIMIT) {
	abort_g=1;
    }
    alarm(1);
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
rand_nelmts(void)
{
    double	p = (rand() % 1000000)/1000000.0;
    double	total = 0.0;
    size_t	size, i;

    for (i=0; i<NELMTS(quant_g); i++) {
	total += quant_g[i].percent/100.0;
	if (p<total) {
	    size = rand()%(1+(quant_g[i].hi-quant_g[i].lo)) + quant_g[i].lo;
	    quant_g[i].nhits++;
	    break;
	}
    }
    assert(i<NELMTS(quant_g));
    return size;
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
main(void)
{
    hid_t		file, dcpl, ra;
    int			*dd, max_nelmts=3000000, total_nelmts=0;
    int			i, rows_at_once=100;
    hssize_t		row;			/*current row number	*/
    hsize_t		max_width = quant_g[NELMTS(quant_g)-1].hi;
    hsize_t		ch_size[2];		/*chunk size		*/
    hsize_t		interval_nelmts;	/*elmts/interval timer	*/
    hsize_t		*size=NULL;		/*size of each row	*/
    void		**buf=NULL;		/*buffer for each row	*/
    struct sigaction	act;			/*alarm signal handler	*/
    H5_timer_t		timer, timer_total;	/*performance timers	*/
    char		s[64];			/*tempory string buffer	*/

    /* Get a SIGALRM every few seconds */
    act.sa_handler = catch_alarm;
    sigemptyset(&(act.sa_mask));
    act.sa_flags = 0;
    sigaction(SIGALRM, &act, NULL);
    alarm(1);

    /* Create the file and ragged array */
    if ((file=H5Fcreate("ragged.h5", H5F_ACC_TRUNC, H5P_DEFAULT,
			H5P_DEFAULT))<0) goto error;
    if ((dcpl=H5Pcreate(H5P_DATASET_CREATE))<0) goto error;
    ch_size[1] = 20;
    ch_size[0] = MAX(1, CH_SIZE/(ch_size[1]*sizeof(int))); /*length*/
    printf("Chunk size is %lu by %lu\n",
	   (unsigned long)(ch_size[0]), (unsigned long)(ch_size[1]));
    if (H5Pset_chunk(dcpl, 2, ch_size)<0) goto error;
    if ((ra=H5Rcreate(file, "ra", H5T_NATIVE_INT, dcpl))<0) goto error;
    if (H5Pclose(dcpl)<0) goto error;

    /* Create the ragged array row in memory */
    if (NULL==(dd = malloc(max_width*sizeof(int)))) goto error;
    for (i=0; i<max_width; i++) dd[i] = i+1;
    size = malloc(rows_at_once*sizeof(*size));
    buf = malloc(rows_at_once*sizeof(*buf));

    /*
     * Describe a few rows then add them to the ragged array.  Print a status
     * report every once in a while too.
     */
    printf("Aggregated to %d row%s\n", rows_at_once, 1==rows_at_once?"":"s");
    printf("   %8s %8s %8s %10s\n",
	   "Row", "Nelmts", "Complete", "Bandwidth");
    printf("   -------- -------- -------- ----------\n");
    H5_timer_reset(&timer_total);
    H5_timer_begin(&timer);
    interval_nelmts = 0;
    for (row=0; total_nelmts<max_nelmts && !abort_g; row+=i) {
	for (i=0; i<rows_at_once && total_nelmts<max_nelmts; i++) {
	    size[i] = rand_nelmts();
	    total_nelmts += size[i];
	    buf[i] = dd;
	    interval_nelmts += size[i];
	}
	if (H5Rwrite(ra, row, i, H5T_NATIVE_INT, size, buf)<0) goto error;
	if (0==row || alarm_g) {
	    alarm_g = 0;
	    H5_timer_end(&timer_total, &timer);
	    H5_bandwidth(s, (double)interval_nelmts*sizeof(int), timer.etime);
	    printf("   %8lu %8lu %7.3f%% %10s%s\n",
		   (unsigned long)(row+i), (unsigned long)total_nelmts,
		   100.0*total_nelmts/max_nelmts, s, abort_g?" (aborting)":"");
	    interval_nelmts = 0;
	    H5_timer_begin(&timer);
	}
    }

    /* Conclusions */
    if (!abort_g) {
	H5_timer_end(&timer_total, &timer);
	H5_bandwidth(s, (double)interval_nelmts*sizeof(int), timer.etime);
	printf("   %8lu %8lu %7.3f%% %10s\n",
	       (unsigned long)row, (unsigned long)total_nelmts,
	       100.0*total_nelmts/max_nelmts, s);
    }
    printf("   -------- -------- -------- ----------\n");
    H5_bandwidth(s, (double)total_nelmts*sizeof(int), timer_total.etime);
    printf("   %27s%10s\n\n", "", s);


    printf("    %9s      %8s %8s\n", "Length", "Requsted", "Actual");
    printf("   --------------- -------- --------\n");
    for (i=0; i<NELMTS(quant_g); i++) {
	printf("   [%6lu,%6lu] %7.3f%% %7.3f%%\n",
	       (unsigned long)(quant_g[i].lo), (unsigned long)(quant_g[i].hi),
	       quant_g[i].percent,
	       100.0*(double)(quant_g[i].nhits)/(double)row);
    }
    printf("   --------------- -------- --------\n");

    /* Cleanup */
    if (H5Rclose(ra)<0) goto error;
    if (H5Fclose(file)<0) goto error;
    free(dd);
    free(size);
    free(buf);
    return 0;

 error:
    return -1;
}
