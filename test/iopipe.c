/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, March 12, 1998
 */
#undef NDEBUG
#include <assert.h>
#include <fcntl.h>
#include <hdf5.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#define RAW_FILE_NAME	"iopipe.raw"
#define HDF5_FILE_NAME	"iopipe.h5"
#define REQUEST_SIZE_X	4579
#define REQUEST_SIZE_Y	4579
#define NREAD_REQUESTS	45
#define NWRITE_REQUESTS	45
#define HEADING		"%-16s"
#define PROGRESS	'='


/*-------------------------------------------------------------------------
 * Function:	print_stats
 *
 * Purpose:	Prints statistics
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, March 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_stats (const char *prefix,
	     struct rusage *r_start, struct rusage *r_stop,
	     struct timeval *t_start, struct timeval *t_stop,
	     size_t nbytes)
{
    double	u_time, s_time, e_time, bw;

    u_time = (r_stop->ru_utime.tv_sec+r_stop->ru_utime.tv_usec/1000000.0) -
	     (r_start->ru_utime.tv_sec+r_start->ru_utime.tv_usec/1000000.0);

    s_time = (r_stop->ru_stime.tv_sec+r_stop->ru_stime.tv_usec/1000000.0) -
	     (r_start->ru_stime.tv_sec+r_start->ru_stime.tv_usec/1000000.0);

    e_time = (t_stop->tv_sec+t_stop->tv_usec/1000000.0) -
	     (t_start->tv_sec+t_start->tv_usec/1000000.0);

    bw = nbytes / e_time;
    
    printf (HEADING "%1.2fuser %1.2fsystem %1.2felapsed %1.2fMB/s\n", 
	    prefix, u_time, s_time, e_time, bw/(1024*1024));

}


/*-------------------------------------------------------------------------
 * Function:	synchronize
 *
 * Purpose:	
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, March 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
synchronize (void)
{
    system ("sync");
    system ("df >/dev/null");
#if 0
    system ("/sbin/swapout 130");
#endif
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
 *              Thursday, March 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    static size_t	size[2] = {REQUEST_SIZE_X, REQUEST_SIZE_Y};
    static int		nread=NREAD_REQUESTS, nwrite=NWRITE_REQUESTS;

    unsigned char	*the_data = NULL;
    hid_t		file, dset, file_space=-1;
    herr_t		status;
    struct rusage	r_start, r_stop;
    struct timeval	t_start, t_stop;
    int			i, fd;
    ssize_t		n;
    off_t		offset;
    int			start[2];
    size_t		count[2];
    
    printf ("I/O request size is %1.1fMB\n", (size[0]*size[1])/(1024.0*1024));

    
    /* Open the files */
    file = H5Fcreate (HDF5_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    assert (file>=0);
    fd = open (RAW_FILE_NAME, O_RDWR|O_CREAT|O_TRUNC, 0666);
    assert (fd>=0);
    
    /* Create the dataset */
    file_space = H5Screate_simple (2, size, size);
    assert (file_space>=0);
    dset = H5Dcreate (file, "dset", H5T_NATIVE_UCHAR, file_space, H5P_DEFAULT);
    assert (dset>=0);
    the_data = malloc (size[0]*size[1]);
    memset (the_data, 0xAA, size[0]*size[1]); /*initial fill for lazy malloc*/

    /* Fill raw */
    synchronize ();
    getrusage (RUSAGE_SELF, &r_start);
    gettimeofday (&t_start, NULL);
    fprintf (stderr, HEADING, "fill raw");
    for (i=0; i<nwrite; i++) {
	putc (PROGRESS, stderr);
	fflush (stderr);
	memset (the_data, 0xAA, size[0]*size[1]);
    }
    getrusage (RUSAGE_SELF, &r_stop);
    gettimeofday (&t_stop, NULL);
    putc ('\n', stderr);
    print_stats ("fill raw",
		 &r_start, &r_stop, &t_start, &t_stop,
		 nread*size[0]*size[1]);
    

    /* Fill hdf5 */
    synchronize ();
    getrusage (RUSAGE_SELF, &r_start);
    gettimeofday (&t_start, NULL);
    fprintf (stderr, HEADING, "fill hdf5");
    for (i=0; i<nread; i++) {
	putc (PROGRESS, stderr);
	fflush (stderr);
	status = H5Dread (dset, H5T_NATIVE_UCHAR, file_space, file_space,
			  H5P_DEFAULT, the_data);
	assert (status>=0);
    }
    getrusage (RUSAGE_SELF, &r_stop);
    gettimeofday (&t_stop, NULL);
    putc ('\n', stderr);
    print_stats ("fill hdf5",
		 &r_start, &r_stop, &t_start, &t_stop,
		 nread*size[0]*size[1]);
    
    /* Write the raw dataset */
    synchronize ();
    getrusage (RUSAGE_SELF, &r_start);
    gettimeofday (&t_start, NULL);
    fprintf (stderr, HEADING, "out raw");
    for (i=0; i<nwrite; i++) {
	putc (PROGRESS, stderr);
	fflush (stderr);
	offset = lseek (fd, 0, SEEK_SET);
	assert (0==offset);
	n = write (fd, the_data, size[0]*size[1]);
	assert (n==size[0]*size[1]);
    }
    getrusage (RUSAGE_SELF, &r_stop);
    gettimeofday (&t_stop, NULL);
    putc ('\n', stderr);
    print_stats ("out raw",
		 &r_start, &r_stop, &t_start, &t_stop,
		 nread*size[0]*size[1]);

    /* Write the hdf5 dataset */
    synchronize ();
    getrusage (RUSAGE_SELF, &r_start);
    gettimeofday (&t_start, NULL);
    fprintf (stderr, HEADING, "out hdf5");
    for (i=0; i<nwrite; i++) {
	putc (PROGRESS, stderr);
	fflush (stderr);
	status = H5Dwrite (dset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL,
			   H5P_DEFAULT, the_data);
	assert (status>=0);
    }
    getrusage (RUSAGE_SELF, &r_stop);
    gettimeofday (&t_stop, NULL);
    putc ('\n', stderr);
    print_stats ("out hdf5",
		 &r_start, &r_stop, &t_start, &t_stop,
		 nread*size[0]*size[1]);

    /* Read the raw dataset */
    synchronize ();
    getrusage (RUSAGE_SELF, &r_start);
    gettimeofday (&t_start, NULL);
    fprintf (stderr, HEADING, "in raw");
    for (i=0; i<nread; i++) {
	putc (PROGRESS, stderr);
	fflush (stderr);
	offset = lseek (fd, 0, SEEK_SET);
	assert (0==offset);
	n = read (fd, the_data, size[0]*size[1]);
	assert (n==size[0]*size[1]);
    }
    getrusage (RUSAGE_SELF, &r_stop);
    gettimeofday (&t_stop, NULL);
    putc ('\n', stderr);
    print_stats ("in raw",
		 &r_start, &r_stop, &t_start, &t_stop,
		 nread*size[0]*size[1]);
    

    /* Read the hdf5 dataset */
    synchronize ();
    getrusage (RUSAGE_SELF, &r_start);
    gettimeofday (&t_start, NULL);
    fprintf (stderr, HEADING, "in hdf5");
    for (i=0; i<nread; i++) {
	putc (PROGRESS, stderr);
	fflush (stderr);
	status = H5Dread (dset, H5T_NATIVE_UCHAR, file_space, file_space,
			  H5P_DEFAULT, the_data);
	assert (status>=0);
    }
    getrusage (RUSAGE_SELF, &r_stop);
    gettimeofday (&t_stop, NULL);
    putc ('\n', stderr);
    print_stats ("in hdf5",
		 &r_start, &r_stop, &t_start, &t_stop,
		 nread*size[0]*size[1]);

    /* Read hyperslab */
    assert (size[0]>20 && size[1]>20);
    start[0] = start[1] = 10;
    count[0] = count[1] = size[0]-20;
    status = H5Sset_hyperslab (file_space, start, count, NULL);
    assert (status>=0);
    synchronize ();
    getrusage (RUSAGE_SELF, &r_start);
    gettimeofday (&t_start, NULL);
    fprintf (stderr, HEADING, "in hdf5 partial");
    for (i=0; i<nread; i++) {
	putc (PROGRESS, stderr);
	fflush (stderr);
	status = H5Dread (dset, H5T_NATIVE_UCHAR, file_space, file_space,
			  H5P_DEFAULT, the_data);
	assert (status>=0);
    }
    getrusage (RUSAGE_SELF, &r_stop);
    gettimeofday (&t_stop, NULL);
    putc ('\n', stderr);
    print_stats ("in hdf5 partial",
		 &r_start, &r_stop, &t_start, &t_stop,
		 nread*count[0]*count[1]);
    

    
    /* Close everything */
    close (fd);
    H5Dclose (dset);
    H5Sclose (file_space);
    H5Fclose (file);
    
    return 0;
}
