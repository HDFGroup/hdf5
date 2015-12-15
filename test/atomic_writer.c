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

/*-------------------------------------------------------------------------
 *
 * Created:    	atomic_writer.c
 *
 * Purpose:   	This is the "writer" part of the standalone test to check 
 *		atomic read-write operation on a system.
 *		a) atomic_writer.c--the writer (this file)
 *		b) atomic_reader.c--the reader
 *		c) atomic_data--the name of the data file used by writer and reader
 *		
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

/****************/
/* Local Macros */
/****************/

#define FILENAME "atomic_data"

/********************/
/* Local Prototypes */
/********************/

static void usage(void);


/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     To print information about the command line options
 *
 * Parameters: 	None
 *
 * Return:   	void
 *
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    printf("\n");
    printf("Usage error!\n");
    printf("Usage: atomic_writer -n <number of integers to write> -i <number of iterations for writer>\n");
    printf("       Note**The number of integers for option n has to be positive\n");
    printf("       Note**The number of integers for option i has to be positive\n");
    printf("\n");
} /* usage() */



/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose: 	To write a series of integers to a file for the reader to verify the data.
 *		A write is atomic if the whole amount written in one operation is not interleaved 
 *		with data from any other process.
 *		(1) Iterate with i iterations
 *		(2) Write a series of integers (0 to n-1) to the file with this pattern:
 *		offset 0: 	0000000000000000000000000000000
 *		offset 1:  	 111111111111111111111111111111
 *		offset 2:   	  22222222222222222222222222222
 *		offset 3:    	   3333333333333333333333333333
 *		...
 *		...
 *		offset n-1: 		 		  (n-1)
 *
 *		At the end of the writes, the data in the file will be:
 *		01234567........(n-1)
 *
 * Note:
 *      (a) The # of integers (via -n option) used by the writer and reader should be the same.
 *      (b) The data file used by the writer and reader should be the same.
 *
 * Future enhancement:
 *      1) Provide default values for n and i and allow user to run with either 0 or 1 option
 *      2) Use HDF library HD<system calls> instead of the system calls
 *      3) Handle large sized buffer (gigabytes) if needed
 *	
 * Return:      Success:      0
 *              Failure:      -1
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    int fd = -1;			/* file descriptor */
    ssize_t bytes_wrote;		/* the nubmer of bytes written */
    unsigned int *buf = NULL;		/* buffer to hold written data */
    unsigned int n, u, i;		/* local index variable */
    int temp;				/* temporary variable */
    unsigned int iterations = 0;	/* the input for "-i" */
    unsigned int num = 0;		/* the input for "-n" */
    int opt = 0;			/* option char */

    /* Ensure the # of arguments is as expected */
    if(argc != 5) {
        usage();
	exit(-1);
    }

    /* Parse command line options */
    while((opt = getopt(argc, argv, "n:i:")) != -1) {
	switch(opt) {
	    case 'n':
		if((temp = atoi(optarg)) < 0) {
		    usage();
		    exit(-1);
		}
		num = (unsigned int)temp;
		break;
	    case 'i':
		if((temp = atoi(optarg)) < 0) {
		    usage();
		    exit(-1);
		}
		iterations = (unsigned int)temp;
		break;
	    default:
		printf("Invalid option encountered\n");
		break;
	}
    }

    printf("WRITER: # of integers to write = %u; # of iterations = %d\n", num, iterations);

    /* Remove existing data file if needed */
    if(remove(FILENAME) < 0) {
	if(errno == ENOENT)
	    printf("WRITER: remove %s--%s\n", FILENAME, strerror(errno));
	else {
	    printf("WRITER: error from remove: %d--%s\n", errno, strerror(errno));
	    goto error;
	}
    } else
	printf("WRITER: %s is removed\n", FILENAME);

    /* Create the data file */
    if((fd = open(FILENAME, O_RDWR|O_TRUNC|O_CREAT, 0664)) < 0) {
	printf("WRITER: error from open\n");
	goto error;
    }

    /* Allocate buffer for holding data to be written */
    if((buf = (unsigned int *)malloc(num * sizeof(unsigned int))) == NULL) {
	printf("WRITER: error from malloc\n");
	if(fd >= 0 && close(fd) < 0)
	    printf("WRITER: error from close\n");
	goto error;
    }

    printf("\n");

    for(i = 1; i <= iterations; i++) { /* iteration loop */
	printf("WRITER: *****start iteration %u*****\n", i);

	/* Write the series of integers to the file */
	for(n = 0; n < num; n++)  { /* write loop */

	    /* Set up data to be written */
	    for(u=0; u < num; u++)
		buf[u] = n;

	    /* Position the file to the proper location */
	    if(lseek(fd, (off_t)(n*sizeof(unsigned int)), SEEK_SET) < 0) {
		printf("WRITER: error from lseek\n");
		goto error;
	    }
    
	    /* Write the data */
	    if((bytes_wrote = write(fd, buf, ((num-n) * sizeof(unsigned int)))) < 0) {
		printf("WRITER: error from write\n");
		goto error;
	    }

	    /* Verify the bytes written is correct */
	    if(bytes_wrote != (ssize_t)((num-n) * sizeof(unsigned int))) {
		printf("WRITER: error from bytes written\n");
		goto error;
	    }
	} /* end for */

	printf("WRITER: *****end iteration %u*****\n\n", i);

    } /* end for */

    /* Close the file */
    if(close(fd) < 0) {
	printf("WRITER: error from close\n");
	goto error;
    }

    /* Free the buffer */
    if(buf) free(buf);

    return(0);

error:
    return(-1);
} /* main() */
