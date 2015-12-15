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
 * Created:    	atomic_reader.c
 *
 * Purpose:     This is the "reader" part of the standalone test to check
 *              atomic read-write operation on a system.
 *              a) atomic_reader.c--the reader (this file)
 *              a) atomic_writer.c--the writer
 *              c) atomic_data--the name of the data file used by writer and reader
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/****************/
/* Local Macros */
/****************/

#define FILENAME "atomic_data"
#define READ_TRIES 20
#define OPEN_TRIES 50

/********************/
/* Local Prototypes */
/********************/

static void usage(void);
int verify(int fd, unsigned int k);
void print_info(int *info, unsigned int lastr, unsigned iteration);



/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     To print the command line options
 *
 * Parameters:  None
 *
 * Return: 	void
 *
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    printf("\n");
    printf("Usage error!\n");
    printf("Usage: atomic_reader -n <number of integers to read> -i <number of iterations for reader>\n");
    printf("       Note**The number of integers for option n has to be positive\n");
    printf("       Note**The number of integers for option i has to be positive\n");
    printf("\n");
} /* usage() */


/*-------------------------------------------------------------------------
 * Function:    verify
 *
 * Purpose:     To verify that the data read is the pattern expected.
 *		Each integer read should be the same as the index.
 *		When a difference is encountered, the remaining integers 
 *		read should be the same as the previous index.
 *		For example, the pattern expected should be either:
 *		a) 01234567....n-1	
 *		or
 *		b) if at index 4, a difference is encountered,
 *		   the remaining integers should be all "3"s as:
 *			012333333333333
 *
 * Parameters:  
 *	fd--the file descriptor
 *	k--the number of integers to read
 *
 * Return:      
 *	positive on success
 *	negative on failure
 *
 *-------------------------------------------------------------------------
 */
int
verify(int fd, unsigned int k)
{
    unsigned int i;		/* local index variable */
    ssize_t bytes_read;		/* the number of bytes read */
    unsigned int *buf = NULL;	/* buffer to hold data read */

    /* Allocate buffer for data read */
    if((buf = (unsigned int *)malloc(k * sizeof(unsigned int))) == NULL) {
	printf("READER: error from malloc\n");
	goto error;
    }

    /* Position the file at the beginning */
    if(lseek(fd, (off_t)0, SEEK_SET) < 0) {
	printf("READER: error from lseek\n");
	goto error;
    }

    /* Read the whole file */
    if((bytes_read = read(fd, buf, (k * sizeof(unsigned int)))) < 0) {
	printf("READER: error from read\n");
	goto error;
    }

    /* Verify the bytes read are correct */
    if(bytes_read != (ssize_t)(k*sizeof(unsigned int))) {
	printf("READER: error from bytes read=%lu\n", (unsigned long)bytes_read);
	goto error;
    }

    /* Verify data read */
    for(i=0; i < k; i++)  {
	if(buf[i] != i)
	    break;
    }

    if(i < k) {
	/* Compare the beginning and ending sentinel values */
	if(buf[k-1] != (i-1)) {
	    printf("FAIL IN READER: ...beginning sentinel value=%u, i=%u\n", (i-1), i);
	    printf("FAIL IN READER: buf[%u]=%u\n", i-1, buf[i-1]);
	    printf("FAIL IN READER: buf[%u]=%u\n", i, buf[i]);
	    printf("FAIL IN READER: buf[%u]=%u\n", i+1, buf[i+1]);
	    printf("FAIL IN READER: ...ending sentinel value=%u\n", buf[k-1]);
	    goto error;
	}
    }

    /* Free the buffer */
    if(buf) free(buf);
    return(0);

error:
    /* Free the buffer */
    if(buf) free(buf);
    return(-1);
} /* verify() */



/*-------------------------------------------------------------------------
 * Function:    print_info
 *
 * Purpose:     To print the statistics gathered for re-reads
 *
 * Parameters: 
 *	info--the array storing the statistics for re-reads
 *	lastr--the last read completed
 *	iteration--the current iteration
 *
 * Return:     	void
 *
 *-------------------------------------------------------------------------
 */
void
print_info(int *info, unsigned int lastr, unsigned iteration)
{
    unsigned j;	/* local index variable */

    printf("--------statistics for %u reads (iteration %u)--------\n", lastr, iteration);

    for(j = 0; j <= READ_TRIES; j++)
	printf("# of %u re-tries = %u\n", j, info[j]);

    printf("--------end statistics for %u reads (iteration %u)--------\n", lastr, iteration);
} /* print_info() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     To verify that the data read is the pattern expected.
 *		(1) Make sure the file opens successfully and the # of bytes read is as expected	
 *		(2) Iterate the reader with i iterations 
 *		(3) Read and verify n integers for each iteration
 *		(4) On verification error, re-read the data at most READ_TRIES
 *	  	    times to see if correct data can be obtained
 *		(5) Print out statistics for the number of re-retries for each iteration
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
 * Return:      Success:       0
 *              Failure:       -1
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    int fd = -1;			/* file descriptor */
    unsigned int j=0, i=0, m=0;		/* local index variables */
    int temp;				/* temporary variable */
    unsigned int iterations = 0;	/* the input for "-i" */
    unsigned num = 0;			/* the input for "-n" */
    int opt = 0;			/* option char */
    int info[READ_TRIES+1];		/* re-tries statistics */

    /* Ensure the expected # of arguments */
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

    printf("READER: number of integers to read = %u; # of iterations = %d\n", num, iterations);

    printf("\n");
    for(i = 1; i <= iterations; i++) { /* iteration loop */
	unsigned opens = OPEN_TRIES;

	printf("READER: *****start iteration %u*****\n", i);

	/* Ensure open and file size are done properly */
	while(opens--) { /* open loop */
	    struct stat sinfo;

	    memset(&sinfo, 0, sizeof(sinfo));

	    if((fd = open(FILENAME, O_RDONLY, 0644)) < 0) {
		printf("READER: error from open--retry open again\n");
	    } else {
		printf("READER: open succeed\n");

		if((fstat(fd, &sinfo) == 0) &&
		   (sinfo.st_size == (off_t)(num * sizeof(unsigned int)))) {
		    printf("READER: file size is correct--%u\n", (unsigned int)sinfo.st_size);
		    break;
		}

		printf("READER: error from fstat or file size of %u is incorrect--retry open again\n", (unsigned int)sinfo.st_size);
		if(close(fd) < 0) {
		    printf("READER: error from close\n");
		    return(-1);
		}
		fd = -1;
	    }

	} /* end while */

	if(fd < 0) {
	    printf("READER: *****open failure/incorrect file size for all %u tries, continue next iteration*****\n\n", OPEN_TRIES);
	    continue;
	}

	memset(info, 0, sizeof(info));

	/* Read and verify data */
	for(j = 1; j <= num; j++) { /* read loop */

	    printf("READER: doing read %u\n", j);
	    if(verify(fd, num) < 0) {
		printf("READER: error from read %u\n", j);

		/* Perform re-read to see if correct data is obtained */
		for(m = 1; m <= READ_TRIES; m++) { /* re-read loop */
		    printf("READER: ===============going to do re-read try %u\n", m);
		    if(verify(fd, num) < 0)
			printf("READER: ===============error from re-read try %u\n", m);
		    else {
			++info[m];
			printf("READER: ===============SUCCESS from re-read try %u\n", m);
			break;
		    }
		} /* end for */

		if(m > READ_TRIES) {
		    printf("READER: ===============error from all re-read tries: %u\n", READ_TRIES);
		    printf("READER:*****ERROR--stop on read %u\n", j);
		    break;
		}
	    } else {
		++info[0];
		printf("READER: success from read %u\n", j);
	    }

	} /* end for */

	/* Print the statistics for re-reads */
	print_info(info, j-1, i);

	/* Close the file */
	if(close(fd) < 0) {
	    printf("READER: error from close\n");
	    return(-1);
	}

	printf("READER: *****end iteration %u*****\n\n", i);

    } /* end for */

    return(0);
}
