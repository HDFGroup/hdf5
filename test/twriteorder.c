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

/***********************************************************
*
* Test program: twriteorder
*
* Test to verify that the write order is strictly consistent.
* The SWMR feature requires that the order of write is strictly consistent.
* <<design requirements of SWMR>>
*
* Created: Albert Cheng, 2013/5/28.
* Modified:
*************************************************************/

/***********************************************************
*
* Algorithm
*
* The test simulates what SWMR does by writing chained blocks and see if
* they can be read back correctly.
* There is a writer process and multiple read processes.
* The file is divided into 2KB partitions. Then writer writes 1 chained
* block, each of 1KB big, in each partition after the first partition.
* Each chained block has this structure:
* Byte 0-3: offset address of its child block. The last child uses 0 as NULL.
* Byte 4-1023: some artificial data.
* The child block address of Block 1 is NULL (0).
* The child block address of Block 2 is the offset address of Block 1.
* The child block address of Block n is the offset address of Block n-1.
* After all n blocks are written, the offset address of Block n is written
* to the offset 0 of the first partition.
* Therefore, by the time the offset address of Block n is written to this
* position, all n chain-linked blocks have been written.
*
* The other reader processes will try to read the address value at the
* offset 0. The value is initially NULL(0). When it changes to non-zero,
* it signifies the writer process has written all the chain-link blocks
* and they are ready for the reader processes to access.
*
* If the system, in which the writer and reader processes run, the readers
* will always get all chain-linked blocks correctly. If the order of write
* is not maintained, some reader processes may found unexpect block data.
*
*************************************************************/

#include "h5test.h"

#define DATAFILE   "twriteorder.dat"
#define READERS_MAX	10	/* max number of readers */
#define BLOCKSIZE_DFT   1024	/* 1KB */
#define PARTITION_DFT	2048	/* 2KB */
#define NLINKEDBLOCKS_DFT 512	/* default 512 */
#define ITERATIONS_DFT	10	/* default 10 */
#define SIZE_BLKADDR	4	/* expected sizeof blkaddr */
#define Hgoto_error(val)	{ret_value=val; goto done;}
#define Hgoto_done		{goto done;}

/* type declarations */
typedef enum part_t {
    UC_READWRITE	=0,	/* both writer and reader */
    UC_WRITER,			/* writer only */
    UC_READER			/* reader only */
} part_t;

/* prototypes */
int create_wo_file(void);
int write_wo_file(void);
int read_wo_file(void);
int setup_parameters(int argc, char * const argv[]);

/* Global Variable definitions */
const char *progname_g="twriteorder";	/* program name */
int	write_fd_g;
int	blocksize_g, part_size_g, nlinkedblock_g, iterations_g;
part_t  launch_g;

/* Setup parameters for the test case.
 * Return: 0 succeed; -1 fail.
 */
int setup_parameters(int argc, char * const argv[])
{
    /* test case defaults */
    blocksize_g = BLOCKSIZE_DFT;
    part_size_g = PARTITION_DFT;
    nlinkedblock_g = NLINKEDBLOCKS_DFT;
    iterations_g = 1;
    launch_g = UC_READWRITE;

    /* parse options */
    /* no option support yet */
    /* dummy assignment to silence compiler warnings */
    argc = argc;
    argv = argv;
#if 0
    if (parse_option(argc, argv) < 0){
	return(-1);
    }
#endif

    /* show parameters and return */
#if 0
    show_parameters();
#else
    printf("blocksize = %ld\n", (long)blocksize_g);
    printf("part_size = %ld\n", (long)part_size_g);
    printf("nlinkedblock = %ld\n", (long)nlinkedblock_g);
    printf("iterations = %ld\n", (long)iterations_g);
#endif
    return(0);
}

/* Create the test file with initial "empty" file, that is,
 * partition 0 has a null (0) address.
 *
 * Return: 0 succeed; -1 fail.
 */
int create_wo_file(void)
{
    int    blkaddr=0;	/* blkaddress of next linked block */
    int	   ret_code;

    /* Create the data file */
    if ((write_fd_g = HDopen(DATAFILE, O_RDWR|O_TRUNC|O_CREAT, 0664)) < 0) {
	printf("WRITER: error from open\n");
	return -1;
    }
    blkaddr=0;
    /* write it to partition 0 */
    if ((ret_code=HDwrite(write_fd_g, &blkaddr, (size_t)SIZE_BLKADDR)) != SIZE_BLKADDR){
	printf("blkaddr write failed\n");
	return -1;
    }

    /* File initialized, return success */
    return 0;
}

int write_wo_file(void)
{
    int blkaddr;
    int blkaddr_old=0;
    int i;
    char buffer[BLOCKSIZE_DFT];
    char pbuffer=&buffer[0];
    int  ret_code;
    

    /* write block 1, 2, ... */
    for (i=1; i<nlinkedblock_g; i++){
	/* calculate where to write this block */
	blkaddr = i*part_size_g + i;
	/* store old block address in byte 0-3 */
	HDmemcpy(&buffer[0], &blkaddr_old, sizeof(blkaddr_old));
	/* fill the rest with the lowest byte of i */
	HDmemset(&buffer[4], i & 0xff, BLOCKSIZE_DFT-4);
	/* write the block */
	printf("writting block at %d\n", blkaddr);
	HDlseek(write_fd_g, blkaddr, SEEK_SET);
	if ((ret_code=HDwrite(write_fd_g, buffer, (size_t)blocksize_g)) != blocksize_g){
	    printf("blkaddr write failed in partition %d\n", i);
	    return -1;
	}
	blkaddr_old = blkaddr;
    }
    /* write the last blkaddr in partition 0 */
    HDlseek(write_fd_g, 0, SEEK_SET);
    if ((ret_code=HDwrite(write_fd_g, &blkaddr_old, (size_t)sizeof(blkaddr_old))) != sizeof(blkaddr_old)){
	printf("blkaddr write failed in partition %d\n", 0);
	return -1;
    }

    /* all writes done. return succeess. */
    return 0;
}

int read_wo_file(void)
{
    int read_fd;
    int blkaddr=0;
    int ret_code;
    char buffer[BLOCKSIZE_DFT];

    /* Open the data file */
    if ((read_fd = HDopen(DATAFILE, O_RDONLY, 0)) < 0) {
	printf("READER: error from open\n");
	return -1;
    }
    /* keep reading the initial block address until it is non-zero before proceeding. */
    while (blkaddr == 0){
	HDlseek(read_fd, 0, SEEK_SET);
	if ((ret_code=HDread(read_fd, &blkaddr, (size_t)sizeof(blkaddr))) != sizeof(blkaddr)){
	    printf("blkaddr read failed in partition %d\n", 0);
	    return -1;
	}
    }
    /* got a non-zero blkaddr. Proceed down the linked blocks. */
    printf("got initial block address=%d\n", blkaddr);
    while (blkaddr != 0){
	HDlseek(read_fd, blkaddr, SEEK_SET);
	if ((ret_code=HDread(read_fd, buffer, (size_t)blocksize_g)) != blocksize_g){
	    printf("blkaddr read failed in partition %d\n", 0);
	    return -1;
	}
	/* retrieve the block address in byte 0-3 */
	HDmemcpy(&blkaddr, &buffer[0], sizeof(blkaddr));
	printf("got next block address=%d\n", blkaddr);
    }

    return 0;
}


/* Overall Algorithm: 
 * Parse options from user;
 * Generate/pre-created the test file needed and close it;
 * fork: child processes become the reader processes;
 *       while parent process continues as the writer process;
 * both run till ending conditions are met.
 */
int
main(int argc, char *argv[])
{
    /*pid_t childpid[READERS_MAX];
    int child_ret_value[READERS_MAX];*/
    pid_t childpid;
    int child_ret_value;
    pid_t mypid, tmppid;
    int	child_status;
    int child_wait_option=0;
    int ret_value = 0;

    /* initialization */
    if (setup_parameters(argc, argv) < 0){
	Hgoto_error(1);
    }

    /* ==============================================================*/
    /* UC_READWRITE: create datafile, launch both reader and writer. */
    /* UC_WRITER:    create datafile, skip reader, launch writer.    */
    /* UC_READER:    skip create, launch reader, exit.               */
    /* ==============================================================*/
    /* ============*/
    /* Create file */
    /* ============*/
    if (launch_g != UC_READER){
	printf("Creating skeleton data file for test...\n");
	if (create_wo_file() < 0){
	    fprintf(stderr, "***encounter error\n");
	    Hgoto_error(1);
	}else
	    printf("File created.\n");
    }

    if (launch_g==UC_READWRITE){
	/* fork process */
	if((childpid = fork()) < 0) {
	    perror("fork");
	    Hgoto_error(1);
	};
    };
    mypid = getpid();

    /* ============= */
    /* launch reader */
    /* ============= */
    if (launch_g != UC_WRITER){
	/* child process launch the reader */
	if(0 == childpid) {
	    printf("%d: launch reader process\n", mypid);
	    if (read_wo_file() < 0){
		fprintf(stderr, "read_wo_file encountered error\n");
		exit(1);
	    }
	    exit(0);
	}
    }

    /* ============= */
    /* launch writer */
    /* ============= */
    /* this process continues to launch the writer */
    printf("%d: continue as the writer process\n", mypid);
    if (write_wo_file() < 0){
	fprintf(stderr, "write_wo_file encountered error\n");
	Hgoto_error(1);
    }

    /* ================================================ */
    /* If readwrite, collect exit code of child process */
    /* ================================================ */
    if (launch_g == UC_READWRITE){
	if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0){
	    perror("waitpid");
	    Hgoto_error(1);
	}
	if (WIFEXITED(child_status)){
	    if ((child_ret_value=WEXITSTATUS(child_status)) != 0){
		printf("%d: child process exited with non-zero code (%d)\n",
		    mypid, child_ret_value);
		Hgoto_error(2);
	    }
	} else {
	    printf("%d: child process terminated abnormally\n", mypid);
	    Hgoto_error(2);
	}
    }
    
done:
    /* Print result and exit */
    if (ret_value != 0){
	printf("Error(s) encountered\n");
    }else{
	printf("All passed\n");
    }

    return(ret_value);
}

#if 0

#endif
