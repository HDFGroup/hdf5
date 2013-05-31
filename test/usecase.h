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

/*
 * Use Case Header file: common definitions for use cases tests.
 */
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include "hdf5.h"

/* Macro definitions */
#define Hgoto_error(val)	{ret_value=val; goto done;}
#define Hgoto_done		{goto done;}
#define Chunksize_DFT		256	/* chunksize default */
#define ErrorReportMax		10	/* max number of errors reported */
/* these two definitions must match each other */
#define UC_DATATYPE		H5T_NATIVE_SHORT    /* use case HDF5 data type */
#define UC_CTYPE		short		    /* use case C data type */

/* type declarations */
typedef struct options_t {
    int num_dsets;              /* number of datasets                   */
    int use_swmr;               /* use swmr open or not 		*/
    int compress;		/* 0: no compress			*/
    int h5_use_chunks;     	/* 0/1: Not use/use chunked dataset     */
    int chunksize;		/* chunks are chunksize^2 planes	*/
    int nplanes;		/* number of planes, default chunksize  */
    char *filename;		/* use case data filename		*/
} options_t;

/* global variables declarations */
extern options_t UC_opts;	/* Use Case Options */
extern const char *progname_g;	/* Program name */

/* prototype declarations */
int parse_option(int argc, char * const argv[]);
void usage(const char *prog);
int create_uc_file(void);
int write_uc_file(void);
int read_uc_file(void);

/* private definitions of Standard functions */

/* Standard POSIX functions */
#define HDassert(s)		assert(s)
#define HDfree(s)		free(s)
#define HDgetenv(s)		getenv(s)
#define HDmalloc(s)		malloc(s)
#define HDmemcpy(X,C,Z)		memcpy(X,C,Z)
#define HDmemset(X,C,Z)		memset(X,C,Z)
#define HDperror(s)		perror(s)
#define HDstrcat(s1, s2)	strcat(s1, s2)
#define HDstrcmp(s1, s2)	strcmp(s1, s2)
#define HDstrcpy(s1, s2)	strcpy(s1, s2)
#define HDstrdup(s)		strdup(s)
#define HDstrlen(s)		strlen(s)
#define HDstrncpy(s1, s2, N)	strncpy(s1, s2, N)
#define HDstrrchr(s, c)		strrchr(s, c)
