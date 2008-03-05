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

#include "hdf5.h"
#include <signal.h>
#include <sys/time.h>
#include <math.h>
#include <stdlib.h>

/*
 * Header file for the trecover test program.
 *
 * Creator: Albert Cheng, Jan 28, 2008.
 */

/* Macro definitions */
/* Crash modes */
#define SyncCrash	0
#define AsyncCrash	1
#define	CRASH	crasher(SyncCrash, 0)

/* Dataset properties */
#define DSContig	0x1		/* Contigous type */
#define DSChunked	0x2		/* Chunked type */
#define DSZip		0x4		/* Zlib compressed */
#define DSSZip		0x8		/* SZlib compressed */
#define DSAll		~0x0   		/* All datasets */
#define DSNone		0x0   		/* No datasets */

/* Dataset default dimensions. Intentional small for easier dumping of data. */
#define RANK   2
#define NX     800                    /* dataset dimensions */
#define NY     16
#define ChunkX 8		    /* Dataset chunk sizes */
#define ChunkY 8
#define H5FILE_NAME        "trecover.h5"
#define CTL_H5FILE_NAME    "CTL"H5FILE_NAME	/* control file name */

/* Data Structures */
typedef union CrasherParam_t {
    float tinterval;		/* time interval to schedule an Async Crash */
} CrasherParam_t;


/* Global variables */
extern CrasherParam_t	AsyncCrashParam;
extern int		CrashMode;
extern hid_t		file, ctl_file;    /* file id and control file id*/

/* protocol definitions */
void crasher(int crash_mode, CrasherParam_t *crash_param);
void writer(hid_t file, int dstype, int rank, hsize_t *dims, hsize_t *dimschunk);
void wakeup(int signum);
void parser(int ac, char **av);	/* command option parser */
void init(void);		/* initialization */
void help(void);		/* initialization */
int create_files(char *filename, char *ctl_filename);
int close_file(hid_t fid);
