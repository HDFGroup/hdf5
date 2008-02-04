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

/* Data Structures */
typedef union CrasherParam_t {
    float tinterval;		/* time interval to schedule an Async Crash */
} CrasherParam_t;


/* Global variables */
extern CrasherParam_t	AsyncCrashParam;
extern int		CrashMode;

/* protocol definitions */
void crasher(int crash_mode, CrasherParam_t *crash_param);
void writer(void);
void wakeup(int signum);
void parser(int ac, char **av);	/* command option parser */
void init(void);		/* initialization */
void help(void);		/* initialization */
