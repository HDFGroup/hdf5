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
 * This example writes data to the HDF5 file.
 * It creates three datasets, first one as a regular dataset, second one
 * uses gzip compression if supported, and the third one use szip compression
 * if supported.
 *
 * Creator: Albert Cheng, Jan 28, 2008.
 */
 
#include "trecover.h"

CrasherParam_t	AsyncCrashParam;
int		CrashMode = SyncCrash;	/* default to synchronous crash */

/* Command arguments parser.
 * 	-a <seconds>	Do Async crash with a floating value of seconds
 */
void
parser(int ac, char **av)
{
    while (--ac > 0) {
	av++;
	if (av[0][0] == '-'){
	    switch (av[0][1]){
	    case 'a':	/* -a async option */
		ac--; av++;
		AsyncCrashParam.tinterval = atof(*av);
		break;
	    case 'h':	/* -h help option */
		help();
		break;
	    default:
		fprintf(stderr, "Unknown command option(%s)\n", *av);
		help();
		return;
	    }
	}else{
	    fprintf(stderr, "Unknown command option(%s)\n", *av);
	    help(); 
	    return;
	}
    } /* end of while */
}


/* initialization */
void
init()
{
    AsyncCrashParam.tinterval = 0;
}


/* show help pages */
void
help()
{
    fprintf(stderr, "Usage: trecover [-a <seconds>]\n");
}


int
main (int ac, char **av)
{
    init();
    parser(ac, av);
    if (AsyncCrashParam.tinterval > 0)
	crasher(AsyncCrash, &AsyncCrashParam);
    writer();
}     
