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
 * This is the Main body of the HDF5 Test program for the h5recover tool.
 * It creates two HDF5 files, one as the Data file, the other the Control file.
 * The two files are intended to contain identical data. The Control file will
 * be completed with no error but the Data file, though written with the same
 * data, is intentional crashed without proper file flush or close.
 * 
 * (The two files are then verified by processes outside of this program.)
 * The crashed Data file is restored by the h5recover tool and compared with
 * the Control file, expecting identical file content up to the last successful
 * file flush or close of the Data file.
 *
 * Creator: Albert Cheng, Jan 28, 2008.
 */
 
#include "trecover.h"

/* Global variables */
CrasherParam_t	AsyncCrashParam;
int		CrashMode = SyncCrash;	/* default to synchronous crash */
hid_t		file, ctl_file;         /* file id and control file id*/

/* local variables */
#if 0
static int	DSTypes=DSNone;		/* set to none first. */
#else
static int	DSTypes=DSChunked;	/* set to Chunked temp. first. */
#endif

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
		if (--ac > 0){
		    av++;
		    AsyncCrashParam.tinterval = atof(*av);
		}else{
		    fprintf(stderr, "Missing async time value\n");
		    help();
		    exit(1);
		}
		break;
	    case 'd':	/* -d dataset type */
		if (--ac > 0){
		    av++;
		    switch (**av){
		    case 'C':
			DSTypes=DSTypes|DSContig;
			break;
		    case 'K':
			DSTypes=DSTypes|DSChunked;
			break;
		    case 'G':
			DSTypes=DSTypes|DSZip;
			break;
		    case 'S':
			DSTypes=DSTypes|DSSZip;
			break;
		    case 'A':
			DSTypes=DSTypes|DSAll;
			break;
		    default:
			fprintf(stderr, "Unknown Dataset type (%c)\n", **av);
			help();
			exit(1);
		    }
		}else{
		    fprintf(stderr, "Missing async time value\n");
		    help();
		    exit(1);
		}
		break;
	    case 'h':	/* -h help option */
		help();
		exit(0);
		break;
	    default:
		fprintf(stderr, "Unknown command option(%s)\n", *av);
		help();
		exit(1);
	    }
	}else{
	    fprintf(stderr, "Unknown command option(%s)\n", *av);
	    help(); 
	    exit(1);
	}
    } /* end of while */
    if (DSTypes==DSNone){
	/* reset to default all datasets */
	DSTypes=DSAll;
    }
}


/* initialization */
void
init(void)
{
    AsyncCrashParam.tinterval = 0;
}


/* show help pages */
void
help(void)
{
    fprintf(stderr, "Usage: trecover [-a <seconds>] [-d <dataset-type>] [-h]\n"
		"\t-a\tAsync crash seconds where <seconds> is a real number.\n"
		"\t-d\tDataset to create. <dataset-type> can be:\n"
		"\t\t  A\tAll datasets\n"
		"\t\t  C\tContingous datasets\n"
		"\t\t  G\tGzip compressed datasets\n"
		"\t\t  K\tChunked datasets\n"
		"\t\t  S\tSzip compressed datasets\n"
		"\t\tDefault is all datasets\n"
		"\t\tTemp Default is Chunked datasets\n"
	    );
}


int
main (int ac, char **av)
{
    hsize_t     dims[RANK]={NX,NY};              /* dataset dimensions */
    hsize_t     dimschunk[RANK]={ChunkX,ChunkY}; /* dataset chunk dimensions */

    init();
    parser(ac, av);

    /* create/open both files. */
    create_files(H5FILE_NAME, CTL_H5FILE_NAME, JNL_H5FILE_NAME);

    /* create datasets in Control file first. */
    writer(ctl_file, DSTypes, RANK, dims, dimschunk);
    close_file(ctl_file);

    /* Schedule Async crash if requested. */
    if (AsyncCrashParam.tinterval > 0)
	crasher(AsyncCrash, &AsyncCrashParam);

    /* create datasets in data file. */
    writer(file, DSTypes, RANK, dims, dimschunk);
    
    /* Do a sync crash. */
    if (AsyncCrashParam.tinterval == 0)
	CRASH;

    /* Close file only if Async crash is scheduled but has not occurred yet. */
    close_file(file);

    return(0);
}     
