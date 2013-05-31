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

#include "usecase.h"

void
usage(const char *prog)
{
    fprintf(stderr, "usage: %s [OPTIONS]\n", prog);
    fprintf(stderr, "  OPTIONS\n");
    fprintf(stderr, "     -h, --help            Print a usage message and exit\n");
    fprintf(stderr, "     -f FN                 Test file name [default: %s.h5]\n", prog);
    fprintf(stderr, "     -l w|r                launch writer or reader only. [default: launch both]\n");
    fprintf(stderr, "     -s N, --swmr=N        Use SWMR mode (0: no, non-0: yes) default is yes\n");
    fprintf(stderr, "     -z N, --chunksize=N   Chunk size [default: %d]\n", Chunksize_DFT);
    fprintf(stderr, "\n");
}

/* Setup Use Case parameters by parsing command line options.
* Setup default values if not set by options. */
int
parse_option(int argc, char * const argv[])
{
    int ret_value=0;
    int c;
    /* command line options: See function usage for a description */
    const char *nagg_options = "f:hl:s:z:";

    /* suppress getopt from printing error */
    opterr = 0;

    while (1){
	c = getopt (argc, argv, nagg_options);
	if (-1 == c)
	    break;
	switch (c) {
	  case 'h':
	    usage(progname_g);
	    break;
	  case 'f':	/* usecase data file name */
	    UC_opts.filename = optarg;
	    break;
	  case 'l':	/* launch reader or writer only */
	    switch (*optarg) {
	      case 'r':	/* reader only */
		UC_opts.launch = UC_READER;
		break;
	      case 'w': /* writer only */
		UC_opts.launch = UC_WRITER;
		break;
	      default:
		fprintf(stderr, "launch value(%c) should be w or r only.\n", *optarg);
		usage(progname_g);
		Hgoto_error(-1);
		break;
	    }
	    break;
	  case 's':	/* use swmr file open mode */
	    if ((UC_opts.use_swmr = atoi(optarg)) < 0){
		fprintf(stderr, "swmr value should be 0(no) or 1(yes)\n");
		usage(progname_g);
		Hgoto_error(-1);
	    };
	    break;
	  case 'z':	/* size of chunk=(z,z) */
	    if ((UC_opts.chunksize = atoi(optarg)) <= 0){
		fprintf(stderr, "bad chunksize %s, must be a positive integer\n", optarg);
		usage(progname_g);
		Hgoto_error(-1);
	    };
	    break;
	  case '?':
	    fprintf(stderr, "getopt returned '%c'.\n", c);
	    Hgoto_error(-1);
	  default:
	    fprintf(stderr, "getopt returned unexpected value.\n");
	    fprintf(stderr, "Unexpected value is %d\n", c);
	    Hgoto_error(-1);
	}
    }

    /* set nplanes */
    if (UC_opts.nplanes == 0)
	UC_opts.nplanes = UC_opts.chunksize;

    /* set test file name if not given */
    if (!UC_opts.filename){
	/* default data file name is <progname>.h5 */
	if ((UC_opts.filename=(char*)HDmalloc(HDstrlen(progname_g)+4))==NULL) {
	    fprintf(stderr, "malloc: failed\n");
	    Hgoto_error(-1);
	};
	HDstrcpy(UC_opts.filename, progname_g);
	HDstrcat(UC_opts.filename, ".h5");
    }

done:
    /* All done. */
    return(ret_value);
}
