/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, May 13, 1998
 *
 * Purpose:	Repartitions a file family.  This program can be used to
 *		split a single file into a family of files, join a family of
 *		files into a single file, or copy one family to another while
 *		changing the size of the family members.  It can also be used
 *		to copy a single file to a single file with holes.
 */

/* See H5private.h for how to include system headers */
#include "hdf5.h"
#include "H5private.h"
#ifdef H5_STDC_HEADERS
#   include <ctype.h>
#   include <errno.h>
#   include <fcntl.h>
#   include <stdio.h>
#   include <stdlib.h>
#   include <string.h>
#endif

#ifdef H5_HAVE_UNISTD_H
#   include <sys/types.h>
#   include <unistd.h>
#endif

#ifdef H5_HAVE_SYS_STAT_H
#   include <sys/stat.h>
#endif

#ifdef WIN32
#   include <io.h>
#	include <fcntl.h>
#endif

#ifndef FALSE
#   define FALSE	0
#endif
#ifndef TRUE
#   define TRUE 	1
#endif
#   define NAMELEN	4096
#define GB	*1024*1024*1024

#ifndef MIN
#   define MIN(X,Y)	((X)<(Y)?(X):(Y))
#endif
#ifndef MIN3
#   define MIN3(X,Y,Z)	MIN(MIN(X,Y),Z)
#endif


/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Prints a usage message.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Wednesday, May 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (const char *progname)
{
    fprintf(stderr, "usage: %s [-v] [-V] [-[b|m] N[g|m|k]] SRC DST\n",
	    progname);
    fprintf(stderr, "   -v     Produce verbose output\n");
    fprintf(stderr, "   -V     Print a version number and exit\n");
    fprintf(stderr, "   -b N   The I/O block size, defaults to 1kB\n");
    fprintf(stderr, "   -m N   The destination member size or 1GB\n");
    fprintf(stderr, "   SRC    The name of the source file\n");
    fprintf(stderr, "   DST	The name of the destination files\n");
    fprintf(stderr, "Sizes may be suffixed with `g' for GB, `m' for MB or "
	    "`k' for kB.\n");
    fprintf(stderr, "File family names include an integer printf "
	    "format such as `%%d'\n");
    exit (1);
}


/*-------------------------------------------------------------------------
 * Function:	get_size
 *
 * Purpose:	Reads a size option of the form `-XNS' where `X' is any
 *		letter, `N' is a multi-character positive decimal number, and
 *		`S' is an optional suffix letter in the set [GgMmk].  The
 *		option may also be split among two arguments as: `-X NS'.
 *		The input value of ARGNO is the argument number for the
 *		switch in the ARGV vector and ARGC is the number of entries
 *		in that vector.
 *
 * Return:	Success:	The value N multiplied according to the
 *				suffix S.  On return ARGNO will be the number
 *				of the next argument to process.
 *
 *		Failure:	Calls usage() which exits with a non-zero
 *				status.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, May 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static off_t
get_size (const char *progname, int *argno, int argc, char *argv[])
{
    off_t	retval=-1;
    char	*suffix;

    if (isdigit ((int)(argv[*argno][2]))) {
	retval = strtol (argv[*argno]+2, &suffix, 10);
	(*argno)++;
    } else if (argv[*argno][2] || *argno+1>=argc) {
	usage (progname);
    } else {
	retval = strtol (argv[*argno+1], &suffix, 0);
	if (suffix==argv[*argno+1]) usage (progname);
	*argno += 2;
    }
    if (suffix && suffix[0] && !suffix[1]) {
	switch (*suffix) {
	case 'G':
	case 'g':
	    retval *= 1024 * 1024 * 1024;
	    break;
	case 'M':
	case 'm':
	    retval *= 1024 * 1024;
	    break;
	case 'k':
	    retval *= 1024;
	    break;
	default:
	    usage (progname);
	}
    } else if (suffix && suffix[0]) {
	usage (progname);
    }
    return retval;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Split an hdf5 file
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Wednesday, May 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    const char	*prog_name;		/*program name			*/
    size_t	blk_size=1024;		/*size of each I/O block	*/
    char	*buf=NULL;		/*I/O block buffer		*/
    size_t	n, i;			/*counters			*/
    ssize_t	nio;			/*I/O return value		*/
    int		argno=1;		/*program argument number	*/
    int		src, dst=-1;		/*source & destination files	*/
    int		need_seek=FALSE;	/*destination needs to seek?	*/
    int		need_write;		/*data needs to be written?	*/
    /*struct stat	sb;			temporary file stat buffer	*/
	/*struct _stati64 sb;*/
	h5_stat_t sb;

    int		verbose=FALSE;		/*display file names?		*/

    const char	*src_gen_name;		/*general source name		*/
    char	src_name[NAMELEN];	/*source member name		*/

    int		src_is_family;		/*is source name a family name?	*/
    int		src_membno=0;		/*source member number		*/

    const char	*dst_gen_name;		/*general destination name	*/
    char	dst_name[NAMELEN];	/*destination member name	*/
    int		dst_is_family;		/*is dst name a family name?	*/
    int		dst_membno=0;		/*destination member number	*/

#if defined(WIN32) && ! defined (__MWERKS__)
    __int64	left_overs=0;		/*amount of zeros left over	*/
    __int64	src_offset=0;		/*offset in source member	*/
    __int64	dst_offset=0;		/*offset in destination member	*/
    __int64	src_size;		/*source logical member size	*/
    __int64	src_act_size;		/*source actual member size	*/
    __int64	dst_size=1 GB;		/*destination logical memb size	*/
#else
    off_t	left_overs=0;		/*amount of zeros left over	*/
    off_t	src_offset=0;		/*offset in source member	*/
    off_t	dst_offset=0;		/*offset in destination member	*/
    off_t	src_size;		/*source logical member size	*/
    off_t	src_act_size;		/*source actual member size	*/
    off_t	dst_size=1 GB;		/*destination logical memb size	*/
#endif

    /*
     * Get the program name from argv[0]. Use only the last component.
     */
    if ((prog_name=strrchr (argv[0], '/'))) prog_name++;
    else prog_name = argv[0];

    /*
     * Parse switches.
     */
    while (argno<argc && '-'==argv[argno][0]) {
	if (!strcmp (argv[argno], "-v")) {
	    verbose = TRUE;
	    argno++;
	} else if (!strcmp(argv[argno], "-V")) {
	    printf("This is %s version %u.%u release %u\n",
		   prog_name, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
	    exit(0);
	} else if ('b'==argv[argno][1]) {
	    blk_size = get_size (prog_name, &argno, argc, argv);
	} else if ('m'==argv[argno][1]) {
	    dst_size = get_size (prog_name, &argno, argc, argv);
	} else {
	    usage (prog_name);
	}
    }

    /*
     * Get the name for the source file and open the first member.  The size
     * of the first member determines the logical size of all the members.
     */
    if (argno>=argc) usage (prog_name);
    src_gen_name = argv[argno++];
    sprintf (src_name, src_gen_name, src_membno);
    src_is_family = strcmp (src_name, src_gen_name);

    if ((src=HDopen(src_name, O_RDONLY,0))<0) {
	perror (src_name);
	exit (1);
    }

    if (HDfstat(src, &sb)<0) {
	perror ("fstat");
	exit (1);
    }
    src_size = src_act_size = sb.st_size;
    if (verbose) fprintf (stderr, "< %s\n", src_name);

    /*
     * Get the name for the destination file and open the first member.
     */
    if (argno>=argc) usage (prog_name);
    dst_gen_name = argv[argno++];
    sprintf (dst_name, dst_gen_name, dst_membno);
    dst_is_family = strcmp (dst_name, dst_gen_name);

    if ((dst=HDopen (dst_name, O_RDWR|O_CREAT|O_TRUNC, 0666))<0) {
	perror (dst_name);
	exit (1);
    }
    if (verbose) fprintf (stderr, "> %s\n", dst_name);

    /* No more arguments */
    if (argno<argc) usage (prog_name);

    /* Now the real work, split the file */
    buf = malloc (blk_size);
    while (src_offset<src_size) {

	/* Read a block.  The amount to read is the minimum of:
	 *    1. The I/O block size
	 *    2. What's left to write in the destination member
	 *    3. Left over zeros or what's left in the source member.
	 */
	n = blk_size;
	if (dst_is_family) n = (size_t)MIN((off_t)n, dst_size-dst_offset);
	if (left_overs) {
	    n = (size_t)MIN (n, left_overs);
	    left_overs -= n;
	    need_write = FALSE;
	} else if (src_offset<src_act_size) {
	    n = (size_t)MIN ((off_t)n, src_act_size-src_offset);
	    if ((nio=read (src, buf, n))<0) {
		perror ("read");
		exit (1);
	    } else if ((size_t)nio!=n) {
		fprintf (stderr, "%s: short read\n", src_name);
		exit (1);
	    }
	    for (i=0; i<n; i++) {
		if (buf[i]) break;
	    }
	    need_write = (i<n);
	} else {
	    n = 0;
	    left_overs = src_size - src_act_size;
	    need_write = FALSE;
	}

	/*
	 * If the block contains non-zero data then write it to the
	 * destination, otherwise just remember that we'll have to do a seek
	 * later in the destination when we finally get non-zero data.
	 */
	if (need_write) {
	    if (need_seek && HDlseek (dst, dst_offset, SEEK_SET)<0) {
		perror ("HDlseek");
		exit (1);
	    }
	    if ((nio=write (dst, buf, n))<0) {
		perror ("write");
		exit (1);
	    } else if ((size_t)nio!=n) {
		fprintf (stderr, "%s: short write\n", dst_name);
		exit (1);
	    }
	    need_seek = FALSE;
	} else {
	    need_seek = TRUE;
	}

	/*
	 * Update the source offset and open the next source family member if
	 * necessary.  The source stream ends at the first member which
	 * cannot be opened because it doesn't exist.  At the end of the
	 * source stream, update the destination offset and break out of the
	 * loop.   The destination offset must be updated so we can fix
	 * trailing holes.
	 */
	src_offset += n;
	if (src_offset==src_act_size) {
	    close (src);
	    if (!src_is_family) {
		dst_offset += n;
		break;
	    }
	    sprintf (src_name, src_gen_name, ++src_membno);
	    if ((src=HDopen (src_name, O_RDONLY,0))<0 && ENOENT==errno) {
		dst_offset += n;
		break;
	    } else if (src<0) {
		perror (src_name);
		exit (1);
	    }
	    if (HDfstat (src, &sb)<0) {
		perror ("fstat");
		exit (1);
	    }
	    src_act_size = sb.st_size;
	    if (src_act_size>src_size) {
		fprintf (stderr, "%s: member truncated to %lu bytes\n",
			 src_name, (unsigned long)src_size);
	    }
	    src_offset = 0;
	    if (verbose) fprintf (stderr, "< %s\n", src_name);
	}

	/*
	 * Update the destination offset, opening a new member if one will be
	 * needed. The first member is extended to the logical member size
	 * but other members might be smaller if they end with a hole.
	 */
	dst_offset += n;
	if (dst_is_family && dst_offset==dst_size) {
	    if (0==dst_membno) {
		if (HDlseek (dst, dst_size-1, SEEK_SET)<0) {
		    perror ("HDHDlseek");
		    exit (1);
		}
		if (read (dst, buf, 1)<0) {
		    perror ("read");
		    exit (1);
		}
		if (HDlseek (dst, dst_size-1, SEEK_SET)<0) {
		    perror ("HDlseek");
		    exit (1);
		}
		if (write (dst, buf, 1)<0) {
		    perror ("write");
		    exit (1);
		}
	    }
	    close (dst);
	    sprintf (dst_name, dst_gen_name, ++dst_membno);
	    if ((dst=HDopen (dst_name, O_RDWR|O_CREAT|O_TRUNC, 0666))<0) {
		perror (dst_name);
		exit (1);
	    }
	    dst_offset = 0;
	    need_seek = FALSE;
	    if (verbose) fprintf (stderr, "> %s\n", dst_name);
	}
    }

    /*
     * Make sure the last family member is the right size and then close it.
     * The last member can't end with a hole or hdf5 will think that the
     * family has been truncated.
     */
    if (need_seek) {
	if (HDlseek (dst, dst_offset-1, SEEK_SET)<0) {
	    perror ("HDlseek");
	    exit (1);
	}
	if (read (dst, buf, 1)<0) {
	    perror ("read");
	    exit (1);
	}
	if (HDlseek (dst, dst_offset-1, SEEK_SET)<0) {
	    perror ("HDlseek");
	    exit (1);
	}
	if (write (dst, buf, 1)<0) {
	    perror ("write");
	    exit (1);
	}
    }
    close (dst);

    /* Free resources and return */
    free (buf);
    return 0;
}
