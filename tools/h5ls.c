/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, March 23, 1998
 */
#include <ctype.h>
#include <h5tools.h>
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <H5private.h>
#ifndef HAVE_ATTRIBUTE
#   undef __attribute__
#   define __attribute__(X) /*void*/
#   define __unused__ /*void*/
#else
#   define __unused__ __attribute__((unused))
#endif

/* Command-line switches */
static int verbose_g = 0;
static int dump_g = 0;
static int width_g = 80;


/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Prints a usage message on stderr and then returns.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage (const char *progname)
{
    fprintf(stderr, "\
usage: %s [OPTIONS] FILE [GROUP]\n\
   OPTIONS\n\
      -h, -?, --help   Print a usage message and exit\n\
      -d, --dump       Print the values of datasets\n\
      -wN, --width=N   Set the number of columns of output\n\
      -v, --verbose    Generate more verbose output\n\
      -V, --version    Print version number and exit\n\
   FILE\n\
      The file name may include a printf(3C) integer format such as\n\
      \"%%05d\" to open a file family.\n\
   GROUP\n\
      If a group name is not specified then the contents of the root group\n\
      \"/\" are displayed.\n", progname);
}


/*-------------------------------------------------------------------------
 * Function:	dump_dataset_values
 *
 * Purpose:	Prints all values of a dataset.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
dump_dataset_values(hid_t dset)
{
    hid_t		f_type = H5Dget_type(dset);
    size_t		size = H5Tget_size(f_type);
    h5dump_t		info;

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);
    info.idx_fmt = "        (%s) ";
    info.line_ncols = width_g;
    if (verbose_g) info.cmpd_name = "%s=";

    /*
     * If the dataset is a 1-byte integer type then format it as an ASCI
     * character string instead of integers.
     */
    if (1==size && H5T_INTEGER==H5Tget_class(f_type)) {
	info.elmt_suf1 = "";
	info.elmt_suf2 = "";
	info.idx_fmt = "        (%s) \"";
	info.line_suf = "\"";
    }

    
    /*
     * Print all the values.
     */
    printf("    Data:\n");
    if (h5dump(stdout, &info, dset, -1)<0) {
	printf("        Unable to print data.\n");
    }

    H5Tclose(f_type);
}


/*-------------------------------------------------------------------------
 * Function:	list_attr
 *
 * Purpose:	Prints information about attributes.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, June  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list_attr (hid_t obj, const char *attr_name, void __unused__ *op_data)
{
    hid_t	attr;
    int		i;
    
    printf ("%*s%s", 26, "", attr_name);
    if ((attr = H5Aopen_name (obj, attr_name))) {
	hid_t space = H5Aget_space (attr);
	hsize_t size[64];
	int ndims = H5Sextent_dims (space, size, NULL);
	H5Sclose (space);
	printf (" {");
	for (i=0; i<ndims; i++) {
	    HDfprintf (stdout, "%s%Hu", i?", ":"", size[i]);
	}
	putchar ('}');
	H5Aclose (attr);
    }
    
    putchar ('\n');
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	list
 *
 * Purpose:	Prints the group member name.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
list (hid_t group, const char *name, void __unused__ *op_data)
{
    hid_t	obj;
    hid_t	(*func)(void*);
    void	*edata;
    int		i, nf;
    char	buf[512], comment[50];
    H5G_stat_t	statbuf;
    struct tm	*tm;
    
    /* Disable error reporting */
    H5Eget_auto (&func, &edata);
    H5Eset_auto (NULL, NULL);

    /* Print info about each name */
    printf ("%-25s ", name);

    if ((obj=H5Dopen (group, name))>=0) {
	hsize_t size[64];
	hsize_t maxsize[64];
	hid_t space = H5Dget_space (obj);
	int ndims = H5Sextent_dims(space, size, maxsize);
	printf ("Dataset {");
	for (i=0; i<ndims; i++) {
	    HDfprintf (stdout, "%s%Hu", i?", ":"", size[i]);
	    if (maxsize[i]==H5S_UNLIMITED) {
		HDfprintf (stdout, "/%s", "Inf");
	    } else if (maxsize[i]!=size[i] || verbose_g>0) {
		HDfprintf(stdout, "/%Hu", maxsize[i]);
	    }
	}
	printf ("}\n");
	H5Dclose (space);
	H5Aiterate (obj, NULL, list_attr, NULL);

	/* Print additional information about datasets */
	if (verbose_g>0) {
	    hid_t dcpl = H5Dget_create_plist(obj);
	    if ((nf = H5Pget_nfilters(dcpl))>0) {
		for (i=0; i<nf; i++) {
		    unsigned filt_flags;
		    H5Z_filter_t filt_id;
		    unsigned cd_values[20];
		    size_t cd_nelmts = NELMTS(cd_values);
		    size_t cd_num;
		    char f_name[32];
		    filt_id = H5Pget_filter(dcpl, i, &filt_flags, &cd_nelmts,
					    cd_values, sizeof(f_name), f_name);
		    f_name[sizeof(f_name)-1] = '\0';
		    sprintf(comment, "Filter-%d:", i);
		    printf("    %-10s %s-%u %s {", comment,
			   f_name[0]?f_name:"method",
			   (unsigned)filt_id,
			   filt_flags & H5Z_FLAG_OPTIONAL?"OPT":"");
		    for (cd_num=0; cd_num<cd_nelmts; cd_num++) {
			printf("%s%u", cd_num?", ":"", cd_values[cd_num]);
		    }
		    printf("}\n");
		}
	    }
	    H5Pclose(dcpl);
	}
	H5Dclose (obj);
    } else if ((obj=H5Gopen (group, name))>=0) {
	printf ("Group\n");
	H5Aiterate (obj, NULL, list_attr, NULL);
	H5Gclose (obj);
    } else if (H5Gget_linkval (group, name, sizeof(buf), buf)>=0) {
	if (NULL==HDmemchr (buf, 0, sizeof(buf))) {
	    strcpy (buf+sizeof(buf)-4, "...");
	}
	printf (" -> %s\n", buf);
    } else if ((obj=H5Topen (group, name))>=0) {
	printf ("Data type\n");
	H5Aiterate (obj, NULL, list_attr, NULL);
	H5Tclose (obj);
    } else {
	printf ("Unknown Type\n");
    }

    if (verbose_g>0) {
	if (H5Gstat(group, name, TRUE, &statbuf)>=0) {
	    printf("    %-10s %lu:%lu:%lu:%lu\n",
		   "Location:", statbuf.fileno[1], statbuf.fileno[0],
		   statbuf.objno[1], statbuf.objno[0]);
	    if (statbuf.mtime>0 && NULL!=(tm = localtime(&(statbuf.mtime)))) {
		strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S %Z", tm);
		printf("    %-10s %s\n", "Modtime:", buf);
	    }
	}

	/* Display the comment if the object has one */
	comment[0] = '\0';
	H5Gget_comment(group, name, sizeof(comment), comment);
	strcpy(comment+sizeof(comment)-4, "...");
	if (comment[0]) printf("    %-10s %s\n", "Comment:", comment);
    }
    
    if (dump_g && (obj=H5Dopen(group, name))>=0) {
	/* Turn on error reporting before dumping the data */
	H5Eset_auto(func, edata);
	dump_dataset_values(obj);
	H5Dclose(obj);
    }

    /* Restore error reporting */
    H5Eset_auto (func, edata);
    return 0;
}



/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Opens a file and lists the specified group
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Robb Matzke
 *              Monday, March 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main (int argc, char *argv[])
{
    hid_t	file, plist=H5P_DEFAULT;
    const char	*fname = NULL;
    const char	*gname = "/";
    const char	*progname;
    const char	*s;
    char	*rest;
    int		argno;

    /* Name of this program without the path */
    if ((progname=strrchr (argv[0], '/'))) progname++;
    else progname = argv[0];
    
    /* Switches come before non-switch arguments */
    for (argno=1; argno<argc && '-'==argv[argno][0]; argno++) {
	if (!strcmp(argv[argno], "--")) {
	    /* Last switch */
	    argno++;
	    break;
	} else if (!strcmp(argv[argno], "--help")) {
	    usage(progname);
	    exit(0);
	} else if (!strcmp(argv[argno], "--dump")) {
	    dump_g++;
	} else if (!strncmp(argv[argno], "--width=", 8)) {
	    width_g = strtol(argv[argno]+8, &rest, 0);
	    if (width_g<=0 || *rest) {
		usage(progname);
		exit(1);
	    }
	} else if (!strcmp(argv[argno], "--verbose")) {
	    verbose_g++;
	} else if (!strcmp(argv[argno], "--version")) {
	    printf("This is %s version %u.%u release %u\n",
		   progname, H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
	    exit(0);
	} else if (!strncmp(argv[argno], "-w", 2)) {
	    if (argv[argno][2]) {
		s = argv[argno]+2;
	    } else if (argno+1>=argc) {
		usage(progname);
		exit(1);
	    } else {
		s = argv[++argno];
	    }
	    width_g = strtol(s, &rest, 0);
	    if (width_g<=0 || *rest) {
		usage(progname);
		exit(1);
	    }
	} else if ('-'!=argv[argno][1]) {
	    /* Single-letter switches */
	    for (s=argv[argno]+1; *s; s++) {
		switch (*s) {
		case '?':
		case 'h':	/* --help */
		    usage(progname);
		    exit(0);
		case 'd':	/* --dump */
		    dump_g++;
		    break;
		case 'v':	/* --verbose */
		    verbose_g++;
		    break;
		case 'V':	/* --version */
		    printf("This is %s version %u.%u release %u\n",
			   progname, H5_VERS_MAJOR, H5_VERS_MINOR,
			   H5_VERS_RELEASE);
		    exit(0);
		default:
		    usage(progname);
		    exit(1);
		}
	    }
	} else {
	    usage(progname);
	    exit(1);
	}
    }

    /* Non-switch arguments */
    if (argno<argc) {
	fname = argv[argno++];
    } else {
	usage(progname);
	exit(1);
    }
    if (argno<argc) gname = argv[argno++];
    if (argno<argc) {
	usage(progname);
	exit(1);
    }
	
    /*
     * Open the file.  If the file name contains a `%' then assume that a
     * file family is being opened.
     */
    if (strchr (fname, '%')) {
	plist = H5Pcreate (H5P_FILE_ACCESS);
	H5Pset_family (plist, 0, H5P_DEFAULT);
    }
    if ((file = H5Fopen (fname, H5F_ACC_RDONLY, plist))<0) exit (1);
    if (H5Giterate (file, gname, NULL, list, NULL)<0) exit (1);
    if (H5Fclose (file)<0) exit (1);
    return 0;
}
